/**************************************************************************************************
 * @(#)MasterLoop.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.sql.Connection;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.sql.ResultSet;

/**
 * Orchestrates the main functionality of the MOVES system. It runs the InputDataManager object
 * and the InputControlStrategy objects to completion, then loops through the target locations and
 * times and calls the InternalControlStrategy, Generator, and EmissionCalculator objects.
 *
 * @author		Wesley Faler
 * @author		Sarah Luo, ERG
 * @author		Mitch C (Task 128)
 * @version		2016-07-19
**/
public class MasterLoop extends MOVESThread {
	/**
	 * Number of MasterLoopable threads to set loose on this Master Loop.
	 * Set this number lower to reduce cache contention in the database server.
	**/
	private static final int NUM_LOOPABLE_THREADS = 1; // 8;

	/**
	 * Count of how many outbound bundles will be created by the subscribers to this loop.
	 * One "bundle" is created each time an EmissionCalculator's executeLoop() method is called.
	 * This will be null until the value is known.
	**/
	public Integer howManyOutboundBundlesWillBeCreated = null;

	/**
	 * Completion fraction of startup activities.
	**/
	public Double startupFractionComplete = null;

	/** The context that the MasterLoop is currently iterating at. **/
	public MasterLoopContext context = new MasterLoopContext();

	/**
	 * This is a map from EmissionProcess objects to a sorted TreeSet of MasterLoopableSubscription
	 * objects.  These subscription objects within are sorted in the desired order of execution
	 * already (see MasterLoopableSubscription's compareTo method).  Hence, once the TreeSet is found
	 * appropriate to the current process, its members can be iterated over until the desired
	 * granularity is found then queued for processing in bunches according to their priority.
	**/
	private TreeMap<EmissionProcess,TreeSet<MasterLoopableSubscription> > subscriptions =
			new TreeMap<EmissionProcess,TreeSet<MasterLoopableSubscription> >();

	/** All subscribers **/
	private ArrayList<MasterLoopable> allSubscribers = new ArrayList<MasterLoopable>();

	/**
	 * A linked list of MasterLoopable objects ready to execute immediately. This collection
	 * is serviced by MasterLoopableThread objects. Synchronized against the MasterLoop.
	**/
	private LinkedList<MasterLoopable> readyToExecute = new LinkedList<MasterLoopable>();

	/**
	 * The number of MasterLoopable objects added to readyToExecute and not removed from
	 * completedExecution. Synchronized against the MasterLoop.
	**/
	private int numLoopableCompletionsToWaitFor = 0;

	/** A collection of active MasterLoopableThread objects. **/
	private LinkedList<MasterLoopableThread> loopableThreads =
			new LinkedList<MasterLoopableThread>();

	/**
	 * The set of contexts, including an associated MasterLoopable object, that have created
	 * non-calculator bundles that must be processed before the assoicated loopable object
	 * is considered finished.
	**/
	private LinkedList<MasterLoopContext> contextsWaitingForBundles = new LinkedList<MasterLoopContext>();

	/** Determines if the loop for years (and down to months, days, and hours) gets run **/
	private boolean mustLoopOverYears = true;
	/** Determines if the loop for months (and down to days and hours) gets run **/
	private boolean mustLoopOverMonths = true;
	/** Determines if the loop for days (and down to hours) gets run **/
	private boolean mustLoopOverDays = true;
	/** Determines if the loop for hours gets run **/
	private boolean mustLoopOverHours = true;

	/** true when trying to determine the value for howManyOutboundBundlesWillBeCreated **/
	private boolean isInBundleCountingMode = false;
	/** internal counter used when determining howManyOutboundBundlesWillBeCreated **/
	private int bundleCount = 0;
	/** true if startup had any failures, prevents the main thread from running **/
	private boolean startupFailed = false;

	/**
	 * Default constructor.
	**/
	public MasterLoop() {
		super("MasterLoop");
	}

	/**
	 * true when calling loopables only for the purpose of counting the number
	 * of bundles that will be generated.
	 * @return true if only counting bundles, false if requesting real work
	**/
	public boolean isCountingBundles() {
		return isInBundleCountingMode;
	}

	/**
	 * Set the startupFractionComplete member variable and notify the MOVESEngine's listeners
	 * of the progress.
	 * @param fraction new value for startupFractionComplete
	**/
	void setStartupFractionComplete(double fraction) {
		synchronized(this) {
			startupFractionComplete = Double.valueOf(fraction);
		}
		MOVESEngine.theInstance.notifyListeners();
	}

	/**
	 * This performs one-time startup activities prior to threaded operation, including merging
	 * databases (using InputDataManager) into the one execution database and then initializing
	 * ExecutionRunSpec data.
	 * @return Should the thread continue to run
	**/
	protected boolean startupInThread() {
		setStartupFractionComplete(0);

		double progress = 0;
		final double fractionBeforeExecutionDatabase = 0.20;
		final double fractionForAllMerges = 0.50;
		final double fractionForPreAggregation = 0.20;
		final double fractionAfterExecutionDatabase = 0.05;
		final double fractionMisc = 0.05;

		ExecutionRunSpec executionRunSpec = ExecutionRunSpec.theExecutionRunSpec;
		
		boolean retValueFinal = true;
		startupFailed = true; // assume failure unless everything works
		
		try {
			ExecutionRunSpec.theExecutionRunSpec.initializeBeforeExecutionDatabase();
			progress += fractionBeforeExecutionDatabase;
			setStartupFractionComplete(progress);

			int eventRecordID = MOVESEngine.logEventStart("Create Execution Database");
			//first we go through all the databases that the user has selected
			//in the reverse order and merge them with the Execution database
			//so that the data in the latest selected input is merged with the
			//Executiondatabase
			LinkedList<DatabaseSelection> databaseSelectionInputSets =
					ExecutionRunSpec.theExecutionRunSpec.getDatabaseSelectionInputSets();
			int howManyMerges = 1 + databaseSelectionInputSets.size();
			DatabaseSelection domainInputDatabase = executionRunSpec.getDomainInputDatabase();
			// Check databases, ensuring they exist
			if(domainInputDatabase != null) {
				Connection t = domainInputDatabase.openConnectionOrNull();
				if(t == null) {
					/**
					 * @issue Unable to open domain database [*]
					 * @explain A database listed in the RunSpec could not be accessed.  Check
					 * the database availability on your database server.
					**/
					Logger.log(LogMessageCategory.ERROR,
							"Unable to open domain database " + domainInputDatabase.databaseName);
					return false;
				}
				DatabaseUtilities.closeConnection(t);
			}
			if(databaseSelectionInputSets.size() != 0) {
				for(ListIterator<DatabaseSelection> i =
						databaseSelectionInputSets.
						listIterator(databaseSelectionInputSets.size());
						i.hasPrevious();) {
					DatabaseSelection tempSelection = (DatabaseSelection)(i.previous());
					Connection t = tempSelection.openConnectionOrNull();
					if(t == null) {
						/**
						 * @issue Unable to open user database [*]
						 * @explain A database listed in the RunSpec could not be accessed.  Check
						 * the database availability on your database server.
						**/
						Logger.log(LogMessageCategory.ERROR,
								"Unable to open user database " + tempSelection.databaseName);
						return false;
					}
					DatabaseUtilities.closeConnection(t);
				}
			}
			if(domainInputDatabase != null) {
				howManyMerges++;
			}
			// Merge the databases
			// Do a first pass copying only shallow data, such as years and regionCounty.
			// This shallow data can be then mined to get secondary identifies to futher
			// filter the rest of the data in the second pass.
			InputDataManager.startMergeSession();
			howManyMerges *= 2;
			for(int pass=0;pass<2 && retValueFinal;pass++) {
				if(databaseSelectionInputSets.size() != 0) {
					for(ListIterator<DatabaseSelection> i =
							databaseSelectionInputSets.
							listIterator(databaseSelectionInputSets.size());
							i.hasPrevious();) {
						DatabaseSelection dbToMerge = (DatabaseSelection)i.previous();
						if(InputDataManager.merge(dbToMerge) == false) {
							retValueFinal = false;
						}
						if(1==pass && InputDataManager.mergeNonRoad(dbToMerge) == false) {
							retValueFinal = false;
						}
						progress += fractionForAllMerges / howManyMerges;
						setStartupFractionComplete(progress);
					}
				}
				if(domainInputDatabase != null) {			
					if(InputDataManager.merge(domainInputDatabase) == false){
						retValueFinal = false;
					}
					if(1==pass && InputDataManager.mergeNonRoad(domainInputDatabase) == false){
						retValueFinal = false;
					}
					// Custom domains used for Mesoscale may have their own custom links
					// and these must be loaded once imported.  Doing so will provide filters
					// to InputDataManager and prevent it from loading all geographical data.
					if(1==pass) {
						if(retValueFinal && executionRunSpec.getRunSpec().isCustomDomain()
								&& executionRunSpec.getRunSpec().scale == ModelScale.MESOSCALE_LOOKUP) {
							executionRunSpec.initializeLocationsAfterDomainDatabase();
						}
					}
					progress += fractionForAllMerges / howManyMerges;
					setStartupFractionComplete(progress);
				}
				//next the default database is merged into the Execution database
				if(InputDataManager.merge(ExecutionRunSpec.theExecutionRunSpec.getModelDomain()) == false){
					retValueFinal = false;
				}
				if(1==pass && InputDataManager.mergeNonRoad(ExecutionRunSpec.theExecutionRunSpec.getModelDomain()) == false){
					retValueFinal = false;
				}
				progress += fractionForAllMerges / howManyMerges;
				setStartupFractionComplete(progress);
				
				if(0 == pass) {
					InputDataManager.advanceMergeSession();
					executionRunSpec.initializeAfterShallowTables();
				}
			}
			InputDataManager.createUniqueVehicleIDs();

			InputDataManager.endMergeSession(MOVESEngine.theInstance.getActiveRunID());
			MOVESEngine.logEventStop(eventRecordID);

			eventRecordID = MOVESEngine.logEventStart("Check VMT sources");
			//next, confirm there is at most one VMT source in the execution database
			if(retValueFinal) {
				if(howManyVMTSources() > 1) {
					Logger.log(LogMessageCategory.ERROR,
							"Too many VMT sources specified. Provide only one of HPMSVtypeYear, HPMSVtypeDay, SourceTypeYearVMT, and SourceTypeDayVMT.");
					retValueFinal = false;
				}
			}
			MOVESEngine.logEventStop(eventRecordID);

			eventRecordID = MOVESEngine.logEventStart("Preaggregate data");
			//next the Execution database is "preaggregated" if necessary
			if(retValueFinal) {
				InputDataManager.preAggregateExecutionDB();
			}
			progress += fractionForPreAggregation;
			setStartupFractionComplete(progress);
			MOVESEngine.logEventStop(eventRecordID);

			// add runSpecXXXXX tables used to assist calculators doing data filtering
			ExecutionRunSpec.theExecutionRunSpec.initializeAfterExecutionDatabase();
			progress += fractionAfterExecutionDatabase;
			setStartupFractionComplete(progress);

			// add default data to the execution database
			eventRecordID = MOVESEngine.logEventStart("Add default data");
			DefaultDataMaker.addDefaultDataToExecutionDatabase();
			MOVESEngine.logEventStop(eventRecordID);

			DatabaseConnectionManager.learnCreateTableStatementsForDefaultAndExecutionDatabases();
			if(retValueFinal) {
				MOVESInstantiator.performInstantiation(ExecutionRunSpec.theExecutionRunSpec, this);
				howManyOutboundBundlesWillBeCreated = Integer.valueOf(calcHowManyOutboundBundlesWillBeCreated());
			} else {
				howManyOutboundBundlesWillBeCreated = Integer.valueOf(0);
			}
			context.totalBundles = this.howManyOutboundBundlesWillBeCreated;
			MOVESEngine.updateExpectedDoneFiles(howManyOutboundBundlesWillBeCreated.intValue());
			progress += fractionMisc;
			setStartupFractionComplete(progress);
		} catch (Exception exception) {
			/**
			 * @explain An error occurred during simulation setup.  The simulation has failed
			 * and is ended.
			**/
			Logger.logError(exception, "Master Loop startup failed.");
			return false;
		}

		for (int i = 0; i < NUM_LOOPABLE_THREADS; i++) {
			MasterLoopableThread loopableThread = new MasterLoopableThread(this);
			loopableThread.start();

			loopableThreads.add(loopableThread);
		}

		setStartupFractionComplete(1);

		if(retValueFinal) {
			startupFailed = false;
		}
        return retValueFinal;
	}

	/**
	 * Count the number of VMT tables that are populated in the execution database.
	 * @return the number of VMT tables that have any data, never less than 0.
	**/
	int howManyVMTSources() {
		String[] tables = {
			"HPMSVtypeYear", "HPMSVtypeDay", "SourceTypeYearVMT", "SourceTypeDayVMT"
		};
		int count = 0;
		Connection db = null;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			for(int i=0;i<tables.length;i++) {
				String sql = "select count(*) from " + tables[i];
				if(SQLRunner.executeScalar(db,sql) > 0) {
					count++;
				}
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to check VMT sources");
			return 1000;
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
		}
		return count;
	}

	/**
	 * Performs the core looping work by calling all MasterLoopable objects in the proper
	 * sequence.
	 * @return Should the thread continue to run
	 * @throws InterruptedException This will be thrown if the current thread is interrupted.
	**/
	protected boolean threadIterationGo() throws InterruptedException {
		if(!isInBundleCountingMode) {
			if(startupFailed || CompilationFlags.GENERATE_CALCULATOR_INFO_DOCUMENTATION) {
				MOVESEngine.terminalErrorFound();
				return false;
			}
			MOVESEngine.theInstance.onStartedBundleGeneration();
		}
		ExecutionRunSpec executionRunSpec = ExecutionRunSpec.theExecutionRunSpec;
		//Make copies of base tables used for uncertainty calculations
		if(executionRunSpec.estimateUncertainty()) {
			InputDataManager.makeBaseUncertaintyInput();
		}

		//Logger.log(LogMessageCategory.DEBUG,"Number of years: " + executionRunSpec.years.size());
		//Logger.log(LogMessageCategory.DEBUG,"Number of months: " + executionRunSpec.months.size());
		//Logger.log(LogMessageCategory.DEBUG,"Number of days: " + executionRunSpec.days.size());
		//Logger.log(LogMessageCategory.DEBUG,"Number of hours: " + executionRunSpec.hours.size());

		// Perform main loop. Call all loopable objects.
		synchronized (subscriptions) {
			context.setRunID(MOVESEngine.theInstance.getActiveRunID());
			int numberOfIterations = executionRunSpec.getHowManyIterationsWillBePerformed();
			for(int iterationID=1;iterationID <= numberOfIterations;iterationID++) {
				if(isSignaledToExit()) {
					return false;
				}
				if(!isInBundleCountingMode) {
					if(executionRunSpec.estimateUncertainty()) {
						InputDataManager.simulateUncertaintyInInput(iterationID);
					}
					int processedIterationCount = MOVESEngine.theInstance.getHowManyIterationsProcessedSoFar();
					while(processedIterationCount<iterationID-1) {
						Logger.log(LogMessageCategory.INFO,"Waiting for iteration "+(iterationID-1)
								+ " to complete");
						sleep(10*1000);
						processedIterationCount =
								MOVESEngine.theInstance.getHowManyIterationsProcessedSoFar();
					}
					if(numberOfIterations>1) {
						Logger.log(LogMessageCategory.INFO,"Starting iteration "+iterationID
								+" of "+numberOfIterations);
					}
				}

				// Start the next iteration.
				context.setIteration(iterationID);
				ExecutionRunSpec.theExecutionRunSpec.initializeBeforeIteration();
				Iterator<EmissionProcess> processIter =
						ExecutionRunSpec.theExecutionRunSpec.targetProcesses.iterator();
				while (processIter.hasNext()) {
					if(isSignaledToExit()) {
						return false;
					}
					EmissionProcess iterProcess = (EmissionProcess)processIter.next();
					waitForPause();
					loopThroughProcess(iterProcess);
				}
			}
		}
		if(!isInBundleCountingMode) {
			MOVESEngine.theInstance.onDoneGeneratingBundles();
		}
		return false;
	}

	/**
	 * Loop through the specified emission process objects.
	 * @param emissionProcess The target emission process to loop through.
	 * @throws InterruptedException This will be thrown if the current thread is interrupted.
	**/
	protected void loopThroughProcess(EmissionProcess emissionProcess)
			throws InterruptedException {
		if(!subscriptions.containsKey(emissionProcess)) {
			// If no loopable objects are subscribed for this process, return.
			return;
		}
		if(!ExecutionRunSpec.theExecutionRunSpec.willRunCalculators) {
			// If there are no calculators, there can be no true bundles output but
			// we increment bundleCount to give a general measure of progress.
			if(isInBundleCountingMode) {
				bundleCount++;
			}
		}

		ExecutionRunSpec executionRunSpec = ExecutionRunSpec.theExecutionRunSpec;

		context.setProcess(emissionProcess);
		//Logger.log(LogMessageCategory.INFO,"context.process=" + emissionProcess.databaseKey);
		boolean hasHourSubscriptions = hasLoopables(MasterLoopGranularity.HOUR);
		boolean hasDaySubscriptions = hasLoopables(MasterLoopGranularity.DAY);
		boolean hasMonthSubscriptions = hasLoopables(MasterLoopGranularity.MONTH);
		boolean hasYearSubscriptions = hasLoopables(MasterLoopGranularity.YEAR);

		mustLoopOverYears = false;
		mustLoopOverMonths = false;
		mustLoopOverDays = false;
		mustLoopOverHours = false;

		if(hasHourSubscriptions) {
			mustLoopOverYears = true;
			mustLoopOverMonths = true;
			mustLoopOverDays = true;
			mustLoopOverHours = true;
		} else if(hasDaySubscriptions) {
			mustLoopOverYears = true;
			mustLoopOverMonths = true;
			mustLoopOverDays = true;
		} else if(hasMonthSubscriptions) {
			mustLoopOverYears = true;
			mustLoopOverMonths = true;
		} else if(hasYearSubscriptions) {
			mustLoopOverYears = true;
		}

		// Call all subscriptions set at process granularity.
		notifyLoopablesOfLoopChange(MasterLoopGranularity.PROCESS, false);

		// Iterate through ExecutionLocation objects.
		//Logger.log(LogMessageCategory.DEBUG,"Iterating through " + executionRunSpec.executionLocations.size() + " locations");
		for (Iterator<ExecutionLocation> executionLocationIter =
				executionRunSpec.executionLocations.iterator();
				executionLocationIter.hasNext();) {
			if(isSignaledToExit()) {
				break;
			}
			ExecutionLocation iterLocation = (ExecutionLocation)executionLocationIter.next();
			loopThroughLocation(iterLocation);
		}
		notifyLoopablesOfLoopChange(MasterLoopGranularity.LINK, true);
		notifyLoopablesOfLoopChange(MasterLoopGranularity.ZONE, true);
		notifyLoopablesOfLoopChange(MasterLoopGranularity.COUNTY, true);
		notifyLoopablesOfLoopChange(MasterLoopGranularity.STATE, true);

		notifyLoopablesOfLoopChange(MasterLoopGranularity.PROCESS, true);
	}

	/**
	 * Loop through the specified location.
	 * @param location The target location to loop through.
	 * @throws InterruptedException This will be thrown if the current thread is interrupted.
	**/
	protected void loopThroughLocation(ExecutionLocation location) throws InterruptedException {
		ExecutionRunSpec executionRunSpec = ExecutionRunSpec.theExecutionRunSpec;
		boolean stateChanging = (context.iterLocation == null) ||
			(context.iterLocation.stateRecordID != location.stateRecordID);
		boolean countyChanging = (context.iterLocation == null) ||
			(context.iterLocation.countyRecordID != location.countyRecordID);
		boolean zoneChanging = (context.iterLocation == null) ||
			(context.iterLocation.zoneRecordID != location.zoneRecordID);
		boolean linkChanging = (context.iterLocation == null) ||
			(context.iterLocation.linkRecordID != location.linkRecordID);

		if(context.iterLocation != null) {
			if(linkChanging) {
				notifyLoopablesOfLoopChange(MasterLoopGranularity.LINK, true);
			}
			if(zoneChanging) {
				notifyLoopablesOfLoopChange(MasterLoopGranularity.ZONE, true);
			}
			if(countyChanging) {
				notifyLoopablesOfLoopChange(MasterLoopGranularity.COUNTY, true);
			}
			if(stateChanging) {
				notifyLoopablesOfLoopChange(MasterLoopGranularity.STATE, true);
			}
		}

		context.setLocation(location);

		if(stateChanging) {
			notifyLoopablesOfLoopChange(MasterLoopGranularity.STATE, false);
		}
		if(countyChanging) {
			notifyLoopablesOfLoopChange(MasterLoopGranularity.COUNTY, false);
		}
		if(zoneChanging) {
			notifyLoopablesOfLoopChange(MasterLoopGranularity.ZONE, false);
		}
		if(linkChanging) {
			notifyLoopablesOfLoopChange(MasterLoopGranularity.LINK, false);
		}

		loopThroughTime();
	}

	/**
	 * Loop through the years, months, days, and hours (if need be).
	 * @throws InterruptedException This will be thrown if the current thread is interrupted.
	**/
	protected void loopThroughTime() throws InterruptedException {
		ExecutionRunSpec executionRunSpec = ExecutionRunSpec.theExecutionRunSpec;

		if(mustLoopOverYears) {
			for(Iterator<Integer> yearIter = executionRunSpec.years.iterator();
					yearIter.hasNext();) {
				if(isSignaledToExit()) {
					break;
				}
				context.resetTime();
				context.year = ((Integer)yearIter.next()).intValue();
				//Logger.log(LogMessageCategory.INFO,"context.year=" + context.year);
				//Logger.log(LogMessageCategory.INFO,"context=" + context.toBundleManifestContextForHumans());
				notifyLoopablesOfLoopChange(MasterLoopGranularity.YEAR, false);

				if(mustLoopOverMonths) {
					for(Iterator<Integer> monthIter=executionRunSpec.months.iterator();
							monthIter.hasNext();) {
						if(isSignaledToExit()) {
							break;
						}
						context.monthID = ((Integer)monthIter.next()).intValue();
						//Logger.log(LogMessageCategory.INFO,"context.monthID=" + context.monthID);
						//Logger.log(LogMessageCategory.INFO,"context=" + context.toBundleManifestContextForHumans());
						notifyLoopablesOfLoopChange(MasterLoopGranularity.MONTH, false);

						if(mustLoopOverDays) {
							for(Iterator<Integer> dayIter=executionRunSpec.days.iterator();
									dayIter.hasNext();) {
								if(isSignaledToExit()) {
									break;
								}
								context.dayID = ((Integer)dayIter.next()).intValue();
								notifyLoopablesOfLoopChange(MasterLoopGranularity.DAY, false);

								if(mustLoopOverHours) {
									for(Iterator<Integer> hourIter=
											executionRunSpec.hours.iterator();hourIter.hasNext();) {
										if(isSignaledToExit()) {
											break;
										}
										context.hourID = ((Integer)hourIter.next()).intValue();
										notifyLoopablesOfLoopChange(MasterLoopGranularity.HOUR,
												false);
										notifyLoopablesOfLoopChange(MasterLoopGranularity.HOUR,
												true);
									}
								}
								notifyLoopablesOfLoopChange(MasterLoopGranularity.DAY, true);
							}
						}
						notifyLoopablesOfLoopChange(MasterLoopGranularity.MONTH, true);
					}
				}
				notifyLoopablesOfLoopChange(MasterLoopGranularity.YEAR, true);
			}
		}
	}

	/**
	 * Called directly after threadIterationGo. Subclasses typically implement a simple sleep
	 * operation here.
	 * @throws InterruptedException This is typically thrown when a sleep() call is interrupted.
	**/
	protected void threadIterationWait() throws InterruptedException {
		sleep(5 * 1000);
	}

	/**
	 * Registers the given loopable object to be executed during the specified conditions.
	 *
	 * @param loopable The target loopable to subscribe.
	 * @param process The process to subscribe it for.
	 * @param loopGranularity The granularity level to execute it at.
	 * @param priority The priority to execute it at relative to other loopable objects at the
	 * same granularity.
	**/
	public void subscribe(MasterLoopable loopable, EmissionProcess process,
			MasterLoopGranularity loopGranularity, int priority) {
		if(loopable == null) {
			return;
		}
		String loopableType = loopable.getClass().getName();
		if(process == null) {
			Logger.log(LogMessageCategory.DEBUG,loopableType + " tried to subscribe to a null process");
			return;
		}
		Logger.log(LogMessageCategory.DEBUG,loopableType + " subscribed to " + process.processName + " at " + loopGranularity.toString() + "/" + priority);
		InterconnectionTracker.recordSubscription(loopable,process,loopGranularity,priority);
		MasterLoopableSubscription newSubscription = new MasterLoopableSubscription();
		newSubscription.loopable = loopable;
		newSubscription.granularity = loopGranularity;
		newSubscription.priority = priority;

		synchronized (subscriptions) {
			allSubscribers.add(loopable);

			TreeSet<MasterLoopableSubscription> processSubscriptions;
			if(subscriptions.containsKey(process)) {
				processSubscriptions = subscriptions.get(process);
			} else {
				processSubscriptions = new TreeSet<MasterLoopableSubscription>();
				subscriptions.put(process, processSubscriptions);
			}
			processSubscriptions.add(newSubscription);
		}
	}

	/**
	 * Unsigns up the specified loopable from all loop notifications. "loopable" may be
	 * subscribed for multiple notifications.
	 *
	 * @param loopable The loopable to unsubscribe from loop notifications.
	**/
	public void completelyUnSubscribe(MasterLoopable loopable) {
		synchronized (subscriptions) {
			allSubscribers.remove(loopable);

			Iterator<EmissionProcess> subscriptionSetIter = subscriptions.keySet().iterator();

			while (subscriptionSetIter.hasNext()) {
				TreeSet<MasterLoopableSubscription> processSubscriptions =
					(TreeSet<MasterLoopableSubscription>) subscriptions.get(subscriptionSetIter.next());
				Iterator<MasterLoopableSubscription> subscriptionIter = processSubscriptions.iterator();

				while (subscriptionIter.hasNext()) {
					MasterLoopableSubscription iterSubscription =
						(MasterLoopableSubscription)subscriptionIter.next();
					if (iterSubscription.loopable == loopable) {
						subscriptionIter.remove();
					}
				}

				if (processSubscriptions.size() < 1) {
					subscriptionSetIter.remove();
				}
			}
		}
	}

	/**
	 * Get the set of subscribers.
	 * @return the set of all subscribers
	**/
	public ArrayList<MasterLoopable> getSubscribers() {
		return allSubscribers;
	}

	/**
	 * Record a non-calculator context that will be creating bundles.
	 * @param c the context which must include an associated MasterLoopable object
	**/
	public synchronized void contextIsMakingBundles(MasterLoopContext c) {
		if(c != null && c.loopable != null) {
			contextsWaitingForBundles.add(c);
		}
	}

	/**
	 * Notification handler called after storing the payload of a non-calculator bundle.
	 * contextsWaitingForBundles is scanned, counts decremented, and when needed the
	 * assoicated loopable is set as complete.
	 * @param context context recovered from the bundle's manifest
	**/
	public synchronized void nonCalculatorBundleReceived(MasterLoopContext context) {
		// Find a matching context within contextsWaitingForBundles
		boolean found = false;
		for(Iterator<MasterLoopContext> i=contextsWaitingForBundles.iterator();i.hasNext();) {
			MasterLoopContext c = i.next();
			if(c.doesMatch(context)) {
				found = true;
				synchronized(c) {
					c.bundleCount--;
				}
			}
		}
		if(found) {
			checkContextsWaitingForBundles();
		}
	}

	/**
	 * Scan contextsWaitingForBundles, marking loopables as complete for any completed contexts.
	**/
	public synchronized void checkContextsWaitingForBundles() {
		LinkedList<MasterLoopContext> ready = null;
		for(Iterator<MasterLoopContext> i=contextsWaitingForBundles.iterator();i.hasNext();) {
			MasterLoopContext c = i.next();
			synchronized(c) {
				if(!c.isCreatingBundles && c.bundleCount <= 0) {
					if(ready == null) {
						ready = new LinkedList<MasterLoopContext>();
					}
					ready.add(c);
				}
			}
		}
		if(ready != null) {
			for(Iterator<MasterLoopContext> i=ready.iterator();i.hasNext();) {
				MasterLoopContext c = i.next();
				contextsWaitingForBundles.remove(c);
				try {
					loopableProcessingComplete(c.loopable);
				} catch(InterruptedException e) {
					return;
				}
			}
		}
	}

	/**
	 * Gets the next MasterLoopable that is ready for looping. This will wait until a loopable is
	 * available or the thread is interrupted.
	 *
	 * @throws InterruptedException if thread is interrupted while waiting
	 * @return The MasterLoopable object that is ready for processing.
	**/
	public synchronized MasterLoopable getNextLoopableForProcessing() throws InterruptedException {
		while(readyToExecute.size() < 1) {
			wait();
		}

		MasterLoopable loopable = (MasterLoopable)readyToExecute.removeFirst();
		//System.out.println("*** STARTING " + loopable.getClass().getName());
		return loopable;
	}

	/**
	 * Signals that the given loopable object has completed processing.
	 *
	 * @param loopable The MasterLoopable object that has completed processing.
	**/
	public synchronized void loopableProcessingComplete(MasterLoopable loopable)
			throws InterruptedException {
		//System.out.println("*** COMPLETED " + loopable.getClass().getName());
		numLoopableCompletionsToWaitFor--;
		notifyAll();
	}

	/**
	 * Queues the given loopable for processing.
	 *
	 * @param loopable The loopable to queue.
	**/
	public synchronized void queueLoopableForProcessing(Integer priority, MasterLoopable loopable) {
		if(isSignaledToExit()) {
			return;
		}
		boolean shouldQueue = true;
		if(loopable instanceof MasterLoopContext.IContextFilter) {
			if(!((MasterLoopContext.IContextFilter)loopable).doesProcessContext(context)) {
				shouldQueue = false;
			}
		}
		if(shouldQueue) {
			if(isInBundleCountingMode) {
				if(!context.isCleanUp && loopable instanceof EmissionCalculator) {
					bundleCount++;
				}
			} else {
				readyToExecute.add(loopable);
				numLoopableCompletionsToWaitFor++;

				notifyAll();
			}
		}
	}

	/**
	 * Waits until all the loopable objects that have been queued for processing have been
	 * processed.
	 * @throws InterruptedException Thrown if this thread is interrupted while waiting.
	**/
	public synchronized void waitUntilLoopablesAreProcessed() throws InterruptedException {
		if(!isInBundleCountingMode) {
			while (numLoopableCompletionsToWaitFor > 0) {
				wait();
			}
		}
	}

	/**
	 * Determines if notifyLoopablesOfLoopChange would actually notify anything.
	 * Depends upon the current EmissionProcess.
	 * @param loopChangeGranularity The granularity that the loop would changed at.
	 * @return true if there any subscribers at the level of granularity specified.
	**/
	private boolean hasLoopables(MasterLoopGranularity loopChangeGranularity)
			throws InterruptedException {
		synchronized (subscriptions) {
			context.setGranularity(loopChangeGranularity);
			if(!subscriptions.containsKey(context.iterProcess)) {
				return false;
			}
			TreeSet<MasterLoopableSubscription> processSubscriptions =
					subscriptions.get(context.iterProcess);
			Iterator<MasterLoopableSubscription> subscriptionIter = processSubscriptions.iterator();
			MasterLoopableSubscription iterSubscription = null;
			// Skip past subscriptions that are set at too coarse a level.
			while (subscriptionIter.hasNext()) {
				iterSubscription = (MasterLoopableSubscription)subscriptionIter.next();
				if (iterSubscription.granularity.compareTo(loopChangeGranularity) >= 0) {
					break;
				}
			}
			if((iterSubscription != null) &&
					(iterSubscription.granularity == MasterLoopGranularity.MATCH_FINEST)) {
				// Loopable's set to execute at MATCH_FINEST should only execute when other
				// loopable objects have executed before them.
				iterSubscription = null;
			}
			// Iterate over all subscriptions at the target granularity.
			if(iterSubscription != null) {
				if(iterSubscription.granularity.compareTo(loopChangeGranularity) == 0) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Checks the subscriptions for an EmissionProcess to ensure at least one of the
	 * objects is derived from EmissionCalculator.  If not, there is no reason to do the
	 * generation for the EmissionProcess.  This is useful in preventing incomplete calculation
	 * pipelines from performing work.  It is primarily a safeguard, but can be exploited by
	 * calculators that realize they should combine several processes but only sign up for one
	 * (such as how Well-To-Pump requires Running, Start, and Extended Idle numbers).
	 * @param emissionProcess an EmissionProcess object to be checked
	 * @return true if at least one of the subscribers to an EmissionProcess is
	 * derived from EmissionCalculator
	**/
	private boolean hasCalculators(EmissionProcess emissionProcess) {
		synchronized (subscriptions) {
			if(!subscriptions.containsKey(emissionProcess)) {
				return false;
			}
			TreeSet<MasterLoopableSubscription> processSubscriptions =
					subscriptions.get(emissionProcess);
			Iterator<MasterLoopableSubscription> subscriptionIter = processSubscriptions.iterator();
			MasterLoopableSubscription iterSubscription = null;

			// Skip past subscriptions that are set at too coarse a level.
			while (subscriptionIter.hasNext()) {
				iterSubscription = (MasterLoopableSubscription)subscriptionIter.next();
				if(iterSubscription.loopable instanceof EmissionCalculator) {
					return true;
				}
			}
		}

		return false;
	}

	/**
	 * Notifies all loopables of new loop state which is specified in context.
	 * Does not return until all affected loopables have completed.
	 * @param loopChangeGranularity The granularity that the loop has changed at.
	 * @param isCleanUp when true, each MasterLoopable's cleanDataLoop method should be called
	 * instead of the executeLoop method.  Used to set context.isCleanUp.
	 * @throws InterruptedException Thrown if this thread is interrupted while waiting.
	**/
	private void notifyLoopablesOfLoopChange(MasterLoopGranularity loopChangeGranularity,
			boolean isCleanUp) throws InterruptedException {
		synchronized (subscriptions) {
			context.isCleanUp = isCleanUp;
			context.setGranularity(loopChangeGranularity);
			if(!subscriptions.containsKey(context.iterProcess)) {
				return;
			}

			TreeSet<MasterLoopableSubscription> processSubscriptions =
					subscriptions.get(context.iterProcess);

			Iterator<MasterLoopableSubscription> subscriptionIter = processSubscriptions.iterator();
			MasterLoopableSubscription iterSubscription = null;

			// Skip past subscriptions that are set at too coarse a level.
			while (subscriptionIter.hasNext()) {
				iterSubscription = (MasterLoopableSubscription)subscriptionIter.next();
				if (iterSubscription.granularity.compareTo(loopChangeGranularity) >= 0) {
					break;
				}
			}

			if((iterSubscription != null) &&
					(iterSubscription.granularity == MasterLoopGranularity.MATCH_FINEST)) {
				// Loopable's set to execute at MATCH_FINEST should only execute when other
				// loopable objects have executed before them.
				iterSubscription = null;
			}

			// Iterate over all subscriptions at the target granularity, stopping to
			// wait for completion of high priority groups before proceeding.
			// Note that the objects here are already sorted due to their inclusion in
			// a TreeSet and due to MasterLoopableSubscription's compareTo method.
			int previousPriorityGroup = -1;
			while (iterSubscription != null) {
				// If the current object's granularity does not match the desired granularity,
				// then we've gone too far in the sorted list of subscriptions and can stop.
				if (iterSubscription.granularity.compareTo(loopChangeGranularity) != 0) {
					break;
				}

				// If the priority of the last object queued is different than the current
				// object's priority, then wait for the previously queued objects to complete.
				if (previousPriorityGroup != -1 &&
						previousPriorityGroup != iterSubscription.priority) {
					waitUntilLoopablesAreProcessed();
				}

				// Remember the priority of the current object about to be queued
				previousPriorityGroup = iterSubscription.priority;
				context.setExecutionPriority(previousPriorityGroup);
				queueLoopableForProcessing(Integer.valueOf(iterSubscription.priority),
						iterSubscription.loopable);

				// If there are more subscriptions in the list, then move on otherwise stop
				// and wait for anything already queued to complete.
				if (subscriptionIter.hasNext()) {
					iterSubscription = (MasterLoopableSubscription)subscriptionIter.next();
				} else {
					break;
				}
			}
			waitUntilLoopablesAreProcessed();
		}
	}

	/** This performs one-time shutdown activities after threaded operation. **/
	protected void shutdownInThread() {
		// Ignore InterruptedExceptions. Continue until they've all quit.
		while (true) {
			try {
				for (Iterator<MasterLoopableThread> i = loopableThreads.iterator(); i.hasNext();) {
					MasterLoopableThread iterThread = (MasterLoopableThread) i.next();
					iterThread.signalToTerminateAndWait();
					i.remove();
				}
				return;
			} catch (InterruptedException exception) {
				// Nothing to do here
			}
		}
	}

	/**
	 * Examine the subscriptions, granularities, and the ExecutionRunSpec to determine
	 * how many outbound bundles will be created.  This can be used to determine the
	 * fraction of completed work.
	 * @return the number of outbound bundles that will be created
	**/
	private int calcHowManyOutboundBundlesWillBeCreated() {
		// Turn on the special flag that ensures our normal loop (in threadIterationGo) will just
		// count the number of calls that would get made to executeLoop within EmissionCalculator
		// objects.
		isInBundleCountingMode = true;
		// Reset the count (incremented by queueLoopableForProcessing when it queues an
		// EmissionCalculator)
		bundleCount = 0;
		try {
			// Do the normal looping over all subscriptions.  Remember that the bundle counting
			// mode is engaged, so this won't actually cause any calls to MasterLoopable objects
			// to be made.
			threadIterationGo();
		} catch(InterruptedException e) {
			// Nothing to do here
		}
		// Turn off the bundle counting mode so that the normal looping with actually make
		// calls to MasterLoopable objects when used next.
		isInBundleCountingMode = false;
		Logger.log(LogMessageCategory.INFO,bundleCount + " bundles will be used.");
		return bundleCount;
	}
}
