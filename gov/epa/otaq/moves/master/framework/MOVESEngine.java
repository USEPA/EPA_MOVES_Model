/**************************************************************************************************
 * @(#)MOVESEngine.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.MasterLoop;
import gov.epa.otaq.moves.master.framework.MOVESEngineListener;
import gov.epa.otaq.moves.master.runspec.RunSpec;
import gov.epa.otaq.moves.master.gui.MOVESWindow;
import java.sql.*;
import java.util.*;
import java.io.*;
import java.text.SimpleDateFormat;

/**
 * This is a singleton object that encapsulates the entire execution of the simulation. This is
 * either built into a stand alone console application or within a GUI application.
 *
 * This owns and launches a MasterLoop and a unbundler, both of which are threads.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @author		EPA (Mitch C.)
 * @author 		Tim Hull
 * @version		2017-03-22
**/
public class MOVESEngine implements LogHandler {
	/** portion of total progress represented by startup activities **/
	public static double STARTUP_TOTAL_FRACTION = 0.20;
	/** portion of total progress represented by bundle generation activities **/
	public static double GENERATED_FILES_TOTAL_FRACTION = 0.20;
	/** portion of total progress represented by receipt and processing of worker results **/
	public static double PROCESSED_FILES_TOTAL_FRACTION =
			1.0 - GENERATED_FILES_TOTAL_FRACTION - STARTUP_TOTAL_FRACTION;

	/** Global pointer to the one and only MOVESEngine instance. **/
	public static MOVESEngine theInstance = null;

	public interface CompletedListener {
		public void engineCompleted(MOVESEngine engine);
	}

	/** List of MOVESEngineListener's that are subscribed to receive progress updates. **/
	static LinkedList<MOVESEngineListener> progressListeners =
			new LinkedList<MOVESEngineListener>();

	/** List of classes to notify after absolutely everything has been done with the engine. **/
	static LinkedList<CompletedListener> completedListeners = new LinkedList<CompletedListener>();

	/** The loop launched by this engine **/
	MasterLoop loop = null;

	/** The object responsible for bundling data for the distributed workers. **/
	EmissionCalculatorOutboundBundler bundler = null;

	/**
	 * The object responsible for detecting work bundles that have been completed by the
	 * distributed workers.
	**/
	EmissionCalculatorInboundUnbundler unbundler = null;

	/** Is this engine currently paused. **/
	boolean isPaused = false;

	/** True if only TODO files should be generated, without DONE files being retrieved or deleted. **/
	boolean isTODOOnly = false;

	/** Details needed for the MOVESRun table entry, needed in each TODO file so DONE files can be processed offline. **/
	BundleManifest.MasterFragment masterFragment = null;

	/** The active run ID. This is significant in the output database. **/
	int activeRunID;

	/** A thread for checking worker IDs to ensure workers are present and available **/
	HeartbeatDetectionThread heartbeatDetector = null;

	/** A thread for checking worker IDs to ensure workers are present and available **/
	MOVESEngineCompletionChecker checker = null;

	/** Starting time in milliseconds for the current simulation **/
	long startTimeMillis = 0;

	/** Next eventRecordID for the MOVESEventLog table.  Reset upon each run. **/
	int nextEventRecordID = 1;

	/** eventRecordID (for use with logEventStop) for the entire simulation's time **/
	int wholeSimulationEventRecordID = 0;

	/** true while a run is being started and objects should not be deleted prematurely **/
	volatile boolean isLaunching = false;

	/**
	 * Internal class used to track the time required for one aspect of the simulation.
	**/
	class TimeEstimator {
		long startTimeMillis = 0;
		long lastTimeMillis = 0;
		long estimatedFinishTimeMillis = 0;
		long actualFinishTimeMillis = 0;

		double fractionOfObservationsNeededForEstimate = 0.15;

		// Observations are milliseconds per bundle
		long expectedObservationCount = 0;
		long observationCount = 0;
		double sumObservationsSquared = 0.0;
		double sumObservations = 0.0;

		void setStartTime() {
			startTimeMillis = System.currentTimeMillis();
			lastTimeMillis = startTimeMillis;
		}

		void setExpectedObservationCount(long count) {
			expectedObservationCount = count;
		}

		void addObservation() {
			if(startTimeMillis > 0 && lastTimeMillis > 0) {
				long now = System.currentTimeMillis();
				long observation = now - lastTimeMillis;
				lastTimeMillis = now;

				observationCount++;
				sumObservations += observation;
				sumObservationsSquared += observation * observation;

				if(observationCount >= expectedObservationCount
						&& expectedObservationCount > 0) {
					actualFinishTimeMillis = now;
				}
			}
		}

		long estimateFinishTime() {
			if(actualFinishTimeMillis > 0) {
				return actualFinishTimeMillis;
			}
			if(startTimeMillis <= 0 || expectedObservationCount <= 1) {
				return estimatedFinishTimeMillis;
			}
			double fractionKnown = (double)observationCount / (double)expectedObservationCount;
			// If not enough observations to trust extrapolation, then don't do anything
			if(fractionKnown <= fractionOfObservationsNeededForEstimate) {
				return estimatedFinishTimeMillis;
			}
			/*
				Percentiles and standard deviation
				(from http://www.spirxpert.com/expressing3.htm)
				 0.1%	-3.09
				 2.3%	-2.00
				 5.0%	-1.64
				16.0%	-1.00
				50.0%	 0.00
				95.0%	 1.64
				97.7%	 2.00
				99.9%	 3.09
			*/
			final double desiredDeviations = 0.90; // Empirically determined
			double mean = sumObservations / observationCount;
			double standardDeviation = Math.sqrt(
					(sumObservationsSquared -
					((sumObservations*sumObservations) / observationCount))
					/ (observationCount - 1));
			double rate = mean + standardDeviation * desiredDeviations;

			estimatedFinishTimeMillis = (long)(lastTimeMillis +
					rate * (expectedObservationCount - observationCount));
			return estimatedFinishTimeMillis;
		}

		void resample() {
			setStartTime();
			expectedObservationCount -= observationCount;
			observationCount = 0;
			sumObservationsSquared = 0.0;
			sumObservations = 0.0;
		}
	}

	/** used to estimate the time required to perform generation on the master **/
	TimeEstimator generationEstimator = null;
	/** used to estimate the time required to perform work on all the workers **/
	TimeEstimator workerEstimator = null;
	/** used to estimate the time required to process bundles received back from workers **/
	TimeEstimator processingEstimator = null;

	/** Used when collecting DONE files created by a prior run **/
	public PDSpec.PDSpecEntry pdEntry = null;
	/** True when the last PDSpecEntry in a PDSpec is being run **/
	public boolean isLastPDSpecEntry = false;
	/** True when a module decides that the simulation should be stopped prematurely. **/
	public boolean shouldStopProcessing = false;

	/** Default constructor **/
	public MOVESEngine() {
		/** @nonissue **/
		Logger.addLogHandler(this);
	}

	/**
	 * Launches all necessary worker threads to process the simulation specified in runSpec.
	 * runSpec should not be modified until after the simulation has completed.
	 * @param entryToUse the master and folder to use for picking up DONE files
	 * @param isLastPDSpecEntry true when entryToUse is the last entry in a list of entries, used for GUI modes
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException If there was a database error creating the run ID.
	 * @throws IOException If issues arise while examining DONE files.
	**/
	public void launch(PDSpec.PDSpecEntry entryToUse, boolean isLastPDSpecEntryToUse) throws InterruptedException, SQLException, IOException {
		shouldStopProcessing = false;
		masterFragment = null;
		pdEntry = entryToUse;
		isLastPDSpecEntry = isLastPDSpecEntryToUse;
		EmissionCalculatorInboundUnbundler.MasterDetails details = EmissionCalculatorInboundUnbundler.getDetails(entryToUse);
		boolean shouldFinishQuickly = false;
		if(details == null || details.fullManifest == null) {
			Logger.log(LogMessageCategory.ERROR,"Unable to obtain manifest information for master " + entryToUse.masterID);
			shouldFinishQuickly = true;
		} else if(details.numberOfDONEFiles <= 0) {
			Logger.log(LogMessageCategory.INFO,"No DONE files were found for master " + entryToUse.masterID + " in directory " + entryToUse.pickupFolderName);
			shouldFinishQuickly = true;
		}
		if(shouldFinishQuickly) {
			if(isLastPDSpecEntry) {
				for (Iterator i = progressListeners.iterator(); i.hasNext();) {
					MOVESEngineListener iterListener = (MOVESEngineListener) i.next();
					iterListener.engineIsCompleting(this);
				}
			}
			for(Iterator<CompletedListener> i=completedListeners.iterator();i.hasNext();) {
				i.next().engineCompleted(this);
			}
			return;
		}

		masterFragment = details.fullManifest.masterFragment;

		// Fill runSpec from the PDSpecEntry
		RunSpec runSpec = new RunSpec();
		// 	fill output database selection by cloning the selection in pdEntry
		runSpec.outputDatabase = (DatabaseSelection)pdEntry.outputDatabase.clone();
		ModelScale scale = ModelScale.getByName(masterFragment.scale);
		if(scale != null) {
			runSpec.scale = scale;
		}
		ModelDomain domain = ModelDomain.getByName(masterFragment.domain);
		if(domain != null) {
			runSpec.domain = domain;
		}
		runSpec.shouldTruncateMOVESActivityOutput = false;
		runSpec.shouldTruncateMOVESOutput = false;
		runSpec.shouldTruncateBaseRateOutput = false;

		launchCore(runSpec,"(unnamed)",Integer.valueOf(details.numberOfDONEFiles));
	}

	/**
	 * Launches all necessary worker threads to process the simulation specified in runSpec.
	 * runSpec should not be modified until after the simulation has completed.
	 * @param runSpec The RunSpec that specifies the simulation parameters.
	 * @param runSpecFileName The name (no path information) of the RunSpec.
	 * @return true if the simulation was successfully started, false otherwise
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException If there was a database error creating the run ID.
	**/
	public boolean launch(RunSpec runSpec, String runSpecFileName)
			throws InterruptedException, SQLException {
		shouldStopProcessing = false;
		pdEntry = null;
		masterFragment = null;
		return launchCore(runSpec,runSpecFileName,null);
	}

	/**
	 * Launches all necessary worker threads to process the simulation specified in runSpec.
	 * runSpec should not be modified until after the simulation has completed.
	 * @param runSpec The RunSpec that specifies the simulation parameters.
	 * @param runSpecFileName The name (no path information) of the RunSpec.
	 * @param numberOfDONEFiles optional number of DONE files expected, used when picking
	 * up DONE files from other masters.
	 * @return true if the simulation was successfully started, false otherwise
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException If there was a database error creating the run ID.
	**/
	private boolean launchCore(RunSpec runSpec, String runSpecFileName, Integer numberOfDONEFiles)
			throws InterruptedException, SQLException {
		shouldStopProcessing = false;
		if(pdEntry == null) {
			Logger.log(LogMessageCategory.INFO,"***Starting MOVES run***");
			Logger.log(LogMessageCategory.INFO,"Master Release: " + MOVESWindow.MOVES_VERSION);
			Logger.log(LogMessageCategory.INFO,"Master Computer ID: "
					+ SystemConfiguration.getTheSystemConfiguration().computerID);
			Logger.log(LogMessageCategory.INFO,"Master ID: "
					+ SystemConfiguration.getTheSystemConfiguration().distributedMasterID);
			Logger.log(LogMessageCategory.INFO,"RunSpec: " + runSpecFileName );
		} else {
			Logger.log(LogMessageCategory.INFO,"***Starting pickup of DONE files ***");
			Logger.log(LogMessageCategory.INFO,"Local Master Release: " + MOVESWindow.MOVES_VERSION);
			Logger.log(LogMessageCategory.INFO,"Local Master Computer ID: "
					+ SystemConfiguration.getTheSystemConfiguration().computerID);
			Logger.log(LogMessageCategory.INFO,"Local Master ID: "
					+ SystemConfiguration.getTheSystemConfiguration().distributedMasterID);
			Logger.log(LogMessageCategory.INFO,"Picking up Master ID: " + pdEntry.masterID);
			Logger.log(LogMessageCategory.INFO,"Picking up from: " + pdEntry.pickupFolderName);
			Logger.log(LogMessageCategory.INFO,"Storing to: " + pdEntry.outputDatabase.toString());
		}

		isLaunching = true;
		boolean shouldRestoreDatabase = false;

		try {
			if(runSpec.hasDeprecatedShouldSeparateRampsTrue) {
				/**
				 * @issue RunSpec includes the flag to provide separate ramp output. It was created with an older version of MOVES.  This flag must be removed from the RunSpec before MOVES will run successfully.
				 * @explain Save the runspec as a new file, which will remove the deprecated ramp flag, then open the new file.
				**/
				Logger.log(LogMessageCategory.ERROR,
						"Runspec uses a deprecated Ramp feature. Update by saving to a new file and opening it.");
				return false;
			}

			if(runSpec.hasAVFT()) {
				/**
				 * @issue RunSpec includes data for Fueltypes and Technologies (AVFT) created with an older version of MOVES.  This data must be converted to the new format or deleted from the RunSpec before MOVES will run successfully.
				 * @explain Try exporting your data then use the AVFT importer.
				**/
				Logger.log(LogMessageCategory.ERROR,
						"RunSpec includes data for Fueltypes and Technologies (AVFT) created with an older version of MOVES.  This data must be converted to the new format or deleted from the RunSpec before MOVES will run successfully." );
				return false;
			}

			if(runSpec.hasOnRoadRetrofit()) {
				/**
				 * @issue RunSpec includes data for Retrofits created with an older version of MOVES.  This data must be converted to the new format or deleted from the RunSpec before MOVES will run successfully.
				 * @explain Try exporting your data then use the OnRoad Retrofit importer.
				**/
				Logger.log(LogMessageCategory.ERROR,
						"RunSpec includes data for Retrofits created with an older version of MOVES.  This data must be converted to the new format or deleted from the RunSpec before MOVES will run successfully." );
				return false;
			}

			// Check databases, ensuring they exist
			DatabaseSelection domainInputDatabase = runSpec.scaleInputDatabase;
			if(domainInputDatabase != null && domainInputDatabase.hasDatabase()) {
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
				
				boolean hasBadHotelling = false;
				boolean hasBadHPMS = false;
				
				// only check hotelling and vmt tables if running onroad model
				Models.ModelCombination mc = runSpec.getModelCombination();
				switch (mc) {
					case M2:
						break;
					case M1:
					case M12:
					default:
						hasBadHotelling = hasWrongHotellingTables(t, domainInputDatabase);
						hasBadHPMS = hasOldHPMSVTypes(t, domainInputDatabase);
						break;
				}
				
				DatabaseUtilities.closeConnection(t);
				
				if(hasBadHotelling || hasBadHPMS) {
					return false;
				}
			}
			if(runSpec.databaseSelectionInputSets.size() != 0) {
				for(ListIterator<DatabaseSelection> i =
						runSpec.databaseSelectionInputSets.
						listIterator(runSpec.databaseSelectionInputSets.size());
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
					boolean hasBadHPMS = hasOldHPMSVTypes(t, tempSelection);
					DatabaseUtilities.closeConnection(t);
					if(hasBadHPMS) {
						return false;
					}
				}
			}
			// Check the custom input database, especially for the correct schema
			if(runSpec.inputDatabase != null && runSpec.inputDatabase.hasDatabase()) {
				Connection t = runSpec.inputDatabase.openConnectionOrNull();
				if(t == null) {
					/**
					 * @issue Custom input database cannot be found [*]
					 * @explain A database to be used as the primary input database to MOVES
					 * could not be located.
					**/
					Logger.log(LogMessageCategory.ERROR,
							"Custom input database cannot be found "
							+ runSpec.inputDatabase.databaseName);
					return false;
				}
				boolean hasSchema = InputDataManager.isDefaultSchemaPresent(t);
				DatabaseUtilities.closeConnection(t);
				t = null;
				if(!hasSchema) {
					/**
					 * @issue Custom input database does not have the MOVES schema [*]
					 * @explain A database to be used as the primary input database to MOVES
					 * does not have the required tables.
					**/
					Logger.log(LogMessageCategory.ERROR,
							"Custom input database does not have the MOVES schema "
							+ runSpec.inputDatabase.databaseName);
					return false;
				}
				shouldRestoreDatabase = true;
				DatabaseConnectionManager.cleanup(MOVESDatabaseType.DEFAULT);
				DatabaseConnectionManager.customInputDatabase = runSpec.inputDatabase;
				DatabaseConnectionManager.initialize(MOVESDatabaseType.DEFAULT);
			}

			nextEventRecordID = 1;
			startTimeMillis = System.currentTimeMillis();

			/** @nonissue **/
			Logger.shouldPromoteErrorLevel = true;
			resetCompletionTimeEstimate();
			DatabaseConnectionManager.setOutputDatabase(runSpec.outputDatabase.serverName,
					runSpec.outputDatabase.databaseName);

			String dbStatus = isOutputDatabaseNameValid(
					DatabaseConnectionManager.outputDatabaseSelection);
			if(dbStatus != null) {
				/** @nonissue **/
				Logger.log(LogMessageCategory.ERROR,dbStatus);
				return false;
			}

			dbStatus = createOutputDatabase(DatabaseConnectionManager.outputDatabaseSelection);
			if(dbStatus != null) {
				/** @nonissue **/
				Logger.log(LogMessageCategory.ERROR,dbStatus);
				return false;
			}

			clearSharedWorkFolder();

			heartbeatDetector = new HeartbeatDetectionThread();
			if(pdEntry == null) {
				heartbeatDetector.start();
			}

			isPaused = false;
			activeRunID = 0;

			loop = new MasterLoop();
			if(pdEntry == null) {
				bundler = new EmissionCalculatorOutboundBundler();
			} else {
				bundler = null;
				loop.howManyOutboundBundlesWillBeCreated = numberOfDONEFiles;
				loop.startupFractionComplete = Double.valueOf(1.0);
			}
			unbundler = new EmissionCalculatorInboundUnbundler();
			if(pdEntry != null) {
				unbundler.distributedMasterIDOverride = pdEntry.masterID;
				unbundler.pickupFolderOverride = new File(pdEntry.pickupFolderName);
			}

			DatabaseConnectionManager.initializeAll();
			PollutantProcessLoader.loadFromDatabase();
			TimeSpan.loadTimeObjects();
			if(masterFragment == null) {
				masterFragment = new BundleManifest.MasterFragment();
			}
			if(pdEntry == null) {
				activeRunID = createOutputRunRecord(runSpec, runSpecFileName, masterFragment); // fills masterFragment
			} else {
				activeRunID = createOutputRunRecord(masterFragment, numberOfDONEFiles.intValue()); // reads masterFragment
			}
			wholeSimulationEventRecordID = logEventStart("Whole Simulation");
			ExecutionRunSpec.theExecutionRunSpec = new ExecutionRunSpec(runSpec);

			notifyListeners();

			if(loop != null && pdEntry == null) {
				loop.start();
			}
			if(!isTODOOnly) {
				unbundler.start();
			}
			startCheckingCompletion();
			shouldRestoreDatabase = false;
		} finally {
			if(shouldRestoreDatabase) {
				restoreDefaultDatabase();
			}
			isLaunching = false;
		}
		return true;
	}

	/**
	 * Check for unwanted HPMS 20 and 30 VMT records.
	 * @param db open connection to the database
	 * @param dbSelection database descriptor, used for error messages
	 * @return true if the database contains any old hpms vtype VMT records
	**/
	private static boolean hasOldHPMSVTypes(Connection db, DatabaseSelection dbSelection) {
		try {
			int count = (int)SQLRunner.executeScalar(db,"select count(*) as c from hpmsvtypeday where hpmsvtypeid in (20,30)");
			if(count > 0) {
				Logger.log(LogMessageCategory.ERROR,
						"Old HPMSVTypeID 20 and/or 30 was found in hpmsVTypeDay table of database " + dbSelection.databaseName);
				return true;
			}
		} catch(Exception e) {
			// Nothing to do here
		}
		try {
			int count = (int)SQLRunner.executeScalar(db,"select count(*) as c from hpmsvtypeyear where hpmsvtypeid in (20,30)");
			if(count > 0) {
				Logger.log(LogMessageCategory.ERROR,
						"Old HPMSVTypeID 20 and/or 30 was found in hpmsVTypeYear table of database " + dbSelection.databaseName);
				return true;
			}
		} catch(Exception e) {
			// Nothing to do here
		}
		return false;
	}

	/**
	 * Check for unconverted hotelling tables.
	 * @param db open connection to the database
	 * @param dbSelection database descriptor, used for error messages
	 * @return true if the database contains any unconverted hotelling tables.
	**/
	private static boolean hasWrongHotellingTables(Connection db, DatabaseSelection dbSelection) {
		String[] commands = {
			"hotellingHoursPerDay", "select count(distinct yearID, zoneID, dayID) from hotellingHoursPerDay",
			"hotellingHourFraction", "select count(distinct zoneID, dayID, hourID)+ifnull(sum(hourFraction),0) from hotellingHourFraction",
			"hotellingAgeFraction", "select count(distinct zoneID, ageID) from hotellingAgeFraction",
			"hotellingActivityDistribution", "select count(distinct zoneID, beginModelYearID, endModelYearID, opModeID) from hotellingActivityDistribution",
			"hotellingMonthAdjust", "select count(distinct zoneID, monthID) from hotellingMonthAdjust"
		};
		String tableName = "";
		try {
			for(int i=0;i<commands.length;i+=2) {
				tableName = commands[i+0];
				String sql = commands[i+1];
				SQLRunner.executeScalar(db,sql);
			}
			return false;
		} catch(Exception e) {
			// tableName could not be queried because it does not exist or has the wrong schema.
			Logger.log(LogMessageCategory.ERROR,
					"Old table definition found for " + tableName + " table of database " + dbSelection.databaseName);
			return true;
		}
	}

	/** Validate the Output Database Name
	 * @param  dbSelection the database selection.
	 * @return String null if valid, a message if not valid.
	 **/
	public static String isOutputDatabaseNameValid(DatabaseSelection dbSelection) {
		String message = null;
		if(dbSelection.databaseName.trim().length()==0) {
			/**
			 * @issue The output database name can not be blank.
			 * @explain An output database name cannot be blank.
			**/
			message = "The output database name can not be blank.";
			return message;
		}

		DatabaseSelection defaultDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
		//default database cannot be accepted as an input selection
		if ((defaultDB.databaseName).equalsIgnoreCase(dbSelection.databaseName)) {
			/**
			 * @issue The default database cannot be used as an output database.
			 * @explain The output of a simulation should not be stored into a database
			 * that is used for input.
			**/
			message = "The default database cannot be used as an output database.";
			return message;
		}
		//execution database cannot be accepted as an input selection
		DatabaseSelection executionDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()];
		if ((executionDB.databaseName).equalsIgnoreCase(dbSelection.databaseName)) {
			/**
			 * @issue The execution database cannot be used as an output database.
			 * @explain The output of a simulation should not be stored into the temporary
			 * MOVESExecution database.
			**/
			message = "The execution database cannot be used as an output database.";
			return message;
		}
		//Worker database cannot be accepted as an input selection
		if (dbSelection.databaseName.equalsIgnoreCase("MOVESWorker")) {
			/**
			 * @issue MOVESWorker database cannot be used as an output database.
			 * @explain The output of a simulation should not be stored into the temporary
			 * MOVESWorker database.
			**/
			message = "MOVESWorker database cannot be used as an output database.";
			return message;
		}
		//MySQL database cannot be accepted as an input selection
		if (dbSelection.databaseName.equalsIgnoreCase("MySQL")) {
			/**
			 * @issue MySQL database cannot be used as an output database.
			 * @explain The output of a simulation should not be stored into the MySQL
			 * system database.
			**/
			message = "MySQL database cannot be used as an ouput database.";
			return message;
		}
		return message;
	}

	/**
	 * Create the output database.
	 * @param  dbSelection the database selection.
	 * @return String null if valid, a message if not valid.
	 **/
	public String createOutputDatabase(DatabaseSelection dbSelection) {
	 	String message = null;
		if(dbSelection.safeCreateDatabase("database/CreateOutput.sql") == DatabaseSelection.NOT_CREATED) {
			/**
			 * @issue Could not create the Output Database.
			 * @explain An error occurred while creating an output database using the
			 * database/CreateOutput.sql script.
			**/
			message = "Could not create the Output Database.";
		}
		return message;
	}

	/**
	 * Update the MOVESRun table once the number of expected bundles is known.
	 * @param howManyExpected number of expected bundles
	 * @throws SQLException if anything goes wrong
	 * @throws InterruptedException if unable to obtain a database connection
	**/
	public static void updateExpectedDoneFiles(int howManyExpected) throws SQLException, InterruptedException {
		theInstance.masterFragment.expectedDONEFiles = howManyExpected;

		Connection outputConnection = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
		String sql = "";

		try {
			sql = "update MOVESRun set expectedDONEFiles=" + howManyExpected
					+ " where MOVESRunID=" + theInstance.activeRunID;
			SQLRunner.executeSQL(outputConnection,sql);
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT, outputConnection);
		}
	}

	/**
	 * Creates the output record for a simulation run.
	 * @param runSpec The RunSpec to provide parameters from.
	 * @param runSpecName The name of the RunSpec that is being run.
	 * @param masterFragment location to store data needed for TODO files
	 * @return The new run ID.
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException If a database error occurs.
	**/
	static int createOutputRunRecord(RunSpec runSpec, String runSpecName, BundleManifest.MasterFragment masterFragment)
			throws InterruptedException, SQLException {
		masterFragment.outputDatabaseName = DatabaseConnectionManager.outputDatabaseSelection.databaseName;
		Connection outputConnection = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
		Connection defaultConnection = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";

		try {
			final String eol = System.getProperty("line.separator");

			// Silently upgrade MOVESRun to include required columns
			String[] upgradeStatements = {
				"alter table MOVESRun add masterIDNumber VARCHAR(20) NULL DEFAULT NULL",
				"alter table MOVESRun add expectedDONEFiles INTEGER UNSIGNED NULL DEFAULT NULL",
				"alter table MOVESRun add retrievedDONEFiles INTEGER UNSIGNED NULL DEFAULT NULL",
				"alter table MOVESRun add models VARCHAR(40) NOT NULL DEFAULT 'onroad'"
			};
			for(int i=0;i<upgradeStatements.length;i++) {
				sql = upgradeStatements[i];
				try {
					SQLRunner.executeSQL(outputConnection,sql);
				} catch(Exception e) {
					// Nothing to do here, we are silently upgrading and tolerate errors here.
					// If the error is because the columns exist, then there is no real
					// problem.  If the column cannot be added, a real error will occur when
					// we use the column in the code below.
				}
			}

			// Add to MOVESRun
			sql = "INSERT INTO MOVESRun "
				+ "(outputTimePeriod, timeUnits, distanceUnits, massUnits, "
				+ "energyUnits, runSpecFileName, runSpecDescription, "
				+ "runSpecFileDateTime, runDateTime, scale, "
				+ "defaultDatabaseUsed, masterVersion, masterComputerID, "
				+ "domain,masterIDNumber,retrievedDONEFiles,models) "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, now(), ?, ?, ?, ?, ?, ?, 0, ?) ";
			PreparedStatement statement = outputConnection.prepareStatement(sql);
			ResultSet rs = null;
			SQLRunner.Query query = new SQLRunner.Query();
			try {
				if(runSpec.outputTimeStep != null) {
					statement.setString(1,runSpec.outputTimeStep.getStandardAbbreviation());
					masterFragment.outputTimePeriod = runSpec.outputTimeStep.getStandardAbbreviation();
				} else {
					statement.setNull(1, java.sql.Types.VARCHAR);
				}
				if(runSpec.outputFactors.timeMeasurementSystem != null) {
					statement.setString(2
						, runSpec.outputFactors.timeMeasurementSystem.getStandardAbbreviation());
					masterFragment.timeUnits = runSpec.outputFactors.timeMeasurementSystem.getStandardAbbreviation();
				} else {
					statement.setNull(2, java.sql.Types.VARCHAR);
				}

				statement.setString(3,"mi");
				if(runSpec.outputFactors.distanceMeasurementSystem != null) {
					masterFragment.distanceUnits = runSpec.outputFactors.distanceMeasurementSystem.getStandardAbbreviation();
					//statement.setString(3, runSpec.outputFactors.distanceMeasurementSystem.getStandardAbbreviation());
				} else {
					//statement.setNull(3, java.sql.Types.VARCHAR);
				}

				statement.setString(4, "g");
				if(runSpec.outputFactors.massMeasurementSystem != null) {
					masterFragment.massUnits = runSpec.outputFactors.massMeasurementSystem.getStandardAbbreviation();
					//statement.setString(4, runSpec.outputFactors.massMeasurementSystem.getStandardAbbreviation());
				} else {
					//statement.setNull(4, java.sql.Types.VARCHAR);
				}

				statement.setString(5, "KJ");
				if(runSpec.outputFactors.energyMeasurementSystem != null) {
					masterFragment.energyUnits = runSpec.outputFactors.energyMeasurementSystem.getStandardAbbreviation();
					//statement.setString(5, runSpec.outputFactors.energyMeasurementSystem.getStandardAbbreviation());
				} else {
					//statement.setNull(5, java.sql.Types.VARCHAR);
				}

				statement.setString(6, runSpecName);
				masterFragment.runSpecFileName = runSpecName;

				if (runSpec.description != null) {
					// remove carriage returns and line feeds from description
					// becuase they screw up importing into Excel
					String desc = (runSpec.description.replace('\r',' ')).replace('\n',' ');
					statement.setString(7,desc);
					masterFragment.runSpecDescription = desc;
				} else {
					statement.setString(7, " ");
				}

				File runSpecPath = MOVESAPI.getTheAPI().getRunSpecFilePath();
				if(runSpecPath != null && runSpecPath.exists()) {
					String t = FileTimeUtility.convertFileTimeToString(runSpecPath.lastModified());
					statement.setString(8,t);
					masterFragment.runSpecFileDateTime = t;
				} else {
					statement.setNull(8, java.sql.Types.VARCHAR);
				}

				statement.setString(9, runSpec.scale.toString());
				masterFragment.scale = runSpec.scale.toString();

				if(runSpec.inputDatabase != null && runSpec.inputDatabase.hasDatabase()) {
					statement.setString(10, runSpec.inputDatabase.databaseName);
					masterFragment.defaultDatabaseUsed = runSpec.inputDatabase.databaseName;
				} else {
					statement.setString(10, SystemConfiguration.getTheSystemConfiguration().
							databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName);
					masterFragment.defaultDatabaseUsed = SystemConfiguration.getTheSystemConfiguration().
							databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
				}

				statement.setString(11, StringUtilities.substring(MOVESWindow.MOVES_VERSION,0,100));
				masterFragment.masterVersionDate = StringUtilities.substring(MOVESWindow.MOVES_VERSION,0,100);

				statement.setString(12, SystemConfiguration.getTheSystemConfiguration().computerID);
				masterFragment.masterComputerID = SystemConfiguration.getTheSystemConfiguration().computerID;

				statement.setString(13, runSpec.domain.toString());
				masterFragment.domain = runSpec.domain.toString();

				masterFragment.masterIDNumber = SystemConfiguration.getTheSystemConfiguration().distributedMasterID;
				statement.setString(14, masterFragment.masterIDNumber);

				/** NR_IMP: **/
				String modelStr;
				Models.ModelCombination mc = runSpec.getModelCombination();
				switch (mc) {
					case M1:
						modelStr = "onroad";
						break;
					case M2:
						modelStr = "nonroad";
						break;
					case M12:
						modelStr = "onroad|nonroad";
						break;
					default:
						modelStr = "onroad";
						break;
				}
				masterFragment.model = modelStr;
				statement.setString(15, masterFragment.model);
				/** end of NR_IMP: **/

				SQLRunner.execute(statement,sql);
				statement.close();
				statement = null;

				sql = "SELECT last_insert_id()";
				statement = outputConnection.prepareStatement(sql);
				rs = SQLRunner.executeQuery(statement,sql);
				rs.next();
				int runID = rs.getInt(1);
				rs.close();
				rs = null;

				if(runSpec.domain != ModelDomain.NATIONAL_ALLOCATION) {
					int countyID = 0;
					String countyName = "";
					String serverName = "";
					String databaseName = "";

					if(runSpec.isCustomDomain()) {
						countyID = runSpec.genericCounty.getCountyID();
						countyName = runSpec.genericCounty.description;
					} else {
						// Get the county ID from the first county-level geographic selection
						countyID = runSpec.getCountyID();
						if(countyID > 0) {
							sql = "select countyName from County where countyID=" + countyID;
							countyName = SQLRunner.executeScalarString(defaultConnection,sql);
						}
					}
					if(runSpec.scaleInputDatabase != null) {
						serverName = runSpec.scaleInputDatabase.serverName;
						databaseName = runSpec.scaleInputDatabase.databaseName;
					}
					countyName = StringUtilities.substring(countyName,0,50);
					serverName = StringUtilities.substring(serverName,0,100);
					databaseName = StringUtilities.substring(databaseName,0,200);

					masterFragment.domainCountyID = countyID;
					masterFragment.domainCountyName = countyName;
					masterFragment.domainDatabaseServer = serverName;
					masterFragment.domainDatabaseName = databaseName;

					sql = "update MOVESRun set domainCountyID=" + countyID
							+ ", domainCountyName="
							+ DatabaseUtilities.escapeSQL(countyName,true)
							+ ", domainDatabaseServer="
							+ DatabaseUtilities.escapeSQL(serverName,true)
							+ ", domainDatabaseName="
							+ DatabaseUtilities.escapeSQL(databaseName,true)
							+ " where MOVESRunID=" + runID;
					SQLRunner.executeSQL(outputConnection,sql);
				}

				if(CompilationFlags.DO_RATES_FIRST) {
					// pollutant.energyOrMass: "mass", "energy", "TEQ"
					TreeMap<String,String> pollutantUnits = new TreeMap<String,String>();
					sql = "select pollutantID, energyOrMass from pollutant";
					query.open(defaultConnection,sql);
					while(query.rs.next()) {
						String pollutantID = query.rs.getString("pollutantID");
						String energyOrMass = query.rs.getString("energyOrMass");
						pollutantUnits.put(pollutantID,energyOrMass);
					}
					query.close();
					for(Iterator<PollutantProcessAssociation> i=runSpec.pollutantProcessAssociations.iterator();i.hasNext();) {
						PollutantProcessAssociation ppa = i.next();
						String pollutantID = "" + ppa.pollutant.databaseKey;
						String energyOrMass = StringUtilities.safeGetString(pollutantUnits.get(pollutantID));
						if(energyOrMass.equalsIgnoreCase("mass")) {
							energyOrMass = "g";
						} else if(energyOrMass.equalsIgnoreCase("energy")) {
							energyOrMass = "KJ";
						}
						String processUnits = "s";
						String activityUnits = "s";
						if(ppa.emissionProcess.databaseKey == 1) {
							activityUnits = "mi";
						} else if(ppa.emissionProcess.databaseKey == 2) {
							processUnits = "start";
							activityUnits = "start";
						}
						sql = "insert into BaseRateUnits ("
								+ "MOVESRunID,"
								+ "pollutantID,"
								+ "processID,"
								+ "meanBaseRateUnitsNumerator,"
								+ "meanBaseRateUnitsDenominator,"
								+ "emissionBaseRateUnitsNumerator,"
								+ "emissionBaseRateUnitsDenominator) "
								+ "values ("
								+ runID + ","
								+ pollutantID + ","
								+ ppa.emissionProcess.databaseKey + ","
								+ DatabaseUtilities.escapeSQL(energyOrMass,true) + ","
								+ DatabaseUtilities.escapeSQL(processUnits,true) + ","
								+ DatabaseUtilities.escapeSQL(energyOrMass,true) + ","
								+ DatabaseUtilities.escapeSQL(activityUnits,true)
								+ ")";
						SQLRunner.executeSQL(outputConnection,sql);
					}
				}

				return runID;
			} finally {
				query.onFinally();
				if(rs != null) {
					try {
						rs.close();
					} catch(Exception e) {
						// Nothing to do here
					}
					rs = null;
				}
				if(statement != null) {
					try {
						statement.close();
					} catch(Exception e) {
						// Nothing to do here
					}
					statement = null;
				}
			}
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT, outputConnection);
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT, defaultConnection);
		}
	}

	/**
	 * Creates the output record for a simulation run.
	 * @param masterFragment provides data contained in the expected DONE files
	 * @param expectedDONEFiles number of DONE files to be processed
	 * @return The new run ID.
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException If a database error occurs.
	**/
	static int createOutputRunRecord(BundleManifest.MasterFragment masterFragment, int expectedDONEFiles)
			throws InterruptedException, SQLException {
		masterFragment.outputDatabaseName = DatabaseConnectionManager.outputDatabaseSelection.databaseName;
		Connection outputConnection = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
		Connection defaultConnection = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";

		try {
			final String eol = System.getProperty("line.separator");

			// Silently upgrade MOVESRun to include required columns
			String[] upgradeStatements = {
				"alter table MOVESRun add masterIDNumber VARCHAR(20) NULL DEFAULT NULL",
				"alter table MOVESRun add expectedDONEFiles INTEGER UNSIGNED NULL DEFAULT NULL",
				"alter table MOVESRun add retrievedDONEFiles INTEGER UNSIGNED NULL DEFAULT NULL",
				"alter table MOVESRun add models VARCHAR(40) NOT NULL DEFAULT 'onroad'"
			         /** NR_IMP: **/
			};
			for(int i=0;i<upgradeStatements.length;i++) {
				sql = upgradeStatements[i];
				try {
					SQLRunner.executeSQL(outputConnection,sql);
				} catch(Exception e) {
					// Nothing to do here, we are silently upgrading and tolerate errors here.
					// If the error is because the columns exist, then there is no real
					// problem.  If the column cannot be added, a real error will occur when
					// we use the column in the code below.
				}
			}

			// Add to MOVESRun
			sql = "INSERT INTO MOVESRun "
				+ "(outputTimePeriod, timeUnits, distanceUnits, massUnits, "
				+ "energyUnits, runSpecFileName, runSpecDescription, "
				+ "runSpecFileDateTime, runDateTime, scale, "
				+ "defaultDatabaseUsed, masterVersion, masterComputerID, "
				+ "domain,masterIDNumber,retrievedDONEFiles,expectedDONEFiles,models) "
				+ "VALUES (?, ?, ?, ?, ?, ?, ?, ?, now(), ?, ?, ?, ?, ?, ?, 0, ?, ?) ";
			PreparedStatement statement = outputConnection.prepareStatement(sql);
			ResultSet rs = null;
			try {
				if(masterFragment.outputTimePeriod != null && masterFragment.outputTimePeriod.length() > 0) {
					statement.setString(1,masterFragment.outputTimePeriod);
				} else {
					statement.setNull(1, java.sql.Types.VARCHAR);
				}
				if(masterFragment.timeUnits != null && masterFragment.timeUnits.length() > 0) {
					statement.setString(2, masterFragment.timeUnits);
				} else {
					statement.setNull(2, java.sql.Types.VARCHAR);
				}

				statement.setString(3,"mi");

				statement.setString(4, "g");

				statement.setString(5, "KJ");

				statement.setString(6, masterFragment.runSpecFileName);

				if (masterFragment.runSpecDescription != null && masterFragment.runSpecDescription.length() > 0) {
					// remove carriage returns and line feeds from description
					// becuase they screw up importing into Excel
					String desc = (masterFragment.runSpecDescription.replace('\r',' ')).replace('\n',' ');
					statement.setString(7,desc);
				} else {
					statement.setString(7, " ");
				}

				if(masterFragment.runSpecFileDateTime != null && masterFragment.runSpecFileDateTime.length() > 0) {
					statement.setString(8,masterFragment.runSpecFileDateTime);
				} else {
					statement.setNull(8, java.sql.Types.VARCHAR);
				}

				statement.setString(9, masterFragment.scale);

				if(masterFragment.defaultDatabaseUsed != null && masterFragment.defaultDatabaseUsed.length() > 0) {
					statement.setString(10, masterFragment.defaultDatabaseUsed);
				} else {
					statement.setString(10, "");
				}

				statement.setString(11, StringUtilities.substring(masterFragment.masterVersionDate,0,10));

				statement.setString(12, masterFragment.masterComputerID);

				statement.setString(13, masterFragment.domain);

				statement.setString(14, masterFragment.masterIDNumber);

				statement.setInt(15, expectedDONEFiles);

				statement.setString(16, masterFragment.model);

				SQLRunner.execute(statement,sql);
				statement.close();
				statement = null;

				sql = "SELECT last_insert_id()";
				statement = outputConnection.prepareStatement(sql);
				rs = SQLRunner.executeQuery(statement,sql);
				rs.next();
				int runID = rs.getInt(1);
				rs.close();
				rs = null;

				if(!masterFragment.domain.equalsIgnoreCase(ModelDomain.NATIONAL_ALLOCATION.toString())) {
					String countyName = StringUtilities.substring(masterFragment.domainCountyName,0,50);
					String serverName = StringUtilities.substring(masterFragment.domainDatabaseServer,0,100);
					String databaseName = StringUtilities.substring(masterFragment.domainDatabaseName,0,200);

					sql = "update MOVESRun set domainCountyID=" + masterFragment.domainCountyID
							+ ", domainCountyName="
							+ DatabaseUtilities.escapeSQL(countyName,true)
							+ ", domainDatabaseServer="
							+ DatabaseUtilities.escapeSQL(serverName,true)
							+ ", domainDatabaseName="
							+ DatabaseUtilities.escapeSQL(databaseName,true)
							+ " where MOVESRunID=" + runID;
					SQLRunner.executeSQL(outputConnection,sql);
				}

				return runID;
			} finally {
				if(rs != null) {
					try {
						rs.close();
					} catch(Exception e) {
						// Nothing to do here
					}
					rs = null;
				}
				if(statement != null) {
					try {
						statement.close();
					} catch(Exception e) {
						// Nothing to do here
					}
					statement = null;
				}
			}
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT, outputConnection);
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT, defaultConnection);
		}
	}

	/**
	 * Straight forward access method.
	 * @return The active run ID. This is significant in the output database.
	**/
	public int getActiveRunID() {
		return activeRunID;
	}

	/**
	 * Get the active iteration id.
	 * @return The active run ID. This is significant in the output database.
	**/
	public int getActiveIterationID() {
		return (loop!=null && pdEntry == null)?loop.context.iterationID:0;
	}

	/**
	 * Subscribe to progress notifications.
	 * @param listener The listener that should receive progress notifications.
	**/
	public static void subscribeToProgress(MOVESEngineListener listener) {
		synchronized(progressListeners) {
			progressListeners.add(listener);
		}
	}

	/**
	 * Unsubscribe from progress notifications.
	 * @param listener The listener that should no longer receive progress notifications.
	**/
	public static void unSubscribeFromProgress(MOVESEngineListener listener) {
		synchronized(progressListeners) {
			while (progressListeners.remove(listener));
		}
	}

	/**
	 * Notifies all listeners of new engine status.
	**/
	public synchronized void notifyListeners() {
		for (Iterator i = progressListeners.iterator(); i.hasNext();) {
			MOVESEngineListener iterListener = (MOVESEngineListener) i.next();

			iterListener.engineProgressUpdate(this);
		}
	}

	/**
	 * Notifies all listeners that the engine is completing.
	**/
	public synchronized void notifyListenersOfCompletion() {
		if(isLaunching) {
			return;
		}
		//Commented out to avoid superfluous messages in CLI run
		//Logger.log(LogMessageCategory.INFO,"Notifying listeners of MOVES Engine completion");
		//System.out.flush();
		for (Iterator i = progressListeners.iterator(); i.hasNext();) {
			MOVESEngineListener iterListener = (MOVESEngineListener) i.next();
			iterListener.engineIsCompleting(this);
		}
		loop = null;
		bundler = null;
		unbundler = null;
		if(heartbeatDetector != null && pdEntry == null) {
			try {
				heartbeatDetector.signalToTerminate();
				heartbeatDetector.join();
			} catch(InterruptedException e) {
				// Nothing to do here
			}
			heartbeatDetector = null;
		}
		if(checker != null) {
			checker.signalToTerminate();
			// May be called from checker, do not join.
			checker = null;
		}
		clearSharedWorkFolder();
		logEventStop(wholeSimulationEventRecordID);
		finalizeEventLog();
		activeRunID = 0;

		/** @nonissue **/
		Logger.shouldPromoteErrorLevel = false;
		for(Iterator<CompletedListener> i=completedListeners.iterator();i.hasNext();) {
			i.next().engineCompleted(this);
		}
	}

	/**
	 * Thread to monitor engine's progress and signal listeners when a run is complete.
	**/
	class MOVESEngineCompletionChecker extends Thread {
		/** This flag signals this thread to exit. This directly affects only one thread. **/
		boolean signaledToExit = false;

		/** Runs the MOVESEngineCompletionChecker thread **/
		public final void run() {
			while(MOVESEngine.theInstance != null
					&& MOVESEngine.theInstance.checkIsRunningAndNotifyListenersOfCompletion()
					&& !signaledToExit) {
				try {
					Thread.sleep(1000);
				} catch(InterruptedException e) {
					// Nothing to do here
				}
			}
			// Flush all tables to disk when the run completes, making it safer to copy files
			// from the output database.
			Connection db = null;
			try {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
				if(db != null) {
					SQLRunner.executeSQL(db,"flush tables");
				}
			} catch(Exception e) {
				Logger.logException(LogMessageCategory.ERROR,e);
			} finally {
				if(db != null) {
					try {
						DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
					} catch(Exception e) {
						// Nothing to do here
					}
					db = null;
				}
			}
			Logger.log(LogMessageCategory.INFO,"MOVESEngineCompletionChecker is shutting down");
			Logger.log(LogMessageCategory.INFO,"***Finished MOVES run***");
			System.out.flush();
		}

		/** Signal the MOVESEngineCompletionChecker thread to terminate **/
		public void signalToTerminate() {
			signaledToExit = true;
		}
	}

	/**
	 * Thread to monitor engine's progress and signal listeners when a run is complete.
	**/
	synchronized void startCheckingCompletion() {
		try {
			checker = new MOVESEngineCompletionChecker();
			checker.start();
		} catch(Throwable e) {
			// Nothing to do here
		}
	}

	/**
	 * Checks whether the engine is actively running and if not, notifies all
	 * listeners of this.
	 * @return This will be true if the engine is actively running.
	**/
	public synchronized boolean checkIsRunningAndNotifyListenersOfCompletion() {
		if(isLaunching) {
			return true;
		}
		if(!isRunning()) {
			notifyListenersOfCompletion();
			return false;
		} else {
			return true;
		}
	}

	/**
	 * Returns whether the engine is actively running or not.
	 * @return This will be true if the engine is actively running.
	**/
	public synchronized boolean isRunning() {
		return !shouldStopProcessing && (
				(pdEntry == null && loop != null && loop.isAlive()) 
				|| (!isTODOOnly && unbundler != null && unbundler.isAlive())
				);
	}

	/**
	 * Gets the fraction of work that the engine has completed.
	 * @return A fraction value (0-1) indicating how much work the engine has completed.
	**/
	public float getPercentCompleted() {
		if(!isRunning()) {
			return 0;
		}

		double result = 0;

		Double startupFractionComplete = getStartupFractionComplete();
		if(startupFractionComplete == null) {
			return (float)result;
		}

		if(startupFractionComplete.doubleValue() > 1.0) {
			result = STARTUP_TOTAL_FRACTION;
		} else {
			result += startupFractionComplete.doubleValue() * STARTUP_TOTAL_FRACTION;
		}

		Integer totalBundleCount = getHowManyOutboundBundlesWillBeCreated();
		if(totalBundleCount == null) {
			return (float)result;
		}

		if(bundler != null) {
			result += ((double)bundler.totalBundleCount) / totalBundleCount.intValue()
					* GENERATED_FILES_TOTAL_FRACTION;
			if(unbundler != null && !isTODOOnly) {
				result += ((double)unbundler.processedWorkBundleCount) / totalBundleCount.intValue()
						* PROCESSED_FILES_TOTAL_FRACTION;
			}
		} else {
			if(unbundler != null && !isTODOOnly) {
				result += ((double)unbundler.processedWorkBundleCount) / totalBundleCount.intValue();
			}
		}
		return (float)result;
	}

	/**
	 * Waits for all worker threads to complete. This does not signal the workers to prematurely
	 * terminate.
	 * Will notify all listeners that the engine is done.
	 * @throws InterruptedException This will be thrown if the active thread is interrupted.
	**/
	public void join() throws InterruptedException {
		join(0);
	}

	/**
	 * Waits for all worker threads to complete. This does not signal the workers to prematurely
	 * terminate.
	 * Will cleanup the execution database and notify all listeners if the engine is complete
	 * within the timeout period.
	 * @param millisecondWait The maximum number of milliseconds to wait.  0 means wait forever.
	 * @throws InterruptedException This will be thrown if the active thread is interrupted.
	**/
	public void join(int millisecondWait) throws InterruptedException {
		if(loop != null && pdEntry == null) {
			loop.join(millisecondWait);
		}
		if(unbundler != null && !isTODOOnly) {
			unbundler.join(millisecondWait);
		}
		if(heartbeatDetector != null && pdEntry == null) {
			heartbeatDetector.signalToTerminate();
			heartbeatDetector.join(millisecondWait);
		}
		if(checker != null) {
			checker.signalToTerminate();
			// May be called from checker, do not join.
			checker = null;
		}
		if(!isRunning()) {
			DatabaseConnectionManager.cleanup(MOVESDatabaseType.EXECUTION);
			restoreDefaultDatabase();
			notifyListenersOfCompletion();
		}
	}

	/** Undo the default database override from the RunSpec **/
	void restoreDefaultDatabase() {
		if(DatabaseConnectionManager.customInputDatabase != null) {
			DatabaseConnectionManager.cleanup(MOVESDatabaseType.DEFAULT);
			DatabaseConnectionManager.customInputDatabase = null;
			DatabaseConnectionManager.initialize(MOVESDatabaseType.DEFAULT);
		}
	}

	/**
	 * Signals all worker threads to terminate. Does NOT wait for completion.
	**/
	public void signalToTerminate() throws InterruptedException {
		if(loop != null && pdEntry == null) {
			loop.signalToTerminate();
		}
		if(unbundler != null) {
			unbundler.signalToTerminate();
		}
		if(heartbeatDetector != null && pdEntry == null) {
			heartbeatDetector.signalToTerminate();
		}
		if(checker != null) {
			checker.signalToTerminate();
			// May be called from checker, do not join.
			checker = null;
		}
	}

	/**
	 * Signals all worker threads to complete and waits for their completion.
	 * @throws InterruptedException This will be thrown if the active thread is interrupted.
	**/
	public void cancel() throws InterruptedException {
		signalToTerminate();
		join();
	}

	/**
	 * Pauses the engine.
	**/
	public synchronized void pause() {
		if(!isPaused) {
			if(loop != null && pdEntry == null) {
				loop.signalToSuspend();
			}
			if(unbundler != null) {
				unbundler.signalToSuspend();
			}
			isPaused = true;
		}
	}

	/**
	 * Resumes the engine from a pause operation.
	**/
	public synchronized void resume() {
		if(isPaused) {
			if(loop != null && pdEntry == null) {
				loop.signalToResume();
			}
			if(unbundler != null) {
				unbundler.signalToResume();
			}
			isPaused = false;
		}
	}

	/**
	 * Return the total number of work files the system will produce.
	 * @return The total number of work files that this system will have produced by the time it
	 * completes. This is null when this total isn't yet known.
	**/
	Integer getHowManyOutboundBundlesWillBeCreated() {
		if(loop != null) {
			return loop.howManyOutboundBundlesWillBeCreated;
		} else {
			return null;
		}
	}

	/**
	 * Return the completion fraction within the startup phase of executing the model.
	 * @return null or a 0-1 (inclusive) number representing the progress through
	 * the setup activities of the model.  This is null when not yet known.
	**/
	Double getStartupFractionComplete() {
		if(loop != null) {
			synchronized(loop) {
				return loop.startupFractionComplete;
			}
		} else {
			return null;
		}
	}

	/**
	 * Return the number of work files that have been successfully unjarred and processed so far.
	 * @return the number of work files that have been successfully unjarred and processed so far.
	 * This value is 0 when the system is not running.
	**/
	int getHowManyBundlesProcessedSoFar() {
		if(unbundler != null && !isTODOOnly) {
			return unbundler.processedWorkBundleCount;
		} else {
			return 0;
		}
	}

	/**
	 * Return the number of iterations that have been successfully completed so far.
	 * @return the number of iterations that have been successfully completed so far.
	 * This value is 0 when the system is not running.
	**/
	int getHowManyIterationsProcessedSoFar() {
		if(unbundler != null && !isTODOOnly) {
			return unbundler.processedIterationCount;
		} else {
			return 0;
		}
	}

	/**
	 * Logs the message to some medium based on the type, MOVESEngine is only concerned with
	 * errors of type Logger.RUN_ERROR.
	 * @param	logMessageCategory int value indicating the type of message.  The values should come from
	 * the static Logger message-type variables.
	 * @param	message the String to get logged.
	**/
	public void handleLog(LogMessageCategory logMessageCategory, String message) {
		if(logMessageCategory != LogMessageCategory.RUN_ERROR) {
			return;
		}
		// Also, if one of the MOVESEngineThreads generated this error, it will be the current
		// thread and thus, current county/month/year info can be determined (excluding the
		// HeartbeatDetectionThread).
		int pollutantID = -1;
		int processID = -1;
		int stateID = -1;
		int countyID = -1;
		int zoneID = -1;
		int linkID = -1;
		int yearID = -1;
		int monthID = -1;
		int dayID = -1;
		int hourID = -1;

		Thread currentThread = Thread.currentThread();
		String currentThreadName = currentThread.getName();

		synchronized(this) {
			if(activeRunID != 0) {
				logRunError(activeRunID, pollutantID, processID, stateID, countyID,
						zoneID, linkID, yearID, monthID, dayID, hourID, message);
			}
		}
	}

	/**
	 * Creates an MOVESError record from the inputs. The numeric inputs can be zero to indicate a
	 * high-level MOVESEngine error which would most likely occur at the very start or end of a
	 * run.
	 * @param runID The identifier assigned to this run
	 * @param pollutantID The pollutant being run
	 * @param processID The emission process being run
	 * @param stateID The state being run
	 * @param countyID The county being run
	 * @param zoneID The zone being run
	 * @param linkID The link being run
	 * @param yearID The year being run
	 * @param monthID The month being run
	 * @param dayID The day being run
	 * @param hourID The hour being run
	 * @param message The run message to be logged
	**/
	public void logRunError(int runID, int pollutantID, int processID, int stateID, int countyID,
			int zoneID, int linkID, int yearID, int monthID, int dayID, int hourID,
			String message) {
		String sql = "";
		try {
			Connection outputConnection = DatabaseConnectionManager.checkOutConnection
					(MOVESDatabaseType.OUTPUT);
			try {
				sql = "INSERT INTO MOVESError ("
								+"MOVESRunID, linkID, zoneID, countyID,"
								+"stateID, hourID, dayID, monthID, yearID, pollutantID,"
								+"processID, ErrorMessage) "
							+"VALUES ("
								+"?, ?, ?, ?,"
								+"?, ?, ?, ?, ?, ?,"
								+"?, ?)";
				PreparedStatement statement = outputConnection.prepareStatement(sql);
				statement.setInt(1, runID);
				if(linkID>=0) {
					statement.setLong(2, linkID);
				} else {
					statement.setNull(2,java.sql.Types.INTEGER);
				}
				if(zoneID>=0) {
					statement.setLong(3, zoneID);
				} else {
					statement.setNull(3,java.sql.Types.INTEGER);
				}
				if(countyID>=0) {
					statement.setLong(4, countyID);
				} else {
					statement.setNull(4,java.sql.Types.INTEGER);
				}
				if(stateID>=0) {
					statement.setInt(5, stateID);
				} else {
					statement.setNull(5,java.sql.Types.SMALLINT);
				}
				if(hourID>=0) {
					statement.setInt(6, hourID);
				} else {
					statement.setNull(6, java.sql.Types.SMALLINT);
				}
				if(dayID>=0) {
					statement.setInt(7, dayID);
				} else {
					statement.setNull(7,java.sql.Types.SMALLINT);
				}
				if(monthID>=0) {
					statement.setInt(8, monthID);
				} else {
					statement.setNull(8,java.sql.Types.SMALLINT);
				}
				if(yearID>=0) {
					statement.setInt(9, yearID);
				} else {
					statement.setNull(9,java.sql.Types.SMALLINT);
				}
				if(processID>=0) {
					statement.setInt(10, pollutantID);
				} else {
					statement.setNull(10,java.sql.Types.SMALLINT);
				}
				if(processID>=0) {
					statement.setInt(11, processID);
				} else {
					statement.setNull(11,java.sql.Types.SMALLINT);
				}
				// Truncate message length, if necessary
				String timeStamp = (new SimpleDateFormat()).format(new java.util.Date());
				message = timeStamp + " " + message;
				if(message.length() > 255) {
					statement.setString(12, message.substring(0, 254));
				} else {
					statement.setString(12, message);
				}
				SQLRunner.execute(statement,sql);
				statement.close();
			} catch(SQLException e) {
				Logger.logSqlError(e, "Unable to write error message to output database.", sql);
			} finally {
				DatabaseConnectionManager.checkInConnection
					(MOVESDatabaseType.OUTPUT, outputConnection);
			}
		} catch(InterruptedException e) {
		}
	}

	/**
	 * Removes entries for this master only from the shared work folder.  Removes
	 * TODO, InProgress, and DONE files.
	**/
	void clearSharedWorkFolder() {
		if(isTODOOnly) {
			// Do not remove our files if running in TODO-only mode.
			return;
		}
		FilenameFilter fileNameFilter = DistributedWorkFileName.buildFileNameFilter
			(SystemConfiguration.getTheSystemConfiguration().distributedMasterID,
			"*", null, "*");

		File[] files = SystemConfiguration.getTheSystemConfiguration().
				sharedDistributedFolderPath.listFiles(fileNameFilter);

		for(int i=0;i<files.length;i++) {
			try {
				files[i].delete();
			} catch(Exception e) {
				/**
				 * @issue Unable to remove shared work file: [*]
				 * @explain While stopping a simulation, MOVES was unable to remove one of
				 * its bundle files from the shared work folder.
				**/
				Logger.log(LogMessageCategory.DEBUG,"Unable to remove shared work file: "
						+ files[i].getName());
			}
		}
	}

	/** Resets all data used to estimate completion time **/
	synchronized void resetCompletionTimeEstimate() {
		generationEstimator = new TimeEstimator();
		workerEstimator = new TimeEstimator();
		processingEstimator = new TimeEstimator();
	}

	/** Called by the rest of the model when bundle generation actually begins **/
	synchronized void onStartedBundleGeneration() {
		long observationCount = getHowManyOutboundBundlesWillBeCreated().intValue();

		generationEstimator.setStartTime();
		generationEstimator.setExpectedObservationCount(observationCount);
		generationEstimator.fractionOfObservationsNeededForEstimate = 0.20;

		workerEstimator.setExpectedObservationCount(observationCount);
		workerEstimator.fractionOfObservationsNeededForEstimate = 0.10;

		processingEstimator.setExpectedObservationCount(observationCount);
		processingEstimator.fractionOfObservationsNeededForEstimate = 0.05;
	}

	/** Called by the rest of the model when each bundle is generated **/
	synchronized void onGeneratedBundle() {
		if(generationEstimator != null) {
			generationEstimator.addObservation();
		}
		if(workerEstimator != null && workerEstimator.startTimeMillis <= 0) {
			workerEstimator.setStartTime();
		}
	}

	/** Called the rest of the model when all bundle generation is complete **/
	synchronized void onDoneGeneratingBundles() {
		if(workerEstimator != null) {
			workerEstimator.resample();
		}
		if(processingEstimator != null) {
			processingEstimator.resample();
		}
		if(unbundler != null && (isTODOOnly || !ExecutionRunSpec.theExecutionRunSpec.willRunCalculators)) {
			synchronized(unbundler) {
				unbundler.shouldFinishImmediately = true;
			}
		}
	}

	/** Called by the rest of the model when a bundle is received from the workers **/
	synchronized void onReceivedBundle() {
		if(workerEstimator != null) {
			workerEstimator.addObservation();
		}
		if(processingEstimator != null && processingEstimator.startTimeMillis <= 0) {
			processingEstimator.setStartTime();
		}
	}

	/**
	 * Called by the rest of the model when a bundle has been received from the
	 * workers and subseequently processed by the master.
	**/
	synchronized void onProcessedBundle() {
		if(processingEstimator != null) {
			processingEstimator.addObservation();
		}
		notifyListeners();
	}

	/** Internal class used to report the amount of time remaining in the simulation **/
	public class CompletionEstimate {
		public String state = "";
		public long remainingMillis = -1;
	}

	/**
	 * Create an estimate for the amount of time remaining in the simulation.
	 * @return a CompletionEstimate object describing the current state and milliseconds
	 * remaining.  Depending upon the state, the remainingMillis entry may be < 0 indicating
	 * that the completion time is not yet known.  Never null.
	**/
	public synchronized CompletionEstimate estimateCompletion() {
		CompletionEstimate estimate = new CompletionEstimate();
		if(!isRunning()
				|| generationEstimator == null
				|| workerEstimator == null
				|| processingEstimator == null) {
			estimate.state = "Not running";
			return estimate;
		}
		long generationFinishTime = generationEstimator.estimateFinishTime();
		if(generationFinishTime <= 0) {
			estimate.state = "Generating bundles";
			return estimate;
		}

		long workerFinishTime = workerEstimator.estimateFinishTime();
		if(workerFinishTime <= 0) {
			estimate.state = "Workers are processing";
			return estimate;
		}

		long processingFinishTime = processingEstimator.estimateFinishTime();
		if(processingFinishTime <= 0) {
			estimate.state = "Receiving data from workers";
			return estimate;
		}

		long finishTime = Math.max(generationFinishTime,workerFinishTime);
		finishTime = Math.max(finishTime,processingFinishTime);

		long postProcessingAmount = 30 * 1000; // seconds * milliseconds/second
		finishTime += postProcessingAmount;

		long now = System.currentTimeMillis();
		estimate.remainingMillis = finishTime - now;
		if(estimate.remainingMillis <= 0) {
			estimate.state = "Finished";
			estimate.remainingMillis = 0;
			return estimate;
		}

		if(estimate.remainingMillis <= postProcessingAmount) {
			estimate.state = "Post-Processing";
			return estimate;
		}

		estimate.state = "Estimated Time Remaining";
		return estimate;
	}

	/**
	 * Record the start of an event in the MOVESEventLog table for the current run.
	 * @param name of the event
	 * @return eventRecordID used in the call to logEventStop when the event is over.
	**/
	public static int logEventStart(String name) {
		if(!CompilationFlags.SHOULD_LOG_TIMING_EVENTS) {
			return 1; // return a fake number so the caller doesn't think this failed
		}

		synchronized(theInstance) {
			if(theInstance.activeRunID <= 0) {
				return 0;
			}
			long now = System.currentTimeMillis() - theInstance.startTimeMillis;
			String sql;
			sql = "INSERT INTO MOVESEventLog (EventRecordID,MOVESRunID,EventName,WhenStarted) "
					+ "VALUES (" + theInstance.nextEventRecordID + ","
					+ theInstance.activeRunID + ",'" + name + "'," + now + ")";
			Connection db = null;
			try {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
				SQLRunner.executeSQL(db,sql);
				int eventRecordID = theInstance.nextEventRecordID;
				theInstance.nextEventRecordID++;
				return eventRecordID;
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to write start time to MOVESEventLog",sql);
				return 0;
			} finally {
				if(db != null) {
					DatabaseConnectionManager.checkInConnection
							(MOVESDatabaseType.OUTPUT, db);
				}
			}
		}
	}

	/**
	 * Record the completion of an event in the MOVESEventLog table for the current run.
	 * @param eventRecordID value returned by a previous call to logEventStart
	**/
	public static void logEventStop(int eventRecordID) {
		if(!CompilationFlags.SHOULD_LOG_TIMING_EVENTS) {
			return;
		}

		if(eventRecordID <= 0) {
			return;
		}
		synchronized(theInstance) {
			if(theInstance.activeRunID <= 0) {
				return;
			}
			long now = System.currentTimeMillis() - theInstance.startTimeMillis;
			String sql;
			sql = "UPDATE MOVESEventLog SET WhenStopped=" + now
					+ " WHERE EventRecordID=" + eventRecordID
					+ " AND MOVESRunID=" + theInstance.activeRunID;
			Connection db = null;
			try {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to write stop time to MOVESEventLog",sql);
			} finally {
				if(db != null) {
					DatabaseConnectionManager.checkInConnection
							(MOVESDatabaseType.OUTPUT, db);
				}
			}
		}
	}

	/**
	 * Perform any final adjustments to the MOVESEventLog table such as calculating
	 * durations.
	**/
	static void finalizeEventLog() {
		if(!CompilationFlags.SHOULD_LOG_TIMING_EVENTS) {
			return;
		}

		synchronized(theInstance) {
			if(theInstance.activeRunID <= 0) {
				return;
			}
			String sql;
			sql = "UPDATE MOVESEventLog SET Duration=WhenStopped-WhenStarted "
					+ "WHERE WhenStopped IS NOT NULL";
			Connection db = null;
			try {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to finalize MOVESEventLog",sql);
			} finally {
				if(db != null) {
					DatabaseConnectionManager.checkInConnection
							(MOVESDatabaseType.OUTPUT, db);
				}
			}
		}
	}

	/**
	 * Check permission to perform unit conversions, aggregation, and use integrated post processors.
	 * When processing DONE files from a prior run, these operations are not allowed.
	 * @return true when unit conversions, aggregation, and use of integrated post processors are allowed.
	**/
	public boolean allowFinalPostProcessing() {
		return pdEntry == null;
	}

	/**
	 * Notify the system that MOVES should stop processing prematurely.
	**/
	public static void terminalErrorFound() {
		if(theInstance != null) {
			if(!theInstance.shouldStopProcessing) {
				theInstance.shouldStopProcessing = true;
				try {
					theInstance.signalToTerminate();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
	}

	/** True when a module has decided that the simulation should be stopped prematurely. **/
	public static boolean isTerminalErrorFound() {
		if(theInstance != null) {
			return theInstance.shouldStopProcessing;
		}
		return false;
	}
}
