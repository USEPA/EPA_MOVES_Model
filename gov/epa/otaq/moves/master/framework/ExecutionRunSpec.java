/**************************************************************************************************
 * @(#)ExecutionRunSpec.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy;
import gov.epa.otaq.moves.master.implementation.ghg.TOGSpeciationCalculator;
import java.sql.*;
import java.util.*;
import java.io.*;

/**
 * Execution-time extension to a RunSpec. This class contains variables and methods that are used
 * only while a RunSpec is being executed.
 *
 * @author		Wesley Faler
 * @author		Jarrod Brown
 * @version		2018-06-21
**/
public class ExecutionRunSpec {
	/** The singleton instance **/
	static public ExecutionRunSpec theExecutionRunSpec = null;
	/** The RunSpec being executed. **/
	private RunSpec targetRunSpec;
	/** SQLs used to delete unwanted pollutant/processes from the output on the Worker. **/
	public Vector<String> removeUnwantedDataOnWorkerSQLs;
	/** SQLs used to adjust for retrofit effects **/
	public Vector<String> retrofitSQLs;
	/** SQLs used to perform output aggregation - processed in Worker. **/
	public Vector<String> workerSQLs;
	/** SQLs used to perform output aggregation - processed in outputProcessor. **/
	public Vector<String> outputProcessorSQLs;
	/** SQLs used to perform output aggregation - processed during final Processing. **/
	public Vector<String> finalProcessSQLs;
	/** List of SCC codes for selected off-road vehicles. **/
	TreeSet<String> runQueueSCCs = new TreeSet<String>();
	/** Object used to produce the list of locations the Master Loop will be executed for **/
	ExecutionLocationProducer locationProducer;
	/** List of locations the Master Loop will be executed for. **/
	TreeSet<ExecutionLocation> executionLocations;
	/** The collection of EmissionProcesses that are targeted for simulation. **/
	public TreeSet<EmissionProcess> targetProcesses = new TreeSet<EmissionProcess>();
	/** The collection of Pollutants that are targeted for simulation. **/
	TreeSet<Pollutant> targetPollutants = new TreeSet<Pollutant>();
	/** The collection of polProcessID Integer objects that are targeted for simulation. **/
	public TreeSet<Integer> targetPollutantProcesses = new TreeSet<Integer>();
	/** A collection of state IDs in ascending order. **/
	TreeSet<Integer> states = new TreeSet<Integer>();
	/** A collection of county IDs in ascending order. **/
	TreeSet<Integer> counties = new TreeSet<Integer>();
	/** A collection of zone IDs in ascending order. **/
	TreeSet<Integer> zones = new TreeSet<Integer>();
	/** A collection of link IDs in ascending order. **/
	TreeSet<Integer> links = new TreeSet<Integer>();
	/**
	 * PollutantProcessAssociation objects to be used in the actual simulation. Due to
	 * dependencies between calculators and pollutants/processes, this may not be the
	 * exact same set as modified by the user - it may have more entries.
	**/
	public TreeSet<PollutantProcessAssociation> pollutantProcessAssociations
			= new TreeSet<PollutantProcessAssociation>();
	/** A collection of years in ascending order. **/
	public TreeSet<Integer> years = new TreeSet<Integer>();
	/** A collection of fuel years in ascending order. **/
	public TreeSet<Integer> fuelYears = new TreeSet<Integer>();
	/** A collection of month IDs in ascending order. **/
	public TreeSet<Integer> months = new TreeSet<Integer>();
	/** A collection of month group IDs in ascending order. **/
	public TreeSet<Integer> monthGroups = new TreeSet<Integer>();
	/** A collection of day IDs in ascending order. **/
	public TreeSet<Integer> days = new TreeSet<Integer>();
	/** A collection of hour IDs in ascending order. **/
	public TreeSet<Integer> hours = new TreeSet<Integer>();
	/** A collection of hour day IDs in ascending order. **/
	TreeSet<Integer> hourdays = new TreeSet<Integer>();

	/** source type IDs used in the simulation as Integer objects **/
	public TreeSet<Integer> sourceTypes = new TreeSet<Integer>();
	/** fuel type IDs used in the simulation as Integer objects **/
	public TreeSet<Integer> fuelTypes = new TreeSet<Integer>();

	/** A collection of region IDs in ascending order. **/
	public TreeSet<Integer> regions = new TreeSet<Integer>();

	/** sector IDs used in the simulation as Integer objects **/
	public TreeSet<Integer> sectors = new TreeSet<Integer>();

	/** false if advanced performance features have disabled execution of calculators **/
	public boolean willRunCalculators = true;

	/** true after errors have been reported and data integrity checked **/
	boolean didCheckForIntegrity = false;
	/** aggregation generator **/
	AggregationSQLGenerator aggregationSQLGenerator = null;
	/** Internal override flag that when set to true forces worker databases to be kept **/
	boolean shouldKeepWorkerDatabases = false;

	/**
	 * Set of pollutants and processes that are calculated in way that results in records
	 * that can be aggregated by pollutant and process.
	**/
	TreeSet<PollutantProcessAssociation> pollutantProcessesNeedingAggregation
			= new TreeSet<PollutantProcessAssociation>();
	/**
	 * Set of pollutants that are calculated in way that results in records
	 * that can be aggregated by pollutant and process.
	**/
	TreeSet<Pollutant> pollutantsNeedingAggregation = new TreeSet<Pollutant>();

	// /** True if the warning about MOVES lacking all motorcycle information has been displayed. **/
	//boolean didWarnAboutMotorcycles = false;

	/**
	 * Translate user-space model years to standardized model years.
	**/
	ModelYearMapper modelYearMapper = null;

	/** Set of roads to be used. Only filled when ramps should be separated **/
	TreeSet<RoadType> executionRoadTypes = null;
	/**
	 * Road types to add in addition to what is given in the runspec.
	 * Only filled when ramps should be separated.
	**/
	TreeSet<RoadType> extraRoadTypes = null;

	/**
	 * Constructor.
	 * @param runSpec The runspec being executed.
	**/
	public ExecutionRunSpec(RunSpec runSpec) {
		theExecutionRunSpec = this;
		targetRunSpec = runSpec;

		willRunCalculators = shouldExecute(EmissionCalculator.class);
	}

	/**
	 * Constructor for use as general utility for decoding RunSpec objects.
	 * @param runSpec The runspec being executed.
	 * @param justAUtility true if only being used for utilitarian reasons and should
	 * not establish itself as the ExecutionRunSpec singleton.
	**/
	public ExecutionRunSpec(RunSpec runSpec, boolean justAUtility) {
		targetRunSpec = runSpec;
		if(!justAUtility) {
			theExecutionRunSpec = this;
			willRunCalculators = shouldExecute(EmissionCalculator.class);
		}
	}

	/**
	 * Performs several one time initialization operations.
	 * Must be called only <b>before</b> InputDataManager
	 * has copied data into the Execution database.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void initializeBeforeExecutionDatabase() throws InterruptedException {
		int eventRecordID = MOVESEngine.logEventStart("initializeBeforeExecutionDatabase");
		Connection defaultDB =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		try {
			BundleUtilities.resetCachedData();

			for(Iterator<PollutantProcessAssociation>
						i = targetRunSpec.pollutantProcessAssociations.iterator(); i.hasNext();) {
				PollutantProcessAssociation iterAssociation = i.next();
				targetProcesses.add(iterAssociation.emissionProcess);
				targetPollutants.add(iterAssociation.pollutant);
				targetPollutantProcesses.add(Integer.valueOf(iterAssociation.getDatabaseKey(defaultDB)));
				pollutantProcessAssociations.add(iterAssociation);
			}
			flagRequiredPollutantProcesses(defaultDB);
			addLumpedSpecies(defaultDB);

			buildExecutionTimeSpan(defaultDB,true);

			locationProducer = new ExecutionLocationProducer(targetRunSpec);
			executionLocations = locationProducer.buildExecutionLocations(defaultDB);
			extractLocationDetailsFromExecutionLocations();
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,defaultDB);
			defaultDB = null;
			MOVESEngine.logEventStop(eventRecordID);
		}
	}

	/**
	 * Reload the execution locations from the MOVESExecution database after it has had the domain
	 * database merged into it.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void initializeLocationsAfterDomainDatabase() throws InterruptedException {
		Connection executionDB =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
		try {
			locationProducer = new ExecutionLocationProducer(targetRunSpec);
			executionLocations = locationProducer.buildExecutionLocations(executionDB);
			extractLocationDetailsFromExecutionLocations();
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,executionDB);
			executionDB = null;
		}
	}

	/**
	 * Performs several one time initialization operations.
	 * Must be called only <b>after</b> InputDataManager
	 * has copied data into the Execution database's Year and regionCounty
	 * tables.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void initializeAfterShallowTables() throws InterruptedException {
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		Connection executionDB =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
		try {
			// Learn fuel years from the year table and the years in the runspec.
			// The Year table has not been filtered, so use the years in the runspec
			// for filters.
			if(years.size() > 0) {
				sql = "";
				for(Integer y : years) {
					if(sql.length() > 0) {
						sql += ",";
					}
					sql += y;
				}
				sql = "select distinct fuelYearID from year where yearID in (" + sql + ")";
				fuelYears.clear();
				query.open(executionDB,sql);
				while(query.rs.next()) {
					fuelYears.add(Integer.valueOf(query.rs.getInt(1)));
				}
				query.close();
			}
			// Learn regions from the regionCounty table. This has already been filtered
			// for the counties in the runspec.
			regions.clear();
			regions.add(Integer.valueOf(0));
			sql = "select distinct regionID from regionCounty";
			query.open(executionDB,sql);
			while(query.rs.next()) {
				regions.add(Integer.valueOf(query.rs.getInt(1)));
			}
			query.close();
			// Learn month groups using the runspec months as a filter.
			if(months.size() > 0) {
				sql = "";
				for(Integer m : months) {
					if(sql.length() > 0) {
						sql += ",";
					}
					sql += m;
				}
				sql = "select distinct monthGroupID from monthOfAnyYear where monthID in (" + sql + ")";
				monthGroups.clear();
				query.open(executionDB,sql);
				while(query.rs.next()) {
					monthGroups.add(Integer.valueOf(query.rs.getInt(1)));
				}
				query.close();
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,"Unable to query year and regionCounty tables",sql);
		} finally {
			query.onFinally();
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,executionDB);
			executionDB = null;
		}
	}

	/**
	 * Performs several one time initialization operations.
	 * Must be called only <b>after</b> InputDataManager
	 * has copied data into the Execution database.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void initializeAfterExecutionDatabase() throws InterruptedException {
		int eventRecordID = MOVESEngine.logEventStart("initializeAfterExecutionDatabase");
		Connection executionDB =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
		try {
			addIndexes(executionDB);

			for (Iterator i = targetRunSpec.pollutantProcessAssociations.iterator(); i.hasNext();) {
				PollutantProcessAssociation iterAssociation = (PollutantProcessAssociation) i.next();
				targetProcesses.add(iterAssociation.emissionProcess);
				targetPollutants.add(iterAssociation.pollutant);
				targetPollutantProcesses.add(Integer.valueOf(iterAssociation.getDatabaseKey(executionDB)));
				pollutantProcessAssociations.add(iterAssociation);
			}
			flagRequiredPollutantProcesses(executionDB);

			buildExecutionTimeSpan(executionDB,false);

			buildNonLocationFilterTables();
			if(targetRunSpec.scale == ModelScale.MESOSCALE_LOOKUP
					&& targetRunSpec.domain != ModelDomain.PROJECT) {
				createMesoscaleLookupLinks(executionDB);
			}

			locationProducer = new ExecutionLocationProducer(targetRunSpec);
			executionLocations = locationProducer.buildExecutionLocations(executionDB);
			extractLocationDetailsFromExecutionLocations();

			buildLocationFilterTables();
			runAdditionalSetupScript(executionDB);

			setupMacroExpander(executionDB);
			ExtractedDataCache.clear();

			modelYearMapper = new ModelYearMapper();
			String sql = "";
			try {
				modelYearMapper.buildMappings(executionDB);
				sql = "insert ignore into PollutantProcessMappedModelYear (polProcessID,modelYearID,modelYearGroupID,fuelMYGroupID,IMModelYearGroupID)"
						+ " select"
						+ " 	polProcessID,"
						+ " 	MYRMAP(modelYearID) as modelYearID,"
						+ " 	modelYearGroupID,"
						+ " 	fuelMYGroupID,"
						+ " 	IMModelYearGroupID"
						+ " from pollutantProcessModelYear";
				sql = modelYearMapper.findAndConvert(sql);
				SQLRunner.executeSQL(executionDB,sql);
			} catch(SQLException e) {
				Logger.logError(e,"Unable to build model year mappings");
			}
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,executionDB);
			executionDB = null;
			MOVESEngine.logEventStop(eventRecordID);
		}
	}

	/**
	 * Performs several before iteration initialization operations.
	 * Must be called only <b>after</b> InputDataManager
	 * has copied data into the Execution database.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void initializeBeforeIteration() throws InterruptedException {
		int eventRecordID = MOVESEngine.logEventStart("initializeBeforeIteration");
		boolean alreadyAdded = false;
		if(aggregationSQLGenerator == null) {
			aggregationSQLGenerator = new AggregationSQLGenerator();
			alreadyAdded = false;
		} else {
			alreadyAdded = true;
		}
		if(!aggregationSQLGenerator.generateReportSQL()) {
			if(!didCheckForIntegrity) {
				/** @explain An error occurred while creating aggregation SQL statements. **/
				Logger.log(LogMessageCategory.ERROR, "Problem while generating Report SQL");
			}
		} else {
			workerSQLs = aggregationSQLGenerator.workerSQLs;

			if(retrofitSQLs != null) {
				OnRoadRetrofitStrategy t = new OnRoadRetrofitStrategy();
				if(this.shouldExecute(t.getClass())) {
					if(workerSQLs == null) {
						workerSQLs = retrofitSQLs;
					} else if(!alreadyAdded) {
						// Add at the beginning, pushing existing SQL down
						workerSQLs.addAll(0,retrofitSQLs);
					}
					if(!alreadyAdded && shouldSaveData(t.getClass())) {
						shouldKeepWorkerDatabases = true;
						// NOTE: The following are in reverse order due to their need to
						// be at the beginning of the SQL list
						workerSQLs.add(0,"create table MOVESWorkerOutput_BeforeRetrofit"
								+ " select * from MOVESWorkerOutput;");
						workerSQLs.add(0,"drop table if exists MOVESWorkerOutput_BeforeRetrofit;");
					}
				}
			}
			if(removeUnwantedDataOnWorkerSQLs != null) {
				if(workerSQLs == null) {
					workerSQLs = removeUnwantedDataOnWorkerSQLs;
				} else if(!alreadyAdded) {
					// Add at the beginning, pushing existing SQL down
					// This ensures minimal table size before retrofit
					workerSQLs.addAll(0,removeUnwantedDataOnWorkerSQLs);
				}
			}
			outputProcessorSQLs = aggregationSQLGenerator.outputProcessorSQLs;
			finalProcessSQLs = aggregationSQLGenerator.finalProcessSQLs;
		}

		if(!didCheckForIntegrity) {
			checkDataIntegrity();
		}

		MOVESEngine.logEventStop(eventRecordID);

		didCheckForIntegrity = true;
	}

	/**
	 * Builds the TimeSpan used during execution.
	 * @param targetDB The database connection to build the TimeSpan selections from.
	 * @param useRunSpec true if the RunSpec's data should be used.  Used only when reading
	 * from the default database.  Set to false when reading from the Execution database
	 * after aggregation operations.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void buildExecutionTimeSpan(Connection targetDB,boolean useRunSpec)
			throws InterruptedException {
		String sql = "";
		try {
			ResultSet results;

			// Initialize years.  This is always from the RunSpec.
			for(Iterator y=targetRunSpec.timeSpan.years.iterator();y.hasNext();) {
				years.add((Integer)y.next());
			}
			// Build the months selections.  MonthOfAnyYear is not filtered but it is aggregated.
			// Thus, the RunSpec itself is used when reading from the default database.  When
			// reading from the execution database, if no pseudo-months have been created, then
			// continue to use the RunSpec's data, otherwise just load what the database actually
			// has.
			months.clear();
			if(useRunSpec
					|| getTimeSpan().aggregateBy.compareTo(OutputTimeStep.YEAR) < 0) {
				for(Iterator i=targetRunSpec.timeSpan.months.iterator();i.hasNext();) {
					TimeSpan.Month m = (TimeSpan.Month)i.next();
					months.add(Integer.valueOf(m.monthID));
				}
			} else {
				// The "YEAR" aggregation option uses one or more pseudo-months, so
				// just load these months now.
				sql = "SELECT monthID FROM MonthOfAnyYear ORDER BY monthID";
				results = SQLRunner.executeQuery(targetDB,sql);
				for(int i=0;results.next();i++) {
					months.add(Integer.valueOf(results.getInt(1)));
				}
				results.close();
			}
			// Build the days selections
			days.clear();
			if(useRunSpec) {
				for(Iterator i=targetRunSpec.timeSpan.days.iterator();i.hasNext();) {
					TimeSpan.Day d = (TimeSpan.Day)i.next();
					days.add(Integer.valueOf(d.dayID));
				}
			} else {
				sql = "SELECT dayID FROM DayOfAnyWeek ORDER BY dayID";
				results = SQLRunner.executeQuery(targetDB,sql);
				for(int i=0;results.next();i++) {
					days.add(Integer.valueOf(results.getInt(1)));
				}
				results.close();
			}
			// Build the hours selections.  HourOfAnyDay is never filtered, so the RunSpec is
			// always used unless time aggregation occurred.
			hours.clear();
			boolean didAggregateHours = false;
			ModelScale scale = getModelScale();
			if(ModelScale.MACROSCALE == scale || ModelScale.MESOSCALE_LOOKUP == scale) {
				if(getTimeSpan().aggregateBy != OutputTimeStep.HOUR) {
					didAggregateHours = true;
				}
			}
			if(useRunSpec || !didAggregateHours) {
				for(Iterator i=targetRunSpec.timeSpan.allHours.iterator();i.hasNext();) {
					TimeSpan.Hour h = (TimeSpan.Hour)i.next();
					if(targetRunSpec.timeSpan.beginHourID <= h.hourID
							&& targetRunSpec.timeSpan.endHourID >= h.hourID) {
						hours.add(Integer.valueOf(h.hourID));
						//System.out.println("Add runSpec hour " + h.hourID + " between " + targetRunSpec.timeSpan.beginHourID + " and " + targetRunSpec.timeSpan.endHourID);
					}
				}
			} else {
				sql = "SELECT hourID FROM HourOfAnyDay ORDER BY hourID";
				results = SQLRunner.executeQuery(targetDB,sql);
				for(int i=0;results.next();i++) {
					hours.add(Integer.valueOf(results.getInt(1)));
				}
				results.close();
			}
			// Build HourDay selections.  This is always from the database and based
			// upon the hour and day IDs already gathered above.
			sql="";
			for(Iterator h=hours.iterator();h.hasNext();) {
				Integer hour = (Integer)h.next();
				for(Iterator d=days.iterator();d.hasNext();) {
					Integer day = (Integer)d.next();
					if(sql.length()==0) {
						sql = "SELECT hourDayID FROM HourDay WHERE ";
					} else {
						sql += " OR ";
					}
					sql += "(dayID=" + day + " AND hourID=" + hour + ")";
				}
				results = SQLRunner.executeQuery(targetDB,sql);
				hourdays.clear();
				for(int i=0;results.next();i++) {
					hourdays.add(Integer.valueOf(results.getInt(1)));
				}
				results.close();
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,"An SQL exception occurred while building the runspec filter "+
					"sets.", sql);
		}
	}

	/**
	 * Examine the pollutant/process selections, looking for other required selections.  If one
	 * is found, add it but also arrange for its data to be deleted from the results since the
	 * user (nor any GUI logic) selected it.
	 * @param db database to use
	**/
	void flagRequiredPollutantProcesses(Connection db) {
		if(targetRunSpec.models.contains(Model.NONROAD)) {
			// Nonroad has no silent pollutants/processes added for the user.
			return;
		}

		//removeUnwantedDataOnWorkerSQLs = null;
		// Format of needs[] array:
		// output pollutant, output process, required input pollutant, required input process
		String[] needs = {
			/*
			"Primary PM10 - Elemental Carbon", "Crankcase Running Exhaust",
				"Elemental Carbon", "Crankcase Running Exhaust",
			"Elemental Carbon", "Crankcase Running Exhaust",
				"Elemental Carbon", "Running Exhaust",
			"Primary PM10 - Elemental Carbon", "Running Exhaust",
				"Elemental Carbon", "Running Exhaust",

			"Primary PM10 - Elemental Carbon", "Crankcase Start Exhaust",
				"Elemental Carbon", "Crankcase Start Exhaust",
			"Elemental Carbon", "Crankcase Start Exhaust",
				"Elemental Carbon", "Start Exhaust",
			"Primary PM10 - Elemental Carbon", "Start Exhaust",
				"Elemental Carbon", "Start Exhaust",

			"Primary PM10 - Elemental Carbon", "Crankcase Extended Idle Exhaust",
				"Elemental Carbon", "Crankcase Extended Idle Exhaust",
			"Elemental Carbon", "Crankcase Extended Idle Exhaust",
				"Elemental Carbon", "Extended Idle Exhaust",
			"Primary PM10 - Elemental Carbon", "Extended Idle Exhaust",
				"Elemental Carbon", "Extended Idle Exhaust",

			"Primary PM10 - Organic Carbon", "Crankcase Running Exhaust",
				"Organic Carbon", "Crankcase Running Exhaust",
			"Organic Carbon", "Crankcase Running Exhaust",
				"Organic Carbon", "Running Exhaust",
			"Primary PM10 - Organic Carbon", "Running Exhaust",
				"Organic Carbon", "Running Exhaust",

			"Primary PM10 - Organic Carbon", "Crankcase Start Exhaust",
				"Organic Carbon", "Crankcase Start Exhaust",
			"Organic Carbon", "Crankcase Start Exhaust",
				"Organic Carbon", "Start Exhaust",
			"Primary PM10 - Organic Carbon", "Start Exhaust",
				"Organic Carbon", "Start Exhaust",

			"Primary PM10 - Organic Carbon", "Crankcase Extended Idle Exhaust",
				"Organic Carbon", "Crankcase Extended Idle Exhaust",
			"Organic Carbon", "Crankcase Extended Idle Exhaust",
				"Organic Carbon", "Extended Idle Exhaust",
			"Primary PM10 - Organic Carbon", "Extended Idle Exhaust",
				"Organic Carbon", "Extended Idle Exhaust",

			"Primary PM10 - Sulfate Particulate", "Crankcase Running Exhaust",
				"Sulfate Particulate", "Crankcase Running Exhaust",
			"Sulfate Particulate", "Crankcase Running Exhaust",
				"Sulfate Particulate", "Running Exhaust",
			"Sulfate Particulate", "Running Exhaust",
				"Total Energy Consumption", "Running Exhaust",

			"Primary PM10 - Sulfate Particulate", "Crankcase Start Exhaust",
				"Sulfate Particulate", "Crankcase Start Exhaust",
			"Sulfate Particulate", "Crankcase Start Exhaust",
				"Sulfate Particulate", "Start Exhaust",
			"Sulfate Particulate", "Start Exhaust",
				"Total Energy Consumption", "Start Exhaust",

			"Primary PM10 - Sulfate Particulate", "Crankcase Extended Idle Exhaust",
				"Sulfate Particulate", "Crankcase Extended Idle Exhaust",
			"Sulfate Particulate", "Crankcase Extended Idle Exhaust",
				"Sulfate Particulate", "Extended Idle Exhaust",
			"Sulfate Particulate", "Extended Idle Exhaust",
				"Total Energy Consumption", "Extended Idle Exhaust",

			"Primary PM10 - Sulfate Particulate", "Running Exhaust",
				"Sulfate Particulate", "Running Exhaust",
			"Primary PM10 - Sulfate Particulate", "Start Exhaust",
				"Sulfate Particulate", "Start Exhaust",
			"Primary PM10 - Sulfate Particulate", "Extended Idle Exhaust",
				"Sulfate Particulate", "Extended Idle Exhaust",
			*/

			"Total Gaseous Hydrocarbons", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Total Gaseous Hydrocarbons", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Total Gaseous Hydrocarbons", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",
			"Non-Methane Hydrocarbons", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Non-Methane Hydrocarbons", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Non-Methane Hydrocarbons", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",
			"Non-Methane Organic Gases", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Non-Methane Organic Gases", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Non-Methane Organic Gases", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",
			"Total Organic Gases", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Total Organic Gases", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Total Organic Gases", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",
			"Volatile Organic Compounds", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Volatile Organic Compounds", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Volatile Organic Compounds", "Refueling Displacement Vapor Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",

			"Total Gaseous Hydrocarbons", "Refueling Spillage Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Total Gaseous Hydrocarbons", "Refueling Spillage Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Total Gaseous Hydrocarbons", "Refueling Spillage Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",
			"Non-Methane Hydrocarbons", "Refueling Spillage Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Non-Methane Hydrocarbons", "Refueling Spillage Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Non-Methane Hydrocarbons", "Refueling Spillage Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",
			"Non-Methane Organic Gases", "Refueling Spillage Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Non-Methane Organic Gases", "Refueling Spillage Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Non-Methane Organic Gases", "Refueling Spillage Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",
			"Total Organic Gases", "Refueling Spillage Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Total Organic Gases", "Refueling Spillage Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Total Organic Gases", "Refueling Spillage Loss",
				"Total Energy Consumption", "Extended Idle Exhaust",
			"Volatile Organic Compounds", "Refueling Spillage Loss",
				"Total Energy Consumption", "Running Exhaust",
			"Volatile Organic Compounds", "Refueling Spillage Loss",
				"Total Energy Consumption", "Start Exhaust",
			"Volatile Organic Compounds", "Refueling Spillage Loss",
				"Total Energy Consumption", "Extended Idle Exhaust"
		};
		boolean done = false;
		while(!done) {
			done = true;
			for(int i=0;i<needs.length;i+=4) {
				if(doesHavePollutantAndProcess(needs[i+0],needs[i+1])) {
					if(require(db,needs[i+2],needs[i+3])) {
						done = false;
					}
				}
			}
		}
	}

	/**
	 * Add lumped species pollutants to the results if any mechansim was selected.
	 * @param db database to use
	**/
	void addLumpedSpecies(Connection db) {
		try {
			ArrayList<Pollutant> mechanismPollutants = TOGSpeciationCalculator.getMechanismPollutants(db);
			for(PollutantProcessAssociation ppa : targetRunSpec.pollutantProcessAssociations) {
				if(!mechanismPollutants.contains(ppa.pollutant)) {
					continue;
				}
				ArrayList<EmissionProcess> mechanismProcesses = TOGSpeciationCalculator.getMechanismProcesses(db,ppa.pollutant);
				if(!mechanismProcesses.contains(ppa.emissionProcess)) {
					continue;
				}
				// Add NonHAPTOG (88).
				Pollutant nonhapTOG = Pollutant.findByID(88);
				if(nonhapTOG != null) {
					PollutantProcessAssociation nht = PollutantProcessAssociation.createByID(88,ppa.emissionProcess.databaseKey);
					if(nht != null && !ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(nht.pollutant,nht.emissionProcess)) {
						targetPollutantProcesses.add(Integer.valueOf(nht.getDatabaseKey(db)));
						pollutantProcessAssociations.add(nht);
						targetPollutants.add(nonhapTOG);
					}
				}
				// Add lumped species
				ArrayList<PollutantProcessAssociation> lumpedSpecies = TOGSpeciationCalculator.getMechanismLumpedSpecies(db,ppa);
				for(PollutantProcessAssociation r : lumpedSpecies) {
					if(!ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(r.pollutant,r.emissionProcess)) {
						targetPollutantProcesses.add(Integer.valueOf(r.getDatabaseKey()));
						pollutantProcessAssociations.add(r);
						targetPollutants.add(r.pollutant);
					}
				}
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to add lumped species to the internal set of pollutants");
		}
	}

	/**
	 * Require a pollutant/process.  If not already present, it will be flagged so that its results
	 * are removed at the end of calculations.
	 * @param db database to use
	 * @param pollutantName textual name of a pollutant that is required, may not be "" or null
	 * @param emissionProcessName textual name of an emission process that is required, may not
	 * be "" or null
	 * @return true if the pollutant/process was added and not already present, false if already
	 * present (either originally or as a result of other calls to require()).
	**/
	public boolean require(Connection db, String pollutantName, String emissionProcessName) {
		if(doesHavePollutantAndProcess(pollutantName,emissionProcessName)) {
			return false;
		}
		Pollutant pollutant = Pollutant.findByName(pollutantName);
		EmissionProcess emissionProcess = EmissionProcess.findByName(emissionProcessName);
		if(pollutant == null || emissionProcess == null) {
			/**
			 * @explain An internally required pollutant/process could not be found within
			 * the default database.  If using a customized database, ensure that the named
			 * pollutant/process is properly represented within the database.
			**/
			Logger.log(LogMessageCategory.ERROR,"Unknown, but required, pollutant/process: "
					+ pollutantName + "/" + emissionProcessName);
			return false;
		}
		PollutantProcessAssociation ppa = PollutantProcessAssociation.createByName(
				pollutantName, emissionProcessName);
		targetProcesses.add(ppa.emissionProcess);
		targetPollutants.add(ppa.pollutant);
		targetPollutantProcesses.add(Integer.valueOf(ppa.getDatabaseKey(db)));
		pollutantProcessAssociations.add(ppa);
		Logger.log(LogMessageCategory.INFO,"Adding:(" + ppa.getDatabaseKey(db) + ") "
				+ pollutantName + "/" + emissionProcessName);
		if(removeUnwantedDataOnWorkerSQLs == null) {
			removeUnwantedDataOnWorkerSQLs = new Vector<String>();
		}
		String sql = "delete from MOVESWorkerOutput where pollutantID=" + ppa.pollutant.databaseKey
				+ " and processID=" + ppa.emissionProcess.databaseKey + ";";
		removeUnwantedDataOnWorkerSQLs.add(sql);
		return true;
	}

	/**
	 * Checks the pollutants and processes associated with the ExecutionRunSpec to see if the
	 * requested combination is selected.  Wildcard-type functionality is obtained by passing
	 * "" or null for one of the two required names.
	 * @param pollutantName textual name of a pollutant to search for or "" or null to match any
	 * pollutant in the selected process
	 * @param emissionProcessName textual name of an emission process to search for or "" or null
	 * to match any process for the selected pollutant
	 * @return true if the requested combination or wildcard match is found, false if both
	 * parameters are "" or null
	**/
	public boolean doesHavePollutantAndProcess(String pollutantName,String emissionProcessName) {
		Pollutant pollutant = null;
		if(pollutantName != null && pollutantName.length() > 0) {
			pollutant = Pollutant.findByName(pollutantName);
			if(pollutant == null) {
				// The code gets here when there is a pollutantName (non-null and non-"") passed
				// in but that pollutant can't be found in the database.
				return false;
			}
		}

		EmissionProcess emissionProcess = null;
		if(emissionProcessName != null && emissionProcessName.length() > 0) {
			emissionProcess = EmissionProcess.findByName(emissionProcessName);
			if(emissionProcess == null) {
				// The code gets here when there is an emissionProcessName
				// (non-null and non-"") passed/ in but that emission process can't
				// be found in the database.
				return false;
			}
		}
		return doesHavePollutantAndProcess(pollutant,emissionProcess);
	}

	/**
	 * Checks the pollutants and processes associated with the ExecutionRunSpec to see if the
	 * requested combination is selected.  Wildcard-type functionality is obtained by passing
	 * "" or null for one of the two required names.
	 * @param pollutantID ID of a pollutant to search for or 0 to match any
	 * pollutant in the selected process
	 * @param emissionProcessID ID of an emission process to search for or 0
	 * to match any process for the selected pollutant
	 * @return true if the requested combination or wildcard match is found, false if both
	 * parameters are 0
	**/
	public boolean doesHavePollutantAndProcess(int pollutantID,int emissionProcessID) {
		Pollutant pollutant = null;
		if(pollutantID > 0) {
			pollutant = Pollutant.findByID(pollutantID);
			if(pollutant == null) {
				// The code gets here when there is a pollutantID (non-0) passed
				// in but that pollutant can't be found in the database.
				return false;
			}
		}

		EmissionProcess emissionProcess = null;
		if(emissionProcessID > 0) {
			emissionProcess = EmissionProcess.findByID(emissionProcessID);
			if(emissionProcessID <= 0) {
				// The code gets here when there is an emissionProcessID
				// (non-0) passed in but that emission process can't
				// be found in the database.
				return false;
			}
		}
		return doesHavePollutantAndProcess(pollutant,emissionProcess);
	}

	/**
	 * Checks the pollutants and processes associated with the ExecutionRunSpec to see if the
	 * requested combination is selected.  Wildcard-type functionality is obtained by passing
	 * null for one of the two required names.
	 * @param pollutant Pollutant to search for or null to match any pollutant in the
	 * selected process
	 * @param emissionProcess Emission process to search for null to match any process for the
	 * selected pollutant
	 * @return true if the requested combination or wildcard match is found, false if both
	 * parameters are null
	**/
	public boolean doesHavePollutantAndProcess(Pollutant pollutant,
			EmissionProcess emissionProcess) {
		int neededMatches = 0;
		if(pollutant != null) {
			neededMatches++;
		}
		if(emissionProcess != null) {
			neededMatches++;
		}
		
		/** NR_IMP: differentiate based on model selection **/
		Models.ModelCombination mc = this.targetRunSpec.getModelCombination();
		if(pollutant == null && emissionProcess != null) {
			if(targetProcesses.contains(emissionProcess)) {
				/** NR_IMP: differentiate based on model selection **/
				switch ( mc) {
					case M2:
						for (Iterator i = pollutantProcessAssociations.iterator(); i.hasNext();) {
							PollutantProcessAssociation a = (PollutantProcessAssociation)i.next();
							if(a.isAffectedByNonroad && emissionProcess != null && 
									emissionProcess.compareTo(a.emissionProcess) == 0) {
								return true;
							}
						}
						return false;
					case M1:
					default:
						return true;
				}
			}
		}
		if(neededMatches > 0) {
			for (Iterator i = pollutantProcessAssociations.iterator(); i.hasNext();) {
				int matches = 0;
				PollutantProcessAssociation a = (PollutantProcessAssociation)i.next();
				if(pollutant != null && 
						pollutant.compareTo(a.pollutant) == 0) {
					matches++;
				}
				if(emissionProcess != null && 
						emissionProcess.compareTo(a.emissionProcess) == 0) {
					switch ( mc) {
						case M2:
							if (a.isAffectedByNonroad) {
								matches++;
							}
							break;
						case M1:
						default:
							if (a.isAffectedByOnroad) {
								matches++;
							}
							break;
					}
				}
				if(matches == neededMatches) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Include a statement as part of the retrofit processing
	 * @param sql statement to execute before aggregation on the worker
	**/
	public void addRetrofitSQL(String sql) {
		if(retrofitSQLs == null) {
			retrofitSQLs = new Vector<String>();
		}
		/*
		boolean exists = false;
		for(Iterator i=retrofitSQLs.iterator();i.hasNext();) {
			String s = (String)i.next();
			if(s.equalsIgnoreCase(sql)) {
				exists = true;
				break;
			}
		}
		if(exists) {
			S ystem.out.println("OLD Retrofit SQL: " + sql);
		} else {
			S ystem.out.println("NEW Retrofit SQL: " + sql);
		}
		*/
		retrofitSQLs.add(sql);
	}

	/**
	 * Obtains the user's database input selection.
	 * @return A LinkList object.
	**/
	public LinkedList<DatabaseSelection> getDatabaseSelectionInputSets() {
		return targetRunSpec.databaseSelectionInputSets;
	}

	/**
	 * Obtains the user's output emission breakdown selection.
	 * @return An OutputEmissionBreakdownSelection object.
	**/
	public OutputEmissionsBreakdownSelection getOutputEmissionsBreakdownSelection() {
		return targetRunSpec.outputEmissionsBreakdownSelection;
	}

	/**
	 * Should MOVES output VMT Data?
	 * @return A bool value.
	**/
	public boolean getOutputVMTData() {
		return targetRunSpec.outputVMTData;
	}

	/**
	 * Gets the user's output time step
	 * @return An OutputTimeStep object.
	**/
	public OutputTimeStep getOutputTimeStep() {
		return targetRunSpec.outputTimeStep;
	}

	/**
	 * Gets the user's selected time span.
	 * @return A TimeSpan object.
	**/
	public TimeSpan getTimeSpan() {
		return targetRunSpec.timeSpan;
	}

	/**
	 * Gets the user's RoadTypes, or RoadTypes required by the specific run (eg. Off-network Idle)
	 * @return The TreeSet of all RoadTypes to be used in the simulation.
	**/
	public TreeSet<RoadType> getRoadTypes() {
		
		executionRoadTypes = new TreeSet<RoadType>();
		
		//For off-network idle, all road types are required to calculate correctly.
		//Check to see if off-network roadtype and running process is selected in the runspec.
		boolean containsOffNetwork = false;
		boolean containsRunning = false;
		boolean isProjectScale = (targetRunSpec.domain == ModelDomain.PROJECT);
		boolean needsAllRoadTypes = false;
		
		//Check if off-network is present in the runspec
		for (Iterator<RoadType> i = targetRunSpec.roadTypes.iterator();i.hasNext();){
			RoadType r = i.next();
			if (r.roadTypeID == 1){
				containsOffNetwork = true;
				break;
			}
		}
		//Check if running process is present in the runspec
		for (Iterator<EmissionProcess> i = targetProcesses.iterator();i.hasNext();){
			EmissionProcess e = i.next();
			if (e.databaseKey == 1){
				containsRunning = true;
				break;
			}
		}
		//If both off-network and running process are in the runspec, all roadtypes are needed
		// (except Project scale doesn't calculate ONI, so they are not needed for Project Scale)
		needsAllRoadTypes = (containsOffNetwork && containsRunning && !isProjectScale);
		
		//If off-network idle is needed, return all roadtypes
		if (needsAllRoadTypes){
			try{
				TreeSet<RoadType> allRoadTypes = getAllRoadTypes();
				if (targetRunSpec.roadTypes.size() < allRoadTypes.size()){
					Logger.log(LogMessageCategory.INFO,"WARNING: Off-Network Idle requires all roadtypes. Selecting all roadtypes regardless of runspec.");
				}
				executionRoadTypes.addAll(allRoadTypes);
				return executionRoadTypes;
			}
			catch (InterruptedException e){
				Logger.logError(e, "ExecutionRunSpec[getRoadTypes]: Thread Interrupted. Not all roadtypes will be selected.");
			}
		//ramps(? Ramps have been removed from the model, this may be deprecated)
		} else if(extraRoadTypes != null && extraRoadTypes.size() > 0) {
			if(executionRoadTypes == null) {
				executionRoadTypes.addAll(extraRoadTypes);
				executionRoadTypes.addAll(targetRunSpec.roadTypes);
			}
			return executionRoadTypes;
		}
		//If off-network idle and ramps are not needed, just return the roadtypes from the runspec
		return targetRunSpec.roadTypes;
	}
	
	/**
	 * Gets all roadtypes from the default Database for onroad only.
	 * @return The TreeSet of all RoadTypes for onroad from the default database.
	 * @throws InterruptedException if thread is interrupted. 
	**/
	public TreeSet<RoadType> getAllRoadTypes() throws InterruptedException {
		
		//Create a blank container for all roadtypes that will be selected from the default database
		TreeSet<RoadType> defaultRoadTypeListOutput = new TreeSet<RoadType>();
		
		//Create a new SQL connection to the default database, log if connection fails
		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		if (null == db) {
			Logger.log(LogMessageCategory.ERROR,"ExecutionRunSpec[getAllRoadTypes]: Unable to connect to database.");
		}
		
		//Create database query string for all onroad roadtype selections
		String sql = "SELECT roadTypeID, roadDesc, isAffectedByOnroad, isAffectedByNonroad"
				+ " FROM roadtype"
				+ " WHERE shouldDisplay = 1"
				+ " AND isAffectedByOnroad = TRUE"
				+ " ORDER BY roadDesc";
				
		//Query database using connection db with the query string
		try {
			PreparedStatement statement = db.prepareStatement(sql);
			ResultSet results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				while (results.next()) {
					int roadTypeID = results.getInt(1);
					boolean isAffectedByOnroad = results.getBoolean(3);
					boolean isAffectedByNonroad = results.getBoolean(4);
					//Create temporary RoadType object for query results
					RoadType r = null;
					//Add query results to the RoadType object only for onroad RoadTypes
					if (isAffectedByOnroad && isAffectedByNonroad) {
						r = new RoadType(roadTypeID,
								results.getString(2),
								Models.ModelCombination.M12);
					} else if (isAffectedByOnroad) {
						r = new RoadType(roadTypeID,
								results.getString(2),
								Models.ModelCombination.M1);
					}
					//Add the temporary RoadType object to the container for all onroad RoadTypes
					if(r != null) {
						defaultRoadTypeListOutput.add(r);
					}
				}
				results.close();
				}
			statement.close();
			}
		catch (SQLException e) {
			Logger.logError(e, "ExecutionRunSpec[getAllRoadTypes]: Unable to load a list of road types.");
		}
	    finally {
			//Close the database connection
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
			db = null;
		}
		
		return defaultRoadTypeListOutput;
	}

	/**
	 * Get the user's selection for geographic detail.
	 * @return A GeographicOutputDetailLevel object.
	**/
	public GeographicOutputDetailLevel getGeographicOutputDetailLevel() {
		return targetRunSpec.geographicOutputDetail;
	}

	/**
	 * Get the user's selected models.
	 * @return A Models object
	**/
	public Models getModels() {
		return targetRunSpec.models;
	}

	/**
	 * Get the user's selection for the model scale.
	 * @return A ModelScale object
	**/
	public ModelScale getModelScale() {
		return targetRunSpec.scale;
	}

	/**
	 * Get the user's selection for the model domain.
	 * @return A ModelDomain object
	**/
	public ModelDomain getModelDomain() {
		return targetRunSpec.domain;
	}

	/**
	 * Get the output factors.
	 * @return An OutputFactors object
	**/
	public OutputFactors getOutputFactors() {
		return targetRunSpec.outputFactors;
	}

	/**
	 * Get the user's onroad vehicle selections.
	 * @return A TreeSet Object
	**/
	public TreeSet<OnRoadVehicleSelection> getOnRoadVehicleSelections() {
		return targetRunSpec.onRoadVehicleSelections;
	}

	/**
	 * Get the user's nonroad vehicle selections.
	 * @return A TreeSet Object
	**/
	public TreeSet<OffRoadVehicleSelection> getOffRoadVehicleSelections() {
		return targetRunSpec.offRoadVehicleSelections;
	}

	/**
	 * Get the user's geographic selections.
	 * @return A TreeSet Object
	**/
	public TreeSet<GeographicSelection> getGeographicSelections() {
		return targetRunSpec.geographicSelections;
	}

	/**
	 * Get the internalControlStrategies TreeMap from the current RunSpec
	 * @return a TreeMap of LinkedLists of InternalControlStrategies
	**/
	public TreeMap<String,LinkedList<InternalControlStrategy> > getInternalControlStrategies() {
		return targetRunSpec.internalControlStrategies;
	}

	/** Populate the states, counties, zones, and links members based upon executionLocations **/
	void extractLocationDetailsFromExecutionLocations() {
		states.clear();
		counties.clear();
		zones.clear();
		links.clear();
		for(Iterator<ExecutionLocation> i=executionLocations.iterator();i.hasNext();) {
			ExecutionLocation location = (ExecutionLocation)i.next();
			states.add(Integer.valueOf(location.stateRecordID));
			counties.add(Integer.valueOf(location.countyRecordID));
			zones.add(Integer.valueOf(location.zoneRecordID));
			links.add(Integer.valueOf(location.linkRecordID));
		}
	}

	void buildVehicleSelections(Models.ModelCombination mc) throws SQLException {
		/** NR_IMP: **/
		switch ( mc) {
		case M1:
			for(Iterator<OnRoadVehicleSelection>
			i = targetRunSpec.onRoadVehicleSelections.iterator(); i.hasNext();) {
				OnRoadVehicleSelection onRoadVehicleSelection = (OnRoadVehicleSelection) i.next();
				sourceTypes.add(Integer.valueOf(onRoadVehicleSelection.sourceTypeID));
				fuelTypes.add(Integer.valueOf(onRoadVehicleSelection.fuelTypeID));

				//if(onRoadVehicleSelection.sourceTypeID == 11 && !didWarnAboutMotorcycles) {
				//	didWarnAboutMotorcycles = true;
				//	/**
				//	 * @explain The draft version of MOVES 2009 does not include all pollutants for motorcycles.
				//	**/
				//	Logger.log(LogMessageCategory.WARNING,
				//			"This version of MOVES does not include criteria pollutant and air toxics emission factors for motorcycles.");
				//}
			}
			break;
		case M2:
			for(Iterator<OffRoadVehicleSelection>
			i = targetRunSpec.offRoadVehicleSelections.iterator(); i.hasNext();) {
				OffRoadVehicleSelection offRoadVehicleSelection = (OffRoadVehicleSelection) i.next();
				sectors.add(Integer.valueOf(offRoadVehicleSelection.sectorID));
				fuelTypes.add(Integer.valueOf(offRoadVehicleSelection.fuelTypeID));
			}			
			break;
		case M12:
			/** NR_IMP: later **/
			// sourceTypes and fuelTypes need be differentiated for nonroad and onroad
			// only do one of the models at a time only for now
			break;
		default: // as M1
			for(Iterator<OnRoadVehicleSelection>
			i = targetRunSpec.onRoadVehicleSelections.iterator(); i.hasNext();) {
				OnRoadVehicleSelection onRoadVehicleSelection = (OnRoadVehicleSelection) i.next();
				sourceTypes.add(Integer.valueOf(onRoadVehicleSelection.sourceTypeID));
				fuelTypes.add(Integer.valueOf(onRoadVehicleSelection.fuelTypeID));

				//if(onRoadVehicleSelection.sourceTypeID == 11 && !didWarnAboutMotorcycles) {
				//	didWarnAboutMotorcycles = true;
				//	/**
				//	 * @explain The draft version of MOVES 2009 does not include all pollutants for motorcycles.
				//	**/
				//	Logger.log(LogMessageCategory.WARNING,
				//			"This version of MOVES does not include criteria pollutant and air toxics emission factors for motorcycles.");
				//}
			}
			break;
		}
		
	}

	/**
	 * Builds filter tables used in queries to limit the items used in calculations.  The tables
	 * built within do not reference location (state, county, zone, link) in any way.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	void buildNonLocationFilterTables() throws InterruptedException {
		String sql = "";
		PreparedStatement statement = null;
		Connection defaultDB =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		Connection executionDB =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
		try {
			Models.ModelCombination mc = targetRunSpec.getModelCombination();
			
			buildVehicleSelections(mc); 
			fillSourceTypes(executionDB,sourceTypes);
			fillSectors(executionDB,sectors);

			// Build the fueltype selections
			sql = "TRUNCATE RunSpecSourceFuelType";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecSourceFuelType (sourceTypeID, fuelTypeID) VALUES (?,?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<OnRoadVehicleSelection>
					i = targetRunSpec.onRoadVehicleSelections.iterator(); i.hasNext();) {
				OnRoadVehicleSelection onRoadVehicleSelection = i.next();
				statement.setInt(1,onRoadVehicleSelection.sourceTypeID);
				statement.setInt(2,onRoadVehicleSelection.fuelTypeID);
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();

			sql = "TRUNCATE RunSpecSectorFuelType";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecSectorFuelType (sectorID, fuelTypeID) VALUES (?,?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<OffRoadVehicleSelection>
					i = targetRunSpec.offRoadVehicleSelections.iterator(); i.hasNext();) {
				OffRoadVehicleSelection offRoadVehicleSelection = i.next();
				statement.setInt(1,offRoadVehicleSelection.sectorID);
				statement.setInt(2,offRoadVehicleSelection.fuelTypeID);
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();

			// Build the roadtype selections
			sql = "TRUNCATE RunSpecRoadType";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT IGNORE INTO RunSpecRoadType (roadTypeID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			//for(Iterator<RoadType> i = targetRunSpec.roadTypes.iterator(); i.hasNext();) {
			for(Iterator<RoadType> i = getRoadTypes().iterator(); i.hasNext();) {
				/** NR_IMP:  **/
				// if later we want to run both of the models at the same time
				// we need to add which are for onroad/nonroad
				RoadType roadType = (RoadType) i.next();
				statement.setInt(1,roadType.roadTypeID);
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();

			// Build the calendar year selections
			fillYearsAndModelYears(executionDB,years);

			// Build the month selection
			sql = "TRUNCATE RunSpecMonth";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecMonth (monthID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=months.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the month group selection
			sql = "TRUNCATE RunSpecMonthGroup";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecMonthGroup (monthGroupID) "
					+ "SELECT DISTINCT monthGroupID FROM MonthOfAnyYear, RunSpecMonth "
					+ "WHERE MonthOfAnyYear.monthID = RunSpecMonth.monthID";
			SQLRunner.executeSQL(executionDB,sql);
			// Build the days selections
			sql = "TRUNCATE RunSpecDay";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecDay (dayID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=days.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the hours selections
			sql = "TRUNCATE RunSpecHour";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecHour (hourID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=hours.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the hour day selections
			sql = "TRUNCATE RunSpecHourDay";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecHourDay (HourDayID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=hourdays.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();

			// Build the processes selections
			sql = "TRUNCATE RunSpecProcess";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecProcess (processID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<EmissionProcess> i = targetProcesses.iterator(); i.hasNext();) {
				statement.setInt(1,((EmissionProcess)(i.next())).databaseKey);
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the pollutants selections
			sql = "TRUNCATE RunSpecPollutant";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecPollutant(pollutantID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Pollutant> i = targetPollutants.iterator(); i.hasNext();) {
				statement.setInt(1,((Pollutant)i.next()).databaseKey);
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the pollutants processes selections
			sql = "TRUNCATE RunSpecPollutantProcess";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecPollutantProcess(polProcessID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> i = targetPollutantProcesses.iterator(); i.hasNext();) {
				statement.setInt(1,((Integer)(i.next())).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();

			// Build the states selections
			sql = "TRUNCATE RunSpecState";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecState (stateID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=states.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the counties selections
			sql = "TRUNCATE RunSpecCounty";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecCounty (countyID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=counties.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the zones selections
			sql = "TRUNCATE RunSpecZone";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecZone (zoneID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=zones.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the links selections
			sql = "TRUNCATE RunSpecLink";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecLink (linkID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=links.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();

			// Build RunSpecChainedTo
			/** NR_IMP: need to get this query for onroad based on new PollutantProcessAssoc **/
			sql = "TRUNCATE RunSpecChainedTo";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "insert into RunSpecChainedTo ("
					+ " outputPolProcessID, outputPollutantID, outputProcessID,"
					+ " inputPolProcessID, inputPollutantID, inputProcessID)"
					+ " select ppa.polProcessID, ppa.pollutantID, ppa.processID,"
					+ " ppaIn.polProcessID, ppaIn.pollutantID, ppaIn.processID"
					+ " from PollutantProcessAssoc ppa"
					+ " inner join PollutantProcessAssoc ppaIn "
					+ " on (ppaIn.polProcessID=ppa.chainedto1 or ppaIn.polProcessID=ppa.chainedto2)";
			SQLRunner.executeSQL(executionDB,sql);

			// Build RunSpecNonRoadChainedTo
			/** NR_IMP: need to get this query for nonroad based on new PollutantProcessAssoc **/
			sql = "TRUNCATE RunSpecNonRoadChainedTo";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "insert into RunSpecNonRoadChainedTo ("
					+ " outputPolProcessID, outputPollutantID, outputProcessID,"
					+ " inputPolProcessID, inputPollutantID, inputProcessID)"
					+ " select ppa.polProcessID, ppa.pollutantID, ppa.processID,"
					+ " ppaIn.polProcessID, ppaIn.pollutantID, ppaIn.processID"
					+ " from PollutantProcessAssoc ppa"
					+ " inner join PollutantProcessAssoc ppaIn "
					+ " on (ppaIn.polProcessID=ppa.chainedto1 or ppaIn.polProcessID=ppa.chainedto2)";
			SQLRunner.executeSQL(executionDB,sql);
		} catch(SQLException e) {
			final String eol = System.getProperty("line.separator");
			Logger.logSqlError(e,"An SQL exception occurred while building the runspec filter "+
					"tables.", sql);
		} finally {
			if(defaultDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT, defaultDB);
				defaultDB = null;
			}
			if(executionDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, executionDB);
				executionDB = null;
			}
		}
	}

	/**
	 * Builds location-centric filter tables used in queries to limit the items used
	 * in calculations.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	void buildLocationFilterTables() throws InterruptedException {
		String sql = "";
		PreparedStatement statement = null;
		Connection defaultDB =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		Connection executionDB =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
		try {
			// Build the states selections
			sql = "TRUNCATE RunSpecState";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecState (stateID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=states.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the counties selections
			sql = "TRUNCATE RunSpecCounty";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecCounty (countyID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=counties.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the zones selections
			sql = "TRUNCATE RunSpecZone";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecZone (zoneID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=zones.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
			// Build the links selections
			sql = "TRUNCATE RunSpecLink";
			SQLRunner.executeSQL(executionDB,sql);
			sql = "INSERT INTO RunSpecLink (linkID) VALUES (?)";
			statement = executionDB.prepareStatement(sql);
			for(Iterator<Integer> iter=links.iterator();iter.hasNext();) {
				statement.setInt(1,((Integer)iter.next()).intValue());
				SQLRunner.executeSQL(statement,sql);
			}
			statement.close();
		} catch(SQLException e) {
			final String eol = System.getProperty("line.separator");
			Logger.logSqlError(e,"An SQL exception occurred while building the runspec filter "+
					"tables.", sql);
		} finally {
			if(defaultDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT, defaultDB);
				defaultDB = null;
			}
			if(executionDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,
						executionDB);
				executionDB = null;
			}
		}
	}

	/**
	 * Return the number of iterations to be performed.
	 * @return the number of iterations to be performed
	**/
	int getHowManyIterationsWillBePerformed() {
		if(targetRunSpec.outputEmissionsBreakdownSelection.estimateUncertainty) {
			return targetRunSpec.outputEmissionsBreakdownSelection.numberOfIterations;
		}
		return 1;
	}

	/**
	 * Return whether uncertainty is to be estimated.
	 * @return true if uncertainty is to be estimated.
	**/
	boolean estimateUncertainty() {
		return targetRunSpec.outputEmissionsBreakdownSelection.estimateUncertainty;
	}

	/**
	 * Determine if a class is allowed to execute.  The class as well as all of its
	 * superclasses are checked against classesNotToExecute.
	 * @param c Class of an object to be checked
	 * @return true if an object of class c is permitted to be instantiated and used
	 * within the simulation.
	**/
	public static boolean shouldExecute(Class c) {
		while(c != null) {
			if(theExecutionRunSpec.targetRunSpec.classesNotToExecute.contains(c.getName())) {
				return false;
			}
			c = c.getSuperclass();
		}
		return true;
	}

	/**
	 * Determine if an object should preserve its data during the simulation.  The object's
	 * class and all of its superclasses are checked against classesToSaveData.
	 * @param o an object to be checked
	 * @return true if the object should save its data.
	**/
	public static boolean shouldSaveData(Object o) {
		return shouldSaveData(o.getClass());
	}

	/**
	 * Determine if a class of objects should preserve data during the simulation.  The
	 * class and all of its superclasses are checked against classesToSaveData.
	 * @param c a class to be checked
	 * @return true if the object should save its data.
	**/
	public static boolean shouldSaveData(Class c) {
		while(c != null) {
			String name = c.getName();
			//System.out.println("Checking save data: " + name);
			if(shouldSaveDataByName(name)) {
				return true;
			}
			c = c.getSuperclass();
		}
		//System.out.println("Not saving data.");
		return false;
	}

	/**
	 * Determine if a class of objects should preserve data during the simulation.
	 * @param className the fully qualified class name to be checked
	 * @return true if the object should save its data.
	**/
	public static boolean shouldSaveDataByName(String className) {
		if(theExecutionRunSpec.shouldKeepWorkerDatabases
				&& className.equalsIgnoreCase("gov.epa.otaq.moves.master.framework.EmissionCalculator")) {
			return true;
		}
		if(theExecutionRunSpec.targetRunSpec.classesToSaveData.contains(className)) {
			return true;
		}
		return false;
	}

	/**
	 * Check the RunSpec's shouldCopySavedGeneratorData flag.
	 * @return the RunSpec's shouldCopySavedGeneratorData flag
	**/
	public static boolean shouldCopySavedGeneratorData() {
		return theExecutionRunSpec.targetRunSpec.shouldCopySavedGeneratorData;
	}

	/**
	 * Retrieve the RunSpec's generatorDatabase
	 * @return the RunSpec's generatorDatabase
	**/
	public static DatabaseSelection getSavedGeneratorDatabase() {
		return theExecutionRunSpec.targetRunSpec.generatorDatabase;
	}

	/**
	 * Retrieve the RunSpec's classesToSaveData collection
	 * @return the RunSpec's classesToSaveData collection
	**/
	public static TreeSetIgnoreCase getClassesToSaveData() {
		return theExecutionRunSpec.targetRunSpec.classesToSaveData;
	}

	/**
	 * Modify the set of links and their average speeds in order to process Mesoscale Lookup
	 * simulations.  The current contents of the Link and LinkAverageSpeed tables are deleted
	 * and recreated.
	 * @param executionDB Execution database to be used
	**/
	private void createMesoscaleLookupLinks(Connection executionDB) {
		String sql = "";
		String[] statements = {
			"delete from Link",
			"insert into Link (linkID,countyID,zoneID,roadTypeID,linkLength,linkVolume)"
					+ " select ((zoneID*10+roadTypeID)*100) as linkID,"
					+ " countyID, zoneID, roadTypeID,"
					+ " null as linkLength,null as linkVolume"
					+ " from Zone,"
					+ " RunSpecRoadType"
					+ " where roadTypeID=1",
			"insert into Link (linkID,countyID,zoneID,roadTypeID,linkLength,linkVolume)"
					+ " select ((zoneID*10+roadTypeID)*100+avgSpeedBinID) as linkID,"
					+ " countyID, zoneID, roadTypeID,"
					+ " null as linkLength,null as linkVolume"
					+ " from Zone,"
					+ " RunSpecRoadType,"
					+ " AvgSpeedBin"
					+ " where roadTypeID<>1",
			"delete from LinkAverageSpeed",
			"insert into LinkAverageSpeed (linkID,averageSpeed)"
					+ " select ((zoneID*10+roadTypeID)*100) as linkID,0 as averageSpeed"
					+ " from Zone,"
					+ " RunSpecRoadType"
					+ " where roadTypeID=1",
			"insert into LinkAverageSpeed (linkID,averageSpeed)"
					+ " select ((zoneID*10+roadTypeID)*100+avgSpeedBinID) as linkID,avgBinSpeed"
					+ " from Zone,"
					+ " RunSpecRoadType,"
					+ " AvgSpeedBin"
					+ " where roadTypeID<>1"
		};
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(executionDB,sql);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,
					"An SQL exception occurred while building mesoscale lookup links.", sql);
		}
	}

	/**
	 * Add useful indexes to the execution database.
	 * @param executionDB Execution database to be used
	**/
	private void addIndexes(Connection executionDB) {
		String sql = "";
		String[] statements = {
			"alter table link add key (zoneID, roadTypeID, linkID)",
			"alter table link add key (roadTypeID, zoneID, linkID)",
			"analyze table link",
			"alter table hourDay add key (hourID, dayID, hourDayID)",
			"alter table hourDay add key (dayID, hourID, hourDayID)",
			"analyze table hourDay",
			"alter table ColdSoakInitialHourFraction add key (zoneID, monthID)",
			"alter table AverageTankTemperature add key (zoneID, monthID)",
			"alter table SoakActivityFraction add key (monthID, zoneID, opModeID, sourceTypeID, hourDayID)",
			"alter table sourceHours add key (linkID, yearID, monthID, hourDayID, sourceTypeID, ageID)"
		};
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(executionDB,sql);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,
					"An SQL exception occurred while building execution indexes.", sql);
		}
	}

	/**
	 * Execute a script to generate utility tables in the execution database.
	 * @param executionDB Execution database to be used
	**/
	private void runAdditionalSetupScript(Connection executionDB) {
		try {
			DatabaseUtilities.executeScript(executionDB,new File("database/FillExecution.sql"));
		} catch(Exception e) {
			Logger.logError(e,
					"An exception occurred while running FillExecution.sql.");
		}
		runUpdateScript(executionDB);
	}

	/**
	 * Execute a script to update utility tables in the execution database.
	 * This should be done after AVFT as changed regclass fractions.
	 * @param executionDB Execution database to be used
	**/
	public void runUpdateScript(Connection executionDB) {
		try {
			DatabaseUtilities.executeScript(executionDB,new File("database/UpdateExecution.sql"),null,true);
		} catch(Exception e) {
			Logger.logError(e,
					"An exception occurred while running UpdateExecution.sql.");
		}
	}

	/**
	 * Examine the execution database for data integrity.
	 * @return true if all is well, false otherwise.  Upon failure, zero or more messages may have
	 * been logged to the current run.
	**/
	private boolean checkDataIntegrity() {
		int eventRecordID = MOVESEngine.logEventStart("checkDataIntegrity");
		Connection executionDB = null;
		boolean result = true;
		try {
			executionDB =
					DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			if(!checkMissingEmissionRates(executionDB)) {
				result = false;
			}
		} catch(Exception e) {
			/** @explain A database error occurred while checking for emission rate consistency.  **/
			Logger.logError(e,"Unable to check data integrity");
			result = false;
		} finally {
			if(executionDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,
						executionDB);
			}
			executionDB = null;
			MOVESEngine.logEventStop(eventRecordID);
		}
		return result;
	}

	/**
	 * Scan for missing emission rates and emission rates by age
	 * @param executionDB database to be scanned for missing rates
	 * @return true if all is well, false otherwise.  Upon failure, zero or more messages may have
	 * been logged to the current run.
	**/
	private boolean checkMissingEmissionRates(Connection executionDB) {
		String[] statementsAndMessages = {
			// Find missing rates
			"select sourceBin.sourceBinID"
			+ " from fuelType"
			+ " inner join sourceBin on sourceBin.fuelTypeID=fuelType.fuelTypeID"
			+ " left outer join emissionRate e on e.sourceBinID=sourceBin.sourceBinID"
			+ " where e.sourceBinID is null"
			+ " limit 10",
			/**
			 * @issue Warning: Missing emission rates detected
			 * @explain A scan of the EmissionRate and SourceBin tables has revealed
			 * source bins without emission rates.
			**/
			"Missing emission rates detected",

			"select sourceBin.sourceBinID"
			+ " from fuelType"
			+ " inner join sourceBin on sourceBin.fuelTypeID=fuelType.fuelTypeID"
			+ " left outer join emissionRateByAge e on e.sourceBinID=sourceBin.sourceBinID"
			+ " where e.sourceBinID is null"
			+ " limit 10",
			/**
			 * @issue Warning: Missing emission rates by age detected
			 * @explain A scan of the EmissionRateByAge and SourceBin tables has revealed
			 * source bins without emission rates.
			**/
			"Missing emission rates by age detected",

			// Find zero rates
			"select sourceBin.sourceBinID"
			+ " from fuelType"
			+ " inner join sourceBin on sourceBin.fuelTypeID=fuelType.fuelTypeID"
			+ " inner join emissionRate e on e.sourceBinID=sourceBin.sourceBinID"
			+ " where fuelTypeDesc<>'Electricity'"
			+ " and e.meanBaseRate <= 0"
			+ " limit 10",
			/**
			 * @issue Warning: Some emission rates are zero for non-electric vehicles
			 * @explain A scan of the EmissionRate table has revealed suspicious emission
			 * rates.
			**/
			"Some emission rates are zero for non-electric vehicles",

			"select sourceBin.sourceBinID"
			+ " from fuelType"
			+ " inner join sourceBin on sourceBin.fuelTypeID=fuelType.fuelTypeID"
			+ " inner join emissionRateByAge e on e.sourceBinID=sourceBin.sourceBinID"
			+ " where fuelTypeDesc<>'Electricity'"
			+ " and e.meanBaseRate <= 0"
			+ " limit 10",
			/**
			 * @issue Warning: Some emission rates by age are zero for non-electric vehicles
			 * @explain A scan of the EmissionRateByAge table has revealed suspicious emission
			 * rates.
			**/
			"Some emission rates by age are zero for non-electric vehicles"
		};
		boolean result = true;
		for(int i=0;i<statementsAndMessages.length;i+=2) {
			String sql = statementsAndMessages[i+0];
			String text = statementsAndMessages[i+1];
			try {
				String scalar = SQLRunner.executeScalarString(executionDB,sql);
				if(scalar != null && scalar.length() > 0) {
					/** @nonissue **/
					Logger.log(LogMessageCategory.WARNING,"Warning: " + text);
					result = false;
				}
			} catch(Exception e) {
				Logger.logError(e,"Unable to check missing emission rates: " + sql);
				return false;
			}
		}
		return result;
	}

	/**
	 * Check if the final aggregation step should be performed.  Consider the need to perform
	 * the aggregation based on calculator information about how pollutants are calculated,
	 * on user dimension choices such as removing emission process from the output, and the user's
	 * preference for allowing such an aggregation.
	 * @return true if final aggregation should be performed on the emission and activity output.
	**/
	public boolean shouldDoFinalAggregation() {
		// If the user flagged not to perform aggregation, then do not perform it even if needed.
		if(targetRunSpec.doNotPerformFinalAggregation) {
			Logger.log(LogMessageCategory.INFO,"Final aggregation has been disabled by the user.");
			return false;
		}
		if(TOGSpeciationCalculator.needsFinalAggregation()) {
			Logger.log(LogMessageCategory.INFO,"Final aggregation is being done because TOG speciation was used.");
			return true;
		}
		if(pollutantProcessesNeedingAggregation.size() > 0) {
			// If any of these are in the simulation, aggregation is needed.
			for(Iterator<PollutantProcessAssociation>
						i=pollutantProcessesNeedingAggregation.iterator();i.hasNext();) {
				PollutantProcessAssociation ip = (PollutantProcessAssociation)i.next();
				if(targetPollutantProcesses.contains(Integer.valueOf(ip.getDatabaseKey()))) {
					Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
							+ " because the runspec includes " + ip);
					return true;
				}
			}
		}
		if(pollutantsNeedingAggregation.size() > 0) {
			// If any of these are in the simulation, aggregation is needed.
			for(Iterator<Pollutant> i=pollutantsNeedingAggregation.iterator();i.hasNext();) {
				Pollutant ip = (Pollutant)i.next();
				if(targetPollutants.contains(ip)) {
					Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
							+ " because the runspec includes " + ip);
					return true;
				}
			}
		}

		// If the preaggregation time does not match the output time, then aggregation
		// is needed.
		if(targetRunSpec.timeSpan.aggregateBy != targetRunSpec.outputTimeStep) {
			Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
					+ " because the runspec preaggregates by "
					+ targetRunSpec.timeSpan.aggregateBy
					+ " but the output is by " + targetRunSpec.outputTimeStep);
			return true;
		}

		/* Model year is aggregated on the worker, so master side aggregation is never needed
		// No model year output
		if(!targetRunSpec.outputEmissionsBreakdownSelection.modelYear) {
			return true;
		}
		*/
		/* Fuel type is aggregated on the worker, so master side aggregation is never needed
		// No fuel type output (as long as more than one fuel type is to be output)
		if(fuelTypes.size() > 1) {
			if(!targetRunSpec.outputEmissionsBreakdownSelection.fuelType) {
				return true;
			}
		}
		*/
		// No emission process output (as long as more than one emission process is to be output)
		if(targetProcesses.size() > 1) {
			if(!targetRunSpec.outputEmissionsBreakdownSelection.emissionProcess) {
				Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
						+ " because the runspec includes " + targetProcesses.size()
						+ " processes but emission process is not desired in the output.");
				return true;
			}
		}

		// If SCC is selected, Road Type and Source Use Type won't matter for aggregation.
		// But, these are done on the workers anyway, so there's nothing special to be done
		// for SCC.

		// No road type output (as long as more than one road type is to be output)
		if(targetRunSpec.roadTypes.size() > 1) {
			if(!targetRunSpec.outputEmissionsBreakdownSelection.roadType) {
				Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
						+ " because the runspec includes " + targetRunSpec.roadTypes.size()
						+ " road types but road type is not desired in the output.");
				return true;
			}
		}

		/* Source type is aggregated on the worker, so master side aggregation is never needed
		// No source type output (as long as more than one source type is to be output)
		if(sourceTypes.size() > 1) {
			if(!targetRunSpec.outputEmissionsBreakdownSelection.sourceUseType) {
				return true;
			}
		}
		*/

		// Summarize the arity of the states, counties, and zones.
		boolean hasMultipleStates = false;
		boolean anyStateHasMultipleCounties = false;
		boolean anyCountyHasMultipleZones = false;
		ExecutionLocation priorLocation = null;
		for(Iterator<ExecutionLocation> i=executionLocations.iterator();i.hasNext();) {
			ExecutionLocation el = (ExecutionLocation)i.next();
			if(priorLocation == null) {
				priorLocation = el;
				continue;
			}
			// Locations are sorted by State, County, Zone, Link, and Road Type.
			if(el.stateRecordID != priorLocation.stateRecordID) {
				hasMultipleStates = true;
				continue;
			}
			if(el.countyRecordID != priorLocation.countyRecordID) {
				anyStateHasMultipleCounties = true;
				continue;
			}
			if(el.zoneRecordID != priorLocation.zoneRecordID) {
				anyCountyHasMultipleZones = true;
				continue;
			}
		}

		/*
		If the output aggregation is by nation, only aggregate if there is more than one
		state, or if the state has more than one county, or if the county has more than
		one zone.
		*/
		if(targetRunSpec.geographicOutputDetail == GeographicOutputDetailLevel.NATION) {
			if(hasMultipleStates) {
				Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
						+ " because the runspec summarizes by Nation"
						+ " and there are multiple states in the output.");
				return true;
			}
			if(anyStateHasMultipleCounties) {
				Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
						+ " because the runspec summarizes by Nation"
						+ " and there are multiple counties in the output.");
				return true;
			}
			if(anyCountyHasMultipleZones) {
				Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
						+ " because the runspec summarizes by Nation"
						+ " and there are multiple zones in the output.");
				return true;
			}
		}

		/*
		If the output aggregation is by state, only aggregate if there is more than one
		county in any state, or if any county has more than one zone.
		*/
		if(targetRunSpec.geographicOutputDetail == GeographicOutputDetailLevel.STATE) {
			if(anyStateHasMultipleCounties) {
				Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
						+ " because the runspec summarizes by State"
						+ " and there are multiple counties in at least one of the states.");
				return true;
			}
			if(anyCountyHasMultipleZones) {
				Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
						+ " because the runspec summarizes by State"
						+ " and there are multiple zones in at least one county.");
				return true;
			}
		}

		/*
		If the output aggregation is by county, only aggregate if there is more than one
		zone in any county.
		*/
		if(targetRunSpec.geographicOutputDetail == GeographicOutputDetailLevel.COUNTY) {
			if(anyCountyHasMultipleZones) {
				Logger.log(LogMessageCategory.INFO,"Final aggregation is being done"
						+ " because the runspec summarizes by County"
						+ " and there are multiple zones in at least one county.");
				return true;
			}
		}

		// If the output aggregation is by zone or link, there is no required aggregation.

		// Nothing has been found that needs aggregation, but not all calculators have been enforcing
		// the registration of items that would require aggregation.  Thus, aggregation must occur
		// by default just in case it is in fact needed.
		Logger.log(LogMessageCategory.INFO,"Final aggregation is being done by default.");
		return true;
		/*
		// If nothing needs aggregation, don't do it.
		Logger.log(LogMessageCategory.INFO,"Final aggregation does not need to be done.");
		return false;
		*/
	}

	/**
	 * Notes that a pollutant is calculated in a way that would benefit from aggregation.
	 * @param p Pollutant that would generate multiple records with identical human-read
	 * key fields without aggregation.
	**/
	public static void pollutantNeedsAggregation(Pollutant p) {
		if(p != null) {
			theExecutionRunSpec.pollutantsNeedingAggregation.add(p);
		}
	}

	/**
	 * Notes that a pollutant/process is calculated in a way that would benefit from aggregation.
	 * @param p PollutantProcessAssociation that would generate multiple records with identical
	 * human-read key fields without aggregation.
	**/
	public static void pollutantProcessNeedsAggregation(PollutantProcessAssociation p) {
		if(p != null) {
			theExecutionRunSpec.pollutantProcessesNeedingAggregation.add(p);
		}
	}

	/**
	 * Obtain the name of the output table to be used by the current runspec.
	 * @return the name of the table to hold emissions
	**/
	public static String getEmissionOutputTable() {
		return "MOVESOutput";
	}

	/**
	 * Obtain the name of the activity output table to be used by the current runspec.
	 * @return the name of the table to hold emissions
	**/
	public static String getActivityOutputTable() {
		return "MOVESActivityOutput";
	}

	/**
	 * Obtain the RunSpec attached to the current ExecutionRunSpec.
	 * @return the RunSpec attached to the current ExecutionRunSpec
	**/
	public static RunSpec getRunSpec() {
		return theExecutionRunSpec.targetRunSpec;
	}

	/**
	 * Obtain the database that provides scale/domain-specific inputs, such as the
	 * County Domain database or Project level database.  The database selection is
	 * only returned if a domain that requires it is selected and if the database
	 * selection has been filled out.
	 * @return a database to be used or null
	**/
	public static DatabaseSelection getDomainInputDatabase() {
		Models.ModelCombination mc = Models.evaluateModels(theExecutionRunSpec.targetRunSpec.models);
		switch (mc) {
		case M1: // ONROAD
			if(theExecutionRunSpec.targetRunSpec.domain == ModelDomain.NATIONAL_ALLOCATION) {
				return null;
			}
			DatabaseSelection db = theExecutionRunSpec.targetRunSpec.scaleInputDatabase;
			if(db == null || db.databaseName == null || db.databaseName.length() <= 0) {
				return null;
			}
			return db;
		case M2: // NONROAD - TODO implment later
			break;
		default: 
			break;
		}
		return null;
	}

	/**
	 * Populate RunSpecYear, RunSpecModelYearAge, RunSpecModelYear, RunSpecNonRoadModelYearAge,
	 * and RunSpecNonRoadModelYear.
	 * @param db database to be used
	 * @param years calendar years required
	 * @throws SQLException if anything goes wrong
	**/
	public static void fillYearsAndModelYears(Connection db, TreeSet<Integer> years)
			throws SQLException {
		TreeSet<Integer> onRoadModelYears = new TreeSet<Integer>();
		TreeSet<Integer> offRoadModelYears = new TreeSet<Integer>();
		String sql = "truncate RunSpecYear";
		SQLRunner.executeSQL(db,sql);

		sql = "truncate RunSpecModelYearAge";
		SQLRunner.executeSQL(db,sql);

		sql = "truncate RunSpecModelYearAgeGroup";
		SQLRunner.executeSQL(db,sql);

		sql = "truncate RunSpecModelYear";
		SQLRunner.executeSQL(db,sql);

		sql = "truncate RunSpecNonRoadModelYearAge";
		SQLRunner.executeSQL(db,sql);

		sql = "truncate RunSpecNonRoadModelYear";
		SQLRunner.executeSQL(db,sql);

		String yearSQL = "";
		for(Iterator<Integer> i = years.iterator(); i.hasNext();) {
			int year = i.next().intValue();
			if(yearSQL.length() > 0) {
				yearSQL += ",";
			}
			yearSQL += "(" + year + ")";

			// Do on road selections
			for(int age=0;age<=30;age++) { // ages 0-30, inclusive
				int modelYear = year - age;
				Integer modelYearObject = Integer.valueOf(modelYear);
				if(!onRoadModelYears.contains(modelYearObject)) {
					onRoadModelYears.add(modelYearObject);
					sql = "insert into RunSpecModelYear (modelYearID) values (" + modelYear + ")";
					SQLRunner.executeSQL(db,sql);
				}
				sql = "insert into RunSpecModelYearAge (yearID, modelYearID, ageID)"
						+ " values (" + year + "," + modelYear + "," + age + ")";
				SQLRunner.executeSQL(db,sql);
			}

			// Do nonroad selections
			for(int age=0;age<=50;age++) { // ages 0-50, inclusive
				int modelYear = year - age;
				Integer modelYearObject = Integer.valueOf(modelYear);
				if(!offRoadModelYears.contains(modelYearObject)) {
					offRoadModelYears.add(modelYearObject);
					sql = "insert into RunSpecNonRoadModelYear (modelYearID) values (" + modelYear + ")";
					SQLRunner.executeSQL(db,sql);
				}
				sql = "insert into RunSpecNonRoadModelYearAge (yearID, modelYearID, ageID)"
						+ " values (" + year + "," + modelYear + "," + age + ")";
				SQLRunner.executeSQL(db,sql);
			}
		}
		yearSQL = "insert into RunSpecYear (yearID) values " + yearSQL;
		SQLRunner.executeSQL(db,yearSQL);

		sql = "insert into runSpecModelYearAgeGroup (yearID, modelYearID, ageGroupID)"
				+ " select yearID, modelYearID, ageGroupID"
				+ " from runSpecModelYearAge rsmya"
				+ " inner join ageCategory ac on (ac.ageID = rsmya.ageID)";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Populate RunSpecSourceType
	 * @param db database to be used
	 * @param sourceTypes source types required
	 * @throws SQLException if anything goes wrong
	**/
	public static void fillSourceTypes(Connection db, TreeSet<Integer> sourceTypes)
			throws SQLException {
		String sql = "TRUNCATE RunSpecSourceType";
		SQLRunner.executeSQL(db,sql);

		for(Iterator<Integer> i = sourceTypes.iterator(); i.hasNext();) {
			sql = "insert into RunSpecSourceType (sourceTypeID) values (" + i.next() + ")";
			SQLRunner.executeSQL(db,sql);
		}
	}

	/**
	 * Populate RunSpecSector
	 * @param db database to be used
	 * @param sectors sectors required
	 * @throws SQLException if anything goes wrong
	**/
	public static void fillSectors(Connection db, TreeSet<Integer> sectors)
			throws SQLException {
		String sql = "TRUNCATE RunSpecSector";
		SQLRunner.executeSQL(db,sql);

		for(Iterator<Integer> i = sectors.iterator(); i.hasNext();) {
			sql = "insert into RunSpecSector (sectorID) values (" + i.next() + ")";
			SQLRunner.executeSQL(db,sql);
		}
	}

	/**
	 * Populate SQLMacroExpander with information from the execution database.
	 * @param db database to be used
	**/
	public static void setupMacroExpander(Connection db) {
		SQLMacroExpander.reset();
		String[] sets = {
			"", "select sourceTypeID from RunSpecSourceType",
			"", "select roadTypeID from RunSpecRoadType",
			"", "select monthID from RunSpecMonth",
			"", "select dayID from RunSpecDay",
			"", "select hourID from RunSpecHour",
			"", "select monthGroupID from RunSpecMonthGroup",
			"", "select yearID from RunSpecYear",
			"", "select modelYearID from RunSpecModelYear",
			"mya.", "select yearID, modelYearID, ageID from RunSpecModelYearAge",
			"sf.", "select sourceTypeID, fuelTypeID from RunSpecSourceFuelType",
			"", "select hourDayID from RunSpecHourDay",
			"", "select stateID from RunSpecState",
			"", "select countyID from RunSpecCounty",
			"", "select zoneID from RunSpecZone",
			"", "select linkID from RunSpecLink",
			"", "select pollutantID from RunSpecPollutant",
			"", "select processID from RunSpecProcess",
			"", "select polProcessID from RunSpecPollutantProcess",
			"", "select distinct soakDayID from sampleVehicleSoaking",
			"", "select distinct soakDayID as dayTwoPlusSoakDayID from sampleVehicleSoaking where soakDayID>1",
			"hd.", "select HourDay.hourDayID as hourDayID, dayID, hourID"
					+ " from RunSpecHourDay"
					+ " inner join HourDay using (hourDayID)",
			"ppa.", "select PollutantProcessAssoc.polProcessID as polProcessID, processID, pollutantID"
					+ " from RunSpecPollutantProcess"
					+ " inner join PollutantProcessAssoc using (polProcessID)",
			"mg.", "select monthOfAnyYear.monthID as monthID, monthGroupID"
					+ " from RunSpecMonth"
					+ " inner join monthOfAnyYear using (monthID)"
		};
		for(int i=0;i<sets.length;i+=2) {
			try {
				SQLMacroExpander.addData(sets[i],db,sets[i+1]);
			} catch(Exception e) {
				Logger.logError(e,"Unable to setup macro expansion using " + sets[i+1]);
			}
		}

		String[] csvsets = {
			"select sourceTypeID from RunSpecSourceType",
			"select distinct fuelTypeID from RunSpecSourceFuelType union select distinct fuelTypeID from RunSpecSectorFuelType",
			"select distinct fuelTypeID as nrFuelTypeID from RunSpecSectorFuelType",
			"select distinct fuelSubTypeID from fuelSubType where fuelTypeID in (select distinct fuelTypeID from RunSpecSourceFuelType union select distinct fuelTypeID from RunSpecSectorFuelType)",
			"select distinct fuelSubTypeID as nrFuelSubTypeID from nrFuelSubType where fuelTypeID in (select distinct fuelTypeID from RunSpecSourceFuelType union select distinct fuelTypeID from RunSpecSectorFuelType)",
			"select roadTypeID from RunSpecRoadType",
			"select monthID from RunSpecMonth",
			"select dayID from RunSpecDay",
			"select hourID from RunSpecHour",
			"select monthGroupID from RunSpecMonthGroup",
			"select yearID from RunSpecYear",
			"select modelYearID from RunSpecModelYear",
			"select hourDayID from RunSpecHourDay",
			//"select stateID from RunSpecState",
			//"select countyID from RunSpecCounty",
			//"select zoneID from RunSpecZone",
			//"select linkID from RunSpecLink",
			"select pollutantID from RunSpecPollutant",
			"select processID from RunSpecProcess",
			"select polProcessID from RunSpecPollutantProcess",
			"select sourceBinID from SourceBin"
		};
		for(int i=0;i<csvsets.length;i++) {
			try {
				SQLMacroExpander.addCSVData(db,csvsets[i],5000,false,false,"0");
			} catch(Exception e) {
				Logger.logError(e,"Unable to setup CSV macro expansion using " + csvsets[i]);
			}
		}

		SQLMacroExpander.compile();
	}

	/**
	 * Refresh SQLMacroExpander's use of sourceBinIDs.
	 * @param db database to be used
	**/
	public static void refreshMacroExpanderSourceBins(Connection db) {
		String[] csvsets = {
			"select sourceBinID from SourceBin"
		};
		for(int i=0;i<csvsets.length;i++) {
			try {
				SQLMacroExpander.addCSVData(db,csvsets[i],5000,false,false,"0");
			} catch(Exception e) {
				Logger.logError(e,"Unable to setup CSV macro expansion using " + csvsets[i]);
			}
		}
	}

	/**
	 * For distance calculations to work, there must be at least one pollutant
	 * from the Running Exhaust process that does not require distance as an
	 * input.  THC is an example that meets these needs.
	 * @return true if the runspec already contains a running exhaust pollutant that
	 * does not require distance as an input.
	**/
	public boolean doesHaveDistancePollutantAndProcess() {
		if(!doesHavePollutantAndProcess(0,1)) {
			return false;
		}
		for(Iterator<PollutantProcessAssociation> iterPollutantProcessAssociations =
				pollutantProcessAssociations.iterator();
				iterPollutantProcessAssociations.hasNext();) {
			PollutantProcessAssociation pollutantProcessAssociation =
					(PollutantProcessAssociation)iterPollutantProcessAssociations.next();
			if(pollutantProcessAssociation.emissionProcess.databaseKey == 1) { // If it is the Running Exhaust process...
				Pollutant p = pollutantProcessAssociation.pollutant;
				PollutantDisplayGroup g = p.displayGroup;
				if(g == null) {
					g = PollutantDisplayGroup.findByPollutantID(p.databaseKey);
					p.displayGroup = g;
				}
				if(g == null || !g.requiresDistance) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Checks for instantiated and enabled Rate of Progress
	 * @return true if the RunSpec requires rate of progress
	**/
	public static boolean hasRateOfProgress() {
		return theExecutionRunSpec.targetRunSpec.hasRateOfProgress();
	}

	/**
	 * Get the set of locations to be simulated.
	 * @return List of locations the Master Loop will be executed for.
	**/
	public TreeSet<ExecutionLocation> getExecutionLocations() {
		return executionLocations;
	}

	/**
	 * Convert an expression that references user-space model years into
	 * standardized model years suitable for joining to effects tables.
	 * @param expression column or SQL expression that yields a user-space
	 * model year.
	 * @return an expression that yields a standardized model year.
	**/
	public String convertToStandardModelYear(String expression) {
		return modelYearMapper.convertToStandard(expression);
	}

	/**
	 * Convert a SQL fragment that may or may not contain one or
	 * more references to the MYMAP pseudo function. Statements of
	 * the form MYMAP(expression) are converted to an expression
	 * that references stanardized model years.
	 * @param sqlLine SQL fragment that contains zero or more occurences
	 * of the MYMAP(expression) statement.
	 * @return SQL containing expressions that reference standardized
	 * model years.
	**/
	public String findAndConvertModelYearMapping(String sqlLine) {
		return modelYearMapper.findAndConvert(sqlLine);
	}
}
