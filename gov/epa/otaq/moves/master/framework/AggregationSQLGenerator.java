/**************************************************************************************************
 * @(#)AggregationSQLGenerator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.ModelScale;
import gov.epa.otaq.moves.common.ModelDomain;
import gov.epa.otaq.moves.common.Model;
import gov.epa.otaq.moves.common.Models;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.TreeSetIgnoreCase;
import gov.epa.otaq.moves.master.runspec.OutputTimeStep;
import gov.epa.otaq.moves.master.runspec.GeographicOutputDetailLevel;
import gov.epa.otaq.moves.master.runspec.GeographicSelectionType;
import gov.epa.otaq.moves.master.runspec.GeographicSelection;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import gov.epa.otaq.moves.common.TimeSpan;
import gov.epa.otaq.moves.common.OutputEmissionsBreakdownSelection;
import java.sql.*;
import java.util.*;
import gov.epa.otaq.moves.common.CompilationFlags;

/**
 * Generates SQL to perform aggregation at several stages of exeuction.  First, SQL is generated
 * for use by Workers.  This reduces the number of records that shipped between Master and
 * Worker.  Second, SQL is generated for the Master-side data.  Consider the case of a user
 * desiring National numbers.  Data is generated at the state and county level but must be
 * SUM'd to a single set of National numbers.  If MOVES were to wait until the last detail
 * had arrived from the Workers prior to doing any summing, there would be an unacceptably
 * large output table to sum over.  Instead, SQL is used to "collapse" the data in the output
 * table periodically.  Also, a final set of SQL is used to do any final aggregations.<br>
 * <br>
 * All SQL is generated, once, at the start of an execution run. The resulting SQL
 * is executed by the Workers or by the OutputProcessor after the calculated output
 * of a worker has been imported into the output database and the IntegratedPostProcessors
 * have been run.<br>
 * <br>
 * A word about aggregation concepts is essential to understanding this class.  To aggregate a table T
 * with detail columns A, B, and C and quantity column Q, one might use the following SQL:
 * <pre>
 * 	SELECT A, B, C, SUM(Q) As Q
 * 	FROM T
 * 	GROUP BY A, B, C
 * </pre>
 * If detail C is not required, then the following SQL could be used:
 * <pre>
 * 	SELECT A, B, null AS C, SUM(Q) AS Q
 * 	FROM T
 * 	GROUP BY A, B
 * </pre>
 * This form has the same column format as the original table T which is advantagous if the aggregated
 * output needs to be intermixed (even temporarily) with unaggregated output.  It also allows a single
 * database schema to be used regardless of the aggregation options selected.<br>
 * <br>
 * If neither detail B nor C is required, then the SQL becomes:
 * <pre>
 * 	SELECT A, null AS B, null AS C, SUM(Q) AS Q
 * 	FROM T
 * 	GROUP BY A
 * </pre>
 * Thus, the general rule for aggregation is to consider each detail column and contribute appropriately
 * to the SELECT'd columns clause and to the GROUP BY clause.<br>
 * <br>
 * Seldom is a mere SELECT appropriate.  In fact, the aggregated results in MOVES must end up back in the
 * original table (hence the use of "null AS C" clauses) intermixed with results from other runs.
 * To accomplish this, the general structure is revised to include a new temporary table T2 and filtering
 * of T's data:
 * <pre>
 * 	CREATE TABLE T2 .....
 *
 * 	INSERT INTO T2 (<font color=Red>A, B, C, Q</font>)
 * 	SELECT <font color=Blue>A, B, null AS C, SUM(Q) AS Q</font>
 * 	FROM T
 * 	WHERE T.RunID = (desired subset of simulation runs in T)
 * 	GROUP BY <font color=Green>A, B</font>
 *
 * 	DELETE FROM T
 * 	WHERE T.RunID = (desired subset of simulation runs in T)
 *
 * 	INSERT INTO T (<font color=Red>A, B, C, Q</font>)
 * 	SELECT <font color=Red>A, B, C, Q</font>
 * 	FROM T2
 *
 * 	DROP TABLE T2
 * </pre>
 * The need for the INSERT statement to name columns adds a third clause that the detail column examination
 * must contribute to.  The 3 common clauses are highlighted above.  The generateBaseSQLForAggregation()
 * method is responsible for building these common clauses, which are then assembled into specialized
 * SQL for Worker, intermediate processing, and final processing by the generateSQLsForWorker(),
 * generateSQLsForOutputProcessor(), and generateSQLsForFinalProcessing() methods respectively.<br>
 * <br>
 * The issue of inflating the daily output up to monthly numbers must also be considered during aggregation.
 * SQL provides a convenient keyword for this: CASE.  In general, CASE is used to change the value SELECT'd
 * depending upon an expression.  For example:
 * <pre>
 * 	SELECT C, CASE
 * 			WHEN C = 2 THEN Q * 28 / 7
 * 			WHEN C = 12 THEN Q * 31 / 7
 * 			ELSE Q * 30 / 7
 * 		  END AS Q
 * 	FROM T
 * </pre>
 * The above SQL will return a modified value of Q for each row by examining C in the row and performing
 * different math for each value of C.  Aggregating with this tool changes our SQL example to:
 * <pre>
 * 	SELECT A, B, null AS C, SUM(CASE
 * 			WHEN C = 2 THEN Q * 28 / 7
 * 			WHEN C = 12 THEN Q * 31 / 7
 * 			ELSE Q * 30 / 7
 * 		  END) AS Q
 * 	FROM T
 * 	GROUP BY A, B
 * </pre>
 * Note that the "C" column used in the CASE statement is the "C" column from the table, not the null value
 * labeled as "C" in the SELECT'd output.<br>
 * <br>
 * This technique is used in the SQL sent to Workers to inflate data to a value consistent with the
 * number of days in each month.
 * @author		Wesley Faler
 * @version		2017-03-22
**/
public class AggregationSQLGenerator {
	/** true to display aggregation SQL statements. **/
	private static final boolean shouldDebug = false;

	/** true when Nonroad results can be aggregated, false during testing **/
	private static final boolean ENABLE_NONROAD_AGGREGATION = true;

	/**
	 * Partial SQL to create the WorkerOutputTemp table from an output table.
	**/
	private String createSQL = "CREATE TABLE WorkerOutputTemp("
			+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
			+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
			+ " yearID SMALLINT UNSIGNED NULL,"
			+ " monthID SMALLINT UNSIGNED NULL,"
			+ " dayID SMALLINT UNSIGNED NULL,"
			+ " hourID SMALLINT UNSIGNED NULL,"
			+ " stateID SMALLINT UNSIGNED NULL,"
			+ " countyID INTEGER UNSIGNED NULL,"
			+ " zoneID INTEGER UNSIGNED NULL,"
			+ " linkID INTEGER UNSIGNED NULL,"
			+ " pollutantID SMALLINT UNSIGNED NULL,"
			+ " roadTypeID SMALLINT UNSIGNED NULL,"
			+ " processID SMALLINT UNSIGNED NULL,"
			+ " fuelTypeID SMALLINT UNSIGNED NULL,"
			+ " fuelSubTypeID SMALLINT UNSIGNED NULL,"
			+ " modelYearID SMALLINT UNSIGNED NULL,"
			+ " sourceTypeID SMALLINT UNSIGNED NULL,"
			+ " regClassID SMALLINT UNSIGNED NULL,"
			+ " SCC CHAR(10) NULL,"
			+ " engTechID SMALLINT UNSIGNED NULL DEFAULT NULL,"
			+ " sectorID SMALLINT UNSIGNED NULL DEFAULT NULL,"
			+ " hpID SMALLINT UNSIGNED NULL DEFAULT NULL,"
			+ " emissionQuant FLOAT NULL,"
			+ " emissionRate FLOAT NULL)";
	/**
	 * Partial SQL to create the WorkerOutputActivityTemp table from an output table.
	**/
	private String createActivitySQL = "CREATE TABLE WorkerActivityOutputTemp("
			+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
			+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
			+ " yearID SMALLINT UNSIGNED NULL,"
			+ " monthID SMALLINT UNSIGNED NULL,"
			+ " dayID SMALLINT UNSIGNED NULL,"
			+ " hourID SMALLINT UNSIGNED NULL,"
			+ " stateID SMALLINT UNSIGNED NULL,"
			+ " countyID INTEGER UNSIGNED NULL,"
			+ " zoneID INTEGER UNSIGNED NULL,"
			+ " linkID INTEGER UNSIGNED NULL,"
			+ " roadTypeID SMALLINT UNSIGNED NULL,"
			+ " fuelTypeID SMALLINT UNSIGNED NULL,"
			+ " fuelSubTypeID SMALLINT UNSIGNED NULL,"
			+ " modelYearID SMALLINT UNSIGNED NULL,"
			+ " sourceTypeID SMALLINT UNSIGNED NULL,"
			+ " regClassID SMALLINT UNSIGNED NULL,"
			+ " SCC CHAR(10) NULL,"
			+ " engTechID SMALLINT UNSIGNED NULL DEFAULT NULL,"
			+ " sectorID SMALLINT UNSIGNED NULL DEFAULT NULL,"
			+ " hpID SMALLINT UNSIGNED NULL DEFAULT NULL,"
			+ " activityTypeID SMALLINT NOT NULL,"
			+ " activity FLOAT NULL"
			+ ") ";

	/**
	 * Partial SQL to create the WorkerBaseRateOutputTemp table from an output table.
	**/
	private String createBaseRateOutputSQL = "CREATE TABLE WorkerBaseRateOutputTemp like BaseRateOutput";

	/**
	 * SQL to insert into a worker's WorkerOutputTemp table from an output table.
	**/
	private String workerInsertSQL;

	/**
	 * SQL to insert into master's WorkerOutputTemp table from an output table.
	**/
	private String masterInsertSQL;

	/**
	 * SQL to select from a master's output table. Insert into WorkerOutputTemp uses this select statement.
	**/
	private String masterSelectSQL;

	/**
	 * SQL to select from a worker's output table. Insert into WorkerOutputTemp uses this select statement.
	**/
	private String workerSelectSQL;

	/**
	 * Group by segment of the SQL for select statement.
	**/
	private String groupBy;

	/**
	 * SQL to insert into WorkerActivityOutputTemp table from an activity output table.
	**/
	private String insertActivitySQL;

	/**
	 * SQL to select from an activity output table. Insert into WorkerActivityOutputTemp uses this
	 * select statement.
	**/
	private String selectActivitySQL;

	/**
	 * Group by segment of the SQL for activity output select statement.
	**/
	private String groupByActivity;

	/** Group by segment of the SQL for population activity output **/
	private String groupByActivitySpatialOnly = "";
	/**
	 * SQL to select population from an activity output table. Insert into
	 * WorkerActivityOutputTemp uses this select statement.
	**/
	private String selectActivityNoScaleSQL = "";

	/**
	 * SQL to insert into WorkerBaseRateOutputTemp table.
	**/
	private String insertBaseRateOutputSQL;

	/**
	 * SQL to select from a BaseRateOutput table. Insert into WorkerBaseRateOutputTemp uses this
	 * select statement.
	**/
	private String selectBaseRateOutputSQL;

	/**
	 * Group by segment of the SQL for BaseRateOutput select statement.
	**/
	private String groupByBaseRateOutput;

	/** SQL executed by outputProcessor while processing worker files. **/
	public Vector<String> outputProcessorSQLs = new Vector<String>();

	/** SQL statements to be executed by the worker.  Each must end with a semicolon. **/
	public Vector<String> workerSQLs = new Vector<String>();

	/**
	 * Final aggregation SQL statements, executed by OutputProcessor while
	 * doing final processing.
	**/
	public Vector<String> finalProcessSQLs = new Vector<String>();

	/**
	 * Output fields on a master, to insert values into output table from the temporary aggregated table.
	**/
	private String masterOutputTableFields = "MOVESRunID,"
			+"iterationID,"
			+"yearID,"
			+"monthID,"
			+"dayID,"
			+"hourID,"
			+"stateID,"
			+"countyID,"
			+"zoneID,"
			+"linkID,"
			+"pollutantID,"
			+"roadTypeID,"
			+"processID,"
			+"sourceTypeID,"
			+"regClassID,"
			+"fuelTypeID,"
			+(CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT? "fuelSubTypeID," : "")
			+"modelYearID,"
			+"SCC,"
			+"engTechID,"
			+"sectorID,"
			+"hpID,"
			+"emissionQuant";

	/**
	 * Output fields on worker, to insert values into output table from the temporary aggregated table.
	**/
	private String workerOutputTableFields = "MOVESRunID,"
			+"iterationID,"
			+"yearID,"
			+"monthID,"
			+"dayID,"
			+"hourID,"
			+"stateID,"
			+"countyID,"
			+"zoneID,"
			+"linkID,"
			+"pollutantID,"
			+"roadTypeID,"
			+"processID,"
			+"sourceTypeID,"
			+"regClassID,"
			+"fuelTypeID,"
			+"fuelSubTypeID,"
			+"modelYearID,"
			+"SCC,"
			+"engTechID,"
			+"sectorID,"
			+"hpID,"
			+"emissionQuant,"
			+"emissionRate";

	/**
	 * Activity Output fields, to insert values into activity output table from the
	 * temporary aggregated table.
	**/
	private String outputActivityTableFields = "MOVESRunID,"
			+"iterationID,"
			+"yearID,"
			+"monthID,"
			+"dayID,"
			+"hourID,"
			+"stateID,"
			+"countyID,"
			+"zoneID,"
			+"linkID,"
			+"roadTypeID,"
			+"sourceTypeID,"
			+"regClassID,"
			+"fuelTypeID,"
			+(CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT? "fuelSubTypeID," : "")
			+"modelYearID,"
			+"SCC,"
			+"engTechID,"
			+"sectorID,"
			+"hpID,"
			+"activityTypeID,"
			+"activity";

	/**
	 * BaseRateOutput fields, to insert values into output table from the temporary aggregated table.
	**/
	private String outputBaseRateOutputTableFields = "MOVESRunID,"
			+"iterationID,"
			+"zoneID,"
			+"linkID,"
			+"sourceTypeID,"
			+"SCC,"
			+"roadTypeID,"
			+"avgSpeedBinID,"
			+"monthID,"
			+"hourDayID,"
			+"pollutantID,"
			+"processID,"
			+"modelYearID,"
			+"yearID,"
			+"fuelTypeID,"
			+"regClassID,"
			+"meanBaseRate,"
			+"emissionRate";

	/**
	 * Select statement for inserting values into a master Output table. Doesn't have FROM, WHERE, ORDER BY,
	 * GROUP BY. Just fields.
	**/
	private String selectSQLForMasterOutput = "SELECT " + masterOutputTableFields;

	/**
	 * Select statement for inserting values into a worker Output table. Doesn't have FROM, WHERE, ORDER BY,
	 * GROUP BY. Just fields.
	**/
	private String selectSQLForWorkerOutput = "SELECT " + workerOutputTableFields;

	/**
	 * Select statement for inserting values into Activity Output table. Doesn't have FROM, WHERE,
	 * ORDER BY, GROUP BY. Just fields.
	**/
	private String selectSQLForActivityOutput = "SELECT " + outputActivityTableFields;

	/**
	 * Select statement for inserting values into BaseRateOutput table. Doesn't have FROM, WHERE,
	 * ORDER BY, GROUP BY. Just fields.
	**/
	private String selectSQLForBaseRateOutput = "SELECT " + outputBaseRateOutputTableFields;

	/** The active run ID. This is significant in the output database. **/
	int activeRunID = 0;

	/** The active iteration ID. This is significant in the output database. **/
	int activeIterationID = 0;

	/** true after issues have already been reported **/
	boolean didReportIssues = false;

	/** true after report SQL has already been generated **/
	boolean didGenerateReportSQL = false;

	/** true when Nonroad activity outputs must be weighted before aggregation **/
	boolean nrNeedsActivityWeight = false;

	/** SQL statements that weight Nonroad activity output before aggregation **/
	ArrayList<String> nrActivityWeightSQL = new ArrayList<String>();
	
	TreeSetIgnoreCase nrActivitySummaryColumns = new TreeSetIgnoreCase();

	/**
	 * Default constructor
	**/
	public AggregationSQLGenerator() {

	}

	/**
	 * Builds the aggregation SQL using data from the ExecutionRunSpec singleton. Typically, this
	 * method is called once and the aggregated result will be stored in an output table.
	 * @return false if fails
	**/
	public boolean generateReportSQL() {
		if(MOVESEngine.theInstance != null) {
			activeRunID = MOVESEngine.theInstance.getActiveRunID();
			activeIterationID = MOVESEngine.theInstance.getActiveIterationID();
		}
		if(didGenerateReportSQL) {
			return true;
		}
		if(!validateAggregation()) {
			return false;
		}
		if(!generateSQLsForWorker()) {
			return false;
		}
		logSQL("Worker",workerSQLs);

		if(!generateSQLsForOutputProcessor()) {
			return false;
		}
		logSQL("OutputProcessor",outputProcessorSQLs);

		if(!generateSQLsForFinalProcessing()) {
			return false;
		}
		logSQL("Final",finalProcessSQLs);

		didGenerateReportSQL = true;
		return true;
	}

	/**
	 * Display aggregation SQL statements.
	 * @param purpose use of the SQL, such as "Worker" or "Final".
	 * @param statements SQL statements, never null, may be empty.
	**/
	void logSQL(String purpose, Vector<String> statements) {
		if(!shouldDebug) {
			return;
		}
		Logger.log(LogMessageCategory.DEBUG,"Aggregation statements for: " + purpose);
		for(String sql : statements) {
			if(sql.length() > 0) {
				Logger.log(LogMessageCategory.DEBUG,sql);
			}
		}
		Logger.log(LogMessageCategory.DEBUG,"End of aggregation statements for: " + purpose);
	}

	/**
	 * Validates the RunSpec's output parameters before aggregation.
	 * @return false if there is any problem during validation
	**/
	boolean validateAggregation() {
		try {
			// All the time periods required are present in the runspec.
			OutputTimeStep outputTimeStep =
					ExecutionRunSpec.theExecutionRunSpec.getOutputTimeStep();
			TimeSpan timeSpan = ExecutionRunSpec.theExecutionRunSpec.getTimeSpan();
			if(!ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)) {
				if(outputTimeStep.requiresAllHours()) {
					if(!timeSpan.hasAllHours() && !didReportIssues) {
						/**
						 * @explain Hour is not a desired output dimension yet aggregation is
						 * called for.  When reporting results, be sure to include the fact that the
						 * totals do not include all hours.
						**/
						Logger.log(LogMessageCategory.WARNING,
								"Warning: RunSpec doesn't have all the Hours.");
					}
				}
			}
			Connection defaultDB = null;
			try {
				defaultDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
				if(outputTimeStep.requiresAllDays()) {
					if(!timeSpan.hasAllDays(defaultDB) && !didReportIssues) {
						/**
						 * @explain Day is not a desired output dimension yet aggregation is
						 * called for.  When reporting results, be sure to include the fact that the
						 * totals do not include all days.
						**/
						Logger.log(LogMessageCategory.WARNING,
								"Warning: RunSpec doesn't have all the Days.");
					}
				}
			} catch(Exception e) {
				/**
				 * @explain A database error was encountered while checking for aggregation
				 * suitability.
				**/
				Logger.logError(e, "Failed to validate data for aggregation");
				return false;
			} finally {
				if(defaultDB != null) {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
							defaultDB);
					defaultDB = null;
				}
			}
			if(outputTimeStep.requiresAllMonths()) {
				if(!timeSpan.hasAllMonths() && !didReportIssues) {
					/**
					 * @explain Month is not a desired output dimension yet aggregation is
					 * called for.  When reporting results, be sure to include the fact that the
					 * totals do not include all months.
					**/
					Logger.log(LogMessageCategory.WARNING,
							"Warning: RunSpec doesn't have all the Months.");
				}
			}

			// All the roadtypes required are present in the runspec.
			Connection executionDatabase = null;
			try {
				executionDatabase = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.EXECUTION);
				// Size comparision, is to make sure runspec has all the roadtypes.
				if(!ExecutionRunSpec.theExecutionRunSpec.
						getOutputEmissionsBreakdownSelection().roadType) {
					int dbRoadTypeCount = 0;
					String sql = "SELECT COUNT(*) FROM RoadType where shouldDisplay=1 and roadTypeID not in (1,100)";
					Models.ModelCombination mc = ExecutionRunSpec.getRunSpec().getModelCombination();
					switch (mc) {
						case M1:
							sql += " and isAffectedByOnroad is true";
							break;
						case M2:
							sql += " and isAffectedByNonroad is true";
							break;
						default:
							break;
					}
					ResultSet result = SQLRunner.executeQuery(executionDatabase, sql);
					if(result.next()) {
						dbRoadTypeCount = result.getInt(1);
					}
					result.close();

					int runSpecRoadTypeCount = 0;					
					for(RoadType r : ExecutionRunSpec.theExecutionRunSpec.getRoadTypes()) {
						if(r.roadTypeID == 1 || r.roadTypeID == 100
								|| r.roadTypeID == 6 || r.roadTypeID == 7
								|| r.roadTypeID == 8 || r.roadTypeID == 9) {
							// Skip hidden roads, only counting road types
							// that matter when aggregated (non-1, non-100)
							// and that are shown to the user.
							continue;
						}
						if(ExecutionRunSpec.theExecutionRunSpec.getModels().containsAny(r.mc)) {
							runSpecRoadTypeCount++;
						}
					}
					if(runSpecRoadTypeCount != dbRoadTypeCount && !didReportIssues) {
						/**
						 * @explain RoadType is not a desired output dimension yet aggregation is
						 * called for.  When reporting results, be sure to include the fact that the
						 * totals do not include all road types.
						**/
						Logger.log(LogMessageCategory.WARNING,
								"Warning: RunSpec doesn't have all the RoadTypes.");
					}
				}
			} catch(Exception e) {
				/**
				 * @explain A database error was encountered while checking for aggregation
				 * suitability.
				**/
				Logger.logError(e, "Failed to validate data for aggregation");
				return false;
			} finally {
				if(executionDatabase != null) {
					DatabaseConnectionManager.checkInConnection(
							MOVESDatabaseType.EXECUTION, executionDatabase);
					executionDatabase = null;
				}
			}

			// Can uncertainty estimation be performed.
			OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
				ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection();
			if(outputEmissionsBreakdownSelection.estimateUncertainty) {
				TreeSet geographicSelections =
						ExecutionRunSpec.theExecutionRunSpec.getGeographicSelections();
				GeographicSelectionType geoType = null;
				Iterator i = geographicSelections.iterator();
				if(i.hasNext()) {
					geoType = ((GeographicSelection) i.next()).type;
				}
				if(geoType != null) {
					if(geoType == GeographicSelectionType.NATION
							|| geoType == GeographicSelectionType.STATE) {
						if(!didReportIssues) {
							/**
							 * @explain Uncertainty cannot be done for NATION or STATE aggregation.
							**/
							Logger.log(LogMessageCategory.WARNING,
								"Uncertainty cannot be estimated for aggregated data.");
							return false;
						}
					}
				}
			}
			return true;
		} finally {
			didReportIssues = true;
		}
	}

	/**
	 * Generates SQLs to be processed by worker, to aggregate worker output.
	 * <p>
	 * <b>NOTE: ALL SQL STATEMENTS CREATED FOR WORKERS *MUST* END WITH A SEMICOLON!!!</b>
	 * This allows parsing of the SQL file by the worker when the statements
	 * are multiple lines long.
	 * @return false if there is any problem while generating SQLs.
	**/
	boolean generateSQLsForWorker() {
		if(!ENABLE_NONROAD_AGGREGATION
				&& ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)) {
			// Temporarily disable all aggregation for Nonroad runs.
			return true;
		}

		workerSQLs.add("starttimer GeneralAggregation;");

		// Populate the SCC fields for onroad. Do this after setting the road type
		// as SCC contains the road type.
		if(ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection().onRoadSCC
				&& !ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)) {
			String[] tables = {
				"MOVESWorkerOutput", // "MOVESWorkerActivityOutput" Doesn't have processID, so can't make a SCC
				(CompilationFlags.DO_RATES_FIRST? "BaseRateOutput":"")
			};
			String sql = "";
			for(int i=0;i<tables.length;i++) {
				String tableName = tables[i];
				if(tableName.length() <= 0) {
					continue;
				}
				sql = "update " + tableName + " set scc=concat('22',lpad(fuelTypeID,2,'00'),lpad(sourceTypeID,2,'00'),lpad(roadTypeID,2,'00'),lpad(processID,2,'00'));";
				workerSQLs.add(sql);
			}
			/*
			Activity table doesn't technically require a SCC but it would help future reporting.
			The process to be used is implied by the activity type:
				activityTypeID|activityType|activityTypeDesc         |Process to use
				1             |distance    |Distance traveled        |1
				2             |sourcehours |Source Hours             |1
				4             |sho         |Source Hours Operating   |1
				6             |population  |Population               |1
				5             |shp         |Source Hours Parked      |2
				7             |starts      |Starts                   |2
				3             |extidle     |Extended Idle Hours      |90
				8             |hotelling   |Hotelling Hours          |90
				9             |avghp       |Average Horsepower       |n/a
				10            |retrofrac   |Fraction Retrofitted     |n/a
				11            |retrocnt    |Number Units Retrofitted |n/a
				12            |loadfactor  |Load Factor              |n/a
				13            |hotelling   |Hotelling Diesel Aux     |90
				14            |hotelling   |Hotelling Battery or AC  |90
				15            |hotelling   |Hotelling All Engines Off|90
			*/
			sql = "update MOVESWorkerActivityOutput set scc=concat('22',lpad(fuelTypeID,2,'00'),lpad(sourceTypeID,2,'00'),lpad(roadTypeID,2,'00'),"
					+ " lpad(case"
					+ " when activityTypeID in (1,2,4,6) then 1"
					+ " when activityTypeID in (5,7) then 2"
					+ " when activityTypeID in (3,8,13,14,15) then 90"
					+ " else null end"
					+ " ,2,'00'));";
			workerSQLs.add(sql);
		}

		try {
			if(!generateBaseSQLForAggregation(true)) {
				return false;
			}
			workerSQLs.add("starttimer OutputAggregation;");
			workerSQLs.add("DROP TABLE IF EXISTS WorkerOutputTemp;");
			workerSQLs.add(createSQL+";");
			//workerSQLs.add("FLUSH TABLES;");
			workerSQLs.add(workerInsertSQL + ") " + workerSelectSQL + " FROM MOVESWorkerOutput "
					+ groupBy + ";");
			workerSQLs.add("TRUNCATE MOVESWorkerOutput;");
			//workerSQLs.add("FLUSH TABLES;");
			String insertOutputSQL = "INSERT INTO MOVESWorkerOutput (" + workerOutputTableFields + ") "
					+ selectSQLForWorkerOutput + " FROM WorkerOutputTemp;";
			workerSQLs.add(insertOutputSQL+";");
			//workerSQLs.add("FLUSH TABLES;");
			workerSQLs.add("DROP TABLE IF EXISTS WorkerOutputTemp;");
			//workerSQLs.add("FLUSH TABLES;");
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while creating SQL statements for
			 * aggregating data.
			**/
			Logger.logError(e, "Failed to generate SQL for Workers");
			return false;
		}

		try {
			if(nrNeedsActivityWeight) {
				workerSQLs.add("starttimer NonroadAggregation;");
				for(String s : nrActivityWeightSQL) {
					workerSQLs.add(s);
				}
			}

			workerSQLs.add("starttimer ActivityAggregation;");
			workerSQLs.add("DROP TABLE IF EXISTS WorkerActivityOutputTemp;");
			workerSQLs.add(createActivitySQL+";");
			//workerSQLs.add("FLUSH TABLES;");

			if(ExecutionRunSpec.getRunSpec().outputPopulation) {
				workerSQLs.add(insertActivitySQL + ") " + selectActivitySQL
						+ " FROM MOVESWorkerActivityOutput "
						+ " WHERE activityTypeID <> 6 "
						+ groupByActivity + ";");
				workerSQLs.add(insertActivitySQL + ") " + selectActivityNoScaleSQL
						+ " FROM MOVESWorkerActivityOutput "
						+ " WHERE activityTypeID=6 "
						+ groupByActivitySpatialOnly + ";");
			} else {
				workerSQLs.add(insertActivitySQL + ") " + selectActivitySQL
						+ " FROM MOVESWorkerActivityOutput "
						+ groupByActivity + ";");
			}

			workerSQLs.add("TRUNCATE MOVESWorkerActivityOutput;");
			//workerSQLs.add("FLUSH TABLES;");
			String insertActivityOutputSQL = "INSERT INTO MOVESWorkerActivityOutput ("
					+ outputActivityTableFields + ") "
					+ selectSQLForActivityOutput + " FROM WorkerActivityOutputTemp;";
			workerSQLs.add(insertActivityOutputSQL+";");
			//workerSQLs.add("FLUSH TABLES;");
			workerSQLs.add("DROP TABLE IF EXISTS WorkerActivityOutputTemp;");
			// Update all MOVESWorkerActivityOutput key fields to non-NULL values
			// This facilitates the overlay algorithm needed by the master.
			String sql = "UPDATE MOVESWorkerActivityOutput SET ";
			String[] affectedColumns = {
				"yearID", "monthID", "dayID", "hourID", "stateID", "countyID", "zoneID",
				"linkID", "sourceTypeID", "regClassID", "fuelTypeID", "modelYearID", "roadTypeID",
				"engTechID", "sectorID", "hpID"
			};
			for(int i=0;i<affectedColumns.length;i++) {
				if(i > 0) {
					sql += ", ";
				}
				sql += affectedColumns[i] + "=coalesce(" + affectedColumns[i] + ",0)";
			}
			sql += ", scc=coalesce(scc,'NOTHING');";
			workerSQLs.add(sql);
			//workerSQLs.add("FLUSH TABLES;");
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while creating SQL statements for
			 * aggregating data.
			**/
			Logger.logError(e, "Failed to generate SQL for Workers Activity");
			return false;
		}

		workerSQLs.add("starttimer GeneralAggregation;");

		// Aggregate base rates
		if(CompilationFlags.DO_RATES_FIRST) {
			try {
				workerSQLs.add("DROP TABLE IF EXISTS WorkerBaseRateOutputTemp;");
				workerSQLs.add(createBaseRateOutputSQL+";");
				workerSQLs.add(insertBaseRateOutputSQL + ") " + selectBaseRateOutputSQL
						+ " FROM BaseRateOutput "
						+ groupByBaseRateOutput + ";");
				workerSQLs.add("TRUNCATE BaseRateOutput;");
				//workerSQLs.add("FLUSH TABLES;");
				String insertBaseRateOutputSQL = "INSERT INTO BaseRateOutput ("
						+ outputBaseRateOutputTableFields + ") "
						+ selectSQLForBaseRateOutput + " FROM WorkerBaseRateOutputTemp;";
				workerSQLs.add(insertBaseRateOutputSQL+";");
				//workerSQLs.add("FLUSH TABLES;");
				workerSQLs.add("DROP TABLE IF EXISTS WorkerBaseRateOutputTemp;");
				//workerSQLs.add("FLUSH TABLES;");
			} catch(Exception e) {
				/**
				 * @explain A database error occurred while creating SQL statements for
				 * aggregating data.
				**/
				Logger.logError(e, "Failed to generate SQL for Base Rate Output");
				return false;
			}
		}

		return true;
	}

	/**
	 * This method generates the base SQL for the aggregation and populates createSQL, 
	 * masterInsertSQL, workerInsertSQL, masterSelectSQL, and workerSelectSQL.
	 * @param isWorkerSQL true if the SQL is being created for use on the workers
	 * @return false if there is any problem creating the SQL
	**/
	boolean generateBaseSQLForAggregation(boolean isWorkerSQL) {
		boolean convertDays = false;
		boolean convertWeeks = false;
		masterInsertSQL = "INSERT INTO WorkerOutputTemp ( MOVESRunID, iterationID";
		workerInsertSQL = "INSERT INTO WorkerOutputTemp ( MOVESRunID, iterationID";
		masterSelectSQL = " SELECT MOVESRunID, iterationID";
		workerSelectSQL = " SELECT MOVESRunID, iterationID";
		groupBy = "GROUP BY MOVESRunID, iterationID";

		insertActivitySQL = "INSERT INTO WorkerActivityOutputTemp ( MOVESRunID, iterationID";
		selectActivitySQL = " SELECT MOVESRunID, iterationID";
		groupByActivity = "GROUP BY MOVESRunID, iterationID";
		groupByActivitySpatialOnly = "GROUP BY MOVESRunID, iterationID";

		insertBaseRateOutputSQL = "INSERT INTO WorkerBaseRateOutputTemp ( MOVESRunID, iterationID";
		selectBaseRateOutputSQL = " SELECT MOVESRunID, iterationID";
		groupByBaseRateOutput = "GROUP BY MOVESRunID, iterationID";

		nrNeedsActivityWeight = false;
		nrActivityWeightSQL.clear();
		nrActivitySummaryColumns.clear();

		try {
			// Time period
			masterInsertSQL += ", yearID, monthID, dayID, hourID";
			workerInsertSQL += ", yearID, monthID, dayID, hourID";
			insertActivitySQL += ", yearID, monthID, dayID, hourID";
			insertBaseRateOutputSQL += ", yearID, monthID, hourDayID";
			OutputTimeStep outputTimeStep = ExecutionRunSpec.theExecutionRunSpec.getOutputTimeStep();

			groupByActivitySpatialOnly += ", yearID, monthID, dayID, hourID";
			if(outputTimeStep.requiresAllHours() && outputTimeStep.requiresAllDays()
					&& outputTimeStep.requiresAllMonths()) {
				// Output results in years.
				masterSelectSQL += ", yearID, null as monthID, null as dayID, null as hourID";
				workerSelectSQL += ", yearID, null as monthID, null as dayID, null as hourID";
				groupBy += ", yearID";
				convertWeeks = true;
				selectActivitySQL += ", yearID, null as monthID, null as dayID, null as hourID";
				groupByActivity += ", yearID";
				selectBaseRateOutputSQL += ", yearID, 0 as monthID, 0 as hourDayID";
				groupByBaseRateOutput += ", yearID";

				nrActivitySummaryColumns.add("yearID");
				nrNeedsActivityWeight = true;
			} else if(outputTimeStep.requiresAllHours() &&
					outputTimeStep.requiresAllDays()) {
				// Output results in months
				masterSelectSQL += ", yearID, monthID, null as dayID, null as hourID";
				workerSelectSQL += ", yearID, monthID, null as dayID, null as hourID";
				groupBy += ", yearID, monthID";
				convertWeeks = true;
				selectActivitySQL += ", yearID, monthID, null as dayID, null as hourID";
				groupByActivity += ", yearID, monthID";
				selectBaseRateOutputSQL += ", yearID, monthID, 0 as hourDayID";
				groupByBaseRateOutput += ", yearID, monthID";

				nrActivitySummaryColumns.add("yearID");
				nrActivitySummaryColumns.add("monthID");
				nrNeedsActivityWeight = true;
			} else if(outputTimeStep.requiresAllHours()) {
				// Output results in days.
				convertDays = outputTimeStep.usesClassicalDay();
				masterSelectSQL += ", yearID, monthID, dayID, null as hourID";
				workerSelectSQL += ", yearID, monthID, dayID, null as hourID";
				groupBy += ", yearID, monthID, dayID";
				selectActivitySQL += ", yearID, monthID, dayID, null as hourID";
				groupByActivity += ", yearID, monthID, dayID";

				selectBaseRateOutputSQL += ", yearID, monthID, hourDayID";
				groupByBaseRateOutput += ", yearID, monthID, hourDayID";

				nrActivitySummaryColumns.add("yearID");
				nrActivitySummaryColumns.add("monthID");
				nrActivitySummaryColumns.add("dayID");
			} else {
				// Output results in hours.
				convertDays = outputTimeStep.usesClassicalDay();
				masterSelectSQL += ", yearID, monthID, dayID, hourID";
				workerSelectSQL += ", yearID, monthID, dayID, hourID";
				groupBy += ", yearID, monthID, dayID, hourID";
				selectActivitySQL += ", yearID, monthID, dayID, hourID";
				groupByActivity += ", yearID, monthID, dayID, hourID";

				selectBaseRateOutputSQL += ", yearID, monthID, hourDayID";
				groupByBaseRateOutput += ", yearID, monthID, hourDayID";

				nrActivitySummaryColumns.add("yearID");
				nrActivitySummaryColumns.add("monthID");
				nrActivitySummaryColumns.add("dayID");
				//nrActivitySummaryColumns.add("hourID");
			}

			// PollutantID
			masterInsertSQL += ", pollutantID";
			workerInsertSQL += ", pollutantID";
			masterSelectSQL += ", pollutantID";
			workerSelectSQL += ", pollutantID";
			groupBy += ", pollutantID";

			// AvgSpeedBinID and PollutantID
			insertBaseRateOutputSQL +=", avgSpeedBinID, pollutantID";
			selectBaseRateOutputSQL += ", avgSpeedBinID, pollutantID";
			groupByBaseRateOutput += ", avgSpeedBinID, pollutantID";

			// activityTypeID
			insertActivitySQL +=", activityTypeID";
			selectActivitySQL += ", activityTypeID";
			groupByActivity += ", activityTypeID";
			groupByActivitySpatialOnly += ", activityTypeID";
			nrActivitySummaryColumns.add("activityTypeID");

			// Geographic detail
			GeographicOutputDetailLevel geographicOutputDetail = ExecutionRunSpec.theExecutionRunSpec.getGeographicOutputDetailLevel();
			ModelScale scale = ExecutionRunSpec.theExecutionRunSpec.getModelScale();
			ModelDomain domain = ExecutionRunSpec.theExecutionRunSpec.getModelDomain();

			/*
			System .out.println("geographicOutputDetail=" + geographicOutputDetail.toString());
			System .out.println("scale=" + scale.toString());
			System .out.println("domain=" + domain.toString());
			*/
			if(scale == null || scale == ModelScale.MACROSCALE) {
				if(geographicOutputDetail == GeographicOutputDetailLevel.NATION) {
					// This is ok
				} else if(geographicOutputDetail == GeographicOutputDetailLevel.STATE) {
					// This is ok
				} else if(geographicOutputDetail == GeographicOutputDetailLevel.COUNTY) {
					// This is ok
				} else if(geographicOutputDetail == GeographicOutputDetailLevel.ZONE) {
					// This is ok
				} else { // Link detail
					if(domain == ModelDomain.PROJECT) {
						// This is ok
					} else {
						// Force this change.  Macroscale, Non-Project cannot use the link level.
						geographicOutputDetail = GeographicOutputDetailLevel.COUNTY;
					}
				}
			}

			if(geographicOutputDetail == GeographicOutputDetailLevel.NATION) {
				masterSelectSQL += ", null as stateID, null as countyID, null as zoneID, null as linkID";
				workerSelectSQL += ", null as stateID, null as countyID, null as zoneID, null as linkID";
				masterInsertSQL += ", stateID, countyID, zoneID, linkID";
				workerInsertSQL += ", stateID, countyID, zoneID, linkID";
				selectActivitySQL += ", null as stateID, null as countyID, null as zoneID, "
						+ "null as linkID";
				insertActivitySQL += ", stateID, countyID, zoneID, linkID";

				selectBaseRateOutputSQL += ", 0 as zoneID, 0 as linkID";
				insertBaseRateOutputSQL += ", zoneID, linkID";

				nrNeedsActivityWeight = true;
			} else if(geographicOutputDetail == GeographicOutputDetailLevel.STATE) {
				if(ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD) && isWorkerSQL) {
					// In the NR worker case, countyID is not aggregated over because this detail is needed
					// on the master side to correctly aggregate LF and avgHP over multiple counties
					masterSelectSQL += ", stateID, countyID, null as zoneID, null as linkID";
					workerSelectSQL += ", stateID, countyID, null as zoneID, null as linkID";
					groupBy += ", stateID";
					masterInsertSQL += ", stateID, countyID, zoneID, linkID";
					workerInsertSQL += ", stateID, countyID, zoneID, linkID";
					selectActivitySQL += ", stateID, countyID, null as zoneID, null as linkID";
					groupByActivity += ", stateID, countyID";
					groupByActivitySpatialOnly += ", stateID, countyID";
					insertActivitySQL += ", stateID, countyID, zoneID, linkID";

					selectBaseRateOutputSQL += ", 0 as zoneID, 0 as linkID";
					insertBaseRateOutputSQL += ", zoneID, linkID";

					nrActivitySummaryColumns.add("stateID");
					nrActivitySummaryColumns.add("countyID");
					nrNeedsActivityWeight = true;					
				} else {
					// In the onroad case or NR master case, countyID is aggregated over as expected
					masterSelectSQL += ", stateID, null as countyID, null as zoneID, null as linkID";
					workerSelectSQL += ", stateID, null as countyID, null as zoneID, null as linkID";
					groupBy += ", stateID";
					masterInsertSQL += ", stateID, countyID, zoneID, linkID";
					workerInsertSQL += ", stateID, countyID, zoneID, linkID";
					selectActivitySQL += ", stateID, null as countyID, null as zoneID, null as linkID";
					groupByActivity += ", stateID";
					groupByActivitySpatialOnly += ", stateID";
					insertActivitySQL += ", stateID, countyID, zoneID, linkID";

					selectBaseRateOutputSQL += ", 0 as zoneID, 0 as linkID";
					insertBaseRateOutputSQL += ", zoneID, linkID";

					nrActivitySummaryColumns.add("stateID");
					nrNeedsActivityWeight = true;
				}
			} else if(geographicOutputDetail == GeographicOutputDetailLevel.COUNTY) {
				masterSelectSQL += ", stateID, countyID, null as zoneID, null as linkID";
				workerSelectSQL += ", stateID, countyID, null as zoneID, null as linkID";
				groupBy += ", stateID, countyID";
				masterInsertSQL += ", stateID, countyID, zoneID, linkID";
				workerInsertSQL += ", stateID, countyID, zoneID, linkID";
				selectActivitySQL += ", stateID, countyID, null as zoneID, null as linkID";
				groupByActivity += ", stateID, countyID";
				groupByActivitySpatialOnly += ", stateID, countyID";
				insertActivitySQL += ", stateID, countyID, zoneID, linkID";

				selectBaseRateOutputSQL += ", 0 as zoneID, 0 as linkID";
				insertBaseRateOutputSQL += ", zoneID, linkID";

				nrActivitySummaryColumns.add("stateID");
				nrActivitySummaryColumns.add("countyID");
			} else if(geographicOutputDetail == GeographicOutputDetailLevel.ZONE) {
				// Zone is the lowest level of geographic detail that can be specified at the
				// Macroscale level. However, at this level there is only one link of a given
				// road type in a given zone. The linkID will be automatically included if and
				// only if the road type is requested below.
				if(scale == null || scale == ModelScale.MACROSCALE) {
					masterSelectSQL += ", stateID, countyID, zoneID";
					workerSelectSQL += ", stateID, countyID, zoneID";
					groupBy += ", stateID, countyID, zoneID";
					masterInsertSQL += ", stateID, countyID, zoneID";
					workerInsertSQL += ", stateID, countyID, zoneID";
					selectActivitySQL += ", stateID, countyID, zoneID";
					groupByActivity += ", stateID, countyID, zoneID";
					groupByActivitySpatialOnly += ", stateID, countyID, zoneID";
					insertActivitySQL += ", stateID, countyID, zoneID";

					selectBaseRateOutputSQL += ", zoneID";
					insertBaseRateOutputSQL += ", zoneID";
					groupByBaseRateOutput += ", zoneID";

					nrActivitySummaryColumns.add("stateID");
					nrActivitySummaryColumns.add("countyID");
					nrActivitySummaryColumns.add("zoneID");
				} else {
					masterSelectSQL += ", stateID, countyID, zoneID, null as linkID";
					workerSelectSQL += ", stateID, countyID, zoneID, null as linkID";
					groupBy += ", stateID, countyID, zoneID";
					masterInsertSQL += ", stateID, countyID, zoneID, linkID";
					workerInsertSQL += ", stateID, countyID, zoneID, linkID";
					selectActivitySQL += ", stateID, countyID, zoneID, null as linkID";
					groupByActivity += ", stateID, countyID, zoneID";
					groupByActivitySpatialOnly += ", stateID, countyID, zoneID";
					insertActivitySQL += ", stateID, countyID, zoneID, linkID";

					selectBaseRateOutputSQL += ", zoneID, 0 as linkID";
					insertBaseRateOutputSQL += ", zoneID, linkID";
					groupByBaseRateOutput += ", zoneID";

					nrActivitySummaryColumns.add("stateID");
					nrActivitySummaryColumns.add("countyID");
					nrActivitySummaryColumns.add("zoneID");
				}
			} else { // Link detail
				if(scale == null || scale == ModelScale.MACROSCALE) {
					if(domain == ModelDomain.PROJECT) {
						masterSelectSQL += ", stateID, countyID, zoneID, linkID";
						workerSelectSQL += ", stateID, countyID, zoneID, linkID";
						groupBy += ", stateID, countyID, zoneID, linkID";
						masterInsertSQL += ", stateID, countyID, zoneID, linkID";
						workerInsertSQL += ", stateID, countyID, zoneID, linkID";
						selectActivitySQL += ", stateID, countyID, zoneID, linkID";
						groupByActivity += ", stateID, countyID, zoneID, linkID";
						groupByActivitySpatialOnly += ", stateID, countyID, zoneID, linkID";
						insertActivitySQL += ", stateID, countyID, zoneID, linkID";

						selectBaseRateOutputSQL += ", zoneID, linkID";
						insertBaseRateOutputSQL += ", zoneID, linkID";
						groupByBaseRateOutput += ", zoneID, linkID";

						nrActivitySummaryColumns.add("stateID");
						nrActivitySummaryColumns.add("countyID");
						nrActivitySummaryColumns.add("zoneID");
						nrActivitySummaryColumns.add("linkID");
					} else {
						// This should not happen
						/**
						 * @issue The Link geographic output detail level is not allowed for Macroscale simulations.
						 * @explain An internal inconsistency within the RunSpec was detected.  Macroscale,
						 * that is simulations involving inventory data, do not support link-level output.
						**/
						Logger.log(LogMessageCategory.ERROR,"The Link geographic output detail "
								+ "level is not allowed for Macroscale simulations.");
						// Provide some default behavior (County-level aggregation)
						masterSelectSQL += ", stateID, countyID, null as zoneID, null as linkID";
						workerSelectSQL += ", stateID, countyID, null as zoneID, null as linkID";
						groupBy += ", stateID, countyID";
						masterInsertSQL += ", stateID, countyID, zoneID, linkID";
						workerInsertSQL += ", stateID, countyID, zoneID, linkID";
						selectActivitySQL += ", stateID, countyID, null as zoneID, null as linkID";
						groupByActivity += ", stateID, countyID";
						groupByActivitySpatialOnly += ", stateID, countyID";
						insertActivitySQL += ", stateID, countyID, zoneID, linkID";

						selectBaseRateOutputSQL += ", zoneID, 0 as linkID";
						insertBaseRateOutputSQL += ", zoneID, linkID";
						groupByBaseRateOutput += ", zoneID";

						nrActivitySummaryColumns.add("stateID");
						nrActivitySummaryColumns.add("countyID");
					}
				} else {
					masterSelectSQL += ", stateID, countyID, zoneID, linkID";
					workerSelectSQL += ", stateID, countyID, zoneID, linkID";
					groupBy += ", stateID, countyID, zoneID, linkID";
					masterInsertSQL += ", stateID, countyID, zoneID, linkID";
					workerInsertSQL += ", stateID, countyID, zoneID, linkID";
					selectActivitySQL += ", stateID, countyID, zoneID, linkID";
					groupByActivity += ", stateID, countyID, zoneID, linkID";
					groupByActivitySpatialOnly += ", stateID, countyID, zoneID, linkID";
					insertActivitySQL += ", stateID, countyID, zoneID, linkID";

					selectBaseRateOutputSQL += ", zoneID, linkID";
					insertBaseRateOutputSQL += ", zoneID, linkID";
					groupByBaseRateOutput += ", zoneID, linkID";

					nrActivitySummaryColumns.add("stateID");
					nrActivitySummaryColumns.add("countyID");
					nrActivitySummaryColumns.add("zoneID");
					nrActivitySummaryColumns.add("linkID");
				}
			}

			// Emission Breakdown selections
			OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
					ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection();
			//System.out.println("outputEmissionsBreakdownSelection.roadType=" + outputEmissionsBreakdownSelection.roadType);

			if(scale == null || scale == ModelScale.MACROSCALE) {
				// Handle Macroscale, roadTypeID, and linkID.  Specifically, at the
				// Macroscale level, knowing roadTypeID and zoneID, implies knowing
				// the linkID.
				if(!outputEmissionsBreakdownSelection.roadType) {
					// Road type is not desired
					if(geographicOutputDetail == GeographicOutputDetailLevel.ZONE) {
						// Road type is not desired but the zone is selected, the linkID is
						// unknown and not already present in the SQL statements
						masterSelectSQL += ", null as linkID, null as roadTypeID";
						workerSelectSQL += ", null as linkID, null as roadTypeID";
						masterInsertSQL += ", linkID, roadTypeID";
						workerInsertSQL += ", linkID, roadTypeID";
						selectActivitySQL += ", null as linkID, null as roadTypeID";
						insertActivitySQL += ", linkID, roadTypeID";

						selectBaseRateOutputSQL += ", 0 as linkID, 0 as roadTypeID";
						insertBaseRateOutputSQL += ", linkID, roadTypeID";
					} else {
						// Road type is not desired and zone is unknown, so linkID
						// is unknown and already present in the SQL statements
						masterSelectSQL += ", null as roadTypeID";
						workerSelectSQL += ", null as roadTypeID";
						masterInsertSQL += ", roadTypeID";
						workerInsertSQL += ", roadTypeID";
						selectActivitySQL += ", null as roadTypeID";
						insertActivitySQL += ", roadTypeID";

						selectBaseRateOutputSQL += ", 0 as roadTypeID";
						insertBaseRateOutputSQL += ", roadTypeID";
					}
				} else {
					// Road type is desired
					if(geographicOutputDetail == GeographicOutputDetailLevel.ZONE) {
						// Road type and zone are known, so linkID is known
						// and not already present in the SQL statements
						masterSelectSQL += ", linkID, roadTypeID";
						workerSelectSQL += ", linkID, roadTypeID";
						groupBy += ", linkID, roadTypeID";
						masterInsertSQL += ", linkID, roadTypeID";
						workerInsertSQL += ", linkID, roadTypeID";
						selectActivitySQL += ", linkID, roadTypeID";
						groupByActivity += ", linkID, roadTypeID";
						groupByActivitySpatialOnly += ", linkID, roadTypeID";
						insertActivitySQL += ", linkID, roadTypeID";

						selectBaseRateOutputSQL += ", linkID, roadTypeID";
						insertBaseRateOutputSQL += ", linkID, roadTypeID";
						groupByBaseRateOutput += ", linkID, roadTypeID";

						nrActivitySummaryColumns.add("linkID");
						//nrActivitySummaryColumns.add("roadTypeID");
					} else {
						// Road type is desired, but the zone is unknown, so linkID is unknown
						// and already present in the SQL statements
						masterSelectSQL += ", roadTypeID";
						workerSelectSQL += ", roadTypeID";
						groupBy += ", roadTypeID";
						masterInsertSQL += ", roadTypeID";
						workerInsertSQL += ", roadTypeID";
						selectActivitySQL += ", roadTypeID";
						groupByActivity += ", roadTypeID";
						groupByActivitySpatialOnly += ", roadTypeID";
						insertActivitySQL += ", roadTypeID";

						selectBaseRateOutputSQL += ", roadTypeID";
						insertBaseRateOutputSQL += ", roadTypeID";
						groupByBaseRateOutput += ", roadTypeID";

						//nrActivitySummaryColumns.add("roadTypeID");
					}
				}
			} else { // All other scales can assume linkID is already in the SQL statements
				masterInsertSQL += ", roadTypeID";
				workerInsertSQL += ", roadTypeID";
				insertActivitySQL += ", roadTypeID";

				if(!outputEmissionsBreakdownSelection.roadType) {
					masterSelectSQL += ", null as roadTypeID";
					workerSelectSQL += ", null as roadTypeID";
					selectActivitySQL += ", null as roadTypeID";

					selectBaseRateOutputSQL += ", 0 as roadTypeID";
					insertBaseRateOutputSQL += ", roadTypeID";
				} else {
					masterSelectSQL += ", roadTypeID";
					workerSelectSQL += ", roadTypeID";
					groupBy += ", roadTypeID";
					selectActivitySQL += ", roadTypeID";
					groupByActivity += ", roadTypeID";
					groupByActivitySpatialOnly += ", roadTypeID";

					selectBaseRateOutputSQL += ", roadTypeID";
					insertBaseRateOutputSQL += ", roadTypeID";
					groupByBaseRateOutput += ", roadTypeID";
				}
			}

			// Process ID
			masterInsertSQL += ", processID";
			workerInsertSQL += ", processID";
			insertBaseRateOutputSQL += ", processID";
			if(!outputEmissionsBreakdownSelection.emissionProcess) {
				masterSelectSQL += ", null as processID";
				workerSelectSQL += ", null as processID";
			} else {
				masterSelectSQL += ", processID";
				workerSelectSQL += ", processID";
				groupBy += ", processID";
			}
			// Base rates always include the process
			selectBaseRateOutputSQL += ", processID";
			groupByBaseRateOutput += ", processID";

			// Fuel Type ID
			masterInsertSQL += ", fuelTypeID";
			workerInsertSQL += ", fuelTypeID";
			insertActivitySQL += ", fuelTypeID";
			insertBaseRateOutputSQL += ", fuelTypeID";
			if(!outputEmissionsBreakdownSelection.fuelType) {
				masterSelectSQL += ", null as fuelTypeID";
				workerSelectSQL += ", null as fuelTypeID";
				selectActivitySQL += ", null as fuelTypeID";
				selectBaseRateOutputSQL += ", 0 as fuelTypeID";

				nrNeedsActivityWeight = true;
			} else {
				masterSelectSQL += ", fuelTypeID";
				workerSelectSQL += ", fuelTypeID";
				groupBy += ", fuelTypeID";
				selectActivitySQL += ", fuelTypeID";
				groupByActivity += ", fuelTypeID";
				groupByActivitySpatialOnly += ", fuelTypeID";
				selectBaseRateOutputSQL += ", fuelTypeID";
				groupByBaseRateOutput += ", fuelTypeID";

				nrActivitySummaryColumns.add("fuelTypeID");
			}

			if(CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT) {
				// Fuel SubType ID
				masterInsertSQL += ", fuelSubTypeID";
				workerInsertSQL += ", fuelSubTypeID";
				insertActivitySQL += ", fuelSubTypeID";
				//insertBaseRateOutputSQL += ", fuelSubTypeID";
				if(!outputEmissionsBreakdownSelection.fuelSubType) {
					masterSelectSQL += ", null as fuelSubTypeID";
					workerSelectSQL += ", null as fuelSubTypeID";
					selectActivitySQL += ", null as fuelSubTypeID";
					//selectBaseRateOutputSQL += ", 0 as fuelSubTypeID";
					// nrNeedsActivityWeight = true; Not needed.
				} else {
					masterSelectSQL += ", fuelSubTypeID";
					workerSelectSQL += ", fuelSubTypeID";
					groupBy += ", fuelSubTypeID";
					selectActivitySQL += ", fuelSubTypeID";
					groupByActivity += ", fuelSubTypeID";
					groupByActivitySpatialOnly += ", fuelSubTypeID";
					//selectBaseRateOutputSQL += ", fuelSubTypeID";
					//groupByBaseRateOutput += ", fuelSubTypeID";
	
					nrActivitySummaryColumns.add("fuelSubTypeID");
				}
			}

			// Model year ID
			masterInsertSQL += ", modelYearID";
			workerInsertSQL += ", modelYearID";
			insertActivitySQL += ", modelYearID";
			insertBaseRateOutputSQL += ", modelYearID";
			if(!outputEmissionsBreakdownSelection.modelYear) {
				masterSelectSQL += ", null as modelYearID";
				workerSelectSQL += ", null as modelYearID";
				selectActivitySQL += ", null as modelYearID";
				selectBaseRateOutputSQL += ", 0 as modelYearID";

				nrNeedsActivityWeight = true;
			} else {
				masterSelectSQL += ", modelYearID";
				workerSelectSQL += ", modelYearID";
				groupBy += ", modelYearID";
				selectActivitySQL += ", modelYearID";
				groupByActivity += ", modelYearID";
				groupByActivitySpatialOnly += ", modelYearID";
				selectBaseRateOutputSQL += ", modelYearID";
				groupByBaseRateOutput += ", modelYearID";

				nrActivitySummaryColumns.add("modelYearID");
			}

			// Source Type ID
			masterInsertSQL += ", sourceTypeID";
			workerInsertSQL += ", sourceTypeID";
			insertActivitySQL += ", sourceTypeID";
			insertBaseRateOutputSQL += ", sourceTypeID";
			if(!outputEmissionsBreakdownSelection.sourceUseType) {
				masterSelectSQL += ", null as sourceTypeID";
				workerSelectSQL += ", null as sourceTypeID";
				selectActivitySQL += ", null as sourceTypeID";
				selectBaseRateOutputSQL += ", 0 as sourceTypeID";
			} else {
				masterSelectSQL += ", sourceTypeID";
				workerSelectSQL += ", sourceTypeID";
				groupBy += ", sourceTypeID";
				selectActivitySQL += ", sourceTypeID";
				groupByActivity += ", sourceTypeID";
				groupByActivitySpatialOnly += ", sourceTypeID";
				selectBaseRateOutputSQL += ", sourceTypeID";
				groupByBaseRateOutput += ", sourceTypeID";
			}

			// Regulatory Class ID
			masterInsertSQL += ", regClassID";
			workerInsertSQL += ", regClassID";
			insertActivitySQL += ", regClassID";
			insertBaseRateOutputSQL += ", regClassID";
			if(!outputEmissionsBreakdownSelection.regClassID) {
				masterSelectSQL += ", null as regClassID";
				workerSelectSQL += ", null as regClassID";
				selectActivitySQL += ", null as regClassID";
			} else {
				masterSelectSQL += ", regClassID";
				workerSelectSQL += ", regClassID";
				groupBy += ", regClassID";
				selectActivitySQL += ", regClassID";
				groupByActivity += ", regClassID";
				groupByActivitySpatialOnly += ", regClassID";
			}
			// Reg. class is not always included in base rates
			if(!outputEmissionsBreakdownSelection.regClassID) {
				selectBaseRateOutputSQL += ", 0 as regClassID";
			} else {
				selectBaseRateOutputSQL += ", regClassID";
				groupByBaseRateOutput += ", regClassID";
			}

			// SCC
			masterInsertSQL += ", SCC";
			workerInsertSQL += ", SCC";
			insertActivitySQL +=", SCC";
			insertBaseRateOutputSQL += ", SCC";
			if(!outputEmissionsBreakdownSelection.onRoadSCC) {
				masterSelectSQL += ", null as SCC";
				workerSelectSQL += ", null as SCC";
				selectActivitySQL += ", null as SCC";
				selectBaseRateOutputSQL += ", '' as SCC";

				nrNeedsActivityWeight = true;
			} else {
				masterSelectSQL += ", SCC";
				workerSelectSQL += ", SCC";
				groupBy += ", SCC";
				selectActivitySQL += ", SCC";
				groupByActivity += ", SCC";
				groupByActivitySpatialOnly += ", SCC";
				selectBaseRateOutputSQL += ", SCC";
				groupByBaseRateOutput += ", SCC";

				nrActivitySummaryColumns.add("SCC");
			}

			// engTechID
			masterInsertSQL += ", engTechID";
			workerInsertSQL += ", engTechID";
			insertActivitySQL +=", engTechID";
			if(!ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)
					|| !outputEmissionsBreakdownSelection.engTechID) {
				masterSelectSQL += ", null as engTechID";
				workerSelectSQL += ", null as engTechID";
				selectActivitySQL += ", null as engTechID";

				nrNeedsActivityWeight = true;
			} else {
				masterSelectSQL += ", engTechID";
				workerSelectSQL += ", engTechID";
				groupBy += ", engTechID";
				selectActivitySQL += ", engTechID";
				groupByActivity += ", engTechID";
				groupByActivitySpatialOnly += ", engTechID";

				nrActivitySummaryColumns.add("engTechID");
			}

			// sectorID
			masterInsertSQL += ", sectorID";
			workerInsertSQL += ", sectorID";
			insertActivitySQL +=", sectorID";
			if(!ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)
					|| !outputEmissionsBreakdownSelection.sector) {
				masterSelectSQL += ", null as sectorID";
				workerSelectSQL += ", null as sectorID";
				selectActivitySQL += ", null as sectorID";

				nrNeedsActivityWeight = true;
			} else {
				masterSelectSQL += ", sectorID";
				workerSelectSQL += ", sectorID";
				groupBy += ", sectorID";
				selectActivitySQL += ", sectorID";
				groupByActivity += ", sectorID";
				groupByActivitySpatialOnly += ", sectorID";

				nrActivitySummaryColumns.add("sectorID");
			}

			// hpID
			masterInsertSQL += ", hpID";
			workerInsertSQL += ", hpID";
			insertActivitySQL +=", hpID";
			if(!ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)
					|| !outputEmissionsBreakdownSelection.hpClass) {
				masterSelectSQL += ", null as hpID";
				workerSelectSQL += ", null as hpID";
				selectActivitySQL += ", null as hpID";

				nrNeedsActivityWeight = true;
			} else {
				masterSelectSQL += ", hpID";
				workerSelectSQL += ", hpID";
				groupBy += ", hpID";
				selectActivitySQL += ", hpID";
				groupByActivity += ", hpID";
				groupByActivitySpatialOnly += ", hpID";

				nrActivitySummaryColumns.add("hpID");
			}

			// Summed emission quantity and associated uncertainty values. If the output time period
			// excludes the day of the week, multiply the day's results by the average number of
			// times that the day occurs in the month to get a total results for the month.
			masterInsertSQL += ", emissionQuant";
			workerInsertSQL += ", emissionQuant, emissionRate";
			insertActivitySQL += ", activity";

			insertBaseRateOutputSQL += ", meanBaseRate, emissionRate";

			selectActivityNoScaleSQL = selectActivitySQL.replace("SELECT MOVESRunID,","SELECT DISTINCT MOVESRunID,") + ", SUM(activity) AS activity";

			if(ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)) {
				// Nonroad runs natively with classical 24-hour days and is restricted
				// to such in the GUI. Therefore, no time aggregation should be done for Nonroad.
				masterSelectSQL += ", SUM(emissionQuant) AS emissionQuant";
				workerSelectSQL += ", SUM(emissionQuant) AS emissionQuant, SUM(emissionRate) as emissionRate";
				selectActivitySQL += ", SUM(activity) AS activity";

				// If outputEmissionsBreakdownSelection.hpClass is off and
				// load factor is not output and retroFrac is not output, then force nrNeedsActivityWeight=false
				// because nothing would be output that needs to be weighted.

				// If things that could need weighting (load factor and retroFrac are always created)
				// must be weighted, then make the SQL for doing so.
				if(nrNeedsActivityWeight) {
					String keyColumnNames = "";
					for(String c : nrActivitySummaryColumns) {
						if(keyColumnNames.length() > 0) {
							keyColumnNames += ",";
						}
						keyColumnNames += c;
					}
					ArrayList<String> detailMatchColumns = new ArrayList<String>(Arrays.asList("SCC","modelYearID",
						"engTechID","sectorID","hpID","fuelTypeID","yearID","monthID","dayID","stateID","countyID"));
					if(CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT && outputEmissionsBreakdownSelection.fuelSubType) {
						detailMatchColumns.add("fuelSubTypeID");
					}
					String updateWhere = "";
					String detailKey = "";
					String detailSelect = "";
					for(int i=0;i<detailMatchColumns.size();i++) {
						String c = detailMatchColumns.get(i);
						if(detailKey.length() > 0) {
							detailKey += ",";
							detailSelect += ",";
							updateWhere += " and ";
						}
						detailKey += c;
						detailSelect += "a." + c;
						updateWhere += "MOVESWorkerActivityOutput." + c + "=nrActivityWeightDetail." + c;
					}

					nrActivityWeightSQL.add("drop table if exists nrActivityWeightSummary;");
					nrActivityWeightSQL.add("drop table if exists nrActivityWeightDetail;");
					nrActivityWeightSQL.add("create table nrActivityWeightSummary like MOVESWorkerActivityOutput;");
					nrActivityWeightSQL.add("alter table nrActivityWeightSummary add primary key (" + keyColumnNames + ");");
					nrActivityWeightSQL.add("insert into nrActivityWeightSummary (" + keyColumnNames + ",activity)"
							+ " select " + keyColumnNames + ",sum(activity) as activity"
							+ " from MOVESWorkerActivityOutput where activityTypeID=2" // weight by source hours (activity type 2)
							+ " group by " + keyColumnNames
							+ " order by null;");
					nrActivityWeightSQL.add("create table nrActivityWeightDetail like MOVESWorkerActivityOutput;");
					nrActivityWeightSQL.add("alter table nrActivityWeightDetail add primary key (" + detailKey + ");");
					nrActivityWeightSQL.add("insert into nrActivityWeightDetail(" + detailKey + ",activity,activityTypeID)"
							+ " select " + detailSelect + ","
							+ " case when s.activity>0 then a.activity/s.activity else 0.0 end as activity,2 as activityTypeID"
							+ " from MOVESWorkerActivityOutput a"
							+ " inner join nrActivityWeightSummary s using (" + keyColumnNames + ")"
							+ " where a.activityTypeID=2;"); // weight by source hours (activity type 2)
					nrActivityWeightSQL.add("update MOVESWorkerActivityOutput, nrActivityWeightDetail set MOVESWorkerActivityOutput.activity=nrActivityWeightDetail.activity*MOVESWorkerActivityOutput.activity"
							+ " where " + updateWhere
							+ " and MOVESWorkerActivityOutput.activityTypeID in (9,10,12);"); // avgHP, retroFrac, LF load factor
					nrActivityWeightSQL.add("drop table if exists nrActivityWeightSummary;");
					nrActivityWeightSQL.add("drop table if exists nrActivityWeightDetail;");
				}
			} else if(convertWeeks && isWorkerSQL) {
				// Only scale to monthly in one place (on the worker)
				// in the calculation pipeline
				WeeksInMonthHelper weekHelper = new WeeksInMonthHelper();
				/*
				String daysPerMonthClause =
						weekHelper.getDaysPerMonthSQLClause("yearID","monthID","dayID");
				masterSelectSQL += ", SUM(emissionQuant*" + daysPerMonthClause
						+ ") AS emissionQuant";
				workerSelectSQL += ", SUM(emissionQuant*" + daysPerMonthClause
						+ ") AS emissionQuant";
				workerSelectSQL += ", SUM(emissionRate*" + daysPerMonthClause
						+ ") AS emissionRate";
				selectActivitySQL += ", SUM(activity*" + daysPerMonthClause
						+ ") AS activity";
				*/
				String weeksPerMonthClause =
						weekHelper.getWeeksPerMonthSQLClause("yearID","monthID");
				masterSelectSQL += ", SUM(emissionQuant*" + weeksPerMonthClause
						+ ") AS emissionQuant";
				workerSelectSQL += ", SUM(emissionQuant*" + weeksPerMonthClause
						+ ") AS emissionQuant";
				workerSelectSQL += ", SUM(emissionRate*" + weeksPerMonthClause
						+ ") AS emissionRate";
				selectActivitySQL += ", SUM(activity*" + weeksPerMonthClause
						+ ") AS activity";
			} else if(convertDays && isWorkerSQL) {
				// Only scale to classical days in one place (on the worker)
				// in the calculation pipeline
				WeeksInMonthHelper weekHelper = new WeeksInMonthHelper();
				String portionOfWeekPerDayClause =
						weekHelper.getPortionOfWeekPerDayClause("dayID");
				masterSelectSQL += ", SUM(emissionQuant*" + portionOfWeekPerDayClause
						+ ") AS emissionQuant";
				workerSelectSQL += ", SUM(emissionQuant*" + portionOfWeekPerDayClause
						+ ") AS emissionQuant";
				workerSelectSQL += ", SUM(emissionRate*" + portionOfWeekPerDayClause
						+ ") AS emissionRate";
				selectActivitySQL += ", SUM(activity*" + portionOfWeekPerDayClause
						+ ") AS activity";
			} else {
				masterSelectSQL += ", SUM(emissionQuant) AS emissionQuant";
				workerSelectSQL += ", SUM(emissionQuant) AS emissionQuant, SUM(emissionRate) as emissionRate";
				selectActivitySQL += ", SUM(activity) AS activity";
			}

			selectBaseRateOutputSQL += ", SUM(meanBaseRate) as meanBaseRate, SUM(emissionRate) as emissionRate";
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while creating SQL statements for
			 * aggregating data.
			**/
			Logger.logError(e, "Failed to create base SQL");
			return false;
		}

		/*
		System .out.println("masterSelectSQL=" + masterSelectSQL);
		System .out.println("workerSelectSQL=" + workerSelectSQL);
		System .out.println("groupBy=" + groupBy);
		System .out.println("selectActivitySQL=" + selectActivitySQL);
		System .out.println("selectActivityNoScaleSQL=" + selectActivityNoScaleSQL);
		System .out.println("groupByActivity=" + groupByActivity);
		System .out.println("groupByActivitySpatialOnly=" + groupByActivitySpatialOnly);
		System .out.println("masterInsertSQL=" + masterInsertSQL);
		System .out.println("workerInsertSQL=" + workerInsertSQL);
		System .out.println("insertActivitySQL=" + insertActivitySQL);
		if(isWorkerSQL) {
			Logger .log(LogMessageCategory.INFO,"nrNeedsActivityWeight="+nrNeedsActivityWeight);
			if(nrNeedsActivityWeight) {
				Logger .log(LogMessageCategory.INFO,"nrActivityWeightSQL=");
				for(String s : nrActivityWeightSQL) {
					Logger.log(LogMessageCategory.INFO,s);
				}
			}
		}
		*/

		return true;
	}

	/**
	 * Generates SQLs to be processed by OutputProcessor periodically while receiving data
	 * from workers.
	 * @return false if there is any problem while generating SQLs.
	**/
	boolean generateSQLsForOutputProcessor() {
		if(!generateBaseSQLForAggregation(false)) {
			return false;
		}
		if(!ENABLE_NONROAD_AGGREGATION
				&& ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)) {
			// Temporarily disable all aggregation for Nonroad runs.
			return true;
		}
		try {
			outputProcessorSQLs.add("DROP TABLE IF EXISTS WorkerOutputTemp");
			outputProcessorSQLs.add(createSQL);
			outputProcessorSQLs.add(masterInsertSQL + ") " + masterSelectSQL
					+ " FROM " + ExecutionRunSpec.getEmissionOutputTable()
					+ " WHERE MOVESRunID = " + activeRunID + " AND iterationID = "
					+ activeIterationID + " " + groupBy);
			outputProcessorSQLs.add("OPTIMIZE TABLE " + ExecutionRunSpec.getEmissionOutputTable());
			outputProcessorSQLs.add("DELETE FROM " + ExecutionRunSpec.getEmissionOutputTable()
					+ " WHERE MOVESRunID = " + activeRunID
					+ " AND iterationID = " + activeIterationID);
			String insertOutputSQL = "INSERT INTO " + ExecutionRunSpec.getEmissionOutputTable()
					+ "(" + masterOutputTableFields + ") "
					+ selectSQLForMasterOutput + " FROM WorkerOutputTemp";
			outputProcessorSQLs.add(insertOutputSQL);
			outputProcessorSQLs.add("DROP TABLE IF EXISTS WorkerOutputTemp");
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while creating SQL statements for
			 * aggregating data.
			**/
			Logger.logError(e, "Failed to generate SQLs for output processor");
			return false;
		}
		try {
			outputProcessorSQLs.add("DROP TABLE IF EXISTS WorkerActivityOutputTemp");
			outputProcessorSQLs.add(createActivitySQL);

			if(ExecutionRunSpec.getRunSpec().outputPopulation) {
				outputProcessorSQLs.add(insertActivitySQL + ") " + selectActivitySQL
						+ " FROM " + ExecutionRunSpec.getActivityOutputTable()
						+ " WHERE MOVESRunID = " + activeRunID + " AND iterationID = "
						+ activeIterationID + " AND activityTypeID <> 6 " + groupByActivity);
				outputProcessorSQLs.add(insertActivitySQL + ") " + selectActivityNoScaleSQL
						+ " FROM " + ExecutionRunSpec.getActivityOutputTable()
						+ " WHERE MOVESRunID = " + activeRunID + " AND iterationID = "
						+ activeIterationID + " AND activityTypeID = 6 " + groupByActivitySpatialOnly);
			} else {
				outputProcessorSQLs.add(insertActivitySQL + ") " + selectActivitySQL
						+ " FROM " + ExecutionRunSpec.getActivityOutputTable()
						+ " WHERE MOVESRunID = " + activeRunID + " AND iterationID = "
						+ activeIterationID + " " + groupByActivity);
			}

			outputProcessorSQLs.add("DELETE FROM " + ExecutionRunSpec.getActivityOutputTable()
					+ " WHERE MOVESRunID = " + activeRunID
					+ " AND iterationID = " + activeIterationID);
			String insertActivityOutputSQL = "INSERT INTO "
					+ ExecutionRunSpec.getActivityOutputTable() + " ("
					+ outputActivityTableFields + ") "
					+ selectSQLForActivityOutput + " FROM WorkerActivityOutputTemp";
			outputProcessorSQLs.add(insertActivityOutputSQL);
			outputProcessorSQLs.add("DROP TABLE IF EXISTS WorkerActivityOutputTemp");
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while creating SQL statements for
			 * aggregating data.
			**/
			Logger.logError(e, "Failed to generate SQLs for output processor");
			return false;
		}

		// Aggregation of BaseRateOutput is not possible without supporting
		// population and activity data.
		if(false && CompilationFlags.DO_RATES_FIRST) {
//		if(CompilationFlags.DO_RATES_FIRST) {
			try {
				outputProcessorSQLs.add("DROP TABLE IF EXISTS WorkerBaseRateOutputTemp");
				outputProcessorSQLs.add(createBaseRateOutputSQL);
				outputProcessorSQLs.add(insertBaseRateOutputSQL + ") " + selectBaseRateOutputSQL
						+ " FROM BaseRateOutput"
						+ " WHERE MOVESRunID = " + activeRunID + " AND iterationID = "
						+ activeIterationID + " " + groupByBaseRateOutput);
				outputProcessorSQLs.add("DELETE FROM BaseRateOutput"
						+ " WHERE MOVESRunID = " + activeRunID
						+ " AND iterationID = " + activeIterationID);
				outputProcessorSQLs.add("INSERT INTO BaseRateOutput ("
						+ outputBaseRateOutputTableFields + ") "
						+ selectSQLForBaseRateOutput + " FROM WorkerBaseRateOutputTemp");
				outputProcessorSQLs.add("DROP TABLE IF EXISTS WorkerBaseRateOutputTemp");
			} catch(Exception e) {
				/**
				 * @explain A database error occurred while creating SQL statements for
				 * aggregating data.
				**/
				Logger.logError(e, "Failed to generate SQLs for output processor");
				return false;
			}
		}

		return true;
	}

	/**
	 * Generates SQLs to be processed by output processor, after all other processing has been
	 * completed but before unit conversions have been done.
	 * @return false if there is any problem while generating SQLs.
	**/
	boolean generateSQLsForFinalProcessing() {
		if(!generateBaseSQLForAggregation(false)) {
			return false;
		}
		if(!ENABLE_NONROAD_AGGREGATION
				&& ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD)) {
			// Temporarily disable all aggregation for Nonroad runs.
			return true;
		}
		try {
			finalProcessSQLs.add("DROP TABLE IF EXISTS WorkerOutputTemp");
			finalProcessSQLs.add(createSQL);
			finalProcessSQLs.add(masterInsertSQL + ") " + masterSelectSQL
					+ " FROM " + ExecutionRunSpec.getEmissionOutputTable()
					+ " WHERE MOVESRunID = " + activeRunID + " AND iterationID = "
					+ activeIterationID + " " + groupBy);
			finalProcessSQLs.add("DELETE FROM " + ExecutionRunSpec.getEmissionOutputTable()
					+ " WHERE MOVESRunID = " + activeRunID
					+ " AND iterationID = " + activeIterationID);
			String insertOutputSQL = "INSERT INTO " + ExecutionRunSpec.getEmissionOutputTable()
					+ "(" + masterOutputTableFields + ") "
					+ selectSQLForMasterOutput + " FROM WorkerOutputTemp";
			finalProcessSQLs.add(insertOutputSQL);
			finalProcessSQLs.add("DROP TABLE IF EXISTS WorkerOutputTemp");
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while creating SQL statements for
			 * aggregating data.
			**/
			Logger.logError(e, "Failed to generate SQLs for final processing");
			return false;
		}
		
		try {
			if(ExecutionRunSpec.theExecutionRunSpec.getModels().contains(Model.NONROAD) 
				&& ExecutionRunSpec.theExecutionRunSpec.getGeographicOutputDetailLevel() == GeographicOutputDetailLevel.STATE) {
				// Weight LF and avgHP during final aggregation if output is reported at state level (because all NR workers return county level data).
				// This is the same weighting code as run on the worker side, with the addition of the MOVESRunID and iterationID columns.
				// (Note: countyID has not been aggregated over by the worker in this specific case--it is aggregated over in the next try block)
				String keyColumnNames = "MOVESRunID,iterationID";
				for(String c : nrActivitySummaryColumns) {
					keyColumnNames += "," + c;
				}
				String[] detailMatchColumns = {
					"SCC","modelYearID","engTechID","sectorID","hpID","fuelTypeID",
					"yearID","monthID","dayID","stateID","countyID","MOVESRunID","iterationID"
				};
				String updateWhere = "";
				String detailKey = "";
				String detailSelect = "";
				for(int i=0;i<detailMatchColumns.length;i++) {
					String c = detailMatchColumns[i];
					if(detailKey.length() > 0) {
						detailKey += ",";
						detailSelect += ",";
						updateWhere += " and ";
					}
					detailKey += c;
					detailSelect += "a." + c;
					updateWhere += "a." + c + "=nrActivityWeightDetail." + c;
				}

				finalProcessSQLs.add("drop table if exists nrActivityWeightSummary;");
				finalProcessSQLs.add("drop table if exists nrActivityWeightDetail;");
				finalProcessSQLs.add("create table nrActivityWeightSummary like " + ExecutionRunSpec.getActivityOutputTable() + ";");
				finalProcessSQLs.add("alter table nrActivityWeightSummary add primary key (" + keyColumnNames + ");");
				finalProcessSQLs.add("insert into nrActivityWeightSummary (" + keyColumnNames + ",activity)"
						+ " select " + keyColumnNames + ",sum(activity) as activity"
						+ " from " + ExecutionRunSpec.getActivityOutputTable() + " where activityTypeID=2" // weight by source hours (activity type 2)
						+ " and MOVESRunID = "+ activeRunID
						+ " and iterationID = " + activeIterationID
						+ " group by " + keyColumnNames
						+ " order by null;");
				finalProcessSQLs.add("create table nrActivityWeightDetail like " + ExecutionRunSpec.getActivityOutputTable() + ";");
				finalProcessSQLs.add("alter table nrActivityWeightDetail add primary key (" + detailKey + ");");
				finalProcessSQLs.add("insert into nrActivityWeightDetail(" + detailKey + ",activity,activityTypeID)"
						+ " select " + detailSelect + ","
						+ " case when s.activity>0 then a.activity/s.activity else 0.0 end as activity,2 as activityTypeID"
						+ " from " + ExecutionRunSpec.getActivityOutputTable() + " a"
						+ " inner join nrActivityWeightSummary s using (" + keyColumnNames + ")"
						+ " where a.activityTypeID=2" // weight by source hours (activity type 2)
						+ " and MOVESRunID = "+ activeRunID
						+ " and iterationID = " + activeIterationID + ";");
				finalProcessSQLs.add("update " + ExecutionRunSpec.getActivityOutputTable() + " a, nrActivityWeightDetail set a.activity=nrActivityWeightDetail.activity*a.activity"
						+ " where " + updateWhere
						+ " and a.activityTypeID in (9,10,12);"); // avgHP, retroFrac, LF load factor
				finalProcessSQLs.add("drop table if exists nrActivityWeightSummary;");
				finalProcessSQLs.add("drop table if exists nrActivityWeightDetail;");
			}
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while creating SQL statements for
			 * aggregating data.
			**/
			Logger.logError(e, "Failed to generate SQLs for final processing");
			return false;
		}
		
		try {
			finalProcessSQLs.add("DROP TABLE IF EXISTS WorkerActivityOutputTemp");
			finalProcessSQLs.add(createActivitySQL);

			if(ExecutionRunSpec.getRunSpec().outputPopulation) {
				finalProcessSQLs.add(insertActivitySQL + ") " + selectActivitySQL
						+ " FROM " + ExecutionRunSpec.getActivityOutputTable()
						+ " WHERE MOVESRunID = " + activeRunID
						+ " AND iterationID = " + activeIterationID
						+ " AND activityTypeID <> 6 " + groupByActivity);
				finalProcessSQLs.add(insertActivitySQL + ") " + selectActivityNoScaleSQL
						+ " FROM " + ExecutionRunSpec.getActivityOutputTable()
						+ " WHERE MOVESRunID = " + activeRunID
						+ " AND iterationID = " + activeIterationID
						+ " AND activityTypeID = 6 " + groupByActivitySpatialOnly);
			} else {
				finalProcessSQLs.add(insertActivitySQL + ") " + selectActivitySQL
						+ " FROM " + ExecutionRunSpec.getActivityOutputTable()
						+ " WHERE MOVESRunID = " + activeRunID
						+ " AND iterationID = " + activeIterationID + " " + groupByActivity);
			}

			finalProcessSQLs.add("DELETE FROM " + ExecutionRunSpec.getActivityOutputTable()
					+ " WHERE MOVESRunID = " + activeRunID
					+ " AND iterationID = " + activeIterationID);
			String insertActivityOutputSQL = "INSERT INTO "
					+ ExecutionRunSpec.getActivityOutputTable() + " ("
					+ outputActivityTableFields + ") " + selectSQLForActivityOutput
					+ " FROM WorkerActivityOutputTemp";
			finalProcessSQLs.add(insertActivityOutputSQL);
			finalProcessSQLs.add("DROP TABLE IF EXISTS WorkerActivityOutputTemp");
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while creating SQL statements for
			 * aggregating data.
			**/
			Logger.logError(e, "Failed to generate SQLs for final processing");
			return false;
		}

		// Aggregation of BaseRateOutput is not possible without supporting
		// population and activity data.
		if(false && CompilationFlags.DO_RATES_FIRST) {
			try {
				finalProcessSQLs.add("DROP TABLE IF EXISTS WorkerBaseRateOutputTemp");
				finalProcessSQLs.add(createBaseRateOutputSQL);
				finalProcessSQLs.add(insertBaseRateOutputSQL + ") " + selectBaseRateOutputSQL
						+ " FROM BaseRateOutput"
						+ " WHERE MOVESRunID = " + activeRunID
						+ " AND iterationID = " + activeIterationID + " " + groupByBaseRateOutput);
				finalProcessSQLs.add("DELETE FROM BaseRateOutput"
						+ " WHERE MOVESRunID = " + activeRunID
						+ " AND iterationID = " + activeIterationID);
				finalProcessSQLs.add("INSERT INTO BaseRateOutput ("
						+ outputBaseRateOutputTableFields + ") " + selectSQLForBaseRateOutput
						+ " FROM WorkerBaseRateOutputTemp");
				finalProcessSQLs.add("DROP TABLE IF EXISTS WorkerBaseRateOutputTemp");
			} catch(Exception e) {
				/**
				 * @explain A database error occurred while creating SQL statements for
				 * aggregating data.
				**/
				Logger.logError(e, "Failed to generate SQLs for final processing");
				return false;
			}
		}

		return true;
	}
}
