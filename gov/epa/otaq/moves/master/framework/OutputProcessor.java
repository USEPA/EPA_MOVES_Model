/**************************************************************************************************
 * @(#)OutputProcessor.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.RunSpec;
import java.io.File;
import java.sql.Connection;
import java.util.*;
import java.sql.*;
import java.io.*;

import org.apache.commons.lang.math.NumberUtils;

/**
 * This Singleton updates and appends output database with results from the distributed
 * workers. Called by the unbundler.
 *
 * @author		Wesley Faler
 * @version		2016-08-30
**/
public class OutputProcessor {
	// true to retain bundles from workers
	static final boolean keepDebugData = false;

	/** File containing the table data that was output by a worker **/
	static final String OUTPUT_TABLE_FILE_NAME = "Output.tbl";

	/** File containing the activity table data that was output by a worker **/
	static final String ACTIVITY_TABLE_FILE_NAME = "Activity.tbl";

	/** File containing text error message reported by a worker **/
	static final String ERROR_FILE_NAME = "Errors.txt";
	
	/** File containing text error message reported by a worker **/
	static final String NR_ERROR_FILE_NAME = "nrerrors.txt";
	
	/** File containing text error message reported by a worker **/
	static final String NR_WARNING_FILE_NAME = "nrwarnings.txt";

	/** File containing worker version information **/
	public static final String VERSION_FILE_NAME = "WorkerVersion.txt";

	/** Flag indicating if uncertainty is being estimated. **/
	boolean estimateUncertainty = false;

	/** The singleton instance **/
	private static OutputProcessor theOutputProcessor = new OutputProcessor();

	/** List of IntegratedPostProcessor objects that modify data added to the Output database. **/
	public LinkedList<IntegratedPostProcessor> integratedPostProcessors =
			new LinkedList<IntegratedPostProcessor>();

	/**
	 * Access method to the singleton instance.
	 * @return The singleton OutputProcessor object.
	**/
	public static OutputProcessor getTheOutputProcessor() {
		return theOutputProcessor;
	}

	/** Default constructor **/
	public OutputProcessor() {
	}

	/** Clear the list of post processors in preparation for a new simulation run **/
	public void resetPostProcessors() {
		integratedPostProcessors = new LinkedList<IntegratedPostProcessor>();
	}

	/**
	 * Process a completed work bundle received from a distributed worker. This
	 * is called by the unbundler.
	 * @param folder The folder that the completed work bundle was unpacked to.
	 * @param filePaths A list of File objects referencing the contents of a completed work
	 * bundle from the distributed processing system.
	**/
	public void processWorkerFiles(File folder, LinkedList<File> filePaths) {
		Logger.log(LogMessageCategory.INFO, "Received bundle from worker");

		Connection executionDatabase = null;
		Connection outputDatabase = null;
		BundleManifest manifest = new BundleManifest();
		try {
			if(!manifest.containsManifest(folder) || !manifest.readFromFolder(folder)) {
				manifest.tablesToRetrieve.add("MOVESWorkerOutput");
				manifest.tablesToRetrieve.add("MOVESWorkerActivityOutput");
				if(CompilationFlags.DO_RATES_FIRST) {
					manifest.tablesToRetrieve.add("BaseRateOutput");
				}
			}
			executionDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION);
			outputDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.OUTPUT);

			String workerVersion = "";
			String workerComputerID = "";
			String workerID = "";

			File versionFile = new File(folder, VERSION_FILE_NAME);
			if(versionFile.isFile()) {
				ArrayList<String> lines = null;
				try {
					lines = FileUtilities.readLines(versionFile);
				} catch (Exception exception) {
					/**
					 * @explain A bundle of completed data from a worker did not include
					 * the required internal file giving the worker's version information.
					 * The worker is likely out of date and should be upgraded.
					**/
					Logger.log(LogMessageCategory.ERROR, "Failed to read worker version file.");
				}
				if(lines != null) {
					for(Iterator<String> i=lines.iterator();i.hasNext();) {
						String line = (String)i.next();
						line = line.trim();
						int index = line.indexOf('=');
						if(index <= 0) {
							continue;
						}
						String name = line.substring(0,index).trim();
						String value = line.substring(index+1).trim();
						if(name.equalsIgnoreCase("WorkerVersion")) {
							if(value.length() > 100) {
								value = value.substring(0,100).trim();
							}
							workerVersion = DatabaseUtilities.escapeSQL(value,true);
						} else if(name.equalsIgnoreCase("WorkerComputerID")) {
							if(value.length() > 255) {
								value = value.substring(0,255).trim();
							}
							workerComputerID = DatabaseUtilities.escapeSQL(value,true);
						} else if(name.equalsIgnoreCase("WorkerID")) {
							if(value.length() > 255) {
								value = value.substring(0,255).trim();
							}
							workerID = DatabaseUtilities.escapeSQL(value,true);
						}
					}
					Logger.log(LogMessageCategory.INFO, "Bundle " + manifest.bundleNumber + " is from worker: "
							+ workerComputerID + "/" + workerVersion + "/" + workerID);
					if(workerVersion.length() > 0 && workerComputerID.length() > 0
							&& workerID.length() > 0) {
						String sql = "";
						try {
							// INSERT IGNORE into MOVESWorkersUsed to establish the record
							sql = "insert ignore into MOVESWorkersUsed (MOVESRunID,"
									+ " workerVersion, workerComputerID, workerID,"
									+ " bundleCount, failedBundleCount)"
									+ " values (" + MOVESEngine.theInstance.getActiveRunID()
									+ "," + workerVersion + "," + workerComputerID
									+ "," + workerID + ",0,0)";
							SQLRunner.executeSQL(outputDatabase, sql);

							// Update MOVESWorkersUsed.bundleCount
							sql = "update MOVESWorkersUsed set bundleCount=bundleCount+1"
									+ " where MOVESRunID="
									+ MOVESEngine.theInstance.getActiveRunID()
									+ " and workerVersion=" + workerVersion
									+ " and workerComputerID=" + workerComputerID
									+ " and workerID=" + workerID;
							SQLRunner.executeSQL(outputDatabase, sql);
						} catch(Exception e) {
							Logger.logSqlError(e,"Unable to update MOVESWorkersUsed",sql);
						}
						manifest.recordEvent(outputDatabase,false,MOVESEngine.theInstance.getActiveRunID());
					}
				}
			}

			if(workerVersion == null || workerVersion.length() <= 0) {
				workerVersion = StringUtilities.safeGetString(manifest.workerVersion).trim();
				if(workerVersion.length() > 100) {
					workerVersion = workerVersion.substring(0,100).trim();
				}
			}
			if(workerComputerID == null || workerComputerID.length() <= 0) {
				workerComputerID = StringUtilities.safeGetString(manifest.workerComputerID).trim();
				if(workerComputerID.length() > 255) {
					workerComputerID = workerComputerID.substring(0,255).trim();
				}
			}
			if(workerID == null || workerID.length() <= 0) {
				workerID = StringUtilities.safeGetString(manifest.workerID).trim();
				if(workerID.length() > 255) {
					workerID = workerID.substring(0,255).trim();
				}
			}
			
			// Process NR Errors and Warnings
			File nrErrorFile = new File(folder, NR_ERROR_FILE_NAME);
			if(nrErrorFile.isFile()) {
				String allNrErrors = FileUtilities.readEntireFile(nrErrorFile);
				for(String nrError : allNrErrors.split("\n")) {
					MOVESEngine.theInstance.logRunError(
							getValue(manifest.context, "run"), 
							0, 
							getValue(manifest.context, "proc"), 
							getValue(manifest.context, "st"), 
							getValue(manifest.context, "cty"), 
							getValue(manifest.context, "zone"), 
							getValue(manifest.context, "link"), 
							getValue(manifest.context, "y"), 
							getValue(manifest.context, "m"), 
							getValue(manifest.context, "d"), 
							getValue(manifest.context, "h"), 
							nrError);
				}
			}
			File nrWarningFile = new File(folder, NR_WARNING_FILE_NAME);
			if(nrWarningFile.isFile()) {
				String allNrWarnings = FileUtilities.readEntireFile(nrWarningFile);
				for(String nrWarning : allNrWarnings.split("\n")) {
					MOVESEngine.theInstance.logRunError(
							getValue(manifest.context, "run"), 
							0, 
							getValue(manifest.context, "proc"), 
							getValue(manifest.context, "st"), 
							getValue(manifest.context, "cty"), 
							getValue(manifest.context, "zone"), 
							getValue(manifest.context, "link"), 
							getValue(manifest.context, "y"), 
							getValue(manifest.context, "m"), 
							getValue(manifest.context, "d"), 
							getValue(manifest.context, "h"), 
							nrWarning);
				}
			}
			
			// Process other errors
			File errorFile = new File(folder, ERROR_FILE_NAME);
			if(errorFile.isFile()) {
				String errorMessage;
				try {
					errorMessage = "Bundle " + manifest.bundleNumber
						+ " from worker " + workerComputerID + "/" + workerVersion + "/" + workerID + " has errors: "
						+ FileUtilities.readEntireFile(errorFile);
				} catch (Exception exception) {
					errorMessage = "Worker reported errors. Failed to read file.";
				}
				/** @nonissue **/
				Logger.log(LogMessageCategory.ERROR, errorMessage);

				// Update MOVESWorkersUsed.failedBundleCount
				if(workerVersion.length() > 0 && workerComputerID.length() > 0
						&& workerID.length() > 0) {
					String sql = "";
					try {
						sql = "update MOVESWorkersUsed set failedBundleCount=failedBundleCount+1"
								+ " where MOVESRunID="
								+ MOVESEngine.theInstance.getActiveRunID()
								+ " and workerVersion=" + workerVersion
								+ " and workerComputerID=" + workerComputerID
								+ " and workerID=" + workerID;
						SQLRunner.executeSQL(outputDatabase, sql);
					} catch(Exception e) {
						Logger.logSqlError(e,"Unable to update MOVESWorkersUsed",sql);
					}
				}

				return;
			}

			String sql;
			boolean hasCalculatorOutputTables = false;

			sql = "update MOVESRun set retrievedDONEFiles=retrievedDONEFiles+1 where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
			SQLRunner.executeSQL(outputDatabase, sql);

			// SELECT * can't be used since it would export MOVESOutputRowID. This
			// AUTO_INCREMENT field can't be exported across databases.
			String outputTableFields = "MOVESRunID,"
					+"yearID,"
					+"monthID,"
					+"dayID,"
					+"hourID,"
					+"stateID,"
					+"countyID,"
					+"zoneID,"
					+"linkID,"
					+"pollutantID,"
					+"processID,"
					+"sourceTypeID,"
					+"regClassID,"
					+"fuelTypeID,"
					+"fuelSubTypeID,"
					+"modelYearID,"
					+"roadTypeID,"
					+"SCC,"
					+"engTechID,"
					+"sectorID,"
					+"hpID,"
					+"emissionQuant";

			String outputTableFieldsWithFuelSubType = "MOVESRunID,"
					+"yearID,"
					+"monthID,"
					+"dayID,"
					+"hourID,"
					+"stateID,"
					+"countyID,"
					+"zoneID,"
					+"linkID,"
					+"pollutantID,"
					+"processID,"
					+"sourceTypeID,"
					+"regClassID,"
					+"fuelTypeID,"
					+"fuelSubTypeID,"
					+"modelYearID,"
					+"roadTypeID,"
					+"SCC,"
					+"engTechID,"
					+"sectorID,"
					+"hpID,"
					+"emissionQuant";

			String outputTableFieldsNoFuelSubType = "MOVESRunID,"
					+"yearID,"
					+"monthID,"
					+"dayID,"
					+"hourID,"
					+"stateID,"
					+"countyID,"
					+"zoneID,"
					+"linkID,"
					+"pollutantID,"
					+"processID,"
					+"sourceTypeID,"
					+"regClassID,"
					+"fuelTypeID,"
					+"modelYearID,"
					+"roadTypeID,"
					+"SCC,"
					+"engTechID,"
					+"sectorID,"
					+"hpID,"
					+"emissionQuant";

			// SELECT * can't be used since it would export MOVESActivityOutputRowID. This
			// AUTO_INCREMENT field can't be exported across databases.
			String outputActivityTableFields = "MOVESRunID,"
					+"yearID,"
					+"monthID,"
					+"dayID,"
					+"hourID,"
					+"stateID,"
					+"countyID,"
					+"zoneID,"
					+"linkID,"
					+"sourceTypeID,"
					+"regClassID,"
					+"fuelTypeID,"
					+"fuelSubTypeID,"
					+"modelYearID,"
					+"roadTypeID,"
					+"SCC,"
					+"engTechID,"
					+"sectorID,"
					+"hpID,"
					+"activityTypeID,"
					+"activity";

			String outputActivityTableFieldsWithFuelSubType = "MOVESRunID,"
					+"yearID,"
					+"monthID,"
					+"dayID,"
					+"hourID,"
					+"stateID,"
					+"countyID,"
					+"zoneID,"
					+"linkID,"
					+"sourceTypeID,"
					+"regClassID,"
					+"fuelTypeID,"
					+"fuelSubTypeID,"
					+"modelYearID,"
					+"roadTypeID,"
					+"SCC,"
					+"engTechID,"
					+"sectorID,"
					+"hpID,"
					+"activityTypeID,"
					+"activity";

			String outputActivityTableFieldsNoFuelSubType = "MOVESRunID,"
					+"yearID,"
					+"monthID,"
					+"dayID,"
					+"hourID,"
					+"stateID,"
					+"countyID,"
					+"zoneID,"
					+"linkID,"
					+"sourceTypeID,"
					+"regClassID,"
					+"fuelTypeID,"
					+"modelYearID,"
					+"roadTypeID,"
					+"SCC,"
					+"engTechID,"
					+"sectorID,"
					+"hpID,"
					+"activityTypeID,"
					+"activity";

			for(Iterator ti=manifest.tablesToRetrieve.iterator();ti.hasNext();) {
				String tableName = (String)ti.next();

				if(tableName.equalsIgnoreCase("MOVESWorkerOutput")) {
					hasCalculatorOutputTables = true;
					File outputTableFile = new File(folder, OUTPUT_TABLE_FILE_NAME);
					if(!outputTableFile.isFile()) {
						/**
						 * @explain A bundle of completed data from a worker did not include
						 * a required data file.  The simulation results should be discarded.
						**/
						Logger.log(LogMessageCategory.ERROR,
								"Didn't get output data from distributed bundle");
						return;
					}

					// Create a temporary table to hold the worker's results
					// (only if the table doesn't already exist)
					sql = "CREATE TABLE IF NOT EXISTS TemporaryOutputImport "
							+ "SELECT * FROM " + ExecutionRunSpec.getEmissionOutputTable() + " LIMIT 0";
					SQLRunner.executeSQL(outputDatabase, sql);
					if(!CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT) {
						try {
							SQLRunner.executeSQL(outputDatabase, "alter table TemporaryOutputImport add fuelSubTypeID SMALLINT UNSIGNED NULL DEFAULT NULL");
						} catch(Exception e) {
							// Nothing to do here. This may happen if the fuelSubTypeID column already exists.
						}
					}
					// Clear temporary output table.
					sql = "TRUNCATE TemporaryOutputImport";
					SQLRunner.executeSQL(outputDatabase, sql);

					// Import data file
					sql = "LOAD DATA INFILE "
							+ DatabaseUtilities.escapeSQL(outputTableFile.getCanonicalPath())
							+ " INTO TABLE TemporaryOutputImport " + "(" + outputTableFields + ")";
					SQLRunner.executeSQL(outputDatabase, sql);

					// Use the Run ID of the current run.
					sql = "UPDATE TemporaryOutputImport SET MOVESRunID = "
							+ MOVESEngine.theInstance.getActiveRunID();
					SQLRunner.executeSQL(outputDatabase, sql);

					// Use the current iterationID.
					if(estimateUncertainty) {
						sql = "UPDATE TemporaryOutputImport SET iterationID = "
								+ MOVESEngine.theInstance.getActiveIterationID();
						SQLRunner.executeSQL(outputDatabase, sql);
					}
				} else if(tableName.equalsIgnoreCase("MOVESWorkerActivityOutput")) {
					hasCalculatorOutputTables = true;
					File outputActivityTableFile = new File(folder, ACTIVITY_TABLE_FILE_NAME);
					if(!outputActivityTableFile.isFile()) {
						/**
						 * @explain A bundle of completed data from a worker did not include
						 * a required data file.  The simulation results should be discarded.
						**/
						Logger.log(LogMessageCategory.ERROR,
								"Didn't get output data from distributed bundle");
						return;
					}

					// Drop the temporary activity table so we can be certain its indexing is correct
					sql = "DROP TABLE IF EXISTS TemporaryActivityOutputImport";
					SQLRunner.executeSQL(outputDatabase, sql);

					// Create a temporary table to hold the activity results
					// (only if the table doesn't already exist)
					sql = "CREATE TABLE IF NOT EXISTS TemporaryActivityOutputImport "
							+ "SELECT * FROM " + ExecutionRunSpec.getActivityOutputTable() + " LIMIT 0";
					SQLRunner.executeSQL(outputDatabase, sql);
					if(!CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT) {
						try {
							SQLRunner.executeSQL(outputDatabase, "alter table TemporaryActivityOutputImport add fuelSubTypeID SMALLINT UNSIGNED NULL DEFAULT NULL");
						} catch(Exception e) {
							// Nothing to do here. This may happen if the fuelSubTypeID column already exists.
						}
					}
					// Clear temporary output table.
					sql = "TRUNCATE TemporaryActivityOutputImport";
					SQLRunner.executeSQL(outputDatabase, sql);

					// Import data file
					sql = "LOAD DATA INFILE "
							+ DatabaseUtilities.escapeSQL(outputActivityTableFile.getCanonicalPath())
							+ " INTO TABLE TemporaryActivityOutputImport " + "("
							+ outputActivityTableFields + ")";
					SQLRunner.executeSQL(outputDatabase, sql);

					// Use the Run ID of the current run.
					sql = "UPDATE TemporaryActivityOutputImport SET MOVESRunID = "
							+ MOVESEngine.theInstance.getActiveRunID();
					SQLRunner.executeSQL(outputDatabase, sql);

					// Use the current iterationID.
					if(estimateUncertainty) {
						sql = "UPDATE TemporaryActivityOutputImport SET iterationID = "
								+ MOVESEngine.theInstance.getActiveIterationID();
						SQLRunner.executeSQL(outputDatabase, sql);
					}
				} else {
					Connection cmitDB = executionDatabase;
					boolean isOutputDatabase = false;
					if(tableName.equalsIgnoreCase("BaseRateOutput")) {
						cmitDB = outputDatabase;
						isOutputDatabase = true;
					}
					// The table is a CMIT table and should be read into a temporary table
					// then INSERT IGNORE'd into the primary CMIT table
					String tempTableName = "temp" + tableName;
					File dataFile = new File(folder,tableName + ".tbl");
					if(dataFile.exists()) {
						// Create a temporary table to handle the loaded data
						sql = "create table if not exists " + tempTableName + " like " + tableName;
						SQLRunner.executeSQL(cmitDB,sql);
						// Ensure the temporary table is empty
						sql = "truncate table " + tempTableName;
						SQLRunner.executeSQL(cmitDB,sql);
						// Import data into the temporary table
						sql = "LOAD DATA INFILE "
								+ DatabaseUtilities.escapeSQL(dataFile.getCanonicalPath())
								+ " INTO TABLE " + tempTableName;
						SQLRunner.executeSQL(cmitDB, sql);

						if(CompilationFlags.DO_RATES_FIRST) {
							if(isOutputDatabase) {
								try {
									// Use the Run ID of the current run.
									sql = "UPDATE " + tempTableName + " SET MOVESRunID = " + MOVESEngine.theInstance.getActiveRunID();
									SQLRunner.executeSQL(cmitDB, sql);

									// Use the current iterationID.
									if(estimateUncertainty) {
										sql = "UPDATE " + tempTableName + " SET iterationID = " + MOVESEngine.theInstance.getActiveIterationID();
										SQLRunner.executeSQL(cmitDB, sql);
									}
								} catch(Exception e) {
									// Nothing to do here because the table simply may not contain MOVESRunID or iterationID columns.
								}
							}
						}

						// INSERT IGNORE the data into the primary table
						sql = "insert ignore into " + tableName
								+ " select * from " + tempTableName;
						SQLRunner.executeSQL(cmitDB, sql);
						// Get rid of the temporary table
						sql = "drop table if exists " + tempTableName;
						SQLRunner.executeSQL(cmitDB,sql);
					} else {
						Logger.log(LogMessageCategory.ERROR,"TBL file not sent from the worker for table " + tableName);
					}
				}
			}

			if(hasCalculatorOutputTables) {
				try {
					// Invoke IntegratedPostProcessor objects
					for (Iterator i = integratedPostProcessors.iterator(); i.hasNext(); ) {
						IntegratedPostProcessor iterProcessor = (IntegratedPostProcessor) i.next();

						iterProcessor.execute(outputDatabase);
					}

					// Move data into the final output emission table
					String outputTableFieldsToUse = CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT? outputTableFieldsWithFuelSubType : outputTableFieldsNoFuelSubType;
					sql = "INSERT INTO " + ExecutionRunSpec.getEmissionOutputTable()
							+ "(iterationID," + outputTableFieldsToUse + ") "
							+ "SELECT iterationID," + outputTableFieldsToUse
							+ " FROM TemporaryOutputImport";
					SQLRunner.executeSQL(outputDatabase, sql);

					// Move data into the final output activity table
					String activityTableFieldsToUse = CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT? outputActivityTableFieldsWithFuelSubType : outputActivityTableFieldsNoFuelSubType;
					sql = "INSERT INTO " + ExecutionRunSpec.getActivityOutputTable()
							+ "(iterationID," + activityTableFieldsToUse + ") "
							+ "SELECT iterationID," + activityTableFieldsToUse
							+ " FROM TemporaryActivityOutputImport";
					SQLRunner.executeSQL(outputDatabase, sql);

					/* The following incremental aggregation was removed for Task 812.
					 * MOVES now only aggregates its outputs (emissions and activity)
					 * at the end of each iteration, if required and if desired by
					 * the user.
						// Aggregate the moves output for some of the processed bundles
						aggregateOutput(outputDatabase);
					*/
				} finally {
					if(!keepDebugData) {
						// Clear temporary output table.
						sql = "TRUNCATE TemporaryOutputImport";
						SQLRunner.executeSQL(outputDatabase, sql);
	
						// Clear temporary output table.
						sql = "TRUNCATE TemporaryActivityOutputImport";
						SQLRunner.executeSQL(outputDatabase, sql);
					}
				}
			}
		} catch (Exception exception) {
			/**
			 * @explain An error occurred while reading results returned from a worker.
			**/
			Logger.logError(exception, "Failed to process Worker Files in OutputProcessor.");
		} finally {
			if(executionDatabase != null) {
				DatabaseConnectionManager.checkInConnection(
						MOVESDatabaseType.EXECUTION, executionDatabase);
				executionDatabase = null;
			}
			if(outputDatabase != null) {
				DatabaseConnectionManager.checkInConnection(
						MOVESDatabaseType.OUTPUT, outputDatabase);
				outputDatabase = null;
			}
			updateMOVESRun();
		}
	}

	/**
	 * This method will aggregate the output every 10% of the total bundles.
	 * In this way, we minimize data in the output table and minimize the amount
	 * of time spent aggregating output data at the final pass.
	 * @param outputDatabase A connection to a MOVES output database containing the
	 * MOVESOutput table.
	**/
	private void aggregateOutput(Connection outputDatabase) {
		Integer t = MOVESEngine.theInstance.getHowManyOutboundBundlesWillBeCreated();
		int totalBundleCount = 0;
		if(t != null) {
			totalBundleCount = t.intValue();
		}
		if(totalBundleCount == 0) {
			return;
		}
		try {
			// Check the progress so far.  If not at a 10% point, then return, otherwise allow
			// the partial aggregation of results accumulated so far.
			if(0 != (MOVESEngine.theInstance.getHowManyBundlesProcessedSoFar()
					% ((totalBundleCount+9)/10))) {
				return;
			}
			Vector outputProcessorSQLs = ExecutionRunSpec.theExecutionRunSpec.outputProcessorSQLs;
			for(int i=0; i<outputProcessorSQLs.size(); i++) {
				SQLRunner.executeSQL(outputDatabase, (String) outputProcessorSQLs.elementAt(i));
			}
		} catch(Exception e) {
			/**
			 * @explain A database error occurred while aggregating data in the master-side
			 * output database.
			**/
			Logger.logError(e, "Failed to aggregate output, OutputProcessor.aggregateOutput()");
		}
	}

	/**
	 * Give IntegratedPostProcessor objects one last chance to examine the output database,
	 * aggregate the final output, then convert the units in the output database to the units
	 * the user has selected.  This is called by the unbundler.
	**/
	public void doFinalPostProcessingOnOutputDatabase() {
		Connection executionDatabase = null;
		Connection outputDatabase = null;
		try {
			outputDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.OUTPUT);

			if(!keepDebugData) {
				// Drop our intermediate results tables
				SQLRunner.executeSQL(outputDatabase,
						"DROP TABLE IF EXISTS TemporaryOutputImport");
				SQLRunner.executeSQL(outputDatabase,
						"DROP TABLE IF EXISTS TemporaryActivityOutputImport");
			}

			if(MOVESEngine.theInstance.allowFinalPostProcessing()) {
				executionDatabase = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.EXECUTION);

				// Invoke IntegratedPostProcessor objects before units get converted
				for (Iterator i = integratedPostProcessors.iterator(); i.hasNext(); ) {
					IntegratedPostProcessor iterProcessor = (IntegratedPostProcessor) i.next();

					iterProcessor.doFinalPostProcessingOnOutputDatabase(outputDatabase,
							executionDatabase,true);
				}

				// Final aggregation done to the output database
				if(ExecutionRunSpec.theExecutionRunSpec.shouldDoFinalAggregation()) {
					Logger.log(LogMessageCategory.INFO,"Final Aggregation starting...");
					if(keepDebugData) {
						SQLRunner.executeSQL(outputDatabase,"drop table if exists FinalAggBefore");
						SQLRunner.executeSQL(outputDatabase,"create table FinalAggBefore select * from MOVESOutput");
					}
					Vector finalProcessSQLs = ExecutionRunSpec.theExecutionRunSpec.finalProcessSQLs;
					for(int i=0; i<finalProcessSQLs.size(); i++) {
						SQLRunner.executeSQL(outputDatabase, (String) finalProcessSQLs.elementAt(i));
					}
					if(keepDebugData) {
						SQLRunner.executeSQL(outputDatabase,"drop table if exists FinalAggAfter");
						SQLRunner.executeSQL(outputDatabase,"create table FinalAggAfter select * from MOVESOutput");
					}
					Logger.log(LogMessageCategory.INFO,"Final Aggregation complete.");
				}

				// Convert the units
				Logger.log(LogMessageCategory.INFO,"Unit conversions starting...");
				convertOutputUnits(executionDatabase, outputDatabase);
				if(keepDebugData) {
					SQLRunner.executeSQL(outputDatabase,"drop table if exists UnitConvertAfter");
					SQLRunner.executeSQL(outputDatabase,"create table UnitConvertAfter select * from MOVESOutput");
				}
				Logger.log(LogMessageCategory.INFO,"Unit conversions done.");

				// Invoke IntegratedPostProcessor objects after units get converted
				if(integratedPostProcessors.size() > 0) {
					Logger.log(LogMessageCategory.INFO,"Final Post processing starting...");
					for (Iterator i = integratedPostProcessors.iterator(); i.hasNext(); ) {
						IntegratedPostProcessor iterProcessor = (IntegratedPostProcessor) i.next();

						iterProcessor.doFinalPostProcessingOnOutputDatabase(outputDatabase,
								executionDatabase,false);
					}
					/** @nonissue **/
					Logger.log(LogMessageCategory.INFO,"Final Post processing done.");
				} else {
					/** @nonissue **/
					Logger.log(LogMessageCategory.INFO,"No final post processing required.");
				}
			} else {
				/** @nonissue **/
				Logger.log(LogMessageCategory.INFO,"No unit conversion, aggregation, or post processing is performed in this mode.");
			}
		} catch (Exception exception) {
			//Logger.logException(LogMessageCategory.ERROR, exception);
			/**
			 * @explain A database error occurred while performing post processing on the
			 * simulator results, before converting units to user-selected units.
			**/
			Logger.logError(exception, "Failed to do final post processing in OutputProcessor.");
		} finally {
			if(executionDatabase != null) {
				DatabaseConnectionManager.checkInConnection(
					MOVESDatabaseType.EXECUTION, executionDatabase);
				executionDatabase = null;
			}
			if(outputDatabase != null) {
				DatabaseConnectionManager.checkInConnection(
					MOVESDatabaseType.OUTPUT, outputDatabase);
				outputDatabase = null;
			}
			updateMOVESRun();
		}
	}

	/**
	 * Converts the units of the Emission Quantities reported in MOVESOutput to the units
	 * selected in the RunSpec. The unit conversion involves,
	 * 1. Mass & Energy unit conversion
	 * 2. Time unit conversion
	 * This method is called by doFinalPostProcessingOnOutputDatabase after all
	 * other processing on the output database is complete.<br>
	 * The base output units are grams for mass and kilojoules for energy.
	 * @param executionDatabase A connection to an MOVES execution database containing the
	 * Pollutant table.
	 * @param outputDatabase A connection to an MOVES output database containing the
	 * MOVESOutput table.
	**/
	void convertOutputUnits(Connection executionDatabase, Connection outputDatabase) {
		PreparedStatement selectStatement = null;
		ResultSet results = null;
		String sql = "";
		try {
			String selectSQL = "SELECT DISTINCT PollutantID, EnergyOrMass FROM POLLUTANT";
			String massPollutants = "";
			String energyPollutants = "";
			selectStatement = executionDatabase.prepareStatement(selectSQL);
			results = SQLRunner.executeQuery(selectStatement, selectSQL);
			boolean isFirstEnergyPollutant = true;
			boolean isFirstMassPollutant = true;
			while(results.next()) {
				String pollutantID = results.getString(1);
				String units = results.getString(2);
				if(units == null || pollutantID == null) {
					continue;
				} else if(units.trim().equalsIgnoreCase("energy") ||
						units.indexOf("energy") >= 0){
					if(isFirstEnergyPollutant) {
						energyPollutants = pollutantID;
						isFirstEnergyPollutant = false;
					} else {
						energyPollutants = energyPollutants + "," + pollutantID;
					}
				} else {
					if(isFirstMassPollutant) {
						massPollutants = pollutantID;
						isFirstMassPollutant = false;
					} else {
						massPollutants = massPollutants + "," + pollutantID;
					}
				}
			}
			results.close();
			selectStatement.close();
			if(massPollutants.length() > 0) {
				double massConversionToKilogramFactor =
						ExecutionRunSpec.theExecutionRunSpec.getOutputFactors().
						massMeasurementSystem.getConversionToKilogramsFactor();
				// Native database units are grams (g) and measurement systems provide
				// factor in kilogram/user-unit.  Remember that dividing by a factor is the
				// same as multiplying by its inverse:
				//
				// n g      kg     user-unit      n
				// ---- * ------ * --------- = -------- user-units
				//  1     1000 g     c kg      c * 1000
				String updateMassUnitSQL = "UPDATE " + ExecutionRunSpec.getEmissionOutputTable()
						+ " SET EmissionQuant ="
						+ " EmissionQuant / 1000 / " + massConversionToKilogramFactor
						+ " WHERE PollutantID IN (" + massPollutants + ") AND MOVESRunID ="
						+ MOVESEngine.theInstance.getActiveRunID();
				if(estimateUncertainty) {
					updateMassUnitSQL += " AND iterationID ="
							+ MOVESEngine.theInstance.getActiveIterationID();
				}
				//SQLRunner.executeSQL(outputDatabase, "FLUSH TABLES");
				SQLRunner.executeSQL(outputDatabase, updateMassUnitSQL);

				if(CompilationFlags.DO_RATES_FIRST) {
					updateMassUnitSQL = "update BaseRateUnits set"
							+ " meanBaseRateUnitsNumerator = " + DatabaseUtilities.escapeSQL(MOVESEngine.theInstance.masterFragment.massUnits,true)
							+ " ,emissionBaseRateUnitsNumerator = " + DatabaseUtilities.escapeSQL(MOVESEngine.theInstance.masterFragment.massUnits,true)
							+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID()
							+ " and pollutantID in (" + massPollutants + ")";
					SQLRunner.executeSQL(outputDatabase, updateMassUnitSQL);

					updateMassUnitSQL = "update BaseRateOutput set"
						+ " meanBaseRate = meanBaseRate / 1000 / " + massConversionToKilogramFactor
						+ ",emissionRate = emissionRate / 1000 / " + massConversionToKilogramFactor
						+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID()
						+ " and pollutantID in (" + massPollutants + ")";
					if(estimateUncertainty) {
						updateMassUnitSQL += " AND iterationID ="
								+ MOVESEngine.theInstance.getActiveIterationID();
					}
					SQLRunner.executeSQL(outputDatabase, updateMassUnitSQL);
				}
			}
			sql = "update MOVESRun set massUnits=" + DatabaseUtilities.escapeSQL(MOVESEngine.theInstance.masterFragment.massUnits,true)
					+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
			SQLRunner.executeSQL(outputDatabase, sql);

			if(energyPollutants.length() > 0) {
				double energyConversionToJoulesFactor =
						ExecutionRunSpec.theExecutionRunSpec.getOutputFactors().
						energyMeasurementSystem.getConversionToJoulesFactor();
				// Native database units are kilojoules (KJ) and measurement systems provide
				// factor in joules/user-unit.  Remember that dividing by a factor is the
				// same as multiplying by its inverse:
				//
				// n KJ   1000 J   user-unit   n*1000
				// ---- * ------ * --------- = ------ user-units
				//  1       KJ        c J         c
				String updateEnergyUnitSQL = "UPDATE " + ExecutionRunSpec.getEmissionOutputTable()
						+ " SET EmissionQuant ="
						+ " EmissionQuant * 1000 / " + energyConversionToJoulesFactor
						+ " WHERE PollutantID IN (" + energyPollutants + ") AND MOVESRunID ="
						+ MOVESEngine.theInstance.getActiveRunID();
				if(estimateUncertainty) {
						updateEnergyUnitSQL += " AND iterationID="
								+ MOVESEngine.theInstance.getActiveIterationID();
				}
				//SQLRunner.executeSQL(outputDatabase, "FLUSH TABLES");
				SQLRunner.executeSQL(outputDatabase, updateEnergyUnitSQL);

				if(CompilationFlags.DO_RATES_FIRST) {
					updateEnergyUnitSQL = "update BaseRateUnits set"
							+ " meanBaseRateUnitsNumerator = " + DatabaseUtilities.escapeSQL(MOVESEngine.theInstance.masterFragment.energyUnits,true)
							+ " ,emissionBaseRateUnitsNumerator = " + DatabaseUtilities.escapeSQL(MOVESEngine.theInstance.masterFragment.energyUnits,true)
							+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID()
							+ " and pollutantID in (" + energyPollutants + ")";
					SQLRunner.executeSQL(outputDatabase, updateEnergyUnitSQL);

					updateEnergyUnitSQL = "update BaseRateOutput set"
						+ " meanBaseRate = meanBaseRate * 1000 / " + energyConversionToJoulesFactor
						+ ",emissionRate = emissionRate * 1000 / " + energyConversionToJoulesFactor
						+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID()
						+ " and pollutantID in (" + energyPollutants + ")";
					if(estimateUncertainty) {
						updateEnergyUnitSQL += " AND iterationID ="
								+ MOVESEngine.theInstance.getActiveIterationID();
					}
					SQLRunner.executeSQL(outputDatabase, updateEnergyUnitSQL);
				}
			}
			sql = "update MOVESRun set energyUnits=" + DatabaseUtilities.escapeSQL(MOVESEngine.theInstance.masterFragment.energyUnits,true)
					+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
			SQLRunner.executeSQL(outputDatabase, sql);

			// Time unit conversion
			double timeConversionToSecondsFactor =
					ExecutionRunSpec.theExecutionRunSpec.getOutputFactors().timeMeasurementSystem.
					getConversionToSecondsFactor();
			double averageHours = ExecutionRunSpec.theExecutionRunSpec.getOutputTimeStep().
					getAverageHours();
			Logger.log(LogMessageCategory.INFO,"Time Measurement System = " +
					ExecutionRunSpec.theExecutionRunSpec.getOutputFactors().timeMeasurementSystem);
			Logger.log(LogMessageCategory.INFO,"Output Time Step = " +
					ExecutionRunSpec.theExecutionRunSpec.getOutputTimeStep());
			Logger.log(LogMessageCategory.INFO,"Output Time Step average hours = " + averageHours);

			// Native database units are user defined and measurement systems,
			// provide factor in sec/(TMS)user-unit.  Output time step uses hour/(OTS)user-unit.
			// Remember that dividing by a factor is the same as multiplying by its inverse:
			// OTS : Output Time Step
			// TMS : Time Measurement System
			//
			//  n      OTS     hour   sec
			// ---- * ------ * ---- * --- =  (n / averageHours / 3600 * TMS) user-units
			// OTS     hour    sec    TMS
			String updateTimeUnitSQL = "UPDATE " + ExecutionRunSpec.getEmissionOutputTable()
					+ " SET EmissionQuant =" + " EmissionQuant / " + averageHours + " / 3600 * "
					+ timeConversionToSecondsFactor
					+ " WHERE MOVESRunID =" + MOVESEngine.theInstance.getActiveRunID();
			if(estimateUncertainty) {
				updateTimeUnitSQL += " AND iterationID ="
						+ MOVESEngine.theInstance.getActiveIterationID();
			}
			//SQLRunner.executeSQL(outputDatabase, "FLUSH TABLES");
//			SQLRunner.executeSQL(outputDatabase, updateTimeUnitSQL);
			// Distance unit conversion, including the time unit conversion too
			if(ExecutionRunSpec.theExecutionRunSpec.getOutputFactors().distanceMeasurementSystem
					!= null) {
				double distanceConversionToMetersFactor =
						ExecutionRunSpec.theExecutionRunSpec.getOutputFactors().
						distanceMeasurementSystem.getConversionToMetersFactor();
				// Native database units are miles and measurement systems provide
				// factor in meters/user-unit.
				//
				//  n miles    1609.344 meters       user-units
				// --------- * ----------------- * ------------- = (n * 1609.344 / c) user-units
				//     1           miles             c meters
				/*
				String updateDistanceUnitSQL = "UPDATE " + ExecutionRunSpec.getActivityOutputTable()
						+ " SET activity ="
						+ " activity * 1609.344 / " + distanceConversionToMetersFactor
						+ " / " + averageHours + " / 3600 * "
						+ timeConversionToSecondsFactor
						+ " WHERE MOVESRunID =" + MOVESEngine.theInstance.getActiveRunID()
						+ " AND activityTypeID=1";
				*/
				String updateDistanceUnitSQL = "UPDATE " + ExecutionRunSpec.getActivityOutputTable()
						+ " SET activity ="
						+ " activity * 1609.344 / " + distanceConversionToMetersFactor
						+ " WHERE MOVESRunID =" + MOVESEngine.theInstance.getActiveRunID()
						+ " AND activityTypeID=1";
				if(estimateUncertainty) {
					updateDistanceUnitSQL += " AND iterationID = "
							+ MOVESEngine.theInstance.getActiveIterationID();
				}

				//SQLRunner.executeSQL(outputDatabase, "FLUSH TABLES");
				SQLRunner.executeSQL(outputDatabase, updateDistanceUnitSQL);

				if(CompilationFlags.DO_RATES_FIRST) {
					updateDistanceUnitSQL = "update BaseRateUnits set"
							+ " emissionBaseRateUnitsDenominator = " + DatabaseUtilities.escapeSQL(MOVESEngine.theInstance.masterFragment.distanceUnits,true)
							+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID()
							+ " and emissionBaseRateUnitsDenominator='mi'";
					SQLRunner.executeSQL(outputDatabase, updateDistanceUnitSQL);

					// g        mi       c meters         g
					// -- * ---------- * ---------- = ----------
					// mi   1609.344 m   user-units   user-units
					updateDistanceUnitSQL = "update BaseRateOutput set"
						+ " emissionRate = emissionRate * " + distanceConversionToMetersFactor + " / 1609.344"
						+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID()
						+ " and processID in (1,9,10,15)";
					if(estimateUncertainty) {
						updateDistanceUnitSQL += " AND iterationID ="
								+ MOVESEngine.theInstance.getActiveIterationID();
					}
					SQLRunner.executeSQL(outputDatabase, updateDistanceUnitSQL);
				}
			}
			sql = "update MOVESRun set distanceUnits=" + DatabaseUtilities.escapeSQL(MOVESEngine.theInstance.masterFragment.distanceUnits,true)
					+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
			SQLRunner.executeSQL(outputDatabase, sql);
		} catch (Exception exception) {
			/**
			 * @explain A database error occurred while converting standard units in the output
			 * database into user-selected units.  The output data should be considered suspect.
			**/
			Logger.logError(exception, "Failed to convert emission quantity units.");
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
			if(selectStatement != null) {
				try {
					selectStatement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
	}

	/**
	 * Prepare output database for uncertainty estimations
	**/
	public void prepareForEstimateUncertainty() {
		estimateUncertainty = ExecutionRunSpec.theExecutionRunSpec.estimateUncertainty();
		if(estimateUncertainty) {
			Connection outputDatabase = null;
			String sql = "";
			try {
				outputDatabase = DatabaseConnectionManager.checkOutConnection(
							MOVESDatabaseType.OUTPUT);
				try {
					sql = "ALTER TABLE " + ExecutionRunSpec.getEmissionOutputTable()
						+ " ADD COLUMN (emissionQuantSum DOUBLE, emissionQuantSum2 DOUBLE)";
					SQLRunner.executeSQL(outputDatabase, sql);
				} catch (Exception exception) {
					// Nothing to do here.
				}
				try {
					sql = "ALTER TABLE " + ExecutionRunSpec.getActivityOutputTable()
						+ " ADD COLUMN (activitySum DOUBLE, activitySum2 DOUBLE)";
					SQLRunner.executeSQL(outputDatabase, sql);
				} catch (Exception exception) {
					// Nothing to do here.
				}
			} catch (Exception exception) {
				/**
				 * @explain A database error occurred while processing uncertainty estimation
				 * data.
				**/
				Logger.logError(exception,
						"Failed to add columns used in estimating uncertainty.");
			} finally {
				if(outputDatabase != null) {
					DatabaseConnectionManager.checkInConnection(
							MOVESDatabaseType.OUTPUT, outputDatabase);
					outputDatabase = null;
				}
			}
		}
	}

	/**
	 * Clean up after uncertainty estimations
	**/
	public void cleanUpAfterEstimateUncertainty() {
		Connection outputDatabase = null;
		String sql = "";
		try {
			outputDatabase = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.OUTPUT);
			if(estimateUncertainty) {
				try {
					sql = "ALTER TABLE " + ExecutionRunSpec.getEmissionOutputTable()
						+ " DROP COLUMN emissionQuantSum, DROP COLUMN emissionQuantSum2";
					SQLRunner.executeSQL(outputDatabase, sql);
					sql = "ALTER TABLE " + ExecutionRunSpec.getActivityOutputTable()
						+ " DROP COLUMN activitySum, DROP COLUMN activitySum2";
					SQLRunner.executeSQL(outputDatabase, sql);
				} catch (Exception exception) {
					/**
					 * @explain A database error occurred while processing uncertainty estimation
					 * data.
					**/
					Logger.logError(exception,
							"Failed to drop columns used in estimating uncertainty.");
				}
			}
			if(ExecutionRunSpec.getRunSpec().scale == ModelScale.MESOSCALE_LOOKUP) {
				if(ExecutionRunSpec.getRunSpec().shouldTruncateMOVESOutput) {
					Logger.log(LogMessageCategory.INFO,"Removing data from MOVESOutput per Advanced Performance Features setting...");
					sql = "delete from MOVESOutput where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
					SQLRunner.executeSQL(outputDatabase, sql);
					// Truncate if there is no more data in order to save time when transfering output databases.
					sql = "select count(*) from MOVESOutput";
					if(SQLRunner.executeScalar(outputDatabase, sql) <= 0) {
						Logger.log(LogMessageCategory.INFO,"Truncating empty MOVESOutput per Advanced Performance Features setting.");
						sql = "truncate MOVESOutput";
						SQLRunner.executeSQL(outputDatabase, sql);
					}
					Logger.log(LogMessageCategory.INFO,"Removed data from MOVESOutput per Advanced Performance Features setting.");
				}
				if(ExecutionRunSpec.getRunSpec().shouldTruncateMOVESActivityOutput) {
					Logger.log(LogMessageCategory.INFO,"Removing data from MOVESActivityOutput per Advanced Performance Features setting...");
					sql = "delete from MOVESActivityOutput where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
					SQLRunner.executeSQL(outputDatabase, sql);
					// Truncate if there is no more data in order to save time when transfering output databases.
					sql = "select count(*) from MOVESActivityOutput";
					if(SQLRunner.executeScalar(outputDatabase, sql) <= 0) {
						Logger.log(LogMessageCategory.INFO,"Truncating empty MOVESActivityOutput per Advanced Performance Features setting.");
						sql = "truncate MOVESActivityOutput";
						SQLRunner.executeSQL(outputDatabase, sql);
					}
					Logger.log(LogMessageCategory.INFO,"Removed data from MOVESActivityOutput per Advanced Performance Features setting.");
				}
				if(ExecutionRunSpec.getRunSpec().shouldTruncateBaseRateOutput) {
					Logger.log(LogMessageCategory.INFO,"Removing data from BaseRateOutput per Advanced Performance Features setting...");
					sql = "delete from BaseRateOutput where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
					SQLRunner.executeSQL(outputDatabase, sql);
					// Truncate if there is no more data in order to save time when transfering output databases.
					sql = "select count(*) from BaseRateOutput";
					if(SQLRunner.executeScalar(outputDatabase, sql) <= 0) {
						Logger.log(LogMessageCategory.INFO,"Truncating empty BaseRateOutput per Advanced Performance Features setting.");
						sql = "truncate BaseRateOutput";
						SQLRunner.executeSQL(outputDatabase, sql);
					}
					Logger.log(LogMessageCategory.INFO,"Removed data from BaseRateOutput per Advanced Performance Features setting.");

					Logger.log(LogMessageCategory.INFO,"Removing data from BaseRateUnits per Advanced Performance Features setting...");
					sql = "delete from BaseRateUnits where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
					SQLRunner.executeSQL(outputDatabase, sql);
					// Truncate if there is no more data in order to save time when transfering output databases.
					sql = "select count(*) from BaseRateUnits";
					if(SQLRunner.executeScalar(outputDatabase, sql) <= 0) {
						Logger.log(LogMessageCategory.INFO,"Truncating empty BaseRateUnits per Advanced Performance Features setting.");
						sql = "truncate BaseRateUnits";
						SQLRunner.executeSQL(outputDatabase, sql);
					}
					Logger.log(LogMessageCategory.INFO,"Removed data from BaseRateUnits per Advanced Performance Features setting.");
				}
			}
		} catch(Exception exception) {
			/**
			 * @explain A database error occurred while removing data from MOVESOutput, MOVESActivityOutput, BaseRateOutput, or BaseRateUnits.
			**/
			Logger.logError(exception,
					"A database error occurred while removing data from MOVESOutput, MOVESActivityOutput, BaseRateOutput, or BaseRateUnits.");
		} finally {
			if(outputDatabase != null) {
				DatabaseConnectionManager.checkInConnection(
						MOVESDatabaseType.OUTPUT, outputDatabase);
				outputDatabase = null;
			}
		}
	}

	/**
	 * Perform uncertainty estimations for the iteration.
	**/
	public void performEstimateUncertainty() {
		if(estimateUncertainty) {
			Connection outputDatabase = null;
			String sql = "";
			try {
				int activeRunID = MOVESEngine.theInstance.getActiveRunID();
				int activeIterationID = MOVESEngine.theInstance.getActiveIterationID();
				Logger.log(LogMessageCategory.INFO,"Estimating uncertainty for run "
						+ activeRunID + " iteration "+activeIterationID);
				outputDatabase = DatabaseConnectionManager.checkOutConnection(
							MOVESDatabaseType.OUTPUT);
				if(activeIterationID == 1) {
					sql = "UPDATE " + ExecutionRunSpec.getEmissionOutputTable()
							+ " SET emissionQuantMean = emissionQuant,"
							+ " emissionQuantSum = emissionQuant, emissionQuantSum2 ="
							+ " emissionQuant * emissionQuant WHERE MOVESRunID = " + activeRunID
							+ " AND iterationID = 1";
					SQLRunner.executeSQL(outputDatabase,sql);
					/*=======================================================
					 * For calculating activity uncertainty when available.
					 *=======================================================
					sql = "UPDATE " + ExecutionRunSpec.getActivityOutputTable()
							+ " SET activityMean = activity, "
							+ " activitySigma = 0, activitySum = activity,"
							+ " activitySum2 = activity * activity"
							+ " WHERE MOVESRunID = " + activeRunID
							+ " AND iterationID = 1";
					SQLRunner.executeSQL(outputDatabase,sql);
					 *=====================================================*/
				} else {
					sql = "drop table if exists MOVESOutputSumTemp";
					SQLRunner.executeSQL(outputDatabase,sql);
					sql = "create table MOVESOutputSumTemp like "
							+ ExecutionRunSpec.getEmissionOutputTable();
					SQLRunner.executeSQLCore(outputDatabase,sql);

					String[] emissionStatements = {
						"create index IXOutputSumTemp on MOVESOutputSumTemp (iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,"
								+ " sourceTypeID,regClassID,fuelTypeID,fuelSubTypeID,modelYearID,roadTypeID,SCC)",
						"insert into MOVESOutputSumTemp (MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,"
								+ " sourceTypeID,regClassID,fuelTypeID,fuelSubTypeID,modelYearID,roadTypeID,SCC,"
//								+ " engTechID,sectorID,hpID,"
								+ " emissionQuant, emissionQuantMean, emissionQuantSum, emissionQuantSum2)"
								+ " select MOVESRunID,iterationID,"
								+ " coalesce(yearID,0),coalesce(monthID,0),coalesce(dayID,0),coalesce(hourID,0),coalesce(stateID,0),coalesce(countyID,0),"
								+ " coalesce(zoneID,0),coalesce(linkID,0),coalesce(pollutantID,0),coalesce(processID,0),"
								+ " coalesce(sourceTypeID,0),coalesce(regClassID,0),coalesce(fuelTypeID,0),coalesce(fuelSubTypeID,0),coalesce(modelYearID,0),coalesce(roadTypeID,0),coalesce(SCC,0),"
//								+ " coalesce(engTechID,0),coalesce(sectorID,0),coalesce(hpID,0),"
								+ " emissionQuant, emissionQuantMean, emissionQuantSum, emissionQuantSum2"
								+ " from " + ExecutionRunSpec.getEmissionOutputTable()
								+ " where MOVESRunID=" + activeRunID
								+ " and iterationID in (" + activeIterationID + "," + (activeIterationID-1) + ")",
						"CREATE TABLE IF NOT EXISTS MOVESOutputSum"
								+ " SELECT mo2.MOVESRunID, mo2.iterationID, mo2.yearID, mo2.monthID, mo2.dayID, mo2.hourID, mo2.stateID, "
								+ " mo2.countyID, mo2.zoneID, mo2.linkID, mo2.pollutantID, mo2.processID, mo2.sourceTypeID, mo2.regClassID, mo2.fuelTypeID, mo2.fuelSubTypeID,"
								+ " mo2.modelYearID, mo2.roadTypeID, mo2.SCC, "
//								+ " mo2.engTechID, mo2.sectorID, mo2.hpID,"
								+ " mo2.emissionQuant,"
								+ " (mo2.emissionQuant + mo1.emissionQuantSum)/ mo2.iterationID AS emissionQuantMean, "
								+ " mo2.emissionQuant + mo1.emissionQuantSum AS emissionQuantSum, "
								+ " mo2.emissionQuant * mo2.emissionQuant + mo1.emissionQuantSum2 AS emissionQuantSum2 "
								+ " FROM MOVESOutputSumTemp AS mo1, MOVESOutputSumTemp AS mo2 "
								+ " WHERE mo2.iterationID = " + activeIterationID
								+ " AND mo1.iterationID = " + (activeIterationID-1)
								+ " AND mo1.yearID=mo2.yearID"
								+ " AND mo1.monthID=mo2.monthID"
								+ " AND mo1.dayID=mo2.dayID"
								+ " AND mo1.hourID=mo2.hourID"
								+ " AND mo1.stateID=mo2.stateID"
								+ " AND mo1.countyID=mo2.countyID"
								+ " AND mo1.zoneID=mo2.zoneID"
								+ " AND mo1.linkID=mo2.linkID"
								+ " AND mo1.pollutantID=mo2.pollutantID"
								+ " AND mo1.processID=mo2.processID"
								+ " AND mo1.sourceTypeID=mo2.sourceTypeID"
								+ " AND mo1.regClassID=mo2.regClassID"
								+ " AND mo1.fuelTypeID=mo2.fuelTypeID"
								+ " AND mo1.fuelSubTypeID=mo2.fuelSubTypeID"
								+ " AND mo1.modelYearID=mo2.modelYearID"
								+ " AND mo1.roadTypeID=mo2.roadTypeID"
								+ " AND mo1.SCC=mo2.SCC",
//								+ " AND mo1.engTechID=mo2.engTechID"
//								+ " AND mo1.sectorID=mo2.sectorID"
//								+ " AND mo1.hpID=mo2.hpID",
						"drop table MOVESOutputSumTemp",
						"DELETE FROM " + ExecutionRunSpec.getEmissionOutputTable()
								+ " WHERE MOVESRunID = " + activeRunID
								+ " AND iterationID = " + activeIterationID,
						"INSERT INTO " + ExecutionRunSpec.getEmissionOutputTable()
								+ " (MOVESRunID, iterationID, yearID, monthID,"
								+ " dayID, hourID, stateID, countyID, zoneID, linkID, pollutantID,"
								+ " processID, sourceTypeID, regClassID, fuelTypeID, fuelSubTypeID, modelYearID, roadTypeID,"
								+ " SCC,"
//								+ " engTechID,sectorID,hpID,"
								+ " emissionQuant, emissionQuantMean, emissionQuantSigma, "
								+ " emissionQuantSum, emissionQuantSum2)"
								+ " SELECT MOVESRunID,"
								+ " iterationID, yearID, monthID, dayID, hourID, stateID,"
								+ " countyID,zoneID, linkID, pollutantID, processID, sourceTypeID, regClassID,"
								+ " fuelTypeID, fuelSubTypeID, modelYearID, roadTypeID, SCC,"
//								+ " engTechID, sectorID, hpID,"
								+ " emissionQuant,"
								+ " emissionQuantMean,SQRT((emissionQuantSum2-"
								+ "(emissionQuantSum*emissionQuantSum/iterationID))"
								+ " /(iterationID-1)) AS emissionQuantSigma,"
								+ " emissionQuantSum,emissionQuantSum2 FROM MOVESOutputSum",
						"DROP TABLE MOVESOutputSum"
					};
					for(int i=0;i<emissionStatements.length;i++) {
						sql = emissionStatements[i];
						SQLRunner.executeSQL(outputDatabase,sql);
					}

					/*=======================================================
					 * For calculating distance uncertainty when available.
					 *=======================================================
					sql = "CREATE TABLE IF NOT EXISTS MOVESActivityOutputSum SELECT"
							+ " mo2.MOVESRunID, mo2.iterationID, mo2.yearID, mo2.monthID,"
							+ " mo2.dayID, mo2.hourID, mo2.stateID, mo2.countyID, mo2.zoneID,"
							+ " mo2.linkID, mo2.sourceTypeID, mo2.regClassID,"
							+ " mo2.fuelTypeID, mo2.fuelSubTypeID, mo2.modelYearID, mo2.roadTypeID, mo2.SCC,"
							+ " mo2.activityTypeID,"
							+ " mo2.activity,(mo2.activity + mo1.activitySum)/"
							+ " mo2.iterationID AS activityMean,"
							+ " mo2.activity + mo1.activitySum AS activitySum,"
							+ " mo2.activity * mo2.activity + mo1.activitySum2"
							+ " AS activitySum2"
							+ " FROM " + ExecutionRunSpec.getActivityOutputTable() + " AS mo1,"
							+ " " + ExecutionRunSpec.getActivityOutputTable() + " AS mo2"
							+ " WHERE mo2.MOVESRunID = " + activeRunID
							+ " AND mo1.MOVESRunID = mo2.MOVESRunID"
							+ " AND mo2.iterationID = " + activeIterationID
							+ " AND mo1.iterationID= " + (activeIterationID-1) // mo2.iterationID - 1"
							+ " AND mo1.activityTypeID=mo2.activityTypeID"
							+ " AND COALESCE(mo1.yearID,0)=COALESCE(mo2.yearID,0)"
							+ " AND COALESCE(mo1.monthID,0)=COALESCE(mo2.monthID,0)"
							+ " AND COALESCE(mo1.dayID,0)=COALESCE(mo2.dayID,0)"
							+ " AND COALESCE(mo1.hourID,0)=COALESCE(mo2.hourID,0)"
							+ " AND COALESCE(mo1.stateID,0)=COALESCE(mo2.stateID,0)"
							+ " AND COALESCE(mo1.countyID,0)=COALESCE(mo2.countyID,0)"
							+ " AND COALESCE(mo1.zoneID,0)=COALESCE(mo2.zoneID,0)"
							+ " AND COALESCE(mo1.linkID,0)=COALESCE(mo2.linkID,0)"
							+ " AND COALESCE(mo1.sourceTypeID,0)=COALESCE(mo2.sourceTypeID,0)"
							+ " AND COALESCE(mo1.regClassID,0)=COALESCE(mo2.regClassID,0)"
							+ " AND COALESCE(mo1.fuelTypeID,0)=COALESCE(mo2.fuelTypeID,0)"
							+ " AND COALESCE(mo1.fuelSubTypeID,0)=COALESCE(mo2.fuelSubTypeID,0)"
							+ " AND COALESCE(mo1.modelYearID,0)=COALESCE(mo2.modelYearID,0)"
							+ " AND COALESCE(mo1.roadTypeID,0)=COALESCE(mo2.roadTypeID,0)"
							+ " AND COALESCE(mo1.SCC,0)=COALESCE(mo2.SCC,0)";
					SQLRunner.executeSQL(outputDatabase,sql);

					sql = "DELETE FROM " + ExecutionRunSpec.getActivityOutputTable()
							+ " WHERE MOVESRunID = " + activeRunID
							+ " AND iterationID = " + activeIterationID;
					SQLRunner.executeSQL(outputDatabase,sql);

					sql = "INSERT INTO " + ExecutionRunSpec.getActivityOutputTable()
							+ " (MOVESRunID, iterationID, yearID,"
							+ " monthID,"
							+ " dayID, hourID, stateID, countyID, zoneID, linkID,"
							+ " sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID,"
							+ " SCC, activityTypeID, activity, activityMean, activitySigma, "
							+ " activitySum, activitySum2) SELECT MOVESRunID,"
							+ " iterationID, yearID, monthID, dayID, hourID, stateID,"
							+ " countyID, zoneID, linkID, sourceTypeID, regClassID, fuelTypeID,"
							+ " modelYearID, roadTypeID, SCC, activityTypeID,"
							+ " activity, activityMean,"
							+ " SQRT(activitySum2-iterationID*activityMean/"
							+ " (iterationID-1)) AS activitySigma, activitySum,"
							+ " activitySum2 FROM MOVESActivityOutputSum";
					SQLRunner.executeSQL(outputDatabase,sql);

					sql = "DROP TABLE MOVESActivityOutputSum";
					SQLRunner.executeSQL(outputDatabase,sql);
				     *=====================================================*/
				}

				OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
						ExecutionRunSpec.theExecutionRunSpec
						.getOutputEmissionsBreakdownSelection();
				if(!outputEmissionsBreakdownSelection.keepIterations) {
					sql = "DELETE FROM " + ExecutionRunSpec.getEmissionOutputTable()
							+ " WHERE MOVESRunID = " + activeRunID
							+ " AND iterationID < " + activeIterationID;
					SQLRunner.executeSQL(outputDatabase,sql);

					sql = "DELETE FROM " + ExecutionRunSpec.getActivityOutputTable()
							+ " WHERE MOVESRunID = " + activeRunID
							+ " AND iterationID < " + activeIterationID;
					SQLRunner.executeSQL(outputDatabase,sql);
				}
			} catch (Exception exception) {
				/**
				 * @explain A database error occurred while processing uncertainty estimation
				 * data.
				**/
				Logger.logError(exception, "Failed to prepare OutputProcessor for estimating"
						+ " uncertainty.");
			} finally {
				if(outputDatabase != null) {
					DatabaseConnectionManager.checkInConnection(
							MOVESDatabaseType.OUTPUT, outputDatabase);
					outputDatabase = null;
				}
			}
		}
	}

	/**
	 * Update MOVESRun.minutesDuration
	**/
	void updateMOVESRun() {
		Connection outputDatabase = null;
		String sql = "";
		try {
			outputDatabase = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.OUTPUT);
			long elapsedMillis = System.currentTimeMillis()
					- MOVESEngine.theInstance.startTimeMillis;
			double elapsedMinutes = elapsedMillis/1000.0/60.0;
			sql = "update MOVESRun set minutesDuration=" + elapsedMinutes
					+ " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
			SQLRunner.executeSQL(outputDatabase, sql);
		} catch (Exception exception) {
			Logger.logSqlError(exception,
					"Failed to update MOVESRun.minutesDuration",sql);
		} finally {
			if(outputDatabase != null) {
				DatabaseConnectionManager.checkInConnection(
						MOVESDatabaseType.OUTPUT, outputDatabase);
				outputDatabase = null;
			}
		}
	}

	/**
	 * Preserve CMITs if the run spec indicates
	**/
	public void saveCMITs() {
		if(!ExecutionRunSpec.shouldCopySavedGeneratorData()) {
			return;
		}
		TreeSetIgnoreCase classesToSaveData = ExecutionRunSpec.getClassesToSaveData();
		if(classesToSaveData.size() <= 0) {
			return;
		}
		Logger.log(LogMessageCategory.INFO,"Saving CMITs...");
		String[] sqlStatements = {
			"gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator",
				"SHO", "SourceHours", "HotellingHours", "Starts", "StartsPerVehicle", "",
			"gov.epa.otaq.moves.master.implementation.ghg.ProjectTAG",
				"SHO", "SourceHours", "HotellingHours", "Starts", "StartsPerVehicle", "",
			"gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator",
				"OpModeDistribution", "",
			"gov.epa.otaq.moves.master.implementation.ghg.LinkOperatingModeDistributionGenerator",
				"OpModeDistribution", "",
			"gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator",
				"OpModeDistribution", "",
			"gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator",
				"OpModeDistribution", "",
			"gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator",
				"OpModeDistribution", "",
			"gov.epa.otaq.moves.master.implementation.ghg.SourceBinDistributionGenerator",
				"SourceBinDistribution", "SourceBin", "",
		    "gov.epa.otaq.moves.master.implementation.general.MeteorologyGenerator",
				"ZoneMonthHour", "",
			"gov.epa.otaq.moves.master.implementation.ghg.TankTemperatureGenerator",
				"AverageTankTemperature", "SoakActivityFraction",
				"ColdSoakInitialHourFraction", "ColdSoakTankTemperature",
				"SampleVehicleTripByHour", "HotSoakEventByHour",
				"",
			"gov.epa.otaq.moves.master.implementation.ghg.TankFuelGenerator",
				"AverageTankGasoline", "",
			"gov.epa.otaq.moves.master.implementation.ghg.FuelEffectsGenerator",
				"ATRatio", "criteriaRatio", "GeneralFuelRatio", "",
			"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupTotalActivityGenerator",
				"SHO", "SourceHours", "",
			"gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupOperatingModeDistributionGenerator",
				"OpModeDistribution", ""
		};
		DatabaseSelection dbSelection = ExecutionRunSpec.getSavedGeneratorDatabase();
		String sql = "";
		Connection userDB = null;
		Connection executionDB = null;
		String calculatorName = EmissionCalculator.class.getName();
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			userDB = dbSelection.openConnectionOrNull();
			if(userDB == null) {
				if(dbSelection.safeCreateDatabase("database/CreateDefault.sql") == DatabaseSelection.NOT_CREATED) {
					/**
					 * @explain A database error occurred while creating a database to hold
					 * saved generator data.
					**/
					Logger.log(LogMessageCategory.ERROR,
							"Could not create the generator database.");
					return;
				}
				userDB = dbSelection.openConnectionOrNull();
				if(userDB == null) {
					return;
				}
			}
			executionDB = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.EXECUTION);

			for(Iterator i=classesToSaveData.iterator();i.hasNext();) {
				String c = (String)i.next();
				if(c.equalsIgnoreCase(calculatorName)) {
					continue;
				}
				for(int j=0;j<sqlStatements.length;j++) {
					if(c.equalsIgnoreCase(sqlStatements[j])) {
						for(j++;j<sqlStatements.length && sqlStatements[j].length() > 0;j++) {
							// Create the table if it doesn't exist.  Doing so allows for
							// tables that aren't in CreateDefault.sql
							try {
								sql = "SHOW CREATE TABLE " + sqlStatements[j];
								query.open(executionDB,sql);
								if(query.rs.next()) {
									String createStatement = StringUtilities.replace(
											query.rs.getString(2),
											"CREATE TABLE `","CREATE TABLE IF NOT EXISTS `") + ";";
									SQLRunner.executeSQL(userDB,createStatement);
								}
							} catch(SQLException e) {
								// Skip the table because it doesn't exist in the execution database.
								// This happens when a required generator was not part of the runspec.
								// As such, the table can be skipped because the generator isn't needed.
								continue;
							} finally {
								query.onFinally();
							}
							// Limit the data with an optional WHERE clause
							String whereClause = getCMITWhereClause(executionDB,c,sqlStatements[j]);
							if(whereClause != null && whereClause.length() > 0) {
								// Try to remove all old data before applying new data.  This
								// is the safest thing to do with distribution data.
								sql = "delete from " + sqlStatements[j]
										+ " where " + whereClause;
								SQLRunner.executeSQL(userDB,sql);
							}
							// Move data
							DatabaseUtilities.replaceIntoTable(executionDB,userDB,sqlStatements[j],
									whereClause,true);
						}
						break;
					}
				}
			}
			/** @nonissue **/
			Logger.log(LogMessageCategory.INFO,"Done saving CMITs.");
		} catch(SQLException e) {
			/** @explain An error occurred while capturing generator data **/
			Logger.logSqlError(e,"Unable to save CMITs",sql);
		} catch(IOException e) {
			/** @explain An error occurred while capturing generator data **/
			Logger.logError(e,"Unable to save CMITs");
		} finally {
			if(userDB != null) {
				DatabaseUtilities.closeConnection(userDB);
			}
			executionDB = null; // does not need to be closed
			query.onFinally();
		}
	}

	/**
	 * Generate a SQL fragment to filter a CMIT during its copy to
	 * another database as part of the Advanced Performance Features.
	 * @param executionDB a connection to the execution database
	 * @param className class that generated the CMIT table
	 * @param tableName CMIT table name
	 * @return SQL fragment suitable to use as with the WHERE word.
	**/
	String getCMITWhereClause(Connection executionDB,String className,String tableName) {
		String whereClause = "";
		if(tableName.equalsIgnoreCase("OpModeDistribution")) {
			String processIDs = "";
			String polProcessIDs = "";
			// The OMDG-family needs OpModeDistribution filtered by processes
			/*
			+-----------+-------------------------+
			| processID | processName             |
			+-----------+-------------------------+
			|         1 | Running Exhaust         |
			|         2 | Start Exhaust           |
			|        90 | Extended Idle Exhaust   |
			|        99 | Well-to-Pump            |
			|         7 | Crankcase               |
			|         9 | Brakewear               |
			|        10 | Tirewear                |
			|        11 | Evap Permeation         |
			|        12 | Evap Fuel Vapor Venting |
			|        13 | Evap Fuel Leaks         |
			|        14 | Evap Non-Fuel Vapors    |
			+-----------+-------------------------+
			*/
			if(className.equalsIgnoreCase("gov.epa.otaq.moves.master.implementation.ghg.OperatingModeDistributionGenerator")) {
				processIDs = "1,9";
			} else if(className.equalsIgnoreCase("gov.epa.otaq.moves.master.implementation.ghg.StartOperatingModeDistributionGenerator")) {
				processIDs = "2";
			} else if(className.equalsIgnoreCase("gov.epa.otaq.moves.master.implementation.ghg.EvaporativeEmissionsOperatingModeDistributionGenerator")) {
				processIDs = "11,12,13,14";
			} else if(className.equalsIgnoreCase("gov.epa.otaq.moves.master.implementation.ghg.MesoscaleLookupOperatingModeDistributionGenerator")) {
				processIDs = "1,9";
			} else if(className.equalsIgnoreCase("gov.epa.otaq.moves.master.implementation.ghg.AverageSpeedOperatingModeDistributionGenerator")) {
				processIDs = "10";
				polProcessIDs = "11710";
			} else if(className.equalsIgnoreCase("gov.epa.otaq.moves.master.implementation.ghg.LinkOperatingModeDistributionGenerator")) {
				processIDs = "1,9";
			}

			String sql = "select polProcessID"
					+ " from pollutantprocessassoc"
					+ " where processID in (" + processIDs + ")";
			if(polProcessIDs.length() > 0) {
				sql += " and polProcessID in (" + polProcessIDs + ")";
			}
			polProcessIDs = "";
			PreparedStatement statement = null;
			ResultSet rs = null;
			try {
				statement = executionDB.prepareStatement(sql);
				rs = SQLRunner.executeQuery(statement, sql);
				while(rs.next()) {
					if(polProcessIDs.length() > 0) {
						polProcessIDs += ",";
					}
					polProcessIDs += rs.getString(1);
				}
				rs.close();
				rs = null;
				statement.close();
				statement = null;
			} catch(SQLException e) {
				Logger.logSqlError(e,"Unable to query PollutantProcessAssoc",sql);
			} finally {
				if(rs != null) {
					try {
						rs.close();
					} catch(Exception e) {
						// Nothing to do here
					}
				}
				if(statement != null) {
					try {
						statement.close();
					} catch(Exception e) {
						// Nothing to do here
					}
				}
			}
			if(polProcessIDs.length() <= 0) {
				polProcessIDs = "0"; // safeguard in case nothing was found
			}
			whereClause = "polProcessID in (" + polProcessIDs + ")";
		}
		return whereClause;
	}
	
	// Parses Manifest context strings
	private int getValue(String str, String id) {
		int index = str.indexOf("|" + id + ":");
		int pipeIndex = str.indexOf("|", index + 1);
		
		String s = str.substring(index + id.length() + 2, pipeIndex);

		if(NumberUtils.isNumber(s)) {
			return Integer.valueOf(s);
		}

		return 0;
	}
}
