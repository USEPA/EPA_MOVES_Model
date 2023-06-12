/**************************************************************************************************
 * @(#)BundleUtilities.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.io.*;
import java.sql.*;
import java.util.*;

/**
 * Utility functions for building bundles to be processed by workers.
 *
 * @author		Wesley Faler, Jarrod Brown
 * @version		2020 March 05
**/
public class BundleUtilities {
	/** true when tables should NOT be dropped in the MOVESWorker database **/
	public static final boolean debugTablesOnWorker = false; // true;

	/** cache of fuel year IDs keyed by year ID **/
	static TreeMap<Integer,Integer> fuelYearFromYearCache = null;
	/** cache of fuel region IDs keyed by county ID **/
	static TreeMap<Integer,Integer> fuelRegionFromCountyCache = null;

	/** Clear any cached data. Use before any run. **/
	public static void resetCachedData() {
		fuelYearFromYearCache = null;
		fuelRegionFromCountyCache = null;
	}

	/** Populate the cache of fuel year given a year **/
	private static void buildFuelYearFromYearCache() {
		if(fuelYearFromYearCache != null) {
			return;
		}
		fuelYearFromYearCache = new TreeMap<Integer,Integer>();
		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			sql = "select yearID, fuelYearID from year";
			query.open(db,sql);
			while(query.rs.next()) {
				int year = query.rs.getInt(1);
				int fuelYear = query.rs.getInt(2);
				fuelYearFromYearCache.put(Integer.valueOf(year),Integer.valueOf(fuelYear));
			}
			query.close();
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to build year and fuel year relationship",sql);
		} finally {
			query.onFinally();
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
		}
	}

	/* 
		Retrieve the fuelRegionID given the countyID and yearID. The fuelRegionID may change between years,
		requiring this function to be run with every context change.
	*/
	private static int getRegionFromCountyAndYear(int countyID, int yearID) {
		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		int regionID = 0;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			sql = "select regionID from regionCounty where regionCodeID=1 and countyID="+countyID+" and fuelYearID="+yearID;
			query.open(db,sql);
			while(query.rs.next()) {
				regionID = query.rs.getInt(1);
			}
			query.close();
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to build county/region/year relationship",sql);
		} finally {
			query.onFinally();
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
		}

		return regionID;
	}

	/**
	 * Utility function to intelligently read large EmissionCalculator-centric SQL
	 * script files.  The script files are divided into sections using SQL comments:<br>
	 * <table>
	 * 	<tr><td>-- Section Test</td></tr>
	 * 	<tr><td>-- SQL code here</td></tr>
	 * 	<tr><td>-- End Section Test</td></tr>
	 * </table><br>
	 * The system accepts a set of sections of SQL that should be read from the file and
	 * otherwise processed (see the enabledSectionNames parameter).  Sections may be
	 * nested within sections and may appear more than once within a script file.  This
	 * allows sections of SQL pertinent to a pollutant to be turned on and off within
	 * sections that are pertinent to a process.<br>
	 * As it is read, the SQL itself is examined for text replacements such as the
	 * current year and county.<br>
	 * Once the SQL for the desired sections has been found and had textual replacements
	 * performed, it is examined and placed into the appropriate parts of the SQLForWorker
	 * object which is a major internal object of the EmissionCalculator class.
	 * @param context the current MasterLoopContext that is being handled.  From this
	 * context, several standard text replacements are enabled, each replaced with the
	 * value of the Java expression of the same text:
	 * <table>
	 * 	<tr><th>Text</th><th>Description</th></tr>
	 *	<tr><td>##create.tablename##</td><td>"CREATE TABLE IF NOT EXISTS tablename..." based on
	 * MOVESExecution database's schema</td></tr>
	 * 	<tr><td>##context.year##</td><td>yearID of the context's year</td></tr>
	 * 	<tr><td>##context.monthID##</td><td>database monthID of the context's month</td></tr>
	 * 	<tr><td>##context.dayID##</td><td>database dayID of the context's month</td></tr>
	 * 	<tr><td>##context.hourID##</td><td>database hourID of the context's hour</td></tr>
	 * 	<tr><td>##context.iterLocation.stateRecordID##</td><td>identifies the current State
	 			</td></tr>
	 * 	<tr><td>##context.iterLocation.countyRecordID##</td><td>identifies the current County
	 			</td></tr>
	 * 	<tr><td>##context.iterLocation.zoneRecordID##</td><td>identifies the current Zone</td></tr>
	 * 	<tr><td>##context.iterLocation.linkRecordID##</td><td>identifies the current Link</td></tr>
	 * 	<tr><td>##context.iterLocation.roadTypeRecordID##</td><td>identifies the current Road Type
	 *			</td></tr>
	 * 	<tr><td>##context.iterProcess.databaseKey##</td><td>identifies the current
	 *  		EmissionProcess</td></tr>
	 * </table><br>
	 * @param replacements additional textual replacements that should be made to the SQL
	 * statements in the file.  Refer to StringUtilities.doReplacements(...) for more details.
	 * @param sqlScriptFilePath path and file name of the SQL script to be processed
	 * @param enabledSectionNames names of sections within the SQL script that should be
	 * handled.  Lines in sections outside of these enabled ones are not processed and
	 * put into sqlForWorker.
	 * @param sqlForWorker a SQLforWorker object that should be populated with the SQL
	 * in the script file
	 * @param shouldSaveData true if data should be preserved on worker machines
	 * @param timerName optional name of the timer used for all commands, may be null but never empty
	 * @return true if the script file was found and parsed.  This does not indicate that
	 * the SQL contained therein is valid in any way.
	**/
	public static boolean readAndHandleScriptedCalculations(MasterLoopContext context,
			TreeMapIgnoreCase replacements,String sqlScriptFilePath,
			TreeSetIgnoreCase enabledSectionNames,SQLForWorker sqlForWorker,
			boolean shouldSaveData,
			String timerName) {
		buildFuelYearFromYearCache();

		// create a DefaultDataMaker object to include default values in the worker SQLs
		DefaultDataMaker defaultDataMaker = new DefaultDataMaker();
		// Add standard replacement values
		replacements.put("##context.year##",Integer.valueOf(context.year).toString());
		replacements.put("##context.monthID##",Integer.valueOf(context.monthID).toString());
		replacements.put("##context.dayID##",Integer.valueOf(context.dayID).toString());
		replacements.put("##context.hourID##",Integer.valueOf(context.hourID).toString());
		replacements.put("##context.iterLocation.stateRecordID##",
				Integer.valueOf(context.iterLocation.stateRecordID).toString());
		replacements.put("##context.iterLocation.countyRecordID##",
				Integer.valueOf(context.iterLocation.countyRecordID).toString());
		replacements.put("##context.iterLocation.zoneRecordID##",
				Integer.valueOf(context.iterLocation.zoneRecordID).toString());
		replacements.put("##context.iterLocation.linkRecordID##",
				Integer.valueOf(context.iterLocation.linkRecordID).toString());
		replacements.put("##context.iterLocation.roadTypeRecordID##",
				Integer.valueOf(context.iterLocation.roadTypeRecordID).toString());
		String processID = Integer.valueOf(context.iterProcess.databaseKey).toString();
		replacements.put("##context.iterProcess.databaseKey##",processID);
		replacements.put("##context.allCurrentProcesses##",context.getAllProcessesCSV());

		OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
				ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection();
		enabledSectionNames.add("WithRegClassID");

		String tText = "0"; // a default that won't break SQL
		Integer t = getRegionFromCountyAndYear(Integer.valueOf(context.iterLocation.countyRecordID),Integer.valueOf(context.year));
		if(t != null) {
			tText = t.toString();
		}
		replacements.put("##context.fuelRegionID##",tText);

		tText = "" + context.year; // a default that won't break SQL
		t = fuelYearFromYearCache.get(Integer.valueOf(context.year));
		if(t != null) {
			tText = t.toString();
		}
		replacements.put("##context.fuelYearID##",tText);

		/*
		Logger.log(LogMessageCategory.INFO,"Begin context replacement values:");
		for(String k : replacements.keySet()) {
			String v = (String)replacements.get(k);
			if(v != null) {
				Logger.log(LogMessageCategory.INFO,k + " = " + v);
			}
		}
		Logger.log(LogMessageCategory.INFO,"End of context replacement values.");
		*/

		// Add replacement values for the CREATE TABLE IF NOT EXISTS statements of the
		// Execution database
		Set<String> tableNames = DatabaseConnectionManager.executionDatabaseCreateTableStatements.keySet();
		for(Iterator<String> i =tableNames.iterator();i.hasNext();) {
			String tableName = (String)i.next();
			String statement = DatabaseConnectionManager.getExecutionCreateTableStatement(tableName);
			if(SQLRunner.allowInnoDB) {
				statement = StringUtilities.replace(statement,"ENGINE=MyISAM","ENGINE=InnoDB");
			}
			replacements.put("##create." + tableName + "##",statement);

			if(SQLRunner.allowInnoDB) {
				statement = StringUtilities.replace(statement,"ENGINE=InnoDB","ENGINE=MEMORY");
			} else {
				statement = StringUtilities.replace(statement,"ENGINE=MyISAM","ENGINE=MEMORY");
			}
			replacements.put("##memory.create." + tableName + "##",statement);
		}

		enabledSectionNames.add("Create Remote Tables for Extracted Data");
		enabledSectionNames.add("Extract Data");
		enabledSectionNames.add("Local Data Removal");
		enabledSectionNames.add("Processing");
		enabledSectionNames.add("Cleanup");
		enabledSectionNames.add("Final Cleanup");

		enabledSectionNames.add("Process" + processID);

		if(ExecutionRunSpec.theExecutionRunSpec != null) {
			if(ExecutionRunSpec.theExecutionRunSpec.getModelScale() == ModelScale.MESOSCALE_LOOKUP) {
				enabledSectionNames.add("Rates");
			} else if(ExecutionRunSpec.theExecutionRunSpec.getModelScale() == ModelScale.MACROSCALE) {
				enabledSectionNames.add("Inventory");
			}
		}

		if(ExecutionRunSpec.theExecutionRunSpec != null) {
			if(ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection().
				estimateUncertainty) {
				enabledSectionNames.add("Uncertain");
			} else {
				enabledSectionNames.add("Certain");
			}
		} else {
			enabledSectionNames.add("Certain");
		}

		// Read the file into memory, one comment or semi-colon terminated statement per String
		LinkedList<String> sqlFromFileRaw = new LinkedList<String>();
		if(!FileUtilities.appendSQLScriptToList(sqlScriptFilePath, sqlFromFileRaw)) {
			return false;
		}
		LinkedList<String> sqlFromFile = new LinkedList<String>();
		for(Iterator<String> i=sqlFromFileRaw.iterator();i.hasNext();) {
			String line = i.next();
			line = ExecutionRunSpec.theExecutionRunSpec.findAndConvertModelYearMapping(line);
			SQLMacroExpander.expandAndAdd(line,sqlFromFile);
		}
		// Preprocess the SQL doing the prescribed replacements on enabled sections of statements
		Stack<Boolean> sectionStatus = new Stack<Boolean>();
		sectionStatus.push(Boolean.TRUE);
		boolean isInEnabledSection = true;

		LinkedList<String> replacedSQL = new LinkedList<String>();
		for(Iterator<String> i=sqlFromFile.iterator();i.hasNext();) {
			String sql = (String)i.next();
			sql = sql.trim();
			if(sql.length() > 0) {
				if(sql.startsWith("-- Section")) {
					if(isInEnabledSection) {
						String sectionName = sql.substring(10).trim();
						if(enabledSectionNames.contains(sectionName)) {
							isInEnabledSection = true;
						} else {
							isInEnabledSection = false;
						}
					}
					sectionStatus.push(Boolean.valueOf(isInEnabledSection));
					if(isInEnabledSection) {
						replacedSQL.add(sql);
					}
				} else if(sql.startsWith("-- End Section")) {
					if(isInEnabledSection) {
						replacedSQL.add(sql);
					}
					sectionStatus.pop();
					isInEnabledSection = ((Boolean)sectionStatus.peek()).booleanValue();
				} else if(isInEnabledSection) {
					replacedSQL.add(StringUtilities.doReplacements(sql,replacements));
				}
			}
		}
		sqlFromFile = null;

		// Find and handle data in the sections. For example, in "Extract Data" section,
		// the statements should go into sqlForWorker.dataExportSQL.  They should also
		// be examined for file references since those should be turned into "LOAD DATA"
		// statements in sqlForWorker.processingSQL.  The file names will also be table
		// names that should be put into "DROP TABLE IF EXISTS" statements in
		// sqlForWorker.cleanupSQL.
		boolean isInRemoteTableCreationSection = false;
		boolean isInExtractDataSection = false;
		boolean isInLocalDataRemovalSection = false;
		boolean isInProcessingSection = false;
		boolean isInRemoteCleanupSection = false;
		boolean isInRemoteFinalCleanupSection = false;

		for(Iterator<String> i=replacedSQL.iterator();i.hasNext();) {
			String sql = (String)i.next();
			if(sql.startsWith("-- Section Create Remote Tables for Extracted Data")) {
				isInRemoteTableCreationSection = true;
				if(!CompilationFlags.USE_WORKER_TIMING_DETAILS || timerName == null) {
					continue;
				}
				sql = "starttimer " + timerName + ";";
			} else if(sql.startsWith("-- End Section Create Remote Tables for Extracted Data")) {
				isInRemoteTableCreationSection = false;
				continue;
			} else if(sql.startsWith("-- Section Extract Data")) {
				isInExtractDataSection = true;
				continue;
			} else if(sql.startsWith("-- End Section Extract Data")) {
				isInExtractDataSection = false;

				defaultDataMaker.determineAllTablesToExtractAndCreate();
				LinkedList<String> extraSQL = null;

				extraSQL = defaultDataMaker.getTableCreationSQL();
				if(!extraSQL.isEmpty()) {
					for(Iterator<String> j=extraSQL.iterator(); j.hasNext(); ){
						sql = (String)j.next();
						sql = StringUtilities.doReplacements(sql,replacements);
						sqlForWorker.processingSQL.add(sql);
					}
				}
				extraSQL = defaultDataMaker.getRemoteProcessingSQL();
				if(!extraSQL.isEmpty()) {
					for(Iterator<String> j=extraSQL.iterator(); j.hasNext(); ) {
						sql = (String)j.next();
						sql = StringUtilities.doReplacements(sql,replacements);
						sqlForWorker.processingSQL.add(sql);
					}
				}
				extraSQL = defaultDataMaker.getRemoteCleanupSQL();
				if(!extraSQL.isEmpty()) {
					for(Iterator<String> j=extraSQL.iterator(); j.hasNext(); ) {
						sql = (String)j.next();
						sql = StringUtilities.doReplacements(sql,replacements);
						sqlForWorker.cleanupSQL.add(sql);
					}
				}
				extraSQL = defaultDataMaker.getDataExtractionSQL();
				if(!extraSQL.isEmpty()) {
					for(Iterator<String> j=extraSQL.iterator(); j.hasNext(); ) {
						sql = (String)j.next();
						sql = StringUtilities.doReplacements(sql,replacements);
						SQLMacroExpander.expandAndAdd(sql,sqlForWorker.dataExportSQL);
						//sqlForWorker.dataExportSQL.add(sql);
					}
				}
				continue;
			} else if(sql.startsWith("-- Section Local Data Removal")) {
				isInLocalDataRemovalSection = true;
				continue;
			} else if(sql.startsWith("-- End Section Local Data Removal")) {
				isInLocalDataRemovalSection = false;
				continue;
			} else if(sql.startsWith("-- Section Processing")) {
				isInProcessingSection = true;
				if(!CompilationFlags.USE_WORKER_TIMING_DETAILS || timerName == null) {
					continue;
				}
				sql = "starttimer " + timerName + ";";
			} else if(sql.startsWith("-- End Section Processing")) {
				isInProcessingSection = false;
				continue;
			} else if(sql.startsWith("-- Section Cleanup")) {
				isInRemoteCleanupSection = true;
				if(!CompilationFlags.USE_WORKER_TIMING_DETAILS || timerName == null) {
					continue;
				}
				sql = "starttimer " + timerName + ";";
			} else if(sql.startsWith("-- End Section Cleanup")) {
				isInRemoteCleanupSection = false;
				continue;
			} else if(sql.startsWith("-- Section Final Cleanup")) {
				isInRemoteFinalCleanupSection = true;
				continue;
			} else if(sql.startsWith("-- End Section Final Cleanup")) {
				isInRemoteFinalCleanupSection = false;
				if(CompilationFlags.USE_WORKER_TIMING_DETAILS && timerName != null) {
					sql = "starttimer " + timerName + ";";
					if(!BundleUtilities.debugTablesOnWorker && !shouldSaveData) {
						sqlForWorker.cleanupSQL.add(sql); // put at the end so it will be at the beginning in the final file.
					}
				}
				continue;
			}

			if(isInRemoteTableCreationSection) {
				sqlForWorker.processingSQL.add(sql);
			} else if(isInExtractDataSection) {
				int tableNameStart = sql.indexOf("##");
				if(tableNameStart >= 0) {
					int tableNameEnd = sql.lastIndexOf("##");
					if(tableNameEnd >= 0) {
						String tableName = sql.substring(tableNameStart+2,tableNameEnd);
						defaultDataMaker.addTableExtractedByScript(tableName);
						sqlForWorker.dataExportSQL.add(sql);
						if(!BundleUtilities.debugTablesOnWorker && !shouldSaveData) {
							sqlForWorker.cleanupSQL.add(0,"DROP TABLE IF EXISTS " + tableName);
						}
						sqlForWorker.processingSQL.add(
								"LOAD DATA INFILE '##" + tableName + "##' INTO TABLE "
								+ tableName);
						sqlForWorker.processingSQL.add("ANALYZE TABLE " + tableName);
					} else {
						sqlForWorker.dataExportSQL.add(sql);
					}
				} else {
					sqlForWorker.dataExportSQL.add(sql);
				}
			} else if(isInLocalDataRemovalSection) {
				sqlForWorker.localDataRemovalSQL.add(sql);
			} else if(isInProcessingSection) {
				sqlForWorker.processingSQL.add(sql);
			} else if(isInRemoteCleanupSection) {
				if(!BundleUtilities.debugTablesOnWorker && !shouldSaveData) {
					sqlForWorker.processingSQL.add(sql);
				}
			} else if(isInRemoteFinalCleanupSection) {
				if(!BundleUtilities.debugTablesOnWorker && !shouldSaveData) {
					sqlForWorker.cleanupSQL.add(0,sql);
				}
			}
		}

		return true;
	}
}
