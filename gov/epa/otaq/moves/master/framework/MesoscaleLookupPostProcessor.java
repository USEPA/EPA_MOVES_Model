/**************************************************************************************************
 * @(#)MesoscaleLookupPostProcessor.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.io.*;
import java.sql.*;
import java.util.*;

/**
 * Create the Mesoscale lookup output tables.
 *
 * @author		Wes Faler
 * @version		2013-10-21
**/
public class MesoscaleLookupPostProcessor extends IntegratedPostProcessor {
	/**
	 * Default constructor
	**/
	public MesoscaleLookupPostProcessor() {
	}

	/**
	 * Process recently imported records in the database. The specifed database will only have
	 * newly imported records and won't have records from previous imports which have already
	 * been moved to the output database.  The data is stored in the TemporaryOutputImport table.
	 * @param database a Connection to a database containing the TemporaryOutputImport table
	**/
	public void execute(Connection database) {
		// Nothing to do here
	}

	/**
	 * Process the MOVESOutput table in the output database.
	 * @param outputDatabase a Connection to the current output database
	 * @param executionDatabase a Connection to the current execution database
	 * @param isBeforeUnitConversion true if units have not yet been set to the user
	 * supplied values.  false after units have been converted and other aggregations
	 * performed.
	**/
	public void doFinalPostProcessingOnOutputDatabase(Connection outputDatabase,
			Connection executionDatabase, boolean isBeforeUnitConversion) {
		if(isBeforeUnitConversion) {
			return; // nothing to do until units have been converted, then we
					// can make the output emission rate tables.
		}
		ArrayList<String> messages = new ArrayList<String>();
		String mainDatabaseName = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()].databaseName;
		File scriptFile = new File(CompilationFlags.DO_RATES_FIRST? "database/RatePostProcessorRatesFirst.sql" : "database/RatePostProcessor.sql");
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			mainDatabaseName = executionDatabase.getCatalog();

			sql = "create table if not exists rateTempMessages "
					+ "( message varchar(1000) not null )";
			SQLRunner.executeSQL(outputDatabase,sql);
			sql = "truncate table rateTempMessages";
			SQLRunner.executeSQL(outputDatabase,sql);

			Logger.log(LogMessageCategory.INFO,"Analyzing mesoscale output tables");
			sql = "analyze table MOVESOutput";
			SQLRunner.executeSQL(outputDatabase,sql);
			sql = "analyze table MOVESActivityOutput";
			SQLRunner.executeSQL(outputDatabase,sql);

			// Convert NULL values in MOVESOutput and MOVESActivityOutput to 0's so they can be joined.
			Logger.log(LogMessageCategory.INFO,"Zeroing null values in rate output tables");
			String[] tables = {
				"MOVESOutput", "MOVESActivityOutput"
			};
			String[] zeroColumns = {
				"yearID", "monthID", "dayID", "hourID", "stateID", "countyID",
				"zoneID", "sourceTypeID", "regClassID", "fuelTypeID", "modelYearID"
			};
			for(int t=0;t<tables.length;t++) {
				// Build one SQL statement for all columns.  This minimizes the passes MySQL must make
				// through these unindexed tables.
				sql = "update " + tables[t] + " set ";
				for(int c=0;c<zeroColumns.length;c++) {
					if(c > 0) {
						sql += ", ";
					}
					sql += zeroColumns[c] + "=ifnull(" + zeroColumns[c] + ",0)";
				}
				sql += ", SCC=ifnull(SCC,'')";
				sql += " where MOVESRunID=" + MOVESEngine.theInstance.getActiveRunID();
				SQLRunner.executeSQL(outputDatabase,sql);
			}

			// Run the script
			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			replacements.put("##mainDatabase##",mainDatabaseName);
			replacements.put("##runID##",""+MOVESEngine.theInstance.getActiveRunID());
			replacements.put("##scenarioID##",ExecutionRunSpec.getRunSpec().scenarioID);
			replacements.put("##isProjectDomain##",
					ExecutionRunSpec.getRunSpec().domain == ModelDomain.PROJECT?"1":"0");
			replacements.put("##isSCC##",
					ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection().onRoadSCC?"1":"0");

			try {
				DatabaseUtilities.executeScript(outputDatabase,scriptFile,replacements);
			} catch(Exception e) {
				messages.add("ERROR: Unable to execute rate script");
				Logger.logError(e,"Unable to execute rate script " + scriptFile.getName());
			}

			if(messages != null) {
				// Retrieve the results from rateTempMessages
				sql = "select message from rateTempMessages";
				query.open(outputDatabase,sql);
				TreeSetIgnoreCase messagesAlreadySeen = new TreeSetIgnoreCase();
				while(query.rs.next()) {
					String m = query.rs.getString(1);
					if(m != null && m.length() > 0) {
						if(!messagesAlreadySeen.contains(m)) {
							messagesAlreadySeen.add(m);
							messages.add(m);
						}
					}
				}
				query.close();
			}

			// Remove any messages from the database.  If an error occured and the code never
			// gets here, leave the messages in the database for debugging purposes.  They
			// will be cleared if another rate run is performed.
			sql = "drop table if exists rateTempMessages";
			SQLRunner.executeSQL(outputDatabase,sql);

			Logger.log(LogMessageCategory.INFO,"Done building rate output tables");
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to create rate output tables",sql);
		}
	}
}
