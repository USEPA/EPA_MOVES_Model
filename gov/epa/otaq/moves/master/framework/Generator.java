/**************************************************************************************************
 * @(#)Generator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.io.*;
import java.sql.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;

/**
 * Abstract base class for MOVES Generators. Generators fall between InternalControlStrategys and
 * Calculators in the processing flow of MOVES data. Conceptually, they perform the "simulation"
 * portion of a simulation run. They simulate, or model, the activity of vehicles over given
 * time periods and locations. Generators perform calculations that cannot be easily divided up
 * and passed on to worker threads for seperate processing.
 *
 * @author		Wesley Faler
 * @version		2014-10-14
**/
public abstract class Generator implements MasterLoopable {
	/** The EmissionProcess that this Generator applies to **/
	EmissionProcess targetProcess;

	/** True if MasterLoopContext.beginCreatingDeferredBundles has been called **/
	boolean didCallBeginCreatingDeferredBundles = false;
	/** Used during iteration when building the list of export files. **/
	File currentExportFilePath = null;

	/**
	 * Create a bundle for execution by a worker.  Prior to calling this
	 * routine, the Generator should have called MasterLoopContext's
	 * beginCreatingDeferredBundles().
	 * @param context The MasterLoopContext that applies to this execution.
	 * @param tableNamesToRetrieve set of CMIT tables to be returned to the master
	**/
	public void buildBundle(MasterLoopContext context, String[] tableNamesToRetrieve) {
		if(!didCallBeginCreatingDeferredBundles) {
			didCallBeginCreatingDeferredBundles = true;
			context.beginCreatingDeferredBundles();
		}
		File temporaryFolderPath = null;
		PrintWriter classNameFileWriter = null;
		PrintWriter workerSQLFileWriter = null;
		PrintWriter flagsFileWriter = null;
		Connection executionDB = null;
		BundleManifest manifest = null;

		try {
			temporaryFolderPath = FileUtilities.createTemporaryFolder(
					null, "GeneratorTemp");
			if(temporaryFolderPath == null) {
				return;
			}

			// Open the classname.txt and worker.sql PrintWriters
			File classNameFilePath = new File(temporaryFolderPath, "classname.txt");
			File workerSQLFilePath = new File(temporaryFolderPath, "worker.sql");
			File flagsFilePath = new File(temporaryFolderPath, "flags.txt");
			manifest = new BundleManifest();
			manifest.copyFrom(MOVESEngine.theInstance.masterFragment);
			manifest.context = context.toBundleManifestContext();
			manifest.contextForHumans = context.toBundleManifestContextForHumans();
			for(int i=0;i<tableNamesToRetrieve.length;i++) {
				manifest.tablesToRetrieve.add(tableNamesToRetrieve[i]);
			}
			manifest.bundleNumber = MOVESEngine.theInstance.bundler.getNextQueueIDCore();

			try {
				classNameFileWriter = new PrintWriter(new BufferedWriter(
						new OutputStreamWriter(new FileOutputStream(classNameFilePath))));
				workerSQLFileWriter = new PrintWriter(new BufferedWriter(
						new OutputStreamWriter(new FileOutputStream(workerSQLFilePath))));
				flagsFileWriter = new PrintWriter(new BufferedWriter(
						new OutputStreamWriter(new FileOutputStream(flagsFilePath))));
			} catch(FileNotFoundException e) {
				/**
				 * @explain An error occurred while creating files to be bundled and sent to
				 * the workers.  Check your permissions to the shared work folder (see the
				 * MOVESConfiguration.txt) file, ensuring there is space and sufficient rights
				 * to create files within it.
				**/
				Logger.logError(e, "Create worker files failed.");
				return;
			}

			// Get db connection
			try {
				executionDB = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.EXECUTION);
			} catch(Exception e) {
				/** @explain Unable to obtain a connection to the MOVESExecution database. **/
				Logger.logError(e,"Failed to checkout the MOVES Execution database connection "+
						"needed to perform Generator Calculations.");
				return;
			}

			LinkedList<File> tableDumpFilePaths = new LinkedList<File>();

			// Invoke this calculator. This will recursively invoke chained calculators.
			SQLForWorker masterSQLForWorker = new SQLForWorker();
			generatorExecution(classNameFileWriter
					, temporaryFolderPath, executionDB
					, tableDumpFilePaths, context, masterSQLForWorker);

			// Add the processing logic to the "worker.sql" file
			for(Iterator<String> iter = masterSQLForWorker.processingSQL.iterator();
					iter.hasNext(); ) {
				workerSQLFileWriter.println(iter.next().toString());
			}
			/*
			if(!BundleUtilities.debugTablesOnWorker && !ExecutionRunSpec.shouldSaveData(this)) {
				// Add the data cleanup logic to the "worker.sql" file
				for(Iterator<String> iter = masterSQLForWorker.cleanupSQL.iterator();
						iter.hasNext(); ) {
					workerSQLFileWriter.println(iter.next().toString());
				}
			} else {
				flagsFileWriter.println("savedata");
			}*/

			classNameFileWriter.close();
			classNameFileWriter = null;
			workerSQLFileWriter.close();
			workerSQLFileWriter = null;
			flagsFileWriter.close();
			flagsFileWriter = null;

			try {
				manifest.writeToFolder(temporaryFolderPath);
			} catch(IOException e) {
				/**
				 * @explain An error occurred while creating the manifest file placed
				 * into bundles sent to workers.
				**/
				Logger.logError(e, "Create manifest file failed.");
				return;
			}

			tableDumpFilePaths.add(classNameFilePath);
			tableDumpFilePaths.add(workerSQLFilePath);
			tableDumpFilePaths.add(flagsFilePath);
			tableDumpFilePaths.add(manifest.getManifestFile(temporaryFolderPath));

			MOVESEngine.theInstance.bundler.bundleData(tableDumpFilePaths,DistributedWorkFilePurpose.GENERATOR,""+manifest.bundleNumber);
			MOVESEngine.theInstance.notifyListeners();

			// Remove local data from the database now that it has been extracted and sent
			// off to the workers
			for(Iterator<String> iter = masterSQLForWorker.localDataRemovalSQL.iterator();
					iter.hasNext(); ) {
				String sql = iter.next().toString();
				try {
					SQLRunner.executeSQL(executionDB,sql);
				} catch(SQLException e) {
					Logger.logSqlError(e, "Error removing generated data from the execution database.", sql);
				}
			}
		} finally {
			if(classNameFileWriter != null) {
				classNameFileWriter.close();
			}
			if(workerSQLFileWriter != null) {
				workerSQLFileWriter.close();
			}
			if(flagsFileWriter != null) {
				flagsFileWriter.close();
			}
			if(temporaryFolderPath != null) {
				FileUtilities.deleteTemporaryFolder(temporaryFolderPath);
			}
			if(executionDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, executionDB);
			}
		}
	}

	/**
	 * Builds a SQL statements for a distributed worker to execute. This is called by
	 * EmissionCalculator.executeLoop. Implementations of this method should contain uncertainty
	 * logic when UncertaintyParameters specifies that this mode is enabled.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting SQL lists as an SQLForWorker object.
	**/
	public SQLForWorker doExecute(MasterLoopContext context) {
		return null;
	}

	/**
	 * Detects file names encoded in a specific escape sequence: "##<file name>##"
	 * This replaces the escape sequence with the full path on the worker system.
	 * @param originalLine The original line with potential escape sequences.
	 * @param temporaryFolderPath Indicates the temporary directory to use.
	 * @return The resulting line with all escape sequences replaced with full paths.
	 * @throws IOException If there is a file I/O error.
	**/
	public String performFilePathReplacements(String originalLine,
			File temporaryFolderPath) throws IOException {
		while (true) {
			final String escapeText = "##";

			int firstEscapeTextIndex = originalLine.indexOf(escapeText);
			if(firstEscapeTextIndex < 0) {
				break;
			}
			int secondEscapeTextIndex = originalLine.indexOf(escapeText,
				firstEscapeTextIndex + escapeText.length());
			if(secondEscapeTextIndex < 0) {
				break;
			}

			String fileName = originalLine.substring(
				firstEscapeTextIndex + escapeText.length(), secondEscapeTextIndex);
			currentExportFilePath = new File(temporaryFolderPath, fileName);
			// Replace backslashes with forward slashes for mysql
			fileName = currentExportFilePath.getCanonicalPath().replace('\\', '/');
			originalLine = originalLine.substring(0, firstEscapeTextIndex) + fileName
					+ originalLine.substring(secondEscapeTextIndex + escapeText.length());
		}

		return originalLine;
	}

	/**
	 * Handles the results from the main processing routine for a single generator.
	 * @param classNameFileWriter An opened PrintWriter to "classname.txt" which is intended
	 * for future use where compiled Java .CLASS files are sent to workers for execution.
	 * @param temporaryFolderPath The path to store table dump files.
	 * @param executionDB An opened Connection to the Execution Database.
	 * @param tableDumpFilePaths Adds any table dump files (as File objects) to this LinkList.
	 * These are files that will get sent to the worker.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @param masterSQLForWorker central set of SQL for the worker.  Generators should add
	 * to this object.
	**/
	void generatorExecution(
			PrintWriter classNameFileWriter,
			File temporaryFolderPath, Connection executionDB, LinkedList<File> tableDumpFilePaths,
			MasterLoopContext context, SQLForWorker masterSQLForWorker) {
		final String eol = System.getProperty("line.separator");
		// Get the sql for the worker
		boolean shouldDisplayEmptyExtractedDataQueries = false;
		SQLForWorker sqlForWorker = doExecute(context);
		if(sqlForWorker == null) {
			// This is expected from calculators that skip looping levels, such as
			// EnergyConsumptionCalculator that skips the link level for Starts and
			// Extended Idle.
			return;
		}

		String sql = "";
		// Generate any export files
		for(Iterator<String> iter = sqlForWorker.dataExportSQL.iterator(); iter.hasNext(); ) {
			try {
				sql = (String)iter.next();
				if(sql.startsWith("--")) {
					continue;
				}
				// Replace filename.ext with a known path, this also sets the
				// currentExportFilePath member
				sql = performFilePathReplacements(sql, temporaryFolderPath);
				// Generate the table dump and add the file to the export bundler
				SQLRunner.executeSQL(executionDB,sql);
				if(currentExportFilePath != null) {
					tableDumpFilePaths.add(currentExportFilePath);
					if(shouldDisplayEmptyExtractedDataQueries &&
							currentExportFilePath.length() < 1) {
						Logger.log(LogMessageCategory.INFO, "Generated empty file: " +
								currentExportFilePath.getPath()+"."+eol+sql);
					}
					currentExportFilePath = null;
				}
			} catch(SQLException e) {
				Logger.logSqlError(e,"A Generator encountered an SQL exception while " +
						"exporting data using: ", sql);
			} catch(IOException e) {
				Logger.logError(e,"A Generator encountered an IO exception while " +
						"exporting data to " + currentExportFilePath.getPath());
			}
		}

		// Add the local cleanup logic to the master set of SQL
		for(Iterator<String> iter = sqlForWorker.localDataRemovalSQL.iterator(); iter.hasNext(); ) {
			// Ensure only one semicolon terminates the sql statement, will get errors
			// otherwise
			sql = (String)iter.next();
			sql.trim();
			if(sql.startsWith("--")) {
				continue;
			}
			if((sql.length() > 0) && (!StringUtilities.isWhitespace(sql))) {
				// Ensure this statement ends with a semicolon
				if(sql.charAt(sql.length()-1) != ';') {
					sql += ';';
				}
			}
			masterSQLForWorker.localDataRemovalSQL.add(sql);
		}

		// Add the processing logic to the master set of SQL
		for(Iterator<String> iter = sqlForWorker.processingSQL.iterator(); iter.hasNext(); ) {
			// Ensure only one semicolon terminates the sql statement, will get errors
			// otherwise
			sql = (String)iter.next();
			sql.trim();
			if(sql.startsWith("--")) {
				continue;
			}
			if((sql.length() > 0) && (!StringUtilities.isWhitespace(sql))) {
				// Ensure this statement ends with a semicolon
				if(sql.charAt(sql.length()-1) != ';') {
					sql += ';';
				}
			}
			masterSQLForWorker.processingSQL.add(sql);
		}

		// Add the data cleanup logic to the master set of SQL
		for(Iterator<String> iter = sqlForWorker.cleanupSQL.iterator(); iter.hasNext(); ) {
			// Ensure only one semicolon terminates the sql statement, will get errors
			// otherwise
			sql = (String)iter.next();
			sql.trim();
			if(sql.startsWith("--")) {
				continue;
			}
			if((sql.length() > 0) && (!StringUtilities.isWhitespace(sql))) {
				// Ensure this statement ends with a semicolon
				if(sql.charAt(sql.length()-1) != ';') {
					sql += ';';
				}
			}
			// add the new cleanup SQL to the beginning of the master list, so as to
			// create a stack-like effect wherein calculators processed first are
			// cleaned up last.  This allows subsequent calculators access to intermediate
			// results.
			masterSQLForWorker.cleanupSQL.add(0,sql); // add to the beginning
		}
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
	 * @return true if the script file was found and parsed.  This does not indicate that
	 * the SQL contained therein is valid in any way.
	**/
	public boolean readAndHandleScriptedCalculations(MasterLoopContext context,
			TreeMapIgnoreCase replacements,String sqlScriptFilePath,
			TreeSetIgnoreCase enabledSectionNames,SQLForWorker sqlForWorker) {
		boolean shouldSaveData = ExecutionRunSpec.shouldSaveData(Generator.class);
		return BundleUtilities.readAndHandleScriptedCalculations(context,replacements,
				sqlScriptFilePath,enabledSectionNames,
				sqlForWorker,shouldSaveData,getClass().getName());
	}
}
