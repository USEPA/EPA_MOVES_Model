/**************************************************************************************************
 * @(#)EmissionCalculator.java
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
 * This takes data from the Core Model Input tables built by Generator objects and builds work
 * bundles which contain processing logic and all relevant data. These work bundles are processed
 * by a distributed worker system (class RemoteEmissionsCalculator). Supported processes use
 * concrete subclasses of this class.
 *
 * This class supports the ability to "chain" one calculator off of another. This allows the
 * chaining calculator's work and logic to go in the same bundle as the logic from the calculator
 * being chained.
 *
 * @author		Wesley Faler
 * @version		2015-03-17
**/
public abstract class EmissionCalculator implements MasterLoopable, Comparable {
	/**
	 * Calculators that should execute at the same time. SQL built for a distributed worker
	 * should be combined with the SQL from chained calculator objects so that the same worker
	 * processes both. This list can be recursive; chained calculators may have additional
	 * chained calculators themselves.
	**/
	public LinkedList<EmissionCalculator> chainedCalculators = new LinkedList<EmissionCalculator>();

	/** Used during iteration when building the list of export files. **/
	File currentExportFilePath = null;

	/**
	 * Used during iteration to control whether or not display of queries that should extract
	 * data but yield no output records should be done.  This is reset before every call to
	 * doExecute().
	**/
	protected boolean shouldDisplayEmptyExtractedDataQueries = false;

	/**
	 * Default constructor
	**/
	public EmissionCalculator() {
	}

	/**
	 * Calls doExecute for this object and any chained calculators. Builds up a single block of
	 * SQL statements for a distributed worker and passes it to EmissionCalculatorOutboundBundler
	 * for bundling and distribution.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void executeLoop(MasterLoopContext context) {
		// IDEA:  this assumes that any chained calculators specify unique output table dump
		// files (they should), however should add some logic to detect the condition where a
		// table name is repeated throughout the list of chained calculators

		File temporaryFolderPath = null;
		PrintWriter classNameFileWriter = null;
		PrintWriter workerSQLFileWriter = null;
		PrintWriter flagsFileWriter = null;
		Connection executionDB = null;
		BundleManifest manifest = null;

		try {
			temporaryFolderPath = FileUtilities.createTemporaryFolder(null, "EmissionCalculatorTemp");
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
			manifest.tablesToRetrieve.add("MOVESWorkerOutput");
			manifest.tablesToRetrieve.add("MOVESWorkerActivityOutput");
			if(CompilationFlags.DO_RATES_FIRST) {
				manifest.tablesToRetrieve.add("BaseRateOutput");
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
						"needed to perform Emission Calculations.");
				return;
			}

			LinkedList<File> tableDumpFilePaths = new LinkedList<File>();

			try {
				addToManifestAndFiles(context, executionDB, manifest, temporaryFolderPath, tableDumpFilePaths);
			} catch (FileNotFoundException e1) {
				e1.printStackTrace();
				Logger.logError(e1, "Failed add files to file list and information to manifest.");
			}

			// Invoke this calculator. This will recursively invoke chained calculators.
			SQLForWorker masterSQLForWorker = new SQLForWorker();
			calculatorExecution(classNameFileWriter
					, temporaryFolderPath, executionDB
					, tableDumpFilePaths, context, masterSQLForWorker, false);

			// Add the processing logic to the "worker.sql" file
			for(Iterator<String> iter = masterSQLForWorker.processingSQL.iterator();
					iter.hasNext(); ) {
				workerSQLFileWriter.println(iter.next().toString());
			}
			if(!BundleUtilities.debugTablesOnWorker && !ExecutionRunSpec.shouldSaveData(this)) {
				// Add the data cleanup logic to the "worker.sql" file
				for(Iterator<String> iter = masterSQLForWorker.cleanupSQL.iterator();
						iter.hasNext(); ) {
					workerSQLFileWriter.println(iter.next().toString());
				}
			} else {
				flagsFileWriter.println("savedata");
			}
			// Add aggregation SQLs to the "worker.sql" file
			Vector<String> workerSQLs = ExecutionRunSpec.theExecutionRunSpec.workerSQLs;
			for(int i=0; i<workerSQLs.size(); i++) {
				workerSQLFileWriter.println((String) workerSQLs.elementAt(i));
			}

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

			MOVESEngine.theInstance.bundler.bundleData(tableDumpFilePaths,""+manifest.bundleNumber);
			MOVESEngine.theInstance.notifyListeners();

			// Remove local data from the database now that it has been extracted and sent
			// off to the workers
			for(Iterator<String> iter = masterSQLForWorker.localDataRemovalSQL.iterator();
					iter.hasNext(); ) {
				String sql = iter.next().toString();
				try {
					SQLRunner.executeSQL(executionDB,sql);
				} catch(SQLException e) {
					Logger.logSqlError(e, "Error removing generated data from the execution "+
							"database.", sql);
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
				DatabaseConnectionManager.checkInConnection(
					MOVESDatabaseType.EXECUTION, executionDB);
			}
		}
	}

	/**
	 * Handles the results from the main processing routine for a single calculator.
	 * @param classNameFileWriter An opened PrintWriter to "classname.txt" which is intended
	 * for future use where compiled Java .CLASS files are sent to workers for execution.
	 * @param temporaryFolderPath The path to store table dump files.
	 * @param executionDB An opened Connection to the Execution Database.
	 * @param tableDumpFilePaths Adds any table dump files (as File objects) to this LinkList.
	 * These are files that will get sent to the worker.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @param masterSQLForWorker central set of SQL for the worker.  Calculators should add
	 * to this object.
	 * @param isChained true if called as a chained calculator, false if a top-level calculator
	**/
	void calculatorExecution(
			PrintWriter classNameFileWriter,
			File temporaryFolderPath, Connection executionDB, LinkedList<File> tableDumpFilePaths,
			MasterLoopContext context, SQLForWorker masterSQLForWorker,
			boolean isChained) {
		final String eol = System.getProperty("line.separator");
		// Get the sql for the worker
		shouldDisplayEmptyExtractedDataQueries = false;
		SQLForWorker sqlForWorker = doExecute(context);
		if(sqlForWorker == null) {
			// This is expected from calculators that skip looping levels, such as
			// EnergyConsumptionCalculator that skips the link level for Starts and
			// Extended Idle.
			return;
		}

		// Flatten the set of chained calculators, if any.
		boolean hasChainedExternalCalculator = false;
		if(!isChained) {
			if(chainedCalculators.size() > 0) {
				//System.out.println("Flattening the chain...");
				LinkedList<EmissionCalculator> flatChain = new LinkedList<EmissionCalculator>();
				flattenChain(flatChain);
				// Now, no child calculator has a chainedCalculator list with anything in it,
				// so we know each will only be called once and in the desired order.
				chainedCalculators.clear();
				chainedCalculators.addAll(flatChain);

				for(Iterator<EmissionCalculator> i=chainedCalculators.iterator();i.hasNext();) {
					if(i.next() instanceof EmissionCalculatorExternal) {
						hasChainedExternalCalculator = true;
						break;
					}
				}
				/*
				// Dump the chain
				Logger.log(LogMessageCategory.INFO,"***Chain from:" + getClass().getName());
				for(Iterator<EmissionCalculator> i=chainedCalculators.iterator();i.hasNext();) {
					Logger.log(LogMessageCategory.INFO,i.next().getClass().getName());
				}
				Logger.log(LogMessageCategory.INFO,"***End of chain from:" + getClass().getName());
				*/
			}
		}



		String sql = "";
		// Generate any export files
		for(Iterator<String> iter = sqlForWorker.dataExportSQL.iterator(); iter.hasNext(); ) {
			try {
				sql = (String)iter.next();
				if(sql.startsWith("--")) {
					continue;
				}
				currentExportFilePath = ExtractedDataCache.handle(executionDB,sql,temporaryFolderPath);
				if(currentExportFilePath == null) {
					// Replace filename.ext with a known path, this also sets the
					// currentExportFilePath member
					sql = performFilePathReplacements(sql, temporaryFolderPath);
					// Generate the table dump and add the file to the export bundler
					SQLRunner.executeSQL(executionDB,sql);
				}
				if(currentExportFilePath != null) {
					if(!tableDumpFilePaths.contains(currentExportFilePath)) {
						tableDumpFilePaths.add(currentExportFilePath);
					}
					if(shouldDisplayEmptyExtractedDataQueries &&
							currentExportFilePath.length() < 1) {
						Logger.log(LogMessageCategory.INFO, "Generated empty file: " +
								currentExportFilePath.getPath()+"."+eol+sql);
					}
					currentExportFilePath = null;
				}
			} catch(SQLException e) {
				Logger.logSqlError(e,"An EmissionCalculator encountered an SQL exception while " +
						"exporting data using: ", sql);
			} catch(IOException e) {
				Logger.logError(e,"An EmissionCalculator encountered an IO exception while " +
						"exporting data to " + (currentExportFilePath==null?"(null)":currentExportFilePath.getPath()));
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
		if(this instanceof EmissionCalculatorExternal) {
			masterSQLForWorker.processingSQL.add("externalcalculator " + getClass().getSimpleName() + ";");
			for(String extraModule : sqlForWorker.externalModules) {
				masterSQLForWorker.processingSQL.add("externalcalculator " + extraModule + ";");
			}
			if(CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT) {
				if(ExecutionRunSpec.getRunSpec().outputEmissionsBreakdownSelection.fuelSubType) {
					masterSQLForWorker.processingSQL.add("externalcalculator FuelSubType;");
				}
			}
		} else {
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
			if(CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT) {
				if(!isChained && !hasChainedExternalCalculator && ExecutionRunSpec.getRunSpec().outputEmissionsBreakdownSelection.fuelSubType) {
					// The top-level calculator is requested to output by fuelSubType.
					// The external calculator splits output by fuel type into fuel subtype.
					// However, there is no reason that would cause the external calculator
					// to be run. So, add a call to the external calculator just to do the
					// fuel subtype split.
					// Do this at the top-level so chained calculators can work with fuelSubTypeID.
					// Doing so after chained calculators would force a linear split on all
					// calculators, which may not be the case.
					masterSQLForWorker.processingSQL.add("externalcalculator FuelSubType;");
				}
			}
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

		// Add to the "classname.txt" file
		// ... This is reserved for future use in a situation where we want to send .CLASS files
		//     to workers for execution.
		// classNameFileWriter.println(getClass().getName());

		sqlForWorker = null;
		// Recursively invoke chained calculators
		if(!isChained) {
			context.resetChainedProcesses();
			for(Iterator<EmissionCalculator> calcIter = chainedCalculators.iterator();
					calcIter.hasNext(); ) {
				EmissionCalculator emissionCalculator = (EmissionCalculator) calcIter.next();
				emissionCalculator.calculatorExecution(classNameFileWriter,
					temporaryFolderPath, executionDB, tableDumpFilePaths, context,
					masterSQLForWorker,true);
			}
			context.resetChainedProcesses();
		}
	}

	/**
	 * Recursively generate a list of chained calculators without duplicates and such that each
	 * duplicated calculator runs at the latest, not earliest, location it can.  Afterwards,
	 * chainedCalculators is empty.
	 * @param flatChain set of calculators without duplicates
	**/
	void flattenChain(LinkedList<EmissionCalculator> flatChain) {
		LinkedList<EmissionCalculator> callStack = new LinkedList<EmissionCalculator>();
		flattenChain(flatChain,callStack);
	}

	/**
	 * Recursively generate a list of chained calculators without duplicates and such that each
	 * duplicated calculator runs at the latest, not earliest, location it can.  Afterwards,
	 * chainedCalculators is empty.
	 * @param flatChain set of calculators without duplicates
	 * @param callStack sequence of calculators leading to the current invocation
	 * @return false if a circular reference was encountered
	**/
	boolean flattenChain(LinkedList<EmissionCalculator> flatChain,
			LinkedList<EmissionCalculator> callStack) {
		//System.out.println("flattenChain " + getClass().getName());
		if(chainedCalculators == null || chainedCalculators.size() <= 0) {
			//System.out.println("(nothing new for the flat chain)");
			return true;
		}
		if(callStack.contains(this)) {
			// A circular reference has been found. Report it and quit.
			Logger.log(LogMessageCategory.ERROR,
					"Circular calculator chain found in " + getClass().getSimpleName());
			// Log the chain
			Logger.log(LogMessageCategory.INFO,"Chain:");
			for(Iterator i=chainedCalculators.iterator();i.hasNext();) {
				Logger.log(LogMessageCategory.INFO,i.next().getClass().getSimpleName());
			}
			Logger.log(LogMessageCategory.INFO,getClass().getSimpleName());
			// Log the callstack
			Logger.log(LogMessageCategory.INFO,"Callstack:");
			for(Iterator i=callStack.iterator();i.hasNext();) {
				Logger.log(LogMessageCategory.INFO,i.next().getClass().getSimpleName());
			}
			Logger.log(LogMessageCategory.INFO,getClass().getSimpleName());
			return false;
		}
		boolean result = true;
		callStack.add(this);
		for(int pass=0;pass<2;pass++) {
			for(Iterator<EmissionCalculator> i=chainedCalculators.iterator();i.hasNext();) {
				EmissionCalculator c = (EmissionCalculator)i.next();
				// In the first pass, consider only external calculator uses.
				// In the second pass, consider only non-external calculator uses.
				if(c instanceof EmissionCalculatorExternal) {
					if(pass != 0) {
						continue;
					}
				} else {
					if(pass == 0) {
						continue;
					}
				}
				if(flatChain.contains(c)) {
					flatChain.remove(c);
				}
				flatChain.add(c);
				if(!c.flattenChain(flatChain,callStack)) {
					result = false;
					break;
				}
			}
		}
		callStack.remove(this);
		return result;
	}

	/**
	 * Builds a SQL statements for a distributed worker to execute. This is called by
	 * EmissionCalculator.executeLoop. Implementations of this method should contain uncertainty
	 * logic when UncertaintyParameters specifies that this mode is enabled.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting SQL lists as an SQLForWorker object.
	**/
	public abstract SQLForWorker doExecute(MasterLoopContext context);

	/**
	 * Default implementation returns false. Subclasses that always use the same SQL can
	 * override this to return true for performance optimizations.
	 * @return Should return true if the derived class uses constant sql.
	**/
	boolean usesConstantSQL() {
		return false;
	}

	/**
	 * Adds the specified calculator to the list of chained calculators.
	 * @param calculator The EmissionCalculator to add to the list.
	**/
	public void chainCalculator(EmissionCalculator calculator) {
		InterconnectionTracker.recordChain(calculator,this);
		chainedCalculators.add(calculator);
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
			fileName = fileName.toLowerCase(); // standardize on lower-case names
			currentExportFilePath = new File(temporaryFolderPath, fileName);
			// Replace backslashes with forward slashes for mysql
			fileName = currentExportFilePath.getCanonicalPath().replace('\\', '/');
			originalLine = originalLine.substring(0, firstEscapeTextIndex) + fileName
					+ originalLine.substring(secondEscapeTextIndex + escapeText.length());
		}

		return originalLine;
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
		boolean shouldSaveData = ExecutionRunSpec.shouldSaveData(EmissionCalculator.class);
		return BundleUtilities.readAndHandleScriptedCalculations(context,replacements,
				sqlScriptFilePath,enabledSectionNames,
				sqlForWorker,shouldSaveData,getClass().getName());
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
	}

	/**
	* Compares to instances of EmissionCalculator to test if they are in fact the same
	* object.  No deep comparison is done, only a test of the standard Java hash codes.
	* This method is provided as a convenience for chained calculators that wish to
	* use a Set to maintain a collection of unique EmissionCalculator objects.
	* @param o The other EmissionCalculator object to compare to.
 	* @return <0 if this one should go before the other, 0 if they are the same, and >0 if
 	* it should go after the other
	**/
	public int compareTo(Object o) {
		EmissionCalculator other = (EmissionCalculator)o;
		return hashCode() - other.hashCode();
	}

	/**
	 * Add files to be sent to a worker, especially for use with external
	 * models such as Nonroad's Fortran-based EXE.
	 * @param context the current iteration context
	 * @param executionDB the database the simulation is using
	 * @param manifest object to be sent to the worker describing the work bundle
	 * @param temporaryFolderPath location of temporary files for this bundle
	 * @param tableDumpFilePaths all files to be sent to the worker
	**/
	public void addToManifestAndFiles(MasterLoopContext context,
			Connection executionDB, BundleManifest manifest,
			File temporaryFolderPath, List<File> tableDumpFilePaths)
			throws FileNotFoundException {
		// Nothing to do here.
	}
}
