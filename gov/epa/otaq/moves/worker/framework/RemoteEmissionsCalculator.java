/**************************************************************************************************
 * @(#)RemoteEmissionsCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker.framework;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.TreeMap;
import java.util.TreeSet;

import org.apache.commons.lang.math.NumberUtils;

import gov.epa.otaq.moves.common.BundleManifest;
import gov.epa.otaq.moves.common.CompilationFlags;
import gov.epa.otaq.moves.common.DatabaseUtilities;
import gov.epa.otaq.moves.common.DistributedWorkFileName;
import gov.epa.otaq.moves.common.DistributedWorkFileState;
import gov.epa.otaq.moves.common.FileUtilities;
import gov.epa.otaq.moves.common.JARUtilities;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.MOVESThread;
import gov.epa.otaq.moves.common.ParallelSQLRunner;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.common.StringUtilities;
import gov.epa.otaq.moves.common.TreeMapIgnoreCase;
import gov.epa.otaq.moves.master.framework.MOVESEngine;
import gov.epa.otaq.moves.master.gui.MOVESWindow;
//import gov.epa.otaq.moves.master.framework.MasterLoopContext;
import gov.epa.otaq.moves.master.nonroad.NonroadHelper;
import gov.epa.otaq.moves.utils.ApplicationRunner;
import gov.epa.otaq.moves.utils.FileUtil;
import gov.epa.otaq.moves.worker.gui.WorkerGUI;
import gov.epa.otaq.moves.worker.gui.WorkerWindow;

/**
 * MOVES breaks up work into generic work bundles which are handed off to a pool of distributed
 * worker systems. This is the main class of one of these distributed work systems. This class
 * detects available work bundles, processes them, and returns resulting data. The workers are
 * designed to be as generic and as independent of specific logic and processes as possible.
 * This system exists solely to offload resource intensive work from one system to many. On the
 * master side, the bundle files are built by EmissionCalculator objects and are retrieved by
 * EmissionCalculatorInboundUnbundler.
 *
 * The distributed work system handles synchronization issues with file system primitives.
 * Workers rename a given file to an internal name to reserve it.
 *
 * Outline of operations:
 * <ul>
 *	<li>Detects available work bundle file.
 *	<li>Reserves bundle file using a file system rename.
 *	<li>Unbundles the data
 *	<li>Processes the data
 *	<li>Bundles the resulting data
 *	<li>Flag bundle as done and ready for master system processing.
 * </ul>
 *
 * @author		Wesley Faler
 * @author		Sarah Luo
 * @author 		Tim Hull
 * @version		2017-02-01
**/
public class RemoteEmissionsCalculator extends MOVESThread {
	/** Constant name of file containing SQL statement for worker to execute. **/
	public static final String WORKER_SQL_FILE_NAME = "worker.sql";
	/** Constant name of file to write version information to. **/
	public static final String VERSION_FILE_NAME = "WorkerVersion.txt";
	/** Constant name of file containing flags from the master **/
	public static final String FLAGS_FILE_NAME = "flags.txt";
	/** Constant name of file to write errors to. **/
	public static final String ERROR_FILE_NAME = "Errors.txt";	
	/** File containing text error message reported by a worker **/
	static final String NR_ERROR_FILE_NAME = "nrerrors.txt";	
	/** File containing text error message reported by a worker **/
	static final String NR_WARNING_FILE_NAME = "nrwarnings.txt";
	/** Constant name of file to write worker output table to. **/
	public static final String OUTPUT_DATA_FILE_NAME = "Output.tbl";
	/** Constant name of file to write worker activity output table to. **/
	public static final String OUTPUT_ACTIVITY_DATA_FILE_NAME = "Activity.tbl";
	/** When true, a new worker database name is created for each bundle. **/
	private static final boolean SHOULD_DEBUG_WORKER_DATABASE = false; // true;
	
	private static final String WARNING = "WARNING: ";
	private static final String ERROR = "ERROR: ";

	/** true if the worker should shutdown if no TODO or InProgress files are available. **/
	public static boolean isAutoShutdownMode = false;
	/** idle time before shutdown **/
	public static long shutdownIncrementMillis = 2L*60L*1000L;

	/** Control cleanup of Nonroad and other other temporary files, true to keep the files **/
	boolean isTest = false;

	/**
	 * This is set to true during idle conditions and set to false otherwise.
	 * This disables thread loop sleeping during periods of high activity.
	 * This is reset to true during every thread loop.
	**/
	boolean doIdleSleep = true;

	/** The window to send status messages to. This will be null during test cases. **/
	WorkerWindow workerWindow = null;

	/** Connection to the worker's database **/
	Connection database = null;

	/** flag summarizing SHOULD_DEBUG_WORKER_DATABASE and master's flags **/
	boolean shouldDebug = false;

	/** Random number generator for timing and file operations **/
	Random random = new Random();

	/**
	 * Clock time of when to shutdown.  If 0, no shutdown should occur.
	 * In GUI operation, this will remain at 0.
	 * In some command line scenarios, this will be updated each time a TODO or InProgress
	 * file is noticed, giving a timeout period to find more files.
	**/
	long whenToShutdownMillis = 0;

	/** ID number of the current bundle or blank if not working on a bundle **/
	String bundleID = "";

	/** Timers **/
	TreeMap<String,TimerInfo> timers = new TreeMap<String,TimerInfo>();
	/** Name of the current timer, if any **/
	String currentTimerName = null;

	public static class TimerInfo {
		public String timerName = "";
		public long startTimeMillis = 0;
		public long durationMillis = 0;
	}

	/**
	 * Standard constructor
	 * @param workerWindowToUse The window to send status messages to.
	**/
	public RemoteEmissionsCalculator(WorkerWindow workerWindowToUse) {
		super("RemoteEmissionsCalculator");

		workerWindow = workerWindowToUse;
	}

	/**
	 * This performs one-time startup activities prior to threaded operation.
	 * @return Should the thread continue to run
	**/
	protected boolean startupInThread() {
		try {
			database = WorkerConfiguration.theWorkerConfiguration.workerDatabaseSelection.openConnection();
			SQLRunner.addInnoDBConnection(database);
		} catch (Exception exception) {
			Logger.logError(exception, "Failed to open database connection");
			return false;
		}

		return true;
	}

	/** This performs one-time shutdown activities after threaded operation. **/
	protected void shutdownInThread() {
		if(database != null) {
			SQLRunner.removeInnoDBConnection(database);
			DatabaseUtilities.closeConnection(database);
			database = null;
		}
	}

	/** Last know set of TODO files in the shared work folder **/
	File[] todoFiles = null;
	int todoFileCount = 0;

	/**
	 * Poll for any unclaimed bundle files that are ready for processing. Uses
	 * SystemConfiguration to obtain location of files.
	 * @return Should the thread continue to run
	 * @throws InterruptedException This is typically thrown when a sleep() or wait() call
	 * is interrupted.
	**/
	protected boolean threadIterationGo() throws InterruptedException {
		FilenameFilter todoFilter = DistributedWorkFileName.buildFileNameFilter("*", "*", null, DistributedWorkFileState.TODO, "*");

		if(todoFiles == null || todoFileCount <= 0) {
			//Logger.log(LogMessageCategory.DEBUG,"Worker searching for TODO files...");
			todoFileCount = 0;
			todoFiles = WorkerConfiguration.theWorkerConfiguration.sharedDistributedFolderPath.listFiles(todoFilter);
			if(todoFiles != null && todoFiles.length > 0) {
				todoFileCount = todoFiles.length;
				advanceShutdownTimeout();
				FileUtilities.sortByDate(todoFiles);
				Logger.log(LogMessageCategory.DEBUG,"Worker found " + todoFileCount + " TODO files.");
			} else {
				//Logger.log(LogMessageCategory.DEBUG,"Worker found no TODO files.");
			}
		}

		if(todoFiles != null && todoFiles.length > 0 && todoFileCount > 0) {
			advanceShutdownTimeout();
			File todoFilePath = null;
			DistributedWorkFileName workFileName = null;
			int attemptedFileCount = 0;
			boolean didFind = false;
			// If we can't rename the first file in the list, skip forward a random number
			// of entries and try again.  This should cut down on file contention among
			// the workers.
			for(int i = 0; todoFiles != null && i < todoFiles.length; 
					i += (todoFiles == null? 0 : (1 + random.nextInt(Math.min(1+todoFiles.length-i,50)))) ) {
				if(todoFileCount > 0) {
					while(i<todoFiles.length && todoFiles[i] == null) { // skip null entries
						i++;
					}
				}
				if(i >= todoFiles.length) {
					if(todoFileCount > 0) { // if there are still files in the list
						i = 0; // start at the beginning of the list
						while(i<todoFiles.length && todoFiles[i] == null) { // skip null entries
							i++;
						}
						if(i >= todoFiles.length) { // if the end is found without a file, give up for now.
							todoFiles = null;
							break;
						}
					} else {
						todoFiles = null;
						break;
					}
				}
				todoFilePath = todoFiles[i];
				todoFiles[i] = null;
				todoFileCount--;
				if(todoFileCount <= 0) {
					todoFiles = null;
				}

				workFileName = DistributedWorkFileName.createFrom(todoFilePath.getName());
				if(workFileName == null) {
					Logger.log(LogMessageCategory.ERROR, "Invalid File Name: " + todoFilePath.getName());
				} else {
					Logger.log(LogMessageCategory.INFO,"Found work file: " + todoFilePath.getName());
					attemptedFileCount++;
					doIdleSleep = false;

					workFileName.state = DistributedWorkFileState.IN_PROGRESS;
					workFileName.wid = WorkerConfiguration.theWorkerConfiguration.distributedWorkerId;

					File ownedFilePath = new File(todoFilePath.getParentFile(),workFileName.toString());
					if(FileUtilities.renameFileWithRetry(todoFilePath,ownedFilePath))  {
						// The file was successfully renamed and is now under the
						// ownership of this worker.
						didFind = true;
						//long startTime = System.currentTimeMillis();
						//Logger.log(LogMessageCategory.INFO,
						//		"Calling RemoteEmissionsCalculator.processProgressFile()");

						advanceShutdownTimeout();
						processProgressFile(ownedFilePath);
						advanceShutdownTimeout();

						//Logger.log(LogMessageCategory.INFO,
						//		"RemoteEmissionsCalculator.processProgressFile() completed. " +
						//		((System.currentTimeMillis() - startTime) / 1000.0) + " secs");

						break;
					} else {
						//Logger.log(LogMessageCategory.INFO,"Failed to claim ownership of "
						//		+ todoFilePath.getName());
					}
				}
			}
			if(!didFind && attemptedFileCount > 0) {
				//Logger.log(LogMessageCategory.INFO,"Failed to claim ownership of " + attemptedFileCount + " files");
			}
			advanceShutdownTimeout();
		} else {
			if(isAutoShutdownMode) {
				if(whenToShutdownMillis == 0) {
					advanceShutdownTimeout();
				} else {
					// No TODO files were found, so check for InProgress files.
					FilenameFilter inProgressFilter = DistributedWorkFileName.buildFileNameFilter
						("*", "*", null, DistributedWorkFileState.IN_PROGRESS, "*");

					File[] inProgressFiles = WorkerConfiguration.theWorkerConfiguration.
							sharedDistributedFolderPath.listFiles(inProgressFilter);

					if(inProgressFiles != null && inProgressFiles.length > 0) {
						// InProgress data was found, even though no TODO files were, so advance
						// the timeout.  If the worker(s) doing the InProgress work fail, they
						// will revert back to TODO files and some other worker needs to pick
						// up the job.
						advanceShutdownTimeout();
					}
				}
			}
			try {
				// Sleep 3-8 seconds before trying again when there are no
				// files found in the queue.
				// Randomization is used to reduce contention between workers.
				Thread.sleep(1000*(3+random.nextInt(6))); // nextInt(6) returns 0-5 inclusive
			} catch (Exception e) {
				// Nothing to do here.
			}
		}

		if(isAutoShutdownMode && whenToShutdownMillis > 0) {
			if(System.currentTimeMillis() >= whenToShutdownMillis) {
				Logger.log(LogMessageCategory.INFO,"Worker stopping due to lack of TODO and InProgress files.");
				WorkerGUI.stopThreads(this);
				return false;
			}
		}
		return true;
	}

	/**
	 * Setup for an automatic shutdown in the near future.
	**/
	void advanceShutdownTimeout() {
		if(!isAutoShutdownMode) {
			return;
		}
		whenToShutdownMillis = System.currentTimeMillis() + shutdownIncrementMillis;
	}

	/**
	 * Called directly after threadIterationGo. Subclasses typically implement a simple sleep
	 * operation here.
	 * @throws InterruptedException This is typically thrown when a sleep() call is interrupted.
	**/
	protected void threadIterationWait() throws InterruptedException {
		if(doIdleSleep) {
			// When idle, wait 5-15 seconds between attempts.
			// Randomization is used to reduce contention between workers.
			sleep(1000*(5+random.nextInt(11))); // nextInt(11) returns 0-10 inclusive
		} else {
			doIdleSleep = true;
		}
	}

	/**
	 * Connect, if not already connected, to the worker database.
	 * @param forceReconnect true if the existing connection should be closed first
	 * @return true if database is valid
	**/
	protected boolean openDatabase(boolean forceReconnect) {
		try {
			if(forceReconnect && database != null) {
				SQLRunner.removeInnoDBConnection(database);
				DatabaseUtilities.closeConnection(database);
				database = null;
			}
			if(shouldDebug) {
				if(database != null) {
					SQLRunner.removeInnoDBConnection(database);
					DatabaseUtilities.closeConnection(database);
					database = null;
				}
				// Create a new connection to a new database
				WorkerConfiguration.theWorkerConfiguration.createWorkerDatabase(true);
			}
			if(database == null) {
				database = WorkerConfiguration.theWorkerConfiguration.workerDatabaseSelection.openConnection();
				if(database == null) {
					Logger.log(LogMessageCategory.ERROR, "Unable to connect to worker database");
					return false;
				}
				SQLRunner.addInnoDBConnection(database);
			}
			if(!shouldDebug) {
				// Ensure the tables in the database match the required schema.
				DatabaseUtilities.executeScript(database, new File("database/CreateWorker.sql"));
				if(CompilationFlags.DO_RATES_FIRST) {
					DatabaseUtilities.executeScript(database, new File("database/CreateWorkerRates.sql"));
				}
			}
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Error trying to connect to worker database");
			return false;
		}
	}

	/**
	 * Remove tables that are not needed within the worker database.
	**/
	private void removeUnwantedTables() {
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			TreeSet<String> tablesToDrop = new TreeSet<String>();
			sql = "SHOW TABLES";
			query.open(database,sql);
			while(query.rs.next()) {
				String name = query.rs.getString(1);
				if(CompilationFlags.DO_RATES_FIRST) {
					if(!name.equalsIgnoreCase("MOVESWorkerOutput")
							&& !name.equalsIgnoreCase("MOVESWorkerActivityOutput")
							&& !name.equalsIgnoreCase("BaseRateOutput")) {
						tablesToDrop.add(name);
					}
				} else {
					if(!name.equalsIgnoreCase("MOVESWorkerOutput")
							&& !name.equalsIgnoreCase("MOVESWorkerActivityOutput")) {
						tablesToDrop.add(name);
					}
				}
			}
			query.close();

			for(Iterator<String> i=tablesToDrop.iterator();i.hasNext();) {
				sql = "drop table if exists " + i.next();
				SQLRunner.executeSQL(database,sql);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,"Unable to remove unwanted worker tables",sql);
		}
	}

	/**
	 * Handles the specified bundle file.
	 * @param progressPath File path of the work file marked "in progress"
	**/
	public void processProgressFile(File progressPath) {
		bundleID = "";
		File workingFolderPath = // WorkerConfiguration.theWorkerConfiguration.currentTemporaryWorkFolderPath;
				FileUtilities.createTemporaryFolder(
				WorkerConfiguration.theWorkerConfiguration.workFolderPath,
				"WorkerTemp");
		File tempWorkFilePath = DistributedWorkFileName.alterFilePathState(
				progressPath, DistributedWorkFileState.TEMP);

		try {
			if(!openDatabase(false)) {
				return; // the finally block below will notice the lack of
						// progress and return the bundle to the pile
			}

			if (!workingFolderPath.exists()) {
				if (!workingFolderPath.mkdir()) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to create working dir: " + workingFolderPath);
					return;
				}
			}

			if(workerWindow != null) {
				workerWindow.changeStatusText("Processing " + progressPath.getName());
			} else {
				Logger.log(LogMessageCategory.INFO,"Worker Processing " + progressPath.getName());
			}

			DistributedWorkFileName workFileName = DistributedWorkFileName.createFrom(progressPath.getName());
			bundleID = workFileName.qid;

			LinkedList<File> unJardFilePathList
					= JARUtilities.unJarFileToFolder(workingFolderPath, progressPath);
			LinkedList<File> filePathsToRetrieve
					= processWorkFileContents(progressPath.getName(), workingFolderPath, unJardFilePathList);
			if(filePathsToRetrieve != null) {
				JARUtilities.jarFiles(tempWorkFilePath, filePathsToRetrieve);

				// Only continue if the Progress file still exists. If it doesn't exist, the master
				// likely terminated this worker.
				if(progressPath.exists()) {
					File doneWorkFilePath = DistributedWorkFileName.alterFilePathState(
							progressPath, DistributedWorkFileState.DONE);

					if(FileUtilities.renameFileWithRetry(tempWorkFilePath,doneWorkFilePath)) {
						progressPath.delete();
						if(workerWindow != null) {
							workerWindow.processedWorkFile();
						} else {
							Logger.log(LogMessageCategory.INFO,"Worker finished processing: "+
								workFileName);
						}
					} else {
						Logger.log(LogMessageCategory.WARNING, "Failed to rename "
								+ tempWorkFilePath.getName() + " to " + doneWorkFilePath.getName());
					}
				} else {
					if(workerWindow != null) {
						workerWindow.interruptedProcessingWorkFile();
					} else {
						Logger.log(LogMessageCategory.INFO,"Worker processing interrupted: "+
								workFileName);
				 	}
				}
			}
		} catch (Exception exception) {
			//Logger.logError(exception, "An attempt to process work files failed. Calculations will be retried.");
			//Logger.logError(exception, "A work file failed. Calculations will be retried. No data has been lost.");
			Logger.log(LogMessageCategory.WARNING,"A work file failed. Calculations will be retried. No data has been lost.");
			Logger.log(LogMessageCategory.INFO,exception.getMessage());
		} finally {
			// Delete the contents of the working folder.
			/*
			File[] folderContents = workingFolderPath.listFiles();
			for (int i = 0; i < folderContents.length; i++) {
				if(!folderContents[i].delete()) {
					Logger.log(LogMessageCategory.WARNING, "Failed to delete "
							+ folderContents[i].getName());
				}
			}
			*/
			if (!isTest) {
				FileUtil.deleteDir(workingFolderPath);
			}

			if(progressPath.exists()) {
				Logger.log(LogMessageCategory.INFO,"Reverting 'in progress' file back to 'todo'");

				File todoWorkFilePath = DistributedWorkFileName.alterFilePathState(
						progressPath, DistributedWorkFileState.TODO);
				if(!FileUtilities.renameFileWithRetry(progressPath,todoWorkFilePath)) {
					Logger.log(LogMessageCategory.WARNING, "Failed to rename "
							+ progressPath.getName() + " to " + todoWorkFilePath.getName());

					Logger.log(LogMessageCategory.INFO,"progressPath.isFile() = " + progressPath.isFile());
					Logger.log(LogMessageCategory.INFO,"progressPath.delete() = " + progressPath.delete());
				}
			}

			if (!WorkerConfiguration.theWorkerConfiguration.workerDebug && tempWorkFilePath.exists()) {
				tempWorkFilePath.delete();
			}

			if(workerWindow != null) {
				workerWindow.changeStatusText("Idle");
			} else {
				Logger.log(LogMessageCategory.INFO,"Worker is idle");
			}

			// Close the database connection so that all bundles reconnect to the server.
			// This should keep stale connections from lying around.
			if(database != null) {
				SQLRunner.removeInnoDBConnection(database);
				DatabaseUtilities.closeConnection(database);
				database = null;
			}
		}
	}

	/**
	 * Process the contents of a given work file.
	 * @param bundleFileName in-progress name of the bundle file
	 * @param workingFolderPath The path of the private working folder.
	 * @param unJardFilePathList A list of File objects that reference the unjar'd work files.
	 * @return The list of File objects that should be jar'd and returned.
	 * @throws Exception that occurred while processing file.
	**/
	public LinkedList<File> processWorkFileContents(String bundleFileName, File workingFolderPath,
			LinkedList<File> unJardFilePathList) throws Exception {
		String sql = "";
		BufferedReader sqlReader = null;
		File errorFile = new File(workingFolderPath, ERROR_FILE_NAME);
		File versionFile = new File(workingFolderPath, VERSION_FILE_NAME);
		File flagsFile = new File(workingFolderPath, FLAGS_FILE_NAME);
		BundleManifest manifest = new BundleManifest();
		File manifestFile = null;
		if(manifest.containsManifest(workingFolderPath)) {
			if(manifest.readFromFolder(workingFolderPath)) {
				manifestFile = manifest.getManifestFile(workingFolderPath);
			}
		}
		manifest.bundleName = bundleFileName;

		boolean oldShouldDebug = shouldDebug;
		shouldDebug = SHOULD_DEBUG_WORKER_DATABASE;

		if(manifestFile == null) {
			manifest.tablesToRetrieve.add("MOVESWorkerOutput");
			manifest.tablesToRetrieve.add("MOVESWorkerActivityOutput");
			if(CompilationFlags.DO_RATES_FIRST) {
				manifest.tablesToRetrieve.add("BaseRateOutput");
			}
		}

		try {
			appendInfo(versionFile, "WorkerVersion=" + MOVESWindow.MOVES_VERSION);
			appendInfo(versionFile, "WorkerComputerID="
					+ WorkerConfiguration.theWorkerConfiguration.computerID);
			appendInfo(versionFile, "WorkerID="
					+ WorkerConfiguration.theWorkerConfiguration.distributedWorkerId);

			manifest.workerVersion = MOVESWindow.MOVES_VERSION;
			manifest.workerComputerID = WorkerConfiguration.theWorkerConfiguration.computerID;
			manifest.workerID = WorkerConfiguration.theWorkerConfiguration.distributedWorkerId;

			LinkedList<File> returnFileList = new LinkedList<File>();
			if(manifestFile != null) {
				returnFileList.add(manifestFile);
			}

			// Read the flags file, if it exists
			if(flagsFile.exists()) {
				BufferedReader flagsReader = null;
				try {
					flagsReader = new BufferedReader(new FileReader(flagsFile));
					String line = null;
					while((line = flagsReader.readLine()) != null) {
						line = line.trim();
						if(line.equalsIgnoreCase("savedata")) {
							shouldDebug = true;
						}
					}
				} catch(Exception e) {
					appendError(errorFile,"Unable to read flags file: " + e.toString());
				} finally {
					if(flagsReader != null) {
						try {
							flagsReader.close();
						} catch(Exception e) {
							// Nothing to do here
						}
					}
				}
			}
			// Sanity check:  Only debug by keeping databases if the worker and the master
			// are on the same computer.  Otherwise, grid-based workers could create so many
			// databases their drives fillup.
			if(shouldDebug &&
					!gov.epa.otaq.moves.master.framework.MOVESAPI.hasMasterOnThisComputer()) {
				shouldDebug = false;
			}
			if(oldShouldDebug != shouldDebug) {
				if(!openDatabase(false)) {
					// We were unable to open our database, so quit processing.
					// Returning null here will revert the bundle back to a TODO state.
					return null;
				}
			}

			long bundleStartMillis = System.currentTimeMillis();
			resetTimers();
			startUnassignedTimer();

			removeUnwantedTables();

			File workerSQLPath = new File(workingFolderPath, WORKER_SQL_FILE_NAME);
			sqlReader = new BufferedReader(new FileReader(workerSQLPath));

			sql = "TRUNCATE MOVESWorkerOutput";
			try {
				SQLRunner.executeSQL(database, sql);
			} catch (SQLException exception) {
				appendSQLError(errorFile, sql, exception);
			}

			sql = "TRUNCATE MOVESWorkerActivityOutput";
			try {
				SQLRunner.executeSQL(database, sql);
			} catch (SQLException exception) {
				appendSQLError(errorFile, sql, exception);
			}

			if(CompilationFlags.DO_RATES_FIRST) {
				String[] ratesTables = {
					"BaseRateOutput"
				};
				for(int i=0;i<ratesTables.length;i++) {
					sql = "TRUNCATE " + ratesTables[i];
					try {
						SQLRunner.executeSQL(database, sql);
					} catch (SQLException exception) {
						appendSQLError(errorFile, sql, exception);
					}
				}
			}
			ArrayList<String> allSQL = new ArrayList<String>(1000);

			String currentSQLStatement = "";
			while (true) {
				String iterLine = sqlReader.readLine();
				if(iterLine == null) {
					break;
				}

				currentSQLStatement += iterLine;
				String entireSQLStatement = detectEndOfSQLStatement(currentSQLStatement);
				if(entireSQLStatement != null) {
					currentSQLStatement = performFilePathReplacements(currentSQLStatement, workingFolderPath);
					allSQL.add(currentSQLStatement);
					currentSQLStatement = "";
				} else {
					currentSQLStatement += " ";
				}
			}

			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			ParallelSQLRunner sqlRunner = null;
			if(WorkerConfiguration.theWorkerConfiguration.concurrentStatements > 1) {
				sqlRunner = new ParallelSQLRunner(
					WorkerConfiguration.theWorkerConfiguration.workerDatabaseSelection,
					WorkerConfiguration.theWorkerConfiguration.concurrentStatements);
			}

			File optFilePath = new File("nonroad.opt");
			if (unJardFilePathList != null) {
				for (File f : unJardFilePathList) {
					if (f.isFile()
							&& f.getName().equalsIgnoreCase("nonroad.opt")) {
						optFilePath = f;
						break;
					} else if (f.isFile() && f.getName().endsWith(".opt")) {
						optFilePath = f;
						break;
					}
				}
			}
			execute(sqlRunner, allSQL, 0, allSQL.size() - 1, replacements, errorFile, workingFolderPath, optFilePath, manifest);

			if(sqlRunner != null) {
				sqlRunner.onFinally();
				sqlRunner.stats.print();
			}

			startTimer("BundleResults");
			if(manifest.tablesToRetrieve.contains("MOVESWorkerOutput")) {
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
				File outputDataFile = new File(workingFolderPath, OUTPUT_DATA_FILE_NAME);
				deleteFile(outputDataFile);
				sql = "SELECT " + outputTableFields
					+ " INTO OUTFILE "
					+ DatabaseUtilities.escapeSQL(outputDataFile.getCanonicalPath())
					+ " FROM MOVESWorkerOutput";
				try {
					SQLRunner.executeSQL(database, sql);
				} catch (SQLException exception) {
					appendSQLError(errorFile, sql, exception);
				}
				sql = "SELECT COUNT(*) FROM MOVESWorkerOutput";
				PreparedStatement statement = database.prepareStatement(sql);
				ResultSet results = SQLRunner.executeQuery(statement,sql);
				results.next();
				Logger.log(LogMessageCategory.INFO,"Extracted " + results.getInt(1) +
						" rows from MOVESWorkerOutput");
				results.close();
				statement.close();

				if(!shouldDebug) {
					sql = "TRUNCATE MOVESWorkerOutput";
					try {
						SQLRunner.executeSQL(database, sql);
					} catch (SQLException exception) {
						appendSQLError(errorFile, sql, exception);
					}
				}
				if(outputDataFile.isFile() && !returnFileList.contains(outputDataFile)) {
					returnFileList.add(outputDataFile);
				}
			}

			if(manifest.tablesToRetrieve.contains("MOVESWorkerActivityOutput")) {
				// SELECT * can't be used since it would export MOVESOutputActivityRowID. This
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
				File outputActivityDataFile = new File(workingFolderPath, OUTPUT_ACTIVITY_DATA_FILE_NAME);
				deleteFile(outputActivityDataFile);
				sql = "SELECT " + outputActivityTableFields
					+ " INTO OUTFILE "
					+ DatabaseUtilities.escapeSQL(outputActivityDataFile.getCanonicalPath())
					+ " FROM MOVESWorkerActivityOutput";
				try {
					SQLRunner.executeSQL(database, sql);
				} catch (SQLException exception) {
					appendSQLError(errorFile, sql, exception);
				}
				sql = "SELECT COUNT(*) FROM MOVESWorkerActivityOutput";
				PreparedStatement statement = database.prepareStatement(sql);
				ResultSet results = SQLRunner.executeQuery(statement,sql);
				results.next();
				Logger.log(LogMessageCategory.INFO,"Extracted " + results.getInt(1) +
						" rows from MOVESWorkerActivityOutput");
				results.close();
				statement.close();
				if(outputActivityDataFile.isFile() && !returnFileList.contains(outputActivityDataFile)) {
					returnFileList.add(outputActivityDataFile);
				}

				if(!shouldDebug) {
					sql = "TRUNCATE MOVESWorkerActivityOutput";
					try {
						SQLRunner.executeSQL(database, sql);
					} catch (SQLException exception) {
						appendSQLError(errorFile, sql, exception);
					}
				}
			}

			for(Iterator ti=manifest.tablesToRetrieve.iterator();ti.hasNext();) {
				String tableName = (String)ti.next();
				if(tableName.equalsIgnoreCase("MOVESWorkerOutput")
						|| tableName.equalsIgnoreCase("MOVESWorkerActivityOutput")) {
					continue;
				}
				// Extract the named table
				File dataFile = new File(workingFolderPath,tableName + ".tbl");
				deleteFile(dataFile);
				sql = "SELECT *"
					+ " INTO OUTFILE "
					+ DatabaseUtilities.escapeSQL(dataFile.getCanonicalPath())
					+ " FROM " + tableName;
				try {
					SQLRunner.executeSQL(database, sql);
				} catch (SQLException exception) {
					appendSQLError(errorFile, sql, exception);
				}
				sql = "SELECT COUNT(*) FROM " + tableName;
				PreparedStatement statement = database.prepareStatement(sql);
				ResultSet results = SQLRunner.executeQuery(statement,sql);
				results.next();
				Logger.log(LogMessageCategory.INFO,"Extracted " + results.getInt(1) +
						" rows from " + tableName);
				results.close();
				statement.close();
				if(dataFile.isFile() && !returnFileList.contains(dataFile)) {
					returnFileList.add(dataFile);
				}
			}

			if(versionFile.isFile() && !returnFileList.contains(versionFile)) {
				returnFileList.add(versionFile);
			}
			if(errorFile.isFile() && !returnFileList.contains(errorFile)) {
				returnFileList.add(errorFile);
			}
			
			// if the NR errors/warnings files exist, add them to the list to send them to the master for the OutputProcessor to handle
			File nrErrorFile = new File(workingFolderPath, NR_ERROR_FILE_NAME);
			if(nrErrorFile.isFile() && !returnFileList.contains(nrErrorFile)) {
				returnFileList.add(nrErrorFile);
			}
			File nrWarningFile = new File(workingFolderPath, NR_WARNING_FILE_NAME);
			if(nrWarningFile.isFile() && !returnFileList.contains(nrWarningFile)) {
				returnFileList.add(nrWarningFile);
			}

			finishActiveTimer();
			long bundleEndMillis = System.currentTimeMillis();
			manifest.durationSeconds = (float)((bundleEndMillis - bundleStartMillis) / 1000.0);

			if(timers.size() > 0) {
				for(String timerName : timers.keySet()) {
					TimerInfo t = timers.get(timerName);
					manifest.durationFragments.add(new BundleManifest.DurationFragment(t.timerName,t.durationMillis/1000.0));
				}
			}

			if(manifestFile != null) {
				manifest.writeToFolder(workingFolderPath);
			}

			return returnFileList;
		} finally {
			if(sqlReader != null) {
				try {
					sqlReader.close();
				} catch (IOException exception) {
					Logger.logSqlError(exception,"Unable to process work file contents", sql);
				}
			}
			shouldDebug = false;
		}
	}

	/**
	 * Throw an exception if a SQL statement is a plain SELECT statement without
	 * side effects.  Such statements have no use in MOVES Workers and can only
	 * cause memory issues.
	 * @param sql statement to be checked
	 * @throws SQLException if the sql statement is a SELECT without an INTO OUTFILE clause
	**/
	void forbidPlainSelectStatements(String sql) throws SQLException {
		if(StringUtilities.substring(sql,0,6).equalsIgnoreCase("SELECT")) {
			if(sql.toLowerCase().indexOf("into outfile") < 0) {
				throw new SQLException("Plain SELECT statements are not allowed in worker SQL scripts");
			}
		}
	}

	/**
	 * Determine if execution may continue in the presence of an exception.
	 * @param sql statement that resulted in the exception
	 * @param e exception that occured while executing the statement
	 * @return true if execution should be halted
	**/
	boolean isExceptionSerious(String sql, Exception e) {
		String lowercaseMessage = e.getMessage().toLowerCase();
		if(lowercaseMessage.indexOf("duplicate key") >= 0) {
			// It is ok to add an index twice.  Either way, the index exists after the statement.
			return false;
		}
		if(lowercaseMessage.indexOf("can't drop") >= 0 && lowercaseMessage.indexOf("exists") > 0) {
			// It is ok to drop an index or column that doesn't exist.  Either way, it is gone after the statement.
			return false;
		}

		return true;
	}

	/**
	 * Perform a subset of the SQL statements in a master list of all SQL statements.
	 * @param sqlRunner object to execute the SQL, likely in parallel, may be null
	 * @param allSQL master list of all SQL statements to be run
	 * @param start 0-based index of the first statement to execute
	 * @param end 0-based index of the last statement, inclusive, to execute
	 * @param replacements all variables and their current values
	 * @param errorFile file to write any SQL errors into
	 * @param workingPath folder for temporary files
	 * @param optFilePath Location of Nonroad OPT file, or null
	 * @param manifest bundle's manifest
	 * @throws IOException if any file operation fails
	**/
	void execute(ParallelSQLRunner sqlRunner, ArrayList<String> allSQL,
			int start, int end, TreeMapIgnoreCase replacements, File errorFile,
			File workingPath, File optFilePath, BundleManifest manifest)
			throws IOException {
		ExternalCalculator externalCalc = new ExternalCalculator(this,database,workingPath,isTest,manifest);
		String sql = "";
		
		try {
			if(sqlRunner == null) {
				for(int i=start;i<=end;i++) {
					sql = (String)allSQL.get(i);
					if(externalCalc.absorbAndExecute(sql)) {
						continue;
					}
					if(sql.startsWith("starttimer")) {
						startTimer(sql.substring(10).trim());
					} else if(sql.startsWith("savemwo;")) {
						saveMOVESWorkerOutput("_input");
					} else if(sql.startsWith("savemwo2;")) {
						saveMOVESWorkerOutput("_output");
					} else if(sql.startsWith("nonroad")) {
						doNonroadSimulation(workingPath, optFilePath, manifest, sql);
					} else if(sql.startsWith("loop")) {
						i = executeLoop(sqlRunner,allSQL,i,end,replacements,errorFile,workingPath,optFilePath,manifest);
					} else {
						sql = StringUtilities.doReplacements(sql,replacements);
						try {
							if(sql.startsWith("stop;")) {
								/*
								Logger.log(LogMessageCategory.INFO,"STOP statement reached, stopping. CTRL^C to quit.");
								while(true) {
									try {
										Thread.sleep(1000);
									} catch(Exception e) {
										// Nothing to do here
									}
								}
								*/
								continue;
							}
							forbidPlainSelectStatements(sql);
							//Logger.log(LogMessageCategory.INFO,"Started: " + sql);
							//long startMillis = System.currentTimeMillis();
							SQLRunner.executeSQL(database, sql);
							//long durationMillis = System.currentTimeMillis() - startMillis;
							//Logger.log(LogMessageCategory.INFO,"Done in " + durationMillis + " ms");
						} catch (SQLException exception) {
							if(isExceptionSerious(sql,exception)) {
								appendSQLError(errorFile, sql, exception);
							}
						}
					}
				}
			} else {
				for(int i=start;i<=end;i++) {
					sql = (String)allSQL.get(i);
					if(externalCalc.absorbAndExecute(sql)) {
						continue;
					}
					if(sql.startsWith("starttimer")) {
						startTimer(sql.substring(10).trim());
					} else if(sql.startsWith("savemwo;")) {
						saveMOVESWorkerOutput("_input");
					} else if(sql.startsWith("savemwo2;")) {
						saveMOVESWorkerOutput("_output");
					} else if(sql.startsWith("nonroad")) {
						doNonroadSimulation(workingPath, optFilePath, manifest, sql);
					} else if(sql.startsWith("loop")) {
						try {
							sqlRunner.execute();
						} catch(Exception exception) {
							if(isExceptionSerious(sql,exception)) {
								appendSQLError(errorFile, sql, exception);
								return;
							}
						}
						i = executeLoop(sqlRunner,allSQL,i,end,replacements,errorFile,workingPath,optFilePath,manifest);
					} else {
						sql = StringUtilities.doReplacements(sql,replacements);
						try {
							forbidPlainSelectStatements(sql);
							sqlRunner.add(sql);
						} catch(Exception exception) {
							if(isExceptionSerious(sql,exception)) {
								appendSQLError(errorFile, sql, exception);
								return;
							}
						}
					}
				}
				try {
					sqlRunner.execute();
				} catch(Exception exception) {
					if(isExceptionSerious(sql,exception)) {
						appendSQLError(errorFile, sql, exception);
						return;
					}
				}
			}
		} finally {
			externalCalc.absorbAndExecute(null); // execute anything that is still pending
		}
	}

	/**
	 * Execute the Nonroad model and collect its results.
	 * @param workingFolderPath folder to hold all intermediate files and results.
	 * @param optFilePath location of the OPT file required by the Nonroad model.
	 * @param manifest information about the current work bundle.
	 * @param sqlStatement the pseudo-SQL statement used to invoke the model, used to pass extra parameters.
	**/
	private void doNonroadSimulation(File workingFolderPath, File optFilePath,
			BundleManifest manifest, String sqlStatement) {
		// File jarFile = this.nonroadDataJarPath;
		// if (!jarFile.exists()) {
		// throw new IOException("No Nonroad Data Jar file exists.");
		// }
		// JarHelper jarHelper = new JarHelper();
		// jarHelper.unjarDir(jarFile, workingFolderPath);

		boolean isOK = true;

		long start = System.currentTimeMillis();
		long elapsedTimeMillis;
		float elapsedTimeSec;

		Logger.log(LogMessageCategory.DEBUG,"Current dir: " + System.getProperty("user.dir"));
		File targetApplicationPath = new File(workingFolderPath, "nonroad.exe");
		
		/*
		File targetApplicationPath = new File(WorkerConfiguration.theWorkerConfiguration.nonroadApplicationPath);

		try {
			if(!targetApplicationPath.exists()) {
				String updatedPath = WorkerConfiguration.theWorkerConfiguration.nonroadApplicationPath;
				if(updatedPath.startsWith("/") || updatedPath.startsWith("\\")) {
					updatedPath = ".." + updatedPath;
				} else {
					updatedPath = "../" + updatedPath;
				}
				targetApplicationPath = new File(updatedPath);
				if(!targetApplicationPath.exists()) {
					if(updatedPath.startsWith("/") || updatedPath.startsWith("\\")) {
						updatedPath = ".." + updatedPath;
					} else {
						updatedPath = "../" + updatedPath;
					}
					targetApplicationPath = new File(updatedPath);
					if(!targetApplicationPath.exists()) {
						targetApplicationPath = null;
					}
				}
			}
		} catch(Exception e) {
			targetApplicationPath = null;
		}
		if(targetApplicationPath == null) {
			targetApplicationPath = new File(workingFolderPath, "nonroad.exe");
		}
		try {
			Logger.log(LogMessageCategory.DEBUG,"Using Nonroad path: " + targetApplicationPath.getCanonicalPath());
		} catch(IOException e) {
			// Nothing to do here
		}
		*/

		String[] arguments = new String[1];
		arguments[0] = "nonroad.opt";
		boolean runInCmd = false; // true;
		String[] environment = null;
		File targetFolderPath = workingFolderPath;
		// new File(WorkerConfiguration.theWorkerConfiguration.nonroadWorkingFolderPath);
		File processOutputPath = new File(targetFolderPath, "NonroadProcessOutput.txt");
		String inputText = null;
		try {
			startTimer("NonroadExternalApp");
			ApplicationRunner.runApplication(targetApplicationPath, arguments,
					targetFolderPath, new FileOutputStream(processOutputPath),
					inputText, runInCmd, environment);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

		if(processOutputPath.exists()) {
			//loop through the file and get Error/Warnings
			List<String> errors = new ArrayList<>();
			List<String> warnings = new ArrayList<>();

			try(BufferedReader br = new BufferedReader(new FileReader(processOutputPath))) {
				StringBuilder message = null;
				boolean readMessage = false;
				boolean isError = false;

				for(String line; (line = br.readLine()) != null; ) {
					// process the line.

					//look for start WARNING or ERROR
			    	if(!readMessage && (line.indexOf(ERROR) > -1 || line.indexOf(WARNING) > -1 )) {
			    		//start reading message
			    		isError = line.indexOf(ERROR) > -1;

			    		message = new StringBuilder(line.substring(line.indexOf( isError ? ERROR : WARNING ) + (isError ? ERROR.length() : WARNING.length())).trim());
			    		readMessage = true;
			    		
			    		continue;
			    	} else if(readMessage && "".equals(line.trim())) {
			    		//message is over, add to list and reset
			    		if(isError) {
			    			errors.add(message.toString());
			    		} else {
			    			warnings.add(message.toString());
			    		}

			    		readMessage = false;
			    	} else if(readMessage) {
			    		//message spans multiple lines, append
			    		message.append("\n|");
			    		message.append(line.trim());
			    	}
			    }
		
				//allow for no empty line
				if(readMessage) {
					if(isError) {
		    			errors.add(message.toString());
		    		} else {
		    			warnings.add(message.toString());
		    		}
				}
				
				// Write errors to file for the OutputProcessor to handle
				if(errors.size() > 0) {
					File errorFilePath = new File(workingFolderPath, NR_ERROR_FILE_NAME);
					PrintWriter errorWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(errorFilePath))));	
							
					for(String error : errors) {
						// This will end up in moveslog.txt as long as the worker is running on the same computer
						// as the master is. It will always end up in the moveserror table of the output database.
						Logger.log(LogMessageCategory.ERROR, error);
						errorWriter.println(ERROR + error);
					}
					
					errorWriter.close();
				}

				// Write warnings to file for the OutputProcessor to handle
				if(warnings.size() > 0) {
					File warningFilePath = new File(workingFolderPath, NR_WARNING_FILE_NAME);
					PrintWriter warningWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(warningFilePath))));	
							
					for(String warning : warnings) {
						// This will end up in moveslog.txt as long as the worker is running on the same computer
						// as the master is. It will always end up in the moveserror table of the output database.
						Logger.log(LogMessageCategory.WARNING, warning);					
						warningWriter.println(WARNING + warning);
					}
					
					warningWriter.close();
				}
				
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		elapsedTimeMillis = System.currentTimeMillis() - start;
		elapsedTimeSec = elapsedTimeMillis / 1000F;
		Logger.log(LogMessageCategory.INFO,
				"Time spent on running nonroad.exe (sec): " + elapsedTimeSec);

		if (true) {
			startTimer("NonroadLoadResults");
			start = System.currentTimeMillis();
			String sql = null; // "load data infile ? into table ?";
			PreparedStatement statement = null;
			try {
				for (String table : NonroadHelper.nonroadTableNeededAtWorkerSide) {
					sql = "load data infile " + "\'"
							+ workingFolderPath.getCanonicalPath()
							+ System.getProperty("file.separator") + table
							+ ".txt\' into table " + table;
					sql = sql.replaceAll("\\\\", "\\\\\\\\");
					statement = database.prepareStatement(sql);
					SQLRunner.executeQuery(statement, sql);
					// SQLRunner.executeQuery(database, sql);
				}
				statement.close();
			} catch (SQLException e) {
				e.printStackTrace();
				Logger.log(LogMessageCategory.ERROR,
						"When loading tables into worker's output database: "
								+ e.getMessage());
				isOK = false;
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				Logger.log(LogMessageCategory.ERROR,
						"When loading tables into worker's output database: "
								+ e.getMessage());
				isOK = false;
			}

			//BundleManifest.ContextHolder context = new BundleManifest.ContextHolder();
			//context.fromBundleManifestContext(manifest.context);

			BufferedReader bmyReader = null;
			BufferedReader bmvReader = null;

			try {
				System.out.println("Nonroad files are in: " + workingFolderPath.getCanonicalPath());
				bmyReader = new BufferedReader(new FileReader(new File(
						workingFolderPath, "NONROAD.BMY")));
			} catch (IOException e) {
				Logger.log(LogMessageCategory.ERROR,
						"Error in loading NONROAD.BMY: " + e.getMessage());
				isOK = false;
			}
			try {
				bmvReader = new BufferedReader(new FileReader(new File(workingFolderPath, "NONROAD.BMV")));
			} catch (IOException e) {
				Logger.log(LogMessageCategory.ERROR,"Error in loading NONROAD.BMY: " + e.getMessage());
				isOK = false;
			}
			if (isOK) {
				NonroadOutputDataLoader loader = new NonroadOutputDataLoader(workingFolderPath);
				loader.setFilter(sqlStatement);
				isOK = loader.loadBmyIntoDatabase(bmyReader, manifest, database);
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to load NONROAD.BMY into worker's output database.");
				}
				isOK = loader.loadBmvIntoDatabase(bmvReader, manifest, database);
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to load NONROAD.BMV into worker's output database.");
				}
			}
			if (bmyReader != null) {
				try {
					bmyReader.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			if (bmvReader != null) {
				try {
					bmvReader.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}

			elapsedTimeMillis = System.currentTimeMillis() - start;
			elapsedTimeSec = elapsedTimeMillis / 1000F;
			Logger.log(LogMessageCategory.INFO,
					"Time spent on loading data into worker output database (sec): "
							+ elapsedTimeSec);
			startUnassignedTimer();
		}
	}

	/**
	 * Execute a LOOP - END LOOP block within SQL.
	 * A loop block has the format:<br>
	 * loop ##my.variable##;<br>
	 * select distinct widgetID from widget; -- this statement provides the values to iterate over<br>
	 * insert into mytable (a) values (##my.variable##)<br>
	 * .......<br>
	 * end loop ##my.variable##;<br>
	 * @param sqlRunner object to execute the SQL, likely in parallel, may be null
	 * @param allSQL master list of all SQL statements to be run
	 * @param start 0-based index of the loop command
	 * @param end 0-based index of the last possible statement, inclusive, to scan for the end loop statement.
	 * @param replacements all variables and their current values
	 * @param errorFile file to write any SQL errors into
	 * @param workingPath folder for temporary files
	 * @param optFilePath Location of Nonroad OPT file, or null
	 * @param manifest bundle's manifest
	 * @throws IOException if any file operation fails
	 * @return the index of the end loop statement or the end of all SQL
	**/
	int executeLoop(ParallelSQLRunner sqlRunner, ArrayList<String> allSQL,
			int start, int end, TreeMapIgnoreCase replacements, File errorFile,
			File workingPath, File optFilePath, BundleManifest manifest)
			throws IOException {
		String sql = (String)allSQL.get(start); // get the "loop ##my.variable##;" statement

		String expectedEndStatement = "end " + sql; // look for "end loop ##my.variable##;"
		// Find loop end
		int loopEnd = -1;
		String t;
		for(int i=start+1;i<=end;i++) {
			t = (String)allSQL.get(i);
			if(t.equals(expectedEndStatement)) {
				loopEnd = i;
				break;
			}
		}
		if(loopEnd <= start) {
			appendError(errorFile,"loop without ending line: " + sql);
			return end;
		} else if(loopEnd == start + 1) {
			appendError(errorFile,"loop without a body/select clause: " + sql);
			return end;
		} else {
			String[] parts = sql.trim().split("\\s");
			if(parts.length != 2) {
				appendError(errorFile,"syntax error in loop statement: " + sql);
			} else {
				String loopVariable = parts[1]; // "loop ##my.variable##;"
				if(loopVariable.endsWith(";")) {
					loopVariable = loopVariable.substring(0,loopVariable.length()-1);
				}
				loopVariable = loopVariable.trim();

				// Get the set of values to iterate over
				ArrayList<String> loopValues = new ArrayList<String>(50);
				sql = StringUtilities.doReplacements((String)allSQL.get(start+1),replacements);

				PreparedStatement statement = null;
				ResultSet results = null;
				try {
					statement = database.prepareStatement(sql);
					results = SQLRunner.executeQuery(statement,sql);
					while(results.next()) {
						loopValues.add(StringUtilities.safeGetString(results.getString(1)));
					}
				} catch (SQLException exception) {
					appendSQLError(errorFile, sql, exception);
				} finally {
					if(results != null) {
						try {
							results.close();
						} catch(Exception e) {
							// Nothing to do here
						}
						results = null;
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
				// Run the loop over the values
				for(int i=0;i<loopValues.size();i++) {
					String value = (String)loopValues.get(i);
					replacements.put(loopVariable,value);
					execute(sqlRunner, allSQL, start + 2, loopEnd - 1,
							replacements, errorFile, workingPath, optFilePath,
							manifest);
				}
			}
			return loopEnd;
		}
	}

	/**
	 * This appends an SQL error to the given error file.
	 * @param errorFile File to append errors to.
	 * @param currentSQLStatement Statement the error occurred on.
	 * @param sqlException The exception that occurred.
	**/
	void appendSQLError(File errorFile, String currentSQLStatement
			, Exception sqlException) {
		Logger.logSqlError
			(sqlException,"RemoteEmissionsCalculator encountered a SQL error during execution" ,currentSQLStatement);
		appendText(errorFile,"Bundle " + bundleID
				+ " (worker " + WorkerConfiguration.theWorkerConfiguration.computerID 
				+ "/" + WorkerConfiguration.theWorkerConfiguration.distributedWorkerId 
				+ ") Error executing: " + currentSQLStatement);
		appendText(errorFile,sqlException.getMessage());
	}

	/**
	 * This appends an error to the given error file.
	 * @param errorFile File to append errors to.
	 * @param text information to write to the file.
	**/
	void appendError(File errorFile, String text) {
		Logger.log(LogMessageCategory.ERROR,text);
		appendText(errorFile,"Bundle " + bundleID
				+ " (worker " + WorkerConfiguration.theWorkerConfiguration.computerID 
				+ "/" + WorkerConfiguration.theWorkerConfiguration.distributedWorkerId 
				+ "): " + text);
	}

	/**
	 * This appends an informational message to the given file.
	 * @param textFile File to append  to.
	 * @param text information to write to the file.
	**/
	void appendInfo(File textFile, String text) {
		Logger.log(LogMessageCategory.INFO,text);
		appendText(textFile,text);
	}

	/**
	 * This appends a line of text to the given file.
	 * @param textFile File to append to.
	 * @param text information to write to the file.
	**/
	static void appendText(File textFile, String text) {
		final String eol = System.getProperty("line.separator");
		try {
			FileWriter fileWriter = new FileWriter(textFile.getPath(), true);
			try {
				fileWriter.write(text + eol);
			} finally {
				fileWriter.close();
			}
		} catch (IOException ioException) {
			// Ignore
		}
	}

	/**
	 * Detects file names encoded in a specific escape sequence: "##<file name>##"
	 * This replaces the escape sequence with the full path on the worker system.
	 * @param originalLine The original line with potential escape sequences.
	 * @param workingFolderPath directory holding temporary files
	 * @return The resulting line with all escape sequences replaced with full paths.
	 * @throws IOException If there is a file I/O error.
	**/
	static String performFilePathReplacements(String originalLine, File workingFolderPath) throws IOException {
		// The SQL must contain a LOAD DATA INFILE statement
		String lowerCaseSQL = originalLine.toLowerCase();
		if(lowerCaseSQL.indexOf("load data infile") < 0) {
			return originalLine;
		}
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

			String fileName = originalLine.substring(firstEscapeTextIndex + escapeText.length(), secondEscapeTextIndex);
			fileName = fileName.toLowerCase(); // standardize on lower-case names
			File fileObject = new File(
					workingFolderPath, // WorkerConfiguration.theWorkerConfiguration.currentTemporaryWorkFolderPath,
					fileName);
			originalLine = originalLine.substring(0, firstEscapeTextIndex)
					+ DatabaseUtilities.escapeSQL(fileObject.getCanonicalPath(), false)
					+ originalLine.substring(secondEscapeTextIndex + escapeText.length());
		}
		return originalLine;
	}

	/**
	 * Detects the end of an SQL statement. A ';' must be the last non-whitespace character
	 * on the line. This can potentially fail if using a database system where ';' isn't an
	 * SQL terminator or if the line is in the middle of a multi-line quoted string.
	 * @param sql The source SQL line.
	 * @return The entire SQL statement without the terminating character.
	**/
	static String detectEndOfSQLStatement(String sql) {
		for (int i = sql.length() - 1; i >= 0; i--) {
			char iterChar = sql.charAt(i);

			if(iterChar == ';') {
				return sql.substring(0, i);
			} else if(!Character.isWhitespace(iterChar)) {
				return null;
			}
		}

		return null;
	}

	/**
	 * Delete a file, making a much longer attempt than is normally done.
	 * @param f file to be removed
	**/
	static void deleteFile(File f) {
		for(int i=0;i<10;i++) {
			FileUtilities.deleteFileWithRetry(f);
			if(!f.exists()) {
				return;
			}
		}
	}

	/**
	 * Remove all timers.
	**/
	void resetTimers() {
		timers.clear();
		currentTimerName = null;
	}

	/**
	 * Accumulate time for the current timer, if any.
	**/
	void finishActiveTimer() {
		long now = System.currentTimeMillis();
		if(currentTimerName == null) {
			return;
		}
		TimerInfo t = timers.get(currentTimerName);
		if(t == null) {
			return;
		}
		t.durationMillis += now - t.startTimeMillis;
		currentTimerName = null;
	}

	/** Start the timer used to account for all time not explicitly assigned **/
	void startUnassignedTimer() {
		startTimer("Other");
	}

	/**
	 * Start a named timer, finishing any previous timer.
	 * @param timerName name of the timer, never null or empty.
	**/
	void startTimer(String timerName) {
		finishActiveTimer();
		if(timerName == null || timerName.length() <= 0) {
			startUnassignedTimer();
			return;
		}
		timerName = timerName.trim();
		while(timerName.endsWith(";")) {
			timerName = timerName.substring(0,timerName.length()-1);
		}
		currentTimerName = timerName.toLowerCase();
		TimerInfo t = timers.get(currentTimerName);
		if(t == null) {
			t = new TimerInfo();
			t.timerName = timerName;
			timers.put(currentTimerName,t);
		}
		t.startTimeMillis = System.currentTimeMillis();
	}

	/**
	 * Store MOVESWorkerOutput to a temporary file. Used for debugging.
	 * @param suffix text to append to the output file name, if any. May be null or empty.
	**/
	void saveMOVESWorkerOutput(String suffix) {
		suffix = StringUtilities.safeGetString(suffix);
		String sql = "select "
				+ " MOVESRunID,iterationID,"
				+ " yearID,monthID,dayID,hourID,"
				+ " stateID,countyID,zoneID,linkID,"
				+ " pollutantID,processID,"
				+ " sourceTypeID,regClassID,"
				+ " fuelTypeID,fuelSubTypeID,modelYearID,"
				+ " roadTypeID,SCC,"
				+ " engTechID,sectorID,hpID,"
				+ " emissionQuant,emissionRate"
				+ " into outfile 'c:/epa/moves/movesghgsource/movesworkeroutput" + suffix + "'"
				+ " from MOVESWorkerOutput";
		try {
			SQLRunner.executeSQL(database,sql);
		} catch(Exception e) {
			Logger.logError(e,"Unable to save MOVESWorkerOutput using: " + sql);
		}
	}
}
