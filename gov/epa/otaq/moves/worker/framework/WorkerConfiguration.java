/**************************************************************************************************
 * @(#)WorkerConfiguration.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker.framework;

import gov.epa.otaq.moves.common.CompilationFlags;
import gov.epa.otaq.moves.common.DatabaseSelection;
import gov.epa.otaq.moves.common.DatabaseUtilities;
import gov.epa.otaq.moves.common.DistributedIDBroker;
import gov.epa.otaq.moves.common.FileUtilities;
import gov.epa.otaq.moves.common.InstanceCounter;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.common.StringUtilities;
import gov.epa.otaq.moves.common.TreeSetIgnoreCase;
import gov.epa.otaq.moves.common.PasswordChecker;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.MissingResourceException;
import java.util.StringTokenizer;

/**
 * This class contains all configuration information used by the distributed
 * worker system. The main information are the worker configuration file name,
 * the SharedWork folder, a temporary folder for the private files, a
 * distributed worker id, The configaration data can be stored to or retrieved
 * from the worker configuration file.
 * 
 * @author Wesley Faler
 * @author Tim Hull
 * @version 2014-12-10
 **/
public class WorkerConfiguration {
	/** Name of the file holding the prior worker's ID file name and path **/
	static public String priorIDFileName = "PriorWorkerID.txt";

	/** reference to the single WorkerConfiguration() instance **/
	static public WorkerConfiguration theWorkerConfiguration = new WorkerConfiguration();

	/**
	 * The name the text configuration file used to back this configuration
	 * data.
	 **/
	static public String CONFIGURATION_FILE_NAME = "WorkerConfiguration.txt";
	/** Constant folder name for shared files in a distributed environment. **/
	static public final String DEFAULT_DISTRIBUTED_SHARED_FOLDER_NAME = "SharedWork";
	/** Constant folder name for private files in a distributed environment. **/
	static public final String DEFAULT_PRIVATE_WORK_FOLDER_NAME = "WorkerTemporary";

	/** The connection parameters for the worker's private database. **/
	public DatabaseSelection workerDatabaseSelection = new DatabaseSelection();

	/** Computer ID from the WorkerConfiguration.txt file **/
	public String computerID = "NoComputerID";

	/** Path to file containing the computer ID assignment line **/
	public String computerIDPath = "MOVESComputerID.txt";

	/**
	 * The ID that uniquely identifies this worker across the distributed
	 * processing system.
	 **/
	public String distributedWorkerId = "";

	/**
	 * The file used to reserve the worker ID. The time stamp on this file
	 * should be updated to provide a heartbeat
	 **/
	public File distributedWorkerIdFilePath;

	/**
	 * The folder used for distributed computing coordination. The master system
	 * queues work files here, the workers detect them, process them, and store
	 * result files here.
	 **/
	public File sharedDistributedFolderPath;

	/**
	 * This worker uses this folder to store temporary internal work files.
	 * Temporary sub-directories are created on startup to assure that there is
	 * no potential confusion with files not belonging to this worker.
	 **/
	public File workFolderPath;

	/**
	 * NONROAD application is in this folder in this worker computer.
	 **/
	public String nonroadApplicationPath = "NONROAD\\NR08a\\nonroad.exe"; // default in the working dir

	/**
	 * NONROAD application's name in this worker computer.
	 **/
	public String nonroadWorkingFolderPath = "NONROAD\\NR08a"; //

	/** Location of the external calculator application. **/
	public String calculatorApplicationPath = "calc\\externalcalculatorgo.exe";

	public boolean workerDebug = false;

	/**
	 * A temporary folder created within workFolderPath upon application
	 * startup. Files are stored in here to guarantee against confusion with
	 * files not belonging to this worker.
	 **/
	public File currentTemporaryWorkFolderPath;

	/** Name of the worker database as read from CONFIGURATION_FILE_NAME **/
	public String originalWorkerDatabaseName = "";

	/** Maximum number of concurrent statements allowed **/
	public int concurrentStatements = 1;

	/**
	 * Read a configuration file, recursing into other configuration files as
	 * needed.
	 * 
	 * @param fileNamesAlreadyRead
	 *            set of names already read, used to prevent infinite recursion.
	 * @param fileName
	 *            full name and path of the file to be read.
	 * @throws IOException
	 *             If a file I/O error occured.
	 **/
	void readConfigurationFile(TreeSetIgnoreCase fileNamesAlreadyRead,
			String fileName) throws IOException {
		File configFile = new File(fileName);
		String mysqlUserName = null;
		String mysqlPassword = null;
		try {
			String fullPath = configFile.getCanonicalPath();
			if (fileNamesAlreadyRead.contains(fullPath)) {
				return;
			}
			fileNamesAlreadyRead.add(fullPath);

			ArrayList lines = null;
			boolean fileExists = FileUtilities.fileExists(configFile);
			if (fileExists) {
				lines = FileUtilities.readLines(configFile);
				if (lines == null || lines.size() <= 0) {
					fileExists = false;
				}
			}
			if (fileExists) {
				String iterLine;
				for (Iterator li = lines.iterator(); li.hasNext();) {
					iterLine = (String) li.next();
					iterLine = iterLine.trim();

					if ((iterLine.length() == 0) || iterLine.startsWith("//")) {
						continue;
					}

					StringTokenizer iterTokenizer = new StringTokenizer(
							iterLine, "=");
					if (iterTokenizer.countTokens() > 2) {
						Logger.log(LogMessageCategory.WARNING,
								"Invalid configuration entry: " + iterLine);
						continue;
					}

					String name = "";
					String value = "";

					if (iterTokenizer.countTokens() == 1) { // blank entry
						name = iterTokenizer.nextToken().trim();
					} else {
						name = iterTokenizer.nextToken().trim();
						value = iterTokenizer.nextToken().trim();
					}

					if (name.compareToIgnoreCase("sharedDistributedFolderPath") == 0) {
						sharedDistributedFolderPath = new File(value);
					} else if (name.compareToIgnoreCase("workerServerName") == 0) {
						workerDatabaseSelection.serverName = value;
					} else if (name.compareToIgnoreCase("workerDatabaseName") == 0) {
						originalWorkerDatabaseName = value;
						workerDatabaseSelection.databaseName = value;
					} else if (name.compareToIgnoreCase("workerUserName") == 0) {
						//workerDatabaseSelection.userName = value;
					} else if (name.compareToIgnoreCase("workerPassword") == 0) {
						//workerDatabaseSelection.password = value;
					} else if (name.compareToIgnoreCase("workFolderPath") == 0) {
						workFolderPath = new File(value);
					} else if (name.compareToIgnoreCase("ComputerID") == 0) {
						computerID = value;
						if (computerID.length() == 0) {
							computerID = "NoComputerID";
						} else if (computerID.length() > 20) {
							computerID = computerID.substring(0, 20).trim();
							if (computerID.length() == 0) {
								computerID = "NoComputerID";
							}
						}
					} else if (name.compareToIgnoreCase("computerIDPath") == 0) {
						computerIDPath = value;
						if (value != null && value.length() > 0
								&& new File(value).exists()) {
							readConfigurationFile(fileNamesAlreadyRead, value);
						}
					} else if (name.compareToIgnoreCase("concurrentStatements") == 0) {
						if (value.length() > 0) {
							try {
								concurrentStatements = Integer.parseInt(value);
							} catch (Exception e) {
								Logger.log(LogMessageCategory.ERROR,
										"Unable to set worker concurrentStatements with \""
												+ value + "\"");
							}
						}
						if (concurrentStatements <= 0) {
							concurrentStatements = 1;
						}
					} else if (name.compareToIgnoreCase("nonroadApplicationPath") == 0) {
						if (value != null && !value.trim().isEmpty()) {
							nonroadApplicationPath = value;
						}
					} else if (name.compareToIgnoreCase("nonroadWorkingFolderPath") == 0) {
						if (value != null && !value.trim().isEmpty()) {
							nonroadWorkingFolderPath = value;
						}
					} else if (name.compareToIgnoreCase("calculatorApplicationPath") == 0) {
						if (value != null && !value.trim().isEmpty()) {
							calculatorApplicationPath = value;
						}
					} else if (name.compareToIgnoreCase("workerDebug") == 0) {
						if (value != null && !value.trim().isEmpty()) {
							this.workerDebug = Boolean.valueOf(value);
						}
					} else if(name.compareToIgnoreCase("mysqlUserName") == 0) {
						if(value != null) {
							mysqlUserName = value.trim();
						}
					} else if(name.compareToIgnoreCase("mysqlPassword") == 0) {
						if(value != null && value.length() > 0) {
							mysqlPassword = value.trim();
						}
					} else {
						Logger.log(LogMessageCategory.INFO,
								"Unknown configuration name: " + iterLine);
					}
				}
			} else {
				Logger.log(LogMessageCategory.WARNING,
						"Couldn't find configuration file: " + fileName);
			}
		} catch (FileNotFoundException e) {
			throw new IOException("File not found : " + fileName);
		} finally {
			if(mysqlUserName != null && mysqlPassword != null) {
				String realPassword = PasswordChecker.decode(mysqlUserName,mysqlPassword);
				if(realPassword == null) {
					Logger.log(LogMessageCategory.ERROR,"Corrupt MySQL username/password provided.");
				} else {
					DatabaseSelection.userProvidedUserName = mysqlUserName;
					DatabaseSelection.userProvidedPassword = realPassword;
				}
			}
		}
	}

	/**
	 * Loads configuration data from disk.
	 * 
	 * @throws IOExcption
	 *             If a file I/O error occurred.
	 **/
	public void loadConfigurationData() throws IOException {
		File configurationPath = new File(CONFIGURATION_FILE_NAME);

		boolean fileExists = FileUtilities.fileExists(configurationPath);
		if (fileExists) {
			TreeSetIgnoreCase fileNames = new TreeSetIgnoreCase();
			readConfigurationFile(fileNames, CONFIGURATION_FILE_NAME);
		} else {
			Logger.log(
					LogMessageCategory.WARNING,
					"Couldn't find configuration file. "
							+ "Using defaults and generating default configuration file.");

			writeDefaultConfigurationFile();
		}

		if (concurrentStatements <= 0) {
			concurrentStatements = 1;
		}

		// Verify that the configured directories and files are all in
		// existence. Otherwise
		// throw an exception.
		if ((sharedDistributedFolderPath == null)
				|| !sharedDistributedFolderPath.isDirectory()) {
			throw new FileNotFoundException(
					"sharedDistributedFolderPath doesn't exist");
		}
		if ((workFolderPath == null) || !workFolderPath.isDirectory()) {
			throw new FileNotFoundException("workFolderPath doesn't exist");
		}

		currentTemporaryWorkFolderPath = null; // FileUtilities.createTemporaryFolder(workFolderPath,
												// "WorkerTemp");

		// Create worker database if it doesn't exist.
		workerDatabaseSelection.databaseName = getWorkerDatabaseNameBase();
		createWorkerDatabase(false);

		Connection testConnection = workerDatabaseSelection
				.openConnectionOrNull();
		Logger.log(LogMessageCategory.INFO, "testConnection = "
				+ testConnection);
		if (testConnection == null) {
			throw new MissingResourceException("Failed to open database",
					Connection.class.getName(), "workerDatabaseSelection");
		} else {
			try {
				testConnection.close();
			} catch (SQLException exception) {
				Logger.logError(exception,
						"Worker Configuration failed to close database connection.");
			}
		}

		distributedWorkerId = DistributedIDBroker.acquireID(
				DistributedIDBroker.PREFIX_WORKER_ID, computerID,
				sharedDistributedFolderPath);
		distributedWorkerIdFilePath = DistributedIDBroker.getIDFilePath(
				DistributedIDBroker.PREFIX_WORKER_ID, distributedWorkerId,
				computerID, sharedDistributedFolderPath);
		rememberCurrentID(distributedWorkerIdFilePath);

		Logger.log(LogMessageCategory.INFO,
				"WorkerConfiguration loaded. distributedWorkerId = "
						+ distributedWorkerId);
	}

	/**
	 * Get the database name to be used, excluding any unique bundle identifiers
	 * if the database is being used for debugging purposes.
	 * 
	 * @return the database name to be used
	 **/
	public String getWorkerDatabaseNameBase() {
		if (originalWorkerDatabaseName == null
				|| originalWorkerDatabaseName.length() <= 0
				|| originalWorkerDatabaseName.equals("*")) {
			// Create a repeatable but unique name
			return InstanceCounter.getDB("MOVESWorker");
		}
		return originalWorkerDatabaseName;
	}

	/**
	 * Ensure the worker database exists and has the required tables within.
	 * 
	 * @param shouldMakeUniqueName
	 *            true if the database used should be created rather than merely
	 *            reused from other iterations.
	 **/
	public void createWorkerDatabase(boolean shouldMakeUniqueName) {
		if (shouldMakeUniqueName) {
			// Format: <dbname><yymmdd_hhMMss>
			// Ex: MOVESWorker060504_152030 for May 4, 2006, 3:20:30 PM
			workerDatabaseSelection.databaseName = getWorkerDatabaseNameBase()
					+ StringUtilities.datetimeForFileName();
		}
		try {
			Connection knownConnection = workerDatabaseSelection.openConnectionOrNull(false);
			String sql = "";
			try {
				sql = "CREATE DATABASE IF NOT EXISTS " + workerDatabaseSelection.databaseName;
				SQLRunner.executeSQL(knownConnection, sql);
				SQLRunner.executeSQL(knownConnection, "USE " + workerDatabaseSelection.databaseName);
				DatabaseUtilities.executeScript(knownConnection, new File("database/CreateWorker.sql"));
				if (CompilationFlags.DO_RATES_FIRST) {
					DatabaseUtilities.executeScript(knownConnection, new File("database/CreateWorkerRates.sql"));
				}
			} catch (Exception exception) {
				Logger.logError(exception,"Failed to execute create worker script");
			} finally {
				if (knownConnection != null) {
					knownConnection.close();
					knownConnection = null;
				}
			}
		} catch (Exception exception) {
			Logger.logError(exception, "Failed to open known connection");
		}
	}

	/**
	 * Deletes the temporary directory used by this worker. This should be
	 * called after loadConfigurationData is called before clean up.
	 **/
	public void cleanupTemporaryDirectory() {
		if (workerDebug) {
			return;
		}

		if (currentTemporaryWorkFolderPath != null
				&& currentTemporaryWorkFolderPath.exists()) {
			FileUtilities.deleteTemporaryFolder(currentTemporaryWorkFolderPath);
			currentTemporaryWorkFolderPath = null;
		}
	}

	/**
	 * Sets the configuration to some reasonable defaults and writes to disk.
	 * 
	 * @throws IOExecption
	 *             If a file I/O error occurred.
	 **/
	public void writeDefaultConfigurationFile() throws IOException {
		Logger.log(LogMessageCategory.INFO,
				"WorkerConfiguration.writeDefaultConfigurationFile()");
		sharedDistributedFolderPath = new File(
				DEFAULT_DISTRIBUTED_SHARED_FOLDER_NAME);
		workFolderPath = new File(DEFAULT_PRIVATE_WORK_FOLDER_NAME);
		initComputerIDAndPath();
		saveConfigurationData();
	}

	/**
	 * Initializes the computer ID and path to default values based on system
	 * settings.
	 **/
	public void initComputerIDAndPath() {
		try {
			computerID = InetAddress.getLocalHost().getHostName();
		} catch (UnknownHostException e) {
			computerID = "NoComputerID";
		}
		computerIDPath = "";
	}

	/**
	 * Saves configuration data to disk.
	 * 
	 * @throws IOException
	 *             If a file I/O error occurred.
	 **/
	public void saveConfigurationData() throws IOException {
		Logger.log(LogMessageCategory.INFO, "saveConfigurationData");

		Writer configurationWriter = new BufferedWriter(new OutputStreamWriter(
				new FileOutputStream(new File(CONFIGURATION_FILE_NAME))));

		writeConfigurationEntry(configurationWriter, "workerServerName",
				workerDatabaseSelection.serverName);
		writeConfigurationEntry(configurationWriter, "workerDatabaseName",
				getWorkerDatabaseNameBase());
		//writeConfigurationEntry(configurationWriter, "workerUserName",
		//		workerDatabaseSelection.userName);
		//writeConfigurationEntry(configurationWriter, "workerPassword",
		//		workerDatabaseSelection.password);
		writeConfigurationEntry(configurationWriter,
				"sharedDistributedFolderPath",
				sharedDistributedFolderPath.getCanonicalPath());
		writeConfigurationEntry(configurationWriter, "workFolderPath",
				workFolderPath.getCanonicalPath());
		writeConfigurationEntry(configurationWriter, "computerIDPath",
				computerIDPath);
		writeConfigurationEntry(configurationWriter, "concurrentStatements",
				"" + concurrentStatements);
		writeConfigurationEntry(configurationWriter, "nonroadApplicationPath",
				nonroadApplicationPath);
		writeConfigurationEntry(configurationWriter, "nonroadWorkingFolderPath", 
				nonroadWorkingFolderPath);
		writeConfigurationEntry(configurationWriter, "calculatorApplicationPath",
				calculatorApplicationPath);

		writeConfigurationEntry(configurationWriter,"mysqlUserName",DatabaseSelection.userProvidedUserName);
		writeConfigurationEntry(configurationWriter,"mysqlPassword",
				PasswordChecker.encode(DatabaseSelection.userProvidedUserName,DatabaseSelection.userProvidedPassword));

		configurationWriter.close();

		if (computerIDPath != null && computerIDPath.length() > 0) {
			configurationWriter = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(new File(computerIDPath))));

			writeConfigurationEntry(configurationWriter, "ComputerID",
					computerID);
			configurationWriter.close();
		}
	}

	/**
	 * Internal method to write a single configuration entry.
	 * 
	 * @param configurationWriter
	 *            The file writer object.
	 * @param name
	 *            The field name
	 * @param value
	 *            The field value
	 * @throws IOException
	 *             If a file I/O error occurred.
	 **/
	static void writeConfigurationEntry(Writer configurationWriter,
			String name, String value) throws IOException {
		if (value.length() > 0) {
			configurationWriter.write(name + " = " + value
					+ System.getProperty("line.separator"));
		}
	}

	/**
	 * Note the current ID file in case it needs to be removed later.
	 * 
	 * @param idFile
	 *            name of the current ID file
	 **/
	static void rememberCurrentID(File idFile) {
		if (priorIDFileName != null && priorIDFileName.length() > 0) {
			try {
				File priorIDFile = new File(priorIDFileName);
				String fullPath = idFile.getCanonicalPath();
				FileUtilities.appendLine(priorIDFile, fullPath);
			} catch (Exception e) {
				Logger.logError(e, "Unable to store current worker ID");
			}
		}
	}

	/** Remove the prior worker's ID file, if present. **/
	public static void clearPriorIDs() {
		if (priorIDFileName != null && priorIDFileName.length() > 0) {
			try {
				File priorIDFile = new File(priorIDFileName);
				if (priorIDFile.exists()) {
					ArrayList lines = FileUtilities.readLines(priorIDFile);
					if (lines != null) {
						for (int i = 0; i < lines.size(); i++) {
							String fileName = (String) lines.get(i);
							fileName = StringUtilities.safeGetString(fileName)
									.trim();
							if (fileName.length() > 0) {
								File f = new File(fileName);
								FileUtilities.deleteFileWithRetry(f);
							}
						}
					}
					FileUtilities.deleteFileWithRetry(priorIDFile);
				}
			} catch (Exception e) {
				Logger.logError(e, "Unable to clear prior worker ID files");
			}
		}
	}
}
