/**************************************************************************************************
 * @(#)MOVESAPI.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.importers.ImporterInstantiator;
import gov.epa.otaq.moves.master.framework.importers.ImporterManager;
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;
import java.net.*;
import java.sql.*;
import gov.epa.otaq.moves.worker.framework.WorkerConfiguration;
import org.apache.commons.io.input.BOMInputStream;

/**
 * This class provides the API for MOVES.
 * <br><br>
 * <b>API High-level process:</b>
 * <table border="0" cellspacing="0" cellpadding="0" width="100%">
 * 	<tr>
 *		<td rowspan="7" width="10%">
 * 		</td>
 * 		<td>
 * 			1) Fill in the runSpecFileList
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			2) Set the error level
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			3) Call runBatch()
 * 		</td>
 * 	</tr>
 * </table>
 * <br>
 * <b>Command Line Arguments:</b>
 * <table border="1" cellspacing="1" cellpadding="0" width="100%">
 * 	<tr>
 * 		<th>
 * 			Argument
 * 		</th>
 * 		<th>
 * 			Parameters
 * 		</th>
 * 		<th>
 * 			Purpose or function
 * 		</th>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			-r
 * 		</td>
 * 		<td>
 * 			RunSpec file name and path
 * 		</td>
 * 		<td>
 * 			Specifies the location and name of a RunSpec file.  If not using the "-rl" or "-i"
 *			arguments, then there should be at least one occurrence of "-r".
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			-rl
 * 		</td>
 * 		<td>
 * 			RunSpec list file name and path
 * 		</td>
 * 		<td>
 * 			Specifies the location and name of a file that contains a list of RunSpec files to run.
 *			See "RunSpec List File Format" below.
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			-i
 * 		</td>
 * 		<td>
 * 			Importer XML file name and path
 * 		</td>
 * 		<td>
 *			Specifies the location and name of an importer XML file.  A RunSpec is not needed
 *			to use this option as the importer file contains all required filters.
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			-il
 * 		</td>
 * 		<td>
 * 			Importer XML list file name and path
 * 		</td>
 * 		<td>
 * 			Specifies the location and name of a file that contains a list of importer XML files to run.
 *			Similar to the -rl option.  See "RunSpec List File Format" below.
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			-noworker
 * 		</td>
 * 		<td>
 *
 * 		</td>
 * 		<td>
 *          Prevents the master from automatically starting a local worker.
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			-onlytodo or -maketodo
 * 		</td>
 * 		<td>
 *
 * 		</td>
 * 		<td>
 *          Causes RunSpecs to generate TODO files but never to retrieve DONE files nor to cleanup
 *			TODO, DONE, or InProgress files upon exit.  Implies -noworker.
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			-p
 * 		</td>
 * 		<td>
 * 			PDSpec XML file
 * 		</td>
 * 		<td>
 *			Pickup DONE files from other masters, storing them to databases named in the PDSpec.
 *			A GUI option can provide example PDSpec files.
 * 		</td>
 * 	</tr>
 * </table>
 * <br>
 * <b>RunSpec List File Format:</b>
 * <table border="0" cellspacing="0" cellpadding="0" width="100%">
 * 	<tr>
 * 		<td colspan="2">
 * 			A RunSpec list file can have any valid name and extension but must specify the
 *			location and name of a single RunSpec per line.  One example:
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td rowspan="3" width="10%">
 * 		</td>
 * 		<td>
 * 			test01.runspec
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			myrunspec.xml
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td>
 * 			c:\temp\myrunspec.xml
 * 		</td>
 * 	</tr>
 * 	<tr>
 * 		<td colspan="2">
 * 			This list file specifies three files, the first two (having implicit relative paths)
 *			are expected to be in the current directory.  The third file is identified by an
 *			absolute path.
 * 		</td>
 * 	</tr>
 * </table>
 *
 * @author		Wesley Faler
 * @author		Sarah Luo
 * @author 		Tim Hull
 * @version		2014-01-23
**/
public class MOVESAPI implements MOVESEngineListener, MOVESEngine.CompletedListener {
	/** The singleton. **/
	static MOVESAPI theAPI;

	/** Access method to the singleton. **/
	public static MOVESAPI getTheAPI() {
		if(theAPI == null) {
			theAPI = new MOVESAPI();
		}
		return theAPI;
	}

	/** TCP/IP port used on the socket designating a running master's presense **/
	private static final int MASTER_FLAG_PORT = 13131;
	/** TCP/IP socket used to designate a running master's presense **/
	private static ServerSocket masterSocket = null;
	/** Depth counter for designating a running master's presense **/
	private static int masterSocketCount = 0;

	/** Indicate that a running master is present on this computer **/
	public static void setupFlagForMaster() {
		if(masterSocketCount == 0) {
			try {
				masterSocket = new ServerSocket(MASTER_FLAG_PORT);
			} catch(Exception e) {
				/**
				 * @explain The MOVES main program uses a TCP/IP socket to signal the fact that
				 * it is running, thus working to prevent duplicate executions on a single computer.
				 * MOVES was unable to create this socket, either because another MOVES Main is already running
				 * or due to an operating system firewall rule that does not trust the MOVES application.
				**/
				if(e.toString().toLowerCase().contains("jvm_bind") || e.toString().toLowerCase().contains("net_bind")) {
					Logger.log(LogMessageCategory.ERROR,"MOVES was unable to bind to " + MASTER_FLAG_PORT + 
													    ". Is MOVES already running on this computer or is a firewall rule causing this issue?");
				} else {
					// a different error has occurred
					Logger.logError(e,"MOVES was unable to bind to " + MASTER_FLAG_PORT);
				}

			}
		}
		masterSocketCount++;
	}

	/** Remove the indication that a running master is present on this computer **/
	public static void shutdownFlagForMaster() {
		masterSocketCount--;
		if(masterSocketCount <= 0) {
			if(masterSocket != null) {
				try {
					masterSocket.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				masterSocket = null;
			}
			InstanceCounter.shutdown(); // all master code eventually goes through this point during shutdown
		}
	}

	/** Determine if a running master is present on this computer **/
	public static boolean hasMasterOnThisComputer() {
		ServerSocket s = null;
		try {
			s = new ServerSocket(MASTER_FLAG_PORT);
			// If we got this far, there was no master already running
			return false;
		} catch(Exception e) {
			// Nothing to do here, these are normal and expected if a master is running
			return true;
		} finally {
			if(s != null) {
				try {
					s.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
	}

	/** The active RunSpec. **/
	RunSpec runSpec;
	/**
	 * The path of the file that is storing the active runSpec. This will be blank
	 * until the RunSpec is saved or loaded.
	**/
	File runSpecFilePath;

	/** True when a PDSpec is being executed. **/
	boolean isExecutingPDSpec = false;
	/** True when PDSpec execution should be halted. **/
	boolean shouldStopPDSpec = false;

	/** The list of File objects representing RunSpecs to run during batch mode. **/
	public LinkedList<File> runSpecFileList = new LinkedList<File>();
	/** The list of File objects representing importers to run during batch mode. **/
	public LinkedList<File> importerFileList = new LinkedList<File>();
	/** The list of File objects representing PDSpecs to run during batch mode. **/
	public LinkedList<File> pdSpecFileList = new LinkedList<File>();

	/** PDSpec being iterated over, used when doing asychronous iteration **/
	private PDSpec iteratingPDSpec = null;
	/** Index of the current entry within iteratingPDSpec, used when doing asychronous iteration **/
	private int pdSpecEntryIndex = 0;

	/** Constructor **/
	public MOVESAPI() {
		MOVESEngine.theInstance = new MOVESEngine();
		runSpec = new RunSpec();
		// This class receives engine progress notifications
		MOVESEngine.subscribeToProgress(this);
		MOVESEngine.completedListeners.add(this);
	}

	/**
	 * Invokes MOVES in either GUI mode or "batch" mode, based on command line arguments.
	 * @param commandLineArgs A String array containing the command line arguments.
	**/
	public void runApplication(String commandLineArgs[]) {
		try {
			setupFlagForMaster();

			boolean isError = false;
			if(parseCommandLineArgs(commandLineArgs)) {
				try {
					runBatch();
				} catch (InterruptedException exception) {
					/** @nonissue **/
					Logger.logException(LogMessageCategory.INFO, exception);
				}
			} else {
				isError = true;
				System.err.println("Invalid command line arg(s)");
			}
			if(isError) {
				// If there was an error, this function won't do much anyway, but end with
				// some non-zero number that could be used by a batch script
				/** @nonissue **/
				MOVESThread.signalAllToTerminate();
				System.exit(1);
			}
		} finally {
			shutdownFlagForMaster();
		}
	}

	/**
	 * Displays the command line options that are expected when in "batch" mode.
	**/
	void showUsage() {
		/** @nonissue **/
		System.out.println("command line options:");
		/** @nonissue **/
		System.out.println("\t-r [runspec path and name]  A runspec file to use");
		/** @nonissue **/
		System.out.println("\t-rl [runspec list path]     "
				+ "Path/name to a file containing RunSpec path/names");
		/** @nonissue **/
		System.out.println("\t-i [importer path and name] An importer to execute");
		/** @nonissue **/
		System.out.println("\t-il [importer list path]     "
				+ "Path/name to a file containing importer path/names");
		/** @nonissue **/
		System.out.println("At least one runspec (via -r or -rl) or at least one importer (via -i or -il)");
		/** @nonissue **/
		System.out.println("must be specified.");
		/** @nonissue **/
		System.out.println("\t-noworker     "
				+ "Do not start a local worker");
		/** @nonissue **/
		System.out.println("\t-onlytodo     "
				+ "Create TODO files but do not collect DONE files nor start a local worker");
		/** @nonissue **/
		System.out.println("\t-maketodo     "
				+ "Same as -onlytodo");
		System.out.println("\t-p [PDSpec XML path and name]     "
				+ "A PDSpec XML file to use");
	}

	/**
	 * Parses the command line, set internals flags indicating which files to save during batch
	 * mode.
	 * @param commandLineArgs A String array containing the command line arguments.
	 * @return True if successfully parsed.
	**/
	boolean parseCommandLineArgs(String commandLineArgs[]) {
		if(commandLineArgs.length == 0) {
			showUsage();
			return false;
		}
		boolean argError = false;
		for(int i = 0; i < commandLineArgs.length; i++) {
			String arg = commandLineArgs[i];
			String lowerArg = arg.toLowerCase();
			if(lowerArg.equals("-r")) {
				// The next argument should not be a command switch, expecting
				// a runspec path and name
				if((i < commandLineArgs.length - 1) && (commandLineArgs[i+1].charAt(0) != '-')) {
					File file = new File(commandLineArgs[i+1]);
					if(FileUtilities.fileExists(file)) {
						runSpecFileList.add(file);
						i++;
					} else {
						// This error will go directly to stdout since it involves parsing
						// command line options
						/**
						 * @issue The specified runspec file does not exist: [*]
						 * @explain A RunSpec file has been named on the MOVES
						 * command line but does not exist.
						**/
						System.out.println("The specified runspec file does not exist:  " +
								commandLineArgs[i+1]);
						argError = true;
					}
				} else {
					argError = true;
				}
			} else if(lowerArg.equals("-rl")) {
				// The next argument should not be a command switch, expecting
				// a runspec listfile path and name
				if((i < commandLineArgs.length - 1) && (commandLineArgs[i+1].charAt(0) != '-')) {
					File runSpecListFile = new File(commandLineArgs[i+1]);
					if(FileUtilities.fileExists(runSpecListFile)) {
						argError = !addToRunSpecFileList(runSpecListFile);
						i++;
					} else {
						// This error will go directly to stdout since it involves parsing
						// command line options
						/**
						 * @issue The specified runspec list file does not exist: [*]
						 * @explain A file listing other runspecs has been named on the MOVES
						 * command line but does not exist.
						**/
						System.out.println("The specified runspec list file does not exist:  " +
								commandLineArgs[i+1]);
						argError = true;
					}
				} else {
					argError = true;
				}
			} else if(lowerArg.equals("-i")) {
				// The next argument should not be a command switch, expecting
				// an importer path and name
				if((i < commandLineArgs.length - 1) && (commandLineArgs[i+1].charAt(0) != '-')) {
					File file = new File(commandLineArgs[i+1]);
					if(FileUtilities.fileExists(file)) {
						importerFileList.add(file);
						i++;
					} else {
						// This error will go directly to stdout since it involves parsing
						// command line options
						/**
						 * @issue The specified importer file does not exist: [*]
						 * @explain An importer XML file named on the MOVES command line does
						 * not exist.
						**/
						System.out.println("The specified importer file does not exist:  " +
								commandLineArgs[i+1]);
						argError = true;
					}
				} else {
					argError = true;
				}
			} else if(lowerArg.equals("-il")) {
				// The next argument should not be a command switch, expecting
				// an importer listfile path and name
				if((i < commandLineArgs.length - 1) && (commandLineArgs[i+1].charAt(0) != '-')) {
					File importerListFile = new File(commandLineArgs[i+1]);
					if(FileUtilities.fileExists(importerListFile)) {
						argError = !addToImporterFileList(importerListFile);
						i++;
					} else {
						// This error will go directly to stdout since it involves parsing
						// command line options
						/**
						 * @issue The specified importer list file does not exist: [*]
						 * @explain A file listing other importers has been named on the MOVES
						 * command line but does not exist.
						**/
						System.out.println("The specified importer list file does not exist:  " +
								commandLineArgs[i+1]);
						argError = true;
					}
				} else {
					argError = true;
				}
			} else if(lowerArg.equals("-p")) {
				// The next argument should not be a command switch, expecting
				// a PDSpec XML file path and name
				if((i < commandLineArgs.length - 1) && (commandLineArgs[i+1].charAt(0) != '-')) {
					File file = new File(commandLineArgs[i+1]);
					if(FileUtilities.fileExists(file)) {
						pdSpecFileList.add(file);
						i++;
					} else {
						// This error will go directly to stdout since it involves parsing
						// command line options
						/**
						 * @issue The specified importer file does not exist: [*]
						 * @explain An importer XML file named on the MOVES command line does
						 * not exist.
						**/
						System.out.println("The specified PDSpec file does not exist:  " +
								commandLineArgs[i+1]);
						argError = true;
					}
				} else {
					argError = true;
				}
			} else if(lowerArg.equals("-noworker")) {
				HeartbeatDetectionThread.isOKToLaunchWorker = false;
				System.out.println("-noworker option found");
			} else if(lowerArg.equals("-onlytodo")) {
				HeartbeatDetectionThread.isOKToLaunchWorker = false;
				MOVESEngine.theInstance.isTODOOnly = true;
				System.out.println("-onlytodo option found");
			} else if(lowerArg.equals("-maketodo")) {
				HeartbeatDetectionThread.isOKToLaunchWorker = false;
				MOVESEngine.theInstance.isTODOOnly = true;
				System.out.println("-maketodo option found");
			} else if(lowerArg.startsWith("-workerconfig=") && lowerArg.length() > "-workerconfig=".length()) {
				String alternateConfigurationFileName = arg.substring("-workerconfig=".length());
				WorkerConfiguration.CONFIGURATION_FILE_NAME = alternateConfigurationFileName;
				WorkerConfiguration.priorIDFileName = ""; 	// don't do automatic ID cleanup if launched with an
															// alternate configuration since this is likely a multiple
															// worker situation on the same computer.
				Logger.log(LogMessageCategory.INFO,"Using alternate worker configuration file: " + alternateConfigurationFileName);
			} else {
				// Encountered an unknown command line option
				argError = true;
				/**
				 * @issue Invalid command arg: [*]
				 * @explain A parameter passed to the MOVES command line is not recognized.
				**/
				System.out.println("Invalid command arg:  " + lowerArg);
				return false;
			}
		}
		if(argError) {
			/** @nonissue **/
			System.out.println("Run without any arguments to display the usage.");
		}
		return !argError;
	}

	/**
	 * Reads a list of RunSpec files from the specified path and adds to the RunSpecFileList.
	 * This function is expected to display its own errors since it's used during the parsing
	 * process.
	 * @param runSpecListFile A list file that contains the file path of one RunSpec per line.
	 * @return True on success, false if a single error encountered (such as one invalid RunSpec).
	**/
	public boolean addToRunSpecFileList(File runSpecListFile) {
		// The format should be one RunSpec per line, each RunSpec should have a valid path
		try {
			boolean isError = false;
			LineNumberReader fileReader = new LineNumberReader(
					new InputStreamReader(new BOMInputStream(new FileInputStream(runSpecListFile))));
			try {
				String iterLine;
				while ((iterLine = fileReader.readLine()) != null) {
					iterLine = iterLine.trim();
					if(iterLine.length() > 0) {
						File file = new File(iterLine);
						if(FileUtilities.fileExists(file)) {
							runSpecFileList.add(file);
						} else {
							/**
							 * @issue Error in the RunSpec list file: [*] doesn't exist
							 * @explain While using MOVES from the command line, an entry in the
							 * supplied file listing other RunSpec files references a RunSpec file
							 * that doesn not exist.
							**/
							System.out.println("Error in the RunSpec list file:  " + iterLine +
									" doesn't exist");
							isError = true;
							break;
						}
					}
				}
			} finally {
				fileReader.close();
			}

			if(isError) {
				return false;
			}
		} catch(IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}

	/**
	 * Reads a list of importer files from the specified path and adds to the importerFileList.
	 * This function is expected to display its own errors since it's used during the parsing
	 * process.
	 * @param importerListFile A list file that contains the file path of one importer XML per line.
	 * @return True on success, false if a single error encountered (such as one invalid importer XML file).
	**/
	public boolean addToImporterFileList(File importerListFile) {
		// The format should be one importer XML per line, each XML should have a valid path
		try {
			boolean isError = false;
			LineNumberReader fileReader = new LineNumberReader(
					new InputStreamReader(new BOMInputStream(new FileInputStream(importerListFile), false)));
			try {
				String iterLine;
				while ((iterLine = fileReader.readLine()) != null) {
					iterLine = iterLine.trim();
					if(iterLine.length() > 0) {
						File file = new File(iterLine);
						if(FileUtilities.fileExists(file)) {
							importerFileList.add(file);
						} else {
							/**
							 * @issue Error in the Importer list file: [*] doesn't exist
							 * @explain While using MOVES from the command line, an entry in the
							 * supplied file listing other Importer files references an importer file
							 * that does not exist.
							**/
							System.out.println("Error in the Importer list file:  " + iterLine +
									" doesn't exist");
							isError = true;
							break;
						}
					}
				}
			} finally {
				fileReader.close();
			}

			if(isError) {
				return false;
			}
		} catch(IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}

	/**
	 * Invokes MOVESEngine on each RunSpec in the batch list.
	 * @throws InterruptedException If the active thread was interrupted.
	**/
	public void runBatch() throws InterruptedException {
		for(Iterator<File> i = pdSpecFileList.iterator(); i.hasNext(); ) {
			File nextPDSpecFile = (File) i.next();
			String filePath = FileUtilities.safeGetPath(nextPDSpecFile);
			Logger.log(LogMessageCategory.INFO, "API processing PDSpec file: " + filePath);
			PDSpec pdSpec = new PDSpec();
			try {
				if(!pdSpec.readXML(nextPDSpecFile)) {
					Logger.log(LogMessageCategory.ERROR,"Unable to load PDSpec " + filePath);
					continue;
				}
			} catch(Exception e) {
				Logger.logError(e,"Unable to load PDSpec " + filePath);
				continue;
			}
			runPDSpec(pdSpec);
		}
		for(Iterator<File> i = importerFileList.iterator(); i.hasNext(); ) {
			File nextImporterFile = (File) i.next();
			String filePath = FileUtilities.safeGetPath(nextImporterFile);
			Logger.log(LogMessageCategory.INFO, "API processing Importer file: " + filePath);
			ArrayList<ImporterManager> importerManagers = ImporterManager.fromXML(filePath);
			if(importerManagers == null || importerManagers.size() <= 0) {
				continue;
			}
			for(Iterator<ImporterManager> j=importerManagers.iterator();j.hasNext();) {
				ImporterManager manager = (ImporterManager)j.next();
				ArrayList<String> messages = new ArrayList<String>();
				manager.doImport(messages);
				for(Iterator<String> k=messages.iterator();k.hasNext();) {
					String message = (String)k.next();
					Logger.log(LogMessageCategory.INFO, message);
				}
			}
		}
		for(Iterator<File> i = runSpecFileList.iterator(); i.hasNext(); ) {
			File nextRunSpecFile = (File) i.next();
			//RunSpec now output by MOVESEngine
			//Logger.log(LogMessageCategory.INFO, "API processing RunSpec: "
			//		+ FileUtilities.safeGetPath(nextRunSpecFile));
			if(loadRunSpec(nextRunSpecFile, false)) {
				startMOVESEngine();

				// Wait for the engine to complete.
				MOVESEngine.theInstance.join();
			}
		}
	}

	/**
	 * Access method to the member variable.
	 * @return The internal runSpec object..
	**/
	public RunSpec getRunSpec() {
		return runSpec;
	}

	/**
	 * Loads the active RunSpec object from the specified file name, and initializes
	 * database connections.
	 * @param newRunSpecFilePath Indicates which file to load.
	 * @param isGUIMode Boolean value indicates if the RunSpec is loaded from MOVES in GUI mode
	 * or in batch mode.  If in GUI mode and the RunSpec's output database name is invalid, this
	 * function will log the error but will return successfully.  This allows MOVESWindow to
	 * continue using the newly-loaded RunSpec object.  If batch mode, this function will always
	 * try to "safely" create the specified output database.
	 * @return true on success.
	**/
	public boolean loadRunSpec(File newRunSpecFilePath, boolean isGUIMode) {
		RunSpec newRunSpec = new RunSpec();
		RunSpecXML runSpecXML = new RunSpecXML(newRunSpec);
		if(!runSpecXML.load(newRunSpecFilePath)) {
			return false;
		}

		runSpecFilePath = newRunSpecFilePath;
		runSpec = newRunSpec;
		return true;
	}

	/**
	 * Saves the internal RunSpec object to a file.
	 * @return True on success.
	**/
	public boolean saveRunSpec() {
		try {
			// first check if the file can be written to (if it already exists)
			boolean fileExists = FileUtilities.fileExists(runSpecFilePath);
			if(fileExists && !runSpecFilePath.canWrite()) {
				/**
				 * @issue RunSpec file error: [*] is read only, cannot save.
				 * @explain An existing RunSpec file is marked Read-Only and cannot be
				 * overwritten.  Commonly, files pulled from CDs are set to Read-Only.
				 * Access the file properties and remove the Read-Only flag.
				**/
				Logger.log(LogMessageCategory.ERROR, "RunSpec file error:  "
					+ FileUtilities.safeGetPath(runSpecFilePath)
					+ " is read only, cannot save.");
				return false;
			}
			RunSpecXML runSpecXML = new RunSpecXML(runSpec);
			runSpecXML.save(runSpecFilePath);
			return true;
		} catch (Exception exception) {
			/**
			 * @explain A RunSpec could not be saved.  Check for rights to the folder
			 * to hold the file.
			**/
			Logger.logError(exception, "Exception occurred writing runspec to " + runSpecFilePath +
					".");
			return false;
		}
	}

	/**
	 * Sets the path and name of the internal RunSpec object.
	 * @param newFilePath The file path
	**/
	public void setRunSpecFilePath(File newFilePath) {
		runSpecFilePath = newFilePath;
	}

	/**
	 * Gets the path and name of the internal RunSpec object.
	 * @return The file path
	**/
	public File getRunSpecFilePath() {
		return runSpecFilePath;
	}

	/**
	 * Starts the MOVESEngine.
	 * @return true if the engine started executing the active RunSpec
	**/
	public boolean startMOVESEngine() {
		// NR_IMP: start differently based on model used.
		//         need to check the command line starting later too!
		Models.ModelCombination mc = runSpec.getModelCombination();
		switch (mc) {
			case M1:
				return startMOVESEngineOnroad();
			case M2:
				return startMOVESEngineNonroad();
			default:
				return false; // not support yet for now
		}
	}

	/**
	 * Starts the MOVESEngine for Nonroad operations.
	 * @return true if the engine started executing the active RunSpec
	**/
	private boolean startMOVESEngineNonroad() {
		if(runSpec.domain != ModelDomain.NATIONAL_ALLOCATION) {
			Logger.log(LogMessageCategory.ERROR,"NONROAD only supports DEFAULT domain/scale.");
			return false;
		}

		String fileName = "(unnamed)";
		if (getRunSpecFilePath() != null) {
			fileName = FileUtilities.safeGetPath(runSpecFilePath);
		}
		try {
			return MOVESEngine.theInstance.launch(runSpec,fileName);
		} catch (Exception exception) {
			/**
			 * @explain An error occurred while starting a simulation.  The simulation has
			 * been stopped.
			 **/
			Logger.logError(exception, "Error launching RunSpec");
		}
		return false;
	}

	/**
	 * Starts the MOVESEngine for Onroad operations.
	 * @return true if the engine started executing the active RunSpec
	**/
	private boolean startMOVESEngineOnroad() {
		try {
			if(runSpec.domain != ModelDomain.NATIONAL_ALLOCATION) {
				// Check the runspec for completeness vs what a county-domain runspec should have
				ArrayList<String> messages = new ArrayList<String>();
				int result = ImporterManager.isReadyForCountyDomain(runSpec,messages);
				if(result < 0) {
					// Log the error messages
					for(Iterator<String> i=messages.iterator();i.hasNext();) {
						/** @nonissue **/
						Logger.log(LogMessageCategory.ERROR,(String)i.next());
					}
					return false;
				}

				// Check the runspec's domain database for completeness
				Connection db = null;
				if(runSpec.scaleInputDatabase != null) {
					db = runSpec.scaleInputDatabase.openConnectionOrNull();
				}
				if(null == db) {
					/**
					 * @issue Unable to open domain database
					 * @explain A database listed in the RunSpec could not be accessed.  Check
					 * the database availability on your database server.
					**/
					Logger.log(LogMessageCategory.ERROR,"Unable to open domain database.");
					return false;
				}
				try {
                    // first, run the QA that would provide pop-up error messages when loading a runspec
					Logger.log(LogMessageCategory.INFO, "Performing compatibility checks between RunSpec and domain database...");
					messages.clear();
					result = ImporterManager.isCountyDomainDatabase(runSpec,messages,db,true);
					if(result < 0) {
						// Log the error messages
						for(Iterator<String> i = messages.iterator(); i.hasNext(); ) {
							Logger.log(LogMessageCategory.ERROR, i.next());
						}
						return false;
					}
					Logger.log(LogMessageCategory.INFO, "Done performing compatibility checks between RunSpec and domain database.");

                    // then, run the QA that would result in a red X for the scale input database (preventing a GUI run)
                    if(!runSpec.skipDomainDatabaseValidation) {
                        Logger.log(LogMessageCategory.INFO, "Performing domain database validation...");
                        messages.clear();
                        // create the manager, setting the domain (since some checks rely on knowing that info) and instantiating
                        ImporterManager manager = new ImporterManager(runSpec);
                        if(runSpec.domain == ModelDomain.SINGLE_COUNTY) {
                            manager.setAsCountyDomain();
                        } else if (runSpec.domain == ModelDomain.PROJECT) {
                            manager.setAsProjectDomain();
                        }
                        manager.instantiate(null);
                        // ready to run the checks
                        result = manager.performAllImporterChecks(runSpec,messages,db);
                        if(result < 0) {
                            // Log the error messages
                            for(Iterator<String> i = messages.iterator(); i.hasNext(); ) {
                                Logger.log(LogMessageCategory.ERROR, i.next());
                            }
                            return false;
                        }
                        Logger.log(LogMessageCategory.INFO, "Done performing domain database validation.");
                    } else {
                        Logger.log(LogMessageCategory.WARNING, "Skipping domain database validation.");
                    }
				} finally {
					DatabaseUtilities.closeConnection(db);
					db = null;
				}
			}

			String fileName = "(unnamed)";
			if (getRunSpecFilePath() != null) {
				fileName = FileUtilities.safeGetPath(runSpecFilePath);
			}
			return MOVESEngine.theInstance.launch(runSpec,fileName);
		} catch (Exception exception) {
			/**
			 * @explain An error occurred while starting a simulation.  The simulation has
			 * been stopped.
			**/
			Logger.logError(exception, "Error launching RunSpec");
		}
		return false;
	}

	/**
	 * Check validity of folders in a PDSpec
	 * @return true if all entries are acceptable
	**/
	private boolean checkPDSpec(PDSpec pdSpec) {
		pdSpec.expandWildcards();

		// Ensure each entry in pdSpec points to a valid directory
		boolean allFoldersExist = true;
		for(Iterator<PDSpec.PDSpecEntry> i=pdSpec.entries.iterator();i.hasNext();) {
			PDSpec.PDSpecEntry entry = i.next();
			File f = new File(entry.pickupFolderName);
			if(!f.exists() || !f.isDirectory()) {
				Logger.log(LogMessageCategory.ERROR,"ERROR: PDSpec entry has invalid pickup folder: " + entry.pickupFolderName);
				allFoldersExist = false;
			}
		}
		if(!allFoldersExist) {
			Logger.log(LogMessageCategory.ERROR,"ERROR: One or more pickup folders are invalid, no DONE files will be collected.");
			return false;
		}
		return true;
	}

	/**
	 * Called when the engine has completed, used to iterate to the next PDSpec entry.
	 * @param engine MOVESEngine instance that just finished
	**/
	public void engineCompleted(MOVESEngine engine) {
		synchronized(this) {
			if(!isExecutingPDSpec || shouldStopPDSpec || iteratingPDSpec == null) {
				return;
			}
		}
		if(pdSpecEntryIndex < 0) {
			pdSpecEntryIndex = 0;
		} else {
			pdSpecEntryIndex++;
		}
		if(pdSpecEntryIndex < iteratingPDSpec.entries.size()) {
			PDSpec.PDSpecEntry entry = iteratingPDSpec.entries.get(pdSpecEntryIndex);
			try {
				MOVESEngine.theInstance.launch(entry,pdSpecEntryIndex == iteratingPDSpec.entries.size() - 1);
			} catch(Exception e) {
				Logger.logError(e,"Unable to start the next PDSpec entry");
			}
		}
	}

	/**
	 * Execute all entries in a PDSpec asynchronously
	 * @return true if processing was started
	**/
	public boolean runPDSpecAsync(PDSpec pdSpec) {
		try {
			if(!checkPDSpec(pdSpec)) {
				return false;
			}
			if(pdSpec.entries.size() <= 0) {
				return false; // nothing to do, so don't even start.
			}

			// Run each entry in pdSpec
			isExecutingPDSpec = true;
			shouldStopPDSpec = false;
			iteratingPDSpec = pdSpec;
			pdSpecEntryIndex = -1;
			engineCompleted(MOVESEngine.theInstance); // start the first iteration

			return true;
		} catch (Exception exception) {
			/**
			 * @explain An error occurred while collecting DONE files.  No further files will be collected.
			**/
			Logger.logError(exception, "An error occurred while collecting DONE files.  No further files will be collected.");
		}
		return false;
	}

	/**
	 * Execute all entries in a PDSpec
	 * @return true if all entries were processed, even if they resulted in no DONE
	 * files being retrieved.
	**/
	public boolean runPDSpec(PDSpec pdSpec) {
		try {
			if(!checkPDSpec(pdSpec)) {
				return false;
			}

			// Run each entry in pdSpec
			isExecutingPDSpec = true;
			shouldStopPDSpec = false;
			for(int i=0;i<pdSpec.entries.size();i++) {
				PDSpec.PDSpecEntry entry = pdSpec.entries.get(i);
				MOVESEngine.theInstance.launch(entry,i == pdSpec.entries.size() - 1);
				// Wait for the engine to complete.
				MOVESEngine.theInstance.join();
				synchronized(this) {
					if(shouldStopPDSpec) {
						break;
					}
				}
			}
			return true;
		} catch (Exception exception) {
			/**
			 * @explain An error occurred while collecting DONE files.  No further files will be collected.
			**/
			Logger.logError(exception, "An error occurred while collecting DONE files.  No further files will be collected.");
		}
		return false;
	}

	/**
	 * Stops the MOVESEngine.
	 * @throws InterruptedException If the active thread was interrupted.
	**/
	public void stopMOVESEngine() throws InterruptedException {
		synchronized(this) {
			if(isExecutingPDSpec) {
				shouldStopPDSpec = true;
			}
		}
		MOVESEngine.theInstance.signalToTerminate();
		MOVESEngine.theInstance.join();
	}

	/**
	 * Pauses the MOVESEngine.
	**/
	public void pauseMOVESEngine() {
		MOVESEngine.theInstance.pause();
	}

	/**
	 * Resumes the MOVESEngine.
	**/
	public void resumeMOVESEngine() {
		MOVESEngine.theInstance.resume();
	}

	/**
	 * Used to send notifications about a MOVESEngines' progress to implementers.
	 * @param inEngine The associated MOVESEngine.
	**/
	public void engineProgressUpdate(MOVESEngine inEngine) {
	}

	/**
	 * Called when the MOVESEngine object is completing
	 * @param inEngine The associated MOVESEngine.
	**/
	public void engineIsCompleting(MOVESEngine inEngine) {
	}

	/**
	 * Gets the "base" name of the RunSpec as determine from its path and name.
	 * @return The "base" name of the RunSpec, (i.e. the filename without extension).
	**/
	public String getRunSpecBaseName() {
		String baseName = runSpecFilePath.getName();
		int dotIndex = baseName.lastIndexOf(".");
		if(dotIndex > -1) {
			return baseName.substring(0, dotIndex);
		}
		return baseName;
	}
}
