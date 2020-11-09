/**************************************************************************************************
 * @(#)WorkerGUI.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker.gui;

import gov.epa.otaq.moves.common.Configuration;
import gov.epa.otaq.moves.common.InstanceCounter;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.PerformanceProfiler;
import gov.epa.otaq.moves.common.MOVESThread;
import gov.epa.otaq.moves.worker.framework.WorkerConfiguration;
import gov.epa.otaq.moves.worker.framework.*;
import java.net.*;
import java.util.LinkedList;
import java.util.Iterator;
import gov.epa.otaq.moves.master.gui.MOVESWindow;

/**
 * Main module for the distributed worker system.
 *
 * @author		Wesley Faler
 * @author 		Tim Hull
 * @version		2013-01-31
**/
public class WorkerGUI {
	/** Name of output file that performance profiles are written to **/
	static final String PERFORMANCE_PROFILER_FILE_NAME = "workerprofile.txt";
	/** Window displayed by the worker **/
	public static WorkerWindow workerWindow = null;
	/** Worker started in standalone mode. **/
	public static boolean isStandAlone = true;
	/** TCP/IP port used on the socket designating a running worker's presense **/
	private static final int WORKER_FLAG_PORT = 13132;
	/** TCP/IP socket used to designate a running worker's presense **/
	private static ServerSocket workerSocket = null;
	/** Depth counter for designating a running worker's presense **/
	private static int workerSocketCount = 0;
	/**
	 * The worker threads that this object is using. This object will wait for these
	 * threads to complete before application termination.
	 * All elements inherit from MOVESThread.
	**/
	public static LinkedList<MOVESThread> workerThreads = new LinkedList<MOVESThread>();

	/**
	 * Application Entry point
	 * @param args Command line arguments.
	**/
	public static void main(String[] args) {
		Configuration.checkEnvironment();
		if(!Configuration.allowGUI) {
			commandLineMain(args);
			return;
		}
		Logger.log(LogMessageCategory.DEBUG, "Starting WorkerGUI...");
		if(hasWorkerOnThisComputer()) {
			Logger.log(LogMessageCategory.DEBUG, "Caution: Another MOVES Worker is already running on this computer");
			//return;
		}
		InstanceCounter.setup(); // must be done before loading configuration data
		WorkerConfiguration.clearPriorIDs();
		WorkerConfiguration.theWorkerConfiguration.initComputerIDAndPath();

		try {
			WorkerConfiguration.theWorkerConfiguration.loadConfigurationData();
		} catch (Exception exception) {
			Logger.log(LogMessageCategory.ERROR, exception.getClass().getName()
					+ " thrown while attempting to create WorkerConfiguration: "
					+ exception.getMessage());
			if(isStandAlone) {
				MOVESThread.signalAllToTerminate();
				InstanceCounter.shutdown();
				System.exit(0);
			}
			InstanceCounter.shutdown();
			return;
		}

		workerWindow = new WorkerWindow();
		setupFlagForWorker();
	}

	/**
	 * Entry point for non-GUI version
	 * @pram args Command line arguments
	**/
	public static void commandLineMain(String[] args) {
		Configuration.allowGUI = false;
		Logger.log(LogMessageCategory.DEBUG, "Starting WorkerCommandLine...");

		// Check for flag indicating automatic shutdown should be used
		boolean isAutoShutdownMode = false;
		long shutdownIncrementMillis = 0;
		String alternateConfigurationFileName = null;
		if(args != null && args.length > 0) {
			for(int i=0;i<args.length;i++) {
				String arg = args[i].toLowerCase().trim();
				if(arg.equalsIgnoreCase("-a") || arg.equalsIgnoreCase("-autoshutdown")) {
					isAutoShutdownMode = true;
				} else if(arg.startsWith("-config=") && arg.length() > "-config=".length()) {
					alternateConfigurationFileName = arg.substring("-config=".length());
				} else if(arg.equalsIgnoreCase("-l") || arg.equalsIgnoreCase("-longshutdown")) {
					isAutoShutdownMode = true;
					shutdownIncrementMillis = 2L*60L*60L*1000L; // 2 hours
				}
			}
		}
		RemoteEmissionsCalculator.isAutoShutdownMode = isAutoShutdownMode;
		if(isAutoShutdownMode && shutdownIncrementMillis > 0) {
			RemoteEmissionsCalculator.shutdownIncrementMillis = shutdownIncrementMillis;
		}
		if(alternateConfigurationFileName != null && alternateConfigurationFileName.length() > 0) {
			WorkerConfiguration.CONFIGURATION_FILE_NAME = alternateConfigurationFileName;
			WorkerConfiguration.priorIDFileName = ""; 	// don't do automatic ID cleanup if launched with an
														// alternate configuration since this is likely a multiple
														// worker situation on the same computer.
			Logger.log(LogMessageCategory.INFO,"Using alternate worker configuration file: " + alternateConfigurationFileName);
		}

		if(hasWorkerOnThisComputer()) {
			Logger.log(LogMessageCategory.DEBUG, "Caution: Another MOVES Worker is already running on this computer");
			//return;
		}
		InstanceCounter.setup(); // must be done before loading configuration data
		WorkerConfiguration.clearPriorIDs();
		WorkerConfiguration.theWorkerConfiguration.initComputerIDAndPath();

		try {
			WorkerConfiguration.theWorkerConfiguration.loadConfigurationData();
		} catch (Exception exception) {
			Logger.log(LogMessageCategory.ERROR, exception.getClass().getName()
					+ " thrown while attempting to create WorkerConfiguration: "
					+ exception.getMessage());
			if(isStandAlone) {
				MOVESThread.signalAllToTerminate();
				InstanceCounter.shutdown();
				System.exit(0);
			}
			InstanceCounter.shutdown();
			return;
		}

		// Startup threads
		startupThreads(null);
		setupFlagForWorker();

		// If we should automatically shutdown, then wait for all threads so we all shutdown nicely.
		// This also ensures the calling program that is waiting on main() actually waits on the whole
		// set of functionality.
		if(isAutoShutdownMode) {
			Iterator workerThreadIterator = workerThreads.iterator();
			while (workerThreadIterator.hasNext()) {
				MOVESThread iterThread = (MOVESThread)workerThreadIterator.next();
				try {
					//Logger.log(LogMessageCategory.INFO,"Command-line waiting for internal thread "+iterThread.getClass());
					System.out.flush();
					iterThread.join();
				} catch (InterruptedException exception) {
					// Nothing to do here
				}
			}
		}
	}

	public static void startupThreads(WorkerWindow window) {
		// Show version information
		Logger.log(LogMessageCategory.INFO,"Worker Release: " + MOVESWindow.MOVES_VERSION);
		Logger.log(LogMessageCategory.INFO,"Worker Computer ID: "
				+ WorkerConfiguration.theWorkerConfiguration.computerID);
		Logger.log(LogMessageCategory.INFO,"Worker ID: "
				+ WorkerConfiguration.theWorkerConfiguration.distributedWorkerId);

		// Launch worker threads.
		MOVESThread theThread;
		theThread = new WorkerHeartbeatThread();
		WorkerGUI.workerThreads.addLast(theThread);
		theThread.start();
		theThread = new RemoteEmissionsCalculator(window);
		WorkerGUI.workerThreads.addLast(theThread);
		theThread.start();
	}

	public static void stopThreads() {
		stopThreads(null);
	}

	public static void stopThreads(MOVESThread threadToIgnore) {
		Logger.log(LogMessageCategory.INFO,"Signalling worker threads to quit");
		System.out.flush();
		Iterator workerThreadIterator = workerThreads.iterator();
		while (workerThreadIterator.hasNext()) {
			MOVESThread iterThread = (MOVESThread)workerThreadIterator.next();
			if(iterThread != threadToIgnore) {
				Logger.log(LogMessageCategory.INFO,"Signalling worker thread to quit "+iterThread.getClass());
				System.out.flush();
				iterThread.signalToTerminate();
			}
		}

		Logger.log(LogMessageCategory.INFO,"Waiting for worker threads to quit");
		System.out.flush();
		workerThreadIterator = workerThreads.iterator();
		while (workerThreadIterator.hasNext()) {
			MOVESThread iterThread = (MOVESThread)workerThreadIterator.next();
			if(iterThread != threadToIgnore) {
				try {
					Logger.log(LogMessageCategory.INFO,"Waiting for thread to quit "+iterThread.getClass());
					System.out.flush();
					iterThread.join();
				} catch (InterruptedException exception) {
					// Nothing to do here
				}
			}
		}

		Logger.log(LogMessageCategory.INFO,"Completing worker close");
		System.out.flush();

		InstanceCounter.shutdown(); // all worker shutdown paths lead to this point
	}

	/**
	 * Close the window or application.
	**/
	public static void close() {
		if(workerWindow != null) {
			workerWindow.closeWindow();
		} else {
			stopThreads();
		}
	}

	/** Called when the main worker window is closing.**/
	public static void workerWindowIsClosing() {
		WorkerConfiguration.theWorkerConfiguration.cleanupTemporaryDirectory();
		PerformanceProfiler.writeProfiles(PERFORMANCE_PROFILER_FILE_NAME);
		workerWindow = null;
		shutdownFlagForWorker();
		WorkerConfiguration.clearPriorIDs();
		if(isStandAlone) {
			MOVESThread.signalAllToTerminate();
			System.exit(0);
		}
	}

	/** Determine if a running worker is present on this computer **/
	public static boolean hasWorkerOnThisComputer() {
		ServerSocket s = null;
		try {
			s = new ServerSocket(WORKER_FLAG_PORT);
			// If we got this far, there was no worker already running
			return false;
		} catch(Exception e) {
			// Nothing to do here, these are normal and expected if a worker is running
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

	/** Indicate that a running worker is present on this computer **/
	public static void setupFlagForWorker() {
		if(workerSocketCount == 0) {
			try {
				workerSocket = new ServerSocket(WORKER_FLAG_PORT);
				Logger.log(LogMessageCategory.INFO,"Setting worker flag for " + WorkerConfiguration.theWorkerConfiguration.distributedWorkerId + 
												   " on port " + WORKER_FLAG_PORT);
			} catch(Exception e) {
				if(e.toString().toLowerCase().contains("jvm_bind") || e.toString().toLowerCase().contains("net_bind")) {
					Logger.log(LogMessageCategory.INFO,"Not setting worker flag for " + WorkerConfiguration.theWorkerConfiguration.distributedWorkerId + 
													   " because port " + WORKER_FLAG_PORT + " is already bound by another worker.");
				} else {
					// a different error has occurred
					Logger.logError(e,"Unable to setup flag for worker's presence");
				}
			}
		}
		workerSocketCount++;
	}

	/** Remove the indication that a running worker is present on this computer **/
	public static void shutdownFlagForWorker() {
		workerSocketCount--;
		if(workerSocketCount <= 0) {
			if(workerSocket != null) {
				try {
					workerSocket.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				workerSocket = null;
			}
		}
	}
}
