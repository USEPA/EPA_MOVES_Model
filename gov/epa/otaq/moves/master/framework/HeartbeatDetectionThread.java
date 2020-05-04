/**************************************************************************************************
 * @(#)HeartbeatDetectionThread.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.worker.gui.WorkerGUI;
import gov.epa.otaq.moves.worker.framework.WorkerConfiguration;
import java.io.*;
import java.util.*;

/**
 * Detects work files that have been claimed by a distributed worker but haven't been processed
 * within a certain timeout period. Renames these files so that other distributed systems can
 * process them.<p>When a worker claims a file for processing, it renames the file with an
 * IN_PROGRESS status and periodically updates the file's last modified date. If the worker dies
 * or looses its network connection, it stops updating the file's date. The Heartbeat Detection
 * Thread checks the date on IN_PROGRESS files and renames them with a TO_DO status if the file
 * has not been updated for a length of time. If the worker re-starts or gets it connection back,
 * it will find that the files has been "reclaimed" by the Master.
 *
 * @author		Wesley Faler
 * @version		2017-08-30
**/
public class HeartbeatDetectionThread extends MOVESThread {
	/**
	 * Local class that expresses both a file's time stamp and the last time that it
	 * was detected as changing.
	**/
	class ObservedFileStamp {
		/**
		 * The file's time stamp. Don't interpret this value since it is synchronized to
		 * a different system's clock.
		**/
		long fileTimeStamp;

		/**
		 * The last time that a change in the file's time stamp was observed.
		 * Upon initial file detection, this is set to the current time.
		**/
		Date lastObservedChange;

		/**
		 * The last time that a file was seen to exist.
		 * Upon initial file detection, this is set to the current time.
		**/
		Date lastSeenToExist;
	};

	/**
	 * Const value for the length of time that the master will wait before considering a worker
	 * to be "dead" and reclaiming its work file.
	**/
	static public final long WORKER_HEARTBEAT_TIMEOUT_MS = 10 * 60 * 1000;

	/**
	 * Maximum amount of time the master will let a worker go without having a Worker ID
	 * file before the master will consider the worker to be dead.
	**/
	static public final long WORKER_NO_HEARTBEAT_FILE_TIMEOUT_MS = 10 * 60 * 1000;

	/** Minimum milliseconds allowed between attempts to start a worker if none are present **/
	static public final long MILLS_BETWEEN_WORKER_START_ATTEMPTS = 5 * 60 * 1000;

	/** Number of known active workers **/
	static int howManyActiveWorkers = 0;

	/** True if a new worker can be started whenever workers don't exist **/
	static public boolean isOKToLaunchWorker = true;

	/**
	 * The last polled state of the worker ID files.
	 * Maps from String (file name) to ObservedFileStamp (local class)
	**/
	TreeMap<String,ObservedFileStamp> workerPollResults = new TreeMap<String,ObservedFileStamp>();

	/** the time in milliseconds of the most recent attempt to start a Worker **/
	long lastWorkerStartAttemptTimeMillis = 0;

	/** true if a local worker is detected or has been created **/
	boolean hasLocalWorker = false;

	/** time at which the file scan is deemed to not have occurred frequently enough **/
	long fileScanTimeout = 0;

	/** true after any prior local worker IDs have been removed **/
	boolean didExamineLocalWorkerHistory = false;

	/**
	 * Standard constructor
	**/
	public HeartbeatDetectionThread() {
	}

	/**
	 * Performs thread start up code
	 * @return Should the thread continue to run
	**/
	protected boolean startupInThread() {
		return true;
	}

	/**
	 * This performs one iteration of thread functionality. Upon return, the MOVESEngineThread
	 * will automatically check for termination conditions.
	 * @return Should the thread continue to run
	**/
	protected boolean threadIterationGo() {
		try {
			threadIterationGoInternal();
		} catch(Exception e) {
			/** @nonissue **/
			Logger.logException(LogMessageCategory.ERROR,e);
			workerPollResults.clear();
		} catch(Error e) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.ERROR,e.toString());
			workerPollResults.clear();
		}

		return true;
	}

	/**
	 * Called by theadIterationGo() to perform all real work.
	**/
	protected void threadIterationGoInternal() {
		if(!didExamineLocalWorkerHistory) {
			didExamineLocalWorkerHistory = true;
			if(!WorkerGUI.hasWorkerOnThisComputer()) {
				// Since there is no currently running local worker, clear any
				// prior local worker IDs.  If there is running local worker, it
				// will have already cleared the IDs and setup its own, which
				// should not be cleared just yet.
				WorkerConfiguration.clearPriorIDs();
				Logger.log(LogMessageCategory.INFO,"HeartbeatDetectionThread !hasWorkerOnThisComputer");
			}
		}

		File[] workerIDFiles = SystemConfiguration.getTheSystemConfiguration().
				sharedDistributedFolderPath.listFiles(
				DistributedIDBroker.getWorkerIDFilenameFilter());
		if(workerIDFiles == null) {
			Logger.log(LogMessageCategory.INFO,"HeartbeatDetectionThread workerIDFiles==null");
			// If we couldn't get a listing, not even an empty listing, then don't
			// check timeouts.  We couldn't do anything about them anyway since
			// the connection to the networked shared folder path is broken.
			// Flag the date/time stamps memorized in the previous successful scan as
			// valid for one more minute. After that, they should be wiped clean.
			if(fileScanTimeout == 0) {
				fileScanTimeout = System.currentTimeMillis() + (60*1000);
			}
			return;
		}
		//Logger.log(LogMessageCategory.INFO,"HeartbeatDetectionThread workerIDFiles# "+workerIDFiles.length);

		if(workerIDFiles.length > 0) { // If there are worker heartbeat files found...
			// If it has been too long since the last system scan, then reset
			// all worker information as the date/time data is out of date.
			if(fileScanTimeout > 0 && fileScanTimeout <= System.currentTimeMillis()) {
				workerPollResults.clear();
				fileScanTimeout = 0;
			}
		}

		// First, do a check based on the heartbeat files that are actually present.
		// If a dead worker is found at this point, then delete its heartbeat file
		// as well as recover its "In Progress" files.
		for(int i = 0; i < workerIDFiles.length; i++) {
			String workerID = DistributedIDBroker.getIDFromFile(workerIDFiles[i]);
			ObservedFileStamp fileStamp = (ObservedFileStamp)workerPollResults.get(workerID);

			if(fileStamp == null) {
				fileStamp = new ObservedFileStamp();
				fileStamp.fileTimeStamp = workerIDFiles[i].lastModified();
				fileStamp.lastObservedChange = new Date();
			} else {
				if(fileStamp.fileTimeStamp == workerIDFiles[i].lastModified()) {
					// Time stamp hasn't changed. Has it been the same for longer than the time out?
					Date currentTime = new Date();

					if(currentTime.getTime() - fileStamp.lastObservedChange.getTime() >
							WORKER_HEARTBEAT_TIMEOUT_MS) {
						// This worker has timed out.
						timeOutWorker(workerIDFiles[i]);
						workerPollResults.remove(workerID);
						fileStamp = null;
					}
				} else {
					fileStamp.lastObservedChange = new Date();
					fileStamp.fileTimeStamp = workerIDFiles[i].lastModified();
					
					//Logger.log(LogMessageCategory.DEBUG, "Observed file timestamp change on " + workerID);
				}
			}

			if(fileStamp != null) {
				fileStamp.lastSeenToExist = new Date();
				workerPollResults.put(workerID, fileStamp);
			}
		}

		// Second, look again through our worker IDs.  This covers workers that we knew
		// about previously but that no longer have heartbeat files.  If a dead worker
		// is found here, then just recover its "In Progress" files (since it doesn't have
		// a heartbeat file).
		LinkedList<String> workerIDsToRemove = new LinkedList<String>();
		Set<String> knownWorkerIDs = workerPollResults.keySet();
		for(Iterator<String> i=knownWorkerIDs.iterator();i.hasNext();) {
			String workerID = (String)i.next();
			ObservedFileStamp fileStamp = (ObservedFileStamp)workerPollResults.get(workerID);
			if(fileStamp != null) {
				Date currentTime = new Date();

				if(currentTime.getTime() - fileStamp.lastObservedChange.getTime() >
						WORKER_HEARTBEAT_TIMEOUT_MS
						||
						currentTime.getTime() - fileStamp.lastSeenToExist.getTime() >
						WORKER_NO_HEARTBEAT_FILE_TIMEOUT_MS) {
					// This worker has timed out.
					/**
					 * @issue Detected dead worker: [*]
					 * @explain A worker computer has been unresponsive too long.  Work assigned
					 * to that worker will be reclaimed, being made accessible to other workers.
					**/
					Logger.log(LogMessageCategory.DEBUG, "Detected dead worker: " + workerID);
					timeOutWorker(workerID);
					// Remember which workers we've timed out.  Removing them from
					// workerPollResults right now would upset the Iterator being used.
					workerIDsToRemove.add(workerID);
				}
			}
		}
		// Remove knowledge of workers that were timed out above.
		for(ListIterator<String> i=workerIDsToRemove.listIterator();i.hasNext();) {
			String workerID = (String)i.next();
			workerPollResults.remove(workerID);
		}

		// Start a worker if needed
		if(isOKToLaunchWorker &&
				workerPollResults.size() <= 0 && MOVESEngine.theInstance != null
				&& MOVESEngine.theInstance.isRunning()) {
			long now = System.currentTimeMillis();
			if(now - lastWorkerStartAttemptTimeMillis >= MILLS_BETWEEN_WORKER_START_ATTEMPTS) {
				lastWorkerStartAttemptTimeMillis = now;
				if(!hasLocalWorker) {
					if(!WorkerGUI.hasWorkerOnThisComputer()) {
						WorkerGUI.isStandAlone = false;
						WorkerGUI.main(null); 	// may return without starting another process if
												// a worker is already on the local computer.
					}
					hasLocalWorker = true;
				}
			}
		}

		howManyActiveWorkers = workerPollResults.size();
		// A successful scan was performed, so do not flag the data with an expiration date.
		fileScanTimeout = 0;
	}

	/**
	 * Times out the specified worker. Reverts all temporary files it has reserved to the
	 * "TODO" pool.
	 * @param workerIDFile File object to the Worker's heartbeat file
	**/
	void timeOutWorker(File workerIDFile) {
		Logger.log(LogMessageCategory.INFO,"HeartbeatDetectionThread.timeOutWorker file "+workerIDFile.getName());
		String workerID = DistributedIDBroker.getIDFromFile(workerIDFile);
		/**
		 * @issue Detected dead worker: [*]
		 * @explain A worker computer has been unresponsive too long.  Work assigned
		 * to that worker will be reclaimed, being made accessible to other workers.
		**/
		Logger.log(LogMessageCategory.DEBUG, "Detected dead worker: " + workerID);

		if(!workerIDFile.delete()) {
			/**
			 * @issue Failed to delete ID file: [*]
			 * @explain A worker computer has been unresponsive too long but an error
			 * occurred while removing its ID file from the shared work folder.
			**/
			Logger.log(LogMessageCategory.DEBUG, "Failed to delete ID file: " + workerIDFile.getName());
		}

		timeOutWorker(workerID);
	}

	/**
	 * Times out the specified worker. Reverts all temporary files it has reserved to the
	 * "TODO" pool.
	 * @param workerID Unique identifier of the worker as taken from the Worker's heartbeat
	 * file's name
	**/
	void timeOutWorker(String workerID) {
		Logger.log(LogMessageCategory.INFO,"HeartbeatDetectionThread.timeOutWorker "+workerID);

		FilenameFilter fileNameFilter = DistributedWorkFileName.buildFileNameFilter
			(SystemConfiguration.getTheSystemConfiguration().distributedMasterID,
			"*", DistributedWorkFileState.IN_PROGRESS, workerID);

		File[] orphanedProgressFiles = SystemConfiguration.getTheSystemConfiguration().
				sharedDistributedFolderPath.listFiles(fileNameFilter);

		Logger.log(LogMessageCategory.INFO,"HeartbeatDetectionThread.timeOutWorker orphanedProgressFiles# "+orphanedProgressFiles.length);

		for(int i = 0; i < orphanedProgressFiles.length; i++) {
			Logger.log(LogMessageCategory.INFO,"HeartbeatDetectionThread.timeOutWorker orphanedProgressFile "+orphanedProgressFiles[i].getName());
			DistributedWorkFileName parsedFileName = DistributedWorkFileName.createFrom(
					orphanedProgressFiles[i].getName());

			if(parsedFileName != null) {
				parsedFileName.state = DistributedWorkFileState.TODO;
				File newFilePath = new File(orphanedProgressFiles[i].getParent(),
						parsedFileName.toString());
				if(!orphanedProgressFiles[i].renameTo(newFilePath)) {
					/**
					 * @issue Failed to rename orphaned file: [*]
					 * @explain A file marked as in progress by a worker has been declared
					 * orphaned by a worker that terminated.  An attempt to rename the file
					 * back to TODO has failed.
					**/
					Logger.log(LogMessageCategory.DEBUG,"Failed to rename orphaned file: " + orphanedProgressFiles[i].getName());
				} else {
					Logger.log(LogMessageCategory.INFO,"HeartbeatDetectionThread.timeOutWorker renamed orphaned file "+orphanedProgressFiles[i].getName());
				}
			} else {
				Logger.log(LogMessageCategory.DEBUG,"Failed to parse progress file name: " + orphanedProgressFiles[i].getName());
			}
		}
	}

	/**
	 * Called directly after ThreadIterationGo. Subclasses typically implement a simple wait
	 * operation here.
	 * @throws InterruptedException from use of java.lang.Thread.wait()
	**/
	protected void threadIterationWait() throws InterruptedException {
		if(workerPollResults.size() <= 1) {
			sleep(5 * 1000);
		} else {
			sleep(20 * 1000);
		}
	}

	/** Performs all necessary shutdown **/
	protected void shutdownInThread() {
		if(WorkerGUI.workerWindow!=null) {
			Logger.log(LogMessageCategory.INFO,"Closing workerWindow in HeartbeatDetectionThread.shutdownInThread");
			System.out.flush();
			WorkerGUI.close();
			Logger.log(LogMessageCategory.INFO,"Closed workerWindow in HeartbeatDetectionThread.shutdownInThread");
			System.out.flush();
		}
	}
}
