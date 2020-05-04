/**************************************************************************************************
 * @(#)WorkerHeartbeatThread.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker.framework;

import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.MOVESThread;

import java.io.File;
import java.io.IOException;
import java.util.Date;

/**
 * This thread updates the time stamp of the worker ID file to let the master systems
 * know that this process is still alive.
 *
 * @author		wfaler
 * @version		2008-02-10
**/
public class WorkerHeartbeatThread extends MOVESThread {
	/** Standard constructor **/
	public WorkerHeartbeatThread() {
		super("WorkerHeartbeatThread");
	}

	/**
	 * Performs thread start up code
	 * @return Should the thread continue to run
	**/
	protected boolean startupInThread() {
//		Logger.log(LogMessageCategory.INFO, "WorkerHeartbeatThread.threadStartup() called");
		return true;
	}

	/**
	 * This performs one iteration of thread functionality. Upon return, the MOVESThread
	 * will automatically check for termination conditions.
	 * @return Should the thread continue to run
	**/
	protected boolean threadIterationGo() {
		try {
			String idFilePathText = WorkerConfiguration.theWorkerConfiguration
					.distributedWorkerIdFilePath.getCanonicalPath();
//			Logger.log(LogMessageCategory.DEBUG, "Worker Heartbeat: " + idFilePathText);
		} catch (IOException e) {
			Logger.logError(e,"Unable to get name for worker ID file");
		}
		boolean setModified = false;
		for(int i = 0; i < 3; i++) {
			try {
				setModified = WorkerConfiguration.theWorkerConfiguration.
						distributedWorkerIdFilePath.setLastModified((new Date()).getTime());
				if(setModified) {
					break;
				}
			} catch(Exception e) {
				Logger.logException(LogMessageCategory.WARNING, e);
				Logger.log(LogMessageCategory.WARNING,
						"distributedWorkerIdFilePath.setLastModified failed");
			}
			// setLastModified failed, so wait a "small" duration and retry
			try {
				WorkerConfiguration.theWorkerConfiguration.
						distributedWorkerIdFilePath.createNewFile();
				WorkerConfiguration.theWorkerConfiguration.
						distributedWorkerIdFilePath.deleteOnExit();
				Logger.log(LogMessageCategory.DEBUG, "Recreated Worker Heartbeat File: "
						+ WorkerConfiguration.theWorkerConfiguration.
						distributedWorkerIdFilePath.getCanonicalPath());
			} catch(Exception e) {
				// Do nothing here.  Failures will be caught naturally due to inability
				// to set the last modified date/time on the ID file.
			}
			try {
				Thread.sleep(2000);
			} catch(InterruptedException e) {
			}
		}
		if(!setModified) {
			Logger.log(LogMessageCategory.WARNING,
					"distributedWorkerIdFilePath.setLastModified failed after retries");
		}
		return true;
	}

	/**
	 * Called directly after threadIterationGo. Subclasses typically implement a simple wait
	 * operation here.
	 * @throws InterruptedException when interrupted after waiting for completion.
	**/
	protected void threadIterationWait() throws InterruptedException {
		for(int i=0;i<30 && !isSignaledToExit();i++) {
			sleep(2*1000);
		}
	}

	/** Performs all necessary shutdown **/
	protected void shutdownInThread() {
//		Logger.log(LogMessageCategory.INFO, "WorkerHeartbeatThread.threadShutdown() called");
		WorkerConfiguration.theWorkerConfiguration.distributedWorkerIdFilePath.delete();
	}
}
