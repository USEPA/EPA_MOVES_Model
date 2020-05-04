/**************************************************************************************************
 * @(#)WorkFileRetrieverThread.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.sql.*;
import java.util.LinkedList;
import java.util.Set;
import java.util.Iterator;

/**
 * Retrieves completed work files that have been processed by a remote worker system.
 *
 * @author		Cimulus
 * @version		2002-01-27
**/
public class WorkFileRetrieverThread extends MOVESThread {
	/**
	 * The temporary folder that files are internally built in before they are JAR'd
	 * in the distributed path.
	**/
	File temporaryFolderPath;

	/**
	 * Standard constructor
	**/
	public WorkFileRetrieverThread() {
	}
	
	/**
	 * Performs thread start up code
	 * @return Should the thread continue to run
	**/
	protected boolean startupInThread() {
		try {
			while (true) {
				// The Java File object doesn't support creating a temporary file 
				temporaryFolderPath = File.createTempFile("RetrieveProcessingTemp", "");
				temporaryFolderPath.delete();
				if(temporaryFolderPath.mkdir()) {
					temporaryFolderPath.deleteOnExit();
					break;
				}
			}
		} catch (IOException exception) {
//			Logger.log(LogMessageCategory.ERROR
//					, "IOException during createTempFile: " + exception);
			return false;
		}
		
		return true;
	}

	/**
	 * This performs one iteration of thread functionality. Upon return, the MOVESThread
	 * will automatically check for termination conditions.
	 * @return Should the thread continue to run
	**/
	protected boolean threadIterationGo() {
		return true;
	}

	/**
	 * Called directly after threadIterationGo. Performs a simple thread loop pause.
	 * @throws InterruptedException This is thrown if the thread is interruped during a sleep()
	**/
	protected void threadIterationWait() throws InterruptedException {
		sleep(5 * 1000);
	}

	/** Performs all necessary shutdown **/
	protected void shutdownInThread() {
	}
}
