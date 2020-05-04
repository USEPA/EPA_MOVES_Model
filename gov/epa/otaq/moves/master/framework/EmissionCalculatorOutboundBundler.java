/**************************************************************************************************
 * @(#)EmissionCalculatorOutboundBundler.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.io.File;
import java.util.LinkedList;

/**
 * Bundles a unit of work to be processed by a worker into a JAR file. A unit of work consists of
 * several files including exported data files, SQL scripts, and potentially Java class files. The
 * files are created by an EmissionCalculator and stored in a temporary folder. The
 * EmissionCalculatorOutboundBundler bundles these files into a single JAR file with a unique work
 * file name. This JAR file is stored in a shared location that the distributed worker systems are
 * watching.
 *
 * @author		Wesley Faler
 * @version		2010-10-17
**/
public class EmissionCalculatorOutboundBundler {
	/**
	 * The queue ID assigned to the generated distributed work items. This simply needs to
	 * uniquely identify a distributed work item. Note that in order for the distributed work file
	 * name to be unique, there can be only one EmissionCalculatorOutboundBundler for each Master.
	**/
	int queueID = 0;

	/** The current count of how many work bundle files this objects has created. **/
	int totalBundleCount = 0;

	/**
	 * Helper function to return the next queue ID.
	 * @return The next queue ID as String.
	**/
	public String getNextQueueID() {
		return new Integer(getNextQueueIDCore()).toString();
	}

	/**
	 * Helper function to return the next queue ID.
	 * @return The next queue ID as String.
	**/
	public synchronized int getNextQueueIDCore() {
		queueID++;
		return queueID;
	}

	/**
	 * Default constructor
	**/
	public EmissionCalculatorOutboundBundler() {
	}

	/**
	 * Takes the given list of files, bundles them into a JAR file and places the file in the
	 * shared path where the distributed worker systems will find it. This shared path is
	 * specified by SystemConfiguration.
	 * @param filePaths A list of File objects that specify which files need to be bundled.
	**/
	public void bundleData(LinkedList<File> filePaths) {
		bundleData(filePaths,DistributedWorkFilePurpose.CALCULATOR,null);
	}

	/**
	 * Takes the given list of files, bundles them into a JAR file and places the file in the
	 * shared path where the distributed worker systems will find it. This shared path is
	 * specified by SystemConfiguration.
	 * @param filePaths A list of File objects that specify which files need to be bundled.
	 * @param queueID bundle number, optional, may be null
	**/
	public void bundleData(LinkedList<File> filePaths, String queueID) {
		bundleData(filePaths,DistributedWorkFilePurpose.CALCULATOR,queueID);
	}

	/**
	 * Takes the given list of files, bundles them into a JAR file and places the file in the
	 * shared path where the distributed worker systems will find it. This shared path is
	 * specified by SystemConfiguration.
	 * @param filePaths A list of File objects that specify which files need to be bundled.
	 * @param purpose the payload type for the bundle
	**/
	public void bundleData(LinkedList<File> filePaths, DistributedWorkFilePurpose purpose) {
		bundleData(filePaths,purpose,null);
	}

	/**
	 * Takes the given list of files, bundles them into a JAR file and places the file in the
	 * shared path where the distributed worker systems will find it. This shared path is
	 * specified by SystemConfiguration.
	 * @param filePaths A list of File objects that specify which files need to be bundled.
	 * @param purpose the payload type for the bundle
	 * @param queueID bundle number, optional, may be null
	**/
	public void bundleData(LinkedList<File> filePaths, DistributedWorkFilePurpose purpose,
			String queueID) {
		if(purpose == null) {
			purpose = DistributedWorkFilePurpose.CALCULATOR;
		}
		try {
			if(queueID == null) {
				queueID = getNextQueueID();
			}

			// Get the destination JAR filename
			DistributedWorkFileName workFileName = new DistributedWorkFileName();
			workFileName.mid = SystemConfiguration.getTheSystemConfiguration().distributedMasterID;
			workFileName.qid = queueID;
			workFileName.purpose = purpose;
			workFileName.state = DistributedWorkFileState.TEMP;
			workFileName.wid = "";

			// Create the JAR in the shared work path
			File sharedWorkFilePath = new File(
					SystemConfiguration.getTheSystemConfiguration().sharedDistributedFolderPath,
					workFileName.toString());
			JARUtilities.jarFiles(sharedWorkFilePath, filePaths);
			if(SystemConfiguration.getTheSystemConfiguration().saveTODOPath != null) {
				if(!SystemConfiguration.getTheSystemConfiguration().saveTODOPath.exists()) {
					SystemConfiguration.getTheSystemConfiguration().saveTODOPath.mkdirs();
				}
				if(SystemConfiguration.getTheSystemConfiguration().saveTODOPath.exists()) {
					File saveTemp = new File(SystemConfiguration.getTheSystemConfiguration().saveTODOPath,sharedWorkFilePath.getName());
					FileUtilities.copyFile(sharedWorkFilePath,saveTemp,true);
					if(saveTemp.exists()) {
						File todoSave = DistributedWorkFileName.alterFilePathState(saveTemp,
								DistributedWorkFileState.TODO);
						if(saveTemp.renameTo(todoSave)) {
							Logger.log(LogMessageCategory.INFO,"Saved bundle file " + todoSave.getName());
						} else {
							/**
							 * @issue Failed to rename saved file [*]
							 * @explain An error was encountered while attempting to preserve a bundle file.
							**/
							Logger.log(LogMessageCategory.ERROR, "Failed to rename saved file " + saveTemp.getName());
						}
					} else {
						/**
						 * @issue Failed to save file [*]
						 * @explain An error was encountered while attempting to preserve a bundle file.
						**/
						Logger.log(LogMessageCategory.ERROR, "Failed to save file " + saveTemp.getName());
					}
				}
			}
			File todoWorkFilePath = DistributedWorkFileName.alterFilePathState(sharedWorkFilePath,
					DistributedWorkFileState.TODO);
			boolean didRename = false;
			for(int renameCount=0;renameCount<5;renameCount++) {
				FileUtilities.deleteFileWithRetry(todoWorkFilePath);
				if(FileUtilities.renameFileWithRetry(sharedWorkFilePath,todoWorkFilePath)) {
					didRename = true;
					break;
				} else {
					try {
						Thread.sleep(1000);
					} catch(InterruptedException e) {
						break;
					}
				}
			}
			if(didRename) {
				if(!MOVESEngine.theInstance.isTODOOnly) {
					//todoWorkFilePath.deleteOnExit();
				}
			} else {
				sharedWorkFilePath.deleteOnExit();
				/**
				 * @issue Failed to rename [*] to [*]
				 * @explain While creating a JAR file to be sent to the workers, an intermediate
				 * file could not be given its final name.  Ensure no programs are accessing the
				 * temporary files as they are being created.
				**/
				Logger.log(LogMessageCategory.ERROR
						, "Failed to rename " + sharedWorkFilePath.getCanonicalPath() +
						" to " + todoWorkFilePath.getCanonicalPath());
			}

			Logger.log(LogMessageCategory.INFO, "Created bundle " + queueID + " for workers to process");
			synchronized (this) {
				totalBundleCount++;
				MOVESEngine.theInstance.onGeneratedBundle();
			}
		} catch(Exception e) {
			/**
			 * @explain An error occurred while creating a JAR file to be sent to the workers.
			 * These are compressed files containing SQL, data, and control files.  Check for
			 * available drive space.
			**/
			Logger.logError(e,"Unable to JAR the worker input files.");
		}
	}
}
