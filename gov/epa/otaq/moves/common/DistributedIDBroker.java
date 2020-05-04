/**************************************************************************************************
 * @(#)DistributedIDBroker.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

/**
 * Obtains unique IDs used to identify individual processes within a distributed system. The MOVES
 * system coordinates the activity of its many, distributed processes by creating and renaming 
 * temporary files in shared folders. The DistributedIDBroker uses the unique filename created by
 * the File.createTempFile() method to extract a unique ID for a process. The process uses this ID
 * in naming the files that it creates.
 *
 * @author		Cimulus
 * @version		2006-08-06
**/
public class DistributedIDBroker {
	/** Const value for a master id prefix. **/
	public static final String PREFIX_MASTER_ID	= "MSTR";
	/** Const value for a worker id prefix. **/
	public static final String PREFIX_WORKER_ID	= "WRKR";
	/** Const value for an id suffix. **/
	public static final String SUFFIX_ID		= ".ID";
	
	/**
	 * Acquires an ID that is unique across the distributed processing system. Uses the File 
	 * class's createTempFile() method to create a uniquely named file within a shared folder and
	 * returns the numeric portion of the created file's name as the process ID.
	 * @param idPrefix Specifies the type of ID desired. Either PREFIX_MASTER_ID or
	 * PREFIX_WORKER_ID
	 * @param computerID Identifier of the computer.  This is a permanent ID, whereas
	 * the id generated herein is a temporary ID valid only for the duration of the run.
	 * @param sharedDistributedFolderPath The shared folder that the distributed processing
	 * system works off of.
	 * @throws IOException from any File operations.
	 * @return The newly acquired ID.
	**/
	public static String acquireID(String idPrefix,
			String computerID,
			File sharedDistributedFolderPath) throws IOException {
		if(computerID == null || computerID.length() == 0) {
			computerID = "NoComputerID";
		}
		String suffix = "_" + computerID + SUFFIX_ID;
		File idTemporaryFile = File.createTempFile(
				idPrefix, suffix, sharedDistributedFolderPath);
		
		String temporaryName = idTemporaryFile.getName();
		idTemporaryFile.deleteOnExit();

		// Strip the prefix and suffix.
		return temporaryName.substring(idPrefix.length()
				, temporaryName.length() - suffix.length());
	}

	/**
	 * Gets the "ID" file for a worker or master. The ID file reserves an ID for a a process. The
	 * time stamp on the worker's ID file is also used as a heartbeat mechanism so that dead workers
	 * can be detected.
	 * @param idPrefix The type of ID. Either PREFIX_MASTER_ID or PREFIX_WORKER_ID.
	 * @param id The system ID.
	 * @param computerID Identifier of the computer.  This is a permanent ID, whereas
	 * the id parameter is a temporary ID valid only for the duration of the run.
	 * @param sharedDistributedFolderPath The path of the folder used to coordinate the distributed
	 * computing system.
	 * @return The path of the ID file.
	**/
	public static File getIDFilePath(String idPrefix, String id, String computerID, 
			File sharedDistributedFolderPath) {
		if(computerID == null || computerID.length() == 0) {
			computerID = "NoComputerID";
		}
		return new File(sharedDistributedFolderPath,
				idPrefix + id + "_" + computerID + SUFFIX_ID);
	}
	
	/**
	 * Gets a FilenameFilter that will identify all Worker ID files.
	 * @return The FilenameFilter instance.
	**/
	public static FilenameFilter getWorkerIDFilenameFilter() {
		return new WildCardFileNameFilter(PREFIX_WORKER_ID + "*" + SUFFIX_ID);
	}
	
	/**
	 * Gets the system ID from a file path. Simply strips the prefix and suffix from the name.
	 * @param srcFilePath The source file path.
	 * @return The resulting file path. This will be null if the file name is invalid.
	**/
	public static String getIDFromFile(File srcFilePath) {
		String fileName = srcFilePath.getName();
		
		String prefixString = PREFIX_WORKER_ID;
		int prefixIDIndex = fileName.indexOf(prefixString);
		if(prefixIDIndex < 0) {
			prefixString = PREFIX_MASTER_ID;
			prefixIDIndex = fileName.indexOf(prefixString);
		}
		int underscoreIndex = fileName.indexOf("_");
		int suffixIDIndex = fileName.indexOf(SUFFIX_ID);
		
		if((prefixIDIndex < 0) || (suffixIDIndex < 0)
				|| (underscoreIndex < 0) || (underscoreIndex > suffixIDIndex)) {
			return null;
		}
		
		return fileName.substring(prefixIDIndex + prefixString.length(), underscoreIndex);
	}
}
