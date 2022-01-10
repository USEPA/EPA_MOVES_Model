/**************************************************************************************************
 * @(#)TemporaryFileManager.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.util.*;

/**
 * Create, destroy, and otherwise manage temporary files and folders.  Use in place of
 * File.createTempFile().
 *
 * @author		Wesley Faler
 * @version		2009-09-06
**/
public class TemporaryFileManager {
	/** Mutex used to synchronize access to variables **/
	private static Object mutex = new Object();
	/** Folder to hold all temporary folders and files **/
	private static File workingFolder = null;
	/** Number of file name collisions, used to add uniqueness to names **/
	private static long collisionCounter = 0;

	/** A folder to be deleted in the future **/
	private static class FolderToDelete {
		public File folder;
		public int attemptNumber = 0;
		public long whenNextAttempt = 0;
		
		public FolderToDelete(File folderToUse) {
			folder = folderToUse;
			whenNextAttempt = 30*1000+System.currentTimeMillis();
		}
	}
	/** folders to be deleted in the future **/
	private static ArrayList<FolderToDelete> foldersToDelete = new ArrayList<FolderToDelete>();

	/**
	 * Creates a new temporary folder with the given name prefix.
	 * @param parentFolder The path to the parent folder to create the directory in. null is
	 * acceptable (will use system temporary directory)
	 * @param namePrefix The naming prefix of the temporary folder. This is required.
	 * @return The path to the new temporary folder. This will be null on error.
	**/
	public static File createTemporaryFolder(File parentFolder, String namePrefix) {
		synchronized(mutex) {
			setup();
			doDeletions(false);

			File folder = parentFolder == null? workingFolder : parentFolder;
			Exception lastError = null;
			boolean isFirst = true;
			for(long attempt=0;attempt<100;attempt++) {
				try {
					String name = isFirst? namePrefix : (namePrefix + collisionCounter);
					isFirst = false;
					File f = new File(folder,name);
					if(f.exists()) {
						collisionCounter++;
						attempt--; // this is not a failed attempt
						continue;
					}
					if(f.mkdirs()) {
						return f;
					}
					// This is a failed attempt since the folder did not exist but couldn't
					// be created.
				} catch(Exception e) {
					lastError = e;
				}
			}
			if(lastError != null) {
				Logger.logError(lastError,"Unable to create temporary folder");
			}
			return null;
		}
	}

	/**
	 * Deletes the contents (non-recursive) of a temporary folder and then deletes the folder
	 * itself. This currently does NOT recursively delete sub-directories. Errors messages are
	 * logged but exceptions aren't thrown.
	 * @param targetFolder The folder to be deleted.
	 * @return Was the folder successfully immediately deleted.
	**/
	public static boolean deleteTemporaryFolder(File targetFolder) {
		synchronized(mutex) {
			setup();
			doDeletions(false);

			if(!deleteTemporaryFolderCore(targetFolder,false)) {
				// Schedule the folder for deletion
				foldersToDelete.add(new FolderToDelete(targetFolder));
				return false;
			}
			return true;
		}
	}

	/**
	 * Delete any files and folders schedule for deletion.  Items that cannot be removed
	 * are scheduled for future deletion according to an escalating scale.  Some virus checkers
	 * may be examining MOVES files long after MOVES has finished with the files, thus
	 * preventing deletion.  Files and folders that take too long to be removed are set with
	 * Java's deleteOnExit() flag in a last hope of removing them.
	 * @param shouldForceDeletion true if anything not immediately deleted should be flagged
	 * for deletion upon exit of the JVM.  This is set to true during shutdown of GUIs or
	 * engines.
	**/
	public static void doDeletions(boolean shouldForceDeletion) {
		synchronized(mutex) {
			long now = System.currentTimeMillis();
			long next = 10*1000+now;
			// Call deleteTemporaryFolderCore for each scheduled folder that has reached its
			// scheduled time to be deleted.  If shouldForDeletion is true, match every folder
			// not just those whose time has arrived.
			for(int i=0;i<foldersToDelete.size();i++) {
				FolderToDelete f = foldersToDelete.get(i);
				if(shouldForceDeletion) {
					deleteTemporaryFolderCore(f.folder,true);
					f.folder = null;
				} else if(f.whenNextAttempt <= now) {
					if(deleteTemporaryFolderCore(f.folder,false)) {
						f.folder = null;
					} else {
						f.attemptNumber++;
						if(f.attemptNumber >= 10) {
							// If we've tried enough times, just mark the folder
							// and move on.
							deleteTemporaryFolderCore(f.folder,true);
							f.folder = null;
						} else {
							f.whenNextAttempt = next;
						}
					}
				}
				if(f.folder == null) { // If the folder is no longer being tracked...
					foldersToDelete.remove(i);
					i--;
				}
			}
		}
	}

	/**
	 * Establish the working folder to be used if not already created.
	**/
	private static void setup() {
		setup(null);
	}

	/**
	 * Establish the working folder to be used if not already created.
	 * @param baseFolder optional folder to hold the temporary folder, may be null
	**/
	public static void setup(File baseFolder) {
		synchronized(mutex) {
			if(workingFolder != null) {
				return;
			}
			String name = "MOVESTemporary";
			int attempt = 0;
			File w = null;
			for(;attempt<1000;attempt++) {
				try {
					if(attempt == 0) {
						if(baseFolder == null) {
							w = new File("MOVESTemporary");
						} else {
							w = new File(baseFolder, "MOVESTemporary");
						}
					} else {
						if(baseFolder == null) {
							w = new File("MOVESTemporary_" + attempt);
						} else {
							w = new File(baseFolder, "MOVESTemporary_" + attempt);
						}
					}
					if(w.exists() && !w.isDirectory()) {
						FileUtilities.deleteFileWithRetry(w);
						if(w.exists()) {
							if(attempt == 0) {
								/**
								 * @issue MOVESTemporary found as a file but should be a folder.
								 * @explain MOVES uses a folder named MOVESTemporary.  A file with that
								 * name was found and could not be removed.
								**/
								Logger.log(LogMessageCategory.ERROR,
										"MOVESTemporary found as a file but should be a folder.");
							}
							continue;
						}
					}
					if(!w.exists()) {
						w.mkdir();
						if(!w.exists()) {
							if(attempt == 0) {
								/**
								 * @issue Unable to create MOVESTemporary folder.
								 * @explain MOVES uses a folder named MOVESTemporary but was unable
								 * to create it.
								**/
								Logger.log(LogMessageCategory.ERROR,
										"Unable to create MOVESTemporary folder.");
							}
							continue;
						}
					}
					workingFolder = w;
					/**
					 * @issue Folder for temporary files: [filepath]
					 * @explain MOVES uses many short-lived temporary files and folders during
					 * processing.  This is their location.
					**/
					Logger.log(LogMessageCategory.INFO,"Folder for temporary files: "
							+ workingFolder.getCanonicalPath());
					break;
				} catch(Exception e) {
					Logger.logError(e,"Error setting up temporary files, trying again.");
				}
			}
		}
	}

	/**
	 * Deletes the contents (non-recursive) of a temporary folder and then deletes the folder
	 * itself. This currently does NOT recursively delete sub-directories. Errors messages are
	 * not logged and exceptions aren't thrown.
	 * @param targetFolder The folder to be deleted.
	 * @param shouldDeleteOnExit true if anything that can't be deleted should be flagged as
	 * deleteOnExit().
	 * @return Was the folder successfully immediately deleted.  false will returned even if the
	 * folder was marked for deleteOnExit().
	**/
	private static boolean deleteTemporaryFolderCore(File targetFolder,
			boolean shouldDeleteOnExit) {
		synchronized(mutex) {
			File[] folderContents = targetFolder.listFiles();
	
			for (int i = 0; i < folderContents.length; i++) {
				if(!folderContents[i].delete()) {
					if(shouldDeleteOnExit) {
						/**
						 * @issue Failed to delete file in temporary folder: [fileName]
						 * @explain MOVES is unable to remove the contents of a temporary folder.  Most likely
						 * this is due to a file within the folder still being in use.  Check that you have
						 * no MOVES files open.  It is also possible that some slower virus checkers may retain
						 * connections to files longer than MOVES expects.
						**/
						Logger.log(LogMessageCategory.WARNING,
								"Failed to delete file in temporary folder: "
								+ FileUtilities.safeGetPath(folderContents[i]));
						folderContents[i].deleteOnExit();
					}
				}
			}

			if(targetFolder.delete()) {
				return true;
			} else {
				if(shouldDeleteOnExit) {
					/**
					 * @issue Failed to delete temporary folder: [folder]
					 * @explain MOVES is unable to remove the contents of a temporary folder.  Most likely
					 * this is due to a file within the folder still being in use.  Check that you have
					 * no MOVES files open.  It is also possible that some slower virus checkers may retain
					 * connections to files longer than MOVES expects.
					**/
					Logger.log(LogMessageCategory.WARNING,
							"Failed to delete temporary folder: "
							+ FileUtilities.safeGetPath(targetFolder));
					targetFolder.deleteOnExit();
				}
				return false;
			}
		}
	}
}
