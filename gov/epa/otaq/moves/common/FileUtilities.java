/**************************************************************************************************
 * @(#)FileUtilities.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.sql.*;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * This static class provides various general purpose file-related functionality.
 *
 * @author		Wesley Faler
 * @author		EPA (Mitch C.)
 * @version		2015-12-20
**/
public class FileUtilities {
	/**
	 * Some file operations must be retried to work around a Java runtime quirk where
	 * some stream objects' close() methods don't immediately release file system resources.
	 * This is the standard maximum retry count.
	**/
	static final int FILE_SYSTEM_STANDARD_RETRY_COUNT		= 10;
	/**
	 * Some file operations must be retried to work around a Java runtime quirk where
	 * some stream objects' close() methods don't immediately release file system resources.
	 * This is the standard sleep time in between retries.
	**/
	static final int FILE_SYSTEM_STANDARD_RETRY_SLEEP_MS	= 1000;

	/**
	 * Helper function to copy a file to destination directory, since Java.io.File doesn't
	 * provide a copy file function.
	 * @param sourceFilePath The file to copy.
	 * @param destinationFolder The destination folder to copy the file to.
	 * @return True on success.  Will fail if the file already exists in the destination folder.
	**/
	public static boolean copyFileToFolder(File sourceFilePath, File destinationFolder) {
		BufferedInputStream fileIn = null;
		BufferedOutputStream fileOut = null;

		try {
			int bufferSize = 256*1024;
			byte[] buffer = new byte[bufferSize];

			fileIn = new BufferedInputStream(new FileInputStream(sourceFilePath),bufferSize);
			File destinationFile = new File(destinationFolder, sourceFilePath.getName());
			if(destinationFile.exists()) {
				throw new IOException("File already exists in the destination folder.");
			}
			// destinationFile.createNewFile();
			fileOut =
					new BufferedOutputStream(new FileOutputStream(destinationFile),bufferSize);
			int length;
			while((length = fileIn.read(buffer)) >= 0) {
				if(length > 0) {
					fileOut.write(buffer,0,length);
				}
			}
		} catch(Exception e) {
			return false;
		} finally {
			if(fileIn != null) {
				try {
					fileIn.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
			if(fileOut != null) {
				try {
					fileOut.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}

		return true;
	}

	/**
	 * Helper function to copy a file to destination file.
	 * @param sourceFilePath The file to copy.
	 * @param destinationFilePath The destination File to copy the file to.
	 * @param overWrite Indicates if the destination should be overwritten.
	 * @return True if copied.
	**/
	public static boolean copyFile(File sourceFilePath, File destinationFilePath,
			boolean overWrite) {
		int bufferSize = 256*1024;
		byte[] buffer = null;
		boolean success = false;
		for(int i = 0; i < FILE_SYSTEM_STANDARD_RETRY_COUNT; i++) {
			if(i > 0) {
				try {
					Runtime.getRuntime().gc(); // Force garbage collection hoping to free memory and file handles
					Thread.sleep(FILE_SYSTEM_STANDARD_RETRY_SLEEP_MS);
				} catch(InterruptedException exception) {
					return false;
				}
			}
			BufferedInputStream fileIn = null;
			BufferedOutputStream fileOut = null;
			try {
				if(overWrite) {
					deleteFileWithRetry(destinationFilePath);
					try {
						destinationFilePath.delete();
					} catch(Exception e) {
						// Nothing to do here
					}
				}
				if(destinationFilePath.exists() && !overWrite) {
					return false;
				}
	
				if(buffer == null) {
					buffer = new byte[bufferSize];
				}
	
				fileIn = new BufferedInputStream(new FileInputStream(sourceFilePath),bufferSize);
				destinationFilePath.delete();
				// destinationFilePath.createNewFile();
				fileOut = new BufferedOutputStream(new FileOutputStream(destinationFilePath),bufferSize);
				int length;
				while((length = fileIn.read(buffer)) >= 0) {
					if(length > 0) {
						fileOut.write(buffer,0,length);
					}
				}
				success = true;
				break;
			} catch(Exception e) {
				String errorText = "Error copying file";
				try {
					errorText = "Error copying file "
							+ sourceFilePath.getCanonicalPath() + " to "
							+ destinationFilePath.getCanonicalPath();
				} catch(IOException e2) {
					errorText += "(exception while getting full path: " + e2.getMessage() + ")";
				}
				errorText += " Got error: " + e.getMessage();
				if(i < FILE_SYSTEM_STANDARD_RETRY_COUNT-1) {
					errorText += ". File copy will be tried again.";
				}
				Logger.log(LogMessageCategory.INFO,errorText);
				success = false;
			} finally {
				if(fileIn != null) {
					try {
						fileIn.close();
					} catch(Exception e) {
						// Nothing to do here
					}
				}
				if(fileOut != null) {
					try {
						fileOut.close();
					} catch(Exception e) {
						// Nothing to do here
					}
				}
			}
		}

		return success;
	}

	/**
	 * Compares two text files (eol differences don't matter), though case does matter.
	 * @param referenceFilePath String indicating the file, with path, to compare against.
	 * @param testFilePath String indicating the file, with path, to compare to.
	 * @return True if both files are the same.
	**/
	public static boolean compareTextFiles(String referenceFilePath, String testFilePath) {
		boolean isSame = false;
		try {
			LineNumberReader testFileReader = new LineNumberReader(
					new FileReader(testFilePath), 16384);
			LineNumberReader referenceFileReader = new LineNumberReader(
					new FileReader(referenceFilePath), 16384);
			// compare each line in both files, the loop terminates on the first difference
			isSame = true;
			while(true) {
				// these strings will be null when the EOF is reached
				String nextTestFilePathLine = testFileReader.readLine();
				String nextReferenceFileLine = referenceFileReader.readLine();
				// if both files have reached EOF, terminate the loop
				if((nextTestFilePathLine == null) && (nextReferenceFileLine == null)) {
					break;
				}
				// if one file reaches EOF, terminate the loop
				if((nextTestFilePathLine == null) || (nextReferenceFileLine == null)) {
					isSame = false;
					break;
				}
				// now it is safe to use String.equals(), both objects are non-NULL
				if(!nextTestFilePathLine.equals(nextReferenceFileLine)) {
					isSame = false;
					break;
				}
			}
			testFileReader.close();
			referenceFileReader.close();
		} catch(Exception e) {
			isSame = false;
//			Logger.logError(e,"File compare "+referenceFilePath+" to "+testFilePath+" failed.");
		}
		return isSame;
	}

	/**
	 * Utility function to compare two file paths, without regards to case,
	 * and without regards to slash style (i.e '/' == '\\').
	 * This is necessary since java.util.RegExp is not available to versions before 1.4.
	 * @param reference The filename to compare to.  This string can use single char
	 * wildcards ('?' char).
	 * @param testFilePath The filename to test.
	 * @return true if the two file paths are the same.
	**/
	public static boolean compareFilePath(String reference, String testFilePath) {
		// return if the two path strings are of different lengths
		if(reference.length() != testFilePath.length()) {
			return false;
		}
		// iterator through each char, run the comparisons, stops at the first real difference
		char refChar;
		char testChar;
		for(int i = 0; i < reference.length(); i++) {
			// check for wildcard
			refChar = Character.toLowerCase(reference.charAt(i));
			if(refChar == '?') {
				continue;
			}
			testChar = Character.toLowerCase(testFilePath.charAt(i));
			// case-insensitive comparison
			if(testChar != refChar) {
				// if different, check for slashes
				if((testChar == '\\' || testChar == '/')
						&& (refChar == '\\' || refChar == '/')) {
					// both chars are some type of slash, and thus "equal"
					continue;
				}
				// the chars are not both slashes, and are different so return fail
				return false;
			}
		}
		// passed all tests, so return success
		return true;
	}

	/**
	 * Verifies that a file specified by the abstract pathname exists.  This would also catch
	 * the case where the abstract pathname is for a directory, not a valid file.  This function
	 * basically wraps a call to File.isFile() and returns a simple boolean without throwing any
	 * exceptions.
	 * @param f The File representing the abstract pathname to check.
	 * @return Boolean true if the file exists and can be read.
	**/
	public static boolean fileExists(File f) {
		boolean result = false;
		try {
			result = f.isFile();
		} catch(SecurityException e) {
			// read access to the file was denied by a possible SecurityManager for this app
//			Logger.logError(e, "Test for file exists failed on "+f.getName());
			result = false;
		}
		return result;
	}

	/**
	 * Tries to open an InputStream for the specified file with retries if it fails, and waits
	 * in between failed attempts.  This will throw an exception if the final open attempt fails.
	 * @param file The File on which to create an InputStream.
	 * @throws FileNotFoundException An exception thrown from the FileInputStream constructor.
	 * Only rethrown once all retries have expired.
	 * @return An FileInputStream object. This won't be null.
	**/
	public static FileInputStream openFileInputStreamWithRetry(File file)
			throws FileNotFoundException {
		for(int i = 0; i < FILE_SYSTEM_STANDARD_RETRY_COUNT; i++) {
			try {
				FileInputStream result = new FileInputStream(file);

				if(i > 0) {
					Logger.log(LogMessageCategory.INFO,
						"openFileInputStreamWithRetry succeeded on retry " + i);
				}

				return result;
			} catch (FileNotFoundException notFoundException) {
				// If this is the last retry, rethrow the exception.
				if(i == FILE_SYSTEM_STANDARD_RETRY_COUNT - 1) {
					throw notFoundException;
				}

				try {
					Thread.sleep(FILE_SYSTEM_STANDARD_RETRY_SLEEP_MS);
				} catch(InterruptedException interruptedException) {
					throw notFoundException;
				}
			}
		}

		throw new UnsupportedOperationException("Code should never reach here");
	}

	/**
	 * Helper function to execute a SQL query and write the results into a Tab Separated Value
	 * file, including a header row with column names.  The heading row will be written even
	 * if there are no data rows.
	 * @param	db a Connection to the database to be queried
	 * @param	sql the statement to run.
	 * @param	outputFile the file to be generated.  Will be overwritten, not appended to,
	 * @throws	SQLException on error from any of the database operations.
	 * @throws	IOException on error from any of the file operations
	**/
	public static void exportSQLResultsToFile(Connection db,String sql,File outputFile)
			throws IOException, SQLException {
		BufferedWriter writer =
				new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFile)));

		PreparedStatement statement;
		ResultSet results;

		statement = db.prepareStatement(sql);
		results = SQLRunner.executeQuery(statement,sql);
		// Write the header row
		ResultSetMetaData meta = results.getMetaData();
		int numColumns = meta.getColumnCount();
		for(int i=1;i<=numColumns;i++) {
			if(i>1) {
				writer.write("\t");
			}
			writer.write(meta.getColumnName(i));
		}
		writer.newLine();
		// Write the data rows
		String nextValue = "";
		while(results.next()) {
			for(int i=1;i<=numColumns;i++) {
				if(i>1) {
					writer.write("\t");
				}
				nextValue = results.getString(i);
				writer.write(nextValue != null ? nextValue : "");
			}
			writer.newLine();
		}
		// Done
		results.close();
		statement.close();
		writer.close();
	}

	/**
	 * Searches for text in a file between the start and end tags.  If the start tag is found,
	 * then all text including the end tag is returned.  If the end tag is not found, then all
	 * all text after the start tag is returned.  The start tag is not included as part of the
	 * resulting string.
	 * @param inFile The File to search in.
	 * @param startTag A String indicating the text that would preceed the desired text "area".
	 * @param endTag A String that delineates the end of the text "area" to return.  This param
	 * can be null, if an end tag is not required.
	 * @return If the start tag is round, then the resulting text is returned as a String.
	**/
	public static String findTextInFile(File inFile, String startTag, String endTag) {
		String returnText = "";
		try {
			StringBuffer buffer = new StringBuffer(16384);
			BufferedReader br = new BufferedReader(new FileReader(inFile), 16384);
			int i;
			while((i = br.read()) != -1) {
				buffer = buffer.append((char)i);
			}
			String lowerString = buffer.toString().toLowerCase();
			String errorTagStart = startTag.toLowerCase();
			String errorTagEnd = endTag != null ? endTag.toLowerCase() : null;
			int errorIndexStart = lowerString.indexOf(errorTagStart);
			int errorIndexEnd = errorTagEnd != null ?
					lowerString.indexOf(errorTagEnd, errorIndexStart) : -1;
			if(errorIndexStart > -1) {
				if(errorIndexEnd > -1) {
					// The error text will include the end tag as part of the message, but
					// not the start tag
					if(errorIndexStart + errorTagStart.length() < (buffer.length() + 1)) {
						if(errorIndexEnd + errorTagEnd.length() < (buffer.length() + 1)) {
							errorIndexEnd += errorTagEnd.length();
						}
						returnText = buffer.toString().substring(
								errorIndexStart + errorTagStart.length(), errorIndexEnd);
					}
				} else {
					if(errorIndexStart + errorTagStart.length() < (buffer.length() + 1)) {
						// End tag not found so return everything after the start tag
						returnText = buffer.toString().substring(
								errorIndexStart + errorTagStart.length());
					}
				}
				if(returnText.length() == 0) {
					returnText = "error found, but no message was specified.";
				}
			}
			br.close();
		} catch(Exception e) {
//			Logger.logError(e, "Find text between '"+startTag+"' and '"+endTag+"' failed for file, "+
//					inFile.getName());
		}
		return returnText;
	}

	/**
	 * Attempts to delete a file. If the delete attempt fails, this waits and retries. This
	 * is frequently necessary since some stream objects' close() methods don't release
	 * underlying file system resources immediately as expected.
	 * @param fileToDelete The target file path to delete.
	 * @return Was the delete attempt successful.
	**/
	public static boolean deleteFileWithRetry(File fileToDelete) {
		for(int i = 0; i < FILE_SYSTEM_STANDARD_RETRY_COUNT; i++) {
			if(i > 0) {
				try {
					Thread.sleep(FILE_SYSTEM_STANDARD_RETRY_SLEEP_MS);
				} catch(InterruptedException exception) {
					return false;
				}
			}

			if(!fileToDelete.exists() || fileToDelete.delete()) {
				if(i > 0) {
					Logger.log(LogMessageCategory.INFO,
						"deleteFileWithRetry succeeded on retry " + i);
				}
				return true;
			}
		}

		return false;
	}

	/**
	 * Attempts to rename a file. If the rename attempt fails, this waits and retries. This
	 * is frequently necessary since some stream objects' close() methods don't release
	 * underlying file system resources immediately as expected.
	 * @param source The source file path.
	 * @param destination The destination file path.
	 * @return Was the rename attempt successful.
	**/
	public static boolean renameFileWithRetry(File source, File destination) {
		for(int i = 0; i < FILE_SYSTEM_STANDARD_RETRY_COUNT; i++) {
			if(i > 0) {
				try {
					Thread.sleep(FILE_SYSTEM_STANDARD_RETRY_SLEEP_MS);
				} catch(InterruptedException exception) {
					return false;
				}
			}

			if(!source.exists()) {
				return false;
			}
			if(source.renameTo(destination)) {
				if(i > 0) {
					Logger.log(LogMessageCategory.INFO,
						"renameFileWithRetry succeeded on retry " + i);
				}
				return true;
			}
		}

		return false;
	}

	/**
	 * Gets a file extension.
	 * @param f A File object.
	 * @param includeDot Indicates whether the dot should be part of the returned String.
	 * @return The file extension.
	**/
	public static String getFileExtension(File f, boolean includeDot) {
		return getFileExtension(f.getName(),includeDot);
	}

	/**
	 * Gets a file extension.
	 * @param fileName name of a file, including extension
	 * @param includeDot Indicates whether the dot should be part of the returned String.
	 * @return The file extension.
	**/
	public static String getFileExtension(String fileName, boolean includeDot) {
		int dotIndex = fileName.lastIndexOf(".");
		if(dotIndex > -1) {
			if(includeDot && (dotIndex == (fileName.length() - 1))) {
				return ".";
			}
			return fileName.substring(dotIndex + (includeDot ? 0 : 1));
		}
		return "";
	}

	/**
	 * Adds SQL statements to a linked list, one statement at a time.
	 * @param sqlScriptFilePath Indicates the path and name of the source SQL script file.
	 * @param toList A linked list with the SQL statements added.
	 * @return True on success.
	**/
	public static boolean appendSQLScriptToList(String sqlScriptFilePath,
			LinkedList<String> toList) {
		final String eol = System.getProperty("line.separator");
		String sql;
		BufferedReader br = null;
		try {
			br = new BufferedReader(new FileReader(sqlScriptFilePath), 16384);
			int state = 0;
			String statement = null;
			while(br.ready()) {
				String line = br.readLine();
				if(line == null) {
					break;
				}
				line = line.trim();
				if(line.length() <= 0) {
					continue;
				}
				switch(state) {
					case 0: // Waiting for SQL statement to start
						if(line.startsWith("--")) {
							toList.add(line);
							continue;
						} else if(line.endsWith(";")) {
							toList.add(line);
							continue;
						} else {
							statement = line;
							state = 1;
						}
						break;
					case 1: // Waiting for SQL statement to terminate
						statement += eol;
						statement += line;

						if(line.startsWith("--")) {
							statement += eol;
							continue;
						} else if(line.endsWith(";")) {
							toList.add(statement);
							statement = null;
							state = 0;
						}
						break;
				}
			}
			if(statement != null) {
				toList.add(statement);
			}
			return true;
		} catch(Exception e) {
			/**
			 * @issue Parse file, '[sqlScriptFilePath]', for SQL statements failed.
			 * @explain While reading a file of SQL statements, MOVES encountered an error.
			**/
			Logger.logError(e,"Parse file, '"+sqlScriptFilePath+"', for SQL statements failed.");
			return false;
		} finally {
			if(br != null) {
				try {
					br.close();
				} catch(Exception e) {
					// Nothing can be done here
				}
				br = null;
			}
		}
	}

	/**
	 * Creates a new temporary folder with the given name prefix.
	 * @param parentFolder The path to the parent folder to create the directory in. null is
	 * acceptable (will use system temporary directory)
	 * @param namePrefix The naming prefix of the temporary folder. This is required.
	 * @return The path to the new temporary folder. This will be null on error.
	**/
	public static File createTemporaryFolder(File parentFolder, String namePrefix) {
		return TemporaryFileManager.createTemporaryFolder(parentFolder,namePrefix);
	}

	/**
	 * Deletes the contents (non-recursive) of a temporary folder and then deletes the folder
	 * itself. This currently does NOT recursively delete sub-directories. Errors messages are
	 * logged but exceptions aren't thrown.
	 * @param targetFolder The folder to be deleted.
	 * @return Was the folder successfully immediately deleted.
	**/
	public static boolean deleteTemporaryFolder(File targetFolder) {
		return TemporaryFileManager.deleteTemporaryFolder(targetFolder);
	}

	/**
	 * Gets the canonical path name of a file. If that fails, return the name of the file
	 * without throwing an IOException.
	 * @param file The target file.
	 * @return The canonical path name or file path name
	**/
	public static String safeGetPath(File file) {
		try {
			return file.getCanonicalPath();
		} catch (IOException exception) {
			return file.getPath();
		}
	}

	/**
	 * Appends text data and an EOL sequence to the specified file.
	 * @param targetFile The file to append to.
	 * @param text The text to append. An EOL is added in addition to this text.
	 * @throws IOException If a file I/O error occurs.
	**/
	public static void appendLine(File targetFile, String text) throws IOException {
		final String eol = System.getProperty("line.separator");

		FileWriter fileWriter = new FileWriter(targetFile.getPath(), true);
		try {
			fileWriter.write(text + eol);
		} finally {
			fileWriter.close();
		}
	}

	/**
	 * Gets the name of a file with any extension stripped of.
	 * @param file The file to get the base name of.
	 * @return The base name of the file with any extension stripped.
	**/
	public static String getBaseFileName(File file) {
		String baseName = file.getName();
		int dotIndex = baseName.lastIndexOf(".");
		if(dotIndex > -1) {
			return baseName.substring(0, dotIndex);
		} else {
			return baseName;
		}
	}

	/**
	 * Reads an entire file to a String.
	 * @param file The file to read.
	 * @return The file contents as a String.
	 * @throws FileNotFoundException If the given file wasn't found.
	 * @throws IOException If there was an error reading the file.
	**/
	public static String readEntireFile(File file) throws FileNotFoundException, IOException {
		char[] characterBuffer = new char[(int) file.length()];
		FileReader fileReader = new FileReader(file);
		try {
			fileReader.read(characterBuffer);
		} finally {
			fileReader.close();
		}
		return new String(characterBuffer);
	}

	/**
	 * Reads an entire file into an ArrayList of String objects, one for each line.
	 * @param file The file to read.
	 * @return The file contents as an ArrayList of String objects, one for each line.
	 * @throws FileNotFoundException If the given file wasn't found.
	 * @throws IOException If there was an error reading the file.
	**/
	public static ArrayList<String> readLines(File file) throws FileNotFoundException, IOException {
		ArrayList<String> lines = new ArrayList<String>();
		BufferedReader reader = new BufferedReader(new FileReader(file), 65536);
		try {
			String line = null;
			while((line = reader.readLine()) != null) {
				lines.add(line);
			}
		} finally {
			reader.close();
		}
		return lines;
	}

	/**
	 * Reads an entire file to a StringBuffer.
	 * @param fileName The file to read.
	 * @return The file contents as a StringBuffer, or null if the file was not found or
	 * could not be read for any reason.
	**/
	public static StringBuffer readFileContents( String fileName ) {
		if ( fileName.length() == 0 ) {
			return null ;
		}

		String fileLine = "";
		String fileString = "";
		StringBuffer fileData = new StringBuffer() ;
		int fileLength ;

		try {
			// If this file exists, write it into the mobile input file

			File userFile = new File( fileName ) ;

			if ( userFile == null ) {
				return null ;
			}

			if ( userFile.exists() == false ) {
				return null ;
			}

			if ( userFile.length() == 0 ) {
				return null ;
			}

			fileLength = ( int ) userFile.length() ;
			fileData.ensureCapacity( fileLength + 10 ) ;

			BufferedReader fileCommandReader = new BufferedReader(
					new InputStreamReader(new FileInputStream(userFile)));

			// Read Lines of User Input file
			fileLine = fileCommandReader.readLine();
			while ( fileLine != null) {
				fileData.append( fileLine ) ;
				fileData.append( "\n" ) ;
				fileLine = fileCommandReader.readLine();
			}
			fileCommandReader.close();

			return fileData ;

		} catch( IOException ex ) {
		}

		return null ;
	}


	/**
	 * Writes an entire string to a file.
	 * @param fileName The filename (passed as a String) to write.
	 * @param data The data (passed as a String) to write to the file.
	 * @return Returns boolean 'true' if successful, 'false' if not.
	**/
	public static boolean writeFileContents( String fileName , String data ) {
		if ( fileName.length() == 0 || data.length() == 0 ) {
			return false ;
		}

		String fileLine = "";
		String newLine = System.getProperty( "line.separator" ) ;
		String fileString = "";
		int fileLength ;
		StringTokenizer stk = new StringTokenizer( data , "\r\n" ) ;
		FileWriter fw = null;

		try {
			fw = new FileWriter( fileName , false ) ;
			if ( fw == null ) {
				return false ;
			}
			while ( stk.hasMoreTokens() == true ) {
				fw.write( stk.nextToken() ) ;
				fw.write( newLine ) ;
			}
			return true ;
		} catch( IOException ex ) {
			// Nothing to do here
		} finally {
			if(fw != null) {
				try {
					fw.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				fw = null;
			}
		}

		return false;
	}


	/**
	 * Appends an entire string to a file. The method will create the file if it does not exist.
	 * @param fileName The filename (passed as a String) to write.
	 * @param data The data (passed as a String) to write to the file.
	 * @return Returns boolean 'true' if successful, 'false' if not.
	**/
	public static boolean appendFileContents( String fileName , String data ) {
		if ( fileName.length() == 0 || data.length() == 0 ) {
			return false ;
		}

		String fileLine = "";
		String newLine = System.getProperty( "line.separator" ) ;
		String fileString = "";
		StringBuffer fileData = new StringBuffer() ;
		int fileLength ;
		StringTokenizer stk = new StringTokenizer( data , "\r\n" ) ;

		try {
			FileWriter fw = new FileWriter( fileName , true ) ;
			if ( fw == null ) {
				return false ;
			}

			while ( stk.hasMoreTokens() == true ) {
				fw.write( stk.nextToken() ) ;
				fw.write( newLine ) ;
			}

			fw.close() ;

			return true ;

		} catch( IOException ex ) {
		}

		return false ;
	}
	/**
	 * Helper function to extract a block of leading comments from a MySQL script file.
	 * Considers this block of comments to extend from the beginning of the file
	 * up to the first non-comment, non-blank line.  It will be empty if first line
	 * is not blank and not a comment.
	 * comment lines begin with "#" or "-- ".
	 * "/*" comment syntax, which is imperfectly implemented by MySQL, is not supported.
	 *
	 * @param sourceFilePath the script file
	 * @return Object[] the block of leading comments
	**/
	public static Object[] extractInitialCommentBlock(File sourceFilePath) {
		ArrayList<String> header = new ArrayList<String>();
		FileReader fr = null;
		BufferedReader br = null;
		if(sourceFilePath.exists() && sourceFilePath.canRead()) {
			try {
				fr = new FileReader(sourceFilePath);
				br = new BufferedReader (fr);
				while (br.ready()) {
					String nextLine = br.readLine();
					if ((nextLine.startsWith("#")) || (nextLine.startsWith("-- "))
						|| (nextLine.trim().length() == 0)) {
							header.add(nextLine);
					} else {
						break;
					}
				}
			} catch(Exception e) {
				// nothing can be done here
			} finally {
				try {
					br.close();
					fr.close();
				} catch (Exception e) {
					// nothing can be done here
				}
			}
			return header.toArray();
		} else {
			return null;
		}
	}

	private static class FileWithDate {
		File f;
		long lastModified;
	}

	/**
	 * Sort an array of files by date/time of their creation, putting the earliest creation dates
	 * (ie farthest in the past) at the beginning of the list.
	 * @param files files to be sorted
	**/
	public static void sortByDate(File[] files) {
		if(files == null || files.length <= 1) {
			return;
		}
		try {
			FileWithDate[] details = new FileWithDate[files.length];
			for(int i=0;i<files.length;i++) {
				details[i] = new FileWithDate();
				details[i].f = files[i];
				details[i].lastModified = files[i].lastModified();
			}
			// Do a simple bubble sort now
			boolean done = false;
			while(!done) {
				done = true;
				for(int i=1;i<details.length;i++) {
					FileWithDate a = details[i-1];
					FileWithDate b = details[i];
					if(a.lastModified > b.lastModified) {
						details[i-1] = b;
						details[i] = a;
						done = false;
					}
				}
			}
			// Save the results
			for(int i=0;i<details.length;i++) {
				files[i] = details[i].f;
			}
		} catch(Exception e) {
			/** @explain While sorting a set of files by date/time, MOVES encountered an error. **/
			Logger.logError(e,"Unable to sort array of files");
		}
	}
}
