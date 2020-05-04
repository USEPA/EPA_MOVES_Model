/**************************************************************************************************
 * @(#)FileUtilitiesTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import junit.framework.*;
import java.util.*;

/**
 * Test Case for the FileUtilitiesTest class
 *
 * @author		Cimulus
 * @version		2003-09-15
**/
public class FileUtilitiesTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public FileUtilitiesTest(String name) {
		super(name);
	}
	
	/**
	 * Tests the copyFile function without the overWrite parameter.
	 * @throws IOException from File.getCanonicalPath()
	**/
	public void testCopyFileNoOverwrite() throws IOException {
		// perform the copy
		File srcFile = new File("testdata", "test0.txt");
		File dstFile = new File(".", srcFile.getName());
		if(dstFile.exists()) {
			dstFile.delete();
		}
		assertTrue("copyFile() failed", FileUtilities.copyFileToFolder(srcFile, new File(".")));
		// verify file lengths
		assertEquals("File lengths differ.", srcFile.length(), dstFile.length());
		// verify destination file (text verification only)
		assertTrue("Failed text comparison.", FileUtilities.compareTextFiles(
				srcFile.getCanonicalPath(), dstFile.getCanonicalPath()));
	}
	
	/**
	 * Tests the copyFile function with the overWrite parameter.
	 * @throws IOException from File.getCanonicalPath()
	**/
	public void testCopyFileOptionalOverwrite() throws IOException {
		// perform the copy
		File srcFile = new File("testdata", "test0.txt");
		File dstFile = new File(".", srcFile.getName());
		assertTrue("copyFile() failed", FileUtilities.copyFile(srcFile, dstFile, true));
		// verify file lengths
		assertEquals("File lengths differ.", srcFile.length(), dstFile.length());
		// verify destination file (text verification only)
		assertTrue("Failed text comparison.", FileUtilities.compareTextFiles(
				srcFile.getCanonicalPath(), dstFile.getCanonicalPath()));
		// this call should fail since it does not allow overwriting
		assertTrue("copyFile() succeeded, but it should have failed",
				!FileUtilities.copyFile(srcFile, dstFile, false));
	}
	
	/** Tests the compareTextFiles function. **/
	public void testCompareTextFiles() {
		// check pass result
		assertTrue("Compare same files.", FileUtilities.compareTextFiles(
				"testdata/comparetextfiles1.txt", "testdata/comparetextfiles4.txt"));
		// check fail result
		assertTrue("Single char change in second file, should fail."
				, !FileUtilities.compareTextFiles(
				"testdata/comparetextfiles1.txt", "testdata/comparetextfiles2.txt"));
		// compare the same files, but have different EOLs
		assertTrue("Compare files with differing EOLs.", FileUtilities.compareTextFiles(
				"testdata/comparetextfiles1.txt", "testdata/comparetextfiles3.txt"));
	}

	/** Tests the compareFilePath function. **/
	public void testCompareFilePath() {
		String reference = "";
		String testFilePath = "";
		// exact same strings
		reference = "/test0/one.dat";
		testFilePath = "/test0/one.dat";
		assertTrue("Failed exact same strings."
				, FileUtilities.compareFilePath(reference, testFilePath));
		// same strings, with differing case
		testFilePath = "/tEst0/oNe.DAT";
		assertTrue("Failed same strings, with differing case."
				, FileUtilities.compareFilePath(reference, testFilePath));
		// same strings, with differing slashes
		testFilePath = "\\test0\\one.dat";
		assertTrue("Failed same strings, with differing slashes."
				, FileUtilities.compareFilePath(reference, testFilePath));
		// wildcard test
		reference = "\\test?\\one.???";
		testFilePath = "\\test1\\one.bin";
		assertTrue("Failed wildcard test."
				, FileUtilities.compareFilePath(reference, testFilePath));
		// different length
		reference = "/test2/two.dat";
		testFilePath = "/data/test2/two.dat";
		assertTrue("Failed different length."
				, !FileUtilities.compareFilePath(reference, testFilePath));
		// different strings
		reference = "/data/test2/two.dat";
		testFilePath = "/data/test3/two.dat";
		assertTrue("Failed different strings."
				, !FileUtilities.compareFilePath(reference, testFilePath));
	}
	
	/**
	 * Tests the fileExists function again a variety of inputs.
	**/
	public void testFileExists() {
		// test with valid dir, but invalid file
		File testFile = new File(".", "NOT_A_FILE.TEST");
		assertTrue("Invalid file test.", !FileUtilities.fileExists(testFile));
		// test with valid dir, and valid file
		testFile = new File(".", "MOVESConfiguration.txt");
		assertTrue("Valid file test.", FileUtilities.fileExists(testFile));
		// test with valid dir, and a dir specified for the "child" param
		testFile = new File(".", "gov");
		assertTrue("Dir test.", !FileUtilities.fileExists(testFile));
		// another dir test, when the "child" param is an empty string, then this abstract pathname
		// represents the dir specified by the "parent" param
		testFile = new File(".", "");
		assertTrue("Invalid file test.", !FileUtilities.fileExists(testFile));
	}

	/** Tests the openFileInputStreamWithRetry function. **/
	public void testOpenFileInputStreamWithRetry() {
		File testFile;
		try {
			// test with valid dir, but invalid file
			testFile = new File(".", "NOT_A_FILE.TEST");
			FileUtilities.openFileInputStreamWithRetry(testFile);
			fail("1-File does not exist, so execution should never reach here "
				+ "due to a thrown exception.");
		} catch(IOException e) {
			// This is the expected result, so don't log the exception
		}
		try {
			// test with valid dir, and valid file
			testFile = new File(".", "MOVESConfiguration.txt");
			assertTrue("Valid file test."
				, FileUtilities.openFileInputStreamWithRetry(testFile) != null);
		} catch(IOException e) {
			fail("Exception thrown when opening file input stream on valid file.");
		}
		try {
			// test with valid dir, and a dir specified for the "child" param
			testFile = new File(".", "gov");
			FileUtilities.openFileInputStreamWithRetry(testFile);
			fail("Attempting to open file stream on directory, so execution should"
					+ " never reach here due to a thrown exception.");
		} catch(IOException e) {
			// This is the expected result, so don't log the exception
		}
		try {
			// another dir test, when the "child" param is an empty string, then this abstract
			// pathname represents the dir specified by the "parent" param
			testFile = new File(".", "");
			FileUtilities.openFileInputStreamWithRetry(testFile);
			fail("Attempting to open file stream on directory, so execution should"
					+ " never reach here due to a thrown exception.");
		} catch(IOException e) {
			// This is the expected result, so don't log the exception
		}
	}

	/** Tests the exportSQLResultsToFile function. **/
	public void testExportSQLResultsToFile() {
		// FileUtilities.exportSQLResultsToFile() is already
		// implicitly tested by the DataAggregatorTest class
	}

	/** Tests the findTextInFile function. **/
	public void testFindTextInFile() {
		final String eol = System.getProperty("line.separator");

		File f = new File("testdata","findtextinfile.txt");
		String startTag;
		String endTag;
		String foundText;

		startTag = " **************************************************"
				+ "***********************************************/" + eol
				+ "package gov.epa.otaq.moves.common;" + eol
				+ eol
				+ "import java.io.*;" + eol
				+ "import junit.framework.*;" + eol
				+ eol
				+ "/**" + eol
				+ " * Test Case for the FileUtilitiesTest class" + eol
				+ " *" + eol
				+ " * @author\t\t";
		endTag = "Cimulus";
		foundText = FileUtilities.findTextInFile(f,startTag,endTag);
		assertTrue("Failed to get the author.", foundText.compareTo("") == 0);

		startTag = "\tpublic void testGetFileExtension() {";
		endTag = "}";
		foundText = FileUtilities.findTextInFile(f,startTag,endTag);
		assertTrue("Failed to get the body of the testGetFileExtension() function.",
				foundText.compareTo(
				eol
				+ "\t\tFile f;" + eol
				+ "\t\tString ext;" + eol
				+ eol
				+ "\t\tf = new File(\"database\", \"CountyDB.sql\");" + eol
				+ eol
				+ "\t\text = FileUtilities.getFileExtension(f,true);" + eol
				+ "\t\tSystem.out.println(\"Extension with dot = \" + ext);" + eol
				+ "\t\tassertTrue(\"Failed to get the extension including the dot.\","
				+ " ext.compareTo(\".sql\") == 0);" + eol
				+ eol
				+ "\t\text = FileUtilities.getFileExtension(f,false);" + eol
				+ "\t\tSystem.out.println(\"Extension without dot = \" + ext);" + eol
				+ "\t\tassertTrue(\"Failed to get the extension without the dot.\","
				+ " ext.compareTo(\"sql\") == 0);" + eol
				+ eol
				+ "\t\tf = new File(\".\", \"abc.def.GHI\");" + eol
				+ eol
				+ "\t\text = FileUtilities.getFileExtension(f,true);" + eol
				+ "\t\tSystem.out.println(\"Extension with dot = \" + ext);" + eol
				+ "\t\tassertTrue(\"Failed to get the extension including the dot.\","
				+ " ext.compareToIgnoreCase(\".ghi\") == 0);" + eol
				+ eol
				+ "\t\text = FileUtilities.getFileExtension(f,false);" + eol
				+ "\t\tSystem.out.println(\"Extension without dot = \" + ext);" + eol
				+ "\t\tassertTrue(\"Failed to get the extension without the dot.\","
				+ " ext.compareTo(\"GHI\") == 0);" + eol
				+ "\t}") == 0);
		}

	/** Tests the deleteFileWithRetry function. **/
	public void testDeleteFileWithRetry() {
		File f;

		try {
			f = new File("testdata","testDeleteFileWithWait.txt");
			f.createNewFile();
			assertTrue("Failed to delete the file \"" + f.getCanonicalPath()
					+ "\", which was just created.", FileUtilities.deleteFileWithRetry(f));
		} catch(Exception e) {
//			Nothing to do here
		}

		try {
			f = new File("testdata","testDeleteFileWithWait2.txt");
			if(!f.exists()) {
				assertTrue("Succeeded in deleting the file \"" + f.getCanonicalPath()
						+ "\", which doesn't exist.", !FileUtilities.deleteFileWithRetry(f));
			}
		} catch(Exception e) {
//			Nothing to do here
		}
	}

	/** Tests the getFileExtension function. **/
	public void testGetFileExtension() {
		File f;
		String ext;

		f = new File("somedir", "test0.sql");

		ext = FileUtilities.getFileExtension(f,true);
		assertTrue("Failed to get the extension including the dot.", ext.compareTo(".sql") == 0);

		ext = FileUtilities.getFileExtension(f,false);
		assertTrue("Failed to get the extension without the dot.", ext.compareTo("sql") == 0);

		f = new File(".", "abc.def.GHI");

		ext = FileUtilities.getFileExtension(f,true);
		assertTrue("Failed to get the extension including the dot."
			, ext.compareToIgnoreCase(".ghi") == 0);

		ext = FileUtilities.getFileExtension(f,false);
		assertTrue("Failed to get the extension without the dot.", ext.compareTo("GHI") == 0);
	}

	/** Tests the appendSQLScriptToList function **/
	public void testAppendSQLScriptToList() {
		LinkedList<String> sqlLines = new LinkedList<String>();
		boolean result = 
				FileUtilities.appendSQLScriptToList("testdata/appendSQLScriptToList.sql",
				sqlLines);
		assertTrue("Failed to read Test.sql",result);

		int lineNumber = 1;
		for(Iterator i=sqlLines.iterator();i.hasNext();) {
			String sql = (String)i.next();
			System.out.println(lineNumber + ": " + sql);
			lineNumber++;
		}

		assertTrue("Invalid number of parsed lines.",sqlLines.size() == 4);
	}
}
