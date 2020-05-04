/**************************************************************************************************
 * @(#)JARUtilitiesTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.util.*;
import junit.framework.*;

/**
 * Test Case for the JARUtilities class
 *
 * @author		Cimulus
 * @version		2003-09-11
**/
public class JARUtilitiesTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public JARUtilitiesTest(String name) {
		super(name);
	}

	/**
	 * Implements the test case(s).
	 * @throws IOException From any of the file operations.
	**/
	public void testCase() throws IOException {
// CIM: NOT DONE
/*		// ensure the test input files don't exist
		File testFile = null;
		for(int i = 0; i < 4; i++) {
			testFile = new File("./OutputDB.sql." + i);
			testFile.delete();
		}
		// create some test files
		LinkedList sourceFilePaths = new LinkedList();
		File destFile = null;
		for(int i = 0; i < 4; i++) {
			testFile = new File("./OutputDB.sql");
			if(!testFile.exists()) {
				FileUtilities.copyFile(new File("database/OutputDB.sql"), new File("."));
			}
			destFile = new File("./OutputDB.sql." + i);
			assertTrue("renameTo() returns false.", testFile.renameTo(destFile));
			sourceFilePaths.addLast(destFile);
		}
		// add to jar
		File jarFile = new File("./junittest.jar");
		JARUtilities.jarFiles(jarFile, sourceFilePaths);
		// verify that the jar file exists
		assertTrue("jarFile doesn't exist.", jarFile.exists());
		// delete the input files
		for(int i = 0; i < 4; i++) {
			testFile = new File("./OutputDB.sql." + i);
			testFile.delete();
		}
		// unjar the files, verify outputs, cleanup files
		File targetPath = new File("./testdata");
		JARUtilities.unJarFileToFolder(targetPath, jarFile);
		for(int i = 0; i < 4; i++) {
			testFile = new File("./testdata/OutputDB.sql." + i);
			assertTrue(testFile.getPath() + "  doesn't exist.", testFile.exists());
			testFile.delete();
		}
		jarFile.delete();
*/
	}
}
