/**************************************************************************************************
 * @(#)EmissionCalculatorOutboundBundlerTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.io.*;
import java.util.*;
import junit.framework.*;

/**
 * Test Case for the EmissionCalculatorOutboundBundler class
 *
 * @author		Wesley Faler
 * @version		2009-03-30
**/
public class EmissionCalculatorOutboundBundlerTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public EmissionCalculatorOutboundBundlerTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testCase() {
		// Get the current count of TODO files in the shared path
		FilenameFilter todoFilter = DistributedWorkFileName.buildFileNameFilter
				("*", "*", DistributedWorkFileState.TODO, "*");
		File[] fileArray = SystemConfiguration.getTheSystemConfiguration()
				.sharedDistributedFolderPath.listFiles(todoFilter);
		assertNotNull(fileArray);
		int originalCount = fileArray.length;
		LinkedList<File> testFiles = new LinkedList<File>();
		testFiles.add(new File("testdata/comparetextfiles1.txt"));
		testFiles.add(new File("testdata/comparetextfiles2.txt"));
		testFiles.add(new File("testdata/comparetextfiles3.txt"));
		testFiles.add(new File("testdata/comparetextfiles4.txt"));
		EmissionCalculatorOutboundBundler bundler = new EmissionCalculatorOutboundBundler();
		bundler.bundleData(testFiles);
		// Verify that a new TODO has been created in the shared path
		fileArray = SystemConfiguration.getTheSystemConfiguration()
				.sharedDistributedFolderPath.listFiles(todoFilter);
		assertTrue(fileArray.length == (originalCount + 1));
	}
}
