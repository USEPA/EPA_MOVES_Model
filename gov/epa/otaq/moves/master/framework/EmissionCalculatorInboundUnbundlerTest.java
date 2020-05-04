/**************************************************************************************************
 * @(#)EmissionCalculatorInboundUnbundlerTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.io.File;
import junit.framework.*;

/**
 * Test Case for the EmissionCalculatorInboundUnbundler class
 *
 * @author		Cimulus
 * @version		2003-09-24
**/
public class EmissionCalculatorInboundUnbundlerTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public EmissionCalculatorInboundUnbundlerTest(String name) {
		super(name);
	}

	/**
	 * Implements test case(s)
	 * @throws Exception If any error occurred during the test.
	**/
	public void testUnbundler() throws Exception {
		ExecutionRunSpecTest.setupExecutionRunSpec();

		File testJarFilePath = new File("testdata/Done.jar");
		assertTrue("Test file isn't available.", testJarFilePath.isFile());

		DistributedWorkFileName workFileName = new DistributedWorkFileName();
		workFileName.mid = SystemConfiguration.getTheSystemConfiguration().distributedMasterID;
		workFileName.qid = "999";
		workFileName.state = DistributedWorkFileState.DONE;

		File doneFilePath = new File(
			SystemConfiguration.getTheSystemConfiguration().sharedDistributedFolderPath,
			workFileName.toString());
		assertTrue("DONE file already exists", !doneFilePath.exists());

		FileUtilities.copyFile(testJarFilePath, doneFilePath, true);
		try {
			MOVESEngine.theInstance = new MOVESEngine();
			MOVESEngine.theInstance.bundler = new EmissionCalculatorOutboundBundler();
			MOVESEngine.theInstance.bundler.totalBundleCount = 1;
			EmissionCalculatorInboundUnbundler unbundler = new EmissionCalculatorInboundUnbundler();
			MOVESEngine.theInstance.unbundler = unbundler;

			unbundler.threadIterationGo();

			assertTrue("Done file wasn't consumed", !doneFilePath.isFile());
		} finally {
			doneFilePath.delete();
		}
	}
}
