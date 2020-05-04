/**************************************************************************************************
 * @(#)RemoteEmissionsCalculatorTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.worker.gui.WorkerWindow;
import java.io.File;
import junit.framework.*;

/**
 * Test Case for the RemoteEmissionsCalculator class
 *
 * @author		Cimulus
 * @version		2003-11-30
**/
public class RemoteEmissionsCalculatorTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public RemoteEmissionsCalculatorTest(String name) {
		super(name);
	}

	/**
	 * Implements test case(s)
	 * @throws Exception If any error occurred during the test.
	**/
	public void testCalculator() throws Exception {
		WorkerConfiguration.theWorkerConfiguration.loadConfigurationData();
		try {
			WorkerWindow workerWindow = new WorkerWindow();

			File testJarFilePath = new File("testdata/Todo.jar");
			assertTrue("Test file isn't available.", testJarFilePath.isFile());

			DistributedWorkFileName workFileName = new DistributedWorkFileName();
			workFileName.mid = "111";
			workFileName.qid = "999";
			workFileName.state = DistributedWorkFileState.TODO;

			File todoFilePath = new File(
				WorkerConfiguration.theWorkerConfiguration.sharedDistributedFolderPath,
				workFileName.toString());
			workFileName.state = DistributedWorkFileState.DONE;
			File doneFilePath = new File(
				WorkerConfiguration.theWorkerConfiguration.sharedDistributedFolderPath,
				workFileName.toString());

			assertTrue("DONE file already exists", !doneFilePath.exists());

			FileUtilities.copyFile(testJarFilePath, todoFilePath, true);

			try {
				System.out.println("RemoteEmissionsCalculatorTest - Built fake TODO file."
					+ " Waiting for the DONE file: " + doneFilePath.getCanonicalPath());
				while (!doneFilePath.isFile()) {
					Thread.sleep(1000);
				}
				System.out.println("Found the DONE file");
				verifyDoneFile(doneFilePath);

				File outputDoneFile = new File("testdata/GeneratedDone.jar");
				FileUtilities.copyFile(doneFilePath, outputDoneFile, true);
			} finally {
				todoFilePath.delete();
				doneFilePath.delete();
			}

			workerWindow.windowClosing(null);
		} finally {
			WorkerConfiguration.theWorkerConfiguration.cleanupTemporaryDirectory();
		}
	}

	/**
	 * Verifies that the DONE file created by a worker has data and doesn't report errors.
	 * @param doneFile The path of the done file.
	 * @throws Exception If any errors occur during the checking.
	**/
	public void verifyDoneFile(File doneFile) throws Exception {
		File temporaryFolder = FileUtilities.createTemporaryFolder(null, "AnalyzeDone");
		try {
			JARUtilities.unJarFileToFolder(temporaryFolder, doneFile);

			File errorFile = new File(temporaryFolder,
					RemoteEmissionsCalculator.ERROR_FILE_NAME);
			if(errorFile.exists()) {
				assertTrue("The worker reported errors.", errorFile.length()<=0);
			}
			File outputFile = new File(temporaryFolder,
					RemoteEmissionsCalculator.OUTPUT_DATA_FILE_NAME);
			assertTrue("The output file doesn't exist.", outputFile.exists());
			File outputActivityFile = new File(temporaryFolder,
					RemoteEmissionsCalculator.OUTPUT_ACTIVITY_DATA_FILE_NAME);
			assertTrue("The output file doesn't exist.", outputActivityFile.exists());
			
		} finally {
			FileUtilities.deleteTemporaryFolder(temporaryFolder);
		}
	
	}
}
