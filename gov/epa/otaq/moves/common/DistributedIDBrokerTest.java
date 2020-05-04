/**************************************************************************************************
 * @(#)DistributedIDBrokerTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import junit.framework.*;

/**
 * Test Case for the DistributedIDBroker class
 *
 * @author		Cimulus
 * @version		2006-07-24
**/
public class DistributedIDBrokerTest extends TestCase {
	/** The number of test worker IDs to create. **/
	static int MAX_WORKER_IDS = 10;
	
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public DistributedIDBrokerTest(String name) {
		super(name);
	}

	/**
	 * Implements the test case(s).
	 * @throws IOException From any of the file operations.
	**/
	public void testCase() throws IOException {
		File testDir = new File("testdata");
		// Create some worker IDs
		String[] testID = new String[MAX_WORKER_IDS];

		File[] workerFileArray = testDir.listFiles(DistributedIDBroker.getWorkerIDFilenameFilter());
		// Count of worker files not owned by this test.
		int unownedWorkerFileCount = workerFileArray.length;

		for(int i = 0; i < MAX_WORKER_IDS; i++) {
			testID[i] = DistributedIDBroker.acquireID(DistributedIDBroker.PREFIX_WORKER_ID,
					"Computer",testDir);
			assertTrue("Invalid ID.", testID[i].length() > 0);
		}

		// verify some worker ID files were created, not master files
		File workerFile = null;
		String workerID = "";
		for(int i = 0; i < MAX_WORKER_IDS; i++) {
			workerFile = DistributedIDBroker.getIDFilePath(DistributedIDBroker.PREFIX_WORKER_ID,
					testID[i], "Computer", testDir);
			// ok to use File.exists() here
			assertTrue("A worker file doesn't exist.", workerFile.exists());
			// verify the worker ID
			workerID = DistributedIDBroker.getIDFromFile(workerFile);
			assertTrue("ID doesn't match.", testID[i].equals(workerID));
			workerFile = DistributedIDBroker.getIDFilePath(DistributedIDBroker.PREFIX_MASTER_ID,
					testID[i], "Computer", testDir);
			// ok to use File.exists() here
			assertTrue("A master file was created.", !workerFile.exists());
		}

		// test the worker file filter
		WildCardFileNameFilter workerFileFilter =
				(WildCardFileNameFilter)DistributedIDBroker.getWorkerIDFilenameFilter();
		assertTrue("Invalid filter.", workerFileFilter.nameFilter.length() > 0);
		workerFileArray = testDir.listFiles(workerFileFilter);
		assertEquals("Invalid number of worker files found."
				, MAX_WORKER_IDS + unownedWorkerFileCount, workerFileArray.length);
		// No need to cleanup the test files, they are all created as temporary files that get
		// deleted when the app exits
	}
}
