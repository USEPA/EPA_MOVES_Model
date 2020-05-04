/**************************************************************************************************
 * @(#)SystemConfigurationTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.io.*;
import junit.framework.*;

/**
 * Test Case for the SystemConfiguration class
 *
 * @author		Cimulus
 * @version		2006-07-24
**/
public class SystemConfigurationTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public SystemConfigurationTest(String name) {
		super(name);
	}

	/**
	 * Implements the test case(s).
	 * @throws IOException From any possible file operations.
	**/
	public void testCase() throws IOException {
		// Verify that the singleton exists and has created a master ID file in the shared
		// distributed work path
		assertNotNull(SystemConfiguration.getTheSystemConfiguration().distributedMasterID);
		assertTrue(DistributedIDBroker.getIDFilePath(
				DistributedIDBroker.PREFIX_MASTER_ID,
				SystemConfiguration.getTheSystemConfiguration().distributedMasterID,
				SystemConfiguration.getTheSystemConfiguration().computerID,
				SystemConfiguration.getTheSystemConfiguration().sharedDistributedFolderPath)
				.exists());
/*	Uncomment to see the temp master id file
		try {
			java.lang.Thread.sleep(5000);
		} catch(InterruptedException e) {

		}
*/
		// Verify that the settings are non-null, especially the DatabaseSelection object members
		assertNotNull("sharedDistributedFolderPath is null",
				SystemConfiguration.getTheSystemConfiguration().sharedDistributedFolderPath);
		assertNotNull("databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()] is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.DEFAULT.getIndex()]);
		assertNotNull
				("databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.DEFAULT.getIndex()].serverName);
		assertNotNull
				("databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.DEFAULT.getIndex()].databaseName);
		assertNotNull("databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].userName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.DEFAULT.getIndex()].userName);
		assertNotNull("databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].password is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.DEFAULT.getIndex()].password);
		assertNotNull("databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()] is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.EXECUTION.getIndex()]);
		assertNotNull
				("databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()].serverName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.EXECUTION.getIndex()].serverName);
		assertNotNull
				("databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()].databaseName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.EXECUTION.getIndex()].databaseName);
		assertNotNull
				("databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()].userName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.EXECUTION.getIndex()].userName);
		assertNotNull
				("databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()].password is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.EXECUTION.getIndex()].password);
		assertNotNull("databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()] is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.OUTPUT.getIndex()]);
		assertNotNull("databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].serverName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.OUTPUT.getIndex()].serverName);
		assertNotNull
				("databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].databaseName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.OUTPUT.getIndex()].databaseName);
		assertNotNull("databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].userName is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.OUTPUT.getIndex()].userName);
		assertNotNull("databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].password is null",
				SystemConfiguration.getTheSystemConfiguration().databaseSelections
				[MOVESDatabaseType.OUTPUT.getIndex()].password);
		assertNotNull("distributedMasterID is null",
				SystemConfiguration.getTheSystemConfiguration().distributedMasterID);

		// Verify that the config file exists
		File configFile = new File(SystemConfiguration.MOVES_CONFIGURATION_FILE_NAME);
		assertTrue("Config file doesn't exist.", configFile.exists());
	}
}
