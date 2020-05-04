/**************************************************************************************************
 * @(#)MOVESAPITest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.systemtests.fast;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.worker.framework.WorkerConfiguration;
import gov.epa.otaq.moves.worker.gui.WorkerWindow;
import java.io.File;
import java.io.FileWriter;
import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the MOVESAPI class
 *
 * @author		wgfaler
 * @author		Tim Hull
 * @version		2009-09-02
**/
public class MOVESAPITest extends TestCase implements LogHandler {
	public static void main(String args[]) {
		MOVESAPITest a = new MOVESAPITest("MOVEAPITest");
		try {
			a.testBatchMode();
		} catch(Exception e) {
			System.out.println(e.toString());
		}
	}

	/** Count of errors logged while running test **/
	int errorCount;

	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public MOVESAPITest(String name) {
		super(name);
	}

	/**
	 * Tests MOVEAPI's batch mode with a single RunSpec.
	 * @throws Exception If an error occurred.
	**/
	public void testBatchMode() throws Exception {
		errorCount = 0;
		Logger.addLogHandler(this);

		HeartbeatDetectionThread.isOKToLaunchWorker = false;

		DatabaseConnectionManagerTest.setupOutputDatabase();
		DatabaseConnectionManager.initializeAll();

		int beginRowCount = getOutputRowCount();
		int beginActivityRowCount = getActivityOutputRowCount();
		
		File sampleRunSpecFile = new File("testdata/SampleRunSpec.xml");
		assertTrue(sampleRunSpecFile.isFile());
		File testLogFile = new File("testdata/TestLog.txt");
		if(testLogFile.isFile()) {
			assertTrue(testLogFile.delete());
		}
		File testListFile = new File("testdata/RunSpecList.txt");
		if(testListFile.isFile()) {
			assertTrue(testListFile.delete());
		}
		FileWriter testListWriter = new FileWriter(testListFile);
		try {
			final String eol = System.getProperty("line.separator");
			testListWriter.write(sampleRunSpecFile.getCanonicalPath() + eol);
		} finally {
			testListWriter.close();
		}
		assertTrue(testListFile.isFile());

		MOVESAPI api = MOVESAPI.getTheAPI();
		api.addToRunSpecFileList(testListFile);

		WorkerConfiguration.theWorkerConfiguration.loadConfigurationData();
		try {
			WorkerWindow workerWindow = new WorkerWindow();

			api.runBatch();

			int endRowCount = getOutputRowCount();
			int rowsCreated = endRowCount - beginRowCount;
			/*
			assertEquals("The number of output rows differed from expectations", 
					186, rowsCreated);
			*/
			assertTrue("There were no rows output but should have been",
					rowsCreated > 0);

			int endActivityRowCount = getActivityOutputRowCount();
			int rowsActivityCreated = endActivityRowCount - beginActivityRowCount;
			assertEquals(0, rowsActivityCreated);
		} finally {
			WorkerConfiguration.theWorkerConfiguration.cleanupTemporaryDirectory();
		}

		/* Removed as these no longer occur
		// The SourceBinDistribution generator writes 6 warnings during its execution
		// (about Copying data) and these are expected to occur.
		assertEquals(6, errorCount);
		*/
		assertEquals("There should be no errors", 0, errorCount);

		DatabaseConnectionManager.cleanupAll();
	}

	/**
	 * Handles the given log message of the specified category.
	 * @param category The category of the log message.
	 * @param message The String to get logged.
	**/
	public void handleLog(LogMessageCategory category, String message) {
		if(category.compareTo(LogMessageCategory.ERROR) >= 0) {
			errorCount++;

			System.out.println(message);
		}
	}

	/**
	 * Gets the number of rows existing in the output table in the output database.
	 * @return The number of rows in the output databse.
	 * @throws InterruptedException If the active thread was interrupted.
	 * @throws SQLException If a database error occurs.
	**/
	public int getOutputRowCount() throws InterruptedException, SQLException {
		Connection outputDatabase = DatabaseConnectionManager.checkOutConnection(
				MOVESDatabaseType.OUTPUT);
		try {
			String sql = "SELECT COUNT(*) FROM MOVESOutput";
			ResultSet rs = SQLRunner.executeQuery(outputDatabase, sql);
			rs.next();
			return rs.getInt(1);
		} catch(SQLException e) {
			if(e.getMessage().indexOf("doesn't exist") < 0) {
				throw e;
			} else {
				return 0;
			}
		} finally {
			DatabaseConnectionManager.checkInConnection(
					MOVESDatabaseType.OUTPUT, outputDatabase);
		}
	}

	/**
	 * Gets the number of rows existing in the activity output table in the output database.
	 * @return The number of rows in the activity output databse.
	 * @throws InterruptedException If the active thread was interrupted.
	 * @throws SQLException If a database error occurs.
	**/
	public int getActivityOutputRowCount() throws InterruptedException, SQLException {
		Connection outputDatabase = DatabaseConnectionManager.checkOutConnection(
				MOVESDatabaseType.OUTPUT);
		try {
			String sql = "SELECT COUNT(*) FROM MOVESActivityOutput";
			ResultSet rs = SQLRunner.executeQuery(outputDatabase, sql);
			rs.next();
			return rs.getInt(1);
		} catch(SQLException e) {
			if(e.getMessage().indexOf("doesn't exist") < 0) {
				throw e;
			} else {
				return 0;
			}
		} finally {
			DatabaseConnectionManager.checkInConnection(
					MOVESDatabaseType.OUTPUT, outputDatabase);
		}
	}
}
