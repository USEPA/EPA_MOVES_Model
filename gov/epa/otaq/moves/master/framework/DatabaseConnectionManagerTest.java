/**************************************************************************************************
 * @(#)DatabaseConnectionManagerTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.sql.*;
import junit.framework.*;
import java.io.*;

/**
 * Test Case for the DatabaseConnectionManager class
 *
 * @author		Wesley Faler
 * @version		2012-11-07
**/
public class DatabaseConnectionManagerTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public DatabaseConnectionManagerTest(String name) {
		super(name);
	}

	/**
	 * Deletes databases such as MOVESExecution and MOVESWorker to test the ability to create
	 * these on the fly.
	 * @throws SQLException If a database error occurs.
	**/
	static public void deleteNonNecessaryDatabases() throws SQLException {
		Connection connection = DatabaseSelection.openKnownWorkingConnection();
		assertNotNull(connection);
		try {
			if(!DatabaseConnectionManager.isInitialized(MOVESDatabaseType.EXECUTION)) {
				SQLRunner.executeSQL(connection, "DROP DATABASE IF EXISTS MOVESExecution");
			}
			SQLRunner.executeSQL(connection, "DROP DATABASE IF EXISTS MOVESWorker");
		} finally {
			connection.close();
		}
	}

	/**
	 * Implements the test case(s).
	 * @throws ClassNotFoundException If a JDBC driver failed to load.
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException from any of the various db operations.
	**/
	public void testCase() throws ClassNotFoundException, InterruptedException, SQLException {
		createTestDatabases();
		DatabaseConnectionManager.setOutputDatabase(null,"JUnitTestOUTPUT");
		assertTrue(DatabaseConnectionManager.initializeAll());

		performTestOnTypes(MOVESDatabaseType.DEFAULT,
				MOVESDatabaseType.EXECUTION, MOVESDatabaseType.OUTPUT);
		performTestOnTypes(MOVESDatabaseType.EXECUTION,
				MOVESDatabaseType.DEFAULT, MOVESDatabaseType.OUTPUT);

		/*
		performTestOnTypes(MOVESDatabaseType.NRDEFAULT,
				MOVESDatabaseType.EXECUTION, MOVESDatabaseType.OUTPUT);
		performTestOnTypes(MOVESDatabaseType.EXECUTION,
				MOVESDatabaseType.NRDEFAULT, MOVESDatabaseType.OUTPUT);

		performTestOnTypes(MOVESDatabaseType.DEFAULT,
				MOVESDatabaseType.NRDEFAULT, MOVESDatabaseType.OUTPUT);
		performTestOnTypes(MOVESDatabaseType.NRDEFAULT,
				MOVESDatabaseType.DEFAULT, MOVESDatabaseType.EXECUTION);
		*/
	}

	/**
	 * Does the actual test with the specified database types.
	 * @param testType The database type to test.
	 * @param otherType1 A type not equivalent to testType.
	 * @param otherType2 Another type not equivalent to testType.
	 * @throws ClassNotFoundException If a JDBC driver failed to load.
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException If an SQL error occurs.
	**/
	void performTestOnTypes(MOVESDatabaseType testType,
			MOVESDatabaseType otherType1, MOVESDatabaseType otherType2)
			throws ClassNotFoundException, InterruptedException, SQLException {
		assertTrue(DatabaseConnectionManager.initialize(testType));

		Connection testConnection = DatabaseConnectionManager.getGUIConnection(testType);
		assertNotNull(testConnection);
		testConnection = DatabaseConnectionManager.openPrivateConnection(testType);
		assertNotNull(testConnection);
		testConnection.close();
		testConnection = DatabaseConnectionManager.checkOutConnection(testType);
		assertNotNull(testConnection);

		try {
			try {
				// Using the wrong type on checkin should cause an exception.
				DatabaseConnectionManager.checkInConnection(otherType1, testConnection);
				fail("Checking in connection with wrong type should have caused exception");
			} catch (IllegalArgumentException exception) {
				// Expected
			}

			try {
				DatabaseConnectionManager.checkInConnection(otherType2, testConnection);
				fail("Checking in connection with wrong type should have caused exception");
			} catch (IllegalArgumentException exception) {
				// Expected
			}
		} finally {
			DatabaseConnectionManager.checkInConnection(testType, testConnection);
		}
	}

	/**
	 * Safely ensures that some number of test databases exist, such that this test class can
	 * always be run.
	 * @throws SQLException If a database error occurred.
	**/
	public static void createTestDatabases() throws SQLException {
		final String[] testDatabaseNames = {
			"JUnitTestDEFAULT",
			"JUnitTestEXECUTION",
			"JUnitTestOUTPUT"
			//"JUnitTestNRDEFAULT"
		};

		Connection connection = DatabaseSelection.openKnownWorkingConnection();
		assertNotNull(connection);
		try {
			for(int i = 0; i < testDatabaseNames.length; i++) {
				String sql = "CREATE DATABASE IF NOT EXISTS " + testDatabaseNames[i];
				SQLRunner.executeSQL(connection, sql);
			}
		} finally {
			connection.close();
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.serverName = "localhost";
		dbSelection.databaseName = "JUnitTestOUTPUT";
		Connection db = null;
		try {
			db = dbSelection.openConnectionOrNull();
			assertNotNull(db);
            String defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
            TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
            replacements.put("##defaultdb##", defaultDatabaseName);
            DatabaseUtilities.executeScript(db,new File("database/CreateOutput.sql"), replacements);
		} catch(Exception e) {
			Logger.logError(e,"Creating sample output database failed.");
			DatabaseUtilities.closeConnection(db);
			db = null;
			fail("Got exception trying to create sample output database " + e.getMessage());
		} finally {
			if(db != null) {
				DatabaseUtilities.closeConnection(db);
			}
		}
	}

	/** Utility function for test cases to setup the output database **/
	public static void setupOutputDatabase() {
		try {
			createTestDatabases();
		} catch(Exception e) {
			fail("An exception occured while trying to create test databases: " + e.toString());
		}
		DatabaseConnectionManager.setOutputDatabase("localhost","JUnitTestOUTPUT");
	}
}
