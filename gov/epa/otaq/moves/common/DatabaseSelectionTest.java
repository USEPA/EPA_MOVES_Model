/**************************************************************************************************
 * @(#)DatabaseSelectionTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the DatabaseSelection class
 *
 * @author		Cimulus
 * @version		2003-02-06
**/
public class DatabaseSelectionTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public DatabaseSelectionTest(String name) {
		super(name);
	}

	/** Implements the test case(s). **/
	public void testCase() {
		// Verify that a default database connection can be opened, using hardcoded test values
		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.serverName = "localhost";
		dbSelection.databaseName = "";
		dbSelection.userName = "";
		dbSelection.password = "";
		Connection c = dbSelection.openConnectionOrNull();
		assertTrue("errorMessage = " + dbSelection.errorMessage, c != null);
		assertNotNull(c);
		// Close db resources
		try {
			c.close();
		} catch(SQLException e) {
			// 
		}
	}

	/** Tests openKnownWorkingConnection() **/
	public void testKnownWorkingConnection() {
		assertNotNull(DatabaseSelection.openKnownWorkingConnection());
	}
}
