/**************************************************************************************************
 * @(#)InputDataManagerTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.sql.*;
import junit.framework.*;
import java.util.Vector;

/**
 * Test Case for the InputDataManager class
 *
 * @author		Cimulus
 * @version		2004-09-30
**/
public class InputDataManagerTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public InputDataManagerTest(String name) {
		super(name);
	}

	/**
	 * Prepares a sample ExecutionRunSpec for use in the various tests.
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException If a database error occurs.
	**/
	protected void setUp() throws InterruptedException, SQLException {
		// Setup an execution RunSpec with enough sample data to return non-empty sql
		// strings below
		ExecutionRunSpecTest.setupExecutionRunSpec();
	}

	/** Test the various buildWhereClause functions. **/
	public void testWhereClauses() {
		// Verify each build function returns a non-empty string
		Vector sqls = null;

		sqls = InputDataManager.buildSQLWhereClauseForYears("year");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForMonths("month");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForCounties("COUNTY");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForPollutants("POLLUTANT.PollutantID");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForProcesses("Process.ProcessID");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForPollutantProcessIDs("PolProcessID");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForDays("day");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForHours("hour");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForRoadTypes("RoadTypeID");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForFuelTypes("FuelTypeID");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);

		sqls = InputDataManager.buildSQLWhereClauseForSourceUseTypes("SourceTypeID");
		assertNotNull(sqls);
		assertTrue(sqls.size() > 0);
	}

	/**
	 * Tests merge()
	 * @throws Exception from merge(), in case of error.
	**/
	public void testMerge() throws Exception {
		InputDataManager.merge();

		Connection executionConnection = DatabaseConnectionManager.checkOutConnection(
				MOVESDatabaseType.EXECUTION);
		try {
			Statement statement = executionConnection.createStatement();
			try {
				assertRecordsInTable(statement, "DayOfAnyWeek", 7);
			} finally {
				statement.close();
			}
		} finally {
			DatabaseConnectionManager.checkInConnection(
					MOVESDatabaseType.EXECUTION, executionConnection);
		}
	}

	/**
	 * Checks that a table contains the expected number of rows.
	 * @param statement A JDBC statement object connected to the target database.
	 * @param tableName The name of the table to test for.
	 * @param expectedRowCount The number of rows expected in the destination database. This is
	 * currently ignored since this will depend on the sample data in the source database.
	 * @throws SQLException If a SQL error occurs.
	**/
	void assertRecordsInTable(Statement statement, String tableName, int expectedRowCount)
			throws SQLException {
		String sql = "SELECT COUNT(*) FROM " + tableName;
		ResultSet rs = statement.executeQuery(sql);
		try {
			assertTrue("SELECT COUNT(*) returned no answer.", rs.next());
			int numRows = rs.getInt(1);
			assertTrue(tableName + " has no rows.", numRows > 0);
		} finally {
			rs.close();
		}
	}
}
