/**************************************************************************************************
 * @(#)DatabaseUtilitiesTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the DatabaseUtilities class
 *
 * @author		Cimulus
 * @version		2003-05-27
**/
public class DatabaseUtilitiesTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public DatabaseUtilitiesTest(String name) {
		super(name);
	}

	/**
	 * Tests executeSQL()
	 * @throws Exception from executeSQL()
	**/
	public void testMerge() throws Exception {
		Connection db = DatabaseSelection.openKnownWorkingConnection();
		assertNotNull(db);
		String sql = "CREATE DATABASE IF NOT EXISTS JunitTest";
		SQLRunner.executeSQL(db, sql);
	}

	/**
	 * Tests copyResultSetToTable()
	 * @throws SQLException from copyResultSetToTable()
	**/
	public void testCopyResultSetToTable() throws SQLException {
		Connection db = DatabaseSelection.openKnownWorkingConnection();
		assertNotNull(db);
		String[] setupSQL = {
			"CREATE DATABASE IF NOT EXISTS JUnitTest",
			"USE JUnitTest",
			"CREATE TABLE IF NOT EXISTS TestA (ColumnA INT, ColumnB INT)",
			"CREATE TABLE IF NOT EXISTS TestB (ColumnA INT, ColumnB INT)",
			"DELETE FROM TestA",
			"DELETE FROM TestB",
			"INSERT INTO TestA VALUES (10, 10)",
			"INSERT INTO TestA VALUES (20, 20)",
			"INSERT INTO TestA VALUES (100, 100)",
			"INSERT INTO TestA VALUES (200, 200)"
		};
		for(int i = 0; i < setupSQL.length; i++) {
			SQLRunner.executeSQL(db, setupSQL[i]);
		}

		String sql = "SELECT * FROM TestA WHERE ColumnA >= 100";
		PreparedStatement statement = db.prepareStatement(sql);
		ResultSet rs = statement.executeQuery();
		DatabaseUtilities.copyResultSetToTable(rs, db, "TestB");
		rs.close();
		statement.close();
		sql = "SELECT COUNT(*) FROM TestB WHERE ColumnA >= 100";
		statement = db.prepareStatement(sql);
		rs = statement.executeQuery();
		int resultCount = 0;
		if(rs.next()) {
			resultCount = rs.getInt(1);
		}
		assertTrue(resultCount == 2);

		sql = "DROP TABLE IF EXISTS TestA";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS TestB";
		SQLRunner.executeSQL(db, sql);
		db.close();
	}
}
