/**************************************************************************************************
 * @(#)MeteorologyGeneratorTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.general;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import java.sql.*;
import java.util.Calendar;
import junit.framework.*;

/**
 * Test Case for the MeteorologyGenerator class
 *
 * @author		Ed Glover
 * @version		2003-12-11
**/
public class MeteorologyGeneratorTest extends TestCase {
	
	/** Generator object to be tested **/
	MeteorologyGenerator mG;
	
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public MeteorologyGeneratorTest(String name) {
		super(name);
	}
	/**
	 * Instantiates new MeteorologyGenerator object in preparation of the test.
	**/
		public void setUp(){
 			mG = new MeteorologyGenerator();
 	}
	/**
	 * Implements test case by creating a complete duplicate of the ZoneMonthHour
	 * table.  The sum of the calculated heat index field is computed and 
	 * compared against a known value.
	 * @throws SQLException from some database operations.
	**/
	public void testdoHeatIndex() throws SQLException {
		try {
			Connection db = DatabaseSelection.openKnownWorkingConnection();
					
			SQLRunner.executeSQL(db, "USE MOVESExecution");
			SQLRunner.executeSQL(db, "Drop TABLE if EXISTS ZoneMonthHour CASCADE ");
			SQLRunner.executeSQL(db, 
				"CREATE TABLE IF NOT EXISTS ZoneMonthHour " + 
				"SELECT * from MOVESDEFAULT.ZoneMonthHour" );
	
			mG.doHeatIndex(db);
	
			// Verify output rows in Generator1
			String sql;
			double result = 0.0;
			PreparedStatement statement;
			ResultSet rs;
			
			sql = "SELECT SUM(heatIndex) as SUM FROM ZoneMonthHour ";
			statement = db.prepareStatement(sql);
			rs = statement.executeQuery();
			
			if(rs.next()) {
				result = rs.getDouble("SUM");
			}
			rs.close();
			System.out.println("heatIndex Sum = " + result);
			assertEquals(52240230.8551, result, 0.01);
	
		} catch (Exception e) {
			System.out.println("Big Problems with the DatabaseConnection ");
		} finally {
		}
	}
}
