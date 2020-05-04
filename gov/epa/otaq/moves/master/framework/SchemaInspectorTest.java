/**************************************************************************************************
 * @(#)SchemaInspectorTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.sql.*;
import junit.framework.*;
import java.text.*;
import java.lang.*;
import java.util.*;


/**
 * Test Case for the SchemaInspector class
 *
 * @author		Cimulus
 * @version		2004-03-17
**/
public class SchemaInspectorTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public SchemaInspectorTest(String name) {
		super(name);
	}

	/**
	 * Runs the schema inspection test on a known good County Database.
	**/
	public void testValid() {
		System.out.println( "Starting testValid - March, 2004" ) ;

		try {
			DatabaseSelection ds = new DatabaseSelection();
			DatabaseSelection ds2 = new DatabaseSelection();
			DatabaseSelection ds3 = new DatabaseSelection();
			Connection db , db2 , db3 ;
			DatabaseMetaData dmd ;
			ResultSet rs , rsCatalog , rsTable , tsColumn , rsTableType ;
			StringBuffer sb = new StringBuffer( 5000 ) ;
			StringTokenizer statement ;
			String lines [] ;
			String columns [] ;
			int line , numTokens , pos , x , iLocation ;
			String sLine , s , s1 , s2 , tableName , sColumns ;
			StringBuffer sb2 ;
			char c ;
			boolean b ;
			
			ds.serverName = "localhost";
			ds.databaseName = "MOVESDefault";
			ds.userName = "" ;
			ds.password = "" ;
			db = ds.openConnection();

			ds2.serverName = "localhost";
			ds2.databaseName = "CountyDB30";
			ds2.userName = "" ;
			ds2.password = "" ;
			db2 = ds2.openConnection();

			ds3.serverName = "localhost";
			ds3.databaseName = "MovesFirstCut";
			ds3.userName = "" ;
			ds3.password = "" ;
			db3 = ds3.openConnection();

			System.out.println( "Testing isMOVESSchemaPresent" ) ;

			System.out.println( "Database: " + ds.databaseName + ", Tables Required" ) ;
			b = SchemaInspector.isMOVESSchemaPresent( db , false , false ) ;
			System.out.println( "Results: " + b ) ;
			assertTrue(ds.databaseName + " should have all tables.", b );

			System.out.println( "Database: " + ds.databaseName + ", Tables Not Required" ) ;
			b = SchemaInspector.isMOVESSchemaPresent( db , false , true ) ;
			System.out.println( "Results: " + b ) ;
			assertTrue(ds.databaseName + " should have all tables.", b );

			System.out.println( "Database: " + ds2.databaseName + ", Tables Required" ) ;
			b = SchemaInspector.isMOVESSchemaPresent( db2 , false , false ) ;
			System.out.println( "Results: " + b ) ;
			assertTrue(ds2.databaseName + " should not have all tables.", !b );

			// Should fail on CountyDB30 because County table has different names
			System.out.println( "Database: " + ds2.databaseName + ", Tables Not Required" ) ;
			b = SchemaInspector.isMOVESSchemaPresent( db2 , false , true ) ;
			System.out.println( "Results: " + b ) ;
			assertTrue(ds2.databaseName + " should not have matching columns on County.", !b );
			
			System.out.println( "Database: " + ds3.databaseName + ", Tables Required" ) ;
			b = SchemaInspector.isMOVESSchemaPresent( db3 , false , false ) ;
			System.out.println( "Results: " + b ) ;

			System.out.println( "Database: " + ds3.databaseName + ", Tables Not Required" ) ;
			b = SchemaInspector.isMOVESSchemaPresent( db3 , false , true ) ;
			System.out.println( "Results: " + b ) ;
			assertTrue(ds3.databaseName + " All tables should have matching columns.", b );

			System.out.println( "End of Testing isMOVESSchemaPresent" ) ;
			
		} catch( Exception ex) {
			System.out.println( "testValid - Exception" ) ;
		}
		
		
/* CIM: NOT DONE
		// create a connection to the CountyDB server
		DatabaseSelection ds = new DatabaseSelection();
		ds.serverName = "localhost";
		ds.databaseName = "CountyDB";
		ds.userName = SystemConfiguration.getTheSystemConfiguration().SERVER_USER_NAME;
		ds.password = SystemConfiguration.getTheSystemConfiguration().SERVER_PASSWORD;
		Connection db = ds.openConnection();

		assertTrue("Unable to open a default connection to the County database.", (db != null));
		
		// run the verification test on the default database name, should be valid
		assertTrue("The default CountyDB is not valid.", 
				SchemaInspector.isMOVESSchemaPresent(db, false));
		try {
			db.close();
		} catch(SQLException e) {
			// must catch any potential SQLExceptions
		}
*/
	}

	/**
	 * Creates an output database which does not have the County Database schema
	 * and runs the schema inspection and verifies that it doesn't pass.  When
	 * finished, the output database is deleted.
	**/
	public void testInvalid() {
/* CIM: NOT DONE
		// try an invalid CountyDB database name, check that it doesn't pass verification
		DatabaseConnectionManager.getTheManager().setOutputServerName("localhost");
		Connection db = DatabaseConnectionManager.getTheManager().getConnection(
				DatabaseConnectionManager.OUTPUT_DATABASE);
		String testOutputDatabaseName = new String("JUnitSchemaInspectorTest");
		String sql = new String("");
		try {
			PreparedStatement statement = db.prepareStatement(sql);
			sql = "DROP DATABASE IF EXISTS " + testOutputDatabaseName;
			statement.execute(sql);
			statement.close();
			db.close();
		} catch(Exception e) {
			Logger.logError(e, "Schema Inspector unit test failed");
		}

		if(!DatabaseConnectionManager.getTheManager().createOutputDatabase
				(testOutputDatabaseName)) {
			fail("Could not create a test output database.");
		}
		db = DatabaseConnectionManager.getTheManager().getConnection(
				DatabaseConnectionManager.OUTPUT_DATABASE);
		assertTrue("Unable to open a connection to the test output database.", (db != null));
		// verify that an invalid CountyDB returns false for this function, also we 
		// are suppressing the SQLException logging that would otherwise get sent 
		// to System.out
		assertTrue("This connection should not be a valid CountyDB."
				, !SchemaInspector.isMOVESSchemaPresent(db, true));
		
		// cleanup
		sql = "DROP DATABASE IF EXISTS ?";
		try {
			PreparedStatement statement = db.prepareStatement(sql);
			sql = "DROP DATABASE IF EXISTS " + testOutputDatabaseName;
			statement.execute(sql);
			statement.close();
			db.close();
		} catch(Exception e) {
			Logger.logError(e, "Schema Inspector unit test failed");
		}
*/
	}
}
