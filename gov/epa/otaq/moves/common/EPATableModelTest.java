/**************************************************************************************************
 * @(#)EPATableModelTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * This class tests the EPATableModel class
 *
 * @author		Cimulus
 * @version		2004-04-25
**/
public class EPATableModelTest extends TestCase {
	DatabaseSelection dbSelection = new DatabaseSelection();
	Connection con = null ;

	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public EPATableModelTest(String name) {
		super(name);

		dbSelection.serverName = "localhost";
		dbSelection.databaseName = "MOVESDefault" ;
		dbSelection.userName = "";
		dbSelection.password = "";
		con = dbSelection.openConnectionOrNull();

		EPATableModel.dbConnection = con ;
	}

	/** Implements the test case(s). **/
	public void testCase() {
		int rc = 0 ;

		if ( rc == 0 ) {
			return ;
		}

		System.out.println( "EPATableModelTest" ) ;

		if ( con == null ) {
			System.out.println( "Connection is null" ) ;
		}

		EPATableModel epaTable = 
				new EPATableModel( "SourceTypeID | FuelTypeID | EngineTechID | Year | Value " );
		EPATableModel epaSourceUseType = new EPATableModel( 
				"SourceTypeID | HPMSVTypeID | sourcetypename | rollingterma | rotatingtermb | "
				+ "dragtermc | sourcemass " );

		System.out.println( "" ) ;
		System.out.println( epaTable.getDefinition() ) ;

		epaTable.setNumRowsActive( 4 ) ;
//		epaTable.numRowsActive = 204 ;
		epaTable.setValueAt( Integer.valueOf( 11 ) , 0 , 0 ) ;
		epaTable.setValueAt( Integer.valueOf( 12 ) , 0 , 1 ) ;
		epaTable.setValueAt( Integer.valueOf( 13 ) , 0 , 2 ) ;
		epaTable.setValueAt( "Mark" , 0 , 4 ) ;
		epaTable.setValueAt( Integer.valueOf( 44 ) , 1 , 0 ) ;
		epaTable.setValueAt( Integer.valueOf( 55 ) , 1 , 1 ) ;
		epaTable.setValueAt( Integer.valueOf( 66 ) , 1 , 2 ) ;
		epaTable.setValueAt( "Andrew" , 2 , 4 ) ;

		System.out.println( "\nData\n" ) ;
		System.out.println( epaTable.getPrintDataRows() ) ;

		epaTable.clearData() ;
		epaTable.setNumRowsActive( 2 ) ;
		epaTable.setValueAt( Integer.valueOf( 144 ) , 1 , 0 ) ;
		epaTable.setValueAt( Integer.valueOf( 155 ) , 1 , 1 ) ;
		epaTable.setValueAt( Integer.valueOf( 166 ) , 1 , 2 ) ;
		epaTable.setValueAt( "Rolfes" , 1 , 4 ) ;
		System.out.println( "\nData after Clear\n" ) ;
		System.out.println( epaTable.getPrintDataRows() ) ;
		
		rc = epaSourceUseType.sqlSelect( con , "select * from sourceusetype" ) ;
		System.out.println( "\nData from SQL  rc =" + rc + "\n" ) ;
		System.out.println( epaSourceUseType.getPrintDataRows() ) ;

		rc = epaSourceUseType.sqlSelect( "select sourcemass , dragtermc , rotatingtermb , " + 
				" rollingterma , SourceTypeID , HPMSVTypeID , sourcetypename from sourceusetype" );
		System.out.println( "\nData from SQL diff order  rc =" + rc + " " 
				+ epaSourceUseType.errorText + "\n" ) ;
		System.out.println( epaSourceUseType.getPrintDataRows() ) ;

		EPATableModel epaPollutant = new EPATableModel();
		rc = epaPollutant.sqlSelect( "select * from pollutant" ) ;
		System.out.println( "\nData from Pollutant SQL  " + rc + "\n" ) ;
		System.out.println( epaPollutant.getPrintDataRows() ) ;
	}

	/** Implements the test case(s). **/
	public void testCase2() {
		int rc = 0 ;

		if ( rc == 0 ) {
			return ;
		}

		System.out.println( "EPATableModelTest 2" ) ;

		EPATableModel epaPollutant = new EPATableModel();
		rc = epaPollutant.sqlSelect( "select * from pollutant" , true ) ;
		System.out.println( "\nData from Pollutant SQL  " + rc + "\n" ) ;
		System.out.println( epaPollutant.getPrintDataRows() ) ;

//		System.out.println( epaTable.getDefinition() ) ;
	}

	/** Implements the test case(s). **/
	public void testCase3() {
		int rc = 0 ;
/*
		System.out.println( "\n\nEPATableModelTest 3" ) ;

		EPATableModel epaSourceUseType = new EPATableModel( 
				"SourceTypeID | HPMSVTypeID | sourcetypename | rollingterma | rotatingtermb | dragtermc | sourcemass " );

		
		rc = epaSourceUseType.sqlSelect( "select sourcemass , dragtermc , rotatingtermb , " + 
				" rollingterma , SourceTypeID , HPMSVTypeID , sourcetypename from sourceusetype" );

		System.out.println( "\nData from SQL diff order  rc =" + rc + " " 
				+ epaSourceUseType.errorText + "\n" ) ;
		System.out.println( epaSourceUseType.getPrintDataRows() ) ;

		System.out.println( "\n********\n" ) ;

		rc = epaSourceUseType.sqlSelect( "select * from sourceusetype" );
		System.out.println( "\nData from SQL rc =" + rc + " " 
				+ epaSourceUseType.errorText + "\n" ) ;
		System.out.println( epaSourceUseType.getPrintDataRows() ) ;
*/
	}

	/** Implements the test case(s). **/
	public void testCase4() {
		int rc = 0 ;
		String sql ;

		EPATableModel tableSourceUseType = new EPATableModel() ;
		EPATableModel tableYear = new EPATableModel() ;
		EPATableModel tableFuelEngTechAssoc = new EPATableModel( 
				"SourceTypeId | FuelTypeId | EngTechId | Category | YearData " ) ;

		rc = tableSourceUseType.sqlSelect( "select * from sourceusetype" ) ;

		System.out.println( tableSourceUseType.getDefinition() ) ;
		System.out.println( tableSourceUseType.getPrintDataRows() ) ;

		rc = tableYear.sqlSelect( "select * from year where yearid >= 2000" ) ;

		sql= "SELECT " + 
				" (sourcetypeid*1000000)+(categoryDisplayOrder*10000)+(fueltypeid*100)+engtechid "
				+ " AS Sorter, sourceTypeID, categoryDisplayOrder, category, " 
				+ " fuelengtechassoc.fuelTypeID, fuelengtechassoc.engTechID " 
				+ " FROM fuelengtechassoc where sourcetypeid = 21 ORDER BY 1 " ;

		rc = tableFuelEngTechAssoc.sqlSelect( sql ) ;

		System.out.println( tableYear.getPrintDataRows() ) ;
		System.out.println( tableSourceUseType.getPrintDataRows() ) ;
		System.out.println( tableFuelEngTechAssoc.getPrintDataRows() ) ;
	}
}
