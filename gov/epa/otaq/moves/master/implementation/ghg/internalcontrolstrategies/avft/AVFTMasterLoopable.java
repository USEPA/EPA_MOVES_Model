/**************************************************************************************************
 * @(#)AVFTMasterLoopable.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.avft;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.sql.*;
import java.util.*;

/**
 * Implements a Master Loopable Object for AVFT (Alternative Vehicle Fuels & Technologies)
 *
 * @author		Wesley Faler
 * @author 		Tim Hull
 * @version		2014-02-18
**/
public class AVFTMasterLoopable implements MasterLoopable {
	/** True when debugging data should be output **/
	static boolean isDebugging = false;

	/** A reference to the AVFTControlStrategy that is being used for this execution **/
	AVFTControlStrategy avftStrategy ;
	/** A table model to store data from the tableSourceTypeModelYear table **/
	EPATableModel tableSourceTypeModelYear = new EPATableModel() ;
	/** A table model to store starting data from the FuelEngFraction table **/
	EPATableModel tableFraction = new EPATableModel( "SourceTypeModelYearId | FuelTypeId |"
			+ " EngTechId | FuelEngFraction " );
	/** The Start Year of this execution **/
	int startYear ;
	/** The End Year of this execution **/
	int endYear ;
	/** The columnIndex of the ModelYearId column within tableSourceTypeModelYear **/
	int indexYear ;
	/** The columnIndex of the SourceTypeId column within tableSourceTypeModelYear **/
	int indexSource ;
	/** The Connection object for the MOVES Execution Database for the execution **/
	Connection conExecution = null ;
	/**
	 * Cache of row index into tableFraction.  Data is Integer, keyed by String
	 * of "sourceTypeModelYearId | fuelTypeId | engTechId"
	**/
	TreeMap<String,Integer> tableFractionIndex = null;

	/**
	 * Constructor
	**/
	public AVFTMasterLoopable( AVFTControlStrategy strategy ) {
		avftStrategy = strategy ;
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		boolean b ;

		// Don't subscribe to anything since this just needs to be done one time before any
		// process is run.

		try {
			Logger.log(LogMessageCategory.INFO,"Calculate AVFT Control Strategy" ) ;
			TreeSet<Integer> changedSourceTypeModelYears = new TreeSet<Integer>();
			b = doDatabaseUpdatesUsingAVFTTable(changedSourceTypeModelYears); // fills sampleVehiclePopulation
			ExecutionRunSpec.theExecutionRunSpec.runUpdateScript(conExecution);
			if ( b == true ) {
				Logger.log(LogMessageCategory.INFO, "AVFT Calculations Complete" ) ;
				doDiagnosticChecks();
			} else {
				Logger.log(LogMessageCategory.WARNING, "Warning: AVFT Calculations Failed");
			}
		} catch( Exception ex ) {
			// Nothing to do here
		} finally {
			//tableFraction.printDataTable( "fraction2.del" ) ;
			doCleanup() ;
		}
	}

	/**
	 * Called during each relevant iteration of the MasterLoop.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void executeLoop(MasterLoopContext context) throws InterruptedException {
		// Nothing to do here
	}

	/**
	 * Cleans up all the data created within the executeLoop() method.  Typically only
	 * Generator-derived classes will have any code within this method.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void cleanDataLoop(MasterLoopContext context) throws InterruptedException {
		doCleanup() ;
	}

	/**
	 * Cleans up the run of this Execution
	 * This method sets the class member variables: startYear and endYear back to 0
	 * and initializes the tableFraction table memory.
	 * The method also checks in the Connection
	**/
	void doCleanup() {
		startYear = endYear = 0 ;

		tableFraction.clearData() ;

		if ( conExecution != null ) {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,conExecution);
			conExecution = null ;
		}
	}

	/**
	 * Setup the run of this Execution
	 * This method sets the class member variables: startYear and endYear and then
	 * initializes the tableFraction table memory and the tableRegFraction table.
	 * @return true if the setup completes successfully, false if it does not.
	**/
	boolean doSetup() {
		int rowSUT ;
		EPATableModel data ;
		String sql ;
		int rc ;
		EPATableModel tableSourceUseType = avftStrategy.tableSourceUseType ;
		Connection defaultDb
				= DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);

		try {
			tableFraction.clearData() ;
			startYear = endYear = 0 ;

			// determine the startYear and endYear
			for ( rowSUT = 0 ; rowSUT < tableSourceUseType.getNumRows() ; rowSUT++ ) {
				data = ( EPATableModel ) tableSourceUseType.getObject(
						rowSUT , "TableFractionData" ) ;
				if ( data == null ) {
					continue ;
				}

				if ( data.getNumRows() == 0 ) {
					continue ;
				}

				startYear = data.getInt( 0 , 0 ) ;
				endYear = data.getInt( data.getNumRows() - 1 , 0 ) ;
				break ;
			}

			if ( startYear == 0 || endYear == 0 ) {
				return false ;
			}

			conExecution = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION ) ;
			// put this connection into EPATableModel; fix to MOVES-HVI Alpha Test Problem 81
			//EPATableModel.dbConnection = conExecution;

			sql = "select svp.sourceTypeModelYearID, fuelTypeID, engTechID,"
					+ " sum(stmyFraction) as fuelEngFraction"
					+ " from sampleVehiclePopulation svp"
					+ " where svp.modelYearID between " + startYear + " and " + endYear
					+ " group by svp.sourceTypeModelYearID, fuelTypeID, engTechID"
					+ " order by svp.sourceTypeModelYearID, fuelTypeID, engTechID";
			rc = tableFraction.sqlSelect( conExecution, sql ) ;
			if ( rc < 0 ) {
				return false ;
			}

			sql = "SELECT * FROM SourceTypeModelYear " +
					"WHERE ModelYearId BETWEEN " + startYear + " AND " + endYear ;
			rc = tableSourceTypeModelYear.sqlSelect( conExecution , sql , true ) ;
			if ( rc < 0 ) {
				return false ;
			}

			indexYear = tableSourceTypeModelYear.getColumnIndex( "ModelYearId" ) ;
			indexSource = tableSourceTypeModelYear.getColumnIndex( "SourceTypeId" ) ;

			return true ;
		} catch( Exception ex ) {
			Logger.log( LogMessageCategory.WARNING ,
					"Error in AVFTMasterLoopable.doSetup()" ) ;
			return false ;
		} finally {
			/*
			tableSourceTypeModelYear.printDataTable( "stmy.del" ) ;
			tableFraction.printDataTable( "fraction1.del" ) ;
			*/
		}
	}

	private static class AVFTTableLine {
		public int sourceTypeModelYearID;
		public int fuelTypeID;
		public int engTechID;
		public double fuelEngFraction;
	}

	/**
	 * Modify the Execution database
	 * @param changedSourceTypeModelYears set of Integers to be filled with
	 * sourceTypeModelYearIDs that were changed.
	 * @return true if no errors occurred
	**/
	boolean doDatabaseUpdatesUsingAVFTTable(TreeSet<Integer> changedSourceTypeModelYears) {
		if ( doSetup() == false ) {
			return false ;
		}

		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "select sourceTypeModelYearID, fuelTypeID, engTechID, fuelEngFraction"
				+ " from avft a"
				+ " inner join sourceTypeModelYear stmy on ("
				+ " 	stmy.sourceTypeID=a.sourceTypeID"
				+ " 	and stmy.modelYearID=a.modelYearID"
				+ " )"
				+ " inner join runspecModelYear rsmy on (rsmy.modelYearID=stmy.modelYearID)";

		try {
			ArrayList<AVFTTableLine> lines = new ArrayList<AVFTTableLine>();
			query.open(conExecution,sql);
			while(query.rs.next()) {
				AVFTTableLine a = new AVFTTableLine();
				a.sourceTypeModelYearID = query.rs.getInt(1);
				a.fuelTypeID = query.rs.getInt(2);
				a.engTechID = query.rs.getInt(3);
				a.fuelEngFraction = query.rs.getDouble(4);
				lines.add(a);
			}
			query.close();
			for(Iterator<AVFTTableLine> i=lines.iterator();i.hasNext();) {
				AVFTTableLine a = i.next();
				if(applyNewFraction( a.sourceTypeModelYearID , a.fuelTypeID , a.engTechID , (float)a.fuelEngFraction )) {
					changedSourceTypeModelYears.add(Integer.valueOf(a.sourceTypeModelYearID));
				}
			}
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to apply AVFT changes using " + sql);
			return false;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Updates the tableFraction table with the NewFraction and sets the DeltaFraction
	 * If a matching row is not found for the search paramaters, then a new row is created
	 * @param sourceTypeModelYearId The search value for sourceTypeModelYearId
	 * @param fuelTypeId The search value for fuelTypeId
	 * @param engTechId The search value for engTechId
	 * @param newFraction The new value for the fraction
	 * @return true if the new fraction was an actual change
	**/
	boolean applyNewFraction( int sourceTypeModelYearId , int fuelTypeId ,
			int engTechId , float newFraction ) {
		boolean shouldLog = false;
		if(isDebugging) {
			if(fuelTypeId == 5 && newFraction > 0) {
				shouldLog = true;
				Logger.log(LogMessageCategory.INFO,"AVFT applyNewFraction stmy="+sourceTypeModelYearId
						+ ", ft=" + fuelTypeId + ", et=" + engTechId
						+ ", frac=" + newFraction);
			}
		}

		int row , rowStart , rowYear ;
		double oldFraction;
		double difference;
		String sql = "";

		for ( row = 0 ; row < tableFraction.getNumRows() ; row++ ) {
			row = tableFraction.find( sourceTypeModelYearId , 0 , row ) ;
			if ( row == -1 ) {
				// Per the basic definition of sampleVehiclePopulation, if the row doesn't exist
				// in sampleVehiclePopulation, it can't exist.
				return false;
			}
			if ( tableFraction.getInt( row , 1 ) != fuelTypeId ||
					tableFraction.getInt( row , 2 ) != engTechId ) {
				continue ;
			}

			// Set the NewFraction and return
			oldFraction = tableFraction.getFloat( row , "FuelEngFraction" ) ;
			difference = newFraction - oldFraction;
			if(difference >= 0.000001) {
				//System.out.println("AVFT adding vehicles, new=" + newFraction + ", old=" + oldFraction);
				// Add vehicles
				sql = "update sampleVehiclePopulation set "
						+ " stmyFraction=stmyFuelEngFraction*" + newFraction
						+ " where sourceTypeModelYearID=" + sourceTypeModelYearId
						+ " and fuelTypeID=" + fuelTypeId
						+ " and engTechID=" + engTechId;
			} else if(difference <= -0.000001) {
				//System.out.println("AVFT subtracting vehicles, new=" + newFraction + ", old=" + oldFraction);
				// Subtract vehicles
				sql = "update sampleVehiclePopulation set "
						+ " stmyFraction=stmyFraction*" + newFraction
						+ "/" + oldFraction
						+ " where sourceTypeModelYearID=" + sourceTypeModelYearId
						+ " and fuelTypeID=" + fuelTypeId
						+ " and engTechID=" + engTechId;
			} else {
				return false;
			}

			try {
				SQLRunner.executeSQL(conExecution,sql);
				return true;
			} catch(SQLException e) {
				Logger.logSqlError(e,"Unable to apply new FuelEngFraction",sql);
				return false;
			}
		}
		return false;
	}

	/**
	 * Finds the value of the SourceTypeModelYearId from the tableSourceTypeModelYear
	 * for a given SourceType and Year
	 * @param sourceType The Source Type to search for
	 * @param year The Year to search for
	 * @return Returns the row number of the matching record or -1 if no match is found
	**/
	int getSourceTypeModelYearId( int sourceType , int year ) {
		int row = findRowSourceTypeModelYear( sourceType , year ) ;

		return row == -1 ? 0 : tableSourceTypeModelYear.getInt( row , 0 ) ;
	}

	/**
	 * Finds the row in the tableFraction table for a given
	 * SourceTypeModelYearId, FuelTypeId , and EngTechId
	 * @param id The SourceTypeModelYearId to search for
	 * @param fuelTypeId The FuelTypeId to search for
	 * @param engTechId The EngTechId to search for
	 * @return Returns the row number of the matching record or -1 if no match is found
	**/
	int findRowFraction( int id , int fuelTypeId , int engTechId ) {
		String s;
		if(tableFractionIndex == null) {
			tableFractionIndex = new TreeMap<String,Integer>();
			int indexId = tableFraction.getColumnIndex( "SourceTypeModelYearId" ) ;
			int indexFuel = tableFraction.getColumnIndex( "FuelTypeId" ) ;
			int indexEng = tableFraction.getColumnIndex( "EngTechId" ) ;
			for ( int row = 0 ; row < tableFraction.getNumRows() ; row++ ) {
				s = tableFraction.getInt( row , indexId ) + "|"
						+ tableFraction.getInt( row , indexFuel ) + "|"
						+ tableFraction.getInt( row , indexEng );
				tableFractionIndex.put(s, Integer.valueOf(row));
			}
		}

		s = id + "|" + fuelTypeId + "|" + engTechId;
		Integer r = (Integer)tableFractionIndex.get(s);
		if(r == null) {
			return -1;
		} else {
			return r.intValue();
		}
	}

	/**
	 * Finds the row in the tableSourceTypeModelYear for a given SourceType and Year
	 * @param sourceType The Source Type to search for
	 * @param year The Year to search for
	 * @return Returns the row number of the matching record or -1 if no match is found
	**/
	int findRowSourceTypeModelYear( int sourceType , int year ) {
		for ( int row = 0 ; row < tableSourceTypeModelYear.getNumRows() ; row++ ) {
			if ( sourceType == tableSourceTypeModelYear.getInt( row , indexSource ) &&
					year == tableSourceTypeModelYear.getInt( row , indexYear ) ) {
				return row ;
			}
		}
		return -1 ;
	}

	/** Check affected tables for appropriate distributions **/
	void doDiagnosticChecks() {
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		Statement statement = null;
		ResultSet rs = null;
		try {
			// No diagnostics to do here
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to do AVFT diagnostics",sql);
		} finally {
			query.onFinally();
		}
	}
}
