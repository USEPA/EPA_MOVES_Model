/**************************************************************************************************
 * @(#)MesoscaleLookupTotalActivityGenerator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.sql.*;

/**
 * This Class builds "Total Activity" records for the Mesoscale Lookup process.
 * Refer to the TotalActivityGenerator class and Task 224 for the basis of the calculations
 * herein.
 *
 * @author		Wesley Faler
 * @version		2013-09-17
**/
public class MesoscaleLookupTotalActivityGenerator extends Generator {
	/** @notused **/

	/** Flag for whether the data tables have been cleared/setup **/
	boolean initialLoop = true;
	/** Database connection used by all functions.  Setup by executeLoop and cleanDataLoop. **/
	Connection db;
	/** Base year used for calculations. **/
	int baseYear = 0;
	/** Current year of result set **/
	int resultsYear = 0;
	/** ID of the last zone that Total Activity was generated for **/
	int currentZoneID = 0;
	/** Comma separated list of links performed. **/
	String linksInZone;
	/** ID of the last link that Total Activity was generated for **/
	int currentLinkID = 0;
	/** The current analysis year for the zone being processed **/
	int currentYearForZone = 0;
	/** The current emission process **/
	EmissionProcess currentProcess = null;
	/** The Running Exhaust emission process **/
	EmissionProcess runningExhaustProcess = EmissionProcess.findByName("Running Exhaust");
	/** The Evap Permeation emission process **/
	EmissionProcess evapPermeationProcess = EmissionProcess.findByName("Evap Permeation");
	/** The Evap Fuel Vapor Venting emission process **/
	EmissionProcess evapFuelVaporVentingProcess = EmissionProcess.findByName("Evap Fuel Vapor Venting");
	/** The Evap Fuel Leaks emission process **/
	EmissionProcess evapFuelLeaksProcess = EmissionProcess.findByName("Evap Fuel Leaks");
	/** The Evap Non-Fuel Vapors emission process **/
	EmissionProcess evapNonFuelVaporsProcess = EmissionProcess.findByName("Evap Non-Fuel Vapors");
	/** The Brakewear emission process **/
	EmissionProcess brakeWearProcess = EmissionProcess.findByName("Brakewear");
	/** The Tirewear emission process **/
	EmissionProcess tireWearProcess = EmissionProcess.findByName("Tirewear");

	/** Default constructor **/
	public MesoscaleLookupTotalActivityGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		if(runningExhaustProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Running Exhaust")) {
			targetLoop.subscribe(this, runningExhaustProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(evapPermeationProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Evap Permeation")) {
			targetLoop.subscribe(this, evapPermeationProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(evapFuelVaporVentingProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Evap Fuel Vapor Venting")) {
			targetLoop.subscribe(this, evapFuelVaporVentingProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(evapFuelLeaksProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Evap Fuel Leaks")) {
			targetLoop.subscribe(this, evapFuelLeaksProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(evapNonFuelVaporsProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Evap Non-Fuel Vapors")) {
			targetLoop.subscribe(this, evapNonFuelVaporsProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(brakeWearProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Brakewear")) {
			targetLoop.subscribe(this, brakeWearProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(tireWearProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Tirewear")) {
			targetLoop.subscribe(this, tireWearProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
	}

	/**
	 * Called each time the year changes.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

			if(initialLoop) {
				setup(inContext.iterProcess);
			}

			if(inContext.year > resultsYear) {
				baseYear = determineBaseYear(inContext.year);
				if(baseYear > resultsYear) {
					calculateBaseYearPopulation();
				}

				growPopulationToAnalysisYear(inContext.year);
				calculateFractionOfTravelUsingHPMS(inContext.year);
				allocateVMTByRoadTypeSourceAge();
				calculateVMTByRoadwayHour();
				convertVMTToTotalActivityBasis();
				resultsYear = inContext.year;
			}

			allocateTotalActivityBasis(inContext);
			calculateDistance(inContext);
			initialLoop = false;
		} catch (Exception e) {
			Logger.logError(e,"Total Activity Generation failed for year "+inContext.year);
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use 
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		// NOTE: Due to the data caching this object does, all cleanup is performed incrementally
		// ----- before writing new data.  The cleanup is done by clearActivityTables() and is
		// called by allocateTotalActivityBasis(...).
	}

	/**
	 * Create all the tables needed by the Total Activity Generator and purge any data left over
	 * in them from a previous run.
	 * @param initialProcess The emission process at the start of the loop.
	 * @throws SQLException If setup cannot be completed.
	**/
	void setup(EmissionProcess initialProcess) throws SQLException {
		String sql = "";

		// Keep track of the current emission process
		currentProcess = initialProcess;

		//
		// Succeeding years may(if there is not an intervening base year) grow values from the
		// following tables to the current year so the data in these tables must be kept for
		// the full run.
		sql = "CREATE TABLE IF NOT EXISTS SourceTypeAgePopulation ("+
					"yearID         SMALLINT NOT NULL,"+
					"sourceTypeID   SMALLINT NOT NULL,"+
					"ageID          SMALLINT NOT NULL,"+
					"population     FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKSourceTypeAgePopulation ("+
						"yearID, sourceTypeID, ageID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE SourceTypeAgePopulation";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS AnalysisYearVMT ("+
					"yearID      SMALLINT NOT NULL,"+
					"HPMSVTypeID SMALLINT NOT NULL,"+
					"VMT         FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKAnalysisYearVMT ("+
						"yearID, HPMSVTypeID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE AnalysisYearVMT";
		SQLRunner.executeSQL(db,sql);

		//
		// The following tables contain data that are used during every loop.
		sql = "CREATE TABLE IF NOT EXISTS SHOByAgeRoadwayHour ("+
				"yearID         SMALLINT NOT NULL,"+
				"roadTypeID     SMALLINT NOT NULL,"+
				"sourceTypeID   SMALLINT NOT NULL,"+
				"ageID          SMALLINT NOT NULL,"+
				"monthID        SMALLINT NOT NULL,"+
				"dayID          SMALLINT NOT NULL,"+
				"hourID         SMALLINT NOT NULL,"+
				"SHO            FLOAT NOT NULL,"+
				"UNIQUE INDEX XPKSHOByAgeRoadwayHour ("+
					"yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, hourID))";
		SQLRunner.executeSQL(db, sql);

		sql = "TRUNCATE SHOByAgeRoadwayHour";
		SQLRunner.executeSQL(db, sql);

		sql = "CREATE TABLE IF NOT EXISTS StartsByAgeHour ("+
				"yearID         SMALLINT NOT NULL,"+
				"sourceTypeID   SMALLINT NOT NULL,"+
				"ageID          SMALLINT NOT NULL,"+
				"monthID        SMALLINT NOT NULL,"+
				"dayID          SMALLINT NOT NULL,"+
				"hourID         SMALLINT NOT NULL,"+
				"starts         FLOAT NOT NULL,"+
				"UNIQUE INDEX XPKStartsByAgeHour ("+
					"yearID, sourceTypeID, ageID, monthID, dayID, hourID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE StartsByAgeHour";
		SQLRunner.executeSQL(db, sql);
		
		sql = "CREATE TABLE IF NOT EXISTS IdleHoursByAgeHour ("+
				"yearID         SMALLINT NOT NULL,"+
				"sourceTypeID   SMALLINT NOT NULL,"+
				"ageID          SMALLINT NOT NULL,"+
				"monthID        SMALLINT NOT NULL,"+
				"dayID          SMALLINT NOT NULL,"+
				"hourID         SMALLINT NOT NULL,"+
				"idleHours      FLOAT NOT NULL,"+
				"UNIQUE INDEX XPKIdleHoursByAgeHour ("+
					"yearID, sourceTypeID, ageID, monthID, dayID, hourID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE IdleHoursByAgeHour";
		SQLRunner.executeSQL(db,sql);

		//
		// The following tables contain data that should be cleaned out each loop.
		sql = "CREATE TABLE IF NOT EXISTS VMTByAgeRoadwayHour ("+
					"yearID        SMALLINT NOT NULL,"+
					"roadTypeID    SMALLINT NOT NULL,"+
					"sourceTypeID  SMALLINT NOT NULL,"+
					"ageID         SMALLINT NOT NULL,"+
					"monthID       SMALLINT NOT NULL,"+
					"dayID         SMALLINT NOT NULL,"+
					"hourID        SMALLINT NOT NULL,"+
					"VMT           FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKVMTByAgeRoadwayHour("+
						"yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, hourID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE VMTByAgeRoadwayHour";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS SourceTypeAgePopulation2 ("+
					"yearID         SMALLINT NOT NULL,"+
					"sourceTypeID   SMALLINT NOT NULL,"+
					"ageID          SMALLINT NOT NULL,"+
					"population     FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKSourceTypeAgePopulation2 ("+
						"yearID, sourceTypeID, ageID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE SourceTypeAgePopulation2";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS HPMSVTypePopulation ("+
					"yearID       SMALLINT NOT NULL,"+
					"HPMSVTypeID  SMALLINT NOT NULL,"+
					"population   FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKHPMSVTypePopulation("+
						"yearID, HPMSVTypeID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE HPMSVTypePopulation";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS FractionWithinHPMSVType ("+
					"yearID       SMALLINT NOT NULL,"+
					"sourceTypeID SMALLINT NOT NULL,"+
					"ageID        SMALLINT NOT NULL,"+
					"fraction     FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKFractionWithinHPMSVType ("+
						"yearID, sourceTypeID, ageID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE FractionWithinHPMSVType";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS HPMSTravelFraction ("+
				"yearID      SMALLINT NOT NULL,"+
				"HPMSVTypeID SMALLINT NOT NULL,"+
				"fraction    FLOAT NOT NULL,"+
				"UNIQUE INDEX XPKHPMSTravelFraction ("+
					"yearID, HPMSVTypeID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE HPMSTravelFraction";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS TravelFraction ("+
					"yearID        SMALLINT NOT NULL,"+
					"sourceTypeID  SMALLINT NOT NULL,"+
					"ageID         SMALLINT NOT NULL,"+
					"fraction      FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKTravelFraction("+
						"yearID, sourceTypeID, ageID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE TravelFraction";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS AnalysisYearVMT2 ("+
					"yearID      SMALLINT NOT NULL,"+
					"HPMSVTypeID SMALLINT NOT NULL,"+
					"VMT         FLOAT NOT NULL,"+
					"UNIQUE INDEX AnalysisYearVMT2("+
						"yearID, HPMSVTypeID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE AnalysisYearVMT2";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS AnnualVMTByAgeRoadway ("+
				"yearID        SMALLINT NOT NULL,"+
				"roadTypeID    SMALLINT NOT NULL,"+
				"sourceTypeID  SMALLINT NOT NULL,"+
				"ageID         SMALLINT NOT NULL,"+
				"VMT           FLOAT NOT NULL,"+
				"UNIQUE INDEX XPKAnnualVMTByAgeRoadway("+
					"yearID, roadTypeID, sourceTypeID, ageID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE AnnualVMTByAgeRoadway";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS AverageSpeed ("+
				"roadTypeID    SMALLINT NOT NULL,"+
				"sourceTypeID  SMALLINT NOT NULL,"+
				"dayID         SMALLINT NOT NULL,"+
				"hourID        SMALLINT NOT NULL,"+
				"averageSpeed  FLOAT NOT NULL,"+
				"UNIQUE INDEX XPKAverageSpeed ("+
					"roadTypeID, sourceTypeID, dayID, hourID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE AverageSpeed";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS SHOByAgeDay ("+
					"yearID         SMALLINT NOT NULL,"+
					"sourceTypeID   SMALLINT NOT NULL,"+
					"ageID          SMALLINT NOT NULL,"+
					"monthID        SMALLINT NOT NULL,"+
					"dayID          SMALLINT NOT NULL,"+
					"SHO            FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKSHOByAgeDay("+
						"yearID, sourceTypeID, ageID, monthID, dayID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE SHOByAgeDay";
		SQLRunner.executeSQL(db,sql);
/*
		sql = "CREATE TABLE IF NOT EXISTS SH ("+
					"hourDayID      SMALLINT NOT NULL,"+
					"monthID        SMALLINT NOT NULL,"+
					"yearID         SMALLINT NOT NULL,"+
					"ageID          SMALLINT NOT NULL,"+
					"zoneID         INTEGER NOT NULL,"+
					"sourceTypeID   SMALLINT NOT NULL,"+
					"roadTypeID     SMALLINT NOT NULL,"+
					"SH             FLOAT NOT NULL,"+
					"SHCV           FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKSH("+
						"hourDayID,monthID,yearID,ageID,zoneID,sourceTypeID,roadTypeID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE SH";
		SQLRunner.executeSQL(db,sql);
*/
		sql = "CREATE TABLE IF NOT EXISTS SHP ("+
				"hourDayID         SMALLINT NOT NULL,"+
				"monthID              SMALLINT NOT NULL,"+
				"yearID               SMALLINT NOT NULL,"+
				"ageID                SMALLINT NOT NULL,"+
				"zoneID               INTEGER NOT NULL,"+
				"sourceTypeID         SMALLINT NOT NULL,"+
				"SHP                  FLOAT NULL,"+
				"UNIQUE INDEX XPKSPH("+
				"hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID))";
		SQLRunner.executeSQL(db,sql);
		
		sql = "TRUNCATE SHP";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Tag-0: Find the year with base population data that is closest to the analysis year.
	 * @param analysisYear The year being analyzed.
	 * @return The base year for the year being analyzed.
	 * @throws Exception If the base year cannot be determined.
	**/
	int determineBaseYear(int analysisYear) throws Exception {
		String sql = "";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			sql = "SELECT "+
						"MAX(yearId) "+
					"FROM "+
						"Year "+
					"WHERE "+
						"yearId <= ? AND "+
						"(isBaseYear = 'Y' OR isBaseyear = 'y')";
			statement=db.prepareStatement(sql);
			statement.setInt(1,analysisYear);

			results = SQLRunner.executeQuery(statement,sql);
			int maxYearID = 0;
			if(results!=null) {
				if(results.next()) {
					maxYearID = results.getInt(1);
					if(results.wasNull()) {
						maxYearID = 0;
					}
				}
			}
			if (maxYearID!=0) {
				return maxYearID;
			} else {
				throw new Exception("No base year found for specified analysis year.");
			}
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				results = null;
			}
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
		}
	}

	/**
	 * Tag-1: Calculate the base year vehicle population by age.
	 * @throws SQLException If base year population cannot be determined.
	**/
	void calculateBaseYearPopulation() throws SQLException {
		String sql = "";
		PreparedStatement statement = null;
		try {
			sql = "INSERT INTO SourceTypeAgePopulation ("+
						"yearID,"+
						"sourceTypeID,"+
						"ageID, "+
						"population) "+
					"SELECT "+
						"sty.YearID,"+
						"sty.SourceTypeID,"+
						"stad.AgeID, "+
						"sty.SourceTypePopulation * stad.AgeFraction "+
					"FROM "+
						"SourceTypeYear sty,"+
						"SourceTypeAgeDistribution stad "+
					"WHERE "+
						"sty.sourceTypeID = stad.sourceTypeID AND "+
						"sty.yearID = stad.yearID AND "+
						"sty.yearID = ?";
			statement = db.prepareStatement(sql);
			statement.setInt(1,baseYear);
			int rows = SQLRunner.executeSQL(statement,sql);
		} finally {
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
		}
	}

	/**
	 * Tag-2: Grow vehicle population from base year to analysis year.
	 * @param analysisYear the year to which the population data should be grown.
	 * @throws SQLException If population cannot be grown to the analysis year.
	**/
	void growPopulationToAnalysisYear(int analysisYear) throws SQLException {
		String sql = "";
		PreparedStatement statement = null;
		PreparedStatement copyStatement = null;
		PreparedStatement purgeStatement = null;
		PreparedStatement age0Statement = null;
		PreparedStatement ageXStatement = null;
		PreparedStatement age30PlusStatement = null;
		try {
			sql = "TRUNCATE SourceTypeAgePopulation2";
			SQLRunner.executeSQL(db,sql);

			//
			// Setup the SQL strings used in the loops
			String copySql =
					"INSERT INTO SourceTypeAgePopulation ("+
						"yearID,"+
						"sourceTypeID,"+
						"ageID,"+
						"population) "+
					"SELECT "+
						"yearID,"+
						"sourceTypeID,"+
						"ageID,"+
						"population "+
					"FROM "+
						"SourceTypeAgePopulation2";
			copyStatement = db.prepareStatement(copySql);

			String purgeSql = "TRUNCATE SourceTypeAgePopulation2";
			purgeStatement = db.prepareStatement(purgeSql);

			String age0Sql =
					"INSERT INTO SourceTypeAgePopulation2 ("+
						"yearID,"+
						"sourceTypeID,"+
						"ageID,"+
						"population) "+
					"SELECT "+
						"sty2.yearID,"+
						"sty.sourceTypeID,"+
						"stap.ageID,"+
						"(stap.population/sty.migrationRate)*sty2.salesGrowthFactor*"+
								"sty2.migrationRate "+
					"FROM "+
						"SourceTypeYear sty,"+
						"SourceTypeYear sty2,"+
						"SourceTypeAgePopulation stap "+
					"WHERE "+
						"sty.yearID = sty2.yearID-1 AND "+
						"sty.sourceTypeID = stap.sourceTypeID AND "+
						"sty2.yearID = ? AND "+
						"sty2.sourceTypeID = stap.sourceTypeID AND "+
						"stap.yearID = sty.yearID AND "+
						"stap.ageID = 0 AND "+
						"sty.migrationRate <> 0";
			age0Statement = db.prepareStatement(age0Sql);

			String ageXSql =
					"INSERT INTO SourceTypeAgePopulation2 ("+
						"yearID,"+
						"sourceTypeID,"+
						"ageID,"+
						"population) "+
					"SELECT "+
						"sty.yearID,"+
						"sty.sourceTypeID,"+
						"sta.ageID+1,"+
						"stap.population * sta.survivalRate * sty.migrationRate "+
					"FROM "+
						"SourceTypeYear sty, "+
						"SourceTypeAge sta, "+
						"SourceTypeAgePopulation stap "+
					"WHERE "+
						"sty.yearID = ? AND "+
						"sty.sourceTypeID = stap.sourceTypeID AND "+
						"sta.ageID = ?-1 AND "+
						"sta.sourceTypeID = stap.sourceTypeID AND "+
						"stap.yearID = sty.yearID-1 AND "+
						"stap.ageID = sta.ageID";
			ageXStatement = db.prepareStatement(ageXSql);

			String age30PlusSql =
					"INSERT INTO SourceTypeAgePopulation2 ("+
						"yearID,"+
						"sourceTypeID,"+
						"ageID,"+
						"population) "+
					"SELECT "+
						"sty.yearID,"+
						"sty.sourceTypeID,"+
						"sta2.ageID,"+
						"stap.population*sta.survivalRate*sty.migrationRate + stap2.population*"+
								"sta2.survivalRate*sty.migrationRate "+
					"FROM "+
						"SourceTypeYear sty,"+
						"SourceTypeAge sta,"+
						"SourceTypeAge sta2,"+
						"SourceTypeAgePopulation stap,"+
						"SourceTypeAgePopulation stap2 "+
					"WHERE "+
						"sty.yearID = ? AND "+
						"sty.sourceTypeID = stap.sourceTypeID AND "+
						"sta.ageID = 29 AND "+
						"sta.sourceTypeID = stap.sourceTypeID AND "+
						"sta2.ageID = 30 AND "+
						"sta2.sourceTypeID = stap.sourceTypeID AND "+
						"sta.sourceTypeID = stap.sourceTypeID AND "+
						"stap.yearID = sty.yearID-1 AND "+
						"stap.ageID = 29 AND "+
						"stap2.sourceTypeID = stap.sourceTypeID AND "+
						"stap2.yearID = stap.yearID AND "+
						"stap2.ageID = 30";
			age30PlusStatement = db.prepareStatement(age30PlusSql);

			int newYear = resultsYear;
			if(resultsYear<baseYear) {
				newYear = baseYear;
			}

			for (newYear=newYear+1;newYear<=analysisYear;newYear++) {
				age0Statement.setInt(1,newYear);
				SQLRunner.executeSQL(age0Statement,age0Sql);

				SQLRunner.executeSQL(copyStatement,copySql);

				SQLRunner.executeSQL(purgeStatement,purgeSql);

				for (int sourceAge=1;sourceAge<30;sourceAge++) {
					ageXStatement.setInt(1,newYear);
					ageXStatement.setInt(2,sourceAge);
					SQLRunner.executeSQL(ageXStatement,ageXSql);
				}

				age30PlusStatement.setInt(1,newYear);
				SQLRunner.executeSQL(age30PlusStatement,age30PlusSql);

				SQLRunner.executeSQL(copyStatement, copySql);

				SQLRunner.executeSQL(purgeStatement, purgeSql);
			}

			//
			// Source population data for years prior to the analysis year are no longer needed.
// Previous years cannot be deleted until everything above the year level has been run.
//			sql = "DELETE FROM SourceTypeAgePopulation WHERE "+
//					"	yearID<?";
//			statement = db.prepareStatement(sql);
//			statement.setInt(1,analysisYear);
//			SQLRunner.executeSQL(statement,sql);
//			statement.close();
		} finally {
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
			if(copyStatement!=null) {
				try {
					copyStatement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
			if(purgeStatement!=null) {
				try {
					purgeStatement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
			if(age0Statement!=null) {
				try {
					age0Statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
			if(ageXStatement!=null) {
				try {
					ageXStatement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
			if(age30PlusStatement!=null) {
				try {
					age30PlusStatement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
		}
	}

	/**
	 * Tag-3: Calculate the fraction of vehicle travel by HPMS type.
	 * @param analysisYear The current year being analyzed.
	 * @throws SQLException if Fraction of Travel by HPMS cannot be determined.
	**/
	void calculateFractionOfTravelUsingHPMS(int analysisYear) throws SQLException {
		String sql = "";

		sql = "TRUNCATE HPMSVTypePopulation";
		SQLRunner.executeSQL(db,sql);

		sql = "INSERT INTO HPMSVTypePopulation ("+
					"yearID,"+
					"HPMSVTypeID,"+
					"population) "+
				"SELECT "+
					"stap.yearID,"+
					"sut.HPMSVTypeID,"+
					"sum(stap.population) "+
				"FROM "+
					"SourceTypeAgePopulation stap,"+
					"SourceUseType sut "+
				"WHERE "+
					"stap.sourceTypeID = sut.sourceTypeID AND "+
					"stap.yearID = " + analysisYear +
				" GROUP BY "+
					"stap.yearID,"+
					"sut.HPMSVTypeID";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE FractionWithinHPMSVType";
		SQLRunner.executeSQL(db,sql);

		sql = "INSERT INTO FractionWithinHPMSVType ("+
					"yearID,"+
					"sourceTypeID,"+
					"ageID,"+
					"fraction) "+
				"SELECT "+
					"stap.yearID,"+
					"stap.sourceTypeID,"+
					"stap.ageID,"+
					"stap.population / hvtp.population "+
				"FROM "+
					"SourceTypeAgePopulation stap,"+
					"SourceUseType sut,"+
					"HPMSVTypePopulation hvtp "+
				"WHERE "+
					"stap.sourceTypeID = sut.sourceTypeID AND "+
					"sut.HPMSVTypeID = hvtp.HPMSVTypeID AND "+
					"stap.yearID = hvtp.yearID AND "+
					"hvtp.population <> 0";
		SQLRunner.executeSQL(db,sql);
									//
									// Since this table is joined with HPMSVTypePopulation
									// and HPMSVTypePopulation only contains data for the
									// analysisYear, there is no need to specify the
									// analysisYear here.

		sql = "TRUNCATE HPMSTravelFraction";
		SQLRunner.executeSQL(db,sql);

		sql = "INSERT INTO HPMSTravelFraction ("+
					"yearID,"+
					"HPMSVTypeID,"+
					"fraction) "+
				"SELECT "+
					"fwhvt.yearID,"+
					"sut.HPMSVTypeID,"+
					"sum(fwhvt.fraction * sta.relativeMAR) "+
				"FROM "+
					"FractionWithinHPMSVType fwhvt,"+
					"SourceUseType sut,"+
					"SourceTypeAge sta "+
				"WHERE "+
					"sta.sourceTypeID = fwhvt.sourceTypeID AND "+
					"sta.ageID = fwhvt.ageID AND "+
					"fwhvt.sourceTypeID = sut.sourceTypeID "+
				"GROUP BY "+
					"fwhvt.yearID,"+
					"sut.HPMSVTypeID";
		SQLRunner.executeSQL(db,sql);
									//
									// Same as previous table except limiting table is
									// the previous table.

		sql = "TRUNCATE TravelFraction";
		SQLRunner.executeSQL(db,sql);

		sql = "INSERT INTO TravelFraction ("+
					"yearID,"+
					"sourceTypeID,"+
					"ageID,"+
					"fraction) "+
				"SELECT "+
					"fwhvt.yearID,"+
					"fwhvt.sourceTypeID,"+
					"fwhvt.ageID,"+
					"(fwhvt.fraction*sta.relativeMAR)/hpmstf.fraction "+
				"FROM "+
					"FractionWithinHPMSVType fwhvt,"+
					"SourceUseType sut,"+
					"SourceTypeAge sta,"+
					"HPMSTravelFraction hpmstf "+
				"WHERE "+
					"sta.sourceTypeID = fwhvt.sourceTypeID AND "+
					"sta.ageID = fwhvt.ageID AND "+
					"fwhvt.sourceTypeID = sut.sourceTypeID AND "+
					"hpmstf.yearID = fwhvt.yearID AND "+
					"hpmstf.HPMSVTypeID = sut.HPMSVTypeID AND "+
					"hpmstf.fraction <> 0";
		SQLRunner.executeSQL(db,sql);
									//
									// Same as previous table except limiting table is
									// the previous table.
	}

	/**
	 * Tag-5: Allocate VMT by road type, source type, and age.
	 * @throws SQLException If VMT cannot be allocated by road type, source, and age.
	**/
	void allocateVMTByRoadTypeSourceAge( ) throws SQLException {
		String sql = "";
		sql = "TRUNCATE AnnualVMTByAgeRoadway";
		SQLRunner.executeSQL(db,sql);

		sql = "INSERT INTO AnnualVMTByAgeRoadway ("
				+ " yearID,"
				+ " roadTypeID,"
				+ " sourceTypeID,"
				+ " ageID,"
				+ " VMT) "
				+ " SELECT" 
				+ " tf.yearID,"
				+ " rt.roadTypeID,"
				+ " tf.sourceTypeID,"
				+ " tf.ageID,"
				+ " tf.fraction "
				+ " FROM "
				+ " RoadType rt,"
				+ " TravelFraction tf";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Tag-6: Temporarlly Allocate VMT to Hours
	 * @throws SQLException If VMT cannot be allocated to hours.
	**/
	void calculateVMTByRoadwayHour() throws SQLException {
		WeeksInMonthHelper weekHelper = new WeeksInMonthHelper();
		String weeksPerMonthClause = 
				weekHelper.getWeeksPerMonthSQLClause("avar.yearID","avar.monthID");
		String sql = "";

		sql = "TRUNCATE VMTByAgeRoadwayHour";
		SQLRunner.executeSQL(db,sql);

		sql = "DROP TABLE IF EXISTS AvarMonth ";
		SQLRunner.executeSQL(db,sql);

		sql = "DROP TABLE IF EXISTS AvarMonthDay ";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE AvarMonth " +
				"SELECT avar.*, monthID, monthVMTFraction " +
				"FROM AnnualVMTByAgeRoadway as avar " +
				"INNER JOIN MonthVMTFraction AS mvf USING (sourceTypeID)";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE INDEX index1 ON AvarMonth (sourceTypeID, monthID, roadTypeID) ";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE AvarMonthDay " +
				"SELECT avarm.*, dayID, dayVMTFraction " +
				"FROM AvarMonth AS avarm INNER JOIN DayVMTFraction AS dvf " +
				"USING (sourceTypeID, monthID, roadTypeID) ";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE INDEX index1 ON AvarMonthDay(sourceTypeID, roadTypeID, dayID) ";
		SQLRunner.executeSQL(db,sql);

		sql = "INSERT INTO VMTByAgeRoadwayHour (yearID, roadTypeID, sourceTypeID, " +
					"ageID, monthID, dayID, hourID, VMT) " +
				"SELECT avar.yearID, avar.roadTypeID, avar.sourceTypeID, " +
					"avar.ageID, avar.monthID, avar.dayID, hvf.hourID, " +
					"avar.VMT*avar.monthVMTFraction*avar.dayVMTFraction*hvf.hourVMTFraction " +
					" / " + weeksPerMonthClause + " "+
					"FROM AvarMonthDay AS avar INNER JOIN HourVMTFraction AS hvf " +
				"USING(sourceTypeID, roadTypeID, dayID) ";
		SQLRunner.executeSQL(db,sql);
		
		sql = "DROP TABLE IF EXISTS AvarMonth ";
		SQLRunner.executeSQL(db,sql);

		sql = "DROP TABLE IF EXISTS AvarMonthDay ";
		SQLRunner.executeSQL(db,sql);
		// end of rewritten statement
	}

	/**
	 * Tag-7: Convert VMT to Total Activity Basis
	 * @throws SQLException If VMT cannot be converted to Total Activity Basis.
	**/
	void convertVMTToTotalActivityBasis() throws SQLException {
		String sql = "";

		// Because Distance is calculated from SHO and is divided out in the end, 
		// the actual SHO doesn't matter. But the proportional distribution of SHO 
		// among ages, sourcetypes and times must be preserved. This step sets SHO = VMT.
		sql = "INSERT INTO SHOByAgeRoadwayHour ("+
					"yearID,"+
					"roadTypeID,"+
					"sourceTypeID,"+
					"ageID,"+
					"monthID,"+
					"dayID,"+
					"hourID,"+
					"SHO) "+
				"SELECT "+
					"varh.yearID,"+
					"varh.roadTypeID,"+
					"varh.sourceTypeID,"+
					"varh.ageID,"+
					"varh.monthID,"+
					"varh.dayID,"+
					"varh.hourID,"+
					"varh.VMT "+
				"FROM VMTByAgeRoadwayHour varh ";
		SQLRunner.executeSQL(db,sql);
		
		sql = "TRUNCATE SHOByAgeDay";
		SQLRunner.executeSQL(db,sql);

		sql = "INSERT INTO SHOByAgeDay ("+
					"yearID,"+
					"sourceTypeID,"+
					"ageID,"+
					"monthID,"+
					"dayID,"+
					"SHO) "+
				"SELECT "+
					"sho.yearID,"+
					"sho.sourceTypeID,"+
					"sho.ageID,"+
					"sho.monthID,"+
					"sho.dayID,"+
					"sum(sho.SHO) "+
				"FROM "+
					"SHOByAgeRoadwayHour sho "+
				"GROUP BY "+
					"sho.yearID,"+
					"sho.sourceTypeID,"+
					"sho.ageID,"+
					"sho.monthID,"+
					"sho.dayID";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Remove records from SHO, ExtendedIdleHours, and Starts based upon the 
	 * currentZoneID, and currentLinkID member variables.  This is done anytime new data is
	 * generated for these activity output tables (which is currently whenever a new year or
	 * new zone or new process is requested).
	 * @throws SQLException If failed to allocate Total Activity Basis.
	**/
	void clearActivityTables() throws SQLException {
		if(currentZoneID == 0 || currentYearForZone == 0) {
			return;
		}
		if(ExecutionRunSpec.shouldSaveData(this)) {
			return;
		}
		String sql = "";
		try {
			sql = "DELETE FROM SHO WHERE isUserInput='N' AND linkID IN ("
					+ linksInZone.substring(1) + ")";
			SQLRunner.executeSQL(db, sql);
		
			sql = "DELETE FROM SourceHours WHERE isUserInput='N' "
					+ "AND linkID IN (" + linksInZone.substring(1) + ")";
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM ExtendedIdleHours WHERE isUserInput='N' "
					+ "AND zoneID = " + currentZoneID;
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM Starts WHERE isUserInput='N' "
					+ "AND zoneID = " + currentZoneID;
			SQLRunner.executeSQL(db, sql);
		} catch(Exception e) {
			Logger.logSqlError(e,"Could not delete Total Activity data from previous run.",sql);
		}
	}

	/**
	 * Tag-8: Allocate Total Activity Basis and Source Hours.
	 * @param inContext Current loop context being run.
	 * @throws SQLException If failed to allocate Total Activity Basis.
	**/
	void allocateTotalActivityBasis(MasterLoopContext inContext) throws SQLException {
		String sql = "";

		int analysisYear = inContext.year;
		int zoneID = inContext.iterLocation.zoneRecordID;

		// See if this is a new year for the current zone.
		boolean newYearForZone = false;
		if(inContext.iterProcess.compareTo(currentProcess)!=0) {
			clearActivityTables(); // do this before changing last known IDs
			currentProcess = inContext.iterProcess;
			newYearForZone = true;
			linksInZone = "";
			currentZoneID = zoneID;
			currentYearForZone = analysisYear;
		} else if(zoneID==currentZoneID) {
			if(currentYearForZone<analysisYear) {
				currentYearForZone = analysisYear;
				newYearForZone = true;
				linksInZone = "";
			}
		} else {
			clearActivityTables(); // do this before changing last known IDs
			currentZoneID = zoneID;
			currentYearForZone = analysisYear;
			newYearForZone = true;
			linksInZone = "";
		}
		currentLinkID = inContext.iterLocation.linkRecordID;
		linksInZone += "," + currentLinkID;

		boolean needSHO = false;
		boolean makeSH = false;

		if((evapPermeationProcess!=null &&
				inContext.iterProcess.compareTo(evapPermeationProcess)==0) ||
				(evapFuelVaporVentingProcess!=null && 
				inContext.iterProcess.compareTo(evapFuelVaporVentingProcess)==0) ||
		   		(evapFuelLeaksProcess!=null &&
	   			inContext.iterProcess.compareTo(evapFuelLeaksProcess)==0) ||
		   		(evapNonFuelVaporsProcess!=null &&
		   		inContext.iterProcess.compareTo(evapNonFuelVaporsProcess)==0)) {
			makeSH = true;
			if(inContext.iterLocation.roadTypeRecordID!=1 || newYearForZone) {
				needSHO = true;
			}
		}
		if((runningExhaustProcess!=null && 
				inContext.iterProcess.compareTo(runningExhaustProcess)==0) ||
				(brakeWearProcess!=null &&
				inContext.iterProcess.compareTo(brakeWearProcess)==0) ||
				(tireWearProcess!=null &&
				inContext.iterProcess.compareTo(tireWearProcess)==0)) {
			needSHO = true;
		}

		// Don't update the activity tables unless the zone changes
		if(newYearForZone) {
			if(needSHO) {
				// TAG8a is modified to assign SHO to multiple links of the same zone/roadtype.
				// Because geographic aggregation is forbidden for Lookup Output, allocation to
				// Zones can be uniform. Similarly, allocation to links is not important as 
				// long as the distribution among ages, sourcetypes & times is preserved. So we
				// set SHO(link) = SHO(roadType).  SHOCV is ignored.
				sql = "INSERT IGNORE INTO SHO ("+
							"hourDayID,"+
							"monthID,"+
							"yearID,"+
							"ageID,"+
							"linkID,"+
							"sourceTypeID,"+
							"SHO) "+
						"SELECT "+
							"hd.hourDayID,"+
							"sarh.monthID,"+
							"sarh.yearID,"+
							"sarh.ageID,"+
							"l.linkID,"+
							"sarh.sourceTypeID,"+
							"sarh.SHO "+
						"FROM "+
							"SHOByAgeRoadwayHour sarh,"+
							"Link l,"+
							"HourDay hd "+
						"WHERE "+
							"l.roadTypeID = sarh.roadTypeID AND "+
							"hd.hourID = sarh.hourID AND "+
							"hd.dayID = sarh.dayID AND "+
							"sarh.yearID = " + analysisYear + " AND "+
							"l.zoneID = " + zoneID;
				SQLRunner.executeSQL(db,sql);
			}
		}

		// Allocate SHO to SourceHours
		if(makeSH) {
			if(inContext.iterLocation.roadTypeRecordID!=1) {
				sql= "INSERT IGNORE INTO SourceHours (hourDayID, monthID, yearID, ageID, linkID, " +
						"sourceTypeID, sourceHours, sourceHoursCV, isUserInput) SELECT hourDayID, " +
						"monthID, yearID, ageID, linkID, sourceTypeID, SHO, SHOCV, 'N' " +
						"FROM SHO sho WHERE sho.yearID = " + analysisYear +
						" AND linkID = " + currentLinkID;
				SQLRunner.executeSQL(db,sql);
			}
		}
	}

	/**
	 * Tag-9: Calculate distance
	 * @throws SQLException If failed to calculate distance.
	**/
	void calculateDistance(MasterLoopContext inContext) throws SQLException {
		// Only calculate distance if process is exhaust running
		// We could also insure roadtype not equal off network but this would
		//     be less general.
		if(!inContext.iterProcess.processName.equals(runningExhaustProcess.processName)) {
			return;
		}
		String sql = "update SHO, LinkAverageSpeed"
				+ " set SHO.distance=SHO.SHO*averageSpeed"
				+ " where LinkAverageSpeed.linkID=SHO.linkID"
				+ " and SHO.distance is null";
		SQLRunner.executeSQL(db,sql);
	}
}
