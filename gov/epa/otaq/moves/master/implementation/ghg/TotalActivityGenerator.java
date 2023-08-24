/**************************************************************************************************
 * @(#)TotalActivityGenerator.java  
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.sql.SQLException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.*;
import java.io.*;

/**
 * This Class builds "Total Activity" records for ELDB data.
 *
 * ELDB is the Execution Location Database shown logically in the MOVES
 * data flow diagram.  The ELDB is implemented physically as tables in
 * MOVESExecution which have locational identifiers (stateID,
 * countyID, zoneID, linkID, gridID, or roadtypeID) in their primary keys.
 * In some contexts the ELDB may also be considered to consist of all
 * tables in MOVESExecution which are not in EERDB.
 *
 * The EERDB is The Execution Emission Rate Database shown logically in the
 * MOVES data flow diagram, physically implemented as the EmissionRate,
 * GREETWellToPump, and (eventually) the GREETManfAndDisposal tables in MOVESExecution.
 *
 * Finds the year with base population data that is closest to the analysis year.
 * Calculates the base year vehicle population by age.
 * Grows vehicle population from base year to analysis year.
 * Calculates the fraction of vehicle travel by HPMS type.
 * Grows VMT from the base year to the analysis year.
 * Allocates VMT by road type, source type, and age.
 * Temporarlly Allocates VMT to Hours.
 * Converts VMT to Total Activity Basis.
 * Calculates Starts and Source Hours Parked.
 * Allocates Total Activity Basis, Starts, SHP and Source Hours.
 * Calculates distance traveled corresponding to SourceHours Operating, when some
 * pollutant has been selected for the Running process.
 *
 * @author		Wesley Faler
 * @author    Chiu Foong, EPA
 * @author		Sarah Luo, ERG
 * @author		William Aikman (minor correction to scappage rate calculation)
 * @author		Mitch C. (minor mods for Tasks 128,133,135,Task 18 Item 169)
 * @version		2014-05-25
 * @author 		John Covey - Task 1806 changes
 * @version 	2018-03-20
**/
public class TotalActivityGenerator extends Generator {
	/**
	 * @algorithm
	 * @owner Total Activity Generator
	 * @generator
	**/

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
	/** The Start Exhaust emission process **/
	EmissionProcess startExhaustProcess = EmissionProcess.findByName("Start Exhaust");
	/** The Extended Idle emission process **/
	EmissionProcess extendedIdleProcess = EmissionProcess.findByName("Extended Idle Exhaust");
	/** The Auxiliary Power Exhaust process **/
	EmissionProcess auxiliaryPowerProcess = EmissionProcess.findByName("Auxiliary Power Exhaust");
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
	/** milliseconds spent during one time operations **/
	long setupTime = 0;
	/** milliseconds spent during growth operations **/
	long growthTime = 0;
	/** milliseconds spent during non-one-time and non-growth operations **/
	long totalTime = 0;
	/** milliseconds spent during a single operation as set during debugging/testing **/
	long focusTime = 0;
	/**
	 * Flags for tables, regions, and years that have been calcualted already.
	 * Data is formated as "table|regionid|year".
	**/
	TreeSetIgnoreCase calculationFlags = new TreeSetIgnoreCase();

	/** Default constructor **/
	public TotalActivityGenerator() {
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
					MasterLoopPriority.GENERATOR-3); // Run after BaseRateGenerator
		}
		if(startExhaustProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Start Exhaust")) {
			targetLoop.subscribe(this, startExhaustProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(extendedIdleProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Extended Idle Exhaust")) {
			targetLoop.subscribe(this, extendedIdleProcess, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(CompilationFlags.ENABLE_AUXILIARY_POWER_EXHAUST && auxiliaryPowerProcess != null
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Auxiliary Power Exhaust")) {
			targetLoop.subscribe(this, auxiliaryPowerProcess, MasterLoopGranularity.YEAR,
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

			long start, focusStart;

			if(initialLoop) {
				start = System.currentTimeMillis();
				setup(inContext.iterProcess); // steps 100-109
				setupTime += System.currentTimeMillis() - start;
			}

			if(inContext.year != resultsYear) {
				start = System.currentTimeMillis();
				baseYear = determineBaseYear(inContext.year); // step 110
				if(baseYear > resultsYear) {
					calculateBaseYearPopulation(); // steps 120-129
				}

				growPopulationToAnalysisYear(inContext.year); // steps 130-139
				calculateFractionOfTravelUsingHPMS(inContext.year); // steps 140-149
				growVMTToAnalysisYear(inContext.year); // steps 150-159
				allocateVMTByRoadTypeSourceAge(inContext.year); // steps 160-169
				calculateVMTByRoadwayHour(inContext.year); // steps 170-179
				resultsYear = inContext.year;
				growthTime += System.currentTimeMillis() - start;
			}

			start = System.currentTimeMillis();
			allocateTotalActivityBasis(inContext); // steps 190-199
			calculateDistance(inContext); // steps 200-209
			initialLoop = false;
			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,"Total Activity Generation failed for year "+inContext.year);
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"TAG setupTime=" + setupTime + " growthTime=" + growthTime
				+ " bundleTime=" + totalTime + " focusTime=" + focusTime);
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

		setupAnalysisYearVMTTables(db);
		setupAgeTables(db);

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
				"hourDayID      SMALLINT NOT NULL DEFAULT 0,"+
				"SHO            DOUBLE NOT NULL,"+
				"VMT            DOUBLE NOT NULL,"+
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
				"starts         DOUBLE NOT NULL,"+
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
				"idleHours      DOUBLE NOT NULL,"+
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
					"VMT           DOUBLE NOT NULL,"+
					"hourDayID     SMALLINT NOT NULL DEFAULT 0,"+
					"UNIQUE INDEX XPKVMTByAgeRoadwayHour("+
						"yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, hourID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE VMTByAgeRoadwayHour";
		SQLRunner.executeSQL(db,sql);

		sql = "create table if not exists vmtByMYRoadHourFraction ("
				+ " 	yearID smallint not null,"
				+ " 	roadTypeID smallint not null,"
				+ " 	sourceTypeID smallint not null,"
				+ " 	modelYearID smallint not null,"
				+ " 	monthID smallint not null,"
				+ " 	dayID smallint not null,"
				+ " 	hourID smallint not null,"
				+ " 	hourDayID smallint not null,"
				+ " 	vmtFraction double,"
				+ " 	unique key (yearID, roadTypeID, sourceTypeID, modelYearID, monthID, hourID, dayID),"
				+ " 	unique key (yearID, roadTypeID, sourceTypeID, modelYearID, monthID, hourDayID)"
				+ " )";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE vmtByMYRoadHourFraction";
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
					"SHO            DOUBLE NOT NULL,"+
					"VMT            DOUBLE NOT NULL,"+
					"UNIQUE INDEX XPKSHOByAgeDay("+
						"yearID, sourceTypeID, ageID, monthID, dayID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE SHOByAgeDay";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE TABLE IF NOT EXISTS VMTByAgeRoadwayDay ("+
					"yearID         SMALLINT NOT NULL,"+
					"roadTypeID     SMALLINT NOT NULL,"+
					"sourceTypeID   SMALLINT NOT NULL,"+
					"ageID          SMALLINT NOT NULL,"+
					"monthID        SMALLINT NOT NULL,"+
					"dayID          SMALLINT NOT NULL,"+
					"VMT            DOUBLE NOT NULL,"+
					"hotellingHours DOUBLE NOT NULL,"+
					"primary key (yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE VMTByAgeRoadwayDay";
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
				"SHP                  DOUBLE NULL,"+
				"UNIQUE INDEX XPKSPH("+
				"hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE SHP";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Create and truncate the AnalysisYearVMT, and related, tables.
	 * @param db database to hold the tables
	 * @throws SQLException if anything goes wrong
	**/
	public static void setupAnalysisYearVMTTables(Connection db) throws SQLException {
		String sql = "";

		sql = "CREATE TABLE IF NOT EXISTS AnalysisYearVMT ("+
					"yearID      SMALLINT NOT NULL,"+
					"HPMSVTypeID SMALLINT NOT NULL,"+
					"VMT         FLOAT NOT NULL,"+
					"UNIQUE INDEX XPKAnalysisYearVMT ("+
						"yearID, HPMSVTypeID))";
		SQLRunner.executeSQL(db,sql);

		sql = "TRUNCATE AnalysisYearVMT";
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
	}

	/**
	 * Remove AnalysisYearVMT, and related, tables.
	 * @param db database to hold the tables
	 * @throws SQLException if anything goes wrong
	**/
	public static void removeAnalysisYearVMTTables(Connection db) throws SQLException {
		String sql = "";

		sql = "drop table if exists AnalysisYearVMT";
		SQLRunner.executeSQL(db,sql);

		sql = "drop table if exists AnalysisYearVMT2";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Create and truncate the SourceTypeAgePopulation, and related, tables.
	 * @param db database to hold the tables
	 * @throws SQLException if anything goes wrong
	**/
	public static void setupAgeTables(Connection db) throws SQLException {
		String sql = "";

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
	}

	/**
	 * Remove SourceTypeAgePopulation, and related, tables.
	 * @param db database to hold the tables
	 * @throws SQLException if anything goes wrong
	**/
	public static void removeAgeTables(Connection db) throws SQLException {
		String sql = "";

		sql = "drop table if exists SourceTypeAgePopulation";
		SQLRunner.executeSQL(db,sql);

		sql = "drop table if exists SourceTypeAgePopulation2";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Tag-0: Find the year with base population data that is closest to the analysis year.
	 * @param analysisYear The year being analyzed.
	 * @return The base year for the year being analyzed.
	 * @throws Exception If the base year cannot be determined.
	**/
	int determineBaseYear(int analysisYear) throws Exception {
		return determineBaseYear(db, analysisYear);
	}

	/**
	 * Tag-0: Find the year with base population data that is closest to the analysis year.
	 * @param db database to be examined
	 * @param analysisYear The year being analyzed.
	 * @return The base year for the year being analyzed.
	 * @throws Exception If the base year cannot be determined.
	**/
	public static int determineBaseYear(Connection db, int analysisYear) throws Exception {
		String sql = "";
		ResultSet results = null;
		PreparedStatement statement = null;
		try {
			/**
			 * @step 110
			 * @algorithm baseYear = max(year) where year <= analysisYear and year is a base year.
			 * @input year
			**/
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
				//System.out.println("Base Year for " + analysisYear + " is " + maxYearID);
				return maxYearID;
			} else {
				throw new Exception("No base year found for analysis year " + analysisYear);
			}
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(SQLException e) {
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
	 * Populate SourceTypeAgePopulation for a range of years
	 * @param db database to populate
	 * @param firstYear first year, inclusive, of the range to be populated
	 * @param lastYear last year, inclusive, of the range to be populated
	 * @throws Exception if the population cannot be grown or if there is no base year available
	**/
	public static void growPopulation(Connection db, int firstYear, int lastYear) throws Exception {
		int baseYear = 0;
		int resultsYear = 0;
		for(int y=firstYear;y<=lastYear;y++) {
			baseYear = determineBaseYear(db,y);
			if(baseYear > resultsYear) {
				calculateBaseYearPopulation(db,baseYear);
			}
			growPopulationToAnalysisYear(db,y,baseYear,resultsYear);
			resultsYear = y;
		}
	}

	/**
	 * Tag-1: Calculate the base year vehicle population by age.
	 * @throws SQLException If base year population cannot be determined.
	**/
	void calculateBaseYearPopulation() throws SQLException {
		calculateBaseYearPopulation(db,baseYear);
	}

	/**
	 * Calculate the base year vehicle population by age.
	 * @param db database to use
	 * @param baseYear year that is the basis for the population
	 * @throws SQLException If base year population cannot be determined.
	**/
	public static void calculateBaseYearPopulation(Connection db, int baseYear)
			throws SQLException {
		String sql = "";

		sql = "delete from SourceTypeAgePopulation"
					+ " where yearID >= " + baseYear;
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 120
		 * @algorithm population = sourceTypePopulation * ageFraction.
		 * @output SourceTypeAgePopulation
		 * @input SourceTypeYear
		 * @input SourceTypeAgeDistribution
		**/
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
					"sty.yearID = " + baseYear;
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Tag-2: Grow vehicle population from base year to analysis year.
	 * @param analysisYear the year to which the population data should be grown.
	 * @throws SQLException If population cannot be grown to the analysis year.
	**/
	void growPopulationToAnalysisYear(int analysisYear) throws SQLException {
		growPopulationToAnalysisYear(db,baseYear,resultsYear,analysisYear);
	}

	/**
	 * Grow vehicle population from base year to analysis year.
	 * @param db database to use
	 * @param baseYear year holding the population basis
	 * @param resultsYear any year between baseYear and analysisYear for which
	 * the population has already been calculated.  Set to 0 to indicate there has
	 * been no calculations since the base year.
	 * @param analysisYear the year to which the population data should be grown.
	 * @throws SQLException If population cannot be grown to the analysis year.
	**/
	public static void growPopulationToAnalysisYear(Connection db, int baseYear,
			int resultsYear, int analysisYear) throws SQLException {
		if(resultsYear < baseYear) {
			resultsYear = baseYear;
		}

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

			/**
			 * @step 130
			 * @algorithm Grow the age 0 population.
			 * population[ageID=0,y] = (population[y-1]/migrationRate[y-1])*salesGrowthFactor[y]*migrationRate[y].
			 * @output SourceTypeAgePopulation2
			 * @input SourceTypeYear for year y
			 * @input SourceTypeYear for year y-1
			 * @input SourceTypeAgePopulation for year y-1
			**/
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

			/**
			 * @step 130
			 * @algorithm Move the newly grown population to the main population table.
			 * @input SourceTypeAgePopulation2
			 * @output SourceTypeAgePopulation
			**/
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

			/**
			 * @step 130
			 * @algorithm Grow the population for 1 <= ageID < 30.
			 * population[ageID,y] = population[y-1,ageID-1]*survivalRate[ageID]*migrationRate[y].
			 * @output SourceTypeAgePopulation2
			 * @input SourceTypeYear for year y
			 * @input SourceTypeAge
			 * @input SourceTypeAgePopulation for year y-1
			**/
			String ageXSql =
					"INSERT INTO SourceTypeAgePopulation2 ("+
						"yearID,"+
						"sourceTypeID,"+
						"ageID,"+
						"population) "+
					"SELECT "+
						"sty.yearID,"+
						"sty.sourceTypeID,"+
					 	"sta.ageID+0,"+
						"stap.population * sta.survivalRate * sty.migrationRate "+
					"FROM "+
						"SourceTypeYear sty, "+
						"SourceTypeAge sta, "+
						"SourceTypeAgePopulation stap "+
					"WHERE "+
						"sty.yearID = ? AND "+
						"sty.sourceTypeID = stap.sourceTypeID AND "+
					 	"sta.ageID = ? AND "+
						"sta.sourceTypeID = stap.sourceTypeID AND "+
						"stap.yearID = sty.yearID-1 AND "+
						"stap.ageID = sta.ageID-1";
			ageXStatement = db.prepareStatement(ageXSql);

			/**
			 * @step 130
			 * @algorithm Grow the population ageID >= 30.
			 * population[ageID,y] = population[ageID=29,y-1]*survivalRate[ageID=29]*migrationRate[y] + population[ageID=30,y]*survivalRate[ageID=30]*migrationRate[y].
			 * @output SourceTypeAgePopulation2
			 * @input sty SourceTypeYear for year y
			 * @input sta SourceTypeAge for ageID=29
			 * @input sta2 SourceTypeAge for ageID=30
			 * @input stap SourceTypeAgePopulation for year y-1 and ageID=29
			 * @input stap2 SourceTypeAgePopulation for year y and ageID=30
			**/
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
//			statement.close();
//			statement = db.prepareStatement(sql);
//			statement.setInt(1,analysisYear);
//			SQLRunner.executeSQL(statement,sql);

			// Populate sourceTypeAgeDistribution with the distribution
			// within the analysis year.
			String[] statements = {
				"drop table if exists sourceTypeAgePopulationTotal",
				
				"create table sourceTypeAgePopulationTotal ("
				+ " 	sourceTypeID smallint not null,"
				+ " 	yearID smallint not null,"
				+ " 	totalPopulation double not null,"
				+ " 	primary key (yearID, sourceTypeID),"
				+ " 	unique key (sourceTypeID, yearID)"
				+ " )",

				/**
				 * @step 130
				 * @algorithm totalPopulation = sum(population).
				 * @output sourceTypeAgePopulationTotal
				 * @input sourceTypeagePopulation
				**/
				"insert into sourceTypeAgePopulationTotal (sourceTypeID, yearID, totalPopulation)"
				+ " select sourceTypeID, yearID, sum(population)"
				+ " from sourceTypeagePopulation"
				+ " where yearID=" + analysisYear
				+ " group by yearID, sourceTypeID"
				+ " order by null",

				/**
				 * @step 130
				 * @algorithm ageFraction = population/totalPopulation.
				 * @output sourceTypeAgeDistribution
				 * @input sourceTypeAgePopulation
				 * @input sourceTypeAgePopulationTotal
				**/
				"insert ignore into sourceTypeAgeDistribution ("
				+ " 	sourceTypeID, yearID, ageID, ageFraction)"
				+ " select detail.sourceTypeID, detail.yearID, detail.ageID, detail.population / total.totalPopulation"
				+ " from sourceTypeAgePopulation detail"
				+ " inner join sourceTypeAgePopulationTotal total on ("
				+ " 	total.sourceTypeID = detail.sourceTypeID"
				+ " 	and total.yearID = detail.yearID)"
				+ " where detail.yearID=" + analysisYear,

				"drop table if exists sourceTypeAgePopulationTotal"
			};
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
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

		/**
		 * @step 140
		 * @algorithm population[HPMSVTypeID] = sum(population[sourceTypeID]).
		 * @output HPMSVTypePopulation
		 * @input SourceTypeAgePopulation
		 * @input SourceUseType
		**/
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

		/**
		 * @step 140
		 * @algorithm FractionWithinHPMSVType = population[sourceTypeID]/population[HPMSVTypeID].
		 * @output FractionWithinHPMSVType
		 * @input HPMSVTypePopulation
		 * @input SourceTypeAgePopulation
		 * @input SourceUseType
		**/
		sql = "INSERT INTO FractionWithinHPMSVType ("+
					"yearID,"+
					"sourceTypeID,"+
					"ageID,"+
					"fraction) "+
				"SELECT "+
					"stap.yearID,"+
					"stap.sourceTypeID,"+
					"stap.ageID,"+
					"COALESCE(stap.population / hvtp.population, 0) "+
				"FROM "+
					"SourceTypeAgePopulation stap,"+
					"SourceUseType sut,"+
					"HPMSVTypePopulation hvtp "+
				"WHERE "+
					"stap.sourceTypeID = sut.sourceTypeID AND "+
					"sut.HPMSVTypeID = hvtp.HPMSVTypeID AND "+
					"stap.yearID = hvtp.yearID";
		SQLRunner.executeSQL(db,sql);
									//
									// Since this table is joined with HPMSVTypePopulation
									// and HPMSVTypePopulation only contains data for the
									// analysisYear, there is no need to specify the
									// analysisYear here.

		sql = "TRUNCATE HPMSTravelFraction";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 140
		 * @algorithm HPMSTravelFraction = sum(relativeMAR * FractionWithinHPMSVType).
		 * @output HPMSTravelFraction
		 * @input FractionWithinHPMSVType
		 * @input SourceUseType
		 * @input SourceTypeAge
		**/
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

		sql = "TRUNCATE TravelFraction";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 140
		 * @algorithm TravelFraction = (FractionWithinHPMSVType * relativeMAR) / HPMSTravelFraction.
		 * @output TravelFraction
		 * @input HPMSTravelFraction
		 * @input FractionWithinHPMSVType
		 * @input SourceUseType
		 * @input SourceTypeAge
		**/
		sql = "INSERT INTO TravelFraction ("+
					"yearID,"+
					"sourceTypeID,"+
					"ageID,"+
					"fraction) "+
				"SELECT "+
					"fwhvt.yearID,"+
					"fwhvt.sourceTypeID,"+
					"fwhvt.ageID,"+
					"COALESCE((fwhvt.fraction*sta.relativeMAR)/hpmstf.fraction, 0) "+
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
					"hpmstf.HPMSVTypeID = sut.HPMSVTypeID";
		SQLRunner.executeSQL(db,sql);

		// If VMT by source type has been provided, instead of by HPMSVType, then
		// normalize TravelFraction by year and sourcetype.
		sql = "select (select count(*) as howMany from SourceTypeDayVMT)+(select count(*) as howMany from SourceTypeYearVMT)";
		if(SQLRunner.executeScalar(db,sql) > 0) {
			sql = "drop table if exists TravelFractionSourceTypeSum";
			SQLRunner.executeSQL(db,sql);
	
			/**
			 * @step 140
			 * @algorithm totalTravelFraction(yearID,sourceTypeID) = Sum(TravelFraction).
			 * @output TravelFractionSourceTypeSum
			 * @input TravelFraction
			 * @condition VMT provided by sourcetype not HPMSVType
			**/
			sql = "create table TravelFractionSourceTypeSum"
					+ " select yearID, sourceTypeID, sum(fraction) as totalTravelFraction"
					+ " from travelFraction"
					+ " group by yearID, sourceTypeID"
					+ " order by null";
			SQLRunner.executeSQL(db,sql);
	
			/**
			 * @step 140
			 * @algorithm When VMT by source type has been provided, normalize TravelFraction by year and source type.
			 * normalized TravelFraction = TravelFraction / totalTravelFraction.
			 * @output TravelFraction
			 * @input TravelFractionSourceTypeSum
			 * @condition VMT provided by sourcetype not HPMSVType
			**/
			sql = "update TravelFraction, TravelFractionSourceTypeSum"
					+ " set fraction = case when totalTravelFraction > 0 then fraction / totalTravelFraction else 0 end"
					+ " where TravelFraction.yearID = TravelFractionSourceTypeSum.yearID"
					+ " and TravelFraction.sourceTypeID = TravelFractionSourceTypeSum.sourceTypeID";
			SQLRunner.executeSQL(db,sql);
		}
	}

	/**
	 * Tag-4: Grow VMT from the base year to the analysis year.
	 * @param analysisYear The year we are doing the analysis for
	 * @throws SQLException if the VMT cannot be grown to the analysis year.
	**/
	void growVMTToAnalysisYear(int analysisYear) throws SQLException {
		growVMTToAnalysisYear(db, analysisYear, baseYear, resultsYear, false);
	}

	/**
	 * Populate AnalysisYearVMT for each year in a range of years
	 * @param db database to populate
	 * @param firstYear first year, inclusive, of the range to be populated
	 * @param lastYear last year, inclusive, of the range to be populated
	 * @throws Exception if the VMT cannot be grown or if there is no base year available
	**/
	public static void growVMT(Connection db, int firstYear, int lastYear) throws Exception {
		int baseYear = 0;
		int resultsYear = 0;
		for(int y=firstYear;y<=lastYear;y++) {
			baseYear = determineBaseYear(db,y);
			growVMTToAnalysisYear(db,y,baseYear,resultsYear,false);
			resultsYear = y;
		}
	}

	/**
	 * Tag-4: Grow VMT from the base year to the analysis year.
	 * @param db database to use
	 * @param analysisYear The year we are doing the analysis for
	 * @param baseYear The base year for the year the analysis is for
	 * @param resultsYear the latest year yet calculated and already stored,
	 * 0 upon initial entry
	 * @throws SQLException if the VMT cannot be grown to the analysis year.
	**/
	public static void growVMTToAnalysisYear(Connection db, int analysisYear, int baseYear,
			int resultsYear, boolean shouldDeletePriorYears) throws SQLException {
		String sql = "";
		PreparedStatement statement = null;
		PreparedStatement copyStatement = null;
		PreparedStatement purgeStatement = null;
		try {
			if(baseYear > resultsYear) {
				sql = "DELETE FROM AnalysisYearVMT WHERE yearID >=" + baseYear;
				SQLRunner.executeSQL(db,sql);

				/**
				 * @step 150
				 * @algorithm VMT = HPMSBaseYearVMT.
				 * @output AnalysisYearVMT
				 * @input RunSpecSourceType
				 * @input SourceUseType
				 * @input HPMSVTypeYear
				**/
				sql = "INSERT IGNORE INTO AnalysisYearVMT ("+
							"yearID,"+
							"HPMSVTypeID,"+
							"VMT) "+
						"SELECT "+
							"hvty.yearID,"+
							"hvty.HPMSVTypeID,"+
							"hvty.HPMSBaseYearVMT "+
						"FROM "+
							"RunSpecSourceType rsst,"+
							"SourceUseType sut,"+
							"HPMSVTypeYear hvty "+
						"WHERE "+
							"rsst.sourceTypeID = sut.sourceTypeID AND "+
							"sut.HPMSVTypeID = hvty.HPMSVTypeID AND "+
							"hvty.yearID = ?";
				statement = db.prepareStatement(sql);
				statement.setInt(1,baseYear);
				SQLRunner.executeSQL(statement,sql);
				statement.close();
			}

			sql = "TRUNCATE AnalysisYearVMT2";
			statement = db.prepareStatement(sql);
			SQLRunner.executeSQL(statement,sql);

			/**
			 * @step 150
			 * @algorithm Grow VMT one year.
			 * VMT[y] = VMT[y-1] * VMTGrowthFactor[y].
			 * @output AnalysisYearVMT2
			 * @input AnalysisYearVMT for year y-1
			 * @input HPMSVTypeYear for year y
			**/
			sql = "INSERT INTO AnalysisYearVMT2 ("+
						"yearID,"+
						"HPMSVTypeID,"+
						"VMT) "+
					"SELECT "+
						"hvty.yearID,"+
						"ayv.HPMSVTypeID,"+
						"ayv.VMT * hvty.VMTGrowthFactor "+
					"FROM "+
						"AnalysisYearVMT ayv,"+
						"HPMSVTypeYear hvty "+
					"WHERE "+
						"ayv.yearID = hvty.yearID-1 AND "+
						"ayv.HPMSVTypeID = hvty.HPMSVTypeID AND "+
						"hvty.yearID = ?";
			statement.close();
			statement = db.prepareStatement(sql);

			/**
			 * @step 150
			 * @algorithm Copy AnalysisYearVMT2 data into AnalysisYearVMT.
			 * @input AnalysisYearVMT2
			 * @output AnalysisYearVMT
			**/
			String copySql =
					"INSERT INTO AnalysisYearVMT ("+
						"yearID,"+
						"HPMSVTypeID,"+
						"VMT) "+
					"SELECT "+
						"ayv2.yearID,"+
						"ayv2.HPMSVTypeID,"+
						"ayv2.VMT "+
					"FROM "+
						"AnalysisYearVMT2 ayv2";
			copyStatement = db.prepareStatement(copySql);

			String purgeSql = "TRUNCATE AnalysisYearVMT2";
			purgeStatement = db.prepareStatement(purgeSql);

			int newYear = resultsYear;
			if(newYear < baseYear) {
				newYear = baseYear;
			}

			for (newYear=newYear+1;newYear<=analysisYear;newYear++) {
				statement.setInt(1,newYear);
				SQLRunner.executeSQL(statement,sql);

				SQLRunner.executeSQL(copyStatement, copySql);
				SQLRunner.executeSQL(purgeStatement, purgeSql);
			}
			if(shouldDeletePriorYears) {
				//
				// VMT for years prior to the analysis year are no longer needed.
				sql = "DELETE FROM AnalysisYearVMT WHERE "+
							"yearID<?";
				statement.close();
				statement = db.prepareStatement(sql);
				statement.setInt(1,analysisYear);
				SQLRunner.executeSQL(statement,sql);
			}
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
		}
	}

	/**
	 * Tag-5: Allocate VMT by road type, source type, and age.
	 * @param yearID calendar year to be used
	 * @throws SQLException If VMT cannot be allocated by road type, source, and age.
	**/
	void allocateVMTByRoadTypeSourceAge(int yearID) throws SQLException {
		String sql = "";

		sql = "TRUNCATE AnnualVMTByAgeRoadway";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 160
		 * @algorithm VMT = VMT * roadTypeVMTFraction * TravelFraction.
		 * @output AnnualVMTByAgeRoadway
		 * @input RoadType
		 * @input TravelFraction
		 * @input AnalysisYearVMT
		 * @input RoadTypeDistribution
		 * @input SourceUseType
		 * @condition VMT provided by HPMSVType
		**/
		sql = "INSERT INTO AnnualVMTByAgeRoadway ("+
					"yearID,"+
					"roadTypeID,"+
					"sourceTypeID,"+
					"ageID,"+
					"VMT) "+
				"SELECT "+
					"tf.yearID,"+
					"rtd.roadTypeID,"+
					"tf.sourceTypeID,"+
					"tf.ageID,"+
					"ayv.vmt*rtd.roadTypeVMTFraction*tf.fraction "+
				"FROM "+
					"RoadType rsrt,"+ // was RunSpecRoadType
					"TravelFraction tf,"+
					"AnalysisYearVMT ayv,"+
					"RoadTypeDistribution rtd,"+
					"SourceUseType sut "+
				"WHERE "+
					"rsrt.roadTypeID = rtd.roadTypeID AND "+
					"ayv.yearID = tf.yearID AND "+
					"tf.sourceTypeID = sut.sourceTypeID AND "+
					"sut.HPMSVTypeID = ayv.HPMSVTypeID AND "+
					"rtd.sourceTypeID = tf.sourceTypeID";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 160
		 * @algorithm VMT = VMT * roadTypeVMTFraction * TravelFraction.
		 * @output AnnualVMTByAgeRoadway
		 * @input RoadType
		 * @input TravelFraction
		 * @input SourceTypeYearVMT
		 * @input RoadTypeDistribution
		 * @condition VMT provided by source type
		**/
		sql = "INSERT INTO AnnualVMTByAgeRoadway ("
				+ " 	yearID,"
				+ " 	roadTypeID,"
				+ " 	sourceTypeID,"
				+ " 	ageID,"
				+ " 	VMT)"
				+ " SELECT"
				+ " 	tf.yearID,"
				+ " 	rtd.roadTypeID,"
				+ " 	tf.sourceTypeID,"
				+ " 	tf.ageID,"
				+ " 	v.vmt*rtd.roadTypeVMTFraction*tf.fraction"
				+ " FROM"
				+ " 	RoadType rsrt,"
				+ " 	TravelFraction tf,"
				+ " 	SourceTypeYearVMT v,"
				+ " 	RoadTypeDistribution rtd"
				+ " WHERE"
				+ " 	rsrt.roadTypeID = rtd.roadTypeID AND"
				+ " 	v.yearID = tf.yearID AND"
				+ " 	tf.sourceTypeID = v.sourceTypeID AND"
				+ " 	rtd.sourceTypeID = tf.sourceTypeID AND"
				+ " 	v.yearID = " + yearID;
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Tag-6: Temporarlly Allocate VMT to Hours
	 * @param analysisYear The year we are doing the analysis for
	 * @throws SQLException If VMT cannot be allocated to hours.
	**/
	void calculateVMTByRoadwayHour(int analysisYear) throws SQLException {
		WeeksInMonthHelper weekHelper = new WeeksInMonthHelper();
		String weeksPerMonthClause =
				weekHelper.getWeeksPerMonthSQLClause("avar.yearID","avar.monthID");
		String sql = "";

		sql = "TRUNCATE VMTByAgeRoadwayHour";
		SQLRunner.executeSQL(db,sql);
		
		sql = "TRUNCATE vmtByMYRoadHourFraction";
		SQLRunner.executeSQL(db,sql);

		sql = "DROP TABLE IF EXISTS AvarMonth ";
		SQLRunner.executeSQL(db,sql);

		sql = "DROP TABLE IF EXISTS AvarMonthDay ";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 170
		 * @algorithm Append monthVMTFraction to AnnualVMTByAgeRoadway.
		 * @output AvarMonth
		 * @input AnnualVMTByAgeRoadway
		 * @input MonthVMTFraction
		**/
		sql = "CREATE TABLE AvarMonth " +
				"SELECT avar.*, monthID, monthVMTFraction " +
				"FROM AnnualVMTByAgeRoadway as avar " +
				"INNER JOIN MonthVMTFraction AS mvf USING (sourceTypeID)";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE INDEX index1 ON AvarMonth (sourceTypeID, monthID, roadTypeID) ";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 170
		 * @algorithm Append dayVMTFraction to AvarMonth.
		 * @output AvarMonthDay
		 * @input AvarMonth
		 * @input DayVMTFraction
		**/
		sql = "CREATE TABLE AvarMonthDay " +
				"SELECT avarm.*, dayID, dayVMTFraction, monthVMTFraction*dayVMTFraction as monthDayFraction " +
				"FROM AvarMonth AS avarm INNER JOIN DayVMTFraction AS dvf " +
				"USING (sourceTypeID, monthID, roadTypeID) ";
		SQLRunner.executeSQL(db,sql);

		sql = "CREATE INDEX index1 ON AvarMonthDay(sourceTypeID, roadTypeID, dayID) ";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 170
		 * @algorithm Hourly VMT = Annual VMT * monthVMTFraction * dayVMTFraction * hourVMTFraction / weeksPerMonth.
		 * @output VMTByAgeRoadwayHour
		 * @input AvarMonthDay
		 * @input HourVMTFraction
		 * @input HourDay
		 * @condition Annual VMT provided, either by HPMSVType or sourceTypeID
		**/
		sql = "INSERT INTO VMTByAgeRoadwayHour (yearID, roadTypeID, sourceTypeID, " +
					"ageID, monthID, dayID, hourID, VMT, hourDayID) " +
				"SELECT avar.yearID, avar.roadTypeID, avar.sourceTypeID, " +
					"avar.ageID, avar.monthID, avar.dayID, hvf.hourID, " +
//					"avar.VMT*avar.monthVMTFraction*avar.dayVMTFraction*hvf.hourVMTFraction " +
					"avar.VMT*avar.monthDayFraction*hvf.hourVMTFraction " +
					" / " + weeksPerMonthClause + ", "+
					"hd.hourDayID " +
				"FROM AvarMonthDay AS avar INNER JOIN HourVMTFraction AS hvf " +
				"USING(sourceTypeID, roadTypeID, dayID) " +
				"INNER JOIN HourDay hd ON (hd.hourID=hvf.hourID and hd.dayID=avar.dayID)";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 170
		 * @algorithm Hourly VMT = Daily VMT * hourVMTFraction * roadTypeVMTFraction * TravelFraction * NumberOfRealDays.
		 * @output VMTByAgeRoadwayHour
		 * @input SourceTypeDayVMT
		 * @input RoadTypeDistribution
		 * @input HourDay
		 * @input HourVMTFraction
		 * @input TravelFraction
		 * @input DayOfAnyWeek
		 * @condition Daily VMT provided by sourceTypeID
		**/
		sql = "insert ignore into VMTByAgeRoadwayHour (yearID, roadTypeID, sourceTypeID,"
				+ " 	ageID, monthID, dayID, hourID, VMT, hourDayID)"
				+ " select vmt.yearID, rtd.roadTypeID, vmt.sourceTypeID,"
				+ " 	tf.ageID, vmt.monthID, vmt.dayID, h.hourID,"
				+ " 	vmt.VMT*h.hourVMTFraction*rtd.roadTypeVMTFraction*tf.fraction*dow.noOfRealDays as VMT,"
				+ " 	hd.hourDayID"
				+ " from SourceTypeDayVMT vmt"
				+ " inner join RoadTypeDistribution rtd on (rtd.sourceTypeID=vmt.sourceTypeID)"
				+ " inner join HourDay hd on (hd.dayID=vmt.dayID)"
				+ " inner join hourVMTFraction h on (h.hourID=hd.hourID and h.roadTypeID=rtd.roadTypeID and h.sourceTypeID=rtd.sourceTypeID)"
				+ " inner join TravelFraction tf on (tf.yearID=vmt.yearID and tf.sourceTypeID=rtd.sourceTypeID)"
				+ " inner join DayOfAnyWeek dow on (dow.dayID=vmt.dayID)"
				+ " where vmt.yearID=" + analysisYear;
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 170
		 * @algorithm Hourly VMT = Daily VMT * hourVMTFraction * roadTypeVMTFraction * TravelFraction * NumberOfRealDays.
		 * @output VMTByAgeRoadwayHour
		 * @input HPMSVTypeDay
		 * @input SourceUseType
		 * @input RoadTypeDistribution
		 * @input HourDay
		 * @input HourVMTFraction
		 * @input TravelFraction
		 * @input DayOfAnyWeek
		 * @condition Daily VMT provided by HPMSVType
		**/
		sql = "insert ignore into VMTByAgeRoadwayHour (yearID, roadTypeID, sourceTypeID,"
				+ " 	ageID, monthID, dayID, hourID, VMT, hourDayID)"
				+ " select vmt.yearID, rtd.roadTypeID, sut.sourceTypeID,"
				+ " 	tf.ageID, vmt.monthID, vmt.dayID, h.hourID,"
				+ " 	vmt.VMT*h.hourVMTFraction*rtd.roadTypeVMTFraction*tf.fraction*dow.noOfRealDays as VMT,"
				+ " 	hd.hourDayID"
				+ " from HPMSVTypeDay vmt"
				+ " inner join SourceUseType sut on (sut.HPMSVTypeID=vmt.HPMSVTypeID)"
				+ " inner join RoadTypeDistribution rtd on (rtd.sourceTypeID=sut.sourceTypeID)"
				+ " inner join HourDay hd on (hd.dayID=vmt.dayID)"
				+ " inner join hourVMTFraction h on (h.hourID=hd.hourID and h.roadTypeID=rtd.roadTypeID and h.sourceTypeID=rtd.sourceTypeID)"
				+ " inner join TravelFraction tf on (tf.yearID=vmt.yearID and tf.sourceTypeID=rtd.sourceTypeID)"
				+ " inner join DayOfAnyWeek dow on (dow.dayID=vmt.dayID)"
				+ " where vmt.yearID=" + analysisYear;
		SQLRunner.executeSQL(db,sql);

		sql = "DROP TABLE IF EXISTS AvarMonth ";
		SQLRunner.executeSQL(db,sql);

		sql = "DROP TABLE IF EXISTS AvarMonthDay ";
		SQLRunner.executeSQL(db,sql);

		sql = "drop table if exists vmtByMYRoadHourSummary";
		SQLRunner.executeSQL(db,sql);

		sql = "create table vmtByMYRoadHourSummary ("
				+ " 	yearID smallint not null,"
				+ " 	roadTypeID smallint not null,"
				+ " 	sourceTypeID smallint not null,"
				+ " 	monthID smallint not null,"
				+ " 	dayID smallint not null,"
				+ " 	hourID smallint not null,"
				+ " 	hourDayID smallint not null,"
				+ " 	totalVMT double,"
				+ " 	unique key (yearID, roadTypeID, sourceTypeID, monthID, hourID, dayID),"
				+ " 	unique key (yearID, roadTypeID, sourceTypeID, monthID, hourDayID)"
				+ " )";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 170
		 * @algorithm totalVMT = sum(VMT).
		 * @output vmtByMYRoadHourSummary
		 * @input vmtByAgeRoadwayHour
		**/
		sql = "insert into vmtByMYRoadHourSummary (yearID, roadTypeID, sourceTypeID,"
				+ " 	monthID, hourID, dayID, hourDayID, totalVMT)"
				+ " select yearID, roadTypeID, sourceTypeID,"
				+ " 	monthID, hourID, dayID, hourDayID,"
				+ " 	sum(VMT) as totalVMT"
				+ " from vmtByAgeRoadwayHour"
				+ " where yearID = " + analysisYear
				+ " group by yearID, roadTypeID, sourceTypeID, monthID, hourID, dayID"
				+ " having sum(VMT) > 0";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 170
		 * @algorithm Find VMT fraction by model year.
		 * vmtFraction = VMT/totalVMT.
		 * @output vmtByMYRoadHourFraction
		 * @input vmtByMYRoadHourSummary
		 * @input vmtByAgeRoadwayHour
		**/
		sql = "insert into vmtByMYRoadHourFraction (yearID, roadTypeID, sourceTypeID,"
				+ " 	modelYearID, monthID, hourID, dayID, hourDayID, vmtFraction)"
				+ " select s.yearID, s.roadTypeID, s.sourceTypeID,"
				+ " 	(v.yearID-v.ageID) as modelYearID, s.monthID, s.hourID, s.dayID, s.hourDayID, "
				+ " 	(VMT/totalVMT) as vmtFraction"
				+ " from vmtByMYRoadHourSummary s"
				+ " inner join vmtByAgeRoadwayHour v using ("
				+ " 	yearID, roadTypeID, sourceTypeID, monthID, dayID, hourID)";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Tag-7: Convert VMT to Total Activity Basis
	 * Calculate SHO, Source Hours Parked, and hotelling. Also calculates the startsPerVehicle table,
	 * which is not the actual starts table that gets used (starts are now calculated in AdjustStarts,
	 * using the startsPerDayPerVehicle table or the user input tables). However, it appears other places
     * might still use startsPerVehicle (?), so this will remain for now.	 
	 * @throws SQLException If VMT cannot be converted to Total Activity Basis.
	**/
	void convertVMTToTotalActivityBasis(int zoneID) throws SQLException {
		long start = 0;

		String sql = "";

		start = System.currentTimeMillis();
		sql = "DROP TABLE IF EXISTS SourceTypeHour2";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 180
		 * @algorithm Remove unwanted days from SourceTypeHour.
		 * @output SourceTypeHour
		 * @input SourceTypeHour
		 * @input HourDay
		 * @input RunSpecDay
		**/
		sql = "delete from sourceTypeHour"
				+ " where hourDayID not in (select hourDayID from hourDay inner join runspecDay using (dayID))";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 180
		 * @algorithm Append dayID and hourID to SourceTypeHour.
		 * @output SourceTypeHour2
		 * @input SourceTypeHour
		 * @input HourDay
		**/
		sql= "CREATE TABLE SourceTypeHour2 " +
				"SELECT sth.sourceTypeID, hd.dayID, hd.hourID, sth.idleSHOFactor, sth.hotellingDist " +
				"FROM SourceTypeHour AS sth INNER JOIN HourDay AS hd USING (hourDayID)";
		SQLRunner.executeSQL(db,sql);

		sql = "alter table sourceTypeHour2 add key (sourceTypeID, dayID)";
		SQLRunner.executeSQL(db,sql);

		Logger.log(LogMessageCategory.INFO,"TAG SourceTypeHour2 ms=" + (System.currentTimeMillis()-start));

		sql = "TRUNCATE AverageSpeed";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 180
		 * @algorithm averageSpeed = sum(avgBinSpeed * avgSpeedFraction).
		 * @output AverageSpeed
		 * @input RoadType
		 * @input RunSpecSourceType
		 * @input RunSpecDay
		 * @input HourOfAnyDay
		 * @input AvgSpeedBin
		 * @input AvgSpeedDistribution
		 * @input HourDay
		**/
		start = System.currentTimeMillis();
		sql = "INSERT INTO AverageSpeed ("+
					"roadTypeID,"+
					"sourceTypeID,"+
					"dayID,"+
					"hourID,"+
					"averageSpeed) "+
				"SELECT "+
					"asd.roadTypeID,"+
					"asd.sourceTypeID,"+
					"hd.dayID,"+
					"hd.hourID,"+
					"sum(asb.avgBinSpeed*asd.avgSpeedFraction) "+
				"FROM "+
					"RoadType rsrt,"+ // was RunSpecRoadType
					"RunSpecSourceType rsst,"+
					"RunSpecDay rsd,"+
					"HourOfAnyDay rsh,"+ // "RunSpecHour rsh,"+
					"AvgSpeedBin asb,"+
					"AvgSpeedDistribution asd,"+
					"HourDay hd "+
				"WHERE "+
					"rsrt.roadTypeID = asd.roadTypeID AND "+
					"rsst.sourceTypeID = asd.sourceTypeID AND "+
					"hd.dayID = rsd.dayID AND "+
					"hd.hourID = rsh.hourID AND "+
					"asb.avgSpeedBinID = asd.avgSpeedBinID AND "+
					"asd.hourDayID = hd.hourDayID "+
				"GROUP BY "+
					"asd.roadTypeID,"+
					"asd.sourceTypeID,"+
					"hd.dayID,"+
					"hd.hourID";
		SQLRunner.executeSQL(db,sql);

		Logger.log(LogMessageCategory.INFO,"TAG AverageSpeed ms=" + (System.currentTimeMillis()-start));

		/**
		 * @step 180
		 * @algorithm SHO = VMT/averageSpeed where averageSpeed > 0, 0 otherwise.
		 * @output SHOByAgeRoadwayHour
		 * @input VMTByAgeRoadwayHour
		 * @input AverageSpeed
		**/
		start = System.currentTimeMillis();
		DatabaseUtilities.insertSelect(false,db,"SHOByAgeRoadwayHour",
					"yearID,"+
					"roadTypeID,"+
					"sourceTypeID,"+
					"ageID,"+
					"monthID,"+
					"dayID,"+
					"hourID,"+
					"hourDayID,"+
					"SHO,"+
					"VMT",
				"SELECT "+
					"varh.yearID,"+
					"varh.roadTypeID,"+
					"varh.sourceTypeID,"+
					"varh.ageID,"+
					"varh.monthID,"+
					"varh.dayID,"+
					"varh.hourID,"+
					"varh.hourDayID,"+
					"IF(asp.averageSpeed<>0,"+
					"COALESCE(varh.VMT/asp.averageSpeed,0.0),0.0),"+
					"varh.VMT "+
				"FROM VMTByAgeRoadwayHour varh "+
				"LEFT JOIN AverageSpeed asp ON ("+
					"asp.roadTypeID = varh.roadTypeID AND "+
					"asp.sourceTypeID = varh.sourceTypeID AND "+
					"asp.dayID = varh.dayID AND "+
					"asp.hourID = varh.hourID)");

		// Not needed, is only insert, so indexes will be all setup
		//SQLRunner.executeSQL(db,"analyze table SHOByAgeRoadwayHour");
		Logger.log(LogMessageCategory.INFO,"TAG SHOByAgeRoadwayHour ms=" + (System.currentTimeMillis()-start));

		// Calculate idle hours
		start = System.currentTimeMillis();

		/**
		 * @step 180
		 * @algorithm Find total VMT by day on Rural Restricted Access roads (roadTypeID=2)
		 * and Urban Restricted Access roads (roadTypeID=4) for Combination Long Haul Trucks (sourceTypeID=62).
		 * Daily VMT = sum(hourly VMT).
		 * hotellingHours = 0.
		 * @output VMTByAgeRoadwayDay
		 * @input VMTByAgeRoadwayHour
		**/
		// clear VMTByAgeRoadwayDay first
		sql = "TRUNCATE TABLE VMTByAgeRoadwayDay";
		SQLRunner.executeSQL(db,sql);
		sql = "insert ignore into VMTByAgeRoadwayDay ("
				+ " 	yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, VMT, hotellingHours)"
				+ " select yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, sum(VMT), 0 as hotellingHours"
				+ " from VMTByAgeRoadwayHour"
				+ " where sourceTypeID=62"
				+ " group by yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID"
				+ " order by null";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 180
		 * @algorithm hotellingHours = Daily VMT * hotellingRate.
		 * @output VMTByAgeRoadwayDay
		 * @input hotellingCalendarYear
		 * VMT = VMT * shoallocfactor from zoneroadtype table (join)
		**/
		sql = "update VMTByAgeRoadwayDay, hotellingCalendarYear, ZoneRoadType"
				+ " set hotellingHours = VMT * ZoneRoadType.SHOAllocFactor * hotellingRate"
				+ " where VMTByAgeRoadwayDay.yearID = hotellingCalendarYear.yearID"
				+ " and ZoneRoadType.zoneID = " + zoneID
				+ " and VMTByAgeRoadwayDay.roadTypeID = ZoneRoadType.roadTypeID";
		SQLRunner.executeSQL(db,sql);
		
		/*
		The IdleHoursByAgeHour table in the next step stores hotelling hours based on data in VMTByAgeRoadwayDay.
		VMTByAgeRoadwayDay has activity for all road types, but we only want road types 2 and 4 in most cases,
		and therefore we typically want to delete road types 3 and 5. The only exception to this is where a 
		county-scale user has provided hotelling activity in hotellinghoursperday, but they have 0 VMT on road 
		types 2 and 4 -- in this case, we want to keep VMT from all road types in this table so we calculate
		non-zero default hotelling activity that can later be adjusted to reflect user input via AdjustHotelling.sql
		*/
		sql = "DELETE FROM VMTByAgeRoadwayDay " +
				// delete road types 3 and 5 any time there is activity on road types 2 and 4 (because the algorithm works as expected for both default scale and county scale with or without user input)
				" WHERE (roadTypeID not in (2,4) and (select sum(hotellinghours) from VMTByAgeRoadwayDay WHERE roadTypeID in (2,4)) > 0)" +
				// if we get here, there is no activity on road types 2 and 4. delete road types 3 and 5 (which will result in no hotelling activity) only if no hotelling input has been provided
				" or (roadTypeID not in (2,4) and (SELECT count(*) from hotellinghoursperday) = 0)";
		SQLRunner.executeSQL(db,sql);

		/**
		 * Note that this is called "idleHours" and stored in "IdleHoursByAgeHour", but
		 * this is actually hotelling activity. This is *not* related to ONI.
		 * @step 180
		 * @algorithm idleHours = hotellingHours * hotellingDist.
		 * @output IdleHoursByAgeHour
		 * @input VMTByAgeRoadwayDay
		 * @input SourceTypeHour2
		**/
		// clear IdleHoursByAgeHour first
		sql = "TRUNCATE TABLE IdleHoursByAgeHour";
		SQLRunner.executeSQL(db,sql);
		sql = "insert ignore into IdleHoursByAgeHour ("
				+ " 	yearID,sourceTypeID,ageID,"
				+ " 	monthID,dayID,hourID,idleHours)"
				+ " select v.yearID,v.sourceTypeID,v.ageID,"
				+ " 	v.monthID,sth.dayID,sth.hourID,sum(v.hotellingHours*sth.hotellingDist)"
				+ " from VMTByAgeRoadwayDay as v"
				+ " inner join sourceTypeHour2 as sth using (sourceTypeID, dayID)"
				+ " group by v.yearID,v.sourceTypeID,v.ageID,"
				+ " 	v.monthID,sth.dayID,sth.hourID";
		SQLRunner.executeSQL(db,sql);

		SQLRunner.executeSQL(db,"analyze table IdleHoursByAgeHour");
		Logger.log(LogMessageCategory.INFO,"TAG IdleHoursByAgeHour ms=" + (System.currentTimeMillis()-start));

		sql = "DROP TABLE IF EXISTS SourceTypeHour2";
		SQLRunner.executeSQL(db,sql);

		// Calculate startsPerVehicle
		if(initialLoop) {
			start = System.currentTimeMillis();
			sql = "DROP TABLE IF EXISTS StartsPerSampleVehicle";
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 180
			 * @algorithm Find the number of starts for each sample vehicle.
			 * starts = count(trips) * noOfRealDays.
			 * @output StartsPerSampleVehicle
			 * @input SampleVehicleDay
			 * @input SampleVehicleTrip
			 * @input HourDay
			 * @input DayOfAnyWeek
			 * @condition Ignore marker trips
			**/
			sql = "CREATE TABLE StartsPerSampleVehicle " +
					"SELECT sv.sourceTypeID, hd.hourDayID, " +
					"(COUNT(*)*noOfRealDays) AS starts, hd.dayID " +
					"FROM SampleVehicleDay sv " +
					"INNER JOIN SampleVehicleTrip svt USING (vehID) " +
					"INNER JOIN HourDay hd ON (hd.dayID=svt.dayID and hd.hourID=svt.hourID) " +
					"INNER JOIN DayOfAnyWeek d ON (d.dayID=hd.dayID) " +
					"WHERE svt.keyOnTime IS NOT NULL " + // Ignore Marker trips
					"GROUP BY sv.sourceTypeID, hd.hourDayID " +
					"ORDER BY NULL";
			SQLRunner.executeSQL(db,sql);

			Logger.log(LogMessageCategory.INFO,"TAG StartsPerSampleVehicle ms=" + (System.currentTimeMillis()-start));

			start = System.currentTimeMillis();
			sql = "DROP TABLE IF EXISTS SourceTypesInStartsPerVehicle";
			SQLRunner.executeSQL(db,sql);

			sql = "CREATE TABLE SourceTypesInStartsPerVehicle SELECT sourceTypeID FROM " +
					"StartsPerVehicle GROUP BY sourceTypeID ORDER BY NULL";
			SQLRunner.executeSQL(db,sql);

			Logger.log(LogMessageCategory.INFO,"TAG SourceTypesInStartsPerVehicle ms=" + (System.currentTimeMillis()-start));

			/**
			 * @step 180
			 * @algorithm startsPerVehicle = starts / count(sample vehicles).
			 * @output StartsPerVehicle
			 * @input SampleVehicleDay
			 * @input StartsPerSampleVehicle
			**/
			start = System.currentTimeMillis();
			sql = "INSERT INTO StartsPerVehicle(sourceTypeID, hourDayID, startsPerVehicle, " +
					"startsPerVehicleCV) " +
					"SELECT sv.sourceTypeID, ssv.hourDayID, " +
					"starts/COUNT(vehID) AS startsPerVehicle,0 " +
					"FROM SampleVehicleDay sv " +
					"INNER JOIN StartsPerSampleVehicle ssv ON (ssv.sourceTypeID = " +
					"sv.sourceTypeID AND ssv.dayID=sv.dayID) " +
					"LEFT JOIN SourceTypesInStartsPerVehicle stsv ON " +
					"(stsv.sourceTypeID =  sv.sourceTypeID) " +
					"WHERE stsv.sourceTypeID IS NULL " +
					"GROUP BY sv.SourceTypeID, ssv.hourDayID " +
					"ORDER BY NULL";
			int rows = SQLRunner.executeSQL(db,sql);
			Logger.log(LogMessageCategory.INFO,"TAG StartsPerVehicle ms=" + (System.currentTimeMillis()-start));

			sql = "DROP TABLE IF EXISTS StartsPerSampleVehicle";
			SQLRunner.executeSQL(db,sql);
		}

		start = System.currentTimeMillis();
		sql = "DROP TABLE IF EXISTS StartsByAgeHour";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 180
		 * @algorithm starts = population * startsPerVehicle.
		 * @output StartsByAgeHour
		 * @input SourceTypeAgePopulation
		 * @input StartsPerVehicle
		**/
		sql = "CREATE TABLE IF NOT EXISTS StartsByAgeHour " +
				"SELECT stap.sourceTypeID, yearID, " +
				"hourDayID, ageID, population*startsPerVehicle AS starts " +
				"FROM SourceTypeAgePopulation stap " +
				"INNER JOIN StartsPerVehicle USING (sourceTypeID)";
		int rows = SQLRunner.executeSQL(db,sql);

		SQLRunner.executeSQL(db,"analyze table StartsByAgeHour");

		Logger.log(LogMessageCategory.INFO,"TAG StartsByAgeHour ms=" + (System.currentTimeMillis()-start));

		start = System.currentTimeMillis();
		// Calculate source hours parked by age hour
		sql = "DROP TABLE IF EXISTS SHPByAgeHour";
		SQLRunner.executeSQL(db,sql);

		/**
		 * @step 180
		 * @algorithm SHP = (population*noOfRealDays) - SUM(sho).
		 * @output SHPByAgeHour
		 * @input ShoByAgeRoadwayHour
		 * @input SourceTypeAgePopulation
		 * @input DayOfAnyWeek
		**/
		sql = "CREATE TABLE IF NOT EXISTS SHPByAgeHour " +
				"SELECT sarh.yearID, sarh.sourceTypeID, sarh.ageID, monthID, " +
				"sarh.dayID, hourID, (population*noOfRealDays) - SUM(sho) AS SHP " +
				"FROM ShoByAgeRoadwayHour sarh " +
				"INNER JOIN SourceTypeAgePopulation stap USING (yearID, sourceTypeID, " +
				"ageID) " +
				"INNER JOIN DayOfAnyWeek d ON (d.dayID=sarh.dayID) " +
				"WHERE VMT > 0 " +
				"GROUP BY sarh.yearID, sarh.sourceTypeID, sarh.ageID, monthID, " +
				"sarh.dayID, hourID " +
				"ORDER BY NULL";
		SQLRunner.executeSQL(db,sql);

		Logger.log(LogMessageCategory.INFO,"TAG SHPByAgeHour ms=" + (System.currentTimeMillis()-start));
	}

	/**
	 * Remove records from SHO, hotellingHours, and Starts based upon the
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
			/*
			sql = "DELETE FROM SHO WHERE isUserInput='N' AND linkID IN ("
					+ linksInZone.substring(1) + ")";
			SQLRunner.executeSQL(db, sql);
			clearFlags("SHO");

			sql = "DELETE FROM SourceHours WHERE isUserInput='N' "
					+ "AND linkID IN (" + linksInZone.substring(1) + ")";
			SQLRunner.executeSQL(db, sql);
			clearFlags("SourceHours");
			*/

			if(CompilationFlags.ENABLE_AUXILIARY_POWER_EXHAUST) {
				sql = "DELETE FROM hotellingHours WHERE isUserInput='N' "
						+ "AND zoneID = " + currentZoneID;
				SQLRunner.executeSQL(db, sql);
				clearFlags("hotellingHours");
			}

			sql = "DELETE FROM Starts WHERE isUserInput='N' "
					+ "AND zoneID = " + currentZoneID;
			SQLRunner.executeSQL(db, sql);
			clearFlags("Starts");
		} catch(Exception e) {
			Logger.logSqlError(e,"Could not delete Total Activity data from previous run.",sql);
		}
	}

	/**
	 * Tag-8: Allocate Total Activity Basis, Starts, SHP and Source Hours.
	 * @param inContext Current loop context being run.
	 * @throws SQLException If failed to allocate Total Activity Basis.
	**/
	void allocateTotalActivityBasis(MasterLoopContext inContext) throws SQLException {
		String sql = "";

		int analysisYear = inContext.year;
		int zoneID = inContext.iterLocation.zoneRecordID;
		int stateID = inContext.iterLocation.stateRecordID;

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
			if(currentYearForZone != analysisYear) {
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
			
			// convertVMTToTotalActivityBasis used to be part of the VMT growth that was done once per year
			// However, the hotelling part of this function changes by ZoneID, so it needs to be done once per zone (as well as per year)
			convertVMTToTotalActivityBasis(inContext.iterLocation.zoneRecordID); // steps 180-189
			
			if(needSHO && !checkAndMark("SHO",zoneID,analysisYear)) {
				if(!checkAndMark("ZoneRoadTypeLinkTemp",zoneID,0)) {
					sql = "drop table if exists ZoneRoadTypeLinkTemp";
					SQLRunner.executeSQL(db,sql);
	
					sql = "create table if not exists ZoneRoadTypeLinkTemp ("
							+ " roadTypeID smallint not null,"
							+ " linkID int(11) not null,"
							+ " SHOAllocFactor double,"
							+ " unique index XPKZoneRoadTypeLinkTemp ("
							+ "     roadTypeID, linkID))";
					SQLRunner.executeSQL(db,sql);
	
					/**
					 * @step 190
					 * @algorithm Append SHOAllocFactor to link information.
					 * @output ZoneRoadTypeLinkTemp
					 * @input ZoneRoadType
					 * @input Link
					**/
					DatabaseUtilities.insertSelect(false,db,"ZoneRoadTypeLinkTemp",
							"roadTypeID, linkID, SHOAllocFactor",
							"select zrt.roadTypeID, linkID, SHOAllocFactor"
								+ " from ZoneRoadType zrt"
								+ " inner join Link l on l.roadTypeID=zrt.roadTypeID"
								+ " where zrt.zoneID=" + zoneID
								+ " and l.zoneID=" + zoneID);
				}
				if(ExecutionRunSpec.getRunSpec().scale == ModelScale.MESOSCALE_LOOKUP) {
					/**
					 * @step 190
					 * @algorithm SHO = SHO * averageSpeed * SHOAllocFactor.
					 * @output SHO
					 * @input SHOByAgeRoadwayHour
					 * @input RunSpecHourDay
					 * @input ZoneRoadTypeLinkTemp
					 * @input LinkAverageSpeed
					 * @input AverageSpeed
					 * @condition Rates
					**/
					DatabaseUtilities.insertSelect(false,db,"SHO",
								"hourDayID,"+
								"monthID,"+
								"yearID,"+
								"ageID,"+
								"linkID,"+
								"sourceTypeID,"+
								"SHO",
							"SELECT "+
								"sarh.hourDayID,"+
								"sarh.monthID,"+
								"sarh.yearID,"+
								"sarh.ageID,"+
								"zrt.linkID,"+
								"sarh.sourceTypeID,"+
								"sarh.SHO*zrt.SHOAllocFactor "+ //EM - there used to be a COALESCE on averageSpeed, but that writes VMT not SHO
							"FROM "+
								"SHOByAgeRoadwayHour sarh "+
								"inner join RunSpecHourDay rshd on (rshd.hourDayID=sarh.hourDayID) "+
								"inner join ZoneRoadTypeLinkTemp zrt "+
									"on zrt.roadTypeID=sarh.roadTypeID "+
								"inner join LinkAverageSpeed las "+
									"on las.linkID=zrt.linkID "+
								"left join AverageSpeed asp on ("+
									"asp.roadTypeID = zrt.roadTypeID and "+
									"asp.sourceTypeID = sarh.sourceTypeID and "+
									"asp.dayID = sarh.dayID and "+
									"asp.hourID = sarh.hourID) "+
							"WHERE "+
								"sarh.yearID = " + analysisYear
							);
				} else {
					/**
					 * @step 190
					 * @algorithm SHO = SHO * SHOAllocFactor.
					 * @output SHO
					 * @input SHOByAgeRoadwayHour
					 * @input RunSpecHourDay
					 * @input ZoneRoadTypeLinkTemp
					 * @condition Inventory
					**/
					DatabaseUtilities.insertSelect(false,db,"SHO",
								"hourDayID,"+
								"monthID,"+
								"yearID,"+
								"ageID,"+
								"linkID,"+
								"sourceTypeID,"+
								"SHO",
							"SELECT "+
								"sarh.hourDayID,"+
								"sarh.monthID,"+
								"sarh.yearID,"+
								"sarh.ageID,"+
								"zrt.linkID,"+
								"sarh.sourceTypeID,"+
								"sarh.SHO*zrt.SHOAllocFactor "+
							"FROM "+
								"SHOByAgeRoadwayHour sarh "+
								"inner join RunSpecHourDay rshd on (rshd.hourDayID=sarh.hourDayID) "+
								"inner join ZoneRoadTypeLinkTemp zrt " +
								"on zrt.roadTypeID=sarh.roadTypeID "+
							"WHERE "+
								"sarh.yearID = " + analysisYear
							);
				}

				// Adjust, or create, TotalIdleFraction one time only.
				if(!checkAndMark("TotalIdleFraction",0,0)) {
					adjustTotalIdleFraction();
				}

				/**
				 * @step 190
				 * @algorithm ONI = SHO on roadtype 1=Sum(SHO[not road type 1] * 
				 *                  (totalIdlingFraction-(sum(sho*drivingIdleFraction)/sum(sho)))/
				 *                  (1-totalIdlingFraction)) 
				 * @output SHO
				 * @input SHO
				 * @input Link
				 * @input County
				 * @input State
				 * @input HourDay
				 * @input TotalIdleFraction
				 * @input DrivingIdleFraction
				**/
				
				//EM - we want different behaviour for ONI in Rates vs Inventory
				// 		In rates, we need to divide SHO by 16 becuase each road type has 16 rows for each avgSpeedBin
				//		Inventory is left how it was originally written
				if(ExecutionRunSpec.getRunSpec().scale == ModelScale.MESOSCALE_LOOKUP) {
					DatabaseUtilities.insertSelect(false,db,"SHO", // ONI=Off network idle=SHO on road type 1
								"hourDayID,"+
								"monthID,"+
								"yearID,"+
								"ageID,"+
								"linkID,"+
								"sourceTypeID,"+
								"SHO",
							"select s.hourDayID,s.monthID,s.yearID,s.ageID, lo.linkID,s.sourceTypeID, "+
							"(" + 
							"		case when totalIdleFraction <> 1 then "+
							"			greatest(sum((s.sho/16))*(totalIdleFraction-sum((s.sho/16)*drivingIdleFraction) /sum((s.sho/16)))/(1-totalIdleFraction),0) "+
							"		else 0 "+
							"		end "+
							"	) as SHO "+
							"from sho s "+
							"inner join link l on ( "+
							"	l.linkID = s.linkID "+
							"	and l.roadTypeID <> 1) "+
							"inner join link lo on ( "+
							"	l.zoneID = lo.zoneID "+
							"	and lo.roadTypeID = 1) "+
							"inner join county c on (lo.countyID = c.countyID) "+
							"inner join state st using (stateID) "+
							"inner join hourDay hd on (s.hourDayID = hd.hourDayID) "+
							"inner join totalIdleFraction tif on ( "+
							"	tif.idleRegionID = st.idleRegionID "+
							"	and tif.countyTypeID = c.countyTypeID "+
							"	and tif.sourceTypeID = s.sourceTypeID "+
							"	and tif.monthID = s.monthID "+
							"	and tif.dayID = hd.dayID "+
							"	and tif.minModelYearID <= s.yearID - s.ageID "+
							"	and tif.maxModelYearID >= s.yearID - s.ageID) "+
							"inner join drivingIdleFraction dif on ( "+
							"	dif.hourDayID = s.hourDayID "+
							"	and dif.yearID = s.yearID "+
							"	and dif.roadTypeID = l.roadTypeID "+
							"	and dif.sourceTypeID = s.sourceTypeID) "+
							"where s.yearID = " + analysisYear + " "+
							"and lo.zoneID = " + zoneID + " "+
							"group by s.hourDayID,s.monthID,s.yearID,s.ageID, lo.linkID,s.sourceTypeID"
							);
				} else {
					DatabaseUtilities.insertSelect(false,db,"SHO", // ONI=Off network idle=SHO on road type 1
								"hourDayID,"+
								"monthID,"+
								"yearID,"+
								"ageID,"+
								"linkID,"+
								"sourceTypeID,"+
								"SHO",
							"select s.hourDayID,s.monthID,s.yearID,s.ageID, lo.linkID,s.sourceTypeID, "+
							"(" + 
							"		case when totalIdleFraction <> 1 then "+
							"			greatest(sum((s.sho))*(totalIdleFraction-sum((s.sho)*drivingIdleFraction) /sum((s.sho)))/(1-totalIdleFraction),0) "+
							"		else 0 "+
							"		end "+
							"	) as SHO "+
							"from sho s "+
							"inner join link l on ( "+
							"	l.linkID = s.linkID "+
							"	and l.roadTypeID <> 1) "+
							"inner join link lo on ( "+
							"	l.zoneID = lo.zoneID "+
							"	and lo.roadTypeID = 1) "+
							"inner join county c on (lo.countyID = c.countyID) "+
							"inner join state st using (stateID) "+
							"inner join hourDay hd on (s.hourDayID = hd.hourDayID) "+
							"inner join totalIdleFraction tif on ( "+
							"	tif.idleRegionID = st.idleRegionID "+
							"	and tif.countyTypeID = c.countyTypeID "+
							"	and tif.sourceTypeID = s.sourceTypeID "+
							"	and tif.monthID = s.monthID "+
							"	and tif.dayID = hd.dayID "+
							"	and tif.minModelYearID <= s.yearID - s.ageID "+
							"	and tif.maxModelYearID >= s.yearID - s.ageID) "+
							"inner join drivingIdleFraction dif on ( "+
							"	dif.hourDayID = s.hourDayID "+
							"	and dif.yearID = s.yearID "+
							"	and dif.roadTypeID = l.roadTypeID "+
							"	and dif.sourceTypeID = s.sourceTypeID) "+
							"where s.yearID = " + analysisYear + " "+
							"and lo.zoneID = " + zoneID + " "+
							"group by s.hourDayID,s.monthID,s.yearID,s.ageID, lo.linkID,s.sourceTypeID"
							);
				}
				
				
			}
			if(startExhaustProcess!=null
					&& inContext.iterProcess.compareTo(startExhaustProcess)==0
					&& !checkAndMark("Starts",zoneID,analysisYear)) {
				/**
				 * @step 190
				 * @algorithm starts = starts * startAllocFactor.
				 * @output Starts
				 * @input StartsByAgeHour
				 * @input RunSpecMonth
				 * @condition Inventory
				**/
				// Starts = StartsPerDay * monthAdjustment * ageFraction * allocationFraction						
//				DatabaseUtilities.insertSelect(false,db,"Starts",
//						"hourDayID, monthID, yearID, ageID, "
//						+ "zoneID, sourceTypeID, starts, startsCV, isUserInput",
//						"SELECT sah.hourDayID, rsm.monthID, sah.yearID, sah.ageID, "
//						+ "z.zoneID, sah.sourceTypeID, sah.starts * z.startAllocFactor, "
//						+ "z.zoneID, sah.sourceTypeID, sah.starts * 1, "
//						+ " 0, 'N' "
//						+ "FROM StartsByAgeHour sah INNER JOIN Zone z "
//						+ "CROSS JOIN RunSpecMonth rsm "
//						+ "WHERE sah.yearID = " + analysisYear + " AND z.zoneID = " + zoneID
//						);
				adjustStarts(zoneID,analysisYear);
				// Remove Start entries that are for hours outside of the user's selections.
				// Filter to analysisYear and zoneID.
				// Don't do this before adjusting the starts though as StartsPerDay requires
				// a full 24-hour distribution.
				sql = "delete from Starts"
						+ " where hourDayID not in (select hourDayID from runSpecHourDay)"
						+ " and zoneID=" + zoneID
						+ " and yearID=" + analysisYear;
//				SQLRunner.executeSQL(db,sql);
			} else if((extendedIdleProcess!=null && inContext.iterProcess.compareTo(extendedIdleProcess)==0)
					|| (CompilationFlags.ENABLE_AUXILIARY_POWER_EXHAUST && auxiliaryPowerProcess!=null && inContext.iterProcess.compareTo(auxiliaryPowerProcess)==0)) {
				if(!checkAndMark("HotellingHours",zoneID,analysisYear)) {
					int hotellingActivityZoneID = findHotellingActivityDistributionZoneIDToUse(db,stateID,zoneID);
					/*
					 *The TotalActivityGenerator class performs inserts into the HotellingHours 
					 *table within its allocateTotalActivityBasis() function. 
					 *These inserts should be filtered to just sourceTypeID 62 as only that type of 
					 *vehicle idles overnight.
					 */

					/**
					 * @step 190
					 * @algorithm hotellingHours = idleHours * SHOAllocFactor
					 * @output hotellingHours
					 * @input IdleHoursByAgeHour
					 * @input runSpecHourDay
					 * @input ZoneRoadType
					 * @input HourDay
					 * take out the shoallocfactor
					**/
					DatabaseUtilities.insertSelect(false,db,"hotellingHours",
							"hourDayID,"+
							"monthID,"+
							"yearID,"+
							"ageID,"+
							"zoneID,"+
							"sourceTypeID,"+
							"fuelTypeID,"+
							"hotellingHours",
						"SELECT "+
							"hd.hourDayID,"+
							"ihah.monthID,"+
							"ihah.yearID,"+
							"ihah.ageID,"+
							"z.zoneID,"+
							"ihah.sourceTypeID,"+
							"svp.fuelTypeID,"+
							"sum(ihah.idleHours*svp.stmyFraction) "+ // This must be total hotelling hours, including extended idle
						"FROM "+
							"IdleHoursByAgeHour ihah,"+
							"Zone z,"+
							"HourDay hd, "+
							"samplevehiclepopulation svp "+
						"WHERE "+
							"hd.hourID = ihah.hourID AND "+
							"hd.dayID = ihah.dayID AND "+
							"ihah.yearID = " + analysisYear + " AND "+
							"z.zoneID = " + zoneID  + " AND "+
							"ihah.sourceTypeID = 62 AND "+
							"svp.sourceTypeID = 62 AND "+
							"svp.modelYearID = ihah.yearID - ihah.ageID " +
						"GROUP BY "+
							"hd.hourDayID,"+
							"ihah.monthID,"+
							"ihah.yearID,"+
							"ihah.ageID,"+
							"z.zoneID,"+
							"ihah.sourceTypeID,"+
							"svp.fuelTypeID"
						);
							
					adjustHotelling(zoneID,analysisYear,hotellingActivityZoneID);
					// Remove HotellingHours entries that are for hours outside of the user's selections.
					// Filter to analysisYear and zoneID.
					// Don't do this before adjusting the hours though as HotellingHoursPerDay requires
					// a full 24-hour distribution.
					sql = "delete from HotellingHours"
							+ " where hourDayID not in (select hourDayID from runSpecHourDay)"
							+ " and zoneID=" + zoneID
							+ " and yearID=" + analysisYear;
					SQLRunner.executeSQL(db,sql);
				}
			}
		}

		// Allocate SHP and SHO to SourceHours
		if(makeSH && !checkAndMark("SourceHours",currentLinkID,analysisYear)) {
			if(inContext.iterLocation.roadTypeRecordID==1) {
				if(newYearForZone) {
					/**
					 * @step 190
					 * @algorithm SHP = SHP * SHPAllocFactor.
					 * @output SHP
					 * @input SHPByAgeHour
					 * @input RunSpecHourDay
					 * @input Zone
					**/
					DatabaseUtilities.insertSelect(false,db,"SHP",
							"hourDayID, monthID, yearID, ageID, zoneID, " +
							"sourceTypeID, SHP ",
							"SELECT hd.hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID, " +
							"SHP * SHPAllocFactor " +
							"FROM SHPByAgeHour sah INNER JOIN HourDay hd USING (hourID, dayID) " +
							"INNER JOIN RunSpecHourDay rshd ON (rshd.hourDayID=hd.hourDayID) "+
							"INNER JOIN Zone z WHERE sah.yearID = " + analysisYear +
							" AND z.zoneID = " + zoneID
							);
				}

				/**
				 * @step 190
				 * @algorithm sourceHours = SHP.
				 * @output SourceHours
				 * @input SHP
				 * @input Link
				**/
				DatabaseUtilities.insertSelect(false,db,"SourceHours",
						"hourDayID, monthID, yearID, ageID, linkID, " +
						"sourceTypeID, sourceHours, sourceHoursCV, isUserInput ",
						"SELECT hourDayID, " +
						"monthID, yearID, ageID, linkID, sourceTypeID, SHP, 0, 'N' " +
						"FROM SHP INNER JOIN Link ON (Link.zoneID = SHP.zoneID) " +
						"WHERE roadTypeID = 1 AND yearID = " + analysisYear +
						" AND linkID = " + currentLinkID
						);
			} else {
				/**
				 * @step 190
				 * @algorithm sourceHours = SHO.
				 * @output SourceHours
				 * @input SHO
				**/
				DatabaseUtilities.insertSelect(false,db,"SourceHours",
						"hourDayID, monthID, yearID, ageID, linkID, " +
						"sourceTypeID, sourceHours, sourceHoursCV, isUserInput",
						"SELECT hourDayID, " +
						"monthID, yearID, ageID, linkID, sourceTypeID, SHO, SHOCV, 'N' " +
						"FROM SHO sho WHERE sho.yearID = " + analysisYear +
						" AND linkID = " + currentLinkID
						);
			}
		}
	}

	/**
	 * Tag-9: Calculate distance
	 * @throws SQLException If failed to calculate distance.
	**/
	void calculateDistance(MasterLoopContext inContext) throws SQLException {
		// System.out.println("TAG Calc Distance called, process = " + inContext.iterProcess.processName);
		// If the user did NOT choose distance output, then return now.
		if(!ExecutionRunSpec.theExecutionRunSpec.getOutputVMTData()) {
			return;
		}
		// Only calculate distance if process is exhaust running
		// We could also insure roadtype not equal off network but this would
		//     be less general.
		if(inContext.iterProcess.processName.equals(runningExhaustProcess.processName)) {
			String sql = " ";

			if(ExecutionRunSpec.getRunSpec().scale == ModelScale.MESOSCALE_LOOKUP) {
				/**
				 * @step 200
				 * @algorithm distance = SHO * averageSpeed.
				 * @output SHO
				 * @input LinkAverageSpeed
				 * @condition Rates
				**/
				sql = "update SHO, LinkAverageSpeed"
						+ " set SHO.distance=SHO.SHO*averageSpeed"
						+ " where LinkAverageSpeed.linkID=SHO.linkID"
						+ " and SHO.distance is null";
				SQLRunner.executeSQL(db,sql);
				return;
			}

			sql = "DROP TABLE IF EXISTS SHOTemp ";
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 200
			 * @algorithm Append link and SHO information.
			 * @output SHOTemp
			 * @input SHO
			 * @input Link
			 * @condition Inventory
			**/
			sql = "CREATE TABLE SHOTemp " +
					"SELECT hourDayID, monthID, yearID, ageID, sho.linkID, " +
					"sourceTypeID, roadTypeID, sho.shoCV, sho.sho " +
					"FROM SHO AS sho INNER JOIN Link USING (linkID) ";
			SQLRunner.executeSQL(db,sql);

			sql = "CREATE INDEX index1 ON SHOTemp (hourDayID, sourceTypeID, roadTypeID) ";
			SQLRunner.executeSQL(db,sql);

			sql = "DROP TABLE IF EXISTS AverageSpeedTemp ";
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 200
			 * @algorithm Append AverageSpeed and HourDay information.
			 * @output AverageSpeedTemp
			 * @input AverageSpeed
			 * @input HourDay
			 * @condition Inventory
			**/
			sql = "CREATE TABLE AverageSpeedTemp " +
					"SELECT hourDayID, sourceTypeID, roadTypeID, averageSpeed " +
					"FROM AverageSpeed INNER JOIN HourDay  USING(dayID, hourID) ";
			SQLRunner.executeSQL(db,sql);

			sql = "CREATE INDEX index1 " +
					"ON AverageSpeedTemp(hourDayID, sourceTypeID, roadTypeID) ";
			SQLRunner.executeSQL(db,sql);

			sql = "CREATE TABLE IF NOT EXISTS SHO2 " +
					"(hourDayID SMALLINT, monthID SMALLINT, yearID SMALLINT, " +
					"ageID SMALLINT,linkID INTEGER, sourceTypeID SMALLINT, " +
					"SHO FLOAT, SHOCV FLOAT, distance FLOAT) ";
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 200
			 * @algorithm distance = SHO * averageSpeed.
			 * @output SHO2
			 * @input SHOTemp
			 * @input AverageSpeedTemp
			 * @condition Inventory
			**/
			sql = "INSERT INTO SHO2 " +
					"SELECT shot.hourDayID, shot.monthID, shot.yearID, shot.ageID, " +
					"shot.linkID, shot.sourceTypeID, shot.SHO, shot.SHOCV, " +
					"(shot.sho * avsp.averageSpeed) " +
					"FROM SHOTEMP AS shot INNER JOIN AverageSpeedTemp AS avsp " +
					"USING(hourDayID, sourceTypeID, roadTypeID) ";
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 200
			 * @algorithm Remove all items from the SHO table.
			 * @output SHO
			 * @condition Inventory
			**/
			sql = "TRUNCATE SHO";
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 200
			 * @algorithm Copy the SHO2 information, which now includes distance, to the SHO table.
			 * @output SHO
			 * @input SHO2
			 * @condition Inventory
			**/
			sql = "INSERT INTO SHO (hourDayID, monthID, yearID, ageID,"+
					"linkID, sourceTypeID, SHO, SHOCV, distance) "+
					"SELECT hourDayID, monthID,yearID, ageID, linkID,"+
					"sourceTypeID, SHO, SHOCV, distance FROM SHO2 ";
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 200
			 * @algorithm Copy back to SHO any SHOTemp information that was ignored because it
			 * was on off-network roads without an average speed. This is ONI data.
			 * @output SHO
			 * @input SHOTemp
			 * @condition Inventory
			**/
			sql = "INSERT IGNORE INTO SHO (hourDayID, monthID, yearID, ageID,"+
					"linkID, sourceTypeID, SHO, SHOCV, distance) "+
					"SELECT hourDayID, monthID,yearID, ageID, linkID,"+
					"sourceTypeID, SHO, SHOCV, 0 as distance FROM SHOTemp ";
			SQLRunner.executeSQL(db,sql);

			sql = "DROP TABLE IF EXISTS SHO2 ";
			SQLRunner.executeSQL(db,sql);
			sql = "DROP TABLE IF EXISTS SHOTemp ";
			SQLRunner.executeSQL(db,sql);
			sql = "DROP TABLE IF EXISTS AverageSpeedTemp ";
			SQLRunner.executeSQL(db,sql);
		}
	}

	/**
	 * Check the markers for table, region, and year combinations that have already
	 * been calculated.  If not already calculated, mark the passed combination as
	 * calculated.
	 * @param tableName table to be checked
	 * @param regionID zone or link to be checked
	 * @param year year to be checked
	 * @return true if the passed combination has already been calculated
	**/
	boolean checkAndMark(String tableName, int regionID, int year) {
		String key = tableName + "|" + regionID + "|" + year;
		if(calculationFlags.contains(key)) {
			return true;
		}
		calculationFlags.add(key);
		return false;
	}

	/**
	 * Clear all calculation markers for a table.
	 * @param tableName table to be cleared
	**/
	void clearFlags(String tableName) {
		String prefix = tableName + "|";
		prefix = prefix.toLowerCase();
		String key;
		TreeSet<String> keysToRemove = new TreeSet<String>();
		for(Iterator i=calculationFlags.iterator();i.hasNext();) {
			key = (String)i.next();
			key = key.toLowerCase();
			if(key.startsWith(prefix)) {
				keysToRemove.add(key);
			}
		}
		for(Iterator<String> i=keysToRemove.iterator();i.hasNext();) {
			calculationFlags.remove(i.next());
		}
	}

	/**
	 * Apply user-supplied adjustments to Starts.
	 * @param zoneID affected zone
	 * @param yearID affected calendar year
	**/	
	void adjustStarts(int zoneID, int yearID) {
		String mainDatabaseName = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()].databaseName;

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		replacements.put("##zoneID##","" + zoneID);
		replacements.put("##yearID##","" + yearID);
		
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			sql = "create table if not exists tempMessages "
					+ "( message varchar(1000) not null )";
			SQLRunner.executeSQL(db,sql);
			sql = "truncate table tempMessages";
			SQLRunner.executeSQL(db,sql);

			try {
				DatabaseUtilities.executeScript(db,new File("database/AdjustStarts.sql"),replacements,false);
			} catch(Exception e) {
				Logger.logError(e,"Unable to adjust starts" + e);
			}

			// Retrieve the results from tempMessages
			sql = "select message from tempMessages";
			query.open(db,sql);
			String m = "";
			while(query.rs.next()) {
				m = query.rs.getString(1);
			}
			query.close();
			if (m != null && m.length() > 0) {
				Logger.log(LogMessageCategory.WARNING,m);
			}
			sql = "drop table if exists tempMessages";
			SQLRunner.executeSQL(db,sql);
		} catch(Exception e) {
			// Nothing to do here
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Apply user-supplied adjustments to HotellingHours.
	 * @param zoneID affected zone
	 * @param yearID affected calendar year
	 * @param hotellingActivityZoneID zoneID to be used for hotellingActivityDistribution.zoneID
	**/	
	void adjustHotelling(int zoneID, int yearID, int hotellingActivityZoneID) {
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		replacements.put("##zoneID##","" + zoneID);
		replacements.put("##yearID##","" + yearID);
		replacements.put("##activityZoneID##","" + hotellingActivityZoneID);
		try {
			DatabaseUtilities.executeScript(db,new File("database/AdjustHotelling.sql"),replacements,false);
		} catch(Exception e) {
			Logger.logError(e,"Unable to adjust HotellingHours");
		}
	}

	/**
	 * Using the wildcard system for zoneIDs within the hotellingActivityDistribution table,
	 * select the value for hotellingActivityDistribution.zoneID that should be used for a
	 * given real state and zone.
	 * @param db database connection to use
	 * @param stateID affected state
	 * @param zoneID affected zone
	 * @return zoneID to be used for hotellingActivityDistribution.zoneID
	 * @throws SQLException if something goes wrong finding the zone
	**/	
	public static int findHotellingActivityDistributionZoneIDToUse(Connection db, int stateID, int zoneID) {
		String sql = "select zoneID"
				+ " from ("
				+ " select distinct zoneID,"
				+ " 	case when zoneID=" + zoneID + " then 1" // The best match is the actual zone
				+ " 	when zoneID=" + (stateID*10000) + " then 2" // The second best match is the zone's state
				+ " 	when zoneID=990000 then 3" // The third best match is the national default
				+ " 	else 4 end as zoneMerit" // Anything else is tied for last place
				+ " from hotellingActivityDistribution"
				+ " where zoneID in ("+zoneID+","+(stateID*10000)+",990000)"
				+ " ) t"
				+ " order by zoneMerit" // Order by best (1) to worst (4)
				+ " limit 1"; // Only get the best scoring
		int resultZoneID = 0;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			if(query.rs.next()) {
				resultZoneID = query.rs.getInt("zoneID");
			}
			query.close();
		} catch(SQLException e) {
			query.onException(e,"Unable to find hotellingActivityDistribution wildcard zone",sql);
		} finally {
			query.onFinally();
		}
		return resultZoneID;
	}

	/**
	 * Apply user-supplied adjustments to TotalIdleFraction.
	**/	
	void adjustTotalIdleFraction() {
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		try {
			DatabaseUtilities.executeScript(db,new File("database/AdjustTotalIdleFraction.sql"),replacements,false);
		} catch(Exception e) {
			Logger.logError(e,"Unable to adjust TotalIdleFraction");
		}
	}
}
