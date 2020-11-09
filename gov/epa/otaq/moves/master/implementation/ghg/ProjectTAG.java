/**************************************************************************************************
 * @(#)ProjectTAG.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.sql.*;
import java.util.*;

/**
 * Build Total Activity records for project domains.
 *
 * @author		Wesley Faler
 * @version		2017-09-17
**/
public class ProjectTAG extends Generator {
	/** Flag for whether the data tables have been cleared/setup **/
	boolean initialLoop = true;
	/** Database connection used by all functions.  Setup by executeLoop and cleanDataLoop. **/
	Connection db;
	/** true on executing the first loop iteration **/
	boolean isFirstIteration = true;
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
	/** total milliseconds spent **/
	long totalTime = 0;
	/** String objects representing tables and time/location keys that have been generated **/
	TreeSet<String> itemsGenerated = new TreeSet<String>();

	/** Default constructor **/
	public ProjectTAG() {
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
	 * @param context The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext context) {
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

			long start, focusStart;
			start = System.currentTimeMillis();
			if(isFirstIteration) {
				isFirstIteration = false;
				setup();
			}

			allocateTotalActivityBasis(context); // SHO, Distance, SHP, SourceHours, etc
			// Distance is an innate part of making SHO since link lengths are known

			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,"Project Total Activity Generation failed for year "+context.year);
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"ProjectTAG totalTime=" + totalTime);
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		// Don't do any cleanup.  All data created herein could be needed across multiple processes.
		/*
		String sql = "";
		try {
			sql = "DELETE FROM SHO WHERE isUserInput='N' AND linkID="
					+ context.iterLocation.linkRecordID;
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM SourceHours WHERE isUserInput='N' "
					+ "AND linkID=" context.iterLocation.linkRecordID;
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM ExtendedIdleHours WHERE isUserInput='N' "
					+ "AND zoneID=" + context.iterLocation.zoneRecordID;
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM hotellingHours WHERE isUserInput='N' "
					+ "AND zoneID=" + context.iterLocation.zoneRecordID;
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM Starts WHERE isUserInput='N' "
					+ "AND zoneID=" + context.iterLocation.zoneRecordID;
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM SHP WHERE isUserInput='N' "
					+ "AND zoneID=" + context.iterLocation.zoneRecordID;
			SQLRunner.executeSQL(db, sql);
		} catch(Exception e) {
			Logger.logSqlError(e,"Could not delete Project Total Activity data.",sql);
		}
		*/
	}

	/**
	 * Determine if a section of data, identified by a key, within a table has already
	 * been generated.  If it has, true is returned.  If it has not, false is returned and
	 * the data is marked as having been generated.
	 * @param tableName table to hold the generated data
	 * @param key key to the data, such as the year or link
	 * @return true if the data has already been generated, false otherwise.
	**/
	boolean hasGenerated(String tableName, int key) {
		return hasGenerated(tableName,key,0);
	}

	/**
	 * Determine if a section of data, identified by two keys, within a table has already
	 * been generated.  If it has, true is returned.  If it has not, false is returned and
	 * the data is marked as having been generated.
	 * @param tableName table to hold the generated data
	 * @param key1 key to the data, such as the year or link
	 * @param key2 key to the data, such as the year or link
	 * @return true if the data has already been generated, false otherwise.
	**/
	boolean hasGenerated(String tableName, int key1, int key2) {
		String t = tableName + "|" + key1 + "|" + key2;
		if(itemsGenerated.contains(t)) {
			return true;
		}
		itemsGenerated.add(t);
		return false;
	}

	/**
	 * Create all the tables needed by the Total Activity Generator and purge any data left over
	 * in them from a previous run.
	 * @throws SQLException If setup cannot be completed.
	**/
	void setup() throws SQLException {
		String sql = "";

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
	 * Allocate Total Activity Basis, Starts, SHP and Source Hours.
	 * @param inContext Current loop context being run.
	 * @throws SQLException If failed to allocate Total Activity Basis.
	**/
	void allocateTotalActivityBasis(MasterLoopContext inContext) throws SQLException {
		String sql = "";

		int zoneID = inContext.iterLocation.zoneRecordID;
		int linkID = inContext.iterLocation.linkRecordID;
		int year = inContext.year;

		boolean isRates = ExecutionRunSpec.getRunSpec().scale == ModelScale.MESOSCALE_LOOKUP;

		boolean makeAge = false;
		boolean makeSHO = false;
		boolean makeSHP = false;
		boolean makeStarts = false;
		boolean makeExtIdle = false;
		boolean makeHotelling = false;
		boolean makeSH = false;

		makeAge = !hasGenerated("SourceTypeAgeDistribution",year);

		if((evapPermeationProcess!=null &&
				inContext.iterProcess.compareTo(evapPermeationProcess)==0) ||
				(evapFuelVaporVentingProcess!=null &&
				inContext.iterProcess.compareTo(evapFuelVaporVentingProcess)==0) ||
		   		(evapFuelLeaksProcess!=null &&
	   			inContext.iterProcess.compareTo(evapFuelLeaksProcess)==0) ||
		   		(evapNonFuelVaporsProcess!=null &&
		   		inContext.iterProcess.compareTo(evapNonFuelVaporsProcess)==0)) {
			makeSH =
					!hasGenerated("SourceHours",linkID,year);
			if(inContext.iterLocation.roadTypeRecordID==1) {
				makeSHP = !hasGenerated("SHP",zoneID);
			} else {
				makeSHO = !hasGenerated("SHO",linkID,year);
			}
		}
		if((runningExhaustProcess!=null &&
				inContext.iterProcess.compareTo(runningExhaustProcess)==0) ||
				(brakeWearProcess!=null &&
				inContext.iterProcess.compareTo(brakeWearProcess)==0) ||
				(tireWearProcess!=null &&
				inContext.iterProcess.compareTo(tireWearProcess)==0)) {
			makeSHO = !hasGenerated("SHO",linkID,year);
		}

		if(startExhaustProcess!=null &&
				inContext.iterProcess.compareTo(startExhaustProcess)==0) {
			makeStarts = !hasGenerated("Starts",zoneID,year);
		}

		if(extendedIdleProcess!=null &&
				inContext.iterProcess.compareTo(extendedIdleProcess)==0) {
			makeExtIdle = !hasGenerated("ExtIdle",zoneID);
		}
		if(CompilationFlags.ENABLE_AUXILIARY_POWER_EXHAUST && auxiliaryPowerProcess!=null &&
				inContext.iterProcess.compareTo(auxiliaryPowerProcess)==0) {
			makeHotelling = !hasGenerated("Hotel",zoneID);
		}

		if(makeAge) {
			// Check SourceTypeAgeDistribution for year
			sql = "select yearID from sourceTypeAgeDistribution"
					+ " where yearID=" + year
					+ " limit 1";
			int temp = (int)SQLRunner.executeScalar(db,sql);
			if(temp <= 0) {
				// There is no age distribution data, so fill it from population data
				try {
					TotalActivityGenerator.setupAgeTables(db);
					int baseYear = TotalActivityGenerator.determineBaseYear(db,year);
					TotalActivityGenerator.calculateBaseYearPopulation(db,baseYear);
					TotalActivityGenerator.growPopulationToAnalysisYear(db,baseYear,0,year);
					// Fill SourceTypeAgeDistribution from SourceTypeAgePopulation
					String[] statements = {
						"drop table if exists sourceTypeAgePopulationSummary",
						"create table sourceTypeAgePopulationSummary"
								+ " select yearID, sourceTypeID, sum(population) as totalPopulation"
								+ " from sourceTypeAgePopulation"
								+ " where yearID=" + year
								+ " group by yearID, sourceTypeID"
								+ " order by null",
						"delete from sourceTypeAgeDistribution where yearID=" + year,
						"insert into sourceTypeAgeDistribution (sourceTypeID, yearID, ageID, "
								+ " ageFraction)"
								+ " select p.sourceTypeID, p.yearID, p.ageID, "
								+ " (p.population/s.totalPopulation) as ageFraction"
								+ " from sourceTypeAgePopulationSummary s"
								+ " inner join sourceTypeAgePopulation p "
								+ " on (p.yearID=s.yearID and p.sourceTypeID=s.sourceTypeID)"
								+ " where s.totalPopulation > 0"
					};
					for(int i=0;i<statements.length;i++) {
						sql = statements[i];
						SQLRunner.executeSQL(db,sql);
					}
				} catch(Exception e) {
					Logger.logError(e,"Unable to make SourceTypeAgeDistribution");
				}
			}
		}

		if(makeSHO) {
			sql = "insert ignore into SHO (hourDayID, monthID, yearID, ageID, linkID, sourceTypeID, SHO, distance)"
					+ " select hd.hourDayID, rsm.monthID, stad.yearID, stad.ageID, l.linkID, stad.sourceTypeID, "
					+ " 	case when l.linkAvgSpeed > 0 then"
					+ " 		(l.linkVolume*lsth.sourceTypeHourFraction*dow.noOfRealDays*stad.ageFraction)"
					+ " 		*least(linkLength / linkAvgSpeed,1.0)"
					+ " 	else"
					+ " 		(l.linkVolume*lsth.sourceTypeHourFraction*dow.noOfRealDays*stad.ageFraction)"
					+ " 	end as SHO,"
					+ " 	case when l.linkAvgSpeed > 0 then"
					+ " 		(l.linkVolume*lsth.sourceTypeHourFraction*dow.noOfRealDays*stad.ageFraction)"
					+ " 		*least(linkLength / linkAvgSpeed,1.0) * linkAvgSpeed"
					+ " 	else 0.0"
					+ " 	end as distance"
					+ " from"
					+ " 	runSpecSourceType rsst,"
					+ " 	runSpecDay rsd,"
					+ " 	runSpecMonth rsm,"
					+ " 	dayOfAnyWeek dow,"
					+ " 	link l,"
					+ " 	linkSourceTypeHour lsth,"
					+ " 	sourceTypeAgeDistribution stad,"
					+ " 	hourDay hd, runSpecHourDay rshd"
					+ " where"
					+ " 	hd.hourDayID = rshd.hourDayID"
					+ " 	and dow.dayID = rsd.dayID"
					+ " 	and hd.dayID = dow.dayID"
					+ " 	and stad.sourceTypeID = rsst.sourceTypeID"
					+ " 	and lsth.sourceTypeID = rsst.sourceTypeID"
					+ " 	and l.linkID = " + linkID
					+ " 	and lsth.linkID = " + linkID
					+ " 	and stad.yearID = " + year;
			SQLRunner.executeSQL(db,sql);
		}
		if(makeSHP) {
			sql = "insert ignore into SHP (hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID, SHP)"
					+ " select hd.hourDayID, rsm.monthID, stad.yearID, stad.ageID, " + zoneID + " as zoneID, stad.sourceTypeID, "
					+ " 	(onl.vehiclePopulation*onl.parkedVehicleFraction*stad.ageFraction*dow.noOfRealDays) as SHP"
					+ " from"
					+ " 	offNetworkLink onl,"
					+ " 	runSpecSourceType rsst,"
					+ " 	runSpecDay rsd,"
					+ " 	runSpecMonth rsm,"
					+ " 	dayOfAnyWeek dow,"
					+ " 	sourceTypeAgeDistribution stad,"
					+ " 	hourDay hd, runSpecHourDay rshd"
					+ " where"
					+ " 	hd.hourDayID = rshd.hourDayID"
					+ " 	and dow.dayID = rsd.dayID"
					+ " 	and hd.dayID = dow.dayID"
					+ " 	and onl.sourceTypeID = rsst.sourceTypeID"
					+ " 	and stad.sourceTypeID = onl.sourceTypeID"
					+ " 	and stad.yearID = " + year
					+ "		and onl.zoneID = " + zoneID;
			SQLRunner.executeSQL(db,sql);
		}
		if(makeStarts) {
			// Unlike national and county domains, project domain reports start rates as the "rate per started vehicle"
			// not the "rate per existing vehicle".  This requires that start rates = Inventory / (started population), but
			// since obtaining the started population is not easily achievable with SCC output, the inventory is instead inflated,
			// by not multiplying by startFraction, instead of dividing by startFraction afterwards.
			String startFractionFragment = isRates? "" : "*onl.startFraction";

			sql = "insert ignore into starts (hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID, starts)"
					+ " select hd.hourDayID, rsm.monthID, stad.yearID, stad.ageID, " + zoneID + " as zoneID, stad.sourceTypeID, "
					+ " 	(onl.vehiclePopulation" + startFractionFragment + "*stad.ageFraction*dow.noOfRealDays) as starts"
					+ " from"
					+ " 	offNetworkLink onl,"
					+ " 	runSpecSourceType rsst,"
					+ " 	runSpecDay rsd,"
					+ " 	runSpecMonth rsm,"
					+ " 	dayOfAnyWeek dow,"
					+ " 	sourceTypeAgeDistribution stad,"
					+ " 	hourDay hd, runSpecHourDay rshd"
					+ " where"
					+ " 	hd.hourDayID = rshd.hourDayID"
					+ " 	and dow.dayID = rsd.dayID"
					+ " 	and hd.dayID = dow.dayID"
					+ " 	and onl.sourceTypeID = rsst.sourceTypeID"
					+ " 	and stad.sourceTypeID = onl.sourceTypeID"
					+ " 	and stad.yearID = " + year
					+ " 	and onl.zoneID = " + zoneID;
			SQLRunner.executeSQL(db,sql);
		}
		if(makeHotelling) {
			// Fill hotellingHours with the total hotelling hours, including any time spent extended idling.
			//2010B: String fractionFragment = isRates? "" : "*onl.extendedIdleFraction";
			String fractionFragment = "*onl.extendedIdleFraction";

			sql = "insert ignore into hotellingHours (hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID, hotellingHours)"
					+ " select hd.hourDayID, rsm.monthID, stad.yearID, stad.ageID, " + zoneID + " as zoneID, stad.sourceTypeID, "
					+ " 	(onl.vehiclePopulation" + fractionFragment + "*stad.ageFraction*dow.noOfRealDays) as hotellingHours"
					+ " from"
					+ " 	offNetworkLink onl,"
					+ " 	runSpecSourceType rsst,"
					+ " 	runSpecDay rsd,"
					+ " 	runSpecMonth rsm,"
					+ " 	dayOfAnyWeek dow,"
					+ " 	sourceTypeAgeDistribution stad,"
					+ " 	hourDay hd, runSpecHourDay rshd"
					+ " where"
					+ " 	hd.hourDayID = rshd.hourDayID"
					+ " 	and dow.dayID = rsd.dayID"
					+ " 	and hd.dayID = dow.dayID"
					+ " 	and onl.sourceTypeID = rsst.sourceTypeID"
					+ "     and onl.sourceTypeID=62"
					+ " 	and stad.sourceTypeID = onl.sourceTypeID"
					+ " 	and stad.yearID = " + year
					+ " 	and onl.zoneID = " + zoneID;
			SQLRunner.executeSQL(db,sql);
		}
		if(makeExtIdle) {
			//2010B: String fractionFragment = isRates? "" : "*onl.extendedIdleFraction";
			String fractionFragment = "*onl.extendedIdleFraction";

			if(CompilationFlags.ENABLE_AUXILIARY_POWER_EXHAUST) {
				sql = "insert ignore into extendedIdleHours (hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID, extendedIdleHours)"
						+ " select hd.hourDayID, rsm.monthID, stad.yearID, stad.ageID, " + zoneID + " as zoneID, stad.sourceTypeID, "
						+ " 	(onl.vehiclePopulation" + fractionFragment + "*stad.ageFraction*dow.noOfRealDays*hac.opModeFraction) as extendedIdleHours"
						+ " from"
						+ " 	offNetworkLink onl,"
						+ " 	runSpecSourceType rsst,"
						+ " 	runSpecDay rsd,"
						+ " 	runSpecMonth rsm,"
						+ " 	dayOfAnyWeek dow,"
						+ " 	sourceTypeAgeDistribution stad,"
						+ "		hotellingActivityDistribution hac,"
						+ " 	hourDay hd, runSpecHourDay rshd"
						+ " where"
						+ " 	hd.hourDayID = rshd.hourDayID"
						+ " 	and dow.dayID = rsd.dayID"
						+ " 	and hd.dayID = dow.dayID"
						+ " 	and onl.sourceTypeID = rsst.sourceTypeID"
						+ " 	and stad.sourceTypeID = onl.sourceTypeID"
						+ "     and onl.sourceTypeID=62"
						+ "		and hac.opModeID = 200"
						+ "		and hac.beginModelYearID <= stad.yearID - stad.ageID"
						+ "		and hac.endModelYearID >= stad.yearID - stad.ageID"
						+ " 	and stad.yearID = " + year
						+ " 	and onl.zoneID = hac.zoneID and onl.zoneID = " + zoneID;
				SQLRunner.executeSQL(db,sql);
			} else {
				sql = "insert ignore into extendedIdleHours (hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID, extendedIdleHours)"
						+ " select hd.hourDayID, rsm.monthID, stad.yearID, stad.ageID, " + zoneID + " as zoneID, stad.sourceTypeID, "
						+ " 	(onl.vehiclePopulation" + fractionFragment + "*stad.ageFraction*dow.noOfRealDays) as extendedIdleHours"
						+ " from"
						+ " 	offNetworkLink onl,"
						+ " 	runSpecSourceType rsst,"
						+ " 	runSpecDay rsd,"
						+ " 	runSpecMonth rsm,"
						+ " 	dayOfAnyWeek dow,"
						+ " 	sourceTypeAgeDistribution stad,"
						+ " 	hourDay hd, runSpecHourDay rshd"
						+ " where"
						+ " 	hd.hourDayID = rshd.hourDayID"
						+ " 	and dow.dayID = rsd.dayID"
						+ " 	and hd.dayID = dow.dayID"
						+ " 	and onl.sourceTypeID = rsst.sourceTypeID"
						+ "     and onl.sourceTypeID=62"
						+ " 	and stad.sourceTypeID = onl.sourceTypeID"
						+ " 	and stad.yearID = " + year
						+ " 	and onl.zoneID = " + zoneID;
				SQLRunner.executeSQL(db,sql);
			}
		}
		if(makeSH) {
			if(inContext.iterLocation.roadTypeRecordID==1) {
				// Fill like SHP
				sql = "insert ignore into sourceHours (hourDayID, monthID, yearID, ageID, linkID, sourceTypeID, sourceHours)"
						+ " select hd.hourDayID, rsm.monthID, stad.yearID, stad.ageID, " + linkID + " as linkID, stad.sourceTypeID, "
						+ " 	(onl.vehiclePopulation*onl.parkedVehicleFraction*stad.ageFraction*dow.noOfRealDays) as sourceHours"
						+ " from"
						+ " 	offNetworkLink onl,"
						+ " 	runSpecSourceType rsst,"
						+ " 	runSpecDay rsd,"
						+ " 	runSpecMonth rsm,"
						+ " 	dayOfAnyWeek dow,"
						+ " 	sourceTypeAgeDistribution stad,"
						+ " 	hourDay hd, runSpecHourDay rshd"
						+ " where"
						+ " 	hd.hourDayID = rshd.hourDayID"
						+ " 	and dow.dayID = rsd.dayID"
						+ " 	and hd.dayID = dow.dayID"
						+ " 	and onl.sourceTypeID = rsst.sourceTypeID"
						+ " 	and stad.sourceTypeID = onl.sourceTypeID"
						+ " 	and stad.yearID = " + year
						+ " 	and onl.zoneID = " + zoneID;
				SQLRunner.executeSQL(db,sql);
			} else {
				// Fill like SHO
				sql = "insert into sourceHours (hourDayID, monthID, yearID, ageID, linkID, sourceTypeID, sourceHours)"
						+ " select hd.hourDayID, rsm.monthID, stad.yearID, stad.ageID, l.linkID, stad.sourceTypeID, "
						+ " 	(l.linkVolume*lsth.sourceTypeHourFraction*dow.noOfRealDays*stad.ageFraction) as sourceHours"
						+ " from"
						+ " 	runSpecSourceType rsst,"
						+ " 	runSpecDay rsd,"
						+ " 	runSpecMonth rsm,"
						+ " 	dayOfAnyWeek dow,"
						+ " 	link l,"
						+ " 	linkSourceTypeHour lsth,"
						+ " 	sourceTypeAgeDistribution stad,"
						+ " 	hourDay hd, runSpecHourDay rshd"
						+ " where"
						+ " 	hd.hourDayID = rshd.hourDayID"
						+ " 	and dow.dayID = rsd.dayID"
						+ " 	and hd.dayID = dow.dayID"
						+ " 	and stad.sourceTypeID = rsst.sourceTypeID"
						+ " 	and lsth.sourceTypeID = rsst.sourceTypeID"
						+ " 	and l.linkID = " + linkID
						+ " 	and lsth.linkID = " + linkID
						+ " 	and stad.yearID = " + year;
				SQLRunner.executeSQL(db,sql);
			}
		}
	}
}
