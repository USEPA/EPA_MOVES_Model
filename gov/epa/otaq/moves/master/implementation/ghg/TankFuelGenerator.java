/**************************************************************************************************
 * @(#)TankFuelGenerator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.util.*;
import java.sql.*;
import java.io.*;

/**
 * This builds commingled RVP information for the fuels in vehicle tanks.
 *
 * @author		wfaler
 * @version		2013-09-16
**/
public class TankFuelGenerator extends Generator {
	/**
	 * @algorithm
	 * @owner Tank Fuel Generator
	 * @generator
	**/

	/** flag controlling removal of temporary data **/
	private static final boolean allowCleanup = true;

	/** The Evaporative Fuel Permeation Process **/
	//private static EmissionProcess evapPermeation = null;
	/** The Evaporative Fuel Vapor Venting Process **/
	private static EmissionProcess evapFuelVaporVenting = null;
	/** The Evaporative Fuel Liquid Leaks Process **/
	private static EmissionProcess evapFuelLeaks = null;

	/** Flag for whether the data tables have been cleared/setup **/
	boolean hasBeenSetup = false;
	/** Database connection used by all functions.  Setup by executeLoop and cleanDataLoop. **/
	Connection db;
	/** milliseconds spent during one time operations **/
	long setupTime = 0;
	/** milliseconds spent during non-one-time operations **/
	long totalTime = 0;
	/** Comma delimited list of pollutant process ids in the runspec. **/
	String polProcessIDs = "";
	/** Master loop context **/
	MasterLoopContext context;
	/** lines output **/
	int linesOutput=0;
	/** Indicates the current trip being processed **/
	int currentTripID = -1;
	/** fuel region for the current county and calendar year **/
	int fuelRegionID = 0;
	/** fuel year for the current county and calendar year **/
	int fuelYearID = 0;

	/** Default constructor **/
	public TankFuelGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		/*
		if(evapPermeation == null) {
			evapPermeation = EmissionProcess.findByName("Evap Permeation");
		}
		*/
		if(evapFuelVaporVenting == null) {
			evapFuelVaporVenting = EmissionProcess.findByName("Evap Fuel Vapor Venting");
		}
		if(evapFuelLeaks == null) {
			evapFuelLeaks = EmissionProcess.findByName("Evap Fuel Leaks");
		}

		/*
		if(evapPermeation != null) {
			targetLoop.subscribe(this, evapPermeation, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		*/
		if(evapFuelVaporVenting != null) {
			targetLoop.subscribe(this, evapFuelVaporVenting, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
		if(evapFuelLeaks != null) {
			targetLoop.subscribe(this, evapFuelLeaks, MasterLoopGranularity.YEAR,
					MasterLoopPriority.GENERATOR);
		}
	}

	/**
	 * Called each time the zone changes.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			context = inContext;

			long start;

			// The following only has to be done once for each run.
			if(!hasBeenSetup) {
				start = System.currentTimeMillis();
				// Setup
				setupTime = System.currentTimeMillis() - start;
				hasBeenSetup = true;
			}

			start = System.currentTimeMillis();

			calculateAverageTankGasoline(); // steps 100-199

			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,"TankFuelGenerator failed.");
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"TankFuelGenerator setupTime=" + setupTime + " totalTime=" + totalTime);
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		String sql = "";
		String[] commands = {
			"drop table if exists EvapTemp",

			"drop table if exists RVPLossTemp",

			"drop table if exists TFG",

			"drop table if exists TFGTemp",

			"DELETE FROM AverageTankGasoline USING AverageTankGasoline, Zone "
					+ " WHERE isUserInput='N' AND AverageTankGasoline.zoneID=Zone.zoneID "
					+ " AND Zone.countyID=" + context.iterLocation.countyRecordID
					+ " AND fuelYearID=" + fuelYearID
		};
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

			if(allowCleanup) {
				for(int i=0;i<commands.length;i++) {
					sql = commands[i];
					SQLRunner.executeSQL(db, sql);
				}
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to delete tank fuel data from a previous run", sql);
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
			db=null;
		}
	}
	
	/**
	 * TFG steps.  These are all performed in one routine since they are just scripted
	 * SQL without decisions or branch points.
	**/
	void calculateAverageTankGasoline() {
		/**
		 * @step 100
		 * @algorithm ethanolRVP = 2.3.
		 * weatheringConstant = 0.049.
		**/
		double ethanolRVP = 2.3;
		double weatheringConstant = 0.049;

		String sql = "";

		fuelRegionID = 0;
		fuelYearID = 0;
		sql = "select rc.regionID, rc.fuelYearID"
				+ " from regionCounty rc"
				+ " inner join year using (fuelYearID)"
				+ " where regionCodeID = 1"
				+ " and year.yearID = " + context.year
				+ " and countyID = " + context.iterLocation.countyRecordID;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			if(query.rs.next()) {
				fuelRegionID = query.rs.getInt(1);
				fuelYearID = query.rs.getInt(2);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,"Unable to query region and fuel year",sql);
		} finally {
			query.onFinally();
		}


		String[] commands = {
			// TFG-1a
			"drop table if exists TFGUsedFuelFormulation",
			
			"create table TFGUsedFuelFormulation ("
			+ "	fuelFormulationID smallint not null primary key,"
			+ "	fuelSubtypeID smallint not null,"
			+ "	fuelTypeID smallint not null,"
			+ "	kGasoline float,"
			+ "	kEthanol float,"
			+ "	gasPortionRVP float,"
			+ "	index (fuelSubtypeID),"
			+ "	index (fuelTypeID)"
			+ ")",
			
			/**
			 * @step 100
			 * @algorithm Find fuel formulations used by the fuel supply.
			 * @output TFGUsedFuelFormulation
			 * @input FuelSupply
			 * @input Year
			 * @input RunSpecYear
			 * @input RunSpecMonthGroup
			 * @input FuelFormulation
			 * @input FuelSubtype
			 * @input FuelType
			**/
			"insert into TFGUsedFuelFormulation (fuelFormulationID, fuelSubtypeID, fuelTypeID)"
			+ " select distinct fs.fuelFormulationID, ff.fuelSubtypeID,  fst.fuelTypeID"
			+ " from FuelSupply fs"
			+ " inner join Year y using (fuelYearID)"
			+ " inner join RunSpecYear rsy using (yearID)"
			+ " inner join RunSpecMonthGroup rsmg using (monthGroupID)"
			+ " inner join FuelFormulation ff using (fuelFormulationID)"
			+ " inner join FuelSubtype fst using (fuelSubtypeID)"
			+ " inner join FuelType ft using (fuelTypeID)"
			+ " where fuelRegionID = " + fuelRegionID
			+ " and y.fuelYearID = " + fuelYearID
			+ " and marketShare > 0"
			+ " and subjectToEvapCalculations='Y'",
			
			/**
			 * @step 100
			 * @algorithm kGasoline=((-7e-7)*pow(ETOHVolume,3))+(0.0002*pow(ETOHVolume,2))+(0.0024*ETOHVolume)+1.0.
			 * kEthanol=(case when ETOHVolume>0 then (46.321*pow(ETOHVolume,-0.8422)) else 1000.0 end).
			 * @output TFGUsedFuelFormulation
			 * @input FuelFormulation
			**/
			"update TFGUsedFuelFormulation, FuelFormulation"
			+ " set kGasoline=((-7e-7)*pow(ETOHVolume,3))+(0.0002*pow(ETOHVolume,2))+(0.0024*ETOHVolume)+1.0,"
			+ " kEthanol=(case when ETOHVolume>0 then (46.321*pow(ETOHVolume,-0.8422))"
			+ " else 1000.0"
			+ " end)"
			+ " where FuelFormulation.fuelFormulationID=TFGUsedFuelFormulation.fuelFormulationID",
			
			/**
			 * @step 100
			 * @algorithm gasPortionRVP=(RVP-kEthanol*ETOHVolume/100.0* ethanolRVP )/(kGasoline*(100.0-ETOHVolume)/100.0).
			 * @output TFGUsedFuelFormulation
			 * @input FuelFormulation
			**/
			"update TFGUsedFuelFormulation, FuelFormulation"
			+ " set gasPortionRVP=(RVP-kEthanol*ETOHVolume/100.0*" + ethanolRVP + ")/(kGasoline*(100.0-ETOHVolume)/100.0)"
			+ " where FuelFormulation.fuelFormulationID=TFGUsedFuelFormulation.fuelFormulationID",
			
			// TFG-1b
			"drop table if exists TFGFuelSupplyAverage",
			
			"create table TFGFuelSupplyAverage ("
			+ " 	fuelTypeID smallint not null,"
			+ " 	fuelYearID smallint not null,"
			+ " 	monthGroupID smallint not null,"
			+ " 	linearAverageRVP float,"
			+ " 	tankAverageETOHVolume float,"
			+ " 	averageGasPortionRVP float,"
			+ " 	gasoholMarketShare float,"
			+ " 	averageKGasoline float,"
			+ " 	averageKEthanol float,"
			+ " 	noWeatheringReddyRVP float,"
			+ " 	commingledRVP float,"
			+ " 	primary key (fuelTypeID, fuelYearID, monthGroupID)"
			+ " )",

			/**
			 * @step 100
			 * @algorithm linearAverageRVP=sum(RVP*marketShare)/sum(marketShare).
			 * tankAverageETOHVolume=sum(ETOHVolume*marketShare)/sum(marketShare).
			 * averageGasPortionRVP=sum(gasPortionRVP*marketShare)/sum(marketShare).
			 * @output TFGFuelSupplyAverage
			 * @input FuelSupply
			 * @input Year
			 * @input RunSpecYear
			 * @input TFGUsedFuelFormulation
			 * @input FuelFormulation
			**/
			"insert into TFGFuelSupplyAverage (fuelTypeID, fuelYearID, monthGroupID,"
			+ " 	linearAverageRVP, tankAverageETOHVolume, averageGasPortionRVP)"
			+ " select uff.fuelTypeID, fs.fuelYearID, fs.monthGroupID,"
			+ " sum(RVP*marketShare)/sum(marketShare) as linearAverageRVP,"
			+ " sum(ETOHVolume*marketShare)/sum(marketShare) as tankAverageETOHVolume,"
			+ " sum(gasPortionRVP*marketShare)/sum(marketShare) as averageGasPortionRVP"
			+ " from FuelSupply fs"
			+ " inner join Year y using (fuelYearID)"
			+ " inner join RunSpecYear rsy using (yearID)"
			+ " inner join TFGUsedFuelFormulation uff using (fuelFormulationID)"
			+ " inner join FuelFormulation ff using (fuelFormulationID)"
			+ " where fuelRegionID = " + fuelRegionID
			+ " and y.fuelYearID = " + fuelYearID
			+ " and marketShare > 0"
			+ " group by uff.fuelTypeID, fs.fuelYearID, fs.monthGroupID"
			+ " order by null",
			
			// TFG-1c
			"drop table if exists TFGTemp",
			
			"create table TFGTemp ("
			+ " 	fuelTypeID smallint not null,"
			+ " 	fuelYearID smallint not null,"
			+ " 	monthGroupID smallint not null,"
			+ " 	gasoholMarketShare float,"
			+ " 	primary key (fuelTypeID, fuelYearID, monthGroupID)"
			+ " )",

			/**
			 * @step 100
			 * @algorithm gasoholMarketShare=sum(marketShare).
			 * @condition 4 <= ETOHVolume <= 20
			 * @output TFGTemp
			 * @input FuelSupply
			 * @input Year
			 * @input RunSpecYear
			 * @input TFGUsedFuelFormulation
			 * @input FuelFormulation
			**/			
			"insert into TFGTemp (fuelTypeID, fuelYearID, monthGroupID, gasoholMarketShare)"
			+ " select uff.fuelTypeID, fs.fuelYearID, fs.monthGroupID,"
			+ " sum(marketShare) as gasoholMarketShare"
			+ " from FuelSupply fs"
			+ " inner join Year y using (fuelYearID)"
			+ " inner join RunSpecYear rsy using (yearID)"
			+ " inner join TFGUsedFuelFormulation uff using (fuelFormulationID)"
			+ " inner join FuelFormulation ff using (fuelFormulationID)"
			+ " where fuelRegionID = " + fuelRegionID
			+ " and y.fuelYearID = " + fuelYearID
			+ " and marketShare > 0"
			+ " and ETOHVolume >= 4 and ETOHVolume <= 20"
			+ " group by uff.fuelTypeID, fs.fuelYearID, fs.monthGroupID"
			+ " order by null",

			/**
			 * @step 100
			 * @algorithm Copy gasoholMarketShare to TFGFuelSupplyAverage.
			 * @output TFGFuelSupplyAverage
			 * @input TFGTemp
			**/
			"update TFGFuelSupplyAverage, TFGTemp"
			+ " set TFGFuelSupplyAverage.gasoholMarketShare=TFGTemp.gasoholMarketShare"
			+ " where TFGFuelSupplyAverage.fuelTypeID = TFGTemp.fuelTypeID"
			+ " and TFGFuelSupplyAverage.fuelYearID = TFGTemp.fuelYearID"
			+ " and TFGFuelSupplyAverage.monthGroupID = TFGTemp.monthGroupID",
			
			"drop table if exists TFGTemp",
			
			// TFG-1d
			
			/**
			 * @step 100
			 * @algorithm averageKGasoline=((-7e-7)*pow(tankAverageETOHVolume,3))+(0.0002*pow(tankAverageETOHVolume,2))+(0.0024*tankAverageETOHVolume)+1.
			 * averageKEthanol=(case when tankAverageETOHVolume > 0 then (46.321*pow(tankAverageETOHVolume,-0.8422)).
			 * @output TFGFuelSupplyAverage
			**/
			"update TFGFuelSupplyAverage"
			+ " set averageKGasoline=((-7e-7)*pow(tankAverageETOHVolume,3))+(0.0002*pow(tankAverageETOHVolume,2))+(0.0024*tankAverageETOHVolume)+1,"
			+ " averageKEthanol=(case when tankAverageETOHVolume > 0 then (46.321*pow(tankAverageETOHVolume,-0.8422))"
			+ " else 1000.0 end)",
			
			// TFG-1e

			/**
			 * @step 100
			 * @algorithm noWeatheringReddyRVP= averageKGasoline* (100.0- tankAverageETOHVolume)/100.0 * averageGasPortionRVP + averageKEthanol * tankAverageETOHVolume/100.0 * ethanolRVP.
			 * @output TFGFuelSupplyAverage
			**/
			"update TFGFuelSupplyAverage"
			+ " set noWeatheringReddyRVP= averageKGasoline* (100.0- tankAverageETOHVolume)/100.0 * averageGasPortionRVP"
			+ " 	+ averageKEthanol * tankAverageETOHVolume/100.0 * " + ethanolRVP,
			
			// TFG-2a
			"drop table if exists TFGZone",
			
			"create table TFGZone ("
			+ " 	zoneID int not null,"
			+ " 	monthGroupID smallint not null,"
			+ " 	zoneMin float,"
			+ " 	zoneMax float,"
			+ " 	zoneEvapTemp float,"
			+ " 	primary key (zoneID, monthGroupID),"
			+ " 	index (monthGroupID, zoneID)"
			+ " )",

			/**
			 * @step 100
			 * @algorithm Find zone temperature extremes.
			 * zoneMin=min(temperature).
			 * zoneMax=max(temperature).
			 * @output TFGZone
			 * @input Zone
			 * @input ZoneMonthHour
			 * @input RunSpecMonth
			 * @input MonthOfAnyYear
			**/			
			"insert into TFGZone (zoneID, monthGroupID, zoneMin, zoneMax)"
			+ " select z.zoneID, m.monthGroupID, min(temperature) as zoneMin, max(temperature) as zoneMax"
			+ " from Zone z"
			+ " inner join ZoneMonthHour zmh using (zoneID)"
			+ " inner join RunSpecMonth rsm on (rsm.monthID = zmh.monthID)"
			+ " inner join MonthOfAnyYear m on (m.monthID = rsm.monthID)"
			+ " where countyID = " + context.iterLocation.countyRecordID
			+ " group by z.zoneID, m.monthGroupID"
			+ " order by null",
			
			/**
			 * @step 100
			 * @algorithm zoneEvapTemp = (case when ((zoneMax < 40) or (zoneMax-zoneMin <= 0)) then (zoneMin+zoneMax)/2.0
			 * else -1.7474+1.029*zoneMin+0.99202*(zoneMax-zoneMin)-0.0025173*zoneMin*(zoneMax-zoneMin)
			 * end).
			 * @output TFGZone
			**/			
			"update TFGZone"
			+ " set zoneEvapTemp = (case when ((zoneMax < 40) or (zoneMax-zoneMin <= 0)) then (zoneMin+zoneMax)/2.0"
			+ " else -1.7474+1.029*zoneMin+0.99202*(zoneMax-zoneMin)-0.0025173*zoneMin*(zoneMax-zoneMin)"
			+ " end)",
			
			// TFG-2b
			"drop table if exists TFGZoneFuel",
			
			"create table TFGZoneFuel ("
			+ " 	zoneID int not null,"
			+ " 	fuelTypeID smallint not null,"
			+ " 	fuelYearID smallint not null,"
			+ " 	monthGroupID smallint not null,"
			+ " 	ratioGasolineRVPLoss float,"
			+ " 	weatheredGasPortionRVP float,"
			+ " 	weatheredReddyRVP float,"
			+ " 	primary key (zoneID, fuelTypeID, fuelYearID, monthGroupID),"
			+ " 	index (fuelTypeID, fuelYearID, monthGroupID),"
			+ " 	index (fuelTypeID),"
			+ " 	index (fuelYearID),"
			+ " 	index (monthGroupID),"
			+ " 	index (zoneID)"
			+ " )",

			/**
			 * @step 100
			 * @algorithm ratioGasolineRVPLoss=greatest(0,
			 * (-2.4908+0.026196*zoneEvapTemp+0.00076898*zoneEvapTemp*averageGasPortionRVP)
			 * /
			 * (-0.0860+0.070592*averageGasPortionRVP)
			 * ).
			 * @output TFGZoneFuel
			 * @input TFGZone
			 * @input TFGFuelSupplyAverage
			**/			
			"insert into TFGZoneFuel (zoneID, fuelTypeID, fuelYearID, monthGroupID, ratioGasolineRVPLoss)"
			+ " select z.zoneID, fs.fuelTypeID, fs.fuelYearID, z.monthGroupID,"
			+ " greatest(0,"
			+ " 	(-2.4908+0.026196*zoneEvapTemp+0.00076898*zoneEvapTemp*averageGasPortionRVP)"
			+ " 	/"
			+ " 	(-0.0860+0.070592*averageGasPortionRVP)"
			+ " ) as ratioGasolineRVPLoss"
			+ " from TFGZone z"
			+ " inner join TFGFuelSupplyAverage fs using (monthGroupID)",
			
			// TFG-2c

			/**
			 * @step 100
			 * @algorithm weatheredGasPortionRVP=averageGasPortionRVP*(1.0-ratioGasolineRVPLoss*weatheringConstant).
			 * @output TFGZoneFuel
			 * @input TFGFuelSupplyAverage
			**/
			"update TFGZoneFuel, TFGFuelSupplyAverage"
			+ " set TFGZoneFuel.weatheredGasPortionRVP="
			+ " 	averageGasPortionRVP*(1.0-ratioGasolineRVPLoss*" + weatheringConstant + ")"
			+ " where TFGFuelSupplyAverage.fuelTypeID=TFGZoneFuel.fuelTypeID"
			+ " and TFGFuelSupplyAverage.fuelYearID=TFGZoneFuel.fuelYearID"
			+ " and TFGFuelSupplyAverage.monthGroupID=TFGZoneFuel.monthGroupID",
			
			// TFG-2d

			/**
			 * @step 100
			 * @algorithm weatheredReddyRVP=
			 * averageKGasoline*(100.0-tankAverageETOHVolume)/100.0*weatheredGasPortionRVP
			 * + averageKEthanol*tankAverageETOHVolume/100.0*ethanolRVP.
			 * @output TFGZoneFuel
			 * @input TFGFuelSupplyAverage
			**/
			"update TFGZoneFuel, TFGFuelSupplyAverage"
			+ " set TFGZoneFuel.weatheredReddyRVP="
			+ " 	averageKGasoline*(100.0-tankAverageETOHVolume)/100.0*weatheredGasPortionRVP"
			+ " 	+ averageKEthanol*tankAverageETOHVolume/100.0*" + ethanolRVP
			+ " where TFGFuelSupplyAverage.fuelTypeID=TFGZoneFuel.fuelTypeID"
			+ " and TFGFuelSupplyAverage.fuelYearID=TFGZoneFuel.fuelYearID"
			+ " and TFGFuelSupplyAverage.monthGroupID=TFGZoneFuel.monthGroupID",
			
			// TFG-3a

			/**
			 * @step 100
			 * @algorithm commingledRVP=linearAverageRVP*(case
			 * when gasoholMarketShare >= 1.0 then 1.000
			 * when gasoholMarketShare >= 0.9 then 1.018
			 * when gasoholMarketShare >= 0.8 then 1.027
			 * when gasoholMarketShare >= 0.7 then 1.034
			 * when gasoholMarketShare >= 0.6 then 1.038
			 * when gasoholMarketShare >= 0.5 then 1.040
			 * when gasoholMarketShare >= 0.4 then 1.039
			 * when gasoholMarketShare >= 0.3 then 1.035
			 * when gasoholMarketShare >= 0.2 then 1.028
			 * when gasoholMarketShare >= 0.1 then 1.016
			 * else 1.000"
			 * end).
			 * @output TFGFuelSupplyAverage
			**/
			"update TFGFuelSupplyAverage"
			+ " set commingledRVP=linearAverageRVP*(case"
			+ " 	when gasoholMarketShare >= 1.0 then 1.000"
			+ " 	when gasoholMarketShare >= 0.9 then 1.018"
			+ " 	when gasoholMarketShare >= 0.8 then 1.027"
			+ " 	when gasoholMarketShare >= 0.7 then 1.034"
			+ " 	when gasoholMarketShare >= 0.6 then 1.038"
			+ " 	when gasoholMarketShare >= 0.5 then 1.040"
			+ " 	when gasoholMarketShare >= 0.4 then 1.039"
			+ " 	when gasoholMarketShare >= 0.3 then 1.035"
			+ " 	when gasoholMarketShare >= 0.2 then 1.028"
			+ " 	when gasoholMarketShare >= 0.1 then 1.016"
			+ " 	else 1.000"
			+ " 	end)",
			
			// TFG-3b

			/**
			 * @step 100
			 * @algorithm ETOHVolume=tankAverageETOHVolume.
			 * RVP=(weatheredReddyRVP*commingledRVP)/noWeatheringReddyRVP.
			 * @output AverageTankGasoline
			 * @input TFGZoneFuel
			 * @input TFGFuelSupplyAverage
			**/
			"insert ignore into AverageTankGasoline (zoneID, fuelTypeID, fuelYearID, monthGroupID,"
			+ " 	ETOHVolume, RVP, isUserInput)"
			+ " select zf.zoneID, zf.fuelTypeID, zf.fuelYearID, zf.monthGroupID,"
			+ " 	tankAverageETOHVolume as ETOHVolume,"
			+ " 	(weatheredReddyRVP*commingledRVP)/noWeatheringReddyRVP as RVP,"
			+ " 	'N' as isUserInput"
			+ " from TFGZoneFuel zf"
			+ " inner join TFGFuelSupplyAverage fsa using (fuelTypeID, fuelYearID, monthGroupID)",
			
			// Cleanup
			"drop table if exists TFGUsedFuelFormulation",
			
			"drop table if exists TFGFuelSupplyAverage",
			
			"drop table if exists TFGTemp",
			
			"drop table if exists TFGZone",
			
			"drop table if exists TFGZoneFuel"
		};

		try {
			for(int i=0;i<commands.length;i++) {
				sql = commands[i];
				SQLRunner.executeSQL(db, sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not do TFG sql.", sql);
		}
	}
}
