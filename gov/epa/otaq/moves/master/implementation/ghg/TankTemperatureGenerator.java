/**************************************************************************************************
 * @(#)TankTemperatureGenerator.java
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
 * This builds average vehicle fuel tank temperature and soak mode activity fraction records
 * for ELDB data. ELDB is the Execution Location Database explained in TotalActivityGenerator.
 *
 * @author		Wesley Faler
 * @author      EPA Mitch C. - added logic to prevent unnecessary rerun for same zone
 *              also adapted SampleVehicleTrip.vehID to full INTEGER
 * @version		2009-12-05
**/
public class TankTemperatureGenerator extends Generator {
	/**
	 * @algorithm
	 * @owner Tank Temperature Generator
	 * @generator
	**/

	/** The Evaporative Fuel Permeation Process **/
	private static EmissionProcess evapPermeation = null;
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
	/** true when routines that only need to run once have been performed **/
	boolean didOneTimeOnlyItems = false;
//	/** true if the model scale is Mesoscale Lookup **/
//	boolean isMesoscaleLookup = false;
	/** True if the SampleVehicleTripByHour table needs to be populated **/
	private boolean needSampleVehicleTripByHour = true;
	/** True if the HotSoakEventByHour table needs to be populated **/
	private boolean needHotSoakEventByHour = true;
	/** True if the ColdSoakTankTemperature table needs to be populated for the current zone **/
	private boolean needColdSoakTankTemperature = true;
	/** True if the AverageTankTemperature table needs to be populated for the current zone **/
	private boolean needAverageTankTemperature = true;
	/** True if the SoakActivityFraction table needs to be populated for the current zone **/
	private boolean needSoakActivityFraction = true;
	/** True if the ColdSoakInitialHourFraction table needs to be populated for the current zone **/
	private boolean needColdSoakInitialHourFraction = true;

	/** Default constructor **/
	public TankTemperatureGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		if(evapPermeation == null) {
			evapPermeation = EmissionProcess.findByName("Evap Permeation");
		}
		if(evapFuelVaporVenting == null) {
			evapFuelVaporVenting = EmissionProcess.findByName("Evap Fuel Vapor Venting");
		}
		if(evapFuelLeaks == null) {
			evapFuelLeaks = EmissionProcess.findByName("Evap Fuel Leaks");
		}

		if(evapPermeation != null) {
			targetLoop.subscribe(this, evapPermeation, MasterLoopGranularity.ZONE,
					MasterLoopPriority.GENERATOR);
		}
		if(evapFuelVaporVenting != null) {
			targetLoop.subscribe(this, evapFuelVaporVenting, MasterLoopGranularity.ZONE,
					MasterLoopPriority.GENERATOR);
		}
		if(evapFuelLeaks != null) {
			targetLoop.subscribe(this, evapFuelLeaks, MasterLoopGranularity.ZONE,
					MasterLoopPriority.GENERATOR);
		}
	}

	/**
	 * Called each time the zone changes.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
//		isMesoscaleLookup = ExecutionRunSpec.theExecutionRunSpec.getModelScale()
//				== ModelScale.MESOSCALE_LOOKUP;

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

				createTemporarySummaryTables(); // steps 100-149
			}

			start = System.currentTimeMillis();
			
			// determine what we need to run
			determineNeedToExecuteSteps(inContext); // steps 150-199
			calculateColdSoakTankTemperature(inContext); // TTG-1, steps 200-209
			
			// TODO TTG-2 and TTG-3 don't have any process, location, or time dependant information,
			// so they only need to be performed once regardless of the number of master loops.
			if(!didOneTimeOnlyItems) {
				didOneTimeOnlyItems = true;
				checkOneTimeNeeds();
				flagMarkerTrips(); // steps 210-219
				buildTTGeMinutes(); // steps 220-229
				createSampleVehicleTripByHour(); // TTG-2, steps 230-239
				createHotSoakEventByHour(); // TTG-3, steps 240-249
			}

			calculateHotSoakAndOperatingTankTemperatures(); // TTG-4, steps 250-259
			calculateAverageTankTemperature(); // TTG-5, steps 260-269

			calculateSoakActivityFraction(); // TTG-6, steps 270-279
			calculateColdSoakInitialHourFractions(); // TTG-7, task 213, steps 280-289

			totalTime += System.currentTimeMillis() - start;
		} catch(Exception e) {
			Logger.logError(e,"TankTemperatureGenerator failed.");
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"TankTemperatureGenerator setupTime=" + setupTime + " totalTime=" + totalTime);
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		String sql = "";
		try {
			// Do not cleanup after each zone since the data is needed for the same zone
			// in other processes.  Since all zones are run before changing processes, this
			// must to lead to fully populated tables that can only be cleaned after the
			// model is run.  By then, there is no point in cleaning the tables as they
			// are useful for debugging and will be cleaned when the next run begins.
			/*
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			sql = "DELETE FROM AverageTankTemperature WHERE isUserInput='N' AND zoneID=" 
					+ context.iterLocation.zoneRecordID;
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM SoakActivityFraction WHERE isUserInput='N' AND zoneID=" 
					+ context.iterLocation.zoneRecordID;
			SQLRunner.executeSQL(db, sql);

			sql = "DELETE FROM ColdSoakInitialHourFraction WHERE isUserInput='N' AND zoneID=" 
					+ context.iterLocation.zoneRecordID;
			SQLRunner.executeSQL(db, sql);
			
			sql = "DELETE FROM ColdSoakTankTemperature WHERE zoneID= " 
					+ context.iterLocation.zoneRecordID;
			SQLRunner.executeSQL(db, sql);
			*/
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to delete tank temperature data from a previous run", sql);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db=null;
			}
		}
	}

	/** Create summary table(s) used once per run **/
	void createTemporarySummaryTables() {
		String sql = "";
		try {
			// Build temp table of vehID and tankTemperatureGroupID
			sql = "drop table if exists TempVehAndTTG";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 100
			 * @algorithm Obtain the required set of vehID and tankTemperatureGroupID combinations.
			 * @output TempVehAndTTG
			 * @input SampleVehicleDay
			 * @input SourceTypeModelYearGroup
			**/
			sql = "create table TempVehAndTTG"
					+ " select distinct vehID, tankTemperatureGroupID"
					+ " from SampleVehicleDay sv"
					+ " inner join SourceTypeModelYearGroup using (sourceTypeID)";
			SQLRunner.executeSQL(db, sql);
			
			sql = "alter table TempVehAndTTG add index (vehID, tankTemperatureGroupID)";
			SQLRunner.executeSQL(db, sql);

			sql = "alter table TempVehAndTTG add index (tankTemperatureGroupID, vehID)";
			SQLRunner.executeSQL(db, sql);

			// Build temp table for storing cold soak tank temperatures
			// for the single zone being executed for
			sql = "drop table if exists TempColdSoakTankTemperature";
			SQLRunner.executeSQL(db, sql);

			sql = "create table TempColdSoakTankTemperature (" +
				  "monthID smallint(6) NOT NULL, " +
				  "hourID smallint(6) NOT NULL, " +
				  "coldSoakTankTemperature float NOT NULL," +
				  "PRIMARY KEY (monthID,hourID), "+
				  "index (hourID,monthID))";
			SQLRunner.executeSQL(db, sql);

		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to create temporary summary tables", sql);
		}
	}

	/**
	 * Utility routine to check for data in a table.
	 * @param sql statement to check, ideally containing a limiting clause because only
	 * the first record is needed.
	 * @return true if the sql statement produced any records, false if there were no
	 * records or a table in the sql does not exist.
	**/
	boolean hasRecord(String sql) {
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			if(query.rs.next()) {
				return true;
			}
		} catch(Exception e) {
			// An error here means the table in the SQL doesn't exist.
		} finally {
			query.onFinally();
		}
		return false;
	}

	/**
	 * Determine whether tables filled once should be filled at all.  They may have been
	 * provided as a CMIT by the user.
	**/
	void checkOneTimeNeeds() {
		needSampleVehicleTripByHour = !hasRecord("select * from SampleVehicleTripByHour limit 1");
		needHotSoakEventByHour = !hasRecord("select * from HotSoakEventByHour limit 1");
	}

	/**
	 * Determine whether this generator needs to execute calculation steps
	 * this time.
	 * @param inContext The Master Loop Context
	**/
	void determineNeedToExecuteSteps(MasterLoopContext inContext) {
		needColdSoakTankTemperature = !hasRecord(
				"select *"
				+ " from ColdSoakTankTemperature"
				+ " where zoneID=" + inContext.iterLocation.zoneRecordID
				+ " limit 1"
				);
		needAverageTankTemperature = !hasRecord(
				"select *"
				+ " from AverageTankTemperature"
				+ " where zoneID=" + inContext.iterLocation.zoneRecordID
				+ " limit 1"
				);
		needSoakActivityFraction = !hasRecord(
				"select *"
				+ " from SoakActivityFraction"
				+ " where zoneID=" + inContext.iterLocation.zoneRecordID
				+ " limit 1"
				);
//		if(isMesoscaleLookup) {
//			needColdSoakInitialHourFraction = false;
//		} else {
			needColdSoakInitialHourFraction = !hasRecord(
					"select *"
					+ " from ColdSoakInitialHourFraction"
					+ " where zoneID=" + inContext.iterLocation.zoneRecordID
					+ " limit 1"
					);
//		}
	}
		
	/**
	 * TTG-1: Calculate coldSoakTankTemperature
	 * @param inContext The Master Loop Context
	**/
	void calculateColdSoakTankTemperature(MasterLoopContext inContext) {
		String sql = "";
		String sql2 = "";
		PreparedStatement statement = null;
		PreparedStatement statement2 = null;
		ResultSet results = null;
		try {
			boolean didMakeQuarterHourTemperature = false;
			if(needColdSoakTankTemperature) {
				needColdSoakTankTemperature = false;
				didMakeQuarterHourTemperature = true;
				//
				// TTG-1a: Calculate temperatures in quarter hour increments.
				//
				sql = "DROP TABLE IF EXISTS QuarterHourTemperature";
				SQLRunner.executeSQL(db, sql);
	
				sql = "CREATE TABLE QuarterHourTemperature ("
						+ "monthID	SMALLINT NOT NULL, "
						+ "hourID	SMALLINT NOT NULL, "
						+ "timeStepID	SMALLINT NOT NULL, "
						+ "quarterHourTemperature FLOAT)";
				SQLRunner.executeSQL(db, sql);
	
				sql = "CREATE UNIQUE INDEX XPKQuarterHourTemperature ON QuarterHourTemperature ("
						+ "monthID ASC, hourID ASC, timeStepID ASC)";
				SQLRunner.executeSQL(db, sql);
	
				int count = 0;
				for(int timeStepID=1;timeStepID<=4;timeStepID++) {
					/**
					 * @step 200
					 * @algorithm Calculate temperatures in quarter hour increments using linear interpolation.
					 * quarterHourTemperature[hourID,timeStep] = temperature[hourID=h-1] + ((timeStep-1) * 0.25 * (temperature[hourID=h] - temperature[hourID=h-1])) for timeSteps 1-4.
					 * @output QuarterHourTemperature
					 * @input ZoneMonthHour for hour h
					 * @input ZoneMonthHour for hour h-1 (or hour 24 if h is the hour 1)
					**/
					sql = "INSERT INTO QuarterHourTemperature (monthID, hourID, timeStepID, "
							+ "quarterHourTemperature) SELECT zmh1.monthID, zmh1.hourID, "
							+ timeStepID + ", zmh1.temperature + " + (timeStepID-1) + "* 0.25 * "
							+ "(zmh2.temperature - zmh1.temperature) FROM ZoneMonthHour zmh1, "
							+ "ZoneMonthHour zmh2 WHERE zmh1.monthID = zmh2.monthID AND "
							+ "zmh1.zoneID = zmh2.zoneID AND (zmh1.hourID = zmh2.hourID-1 OR "
							+ "zmh1.hourID = 24 AND zmh2.hourID = 1) AND zmh1.zoneID = " 
							+ inContext.iterLocation.zoneRecordID;
					count += SQLRunner.executeSQL(db, sql);
				}
	
				//
				// TTG-1b: Calculate tank temperatures by quarter hour.
				//
				sql = "DROP TABLE IF EXISTS QuarterHourTankTemperature";
				SQLRunner.executeSQL(db, sql);
	
				sql = "CREATE TABLE QuarterHourTankTemperature ("
						+ "monthID	SMALLINT NOT NULL, "
						+ "hourID	SMALLINT NOT NULL, "
						+ "timeStepID	SMALLINT NOT NULL, "
						+ "tempDelta FLOAT,"
						+ "quarterHourTankTemperature FLOAT)";
				SQLRunner.executeSQL(db, sql);
	
				sql = "CREATE UNIQUE INDEX XPKQuarterHourTankTemperature ON "
						+ "QuarterHourTankTemperature (monthID ASC, "
						+ "hourID ASC, timeStepID ASC)";
				SQLRunner.executeSQL(db, sql);
	
				sql = "SELECT monthID, quarterHourTemperature FROM QuarterHourTemperature "
						+ "WHERE hourID = 1 AND timeStepID = 1 GROUP BY monthID ORDER BY NULL";
				statement = db.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement,sql);
				Vector<Integer> monthIDs = new Vector<Integer>();
				Vector<Float> quarterHourTemperatures = new Vector<Float>();
				if(results!=null) {
					while(results.next()) {
						monthIDs.add(new Integer(results.getInt(1)));
						quarterHourTemperatures.add(new Float(results.getFloat(2)));
					}
					results.close();
					results = null;
				}
				statement.close();
				statement = null;
	
				count = 0;
				sql2 = "INSERT INTO QuarterHourTankTemperature (monthID, hourID, timeStepID,"
						+ " tempDelta, quarterHourTankTemperature) VALUES (?, ?, ?, ?, ?)";
				statement2=db.prepareStatement(sql2);
	
				for(int i=0;i<monthIDs.size();i++) {
					int monthID = ((Integer)monthIDs.get(i)).intValue();
					/**
					 * @step 200
					 * @algorithm firstQuarterHourTankTemperature[monthID]=quarterHourTemperature[hourID=1,monthID,timeStep=1].
					 * @input QuarterHourTemperature
					**/
					float firstQuarterHourTankTemperature = 
							((Float)quarterHourTemperatures.get(i)).floatValue();
					sql = "SELECT hourID, timeStepID, quarterHourTemperature FROM "
							+ "QuarterHourTemperature WHERE monthID = " + monthID
							+ " ORDER BY hourID, timeStepID";
					statement = db.prepareStatement(sql);
					results = SQLRunner.executeQuery(statement,sql);
					if(results!=null) {
						/**
						 * @step 200
						 * @algorithm sumTempDelta=0
						 * @condition Resets to 0 for each monthID.
						**/
						float sumTempDelta = 0;
						while(results.next()) {
							int hourID = results.getInt(1);
							int timeStepID = results.getInt(2);
							float quarterHourTemperature = results.getFloat(3);
							
							float quarterHourTankTemperature = ((float)1.4)*sumTempDelta
									+ firstQuarterHourTankTemperature;
							float tempDelta = quarterHourTemperature - quarterHourTankTemperature;
							
							/**
							 * @step 200
							 * @algorithm Calculate quarter hour tank temperature from the quarter hour ambient temperature.
							 * quarterHourTankTemperature = 1.4*sumTempDelta + firstQuarterHourTankTemperature.
							 * tempDelta = quarterHourTemperature - quarterHourTankTemperature.
							 * sumTempDelta = sumTempDelta + tempDelta.
							 * @condition for each hour and quarter hour time step of the current month, in temporal sequence
							 * @input QuarterHourTemperature
							 * @output QuarterHourTankTemperature
							**/
							statement2.setInt(1,monthID);
							statement2.setInt(2,hourID);
							statement2.setInt(3,timeStepID);
							statement2.setFloat(4,tempDelta);
							statement2.setFloat(5,quarterHourTankTemperature);
							count += SQLRunner.executeSQL(statement2, sql2);
							sumTempDelta += tempDelta;
						}
						results.close();
						results = null;
					}
					statement.close();
					statement = null;
				}
				
				//
				// TTG-1c: Calculate Cold Soak Tank Temperature
				//
				
				// This copy of ColdSoakTankTemperature goes to MOVESExecution 
				// for use by other model components

				/**
				 * @step 200
				 * @algorithm coldSoakTankTemperature[zoneID,monthID,hourID]=quarterHourTankTemperature[zoneID,monthID,hourID,timeStep=1].
				 * @output ColdSoakTankTemperature
				 * @input QuarterHourTankTemperature
				**/
				sql = "INSERT INTO ColdSoakTankTemperature "
						+ "SELECT " + context.iterLocation.zoneRecordID + " as zoneID, monthID, hourID, "
						+ "quarterHourTankTemperature FROM QuarterHourTankTemperature "
						+ "WHERE timeStepID = 1";
				count = SQLRunner.executeSQL(db, sql);
			}

			if(needAverageTankTemperature || needSoakActivityFraction || needColdSoakInitialHourFraction) {
				// This version of ColdSoakTankTemperature is needed for subsequent steps
				// which were written to expect it to contain data for this zone only.
				sql = "TRUNCATE TempColdSoakTankTemperature" ;
				SQLRunner.executeSQL(db, sql);
	
				// Select from ColdSoakTankTemperature keyed by the zone, if quarterHourTankTemperature wasn't build above because ColdSoakTankTemperature already had data for the zone
				if(didMakeQuarterHourTemperature) {
					// If quarterHourTankTemperature is available, use it directly since it is
					// a smaller table and therefore faster to use.

					/**
					 * @step 200
					 * @algorithm coldSoakTankTemperature[zoneID,monthID,hourID]=quarterHourTankTemperature[zoneID,monthID,hourID,timeStep=1].
					 * @output TempColdSoakTankTemperature
					 * @input QuarterHourTankTemperature
					 * @condition QuarterHourTankTemperature has been populated
					**/
					sql = "INSERT INTO TempColdSoakTankTemperature " +
							"SELECT monthID, hourID, "+
							"quarterHourTankTemperature FROM QuarterHourTankTemperature " +
							"WHERE timeStepID = 1";
				} else {
					/**
					 * @step 200
					 * @algorithm Copy ColdSoakTankTemperature to TempColdSoakTankTemperature.
					 * @output TempColdSoakTankTemperature
					 * @input ColdSoakTankTemperature
					 * @condition QuarterHourTankTemperature has not been populated
					**/
					sql = "INSERT INTO TempColdSoakTankTemperature " +
							"SELECT monthID, hourID, "+
							"quarterHourTankTemperature "+
							"FROM ColdSoakTankTemperature " +
							"WHERE zoneID=" + context.iterLocation.zoneRecordID;
				}
				SQLRunner.executeSQL(db, sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine cold soak tank temperature.", sql);
		} finally {
			if(results!=null) {
				try {
					results.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				results = null;
			} 
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement = null;
			}
			if(statement2!=null) {
				try {
					statement2.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement2 = null;
			}
		}
	}

	/**
	 * Add a column ("tripType", char(1), not null) to the SampleVehicleTrip table that
	 * denotes the type of trip.  "N" for Normal, "M" for marker, "F" for first trip of
	 * the day.
	**/
	void flagMarkerTrips() {
		String[] statements = {
			/**
			 * @step 210
			 * @algorithm Flag all trips by default as Normal (tripType=N). Subsequent steps
			 * may alter this designation.
			 * @output SampleVehicleTrip
			**/
			"alter table SampleVehicleTrip add tripType char(1) not null default 'N'",

			"alter table SampleVehicleTrip add key(tripType)",

			/**
			 * @step 210
			 * @algorithm Flag marker trips.
			 * tripType=M.
			 * @condition keyOnTime is null and priorTripID is null
			 * @output SampleVehicleTrip
			**/
			"update SampleVehicleTrip set tripType='M'"
			+ " where keyOnTime is null and priorTripID is null",

			"drop table if exists MarkerTrip",

			/**
			 * @step 210
			 * @algorithm Copy marker trips from SampleVehicleTrip to MarkerTrip.
			 * @input SampleVehicleTrip
			 * @output MarkerTrip
			**/
			"create table MarkerTrip"
			+ " select vehID, tripID, dayID, hourID, priorTripID, keyOnTime, keyOffTime "
			+ " from SampleVehicleTrip "
			+ " where tripType='M'",

			// Set real first trips to have no prior trip, that way the rest of the code
			// (which is unaware of tripType) can just check for priorTripID

			/**
			 * @step 210
			 * @algorithm Identify trips as the first trip of a day.
			 * tripType = F.
			 * priorTripID = NULL.
			 * @condition Trips having a prior trip that is a marker trip.
			 * @output SampleVehicleTrip
			 * @input MarkerTrip
			**/
			"update SampleVehicleTrip, MarkerTrip"
			+ " set SampleVehicleTrip.priorTripID = NULL, tripType='F'"
			+ " where SampleVehicleTrip.vehID=MarkerTrip.vehID"
			+ " and SampleVehicleTrip.dayID=MarkerTrip.dayID"
			+ " and SampleVehicleTrip.priorTripID=MarkerTrip.tripID",

			"drop table if exists MarkerTrip",

			/**
			 * @step 210
			 * @algorithm Identify more trips as the first trip of a day.
			 * tripType = F.
			 * @condition priorTripID is null and keyOnTime is not null (i.e. there is no prior trip and the vehicle was
			 * started during this trip) and the trip is still considered a Normal trip (tripType=N)
			 * @output SampleVehicleTrip
			**/
			"update SampleVehicleTrip"
			+ " set tripType='F'"
			+ " where priorTripID is null and keyOnTime is not null"
			+ " and tripType='N'",

			null
		};

		String sql = "";
		try {
			for(int i=0;statements[i] != null;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db, sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not flag marker trips", sql);
		}
	}

	/**
	 * TTG-2: Create sampleVehicleTripByHour
	**/
	void createSampleVehicleTripByHour() {
		if(!needSampleVehicleTripByHour) {
			return;
		}
		needSampleVehicleTripByHour = false;

		String sql = "";
		PreparedStatement statement = null;
		PreparedStatement statement2 = null;
		ResultSet results = null;
		try {
			sql = "DROP TABLE IF EXISTS SampleVehicleTripByHour";
			SQLRunner.executeSQL(db, sql);
			
			sql = "CREATE TABLE SampleVehicleTripByHour ("
					+ "vehID INTEGER NOT NULL, "
					+ "tripID SMALLINT NOT NULL, "
					+ "dayID SMALLINT NOT NULL, "
					+ "hourDayID SMALLINT NOT NULL, "
					+ "hourID SMALLINT NOT NULL, "
					+ "priorTripID SMALLINT, "
					+ "keyOnTime INTEGER NOT NULL, "
					+ "keyOffTime INTEGER NOT NULL, "
					+ "endOfHour INTEGER NOT NULL, "
					+ "startOfTrip TINYINT NOT NULL, "
					+ "endOfTrip TINYINT NOT NULL, "
					+ "index (priorTripID, vehID, dayID, hourID), "
					+ "index (vehID, dayID, tripID, hourID) "
					+ ")";
			SQLRunner.executeSQL(db, sql);

			String sql2 = "INSERT INTO SampleVehicleTripByHour (vehID, tripID, dayID, hourDayID, "
					+ "hourID, priorTripID, keyOnTime, keyOffTime, endOfHour, startOfTrip, "
					+ "endOfTrip) "
					+ "SELECT ?, ?, dayID, hourDayID, hourID, ?, ?, ?, ?, ?, ? "
					+ "FROM HourDay WHERE hourID=? AND dayID=?";
			statement2 = db.prepareStatement(sql2);

			// Join to SampleVehicleDay table which is limited by sourceTypeID so that we 
			// don't process records that we don't care about

			/**
			 * @step 230
			 * @algorithm EndOfHour = (FLOOR(keyOnTime/60)+1)*60.
			 * @input SampleVehicleDay
			 * @input SampleVehicleTrip
			 * @input HourDay
			 * @condition tripType <> M (i.e. don't include marker trips)
			 * @output list of trip data in memory
			**/
			sql = "SELECT sv.vehID, tripID, hd.hourDayID, svt.hourID, sv.dayID, priorTripID,"
					+ " keyOnTime, keyOffTime, (FLOOR(keyOnTime/60)+1)*60 AS EndOfHour"
					+ " FROM SampleVehicleDay sv"
					+ " INNER JOIN SampleVehicleTrip svt USING (vehID, dayID)"
					+ " INNER JOIN HourDay hd ON (hd.hourID=svt.hourID AND hd.dayID=svt.dayID)"
					+ " WHERE svt.tripType <> 'M'" // don't include marker trips
					+ " ORDER BY sv.vehID, sv.dayID, tripID, keyOnTime";
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			int count = 0;
			if(results!=null) {
				while(results.next()) {
					int vehID = results.getInt(1);
					int tripID = results.getInt(2);
					int hourDayID = results.getInt(3);
					int hourID = results.getInt(4);
					int dayID = results.getInt(5);
					int priorTripID = results.getInt(6);
					int keyOnTime = results.getInt(7);
					int keyOffTime = results.getInt(8);
					int endOfHour = results.getInt(9);
					
					/**
					 * @step 230
					 * @algorithm Split trips into individual hours.
					 * startOfTrip = 1 for the first hour of the trip, 0 otherwise.
					 * endOfTrip = 1 when keyOffTime <= endOfHour, 0 otherwise.
					 * @output SampleVehicleTripByHour
					 * @input list of trip data in memory
					**/
					if(keyOffTime<=endOfHour) {
						statement2.setInt(1,vehID);
						statement2.setInt(2,tripID);
						statement2.setInt(3,priorTripID);
						statement2.setInt(4,keyOnTime);
						statement2.setInt(5,keyOffTime);
						statement2.setInt(6,endOfHour);
						statement2.setInt(7,1);
						statement2.setInt(8,1);
						statement2.setInt(9,hourID);
						statement2.setInt(10,dayID);
						count += SQLRunner.executeSQL(statement2, sql2);
					} else {
						int newKeyOffTime = endOfHour;
						statement2.setInt(1,vehID);
						statement2.setInt(2,tripID);
						statement2.setInt(3,priorTripID);
						statement2.setInt(4,keyOnTime);
						statement2.setInt(5,newKeyOffTime);
						statement2.setInt(6,endOfHour);
						statement2.setInt(7,1);
						statement2.setInt(8,0);
						statement2.setInt(9,hourID);
						statement2.setInt(10,dayID);
						count += SQLRunner.executeSQL(statement2, sql2);

						boolean done = false;
						while(!done) {
							hourID = hourID==24?1:hourID+1;
							keyOnTime = endOfHour + 1;
							endOfHour += 60;
							if(keyOffTime>endOfHour) {
								newKeyOffTime = endOfHour;
							} else {
								newKeyOffTime = keyOffTime;
								done = true;
							}
							statement2.setInt(1,vehID);
							statement2.setInt(2,tripID);
							statement2.setInt(3,priorTripID);
							statement2.setInt(4,keyOnTime);
							statement2.setInt(5,newKeyOffTime);
							statement2.setInt(6,endOfHour);
							statement2.setInt(7,0);
							statement2.setInt(8,done?1:0);
							statement2.setInt(9,hourID);
							statement2.setInt(10,dayID);
							count += SQLRunner.executeSQL(statement2, sql2);
						}
					}
				}
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine soak times for sample vehicle trips.", sql);
		} finally {
			if(results!=null) {
				try {
					results.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				results = null;
			}
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement = null;
			}
			if(statement2!=null) {
				try {
					statement2.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement2 = null;
			}
		}
	}

	/**
	 * TTG-3: Create HotSoakEventByHour
	**/
	void createHotSoakEventByHour() {
		if(!needHotSoakEventByHour) {
			return;
		}
		needHotSoakEventByHour = false;

		String sql = "";
		PreparedStatement statement = null;
		PreparedStatement statement2 = null;
		ResultSet results = null;
		ResultSet results2 = null;
		try {
			sql = "DROP TABLE IF EXISTS HotSoakEventByHour";
			SQLRunner.executeSQL(db, sql);
			
			sql = "CREATE TABLE HotSoakEventByHour ( "
					+ "vehID INTEGER NOT NULL, "
					+ "tripID SMALLINT NOT NULL, "
					+ "hourDayID SMALLINT NOT NULL, "
					+ "endOfHour INTEGER, "
					+ "hotSoakBegin INTEGER, "
					+ "hotSoakEnd INTEGER, "
					+ "startOfHotSoak TINYINT, "
					+ "endOfHotSoak TINYINT)";
			SQLRunner.executeSQL(db, sql);
			
			sql = "CREATE UNIQUE INDEX XPKHotSoakEventByHour ON HotSoakEventByHour ( "
					+ "vehID ASC, "
					+ "tripID ASC, "
					+ "hourDayID ASC)";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 240
			 * @algorithm Find all hours in which a trip ends.
			 * @input SampleVehicleTripByHour
			 * @output list of trip end hours in memory
			 * @condition endOfTrip=1
			**/
			sql = "SELECT vehID, tripID, hourID, dayID, priorTripID, keyOnTime, keyOffTime, "
					+ "endOfHour "
					+ "FROM SampleVehicleTripByHour svth "
					+ "WHERE endOfTrip = 1 "
					+ "ORDER BY vehID, dayID, tripID";
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			int count = 0;
			if(results!=null) {
				while(results.next()) {
					int vehID = results.getInt(1);
					int tripID = results.getInt(2);
					int hourID = results.getInt(3);
					int dayID = results.getInt(4);
					int priorTripID = results.getInt(5);
					int keyOnTime = results.getInt(6);
					int keyOffTime = results.getInt(7);
					int endOfHour = results.getInt(8);

					/**
					 * @step 240
					 * @algorithm hotSoakBegin = keyOffTime of each trip end.
					 * @input list of trip end hours in memory
					**/
					int hotSoakBegin = keyOffTime;
					int startOfHotSoak = 1;

					/**
					 * @step 240
					 * @algorithm Get keyOnTime for the first hour of each trip's subsequent trip.
					 * @input SampleVehcileTripByHour
					 * @output keyOnTime for the current trip's first hour
					**/
					sql = "SELECT keyOnTime FROM SampleVehicleTripByHour svth " 
							+ "WHERE vehID=" + vehID + " AND priorTripID=" + tripID 
							+ " AND startOfTrip=1"
							+ " AND dayID=" + dayID;
					statement2 = db.prepareStatement(sql);
					results2 = SQLRunner.executeQuery(statement2,sql);
					
					int endOfHotSoak = 0;
					/**
					 * @step 240
					 * @algorithm hotSoakEnd = min(endOfHour, nextKeyOnTime) when there is a next trip, endOfHour otherwise.
					**/
					int hotSoakEnd = endOfHour;
					int nextKeyOnTime = endOfHour;
					boolean runToEndOfDay = false;
					if(results2!=null) {
						if(results2.next()) {
							nextKeyOnTime = results2.getInt(1);
							if(nextKeyOnTime<endOfHour) {
								endOfHotSoak = 1;
								hotSoakEnd = nextKeyOnTime;
							}
						} else { // If no next trip at all
							runToEndOfDay = true;
							/* Old:
							endOfHotSoak = 1;
							results2.close();
							results2 = null;
							continue; // <-- Remove this if want to generate hot soak following last trip for a vehicle.
							*/
						}
						results2.close();
						results2 = null;
					}

					/**
					 * @step 240
					 * @algorithm Record each hot soaking event from hotSoakBegin to hotSoakEnd.
					 * @output HotSoakEventByHour
					**/
					sql = "INSERT INTO HotSoakEventByHour ( "
							+ "vehID, tripID, hourDayID, endOfHour, hotSoakBegin, hotSoakEnd, "
							+ "startOfHotSoak, endOfHotSoak) SELECT ?, ?, hourDayID, ?, ?, ?, "
							+ "?, ? FROM HourDay WHERE hourID = ? AND dayID = ?";
					statement2.close();
					statement2 = db.prepareStatement(sql);

					statement2.setInt(1,vehID);
					statement2.setInt(2,tripID);
					statement2.setInt(3,endOfHour);
					statement2.setInt(4,hotSoakBegin);
					statement2.setInt(5,hotSoakEnd);
					statement2.setInt(6,startOfHotSoak);
					statement2.setInt(7,endOfHotSoak);
					statement2.setInt(8,hourID);
					statement2.setInt(9,dayID);
					count += SQLRunner.executeSQL(statement2, sql);

					startOfHotSoak = 0;
					while(endOfHotSoak==0) {
						if(runToEndOfDay) {
							if(hourID == 24) {
								break; // quit since there is not a next hour in the day
							} else {
								nextKeyOnTime = endOfHour+60+1;
							}
						}
						hourID = hourID==24?1:hourID+1;
						hotSoakBegin = endOfHour + 1;
						endOfHour += 60;
						if(nextKeyOnTime>endOfHour) {
							hotSoakEnd = endOfHour;
						} else {
							hotSoakEnd = nextKeyOnTime;
							endOfHotSoak = 1;
						}

						statement2.setInt(1,vehID);
						statement2.setInt(2,tripID);
						statement2.setInt(3,endOfHour);
						statement2.setInt(4,hotSoakBegin);
						statement2.setInt(5,hotSoakEnd);
						statement2.setInt(6,startOfHotSoak);
						statement2.setInt(7,endOfHotSoak);
						statement2.setInt(8,hourID);
						statement2.setInt(9,dayID);
						count += SQLRunner.executeSQL(statement2, sql);
					}
				}
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine soak times for sample vehicle trips.", sql);
		} finally {
			if(results2!=null) {
				try {
					results2.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				results2 = null;
			}
			if(results!=null) {
				try {
					results.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				results = null;
			}
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement = null;
			}
			if(statement2!=null) {
				try {
					statement2.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement2 = null;
			}
		}
	}

	class TTG4aParams {
		public int monthID = 0;
		public int tankTemperatureGroupID = 0;
		public int vehID = 0;
		public int dayID = 0;
		public int priorTripID = 0;
		public float keyOnTemp = (float)0.0;
		
		public String toString() {
			return "monthID=" + monthID + ", ttgID=" + tankTemperatureGroupID
					+ ", vehID=" + vehID + ", dayID=" + dayID
					+ ", priorTripID=" + priorTripID + ", keyOnTemp=" + keyOnTemp;
		}
	}

	/**
	 * TTG-4a: Calculate Hot Soak and Operating Tank Temperature by Parsed Trip.
	**/
	void calculateHotSoakAndOperatingTankTemperatures() {
		if(!needAverageTankTemperature && !needSoakActivityFraction && !needColdSoakInitialHourFraction) {
			return;
		}
		Logger.log(LogMessageCategory.INFO,"TTG-4a");
		String sql = "";
		PreparedStatement statement = null;
		PreparedStatement statement2 = null;
		ResultSet results = null;
		ArrayList<TTG4aParams> parameters = new ArrayList<TTG4aParams>(50);
				// ArrayList expands as needed, but 50 is ok
		parameters.add(new TTG4aParams());

		File operatingFile = new File("Operating.txt");
		File hotSoakFile = new File("HotSoak.txt");
		PrintWriter operatingWriter = null;
		PrintWriter hotSoakWriter = null;

		try {
			// Try to remove the temporary data files before we fill them, just in case they exist
			if(operatingFile.exists()) {
				operatingFile.delete();
			}
			if(hotSoakFile.exists()) {
				hotSoakFile.delete();
			}

			operatingWriter = new PrintWriter(new BufferedWriter(new FileWriter(operatingFile),128*1024));
			hotSoakWriter = new PrintWriter(new BufferedWriter(new FileWriter(hotSoakFile),1024*1024));

			createHotSoakAndOperatingTemperatureTables();
/*
			sql = "INSERT INTO OperatingTemperature (monthID, tankTemperatureGroupID, "
					+ "vehID, tripID, hourDayID, priorTripID, keyOnTime, "
					+ "keyOffTime, endOfHour, startOfTrip, endOfTrip, keyOnTemp, "
					+ "keyOffTemp) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)";
			statement2 = db.prepareStatement(sql);
*/
			int loopCount = 0;
			int count = 0;
			while(!parameters.isEmpty()) {
				/**
				 * @step 250
				 * @algorithm Collect the list of tank temperature data sets to be iterated.
				 * @output list of tank temperature data sets to be iterated
				 * @input SampleVehicleTripByHour
				 * @input TempVehAndTTG
				 * @input TankTemperatureRise
				 * @input TempColdSoakTankTemperature
				**/
				TTG4aParams p = (TTG4aParams)parameters.remove(0);
				//System.out.println("TTG-4a p: " + p.toString());
				sql = "SELECT tripID, svth.hourDayID, keyOnTime, keyOffTime, endOfHour, "
						+ " startOfTrip, endOfTrip, coldSoakTankTemperature, "
						+ " tankTemperatureRiseTermA, tankTemperatureRiseTermB, "
						+ " monthID, ttr.tankTemperatureGroupID, svth.vehID, priorTripID, dayID "
						+ " FROM SampleVehicleTripByHour svth "
						+ " inner join TempVehAndTTG using (vehID) "
						+ " inner join TankTemperatureRise ttr using (tankTemperatureGroupID), "
						+ " TempColdSoakTankTemperature cstt";
				if(p.priorTripID <= 0) { // Calculate operating temperatures for first trips.
					sql += " WHERE priorTripID=0 "
							+ "AND svth.hourID=cstt.hourID "
							+ "ORDER BY monthID, ttr.tankTemperatureGroupID, "
							+ "svth.vehID, svth.dayID, keyOnTime";
				} else { // Create temperatures for trips that have a prior trip and therefore a hot soak
					sql += " WHERE svth.hourID=cstt.hourID AND "
							+ "monthID=" + p.monthID + " AND ttr.tankTemperatureGroupID=" 
							+ p.tankTemperatureGroupID + " AND svth.vehID=" + p.vehID
							+ " AND priorTripID = " + p.priorTripID
							+ " AND svth.dayID=" + p.dayID
							+ " ORDER BY keyOnTime";
				}
				//System.out.println("TTG-4a sql=" + sql);
				statement = db.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement,sql);
				if(results!=null) {
					float keyOffTemp = p.keyOnTemp;
					float keyOnTemp = p.keyOnTemp;
					while(results.next()) {
						int tripID = results.getInt(1);
						int hourDayID = results.getInt(2);
						int keyOnTime = results.getInt(3);
						int keyOffTime = results.getInt(4);
						int endOfHour = results.getInt(5);
						int startOfTrip = results.getInt(6);
						int endOfTrip = results.getInt(7);
						float coldSoakTankTemperature = results.getFloat(8);
						float tankTemperatureRiseTermA = results.getFloat(9);
						float tankTemperatureRiseTermB = results.getFloat(10);
						int monthID = results.getInt(11);
						int tankTemperatureGroupID = results.getInt(12);
						int vehID = results.getInt(13);
						int priorTripID = results.getInt(14);
						int dayID = results.getInt(15);

						/**
						 * @step 250
						 * @algorithm Calculate operating temperatures.
						 * keyOnTemp = coldSoakTankTemperature when starting a trip and a prior trip exists and it known that hot soaking has already ended.
						 * keyOnTemp = tank temperature when the the trip started when starting a trip and a prior trip exists and hot soaking is not already ended.
						 * keyOnTemp = coldSoakTankTemperature when starting a trip and no prior trip exists.
						 * keyOnTemp = keyOffTemp when a trip is continued into a new hour.
						 * keyOffTemp = keyOnTemp + ((tankTemperatureRiseTermA+tankTemperatureRiseTermB*(95.0-keyOnTemp))/1.2)*((keyOffTime-keyOnTime)/60.0).
						 * keyOffTemp = 140 when keyOffTemp > 140, limiting keyOffTemp to 140 degrees Farenheit.
						 * @input current item from the list of tank temperature data sets to be iterated
						 * @output OperatingTemperature
						**/
						if(startOfTrip == 1) { // New trip
							if(p.priorTripID > 0) {
								if(p.keyOnTemp <= -1000) {
									keyOnTemp = coldSoakTankTemperature;
								} else {
									keyOnTemp = p.keyOnTemp;
								}
							} else {
								keyOnTemp = coldSoakTankTemperature;
							}
						} else { // Continuation of same trip but in new hour
							keyOnTemp = keyOffTemp;
						}
						keyOffTemp = (float)(keyOnTemp + ((tankTemperatureRiseTermA+
								tankTemperatureRiseTermB*(95.0-keyOnTemp))/1.2)*(
								(keyOffTime-keyOnTime)/60.0));
								
						// cap of operating tank temperature added by EPA - Mitch C.		
						if (keyOffTemp > 140.0f) {
							keyOffTemp = 140.0f;
						}

						operatingWriter.println(monthID + "\t"
								+ tankTemperatureGroupID + "\t"
								+ vehID + "\t"
								+ tripID + "\t"
								+ hourDayID + "\t"
								+ priorTripID + "\t"
								+ keyOnTime + "\t"
								+ keyOffTime + "\t"
								+ endOfHour + "\t"
								+ startOfTrip + "\t"
								+ endOfTrip + "\t"
								+ keyOnTemp + "\t"
								+ keyOffTemp);
						count++;

						if(endOfTrip == 1) {
							// Do the hot soak after the trip
							currentTripID = tripID;
							float endHotSoakTemperature = calculateHotSoakTemperaturesForTrip(
									hotSoakWriter, monthID, tankTemperatureGroupID, vehID, 
									hourDayID%10, tripID, keyOffTemp); // step 251-254
							// Arrange to process the trip(s) that happen after the hot soak
							TTG4aParams p2 = new TTG4aParams();
							p2.monthID = monthID;
							p2.tankTemperatureGroupID = tankTemperatureGroupID;
							p2.vehID = vehID;
							p2.dayID = dayID;
							p2.priorTripID = tripID;
							p2.keyOnTemp = endHotSoakTemperature;
							parameters.add(p2);
						}
						
						loopCount++;
						if(0 == (loopCount % 5000)) {
							Logger.log(LogMessageCategory.INFO,"TTG-4a loopCount=" + loopCount);
							System.gc();
						}
					}

					results.close();
					results = null;
				}

				statement.close();
				statement = null;
			}
			Logger.log(LogMessageCategory.INFO,"TTG-4a final loopCount=" + loopCount);

			if(statement != null) {
				statement.close();
				statement = null;
			}

			// Close temporary files so they can be read by the database
			hotSoakWriter.close();
			hotSoakWriter = null;
			operatingWriter.close();
			operatingWriter = null;

			// Load OperatingTemperature
			sql = "LOAD DATA INFILE '" + operatingFile.getCanonicalPath().replace('\\', '/')
					+ "' IGNORE INTO TABLE OperatingTemperature (monthID, tankTemperatureGroupID, "
					+ "vehID, tripID, hourDayID, priorTripID, keyOnTime, "
					+ "keyOffTime, endOfHour, startOfTrip, endOfTrip, keyOnTemp, "
					+ "keyOffTemp)";
			SQLRunner.executeSQL(db, sql);

			// Load HotSoakTemperature
			sql = "LOAD DATA INFILE '" + hotSoakFile.getCanonicalPath().replace('\\', '/')
					+ "' IGNORE INTO TABLE HotSoakTemperature (monthID, "
					+ "tankTemperatureGroupID, vehID, tripID, hourDayID, "
					+ "hotSoakTime, initialTankTemp, soakTankTemp, tempDelta, "
					+ "temperature, coldSoakTemp, opModeID)";
			SQLRunner.executeSQL(db, sql);
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not determine soak times for sample vehicle trips.", sql);
		} catch(IOException e) {
			Logger.logError(e,"File I/O error while determining soak times for sample vehicle trips");
		} finally {
			if(results!=null) {
				try {
					results.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				results = null;
			}
			if(statement2!=null) {
				try {
					statement2.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement2 = null;
			}
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement = null;
			}
			if(hotSoakWriter != null) {
				try {
					hotSoakWriter.close();
				} catch(Exception e) {
					// Failure to close this file should not be an issue
				}
				hotSoakWriter = null;
			}
			if(hotSoakFile != null) {
				try {
					hotSoakFile.delete();
				} catch(Exception e) {
					// Failure to delete this file should not be an issue
				}
				hotSoakFile = null;
			}
			if(operatingWriter != null) {
				try {
					operatingWriter.close();
				} catch(Exception e) {
					// Failure to close this file should not be an issue
				}
				operatingWriter = null;
			}
			if(operatingFile != null) {
				try {
					operatingFile.delete();
				} catch(Exception e) {
					// Failure to delete this file should not be an issue
				}
				operatingFile = null;
			}
		}
		Logger.log(LogMessageCategory.INFO,"TTG-4a Done");
	}
	
	/**
	 * Create Hot Soak and Operating Temperature tables.
	**/
	void createHotSoakAndOperatingTemperatureTables() {
		String sql = "";

		try {
			sql = "DROP TABLE IF EXISTS OperatingTemperature";
			SQLRunner.executeSQL(db, sql);
	
			sql = "CREATE TABLE OperatingTemperature ( "
					+ "monthID SMALLINT NOT NULL, "
					+ "tankTemperatureGroupID SMALLINT NOT NULL, "
					+ "vehID INTEGER NOT NULL, " 
					+ "tripID SMALLINT NOT NULL, "
					+ "hourDayID SMALLINT NOT NULL, "
					+ "priorTripID SMALLINT, "
					+ "keyOnTime INTEGER NOT NULL, "
					+ "keyOffTime INTEGER NOT NULL, "
					+ "endOfHour INTEGER NOT NULL, "
					+ "startOfTrip TINYINT NOT NULL, "
					+ "endOfTrip TINYINT NOT NULL, "
					+ "keyOnTemp FLOAT, "
					+ "keyOffTemp FLOAT,"
					+ "key (monthID, hourDayID, tankTemperatureGroupID))";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE UNIQUE INDEX XPKOperatingTemperature ON OperatingTemperature ("
					+ "monthID ASC, tankTemperatureGroupID ASC, vehID ASC, tripID ASC, "
					+ "hourDayID ASC)";
			SQLRunner.executeSQL(db, sql);

			sql = "drop table if exists TempOperatingTemperatureSummary";
			SQLRunner.executeSQL(db, sql);

			sql = "create table TempOperatingTemperatureSummary ("
					+ " monthID SMALLINT NOT NULL,"
					+ " hourDayID SMALLINT NOT NULL,"
					+ " tankTemperatureGroupID SMALLINT NOT NULL,"
					+ " primary key (monthID, hourDayID, tankTemperatureGroupID)"
					+ " )";
			SQLRunner.executeSQL(db, sql);
	
			sql = "DROP TABLE IF EXISTS HotSoakTemperature";
			SQLRunner.executeSQL(db, sql);
	
			sql = "CREATE TABLE HotSoakTemperature ( "
					+ "monthID SMALLINT NOT NULL, "
					+ "tankTemperatureGroupID SMALLINT NOT NULL, "
					+ "vehID INTEGER NOT NULL, "
					+ "tripID SMALLINT NOT NULL, "
					+ "hourDayID SMALLINT NOT NULL, "
					+ "hotSoakTime INTEGER NOT NULL, "
					+ "initialTankTemp FLOAT, "
					+ "soakTankTemp FLOAT, "
					+ "temperature FLOAT, "
					+ "tempDelta FLOAT,"
					+ "coldSoakTemp FLOAT,"
					+ "opModeID SMALLINT)";
			SQLRunner.executeSQL(db, sql);
	
			sql = "CREATE UNIQUE INDEX XPKHotSoakTemperature ON HotSoakTemperature ("
					+ "monthID ASC, tankTemperatureGroupID ASC, vehID ASC, tripID ASC, "
					+ "hourDayID ASC, hotSoakTime ASC)";
			SQLRunner.executeSQL(db, sql);
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not create hot soak and operating temperature tables.",
					sql);
		}
	}
	
	/**
	 * TTG-4b: Calculate Hot Soak Temperatures
	**/
	private float calculateHotSoakTemperaturesForTrip(PrintWriter hotSoakWriter,
			int monthID, int tankTemperatureGroupID,int vehID, int dayID,
			int tripID, float keyOffTemp) {
		String sql = "";
		PreparedStatement statement = null;
		PreparedStatement statement2 = null;
		PreparedStatement statement3 = null;
		ResultSet results = null;
		ResultSet results2 = null;
		float soakTankTemperature = keyOffTemp;
		try {
			/**
			 * @step 251
			 * @algorithm Get the hot soak events beginning at the end of the current trip.
			 * @input HotSoakEventByHour
			 * @input HourDay
			 * @output list of hot soak events
			**/
			sql = "SELECT hourDayID, endOfHour, hotSoakBegin, hotSoakEnd, startOfHotSoak, "
					+ "endOfHotSoak"
					+ " FROM HotSoakEventByHour"
					+ " INNER JOIN HourDay hd using (hourDayID)"
				 	+ " WHERE vehID=" + vehID
				 	+ " AND dayID=" + dayID
					+ " AND tripID=" + tripID
					+ " ORDER BY hotSoakBegin";
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			int count=0;
			if(results!=null) {
				/**
				 * @step 251
				 * @algorithm Setup hot soak variables for the current vehicle, day, and trip.
				 * tempDeltaSum = 0.
				 * initialTankTemperature = keyOffTemp.
				**/
				float tempDeltaSum = 0;
				float initialTankTemperature = keyOffTemp;

				/**
				 * @step 251
				 * @algorithm Get the ambient temperature and coldSoakTankTemperature for the current
				 * month, zone, hour, and day.
				 * @input TempColdSoakTankTemperature
				 * @input ZoneMonthHour
				 * @input HourDay
				**/
				sql = "SELECT temperature, coldSoakTankTemperature "
						+ "FROM TempColdSoakTankTemperature cstt "
						+ "INNER JOIN ZoneMonthHour zmh ON (zmh.monthID=cstt.monthID AND "
						+ "zmh.hourID=cstt.hourID) "
						+ "INNER JOIN HourDay hd ON (hd.hourID=zmh.hourID) "
						+ "WHERE zmh.monthID = ? "
						+ "AND zmh.zoneID = ? "
						+ "AND hd.hourDayID = ?";
				statement2 = db.prepareStatement(sql);
/*
				sql="INSERT INTO HotSoakTemperature (monthID, "
						+ "tankTemperatureGroupID, vehID, tripID, hourDayID, "
						+ "hotSoakTime, initialTankTemp, soakTankTemp, tempDelta, "
						+ "temperature, coldSoakTemp, opModeID) VALUES ("
						+ "?,?,?,?,?,?,?,?,?,?,?,?)";
				statement3 = db.prepareStatement(sql);
*/
				// For each hour in the hot soak event.
				boolean done = false;
				while(!done && results.next()) {
					int hourDayID = results.getInt(1);
					int endOfHour = results.getInt(2);
					int hotSoakBegin = results.getInt(3);
					int hotSoakEnd = results.getInt(4);
					int startOfHotSoak = results.getInt(5);
					int endOfHotSoak = results.getInt(6);
	
					// Get the ambient and cold soak temperatures for the hour.
					sql = "SELECT temperature, coldSoakTankTemperature "
							+ "FROM TempColdSoakTankTemperature cstt "
							+ "INNER JOIN ZoneMonthHour zmh ON (zmh.monthID=cstt.monthID AND "
							+ "zmh.hourID=cstt.hourID) "
							+ "INNER JOIN HourDay hd ON (hd.hourID=zmh.hourID) "
							+ "WHERE zmh.monthID = ? "
							+ "AND zmh.zoneID = ? "
							+ "AND hd.hourDayID = ?";
					// Note: statement2 has already been created
					statement2.clearParameters();
					statement2.setInt(1,monthID);
					statement2.setInt(2,context.iterLocation.zoneRecordID);
					statement2.setInt(3,hourDayID);

					results2 = SQLRunner.executeQuery(statement2,sql);
					if(results2!=null && results2.next()) {
						float temperature = results2.getFloat(1);
						float coldSoakTankTemperature = results2.getFloat(2);
						results2.close();
						results2 = null;
					
						// Generate the minute-by-minute hot soak temperatures.

						/**
						 * @step 251
						 * @algorithm Generate the minute-by-minute hot soak temperatures.
						 * soakTankTemperature = 1.4*tempDeltaSum/60 + initialTankTemperature.
						 * tempDelta = temperature - soakTankTemperature.
						 * tempDeltaSum = tempDeltaSum + tempDelta.
						 * opModeID = 150 (hot soaking) when soakTankTemperature > coldSoakTankTemperature + 3, 151 (cold soaking) otherwise.
						 * When opModeID=151 (cold soaking), set soakTankTemperature=-1000 and do not record the hot soak minute or any others.
						 * @condition each minute from hotSoakBegin, inclusive, to hotSoakEnd, exclusive.
						 * @output HotSoakTemperature
						**/
						for(int minute=hotSoakBegin;minute<hotSoakEnd;minute++) { // include hotSoakBegin, but not hotSoakEnd
							soakTankTemperature = (float)1.4*tempDeltaSum/(float)60
									+ initialTankTemperature;
							float tempDelta = temperature - soakTankTemperature;
							tempDeltaSum += tempDelta;
							
							int opModeID = (soakTankTemperature>coldSoakTankTemperature+3)?150:151;
							if(opModeID == 151) { // once opModeID drops to cold soaking, 
												  // leave it at cold soaking by quitting the calc
								soakTankTemperature = -1000; // flag that hot soaking ended
								done = true; // flag so the hourly loop stops as well
								break; // quit without even writing the cold soaking record
							}

							hotSoakWriter.println(monthID + "\t"
									+ tankTemperatureGroupID + "\t"
									+ vehID + "\t"
									+ tripID + "\t"
									+ hourDayID + "\t"
									+ minute + "\t"
									+ initialTankTemperature + "\t"
									+ soakTankTemperature + "\t"
									+ tempDelta + "\t"
									+ temperature + "\t"
									+ coldSoakTankTemperature + "\t"
									+ opModeID);
							count++;
						}
					} else {
						currentTripID = -1;
					}
					if(results2 != null) {
						results2.close();
						results2 = null;
					}
				}
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine hot soak temperatures.", sql);
		} finally {
			if(results2!=null) {
				try {
					results2.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				results2 = null;
			}
			if(results!=null) {
				try {
					results.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				results = null;
			}
			if(statement3!=null) {
				try {
					statement3.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement3 = null;
			}
			if(statement2!=null) {
				try {
					statement2.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement2 = null;
			}
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement = null;
			}
		}
		
		return soakTankTemperature;
	}
	
	/**
	 * TTG-5: Calculate AverageTankTemperature CMIT
	**/
	void calculateAverageTankTemperature() {
		if(!needAverageTankTemperature) {
			return;
		}
		needAverageTankTemperature = false;

		String sql = "";

		try {
			sql = "DROP TABLE IF EXISTS AverageTankTemperatureKeys";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 260
			 * @algorithm Find AverageTankTemperature entries provided by the user as these should
			 * not be replaced with calculated values.
			 * @output AverageTankTemperatureKeys
			 * @input AverageTankTemperature
			**/
			sql = "CREATE TABLE AverageTankTemperatureKeys SELECT DISTINCT tankTemperatureGroupID, "
					+ "zoneID, monthID FROM AverageTankTemperature WHERE isUserInput='Y'";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE UNIQUE INDEX XPKAverageTankTemperatureKeys ON AverageTankTemperatureKeys ( "
					+ "tankTemperatureGroupID ASC, zoneID ASC, monthID ASC)";
			SQLRunner.executeSQL(db, sql);

			sql = "truncate TempOperatingTemperatureSummary";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 260
			 * @algorithm Find all unique combinations of monthID, hourDayID, tankTemperatureGroupID
			 * in the operating temperature table.
			 * @output TempOperatingTemperatureSummary
			 * @input OperatingTemperature
			**/
			sql = "insert into TempOperatingTemperatureSummary (monthID, hourDayID, tankTemperatureGroupID)"
					+ " select distinct monthID, hourDayID, tankTemperatureGroupID"
					+ " from OperatingTemperature";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 260
			 * @algorithm averageTankTemperature[opModeID=151 cold soaking]=coldSoakTankTemperature.
			 * @output AverageTankTemperature
			 * @input TempOperatingTemperatureSummary
			 * @input TempColdSoakTankTemperature
			 * @input HourDay
			 * @input AverageTankTemperatureKeys
			 * @condition No matching entries in AverageTankTemperatureKeys
			**/
			sql="INSERT INTO AverageTankTemperature (zoneID, monthID, hourDayID, "
					+ "tankTemperatureGroupID, opModeID, averageTankTemperature, isUserInput) "
					+ "SELECT "+ context.iterLocation.zoneRecordID + " AS ZoneID, ot.monthID, "
					+ "ot.hourDayID, ot.tankTemperatureGroupID, 151 AS opModeID, "
					+ "coldSoakTankTemperature AS averageTankTemperature, 'N' "
					+ "FROM TempOperatingTemperatureSummary ot INNER JOIN TempColdSoakTankTemperature cstt "
					+ "ON (cstt.monthID=ot.monthID) INNER JOIN HourDay hd ON "
					+ "(hd.hourDayID=ot.hourDayID AND hd.hourID=cstt.hourID) "
					+ "LEFT JOIN AverageTankTemperatureKeys attk ON ("
					+ "attk.tankTemperatureGroupID=ot.tankTemperatureGroupID AND "
					+ "attk.zoneID="+ context.iterLocation.zoneRecordID + " AND attk.monthID=ot.monthID) "
					+ "WHERE attk.tankTemperatureGroupID IS NULL "
					+ "GROUP BY ot.monthID, ot.hourDayID, ot.tankTemperatureGroupID";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 260
			 * @algorithm averageTankTemperature[opModeID=300 all running]=SUM((keyOffTime-keyOnTime)*(keyOnTemp+keyOffTemp)/2.0)/SUM(keyOffTime-keyOnTime).
			 * @output AverageTankTemperature
			 * @input OperatingTemperature
			 * @input AverageTankTemperatureKeys
			 * @condition No matching entries in AverageTankTemperatureKeys
			**/
			sql = "INSERT INTO AverageTankTemperature (zoneID, monthID, hourDayID, "
					+ "tankTemperatureGroupID, opModeID, averageTankTemperature, isUserInput) "
					+ "SELECT " + context.iterLocation.zoneRecordID 
					+ " AS ZoneID, ot.monthID, hourDayID, "
					+ "ot.tankTemperatureGroupID, 300 AS opModeID, "
					+ "SUM((keyOffTime-keyOnTime)*(keyOnTemp+keyOffTemp)/2.0)/SUM(keyOffTime-keyOnTime), "
					+ "'N' "
					+ "FROM OperatingTemperature ot "
					+ "LEFT JOIN AverageTankTemperatureKeys attk ON ("
					+ "attk.tankTemperatureGroupID=ot.tankTemperatureGroupID AND "
					+ "attk.zoneID="+ context.iterLocation.zoneRecordID + " AND attk.monthID=ot.monthID) "
					+ "WHERE attk.tankTemperatureGroupID IS NULL "
					+ "GROUP BY ot.monthID, hourDayID, tankTemperatureGroupID";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 260
			 * @algorithm averageTankTemperature[opModeID=150 hot soaking]=average(soakTankTemp).
			 * @output AverageTankTemperature
			 * @input HotSoakTemperature
			 * @input AverageTankTemperatureKeys
			 * @condition No matching entries in AverageTankTemperatureKeys
			**/
			sql="INSERT INTO AverageTankTemperature (zoneID, monthID, hourDayID, "
					+"tankTemperatureGroupID, opModeID, averageTankTemperature, isUserInput) "
					+ "SELECT " + context.iterLocation.zoneRecordID 
					+ " AS ZoneID, hst.monthID, hourDayID, hst.tankTemperatureGroupID, 150 AS opModeID, "
					+ "AVG(soakTankTemp), 'N' "
					+ "FROM HotSoakTemperature hst "
					+ "LEFT JOIN AverageTankTemperatureKeys attk ON ("
					+ "attk.tankTemperatureGroupID=hst.tankTemperatureGroupID AND "
					+ "attk.zoneID="+ context.iterLocation.zoneRecordID + " AND attk.monthID=hst.monthID) "
					+ "WHERE attk.tankTemperatureGroupID IS NULL "
					+ "GROUP BY hst.monthID, hst.tankTemperatureGroupID, hourDayID";
			SQLRunner.executeSQL(db, sql);
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine average tank temperature.", sql);
		}
	}

	/**
	 * Build TTGeMinutes table used by TTG-6
	**/
	void buildTTGeMinutes() {
		String[] statements = {
			"drop table if exists TTGeMinutes",

			"create table TTGeMinutes ("
					+ " sourceTypeID smallint(6) not null,"
					+ " monthID smallint(6) not null,"
					+ " hourDayID smallint(6) not null,"
					+ " eMinutes int not null,"
					+ " key(sourceTypeID),"
					+ " key(monthID),"
					+ " key(hourDayID)"
					+ " )",

			"drop table if exists TTGeTemp",

			/**
			 * @step 220
			 * @algorithm vehicleCount = count(*) for each source type and day.
			 * @output TTGeTemp
			 * @input sampleVehicleDay
			**/
			"create table TTGeTemp"
					+ " select sourceTypeID, dayID, count(*) as vehicleCount"
					+ " from sampleVehicleDay"
					+ " group by sourceTypeID, dayID",

			/**
			 * @step 220
			 * @algorithm eMinutes = 60*vehicleCount.
			 * @input TTGeTemp
			 * @output TTGeMinutes
			 * @input HourDay
			 * @input runSpecHourDay
			 * @input runSpecMonth
			**/
			"insert into TTGeMinutes (sourceTypeID, monthID, hourDayID, eMinutes)"
					+ " select sourceTypeID, monthID, rshd.hourDayID, 60*vehicleCount"
					+ " from TTGeTemp"
					+ " inner join HourDay hd on (hd.dayID=TTGeTemp.dayID)"
					+ " inner join runSpecHourDay rshd on (rshd.hourDayID=hd.hourDayID),"
					+ " runSpecMonth",

			"drop table TTGeTemp",

			null
		};
		String sql = "";

		try {
			for(int i=0;statements[i] != null;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db, sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not build TTGeMinutes table.", sql);
		}
	}

	/**
	 * TTG-6: Calculate Soak Activity Fraction
	**/
	void calculateSoakActivityFraction() {
		if(!needSoakActivityFraction) {
			return;
		}
		needSoakActivityFraction = false;

		String[] statements = {
			"DROP TABLE IF EXISTS HotSoakOpModeCount",
			"CREATE TABLE HotSoakOpModeCount (sourceTypeID SMALLINT NOT NULL, "
					+ "monthID SMALLINT NOT NULL, hourDayID SMALLINT NOT NULL, "
					+ "	hotSoakOpModeCount SMALLINT,"
					+ " key (monthID, hourDayID, sourceTypeID),"
					+ " key (monthID, sourceTypeID, hourDayID))",
			"CREATE UNIQUE INDEX XPKHotSoakOpModeCount ON HotSoakOpModeCount ( "
					+ "sourceTypeID ASC, monthID ASC, hourDayID ASC)",

			/**
			 * @step 270
			 * hotSoakOpModeCount = count(distinct vehID, hotSoakTime).
			 * @output HotSoakOpModeCount
			 * @input HotSoakTemperature
			 * @input SampleVehicleDay
			 * @condition opModeID=150 hot soaking
			**/

			// Note: we know vehID to be unique within sampleVehicleDay, so there is no
			// ----- need to join to dayID and therefore, no need to bring in HourDay.
			"INSERT INTO HotSoakOpModeCount SELECT sourceTypeID, monthID, "
					+ "hst.hourDayID, COUNT(DISTINCT hst.vehID, hotSoakTime) "
					+ "FROM HotSoakTemperature hst "
					+ "INNER JOIN SampleVehicleDay sv ON (sv.vehID=hst.vehID) "
					+ "WHERE opModeID = 150 " // in theory, all records should already
											  // be hot soaking, so this is just in case.
					+ "GROUP BY sourceTypeID, monthID, hst.hourDayID",
			"DROP TABLE IF EXISTS SoakActivityFractionKeys",
			"CREATE TABLE SoakActivityFractionKeys SELECT DISTINCT sourceTypeID, "
					+ "zoneID, monthID FROM SoakActivityFraction WHERE isUserInput='Y'",
			"CREATE UNIQUE INDEX XPKSoakActivityFractionKeys ON SoakActivityFractionKeys ( "
					+ "sourceTypeID ASC, zoneID ASC, monthID ASC)",

			"drop table if exists TTGoMinutes",

			"create table TTGoMinutes ("
					+ " sourceTypeID smallint(6) not null,"
					+ " monthID smallint(6) not null,"
					+ " hourDayID smallint(6) not null,"
					+ " oMinutes int not null,"
					+ " key(sourceTypeID),"
					+ " key(monthID),"
					+ " key(hourDayID)"
					+ " )",

			/**
			 * @step 270
			 * oMinutes = sum(keyOffTime-keyOnTime).
			 * @output TTGoMinutes
			 * @input sampleVehicleTripByHour
			 * @input sampleVehicleDay
			 * @input runSpecMonth
			**/
			"insert into TTGoMinutes (sourceTypeID, monthID, hourDayID, oMinutes)"
					+ " select sourceTypeID, monthID, hourDayID,sum(keyOffTime-keyOnTime)"
					+ " from sampleVehicleTripByHour svth"
					+ " inner join sampleVehicleDay using (vehID, dayID),"
					+ " runSpecMonth"
					+ " group by sourceTypeID, monthID, hourDayID"
					+ " order by null",

			/**
			 * @step 270
			 * @algorithm soakActivityFraction[opModeID=150 hot soaking]=hotSoakOpModeCount / (eMinutes - oMinutes).
			 * @output SoakActivityFraction
			 * @input HotSoakOpModeCount
			 * @input TTGeMinutes
			 * @input TTGoMinutes
			 * @input SoakActivityFractionKeys
			**/
			"insert into SoakActivityFraction (sourceTypeID, zoneID, monthID,"
					+ " hourDayID, opModeID, soakActivityFraction, isUserInput)"
					+ " select d.sourceTypeID, "
					+ context.iterLocation.zoneRecordID + " AS zoneID, d.monthID, "
					+ " d.hourDayID, 150 AS OpModeID, "
					+ " 1.0*hotSoakOpModeCount / (1.0*eMinutes - coalesce(1.0*oMinutes,0.0)),"
					+ " 'N' as isUserInput"
					+ " from HotSoakOpModeCount d"
					+ " inner join TTGeMinutes e on (e.sourceTypeID=d.sourceTypeID"
					+ " and e.monthID=d.monthID and e.hourDayID=d.hourDayID)"
					+ " left join TTGoMinutes o on (o.sourceTypeID=e.sourceTypeID"
					+ " and o.monthID=e.monthID and o.hourDayID=e.hourDayID)"
					+ " left join SoakActivityFractionKeys safk on (safk.sourceTypeID="
					+ " e.sourceTypeID and safk.zoneID= " + context.iterLocation.zoneRecordID
					+ " and safk.monthID=e.monthID)"
					+ " where safk.sourceTypeID is null",

			/**
			 * @step 270
			 * @algorithm soakActivityFraction[opModeID=151 cold soaking]=(eMinutes-oMinutes-hotSoakOpModeCount) / (eMinutes - oMinutes).
			 * @output SoakActivityFraction
			 * @input HotSoakOpModeCount
			 * @input TTGeMinutes
			 * @input TTGoMinutes
			 * @input SoakActivityFractionKeys
			**/
			"insert into SoakActivityFraction (sourceTypeID, zoneID, monthID,"
					+ " hourDayID, opModeID, soakActivityFraction, isUserInput)"
					+ " select e.sourceTypeID, "
					+ context.iterLocation.zoneRecordID + " AS zoneID, e.monthID, "
					+ " e.hourDayID, 151 AS OpModeID, "
					+ " 1.0*(eMinutes-coalesce(oMinutes,0)-coalesce(hotSoakOpModeCount,0))"
					+ " / (1.0*eMinutes - coalesce(1.0*oMinutes,0.0)) as soakActivityFraction,"
					+ " 'N' as isUserInput"
					+ " from TTGeMinutes e"
					+ " left join HotSoakOpModeCount h on (h.sourceTypeID=e.sourceTypeID"
					+ " and h.monthID=e.monthID and h.hourDayID=e.hourDayID)"
					+ " left join TTGoMinutes o on (o.sourceTypeID=e.sourceTypeID"
					+ " and o.monthID=e.monthID and o.hourDayID=e.hourDayID)"
					+ " left join SoakActivityFractionKeys safk on (safk.sourceTypeID="
					+ " e.sourceTypeID and safk.zoneID= " + context.iterLocation.zoneRecordID
					+ " and safk.monthID=e.monthID)"
					+ " where safk.sourceTypeID is null",

			null
		};
		String sql = "";

		try {
			for(int i=0;statements[i] != null;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db, sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine soak activity fraction.", sql);
		}
	}

	// Cursor data for iterating over SampleVehicleTripByHour
	boolean svthHasData = false;
	int svthVehID = 0;
	int svthHourDayID = 0;
	int svthMinKeyOnTime = 0;
	int svthSum = 0;
	int svthNextVehID = 0;
	int svthNextHourDayID = 0;
	// vehID, hourDayID, keyOnTime, keyOffTime
	int[] svthRow = new int[4];
	int svthRowCount = 0;
	File svthCursorFile = new File("SVTHCursor.txt");
	BufferedReader svthReader = null;

	// Cursor data for iterating over HotSoakTemperature
	boolean hotSoakHasData = false;
	int hotSoakVehID = 0;
	int hotSoakHourDayID = 0;
	int[/*monthID*/][/*ttgID*/] hotSoakMinTime = null;
	int[/*monthID*/][/*ttgID*/] hotSoakCount = null;
	int hotSoakNextVehID = 0;
	int hotSoakNextHourDayID = 0;
	// vehID, hourDayID, hotSoakTime, tankTemperatureGroupID, monthID
	int[] hotSoakRow = new int[5];
	int hotSoakRowCount = 0;
	File hotSoakCursorFile = new File("HotSoakCursor.txt");
	BufferedReader hotSoakReader = null;

	// Arrays of data items
	int[] vehIDs = null;
	int[] sourceTypeIDForVehID = null;
	int[] dayIDforVehID = null;
	int[] sourceTypeIDs = null;
	int[] hourDayIDs = null;
	int[] hourIDForHourDayID = null;
	int[] dayIDForHourDayID = null;
	int[] monthIDs = null;
	int[] ttgIDs = null;

	int[/*sourceTypeID*/][/*monthID*/][/*hourDayID*/][/*initialHourDayID*/] coldSoakMinutes = null;
	int[/*sourceTypeID*/][/*monthID*/][/*hourDayID*/] coldSoakMinutesSum = null;

	/** key is sourceTypeID, data is 0-based index into sourceTypeIDs array **/
	TreeMap<Integer,Integer> sourceTypeIndexes = null;
	/** key is monthID, data is 0-based index into monthIDs array **/
	TreeMap<Integer,Integer> monthIndexes = null;
	/** key is ttgID, data is 0-based index into ttgIDs array **/
	TreeMap<Integer,Integer> ttgIndexes = null;
	/**
	 * set of valid vehicle ID and tank temperature group IDs in TempVehAndTTG.
	 * Data is a string formated as vehID+"|"+ttgID
	**/
	TreeSet<String> vehTTGs = null;

	/** Write debugging data for the SampleVehicleTripByHour and HotSoakTemperature cursors **/
	void writeCursorDebugData() {
		String t = "SVTH Cursor Data:\n"
			+ "svthHasData=" + svthHasData + "\n"
			+ "svthVehID=" + svthVehID + "\n"
			+ "svthHourDayID=" + svthHourDayID + "\n"
			+ "svthMinKeyOnTime=" + svthMinKeyOnTime + "\n"
			+ "svthSum=" + svthSum + "\n"
			+ "svthNextVehID=" + svthNextVehID + "\n"
			+ "svthNextHourDayID=" + svthNextHourDayID + "\n"
			+ "svthReader=" + (svthReader==null?"NULL":"Not NULL");
		Logger.log(LogMessageCategory.INFO,t);

		t = "Hot Soak Cursor Data:\n"
			+ "hotSoakHasData=" + hotSoakHasData + "\n"
			+ "hotSoakVehID=" + hotSoakVehID + "\n"
			+ "hotSoakHourDayID=" + hotSoakHourDayID + "\n"
			+ "hotSoakNextVehID=" + hotSoakNextVehID + "\n"
			+ "hotSoakNextHourDayID=" + hotSoakNextHourDayID + "\n"
			+ "hotSoakReader=" + (hotSoakReader==null?"NULL":"Not NULL") + "\n";
		for(int mIndex=0;mIndex<monthIDs.length;mIndex++) {
			for(int tIndex=0;tIndex<ttgIDs.length;tIndex++) {
				t += "hotSoakMinTime["+mIndex+"]["+tIndex+"]=" + hotSoakMinTime[mIndex][tIndex] + "\n";
				t += "hotSoakCount["+mIndex+"]["+tIndex+"]=" + hotSoakCount[mIndex][tIndex] + "\n";
			}
		}
		Logger.log(LogMessageCategory.INFO,t);
	}

	/**
	 * Construct a mapping from value to 0-based index
	 * @param values integers to be examined
	 * @return map, keyed by values, with data being 0-based indexes into the values array
	**/
	TreeMap<Integer,Integer> buildMapping(int[] values) {
		TreeMap<Integer,Integer> m = new TreeMap<Integer,Integer>();
		for(int i=0;i<values.length;i++) {
			m.put(new Integer(values[i]),new Integer(i));
		}
		return m;
	}

	/**
	 * Get an array of integer IDs from the database.
	 * @param countSQL SQL for obtaining the number of entries in the database
	 * @param dataSQL SQL for obtaining the entries in the desired sequence
	 * @return a sequenced set of integers from the database
	**/
	int[] getIDs(String countSQL,String dataSQL) {
		String sql = "";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			int count, index;

			sql = countSQL;
			count = (int)SQLRunner.executeScalar(db,sql);
			int[] ids = new int[count];
			sql = dataSQL;
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			index = 0;
			while(results.next() && index < count) {
				ids[index++] = results.getInt(1);
			}
			return ids;
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to fill lookup array", sql);
			return null;
		} finally {
			if(results!=null) {
				try {
					results.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
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

	/** Fill all lookup arrays, such as vehIDs, sourceTypeIDs, etc as well as mappings. **/
	void buildArrays() {
		String sql = "";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			int count, index;

			sql = "select count(vehID) from SampleVehicleDay";
			count = (int)SQLRunner.executeScalar(db,sql);
			vehIDs = new int[count];
			sourceTypeIDForVehID = new int[count];
			dayIDforVehID = new int[count];
			sql = "select vehID, sourceTypeID, dayID from SampleVehicleDay order by vehID";
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			index = 0;
			while(results.next()) {
				vehIDs[index] = results.getInt(1);
				sourceTypeIDForVehID[index] = results.getInt(2);
				dayIDforVehID[index] = results.getInt(3);
				index++;
			}
			results.close();
			results = null;
			statement.close();
			statement = null;

			/**
			 * @step 280
			 * @algorithm Read TempVehAndTTG into memory.
			 * @input TempVehAndTTG
			 * @output list of vehID, tankTemperatureGroupID in memory
			**/
			vehTTGs = new TreeSet<String>();
			sql = "select vehID, tankTemperatureGroupID from TempVehAndTTG";
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			index = 0;
			while(results.next()) {
				vehTTGs.add(results.getInt(1) + "|" + results.getInt(2));
			}
			results.close();
			results = null;
			statement.close();
			statement = null;

			sourceTypeIDs = getIDs("select count(distinct sourceTypeID) from SampleVehicleDay",
					"select distinct sourceTypeID from SampleVehicleDay order by sourceTypeID");
			hourDayIDs = getIDs("select count(*) from HourDay",
					"select hourDayID from HourDay order by hourDayID");
			hourIDForHourDayID = getIDs("select count(*) from HourDay",
					"select hourID from HourDay order by hourDayID");
			dayIDForHourDayID = getIDs("select count(*) from HourDay",
					"select dayID from HourDay order by hourDayID");
			monthIDs = getIDs("select count(*) from RunSpecMonth",
					"select monthID from RunSpecMonth order by monthID");
			ttgIDs = getIDs("select count(*) from TankTemperatureGroup",
					"select tankTemperatureGroupID from TankTemperatureGroup order by tankTemperatureGroupID");

			sourceTypeIndexes = buildMapping(sourceTypeIDs);
			monthIndexes = buildMapping(monthIDs);
			ttgIndexes = buildMapping(ttgIDs);

			coldSoakMinutes = new int[sourceTypeIDs.length][monthIDs.length]
					[hourDayIDs.length][hourDayIDs.length]; // all should be initialized to 0
			coldSoakMinutesSum = new int[sourceTypeIDs.length][monthIDs.length]
					[hourDayIDs.length]; // all should be initialized to 0

			hotSoakMinTime = new int[monthIDs.length][ttgIDs.length];
			hotSoakCount = new int[monthIDs.length][ttgIDs.length];
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to fill lookup arrays", sql);
		} finally {
			if(results!=null) {
				try {
					results.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
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

	/** Open svtdData and setup initial variables for iterating over SampleVehicleTripByHour **/
	void openSvthCursor() {
		try {
			if(svthCursorFile.exists()) {
				svthCursorFile.delete();
			}
		} catch(Exception e) {
			// Nothing to do here
		}
		String sql = "";
		try {
			/**
			 * @step 280
			 * @algorithm Read SampleVehicleTripByHour into memory.
			 * @input SampleVehicleTripByHour
			 * @output list of vehID, hourDayID, keyOnTime, keyOffTime in memory
			**/
			sql = "select vehID, hourDayID, keyOnTime, keyOffTime"
					+ " into outfile '" + svthCursorFile.getCanonicalPath().replace('\\', '/') + "'"
					+ " from SampleVehicleTripByHour"
					+ " order by vehID, hourDayID, keyOnTime";
			svthHasData = false;
			svthRowCount = 0;
			SQLRunner.executeSQL(db,sql);

			svthReader = new BufferedReader(new FileReader(svthCursorFile));

			if(svthReadNextRow()) {
				svthNextVehID = svthRow[1-1];
				svthNextHourDayID = svthRow[2-1];
				advanceSvthCursor();
			} else {
				svthReader.close();
				svthReader = null;
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to open SVTH cursor", sql);
		}
	}

	/**
	 * Read the next row from svthReader and place it into svthRow[]
	 * @return true if a row was read and decoded
	**/
	boolean svthReadNextRow() {
		try {
			String line = svthReader.readLine();
			if(line == null || line.length() <= 0) {
				Logger.log(LogMessageCategory.INFO,"EOF svth cursor#1, rows=" + svthRowCount);
				return false;
			}
			String[] parts = line.split("\\s");
			for(int i=0;i<svthRow.length;i++) {
				if(i < parts.length && parts[i].length() > 0) {
					try {
						svthRow[i] = Integer.parseInt(parts[i]);
					} catch(Exception e) {
						Logger.log(LogMessageCategory.INFO,"EOF svth cursor#2, rows=" + svthRowCount);
						return false;
					}
				} else {
					svthRow[0] = 0;
				}
			}
			svthRowCount++;
			return true;
		} catch(Exception e) {
			// Nothing to do here
		}
		return false;
	}

	/** Move svtdData to the next set of records **/
	void advanceSvthCursor() {
		try {
			if(svthReader == null) {
				svthHasData = false;
				return;
			}
			svthVehID = svthNextVehID;
			svthHourDayID = svthNextHourDayID;
			svthHasData = true;
			svthMinKeyOnTime = svthRow[3-1]; // 3 == KeyOnTime
			svthSum = svthRow[4-1] - svthMinKeyOnTime; // 4 == KeyOffTime
			while(svthReadNextRow()) {
				svthNextVehID = svthRow[1-1];
				svthNextHourDayID = svthRow[2-1];
				if(svthNextVehID != svthVehID || svthNextHourDayID != svthHourDayID) {
					return;
				}
				svthSum += svthRow[4-1] - svthRow[3-1]; // KeyOffTime - KeyOnTime
			}
			// Out of data before change to another vehicle or hour
			svthReader.close();
			svthReader = null;
		} catch(Exception e) {
			Logger.logError(e,"Unable to advance SVTH cursor");
		}
	}

	/** Open hotSoakReader and setup initial variables for iterating over HotSoakTemperature **/
	void openHotSoakCursor() {
		try {
			if(hotSoakCursorFile.exists()) {
				hotSoakCursorFile.delete();
			}
		} catch(Exception e) {
			// Nothing to do here
		}
		String sql = "";
		try {
			/**
			 * @step 280
			 * @algorithm Read hot soak vehID, hourDayID, hotSoakTime, tankTemperatureGroupID, monthID combinations into memory.
			 * @input HotSoakTemperature
			 * @output list of objects in memory
			**/
			sql = "select vehID, hourDayID, hotSoakTime, tankTemperatureGroupID, monthID"
					+ " into outfile '" + hotSoakCursorFile.getCanonicalPath().replace('\\', '/') + "'"
					+ " from HotSoakTemperature"
					+ " where opModeID = 150" // should all be 150 (hot soak) already
					+ " order by vehID, hourDayID, hotSoakTime, tankTemperatureGroupID, monthID";
			hotSoakHasData = false;
			hotSoakRowCount = 0;
			SQLRunner.executeSQL(db,sql);
			hotSoakReader = new BufferedReader(new FileReader(hotSoakCursorFile));
			if(hotSoakReadNextRow()) {
				hotSoakNextVehID = hotSoakRow[1-1];
				hotSoakNextHourDayID = hotSoakRow[2-1];
				advanceHotSoakCursor();
			} else {
				hotSoakReader.close();
				hotSoakReader = null;
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to open hot soak cursor", sql);
		}
	}

	/**
	 * Read the next row from hotSoakReader and place it into hotSoakRow[]
	 * @return true if a row was read and decoded
	**/
	boolean hotSoakReadNextRow() {
		try {
			String line = hotSoakReader.readLine();
			if(line == null || line.length() <= 0) {
				Logger.log(LogMessageCategory.INFO,"EOF hotSoak cursor#1, rows=" + hotSoakRowCount);
				return false;
			}
			String[] parts = line.split("\\s");
			for(int i=0;i<hotSoakRow.length;i++) {
				if(i < parts.length && parts[i].length() > 0) {
					try {
						hotSoakRow[i] = Integer.parseInt(parts[i]);
					} catch(Exception e) {
						Logger.log(LogMessageCategory.INFO,"EOF hotSoak cursor#2, rows=" + hotSoakRowCount);
						return false;
					}
				} else {
					hotSoakRow[0] = 0;
				}
			}
			hotSoakRowCount++;
			return true;
		} catch(Exception e) {
			// Nothing to do here
		}
		return false;
	}

	/** Move hotSoakReader to the next set of records **/
	void advanceHotSoakCursor() {
		// Clear arrays
		for(int m=0;m<monthIDs.length;m++) {
			for(int t=0;t<ttgIDs.length;t++) {
				hotSoakMinTime[m][t] = 999999; // a number beyond the max possible in a day
				hotSoakCount[m][t] = 0; // will be at least 1 if there is any data
			}
		}
		try {
			if(hotSoakReader == null) {
				hotSoakHasData = false;
				return;
			}
			hotSoakVehID = hotSoakNextVehID;
			hotSoakHourDayID = hotSoakNextHourDayID;
			hotSoakHasData = true;
			int hotSoakTime = hotSoakRow[3-1];
			int ttgID = hotSoakRow[4-1];
			int monthID = hotSoakRow[5-1];
			// Get array indexes from month and ttg ID values
			Integer monthInteger = new Integer(monthID);
			Integer ttgInteger = new Integer(ttgID);
			int monthIndex = ((Integer)monthIndexes.get(monthInteger)).intValue();
			int ttgIndex = ((Integer)ttgIndexes.get(ttgInteger)).intValue();
			// Record the first minute of hot soaking within the hour and the number of minutes
			if(hotSoakTime < hotSoakMinTime[monthIndex][ttgIndex]) {
				hotSoakMinTime[monthIndex][ttgIndex] = hotSoakTime;
			}
			hotSoakCount[monthIndex][ttgIndex]++;

			while(hotSoakReadNextRow()) {
				hotSoakNextVehID = hotSoakRow[1-1];
				hotSoakNextHourDayID = hotSoakRow[2-1];
				if(hotSoakNextVehID != hotSoakVehID || hotSoakNextHourDayID != hotSoakHourDayID) {
					return;
				}
				hotSoakTime = hotSoakRow[3-1];
				ttgID = hotSoakRow[4-1];
				monthID = hotSoakRow[5-1];
				// Get array indexes from month and ttg ID values
				if(monthInteger == null || monthInteger.intValue() != monthID) {
					monthInteger = new Integer(monthID);
				}
				if(ttgInteger == null || ttgInteger.intValue() != ttgID) {
					ttgInteger = new Integer(ttgID);
				}
				monthIndex = ((Integer)monthIndexes.get(monthInteger)).intValue();
				ttgIndex = ((Integer)ttgIndexes.get(ttgInteger)).intValue();
				// Record the first minute of hot soaking within the hour and the number of minutes
				if(hotSoakTime < hotSoakMinTime[monthIndex][ttgIndex]) {
					hotSoakMinTime[monthIndex][ttgIndex] = hotSoakTime;
				}
				hotSoakCount[monthIndex][ttgIndex]++;
			}
			// Out of data before change to another vehicle or hour
			hotSoakReader.close();
			hotSoakReader = null;
		} catch(Exception e) {
			Logger.logError(e,"Unable to advance hot soak cursor");
		}
	}

	/**
	* Get sourceTypeID and monthID combinations that already exist in
	* ColdSoakInitialHourFraction for the current zone
	* @return a set of strings of the format sourceTypeID + "|" + monthID
	**/
	TreeSet getUserInputKeys() {
		TreeSet<String> userInputs = new TreeSet<String>();
		String sql = "select distinct sourceTypeID, monthID "
				+ " from ColdSoakInitialHourFraction"
				+ " where isUserInput='Y'"
				+ " and zoneID = " + context.iterLocation.zoneRecordID;
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			while(results.next()) {
				int sourceTypeID = results.getInt(1);
				int monthID = results.getInt(2);
				String key = sourceTypeID + "|" + monthID;
				userInputs.add(key);
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to find user inputs for ColdSoakInitialHourFraction",sql);
		} finally {
			if(results!=null) {
				try {
					results.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				results = null;
			}
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
				statement = null;
			}
		}
		return userInputs;
	}

	/**
	 * TTG-7: Calculate Cold Soak Initial Hour Fractions
	**/
	void calculateColdSoakInitialHourFractions() {
		if(!needColdSoakInitialHourFraction) {
			return;
		}
		needColdSoakInitialHourFraction = false;

		buildArrays();
		openSvthCursor();
		openHotSoakCursor();

		int[/*monthID*/][/*ttgID*/] initialHourDayIndex = new int[monthIDs.length][ttgIDs.length];
		int sIndex;
		boolean hasSVTH, hasHotSoakPartial, hasHotSoak, shouldResetInitialHour = false;
		int minutesColdSoakStartingFromInitialHour;
		int minutesColdSoakStartingInCurrentHour;
		for(int vIndex=0;vIndex<vehIDs.length;vIndex++) {
			sIndex = ((Integer)sourceTypeIndexes.get(new Integer(sourceTypeIDForVehID[vIndex]))).intValue();
			while(svthHasData && svthVehID < vehIDs[vIndex]) {
				advanceSvthCursor();
			}
			while(hotSoakHasData && hotSoakVehID < vehIDs[vIndex]) {
				advanceHotSoakCursor();
			}
			for(int hIndex=0;hIndex<hourDayIDs.length;hIndex++) {
				if(dayIDForHourDayID[hIndex] != dayIDforVehID[vIndex]) { // Skip entries not for the correct day
					continue;
				}
				while(svthHasData && svthVehID == vehIDs[vIndex]
						&& svthHourDayID < hourDayIDs[hIndex]) {
					advanceSvthCursor();
				}
				hasSVTH = svthHasData && svthVehID == vehIDs[vIndex]
						&& svthHourDayID == hourDayIDs[hIndex];

				while(hotSoakHasData && hotSoakVehID == vehIDs[vIndex]
						&& hotSoakHourDayID < hourDayIDs[hIndex]) {
					advanceHotSoakCursor();
				}
				hasHotSoakPartial = hotSoakHasData && hotSoakVehID == vehIDs[vIndex] 
						&& hotSoakHourDayID == hourDayIDs[hIndex];

				// Track initialHour by month and ttg, reseting to 1st hourDayID
				// of the day every time we start a new day
				if(hourIDForHourDayID[hIndex] == 1) {
					for(int m=0;m<monthIDs.length;m++) {
						for(int t=0;t<ttgIDs.length;t++) {
							initialHourDayIndex[m][t] = hIndex;
						}
					}
				}

				boolean shouldLog = false; // hourDayIDs[hIndex] == 121; //  && vIndex == 1;

				for(int tIndex=0;tIndex<ttgIDs.length;tIndex++) {
					// skip vehID/ttgID not present in TempVehAndTTG
					if(!vehTTGs.contains(vehIDs[vIndex]+"|"+ttgIDs[tIndex])) {
						continue;
					}

					for(int mIndex=0;mIndex<monthIDs.length;mIndex++) {
						hasHotSoak = hasHotSoakPartial && hotSoakCount[mIndex][tIndex] > 0;
						if(shouldLog) {
							Logger.log(LogMessageCategory.INFO,"vehIDs["+vIndex+"]="+vehIDs[vIndex]
								+ ", sourceTypeIDs["+sIndex+"]="+sourceTypeIDs[sIndex]
								+ ", hourDayIDs["+hIndex+"]="+hourDayIDs[hIndex]
								+ ", ttgs["+tIndex+"]="+ttgIDs[tIndex]
								+ ", monthIDs["+mIndex+"]="+monthIDs[mIndex]);
							Logger.log(LogMessageCategory.INFO,"initialHourDayIndex["+mIndex+"]["+tIndex+"]="+initialHourDayIndex[mIndex][tIndex]);
							Logger.log(LogMessageCategory.INFO,"hasSVTH="+hasSVTH+", hasHotSoak="+hasHotSoak);
						}
						if(!hasSVTH && !hasHotSoak) {
							// There is no first record because there are no records
							minutesColdSoakStartingFromInitialHour = 60;
							minutesColdSoakStartingInCurrentHour = 0;
							if(shouldLog) {
								Logger.log(LogMessageCategory.INFO,"There is no first record because there are no records");
							}
						} else if(hasSVTH && (!hasHotSoak || svthMinKeyOnTime < hotSoakMinTime[mIndex][tIndex])) {
							// The first record is a vehicle trip
							if(shouldLog) {
								Logger.log(LogMessageCategory.INFO,"The first record is a vehicle trip");
								writeCursorDebugData();
							}
							/**
							 * @step 282
							 * @algorithm minutesColdSoakStartingFromInitialHour = modulus(keyOnTime,60).
							 * minutesColdSoakStartingInCurrentHour = 60 - minutesColdSoakStartingFromInitialHour - sum(KeyOffTime - KeyOnTime).
							 * minutesColdSoakStartingInCurrentHour -= hotSoakCount[monthID][tankTemperatureGroupID].
							 * @condition Vehicle trip record
							**/
							shouldResetInitialHour = true;
							minutesColdSoakStartingFromInitialHour = svthMinKeyOnTime % 60;
							minutesColdSoakStartingInCurrentHour
									= 60 - minutesColdSoakStartingFromInitialHour - svthSum;
							if(hasHotSoak) {
								minutesColdSoakStartingInCurrentHour -= hotSoakCount[mIndex][tIndex];
							}
							if(shouldLog && minutesColdSoakStartingInCurrentHour < 0) {
								Logger.log(LogMessageCategory.INFO,"***** ERROR ERROR ERROR ERROR *****");
							}
						} else {
							// The first record is a hot soak minute.
							if(shouldLog) {
								Logger.log(LogMessageCategory.INFO,"The first record is a hot soak minute");
								writeCursorDebugData();
							}
							/**
							 * @step 282
							 * @algorithm minutesColdSoakStartingFromInitialHour = 0.
							 * minutesColdSoakStartingInCurrentHour = 60.
							 * minutesColdSoakStartingInCurrentHour -= hotSoakCount[monthID][tankTemperatureGroupID].
							 * minutesColdSoakStartingInCurrentHour -= sum(KeyOffTime - KeyOnTime).
							 * @condition Hot soak minute
							**/
							shouldResetInitialHour = true;
							minutesColdSoakStartingFromInitialHour = 0;
							minutesColdSoakStartingInCurrentHour = 60;
							if(hasHotSoak) {
								minutesColdSoakStartingInCurrentHour -= hotSoakCount[mIndex][tIndex];
							}
							if(hasSVTH) {
								minutesColdSoakStartingInCurrentHour -= svthSum;
							}
							if(shouldLog && minutesColdSoakStartingInCurrentHour < 0) {
								Logger.log(LogMessageCategory.INFO,"***** ERROR ERROR ERROR ERROR *****");
							}
						}

						if(shouldLog) {
							Logger.log(LogMessageCategory.INFO,"cold soak from initial hour=" + minutesColdSoakStartingFromInitialHour
								+ ", in current hour=" + minutesColdSoakStartingInCurrentHour);
						}
						/**
						 * @step 282
						 * @algorithm coldSoakMinutes[sourceTypeID][monthID][hourID][initialHourID] += minutesColdSoakStartingFromInitialHour.
						 * coldSoakMinutes[sourceTypeID][monthID][hourID][hourID] += minutesColdSoakStartingInCurrentHour.
						 * coldSoakMinutesSum[sourceTypeID][monthID][hourID] += minutesColdSoakStartingFromInitialHour + minutesColdSoakStartingInCurrentHour.
						**/
						coldSoakMinutes[sIndex][mIndex][hIndex][initialHourDayIndex[mIndex][tIndex]]
								+= minutesColdSoakStartingFromInitialHour;
						coldSoakMinutes[sIndex][mIndex][hIndex][hIndex]
								+= minutesColdSoakStartingInCurrentHour;

						coldSoakMinutesSum[sIndex][mIndex][hIndex]
								+= minutesColdSoakStartingFromInitialHour
								+ minutesColdSoakStartingInCurrentHour;

						if(shouldResetInitialHour) {
							shouldResetInitialHour = false;
							initialHourDayIndex[mIndex][tIndex] = hIndex;
							if(shouldLog) {
								Logger.log(LogMessageCategory.INFO,"Reset initialHourDayIndex["+mIndex+"]["+tIndex+"]="+initialHourDayIndex[mIndex][tIndex]);
							}
						}
					}
				}
			}
		}

		// Get sourceTypeID and monthID combinations that already exist in
		// ColdSoakInitialHourFraction for the current zone
		TreeSet userInputs = getUserInputKeys();

		// Write ColdSoakInitialHourFraction table, ignoring records which already exist
		// for a source type and month combination in the current zone
		File coldSoakFile = new File("ColdSoak.txt");
		PrintWriter coldSoakWriter = null;
		String sql = "";

		try {
			// Try to remove the temporary data files before we fill them, just in case they exist
			if(coldSoakFile.exists()) {
				coldSoakFile.delete();
			}

			// open file for records which can then be bulk loaded
			coldSoakWriter = new PrintWriter(new BufferedWriter(new FileWriter(coldSoakFile),128*1024));
	
			for(sIndex=0;sIndex<sourceTypeIDs.length;sIndex++) {
				for(int mIndex=0;mIndex<monthIDs.length;mIndex++) {
					// Continue if the sourcetype/month combination already exists in this zone
					String key = sourceTypeIDs[sIndex] + "|" + monthIDs[mIndex];
					if(userInputs.contains(key)) {
						continue;
					}
					for(int hIndex=0;hIndex<hourDayIDs.length;hIndex++) {
						if(coldSoakMinutesSum[sIndex][mIndex][hIndex] <= 0) { // skip any 0 counts
							continue;
						}
						for(int iIndex=0;iIndex<hourDayIDs.length;iIndex++) { // initial hours
							if(coldSoakMinutes[sIndex][mIndex][hIndex][iIndex] <= 0) { // skip any 0 counts
								continue;
							}
							/**
							 * @step 282
							 * @algorithm coldSoakInitialHourFraction = coldSoakMinutes / coldSoakMinutesSum.
							 * @output ColdSoakInitialHourFraction
							**/
							double fraction = (double)coldSoakMinutes[sIndex][mIndex][hIndex][iIndex]
								/ (double)coldSoakMinutesSum[sIndex][mIndex][hIndex];
							// write record to file
							coldSoakWriter.println(sourceTypeIDs[sIndex] + "\t"
									+ context.iterLocation.zoneRecordID + "\t"
									+ monthIDs[mIndex] + "\t"
									+ hourDayIDs[hIndex] + "\t"
									+ hourDayIDs[iIndex] + "\t"
									+ fraction);
						}
					}
				}
			}
	
			// Close temporary files so they can be read by the database
			coldSoakWriter.close();
			coldSoakWriter = null;

			// bulk load the records
			sql = "LOAD DATA INFILE '" + coldSoakFile.getCanonicalPath().replace('\\', '/')
					+ "' IGNORE INTO TABLE ColdSoakInitialHourFraction ( "
					+ "sourceTypeID, zoneID, monthID, hourDayID, initialHourDayID,"
					+ "coldSoakInitialHourFraction)";
			SQLRunner.executeSQL(db, sql);
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not determine cold soak fractions.", sql);
		} catch(IOException e) {
			Logger.logError(e,"File I/O error while determining cold soak fractions");
		} finally {
			if(coldSoakWriter != null) {
				try {
					coldSoakWriter.close();
				} catch(Exception e) {
					// Failure to close this file should not be an issue
				}
				coldSoakWriter = null;
			}
			if(coldSoakFile != null) {
				try {
					coldSoakFile.delete();
				} catch(Exception e) {
					// Failure to delete this file should not be an issue
				}
				coldSoakFile = null;
			}
			// TODO delete svthCursorFile and hotSoakCursorFile after making sure the
			// the memory leak has been fixed.
		}
	}
}
