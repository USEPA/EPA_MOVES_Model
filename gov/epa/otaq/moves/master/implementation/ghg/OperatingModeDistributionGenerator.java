/**************************************************************************************************
 * @(#)OperatingModeDistributionGenerator.java
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

/**
 * This builds "Operating Mode Distribution" records for ELDB data.
 * ELDB is the Execution Location Database explained in TotalActivityGenerator
 *
 * @author		Wesley Faler
 * @author		W. Aikman
 * @author		EPA Mitch C. (Task 18 Item 169)
 * @version		2014-05-28
**/
public class OperatingModeDistributionGenerator extends Generator {
	/** @notused **/

	/** Flags for tasks already done, used to prevent duplicate execution **/
	TreeSet<String> alreadyDoneFlags = new TreeSet<String>();
	/** Flag for whether the data tables have been cleared/setup **/
	boolean hasBeenSetup = false;
	/** Flag to validate the data before determining drive schedule distribution **/
	boolean isValid = true;
	/** Database connection used by all functions.  Setup by executeLoop and cleanDataLoop. **/
	Connection db;
	/** milliseconds spent during one time operations **/
	long setupTime = 0;
	/** milliseconds spent during non-one-time operations **/
	long totalTime = 0;
	/** comma-separated list of polProcessIDs used by this generator **/
	String polProcessIDs = "";
	/** Model-year specific rolling and drag terms **/
	SourceTypePhysics modelYearPhysics = new SourceTypePhysics();

	/** Default constructor **/
	public OperatingModeDistributionGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		EmissionProcess runningProcess = EmissionProcess.findByName("Running Exhaust");

		targetLoop.subscribe(this, runningProcess, MasterLoopGranularity.YEAR, // LINK. Year level for source bins from SBDG.
				MasterLoopPriority.GENERATOR);
				
		EmissionProcess brakeProcess = EmissionProcess.findByName("Brakewear");
		targetLoop.subscribe(this, brakeProcess, MasterLoopGranularity.YEAR, // LINK. Year level for source bins from SBDG.
				MasterLoopPriority.GENERATOR);
	}

	/**
	 * Called each time the link changes.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

			long start;

			// The following only has to be done once for each run.
			if(!hasBeenSetup) {
				start = System.currentTimeMillis();
				modelYearPhysics.setup(db);
				calculateRampOpModes();
				bracketAverageSpeedBins();
				determineDriveScheduleProportions();
				if(!validateDriveScheduleDistribution()) {
					isValid = false;
				}
				if(isValid) {
					//determineDriveScheduleDistributionIsRamp();
					determineDriveScheduleDistributionNonRamp();
					calculateEnginePowerBySecond();
					determineOpModeIDPerSecond();
					calculateOpModeFractionsPerDriveSchedule();
					preliminaryCalculateOpModeFractions();
					hasBeenSetup = true;
				}
				setupTime += System.currentTimeMillis() - start;
			}

			start = System.currentTimeMillis();
			if(isValid) {
				String alreadyKey = "calc|" + inContext.iterProcess.databaseKey + "|" + inContext.iterLocation.linkRecordID;
				if(!alreadyDoneFlags.contains(alreadyKey)) {
					alreadyDoneFlags.add(alreadyKey);
					calculateOpModeFractions(inContext.iterLocation.linkRecordID);
				}
				alreadyKey = "rates|" + inContext.iterProcess.databaseKey;
				if(!alreadyDoneFlags.contains(alreadyKey)) {
					alreadyDoneFlags.add(alreadyKey);
					modelYearPhysics.updateEmissionRateTables(db,inContext.iterProcess.databaseKey);
				}
			} else {
				Logger.log(LogMessageCategory.ERROR, "Error while validating drive schedule "
						+ "distribution, operating mode computation cannot continue");
			}
			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,"Operating Mode Distribution Generation failed.");
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"OMDG setupTime=" + setupTime + " bundleTime=" + totalTime);
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		// Do not remove data since it is needed across multiple processes
		// (Running Exhaust and Brakewear).
		/*
		String sql = "";
		try {
			if(polProcessIDs.length() > 0) {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

				sql = "DELETE FROM OpModeDistribution WHERE isUserInput='N' AND linkID = "
						+ context.iterLocation.linkRecordID
						+ " AND polProcessID IN (" + polProcessIDs + ")";
				//System.out.println("########## DELETING OPD ###### : " +
				//		context.iterLocation.linkRecordID);
				SQLRunner.executeSQL(db, sql);
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to delete Operating Mode Distribution data from a previous"
					+ " run", sql);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db=null;
			}
		}
		*/
	}

	/**
	 * Calculate the operating modes for vehicles on ramps.
	**/
	void calculateRampOpModes() {
		String sql = "";
		String[] statements = {
			/*
			// Use the ramp operating mode distribution associated with the road's average speed

			"drop table if exists OMDGAvgSpeedTemp",
			"drop table if exists OMDGAvgSpeed",
			"drop table if exists OMDGRampOpMode",
			"drop table if exists OMDGAvgSpeedBin",

			"create table OMDGAvgSpeedBin"
					+ " select a1.avgSpeedBinID, (a1.avgBinSpeed+a2.avgBinSpeed)/2 as maxBinSpeed"
					+ " from avgSpeedBin a1,"
					+ " avgSpeedBin a2"
					+ " where a2.avgSpeedBinID=1+a1.avgSpeedBinID"
					+ " and a1.avgSpeedBinID>1",

			"insert into OMDGAvgSpeedBin (avgSpeedBinID, maxBinSpeed)"
					+ " select min(avgSpeedBinID), min(avgBinSpeed) from avgSpeedBin",

			"insert into OMDGAvgSpeedBin (avgSpeedBinID, maxBinSpeed)"
					+ " select max(avgSpeedBinID), 10000.0 from avgSpeedBin",

			"create table OMDGAvgSpeedTemp"
					+ " select sourceTypeID, roadTypeID, hourDayID,"
					+ " sum(avgSpeedFraction*avgBinSpeed) as avgSpeed"
					+ " from avgSpeedDistribution"
					+ " inner join avgSpeedBin using (avgSpeedBinID)"
					+ " group by sourceTypeID, roadTypeID, hourDayID"
					+ " order by null",

			"create table OMDGAvgSpeed"
					+ " select sourceTypeID, roadTypeID, hourDayID, "
					+ " min(avgSpeedBinID) as avgSpeedBinID"
					+ " from OMDGAvgSpeedTemp,"
					+ " OMDGAvgSpeedBin"
					+ " where maxBinSpeed > avgSpeed"
					+ " group by sourceTypeID, roadTypeID, hourDayID"
					+ " order by null",

			"create table OMDGRampOpMode"
					+ " select romd.sourceTypeID, romd.roadTypeID, hourDayID, opModeID,"
					+ " (rampFraction*opModeFraction) as opModeFraction"
					+ " from RoadType rt"
					+ " inner join OMDGAvgSpeed avgsp on avgsp.roadTypeID=rt.roadTypeID"
					+ " inner join RoadOpmodeDistribution romd on ("
					+ " 	romd.sourceTypeID=avgsp.sourceTypeID"
					+ " 	and romd.roadTypeID=avgsp.roadTypeID"
					+ " 	and romd.avgSpeedBinID=avgsp.avgSpeedBinID"
					+ " )"
					+ " where romd.isRamp='Y'"
			*/

			// Use ramps from all speed bins, weighting their operating mode distributions by the time
			// spent in the speed bin.

			"drop table if exists OMDGRampOpMode",
			
			"alter table avgSpeedDistribution add key rampIndex   (roadTypeID, sourceTypeID, hourDayID, avgSpeedBinID)",
			
			"alter table roadOpModeDistribution add key rampIndex (roadTypeID, sourceTypeID, opModeID, isRamp, avgSpeedBinID)",
			
			"create table OMDGRampOpMode"
					+ " select straight_join romd.sourceTypeID, romd.roadTypeID, hourDayID, romd.opModeID,"
					+ " sum(rampFraction*opModeFraction*avgSpeedFraction) as opModeFraction"
					+ " from RoadType rt"
					+ " inner join avgSpeedDistribution asd force key (rampIndex) on asd.roadTypeID=rt.roadTypeID"
					+ " inner join RoadOpmodeDistribution romd force key (rampIndex) on ("
					+ " 	romd.sourceTypeID=asd.sourceTypeID"
					+ " 	and romd.roadTypeID=asd.roadTypeID"
					+ " 	and romd.avgSpeedBinID=asd.avgSpeedBinID"
					+ " )"
					+ " where romd.isRamp='Y' and rt.rampFraction > 0"
					+ " group by romd.roadTypeID, romd.sourceTypeID, asd.hourDayID, romd.opModeID"
					+ " order by null",
			
			"alter table avgSpeedDistribution drop key rampIndex",
			
			"alter table roadOpModeDistribution drop key rampIndex"
		};
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
			sql = "insert ignore into OMDGRampOpMode (sourceTypeID, roadTypeID, hourDayID, opModeID, opModeFraction)"
					+ " select tempSourceTypeID, roadTypeID, hourDayID, opModeID, opModeFraction"
					+ " from OMDGRampOpMode"
					+ " inner join sourceUseTypePhysicsMapping on (realSourceTypeID=sourceTypeID)"
					+ " where tempSourceTypeID <> realSourceTypeID";
			SQLRunner.executeSQL(db,sql);
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not calculate average speeds", sql);
		}
	}

	/**
	 * OMDG-1: Determine the drive schedules that bracket each Average Speed Bin value.
	 * <p>Each average speed bin lies between (is bracketed) by the average speeds of two drive
	 * schedules. Determine which two drive schedules bracket the average speed bin and store the
	 * identity and average speeds of the two bins.  This is done for each source type, roadway
	 * type, day of week and hour of day for each average speed bin.</p>
	**/
	void bracketAverageSpeedBins() {
		String sql = "";

		ResultSet rs = null;
		try {
			// Remove DriveScheduleAssoc records with isRamp='Y' but associated with RoadTypes with
			// rampFraction <= 0 since these are nonsensical conditions.
			sql = "SELECT dsa.sourceTypeID,dsa.roadTypeID,dsa.isRamp,dsa.driveScheduleID "+
					"FROM DriveScheduleAssoc dsa, RoadType rt "+
					"WHERE dsa.isRamp IN ('Y','y') AND "+
					"dsa.roadTypeID = rt.roadTypeID AND "+
					"rt.rampFraction <= 0";
			rs = SQLRunner.executeQuery(db,sql);
			while(rs.next()) {
				sql = "DELETE FROM DriveScheduleAssoc "+
						"WHERE isRamp IN ('Y','y') AND "+
						"sourceTypeID = " + rs.getInt("sourceTypeID") + " AND " +
						"roadTypeID = " + rs.getInt("roadTypeID") + " AND " +
						"driveScheduleID = " + rs.getInt("driveScheduleID");
				SQLRunner.executeSQL(db,sql);
			}
			rs.close();
			rs = null;

			//
			// The documentation doesn't mention this but, going from the spreadsheet, speed bins
			// with values below and above the lowest and highest drive schedule values are bound
			// to those values. The following query determines these bounded values.
			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleBounds ("+
						"sourceTypeID     SMALLINT,"+
						"roadTypeID       SMALLINT,"+
						"scheduleBoundLo  FLOAT,"+
						"scheduleBoundHi  FLOAT,"+
						"UNIQUE INDEX XPKDriveScheduleBounds ("+
							"sourceTypeID, roadTypeID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleBounds";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleBounds ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"scheduleBoundLo,"+
						"scheduleBoundHi) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"MIN(ds.averageSpeed),"+
						"MAX(ds.averageSpeed) "+
					"FROM "+
						"RunSpecRoadType rsrt,"+
						"RunSpecSourceType rsst,"+
						"DriveSchedule ds,"+
						"DriveScheduleAssoc dsa "+
					"WHERE "+
						"rsrt.roadTypeID = dsa.roadTypeID AND "+
						"rsst.sourceTypeID = dsa.sourceTypeID AND "+
						"ds.drivescheduleid = dsa.drivescheduleid "+
					"GROUP BY "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleBounds");

			sql = "CREATE TABLE IF NOT EXISTS BracketScheduleLo2 ("+
						"sourceTypeID     SMALLINT,"+
						"roadTypeID       SMALLINT,"+
						"avgSpeedBinID    SMALLINT,"+
						"driveScheduleID  SMALLINT,"+
						"loScheduleSpeed  FLOAT,"+
						"isOutOfBounds    SMALLINT,"+
						"UNIQUE INDEX XPKBracketScheduleLo2 ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BracketScheduleLo2";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO BracketScheduleLo2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"loScheduleSpeed,"+
						"isOutOfBounds) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgSpeedBinID,"+
						"MAX(ds.averageSpeed),"+
						"0 as isOutOfBounds "+
					"FROM "+
						"RunSpecRoadType rsrt,"+
						"RunSpecSourceType rsst,"+
						"DriveSchedule ds,"+
						"DriveScheduleAssoc dsa,"+
						"AvgSpeedBin asb "+
					"WHERE "+
						"rsrt.roadTypeID = dsa.roadTypeID AND "+
						"rsst.sourceTypeID = dsa.sourceTypeID AND "+
						"ds.driveScheduleID = dsa.driveScheduleID AND "+
						"ds.averageSpeed <= asb.avgBinSpeed AND "+
						"(dsa.isRamp <> 'Y' AND dsa.isRamp <> 'y') "+
					"GROUP BY "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgBinSpeed";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleLo2");
				// changed to INSERT IGNORE to work properly with MySQL 4
			sql = "INSERT IGNORE INTO BracketScheduleLo2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"loScheduleSpeed,"+
						"isOutOfBounds) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgSpeedBinID,"+
						"dsb.scheduleBoundLo,"+
						"1 as isOutOfBounds "+
					"FROM "+
						"RunSpecRoadType rsrt,"+
						"RunSpecSourceType rsst,"+
						"DriveSchedule ds,"+
						"DriveScheduleAssoc dsa,"+
						"DriveScheduleBounds dsb,"+
						"AvgSpeedBin asb "+
					"WHERE "+
						"rsrt.roadTypeID = dsa.roadTypeID AND "+
						"rsst.sourceTypeID = dsa.sourceTypeID AND "+
						"ds.driveScheduleID = dsa.driveScheduleID AND "+
						"dsb.sourceTypeID = dsa.sourceTypeID AND "+
						"dsb.roadTypeID = dsa.roadTypeID AND "+
						"asb.avgBinSpeed < dsb.scheduleBoundLo AND "+
						"(dsa.isRamp <> 'Y' AND dsa.isRamp <> 'y') ";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE TABLE IF NOT EXISTS BracketScheduleLo ("+
					"sourceTypeID    SMALLINT,"+
					"roadTypeID      SMALLINT,"+
					"avgSpeedBinID   SMALLINT,"+
					"driveScheduleID SMALLINT,"+
					"loScheduleSpeed FLOAT,"+
					"UNIQUE INDEX XPKBracketScheduleLo ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BracketScheduleLo";
			SQLRunner.executeSQL(db, sql);
			// changed to INSERT IGNORE to work with MySQL 4
			sql = "INSERT IGNORE INTO BracketScheduleLo ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"driveScheduleID,"+
						"loScheduleSpeed) "+
					"SELECT "+
						"bsl.sourceTypeID,"+
						"bsl.roadTypeID,"+
						"bsl.avgSpeedBinID,"+
						"ds.driveScheduleID,"+
						"bsl.loScheduleSpeed "+
					"FROM "+
						"BracketScheduleLo2 bsl,"+
						"DriveScheduleAssoc dsa,"+
						"DriveSchedule ds "+
					"WHERE "+
						"dsa.driveScheduleID = ds.driveScheduleID AND "+
						"dsa.sourceTypeID = bsl.sourceTypeID AND "+
						"dsa.roadTypeID = bsl.roadTypeID AND "+
						"ds.averageSpeed = bsl.loScheduleSpeed AND "+
						"(dsa.isRamp <> 'Y' AND dsa.isRamp <> 'y') ";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleLo");

			sql = "CREATE TABLE IF NOT EXISTS BracketScheduleHi2 ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"avgSpeedBinID     SMALLINT,"+
					"driveScheduleID   SMALLINT,"+
					"hiScheduleSpeed   FLOAT,"+
					"isOutOfBounds	   SMALLINT,"+
					"UNIQUE INDEX XPKBracketScheduleHi2 ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BracketScheduleHi2";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO BracketScheduleHi2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"hiScheduleSpeed,"+
						"isOutOfBounds) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgSpeedBinID,"+
						"MIN(ds.averageSpeed),"+
						"0 as isOutOfBounds "+
					"FROM "+
						"RunSpecRoadType rsrt,"+
						"RunSpecSourceType rsst,"+
						"DriveSchedule ds,"+
						"DriveScheduleAssoc dsa,"+
						"AvgSpeedBin asb "+
					"WHERE "+
						"rsrt.roadTypeID = dsa.roadTypeID AND "+
						"rsst.sourceTypeID = dsa.sourceTypeID AND "+
						"ds.driveScheduleID = dsa.driveScheduleID AND "+
						"ds.averageSpeed > asb.avgBinSpeed AND "+
						"(dsa.isRamp <> 'Y' AND dsa.isRamp <> 'y') "+
					"GROUP BY "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgBinSpeed";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleHi2");
				// changed to INSERT IGNORE to work with MySQL 4.0
			sql = "INSERT IGNORE INTO BracketScheduleHi2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"hiScheduleSpeed,"+
						"isOutOfBounds) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgSpeedBinID,"+
						"dsb.scheduleBoundHi,"+
						"1 as isOutOfBounds "+
					"FROM "+
						"RunSpecRoadType rsrt,"+
						"RunSpecSourceType rsst,"+
						"DriveSchedule ds,"+
						"DriveScheduleAssoc dsa,"+
						"DriveScheduleBounds dsb,"+
						"AvgSpeedBin asb "+
					"WHERE "+
						"rsrt.roadTypeID = dsa.roadTypeID AND "+
						"rsst.sourceTypeID = dsa.sourceTypeID AND "+
						"ds.driveScheduleID = dsa.driveScheduleID AND "+
						"dsb.sourceTypeID = dsa.sourceTypeID AND "+
						"dsb.roadTypeID = dsa.roadTypeID AND "+
						"asb.avgBinSpeed > dsb.scheduleBoundHi AND "+
						"(dsa.isRamp <> 'Y' AND dsa.isRamp <> 'y') ";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleHi2");

			// Look for BracketScheduleLo2.isOutOfBounds=1 entries and complain.
			// Look for BracketScheduleHi2.isOutOfBounds=1 entries and complain.
			sql = "select distinct sourceTypeID, roadTypeID, avgSpeedBinID, 1 as isLow"
					+ " from BracketScheduleLo2"
					+ " where isOutOfBounds=1"
					+ " union"
					+ " select distinct sourceTypeID, roadTypeID, avgSpeedBinID, 0 as isLow"
					+ " from BracketScheduleHi2"
					+ " where isOutOfBounds=1";
			SQLRunner.Query query = new SQLRunner.Query();
			try {
				query.open(db,sql);
				while(query.rs.next()) {
					int sourceTypeID = query.rs.getInt(1);
					int roadTypeID = query.rs.getInt(2);
					int avgSpeedBinID = query.rs.getInt(3);
					boolean isLow = query.rs.getInt(4) > 0;

					String message = "";
					if(isLow) {
						message = "All driving cycles for avgSpeedBinID " + avgSpeedBinID
							+ " for sourcetype " + sourceTypeID
							+ " on roadtype " + roadTypeID
							+ " were too fast.";
					} else {
						message = "All driving cycles for avgSpeedBinID " + avgSpeedBinID
							+ " for sourcetype " + sourceTypeID
							+ " on roadtype " + roadTypeID
							+ " were too slow.";
					}
					if(CompilationFlags.ALLOW_DRIVE_CYCLE_EXTRAPOLATION) {
						message += " MOVES results for this speed were extrapolated from the closest available driving cycles.";
						Logger.log(LogMessageCategory.WARNING,message);
					} else {
						message += " MOVES cannot proceed.";
						Logger.log(LogMessageCategory.ERROR,message);
						MOVESEngine.terminalErrorFound();
					}
				}
			} finally {
				query.onFinally();
			}

			sql = "CREATE TABLE IF NOT EXISTS BracketScheduleHi ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"avgSpeedBinID     SMALLINT,"+
					"driveScheduleID   SMALLINT,"+
					"hiScheduleSpeed   FLOAT,"+
					"UNIQUE INDEX XPKBracketScheduleHi ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BracketScheduleHi";
			SQLRunner.executeSQL(db, sql);
			// changed to INSERT IGNORE to work with MySQL 4.
			sql = "INSERT IGNORE INTO BracketScheduleHi ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"driveScheduleID,"+
						"hiScheduleSpeed) "+
					"SELECT "+
						"bsl.sourceTypeID,"+
						"bsl.roadTypeID,"+
						"bsl.avgSpeedBinID,"+
						"ds.driveScheduleID,"+
						"bsl.hiScheduleSpeed "+
					"FROM "+
						"BracketScheduleHi2 bsl,"+
						"DriveScheduleAssoc dsa,"+
						"DriveSchedule ds "+
					"WHERE "+
						"dsa.driveScheduleID = ds.driveScheduleID AND "+
						"dsa.sourceTypeID = bsl.sourceTypeID AND "+
						"dsa.roadTypeID = bsl.roadTypeID AND "+
						"ds.averageSpeed = bsl.hiScheduleSpeed AND "+
						"(dsa.isRamp <> 'Y' AND dsa.isRamp <> 'y') ";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleHi");

			// Delete intermediate results for large tables. Normally, intermediate
			// results are kept when possible for debugging purposes.
			sql = "TRUNCATE BracketScheduleLo2";
			SQLRunner.executeSQL(db, sql);

			// Delete intermediate results for potentially large tables. Normally, intermediate
			// results are kept when possible for debugging purposes.
			sql = "TRUNCATE BracketScheduleHi2";
			SQLRunner.executeSQL(db, sql);

		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine brackets for Average Speed Bins.", sql);
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch (SQLException e) {
					// Failure to close on a ResultSet should not be an issue.
				}
				rs = null;
			}
		}
	}

	/**
	 * OMDG-2: Determine proportions for bracketing drive schedules.
	 * <p>This step determines the proportion of each of the bracketing drive schedules such that
	 * the combination of the average speeds of drive schedules equals the nominal average speed
	 * of each average speed bin. The results are then weighted by the fraction of all operating
	 * time that are represented by the time spent in that average speed bin. This is done for each
	 * source type, roadway type, day of week and hour of day.</p>
	**/
	void determineDriveScheduleProportions() {
		String sql = "";

		try {
			sql = "CREATE TABLE IF NOT EXISTS LoScheduleFraction2 ("+
					"sourceTypeId       SMALLINT,"+
					"roadTypeId         SMALLINT,"+
					"avgSpeedBinID      SMALLINT,"+
					"loScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKLoScheduleFraction2 ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE LoScheduleFraction2";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO LoScheduleFraction2 ("+
						"sourceTypeId,"+
						"roadTypeId,"+
						"avgSpeedBinID,"+
						"loScheduleFraction) "+
					"SELECT "+
						"bsl.sourceTypeId,"+
						"bsl.roadTypeId,"+
						"bsl.avgSpeedBinID,"+
						"(bsh.hiScheduleSpeed - asb.avgBinSpeed) / (bsh.hiScheduleSpeed -"+
								"bsl.loScheduleSpeed) "+
					"FROM "+
						"BracketScheduleLo bsl,"+
						"BracketScheduleHi bsh,"+
						"AvgSpeedBin asb "+
					"WHERE "+
						"bsl.sourceTypeId = bsh.sourceTypeId AND "+
						"bsl.roadTypeId = bsh.roadTypeId AND "+
						"bsl.avgSpeedBinID = bsh.avgSpeedBinID AND "+
						"bsl.avgSpeedBinID = asb.avgSpeedBinID AND "+
						"bsh.hiScheduleSpeed <> bsl.loScheduleSpeed";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE LoScheduleFraction2");

			sql = "INSERT INTO LoScheduleFraction2 ("+
						"sourceTypeId,"+
						"roadTypeId,"+
						"avgSpeedBinID,"+
						"loScheduleFraction) "+
					"SELECT "+
						"bsl.sourceTypeId,"+
						"bsl.roadTypeId,"+
						"bsl.avgSpeedBinID,"+
						"1 "+
					"FROM "+
						"BracketScheduleLo bsl,"+
						"BracketScheduleHi bsh "+
					"WHERE "+
						"bsl.sourceTypeId = bsh.sourceTypeId AND "+
						"bsl.roadTypeId = bsh.roadTypeId AND "+
						"bsl.avgSpeedBinID = bsh.avgSpeedBinID AND "+
						"bsh.hiScheduleSpeed = bsl.loScheduleSpeed";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE LoScheduleFraction2");

			sql = "CREATE TABLE IF NOT EXISTS HiScheduleFraction2 ("+
					"sourceTypeId       SMALLINT,"+
					"roadTypeId         SMALLINT,"+
					"avgSpeedBinID      SMALLINT,"+
					"hiScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKHiScheduleFraction2 ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE HiScheduleFraction2";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO HiScheduleFraction2 ("+
						"sourceTypeId,"+
						"roadTypeId,"+
						"avgSpeedBinID,"+
						"hiScheduleFraction) "+
					"SELECT "+
						"bsh.sourceTypeId,"+
						"bsh.roadTypeId,"+
						"bsh.avgSpeedBinID,"+
						"(1 - lsf2.loScheduleFraction) "+
					"FROM "+
						"BracketScheduleHi bsh,"+
						"LoScheduleFraction2 lsf2 "+
					"WHERE "+
						"lsf2.sourceTypeId = bsh.sourceTypeId AND "+
						"lsf2.roadTypeId = bsh.roadTypeId AND "+
						"lsf2.avgSpeedBinID = bsh.avgSpeedBinID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE HiScheduleFraction2");

			sql = "CREATE TABLE IF NOT EXISTS LoScheduleFraction ("+
					"sourceTypeId       SMALLINT,"+
					"roadTypeId         SMALLINT,"+
					"avgSpeedBinID      SMALLINT,"+
					"hourDayID          SMALLINT,"+
					"loScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKLoScheduleFraction ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE LoScheduleFraction";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO LoScheduleFraction ("+
						"sourceTypeId,"+
						"roadTypeId,"+
						"avgSpeedBinID,"+
						"hourDayID,"+
						"loScheduleFraction) "+
					"SELECT "+
						"lsf2.sourceTypeId,"+
						"lsf2.roadTypeId,"+
						"lsf2.avgSpeedBinID,"+
						"asd.hourDayID,"+
						"lsf2.loScheduleFraction * asd.avgSpeedFraction "+
					"FROM "+
						"RunSpecHour rsh,"+
						"RunSpecDay rsd,"+
						"HourDay hd,"+
						"LoScheduleFraction2 lsf2,"+
						"AvgSpeedDistribution asd "+
					"WHERE "+
						"rsh.hourID = hd.hourID AND "+
						"rsd.dayID = hd.dayID AND "+
						"hd.hourDayID = asd.hourDayID AND "+
						"lsf2.sourceTypeId = asd.sourceTypeID AND "+
						"lsf2.roadTypeId = asd.roadTypeID AND "+
						"lsf2.avgSpeedBinID = asd.avgSpeedBinID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE LoScheduleFraction");

			sql = "CREATE TABLE IF NOT EXISTS HiScheduleFraction ("+
					"sourceTypeId       SMALLINT,"+
					"roadTypeId         SMALLINT,"+
					"avgSpeedBinID      SMALLINT,"+
					"hourDayID          SMALLINT,"+
					"hiScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKHiScheduleFraction ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE HiScheduleFraction";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO HiScheduleFraction ("+
						"sourceTypeId,"+
						"roadTypeId,"+
						"avgSpeedBinID,"+
						"hourDayID,"+
						"hiScheduleFraction) "+
					"SELECT "+
						"hsf2.sourceTypeId,"+
						"hsf2.roadTypeId,"+
						"hsf2.avgSpeedBinID,"+
						"asd.hourDayID,"+
						"hsf2.hiScheduleFraction * asd.avgSpeedFraction "+
					"FROM "+
						"RunSpecHour rsh,"+
						"RunSpecDay rsd,"+
						"HourDay hd,"+
						"HiScheduleFraction2 hsf2,"+
						"AvgSpeedDistribution asd "+
					"WHERE "+
						"rsh.hourID = hd.hourID AND "+
						"rsd.dayID = hd.dayID AND "+
						"hd.hourDayID = asd.hourDayID AND "+
						"hsf2.sourceTypeId = asd.sourceTypeID AND "+
						"hsf2.roadTypeId = asd.roadTypeID AND "+
						"hsf2.avgSpeedBinID = asd.avgSpeedBinID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE HiScheduleFraction");
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine fraction of drive schedules in each "
					+ "speed bin.", sql);
		}
	}

	/**
	 * Validate drive schedule proportions before determining drive schedule distribution.
	 * @return true on success
	**/
	boolean validateDriveScheduleDistribution() {
		String sql = "";
		PreparedStatement statement = null;
		ResultSet result = null;
		int lastSourceType = -1;
		int lastRoadType = -1;
		int lastDriveScheduleID = -1;
		try {
			/*
			sql = "SELECT DISTINCT dsa.sourceTypeID, dsa.roadTypeID"
					+ " FROM DriveScheduleAssoc dsa, RunSpecRoadType rsrt, "
					+ " RunSpecSourceType rsst "
					+ " WHERE (dsa.isRamp = 'Y' OR dsa.isRamp='y')"
					+ " AND dsa.roadTypeID = rsrt.roadTypeID"
					+ " AND dsa.sourceTypeID = rsst.sourceTypeID"
					+ " ORDER BY sourceTypeID, roadTypeID";
			statement = db.prepareStatement(sql);
			result = SQLRunner.executeQuery(statement, sql);
			while(result.next()) {
				int sourceType = result.getInt(1);
				int roadType = result.getInt(2);
				if(lastSourceType != sourceType || lastRoadType != roadType) {
					lastSourceType = sourceType;
					lastRoadType = roadType;
				} else {
					Logger.log(LogMessageCategory.ERROR,
							"Ramp Schedule has more than one driving schedule");
					return false;
				}
			}
			statement.close();
			result.close();
			*/

			/*
			sql = "SELECT DISTINCT dsa.SourceTypeID, dsa.RoadTypeID, dsa.IsRamp "
					+ " FROM RoadType rt, DriveScheduleAssoc dsa, RunSpecRoadType rsrt, "
					+ " RunSpecSourceType rsst "
					+ " WHERE rt.RampFraction > 0 "
					+ " AND rt.RoadTypeID = dsa.RoadTypeID "
					+ " AND dsa.RoadTypeID = rsrt.RoadTypeID "
					+ " AND dsa.sourceTypeID = rsst.sourceTypeID"
					+ " ORDER BY dsa.SourceTypeID, dsa.RoadTypeID, dsa.IsRamp";
			statement = db.prepareStatement(sql);
			result = SQLRunner.executeQuery(statement, sql);
			lastSourceType = -1;
			lastRoadType = -1;
			boolean hasRamp = false;
			while(result.next()) {
				int sourceType = result.getInt(1);
				int roadType = result.getInt(2);
				boolean isRamp = result.getString(3).equalsIgnoreCase("Y");
				if(lastSourceType != sourceType || lastRoadType != roadType) {
					if(lastSourceType >= 0 && !hasRamp) {
						Logger.log(LogMessageCategory.ERROR,
								"No ramp schedule for road type " + lastRoadType + " and "
								+ "source type " + lastSourceType);
						return false;
					}
					lastSourceType = sourceType;
					lastRoadType = roadType;
					hasRamp = isRamp;
				} else {
					hasRamp = hasRamp || isRamp;
				}
			}
			if(lastSourceType >= 0 && !hasRamp) {
				Logger.log(LogMessageCategory.ERROR,
						"No ramp schedule for road type " + lastRoadType + " and "
						+ "source type " + lastSourceType);
				return false;
			}
			statement.close();
			result.close();
			*/

			sql = "SELECT DISTINCT dsa.SourceTypeID, dsa.RoadTypeID, dsa.IsRamp "
					+ " FROM DriveScheduleAssoc dsa, RunSpecRoadType rsrt, "
					+ " RunSpecSourceType rsst "
					+ " WHERE dsa.roadTypeID = rsrt.roadTypeID"
					+ " AND dsa.sourceTypeID = rsst.sourceTypeID"
					+ " ORDER BY dsa.SourceTypeID, dsa.RoadTypeID, dsa.IsRamp";
			statement = db.prepareStatement(sql);
			result = SQLRunner.executeQuery(statement, sql);
			lastSourceType = -1;
			lastRoadType = -1;
			boolean hasNonRamp = false;
			while(result.next()) {
				int sourceType = result.getInt(1);
				int roadType = result.getInt(2);
				boolean isNonRamp = result.getString(3).equalsIgnoreCase("N");
				if(lastSourceType != sourceType || lastRoadType != roadType) {
					if(lastSourceType >= 0 && !hasNonRamp) {
						Logger.log(LogMessageCategory.ERROR,
								"No non-ramp schedule for road type " + lastRoadType + " and "
								+ "source type " + lastSourceType);
						return false;
					}
					lastSourceType = sourceType;
					lastRoadType = roadType;
					hasNonRamp = isNonRamp;
				} else {
					hasNonRamp = hasNonRamp || isNonRamp;
				}
			}
			if(lastSourceType >= 0 && !hasNonRamp) {
				Logger.log(LogMessageCategory.ERROR,
						"No non-ramp schedule for road type " + lastRoadType + " and "
						+ "source type " + lastSourceType);
				return false;
			}
		} catch(Exception e) {
			Logger.logError(e, "Error while validating drive schedule distribution");
			return false;
		} finally {
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatment should not be an issue.
				}
			}
		}
		return true;
	}

	/**
	 * OMDG-3 (IsRamp) : Determine Distribution of RAMP Drive Schedules.
	 * <p>This step determines the distribution of drive schedules which represents the sum of
	 * all of the average speed bins. This is done for each source type, roadway type, day of
	 * week and hour of day.</p>
	**/
	/*
	void determineDriveScheduleDistributionIsRamp() {
		String sql = "";

		try {
			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleFraction ("+
					"sourceTypeID          SMALLINT,"+
					"roadTypeID            SMALLINT,"+
					"hourDayID             SMALLINT,"+
					"driveScheduleID       SMALLINT,"+
					"isRamp                CHAR(1),"+
					"driveScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKDriveScheduleFraction ("+
							"sourceTypeID, roadTypeID, hourDayID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleFraction";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleFraction ( "+
						"sourceTypeID,"+
						"roadTypeID,"+
						"hourDayID,"+
						"driveScheduleID,"+
						"isRamp,"+
						"driveScheduleFraction) "+
					"SELECT " +
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"hd.hourDayID,"+
						"dsa.driveScheduleID,"+
						"dsa.isRamp,"+
						"rt.rampFraction "+
					"FROM " +
						"DriveScheduleAssoc dsa,"+
						"HourDay hd,"+
						"RoadType rt,"+
						"SourceTypeHour sth,"+
						"RunSpecHour rsh,"+
						"RunSpecDay rsd "+
					"WHERE (dsa.isRamp = 'Y' OR dsa.isRamp='y') AND "+
						"dsa.roadTypeID = rt.roadTypeID AND "+
						"rsh.hourID = hd.hourID AND "+
						"rsd.dayID = hd.dayID AND "+
						"sth.hourDayID = hd.hourDayID AND "+
						"sth.sourceTypeID = dsa.sourceTypeID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFraction");
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not determine the distribution of drive schedules for ramp"
					+ " drive cycle.", sql);
		}
	}
	*/

	/**
	 * OMDG-3 (Non-Ramp) : Determine Distribution of Non Ramp Drive Schedules.
	 * <p>This step determines the distribution of drive schedules which represents the sum of
	 * all of the average speed bins. This is done for each source type, roadway type, day of
	 * week and hour of day.</p>
	**/
	void determineDriveScheduleDistributionNonRamp() {
		String sql = "";

		try {
			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleFractionLo ("+
					"sourceTypeID          SMALLINT,"+
					"roadTypeID            SMALLINT,"+
					"hourDayID             SMALLINT,"+
					"driveScheduleID       SMALLINT,"+
					"driveScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKDriveScheduleFractionLo ("+
							"sourceTypeID, roadTypeID, hourDayID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleFractionLo";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleFractionLo ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"hourDayID,"+
						"driveScheduleID,"+
						"driveScheduleFraction) "+
					"SELECT "+
						"bsl.sourceTypeID,"+
						"bsl.roadTypeID,"+
						"lsf.hourDayID,"+
						"bsl.driveScheduleID,"+
						"SUM(lsf.loScheduleFraction) "+
					"FROM "+
						"BracketScheduleLo bsl,"+
						"LoScheduleFraction lsf "+
					"WHERE "+
						"bsl.sourceTypeid = lsf.sourceTypeID AND "+
						"bsl.roadTypeID = lsf.roadTypeID AND "+
						"bsl.avgSpeedBinID = lsf.avgSpeedBinID "+
					"GROUP BY "+
						"bsl.sourceTypeId,"+
						"bsl.roadTypeId,"+
						"lsf.hourDayId,"+
						"bsl.driveScheduleId";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFractionLo");

			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleFractionHi ( "+
					"sourceTypeID          SMALLINT, "+
					"roadTypeID            SMALLINT, "+
					"hourDayID             SMALLINT, "+
					"driveScheduleID       SMALLINT, "+
					"driveScheduleFraction FLOAT, "+
					"UNIQUE INDEX XPKDriveScheduleFractionHi ( "+
					"sourceTypeID, roadTypeID, hourDayID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleFractionHi";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleFractionHi ( "+
						"sourceTypeID, "+
						"roadTypeID, "+
						"hourDayID, "+
						"driveScheduleID, "+
						"driveScheduleFraction) "+
					"SELECT "+
						"bsh.sourceTypeID, "+
						"bsh.roadTypeID, "+
						"hsf.hourDayID, "+
						"bsh.driveScheduleID, "+
						"SUM(hsf.hiScheduleFraction) "+
					"FROM "+
						"BracketScheduleHi bsh, "+
						"HiScheduleFraction hsf "+
					"WHERE "+
						"bsh.sourceTypeid = hsf.sourceTypeID AND "+
						"bsh.roadTypeID = hsf.roadTypeID AND "+
						"bsh.avgSpeedBinID = hsf.avgSpeedBinID "+
					"GROUP BY "+
						"bsh.sourceTypeId, "+
						"bsh.roadTypeId, "+
						"hsf.hourDayId, "+
						"bsh.driveScheduleId";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFractionHi");

			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleFraction ("+
					"sourceTypeID          SMALLINT,"+
					"roadTypeID            SMALLINT,"+
					"hourDayID             SMALLINT,"+
					"driveScheduleID       SMALLINT,"+
					"isRamp                CHAR(1),"+
					"driveScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKDriveScheduleFraction ("+
							"sourceTypeID, roadTypeID, hourDayID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);
			// changed to INSERT IGNORE to work with MySQL 4, 
			// because DriveScheduleFractionLo and Hi tables have multiple speed bins
			sql = "INSERT IGNORE INTO DriveScheduleFraction ( "+
						"sourceTypeID, "+
						"roadTypeID, "+
						"hourDayID, "+
						"driveScheduleID, "+
						"isRamp, "+
						"driveScheduleFraction) "+
					"SELECT  "+
						"bsh.sourceTypeID, "+
						"bsh.roadTypeID, "+
						"dsfh.hourDayID, "+
						"bsh.driveScheduleID, "+
						"dsa.isRamp, "+
						"(dsfl.driveScheduleFraction + dsfh.driveScheduleFraction) "+
							"* (1 - rt.rampFraction) "+
					"FROM  "+
						"BracketScheduleHi bsh, "+
						"DriveScheduleFractionLo dsfl, "+
						"DriveScheduleFractionHi dsfh, "+
						"RoadType rt, "+
						"DriveScheduleAssoc dsa "+
					"WHERE  "+
						"(dsa.isRamp='N' OR dsa.isRamp='n') AND  "+
						"bsh.sourceTypeID = dsfl.sourceTypeID AND "+
						"bsh.roadTypeID = dsfl.roadTypeID AND  "+
						"rt.roadTypeID = dsa.roadTypeID AND  "+
						"bsh.driveScheduleID = dsfl.driveScheduleID AND  "+
						"bsh.driveScheduleID = dsa.driveScheduleID AND "+
						"bsh.sourceTypeID = dsfh.sourceTypeID AND "+
						"bsh.roadTypeID = dsfh.roadTypeID AND  "+
						"bsh.driveScheduleID = dsfh.driveScheduleID AND "+ 
						"bsh.sourceTypeID = dsa.sourceTypeID AND "+
						"bsh.roadTypeID = dsa.roadTypeID AND "+
						"bsh.roadTypeID = rt.roadTypeID AND "+
						"dsfl.hourDayID = dsfh.hourDayID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFraction");

			sql = "INSERT IGNORE INTO DriveScheduleFraction ( "+
						"sourceTypeID, "+
						"roadTypeID, "+
						"hourDayID, "+
						"driveScheduleID, "+
						"isRamp, "+
						"driveScheduleFraction) "+
					"SELECT  "+
						"bsl.sourceTypeID, "+
						"bsl.roadTypeID, "+
						"dsfl.hourDayID, "+
						"bsl.driveScheduleID, "+
						"dsa.isRamp, "+
						"dsfl.driveScheduleFraction "+
							"* (1 - rt.rampFraction) "+
					"FROM  "+
						"BracketScheduleLo bsl, "+
						"DriveScheduleFractionLo dsfl, "+
						"RoadType rt, "+
						"DriveScheduleAssoc dsa "+
					"WHERE  "+
						"(dsa.isRamp='N' OR dsa.isRamp='n') AND "+
						"bsl.sourceTypeID = dsfl.sourceTypeID AND "+
						"bsl.roadTypeID = dsfl.roadTypeID AND  "+
						"bsl.roadTypeID = rt.roadTypeID AND "+
						"bsl.roadTypeID = dsa.roadTypeID AND "+
						"bsl.driveScheduleID = dsa.driveScheduleID AND "+
						"rt.roadTypeID = dsa.roadTypeID AND  "+
						"bsl.driveScheduleID = dsfl.driveScheduleID AND "+
						"bsl.driveScheduleID = dsa.driveScheduleID ";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFraction");

			sql = "INSERT IGNORE INTO DriveScheduleFraction ( "+
						"sourceTypeID, "+
						"roadTypeID, "+
						"hourDayID, "+
						"driveScheduleID, "+
						"isRamp, "+
						"driveScheduleFraction) "+
					"SELECT  "+
						"bsh.sourceTypeID, "+
						"bsh.roadTypeID, "+
						"dsfh.hourDayID, "+
						"bsh.driveScheduleID, "+
						"dsa.isRamp, "+
						"dsfh.driveScheduleFraction "+
							"* (1 - rt.rampFraction) "+
					"FROM  "+
						"BracketScheduleHi bsh, "+
						"DriveScheduleFractionHi dsfh, "+
						"RoadType rt, "+
						"DriveScheduleAssoc dsa "+
					"WHERE  "+
						"(dsa.isRamp='N' OR dsa.isRamp='n') AND  "+
						"bsh.sourceTypeID = dsfh.sourceTypeID AND "+
						"bsh.roadTypeID = dsfh.roadTypeID AND "+
						"bsh.roadTypeID = rt.roadTypeID AND "+
						"bsh.roadTypeID = dsa.roadTypeID AND "+
						"rt.roadTypeID = dsa.roadTypeID AND  "+
						"bsh.sourceTypeID = dsa.SourceTypeID AND "+
						"bsh.driveScheduleID = dsfh.driveScheduleID AND "+
						"bsh.driveScheduleID = dsa.driveScheduleID ";
			SQLRunner.executeSQL(db, sql);

			sql = "insert ignore into DriveScheduleFraction (sourceTypeID, roadTypeID, hourDayID, driveScheduleID, isRamp, driveScheduleFraction)"
					+ " select tempSourceTypeID, roadTypeID, hourDayID, driveScheduleID, isRamp, driveScheduleFraction"
					+ " from DriveScheduleFraction"
					+ " inner join sourceUseTypePhysicsMapping on (realSourceTypeID=sourceTypeID)"
					+ " where tempSourceTypeID <> realSourceTypeID";
			SQLRunner.executeSQL(db,sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFraction");
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not determine the distribution of drive schedules for "
					+ "non ramp drive cycle.", sql);
		}
	}

	/**
	 *	OMDG-4: Calculate the second-by-second engine specific power.
	 * <p>This step calculates the engine specific power for each drive schedule for each source
	 * type.  This step could be limited to only those drive schedules needed for the specified
	 * source types and roadway types and indicated by the non-zero values for
	 * DriveScheduleFraction.
	**/
	void calculateEnginePowerBySecond() {
		String sql="";

		try {
			sql = "CREATE TABLE IF NOT EXISTS SourceTypeDriveSchedule ("+
					"sourceTypeID     SMALLINT,"+
					"driveScheduleID  SMALLINT,"+
					"UNIQUE INDEX XPKSourceTypeDriveSchedule ("+
							"sourceTypeID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE SourceTypeDriveSchedule";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO SourceTypeDriveSchedule ("+
						"sourceTypeID,"+
						"driveScheduleID) "+
					"SELECT "+
						"dsf.sourceTypeID,"+
						"dsf.driveScheduleID "+
					"FROM "+
						"DriveScheduleFraction dsf "+
					"GROUP BY "+
						"dsf.sourceTypeID,"+
						"dsf.driveScheduleID "+
					"HAVING "+
						"SUM(dsf.driveScheduleFraction) <> 0";
			SQLRunner.executeSQL(db, sql);

			sql = "insert ignore into SourceTypeDriveSchedule ( sourceTypeID, driveScheduleID)"
					+ " select tempSourceTypeID, driveScheduleID"
					+ " from SourceTypeDriveSchedule"
					+ " inner join sourceUseTypePhysicsMapping on (realSourceTypeID=sourceTypeID)"
					+ " where tempSourceTypeID <> realSourceTypeID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE SourceTypeDriveSchedule");

			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleFirstSecond ("+
					"driveScheduleID	SMALLINT,"+
					"second			SMALLINT,"+
					"UNIQUE INDEX XPKDriveScheduleFirstSecond ("+
							"driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleFirstSecond";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleFirstSecond ("+
						"driveScheduleID,"+
						"second) "+
					"SELECT "+
						"dss.driveScheduleID,"+
						"MIN(dss.second) "+
					"FROM "+
						"DriveScheduleSecond dss "+
					"GROUP BY "+
						"dss.driveScheduleID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFirstSecond");

			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleSecond2 ("+
					"driveScheduleID SMALLINT,"+
					"second          SMALLINT,"+
					"speed           FLOAT,"+
					"acceleration    FLOAT,"+
					"UNIQUE INDEX XPKDriveScheduleSecond2 ("+
							"driveScheduleID, second))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleSecond2";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleSecond2 ("+
						"driveScheduleID,"+
						"second,"+
						"speed,"+
						"acceleration) "+
					"SELECT "+
						"dsfs.driveScheduleID,"+
						"dsfs.second,"+
						"dss.speed * 0.44704,"+
						"0 "+
					"FROM "+
						"DriveScheduleFirstSecond dsfs,"+
						"DriveScheduleSecond dss "+
					"WHERE "+
						"dsfs.driveScheduleID = dss.driveScheduleID AND "+
						"dsfs.second = dss.second";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleSecond2");

			sql = "INSERT INTO DriveScheduleSecond2 ("+
						"driveScheduleID,"+
						"second,"+
						"speed,"+
						"acceleration) "+
					"SELECT "+
						"dss.driveScheduleID,"+
						"dss.second,"+
						"dss.speed * 0.44704,"+
						"(dss.speed - dss2.speed) * 0.44704 "+
					"FROM "+
						"DriveScheduleSecond dss,"+
						"DriveScheduleSecond dss2 "+
					"WHERE "+
						"dss.driveScheduleID = dss2.driveScheduleID AND "+
						"dss2.second = dss.second - 1";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleSecond2");

			sql = "CREATE TABLE IF NOT EXISTS VSP ("+
					"sourceTypeID    SMALLINT,"+
					"driveScheduleID SMALLINT,"+
					"second          SMALLINT,"+
					"VSP             FLOAT,"+
					"UNIQUE INDEX XPKESP ("+
							"sourceTypeID, driveScheduleID, second))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE VSP";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO VSP ("+
						"sourceTypeID,"+
						"driveScheduleID,"+
						"second,"+
						"VSP) "+
					"SELECT "+
						"stds.sourceTypeID,"+
						"stds.driveScheduleID,"+
						"dss2.second,"+
						"(sut.rollingTermA * dss2.speed +"+
								"sut.rotatingTermB * POW(dss2.speed,2) + "+
								"sut.dragTermC * POW(dss2.speed,3) + "+
								"sut.sourceMass * dss2.speed * "+
									// old line -  "dss2.acceleration) / sut.sourceMass "+
								"dss2.acceleration) / sut.fixedMassFactor "+
					"FROM "+				
						"SourceTypeDriveSchedule stds,"+
						"DriveScheduleSecond2 dss2,"+
						"sourceUseTypePhysicsMapping sut "+
					"WHERE "+
						"dss2.driveScheduleID = stds.driveScheduleID AND "+
						"sut.tempSourceTypeID = stds.sourceTypeID AND "+
						"sut.sourceMass <> 0 AND "+
						"dss2.second > 0";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE VSP");
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not calculate Engine Power Distribution.",sql);
		}
	}

	/**
	 * OMDG-5: Determine the operating mode bin for each second.
	 * <p>The ESP value for each second is compared to the upper and lower bounds for the
	 * operating mode bins and a bin ID is assigned to each second. This is done for each
	 * source type, drive schedule and second.</p>
	**/
	void determineOpModeIDPerSecond() {
		String sql = "";
		PreparedStatement statement = null;
		try {
			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleSecond3 ("+
					"driveScheduleID SMALLINT,"+
					"second          SMALLINT,"+
					"speed           FLOAT,"+
					"acceleration    FLOAT,"+
					"UNIQUE INDEX XPKDriveScheduleSecond3 ("+
							"driveScheduleID, second))";
			SQLRunner.executeSQL(db, sql);
			//System.out.println("######## Creating Drive Schedule Second 3 ##########");

			sql = "TRUNCATE DriveScheduleSecond3";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleSecond3 ("+
						"driveScheduleID,"+
						"second,"+
						"speed,"+
						"acceleration) "+
					"SELECT "+
						"dss.driveScheduleID,"+
						"dss.second,"+
						"dss.speed,"+
						"dss.speed - dss2.speed "+
					"FROM "+
						"DriveScheduleSecond dss,"+
						"DriveScheduleSecond dss2 "+
					"WHERE "+
						"dss.driveScheduleID = dss2.driveScheduleID AND "+
						"dss2.second = dss.second - 1";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleSecond3");

			sql = "CREATE TABLE IF NOT EXISTS OpModeIDBySecond ("+
					"sourceTypeID    SMALLINT,"+
					"driveScheduleID SMALLINT,"+
					"second          SMALLINT,"+
					"opModeID        SMALLINT,"+
					"polProcessID    INT,"+
					"speed           FLOAT,"+
					"acceleration    FLOAT,"+
					"VSP             FLOAT,"+
					"UNIQUE INDEX XPKOpModeIDBySecond ("+
							"sourceTypeID, driveScheduleID, second, polProcessID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE OpModeIDBySecond";
			SQLRunner.executeSQL(db, sql);
/*
			sql = "INSERT INTO OpModeIDBySecond ("+
						"sourceTypeID,"+
						"driveScheduleID,"+
						"second,"+
						"OpModeID,"+
						"speed,"+
						"acceleration,"+
						"VSP) "+
					"SELECT "+
						"v.sourceTypeID,"+
						"v.driveScheduleID,"+
						"v.second,"+
						"NULL,"+
						"dss.speed,"+
						"dss.acceleration,"+
						"v.VSP "+
					"FROM "+
						"RunSpecSourceType rsst,"+
						"DriveScheduleSecond3 dss,"+
						"VSP v "+
					"WHERE "+
						"v.sourceTypeID = rsst.sourceTypeID AND "+
						"dss.driveScheduleID = v.driveScheduleID AND "+
						"dss.second = v.second";
*/
			sql = "CREATE TABLE OMDGPollutantProcess ("
					+ " polProcessID int not null,"
					+ " unique index PKXOMDGPollutantProcess (polProcessID) )";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO OMDGPollutantProcess (polProcessID)"
					+ " SELECT DISTINCT polProcessID"
					+ " FROM OpModePolProcAssoc"; // RunSpecPollutantProcess
			SQLRunner.executeSQL(db, sql);

			// Note: We cannot remove anything that already has an operating mode distribution
			// because not all required links may have been provided.

			// Remove anything from OMDGPollutantProcess that has a representing pollutant/process.  Only
			// its representing item should be calculated.
			sql = "delete from OMDGPollutantProcess"
					+ " using OMDGPollutantProcess"
					+ " inner join OMDGPolProcessRepresented on (OMDGPollutantProcess.polProcessID = OMDGPolProcessRepresented.polProcessID)";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE OMDGPollutantProcess");

			sql = "CREATE TABLE IF NOT EXISTS OpModePolProcAssocTrimmed ("
					+ " 	polProcessID int NOT NULL DEFAULT '0',"
					+ " 	opModeID smallint(6) NOT NULL DEFAULT '0',"
					+ " 	PRIMARY KEY (opModeID,polProcessID),"
					+ " 	KEY (polProcessID),"
					+ " 	KEY (opModeID),"
					+ " 	KEY (opModeID,polProcessID)"
					+ " )";
			SQLRunner.executeSQL(db, sql);

			sql = "truncate OpModePolProcAssocTrimmed";
			SQLRunner.executeSQL(db, sql);

			sql = "insert into OpModePolProcAssocTrimmed (polProcessID, opModeID)"
					+ " select omp.polProcessID, omp.opModeID"
					+ " from OpModePolProcAssoc omp"
					+ " inner join OMDGPollutantProcess pp on pp.polProcessID=omp.polProcessID";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO OpModeIDBySecond ("+
						"sourceTypeID,"+
						"driveScheduleID,"+
						"second,"+
						"opModeID,"+
						"polProcessID,"+
						"speed,"+
						"acceleration,"+
						"VSP) "+
					"SELECT "+
						"v.sourceTypeID,"+
						"v.driveScheduleID,"+
						"v.second,"+
						"NULL,"+
						"rspp.polProcessID,"+
						"dss.speed,"+
						"dss.acceleration,"+
						"v.VSP "+
					"FROM "+
						"DriveScheduleSecond3 dss,"+
						"VSP v,"+
						"OMDGPollutantProcess rspp "+
					"WHERE "+
						"dss.driveScheduleID = v.driveScheduleID AND "+
						"dss.second = v.second";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE OpModeIDBySecond");

			sql = "DROP TABLE IF EXISTS OpModeIDBySecond_Temp";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE TABLE OpModeIDBySecond_Temp "+
					"SELECT " +
						"opid3.sourceTypeID, " +
						"opid3.driveScheduleID, " +
						"opid3.second, " +
						"0 AS OpModeID, " +
						"opid3.polProcessID, "+
						"opid3.speed, " +
						"opid3.acceleration, " +
						"opid3.vsp " +
					"FROM " +
						"OpModeIDBySecond opid1, " +
						"OpModeIDBySecond opid2, " +
						"OpModeIDBySecond opid3 " +
					"WHERE " +
						"opid1.sourceTypeID = opid2.sourceTypeID AND " +
						"opid2.sourceTypeID = opid3.sourceTypeID AND " +
						"opid1.driveScheduleID = opid2.driveScheduleID AND " +
						"opid2.driveScheduleID = opid3.driveScheduleID AND " +
						"opid1.polProcessID = opid2.polProcessID AND " +
						"opid2.polProcessID = opid3.polProcessID AND " +
						"opid1.second = opid2.second-1 AND " +
						"opid2.second = opid3.second-1 AND " +
						"(opid3.acceleration <= -2 OR (opid1.acceleration<-1 AND " +
						"opid2.acceleration<-1 AND " +
						"opid3.acceleration<-1))";
			SQLRunner.executeSQL(db, sql);

			sql = "REPLACE INTO OpModeIDBySecond ( "+
						"sourceTypeID, "+
						"driveScheduleID, "+
						"second, "+
						"OpModeID, "+
						"polProcessID, "+
						"speed, "+
						"acceleration, "+
						"VSP "+
						") "+
					"SELECT "+
						"sourceTypeID, "+
						"driveScheduleID, "+
						"second, "+
						"opModeID, "+
						"polProcessID, "+
						"speed, "+
						"acceleration, "+
						"VSP "+
					"FROM "+
						"OpModeIDBySecond_Temp";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE OpModeIDBySecond");

			sql = "UPDATE OpModeIDBySecond SET OpModeID=0 WHERE acceleration <= -2";
			SQLRunner.executeSQL(db, sql);

			sql = "SELECT om.OpModeID, polProcessID, VSPLower, VSPUpper, speedLower, speedUpper " +
					"FROM OperatingMode om INNER JOIN OpModePolProcAssocTrimmed USING (opModeID) " +
					"WHERE om.opModeID > 1 AND om.opModeID < 100";
			statement = db.prepareStatement(sql);
			ResultSet result = SQLRunner.executeQuery(statement, sql);
			while(result.next()) {
				int opModeID = result.getInt(1);
				int polProcessID = result.getInt(2);
				float vspLower = result.getFloat(3);
				boolean isVSPLowerNull = result.wasNull();
				float vspUpper = result.getFloat(4);
				boolean isVSPUpperNull = result.wasNull();
				float speedLower = result.getFloat(5);
				boolean isSpeedLowerNull = result.wasNull();
				float speedUpper = result.getFloat(6);
				boolean isSpeedUpperNull = result.wasNull();
				sql = "UPDATE OpModeIDBySecond SET opModeID = " + opModeID;
				String whereClause = "";
				String vspClause = "";
				String speedClause = "";

				if(!isVSPLowerNull) {
					vspClause += "VSP >= " + vspLower;
				}
				if(!isVSPUpperNull) {
					if(vspClause.length() > 0) {
						vspClause += " AND ";
					}
					vspClause += "VSP < " + vspUpper;
				}
				if(!isSpeedLowerNull) {
					speedClause += "speed >= " + speedLower;
				}
				if(!isSpeedUpperNull) {
					if(speedClause.length() > 0) {
						speedClause += " AND ";
					}
					speedClause += "speed < " + speedUpper;
				}
				if(vspClause.length() > 0) {
					whereClause += "(" + vspClause + ")";
				}
				if(speedClause.length() > 0) {
					if(whereClause.length() > 0) {
						whereClause += " AND ";
					}
					whereClause += "(" + speedClause + ")";
				}
				sql += " WHERE " + whereClause + " AND polProcessID = " + polProcessID + 
						" AND OpModeID IS NULL";
				SQLRunner.executeSQL(db, sql);
			}
			statement.close();
			statement = null;
			result.close();

			sql = "UPDATE OpModeIDBySecond SET OpModeID=IF(speed=0 and polProcessID=11609,501,if(speed<1.0,1,opModeID))";
			SQLRunner.executeSQL(db, sql);
		} catch (SQLException e) {
			Logger.logSqlError(e, "Could not determine Operating Mode ID distribution.", sql);
		} finally {
			if(statement!=null) {
				try {
					statement.close();
				} catch (SQLException e) {
					// Failure to close on a preparedStatement should not be an issue.
				}
			}
		}
	}

	/**
	 * OMDG-6: Calculate operating mode fractions for each drive schedule.
	 * <p>Once all the seconds in each operating mode bin are known, the distribution of the bins
	 * can be determined. The sum of the operating mode fractions will add to one for each source
	 * type and drive schedule combination.  This is done for each source type and drive schedule.
	 * </p>
	**/
	void calculateOpModeFractionsPerDriveSchedule() {
		String sql = "";

		try {
			sql = "CREATE TABLE IF NOT EXISTS OpModeFractionBySchedule2 ("+
					"sourceTypeID       SMALLINT,"+
					"driveScheduleID    SMALLINT,"+
					"polProcessID       INT,"+
					"secondSum          SMALLINT,"+
					"UNIQUE INDEX XPKOpModeFractionBySchedule2 ("+
							"sourceTypeID, driveScheduleID, polProcessID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE OpModeFractionBySchedule2";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO OpModeFractionBySchedule2 ("+
						"sourceTypeID,"+
						"driveScheduleID,"+
						"polProcessID,"+
						"secondSum) "+
					"SELECT "+
						"omis.sourceTypeID,"+
						"omis.driveScheduleID,"+
						"omis.polProcessID,"+
						"count(*) "+
					"FROM "+
						" OpModeIDBySecond omis "+
					"GROUP BY "+
						"omis.sourceTypeID,"+
						"omis.driveScheduleID,"+
						"omis.polProcessID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE OpModeFractionBySchedule2");

			sql = "CREATE TABLE IF NOT EXISTS OpModeFractionBySchedule ("+
					"sourceTypeID      SMALLINT,"+
					"driveScheduleID   SMALLINT,"+
					"polProcessID      INT,"+
					"opModeID          SMALLINT,"+
					"modeFraction      FLOAT,"+
					"UNIQUE INDEX XPKOpModeFractionBySchedule ("+
							"sourceTypeID, driveScheduleID, polProcessID, opModeID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE OpModeFractionBySchedule";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO OpModeFractionBySchedule ("+
						"sourceTypeID,"+
						"driveScheduleID,"+
						"polProcessID,"+
						"opModeID,"+
						"modeFraction) "+
					"SELECT "+
						"omis.sourceTypeID,"+
						"omis.driveScheduleID,"+
						"omis.polProcessID,"+
						"omis.opModeID,"+
						"count(*) / omfs2.secondSum "+
					"FROM "+
						"OpModeIDBySecond omis,"+
						"OpModeFractionBySchedule2 omfs2 "+
					"WHERE "+
						"omis.sourceTypeID = omfs2.sourceTypeID AND "+
						"omis.driveScheduleID = omfs2.driveScheduleID AND "+
						"omis.polProcessID = omfs2.polProcessID AND "+
						"omfs2.secondSum <> 0 "+
					"GROUP BY "+
						"omis.sourceTypeID,"+
						"omis.driveScheduleID,"+
						"omis.polProcessID,"+
						"omis.opModeID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE OpModeFractionBySchedule");

			sql = "TRUNCATE OpModeFractionBySchedule2";
			SQLRunner.executeSQL(db, sql);
		} catch (SQLException e) {
			Logger.logSqlError(e, "Could not determine fractions of Operating Modes per Drive"
					+ " Schedule", sql);
		}
	}

	/**
	 * Preliminary steps for OMDG-7: Calculate overall operating mode fractions.
	 * <p>The overall operating mode fractions are calculated by weighting the operating mode
	 * fractions of each drive schedule by the drive schedule fractions. This is done for each
	 * source type, road type, day of the week, hour of the day and operating mode. This generator
	 * only applies to the running process of the total energy pollutant.</p>
	**/
	void preliminaryCalculateOpModeFractions() {
		String sql = "";
		try {
			String[] statements = {
				"drop table if exists OpModeFraction2",
				"drop table if exists OpModeFraction2a",

				"CREATE TABLE IF NOT EXISTS OpModeFraction2 ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"hourDayID         SMALLINT,"+
					"opModeID          SMALLINT,"+
					"polProcessID      INT,"+
					"opModeFraction    FLOAT,"+
					"UNIQUE INDEX XPKOpModeFraction2 ("+
							"roadTypeID, sourceTypeID, hourDayID, opModeID, polProcessID))",
				// NOTE: above, roadTypeID is the first in the index so that it will be used
				// in calculateOpModeFractions which joins based on roadTypeID only.

				"CREATE TABLE IF NOT EXISTS OpModeFraction2a ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"hourDayID         SMALLINT,"+
					"opModeID          SMALLINT,"+
					"polProcessID      INT,"+
					"opModeFraction    FLOAT,"+
					"INDEX IdxOpModeFraction2a ("+
							"roadTypeID, sourceTypeID, hourDayID, opModeID, polProcessID))",

				"TRUNCATE OpModeFraction2",
				"TRUNCATE OpModeFraction2a",

				// Add ramp-based information (which has already been scaled by rampFraction)
				"INSERT INTO OpModeFraction2a ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"hourDayID,"+
						"opModeID,"+
						"polProcessID,"+
						"opModeFraction) "+
					"SELECT "+
						"ropm.sourceTypeID,"+
						"ropm.roadTypeID,"+
						"ropm.hourDayID,"+
						"ropm.opModeID,"+
						"omppa.polProcessID,"+
						"ropm.opModeFraction "+
					"FROM "+
						"OMDGRampOpMode ropm,"+
						"OpModePolProcAssocTrimmed omppa " +
					"WHERE "+
						"omppa.opModeID = ropm.opModeID AND "+
						"omppa.polProcessID not in (11710) ",

				// Add non-ramp-based information (which has already been scaled by 1-rampFraction)
				"INSERT INTO OpModeFraction2a ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"hourDayID,"+
						"opModeID,"+
						"polProcessID,"+
						"opModeFraction) "+
					"SELECT "+
						"dsf.sourceTypeID,"+
						"dsf.roadTypeID,"+
						"dsf.hourDayID,"+
						"omfs.opModeID,"+
						"omppa.polProcessID,"+
						"sum(omfs.modeFraction * dsf.driveScheduleFraction) as opModeFraction "+
					"FROM "+
						"DriveScheduleFraction dsf,"+
						"OpModeFractionBySchedule omfs, "+
						"OpModePolProcAssocTrimmed omppa " +
					"WHERE "+
						"dsf.sourceTypeID = omfs.sourceTypeID AND "+
						"dsf.driveScheduleID = omfs.driveScheduleID AND "+
						"omfs.polProcessID = omppa.polProcessID AND "+
						"omppa.opModeID = omfs.opModeID AND " +
						"omppa.polProcessID not in (11710) " +
					"GROUP BY "+
						"dsf.sourceTypeID,"+
						"dsf.roadTypeID,"+
						"dsf.hourDayID,"+
						"omppa.polProcessID,"+
						"omfs.opModeID",

				"ANALYZE TABLE OpModeFraction2a",

				// Aggregate ramp and non-ramp data
				"INSERT INTO OpModeFraction2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"hourDayID,"+
						"opModeID,"+
						"polProcessID,"+
						"opModeFraction) "+
					"SELECT "+
						"sourceTypeID,"+
						"roadTypeID,"+
						"hourDayID,"+
						"opModeID,"+
						"polProcessID,"+
						"sum(opModeFraction) as opModeFraction "+
					"FROM "+
						"OpModeFraction2a "+
					"GROUP BY "+
						"roadTypeID, sourceTypeID, hourDayID, opModeID, polProcessID",

				"ANALYZE TABLE OpModeFraction2"
			};

			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine final Operating Mode Distribution.",sql);
		}
	}

	/**
	 * OMDG-7: Calculate overall operating mode fractions.
	 * <p>The overall operating mode fractions are calculated by weighting the operating mode
	 * fractions of each drive schedule by the drive schedule fractions. This is done for each
	 * source type, road type, day of the week, hour of the day and operating mode. This generator
	 * only applies to the running process of the total energy pollutant.</p>
	 * @param linkID The link being processed.
	**/
	void calculateOpModeFractions(int linkID) {
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			// Find [sourceTypeID, linkID, hourDayID, polProcessID] combinations
			// that already exist within OpModeDistribution.
			sql = "create table if not exists OMDGOMDKeys ( "+
					"	sourceTypeID smallint(6) NOT NULL DEFAULT '0', "+
					"	hourDayID smallint(6) NOT NULL DEFAULT '0', "+
					"	polProcessID int NOT NULL DEFAULT '0', "+
					"	primary key (hourdayID, polProcessID, sourceTypeID) "+
					")";
			SQLRunner.executeSQL(db, sql);

			sql = "truncate table OMDGOMDKeys";
			SQLRunner.executeSQL(db, sql);

			sql = "insert into OMDGOMDKeys (sourceTypeID, hourDayID, polProcessID) "+
					"select distinct sourceTypeID, hourDayID, polProcessID "+
					"from OpModeDistribution "+
					"where linkID = " + linkID;
			SQLRunner.executeSQL(db, sql);

			// Add to OpModeDistribution those entries that don't already have records
			sql = "INSERT IGNORE INTO OpModeDistribution ("+
						"sourceTypeID,"+
						"linkID,"+
						"hourDayID,"+
						"polProcessID,"+
						"opModeID,"+
						"opModeFraction) "+
					"SELECT "+
						"omf2.sourceTypeID,"+
						"l.linkID,"+
						"omf2.hourDayID,"+
						"omf2.polProcessID,"+
						"omf2.opModeID,"+
						"omf2.opModeFraction "+
					"FROM "+
						"OpModeFraction2 omf2 "+
						"inner join Link l on l.roadTypeID=omf2.roadtypeID "+
						"left outer join OMDGOMDKeys k on ( "+
							"k.hourDayID=omf2.hourDayID "+
							"and k.polProcessID=omf2.polProcessID "+
							"and k.sourceTypeID=omf2.sourceTypeID "+
						") "+
					"WHERE "+
						"k.hourDayID is null "+
						"and k.polProcessID is null "+
						"and k.sourceTypeID is null "+
						"and l.linkID =" + linkID;
			SQLRunner.executeSQL(db, sql);

			// Copy representing entries to those being represented, but only if those
			// being represented are not already present.
			sql = "truncate table OMDGOMDKeys";
			SQLRunner.executeSQL(db, sql);

			sql = "insert into OMDGOMDKeys (sourceTypeID, hourDayID, polProcessID) "+
					"select distinct sourceTypeID, hourDayID, polProcessID "+
					"from OpModeDistribution "+
					"where linkID = " + linkID;
			SQLRunner.executeSQL(db, sql);

			ArrayList<String> ppaList = new ArrayList<String>();
			ArrayList<String> repPPAList = new ArrayList<String>();
			sql = "select polProcessID, representingPolProcessID"
					+ " from OMDGPolProcessRepresented";
			query.open(db,sql);
			while(query.rs.next()) {
				ppaList.add(query.rs.getString(1));
				repPPAList.add(query.rs.getString(2));
			}
			query.close();
			for(int i=0;i<ppaList.size();i++) {
				String ppa = ppaList.get(i);
				String repPPA = repPPAList.get(i);

				sql = "INSERT IGNORE INTO OpModeDistribution ("+
							"sourceTypeID,"+
							"linkID,"+
							"hourDayID,"+
							"polProcessID,"+
							"opModeID,"+
							"opModeFraction) "+
						"SELECT "+
							"omf2.sourceTypeID,"+
							"l.linkID,"+
							"omf2.hourDayID,"+
							ppa + " as polProcessID,"+
							"omf2.opModeID,"+
							"omf2.opModeFraction "+
						"FROM "+
							"OpModeFraction2 omf2 "+
							"inner join Link l on l.roadTypeID=omf2.roadtypeID "+
							"left outer join OMDGOMDKeys k on ( "+
								"k.hourDayID=omf2.hourDayID "+
								"and k.polProcessID=" + ppa + " "+
								"and k.sourceTypeID=omf2.sourceTypeID "+
							") "+
						"WHERE "+
							"k.hourDayID is null "+
							"and k.polProcessID is null "+
							"and k.sourceTypeID is null "+
							"and l.linkID =" + linkID+ " "+
							"and omf2.polProcessID =" + repPPA;
				SQLRunner.executeSQL(db, sql);
			}

			modelYearPhysics.updateOperatingModeDistribution(db,"OpModeDistribution");
			SQLRunner.executeSQL(db,"ANALYZE TABLE OpModeDistribution");

			// Get distinct polProcessID in OpModeDistributionTemp as these are the ones to 
			// be cleaned out of OpModeDistribution
			sql = "SELECT DISTINCT polProcessID FROM OpModeFraction2";
			polProcessIDs = "";
			query.open(db,sql);
			while(query.rs.next()) {
				if(polProcessIDs.length() > 0) {
					polProcessIDs += ",";
				}
				polProcessIDs += query.rs.getString(1);
			}
			query.close();
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine final Operating Mode Distribution.",sql);
		} finally {
			query.onFinally();
		}
	}
}
