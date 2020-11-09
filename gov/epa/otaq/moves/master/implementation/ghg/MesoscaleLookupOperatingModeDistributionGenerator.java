/**************************************************************************************************
 * @(#)MesoscaleLookupOperatingModeDistributionGenerator.java
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
 * This builds "Operating Mode Distribution" records for ELDB data.
 * ELDB is the Execution Location Database explained in TotalActivityGenerator
 *
 * @author		Wesley Faler
 * @author		W. Aikman
 * @version		2014-05-28
**/
public class MesoscaleLookupOperatingModeDistributionGenerator extends Generator {
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
	public MesoscaleLookupOperatingModeDistributionGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		EmissionProcess process = EmissionProcess.findByName("Running Exhaust");

		targetLoop.subscribe(this, process, MasterLoopGranularity.YEAR, // LINK. Year level for source bins from SBDG.
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
				bracketAverageSpeedBins();
				determineDriveScheduleProportions();
				if(!validateDriveScheduleDistribution()) {
					isValid = false;
				}
				if(isValid) {
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

		Logger.log(LogMessageCategory.INFO,"MESLKOMDG setupTime=" + setupTime + " totalTime=" + totalTime);
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
				//System.out.println("########## DELETING MESLKOPD ###### : " +
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
						"UNIQUE INDEX XPKBracketScheduleLo2 ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BracketScheduleLo2";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO BracketScheduleLo2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"loScheduleSpeed) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgSpeedBinID,"+
						"MAX(ds.averageSpeed) "+
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
						"ds.averageSpeed <= asb.avgBinSpeed "+
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
						"loScheduleSpeed) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgSpeedBinID,"+
						"dsb.scheduleBoundLo "+
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
						"asb.avgBinSpeed < dsb.scheduleBoundLo";
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
						"ds.averageSpeed = bsl.loScheduleSpeed";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleLo");

			//
			// Delete intermediate results for large tables. Normally, intermediate
			// results are kept when possible for debugging purposes.
			sql = "TRUNCATE BracketScheduleLo2";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE TABLE IF NOT EXISTS BracketScheduleHi2 ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"avgSpeedBinID     SMALLINT,"+
					"driveScheduleID   SMALLINT,"+
					"hiScheduleSpeed   FLOAT,"+
					"UNIQUE INDEX XPKBracketScheduleHi2 ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BracketScheduleHi2";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO BracketScheduleHi2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"hiScheduleSpeed) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgSpeedBinID,"+
						"MIN(ds.averageSpeed) "+
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
						"ds.averageSpeed > asb.avgBinSpeed "+
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
						"hiScheduleSpeed) "+
					"SELECT "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgSpeedBinID,"+
						"dsb.scheduleBoundHi "+
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
						"asb.avgBinSpeed > dsb.scheduleBoundHi";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleHi2");

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
						"ds.averageSpeed = bsl.hiScheduleSpeed";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleHi");

			//
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
					// Failure to close on a preparedStatment should not be an issue.
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
		String[] statements = {
			"CREATE TABLE IF NOT EXISTS LoScheduleFraction ("+
					"sourceTypeId       SMALLINT,"+
					"roadTypeId         SMALLINT,"+
					"avgSpeedBinID      SMALLINT,"+
					"loScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKLoScheduleFraction ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID))",
			"TRUNCATE LoScheduleFraction",
			"CREATE TABLE IF NOT EXISTS HiScheduleFraction ("+
					"sourceTypeId       SMALLINT,"+
					"roadTypeId         SMALLINT,"+
					"avgSpeedBinID      SMALLINT,"+
					"hiScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKHiScheduleFraction ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID))",
			"TRUNCATE HiScheduleFraction",

			"INSERT INTO LoScheduleFraction ("+
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
						"bsh.hiScheduleSpeed <> bsl.loScheduleSpeed",
			"ANALYZE TABLE LoScheduleFraction",
			"INSERT INTO LoScheduleFraction ("+
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
						"bsh.hiScheduleSpeed = bsl.loScheduleSpeed",
			"ANALYZE TABLE LoScheduleFraction",
			"INSERT INTO HiScheduleFraction ("+
						"sourceTypeId,"+
						"roadTypeId,"+
						"avgSpeedBinID,"+
						"hiScheduleFraction) "+
					"SELECT "+
						"bsh.sourceTypeId,"+
						"bsh.roadTypeId,"+
						"bsh.avgSpeedBinID,"+
						"(1 - lsf.loScheduleFraction) "+
					"FROM "+
						"BracketScheduleHi bsh,"+
						"LoScheduleFraction lsf "+
					"WHERE "+
						"lsf.sourceTypeId = bsh.sourceTypeId AND "+
						"lsf.roadTypeId = bsh.roadTypeId AND "+
						"lsf.avgSpeedBinID = bsh.avgSpeedBinID",
			"ANALYZE TABLE HiScheduleFraction"
		};
		String sql = "";
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db, sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine fraction of drive schedules in each "
					+ "speed bin.", sql);
		}
	}

	/**
	 * Validate drive schedule proportions befor determining drive schedule distribution.
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
			sql = "SELECT DISTINCT dsa.SourceTypeID, dsa.RoadTypeID "
					+ " FROM DriveScheduleAssoc dsa, RunSpecRoadType rsrt, "
					+ " RunSpecSourceType rsst "
					+ " WHERE dsa.roadTypeID = rsrt.roadTypeID"
					+ " AND dsa.sourceTypeID = rsst.sourceTypeID"
					+ " ORDER BY dsa.SourceTypeID, dsa.RoadTypeID";
			statement = db.prepareStatement(sql);
			result = SQLRunner.executeQuery(statement, sql);
			lastSourceType = -1;
			lastRoadType = -1;
			boolean hasNonRamp = false;
			while(result.next()) {
				int sourceType = result.getInt(1);
				int roadType = result.getInt(2);
				if(lastSourceType != sourceType || lastRoadType != roadType) {
					if(lastSourceType >= 0 && !hasNonRamp) {
						Logger.log(LogMessageCategory.ERROR,
								"No drive schedule for road type " + lastRoadType + " and "
								+ "source type " + lastSourceType);
						return false;
					}
					lastSourceType = sourceType;
					lastRoadType = roadType;
					hasNonRamp = true;
				} else {
					hasNonRamp = true;
				}
			}
			if(lastSourceType >= 0 && !hasNonRamp) {
				Logger.log(LogMessageCategory.ERROR,
						"No drive schedule for road type " + lastRoadType + " and "
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
					"avgSpeedBinID         SMALLINT,"+
					"driveScheduleID       SMALLINT,"+
					"driveScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKDriveScheduleFractionLo ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleFractionLo";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleFractionLo ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"driveScheduleID,"+
						"driveScheduleFraction) "+
					"SELECT "+
						"bsl.sourceTypeID,"+
						"bsl.roadTypeID,"+
						"lsf.avgSpeedBinID,"+
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
						"lsf.avgSpeedBinID,"+
						"bsl.driveScheduleId";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFractionLo");

			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleFractionHi ( "+
					"sourceTypeID          SMALLINT, "+
					"roadTypeID            SMALLINT, "+
					"avgSpeedBinID         SMALLINT,"+
					"driveScheduleID       SMALLINT, "+
					"driveScheduleFraction FLOAT, "+
					"UNIQUE INDEX XPKDriveScheduleFractionHi ( "+
					"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleFractionHi";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO DriveScheduleFractionHi ( "+
						"sourceTypeID, "+
						"roadTypeID, "+
						"avgSpeedBinID, "+
						"driveScheduleID, "+
						"driveScheduleFraction) "+
					"SELECT "+
						"bsh.sourceTypeID, "+
						"bsh.roadTypeID, "+
						"hsf.avgSpeedBinID, "+
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
						"hsf.avgSpeedBinID, "+
						"bsh.driveScheduleId";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFractionHi");

			sql = "CREATE TABLE IF NOT EXISTS DriveScheduleFraction ("+
					"sourceTypeID          SMALLINT,"+
					"roadTypeID            SMALLINT,"+
					"avgSpeedBinID         SMALLINT,"+
					"driveScheduleID       SMALLINT,"+
					"driveScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKDriveScheduleFraction ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);
			// changed to INSERT IGNORE to work with MySQL 4, 
			// because DriveScheduleFractionLo and Hi tables have multiple speed bins
			sql = "INSERT IGNORE INTO DriveScheduleFraction ( "+
						"sourceTypeID, "+
						"roadTypeID, "+
						"avgSpeedBinID, "+
						"driveScheduleID, "+
						"driveScheduleFraction) "+
					"SELECT  "+
						"bsh.sourceTypeID, "+
						"bsh.roadTypeID, "+
						"dsfh.avgSpeedBinID, "+
						"bsh.driveScheduleID, "+
						"(dsfl.driveScheduleFraction + dsfh.driveScheduleFraction) "+
					"FROM  "+
						"BracketScheduleHi bsh, "+
						"DriveScheduleFractionLo dsfl, "+
						"DriveScheduleFractionHi dsfh, "+
						"RoadType rt, "+
						"DriveScheduleAssoc dsa "+
					"WHERE  "+
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
						"dsfl.avgSpeedBinID = dsfh.avgSpeedBinID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFraction");

			sql = "INSERT IGNORE INTO DriveScheduleFraction ( "+
						"sourceTypeID, "+
						"roadTypeID, "+
						"avgSpeedBinID, "+
						"driveScheduleID, "+
						"driveScheduleFraction) "+
					"SELECT  "+
						"bsl.sourceTypeID, "+
						"bsl.roadTypeID, "+
						"dsfl.avgSpeedBinID, "+
						"bsl.driveScheduleID, "+
						"dsfl.driveScheduleFraction "+
					"FROM  "+
						"BracketScheduleLo bsl, "+
						"DriveScheduleFractionLo dsfl, "+
						"RoadType rt, "+
						"DriveScheduleAssoc dsa "+
					"WHERE  "+
						"bsl.sourceTypeID = dsfl.sourceTypeID AND "+
						"bsl.roadTypeID = dsfl.roadTypeID AND  "+
						"bsl.avgSpeedBinID = dsfl.avgSpeedBinID AND "+
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
						"avgSpeedBinID, "+
						"driveScheduleID, "+
						"driveScheduleFraction) "+
					"SELECT  "+
						"bsh.sourceTypeID, "+
						"bsh.roadTypeID, "+
						"dsfh.avgSpeedBinID, "+
						"bsh.driveScheduleID, "+
						"dsfh.driveScheduleFraction "+
					"FROM  "+
						"BracketScheduleHi bsh, "+
						"DriveScheduleFractionHi dsfh, "+
						"RoadType rt, "+
						"DriveScheduleAssoc dsa "+
					"WHERE  "+
						"bsh.sourceTypeID = dsfh.sourceTypeID AND "+
						"bsh.roadTypeID = dsfh.roadTypeID AND "+
						"bsh.avgSpeedBinID = dsfh.avgSpeedBinID AND "+
						"bsh.roadTypeID = rt.roadTypeID AND "+
						"bsh.roadTypeID = dsa.roadTypeID AND "+
						"rt.roadTypeID = dsa.roadTypeID AND  "+
						"bsh.sourceTypeID = dsa.SourceTypeID AND "+
						"bsh.driveScheduleID = dsfh.driveScheduleID AND "+
						"bsh.driveScheduleID = dsa.driveScheduleID ";
			SQLRunner.executeSQL(db, sql);

			sql = "insert ignore into DriveScheduleFraction (sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID, driveScheduleFraction)"
					+ " select tempSourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID, driveScheduleFraction"
					+ " from DriveScheduleFraction"
					+ " inner join sourceUseTypePhysicsMapping on (realSourceTypeID=sourceTypeID)"
					+ " where tempSourceTypeID <> realSourceTypeID";
			SQLRunner.executeSQL(db,sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleFraction");
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not determine the distribution of drive schedules for "
					+ "a drive cycle.", sql);
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
							// old line	"dss2.acceleration) / sut.sourceMass "+
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
					"FROM OperatingMode om INNER JOIN OpModePolProcAssoc USING (opModeID) " +
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
			SQLRunner.executeSQL(db,sql);
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
				"drop table if exists OpModeFraction2b",

				"CREATE TABLE IF NOT EXISTS OpModeFraction2 ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"avgSpeedBinID     SMALLINT,"+
					"hourDayID         SMALLINT,"+
					"opModeID          SMALLINT,"+
					"polProcessID      INT,"+
					"opModeFraction    FLOAT,"+
					"UNIQUE INDEX XPKOpModeFraction2 ("+
							"roadTypeID, avgSpeedBinID, sourceTypeID, hourDayID, opModeID, polProcessID))",
				// NOTE: above, roadTypeID is the first in the index so that it will be used
				// in calculateOpModeFractions which joins based on roadTypeID only.

				"CREATE TABLE IF NOT EXISTS OpModeFraction2a ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"avgSpeedBinID     SMALLINT,"+
					"hourDayID         SMALLINT,"+
					"opModeID          SMALLINT,"+
					"polProcessID      INT,"+
					"opModeFraction    FLOAT,"+
					"INDEX IdxOpModeFraction2a ("+
							"roadTypeID, avgSpeedBinID, sourceTypeID, hourDayID, opModeID, polProcessID))",

				"CREATE TABLE IF NOT EXISTS OpModeFraction2b ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"avgSpeedBinID     SMALLINT,"+
					"opModeID          SMALLINT,"+
					"polProcessID      INT,"+
					"opModeFraction    FLOAT)",

				"TRUNCATE OpModeFraction2",
				"TRUNCATE OpModeFraction2a",
				"TRUNCATE OpModeFraction2b",

				// Add road information
				"INSERT INTO OpModeFraction2b ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"opModeID,"+
						"polProcessID,"+
						"opModeFraction) "+
					"SELECT "+
						"dsf.sourceTypeID,"+
						"dsf.roadTypeID,"+
						"dsf.avgSpeedBinID,"+
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
						"dsf.avgSpeedBinID,"+
						"omppa.polProcessID,"+
						"omfs.opModeID",

				"INSERT INTO OpModeFraction2a ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"hourDayID,"+
						"opModeID,"+
						"polProcessID,"+
						"opModeFraction) "+
					"SELECT sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, opModeID, polProcessID, opModeFraction "+
					"FROM OpModeFraction2b,"+
						"RunSpecHourDay",

				"ANALYZE TABLE OpModeFraction2a",

				// Aggregate data from OpModeFraction2a
				"INSERT INTO OpModeFraction2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"hourDayID,"+
						"opModeID,"+
						"polProcessID,"+
						"opModeFraction) "+
					"SELECT "+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"hourDayID,"+
						"opModeID,"+
						"polProcessID,"+
						"sum(opModeFraction) as opModeFraction "+
					"FROM "+
						"OpModeFraction2a "+
					"GROUP BY "+
						"roadTypeID, avgSpeedBinID, sourceTypeID, hourDayID, opModeID, polProcessID",

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
		//System.out.println("calculateOpModeFractions(" + linkID + ")");
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
						"inner join Link l on ("+
							"l.roadTypeID=omf2.roadtypeID "+
							"and omf2.avgSpeedBinID = Mod(l.linkID,100) "+
							") "+
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
							"inner join Link l on ("+
								"l.roadTypeID=omf2.roadtypeID "+
								"and omf2.avgSpeedBinID = Mod(l.linkID,100) "+
								") "+
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
