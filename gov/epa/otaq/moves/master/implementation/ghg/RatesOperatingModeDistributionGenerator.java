/**************************************************************************************************
 * @(#)RatesOperatingModeDistributionGenerator.java
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
 * @version		2017-07-04
**/
public class RatesOperatingModeDistributionGenerator extends Generator {
	// true when the external generator should be invoked for some of the algorithms
	public static final boolean USE_EXTERNAL_GENERATOR = true;
	public static final boolean USE_EXTERNAL_GENERATOR_FOR_DRIVE_CYCLES = true;

	/**
	 * @algorithm
	 * @owner Rates Operating Mode Distribution Generator
	 * @generator
	**/

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
	/** true when a Project simulation is being used **/
	boolean isProjectDomain = false;
	/**
	 * True when the BaseRateGenerator's MasterLoop subscription has been checked.
	 * Significant speedups are possible by combining the RatesOpModeDistributionGenerator
	 * and the BaseRateGenerator in the external generator code.
	**/
	boolean didCheckForBaseRateGenerator = false;
	/** True when external generator steps should be combined with the BaseRateGenerator's steps. **/
	boolean shouldDeferForBaseRateGenerator = false;
	/** MasterLoop object that owns and executes this generator **/
	MasterLoop ownerLoop;

	/** Default constructor **/
	public RatesOperatingModeDistributionGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		ownerLoop = targetLoop;
		isProjectDomain = ExecutionRunSpec.theExecutionRunSpec.getModelDomain() == ModelDomain.PROJECT;
		String[] processNames = {
			(isProjectDomain? null : "Running Exhaust"), // Don't do Running Exhaust in project domain.
			"Extended Idle Exhaust",
			"Auxiliary Power Exhaust"
		};
		for(int i=0;i<processNames.length;i++) {
			if(processNames[i] == null) {
				continue;
			}
			EmissionProcess process = EmissionProcess.findByName(processNames[i]);
			if(process != null) {
				targetLoop.subscribe(this, process, MasterLoopGranularity.YEAR, // Year level for source bins from SBDG.
						MasterLoopPriority.GENERATOR);
			}
		}
	}

	/**
	 * Called each time the link changes.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
		// Look ahead to see if the BaseRateGenerator will be used as well.
		// If it will, and if it too will use the external generator, then
		// performance is improved by combining steps in this generator with
		// steps in BaseRateGenerator.
		if(!didCheckForBaseRateGenerator) {
			didCheckForBaseRateGenerator = true;
			shouldDeferForBaseRateGenerator = false;
			if(USE_EXTERNAL_GENERATOR && BaseRateGenerator.USE_EXTERNAL_GENERATOR) {
				ArrayList<MasterLoopable> loopables = ownerLoop.getSubscribers();
				for(MasterLoopable ml : loopables) {
					if(ml instanceof BaseRateGenerator) {
						shouldDeferForBaseRateGenerator = true;
						break;
					}
				}
			}
		}

		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

			long start, detailStart, detailEnd;

			// The following only has to be done once for each run.
			if(!hasBeenSetup) {
				start = System.currentTimeMillis();
				/**
				 * @step 010
				 * @algorithm Setup for Model Year Physics effects.
				**/
				modelYearPhysics.setup(db);
				if(isProjectDomain || USE_EXTERNAL_GENERATOR_FOR_DRIVE_CYCLES) {
					hasBeenSetup = true;
				} else {
					detailStart = System.currentTimeMillis();
					bracketAverageSpeedBins(); // steps 100-109
					detailEnd = System.currentTimeMillis();
					Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.bracketAverageSpeedBins ms="+(detailEnd-detailStart));
					
					detailStart = System.currentTimeMillis();
					determineDriveScheduleProportions(); // steps 110-119
					detailEnd = System.currentTimeMillis();
					Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.determineDriveScheduleProportions ms="+(detailEnd-detailStart));

					if(isValid) {
						detailStart = System.currentTimeMillis();
						determineDriveScheduleDistributionNonRamp(); // steps 130-139
						detailEnd = System.currentTimeMillis();
						Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.determineDriveScheduleDistributionNonRamp ms="+(detailEnd-detailStart));

						detailStart = System.currentTimeMillis();
						calculateEnginePowerBySecond(); // steps 140-149
						detailEnd = System.currentTimeMillis();
						Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.calculateEnginePowerBySecond ms="+(detailEnd-detailStart));

						detailStart = System.currentTimeMillis();
						determineOpModeIDPerSecond(); // steps 150-159
						detailEnd = System.currentTimeMillis();
						Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.determineOpModeIDPerSecond ms="+(detailEnd-detailStart));

						detailStart = System.currentTimeMillis();
						calculateOpModeFractionsPerDriveSchedule(); // steps 160-169
						detailEnd = System.currentTimeMillis();
						Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.calculateOpModeFractionsPerDriveSchedule ms="+(detailEnd-detailStart));

						detailStart = System.currentTimeMillis();
						preliminaryCalculateOpModeFractions(); // steps 170-179
						detailEnd = System.currentTimeMillis();
						Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.preliminaryCalculateOpModeFractions ms="+(detailEnd-detailStart));

						hasBeenSetup = true;
					}
				}
				setupTime += System.currentTimeMillis() - start;
			}

			start = System.currentTimeMillis();
			if(isValid) {
				String alreadyKey = "calc|" + inContext.iterProcess.databaseKey;
				if(!alreadyDoneFlags.contains(alreadyKey)) {
					alreadyDoneFlags.add(alreadyKey);

					detailStart = System.currentTimeMillis();
					calculateOpModeFractions(inContext); // steps 200-299
					detailEnd = System.currentTimeMillis();
					Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.calculateOpModeFractions ms="+(detailEnd-detailStart));
				}
				
				alreadyKey = "rates|" + inContext.iterProcess.databaseKey;
				if(!alreadyDoneFlags.contains(alreadyKey)) {
					alreadyDoneFlags.add(alreadyKey);
					if(!isProjectDomain && !USE_EXTERNAL_GENERATOR_FOR_DRIVE_CYCLES) {
						/**
						 * @step 900
						 * @algorithm Update emission rate tables for Model Year Physics effects.
						 * In Project domain, only Extended Idle and APU are used in this generator.
						 * Since neither is affected by aerodynamics, only do this for Non-Project domain.
						 * @condition Non-Project domain
						**/
						detailStart = System.currentTimeMillis();
						modelYearPhysics.updateEmissionRateTables(db,inContext.iterProcess.databaseKey);
						detailEnd = System.currentTimeMillis();
						Logger.log(LogMessageCategory.DEBUG,"RatesOpModeDistributionGenerator.modelYearPhysics.updateEmissionRateTables ms="+(detailEnd-detailStart));
					}
				}
			} else {
				Logger.log(LogMessageCategory.ERROR, "Error while validating drive schedule "
						+ "distribution, rates operating mode computation cannot continue");
			}
			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,"Rates Operating Mode Distribution Generation failed.");
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"ROMDG setupTime=" + setupTime + " bundleTime=" + totalTime);
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		// Do not remove data since it is needed across multiple processes
	}

	/**
	 * Determine the drive schedules that bracket each Average Speed Bin value.
	 * <p>Each average speed bin lies between (is bracketed) by the average speeds of two drive
	 * schedules. Determine which two drive schedules bracket the average speed bin and store the
	 * identity and average speeds of the two bins.  This is done for each source type, roadway
	 * type, day of week and hour of day for each average speed bin.</p>
	**/
	void bracketAverageSpeedBins() {
		String sql = "";

		ResultSet rs = null;
		try {
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

			/**
			 * @step 100
			 * @algorithm Speed bins with values below and above the lowest and highest drive schedule values are bound
			 * to those values.
			 * scheduleBoundLo = min(averageSpeed). scheduleBoundHi = max(averageSpeed).
			 * @output DriveScheduleBounds
			 * @input DriveSchedule
			 * @input DriveScheduleAssoc
			 * @input RunSpecRoadType
			 * @input RunSpecSourceType
			**/
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
						"isOutOfBounds	  SMALLINT,"+
						"UNIQUE INDEX XPKBracketScheduleLo2 ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BracketScheduleLo2";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 100
			 * @algorithm loScheduleSpeed = max(averageSpeed). isOutOfBounds = 0.
			 * @condition Non-ramps, Drive schedules with averageSpeed <= avgBinSpeed
			 * @output BracketScheduleLo2
			 * @input RunSpecRoadType
			 * @input RunSpecSourceType
			 * @input DriveSchedule
			 * @input DriveScheduleAssoc
			 * @input AvgSpeedBin
			**/
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
						"ds.averageSpeed <= asb.avgBinSpeed "+
					"GROUP BY "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgBinSpeed";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleLo2");

			/**
			 * @step 100
			 * @algorithm loScheduleSpeed = scheduleBoundLo. isOutOfBounds = 1.
			 * @condition Non-ramps, avgBinSpeed < scheduleBoundLo.
			 * @output BracketScheduleLo2
			 * @input RunSpecRoadType
			 * @input RunSpecSourceType
			 * @input DriveSchedule
			 * @input DriveScheduleAssoc
			 * @input AvgSpeedBin
			 * @input DriveScheduleBounds
			**/
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

			/**
			 * @step 100
			 * @algorithm Find the drive cycle with an averageSpeed = loScheduleSpeed.
			 * @output BracketScheduleLo
			 * @input BracketScheduleLo2
			 * @input DriveSchedule
			 * @input DriveScheduleAssoc
			**/
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

			sql = "CREATE TABLE IF NOT EXISTS BracketScheduleHi2 ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"avgSpeedBinID     SMALLINT,"+
					"driveScheduleID   SMALLINT,"+
					"hiScheduleSpeed   FLOAT,"+
					"isOutOfBounds     SMALLINT,"+
					"UNIQUE INDEX XPKBracketScheduleHi2 ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BracketScheduleHi2";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 100
			 * @algorithm hiScheduleSpeed = min(averageSpeed). isOutOfBounds = 0.
			 * @condition Non-ramps, Drive schedules with averageSpeed > avgBinSpeed
			 * @output BracketScheduleHi2
			 * @input RunSpecRoadType
			 * @input RunSpecSourceType
			 * @input DriveSchedule
			 * @input DriveScheduleAssoc
			 * @input AvgSpeedBin
			**/
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
						"ds.averageSpeed > asb.avgBinSpeed "+
					"GROUP BY "+
						"dsa.sourceTypeID,"+
						"dsa.roadTypeID,"+
						"asb.avgBinSpeed";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE BracketScheduleHi2");

			/**
			 * @step 100
			 * @algorithm hiScheduleSpeed = scheduleBoundHi. isOutOfBounds = 1.
			 * @condition Non-ramps, avgBinSpeed > scheduleBoundHi.
			 * @output BracketScheduleHi2
			 * @input RunSpecRoadType
			 * @input RunSpecSourceType
			 * @input DriveSchedule
			 * @input DriveScheduleAssoc
			 * @input AvgSpeedBin
			 * @input DriveScheduleBounds
			**/
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

			/**
			 * @step 100
			 * @algorithm Find the drive cycle with an averageSpeed = hiScheduleSpeed.
			 * @output BracketScheduleHi
			 * @input BracketScheduleHi2
			 * @input DriveSchedule
			 * @input DriveScheduleAssoc
			**/
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
	 * Determine proportions for bracketing drive schedules.
	**/
	void determineDriveScheduleProportions() {
		String sql = "";

		try {
			sql = "CREATE TABLE IF NOT EXISTS LoScheduleFraction ("+
					"sourceTypeId       SMALLINT,"+
					"roadTypeId         SMALLINT,"+
					"avgSpeedBinID      SMALLINT,"+
					"loScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKLoScheduleFraction ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE LoScheduleFraction";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 110
			 * @algorithm loScheduleFraction=(hiScheduleSpeed - avgBinSpeed) / (hiScheduleSpeed - loScheduleSpeed).
			 * @condition hiScheduleSpeed <> loScheduleSpeed
			 * @output LoScheduleFraction
			 * @input BracketScheduleLo
			 * @input BracketScheduleHi
			 * @input AvgSpeedBin
			**/
			sql = "INSERT INTO LoScheduleFraction ("+
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

			SQLRunner.executeSQL(db,"ANALYZE TABLE LoScheduleFraction");

			/**
			 * @step 110
			 * @algorithm loScheduleFraction=1.
			 * @condition hiScheduleSpeed = loScheduleSpeed
			 * @output LoScheduleFraction
			 * @input BracketScheduleLo
			 * @input BracketScheduleHi
			**/
			sql = "INSERT INTO LoScheduleFraction ("+
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

			SQLRunner.executeSQL(db,"ANALYZE TABLE LoScheduleFraction");

			sql = "CREATE TABLE IF NOT EXISTS HiScheduleFraction ("+
					"sourceTypeId       SMALLINT,"+
					"roadTypeId         SMALLINT,"+
					"avgSpeedBinID      SMALLINT,"+
					"hiScheduleFraction FLOAT,"+
					"UNIQUE INDEX XPKHiScheduleFraction ("+
							"sourceTypeID, roadTypeID, avgSpeedBinID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE HiScheduleFraction";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 110
			 * @algorithm hiScheduleFraction = 1-loScheduleFraction.
			 * @output HiScheduleFraction
			 * @input BracketScheduleHi
			 * @input loScheduleFraction
			**/
			sql = "INSERT INTO HiScheduleFraction ("+
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
						"lsf.avgSpeedBinID = bsh.avgSpeedBinID";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE HiScheduleFraction");
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine fraction of drive schedules in each "
					+ "speed bin.", sql);
		}
	}

	/**
	 * Determine Distribution of Non Ramp Drive Schedules.
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

			/**
			 * @step 130
			 * @algorithm driveScheduleFraction=sum(loScheduleFraction).
			 * @output DriveScheduleFractionLo
			 * @input BracketScheduleLo
			 * @input LoScheduleFraction
			**/
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
					"avgSpeedBinID         SMALLINT, "+
					"driveScheduleID       SMALLINT, "+
					"driveScheduleFraction FLOAT, "+
					"UNIQUE INDEX XPKDriveScheduleFractionHi ( "+
					"sourceTypeID, roadTypeID, avgSpeedBinID, driveScheduleID))";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE DriveScheduleFractionHi";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 130
			 * @algorithm driveScheduleFraction=sum(hiScheduleFraction).
			 * @output DriveScheduleFractionHi
			 * @input BracketScheduleHi
			 * @input HiScheduleFraction
			**/
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

			/**
			 * @step 130
			 * @algorithm driveScheduleFraction = DriveScheduleFractionLo.driveScheduleFraction + DriveScheduleFractionHi.driveScheduleFraction.
			 * @output DriveScheduleFraction
			 * @input BracketScheduleHi
			 * @input DriveScheduleFractionLo
			 * @input DriveScheduleFractionHi
			 * @input RoadType
			 * @input DriveScheduleAssoc
			**/
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
					+ "non ramp drive cycle.", sql);
		}
	}

	/**
	 * Calculate the second-by-second engine specific power.
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

			/**
			 * @step 140
			 * @algorithm Get unique combinations of sourceTypeID and driveScheduleID.
			 * Only these combinations require VSP and operating mode computations.
			 * @condition driveScheduleFraction <> 0
			 * @output SourceTypeDriveSchedule
			 * @input DriveScheduleFraction
			**/
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

			/**
			 * @step 140
			 * @algorithm Find the first recorded second in each drive schedule.
			 * second=min(second).
			 * @output DriveScheduleFirstSecond
			 * @input DriveScheduleSecond
			**/
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

			/**
			 * @step 140
			 * @algorithm Calculate speed and acceleration during the first second.
			 * The acceleration of the 1st second = the acceleration of the 2nd second.
			 * The acceleration calculation assumes that the grade = 0.
			 * speed[t] = speed[t] * 0.44704.
			 * acceleration[t] = (speed[t+1]-speed[t]) * 0.44704.
			 * @output DriveScheduleSecond2
			 * @input DriveScheduleSecond at time t
			 * @input DriveScheduleSecond at time t+1
			 * @input DriveScheduleFirstSecond
			**/
			sql = "INSERT INTO DriveScheduleSecond2 ("+
						"driveScheduleID,"+
						"second,"+
						"speed,"+
						"acceleration) "+
					"SELECT "+
						"dsfs.driveScheduleID,"+
						"dsfs.second,"+
						"dss.speed * 0.44704,"+
						"(dss2.speed - dss.speed) * 0.44704 "+ // the accel of the 1st second = accel of 2nd second
					"FROM "+
						"DriveScheduleFirstSecond dsfs,"+
						"DriveScheduleSecond dss, "+
						"DriveScheduleSecond dss2 "+
					"WHERE "+
						"dsfs.driveScheduleID = dss.driveScheduleID AND "+
						"dss2.driveScheduleID = dss.driveScheduleID AND "+
						"dsfs.second = dss.second AND "+
						"dss2.second = dss.second+1";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE DriveScheduleSecond2");

			/**
			 * @step 140
			 * @algorithm Calculate speed and acceleration during the remaining seconds.
			 * speed[t] = speed[t] * 0.44704.
			 * acceleration[t] = (speed[t]-speed[t-1]) * 0.44704.
			 * @output DriveScheduleSecond2
			 * @input DriveScheduleSecond at time t
			 * @input DriveScheduleSecond at time t-1
			**/
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

			/**
			 * @step 140
			 * @algorithm Calculate VSP for each second of each drive schedule, using sourceTypeID-specific terms.
			 * VSP=(rollingTermA * speed + sut.rotatingTermB * POW(dss2.speed,2) + dragTermC * POW(speed,3) + sourceMass * speed * acceleration) / fixedMassFactor.
			 * @output VSP
			 * @input SourceTypeDriveSchedule
			 * @input DriveScheduleSecond2
			 * @input sourceUseTypePhysicsMapping
			**/
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
	 * Determine the operating mode bin for each second.
	 * <p>The VSP value for each second is compared to the upper and lower bounds for the
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

			// The following statement drops the 1st second of a drive cycle
			// because it does not join to any prior second.

			/**
			 * @step 150
			 * @algorithm Get the acceleration of every second beyond the first.
			 * acceleration[t] = speed[t] - speed[t-1].
			 * @output DriveScheduleSecond3
			 * @input DriveScheduleSecond for time t
			 * @input DriveScheduleSecond for time t-1
			**/
			sql = "INSERT INTO DriveScheduleSecond3 ("+
						"driveScheduleID,"+
						"second,"+
						"speed,"+
						"acceleration) "+
					"SELECT "+
						"dss.driveScheduleID,"+
						"dss.second,"+
						"dss.speed,"+
						"dss.speed - dss2.speed "+ // current speed - previous second's speed
					"FROM "+
						"DriveScheduleSecond dss,"+
						"DriveScheduleSecond dss2 "+
					"WHERE "+
						"dss.driveScheduleID = dss2.driveScheduleID AND "+
						"dss2.second = dss.second - 1"; // dss2 is 1 second in the past
			SQLRunner.executeSQL(db, sql);

			// Add the 1st second using the acceleration of the 2nd second.
			// Use INSERT IGNORE so only the 1st second entries will be
			// altered. All other seconds already exist and will be ignored.

			/**
			 * @step 150
			 * @algorithm Get the acceleration of the first second.
			 * Use INSERT IGNORE so only the 1st second entries will be
			 * altered. All other seconds already exist and will be ignored.
			 * acceleration[t] = speed[t+1] - speed[t].
			 * @output DriveScheduleSecond3
			 * @input DriveScheduleSecond for time t
			 * @input DriveScheduleSecond for time t+1
			**/
			sql = "INSERT IGNORE INTO DriveScheduleSecond3 ("+
						"driveScheduleID,"+
						"second,"+
						"speed,"+
						"acceleration) "+
					"SELECT "+
						"dss.driveScheduleID,"+
						"dss.second,"+
						"dss.speed,"+
						"dss2.speed - dss.speed "+ // future second's speed - current second's speed
					"FROM "+
						"DriveScheduleSecond dss,"+
						"DriveScheduleSecond dss2 "+
					"WHERE "+
						"dss.driveScheduleID = dss2.driveScheduleID AND "+
						"dss2.second = dss.second + 1"; // dss2 is 1 second in the future
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
							"sourceTypeID, driveScheduleID, second, polProcessID),"+
					"UNIQUE INDEX XPKOpModeIDBySecond2 ("+
							"sourceTypeID, driveScheduleID, polProcessID, second)"+
					//",key speed1 (opModeID,polProcessID,VSP,speed)"+ // Slower!
					")";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE OpModeIDBySecond";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE TABLE OMDGPollutantProcess ("
					+ " polProcessID int not null,"
					+ " unique index PKXOMDGPollutantProcess (polProcessID) )";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 150
			 * @algorithm Get the distinct polProcessIDs that require operating modes.
			 * @output OMDGPollutantProcess
			 * @input OpModePolProcAssoc
			**/
			sql = "INSERT INTO OMDGPollutantProcess (polProcessID)"
					+ " SELECT DISTINCT polProcessID"
					+ " FROM OpModePolProcAssoc";
			SQLRunner.executeSQL(db, sql);

			// Note: We cannot remove anything that already has an operating mode distribution
			// because not all required links may have been provided.

			/**
			 * @step 150
			 * @algorithm Remove anything from OMDGPollutantProcess that has a representing pollutant/process.  Only
			 * its representing item should be calculated.
			 * @output OMDGPollutantProcess
			 * @input OMDGPolProcessRepresented
			**/
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

			/**
			 * @step 150
			 * @algorithm Get the set of polProcessID and opModeID combinations that must
			 * be calculated.
			 * @output OpModePolProcAssocTrimmed
			 * @input OMDGPollutantProcess
			 * @input OpModePolProcAssoc
			**/
			sql = "insert into OpModePolProcAssocTrimmed (polProcessID, opModeID)"
					+ " select omp.polProcessID, omp.opModeID"
					+ " from OpModePolProcAssoc omp"
					+ " inner join OMDGPollutantProcess pp on pp.polProcessID=omp.polProcessID";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 150
			 * @algorithm Associate each second of each drive schedule with a polProcessID.
			 * Obtain the speed, acceleration, and VSP at each point. This facilitates easy
			 * opModeID assignment.
			 * @output OpModeIDBySecond
			 * @input DriveScheduleSecond3
			 * @input VSP
			 * @input OMDGPollutantProcess
			**/
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

			/**
			 * @step 150
			 * @algorithm Find braking events, copying information from time t that meet braking conditions
			 * and setting opModeID=0.
			 * @condition acceleration[t] <= -2 or (acceleration[t] < -1 and acceleration[t-1] < -1 and acceleration[t-2] < -1)
			 * @output OpModeIDBySecond_Temp
			 * @input OpModeIDBySecond for time t
			 * @input OpModeIDBySecond for time t-1
			 * @input OpModeIDBySecond for time t-2
			**/
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

			/**
			 * @step 150
			 * @algorithm Copy braking events back into OpModeIDBySecond.
			 * @output OpModeIDBySecond
			 * @input OpModeIDBySecond_Temp
			**/
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

			/**
			 * @step 150
			 * @algorithm opModeID=0, braking.
			 * @condition Any second in which the acceleration <= -2.
			 * @output OpModeIDBySecond
			**/
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
				
				/**
				 * @step 150
				 * @algorithm Assign an opModeID to each second based upon operating mode VSP and speed
				 * information.
				 * @output OpModeIDBySecond
				 * @input OperatingMode
				 * @input OpModePolProcAssocTrimmed
				 * @condition 1 < opModeID < 100, opModeID not previously assigned
				**/
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

			// Assign Idle to speed=0

			/**
			 * @step 150
			 * @algorithm Assign the Idle operating mode to speed=0.
			 * OpModeID=IF(speed=0 and polProcessID=11609,501,if(speed<1.0,1,opModeID)).
			 * @output OpModeIDBySecond
			**/
			sql = "UPDATE OpModeIDBySecond SET OpModeID=IF(speed=0 and polProcessID=11609,501,if(speed<1.0,1,opModeID))";
//					+ " where (speed=0 and polProcessID=11609) or (speed<1.0 and not (speed=0 and polProcessID=11609))";
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
	 * Calculate operating mode fractions for each drive schedule.
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

			/**
			 * @step 160
			 * @algorithm secondSum=count(seconds in each drive schedule).
			 * @output OpModeFractionBySchedule2
			 * @input OpModeIDBySecond
			**/
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

			/**
			 * @step 160
			 * @algorithm modeFraction = count(seconds in each opModeID)/secondSum.
			 * @output OpModeFractionBySchedule
			 * @input OpModeFractionBySchedule2
			 * @input OpModeIDBySecond
			**/
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
	 * Calculate overall operating mode fractions.
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
					"avgSpeedBinID     SMALLINT,"+
					"opModeID          SMALLINT,"+
					"polProcessID      INT,"+
					"opModeFraction    FLOAT,"+
					"avgBinSpeed       FLOAT DEFAULT '0',"+
					"UNIQUE INDEX XPKOpModeFraction2 ("+
							"roadTypeID, sourceTypeID, avgSpeedBinID, opModeID, polProcessID))",
				// NOTE: above, roadTypeID is the first in the index so that it will be used
				// in calculateOpModeFractions which joins based on roadTypeID only.

				"CREATE TABLE IF NOT EXISTS OpModeFraction2a ("+
					"sourceTypeID      SMALLINT,"+
					"roadTypeID        SMALLINT,"+
					"avgSpeedBinID     SMALLINT,"+
					"opModeID          SMALLINT,"+
					"polProcessID      INT,"+
					"opModeFraction    FLOAT,"+
					"INDEX IdxOpModeFraction2a ("+
							"roadTypeID, sourceTypeID, avgSpeedBinID, opModeID, polProcessID))",

				"TRUNCATE OpModeFraction2",
				"TRUNCATE OpModeFraction2a",

				/**
				 * @step 170
				 * @algorithm Add non-ramp-based information.
				 * opModeFraction = sum(modeFraction * driveScheduleFraction).
				 * @output OpModeFraction2a
				 * @input DriveScheduleFraction
				 * @input OpModeFractionBySchedule
				 * @input OpModePolProcAssocTrimmed
				 * @condition Not polProcessID 11710
				**/
				"INSERT INTO OpModeFraction2a ("+
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

				"updateOpModeFraction2a",

				"ANALYZE TABLE OpModeFraction2a",

				/**
				 * @step 170
				 * @algorithm Aggregate ramp and non-ramp data.
				 * opModeFraction = sum(opModeFraction).
				 * @output OpModeFraction2
				 * @input OpModeFraction2a
				**/
				"INSERT INTO OpModeFraction2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"opModeID,"+
						"polProcessID,"+
						"opModeFraction) "+
					"SELECT "+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"opModeID,"+
						"polProcessID,"+
						"sum(opModeFraction) as opModeFraction "+
					"FROM "+
						"OpModeFraction2a "+
					"GROUP BY "+
						"roadTypeID, sourceTypeID, avgSpeedBinID, opModeID, polProcessID",

				/**
				 * @step 170
				 * @algorithm Add running operating mode 300 to all source types.
				 * opModeFraction[opModeID=300]=1.
				 * @output OpModeFraction2
				 * @input OpModeFraction2a
				 * @input sourceUseTypePhysicsMapping
				**/
				"INSERT IGNORE INTO OpModeFraction2 ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"opModeID,"+
						"polProcessID,"+
						"opModeFraction) "+
					"SELECT "+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"300 as opModeID,"+
						"polProcessID,"+
						"1 as opModeFraction "+
					"FROM "+
						"(select distinct roadTypeID, avgSpeedBinID, polProcessID from OpModeFraction2a) T, "+
						"(select distinct realSourceTypeID as sourceTypeID from sourceUseTypePhysicsMapping union select distinct tempSourceTypeID as sourceTypeID from sourceUseTypePhysicsMapping) T2",
//					"GROUP BY "+
//						"roadTypeID, sourceTypeID, avgSpeedBinID, polProcessID",

				"update OpModeFraction2, avgSpeedBin set OpModeFraction2.avgBinSpeed=avgSpeedBin.avgBinSpeed where OpModeFraction2.avgSpeedBinID=avgSpeedBin.avgSpeedBinID",

				"ANALYZE TABLE OpModeFraction2"
			};

			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				if(sql != null && sql.length() > 0) {
					if(sql.equalsIgnoreCase("updateOpModeFraction2a")) {
						modelYearPhysics.updateOpModes(db,"OpModeFraction2a");
					} else {
						SQLRunner.executeSQL(db,sql);
					}
				}
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine final Operating Mode Distribution.",sql);
		}
	}

	/**
	 * Calculate overall operating mode fractions.
	 * @param inContext current iteration context
	**/
	void calculateOpModeFractions(MasterLoopContext inContext) {
		int processID = inContext.iterProcess.databaseKey;
		if(processID == 90) {
			calculateExtendedIdleOpModeFractions(); // steps 200-209
			return;
		} else if(processID == 91) {
			calculateAuxiliaryPowerOpModeFractions(); // steps 210-219
			return;
		} else if(processID == 1 && isProjectDomain) {
			return;
		}
		Logger.log(LogMessageCategory.DEBUG,"ROMD calculateOpModeFractions processID=" + processID);
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			if(!USE_EXTERNAL_GENERATOR_FOR_DRIVE_CYCLES) {
				/**
				 * @step 220
				 * @algorithm Find [sourceTypeID, roadTypeID, avgSpeedBinID, polProcessID] combinations
				 * that already exist within RatesOpModeDistribution.
				 * @condition Non-Project domain Running Exhaust
				 * @output OMDGOMDKeys
				 * @input RatesOpModeDistribution
				 * @input PollutantProcessAssoc
				**/
				sql = "create table if not exists OMDGOMDKeys ( "+
						"	sourceTypeID smallint(6) NOT NULL DEFAULT '0', "+
						"   avgSpeedBinID smallint(6) NOT NULL DEFAULT '0', "+
						"	roadTypeID smallint(6) NOT NULL DEFAULT '0', "+
						"	polProcessID int NOT NULL DEFAULT '0', "+
						"	primary key (avgSpeedBinID, roadTypeID, polProcessID, sourceTypeID) "+
						")";
				SQLRunner.executeSQL(db, sql);
	
				sql = "truncate table OMDGOMDKeys";
				SQLRunner.executeSQL(db, sql);
	
				sql = "insert into OMDGOMDKeys (sourceTypeID, avgSpeedBinID, roadTypeID, polProcessID) "+
						"select distinct romd.sourceTypeID, romd.avgSpeedBinID, romd.roadTypeID, romd.polProcessID "+
						"from RatesOpModeDistribution romd "+
						"inner join PollutantProcessAssoc ppa on (ppa.polProcessID=romd.polProcessID) "+
						"where ppa.processID = " + processID;
				SQLRunner.executeSQL(db, sql);
	
				/**
				 * @step 220
				 * @algorithm Add to RatesOpModeDistribution those entries that don't already have records.
				 * Add records that do not appear within OMDGOMDKeys.
				 * @condition Non-Project domain Running Exhaust
				 * @output RatesOpModeDistribution
				 * @input OpModeFraction2
				 * @input OMDGOMDKeys
				 * @input runSpecHourDay
				**/
				sql = "INSERT IGNORE INTO RatesOpModeDistribution ("+
							"sourceTypeID,"+
							"roadTypeID,"+
							"avgSpeedBinID,"+
							"avgBinSpeed,"+
							"polProcessID,"+
							"hourDayID,"+
							"opModeID,"+
							"opModeFraction) "+
						"SELECT "+
							"omf2.sourceTypeID,"+
							"omf2.roadTypeID,"+
							"omf2.avgSpeedBinID,"+
							"omf2.avgBinSpeed,"+
							"omf2.polProcessID,"+
							"rshd.hourDayID,"+
							"omf2.opModeID,"+
							"omf2.opModeFraction "+
						"FROM "+
							"OpModeFraction2 omf2 "+
							"left outer join OMDGOMDKeys k on ( "+
								"k.roadTypeID=omf2.roadTypeID "+
								"and k.avgSpeedBinID=omf2.avgSpeedBinID "+
								"and k.polProcessID=omf2.polProcessID "+
								"and k.sourceTypeID=omf2.sourceTypeID "+
							") "+
							", runSpecHourDay rshd "+
						"WHERE "+
							"k.roadTypeID is null "+
							"and k.polProcessID is null "+
							"and k.sourceTypeID is null "+
							"and k.avgSpeedBinID is null";
				SQLRunner.executeSQL(db, sql);
	
				// Copy representing entries to those being represented, but only if those
				// being represented are not already present.
	
				/**
				 * @step 220
				 * @algorithm Clear all entries from OMDGOMDKeys.
				 * @condition Non-Project domain Running Exhaust
				 * @output OMDGOMDKeys
				**/
				sql = "truncate table OMDGOMDKeys";
				SQLRunner.executeSQL(db, sql);
	
				/**
				 * @step 220
				 * @algorithm Find [sourceTypeID, roadTypeID, avgSpeedBinID, polProcessID] combinations
				 * that already exist within RatesOpModeDistribution.
				 * @condition Non-Project domain Running Exhaust
				 * @output OMDGOMDKeys
				 * @input RatesOpModeDistribution
				 * @input PollutantProcessAssoc
				**/
				sql = "insert into OMDGOMDKeys (sourceTypeID, avgSpeedBinID, roadTypeID, polProcessID) "+
						"select distinct romd.sourceTypeID, romd.avgSpeedBinID, romd.roadTypeID, romd.polProcessID "+
						"from RatesOpModeDistribution romd "+
						"inner join PollutantProcessAssoc ppa on (ppa.polProcessID=romd.polProcessID) "+
						"where ppa.processID = " + processID;
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
	
					/**
					 * @step 220
					 * @algorithm Copy representing entries to those being represented, but only if those
					 * being represented are not already present. Add to RatesOpModeDistribution those 
					 * entries that don't already have records. Add records that do not appear within OMDGOMDKeys.
					 * @condition Non-Project domain Running Exhaust
					 * @output RatesOpModeDistribution
					 * @input OpModeFraction2
					 * @input OMDGOMDKeys
					 * @input runSpecHourDay
					**/
					sql = "INSERT IGNORE INTO RatesOpModeDistribution ("+
								"sourceTypeID,"+
								"roadTypeID,"+
								"avgSpeedBinID,"+
								"avgBinSpeed,"+
								"polProcessID,"+
								"hourDayID,"+
								"opModeID,"+
								"opModeFraction) "+
							"SELECT "+
								"omf2.sourceTypeID,"+
								"omf2.roadTypeID,"+
								"omf2.avgSpeedBinID,"+
								"omf2.avgBinSpeed,"+
								ppa + " as polProcessID,"+
								"rshd.hourDayID,"+
								"omf2.opModeID,"+
								"omf2.opModeFraction "+
							"FROM "+
								"OpModeFraction2 omf2 "+
								"left outer join OMDGOMDKeys k on ( "+
									"k.avgSpeedBinID=omf2.avgSpeedBinID "+
									"and k.polProcessID=" + ppa + " "+
									"and k.sourceTypeID=omf2.sourceTypeID "+
									"and k.roadTypeID=omf2.roadTypeID "+
								") "+
								", runSpecHourDay rshd "+
							"WHERE "+
								"k.avgSpeedBinID is null "+
								"and k.polProcessID is null "+
								"and k.sourceTypeID is null "+
								"and k.roadTypeID is null "+
								"and omf2.polProcessID =" + repPPA;
					SQLRunner.executeSQL(db, sql);
				}
			}

			if(processID == 1) {
				if(USE_EXTERNAL_GENERATOR) {
					if(!shouldDeferForBaseRateGenerator) {
						if(runLocalExternalGenerator(inContext,"SourceTypePhysics.updateOperatingModeDistribution.RatesOpModeDistribution",null,null,null)) {
							Logger.log(LogMessageCategory.DEBUG,"Success running the external generator in RatesOpModeDistribution");
						} else {
							Logger.log(LogMessageCategory.ERROR,"Unable to run external generator in RatesOpModeDistribution");
						}
					}
				} else {
					/*
					Logger.log(LogMessageCategory.DEBUG,"Making backup copy of RatesOpModeDistribution into RatesOpModeDistributionSQL...");
					SQLRunner.executeSQL(db, "drop table if exists RatesOpModeDistributionSQLBackup");
					SQLRunner.executeSQL(db, "create table RatesOpModeDistributionSQLBackup like RatesOpModeDistribution");
					SQLRunner.executeSQL(db, "insert into RatesOpModeDistributionSQLBackup select * from RatesOpModeDistribution");
					*/
					Logger.log(LogMessageCategory.DEBUG,"Using SQL-based SourceTypePhysics.updateOperatingModeDistribution for RatesOpModeDistribution...");
					modelYearPhysics.updateOperatingModeDistribution(db,"RatesOpModeDistribution");
					Logger.log(LogMessageCategory.DEBUG,"Done using SQL-based SourceTypePhysics.updateOperatingModeDistribution for RatesOpModeDistribution.");
				}
			}
			polProcessIDs = "";

			if(!USE_EXTERNAL_GENERATOR_FOR_DRIVE_CYCLES) {
				Logger.log(LogMessageCategory.DEBUG,"Analyzing RatesOpModeDistribution...");
				SQLRunner.executeSQL(db,"ANALYZE TABLE RatesOpModeDistribution");
				Logger.log(LogMessageCategory.DEBUG,"Done analyzing RatesOpModeDistribution.");

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
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine final Rates Operating Mode Distribution.",sql);
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Calculate operating mode fractions for Extended Idle Exhaust (90).
	**/
	void calculateExtendedIdleOpModeFractions() {
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			/**
			 * @step 200
			 * @algorithm opModeFraction=1.
			 * @condition Extended Idle
			 * @condition sourceTypeID=62 only
			 * @output RatesOpModeDistribution
			 * @input pollutantProcessAssoc
			 * @input sourceTypePolProcess
			 * @input opModePolProcessAssoc
			 * @input runSpecHourDay
			**/
			sql = "INSERT IGNORE INTO RatesOpModeDistribution ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"avgBinSpeed,"+
						"polProcessID,"+
						"hourDayID,"+
						"opModeID,"+
						"opModeFraction) "+
					"select "+
						"stpp.sourceTypeID,"+
						"1 as roadTypeID,"+
						"0 as avgSpeedBinID,"+
						"0 as avgBinSpeed,"+
						"omppa.polProcessID,"+
						"rshd.hourDayID,"+
						"omppa.opModeID,"+
						"1 as opModeFraction "+
					"from "+
						"pollutantProcessAssoc ppa "+
						"inner join sourceTypePolProcess stpp on (stpp.polProcessID = ppa.polProcessID) "+
						"inner join opModePolProcAssoc omppa on (omppa.polProcessID = stpp.polProcessID) "+
						", runSpecHourDay rshd "+
					"where "+
						"ppa.processID = 90 and stpp.sourceTypeID=62";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 200
			 * @algorithm Add data for all extended idling (opModeID 200).
			 * opModeFraction[opModeID=200]=1.
			 * @condition Extended Idle
			 * @condition sourceTypeID=62 only
			 * @condition opModeFraction[opModeID=200] not already specified
			 * @output RatesOpModeDistribution
			 * @input pollutantProcessAssoc
			 * @input sourceTypePolProcess
			 * @input opModePolProcessAssoc
			 * @input runSpecHourDay
			**/
			sql = "INSERT IGNORE INTO RatesOpModeDistribution ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"avgBinSpeed,"+
						"polProcessID,"+
						"hourDayID,"+
						"opModeID,"+
						"opModeFraction) "+
					"select "+
						"sourceTypeID,"+
						"1 as roadTypeID,"+
						"0 as avgSpeedBinID,"+
						"0 as avgBinSpeed,"+
						"polProcessID,"+
						"rshd.hourDayID,"+
						"200 as opModeID,"+
						"1 as opModeFraction "+
					"from "+
						"pollutantProcessAssoc,"+
						"runSpecSourceType "+
						", runSpecHourDay rshd "+
					"where "+
						"processID = 90 and sourceTypeID=62";
			SQLRunner.executeSQL(db, sql);

			//modelYearPhysics.updateOperatingModeDistribution(db,"RatesOpModeDistribution");
			SQLRunner.executeSQL(db,"ANALYZE TABLE RatesOpModeDistribution");
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine final Rates Operating Mode Distribution.",sql);
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Calculate operating mode fractions for Auxiliary Power Exhaust (91).
	**/
	void calculateAuxiliaryPowerOpModeFractions() {
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			/**
			 * @step 210
			 * @algorithm opModeFraction=1.
			 * @condition Auxiliary Power Exhaust
			 * @condition sourceTypeID=62 only
			 * @condition Not opModeID 200 (Extended Idle)
			 * @output RatesOpModeDistribution
			 * @input pollutantProcessAssoc
			 * @input sourceTypePolProcess
			 * @input opModePolProcessAssoc
			 * @input runSpecHourDay
			**/
			sql = "INSERT IGNORE INTO RatesOpModeDistribution ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"avgBinSpeed,"+
						"polProcessID,"+
						"hourDayID,"+
						"opModeID,"+
						"opModeFraction) "+
					"select "+
						"stpp.sourceTypeID,"+
						"1 as roadTypeID,"+
						"0 as avgSpeedBinID,"+
						"0 as avgBinSpeed,"+
						"omppa.polProcessID,"+
						"rshd.hourDayID,"+
						"omppa.opModeID,"+
						"1 as opModeFraction "+
					"from "+
						"pollutantProcessAssoc ppa "+
						"inner join sourceTypePolProcess stpp on (stpp.polProcessID = ppa.polProcessID) "+
						"inner join opModePolProcAssoc omppa on (omppa.polProcessID = stpp.polProcessID) "+
						", runSpecHourDay rshd "+
					"where "+
						"ppa.processID = 91 and omppa.opModeID<>200 and stpp.sourceTypeID=62";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 210
			 * @algorithm Add data for all hotelling except extended idling (opModeID 200).
			 * opModeFraction=1.
			 * @condition Auxiliary Power Exhaust
			 * @condition sourceTypeID=62 only
			 * @condition Not opModeID 200 (Extended Idle)
			 * @output RatesOpModeDistribution
			 * @input pollutantProcessAssoc
			 * @input runSpecSourceType
			 * @input runSpecHourDay
			 * @input hotellingActivityDistribution
			**/
			sql = "INSERT IGNORE INTO RatesOpModeDistribution ("+
						"sourceTypeID,"+
						"roadTypeID,"+
						"avgSpeedBinID,"+
						"avgBinSpeed,"+
						"polProcessID,"+
						"hourDayID,"+
						"opModeID,"+
						"opModeFraction) "+
					"select distinct "+
						"sourceTypeID,"+
						"1 as roadTypeID,"+
						"0 as avgSpeedBinID,"+
						"0 as avgBinSpeed,"+
						"polProcessID,"+
						"rshd.hourDayID,"+
						"opModeID,"+
						"1 as opModeFraction "+
					"from "+
						"pollutantProcessAssoc,"+
						"runSpecSourceType,"+
						"runSpecHourDay rshd,"+
						"hotellingActivityDistribution "+
					"where "+
						"processID = 91 and opModeID<>200 and sourceTypeID=62";
			SQLRunner.executeSQL(db, sql);

			SQLRunner.executeSQL(db,"ANALYZE TABLE RatesOpModeDistribution");
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine final Rates Operating Mode Distribution.",sql);
		} finally {
			query.onFinally();
		}
	}
}
