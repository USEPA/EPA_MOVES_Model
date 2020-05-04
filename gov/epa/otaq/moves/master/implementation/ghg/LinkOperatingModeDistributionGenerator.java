/**************************************************************************************************
 * @(#)LinkOperatingModeDistributionGenerator.java
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
 * This builds "Operating Mode Distribution" records for project domains.
 *
 * @author		Wesley Faler
 * @author		W. Aikman
 * @version		2014-05-28
**/
public class LinkOperatingModeDistributionGenerator extends Generator {
	/**
	 * @algorithm
	 * @owner Link Operating Mode Distribution Generator
	 * @generator
	**/

	/** Flags for tasks already done, used to prevent duplicate execution **/
	TreeSet<String> alreadyDoneFlags = new TreeSet<String>();
	/** String objects representing tables and time/location keys that have been generated **/
	TreeSet<String> itemsGenerated = new TreeSet<String>();
	/** Database connection used by all functions.  Setup by executeLoop and cleanDataLoop. **/
	Connection db;
	/** milliseconds spent during one time operations **/
	long setupTime = 0;
	/** milliseconds spent during non-one-time operations **/
	long totalTime = 0;
	/** comma-separated list of polProcessIDs used by this generator **/
	String polProcessIDs = "";
	/** case-clause for assigning operating modes **/
	String opModeAssignmentSQL = null;
	/** true if one-time setup activities have already occurred **/
	boolean didSetup = false;
	/**
	 * Messages generated when driving cycles are not available from average speeds.
	 * Used here to prevent duplicate messages.
	**/
	TreeSet<String> outOfBoundMessagesGenerated = new TreeSet<String>();
	/** Model-year specific rolling and drag terms **/
	SourceTypePhysics modelYearPhysics = new SourceTypePhysics();
	/** road type of the previous link **/
	int previousRoadTypeID = 0;

	/** Default constructor **/
	public LinkOperatingModeDistributionGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		EmissionProcess runningProcess = EmissionProcess.findByName("Running Exhaust");
		// Signup at the Link level but only for the purpose of populating OperatingModeDistribution
		// in small steps which are faster for subsequent joins by calculators.  Grade data is
		// used for the links as given by the driving cycle.

		// GENERATOR+1 priority is used because this generator fills Link.linkAvgSpeed based
		// on drive schedules and Link.linkAvgSpeed is needed by ProjectTAG to do SHO calculations.

		targetLoop.subscribe(this, runningProcess, MasterLoopGranularity.LINK,
				MasterLoopPriority.GENERATOR+1);

		EmissionProcess brakeProcess = EmissionProcess.findByName("Brakewear");
		targetLoop.subscribe(this, brakeProcess, MasterLoopGranularity.LINK,
				MasterLoopPriority.GENERATOR+1);
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

			if(!didSetup) {
				start = System.currentTimeMillis();
				didSetup = true;
				/**
				 * @step 010
				 * @algorithm Setup for Model Year Physics effects.
				**/
				modelYearPhysics.setup(db);
				setup();
				setupTime = System.currentTimeMillis() - start;
			}

			start = System.currentTimeMillis();
			if(inContext.iterLocation.roadTypeRecordID != 1) {
				String alreadyKey = "calc|" + inContext.iterProcess.databaseKey + "|" + inContext.iterLocation.linkRecordID;
				if(!alreadyDoneFlags.contains(alreadyKey)) {
					alreadyDoneFlags.add(alreadyKey);
					calculateOpModeFractions(inContext.iterLocation.linkRecordID); // steps 100-199
					if(CompilationFlags.DO_RATES_FIRST) {
						populateRatesOpModeDistribution(inContext.iterLocation.linkRecordID, inContext.iterLocation.roadTypeRecordID); // steps 200-299
					}
				}

				alreadyKey = "rates|" + inContext.iterProcess.databaseKey;
				if(!alreadyDoneFlags.contains(alreadyKey)) {
					alreadyDoneFlags.add(alreadyKey);
					/**
					 * @step 900
					 * @algorithm Update emission rate tables for Model Year Physics effects.
					**/
					modelYearPhysics.updateEmissionRateTables(db,inContext.iterProcess.databaseKey);
				}
			}
			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,"Operating Mode Distribution Generation failed.");
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"LOMDG setupTime=" + setupTime + " bundleTime=" + totalTime);
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

	/** Perform one-time setup operations **/
	void setup() {
		String sql = "";
		String[] statements = {
			// Get the table of sourceTypeIDs and polProcessIDs only once before we write
			// anything to opModeDistrubtion
			"drop table if exists tempExistingOpMode",

			"create table tempExistingOpMode"
					+ " select distinct sourceTypeID, polProcessID, linkID"
					+ " from opModeDistribution",

			"drop table if exists tempLinkBracket",

			/**
			 * @step 010
			 * @algorithm Set rampFraction=0 for all road types as there is no automatic
			 * ramp contribution in Project mode.
			 * @condition Project domain
			**/
			"update roadtype set rampFraction=0"
		};
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
			/*
			// Remind the user that there are no automatic ramp contributions in Project mode.
			TreeSet<RoadType> roadTypes = ExecutionRunSpec.theExecutionRunSpec.getRoadTypes();
			for(RoadType r : roadTypes) {
				if(r.roadTypeID == 2 || r.roadTypeID == 4) {
					Logger.log(LogMessageCategory.WARNING,"In Project mode, there are no automatic ramp contributions to road types 2 and 4.");
					break;
				}
			}
			*/
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not do link operating mode setup", sql);
		}
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
	 * Build case-centric clause for assigning operating modes.  The clause will be used within
	 * a SQL CASE statement.
	 * Example lines:
	 * 	when (VSP < 1 and speed >= 5) then 314
	 *  when (VSP <= 2 and speed <= 17) then 999
	 * @return case-centric clause for assigning operating modes
	**/
	String buildOpModeClause() {
		String clause = "";
		String sql = "select VSPLower, VSPUpper, speedLower, speedUpper, opModeID "
				+ " from operatingMode"
				+ " where opModeID >= 1 and opModeID <= 99"
				+ " and opModeID not in (26,36)" // These operating modes are redundant with others
				+ " order by opModeID";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				boolean hasCondition = false;
				String line = "when (";

				float vspLower = query.rs.getFloat("VSPLower");
				if(!query.rs.wasNull()) {
					if(hasCondition) {
						line += " and ";
					}
					hasCondition = true;
					line += "" + vspLower + " <= VSP";
				}

				float vspUpper = query.rs.getFloat("VSPUpper");
				if(!query.rs.wasNull()) {
					if(hasCondition) {
						line += " and ";
					}
					hasCondition = true;
					line += "VSP < " + vspUpper;
				}

				float speedLower = query.rs.getFloat("speedLower");
				if(!query.rs.wasNull()) {
					if(hasCondition) {
						line += " and ";
					}
					hasCondition = true;
					line += "" + speedLower + " <= speed";
				}

				float speedUpper = query.rs.getFloat("speedUpper");
				if(!query.rs.wasNull()) {
					if(hasCondition) {
						line += " and ";
					}
					hasCondition = true;
					line += "speed < " + speedUpper;
				}

				line += ") then " + query.rs.getInt("opModeID");
				clause += line + "\n";
			}
		} catch(SQLException e) {
			query.onException(e,"Unable to build operating mode clause",sql);
		} finally {
			query.onFinally();
		}
		return clause;
	}

	/**
	 * OMDG-7: Calculate overall operating mode fractions.
	 * <p>The overall operating mode fractions are calculated by weighting the operating mode
	 * fractions of each drive schedule by the drive schedule fractions. This is done for each
	 * source type, road type, day of the week, hour of the day and operating mode.</p>
	 * @param linkID The link being processed.
	**/
	void calculateOpModeFractions(int linkID) {
		if(hasGenerated("driveScheduleSecondLink",linkID)) {
			return;
		}
		String sql = "";
		try {
			boolean hasDriveSchedule = false;
			double averageSpeed = 0.0;

			sql = "select count(*) from driveScheduleSecondLink where linkID=" + linkID;
			if(SQLRunner.executeScalar(db,sql) > 0) {
				hasDriveSchedule = true;
			} else {
				/**
				 * @step 100
				 * @algorithm Lookup linkAvgSpeed for the current link.
				 * @input link
				**/
				sql = "select linkAvgSpeed from link where linkID=" + linkID;
				averageSpeed = SQLRunner.executeScalar(db,sql);
				if(averageSpeed <= 0) {
					/**
					 * @step 100
					 * @algorithm For links with zero average speed, provide a default
					 * drive schedule of 30 seconds of idling. Use 0 grade because with 0 speed, 
					 * brakes are likely applied rather than using the engine to counteract 
					 * any grade.  This removes the grade's effect on VSP.
					 * @output driveScheduleSecondLink
					 * @condition linkAvgSpeed <= 0
					**/

					// Provide a default drive schedule of all idling
					sql = "insert into driveScheduleSecondLink"
							+ " (linkID, secondID, speed, grade) values ";
					for(int i=1;i<=30;i++) { // 30 seconds
						if(i > 1) {
							sql += ",";
						}
						sql += "(" + linkID + "," + i + ", 0, 0)";
						// Use 0 grade because with 0 speed, brakes are likely applied rather
						// than using the engine to counteract any grade.  This removes the
						// grade's effect on VSP.
					}
					SQLRunner.executeSQL(db,sql);
					hasDriveSchedule = true;
				}
			}

			if(hasDriveSchedule) {
				sql = "";
				/**
				 * @step 109
				 * @algorithm Perform the steps to calculate a link's operating mode distribution from its drive schedule.
				 * @condition The current link has a drive schedule not just an average speed.
				**/
				calculateOpModeFractionsCore(linkID,null); // steps 110-149
			} else if(averageSpeed > 0) {
				sql = "";
				interpolateOpModeFractions(linkID,averageSpeed); // steps 101-105
			} else {
				Logger.log(LogMessageCategory.ERROR,"Link " + linkID + " has no drive schedule or average speed.");
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not calculate operating mode distributions", sql);
		}
	}

	/**
	 * Populate the RatesOpModeDistribution table with data from the OpModeDistribution
	 * table for a single link. RatesOpModeDistribution has data only by road type, not
	 * per link, and needs to be updated for each project link.
	 * @param linkID link to be used.
	 * @param roadTypeID road type of the link to be used.
	**/
	void populateRatesOpModeDistribution(int linkID, int roadTypeID) {
		String sql = "";
		try {
			// Cleanup previous data
			if(previousRoadTypeID > 0) {
				/**
				 * @step 200
				 * @algorithm Delete the previous road type's data from ratesOpModeDistribution.
				 * @output ratesOpModeDistribution
				 * @condition A previous road type has been used in the run.
				**/
				sql = "delete from ratesOpModeDistribution where roadTypeID=" + previousRoadTypeID;
				SQLRunner.executeSQL(db,sql);
			}

			// Remove current data that might overlap

			/**
			 * @step 200
			 * @algorithm Delete the current road type's data from ratesOpModeDistribution.
			 * @output ratesOpModeDistribution
			**/
			sql = "delete from ratesOpModeDistribution where roadTypeID=" + roadTypeID;
			SQLRunner.executeSQL(db,sql);

			// Copy opmode data to RatesOpModeDistribution

			/**
			 * @step 200
			 * @algorithm Lookup the current link's linkAvgSpeed.
			 * @input linkAvgSpeed
			**/
			sql = "select linkAvgSpeed from link where linkID=" + linkID;
			double averageSpeed = SQLRunner.executeScalar(db,sql);

			/**
			 * @step 200
			 * @algorithm Copy the current link's opModeDistribution entries to ratesOpModeDistribution,
			 * providing the current link's road type and average speed.
			 * Do not copy any generic polProcessID entries (polProcessID <= 0) as these are just to speedup
			 * internal opModeDistribution calculations.
			 * @input opModeDistribution
			 * @output ratesOpModeDistribution
			**/
			sql = "insert into ratesOpModeDistribution (sourceTypeID, hourDayID, polProcessID, opModeID, opModeFraction,"
					+ " 	roadTypeID, avgSpeedBinID, avgBinSpeed)"
					+ " select sourceTypeID, hourDayID, polProcessID, opModeID, opModeFraction,"
					+ " 	" + roadTypeID + " as roadTypeID, "
					+ " 	0 as avgSpeedBinID, "
					+ " 	" + averageSpeed + " as avgBinSpeed"
					+ " from opModeDistribution omd"
					+ " where polProcessID > 0" // don't copy the generic polprocess entries, these are just to speed up OMD population itself
					+ " and linkID = " + linkID;
			SQLRunner.executeSQL(db,sql);
			previousRoadTypeID = roadTypeID;
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not populate rates operating mode distributions", sql);
		}
	}

	/**
	 * @param linkID The link being processed.
	 * @param averageSpeed the average speed on the link
	**/
	void interpolateOpModeFractions(int linkID, double averageSpeed) throws SQLException {
		String sql = "";
		int roadTypeID = 0;
		double averageGrade = 0;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			/**
			 * @step 105
			 * @algorithm Lookup the road type and average grade for the current link.
			 * @input link
			 * @condition The current link has no drive schedule
			**/
			sql = "select roadTypeID from link where linkID=" + linkID;
			roadTypeID = (int)SQLRunner.executeScalar(db,sql);

			sql = "select ifnull(linkAvgGrade,0.0) from link where linkID=" + linkID;
			averageGrade = SQLRunner.executeScalar(db,sql);

			// Find brackets for each source type given the road type and average speed
			String[] statements = {
				"create table if not exists tempLinkBracket ("
						+ " 	linkID int not null,"
						+ " 	sourceTypeID smallint(6) not null,"
						+ " 	roadTypeID smallint(6) not null,"
						+ " 	driveScheduleIDLow int null,"
						+ " 	driveScheduleIDHigh int null,"
						+ "     isOutOfBoundsLow int not null default 0,"
						+ "     isOutOfBoundsHigh int not null default 0,"
						+ " 	unique key (linkID, sourceTypeID, roadTypeID),"
						+ " 	key (linkID, roadTypeID, sourceTypeID)"
						+ " )",

				/**
				 * @step 105
				 * @algorithm Find the drive schedule with the greatest average speed that is still <= the link's average speed.
				 * Use this as the lower bracketing schedule.
				 * Such a drive schedule may not exist.
				 * @output tempLinkBracket
				 * @input driveScheduleAssoc
				 * @input driveSchedule
				 * @condition The current link has no drive schedule
				**/
				"insert into tempLinkBracket (linkID, sourceTypeID, roadTypeID, driveScheduleIDLow)"
						+ " select " + linkID + " as linkID,"
						+ " 	dsal2.sourceTypeID, dsal2.roadTypeID, max(dsal2.driveScheduleID) as driveScheduleIDLow"
						+ " from driveScheduleAssoc dsal2"
						+ " inner join driveSchedule dsl2 using (driveScheduleID)"
						+ " where dsl2.averageSpeed=("
						+ " 	select max(averageSpeed)"
						+ " 	from driveScheduleAssoc dsal"
						+ " 	inner join driveSchedule dsl using (driveScheduleID)"
						+ " 	where isRamp='N'"
						+ " 	and dsl.averageSpeed <= " + averageSpeed
						+ " 	and dsal.roadTypeID=" + roadTypeID
						+ " 	and dsal.sourceTypeID=dsal2.sourceTypeID"
						+ " 	and dsal.roadTypeID=dsal2.roadTypeID"
						+ " )"
						+ " group by dsal2.sourceTypeID, dsal2.roadTypeID"
						+ " order by null",

				// Do an insert ignore set to isOutOfBoundsLow=1

				/**
				 * @step 105
				 * @algorithm Find the drive schedule with the lowest average speed that is still > the link's average speed.
				 * Use this as the lower bracketing schedule if one was not previously found. Flag the bracket as out of bounds
				 * on the low side.
				 * Such a drive schedule may not exist.
				 * @output tempLinkBracket
				 * @input driveScheduleAssoc
				 * @input driveSchedule
				 * @condition The current link has no drive schedule
				**/
				"insert ignore into tempLinkBracket (linkID, sourceTypeID, roadTypeID, driveScheduleIDLow, isOutOfBoundsLow)"
						+ " select " + linkID + " as linkID,"
						+ " 	dsal2.sourceTypeID, dsal2.roadTypeID, max(dsal2.driveScheduleID) as driveScheduleIDLow,"
						+ "     1 as isOutOfBoundsLow"
						+ " from driveScheduleAssoc dsal2"
						+ " inner join driveSchedule dsl2 using (driveScheduleID)"
						+ " where dsl2.averageSpeed=("
						+ " 	select min(averageSpeed)"
						+ " 	from driveScheduleAssoc dsal"
						+ " 	inner join driveSchedule dsl using (driveScheduleID)"
						+ " 	where isRamp='N'"
						+ " 	and dsl.averageSpeed > " + averageSpeed
						+ " 	and dsal.roadTypeID=" + roadTypeID
						+ " 	and dsal.sourceTypeID=dsal2.sourceTypeID"
						+ " 	and dsal.roadTypeID=dsal2.roadTypeID"
						+ " )"
						+ " group by dsal2.sourceTypeID, dsal2.roadTypeID"
						+ " order by null",

				"drop table if exists tempLinkBracketHigh",

				"create table if not exists tempLinkBracketHigh ("
						+ " 	linkID int not null,"
						+ " 	sourceTypeID smallint(6) not null,"
						+ " 	roadTypeID smallint(6) not null,"
						+ " 	driveScheduleIDHigh int null,"
						+ "     isOutOfBoundsHigh int not null default 0,"
						+ " 	unique key (linkID, sourceTypeID, roadTypeID),"
						+ " 	key (linkID, roadTypeID, sourceTypeID)"
						+ " )",

				/**
				 * @step 105
				 * @algorithm Find the drive schedule with the lowest average speed that is still >= the link's average speed.
				 * Use this as the upper bracketing schedule.
				 * Such a drive schedule may not exist.
				 * @output tempLinkBracketHigh
				 * @input driveScheduleAssoc
				 * @input driveSchedule
				 * @condition The current link has no drive schedule
				**/
				"insert into tempLinkBracketHigh (linkID, sourceTypeID, roadTypeID, driveScheduleIDHigh)"
						+ " select " + linkID + " as linkID,"
						+ " 	dsal2.sourceTypeID, dsal2.roadTypeID, max(dsal2.driveScheduleID) as driveScheduleIDHigh"
						+ " from driveScheduleAssoc dsal2"
						+ " inner join driveSchedule dsl2 using (driveScheduleID)"
						+ " where dsl2.averageSpeed=("
						+ " 	select min(averageSpeed)"
						+ " 	from driveScheduleAssoc dsal"
						+ " 	inner join driveSchedule dsl using (driveScheduleID)"
						+ " 	where isRamp='N'"
						+ " 	and dsl.averageSpeed >= " + averageSpeed
						+ " 	and dsal.roadTypeID=" + roadTypeID
						+ " 	and dsal.sourceTypeID=dsal2.sourceTypeID"
						+ " 	and dsal.roadTypeID=dsal2.roadTypeID"
						+ " )"
						+ " group by dsal2.sourceTypeID, dsal2.roadTypeID"
						+ " order by null",

				// Do an insert ignore set to isOutOfBoundsHigh=1

				/**
				 * @step 105
				 * @algorithm Find the drive schedule with the highest average speed that is still < the link's average speed.
				 * Use this as the upper bracketing schedule if one was not previously found. Flag the bracket as out of bounds
				 * on the high side.
				 * Such a drive schedule may not exist.
				 * @output tempLinkBracketHigh
				 * @input driveScheduleAssoc
				 * @input driveSchedule
				 * @condition The current link has no drive schedule
				**/
				"insert ignore into tempLinkBracketHigh (linkID, sourceTypeID, roadTypeID, driveScheduleIDHigh, isOutOfBoundsHigh)"
						+ " select " + linkID + " as linkID,"
						+ " 	dsal2.sourceTypeID, dsal2.roadTypeID, max(dsal2.driveScheduleID) as driveScheduleIDHigh,"
						+ "     1 as isOutOfBoundsHigh"
						+ " from driveScheduleAssoc dsal2"
						+ " inner join driveSchedule dsl2 using (driveScheduleID)"
						+ " where dsl2.averageSpeed=("
						+ " 	select max(averageSpeed)"
						+ " 	from driveScheduleAssoc dsal"
						+ " 	inner join driveSchedule dsl using (driveScheduleID)"
						+ " 	where isRamp='N'"
						+ " 	and dsl.averageSpeed < " + averageSpeed
						+ " 	and dsal.roadTypeID=" + roadTypeID
						+ " 	and dsal.sourceTypeID=dsal2.sourceTypeID"
						+ " 	and dsal.roadTypeID=dsal2.roadTypeID"
						+ " )"
						+ " group by dsal2.sourceTypeID, dsal2.roadTypeID"
						+ " order by null",

				/**
				 * @step 105
				 * @algorithm Note the upper bracket link and its out of bounds flag, if any.
				 * @output tempLinkBracket
				 * @input tempLinkBracketHigh
				 * @condition The current link has no drive schedule
				**/
				"update tempLinkBracket, tempLinkBracketHigh set tempLinkBracket.driveScheduleIDHigh=tempLinkBracketHigh.driveScheduleIDHigh,"
						+ " tempLinkBracket.isOutOfBoundsHigh=tempLinkBracketHigh.isOutOfBoundsHigh"
						+ " where tempLinkBracket.sourceTypeID=tempLinkBracketHigh.sourceTypeID"
						+ " and tempLinkBracket.roadTypeID=tempLinkBracketHigh.roadTypeID"
						+ " and tempLinkBracket.linkID=tempLinkBracketHigh.linkID"
						+ " and tempLinkBracket.linkID=" + linkID,

				"drop table if exists tempLinkBracketHigh"
			};
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
			// Build operating mode distributions for each drive schedule, using its -driveScheduleID as the pseudo-linkID
			int priorDriveScheduleID = 0;
			String sourceTypes = "";
			/**
			 * @step 105
			 * @algorithm Get the set of distinct driveScheduleID and sourceTypeID combinations
			 * that require operating mode calculations.
			 * @input tempLinkBracket
			 * @output list of bracketing drive schedules that need operating mode distributions
			 * @condition The current link has no drive schedule
			**/
			sql = "select distinct driveScheduleIDLow as driveScheduleID, sourceTypeID"
					+ " from tempLinkBracket"
					+ " where driveScheduleIDLow is not null and driveScheduleIDHigh is not null"
					+ " and linkID=" + linkID
					+ " union"
					+ " select distinct driveScheduleIDHigh as driveScheduleID, sourceTypeID"
					+ " from tempLinkBracket"
					+ " where driveScheduleIDLow is not null and driveScheduleIDHigh is not null"
					+ " and linkID=" + linkID
					+ " order by driveScheduleID, sourceTypeID";
			query.open(db,sql);
			while(query.rs.next()) {
				int driveScheduleID = query.rs.getInt(1);
				int sourceTypeID = query.rs.getInt(2);
				if(driveScheduleID != priorDriveScheduleID) {
					if(priorDriveScheduleID != 0) {
						// Copy drive schedule into driveScheduleSecondLink using averageGrade
						sql = "delete from driveScheduleSecondLink where linkID=" + -priorDriveScheduleID;
						SQLRunner.executeSQL(db,sql);

						/**
						 * @step 105
						 * @algorithm Copy each second of each bracketing drive schedule into driveScheduleSecondLink.
						 * Use the link's average grade as each entry's grade.
						 * Use -driveScheduleID as the linkID field in driveScheduleSecondLink, denoting it as a
						 * bracketing link.
						 * @input driveScheduleSecond
						 * @output driveScheduleSecondLink
						 * @condition The current link has no drive schedule
						**/
						sql = "insert into driveScheduleSecondLink (linkID, secondID, speed, grade)"
								+ " select " + -priorDriveScheduleID + ", second, speed, " + averageGrade
								+ " from driveScheduleSecond"
								+ " where driveScheduleID=" + priorDriveScheduleID;
						SQLRunner.executeSQL(db,sql);
						/**
						 * @step 105
						 * @algorithm Perform the steps to calculate a link's operating mode distribution from its drive schedule.
						 * Use -driveScheduleID as the linkID to calculate.
						 * @condition The current link has a drive schedule not just an average speed.
						**/
						calculateOpModeFractionsCore(-priorDriveScheduleID,sourceTypes);
						sourceTypes = "";
					}
					priorDriveScheduleID = driveScheduleID;
				}
				if(sourceTypes.length() > 0) {
					sourceTypes += "," + sourceTypeID;
				} else {
					sourceTypes = "" + sourceTypeID;
				}
			}
			query.close();
			if(priorDriveScheduleID != 0 && sourceTypes.length() > 0) {
				// Copy drive schedule into driveScheduleSecondLink using averageGrade
				sql = "delete from driveScheduleSecondLink where linkID=" + -priorDriveScheduleID;
				SQLRunner.executeSQL(db,sql);
				sql = "insert into driveScheduleSecondLink (linkID, secondID, speed, grade)"
						+ " select " + -priorDriveScheduleID + ", second, speed, " + averageGrade
						+ " from driveScheduleSecond"
						+ " where driveScheduleID=" + priorDriveScheduleID;
				SQLRunner.executeSQL(db,sql);
				// Build the operating mode distribution
				calculateOpModeFractionsCore(-priorDriveScheduleID,sourceTypes);
				sourceTypes = "";
			}
			// Build the interpolated opModeDistribution for each source type given its bounding drive schedules
			ArrayList<Double> tempValues = new ArrayList<Double>();
			sql = "select sourceTypeID, driveScheduleIDLow, dsl.averageSpeed, driveScheduleIDHigh, dsh.averageSpeed,"
					+ " 	isOutOfBoundsLow, isOutOfBoundsHigh"
					+ " from tempLinkBracket"
					+ " inner join driveSchedule dsl on (dsl.driveScheduleID=driveScheduleIDLow)"
					+ " inner join driveSchedule dsh on (dsh.driveScheduleID=driveScheduleIDHigh)"
					+ " where linkID=" + linkID
					+ " order by sourceTypeID";
			query.open(db,sql);
			while(query.rs.next()) {
				int sourceTypeID = query.rs.getInt(1);
				tempValues.add(new Double(sourceTypeID));
				tempValues.add(new Double(query.rs.getInt(2)));
				tempValues.add(new Double(query.rs.getFloat(3)));
				tempValues.add(new Double(query.rs.getInt(4)));
				tempValues.add(new Double(query.rs.getFloat(5)));

				boolean isTooLow = query.rs.getInt(6) > 0;
				boolean isTooHigh = query.rs.getInt(7) > 0;
				if(isTooLow || isTooHigh) {
					String message = "Driving cycles for average speed " + averageSpeed
							+ " for sourcetype " + sourceTypeID
							+ " on roadtype " + roadTypeID
							+ " were not available.  MOVES results for this speed were extrapolated from the closest available driving cycles.";
					if(!outOfBoundMessagesGenerated.contains(message)) {
						outOfBoundMessagesGenerated.add(message);
						if(CompilationFlags.ALLOW_DRIVE_CYCLE_EXTRAPOLATION) {
							Logger.log(LogMessageCategory.WARNING,message);
						} else {
							Logger.log(LogMessageCategory.ERROR,message);
							MOVESEngine.terminalErrorFound();
						}
					}
				}
			}
			query.close();

			/**
			 * @step 105
			 * @algorithm Create expanded operating mode tables for Model Year physics effects.
			 * @condition The current link has a drive schedule not just an average speed.
			**/
			modelYearPhysics.createExpandedOperatingModesTable(db);

			for(int ti=0;ti<tempValues.size();ti+=5) {
				int sourceTypeID = (int)(tempValues.get(ti+0).doubleValue());
				int lowScheduleID = (int)(tempValues.get(ti+1).doubleValue());
				double lowAverageSpeed = tempValues.get(ti+2).doubleValue();
				int highScheduleID = (int)(tempValues.get(ti+3).doubleValue());
				double highAverageSpeed = tempValues.get(ti+4).doubleValue();

				/**
				 * @step 105
				 * @algorithm interpolationFactor = (link average speed - low bracket schedule average speed) / (high bracket schedule average speed - low bracket schedule average speed).
				 * @condition The current link has a drive schedule not just an average speed.
				**/
				double factor = 0.0;
				if(lowAverageSpeed < highAverageSpeed) {
					factor = (averageSpeed - lowAverageSpeed) / (highAverageSpeed - lowAverageSpeed);
				}

				String[] interpolateStatements = {
					"drop table if exists tempOpModeDistribution",
					"create table tempOpModeDistribution like opModeDistribution",
					"drop table if exists tempOpModeDistribution2",
					"create table tempOpModeDistribution2 like opModeDistribution",

					"insert into tempOpModeDistribution2 (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction, isUserInput)"
							+ " select sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction, isUserInput"
							+ " from opModeDistribution"
							+ " where sourceTypeID=" + sourceTypeID
							+ " and linkID in (" + -lowScheduleID + "," + -highScheduleID + ")",

					"insert ignore into tempOpModeDistribution2 (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction, isUserInput)"
							+ " select distinct sourceTypeID, hourDayID, linkID, polProcessID, om.opModeID, 0.0 as opModeFraction, 'N' as isUserInput"
							+ " from physicsOperatingMode om, tempOpModeDistribution2",

					"insert into tempOpModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction, isUserInput)"
							+ " select omdLow.sourceTypeID, omdLow.hourDayID, " + linkID + " as linkID,"
							+ " 	omdLow.polProcessID, omdLow.opModeID,"
							+ " 	(omdLow.opModeFraction+(omdHigh.opModeFraction-omdLow.opModeFraction)*" + factor + ") as opModeFraction,"
							+ " 	'N' as isUserInput"
							+ " from tempOpModeDistribution2 omdLow"
							+ " inner join tempOpModeDistribution2 omdHigh on ("
							+ " 	omdHigh.linkID= " + -highScheduleID
							+ " 	and omdHigh.sourceTypeID = omdLow.sourceTypeID"
							+ " 	and omdHigh.hourDayID = omdLow.hourDayID"
							+ " 	and omdHigh.polProcessID = omdLow.polProcessID"
							+ " 	and omdHigh.opModeID = omdLow.opModeID"
							+ " )"
							+ " where omdLow.sourceTypeID=" + sourceTypeID
							+ " and omdLow.linkID=" + -lowScheduleID,

					"delete from tempOpModeDistribution where opModeFraction <= 0",

					/**
					 * @step 105
					 * @algorithm Interpolate the operating mode distribution from the low and high bracket's operating mode distribution.
					 * opModeFraction = opModeFraction[low]+(opModeFraction[high]-opModeFraction[low])*interpolationFactor.
					 * @output opModeDistribution
					 * @condition The current link has a drive schedule not just an average speed.
					**/
					"insert ignore into opModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction, isUserInput)"
							+ " select tomd.sourceTypeID, tomd.hourDayID, tomd.linkID, tomd.polProcessID, tomd.opModeID, tomd.opModeFraction, tomd.isUserInput"
							+ " from tempOpModeDistribution tomd"
							+ " left join tempExistingOpMode eom on ("
							+ " 	eom.sourceTypeID=tomd.sourceTypeID"
							+ " 	and eom.linkID=tomd.linkID"
							+ " 	and eom.polProcessID=tomd.polProcessID)"
							+ " where eom.sourceTypeID is null"
							+ " and eom.polProcessID is null"
							+ " and eom.linkID is null"

					,"drop table if exists tempOpModeDistribution"
					,"drop table if exists tempOpModeDistribution2"
				};
				for(int i=0;i<interpolateStatements.length;i++) {
					sql = interpolateStatements[i];
					//System.out.println(sql);
					SQLRunner.executeSQL(db,sql);
				}
				/*
				sql = " select count(*)"
						+ " from tempOpModeDistribution tomd"
						+ " left join tempExistingOpMode eom on ("
						+ " 	eom.sourceTypeID=tomd.sourceTypeID"
						+ " 	and eom.linkID=tomd.linkID"
						+ " 	and eom.polProcessID=tomd.polProcessID)"
						+ " where eom.sourceTypeID is null"
						+ " and eom.polProcessID is null"
						+ " and eom.linkID is null";
				int countAvailable = (int)SQLRunner.executeScalar(db,sql);
				System.out.println("***** countAvailable=" + countAvailable + " for linkID=" + linkID);
				*/
			}
			//System.out.println("***** Built opModeDistribution for linkID=" + linkID);
			// Remove temporary data from driveScheduleSecondLink

			/**
			 * @step 105
			 * @algorithm Remove bracketing schedules from driveScheduleSecondLink.
			 * @input tempLinkBracket
			 * @output driveScheduleSecondLink
			 * @condition The current link has a drive schedule not just an average speed.
			**/
			sql = "select distinct driveScheduleIDLow as driveScheduleID"
					+ " from tempLinkBracket"
					+ " where driveScheduleIDLow is not null and driveScheduleIDHigh is not null"
					+ " and linkID=" + linkID
					+ " union"
					+ " select distinct driveScheduleIDHigh as driveScheduleID"
					+ " from tempLinkBracket"
					+ " where driveScheduleIDLow is not null and driveScheduleIDHigh is not null"
					+ " and linkID=" + linkID
					+ " order by driveScheduleID";
			query.open(db,sql);
			while(query.rs.next()) {
				int driveScheduleID = query.rs.getInt(1);
				sql = "delete from driveScheduleSecondLink where linkID=" + -driveScheduleID;
				SQLRunner.executeSQL(db,sql);
			}
			query.close();
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not interpolate operating mode distributions", sql);
			throw e;
		} finally {
			query.onFinally();
			String[] statements = {
				"drop table if exists tempLinkBracketHigh"
				//,"drop table if exists tempLinkBracket" Leave tempLinkBracket in MOVESExecution for debugging
			};
			for(int i=0;i<statements.length;i++) {
				try {
					sql = statements[i];
					SQLRunner.executeSQL(db,sql);
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
	}

	/**
	 * Core routine for calculating operating mode distribution for a link based on its
	 * drive schedule.  This routine may be called multiple times while establishing the
	 * distributions bracketing a link.
	 * <p>The overall operating mode fractions are calculated by weighting the operating mode
	 * fractions of each drive schedule by the drive schedule fractions. This is done for each
	 * source type, road type, day of the week, hour of the day and operating mode.</p>
	 * @param linkID The link being processed.  Bracketing links are given negative number IDs.
	 * @param sourceTypes comma-separated list of source types to be used.  If null or empty,
	 * all source types in the RunSpec will be used.
	**/
	void calculateOpModeFractionsCore(int linkID, String sourceTypes) throws SQLException {
		String sql = "";
		try {
			// Update the link's average speed and grade to be consistent with the drive schedule for
			// real links.  Bracketing links are given negative number IDs.
			if(linkID > 0) {
				/**
				 * @step 110
				 * @algorithm linkAvgSpeed=average(speed).
				 * linkAvgGrade=average(grade).
				 * @input driveScheduleSecondLink
				 * @output link
				 * @condition Calculate a link's operating mode distribution from its drive schedule.
				 * @condition A real linkID is provided (linkID > 0)
				**/
				sql = "select avg(speed) from driveScheduleSecondLink where linkID=" + linkID;
				double averageSpeed = SQLRunner.executeScalar(db,sql);
				sql = "update link set linkAvgSpeed=" + averageSpeed
						+ " where linkID=" + linkID;
						//+ " and (linkAvgSpeed is null or linkAvgSpeed <= 0)";
				SQLRunner.executeSQL(db,sql);

				sql = "select avg(grade) from driveScheduleSecondLink where linkID=" + linkID;
				double averageGrade = SQLRunner.executeScalar(db,sql);
				sql = "update link set linkAvgGrade=" + averageGrade
						+ " where linkID=" + linkID;
						//+ " and linkAvgGrade is null";
				SQLRunner.executeSQL(db,sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not check driveScheduleSecondLink", sql);
			return;
		}
		if(opModeAssignmentSQL == null) {
			opModeAssignmentSQL = buildOpModeClause();
		}
		String sourceTypeIDClause = "";
		if(sourceTypes != null && sourceTypes.trim().length() > 0) {
			sourceTypeIDClause = " and rst.sourceTypeID in (" + sourceTypes + ")";
		}
		String[] statements = {
			"drop table if exists tempDriveScheduleSecondLink",

			// Accelerations are in units of miles/(hour*sec)
			// Speeds is miles/hour
			// VSP is kW/tonne
			// 0.44704 (meter*hour)/(miles*sec)

			/**
			 * @step 110
			 * @algorithm Calculate accelerations in units of miles/(hour*second).
			 * Speeds are given in miles/hour.
			 * VSP is kW/tonne.
			 * There are 0.44704 (meter*hours)/(miles*second).
			 * at0 = coalesce(
			 * (speed[t]-speed[t-1])+(9.81/0.44704*sin(atan(grade[t]/100.0))),
			 * (speed[t+1]-speed[t])+(9.81/0.44704*sin(atan(grade[t]/100.0))),
			 * 0.0).
			 * at1 = coalesce((speed[t-1]-speed[t-2])+(9.81/0.44704*sin(atan(grade[t-1]/100.0))),0.0).
			 * at2 = coalesce((speed[t-2]-speed[t-3])+(9.81/0.44704*sin(atan(grade[t-2]/100.0))),0.0).
			 * VSP = (((speed[t]*0.44704)*(rollingTermA+(speed[t]*0.44704)*(rotatingTermB+dragTermC*(speed[t]*0.44704)))
			 * +sourceMass*(speed[t]*0.44704)*coalesce(speed[t]-speed[t-1],speed[t+1]-speed[t],0.0)*0.44704
			 * +sourceMass*9.81*sin(atan(grade[t]/100.0))*(speed[t]*0.44704)))/fixedMassFactor.
			 * @input driveScheduleSecondLink at time t
			 * @input driveScheduleSecondLink at time t-1 seconds
			 * @input driveScheduleSecondLink at time t-2 seconds
			 * @input driveScheduleSecondLink at time t-3 seconds
			 * @input driveScheduleSecondLink at time t+1 seconds
			 * @output tempDriveScheduleSecondLink
			 * @condition Calculate a link's operating mode distribution from its drive schedule.
			**/
			"create table tempDriveScheduleSecondLink"
					+ " select "
					+ " sut.tempSourceTypeID as sourceTypeID, "
					+ "     a.linkID, a.secondID, a.speed,"
					+ " 	coalesce("
					+ "			(a.speed-b.speed)+(9.81/0.44704*sin(atan(a.grade/100.0))),"
					+ "			(z.speed-a.speed)+(9.81/0.44704*sin(atan(a.grade/100.0))),"
					+ "			0.0) as At0,"
					+ "		coalesce((b.speed-c.speed)+(9.81/0.44704*sin(atan(b.grade/100.0))),0.0) as At1,"
					+ "		coalesce((c.speed-d.speed)+(9.81/0.44704*sin(atan(c.grade/100.0))),0.0) as At2,"
				 	+ " 	(((a.speed*0.44704)*(rollingTermA+(a.speed*0.44704)*(rotatingTermB+dragTermC*(a.speed*0.44704)))"
				 	+ "		+sourceMass*(a.speed*0.44704)*coalesce(a.speed-b.speed,z.speed-a.speed,0.0)*0.44704"
			 		+ " 	+sourceMass*9.81*sin(atan(a.grade/100.0))*(a.speed*0.44704)))/fixedMassFactor as VSP,"
					+ " 	-1 as opModeID"
					+ " from driveScheduleSecondLink a"
					+ " left join driveScheduleSecondLink b on (b.linkID=a.linkID and b.secondID=a.secondID-1)"
					+ " left join driveScheduleSecondLink c on (c.linkID=b.linkID and c.secondID=b.secondID-1)"
					+ " left join driveScheduleSecondLink d on (d.linkID=c.linkID and d.secondID=c.secondID-1)"
					+ " left join driveScheduleSecondLink z on (z.linkID=a.linkID and z.secondID=a.secondID+1)"
					+ ","
					+ " sourceUseTypePhysicsMapping sut"
					+ " inner join RunSpecSourceType rst on (rst.sourceTypeID = sut.realSourceTypeID)"
					+ " where a.linkID=" + linkID
					+ sourceTypeIDClause,

			/**
			 * @step 110
			 * @algorithm Assign operating modes.
			 * Assign a stopped mode 501 when speed = 0, assign mode 501. This mode will be converted to 1 based on polProcessID later.
			 * Otherwise, assign the idle mode 1 when speed < 1.
			 * Otherwise, assign the braking mode 0 when At0 <= -2 or (At0 < -1 and At1 < -1 and At2 < -1).
			 * Otherwise, assign operating modes by their data-drive speed and VSP ranges.
			 * @output tempDriveScheduleSecondLink
			 * @condition Calculate a link's operating mode distribution from its drive schedule.
			**/
			"update tempDriveScheduleSecondLink set opModeID= case"
					+ " when (speed = 0) then 501" // force special bin for stopped, will be converted to bin 1 based on polProcessID later
					+ " when (speed < 1) then 1" // force idle if speed < 1, just like OMDG and MesoscaleOMDG
					+ "	when (At0 <= -2 or (At0 < -1 and At1 < -1 and At2 < -1)) then 0" // braking
					+ " " + opModeAssignmentSQL
					+ "	else -1"
					+ " end",

			"drop table if exists tempDriveScheduleSecondLinkTotal",

			/**
			 * @step 110
			 * @algorithm secondTotal = Count of the number of entries in the drive schedule. This is the number
			 * of seconds of driving, even if there are gaps within the data.
			 * @output tempDriveScheduleSecondLinkTotal
			 * @input tempDriveScheduleSecondLink
			 * @condition Calculate a link's operating mode distribution from its drive schedule.
			**/
			"create table tempDriveScheduleSecondLinkTotal"
					+ " select sourceTypeID, linkID, count(*) as secondTotal"
					+ " from tempDriveScheduleSecondLink"
					+ " group by sourceTypeID, linkID"
					+ " order by null",

			"drop table if exists tempDriveScheduleSecondLinkCount",

			/**
			 * @step 110
			 * @algorithm secondCount = Count of the number of entries in the drive schedule for each operating mode.
			 * @output tempDriveScheduleSecondLinkCount
			 * @input tempDriveScheduleSecondLink
			 * @condition Calculate a link's operating mode distribution from its drive schedule.
			**/
			"create table tempDriveScheduleSecondLinkCount"
					+ " select sourceTypeID, linkID, opModeID, count(*) as secondCount"
					+ " from tempDriveScheduleSecondLink"
					+ " group by sourceTypeID, linkID, opModeID"
					+ " order by null",

			"drop table if exists tempDriveScheduleSecondLinkFraction",

			/**
			 * @step 110
			 * @algorithm opModeFraction = secondCount/secondTotal.
			 * @output tempDriveScheduleSecondLinkFraction
			 * @input tempDriveScheduleSecondLinkCount
			 * @input tempDriveScheduleSecondLinkTotal
			 * @condition Calculate a link's operating mode distribution from its drive schedule.
			**/
			"create table tempDriveScheduleSecondLinkFraction"
					+ " select sourceTypeID, linkID, opModeID, (secondCount*1.0/secondTotal) as opModeFraction"
					+ " from tempDriveScheduleSecondLinkCount sc"
					+ " inner join tempDriveScheduleSecondLinkTotal st using (sourceTypeID, linkID)",

			"drop table if exists opModeDistributionTemp",

			"CREATE TABLE opModeDistributionTemp ("
					+ "   sourceTypeID smallint(6) NOT NULL DEFAULT '0',"
					+ "   hourDayID smallint(6) NOT NULL DEFAULT '0',"
					+ "   linkID int(11) NOT NULL DEFAULT '0',"
					+ "   polProcessID int NOT NULL DEFAULT '0',"
					+ "   opModeID smallint(6) NOT NULL DEFAULT '0',"
					+ "   opModeFraction float DEFAULT NULL,"
					+ "   opModeFractionCV float DEFAULT NULL,"
					+ "   isUserInput char(1) NOT NULL DEFAULT 'N',"
					+ "   KEY allColumns (hourDayID,linkID,opModeID,polProcessID,sourceTypeID),"
					+ "   KEY sourceTypeID (sourceTypeID),"
					+ "   KEY hourDayID (hourDayID),"
					+ "   KEY linkID (linkID),"
					+ "   KEY polProcessID (polProcessID),"
					+ "   KEY opModeID (opModeID)"
					+ " )",

			/**
			 * @step 110
			 * @algorithm Copy the operating mode distribution to all polProcessIDs that share it.
			 * For polProcessID 11609, retain opMode 501 and convert opMode 501 to opMode 1 for all
			 * other polProcessIDs.
			 * @output opModeDistributionTemp
			 * @input tempDriveScheduleSecondLinkFraction
			 * @input opModePolProcAssoc
			 * @condition Calculate a link's operating mode distribution from its drive schedule.
			**/
			"insert into opModeDistributionTemp (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction)"
					+ " select lf.sourceTypeID, hourDayID, lf.linkID, omppa.polProcessID, "
					+ " if(lf.opModeID=501,if(omppa.polProcessID=11609,501,1),lf.opModeID) as opModeID,"
					+ " opModeFraction"
					+ " from tempDriveScheduleSecondLinkFraction lf"
					+ " inner join opModePolProcAssoc omppa using (opModeID)"
					+ " left join tempExistingOpMode eom on ("
					+ "		eom.sourceTypeID=lf.sourceTypeID"
					+ "		and eom.linkID=lf.linkID"
					+ "		and eom.polProcessID=omppa.polProcessID),"
					+ " RunSpecHourDay rshd"
					+ " where eom.sourceTypeID is null"
					+ " and eom.polProcessID is null"
					+ " and eom.linkID is null",

			/**
			 * @step 110
			 * @algorithm opModeFraction=sum(temporary opModeFraction).
			 * other polProcessIDs.
			 * @output opModeDistribution
			 * @output opModeDistributionTemp
			 * @condition Calculate a link's operating mode distribution from its drive schedule.
			**/
			"insert ignore into opModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction)"
					+ " select sourceTypeID, hourDayID, linkID, polProcessID, opModeID, sum(opModeFraction)"
					+ " from opModeDistributionTemp"
					+ " group by hourDayID, linkID, opModeID, polProcessID, sourceTypeID"
			/*
			"insert ignore into opModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction)"
					+ " select lf.sourceTypeID, hourDayID, lf.linkID, omppa.polProcessID, lf.opModeID, opModeFraction"
					+ " from tempDriveScheduleSecondLinkFraction lf"
					+ " inner join opModePolProcAssoc omppa using (opModeID)"
					+ " left join tempExistingOpMode eom on ("
					+ "		eom.sourceTypeID=lf.sourceTypeID"
					+ "		and eom.linkID=lf.linkID"
					+ "		and eom.polProcessID=omppa.polProcessID),"
					+ " RunSpecHourDay rshd"
					+ " where eom.sourceTypeID is null"
					+ " and eom.polProcessID is null"
					+ " and eom.linkID is null"
			*/
		};
		try {
			if(linkID < 0) {
				/**
				 * @step 110
				 * @algorithm Remove data for bracketing links first, just in case they were left from a prior link's settings.
				 * @output opModeDistribution
				 * @condition Calculate a link's operating mode distribution from its drive schedule.
				 * @condition A bracketing link is provided (linkID < 0)
				**/
				sql = "delete from opModeDistribution where linkID=" + linkID;
				SQLRunner.executeSQL(db,sql);
			}
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				if(sql != null && sql.length() > 0) {
					SQLRunner.executeSQL(db,sql);
				}
			}
			//System.out.println("**** Built opModes from drive schedule for link " + linkID);
			/**
			 * @step 110
			 * @algorithm Update OpModeDistribution for Source Type Physics effects.
			 * @output opModeDistribution
			 * @condition Calculate a link's operating mode distribution from its drive schedule.
			**/
			modelYearPhysics.updateOperatingModeDistribution(db,"OpModeDistribution", "linkID=" + linkID);
		} catch(SQLException e) {
			Logger.logSqlError(e,"Could not calculate operating mode distributions", sql);
			throw e;
		}
	}
}
