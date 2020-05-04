/**************************************************************************************************
 * @(#)AverageSpeedOperatingModeDistributionGenerator.java
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
 * This builds "Operating Mode Distribution" records for Running Exhaust and Tirewear
 * based upon average speed distributions.
 *
 * @author		Wesley Faler
 * @version		2014-03-04
**/
public class AverageSpeedOperatingModeDistributionGenerator extends Generator {
	/**
	 * @algorithm
	 * @owner Average Speed Operating Mode Distribution Generator
	 * @generator
	**/

	/** Database connection used by all functions.  Setup by executeLoop and cleanDataLoop. **/
	Connection db;
	/** Running Exhaust process **/
	EmissionProcess runningProcess = null;
	/** Tirewear process **/
	EmissionProcess tireProcess = null;
	/** true if executeLoop and cleanDataLoop are being execute for the Tirewear process **/
	boolean isTirewear = false;
	/** comma-separated list of polProcessIDs used by the most recent run of executeLoop() **/
	String polProcessIDs = "";
	/** case-clause for assigning operating modes **/
	String opModeAssignmentSQL = null;

	/** Default constructor **/
	public AverageSpeedOperatingModeDistributionGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// Subscribe at the LINK level because testing revealed it to be substantially faster to
		// perform the queries for a single link than once for an entire zone.
		tireProcess = EmissionProcess.findByName("Tirewear");
		if(CompilationFlags.DO_RATES_FIRST) {
			if(ExecutionRunSpec.getRunSpec().domain == ModelDomain.PROJECT) {
				targetLoop.subscribe(this, tireProcess, MasterLoopGranularity.LINK,
						MasterLoopPriority.GENERATOR);
			} else {
				targetLoop.subscribe(this, tireProcess, MasterLoopGranularity.PROCESS,
						MasterLoopPriority.GENERATOR);
			}
		} else {
			targetLoop.subscribe(this, tireProcess, MasterLoopGranularity.LINK,
					MasterLoopPriority.GENERATOR);
		}
	}

	/**
	 * Called each time the link changes.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			if(inContext.iterProcess.databaseKey == tireProcess.databaseKey) {
				isTirewear = true;
				polProcessIDs = "11710";
			} else {
				isTirewear = false;
				polProcessIDs = "";
				Logger.log(LogMessageCategory.ERROR,"AvgSpeedOMDG called for unknown process");
				return;
			}
			if(CompilationFlags.DO_RATES_FIRST) {
				if(ExecutionRunSpec.getRunSpec().domain == ModelDomain.PROJECT) {
					if(isTirewear) {
						calculateTireProjectOpModeFractions(inContext.iterLocation.linkRecordID);
					}
				} else {
					calculateRatesFirstOpModeFractions();
				}
			} else {
				if(ExecutionRunSpec.getRunSpec().domain == ModelDomain.PROJECT) {
					if(isTirewear) {
						calculateTireProjectOpModeFractions(inContext.iterLocation.linkRecordID);
					}
				} else {
					calculateOpModeFractions(inContext.iterLocation.linkRecordID);
				}
			}
		} catch (Exception e) {
			Logger.logError(e,"Operating Mode Distribution Generation failed.");
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
		if(CompilationFlags.DO_RATES_FIRST) {
			// Nothing to do in Rates First mode.
			return;
		}
		String sql = "";
		try {
			if(polProcessIDs.length() > 0) {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
				sql = "DELETE FROM OpModeDistribution WHERE isUserInput='N' AND linkID = "
						+ context.iterLocation.linkRecordID
						+ " AND polProcessID IN (" + polProcessIDs + ")";
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
	}

	/**
	 * Calculate operating mode fractions based upon the link average speed.
	 * @param linkID The link being processed.
	**/
	void calculateTireProjectOpModeFractions(int linkID) {
		if(opModeAssignmentSQL == null) {
			opModeAssignmentSQL = buildOpModeClause();
		}
		String[] statements = null;
		if(CompilationFlags.DO_RATES_FIRST) {
			String[] t = {
				/**
				 * @step 010
				 * @algorithm Remove all tirewear pollutant/processes on any link from the operating mode distribution.
				 * @output RatesOpModeDistribution
				 * @condition Project domain
				 * @condition Tirewear process
				**/
				"delete from RatesOpModeDistribution where polProcessID=11710",

				/**
				 * @step 020
				 * @algorithm Assign operating mode based upon a single link's linkAvgSpeed, one operating mode entry per link with opModeFraction=1.0.
				 * @input link
				 * @input operatingMode
				 * @output RatesOpModeDistribution
				 * @condition Project domain
				 * @condition Tirewear process
				**/
				"insert ignore into RatesOpModeDistribution (sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID,"
				+ " polProcessID, opModeID, opModeFraction, avgBinSpeed)"
				+ " select sourceTypeID, roadTypeID, 0 as avgSpeedBinID, hourDayID, "
				+ " 	11710 as polProcessID,"
				+ " 	(case " + opModeAssignmentSQL + " else -1 end) as opModeID,"
				+ " 	1 as opModeFraction,"
				+ " 	linkAvgSpeed avgBinSpeed"
				+ " from link"
				+ " inner join RunSpecSourceType"
				+ " inner join RunSpecHourDay"
				+ " where linkID=" + linkID
			};
			statements = t;
		} else {
			String[] t = {
				"insert ignore into opModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID,"
				+ " 	opModeID, opModeFraction, opModeFractionCV, isUserInput)"
				+ " select sourceTypeID, hourDayID, linkID, 11710 as polProcessID,"
				+ " 	(case " + opModeAssignmentSQL + " else -1 end) as opModeID,"
				+ " 	1 as opModeFraction,"
				+ " 	0 as opModeFractionCV,"
				+ " 	'N' as isUserInput"
				+ " from link"
				+ " inner join RunSpecSourceType"
				+ " inner join RunSpecHourDay"
			};
			statements = t;
		}
		String sql = "";
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				if(sql != null && sql.length() > 0) {
					SQLRunner.executeSQL(db,sql);
				}
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not create tirewear opmode distribution.",sql);
		}
	}

	/**
	 * Calculate operating mode fractions based upon the average speed distribution.
	 * @param linkID The link being processed.
	**/
	void calculateOpModeFractions(int linkID) {
		String opModeIDColumn = "";
		if(isTirewear) {
			opModeIDColumn = "opModeIDTirewear";
		//} else if(isRunningExhaust) {
		//	opModeIDColumn = "opModeIDRunning";
		}

		String[] statements = {
			"insert ignore into OpModeDistribution (sourceTypeID, linkID, hourDayID,"
				+ " polProcessID, opModeID, opModeFraction)"
				+ " select sourceTypeID, linkID, hourDayID, polProcessID, "
				+ opModeIDColumn + ", sum(avgSpeedFraction)"
				+ " from avgSpeedBin bin"
				+ " inner join avgSpeedDistribution dist on dist.avgSpeedBinID=bin.avgSpeedBinID"
				+ " inner join link on link.roadTypeID=dist.roadTypeID,"
				+ " pollutantProcessAssoc"
				+ " where polProcessID in (" + polProcessIDs + ")"
				+ " and linkID=" + linkID
				+ " group by sourceTypeID, linkID, hourDayID, polProcessID, " + opModeIDColumn
				+ " having sum(avgSpeedFraction) > 0"
				+ " order by sourceTypeID, linkID, hourDayID, polProcessID, " + opModeIDColumn,

			"ANALYZE TABLE OpModeDistribution"
		};
		String sql = "";
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine final Operating Mode Distribution.",sql);
		}
	}

	/**
	 * Calculate operating mode fractions for each speed bin.
	**/
	void calculateRatesFirstOpModeFractions() {
		String opModeIDColumn = "";
		if(isTirewear) {
			opModeIDColumn = "opModeIDTirewear";
		//} else if(isRunningExhaust) {
		//	opModeIDColumn = "opModeIDRunning";
		}

		String[] statements = null;
		if(CompilationFlags.USE_2010B_TIREWEAR_RATE_METHOD) {
			// This method is known to make incorrect rates. It does match
			// the rates produced by 2010B. However, those rates cannot be
			// combined with activity to match the 2010B tirewear inventory.
			String[] t = {
				"drop table if exists tAvgSpeedROMD",
	
				"create table tAvgSpeedROMD"
					+ " select sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, "
					+ " polProcessID, avgBinSpeed"
					+ " from avgSpeedBin bin,"
					+ " runSpecSourceType sut,"
					+ " runSpecRoadType rt,"
					+ " runSpecHourDay hd,"
					+ " pollutantProcessAssoc ppa"
					+ " where polProcessID in (" + polProcessIDs + ")",
	
				"alter table tAvgSpeedROMD add key (hourDayID, roadTypeID, sourceTypeID)",
	
				"insert ignore into RatesOpModeDistribution (sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID,"
					+ " polProcessID, opModeID, opModeFraction, avgBinSpeed)"
					+ " select r.sourceTypeID, r.roadTypeID, r.avgSpeedBinID, r.hourDayID, "
					+ " 	r.polProcessID, " + opModeIDColumn + ", dist.avgSpeedFraction as opModeFraction, r.avgBinSpeed"
					+ " from avgSpeedBin bin"
					+ " inner join avgSpeedDistribution dist on (dist.avgSpeedBinID=bin.avgSpeedBinID)"
					+ " inner join tAvgSpeedROMD r on ("
					+ " 	r.sourceTypeID = dist.sourceTypeID"
					+ " 	and r.roadTypeID = dist.roadTypeID"
					+ " 	and r.hourDayID = dist.hourDayID)",
	
				"ANALYZE TABLE RatesOpModeDistribution"
			};
			statements = t;
		} else {
			String[] t = {
				/**
				 * @step 100
				 * @algorithm Create a single operating mode entry, with opModeFraction=1.0, for every combination of speed bin, source type, 
				 * road type, hour day, and tirewear pollutant in the runspec. Each speed bin has an associated tirewear operating mode in 
				 * its opModeIDTirewear column.
				 * @input avgSpeedBin
				 * @output RatesOpModeDistribution
				 * @condition Non-Project domain
				 * @condition Tirewear process
				**/
				"insert ignore into RatesOpModeDistribution (sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID,"
					+ " polProcessID, opModeID, opModeFraction, avgBinSpeed)"
					+ " select sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, "
					+ " polProcessID, " + opModeIDColumn + ", 1 as opModeFraction, avgBinSpeed"
					+ " from avgSpeedBin bin,"
					+ " runSpecSourceType sut,"
					+ " runSpecRoadType rt,"
					+ " runSpecHourDay hd,"
					+ " pollutantProcessAssoc ppa"
					+ " where polProcessID in (" + polProcessIDs + ")",
	
				"ANALYZE TABLE RatesOpModeDistribution"
			};
			statements = t;
		}
		String sql = "";
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				if(sql != null && sql.length() > 0) {
					SQLRunner.executeSQL(db,sql);
				}
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not create RatesOpModeDistribution for tire wear.",sql);
		}
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
		String clause = "when linkAvgSpeed<0.1 then 400\n";
		String sql = "select speedLower, speedUpper, opModeID "
				+ " from operatingMode"
				+ " where opModeID >= 401 and opModeID <= 499"
				+ " and opModeName like 'tirewear%'"
				+ " order by speedLower";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				boolean hasCondition = false;
				String line = "when (";

				float speedLower = query.rs.getFloat("speedLower");
				if(!query.rs.wasNull()) {
					if(hasCondition) {
						line += " and ";
					}
					hasCondition = true;
					line += "" + speedLower + " <= linkAvgSpeed";
				}

				float speedUpper = query.rs.getFloat("speedUpper");
				if(!query.rs.wasNull()) {
					if(hasCondition) {
						line += " and ";
					}
					hasCondition = true;
					line += "linkAvgSpeed < " + speedUpper;
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
}
