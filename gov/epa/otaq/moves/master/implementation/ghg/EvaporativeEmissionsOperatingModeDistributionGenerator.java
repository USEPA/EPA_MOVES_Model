/**************************************************************************************************
 * @(#)EvaporativeEmissionsOperatingModeDistributionGenerator.java
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

/**
 * This builds Evaporative Emissions "Operating Mode Distribution" records for ELDB data.
 * ELDB is the Execution Location Database explained in TotalActivityGenerator
 *
 * @author		Wesley Faler
 * @version		2014-02-11
**/
public class EvaporativeEmissionsOperatingModeDistributionGenerator extends Generator {
	/**
	 * @algorithm
	 * @owner Evaporative Emissions Operating Mode Distribution Generator
	 * @generator
	**/

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
	/** true if the model scale is Mesoscale Lookup, wherein 100% of activity is Operating **/
	boolean isMesoscaleLookup = false;
	/** Year|Month for a link keyed by linkID **/
	private TreeMap<String,String> contextForLink = new TreeMap<String,String>();

	/** Default constructor **/
	public EvaporativeEmissionsOperatingModeDistributionGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// Signup at the month level because temperature is dependent on month but Operating
		// Mode Distribution table does not have months.
		String[] processNames = {
			"Evap Permeation",
			"Evap Fuel Vapor Venting",
			"Evap Fuel Leaks",
			"Evap Non-Fuel Vapors"
		};
		for(int i=0;i<processNames.length;i++) {
			EmissionProcess process = EmissionProcess.findByName(processNames[i]);
			if(process != null) {
				targetLoop.subscribe(this, process, MasterLoopGranularity.MONTH,
						MasterLoopPriority.GENERATOR);
			}
		}
	}

	/**
	 * Called each time the location or month changes.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
		isMesoscaleLookup = ExecutionRunSpec.theExecutionRunSpec.getModelScale()
				== ModelScale.MESOSCALE_LOOKUP;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

			long start;

			// The following only has to be done once for each run.
			if(!hasBeenSetup) {
				start = System.currentTimeMillis();
				hasBeenSetup = true;
				setupTime = System.currentTimeMillis() - start;
			}

			start = System.currentTimeMillis();
			calculateOpModeDistribution(inContext);
			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,
					"Evaporative Emission Operating Mode Distribution Generation failed.");
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"EEOMDG setupTime=" + setupTime + " totalTime=" + totalTime);
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		// Remove data that was added by this class.  No caching or attempts to retain data
		// between loops is performed since the data depends upon location (link) and
		// very detailed time (year, month, and hour).
		String sql = "";
		try {
			if(polProcessIDs.length() > 0) {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

				sql = "DELETE FROM OpModeDistribution WHERE isUserInput='N' AND linkID = "
						+ context.iterLocation.linkRecordID
						+ " AND polProcessID IN (" + polProcessIDs + ")";
				SQLRunner.executeSQL(db, sql);

				String key = "" +context.iterLocation.linkRecordID;
				contextForLink.put(key,"");
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to delete EE Operating Mode Distribution data", sql);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
				db = null;
			}
		}
	}

	/**
	 * Calculate the operating distribution for evaporative emissions.
	 * @param inContext The current context of the loop.
	**/
	private void calculateOpModeDistribution(MasterLoopContext inContext) {
		String sql = "";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			// Cleanout old data in OpModeDistribution.
			// Do so when the data stored for a link was stored in a different year or month.

			/**
			 * @step 010
			 * @algorithm Delete data for a link that was calculated for a previous year or month than the current context.
			 * @output OpModeDistribution
			**/
			String key = "" +inContext.iterLocation.linkRecordID;
			String newContext = "" + inContext.year + "|" + inContext.monthID;
			String oldContext = contextForLink.get(key);
			if(polProcessIDs.length() > 0) {
				if(oldContext != null && oldContext.length() > 0 && !oldContext.equals(newContext)) {
					sql = "DELETE FROM OpModeDistribution WHERE isUserInput='N' AND linkID = "
							+ inContext.iterLocation.linkRecordID
							+ " AND polProcessID IN (" + polProcessIDs + ")";
					SQLRunner.executeSQL(db, sql);
				}
			}
			contextForLink.put(key,newContext);

			sql = "DROP TABLE IF EXISTS FractionOfOperating";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE TABLE FractionOfOperating ( "
					+ "hourDayID SMALLINT NOT NULL, "
					+ "sourceTypeID SMALLINT NOT NULL, "
					+ "fractionOfOperating FLOAT)";
			SQLRunner.executeSQL(db, sql);

			sql="CREATE UNIQUE INDEX XPKFractionOfOperating ON FractionOfOperating ( "
					+ "hourDayID ASC, sourceTypeID ASC)";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 010
			 * @algorithm fractionOfOperating=least(1,COALESCE(SUM(SHO),0)/SUM(sourceHours)).
			 * @input sourceHours
			 * @input SHO
			 * @output FractionOfOperating
			**/
			sql="INSERT INTO FractionOfOperating (hourDayID, sourceTypeID, fractionOfOperating)"
					+ " SELECT sh.hourDayID, "
					+ "     sh.sourceTypeID, least(1,COALESCE(SUM(SHO),0)/SUM(sourceHours)) AS fractionOfOperating"
					+ " FROM sourceHours sh"
					+ " LEFT JOIN sho sho ON (sho.hourDayID=sh.hourDayID"
					+ "     AND sho.monthID=" + inContext.monthID
					+ "     AND sho.yearID=" + inContext.year
					+ "     AND sho.ageID=sh.ageID"
					+ "     AND sho.linkID=" + inContext.iterLocation.linkRecordID
					+ "     AND sho.sourceTypeID=sh.sourceTypeID)"
					+ " WHERE sourceHours>0"
					+ " AND sh.linkID=" + inContext.iterLocation.linkRecordID
					+ " and sh.monthID=" + inContext.monthID
					+ " and sh.yearID=" + inContext.year
					+ " GROUP BY sh.hourDayID, sh.sourceTypeID"
					+ " ORDER BY sh.hourDayID, sh.sourceTypeID";
			SQLRunner.executeSQL(db, sql);

			sql="DROP TABLE IF EXISTS OpModeDistributionTemp";
			SQLRunner.executeSQL(db, sql);

			sql="CREATE TABLE OpModeDistributionTemp (sourceTypeID	SMALLINT NOT NULL, "
					+ "hourDayID SMALLINT NOT NULL, linkID INTEGER NOT NULL,"
					+ "polProcessID INT NOT NULL, opModeID SMALLINT NOT NULL,"
					+ "opModeFraction FLOAT, key (polProcessID))";
			SQLRunner.executeSQL(db, sql);

			sql="CREATE UNIQUE INDEX XPKOpModeDistributionTemp ON OpModeDistributionTemp ("
					+ "sourceTypeID ASC, hourDayID ASC, linkID ASC, polProcessID ASC, "
					+ "opModeID ASC)";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE INDEX OpTempPolProcID ON OpModeDistributionTemp (polProcessID)";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 010
			 * @algorithm opModeFraction=soakActivityFraction*(1-fractionOfOperating).
			 * @input FractionOfOperating
			 * @input SoakActivityFraction
			 * @input OpModePolProcAssoc
			 * @input PollutantProcessAssoc
			 * @output OpModeDistributionTemp
			**/
			sql = "INSERT INTO OpModeDistributionTemp (sourceTypeID, hourDayID, linkID, "
					+ "polProcessID, opModeID, opModeFraction)"
					+ " SELECT fo.sourceTypeID, fo.hourDayID,"
					+ inContext.iterLocation.linkRecordID + " as linkID, "
					+ "     omppa.polProcessID, omppa.opModeID, "
					+ "     saf.soakActivityFraction*(1-fractionOfOperating) AS opModeFraction"
					+ " FROM FractionOfOperating fo"
					+ " INNER JOIN SoakActivityFraction saf ON (saf.sourceTypeID=fo.sourceTypeID AND saf.hourDayID=fo.hourDayID) "
					+ " INNER JOIN OpModePolProcAssoc omppa ON (omppa.opModeID=saf.opModeID) "
					+ " INNER JOIN PollutantProcessAssoc ppa ON (ppa.polProcessID=omppa.polProcessID) "
					+ " WHERE saf.monthID = " + inContext.monthID
					+ " AND saf.zoneID = " + inContext.iterLocation.zoneRecordID
					+ " AND ppa.processID=" + inContext.iterProcess.databaseKey;
			SQLRunner.executeSQL(db, sql);

			// At this point, all non-operating modes (modes != 300) are in the database along
			// with the relavant pollutant/processes.  Add 100%-sum(non-operating) as operating (mode 300).

			/**
			 * @step 010
			 * @algorithm At this point, all non-operating modes (modes != 300) are in the database along
			 * with the relavant pollutant/processes.  Add 100%-sum(non-operating) as operating (mode 300).
			 * opModeFraction[opModeID=300]=greatest(0,1-sum(opModeFraction)).
			 * @output OpModeDistributionTemp
			**/
			sql = "INSERT INTO OpModeDistributionTemp (sourceTypeID, hourDayID, linkID, "
					+ "polProcessID, opModeID, opModeFraction) "
					+ "SELECT sourceTypeID, hourDayID, linkID, polProcessID, 300, "
					+ "greatest(0,1-SUM(opModeFraction)) FROM OpModeDistributionTemp "
					+ "GROUP BY sourceTypeID, hourDayID, linkID, polProcessID ORDER BY NULL";
			SQLRunner.executeSQL(db, sql);

			/**
			 * @step 010
			 * @algorithm Copy OpModeDistributionTemp into OpModeDistribution.
			 * @input OpModeDistributionTemp
			 * @output OpModeDistribution
			**/
			sql = "INSERT IGNORE INTO OpModeDistribution (sourceTypeID, hourDayID, linkID, "
					+ "polProcessID, opModeID, opModeFraction, isUserInput) SELECT sourceTypeID, "
					+ "hourDayID, linkID, polProcessID, opModeID, opModeFraction, 'N' FROM "
					+ "OpModeDistributionTemp";
			SQLRunner.executeSQL(db, sql);

			// Get distinct polProcessID in OpModeDistributionTemp as these are the ones to
			// be cleaned out of OpModeDistribution during the next year or month.
			sql = "SELECT DISTINCT polProcessID FROM OpModeDistributionTemp";
			polProcessIDs = "";
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					if(polProcessIDs.length() > 0) {
						polProcessIDs += ",";
					}
					polProcessIDs += results.getString(1);
				}
			}
			results.close();
			results = null;
			statement.close();
			statement = null;
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not determine Evaporative Emissions Operating Mode "
					+ "Distribution.", sql);
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(SQLException e) {
					// Nothing can be done here
				}
				results = null;
			}
			if(statement!=null) {
				try {
					statement.close();
				} catch(SQLException e) {
					// Failure to close on a preparedstatement should not be an issue.
				}
				statement = null;
			}
		}
	}
}
