/**************************************************************************************************
 * @(#)SourceTypePhysics.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import java.util.*;
import java.sql.*;

/**
 * Utilities to create modelyear-specific VSP and operating modes.
 *
 * @author		Wesley Faler
 * @version		2014-05-28
**/
public class SourceTypePhysics {
	/** Flags for tasks already done, used to prevent duplicate execution **/
	TreeSet<String> alreadyDoneFlags = new TreeSet<String>();

	/**
	 * Populate the sourceUseTypePhysicsMapping table, making it appropriate for use with VSP creation.
	 * @param db database connection to use
	 * @throws SQLException if anything goes wrong
	**/
	public void setup(Connection db) throws SQLException {
		String alreadyKey = "setup";
		if(alreadyDoneFlags.contains(alreadyKey)) {
			return;
		}
		alreadyDoneFlags.add(alreadyKey);

		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			// Fill the mapping table with basic data
			String[] statements = {
				"drop table if exists sourceUseTypePhysicsMapping",

				"create table sourceUseTypePhysicsMapping ("
						+ " 	realSourceTypeID smallint not null,"
						+ " 	tempSourceTypeID smallint not null,"
						+ " 	beginModelYearID smallint not null,"
						+ " 	endModelYearID smallint not null,"
						+ " 	opModeIDOffset smallint not null,"

						+ " 	rollingTermA float DEFAULT NULL,"
						+ " 	rotatingTermB float DEFAULT NULL,"
						+ " 	dragTermC float DEFAULT NULL,"
						+ " 	sourceMass float DEFAULT NULL,"
						+ " 	fixedMassFactor float DEFAULT NULL,"

						+ " 	primary key (realSourceTypeID, beginModelYearID, endModelYearID),"
						+ " 	key (beginModelYearID, endModelYearID, realSourceTypeID),"
						+ " 	key (tempSourceTypeID, realSourceTypeID, beginModelYearID, endModelYearID)"
						+ ")",

				"insert into sourceUseTypePhysicsMapping (realSourceTypeID, tempSourceTypeID, "
						+ " 	beginModelYearID, endModelYearID, opModeIDOffset,"
						+ " 	rollingTermA, rotatingTermB, dragTermC, sourceMass, fixedMassFactor)"
						+ " select sourceTypeID as realSourceTypeID, sourceTypeID as tempSourceTypeID,"
						+ " 	beginModelYearID, endModelYearID, 0 as opModeIDOffset,"
						+ " 	rollingTermA, rotatingTermB, dragTermC, sourceMass, fixedMassFactor"
						+ " from sourceUseTypePhysics"
			};
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
			// Create temporary source types and assign op mode mapping
			sql = "select * from sourceUseTypePhysicsMapping order by realSourceTypeID, beginModelYearID";
			ArrayList<String> updateStatements = new ArrayList<String>();
			int nextTempSourceTypeID = 100;
			int nextOpModeIDOffset = 1000;
			int priorRealSourceTypeID = 0;
			int priorBeginModelYearID = 0;
			boolean isFirst = true;
			query.open(db,sql);
			while(query.rs.next()) {
				int realSourceTypeID = query.rs.getInt("realSourceTypeID");
				int beginModelYearID = query.rs.getInt("beginModelYearID");
				if(!isFirst && realSourceTypeID == priorRealSourceTypeID && beginModelYearID != priorBeginModelYearID) {
					sql = "update sourceUseTypePhysicsMapping set tempSourceTypeID=" + nextTempSourceTypeID
							+ ", opModeIDOffset=" + nextOpModeIDOffset
							+ " where realSourceTypeID=" + realSourceTypeID
							+ " and beginModelYearID=" + beginModelYearID;
					updateStatements.add(sql);
					nextTempSourceTypeID++;
					nextOpModeIDOffset += 500;
				} else {
					nextOpModeIDOffset = 1000;
				}
				isFirst = false;
				priorRealSourceTypeID = realSourceTypeID;
				priorBeginModelYearID = beginModelYearID;
			}
			query.close();
			for(Iterator<String> i=updateStatements.iterator();i.hasNext();) {
				sql = i.next();
				SQLRunner.executeSQL(db,sql);
			}
		} catch(SQLException e) {
			Logger.logError(e,"Unable to setup sourceUseTypePhysicsMapping");
			throw e;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Update an operating mode distribution table, replacing temporary source types with
	 * real source types and changing real operating modes to modelyear-specific modes.
	 * @param db database connection to use
	 * @param tableName table containing an operating mode distribution and using standard
	 * column names of "sourceTypeID" and "opModeID".
	 * @throws SQLException if anything goes wrong
	**/
	public void updateOperatingModeDistribution(Connection db, String tableName) throws SQLException {
		updateOperatingModeDistribution(db,tableName,null);
	}

	/**
	 * Update an operating mode distribution table, replacing temporary source types with
	 * real source types and changing real operating modes to modelyear-specific modes.
	 * @param db database connection to use
	 * @param tableName table containing an operating mode distribution and using standard
	 * column names of "sourceTypeID" and "opModeID".
	 * @param whereClause optional string containing an expression to add to the WHERE clause
	 * used to update the table.  Use this to improve performance when incrementally constructing
	 * a table.
	 * @throws SQLException if anything goes wrong
	**/
	public void updateOperatingModeDistribution(Connection db, String tableName, String whereClause) throws SQLException {
		String alreadyKey = "updateOMD|" + tableName + "|" + StringUtilities.safeGetString(whereClause);
		if(alreadyDoneFlags.contains(alreadyKey)) {
			//return;
		}
		alreadyDoneFlags.add(alreadyKey);

		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			sql = "select realSourceTypeID, tempSourceTypeID, opModeIDOffset"
					+ " from sourceUseTypePhysicsMapping"
					+ " where realSourceTypeID <> tempSourceTypeID"
					+ " order by realSourceTypeID, beginModelYearID";
			ArrayList<String> updateStatements = new ArrayList<String>();
			query.open(db,sql);
			while(query.rs.next()) {
				int realSourceTypeID = query.rs.getInt("realSourceTypeID");
				int tempSourceTypeID = query.rs.getInt("tempSourceTypeID");
				int opModeIDOffset = query.rs.getInt("opModeIDOffset");

				sql = "update " + tableName + " set sourceTypeID=" + realSourceTypeID
						+ ", opModeID=opModeID + " + opModeIDOffset
						+ " where sourceTypeID=" + tempSourceTypeID
						+ " and opModeID > 1 and opModeID < 100";
				if(whereClause != null && whereClause.length() > 0) {
					sql += " and (" + whereClause + ")";
				}
				updateStatements.add(sql);
			}
			query.close();
			for(Iterator<String> i=updateStatements.iterator();i.hasNext();) {
				sql = i.next();
				SQLRunner.executeSQL(db,sql);
			}
		} catch(SQLException e) {
			Logger.logError(e,"Unable to updateOperatingModeDistribution");
			throw e;
		} finally {
			query.onFinally();
		}
	}

	static class SourceTypeOpMode {
		public int sourceTypeID;
		public int opModeID;
		public int newOpModeID;
		public int beginModelYearID;
		public int endModelYearID;
	}

	/**
	 * Get the new operating mode IDs for each opModeID.
	 * @param db database connection to use
	 * @throws SQLException if anything goes wrong
	**/
	ArrayList<SourceTypeOpMode> getOpModeUpdates(Connection db) throws SQLException {
		ArrayList<SourceTypeOpMode> opModeMap = new ArrayList<SourceTypeOpMode>();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			ArrayList<Integer> opModes = new ArrayList<Integer>();
			sql = "select opModeID from operatingMode where opModeID > 1 and opModeID < 100";
			query.open(db,sql);
			while(query.rs.next()) {
				opModes.add(new Integer(query.rs.getInt(1)));
			}
			query.close();

			ArrayList<Integer> bases = new ArrayList<Integer>();
			sql = "select distinct realSourceTypeID, opModeIDOffset, beginModelYearID, endModelYearID"
					+ " from sourceUseTypePhysicsMapping"
					+ " where opModeIDOffset > 0"
					+ " order by opModeIDOffset";
			query.open(db,sql);
			while(query.rs.next()) {
				int sourceTypeID = query.rs.getInt(1);
				int offset = query.rs.getInt(2);
				int beginModelYearID = query.rs.getInt(3);
				int endModelYearID = query.rs.getInt(4);

				for(Iterator<Integer> i=opModes.iterator();i.hasNext();) {
					SourceTypeOpMode t = new SourceTypeOpMode();
					t.sourceTypeID = sourceTypeID;
					t.opModeID = i.next().intValue();
					t.newOpModeID = t.opModeID + offset;
					t.beginModelYearID = beginModelYearID;
					t.endModelYearID = endModelYearID;
					opModeMap.add(t);
				}
			}
			query.close();
		} catch(SQLException e) {
			Logger.logError(e,"Unable to setup getOpModeUpdates");
			throw e;
		} finally {
			query.onFinally();
		}
		return opModeMap;
	}

	/**
	 * Change operating mode assignments for sourcebins in affected model years
	 * in the EmissionRate and EmissionRateByAge tables.  Also ensure the new
	 * operating modes are associated with the pollutant/processes.
	 * @param db database connection to use
	 * @param processID identifier of the affected emission process
	 * @throws SQLException if anything goes wrong
	**/
	public void updateEmissionRateTables(Connection db, int processID) throws SQLException {
		String alreadyKey = "updateEmissionRates|" + processID;
		if(alreadyDoneFlags.contains(alreadyKey)) {
			return;
		}
		alreadyDoneFlags.add(alreadyKey);

		SQLRunner.Query query = new SQLRunner.Query();
		ArrayList<String> updateStatements = new ArrayList<String>();
		String[] tableNames = {
			"emissionRate",
			"emissionRateByAge"
		};

		TreeMap<Integer,TreeSet<Integer>> offsetsByPolProcess = new TreeMap<Integer,TreeSet<Integer>>();

		String sql = "select distinct stpm.opModeIDOffset, sbd.polProcessID, sbd.sourceBinID"
				+ " from sourceTypePolProcess stpp"
				+ " inner join sourceUseTypePhysicsMapping stpm on ("
				+ " 	stpm.realSourceTypeID=stpp.sourceTypeID"
				+ " 	and stpm.opModeIDOffset>0)"
				+ " inner join pollutantProcessAssoc ppa on ("
				+ " 	ppa.polProcessID=stpp.polProcessID"
				+ " 	and ppa.processID=" + processID
				+ " 	and stpp.isMYGroupReqd='Y')"
				+ " inner join sourceBinDistribution sbd on ("
				+ " 	sbd.polProcessID=ppa.polProcessID)"
				+ " inner join sourceTypeModelYear stmy on ("
				+ " 	stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID"
				+ " 	and stmy.sourceTypeID=stpm.realSourceTypeID"
				+ " 	and stmy.modelYearID >= stpm.beginModelYearID"
				+ " 	and stmy.modelYearID <= stpm.endModelYearID)"
				+ " order by stpm.opModeIDOffset, sbd.polProcessID";
		try {
			// Build update statements
			query.open(db,sql);
			int opModeIDOffset = 0, polProcessID = 0;
			String sourceBinIDs = "";
			while(query.rs.next()) {
				int tempOpModeIDOffset = query.rs.getInt(1);
				int tempPolProcessID = query.rs.getInt(2);
				String tempSourceBinID = query.rs.getString(3);
				if(tempOpModeIDOffset != opModeIDOffset || tempPolProcessID != polProcessID) {
					if(sourceBinIDs.length() > 0) {
						for(int ti=0;ti<tableNames.length;ti++) {
							String tableName = tableNames[ti];
							sql = "update " + tableName + " set opModeID=opModeID+" + opModeIDOffset
									+ " where opModeID > 1 and opModeID < 100"
									+ " and polProcessID=" + polProcessID
									+ " and sourceBinID in (" + sourceBinIDs + ")";
							updateStatements.add(sql);
						}

						sql = "insert ignore into fullACAdjustment (sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID)"
								+ " select sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID + " + opModeIDOffset
								+ " from fullACAdjustment"
								+ " where opModeID > 1 and opModeID < 100"
								+ " and polProcessID = " + polProcessID;
						updateStatements.add(sql);

					}
					opModeIDOffset = tempOpModeIDOffset;
					polProcessID = tempPolProcessID;
					sourceBinIDs = "";

					Integer polProcessInt = new Integer(polProcessID);					
					TreeSet<Integer> offsets = offsetsByPolProcess.get(polProcessInt);
					if(offsets == null) {
						offsets = new TreeSet<Integer>();
						offsetsByPolProcess.put(polProcessInt,offsets);
					}
					offsets.add(new Integer(opModeIDOffset));
				}
				if(sourceBinIDs.length() > 0) {
					sourceBinIDs += ",";
				}
				sourceBinIDs += tempSourceBinID;
			}
			if(sourceBinIDs.length() > 0) {
				for(int ti=0;ti<tableNames.length;ti++) {
					String tableName = tableNames[ti];
					sql = "update " + tableName + " set opModeID=opModeID+" + opModeIDOffset
							+ " where opModeID > 1 and opModeID < 100"
							+ " and polProcessID=" + polProcessID
							+ " and sourceBinID in (" + sourceBinIDs + ")";
					updateStatements.add(sql);
				}

				sql = "insert ignore into fullACAdjustment (sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID)"
						+ " select sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID + " + opModeIDOffset
						+ " from fullACAdjustment"
						+ " where opModeID > 1 and opModeID < 100"
						+ " and polProcessID = " + polProcessID;
				updateStatements.add(sql);

			}
			query.close();
			// Execute the updates
			for(Iterator<String> i=updateStatements.iterator();i.hasNext();) {
				sql = i.next();
				SQLRunner.executeSQL(db,sql);
			}
			// Ensure opModePolProcAssoc has the required values
			TreeSet<Integer> allOffsets = new TreeSet<Integer>();
			Set<Integer> keys = offsetsByPolProcess.keySet();
			for(Iterator<Integer> i=offsetsByPolProcess.keySet().iterator();i.hasNext();) {
				Integer polProcessInt = i.next();
				TreeSet<Integer> offsets = offsetsByPolProcess.get(polProcessInt);
				if(offsets != null) {
					for(Iterator<Integer> oi=offsets.iterator();oi.hasNext();) {
						Integer offset = oi.next();
						allOffsets.add(offset);
						for(int ti=0;ti<tableNames.length;ti++) {
							String tableName = tableNames[ti];
							sql = "insert ignore into opModePolProcAssoc (polProcessID, opModeID)"
									+ " select distinct " + polProcessInt + ", opModeID"
									+ " from " + tableName
									+ " where polProcessID=" + polProcessInt
									+ " and opModeID > 1 + " + offset + " and opModeID < 100 + " + offset;
							SQLRunner.executeSQL(db,sql);
						}
					}
				}
			}

			// Ensure roadOpModeDistribution has the required values
			for(Iterator<Integer> i=allOffsets.iterator();i.hasNext();) {
				sql = "insert ignore into roadOpModeDistribution (sourceTypeID, roadTypeID, isRamp, avgSpeedBinID, opModeFraction, opModeFractionCV, opModeID)"
						+ " select sourceTypeID, roadTypeID, isRamp, avgSpeedBinID, opModeFraction, opModeFractionCV, opModeID + " + i.next()
						+ " from roadOpModeDistribution"
						+ " where opModeID > 1 and opModeID < 100";
				SQLRunner.executeSQL(db,sql);

				sql = "analyze table roadOpModeDistribution";
				SQLRunner.executeSQL(db,sql);
			}

			for(int i=0;i<tableNames.length;i++) {
				sql = "analyze table " + tableNames[i];
				SQLRunner.executeSQL(db,sql);
			}
			sql = "analyze table opModePolProcAssoc";
			SQLRunner.executeSQL(db,sql);
		} catch(SQLException e) {
			Logger.logError(e,"Unable to update emission rate tables for source type physics");
			throw e;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Create and populate the physicsOperatingMode table containing all operating modes.
	 * @param db database connection to use
	 * @throws SQLException if anything goes wrong
	**/	
	public void createExpandedOperatingModesTable(Connection db) throws SQLException {
		String alreadyKey = "createExpandedOperatingModesTable";
		if(alreadyDoneFlags.contains(alreadyKey)) {
			return;
		}
		alreadyDoneFlags.add(alreadyKey);

		String sql = "";
		try {
			String[] statements = {
				"drop table if exists physicsOperatingMode",
	
				"create table physicsOperatingMode like operatingMode",
	
				"insert into physicsOperatingMode select * from operatingMode",
	
				"insert ignore into physicsOperatingMode (opModeID, opModeName, VSPLower, VSPUpper, speedLower, speedUpper, brakeRate1Sec, brakeRate3Sec, minSoakTime, maxSoakTime)"
						+ " select distinct opModeID+opModeIDOffset, "
						+ " 	opModeName, VSPLower, VSPUpper, speedLower, speedUpper, brakeRate1Sec, brakeRate3Sec, minSoakTime, maxSoakTime"
						+ " from operatingMode, sourceUseTypePhysicsMapping"
						+ " where opModeID > 1 and opModeID < 100"
						+ " and opModeIDOffset>0"
			};
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
		} catch(SQLException e) {
			Logger.logError(e,"Unable to update emission rate tables for source type physics");
			throw e;
		}
	}
}
