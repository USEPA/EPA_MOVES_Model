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
 * @version		2017-04-08
**/
public class SourceTypePhysics {
	/** Flags for tasks already done, used to prevent duplicate execution **/
	TreeSet<String> alreadyDoneFlags = new TreeSet<String>();
	/** Flags for error messages already reported, used to prevent duplicate messages **/
	TreeSet<String> alreadyDoneMessages = new TreeSet<String>();

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
						+ " 	regClassID smallint not null,"
						+ " 	beginModelYearID smallint not null,"
						+ " 	endModelYearID smallint not null,"
						+ " 	opModeIDOffset smallint not null,"

						+ " 	rollingTermA float DEFAULT NULL,"
						+ " 	rotatingTermB float DEFAULT NULL,"
						+ " 	dragTermC float DEFAULT NULL,"
						+ " 	sourceMass float DEFAULT NULL,"
						+ " 	fixedMassFactor float DEFAULT NULL,"

						+ " 	primary key (realSourceTypeID, regClassID, beginModelYearID, endModelYearID),"
						+ " 	key (beginModelYearID, endModelYearID, realSourceTypeID, regClassID),"
						+ " 	key (tempSourceTypeID, realSourceTypeID, regClassID, beginModelYearID, endModelYearID)"
						+ ")",

				"insert ignore into sourceUseTypePhysicsMapping (realSourceTypeID, tempSourceTypeID, regClassID, "
						+ " 	beginModelYearID, endModelYearID, opModeIDOffset,"
						+ " 	rollingTermA, rotatingTermB, dragTermC, sourceMass, fixedMassFactor)"
						+ " select sourceTypeID as realSourceTypeID, sourceTypeID as tempSourceTypeID, regClassID,"
						+ " 	beginModelYearID, endModelYearID, 0 as opModeIDOffset,"
						+ " 	rollingTermA, rotatingTermB, dragTermC, sourceMass, fixedMassFactor"
						+ " from sourceUseTypePhysics"
						+ " join runspecmodelyear rsmy"
						+ " where rsmy.modelYearID between beginModelYearID and endModelYearID"
			};
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
			// Create temporary source types and assign op mode mapping
			sql = "select * from sourceUseTypePhysicsMapping order by realSourceTypeID, regClassID, beginModelYearID";
			ArrayList<String> updateStatements = new ArrayList<String>();
			int nextTempSourceTypeID = 100;
			int nextOpModeIDOffset = 1000;
			int priorRealSourceTypeID = 0;
			int priorRegClassID = 0;
			int priorBeginModelYearID = 0;
			boolean isFirst = true;
			query.open(db,sql);
			while(query.rs.next()) {
				int realSourceTypeID = query.rs.getInt("realSourceTypeID");
				int regClassID = query.rs.getInt("regClassID");
				int beginModelYearID = query.rs.getInt("beginModelYearID");
				
				/*
				A new sourceTypeID should be assigned for each combination of [realSourceTypeID, regClassID, beginModelYearID].
				Each realSourceTypeID gets its own offset sequence, starting at 1000 and adding 100 for each [regClassID, beginModelYearID] combination.
				*/
				if(!isFirst && realSourceTypeID == priorRealSourceTypeID && (regClassID != priorRegClassID || beginModelYearID != priorBeginModelYearID)) {
					sql = "update sourceUseTypePhysicsMapping set tempSourceTypeID=" + nextTempSourceTypeID
							+ ", opModeIDOffset=" + nextOpModeIDOffset
							+ " where realSourceTypeID=" + realSourceTypeID
							+ " and regClassID=" + regClassID
							+ " and beginModelYearID=" + beginModelYearID;
					updateStatements.add(sql);
					nextTempSourceTypeID++;
					nextOpModeIDOffset += 100; // Opmodes used are 0-99, so an offset of 100 won't overlap
				} else {
					// Leave the current row using the original sourceTypeID tied to the original operating modes.
					// The next modelyear break for this [sourceTypeID, regClassID] will get new operating modes.

					sql = "update sourceUseTypePhysicsMapping set tempSourceTypeID=" + nextTempSourceTypeID
							+ ", opModeIDOffset=" + nextOpModeIDOffset
							+ " where realSourceTypeID=" + realSourceTypeID
							+ " and regClassID=" + regClassID
							+ " and beginModelYearID=" + beginModelYearID;
					updateStatements.add(sql);
					nextTempSourceTypeID++;
					nextOpModeIDOffset += 100; // Opmodes used are 0-99, so an offset of 100 won't overlap
				}
				isFirst = false;
				priorRealSourceTypeID = realSourceTypeID;
				priorRegClassID = regClassID;
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
			sql = "select distinct realSourceTypeID, tempSourceTypeID, opModeIDOffset"
					+ " from sourceUseTypePhysicsMapping"
					+ " join (select distinct sourceTypeID,linkID from opmodedistribution) omd"
					+ " on (omd.sourceTypeID = realSourceTypeID or omd.sourceTypeID = tempSourceTypeID)"
					+ " where realSourceTypeID <> tempSourceTypeID";
					if(whereClause != null && whereClause.length() > 0) {
						sql += " and (" + whereClause + ")";
					}
					sql += " order by realSourceTypeID, beginModelYearID";
			ArrayList<String> updateStatements = new ArrayList<String>();
			query.open(db,sql);
			
			while(query.rs.next()) {	
				int realSourceTypeID = query.rs.getInt("realSourceTypeID");
				int tempSourceTypeID = query.rs.getInt("tempSourceTypeID");
				int opModeIDOffset = query.rs.getInt("opModeIDOffset");

				// Change source types for any new operating modes
				sql = "update " + tableName + " set sourceTypeID=" + realSourceTypeID
						+ " where sourceTypeID=" + tempSourceTypeID
						+ " and opModeID >= 0+" + opModeIDOffset + " and opModeID < 100+" + opModeIDOffset
						+ " and (polProcessID < 0 or mod(polProcessID,100) = 1)";
				if(whereClause != null && whereClause.length() > 0) {
					sql += " and (" + whereClause + ")";
				}
				updateStatements.add(sql);
				
				sql = "create table if not exists physics_" + tableName
						+ " like " + tableName;
				updateStatements.add(sql);

				// Change source types for brakewear
				sql = "truncate physics_" + tableName;
				updateStatements.add(sql);
				
				
				sql = "insert into physics_" + tableName
						+ " select * from " + tableName
						+ " where sourceTypeID=" + tempSourceTypeID
						+ " and polProcessID = 11609";
				if(whereClause != null && whereClause.length() > 0) {
					sql += " and (" + whereClause + ")";
				}
				updateStatements.add(sql);
				
				sql = "delete from " + tableName
						+ " where sourceTypeID=" + tempSourceTypeID
						+ " and polProcessID = 11609";
				if(whereClause != null && whereClause.length() > 0) {
					sql += " and (" + whereClause + ")";
				}
				updateStatements.add(sql);

				sql = "update physics_" + tableName + " set sourceTypeID=" + realSourceTypeID
					+ ", opModeID=opModeID+ " + opModeIDOffset
					+ " where opModeID <> 501";
				if(whereClause != null && whereClause.length() > 0) {
					sql += " and (" + whereClause + ")";
				}
				updateStatements.add(sql);

				sql = "insert ignore into " + tableName + " select * from physics_" + tableName;
				updateStatements.add(sql);

				// Promote old operating modes and change source types
				// This statement fail (which is ok and can be ignored) if
				// entries exist with extended operating modes already.
				sql = "update " + tableName + " set sourceTypeID=" + realSourceTypeID
						+ ", opModeID=opModeID + " + opModeIDOffset
						+ " where sourceTypeID=" + tempSourceTypeID
						+ " and opModeID >= 0 and opModeID < 100"
						+ " and (polProcessID < 0 or mod(polProcessID,100) = 1)";
				if(whereClause != null && whereClause.length() > 0) {
					sql += " and (" + whereClause + ")";
				}
				updateStatements.add(sql);
				
				if(opModeIDOffset > 0) {
					sql = "delete from " + tableName
							+ " where sourceTypeID=" + tempSourceTypeID
							+ " and opModeID >= 0 and opModeID < 100"
							+ " and (polProcessID < 0 or mod(polProcessID,100) = 1)"
							+ " and isUserInput = 'N'";
					if(whereClause != null && whereClause.length() > 0) {
						sql += " and (" + whereClause + ")";
					}
					updateStatements.add(sql);

					// tempSourceTypeID never equals realSourceTypeID any more, so get rid of real source type operating modes
					sql = "delete from " + tableName
							+ " where sourceTypeID=" + realSourceTypeID
							+ " and opModeID >= 0 and opModeID < 100"
							+ " and (polProcessID < 0 or mod(polProcessID,100) = 1)"
							+ " and isUserInput = 'N'";
					if(whereClause != null && whereClause.length() > 0) {
						sql += " and (" + whereClause + ")";
					}
					updateStatements.add(sql);
				}
			}
			query.close();
			for(Iterator<String> i=updateStatements.iterator();i.hasNext();) {
				sql = i.next();
				try {
					SQLRunner.executeSQL(db,sql);
				} catch(SQLException e) {
					// Nothing to do here
				}
			}

		} catch(SQLException e) {
			Logger.logError(e,"Unable to updateOperatingModeDistribution");
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
	 * @param whereClause optional string containing an expression to add to the WHERE clause
	 * used to update the table.  Use this to improve performance when incrementally constructing
	 * a table.
	 * @throws SQLException if anything goes wrong
	**/
	public void updateOpModes(Connection db, String tableName) throws SQLException {
		String alreadyKey = "updateOpModes|" + tableName;
		if(alreadyDoneFlags.contains(alreadyKey)) {
			//return;
		}
		alreadyDoneFlags.add(alreadyKey);

		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		TreeSet<Integer> seenRealSourceTypeIDs = new TreeSet<Integer>();
		try {
			sql = "select distinct realSourceTypeID, tempSourceTypeID, opModeIDOffset"
					+ " from sourceUseTypePhysicsMapping"
					+ " where realSourceTypeID <> tempSourceTypeID"
					+ " order by realSourceTypeID, beginModelYearID";
			ArrayList<String> updateStatements = new ArrayList<String>();
			query.open(db,sql);
			while(query.rs.next()) {
				int realSourceTypeID = query.rs.getInt("realSourceTypeID");
				int tempSourceTypeID = query.rs.getInt("tempSourceTypeID");
				int opModeIDOffset = query.rs.getInt("opModeIDOffset");

				// Translate the old operating mode to a new operating mode
				sql = "drop table if exists physics_" + tableName;
				updateStatements.add(sql);

				sql = "create table physics_" + tableName + " like " + tableName;
				updateStatements.add(sql);
				
				sql = "insert into physics_" + tableName
						+ " select * from " + tableName
						+ " where opModeID >= 0 and opModeID < 100"
						+ " and sourceTypeID=" + tempSourceTypeID
						+ " and (polProcessID < 0 or mod(polProcessID,100) = 1)";
				updateStatements.add(sql);
		
				sql = "update physics_" + tableName + " set opModeID=opModeID+" + opModeIDOffset;
				updateStatements.add(sql);
				
				sql = "insert ignore into " + tableName + " select * from physics_" + tableName;
				updateStatements.add(sql);
				
				Integer t = Integer.valueOf(realSourceTypeID);
				if(!seenRealSourceTypeIDs.contains(t)) {
					seenRealSourceTypeIDs.add(t);

					sql = "truncate table physics_" + tableName;
					updateStatements.add(sql);

					// Handle brakewear special case for pollutant 11609
					sql = "insert into physics_" + tableName
							+ " select * from " + tableName
							+ " where (opModeID=501 or (opModeID >= 0 and opModeID < 100))"
							+ " and sourceTypeID=" + tempSourceTypeID
							+ " and polProcessID=11609 and roadTypeID in (2,4)";
					updateStatements.add(sql);
	
					// Finish the updates by changing to the real sourceTypeID
					sql = "update physics_" + tableName
							+ " set sourceTypeID=" + realSourceTypeID;
					updateStatements.add(sql);
	
					sql = "insert ignore into " + tableName
							+ " select * from physics_" + tableName;
					updateStatements.add(sql);
				}
			}
			query.close();
			for(Iterator<String> i=updateStatements.iterator();i.hasNext();) {
				sql = i.next();
				try {
					SQLRunner.executeSQL(db,sql);
				} catch(SQLException e) {
					Logger.logError(e,"Unable to updateOpModes using: " + sql);
					// Nothing to do here
				}
			}
		} catch(SQLException e) {
			Logger.logError(e,"Unable to updateOpModes using: " + sql);
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
			sql = "select opModeID from operatingMode where opModeID >= 0 and opModeID < 100";
			query.open(db,sql);
			while(query.rs.next()) {
				opModes.add(Integer.valueOf(query.rs.getInt(1)));
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
		if(!(processID == 9 || processID == 1)) {
			//Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables skipped for process " + processID);
			return;
		}
		Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables starting for process " + processID);
		String alreadyKey = "updateEmissionRates|" + processID;
		if(alreadyDoneFlags.contains(alreadyKey)) {
			Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables already done for process " + processID);
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

        // DBC - Brakewear now varies by model year, so we want to do the offsets for all entries in 
        // sourceTypePolProcess where isMYGroupReqd = 'Y'. Previously, we only wanted running pollutants
        // that varied by model year (along with brakewear which didn't), so this was a complicated join before.
        // Now, we can simply just look for all rows where isMYGroupReqd = 'Y'
		char MYGroupYesOrNo = 'Y';

		String sql = "select distinct stpm.opModeIDOffset, sbd.polProcessID, sbd.sourceBinID"
				+ " from sourceTypePolProcess stpp"
				+ " inner join sourceUseTypePhysicsMapping stpm on ("
				+ " 	stpm.realSourceTypeID=stpp.sourceTypeID"
				+ " 	and stpm.opModeIDOffset>0)"
				+ " inner join pollutantProcessAssoc ppa on ("
				+ " 	ppa.polProcessID=stpp.polProcessID"
				+ " 	and ppa.processID=" + processID
				+ " 	and stpp.isMYGroupReqd='" + MYGroupYesOrNo + "')"
				+ " inner join sourceBinDistribution sbd on ("
				+ " 	sbd.polProcessID=ppa.polProcessID)"
				+ " inner join sourceTypeModelYear stmy on ("
				+ " 	stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID"
				+ " 	and stmy.sourceTypeID=stpm.realSourceTypeID"
				+ " 	and stmy.modelYearID >= stpm.beginModelYearID"
				+ " 	and stmy.modelYearID <= stpm.endModelYearID)"
				+ " inner join sourceBin sb on ("
				+ " 	sb.sourceBinID = sbd.sourceBinID"
				+ " 	and (sb.regClassID = 0 or sb.regClassID = stpm.regClassID or stpm.regClassID = 0) )"
				+ " order by stpm.opModeIDOffset, sbd.polProcessID";

		try {
			checkSourceBins(db,processID);

			// Build update statements
			query.open(db,sql);
			int opModeIDOffset = 0, polProcessID = 0;
			String sourceBinIDs = "";
			TreeMap<String,TreeSet<String> > polProcessIDsByOpModeIDOffset = new TreeMap<String,TreeSet<String> >();
			while(query.rs.next()) {
				int tempOpModeIDOffset = query.rs.getInt(1);
				int tempPolProcessID = query.rs.getInt(2);
				String tempSourceBinID = query.rs.getString(3);

				//Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables opModeIDOffset="+tempOpModeIDOffset + " polProcessID=" + tempPolProcessID + " sourceBinID=" + tempSourceBinID);
				if(tempOpModeIDOffset != opModeIDOffset || tempPolProcessID != polProcessID) {
					if(sourceBinIDs.length() > 0) {
						for(int ti=0;ti<tableNames.length;ti++) {
							String tableName = tableNames[ti];
							copyOperatingModes(updateStatements,tableName,opModeIDOffset,polProcessID,sourceBinIDs);
						}

						/*
						sql = "insert ignore into fullACAdjustment (sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID)"
								+ " select sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID + " + opModeIDOffset
								+ " from fullACAdjustment"
								+ " where opModeID >= 0 and opModeID < 100"
								+ " and polProcessID = " + polProcessID;
						updateStatements.add(sql);
						*/
						String textOpModeIDOffset = "" + opModeIDOffset;
						String textPolProcessID = "" + polProcessID;
						TreeSet<String> opModeOffsetList = polProcessIDsByOpModeIDOffset.get(textOpModeIDOffset);
						if(opModeOffsetList == null) {
							opModeOffsetList = new TreeSet<String>();
							polProcessIDsByOpModeIDOffset.put(textOpModeIDOffset,opModeOffsetList);
						}
						opModeOffsetList.add(textPolProcessID);
					}
					opModeIDOffset = tempOpModeIDOffset;
					polProcessID = tempPolProcessID;
					sourceBinIDs = "";

					Integer polProcessInt = Integer.valueOf(polProcessID);					
					TreeSet<Integer> offsets = offsetsByPolProcess.get(polProcessInt);
					if(offsets == null) {
						offsets = new TreeSet<Integer>();
						offsetsByPolProcess.put(polProcessInt,offsets);
					}
					offsets.add(Integer.valueOf(opModeIDOffset));
				}
				if(sourceBinIDs.length() > 0) {
					sourceBinIDs += ",";
				}
				sourceBinIDs += tempSourceBinID;
			}
			if(sourceBinIDs.length() > 0) {
				for(int ti=0;ti<tableNames.length;ti++) {
					String tableName = tableNames[ti];
					copyOperatingModes(updateStatements,tableName,opModeIDOffset,polProcessID,sourceBinIDs);
				}

				/*
				sql = "insert ignore into fullACAdjustment (sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID)"
						+ " select sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID + " + opModeIDOffset
						+ " from fullACAdjustment"
						+ " where opModeID >= 0 and opModeID < 100"
						+ " and polProcessID = " + polProcessID;
				updateStatements.add(sql);
				*/
				String textOpModeIDOffset = "" + opModeIDOffset;
				String textPolProcessID = "" + polProcessID;
				TreeSet<String> opModeOffsetList = polProcessIDsByOpModeIDOffset.get(textOpModeIDOffset);
				if(opModeOffsetList == null) {
					opModeOffsetList = new TreeSet<String>();
					polProcessIDsByOpModeIDOffset.put(textOpModeIDOffset,opModeOffsetList);
				}
				opModeOffsetList.add(textPolProcessID);
			}
			query.close();
			for(String textOpModeIDOffset : polProcessIDsByOpModeIDOffset.keySet()) {
				TreeSet<String> polProcessIDs = polProcessIDsByOpModeIDOffset.get(textOpModeIDOffset);
				if(polProcessIDs == null || polProcessIDs.size() <= 0) {
					continue;
				}
				sql = "insert ignore into fullACAdjustment (sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID)"
						+ " select sourceTypeID, polProcessID, fullACAdjustment, fullACAdjustmentCV, opModeID + " + textOpModeIDOffset
						+ " from fullACAdjustment"
						+ " where opModeID >= 0 and opModeID < 100"
						+ " and polProcessID in (";
				boolean isFirst = true;
				for(String textPolProcessID : polProcessIDs) {
					if(!isFirst) {
						sql += ",";
					}
					sql += textPolProcessID;
					isFirst = false;
				}
				sql += ")";
				updateStatements.add(sql);
			}
			// Execute the updates
			for(Iterator<String> i=updateStatements.iterator();i.hasNext();) {
				sql = i.next();
				SQLRunner.executeSQL(db,sql);
				//Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables "+sql);
			}
			updateStatements.clear();
			// Remove unwanted unadjusted entries
			sql = "select distinct p1.regClassID"
					+ " from sourceUseTypePhysicsMapping p1"
					+ " left outer join sourceUseTypePhysicsMapping p2 on ("
					+ " 	p1.regClassID = p2.regClassID"
					+ " 	and p2.opModeIDOffset = 0"
					+ " )"
					+ " where p2.regClassID is null and p1.regClassID > 0";
			query.open(db,sql);
			/*
			while(query.rs.next()) {
				int regClassID = query.rs.getInt(1);
				for(int ti=0;ti<tableNames.length;ti++) {
					String tableName = tableNames[ti];
					sql = "delete from " + tableName
							+ " where opModeID >= 0 and opModeID < 100"
							+ " and polProcessID = " + polProcessID
							+ " and sourceBinID in ("
							+ " 	select sourceBinID"
							+ " 	from sourceBin"
							+ " 	where regClassID = " + regClassID
							+ " )";
					updateStatements.add(sql);
				}
			}
			*/
			String regClassIDsCSV = "";
			while(query.rs.next()) {
				int regClassID = query.rs.getInt(1);
				if(regClassIDsCSV.length() > 0) {
					regClassIDsCSV += ",";
				}
				regClassIDsCSV += regClassID;
			}
			query.close();
			if(regClassIDsCSV.length() > 0) {
				for(int ti=0;ti<tableNames.length;ti++) {
					String tableName = tableNames[ti];
					sql = "delete from " + tableName
							+ " where opModeID >= 0 and opModeID < 100"
							+ " and polProcessID = " + polProcessID
							+ " and sourceBinID in ("
							+ " 	select sourceBinID"
							+ " 	from sourceBin"
							+ " 	where regClassID in (" + regClassIDsCSV + ")"
							+ " )";
					updateStatements.add(sql);
				}
			}
			for(Iterator<String> i=updateStatements.iterator();i.hasNext();) {
				sql = i.next();
				SQLRunner.executeSQL(db,sql);
				//Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables#2 "+sql);
			}
			updateStatements.clear();
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
									+ " and opModeID >= 0 + " + offset + " and opModeID < 100 + " + offset;
							SQLRunner.executeSQL(db,sql);
							//Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables#4 "+sql);
						}
					}
				}
			}
			for(int i=0;i<tableNames.length;i++) {
				sql = "analyze table " + tableNames[i];
				SQLRunner.executeSQL(db,sql);
				//Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables#3 "+sql);
			}
			sql = "analyze table opModePolProcAssoc";
			SQLRunner.executeSQL(db,sql);
		} catch(SQLException e) {
			Logger.logError(e,"Unable to update emission rate tables for source type physics");
			throw e;
		} finally {
			query.onFinally();
		}
		Logger.log(LogMessageCategory.DEBUG,"SourceTypePhysics.updateEmissionRateTables done");
	}

	/**
	 * Make SQL statements to copy operating modes within a table.
	 * @param updateStatements list of statements to be appended
	 * @param tableName affected table
	 * @param opModeIDOffset offset used to create new operating modes
	 * @param polProcessID affected pollutant/process identifier
	 * @param sourceBinIDs comma-separated list of affected sourcebin identifiers
	**/
	private void copyOperatingModes(ArrayList<String> updateStatements,
			String tableName, int opModeIDOffset, int polProcessID, String sourceBinIDs) {
		if(opModeIDOffset <= 0) {
			return;
		}
		String sql = "";
		sql = "drop table if exists physics_" + tableName;
		updateStatements.add(sql);
		
		sql = "create table physics_" + tableName + " like " + tableName;
		updateStatements.add(sql);
		
		sql = "insert into physics_" + tableName
				+ " select * from " + tableName
				+ " where opModeID >= 0 and opModeID < 100"
				+ " and polProcessID=" + polProcessID
				+ " and sourceBinID in (" + sourceBinIDs + ")";
		updateStatements.add(sql);

		sql = "update physics_" + tableName + " set opModeID=opModeID+" + opModeIDOffset;
		updateStatements.add(sql);
		
		sql = "insert ignore into " + tableName + " select * from physics_" + tableName;
		updateStatements.add(sql);
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
						+ " where opModeID >= 0 and opModeID < 100"
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

	/**
	 * Check sourceBins for model year overlap issues.
	 * @param db database connection to use
	 * @param processID identifier of the affected emission process
	 * @throws SQLException if anything goes wrong
	**/
	void checkSourceBins(Connection db, int processID) throws SQLException {
		String alreadyKey = "checkSourceBins|" + processID;
		if(alreadyDoneFlags.contains(alreadyKey)) {
			return;
		}
		alreadyDoneFlags.add(alreadyKey);

		String[] setupStatements = {
			"create table if not exists sourceBinModelYearRange ("
					+ " 	sourceBinID bigint not null,"
					+ " 	polProcessID int not null,"
					+ " 	minModelYearID int not null,"
					+ " 	maxModelYearID int not null,"
					+ " 	howManyModelYears int not null,"
					+ " 	primary key (sourceBinID, polProcessID)"
					+ " )",

			"insert ignore into sourceBinModelYearRange(sourceBinID,polProcessID,minModelYearID,maxModelYearID,howManyModelYears)"
					+ " select sbd.sourceBinID, sbd.polProcessID, min(stmy.modelYearID) as minModelYearID, max(stmy.modelYearID) as maxModelYearID, count(distinct stmy.modelYearID) as howMany"
					+ " from sourceTypePolProcess stpp"
					+ " inner join pollutantProcessAssoc ppa on ("
					+ " 	ppa.polProcessID=stpp.polProcessID"
					+ " 	and ppa.processID=" + processID
					+ " 	and stpp.isMYGroupReqd='Y')"
					+ " inner join sourceBinDistribution sbd on ("
					+ " 	sbd.polProcessID=ppa.polProcessID)"
					+ " inner join sourceTypeModelYear stmy on ("
					+ " 	stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID"
					+ " 	and stmy.sourceTypeID=stpp.sourceTypeID)"
					+ " group by sbd.sourceBinID, sbd.polProcessID",

			// Get sbid, my, st, rc that are allowed.
			"drop table if exists allowedSourceBinMYSTRC",
			"create table allowedSourceBinMYSTRC ("
					+ " 	sourceBinID bigint not null,"
					+ " 	modelYearID smallint not null,"
					+ " 	sourceTypeID smallint not null,"
					+ " 	regClassID smallint not null,"
					+ " 	primary key (modelYearID, sourceTypeID, regClassID, sourceBinID)"
					+ " )",
			"insert into allowedSourceBinMYSTRC (sourceBinID, modelYearID, sourceTypeID, regClassID)"
					+ " select distinct sbd.sourceBinID, stmy.modelYearID, stpp.sourceTypeID, sb.regClassID"
					+ " from sourceTypePolProcess stpp"
					+ " inner join pollutantProcessAssoc ppa on ("
					+ " 	ppa.polProcessID=stpp.polProcessID"
					+ " 	and ppa.processID=" + processID
					+ " 	and stpp.isMYGroupReqd='Y')"
					+ " inner join sourceBinDistribution sbd on ("
					+ " 	sbd.polProcessID=ppa.polProcessID)"
					+ " inner join sourceTypeModelYear stmy on ("
					+ " 	stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID"
					+ " 	and stmy.sourceTypeID=stpp.sourceTypeID)"
					+ " inner join sourceBin sb on ("
					+ " 	sb.sourceBinID = sbd.sourceBinID)"
					+ " inner join sampleVehiclePopulation svp on ("
					+ " 	svp.sourceTypeModelYearID = stmy.sourceTypeModelYearID"
					+ " 	and (sb.regClassID = 0 or svp.regClassID = sb.regClassID)"
					+ " 	and svp.fuelTypeID = sb.fuelTypeID)",
			// Get my, st, rc used by physics.
			"drop table if exists usedMYSTRC",
			"create table usedMYSTRC ("
					+ " 	modelYearID smallint not null,"
					+ " 	sourceTypeID smallint not null,"
					+ " 	regClassID smallint not null,"
					+ " 	primary key (modelYearID, sourceTypeID, regClassID)"
					+ " )",
			"insert into usedMYSTRC (modelYearID, sourceTypeID, regClassID)"
					+ " select distinct stmy.modelYearID, stpm.realSourceTypeID as sourceTypeID, stpm.regClassID"
					+ " from sourceUseTypePhysicsMapping stpm"
					+ " inner join sourceTypeModelYear stmy on ("
					+ " 	stmy.sourceTypeID=stpm.realSourceTypeID"
					+ " 	and stmy.modelYearID >= stpm.beginModelYearID"
					+ " 	and stmy.modelYearID <= stpm.endModelYearID)"
					+ " inner join sampleVehiclePopulation svp on ("
					+ " 	svp.sourceTypeModelYearID = stmy.sourceTypeModelYearID"
					+ " 	and svp.regClassID = stpm.regClassID)",
			// Find missing entries
			"drop table if exists missingMYSTRC",
			"create table missingMYSTRC ("
					+ " 	modelYearID smallint not null,"
					+ " 	sourceTypeID smallint not null,"
					+ " 	regClassID smallint not null,"
					+ " 	primary key (modelYearID, sourceTypeID, regClassID)"
					+ " )",
			"insert into missingMYSTRC (modelYearID, sourceTypeID, regClassID)"
					+ " select distinct a.modelYearID, a.sourceTypeID, a.regClassID"
					+ " from allowedSourceBinMYSTRC a"
					+ " left outer join usedMYSTRC u using (modelYearID, sourceTypeID, regClassID)"
					+ " where u.modelYearID is null",
			// Report using a range
			"drop table if exists missingMYSTRC_Ranges",
			"create table missingMYSTRC_Ranges ("
					+ " 	modelYearID smallint not null,"
					+ " 	sourceTypeID smallint not null,"
					+ " 	regClassID smallint not null,"
					+ " 	isUpperLimit smallint not null,"
					+ " 	minModelYearID smallint null,"
					+ " 	maxModelYearID smallint null,"
					+ " 	primary key (modelYearID, sourceTypeID, regClassID, isUpperLimit)"
					+ " )",
			"insert into missingMYSTRC_Ranges (sourceTypeID, regClassID, modelYearID, isUpperLimit)"
					+ " select b.sourceTypeID, b.regClassID, b.modelYearID,"
					+ " 	case when bp1.modelYearID is null then 1 else 0 end as isUpperLimit"
					+ " from missingMYSTRC b"
					+ " left outer join missingMYSTRC bp1 on ("
					+ " 	b.modelYearID + 1 = bp1.modelYearID"
					+ " 	and b.sourceTypeID = bp1.sourceTypeID"
					+ " 	and b.regClassID = bp1.regClassID"
					+ " )"
					+ " left outer join missingMYSTRC bm1 on ("
					+ " 	b.modelYearID - 1 = bm1.modelYearID"
					+ " 	and b.sourceTypeID = bm1.sourceTypeID"
					+ " 	and b.regClassID = bm1.regClassID"
					+ " )"
					+ " where bp1.modelYearID is null or bm1.modelYearID is null",
			"drop table if exists missingMYSTRC_Ranges2",
			"create table missingMYSTRC_Ranges2 like missingMYSTRC_Ranges",
			"insert into missingMYSTRC_Ranges2 select * from missingMYSTRC_Ranges",
			"update missingMYSTRC_Ranges set minModelYearID=modelYearID where isUpperLimit = 0",
			"update missingMYSTRC_Ranges set maxModelYearID=("
					+ " 	select min(modelYearID)"
					+ " 	from missingMYSTRC_Ranges2 r"
					+ " 	where r.isUpperLimit=1"
					+ " 	and r.modelYearID >= missingMYSTRC_Ranges.modelYearID"
					+ " )"
					+ " where isUpperLimit=0",
			"drop table if exists missingMYSTRC_Ranges2"
		};
		
		String[] queryStatements = {
				/*
				"select distinct case when howMany > 1 then"
				+ " 		concat('Sourcebin ',sourceBinID,' is affected by ',howMany,' sourceUseTypePhysics entries.')"
				+ " 	else ''"
				+ " 	end as errorMessage"
				+ " from ("
					+ " select sourceTypeID, sourceBinID, "
					+ " 	count(distinct opModeIDOffset) as howMany, "
					+ " 	min(minModelYearID) as minModelYearID, "
					+ " 	max(maxModelYearID) as maxModelYearID,"
					+ " 	min(binMinModelYearID) as binMinModelYearID,"
					+ " 	max(binMaxModelYearID) as binMaxModelYearID,"
					+ " 	max(howManyModelYears) as howManyModelYears,"
					+ " 	max(binHowManyModelYears) as binHowManyModelYears"
					+ " from ("
						+ " select stpp.sourceTypeID, sbd.sourceBinID, sbd.polProcessID, stpm.opModeIDOffset, "
						+ " 	min(stmy.modelYearID) as minModelYearID, max(stmy.modelYearID) as maxModelYearID, "
						+ " 	count(distinct stmy.modelYearID) as howManyModelYears,"
						+ " 	sbmyr.minModelYearID as binMinModelYearID,"
						+ " 	sbmyr.maxModelYearID as binMaxModelYearID,"
						+ " 	sbmyr.howManyModelYears as binHowManyModelYears"
						+ " from sourceTypePolProcess stpp"
						+ " inner join sourceUseTypePhysicsMapping stpm on ("
						+ " 	stpm.realSourceTypeID=stpp.sourceTypeID"
						+ " 	and stpm.opModeIDOffset>=0)"
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
						+ " inner join sourceBin sb on ("
						+ " 	sb.sourceBinID = sbd.sourceBinID"
						+ " 	and (sb.regClassID = 0 or sb.regClassID = stpm.regClassID or stpm.regClassID = 0) )"
						+ " inner join sourceBinModelYearRange sbmyr on ("
						+ " 	sbmyr.sourceBinID = sbd.sourceBinID"
						+ " 	and sbmyr.polProcessID = sbd.polProcessID)"
						+ " group by stpp.sourceTypeID, sbd.sourceBinID, sbd.polProcessID, stpm.opModeIDOffset"
					+ " ) T"
					+ " group by sourceTypeID, sourceBinID"
				+ " ) T2"
				+ " where howMany > 1"
				+ " order by sourceBinID",
				*/

				"select concat('Sourcetype ',sourceTypeID,' regClass ',regClassID,' has no sourceUseTypePhysics coverage for model years ',minModelYearID,'-',maxModelYearID) as errorMessage"
						+ " from missingMYSTRC_Ranges"
						+ " where isUpperLimit=0 and minModelYearID is not null and maxModelYearID is not null"
						+ " order by sourceTypeID, regClassID, minModelYearID"
		};

		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			for(int i=0;i<setupStatements.length;i++) {
				sql = setupStatements[i];
				if(sql != null && sql.length() > 0) {
					SQLRunner.executeSQL(db,sql);
				}
			}

			for(int i=0;i<queryStatements.length;i++) {
				sql = queryStatements[i];
				query.open(db,sql);
				while(query.rs.next()) {
					String message = StringUtilities.safeGetString(query.rs.getString(1));
					if(message.length() <= 0) {
						continue;
					}
					if(alreadyDoneMessages.contains(message)) {
						continue;
					}
					alreadyDoneMessages.add(message);
					Logger.log(LogMessageCategory.ERROR,"ERROR: " + message);
				}
				query.close();
			}
		} catch(SQLException e) {
			Logger.logError(e,"Unable to check sourcebins using: " + sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}
	
	// This code is called becuase user-provided operating mode distributions (at project scale) do not have the same
	// offsets as other input options, which causes no output. Therefore we need to offset them using sourceusetypephysicsmapping
	// as the reference for the opModeOffsets. This only applies for roadTypes 2-5
	public void offsetUserInputOpModeIDs(Connection db) throws SQLException {
		ArrayList<SourceTypeOpMode> gottenOpModeUpdates = new ArrayList<SourceTypeOpMode>();
		gottenOpModeUpdates = getOpModeUpdates(db);
		
		
		
		// Step 1: Create temporary table to hold the offset opModeIDs
		String[] setupStatements = {
			"drop table if exists tempOffsetOpModeDistribution",
			"create table if not exists tempOffsetOpModeDistribution like opmodedistribution", 
			"drop table if exists tempOpModeUpdates",
			"create table if not exists tempOpModeUpdates (" +
				"sourceTypeID int, " +
				"opModeID int, " +
				"newOpModeID int, " +
				"beginModelYearID int, " +
				"endModelYearID int, " +
				"UNIQUE KEY(sourceTypeID,opModeID,newOpModeID,beginModelYearID,endModelYearID), KEY(sourceTypeID,opModeID,newOpModeID,beginModelYearID,endModelYearID))"
		};
		for(String sql : setupStatements) {
			SQLRunner.executeSQL(db,sql);
		}
		
		// Step 2: Put the corect offset OpModeIDs in the temporary table using the non-offset table
		for (SourceTypeOpMode s : gottenOpModeUpdates) {
			String sql = "INSERT INTO tempOpModeUpdates VALUES (" + 
				s.sourceTypeID + ", " + s.opModeID + ", " + s.newOpModeID + ", " + s.beginModelYearID + ", " + s.endModelYearID + ")";
			SQLRunner.executeSQL(db,sql);
		}
		// join the update table with the existing op mode distribution table
		String sql = "insert into tempoffsetopmodedistribution " +
			"select tomu.sourceTypeID, omd.hourDayID, omd.linkID, omd.polProcessID, tomu.newopModeID, omd.opModeFraction, omd.opModeFractionCV, omd.isUserInput " +
			"FROM tempopmodeupdates tomu " +
			"LEFT JOIN opmodedistribution omd " +
			"USING (sourceTypeID,opModeID) " +
			"WHERE opModeFraction IS NOT NULL and omd.opModeID < 100 and linkID > 0";
		SQLRunner.executeSQL(db,sql);
		
		// Step 3: delete relevant rows from old table without the offset IDs
		sql = "delete from opmodedistribution where opModeID < 100 and linkID > 0";
		SQLRunner.executeSQL(db,sql);
		
		// Step 4: put all the data from the temporary table into the existing table
		sql = "insert into opmodedistribution select * from tempoffsetopmodedistribution";
		SQLRunner.executeSQL(db,sql);
		
		// Step 5: drop the temporary tables
		sql = "drop table if exists tempOffsetOpModeDistribution;";
		SQLRunner.executeSQL(db,sql);
		sql = "drop table if exists tempOpModeUpdates;";
		SQLRunner.executeSQL(db,sql);
	}
}
