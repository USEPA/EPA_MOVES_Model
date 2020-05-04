/**
 * Project: EpaMOVES
 * Package: gov.epa.otaq.moves.worker.framework.NonroadPostProcessor.java
 * Version: Mar 22, 2012
 */
package gov.epa.otaq.moves.worker.framework;

import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.master.nonroad.MonthlyFractions;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

/**
 * @author Jizhen
 * 
 */
public class NonroadPostProcessor {

	/**
	 * Aggregates and overwrites the specified NONROAD table file. Runs sql to
	 * reduce the number of necessary rows in the resulting table file:
	 * <ul compact>
	 * <li>TSV (tab separated file) format.
	 * <li>No header row.
	 * <li>Columns:
	 * <ul compact>
	 * <li>Cnty, SubR, SCC, HP, Population, THC-Exhaust, CO-Exhaust,
	 * NOx-Exhaust, CO2-Exhaust</li>
	 * <li>SO2-Exhaust, PM-Exhaust, Crankcase, Hot-Soaks, Diurnal, Displacement,
	 * Spillage</li>
	 * <li>RunLoss, TankPerm, HosePerm, FuelCons., Activity, LF, HPAvg,
	 * FracRetro, UnitsRetro</li>
	 * </ul>
	 * </ul>
	 * The ORDER BY of the results will be decided at a later time so as to be
	 * most useful to the algorithm eventually used on the NMIM master.<br>
	 * 
	 * @param bmyFile
	 *            the nonRoadMBX file to read.
	 * @param bmvFile
	 *            the nonRoadMBX file to read.
	 * @param seasonsFile
	 *            the nonRoadMBX file to write.
	 * @param stateID
	 *            the current stateid being run.
	 * @param month
	 *            the current month being run.
	 * @param monthlyFractionFile
	 *            temporary file to write monthly fractions to.
	 * @param outFile
	 *            the nonRoadMBX file to write.
	 * @throws Exception
	 *             IOExceptions or SQLException for any io/sql functions.
	 **/
	public void aggregateNonRoad(//
			Connection db, //
			File bmyFile, //
			File bmvFile, //
			String nonRoadRetrofitFileCanonicalPath, // Retrofit - do not
														// support
														// now - 2012-03-23
			boolean shouldKeepRetrofitTemporaryData, //
			String activityFractionFileCanonicalPath,// Fleet - do not support
														// now - 2012-03-23
			File monthlyFractionFile, // Fleet - do not support now - 2012-03-23
			File seasonsFile, // Fleet - not same as SEASON.DAT - do not support
								// now - 2012-03-23
			int stateID, int month,//
			File outFile //
	) throws Exception {
		if (db == null) {
			Logger.log(LogMessageCategory.DEBUG,
					"NONROAD worker database is null");
		}
		// swap out the backslashes for forward slashes
		String bmyFileCanonicalPath = bmyFile.getCanonicalPath().replace('\\',
				'/');
		Logger.log(LogMessageCategory.DEBUG, "NONROAD bmyFile = "
				+ bmyFileCanonicalPath);

		String bmvFileCanonicalPath = bmvFile.getCanonicalPath().replace('\\',
				'/');
		Logger.log(LogMessageCategory.DEBUG, "NONROAD bmvFile = "
				+ bmvFileCanonicalPath);
		String seasonsFileCanonicalPath = seasonsFile.getCanonicalPath()
				.replace('\\', '/');
		Logger.log(LogMessageCategory.DEBUG, "NONROAD seasonsFile = "
				+ seasonsFileCanonicalPath);
		String outFileCanonicalPath = outFile.getCanonicalPath().replace('\\',
				'/');
		Logger.log(LogMessageCategory.DEBUG, "NONROAD outFile = "
				+ outFileCanonicalPath);
		String monthlyFractionFileCanonicalPath = monthlyFractionFile
				.getCanonicalPath().replace('\\', '/');

		String sql;

		// if they exist, drop any of the import tables
		sql = "DROP TABLE IF EXISTS BmyImport";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS BmvImport";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS BmyData";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS BmyExport";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS NonRoadRetrofitTemp";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS NonRoadRetrofit";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS ActivityFraction";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS ActivityFraction2";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS ActivityAdjustment";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS ActivityAdjustmentCombined";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS MonthlyFraction";
		SQLRunner.executeSQL(db, sql);
		sql = "DROP TABLE IF EXISTS NROutTemp";
		SQLRunner.executeSQL(db, sql);

		// Create the BMYExport table
		sql = "CREATE TABLE IF NOT EXISTS BMYExport (" + " Cnty CHAR(5),"
				+ " SubR  CHAR(5)," + " SCC  CHAR(10)," + " HP  SMALLINT,"
				+ " TechType  CHAR(10)," + " MYr  SMALLINT,"
				+ " Population  DOUBLE," + " THCExhaust  DOUBLE NULL,"
				+ " COExhaust  DOUBLE NULL," + " NOxExhaust  DOUBLE NULL,"
				+ " CO2Exhaust  DOUBLE NULL," + " SO2Exhaust  DOUBLE NULL,"
				+ " PMExhaust  DOUBLE NULL," + " Crankcase  DOUBLE NULL,"
				+ " HotSoaks  DOUBLE NULL," + " Diurnal  DOUBLE NULL,"
				+ " Displacement  DOUBLE NULL," + " Spillage  DOUBLE NULL,"
				+ " RunLoss  DOUBLE NULL," + " TankPerm  DOUBLE NULL,"
				+ " HosePerm  DOUBLE NULL," + " FuelCons  DOUBLE NULL,"
				+ " Activity  DOUBLE," + " LF  DOUBLE NULL,"
				+ " HPAvg  DOUBLE NULL," + " FracRetro  DOUBLE NULL,"
				+ " UnitsRetro  DOUBLE NULL," + " EmissionType TINYINT,"
				+ " INDEX IX_SCC (SCC)," + " INDEX IX_HP (HP),"
				+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr),"
				+ " INDEX IX_EmissionType (EmissionType))";
		SQLRunner.executeSQL(db, sql);

		// Load the exhaust emissions import table
		sql = "CREATE TABLE IF NOT EXISTS BmyImport (" + " Cnty CHAR(5),"
				+ " SubR CHAR(5)," + " SCC CHAR(10)," + " HP SMALLINT,"
				+ " TechType CHAR(10)," + " MYr SMALLINT,"
				+ " Population DOUBLE," + " THCExhaust DOUBLE NULL,"
				+ " COExhaust DOUBLE NULL," + " NOxExhaust DOUBLE NULL,"
				+ " CO2Exhaust DOUBLE NULL," + " SO2Exhaust DOUBLE NULL,"
				+ " PMExhaust DOUBLE NULL," + " Crankcase DOUBLE NULL,"
				+ " FuelCons DOUBLE NULL," + " Activity DOUBLE,"
				+ " LF DOUBLE NULL," + " HPAvg DOUBLE NULL,"
				+ " FracRetro DOUBLE NULL," + " UnitsRetro DOUBLE NULL,"
				+ " INDEX IX_SCC (SCC)," + " INDEX IX_HP (HP),"
				+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr))";
		SQLRunner.executeSQL(db, sql);
		sql = "LOAD DATA INFILE '" + bmyFileCanonicalPath
				+ "' INTO TABLE BmyImport"
				+ " FIELDS TERMINATED BY ',' IGNORE 1 LINES";
		SQLRunner.executeSQL(db, sql);

		// Load the evaporative emissions import table
		sql = "CREATE TABLE IF NOT EXISTS BmvImport (" + " Cnty CHAR(5),"
				+ " SubR  CHAR(5)," + " SCC  CHAR(10)," + " HP  SMALLINT,"
				+ " TechType  CHAR(10)," + " MYr  SMALLINT,"
				+ " Population  DOUBLE," + " HotSoaks  DOUBLE NULL,"
				+ " Diurnal  DOUBLE NULL," + " Displacement  DOUBLE NULL,"
				+ " Spillage  DOUBLE NULL," + " RunLoss  DOUBLE NULL,"
				+ " TankPerm  DOUBLE NULL," + " HosePerm  DOUBLE NULL,"
				+ " FuelCons  DOUBLE NULL," + " Activity  DOUBLE,"
				+ " INDEX IX_SCC (SCC)," + " INDEX IX_HP (HP),"
				+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr))";
		SQLRunner.executeSQL(db, sql);

		sql = "LOAD DATA INFILE '" + bmvFileCanonicalPath
				+ "' INTO TABLE BmvImport"
				+ " FIELDS TERMINATED BY ',' IGNORE 1 LINES";
		SQLRunner.executeSQL(db, sql);

		// Create a single combined table for exhaust and evaporative emissions
		String destination = "BMYExport";
		if (nonRoadRetrofitFileCanonicalPath != null
				|| activityFractionFileCanonicalPath != null) {
			if (nonRoadRetrofitFileCanonicalPath != null) {
				destination = "BMYData";
			}
			sql = "CREATE TABLE IF NOT EXISTS BMYData (" + " Cnty CHAR(5),"
					+ " SubR  CHAR(5)," + " SCC  CHAR(10)," + " HP  SMALLINT,"
					+ " TechType  CHAR(10)," + " MYr  SMALLINT,"
					+ " Population  DOUBLE," + " THCExhaust  DOUBLE NULL,"
					+ " COExhaust  DOUBLE NULL," + " NOxExhaust  DOUBLE NULL,"
					+ " CO2Exhaust  DOUBLE NULL," + " SO2Exhaust  DOUBLE NULL,"
					+ " PMExhaust  DOUBLE NULL," + " Crankcase  DOUBLE NULL,"
					+ " HotSoaks  DOUBLE NULL," + " Diurnal  DOUBLE NULL,"
					+ " Displacement  DOUBLE NULL," + " Spillage  DOUBLE NULL,"
					+ " RunLoss  DOUBLE NULL," + " TankPerm  DOUBLE NULL,"
					+ " HosePerm  DOUBLE NULL," + " FuelCons  DOUBLE NULL,"
					+ " Activity  DOUBLE," + " LF  DOUBLE NULL,"
					+ " HPAvg  DOUBLE NULL," + " FracRetro  DOUBLE NULL,"
					+ " UnitsRetro  DOUBLE NULL," + " EmissionType TINYINT,"
					+ " INDEX IX_SCC (SCC)," + " INDEX IX_HP (HP),"
					+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr),"
					+ " INDEX IX_EmissionType (EmissionType))";
			SQLRunner.executeSQL(db, sql);
		}

		// Insert exhaust emissions
		sql = "INSERT INTO "
				+ destination
				+ " SELECT bmy.Cnty, bmy.SubR, bmy.SCC, bmy.HP, bmy.TechType,"
				+ " bmy.MYr, bmy.Population,"
				+ " bmy.THCExhaust, bmy.COExhaust, bmy.NOxExhaust,"
				+ " bmy.CO2Exhaust, bmy.SO2Exhaust, bmy.PMExhaust,"
				+ " bmy.Crankcase, NULL AS HotSoaks, NULL AS Diurnal,"
				+ " NULL AS Displacement, NULL AS Spillage, NULL AS RunLoss, NULL AS TankPerm,"
				+ " NULL AS HosePerm, bmy.FuelCons," + " bmy.Activity, bmy.LF,"
				+ " bmy.HPAvg, bmy.FracRetro, bmy.UnitsRetro, 1"
				+ " FROM BmyImport bmy"
				+ " LEFT JOIN BmvImport bmv ON bmv.Cnty = bmy.Cnty"
				+ " AND bmv.SubR = bmy.SubR" + " AND bmv.SCC = bmy.SCC"
				+ " AND bmv.HP = bmy.HP" + " AND bmv.TechType = bmy.TechType"
				+ " AND bmv.MYr = bmy.MYr" + " WHERE bmv.Population IS NULL";
		SQLRunner.executeSQL(db, sql);

		// Insert evaporative emissions
		sql = "INSERT INTO " + destination
				+ " SELECT bmv.Cnty, bmv.SubR, bmv.SCC, bmv.HP, bmv.TechType,"
				+ " bmv.MYr, bmv.Population,"
				+ " NULL AS THCExhaust, NULL AS COExhaust, NULL AS NOxExhaust,"
				+ " NULL AS CO2Exhaust, NULL AS SO2Exhaust, NULL AS PMExhaust,"
				+ " NULL AS Crankcase, bmv.HotSoaks, bmv.Diurnal,"
				+ " bmv.Displacement, bmv.Spillage, bmv.RunLoss, bmv.TankPerm,"
				+ " bmv.HosePerm, bmv.FuelCons," + " bmv.Activity, bmy.LF,"
				+ " NULL AS HPAvg, NULL AS FracRetro, NULL AS UnitsRetro, 2"
				+ " FROM BmyImport bmy"
				+ " RIGHT JOIN BmvImport bmv ON bmv.Cnty = bmy.Cnty"
				+ " AND bmv.SubR = bmy.SubR" + " AND bmv.SCC = bmy.SCC"
				+ " AND bmv.HP = bmy.HP" + " AND bmv.TechType = bmy.TechType"
				+ " AND bmv.MYr = bmy.MYr" + " WHERE bmy.Population IS NULL";
		SQLRunner.executeSQL(db, sql);

		// Apply retrofit factors.
		if (nonRoadRetrofitFileCanonicalPath != null) {
			// Import the retrofit factors
			sql = "CREATE TABLE IF NOT EXISTS NonRoadRetrofitTemp ("
					+ " SCC  CHAR(10)," + " TechType  CHAR(10),"
					+ " MYr  SMALLINT," + " HPMin  SMALLINT,"
					+ " HPMax  SMALLINT," + " THCFactor  DOUBLE,"
					+ " COFactor  DOUBLE," + " NOxFactor  DOUBLE,"
					+ " CO2Factor  DOUBLE," + " SO2Factor  DOUBLE,"
					+ " PMFactor  DOUBLE," + " INDEX IX_SCC (SCC),"
					+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr),"
					+ " INDEX IX_HPMin (HPMin)," + " INDEX IX_HPMax (HPMax))";
			SQLRunner.executeSQL(db, sql);

			sql = "LOAD DATA INFILE '" + nonRoadRetrofitFileCanonicalPath
					+ "' INTO TABLE NonRoadRetrofitTemp";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE TABLE IF NOT EXISTS NonRoadRetrofit ("
					+ " SCC  CHAR(10)," + " TechType  CHAR(10),"
					+ " MYr  SMALLINT," + " HPMin  SMALLINT,"
					+ " HPMax  SMALLINT," + " THCFactor  DOUBLE,"
					+ " COFactor  DOUBLE," + " NOxFactor  DOUBLE,"
					+ " CO2Factor  DOUBLE," + " SO2Factor  DOUBLE,"
					+ " PMFactor  DOUBLE," + " INDEX IX_SCC (SCC),"
					+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr),"
					+ " INDEX IX_HPMin (HPMin)," + " INDEX IX_HPMax (HPMax))";
			SQLRunner.executeSQL(db, sql);

			// Expand retrofit data to use real SCCs and Tech Types.
			sql = "INSERT INTO NonRoadRetrofit"
					+ " SELECT bmy.SCC, bmy.TechType, nr.MYr, nr.HPMin, nr.HPMax, nr.THCFactor,"
					+ " nr.COFactor, nr.NOxFactor, nr.CO2Factor, nr.SO2Factor, nr.PMFactor"
					+ " FROM BMYData bmy"
					+ " INNER JOIN NonRoadRetrofitTemp nr ON (nr.SCC = bmy.SCC"
					+ " OR (LEFT(nr.SCC,7) = LEFT(bmy.SCC,7) AND RIGHT(nr.SCC,3) = '000')"
					+ " OR (LEFT(nr.SCC,4) = LEFT(bmy.SCC,4) AND RIGHT(nr.SCC,6) = '000000'))"
					+ " AND (nr.TechType = bmy.TechType OR nr.TechType = 'ALL')"
					+ " AND nr.MYr = bmy.MYr AND bmy.HP > nr.HPMin AND bmy.HP <= nr.HPMax"
					+ " GROUP BY bmy.SCC, bmy.TechType, nr.MYr, nr.HPMin, nr.HPMax";
			SQLRunner.executeSQL(db, sql);

			// Do the retrofit calculations
			sql = "INSERT INTO BMYExport"
					+ " SELECT bmy.Cnty, bmy.SubR, bmy.SCC, bmy.HP, bmy.TechType,"
					+ " bmy.MYr, bmy.Population,"
					+ " COALESCE(bmy.THCExhaust*nr.THCFactor,bmy.THCExhaust),"
					+ " COALESCE(bmy.COExhaust*nr.COFactor,bmy.COExhaust),"
					+ " COALESCE(bmy.NOxExhaust*nr.NOxFactor,bmy.NOxExhaust),"
					+ " COALESCE(bmy.CO2Exhaust*nr.CO2Factor,bmy.CO2Exhaust),"
					+ " COALESCE(bmy.SO2Exhaust*nr.SO2Factor,bmy.SO2Exhaust),"
					+ " COALESCE(bmy.PMExhaust*nr.PMFactor,bmy.PMExhaust),"
					+ " bmy.Crankcase,"
					+ " bmy.HotSoaks, bmy.Diurnal, bmy.Displacement, bmy.Spillage,"
					+ " bmy.RunLoss, bmy.TankPerm, bmy.HosePerm,bmy.FuelCons,"
					+ " bmy.Activity, bmy.LF, bmy.HPAvg, bmy.FracRetro, bmy.UnitsRetro,"
					+ " bmy.EmissionType FROM BmyData bmy"
					+ " LEFT JOIN NonRoadRetrofit nr ON nr.scc = bmy.scc"
					+ " AND nr.HPMin < bmy.HP AND nr.HPMax >= bmy.HP"
					+ " AND nr.MYr = bmy.MYr AND nr.TechType = bmy.TechType";
			SQLRunner.executeSQL(db, sql);

			// Check for retrofit entries that do not have corresponding data
			// from NONROAD
			sql = "SELECT nr.scc, nr.techType, nr.MYr, nr.HPMin, nr.HPMax FROM NonRoadRetrofitTemp nr"
					+ " LEFT JOIN BMYData bmy ON (bmy.scc = nr.scc"
					+ " OR (RIGHT(nr.scc,3)='000' AND LEFT(bmy.scc,7) = LEFT(nr.scc,7))"
					+ " OR (RIGHT(nr.scc,6)='000000' AND LEFT(bmy.scc,4) = LEFT(nr.scc,4)))"
					+ " AND (bmy.TechType = nr.TechType OR nr.TechType = 'ALL')"
					+ " AND bmy.MYr = nr.MYr AND bmy.HP > nr.HPMin AND bmy.HP <= nr.HPMax"
					+ " WHERE bmy.scc IS NULL";

			PreparedStatement statement = null;
			ResultSet results = null;
			try {
				statement = db.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						Logger.log(
								LogMessageCategory.ERROR,
								"NMIM cannot determine retrofit effects for SCC, '"
										+ results.getString(1)
										+ "', TechType, '"
										+ results.getString(2)
										+ "', Model Year, "
										+ results.getInt(3)
										+ ", HP >"
										+ results.getInt(4)
										+ ", and HP <= "
										+ results.getInt(5)
										+ " because there is no data from Non-road");
					}
				}
			} catch (Exception e) {
				Logger.logError(e, "Could not check for invalid model year.");
			} finally {
				if (results != null) {
					try {
						results.close();
					} catch (Exception e) {
						// Nothing to do here
					}
				}
				if (statement != null) {
					try {
						statement.close();
					} catch (Exception e) {
						// Nothing to do here
					}
				}
			}

			// Clean out BMY data for Fleet Adjustment calculations
			sql = "TRUNCATE BMYData";
			SQLRunner.executeSQL(db, sql);
		}

		// Calculate the fleet adjustment effects.
		if (activityFractionFileCanonicalPath != null) {
			// Load the activity fraction data
			sql = "CREATE TABLE IF NOT EXISTS ActivityFraction ("
					+ " SCC  CHAR(10)," + " TechType  CHAR(10),"
					+ " MYr  SMALLINT," + " HPBin  SMALLINT,"
					+ " Population  DOUBLE," + " Activity  DOUBLE,"
					+ " MonthlyFraction DOUBLE," + " INDEX IX_SCC (SCC),"
					+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr),"
					+ " INDEX IX_HPMax (HPBin))";
			SQLRunner.executeSQL(db, sql);

			sql = "LOAD DATA INFILE '" + activityFractionFileCanonicalPath
					+ "' INTO TABLE ActivityFraction";
			SQLRunner.executeSQL(db, sql);

			// Force non-diesel TechTypes to ALL
			sql = "UPDATE ActivityFraction" + " SET TechType = 'ALL'"
					+ " WHERE TechType NOT LIKE 'T%'";
			SQLRunner.executeSQL(db, sql);

			// Get the default monthly fractions
			sql = "CREATE TABLE IF NOT EXISTS MonthlyFraction ("
					+ " SCC  CHAR(10)," + " MonthlyFraction  DOUBLE,"
					+ " INDEX IX_SCC (SCC))";
			SQLRunner.executeSQL(db, sql);

			try {
				MonthlyFractions mf = new MonthlyFractions();
				mf.load(seasonsFileCanonicalPath,
						monthlyFractionFileCanonicalPath, stateID, month);
			} catch (IOException e) {
				// Nothing to do here
			}

			sql = "LOAD DATA INFILE '" + monthlyFractionFileCanonicalPath
					+ "' INTO TABLE MonthlyFraction";
			SQLRunner.executeSQL(db, sql);

			// Apply the default monthly fractions where needed
			sql = "CREATE TABLE IF NOT EXISTS ActivityFraction2 ("
					+ " SCC  CHAR(10)," + " TechType  CHAR(10),"
					+ " MYr  SMALLINT," + " HPBin  SMALLINT,"
					+ " Population  DOUBLE," + " Activity  DOUBLE,"
					+ " MonthlyFraction DOUBLE," + " MonthlyAdjustment DOUBLE,"
					+ " INDEX IX_SCC (SCC)," + " INDEX IX_TechType (TechType),"
					+ " INDEX IX_MYr (MYr)," + " INDEX IX_HPMax (HPBin))";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO ActivityFraction2"
					+ " SELECT"
					+ " af.scc,"
					+ " af.TechType,"
					+ " af.MYr,"
					+ " af.HPBin,"
					+ " af.Population,"
					+ " af.Activity,"
					+ " IF(af.MonthlyFraction>=0,af.MonthlyFraction,"
					+ " COALESCE(mf.MonthlyFraction,mf7.MonthlyFraction,mf4.MonthlyFraction)),"
					+ " IF(af.MonthlyFraction<0,1.0,af.MonthlyFraction/"
					+ " COALESCE(mf.MonthlyFraction,mf7.MonthlyFraction,mf4.MonthlyFraction))"
					+ " FROM ActivityFraction af"
					+ " LEFT JOIN MonthlyFraction mf ON mf.SCC = af.SCC"
					+ " LEFT JOIN MonthlyFraction mf7 ON mf7.SCC = CONCAT(LEFT(af.SCC,7),'000')"
					+ " LEFT JOIN MonthlyFraction mf4 ON mf4.SCC = CONCAT(LEFT(af.SCC,4),'000000')";
			SQLRunner.executeSQL(db, sql);

			// Sum the "ALL" Tech Types into ALL exhaust and ALL evaporative
			// emissions.
			sql = "INSERT INTO BMYData"
					+ " SELECT bmy.Cnty, bmy.SubR, bmy.SCC, bmy.HP,"
					+ " COALESCE(af2.TechType,bmy.TechType) AS TechType,"
					+ " bmy.MYr, SUM(bmy.Population), SUM(bmy.THCExhaust), SUM(bmy.COExhaust),"
					+ " SUM(bmy.NOxExhaust), SUM(bmy.CO2Exhaust), SUM(bmy.SO2Exhaust),"
					+ " SUM(bmy.PMExhaust), SUM(bmy.Crankcase), SUM(bmy.HotSoaks),"
					+ " SUM(bmy.Diurnal), SUM(bmy.Displacement), SUM(bmy.Spillage),"
					+ " SUM(bmy.RunLoss), SUM(bmy.TankPerm), SUM(bmy.HosePerm), SUM(bmy.FuelCons),"
					+ " SUM(bmy.Activity), AVG(bmy.LF), AVG(bmy.HPAvg), AVG(bmy.FracRetro),"
					+ " SUM(bmy.UnitsRetro), bmy.EmissionType"
					+ " FROM BMYExport bmy"
					+ " LEFT JOIN ActivityFraction2 af2 ON"
					+ " af2.SCC = bmy.scc AND"
					+ " (af2.TechType=bmy.TechType OR af2.TechType='ALL') AND"
					+ " af2.MYr = bmy.MYr AND"
					+ " af2.HPBin = bmy.HP"
					+ " GROUP BY bmy.SCC, bmy.HP, bmy.MYr, TechType, bmy.EmissionType";
			SQLRunner.executeSQL(db, sql);

			sql = "TRUNCATE BMYExport";
			SQLRunner.executeSQL(db, sql);

			// Calculate the adjustments for fleet activity. Note that both
			// tables now
			// have a techtype of all.
			sql = "CREATE TABLE IF NOT EXISTS ActivityAdjustment ("
					+ " SCC  CHAR(10)," + " TechType  CHAR(10),"
					+ " MYr  SMALLINT," + " HPBin  SMALLINT,"
					+ " Population  DOUBLE," + " Activity  DOUBLE,"
					+ " MonthlyAdjustment DOUBLE,"
					+ " PopulationAdjustment DOUBLE,"
					+ " ActivityAdjustment DOUBLE," + " INDEX IX_SCC (SCC),"
					+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr),"
					+ " INDEX IX_HPMax (HPBin))";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO ActivityAdjustment"
					+ " SELECT"
					+ " af2.scc,"
					+ " bmy.TechType,"
					+ " af2.MYr,"
					+ " af2.HPBin,"
					+ " af2.Population,"
					+ " IF(af2.Activity>=0,af2.Activity*af2.MonthlyFraction*"
					+ " af2.Population,bmy.Activity),"
					+ " af2.MonthlyAdjustment,"
					+ " IF(bmy.Population=0,0,af2.Population/bmy.Population),"
					+ " IF(af2.Activity>=0,"
					+ " IF(bmy.Activity=0 || af2.MonthlyAdjustment=0,0,af2.Activity*af2.MonthlyFraction*"
					+ " bmy.Population/(bmy.Activity*af2.MonthlyAdjustment)),1.0)"
					+ " FROM BmyData bmy"
					+ " INNER JOIN ActivityFraction2 af2 ON"
					+ " af2.SCC = bmy.scc AND"
					+ " af2.TechType = bmy.TechType AND"
					+ " af2.MYr = bmy.MYr AND" + " af2.HPBin = bmy.HP"
					+ " GROUP BY bmy.SCC, bmy.HP, bmy.TechType,	bmy.MYr";
			SQLRunner.executeSQL(db, sql);

			sql = "CREATE TABLE IF NOT EXISTS ActivityAdjustmentCombined ("
					+ " SCC  CHAR(10)," + " TechType  CHAR(10),"
					+ " MYr  SMALLINT," + " HPBin  SMALLINT,"
					+ " Population  DOUBLE," + " Activity  DOUBLE,"
					+ " PopulationAdjustment DOUBLE,"
					+ " ActivityAdjustment DOUBLE," + " INDEX IX_SCC (SCC),"
					+ " INDEX IX_TechType (TechType)," + " INDEX IX_MYr (MYr),"
					+ " INDEX IX_HPMax (HPBin))";
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO ActivityAdjustmentCombined"
					+ " SELECT"
					+ " aa.scc,"
					+ " aa.TechType,"
					+ " aa.MYr,"
					+ " aa.HPBin,"
					+ " aa.Population,"
					+ " aa.Activity,"
					+ " aa.PopulationAdjustment,"
					+ " aa.MonthlyAdjustment*aa.PopulationAdjustment*aa.ActivityAdjustment"
					+ " FROM ActivityAdjustment aa";
			SQLRunner.executeSQL(db, sql);

			// Apply the adjustments
			sql = "INSERT INTO BMYExport"
					+ " SELECT bmy.Cnty, bmy.SubR, bmy.SCC, bmy.HP, bmy.TechType,"
					+ " bmy.MYr, COALESCE(aac.Population,bmy.Population), "
					+ " COALESCE(aac.ActivityAdjustment*bmy.THCExhaust,bmy.THCExhaust),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.COExhaust,bmy.COExhaust),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.NOxExhaust,bmy.NOxExhaust),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.CO2Exhaust,bmy.CO2Exhaust),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.SO2Exhaust,bmy.SO2Exhaust),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.PMExhaust,bmy.PMExhaust),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.Crankcase,bmy.Crankcase),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.HotSoaks,bmy.HotSoaks),"
					+ " COALESCE(aac.PopulationAdjustment*bmy.Diurnal,bmy.Diurnal),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.Displacement, bmy.Displacement),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.Spillage,bmy.Spillage),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.RunLoss, bmy.RunLoss),"
					+ " COALESCE(aac.PopulationAdjustment*bmy.TankPerm, bmy.TankPerm),"
					+ " COALESCE(aac.PopulationAdjustment*bmy.HosePerm, bmy.HosePerm),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.FuelCons,bmy.FuelCons),"
					+ " COALESCE(aac.Activity,bmy.Activity),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.LF, bmy.LF),"
					+ " COALESCE(aac.ActivityAdjustment*bmy.HPAvg,bmy.HPAvg),"
					+ "	bmy.FracRetro, bmy.UnitsRetro, bmy.EmissionType"
					+ " FROM BmyData bmy"
					+ " INNER JOIN ActivityAdjustmentCombined aac ON aac.SCC = bmy.SCC AND"
					+ " aac.TechType = bmy.TechType AND aac.MYr = bmy.MYr AND"
					+ " aac.HPBin = bmy.HP";
			SQLRunner.executeSQL(db, sql);

			// Check for changes from 0 monthly fractions to something else.
			sql = "SELECT af.scc, af.TechType, af.MYr, af.HPBin"
					+ " FROM ActivityFraction af"
					+ " LEFT JOIN MonthlyFraction mf ON mf.SCC = af.SCC"
					+ " LEFT JOIN MonthlyFraction mf7 ON mf7.SCC = CONCAT(LEFT(af.SCC,7),'000')"
					+ " LEFT JOIN MonthlyFraction mf4 ON mf4.SCC = CONCAT(LEFT(af.SCC,4),'000000')"
					+ " WHERE COALESCE(mf.MonthlyFraction,mf7.MonthlyFraction,mf4.MonthlyFraction)=0"
					+ " AND af.MonthlyFraction>0";

			PreparedStatement statement = null;
			ResultSet results = null;
			try {
				statement = db.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					if (results.next()) {
						do {
							Logger.log(
									LogMessageCategory.ERROR,
									"NMIM was unable to calculate the requested"
											+ " fleet emissions for month "
											+ month
											+ " for SCC "
											+ results.getString(1)
											+ ", TechType "
											+ results.getString(2)
											+ ", Model Year "
											+ results.getInt(3)
											+ ", "
											+ results.getInt(4)
											+ " Hp, because the default monthly activity for that month is"
											+ " zero.");
						} while (results.next());
						Logger.log(
								LogMessageCategory.ERROR,
								"To allow NMIM to perform the desired calculation"
										+ " you will need to modify the default NONROAD season.dat file to"
										+ " have some non-zero activity allocation fraction for that SCC in"
										+ " that month.");
					}
				}
			} catch (Exception e) {
				Logger.logError(e,
						"Could not check for change in monthly value from 0 "
								+ " to a number.");
			} finally {
				if (results != null) {
					try {
						results.close();
					} catch (Exception e) {
						// Nothing to do here
					}
				}
				if (statement != null) {
					try {
						statement.close();
					} catch (Exception e) {
						// Nothing to do here
					}
				}
			}

			// Check for entries in fleet entries that do not have values
			// returned from NONROAD
			sql = "SELECT af.SCC, af.TechType, af.MYr, af.HPBin"
					+ " FROM ActivityFraction af LEFT JOIN BMYExport bmy ON"
					+ " bmy.SCC = af.SCC AND bmy.TechType = af.TechType"
					+ " AND bmy.HP = af.HPBin AND bmy.MYr = af.MYr"
					+ "	WHERE bmy.MYr IS NULL";

			statement = null;
			results = null;
			try {
				statement = db.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						Logger.log(
								LogMessageCategory.ERROR,
								"NMIM cannot determine fleet adjustments for SCC, '"
										+ results.getString(1)
										+ "', TechType, '"
										+ results.getString(2)
										+ "', Model Year, "
										+ results.getInt(3)
										+ ", and "
										+ results.getInt(4)
										+ " Hp because there is no data from Non-road");
					}
				}
			} catch (Exception e) {
				Logger.logError(e, "Could not check for invalid model year.");
			} finally {
				if (results != null) {
					try {
						results.close();
					} catch (Exception e) {
						// Nothing to do here
					}
				}
				if (statement != null) {
					try {
						statement.close();
					} catch (Exception e) {
						// Nothing to do here
					}
				}
			}
		}

		// pre-aggregate out the tech types and while at it the model years,
		// but leave in emissiontype for later coalescing join into nroutfile.
		sql = "CREATE TABLE IF NOT EXISTS NROutTemp (" + " Cnty CHAR(5),"
				+ " SubR  CHAR(5)," + " SCC  CHAR(10)," + " HP  SMALLINT,"
				+ " Population  DOUBLE," + " THCExhaust  DOUBLE NULL,"
				+ " COExhaust  DOUBLE NULL," + " NOxExhaust  DOUBLE NULL,"
				+ " CO2Exhaust  DOUBLE NULL," + " SO2Exhaust  DOUBLE NULL,"
				+ " PMExhaust  DOUBLE NULL," + " Crankcase  DOUBLE NULL,"
				+ " HotSoaks  DOUBLE NULL," + " Diurnal  DOUBLE NULL,"
				+ " Displacement  DOUBLE NULL," + " Spillage  DOUBLE NULL,"
				+ " RunLoss  DOUBLE NULL," + " TankPerm  DOUBLE NULL,"
				+ " HosePerm  DOUBLE NULL," + " FuelCons  DOUBLE NULL,"
				+ " Activity  DOUBLE," + " LF  DOUBLE NULL,"
				+ " HPAvg  DOUBLE NULL," + " FracRetro  DOUBLE NULL,"
				+ " UnitsRetro  DOUBLE NULL," + " EmissionType TINYINT,"
				+ " INDEX IX_SCC (SCC)," + " INDEX IX_HP (HP),"
				+ " INDEX IX_EmissionType (EmissionType))";
		SQLRunner.executeSQL(db, sql);

		sql = "INSERT INTO NROutTemp"
				+ " SELECT bmy.Cnty, bmy.SubR, bmy.SCC, bmy.HP,"
				+ " SUM(bmy.Population), SUM(bmy.THCExhaust),"
				+ " SUM(bmy.COExhaust), SUM(bmy.NOxExhaust),"
				+ " SUM(bmy.CO2Exhaust), SUM(bmy.SO2Exhaust),"
				+ " SUM(bmy.PMExhaust), SUM(bmy.Crankcase),"
				+ " SUM(bmy.HotSoaks), SUM(bmy.Diurnal),"
				+ " SUM(bmy.Displacement), SUM(bmy.Spillage),"
				+ " SUM(bmy.RunLoss), SUM(bmy.TankPerm),"
				+ " SUM(bmy.HosePerm), SUM(bmy.FuelCons),"
				+ " SUM(bmy.Activity), AVG(bmy.LF),"
				+ " AVG(bmy.HPAvg), SUM(bmy.FracRetro),"
				+ " SUM(bmy.UnitsRetro), bmy.EmissionType"
				+ " FROM BmyExport bmy"
				+ " GROUP BY bmy.Cnty, bmy.SubR, bmy.SCC, bmy.HP, bmy.EmissionType";
		SQLRunner.executeSQL(db, sql);

		// Create the aggregated output file.
		createNonRoadOutputFile(db, outFileCanonicalPath);

		if (!shouldKeepRetrofitTemporaryData) {
			sql = "DROP TABLE IF EXISTS BmyImport";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS BmvImport";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS BmyData";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS BmyExport";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS NonRoadRetrofitTemp";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS NonRoadRetrofit";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS ActivityFraction";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS ActivityFraction2";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS ActivityAdjustment";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS ActivityAdjustmentCombined";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS MonthlyFraction";
			SQLRunner.executeSQL(db, sql);
			sql = "DROP TABLE IF EXISTS NROutTemp";
			SQLRunner.executeSQL(db, sql);
		}

		// db.close();
	}

	/**
	 * Create the Non-Road Output File
	 * 
	 * @param db
	 *            database connection
	 * @param outputFileName
	 *            Output file name
	 **/
	// need to change

	public void createNonRoadOutputFile(Connection db, String outputFileName) {
		PreparedStatement statement = null;
		ResultSet results = null;
		PrintWriter outFileWriter = null;
		try {
			outFileWriter = new PrintWriter(
					new BufferedWriter(new OutputStreamWriter(
							new FileOutputStream(outputFileName))));
			outFileWriter
					.println(" , , ,    0, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,");
			outFileWriter.println("\" NMIM's NONROAD Emissions Model\"");
			outFileWriter.println(" ");
			outFileWriter.println(" ");
			outFileWriter.println(" ");
			outFileWriter.println(" ");
			outFileWriter.println(" ");
			outFileWriter.println(" ");
			outFileWriter.println(" ");
			outFileWriter
					.println("Cnty ,SubR ,SCC       ,HP   ,Population     ,THC-Exhaust    ,CO-Exhaust     ,NOx-Exhaust    ,CO2-Exhaust    ,SO2-Exhaust    ,PM-Exhaust     ,Crankcase      ,Hot-Soaks      ,Diurnal        ,Displacement   ,Spillage       ,RunLoss        ,TankPerm       ,HosePerm       ,FuelCons.      ,Activity       ,LF             ,HPAvg          ,FracRetro      ,UnitsRetro     ,");

			String sql = "SELECT tmp1.Cnty, tmp1.SubR, tmp1.SCC, tmp1.HP,"
					+ " SUM(tmp1.Population), COALESCE(SUM(tmp1.THCExhaust),0),"
					+ " COALESCE(SUM(tmp1.COExhaust),0), COALESCE(SUM(tmp1.NOxExhaust),0),"
					+ " COALESCE(SUM(tmp1.CO2Exhaust),0), COALESCE(SUM(tmp1.SO2Exhaust),0),"
					+ " COALESCE(SUM(tmp1.PMExhaust),0), COALESCE(SUM(tmp1.Crankcase),0),"
					+ " COALESCE(SUM(tmp2.HotSoaks),0), COALESCE(SUM(tmp2.Diurnal),0),"
					+ " COALESCE(SUM(tmp2.Displacement),0), COALESCE(SUM(tmp2.Spillage),0),"
					+ " COALESCE(SUM(tmp2.RunLoss),0), COALESCE(SUM(tmp2.TankPerm),0),"
					+ " COALESCE(SUM(tmp2.HosePerm),0), COALESCE(SUM(tmp1.FuelCons),0),"
					+ " COALESCE(SUM(tmp1.Activity),0), COALESCE(AVG(tmp1.LF),0),"
					+ " COALESCE(AVG(tmp1.HPAvg),0),COALESCE(SUM(tmp1.FracRetro),0),"
					+ " COALESCE(SUM(tmp1.UnitsRetro),0)"
					+ " FROM NROutTemp tmp1 LEFT JOIN NROutTemp tmp2 ON ("
					+ " tmp1.Cnty = tmp2.Cnty AND tmp1.Subr = tmp2.Subr AND"
					+ " tmp1.SCC = tmp2.SCC AND tmp1.HP = tmp2.HP AND"
					+ " tmp2.EmissionType = 2) WHERE tmp1.EmissionType = 1"
					+ " GROUP BY tmp1.Cnty, tmp1.SubR, tmp1.SCC, tmp1.HP, tmp1.emissiontype";

			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				while (results.next()) {
					outFileWriter.print(results.getString(1) + ",");
					outFileWriter.print(results.getString(2) + ",");
					outFileWriter.print(results.getString(3) + ",");
					outFileWriter.print(results.getInt(4) + ",");
					for (int i = 5; i < 26; i++) {
						outFileWriter.print(results.getDouble(i) + ",");
					}
					outFileWriter.println();
				}
			}
		} catch (Exception e) {
			Logger.logError(e, "Could not write output file.");
		} finally {
			if (outFileWriter != null) {
				try {
					outFileWriter.close();
				} catch (Exception e) {
					// Nothing to do here.
				}
			}
			if (results != null) {
				try {
					results.close();
				} catch (Exception e) {
					// Nothing to do here
				}
			}
			if (statement != null) {
				try {
					statement.close();
				} catch (Exception e) {
					// Nothing to do here
				}
			}
		}
	}
}
