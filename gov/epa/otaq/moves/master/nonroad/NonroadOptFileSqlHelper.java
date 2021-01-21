package gov.epa.otaq.moves.master.nonroad;

import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.utils.FormatUtil;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.TreeSet;

import org.apache.commons.lang.mutable.MutableDouble;

public class NonroadOptFileSqlHelper {

	// String stateAbbr = null; // Temporally Store the State Abbreviation.

	/**
	 * Default constructor Must create a new one by each opt file
	 **/
	public NonroadOptFileSqlHelper() {
	}

	/**
	 * Generates the "OPTIONS" packet in the OPT file. Assumes optFileWriter has
	 * been initialized.
	 * @return true on success
	 **/
	public boolean getOptionsParameters(
			// inputs
			int year, int monthID, int dayID, // weekday or weekend
			int stateID, int countyID,
			// add as needed
			Connection executionDB, NonroadOptfileOptionsPacket parameterObject) {

		double ETOHVolume = parameterObject.geteTOHVolume();
		double ETOHMktShare = parameterObject.geteTOHMktShare();
		String altitude = parameterObject.getAltitude();
		double averageTemperature = parameterObject.getAverageTemperature();
		double maximumTemperature = parameterObject.getMaximumTemperature();
		double minimumTemperature = parameterObject.getMinimumTemperature();
		double naturalGasSulfurPercent = parameterObject
				.getNaturalGasSulfurPercent();
		double marineDieselSulfurPercent = parameterObject
				.getMarineDieselSulfurPercent();
		double dieselSulfurPercent = parameterObject.getDieselSulfurPercent();
		double gasSulfurPercent = parameterObject.getGasSulfurPercent();
		double oxygenWeightPercent = parameterObject.getOxygenWeightPercent();
		double RVPmkt = parameterObject.getrVPmkt();
		// hard code for test
		RVPmkt = 8.0;
		oxygenWeightPercent = 2.44;
		gasSulfurPercent = 0.0339; // "Gas sulfur % :
		dieselSulfurPercent = 0.0351; // "Diesel sulfur %    : "
		marineDieselSulfurPercent = 0.0435; // "Marine Dsl sulf %  : "
		naturalGasSulfurPercent = 0.003; // "CNG/LPG sulfur %   : "
		minimumTemperature = 60; // "Minimum temper. (F): "
		maximumTemperature = 84; // "Maximum temper. (F): "
		averageTemperature = 75; // "Average temper. (F): "
		altitude = "L"; // "Altitude of region : "
		ETOHMktShare = 0.751; // "EtOH Blend % Mkt   : "
		ETOHVolume = 9.3; // "EtOH Vol %         : "

		boolean isOK = true;

		if (countyID < 1000 && countyID >= 0 && stateID < 100 && stateID >= 0) {
			countyID = countyID + stateID * 1000;
		}

		String sql = null;
		PreparedStatement statement = null;
		ResultSet results = null;

		try {

			int episodeYear = year;

			// func A
			sql = "select sum(ff.RVP*fs.marketshare) as RVPmkt, "
					+ " sum(ff.volToWtPercentOxy*fs.marketshare) as oxyMkt, "
					+ " sum(ff.sulfurLevel*fs.marketshare)/10000 as gasSulfur "
					+ " from nrfuelsupply fs"
					+ " inner join fuelformulation ff on fs.fuelformulationid = ff.fuelformulationid"
					+ " inner join nrfuelsubtype sub on ff.fuelsubtypeid = sub.fuelsubtypeid"
					+ " inner join year y on y.fuelYearID=fs.fuelYearID"
					+ " inner join regionCounty rc on (rc.regionID=fs.fuelRegionID and rc.regionCodeID=2 and rc.fuelYearID=y.fuelYearID)"
					+ " where rc.countyid = ? and fs.monthgroupid = ?"
					+ " and y.yearID = ? "
					+ " and sub.fueltypeid = 1";

			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, countyID);
			statement.setInt(2, monthID);
			statement.setInt(3, episodeYear);
			results = SQLRunner.executeQuery(statement, sql);
			
			// Note: Even if the results set contains no rows, this block will still run and RVPmkt and gasSulfurPercent will be 0.
			// This will happen if the user imports fuel data that do not include gasoline rows (e.g., if you are running diesel only,
			// export default fuels, and reimport them).
			if ((results != null) && results.next()) {
				RVPmkt = results.getDouble(1);
				// oxygenWeightPercent = results.getDouble(2);
				gasSulfurPercent = results.getDouble(3);
				results.close();
			} else {
				isOK = false;
			}
			statement.close();
			
			// isOK will be true even if the above statement runs and returns no rows
			// Therefore, check to make sure RVP and sulfur have nonzero values. If they are both zeros, it is most likely that 
			// no data were actually found in the above rows (also, NR Fortran will fail when error checking this input data).
			// If this happens, get the default RVP and sulfur values for fuelFormulationID 10 so that the Fortran will run.
			// Issue a warning message so that the user knows this is happening in case they intended to import gasoline fuels
			if (!isOK || (RVPmkt == 0.0 && gasSulfurPercent == 0.0)) {
				Logger.log(LogMessageCategory.INFO, "Failed to get the RVPmkt, gasSulfurPercent information for OPTIONS. Using default values for FuelFormulationID 10 instead.");
				
				// func A
				sql = "select RVP as RVPmkt, volToWtPercentOxy as oxyMkt, sulfurLevel/10000 as gasSulfur " +
					  "from fuelformulation fs " +
					  "where fuelFormulationID = 10;";
				statement = executionDB.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					RVPmkt = results.getDouble(1);
					// oxygenWeightPercent = results.getDouble(2);
					gasSulfurPercent = results.getDouble(3);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				
				// If RVP and sulfur are *still* 0, we couldn't find even a default fuel to run with, so issue the error message
				// (NR Fortran will fail)
				if (!isOK || (RVPmkt == 0.0 && gasSulfurPercent == 0.0)) {
					Logger.log(LogMessageCategory.ERROR, "Failed to get the default FuelFormulationID 10 RVPmkt, gasSulfurPercent information for OPTIONS.");
				}
			}

			// oxygenWeightPercent -- function A2
			if (isOK) {
				sql = "select  (sum(ff.ETOHVolume * ff.voltowtpercentoxy * fs.MarketShare)  + sum(ff.MTBEVolume * "
						+ " ff.voltowtpercentoxy * fs.MarketShare)  + sum(ff.ETBEVolume * ff.voltowtpercentoxy * fs.MarketShare)  + "
						+ " sum(ff.TAMEVolume *  ff.voltowtpercentoxy * fs.MarketShare)) as oxyWtPct "
						+ " from nrfuelsupply fs"
						+ " inner join fuelformulation ff on fs.fuelformulationid = ff.fuelformulationid"
						+ " inner join nrfuelsubtype sub on ff.fuelsubtypeid = sub.fuelsubtypeid"
						+ " inner join year y on y.fuelYearID=fs.fuelYearID"
						+ " inner join regionCounty rc on (rc.regionID=fs.fuelRegionID and rc.regionCodeID=2 and rc.fuelYearID=y.fuelYearID)"
						+ " where rc.countyid = ? and fs.monthgroupid = ?"
						+ " and y.yearID = ? "
						+ " and sub.fueltypeid = 1";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, monthID);
				statement.setInt(3, episodeYear);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					oxygenWeightPercent = results.getDouble(1);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the oxygenWeightPercent information for OPTIONS.");
				}
			}

			// dieselSulfurPercent -- function C
			if (isOK) {
				sql = "select sum(ff.sulfurLevel*fs.marketshare)/10000 as dslSulfur  "
						+ " from nrfuelsupply fs"
						+ " inner join fuelformulation ff on fs.fuelformulationid = ff.fuelformulationid"
						+ " inner join year y on y.fuelYearID=fs.fuelYearID"
						+ " inner join regionCounty rc on (rc.regionID=fs.fuelRegionID and rc.regionCodeID=2 and rc.fuelYearID=y.fuelYearID)"
						+ " where rc.countyid = ? and fs.monthgroupid = ?"
						+ " and y.yearID = ? "
						+ "and ff.fuelsubtypeid = 23";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, monthID);
				statement.setInt(3, episodeYear);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					dieselSulfurPercent = results.getDouble(1);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the dieselSulfurPercent information for OPTIONS.");
				}
			}

			// marineDieselSulfurPercent -- function D
			if (isOK) {
				sql = "select sum(ff.sulfurLevel*fs.marketshare)/10000 as MarineDslSulfur  "
						+ " from nrfuelsupply fs"
						+ " inner join fuelformulation ff on fs.fuelformulationid = ff.fuelformulationid"
						+ " inner join year y on y.fuelYearID=fs.fuelYearID"
						+ " inner join regionCounty rc on (rc.regionID=fs.fuelRegionID and rc.regionCodeID=2 and rc.fuelYearID=y.fuelYearID)"
						+ " where rc.countyid = ? and fs.monthgroupid = ?"
						+ " and y.yearID = ? "
						+ " and ff.fuelsubtypeid = 24";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, monthID);
				statement.setInt(3, episodeYear);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					marineDieselSulfurPercent = results.getDouble(1);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the marineDieselSulfurPercent information for OPTIONS.");
				}
			}

			// naturalGasSulfurPercent -- function E
			if (isOK) {
				sql = "select (sum(ff.sulfurLevel*fs.marketshare)/10000)/sum(fs.marketshare) as CNGLPGlSulfur "
						+ " from nrfuelsupply fs"
						+ " inner join fuelformulation ff on fs.fuelformulationid = ff.fuelformulationid"
						+ " inner join year y on y.fuelYearID=fs.fuelYearID"
						+ " inner join regionCounty rc on (rc.regionID=fs.fuelRegionID and rc.regionCodeID=2 and rc.fuelYearID=y.fuelYearID)"
						+ " where rc.countyid = ? and fs.monthgroupid = ?"
						+ " and y.yearID = ? "
						+ " and ( ff.fuelsubtypeid = 30 OR ff.fuelsubtypeid = 40)";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, monthID);
				statement.setInt(3, episodeYear);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					naturalGasSulfurPercent = results.getDouble(1);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the naturalGasSulfurPercent information for OPTIONS.");
				}
			}

			// minimumTemperature, maximumTemperature
			if (isOK) {
				sql = "SELECT min(temperature), max(temperature) from zonemonthhour t "
						+ "inner join zone z on t.zoneid = z.zoneid "
						+ " where z.countyid = ? and t.monthid = ?";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, monthID);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					minimumTemperature = results.getDouble(1);
					maximumTemperature = results.getDouble(2);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the Min and Max temperature for OPTIONS.");
				}
			}
			// averageTemperature
			if (isOK) {
				sql = "SELECT avg(temperature) from zonemonthhour t "
						+ "inner join zone z on t.zoneid = z.zoneid "
						+ " where z.countyid = ? and t.monthid = ? and hourid >= 6 and hourid <= 18";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, monthID);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					averageTemperature = results.getDouble(1);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the Average temperature for OPTIONS.");
				}
			}

			// altitude
			if (isOK) {
				sql = "SELECT Altitude FROM County WHERE CountyId = ? AND StateId = ?";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, stateID);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					altitude = "" + results.getString(1);
					altitude = (altitude.compareTo("L") == 0) ? "LOW" : "HIGH";
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the County Altitude information for OPTIONS.");
				}
			}

			// func B
			if (isOK) {
				sql = "select sum(fs.marketshare)*100. as etohmkt, sum(ff.etohvolume * fs.marketshare)/sum(fs.marketshare) as etohvolpct "
						+ " from nrfuelsupply fs"
						+ " inner join fuelformulation ff on fs.fuelformulationid = ff.fuelformulationid"
						+ " inner join nrfuelsubtype sub on ff.fuelsubtypeid = sub.fuelsubtypeid"
						+ " inner join year y on y.fuelYearID=fs.fuelYearID"
						+ " inner join regionCounty rc on (rc.regionID=fs.fuelRegionID and rc.regionCodeID=2 and rc.fuelYearID=y.fuelYearID)"
						+ " where rc.countyid = ? and fs.monthgroupid = ?"
						+ " and y.yearID = ? "
						+ " and etohvolume > 0.0 and sub.fueltypeid = 1";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, monthID);
				statement.setInt(3, episodeYear);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					ETOHMktShare = results.getDouble(1);
					ETOHVolume = results.getDouble(2);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the ETOHMktShare and ETOHVolume information for OPTIONS.");
					return false;
				}
			}

		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in querying OPTIONS parameters : " + e);
			isOK = false;
		} finally {
			try {
				if (statement != null && !statement.isClosed()) {
					statement.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
				isOK = false;
			}

			parameterObject.seteTOHVolume(ETOHVolume);
			parameterObject.seteTOHMktShare(ETOHMktShare);
			parameterObject.setAltitude(altitude);
			parameterObject.setAverageTemperature(averageTemperature);
			parameterObject.setMaximumTemperature(maximumTemperature);
			parameterObject.setMinimumTemperature(minimumTemperature);
			parameterObject.setNaturalGasSulfurPercent(naturalGasSulfurPercent);
			parameterObject
					.setMarineDieselSulfurPercent(marineDieselSulfurPercent);
			parameterObject.setDieselSulfurPercent(dieselSulfurPercent);
			parameterObject.setGasSulfurPercent(gasSulfurPercent);
			parameterObject.setOxygenWeightPercent(oxygenWeightPercent);
			parameterObject.setrVPmkt(RVPmkt);
		}

		return isOK;
	}

	/**
	 * Get parameters for the "REGION" packet in the OPT file. Assumes
	 * optFileWriter has been initialized.
	 * @return true on success
	 **/
	public boolean getRegionParameters(
			// inputs
			int stateID, int countyID,
			// add as needed
			Connection executionDB, NonroadOptfileRegionPacket parameterObject) {

		String countyName = parameterObject.getCountyName();
		String stateAbbr = parameterObject.getStateAbbr();
		boolean isOK = true;

		if (countyID < 1000 && countyID >= 0 && stateID < 100 && stateID >= 0) {
			countyID = countyID + stateID * 1000;
		}

		String sql = null;
		PreparedStatement statement = null;
		ResultSet results = null;

		try {
			sql = "SELECT stateAbbr FROM State WHERE StateId = ?";
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, stateID);
			results = SQLRunner.executeQuery(statement, sql);
			if ((results != null) && results.next()) {
				stateAbbr = results.getString(1);
				results.close();
			} else {
				isOK = false;
			}
			statement.close();
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get the State Abbrevation information for REGION.");
			}

			if (isOK) {
				sql = "SELECT countyName FROM County WHERE CountyId = ? AND StateId = ?";
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, countyID);
				statement.setInt(2, stateID);
				results = SQLRunner.executeQuery(statement, sql);
				if ((results != null) && results.next()) {
					countyName = results.getString(1);
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get the County Name information for REGION.");
				}
			}
		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in querying REGION parameters : " + e);
			isOK = false;
		} finally {
			try {
				if (statement != null && !statement.isClosed()) {
					statement.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
				isOK = false;
			}
			parameterObject.setCountyName(countyName);
			parameterObject.setStateAbbr(stateAbbr);
		}

		return isOK;
	}

	/**
	 * Get SCCs for "SOURCE CATEGORY" packet in the OPT file. Assumes
	 * optFileWriter has been initialized.
	 * @return true on success
	 **/
	public boolean getSCCs(
			// inputs
			int[] fuelTypeIDs, int[] NREquipTypeIDs, Connection executionDB,
			// outputs
			TreeSet<String> SCCSelections) {
		boolean isOK = true;

		String sql;
		PreparedStatement statement = null;
		ResultSet results;

		try {
			String fuelTypeList = FormatUtil.intArrayToString(fuelTypeIDs, ",");
			String equipTypeList = FormatUtil.intArrayToString(NREquipTypeIDs,",");

			sql = "SELECT distinct SCC FROM nrscc WHERE fuelTypeID in ( "
					+ fuelTypeList + " ) and NREquipTypeID in ( "
					+ equipTypeList + " ) order by SCC";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if ((results != null)) {
				while (results.next()) {
					SCCSelections.add(results.getString(1));
				}
				results.close();
			} else {
				isOK = false;
			}
			statement.close();
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get SCC information for SOURCE CATEGORY.");
			}
		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in querying REGION parameters : " + e);
			isOK = false;
		} finally {
			try {
				if (statement != null && !statement.isClosed()) {
					statement.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
				isOK = false;
			}
		}

		return isOK;
	}

	/**
	 * check Retrofit table to see if it is empty
	 * 
	 * @param executionDB
	 * @return
	 **/
	public boolean checkRetrofit(
			// inputs
			Connection executionDB) {
		boolean isOK = true;
		boolean isEmpty = true;
		String sql;
		PreparedStatement statement = null;
		ResultSet results;

		try {

			sql = "select count(*) from nrretrofitfactors;";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if ((results != null)) {
				results.next();
				int count = results.getInt(1);
				isEmpty = count == 0 ? true : false;
			} else {
				isEmpty = true;
			}
			statement.close();

		} catch (SQLException e) {
			// Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in querying checking RETROFIT table : " + e);
			isOK = false;
		} finally {
			try {
				if (statement != null && !statement.isClosed()) {
					statement.close();
				}
			} catch (SQLException e) {
				e.printStackTrace();
				isOK = false;
			}
		}

		return isEmpty;
	}

	/**
	 * Get parameters for the "STAGE II" packet in the OPT file. Assumes
	 * optFileWriter has been initialized.
	 * 
	 * @return true on success
	 **/
	public boolean getStageIIParameters(
			// inputs
			int yearID, int countyID,
			// add as needed
			Connection executionDB,
			// outputs
			MutableDouble stgIIFac) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		ResultSet results = null;

		/*
		if (countyID < 1000) {
			Logger.log(LogMessageCategory.ERROR,"CountyID < 1000, cannot get the stage II factor information.");
			isOK = false;
		}
		*/

		try {
			sql = "select refuelingvaporprogramadjust*100. as stgIIFac from countyyear "
					+ "where countyid = ? and yearid = ?;";
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, countyID);
			statement.setInt(2, yearID);
			results = SQLRunner.executeQuery(statement, sql);
			if ((results != null) && results.next()) {
				stgIIFac.setValue(results.getDouble(1));
				results.close();
			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,"Failed to get the stage II factor information.");
			}
		} catch (SQLException e) {

			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,"Exception in querying stage II factor : " + e);
			isOK = false;
		} finally {
			try {
				if (statement != null && !statement.isClosed()) {
					statement.close();
				}
			} catch (SQLException e1) {
				e1.printStackTrace();
				isOK = false;
			}
		}

		return isOK;
	}

	/**
	 * Get information PM Based Sulfur packet in the OPT file. Assumes
	 * optFileWriter has been initialized.
	 * 
	 * @return true on success
	 **/
	public boolean getPMBasedSulfur(
			// inputs
			Connection executionDB,
			// outputs
			List<NonroadPMBasedSulfur> pmBasedSulfurList) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		ResultSet results = null;

		try {
			sql = "select distinct t.engtechname, s.pmbasesulfur, s.sulfatepmconversionfactor "
					+ "from nrsulfuradjustment s "
					+ "inner join enginetech t on s.engtechid = t.engtechid "
					+ "where s.fueltypeid in (23,24) order by t.engtechname;";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				while (results.next()) {
					pmBasedSulfurList.add(new NonroadPMBasedSulfur(results
							.getString(1), results.getDouble(2), results
							.getDouble(3)));
				}
				Collections.sort(pmBasedSulfurList);
			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get the PM Based Sulfur information.");
			}
		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in querying PM Based Sulfur : " + e);
			isOK = false;
		} finally {
			try {
				if (statement != null && !statement.isClosed()) {
					statement.close();
				}
			} catch (SQLException e1) {
				e1.printStackTrace();
				isOK = false;
			}
		}

		return isOK;
	}

	/**
	 * Get day name (weekday or weekend) according to a day ID. Assumes
	 * optFileWriter has been initialized.
	 **/
	public String getDayName(int dayID, Connection executionDB) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		ResultSet results = null;
		String name = null;

		try {
			sql = "SELECT dayName FROM dayofanyweek d where dayID = ?;";
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, dayID);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null && results.next()) {
				name = results.getString(1);
			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get day name from day ID.");
			}
		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in querying day name from day ID : " + e);
			isOK = false;
		} finally {
			try {
				if (statement != null && !statement.isClosed()) {
					statement.close();
				}
			} catch (SQLException e1) {
				e1.printStackTrace();
				isOK = false;
			}
		}

		return name;
	}

	/**
	 * Get month name according to a month ID.
	 **/
	public String getMonthName(int monthID, Connection executionDB) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		ResultSet results = null;
		String name = null;

		try {
			sql = "SELECT monthName FROM monthofanyyear m where monthID = ?;";
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, monthID);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null && results.next()) {
				name = results.getString(1);
			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get month name from day ID.");
			}
		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in querying month name from day ID : " + e);
			isOK = false;
		} finally {
			try {
				if (statement != null && !statement.isClosed()) {
					statement.close();
				}
			} catch (SQLException e1) {
				e1.printStackTrace();
				isOK = false;
			}
		}

		return name;
	}
}
