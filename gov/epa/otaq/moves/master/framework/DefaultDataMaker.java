/************************************************************************************************
 * @(#)DefaultDataMaker.java
 *
 ***********************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.*;
import java.sql.*;
import java.io.*;
import gov.epa.otaq.moves.common.*;

/**
 * An instance of this class is created by a calculator. It is used to insert
 * default values into the tables for the worker.
 * It identifies the tables extracted by an SQL script, and the additional
 * tables to be extracted.
 *
 * @author		Wesley Faler
 * @author      EPA - Mitch C
 * @version		2016-11-03
**/
public class DefaultDataMaker {
	/** a TreeSetIgnoreCase of the tables extracted by the script of a calculator **/
	TreeSetIgnoreCase tablesExtractedByScript = new TreeSetIgnoreCase();
	/** a TreeSetIgnoreCase of the tables to be extracted by the script of a calculator &*/
	TreeSetIgnoreCase tablesToBeExtracted = new TreeSetIgnoreCase();

	/**
	 * Add default data to the execution database.  This is useful for things such
	 * as the FuelSupply table which is used by WTP calculators on the master-side
	 * yet must have default data present.
	**/
	public static void addDefaultDataToExecutionDatabase() {
		boolean needsNonRoad = ExecutionRunSpec.getRunSpec().models.contains(Model.NONROAD);

		Connection executionDatabase = null;
		try {
			executionDatabase = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
		} catch(Exception e) {
			/**
			 * @explain A connection to the MOVESExecution database could not be established but
			 * was needed in order to create default fuel supply information.
			**/
			Logger.logError(e,"Unable to get the Execution Database connection needed for running"
					+ " DefaultDataMaker.");
			return;
		}

		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			// Fix any misaligned fuelSubtypeIDs in gasoline and ethanol fuels
			String[] formulationFixes = {
				"update fuelFormulation set fuelSubtypeID = 10 where fuelSubtypeID <> 10 and ETOHVolume < 0.10  and fuelSubtypeID <> 11 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18)",
				"update fuelFormulation set fuelSubtypeID = 12 where fuelSubtypeID <> 12 and ETOHVolume >= 9    and ETOHVolume < 12.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18)",
				"update fuelFormulation set fuelSubtypeID = 13 where fuelSubtypeID <> 13 and ETOHVolume >= 6    and ETOHVolume < 9 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18)",
				"update fuelFormulation set fuelSubtypeID = 14 where fuelSubtypeID <> 14 and ETOHVolume >= 0.10 and ETOHVolume < 6 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18)",
				"update fuelFormulation set fuelSubtypeID = 15 where fuelSubtypeID <> 15 and ETOHVolume >= 12.5 and ETOHVolume < 17.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18)",
				"update fuelFormulation set fuelSubtypeID = 51 where fuelSubtypeID <> 51 and ETOHVolume >= 70.5 and ETOHVolume <= 100 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18)",
				"update fuelFormulation set fuelSubtypeID = 52 where fuelSubtypeID <> 52 and ETOHVolume >= 50.5 and ETOHVolume < 70.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18)",
				"update fuelFormulation set fuelSubtypeID = 18 where fuelSubtypeID <> 18 and ETOHVolume >= 17.5 and ETOHVolume < 50.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18)"
			};
			for(int i=0;i<formulationFixes.length;i++) {
				sql = formulationFixes[i];
				SQLRunner.executeSQL(executionDatabase,sql);
			}

			//Set fuelFormulation values to 0 instead of null
			sql = getFuelFormulationNullsSQL();
			SQLRunner.executeSQL(executionDatabase,sql);

			//Calculate volToWtPercentOxy
			sql = "calculateVolToWtPercentOxy()";
			calculateVolToWtPercentOxy(executionDatabase);

			// Update T50, T90, e200, and e300
			String[] t50t90Calculations = {
				"update fuelFormulation set T50 = 2.0408163 * (147.91 - e200) where e200 is not null and e200 > 0 and (T50 is null or T50 <= 0)",
				"update fuelFormulation set T90 = 4.5454545 * (155.47 - e300) where e300 is not null and e300 > 0 and (T90 is null or T90 <= 0)",
				"update fuelFormulation set e200 = 147.91-(T50/2.0408163) where T50 is not null and T50 > 0 and (e200 is null or e200 <= 0)",
				"update fuelFormulation set e300 = 155.47-(T90/4.5454545) where T90 is not null and T90 > 0 and (e300 is null or e300 <= 0)"
				/*
				"update nrFuelFormulation set T50 = 2.0408163 * (147.91 - e200) where e200 is not null and e200 > 0 and (T50 is null or T50 <= 0)",
				"update nrFuelFormulation set T90 = 4.5454545 * (155.47 - e300) where e300 is not null and e300 > 0 and (T90 is null or T90 <= 0)",
				"update nrFuelFormulation set e200 = 147.91-(T50/2.0408163) where T50 is not null and T50 > 0 and (e200 is null or e200 <= 0)",
				"update nrFuelFormulation set e300 = 155.47-(T90/4.5454545) where T90 is not null and T90 > 0 and (e300 is null or e300 <= 0)"
				*/
			};
			for(int i=0;i<t50t90Calculations.length;i++) {
				sql = t50t90Calculations[i];
				SQLRunner.executeSQL(executionDatabase,sql);
			}

			//Remove fuelSupply records with zero market shares.  Their presence causes unneeded
			//overhead when joining to the fuelSupply table.
			sql = "delete from fuelSupply where marketShare < 0.0001";
			SQLRunner.executeSQL(executionDatabase,sql);

			//use default values for the Marketshare field  if no record is found in the
			//FuelSupply table for a given county, year, monthgroup, and fueltype.
			// As first step, create a lists of fuelYears and fuelTypes relevant to the run specification
			sql = "DROP TABLE IF EXISTS RunSpecFuelYear";
			SQLRunner.executeSQL(executionDatabase,sql);

			sql = "CREATE TABLE RunSpecFuelYear " +
				  "SELECT DISTINCT fuelYearID FROM Year INNER JOIN RunSpecYear USING(yearID)";
			SQLRunner.executeSQL(executionDatabase,sql);

			sql = "DROP TABLE IF EXISTS RunSpecFuelType";
			SQLRunner.executeSQL(executionDatabase,sql);

			sql = "CREATE TABLE RunSpecFuelType SELECT DISTINCT fuelTypeID from RunSpecSourceFuelType union select distinct fuelTypeID from RunSpecSectorFuelType";
			SQLRunner.executeSQL(executionDatabase,sql);

			TreeSet<String> defaultFuelMessages = new TreeSet<String>();
			boolean[] isNonroadTables = { false, true };
			String[] fuelSupplyTables = { "FuelSupply", "NRFuelSupply" };
			String[] fuelTypeTables = { "FuelType", "NRFuelType" };
			String[] fuelSubTypeTables = { "FuelSubtype", "NRFuelSubtype" };
			for(int i=0;i<fuelSupplyTables.length;i++) {
				if(needsNonRoad != isNonroadTables[i]) {
					continue;
				}
				// as second step, create a list of fuelTypes by county, fuelYear, and MonthGroup
				// which have a non-default fuel supply, making list relevant to the run specification
				sql = "DROP TABLE IF EXISTS GivenFuelSupply";
				SQLRunner.executeSQL(executionDatabase,sql);
	
				sql = "CREATE TABLE GivenFuelSupply " +
					"SELECT DISTINCT fs.fuelRegionID, fs.fuelYearID, fs.monthGroupID, fst.fuelTypeID " +
					"FROM " + fuelSupplyTables[i] + " fs " +
					"INNER JOIN RunSpecFuelRegion rsc ON fs.fuelRegionID=rsc.fuelRegionID " +
					"INNER JOIN RunSpecFuelYear rsfy ON fs.fuelYearID=rsfy.fuelYearID " +
					"INNER JOIN RunSpecMonthGroup rsmg ON fs.monthGroupID=rsmg.monthGroupID " +
					"INNER JOIN FuelFormulation ff ON fs.fuelFormulationID=ff.fuelFormulationID " +
					"INNER JOIN " + fuelSubTypeTables[i] + " fst ON ff.fuelSubtypeID=fst.fuelSubtypeID ";
				SQLRunner.executeSQL(executionDatabase,sql);
	
				// as third step create a list of fuelTypes, also by county, fuelYear, and monthGroup
				//   which need a fuel supply. Join with regioncounty to ensure only valid combinations get selected
				sql = "DROP TABLE IF EXISTS NeededFuelSupply";
				SQLRunner.executeSQL(executionDatabase,sql);
	
				sql = "CREATE TABLE NeededFuelSupply " +
					"SELECT DISTINCT mrsfr.fuelRegionID, mrsfy.fuelYearID, mrsmg.monthGroupID, " +
						"ft.fuelTypeID, defaultFormulationID " +
					" FROM RunSpecFuelRegion mrsfr" +
					" CROSS JOIN RunSpecFuelYear mrsfy" +
					" CROSS JOIN RunSpecMonthGroup mrsmg" +
					" CROSS JOIN RunSpecFuelType mrsft" +
					" CROSS JOIN RunSpecCounty mrsc " +
					" INNER JOIN " + fuelTypeTables[i] + " ft ON mrsft.fuelTypeID = ft.fuelTypeID " +
					" INNER JOIN regioncounty rc ON mrsfr.fuelRegionID = rc.regionID " +
                    "      AND mrsc.countyID = rc.countyID " +
					"			  AND mrsfy.fuelYearID = rc.fuelYearID " +
					"			  AND rc.regionCodeID = 1";
				SQLRunner.executeSQL(executionDatabase,sql);
	
				sql = "create unique index XPKGivenFuelSupply on GivenFuelSupply ("
						+ " fuelRegionID, fuelYearID, monthGroupID, fuelTypeID)";
				SQLRunner.executeSQL(executionDatabase,sql);
	
				sql = "create unique index XPKNeededFuelSupply on NeededFuelSupply ("
						+ " fuelRegionID, fuelYearID, monthGroupID, fuelTypeID)";
				SQLRunner.executeSQL(executionDatabase,sql);
	
				// as fourth and final step, insert needed-but-missing records into FuelSupply
				sql = "INSERT INTO " + fuelSupplyTables[i] + " (fuelRegionID, fuelYearID, monthGroupID, " +
					"fuelFormulationID, marketShare, marketShareCV) " +
					"SELECT nfs.fuelRegionID, nfs.fuelYearID, nfs.monthGroupID, " +
					"nfs.defaultFormulationID, 1.0, 0.0 " +
					"FROM NeededFuelSupply nfs  LEFT JOIN GivenFuelSupply gfs " +
					"USING(fuelRegionID, fuelYearID, monthGroupID, fuelTypeID) " +
					"WHERE gfs.fuelTypeID IS NULL ";
				SQLRunner.executeSQL(executionDatabase,sql);
	
				sql = "ANALYZE TABLE " + fuelSupplyTables[i];
				SQLRunner.executeSQL(executionDatabase,sql);
	
				// Issue warnings for each default fuel that had to be added
				sql = "SELECT nfs.fuelRegionID, nfs.fuelYearID, nfs.monthGroupID, " +
					"nfs.defaultFormulationID, fuelTypeDesc " +
					"FROM NeededFuelSupply nfs " +
					"INNER JOIN " + fuelTypeTables[i] + " ft USING (fuelTypeID) " +
					"LEFT JOIN GivenFuelSupply gfs " +
					"USING(fuelRegionID, fuelYearID, monthGroupID, fuelTypeID) " +
					"WHERE gfs.fuelTypeID IS NULL and ft.fuelTypeID <> 9 " + // give no warning about default formulation for Electricity
					"ORDER BY nfs.fuelRegionID, nfs.fuelYearID, nfs.monthGroupID, fuelTypeDesc";
				query.open(executionDatabase,sql);
				while(query.rs.next()) {
					int regionID = query.rs.getInt(1);
					int fuelYearID = query.rs.getInt(2);
					int monthGroupID = query.rs.getInt(3);
					int defaultFormulationID = query.rs.getInt(4);
					String fuelTypeDescription = query.rs.getString(5);
					String message = "WARNING: Using default formulation " + defaultFormulationID
							+ " for " + fuelTypeDescription
							+ " in region " + regionID
							+ ", year " + fuelYearID
							+ ", month " + monthGroupID
							+ ". Check your input " + fuelSupplyTables[i] + " table for errors.";
					if(!defaultFuelMessages.contains(message)) {
						defaultFuelMessages.add(message);
						Logger.log(LogMessageCategory.WARNING,message);
					}
				}
				query.close();
			}

			/*
			//use default values for the fuel adjustment field  if no record is found in the
			//FuelAdjustment table for a given pollutant-process-modelYearGroup, sourceTypeID, and
			//fuelFormulationID.
			sql = "INSERT INTO FuelAdjustment ("
					+ " polProcessID,"
					+ " fuelMYGroupID,"
					+ " sourceTypeID,"
					+ " fuelFormulationID,"
					+ " fuelAdjustment,"
					+ " fuelAdjustmentCV,"
					+ " fuelAdjustmentGPA,"
					+ " fuelAdjustmentGPACV)"
					+ " SELECT"
					+ " rspp.polProcessID,"
					+ " fmyg.fuelMYGroupID,"
					+ " rssft.sourceTypeID,"
					+ " ff.fuelFormulationID,"
					+ " 1, 0, 1, 0"
					+ " FROM RunSpecPollutantProcess rspp"
					+ " INNER JOIN FuelModelYearGroup fmyg"
					+ " INNER JOIN RunSpecSourceFuelType rssft"
					+ " INNER JOIN FuelSubType fst ON fst.fuelTypeID = rssft.fuelTypeID"
					+ " INNER JOIN FuelFormulation ff ON ff.fuelSubTypeID = fst.fuelSubTypeID"
					+ " LEFT JOIN FuelAdjustment fa ON (fa.polProcessID = rspp.polProcessID"
					+ " AND fa.fuelMYGroupID = fmyg.fuelMYGroupID"
					+ " AND fa.sourceTypeID = rssft.sourceTypeID"
					+ " AND fa.fuelFormulationID = ff.fuelFormulationID)"
					+ " WHERE fa.polProcessID IS NULL";

			SQLRunner.executeSQL(executionDatabase,sql);

			sql = "ANALYZE TABLE FuelAdjustment";
			SQLRunner.executeSQL(executionDatabase,sql);
			*/

			sql = "insert ignore into countyYear ("
					+ " countyID, yearID, refuelingVaporProgramAdjust, refuelingSpillProgramAdjust)"
					+ " select countyID, yearID, 0.0, 0.0"
					+ " from runSpecCounty, runSpecYear";
			SQLRunner.executeSQL(executionDatabase,sql);

			sql = "insert into TemperatureProfileID (temperatureProfileID, zoneID, monthID)"
					+ " select distinct (zoneID*10000)+(monthID*100) as temperatureProfileID, zoneID, monthID"
					+ " from zoneMonthHour"
					+ " where not exists ("
					+ " select *"
					+ " from TemperatureProfileID"
					+ " where TemperatureProfileID.zoneID=zoneMonthHour.zoneID"
					+ " and TemperatureProfileID.monthID=zoneMonthHour.monthID)";
			SQLRunner.executeSQL(executionDatabase,sql);

			sql = "create table if not exists NRModelYear like modelYear";
			SQLRunner.executeSQL(executionDatabase,sql);
			for(int i=1940;i<2060;i++) {
				sql = "insert ignore into NRModelYear (modelYearID) values (" + i + ")";
				SQLRunner.executeSQL(executionDatabase,sql);
			}
		} catch(SQLException e) {
			/**
			 * @explain An error occurred while creating default records.
			**/
			Logger.logError(e,"Unable to create default data");
		} finally {
			query.onFinally();
		}

		DatabaseConnectionManager.checkInConnection(
			MOVESDatabaseType.EXECUTION, executionDatabase);
		executionDatabase = null;
	}

	/**
	 * Obtain SQL to change any NULL-valued columns into 0-valued columns in the fuelFormulation table.
	**/
	public static String getFuelFormulationNullsSQL() {
		String[] columnNames = { "RVP","sulfurLevel","ETOHVolume","MTBEVolume","ETBEVolume",
				"TAMEVolume","aromaticContent","olefinContent","benzeneContent","e200","e300",
				"volToWtPercentOxy","BioDieselEsterVolume","CetaneIndex","PAHContent","T50","T90"
		};
		String sql = "update fuelFormulation set ";
		for(int i=0;i<columnNames.length;i++) {
			if(i > 0) {
				sql += ",";
			}
			sql += columnNames[i] + "=ifnull(" + columnNames[i] + ",0)";
		}
		return sql;
	}

	/**
	 * Adds the tables extracted by the calculator script to tablesExtractedByScript
	 * @param tableName The name of the table to be added to the TreeSet
	**/
	public void addTableExtractedByScript(String tableName) {
		String t = tableName.toLowerCase();
		tablesExtractedByScript.add(t);
	}

	/**
	 * Adds the tables to be extracted by the calculator script to the
	 * tablesToBeExtracted TreeSet. These tables are related and are identified
	 * by tablesExtractedByScript.
	**/
	public void determineAllTablesToExtractAndCreate() {
		if(tablesExtractedByScript.contains("TemperatureAdjustment")) {
			tablesToBeExtracted.add("RunSpecSourceFuelType");
			tablesToBeExtracted.add("PollutantProcessAssoc");
		}

		// Disregard any tables already being handled by the script
		for(Iterator i=tablesExtractedByScript.iterator();i.hasNext();) {
			String tableName = (String)i.next();
			tablesToBeExtracted.remove(tableName);
		}
	}

	/**
	 * Creates a linked list of the SQLs which stores default data into the
	 * tables identified by a calculator
	 * @return the Linked List of the SQLs to store default values into
	 * the tables used in a calculator
	**/
	public LinkedList<String> getTableCreationSQL() {
		LinkedList<String> result = new LinkedList<String>();
		String sql;

		for(Iterator<String> i=tablesToBeExtracted.iterator();i.hasNext();) {
			String tableName = (String)i.next();
			sql = (String)DatabaseConnectionManager.executionDatabaseCreateTableStatements.get(tableName);
			result.add(sql);
			sql = "TRUNCATE TABLE " + tableName + ";";
			result.add(sql);
		}

		/*
		// Give the total set of pollutant/process selections to the external calculator. Just
		// the file is needed. It doesn't need to be loaded into a real table.
		sql = (String)DatabaseConnectionManager.executionDatabaseCreateTableStatements.get("RunSpecPollutantProcess");
		sql = StringUtilities.replace(sql,"RunSpecPollutantProcess","extpollutantprocess");
		result.add(sql);
		sql = "TRUNCATE TABLE extpollutantprocess;";
		result.add(sql);
		*/

		return result;
	}

	/**
	 * Creates a linked list of the SQLs to extract data from the
	 * tablesToBeExtracted TreeSet into output flat files
	 * @return the Linked List of the SQLs to extract data from the
	 * tablesToBeExtracted TreeSet into output flat files
	**/
	public LinkedList<String> getDataExtractionSQL() {
		LinkedList<String> result = new LinkedList<String>();
		String sql;

		for(Iterator<String> i=tablesToBeExtracted.iterator();i.hasNext();) {
			String tableName = (String)i.next();
			sql = "SELECT * INTO OUTFILE '##" + tableName.toLowerCase() + "##' FROM " + tableName + ";";
			result.add(sql);
		}

		String[] externalCalculatorStatements = {
			"cache select "
			+ " ##context.iterLocation.stateRecordID## as stateID,"
			+ " ##context.iterLocation.countyRecordID## as countyID,"
			+ " ##context.iterLocation.zoneRecordID## as zoneID,"
			+ " ##context.iterLocation.linkRecordID## as linkID,"
			+ " ##context.year## as yearID,"
			+ " ##context.monthID## as monthID"
			+ " into outfile '##extconstants##';",

			// Give the total set of pollutant/process selections to the external calculator. Just
			// the file is needed. It doesn't need to be loaded into a real table.
			"cache SELECT * INTO OUTFILE '##extpollutantprocess##' FROM RunSpecPollutantProcess;",

			"cache select ageID, ageGroupID"
			+ " into outfile '##extagecategory##'"
			+ " from AgeCategory;",

			"cache select ##context.iterLocation.countyRecordID##, ##context.year##, ##context.monthID##, "
			+ " 		fst.fuelTypeID, fst.fuelSubTypeID, ff.fuelFormulationID, fs.marketShare"
			+ " into outfile '##extfuelsupply##'"
			+ " from year"
			+ " inner join fuelSupply fs on (fs.fuelYearID=year.fuelYearID)"
			+ " inner join monthOfAnyYear moay on (moay.monthGroupID=fs.monthGroupID)"
			+ " inner join fuelFormulation ff on (ff.fuelFormulationID=fs.fuelFormulationID)"
			+ " inner join fuelSubtype fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)"
			+ " where yearID = ##context.year##"
			+ " and fs.fuelRegionID = ##context.fuelRegionID##"
			+ " and moay.monthID = ##context.monthID##"
			+ " and fst.fuelTypeID in (##macro.csv.all.fuelTypeID##);",

			"cache select ##context.iterLocation.countyRecordID##, ##context.year##, ##context.monthID##,"
			+ " 	fst.fuelTypeID, fst.fuelSubTypeID, ff.fuelFormulationID, fs.marketShare"
			+ " into outfile '##extnrfuelsupply##'"
			+ " from year"
			+ " inner join nrFuelSupply fs on (fs.fuelYearID=year.fuelYearID)"
			+ " inner join monthOfAnyYear moay on (moay.monthGroupID=fs.monthGroupID)"
			+ " inner join fuelFormulation ff on (ff.fuelFormulationID=fs.fuelFormulationID)"
			+ " inner join nrFuelSubtype fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)"
			+ " where yearID = ##context.year##"
			+ " and fs.fuelRegionID = ##context.fuelRegionID##"
			+ " and moay.monthID = ##context.monthID##"
			+ " and fst.fuelTypeID in (##macro.csv.all.nrFuelTypeID##);",

			"cache select fuelTypeID, fuelDensity, subjectToEvapCalculations"
			+ " into outfile '##extfueltype##'"
			+ " from fuelType;",

			"cache select fuelTypeID, fuelDensity, subjectToEvapCalculations"
			+ " into outfile '##extnrfueltype##'"
			+ " from nrFuelType;",
			
			"cache select fuelSubtypeID, fuelTypeID, fuelSubtypePetroleumFraction, fuelSubtypeFossilFraction,"
			+ " 	carbonContent, oxidationFraction, energyContent"
			+ " into outfile '##extfuelsubtype##'"
			+ " from fuelSubtype;",

			"cache select fuelSubtypeID, fuelTypeID, fuelSubtypePetroleumFraction, fuelSubtypeFossilFraction,"
			+ " 	carbonContent, oxidationFraction, energyContent"
			+ " into outfile '##extnrfuelsubtype##'"
			+ " from nrFuelSubtype;",
			
			"cache select distinct"
			+ " 	FuelFormulation.fuelFormulationID,"
			+ " 	FuelFormulation.fuelSubtypeID,"
			+ " 	ifnull(FuelFormulation.RVP,0),"
			+ " 	ifnull(FuelFormulation.sulfurLevel,0),"
			+ " 	ifnull(FuelFormulation.ETOHVolume,0),"
			+ " 	ifnull(FuelFormulation.MTBEVolume,0),"
			+ " 	ifnull(FuelFormulation.ETBEVolume,0),"
			+ " 	ifnull(FuelFormulation.TAMEVolume,0),"
			+ " 	ifnull(FuelFormulation.aromaticContent,0),"
			+ " 	ifnull(FuelFormulation.olefinContent,0),"
			+ " 	ifnull(FuelFormulation.benzeneContent,0),"
			+ " 	ifnull(FuelFormulation.e200,0),"
			+ " 	ifnull(FuelFormulation.e300,0),"
			+ " 	ifnull(FuelFormulation.volToWtPercentOxy,0),"
			+ " 	ifnull(FuelFormulation.BioDieselEsterVolume,0),"
			+ " 	ifnull(FuelFormulation.CetaneIndex,0),"
			+ " 	ifnull(FuelFormulation.PAHContent,0),"
			+ " 	ifnull(FuelFormulation.T50,0),"
			+ " 	ifnull(FuelFormulation.T90,0)"
			+ " INTO OUTFILE '##extfuelformulation##'"
			+ " FROM FuelFormulation"
			+ " WHERE FuelFormulation.FuelFormulationID IN ("
			+ " 	select distinct ff.fuelFormulationID"
			+ " 	FROM FuelSupply"
			+ " 	INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)"
			+ " 	INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)"
			+ " 	INNER JOIN FuelFormulation ff ON (ff.fuelFormulationID = FuelSupply.fuelFormulationID)"
			+ " 	INNER JOIN FuelSubtype ON (FuelSubtype.fuelSubtypeID = ff.fuelSubtypeID)"
			+ " 	INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)"
			+ " 	INNER JOIN RunSpecMonth ON (RunSpecMonth.monthID = MonthOfAnyYear.monthID)"
			+ " 	WHERE fuelRegionID = ##context.fuelRegionID##"
			+ " 	AND yearID = ##context.year##"
			+ " 	AND MonthOfAnyYear.monthID = ##context.monthID##"
			+ " 	AND FuelSubtype.fuelTypeID in (##macro.csv.all.fuelTypeID##)"
			+ " );",

			"cache select distinct"
			+ " 	FuelFormulation.fuelFormulationID,"
			+ " 	FuelFormulation.fuelSubtypeID,"
			+ " 	ifnull(FuelFormulation.RVP,0),"
			+ " 	ifnull(FuelFormulation.sulfurLevel,0),"
			+ " 	ifnull(FuelFormulation.ETOHVolume,0),"
			+ " 	ifnull(FuelFormulation.MTBEVolume,0),"
			+ " 	ifnull(FuelFormulation.ETBEVolume,0),"
			+ " 	ifnull(FuelFormulation.TAMEVolume,0),"
			+ " 	ifnull(FuelFormulation.aromaticContent,0),"
			+ " 	ifnull(FuelFormulation.olefinContent,0),"
			+ " 	ifnull(FuelFormulation.benzeneContent,0),"
			+ " 	ifnull(FuelFormulation.e200,0),"
			+ " 	ifnull(FuelFormulation.e300,0),"
			+ " 	ifnull(FuelFormulation.volToWtPercentOxy,0),"
			+ " 	ifnull(FuelFormulation.BioDieselEsterVolume,0),"
			+ " 	ifnull(FuelFormulation.CetaneIndex,0),"
			+ " 	ifnull(FuelFormulation.PAHContent,0),"
			+ " 	ifnull(FuelFormulation.T50,0),"
			+ " 	ifnull(FuelFormulation.T90,0)"
			+ " INTO OUTFILE '##extnrfuelformulation##'"
			+ " FROM fuelFormulation as FuelFormulation"
			+ " WHERE FuelFormulation.FuelFormulationID IN ("
			+ " 	select distinct ff.fuelFormulationID"
			+ " 	FROM NRFuelSupply as FuelSupply"
			+ " 	INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)"
			+ " 	INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)"
			+ " 	INNER JOIN FuelFormulation ff ON (ff.fuelFormulationID = FuelSupply.fuelFormulationID)"
			+ " 	INNER JOIN NRFuelSubtype as FuelSubtype ON (FuelSubtype.fuelSubtypeID = ff.fuelSubtypeID)"
			+ " 	INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)"
			+ " 	INNER JOIN RunSpecMonth ON (RunSpecMonth.monthID = MonthOfAnyYear.monthID)"
			+ " 	WHERE fuelRegionID = ##context.fuelRegionID##"
			+ " 	AND yearID = ##context.year##"
			+ " 	AND MonthOfAnyYear.monthID = ##context.monthID##"
			+ " 	AND FuelSubtype.fuelTypeID in (##macro.csv.all.nrFuelTypeID##)"
			+ " );",

			"cache select SCC, NREquipTypeID, fuelTypeID"
			+ " into outfile '##extnrscc##'"
			+ " from nrSCC;",

			"cache select nrhprangebinid, engtechid, nrhpcategory"
			+ " into outfile '##extnrhpcategory##'"
			+ " from nrHPCategory;"
		};
		for(int i=0;i<externalCalculatorStatements.length;i++) {
			result.add(externalCalculatorStatements[i]);
		}

		return result;
	}

	/**
	 * Creates a linked list of the SQLs to extract data from the
	 * tablesToBeExtracted TreeSet into input flat files
	 * @return the Linked List of the SQLs to extract data from the
	 * tablesToBeExtracted TreeSet into input flat files
	**/
	public LinkedList<String> getRemoteProcessingSQL() {
		LinkedList<String> result = new LinkedList<String>();
		String sql;

		// Do LOAD DATA INFILE statements first
		for(Iterator<String> i=tablesToBeExtracted.iterator(); i.hasNext();) {
			String tableName = (String)i.next();
			tableName = tableName.toLowerCase();
			sql = "LOAD DATA INFILE '##" + tableName + "##' INTO TABLE "
					+ tableName + ";";
			result.add(sql);

			sql = "ANALYZE TABLE " + tableName + ";";
			result.add(sql);
		}

		return result;
	}

	/**
	 * Creates a linked list of the SQLs to remove tables extracted
	 * @return the Linked List of the SQLs to delete the tables extracted
	**/
	public LinkedList<String> getRemoteCleanupSQL() {
		LinkedList<String> result = new LinkedList<String>();
		String sql;
		for(Iterator<String> i=tablesToBeExtracted.iterator(); i.hasNext();) {
			String tableName = (String)i.next();
			sql = "DROP TABLE IF EXISTS " + tableName;
			result.add(sql);
		}
		
		sql = "DROP TABLE IF EXISTS extpollutantprocess;";
		result.add(sql);

		return result;
	}

	/**
	 * Update the FuelFormulation table's volToWtPercentOxy field.
	 * @param db database to be used
	 * @throws SQLException if anything goes wrong
	**/
	public static void calculateVolToWtPercentOxy(Connection db) throws SQLException {
		String[] tableNames = { "fuelFormulation" }; // , "nrFuelFormulation" };
		for(int i=0;i<tableNames.length;i++) {
			String sql = "update " + tableNames[i] + " set volToWtPercentOxy="
					+ " case when (ETOHVolume+MTBEVolume+ETBEVolume+TAMEVolume) > 0 then"
					+ " 	(ETOHVolume*0.3653"
					+ " 	+ MTBEVolume*0.1792"
					+ " 	+ ETBEVolume*0.1537"
					+ " 	+ TAMEVolume*0.1651) / (ETOHVolume+MTBEVolume+ETBEVolume+TAMEVolume)"
					+ " else 0"
					+ " end";
			SQLRunner.executeSQL(db,sql);
		}
	}
}
