/**************************************************************************************************
 * @(#)BasicDataHandler.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.util.*;
import java.sql.*;
import java.io.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;

/**
 * Utility class for moving data with importers.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @author		Jarrod Brown
 * @version		2018-06-22
**/
public class BasicDataHandler implements IDataHandler {
	/** record whether or not any tables were imported; static so this record is valid across all basic data handlers **/
	private static boolean foundAnyTables = false;
	
	/** Marker for the beginning of a table's definition **/
	public static final String BEGIN_TABLE = "**BEGIN_TABLE**";

	/** Interface for objects providing data source information during imports **/
	public interface IProvider {
		/**
		 * Obtain the name of the file holding data for a table.
		 * @param tableName table in question
		 * @return the name of the file holding data for a table, null or blank if
		 * no file has been specified.
		**/
		String getTableFileSource(String tableName);

		/**
		 * Obtain the name of the worksheet within an XLS file holding data for a table.
		 * @param tableName table in question
		 * @return the name of the worksheet within an XLS file, null or blank if no
		 * worksheet has been specified or if the file is not an XLS file.
		**/
		String getTableWorksheetSource(String tableName);

		/**
		 * Allow custom processing and SQL for exporting data.
		 * @param type which type of MOVES database holds the exported data.  Typically, this
		 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
		 * being used.
		 * @param db database holding the data to be exported
		 * @param tableName table being exported
		 * @return SQL to be used or null if there is no alternate SQL.
		**/
		String getAlternateExportSQL(MOVESDatabaseType type, Connection db, String tableName);

		/**
		 * Cleanup custom processing and SQL for exporting data.
		 * @param type which type of MOVES database holds the exported data.  Typically, this
		 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
		 * being used.
		 * @param db database holding the data to be exported
		 * @param tableName table being exported
		**/
		void cleanupAlternateExportSQL(MOVESDatabaseType type, Connection db, String tableName);
	}

	/** Interface for objects providing data source information during imports **/
	public interface IProvider2 {
		/**
		 * Called to notify that an Import operation is starting.
		**/
		void onImportBegin();
	}

	/**
	 * Commonly used decode tables and queries for them.  Arranged as pairs of entries,
	 * the first entry is the table name, the second the ORDER BY'd query for records.
	 * These records are filtered against the ImporterManager using common column names
	 * to identify the filter columns and purposes.
	**/
	public static String[] commonDecodeTablesAndQueries = {
		"MonthGroupOfAnyYear",
		"select monthGroupID, monthGroupName"
		+ " from MonthGroupOfAnyYear"
		+ " order by monthGroupID",

		"MonthOfAnyYear",
		//"select monthID, monthName, (case when monthID=2 then (noOfDays+##isLeapYear##) else noOfDays end) as noOfDays"
		"select monthID, monthName, noOfDays"
		+ " from monthOfAnyYear"
		+ " order by monthID",

		"FuelFormulation",
		"select ff.fuelFormulationID,"
		+ " ft.fuelTypeID, ft.fuelTypeDesc,"
		+ " ff.fuelSubTypeID, fuelSubtypeDesc,"
		+ " RVP, sulfurLevel, ETOHVolume,"
		+ " MTBEVolume, ETBEVolume,"
		+ " TAMEVolume, aromaticContent, olefinContent,"
		+ " benzeneContent, e200, e300, t50, t90,"
		//+ " volToWtPercentOxy,"
		+ " bioDieselEsterVolume,"
		+ " cetaneIndex, PAHContent,"
		+ " fuelSubtypePetroleumFraction,"
		+ " fuelSubtypeFossilFraction,"
		+ " carbonContent, oxidationFraction,"
		+ " humidityCorrectionCoeff,"
		+ " energyContent, fuelDensity"
		+ " from FuelFormulation ff"
		+ " inner join FuelSubType fs using (fuelSubTypeID)"
		+ " inner join FuelType ft using (fuelTypeID)"
		+ " order by ft.fuelTypeID, ff.fuelFormulationID",

		"FuelSubtype",
		"select ft.fuelTypeID, ft.fuelTypeDesc,"
		+ " fuelSubTypeID, fuelSubtypeDesc,"
		+ " fuelSubtypePetroleumFraction,"
		+ " fuelSubtypeFossilFraction,"
		+ " carbonContent, oxidationFraction,"
		+ " humidityCorrectionCoeff,"
		+ " energyContent, fuelDensity"
		+ " from FuelSubType fs"
		+ " inner join FuelType ft using (fuelTypeID)"
		+ " order by ft.fuelTypeID, fs.fuelSubtypeID",

		"FuelType",
		"select fuelTypeID, fuelTypeDesc,"
		+ " humidityCorrectionCoeff,"
		+ " fuelDensity"
		+ " from FuelType ft"
		+ " order by ft.fuelTypeID",

		"nrFuelSubtype",
		"select ft.fuelTypeID, ft.fuelTypeDesc,"
		+ " fuelSubTypeID, fuelSubtypeDesc,"
		+ " fuelSubtypePetroleumFraction,"
		+ " fuelSubtypeFossilFraction,"
		+ " carbonContent, oxidationFraction,"
		+ " humidityCorrectionCoeff,"
		+ " energyContent, fuelDensity"
		+ " from nrFuelSubType fs"
		+ " inner join nrFuelType ft using (fuelTypeID)"
		+ " order by ft.fuelTypeID, fs.fuelSubtypeID",

		"nrFuelType",
		"select fuelTypeID, fuelTypeDesc,"
		+ " humidityCorrectionCoeff,"
		+ " fuelDensity"
		+ " from nrFuelType ft"
		+ " order by ft.fuelTypeID",

		"FuelSupplyYear",
		"select fuelYearID, yearID"
		+ " from year"
		+ " order by yearID",

		"County",
		"select countyID, stateName, countyName, countyTypeID, idleRegionID"
		+ " from County"
		+ " inner join State using (stateID)"
		+ " order by stateName, countyName",

		"CountyState",
		"select countyID, countyName, State.stateID, stateName, countyTypeID, idleRegionID"
		+ " from County"
		+ " inner join State using (stateID)"
		+ " order by stateName, countyName",

		"Zone",
		"select zoneID, z.countyID, stateName, countyName"
		+ " from Zone z"
		+ " inner join County c using (countyID)"
		+ " inner join State s using (stateID)"
		+ " order by stateName, countyName",

		"HPMSVType",
		"select HPMSVtypeID, HPMSVtypeName"
		+ " from HPMSVType"
		+ " order by HPMSVtypeID",

		"AgeCategory",
		"select ageID, ageCategoryName"
		+ " from ageCategory"
		+ " order by ageID",

		"RoadType",
		"select roadTypeID, roadDesc"
		+ " from roadType"
		+ " where roadTypeID <= 5"
		+ " order by roadTypeID",

		"RoadTypeHwy", // Not a real table, but a filtered version of RoadType
		"select roadTypeID, roadDesc"
		+ " from roadType"
		+ " where roadTypeID in (2,4)"
		+ " order by roadTypeID",

		"AvgSpeedBin",
		"select avgSpeedBinID, avgBinSpeed, avgSpeedBinDesc,"
		+ " opModeIDTirewear, opModeIDRunning"
		+ " from avgSpeedBin"
		+ " order by avgSpeedBinID",

		"SourceUseType",
		"select sourceTypeID, sourceTypeName, s.HPMSVtypeID, h.HPMSVtypeName"
		+ " from sourceUseType s"
		+ " inner join HPMSVtype h on h.HPMSVtypeID=s.HPMSVtypeID"
		+ " order by sourceTypeID",

		"HourOfAnyDay",
		"select hourID, hourName"
		+ " from hourOfAnyDay"
		+ " order by hourID",

		"HourDay",
		"select hourDayID, hd.dayID, dayName, hd.hourID, hourName"
		+ " from hourDay hd"
		+ " inner join hourOfAnyDay h using (hourID)"
		+ " inner join dayOfAnyWeek d using (dayID)"
		+ " order by hd.dayID, hd.hourID",

		"PollutantProcessAssoc",
		"select polProcessID, ppa.processID, processName, ppa.pollutantID, pollutantName"
		+ " from PollutantProcessAssoc ppa"
		+ " inner join EmissionProcess using (processID)"
		+ " inner join Pollutant using (pollutantID)"
		+ " order by polProcessID",

		"Pollutant",
		"select pollutantID, pollutantName, NEIPollutantCode, shortName"
		+ " from pollutant"
		+ " order by pollutantID",

		"Process",
		"select processID, processName, SCCProcID"
		+ " from emissionProcess"
		+ " order by processID",

		"IMPollutantProcessAssoc",
		"select polProcessID, ppa.processID, processName, ppa.pollutantID, pollutantName,"
		+ " isAffectedByExhaustIM, isAffectedByEvapIM"
		+ " from PollutantProcessAssoc ppa"
		+ " inner join EmissionProcess using (processID)"
		+ " inner join Pollutant using (pollutantID)"
		+ " where (isAffectedByExhaustIM='Y' or isAffectedByEvapIM='Y')"
		+ " order by polProcessID",

		"OperatingMode",
		"select opModeID, opModeName, VSPLower, VSPUpper, speedLower, speedUpper,"
		+ " brakeRate1Sec, brakeRate3Sec, minSoakTime, maxSoakTime"
		+ " from OperatingMode"
		+ " order by opModeID",

		"OperatingModeAux",
		"select opModeID, opModeName"
		+ " from OperatingMode"
		+ " where opModeID >= 200 and opModeID <= 299"
		+ " order by opModeID",

		"StartsOperatingMode",
		"select opModeID, opModeName, VSPLower, VSPUpper, speedLower, speedUpper,"
		+ " brakeRate1Sec, brakeRate3Sec, minSoakTime, maxSoakTime"
		+ " from OperatingMode"
		+ " where opModeID >= 101 and opModeID < 150"
		+ " order by opModeID",

		"State",
		"SELECT stateID, stateName, stateAbbr, idleRegionID FROM state ORDER BY stateName",

		"IMInspectFreq",
		"select inspectFreq, inspectFreqDesc from IMInspectFreq order by inspectFreq",

		"IMTestStandards",
		"select testStandardsID, testStandardsDesc"
		+ " from IMTestStandards order by testStandardsDesc",

		"EngineTech",
		"select engTechID, engTechName"
		+ " from engineTech order by engTechID",

		"DayOfAnyWeek",
		"select dayID, dayName, noOfRealDays"
		+ " from DayOfAnyWeek"
		+ " order by dayID",

		"Region",
		"select regionID, VV, WW, XX, YY, ZZ, description"
		+ " from region"
		+ " order by regionID",

		"CountyType",
		"select countyTypeID, countyTypeDescription"
		+ " from countyType"
		+ " order by countyTypeID",

		"IdleRegion",
		"select idleRegionID, idleRegionDescription"
		+ " from idleRegion"
		+ " order by idleRegionID"

		/* TODO reinstate once NRDB use is mandatory
		"NRAgeCategory",
		"select ageID, ageCategoryName"
		+ " from NRAgeCategory"
		+ " order by ageID",

		"NREquipmentType",
		"select NREquipTypeID, description, sectorID, useDefaultScrappage, surrogateID"
		+ " from NREquipmentType"
		+ " order by NREquipTypeID"
		*/
	};

	/** Importer **/
	IImporter importer;
	/** Decode tables and their queries, keyed by table name, holds SQL strings **/
	TreeMapIgnoreCase decodeTablesAndQueries = new TreeMapIgnoreCase();
	/**
	 * Descriptor for the table(s) used by this importer.  The format is as follows:
	 * BEGIN_TABLE, "tablename1",
	 * "columnName1", "optionalDecodeTableName", "optionalFilterType",
	 * "columnNameN", "optionalDecodeTableName", "optionalFilterType",
	 * BEGIN_TABLE, "tableName2",
	 * ...
	 * Each column in each managed table is listed.  If the column requires the use of
	 * decode table when generating a template, then name the table otherwise just
	 * provide "".  If the column is subject to filtering according to the RunSpec's
	 * information, provide its standard name as the filter type (using ImportManager's
	 * FILTER_XXXX constants), otherwise provide "".
	**/
	String[] descriptor;
	/**
	 * Data table names as read from descriptor and created by the first call
	 * to getDataTableNames()
	**/
	ArrayList<String> dataTableNames;
	/** Provider for data required during imports **/
	IProvider importDataProvider;

	/**
	 * Constructor
	 * @param importerToUse importer for this data handler
	 * @param descriptorToUse descriptor for the table(s) used by this importer
	 * @param importDataProviderToUse Provider for data required during imports
	**/
	public BasicDataHandler(IImporter importerToUse, String[] descriptorToUse,
			IProvider importDataProviderToUse) {
		importer = importerToUse;
		descriptor = descriptorToUse;
		importDataProvider = importDataProviderToUse;
		// Setup common decode tables and their queries
		for(int i=0;i<commonDecodeTablesAndQueries.length;i+=2) {
			addDecodeTable(commonDecodeTablesAndQueries[i+0],commonDecodeTablesAndQueries[i+1]);
		}
		// Get definitions from passed in structure
		getDataTableNames();
		addAnyCustomDomainInformation();
	}

	/**
	 * Set the descriptor for the table, specifying affected tables, related tables, columns, and filters.
	 * @param descriptorToUse descriptor for the table(s) used by this importer
	**/
	public void changeDescriptor(String[] descriptorToUse) {
		descriptor = descriptorToUse;
		dataTableNames = null; // forcing this to null causes getDataTableNames() to rescan descriptor
		getDataTableNames();
	}

	/** Add custom domain information, if it is available and needed **/
	void addAnyCustomDomainInformation() {
		if(importer == null || importer.getImporterManager() == null
				|| importer.getImporterManager().runSpec == null) {
			return;
		}
		// If use a custom domain via a Generic County, its data won't be in the database
		// so we provide it via direct means.
		if(importer.getImporterManager().runSpec.isCustomDomain()) {
			GenericCounty g = importer.getImporterManager().runSpec.genericCounty;
			int countyID = g.getCountyID();
			String sql = "select " + g.stateID + " as stateID, 'Generic' as stateName";
			addDecodeTable("State",sql);

			sql = "select " + countyID + " as countyID, 'Generic' as stateName,"
					+ DatabaseUtilities.escapeSQL(g.description,true) + " as countyName";
			addDecodeTable("County",sql);

			sql = "select " + (countyID*10) + " as zoneID,"
					+ countyID + " as countyID,"
					+ "'Generic' as stateName,"
					+ DatabaseUtilities.escapeSQL(g.description,true) + " as countyName";
			addDecodeTable("Zone",sql);
		}
	}

	/**
	 * Add a decode table and its query.
	 * @param tableName decode table
	 * @param sql query to explain the values within the decode table, ideally including
	 * an ORDER BY clause.
	**/
	public void addDecodeTable(String tableName, String sql) {
		decodeTablesAndQueries.put(tableName,sql);
	}

	/**
	 * Obtain the decode SQL for a table, substituting variables into the SQL as needed.
	 * This routine should be used instead of directly accessing decodeTablesAndQueries due
	 * to the logic for value replacements such as ##isLeapYear##.
	 * @param tableName table to decoded
	 * @return decode SQL or null
	**/
	public String getDecodeSQL(String tableName) {
		String sql = (String)decodeTablesAndQueries.get(tableName);
		if(sql != null && sql.length() > 0) {
			// Do replacements
			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			if(importer != null && importer.getImporterManager() != null
					&& importer.getImporterManager().runSpec != null) {
				replacements.put("##isLeapYear##",importer.getImporterManager().isLeapYear()?"1":"0");
			}
			sql = StringUtilities.doReplacements(sql,replacements);
		}
		return sql;
	}

	/**
	 * Get a list of the tables that affected by import, export, and clear.
	 * @return an ArrayList of String objects holding table names
	**/
	public ArrayList<String> getDataTableNames() {
		if(dataTableNames == null) {
			dataTableNames = new ArrayList<String>();
			for(int i=0;i<descriptor.length-1;i++) {
				if(descriptor[i].equalsIgnoreCase(BEGIN_TABLE)) {
					i++;
					dataTableNames.add(descriptor[i]);
				}
			}
		}
		return dataTableNames;
	}

	/**
	 * Add a filter to the list of filters.  This is an overridable routine for classes
	 * that wish to filter the filters.  If rejecting a filter, insert "" into the list
	 * of filters.
	 * @param filterNames all filter names seen so far
	 * @param filterName active filter
	**/
	public void addTemplateFilterName(ArrayList<String> filterNames, String filterName) {
		filterNames.add(filterName);
	}

	/** Event called when a template is being initiated **/
	public void onBeginTemplate() {
		// Nothing to do here
	}

	/**
	 * Called for each row in a template to accept or reject the combination.
	 * @param value array of objects for each column in the template
	 * @return true if the row should be written
	**/
	public boolean shouldWriteTemplateRow(Object[] values) {
		return true;
	}

	/**
	 * Alter the name of a filter during template creation.  Used, for instance,
	 * to build a template using all fuel types rather than just those in the
	 * runspec.
	 * @param tableName name of the current table.
	 * @param filterName name of the ImporterManager filter.
	 * @return the name of the ImporterManager filter to be used.  Never null, never blank.
	**/
	public String adjustTemplateFilterName(String tableName, String filterName) {
		return filterName;
	}

	/**
	 * Check the applicability of a filter during export of default data.
	 * @param tableName the current table.
	 * @param filterName name of the ImporterManager filter.
	 * @return true if the filter should be used
	**/
	public boolean shouldUseFilterForExport(String tableName, String filterName) {
		return true;
	}

	/**
	 * Create a template file (or files).
	 * @param tableName table that a template should be created for
	 * @param destinationFile file selected by the user to be created.  The file may already
	 * exist.
	 * @return true if the template was created successfully, false otherwise.
	**/
	public boolean createTemplate(String tableName, File destinationFile) {
		addAnyCustomDomainInformation();
		onBeginTemplate();
		TreeSetIgnoreCase decodeTablesToCreate = new TreeSetIgnoreCase();
		ArrayList<String> columnNames = new ArrayList<String>();
		ArrayList<String> filterNames = new ArrayList<String>();
		// Read the descriptor for the table to be exported
		for(int i=0;i<descriptor.length-1;i++) {
			if(!descriptor[i].equalsIgnoreCase(BEGIN_TABLE)
					|| !descriptor[i+1].equalsIgnoreCase(tableName)) {
				continue;
			}
			for(int j=i+2;j<descriptor.length-2;j+=3) {
				if(descriptor[j].equalsIgnoreCase(BEGIN_TABLE)) {
					break;
				}
				if(descriptor[j+0].startsWith("*")) {
					continue;
				}
				columnNames.add(descriptor[j+0]);
				if(descriptor[j+1].length() > 0) {
					decodeTablesToCreate.add(descriptor[j+1]);
				}
				addTemplateFilterName(filterNames,descriptor[j+2]);
			}
			break;
		}
		// Get the set of filter values to be written into the template
		ArrayList[] filterValues = new ArrayList[columnNames.size()];
		for(int i=0;i<filterNames.size();i++) {
			String filterName = (String)filterNames.get(i);
			filterName = adjustTemplateFilterName(tableName, filterName);
			filterValues[i] = importer.getImporterManager().getFilterValues(filterName);
		}

		CellFileWriter writer = null;
		try {
			// Create the primary destination file
			writer = new CellFileWriter(destinationFile,tableName);
			// Fill the primary destination file with the template column names
			for(Iterator<String> i=columnNames.iterator();i.hasNext();) {
				String columnName = (String)i.next();
				writer.writeTextCell(columnName);
			}
			writer.endRow();
			// Write records for all key combinations present in the filter.
			// Just write the filter keys, leaving other columns empty.
			writeFilterValues(writer,filterValues);
			// For each decode table to create, either create it as a new file or add it
			// as a new tab to the primary destination file if the file type is XLS.
			CommonNamesFilter filter = new CommonNamesFilter(importer.getImporterManager(),true);
			Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
			for(Iterator<String> i=decodeTablesToCreate.iterator();i.hasNext();) {
				String decodeTableName = (String)i.next();
				String sql = getDecodeSQL(decodeTableName);
				if(sql != null && sql.length() > 0) {
					writer.startTab(decodeTableName);
					writer.writeSQLResults(db,sql,filter);
				}
			}
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to create template for " + tableName);
			return false;
		} finally {
			if(writer != null) {
				writer.close();
			}
		}
	}

	/**
	 * Write all combinations of the filterValues (the non-null entries that is)
	 * into a template file.  Columns without filter values are left empty.
	 * @param writer file to be written
	 * @param filterValues array of ArrayList objects, each holding any type of object
	 * that can be written with CellFileWriter.writeCell(Object).
	 * @throws Exception if anything goes wrong
	**/
	private void writeFilterValues(CellFileWriter writer, ArrayList[] filterValues)
			throws Exception {
		int[] cursors = new int[filterValues.length]; // default to 0's
		Object[] currentValues = new Object[filterValues.length]; // default to null's
		// Setup initial values
		for(int i=0;i<filterValues.length;i++) {
			if(filterValues[i] != null && filterValues[i].size() > 0) {
				currentValues[i] = filterValues[i].get(0);
			}
		}
		// Loop until done
		while(true) {
			if(shouldWriteTemplateRow(currentValues)) {
				// Write the current values
				for(int i=0;i<currentValues.length;i++) {
					writer.writeCell(currentValues[i]); // ok if null
				}
				writer.endRow();
			}
			// Advance the cursors and current values
			int index = cursors.length-1;
			while(index >= 0) {
				if(filterValues[index] == null || filterValues[index].size() == 0) {
					index--;
					continue;
				}
				if(cursors[index] >= filterValues[index].size()-1) {
					cursors[index] = 0;
					currentValues[index] = filterValues[index].get(0);
					index--;
					continue;
				}
				cursors[index]++;
				currentValues[index] = filterValues[index].get(cursors[index]);
				break;
			}
			// When done, just return.
			if(index < 0) {
				return;
			}
		}
	}

	/**
	 * Check the import source for errors and optionally store its data.
	 * Record the successful import in the database's log.
	 * @param db database to receive the imported data
	 * @param shouldCommit true if the imported data should actually be saved,
	 * false if it should only be scanned for possible issues.
	 * @param messages information, warnings, and errors to be shown to the user
	 * @return true if the data was imported successfully.
	**/
	public boolean doImport(Connection db, boolean shouldCommit, ArrayList<String> messages) {
		addAnyCustomDomainInformation();
		Logger.log(LogMessageCategory.INFO,"doImport for " + importer.getClass().getName());
		messages.clear();
		CellFileReader reader = null;
		String tableName = "";
		try {
			importer.getImporterManager().fillCountyDomainTables(db);
			if(importDataProvider instanceof IProvider2) {
				((IProvider2)importDataProvider).onImportBegin();
			}

			// Import each table, noting table and column names from the descriptor array
			for(int i=0;i<descriptor.length-1;i++) {
				if(!descriptor[i].equalsIgnoreCase(BEGIN_TABLE)) {
					continue;
				}
				tableName = descriptor[i+1];
				ArrayList<String> columnNames = new ArrayList<String>();
				ArrayList<String> filterTypes = new ArrayList<String>();
				TreeSetIgnoreCase columnsToIgnore = new TreeSetIgnoreCase();
				for(int j=i+2;j<descriptor.length-2;j+=3) {
					if(descriptor[j].equalsIgnoreCase(BEGIN_TABLE)) {
						break;
					}
					if(descriptor[j+0].startsWith("*")) {
						columnsToIgnore.add(descriptor[j+0].substring(1)); // get the name, skipping the leading *
					} else {
						columnNames.add(descriptor[j+0]);
						filterTypes.add(descriptor[j+2]);
					}
				}
				String fileName = importDataProvider.getTableFileSource(tableName);
				if(fileName != null && fileName.equals("\rSKIP\r")) {
					// Nothing to do here
				} else if(fileName != null && fileName.length() > 0) {
					String worksheetName = importDataProvider.getTableWorksheetSource(tableName);
					reader = new CellFileReader(new File(fileName),worksheetName);
					if(!doImport(db,shouldCommit,messages,reader,tableName,columnNames,filterTypes,columnsToIgnore)) {
						return false;
					}
					reader.close();
					reader = null;
					foundAnyTables = true;
					if (shouldCommit){
						messages.add(tableName + " imported.");
					}
				} else {
					messages.add(tableName + " not imported, no file specified.");
				}
			}
			// Try to run a script that will finish the import, issuing warnings as required
			String scriptFileName = getScriptFileName(importer);
			runPostImportScript(db,scriptFileName,messages);
			importer.getImporterManager().addDatabaseToRunSpec(true);
			if(foundAnyTables) {
				messages.add("Import complete.");
			} else {
				messages.add("Import complete, but no files were specified.");
			}
			return true;
		} catch(Exception e) {
			String t = "Unable to import data into " + tableName + ": " + e.getMessage();
			messages.add(t);
			Logger.logError(e,t);
			return false;
		} finally {
			if(reader != null) {
				reader.close();
				reader = null;
			}
		}
	}

	/**
	 * Check the import source for errors and optionally store its data.
	 * Record the successful import in the database's log.
	 * @param db database to receive the imported data
	 * @param shouldCommit true if the imported data should actually be saved,
	 * false if it should only be scanned for possible issues.
	 * @param messages information, warnings, and errors to be shown to the user
	 * @param reader CellFileReader setup to read from a selected file and worksheet
	 * @param tableName table to receive the imported data
	 * @param columnNames columns to be imported
	 * @param filterTypes ImporterManager.FILTER_XXX type, if any, for each column.
	 * Contains null or "" entries for those columns that do not use filtering.
	 * @param columnsToIgnore names of columns to be skipped during import, may be empty
	 * @return true if the data was imported successfully.
	**/
	private boolean doImport(Connection db, boolean shouldCommit, ArrayList<String> messages,
			CellFileReader reader, String tableName, ArrayList<String> columnNames,
			ArrayList<String> filterTypes, TreeSetIgnoreCase columnsToIgnore) throws Exception {
		boolean success = true;
		File tempFile = new File("doImportTemp.txt");
		if(tempFile.exists()) {
			try {
				tempFile.delete();
			} catch(Exception e) {
				// Nothing to do here
			}
			if(tempFile.exists()) {
				throw new SQLException("Unable to delete temporary file used by doImport");
			}
		}
		BufferedWriter tempWriter = null;
		// Create the destination table if it doesn't already exist
		String createTableSQL = (String)
				DatabaseConnectionManager.defaultDatabaseCreateTableStatements.get(tableName.toLowerCase());
		if(createTableSQL != null && createTableSQL.length() > 0) {
			SQLRunner.executeSQL(db,createTableSQL);
		}
		int[] dataTypes = new int[columnNames.size()]; // 0:string, 1:int, 2:float, 3:double
		boolean[] isNullable = new boolean[columnNames.size()];
		int[] stringLengths = new int[columnNames.size()]; // max length of string columns
		Statement statement = null;
		ResultSet rs = null;
		String sql = "";
		String columnNamesText = "";
		try {
			if(shouldCommit) {
				tempWriter = new BufferedWriter(new FileWriter(tempFile));
			}
			// Learn the column data types for the target table
			columnNamesText = (String)columnNames.get(0);
			for(int i=1;i<columnNames.size();i++) {
				columnNamesText += "," + (String)columnNames.get(i);
			}
			sql = "select " + columnNamesText + " from " + tableName + " limit 1";
			statement = db.createStatement();
			rs = SQLRunner.executeQuery(statement,sql);
			ResultSetMetaData metaData = rs.getMetaData();
			for(int i=0;i<dataTypes.length;i++) {
				// Get the data type for the column
				int t = metaData.getColumnType(i+1);
				switch(t) {
					case java.sql.Types.TINYINT:
					case java.sql.Types.BIGINT:
					case java.sql.Types.INTEGER:
					case java.sql.Types.SMALLINT:
						dataTypes[i] = 1; // int
						break;
					case java.sql.Types.FLOAT:
						dataTypes[i] = 2; // float
						break;
					case java.sql.Types.DOUBLE:
					case java.sql.Types.NUMERIC:
					case java.sql.Types.REAL:
						dataTypes[i] = 3; // double
						break;
					default:
						dataTypes[i] = 0; // string
						stringLengths[i] = metaData.getColumnDisplaySize(i+1);
						break;
				}
				isNullable[i] = metaData.isNullable(i+1) == 0? false : true;
			}
			// Find the column headers.  They should be the first non-blank, non-skippable
			// row in the file.
			while(!reader.isEndOfData() && reader.shouldSkipLine()) {
				reader.endRow();
			}
			if(reader.isEndOfData()) {
				messages.add("ERROR: No header row found for table " + tableName);
				return false;
			}
			ArrayList<String> columnNamesInFile = new ArrayList<String>();
			int consecutiveBlankColumnCount = 0;
			while(true) {
				String t = reader.readStringCell();
				if(t == null) {
					break;
				}
				t = t.trim();
				if(columnsToIgnore.contains(t)) { // treat ignored columns as blank columns
					t = "";
				}
				columnNamesInFile.add(t);
				if(t.length() <= 0) {
					consecutiveBlankColumnCount++;
					if(consecutiveBlankColumnCount >= 10) {
						break;
					}
				} else {
					consecutiveBlankColumnCount = 0;
				}
			}
			reader.endRow(); // move past the header row
			// Setup the mapping between the database columns and those in the file
			ImporterManager manager = importer.getImporterManager();
			int[] sqlColumnIndexByFileIndex = new int[columnNamesInFile.size()];
			int[] fileColumnIndexBySQLIndex = new int[dataTypes.length];
			for(int i=0;i<sqlColumnIndexByFileIndex.length;i++) {
				sqlColumnIndexByFileIndex[i] = -1;
			}
			for(int i=0;i<fileColumnIndexBySQLIndex.length;i++) {
				fileColumnIndexBySQLIndex[i] = -1;
			}
			int si;
			for(int fi=0;fi<columnNamesInFile.size();fi++) {
				String fileColumnName = (String)columnNamesInFile.get(fi);
				for(si=0;si<columnNames.size();si++) {
					String sqlColumnName = (String)columnNames.get(si);
					if(fileColumnName.equalsIgnoreCase(sqlColumnName)) {
						sqlColumnIndexByFileIndex[fi] = si;
						fileColumnIndexBySQLIndex[si] = fi;
						// Tell the reader about any wildcards available for this column
						manager.setupWildcards(sqlColumnName,reader,fi);
						break;
					}
				}
			}
			for(si=0;si<columnNames.size();si++) {
				String sqlColumnName = (String)columnNames.get(si);
				if(fileColumnIndexBySQLIndex[si] < 0) {
					messages.add("ERROR: Missing column " + sqlColumnName
							+ " for " + tableName + " table.");
					return false;
				}
			}
			String[] rowData = new String[columnNames.size()];
			Object[] rowObjects = new Object[columnNames.size()];
			int missingDataCount = 0;
			int filteredRowCount = 0;
			while(!reader.isEndOfData()) {
				if(reader.shouldSkipLine()) {
					reader.endRow();
					continue;
				}
				for(int i=0;i<rowData.length;i++) {
					rowData[i] = null;
					rowObjects[i] = null;
				}
				for(int fi=0;fi<columnNamesInFile.size();fi++) {
					si = sqlColumnIndexByFileIndex[fi];
					if(si < 0) {
						reader.skipCell();
						continue;
					}
					switch(dataTypes[si]) {
						default:
						case 0: // string
							rowData[si] = reader.readStringCell();
							if(stringLengths[si] > 0 && rowData[si].length() > stringLengths[si]) {
								rowData[si] = rowData[si].substring(0,stringLengths[si]);
							} else if(rowData[si] == null) {
								rowData[si] = "";
							}
							rowObjects[si] = rowData[si];
							break;
						case 1: // int
							rowObjects[si] = reader.readIntegerCell();
							if(rowObjects[si] != null) {
								rowData[si] = rowObjects[si].toString();
							}
							break;
						case 2: // float
						case 3: // double
							rowObjects[si] = reader.readDoubleCell();
							if(rowObjects[si] != null) {
								rowData[si] = rowObjects[si].toString();
							}
							break;
					}
					if(rowObjects[si] == null && !isNullable[si]) {
						if(missingDataCount <= 2) {
							// First few times, give row and column name
							messages.add("ERROR: Missing required data for column "
									+ (String)columnNames.get(si)
									+ ", line " + reader.getLineNumber());
							success = false;
						} else if(missingDataCount == 3) {
							// Several messages have been issued, give generic message
							messages.add("ERROR: Additional required data is missing");
						}
						missingDataCount++;
						continue;
					}
				}
				reader.endRow();
				if(!success) {
					// Don't check the filters if unsuccessful in getting data.
					continue;
				}
				// Check the filter for each column and each row, issuing a warning
				// but still using the row.
				ImporterManager.Message tempMessage = new ImporterManager.Message();
				for(int i=0;i<rowData.length;i++) {
					String filterType = (String)filterTypes.get(i);
					if(filterType == null || filterType.length() <= 0) {
						continue;
					}
					if(rowObjects[i] != null && !manager.doesInclude(filterType,rowObjects[i],tempMessage)) {
						if(filteredRowCount <= 2) {
							String t;
							if(tempMessage.hasMessage()) {
								t = "WARNING: " + (String)columnNames.get(i)
										+ " " + tempMessage.text;
							} else {
								t = "WARNING: " + (String)columnNames.get(i)
										+ " " + rowData[i] + " is not used.";
							}
							for(Iterator j=messages.iterator();j.hasNext();) {
								String tm = (String)j.next();
								if(tm.equalsIgnoreCase(t)) {
									t = null;
									break;
								}
							}
							if(t == null) {
								filteredRowCount--;
							} else {
								messages.add(t);
							}
						} else if(filteredRowCount == 3) {
							messages.add(
									"WARNING: Additional data is not used.");
						}
						filteredRowCount++;
					}
				}
				if(!success || !shouldCommit) {
					// Don't bother writing to a temporary file if not successful
					// or the data shouldn't be committed.
					continue;
				}
				// Write each line to a text file, separating each field by \t and ending
				// each line with \n.  This makes the file compatible with MySQL's
				// LOAD DATA INFILE default settings.
				for(int i=0;i<rowData.length;i++) {
					if(i > 0) {
						tempWriter.write("\t");
					}
					if(rowData[i] != null) {
						tempWriter.write(rowData[i]);
					} else {
						tempWriter.write("\\N");
					}
				}
				tempWriter.write("\n");
			}
			if(success && shouldCommit) {
				// Close the writer to the temporary file
				tempWriter.close();
				// Load the data
				sql = "LOAD DATA INFILE '"
						+ tempFile.getCanonicalPath().replace('\\','/') + "' REPLACE INTO TABLE "
						+ tableName + " (" + columnNamesText + ")";
				SQLRunner.executeSQL(db,sql);
				// After the import, analyze the table for performance reasons
				sql = "analyze table " + tableName;
				SQLRunner.executeSQL(db,sql);
				// Record the successful import in the database's log.
				importer.getImporterManager().log(importer,"Filled " + tableName + " table",
						importer.getDescription());
			}
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
			if(tempWriter != null) {
				try {
					tempWriter.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				tempWriter = null;
			}
			if(tempFile != null && tempFile.exists()) {
				try {
					tempFile.delete();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
		return success;
	}

	/**
	 * Export data into one or more files.  If an XLS file is specified, each exported
	 * table will get its own worksheet.
	 * @param type which type of MOVES database holds the exported data.  Typically, this
	 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
	 * being used.
	 * @param db database holding the data to be exported
	 * @param file File to receive the exported data.  If multiple tables are required,
	 * subsequent files will use names similar to this one's name.
	 * @return -1 if any error occured, 0 if no errors but no data, +1 for no errors and data
	**/
	public int doExport(MOVESDatabaseType type, Connection db, File file) {
		addAnyCustomDomainInformation();
		CellFileWriter writer = null;
		boolean wereAnyPartsEmpty = true;
		boolean shouldCleanupAlternateSQL = false;
		TreeSetIgnoreCase tablesToDrop = new TreeSetIgnoreCase();
		SQLRunner.Query query = new SQLRunner.Query();

		try {
			TreeSetIgnoreCase decodeTablesToCreate = new TreeSetIgnoreCase();
			for(int i=0;i<descriptor.length-1;i++) {
				if(!descriptor[i].equalsIgnoreCase(BEGIN_TABLE)) {
					continue;
				}
				String tableName = descriptor[i+1];
				CommonNamesFilter filter =
						new CommonNamesFilter(importer.getImporterManager(),false);
				String sql = "";
				String orderBy = "";
				for(i=i+2;i<descriptor.length-2;i+=3) {
					if(descriptor[i].equalsIgnoreCase(BEGIN_TABLE)) {
						i--; // backup so this entry is found at the start of the next iteration
						break;
					}
					if(descriptor[i+0].startsWith("*")) { // skip optional columns
						continue;
					}
					if(sql.length() > 0) {
						sql += ",";
					}
					sql += descriptor[i+0];
					if(descriptor[i+2].length() > 0 && shouldUseFilterForExport(tableName,descriptor[i+2])) {
						// Add the filter types for the columns
						filter.setFilter(descriptor[i+0],descriptor[i+2]);
						// Order by the filtered columns
						if(orderBy.length() > 0) {
							orderBy += ",";
						}
						orderBy += descriptor[i+0];
					}
				}
				if(writer == null) {
					writer = new CellFileWriter(file,tableName);
				} else {
					writer.startTab(tableName);
				}
				// If the table doesn't exist, create it and add it to tablesToDrop
				try {
					query.open(db,"describe " + tableName);
				} catch(Exception e) {
					// This happens when the table doesn't exist, so create it
					String createTableSQL = (String)
							DatabaseConnectionManager.defaultDatabaseCreateTableStatements.get(tableName.toLowerCase());
					if(createTableSQL != null && createTableSQL.length() > 0) {
						SQLRunner.executeSQL(db,createTableSQL);
					}
					tablesToDrop.add(tableName);
				} finally {
					query.onFinally();
				}
				// Do the query now that the table definitely exists
				sql = "select " + sql + " from " + tableName;
				if(orderBy.length() > 0) {
					sql += " order by " + orderBy;
				}
				String alternateSQL = null;
				if(importDataProvider != null) {
					alternateSQL = importDataProvider.getAlternateExportSQL(type,db,tableName);
					if(alternateSQL != null && alternateSQL.length() > 0) {
						sql = alternateSQL;
						shouldCleanupAlternateSQL = true;
					}
				}
				if(writer.writeSQLResults(db,sql,filter) > 0) {
					wereAnyPartsEmpty = false;
				}
				if(shouldCleanupAlternateSQL) {
					importDataProvider.cleanupAlternateExportSQL(type,db,tableName);
				}
				// For XLS files, create the decode tabs
				if(CellFile.formatAllowsWorksheets(writer.getFileType())) {
					// Read the descriptor for the table to be exported
					for(int j=0;j<descriptor.length-1;j++) {
						if(!descriptor[j].equalsIgnoreCase(BEGIN_TABLE)
								|| !descriptor[j+1].equalsIgnoreCase(tableName)) {
							continue;
						}
						for(int k=j+2;k<descriptor.length-2;k+=3) {
							if(descriptor[k].equalsIgnoreCase(BEGIN_TABLE)) {
								break;
							}
							if(descriptor[k+1].length() > 0) {
								decodeTablesToCreate.add(descriptor[k+1]);
							}
						}
						break;
					}
				}
			}

			// For each decode table to create, either create it as a new file or add it
			// as a new tab to the primary destination file if the file type is XLS.
			CommonNamesFilter filter = new CommonNamesFilter(importer.getImporterManager(),true);
			Connection defaultDb
					= DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
			for(Iterator i=decodeTablesToCreate.iterator();i.hasNext();) {
				String decodeTableName = (String)i.next();
				String sql = getDecodeSQL(decodeTableName);
				if(sql != null && sql.length() > 0) {
					writer.startTab(decodeTableName);
					try {
						writer.writeSQLResults(defaultDb,sql,filter);
					} catch(SQLException e) {
						Logger.log(LogMessageCategory.INFO,"SQL failed: " + sql);
						throw e;
					}
				}
			}

			if(wereAnyPartsEmpty) {
				return 0;
			}
			return +1;
		} catch(Exception e) {
			Logger.logError(e,"Unable to export data");
			return -1;
		} finally {
			if(writer != null) {
				writer.close();
			}
			for(Iterator i=tablesToDrop.iterator();i.hasNext();) {
				String tableName = (String)i.next();
				try {
					SQLRunner.executeSQL(db,"drop table if exists " + tableName);
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
	}

	/**
	 * Clear data from tables.
	 * @param db database holding the data to be cleared
	 * @return true if the data was cleared
	**/
	public boolean doClear(Connection db) {
		getDataTableNames(); // make sure dataTableNames is populated
		String sql = "";
		try {
			for(Iterator<String> i=dataTableNames.iterator();i.hasNext();) {
				String tableName = (String)i.next();
				sql = "truncate table " + tableName;
				try{
					SQLRunner.executeSQL(db,sql);
				}catch(Exception e){
 					if( e.toString().toLowerCase().contains("doesn't exist") ){
 						continue;
					}else{
						throw e;
				 	}
				}
			}
			return true;
		} catch(Exception e) {
 			Logger.logError(e,"Unable to clear importer data");
			return false;
		}
	}

	/**
	 * Generate the expected name of a SQL script file associated with an importer.
	 * A file of the form "database/import/SCRIPTNAME.sql" is preferred, where
	 * IImporter.getScriptName() is called to obtain the SCRIPTNAME portion.
	 * If the preferred file does not exist, a file of the form "database/SCRIPTNAMEImporter.sql"
	 * is used, even if it does not exist.
	 * @param importer the importer to be examined
	 * @return name of the SQL script file to be used, including any relative path
	**/
	public static String getScriptFileName(IImporter importer) {
		String t = importer.getScriptName();
		String baseFileName = "";
		for(int i=0;i<t.length();i++) {
			char c = t.charAt(i);
			if(Character.isLetterOrDigit(c)) {
				baseFileName += c;
			}
		}

		String scriptFileName = "database/import/" + baseFileName + ".sql";
		if(new File(scriptFileName).exists()) {
			return scriptFileName;
		}

		scriptFileName = "database/" + baseFileName + "Importer.sql";
		return scriptFileName;
	}

	/**
	 * Attempt to run an SQL script to both finish an import as well as validate its data.
	 * To return messages, the script must fill a table named "importTempMessages" which
	 * has only one column, "message" which is varchar(1000).  The script can know the name
	 * of the current default database by referencing ##defaultDatabase## where a constant
	 * database name would normally go.
	 * @param db database holding the new data
	 * @param scriptFileName full path and name of the script to be run
	 * @param messages messages to be shown to the user about the data checks
	**/
	public void runPostImportScript(Connection db, String scriptFileName,
			ArrayList<String> messages) {
		String defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
		File scriptFile = new File(scriptFileName);
		String sql = "";
		Statement statement = null;
		ResultSet rs = null;
		try {
			if(!scriptFile.exists()) {
				return;
			}
			sql = "create table if not exists importTempMessages "
					+ "( message varchar(1000) not null )";
			SQLRunner.executeSQL(db,sql);
			sql = "truncate table importTempMessages";
			SQLRunner.executeSQL(db,sql);

			// Run the script, substituting the defaultDatabaseName as ##defaultDatabase##
			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			replacements.put("##defaultDatabase##",defaultDatabaseName);
			replacements.put("##mode##","0"); // 0 is the mode after importing
			fillStandardReplacements(importer,replacements);
			try {
				DatabaseUtilities.executeScript(db,scriptFile,replacements);
			} catch(Exception e) {
				messages.add("ERROR: Unable to execute import validation script");
				Logger.logError(e,"Unable to execute script " + scriptFileName);
			}

			// Retrieve the results from importTempMessages
			sql = "select message from importTempMessages";
			statement = db.createStatement();
			rs = SQLRunner.executeQuery(db,sql);
			TreeSetIgnoreCase messagesAlreadySeen = new TreeSetIgnoreCase();
			while(rs.next()) {
				String m = rs.getString(1);
				if(m != null && m.length() > 0) {
					if(!messagesAlreadySeen.contains(m)) {
						messagesAlreadySeen.add(m);
						messages.add(m);
					}
				}
			}
			rs.close();
			rs = null;
			statement.close();
			statement = null;

			sql = "drop table importTempMessages";
			SQLRunner.executeSQL(db,sql);
		} catch(Exception e) {
			// Nothing to do here
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
	}

	/**
	 * Generate standard replacement text for use with importer scripts.  Such items include
	 * the set of years in the RunSpec as a CSV list.
	 * @param importer importer to examine
	 * @param replacments map to be populated
	**/
	public static void fillStandardReplacements(IImporter importer,
			TreeMapIgnoreCase replacements) {
		ImporterManager manager = importer.getImporterManager();
		String[] pairs = {
			ImporterManager.FILTER_YEAR, "##yearIDs##",
			ImporterManager.FILTER_FUEL, "##fuelTypeIDs##",
			ImporterManager.FILTER_HOURDAY, "##hourDayIDs##",
			ImporterManager.FILTER_HOUR, "##hourIDs##",
			ImporterManager.FILTER_DAY, "##dayIDs##",
			ImporterManager.FILTER_MONTH, "##monthIDs##",
			ImporterManager.FILTER_MONTH_GROUP, "##monthGroupIDs##",
			ImporterManager.FILTER_SOURCE, "##sourceTypeIDs##",
			ImporterManager.FILTER_ZONE, "##zoneIDs##",
			ImporterManager.FILTER_COUNTY, "##countyIDs##",
			ImporterManager.FILTER_STATE, "##stateIDs##",
			ImporterManager.FILTER_FUEL_SUBTYPE, "##fuelSubtypeIDs##",
			ImporterManager.FILTER_ROAD_TYPE, "##roadTypeIDs##",
			ImporterManager.FILTER_HPMS_VTYPE, "##hpmsVtypeIDs##",
			ImporterManager.FILTER_POLPROCESSID, "##polProcessIDs##",
			ImporterManager.FILTER_SOURCEFUELTYPE, "##sourceFuelTypeIDs##",
			ImporterManager.FILTER_FUEL_REGION, "##regionIDs##",
			ImporterManager.FILTER_FUEL_YEAR, "##fuelYearIDs##",
			ImporterManager.FILTER_PROCESS, "##processIDs##"
		};
		for(int i=0;i<pairs.length;i+=2) {
			String csv = manager.getFilterValuesCSV(pairs[i+0]);
			if(csv == null || csv.length() <= 0) {
				csv = "";
			}
			replacements.put(pairs[i+1],csv);
		}

		String scale = "0";
		if(manager.isNonroad()) {
			scale = "3";
		} else if(manager.isProject()) {
			scale = "2";
		} else if(manager.isCounty()) {
			scale = "1";
		}
		replacements.put("##scale##", scale);

		replacements.put("##USE_FUELUSAGEFRACTION##",CompilationFlags.USE_FUELUSAGEFRACTION?"1":"0");

		String rate = manager.isRate()? "1" : "0";
		replacements.put("##rate##", rate);
	}

	/**
	 * Attempt to run an SQL script to validate the state of imported data.
	 * To return messages, the script must fill a table named "importTempMessages" which
	 * has only one column, "message" which is varchar(1000).  The script can know the name
	 * of the current default database by referencing ##defaultDatabase## where a constant
	 * database name would normally go.
	 * @param db database holding the new data
	 * @param scriptFileName full path and name of the script to be run
	 * @param messages messages to be shown to the user about the data checks
	 * @param mode 0 if run after importing, 1 if run to check for tab status
	**/
	public static void runScript(Connection db, IImporter importer, ArrayList<String> messages,
			int mode) {
		String scriptFileName = getScriptFileName(importer);
		runScript(db,importer,messages,mode,scriptFileName);
	}

	/**
	 * Attempt to run an SQL script to validate the state of imported data.
	 * To return messages, the script must fill a table named "importTempMessages" which
	 * has only one column, "message" which is varchar(1000).  The script can know the name
	 * of the current default database by referencing ##defaultDatabase## where a constant
	 * database name would normally go.
	 * @param db database holding the new data
	 * @param scriptFileName full path and name of the script to be run
	 * @param messages messages to be shown to the user about the data checks
	 * @param mode 0 if run after importing, 1 if run to check for tab status
	 * @param scriptFileName file to be run
	**/
	public static void runScript(Connection db, IImporter importer, ArrayList<String> messages,
			int mode, String scriptFileName) {
		String defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
		File scriptFile = new File(scriptFileName);
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			if(!scriptFile.exists()) {
				return;
			}
			sql = "create table if not exists importTempMessages "
					+ "( message varchar(1000) not null )";
			SQLRunner.executeSQL(db,sql);
			sql = "truncate table importTempMessages";
			SQLRunner.executeSQL(db,sql);

			// Run the script, substituting the defaultDatabaseName as ##defaultDatabase##
			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			replacements.put("##defaultDatabase##",defaultDatabaseName);
			replacements.put("##mode##",""+mode);
			fillStandardReplacements(importer,replacements);
			try {
				DatabaseUtilities.executeScript(db,scriptFile,replacements);
			} catch(Exception e) {
				messages.add("ERROR: Unable to execute importer script");
				Logger.logError(e,"Unable to execute script " + scriptFileName);
			}

			if(messages != null) {
				// Retrieve the results from importTempMessages
				sql = "select message from importTempMessages";
				query.open(db,sql);
				TreeSetIgnoreCase messagesAlreadySeen = new TreeSetIgnoreCase();
				while(query.rs.next()) {
					String m = query.rs.getString(1);
					if(m != null && m.length() > 0) {
						if(!messagesAlreadySeen.contains(m)) {
							messagesAlreadySeen.add(m);
							messages.add(m);
							importer.addQualityMessage(m);
						}
					}
				}
				query.close();
			}

			sql = "drop table importTempMessages";
			SQLRunner.executeSQL(db,sql);
		} catch(Exception e) {
			// Nothing to do here
		} finally {
			query.onFinally();
		}
	}
}
