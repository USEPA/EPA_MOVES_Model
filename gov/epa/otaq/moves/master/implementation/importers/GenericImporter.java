/**************************************************************************************************
 * @(#)GenericImporter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;

import java.io.*;
import java.sql.*;
import java.util.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;

/**
 * Importer for any table.
 * 
 * @author		Wesley Faler
 * @author		Don Smith
 * @version		2014-04-22
**/
public class GenericImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object **/
	TableFileLinkagePart part;
	/** Set of tables to be offered **/
	TreeSetIgnoreCase tables = new TreeSetIgnoreCase();

	/** Constant used to denote the lack of a table selection **/
	private static final String emptyPrimaryTableName = "(none)";

	/**
	 * Name of the primary table handled by the importer.
	 * Note the MOVES database naming convention.
	**/
	String primaryTableName = emptyPrimaryTableName;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
	};

	/** Class for editing the data source **/
	class PartProvider implements TableFileLinkagePart.IProvider, TableFileLinkagePart.ITableProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return null; // force use of the table provider aspect
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(primaryTableName,destinationFile);
		}

		/**
		 * Get the table names to be offered
		 * @return the set of table names
		**/
		public TreeSetIgnoreCase getTableNames() {
			return tables;
		}

		/**
		 * Event handler for notification of table name selection,
		 * either via the GUI or via XML.
		 * @param tableName table that has been selected
		**/
		public void onTableNameChanged(String tableName) {
			primaryTableName = tableName;
			// Build dataTableDescriptor based on primaryTableName.
			ArrayList<String> columnNames = getColumnNames(primaryTableName);
			String[] descriptor = new String[2+columnNames.size()*3];
			descriptor[0] = BasicDataHandler.BEGIN_TABLE;
			descriptor[1] = primaryTableName;
			for(int i=0;i<columnNames.size();i++) {
				String columnName = columnNames.get(i);
				String referenceTable = StringUtilities.safeGetString(getReferenceTable(primaryTableName,columnName));
				String filter = StringUtilities.safeGetString(getFilter(primaryTableName,columnName));
				descriptor[2+i*3+0] = columnName;
				descriptor[2+i*3+1] = referenceTable;
				descriptor[2+i*3+2] = filter;
			}

			// Change the dataTableDescriptor being used by basicDataHandler
			// to that created above.
			basicDataHandler.changeDescriptor(descriptor);
		}

		/**
		 * Get the names of the columns that within a table.  The table may be in either
		 * the DEFAULT or NRDEFAULT databases.
		 * @param tableName table to be scanned
		 * @return names of the columns found within the table
		**/
		private ArrayList<String> getColumnNames(String tableName) {
			ArrayList<String> columnNames = new ArrayList<String>();
			Connection defaultDB = null;
			Connection nrDefaultDB = null;
			String sql = "";
			SQLRunner.Query query = new SQLRunner.Query();
			boolean foundColumnInNRDefaultDB = false;

			try {
				defaultDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
				if( defaultDB!=null ){
					sql = "describe "+tableName;
					query.open(defaultDB,sql);
					while(query.rs.next()) {
						String columnName = query.rs.getString(1);
						columnNames.add(columnName);
					}
					query.close();
				}
			} catch(Exception e) {
				/*
				if(CompilationFlags.USE_NONROAD) {
					try {
						nrDefaultDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.NRDEFAULT);
						if( nrDefaultDB!=null ){
							sql = "describe "+tableName;
							query.open(nrDefaultDB,sql);
							while(query.rs.next()) {
								String columnName = query.rs.getString(1);
								columnNames.add(columnName);
							}
							query.close();
							foundColumnInNRDefaultDB = true;
						}
					} catch(Exception e2) {
						e = null; // ensure only one error message is shown
						Logger.logError(e2,"Unable to find columns for table " + tableName);
					} finally {
						query.onFinally();
						if(nrDefaultDB != null) {
							DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.NRDEFAULT,nrDefaultDB);
							nrDefaultDB = null;
						}
					}
				}
				*/
				if( e != null && !foundColumnInNRDefaultDB ){
					Logger.logError(e,"Unable to find columns for table " + tableName);
				}
			} finally {
				query.onFinally();
				if(defaultDB != null) {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,defaultDB);
					defaultDB = null;
				}
			}

			return columnNames;
		}

		/**
		 * Get the name of a reference table given a table and column.
		 * @param tableName input table
		 * @param columnName input column
		 * @return name of table containing reference data, such a the Zone table when
		 * presented with a zoneID column.  May be null or empty.
		**/
		private String getReferenceTable(String tableName, String columnName) {
			boolean isNonRoad = StringUtilities.substring(tableName,0,2).equalsIgnoreCase("NR");

			// Handle special cases by table
			// (none likely)

			// Handle standard column name mappings independent of table.
			// Duplicate names are allowed, such as ageID which can refer to either ageCategory
			// or NRAgeCategory.
			// NOTE: Place NonRoad-specific pairings first.  This lets NonRoad tables refer
			// ----- preferentially to NonRoad tables (such as with ageID) but still accept bindings
			// to tables in the main default database (such as fuelTypeID).
			String[] basicPairs = {
				"ageID", "NRAgeCategory",
				"ageID", "ageCategory",
				"countyID", "County",
				"stateID", "State",
				"zoneID", "Zone",
				"opModeID", "OperatingMode",
				"monthGroupID", "MonthGroupOfAnyYear",
				"monthID", "MonthOfAnyYear",
				"fuelFormulationID", "FuelFormulation",
				"fuelSubTypeID", "FuelSubtype",
				"fuelTypeID", "FuelType",
				"fuelYearID", "FuelSupplyYear",
				"countyID", "CountyState",
				"HPMSVtypeID", "HPMSVType",
				"roadTypeID", "RoadType",
				"avgSpeedBinID", "AvgSpeedBin",
				"sourceTypeID", "NRSourceUseType",
				"sourceTypeID", "SourceUseType",
				"hourID", "HourOfAnyDay",
				"hourDayID", "HourDay",
				"polProcessID", "PollutantProcessAssoc",
				"polProcessID", "IMPollutantProcessAssoc",
				"inspectFreq", "IMInspectFreq",
				"testStandardsID", "IMTestStandards",
				"engTechID", "EngineTech",		
				"dayID", "DayOfAnyWeek",

				"NREquipTypeID", "NREquipmentType",
				"growthPatternID", "NRGrowthPattern",
				"NRHPRangeBinID", "NRHPRangeBin",
				"SCC", "NRSCC",
				"sectorID", "Sector",
				"surrogateID", "NRSurrogate"
			};
			for(int i=0;i<basicPairs.length;i+=2) {
				if(basicPairs[i+0].equalsIgnoreCase(columnName)) {
					if(!isNonRoad) { // If the input is an On Road table, do not respond with a NonRoad table.
						if(basicPairs[i+1].startsWith("NR") || basicPairs[i+1].startsWith("nr")) {
							continue;
						}
					}
					if(tableName.equalsIgnoreCase(basicPairs[i+1])) {
						// Don't allow a reference table with same name as the input table
						return null;
					}
					return basicPairs[i+1];
				}
			}
			return null;
		}

		/**
		 * Get the name of an ImporterManager filter given a table and column.
		 * @param tableName input table
		 * @param columnName input column
		 * @return name of an ImporterManager filter.  May be null or empty.
		**/
		private String getFilter(String tableName, String columnName) {
			// Handle special cases by table
			// (none)

			// Handle standard column name mappings independent of table
			String[] basicPairs = {
				"fuelTypeID", ImporterManager.FILTER_FUEL,
				"fuelYearID", ImporterManager.FILTER_FUEL_YEAR,
				"hourDayID", ImporterManager.FILTER_HOURDAY,
				"hourID", ImporterManager.FILTER_HOUR,
				"dayID", ImporterManager.FILTER_DAY,
				"monthID", ImporterManager.FILTER_MONTH,
				"monthGroupID", ImporterManager.FILTER_MONTH_GROUP,
				"yearID", ImporterManager.FILTER_YEAR,
				"sourceTypeID", ImporterManager.FILTER_SOURCE,
				"zoneID", ImporterManager.FILTER_ZONE,
				"countyID", ImporterManager.FILTER_COUNTY,
				"stateID", ImporterManager.FILTER_STATE,
				"fuelFormulationID", ImporterManager.FILTER_FUEL_FORMULATION,
				"fuelSubtypeID", ImporterManager.FILTER_FUEL_SUBTYPE,
				"roadTypeID", ImporterManager.FILTER_ROAD_TYPE,
				"hpmsVtypeID", ImporterManager.FILTER_HPMS_VTYPE,
				"ageID", ImporterManager.FILTER_AGE,
				"avgSpeedBinID", ImporterManager.FILTER_AVGSPEED_BIN,
				"polProcessID", ImporterManager.FILTER_POLPROCESSID,
				"opModeID", ImporterManager.FILTER_OPMODEID,
				"testStandardsID", ImporterManager.FILTER_TESTSTANDARDSID 
			};
			for(int i=0;i<basicPairs.length;i+=2) {
				if(basicPairs[i+0].equalsIgnoreCase(columnName)) {
					return basicPairs[i+1];
				}
			}
			return null;
		}
	}

	/** Class for interfacing to BasicDataHandler's needs during an import **/
	class BasicDataHandlerProvider implements BasicDataHandler.IProvider {
		/**
		 * Obtain the name of the file holding data for a table.
		 * @param tableName table in question
		 * @return the name of the file holding data for a table, null or blank if
		 * no file has been specified.
		**/
		public String getTableFileSource(String tableName) {
			if(tableName.equalsIgnoreCase(primaryTableName)) {
				return part.fileName;
			}
			return null;
		}

		/**
		 * Obtain the name of the worksheet within an XLS file holding data for a table.
		 * @param tableName table in question
		 * @return the name of the worksheet within an XLS file, null or blank if no
		 * worksheet has been specified or if the file is not an XLS file.
		**/
		public String getTableWorksheetSource(String tableName) {
			if(tableName.equalsIgnoreCase(primaryTableName)) {
				return part.worksheetName;
			}
			return null;
		}

		/**
		 * Allow custom processing and SQL for exporting data.
		 * @param type which type of MOVES database holds the exported data.  Typically, this
		 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
		 * being used.
		 * @param db database holding the data to be exported
		 * @param tableName table being exported
		 * @return SQL to be used or null if there is no alternate SQL.
		**/
		public String getAlternateExportSQL(MOVESDatabaseType type, Connection db, 
				String tableName) {
			return null;
		}

		/**
		 * Cleanup custom processing and SQL for exporting data.
		 * @param type which type of MOVES database holds the exported data.  Typically, this
		 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
		 * being used.
		 * @param db database holding the data to be exported
		 * @param tableName table being exported
		**/
		public void cleanupAlternateExportSQL(MOVESDatabaseType type, Connection db, 
				String tableName) {
			// Nothing to do here
		}
	}

	/** Constructor **/
	public GenericImporter() {
		super("Generic", // common name
				"generic", // XML node name
				null // required tables
				);
		fillTableNames();
		removeDisallowedTables();

		basicDataHandler = new BasicDataHandler(this,dataTableDescriptor,
				new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
		PartProvider partProvider = new PartProvider();
		part = new TableFileLinkagePart(this,partProvider,partProvider);
		parts.add(part);
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		if(primaryTableName != null && !primaryTableName.equalsIgnoreCase(emptyPrimaryTableName)) {
			return primaryTableName;
		}
		return "Generic";
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.  Will be null if merely checking
	 * for whether to show the importer to the user.
	 * @return the status, or null if the importer should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getCountyDataStatus(Connection db) 
			throws Exception {
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		/*
		boolean hasZones = manager.tableHasZones(db,
				"select distinct zoneID from zoneRoadType");
		boolean hasRoadTypes = manager.tableHasNonOffnetworkRoadTypes(db,
				"select distinct roadTypeID from zoneRoadType");
		if(hasZones && hasRoadTypes) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		return getImporterDataStatus(db);
		*/
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatus(Connection db) throws Exception {
		/*
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/ZoneRoadTypeImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.equalsIgnoreCase("OK")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
			} else if(t.equalsIgnoreCase("NOT_READY")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		*/
		return null; // no comment
	}

	/**
	 * Fill the tables set with the names of all tables that are allowed
	 * through the generic importer.
	**/
	private void fillTableNames() {
		// Get the list of table names from DEFAULT and NRDEFAULT. See DatabaseConnectionManager for its use of "SHOW TABLES".
		MOVESDatabaseType[] typesToCheck = {
			MOVESDatabaseType.DEFAULT
			//,MOVESDatabaseType.NRDEFAULT
		};
		Connection db = null;
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();

		for(int i=0;i<typesToCheck.length;i++) {
			/*
			if(!CompilationFlags.USE_NONROAD && typesToCheck[i] == MOVESDatabaseType.NRDEFAULT) {
				continue;
			}
			*/
			try {
				db = DatabaseConnectionManager.checkOutConnection(typesToCheck[i]);
				
				if( db!=null ){		
					sql = "show tables";
					query.open(db,sql);				
					while(query.rs.next()) {
						String tableName = query.rs.getString(1);
						tables.add(tableName);
					}
					query.close();
				}
			} catch(Exception e) {
				Logger.logError(e,"Unable to show tables in database " + typesToCheck[i].toString());
			} finally {
				query.onFinally();
				if(db != null) {
					DatabaseConnectionManager.checkInConnection(typesToCheck[i],db);
					db = null;
				}
			}
		}
	}

	/**
	 * Remove tables handled by other importers.
	**/
	private void removeDisallowedTables() {
		tables.remove("x_tempc");
		tables.remove("x_tempp");

		tables.remove("sourceTypeAgeDistribution");
		tables.remove("avgSpeedDistribution");
		tables.remove("driveScheduleSecondLink");
		tables.remove("FuelFormulation");
		tables.remove("FuelSupply");
		tables.remove("link");
		tables.remove("opModeDistribution");
		tables.remove("linkSourceTypeHour");
		tables.remove("zoneMonthHour");
		tables.remove("offNetworkLink");
		tables.remove("roadType");
		tables.remove("roadTypeDistribution");
		tables.remove("sourceTypeYear");
		tables.remove("HPMSVTypeYear");
		tables.remove("MonthVMTFraction");
		tables.remove("DayVMTFraction");
		tables.remove("HourVMTFraction");
		tables.remove("zoneRoadType");

		if(!ImporterInstantiator.activeManager.isProject()) {
			// HotellingHours has an importer for non-project mode.
			// Don't allow it to be generically imported for project domain.
			tables.remove("hotellingHours");
		}
	}
}
