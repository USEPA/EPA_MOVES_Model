/**************************************************************************************************
 * @(#)HotellingImporter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;

import java.io.*;
import java.sql.*;
import java.util.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.common.*;

/**
 * MOVES Importer for hotellingActivityDistribution.
 *
 * @author		Wesley Faler
 * @author		John Covey
 * @version		2019-2-08
**/
public class HotellingImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the hotellingActivityDistribution table **/
	TableFileLinkagePart hotellingActivityDistributionPart;

	/** Part object for the hotellingHourFraction table **/
	TableFileLinkagePart hotellingHourFractionPart;
	/** Part object for the hotellingAgeFraction table **/
	TableFileLinkagePart hotellingAgeFractionPart;
	/** Part object for the hotellingMonthAdjust table **/
	TableFileLinkagePart hotellingMonthAdjustPart;
	/** Part object for the hotellingHoursPerDay table **/
	TableFileLinkagePart hotellingHoursPerDayPart;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer
	 * when applied to a project domain.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptorProject = {
		BasicDataHandler.BEGIN_TABLE, "hotellingActivityDistribution",
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"beginModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"endModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"opModeID", "OperatingModeAux", ImporterManager.FILTER_OPMODEID_AUX,
		"opModeFraction", "", ImporterManager.FILTER_0_TO_1_FRACTION
	};

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer
	 * when applied to anything except a project domain.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptorNotProject = {
		BasicDataHandler.BEGIN_TABLE, "hotellingHoursPerDay",
		"yearID", "Year", ImporterManager.FILTER_YEAR,
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"dayID", "DayOfAnyWeek", ImporterManager.FILTER_DAY,
		"hotellingHoursPerDay", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "hotellingHourFraction",
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"dayID", "DayOfAnyWeek", ImporterManager.FILTER_DAY,
		"hourID", "HourOfAnyDay", ImporterManager.FILTER_HOUR,
		"hourFraction", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "hotellingAgeFraction",
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"ageID", "AgeCategory", ImporterManager.FILTER_AGE,
		"ageFraction", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "hotellingMonthAdjust",
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"monthAdjustment", "", ImporterManager.FILTER_NON_NEGATIVE_DEFAULT_1,

		BasicDataHandler.BEGIN_TABLE, "hotellingActivityDistribution",
		"zoneID", "Zone", "%%", // % wildcard is needed here so that the getAlternateExportSQL() results for this table are not filtered (default db has ZoneID 990000 and we want all of those rows)
		"beginModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"endModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"opModeID", "OperatingModeAux", ImporterManager.FILTER_OPMODEID_AUX,
		"opModeFraction", "", ImporterManager.FILTER_0_TO_1_FRACTION
	};

	/** Class for editing the data source **/
	class HotellingActivityDistributionPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "hotellingActivityDistribution";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class HotellingHourFractionPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "hotellingHourFraction";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class HotellingAgeFractionPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "hotellingAgeFraction";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class HotellingMonthAdjustPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "hotellingMonthAdjust";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class HotellingHoursPerDayPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "hotellingHoursPerDay";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
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
			if(tableName.equalsIgnoreCase("hotellingActivityDistribution")) {
				return hotellingActivityDistributionPart.fileName;
			} else if(hotellingHourFractionPart != null && tableName.equalsIgnoreCase("hotellingHourFraction")) {
				return hotellingHourFractionPart.fileName;
			} else if(hotellingAgeFractionPart != null && tableName.equalsIgnoreCase("hotellingAgeFraction")) {
				return hotellingAgeFractionPart.fileName;
			} else if(hotellingMonthAdjustPart != null && tableName.equalsIgnoreCase("hotellingMonthAdjust")) {
				return hotellingMonthAdjustPart.fileName;
			} else if(hotellingHoursPerDayPart != null && tableName.equalsIgnoreCase("hotellingHoursPerDay")) {
				return hotellingHoursPerDayPart.fileName;
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
			if(tableName.equalsIgnoreCase("hotellingActivityDistribution")) {
				return hotellingActivityDistributionPart.worksheetName;
			} else if(hotellingHourFractionPart != null && tableName.equalsIgnoreCase("hotellingHourFraction")) {
				return hotellingHourFractionPart.worksheetName;
			} else if(hotellingAgeFractionPart != null && tableName.equalsIgnoreCase("hotellingAgeFraction")) {
				return hotellingAgeFractionPart.worksheetName;
			} else if(hotellingMonthAdjustPart != null && tableName.equalsIgnoreCase("hotellingMonthAdjust")) {
				return hotellingMonthAdjustPart.worksheetName;
			} else if(hotellingHoursPerDayPart != null && tableName.equalsIgnoreCase("hotellingHoursPerDay")) {
				return hotellingHoursPerDayPart.worksheetName;
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
			if(type != MOVESDatabaseType.EXECUTION && type != MOVESDatabaseType.DEFAULT) {
				// Only the execution database or the default has all the supporting tables filled.
				return null;
			}
			if(tableName.equalsIgnoreCase("hotellingHourFraction")) {
				return "select zoneID,dayid, hourid, hourFraction from (select "
						+ "	dayID, hourID,"
						+ "	hotellingDist as hourFraction"
						+ " from sourceTypeHour"
						+ " inner join hourDay using (hourDayID)) as t"
						+ " cross join zone"
						+ " order by dayID, hourID";
			}
			if(tableName.equalsIgnoreCase("hotellingMonthAdjust")) {
				return "select zoneId, monthId, 1 as monthAdjustment "
						+ "	from zone cross join monthofanyyear";
			}
			if(tableName.equalsIgnoreCase("hotellingActivityDistribution")) {
				return "SELECT '' as zoneID, beginModelYearID, endModelYearID, opModeID, opModeFraction "
					   + "FROM hotellingactivitydistribution";
			}
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
			// nothing to do here
		}
	}

	/** Constructor **/
	public HotellingImporter() {
		super("Hotelling", // common name
				"hotelling", // XML node name
				new String[] { "hotellingActivityDistribution", "hotellingHourFraction",
						"hotellingAgeFraction", "hotellingMonthAdjust", "hotellingHoursPerDay" } // required tables
				);
		shouldDoExecutionDataExport = false;
		shouldDoDefaultDataExport = ImporterInstantiator.activeManager.isProject()? false : true;
		subjectToExportRestrictions = false;

		if(ImporterInstantiator.activeManager.isProject()) {
			hotellingActivityDistributionPart = new TableFileLinkagePart(this,new HotellingActivityDistributionPartProvider());
			parts.add(hotellingActivityDistributionPart);
			basicDataHandler = new BasicDataHandler(this,dataTableDescriptorProject,new BasicDataHandlerProvider());
		} else {
			hotellingHoursPerDayPart = new TableFileLinkagePart(this,new HotellingHoursPerDayPartProvider());
			parts.add(hotellingHoursPerDayPart);
			hotellingHourFractionPart = new TableFileLinkagePart(this,new HotellingHourFractionPartProvider());
			parts.add(hotellingHourFractionPart);
			hotellingAgeFractionPart = new TableFileLinkagePart(this,new HotellingAgeFractionPartProvider());
			parts.add(hotellingAgeFractionPart);
			hotellingMonthAdjustPart = new TableFileLinkagePart(this,new HotellingMonthAdjustPartProvider());
			parts.add(hotellingMonthAdjustPart);

			hotellingActivityDistributionPart = new TableFileLinkagePart(this,new HotellingActivityDistributionPartProvider());
			parts.add(hotellingActivityDistributionPart);

			basicDataHandler = new BasicDataHandler(this,dataTableDescriptorNotProject,new BasicDataHandlerProvider());
		}

		dataHandler = basicDataHandler;
	}

	/**
	 * Designate the ImporterManager object that his hosting the importer.
	 * @param managerToUse the ImporterManager object that his hosting the importer
	**/
	public void setImporterManager(ImporterManager managerToUse) {
		super.setImporterManager(managerToUse);
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		return "Hotelling";
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
		if(checkCountyDataStatus(db) && getDataStatusFromScript(db)) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		} else {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.  Will be null if merely checking
	 * for whether to show the importer to the user.
	 * @return true if all tables have data that matches the RunSpec.
	 * @throws Exception if anything goes wrong
	**/
	private boolean checkCountyDataStatus(Connection db) throws Exception {
		/*
		if(hotellingHoursPart != null) {
			if(SQLRunner.executeScalar(db,"select count(*) from hotellingHours") > 0) {
				// Check hourDayID
				if(!manager.tableHasHourDays(db,"select distinct hourDayID from hotellingHours",
						this,"hotellingHours is missing hourDayID(s)")) {
					return false;
				}
				// Check monthID
				if(!manager.tableHasMonths(db,"select distinct monthID from hotellingHours",
						this,"hotellingHours is missing monthID(s)")) {
					return false;
				}
				// Check yearID
				if(!manager.tableHasYears(db,"select distinct yearID from hotellingHours",
						this,"hotellingHours is missing yearID(s)")) {
					return false;
				}
				// Check zoneID
				if(!manager.tableHasZones(db,"select distinct zoneID from hotellingHours",
						this,"hotellingHours is missing zoneID(s)")) {
					return false;
				}
				// Check ageID
				if(!manager.tableHasAges(db,"select distinct ageID from hotellingHours",
						this,"hotellingHours is missing ageID(s)")) {
					return false;
				}
			}
		}
		*/

		if(SQLRunner.executeScalar(db,"select count(*) from hotellingActivityDistribution") > 0) {
			if(!manager.tableHasZones(db,"select distinct zoneID from hotellingActivityDistribution",
					this,"hotellingActivityDistribution is missing zoneID(s)")) {
				return false;
			}
		}

		if(SQLRunner.executeScalar(db,"select count(*) from hotellingHoursPerDay") > 0) {
			if(!manager.tableHasZones(db,"select distinct zoneID from hotellingHoursPerDay",
					this,"hotellingHoursPerDay is missing zoneID(s)")) {
				return false;
			}
			if(!manager.tableHasDays(db,"select distinct dayID from hotellingHoursPerDay",
					this,"hotellingHoursPerDay is missing dayID(s)")) {
				return false;
			}
		}

		if(SQLRunner.executeScalar(db,"select count(*) from hotellingHourFraction") > 0) {
			if(!manager.tableHasZones(db,"select distinct zoneID from hotellingHourFraction",
					this,"hotellingHourFraction is missing zoneID(s)")) {
				return false;
			}
			if(!manager.tableHasDays(db,"select distinct dayID from hotellingHourFraction",
					this,"hotellingHourFraction is missing dayID(s)")) {
				return false;
			}
			if(!manager.tableHasHours(db,"select distinct hourID from hotellingHourFraction",
					this,"hotellingHourFraction is missing hourID(s)")) {
				return false;
			}
		}

		if(SQLRunner.executeScalar(db,"select count(*) from hotellingAgeFraction") > 0) {
			if(!manager.tableHasZones(db,"select distinct zoneID from hotellingAgeFraction",
					this,"hotellingAgeFraction is missing zoneID(s)")) {
				return false;
			}
			if(!manager.tableHasAges(db,"select distinct ageID from hotellingAgeFraction",
					this,"hotellingAgeFraction is missing ageID(s)")) {
				return false;
			}
		}

		if(SQLRunner.executeScalar(db,"select count(*) from hotellingMonthAdjust") > 0) {
			if(!manager.tableHasZones(db,"select distinct zoneID from hotellingMonthAdjust",
					this,"hotellingMonthAdjust is missing zoneID(s)")) {
				return false;
			}
			if(!manager.tableHasMonths(db,"select distinct monthID from hotellingMonthAdjust",
					this,"hotellingMonthAdjust is missing monthID(s)")) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.  Will be null if merely checking
	 * for whether to show the importer to the user.
	 * @return the status, or null if the importer should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getProjectDataStatus(Connection db)
			throws Exception {
		return getCountyDataStatus(db);
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * Look for an OK or a NOT_READY message from the HotellingImporter.sql script
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	private boolean getDataStatusFromScript(Connection db) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/HotellingImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.equalsIgnoreCase("OK")) {
				return true;
			} else if(t.equalsIgnoreCase("NOT_READY")) {
				return false;
			}
		}
		return true;
	}
}
