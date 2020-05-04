/**************************************************************************************************
 * @(#)StartsImporter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;

import java.io.*;
import java.sql.*;
import java.util.*;
import javax.xml.parsers.*;
import javax.swing.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.SystemConfiguration;

/**
 * MOVES Starts Importer.
 *
 * @author		Wesley Faler
 * @version		2014-01-08
**/
public class StartsImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;

	/** Part object for the FuelSupply table **/
	TableFileLinkagePart startsPart;
	/** Part object for the startsPerDay table **/
	TableFileLinkagePart startsPerDay;
	/** Part object for the startsHourFraction table **/
	TableFileLinkagePart startsHourFraction;
	/** Part object for the startsSourceTypeFraction table **/
	TableFileLinkagePart startsSourceTypeFraction;
	/** Part object for the startsMonthAdjust table **/
	TableFileLinkagePart startsMonthAdjust;
	/** Part object for the importStartsOpModeDistribution table **/
	TableFileLinkagePart importStartsOpModeDistribution;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "startsPerDay",
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"dayID","Day", ImporterManager.FILTER_DAY,
		"yearID", "", ImporterManager.FILTER_YEAR,
		"startsPerDay", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "startsHourFraction",
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"dayID","Day", ImporterManager.FILTER_DAY,
		"hourID","Hour", ImporterManager.FILTER_HOUR,
		"allocationFraction", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "startsSourceTypeFraction",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"allocationFraction", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "startsMonthAdjust",
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"monthAdjustment", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "importStartsOpModeDistribution",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"hourDayID", "HourDay", ImporterManager.FILTER_HOURDAY,
		"linkID", "", ImporterManager.FILTER_OFFNETWORK_LINK,
		"polProcessID", "PollutantProcessAssoc", ImporterManager.FILTER_STARTS_POLPROCESSID,
		"opModeID", "StartsOperatingMode", ImporterManager.FILTER_STARTS_OPMODEID,
		"opModeFraction", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "Starts",
		"hourDayID", "HourDay", ImporterManager.FILTER_HOURDAY,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"yearID", "", ImporterManager.FILTER_YEAR,
		"ageID", "", ImporterManager.FILTER_AGE,
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"starts", "", ImporterManager.FILTER_NON_NEGATIVE,
		"startsCV", "", ImporterManager.FILTER_NON_NEGATIVE
	};

	/** Class for editing the data source **/
	class StartsProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "Starts";
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
	class StartsPerDay implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "startsPerDay";
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
	class StartsHourFraction implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "startsHourFraction";
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
	class StartsSourceTypeFraction implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "startsSourceTypeFraction";
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
	class StartsMonthAdjust implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "startsMonthAdjust";
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
	class ImportStartsOpModeDistribution implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "importStartsOpModeDistribution";
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
			if(tableName.equalsIgnoreCase("Starts")) {
				return startsPart.fileName;
			} else if(tableName.equalsIgnoreCase("startsPerDay")) {
				return startsPerDay.fileName;
			} else if(tableName.equalsIgnoreCase("startsHourFraction")) {
				return startsHourFraction.fileName;
			} else if(tableName.equalsIgnoreCase("startsSourceTypeFraction")) {
				return startsSourceTypeFraction.fileName;
			} else if(tableName.equalsIgnoreCase("startsMonthAdjust")) {
				return startsMonthAdjust.fileName;
			} else if(tableName.equalsIgnoreCase("importStartsOpModeDistribution")) {
				return importStartsOpModeDistribution.fileName;
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
			if(tableName.equalsIgnoreCase("Starts")) {
				return startsPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsPerDay")) {
				return startsPerDay.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsHourFraction")) {
				return startsHourFraction.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsSourceTypeFraction")) {
				return startsSourceTypeFraction.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsMonthAdjust")) {
				return startsMonthAdjust.worksheetName;
			} else if(tableName.equalsIgnoreCase("importStartsOpModeDistribution")) {
				return importStartsOpModeDistribution.worksheetName;
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
	public StartsImporter() {
		super("Starts", // common name
				"starts", // XML node name
				new String[] { "Starts", // required tables
					"startsPerDay", "startsHourFraction",
					"startsSourceTypeFraction", "startsMonthAdjust",
					"importStartsOpModeDistribution"
				});

		shouldDoExecutionDataExport = true;
		shouldDoDefaultDataExport = false;
		shouldDoCustomDefaultDataExport = false;
		subjectToExportRestrictions = false;

		startsPerDay = new TableFileLinkagePart(this,new StartsPerDay());
		parts.add(startsPerDay);

		startsHourFraction = new TableFileLinkagePart(this,new StartsHourFraction());
		parts.add(startsHourFraction);

		startsSourceTypeFraction= new TableFileLinkagePart(this,new StartsSourceTypeFraction());
		parts.add(startsSourceTypeFraction);

		startsMonthAdjust = new TableFileLinkagePart(this,new StartsMonthAdjust());
		parts.add(startsMonthAdjust);

		importStartsOpModeDistribution = new TableFileLinkagePart(this,new ImportStartsOpModeDistribution());
		parts.add(importStartsOpModeDistribution);

		startsPart = new TableFileLinkagePart(this,new StartsProvider());
		parts.add(startsPart);

		basicDataHandler = new BasicDataHandler(this,dataTableDescriptor,new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		return "Starts";
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
		// Maybe need code here
		/*
		boolean hasCounties = manager.tableHasCounties(db,
				"select distinct countyID from fuelSupply");
		if(!hasCounties) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		boolean hasYears = manager.tableHasYears(db,
				"select distinct yearID from fuelSupply"
				+ " inner join year using (fuelYearID)");
		if(!hasYears) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}

		String defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
		boolean hasFuels = manager.tableHasFuelTypes(db,
				"select distinct fuelTypeID"
				+ " from fuelSupply fs"
				+ " inner join fuelFormulation ff using (fuelFormulationID)"
				+ " inner join " + defaultDatabaseName + ".fuelSubType fst using (fuelSubTypeID)");
		if(!hasFuels) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		*/
		return getImporterDataStatusCore(db,true);
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
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatus(Connection db) throws Exception {
		return getImporterDataStatusCore(db,false);
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @param requireAllData true if the user must provide all fuel formulations for their location
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatusCore(Connection db, boolean requireAllData) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,requireAllData?2:1,"database/StartsImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toUpperCase().startsWith("ERROR")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}
}
