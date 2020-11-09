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
import gov.epa.otaq.moves.worker.framework.WorkerConfiguration;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.SystemConfiguration;

/**
 * MOVES Starts Importer.
 *
 * @author		Wesley Faler
 * @author 		John Covey - Task 1806 changes
 * @version 	2018-11-16

**/
public class StartsImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;

	/** Part object for the startsHourFraction table **/
	TableFileLinkagePart startsHourFraction;
	/** Part object for the startsMonthAdjust table **/
	TableFileLinkagePart startsMonthAdjust;
	/** Part object for the startsOpModeDistribution table **/
	TableFileLinkagePart startsOpModeDistribution;
	/** Part object for the startsPerDayPerVehicle table **/
	TableFileLinkagePart startsPerDayPerVehicle;
	/** Part object for the starts table **/
	TableFileLinkagePart starts;
	/** Part object for the startsPerDay table **/
	TableFileLinkagePart startsPerDay;
	/** Part object for the startsAgeAdjustment table **/
	TableFileLinkagePart startsAgeAdjustment;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		
		BasicDataHandler.BEGIN_TABLE, "startsPerDayPerVehicle",
		"dayID","Day", ImporterManager.FILTER_DAY,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		// yeet for new starts shaping "ageID", "", ImporterManager.FILTER_AGE,
		"startsPerDayPerVehicle", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "StartsPerDay",
		"dayID","Day", ImporterManager.FILTER_DAY,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"startsPerDay", "", ImporterManager.FILTER_NON_NEGATIVE,
		
		BasicDataHandler.BEGIN_TABLE, "startsHourFraction",
		"dayID","Day", ImporterManager.FILTER_DAY,
		"hourID","Hour", ImporterManager.FILTER_HOUR,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"allocationFraction", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "startsMonthAdjust",
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"monthAdjustment", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "StartsAgeAdjustment",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"ageID", "", ImporterManager.FILTER_AGE,
		"ageAdjustment", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "startsOpModeDistribution",
		"dayID","Day", ImporterManager.FILTER_DAY,
		"hourID","Hour", ImporterManager.FILTER_HOUR,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"ageID", "", ImporterManager.FILTER_AGE,
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
		"startsCV", "", ImporterManager.FILTER_NON_NEGATIVE,
		"isUserInput", "", ImporterManager.FILTER_YN

	};

	/** Class for editing the data source **/
	class StartsProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "starts";
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
	class StartsHourFractionProvider implements TableFileLinkagePart.IProvider {
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
	class StartsMonthAdjustProvider implements TableFileLinkagePart.IProvider {
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
	class StartsOpModeDistributionProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "startsOpModeDistribution";
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
	class StartsPerDayPerVehicleProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "startsPerDayPerVehicle";
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
	class StartsPerDayProvider implements TableFileLinkagePart.IProvider {
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
	class StartsAgeAdjustmentProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "startsAgeAdjustment";
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
			if(tableName.equalsIgnoreCase("startsHourFraction")) {
				return startsHourFraction.fileName;
			} else if(tableName.equalsIgnoreCase("startsMonthAdjust")) {
				return startsMonthAdjust.fileName;
			} else if(tableName.equalsIgnoreCase("startsOpModeDistribution")) {
				return startsOpModeDistribution.fileName;
			} else if(tableName.equalsIgnoreCase("startsPerDayPerVehicle")) {
				return startsPerDayPerVehicle.fileName;
			// new starts shaping stuff
			} else if(tableName.equalsIgnoreCase("starts")) {
				return starts.fileName;
			} else if(tableName.equalsIgnoreCase("startsPerDay")) {
				return startsPerDay.fileName;
			} else if(tableName.equalsIgnoreCase("startsAgeAdjustment")) {
				return startsAgeAdjustment.fileName;
			}
			// end new starts shaping stuff
			return null;
		}

		/**
		 * Obtain the name of the worksheet within an XLS file holding data for a table.
		 * @param tableName table in question
		 * @return the name of the worksheet within an XLS file, null or blank if no
		 * worksheet has been specified or if the file is not an XLS file.
		**/
		public String getTableWorksheetSource(String tableName) {
			if(tableName.equalsIgnoreCase("startsHourFraction")) {
				return startsHourFraction.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsMonthAdjust")) {
				return startsMonthAdjust.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsOpModeDistribution")) {
				return startsOpModeDistribution.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsPerDayPerVehicle")) {
				return startsPerDayPerVehicle.worksheetName;
			// new starts shaping stuff
			} else if(tableName.equalsIgnoreCase("starts")) {
				return starts.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsPerDay")) {
				return startsPerDay.worksheetName;
			} else if(tableName.equalsIgnoreCase("startsAgeAdjustment")) {
				return startsAgeAdjustment.worksheetName;
			}
			// end new starts shaping stuff
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
				new String[] { // required tables // are there optional tables???
					"startsHourFraction",
					"startsMonthAdjust",
					"startsOpModeDistribution",
					"startsPerDayPerVehicle",
					//new importer parts
					"starts",
					"startsPerDay",
					"startsAgeAdjustment"
				});

		shouldDoExecutionDataExport = false;
		shouldDoDefaultDataExport = true;
		shouldDoCustomDefaultDataExport = false;
		subjectToExportRestrictions = false;
		
		startsPerDayPerVehicle = new TableFileLinkagePart(this,new StartsPerDayPerVehicleProvider());
		parts.add(startsPerDayPerVehicle);

		startsPerDay = new TableFileLinkagePart(this,new StartsPerDayProvider());
		parts.add(startsPerDay);

		startsHourFraction = new TableFileLinkagePart(this,new StartsHourFractionProvider());
		parts.add(startsHourFraction);

		startsMonthAdjust = new TableFileLinkagePart(this,new StartsMonthAdjustProvider());
		parts.add(startsMonthAdjust);

		startsAgeAdjustment = new TableFileLinkagePart(this,new StartsAgeAdjustmentProvider());
		parts.add(startsAgeAdjustment);
		
		startsOpModeDistribution = new TableFileLinkagePart(this,new StartsOpModeDistributionProvider());
		parts.add(startsOpModeDistribution);

		starts = new TableFileLinkagePart(this,new StartsProvider());
		parts.add(starts);

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
		return getImporterDataStatusCore(db);
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
		return getImporterDataStatusCore(db);
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatusCore(Connection db) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/StartsImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toUpperCase().startsWith("ERROR")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}
}
