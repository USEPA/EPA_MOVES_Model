/**************************************************************************************************
 * @(#)HotellingImporter.java
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
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator;

/**
 * MOVES Importer for hotellingActivityDistribution.
 *
 * @author		Wesley Faler
 * @version		2014-04-30
**/
public class HotellingImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the hotellingActivityDistribution table **/
	TableFileLinkagePart hotellingActivityDistributionPart;
	/** Part object for the hotellingHours table **/
	TableFileLinkagePart hotellingHoursPart;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer
	 * when applied to a project domain.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptorProject = {
		BasicDataHandler.BEGIN_TABLE, "hotellingActivityDistribution",
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
		BasicDataHandler.BEGIN_TABLE, "hotellingActivityDistribution",
		"beginModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"endModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"opModeID", "OperatingModeAux", ImporterManager.FILTER_OPMODEID_AUX,
		"opModeFraction", "", ImporterManager.FILTER_0_TO_1_FRACTION,
		
		BasicDataHandler.BEGIN_TABLE, "hotellingHours",
		"hourDayID", "HourDay", ImporterManager.FILTER_HOURDAY,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"yearID", "", ImporterManager.FILTER_YEAR,
		"ageID", "", ImporterManager.FILTER_AGE,
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"hotellingHours", "", ImporterManager.FILTER_NON_NEGATIVE
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
	class HotellingHoursPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "hotellingHours";
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
			} else if(hotellingHoursPart != null && tableName.equalsIgnoreCase("hotellingHours")) {
				return hotellingHoursPart.fileName;
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
			} else if(hotellingHoursPart != null && tableName.equalsIgnoreCase("hotellingHours")) {
				return hotellingHoursPart.worksheetName;
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
			// nothing to do here
		}
	}

	/** Constructor **/
	public HotellingImporter() {
		super("Hotelling", // common name
				"hotelling", // XML node name
				new String[] { "hotellingActivityDistribution", "hotellingHours" } // required tables
				);
		shouldDoExecutionDataExport = false;
		shouldDoDefaultDataExport = ImporterInstantiator.activeManager.isProject()? false : true;
		subjectToExportRestrictions = false;

		hotellingActivityDistributionPart = new TableFileLinkagePart(this,new HotellingActivityDistributionPartProvider());
		parts.add(hotellingActivityDistributionPart);

		if(ImporterInstantiator.activeManager.isProject()) {
			hotellingHoursPart = null;
			basicDataHandler = new BasicDataHandler(this,dataTableDescriptorProject,new BasicDataHandlerProvider());
		} else {
			hotellingHoursPart = new TableFileLinkagePart(this,new HotellingHoursPartProvider());
			parts.add(hotellingHoursPart);

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
		if(hotellingHoursPart != null) {
			// Allow hotellingHours to be empty
			if(SQLRunner.executeScalar(db,"select count(*) from hotellingHours") <= 0) {
				return true;
			}
			// Check hourDayID
			if(!manager.tableHasHourDays(db,"select distinct hourDayID from hotellingHours")) {
				return false;
			}
			// Check monthID
			if(!manager.tableHasMonths(db,"select distinct monthID from hotellingHours")) {
				return false;
			}
			// Check yearID
			if(!manager.tableHasYears(db,"select distinct yearID from hotellingHours")) {
				return false;
			}
			// Check zoneID
			if(!manager.tableHasZones(db,"select distinct zoneID from hotellingHours")) {
				return false;
			}
			// Check ageID
			if(!manager.tableHasAges(db,"select distinct ageID from hotellingHours")) {
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
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	private boolean getDataStatusFromScript(Connection db) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1);
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toLowerCase().startsWith("error")) {
				return false;
			}
		}
		return true;
	}
}
