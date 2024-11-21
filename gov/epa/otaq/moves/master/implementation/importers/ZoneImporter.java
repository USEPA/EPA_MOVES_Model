/**************************************************************************************************
 * @(#)ZoneImporter.java
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
 * MOVES Zone ZoneRoadType Importer.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @version		2014-02-18
**/
public class ZoneImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the Zone table **/
	TableFileLinkagePart zonePart;

	/** Part object for the zoneRoadType table **/
	TableFileLinkagePart zoneRoadTypePart;

	/**
	 * Name of the primary table handled by the importer.
	 * Note the MOVES database naming convention.
	**/
	String primaryTableName = "zone";

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "zone",
		"zoneID", "ZoneDecode", ImporterManager.FILTER_ZONE, // use the ZoneDecode table, as using just "Zone" will conflict with the Zone input tab and decode table will fail to write
		"countyID", "County", ImporterManager.FILTER_COUNTY,
		"startAllocFactor", "", ImporterManager.FILTER_NON_NEGATIVE,
		"idleAllocFactor", "", ImporterManager.FILTER_NON_NEGATIVE,
		"SHPAllocFactor", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "zoneroadtype",
		"zoneID", "ZoneDecode", ImporterManager.FILTER_ZONE, // use ZoneDecode. Even if this table is exported by itself, don't use the "Zone" tab as the name for the decode tab, as the Zone Importer will see that tab and try to import it as the Zone table
		"roadTypeID", "RoadType", ImporterManager.FILTER_ROAD_TYPE_NOT_OFFNETWORK,
		"SHOAllocFactor", "", ImporterManager.FILTER_NON_NEGATIVE
	};

	/** Class for editing the data source **/
	class ZonePartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "zone";
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
	class ZoneRoadTypePartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "zoneRoadType";
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
			if(tableName.equalsIgnoreCase("zone")) {
				return zonePart.fileName;
			} else if(tableName.equalsIgnoreCase("zoneRoadType")) {
				return zoneRoadTypePart.fileName;
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
			if(tableName.equalsIgnoreCase("zone")) {
				return zonePart.worksheetName;
			} else if(tableName.equalsIgnoreCase("zoneRoadType")) {
				return zoneRoadTypePart.worksheetName;
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
	public ZoneImporter() {
		super("Zone", // common name
				"zone", // XML node name
				new String[] { "Zone", "ZoneRoadType" } // required tables
				);
		shouldDoExecutionDataExport = false;
		shouldDoDefaultDataExport = true;
		subjectToExportRestrictions = false;

		zonePart = new TableFileLinkagePart(this,new ZonePartProvider());
		parts.add(zonePart);
		zoneRoadTypePart = new TableFileLinkagePart(this,new ZoneRoadTypePartProvider());
		parts.add(zoneRoadTypePart);

		basicDataHandler = new BasicDataHandler(this,dataTableDescriptor,new BasicDataHandlerProvider());
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
		return "Zone";
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
		return true;
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	private boolean getDataStatusFromScript(Connection db) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/ZoneImporter.sql");
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
