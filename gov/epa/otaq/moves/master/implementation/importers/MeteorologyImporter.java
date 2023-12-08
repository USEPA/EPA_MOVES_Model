/**************************************************************************************************
 * @(#)MeteorologyImporter.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;
import gov.epa.otaq.moves.common.*;

import java.io.*;
import java.sql.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.common.MOVESDatabaseType;

/**
 * MOVES ZoneMonthHour Data Importer.
 * 
 * @author		Wesley Faler
 * @version		2015-09-16
**/
public class MeteorologyImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the ZoneMonthHour table **/
	TableFileLinkagePart part;

	/**
	 * Name of the primary table handled by the importer.
	 * Note the MOVES database naming convention.
	**/
	String primaryTableName = "zoneMonthHour";

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "ZoneMonthHour",
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"zoneID", "Zone", ImporterManager.FILTER_ZONE,
		"hourID", "HourOfAnyDay", ImporterManager.FILTER_HOUR,
		"temperature", "", "",
		"relHumidity", "", ImporterManager.FILTER_0_TO_100_PERCENTAGE
	};

	/** Class for editing the data source **/
	class PartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return primaryTableName;
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
	public MeteorologyImporter() {
		super("Meteorology Data", // common name
				"zonemonthhour", // XML node name
				new String[] { "ZoneMonthHour" } // required tables
				);
		part = new TableFileLinkagePart(this,new PartProvider());
		parts.add(part);
		basicDataHandler = new BasicDataHandler(this,dataTableDescriptor,
				new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
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
		String sql;
		SQLRunner.Query query = new SQLRunner.Query();
		boolean hasError = false;
		
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		boolean hasMonths = manager.tableHasMonths(db,
				"select distinct monthID from zoneMonthHour",
				this,"zoneMonthHour is missing monthID(s)");
		boolean hasHours = manager.tableHasHours(db,
				"select distinct hourID from zoneMonthHour",
				this,"zoneMonthHour is missing hourID(s)");
		boolean hasZones = manager.tableHasZones(db,
				"select distinct zoneID from zoneMonthHour",
				this,"zoneMonthHour is missing zoneID(s)");
				
		// check for any relHumidity over 100
		sql = "SELECT monthID, hourID " +
			  "  FROM zonemonthhour " +
			  "  WHERE relHumidity > 100";
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				int monthID = query.rs.getInt(1);
				int hourID = query.rs.getInt(2);
				addQualityMessage("ERROR: relative humidity is > 100 for monthID " + monthID + " and hourID " + hourID); 
				hasError = true;
			}
		} finally {
			query.close();
		}

        // check for wrong schema
		sql = "SELECT molWaterFraction FROM zonemonthhour LIMIT 0";
        try {
            query.open(db,sql);
            while(query.rs.next()) {}
        } catch (SQLException e) {
            addQualityMessage("ERROR: zonemonthhour has incorrect schema. This database will either need to be converted or recreated.");
            hasError = true;
        } finally {
            query.close();
        }

        // check for NULL values
		sql = "SELECT monthID, hourID FROM zonemonthhour WHERE temperature IS NULL or relHumidity IS NULL LIMIT 1";
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				int monthID = query.rs.getInt(1);
				int hourID = query.rs.getInt(2);
				addQualityMessage("ERROR: NULL temperature and/or relHumidity value for monthID " + monthID + " and hourID " + hourID); 
				hasError = true;
			}
		} finally {
			query.close();
		}
				
		if(hasMonths && hasHours && hasZones && !hasError) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
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
	
	// no SQL error checks for MeteorologyImporter
}
