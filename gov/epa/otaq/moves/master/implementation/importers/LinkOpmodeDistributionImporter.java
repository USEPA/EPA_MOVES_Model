/**************************************************************************************************
 * @(#)LinkOpmodeDistributionImporter.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;

import java.io.*;
import java.util.*;
import java.sql.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

/**
 * MOVES Link OpmodeDistribution Importer.  Imports data in MOVES format (as opposed to
 * NMIM or Mobile format) into MOVES.
 * 
 * @author		Wesley Faler
 * @version		2016-09-16
**/
public class LinkOpmodeDistributionImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object **/
	TableFileLinkagePart part;

	/**
	 * Name of the primary table handled by the importer.
	 * Note the MOVES database naming convention.
	**/
	String primaryTableName = "opModeDistribution";

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "OpModeDistribution",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"hourDayID", "HourDay", ImporterManager.FILTER_HOURDAY,
		"linkID", "", "",
		"polProcessID", "PollutantProcessAssoc", ImporterManager.FILTER_POLPROCESSID,
		"opModeID", "OperatingMode", ImporterManager.FILTER_OPMODEID,
		"opModeFraction", "", ImporterManager.FILTER_NON_NEGATIVE
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

	public class MyBasicDataHandler extends BasicDataHandler {
		/**
		 * Constructor
		 * @param importerToUse importer for this data handler
		 * @param descriptorToUse descriptor for the table(s) used by this importer
		 * @param importDataProviderToUse Provider for data required during imports
		**/
		public MyBasicDataHandler(IImporter importerToUse, String[] descriptorToUse,
				IProvider importDataProviderToUse) {
			super(importerToUse,descriptorToUse,importDataProviderToUse);
		}

		TreeSet<String> templateKeys = new TreeSet<String>();

		/** Event called when a template is being initiated **/
		public void onBeginTemplate() {
			templateKeys.clear();

			Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "select polProcessID, opModeID from opModePolProcAssoc";
			try {
				query.open(db,sql);
				while(query.rs.next()) {
					int p = query.rs.getInt(1);
					int o = query.rs.getInt(2);
					String key = "" + p + "|" + o;
					templateKeys.add(key);
				}
				query.close();
			} catch(SQLException e) {
				Logger.logSqlError(e,"Unable to get template keys",sql);
			} finally {
				query.onFinally();
			}
		}
	
		/**
		 * Called for each row in a template to accept or reject the combination.
		 * @param value array of objects for each column in the template
		 * @return true if the row should be written
		**/
		public boolean shouldWriteTemplateRow(Object[] values) {
			/*
			"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
			"hourDayID", "HourDay", ImporterManager.FILTER_HOURDAY,
			"linkID", "", "",
			"polProcessID", "PollutantProcessAssoc", ImporterManager.FILTER_POLPROCESSID,
			"opModeID", "OperatingMode", ImporterManager.FILTER_OPMODEID,
			*/
			if(values.length < 5) {
				return true;
			}
			Object p = values[3];
			Object o = values[4];
			String key = p.toString() + "|" + o.toString();
			return templateKeys.contains(key);
		}
	}

	/** Constructor **/
	public LinkOpmodeDistributionImporter() {
		super("Operating Mode Distribution", // common name
				"linkopmodedistribution", // XML node name
				new String[] { "OpModeDistribution" } // required tables
				);
		shouldDoExecutionDataExport = true;
		shouldDoDefaultDataExport = false;
		part = new TableFileLinkagePart(this,new PartProvider());
		parts.add(part);
		basicDataHandler = new MyBasicDataHandler(this,dataTableDescriptor,
				new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		return "OpModeDistribution";
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
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		
		// We don't want this importer to have a red X where it's not necessary
		// 	This is the case when we don't have an off-network link, as the running process
		// 	will have either an average speed or a link drive schedule, making the op mode distribution
		// 	optional but not needed
		
		// query the input database to see if there's an off-network link
		SQLRunner.Query query = new SQLRunner.Query();
		int offNetworkCount = -1;
		String sql = "select count(*) from link where roadTypeID = 1";
		try {
			query.open(db,sql);
			query.rs.next();
			offNetworkCount = query.rs.getInt(1);
			
			query.close();
		} catch(SQLException e) {
			Logger.logSqlError(e,"Unable to count offNetwork links",sql);
		} finally {
			query.onFinally();
		}
		
		// if there isn't, then we can say this is ok
		if (offNetworkCount == 0) {
			return getImporterDataStatusCore(db, false);
		}
		
		// if the count is greater than 0, or -1 (indicates no data in link table),
		// 	 then we need to have the op mode distribution
		boolean hasSourceTypes = manager.tableHasSourceTypes(db,
				"select distinct sourceTypeID from " + primaryTableName,
				this,primaryTableName + " is missing sourceTypeID(s)");
		boolean hasHourDays = manager.tableHasHourDays(db,
				"select distinct hourDayID from " + primaryTableName,
				this,primaryTableName + " is missing hourDayID(s)");
		// We can't check for polProcessID since not all pollutant/processes use
		// operating modes.  Many are chained.
		if(hasSourceTypes && hasHourDays) {
			return getImporterDataStatusCore(db, false);
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
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
		BasicDataHandler.runScript(db,this,messages,requireAllData?2:1,"database/opModeDistributionImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toUpperCase().startsWith("ERROR")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}

}
