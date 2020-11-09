/**************************************************************************************************
 * @(#)RoadTypeDistributionImporter.java 
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
import gov.epa.otaq.moves.common.MOVESDatabaseType;

/**
 * MOVES RoadTypeDistribution Data Importer.
 * 
 * @author		Wesley Faler
 * @version		2015-09-16
**/
public class RoadTypeDistributionImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the RoadTypeDistribution table **/
	TableFileLinkagePart part;

	/**
	 * Name of the primary table handled by the importer.
	 * Note the MOVES database naming convention.
	**/
	String primaryTableName = "roadTypeDistribution";

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "RoadTypeDistribution",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"roadTypeID", "RoadType", ImporterManager.FILTER_ROAD_TYPE_NOT_OFFNETWORK,
		"roadTypeVMTFraction", "", ImporterManager.FILTER_NON_NEGATIVE
	};

	/** Class for editing the table data source **/
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
	public RoadTypeDistributionImporter() {
		super("Road Type Distribution", // common name
				"roadtypedistribution", // XML node name
				new String[] { "RoadTypeDistribution" } // required tables
				);
		subjectToExportRestrictions = true;
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
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		boolean hasSourceTypes = manager.tableHasSourceTypes(db,
				"select distinct sourceTypeID from " + primaryTableName,
				this,primaryTableName + " is missing sourceTypeID(s)");
		boolean hasRoadTypes = manager.tableHasNonOffnetworkRoadTypes(db, // tableHasRoadTypes
				"select distinct roadTypeID from " + primaryTableName,
				this,primaryTableName + " is missing roadTypeID(s)");
		if(hasSourceTypes && hasRoadTypes) {
			return getImporterDataStatus(db);
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatus(Connection db) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/RoadTypeDistributionImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toUpperCase().startsWith("ERROR")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}
}
