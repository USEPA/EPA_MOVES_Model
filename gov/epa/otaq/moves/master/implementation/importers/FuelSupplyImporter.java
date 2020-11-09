/**************************************************************************************************
 * @(#)FuelSupplyImporter.java 
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
import gov.epa.otaq.moves.master.framework.SystemConfiguration;

/**
 * MOVES Fuel Supply Data Importer.  Imports data in MOVES format (as opposed to
 * NMIM or Mobile format) into the MOVES FuelSupply table.
 * 
 * @author		Wesley Faler
 * @version		2015-09-16
**/
public class FuelSupplyImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the FuelSupply table **/
	TableFileLinkagePart fuelSupplyPart;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "FuelSupply",
		"countyID", "County", ImporterManager.FILTER_COUNTY,
		"fuelYearID", "FuelSupplyYear", ImporterManager.FILTER_FUEL_YEAR,
		"monthGroupID", "MonthGroupOfAnyYear", ImporterManager.FILTER_MONTH_GROUP,
		"fuelFormulationID", "FuelFormulation", "", // ImporterManager.FILTER_FUEL_FORMULATION,
		"marketShare", "", ImporterManager.FILTER_MARKET_SHARE,
		"marketShareCV", "", ""
	};

	/** Class for editing the fuelSupply data source **/
	class FuelSupplyTablePartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "fuelSupply"; // NOTE: MOVES database naming convention used
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
			if(tableName.equalsIgnoreCase("fuelSupply")) {
				return fuelSupplyPart.fileName;
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
			if(tableName.equalsIgnoreCase("fuelSupply")) {
				return fuelSupplyPart.worksheetName;
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
	public FuelSupplyImporter() {
		super("Fuel Supply", // common name
				"fuelsupply", // XML node name
				new String[] { "FuelSupply" } // required tables
				);
		fuelSupplyPart = new TableFileLinkagePart(this,new FuelSupplyTablePartProvider());
		parts.add(fuelSupplyPart);
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
		boolean hasCounties = manager.tableHasCounties(db,
				"select distinct countyID from fuelSupply",
				this,"fuelSupply is missing countyID(s)");
		if(!hasCounties) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		boolean hasYears = manager.tableHasYears(db,
				"select distinct yearID from fuelSupply"
				+ " inner join year using (fuelYearID)",
				this,"fuelSupply is missing fuels from year(s)");
		if(!hasYears) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}

		String defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
		boolean hasFuels = manager.tableHasFuelTypes(db,
				"select distinct fuelTypeID"
				+ " from fuelSupply fs"
				+ " inner join " + defaultDatabaseName + ".fuelFormulation ff using (fuelFormulationID)"
				+ " inner join " + defaultDatabaseName + ".fuelSubType fst using (fuelSubTypeID)"
				+ " union"
				+ " select distinct fuelTypeID"
				+ " from fuelSupply fs"
				+ " inner join fuelFormulation ff using (fuelFormulationID)"
				+ " inner join " + defaultDatabaseName + ".fuelSubType fst using (fuelSubTypeID)",
				this,"fuelSupply is missing forumulations for fuelTypeID(s)");
		if(!hasFuels) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
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
		BasicDataHandler.runScript(db,this,messages,requireAllData?2:1,"database/FuelSupplyImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toUpperCase().startsWith("ERROR")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}
}
