/**************************************************************************************************
 * @(#)FuelImporterImporter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;

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
 * MOVES Fuel Formulation Data Importer.  Imports data in MOVES format (as opposed to
 * NMIM or Mobile format) into the MOVES FuelFormulation table.
 *
 * @author		Don Smith
 * @author		Wesley Faler
 * @version		2013-01-21
**/
public class FuelFormulationImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the FuelSupply table **/
	TableFileLinkagePart fuelFormulationPart;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "FuelFormulation",
		"fuelFormulationID", "", "",
		"fuelSubtypeID", "FuelSubtype", ImporterManager.FILTER_FUEL_SUBTYPE,
		"RVP", "", "",
		"sulfurLevel", "", "",
		"ETOHVolume", "", "",
		"MTBEVolume", "", "",
		"ETBEVolume", "", "",
		"TAMEVolume", "", "",
		"aromaticContent", "", "",
		"olefinContent", "", "",
		"benzeneContent", "", "",
		"e200", "", "",
		"e300", "", "",
		"*volToWtPercentOxy", "", "",
		"bioDieselEsterVolume", "", "",
		"cetaneIndex", "", "",
		"PAHContent", "", ""
	};

	/** Class for editing the fuelFormulation data source **/
	class FuelFormulationTablePartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "fuelFormulation"; // NOTE: MOVES database naming convention used
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
			if(tableName.equalsIgnoreCase("fuelFormulation")) {
				return fuelFormulationPart.fileName;
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
			if(tableName.equalsIgnoreCase("fuelFormulation")) {
				return fuelFormulationPart.worksheetName;
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
	public FuelFormulationImporter() {
		super("Fuel Formulation", // common name
				"fuelformulation", // XML node name
				new String[] { "FuelFormulation" } // required tables
				);
		shouldDoDefaultDataExport = true;
		shouldDoCustomDefaultDataExport = true;
		fuelFormulationPart = new TableFileLinkagePart(this,new FuelFormulationTablePartProvider());
		parts.add(fuelFormulationPart);
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
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		/*
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		boolean hasCounties = manager.tableHasCounties(db,
				"select distinct countyID from fuelSupply");
		boolean hasYears = manager.tableHasYears(db,
				"select distinct yearID from fuelSupply"
				+ " inner join year using (fuelYearID)");
		if(hasCounties && hasYears) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		*/
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
}
