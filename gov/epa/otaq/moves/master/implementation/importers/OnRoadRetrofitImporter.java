/**************************************************************************************************
 * @(#)OnRoadRetrofitImporter.java
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
import gov.epa.otaq.moves.common.*;
import java.util.*;
import gov.epa.otaq.moves.master.framework.SystemConfiguration;

/**
 * MOVES OnRoad Retrofit Data Importer.
 *
 * @author		Wesley Faler
 * @version		2012-09-29
**/
public class OnRoadRetrofitImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the OnRoadRetrofit table **/
	TableFileLinkagePart part;

	/**
	 * Name of the primary table handled by the importer.
	 * Note the MOVES database naming convention.
	**/
	String primaryTableName = "onRoadRetrofit";

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "onRoadRetrofit",
		"pollutantID", "Pollutant", ImporterManager.FILTER_POLLUTANT,
		"processID", "Process", ImporterManager.FILTER_PROCESS,
		"fuelTypeID", "FuelType", ImporterManager.FILTER_FUEL,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"retrofitYearID", "", ImporterManager.FILTER_AGEDYEAR,
		"beginModelYearID", "", ImporterManager.FILTER_AGEDMODELYEAR,
		"endModelYearID", "", ImporterManager.FILTER_AGEDMODELYEAR,
		"cumFractionRetrofit", "", ImporterManager.FILTER_0_TO_1_FRACTION,
		"retrofitEffectiveFraction", "", "" // This can be negative, implying a retrofit increased pollution
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

	/** Internal class used to restrict the template filters **/
	class PrivateBasicDataHandler extends BasicDataHandler {
		/**
		 * Constructor
		 * @param importerToUse importer for this data handler
		 * @param descriptorToUse descriptor for the table(s) used by this importer
		 * @param importDataProviderToUse Provider for data required during imports
		**/
		public PrivateBasicDataHandler(IImporter importerToUse, String[] descriptorToUse,
				IProvider importDataProviderToUse) {
			super(importerToUse,descriptorToUse,importDataProviderToUse);
		}

		/**
		 * Called for each row in a template to accept or reject the combination.
		 * @param value array of objects for each column in the template
		 * @return true if the row should be written
		**/
		public boolean shouldWriteTemplateRow(String tableName, Object[] values) {
			// The retrofit importer should produce an empty template.
			return false;
		}
	}

	/** Constructor **/
	public OnRoadRetrofitImporter() {
		super("Retrofit Data", // common name
				"onroadretrofit", // XML node name
				new String[] { "onRoadRetrofit" } // required tables
				);
		shouldDoExecutionDataExport = false;
		shouldDoDefaultDataExport = false;
		part = new TableFileLinkagePart(this,new PartProvider());
		parts.add(part);
		basicDataHandler = new PrivateBasicDataHandler(this,dataTableDescriptor,new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		return "OnRoadRetrofit";
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
	 * Look for an OK or a NOT_READY message from the OnRoadRetrofitImporter.sql script
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatusCore(Connection db) throws Exception {
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		boolean hasCount = SQLRunner.executeScalar(db,"select count(*) from onRoadRetrofit") >0;
		if(!hasCount) {
			// It is acceptable to have no records.
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}

		// Pollutants, processes, fuels, and source types can be missing, so no completeness check can be done.

		// Do a detailed check of each record
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,2,"database/OnRoadRetrofitImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.equalsIgnoreCase("OK")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
			} else if(t.equalsIgnoreCase("NOT_READY")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}

		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}
}
