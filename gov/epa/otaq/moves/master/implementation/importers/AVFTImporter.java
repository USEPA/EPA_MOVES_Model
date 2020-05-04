/**************************************************************************************************
 * @(#)AVFTImporter.java
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
 * MOVES AVFT Data Importer.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @version		2013-09-15
**/
public class AVFTImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the AVFT table **/
	TableFileLinkagePart part;

	/**
	 * Name of the primary table handled by the importer.
	 * Note the MOVES database naming convention.
	**/
	String primaryTableName = "avft";

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "avft",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"modelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"fuelTypeID", "FuelType", ImporterManager.FILTER_FUEL,
		"engTechID", "EngineTech", "",
		"fuelEngFraction", "", ImporterManager.FILTER_NON_NEGATIVE
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

			if( type != MOVESDatabaseType.DEFAULT ){
				return null;
			}

			String sourceTypesCSV = manager.getFilterValuesCSV(ImporterManager.FILTER_SOURCE);
			if( sourceTypesCSV == null || sourceTypesCSV.length() <= 0) {
				return null;
			}

			String sql = "select svp.sourceTypeID, svp.modelYearID, fuelTypeID, engTechID, sum(stmyFraction) as fuelEngFraction"
					+ " from sampleVehiclePopulation svp"
					+ " where sourceTypeID in (" + sourceTypesCSV + ")"
					+ " group by svp.sourceTypeModelYearID, fuelTypeID, engTechID"
					+ " order by svp.sourceTypeModelYearID, fuelTypeID, engTechID";

			return sql;
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

	class CustomBasicDataHandler extends BasicDataHandler {
		/**
		 * Constructor
		 * @param importerToUse importer for this data handler
		 * @param descriptorToUse descriptor for the table(s) used by this importer
		 * @param importDataProviderToUse Provider for data required during imports
		**/
		public CustomBasicDataHandler(IImporter importerToUse, String[] descriptorToUse,
				IProvider importDataProviderToUse) {
			super(importerToUse,descriptorToUse,importDataProviderToUse);
		}

		/**
		 * Alter the name of a filter during template creation.  Used, for instance,
		 * to build a template using all fuel types rather than just those in the
		 * runspec.
		 * @param tableName name of the current table.
		 * @param filterName name of the ImporterManager filter.
		 * @return the name of the ImporterManager filter to be used.  Never null, never blank.
		**/
		public String adjustTemplateFilterName(String tableName, String filterName) {
			if(filterName.equalsIgnoreCase(ImporterManager.FILTER_SOURCE)
					|| filterName.equalsIgnoreCase(ImporterManager.FILTER_FUEL)) {
				// If the runspec has no fuel or source type listed, then use all
				// fuels or sources instead of nothing.
				if(getImporterManager().getFilterValues(filterName).size() <= 0) {
					return "ALL_" + filterName;
				}
			}
			return filterName;
		}

		/**
		 * Check the applicability of a filter during export of default data.
		 * @param tableName the current table.
		 * @param filterName name of the ImporterManager filter.
		 * @return true if the filter should be used
		**/
		public boolean shouldUseFilterForExport(String tableName, String filterName) {
			if(filterName.equalsIgnoreCase(ImporterManager.FILTER_FUEL)) {
				return false;
			}
			return true;
		}
	}

	/** Constructor **/
	public AVFTImporter() {
		super("Fueltype and Technologies", // common name
				"avft", // XML node name
				new String[] { "avft" } // required tables
				);
		part = new TableFileLinkagePart(this,new PartProvider());
		parts.add(part);
		basicDataHandler = new CustomBasicDataHandler(this,dataTableDescriptor,new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		return "AVFT";
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
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		boolean hasCount = SQLRunner.executeScalar(db,"select count(*) from AVFT") >0;
		if(!hasCount) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}

		boolean hasSourceType = manager.tableHasSourceTypes(db,
				"select distinct sourceTypeID from AVFT");
		if(!hasSourceType) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		TreeSet sourceTypes = manager.getFilterValuesSet(ImporterManager.FILTER_SOURCE);
		TreeSet years = manager.getFilterValuesSet(ImporterManager.FILTER_YEAR);
		TreeSet<Object> modelYears = new TreeSet<Object>();
		for( Iterator i=years.iterator(); i.hasNext(); ){
			Object yo = i.next();
			if( yo instanceof Integer ){
				int year = ((Integer)yo).intValue();
				for( int age=0; age<=30; age++){
					modelYears.add(new Integer(year-age));
				}
			}
		}
		for( Iterator i=sourceTypes.iterator(); i.hasNext(); ){
			Object yo = i.next();
			boolean hasModelYears = manager.tableHasIntegers(db,
					"select distinct modelYearID from AVFT where sourceTypeID = "+yo,
					modelYears);
			if(!hasModelYears ) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}

		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,requireAllData?2:1,"database/AVFTImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toUpperCase().startsWith("ERROR")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}
}

