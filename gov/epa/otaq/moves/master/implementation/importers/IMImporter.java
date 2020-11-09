/**************************************************************************************************
 * @(#)IMImporter.java
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
import gov.epa.otaq.moves.master.framework.DatabaseConnectionManager;

/**
 * MOVES IM Importer.  Imports data in MOVES format (as opposed to
 * NMIM or Mobile format) into the MOVES IM tables.
 *
 * @author		Don Smith
 * @author		Wesley Faler
 * @author		W. Aikman
 * @version		2012-09-08
**/
public class IMImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the IM tables **/
	TableFileLinkagePart imCoveragePart;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/

	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "IMCoverage",
		"polProcessID", "IMPollutantProcessAssoc", ImporterManager.FILTER_POLPROCESSID_IM,
		"stateID", "", ImporterManager.FILTER_STATE,
		"countyID", "CountyState", ImporterManager.FILTER_COUNTY,
		"yearID", "", ImporterManager.FILTER_YEAR,
		"sourcetypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"fuelTypeID", "FuelType", ImporterManager.FILTER_FUEL,
		"IMProgramID", "", ImporterManager.FILTER_IMPROGRAMID,
		"inspectFreq", "IMInspectFreq", ImporterManager.FILTER_INSPECTFREQ,
		"testStandardsID", "IMTestStandards", ImporterManager.FILTER_TESTSTANDARDSID,
		"begModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"endModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"useIMyn", "", ImporterManager.FILTER_YN,
		"complianceFactor", "", ImporterManager.FILTER_NON_NEGATIVE
	};

	/** Class for editing the IMCoverage data source **/
	class IMCoverageTablePartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "IMCoverage"; // NOTE: MOVES database naming convention used
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
			if(tableName.equalsIgnoreCase("IMCoverage")) {
				return imCoveragePart.fileName;
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
			if(tableName.equalsIgnoreCase("IMCoverage")) {
				return imCoveragePart.worksheetName;
			}
			return null;
		}

		/**
		 * Get the CSV list of values for a filter from the manager, using "0" if there
		 * are no selected values in the filter.
		 * @param filterName one of the ImporterManager.FILTER_* constants
		 * @return a CSV list of values for the filter or "0" if there were none
		**/
		private String getCSV(String filterName) {
			String values = manager.getFilterValuesCSV(filterName);
			if(values == null || values.length() <= 0) {
				return "0";
			}
			return values;
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
			String sql = "select polProcessID,stateID,countyID,yearID,sourcetypeID,"
					+ " fuelTypeID,IMProgramID,inspectFreq,testStandardsID,begModelYearID,"
					+ " endModelYearID,useIMyn,complianceFactor"
					+ " from IMCoverage"
					+ " where polProcessID in (" + getCSV(ImporterManager.FILTER_POLPROCESSID) + ")"
					+ " and countyID in (" + getCSV(ImporterManager.FILTER_COUNTY) + ")"
					+ " and yearID in (" + getCSV(ImporterManager.FILTER_YEAR) + ")"
					+ " and sourceTypeID in (" + getCSV(ImporterManager.FILTER_SOURCE) + ")"
					+ " and fuelTypeID in (" + getCSV(ImporterManager.FILTER_FUEL) + ")"
					+ " and (sourceTypeID*100+fuelTypeID) in (" + getCSV(ImporterManager.FILTER_SOURCEFUELTYPE) + ")"
					+ " order by polProcessID,stateID,countyID,yearID,sourcetypeID,fuelTypeID,IMProgramID";
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

	/** Internal class used to restrict the template filters **/
	class PrivateBasicDataHandler extends BasicDataHandler {
		/** polProcessID|sourceTypeID|fuelTypeID combinations allowed by imFactor **/
		TreeSet<String> keys = new TreeSet<String>();

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
		 * Add a filter to the list of filters.  This is an overridable routine for classes
		 * that wish to filter the filters.  If rejecting a filter, insert "" into the list
		 * of filters.
		 * @param filterNames all filter names seen so far
		 * @param filterName active filter
		**/
		public void addTemplateFilterName(ArrayList<String> filterNames, String filterName) {
			if(filterName.equalsIgnoreCase(ImporterManager.FILTER_TESTSTANDARDSID)
					|| filterName.equalsIgnoreCase(ImporterManager.FILTER_INSPECTFREQ)
					|| filterName.equalsIgnoreCase(ImporterManager.FILTER_SOURCE)
					|| filterName.equalsIgnoreCase(ImporterManager.FILTER_FUEL)
						) {
				filterNames.add("");
			} else {
				filterNames.add(filterName);
			}
		}

		/** Event called when a template is being initiated **/
		public void onBeginTemplate() {
			keys.clear();

			String sql = "select distinct polProcessID from imFactor";
			SQLRunner.Query query = new SQLRunner.Query();
			Connection db = null;
			try {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
				if(db != null) {
					query.open(db,sql);
					while(query.rs.next()) {
						String key = query.rs.getString(1);
						keys.add(key);
					}
					query.close();
				}
			} catch(SQLException e) {
				query.onException(e,"Unable to get imFactor filters",sql);
			} catch(Exception e) {
				Logger.logError(e,"Unable to get imFactor filters with: " + sql);
			} finally {
				query.onFinally();
				if(db != null) {
					try {
						DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
					} catch(Exception e) {
						// Nothing to do here
					}
					db = null;
				}
			}
			// Nothing to do here
		}

		/**
		 * Called for each row in a template to accept or reject the combination.
		 * @param value array of objects for each column in the template
		 * @return true if the row should be written
		**/
		public boolean shouldWriteTemplateRow(Object[] values) {
			// Only approve the row if it contains a polProcessID combination present in imFactor,
			// as read during onBeginTemplate.
			// According to the layout above, polProcessID is values[0]
			if(values.length <= 5) {
				return false;
			}
			String key = "";
			if(values[0] != null) {
				key += values[0].toString();
			} else {
				return false;
			}
			return keys.contains(key);
		}
	}

	/** Constructor **/
	public IMImporter() {
		super("I/M Programs", // common name
				"imcoverage", // XML node name
				new String[] { "IMCoverage" } // required tables
				);
		shouldDoDefaultDataExport = true;
		shouldDoExecutionDataExport = false;

		if(ImporterInstantiator.activeManager.isCounty() || ImporterInstantiator.activeManager.isProject()) {
			imCoveragePart = new TableFileLinkagePart(this,new IMCoverageTablePartProvider(),null,true,"No I/M Program");
		} else {
			imCoveragePart = new TableFileLinkagePart(this,new IMCoverageTablePartProvider());
		}

		parts.add(imCoveragePart);
		basicDataHandler = new PrivateBasicDataHandler(this,dataTableDescriptor,
				new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		return "IMCoverage";
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
		if(imCoveragePart.isNoDataNeeded()) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,2,"database/IMCoverageImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.equalsIgnoreCase("OK")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
			} else if(t.equalsIgnoreCase("NOT_READY")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return null; // no comment
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
	 * Look for an OK or a NOT_READY message from the IMCoverageImporter.sql script
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatus(Connection db) throws Exception {
		if(imCoveragePart.isNoDataNeeded()) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/IMCoverageImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.equalsIgnoreCase("OK")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
			} else if(t.equalsIgnoreCase("NOT_READY")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return null; // no comment
	}
}
