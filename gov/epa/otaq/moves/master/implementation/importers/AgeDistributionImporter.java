/**************************************************************************************************
 * @(#)AgeDistributionImporter.java 
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
 * MOVES Age Distribution Importer.  Imports data in MOVES format (as opposed to
 * NMIM or Mobile format) into MOVES.
 * 
 * @author		Wesley Faler
 * @author		Mike Kender	task 1903
 * @version		2019-10-18
**/
public class AgeDistributionImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object **/
	TableFileLinkagePart part;

	/**
	 * Name of the primary table handled by the importer.
	 * Note the MOVES database naming convention.
	**/
	String primaryTableName = "sourceTypeAgeDistribution";

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "SourceTypeAgeDistribution",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"yearID", "Year", ImporterManager.FILTER_YEAR,
		"ageID", "AgeCategory", ImporterManager.FILTER_AGE,
		"ageFraction", "", ImporterManager.FILTER_NON_NEGATIVE
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
			if(type != MOVESDatabaseType.EXECUTION) {
				// Only the execution database has all the supporting tables filled.
				return null;
			}
			String yearsCSV = manager.getFilterValuesCSV(ImporterManager.FILTER_YEAR);
			String sourceTypesCSV = manager.getFilterValuesCSV(ImporterManager.FILTER_SOURCE);
			if(yearsCSV == null || yearsCSV.length() <= 0
					|| sourceTypesCSV == null || sourceTypesCSV.length() <= 0) {
				return null;
			}
			String[] statements = {
				"drop table if exists tempSourceTypeAgePopulation",
				"create table tempSourceTypeAgePopulation"
						+ " select sourceTypeID, yearID, sum(population) as population"
						+ " from sourceTypeAgePopulation"
						+ " where yearID in (" + yearsCSV + ")"
						+ " and sourceTypeID in (" + sourceTypesCSV + ")"
						+ " group by sourceTypeID"
						+ " order by null"
			};
			String sql = "";
			try {
				/*
				TreeSet years = manager.getFilterValuesSet(ImporterManager.FILTER_YEAR);
				if(years == null || years.size() <= 0) {
					return null;
				}
				Integer firstYear = (Integer)years.first();
				Integer lastYear = (Integer)years.last();
				TotalActivityGenerator.setupAgeTables(db);
				TotalActivityGenerator.growPopulation(db,firstYear.intValue(),lastYear.intValue());
				*/
				// Summarize the ages
				for(int i=0;i<statements.length;i++) {
					sql = statements[i];
					SQLRunner.executeSQL(db,sql);
				}
			} catch(Exception e) {
				Logger.logError(e,"Unable to calculate sourceTypeAgeDistribution");
				return null;
			}
			return "select sourceTypeID, yearID, ageID,"
					+ " (case when summary.population > 0 then stap.population/summary.population"
					+ " else 0 end) as ageFraction"
					+ " from sourceTypeAgePopulation stap"
					+ " inner join tempSourceTypeAgePopulation summary using (sourceTypeID,yearID)"
					+ " where stap.yearID in (" + yearsCSV + ")"
					+ " and stap.sourceTypeID in (" + sourceTypesCSV + ")"
					+ " order by sourceTypeID, yearID, ageID";
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
			try {
				if(type != MOVESDatabaseType.EXECUTION) {
					TotalActivityGenerator.removeAgeTables(db);
				}
			} catch(Exception e) {
				Logger.logError(e,"Unable to cleanup source type population after export");
			}
		}
	}

	/** Constructor **/
	public AgeDistributionImporter() {
		super("Age Distribution", // common name
				"agedistribution", // XML node name
				new String[] { "SourceTypeAgeDistribution" } // required tables
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
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		boolean hasYears = manager.tableHasYears(db,
				"select distinct yearID from " + primaryTableName,
				this,primaryTableName + " is missing yearID(s)");
		boolean hasSourceTypes = manager.tableHasSourceTypes(db,
				"select distinct sourceTypeID from " + primaryTableName,
				this,primaryTableName + " is missing sourceTypeID(s)");
        boolean hasAges = manager.tableHasAges(db,
                "select distinct ageID from " + primaryTableName,
                this,primaryTableName + " is missing ageID(s)");
		if(hasYears && hasSourceTypes && hasAges) {
			return getImporterDataStatusCore(db);
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
	
	
	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatusCore(Connection db) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/AgeDistributionImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toUpperCase().startsWith("ERROR")) {
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}
}
