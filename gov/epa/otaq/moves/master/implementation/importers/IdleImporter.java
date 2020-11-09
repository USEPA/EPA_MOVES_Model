/**************************************************************************************************
 * @(#)IdleImporter.java
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
 * MOVES Importer for TotalIdleFraction, IdleModelYearGrouping, IdleMonthAdjust, and IdleDayAdjust.
 *
 * @author		Wesley Faler
 * @author		Jarrod Brown
 * @version		2018-06-22
**/
public class IdleImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the totalIdleFraction table **/
	TableFileLinkagePart totalIdleFractionPart;
	/** Part object for the idleModelYearGrouping table **/
	TableFileLinkagePart idleModelYearGroupingPart;
	/** Part object for the idleMonthAdjust table **/
	TableFileLinkagePart idleMonthAdjustPart;
	/** Part object for the idleDayAdjust table **/
	TableFileLinkagePart idleDayAdjustPart;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "totalIdleFraction",
		"idleRegionID", "IdleRegion", ImporterManager.FILTER_IDLEREGION,
		"countyTypeID", "CountyType", ImporterManager.FILTER_COUNTYTYPE,
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"dayID", "DayOfAnyWeek", ImporterManager.FILTER_DAY,
		"minModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"maxModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"totalIdleFraction", "", ImporterManager.FILTER_0_TO_1_FRACTION,

		BasicDataHandler.BEGIN_TABLE, "idleModelYearGrouping",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"minModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"maxModelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"totalIdleFraction", "", ImporterManager.FILTER_0_TO_1_FRACTION,

		BasicDataHandler.BEGIN_TABLE, "idleMonthAdjust",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"idleMonthAdjust", "", ImporterManager.FILTER_NON_NEGATIVE_DEFAULT_1,

		BasicDataHandler.BEGIN_TABLE, "idleDayAdjust",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"dayID", "DayOfAnyWeek", ImporterManager.FILTER_DAY,
		"idleDayAdjust", "", ImporterManager.FILTER_NON_NEGATIVE_DEFAULT_1
	};

	/** Class for editing the data source **/
	class TotalIdleFractionPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "totalIdleFraction";
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
	class IdleModelYearGroupingPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "idleModelYearGrouping";
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
	class IdleMonthAdjustPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "idleMonthAdjust";
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
	class IdleDayAdjustPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "idleDayAdjust";
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
			if(tableName.equalsIgnoreCase("totalIdleFraction")) {
				return totalIdleFractionPart.fileName;
			} else if(tableName.equalsIgnoreCase("idleModelYearGrouping")) {
				return idleModelYearGroupingPart.fileName;
			} else if(tableName.equalsIgnoreCase("idleMonthAdjust")) {
				return idleMonthAdjustPart.fileName;
			} else if(tableName.equalsIgnoreCase("idleDayAdjust")) {
				return idleDayAdjustPart.fileName;
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
			if(tableName.equalsIgnoreCase("totalIdleFraction")) {
				return totalIdleFractionPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("idleModelYearGrouping")) {
				return idleModelYearGroupingPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("idleMonthAdjust")) {
				return idleMonthAdjustPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("idleDayAdjust")) {
				return idleDayAdjustPart.worksheetName;
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
			if(type != MOVESDatabaseType.EXECUTION && type != MOVESDatabaseType.DEFAULT) {
				// Only the execution database or the default has all the supporting tables filled.
				return null;
			}
			// Nothing to do here
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
	public IdleImporter() {
		super("Idle", // common name
				"idle", // XML node name
				new String[] { "totalIdleFraction", "idleModelYearGrouping",
						"idleMonthAdjust", "idleDayAdjust" } // required tables
				);
		shouldDoExecutionDataExport = false;
		shouldDoDefaultDataExport = true;
		subjectToExportRestrictions = false;

		totalIdleFractionPart = new TableFileLinkagePart(this,new TotalIdleFractionPartProvider());
		parts.add(totalIdleFractionPart);
		idleModelYearGroupingPart = new TableFileLinkagePart(this,new IdleModelYearGroupingPartProvider());
		parts.add(idleModelYearGroupingPart);
		idleMonthAdjustPart = new TableFileLinkagePart(this,new IdleMonthAdjustPartProvider());
		parts.add(idleMonthAdjustPart);
		idleDayAdjustPart = new TableFileLinkagePart(this,new IdleDayAdjustPartProvider());
		parts.add(idleDayAdjustPart);

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
		return "Idle";
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
		boolean hasTotalIdleFractionRecords = false;
		boolean hasIdleModelYearGroupingRecords = false;
		boolean hasIdleMonthAdjustRecords = false;
		boolean hasIdleDayAdjustRecords = false;
		
		if(SQLRunner.executeScalar(db,"select count(*) from totalIdleFraction") > 0) {
			hasTotalIdleFractionRecords = true;
			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from totalIdleFraction",
					this,"totalIdleFraction is missing sourceTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasMonths(db,"select distinct monthID from totalIdleFraction",
					this,"totalIdleFraction is missing monthID(s)")) {
				return false;
			}
			if(!manager.tableHasDays(db,"select distinct dayID from totalIdleFraction",
					this,"totalIdleFraction is missing dayID(s)")) {
				return false;
			}
		}

		int howManyFractionTablesHaveData = 0;
		if(SQLRunner.executeScalar(db,"select count(*) from idleModelYearGrouping") > 0) {
			hasIdleModelYearGroupingRecords = true;
			howManyFractionTablesHaveData++;
			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from idleModelYearGrouping",
					this,"idleModelYearGrouping is missing sourceTypeID(s)")) {
				return false;
			}
		}

		if(SQLRunner.executeScalar(db,"select count(*) from idleMonthAdjust") > 0) {
			hasIdleMonthAdjustRecords = true;
			howManyFractionTablesHaveData++;

			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from idleMonthAdjust",
					this,"idleMonthAdjust is missing sourceTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasMonths(db,"select distinct monthID from idleMonthAdjust",
					this,"idleMonthAdjust is missing monthID(s)")) {
				return false;
			}
			
			if (SQLRunner.executeScalar(db,"select count(*) from totalIdleFraction") > 0) {
				addQualityMessage("ERROR: idleMonthAdjust cannot be used in combination with totalIdleFraction");
				return false;
			}
		}

		if(SQLRunner.executeScalar(db,"select count(*) from idleDayAdjust") > 0) {
			hasIdleDayAdjustRecords = true;
			howManyFractionTablesHaveData++;

			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from idleDayAdjust",
					this,"idleDayAdjust is missing sourceTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasDays(db,"select distinct dayID from idleDayAdjust",
					this,"idleDayAdjust is missing dayID(s)")) {
				return false;
			}

			if (SQLRunner.executeScalar(db,"select count(*) from totalIdleFraction") > 0) {
				addQualityMessage("ERROR: idleDayAdjust cannot be used in combination with totalIdleFraction");
				return false;
			}
		}

		if(hasTotalIdleFractionRecords && hasIdleModelYearGroupingRecords) {
			addQualityMessage("ERROR: Either provide a totalIdleFraction table or the idleModelYearGrouping table, but not both.");
			return false;
		}
		
		if(hasIdleModelYearGroupingRecords && !hasIdleMonthAdjustRecords) {
			addQualityMessage("idleMonthAdjust should be supplied when using the idleModelYearGrouping table.");
		}
		
		if(hasIdleModelYearGroupingRecords && !hasIdleDayAdjustRecords) {
			addQualityMessage("idleDayAdjust should be supplied when using the idleModelYearGrouping table.");
		}

		return true;
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
	private boolean getDataStatusFromScript(Connection db) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/IdleImporter.sql");
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
