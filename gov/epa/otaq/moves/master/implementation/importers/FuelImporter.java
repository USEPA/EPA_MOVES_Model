/**************************************************************************************************
 * @(#)FuelImporter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;

import java.io.*;
import java.sql.*;
import java.util.*;
import javax.xml.parsers.*;
import java.awt.Component;
import javax.swing.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.avfttool.AVFTTool;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

/**
 * MOVES Fuels Importer.
 *
 * @author		Wesley Faler
 * @version		2015-09-17
**/
public class FuelImporter extends ImporterBase {
	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;

	/** Part object for the FuelSupply table **/
	TableFileLinkagePart fuelSupplyPart;

	/** Part object for the FuelSupply table **/
	TableFileLinkagePart fuelFormulationPart;

	/** Part object for the FuelUsageFraction table **/
	TableFileLinkagePart fuelUsageFractionPart;

	/** Part object for the AVFT table. Public so the AVFTTool can access it **/
	public TableFileLinkagePart avftPart;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "FuelSupply",
		"fuelRegionID", "Region", ImporterManager.FILTER_FUEL_REGION,
		"fuelYearID", "FuelSupplyYear", ImporterManager.FILTER_FUEL_YEAR,
		"monthGroupID", "MonthGroupOfAnyYear", ImporterManager.FILTER_MONTH_GROUP,
		"fuelFormulationID", "", "", // ImporterManager.FILTER_FUEL_FORMULATION,
		"marketShare", "", ImporterManager.FILTER_MARKET_SHARE,
		"marketShareCV", "", "",

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
		"PAHContent", "", "",
		"T50", "", "",
		"T90", "", "",

		BasicDataHandler.BEGIN_TABLE, "FuelUsageFraction",
		"countyID", "County", ImporterManager.FILTER_COUNTY,
		"fuelYearID", "FuelSupplyYear", ImporterManager.FILTER_FUEL_YEAR,
		"modelYearGroupID", "ModelYearGroup", ImporterManager.FILTER_MODEL_YEAR_RANGE,
		"sourceBinFuelTypeID", "FuelType", ImporterManager.FILTER_FUEL,
		"fuelSupplyFuelTypeID", "FuelType", ImporterManager.FILTER_FUEL,
		"usageFraction", "", ImporterManager.FILTER_0_TO_1_FRACTION,

		BasicDataHandler.BEGIN_TABLE, "avft",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"modelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"fuelTypeID", "FuelType", ImporterManager.FILTER_FUEL,
		"engTechID", "EngineTech", ImporterManager.FILTER_ONROAD_ENGTECHID,
		"fuelEngFraction", "", ImporterManager.FILTER_NON_NEGATIVE
	};

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * Does not contain the FuelUsageFraction table.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptorNoUsage = {
		BasicDataHandler.BEGIN_TABLE, "FuelSupply",
		"fuelRegionID", "Region", ImporterManager.FILTER_FUEL_REGION,
		"fuelYearID", "FuelSupplyYear", ImporterManager.FILTER_FUEL_YEAR,
		"monthGroupID", "MonthGroupOfAnyYear", ImporterManager.FILTER_MONTH_GROUP,
		"fuelFormulationID", "", "", // ImporterManager.FILTER_FUEL_FORMULATION,
		"marketShare", "", ImporterManager.FILTER_MARKET_SHARE,
		"marketShareCV", "", "",

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
		//"volToWtPercentOxy", "", "",
		"bioDieselEsterVolume", "", "",
		"cetaneIndex", "", "",
		"PAHContent", "", "",
		"T50", "", "",
		"T90", "", "",

		BasicDataHandler.BEGIN_TABLE, "avft",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"modelYearID", "", ImporterManager.FILTER_MODELYEARID,
		"fuelTypeID", "FuelType", ImporterManager.FILTER_FUEL,
		"engTechID", "EngineTech", ImporterManager.FILTER_ONROAD_ENGTECHID,
		"fuelEngFraction", "", ImporterManager.FILTER_NON_NEGATIVE
	};

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer in Nonroad mode.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] nonroadDataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "nrFuelSupply",
		"fuelRegionID", "Region", ImporterManager.FILTER_FUEL_REGION,
		"fuelYearID", "FuelSupplyYear", ImporterManager.FILTER_FUEL_YEAR,
		"monthGroupID", "MonthGroupOfAnyYear", ImporterManager.FILTER_MONTH_GROUP,
		"fuelFormulationID", "", "", // ImporterManager.FILTER_FUEL_FORMULATION,
		"marketShare", "", ImporterManager.FILTER_MARKET_SHARE,
		"marketShareCV", "", "",

		BasicDataHandler.BEGIN_TABLE, "FuelFormulation",
		"fuelFormulationID", "", "",
		"fuelSubtypeID", "nrFuelSubtype", ImporterManager.FILTER_FUEL_SUBTYPE,
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
		"PAHContent", "", "",
		"T50", "", "",
		"T90", "", ""
	};

	/** Class for editing the data source **/
	class FuelSupplyProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "FuelSupply";
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
	class NonroadFuelSupplyProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "nrFuelSupply";
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
	class FuelFormulationProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "FuelFormulation";
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
	class FuelUsageFractionProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "FuelUsageFraction";
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
	class AVFTProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "AVFT";
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
			if(tableName.equalsIgnoreCase("FuelSupply") || tableName.equalsIgnoreCase("nrFuelSupply")) {
				return fuelSupplyPart.fileName;
			} else if(tableName.equalsIgnoreCase("FuelFormulation")) {
				return fuelFormulationPart.fileName;
			} else if(CompilationFlags.USE_FUELUSAGEFRACTION && tableName.equalsIgnoreCase("FuelUsageFraction")) {
				return fuelUsageFractionPart.fileName;
			} else if(tableName.equalsIgnoreCase("AVFT")) {
				return avftPart.fileName;
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
			if(tableName.equalsIgnoreCase("FuelSupply") || tableName.equalsIgnoreCase("nrFuelSupply")) {
				return fuelSupplyPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("FuelFormulation")) {
				return fuelFormulationPart.worksheetName;
			} else if(CompilationFlags.USE_FUELUSAGEFRACTION && tableName.equalsIgnoreCase("FuelUsageFraction")) {
				return fuelUsageFractionPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("AVFT")) {
				return avftPart.worksheetName;
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
			if(type == MOVESDatabaseType.DEFAULT && tableName.equalsIgnoreCase("AVFT")) {
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
			if(tableName.equalsIgnoreCase("FuelSupply") || tableName.equalsIgnoreCase("nrFuelSupply")) {
				String regionIDs = manager.getFilterValuesCSV(ImporterManager.FILTER_FUEL_REGION);
				String fuelYearIDs = manager.getFilterValuesCSV(ImporterManager.FILTER_FUEL_YEAR);
				String monthGroupIDs = manager.getFilterValuesCSV(ImporterManager.FILTER_MONTH_GROUP);
				String formulationIDs = manager.getFilterValuesCSV(ImporterManager.FILTER_FUEL_FORMULATION);

				if(regionIDs == null || regionIDs.length() <= 0
						|| fuelYearIDs == null || fuelYearIDs.length() <= 0
						|| monthGroupIDs == null || monthGroupIDs.length() <= 0
						|| formulationIDs == null || formulationIDs.length() <= 0) {
					return null;
				}

				String sql = "select fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID, marketShare, marketShareCV"
						+ " from " + tableName
						+ " where fuelRegionID in (" + regionIDs + ")"
						+ " and fuelYearID in (" + fuelYearIDs + ")"
						+ " and monthGroupID in (" + monthGroupIDs + ")"
						+ (type == MOVESDatabaseType.DEFAULT?
							" and fuelFormulationID in (" + formulationIDs + ")"
							:
							"")
						+ " order by fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID";
				//System.out.println(sql);
				return sql;
			}
			if(tableName.equalsIgnoreCase("FuelFormulation")) {
				String fuelSupplyTable = manager.isNonroad()? "nrFuelSupply" : "fuelSupply";
				String subtypes = manager.getFilterValuesCSV(ImporterManager.FILTER_FUEL_SUBTYPE);
				//System.out.println("subtypes=" + subtypes);

				if(subtypes == null || subtypes.length() <= 0) {
					return null;
				}

				if(type == MOVESDatabaseType.DEFAULT) {
					// Export only the system fuels and formulations used by the default fuel supply
					String regionIDs = manager.getFilterValuesCSV(ImporterManager.FILTER_FUEL_REGION);
					String fuelYearIDs = manager.getFilterValuesCSV(ImporterManager.FILTER_FUEL_YEAR);
					String monthGroupIDs = manager.getFilterValuesCSV(ImporterManager.FILTER_MONTH_GROUP);
	
					if(regionIDs != null && regionIDs.length() > 0
							&& fuelYearIDs != null && fuelYearIDs.length() > 0
							&& monthGroupIDs != null && monthGroupIDs.length() > 0) {
						String sql = "select distinct fuelFormulationID"
								+ " from " + fuelSupplyTable
								+ " where fuelRegionID in (" + regionIDs + ")"
								+ " and fuelYearID in (" + fuelYearIDs + ")"
								+ " and monthGroupID in (" + monthGroupIDs + ")";
						//System.out.println(sql);
						SQLRunner.Query query = new SQLRunner.Query();
						String formulationIDs = "0";
						try {
							query.open(db,sql);
							while(query.rs.next()) {
								formulationIDs += "," + query.rs.getInt(1);
							}
							query.close();

							sql = "select fuelFormulationID, fuelSubtypeID, RVP, sulfurLevel, ETOHVolume, MTBEVolume,"
									+ " ETBEVolume, TAMEVolume, aromaticContent, olefinContent, benzeneContent, e200, e300,"
									+ " BioDieselEsterVolume, CetaneIndex, PAHContent, T50, T90"
									+ " from fuelFormulation"
									+ " where fuelSubtypeID in (" + subtypes + ")"
									+ " and (fuelFormulationID < 100 or fuelFormulationID in (" + formulationIDs + "))"
									+ " order by fuelFormulationID";
							//System.out.println(sql);
							return sql;
						} catch(Exception e) {
							Logger.logError(e,"Unable to find formulations in default database: " + sql);
						} finally {
							query.onFinally();
						}
					}
	
				}
				// Export formulations in the requested database filtered only by the
				// subtypes needed for the runspec.
				return "select fuelFormulationID, fuelSubtypeID, RVP, sulfurLevel, ETOHVolume, MTBEVolume,"
						+ " ETBEVolume, TAMEVolume, aromaticContent, olefinContent, benzeneContent, e200, e300,"
						+ " BioDieselEsterVolume, CetaneIndex, PAHContent, T50, T90"
						+ " from fuelFormulation"
						+ " where fuelSubtypeID in (" + subtypes + ")"
						+ " order by fuelFormulationID";
			}
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

	class CustomBasicDataHandler extends BasicDataHandler {
		TreeSet<String> templateKeysAVFT = new TreeSet<String>();
		TreeSet<String> templateKeysFUF = new TreeSet<String>();

		/**
		 * Constructor
		 * @param importerToUse importer for this data handler
		 * @param descriptorToUse descriptor for the table(s) used by this importer
		 * @param importDataProviderToUse Provider for data required during imports
		**/
		public CustomBasicDataHandler(IImporter importerToUse, String[] descriptorToUse,
				IProvider importDataProviderToUse) {
			super(importerToUse,descriptorToUse,importDataProviderToUse);

            // override the default decode table for engine tech with one that only contains onroad engine techs
            this.addDecodeTable("EngineTech", "select engTechID, engTechName from engineTech where engTechID between 1 and 99 order by engTechID");
                    
            // include model year group decode table for fuelusagefraction template
            this.addDecodeTable("ModelYearGroup", 
                                "SELECT modelYearGroupID, " +
                                "CASE WHEN modelYearGroupID = 0 THEN 'Doesn\\\'t Matter (matches all model years)' ELSE modelYearGroupName END AS modelYearGroupName, " +
                                "modelYearGroupStartYear, modelYearGroupEndYear " + 
                                "FROM modelyeargroup ORDER BY modelYearGroupStartYear, modelYearGroupEndYear");
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
            // only adjust for the AVFT table 
			if(!tableName.equalsIgnoreCase("AVFT")) {
				return filterName;
			}

            // If the runspec has no source types selected, then use all source types
			if(filterName.equalsIgnoreCase(ImporterManager.FILTER_SOURCE)) {
				if(getImporterManager().getFilterValues(filterName) == null || 
                   getImporterManager().getFilterValues(filterName).size() <= 0) {
					return "ALL_" + filterName;
				}
			}

            // always select all fuel types for the AVFT table
            if(filterName.equalsIgnoreCase(ImporterManager.FILTER_FUEL)) {
                return "ALL_" + filterName;
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
			if(tableName.equalsIgnoreCase("AVFT")) {
				if(filterName.equalsIgnoreCase(ImporterManager.FILTER_FUEL)) {
					return false;
				}
			} else if(tableName.equalsIgnoreCase("FuelSupply") || tableName.equalsIgnoreCase("nrFuelSupply")) {
				if(filterName.equalsIgnoreCase(ImporterManager.FILTER_FUEL_FORMULATION)) {
					return false;
				}
			}
			return true;
		}

		/** Event called when a template is being initiated **/
		public void onBeginTemplate() {
			templateKeysAVFT.clear();
			templateKeysFUF.clear();

			Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "SELECT DISTINCT sourceTypeID, fuelTypeID, engTechID FROM samplevehiclepopulation";
			try {
				query.open(db,sql);
				while(query.rs.next()) {
					int s = query.rs.getInt(1);
					int f = query.rs.getInt(2);
					int e = query.rs.getInt(3);
					String key = "" + s + "|" + f + "|" + e;
					templateKeysAVFT.add(key);
				}
				query.close();
			} catch(SQLException e) {
				Logger.logSqlError(e,"Unable to get template keys",sql);
			} finally {
				query.onFinally();
			}
            
			sql = "SELECT DISTINCT sourceBinFuelTypeID, fuelSupplyFuelTypeID FROM fuelusagefraction";
			try {
				query.open(db,sql);
				while(query.rs.next()) {
					int sbft = query.rs.getInt(1);
					int fsft = query.rs.getInt(2);
					String key = "" + sbft + "|" + fsft;
					templateKeysFUF.add(key);
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
		public boolean shouldWriteTemplateRow(String tableName, Object[] values) {
			if(tableName.equalsIgnoreCase("AVFT")) {
                Object s = values[0];
                Object f = values[2];
                Object e = values[3];
                String key = s.toString() + "|" + f.toString() + "|" + e.toString();
                
                // templateKeysAVFT contains all valid source type / fuel type / engtech combinations
                return templateKeysAVFT.contains(key);
            } else if (tableName.equalsIgnoreCase("FuelUsageFraction")) {
                Object sbft = values[3];
                Object fsft = values[4];
                String key = sbft.toString() + "|" + fsft.toString();
                
                // templateKeysFUF contains all valid sourceBinFuelTypeID / fuelSupplyFuelTypeID combinations
                return templateKeysFUF.contains(key);
            }

            // Other tables do not need to be filtered, so default to writing the given row
            return true;
        }
	}

	/** Tables needed by the Onroad model **/
	static String[] onroadRequiredTables = new String[] { "FuelSupply", "FuelFormulation", "FuelUsageFraction", "AVFT" };
	
	/** Tables needed by the Nonroad model **/
	static String[] nonroadRequiredTables = new String[] { "nrFuelSupply", "FuelFormulation" };

	/** Constructor **/
	public FuelImporter() {
		super("Fuel", // common name
				"fuel", // XML node name
				ImporterInstantiator.activeManager.isNonroad()? nonroadRequiredTables : onroadRequiredTables
				);
		boolean isNonroad = false;
        boolean avftOnly = false;
		if(ImporterInstantiator.activeManager.isNonroad()) {
			isNonroad = true;
		}
        if(ImporterInstantiator.activeManager.isAVFTTool()) {
			avftOnly = true;
		}
		if(!isNonroad && !CompilationFlags.USE_FUELUSAGEFRACTION) {
			requiredTables = new String[] { "FuelSupply", "FuelFormulation", "AVFT" };
		}

		customButtonNames = new String[]{"AVFT Tool", "Fuels Wizard"};

		shouldDoExecutionDataExport = false;
		shouldDoDefaultDataExport = true;
		shouldDoCustomDefaultDataExport = false;
		subjectToExportRestrictions = false;

        // fuel supply
		if(isNonroad) {
			fuelSupplyPart = new TableFileLinkagePart(this,new NonroadFuelSupplyProvider());
		} else {
			fuelSupplyPart = new TableFileLinkagePart(this,new FuelSupplyProvider());
		}
        if(!avftOnly) {
            parts.add(fuelSupplyPart);
        }

        // fuel formulation
		fuelFormulationPart = new TableFileLinkagePart(this,new FuelFormulationProvider());
        if(!avftOnly) {
            parts.add(fuelFormulationPart);
        }

		if(!isNonroad) {
            // fuel usage fraction
			if(CompilationFlags.USE_FUELUSAGEFRACTION) {
				fuelUsageFractionPart = new TableFileLinkagePart(this,new FuelUsageFractionProvider());
                if(!avftOnly) {
				    parts.add(fuelUsageFractionPart);
                }
			}
			
            // avft
			avftPart = new TableFileLinkagePart(this, new AVFTProvider());
			parts.add(avftPart);
		}

		basicDataHandler = new CustomBasicDataHandler(this,
				isNonroad? nonroadDataTableDescriptor : (
				CompilationFlags.USE_FUELUSAGEFRACTION?dataTableDescriptor:dataTableDescriptorNoUsage),
				new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		return "FuelSupply";
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
		/*
		boolean hasCounties = manager.tableHasCounties(db,
				"select distinct countyID from fuelSupply");
		if(!hasCounties) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		*/
		boolean hasYears = manager.tableHasYears(db,
				"select distinct yearID from fuelSupply"
				+ " inner join year using (fuelYearID)",
				this,"fuelSupply is missing fuels from year(s)");
		if(!hasYears) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		
		boolean hasMonths = manager.tableHasMonths(db,
				"select distinct monthGroupID from fuelSupply ",
				this,"fuelSupply is missing fuels from months(s)");
		if(!hasMonths) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}

		String defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
		boolean hasFuels = manager.tableHasFuelTypes(db,
				"select distinct fuelTypeID"
				+ " from fuelSupply fs"
				+ " inner join fuelFormulation ff using (fuelFormulationID)"
				+ " inner join " + defaultDatabaseName + ".fuelSubType fst using (fuelSubTypeID)",
				this,"fuelSupply is missing formulations for fuelTypeID(s)");
		if(!hasFuels) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		
		// Check for the ImporterBase's check for unused fuelRegionIDs (which is originally declared a warning, but should be 
		// converted into an error)
		SQLRunner.Query query = new SQLRunner.Query();
		ArrayList<Integer> fuelRegionIDs = new ArrayList<Integer>();
		String sql = "SELECT DISTINCT fuelRegionID FROM fuelsupply";
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				fuelRegionIDs.add(Integer.valueOf(query.rs.getInt(1)));
			}
		} finally {
			query.close();
		}
		boolean hasWrongRegion = false;
		for (int fuelRegion : fuelRegionIDs) {
			String searchMessage = "WARNING: fuelRegionID " + fuelRegion + " is not used.";
			String replaceMessage = "ERROR: fuelRegionID " + fuelRegion + " is not used. Hint: change fuelFormulationIDs instead of fuelRegionIDs.";
			if (messages.contains(searchMessage)) {
				messages.remove(searchMessage);
				messages.add(replaceMessage);
				hasWrongRegion = true;
			}
			if (qualityMessages.contains(searchMessage)) {
				qualityMessages.remove(searchMessage);
				qualityMessages.add(replaceMessage);
				hasWrongRegion = true;
			}
		}
		if (hasWrongRegion) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		
		// Perform another check for unused fuelRegionIDs (because the ImporterBase's checks don't happen if you close and reopen the CDM/PDM)
		int countyID = (int)SQLRunner.executeScalar(db,"select countyID from county");
		int previousFuelRegionID = -1;
		boolean fuelRegionUsed = true; // initialize to true so the -1 fuel region (above) doesn't end up in the error list
		sql = "SELECT distinct fuelRegionID, countyID " + 
			  "FROM fuelsupply JOIN " + defaultDatabaseName + ".regionCounty on fuelRegionID = regionID " + 
			  "ORDER BY fuelRegionID, countyID";
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				int recordFuelRegionID = query.rs.getInt(1);
				int recordCountyID = query.rs.getInt(2);
				
				// new region
				if (previousFuelRegionID != recordFuelRegionID) {
					// issue error if we got to a new region without finding the current county associated with it
					if (!fuelRegionUsed) {
						addQualityMessage("ERROR: fuelRegionID " + previousFuelRegionID + " is not used. Hint: change fuelFormulationIDs instead of fuelRegionIDs.");
						return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
					}
					// reset for new region
					previousFuelRegionID = recordFuelRegionID;
					fuelRegionUsed = false;
				}
				
				// current county is associated with this fuel region
				if (recordCountyID == countyID) {
					fuelRegionUsed = true;
				}
			}
			
			// issue error if we got to the end of the query without finding the county associated with the last fuelRegionID
			if (!fuelRegionUsed) {
				addQualityMessage("ERROR: fuelRegionID " + previousFuelRegionID + " is not used. Hint: change fuelFormulationIDs instead of fuelRegionIDs.");
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			} 
		} finally {
			query.close();
		}

		// on to other checks
		if(!manager.isNonroad()) {
			if(CompilationFlags.USE_FUELUSAGEFRACTION) {
				// Check fuelUsageFraction.  Earlier versions of MOVES accepted the table if it does not exist or if it is empty.
				// (otherwise, it must have all required counties, fuel years, and fuel types just as fuelSupply must).
				// For MOVES3+, you must import the FuelUsageFraction table, and it must have all required counties, fuel years, and fuel types.
				boolean hasNonEmptyFuelUsageFractionTable = false;
				try {
					int count = (int)SQLRunner.executeScalar(db,"select count(*) from fuelUsageFraction");
					if(count > 0) {
						hasNonEmptyFuelUsageFractionTable = true;
					}
				} catch(Exception e) {
					// This happens if the table doesn't exist
					addQualityMessage("ERROR: FuelUsageFraction table does not exist. You may need to recreate your database to solve this problem.");
					return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);			
				}
				if(hasNonEmptyFuelUsageFractionTable) {
					boolean hasCounties = manager.tableHasCounties(db,
							"select distinct countyID from fuelUsageFraction",
							this,"fuelUsageFraction is missing countyID(s)");
					if(!hasCounties) {
						return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
					}
					hasYears = manager.tableHasYears(db,
							"select distinct yearID from fuelUsageFraction"
							+ " inner join year using (fuelYearID)",
							this,"fuelUsageFraction is missing entries for year(s)");
					if(!hasYears) {
						return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
					}
	
					hasFuels = manager.tableHasFuelTypes(db,
							"select distinct sourceBinFuelTypeID from fuelUsageFraction",
							this,"fuelUsageFraction sourceBinFuelTypeID is missing fuelTypeID(s)");
					if(!hasFuels) {
						return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
					}
					hasFuels = manager.tableHasFuelTypes(db,
							"select distinct fuelSupplyFuelTypeID from fuelUsageFraction",
							this,"fuelUsageFraction fuelSupplyFuelTypeID is missing fuelTypeID(s)");
					if(!hasFuels) {
						return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
					}
				} else {
					addQualityMessage("ERROR: FuelUsageFraction table is not imported.");
					return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);	
				}
			}
	

			// Check AVFT.  Earlier versions of MOVES accepted the table if it does not exist or if it is empty.
			// (otherwise, it must have all fuel types, regardless if they are in the runspec).
			// For MOVES3+, you must import the AVFT table, and it must have all fuel types.
			boolean hasNonEmptyAVFTTable = false;
			try {
				int count = (int)SQLRunner.executeScalar(db,"select count(*) from avft");
				if(count > 0) {
					hasNonEmptyAVFTTable = true;
				}
			} catch(Exception e) {
				// This happens if the table doesn't exist
				addQualityMessage("ERROR: AVFT table does not exist. You may need to recreate your database to solve this problem.");
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);	
			}
			if(hasNonEmptyAVFTTable) {
				// Verify the user imported at least the fuels in the runspec.
				hasFuels = manager.tableHasFuelTypes(db, "select distinct fuelTypeID from avft",
						this,"AVFT is missing fuelTypeID(s)");
				if(!hasFuels) {
					return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
				}
			} else {
				addQualityMessage("ERROR: AVFT table is not imported.");
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);	
			}
			
			
			// Check FuelFormulation
			boolean hasUnsupportedDieselParameters = false;
			try {
				int count = (int)SQLRunner.executeScalar(db,"select count(*) from fuelformulation where fuelSubTypeID in (20,21,22,23,24) and ( " +
                                                            "(RVP > 0) OR (ETOHVolume > 0) OR (MTBEVolume > 0) OR (ETBEVolume > 0) OR (TAMEVolume > 0) OR " +
															"(aromaticContent > 0) OR (olefinContent > 0) OR (benzeneContent > 0) OR (e200 > 0) OR " +
															"(e300 > 0) OR (volToWtPercentOxy > 0) OR (PAHContent > 0) OR (T50 > 0) OR (T90 > 0))");
				if(count > 0) {
					hasUnsupportedDieselParameters = true;
				}
			} catch(Exception e) {
				// This happens if the table doesn't exist
				addQualityMessage("ERROR: FuelFormulation table does not exist. You may need to recreate your database to solve this problem.");
				return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);	
			}
			if(hasUnsupportedDieselParameters) {
				addCustomMessage("Warning: Diesel rows in FuelFormulation currently only support entries for SulfurLevel and BioDieselEsterVolume. Other values will be ignored.");
			}
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

	/**
	 * Process a click on a custom button.
	 * @param name identifies the custom button.
	 * @param guiOwner window that is showing the custom button
	**/	
	public void handleCustomButton(String name, JPanel guiOwner) {
		JFrame frame = null;
		Component parent = guiOwner;
		while(parent != null) {
			if(parent instanceof JFrame) {
				frame = (JFrame)parent;
				break;
			}
			parent = parent.getParent();
		}
		if(frame == null) {
			return;
		}
        if (name == "Fuels Wizard") {
            Connection db = manager.openDatabase(false);
            if(db == null) {
                return;
            }
            FuelWizard w = new FuelWizard(frame, db, manager.isNonroad());
            w.setLocation(guiOwner.getLocationOnScreen().x + 200, guiOwner.getLocationOnScreen().y - 50);
            w.showModal();
            DatabaseUtilities.closeConnection(db);
            db = null;
        } else if (name == "AVFT Tool") {
            AVFTTool t = new AVFTTool(frame);
            t.setLocation(guiOwner.getLocationOnScreen().x, guiOwner.getLocationOnScreen().y);
            t.showModal();
        }
        
	}
}
