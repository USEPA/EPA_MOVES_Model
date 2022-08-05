/**************************************************************************************************
 * @(#)ImporterManager.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.CreateInputDatabase;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.implementation.importers.GenericImporter;
import java.util.*;
import java.sql.*;
import java.io.*;
import javax.swing.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;

/**
 * Manages the life cycle of importers, both via GUI and via XML execution.
 *
 * @author		Don Smith
 * @author		Wesley Faler
 * @author		Mike Kender	task 1903
 * @version		2019-10-18
**/
public class ImporterManager {
	public static ImporterManager singleton = null;

	/** Mode to show all available GUI tabs **/
	public static final int STANDARD_MODE = 0;
	/** Mode to show only the I/M and related tabs **/
	public static final int IM_ONLY_MODE = 1;
	
	/** Importers might need to behave differently depending on if the GUI is actually being shown or not **/
	public static final int RUN_VIA_GUI = 0;
	public static final int RUN_VIA_XML = 1;
	public static int RunMode = RUN_VIA_GUI; // assume importers are being run via the GUI unless otherwise specified

	/** True if the XML generation code should perform a self-check **/
	private static boolean shouldTestXML = false;

	public static final String FILTER_FUEL = "fuelTypeID";
	public static final String FILTER_FUEL_YEAR = "fuelYearID";
	public static final String FILTER_HOURDAY = "hourDayID";
	public static final String FILTER_HOUR = "hourID";
	public static final String FILTER_DAY = "dayID";
	public static final String FILTER_MONTH = "monthID";
	public static final String FILTER_MONTH_GROUP = "monthGroupID";
	public static final String FILTER_YEAR = "yearID";
	public static final String FILTER_AGEDYEAR = "agedYearID";
	public static final String FILTER_AGEDMODELYEAR = "agedModelYearID";
	public static final String FILTER_SOURCE = "sourceTypeID";
	public static final String FILTER_OFFNETWORK_LINK = "offNetworkLinkID";
	public static final String FILTER_ZONE = "zoneID";
	public static final String FILTER_COUNTY = "countyID";
	public static final String FILTER_STATE = "stateID";
	public static final String FILTER_FUEL_REGION = "regionID";
	public static final String FILTER_FUEL_FORMULATION = "fuelFormulationID";
	public static final String FILTER_FUEL_SUBTYPE = "fuelSubtypeID";
	public static final String FILTER_ROAD_TYPE = "roadTypeID";
	public static final String FILTER_ROAD_TYPE_HWY = "highwayRoadTypeID";
	public static final String FILTER_ROAD_TYPE_NOT_OFFNETWORK = "notOffNetworkRoadTypeID";
	public static final String FILTER_MARKET_SHARE = "marketShare";
	public static final String FILTER_HPMS_VTYPE = "hpmsVtypeID";
	public static final String FILTER_NON_NEGATIVE = "NonNegativeFloat";
	public static final String FILTER_NON_NEGATIVE_DEFAULT_1 = "NonNegativeFloatDefault1";
	public static final String FILTER_0_TO_1_FRACTION = "zeroToOneFraction";
	public static final String FILTER_0_TO_100_PERCENTAGE = "zeroTo100Percentage";
	public static final String FILTER_AGE = "ageID";
	public static final String FILTER_AVGSPEED_BIN = "avgSpeedBinID";
	public static final String FILTER_POLPROCESSID = "polProcessID";
	public static final String FILTER_POLPROCESSID_IM = "polProcessIDIM";
	public static final String FILTER_STARTS_POLPROCESSID = "startsPolProcessID";
	public static final String FILTER_POLLUTANT = "pollutantID";
	public static final String FILTER_PROCESS = "processID";
	public static final String FILTER_OPMODEID = "opModeID";
	public static final String FILTER_STARTS_OPMODEID = "startsOpModeIDs";
	public static final String FILTER_OPMODEID_AUX = "opModeIDAux";
	public static final String FILTER_IMPROGRAMID = "IMProgramID"; //int >=1
	public static final String FILTER_INSPECTFREQ = "inspectFreq";
	public static final String FILTER_TESTSTANDARDSID = "testStandardsID";
	public static final String FILTER_MODELYEARID = "modelYearID"; // int  between  >=1960 to <=2060
	public static final String FILTER_YN = "yn";
	public static final String FILTER_SOURCEFUELTYPE = "sourceFuelTypeID"; // sourceTypeID*100 + fuelTypeID
	public static final String FILTER_IS_LEAP_YEAR = "isLeapYear";
	public static final String FILTER_MODEL_YEAR_RANGE = "modelYearRange"; // 2 model years, low to high, both >= 1960 to <= 2060
	public static final String FILTER_COUNTYTYPE = "countyTypeID";
	public static final String FILTER_IDLEREGION = "idleRegionID";
	public static final String LOG_TABLES_SCRIPT = "database/CreateAuditLogTables.sql";

	/**
	 * Values for each filter type.  Keyed by the filter type name from the
	 * FILTER_XXXX constants.  The value is an ArrayList of Object.  The type of
	 * object within each ArrayList depends upon the type of the filtered column.
	**/
	private TreeMapIgnoreCase filterValueLists = new TreeMapIgnoreCase();
	/**
	 * CSV-values for each filter type.  Keyed by the filter type name from the
	 * FILTER_XXX constants.  The value is a comma-separated list of values as a
	 * String.
	**/
	private TreeMapIgnoreCase filterValuesCSV = new TreeMapIgnoreCase();
	/**
	 * Values for each filter type, like filterValueLists, but the data is a
	 * TreeSet of unique values instead of a flat array.
	**/
	private TreeMapIgnoreCase filterValueSets = new TreeMapIgnoreCase();
	/**
	 * Data type flag for each filter type.  Data is name of the class contained
	 * within the TreeSet and ArrayList of filterValueSets and filterValueLists.
	 * Examples are "Integer", "String", and "Double".
	**/
	private TreeMapIgnoreCase filterDataTypes = new TreeMapIgnoreCase();

	/** Fuel year IDs **/
	private TreeSet<Object> fuelYears = null;

	/**
	 * Create an ImporterManager, instantiate importers (all or only some), and display
	 * the import GUI.
	 * @param ownerWindow window to own the importer GUI
	 * @param importerNamesToShow common names of importers to be shown, may be empty or null
	 * to show all available importers.
	 * @param windowLeft importer GUI position
	 * @param windowTop importer GUI position
	 * @param domainType type of domain the importer is for, or null for general usage
	 * @param mode one of the ImporterManager.*_MODE constants
	 * @param mc model combination, used to detect Nonroad operation
	**/
	public static void display(JFrame ownerWindow,String[] importerNamesToShow,
			int windowLeft, int windowTop, ModelDomain domainType, int mode,
			Models.ModelCombination mc) {
		boolean isCountyDomain = domainType == ModelDomain.SINGLE_COUNTY;
		boolean isProjectDomain = domainType == ModelDomain.PROJECT;

		// Models.evaluateModels(runspec.models);
		boolean isNonroad = mc == Models.ModelCombination.M2;

		ImporterManager manager = new ImporterManager();
		manager.setMode(mode);
		if(isNonroad) {
			manager.setAsNonroad();
		} else if(isCountyDomain) {
			manager.setAsCountyDomain();
		} else if(isProjectDomain) {
			manager.setAsProjectDomain();
		}
		manager.instantiate(importerNamesToShow);
		ImporterGUI gui = manager.createGUI(ownerWindow);
		gui.setLocation(windowLeft,windowTop);
		gui.showModal();
	}

	/** IImporter objects being managed **/
	public ArrayList<IImporter> importers = new ArrayList<IImporter>();
	/** The RunSpec being edited **/
	public RunSpec runSpec;
	/** Current GUI, if any **/
	ImporterGUI gui;
	/** Set of tables required for the importers being managed **/
	TreeSetIgnoreCase requiredTableNames = new TreeSetIgnoreCase();
	/** Database for the imported data **/
	public DatabaseSelection database = new DatabaseSelection();
	/** Database connection in use during import, export, and template operations **/
	public Connection activeDb = null;
	/** True when being used for a single county and single year **/
	boolean isCountyDomain = false;
	/** True when being used for a project domain  **/
	boolean isProjectDomain = false;
	/** True when being used for Nonroad **/
	boolean isNonroad = false;
	/** one of the ImporterManager.*_MODE constants **/
	int mode = STANDARD_MODE;

	/** Constructor, uses the default API's RunSpec object. **/
	public ImporterManager() {
		singleton = this;

		runSpec = MOVESAPI.getTheAPI().getRunSpec();
		if(runSpec == null) {
			runSpec = new RunSpec();
		}
	}

	/**
	 * Constructor
	 * @param runSpecToUse runSpec to be used.  If null a new RunSpec will be created.
	**/
	public ImporterManager(RunSpec runSpecToUse) {
		singleton = this;
		runSpec = runSpecToUse;
		if(runSpec == null) {
			runSpec = new RunSpec();
		}
	}

	/**
	 * Return the project domain mode
	 * @return true if the importer is in project domain mode
	**/
	public boolean isProject() {
		return isProjectDomain;
	}

	/**
	 * Return the county domain mode
	 * @return true if the importer is in county domain mode
	**/
	public boolean isCounty() {
		return isCountyDomain;
	}

	/**
	 * Return the Nonroad mode
	 * @return true if the RunSpec is a Nonroad simulation
	**/
	public boolean isNonroad() {
		return isNonroad;
	}

	/**
	 * Return the custom domain mode for the RunSpec
	 * @return true if the RunSpec is using a custom geographic domain
	**/
	public boolean isCustomDomain() {
		return runSpec.isCustomDomain();
	}

	/**
	 * Change the mode that sets that available tabs.
	 * @param modeToUse one of the ImporterManager.*_MODE constants
	**/
	public void setMode(int modeToUse) {
		switch(modeToUse) {
			case STANDARD_MODE:
			case IM_ONLY_MODE:
				mode = modeToUse;
				break;
		}
	}

	/** Setup to require only one county and one calendar year **/
	public void setAsCountyDomain() {
		isCountyDomain = true;

		if(isCountyDomain && runSpec.domain == ModelDomain.SINGLE_COUNTY) {
			if(runSpec.scaleInputDatabase != null) {
				database = (DatabaseSelection)runSpec.scaleInputDatabase.clone();
			}
		}
	}

	/** Setup to require only one county, one calendar year, etc per project domain requirements **/
	public void setAsProjectDomain() {
		isProjectDomain = true;

		if(isProjectDomain && runSpec.domain == ModelDomain.PROJECT) {
			if(runSpec.scaleInputDatabase != null) {
				database = (DatabaseSelection)runSpec.scaleInputDatabase.clone();
			}
		}
	}

	/** Setup to require only one county, one calendar year, etc per project domain requirements **/
	public void setAsNonroad() {
		isNonroad = true;
	}

	/**
	 * Check for use of Rates rather than Inventory scale.
	 * @return true if being used for Rate calculations.
	**/
	public boolean isRate() {
		return runSpec.scale == ModelScale.MESOSCALE_LOOKUP;
	}

	/**
	 * Instantiate importers to be managed.
	 * @param importerNames common names of importers to be shown, may be empty or null
	 * to show all available importers.
	**/
	public void instantiate(String[] importerNames) {
		TreeSetIgnoreCase unwantedImporters = new TreeSetIgnoreCase();
		if(!runSpec.isCustomDomain()) {
			unwantedImporters.add("Zone Road Activity");
			unwantedImporters.add("zoneroadtype");
			unwantedImporters.add("gov.epa.otaq.moves.master.implementation.importers.ZoneRoadTypeImporter");

			unwantedImporters.add("Zone");
			unwantedImporters.add("zone");
			unwantedImporters.add("gov.epa.otaq.moves.master.implementation.importers.ZoneImporter");
		}

		if(mode == IM_ONLY_MODE && (importerNames == null || importerNames.length <= 0)) {
			importerNames = new String[] { "I/M" };
		}

		if(importerNames == null || importerNames.length <= 0) {
			Iterator<String> i=null;
			if(isNonroad) {
				i = ImporterInstantiator.nonRoadNamesIterator();
			} else if(isProjectDomain) {
				i = ImporterInstantiator.projectNamesIterator();
			} else if(isCountyDomain) {
				i = ImporterInstantiator.countyNamesIterator();
			} else {
				i = ImporterInstantiator.generalNamesIterator();
			}
			for(;i.hasNext();) {
				String t = (String)i.next();
				if(unwantedImporters.contains(t)) {
					continue;
				}
				IImporter importer = ImporterInstantiator.createByName(t,this);
				if(importer != null) {
					importers.add(importer);
				}
			}
		} else {
			for(int i=0;i<importerNames.length;i++) {
				if(unwantedImporters.contains(importerNames[i])) {
					continue;
				}
				IImporter importer = ImporterInstantiator.createByName(importerNames[i],this);
				if(importer != null) {
					importers.add(importer);
				}
			}
		}
		summarizeRequirements();
	}

	/**
	 * Summarize the required tables and other requirements of the importers being managed.
	**/
	private void summarizeRequirements() {
		for(int i=0;i<importers.size();i++) {
			IImporter importer = (IImporter)importers.get(i);
			importer.setImporterManager(this);
		}

		if(isCountyDomain) {
			for(int i=0;i<importers.size();i++) {
				IImporter importer = (IImporter)importers.get(i);
				if(importer instanceof ICountyDataImporter) {
					ICountyDataImporter cdi = (ICountyDataImporter)importer;
					try {
						if(cdi.getCountyDataStatus(null) == null) {
							importers.remove(i);
							i--;
						}
					} catch(Exception e) {
						// Nothing to do here
					}
				}
			}
		}

		for(int i=0;i<importers.size();i++) {
			IImporter importer = (IImporter)importers.get(i);
			String[] tableNames = importer.getRequiredTables();
			if(tableNames != null) {
				for(int j=0;j<tableNames.length;j++) {
					if(tableNames[j] != null) {
						requiredTableNames.add(tableNames[j]);
					}
				}
			}
		}
		loadFilterValues();
	}

	/**
	 * Open, or create, the selected database.
	 * @return the selected database or null.
	**/
	public Connection openDatabase() {
		return openDatabase(true);
	}

	/**
	 * Open, or create, the selected database.
	 * @param createIfNeeded true to create the database if it does not already exist
	 * @return the selected database or null.
	**/
	public Connection openDatabase(boolean createIfNeeded) {
		Connection db = null;
		try {
			db = database.openConnectionOrNull();
			if(db == null) {
				if(!createIfNeeded) {
					return null;
				}
				Logger.log(LogMessageCategory.INFO,"Creating importer database "
						+ database.toString());
				if(database.safeCreateDatabase(LOG_TABLES_SCRIPT) == DatabaseSelection.NOT_CREATED) {
					Logger.log(LogMessageCategory.ERROR,"Unable to create database "
							+ database.toString());
					return null;
				}
			} else {
				try {
					DatabaseUtilities.executeScript(db,new File(LOG_TABLES_SCRIPT));
				} catch (SQLException e) {
					// this will error if the database hasn't been created.
					return null;
				} finally {
					DatabaseUtilities.closeConnection(db);
					db = null;
				}
			}
			if(createDatabase(database) == DatabaseSelection.NOT_CREATED) {
				return null;
			}
			db = database.openConnectionOrNull();
			return db;
		} catch(Exception e) {
			Logger.logError(e,"Unable to open importer database");
			if(db != null) {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
			return null;
		}
	}

	/**
	 * Execute all importers.
	 * @param messages information, warnings, and errors to be shown to the user
	**/
	public void doImport(ArrayList<String> messages) {
		Connection db = openDatabase();
		if(db == null) {
			Logger.log(LogMessageCategory.ERROR,"Unable to create database "
					+ database.toString());
			return;
		}
		TreeSet<String> clearedClasses = new TreeSet<String>();
		try {
			activeDb = db;
			for(Iterator<IImporter> i=importers.iterator();i.hasNext();) {
				IImporter importer = (IImporter)i.next();
				IDataHandler dataHandler = importer.getDataHandler();
				if(dataHandler != null) {
					// Clear table data upon first use of the importer within the batch
					String className = importer.getClass().getName();
					if(importer instanceof GenericImporter) {
						// The generic importer can do almost any table, so blocking it once
						// is inappropriate.  Instead, learn the tables affected by each
						// instance and block the duplicate truncation of the combination
						// of GenericImporter and table.
						boolean found = false;
						ArrayList<String> toBeCleared = new ArrayList<String>();
						ArrayList<String> tableNames = dataHandler.getDataTableNames();
						for(Iterator<String> j=tableNames.iterator();j.hasNext();) {
							String t = className + "|" + j.next().toLowerCase();
							toBeCleared.add(t);
							if(clearedClasses.contains(t)) {
								found = true;
							}
						}
						if(!found) {
							for(Iterator<String> j=toBeCleared.iterator();j.hasNext();) {
								clearedClasses.add(j.next());
							}
							dataHandler.doClear(db);
						}
					} else {
						if(!clearedClasses.contains(className)) {
							clearedClasses.add(className);
							dataHandler.doClear(db);
						}
					}
					// Import data now
					dataHandler.doImport(db,true,messages);
				}
			}
		} finally {
			activeDb = null;
			if(db != null) {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
		}
	}

	/**
	 * Return the GUI for this importer manager, creating one if it hasn't already been created.
	 * @param ownerWindow window to own to the importer GUI
	 * @return the GUI window to be shown for this manager
	**/
	public ImporterGUI createGUI(JFrame ownerWindow) {
		if(gui == null) {
			gui = new ImporterGUI(ownerWindow,this);
		}
		return gui;
	}

	/**
	 * Create a database and all required tables.  If the database already exists, only
	 * tables will be created, but only the ones that don't already exist.
	 * @param dbSelection database, including server name, to be created.
	 * @return DatabaseSelection.CREATED, NOT_CREATED, or EXISTS.
	**/
	public int createDatabase(DatabaseSelection dbSelection) {
		Connection db = null;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
			int createStatus = dbSelection.safeCreateDatabase(LOG_TABLES_SCRIPT);
			if(createStatus == DatabaseSelection.NOT_CREATED) {
				Logger.log(LogMessageCategory.ERROR,
						"Could not create the Input database.");
				return DatabaseSelection.NOT_CREATED;
			}
			Connection newDB = dbSelection.openConnectionOrNull();
			if(newDB == null) {
				Logger.log(LogMessageCategory.ERROR,
						"Could not connect to the Input database.");
				return DatabaseSelection.NOT_CREATED;
			}
			Statement statement = null;
			ResultSet rs = null;
			String sql = "";

			try {
				for(Iterator<String> i=requiredTableNames.iterator();i.hasNext();) {
					String tableName = (String)i.next();
					String createStatement = "";
					sql = "SHOW CREATE TABLE " + tableName;
					statement = db.createStatement();
					rs = SQLRunner.executeQuery(statement,sql);
					if(rs.next()) {
						createStatement = StringUtilities.replace(rs.getString(2),
								"CREATE TABLE `","CREATE TABLE IF NOT EXISTS `") + ";";

						SQLRunner.executeSQL(newDB,createStatement);
					}
					rs.close();
					rs = null;
					statement.close();
					statement = null;
				}

				DatabaseUtilities.executeScript(db,new File(LOG_TABLES_SCRIPT));

				if(createStatus == DatabaseSelection.CREATED) {
					if(isCountyDomain || isProjectDomain) {
						fillCountyDomainTables(newDB);
					}
				}
				return createStatus;
			} catch(Exception e) {
				Logger.log(LogMessageCategory.INFO,e.toString());
				e.printStackTrace();
			} finally {
				if(rs!=null) {
					try {
						rs.close();
					} catch (SQLException e) {
						// Failure to close a ResultSet should not be an issue.
					}
				}
				if(statement!=null) {
					try {
						statement.close();
					} catch (SQLException e) {
						// Failure to close a PreparedStatment should not be an issue.
					}
				}
				if(newDB != null) {
					DatabaseUtilities.closeConnection(newDB);
				}
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to create database");
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}
		return DatabaseSelection.NOT_CREATED;
	}

	/**
	 * Create, if they don't already exist, and fill, if not already filled,
	 * the County-domain specific tables.
	 * @param newDB database to be updated
	 * @throws Exception if anything goes wrong
	**/
	public void fillCountyDomainTables(Connection newDB) throws Exception {
		if(!isCountyDomain && !isProjectDomain) {
			return;
		}

		String sql = "";

		try {
			String defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().
						databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
			// Year table
			if(runSpec.timeSpan.years.size() == 1) {
				Object year = runSpec.timeSpan.years.first();
				sql = (String)DatabaseConnectionManager.
						defaultDatabaseCreateTableStatements.get("year");
				SQLRunner.executeSQL(newDB,sql);
				sql = "insert ignore into Year"
						+ " select *"
						+ " from " + defaultDatabaseName + ".Year"
						+ " where yearID=" + year;
				SQLRunner.executeSQL(newDB,sql);
				sql = "update Year set isBaseYear='Y' where yearID=" + year;
				SQLRunner.executeSQL(newDB,sql);
			}
			GeographicSelection g = null;
			if(runSpec.geographicSelections.size() == 1) {
				g = (GeographicSelection)runSpec.geographicSelections.first();
				if(g.type != GeographicSelectionType.COUNTY) {
					g = null;
				}
			}
			if(g == null) {
				return;
			}

			// Create tables needed by all domains
			sql = (String)DatabaseConnectionManager.
					defaultDatabaseCreateTableStatements.get("state");
			SQLRunner.executeSQL(newDB,sql);
			sql = (String)DatabaseConnectionManager.
					defaultDatabaseCreateTableStatements.get("county");
			SQLRunner.executeSQL(newDB,sql);
			sql = (String)DatabaseConnectionManager.
					defaultDatabaseCreateTableStatements.get("zone");
			SQLRunner.executeSQL(newDB,sql);

			String[] statements = null;
			if(runSpec.isCustomDomain()) {
				GenericCounty c = runSpec.genericCounty;

				sql = (String)DatabaseConnectionManager.
						defaultDatabaseCreateTableStatements.get("countyYear");
				SQLRunner.executeSQL(newDB,sql);

				sql = (String)DatabaseConnectionManager.
						defaultDatabaseCreateTableStatements.get("regionCounty");
				SQLRunner.executeSQL(newDB,sql);

				for(Iterator<Integer> i=runSpec.timeSpan.years.iterator();i.hasNext();) {
					Integer year = i.next();
					sql = "insert ignore into CountyYear (countyID, yearID,"
							+ " refuelingVaporProgramAdjust, refuelingSpillProgramAdjust)"
							+ " values (" + c.getCountyID() + "," + year
							+ "," + c.refuelingVaporProgramAdjust
							+ "," + c.refuelingSpillProgramAdjust
							+ ")";
					SQLRunner.executeSQL(newDB,sql);

					sql = "insert ignore into regionCounty (regionID, countyID, regionCodeID, fuelYearID)"
							+ " values (100000000," + c.getCountyID() + ",1," + year
							+ ")";
					SQLRunner.executeSQL(newDB,sql);

					sql = "insert ignore into regionCounty (regionID, countyID, regionCodeID, fuelYearID)"
							+ " values (100000000," + c.getCountyID() + ",2," + year
							+ ")";
					SQLRunner.executeSQL(newDB,sql);
				}

				String[] genericCountyStatements = {
					// State table
					"insert ignore into State (stateID, stateName, stateAbbr, idleRegionID)"
							+ " values (" + c.stateID + ",'Generic','99',1)",

					// County table
					"insert ignore into County (countyID, stateID, countyName, altitude,"
							+ " gpaFract, barometricPressure, barometricPressureCV,countyTypeID)"
							+ " values (" + c.getCountyID() + "," + c.stateID
							+ "," + DatabaseUtilities.escapeSQL(c.description,true)
							+ ",'" + (c.isHighAltitude()?"H":"L") + "'"
							+ "," + c.gpaFraction
							+ "," + c.barometricPressure
							+ ",null,1)",

					"update County set stateID=" + c.stateID
							+ ", countyName=" + DatabaseUtilities.escapeSQL(c.description,true)
							+ ", altitude='" + (c.isHighAltitude()?"H":"L") + "'"
							+ ", gpaFract=" + c.gpaFraction
							+ ", barometricPressure=" + c.barometricPressure
							+ ", barometricPressureCV=null"
							+ ", countyTypeID=1"
							+ " where countyID=" + c.getCountyID(),

					/*
					// Zone table
					"insert ignore into Zone (zoneID, countyID, startAllocFactor,"
							+ " idleAllocFactor, SHPAllocFactor)"
							+ " values (" + (c.getCountyID()*10) + "," + c.getCountyID()
							+ ",1.0,1.0,1.0)",
					*/

					// Link table - but not if Project
					runSpec.domain == ModelDomain.PROJECT? null :
							(String)DatabaseConnectionManager.defaultDatabaseCreateTableStatements.get("link")

					/*
					runSpec.domain == ModelDomain.PROJECT? null :
							"insert ignore into Link (linkID, countyID, zoneID, roadTypeID)"
							+ " select (" + (c.getCountyID()*100) + "+roadTypeID) as linkID,"
							+ c.getCountyID() + " as countyID,"
							+ (c.getCountyID()*10) + " as zoneID, roadTypeID"
							+ " from " + defaultDatabaseName + ".roadType"
					*/
				};
				statements = genericCountyStatements;
			} else {
				String[] standardStatements = {
					// State table
					"insert ignore into State"
							+ " select State.*"
							+ " from " + defaultDatabaseName + ".County"
							+ " inner join " + defaultDatabaseName + ".State"
							+ " using (stateID)"
							+ " where countyID=" + g.databaseKey,

					// County table
					"insert ignore into County"
							+ " select *"
							+ " from " + defaultDatabaseName + ".County"
							+ " where countyID=" + g.databaseKey,

					// Zone table
					"insert ignore into Zone"
							+ " select Zone.*"
							+ " from " + defaultDatabaseName + ".County c"
							+ " inner join " + defaultDatabaseName + ".Zone"
							+ " using (countyID)"
							+ " where c.countyID=" + g.databaseKey,
					"drop table if exists zoneTemp",
					"create table zoneTemp"
							+ " select sum(startAllocFactor) as sumStartAllocFactor,"
							+ " sum(idleAllocFactor) as sumIdleAllocFactor,"
							+ " sum(SHPAllocFactor) as sumSHPAllocFactor"
							+ " from Zone"
							+ " where countyID=" + g.databaseKey,
					"update Zone, zoneTemp set "
							+ " startAllocFactor=(case when (sumStartAllocFactor is null or sumStartAllocFactor <= 0) then 1 else (startAllocFactor/sumStartAllocFactor) end),"
							+ " idleAllocFactor=(case when (sumIdleAllocFactor is null or sumIdleAllocFactor <= 0) then 1 else (idleAllocFactor/sumIdleAllocFactor) end),"
							+ " SHPAllocFactor=(case when (sumSHPAllocFactor is null or sumSHPAllocFactor <= 0) then 1 else (SHPAllocFactor/sumSHPAllocFactor) end)"
							+ " where countyID=" + g.databaseKey,
					"drop table if exists zoneTemp",

					// ZoneRoadType table
					(String)DatabaseConnectionManager.
							defaultDatabaseCreateTableStatements.get("zoneroadtype"),
					"insert ignore into ZoneRoadType"
							+ " select ZoneRoadType.*"
							+ " from " + defaultDatabaseName + ".County c"
							+ " inner join " + defaultDatabaseName + ".Zone"
							+ " using (countyID)"
							+ " inner join " + defaultDatabaseName + ".ZoneRoadType"
							+ " using (zoneID)"
							+ " where c.countyID=" + g.databaseKey,
					"drop table if exists zoneTemp",
					"create table zoneTemp"
							+ " select roadTypeID, sum(SHOAllocFactor) as sumSHOAllocFactor"
							+ " from ZoneRoadType zrt"
							+ " inner join Zone z using (zoneID)"
							+ " where countyID=" + g.databaseKey
							+ " group by roadTypeID",
					"update ZoneRoadType, zoneTemp, Zone set "
							+ " SHOAllocFactor=(case when (sumSHOAllocFactor is null or sumSHOAllocFactor <= 0) then 1 else (SHOAllocFactor/sumSHOAllocFactor) end)"
							+ " where ZoneRoadType.zoneID=Zone.zoneID"
							+ " and ZoneRoadType.roadTypeID=zoneTemp.roadTypeID"
							+ " and Zone.countyID=" + g.databaseKey,
					"drop table if exists zoneTemp"
				};
				statements = standardStatements;
			}
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				if(sql != null && sql.length() > 0) {
					SQLRunner.executeSQL(newDB,sql);
				}
			}
		} catch(Exception e) {
			Logger.log(LogMessageCategory.INFO,e.toString());
			e.printStackTrace();
		}
	}

	/**
	 * Generate an XML file describing the current RunSpec's pertinent sections,
	 * the database selection, and the importers.
	 * @param fileNameAndPath file name and path to be created (or overwritten)
	 * @throws IOException if anything goes wrong
	**/
	public void generateXML(String fileNameAndPath) throws IOException {
		File file = new File(fileNameAndPath);
		PrintWriter printWriter = null;
		try {
			printWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(file))));
			printWriter.println("<moves>");
			if(isCountyDomain) {
				printWriter.println("\t<importer mode=\"county\" >");
			} else if(isProjectDomain) {
				printWriter.println("\t<importer mode=\"project\" >");
			} else {
				printWriter.println("\t<importer>");
			}
			printWriter.println("\t\t<filters>");
			RunSpecXML runSpecWriter = new RunSpecXML(runSpec);
			runSpecWriter.save(printWriter,true);
			printWriter.println("\t\t</filters>");
			printWriter.println("\t\t<databaseselection"
					+ " servername=\""
					+ StringUtilities.safeGetString(database.serverName) + "\""
					+ " databasename=\""
					+ StringUtilities.safeGetString(database.databaseName) + "\""
					+ "/>");
			for(Iterator<IImporter> i=importers.iterator();i.hasNext();) {
				IImporter importer = (IImporter)i.next();
				String textToWrite = importer.toXML();
				if(textToWrite != null && textToWrite.length() > 0) {
					printWriter.println(textToWrite);
				}
			}
			printWriter.println("\t</importer>");
			printWriter.println("</moves>");
		} finally {
			if(printWriter != null) {
				try {
					printWriter.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				printWriter = null;
			}
		}

		// Test the XML load
		if(shouldTestXML) {
			shouldTestXML = false; // prevent recursion of the testing code
			try {
				ArrayList<ImporterManager> managers = fromXML(fileNameAndPath);
				if(managers == null) {
					Logger.log(LogMessageCategory.ERROR,"Unable to load from saved XML");
					return;
				}
				int index = 0;
				for(Iterator<ImporterManager> i=managers.iterator();i.hasNext();index++) {
					ImporterManager manager = (ImporterManager)i.next();
					manager.generateXML(fileNameAndPath + "_" + index);
				}
			} finally {
				shouldTestXML = true;
			}
		}
	}

	/**
	 * Load from an XML file.
	 * @param fileNameAndPath file to be read
	 * @return ArrayList of ImporterManager objects as found within the XML document.
	 * null if any of the ImporterManager objects failed to load.
	**/
	public static ArrayList<ImporterManager> fromXML(String fileNameAndPath) {
		ArrayList<ImporterManager> managers = new ArrayList<ImporterManager>();
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			db.setErrorHandler(new RunSpecXML.XMLErrorHandler());
			Document doc = db.parse(new File(fileNameAndPath));
			// Verify that this is an importer xml file
			Node rootNode = doc.getFirstChild();
			if(!(rootNode != null && rootNode.getNodeName().equalsIgnoreCase("moves"))) {
				Logger.log(LogMessageCategory.ERROR,
						"Invalid Importer XML file - missing <moves> tag");
				return null;
			}
			for(Node subNode = rootNode.getFirstChild(); subNode != null;
					subNode = subNode.getNextSibling()) {
				if(subNode.getNodeName().equalsIgnoreCase("importer")) {
					ImporterManager manager = fromXML(subNode);
					if(manager != null) {
						managers.add(manager);
					}
				}
			}
			if(managers.size() <= 0) {
				Logger.log(LogMessageCategory.ERROR,
						"Invalid Importer XML file - no loaded importers");
				return null;
			}
		} catch(SAXException e) {
			// If we get this exception, then we don't have a chance to read any fields
			Logger.logError(e,"Could not load Importer XML.");
		} catch(Exception e) {
			// This indicates an error during one field's parsing
			Logger.logError(e,"Could not load Importer XML");
			return null;
		}
		return managers;
	}

	/**
	 * Load a single importer manager from XML.
	 * @param importerNode XML node describing a single importer node
	 * @return an ImporterManager object or null if one could not be loaded.
	 * @throws SAXException upon an XML issue
	 * @throws Exception upon any other problem
	**/
	private static ImporterManager fromXML(Node importerNode) throws SAXException, Exception {
		// turn on XML mode so the importer manager knows it is being run from XML
		RunMode = RUN_VIA_XML;
		ImporterManager manager = new ImporterManager(null);
		NamedNodeMap attributes = importerNode.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("mode")) {
				if(attributeNode.getNodeValue().equalsIgnoreCase("county")) {
					manager.runSpec.domain = ModelDomain.SINGLE_COUNTY;
					manager.setAsCountyDomain();
				} else if(attributeNode.getNodeValue().equalsIgnoreCase("project")) {
					manager.runSpec.domain = ModelDomain.PROJECT;
					manager.setAsProjectDomain();
				}
			}
		}
		// Load the nodes within the file
		for(Node subNode = importerNode.getFirstChild(); subNode != null;
				subNode = subNode.getNextSibling()) {
			if(subNode.getNodeName().equalsIgnoreCase("filters")) {
				RunSpecXML rsxml = new RunSpecXML(manager.runSpec);
				for(Node rsChild=subNode.getFirstChild(); rsChild != null;
						rsChild = rsChild.getNextSibling()) {
					rsxml.processRunSpecSubNode(rsChild);
				}
			} else if(subNode.getNodeName().equalsIgnoreCase("databaseselection")) {
				attributes = subNode.getAttributes();
				manager.database.serverName = "";
				manager.database.databaseName = "";
				for(int i = 0; i < attributes.getLength(); i++) {
					Node attributeNode = attributes.item(i);
					if(attributeNode.getNodeName().equalsIgnoreCase("servername")) {
						manager.database.serverName = attributeNode.getNodeValue();
					} else if(attributeNode.getNodeName().equalsIgnoreCase("databasename")) {
						manager.database.databaseName = attributeNode.getNodeValue();
					}
				}
				if(manager.database.serverName.length() <= 0
						&& manager.database.databaseName.length() <= 0) {
					Logger.log(LogMessageCategory.ERROR,
							"Invalid Importer XML file - blank/missing server or database");
					return null;
				}
			} else {
				// Try to instantiate an importer based on the node
				IImporter importer =
						ImporterInstantiator.createByXMLNodeType(subNode.getNodeName(),manager);
				if(importer != null) {
					manager.importers.add(importer);
					if(!importer.fromXML(subNode)) {
						return null;
					}
				}
			}
		}
		manager.summarizeRequirements();
		return manager;
	}

	/**
	 * Add a log entry for an importer.
	 * @param importer importer that took action within the current database
	 * @param briefDescription short description of the action taken by the importer,
	 * such as "Imported FuelSupply Table".
	 * @param fullDescription long description of the action taken by the importer,
	 * such as "July 2006 North East States updated rates from Wes Faler".
	**/
	public void log(IImporter importer, String briefDescription, String fullDescription) {
		Connection db = activeDb;
		if(db == null) {
			db = openDatabase();
		}
		if(db == null) {
			Logger.log(LogMessageCategory.ERROR,"Unable to create database "
					+ database.toString());
			return;
		}
		String sql = "";
		try {
			// Trim descriptions to a size that fits in the database
			briefDescription = StringUtilities.safeGetString(briefDescription);
			if(briefDescription.length() > 100) {
				briefDescription = briefDescription.substring(0,100).trim();
			}
			fullDescription = StringUtilities.safeGetString(fullDescription);
			if(fullDescription.length() > 4096) {
				fullDescription = fullDescription.substring(0,4096).trim();
			}
			// Store the descriptions
			sql = "insert into auditLog (whenHappened,importerName,"
					+ "briefDescription,fullDescription) "
					+ "values (now()"
					+ "," + DatabaseUtilities.escapeSQL(importer.getName(),true)
					+ "," + DatabaseUtilities.escapeSQL(briefDescription,true)
					+ "," + DatabaseUtilities.escapeSQL(fullDescription,true)
					+ ")";
			SQLRunner.executeSQL(db,sql);
		} catch(Exception e) {
			Logger.logError(e,"Unable to write to audit log");
		} finally {
			if(db != null && db != activeDb) {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
		}
	}

	/**
	 * Remove all log entries for an importer.  This should be done after all data has
	 * been cleared from tables managed by an importer, or before an importer makes a
	 * total data change over (rather than an incremental addition of new data).
	 * @param importer importer to clear information for
	**/
	public void deleteLog(IImporter importer) {
		Connection db = activeDb;
		if(db == null) {
			db = openDatabase();
		}
		if(db == null) {
			Logger.log(LogMessageCategory.ERROR,"Unable to create database "
					+ database.toString());
			return;
		}
		String sql = "";
		try {
			sql = "delete from auditLog where importerName="
					+ DatabaseUtilities.escapeSQL(importer.getName(),true);
			SQLRunner.executeSQL(db,sql);
		} catch(Exception e) {
  			if( e.toString().toLowerCase().contains("auditlog")==false ){
				Logger.logError(e,"Unable to clear audit log entries for " + importer.getName());
			}else{
				return;
			}
		} finally {
			if(db != null && db != activeDb) {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
		}
	}

	/**
	 * Remove log entries for an importer specific to a table.  This should be done after all data has
	 * been cleared from a table managed by an importer.
	 * @param importer importer to clear information for
	 * @param tableName affected table
	**/
	public void deleteLog(IImporter importer, String tableName) {
		Connection db = activeDb;
		if(db == null) {
			db = openDatabase();
		}
		if(db == null) {
			Logger.log(LogMessageCategory.ERROR,"Unable to create database "
					+ database.toString());
			return;
		}
		String sql = "";
		try {
			sql = "delete from auditLog where importerName="
					+ DatabaseUtilities.escapeSQL(importer.getName(),true)
					+ " and briefDescription like '% " + DatabaseUtilities.escapeSQL(tableName,false) + " %'";
			SQLRunner.executeSQL(db,sql);
		} catch(Exception e) {
			Logger.logError(e,"Unable to clear audit log entries for " + importer.getName() + ", table " + tableName);
		} finally {
			if(db != null && db != activeDb) {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
		}
	}

	/**
	 * Get the set of values for a named filter.
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return set of unique values for the filter, or null if the filter is undefined.
	**/
	public TreeSet getFilterValuesSet(String filterType) {
		return (TreeSet)filterValueSets.get(filterType);
	}

	/**
	 * Check a value against the filters.  The value's type must match the data type of
	 * the filter for it to be accepted.  The value is accepted if there are no
	 * restrictions or if the value is explicitly listed.
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @param value String or Integer object holding the value to check against
	 * the filters.
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	**/
	public boolean doesInclude(String filterType, Object value) {
		return doesInclude(filterType,value,null);
	}

	public static class Message {
		public String text;

		public boolean hasMessage() {
			return text != null && text.length() > 0;
		}
	}

	/**
	 * Check a value against the filters.  The value's type must match the data type of
	 * the filter for it to be accepted.  The value is accepted if there are no
	 * restrictions or if the value is explicitly listed.
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @param value String or Integer object holding the value to check against
	 * the filters.
	 * @param message optional receiver of a customized error message, may be null
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	**/
	public boolean doesInclude(String filterType, Object value, Message message) {
		if(message != null) {
			message.text = "";
		}
		//System.out.println("doesInclude(" + filterType + "," + value.toString() + ")");
		String dataType = (String)filterDataTypes.get(filterType);
		if(dataType == null) {
			//System.out.println("dataType == null for " + filterType);
			return true;
		}
		//System.out.println("dataType=\"" + dataType + "\"");
		if(dataType.equalsIgnoreCase("Integer")) {
			if(value instanceof Integer) {
				TreeSet set = (TreeSet)filterValueSets.get(filterType);
				// Accept the value if there are no restrictions or if the value is listed
				return set == null || set.size() <= 0 || set.contains((Integer)value);
			} else {
				return false;
			}
		} else if(dataType.equalsIgnoreCase("String")) {
			if(value instanceof String) {
				TreeSet set = (TreeSet)filterValueSets.get(filterType);
				// Accept the value if there are no restrictions or if the value is listed
				return set == null || set.size() <= 0 || set.contains((String)value);
			} else {
				return false;
			}
		} else if(dataType.equalsIgnoreCase("GreaterThanZeroFloat")) {
			return true; // accept all floating point values coming into system
		} else if(dataType.equalsIgnoreCase("GreaterThanZeroInt")){
			double f=0;
			if(value instanceof Integer) {
				f = ((Integer)value).intValue();
			} else if(value instanceof Double) {
				f = ((Double)value).doubleValue();
			} else if(value instanceof String) {
				try {
					f = Double.parseDouble((String)value);
				} catch(Exception e) {
					return false;
				}
			}
			int i = (int)f;
			return (i>0) && Math.abs(((double)i-f))<0.1;
		} else if(dataType.equalsIgnoreCase("NonNegativeFloat") || dataType.equalsIgnoreCase("NonNegativeFloatDefault1")) {
			return true; // accept all floating point values coming into system
		} else if(dataType.equalsIgnoreCase("zeroToOneFraction")) {
			double f=0;
			if(value instanceof Integer) {
				f = ((Integer)value).intValue();
			} else if(value instanceof Double) {
				f = ((Double)value).doubleValue();
			} else if(value instanceof String) {
				try {
					f = Double.parseDouble((String)value);
				} catch(Exception e) {
					if(message != null) {
						message.text = ((String)value) + " is not a number from 0 to 1, inclusive";
					}
					return false;
				}
			}
			boolean success = f >= 0 && f <= 1;
			if(!success && message != null) {
				message.text = "" + f + " is not a number from 0 to 1, inclusive";
			}
			return success;
		} else if(dataType.equalsIgnoreCase("zeroTo100Percentage")) {
			double f=0;
			if(value instanceof Integer) {
				f = ((Integer)value).intValue();
			} else if(value instanceof Double) {
				f = ((Double)value).doubleValue();
			} else if(value instanceof String) {
				try {
					f = Double.parseDouble((String)value);
				} catch(Exception e) {
					if(message != null) {
						message.text = ((String)value) + " is not a number from 0 to 100, inclusive";
					}
					return false;
				}
			}
			boolean success = f >= 0 && f <= 100;
			if(!success && message != null) {
				message.text = "" + f + " is not a number from 0 to 100, inclusive";
			}
			return success;
		} else if( dataType.equalsIgnoreCase("YesNo")){
			String c = value.toString();
			return c.equalsIgnoreCase("Y") || c.equalsIgnoreCase("N");
		}else if( dataType.equalsIgnoreCase("ModelYear")){
			int modelYear = 0;
			if(value instanceof Integer) {
				modelYear = ((Integer)value).intValue();
			} else if(value instanceof Double) {
				modelYear = (int)((Double)value).doubleValue();
			} else if(value instanceof String) {
				try {
					modelYear = (int)Double.parseDouble((String)value);
				} catch(Exception e) {
					return false;
				}
			}
			return modelYear>=1960 && modelYear<=2060;
		} else if(dataType.equalsIgnoreCase("ModelYearRange")) {
			int modelYearRange = 0; // format: <low><high>, such as "19802010" for 1980 through 2010, inclusive.  0 is also accepted as a wildcard.
			if(value instanceof Integer) {
				modelYearRange = ((Integer)value).intValue();
			} else if(value instanceof Double) {
				modelYearRange = (int)((Double)value).doubleValue();
			} else if(value instanceof String) {
				try {
					modelYearRange = (int)Double.parseDouble((String)value);
				} catch(Exception e) {
					return false;
				}
			}
			int lowModelYear = modelYearRange / 10000;
			int highModelYear = modelYearRange % 10000;
			return modelYearRange == 0 ||
				(lowModelYear>=1960 && lowModelYear<=2060
				&& highModelYear>=1960 && highModelYear<=2060
				&& lowModelYear <= highModelYear);
		}
		return true;
	}

	/**
	 * Test a column in a ResultSet against the filters.  The value is accepted if
	 * there are no restrictions or if the value is explicitly listed.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	public boolean doesInclude(ResultSet rs, int columnIndex1, String filterType)
			throws SQLException {
		String dataType = (String)filterDataTypes.get(filterType);
		if(dataType == null) {
			return true;
		}
		if(dataType.equalsIgnoreCase("Integer")) {
			return doesIncludeInteger(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("String")) {
			return doesIncludeString(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("GreaterThanZeroFloat")) {
			return doesIncludeGreaterThanZeroFloat(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("GreaterThanZeroInt")) {
			return doesIncludeGreaterThanZeroInt(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("NonNegativeFloat") || dataType.equalsIgnoreCase("NonNegativeFloatDefault1")) {
			return doesIncludeNonNegativeFloat(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("zeroToOneFraction")) {
			return doesIncludeZeroToOneFraction(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("zeroTo100Percentage")) {
			return doesIncludeZeroTo100Percentage(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("YesNo")) {
			return doesIncludeYesNo(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("ModelYear")) {
			return doesIncludeModelYear(rs,columnIndex1,filterType);
		} else if(dataType.equalsIgnoreCase("ModelYearRange")){
			return doesIncludeModelYearRange(rs,columnIndex1,filterType);
		}
		return true;
	}

	/**
	 * Test an Integer column in a ResultSet against the filters.  The value is accepted if
	 * there are no restrictions or if the value is explicitly listed.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeInteger(ResultSet rs, int columnIndex1, String filterType)
			throws SQLException {
		Integer value = Integer.valueOf(rs.getInt(columnIndex1));
		TreeSet set = (TreeSet)filterValueSets.get(filterType);
		// Accept the value if there are no restrictions or if the value is listed
		return set == null || set.size() <= 0 || set.contains(value);
	}

	/**
	 * Test a String column in a ResultSet against the filters.  The value is accepted if
	 * there are no restrictions or if the value is explicitly listed.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeString(ResultSet rs, int columnIndex1, String filterType)
			throws SQLException {
		String value = rs.getString(columnIndex1);
		if(value == null) {
			value = "";
		}
		TreeSet set = (TreeSet)filterValueSets.get(filterType);
		// Accept the value if there are no restrictions or if the value is listed
		return set == null || set.size() <= 0 || set.contains(value);
	}

	/**
	 * Test a Float column in a ResultSet against zero.  The value is accepted if
	 * it is greater than zero.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeGreaterThanZeroFloat(ResultSet rs, int columnIndex1,
			String filterType) throws SQLException {
		float f = rs.getFloat(columnIndex1);
		return f > 0.0;
	}

	/**
	 * Test an Int column in a ResultSet against greater than zero.  The value is accepted if
	 * it is greater than zero.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeGreaterThanZeroInt(ResultSet rs, int columnIndex1,
			String filterType) throws SQLException {
		float f = rs.getFloat(columnIndex1);
		int i = (int)f;
		return (i>0) && Math.abs(((float)i-f))<0.1;
	}

	/**
	 * Test a Yes/No column in a ResultSet.  The value is accepted if
	 * it is Y or N.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeYesNo(ResultSet rs, int columnIndex1,
			String filterType) throws SQLException {
		String c = rs.getString(columnIndex1);
		return c.equalsIgnoreCase("Y") || c.equalsIgnoreCase("N");
	}


	/**
	 * Test Model Year column in a ResultSet.  The value is accepted if
	 * it is between >=1960 to <=2060
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeModelYear(ResultSet rs, int columnIndex1,
			String filterType) throws SQLException {
		int value = rs.getInt(columnIndex1);
		return value>=1960 && value<=2060;
	}

	/**
	 * Test Model Year range column in a ResultSet.  The value is accepted if
	 * it is 0 or if both model years are between >=1960 to <=2060 and the low model year is less
	 * than or equal to the high model year.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeModelYearRange(ResultSet rs, int columnIndex1,
			String filterType) throws SQLException {
		int modelYearRange = rs.getInt(columnIndex1);
		int lowModelYear = modelYearRange / 10000;
		int highModelYear = modelYearRange % 10000;
		return modelYearRange == 0 ||
			(lowModelYear>=1960 && lowModelYear<=2060
			&& highModelYear>=1960 && highModelYear<=2060
			&& lowModelYear <= highModelYear);
	}

	/**
	 * Test a Float column in a ResultSet against zero.  The value is accepted if
	 * it is greater than or equal to zero.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeNonNegativeFloat(ResultSet rs, int columnIndex1,
			String filterType) throws SQLException {
		float f = rs.getFloat(columnIndex1);
		return f >= 0.0;
	}

	/**
	 * Test a Float column in a ResultSet against zero.  The value is accepted if
	 * it is greater than or equal to zero and less than or equal to one.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeZeroToOneFraction(ResultSet rs, int columnIndex1,
			String filterType) throws SQLException {
		float f = rs.getFloat(columnIndex1);
		return f >= 0.0 && f <= 1.0;
	}

	/**
	 * Test a Float column in a ResultSet against zero.  The value is accepted if
	 * it is greater than or equal to zero and less than or equal to one hundred.
	 * @param rs data record to be examined
	 * @param columnIndex1 1-based column index within the row
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return true if the record matches the filters, false if it is not included by
	 * the filters
	 * @throws SQLException if anything goes wrong
	**/
	private boolean doesIncludeZeroTo100Percentage(ResultSet rs, int columnIndex1,
			String filterType) throws SQLException {
		float f = rs.getFloat(columnIndex1);
		return f >= 0.0 && f <= 100.0;
	}

	/**
	 * Get the set of values for a given filter.
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return ArrayList of Objects (Integer, Double, String, other) that give
	 * the unique values permitted within the current RunSpec for the named filter.
	 * May be null or empty.
	**/
	public ArrayList getFilterValues(String filterType) {
		if(filterValueLists.containsKey(filterType)) {
			return (ArrayList)filterValueLists.get(filterType);
		}
		return null;
	}

	/**
	 * Get the set of values for a given filter.
	 * @param filterType one of the ImporterManager.FILTER_XXXX constants
	 * @return ArrayList of Objects (Integer, Double, String, other) that give
	 * the unique values permitted within the current RunSpec for the named filter.
	 * May be null or empty.
	**/
	public String getFilterValuesCSV(String filterType) {
		String result = null;
		if(filterValuesCSV.containsKey(filterType)) {
			result = (String)filterValuesCSV.get(filterType);
		}
		if(result == null || result.length() <= 0) {
			return "0";
		}
		return result;
	}

	/**
	 * Load the set of filter values from the current RunSpec.
	**/
	private void loadFilterValues() {
		try {
			loadTimeFilterValues(); // must be done before loadGeographicFilterValues()
			loadGeographicFilterValues(); // do after loadTimeFilterValues()
			loadRoadTypeValues();
			if(isNonroad) {
				loadNonroadSourceAndFuels();
			} else {
				loadOnRoadSourceAndFuels();
			}
			loadPolProcessIDs();
			loadOpModeIDs();
			loadTestStandardsIDs();
			loadIsLeapYear();

			filterDataTypes.put(FILTER_YN,"YesNo");
			filterDataTypes.put(FILTER_MODELYEARID,"ModelYear");
			filterDataTypes.put(FILTER_MODEL_YEAR_RANGE,"ModelYearRange");
			filterDataTypes.put(FILTER_MARKET_SHARE,"GreaterThanZeroFloat");
			filterDataTypes.put(FILTER_NON_NEGATIVE,"NonNegativeFloat");
			filterDataTypes.put(FILTER_0_TO_1_FRACTION,"zeroToOneFraction");
			filterDataTypes.put(FILTER_0_TO_100_PERCENTAGE,"zeroTo100Percentage");
			filterDataTypes.put(FILTER_IMPROGRAMID,"GreaterThanZeroInt");

			// FILTER_NON_NEGATIVE_DEFAULT_1
			TreeSet<Object> one = new TreeSet<Object>();
			filterValueSets.put(FILTER_NON_NEGATIVE_DEFAULT_1,one);
			one.add(Double.valueOf(1));
			filterDataTypes.put(FILTER_NON_NEGATIVE_DEFAULT_1,"NonNegativeFloat");
		} catch(Exception e) {
			Logger.logError(e,"Unable to load filter values");
		}

		// Produce flat lists in filterValueLists from the tree sets that were populated
		// in filterValueSets.
		Set<String> keys = filterValueSets.keySet();
		for(Iterator<String> i=keys.iterator();i.hasNext();) {
			String filterName = (String)i.next();
			TreeSet set = (TreeSet)filterValueSets.get(filterName);
			if(set != null && set.size() > 0) {
				String csv = "";
				ArrayList<Object> list = new ArrayList<Object>();
				for(Iterator j=set.iterator();j.hasNext();) {
					Object t = (Object)j.next();
					list.add(t);
					if(csv.length() > 0) {
						csv += ",";
					}
					csv += t.toString();
				}
				filterValueLists.put(filterName,list);
				filterValuesCSV.put(filterName,csv);
			}
		}
	}

	/**
	 * Populate FILTER_IS_LEAP_YEAR values.
	**/
	private void loadIsLeapYear() {
		TreeSet<Object> isLeapYearValues = new TreeSet<Object>();
		filterValueSets.put(FILTER_IS_LEAP_YEAR,isLeapYearValues);
		filterDataTypes.put(FILTER_IS_LEAP_YEAR,"String");

		isLeapYearValues.add(isLeapYear()? "Y":"N");
	}

	/**
	 * Populate FILTER_TESTSTANDARDSID values.
	 * @throws Exception if anything goes wrong
	**/
	private void loadTestStandardsIDs() throws Exception {
		TreeSet<Object> testStandardsIDs = new TreeSet<Object>();
		filterValueSets.put(FILTER_TESTSTANDARDSID,testStandardsIDs);
		filterDataTypes.put(FILTER_TESTSTANDARDSID,"Integer");

		TreeSet<Object> inspectFreqIDs = new TreeSet<Object>();
		filterValueSets.put(FILTER_INSPECTFREQ,inspectFreqIDs);
		filterDataTypes.put(FILTER_INSPECTFREQ,"Integer");

		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";
		try {
			sql = "select testStandardsID from IMTestStandards";
			addIntegers(db,sql,testStandardsIDs,null);

			sql = "select inspectFreq from IMInspectFreq";
			addIntegers(db,sql,inspectFreqIDs ,null);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}
	}

	/**
	 * Populate FILTER_POLPROCESSID, FILTER_POLPROCESSID_IM, FILTER_STARTS_POLPROCESSID,
	 * FILTER_POLLUTANT, and FILTER_PROCESS values.
	 * @throws Exception if anything goes wrong
	**/
	private void loadPolProcessIDs() throws Exception {
		TreeSet<Object> polProcessIDs = new TreeSet<Object>();
		filterValueSets.put(FILTER_POLPROCESSID,polProcessIDs);
		filterDataTypes.put(FILTER_POLPROCESSID,"Integer");

		TreeSet<Object> polProcessIDsIM = new TreeSet<Object>();
		filterValueSets.put(FILTER_POLPROCESSID_IM,polProcessIDsIM);
		filterDataTypes.put(FILTER_POLPROCESSID_IM,"Integer");

		TreeSet<Object> startsPolProcessIDs = new TreeSet<Object>();
		filterValueSets.put(FILTER_STARTS_POLPROCESSID,startsPolProcessIDs);
		filterDataTypes.put(FILTER_STARTS_POLPROCESSID,"Integer");

		TreeSet<Object> pollutantIDs = new TreeSet<Object>();
		filterValueSets.put(FILTER_POLLUTANT,pollutantIDs);
		filterDataTypes.put(FILTER_POLLUTANT,"Integer");

		TreeSet<Object> processIDs = new TreeSet<Object>();
		filterValueSets.put(FILTER_PROCESS,processIDs);
		filterDataTypes.put(FILTER_PROCESS,"Integer");

		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		TreeSet<Integer> affectedIDs = new TreeSet<Integer>();
		try {
			sql = "select polProcessID"
					+ " from PollutantProcessAssoc ppa"
					+ " inner join EmissionProcess using (processID)"
					+ " inner join Pollutant using (pollutantID)"
					+ " where (isAffectedByExhaustIM='Y' or isAffectedByEvapIM='Y')";
			query.open(db,sql);
			while(query.rs.next()) {
				affectedIDs.add(Integer.valueOf(query.rs.getInt(1)));
			}
		} finally {
			query.onFinally();
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}

		for(Iterator<PollutantProcessAssociation> i=runSpec.pollutantProcessAssociations.iterator();
				i.hasNext();) {
			PollutantProcessAssociation ppa = (PollutantProcessAssociation)i.next();
			Integer key = Integer.valueOf(ppa.getDatabaseKey());
			polProcessIDs.add(key);
			if(2 == ppa.emissionProcess.databaseKey) {
				startsPolProcessIDs.add(key);
			}
			if(affectedIDs.contains(key)) {
				polProcessIDsIM.add(key);
			}
			processIDs.add(Integer.valueOf(ppa.emissionProcess.databaseKey));
			pollutantIDs.add(Integer.valueOf(ppa.pollutant.databaseKey));
		}
	}

	/**
	 * Populate FILTER_OPMODEID, FILTER_STARTS_OPMODEID, and FILTER_OPMODEID_AUX values.
	 * @throws Exception if anything goes wrong
	**/
	private void loadOpModeIDs() throws Exception {
		TreeSet<Object> opModeIDs = new TreeSet<Object>();
		filterValueSets.put(FILTER_OPMODEID,opModeIDs);
		filterDataTypes.put(FILTER_OPMODEID,"Integer");

		TreeSet<Object> startsOpModeIDs = new TreeSet<Object>();
		filterValueSets.put(FILTER_STARTS_OPMODEID,startsOpModeIDs);
		filterDataTypes.put(FILTER_STARTS_OPMODEID,"Integer");

		TreeSet<Object> opModeIDsAux = new TreeSet<Object>();
		filterValueSets.put(FILTER_OPMODEID_AUX,opModeIDsAux);
		filterDataTypes.put(FILTER_OPMODEID_AUX,"Integer");

		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";
		try {
			sql = "select opModeID from OperatingMode";
			addIntegers(db,sql,opModeIDs,null);

			sql = "select opModeID from OperatingMode where opModeID >= 101 and opModeID < 150";
			addIntegers(db,sql,startsOpModeIDs,null);

			sql = "select opModeID from OperatingMode where opModeID >= 200 and opModeID <= 299";
			addIntegers(db,sql,opModeIDsAux,null);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}
	}

	/**
	 * Populate FILTER_FUEL, FILTER_FUEL_FORMULATION, FILTER_SOURCE, FILTER_FUEL_SUBTYPE,
	 * and FILTER_HPMS_VTYPE values.
	 * @throws Exception if anything goes wrong
	**/
	private void loadOnRoadSourceAndFuels() throws Exception {
		TreeSet<Object> fuels = new TreeSet<Object>();
		filterValueSets.put(FILTER_FUEL,fuels);
		filterDataTypes.put(FILTER_FUEL,"Integer");
		TreeSet<Object> fuelFormulations = new TreeSet<Object>();
		filterValueSets.put(FILTER_FUEL_FORMULATION,fuelFormulations);
		filterDataTypes.put(FILTER_FUEL_FORMULATION,"Integer");
		TreeSet<Object> fuelSubtypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_FUEL_SUBTYPE,fuelSubtypes);
		filterDataTypes.put(FILTER_FUEL_SUBTYPE,"Integer");
		TreeSet<Object> sourceTypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_SOURCE,sourceTypes);
		filterDataTypes.put(FILTER_SOURCE,"Integer");
		TreeSet<Object> hpmsVTypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_HPMS_VTYPE,hpmsVTypes);
		filterDataTypes.put(FILTER_HPMS_VTYPE,"Integer");
		TreeSet<Object> sourceFuelTypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_SOURCEFUELTYPE,sourceFuelTypes);
		filterDataTypes.put(FILTER_SOURCEFUELTYPE,"Integer");

		TreeSet<Object> allFuels = new TreeSet<Object>();
		filterValueSets.put("ALL_"+FILTER_FUEL,allFuels);
		filterDataTypes.put("ALL_"+FILTER_FUEL,"Integer");

		TreeSet<Object> allSourceTypes = new TreeSet<Object>();
		filterValueSets.put("ALL_"+FILTER_SOURCE,allSourceTypes);
		filterDataTypes.put("ALL_"+FILTER_SOURCE,"Integer");

		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";
		try {
			for(Iterator<OnRoadVehicleSelection> i=runSpec.onRoadVehicleSelections.iterator();
					i.hasNext();) {
				OnRoadVehicleSelection s = (OnRoadVehicleSelection)i.next();
				fuels.add(Integer.valueOf(s.fuelTypeID));
				sourceTypes.add(Integer.valueOf(s.sourceTypeID));
				sourceFuelTypes.add(Integer.valueOf(s.sourceTypeID*100 + s.fuelTypeID));
			}
			for(Iterator<Object> i=fuels.iterator();i.hasNext();) {
				Integer fuelType = (Integer)i.next();
				sql = "select fuelFormulationID"
						+ " from fuelSubType"
						+ " inner join fuelFormulation using (fuelSubTypeID)"
						+ " where fuelTypeID=" + fuelType;
				addIntegers(db,sql,fuelFormulations,null);

				sql = "select fuelSubtypeID"
						+ " from fuelSubType"
						+ " where fuelTypeID=" + fuelType;
				addIntegers(db,sql,fuelSubtypes,null);
			}
			if(sourceTypes.size() > 0) {
				sql = "select distinct HPMSVtypeID"
						+ " from SourceUseType"
						+ " where sourceTypeID in (0";
				for(Iterator<Object> i=sourceTypes.iterator();i.hasNext();) {
					sql += ",";
					sql += i.next();
				}
				sql += ")";
				addIntegers(db,sql,hpmsVTypes,null);
			}

			// Fill the set of all fuels and source types
			sql = "select fuelTypeID from fuelType";
			addIntegers(db,sql,allFuels,null);

			sql = "select sourceTypeID from sourceUseType";
			addIntegers(db,sql,allSourceTypes,null);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}
	}

	/**
	 * Populate FILTER_FUEL, FILTER_FUEL_FORMULATION, FILTER_SOURCE, FILTER_FUEL_SUBTYPE,
	 * and FILTER_HPMS_VTYPE values.
	 * @throws Exception if anything goes wrong
	**/
	private void loadNonroadSourceAndFuels() throws Exception {
		TreeSet<Object> fuels = new TreeSet<Object>();
		filterValueSets.put(FILTER_FUEL,fuels);
		filterDataTypes.put(FILTER_FUEL,"Integer");
		TreeSet<Object> fuelFormulations = new TreeSet<Object>();
		filterValueSets.put(FILTER_FUEL_FORMULATION,fuelFormulations);
		filterDataTypes.put(FILTER_FUEL_FORMULATION,"Integer");
		TreeSet<Object> fuelSubtypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_FUEL_SUBTYPE,fuelSubtypes);
		filterDataTypes.put(FILTER_FUEL_SUBTYPE,"Integer");
		TreeSet<Object> sourceTypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_SOURCE,sourceTypes);
		filterDataTypes.put(FILTER_SOURCE,"Integer");
		TreeSet<Object> hpmsVTypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_HPMS_VTYPE,hpmsVTypes);
		filterDataTypes.put(FILTER_HPMS_VTYPE,"Integer");
		TreeSet<Object> sourceFuelTypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_SOURCEFUELTYPE,sourceFuelTypes);
		filterDataTypes.put(FILTER_SOURCEFUELTYPE,"Integer");

		TreeSet<Object> allFuels = new TreeSet<Object>();
		filterValueSets.put("ALL_"+FILTER_FUEL,allFuels);
		filterDataTypes.put("ALL_"+FILTER_FUEL,"Integer");

		TreeSet<Object> allSourceTypes = new TreeSet<Object>();
		filterValueSets.put("ALL_"+FILTER_SOURCE,allSourceTypes);
		filterDataTypes.put("ALL_"+FILTER_SOURCE,"Integer");

		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";
		try {
			for(Iterator<OffRoadVehicleSelection> i=runSpec.offRoadVehicleSelections.iterator();
					i.hasNext();) {
				OffRoadVehicleSelection s = (OffRoadVehicleSelection)i.next();
				fuels.add(Integer.valueOf(s.fuelTypeID));
			}
			for(Iterator<Object> i=fuels.iterator();i.hasNext();) {
				Integer fuelType = (Integer)i.next();
				sql = "select fuelFormulationID"
						+ " from nrFuelSubType"
						+ " inner join fuelFormulation using (fuelSubTypeID)"
						+ " where fuelTypeID=" + fuelType;
				addIntegers(db,sql,fuelFormulations,null);

				sql = "select fuelSubtypeID"
						+ " from nrFuelSubType"
						+ " where fuelTypeID=" + fuelType;
				addIntegers(db,sql,fuelSubtypes,null);
			}
			// Fill the set of all fuels and source types
			sql = "select fuelTypeID from nrFuelType";
			addIntegers(db,sql,allFuels,null);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}
	}

	/**
	 * Populate FILTER_ROAD_TYPE and FILTER_AVGSPEED_BIN values.
	**/
	private void loadRoadTypeValues() throws Exception {
		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";

		try {
			TreeSet<Object> roadTypes = new TreeSet<Object>();
			filterValueSets.put(FILTER_ROAD_TYPE,roadTypes);
			filterDataTypes.put(FILTER_ROAD_TYPE,"Integer");
			TreeSet<Object> highwayRoadTypes = new TreeSet<Object>();
			filterValueSets.put(FILTER_ROAD_TYPE_HWY,highwayRoadTypes);
			filterDataTypes.put(FILTER_ROAD_TYPE_HWY,"Integer");
			TreeSet<Object> nonOffnetworkRoadTypes = new TreeSet<Object>();
			filterValueSets.put(FILTER_ROAD_TYPE_NOT_OFFNETWORK,nonOffnetworkRoadTypes);
			filterDataTypes.put(FILTER_ROAD_TYPE_NOT_OFFNETWORK,"Integer");

			for(Iterator<RoadType> i=runSpec.roadTypes.iterator();i.hasNext();) {
				RoadType r = (RoadType)i.next();
				Integer ri = Integer.valueOf(r.roadTypeID);
				roadTypes.add(ri);
				if(r.roadTypeID == 2 || r.roadTypeID == 4) {
					highwayRoadTypes.add(ri);
				}
				if(r.roadTypeID > 1 && r.roadTypeID <= 5) {
					nonOffnetworkRoadTypes.add(ri);
				}
			}

			TreeSet<Object> speedBins = new TreeSet<Object>();
			filterValueSets.put(FILTER_AVGSPEED_BIN,speedBins);
			filterDataTypes.put(FILTER_AVGSPEED_BIN,"Integer");
			sql = "select avgSpeedBinID from avgSpeedBin";
			addIntegers(db,sql,speedBins,null);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}
	}

	/**
	 * Populate FILTER_HOUR, FILTER_DAY, FILTER_MONTH, FILTER_MONTH_GROUP,
	 * FILTER_YEAR, FILTER_FUEL_YEAR, FILTER_HOURDAY, and FILTER_AGE values.
	 * @throws Exception if anything goes wrong
	**/
	private void loadTimeFilterValues() throws Exception {
		TreeSet<Object> hours = new TreeSet<Object>();
		filterValueSets.put(FILTER_HOUR,hours);
		filterDataTypes.put(FILTER_HOUR,"Integer");
		TreeSet<Object> days = new TreeSet<Object>();
		filterValueSets.put(FILTER_DAY,days);
		filterDataTypes.put(FILTER_DAY,"Integer");
		TreeSet<Object> months = new TreeSet<Object>();
		filterValueSets.put(FILTER_MONTH,months);
		filterDataTypes.put(FILTER_MONTH,"Integer");
		TreeSet<Object> monthGroups = new TreeSet<Object>();
		filterValueSets.put(FILTER_MONTH_GROUP,monthGroups);
		filterDataTypes.put(FILTER_MONTH_GROUP,"Integer");
		TreeSet<Object> years = new TreeSet<Object>();
		filterValueSets.put(FILTER_YEAR,years);
		filterDataTypes.put(FILTER_YEAR,"Integer");
		fuelYears = new TreeSet<Object>();
		filterValueSets.put(FILTER_FUEL_YEAR,fuelYears);
		filterDataTypes.put(FILTER_FUEL_YEAR,"Integer");
		TreeSet<Object> agedYears = new TreeSet<Object>();
		filterValueSets.put(FILTER_AGEDYEAR,agedYears);
		filterDataTypes.put(FILTER_AGEDYEAR,"Integer");
		TreeSet<Object> agedModelYears = new TreeSet<Object>();
		filterValueSets.put(FILTER_AGEDMODELYEAR,agedModelYears);
		filterDataTypes.put(FILTER_AGEDMODELYEAR,"Integer");

		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";
		try {
			ExecutionRunSpec e = new ExecutionRunSpec(runSpec,true);
			e.buildExecutionTimeSpan(db,true);
			hours.addAll(e.hours);
			days.addAll(e.days);
			for(Iterator<Integer> i=e.years.iterator();i.hasNext();) {
				Integer year = (Integer)i.next();
				years.add(year);
				sql = "select fuelYearID from year where yearID=" + year;
				addIntegers(db,sql,fuelYears,null);
				// Add model years and calendar years within the 30 year age range of the simulated calendar year
				for(int ai=0;ai<=30;ai++) {
					int agedYear = year.intValue() - ai;
					if(agedYear >= 1960 && agedYear <= 2060) {
						agedModelYears.add(Integer.valueOf(agedYear));
					}
					if(agedYear >= 1960 && agedYear <= 2060) {
						agedYears.add(Integer.valueOf(agedYear));
					}
				}
			}
			for(Iterator<Integer> i=e.months.iterator();i.hasNext();) {
				Integer month = (Integer)i.next();
				months.add(month);
				sql = "select monthGroupID from monthOfAnyYear where monthID=" + month;
				addIntegers(db,sql,monthGroups,null);
			}

			TreeSet<Object> hourDays = new TreeSet<Object>();
			filterValueSets.put(FILTER_HOURDAY,hourDays);
			filterDataTypes.put(FILTER_HOURDAY,"Integer");
			sql = "select hourDayID from hourDay where hourID in (0";
			for(Iterator<Object> i=hours.iterator();i.hasNext();) {
				sql += ",";
				sql += i.next();
			}
			sql += ") and dayID in (0";
			for(Iterator<Object> i=days.iterator();i.hasNext();) {
				sql += ",";
				sql += i.next();
			}
			sql += ")";
			addIntegers(db,sql,hourDays,null);

			TreeSet<Object> ages = new TreeSet<Object>();
			filterValueSets.put(FILTER_AGE,ages);
			filterDataTypes.put(FILTER_AGE,"Integer");
			sql = "select ageID from ageCategory";
			addIntegers(db,sql,ages,null);
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}
	}

	/**
	 * Populate FILTER_ZONE, FILTER_COUNTY, FILTER_STATE, FILTER_FUEL_REGION,
	 * FILTER_COUNTYTYPE, and FILTER_IDLEREGION values.
	 * @throws Exception if anything goes wrong
	**/
	private void loadGeographicFilterValues() throws Exception {
		TreeSet<Object> zones = new TreeSet<Object>();
		filterValueSets.put(FILTER_ZONE,zones);
		filterDataTypes.put(FILTER_ZONE,"Integer");
		TreeSet<Object> counties = new TreeSet<Object>();
		filterValueSets.put(FILTER_COUNTY,counties);
		filterDataTypes.put(FILTER_COUNTY,"Integer");
		TreeSet<Object> states = new TreeSet<Object>();
		filterValueSets.put(FILTER_STATE,states);
		filterDataTypes.put(FILTER_STATE,"Integer");
		TreeSet<Object> regions = new TreeSet<Object>();
		filterValueSets.put(FILTER_FUEL_REGION,regions);
		filterDataTypes.put(FILTER_FUEL_REGION,"Integer");
		TreeSet<Object> offNetworkLinks = new TreeSet<Object>();
		filterValueSets.put(FILTER_OFFNETWORK_LINK,offNetworkLinks);
		filterDataTypes.put(FILTER_OFFNETWORK_LINK,"Integer");

		TreeSet<Object> countyTypes = new TreeSet<Object>();
		filterValueSets.put(FILTER_COUNTYTYPE,countyTypes);
		filterDataTypes.put(FILTER_COUNTYTYPE,"Integer");
		TreeSet<Object> idleRegions = new TreeSet<Object>();
		filterValueSets.put(FILTER_IDLEREGION,idleRegions);
		filterDataTypes.put(FILTER_IDLEREGION,"Integer");

		String fuelYearsText = StringUtilities.getCSVGeneric(fuelYears);

		if(runSpec.isCustomDomain()) {
			//zones.add(Integer.valueOf(runSpec.genericCounty.getCountyID()*10));
			counties.add(Integer.valueOf(runSpec.genericCounty.getCountyID()));
			states.add(Integer.valueOf(runSpec.genericCounty.stateID));
			regions.add(Integer.valueOf(100000000));
			return;
		}

		Connection db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		String sql = "";
		TreeSet<Object> regionCounties = new TreeSet<Object>();
		try {
			for(Iterator<GeographicSelection> i=runSpec.geographicSelections.iterator();
					i.hasNext();) {
				regionCounties.clear();
				GeographicSelection g = (GeographicSelection)i.next();
				if(g.type == GeographicSelectionType.COUNTY) {
					counties.add(Integer.valueOf(g.databaseKey));
					regionCounties.add(Integer.valueOf(g.databaseKey));
					// Get the state for the county
					sql = "select stateID from County where countyID=" + g.databaseKey;
					addIntegers(db,sql,states,null);
					// Get the zones in the county
					sql = "select zoneID from Zone where countyID=" + g.databaseKey;
					addIntegers(db,sql,zones,null);
					// Get the offnetwork links in the county
					sql = "select linkID from link where roadTypeID=1 and countyID=" + g.databaseKey;
					addIntegers(db,sql,offNetworkLinks,null);
					// Get the countyTypeID for the county
					sql = "select countyTypeID from county where countyID=" + g.databaseKey;
					addIntegers(db,sql,countyTypes,null);
					// Get the idleRegionID for the county
					sql = "select idleRegionID"
							+ " from State"
							+ " inner join County using (stateID)"
							+ " where countyID=" + g.databaseKey;
					addIntegers(db,sql,idleRegions,null);
				} else if(g.type == GeographicSelectionType.STATE) {
					states.add(Integer.valueOf(g.databaseKey));
					// Get the counties for the state
					sql = "select countyID from County where stateID=" + g.databaseKey;
					addIntegers(db,sql,counties,null);
					addIntegers(db,sql,regionCounties,null);
					// Get the zones in the state
					sql = "select zoneID"
							+ " from Zone"
							+ " inner join County using (countyID)"
							+ " where stateID=" + g.databaseKey;
					addIntegers(db,sql,zones,null);
					// Get the offnetwork links in the state
					sql = "select linkID"
							+ " from link"
							+ " inner join county using (countyID)"
							+ " where roadTypeID=1 and stateID=" + g.databaseKey;
					addIntegers(db,sql,offNetworkLinks,null);
					// Get the countyTypeIDs
					sql = "select countyTypeID from countyType";
					addIntegers(db,sql,countyTypes,null);
					// Get the idleRegionID for the state
					sql = "select idleRegionID from State where stateID=" + g.databaseKey;
					addIntegers(db,sql,idleRegions,null);
				} else if(g.type == GeographicSelectionType.NATION) {
					// Get the states and counties in the nation
					sql = "select stateID, countyID from County";
					addIntegers(db,sql,states,counties);
					// Get the zones in the nation
					sql = "select zoneID from Zone";
					addIntegers(db,sql,zones,null);
					// Get the offnetwork links in the nation
					sql = "select linkID from link where roadTypeID=1";
					addIntegers(db,sql,offNetworkLinks,null);
					// Get all countyTypeIDs 
					sql = "select countyTypeID from countyType";
					addIntegers(db,sql,countyTypes,null);
					// Get all idleRegionIDs
					sql = "select idleRegionID from idleRegion";
					addIntegers(db,sql,idleRegions,null);
					// Get the regions in the nation (only works if year is selected in runspec)
					if(fuelYearsText != "") {
						sql = "select regionID"
								+ " from regionCounty"
								+ " where regionCodeID=1"
								+ " and fuelYearID in (" + fuelYearsText + ")";
						addIntegers(db,sql,regions,null);
					}
					regionCounties.clear();
				}
				if(fuelYearsText != "" && regionCounties.size() > 0) {
					sql = "select regionID"
							+ " from regionCounty"
							+ " where regionCodeID=1"
							+ " and fuelYearID in (" + fuelYearsText + ")"
							+ " and countyID in (" + StringUtilities.getCSVGeneric(regionCounties) + ")";
					addIntegers(db,sql,regions,null);
				}
			}
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				db = null;
			}
		}
	}

	/**
	 * Perform a query, adding its Integer results to filter value sets.
	 * @param db database to be queried
	 * @param sql query to be executed
	 * @param set1 TreeSet of Integer objects to be filled with the first column's values
	 * @param set2 TreeSet of Integer objects to be filled with the second column's values.
	 * May be null if there is no second column to be read.
	 * @throws SQLException if there is a database error
	**/
	private void addIntegers(Connection db, String sql, TreeSet<Object> set1, TreeSet<Object> set2)
			throws SQLException {
		Statement statement = null;
		ResultSet rs = null;
		try {
			statement = db.createStatement();
			rs = SQLRunner.executeQuery(statement,sql);
			while(rs.next()) {
				Integer i = Integer.valueOf(rs.getInt(1));
				set1.add(i);
				if(set2 != null) {
					i = Integer.valueOf(rs.getInt(2));
					set2.add(i);
				}
			}
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
	}

	/**
	 * Add the currently selected databaes to the RunSpec as user input database
	 * or as the scale input database. This should be done after an import so that
	 * users executing the model immediately do so with their imported data.
	 * @param importDidOccur true if the database should be added because an import happened
	**/
	public void addDatabaseToRunSpec(boolean importDidOccur) {
		if((isCountyDomain && runSpec.domain == ModelDomain.SINGLE_COUNTY)
				|| (isProjectDomain && runSpec.domain == ModelDomain.PROJECT)) {
			runSpec.scaleInputDatabase = (DatabaseSelection)database.clone();
			return;
		}
		if(importDidOccur) {
			// The database selection should be put into the RunSpec as the last user-
			// input database if not already listed.
			boolean found = false;
			for(Iterator i=runSpec.databaseSelectionInputSets.iterator();i.hasNext();) {
				DatabaseSelection s = (DatabaseSelection)i.next();
				if(s.databaseName.equalsIgnoreCase(database.databaseName)) {
					found = true;
					break;
				}
			}
			if(!found) {
				runSpec.databaseSelectionInputSets.add((DatabaseSelection)database.clone());
			}
		}
	}

	/**
	 * Check a RunSpec against the inputs required to create a county domain database.
	 * @param runSpec RunSpec to be inspected.  If null, the global RunSpec from the
	 * MOVESAPI singleton will be used.
	 * @param messages holds output error and warning messages as String objects.  May be null.
	 * @return -1 if not ready, 0 if ready but with warnings, +1 if ready without warnings
	**/
	public static int isReadyForCountyDomain(RunSpec runSpec, ArrayList<String> messages) {
		if(runSpec == null) {
			runSpec = MOVESAPI.getTheAPI().getRunSpec();
			if(runSpec == null) {
				if(messages != null) {
					messages.add("Error: No RunSpec found");
				}
				return -1;
			}
		}
		int result = +1;
		if(runSpec.domain != ModelDomain.PROJECT
				&& runSpec.domain != ModelDomain.SINGLE_COUNTY) {
			if(messages != null) {
				messages.add("Error: Select either County or Project domain first.");
			}
			result = -1;
		}
		if(runSpec.timeSpan.years.size() <= 0) {
			if(messages != null) {
				messages.add("Error: Select a calendar year first.");
			}
			result = -1;
		} else if(runSpec.timeSpan.years.size() > 1) {
			if(messages != null) {
				messages.add("Error: A domain database can contain only one calendar year,"
						+ " but the runspec has " + runSpec.timeSpan.years.size() + ".");
			}
			result = -1;
		}

		GeographicSelection g = null;
		if(runSpec.geographicSelections.size() <= 0) {
			if(messages != null) {
				messages.add("Error: Select a single county first.");
			}
			result = -1;
		} else if(runSpec.geographicSelections.size() == 1) {
			g = (GeographicSelection)runSpec.geographicSelections.first();
			if(g.type != GeographicSelectionType.COUNTY) {
				if(messages != null) {
					messages.add("Error: Too many counties selected.  Select exactly one first.");
				}
				result = -1;
				g = null;
			}
		} else if(runSpec.geographicSelections.size() > 1) {
			if(messages != null) {
				messages.add("Error: Too many counties selected.  Select exactly one first.");
			}
			result = -1;
		}
		if(runSpec.domain == ModelDomain.PROJECT) {
			int count = 0;

			// Only 1 month at a time
			count = runSpec.timeSpan.months.size();
			if(count < 1) {
				if(messages != null) {
					messages.add("Error: Select a month first.");
				}
				result = -1;
			} else if(count > 1) {
				if(messages != null) {
					messages.add("Error: Too many months selected.  Select exactly one first.");
				}
				result = -1;
			}

			// Only 1 day at a time
			count = runSpec.timeSpan.days.size();
			if(count < 1) {
				if(messages != null) {
					messages.add("Error: Select a day first.");
				}
				result = -1;
			} else if(count > 1) {
				if(messages != null) {
					messages.add("Error: Too many days selected.  Select exactly one first.");
				}
				result = -1;
			}

			// Only 1 hour at a time
			if(runSpec.timeSpan.beginHourID <= 0 || runSpec.timeSpan.endHourID <= 0) {
				if(messages != null) {
					messages.add("Error: Select an hour first.");
				}
				result = -1;
			} else if(runSpec.timeSpan.beginHourID != runSpec.timeSpan.endHourID) {
				if(messages != null) {
					messages.add("Error: Too many hours selected.  Select exactly one first.");
				}
				result = -1;
			}
		}
		return result;
	}

	/**
	 * Get a set of Integer objects from the database.
	 * @param db database holding the table to be scanned
	 * @param sql statement to get values.  Only the first column is used and it
	 * must be an integer-compatible type.
	 * @return a set of Integer objects
	 * @throws Exception if anything goes wrong
	**/
	public TreeSet<Object> getIntegers(Connection db, String sql) throws Exception {
		TreeSet<Object> results = new TreeSet<Object>();
		Statement statement = null;
		ResultSet rs = null;
		try {
			statement = db.createStatement();
			rs = SQLRunner.executeQuery(statement,sql);
			while(rs.next()) {
				Integer i = Integer.valueOf(rs.getInt(1));
				results.add(i);
			}
			return results;
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql statement to get values.  Only the first column is used and it
	 * must be an integer-compatible type.
	 * @param neededValues set of Integer objects defining all required values
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasIntegers(Connection db, String sql, TreeSet<Object> neededValues,
			IImporter importer, String qualityMessagePrefix)
			throws Exception {
		return tableHasIntegers(db,sql,neededValues,null,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql statement to get values.  Only the first column is used and it
	 * must be an integer-compatible type.
	 * @param neededValues set of Integer objects defining all required values
	 * @param alternateNeededValueSource optional SQL to obtain an alternate set of
	 * needed values.  If this is provided and yields an empty set, the result is false,
	 * unlike the handling of normal needed values.
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasIntegers(Connection db, String sql, TreeSet<Object> neededValues, String alternateNeededValueSource,
			IImporter importer, String qualityMessagePrefix)
			throws Exception {
		SQLRunner.Query query = new SQLRunner.Query();
		TreeSet<Object> neededValuesToUse = neededValues;
		if(alternateNeededValueSource != null && alternateNeededValueSource.length() > 0) {
			try {
				query.open(db,alternateNeededValueSource);
				neededValuesToUse = new TreeSet<Object>();
				while(query.rs.next()) {
					Integer i = Integer.valueOf(query.rs.getInt(1));
					neededValuesToUse.add(i);
				}
				// If we got this far, the alternate source exists and we will reject anything that
				// depends upon this empty alternate source.
				if(neededValuesToUse.size() <= 0) {
					return false;
				}
			} catch(Exception e) {
				// This happens if the alternate source doesn't exist.
				// If so, just continue on and use the neededValues like normal.
			} finally {
				query.onFinally();
			}
		}
		if(neededValues == neededValuesToUse) {
			if(neededValues == null || neededValues.size() <= 0) {
				return true;
			}
		}
		//System.out.println("Searching for " + neededValuesToUse.size() + " items: " + sql);
		TreeSet<Object> foundNeededValues = new TreeSet<Object>();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				Integer i = Integer.valueOf(query.rs.getInt(1));
				if(neededValuesToUse.contains(i)) {
					//System.out.println("Matched " + i);
					foundNeededValues.add(i);
				} else {
					//System.out.println("Not matched " + i);
				}
			}
			if(neededValuesToUse.size() == foundNeededValues.size()) {
				return true;
			}
			if(importer != null && qualityMessagePrefix != null) {
				String neededText = "";
				for(Object n : neededValuesToUse) {
					if(!foundNeededValues.contains(n)) {
						if(neededText.length() > 0) {
							neededText += ",";
						}
						neededText += n.toString();
					}
				}
				if(neededText.length() > 0) {
					neededText = " " + neededText;
				}
				importer.addQualityMessage(qualityMessagePrefix + neededText);
			}
			return false;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql statement to get values.  Only the first column is used and it
	 * must be an integer-compatible type.
	 * @param neededValues set of Integer objects defining all required values
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasIntegerObjects(Connection db, String sql, TreeSet<Integer> neededValues,
			IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(neededValues == null || neededValues.size() <= 0) {
			return true;
		}
		//System.out.println("Searching for " + neededValues.size() + " items: " + sql);
		TreeSet<Object> foundNeededValues = new TreeSet<Object>();
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				Integer i = Integer.valueOf(query.rs.getInt(1));
				if(neededValues.contains(i)) {
					//System.out.println("Matched " + i);
					foundNeededValues.add(i);
				} else {
					//System.out.println("Not matched " + i);
				}
			}
			if(neededValues.size() == foundNeededValues.size()) {
				return true;
			}
			if(importer != null && qualityMessagePrefix != null) {
				String neededText = "";
				for(Object n : neededValues) {
					if(!foundNeededValues.contains(n)) {
						if(neededText.length() > 0) {
							neededText += ",";
						}
						neededText += n.toString();
					}
				}
				if(neededText.length() > 0) {
					neededText = " " + neededText;
				}
				importer.addQualityMessage(qualityMessagePrefix + neededText);
			}
			return false;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Build all needed caches for validation and wildcarding.
	 * @throws Exception if anything goes wrong
	**/
	private void loadCaches() throws Exception {
		if(hasCaches) {
			return;
		}
		roadTypes = new TreeSet<Object>();
		nonOffNetworkRoadTypes = new TreeSet<Object>();
		for(Iterator<RoadType> i=runSpec.roadTypes.iterator();i.hasNext();) {
			RoadType r = (RoadType)i.next();
			Integer ri = Integer.valueOf(r.roadTypeID);
			roadTypes.add(ri);
			if(r.roadTypeID != 1) {
				nonOffNetworkRoadTypes.add(ri);
			}
		}

		sourceTypes = new TreeSet<Object>();
		fuelTypes = new TreeSet<Object>();
		for(Iterator<OnRoadVehicleSelection> i=runSpec.onRoadVehicleSelections.iterator();
				i.hasNext();) {
			OnRoadVehicleSelection s = (OnRoadVehicleSelection)i.next();
			sourceTypes.add(Integer.valueOf(s.sourceTypeID));
			fuelTypes.add(Integer.valueOf(s.fuelTypeID));
		}

		counties = new TreeSet<Object>();
		String countyIDsText = "0";
		for(Iterator<GeographicSelection> i=runSpec.geographicSelections.iterator();i.hasNext();) {
			GeographicSelection g = (GeographicSelection)i.next();
			if(g.type == GeographicSelectionType.COUNTY) {
				counties.add(Integer.valueOf(g.databaseKey));
				countyIDsText += "," + g.databaseKey;
			}
		}

		ages = new TreeSet<Object>();
		for(int i=0;i<=30;i++) {
			ages.add(Integer.valueOf(i));
		}

		boolean isCustomDomain = runSpec.isCustomDomain();

		Connection defaultDb =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		try {
			String t = "0";
			for(Iterator<Object> i=sourceTypes.iterator();i.hasNext();) {
				t += "," + i.next();
			}
			hpmsVTypes = getIntegers(defaultDb,
					"select distinct hpmsVTypeID from sourceUseType"
					+ " where sourceTypeID in (" + t + ")");

			loadTimesInRunSpec();

			if(isCustomDomain) {
				zones = new TreeSet<Object>();
				zones.add(Integer.valueOf(runSpec.genericCounty.getCountyID()*10));
			} else {
				zones = getIntegers(defaultDb,"select zoneID"
						+ " from zone"
						+ " where countyID in (" + countyIDsText + ")");
			}

			hasCaches = true;
		} finally {
			if(defaultDb != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,defaultDb);
				defaultDb = null;
			}
		}
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasYears(Connection db, String sql, IImporter importer, String qualityMessagePrefix) throws Exception {
		return tableHasIntegerObjects(db,sql,runSpec.timeSpan.years,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasRoadTypes(Connection db, String sql, IImporter importer, String qualityMessagePrefix) throws Exception {
		if(roadTypes == null) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,roadTypes,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasNonOffnetworkRoadTypes(Connection db, String sql, IImporter importer, String qualityMessagePrefix) throws Exception {
		if(nonOffNetworkRoadTypes == null) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,nonOffNetworkRoadTypes,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasSourceTypes(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(!hasCaches) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,sourceTypes,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasFuelTypes(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(!hasCaches) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,fuelTypes,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasCounties(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(counties != null) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,counties,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasZones(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(zones == null) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,zones,"select zoneID from Zone",importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasAges(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(counties != null) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,ages,importer,qualityMessagePrefix);
	}

	/**
	 * Build the cache of time-related IDs for the items in the RunSpec.
	 * @throws Exception if anything goes wrong
	**/
	private void loadTimesInRunSpec() throws Exception {
		if(!TimeSpan.isLoaded()) {
			TimeSpan.loadTimeObjects();
		}
		if(hours != null && days != null && hourDays != null) {
			return;
		}
		int i;
		Integer v;
		TreeSet<Object> t;
		Connection defaultDb =
				DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
		try {
			String daysText = "0";
			days = new TreeSet<Object>();
			for(Iterator<TimeSpan.Day> ti=runSpec.timeSpan.days.iterator();ti.hasNext();) {
				TimeSpan.Day d = (TimeSpan.Day)ti.next();
				days.add(Integer.valueOf(d.dayID));
				daysText += "," + d.dayID;
			}

			String hoursText = "0";
			hours = new TreeSet<Object>();
			for(Iterator<TimeSpan.Hour> ti=TimeSpan.allHours.iterator();ti.hasNext();) {
				TimeSpan.Hour h = (TimeSpan.Hour)ti.next();
				if(runSpec.timeSpan.beginHourID <= h.hourID
						&& runSpec.timeSpan.endHourID >= h.hourID) {
					hours.add(Integer.valueOf(h.hourID));
					hoursText += "," + h.hourID;
				}
			}

			hourDays = getIntegers(defaultDb,"select hourDayID"
					+ " from hourDay"
					+ " where hourID in (" + hoursText + ")"
					+ " and dayID in (" + daysText + ")");

			if(days.contains(Integer.valueOf(2))) {
				day2HourDayIDs = getIntegers(defaultDb,"select hourDayID"
						+ " from hourDay"
						+ " where hourID in (" + hoursText + ")"
						+ " and dayID=2");
			} else {
				day2HourDayIDs = new TreeSet<Object>();
			}
			if(days.contains(Integer.valueOf(5))) {
				day5HourDayIDs = getIntegers(defaultDb,"select hourDayID"
						+ " from hourDay"
						+ " where hourID in (" + hoursText + ")"
						+ " and dayID=5");
			} else {
				day5HourDayIDs = new TreeSet<Object>();
			}

			months = new TreeSet<Object>();
			for(Iterator<TimeSpan.Month> ti=runSpec.timeSpan.months.iterator();ti.hasNext();) {
				TimeSpan.Month m = (TimeSpan.Month)ti.next();
				months.add(Integer.valueOf(m.monthID));
			}
		} finally {
			if(defaultDb != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,defaultDb);
				defaultDb = null;
			}
		}
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasHours(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(!hasCaches) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,hours,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasDays(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(!hasCaches) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,days,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasHourDays(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(!hasCaches) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,hourDays,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasMonths(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(!hasCaches) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,months,importer,qualityMessagePrefix);
	}

	/**
	 * Check a table for presence of all needed values.
	 * @param db database holding the table to be scanned
	 * @param sql query returning all values in the database
	 * @param importer optional object to receive quality messages
	 * @param qualityMessagePrefix optional prefix to use for quality messages, never null if importer is provided
	 * @return true if all neededValues are present, even if other values are present
	 * in the database.
	 * @throws Exception if anything goes wrong
	**/
	public boolean tableHasHPMSVTypes(Connection db, String sql, IImporter importer, String qualityMessagePrefix)
			throws Exception {
		if(hpmsVTypes == null) {
			loadCaches();
		}
		return tableHasIntegers(db,sql,hpmsVTypes,importer,qualityMessagePrefix);
	}

	/** True if all cached IDs from the RunSpec have been created **/
	boolean hasCaches = false;
	/** IDs from the active RunSpec **/
	TreeSet<Object> roadTypes = null;
	/** IDs, except road type 1, from the active RunSpec **/
	TreeSet<Object> nonOffNetworkRoadTypes = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> sourceTypes = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> fuelTypes = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> counties = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> zones = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> ages = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> hours = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> days = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> hourDays = null;
	/** hourDayIDs from day 2 (Weekend) in the active RunSpec **/
	TreeSet<Object> day2HourDayIDs = null;
	/** hourDayIDs from day 5 (Weekday) in the active RunSpec **/
	TreeSet<Object> day5HourDayIDs = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> months = null;
	/** IDs from the active RunSpec **/
	TreeSet<Object> hpmsVTypes = null;

	/**
	 * Check a database for suitability as a county domain data source for a given
	 * RunSpec.  A database without county/year information may be tolerated depending
	 * upon the isForRun parameter.
	 * @param runSpec the target RunSpec for which compatibility is desired
	 * @param messages list of String objects, filled with reasons why the database is
	 * incompatible.  If null, no reasons will be supplied.
	 * @param db Connection to the database to be examined
	 * @param isForRun true if the database is being used without further user involvement.
	 * Such databases do not tolerate empty county/year information, but on while on the GUI
	 * they will since an import operation will fill them correctly.
	 * @return -1 if a county database that does not match the county/year in the RunSpec.
	 * This includes databases with multiple counties, multiple years, or having a county or year
	 * that does not include the one in the RunSpec.  0 if the database is acceptable but still
	 * incomplete.  +1 if the database is acceptable and complete.
	**/
	public static int isCountyDomainDatabase(RunSpec runSpec, ArrayList<String> messages,
			Connection db, boolean isForRun) {
		if(runSpec.timeSpan.years.size() == 0) {
			if(messages != null) {
				messages.add("Error: You should select the calendar year in the Time panel before selecting a County Domain database.");
			}
			return -1;
		}else
		if(runSpec.timeSpan.years.size() > 1) {
			if(messages != null) {
				messages.add("Error: You should select only one calendar year in the Time panel before selecting a County Domain database.");
			}
			return -1;
		}
		int year = ((Integer)runSpec.timeSpan.years.first()).intValue();
		int countyID = -1;
		if(runSpec.geographicSelections.size() == 1) {
			GeographicSelection g = (GeographicSelection)runSpec.geographicSelections.first();
			if(g.type == GeographicSelectionType.COUNTY) {
				countyID = g.databaseKey;
			}
		}
		if(countyID < 0) {
			if(messages != null) {
				messages.add("Error: RunSpec does not specify exactly one county.");
			}
			return -1;
		}
		String sql = "";
		try {
			// The County table must be empty or have only the desired County.
			sql = "select count(*) from County";
			int countyCount = 0;
			try {
				countyCount = (int)SQLRunner.executeScalar(db,sql);
				if(countyCount > 1) {
					if(messages != null) {
						messages.add("Error: The database has more than one county.");
					}
					return -1;
				} else if(countyCount == 1) {
					sql = "select count(*) from County where countyID=" + countyID;
					countyCount = (int)SQLRunner.executeScalar(db,sql);
					if(countyCount != 1) {
						if(messages != null) {
							messages.add("Error: The database already has another county.");
						}
						return -1;
					}
					
					// The county's countyTypeID and MSA can't be NULL
					sql = "select count(*) from County where ISNULL(countyTypeID) or ISNULL(msa)";
					int numRowsWithNullData = (int)SQLRunner.executeScalar(db,sql);
					if(numRowsWithNullData > 0) {
						if(messages != null) {
							messages.add("Error: The County table is missing countyTypeID and/or MSA data. To fix, recreate this database from scratch using the County Database Manager.");
						}
						return -1;
					}
				}
			} catch(Exception e) {
				// An error here indicates the lack of the County table.
				// Treat this as if the County doesn't exist in the database.
				countyCount = 0;
			}
			if(countyCount == 0) {
				if(isForRun) {
					if(messages != null) {
						messages.add("Error: The database does not have the required county.");
					}
					return -1;
				}
			}
			if(countyCount == 1) {
				// The county exists, so check supporting tables.
				// The other geography tables are filled based on the County and a simple
				// join to ensure State->County->Zone->ZoneRoadType exist will suffice.
				sql = "select count(*)"
						+ " from state s"
						+ " inner join county c on c.stateID=s.stateID"
						+ " inner join zone z on z.countyID=c.countyID";
				if(!runSpec.isCustomDomain()) {
					sql += " inner join zoneRoadType zrt on zrt.zoneID=z.zoneID";
				}
				sql += " where c.countyID=" + countyID;
				int supportingCount = (int)SQLRunner.executeScalar(db,sql);
				if(supportingCount < 1) { // 1 or more is required
					if(messages != null) {
						messages.add("Error: Core geography tables are lacking data, meaning this database is incompatible with this RunSpec. Check to make sure this is the database you intend; if so, you may need to recreate it from scratch.");
					}
					return -1;
				}

				//check RoadTypeDistribution (cannot be empty at County Scale or Custom Domain -- not used at Project Scale)
				if(runSpec.domain == ModelDomain.SINGLE_COUNTY || runSpec.isCustomDomain()) {
					sql = "select count(*)"
						+ " from roadtypedistribution";
					int rtdCount = (int)SQLRunner.executeScalar(db,sql);
					if(rtdCount < 1) { // 1 or more is required
						if(isForRun) { // (only make pop-up if no more user interaction is expected)
							if(messages != null) {
								messages.add("Error: The RoadTypeDistribution is empty. In County Domain/Scale, users must provide the RoadTypeDistribution for the county.");
							}
							return -1;
						}
					}
				}

			}
			// The Year table must be empty or have only the desired Year.
			sql = "select count(*) from Year";
			int yearCount = 0;
			try {
				yearCount = (int)SQLRunner.executeScalar(db,sql);
				if(yearCount > 1) {
					if(messages != null) {
						messages.add("Error: The database has more than one year.");
					}
					return -1;
				} else if(yearCount == 1) {
					sql = "select count(*) from Year where yearID=" + year;
					yearCount = (int)SQLRunner.executeScalar(db,sql);
					if(yearCount != 1) {
						if(messages != null) {
							messages.add("Error: The database already has another year.");
						}
						return -1;
					}
				}
			} catch(Exception e) {
				// An error here indicates the lack of the Year table.
				// Treat this as if the Year doesn't exist in the database.
				yearCount = 0;
			}
			if(yearCount == 0) {
				if(isForRun) {
					if(messages != null) {
						messages.add("Error: The database does not have the required year.");
					}
					return -1;
				}
			}
			if(yearCount == 0) {
				if(messages != null) {
					messages.add("Warning: Perform an Import to set the database Year entry.");
				}
			}
			if(countyCount == 0) {
				if(messages != null) {
					messages.add("Warning: Perform an Import to set the database County entry.");
				}
			}
			return (countyCount == 1 && yearCount == 1)? +1 : 0;
		} catch(SQLException e) {
			Logger.logSqlError(e,"Unable to check for domain data",sql);
			if(messages != null) {
				messages.add("Error: Unable to check for domain data");
			}
			return -1;
		}
	}

	/**
	 * Inform a reader of wildcard possibilities, if they exist for a given column.
	 * @param sqlColumnName name of the column in the database that will receive the data
	 * @param reader reader to be configured
	 * @param fileColumnIndex index within the reader's row
	**/
	public void setupWildcards(String sqlColumnName, CellFileReader reader, int fileColumnIndex) {
		if(!hasCaches) {
			try {
				loadCaches();
			} catch(Exception e) {
				Logger.logError(e,"Unable to setup wildcards");
				return;
			}
		}
		if(sqlColumnName.equalsIgnoreCase("dayID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",days);
		} else if(sqlColumnName.equalsIgnoreCase("hourID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",hours);
		} else if(sqlColumnName.equalsIgnoreCase("hourDayID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",hourDays);

			reader.addWildcardDefinition(fileColumnIndex,"ALL Weekday",day5HourDayIDs);
			reader.addWildcardDefinition(fileColumnIndex,"ALL 5",day5HourDayIDs);
			reader.addWildcardDefinition(fileColumnIndex,"5*",day5HourDayIDs);
			reader.addWildcardDefinition(fileColumnIndex,"*5",day5HourDayIDs);

			reader.addWildcardDefinition(fileColumnIndex,"ALL Weekend",day2HourDayIDs);
			reader.addWildcardDefinition(fileColumnIndex,"ALL 2",day2HourDayIDs);
			reader.addWildcardDefinition(fileColumnIndex,"2*",day2HourDayIDs);
			reader.addWildcardDefinition(fileColumnIndex,"*2",day2HourDayIDs);
		} else if(sqlColumnName.equalsIgnoreCase("monthID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",months);
		} else if(sqlColumnName.equalsIgnoreCase("yearID")) {
			TreeSet<Object> yearObjects = new TreeSet<Object>();
			for(Iterator<Integer> i=runSpec.timeSpan.years.iterator();i.hasNext();) {
				yearObjects.add((Object)i.next());
			}
			reader.addWildcardDefinition(fileColumnIndex,"ALL",yearObjects);
		} else if(sqlColumnName.equalsIgnoreCase("roadTypeID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",roadTypes);
		} else if(sqlColumnName.equalsIgnoreCase("sourceTypeID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",sourceTypes);
		} else if(sqlColumnName.equalsIgnoreCase("hpmsVTypeID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",hpmsVTypes);
		} else if(sqlColumnName.equalsIgnoreCase("countyID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",counties);
		} else if(sqlColumnName.equalsIgnoreCase("zoneID")) {
			reader.addWildcardDefinition(fileColumnIndex,"ALL",zones);
		}
	}

	/**
	 * Examine the set of years in the current filter and check for leap year status.
	 * @return true if there is exactly one filtered year and it is a leap year
	**/
	public boolean isLeapYear() {
		if(runSpec.timeSpan.years.size() == 1) {
			for(Iterator<Integer> i=runSpec.timeSpan.years.iterator();i.hasNext();) {
				int y = i.next().intValue();
				if((y%4) == 0 && ((y%100) != 0 || (y%400)==0)) {
					return true;
				}
			}
		}
		return false;
	}
}
