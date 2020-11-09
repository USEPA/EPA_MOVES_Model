/**
 * @(#)NonroadEmissionCalculator.java
 */
package gov.epa.otaq.moves.master.nonroad;

import gov.epa.otaq.moves.common.BundleManifest;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.master.framework.EmissionCalculator;
import gov.epa.otaq.moves.master.framework.EmissionCalculatorRegistration;
import gov.epa.otaq.moves.master.framework.EmissionProcess;
import gov.epa.otaq.moves.master.framework.ExecutionRunSpec;
import gov.epa.otaq.moves.master.framework.MasterLoop;
import gov.epa.otaq.moves.master.framework.MasterLoopContext;
import gov.epa.otaq.moves.master.framework.MasterLoopGranularity;
import gov.epa.otaq.moves.master.framework.MasterLoopPriority;
import gov.epa.otaq.moves.master.framework.MOVESEngine;
import gov.epa.otaq.moves.master.framework.MOVESEngineListener;
import gov.epa.otaq.moves.master.framework.Pollutant;
import gov.epa.otaq.moves.master.framework.SQLForWorker;
import gov.epa.otaq.moves.master.framework.SystemConfiguration;
import gov.epa.otaq.moves.master.framework.WeeksInMonthHelper;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;
import java.util.TreeSet;
import gov.epa.otaq.moves.common.TreeMapIgnoreCase;
import gov.epa.otaq.moves.common.TreeSetIgnoreCase;

/**
 * Main class run NONROAD simulation. It is for all the NONROAD pollutants and
 * processes. The simulation will be divided into pieces and spread onto worker
 * computers to run.
 * 
 * @author Wesley Faler
 * @author Jizhen (Jason) Zhao
 * @version 2015-04-07
**/
public class NonroadEmissionCalculator extends EmissionCalculator
		implements MOVESEngineListener, MasterLoopContext.IContextFilter {
	/**
	 * @algorithm
	 * @owner Nonroad Calculator
	 * @calculator
	**/

	/** Mutex used to synchronize access to variables **/
	private static Integer mutex = Integer.valueOf(42);

	/** Set of pollutant/processes calculated by the Nonroad.exe program **/
	public static final int[] nonroadPolProcessIDs = {
		131,130,118,119,132,120,121,9001,9101,9901,101,201,301,3001,3101,10001,11001,115
	};

	/** Flag used during developer testing **/
	private static final boolean isTest = false;

	/** Temporary folder holding Nonroad data files **/
	File tempDataFolder = null;

	/** Number of bundles created so far **/
	private int calledTimes = 0;

	/** True after subscribing as a MOVESEngineListener **/	
	private boolean didSubscribeToEngineEvents = false;

	/** Zone-Year-Month-Day combinations that have been processed **/
	TreeSet<String> processedKeys = new TreeSet<String>();
	/** Loop that owns this calculator **/
	MasterLoop owningLoop = null;
	/** true when the owningLoop is only counting bundles **/
	boolean isCountingBundles = true;
	/** space-separated list of pollutant/process IDs **/
	String desiredPolProcessIDs = "";

	/** Provide information to convert BSFC from gallons/month to gallons/hour **/
	WeeksInMonthHelper weeksHelper = null;

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator. Such registration facilitates calculator
	 * chaining.
	 **/
	public NonroadEmissionCalculator() {
		register();
		Logger.log(LogMessageCategory.INFO,
				"In Nonroad Emission Calculator constructor: calledTimes = "
						+ calledTimes + ".");
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * 
	 * @param targetLoop
	 *            The loop to subscribe to.
	 **/
	@Override
	public void subscribeToMe(MasterLoop targetLoop) {
		owningLoop = targetLoop;
		isCountingBundles = true;

		boolean foundProcess = false;
		for(int i=0;i<nonroadPolProcessIDs.length;i++) {
			int processID = nonroadPolProcessIDs[i] % 100;
			int pollutantID = (int)(nonroadPolProcessIDs[i] / 100);
			EmissionProcess process = EmissionProcess.findByID(processID);
			Pollutant pollutant = Pollutant.findByID(pollutantID);
			if(process != null && pollutant != null
					&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(pollutant,process)) {
				targetLoop.subscribe(this, process, MasterLoopGranularity.DAY,
						MasterLoopPriority.EMISSION_CALCULATOR);
				foundProcess = true;
				if(desiredPolProcessIDs.length() > 0) {
					desiredPolProcessIDs += " ";
				}
				desiredPolProcessIDs += nonroadPolProcessIDs[i];
			}
		}
		if (!foundProcess) {
			Logger.log(LogMessageCategory.WARNING,
					"NonroadEmissionCalculator created needlessly.");
			return;
		}
	}

	/**
	 * Builds SQL statements for a distributed worker to execute. This is called
	 * by NonroadEmissionCalculator.executeLoop.
	 * 
	 * @param context
	 *            The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	 **/
	@Override
	public SQLForWorker doExecute(MasterLoopContext context) {
		if(weeksHelper == null) {
			weeksHelper = new WeeksInMonthHelper();
		}
		double weeksPerMonth = weeksHelper.getWeeksPerMonth(context.year,context.monthID);
		double monthToRealDayFactor = 1.0/(weeksPerMonth*7.0);

		SQLForWorker sqlForWorker = new SQLForWorker();
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		/*
		sqlForWorker.processingSQL.add("DROP TABLE IF EXISTS nrequipmenttype;");
		sqlForWorker.processingSQL.add("CREATE TABLE  nrequipmenttype ("
				+ "  NREquipTypeID smallint(6) NOT NULL,"
				+ "  description char(40) DEFAULT NULL,"
				+ "  sectorID smallint(6) NOT NULL,"
				+ "  useDefaultScrappage char(1) DEFAULT NULL,"
				+ "  surrogateID smallint(6) DEFAULT NULL,"
				+ "  PRIMARY KEY (NREquipTypeID),"
				+ "  UNIQUE KEY XPKNREquipmentType (NREquipTypeID)"
				+ ") ENGINE=MyISAM DEFAULT CHARSET=latin1;");

		sqlForWorker.processingSQL.add("DROP TABLE IF EXISTS nrscc;");
		sqlForWorker.processingSQL.add("CREATE TABLE  nrscc ("
				+ "  SCC char(10) NOT NULL,"
				+ "  NREquipTypeID smallint(6) NOT NULL,"
				+ "  strokes tinyint(4) DEFAULT NULL,"
				+ "  description char(40) DEFAULT NULL,"
				+ "  fuelTypeID smallint(6) NOT NULL,"
				+ "  PRIMARY KEY (SCC)," + "  UNIQUE KEY XPKNRSCC (SCC)"
				+ ") ENGINE=MyISAM DEFAULT CHARSET=latin1;");

		sqlForWorker.processingSQL.add("DROP TABLE IF EXISTS enginetech;");
		sqlForWorker.processingSQL.add("CREATE TABLE  enginetech ("
				+ "  engTechID smallint(6) NOT NULL DEFAULT '0',"
				+ "  engTechName char(50) DEFAULT NULL,"
				+ "  PRIMARY KEY (engTechID)"
				+ ") ENGINE=MyISAM DEFAULT CHARSET=latin1;");

		sqlForWorker.processingSQL.add("DROP TABLE IF EXISTS nrsourceusetype;");
		sqlForWorker.processingSQL.add("CREATE TABLE  nrsourceusetype ("
				+ " sourceTypeID smallint(6) NOT NULL,"
				+ " SCC char(10) NOT NULL,"
				+ " NRHPRangeBinID smallint(6) NOT NULL,"
				+ " medianLifeFullLoad float DEFAULT NULL,"
				+ " hoursUsedPerYear float DEFAULT NULL,"
				+ " loadFactor float DEFAULT NULL,"
				+ " hpAvg float DEFAULT NULL,"
				+ " isPumpFilled char(1) DEFAULT NULL,"
				+ " tankUnits char(7) DEFAULT NULL,"
				+ " tankSize float DEFAULT NULL,"
				+ " tankFillFrac float DEFAULT NULL,"
				+ " tankMetalFrac float DEFAULT NULL,"
				+ " hoseLength float DEFAULT NULL,"
				+ " hoseDiameter float DEFAULT NULL,"
				+ " hoseMetalFrac float DEFAULT NULL,"
				+ " marineFillNeckHoseLength float DEFAULT NULL,"
				+ " marineFillNeckHoseDiameter float DEFAULT NULL,"
				+ " marineSupplyHoseLength float DEFAULT NULL,"
				+ " marineSupplyHoseDiameter float DEFAULT NULL,"
				+ " marineVentHoseLength float DEFAULT NULL,"
				+ " marineVentHoseDiameter float DEFAULT NULL,"
				+ " hotSoaksPerSHO float DEFAULT NULL,"
				+ " nonInstMarineTankFrac float DEFAULT NULL,"
				+ " marineInstPlasticTankTrailFrac float NOT NULL,"
				+ " marineInstPlasticTankWaterFrac float DEFAULT NULL,"
				+ " marineInstMetalTankTrailerFrac float DEFAULT NULL,"
				+ " marineInstMetalTankWaterFrac float DEFAULT NULL,"
				+ " e10TankPermeationAdjFac float DEFAULT NULL,"
				+ " e10HosePermeationAdjFac float DEFAULT NULL,"
				+ " e10MarineFillNeckPermAdjFac float DEFAULT NULL,"
				+ " e10MarineSupplyHosePermAdjFac float DEFAULT NULL,"
				+ " e10MarineVentHosePermAdjFac float DEFAULT NULL,"
				+ " PRIMARY KEY (sourceTypeID),"
				+ " UNIQUE KEY XPKNRSourceUseType (sourceTypeID)"
				+ ") ENGINE=MyISAM DEFAULT CHARSET=latin1;");

		sqlForWorker.processingSQL.add("nonroad monthToRealDayFactor=" + monthToRealDayFactor + " " + desiredPolProcessIDs + ";");
		*/

		replacements.put("##NRmonthToRealDayFactor##",""+monthToRealDayFactor);
		replacements.put("##NRPolProcessIDs##",desiredPolProcessIDs);

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/NonroadCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}

	/**
	 * register to potential processes and pollutants handled by this
	 * calculator. Such registration facilitates calculator chaining.
	 **/
	private void register() {
		for(int i=0;i<nonroadPolProcessIDs.length;i++) {
			int processID = nonroadPolProcessIDs[i] % 100;
			int pollutantID = (int)(nonroadPolProcessIDs[i] / 100);
			EmissionProcess process = EmissionProcess.findByID(processID);
			Pollutant pollutant = Pollutant.findByID(pollutantID);
			if(process != null && pollutant != null) {
				EmissionCalculatorRegistration.register(pollutant, process, this);
			}
		}
	}

//TODO	@Override
	public void addToManifestAndFiles(MasterLoopContext context,
			Connection executionDB, BundleManifest manifest,
			File temporaryFolderPath, List<File> tableDumpFilePaths)
			throws FileNotFoundException {

		// check the current process and map it to nonroad processes
		// use these info to control data and opt file generation
		// TODO

		// Add to the manifest
		// TODO right now wrap the data files and all other files together
		// so do not need to add other information. Use this later.

		// Build the Data files

		if (!this.getDataFiles(executionDB)) {
			Logger.log(LogMessageCategory.ERROR,
					"Failed to get nonroad data files, program stopped.");
			return;
		}

		// Build OPT file

		PrintWriter optFileWriter = null;

		File optFilePath = new File(temporaryFolderPath, "nonroad.opt");
		optFileWriter = new PrintWriter(new BufferedWriter(
				new OutputStreamWriter(new FileOutputStream(optFilePath))));
		NonroadInputCreator.theNonroadInputCreator.generateOPTFile(context,
				optFileWriter, executionDB);

		// Add nonroad.exe to the bundle
		File copiedFile = null;
		File nonroadExe = SystemConfiguration.getTheSystemConfiguration().nonroadExePath;

		try {
			NonroadTemparyDataFileManager.copyFileToFolder(nonroadExe,
					temporaryFolderPath);
			copiedFile = new File(temporaryFolderPath, nonroadExe.getName());
			if (!isTest) {
				tableDumpFilePaths.add(copiedFile);
			}
		} catch (IOException e) {
			Logger.logError(e,
					"Failed to copy nonroad.exe to bundle temparory folder.");
			return;
		}

		// copy data files to bundle temp dir and add them to the list of files
		// in the bundle

		if (isTest) {
			this.tempDataFolder = new File(
					"C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\nonroad_test");
		}

		File[] dataFiles = this.tempDataFolder.listFiles();

		try {
			for (File file : dataFiles) {
				if (file.isFile()) {
					NonroadTemparyDataFileManager.copyFileToFolder(file,
							temporaryFolderPath);
					copiedFile = new File(temporaryFolderPath, file.getName());
					if (!copiedFile.exists()) {
						throw new IOException("File does not exist: "
								+ copiedFile);
					}
					tableDumpFilePaths.add(copiedFile);
				}
			}
		} catch (IOException e) {
			Logger.logError(e,
					"Failed to copy nonroad data files to bundle temparory folder.");
			return;
		}

		// Add the OPT file to the list of files in the bundle

		if (!isTest) {
			tableDumpFilePaths.add(optFilePath);
		}
	}

	/**
	 * Create a temporary directory and populate it with frequently
	 * used Nonroad data files.
	 * @param executionDB database to be used
	 * @return true if the directory and supporting data files exist, even if they
	 * already existed and were not created within this invocation.
	**/
	private boolean getDataFiles(Connection executionDB) {
		synchronized (mutex) {
			if(!didSubscribeToEngineEvents) {
				didSubscribeToEngineEvents = true;
				MOVESEngine.subscribeToProgress(this);
			}

			boolean isOK = true;

			if (executionDB == null) {
				Logger.log(LogMessageCategory.ERROR,
						"getDataFiles: executionDB is null.");
				isOK = false;
			}

			if (isOK) {
				if (calledTimes == 0 || tempDataFolder == null) {
					// generate data files only on first time
					Logger.log(LogMessageCategory.INFO,
							"Nonroad Emission Calculator called first time. Generate data files.");

					try {
						tempDataFolder = NonroadTemparyDataFileManager
								.createTemporaryDataFolder(null,
										"NonroadEmissionCalculatorDataTemp",
										false);

						if (tempDataFolder != null) {
							NonroadInputCreator.theNonroadInputCreator.reset(
									tempDataFolder.getCanonicalPath(),
									executionDB);
						} else {
							Logger.log(LogMessageCategory.INFO, "Nonroad Emission Calculator unable to create tempDataFolder.");
							isOK = false;
						}

						// export related tables into files
						String sql = null;
						PreparedStatement statement = null;
						for (String table : NonroadHelper.nonroadTableNeededAtWorkerSide) {
							sql = "select * from " + table + " into outfile "
									+ "\"" + tempDataFolder.getCanonicalPath()

									+ System.getProperty("file.separator")
									+ table + ".txt\"";
							sql = sql.replaceAll("\\\\", "\\\\\\\\");
							statement = executionDB.prepareStatement(sql);
							SQLRunner.executeQuery(statement, sql);
						}
						if (statement != null) {
							statement.close();
						}
					} catch (IOException e) {
						Logger.log(LogMessageCategory.ERROR,
								"IO exception when generating nonroad datafiles.");
						isOK = false;
						e.printStackTrace();
					} catch (SQLException e) {
						Logger.log(LogMessageCategory.ERROR,
								"SQL exception when exporting execution database tables.");
						isOK = false;
						e.printStackTrace();
					}
				} else {
					if (tempDataFolder == null) {
						Logger.log(LogMessageCategory.ERROR,
								"Nonroad data file folder does not exist.");
						isOK = false;
					}
				}
			}

			calledTimes++;

			return isOK;
		}
	}

	/**
	 * Used to send notifications about a MOVESEngines' progress to implementers.
	 * 
	 * @param inEngine The associated MOVESEngine.
	**/
	public void engineProgressUpdate(MOVESEngine inEngine) {
		// Nothing to do here
	}

	/**
	 * Called when the MOVESEngine object is completing
	 * 
	 * @param inEngine The associated MOVESEngine.
	**/
	public void engineIsCompleting(MOVESEngine inEngine) {
		// delete temporary datafiles
		if (!isTest) {
			synchronized (mutex) {
				if (tempDataFolder != null) {
					NonroadTemparyDataFileManager.deleteTemporaryFolder(tempDataFolder);
					tempDataFolder = null;
				}
			}
		}
	}

	/**
	 * Examine a context for suitability.  Used to override the natural execution hierarchy.
	 * @param context Context to be examined
	 * @return true if the context should be used by a MasterLoopable.
	**/
	public boolean doesProcessContext(MasterLoopContext context) {
		if(owningLoop.isCountingBundles() != isCountingBundles) {
			isCountingBundles = owningLoop.isCountingBundles();
			processedKeys.clear();
		}
		// Zone-Year-Month-Day
		if(context.iterLocation == null || context.iterLocation.zoneRecordID < 0
				|| context.year <= 0 || context.monthID <= 0 || context.dayID <= 0) {
			return false;
		}

		if(context.iterLocation != null && context.iterLocation.roadTypeRecordID != 100) {
			return false;
		}

		String key = "" + context.iterLocation.zoneRecordID
				+ "|" + context.year + "|" + context.monthID + "|" + context.dayID;
		if(processedKeys.contains(key)) {
			// doExecute was already called for the zone-year-month-day combination and
			// should not be run again.
			return false;
		}
		processedKeys.add(key);
		return true;
	}
}
