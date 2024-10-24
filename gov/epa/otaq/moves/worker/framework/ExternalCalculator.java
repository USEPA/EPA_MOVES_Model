/**************************************************************************************************
 * @(#)ExternalCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.ExecutionRunSpec;
import gov.epa.otaq.moves.master.framework.SystemConfiguration;
import gov.epa.otaq.moves.worker.gui.*;
import gov.epa.otaq.moves.utils.ApplicationRunner;
import gov.epa.otaq.moves.utils.FileUtil;

import java.io.*;
import java.sql.*;
import java.util.*;

/**
 * Interface to an external calculator program.
 *
 * @author		Wesley Faler
 * @version		2017-05-17
**/
public class ExternalCalculator {
	/** SQL pseudo command to use the external calculator **/
	static final String markerText = "externalcalculator";

	/** Database connection **/
	Connection database;
	/** folder to hold all intermediate files and results **/
	File workingFolderPath;
	/** Set of module names **/
	TreeSetIgnoreCase moduleNames = new TreeSetIgnoreCase();
	/** true when detailed debugging information should be logged by the external calculator **/
	boolean shouldDebug;
	/** Owner used for tracking timing **/
	RemoteEmissionsCalculator owner;
	/** true when the bundle manifest says that we are in the project domain **/
	boolean isProject;

	/**
	 * Constructor.
	 * @param ownerToUse owner used for tracking timing.
	 * @param databaseToUse database connection.
	 * @param workingFolderPathToUse folder to hold all intermediate files and results.
	**/
	public ExternalCalculator(RemoteEmissionsCalculator ownerToUse, Connection databaseToUse, 
                              File workingFolderPathToUse, boolean shouldDebugToUse, BundleManifest manifest) {
		owner = ownerToUse;
		database = databaseToUse;
		workingFolderPath = workingFolderPathToUse;
		shouldDebug = shouldDebugToUse;
        isProject = ModelDomain.getByName(manifest.masterFragment.domain) == ModelDomain.PROJECT;
	}

	/** Clear any accumulated context. **/
	public void reset() {
		moduleNames.clear();
	}

	/**
	 * Examine an SQL statement, running the external calculator if needed.
	 * @param sql SQL statement. When null, the calculator is run if there
	 * are accumulated modules. When an "externalcalculator" statement is given,
	 * its context is accumulated and the calculator is not run immediately.
	 * When any other non-blank statement, the calculator is run if there are
	 * accumulated modules.
	 * @return true if the SQL was an "externalcalculator" statement.
	**/
	public boolean absorbAndExecute(String sql) {
		if(sql == null) {
			if(moduleNames.size() > 0) {
				execute();
			}
			return false;
		}
		if(sql.startsWith(markerText)) {
			//Logger.log(LogMessageCategory.DEBUG,"Got external calculator command: " + sql);
			String moduleName = sql.substring(markerText.length()).trim();
			while(moduleName.endsWith(";")) {
				moduleName = moduleName.substring(0,moduleName.length()-1);
			}
			//Logger.log(LogMessageCategory.DEBUG,"Got external calculator module: " + moduleName);
			moduleNames.add(moduleName);
			return true;
		}
		if(sql.length() == 0 || sql.equals(";")) {
			return false;
		}
		if(moduleNames.size() > 0) {
			execute();
		}
		return false;
	}

	/**
	 * Run the external calculator, creating the required interface files and
	 * processing the output files.
	**/
	void execute() {
		//Logger.log(LogMessageCategory.DEBUG,"ExternalCalculator.execute. moduleNames.size()="+moduleNames.size());
		Writer extmodulesWriter = null;
		PrintWriter loadDetailsWriter = null;
		String sql = "";
		boolean splitByFuelSubTypeID = moduleNames.contains("FuelSubType");
		boolean hasActivityOutput = false;

		//Logger.log(LogMessageCategory.DEBUG,"ExternalCalculator.execute. splitByFuelSubTypeID="+splitByFuelSubTypeID);
		try {
			// Write the module names into extmodules file
			if(shouldDebug) {
				moduleNames.add("outputfulldetail");

				File detailsFile = new File(workingFolderPath,"newmovesworkeroutput_detail");
				if(detailsFile.exists()) {
					FileUtilities.deleteFileWithRetry(detailsFile);
				}
				String detailsPath = detailsFile.getCanonicalPath().replace('\\','/');
				
				loadDetailsWriter = new PrintWriter(new OutputStreamWriter(new FileOutputStream(new File(workingFolderPath,"loaddetails.sql"))));

				String[] statements = {
					"use movesworker;",

					"drop table if exists ExtMOVESWorkerOutputDetail;",
	
					"create table ExtMOVESWorkerOutputDetail like movesworkeroutput;",
					
					//"alter table ExtMOVESWorkerOutputDetail add fuelSubTypeID int null;",

					"alter table ExtMOVESWorkerOutputDetail add fuelFormulationID int null;",

					"create table ExtMOVESWorkerOutputDetailSum like ExtMOVESWorkerOutputDetail;",
	
					"load data infile " + DatabaseUtilities.escapeSQL(detailsPath)
						+ " into table ExtMOVESWorkerOutputDetail ("
						+ " 	MOVESRunID,iterationID,"
						+ " 	yearID,monthID,dayID,hourID,"
						+ " 	stateID,countyID,zoneID,linkID,"
						+ " 	pollutantID,processID,"
						+ " 	sourceTypeID,regClassID,"
						+ " 	fuelTypeID,modelYearID,"
						+ " 	roadTypeID,SCC,"
						+ " 	engTechID,sectorID,hpID,"
						+ " 	emissionQuant,emissionRate,"
						+ " 	fuelSubTypeID,fuelFormulationID);",
	
					"insert into ExtMOVESWorkerOutputDetailSum ("
							+ " 	MOVESRunID,iterationID,"
							+ " 	yearID,monthID,dayID,hourID,"
							+ " 	stateID,countyID,zoneID,linkID,"
							+ " 	pollutantID,processID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	engTechID,sectorID,hpID,"
							+ " 	emissionQuant,emissionRate,"
							+ " 	fuelSubTypeID,fuelFormulationID)"
							+ " select MOVESRunID,iterationID,"
							+ " 	yearID,monthID,dayID,hourID,"
							+ " 	stateID,countyID,zoneID,linkID,"
							+ " 	pollutantID,processID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	engTechID,sectorID,hpID,"
							+ " 	sum(emissionQuant) as emissionQuant, sum(emissionRate) as emissionRate,"
							+ "		fuelSubTypeID,fuelFormulationID"
							+ " from ExtMOVESWorkerOutputDetail"
							+ " group by yearID,monthID,dayID,hourID,"
							+ " 	stateID,countyID,zoneID,linkID,"
							+ " 	pollutantID,processID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	engTechID,sectorID,hpID,"
							+ " 	fuelSubTypeID,fuelFormulationID;",
				};
				for(int i=0;i<statements.length;i++) {
					sql = statements[i];
					if(sql == null) {
						continue;
					}
					loadDetailsWriter.println(sql);
				}

				loadDetailsWriter.close();
				loadDetailsWriter = null;
			}
			owner.startTimer("ExternalCalcWriteInput");
			extmodulesWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(workingFolderPath,"extmodules"))));
			for(String moduleName : moduleNames) {
				if (moduleName.equals("DistanceCalculator")){
					hasActivityOutput = true;
				}

				extmodulesWriter.write(moduleName);
				extmodulesWriter.write('\n');
			}
			extmodulesWriter.close();
			extmodulesWriter = null;
			moduleNames.clear();
			// Save MOVESWorkerOutput to disk
			File mwo = new File(workingFolderPath,"movesworkeroutput");
			if(mwo.exists()) {
				FileUtilities.deleteFileWithRetry(mwo);
			}
			String mwoPath = mwo.getCanonicalPath().replace('\\','/');
			sql = "select "
					+ " MOVESRunID,iterationID,"
					+ " yearID,monthID,dayID,hourID,"
					+ " stateID,countyID,zoneID,linkID,"
					+ " pollutantID,processID,"
					+ " sourceTypeID,regClassID,"
					+ " fuelTypeID,modelYearID,"
					+ " roadTypeID,SCC,"
					+ " engTechID,sectorID,hpID,"
					+ " emissionQuant,emissionRate"
					+ " into outfile " + DatabaseUtilities.escapeSQL(mwoPath)
					+ " from MOVESWorkerOutput";
			try {
				SQLRunner.executeSQL(database,sql);
			} catch(Exception e) {
				Logger.logError(e,"Unable to save MOVESWorkerOutput using: " + sql);
				return;
			}

			// Save MOVESWorkerActivityOutput to disk, only when it needs to be split by fuelsubtype.
			if(splitByFuelSubTypeID) {
				File mwoActivity = new File(workingFolderPath,"movesworkeractivityoutput");
				if(mwoActivity.exists()) {
					FileUtilities.deleteFileWithRetry(mwoActivity);
				}
				String mwoActivityPath = mwoActivity.getCanonicalPath().replace('\\','/');
				sql = "select "
						+ " MOVESRunID,iterationID,"
						+ " yearID,monthID,dayID,hourID,"
						+ " stateID,countyID,zoneID,linkID,"
						+ " sourceTypeID,regClassID,"
						+ " fuelTypeID,modelYearID,"
						+ " roadTypeID,SCC,"
						+ " engTechID,sectorID,hpID,"
						+ " activityTypeID,activity"
						+ " into outfile " + DatabaseUtilities.escapeSQL(mwoActivityPath)
						+ " from MOVESWorkerActivityOutput";
				try {
					SQLRunner.executeSQL(database,sql);
				} catch(Exception e) {
					Logger.logError(e,"Unable to save MOVESWorkerActivityOutput using: " + sql);
					return;
				}
			}

			sql = "";
			// Prepare to receive the new output
			File newMWO = new File(workingFolderPath,"newmovesworkeroutput");
			if(newMWO.exists()) {
				FileUtilities.deleteFileWithRetry(newMWO);
			}
			String newMWOPath = newMWO.getCanonicalPath().replace('\\','/');

			File newMWOActivity = new File(workingFolderPath,"newmovesworkeractivityoutput");
			if(newMWOActivity.exists()) {
				FileUtilities.deleteFileWithRetry(newMWOActivity);
			}
			String newMWOActivityPath = newMWOActivity.getCanonicalPath().replace('\\','/');
			
			//EM - this block added to fix the rates bug EMT-809 on 12/20/2018
			File newBRO = new File(workingFolderPath,"newbaserateoutput");
			if(newBRO.exists()) {
				FileUtilities.deleteFileWithRetry(newBRO);
			}
			String newBROPath = newBRO.getCanonicalPath().replace('\\','/');

			// Run the external calculator in the working directory that contains all table files
			long start = System.currentTimeMillis();
			long elapsedTimeMillis;
			double elapsedTimeSec;

			File targetApplicationPath = new File(WorkerConfiguration.theWorkerConfiguration.calculatorApplicationPath);
			String[] arguments = {"-isproject=" + (isProject?1:0)};
			boolean runInCmd = false;
			String[] environment = { "GOMAXPROCS", "4" };
			File targetFolderPath = workingFolderPath;
			File processOutputPath = new File(targetFolderPath, "ExternalCalculatorProcessOutput.txt");
			String inputText = null;
			try {
				owner.startTimer("ExternalCalcRun");
				ApplicationRunner.runApplication(targetApplicationPath, arguments,
						targetFolderPath, new FileOutputStream(processOutputPath),
						inputText, runInCmd, environment);
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
	
			elapsedTimeMillis = System.currentTimeMillis() - start;
			elapsedTimeSec = elapsedTimeMillis / 1000F;
			Logger.log(LogMessageCategory.INFO,
					"Time spent on running the external calculator (sec): " + elapsedTimeSec);

			// Complain if the calculator did not make a response file.
			if(!newMWO.exists()) {
				Logger.log(LogMessageCategory.ERROR,"No response from external calculator in file: " + newMWOPath);
				return;
			}
			// Process the calculator response
			owner.startTimer("ExternalCalcReadResults");
			String[] statements = {
				"drop table if exists ExtMOVESWorkerOutput",

				"create table ExtMOVESWorkerOutput like movesworkeroutput",

				"load data infile " + DatabaseUtilities.escapeSQL(newMWOPath)
					+ " into table ExtMOVESWorkerOutput ("
					+ " 	MOVESRunID,iterationID,"
					+ " 	yearID,monthID,dayID,hourID,"
					+ " 	stateID,countyID,zoneID,linkID,"
					+ " 	pollutantID,processID,"
					+ " 	sourceTypeID,regClassID,"
					+ " 	fuelTypeID,modelYearID,"
					+ " 	roadTypeID,SCC,"
					+ " 	engTechID,sectorID,hpID,"
					+ " 	emissionQuant,emissionRate"
					+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
					+ ")",

				(splitByFuelSubTypeID? "truncate MOVESWorkerOutput" : ""),
/*
				"insert into MOVESWorkerOutput ("
						+ " 	MOVESRunID,iterationID,"
						+ " 	yearID,monthID,dayID,hourID,"
						+ " 	stateID,countyID,zoneID,linkID,"
						+ " 	pollutantID,processID,"
						+ " 	sourceTypeID,regClassID,"
						+ " 	fuelTypeID,modelYearID,"
						+ " 	roadTypeID,SCC,"
						+ " 	engTechID,sectorID,hpID,"
						+ " 	emissionQuant,emissionRate"
						+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
						+ ")"
						+ " select MOVESRunID,iterationID,"
						+ " 	yearID,monthID,dayID,hourID,"
						+ " 	stateID,countyID,zoneID,linkID,"
						+ " 	pollutantID,processID,"
						+ " 	sourceTypeID,regClassID,"
						+ " 	fuelTypeID,modelYearID,"
						+ " 	roadTypeID,SCC,"
						+ " 	engTechID,sectorID,hpID,"
						+ " 	sum(emissionQuant) as emissionQuant, sum(emissionRate) as emissionRate"
						+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
						+ " from ExtMOVESWorkerOutput"
						+ " group by yearID,monthID,dayID,hourID,"
						+ " 	stateID,countyID,zoneID,linkID,"
						+ " 	pollutantID,processID,"
						+ " 	sourceTypeID,regClassID,"
						+ " 	fuelTypeID,modelYearID,"
						+ " 	roadTypeID,SCC,"
						+ " 	engTechID,sectorID,hpID"
						+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
						,
*/
				"insert into MOVESWorkerOutput ("
						+ " 	MOVESRunID,iterationID,"
						+ " 	yearID,monthID,dayID,hourID,"
						+ " 	stateID,countyID,zoneID,linkID,"
						+ " 	pollutantID,processID,"
						+ " 	sourceTypeID,regClassID,"
						+ " 	fuelTypeID,modelYearID,"
						+ " 	roadTypeID,SCC,"
						+ " 	engTechID,sectorID,hpID,"
						+ " 	emissionQuant,emissionRate"
						+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
						+ ")"
						+ " select MOVESRunID,iterationID,"
						+ " 	yearID,monthID,dayID,hourID,"
						+ " 	stateID,countyID,zoneID,linkID,"
						+ " 	pollutantID,processID,"
						+ " 	sourceTypeID,regClassID,"
						+ " 	fuelTypeID,modelYearID,"
						+ " 	roadTypeID,SCC,"
						+ " 	engTechID,sectorID,hpID,"
						+ " 	emissionQuant, emissionRate"
						+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
						+ " from ExtMOVESWorkerOutput"
						,

				"drop table if exists ExtMOVESWorkerOutput"
			};
			start = System.currentTimeMillis();
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				if(sql == null || sql.length() == 0) {
					continue;
				}
				SQLRunner.executeSQL(database,sql);
			}

			// Load activity data, when split by fuelsubtype
			if(hasActivityOutput || splitByFuelSubTypeID) {	
				String[] activityStatements = {
					"drop table if exists ExtMOVESWorkerActivityOutput",
	
					"create table ExtMOVESWorkerActivityOutput like movesworkeractivityoutput",
	
					"load data infile " + DatabaseUtilities.escapeSQL(newMWOActivityPath)
						+ " into table ExtMOVESWorkerActivityOutput ("
						+ " 	MOVESRunID,iterationID,"
						+ " 	yearID,monthID,dayID,hourID,"
						+ " 	stateID,countyID,zoneID,linkID,"
						+ " 	sourceTypeID,regClassID,"
						+ " 	fuelTypeID,modelYearID,"
						+ " 	roadTypeID,SCC,"
						+ " 	engTechID,sectorID,hpID,"
						+ " 	activityTypeID,activity"
						+ ((splitByFuelSubTypeID)? ",fuelSubTypeID" : "")
						+ ")",
	
					((splitByFuelSubTypeID)? "truncate MOVESWorkerActivityOutput" : ""),
/*	
					"insert into MOVESWorkerActivityOutput ("
							+ " 	MOVESRunID,iterationID,"
							+ " 	yearID,monthID,dayID,hourID,"
							+ " 	stateID,countyID,zoneID,linkID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	engTechID,sectorID,hpID,"
							+ " 	activityTypeID,activity"
							//+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
							+ ")"
							+ " select MOVESRunID,iterationID,"
							+ " 	yearID,monthID,dayID,hourID,"
							+ " 	stateID,countyID,zoneID,linkID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	engTechID,sectorID,hpID,"
							+ " 	activityTypeID, sum(activity) as activity"
							//+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
							+ " from ExtMOVESWorkerActivityOutput"
							+ " group by yearID,monthID,dayID,hourID,"
							+ " 	stateID,countyID,zoneID,linkID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	engTechID,sectorID,hpID,"
							+ " 	activityTypeID"
							//+ (splitByFuelSubTypeID? ",fuelSubTypeID" : "")
							,
*/

					"insert into MOVESWorkerActivityOutput ("
							+ " 	MOVESRunID,iterationID,"
							+ " 	yearID,monthID,dayID,hourID,"
							+ " 	stateID,countyID,zoneID,linkID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	engTechID,sectorID,hpID,"
							+ " 	activityTypeID,activity"
							+ ((splitByFuelSubTypeID)? ",fuelSubTypeID" : "")
							+ ")"
							+ " select MOVESRunID,iterationID,"
							+ " 	yearID,monthID,dayID,hourID,"
							+ " 	stateID,countyID,zoneID,linkID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	engTechID,sectorID,hpID,"
							+ " 	activityTypeID, activity"
							+ ((splitByFuelSubTypeID)? ",fuelSubTypeID" : "")
							+ " from ExtMOVESWorkerActivityOutput"
							,
					"drop table if exists ExtMOVESWorkerActivityOutput"
				};
				start = System.currentTimeMillis();
				for(int i=0;i<activityStatements.length;i++) {
					sql = activityStatements[i];
					if(sql == null || sql.length() == 0) {
						continue;
					}
					SQLRunner.executeSQL(database,sql);
				}
			}
			
			// When we include fuelSubTypeID, the Go calculator splits out the emissions and activity by fuelSubTypeID.
			// This works for everything except avgHP (activityTypeID 9) and LF (activityTypeID 12), which should not be split.
			// This chunk fixes avgHP and LF by summing the split components and resaving.
			if (splitByFuelSubTypeID) {				
				sql = "UPDATE MOVESWorkerActivityOutput mwoActivity, ( " + 
					  "SELECT MOVESRunID, iterationID, yearID , monthID,dayID,hourID,stateID,countyID,zoneID ,linkID,sourceTypeID,regClassID,fuelTypeID, " +
					  "modelYearID,roadTypeID,SCC,engTechID,sectorID,hpID,activityTypeID,sum(activity) as totalActivity " +
					  "FROM MOVESWorkerActivityOutput " +
					  "WHERE fuelTypeID = 1 and activityTypeID in (9, 12) " +
					  "group by MOVESRunID,iterationID,yearID ,monthID,dayID,hourID,stateID,countyID,zoneID ,linkID,sourceTypeID,regClassID,fuelTypeID, " +
					  "         modelYearID,roadTypeID,SCC,engTechID,sectorID,hpID,activityTypeID " +
					  ") as tActivity " +
					  "SET activity = totalActivity " +
					  "WHERE mwoActivity.MOVESRunID = tActivity.MOVESRunID and" +
					  "      mwoActivity.iterationID = tActivity.iterationID and" +
					  "      mwoActivity.yearID = tActivity.yearID and" +
					  "      mwoActivity.monthID = tActivity.monthID and" +
					  "      mwoActivity.dayID = tActivity.dayID and" +
					  "      mwoActivity.hourID = tActivity.hourID and" +
					  "      mwoActivity.stateID = tActivity.stateID and" +
					  "      mwoActivity.countyID = tActivity.countyID and" +
					  "      mwoActivity.zoneID = tActivity.zoneID and" +
					  "      mwoActivity.linkID = tActivity.linkID and" +
					  "      mwoActivity.sourceTypeID = tActivity.sourceTypeID and" +
					  "      mwoActivity.regClassID = tActivity.regClassID and" +
					  "      mwoActivity.fuelTypeID = tActivity.fuelTypeID and" +
					  "      mwoActivity.modelYearID = tActivity.modelYearID and" +
					  "      mwoActivity.roadTypeID = tActivity.roadTypeID and" +
					  "      mwoActivity.SCC = tActivity.SCC and" +
					  "      mwoActivity.engTechID = tActivity.engTechID and" +
					  "      mwoActivity.sectorID = tActivity.sectorID and" +
					  "      mwoActivity.hpID = tActivity.hpID and" +
					  "      mwoActivity.activityTypeID = tActivity.activityTypeID";
				SQLRunner.executeSQL(database,sql);
			}
	  
			
			//EM - entire if block added to fix rates bug EMT-809 12/20/2018
			if(newBRO.exists()) {
				String[] broStatements = {
					"drop table if exists ExtBaseRateOutput",

					"create table ExtBaseRateOutput like baserateoutput",

					"load data infile " + DatabaseUtilities.escapeSQL(newBROPath)
						+ " into table ExtBaseRateOutput ("
						+ " 	MOVESRunID,iterationID,"
						+ " 	yearID,monthID,hourDayID,"
						+ " 	zoneID,linkID,"
						+ " 	pollutantID,processID,"
						+ " 	sourceTypeID,regClassID,"
						+ " 	fuelTypeID,modelYearID,"
						+ " 	roadTypeID,SCC,"
						+ " 	avgSpeedBinID,"
						+ " 	meanBaseRate,emissionRate"
						+ ")",

					"insert into baserateoutput ("
							+ " 	MOVESRunID,iterationID,"
							+ " 	yearID,monthID,hourDayID,"
							+ " 	zoneID,linkID,"
							+ " 	pollutantID,processID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	avgSpeedBinID,"
							+ " 	meanBaseRate,emissionRate"
							+ ")"
							+ " select MOVESRunID,iterationID,"
							+ " 	yearID,monthID,hourDayID,"
							+ " 	zoneID,linkID,"
							+ " 	pollutantID,processID,"
							+ " 	sourceTypeID,regClassID,"
							+ " 	fuelTypeID,modelYearID,"
							+ " 	roadTypeID,SCC,"
							+ " 	avgSpeedBinID,"
							+ " 	meanBaseRate,emissionRate"
							+ " from ExtBaseRateOutput"
							,

					"drop table if exists ExtBaseRateOutput"
				};
				start = System.currentTimeMillis();
				for(int i=0;i<broStatements.length;i++) {
					sql = broStatements[i];
					if(sql == null || sql.length() == 0) {
						continue;
					}
					SQLRunner.executeSQL(database,sql);
				}
			}

			elapsedTimeMillis = System.currentTimeMillis() - start;
			elapsedTimeSec = elapsedTimeMillis / 1000F;
			Logger.log(LogMessageCategory.INFO,
					"Time spent on absorbing external calculator results (sec): " + elapsedTimeSec);
			owner.startUnassignedTimer();
		} catch(Exception e) {
			Logger.logError(e,"Unable to run the external calculator.");
		} finally {
			if(extmodulesWriter != null) {
				try {
					extmodulesWriter.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				extmodulesWriter = null;
			}
			if(loadDetailsWriter != null) {
				try {
					loadDetailsWriter.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				loadDetailsWriter = null;
			}
			reset();
		}
	}
}
