/**************************************************************************************************
 * @(#)ExternalCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker.framework;

import gov.epa.otaq.moves.common.*;
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
 * @version		2015-03-22
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

	/**
	 * Constructor.
	 * @param databaseToUse database connection.
	 * @param workingFolderPathToUse folder to hold all intermediate files and results.
	**/
	public ExternalCalculator(Connection databaseToUse, File workingFolderPathToUse, boolean shouldDebugToUse) {
		database = databaseToUse;
		workingFolderPath = workingFolderPathToUse;
		shouldDebug = shouldDebugToUse;
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
			extmodulesWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(workingFolderPath,"extmodules"))));
			for(String moduleName : moduleNames) {
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
			sql = "";
			// Prepare to receive the new output
			File newMWO = new File(workingFolderPath,"newmovesworkeroutput");
			if(newMWO.exists()) {
				FileUtilities.deleteFileWithRetry(newMWO);
			}
			String newMWOPath = newMWO.getCanonicalPath().replace('\\','/');
			// Run the external calculator in the working directory that contains all table files
			long start = System.currentTimeMillis();
			long elapsedTimeMillis;
			double elapsedTimeSec;

			File targetApplicationPath = new File(WorkerConfiguration.theWorkerConfiguration.calculatorApplicationPath);
			String[] arguments = new String[0];
			boolean runInCmd = false;
			String[] environment = { "GOMAXPROCS", "4" };
			File targetFolderPath = workingFolderPath;
			File processOutputPath = new File(targetFolderPath, "ExternalCalculatorProcessOutput.txt");
			String inputText = null;
			try {
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
			elapsedTimeMillis = System.currentTimeMillis() - start;
			elapsedTimeSec = elapsedTimeMillis / 1000F;
			Logger.log(LogMessageCategory.INFO,
					"Time spent on absorbing external calculator results (sec): " + elapsedTimeSec);
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
