/**************************************************************************************************
 * @(#)AirToxicsDistanceCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

import java.util.*;
import java.sql.*;

/**
 * Perform Air Toxics calculations that are ratioed to distance
 * @author		Wesley Faler
 * @version		2013-09-30
**/
public class AirToxicsDistanceCalculator extends EmissionCalculator {
	/** Summary class holding details of the ATRatio* tables **/
	class ATRatioEntry {
		String tableName;
		int outputPolProcessID;
		PollutantProcessAssociation output;

		ATRatioEntry(String tableNameToUse, int outputPolProcessIDToUse) {
			tableName = tableNameToUse;
			outputPolProcessID = outputPolProcessIDToUse;
			output = PollutantProcessAssociation.createByID(outputPolProcessID);
		}
	}

	/** List of ATRatioEntry objects **/
	ArrayList<ATRatioEntry> entries = new ArrayList<ATRatioEntry>();
	/** True if loadRatioEntries() has been called **/
	boolean didLoadRatioEntries = false;
	/** true if the "Running Exhaust" process should be considered **/
	boolean shouldCalcRunningExhaust = false;

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public AirToxicsDistanceCalculator() {
		loadPlaceholderRatioEntries();

		for(Iterator i=entries.iterator();i.hasNext();) {
			ATRatioEntry a = (ATRatioEntry)i.next();
			EmissionCalculatorRegistration.register(a.output.pollutant,a.output.emissionProcess,this);
		}
	}

	/**
	 * Fill tne entries list with placeholder values for all possible pollutants and processes
	 * that this calculator could handle.
	**/
	private void loadPlaceholderRatioEntries() {
		entries.clear();

		int[] possibleValues = {
			6001,6101,6201,6301,6401,6501,6601,6701,
			13001,13101,13201,13301,13401,13501,13601,13701,
			13801,13901,14001,14101,14201,14301,14401,14501,
			14601
		};
		TreeSet<Integer> values = new TreeSet<Integer>();
		for(int i=0;i<possibleValues.length;i++) {
			Integer v = new Integer(possibleValues[i]);
			values.add(v);
		}

		for(Iterator<Integer> i=values.iterator();i.hasNext();) {
			Integer v = i.next();
			ATRatioEntry a = new ATRatioEntry("placeholder",v.intValue());
			if(a.output != null) {
				entries.add(a);
			}
		}
	}

	/** Fill the entries list from the ATRatio* tables **/
	private void loadRatioEntries() {
		entries.clear();

		String sql = "";
		String[] tableNames = {
			"dioxinEmissionRate",
			"metalEmissionRate"
		};

		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();

		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			for(int i=0;i<tableNames.length;i++) {
				sql = "select distinct polProcessID from " + tableNames[i];
				query.open(db,sql);
				while(query.rs.next()) {
					ATRatioEntry a = new ATRatioEntry(tableNames[i],query.rs.getInt(1));
					if(a.output != null) {
						entries.add(a);
					}
				}
				query.rs.close();
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to load ATRatio entries",sql);
		} finally {
			query.onFinally();
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// First, without considering interactions, just determine what is in the user's
		// runspec.
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Running Exhaust")) {
			shouldCalcRunningExhaust = true;
		}

		// Now sign up for the minimal set of processes
		if(shouldCalcRunningExhaust) {
			EmissionProcess process = EmissionProcess.findByName("Running Exhaust");
			targetLoop.subscribe(this, process, MasterLoopGranularity.YEAR,
					MasterLoopPriority.EMISSION_CALCULATOR);
		}
	}

	/**
	 * Builds SQL statements for a distributed worker to execute. This is called by
	 * EnergyConsumptionCalculator.executeLoop. Implementations of this method
	 * should contain uncertainty logic when UncertaintyParameters specifies that
	 * this mode is enabled.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	**/
	public SQLForWorker doExecute(MasterLoopContext context) {
		// Determine which pollutant(s) should be calculated
		shouldCalcRunningExhaust = false;

		if(context.iterProcess.processName.equalsIgnoreCase("Running Exhaust")) {
			shouldCalcRunningExhaust = true;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		Connection executionDatabase = null;
		try {
			executionDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION);
		} catch(Exception e) {
			Logger.logError(e,"Unable to get the Execution Database connection needed for running"
					+ " Distance Calculations.");
			return null;
		}

		// Get the first Pollutant/Process for the Running Exhaust process that is in the
		// SourceBinDistribution and runspec.
		String sql = "SELECT sbd.polProcessID FROM SourceBinDistribution sbd"
				+ " INNER JOIN PollutantProcessAssoc ppa ON ppa.polProcessID = sbd.polProcessID"
				+ " INNER JOIN RunspecPollutantProcess rpp ON rpp.polProcessID = sbd.polProcessID"
				+ " WHERE ppa.processID = 1 LIMIT 1";

		String pollutantProcessID = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(executionDatabase,sql);
			if(query.rs.next()) {
				pollutantProcessID = query.rs.getString(1);
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to get a Pollutant/Process needed for running AirToxicsDistance Calculations.");
		} finally {
			query.onFinally();
		}

		if(pollutantProcessID.length()>0) {
			replacements.put("##pollutantProcessIDs##",pollutantProcessID);
		} else {
			Logger.log(LogMessageCategory.ERROR,"AirToxicsDistance calculation requires Running Exhaust.");
			return null;
		}

		if(executionDatabase != null) {
			DatabaseConnectionManager.checkInConnection(
				MOVESDatabaseType.EXECUTION, executionDatabase);
			executionDatabase = null;
		}

		if(shouldCalcRunningExhaust) {
			enabledSectionNames.add("Running Exhaust");
		}

		OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
				ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection();
		if(outputEmissionsBreakdownSelection.onRoadSCC) {
			enabledSectionNames.add("SCCOutput");
		} else {
			enabledSectionNames.add("NoSCCOutput");
		}

		if(!didLoadRatioEntries) {
			didLoadRatioEntries = true;
			entries.clear();
			loadRatioEntries();
		}

		// Determine which pollutant(s) should be calculated
		String outputPolProcessIDs = "";
		boolean foundPollutant = false;
		TreeSet<String> allOutputPolProcessIDs = new TreeSet<String>();

		boolean foundDioxinEmissionRate = false;
		boolean foundMetalEmissionRate = false;
		String outputDioxinEmissionRate = "";
		String outputMetalEmissionRate = "";

		for(Iterator<ATRatioEntry> i=entries.iterator();i.hasNext();) {
			ATRatioEntry a = (ATRatioEntry)i.next();

			if(!context.hasProcess(a.output.emissionProcess)) {
				continue;
			}
			if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					a.output.pollutant.pollutantName,a.output.emissionProcess.processName)) {
				foundPollutant = true;
				String t = "" + a.outputPolProcessID;
				if(!allOutputPolProcessIDs.contains(t)) {
					allOutputPolProcessIDs.add(t);
					if(outputPolProcessIDs.length() > 0) {
						outputPolProcessIDs += ",";
					}
					outputPolProcessIDs += "" + a.outputPolProcessID;
				}
				if(a.tableName.equalsIgnoreCase("dioxinEmissionRate")) {
					if(!foundDioxinEmissionRate) {
						foundDioxinEmissionRate = true;
						enabledSectionNames.add("UseDioxinEmissionRate");
					}
					if(outputDioxinEmissionRate.length()>0) {
						outputDioxinEmissionRate += ",";
					}
					outputDioxinEmissionRate += "" + a.outputPolProcessID;
				}
				if(a.tableName.equalsIgnoreCase("metalEmissionRate")) {
					if(!foundMetalEmissionRate) {
						foundMetalEmissionRate = true;
						enabledSectionNames.add("UseMetalEmissionRate");
					}
					if(outputMetalEmissionRate.length()>0) {
						outputMetalEmissionRate += ",";
					}
					outputMetalEmissionRate += "" + a.outputPolProcessID;
				}
			}
		}

		if(!foundPollutant) {
			//System.out.println("AirToxicsDistanceCalculator found nothing to run");
			return null;
		}

		replacements.put("##ATOutputPolProcessIDs##",outputPolProcessIDs);

		replacements.put("##outputDioxinEmissionRate##",outputDioxinEmissionRate);
		replacements.put("##outputMetalEmissionRate##",outputMetalEmissionRate);

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/AirToxicsDistanceCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
