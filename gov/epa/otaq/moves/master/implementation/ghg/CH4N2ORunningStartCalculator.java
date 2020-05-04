/**************************************************************************************************
 * @(#)CH4N2ORunningStartCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.RunSpec;

import java.util.*;
import java.sql.*;

/**
 * Does processing for the CH4 and N2O pollutants, within the engine start
 * and engine running processes.
 *
 * @author		Wesley Faler
 * @version		2011-06-21
**/
public class CH4N2ORunningStartCalculator extends EmissionCalculator
		implements MasterLoopContext.IContextFilter {
	/** @notused **/
	boolean shouldCalcStartExhaust = false;
	boolean shouldCalcRunningExhaust = false;

	/**
	 * constructor
	**/
	public CH4N2ORunningStartCalculator() {
		//Pollutant methane = Pollutant.findByID(5);
		Pollutant nitrousOxide = Pollutant.findByID(6);

		EmissionProcess runningExhaust = EmissionProcess.findByID(1);
		EmissionProcess startExhaust = EmissionProcess.findByID(2);

		//EmissionCalculatorRegistration.register(methane,startExhaust,this);
		EmissionCalculatorRegistration.register(nitrousOxide,startExhaust,this);

		//EmissionCalculatorRegistration.register(methane,runningExhaust,this);
		EmissionCalculatorRegistration.register(nitrousOxide,runningExhaust,this);
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// First, without considering interactions, just determine what is in the user's
		// runspec.
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Start Exhaust")) {
			shouldCalcStartExhaust = true;
		}

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Running Exhaust")) {
			shouldCalcRunningExhaust = true;
		}

		// Now sign up for the minimal set of processes
		if(shouldCalcStartExhaust) {
			EmissionProcess process = EmissionProcess.findByName("Start Exhaust");
			targetLoop.subscribe(this, process, MasterLoopGranularity.MONTH,
					MasterLoopPriority.EMISSION_CALCULATOR);
		}

		if(shouldCalcRunningExhaust) {
			EmissionProcess process = EmissionProcess.findByName("Running Exhaust");
			targetLoop.subscribe(this, process, MasterLoopGranularity.MONTH,
					MasterLoopPriority.EMISSION_CALCULATOR);
		}
	}

	/**
	 * Examine a context for suitability.  Used to override the natural execution hierarchy.
	 * @param context Context to be examined
	 * @return true if the context should be used by a MasterLoopable.
	**/
	public boolean doesProcessContext(MasterLoopContext context) {
		if(context.iterProcess != null && context.iterProcess.databaseKey == 2) {
			if(context.iterLocation != null
					&& context.iterLocation.roadTypeRecordID > 0
					&& context.iterLocation.roadTypeRecordID != 1) {
				return false;
			}
		}
		return true;
	}

	/**
	 * This is an EmissionCalculator override that builds SQL statements for a distributed
	 * worker to execute. This is called by EmissionCalculator.executeLoop.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	**/
	public SQLForWorker doExecute(MasterLoopContext context) {
		// Determine which pollutant(s) should be calculated
		shouldCalcStartExhaust = false;
		shouldCalcRunningExhaust = false;

		boolean foundProcess = false;
		if(context.iterProcess.processName.equalsIgnoreCase("Start Exhaust")) {
			// For zone emissions, only work with the off-network road type.
			if(context.iterLocation.roadTypeRecordID != 1) {
				return null;
			}
			shouldCalcStartExhaust = true;
			foundProcess = true;
		}

		if(context.iterProcess.processName.equalsIgnoreCase("Running Exhaust")) {
			shouldCalcRunningExhaust = true;
			foundProcess = true;
		}

		if(!foundProcess) {
			return null;
		}

		boolean foundPollutant = false;
		//boolean shouldCalcCH4 = false;
		boolean shouldCalcN2O = false;

		/*
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Methane (CH4)",context.iterProcess.processName)) {
			shouldCalcCH4 = true;
			foundPollutant = true;
		}
		*/
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Nitrous Oxide (N2O)",context.iterProcess.processName)) {
			shouldCalcN2O = true;
			foundPollutant = true;
		}

		if(!foundPollutant) {
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		String pollutantProcessIDs = "";

		Connection executionDatabase = null;
		try {
			executionDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION);
		} catch(Exception e) {
			Logger.logError(e,"Unable to get the Execution Database connection needed for running"
					+ " CH4 and N2O emissions.");
			return null;
		}

		/*
		if(shouldCalcCH4) {
			PollutantProcessAssociation a = PollutantProcessAssociation.findByName(
					"Methane (CH4)",context.iterProcess.processName);
			if(a != null) {
				if(pollutantProcessIDs.length() > 0) {
					pollutantProcessIDs += ",";
				}
				pollutantProcessIDs += a.getDatabaseKey(executionDatabase);
			}
		}
		*/

		if(shouldCalcN2O) {
			PollutantProcessAssociation a = PollutantProcessAssociation.findByName(
					"Nitrous Oxide (N2O)",context.iterProcess.processName);
			if(a != null) {
				if(pollutantProcessIDs.length() > 0) {
					pollutantProcessIDs += ",";
				}
				pollutantProcessIDs += a.getDatabaseKey(executionDatabase);
			}
		}

		replacements.put("##pollutantProcessIDs##",pollutantProcessIDs);

		if(executionDatabase != null) {
			DatabaseConnectionManager.checkInConnection(
				MOVESDatabaseType.EXECUTION, executionDatabase);
			executionDatabase = null;
		}

		if(shouldCalcStartExhaust) {
			enabledSectionNames.add("Start Exhaust");
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

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/CH4N2ORunningStartCalculator.sql",enabledSectionNames,
				sqlForWorker);
		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
