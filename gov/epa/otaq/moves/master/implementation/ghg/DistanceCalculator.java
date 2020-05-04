/**************************************************************************************************
 * @(#)DistanceCalculator.java
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
 * Calculates distance output. This calculator is instantiated when a
 * distance output is requested by the RunSpec. It signs up for the
 * Running Exhaust emission process at the Year level. The distance is
 * is calculated as the product of Source Hours Operating distance
 * and the sum of all source bins in fuel type ID.
 *
 * @author		Wesley Faler
 * @version		2014-06-10
**/
public class DistanceCalculator extends EmissionCalculator
		implements MasterLoopContext.IContextFilter {
	/**
	 * @algorithm
	 * @owner Distance Calculator
	**/

	/** true if the "Running Exhaust" process should be considered **/
	boolean shouldCalcRunningExhaust = false;

	/**
	 * Constructor, including registration of distance handled by this calculator.
	 * Such registration facilitates calculator
	 * chaining.
	**/
	public DistanceCalculator() {
		// This calculator doesn't determine pollutants in any way, so it
		// does not register itself.
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
	 * Examine a context for suitability.  Used to override the natural execution hierarchy.
	 * @param context Context to be examined
	 * @return true if the context should be used by a MasterLoopable.
	**/
	public boolean doesProcessContext(MasterLoopContext context) {
		if(context.iterLocation != null
				&& context.iterLocation.roadTypeRecordID == 1) {
			return false;
		}
		return true;
	}

	/**
	 * Builds SQL statements for a distributed worker to execute. This is called by
	 * DistanceCalculator.executeLoop. Implementations of this method
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
			Logger.logError(e,"Unable to get a Pollutant/Process needed for running"
					+ " Distance Calculations.");
		} finally {
			query.onFinally();
		}

		if(pollutantProcessID.length()>0) {
			replacements.put("##pollutantProcessIDs##",pollutantProcessID);
		} else {
			Logger.log(LogMessageCategory.ERROR,"Distance calculation requires Running Exhaust.");
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

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/DistanceCalculator.sql",enabledSectionNames,
				sqlForWorker);
		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
