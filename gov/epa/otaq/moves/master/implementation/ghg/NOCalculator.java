/**************************************************************************************************
 * @(#)NOCalculator.java
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
 * Calculate gaseous NO Emissions for running, start, and extended idle processes
 * if called for in the ExecutionRunSpec.
 * This is done as a chained calculator to the  NOx calculator.
 *
 * @author		Wesley Faler
 * @author		EPA Ed Glover
 * @version		2011-12-12
**/
public class NOCalculator extends EmissionCalculator {
	/**
	 * @algorithm
	 * @owner NO Calculator
	 * @calculator
	**/

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.   Pollutant 32 - Nitrogen Oxide (NO) is registered.
	**/
	public NOCalculator() {
		Pollutant nO = Pollutant.findByID(32);
		Pollutant hono = Pollutant.findByID(34);

		EmissionProcess runningExhaust = EmissionProcess.findByID(1);
		EmissionProcess startExhaust = EmissionProcess.findByID(2);
		EmissionProcess extendedIdleExhaust = EmissionProcess.findByID(90);
		EmissionProcess auxiliaryPowerExhaust = EmissionProcess.findByID(91);

		EmissionCalculatorRegistration.register(nO,runningExhaust,this);
		EmissionCalculatorRegistration.register(nO,startExhaust,this);
		EmissionCalculatorRegistration.register(nO,extendedIdleExhaust,this);
		EmissionCalculatorRegistration.register(nO,auxiliaryPowerExhaust,this);

		EmissionCalculatorRegistration.register(hono,runningExhaust,this);
		EmissionCalculatorRegistration.register(hono,startExhaust,this);
		EmissionCalculatorRegistration.register(hono,extendedIdleExhaust,this);
		EmissionCalculatorRegistration.register(hono,auxiliaryPowerExhaust,this);
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.

		EmissionProcess runningExhaust = EmissionProcess.findByID(1);
 		EmissionProcess startExhaust = EmissionProcess.findByID(2);
 		EmissionProcess extendedIdleExhaust = EmissionProcess.findByID(90);
		EmissionProcess auxiliaryPowerExhaust = EmissionProcess.findByID(91);

		Pollutant oxidesOfNitrogen = Pollutant.findByID(3);

		TreeSet<EmissionCalculator> calculators = new TreeSet<EmissionCalculator>();

		LinkedList<EmissionCalculator> t = EmissionCalculatorRegistration.find(oxidesOfNitrogen,runningExhaust);
 		calculators.addAll(t);
 		t = EmissionCalculatorRegistration.find(oxidesOfNitrogen,startExhaust);
 		calculators.addAll(t);
 		t = EmissionCalculatorRegistration.find(oxidesOfNitrogen,extendedIdleExhaust);
 		calculators.addAll(t);
 		t = EmissionCalculatorRegistration.find(oxidesOfNitrogen,auxiliaryPowerExhaust);
 		calculators.addAll(t);

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				c.chainCalculator(this);
				Logger.log(LogMessageCategory.INFO,"NOCalculator chained to " + c);
			}
		}
	}

	/**
	 * Builds SQL statements for a distributed worker to execute. This is called by
	 * Oxides of Nitrogen.executeLoop. Implementations of this method
	 * should contain uncertainty logic when UncertaintyParameters specifies that
	 * this mode is enabled.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	**/
	public SQLForWorker doExecute(MasterLoopContext context) {
		boolean foundPollutant = false;
		String pollutantIDs = "";
		String pollutantProcessIDs = "";

		Connection executionDatabase = null;
		try {
			executionDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION);
		} catch(Exception e) {
			Logger.logError(e,"Unable to get the Execution Database connection needed for running"
					+ " NOCalculator.");
			return null;
		}

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Nitrogen Oxide (NO)",context.iterProcess.processName)) {
			if(pollutantIDs.length() > 0) {
				pollutantIDs += ",";
			}
			pollutantIDs += "32";

			PollutantProcessAssociation a = PollutantProcessAssociation.findByName(
					"Nitrogen Oxide (NO)",context.iterProcess.processName);
			if(a != null) {
				if(pollutantProcessIDs.length() > 0) {
					pollutantProcessIDs += ",";
				}
				int polProcessID = a.getDatabaseKey(executionDatabase);
				pollutantProcessIDs += polProcessID;
			}

			foundPollutant = true;
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Nitrous Acid (HONO)",context.iterProcess.processName)) {
			if(pollutantIDs.length() > 0) {
				pollutantIDs += ",";
			}
			pollutantIDs += "34";

			PollutantProcessAssociation a = PollutantProcessAssociation.findByName(
					"Nitrous Acid (HONO)",context.iterProcess.processName);
			if(a != null) {
				if(pollutantProcessIDs.length() > 0) {
					pollutantProcessIDs += ",";
				}
				int polProcessID = a.getDatabaseKey(executionDatabase);
				pollutantProcessIDs += polProcessID;
			}

			foundPollutant = true;
		}

		if(executionDatabase != null) {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, executionDatabase);
			executionDatabase = null;
		}

		if(!foundPollutant) {
			Logger.log(LogMessageCategory.INFO,"NO Calculator's SQLForWorker method does NOT find oxides of nitrogen");
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		replacements.put("##pollutantIDs##",pollutantIDs);
		replacements.put("##pollutantProcessIDs##",pollutantProcessIDs);

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/NOCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			//Logger.log(LogMessageCategory.INFO,"NO Calculator doExecute method IS successful");
			return sqlForWorker;
		} else {
			Logger.log(LogMessageCategory.INFO,"NO Calculator doExecute method NOT successful");
			return null;
		}
	}
}
