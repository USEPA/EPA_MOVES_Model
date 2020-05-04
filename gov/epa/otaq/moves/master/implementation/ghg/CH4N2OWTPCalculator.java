/**************************************************************************************************
 * @(#)CH4N2OWTPCalculator.java
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
 * Does processing for the CH4, N2O pollutants
 *
 * @author		Wesley Faler
 * @version		2009-04-04
**/
public class CH4N2OWTPCalculator extends EmissionCalculator {
	/** @notused **/
	boolean shouldCalcWTPExhaust = false;

	/**
	 * constructor
	**/
	public CH4N2OWTPCalculator() {
		Pollutant methane = Pollutant.findByID(5);
		Pollutant nitrousOxide = Pollutant.findByID(6);
		EmissionProcess wellToPump = EmissionProcess.findByID(99);
		EmissionCalculatorRegistration.register(methane,wellToPump,this);
		EmissionCalculatorRegistration.register(nitrousOxide,wellToPump,this);
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		Pollutant totalEnergy = Pollutant.findByID(91);
		EmissionProcess wellToPump = EmissionProcess.findByID(99);

		TreeSet<EmissionCalculator> calculators = new TreeSet<EmissionCalculator>();
		LinkedList<EmissionCalculator> t = 
				EmissionCalculatorRegistration.findNotInProcess(totalEnergy,wellToPump);
		calculators.addAll(t);

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				c.chainCalculator(this);
			}
		}
	}

	/**
	 * Builds SQL statements for a distributed worker to execute. This is called by
	 * EnergyConsumptionCalculator calculator.executeLoop. Implementations of this method
	 * should contain uncertainty logic when UncertaintyParameters specifies that
	 * this mode is enabled.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	**/
	public SQLForWorker doExecute(MasterLoopContext context) {
		// First, make sure the current process calculates Total Energy or this
		// calculator cannot calculate Methane or Nitrous Oxide
		Pollutant totalEnergy = Pollutant.findByID(91);

		if(!ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				totalEnergy.pollutantName,context.iterProcess.processName)) {
			return null;
		}

		// Determine which pollutant(s) should be calculated
		EmissionProcess wellToPump = EmissionProcess.findByID(99);
		boolean shouldCalcMethane = false;
		boolean shouldCalcNitrousOxide = false;
		boolean foundPollutant = false;

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Methane (CH4)",wellToPump.processName)) {
			shouldCalcMethane = true;
			foundPollutant = true;
		}

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Nitrous Oxide (N2O)",wellToPump.processName)) {
			shouldCalcNitrousOxide = true;
			foundPollutant = true;
		}

		if(!foundPollutant) {
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		String pollutantIDs = "";

		if(shouldCalcMethane) {
			if(pollutantIDs.length() > 0) {
				pollutantIDs += ",";
			}
			pollutantIDs += "5";
		}
		if(shouldCalcNitrousOxide) {
			if(pollutantIDs.length() > 0) {
				pollutantIDs += ",";
			}
			pollutantIDs += "6";
		}

		replacements.put("##pollutantIDs##",pollutantIDs);

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/CH4N2OWTPCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
