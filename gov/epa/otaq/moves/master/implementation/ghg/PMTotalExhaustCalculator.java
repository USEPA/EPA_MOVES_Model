/**************************************************************************************************
 * @(#)PMTotalExhaustCalculator.java
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
 * Perform PM Total calculations
 * @author		Wesley Faler
 * @version		2012-03-11
**/
public class PMTotalExhaustCalculator extends EmissionCalculator {
	/** @notused **/
	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public PMTotalExhaustCalculator() {
		Pollutant PM10Total = Pollutant.findByID(100);
		Pollutant PM25Total = Pollutant.findByID(110);

		ExecutionRunSpec.pollutantNeedsAggregation(PM10Total);
		ExecutionRunSpec.pollutantNeedsAggregation(PM25Total);

		Object[] allProcesses = EmissionProcess.getAllEmissionProcesses();
		for(int i=0;i<allProcesses.length;i++) {
			EmissionProcess process = (EmissionProcess)allProcesses[i];
			EmissionCalculatorRegistration.register(PM10Total,process,this);
			EmissionCalculatorRegistration.register(PM25Total,process,this);
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		Pollutant ocPM25 = Pollutant.findByID(111);
		Pollutant ecPM25 = Pollutant.findByID(112);
		Pollutant sPM25 = Pollutant.findByID(115);
		Pollutant ocPM10 = Pollutant.findByID(101);
		Pollutant ecPM10 = Pollutant.findByID(102);
		Pollutant sPM10 = Pollutant.findByID(105);

		LinkedList<EmissionCalculator> calculators = new LinkedList<EmissionCalculator>();
		LinkedList<EmissionCalculator> t = EmissionCalculatorRegistration.findPollutant(ocPM25);
		EmissionCalculatorRegistration.merge(calculators,t);
		t = EmissionCalculatorRegistration.findPollutant(ecPM25);
		EmissionCalculatorRegistration.merge(calculators,t);
		t = EmissionCalculatorRegistration.findPollutant(sPM25);
		EmissionCalculatorRegistration.merge(calculators,t);
		t = EmissionCalculatorRegistration.findPollutant(ocPM10);
		EmissionCalculatorRegistration.merge(calculators,t);
		t = EmissionCalculatorRegistration.findPollutant(ecPM10);
		EmissionCalculatorRegistration.merge(calculators,t);
		t = EmissionCalculatorRegistration.findPollutant(sPM10);
		EmissionCalculatorRegistration.merge(calculators,t);

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				//System.out.println("PMTotalCalc chained to " + c.getClass().getName());
				c.chainCalculator(this);
			}
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
		Pollutant PM10Total = Pollutant.findByID(100);
		Pollutant PM25Total = Pollutant.findByID(110);

		boolean shouldCalcPM10Total = false;
		boolean shouldCalcPM25Total = false;
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		
		boolean foundPollutant = false;
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				PM10Total.pollutantName,context.iterProcess.processName)) {
			shouldCalcPM10Total = true;
			foundPollutant = true;
			enabledSectionNames.add("PM10Total");
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				PM25Total.pollutantName,context.iterProcess.processName)) {
			shouldCalcPM25Total = true;
			foundPollutant = true;
			enabledSectionNames.add("PM25Total");
		}

		if(!foundPollutant) {
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/PMTotalExhaustCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
