/**************************************************************************************************
 * @(#)NO2Calculator.java
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
 * @author		EPA Ed Glover
 * @version		2011-12-12
**/
public class NO2Calculator extends EmissionCalculator {
	/**
	 * @algorithm
	 * @owner NO2 Calculator
	 * @calculator
	**/

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.   Pollutant 33 - Nitrogen Dioxide (NO2) is registered.
	**/
	public NO2Calculator() {
		Pollutant nO2 = Pollutant.findByID(33);

		EmissionProcess runningExhaust = EmissionProcess.findByID(1);
		EmissionProcess startExhaust = EmissionProcess.findByID(2);
		EmissionProcess extendedIdleExhaust = EmissionProcess.findByID(90);
		EmissionProcess auxiliaryPowerExhaust = EmissionProcess.findByID(91);

		EmissionCalculatorRegistration.register(nO2,runningExhaust,this);
		EmissionCalculatorRegistration.register(nO2,startExhaust,this);
		EmissionCalculatorRegistration.register(nO2,extendedIdleExhaust,this);
		EmissionCalculatorRegistration.register(nO2,auxiliaryPowerExhaust,this);
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
				Logger.log(LogMessageCategory.INFO,"NO2Calculator chained to " + c);
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

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Oxides of Nitrogen (NOx)",context.iterProcess.processName)) {
			foundPollutant = true;
		}

		if(!foundPollutant) {
			Logger.log(LogMessageCategory.INFO,"NO2 Calculator's SQLForWorker method " +
			"does NOT find oxides of nitrogen");
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		// no special replacements, no special sections

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/NO2Calculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			Logger.log(LogMessageCategory.INFO,"NO2 Calculator doExecute method IS successful");
			return sqlForWorker;
		} else {
			Logger.log(LogMessageCategory.INFO,"NO2 Calculator doExecute method NOT successful");
			return null;
		}
	}
}
