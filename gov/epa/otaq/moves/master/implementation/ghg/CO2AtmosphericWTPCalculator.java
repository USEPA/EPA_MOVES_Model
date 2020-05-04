/**************************************************************************************************
 * @(#)CO2AtmosphericWTPCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.master.framework.OutputProcessor;

import java.util.*;
//import java.sql.*;

/**
 * Perform step 1b: calculating Atmosphric CO2 emission from 
 * Well-To-Pump process if called for in the ExecutionRunSpec.
 * This is done as a chained calculator, so that Atmosphric CO2
 * can be added to processes that produce pump-to-wheel Total Energy,
 * ie, the step 6a of Total Energy Calcilator.
 * @author		Gwo Shyu, EPA
 * @version		2009-04-04
**/
public class CO2AtmosphericWTPCalculator extends EmissionCalculator {
	/** @notused **/
	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public CO2AtmosphericWTPCalculator() {
		Pollutant atmoshpericCO2 = Pollutant.findByID(90);
		EmissionProcess wellToPump = EmissionProcess.findByID(99);

		EmissionCalculatorRegistration.register(atmoshpericCO2,wellToPump,this);
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
   		Pollutant atmosphericCO2 = Pollutant.findByID(90);
		LinkedList<EmissionCalculator> t = 
				EmissionCalculatorRegistration.findNotInProcess(atmosphericCO2,wellToPump);
		calculators.addAll(t);

 //Logger.log(LogMessageCategory.WARNING,"** Start * CO2AtmosphericWTPCalculator chained to:");
		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				c.chainCalculator(this);
 //Logger.log(LogMessageCategory.WARNING,"              "+ c.toString());
			}
		}
 //Logger.log(LogMessageCategory.WARNING,"** End CO2AtmosphericWTPCalculator");
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
		boolean shouldCalcAtmoshpericCO2 = false;
		boolean shouldCalcEquivalentCO2 = false;
		EmissionProcess wellToPump = EmissionProcess.findByID(99);

		boolean foundPollutant = false;
		
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2",wellToPump.processName)){
					shouldCalcAtmoshpericCO2 = true;
			foundPollutant = true;
		}

		if(!foundPollutant) {
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		String pollutantIDs = "";

		replacements.put("##atmoshpericCO2pollutantID##","90");
		replacements.put("##totalEnergyConsumptionID##","91");
		replacements.put("##ExtendedIdleExhaustID##","90");
		replacements.put("##StartExhaustID##","2");
		replacements.put("##RunningExhaustID##","1");
		replacements.put("##Well-To-PumpID##","99");
		

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/CO2AtmosphericWTPCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
