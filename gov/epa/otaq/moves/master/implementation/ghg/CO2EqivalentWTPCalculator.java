/**************************************************************************************************
 * @(#)CO2EqivalentWTPCalculator.java
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
 * Perform step 2 calculating Eqivalent CO2 emission from 
 * Well-To-Pump process if called for in the ExecutionRunSpec.
 * This is done as a chained calculator, so that Eqivalent CO2
 * can be added to processes that produce CO2, CH4, and N2O.
 * @author		Gwo Shyu, EPA
 * @version		2009-04-04
**/
public class CO2EqivalentWTPCalculator extends EmissionCalculator {
	/** @notused **/
	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public CO2EqivalentWTPCalculator() {
		Pollutant equivalentCO2 = Pollutant.findByID(98);
		EmissionProcess wellToPump = EmissionProcess.findByID(99);

		ExecutionRunSpec.pollutantNeedsAggregation(equivalentCO2);

		EmissionCalculatorRegistration.register(equivalentCO2,wellToPump,this);
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		Pollutant atmosphericCO2 = Pollutant.findByID(90);
		Pollutant methane = Pollutant.findByID(5);
		Pollutant nitrousOxides = Pollutant.findByID(6);
		EmissionProcess runningExhaust = EmissionProcess.findByID(1);
		EmissionProcess startExhaust = EmissionProcess.findByID(2);
		EmissionProcess extendedIdleExhaust = EmissionProcess.findByID(90);
		EmissionProcess wellToPump = EmissionProcess.findByID(99);
		Pollutant equivalentCO2 = Pollutant.findByID(98);
		boolean shouldCalcEquivalentCO2 = false;

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,wellToPump.processName)) {
					shouldCalcEquivalentCO2 = true;
		}
		
		TreeSet<EmissionCalculator> calculators = new TreeSet<EmissionCalculator>();
		if (shouldCalcEquivalentCO2) {

			// Chain this calculator on to CO2AtmosphericWTPCalculator
			LinkedList<EmissionCalculator> t3 = 
					EmissionCalculatorRegistration.find(atmosphericCO2,wellToPump);
			calculators.addAll(t3);
		}
			
 //Logger.log(LogMessageCategory.WARNING,"** Start CO2EqivalentWTPCalculator chained to :");
		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				c.chainCalculator(this);
 //Logger.log(LogMessageCategory.WARNING,"              "+ c.toString());
			}
		}
 //Logger.log(LogMessageCategory.WARNING,"** End CO2EqivalentWTPCalculator");
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
		boolean shouldCalcEquivalentCO2 = false;
		EmissionProcess wellToPump = EmissionProcess.findByID(99);
		Pollutant equivalentCO2 = Pollutant.findByID(98);

		boolean foundPollutant = false;
		
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,wellToPump.processName)){
					shouldCalcEquivalentCO2 = true;
			foundPollutant = true;
		}

		if(!foundPollutant) {
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		// Step 2: Equivqlent CO2 Requested by RunSpec, use Total energy, CH4, and N2O
		//			for the calculation of Equivqlent CO2.
		String pollutantIDsStep2 = "";
		
		if(pollutantIDsStep2.length() > 0) {
			pollutantIDsStep2 += ",";
		}
		pollutantIDsStep2 += "90";
		
		if(pollutantIDsStep2.length() > 0) {
			pollutantIDsStep2 += ",";
		}
		pollutantIDsStep2 += "5";
		
		if(pollutantIDsStep2.length() > 0) {
			pollutantIDsStep2 += ",";
		}
		pollutantIDsStep2 += "6";
		
		replacements.put("##CO2Step2EqpollutantIDs##",pollutantIDsStep2);

		if (shouldCalcEquivalentCO2){
			String processIDsStep2 = "";
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					equivalentCO2.pollutantName,wellToPump.processName)) {
				if(processIDsStep2.length() > 0) {
					processIDsStep2 += ",";
				}
				processIDsStep2 += "99";
			}
			replacements.put("##CO2Step2EqprocessIDs##","mwo.processID IN (" + 
				processIDsStep2 + ") ");
		} else {
			replacements.put("##CO2Step2EqprocessIDs##","1=2 ");
		}

		replacements.put("##CO2EqpollutantID##","98");
/*	
		replacements.put("##atmosphericCO2pollutantID##","90");
		replacements.put("##CH4pollutantID##","5");
		replacements.put("##N2OpollutantID##","6");
		replacements.put("##totalEnergyConsumptionID##","91");
		replacements.put("##ExtendedIdleExhaustID##","90");
		replacements.put("##StartExhaustID##","2");
		replacements.put("##RunningExhaustID##","1");
		replacements.put("##Well-To-PumpID##","99");
*/		

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/CO2EqivalentWTPCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
