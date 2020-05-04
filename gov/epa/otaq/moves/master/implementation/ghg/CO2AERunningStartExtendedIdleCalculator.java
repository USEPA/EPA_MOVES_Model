/**************************************************************************************************
 * @(#)CO2AERunningStartExtendedIdleCalculator.java
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
 * Perform CO2 step-1a & -2: calculating Atmosphric CO2 and CO2 Equivalent emissions from
 * Running, start, and Extended Idle processes if called for in the ExecutionRunSpec.
 * This is done as a chained calculator, so that Atmosphric CO2 and CO2 Equivalent
 * can be added to anything that produces Total energy, CH4, and N2O.
 * @author		Gwo Shyu, EPA
 * @author		Wesley Faler
 * @version		2011-12-12
**/
public class CO2AERunningStartExtendedIdleCalculator extends EmissionCalculator {
	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public CO2AERunningStartExtendedIdleCalculator() {
		Pollutant atmoshpericCO2 = Pollutant.findByID(90);
		Pollutant equivalentCO2 = Pollutant.findByID(98);

		ExecutionRunSpec.pollutantNeedsAggregation(atmoshpericCO2);
		ExecutionRunSpec.pollutantNeedsAggregation(equivalentCO2);

		EmissionProcess runningExhaust = EmissionProcess.findByID(1);
		EmissionProcess startExhaust = EmissionProcess.findByID(2);
		EmissionProcess extendedIdleExhaust = EmissionProcess.findByID(90);
		EmissionProcess auxiliaryPowerExhaust = EmissionProcess.findByID(91);

		EmissionCalculatorRegistration.register(atmoshpericCO2,runningExhaust,this);
		EmissionCalculatorRegistration.register(equivalentCO2,runningExhaust,this);

		EmissionCalculatorRegistration.register(atmoshpericCO2,startExhaust,this);
		EmissionCalculatorRegistration.register(equivalentCO2,startExhaust,this);

		EmissionCalculatorRegistration.register(atmoshpericCO2,extendedIdleExhaust,this);
		EmissionCalculatorRegistration.register(equivalentCO2,extendedIdleExhaust,this);

		EmissionCalculatorRegistration.register(atmoshpericCO2,auxiliaryPowerExhaust,this);
		EmissionCalculatorRegistration.register(equivalentCO2,auxiliaryPowerExhaust,this);
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		Pollutant totalEnergy = Pollutant.findByID(91);
		Pollutant methane = Pollutant.findByID(5);
		Pollutant nitrousOxides = Pollutant.findByID(6);
		EmissionProcess wellToPump = EmissionProcess.findByID(99);
		// Determine which pollutant(s) should be calculated
		boolean shouldCalcAtmoshpericCO2 = false;
		boolean shouldCalcEquivalentCO2 = false;
		Pollutant equivalentCO2 = Pollutant.findByID(98);

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2","Running Exhaust") ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2","Start Exhaust")  ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2","Extended Idle Exhaust") ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2","Auxiliary Power Exhaust")) {
					shouldCalcAtmoshpericCO2 = true;
		}

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,"Running Exhaust") ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,"Start Exhaust")  ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,"Extended Idle Exhaust") ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,"Auxiliary Power Exhaust")) {
					shouldCalcEquivalentCO2 = true;
		}

		TreeSet<EmissionCalculator> calculators = new TreeSet<EmissionCalculator>();
		if(shouldCalcAtmoshpericCO2) {
			LinkedList<EmissionCalculator> t = EmissionCalculatorRegistration.findNotInProcess(
				totalEnergy,wellToPump);
			calculators.addAll(t);
		}
		if(shouldCalcEquivalentCO2) {
			LinkedList<EmissionCalculator> t1 = EmissionCalculatorRegistration.findNotInProcess(
					methane,wellToPump);
			calculators.addAll(t1);
			LinkedList<EmissionCalculator> t2 = EmissionCalculatorRegistration.findNotInProcess(
				nitrousOxides,wellToPump);
			calculators.addAll(t2);
		}

//Logger.log(LogMessageCategory.WARNING,"*** Start CO2AERunningStartExtendedIdleCalculator chained to:");
		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				c.chainCalculator(this);
//Logger.log(LogMessageCategory.WARNING,"              "+ c.toString());
			}
		}
//Logger.log(LogMessageCategory.WARNING,"** End CO2AERunningStartExtendedIdleCalculator");
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

		boolean foundPollutant = false;
		Pollutant equivalentCO2 = Pollutant.findByID(98);

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2","Running Exhaust") ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2","Start Exhaust")  ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2","Extended Idle Exhaust") ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Atmospheric CO2","Auxiliary Power Exhaust")) {
					shouldCalcAtmoshpericCO2 = true;
			foundPollutant = true;
		}

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,"Running Exhaust") ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,"Start Exhaust")  ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,"Extended Idle Exhaust") ||
			ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				equivalentCO2.pollutantName,"Auxiliary Power Exhaust")) {
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
		//			for the calculation of Equivalent CO2.
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

		replacements.put("##CO2Step2pollutantIDs##",pollutantIDsStep2);

		if (shouldCalcEquivalentCO2){
			String processIDsStep2 = "";
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					equivalentCO2.pollutantName,"Running Exhaust")) {
				if(processIDsStep2.length() > 0) {
					processIDsStep2 += ",";
				}
				processIDsStep2 += "1";
			}
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					equivalentCO2.pollutantName,"Start Exhaust")) {
				if(processIDsStep2.length() > 0) {
					processIDsStep2 += ",";
				}
				processIDsStep2 += "2";
			}
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					equivalentCO2.pollutantName,"Extended Idle Exhaust")) {
				if(processIDsStep2.length() > 0) {
					processIDsStep2 += ",";
				}
				processIDsStep2 += "90";
			}
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					equivalentCO2.pollutantName,"Auxiliary Power Exhaust")) {
				if(processIDsStep2.length() > 0) {
					processIDsStep2 += ",";
				}
				processIDsStep2 += "91";
			}
			replacements.put("##CO2Step2processIDs##","mwo.processID IN (" +
				processIDsStep2 + ") ");
		} else {
			replacements.put("##CO2Step2processIDs##","1=2 ");
		}

		// Step 1a: Atmospheric CO2 Requested by RunSpec
		if (shouldCalcAtmoshpericCO2) {
			String processIDsStep1A = "";
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					"Atmospheric CO2","Running Exhaust")) {
				if(processIDsStep1A.length() > 0) {
					processIDsStep1A += ",";
				}
				processIDsStep1A += "1";
			}
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					"Atmospheric CO2","Start Exhaust")) {
				if(processIDsStep1A.length() > 0) {
					processIDsStep1A += ",";
				}
				processIDsStep1A += "2";
			}
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					"Atmospheric CO2","Extended Idle Exhaust")) {
				if(processIDsStep1A.length() > 0) {
					processIDsStep1A += ",";
				}
				processIDsStep1A += "90";
			}
			if (ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					"Atmospheric CO2","Auxiliary Power Exhaust")) {
				if(processIDsStep1A.length() > 0) {
					processIDsStep1A += ",";
				}
				processIDsStep1A += "91";
			}
			replacements.put("##CO2Step1AprocessIDs##",	"mwo.processID IN (" +
			processIDsStep1A + ") ");
		} else {
			replacements.put("##CO2Step1AprocessIDs##","1=2 ");
		}

		replacements.put("##CH4pollutantID##","5");
		replacements.put("##N2OpollutantID##","6");
		replacements.put("##equivalentCO2pollutantID##","98");
		replacements.put("##atmoshpericCO2pollutantID##","90");
		replacements.put("##totalEnergyConsumptionID##","91");
		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/CO2AERunningStartExtendedIdleCalculator.sql",
				enabledSectionNames, sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
