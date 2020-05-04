/**************************************************************************************************
 * @(#)PM10EmissionCalculator.java
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
 * Perform PM10 calculations as ratios to PM2.5 pollutants.
 * @author		Wesley Faler
 * @version		2013-05-27
**/
public class PM10EmissionCalculator extends EmissionCalculator {
	/**
	 * @algorithm
	 * @owner PM10 Emission Calculator
	 * @calculator
	**/

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public PM10EmissionCalculator() {
		Pollutant totalPM10 = Pollutant.findByID(100);
		//Pollutant ocPM10 = Pollutant.findByID(101);
		//Pollutant ecPM10 = Pollutant.findByID(102);
		//Pollutant sPM10 = Pollutant.findByID(105);

		EmissionProcess running = EmissionProcess.findByID(1);
		EmissionProcess start = EmissionProcess.findByID(2);
		EmissionProcess extIdle = EmissionProcess.findByID(90);
		EmissionProcess auxiliaryPower = EmissionProcess.findByID(91);
		EmissionProcess crankcaseRunning = EmissionProcess.findByID(15);
		EmissionProcess crankcaseStart = EmissionProcess.findByID(16);
		EmissionProcess crankcaseExtIdle = EmissionProcess.findByID(17);

		EmissionCalculatorRegistration.register(totalPM10,running,this);
		//EmissionCalculatorRegistration.register(ocPM10,running,this);
		//EmissionCalculatorRegistration.register(ecPM10,running,this);
		//EmissionCalculatorRegistration.register(sPM10,running,this);

		EmissionCalculatorRegistration.register(totalPM10,start,this);
		//EmissionCalculatorRegistration.register(ocPM10,start,this);
		//EmissionCalculatorRegistration.register(ecPM10,start,this);
		//EmissionCalculatorRegistration.register(sPM10,start,this);

		EmissionCalculatorRegistration.register(totalPM10,extIdle,this);
		//EmissionCalculatorRegistration.register(ocPM10,extIdle,this);
		//EmissionCalculatorRegistration.register(ecPM10,extIdle,this);
		//EmissionCalculatorRegistration.register(sPM10,extIdle,this);

		EmissionCalculatorRegistration.register(totalPM10,auxiliaryPower,this);
		//EmissionCalculatorRegistration.register(ocPM10,auxiliaryPower,this);
		//EmissionCalculatorRegistration.register(ecPM10,auxiliaryPower,this);
		//EmissionCalculatorRegistration.register(sPM10,auxiliaryPower,this);

		EmissionCalculatorRegistration.register(totalPM10,crankcaseRunning,this);
		//EmissionCalculatorRegistration.register(ocPM10,crankcaseRunning,this);
		//EmissionCalculatorRegistration.register(ecPM10,crankcaseRunning,this);
		// // Sulfate PM 10 for crankcase is handled elsewhere

		EmissionCalculatorRegistration.register(totalPM10,crankcaseStart,this);
		//EmissionCalculatorRegistration.register(ocPM10,crankcaseStart,this);
		//EmissionCalculatorRegistration.register(ecPM10,crankcaseStart,this);
		// // Sulfate PM 10 for crankcase is handled elsewhere

		EmissionCalculatorRegistration.register(totalPM10,crankcaseExtIdle,this);
		//EmissionCalculatorRegistration.register(ocPM10,crankcaseExtIdle,this);
		//EmissionCalculatorRegistration.register(ecPM10,crankcaseExtIdle,this);
		// // Sulfate PM 10 for crankcase is handled elsewhere
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		Pollutant totalPM25 = Pollutant.findByID(110);
		//Pollutant ocPM25 = Pollutant.findByID(111);
		//Pollutant ecPM25 = Pollutant.findByID(112);
		//Pollutant sPM25 = Pollutant.findByID(115);

		LinkedList<EmissionCalculator> calculators = new LinkedList<EmissionCalculator>();
		LinkedList<EmissionCalculator> t = EmissionCalculatorRegistration.findPollutant(totalPM25);
		EmissionCalculatorRegistration.merge(calculators,t);
		/*
		t = EmissionCalculatorRegistration.findPollutant(ocPM25);
		EmissionCalculatorRegistration.merge(calculators,t);
		t = EmissionCalculatorRegistration.findPollutant(ecPM25);
		EmissionCalculatorRegistration.merge(calculators,t);

		// Sulfate 10 is based on Sulfate 2.5 but only here for Running, Start, and Extended Idle
		EmissionProcess running = EmissionProcess.findByID(1);
		t = EmissionCalculatorRegistration.find(sPM25,running);
		EmissionCalculatorRegistration.merge(calculators,t);

		EmissionProcess start = EmissionProcess.findByID(2);
		t = EmissionCalculatorRegistration.find(sPM25,start);
		EmissionCalculatorRegistration.merge(calculators,t);

		EmissionProcess extIdle = EmissionProcess.findByID(90);
		t = EmissionCalculatorRegistration.find(sPM25,extIdle);
		EmissionCalculatorRegistration.merge(calculators,t);

		EmissionProcess auxiliaryPower = EmissionProcess.findByID(91);
		t = EmissionCalculatorRegistration.find(sPM25,auxiliaryPower);
		EmissionCalculatorRegistration.merge(calculators,t);
		*/

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
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
		Pollutant totalPM10 = Pollutant.findByID(100);
		//Pollutant ocPM10 = Pollutant.findByID(101);
		//Pollutant ecPM10 = Pollutant.findByID(102);
		//Pollutant sPM10 = Pollutant.findByID(105);

		boolean shouldCalcTotalPM10 = false;
		boolean shouldCalcOCPM10 = false;
		boolean shouldCalcECPM10 = false;
		boolean shouldCalcSPM10 = false;

		boolean foundPollutant = false;
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				totalPM10.pollutantName,context.iterProcess.processName)) {
			shouldCalcTotalPM10 = true;
			foundPollutant = true;
		}
		/*
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				ocPM10.pollutantName,context.iterProcess.processName)) {
			shouldCalcOCPM10 = true;
			foundPollutant = true;
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				ecPM10.pollutantName,context.iterProcess.processName)) {
			shouldCalcECPM10 = true;
			foundPollutant = true;
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				sPM10.pollutantName,context.iterProcess.processName)) {
			if(context.iterProcess.processName.equalsIgnoreCase("Running Exhaust")
					|| context.iterProcess.processName.equalsIgnoreCase("Start Exhaust")
					|| context.iterProcess.processName.equalsIgnoreCase("Extended Idle Exhaust")
					|| context.iterProcess.processName.equalsIgnoreCase("Auxiliary Power Exhaust")) {
				shouldCalcSPM10 = true;
				foundPollutant = true;
			}
		}
		*/

		if(!foundPollutant) {
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		String pollutantIDs = "";
		String sourcePollutantIDs = "";

		if(shouldCalcTotalPM10) {
			if(pollutantIDs.length() > 0) {
				pollutantIDs += ",";
				sourcePollutantIDs += ",";
			}
			pollutantIDs += "100";
			sourcePollutantIDs += "110";
		}
		/*
		if(shouldCalcOCPM10) {
			if(pollutantIDs.length() > 0) {
				pollutantIDs += ",";
				sourcePollutantIDs += ",";
			}
			pollutantIDs += "101";
			sourcePollutantIDs += "111";
		}
		if(shouldCalcECPM10) {
			if(pollutantIDs.length() > 0) {
				pollutantIDs += ",";
				sourcePollutantIDs += ",";
			}
			pollutantIDs += "102";
			sourcePollutantIDs += "112";
		}
		if(shouldCalcSPM10) {
			if(pollutantIDs.length() > 0) {
				pollutantIDs += ",";
				sourcePollutantIDs += ",";
			}
			pollutantIDs += "105";
			sourcePollutantIDs += "115";
		}
		*/

		replacements.put("##sourcePollutantIDs##",sourcePollutantIDs);
		replacements.put("##pollutantIDs##",pollutantIDs);

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/PM10EmissionCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
