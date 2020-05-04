/**************************************************************************************************
 * @(#)PM10BrakeTireCalculator.java
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
 * Perform Brakewear and Tirewear PM10 calculations
 * @author		Wesley Faler
 * @version		2009-09-19
**/
public class PM10BrakeTireCalculator extends EmissionCalculator {
	/**
	 * @algorithm
	 * @owner PM10 Brake Tire Calculator
	 * @calculator
	**/

	/** Brakewear process **/
	EmissionProcess brakewear;
	/** Tirewear process **/
	EmissionProcess tirewear;

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public PM10BrakeTireCalculator() {
		Pollutant brakePM10 = Pollutant.findByID(106);
		brakewear = EmissionProcess.findByID(9);

		Pollutant tirePM10 = Pollutant.findByID(107);
		tirewear = EmissionProcess.findByID(10);

		if(brakewear != null && brakePM10 != null) {
			EmissionCalculatorRegistration.register(brakePM10,brakewear,this);
		} else {
			brakewear = null;
		}
		if(tirewear != null && tirePM10 != null) {
			EmissionCalculatorRegistration.register(tirePM10,tirewear,this);
		} else {
			tirewear = null;
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		LinkedList<EmissionCalculator> calculators = new LinkedList<EmissionCalculator>();

		Pollutant brakePM25 = Pollutant.findByID(116);
		if(brakewear != null && brakePM25 != null) {
			LinkedList<EmissionCalculator> t = 
					EmissionCalculatorRegistration.find(brakePM25,brakewear);
			EmissionCalculatorRegistration.merge(calculators,t);
		} else {
			brakewear = null;
		}

		Pollutant tirePM25 = Pollutant.findByID(117);
		if(tirewear != null && tirePM25 != null) {
			LinkedList<EmissionCalculator> t = 
					EmissionCalculatorRegistration.find(tirePM25,tirewear);
			EmissionCalculatorRegistration.merge(calculators,t);
		} else {
			tirewear = null;
		}

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
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		SQLForWorker sqlForWorker = new SQLForWorker();
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();

		boolean found = false;

		if(brakewear != null && context.hasProcess(brakewear)) {
			enabledSectionNames.add("Brakewear");
			found = true;
		}
		if(tirewear != null && context.hasProcess(tirewear)) {
			enabledSectionNames.add("Tirewear");
			found = true;
		}

		if(!found) {
			return null;
		}

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/PM10BrakeTireCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
