/**************************************************************************************************
 * @(#)DummyCalculator.java
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
 * Dummy calculator to fulfill requirement for generators without a calculator. 
 *
 * @author		Cimulus
 * @version		2005-10-26
**/
public class DummyCalculator extends EmissionCalculator {
	/** The Evap Permeation emission process **/
	EmissionProcess evapPermeationProcess = EmissionProcess.findByName("Evap Permeation");
	/** The Evap Fuel Vapor Venting emission process **/
	EmissionProcess evapFuelVaporVentingProcess = EmissionProcess.findByName("Evap Fuel Vapor Venting");
	/** The Evap Fuel Leaks emission process **/
	EmissionProcess evapFuelLeaksProcess = EmissionProcess.findByName("Evap Fuel Leaks");
	/** The Evap Non-Fuel Vapors emission process **/
	EmissionProcess evapNonFuelVaporsProcess = EmissionProcess.findByName("Evap Non-Fuel Vapors");

	/**
	 * Constructor, including registration of distance handled by this calculator.  
	 * Such registration facilitates calculator
	 * chaining.
	**/
	public DummyCalculator() {
		// This calculator doesn't determine pollutants in any way, so it
		// does not register itself.
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		targetLoop.subscribe(this, evapPermeationProcess, MasterLoopGranularity.YEAR,
				MasterLoopPriority.EMISSION_CALCULATOR);
		targetLoop.subscribe(this, evapFuelVaporVentingProcess, MasterLoopGranularity.YEAR,
				MasterLoopPriority.EMISSION_CALCULATOR);
		targetLoop.subscribe(this, evapFuelLeaksProcess, MasterLoopGranularity.YEAR,
				MasterLoopPriority.EMISSION_CALCULATOR);
		targetLoop.subscribe(this, evapNonFuelVaporsProcess, MasterLoopGranularity.YEAR,
				MasterLoopPriority.EMISSION_CALCULATOR);
	}

	/**
	 * Builds SQL statements for a distributed worker to execute. This is called by
	 * DummyCalculator.executeLoop. Implementations of this method
	 * should contain uncertainty logic when UncertaintyParameters specifies that
	 * this mode is enabled.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	**/
	public SQLForWorker doExecute(MasterLoopContext context) {
		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		//boolean isOK = readAndHandleScriptedCalculations(context,replacements,
		//		"database/DummyCalculator.sql",enabledSectionNames,
		//		sqlForWorker);
		//if(isOK) {
		return sqlForWorker;
		//} else {
		//	return null;
		//}
	}
}
