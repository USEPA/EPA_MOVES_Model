/**************************************************************************************************
 * @(#)CriteriaRunningCalculator.java
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
 * Does processing for the criteria pollutants, within the engine running processes.
 *
 * @author		Wesley Faler
 * @author      Gwo Shyu, EPA (minor mods for Task 216)
 * @version     2010-10-10
**/
public class CriteriaRunningCalculator extends GenericCalculatorBase {
	/** @notused **/
	/**
	 * constructor
	**/
	public CriteriaRunningCalculator() {
		super(new String[] {
				"101", // Total Gaseous Hydrocarbons, Running Exhaust
				"201", // Carbon Monoxide (CO), Running Exhaust
				"301"}, // Oxides of Nitrogen, Running Exhaust
			MasterLoopGranularity.MONTH,
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/CriteriaRunningCalculator.sql",
			new String[]{
				"Total Gaseous Hydrocarbons Running Exhaust",
				"Carbon Monoxide (CO) Running Exhaust",
				"Oxides of Nitrogen (NOx) Running Exhaust"
			}
		);
	}
}
