/**************************************************************************************************
 * @(#)EvaporativePermeationCalculator.java
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
 * Does processing for the evaporative permeation process.
 *
 * @author		Cimulus
 * @author      Gwo Shyu, EPA (minor mods for Task 216)
 * @version     2006-06-12
**/
public class EvaporativePermeationCalculator extends GenericCalculatorBase {
	/**
	 * @algorithm
	 * @owner Evaporative Permeation Calculator
	 * @calculator
	**/

	/**
	 * constructor
	**/
	public EvaporativePermeationCalculator() {
		super(new String[]{ 
				"111"},	// Total Gaseous Hydrocarbons Evap Permeation
			MasterLoopGranularity.MONTH,
			1, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/EvaporativePermeationCalculator.sql",
			new String[]{
				"Total Gaseous Hydrocarbons Evap Permeation"
			}
		);
	}
}
