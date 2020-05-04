/**************************************************************************************************
 * @(#)BasicTireWearPMEmissionCalculator.java
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
 * Calculations for PM 2.5 pollutants in the Tire Wear process.
 * @author		Cimulus
 * @version		2007-05-13
**/
public class BasicTireWearPMEmissionCalculator extends GenericCalculatorBase {
	/** @notused **/

	/**
	 * constructor
	**/
	public BasicTireWearPMEmissionCalculator() {
		super(new String[] { 
				"11710"   // Tirewear PM Size 2.5, Tire Wear process
			},
			MasterLoopGranularity.YEAR,
				// Year level is used for compatibility with TAG and in case IM
				// is needed in future versions.
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/BasicPM25Calculator.sql",
			new String[]{ "HasManyOpModes", "EmissionRateRates", 
					"SourceHoursOperatingActivity", "NoTemperatureAdjustment" }
		);
	}
}
