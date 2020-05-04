/**************************************************************************************************
 * @(#)BasicBrakeWearPMEmissionCalculator.java
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
 * Calculations for PM 2.5 pollutants in the Brake Wear process.
 * @author		Cimulus
 * @author      EPA - Mitch C, changed to expect an operating mode distribution
 * @version		2006-09-20
**/
public class BasicBrakeWearPMEmissionCalculator extends GenericCalculatorBase {
	/** @notused **/

	/**
	 * constructor
	**/
	public BasicBrakeWearPMEmissionCalculator() {
		super(new String[] { 
				"11609"   // Brakewear PM Size 2.5, Brake Wear process
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
