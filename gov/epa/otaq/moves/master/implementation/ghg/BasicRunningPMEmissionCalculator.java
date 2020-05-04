/**************************************************************************************************
 * @(#)BasicRunningPMEmissionCalculator.java
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
 * Calculations for PM 2.5 pollutants in the Running Exhaust process.
 * @author		Wesley Faler
 * @version		2013-05-15
**/
public class BasicRunningPMEmissionCalculator extends GenericCalculatorBase {
	/** @notused **/

	/**
	 * constructor
	**/
	public BasicRunningPMEmissionCalculator() {
		super(new String[] { 
				"11801",  // NonECPM Size 2.5, Running Exhaust
				"11201"   // ECarbon PM Size 2.5, Running Exhaust
			},
			MasterLoopGranularity.YEAR,
				// Year level is used for compatibility with TAG and in case IM
				// is needed in future versions.
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/BasicPM25Calculator.sql",
			new String[]{ "HasManyOpModes", "EmissionRateByAgeRates", 
					"SourceHoursOperatingActivity", "ApplyTemperatureAdjustment" }
		);
	}
}
