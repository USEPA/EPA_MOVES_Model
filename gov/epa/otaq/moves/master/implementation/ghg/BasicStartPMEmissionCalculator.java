/**************************************************************************************************
 * @(#)BasicStartPMEmissionCalculator.java
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
 * Calculations for PM 2.5 pollutants in the Start Exhaust process.
 * @author		Wesley Faler
 * @author		David Brzezinski
 * @version		2013-05-15
**/
public class BasicStartPMEmissionCalculator extends GenericCalculatorBase
		implements MasterLoopContext.IContextFilter {
	/** @notused **/

	/**
	 * constructor
	**/
	public BasicStartPMEmissionCalculator() {
		super(new String[] {
				"11802",  // NonECPM Size 2.5, Start Exhaust
				"11202"   // ECarbon PM Size 2.5, Start Exhaust
			},
			MasterLoopGranularity.YEAR,
				// Year level is used for compatibility with TAG and in case IM
				// is needed in future versions.
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/BasicStartPM25Calculator.sql",
			new String[]{ "HasManyOpModes", "EmissionRateByAgeRates",
					"StartsActivity", "ApplyTemperatureAdjustment" }
		);
	}

	/**
	 * Examine a context for suitability.  Used to override the natural execution hierarchy.
	 * @param context Context to be examined
	 * @return true if the context should be used by a MasterLoopable.
	**/
	public boolean doesProcessContext(MasterLoopContext context) {
		if(context.iterLocation != null
				&& context.iterLocation.roadTypeRecordID > 0
				&& context.iterLocation.roadTypeRecordID != 1) {
			return false;
		}
		return true;
	}
}
