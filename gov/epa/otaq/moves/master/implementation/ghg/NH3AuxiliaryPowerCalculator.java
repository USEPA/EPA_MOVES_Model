/**************************************************************************************************
 * @(#)NH3AuxiliaryPowerCalculator.java
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
 * Does processing for the ammonia (NH3) pollutant in MOVES2010, within auxiliary power exhaust processes.
 *
 * @author		Wesley Faler
 * @version		2011-12-12
**/
public class NH3AuxiliaryPowerCalculator extends GenericCalculatorBase
		implements MasterLoopContext.IContextFilter {
	/** @notused **/
	/**
	 * constructor
	**/
	public NH3AuxiliaryPowerCalculator() {
		super(new String[]{
				"3091"}, // Ammonia (NH3), auxiliary power exhaust
			MasterLoopGranularity.YEAR,  // Do not sign up for ZONE level or above
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/NH3AuxiliaryPowerCalculator.sql",
			null
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
