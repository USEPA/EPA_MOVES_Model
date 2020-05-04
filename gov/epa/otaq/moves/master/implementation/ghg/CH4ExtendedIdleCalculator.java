/**************************************************************************************************
 * @(#)CH4ExtendedIdleCalculator.java
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
 * Calculations for Methane in Extended Idle Exhaust process.
 * @author		Wesley Faler
 * @version		2011-06-21
**/
public class CH4ExtendedIdleCalculator extends GenericCalculatorBase
		implements MasterLoopContext.IContextFilter {
	/** @notused **/

	/**
	 * constructor
	**/
	public CH4ExtendedIdleCalculator() {
		super(new String[] {
				"590"   // Methane, Extended Idle Exhaust
			},
			MasterLoopGranularity.YEAR,
				// Year level is used for compatibility with TAG and in case IM
				// is needed in future versions.
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/CH4ExtendedIdleCalculator.sql",
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
