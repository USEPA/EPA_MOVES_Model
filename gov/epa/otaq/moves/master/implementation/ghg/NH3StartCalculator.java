/**************************************************************************************************
 * @(#)NH3StartCalculator.java
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
 * Does processing for the ammonia (NH3) pollutants.
 *
 * @author		Wesley Faler
 * @author      Gwo Shyu, EPA (2006/6/12 Task 216; 2009/10/16 EPA NH3 task)
 * @version     2011-06-21
**/
public class NH3StartCalculator extends GenericCalculatorBase
		implements MasterLoopContext.IContextFilter {
	/** @notused **/
	/**
	 * constructor
	**/
	public NH3StartCalculator() {
		super(new String[]{
				"3002" // Ammonia (NH3), Start Exhaust
			},
			MasterLoopGranularity.YEAR,
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/NH3StartCalculator.sql",
			null // No special section names in the SQL
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
