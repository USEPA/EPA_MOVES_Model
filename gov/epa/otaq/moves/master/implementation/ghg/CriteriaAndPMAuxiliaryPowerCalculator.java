/**************************************************************************************************
 * @(#)CriteriaAndPMAuxiliaryPowerCalculator.java
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
 * Does processing for the criteria pollutants in MOVES2010, within auxiliary power exhaust processes.
 *
 * @author		Wesley Faler
 * @version		2013-05-28
**/
public class CriteriaAndPMAuxiliaryPowerCalculator extends GenericCalculatorBase
		implements MasterLoopContext.IContextFilter {
	/** @notused **/
	/**
	 * constructor
	**/
	public CriteriaAndPMAuxiliaryPowerCalculator() {
		super(new String[]{
				"191", // Total Gaseous Hydrocarbons, auxiliary power exhaust
				"291", // Carbon Monoxide (CO), auxiliary power exhaust
				"391", // Oxides of Nitrogen, auxiliary power exhaust
				"11891", // Organic Carbon PM Size 2.5, auxiliary power exhaust
				"11291"}, // Elemental Carbon PM Size 2.5, auxiliary power exhaust
			MasterLoopGranularity.YEAR,  // Do not sign up for ZONE level or above
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/CriteriaAndPMAuxiliaryPowerCalculator.sql",
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
