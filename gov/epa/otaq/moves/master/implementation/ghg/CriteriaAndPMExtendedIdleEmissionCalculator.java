/**************************************************************************************************
 * @(#)CriteriaAndPMExtendedIdleEmissionCalculator.java
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
 * Does processing for the criteria pollutants in MOVES2006, within extended idle exhaust processes.
 *
 * @author		Wesley Faler
 * @author		Gwo Shyu/EPA
 * @version		2013-05-28
**/
public class CriteriaAndPMExtendedIdleEmissionCalculator extends GenericCalculatorBase
		implements MasterLoopContext.IContextFilter {
	/** @notused **/
	/**
	 * constructor
	**/
	public CriteriaAndPMExtendedIdleEmissionCalculator() {
		super(new String[]{
				"190", // Total Gaseous Hydrocarbons, extended idle exhaust
				"290", // Carbon Monoxide (CO), extended idle exhaust
				"390", // Oxides of Nitrogen, extended idle exhaust
				"11890", // Organic Carbon PM Size 2.5, extended idle exhaust
				"11290"}, // Elemental Carbon PM Size 2.5, extended idle exhaust
			MasterLoopGranularity.YEAR,  // Do not sign up for ZONE level or above
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/CriteriaAndPMExtendedIdleEmissionCalculator.sql",
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
