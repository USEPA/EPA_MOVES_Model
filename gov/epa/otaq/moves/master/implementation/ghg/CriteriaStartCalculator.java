/**************************************************************************************************
 * @(#)CriteriaStartCalculator.java
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
 * Does processing for the criteria pollutants.
 *
 * @author		Wesley Faler
 * @author      Gwo Shyu, EPA (minor mods for Task 216)
 * @version     2011-06-21
**/
public class CriteriaStartCalculator extends GenericCalculatorBase
		implements MasterLoopContext.IContextFilter {
	/** @notused **/
	/**
	 * constructor
	**/
	public CriteriaStartCalculator() {
		super(new String[]{
				"102", // Total Gaseous Hydrocarbons, Start Exhaust
				"202", // Carbon Monoxide (CO), Start Exhaust
				"302"}, // Oxides of Nitrogen, Start Exhaust
			MasterLoopGranularity.MONTH,
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/CriteriaStartCalculator.sql",
			new String[]{
				"Total Gaseous Hydrocarbons Start Exhaust",
				"Carbon Monoxide (CO) Start Exhaust",
				"Oxides of Nitrogen (NOx) Start Exhaust"
			}
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
