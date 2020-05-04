/**************************************************************************************************
 * @(#)NH3RunningCalculator.java
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
 * Does processing for the ammonia (NH3) pollutants, within the engine running processes.
 *
 * @author      Gwo Shyu, EPA (2007/5/29 Task 216; 2009/10/16 EPA NH3 task)
 * @version     2009-10-16
**/
public class NH3RunningCalculator extends GenericCalculatorBase {
	/** @notused **/
	/**
	 * constructor
	**/
	public NH3RunningCalculator() {
		super(new String[]{
				"3001" 
			},  // Ammonia (NH3), Running Exhaust
			MasterLoopGranularity.YEAR,
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/NH3RunningCalculator.sql",
			null
		);
	}
}
