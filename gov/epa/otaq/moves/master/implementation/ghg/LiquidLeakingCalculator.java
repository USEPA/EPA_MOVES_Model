/**************************************************************************************************
 * @(#)LiquidLeakingCalculator.java
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
 * Does processing for the Liquid Leaking process
 * @author      EPA - Mitch C. Task 212A
 * @version		2006-03-23
**/
public class LiquidLeakingCalculator extends GenericCalculatorBase {
	/**
	 * @algorithm
	 * @owner Liquid Leaking Calculator
	 * @calculator
	**/

	/**
	 * constructor
	**/
	public LiquidLeakingCalculator() {
		super(new String[] { 
				"113"  // Total Gaseous Hydrocarbons Liquid Leaking
			},
			MasterLoopGranularity.MONTH, // Year instead of zone due to IM
										 // Month instead of year due to op modes
			0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
			"database/LiquidLeakingCalculator.sql",
			null // No special section names in the SQL
		);
	}
}
