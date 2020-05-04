/**************************************************************************************************
 * @(#)CrankcaseEmissionCalculatorPM.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

import java.util.*;
import java.sql.*;

/**
 * Perform Crankcase calculations as ratios to other pollutants for particulates.
 * @author		Wesley Faler
 * @version		2013-05-27
**/
public class CrankcaseEmissionCalculatorPM extends CrankcaseEmissionCalculator {
	public CrankcaseEmissionCalculatorPM() {
		super(new int[] {
			105, 118, 112, 115
		});
		tablePrefix = "PM";
	}
}
