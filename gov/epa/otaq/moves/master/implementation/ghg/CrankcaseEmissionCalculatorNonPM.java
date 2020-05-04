/**************************************************************************************************
 * @(#)CrankcaseEmissionCalculatorNonPM.java
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
 * Perform Crankcase calculations as ratios to other pollutants for non-Particulates.
 * @author		Wesley Faler
 * @version		2010-11-27
**/
public class CrankcaseEmissionCalculatorNonPM extends CrankcaseEmissionCalculator {
	/**
	 * @algorithm
	 * @owner Crankcase Emission Calculator
	 * @calculator
	**/
	public CrankcaseEmissionCalculatorNonPM() {
		super(new int[] {
			1, 2, 3, 5, 6,
			20, 21, 22, 23, 24, 25, 26, 27,
			30, 31, 32, 33, 34,
			79, 80, 86, 87,

			40,41,42,43,44,45,46,47,68,69,70,71,72,73,74,75,76,77,78,81,82,
			83,84,168,169,170,171,172,173,174,175,176,177,178,179,181,182,183,
			184,185,186
		});
		tablePrefix = "NonPM";
	}
}
