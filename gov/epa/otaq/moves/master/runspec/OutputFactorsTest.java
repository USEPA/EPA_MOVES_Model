/**************************************************************************************************
 * @(#)OutputFactorsTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import junit.framework.*;

/**
 * Test Case for the OutputFactors class
 *
 * @author		Cimulus
 * @version		2003-11-30
**/
public class OutputFactorsTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public OutputFactorsTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testEquals() {
		OutputFactors first = new OutputFactors();
		OutputFactors second = new OutputFactors();
		assertTrue(first.equals(second));
		assertEquals(first, second);
		first.timeFactorsSelected = true;
		first.timeMeasurementSystem = TimeMeasurementSystem.SECONDS;
		first.distanceMeasurementSystem = DistanceMeasurementSystem.KILOMETERS;
		assertTrue(!first.equals(second));
		second.timeFactorsSelected = true;
		second.timeMeasurementSystem = TimeMeasurementSystem.SECONDS;
		second.distanceMeasurementSystem = DistanceMeasurementSystem.KILOMETERS;
		assertTrue(first.equals(second));
	}
}
