/**************************************************************************************************
 * @(#)OutputEmissionsBreakdownSelectionTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import junit.framework.*;

/**
 * Test Case for the OutputEmissionsBreakdownSelection class
 *
 * @author		Cimulus
 * @version		2003-02-05
**/
public class OutputEmissionsBreakdownSelectionTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public OutputEmissionsBreakdownSelectionTest(String name) {
		super(name);
	}

	/** Test equals() **/
	public void testEquals() {
		OutputEmissionsBreakdownSelection first = new OutputEmissionsBreakdownSelection();
		OutputEmissionsBreakdownSelection second = new OutputEmissionsBreakdownSelection();
		assertEquals(first, second);
		first.modelYear = true;
		assertTrue(!first.equals(second));
		second.modelYear = true;
		assertEquals(first, second);
	}
}
