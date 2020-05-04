/**************************************************************************************************
 * @(#)GeographicOutputDetailLevelTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import junit.framework.*;

/**
 * Test Case for the GeographicOutputDetailLevel class
 *
 * @author		Cimulus
 * @version		2003-02-03
**/
public class GeographicOutputDetailLevelTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public GeographicOutputDetailLevelTest(String name) {
		super(name);
	}

	/** Implements the test case(s). **/
	public void testCase() {
		GeographicOutputDetailLevel testGeographicOutputDetailLevel;
		// Verify against the static instances
		testGeographicOutputDetailLevel = GeographicOutputDetailLevel.getByName(
				GeographicOutputDetailLevel.NATION.toString());
		assertEquals(testGeographicOutputDetailLevel.toString().compareToIgnoreCase(
				GeographicOutputDetailLevel.NATION.toString()), 0);
		testGeographicOutputDetailLevel = GeographicOutputDetailLevel.getByName(
				GeographicOutputDetailLevel.COUNTY.toString());
		assertEquals(testGeographicOutputDetailLevel.toString().compareToIgnoreCase(
				GeographicOutputDetailLevel.COUNTY.toString()), 0);
		testGeographicOutputDetailLevel = GeographicOutputDetailLevel.getByName(
				GeographicOutputDetailLevel.ROADTYPE.toString());
		assertEquals(testGeographicOutputDetailLevel.toString().compareToIgnoreCase(
				GeographicOutputDetailLevel.ROADTYPE.toString()), 0);
		// Verify invalid
		assertNull(GeographicOutputDetailLevel.getByName(" "));
	}
}
