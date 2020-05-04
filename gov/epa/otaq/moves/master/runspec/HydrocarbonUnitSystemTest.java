/**************************************************************************************************
 * @(#)HydrocarbonUnitSystemTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import junit.framework.*;

/**
 * Test Case for the HydrocarbonUnitSystem class
 *
 * @author		Cimulus
 * @version		2003-02-04
**/
public class HydrocarbonUnitSystemTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public HydrocarbonUnitSystemTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testHydrocarbonUnitSystem() {
		HydrocarbonUnitSystem firstTestHydrocarbonUnitSystem =
				HydrocarbonUnitSystem.getByName("this doesn't exist");
		assertNull(firstTestHydrocarbonUnitSystem);
		firstTestHydrocarbonUnitSystem = HydrocarbonUnitSystem.getByName("NMOG");
		assertNotNull(firstTestHydrocarbonUnitSystem);
	}
}
