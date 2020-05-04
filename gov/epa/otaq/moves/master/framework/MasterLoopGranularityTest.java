/**************************************************************************************************
 * @(#)MasterLoopGranularityTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

//import gov.epa.otaq.moves.master.implementation.general.FuelGenerator;
import java.io.*;
import junit.framework.*;

/**
 * Test Case for the MasterLoopGranularity class
 *
 * @author		Cimulus
 * @version		2006-01-16
**/
public class MasterLoopGranularityTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public MasterLoopGranularityTest(String name) {
		super(name);
	}

	/**
	 * Tests MasterLoopGranularity.compareTo
	**/
	public void testCompareTo() {
		assertEquals("HOUR > DAY",
			getUnitInt(MasterLoopGranularity.HOUR.compareTo(MasterLoopGranularity.DAY)), 1);
		assertEquals("DAY < HOUR",
			getUnitInt(MasterLoopGranularity.DAY.compareTo(MasterLoopGranularity.HOUR)), -1);
		assertEquals("COUNTY < DAY",
			getUnitInt(MasterLoopGranularity.COUNTY.compareTo(MasterLoopGranularity.DAY)), -1);
	}

	/**
	 * Extremely simple function that takes a signed int and divides it by the absolute
	 * value of itself. The result will be -1, 0, or 1.
	 * @param source The signed integer
	 * @return The result. Either -1, 0, or 1.
	**/
	private int getUnitInt(int source) {
		if (source > 0) {
			return 1;
		} else if (source < 0) {
			return -1;
		} else {
			return 0;
		}
	}
}
