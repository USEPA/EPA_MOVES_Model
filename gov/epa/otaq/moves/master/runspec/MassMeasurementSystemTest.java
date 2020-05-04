/**************************************************************************************************
 * @(#)MassMeasurementSystemTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import junit.framework.*;

/**
 * Test Case for the MassMeasurementSystem class
 *
 * @author		Cimulus
 * @version		2003-02-05
**/
public class MassMeasurementSystemTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public MassMeasurementSystemTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testStringConversion() {
		for (Iterator i = MassMeasurementSystem.allTypes.iterator(); i.hasNext();) {
			MassMeasurementSystem iterSystem = (MassMeasurementSystem) i.next();
			MassMeasurementSystem foundObject
				= MassMeasurementSystem.getByDescription(iterSystem.toString());
			assertTrue(iterSystem == foundObject);
		}
	}

	/** Tests equals(). **/
	public void testEquals() {
		MassMeasurementSystem first = null;
		MassMeasurementSystem second = MassMeasurementSystem.KILOGRAMS;
		// Expect equals() to return false when compared to null
		assertTrue(!(second.equals(first)));
		first = MassMeasurementSystem.KILOGRAMS;
		assertEquals(first, second);
		first = MassMeasurementSystem.POUNDS;
		assertTrue(!(first.equals(second)));
	}
}
