/**************************************************************************************************
 * @(#)DistanceMeasurementSystemTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import junit.framework.*;

/**
 * Test Case for the DistanceMeasurementSystem class
 *
 * @author		Cimulus
 * @version		2003-11-30
**/
public class DistanceMeasurementSystemTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public DistanceMeasurementSystemTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testStringConversion() {
		for (Iterator i = DistanceMeasurementSystem.allTypes.iterator(); i.hasNext();) {
			DistanceMeasurementSystem iterSystem = (DistanceMeasurementSystem) i.next();
			DistanceMeasurementSystem foundObject
				= DistanceMeasurementSystem.getByDescription(iterSystem.toString());
			assertTrue(iterSystem == foundObject);
		}
	}

	/** Tests equals(). **/
	public void testEquals() {
		DistanceMeasurementSystem first = null;
		DistanceMeasurementSystem second = DistanceMeasurementSystem.KILOMETERS;
		// Expect equals() to return false when compared to null
		assertTrue(!(second.equals(first)));
		first = DistanceMeasurementSystem.KILOMETERS;
		assertEquals(first, second);
		first = DistanceMeasurementSystem.MILES;
		assertTrue(!(first.equals(second)));
	}
}
