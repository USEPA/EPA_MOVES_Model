/**************************************************************************************************
 * @(#)TimeMeasurementSystemTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import junit.framework.*;

/**
 * Test Case for the TimeMeasurementSystem class
 *
 * @author		Cimulus
 * @version		2003-02-04
**/
public class TimeMeasurementSystemTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public TimeMeasurementSystemTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testStringConversion() {
		for (Iterator i = TimeMeasurementSystem.allTypes.iterator(); i.hasNext();) {
			TimeMeasurementSystem iterSystem = (TimeMeasurementSystem) i.next();
			TimeMeasurementSystem foundObject
				= TimeMeasurementSystem.getByDescription(iterSystem.toString());
			assertTrue(iterSystem == foundObject);
		}
	}

	/** Tests equals(). **/
	public void testEquals() {
		TimeMeasurementSystem first = null;
		TimeMeasurementSystem second = TimeMeasurementSystem.SECONDS;
		// Expect equals() to return false when compared to null
		assertTrue(!(second.equals(first)));
		first = TimeMeasurementSystem.SECONDS;
		assertEquals(first, second);
	}
}
