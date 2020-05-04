/**************************************************************************************************
 * @(#)MasterLoopableSubscriptionTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

//import gov.epa.otaq.moves.master.implementation.general.FuelGenerator;
import java.io.*;
import junit.framework.*;

/**
 * Test Case for the MasterLoopableSubscription class
 *
 * @author		Cimulus
 * @version		2006-01-16
**/
public class MasterLoopableSubscriptionTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public MasterLoopableSubscriptionTest(String name) {
		super(name);
	}

	/**
	 * Implements the test case(s).
	**/
	public void testCompareTo() {
		MasterLoopableSubscription subscriptionA = new MasterLoopableSubscription();
		MasterLoopableSubscription subscriptionB = new MasterLoopableSubscription();
/*
		subscriptionA.granularity = MasterLoopGranularity.HOUR;
		subscriptionA.priority = 10;
		// Arbitrary concrete implementation of MasterLoopable
		subscriptionA.loopable = new FuelGenerator();
		subscriptionB.granularity = MasterLoopGranularity.HOUR;
		subscriptionB.priority = 10;
		subscriptionB.loopable = subscriptionA.loopable;

		assertEquals("The loopable object should equal itself",
			subscriptionA.compareTo(subscriptionB), 0);
		assertEquals("Symmetry Test Failed",
			getUnitInt(subscriptionA.compareTo(subscriptionB)),
			-getUnitInt(subscriptionB.compareTo(subscriptionA)));

		// Arbitrary concrete implementation of MasterLoopable
		subscriptionB.loopable = new FuelGenerator();

		assertTrue("Two different loopable objects shouldn't be equal",
			subscriptionA.compareTo(subscriptionB) != 0);
		assertEquals("Symmetry Test Failed",
			getUnitInt(subscriptionA.compareTo(subscriptionB)),
			-getUnitInt(subscriptionB.compareTo(subscriptionA)));

		subscriptionB.loopable = subscriptionA.loopable;
		subscriptionA.granularity = MasterLoopGranularity.HOUR;
		subscriptionB.granularity = MasterLoopGranularity.DAY;	// B will be less than A

		assertEquals("DAY > HOUR", getUnitInt(subscriptionA.compareTo(subscriptionB)), +1);
		assertEquals("Symmetry Test Failed",
			getUnitInt(subscriptionA.compareTo(subscriptionB)),
			-getUnitInt(subscriptionB.compareTo(subscriptionA)));

		subscriptionA.granularity = MasterLoopGranularity.HOUR;	// A will be less than B
		subscriptionA.priority = 100;
		subscriptionB.granularity = MasterLoopGranularity.HOUR;
		subscriptionB.priority = 10;

		assertEquals("priority 100 should sort before 10",
			getUnitInt(subscriptionA.compareTo(subscriptionB)), -1);
		assertEquals("Symmetry Test Failed",
			getUnitInt(subscriptionA.compareTo(subscriptionB)),
			-getUnitInt(subscriptionB.compareTo(subscriptionA)));
*/
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
