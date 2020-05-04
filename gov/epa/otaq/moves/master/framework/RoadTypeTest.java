/**************************************************************************************************
 * @(#)RoadTypeTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.Models;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the RoadType class
 *
 * @author		Wesley Faler
 * @version		2012-11-07
**/
public class RoadTypeTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public RoadTypeTest(String name) {
		super(name);
	}

	/** Implements the test case(s). **/
	public void testCase() {
		RoadType type1 = new RoadType(1,"type1", Models.ModelCombination.M1);
		RoadType testRoadType = RoadType.findByName("type1");
		assertNotNull(testRoadType);
		assertTrue(type1.compareTo(testRoadType) == 0);
		assertTrue(type1 == testRoadType);
		RoadType type2 = new RoadType(2,"type2", Models.ModelCombination.M1);
		testRoadType = RoadType.findByName("type2");
		assertNotNull(testRoadType);
		assertTrue(type2 == testRoadType);
		assertTrue(type2.compareTo(testRoadType) == 0);
		assertTrue(type1.compareTo(type2) != 0);
		assertTrue(type1 != type2);
		// Verify invalid
		assertNull(RoadType.findByName(" "));
	}
}
