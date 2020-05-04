/**************************************************************************************************
 * @(#)OffRoadVehicleSelectionTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the OffRoadVehicleSelection class
 *
 * @author		Wesley Faler
 * @version		2010-01-18
**/
public class OffRoadVehicleSelectionTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public OffRoadVehicleSelectionTest(String name) {
		super(name);
	}

	/**
	 * Implements the test case(s).
	**/
	public void testOffRoadVehicleSelection() {
		// Test the isValid() function
		OffRoadVehicleSelection test = new OffRoadVehicleSelection();
		assertTrue(!test.isValid());
		test.fuelTypeID = 0;
		test.fuelTypeDesc = "";
		test.sectorID = 0;
		test.sectorName = "";
		assertTrue(!test.isValid());
		// Currently OffRoadVehicleSelection doesn't validate against the database, so these
		// settings should be valid.
		test.fuelTypeID = 2;
		test.fuelTypeDesc = "Diesel";
		test.sectorID = 6;
		test.sectorName = "Sector 6";
		assertTrue(test.isValid());

		// Test compareTo()
		OffRoadVehicleSelection test2 = new OffRoadVehicleSelection();
		test2.fuelTypeID = 1;
		test2.fuelTypeDesc = "Gasoline";
		test2.sectorID = 6;
		test2.sectorName = "Sector 6";
		assertTrue(test2.compareTo(test) < 0);
		assertTrue(test.compareTo(test2) > 0);
		test.fuelTypeID = test2.fuelTypeID;
		test.fuelTypeDesc = test2.fuelTypeDesc;
		test.sectorID = test2.sectorID;
		test.sectorName = test2.sectorName;
		assertTrue(test.compareTo(test2) == 0);
		assertTrue(test2.compareTo(test) == 0);
	}
}
