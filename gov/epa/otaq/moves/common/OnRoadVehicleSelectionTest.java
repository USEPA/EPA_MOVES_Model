/**************************************************************************************************
 * @(#)OnRoadVehicleSelectionTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the OnRoadVehicleSelection class
 *
 * @author		Cimulus
 * @version		2003-04-25
**/
public class OnRoadVehicleSelectionTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public OnRoadVehicleSelectionTest(String name) {
		super(name);
	}

	/**
	 * Implements the test case(s).
	**/
	public void testOnRoadVehicleSelection() {
		// Test the isValid() function
		OnRoadVehicleSelection test = new OnRoadVehicleSelection();
		assertTrue(!test.isValid());
		test.fuelTypeID = 0;
		test.fuelTypeDesc = "";
		test.sourceTypeID = 0;
		test.sourceTypeName = "";
		assertTrue(!test.isValid());
		// Currently OnRoadVehicleSelection doesn't validate against the database, so these
		// settings should be valid.
		test.fuelTypeID = 100;
		test.fuelTypeDesc = "Fuel BBB";
		test.sourceTypeID = 200;
		test.sourceTypeName = "SourceUseType YYY";
		assertTrue(test.isValid());

		// Test compareTo()
		OnRoadVehicleSelection test2 = new OnRoadVehicleSelection();
		test2.fuelTypeID = 99;
		test2.fuelTypeDesc = "Fuel AAA";
		test2.sourceTypeID = 199;
		test2.sourceTypeName = "SourceUseType XXX";
		assertTrue(test2.compareTo(test) < 0);
		assertTrue(test.compareTo(test2) > 0);
		test.fuelTypeID = test2.fuelTypeID;
		test.fuelTypeDesc = test2.fuelTypeDesc;
		test.sourceTypeID = test2.sourceTypeID;
		test.sourceTypeName = test2.sourceTypeName;
		assertTrue(test.compareTo(test2) == 0);
		assertTrue(test2.compareTo(test) == 0);
	}
}
