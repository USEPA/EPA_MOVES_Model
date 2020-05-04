/**************************************************************************************************
 * @(#)ModelScaleTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the ModelScale class
 *
 * @author		wfaler
 * @version		2008-11-08
**/
public class ModelScaleTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public ModelScaleTest(String name) {
		super(name);
	}

	/** Implements the test case(s). **/
	public void testCase() {
		ModelScale testModelScale;
		// Verify against the static instances
		testModelScale = ModelScale.getByName(ModelScale.MACROSCALE.toString());
		assertEquals(testModelScale.toString().compareToIgnoreCase(
				ModelScale.MACROSCALE.toString()), 0);
		testModelScale = ModelScale.getByName(ModelScale.MESOSCALE_LOOKUP.toString());
		assertEquals(testModelScale.toString().compareToIgnoreCase(
				ModelScale.MESOSCALE_LOOKUP.toString()), 0);
		// Verify invalid
		assertNull(ModelScale.getByName(" "));
	}
}
