/**************************************************************************************************
 * @(#)GeographicSelectionTypeTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import junit.framework.*;

/**
 * Test Case for the GeographicSelection class
 *
 * @author		Cimulus
 * @version		2003-02-28
**/
public class GeographicSelectionTypeTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public GeographicSelectionTypeTest(String name) {
		super(name);
	}

	/** Tests the compareTo method. **/
	public void testCompare() {
		assertTrue(GeographicSelectionType.NATION.compareTo(GeographicSelectionType.STATE) > 0);
		assertTrue(GeographicSelectionType.STATE.compareTo(GeographicSelectionType.COUNTY) > 0);
		assertTrue(GeographicSelectionType.COUNTY.compareTo(GeographicSelectionType.ZONE) > 0);
		assertTrue(GeographicSelectionType.ZONE.compareTo(GeographicSelectionType.LINK) > 0);
	}

	/** Converts each type to and from a String and tests for identity. **/
	public void testStringConversion() {
		typeStringConversionTest(GeographicSelectionType.LINK);
		typeStringConversionTest(GeographicSelectionType.ZONE);
		typeStringConversionTest(GeographicSelectionType.COUNTY);
		typeStringConversionTest(GeographicSelectionType.STATE);
		typeStringConversionTest(GeographicSelectionType.NATION);
	}

	/**
	 * Tests the conversion to and from a String for the specified type.
	 *
	 * @param testType The target type to test.
	**/
	void typeStringConversionTest(GeographicSelectionType testType) {
		GeographicSelectionType fromStringType
			= GeographicSelectionType.getByName(testType.toString());
		assertTrue(fromStringType == testType);
	}
}
