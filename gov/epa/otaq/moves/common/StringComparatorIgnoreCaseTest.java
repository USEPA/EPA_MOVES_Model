/**************************************************************************************************
 * @(#)StringComparatorIgnoreCaseTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the StringComparatorIgnoreCaseTest class
 *
 * @author		Cimulus
 * @version		2004-01-14
**/
public class StringComparatorIgnoreCaseTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public StringComparatorIgnoreCaseTest(String name) {
		super(name);
	}

	/**
	 * Tests compare()
	**/
	public void testCompare() {
		StringComparatorIgnoreCase comparator = new StringComparatorIgnoreCase();

		String s1 = "ABC";
		assertTrue("Case insensitive identity did not work",comparator.compare(s1,s1) == 0);

		String s2 = "abc";
		assertTrue("Case insensitive equality did not work",comparator.compare(s1,s2) == 0);

		String s3 = "xyz";
		assertTrue("Case insensitive ordering did not work",comparator.compare(s1,s3) < 0);
		assertTrue("Case insensitive ordering did not work",comparator.compare(s3,s1) > 0);
	}
}
