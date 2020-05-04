/**************************************************************************************************
 * @(#)TreeSetIgnoreCaseTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the TreeSetIgnoreCaseTest class
 *
 * @author		Cimulus
 * @version		2004-01-14
**/
public class TreeSetIgnoreCaseTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public TreeSetIgnoreCaseTest(String name) {
		super(name);
	}

	/**
	 * Tests case insensitive operations
	**/
	public void testCaseInsensitivity() {
		TreeSetIgnoreCase set = new TreeSetIgnoreCase();

		String s1 = "ABC";
		String s2 = "abc";

		set.add(s1);
		assertTrue("Case insensitive contains did not work",set.contains(s2));

		set.add(s2);
		assertTrue("Case insensitive add did not work",set.size() == 1);
	}
}
