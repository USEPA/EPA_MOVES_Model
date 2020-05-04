/**************************************************************************************************
 * @(#)TreeMapIgnoreCaseTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the TreeMapIgnoreCaseTest class
 *
 * @author		Cimulus
 * @version		2004-01-14
**/
public class TreeMapIgnoreCaseTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public TreeMapIgnoreCaseTest(String name) {
		super(name);
	}

	/**
	 * Tests case insensitive operations
	**/
	public void testCaseInsensitivity() {
		TreeMapIgnoreCase map = new TreeMapIgnoreCase();

		String data1 = "DATA1";
		String data2 = "DATA2";

		String s1 = "ABC";
		String s2 = "abc";

		map.put(s1,data1);
		assertTrue("Case insensitive get did not work",map.get(s2) == data1);

		map.put(s2,data2);
		assertTrue("Case insensitive put did not work",map.size() == 1);
		assertTrue("Case insensitive get did not work",map.get(s1) == data2);
	}
}
