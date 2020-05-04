/**************************************************************************************************
 * @(#)WildCardFileNameFilterTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import junit.framework.*;

/**
 * Test Case for the WildCardFileNameFilter class
 *
 * @author		Cimulus
 * @version		2003-01-28
**/
public class WildCardFileNameFilterTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public WildCardFileNameFilterTest(String name) {
		super(name);
	}

	/** Test wildcard pattern matching **/
	public void testFiltering() {
		assertTrue(WildCardFileNameFilter.doesFilterMatch("",			""));
		assertTrue(WildCardFileNameFilter.doesFilterMatch("*",			""));
		assertTrue(WildCardFileNameFilter.doesFilterMatch("*",			"abcdefg"));
		assertTrue(WildCardFileNameFilter.doesFilterMatch("*cd*",		"abcdefg"));
		assertTrue(WildCardFileNameFilter.doesFilterMatch("**cd*",		"abcdefg"));
		assertTrue(WildCardFileNameFilter.doesFilterMatch("***cd*",		"abcdefg"));
		assertTrue(WildCardFileNameFilter.doesFilterMatch("a*bc**g",	"abcdefg"));

		assertTrue(!WildCardFileNameFilter.doesFilterMatch("asdf",		""));
		assertTrue(!WildCardFileNameFilter.doesFilterMatch("",			"abcdefg"));
		assertTrue(!WildCardFileNameFilter.doesFilterMatch("asdf",		"abcdefg"));
		assertTrue(!WildCardFileNameFilter.doesFilterMatch("asdf",		"abcdefg"));
	}
}
