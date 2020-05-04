/**************************************************************************************************
 * @(#)WildcardMatcherTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import junit.framework.*;
import java.util.*;

/**
 * Test Case for the WildcardMatcher class
 *
 * @author		Wesley Faler
 * @version		2010-02-06
**/
public class WildcardMatcherTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public WildcardMatcherTest(String name) {
		super(name);
	}

	/** Test hasWildcards() **/
	public void testHasWildcards() {
		assertTrue("Failed to find wildcard \"*\"",WildcardMatcher.hasWildcards("*"));
		assertTrue("Failed to find wildcard \"3*\"",WildcardMatcher.hasWildcards("3*"));
		assertTrue("Found wildcard in \"30\"",!WildcardMatcher.hasWildcards("30"));
		assertTrue("Found wildcard in \"\"",!WildcardMatcher.hasWildcards(""));
	}

	/** Test wouldMatchEverything() **/
	public void testWouldMatchEverything() {
		assertTrue("Failed to find all match in \"*\"",WildcardMatcher.wouldMatchEverything("*"));
		assertTrue("Found all match in \"3*\"",!WildcardMatcher.wouldMatchEverything("3*"));
		assertTrue("Found all match in \"30\"",!WildcardMatcher.wouldMatchEverything("30"));
		assertTrue("Found all match in \"\"",!WildcardMatcher.wouldMatchEverything(""));
	}

	/** Test filter() **/
	public void testFilter() {
		TreeSet<Object> numbers = new TreeSet<Object>();
		for(int i=10;i<=99;i++) {
			numbers.add(new Integer(i));
		}
		int sizeBeforeFilter = numbers.size();

		TreeSet values = WildcardMatcher.filter(numbers,"*");
		assertEquals("\"*\" filtered set wrong size",numbers.size(),values.size());

		values = WildcardMatcher.filter(numbers,"ABC");
		assertNull("Did not produce null set",values);

		values = WildcardMatcher.filter(numbers,"3*");
		assertEquals("\"3*\" filtered set wrong size",10,values.size());
		for(int i=30;i<=39;i++) {
			Integer t = new Integer(i);
			assertTrue("" + i + " not in \"3*\" filtered set",values.contains(t));
		}
	}

	/** Test doesFilterMatch() **/
	public void testFiltering() {
		assertTrue(WildcardMatcher.doesFilterMatch("",			""));
		assertTrue(WildcardMatcher.doesFilterMatch("*",			""));
		assertTrue(WildcardMatcher.doesFilterMatch("*",			"abcdefg"));
		assertTrue(WildcardMatcher.doesFilterMatch("*cd*",		"abcdefg"));
		assertTrue(WildcardMatcher.doesFilterMatch("**cd*",		"abcdefg"));
		assertTrue(WildcardMatcher.doesFilterMatch("***cd*",		"abcdefg"));
		assertTrue(WildcardMatcher.doesFilterMatch("a*bc**g",	"abcdefg"));

		assertTrue(!WildcardMatcher.doesFilterMatch("asdf",		""));
		assertTrue(!WildcardMatcher.doesFilterMatch("",			"abcdefg"));
		assertTrue(!WildcardMatcher.doesFilterMatch("asdf",		"abcdefg"));
		assertTrue(!WildcardMatcher.doesFilterMatch("asdf",		"abcdefg"));
	}

	/** Test doesFilterMatch() for use with generator bundles **/
	public void testGeneratorBundleNaming() {
		// Ensure old workers won't find the generator bundles
		assertTrue(WildcardMatcher.doesFilterMatch("*_*_TODO","M123_456_TODO"));
		assertFalse(WildcardMatcher.doesFilterMatch("*_*_TODO","M123_456_DONE"));
		assertFalse(WildcardMatcher.doesFilterMatch("*_*_TODO","M123_456_GenTODO"));
		assertFalse(WildcardMatcher.doesFilterMatch("*_*_TODO","M123_456_GenDONE"));

		// Ensure new workers find the generator bundles
		assertTrue(WildcardMatcher.doesFilterMatch("*_*_*TODO","M123_456_TODO"));
		assertTrue(WildcardMatcher.doesFilterMatch("*_*_*TODO","M123_456_GenTODO"));
		assertFalse(WildcardMatcher.doesFilterMatch("*_*_*TODO","M123_456_DONE"));
		assertFalse(WildcardMatcher.doesFilterMatch("*_*_*TODO","M123_456_GenDONE"));

		// Ensure new masters find the generator bundles
		assertTrue(WildcardMatcher.doesFilterMatch("*_*_GenDONE","M123_456_GenDONE"));
		assertFalse(WildcardMatcher.doesFilterMatch("*_*_GenDONE","M123_456_GenTODO"));
		assertTrue(WildcardMatcher.doesFilterMatch("*_*_DONE","M123_456_DONE"));
		assertFalse(WildcardMatcher.doesFilterMatch("*_*_DONE","M123_456_GenDONE"));
		assertTrue(WildcardMatcher.doesFilterMatch("*_*_*DONE","M123_456_DONE"));
		assertTrue(WildcardMatcher.doesFilterMatch("*_*_*DONE","M123_456_GenDONE"));
	}
}
