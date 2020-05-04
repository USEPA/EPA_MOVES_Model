/**************************************************************************************************
 * @(#)SCCTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import junit.framework.*;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.*;

/**
 * Test Case for the SCC class
 *
 * @author		Wesley Faler
 * @version		2009-04-05
**/
public class SCCTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public SCCTest(String name) {
		super(name);
	}

	/**
	 * Tests the compareTo() function.
	**/
	public void testCompareTo() {
		SCC firstSCC = new SCC();
		firstSCC.scc = "scc 1";
		SCC secondSCC = new SCC();
		secondSCC.scc = "scc 1";
		assertTrue(firstSCC.compareTo(secondSCC) == 0);
		assertTrue(secondSCC.compareTo(firstSCC) == 0);
		secondSCC.scc = "scc 2";
		assertTrue(firstSCC.compareTo(secondSCC) < 0);
		assertTrue(secondSCC.compareTo(firstSCC) > 0);

		// Test implicit use of compareTo from use of TreeSet
		TreeSet<SCC> testTreeSet = new TreeSet<SCC>();
		testTreeSet.add(firstSCC);
		testTreeSet.add(secondSCC);
	}
}
