/**************************************************************************************************
 * @(#)PollutantProcessLoaderTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import junit.framework.*;

/**
 * Test Case for the PollutantProcessLoader class
 *
 * @author		Cimulus
 * @version		2003-02-03
**/
public class PollutantProcessLoaderTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public PollutantProcessLoaderTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testLoad() {
		PollutantProcessLoader.loadFromDatabase();
	}
}
