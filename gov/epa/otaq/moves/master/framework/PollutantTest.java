/**************************************************************************************************
 * @(#)PollutantTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import junit.framework.*;

/**
 * Test Case for the Pollutant class
 *
 * @author		Cimulus
 * @version		2003-02-07
**/
public class PollutantTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public PollutantTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testPollutant() {
		int pollutantInstanceCount = Pollutant.allPollutants.size();
		Pollutant p1 = new Pollutant(5001, "p1", true, true);
		assertTrue(Pollutant.allPollutants.size() == pollutantInstanceCount + 1);
		pollutantInstanceCount = Pollutant.allPollutants.size();
		Pollutant p2 = new Pollutant(5002, "p2", true, true);
		assertTrue(Pollutant.allPollutants.size() == pollutantInstanceCount + 1);
		pollutantInstanceCount = Pollutant.allPollutants.size();
		Pollutant p3 = new Pollutant(5003, "p3", true, true);
		assertTrue(Pollutant.allPollutants.size() == pollutantInstanceCount + 1);

		Pollutant firstTestPollutant = Pollutant.findByName("this doesn't exist");
		assertNull(firstTestPollutant);
		firstTestPollutant = Pollutant.findByName("p2");
		assertNotNull(firstTestPollutant);

		Pollutant secondTestPollutant = Pollutant.findByName("p3");
		assertTrue(firstTestPollutant != secondTestPollutant);
		assertTrue(firstTestPollutant.compareTo(secondTestPollutant) != 0);
		secondTestPollutant = Pollutant.findByName("p2");
		assertTrue(firstTestPollutant == secondTestPollutant);
		assertTrue(firstTestPollutant.compareTo(secondTestPollutant) == 0);
	}
}
