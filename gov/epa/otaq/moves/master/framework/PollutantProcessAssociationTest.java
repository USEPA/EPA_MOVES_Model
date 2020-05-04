/**************************************************************************************************
 * @(#)PollutantProcessAssociationTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import junit.framework.*;

/**
 * Test Case for the PollutantProcessAssociation class
 *
 * @author		Wesley Faler
 * @version		2012-11-07
**/
public class PollutantProcessAssociationTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public PollutantProcessAssociationTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testPollutantProcessAssociation() {
		EmissionProcess process1 = new EmissionProcess(3001, "process1", "Y", true, true);
		EmissionProcess process2 = new EmissionProcess(3002, "process2", "Y", true, true);
		Pollutant pollutant1 = new Pollutant(5001, "pollutant1", true, true);
		Pollutant pollutant2 = new Pollutant(5002, "pollutant2", true, true);
		PollutantProcessAssociation test1 = new PollutantProcessAssociation();
		test1.pollutant = pollutant1;
		test1.emissionProcess = process1;
		PollutantProcessAssociation test2 = new PollutantProcessAssociation();
		test2.pollutant = pollutant2;
		test2.emissionProcess = process2;
		PollutantProcessAssociation test3 = new PollutantProcessAssociation();
		test3.pollutant = pollutant1;
		test3.emissionProcess = process2;
		PollutantProcessAssociation test4 = test3;
		assertTrue(test1 != test2);
		assertTrue(test1.compareTo(test2) != 0);
		assertTrue(test1 != test4);
		assertTrue(test1.compareTo(test4) != 0);
		assertTrue(test3 == test4);
		assertTrue(test3.compareTo(test4) == 0);
		test4.pollutant = pollutant1;
		test4.emissionProcess = process2;
		assertTrue(test3 == test4);
		assertTrue(test3.compareTo(test4) == 0);
	}
}
