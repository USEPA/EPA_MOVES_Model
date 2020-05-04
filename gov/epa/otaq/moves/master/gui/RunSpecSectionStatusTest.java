/**************************************************************************************************
 * @(#)RunSpecSectionStatusTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import junit.framework.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;

/**
 * Test Case for the RunSpecSectionStatus class
 *
 * @author		Cimulus
 * @version		2003-08-21
**/
public class RunSpecSectionStatusTest extends TestCase {
	/** The various runspec statues **/
	static final int[] statusValues =
	{
		RunSpecSectionStatus.NOT_READY,
		RunSpecSectionStatus.DEFAULTS,
		RunSpecSectionStatus.OK
	};

	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public RunSpecSectionStatusTest(String name) {
		super(name);
	}

	/** Test the equals operator **/
	public void testEquals() {
		for (int i = 0; i < statusValues.length; i++) {
			RunSpecSectionStatus a = new RunSpecSectionStatus(statusValues[i]);
			for (int j = 0; j < statusValues.length; j++) {
				RunSpecSectionStatus b = new RunSpecSectionStatus(statusValues[j]);
				
				assertEquals("equals isn't communicative", a.equals(b), b.equals(a));
				assertEquals(a.equals(b), a.status == b.status);
				assertEquals(a.equals(b), i == j);
			}
		}
	}

	/** Test the compareTo operator **/
	public void testCompareTo() {
		for (int i = 0; i < statusValues.length; i++) {
			RunSpecSectionStatus a = new RunSpecSectionStatus(statusValues[i]);
			for (int j = 0; j < statusValues.length; j++) {
				RunSpecSectionStatus b = new RunSpecSectionStatus(statusValues[j]);
				
				assertTrue(a.compareTo(b) * b.compareTo(a) <= 0);
				assertEquals(a.compareTo(b) == 0, a.equals(b));
				assertEquals(b.compareTo(a) == 0, a.equals(b));
			}
		}
	}

	/** Test the best and worst methods **/
	public void testBestWorst() {
		RunSpecSectionStatus best = new RunSpecSectionStatus();
		best.makeBest();
		RunSpecSectionStatus worst = new RunSpecSectionStatus();
		worst.makeWorst();
		
		for (int i = 0; i < statusValues.length; i++) {
			RunSpecSectionStatus a;
			
			a = new RunSpecSectionStatus(statusValues[i]);
			a.makeBestOfTwo(best);
			assertTrue(a.equals(best));

			a = new RunSpecSectionStatus(statusValues[i]);
			a.makeBestOfTwo(worst);
			assertTrue(a.compareTo(worst) >= 0);
			
			a = new RunSpecSectionStatus(statusValues[i]);
			a.makeWorstOfTwo(best);
			assertTrue(a.compareTo(best) <= 0);

			a = new RunSpecSectionStatus(statusValues[i]);
			a.makeWorstOfTwo(worst);
			assertTrue(a.equals(worst));
		}
	}
}
