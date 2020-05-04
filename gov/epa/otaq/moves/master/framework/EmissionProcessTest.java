/**************************************************************************************************
 * @(#)EmissionProcessTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import junit.framework.*;

/**
 * Test Case for the EmissionProcess class
 *
 * @author		Wesley Faler
 * @version		2012-11-08
**/
public class EmissionProcessTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public EmissionProcessTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testProcess() {
		int processInstanceCount = EmissionProcess.allProcesses.size();
		EmissionProcess p1 = new EmissionProcess(3001, "p1", "Y", true, true);
		assertTrue(EmissionProcess.allProcesses.size() == processInstanceCount + 1);
		processInstanceCount = EmissionProcess.allProcesses.size();
		EmissionProcess p2 = new EmissionProcess(3002, "p2", "Y", true, true);
		assertTrue(EmissionProcess.allProcesses.size() == processInstanceCount + 1);
		processInstanceCount = EmissionProcess.allProcesses.size();
		EmissionProcess p3 = new EmissionProcess(3003, "p3", "Y", true, true);
		assertTrue(EmissionProcess.allProcesses.size() == processInstanceCount + 1);

		EmissionProcess firstTestProcess = EmissionProcess.findByName("this doesn't exist");
		assertNull(firstTestProcess);
		firstTestProcess = EmissionProcess.findByName("p2");
		assertNotNull(firstTestProcess);

		EmissionProcess secondTestProcess = EmissionProcess.findByName("p3");
		assertTrue(firstTestProcess != secondTestProcess);
		assertTrue(firstTestProcess.compareTo(secondTestProcess) != 0);
		secondTestProcess = EmissionProcess.findByName("p2");
		assertTrue(firstTestProcess == secondTestProcess);
		assertTrue(firstTestProcess.compareTo(secondTestProcess) == 0);
	}
}
