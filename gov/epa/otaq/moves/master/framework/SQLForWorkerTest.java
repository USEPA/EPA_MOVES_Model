/**************************************************************************************************
 * @(#)SQLForWorkerTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import junit.framework.*;

/**
 * Test Case for the SQLForWorker class
 *
 * @author		Cimulus
 * @version		2003-02-12
**/
public class SQLForWorkerTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public SQLForWorkerTest(String name) {
		super(name);
	}

	/** Implements test case(s) **/
	public void testCase() {
		// Simple check on data members
		SQLForWorker sqlForWorker = new SQLForWorker();
		assertNotNull(sqlForWorker.dataExportSQL);
		assertNotNull(sqlForWorker.processingSQL);
		assertTrue(sqlForWorker.dataExportSQL.size() == 0);
		assertTrue(sqlForWorker.processingSQL.size() == 0);
	}
}
