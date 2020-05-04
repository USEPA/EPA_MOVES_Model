/**************************************************************************************************
 * @(#)DistributedWorkFileStateTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import junit.framework.*;

/**
 * Test Case for the DistributedWorkFileState class
 *
 * @author		Cimulus
 * @version		2003-02-06
**/
public class DistributedWorkFileStateTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public DistributedWorkFileStateTest(String name) {
		super(name);
	}

	/** Various test on DistributedWorkFileState **/
	public void testDistributedWorkFileState() {
		assertNotNull(DistributedWorkFileState.getByDescription("TEMP"));
		assertNotNull(DistributedWorkFileState.getByDescription("TODO"));
		assertNotNull(DistributedWorkFileState.getByDescription("InProgress"));
		assertNotNull(DistributedWorkFileState.getByDescription("DONE"));
		assertNull(DistributedWorkFileState.getByDescription("This doesn't exist."));
	}
}
