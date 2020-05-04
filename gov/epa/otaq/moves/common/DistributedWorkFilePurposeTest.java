/**************************************************************************************************
 * @(#)DistributedWorkFilePurposeTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import junit.framework.*;

/**
 * Test Case for the DistributedWorkFilePurpose class
 *
 * @author		Wesley Faler
 * @version		2010-02-06
**/
public class DistributedWorkFilePurposeTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public DistributedWorkFilePurposeTest(String name) {
		super(name);
	}

	/** Various tests on DistributedWorkFilePurpose **/
	public void testDistributedWorkFilePurpose() {
		assertNotNull(DistributedWorkFilePurpose.getByDescription("Gen"));
		assertNotNull(DistributedWorkFilePurpose.getByDescription("Calc"));
		assertNull(DistributedWorkFilePurpose.getByDescription("This doesn't exist."));
	}
}
