/****************************************************************************************
 * @(#)ExecutionLocationTest.java
 *
	Last change:  MC   17 Dec 2003    8:29 am
 ***************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import junit.framework.*;

/**
 * Test Case for the ExecutionLocation class
 *
 * @author		Mitch Cumberworth
 * @version		2003-12-17
**/
public class ExecutionLocationTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public ExecutionLocationTest(String name) {
		super(name);
	}

	/** An array of nine ExecutionLocations is needed for this test **/
	ExecutionLocation[] execLocs = new ExecutionLocation[9];

	/** JUnit "fixture". Create 9 ExecutionLocation objects needed by tests **/
	protected void setUp() {
		execLocs[0] = createExecLoc(2,2,2,2);
		execLocs[1] = createExecLoc(2,2,2,1);
		execLocs[2] = createExecLoc(2,2,2,3);
		execLocs[3] = createExecLoc(2,2,1,1);
		execLocs[4] = createExecLoc(2,2,3,3);
		execLocs[5] = createExecLoc(2,1,4,1);
		execLocs[6] = createExecLoc(2,3,1,4);
		execLocs[7] = createExecLoc(1,1,1,4);
		execLocs[8] = createExecLoc(3,1,1,4);
	}
/** makes and assigns value to an ExecutionLocation.
 *  ExecutionLocation does not have a constructor that does this.
 *  @param stateRecID the database primary key value for the state
 *  @param countyRecID the database primary key value for the county
 *  @param zoneRecID the database primary key value for the zone
 *  @param linkRecID the database primary key value for the link
 *  @return the ExecutionLocation object created
 **/
	protected ExecutionLocation createExecLoc(int stateRecID, int countyRecID,
			int zoneRecID, int linkRecID) {
		ExecutionLocation eLoc = new ExecutionLocation();
		eLoc.stateRecordID = stateRecID;
		eLoc.countyRecordID = countyRecID;
		eLoc.zoneRecordID = zoneRecID;
		eLoc.linkRecordID = linkRecID;
		return eLoc;
	}

	/** Implement test case(s)
  * Compare first element of execLocs to every element, including itself
  * Test that the integer returned,
  * when converted to -1 (when negative) , 0 (when zero) , or +1 (when positive) ,
  * agrees with the corresponding element of expectedOutcomes. **/
	public void testExecLocCompareTo() {
		int[] expectedOutcomes = {0, 1,-1, 1,-1, 1,-1, 1,-1} ;
		for ( int i=0 ; i<expectedOutcomes.length ; i++) {
			int j = execLocs[0].compareTo(execLocs[i]);
			assertTrue("ExecutionLocationTest Number " + String.valueOf(i) + " Failed",
					((j==0) ? 0 : ((Math.abs(j)/j))) == expectedOutcomes[i] );
		}
	}
}
