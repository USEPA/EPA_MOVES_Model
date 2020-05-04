/**************************************************************************************************
 * @(#)MOVESEngineTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.master.runspec.*;
import junit.framework.*;

/**
 * Test Case for the MOVESEngine class
 *
 * @author		Cimulus
 * @version		2004-07-15
**/
public class MOVESEngineTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public MOVESEngineTest(String name) {
		super(name);
	}

	/** Performs class wide setup. **/
	protected void setUp() {
	}

	/** Performs class wide setup. **/
	protected void tearDown() {
		DatabaseConnectionManager.cleanupAll();
	}

	/**
	 * Implements the test case(s).
	 * @throws Exception This is thrown if an error occurs.
	**/
	public void testEngine() throws Exception {
		DatabaseConnectionManagerTest.setupOutputDatabase();
		assertTrue(DatabaseConnectionManager.initializeAll());

		RunSpec runSpec = new RunSpec();
		RunSpecTest.setSampleValues(runSpec);

		HeartbeatDetectionThread.isOKToLaunchWorker = false;
		MOVESEngine.theInstance = new MOVESEngine();
		try {
			MOVESEngine.theInstance.launch(runSpec, "MOVESEngineTest");
			System.out.println("Launched engine. Waiting for termination...");
			MOVESEngine.theInstance.join(10000);

			if(MOVESEngine.theInstance.isRunning()) {
				System.out.println("Timed out waiting for MOVESEngine to exit. Terminating.");
				MOVESEngine.theInstance.signalToTerminate();
				MOVESEngine.theInstance.join();
			} else {
				System.out.println("MOVESEngine completed on its own.");
			}
		} finally {
			MOVESEngine.theInstance = null;
		}
	}
}
