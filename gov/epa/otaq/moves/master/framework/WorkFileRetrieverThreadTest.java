/**************************************************************************************************
 * @(#)WorkFileRetrieverThreadTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.master.runspec.*;
import java.sql.SQLException;
import junit.framework.*;

/**
 * Test Case for the WorkFileRetrieverThread class
 *
 * @author		Cimulus
 * @version		2003-03-07
**/
public class WorkFileRetrieverThreadTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public WorkFileRetrieverThreadTest(String name) {
		super(name);
	}

	/**
	 * Test launching and requesting termination of the thread.
	 * @throws InterruptedException Exception from Thread.join method
	 **/
	public void testLifeCycle() throws InterruptedException, SQLException {
/*
		// initialize the connections to the county db
		DatabaseConnectionManager.getTheManager().openDefaultGUIConnection();

		MOVESEngine nmimEngine = new MOVESEngine();
		DataHandlerHelper dataHandlerHelper = new MOBILEDataHandlerHelper(0);
		
		RunSpec runSpec = new RunSpec();
		ExecutionRunSpec.theExecutionRunSpec = new ExecutionRunSpec(runSpec);
		WorkFileRetrieverThread localThread = new WorkFileRetrieverThread(movesEngine,
				dataHandlerHelper, "WorkFileRetrieverThread");
		localThread.start();
		
		assertTrue(localThread.isAlive());
		localThread.signalToTerminate();
		
		Logger.log(LogMessageCategory.DEBUG, "Called join");
		localThread.join();
*/
	}
}
