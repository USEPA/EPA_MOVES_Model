/**************************************************************************************************
 * @(#)ExecutionRunSpecTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.io.File;
import java.sql.*;
import java.util.Iterator;
import java.util.TreeSet;
import junit.framework.*;

/**
 * Test Case for the ExecutionRunSpec class
 *
 * @author		Cimulus
 * @version		2004-02-01
**/
public class ExecutionRunSpecTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public ExecutionRunSpecTest(String name) {
		super(name);
	}

	/**
	 * Implementes the test case(s).
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException From any of the database operations.
	**/
	public void testInitialization() throws InterruptedException, SQLException {
		setupExecutionRunSpec();

		assertTrue("executionLocations is empty",
				ExecutionRunSpec.theExecutionRunSpec.executionLocations.size() > 0);
		assertTrue("targetProcesses is empty",
				ExecutionRunSpec.theExecutionRunSpec.targetProcesses.size() > 0);
	}

	/**
	 * Initializes the static ExecutionRunSpec instance with test data.
	 * @throws InterruptedException If the active thread is interrupted.
	 * @throws SQLException If a database error occurs.
	**/
	static public void setupExecutionRunSpec() throws InterruptedException, SQLException {
		assertTrue(SystemConfiguration.theSystemConfiguration.didLoad);

		DatabaseConnectionManagerTest.deleteNonNecessaryDatabases();
		DatabaseConnectionManagerTest.setupOutputDatabase();
		assertTrue(DatabaseConnectionManager.initializeAll());

		RunSpec runSpec = new RunSpec();

//		File sampleRunSpecFile = new File("testdata/SampleRunSpec.xml");
//		assertTrue(sampleRunSpecFile.isFile());
//		RunSpecXML streamer = new RunSpecXML(runSpec);
//		streamer.load(sampleRunSpecFile);

		RunSpecTest.setSampleValues(runSpec);

		ExecutionRunSpec.theExecutionRunSpec = new ExecutionRunSpec(runSpec);
		ExecutionRunSpec.theExecutionRunSpec.initializeBeforeExecutionDatabase();
		try {
			InputDataManager.merge();
		} catch(Exception e) {
			fail("Exception merging default into execution database");
			// Nothing can be done here
		}
		ExecutionRunSpec.theExecutionRunSpec.initializeAfterExecutionDatabase();

		assertValidity(ExecutionRunSpec.theExecutionRunSpec);
	}

	/**
	 * Asserts that the given ExecutionRunSpec has valid data.
	 * @param executionRunSpec The ExecutionRunSpec to test.
	**/
	static void assertValidity(ExecutionRunSpec executionRunSpec) {
		validateContainerContents(ExecutionRunSpec.theExecutionRunSpec.runQueueSCCs, SCC.class);
		validateContainerContents(ExecutionRunSpec.theExecutionRunSpec.executionLocations
				, ExecutionLocation.class);
		validateContainerContents(ExecutionRunSpec.theExecutionRunSpec.targetProcesses
				, EmissionProcess.class);
	}

	/**
	 * Asserts that the given container contains no null objects and that all objects
	 * are of the specified class type.
	 * @param treeSet The container to test.
	 * @param requiredClass The class that the objects must be a type of.
	**/
	static void validateContainerContents(TreeSet treeSet, Class requiredClass) {
		assertNotNull(treeSet);

		for (Iterator i = treeSet.iterator(); i.hasNext();) {
			Object object = i.next();

			assertNotNull(object);
			assertEquals(requiredClass, object.getClass());
		}
	}
}
