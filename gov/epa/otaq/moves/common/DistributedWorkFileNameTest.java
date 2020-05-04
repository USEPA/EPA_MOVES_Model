/**************************************************************************************************
 * @(#)DistributedWorkFileNameTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.File;
import java.io.FilenameFilter;
import junit.framework.*;

/**
 * Test Case for the DistributedWorkFileName class
 *
 * @author		Wesley Faler
 * @version		2010-02-06
**/
public class DistributedWorkFileNameTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public DistributedWorkFileNameTest(String name) {
		super(name);
	}

	/** Test parsing of file names **/
	public void testParsing() {
		DistributedWorkFileName workFileName;

		assertNull(DistributedWorkFileName.createFrom("8781_1_TODO_42001"));
		assertNull(DistributedWorkFileName.createFrom("8781_1_BAD"));
		assertNull(DistributedWorkFileName.createFrom("8781_1_DONE_42001"));
		assertNull(DistributedWorkFileName.createFrom("8781_1__DONE"));
		assertNull(DistributedWorkFileName.createFrom("gibberish"));
		assertNull(DistributedWorkFileName.createFrom("8781_1_TODO_42001_"));
		assertNull(DistributedWorkFileName.createFrom("8781_1_TODO42001"));

		workFileName = DistributedWorkFileName.createFrom("8781_1_TODO");
		assertNotNull(workFileName);
		identityFilterTest(workFileName);
		assertEquals(workFileName.mid.compareTo("8781"), 0);
		assertEquals(workFileName.qid.compareTo("1"), 0);
		assertEquals(workFileName.purpose,DistributedWorkFilePurpose.CALCULATOR);
		assertEquals(workFileName.state, DistributedWorkFileState.TODO);
		assertNull(workFileName.wid);
		
		FilenameFilter filter = DistributedWorkFileName.buildFileNameFilter(
				workFileName.mid, workFileName.qid, workFileName.purpose, workFileName.state, "*");
		assertTrue(filter.accept(null, workFileName.toString()));

		workFileName = DistributedWorkFileName.createFrom("8781_1_InProgress_223");
		assertNotNull(workFileName);
		identityFilterTest(workFileName);
		assertEquals(workFileName.mid.compareTo("8781"), 0);
		assertEquals(workFileName.qid.compareTo("1"), 0);
		assertEquals(workFileName.purpose,DistributedWorkFilePurpose.CALCULATOR);
		assertEquals(workFileName.state, DistributedWorkFileState.IN_PROGRESS);
		assertEquals(workFileName.wid.compareTo("223"), 0);

		workFileName = DistributedWorkFileName.createFrom("8781_1_GenInProgress_223");
		assertNotNull(workFileName);
		identityFilterTest(workFileName);
		assertEquals(workFileName.mid.compareTo("8781"), 0);
		assertEquals(workFileName.qid.compareTo("1"), 0);
		assertEquals(workFileName.purpose,DistributedWorkFilePurpose.GENERATOR);
		assertEquals(workFileName.state, DistributedWorkFileState.IN_PROGRESS);
		assertEquals(workFileName.wid.compareTo("223"), 0);

		workFileName = DistributedWorkFileName.createFrom("8781_1_GenTODO");
		assertNotNull(workFileName);
		identityFilterTest(workFileName);
		assertEquals(workFileName.mid.compareTo("8781"), 0);
		assertEquals(workFileName.qid.compareTo("1"), 0);
		assertEquals(workFileName.purpose,DistributedWorkFilePurpose.GENERATOR);
		assertEquals(workFileName.state, DistributedWorkFileState.TODO);
		assertNull(workFileName.wid);
	}

	/**
	 * Asserts that a filter built around a file name will accept itself.
	 * @param testName The parsed name to test.
	**/
	public void identityFilterTest(DistributedWorkFileName testName) {
		FilenameFilter filter = DistributedWorkFileName.buildFileNameFilter(
				testName.mid, testName.qid, testName.purpose, testName.state, testName.wid);
		assertTrue(filter.accept(null, testName.toString()));
	}

	/** Tests isValid() **/
	public void testIsValid() {
		DistributedWorkFileName workFileName = new DistributedWorkFileName();
		assertTrue(!(workFileName.isValid()));
		workFileName = DistributedWorkFileName.createFrom("8781_1_TODO");
		assertTrue(workFileName.isValid());
		workFileName.mid = null;
		assertTrue(!(workFileName.isValid()));

		workFileName = DistributedWorkFileName.createFrom("8781_1_InProgress_42001");
		assertNotNull(workFileName);
		assertTrue(workFileName.isValid());
		workFileName.mid = null;
		assertTrue(!(workFileName.isValid()));
	}

	/** Tests alterFilePathState() **/
	public void testAlterFilePathState() {
		DistributedWorkFileName workFileName =
				DistributedWorkFileName.createFrom("8781_1_TODO");
		File testFile = new File(workFileName.toString());
		File alteredFile = DistributedWorkFileName.alterFilePathState(testFile,
				DistributedWorkFileState.DONE);
		assertNotNull(alteredFile);
		assertEquals(alteredFile.getName().compareTo("8781_1_DONE"), 0);
	}
}
