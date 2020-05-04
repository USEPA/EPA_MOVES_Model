/**************************************************************************************************
 * @(#)RunSpecXMLTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.io.File;
import java.io.IOException;
import junit.framework.*;

/**
 * Test Case for the RunSpecXML class
 *
 * @author		Cimulus
 * @version		2003-02-04
**/
public class RunSpecXMLTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public RunSpecXMLTest(String name) {
		super(name);
	}

	/**
	 * Tests streaming RunSpec's to and from XML files.
	 * @throws IOException This will be thrown in the case of file I/O errors.
	**/
	public void testXMLStreaming() throws IOException {
		RunSpec sampleRunSpec = new RunSpec();
		RunSpecTest.setSampleValues(sampleRunSpec);

		convertToXMLAndBackAndAssertEquality(sampleRunSpec);
	}

	/**
	 * Converts the specified RunSpec to XML, reparses it as a new RunSpec and asserts
	 * that the two are equal.
	 * @param originalRunSpec The original RunSpec to work from.
	 * @throws IOException This will be thrown in the case of file I/O errors.
	**/
	void convertToXMLAndBackAndAssertEquality(final RunSpec originalRunSpec) throws IOException {
		File temporaryXMLPath = new File("testdata/junit_test_runspec.xml");
		File reSaveXMLPath = new File("testdata/reread_junit_test_runspec.cso");

		try {
			// Save to a temp file
			RunSpecXML streamer = new RunSpecXML(originalRunSpec);
			streamer.save(temporaryXMLPath);
			// Reload into a new RunSpec object
			RunSpec reReadRunSpec = new RunSpec();
			streamer = new RunSpecXML(reReadRunSpec);
			streamer.load(temporaryXMLPath);
			// Save the newly re-loaded XML file, useful for debugging
			streamer.save(reSaveXMLPath);
			// Compare objects
			RunSpecTest.assertEqualRunSpecs(originalRunSpec, reReadRunSpec);

/*			// Test simple save/load
			streamer.load(temporaryXMLPath);
			streamer.save(reSaveXMLPath);
			RunSpec reReadRunSpec = new RunSpec();
			streamer = new RunSpecXML(reReadRunSpec);
			streamer.load(reSaveXMLPath);
			RunSpecTest.assertEqualRunSpecs(originalRunSpec, reReadRunSpec);
*/
		} finally {
			System.out.println("Temporarily leaving XML file");
//			temporaryXMLPath.delete();
//			reSaveXMLPath.delete();
		}
	}
}
