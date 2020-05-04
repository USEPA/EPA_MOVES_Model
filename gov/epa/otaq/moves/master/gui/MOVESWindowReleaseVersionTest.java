/**************************************************************************************************
 * @(#)MOVESWindowReleaseVersionTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import junit.framework.*;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import gov.epa.otaq.moves.master.gui.MOVESWindow;
import gov.epa.otaq.moves.worker.gui.WorkerWindow;

/**
 * Test Case for the Release version in MOVESWindow and WorkerWindow classes
 *
 * @author		Wesley Faler
 * @version		2013-01-31
**/
public class MOVESWindowReleaseVersionTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public MOVESWindowReleaseVersionTest(String name) {
		super(name);
	}

	/**
	 * Tests case to check current release date in MOVESWindow
	**/
	public void testCase() {
		checkDate(MOVESWindow.MOVES_VERSION, "MOVESWindow");
	}

	/**
	 * Check a date for the correct format
	 * @param dateText date in yyyy/MM/dd format
	 * @param moduleName module being checked
	**/
	void checkDate(String dateText,String moduleName) {
		SimpleDateFormat convertDate = new SimpleDateFormat("yyyy/MM/dd");
		Calendar cal = Calendar.getInstance();
		String today = convertDate.format(cal.getTime());
		assertEquals("Invalid Release/Version number for " + moduleName + ": "
				, today, dateText);
	}
}
