/**************************************************************************************************
 * @(#)TimeSpanTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import junit.framework.*;
import java.sql.*;
import java.util.*;

/**
 * Test Case for the TimeSpan class
 *
 * @author		wgfaler
 * @version		2009-02-09
**/
public class TimeSpanTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public TimeSpanTest(String name) {
		super(name);
	}

	/** General tests **/
	public void testTimeSpan() {
		TimeSpan t = new TimeSpan();

		Connection executionConnection = null;
		try {
			executionConnection = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.EXECUTION);

			TimeSpan.loadTimeObjects();

			t.years.add(Integer.valueOf(2003));
			TimeSpan.Month m = TimeSpan.getMonthByIndex(11);
			assertNotNull(m);
			t.months.add(m);

			TimeSpan.Day d = TimeSpan.getDayByIndex(1);
			assertNotNull(d);
			t.days.add(d);

			t.beginHourID = 1;
			t.endHourID = 24;
	
			assertTrue(t.hasMonths());
			assertTrue(t.hasDays());
			assertTrue(t.hasHours());

			assertTrue(!t.hasAllMonths());
			assertTrue(!t.hasAllDays(executionConnection));
			assertTrue(t.hasAllHours());

			for(Iterator i=TimeSpan.allDays.iterator();i.hasNext();) {
				d = (TimeSpan.Day)i.next();
				t.days.add(d);
			}
			assertTrue(t.hasAllDays(executionConnection));
	
			for(Iterator i=TimeSpan.allMonths.iterator();i.hasNext();) {
				m = (TimeSpan.Month)i.next();
				t.months.add(m);
			}
			assertTrue(t.hasAllMonths());
		} catch (Exception e) {
			// Nothing to do here
		} finally {
			if(executionConnection != null) {
				DatabaseConnectionManager.checkInConnection(
						MOVESDatabaseType.EXECUTION, executionConnection);
			}
		}
	}
}
