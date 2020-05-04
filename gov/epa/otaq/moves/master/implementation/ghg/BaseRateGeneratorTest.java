/**************************************************************************************************
 * @(#)BaseRateGeneratorTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.common.expression.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.io.*;
import java.sql.*;
import java.util.*;

import junit.framework.*;

/**
 * Test Case for the BaseRateGenerator's support classes
 *
 * @author		Wesley Faler
 * @version		2014-07-22
**/
public class BaseRateGeneratorTest extends TestCase {
	/** Connection to the execution database **/
	Connection executionDB = null;

	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public BaseRateGeneratorTest(String name) throws Exception {
		super(name);

		Configuration.allowGUI = false;

		if(!SystemConfiguration.theSystemConfiguration.didLoad) {
			Logger.log(LogMessageCategory.ERROR, "The system configuration failed to load.");
			System.exit(0);
			return;
		}
		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.databaseName = "MOVESExecution";
		executionDB = dbSelection.openConnectionOrNull();
	}

	/** Tests the BaseRateByAgeHelper class **/
	public void testBaseRateByAgeHelper() throws Exception {
		int processID = 1;
		int yearID = 2030;
		String tableName = "Test_BaseRateByAge_" + processID + "_" + yearID;
		String sql = "create table if not exists " + tableName + " like BaseRateByAge";
		SQLRunner.executeSQL(executionDB,sql);
		sql = "truncate table " + tableName;
		SQLRunner.executeSQL(executionDB,sql);

		BaseRateByAgeHelper.isTest = true;
		BaseRateByAgeHelper h = new BaseRateByAgeHelper(executionDB);
		int howMany = 0;

		BaseRateByAgeHelper.Flags flags = new BaseRateByAgeHelper.Flags();
		flags.keepOpModeID = false;
		flags.useAvgSpeedBin = false;
		flags.useAvgSpeedFraction = true;
		flags.useSumSBD = true;
		flags.useSumSBDRaw = false;

		BaseRateByAgeHelper.Context context = new BaseRateByAgeHelper.Context();
		context.processID = processID;
		context.yearID = yearID;
		context.sourceTypeID = 21;
		context.polProcessID = 101;
		context.roadTypeID = 0; // no road type
		howMany += h.process(context,flags);

		context.polProcessID = 201;
		howMany += h.process(context,flags);

		assertEquals("Wrong number of records written",/*62*/23808,howMany);
	}
}
