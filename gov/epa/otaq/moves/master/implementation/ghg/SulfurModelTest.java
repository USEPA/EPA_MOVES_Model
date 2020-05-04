/**************************************************************************************************
 * @(#)SulfurModelTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.common.expression.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.sql.*;
import java.util.*;
import java.io.*;
import junit.framework.*;

/**
 * Test Case for the Moble6 sulfur model
 *
 * @author		Ed Campbell
 * @version		2011-10-13
**/
public class SulfurModelTest extends TestCase {
	/** True when tests should exercise all aspects, taking 20 or more minutes **/
	public static boolean doSlowTests = false;

	/** Connection to the test database, such as MOVES912 **/
	Connection testDB = null;
	/** Connection to the input database, such as MOVESDB20090206 **/
	Connection inputDB = null;

	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public SulfurModelTest(String name) throws Exception {
		super(name);

		if(!SystemConfiguration.theSystemConfiguration.didLoad) {
			Logger.log(LogMessageCategory.ERROR, "The system configuration failed to load.");
			System.exit(0);
			return;
		}
		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.databaseName = "MOVES912";
		testDB = dbSelection.openConnectionOrNull();

		dbSelection.databaseName = "MOVESDBDraft2009";
		inputDB = dbSelection.openConnectionOrNull();

		DatabaseUtilities.executeScript(testDB,new File("database/CreateExecution.sql"));
	}

	/** Tests sulfur model SQL Script **/
	public void testScriptFile() throws Exception {
		assertNotNull("testDB is null",testDB);
		assertNotNull("inputDB is null",inputDB);

		int startYear = 2010;
		int endYear = 2010;
		int noFormulationIDs = 3;
		int noSourceTypes = 13;
		int noInputRecords = 6;
		int noModelYears = (endYear - startYear) + 1;

		// Setup calendar years so that a model year will need 2 ages
		TreeSet<Integer> calendarYears = new TreeSet<Integer>();
		calendarYears.add(new Integer(startYear));
		calendarYears.add(new Integer(startYear+1));
		ExecutionRunSpec.fillYearsAndModelYears(testDB,calendarYears);

		TreeSet<Integer> sourceTypes = DatabaseUtilities.getIntegerSet(inputDB,
				"select sourceTypeID from sourceUseType");
		ExecutionRunSpec.fillSourceTypes(testDB,sourceTypes);

		String[] setupStatements = {
			"drop table if exists testSulfurInputRatio",
			"create table if not exists testSulfurInputRatio ("
					+ " fuelTypeID int not null,"
					+ " fuelFormulationID int not null,"
					+ " baseFuelFormulationID int not null,"
					+ " polProcessID int not null,"
					+ " pollutantID int not null,"
					+ " processID int not null,"
					+ " modelYearGroupID int not null,"
					+ " minModelYearID int not null,"
					+ " maxModelYearID int not null,"
					+ " ageID int not null,"
					+ " ratioNoSulfur double"
					+ " )",
			"drop table if exists testSulfurOutputRatio",
			"create table if not exists testSulfurOutputRatio ("
					+ " fuelTypeID int not null,"
					+ " fuelFormulationID int not null,"
					+ " polProcessID int not null,"
					+ " pollutantID int not null,"
					+ " processID int not null,"
					+ " sourceTypeID int not null,"
					+ " modelYearID int not null,"
					+ " ageID int not null,"
					+ " ratio double null,"
					+ " ratioGPA double null,"
					+ " ratioNoSulfur double null"
					+ " )",
			//Populate Input Ratio data
			"insert into testSulfurInputRatio (fuelTypeID,"
					+ " fuelFormulationID,"
					+ " baseFuelFormulationID,"
					+ " polProcessID,"
					+ " pollutantID,"
					+ " processID,"
					+ " modelYearGroupID,"
					+ " minModelYearID,"
					+ " maxModelYearID,"
					+ " ageID,"
					+ " ratioNoSulfur)"
					+ " values "
					+ " (1, 10, 98, 101, 1, 1, " + startYear + "" + endYear + ", " + startYear + ", " + endYear + ", 0, .9),"
					+ " (1, 10, 98, 301, 3, 1, " + startYear + "" + endYear + ", " + startYear + ", " + endYear + ", 1, .9),"
					// Base fuel 98
					+ " (1, 98, 98, 101, 1, 1, " + startYear + "" + endYear + ", " + startYear + ", " + endYear + ", 0, .9),"
					+ " (1, 98, 98, 301, 3, 1, " + startYear + "" + endYear + ", " + startYear + ", " + endYear + ", 1, .9),"
					// Base fuel 99
					+ " (1, 99, 99, 101, 1, 1, " + startYear + "" + endYear + ", " + startYear + ", " + endYear + ", 0, .9),"
					+ " (1, 99, 99, 301, 3, 1, " + startYear + "" + endYear + ", " + startYear + ", " + endYear + ", 1, .9)",
			"alter table testSulfurInputRatio add key(processID, pollutantID, fuelFormulationID, minModelYearID, maxModelYearID, ageID)"
		};
		String sql = "";
		for(int i=0;i<setupStatements.length;i++) {
			sql = setupStatements[i];
			SQLRunner.executeSQL(testDB,sql);
		}

		FuelEffectsGenerator.checkSulfurModelInput(testDB,"testSulfurInputRatio");
		FuelEffectsGenerator.runSulfurModel(testDB,"testSulfurInputRatio","testSulfurOutputRatio",false);
		FuelEffectsGenerator.checkSulfurModelOutput(testDB,"testSulfurOutputRatio");

		//Test Cases
		sql = "select count(*) as inputCount from testSulfurInputRatio";
		int inputCount = (int)SQLRunner.executeScalar(testDB,sql);
		assertEquals("Invalid number of Results in Input table",
				noInputRecords, inputCount);

		int halfOutputCount = (noModelYears * inputCount * noSourceTypes) / 2;
		int checkFuelFormulationsCount = (noModelYears * inputCount * noSourceTypes) / noFormulationIDs;

		sql = "select count(*) as outputCount from testSulfurOutputRatio";
		int outputCount = (int)SQLRunner.executeScalar(testDB,sql);
		assertEquals("Wrong number of results",
				inputCount * noSourceTypes, outputCount);

		sql = "select count(*) as fuelFormulationCount, fuelFormulationID from testSulfurOutputRatio"
			 	+ " group by fuelFormulationID having count(*) != " + checkFuelFormulationsCount;
		int fuelFormulationCount = (int)SQLRunner.executeScalar(testDB,sql);
		assertEquals("Un-Even amount of Fuel Formulation IDs in results table",
				0, fuelFormulationCount);

		sql = "select count(distinct fuelFormulationID) as distinctFuelFormulationCount from testSulfurOutputRatio";
		int distinctFuelFormulationCount = (int)SQLRunner.executeScalar(testDB,sql);
		assertEquals("Invalid amount of Fuel Formulation IDs in results table",
				noFormulationIDs, distinctFuelFormulationCount);

		sql = "select count(*) as blankRatioCount from testSulfurOutputRatio"
				+ " where ratio < 0 or ratioGPA < 0 or ratio is null or ratioGPA is null";
		int blankRatioCount = (int)SQLRunner.executeScalar(testDB,sql);
		assertEquals("There are null values or values lower than 0 for Ratio or RatioGPA",
				0, blankRatioCount);

		sql = "select count(*) as highRatioCount from testSulfurOutputRatio"
				+ " where ratio > ratioGPA";
		int highRatioCount = (int)SQLRunner.executeScalar(testDB,sql);
		assertEquals("There are entries where Ratio > RatioGPA", 0, highRatioCount);

		sql = "select count(*) as polProcessIDCount, polProcessID from testSulfurOutputRatio"
				+ " group by polProcessID having count(*) != " + halfOutputCount;
		int polProcessIDCount = (int)SQLRunner.executeScalar(testDB,sql);
		assertEquals("Un-Even amount of polProcess IDs",
				0, polProcessIDCount);
	}
}
