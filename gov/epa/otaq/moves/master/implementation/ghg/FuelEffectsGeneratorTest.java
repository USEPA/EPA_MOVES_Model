/**************************************************************************************************
 * @(#)FuelEffectsGeneratorTest.java
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
 * Test Case for the FuelEffectsGenerator class
 *
 * @author		Wesley Faler
 * @version		2016-10-04
**/
public class FuelEffectsGeneratorTest extends TestCase {
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
	public FuelEffectsGeneratorTest(String name) throws Exception {
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

		if(testDB != null) {
			DatabaseUtilities.executeScript(testDB,new File("database/CreateExecution.sql"));
		}

		// Setup calendar years so that a model year will need 2 ages
		if(testDB != null) {
			TreeSet<Integer> calendarYears = new TreeSet<Integer>();
			int startYear = 2010;
			calendarYears.add(Integer.valueOf(startYear));
			calendarYears.add(Integer.valueOf(startYear+1));
			ExecutionRunSpec.fillYearsAndModelYears(testDB,calendarYears);
		}

		if(inputDB != null && testDB != null) {
			TreeSet<Integer> sourceTypes = DatabaseUtilities.getIntegerSet(inputDB,
					"select sourceTypeID from sourceUseType");
			ExecutionRunSpec.fillSourceTypes(testDB,sourceTypes);
		}
	}

	/** Test loading fuel models **/
	public void testLoadFuelModelIDs() throws Exception {
		FuelEffectsGenerator g = new FuelEffectsGenerator();
		g.db = testDB;
		String calculationEngine = "airtoxicsA";
		g.calculationEngine = calculationEngine;
		g.loadFuelModelIDs();
		assertEquals("Wrong number of fuel models loaded for " + calculationEngine,
				10,g.fuelModelIDs.size());
		assertEquals("Wrong fuel models loaded for " + calculationEngine,
				"1,2,3,4,5,6,7,8,9,10",g.fuelModelIDsCSV);

		calculationEngine = "mtbeA";
		g.calculationEngine = calculationEngine;
		g.loadFuelModelIDs();
		assertEquals("Wrong number of fuel models loaded for " + calculationEngine,
				1,g.fuelModelIDs.size());
		assertEquals("Wrong fuel models loaded for " + calculationEngine,
				"100",g.fuelModelIDsCSV);
	}

	/** Test loading fuel and complex model parameters **/
	public void testLoadParameters() throws Exception {
		FuelEffectsGenerator g = new FuelEffectsGenerator();
		g.db = testDB;
		g.loadFuelParameters();
		g.loadComplexModelParameters();
		assertEquals("Wrong number of fuel parameters loaded",19,g.fuelParameters.size());
		assertEquals("Wrong number of complex model parameters loaded",64,g.cmParameters.size());
	}

	/** Test making the atDifferenceFraction expression **/
	public void testAtDifferenceFraction() throws Exception {
		FuelEffectsGenerator g = new FuelEffectsGenerator();
		g.db = testDB;
		String calculationEngine = "airtoxicsA";
		g.calculationEngine = calculationEngine;
		g.loadFuelModelIDs();
		g.loadFuelParameters();
		g.loadComplexModelParameters();
		FuelEffectsGenerator.ExpressionHolder holder = new FuelEffectsGenerator.ExpressionHolder();
		g.makeAtDifferenceFraction(holder);
		Common.NamedExpressionNode expression =
				(Common.NamedExpressionNode)holder.namedExpressions.get("atDifferenceFraction");
		assertNotNull("Did not create atDifferenceFraction",expression);
	}

	/** Test creating placeholder values for all fuel/cmp combinations **/
	public void testCreateComplexModelParameterVariables() throws Exception {
		FuelEffectsGenerator g = new FuelEffectsGenerator();
		g.db = testDB;
		String calculationEngine = "airtoxicsA";
		g.calculationEngine = calculationEngine;
		g.loadFuelModelIDs();
		g.loadFuelParameters();
		g.loadComplexModelParameters();
		FuelEffectsGenerator.ExpressionHolder holder = new FuelEffectsGenerator.ExpressionHolder();
		g.makeAtDifferenceFraction(holder);

		int expressionCountBefore = holder.namedExpressions.size();
		int variableCountBefore = holder.variables.size();

		g.createComplexModelParameterVariables(holder);
		assertEquals("Wrong number of expressions added",
				g.fuelModelIDs.size()*g.cmParameters.size(),
				holder.namedExpressions.size() - expressionCountBefore);
		assertEquals("Wrong number of variables added",
				3*g.fuelModelIDs.size()*g.cmParameters.size(),
				holder.variables.size()-variableCountBefore);

		g.clearComplexModelParameterVariables(holder); // this should not throw an exception
	}

	private class AirToxicsFuelHandler implements FuelEffectsGenerator.FuelHandler {
		/** ID of temporary fuel formulation added to the database **/
		public int testFuelID = 0;

		/**
		 * Constructor
		 * @param db database to use
		**/
		public AirToxicsFuelHandler(Connection db) {
			String sql = "";
			try {
				sql = "select max(fuelFormulationID) from fuelFormulation";
				testFuelID = 1+(int)SQLRunner.executeScalar(db,sql);
				sql = "insert into fuelFormulation (fuelFormulationID, fuelSubtypeID,"
						+ " 	volToWtPercentOxy, sulfurLevel, RVP, e200, e300,"
						+ " 	aromaticContent, olefinContent, benzeneContent)"
						+ " values (" + testFuelID + ",10,"
						+ " 3.529924,215.96,8.7,50.17562,82.6122,"
						+ " 34.3368,7.0944,1.4)";
				SQLRunner.executeSQL(db,sql);

				sql = "insert into fuelSupply ("
						+ " countyID,fuelYearID,monthGroupID,fuelFormulationID,marketShare)"
						+ " values (-1,-1,0," + testFuelID + ",1.0)";
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to add test fuel formulation",sql);
			}
		}

		/**
		 * Cleanup the test fuel formulation added to the database
		 * @param db database to use
		**/
		public void cleanup(Connection db) {
			if(testFuelID <= 0) {
				return;
			}
			String sql = "delete from fuelFormulation where fuelFormulationID=" + testFuelID;
			try {
				SQLRunner.executeSQL(db,sql);

				sql = "delete from fuelSupply where countyID= -1 and fuelYearID= -1"
						+ " and monthGroupID=0";
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to remove test fuel formulation",sql);
			}
		}

		/**
		 * Get a set of all fuel formulations that exist for a given fuel type
		 * @param fuelTypeID fuel type to be queried
		 * @return set of all fuel formulations for fuelTypeID, never null but may be empty
		 * @throws SQLException if anything goes wrong
		**/
		public TreeSet<Integer> getFuelFormulations(int fuelTypeID) {
			TreeSet<Integer> results = new TreeSet<Integer>();
			results.add(Integer.valueOf(97)); // base fuel for air toxics
			results.add(Integer.valueOf(96)); // base fuel for others
			results.add(Integer.valueOf(98)); // base fuel for others
			results.add(Integer.valueOf(testFuelID)); // test target fuel
			return results;
		}
	}

	/** Test the air toxics calculations **/
	public void testDoAirToxicsCalculations() throws Exception {
		AirToxicsFuelHandler fuelHandler = new AirToxicsFuelHandler(testDB);
		try {
			String sql = "truncate atRatio";
			SQLRunner.executeSQL(testDB,sql);

			FuelEffectsGenerator g = new FuelEffectsGenerator();
			g.db = testDB;
			if(!doSlowTests) {
				g.fuelHandler = fuelHandler;
			}
			g.doAirToxicsCalculations();

			sql = "select atRatio"
					+ " from ATRatio"
					+ " where fuelTypeID=1"
					+ " and fuelFormulationID=" + fuelHandler.testFuelID
					+ " and polProcessID=2001" // benzene, running
					+ " and ageID=0"
					+ " and monthGroupID=1"
					+ " and maxModelYearID=2000";
			double ratio = SQLRunner.executeScalar(g.db,sql);
			assertEquals("ATRatio does not match",((int)(0.0639*10000))/10000.0,((int)(ratio*10000))/10000.0);

			long startMillis = System.currentTimeMillis();
			g.doAirToxicsCalculations();
			long endMillis = System.currentTimeMillis();
			System.out.println("Did 2nd air toxics run with logic to skip existing ratios in "
					+ (endMillis-startMillis) + " ms");
		} finally {
			fuelHandler.cleanup(testDB);
		}
	}

	/** Test the carbon monoxide calculations **/
	public void testDoCOCalculations() throws Exception {
		AirToxicsFuelHandler fuelHandler = new AirToxicsFuelHandler(testDB);
		try {
			String sql = "truncate criteriaRatio";
			SQLRunner.executeSQL(testDB,sql);

			FuelEffectsGenerator g = new FuelEffectsGenerator();
			g.db = testDB;
			if(!doSlowTests) {
				g.fuelHandler = fuelHandler;
			}
			g.doCOCalculations();

			sql = "select ratio"
					+ " from criteriaRatio"
					+ " where fuelTypeID=1"
					+ " and fuelFormulationID=" + fuelHandler.testFuelID
					+ " and polProcessID=201" // CO, running
					+ " and ageID=0"
					+ " and sourceTypeID=21"
					+ " and modelYearID=2010";
			double ratio = SQLRunner.executeScalar(g.db,sql);
			assertEquals("coRatio does not match",((int)(1.7548*10000))/10000.0,((int)(ratio*10000))/10000.0);

			long startMillis = System.currentTimeMillis();
			g.doCOCalculations();
			long endMillis = System.currentTimeMillis();
			System.out.println("Did 2nd CO run with logic to skip existing ratios in "
					+ (endMillis-startMillis) + " ms");
		} finally {
			fuelHandler.cleanup(testDB);
		}
	}

	/** Test rewriteCmpExpressionToIncludeStdDev **/
	public void testRewriteCmpExpressionToIncludeStdDev() {
		String[] names = {
			"Oxygen", "Sulfur", "RVP", "E200", "E300", "Aromatics", "Benzene", "Olefins",
			"MTBE", "ETBE", "TAME", "Ethanol", "Intercept", "Hi", "T50", "T90"
		};
		for(int i=0;i<names.length;i++) {
			String t = names[i];
			// "(fp.E200-fp_E200.center)" to "((fp.E200-fp_E200.center)/fp_E200.stddev)"
			String original = "(cmp.coeff*(fp."+t+"-fp_"+t+".center))";
			String expected = "(cmp.coeff*((fp."+t+"-fp_"+t+".center)/fp_"+t+".stddev))";
			String got = FuelEffectsGenerator.rewriteCmpExpressionToIncludeStdDev(original);
			if(!got.equals(expected)) {
				System.out.println("original: " + original);
				System.out.println("expected: " + expected);
				System.out.println("got: " + got);
			}
			assertEquals("Single rewrite failed",expected,got);

			original = "(cmp.coeff*(fp."+t+"-fp_"+t+".center)^2)";
			expected = "(cmp.coeff*((fp."+t+"-fp_"+t+".center)/fp_"+t+".stddev)^2)";
			got = FuelEffectsGenerator.rewriteCmpExpressionToIncludeStdDev(original);
			if(!got.equals(expected)) {
				System.out.println("original: " + original);
				System.out.println("expected: " + expected);
				System.out.println("got: " + got);
			}
			assertEquals("Single rewrite failed",expected,got);

			// "(cmp.coeff*(fp.Sulfur-fp_Sulfur.center)*(fp.Aromatics-fp_Aromatics.center))"
			// to "(cmp.coeff*(fp.Sulfur-fp_Sulfur.center)*(fp.Aromatics-fp_Aromatics.center))"
			original = "(cmp.coeff*(fp."+t+"-fp_"+t+".center)*(fp.E200-fp_E200.center))";
			expected = "(cmp.coeff*((fp."+t+"-fp_"+t+".center)/fp_"+t+".stddev)"
					+ "*((fp.E200-fp_E200.center)/fp_E200.stddev))";
			got = FuelEffectsGenerator.rewriteCmpExpressionToIncludeStdDev(original);
			if(!got.equals(expected)) {
				System.out.println("original: " + original);
				System.out.println("expected: " + expected);
				System.out.println("got: " + got);
			}
			assertEquals("Multiple rewrite failed",expected,got);
		}
		// Test more complex expression
		String original = "(cmp.coeff*(if(and(fp.Oxygen>3.5,maxModelYear<=2001),3.5,fp.Oxygen)-fp_Oxygen.center))";
		String expected = "(cmp.coeff*((if(and(fp.Oxygen>3.5,maxModelYear<=2001),3.5,fp.Oxygen)-fp_Oxygen.center)/fp_Oxygen.stddev))";
		String got = FuelEffectsGenerator.rewriteCmpExpressionToIncludeStdDev(original);
		if(!got.equals(expected)) {
			System.out.println("original: " + original);
			System.out.println("expected: " + expected);
			System.out.println("got: " + got);
		}
		assertEquals("Complex rewrite failed",expected,got);

		original = "(cmp.coeff*(if(and(fp.Oxygen>3.5,maxModelYear<=2001),3.5,fp.Oxygen)-fp_Oxygen.center)^2)";
		expected = "(cmp.coeff*((if(and(fp.Oxygen>3.5,maxModelYear<=2001),3.5,fp.Oxygen)-fp_Oxygen.center)/fp_Oxygen.stddev)^2)";
		got = FuelEffectsGenerator.rewriteCmpExpressionToIncludeStdDev(original);
		if(!got.equals(expected)) {
			System.out.println("original: " + original);
			System.out.println("expected: " + expected);
			System.out.println("got: " + got);
		}
		assertEquals("Complex rewrite failed",expected,got);
	}

	/** Test the predictive model HC calculations **/
	public void testDoHCCalculations() throws Exception {
		AirToxicsFuelHandler fuelHandler = new AirToxicsFuelHandler(testDB);
		try {
			String sql = "truncate criteriaRatio";
			SQLRunner.executeSQL(testDB,sql);

			FuelEffectsGenerator g = new FuelEffectsGenerator();
			g.db = testDB;
			if(!doSlowTests) {
				g.fuelHandler = fuelHandler;
			}
			g.doHCCalculations();

			sql = "select ratio"
					+ " from criteriaRatio"
					+ " where fuelTypeID=1"
					+ " and fuelFormulationID=" + fuelHandler.testFuelID
					+ " and polProcessID=101" // HC, running
					+ " and ageID=0"
					+ " and sourceTypeID=21"
					+ " and modelYearID=2010";
			double ratio = SQLRunner.executeScalar(g.db,sql);
			assertEquals("HC ratio does not match",((int)(1.4634*10000))/10000.0,((int)(ratio*10000))/10000.0);

			long startMillis = System.currentTimeMillis();
			g.doHCCalculations();
			long endMillis = System.currentTimeMillis();
			System.out.println("Did 2nd HC run with logic to skip existing ratios in "
					+ (endMillis-startMillis) + " ms");
		} finally {
			fuelHandler.cleanup(testDB);
		}
	}

	/** Test the predictive model NOx calculations **/
	public void testDoNOxCalculations() throws Exception {
		AirToxicsFuelHandler fuelHandler = new AirToxicsFuelHandler(testDB);
		try {
			String sql = "truncate criteriaRatio";
			SQLRunner.executeSQL(testDB,sql);

			FuelEffectsGenerator g = new FuelEffectsGenerator();
			g.db = testDB;
			if(!doSlowTests) {
				g.fuelHandler = fuelHandler;
			}
			g.doNOxCalculations();

			sql = "select ratio"
					+ " from criteriaRatio"
					+ " where fuelTypeID=1"
					+ " and fuelFormulationID=" + fuelHandler.testFuelID
					+ " and polProcessID=301" // NOx, running
					+ " and ageID=0"
					+ " and sourceTypeID=21"
					+ " and modelYearID=2010";
			double ratio = SQLRunner.executeScalar(g.db,sql);
			assertEquals("NOx ratio does not match",((int)(2.1019*10000))/10000.0,((int)(ratio*10000))/10000.0);

			long startMillis = System.currentTimeMillis();
			g.doNOxCalculations();
			long endMillis = System.currentTimeMillis();
			System.out.println("Did 2nd NOx run with logic to skip existing ratios in "
					+ (endMillis-startMillis) + " ms");
		} finally {
			fuelHandler.cleanup(testDB);
		}
	}

	private class MTBEFuelHandler implements FuelEffectsGenerator.FuelHandler {
		/** ID of temporary fuel formulation added to the database **/
		public int testFuelID = 0;

		/**
		 * Constructor
		 * @param db database to use
		**/
		public MTBEFuelHandler(Connection db) {
			String sql = "";
			try {
				sql = "select max(fuelFormulationID) from fuelFormulation";
				testFuelID = 1+(int)SQLRunner.executeScalar(db,sql);
				sql = "insert into fuelFormulation (fuelFormulationID, fuelSubtypeID,"
						+ " 	volToWtPercentOxy, sulfurLevel, RVP, e200, e300,"
						+ " 	aromaticContent, olefinContent, benzeneContent, MTBEVolume)"
						+ " values (" + testFuelID + ",10,"
						+ " 3.529924,215.96,8.7,50.17562,82.6122,"
						+ " 34.3368,7.0944,1.4,10)";
				SQLRunner.executeSQL(db,sql);

				sql = "insert into fuelSupply ("
						+ " countyID,fuelYearID,monthGroupID,fuelFormulationID,marketShare)"
						+ " values (-1,-1,0," + testFuelID + ",1.0)";
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to add test fuel formulation",sql);
			}
		}

		/**
		 * Cleanup the test fuel formulation added to the database
		 * @param db database to use
		**/
		public void cleanup(Connection db) {
			if(testFuelID <= 0) {
				return;
			}
			String sql = "delete from fuelFormulation where fuelFormulationID=" + testFuelID;
			try {
				SQLRunner.executeSQL(db,sql);

				sql = "delete from fuelSupply where countyID= -1 and fuelYearID= -1"
						+ " and monthGroupID=0";
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to remove test fuel formulation",sql);
			}
		}

		/**
		 * Get a set of all fuel formulations that exist for a given fuel type
		 * @param fuelTypeID fuel type to be queried
		 * @return set of all fuel formulations for fuelTypeID, never null but may be empty
		 * @throws SQLException if anything goes wrong
		**/
		public TreeSet<Integer> getFuelFormulations(int fuelTypeID) {
			TreeSet<Integer> results = new TreeSet<Integer>();
			results.add(Integer.valueOf(testFuelID)); // test target fuel
			return results;
		}
	}

	/** Test the MTBE calculations **/
	public void testMTBECalculations() throws Exception {
		MTBEFuelHandler fuelHandler = new MTBEFuelHandler(testDB);
		try {
			String sql = "drop table if exists MTBERatio";
			SQLRunner.executeSQL(testDB,sql);

			FuelEffectsGenerator g = new FuelEffectsGenerator();
			g.db = testDB;
			if(!doSlowTests) {
				g.fuelHandler = fuelHandler;
			}
			g.doMTBECalculations();

			sql = "select mtbeRatio"
					+ " from mtbeRatio"
					+ " where fuelTypeID=1"
					+ " and fuelFormulationID=" + fuelHandler.testFuelID
					+ " and polProcessID=2201"; // MTBE, running
			double ratio = SQLRunner.executeScalar(g.db,sql);
			assertEquals("mtbeRatio does not match",((int)(0.013248*10000))/10000.0,((int)(ratio*10000))/10000.0);

			long startMillis = System.currentTimeMillis();
			g.doMTBECalculations();
			long endMillis = System.currentTimeMillis();
			System.out.println("Did 2nd MTBE run with logic to skip existing ratios in "
					+ (endMillis-startMillis) + " ms");
		} finally {
			fuelHandler.cleanup(testDB);
		}
	}

	/** Test the getCSV() function **/
	public void testGetCSV() {
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(Integer.valueOf(5));
		set.add(Integer.valueOf(6));
		set.add(Integer.valueOf(1));
		set.add(Integer.valueOf(2));

		String got =  FuelEffectsGenerator.getCSV(set);
		assertEquals("getCSV failed","1,2,5,6",got);
	}

	/** Test the getPolProcessIDsNotAlreadyDone() function **/
	public void testGetPolProcessIDsNotAlreadyDone() {
		TreeSet<Integer> idsAlreadyDone = new TreeSet<Integer>();
		String got = FuelEffectsGenerator.getPolProcessIDsNotAlreadyDone("101,102",idsAlreadyDone);
		assertEquals("getPolProcessIDsNotAlreadyDone failed","101,102",got);

		got = FuelEffectsGenerator.getPolProcessIDsNotAlreadyDone("101,102",idsAlreadyDone);
		assertNull("getPolProcessIDsNotAlreadyDone didn't return null when nothing unique was presented",got);

		got = FuelEffectsGenerator.getPolProcessIDsNotAlreadyDone("201,202,101,103",idsAlreadyDone);
		assertEquals("getPolProcessIDsNotAlreadyDone failed","201,202,103",got);
	}

	/** Test HCSpeciationCalculator.buildOxyThreshCase() **/
	public void testBuildOxyThreshCase() throws Exception {
		String caseStatement = HCSpeciationCalculator.buildOxyThreshCase(testDB);
		String sql = "select fuelFormulationID, (" + caseStatement + ") as oxyThreshID"
				+ " from fuelFormulation";
		System.out.println(sql);
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			TreeSet<Integer> ids = new TreeSet<Integer>();
			query.open(testDB,sql);
			while(query.rs.next()) {
				double idDouble = query.rs.getDouble(2);
				int id = (int)idDouble;
				assertEquals("oxyThreshID not an integer",(double)id,idDouble);
				Integer idInt = Integer.valueOf(id);
				if(!ids.contains(idInt)) {
					ids.add(idInt);
				}
			}
			query.close();

			sql = "select count(*) from oxyThreshName";
			int count = (int)SQLRunner.executeScalar(testDB,sql);
			assertEquals("Not all oxyThreshIDs found in fuelFormulation",count,ids.size());
		} finally {
			query.onFinally();
		}
	}

	/** Test the creation of general fuel ratios **/
	public void testDoGeneralFuelRatio() throws Exception {
		MTBEFuelHandler fuelHandler = new MTBEFuelHandler(testDB);
		String sql = "";
		try {
			sql = "truncate GeneralFuelRatio";
			SQLRunner.executeSQL(testDB,sql);

			sql = "insert into GeneralFuelRatioExpression ("
					+ " fuelTypeID, polProcessID, minModelYearID, maxModelYearID,"
					+ " minAgeID, maxAgeID,sourceTypeID,fuelEffectRatioExpression,"
					+ " fuelEffectRatioGPAExpression)"
					+ " values (1,-101,1960,2060,0,30,0,"
					+ " 'MTBEVolume+7','MTBEVolume*2')";
			SQLRunner.executeSQL(testDB,sql);

			FuelEffectsGenerator g = new FuelEffectsGenerator();
			g.db = testDB;
			if(!doSlowTests) {
				g.fuelHandler = fuelHandler;
			}
			g.doGeneralFuelRatio();

			sql = "select fuelEffectRatio"
					+ " from GeneralFuelRatio"
					+ " where fuelTypeID=1"
					+ " and fuelFormulationID=" + fuelHandler.testFuelID
					+ " and polProcessID= -101";
			double ratio = SQLRunner.executeScalar(g.db,sql);
			assertEquals("fuelEffectRatio does not match",((int)(17.0*10000))/10000.0,((int)(ratio*10000))/10000.0);

			sql = "select fuelEffectRatioGPA"
					+ " from GeneralFuelRatio"
					+ " where fuelTypeID=1"
					+ " and fuelFormulationID=" + fuelHandler.testFuelID
					+ " and polProcessID= -101";
			ratio = SQLRunner.executeScalar(g.db,sql);
			assertEquals("fuelEffectRatioGPA does not match",((int)(20.0*10000))/10000.0,((int)(ratio*10000))/10000.0);

			long startMillis = System.currentTimeMillis();
			g.doGeneralFuelRatio();
			long endMillis = System.currentTimeMillis();
			System.out.println("Did 2nd General run with logic to skip existing ratios in "
					+ (endMillis-startMillis) + " ms");
		} finally {
			fuelHandler.cleanup(testDB);

			sql = "delete from GeneralFuelRatioExpression"
					+ " where polProcessID= -101";
			SQLRunner.executeSQL(testDB,sql);
		}
	}

	/** Invokes the main application. **/
	public static void main(String[] args) {
		Configuration.allowGUI = false;

		if(!SystemConfiguration.getTheSystemConfiguration().didLoad) {
			System.exit(1);
		}

		DatabaseSelection executionDatabase = new DatabaseSelection();
		executionDatabase.databaseName = "MOVESExecution";
		Connection db = null;
		String sql = "";
		try {
			db = executionDatabase.openConnection();

			ArrayList<FuelEffectsGenerator.FuelUsageEntry> highEthanolUsages = FuelEffectsGenerator.cloneEthanolFuelsForRegions(db);
			sql = "alter table fuelFormulation add altRVP float null";
			SQLRunner.executeSQL(db,sql);
			sql = "update fuelFormulation set altRVP=RVP";
			SQLRunner.executeSQL(db,sql);
			FuelEffectsGenerator.alterHighEthanolFuelProperties(db,highEthanolUsages);
		} catch(Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		} finally {
			if(db != null) {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
		}
	}
}
