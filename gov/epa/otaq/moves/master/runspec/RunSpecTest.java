/**************************************************************************************************
 * @(#)RunSpecTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.TreeSet;
import junit.framework.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.implementation.general.*;

/**
 * Test Case for the RunSpec class
 *
 * @author		Wesley Faler
 * @version		2012-11-08
**/
public class RunSpecTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public RunSpecTest(String name) {
		super(name);
	}

	/**
	 * Tests filling a RunSpec with a comprehensive set of values. This particularly tests
	 * the TreeSet collections which can only accept Objects that implement the Comparable
	 * interface. Exceptions will be thrown if the test fails.
	**/
	public void testSampleValues() {
		RunSpec sampleRunSpec = new RunSpec();
		setSampleValues(sampleRunSpec);

		assertEqualRunSpecs(sampleRunSpec, sampleRunSpec);

		RunSpec otherRunSpec = new RunSpec();
		setSampleValues(otherRunSpec);

		assertEqualRunSpecs(sampleRunSpec, otherRunSpec);
	}

	/**
	 * Populates the given RunSpec with a wide range of sample data.
	 * @param target The RunSpec to fill with values.
	**/
	public static void setSampleValues(RunSpec target) {
		if(!TimeSpan.isLoaded()) {
			TimeSpan.loadTimeObjects();
		}
		// Must set "semi-realistic" data in any object that gets added to the various
		// RunSpec TreeSet member variables (so as not to break compareTo())
		target.description = "Test Description";
		target.scale = ModelScale.MACROSCALE;

		GeographicSelection testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.COUNTY;
		testSelection.textDescription = "Cuyahoga County";
		testSelection.databaseKey = 39035;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.COUNTY;
		testSelection.textDescription = "Washtenaw County";
		testSelection.databaseKey = 26161;
		target.geographicSelections.add(testSelection);
/*
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.STATE;
		testSelection.textDescription = "Ohio";
		testSelection.databaseKey = 39;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.STATE;
		testSelection.textDescription = "Michigan";
		testSelection.databaseKey = 26;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.LINK;
		testSelection.textDescription = "Link 1";
		testSelection.databaseKey = 3903501;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.ZONE;
		testSelection.textDescription = "Zone 3";
		testSelection.databaseKey = 20003;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.ZONE;
		testSelection.textDescription = "Zone 1";
		testSelection.databaseKey = 20006;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.ZONE;
		testSelection.textDescription = "Zone 2";
		testSelection.databaseKey = 20007;
		target.geographicSelections.add(testSelection);
*/
		// Must be careful to use text date values as opposed to millisecond times. Otherwise, a
		// small amount of precision will be lost when converted to text and back and the equality
		// assertion will fail.
		TimeSpan testTimeSpan = new TimeSpan();
		testTimeSpan.years.add(Integer.valueOf(2001));
		testTimeSpan.months.add(TimeSpan.getMonthByIndex(6));

		for(Iterator i=TimeSpan.allDays.iterator();i.hasNext();) {
			TimeSpan.Day d = (TimeSpan.Day)i.next();
			testTimeSpan.days.add(d);
		}
		testTimeSpan.beginHourID = 1;		// All hours required for OutputTimeStep of Month
		testTimeSpan.endHourID = 24;
		target.timeSpan = testTimeSpan;

		OnRoadVehicleSelection onRoadVehicleSelection = new OnRoadVehicleSelection();
		onRoadVehicleSelection.fuelTypeID = 100;
		onRoadVehicleSelection.fuelTypeDesc = "Fuel BBB";
		onRoadVehicleSelection.sourceTypeID = 200;
		onRoadVehicleSelection.sourceTypeName = "SourceUseType YYY";
		target.onRoadVehicleSelections.add(onRoadVehicleSelection);
		RoadType testRoadType = new RoadType(4,"Urban Restricted Access",Models.ModelCombination.M1);
		target.roadTypes.add(testRoadType);

		PollutantProcessAssociation testAssociation = new PollutantProcessAssociation();
		testAssociation.pollutant = new Pollutant(91, "Total Energy Consumption", true, true);
		testAssociation.emissionProcess = new EmissionProcess(1, "Running Exhaust", "Y", true, true);
		target.pollutantProcessAssociations.add(testAssociation);

		DatabaseSelection testDatabaseSelection = new DatabaseSelection();
		testDatabaseSelection.serverName = "server 1";
		testDatabaseSelection.databaseName = "database 1";
		testDatabaseSelection.userName = DatabaseSelection.SERVER_USER_NAME;
		testDatabaseSelection.password = DatabaseSelection.SERVER_PASSWORD;
		target.databaseSelectionInputSets.add(testDatabaseSelection);
		target.databaseSelectionInputSets.remove(testDatabaseSelection);

		target.geographicOutputDetail = GeographicOutputDetailLevel.STATE;
		target.outputEmissionsBreakdownSelection.emissionProcess = false;
		target.outputDatabase = new DatabaseSelection();
		target.outputDatabase.databaseName = "JUnitTestOUTPUT";
		target.outputDatabase.serverName = "localhost";
		target.outputTimeStep = OutputTimeStep.MONTH;
		target.outputVMTData = true;
//		target.hydrocarbonUnitSystem = HydrocarbonUnitSystem.NMOG;
		target.pmSize = 5;
		target.outputFactors.timeFactorsSelected = true;
		target.outputFactors.massFactorsSelected = true;
		target.outputFactors.timeMeasurementSystem = TimeMeasurementSystem.SECONDS;
		target.outputFactors.massMeasurementSystem = MassMeasurementSystem.KILOGRAMS;
	}

	/**
	 * Asserts that two RunSpecs are equal.
	 * @param runSpecA The first RunSpec object to assert equality against.
	 * @param runSpecB The second RunSpec object to assert equality against.
	**/
	static void assertEqualRunSpecs(final RunSpec runSpecA, final RunSpec runSpecB) {
		assertEquals(runSpecA.description, runSpecB.description);
		assertEquals(runSpecA.scale, runSpecB.scale);
		assertEquals(runSpecA.geographicSelections, runSpecB.geographicSelections);
		// Don't check timeSpan since it has no comparision functionality
		assertEquals(runSpecA.onRoadVehicleSelections, runSpecB.onRoadVehicleSelections);
		assertEquals(runSpecA.offRoadVehicleSelections, runSpecB.offRoadVehicleSelections);
		assertEquals(runSpecA.offRoadVehicleSCCs, runSpecB.offRoadVehicleSCCs);
		assertEquals(runSpecA.roadTypes, runSpecB.roadTypes);
		assertEquals(runSpecA.pollutantProcessAssociations, runSpecB.pollutantProcessAssociations);
		assertEquals(runSpecA.databaseSelectionInputSets, runSpecB.databaseSelectionInputSets);
		assertEquals(runSpecA.inputDatabase, runSpecB.inputDatabase);
		assertEquals(runSpecA.uncertaintyParameters, runSpecB.uncertaintyParameters);
		assertEquals(runSpecA.geographicOutputDetail, runSpecB.geographicOutputDetail);
		assertEquals(runSpecA.outputEmissionsBreakdownSelection,
				runSpecB.outputEmissionsBreakdownSelection);
		assertEquals(runSpecA.outputDatabase, runSpecB.outputDatabase);
		assertEquals(runSpecA.outputTimeStep, runSpecB.outputTimeStep);
		assertEquals(runSpecA.outputVMTData, runSpecB.outputVMTData);
//		assertEquals(runSpecA.hydrocarbonUnitSystem, runSpecB.hydrocarbonUnitSystem);
		assertEquals(runSpecA.scaleInputDatabase, runSpecB.scaleInputDatabase);
		assertEquals(runSpecA.pmSize, runSpecB.pmSize);
		// Don't check outputFactors since it has no comparison functionality
	}

	/**
	 * Compares to treesets and returns if they are equal.
	 * @param setA The first treeset to compare.
	 * @param setB The second treeset to compare.
	**/
	static void assertEqualTreeSetContents(TreeSet setA, TreeSet setB) {
		Iterator iteratorA = setA.iterator();
		Iterator iteratorB = setB.iterator();

		while (iteratorA.hasNext() && iteratorB.hasNext()) {
			Object objectA = iteratorA.next();
			Object objectB = iteratorB.next();

			if(!objectA.equals(objectB)) {
				System.out.println("About to fail");
			}
			assertEquals(objectA, objectB);
		}

		assertEquals(iteratorA.hasNext(), iteratorB.hasNext());
	}
}
