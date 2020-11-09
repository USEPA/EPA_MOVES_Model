/**************************************************************************************************
 * @(#)GREETInterfaceTest.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.sql.*;
import java.io.*;
import java.util.*;
import junit.framework.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.implementation.general.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Test Case for the GREETInterface class
 *
 * @author		Wesley Faler
 * @version		2012-11-08
**/
public class GREETInterfaceTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public GREETInterfaceTest(String name) {
		super(name);
	}
	
	/** sample runspec to test GREETInterface **/
	RunSpec runspec = new RunSpec();

	/**
	 * Tests getYearAndFuelInfo()
	**/
	public void testGetYearAndFuelInfo() {
		runspec = new RunSpec();
		setSampleValues(runspec);
		GREETInterface testgi = new GREETInterface(runspec); 
	}

	/**
	 * Tests updateWellToPumpRates()
	**/
	public void testUpdateWellToPumpRates() {
	}

	/**
	 * Tests updateManufactureDisposalRates()
	**/
	public void testUpdateManufactureDisposalRates(RunSpec runspecToUse) {
	}
	
	/**
	 * Tests WriteWellToPump()
	**/
	public void testWriteWellToPump() {
	}

	/**
	 * Tests WriteManufactureDisposal()
	**/
	public void testWriteManufactureDisposal() {
	}

	/**
	 * Populates the given RunSpec with a wide range of sample data.
	 * @param target RunSpec object that should be examined for years, fuel types,
	 * and model years.
	**/
	static void setSampleValues(RunSpec target) {
		TimeSpan.loadTimeObjects();

		target.description = "Test Description";
		target.scale = ModelScale.MACROSCALE;
		GeographicSelection testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.COUNTY;
		testSelection.textDescription = "Cuyahoga";
		testSelection.databaseKey = 4004;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.COUNTY;
		testSelection.textDescription = "Washtenaw";
		testSelection.databaseKey = 4005;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.STATE;
		testSelection.textDescription = "Ohio";
		testSelection.databaseKey = 40011;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.STATE;
		testSelection.textDescription = "Michigan";
		testSelection.databaseKey = 40010;
		target.geographicSelections.add(testSelection);
		testSelection = new GeographicSelection();
		testSelection.type = GeographicSelectionType.LINK;
		testSelection.textDescription = "Link 1";
		testSelection.databaseKey = 1001;
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

		// Must be careful to use text date values as opposed to millisecond times. Otherwise, a
		// small amount of precision will be lost when converted to text and back and the equality
		// assertion will fail.
		TimeSpan testTimeSpan = new TimeSpan();
		testTimeSpan.years.add(Integer.valueOf(2001));
		TimeSpan.Month m = TimeSpan.getMonthByIndex(6);
		testTimeSpan.months.add(m);

		for(Iterator i=TimeSpan.allDays.iterator();i.hasNext();) {
			TimeSpan.Day d = (TimeSpan.Day)i.next();
			testTimeSpan.days.add(d);
		}
		testTimeSpan.beginHourID = 12;
		testTimeSpan.endHourID = 16;
		target.timeSpan = testTimeSpan;

		OnRoadVehicleSelection onRoadVehicleSelection = new OnRoadVehicleSelection();
		onRoadVehicleSelection.fuelTypeID = 1;
		onRoadVehicleSelection.fuelTypeDesc = "Fuel BBB";
		
		onRoadVehicleSelection.sourceTypeID = 200;
		onRoadVehicleSelection.sourceTypeName = "SourceUseType YYY";
		target.onRoadVehicleSelections.add(onRoadVehicleSelection);
		RoadType testRoadType = new RoadType(1,"Sample Road Type",Models.ModelCombination.M1);
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
		target.pmSize = 5;
		target.outputFactors.timeFactorsSelected = true;
		target.outputFactors.massFactorsSelected = true;
		target.outputFactors.timeMeasurementSystem = TimeMeasurementSystem.SECONDS;
		target.outputFactors.massMeasurementSystem = MassMeasurementSystem.KILOGRAMS;
	}
}
