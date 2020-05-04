/**************************************************************************************************
 * @(#)RefineryModelTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.general;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import java.util.*;
import java.sql.*;

/**
 * Test the RefineryModel class.
 *
 * @author		Wesley Faler
 * @version		2015-05-21
**/
public class RefineryModelTest {
	public static void main(String args[]) {
		RefineryModel.isTest = true;
		RefineryModelTest a = new RefineryModelTest();
		try {
			a.testUpdates();
		} catch(Exception e) {
			System.out.println(e.toString());
		}
	}
	
	void testUpdates() throws Exception {
		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.databaseName = "movesdb20121012";
		Connection db = dbSelection.openConnection();
		try {
			RefineryModel rm = new RefineryModel(db,false);
			ArrayList<String> messages = new ArrayList<String>();
			RefineryModel.FuelFormulation ff = new RefineryModel.FuelFormulation();
			ff.regionID = 1;
			ff.fuelYearID = 2013;
			ff.monthGroupID = 7;
			ff.fuelFormulationID = 0;
			ff.fuelTypeID = 1;
			ff.fuelSubtypeID = 10;
			ff.RVP = 6.9;
			ff.sulfurLevel = 30;
			ff.ETOHVolume = 2; // chosen as non-zero to test mid-range start and end points
			ff.MTBEVolume = 0;
			ff.ETBEVolume = 0;
			ff.TAMEVolume = 0;
			ff.aromaticContent = 26.1;
			ff.olefinContent = 5.6;
			ff.benzeneContent = 1;
			ff.e200 = 41.09;
			ff.e300 = 83.09;
			ff.volToWtPercentOxy = 0;
			ff.BioDieselEsterVolume = 0;
			ff.CetaneIndex = 0;
			ff.PAHContent = 0;
			ff.T50 = 218;
			ff.T90 = 329;
	
			String adjustedParameter = "ETOH";
			double targetValue = 16;
			System.out.println("Changing " + adjustedParameter + " from " + ff.getProperty(adjustedParameter) + " to " + targetValue);
			ArrayList<RefineryModel.FuelWizardProperty> adjustments = rm.loadFuelWizardProperties(db,adjustedParameter,messages);
			RefineryModel.FuelFormulation nf = ff.clone();
			rm.updateFuel(nf,adjustments,adjustedParameter,targetValue,messages);
			nf.calculateProperties();
			printFuelChanges(messages,ff,nf);

			// Change the value back to the original value to assess impact
			targetValue = ff.getProperty(adjustedParameter);
			System.out.println("Changing " + adjustedParameter + " from " + nf.getProperty(adjustedParameter) + " to original " + targetValue);
			messages.clear();
			rm.updateFuel(nf,adjustments,adjustedParameter,targetValue,messages);
			nf.calculateProperties();
			printFuelChanges(messages,ff,nf);
		} finally {
			if(db != null) {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
		}
	}

	/**
	 * Print messages and fuel property changes.
	 * @param messages list of messages to be shown.
	 * @param ff input fuel formulation.
	 * @param nf new fuel formulation after changes.
	**/
	void printFuelChanges(ArrayList<String> messages, RefineryModel.FuelFormulation ff, RefineryModel.FuelFormulation nf) {
		for(String s : messages) {
			System.out.println(s);
		}
		System.out.println("RVP: "+ff.RVP+"  "+nf.RVP);
		System.out.println("sulfurLevel: "+ff.sulfurLevel+"  "+nf.sulfurLevel);
		System.out.println("ETOHVolume: "+ff.ETOHVolume+"  "+nf.ETOHVolume);
		System.out.println("MTBEVolume: "+ff.MTBEVolume+"  "+nf.MTBEVolume);
		System.out.println("ETBEVolume: "+ff.ETBEVolume+"  "+nf.ETBEVolume);
		System.out.println("TAMEVolume: "+ff.TAMEVolume+"  "+nf.TAMEVolume);
		System.out.println("aromaticContent: "+ff.aromaticContent+"  "+nf.aromaticContent);
		System.out.println("olefinContent: "+ff.olefinContent+"  "+nf.olefinContent);
		System.out.println("benzeneContent: "+ff.benzeneContent+"  "+nf.benzeneContent);
		System.out.println("e200: "+ff.e200+"  "+nf.e200);
		System.out.println("e300: "+ff.e300+"  "+nf.e300);
		System.out.println("volToWtPercentOxy: "+ff.volToWtPercentOxy+"  "+nf.volToWtPercentOxy);
		System.out.println("BioDieselEsterVolume: "+ff.BioDieselEsterVolume+"  "+nf.BioDieselEsterVolume);
		System.out.println("CetaneIndex: "+ff.CetaneIndex+"  "+nf.CetaneIndex);
		System.out.println("PAHContent: "+ff.PAHContent+"  "+nf.PAHContent);
		System.out.println("T50: "+ff.T50+"  "+nf.T50);
		System.out.println("T90: "+ff.T90+"  "+nf.T90);
	}
}
