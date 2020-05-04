/**************************************************************************************************
 * @(#)ImporterInstantiator.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import gov.epa.otaq.moves.common.*;
import java.util.*;

/**
 * Factory for all importers, either by name or by XML node type.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @version		2015-05-21
**/
public class ImporterInstantiator {
	/**
	 * Importer names, XML node types, and full class names.  Stored as sets of 4
	 * string objects: common name, XML node type, purpose, full class name.
	 * These are alphabetized by common name except for project-domain-specific items
	 * that must be shown ahead of others.
	**/
	private static final String[] idInfo = {
		"Links", "link", "|project|",
			"gov.epa.otaq.moves.master.implementation.importers.LinkImporter",
		"Link Source Types", "linksourcetypehour", "|project|",
			"gov.epa.otaq.moves.master.implementation.importers.LinkSourceTypeHourImporter",
		"Link Drive Schedules", "driveschedulesecondlink", "|project|",
			"gov.epa.otaq.moves.master.implementation.importers.DriveScheduleSecondLinkImporter",
		"Off-Network", "offnetworklink", "|project|",
			"gov.epa.otaq.moves.master.implementation.importers.OffNetworkLinkImporter",
		"Operating Mode Distribution", "linkopmodedistribution", "|project|",
			"gov.epa.otaq.moves.master.implementation.importers.LinkOpmodeDistributionImporter",

		"Age Distribution", "agedistribution", "|general|county|project|",
			"gov.epa.otaq.moves.master.implementation.importers.AgeDistributionImporter",
		"Average Speed Distribution", "avgspeeddistribution", "|general|county|",
			"gov.epa.otaq.moves.master.implementation.importers.AverageSpeedDistributionImporter",
		"Fueltype and Technologies", "avft", "|quiet|",
			"gov.epa.otaq.moves.master.implementation.importers.AVFTImporter",
		"Fuel", "fuel", "|general|county|project|nonroad|",
			"gov.epa.otaq.moves.master.implementation.importers.FuelImporter",
		"Fuel Supply", "fuelsupply", "|quiet|",
			"gov.epa.otaq.moves.master.implementation.importers.FuelSupplyImporter",
		"Fuel Formulations", "fuelformulation", "|quiet|",
			"gov.epa.otaq.moves.master.implementation.importers.FuelFormulationImporter",
		"Meteorology Data", "zonemonthhour", "|general|county|project|nonroad|",
			"gov.epa.otaq.moves.master.implementation.importers.MeteorologyImporter",
		"Ramp Fraction", "rampfraction", "|general|county|",
			"gov.epa.otaq.moves.master.implementation.importers.RampFractionImporter",
		"Road Type Distribution", "roadtypedistribution", "|general|county|",
			"gov.epa.otaq.moves.master.implementation.importers.RoadTypeDistributionImporter",
		"Source Type Population", "sourcetypepopulation", "|general|county|",
			"gov.epa.otaq.moves.master.implementation.importers.SourceTypePopulationImporter",
		"Starts", "starts", "|general|county|",
			"gov.epa.otaq.moves.master.implementation.importers.StartsImporter",
		"Vehicle Type VMT", "vehicletypevmt", "|general|county|",
			"gov.epa.otaq.moves.master.implementation.importers.VehicleTypeVMTImporter",
		"Zone", "zone", "|county|project|",
			"gov.epa.otaq.moves.master.implementation.importers.ZoneImporter",
		"Hotelling", "hotelling", "|general|county|project|",
			"gov.epa.otaq.moves.master.implementation.importers.HotellingImporter",
		"I/M", "imcoverage", "|general|county|project|",
			"gov.epa.otaq.moves.master.implementation.importers.IMImporter",
		"Retrofit Data", "onroadretrofit", "|general|county|project|",
			"gov.epa.otaq.moves.master.implementation.importers.OnRoadRetrofitImporter",
		"Generic", "generic", "|general|county|project|nonroad|",
			"gov.epa.otaq.moves.master.implementation.importers.GenericImporter"
	};

	/** List of all common names in the sequence needed for the project domain **/
	private static ArrayList<String> projectCommonNames = null;

	/** List of all common names in the sequence needed for the county domain **/
	private static ArrayList<String> countyCommonNames = null;

	/** List of all common names in the sequence needed for the general importer **/
	private static ArrayList<String> generalCommonNames = null;

	/** List of all common names in the sequence needed for the Nonroad importer **/
	private static ArrayList<String> nonRoadCommonNames = null;

	/** ImporterManager active during instantiation **/
	public static ImporterManager activeManager = null;

	/**
	 * Fill a list of common names that have at least one of the provided purposes.
	 * @param namesToFill list to be filled with String objects of common names
	 * @param purpose a purpose to be found, such as "general", "county", or "project".
	 * May not be null or empty.
	**/
	private static void fillNames(ArrayList<String> namesToFill, String purpose) {
		purpose = "|" + purpose + "|";
		for(int i=0;i<idInfo.length;i+=4) {
			if(!CompilationFlags.ENABLE_AUXILIARY_POWER_EXHAUST && idInfo[i+0].equalsIgnoreCase("Hotelling")) {
				continue;
			}
			if(idInfo[i+2].indexOf(purpose) >= 0) {
				namesToFill.add(idInfo[i+0]);
			}
		}
	}

	/** Fill all lists of common names **/
	private static void fillAllNames() {
		if(projectCommonNames == null) {
			projectCommonNames = new ArrayList<String>();
			fillNames(projectCommonNames,"project");
		}
		if(countyCommonNames == null) {
			countyCommonNames = new ArrayList<String>();
			fillNames(countyCommonNames,"county");
		}
		if(generalCommonNames == null) {
			generalCommonNames = new ArrayList<String>();
			fillNames(generalCommonNames,"general");
		}
		if(nonRoadCommonNames == null) {
			nonRoadCommonNames = new ArrayList<String>();
			fillNames(nonRoadCommonNames,"nonroad");
		}
	}

	/**
	 * Get an Iterator to the set of common names for Nonroad use.  The Iterator returns String
	 * objects, each holding the common name of an Importer.  The names are stored in the order
	 * they are expected to be used and shown.
	 * @return Iterator iterator returning String objects for the common names of the importers
	**/
	public static Iterator<String> nonRoadNamesIterator() {
		fillAllNames();
		return nonRoadCommonNames.iterator();
	}

	/**
	 * Get an Iterator to the set of common names for general use.  The Iterator returns String
	 * objects, each holding the common name of an Importer.  The names are stored in the order
	 * they are expected to be used and shown.
	 * @return Iterator iterator returning String objects for the common names of the importers
	**/
	public static Iterator<String> generalNamesIterator() {
		fillAllNames();
		return generalCommonNames.iterator();
	}

	/**
	 * Get an Iterator to the set of common names for County-domain use.  The Iterator returns
	 * String objects, each holding the common name of an Importer.  The names are stored in the
	 * order they are expected to be used and shown.
	 * @return Iterator iterator returning String objects for the common names of the importers
	**/
	public static Iterator<String> countyNamesIterator() {
		fillAllNames();
		return countyCommonNames.iterator();
	}

	/**
	 * Get an Iterator to the set of common names for Project-domain use.  The Iterator returns
	 * String objects, each holding the common name of an Importer.  The names are stored in the
	 * order they are expected to be used and shown.
	 * @return Iterator iterator returning String objects for the common names of the importers
	**/
	public static Iterator<String> projectNamesIterator() {
		fillAllNames();
		return projectCommonNames.iterator();
	}

	/**
	 * Instantiate an importer given its common name.
	 * @param name common name of the importer to be created
	 * @param manager owner of the created importer
	 * @return the instantiated importer, complete with setManager() already called,
	 * or null if the name could not be found.
	**/
	public static IImporter createByName(String name, ImporterManager manager) {
		for(int i=0;i<idInfo.length;i+=4) {
			if(name.equalsIgnoreCase(idInfo[i+0])) {
				return createByClassName(idInfo[i+3],manager);
			}
		}
		return null;
	}

	/**
	 * Instantiate an importer given its XML node type.
	 * @param xmlNodeTypeName XML node type of the importer to be instantiated
	 * @param manager owner of the created importer
	 * @return the instantiated importer, complete with setManager() already called,
	 * or null if the XML node type could not be found.
	**/
	public static IImporter createByXMLNodeType(String xmlNodeTypeName, ImporterManager manager) {
		for(int i=0;i<idInfo.length;i+=4) {
			if(xmlNodeTypeName.equalsIgnoreCase(idInfo[i+1])) {
				return createByClassName(idInfo[i+3],manager);
			}
		}
		return null;
	}

	/**
	 * Instantiate an importer given its full class name.
	 * @param className class name of the importer to be instantiated
	 * @param manager owner of the created importer
	 * @return the instantiated importer, complete with setManager() already called,
	 * or null if the class name is not an IImporter.
	**/
	private static synchronized IImporter createByClassName(String className, ImporterManager manager) {
		activeManager = manager;
		try {
			Class c = Class.forName(className);
			try {
				Object object = c.newInstance();
				IImporter importer = (IImporter)object;
				return importer;
			} catch(Exception e) {
				Logger.logError(e, "ImporterInstantiator is unable to instantiate " + className);
			}
		} catch(ClassNotFoundException e) {
			Logger.logError(e, "ImporterInstantiator could not find Java class " + className +
					" to instantiate");
		} finally {
			activeManager = null;
		}
		return null;
	}
}
