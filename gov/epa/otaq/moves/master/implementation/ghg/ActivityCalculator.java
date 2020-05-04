/**************************************************************************************************
 * @(#)ActivityCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.RunSpec;

import java.util.*;
import java.sql.*;

/**
 * Capture activity information for SHO, SourceHours, SHP, ExtendedIdleHours, HotellingHours,
 * Starts, and Population.
 *
 * @author		Wesley Faler
 * @version		2014-08-21
**/
public class ActivityCalculator extends EmissionCalculator {
	/**
	 * @algorithm
	 * @owner Activity Calculator
	**/

	/** True if the calculator script has yet to be executed **/
	boolean isFirstExecute = true;
	/** Loop that owns this calculator **/
	MasterLoop owningLoop = null;
	/** true when the owningLoop is only counting bundles **/
	boolean isCountingBundles = true;
	/** county/year combinations used for fuelUsageFraction **/
	TreeSet<String> fuelUsageKeys = new TreeSet<String>();

	/** Information about a single activity type **/
	class ActivityInfo {
		/** Unique <location>|<time> keys combinations already processed **/
		public TreeSet<String> keys = new TreeSet<String>();
		/** Name of the associated calculator section **/
		public String sectionName;
		/** Array of processID values that generate activity information **/
		public int[] processIDs;
		/** Process objects associated with the processIDs **/
		ArrayList<EmissionProcess> processObjects = new ArrayList<EmissionProcess>();
		/** True if the active RunSpec requires the activity **/
		public boolean isNeeded = false;
		/** True if only the offnetwork road type should be used **/
		public boolean offNetworkOnly = false;
		/** True if only the non-offnetwork road types should be used **/
		public boolean onNetworkOnly = false;
		/** Location level used for the key, such as "Zone" or "Link" **/
		public String locationLevel = "";

		/**
		 * Constructor
		 * @param sectionNameToUse Name of the associated calculator section
		 * @param processIDsToUse Array of processID values that generate activity information
		 * @param offNetworkOnlyToUse True if only the offnetwork road type should be used
		 * @param locationLevelToUse Location level used for the key, such as "Zone" or "Link"
		 * @param onNetworkOnlyToUse True if only the non-offnetwork road types should be used
		**/
		public ActivityInfo(String sectionNameToUse, int[] processIDsToUse,
				boolean offNetworkOnlyToUse, String locationLevelToUse, boolean onNetworkOnlyToUse) {
			sectionName = sectionNameToUse;
			processIDs = processIDsToUse;
			offNetworkOnly = offNetworkOnlyToUse;
			locationLevel = locationLevelToUse;
			onNetworkOnly = onNetworkOnlyToUse;
		}
	}

	/** All activity types handled by this calculator **/
	ActivityInfo[] activities = {
		new ActivityInfo("SourceHours",new int[] { 11, 12, 13 },false,"Link",false),
		new ActivityInfo("ExtendedIdleHours",new int[] { 90 },true,"Zone",false),
		new ActivityInfo("hotellingHours",new int[] { 91 },true,"Zone",false),
		new ActivityInfo("SHO",new int[] { 1, 9, 10, 11, 12, 13 },false,"Link",false),
		new ActivityInfo("SHP",new int[] { 11, 12, 13 },true,"Zone",false),
		new ActivityInfo("Population",new int[] { 1, 2, 9, 10, 11, 12, 13, 90, 91 },false,"Zone",false),
		new ActivityInfo("Starts",new int[] { 2 },true,"Zone",false)
	};

	/**
	 * Constructor
	**/
	public ActivityCalculator() {
		// This calculator doesn't determine pollutants in any way, so it
		// does not register itself.
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		owningLoop = targetLoop;
		isCountingBundles = true;

		RunSpec runSpec = ExecutionRunSpec.theExecutionRunSpec.getRunSpec();
		enableActivity("SourceHours",runSpec.outputSH);
		enableActivity("ExtendedIdleHours",runSpec.outputSHIdling);
		enableActivity("hotellingHours",runSpec.outputSHIdling);
		enableActivity("SHO",runSpec.outputSHO);
		enableActivity("SHP",runSpec.outputSHP);
		enableActivity("Population",runSpec.outputPopulation);
		enableActivity("Starts",runSpec.outputStarts);

		// Subscribe to all processes for all needed activities
		TreeSet<Integer> subscribedProcesses = new TreeSet<Integer>();
		for(int i=0;i<activities.length;i++) {
			ActivityInfo a = activities[i];
			if(!a.isNeeded) {
				continue;
			}
			for(int j=0;j<a.processIDs.length;j++) {
				EmissionProcess p = EmissionProcess.findByID(a.processIDs[j]);
				if(p == null) {
					continue;
				}
				a.processObjects.add(p);

				Integer pid = new Integer(a.processIDs[j]);
				if(!subscribedProcesses.contains(pid)) {
					subscribedProcesses.add(pid);
					subscribe(targetLoop,pid.intValue());
				}
			}
		}
	}

	/**
	 * Flag an activity as needed or not needed according to the active RunSpec.
	 * @param sectionName section name of the associated activity type
	 * @param isNeeded True if the RunSpec requires the activity output
	**/
	private void enableActivity(String sectionName, boolean isNeeded) {
		for(int i=0;i<activities.length;i++) {
			if(activities[i].sectionName.equalsIgnoreCase(sectionName)) {
				activities[i].isNeeded = isNeeded;
				return;
			}
		}
	}
	/**
	 * Subscribe to the MasterLoop if the RunSpec requires a given process.
	 * @param targetLoop The loop to subscribe to.
	 * @param processID ID of candidate emission process
	**/
	private void subscribe(MasterLoop targetLoop, int processID) {
		if(!ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(0,processID)) {
			return;
		}
		EmissionProcess process = EmissionProcess.findByID(processID);
		targetLoop.subscribe(this, process, MasterLoopGranularity.YEAR,
				MasterLoopPriority.EMISSION_CALCULATOR);
	}

	/**
	 * Examine a context for suitability.  Used to override the natural execution hierarchy.
	 * @param context Context to be examined
	 * @return true if the context should be used by a MasterLoopable.
	**/
	public boolean doesProcessContext(MasterLoopContext context) {
		if(owningLoop.isCountingBundles() != isCountingBundles) {
			isCountingBundles = owningLoop.isCountingBundles();
			// Reset flags about what has been calculated
			for(int i=0;i<activities.length;i++) {
				ActivityInfo a = activities[i];
				a.keys.clear();
			}
			fuelUsageKeys.clear();
		}
		if(context.iterLocation == null || context.iterLocation.zoneRecordID < 0 || context.iterLocation.roadTypeRecordID < 0
				|| context.year < 0) {
			return false;
		}

		boolean isOffNetwork = context.iterLocation.roadTypeRecordID == 1;
		boolean foundSomethingToCalculate = false;

		for(int i=0;i<activities.length;i++) {
			ActivityInfo a = activities[i];
			if(!a.isNeeded) {
				continue;
			}
			if(a.offNetworkOnly && !isOffNetwork) {
				continue;
			}
			if(a.onNetworkOnly && isOffNetwork) {
				continue;
			}
			boolean foundProcess = false;
			for(Iterator<EmissionProcess> pi=a.processObjects.iterator();pi.hasNext();) {
				EmissionProcess p = pi.next();
				if(context.hasProcess(p)) {
					foundProcess = true;
					break;
				}
			}
			if(!foundProcess) {
				continue;
			}
			String key = "";
			if(a.locationLevel.equalsIgnoreCase("Link")) {
				key = "Link|" + context.iterLocation.linkRecordID + "|" + context.year;
			} else {
				key = "Zone|" + context.iterLocation.zoneRecordID + "|" + context.year;
			}
			if(!a.keys.contains(key)) {
				// found something to calculate that wasn't already calculated
				if(owningLoop.isCountingBundles()) {
					// When counting bundles, doExecute() won't set these flags, so it must be done here.
					a.keys.add(key);
				}
				foundSomethingToCalculate = true;
			}
		}

		return foundSomethingToCalculate;
	}

	/**
	 * Builds SQL statements for a distributed worker to execute. This is called by
	 * DistanceCalculator.executeLoop. Implementations of this method
	 * should contain uncertainty logic when UncertaintyParameters specifies that
	 * this mode is enabled.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	**/
	public SQLForWorker doExecute(MasterLoopContext context) {
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		boolean isOffNetwork = context.iterLocation.roadTypeRecordID == 1;

		boolean foundSomethingToCalculate = false;

		for(int i=0;i<activities.length;i++) {
			ActivityInfo a = activities[i];
			if(!a.isNeeded) {
				continue;
			}
			if(a.offNetworkOnly && !isOffNetwork) {
				continue;
			}
			if(a.onNetworkOnly && isOffNetwork) {
				continue;
			}
			boolean foundProcess = false;
			for(Iterator<EmissionProcess> pi=a.processObjects.iterator();pi.hasNext();) {
				EmissionProcess p = pi.next();
				if(context.hasProcess(p)) {
					foundProcess = true;
					break;
				}
			}
			if(!foundProcess) {
				continue;
			}
			String key = "";
			if(a.locationLevel.equalsIgnoreCase("Link")) {
				key = "Link|" + context.iterLocation.linkRecordID + "|" + context.year;
			} else {
				key = "Zone|" + context.iterLocation.zoneRecordID + "|" + context.year;
			}
			if(!a.keys.contains(key)) {
				a.keys.add(key);
				enabledSectionNames.add(a.sectionName);
				foundSomethingToCalculate = true;
			}
		}

		if(!foundSomethingToCalculate) {
			return null;
		}

		String fuelUsageKey = "";
		if(CompilationFlags.USE_FUELUSAGEFRACTION) {
			enabledSectionNames.add("UseFuelUsageFraction");
			fuelUsageKey = "" + context.iterLocation.countyRecordID + "|" + context.year; // once per county/year
		} else {
			fuelUsageKey = "createSourceTypeFuelFraction"; // only once
			enabledSectionNames.add("UseSampleVehiclePopulation");
		}
		if(!fuelUsageKeys.contains(fuelUsageKey)) {
			fuelUsageKeys.add(fuelUsageKey);
			enabledSectionNames.add("createSourceTypeFuelFraction");
		}

		if(isFirstExecute) {
			isFirstExecute = false;

			// Ensure definitions for required tables are available.
			// These tables are created during execution and aren't learned at the beginning
			// of the master loop.
			String[] extraTableNames = {
				"sourceTypeAgePopulation",
				"fractionWithinHPMSVType",
				"analysisYearVMT",
				"roadTypeDistribution",
				"zoneRoadType",
				"monthVMTFraction",
				"hourVMTFraction",
				"SHP"
			};
			Connection executionDB = null;
			SQLRunner.Query query = new SQLRunner.Query();
			for(int i=0;i<extraTableNames.length;i++) {
				String sql = "select * from " + extraTableNames[i] + " limit 0";
				try {
					if(executionDB == null) {
						executionDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
					}
					query.open(executionDB,sql);
					query.close();
					// The table exists, so ensure its creation statement is known to the cache
					DatabaseConnectionManager.getExecutionCreateTableStatement(extraTableNames[i]);
				} catch(Exception e) {
					// The table doesn't exist, nothing to do here
				} finally {
					query.onFinally();
				}
				/*
				String t = DatabaseConnectionManager.getExecutionCreateTableStatement(extraTableNames[i]);
				if(t == null || t.length() <= 0) {
					t = "**** TABLE NOT FOUND ****";
				}
				if(enabledSectionNames.contains("Population") || enabledSectionNames.contains("SHP")) {
					System.out.println("ActivityCalculator: " + context.iterProcess.processName
							+ ": " + extraTableNames[i] + ": " + t);
				}
				*/
			}
			if(executionDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,executionDB);
				executionDB = null;
			}
		}

		SQLForWorker sqlForWorker = new SQLForWorker();
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();

		OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
				ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection();
		if(outputEmissionsBreakdownSelection.onRoadSCC) {
			enabledSectionNames.add("SCCOutput");
		} else {
			enabledSectionNames.add("NoSCCOutput");
		}
		/*
		if(outputEmissionsBreakdownSelection.regClassID) {
			enabledSectionNames.add("WithRegClassID");
		} else {
			enabledSectionNames.add("NoRegClassID");
		}
		*/
		enabledSectionNames.add("WithRegClassID");
		replacements.put("##ActivityTable##","MOVESWorkerActivityOutput");

		if(ExecutionRunSpec.getRunSpec().domain == ModelDomain.PROJECT) {
			enabledSectionNames.add("ProjectDomain");
		} else {
			enabledSectionNames.add("NonProjectDomain");
		}

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/ActivityCalculator.sql",enabledSectionNames,
				sqlForWorker);
		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
