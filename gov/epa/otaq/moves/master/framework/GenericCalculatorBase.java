/**************************************************************************************************
 * @(#)GenericCalculatorBase.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.RunSpec;

import java.util.*;
import java.sql.*;

/**
 * Base class for calculators that follow the basic pattern of activity*emissions.
 * <br>
 * Derived classes should pay particular attention to the parameters passed to the constructor:
 * <ul>
 * <li><b>pollutantProcessIDsToUse</b> array of polProcessID strings.  These are all the items
 * that any instance of this calculator can calculate.  These will be filtered during execution
 * to only those actually implied by the current RunSpec.</li>
 * <li><b>granularityToUse</b> the granularity (typically MasterLoopGranularity.YEAR) at
 * which the calculator runs</li>
 * <li><b>priorityAdjustmentToUse</b> any offset from the MasterLoopPriority.EMISSION_CALCULATOR
 * priority to run at, typically 0</li>
 * <li><b>scriptFileNameToUse</b> the name of the script file to use.  This should include any
 * subdirectory reference.  If "" or null, then "database/GenericCalculator.sql" is used.</li>
 * <li><b>additionalSectionNamesToUse</b> names of non-standard script section names that should
 * always be enabled for all supported pollutants and processes.  If conditions need to be
 * placed on which sections are enabled, use the alterReplacementsAndSections(...) routine.</li>
 * </ul>
 * <br>
 * A typical call to the constructor via super() in a derived class would look like:<br>
 * <pre>
 * public SampleCalculator() {
 * 	super(new String[]{ "101", "201" }, // HC Running Exhaust and CO Running Exhaust
 * 		MasterLoopGranularity.YEAR,
 * 		0, // no offset from the standard MasterLoopPriority.EMISSION_CALCULATOR
 * 		null, // use default script
 * 		new String[]{ "Special Section", "Second Special Section" }
 * 	);
 * }
 * </pre>
 * Such a constructor call will:
 * <ul>
 * <li>register the calculator for HC Running Exhaust and CO Running Exhaust</li>
 * <li>check the executing RunSpec and only subscribe at all if HC Running Exhaust
 * or CO Running Exhaust are requested.  If not, then no subscription will occur.</li>
 * <li>maintain an internal set of processes and pollutants actually requested by the RunSpec that
 * intersect with this instance's abilities.
 * <li>subscribe the calculator at MasterLoopGranularity.YEAR granularity</li>
 * <li>subscribe the calculator at MasterLoopPriority.EMISSION_CALCULATOR priority</li>
 * </ul>
 * <br>
 * During the looping phase, the intersected set of pollutants and processes is used to
 * build the standard replacement strings ##pollutantProcessIDs## and ##pollutantIDs##.
 * Also, these replacements are accompanied by several automatically enabled section names:
 * <ul>
 * <li><b>[process name]</b> the human readable name of the current process.  For example:
 * <b>Running Exhaust</b></li>
 * <li><b>Process [process ID]</b> the word "Process" followed by a space and the process ID.
 * For example: <b>Process 1</b></li>
 * <li><b>[pollutant name]</b> the human readable name of one of the pollutants supported for
 * the current process.  Each supported pollutant will get such a section.  For example:
 * <b>CO</b></li>
 * <li><b>Pollutant [pollutant ID]</b> the word "Pollutant" followed by a space and the pollutant
 * ID of one of the pollutants supported for the current process.  Each supported pollutant will
 * get such a section.  For example: <b>Pollutant 2</b></li>
 * </ul>
 * <br>
 * The above sections are in addition to any provided by the constructor call of course.
 * <br>
 * Derived classes have the code-level opportunity to alter these replacements and section names
 * by overrrding the alterReplacementsAndSections(...) routine.  This routine is called immediately
 * prior to executing the script file with the replacments and sections.  Derived classes are
 * free to add, modify, or delete entries from the replacements and section names in this
 * routine.
 *
 * @author		Wesley Faler
 * @author		Gwo Shyu, EPA (minor mods for Task 216)
 * @version		2010-10-10
**/
public class GenericCalculatorBase extends EmissionCalculator {
	/** class to track which pollutants are calculated by this calculator **/
	class Pollutants {
		/** emission process the pollutants are related to **/
		EmissionProcess process;
		/** comma-separated list of pollutant IDs, setup during subscribeToMe **/
		String pollutantIDs = "";
		/** comma-separated list of polProcessIDs, setup during subscribeToMe **/
		String pollutantProcessIDs = "";
		/** set of Pollutant objects, trimmed during subscribeToMe **/
		TreeSet<Pollutant> pollutants = new TreeSet<Pollutant>();
	}

	/**
	 * array of polProcessID strings passed to the constructor.  These are all the items
	 * that any instance of this calculator can calculate.
	**/
	String[] pollutantProcessIDs;
	/** the granularity (typically MasterLoopGranularity.YEAR) at which the calculator runs **/
	MasterLoopGranularity granularity;
	/**
	 * any offset from the MasterLoopPriority.EMISSION_CALCULATOR priority to run at,
	 * typically 0
	**/
	int priorityAdjustment;
	/** the name of the script file to use.  This should include any subdirectory reference **/
	String scriptFileName;
	/**
	 * names of non-standard script section names that should always be enabled for all
	 * supported pollutants and processes.  If conditions need to be placed on which sections
	 * are enabled, use the alterReplacementsAndSections(...) routine.
	**/
	String[] additionalSectionNames;
	/** Pollutants objects keyed by process **/
	TreeMap<EmissionProcess,Pollutants> pollutantsByProcess = new TreeMap<EmissionProcess,Pollutants>();

	/** True during the first time a bundle is created **/
	boolean isFirstExecution = true;

	/**
	 * Constructor
	 * @param pollutantProcessIDsToUse array of polProcessID strings.  These are all the items
	 * that any instance of this calculator can calculate.  These will be filtered during execution
	 * to only those actually implied by the current RunSpec.
	 * @param granularityToUse the granularity (typically MasterLoopGranularity.YEAR) at
	 * which the calculator runs
	 * @param priorityAdjustmentToUse any offset from the MasterLoopPriority.EMISSION_CALCULATOR
	 * priority to run at, typically 0
	 * @param scriptFileNameToUse the name of the script file to use.  This should include any
	 * subdirectory reference.  If "" or null, then "database/GenericCalculator.sql" is used.
	 * @param additionalSectionNamesToUse names of non-standard script section names that should
	 * always be enabled for all supported pollutants and processes.  If conditions need to be
	 * placed on which sections are enabled, use the alterReplacementsAndSections(...) routine.
	**/
	public GenericCalculatorBase(String[] pollutantProcessIDsToUse,
			MasterLoopGranularity granularityToUse,
			int priorityAdjustmentToUse,
			String scriptFileNameToUse,
			String[] additionalSectionNamesToUse) {
		pollutantProcessIDs = pollutantProcessIDsToUse;
		granularity = granularityToUse;
		priorityAdjustment = priorityAdjustmentToUse;
		scriptFileName = scriptFileNameToUse;
		if(scriptFileName == null || scriptFileName.length() <= 0) {
			scriptFileName = "database/GenericCalculator.sql";
		}
		additionalSectionNames = additionalSectionNamesToUse;

		splitPollutantProcessIDs();
		doRegistration();
	}

	/**
	 * Interpret the pollutantProcessIDs array, finding the polluant IDs and
	 * process IDs, and building the pollutantsByProcess data structure
	**/
	void splitPollutantProcessIDs() {
		Connection executionDatabase = null;
		try {
			executionDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION);
		} catch(Exception e) {
			/** @explain A connection to the MOVESExecution database could not be established. **/
			Logger.logError(e,"Unable to get the Execution Database connection");
			return;
		}

		for(int i=0;i<pollutantProcessIDs.length;i++) {
			int pollutantID = 0;
			int processID = 0;

			String sql = "";
			ResultSet results = null;
			// Find the database entry with the polProcessID, getting pollutantID and processID
			try {
				sql = "SELECT processID, pollutantID FROM PollutantProcessAssoc "
					+ "WHERE polProcessID = " + pollutantProcessIDs[i];
				results = SQLRunner.executeQuery(executionDatabase,sql);
				if(results != null && results.next()) {
					processID = results.getInt(1);
					pollutantID = results.getInt(2);
				}
			} catch(SQLException e) {
				Logger.logSqlError(e, "Unable to get database info for Pollutant Process " +
						"Association where polProcessID = " + pollutantProcessIDs[i],sql);
			} finally {
				if(results != null) {
					try {
						results.close();
					} catch(Exception e) {
						// Nothing to do here
					}
				}
			}

			// Find the pollutant
			Pollutant pollutant = Pollutant.findByID(pollutantID);
			if(pollutant == null) {
				continue;
			}
			// Find the process
			EmissionProcess process = EmissionProcess.findByID(processID);
			if(process == null) {
				continue;
			}

			Pollutants pollutants = (Pollutants)pollutantsByProcess.get(process);
			if(pollutants == null) {
				pollutants = new Pollutants();
				pollutants.process = process;
				pollutantsByProcess.put(process,pollutants);
			}
			pollutants.pollutants.add(pollutant);
		}

		if(executionDatabase != null) {
			DatabaseConnectionManager.checkInConnection(
				MOVESDatabaseType.EXECUTION, executionDatabase);
			executionDatabase = null;
		}
	}

	/**
	 * Register this calculator for all of the pollutants and process it supports
	**/
	void doRegistration() {
		for(Iterator<EmissionProcess> i=pollutantsByProcess.keySet().iterator();i.hasNext();) {
			Pollutants pollutants = (Pollutants)pollutantsByProcess.get(i.next());
			for(Iterator<Pollutant> j=pollutants.pollutants.iterator();j.hasNext();) {
				Pollutant p = (Pollutant)j.next();
				EmissionCalculatorRegistration.register(p,pollutants.process,this);
			}
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public final void subscribeToMe(MasterLoop targetLoop) {
		Connection executionDatabase = null;
		try {
			executionDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION);
		} catch(Exception e) {
			/** @explain A connection to the MOVESExecution database could not be established. **/
			Logger.logError(e,"Unable to get the Execution Database connection");
			return;
		}

		// For every process in pollutantsByProcess...
		for(Iterator<EmissionProcess> i=pollutantsByProcess.keySet().iterator();i.hasNext();) {
			Pollutants pollutants = (Pollutants)pollutantsByProcess.get(i.next());
			TreeSet<Pollutant> usedPollutants = new TreeSet<Pollutant>();
			for(Iterator<Pollutant> j=pollutants.pollutants.iterator();j.hasNext();) {
				Pollutant p = (Pollutant)j.next();
				if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
						p,pollutants.process)) {
					usedPollutants.add(p);
					if(pollutants.pollutantIDs.length() > 0) {
						pollutants.pollutantIDs += ",";
					}
					pollutants.pollutantIDs += p.databaseKey;

					PollutantProcessAssociation assoc =
							PollutantProcessAssociation.createByID(p.databaseKey,
							pollutants.process.databaseKey);

					if(pollutants.pollutantProcessIDs.length() > 0) {
						pollutants.pollutantProcessIDs += ",";
					}
					pollutants.pollutantProcessIDs += assoc.getDatabaseKey(executionDatabase);
				}
			}
			pollutants.pollutants = usedPollutants;
			if(usedPollutants.size() > 0) {
				targetLoop.subscribe(this, pollutants.process, granularity,
						MasterLoopPriority.EMISSION_CALCULATOR+priorityAdjustment);
			}
		}

		if(executionDatabase != null) {
			DatabaseConnectionManager.checkInConnection(
				MOVESDatabaseType.EXECUTION, executionDatabase);
			executionDatabase = null;
		}
	}

	/**
	 * This is an EmissionCalculator override that builds SQL statements for a distributed
	 * worker to execute. This is called by EmissionCalculator.executeLoop.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	**/
	public final SQLForWorker doExecute(MasterLoopContext context) {
		Pollutants pollutants = (Pollutants)pollutantsByProcess.get(context.iterProcess);

		if(pollutants == null) { // If the context's process is not supported, then quit
			return null;
		}

		// Roadtype for start emissions is always 1.
		if(context.iterProcess.processName.equalsIgnoreCase("Start Exhaust")) {
			if(context.iterLocation.roadTypeRecordID != 1) {
				return null;
			}
		}

        // Roadtype for Extended Idle Exhaust emissions is also always 1 (Task 216).
        if(context.iterProcess.processName.equalsIgnoreCase("Extended Idle Exhaust")) {
            if(context.iterLocation.roadTypeRecordID != 1) {
                return null;
            }
        }

		// At this point, we know that a supported process is being asked for and since
		// subscribeToMe has trimmed the set of pollutants to match those in the RunSpec,
		// we can proceed without checking the RunSpec again.

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		replacements.put("##pollutantProcessIDs##",pollutants.pollutantProcessIDs);
		replacements.put("##pollutantIDs##",pollutants.pollutantIDs);

		enabledSectionNames.add(pollutants.process.processName);
		enabledSectionNames.add("Process " + pollutants.process.databaseKey);

		for(Iterator<Pollutant> i=pollutants.pollutants.iterator();i.hasNext();) {
			Pollutant p = (Pollutant)i.next();
			enabledSectionNames.add(p.pollutantName);
			enabledSectionNames.add("Pollutant " + p.databaseKey);
		}

		if(additionalSectionNames != null) {
			for(int i=0;i<additionalSectionNames.length;i++) {
				enabledSectionNames.add(additionalSectionNames[i]);
			}
		}

		OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
			ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection();
		if(outputEmissionsBreakdownSelection.onRoadSCC) {
			enabledSectionNames.add("SCCOutput");
		} else {
			enabledSectionNames.add("NoSCCOutput");
		}

		if(isFirstExecution) {
			enabledSectionNames.add("FirstBundle");
			isFirstExecution = false;
		}

		alterReplacementsAndSections(context,replacements,enabledSectionNames);

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				scriptFileName,enabledSectionNames,sqlForWorker);
		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}

	/**
	 * Called just before readAndHandleScriptedCalculations(...) is called, this is the last
	 * point at which a derived class can alter the script replacement values and/or the
	 * set of section names to be enabled.  The derived class is allowed to remove entries
	 * from the passed parameters as well as add or modify entries.
	 * The default implementation is to do nothing.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @param replacements name/value pairs for replacement within the SQL script
	 * @param enabledSectionNames names of non-standard sections to used within the SQL script
	**/
	public void alterReplacementsAndSections(MasterLoopContext context,
			TreeMapIgnoreCase replacements,
			TreeSetIgnoreCase enabledSectionNames) {
		// Default implementation is to do nothing
	}
}
