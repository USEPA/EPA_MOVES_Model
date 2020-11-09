/**************************************************************************************************
 * @(#)HCSpeciationCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

import java.util.*;
import java.sql.*;

/**
 * Perform HC Speciation calculations
 * @author		Wesley Faler
 * @version		2015-03-11
**/
public class HCSpeciationCalculator extends EmissionCalculator implements EmissionCalculatorExternal {
	/**
	 * @algorithm
	 * @owner HC Speciation Calculator
	 * @calculator
	**/

	/** Summary class holding details of the HC pollutants **/
	class HCEntry {
		int outputPolProcessID;
		PollutantProcessAssociation output;
		// inputs holds PollutantProcessAssociation entries
		TreeSet<PollutantProcessAssociation> inputs = null;

		HCEntry(int outputPolProcessIDToUse) {
			outputPolProcessID = outputPolProcessIDToUse;
			output = PollutantProcessAssociation.createByID(outputPolProcessID);
			if(output != null) {
				TreeSet<Integer> inputIDs =
						PollutantProcessAssociation.chainedTo(outputPolProcessID);
				if(inputIDs != null && inputIDs.size() > 0) {
					inputs = new TreeSet<PollutantProcessAssociation>();
					for(Iterator<Integer> iterInputIDs = inputIDs.iterator();
							iterInputIDs.hasNext();) {
						Integer chainedToObject = (Integer)iterInputIDs.next();
						int chainedTo = chainedToObject.intValue();
						inputs.add(PollutantProcessAssociation.createByID(chainedTo));
					}
				}
			}
		}
	}

	/** List of HCEntry objects **/
	ArrayList<HCEntry> entries = new ArrayList<HCEntry>();
	/** True if buildPollutantAndProcessRequirements() has been called **/
	boolean didBuildPollutantAndProcessRequirements = false;
	/** Processes requiring Methane output **/
	String methaneProcessIDs = "";
	/** Processes requiring NMOG output **/
	String nmogProcessIDs = "";
	/** Processes requiring NMHC output **/
	String nmhcProcessIDs = "";
	/** Processes requiring VOC output **/
	String vocProcessIDs = "";
	/** Processes requiring TOG output **/
	String togProcessIDs = "";
	/** All output pollutant process IDs needed **/
	String hcPolProcessIDs = "";
	/** All processes requiring any HC species output **/
	String hcProcessIDs = "";

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public HCSpeciationCalculator() {
		loadEntries();

		for(Iterator<HCEntry> i=entries.iterator();i.hasNext();) {
			HCEntry a = (HCEntry)i.next();
			EmissionCalculatorRegistration.register(a.output.pollutant,
					a.output.emissionProcess,this);
		}
	}

	/** Fill the entries list from the PollutantProcessAssoc table **/
	private void loadEntries() {
		String sql = "";

		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();

		try {
			sql = "select polProcessID"
					+ " from PollutantProcessAssoc"
					+ " where pollutantID in (5,79,80,86,87)"
					+ " and processID in (1,2,11,12,13,15,16,17,18,19,20,21,30,31,32,90,91)";
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			query.open(db,sql);
			while(query.rs.next()) {
				HCEntry a = new HCEntry(query.rs.getInt(1));
				if(a.output != null && a.inputs != null && a.inputs.size() > 0) {
					entries.add(a);
				}
			}
			query.rs.close();
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to load HC entries",sql);
		} finally {
			query.onFinally();
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		LinkedList<EmissionCalculator> calculators = new LinkedList<EmissionCalculator>();

		for(Iterator<HCEntry> i=entries.iterator();i.hasNext();) {
			HCEntry a = (HCEntry)i.next();
			if(a.inputs == null || a.inputs.size() <= 0) {
				continue;
			}
			if(a.inputs.size() > 1) {
				// Master-side aggregation will be needed
				ExecutionRunSpec.pollutantProcessNeedsAggregation(a.output);
			}
			for(Iterator<PollutantProcessAssociation> j=a.inputs.iterator();j.hasNext();) {
				PollutantProcessAssociation input = (PollutantProcessAssociation)j.next();
				LinkedList<EmissionCalculator> t =
						EmissionCalculatorRegistration.find(input.pollutant,input.emissionProcess);
				EmissionCalculatorRegistration.merge(calculators,t);
			}
		}

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				//System.out.println("HCSpeciationCalc chained to " + c.getClass().getName());
				c.chainCalculator(this);
			}
		}
	}

	/**
	 * Utility to append an ID to a comma-separated list.
	 * @param set existing values already present in the text
	 * @param text existing text to be appended.  May be blank or null.
	 * @param id number to be appended.
	 * @return text with id appended, including a separating comma if required.
	**/
	String append(TreeSet<Integer> set, String text, Integer id) {
		if(set.contains(id)) {
			if(text == null) {
				return "";
			}
			return text;
		}
		set.add(id);

		if(text == null) {
			text = "";
		} else if(text.length() > 0) {
			text += ",";
		}
		text += id;
		return text;
	}

	/** Build the flags describing the pollutant sections needed and their input processes **/
	void buildPollutantAndProcessRequirements() {
		// Key is output pollutant ID Integer, data is TreeSet of processID Integer objects
		TreeMap<Integer,TreeSet<Integer> > processesByOutputPollutant =
				new TreeMap<Integer,TreeSet<Integer> >();
		TreeSet<Integer> allOutputs = new TreeSet<Integer>();

		TreeSet<Integer> methaneProcessIDset = new TreeSet<Integer>();
		TreeSet<Integer> nmogProcessIDset = new TreeSet<Integer>();
		TreeSet<Integer> nmhcProcessIDset = new TreeSet<Integer>();
		TreeSet<Integer> vocProcessIDset = new TreeSet<Integer>();
		TreeSet<Integer> togProcessIDset = new TreeSet<Integer>();
		TreeSet<Integer> hcPolProcessIDset = new TreeSet<Integer>();
		TreeSet<Integer> hcProcessIDset = new TreeSet<Integer>();

		for(Iterator<HCEntry> ei=entries.iterator();ei.hasNext();) {
			HCEntry a = (HCEntry)ei.next();
			if(!ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					a.output.pollutant,a.output.emissionProcess)) {
				continue;
			}
			Integer outputPolProcessID = Integer.valueOf(a.outputPolProcessID);
			if(!allOutputs.contains(outputPolProcessID)) {
				allOutputs.add(outputPolProcessID);
				hcPolProcessIDs = append(hcPolProcessIDset,hcPolProcessIDs,outputPolProcessID);
			}
			Integer outputPollutantID = Integer.valueOf(a.output.pollutant.databaseKey);
			TreeSet<Integer> processes = processesByOutputPollutant.get(outputPollutantID);
			if(processes == null) {
				processes = new TreeSet<Integer>();
				processesByOutputPollutant.put(outputPollutantID,processes);
			}
			for(Iterator<PollutantProcessAssociation> i=a.inputs.iterator();i.hasNext();) {
				PollutantProcessAssociation ppa = (PollutantProcessAssociation)i.next();
				Integer processID = Integer.valueOf(ppa.emissionProcess.databaseKey);
				if(!processes.contains(processID)) {
					processes.add(processID);
					switch(a.output.pollutant.databaseKey) {
						case 5: // Methane
							methaneProcessIDs = append(methaneProcessIDset,methaneProcessIDs,processID);
							break;
						case 79: // NMHC
							nmhcProcessIDs = append(nmhcProcessIDset,nmhcProcessIDs,processID);
							break;
						case 80: // NMOG
							nmogProcessIDs = append(nmogProcessIDset,nmogProcessIDs,processID);
							break;
						case 86: // TOG
							togProcessIDs = append(togProcessIDset,togProcessIDs,processID);
							break;
						case 87: // VOC
							vocProcessIDs = append(vocProcessIDset,vocProcessIDs,processID);
							break;
					}
				}
			}
			for(Iterator<Integer> i=processes.iterator();i.hasNext();) {
				Integer processID = i.next();
				hcProcessIDs = append(hcProcessIDset,hcProcessIDs,processID);
			}
		}
	}

	/**
	 * Build case-statements that assign oxyThreshID to each fuel formulation
	 * @param db database to be used
	 * @return case statements that assign oxyThreshID to each fuel formulation
	 * @throws SQLException if anything goes wrong
	**/
	public static String buildOxyThreshCase(Connection db) throws SQLException {
		String sql = "select oxyThreshID, oxyThreshName from oxyThreshName";
		SQLRunner.Query query = new SQLRunner.Query();
		String result = "case\n";
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				int id = query.rs.getInt(1);
				String expression = query.rs.getString(2);
				result += "when (" + expression + ") then " + id + "\n";
			}
		} finally {
			query.onFinally();
		}
		return result + "else 0 end";
	}

	/**
	 * Builds SQL statements for a distributed worker to execute. This is called by
	 * EnergyConsumptionCalculator.executeLoop. Implementations of this method
	 * should contain uncertainty logic when UncertaintyParameters specifies that
	 * this mode is enabled.
	 * @param context The MasterLoopContext that applies to this execution.
	 * @return The resulting sql lists as an SQLForWorker object.
	**/
	public SQLForWorker doExecute(MasterLoopContext context) {
		synchronized(this) {
			if(!didBuildPollutantAndProcessRequirements) {
				buildPollutantAndProcessRequirements();
				didBuildPollutantAndProcessRequirements = true;
			}
		}
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		SQLForWorker sqlForWorker = new SQLForWorker();
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();

		if(methaneProcessIDs.length() > 0) {
			enabledSectionNames.add("Methane");
		}
		if(nmogProcessIDs.length() > 0) {
			enabledSectionNames.add("NMOG");
		}
		if(nmhcProcessIDs.length() > 0) {
			enabledSectionNames.add("NMHC");
		}
		if(vocProcessIDs.length() > 0) {
			enabledSectionNames.add("VOC");
		}
		if(togProcessIDs.length() > 0) {
			enabledSectionNames.add("TOG");
		}

		String caseStatement = "";
		Connection db = null;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			caseStatement = buildOxyThreshCase(db);
		} catch(Exception e) {
			Logger.logError(e,"Unable to build oxygen threshold case statement");
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
		}

		replacements.put("##oxyThreshCase##",caseStatement);
		replacements.put("##e85FuelSubtypeIDs##","50,51");
		replacements.put("##e70FuelSubtypeIDs##","52");
		replacements.put("##e85E70FuelSubtypeIDs##","50,51,52");
		replacements.put("##hcPolProcessIDs##",hcPolProcessIDs);
		replacements.put("##methaneProcessIDs##",methaneProcessIDs);
		replacements.put("##nmhcProcessIDs##",nmhcProcessIDs);
		replacements.put("##nmogProcessIDs##",nmogProcessIDs);
		replacements.put("##vocProcessIDs##",vocProcessIDs);
		replacements.put("##togProcessIDs##",togProcessIDs);
		replacements.put("##hcProcessIDs##",hcProcessIDs);

		/*
		System.out.println("##hcPolProcessIDs##"+hcPolProcessIDs);
		System.out.println("##nmhcProcessIDs##"+nmhcProcessIDs);
		System.out.println("##nmogProcessIDs##"+nmogProcessIDs);
		System.out.println("##vocProcessIDs##"+vocProcessIDs);
		System.out.println("##togProcessIDs##"+togProcessIDs);
		System.out.println("##hcProcessIDs##"+hcProcessIDs);
		*/

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/HCSpeciationCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
