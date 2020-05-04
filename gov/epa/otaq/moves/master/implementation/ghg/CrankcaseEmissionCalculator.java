/**************************************************************************************************
 * @(#)CrankcaseEmissionCalculator.java
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
 * Perform Crankcase calculations as ratios to other pollutants.
 * @author		Wesley Faler
 * @author		Ed glover
 * @author		William Aikman
 * @version		2014-10-07
**/
public class CrankcaseEmissionCalculator extends EmissionCalculator {
	/**
	 * Prefix to place before each table used by the calculation script.
	 * Filled by derived classes.
	**/
	String tablePrefix = "";
	/** List of pollutant IDs served by this calculator.  Filled by derived classes. **/
	int[] pollutantIDs = null;
	/*
		1, 2, 3, 5, 6,
		20, 21, 22, 23, 24, 25, 26, 27,
		30, 31, 32, 33,
		79, 80, 86, 87
	*/
	/** Sulfate PM 10 ID **/
	int sPM10ID = 105;
	/** List of process IDs served by this calculator and their sources, as pairs **/
	int[] processIDs = {
		15, 1,
		16, 2,
		17, 90
	};

	/** List of pollutants served by this calculator **/
	ArrayList<Pollutant> pollutants = new ArrayList<Pollutant>();

	class ProcessPair {
		public EmissionProcess process;
		public EmissionProcess sourceProcess;
	}
	/** List of processes served by this calculator **/
	ArrayList<ProcessPair> processes = new ArrayList<ProcessPair>();

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	 * @param pollutantIDsToUse array of pollutant IDs served by this calculator
	**/
	public CrankcaseEmissionCalculator(int[] pollutantIDsToUse) {
		pollutantIDs = pollutantIDsToUse;

		for(int i=0;i<pollutantIDs.length;i++) {
			Pollutant p = Pollutant.findByID(pollutantIDs[i]);
			if(p != null) {
				pollutants.add(p);
			}
		}
		for(int i=0;i<processIDs.length;i+=2) {
			EmissionProcess p = EmissionProcess.findByID(processIDs[i+0]);
			EmissionProcess s = EmissionProcess.findByID(processIDs[i+1]);
			if(p != null && s != null) {
				ProcessPair pair = new ProcessPair();
				pair.process = p;
				pair.sourceProcess = s;
				processes.add(pair);
			}
		}
		for(Iterator<ProcessPair> i=processes.iterator();i.hasNext();) {
			ProcessPair p = i.next();
			for(Iterator<Pollutant> j=pollutants.iterator();j.hasNext();) {
				EmissionCalculatorRegistration.register(j.next(),p.process,this);
			}
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		// Chain to everything except the sulfate PM 10 pollutant

		LinkedList<EmissionCalculator> calculators = new LinkedList<EmissionCalculator>();
		for(Iterator<ProcessPair> i=processes.iterator();i.hasNext();) {
			ProcessPair pair = i.next();
			for(Iterator<Pollutant> j=pollutants.iterator();j.hasNext();) {
				Pollutant p = j.next();
				if(p.databaseKey == sPM10ID) {
					continue;
				}
				if(!ExecutionRunSpec.theExecutionRunSpec.
						doesHavePollutantAndProcess(p,pair.process)) {
					continue;
				}
				LinkedList<EmissionCalculator> t 
						= EmissionCalculatorRegistration.find(p,pair.sourceProcess);
				if(t != null && t.size() > 0) {
					EmissionCalculatorRegistration.merge(calculators,t);
				}
			}
		}

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				c.chainCalculator(this);
			}
		}
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
		// Determine the output process
		EmissionProcess outputProcess = null;
		for(Iterator<ProcessPair> i=processes.iterator();i.hasNext();) {
			ProcessPair pair = i.next();
			if(context.hasProcess(pair.sourceProcess)) {
				outputProcess = pair.process;
				break;
			}
		}
		if(outputProcess == null) {
			return null;
		}

		// Determine which pollutant(s) should be calculated
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		String pollutantIDsText = "";

		boolean foundPollutant = false;
		for(Iterator<Pollutant> i=pollutants.iterator();i.hasNext();) {
			Pollutant p = i.next();
			if(!ExecutionRunSpec.theExecutionRunSpec.
					doesHavePollutantAndProcess(p,outputProcess)) {
				continue;
			}
			if(pollutantIDsText.length() > 0) {
				pollutantIDsText += ",";
			}
			pollutantIDsText += "" + p.databaseKey;
			foundPollutant = true;
			if(p.databaseKey == sPM10ID) {
				enabledSectionNames.add("SulfatePM10");
			}
		}

		if(!foundPollutant) {
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		context.addChainedProcess(outputProcess);
		replacements.put("##outputProcessID##",""+outputProcess.databaseKey);
		replacements.put("##pollutantIDs##",pollutantIDsText);
		replacements.put("##prefix##",tablePrefix);
		if(tablePrefix != null && tablePrefix.length() > 0) {
			enabledSectionNames.add(tablePrefix);
		}
		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/CrankcaseEmissionCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
