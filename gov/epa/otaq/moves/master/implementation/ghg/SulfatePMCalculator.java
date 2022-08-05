/**************************************************************************************************
 * @(#)SulfatePMCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

/**
 * @algorithm
 * @owner Sulfate PM Calculator
 * @calculator
 * @signup YEAR
**/
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

import java.util.*;
import java.sql.*;

/**
 * Calculate Sulfate PM2.5 Emissions for running, start, and extended idle processes
 * if called for in the ExecutionRunSpec.
 * This is done as a chained calculator to the  Total Energy calculator.
 *
 * @author		EPA Mitch Cumberworth
 * @author		EPA Ed Glover
 * @author      Wesley Faler
 * @version		2013-07-18
**/
public class SulfatePMCalculator extends EmissionCalculator {
	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.   Pollutant 115 - SulfatePM2.5 microns is registered.
	**/
	public SulfatePMCalculator() {
		int[] processIDs = {
			1, 2, 90, 91, 15, 16, 17
		};
		int[] pollutantIDs = getPollutantIDsToRegister(processIDs);
		Pollutant[] pollutants = new Pollutant[pollutantIDs.length];
		EmissionProcess[] processes = new EmissionProcess[processIDs.length];
		
		for(int i=0;i<pollutantIDs.length;i++) {
			pollutants[i] = Pollutant.findByID(pollutantIDs[i]);
		}
		for(int i=0;i<processIDs.length;i++) {
			processes[i] = EmissionProcess.findByID(processIDs[i]);
		}
		for(int i=0;i<pollutants.length;i++) {
			if(pollutants[i] != null) {
				for(int j=0;j<processes.length;j++) {
					if(processes[j] != null) {
						EmissionCalculatorRegistration.register(pollutants[i],processes[j],this);
					}
				}
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
		Pollutant pm25EC = Pollutant.findByID(112); // Unadjusted EC is an input but an output as well after it is adjusted
		Pollutant pm25NonEC = Pollutant.findByID(118); // Unadjusted NonECPM is an input but an output as well after it is adjusted

		TreeSet<EmissionCalculator> calculators = new TreeSet<EmissionCalculator>();
		LinkedList<EmissionCalculator> t = EmissionCalculatorRegistration.findPollutant(pm25EC);
		calculators.addAll(t);
		t = EmissionCalculatorRegistration.findPollutant(pm25NonEC);
		calculators.addAll(t);

		if(calculators.size() <= 0) {
			Logger.log(LogMessageCategory.INFO,"SulfatePMCalculator found no chain points");
		}

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				c.chainCalculator(this);
				Logger.log(LogMessageCategory.INFO,"SulfatePMCalculator chained to " + c);
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
		boolean foundPollutant = false;

		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Elemental Carbon",context.iterProcess.processName)) {
			foundPollutant = true;
		} else if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Composite - NonECPM",context.iterProcess.processName)) {
			foundPollutant = true;
		}

		if(!foundPollutant) {
			Logger.log(LogMessageCategory.INFO,"Sulfate Calculator's SQLForWorker method does NOT find any required input pollutants");
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		String crankcaseProcessName = "";
		String crankcaseProcessID = "";
		switch(context.iterProcess.databaseKey) {
			case 1: // Running Exhaust
				crankcaseProcessID = "15";
				crankcaseProcessName = "Crankcase Running Exhaust";
				break;
			case 2: // Start Exhaust
				crankcaseProcessID = "16";
				crankcaseProcessName = "Crankcase Start Exhaust";
				break;
			case 90: // Extended Idle Exhaust
				crankcaseProcessID = "17";
				crankcaseProcessName = "Crankcase Extended Idle Exhaust";
				break;
			case 91: // APU Exhaust
				// There is no APU crankcase process
				crankcaseProcessID = "";
				crankcaseProcessName = "";
				break;
		}
		String primaryAndCrankcaseProcessIDs = "" + context.iterProcess.databaseKey;
		if(crankcaseProcessID.length() > 0) {
			primaryAndCrankcaseProcessIDs += "," + crankcaseProcessID;
		}
		replacements.put("##primaryAndCrankcaseProcessIDs##",primaryAndCrankcaseProcessIDs);

		String primaryAndCrankcaseProcessIDsForPM25Total = "";
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Primary Exhaust PM2.5 - Total",context.iterProcess.processName)) {
			enabledSectionNames.add("MakePM2.5Total");
			primaryAndCrankcaseProcessIDsForPM25Total = "" + context.iterProcess.databaseKey;
		}
		if(crankcaseProcessName.length() > 0
				&& ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Primary Exhaust PM2.5 - Total",crankcaseProcessName)) {
			enabledSectionNames.add("MakePM2.5Total");
			if(primaryAndCrankcaseProcessIDsForPM25Total.length() > 0) {
				primaryAndCrankcaseProcessIDsForPM25Total += ",";
			}
			primaryAndCrankcaseProcessIDsForPM25Total += crankcaseProcessID;
		}
		replacements.put("##primaryAndCrankcaseProcessIDsForPM25Total##",primaryAndCrankcaseProcessIDsForPM25Total);

		String polProcessIDs = getPolProcessIDs("" + context.iterProcess.databaseKey, crankcaseProcessID);
		replacements.put("##polProcessIDs##",polProcessIDs);

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/SulfatePMCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			Logger.log(LogMessageCategory.INFO,"Sulfate Calculator doExecute method NOT successful");
			return null;
		}
	}

	/**
	 * Obtain the list of pollutants that could be output by this calculator.
	 * @param processIDs processes to be used
	 * @return array of polutant IDs, never empty, may be null
	**/
	int[] getPollutantIDsToRegister(int[] processIDs) {
		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			String processIDsText = "";
			for(int i=0;i<processIDs.length;i++) {
				if(processIDsText.length() > 0) {
					processIDsText += ",";
				}
				processIDsText += processIDs[i];
			}

			TreeSet<Integer> pollutantIDs = new TreeSet<Integer>();
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			sql = "select distinct outputPollutantID"
					+ " from PMSpeciation pm"
					+ " where pm.processID in (" + processIDsText + ")"
					+ " union"
					+ " select distinct ppa.pollutantID"
					+ " from pollutantProcessAssoc ppa"
					+ " inner join emissionProcess ep on (ep.processID=ppa.processID)"
					+ " where ppa.processID in (" + processIDsText + ")"
					+ " and ppa.pollutantID in (110,112,115,118,119,120)";
			query.open(db,sql);
			while(query.rs.next()) {
				int pollutantID = query.rs.getInt(1);
				pollutantIDs.add(Integer.valueOf(pollutantID));
			}
			if(pollutantIDs.size() <= 0) {
				return null;
			}
			int[] result = new int[pollutantIDs.size()];
			int i = 0;
			for(Integer pi : pollutantIDs) {
				result[i++] = pi.intValue();
			}
			return result;
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to get PM2.5 pollutant IDs",sql);
			return null;
		} finally {
			query.onFinally();
			if(db != null) {
				try {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				} catch(Exception e) {
					// Nothing to do here
				}
				db = null;
			}
		}
	}

	/** Pollutant/processes keyed by process ID **/
	TreeMap<String,String> polProcessIDsCache = new TreeMap<String,String>();

	/**
	 * Obtain all output pollutant process IDs for the primary and optional crankcase process.
	 * @param primaryProcessID process to be checked
	 * @param crankcaseProcessID optional crankcase process to be checked
	 * @return comma-separated list of pollutant/process IDs, never null or empty.
	**/
	String getPolProcessIDs(String primaryProcessID, String crankcaseProcessID) {
		String key = primaryProcessID + "|" + crankcaseProcessID;
		String result = polProcessIDsCache.get(key);
		if(result != null && result.length() > 0) {
			return result;
		}
		result = "";
		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			String processIDs = primaryProcessID;
			if(crankcaseProcessID.length() > 0) {
				processIDs += "," + crankcaseProcessID;
			}
			
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			sql = "select distinct pm.processID, outputPollutantID, processName, pollutantName"
					+ " from PMSpeciation pm"
					+ " inner join emissionProcess ep on (ep.processID=pm.processID)"
					+ " inner join pollutant p on (p.pollutantID=pm.outputPollutantID)"
					+ " where pm.processID in (" + processIDs + ")"
					+ " union"
					+ " select ppa.processID, ppa.pollutantID, processName, pollutantName"
					+ " from pollutantProcessAssoc ppa"
					+ " inner join emissionProcess ep on (ep.processID=ppa.processID)"
					+ " inner join pollutant p on (p.pollutantID=ppa.pollutantID)"
					+ " where ppa.processID in (" + processIDs + ")"
					+ " and ppa.pollutantID in (112,115,119,118,120,123,124)";
			query.open(db,sql);
			while(query.rs.next()) {
				int processID = query.rs.getInt(1);
				int pollutantID = query.rs.getInt(2);
				String processName = query.rs.getString(3);
				String pollutantName = query.rs.getString(4);

				if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(pollutantName,processName)) {
					if(result.length() > 0) {
						result += ",";
					}
					result += "" + ((pollutantID * 100) + processID);
				}
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to get PM2.5 pollutant/process IDs",sql);
		} finally {
			query.onFinally();
			if(db != null) {
				try {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				} catch(Exception e) {
					// Nothing to do here
				}
				db = null;
			}
		}
		if(result == null || result.length() <= 0) {
			result = "0";
		}
		polProcessIDsCache.put(key,result);
		//System.out.println("SulfatePMCalculator getPolProcessIDs(" + key + ") = " + result);
		return result;
	}
}
