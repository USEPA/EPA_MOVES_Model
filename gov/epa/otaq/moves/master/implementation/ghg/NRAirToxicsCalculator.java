/**************************************************************************************************
 * @(#)NRAirToxicsCalculator.java
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
 * Perform Air Toxics calculations for Nonroad
 * @author		Wesley Faler
 * @version		2015-04-07
**/
public class NRAirToxicsCalculator extends EmissionCalculator implements EmissionCalculatorExternal {
	/**
	 * @algorithm
	 * @owner Nonroad Air toxics Calculator
	 * @calculator
	**/

	/** Summary class holding details of the ATRatio* tables **/
	class ATRatioEntry {
		String tableName;
		int outputPolProcessID;
		PollutantProcessAssociation output;
		PollutantProcessAssociation input;

		ATRatioEntry(String tableNameToUse, int outputPolProcessIDToUse, int inputPollutantID) {
			tableName = tableNameToUse;
			outputPolProcessID = outputPolProcessIDToUse;
			output = PollutantProcessAssociation.createByID(outputPolProcessID);
			if(output != null && inputPollutantID > 0) {
				input = PollutantProcessAssociation.createByID(inputPollutantID,output.emissionProcess.databaseKey);
			}
		}
	}

	/** Cached results of the getIntegratedSpecies() function. **/
	static ArrayList<Pollutant> cachedIntegratedSpecies = null;

	/**
	 * Retrieve the set of pollutants in the nrIntegratedSpecies table.
	 * Combine these with NMOG (80) to get the pollutants required to compute
	 * NonHapTOG (88).
	 * @param db database to be used.
	 * @return a list of pollutants, may be empty, never null.
	 * @throws SQLException if anything goes wrong.
	**/
	public static ArrayList<Pollutant> getIntegratedSpecies(Connection db) throws SQLException {
		if(cachedIntegratedSpecies != null) {
			return cachedIntegratedSpecies;
		}
		// NMOG (80) is always required. It is already TOG - Methane so we avoid
		// chaining to TOG and Methane.
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "select distinct pollutantID"
				+ " from nrIntegratedSpecies"
				+ " union"
				+ " select distinct pollutantID"
				+ " from pollutant"
				+ " where pollutantID = 80";
		try {
			ArrayList<Pollutant> results = new ArrayList<Pollutant>();
			query.open(db,sql);
			while(query.rs.next()) {
				int pollutantID = query.rs.getInt(1);
				Pollutant pollutant = Pollutant.findByID(pollutantID);
				if(pollutant != null) {
					results.add(pollutant);
				}
			}
			query.close();
			cachedIntegratedSpecies = results;
			return results;
		} catch(SQLException e) {
			Logger.logError(e,"Unable to query NR NonHapTOG prerequisites: " + sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}

	/** List of ATRatioEntry objects **/
	ArrayList<ATRatioEntry> entries = new ArrayList<ATRatioEntry>();
	/** True if loadRatioEntries() has been called **/
	boolean didLoadRatioEntries = false;

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public NRAirToxicsCalculator() {
		loadPlaceholderRatioEntries();

		for(Iterator i=entries.iterator();i.hasNext();) {
			ATRatioEntry a = (ATRatioEntry)i.next();
			EmissionCalculatorRegistration.register(a.output.pollutant,a.output.emissionProcess,this);
		}
	}

	/**
	 * Fill tne entries list with placeholder values for all possible pollutants and processes
	 * that this calculator could handle.
	**/
	private void loadPlaceholderRatioEntries() {
		entries.clear();

		int[] possibleValues = {
			/*
			2001,2015,2018,2019,2020,2021,2030,2031,2032,
			2101,2115,2118,2119,2120,2121,2130,2131,2132,
			2201,2215,2218,2219,2220,2221,2230,2231,2232,
			2301,2315,2318,2319,2320,2321,2330,2331,2332,
			2401,2415,2418,2419,2420,2421,2430,2431,2432,
			2501,2515,2518,2519,2520,2521,2530,2531,2532,
			2601,2615,2618,2619,2620,2621,2630,2631,2632,
			2701,2715,2718,2719,2720,2721,2730,2731,2732
			*/
			2001,2015,2018,2019,2020,2021,2022,2023,2024,2030,
			2031,2032,2101,2115,2118,2119,2120,2121,2122,2123,
			2124,2130,2131,2132,2201,2215,2218,2219,2220,2221,
			2222,2223,2224,2230,2231,2232,2301,2315,2401,2415,
			2501,2515,2601,2615,2701,2715,4001,4015,4018,4019,
			4020,4021,4022,4023,4024,4030,4031,4032,4101,4115,
			4118,4119,4120,4121,4122,4123,4124,4130,4131,4132,
			4201,4215,4218,4219,4220,4221,4222,4223,4224,4230,
			4231,4232,4301,4315,4401,4415,4501,4515,4518,4519,
			4520,4521,4522,4523,4524,4530,4531,4532,4601,4615,
			4618,4619,4620,4621,4622,4623,4624,4630,4631,4632,
			6001,6101,6201,6301,6501,6601,6701,6801,6815,6901,
			6915,7001,7015,7101,7115,7201,7215,7301,7315,7401,
			7415,7501,7515,7601,7615,7701,7715,7801,7815,8101,
			8115,8201,8215,8301,8315,8401,8415,
			13001,13101,13201,13301,13401,13501,13601,13701,13801,13901,
			14001,14101,14201,14301,14401,14501,14601,16801,16815,16901,
			16915,17001,17015,17101,17115,17201,17215,17301,17315,17401,
			17415,17501,17515,17601,17615,17701,17715,17801,17815,18101,
			18115,18201,18215,18301,18315,18401,18415,18501,18515,

			8801,8815,8818,8819,8820,8821,8830,8831,8832
		};
		TreeSet<Integer> values = new TreeSet<Integer>();
		for(int i=0;i<possibleValues.length;i++) {
			Integer v = new Integer(possibleValues[i]);
			values.add(v);
		}

		for(Iterator<Integer> i=values.iterator();i.hasNext();) {
			Integer v = i.next();
			ATRatioEntry a = new ATRatioEntry("placeholder",v.intValue(),0);
			if(a.output != null) {
				entries.add(a);
			}
		}
	}

	/** Fill the entries list from the ATRatio* tables **/
	private void loadRatioEntries() {
		entries.clear();

		String sql = "";
		String[] tableNames = {
			"nrATRatio", "87",
			"nrDioxinEmissionRate", "99",
			"nrMetalEmissionRate", "99",
			"nrPAHGasRatio", "87",
			"nrPAHParticleRatio", "110",
			"NonHAPTOG", "80" // special case, not really a table name
		};

		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();

		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			for(int i=0;i<tableNames.length;i+=2) {
				int inputPollutantID = Integer.parseInt(tableNames[i+1]);
				if(tableNames[i].equalsIgnoreCase("NonHAPTOG")) {
					sql = "select pollutantID, processID from pollutantProcessAssoc where pollutantID=88 and isAffectedByNonroad=1";
				} else {
					sql = "select distinct pollutantID, processID from " + tableNames[i];
				}
				query.open(db,sql);
				while(query.rs.next()) {
					int pollutantID = query.rs.getInt(1);
					int processID = query.rs.getInt(2);
					ATRatioEntry a = new ATRatioEntry(tableNames[i],pollutantID*100+processID,inputPollutantID);
					if(a.output != null) {
						entries.add(a);
					}
				}
				query.close();
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to load nrATRatio entries",sql);
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
		Logger.log(LogMessageCategory.INFO,"NRAirToxicsCalc subscribing...");
		if(!didLoadRatioEntries) {
			didLoadRatioEntries = true;
			entries.clear();
			loadRatioEntries();
		}

		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		LinkedList<EmissionCalculator> calculators = new LinkedList<EmissionCalculator>();

		for(Iterator<ATRatioEntry> i=entries.iterator();i.hasNext();) {
			ATRatioEntry a = (ATRatioEntry)i.next();
			if(a.input != null) {
				LinkedList<EmissionCalculator> t = EmissionCalculatorRegistration.find(a.input.pollutant,a.input.emissionProcess);
				EmissionCalculatorRegistration.merge(calculators,t);
			}
		}

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				Logger.log(LogMessageCategory.INFO,"nrATCalc chained to " + c.getClass().getSimpleName());
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
		//System.out.println("NRAirToxicsCalculator context process: " + context.iterProcess.databaseKey);
		if(!didLoadRatioEntries) {
			didLoadRatioEntries = true;
			entries.clear();
			loadRatioEntries();
		}

		// Determine which pollutant(s) should be calculated
		String outputPolProcessIDs = "";
		boolean foundPollutant = false;
		boolean foundATRatio = false;
		String outputATRatio = "";
		String inputATRatio = "";
		TreeSet<String> allInput = new TreeSet<String>();
		TreeSet<String> allOutputPolProcessIDs = new TreeSet<String>();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		TreeMap<String,String> tablePolProcessIDs = new TreeMap<String,String>();
		TreeSetIgnoreCase usedTables = new TreeSetIgnoreCase();

		for(Iterator<ATRatioEntry> i=entries.iterator();i.hasNext();) {
			ATRatioEntry a = (ATRatioEntry)i.next();
			/* Nonroad does all processes at once, so don't filter here.
			if(!context.hasProcess(a.input.emissionProcess)) {
				continue;
			}
			*/
			if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					a.input.pollutant.pollutantName,a.input.emissionProcess.processName)) {
				foundPollutant = true;
				enabledSectionNames.add("Use" + a.tableName);
				usedTables.add(a.tableName);

				String t = "" + a.outputPolProcessID;

				String ppIDs = StringUtilities.safeGetString(tablePolProcessIDs.get(a.tableName));
				if(ppIDs.length() > 0) {
					ppIDs += ",";
				}
				ppIDs += t;
				tablePolProcessIDs.put(a.tableName,ppIDs);

				if(!allOutputPolProcessIDs.contains(t)) {
					allOutputPolProcessIDs.add(t);
					if(outputPolProcessIDs.length() > 0) {
						outputPolProcessIDs += ",";
					}
					outputPolProcessIDs += "" + a.outputPolProcessID;
				}
				String inputPolProcessID = "" + a.input.getDatabaseKey();
				if(a.tableName.equalsIgnoreCase("nrATRatio")) {
					if(!foundATRatio) {
						foundATRatio = true;
						enabledSectionNames.add("UseNRATRatio");
					}
					if(outputATRatio.length()>0) {
						outputATRatio += ",";
					}
					outputATRatio += "" + a.outputPolProcessID;
				}
			}
		}

		if(!foundPollutant) {
			//System.out.println("NRAirToxicsCalculator found nothing to run");
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();

		replacements.put("##ATOutputPolProcessIDs##",outputPolProcessIDs);
		replacements.put("##outputATRatio##",outputATRatio);

		for(String tableName : tablePolProcessIDs.keySet()) {
			String ppIDs = tablePolProcessIDs.get(tableName);
			String t = "##output" + tableName + "##";
			replacements.put(t,ppIDs);
			//System.out.println(t+ppIDs);
		}

		/*
		Logger.log(LogMessageCategory.INFO,"##ATOutputPolProcessIDs##"+outputPolProcessIDs);
		Logger.log(LogMessageCategory.INFO,"##outputATRatio##"+outputATRatio);
		*/

		if(usedTables.contains("NonHAPTOG")) {
			sqlForWorker.externalModules.add("nrNonHAPTOG");
		}

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/NRAirToxicsCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
