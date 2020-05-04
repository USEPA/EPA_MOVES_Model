/**************************************************************************************************
 * @(#)AirToxicsCalculator.java
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
 * Perform Air Toxics calculations
 * @author		Wesley Faler
 * @version		2015-04-07
**/
public class AirToxicsCalculator extends EmissionCalculator {
	/**
	 * @algorithm
	 * @owner Air toxics Calculator
	 * @calculator
	**/

	/** Summary class holding details of the ATRatio* tables **/
	class ATRatioEntry {
		String tableName;
		int outputPolProcessID;
		PollutantProcessAssociation output;
		PollutantProcessAssociation input;

		ATRatioEntry(String tableNameToUse, int outputPolProcessIDToUse) {
			tableName = tableNameToUse;
			outputPolProcessID = outputPolProcessIDToUse;
			output = PollutantProcessAssociation.createByID(outputPolProcessID);
			if(output != null) {
				TreeSet inputIDs = PollutantProcessAssociation.chainedTo(outputPolProcessID);
				if(inputIDs != null && inputIDs.size() > 0) {
					for(Iterator iterInputIDs = inputIDs.iterator();iterInputIDs.hasNext();) {
						Integer chainedToObject = (Integer)iterInputIDs.next();
						int chainedTo = chainedToObject.intValue();
						input = PollutantProcessAssociation.createByID(chainedTo);
					}
				}
			}
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
	public AirToxicsCalculator() {
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
			2001,2002,2011,2012,2013,2018,2019,2201,2202,2211,2212,
			2213,2218,2219,2401,2402,2501,2502,2601,2602,

			2101,2102,2111,2112,2113,2118,2119,2301,2302,2311,2312,
			2313,2318,2319,2701,2702,

			2001,2002,2011,2012,2013,2018,2019,2090,2091,2101,2102,2111,
			2112,2113,2118,2119,2201,2202,2211,2212,2213,2218,2219,
			2311,2312,2313,2318,2319,2390,2391,2401,2402,2490,2491,
			2501,2502,2590,2591,2601,2602,2690,2691,2701,2702,2790,2791,

			2301,2302,4001,4002,4011,4012,4013,4018,4019,4090,4091,4101,4102,4111,4112,
			4113,4118,4119,4190,4191,4201,4202,4211,4212,4213,4218,4219,4290,4291,4301,4302,
			4390,4391,4401,4402,4490,4491,4501,4502,4511,4512,4513,4518,4519,4590,4591,4601,4602,
			4611,4612,4613,4618,4619,4690,4691,6801,6802,6901,6902,7001,7002,7101,7102,
			7201,7202,7301,7302,7401,7402,7501,7502,7601,7602,7701,7702,7801,7802,
			8101,8102,8201,8202,8301,8302,8401,8402,16801,16802,16890,16891,16901,16902,
			16990,16991,17001,17002,17090,17091,17101,17102,17190,17191,17201,17202,17290,17291,17301,17302,
			17390,17391,17401,17402,17490,17491,17501,17502,17590,17591,17601,17602,17690,17691,17701,17702,
			17790,17791,17801,17802,17890,17891,18101,18102,18190,18191,18201,18202,18290,18291,18301,18302,
			18390,18391,18401,18402,18490,18491,18501,18502,18511,18512,18513,18518,18519,18590,18591
		};
		TreeSet<Integer> values = new TreeSet<Integer>();
		for(int i=0;i<possibleValues.length;i++) {
			Integer v = new Integer(possibleValues[i]);
			values.add(v);
		}

		for(Iterator<Integer> i=values.iterator();i.hasNext();) {
			Integer v = i.next();
			ATRatioEntry a = new ATRatioEntry("placeholder",v.intValue());
			if(a.output != null && a.input != null) {
				entries.add(a);
			}
		}
	}

	/** Fill the entries list from the ATRatio* tables **/
	private void loadRatioEntries() {
		entries.clear();

		String sql = "";
		String[] tableNames = {
			"ATRatio",
			"ATRatioGas2",
			"ATRatioNonGas",
			"minorHAPRatio",
			"pahGasRatio",
			"pahParticleRatio"
		};

		Connection db = null;
		SQLRunner.Query query = new SQLRunner.Query();

		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			for(int i=0;i<tableNames.length;i++) {
				sql = "select distinct polProcessID from " + tableNames[i];
				query.open(db,sql);
				while(query.rs.next()) {
					ATRatioEntry a = new ATRatioEntry(tableNames[i],query.rs.getInt(1));
					if(a.output != null && a.input != null) {
						entries.add(a);
					}
				}
				query.close();
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to load ATRatio entries",sql);
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
		// This is a chained calculator, so don't subscribe to the MasterLoop.  Instead,
		// find calculators that produce data this one needs.
		LinkedList<EmissionCalculator> calculators = new LinkedList<EmissionCalculator>();

		for(Iterator<ATRatioEntry> i=entries.iterator();i.hasNext();) {
			ATRatioEntry a = (ATRatioEntry)i.next();
			LinkedList<EmissionCalculator> t =
					EmissionCalculatorRegistration.find(a.input.pollutant,a.input.emissionProcess);
			EmissionCalculatorRegistration.merge(calculators,t);
		}

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				//System.out.println("ATCalc chained to " + c.getClass().getSimpleName());
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
		//System.out.println("AirToxicsCalculator context process: " + context.iterProcess.databaseKey);
		if(!didLoadRatioEntries) {
			didLoadRatioEntries = true;
			entries.clear();
			loadRatioEntries();
		}

		// Determine which pollutant(s) should be calculated
		String outputPolProcessIDs = "";
		boolean foundPollutant = false;
		boolean foundATRatioGas1 = false;
		boolean foundATRatioGas2 = false;
		boolean foundATRatioNonGas = false;
		String outputATRatioGas1 = "";
		String outputATRatioGas2 = "";
		String outputATRatioNonGas = "";
		String inputATRatioGas1 = "";
		String inputATRatioGas2 = "";
		String inputATRatioNonGas = "";
		TreeSet<String> allInputGas1 = new TreeSet<String>();
		TreeSet<String> allInputGas2 = new TreeSet<String>();
		TreeSet<String> allInputNonGas = new TreeSet<String>();
		TreeSet<String> allOutputPolProcessIDs = new TreeSet<String>();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		boolean foundMinorHAPRatio = false;
		boolean foundPAHGasRatio = false;
		boolean foundPAHParticleRatio = false;
		String outputMinorHAPRatio = "";
		String outputPAHGasRatio = "";
		String outputPAHParticleRatio = "";
		String inputMinorHAPRatio = "";
		String inputPAHGasRatio = "";
		String inputPAHParticleRatio = "";
		TreeSet<String> allInputMinorHAPRatio = new TreeSet<String>();
		TreeSet<String> allInputPAHGasRatio = new TreeSet<String>();
		TreeSet<String> allInputPAHParticleRatio = new TreeSet<String>();

		for(Iterator<ATRatioEntry> i=entries.iterator();i.hasNext();) {
			ATRatioEntry a = (ATRatioEntry)i.next();

			if(!context.hasProcess(a.input.emissionProcess)) {
				continue;
			}
			if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
					a.input.pollutant.pollutantName,a.input.emissionProcess.processName)) {
				foundPollutant = true;
				String t = "" + a.outputPolProcessID;
				if(!allOutputPolProcessIDs.contains(t)) {
					allOutputPolProcessIDs.add(t);
					if(outputPolProcessIDs.length() > 0) {
						outputPolProcessIDs += ",";
					}
					outputPolProcessIDs += "" + a.outputPolProcessID;
				}
				String inputPolProcessID = "" + a.input.getDatabaseKey();
				if(a.tableName.equalsIgnoreCase("ATRatio")) {
					if(!foundATRatioGas1) {
						foundATRatioGas1 = true;
						enabledSectionNames.add("UseATRatioGas1");
					}
					if(outputATRatioGas1.length()>0) {
						outputATRatioGas1 += ",";
					}
					outputATRatioGas1 += "" + a.outputPolProcessID;

					if(!allInputGas1.contains(inputPolProcessID)) {
						allInputGas1.add(inputPolProcessID);
						if(inputATRatioGas1.length() > 0) {
							inputATRatioGas1 += ",";
						}
						inputATRatioGas1 += inputPolProcessID;
					}
				}
				if(a.tableName.equalsIgnoreCase("ATRatioGas2")) {
					if(!foundATRatioGas2) {
						foundATRatioGas2 = true;
						enabledSectionNames.add("UseATRatioGas2");
					}
					if(outputATRatioGas2.length()>0) {
						outputATRatioGas2 += ",";
					}
					outputATRatioGas2 += "" + a.outputPolProcessID;

					if(!allInputGas2.contains(inputPolProcessID)) {
						allInputGas2.add(inputPolProcessID);
						if(inputATRatioGas2.length() > 0) {
							inputATRatioGas2 += ",";
						}
						inputATRatioGas2 += inputPolProcessID;
					}
				}
				if(a.tableName.equalsIgnoreCase("ATRatioNonGas")) {
					if(!foundATRatioNonGas) {
						foundATRatioNonGas = true;
						enabledSectionNames.add("UseATRatioNonGas");
					}
					if(outputATRatioNonGas.length()>0) {
						outputATRatioNonGas += ",";
					}
					outputATRatioNonGas += "" + a.outputPolProcessID;

					if(!allInputNonGas.contains(inputPolProcessID)) {
						allInputNonGas.add(inputPolProcessID);
						if(inputATRatioNonGas.length() > 0) {
							inputATRatioNonGas += ",";
						}
						inputATRatioNonGas += inputPolProcessID;
					}
				}
				if(a.tableName.equalsIgnoreCase("MinorHAPRatio")) {
					if(!foundMinorHAPRatio) {
						foundMinorHAPRatio = true;
						enabledSectionNames.add("UseMinorHAPRatio");
					}
					if(outputMinorHAPRatio.length()>0) {
						outputMinorHAPRatio += ",";
					}
					outputMinorHAPRatio += "" + a.outputPolProcessID;

					if(!allInputMinorHAPRatio.contains(inputPolProcessID)) {
						allInputMinorHAPRatio.add(inputPolProcessID);
						if(inputMinorHAPRatio.length() > 0) {
							inputMinorHAPRatio += ",";
						}
						inputMinorHAPRatio += inputPolProcessID;
					}
				}
				if(a.tableName.equalsIgnoreCase("pahGasRatio")) {
					if(!foundPAHGasRatio) {
						foundPAHGasRatio = true;
						enabledSectionNames.add("UsePAHGasRatio");
					}
					if(outputPAHGasRatio.length()>0) {
						outputPAHGasRatio += ",";
					}
					outputPAHGasRatio += "" + a.outputPolProcessID;

					if(!allInputPAHGasRatio.contains(inputPolProcessID)) {
						allInputPAHGasRatio.add(inputPolProcessID);
						if(inputPAHGasRatio.length() > 0) {
							inputPAHGasRatio += ",";
						}
						inputPAHGasRatio += inputPolProcessID;
					}
				}
				if(a.tableName.equalsIgnoreCase("pahParticleRatio")) {
					if(!foundPAHParticleRatio) {
						foundPAHParticleRatio = true;
						enabledSectionNames.add("UsePAHParticleRatio");
					}
					if(outputPAHParticleRatio.length()>0) {
						outputPAHParticleRatio += ",";
					}
					outputPAHParticleRatio += "" + a.outputPolProcessID;

					if(!allInputPAHParticleRatio.contains(inputPolProcessID)) {
						allInputPAHParticleRatio.add(inputPolProcessID);
						if(inputPAHParticleRatio.length() > 0) {
							inputPAHParticleRatio += ",";
						}
						inputPAHParticleRatio += inputPolProcessID;
					}
				}
			}
		}

		if(!foundPollutant) {
			//System.out.println("AirToxicsCalculator found nothing to run");
			return null;
		}

		SQLForWorker sqlForWorker = new SQLForWorker();
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();

		replacements.put("##ATOutputPolProcessIDs##",outputPolProcessIDs);
		replacements.put("##outputATRatioGas1##",outputATRatioGas1);
		replacements.put("##outputATRatioGas2##",outputATRatioGas2);
		replacements.put("##outputATRatioNonGas##",outputATRatioNonGas);

		replacements.put("##outputMinorHAPRatio##",outputMinorHAPRatio);
		replacements.put("##outputPAHGasRatio##",outputPAHGasRatio);
		replacements.put("##outputPAHParticleRatio##",outputPAHParticleRatio);

		replacements.put("##inputATRatioGas1##",inputATRatioGas1);
		replacements.put("##inputATRatioGas2##",inputATRatioGas2);
		replacements.put("##inputATRatioNonGas##",inputATRatioNonGas);

		replacements.put("##inputMinorHAPRatio##",inputMinorHAPRatio);
		replacements.put("##inputPAHGasRatio##",inputPAHGasRatio);
		replacements.put("##inputPAHParticleRatio##",inputPAHParticleRatio);

		/*
		System.out.println("##ATOutputPolProcessIDs##"+outputPolProcessIDs);
		System.out.println("##outputATRatioGas1##"+outputATRatioGas1);
		System.out.println("##outputATRatioGas2##"+outputATRatioGas2);
		System.out.println("##outputATRatioNonGas##"+outputATRatioNonGas);
		System.out.println("##inputATRatioGas1##"+inputATRatioGas1);
		System.out.println("##inputATRatioGas2##"+inputATRatioGas2);
		System.out.println("##inputATRatioNonGas##"+inputATRatioNonGas);

		System.out.println("##outputMinorHAPRatio##"+outputMinorHAPRatio);
		System.out.println("##outputPAHGasRatio##"+outputPAHGasRatio);
		System.out.println("##outputPAHParticleRatio##"+outputPAHParticleRatio);
		System.out.println("##inputMinorHAPRatio##"+inputMinorHAPRatio);
		System.out.println("##inputPAHGasRatio##"+inputPAHGasRatio);
		System.out.println("##inputPAHParticleRatio##"+inputPAHParticleRatio);
		*/

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/AirToxicsCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
