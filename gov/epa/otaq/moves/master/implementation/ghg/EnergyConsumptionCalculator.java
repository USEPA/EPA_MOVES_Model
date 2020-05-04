/**************************************************************************************************
 * @(#)EnergyConsumptionCalculator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

import java.util.*;
import java.sql.*;

/*
import java.io.*;
import java.sql.*;
import java.util.*;
*/

/**
 * @algorithm
 * @owner Energy Consumption Calculator
 * @calculator
 * @notused
**/

/**
 * Calculate energy consumption (total, petroleum, and fossil fuel) for running exhaust,
 * start exhaust, extended idle exhaust, and well to pump emissions.
 * Operates at the YEAR level. The Start and Extended Idle processes operate at the
 * zone/year boundary, the Running Exhaust operates at Link/Year. This class registers
 * potential processes and pollutants handled by this calculator.  Builds SQL statements
 * for a distributed worker to execute. This is called by EnergyConsumptionCalculator.executeLoop.
 *
 * @author		Wesley Faler
 * @version		2013-01-19
**/
public class EnergyConsumptionCalculator extends EmissionCalculator {
	/** true if the "Running Exhaust" process should be considered **/
	boolean shouldCalcRunningExhaust = false;
	/** true if the "Start Exhaust" process should be considered **/
	boolean shouldCalcStartExhaust = false;
	/** true if the "Extended Idle Exhaust" process should be considered **/
	boolean shouldCalcExtendedIdleExhaust = false;
	/** true if the "Auxiliary Power Exhaust" process should be considered **/
	boolean shouldCalcAuxiliaryPowerExhaust = false;
	/** true if the petroleum energy should be calculated **/
	boolean shouldCalcPetroleumEnergy = false;
	/** true if the fossil fuel energy should be calculated **/
	boolean shouldCalcFossilFuelEnergy = false;
	/** true if the total energy should be calculated **/
	boolean shouldCalcTotalEnergy = false;

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public EnergyConsumptionCalculator() {
		Pollutant totalEnergy = Pollutant.findByID(91);
		Pollutant petroleumEnergy = Pollutant.findByID(92);
		Pollutant fossilEnergy = Pollutant.findByID(93);

		EmissionProcess runningExhaust = EmissionProcess.findByID(1);
		EmissionProcess startExhaust = EmissionProcess.findByID(2);
		EmissionProcess extendedIdleExhaust = EmissionProcess.findByID(90);
		EmissionProcess auxiliaryPowerExhaust = EmissionProcess.findByID(91);

		EmissionCalculatorRegistration.register(totalEnergy,runningExhaust,this);
		EmissionCalculatorRegistration.register(petroleumEnergy,runningExhaust,this);
		EmissionCalculatorRegistration.register(fossilEnergy,runningExhaust,this);

		EmissionCalculatorRegistration.register(totalEnergy,startExhaust,this);
		EmissionCalculatorRegistration.register(petroleumEnergy,startExhaust,this);
		EmissionCalculatorRegistration.register(fossilEnergy,startExhaust,this);

		EmissionCalculatorRegistration.register(totalEnergy,extendedIdleExhaust,this);
		EmissionCalculatorRegistration.register(petroleumEnergy,extendedIdleExhaust,this);
		EmissionCalculatorRegistration.register(fossilEnergy,extendedIdleExhaust,this);

		EmissionCalculatorRegistration.register(totalEnergy,auxiliaryPowerExhaust,this);
		EmissionCalculatorRegistration.register(petroleumEnergy,auxiliaryPowerExhaust,this);
		EmissionCalculatorRegistration.register(fossilEnergy,auxiliaryPowerExhaust,this);
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		/**
		 * @algorithm
		 * @signup Month
		**/

		// First, without considering interactions, just determine what is in the user's
		// runspec.
		boolean foundProcess = false;
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Running Exhaust")) {
			shouldCalcRunningExhaust = true;
			foundProcess = true;
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Start Exhaust")) {
			shouldCalcStartExhaust = true;
			foundProcess = true;
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Extended Idle Exhaust")) {
			shouldCalcExtendedIdleExhaust = true;
			foundProcess = true;
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				null,"Auxiliary Power Exhaust")) {
			shouldCalcAuxiliaryPowerExhaust = true;
			foundProcess = true;
		}

		if(!foundProcess) {
			Logger.log(LogMessageCategory.WARNING,
					"EnergyConsumptionCalculator created needlessly.");
			return;
		}

		// Now sign up for the minimal set of processes
		if(shouldCalcRunningExhaust) {
			EmissionProcess process = EmissionProcess.findByName("Running Exhaust");
			targetLoop.subscribe(this, process, MasterLoopGranularity.MONTH,
					MasterLoopPriority.EMISSION_CALCULATOR);
		}
		if(shouldCalcStartExhaust) {
			EmissionProcess process = EmissionProcess.findByName("Start Exhaust");
			targetLoop.subscribe(this, process, MasterLoopGranularity.MONTH,
					MasterLoopPriority.EMISSION_CALCULATOR);
		}
		if(shouldCalcExtendedIdleExhaust) {
			EmissionProcess process = EmissionProcess.findByName("Extended Idle Exhaust");
			targetLoop.subscribe(this, process, MasterLoopGranularity.MONTH,
					MasterLoopPriority.EMISSION_CALCULATOR);
		}
		if(shouldCalcAuxiliaryPowerExhaust) {
			EmissionProcess process = EmissionProcess.findByName("Auxiliary Power Exhaust");
			targetLoop.subscribe(this, process, MasterLoopGranularity.MONTH,
					MasterLoopPriority.EMISSION_CALCULATOR);
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
		// Determine which pollutant(s) should be calculated
		shouldCalcRunningExhaust = false;
		shouldCalcStartExhaust = false;
		shouldCalcExtendedIdleExhaust = false;
		shouldCalcAuxiliaryPowerExhaust = false;

		boolean foundProcess = false;
		boolean shouldCheckZone = false;
		if(context.iterProcess.processName.equalsIgnoreCase("Running Exhaust")) {
			shouldCalcRunningExhaust = true;
			foundProcess = true;
			shouldCheckZone = false;
		} else if(context.iterProcess.processName.equalsIgnoreCase("Start Exhaust")) {
			shouldCalcStartExhaust = true;
			foundProcess = true;
			shouldCheckZone = true;
		} else if(context.iterProcess.processName.equalsIgnoreCase("Extended Idle Exhaust")) {
			shouldCalcExtendedIdleExhaust = true;
			foundProcess = true;
			shouldCheckZone = true;
		} else if(context.iterProcess.processName.equalsIgnoreCase("Auxiliary Power Exhaust")) {
			shouldCalcAuxiliaryPowerExhaust = true;
			foundProcess = true;
			shouldCheckZone = true;
		}

		boolean foundPollutant = false;
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Petroleum Energy Consumption",context.iterProcess.processName)) {
			shouldCalcPetroleumEnergy = true;
			foundPollutant = true;
		} else {
			shouldCalcPetroleumEnergy = false;
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Fossil Fuel Energy Consumption",context.iterProcess.processName)) {
			shouldCalcFossilFuelEnergy = true;
			foundPollutant = true;
		} else {
			shouldCalcFossilFuelEnergy = false;
		}
		if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(
				"Total Energy Consumption",context.iterProcess.processName)) {
			shouldCalcTotalEnergy = true;
			foundPollutant = true;
		} else {
			shouldCalcTotalEnergy = false;
		}

		if(!foundPollutant || !foundProcess) {
			return null;
		}

		if(shouldCheckZone) {
			// For zone emissions, only work with the off-network road type.
			if(context.iterLocation.roadTypeRecordID != 1) {
				return null;
			}
		}

		// Note that there are no interactions between Total, Petroleum, and Fossil energy.
		// The database script puts total energy into a temporary table then only copies
		// it to the output if total energy is requested.  Petroleum and fossil energy
		// are determined from this temporary table.  Thus, there is no need to turn on
		// shouldCalcTotalEnergy just because shouldCalcPetroleumEnergy or
		// shouldCalcFossilFuelEnergy are true.

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();
		String pollutantProcessIDs = "";
		int polProcessID = 0;

		Connection executionDatabase = null;
		try {
			executionDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION);
		} catch(Exception e) {
			Logger.logError(e,"Unable to get the Execution Database connection needed for running"
					+ " Energy Consumption Calculations.");
			return null;
		}

		/*
		// The following logic is used to help debug missing output records
		// Basically, if there are SHO records, we expect output, so turn on logging
		// of extraction statements that result in no output.
		String sql = "SELECT DISTINCT SHO.* " +
				"FROM SHO " +
				"WHERE yearID = " + context.year + " " +
				"AND linkID = " + context.iterLocation.linkRecordID;
		PreparedStatement statement = null;
		ResultSet results = null;
		int count = 0;
		try {
			statement = executionDatabase.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null && results.next()) {
				count = results.getInt(1);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e, "Unable to get SHO count in ECC",sql);
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				results = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
		if(count < 1) {
			System.out.println("No matching SHO records in ECC");
			// Since there're no SHO records, there'll be no output anyway, so don't
			// bother complaining about tables with no entries.
			shouldDisplayEmptyExtractedDataQueries = false;
		} else {
			shouldDisplayEmptyExtractedDataQueries = true;
		}
		*/

		if(shouldCalcFossilFuelEnergy) {
			enabledSectionNames.add("Fossil Fuel Energy");
			PollutantProcessAssociation a = PollutantProcessAssociation.findByName(
					"Fossil Fuel Energy Consumption",context.iterProcess.processName);
			if(a != null) {
				if(pollutantProcessIDs.length() > 0) {
					pollutantProcessIDs += ",";
				}
				polProcessID = a.getDatabaseKey(executionDatabase);
				pollutantProcessIDs += polProcessID;
				if(a.doesHaveMultipleOperatingModes(executionDatabase,polProcessID)) {
					enabledSectionNames.add("Fossil Energy With OpModeDistribution");
				} else {
					enabledSectionNames.add("Fossil Energy Without OpModeDistribution");
					replacements.put("##FossilEnergyOpModeID##",
							"" + a.getSingleOpModeID(executionDatabase,polProcessID));
				}
			}
		}
		if(shouldCalcPetroleumEnergy) {
			enabledSectionNames.add("Petroleum Energy");
			PollutantProcessAssociation a = PollutantProcessAssociation.findByName(
					"Petroleum Energy Consumption",context.iterProcess.processName);
			if(a != null) {
				if(pollutantProcessIDs.length() > 0) {
					pollutantProcessIDs += ",";
				}
				polProcessID = a.getDatabaseKey(executionDatabase);
				pollutantProcessIDs += polProcessID;
				if(a.doesHaveMultipleOperatingModes(executionDatabase,polProcessID)) {
					enabledSectionNames.add("Petroleum Energy With OpModeDistribution");
				} else {
					enabledSectionNames.add("Petroleum Energy Without OpModeDistribution");
					replacements.put("##PetroleumEnergyOpModeID##",
							"" + a.getSingleOpModeID(executionDatabase,polProcessID));
				}
			}
		}
		if(shouldCalcTotalEnergy) {
			enabledSectionNames.add("Total Energy");
			PollutantProcessAssociation a = PollutantProcessAssociation.findByName(
					"Total Energy Consumption",context.iterProcess.processName);
			if(a != null) {
				if(pollutantProcessIDs.length() > 0) {
					pollutantProcessIDs += ",";
				}
				polProcessID = a.getDatabaseKey(executionDatabase);
				pollutantProcessIDs += polProcessID;
				if(a.doesHaveMultipleOperatingModes(executionDatabase,polProcessID)) {
					enabledSectionNames.add("Total Energy With OpModeDistribution");
				} else {
					enabledSectionNames.add("Total Energy Without OpModeDistribution");
					replacements.put("##TotalEnergyOpModeID##",
							"" + a.getSingleOpModeID(executionDatabase,polProcessID));
				}
			}
		}

		replacements.put("##pollutantProcessIDs##",pollutantProcessIDs);

		if(executionDatabase != null) {
			DatabaseConnectionManager.checkInConnection(
				MOVESDatabaseType.EXECUTION, executionDatabase);
			executionDatabase = null;
		}

		if(shouldCalcRunningExhaust) {
			enabledSectionNames.add("Running Exhaust");
		}
		if(shouldCalcStartExhaust) {
			enabledSectionNames.add("Start Exhaust");
		}
		if(shouldCalcExtendedIdleExhaust) {
			enabledSectionNames.add("Extended Idle Exhaust");
		}
		if(shouldCalcAuxiliaryPowerExhaust) {
			enabledSectionNames.add("Auxiliary Power Exhaust");
		}

		OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
			ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection();
		if(outputEmissionsBreakdownSelection.onRoadSCC) {
			enabledSectionNames.add("SCCOutput");
		} else {
			enabledSectionNames.add("NoSCCOutput");
		}

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/EnergyConsumptionCalculator.sql",enabledSectionNames,
				sqlForWorker);

		// NOTE: ECCP-6 is Well-To-Pump which is calculated in WellToPumpProcessor
		// ----- as a chained calculator.

		if(isOK) {
			return sqlForWorker;
		} else {
			return null;
		}
	}
}
