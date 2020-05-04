/**************************************************************************************************
 * @(#)PollutantProcessAssociation.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.SQLRunner;

import java.sql.*;
import java.util.*;

/**
 * An object that parallels the database joining record. Used internally by 
 * PollutantProcessLoader to establish the connections between Pollutant and EmissionProcess.
 * It includes an emissionProcess and a pollutant as members.
 *
 * @author		Wesley Faler
 * @version		2015-02-05
**/
public class PollutantProcessAssociation implements Comparable {
	/**
	 * Map of required input polProcessID for each output polProcessID for Onroad.  Keys are Integer objects
	 * and the data for each is a TreeSet of Integer objects holding the input polProcessIDs.
	**/
	private static TreeMap<Integer,TreeSet<Integer> > chainedToMap = new TreeMap<Integer,TreeSet<Integer> >();

	/**
	 * Map of required input polProcessID for each output polProcessID for Nonroad.  Keys are Integer objects
	 * and the data for each is a TreeSet of Integer objects holding the input polProcessIDs.
	**/
	private static TreeMap<Integer,TreeSet<Integer> > chainedToMapNR = new TreeMap<Integer,TreeSet<Integer> >();

	/**
	 * Resets all chaining information.  Done prior to rescanning a database.
	**/
	public static void clearChainedTo() {
		chainedToMap.clear();
		chainedToMapNR.clear();
	}

	/**
	 * Record the dependence of one pollutant/process on another for Onroad.
	 * @param outputPolProcessID the pollutant created by the input pollutant
	 * @param inputPolProcessID the pollutant requried to produce the output pollutant
	**/
	public static void addChainedTo(int outputPolProcessID, int inputPolProcessID) {
		Integer output = new Integer(outputPolProcessID);
		Integer input = new Integer(inputPolProcessID);
		TreeSet<Integer> inputSet = chainedToMap.get(output);
		if(inputSet == null) {
			inputSet = new TreeSet<Integer>();
			chainedToMap.put(output,inputSet);
		}
		inputSet.add(input);
	}

	/**
	 * Record the dependence of one pollutant/process on another for Nonroad.
	 * @param outputPolProcessID the pollutant created by the input pollutant
	 * @param inputPolProcessID the pollutant requried to produce the output pollutant
	**/
	public static void addChainedToNR(int outputPolProcessID, int inputPolProcessID) {
		Integer output = new Integer(outputPolProcessID);
		Integer input = new Integer(inputPolProcessID);
		TreeSet<Integer> inputSet = chainedToMapNR.get(output);
		if(inputSet == null) {
			inputSet = new TreeSet<Integer>();
			chainedToMapNR.put(output,inputSet);
		}
		inputSet.add(input);
	}

	/**
	 * Determine if a pollutant/process requires another for its calculation for Onroad.
	 * @param polProcessID pollutant/process to be checked
	 * @return null if there are no dependencies, otherwise a TreeSet of polProcessID Integer
	 * objects for the required inputs.
	**/
	public static TreeSet<Integer> chainedTo(int polProcessID) {
		Integer output = new Integer(polProcessID);
		TreeSet<Integer> inputSet = chainedToMap.get(output);
		if(inputSet != null && inputSet.size() <= 0) {
			inputSet = null;
		}
		return inputSet;
	}

	/**
	 * Determine if a pollutant/process requires another for its calculation for Nonroad.
	 * @param polProcessID pollutant/process to be checked
	 * @return null if there are no dependencies, otherwise a TreeSet of polProcessID Integer
	 * objects for the required inputs.
	**/
	public static TreeSet<Integer> chainedToNR(int polProcessID) {
		Integer output = new Integer(polProcessID);
		TreeSet<Integer> inputSet = chainedToMapNR.get(output);
		if(inputSet != null && inputSet.size() <= 0) {
			inputSet = null;
		}
		return inputSet;
	}

	/** From master pollutant list **/
	public Pollutant pollutant;
	
	/** From master emission process list **/
	public EmissionProcess emissionProcess;

	/** For Onroad **/
	public boolean isAffectedByOnroad;
	public boolean isChosenForOnroad = false;
	
	/** For Nonroad **/
	public boolean isAffectedByNonroad;
	public boolean isChosenForNonroad = false;

	/**
	 * Creates a PollutantProcessAssociation object from a pollutant name and a process name.
	 * @param pollutantName The name of the pollutant.
	 * @param processName The name of the emisssion process.
	 * @return The PollutantProcessAssociation object. If either pollutant or process wasn't found
	 * this will return null.
	**/
	public static PollutantProcessAssociation createByName(
			String pollutantName, String processName) {
		PollutantProcessAssociation result = new PollutantProcessAssociation();

		result.pollutant = Pollutant.findByName(pollutantName);
		result.emissionProcess = EmissionProcess.findByName(processName);

		if((result.pollutant != null) && (result.emissionProcess != null)) {
			PollutantProcessLoader.setPollutantProcessAssoc(result);
			return result;
		} else {
			return null;
		}
	}

	/**
	 * Creates a PollutantProcessAssociation object from a pollutant ID and a process ID.
	 * @param pollutantID The ID (databaseKey) of the pollutant.
	 * @param processID The ID (databaseKey) of the emisssion process.
	 * @return The PollutantProcessAssociation object. If either pollutant or process wasn't found
	 * this will return null.
	**/
	public static PollutantProcessAssociation createByID(int pollutantID, int processID) {
		PollutantProcessAssociation result = new PollutantProcessAssociation();

		result.pollutant = Pollutant.findByID(pollutantID);
		result.emissionProcess = EmissionProcess.findByID(processID);

		if((result.pollutant != null) && (result.emissionProcess != null)) {
			PollutantProcessLoader.setPollutantProcessAssoc(result); // check if it is for nonroad/onroad
			return result;
		} else {
			return null;
		}
	}

	/**
	 * Create a PollutantProcessAssociation object given a polProcessID.
	 * @param polProcessID Unique identifier for the pollutant/process association
	 * @return The PollutantProcessAssociation object. If either pollutant or process wasn't found
	 * this will return null.
	**/
	public static PollutantProcessAssociation createByID(int polProcessID) {
		int pollutantID = polProcessID / 100;
		int processID = polProcessID % 100;
		return createByID(pollutantID, processID);
	}

	/**
	 * Find a PollutantProcessAssociation object from a pollutant name and a process name.
	 * The combination must be valid or null is returned.
	 * @param pollutantName The name of the pollutant.
	 * @param processName The name of the emisssion process.
	 * @return The PollutantProcessAssociation object. If either pollutant or process wasn't found
	 * or if the combination requested is not legal, this will return null.
	**/
	public static PollutantProcessAssociation findByName(
			String pollutantName, String processName) {
		PollutantProcessAssociation result = new PollutantProcessAssociation();

		result.pollutant = Pollutant.findByName(pollutantName);
		result.emissionProcess = EmissionProcess.findByName(processName);

		if((result.pollutant != null) && (result.emissionProcess != null)) {
			if(result.pollutant.associatedProcesses.contains(result.emissionProcess)) {
				PollutantProcessLoader.setPollutantProcessAssoc(result);
				return result;
			}
		}
		return null;
	}

	/**
	 * Comparable Interface Implementation. Provides Tree container ordering and display
	 * order.
	 * @param other PollutantProcessAssociation object to compare.
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		PollutantProcessAssociation ppa = (PollutantProcessAssociation)other;

		int result = pollutant.compareTo(ppa.pollutant);
		if(result == 0) {
			result = emissionProcess.compareTo(ppa.emissionProcess);
		}
		return result;
	}

	/**
	 * Gets a textual representation of the object
	 * @return the process name followed by the pollutant name
	**/
	public String toString() {
		return emissionProcess.toString() + " " + pollutant.toString();
	}

	/**
	 * Get the polProcessID for the association
	 * @return the polProcessID of the entry
	**/
	public int getDatabaseKey() {
		int pollutantID = pollutant.databaseKey;
		int processID = emissionProcess.databaseKey;
		return pollutantID*100 + processID;
	}

	/**
	 * Lookup the corresponding entry in the PollutantProcessAssoc table.
	 * @param database a Connection to the execution database
	 * @return the polProcessID of the matching entry or -1 if not found
	**/
	public int getDatabaseKey(Connection database) {
		int id = -1;
		String sql = "";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			sql = "SELECT polProcessID FROM PollutantProcessAssoc "
					+ "WHERE processID = ? AND pollutantID = ?";
			statement = database.prepareStatement(sql);
			statement.setInt(1,emissionProcess.databaseKey);
			statement.setInt(2,pollutant.databaseKey);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null && results.next()) {
				id = results.getInt(1);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e, "Unable to get database key for Pollutant Process Association " +
					"where Process ID = " + emissionProcess.databaseKey + " and Pollutant ID = " +
					pollutant.databaseKey + ".", sql );
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
		return id;
	}

	/**
	 * Check the OpModePolProcessAssoc table for the number of possible operating modes associated
	 * with this pollutant and process.  If 1 (or less), it is expected that EmissionCalculator-
	 * based classes will change their SQL to not reference the OpModeDistribution table.
	 * @param database a Connection to the execution database
	 * @param databaseKey the polProcessID returned by a prior call to getDatabaseKey(...)
	 * @return true if there is more than 1 operating mode available for this pollutant and process
	**/
	public boolean doesHaveMultipleOperatingModes(Connection database, int databaseKey) {
		boolean doesHaveMultiple = false;
		String sql = "";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			sql = "SELECT COUNT(*) FROM OpModePolProcAssoc WHERE polProcessID=?";
			statement = database.prepareStatement(sql);
			statement.setInt(1,databaseKey);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null && results.next()) {
				int count = results.getInt(1);
				if(count > 1) {
					doesHaveMultiple = true;
				}
			}
		} catch(SQLException e) {
			Logger.logSqlError(e, "Unable to count operating modes for polProcessID = "
					+ databaseKey + ".", sql );
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
		return doesHaveMultiple;
	}

	/**
	 * Get the single opModeID associated with this pollutant and process.  The results of this
	 * routine are only valid for pollutants and processes that cause the
	 * doesHaveMultipleOperatingModes(...) routine to return false.
	 * @param database a Connection to the execution database
	 * @param databaseKey the polProcessID returned by a prior call to getDatabaseKey(...)
	 * @return the opModeID for the polProcessID or 0 if not found.
	**/
	public int getSingleOpModeID(Connection database, int databaseKey) {
		int opModeID = 0;
		String sql = "";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			sql = "SELECT opModeID FROM OpModePolProcAssoc WHERE polProcessID=? LIMIT 1";
			statement = database.prepareStatement(sql);
			statement.setInt(1,databaseKey);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null && results.next()) {
				opModeID = results.getInt(1);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e, "Unable to get single opModeID for polProcessID = "
					+ databaseKey + ".", sql );
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
		return opModeID;
	}
}
