/**************************************************************************************************
 * @(#)PollutantProcessLoader.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.Iterator;
import java.util.TreeSet;
import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.gui.MOVESNavigation;
import gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator;

/**
 * Utility that loads all process and pollutant information from the database into Java
 * objects.  Establishes appropriate connections between them.
 *
 * @author		Wesley Faler
 * @author		Ed Glover (minor mods - task 219)
 * @author		Ed Glover & William Aikman
 * @version		2015-02-05
**/
public class PollutantProcessLoader {
	/** Flags that the pollutant processes have been loaded from the database. **/
	static boolean isLoaded = false;

	/** 
	 * Returns whether the pollutant processes have been loaded. 
	 * @return True if the Pollutant Processes have been loaded.
	**/
	public static boolean isLoaded() {
		return isLoaded;
	}

	/** Performs the load from database to Java objects. **/
	public static void loadFromDatabase() {
		//Pollutant.clearAll();
		//EmissionProcess.clearAll();
		isLoaded = false;

		TreeSet<PollutantProcessAssociation> associations = new TreeSet<PollutantProcessAssociation>();
		Connection db = null;
		boolean hasNonroad = false;

		Models.ModelCombination mc;
		if (MOVESNavigation.singleton != null
				&& MOVESNavigation.singleton.parent != null
				&& MOVESNavigation.singleton.parent.runSpec != null) {
			mc = MOVESNavigation.singleton.parent.runSpec.getModelCombination();
		} else {
			mc = Models.ModelCombination.M0;
		}
		hasNonroad = mc == Models.ModelCombination.M2;
		TreeSet<Integer> nonroadPolProcessIDs = new TreeSet<Integer>();
		if(hasNonroad) {
			for(int i=0;i<NonroadEmissionCalculator.nonroadPolProcessIDs.length;i++) {
				nonroadPolProcessIDs.add(new Integer(NonroadEmissionCalculator.nonroadPolProcessIDs[i]));
			}
		}

		String sql = "SELECT processID,processName,occursOnRealRoads,isAffectedByOnroad,isAffectedByNonroad FROM emissionprocess";
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);

			PreparedStatement statement = db.prepareStatement(sql);
			ResultSet results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					// NOTE: Simply new'ing EmissionProcess objects adds them to a global set
					new EmissionProcess(results.getInt(1),results.getString(2),
							results.getString(3),results.getBoolean(4), results.getBoolean(5));
				}
				results.close();
			}
			statement.close();

			sql = "SELECT p.pollutantID, p.pollutantName, p.pollutantDisplayGroupID, p.isAffectedByOnroad, p.isAffectedByNonroad, "
					+ " pdg.displayAsGroup, pdg.pollutantDisplayGroupName"
					+ " FROM Pollutant p"
					+ " INNER JOIN PollutantDisplayGroup pdg USING (pollutantDisplayGroupID)"
					+ " ORDER BY p.pollutantDisplayGroupID, p.pollutantID";
			
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					// NOTE: Simply new'ing Pollutant objects adds them to a global set
					new Pollutant(results.getInt(1), results.getString(2),
							results.getBoolean(4), results.getBoolean(5));
					PollutantDisplayGroup.add(results.getInt(1), results.getInt(3),
							results.getString(6).equalsIgnoreCase("Y"), results.getString(7));
				}
				results.close();
			}
			statement.close();

			// "Load" pollutants that require other pollutants to be run.
			Pollutant pollutant = Pollutant.findByID(90); // Atmospheric CO2
			Pollutant requiredPollutant = Pollutant.findByID(91); // Total Energy Consumption
			if (pollutant != null && requiredPollutant != null) {
				pollutant.requiredPollutants.add(requiredPollutant);
			}
			pollutant = Pollutant.findByID(93); // Fossil Fuel Energy Consumption
			requiredPollutant = Pollutant.findByID(91); // Total Energy Consumption
			if (pollutant != null && requiredPollutant != null) {
				pollutant.requiredPollutants.add(requiredPollutant);
			}
			pollutant = Pollutant.findByID(92);	// Petroleum Energy Consumption
			requiredPollutant = Pollutant.findByID(91); // Total Energy Consumption
			if (pollutant != null && requiredPollutant != null) {
				pollutant.requiredPollutants.add(requiredPollutant);
			}
			pollutant = Pollutant.findByID(98);	// CO2 Equivalent
			requiredPollutant = Pollutant.findByID(90); // Atmospheric CO2
			if (pollutant != null && requiredPollutant != null) {
				pollutant.requiredPollutants.add(requiredPollutant);
			}
		
			// "Load" the pollutants required if this one is run.
			sql = "SELECT processID,pollutantID,polProcessID,chainedto1,chainedto2,nrChainedTo1,nrChainedTo2"
					+ " FROM pollutantprocessassoc";
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				PollutantProcessAssociation.clearChainedTo();
				while(results.next()) {
					int processID = results.getInt(1);
					EmissionProcess process = EmissionProcess.findByID(processID);
					int pollutantID = results.getInt(2);
					pollutant = Pollutant.findByID(pollutantID);
					if(process == null) {
						Logger.log(LogMessageCategory.ERROR,"Unable to find process " + processID + " while loading pollutant/process associations");
						continue;
					}
					if(pollutant == null) {
						Logger.log(LogMessageCategory.ERROR,"Unable to find pollutant " + pollutantID + " while loading pollutant/process associations");
						continue;
					}
					associations.add(PollutantProcessAssociation.createByID(pollutantID,processID));
					int polProcessID = results.getInt(3);
					if(!hasNonroad || !nonroadPolProcessIDs.contains(new Integer(polProcessID))) {
						int chainedTo1 = results.getInt(4);
						if(chainedTo1 > 0) {
							PollutantProcessAssociation.addChainedTo(polProcessID, chainedTo1);
						}
						int chainedTo2 = results.getInt(5);
						if(chainedTo2 > 0) {
							PollutantProcessAssociation.addChainedTo(polProcessID, chainedTo2);
						}
					}

					int nrChainedTo1 = results.getInt(6);
					if(nrChainedTo1 > 0) {
						PollutantProcessAssociation.addChainedToNR(polProcessID, nrChainedTo1);
					}
					int nrChainedTo2 = results.getInt(7);
					if(nrChainedTo2 > 0) {
						PollutantProcessAssociation.addChainedToNR(polProcessID, nrChainedTo2);
					}
				}
				results.close();
			}
			statement.close();

			isLoaded = true;
		} catch(Exception e) {
			/**
			 * @explain The set of pollutants and processes could not be loaded from the
			 * database.  Check the default input database for completeness.
			**/
			Logger.logError(e, "Loading Pollutants and Processes from the database failed.");
		}

		for (Iterator<PollutantProcessAssociation> i = associations.iterator(); i.hasNext();) {
			PollutantProcessAssociation iterAssociation = (PollutantProcessAssociation) i.next();
			iterAssociation.pollutant.associatedProcesses.add(iterAssociation.emissionProcess);
			iterAssociation.emissionProcess.associatedPollutants.add(iterAssociation.pollutant);
		}

		if(db != null) {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
		}
	}

	/**
	 * Fill flags for which models affect a pollutant/process association.
	 * @param assoc association to be completed
	**/
	public static void setPollutantProcessAssoc(PollutantProcessAssociation assoc) {
		Connection db = null;

		String sql = "SELECT isAffectedByOnroad, isAffectedByNonroad"
				+ " FROM pollutantprocessassoc where pollutantID="
				+ assoc.pollutant.databaseKey + " and processID="
				+ assoc.emissionProcess.databaseKey;
		SQLRunner.Query query = new SQLRunner.Query();

		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
			query.open(db,sql);
			if(query.rs.next()) {
				assoc.isAffectedByOnroad = query.rs.getBoolean(1);
				assoc.isAffectedByNonroad = query.rs.getBoolean(2);
			} else {
				// Cannot throw an exception here as no calling routine can itself throw
				// an exception without major changes. Adding a try/catch to the calling
				// routines would mean this exception gets discarded anyway, so there is
				// no point to throwing it ever.
				//throw new Exception("No results or result set's size is not 1.");
			}
		} catch (Exception e) {
			/**
			 * @explain The set of pollutants and processes could not be loaded
			 *          from the database. Check the default input database for
			 *          completeness.
			 **/
			Logger.logError(e,"Set pollutant and process association from the database failed.");
		} finally {
			query.onFinally();
			if (db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT, db);
			}
		}
	}

	public static Boolean getBooleanColVal(String col, int pollutantID,
			int processID) {
		Connection db = null;

		String sql = "SELECT " + col
				+ " FROM pollutantprocessassoc where pollutantID="
				+ pollutantID + " and processID=" + processID;

		Boolean boolVal = null;
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);

			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if (results == null) {
				throw new Exception("No results.");
			}
			if (results.next()) {
				boolVal = results.getBoolean(1);
			}
		} catch (Exception e) {
			Logger.logError(e, "getBooleanColVal from the database failed.");
		} finally {
			try {
				if (results != null)
					results.close();
				if (statement != null)
					statement.close();
			} catch (Exception e1) {
				Logger.logError(e1, "Could not close results or statement.");
			}
			if (db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT, db);
			}
		}
		return boolVal;
	}
}
