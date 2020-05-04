/**************************************************************************************************
 * @(#)TOGSpeciationCalculator.java
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

import java.util.*;
import java.sql.*;

/**
 * Calculate lumped species from TOG emissions.
 *
 * @author      Wesley Faler
 * @version		2015-11-18
**/
public class TOGSpeciationCalculator extends EmissionCalculator {
	/** Cached results from the getMechanismPollutants() function **/
	private static ArrayList<Pollutant> cachedMechanismPollutants = null;
	/** Cached results from the getMechanismProcesses() function **/
	private static TreeMap<String,ArrayList<EmissionProcess>> cachedMechanismProcesses = new TreeMap<String,ArrayList<EmissionProcess>>();
	/** Cached results from the getMechanismRequirements() function **/
	private static TreeMap<String,ArrayList<PollutantProcessAssociation>> cachedRequirements = new TreeMap<String,ArrayList<PollutantProcessAssociation>>();

	/**
	 * Retrieve the set of mechanism pollutants.
	 * @param db database to be used.
	 * @return a list of pollutants, may be empty, never null.
	 * @throws SQLException if anything goes wrong.
	**/
	public static ArrayList<Pollutant> getMechanismPollutants(Connection db)
			throws SQLException {
		if(cachedMechanismPollutants != null) {
			return cachedMechanismPollutants;
		}
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "select pollutantID"
				+ " from pollutant"
				+ " inner join pollutantDisplayGroup using (pollutantDisplayGroupID)"
				+ " where pollutantDisplayGroupName='Mechanisms'";
		try {
			ArrayList<Pollutant> pollutants = new ArrayList<Pollutant>();
			query.open(db,sql);
			while(query.rs.next()) {
				int pollutantID = query.rs.getInt(1);
				Pollutant pollutant = Pollutant.findByID(pollutantID);
				if(pollutant != null) {
					pollutants.add(pollutant);
				}
			}
			query.close();
			cachedMechanismPollutants = pollutants;
			return pollutants;
		} catch(SQLException e) {
			Logger.logError(e,"Unable to query mechanism pollutants: " + sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Retrieve the set of emission processes that include a mechanism.
	 * @param db database to be used.
	 * @param mechanismPollutant pollutant that is itself a mechanism pseudo-pollutant.
	 * @return a list of processes, may be empty, never null.
	 * @throws SQLException if anything goes wrong.
	**/
	public static ArrayList<EmissionProcess> getMechanismProcesses(Connection db, Pollutant mechanismPollutant)
			throws SQLException {
		String cacheKey = "" + mechanismPollutant.databaseKey;
		if(cachedMechanismProcesses.containsKey(cacheKey)) {
			return cachedMechanismProcesses.get(cacheKey);
		}
		int mechanismID = 1 + ((mechanismPollutant.databaseKey - 1000) / 500);
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "select distinct processID"
				+ " from integratedSpeciesSet iss"
				+ " inner join TOGSpeciationProfile sp on ("
				+ " 	sp.mechanismID = iss.mechanismID"
				+ " 	and sp.integratedSpeciesSetID = iss.integratedSpeciesSetID"
				+ " 	and iss.useISSyn='Y')"
				+ " inner join TOGSpeciation spec on (spec.togSpeciationProfileID = sp.togSpeciationProfileID)"
				+ " where iss.mechanismID=" + mechanismID;
		try {
			ArrayList<EmissionProcess> processes = new ArrayList<EmissionProcess>();
			query.open(db,sql);
			while(query.rs.next()) {
				int processID = query.rs.getInt(1);
				EmissionProcess process = EmissionProcess.findByID(processID);
				if(process != null) {
					processes.add(process);
				}
			}
			query.close();
			cachedMechanismProcesses.put(cacheKey,processes);
			return processes;
		} catch(SQLException e) {
			Logger.logError(e,"Unable to query processes for a mechanism using: " + sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Retrieve the set of pollutant/process associations required to compute
	 * a mechanism. This includes NMOG as well as items listed in active
	 * integrated species.
	 * @param db database to be used.
	 * @param mechanism a pollutant/process association for a mechanism.
	 * @return a list of pollutants/process associations, may be empty, never null.
	 * @throws SQLException if anything goes wrong.
	**/
	public static ArrayList<PollutantProcessAssociation> getMechanismRequirements(Connection db,
			PollutantProcessAssociation mechanism)
			throws SQLException {
		String cacheKey = "" + mechanism.pollutant.databaseKey + "|" + mechanism.emissionProcess.databaseKey;
		if(cachedRequirements.containsKey(cacheKey)) {
			return cachedRequirements.get(cacheKey);
		}
		int mechanismID = 1 + ((mechanism.pollutant.databaseKey - 1000) / 500);
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "select distinct pollutantID"
				+ " from integratedSpeciesSet"
				+ " where useISSyn='Y'"
				+ " and mechanismID=" + mechanismID
				+ " union"
				+ " select distinct pollutantID"
				+ " from togSpeciationProfile"
				+ " where pollutantID <> 88"
				+ " and mechanismID=" + mechanismID;
		try {
			ArrayList<PollutantProcessAssociation> results = new ArrayList<PollutantProcessAssociation>();
			// NMOG (80) is always required. It is already TOG - Methane so we avoid
			// chaining to TOG and Methane.
			Pollutant pollutant = Pollutant.findByID(80);
			if(pollutant != null) {
				PollutantProcessAssociation ppa = PollutantProcessAssociation.createByID(pollutant.databaseKey,mechanism.emissionProcess.databaseKey);
				if(ppa != null && ppa.isAffectedByOnroad) { // mechanisms only apply to Onroad
					results.add(ppa);
				}
			}
			// Get the list from the database.
			query.open(db,sql);
			while(query.rs.next()) {
				int pollutantID = query.rs.getInt(1);
				pollutant = Pollutant.findByID(pollutantID);
				if(pollutant != null) {
					PollutantProcessAssociation ppa = PollutantProcessAssociation.createByID(pollutant.databaseKey,mechanism.emissionProcess.databaseKey);
					if(ppa != null && ppa.isAffectedByOnroad) { // mechanisms only apply to Onroad
						results.add(ppa);
					}
				}
			}
			query.close();
			cachedRequirements.put(cacheKey,results);
			return results;
		} catch(SQLException e) {
			Logger.logError(e,"Unable to query mechanism prerequisites: " + sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Retrieve the set of pollutant/process associations that result from
	 * a mechanism. This includes on the lumped species, not NonHAPTOG.
	 * @param db database to be used.
	 * @param mechanism a pollutant/process association for a mechanism.
	 * @return a list of pollutants/process associations, may be empty, never null.
	 * @throws SQLException if anything goes wrong.
	**/
	public static ArrayList<PollutantProcessAssociation> getMechanismLumpedSpecies(Connection db,
			PollutantProcessAssociation mechanism)
			throws SQLException {
		int mechanismID = 1 + ((mechanism.pollutant.databaseKey - 1000) / 500);
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "select distinct 1000+(iss.mechanismID-1)*500+lumpedSpeciesID as pollutantID"
				+ " from integratedSpeciesSet iss"
				+ " inner join TOGSpeciationProfile sp on ("
				+ " 	sp.mechanismID = iss.mechanismID"
				+ " 	and sp.integratedSpeciesSetID = iss.integratedSpeciesSetID"
				+ " 	and iss.useISSyn='Y')"
				+ " inner join TOGSpeciation spec on (spec.togSpeciationProfileID = sp.togSpeciationProfileID)"
				+ " inner join lumpedSpeciesName using (lumpedSpeciesName)"
				+ " where iss.mechanismID=" + mechanismID
				+ " and spec.processID=" + mechanism.emissionProcess.databaseKey;
		try {
			ArrayList<PollutantProcessAssociation> results = new ArrayList<PollutantProcessAssociation>();
			query.open(db,sql);
			while(query.rs.next()) {
				int pollutantID = query.rs.getInt(1);
				Pollutant pollutant = Pollutant.findByID(pollutantID);
				if(pollutant != null) {
					PollutantProcessAssociation ppa = PollutantProcessAssociation.createByID(pollutant.databaseKey,mechanism.emissionProcess.databaseKey);
					if(ppa != null) {
						results.add(ppa);
					}
				}
			}
			query.close();
			return results;
		} catch(SQLException e) {
			Logger.logError(e,"Unable to query mechanism results: " + sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}

	/** True if any instance has been called twice in a single context **/
	static boolean didCalculateTwiceInAContext = false;

	/**
	 * Check the need to do final aggregation.
	 * @return true if final aggregation is called for.
	**/	
	public static boolean needsFinalAggregation() {
		return didCalculateTwiceInAContext;
	}

	/**
	 * Set of contexts that have been calculated. Running twice in a single
	 * context will require final aggregation to be performed.
	**/
	TreeSet<String> contextsSeen = new TreeSet<String>();

	/**
	 * Constructor, including registration of potential processes and pollutants
	 * handled by this calculator.  Such registration facilitates calculator
	 * chaining.
	**/
	public TOGSpeciationCalculator() {
		didCalculateTwiceInAContext = false;
		Pollutant nonhapTOG = Pollutant.findByID(88);
		if(nonhapTOG == null) {
			return;
		}
		Connection db = null;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);

			TreeSet<Integer> nonhapProcesses = new TreeSet<Integer>();
			TreeSet<String> lumpedProcesses = new TreeSet<String>();

			ArrayList<Pollutant> mechanismPollutants = getMechanismPollutants(db);
			for(Pollutant mp : mechanismPollutants) {
				ArrayList<EmissionProcess> mechanismProcesses = getMechanismProcesses(db,mp);
				for(EmissionProcess ep : mechanismProcesses) {
					PollutantProcessAssociation mechanismPPA = PollutantProcessAssociation.createByID(mp.databaseKey,ep.databaseKey);
					// Register mechanisms for each required process.
					EmissionCalculatorRegistration.register(mp,ep,this);
					// Register NonHAPTOG (88) for each mechanism pollutant/process.
					Integer processID = new Integer(ep.databaseKey);
					if(!nonhapProcesses.contains(processID)) {
						nonhapProcesses.add(processID);
						EmissionCalculatorRegistration.register(nonhapTOG,ep,this);
					}
					// Register lumped species pollutants for each mechanism pollutant/process.
					ArrayList<PollutantProcessAssociation> lumpedSpecies = getMechanismLumpedSpecies(db,mechanismPPA);
					for(PollutantProcessAssociation lump : lumpedSpecies) {
						String lumpedKey = "" + processID + "|" + lump.pollutant.databaseKey;
						if(!lumpedProcesses.contains(lumpedKey)) {
							lumpedProcesses.add(lumpedKey);
							EmissionCalculatorRegistration.register(lump.pollutant,ep,this);
						}
					}
				}
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to register pollutants in TOGSpeciationCalculator.");
		} finally {
			if(db != null) {
				try {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
				} catch(Exception e) {
					// Nothing to do here
				}
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
		TreeSet<EmissionCalculator> calculators = new TreeSet<EmissionCalculator>();

		Connection db = null;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

			ArrayList<Pollutant> mechanismPollutants = getMechanismPollutants(db);
			for(Pollutant mp : mechanismPollutants) {
				ArrayList<EmissionProcess> mechanismProcesses = getMechanismProcesses(db,mp);
				for(EmissionProcess ep : mechanismProcesses) {
					PollutantProcessAssociation mechanismPPA = PollutantProcessAssociation.createByID(mp.databaseKey,ep.databaseKey);
					if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(mp,ep)) {
						Integer processID = new Integer(ep.databaseKey);
						ArrayList<PollutantProcessAssociation> inputs = getMechanismRequirements(db,mechanismPPA);
						for(PollutantProcessAssociation input : inputs) {
							LinkedList<EmissionCalculator> t = EmissionCalculatorRegistration.findPollutant(input.pollutant);
							calculators.addAll(t);
						}
					}
				}
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to find requirements in TOGSpeciationCalculator.");
		} finally {
			if(db != null) {
				try {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				} catch(Exception e) {
					// Nothing to do here
				}
				db = null;
			}
		}

		for(Iterator<EmissionCalculator> i=calculators.iterator();i.hasNext();) {
			EmissionCalculator c = (EmissionCalculator)i.next();
			if(c != this) {
				c.chainCalculator(this);
				Logger.log(LogMessageCategory.INFO,"TOGSpeciationCalculator chained to " + c);
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
		String mechanismIDs = "";

		Connection db = null;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			ArrayList<Pollutant> mechanisms = getMechanismPollutants(db);
			for(Pollutant p : mechanisms) {
				boolean found = false;
				if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(p,context.iterProcess)) {
					if(mechanismIDs.length() > 0) {
						mechanismIDs += ",";
					}
					mechanismIDs += "" + (1+((p.databaseKey-1000)/500));
					found = true;
				}
				if(!found && context.chainedProcesses != null) {
					for(EmissionProcess proc : context.chainedProcesses) {
						if(ExecutionRunSpec.theExecutionRunSpec.doesHavePollutantAndProcess(p,proc)) {
							if(mechanismIDs.length() > 0) {
								mechanismIDs += ",";
							}
							mechanismIDs += "" + (1+((p.databaseKey-1000)/500));
							break;
						}
					}
				}
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to check pollutants in TOGSpeciationCalculator");
			return null;
		} finally {
			if(db != null) {
				try {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				} catch(Exception e) {
					// Nothing to do here
				}
				db = null;
			}
		}
		//Logger.log(LogMessageCategory.DEBUG,"TOGSpeciationCalculator proc=" + context.iterProcess.databaseKey + ",ids=" + mechanismIDs);
		if(mechanismIDs.length() <= 0) {
			return null;
		}

		if(!context.isCleanUp && !context.isCreatingBundles) {
			String contextString = context.toString();
			if(contextsSeen.contains(contextString)) {
				didCalculateTwiceInAContext = true;
			} else {
				contextsSeen.add(contextString);
			}
		}

		SQLForWorker sqlForWorker = new SQLForWorker();

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		TreeSetIgnoreCase enabledSectionNames = new TreeSetIgnoreCase();

		replacements.put("##mechanismIDs##",mechanismIDs);

		boolean isOK = readAndHandleScriptedCalculations(context,replacements,
				"database/TOGSpeciationCalculator.sql",enabledSectionNames,
				sqlForWorker);

		if(isOK) {
			Logger.log(LogMessageCategory.INFO,"TOGSpeciationCalculator doExecute method IS successful");
			return sqlForWorker;
		} else {
			Logger.log(LogMessageCategory.INFO,"TOGSpeciationCalculator doExecute method NOT successful");
			return null;
		}
	}
}
