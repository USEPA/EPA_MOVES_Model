/**************************************************************************************************
 * @(#)Pollutant.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.Iterator;
import java.util.TreeSet;

/**
 * Represents a single Pollutant type. A Pollutant can be identified by its
 * ID or by its name. very Pollutant object created wii be added to the
 * allPollutant TreeSet collection. The EmissionProcess objects associated with the Pollutant
 * can be stored in a TreeSet collection.
 *
 * @author		Wesley Faler
 * @version		2015-02-10
**/
public class Pollutant implements Comparable {
	/** List of all Pollutant objects. **/
	public static TreeSet<Pollutant> allPollutants = new TreeSet<Pollutant>();

	/** List of all displayable Pollutant objects. **/
	public static TreeSet<Pollutant> allDisplayablePollutants = new TreeSet<Pollutant>();
	/** A display group ID that should always be displayed. This value was used to populate allDisplayablePollutants. **/
	public static int cacheSpecialCaseToShow = -100;

	/**
	 * Primary Key value used to indicate the exact database record that this object relates to.
	**/
	public int databaseKey;

	/** Name of the Pollutant **/
	public String pollutantName;

	/** Set of EmissionProcess objects related to this pollutant **/
	public TreeSet<EmissionProcess> associatedProcesses = new TreeSet<EmissionProcess>();

	/** Set of Pollutants that this one requires. **/
	public TreeSet<Pollutant> requiredPollutants = new TreeSet<Pollutant>();

	/** Display grouping the pollutant currently belongs to.  Frequenty NULL and filled by GUI algorithms. **/
	public PollutantDisplayGroup displayGroup = null;

	/** True when OnRoad can calculate this pollutant **/
	public boolean isAffectedByOnroad;

	/** True when NonRoad can calculate this pollutant **/
	public boolean isAffectedByNonroad;

	/**
	 * Gets a Pollutant by name or textual ID.
	 * @param pollutantName The name to search for
	 * @return Returns the associated Pollutant, or null if not found
	**/
	public static Pollutant findByName(String pollutantName) {
		if(pollutantName != null) {
			for (Iterator<Pollutant> i = allPollutants.iterator(); i.hasNext();) {
				Pollutant iterPollutant = (Pollutant)i.next();
				if(iterPollutant.pollutantName.compareToIgnoreCase(pollutantName) == 0
						|| pollutantName.equals(""+iterPollutant.databaseKey)) {
					return iterPollutant;
				}
			}
		}
		return null;
	}

	/**
	 * Gets a Pollutant by ID
	 * @param pollutantID The ID to search for
	 * @return Returns the associated Pollutant, or null if not found
	**/
	public static Pollutant findByID(int pollutantID) {
		for (Iterator<Pollutant> i = allPollutants.iterator(); i.hasNext();) {
			Pollutant iterPollutant = (Pollutant)i.next();
			if(iterPollutant.databaseKey == pollutantID) {
				return iterPollutant;
			}
		}
		return null;
	}

	/**
	 * Gets an array of all loaded Pollutant objects.
	 * @return an array of Pollutant objects
	**/
	public static Object[] getAllPollutants() {
		return allPollutants.toArray();
	}

	/**
	 * Gets an array of all displayable Pollutant objects.
	 * Non-displayable pollutants belong to a display group
	 * that has an ID less than 0.
	 * @param specialCaseToShow a display group ID that should always be displayed.
	 * @return an array of Pollutant objects
	**/
	public static Object[] getAllDisplayablePollutants(int specialCaseToShow) {
		if(allDisplayablePollutants.size() == 0 || specialCaseToShow != cacheSpecialCaseToShow) {
			allDisplayablePollutants.clear();
			cacheSpecialCaseToShow = specialCaseToShow;
			for(Pollutant p : allPollutants) {
				Integer pollutantID = new Integer(p.databaseKey);
				PollutantDisplayGroup group = PollutantDisplayGroup.pollutantDisplayGroups.get(pollutantID);
				if(group == null || group.displayGroupID >= 0 || group.displayGroupID == specialCaseToShow) {
					allDisplayablePollutants.add(p);
				}
			}
		}
		return allDisplayablePollutants.toArray();
	}

	/**
	 * Clears the static list of Pollutant objects.
	**/
	public static void clearAll() {
		allPollutants.clear();
		allDisplayablePollutants.clear();
	}

	/**
	 * Default constructor
	 * @param key Sets databaseKey.
	 * @param name Sets pollutantName.
	 * @param isAffectedByOnroad True when OnRoad can calculate this pollutant
	 * @param isAffectedByNonroad True when NonRoad can calculate this pollutant
	**/
	public Pollutant(int key, String name, 
			boolean isAffectedByOnroad, boolean isAffectedByNonroad) {
		pollutantName = name;
		databaseKey = key;
		this.isAffectedByOnroad = isAffectedByOnroad;
		this.isAffectedByNonroad = isAffectedByNonroad;
		if(!allPollutants.contains(this)) {
			allPollutants.add(this);
			allDisplayablePollutants.clear(); // since group is assigned later, this must be cleared.
		}
	}

	/**
	 * Comparable Interface Implementation. Provides Tree container ordering and display
	 * order.
	 * @param other Pollutant object to compare.
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		//return pollutantName.compareTo(((Pollutant)other).pollutantName);
		Pollutant p = (Pollutant)other;
		int c = pollutantName.compareTo(p.pollutantName);
		if(c == 0) {
			if(databaseKey == 0 || p.databaseKey == 0) {
				return 0;
			}
			return databaseKey - p.databaseKey;
		}
		return c;
	}

	/**
	 * Gets a textual representation of the object
	 * @return the pollutant name
	**/
	public String toString() {
		return pollutantName;
	}
}
