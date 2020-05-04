/**************************************************************************************************
 * @(#)PollutantDisplayGroup.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.Iterator;
import java.util.TreeSet;
import java.util.TreeMap;

/**
 * Represents the displayed pollutants and pollutant groups.
 * @author		Wesley Faler
 * @version		2010-11-26
**/
public class PollutantDisplayGroup implements Comparable {
	/** List of all pollutant display groups. **/
	public static TreeMap<PollutantDisplayGroup,PollutantDisplayGroup> allPollutantDisplayGroups
			= new TreeMap<PollutantDisplayGroup,PollutantDisplayGroup>();

	/** The pollutant display group id **/
	public int displayGroupID;

	/** Flag indicating if this is a pollutant or group of pollutants **/
	public boolean displayAsGroup;

	/** Used if individual members within the group are displayed **/
	public int displayMemberID;

	/** The displayed name for the pollutant or pollutant group **/
	public String displayGroupName;

	/** Set of Pollutant IDs in this display group **/
	public TreeSet<Integer> pollutantIDsInDisplayGroup = new TreeSet<Integer>();

	/** List of display group ids for each pollutant **/
	public static TreeMap<Integer,PollutantDisplayGroup> pollutantDisplayGroups
			= new TreeMap<Integer,PollutantDisplayGroup>();

	/** true if any pollutant in the group requires distance to be calculated **/
	public boolean requiresDistance = false;

	/**
	 * Default constructor
	 * @param pollutantID The ID of a pollutant that is a member of this display group.
	 * @param displayGroupIDToUse The pollutant display group id.
	 * @param displayAsGroupToUse Display this pollutant as a group.
	 * @param displayGroupNameToUse The displayed name of the group.
	**/
	public PollutantDisplayGroup(int pollutantID, int displayGroupIDToUse,
			boolean displayAsGroupToUse, String displayGroupNameToUse) {
		if(displayGroupNameToUse != null
				&& (displayGroupNameToUse.equalsIgnoreCase("Metals") || displayGroupNameToUse.equalsIgnoreCase("Dioxins and Furans"))) {
			requiresDistance = true;
		}

		displayGroupID = displayGroupIDToUse;
		displayAsGroup = displayAsGroupToUse;
		if(displayAsGroup) {
			displayGroupName = displayGroupNameToUse;
			displayMemberID = -1;
		} else {
			// Create a "virtual" group with one pollutant.
			Pollutant pollutant = Pollutant.findByID(pollutantID);
			displayGroupName = pollutant.pollutantName;
			displayMemberID = pollutant.databaseKey;
		}
		Integer pollutantIDObject = new Integer(pollutantID);
		PollutantDisplayGroup pollutantDisplayGroup = null;
		if(allPollutantDisplayGroups.containsKey(this)) {
			pollutantDisplayGroup = (PollutantDisplayGroup)allPollutantDisplayGroups.get(this);
			pollutantDisplayGroup.pollutantIDsInDisplayGroup.add(pollutantIDObject);
		} else {
			pollutantDisplayGroup = this;
			pollutantIDsInDisplayGroup.add(pollutantIDObject);
			allPollutantDisplayGroups.put(this,this);
		}
		pollutantDisplayGroups.put(pollutantIDObject, pollutantDisplayGroup);
	}

	/**
	 * Add pollutant's display group to the list of pollutant display groups.
	 * @param pollutantID The id of the member that is being adding this group.
	 * @param displayGroupID The id of this display group.
	 * @param displayAsGroup True if the members of this group are displayed as a group.
	 * @param displayGroupName The name of this display group.
	**/
	public static void add(int pollutantID, int displayGroupID, boolean displayAsGroup,
			String displayGroupName) {
		new PollutantDisplayGroup(pollutantID,displayGroupID,displayAsGroup,displayGroupName);
	}

	/**
	 * Comparable Interface Implementation. Provides Tree container ordering and display
	 * order.
	 * @param other PollutantDisplayGroup object to compare.
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		int myDisplayGroupID = Math.abs(displayGroupID);
		int otherDisplayGroupID = Math.abs(((PollutantDisplayGroup)other).displayGroupID);
		if(myDisplayGroupID < otherDisplayGroupID) {
			return -1;
		} else if(myDisplayGroupID > otherDisplayGroupID) {
			return 1;
		}

		if(displayAsGroup) {
			return 0;
		}

		if(displayMemberID < ((PollutantDisplayGroup)other).displayMemberID) {
			return -1;
		} else if(displayMemberID >
				((PollutantDisplayGroup)other).displayMemberID) {
			return 1;
		}
		return 0;
	}

	/**
	 * Gets a textual representation of the object
	 * @return the pollutant name
	**/
	public String toString() {
		return displayGroupName;
	}

	/**
	 * Find PollutantDisplayGroup by ID
	 * @param pollutantDisplayGroupID The ID to search for
	 * @return Returns the associated PollutantDisplayGroup, or null if not found
	**/
	public static PollutantDisplayGroup findById(int pollutantDisplayGroupID) {
		for (Iterator<PollutantDisplayGroup> i = allPollutantDisplayGroups.values().iterator();
				i.hasNext();) {
			PollutantDisplayGroup iterPollutantDisplayGroup = (PollutantDisplayGroup)i.next();
			if(iterPollutantDisplayGroup.displayGroupID == pollutantDisplayGroupID) {
				return iterPollutantDisplayGroup;
			}
		}
		return null;
	}

	/**
	 * Find a display group for a given pollutant
	 * @param pollutantID the pollutant to search fo
	 * @return the PollutantDisplayGroup that uses the pollutant, or null if not found
	**/
	public static PollutantDisplayGroup findByPollutantID(int pollutantID) {
		Integer id = new Integer(pollutantID);
		for (Iterator<PollutantDisplayGroup> i = allPollutantDisplayGroups.values().iterator();
				i.hasNext();) {
			PollutantDisplayGroup iterPollutantDisplayGroup = (PollutantDisplayGroup)i.next();
			if(iterPollutantDisplayGroup.pollutantIDsInDisplayGroup.contains(id)) {
				return iterPollutantDisplayGroup;
			}
		}
		return null;
	}
}
