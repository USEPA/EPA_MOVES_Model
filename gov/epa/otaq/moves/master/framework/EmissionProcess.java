/**************************************************************************************************
 * @(#)EmissionProcess.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.Iterator;
import java.util.TreeSet;

/**
 * An Emission Process such as "Running Exhaust," "Start Exhaust," and "Extended Idle Exhaust". 
 * There is exactly one EmissionProcess object created for each Emission Process entry in the
 * database. All references to an EmissionProcess object refer to this one instance. The static
 * methods findByName(), findByID(), or getAllEmissionProcesses() are used to return a reference
 * to a specific instance.
 *
 * @author		Wesley Faler
 * @version		2012-11-08
**/
public class EmissionProcess implements Comparable {
	/** This is a set of all active EmissionProcess objects. **/
	static TreeSet<EmissionProcess> allProcesses = new TreeSet<EmissionProcess>();
	
	/**
	 * Primary Key value used to indicate the exact database record that this object relates to.
	**/
	public int databaseKey;
	
	//saves the order of the columns in the jtable
	private int displayKey;

	/** The displayable name for the process **/
	public String processName;

	/** true if the process requires real roads, not just road types **/
	public boolean occursOnRealRoads = false;
	
	/**
	 * Pollutant objects that are associated with this process as defined in the 
	 * PollutantProcessAssoc table.
	**/
	public TreeSet<Pollutant> associatedPollutants = new TreeSet<Pollutant>();

	/** True when OnRoad can calculate this pollutant **/
	public boolean isAffectedByOnroad;

	/** True when NonRoad can calculate this pollutant **/
	public boolean isAffectedByNonroad;
	
	/**
	 * Gets the EmissionProcess matching the specified name or textual ID.
	 * @param processName The name to search for.
	 * @return Returns the matching EmissionProcess, or null if not found.
	**/
	public static EmissionProcess findByName(String processName) {
		if(processName != null) {
			for(Iterator<EmissionProcess> iterProcess = allProcesses.iterator();
					iterProcess.hasNext();) {
				EmissionProcess process = (EmissionProcess)iterProcess.next();
				if(process.processName.compareToIgnoreCase(processName) == 0
						|| processName.equals(""+process.databaseKey)) {
					return process;
				}
			}
		}
		return null;
	}

	/**
	 * Gets the EmissionProcess matching the specified ID.
	 * @param processID The ID to search for.
	 * @return Returns the matching EmissionProcess, or null if not found.
	**/
	public static EmissionProcess findByID(int processID) {
		for(Iterator<EmissionProcess> iterProcess = allProcesses.iterator();
				iterProcess.hasNext();) {
			EmissionProcess process = (EmissionProcess)iterProcess.next();
			if(process.databaseKey == processID) {
				return process;
			}
		}
		return null;
	}

	/**
	 * Gets an array of all loaded EmissionProcess objects.
	 * @return an array of EmissionProcess objects
	**/
	public static Object[] getAllEmissionProcesses() {
		return allProcesses.toArray();
	}

	/**
	 * Gets an array of all loaded EmissionProcess objects that match
	 * the required occursOnRealRoads value.
	 * @param occursOnRealRoads criteria to match against
	 * @return an array of EmissionProcess objects
	**/
	public static Object[] getAllEmissionProcesses(boolean occursOnRealRoads) {
		TreeSet<EmissionProcess> matches = new TreeSet<EmissionProcess>();
		for(Iterator<EmissionProcess> i=allProcesses.iterator();i.hasNext();) {
			EmissionProcess p = (EmissionProcess)i.next();
			if(p.occursOnRealRoads == occursOnRealRoads) {
				matches.add(p);
			}
		}
		return matches.toArray();
	}

	/**
	 * Clears the static list of Pollutant objects.
	**/
	public static void clearAll() {
		allProcesses.clear();
	}

	/**
	 * Default constructor
	 * @param key Sets databaseKey.
	 * @param name Sets processName.
	 * @param useRealRoads Sets occursOnRealRoads.  Pass "Y" for true.
	 * @param isAffectedByOnroad True when OnRoad can calculate this pollutant
	 * @param isAffectedByNonroad True when NonRoad can calculate this pollutant
	**/
	public EmissionProcess(int key, String name, String useRealRoads,
			boolean isAffectedByOnroad, boolean isAffectedByNonroad) {
		processName = name;
		databaseKey = key;
		occursOnRealRoads = useRealRoads != null && useRealRoads.equals("Y");
		this.isAffectedByOnroad = isAffectedByOnroad;
		this.isAffectedByNonroad = isAffectedByNonroad;
		if(!allProcesses.contains(this)) {
			allProcesses.add(this);
		}
	}

	/**
	 * Default constructor
	 * @param key Sets databaseKey.
	 * @param name Sets processName.
	 * @param useRealRoads Sets occursOnRealRoads.  Pass "Y" for true.
	 * @param isAffectedByOnroad True when OnRoad can calculate this pollutant
	 * @param isAffectedByNonroad True when NonRoad can calculate this pollutant
	 * @param displayKey order of display on the Process table
	**/
	public EmissionProcess(int key, String name, String useRealRoads,
			boolean isAffectedByOnroad, boolean isAffectedByNonroad, int display) {
		processName = name;
		databaseKey = key;
		displayKey = display;
		occursOnRealRoads = useRealRoads != null && useRealRoads.equals("Y");
		this.isAffectedByOnroad = isAffectedByOnroad;
		this.isAffectedByNonroad = isAffectedByNonroad;
		if(!allProcesses.contains(this)) {
			allProcesses.add(this);
		}
	}

	/**
	 * Comparable Interface Implementation. Used to order EmissionProcess objects for display. This
	 * method should never return 0 (equal) unless the objects compared are the same object.
	 * @param otherObject Another EmissionProcess object to compare.
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object otherObject) {
/*		if(processName == null) {
			return -1;
		}
		EmissionProcess other = (EmissionProcess)otherObject;
		if(other == null) {
			return 1;
		}
		if(other.processName == null) {
			return 1;
		}
		return processName.compareToIgnoreCase(other.processName);
*/
		//code for task 1902 (changed to use displaykey - was the databasekey)
		EmissionProcess other = (EmissionProcess)otherObject;
		if(displayKey < other.displayKey) {
			return -1;
		} else if (displayKey > other.displayKey) {
			return 1;
		} else {
			return 0;
		}
	}

	/**
	 * Gets a textual representation of the object
	 * @return the process name
	**/
	public String toString() {
		return processName;
	}
}
