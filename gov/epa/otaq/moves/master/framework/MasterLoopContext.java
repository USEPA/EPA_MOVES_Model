/**************************************************************************************************
 * @(#)MasterLoopContext.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.*;
import gov.epa.otaq.moves.common.StringUtilities;

/**
 * The current execution context for the MasterLoop. An object of this class keeps track of the
 * current process, location, and calendar context of the MasterLoop. It also keeps track of the
 * granularity and execution priority at which the loop is executing. The MasterLoopContext is
 * passed to the MasterLoopables so that they can determine what context they are performing
 * their calculations for.
 *
 * @author		Wesley Faler
 * @version		2013-04-29
**/
public class MasterLoopContext {
	public static interface IContextFilter {
		/**
		 * Examine a context for suitability.  Used to override the natural execution hierarchy.
		 * @param context Context to be examined
		 * @return true if the context should be used by a MasterLoopable.
		**/
		public boolean doesProcessContext(MasterLoopContext context);
	}

	/** The ID of the current simulation run **/
	public int movesRunID;

	/** The pseudo-random uncertainty modeling iteration that the loop is iterating on. **/
	public int iterationID;

	/** The EmissionProcess that the loop is iterating on. **/
	public EmissionProcess iterProcess;
	public ArrayList<EmissionProcess> chainedProcesses = null;

	/** The location that the loop is iterating on. **/
	public ExecutionLocation iterLocation;

	/** The current year (4 digits) **/
	public int year;

	/**
	 * The database ID of the current month, <b>NOT</b> necessarily related to the traditional
	 * 1 through 12 numbering for months.
	**/
	public int monthID;

	/**
	 * The database ID of the current day, <b>NOT</b> necessarily related to the traditional
	 * 1 through 31 (or 1 through 7) numbering for days (or days of the week).
	**/
	public int dayID;

	/**
	 * The database ID of the current hour, <b>NOT</b> necessarily related to the traditional
	 * 0 through 23 numbering for hours of the day.
	**/
	public int hourID;

	/** The granularity that the loop is iterating at. **/
	public MasterLoopGranularity executionGranularity;

	/** The loop execution priority that the loop is iterating at. **/
	public int executionPriority;

	/**
	 * Flag indicating whether this context is before data creation and processing (and hence
	 * should be passed to MasterLoopable.executeLoop) or afterwards (and should be passed to
	 * MasterLoopable.cleanDataLoop).
	**/
	public boolean isCleanUp = false;

	/** The MasterLoop object that owns this context, may be null **/
	public MasterLoop owningLoop = null;
	/** Current MasterLoopable object using this context **/
	public MasterLoopable loopable = null;
	/** Name of loopable's underlying class.  This is used when matching serialized contexts to objects **/
	public String loopableClassName = null;
	/** true if the using MasterLoopable object has initiated asynchronous operation by creating bundles **/
	public boolean isDeferred = false;
	/** The total number of bundles need to be created **/
	public int totalBundles = 0;
	/** The number of bundles remaining to be retrieved **/
	public int bundleCount = 0;
	/** true if bundles are still being created **/
	public boolean isCreatingBundles = false;

	/** Standard Constructor **/
	public MasterLoopContext() {
		setProcess(null);
	}

	/**
	 * Sets a new run iteration..
	 * @param newIterationID The new iteration to use.
	**/
	public synchronized void setIteration(int newIterationID) {
		iterationID = newIterationID;
		setProcess(null);
	}

	/**
	 * Sets a new run id
	 * @param newRunID The new iteration to use.
	**/
	public synchronized void setRunID(int newRunID) {
		movesRunID = newRunID;
		setProcess(null);
	}

	/**
	 * Sets a new process. Clears all elements down the loop chain.
	 * @param emissionProcess The new EmissionProcess to use.
	**/
	public synchronized void setProcess(EmissionProcess emissionProcess) {
		iterProcess = emissionProcess;
		setLocation(null);
	}

	/**
	 * Sets a new location. Clears all elements down the loop chain.
	 * @param location The new location to use.
	**/
	public synchronized void setLocation(ExecutionLocation location) {
		iterLocation = location;
		resetTime();
	}

	/** Clears current year, month, day, and hour settings. **/
	public synchronized void resetTime() {
		year = -1;
		monthID = -1;
		dayID = -1;
		hourID = -1;
	}

	/**
	 * Sets a new granularity. Clears all elements down the loop chain.
	 * @param granularity The new granularity to use.
	**/
	public synchronized void setGranularity(MasterLoopGranularity granularity) {
		executionGranularity = granularity;
		setExecutionPriority(0);
	}

	/**
	 * Sets a new priority level. Clears all elements down the loop chain.
	 * @param priority The new priority level to use.
	**/
	public synchronized void setExecutionPriority(int priority) {
		executionPriority = priority;
	}

	/** Clear any accumulated chained processes **/
	public synchronized void resetChainedProcesses() {
		if(chainedProcesses != null) {
			chainedProcesses.clear();
		}
	}

	/**
	 * Accumulate a process other than the current iteration process due to chaining.
	 * @param p process being added due to a chained calculator, may be null in which
	 * case it will be ignored
	**/
	public synchronized void addChainedProcess(EmissionProcess p) {
		if(p == null) {
			return;
		}
		if(chainedProcesses == null) {
			chainedProcesses = new ArrayList<EmissionProcess>();
		}
		if(!chainedProcesses.contains(p)) {
			chainedProcesses.add(p);
		}
	}

	/**
	 * Check a process' inclusion directly and via chained contexts
	 * @param p process to be checked
	 * @return true if p is directly or indirectly part of the context
	**/
	public boolean hasProcess(EmissionProcess p) {
		if(p.databaseKey == iterProcess.databaseKey) {
			return true;
		}
		if(chainedProcesses != null) {
			for(int i=0;i<chainedProcesses.size();i++) {
				if(chainedProcesses.get(i).databaseKey == p.databaseKey) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Get a comma-separated list of the current process and all chained processes.
	 * @return CSV list of the current process and all chained processes, never blank, never null
	**/
	public String getAllProcessesCSV() {
		TreeSet<Integer> seen = new TreeSet<Integer>();
		if(iterProcess != null) {
			seen.add(Integer.valueOf(iterProcess.databaseKey));
		}
		if(chainedProcesses != null) {
			for(EmissionProcess p : chainedProcesses) {
				seen.add(Integer.valueOf(p.databaseKey));
			}
		}
		if(seen.size() <= 0) {
			return "0";
		} else {
			String csv = "";
			for(Integer i : seen) {
				if(csv.length() > 0) {
					csv += ",";
				}
				csv += i;
			}
			return csv;
		}
	}

	/**
	 * Get a textual representation of the object.
	 * @return a human readable string for this object
	**/
	public String toString() {
		String result = "context: ";
		result += iterProcess.toString() + "@" + iterLocation.toString();
		return result;
	}

	/** Indicate that a generator will be creating bundles **/
	public synchronized void beginCreatingDeferredBundles() {
		isCreatingBundles = true;
		isDeferred = true;
		bundleCount = 0;
		if(loopable != null) {
			loopableClassName = loopable.getClass().getCanonicalName();
		}
		// Notify the MasterLoop
		owningLoop.contextIsMakingBundles(this);
	}

	/** Record the fact that a bundle was created **/
	public synchronized void addBundle() {
		bundleCount++;
	}

	/**
	 * Clone this context so that it can be used with a new MasterLoopable object
	 * @param a clone of this context
	**/
	public MasterLoopContext copyForLoopable() {
		MasterLoopContext clone = new MasterLoopContext();
		clone.movesRunID = movesRunID;
		clone.iterationID = iterationID;
		clone.iterProcess = iterProcess;
		clone.iterLocation = iterLocation;
		clone.year = year;
		clone.monthID = monthID;
		clone.dayID = dayID;
		clone.hourID = hourID;
		clone.executionGranularity = executionGranularity;
		clone.executionPriority = executionPriority;
		clone.isCleanUp = isCleanUp;
		// Other fields intentionally omitted
		return clone;
	}

	/**
	 * Compare this object to a deserialized context recovered from a bundle.
	 * @param other a deserialized context, lacking anything but baseline information
	 * @param otherLoopableClassName the class name of the loopable assicated with the other context
	 * @return true if other context and loopable matches this context
	**/
	public boolean doesMatch(MasterLoopContext other) {
		return movesRunID == other.movesRunID
				&& iterationID == other.iterationID
				&& iterProcess.compareTo(other.iterProcess) == 0
				&& iterLocation.compareTo(other.iterLocation) == 0
				&& year == other.year
				&& monthID == other.monthID
				&& dayID == other.dayID
				&& hourID == other.hourID
				&& executionGranularity.compareTo(other.executionGranularity) == 0
				&& executionPriority == other.executionPriority
				&& isCleanUp == other.isCleanUp
				&& loopableClassName.equals(other.loopableClassName);
	}

	/**
	 * Convert this context into a format suitable for the machine-readable portion of a BundleManifest object.
	 * @return machine-readable context text for a BundleManifest
	**/
	public String toBundleManifestContext() {
		if(loopable != null) {
			loopableClassName = loopable.getClass().getCanonicalName();
		}
		String result = "";
		result += "|run:" + movesRunID;
		result += "|iter:" + iterationID;
		result += "|proc:" + (iterProcess == null? 0 : iterProcess.databaseKey);
		result += "|road:" + (iterLocation == null? 0 : iterLocation.roadTypeRecordID);
		result += "|link:" + (iterLocation == null? 0 : iterLocation.linkRecordID);
		result += "|zone:" + (iterLocation == null? 0 : iterLocation.zoneRecordID);
		result += "|cty:" + (iterLocation == null? 0 : iterLocation.countyRecordID);
		result += "|st:" + (iterLocation == null? 0 : iterLocation.stateRecordID);
		result += "|y:" + year;
		result += "|m:" + monthID;
		result += "|d:" + dayID;
		result += "|h:" + hourID;
		result += "|gran:" + executionGranularity.toString();
		result += "|pri:" + executionPriority;
		result += "|ph:" + (isCleanUp?"clean":"exec");
		result += "|class:" + loopableClassName;
		result += "|";
		return result;
	}

	/**
	 * Convert this context into a format suitable for the human-readable portion of a BundleManifest object.
	 * @return human-readable context text for a BundleManifest
	**/
	public String toBundleManifestContextForHumans() {
		if(loopable != null) {
			loopableClassName = loopable.getClass().getCanonicalName();
		}
		String result = "";
		result += "Process: " + iterProcess.databaseKey + " " + iterProcess.processName;
		result += "\nState: " + iterLocation.stateRecordID;
		result += "\nCounty: " + iterLocation.countyRecordID;
		result += "\nZone: " + iterLocation.zoneRecordID;
		result += "\nLink: " + iterLocation.linkRecordID;
		result += "\nRoad Type: " + iterLocation.roadTypeRecordID;
		result += "\nYear: " + year;
		result += "\nMonth: " + monthID;
		result += "\nDay: " + dayID;
		result += "\nHour: " + hourID;
		result += "\nGranularity: " + executionGranularity.toString();
		result += "\nPriority: " + executionPriority;
		result += "\nPhase: " + (isCleanUp?"clean":"exec");
		result += "\nModule: " + loopableClassName;
		result += "\nIteration: " + iterationID;
		result += "\nMovesRunID: " + movesRunID;
		return result;
	}

	/**
	 * Fill this context from the machine-readable context within a BundleManifest
	 * @param context the machine-readable context within a BundleManifest
	**/
	public void fromBundleManifestContext(String context) {
		if(iterLocation == null) {
			iterLocation = new ExecutionLocation();
		}
		if(executionGranularity == null) {
			executionGranularity = MasterLoopGranularity.HOUR;
		}
		if(loopableClassName == null) {
			loopableClassName = "";
		}
		String[] parts = context.split("\\|");
		if(parts == null || parts.length <= 0) {
			return;
		}
		for(int i=0;i<parts.length;i++) {
			int index = parts[i].indexOf(":");
			if(index < 0) {
				continue;
			}
			String name = parts[i].substring(0,index);
			String valueText = parts[i].substring(index+1);
			int valueID = 0;
			if(StringUtilities.isDigits(valueText)) {
				try {
					valueID = Integer.parseInt(valueText);
				} catch(Exception e) {
					// Nothing to do here
				}
			}
			if (name.equalsIgnoreCase("run")) {
				movesRunID = valueID;
			} else if (name.equalsIgnoreCase("iter")) {
				iterationID = valueID;
			} else if(name.equalsIgnoreCase("proc")) {
				iterProcess = EmissionProcess.findByID(valueID);
			} else if(name.equalsIgnoreCase("road")) {
				iterLocation.roadTypeRecordID = valueID;
			} else if(name.equalsIgnoreCase("link")) {
				iterLocation.linkRecordID = valueID;
			} else if(name.equalsIgnoreCase("zone")) {
				iterLocation.zoneRecordID = valueID;
			} else if(name.equalsIgnoreCase("cty")) {
				iterLocation.countyRecordID = valueID;
			} else if(name.equalsIgnoreCase("st")) {
				iterLocation.stateRecordID = valueID;
			} else if(name.equalsIgnoreCase("y")) {
				year = valueID;
			} else if(name.equalsIgnoreCase("m")) {
				monthID = valueID;
			} else if(name.equalsIgnoreCase("d")) {
				dayID = valueID;
			} else if(name.equalsIgnoreCase("h")) {
				hourID = valueID;
			} else if(name.equalsIgnoreCase("gran")) {
				executionGranularity = MasterLoopGranularity.find(valueText);
			} else if(name.equalsIgnoreCase("pri")) {
				executionPriority = valueID;
			} else if(name.equalsIgnoreCase("ph")) {
				isCleanUp = valueText.equalsIgnoreCase("clean");
			} else if(name.equalsIgnoreCase("class")) {
				loopableClassName = valueText;
			}
		}
		if(executionGranularity == null) {
			executionGranularity = MasterLoopGranularity.HOUR;
		}
		if(loopableClassName == null) {
			loopableClassName = "";
		}
	}
}
