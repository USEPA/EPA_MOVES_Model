/**************************************************************************************************
 * @(#)DistributedWorkFileState.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;

/**
 * This class, which acts like a "typesafe enum", describes the states that distributed work files
 * can exist in.
 * 
 * @author		Wesley Faler
 * @version		2010-02-06
**/
public class DistributedWorkFileState {
	/** The number of types supported by this class. **/
	public static final int NUM_TYPES = 4;
	/** Enumerated type (as static instance) for the TEMP state. **/
	public static final DistributedWorkFileState TEMP
			= new DistributedWorkFileState("TEMP");
	/** Enumerated type (as static instance) for the TODO state. **/
	public static final DistributedWorkFileState TODO
			= new DistributedWorkFileState("TODO");
	/** Enumerated type (as static instance) for the IN_PROGRESS state. **/
	public static final DistributedWorkFileState IN_PROGRESS
			= new DistributedWorkFileState("InProgress");
	/** Enumerated type (as static instance) for the DONE state. **/
	public static final DistributedWorkFileState DONE
			= new DistributedWorkFileState("DONE");
	/** All state instances **/
	public static ArrayList<DistributedWorkFileState> allStates;

	/** The textual description of the type. **/
	private String description;

	/**
	 * Constructor, but private, and as such this class can not be instatiated except
	 * by the static types defined above.
	 * 
	 * @param descriptionToUse Textual description of the type.
	**/
	private DistributedWorkFileState(String descriptionToUse) {
		description = descriptionToUse;
		if(allStates == null) {
			allStates = new ArrayList<DistributedWorkFileState>();
		}
		allStates.add(this);
	}

	/**
	 * Provides the textual description of the type.
	 * 
	 * @return The textual description of the type.
	**/
	public String toString() {
		return description;
	}

	/**
	 * Gets the static DistributedWorkFileState instance by description, or returns null if 
	 * not found.
	 * @param description The description to check.
	 * @return The corresponding DistributedWorkFileState object, or null if not found.
	**/
	public static DistributedWorkFileState getByDescription(String description) {
		if(description != null) {
			if(description.compareTo(DistributedWorkFileState.TEMP.toString()) == 0) {
				return DistributedWorkFileState.TEMP;
			} else if(description.compareTo(DistributedWorkFileState.TODO.toString()) == 0) {
				return DistributedWorkFileState.TODO;
			} else if(description.compareTo(DistributedWorkFileState.IN_PROGRESS.toString()) == 0) {
				return DistributedWorkFileState.IN_PROGRESS;
			} else if(description.compareTo(DistributedWorkFileState.DONE.toString()) == 0) {
				return DistributedWorkFileState.DONE;
			}
		}
		return null;
	}
}
