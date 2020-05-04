/**************************************************************************************************
 * @(#)ModelScale.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * This class, which acts like a "typesafe enum", describes the geographic scale at which a
 * simulation can be run at. For macro-scale, the simulation produces inventory results.
 * Mesoscale lookup provides emission rates instead of inventory results.
 * 
 * @author		Wesley Faler
 * @version		2009-12-02
**/
public class ModelScale {
	/** The list of all ModelScale instances **/
	static LinkedList<ModelScale> allInstances = new LinkedList<ModelScale>();

	/** Enumerated type (as static instance) for a model scale value. **/
	public static final ModelScale MACROSCALE = new ModelScale("Inv", 60 * 60);

	// Enumerated type (as static instance) for a model scale value.
	//public static final ModelScale MESOSCALE = new ModelScale("MESO", 60 * 60);

	/** Enumerated type (as static instance) for a model scale value. **/
	public static final ModelScale MESOSCALE_LOOKUP = new ModelScale("Rates", 60 * 60);

	// Enumerated type (as static instance) for a model scale value.
	//public static final ModelScale MICROSCALE  = new ModelScale("MICRO", 15 * 60);

	/** The textual description of the type. **/
	private String description;

	/** The size of the block of time that the model is run at in seconds. **/
	private int timeIncrementSeconds;

	/**
	 * Constructor, but private, and as such this class can not be instatiated except
	 * by the static types defined above.
	 * 
	 * @param description Textual description of the type.
	 * @param timeIncrementSeconds The size of the block of time that the model is run at in
	 * seconds.
	**/
	private ModelScale(String description, int timeIncrementSeconds) {
		this.description = description;
		this.timeIncrementSeconds = timeIncrementSeconds;
		allInstances.add(this);
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
	 * Provides the size of the block of time that the model is run at in seconds.
	 * 
	 * @return The size of the block of time that the model is run at in seconds.
	**/
	public int getTimeIncrementSeconds() {
		return timeIncrementSeconds;
	}

	/**
	 * Gets the constant type based on a text name.
	 * 
	 * @param name The name of the type
	 * @return The constant type. This will be null if the given name wasn't found.
	**/
	public static ModelScale getByName(String name) {
		for(Iterator<ModelScale> instanceIter = allInstances.iterator(); instanceIter.hasNext();) {
			ModelScale iterScale = (ModelScale)instanceIter.next();
			if (iterScale.description.compareToIgnoreCase(name) == 0) {
				return iterScale;
			}
		}

		return null;
	}
}
