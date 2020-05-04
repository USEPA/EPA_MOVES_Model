/**************************************************************************************************
 * @(#)HydrocarbonUnitSystem.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.*;

/**
 * This class, which acts like a "typesafe enum", describes a hydrocarbon unit system.
 * 
 * @author		Wesley Faler
 * @version		2009-04-05
**/
public class HydrocarbonUnitSystem {
	/** The list of all instances of this class. **/
	static LinkedList<HydrocarbonUnitSystem> allInstances = new LinkedList<HydrocarbonUnitSystem>();

	/** Enumerated type (as static instance) for the NMHC unit system. **/
	public static final HydrocarbonUnitSystem NMHC
			= new HydrocarbonUnitSystem("NMHC");
	/** Enumerated type (as static instance) for the NMOG unit system. **/
	public static final HydrocarbonUnitSystem NMOG
			= new HydrocarbonUnitSystem("NMOG");
	/** Enumerated type (as static instance) for the THC unit system. **/
	public static final HydrocarbonUnitSystem THC
			= new HydrocarbonUnitSystem("THC");
	/** Enumerated type (as static instance) for the VOC unit system. **/
	public static final HydrocarbonUnitSystem VOC
			= new HydrocarbonUnitSystem("VOC");
	/** Enumerated type (as static instance) for the TOG unit system. **/
	public static final HydrocarbonUnitSystem TOG
			= new HydrocarbonUnitSystem("TOG");

	/** Text description of the type. **/
	private String description;

	/**
	 * This is a private constructor to prevent any instances being created outside this
	 * class. The only existing instances will be the constant ones defined above in this
	 * class.
	 * 
	 * @param description The textual description of the type.
	**/
	private HydrocarbonUnitSystem(String description) {
		this.description = description;
		allInstances.add(this);
	}

	/**
	 * Provides textual description of the type.
	 * 
	 * @return The textual description of the type.
	**/
	public String toString() {
		return description;
	}

	/**
	 * Gets the constant type based on a text name.
	 * @param name The name of the type
	 * @return The constant type. This will be null if the given name wasn't found.
	**/
	public static HydrocarbonUnitSystem getByName(String name) {
		for(Iterator<HydrocarbonUnitSystem> instanceIter = allInstances.iterator(); 
				instanceIter.hasNext();) {
			HydrocarbonUnitSystem iterType = (HydrocarbonUnitSystem)instanceIter.next();
			if (iterType.description.compareToIgnoreCase(name) == 0) {
				return iterType;
			}
		}
		return null;
	}

	/**
	 * Overrides Object.equals().
	 * @param other The other HydrocarbonUnitSystem to check.
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(other instanceof HydrocarbonUnitSystem) {
			HydrocarbonUnitSystem otherHydrocarbonUnitSystem = (HydrocarbonUnitSystem)other;
			return this.description.compareTo(otherHydrocarbonUnitSystem.description) == 0;
		} else {
			throw new ClassCastException();
		}
	}
}
