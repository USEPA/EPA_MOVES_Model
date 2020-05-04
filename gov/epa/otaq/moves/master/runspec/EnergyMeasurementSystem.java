/**************************************************************************************************
 * @(#)EnergyMeasurementSystem.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * A "typesafe enum" class that represents a specifc energy measurement system such as joules.
 *
 * @author		Wesley Faler
 * @version		2009-03-30
**/
public class EnergyMeasurementSystem {
	/** A collection of all available instances of this type. Useful for find by description. **/
	public static LinkedList<EnergyMeasurementSystem> allTypes
			= new LinkedList<EnergyMeasurementSystem>();
	
	/** Metric joules type. **/
	public static final EnergyMeasurementSystem JOULES = 
			new EnergyMeasurementSystem("Joules", "J", 1);
	/**
	 * Metric KiloJoules type.
	**/
	public static final EnergyMeasurementSystem KJOULES = 
			new EnergyMeasurementSystem("KiloJoules", "KJ", 1000);
	/**
	 * Millions of International BTU (not thermal BTU)
	 * with conversion from www.onlineconversion.com
	**/
	public static final EnergyMeasurementSystem MMBTU = 
			new EnergyMeasurementSystem("Million BTU", "MMBTU", 1055.0559 * 1000000);

	/** The description of this type. **/
	private String description;
	/** The standard abbreviation of this type. **/
	private String standardAbbreviation;
	/**
	 * The conversion factor to go from units of the current
	 * object to Joules. 
	 * In units of [Joules per this unit]
	**/
	private double conversionToJoulesFactor;

	/**
	 * Private Constructor. No instances should be created outside of this class.
	 * @param description The description of this type.
	 * @param standardAbbreviation The standard abbreviation for this type.
	 * @param conversionToJoulesFactor The conversion factor to go from units of the 
	 * current object to Joules.
	**/
	private EnergyMeasurementSystem(String description, String standardAbbreviation
			, double conversionToJoulesFactor) {
		this.description = description;
		this.standardAbbreviation = standardAbbreviation;
		this.conversionToJoulesFactor = conversionToJoulesFactor;
		allTypes.add(this);
	}

	/**
	 * Simple access method.
	 * @return The standard abbreviation of this type.
	**/
	public String getStandardAbbreviation() {
		return standardAbbreviation;
	}

	/**
	 * Provides the conversion factor to go from units of the current object to Joules.
	 * @return The conversion factor to go from units of this type to Joules.
	**/
	public double getConversionToJoulesFactor() {
		return conversionToJoulesFactor;
	}

	/**
	 * Provides a textual description of this type.
	 * @return The textual description of this type.
	**/
	public String toString() {
		return description;
	}

	/**
	 * Finds the EnergyMeasurementSystem type class that uses the specified description.
	 * @param description The textual description to look for.
	 * @return The EnergyMeasurementSystem object. This will be null if the specified
	 * description could not be matched.
	**/
	public static EnergyMeasurementSystem getByDescription(String description) {
		for(Iterator<EnergyMeasurementSystem> i = allTypes.iterator(); i.hasNext();) {
			EnergyMeasurementSystem iterSystem = (EnergyMeasurementSystem) i.next();
			if(iterSystem.description.compareToIgnoreCase(description) == 0) {
				return iterSystem;
			}
		}

		return null;
	}

	/**
	 * Overrides Object.equals().
	 * @param other The other EnergyMeasurementSystem object to check.  Allows null (but returns
	 * false).
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(other == null) {
			return false;
		}
		if(other instanceof EnergyMeasurementSystem) {
			EnergyMeasurementSystem otherEnergyMeasurementSystem = (EnergyMeasurementSystem)other;
			// These instances are unique by description
			return this.description.compareTo(otherEnergyMeasurementSystem.description) == 0;
		} else {
			throw new ClassCastException();
		}
	}
}
