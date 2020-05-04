/**************************************************************************************************
 * @(#)MassMeasurementSystem.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * A "typesafe enum" class that represents a specifc mass measurement system such as kilograms.
 *
 * @author		Wesley Faler
 * @version		2009-04-04
**/
public class MassMeasurementSystem {
	/** A collection of all available instances of this type. Useful for find by description. **/
	public static LinkedList<MassMeasurementSystem> allTypes = 
			new LinkedList<MassMeasurementSystem>();

	/** Metric kilograms type. **/
	public static final MassMeasurementSystem KILOGRAMS =
			new MassMeasurementSystem("Kilograms", "kg", 1);
	/** Metric grams type. **/
	public static final MassMeasurementSystem GRAMS =
			new MassMeasurementSystem("Grams", "g", 0.001);
	/** English pounds type. This is actually a unit of force and not mass. **/
	public static final MassMeasurementSystem POUNDS =
			new MassMeasurementSystem("Pounds", "lb", 0.453592);
	/**
	 * English tons type. This is actually a unit of force and not mass, 
	 * from www.onlineconversion.com
	**/
	public static final MassMeasurementSystem TONS =
			new MassMeasurementSystem("U.S. Ton", "ton", 907.184818);

	/** The description of this type. **/
	private String description;
	/** The standard abbreviation of this type. **/
	private String standardAbbreviation;
	/** The conversion factor to go from units of this type to kilograms. **/
	private double conversionToKilogramsFactor;

	/**
	 * Private Constructor. No instances should be created outside of this class.
	 * @param description The description of this type.
	 * @param standardAbbreviation The standard abbreviation for this type.
	 * @param conversionToKilogramsFactor The conversion factor to go from units of this type to
	 * kilograms.
	**/
	private MassMeasurementSystem(String description, String standardAbbreviation
			, double conversionToKilogramsFactor) {
		this.description = description;
		this.standardAbbreviation = standardAbbreviation;
		this.conversionToKilogramsFactor = conversionToKilogramsFactor;
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
	 * Provides the conversion factor to go from units of this type to kilograms.
	 * @return The conversion factor to go from units of this type to kilograms.
	**/
	public double getConversionToKilogramsFactor() {
		return conversionToKilogramsFactor;
	}

	/**
	 * Provides a textual description of this type.
	 * @return The textual description of this type.
	**/
	public String toString() {
		return description;
	}

	/**
	 * Finds the MassMeasurementSystem type class that uses the specified description.
	 * @param description The textual description to look for.
	 * @return The MassMeasurementSystem object. This will be null if the specified
	 * description could not be matched.
	**/
	public static MassMeasurementSystem getByDescription(String description) {
		for (Iterator<MassMeasurementSystem> i = allTypes.iterator(); i.hasNext();) {
			MassMeasurementSystem iterSystem = (MassMeasurementSystem) i.next();
			if(iterSystem.description.compareToIgnoreCase(description) == 0) {
				return iterSystem;
			}
		}

		return null;
	}

	/**
	 * Overrides Object.equals().
	 * @param other The other MassMeasurementSystem object to check.  Allows null (but returns
	 * false).
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(other == null) {
			return false;
		}
		if(other instanceof MassMeasurementSystem) {
			MassMeasurementSystem otherMassMeasurementSystem = (MassMeasurementSystem)other;
			// These instances are unique by description
			return this.description.compareTo(otherMassMeasurementSystem.description) == 0;
		} else {
			throw new ClassCastException();
		}
	}
}
