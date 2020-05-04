/**************************************************************************************************
 * @(#)DistanceMeasurementSystem.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * A "typesafe enum" class that represents a specifc distance measurement system such as meters.
 *
 * @author		Wesley Faler
 * @version		2009-03-30
**/
public class DistanceMeasurementSystem {
	/** A collection of all available instances of this type. Useful for find by description. **/
	public static LinkedList<DistanceMeasurementSystem> allTypes =
			new LinkedList<DistanceMeasurementSystem>();
	
	/** Metric kilometers type. **/
	public static final DistanceMeasurementSystem KILOMETERS = 
			new DistanceMeasurementSystem("Kilometers", "km", 1000);
	/** International miles type with conversion from www.onlineconversion.com **/
	public static final DistanceMeasurementSystem MILES = 
			new DistanceMeasurementSystem("Miles", "mi", 1609.344 );

	/** The description of this type. **/
	private String description;
	/** The standard abbreviation of this type. **/
	private String standardAbbreviation;
	/** The conversion factor to go from units of the current to meters. **/
	private double conversionToMetersFactor;

	/**
	 * Private Constructor. No instances should be created outside of this class.
	 * @param description The description of this type.
	 * @param standardAbbreviation The standard abbreviation for this type.
	 * @param conversionToMetersFactor The conversion factor to go from units of the current object
	 * to meters.
	**/
	private DistanceMeasurementSystem(String description, String standardAbbreviation
			, double conversionToMetersFactor) {
		this.description = description;
		this.standardAbbreviation = standardAbbreviation;
		this.conversionToMetersFactor = conversionToMetersFactor;
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
	 * Provides the conversion factor to go from units of the current object to meters.
	 * @return The conversion factor to go from units of the current object to meters.
	**/
	public double getConversionToMetersFactor() {
		return conversionToMetersFactor;
	}

	/**
	 * Provides a textual description of the current object.
	 * @return The textual description of the current object.
	**/
	public String toString() {
		return description;
	}

	/**
	 * Finds the DistanceMeasurementSystem type class that uses the specified description.
	 * @param description The textual description to look for.
	 * @return The DistanceMeasurementSystem object. This will be null if the specified
	 * description could not be matched.
	**/
	public static DistanceMeasurementSystem getByDescription(String description) {
		for(Iterator<DistanceMeasurementSystem> i = allTypes.iterator(); i.hasNext();) {
			DistanceMeasurementSystem iterSystem = (DistanceMeasurementSystem) i.next();
			if(iterSystem.description.compareToIgnoreCase(description) == 0) {
				return iterSystem;
			}
		}

		return null;
	}

	/**
	 * Overrides Object.equals().
	 * @param other The other DistanceMeasurementSystem object to check.  Allows null (but returns
	 * false).
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(other == null) {
			return false;
		}
		if(other instanceof DistanceMeasurementSystem) {
			DistanceMeasurementSystem otherDistanceMeasurementSystem =
					(DistanceMeasurementSystem)other;
			// These instances are unique by description
			return this.description.compareTo(otherDistanceMeasurementSystem.description) == 0;
		} else {
			throw new ClassCastException();
		}
	}
}
