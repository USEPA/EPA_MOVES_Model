/**************************************************************************************************
 * @(#)TimeMeasurementSystem.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * A "typesafe enum" class that represents a specifc time measurement system such as seconds.
 *
 * @author		Cimulus
 * @version		2007-01-01
**/
public class TimeMeasurementSystem {
	/** A collection of all available instances of this type. Useful for find by description. **/
	public static LinkedList<TimeMeasurementSystem> allTypes = 
			new LinkedList<TimeMeasurementSystem>();
	
	/** Seconds type. **/
	public static final TimeMeasurementSystem SECONDS = 
			new TimeMeasurementSystem("Seconds", "sec", 1, 100);
	/** Hours type **/
	public static final TimeMeasurementSystem HOURS = 
			new TimeMeasurementSystem("Hours", "hour", 60*60, 200);
	/** Days type **/
	public static final TimeMeasurementSystem DAYS = 
			new TimeMeasurementSystem("Days", "day", 60*60*24, 300);
	/** Portion-of-week type **/
	public static final TimeMeasurementSystem PORTION_OF_WEEK = 
			new TimeMeasurementSystem("Portions of Week", "wkprt", 60*60*24, 400);
	/** Weeks type **/
	public static final TimeMeasurementSystem WEEKS = 
			new TimeMeasurementSystem("Weeks", "week", 60*60*24*7, 500);
	/** Months type **/
	public static final TimeMeasurementSystem MONTHS = 
			new TimeMeasurementSystem("Months", "month", 60*60*24*365.25/12, 600);
	/** Years type **/
	public static final TimeMeasurementSystem YEARS = 
			new TimeMeasurementSystem("Years", "year", 60*60*24*365.25, 700);
	
	/** The description of this type. **/
	private String description;
	/** The standard abbreviation of this type. **/
	private String standardAbbreviation;
	/** The conversion factor to go from units of this type to seconds. **/
	private double conversionToSecondsFactor;
	/** Sort order used for display **/
	private int sortOrder = 0;

	/**
	 * Private Constructor. No instances should be created outside of this class.
	 * @param description The description of this type.
	 * @param standardAbbreviation The standard abbreviation for this type.
	 * @param conversionToSecondsFactor The conversion factor to go from units of this type to
	 * seconds.
	 * @param sortOrder sort order for display
	**/
	private TimeMeasurementSystem(String description, String standardAbbreviation
			, double conversionToSecondsFactor, int sortOrder) {
		this.description = description;
		this.standardAbbreviation = standardAbbreviation;
		this.conversionToSecondsFactor = conversionToSecondsFactor;
		this.sortOrder = sortOrder;
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
	 * Provides the conversion factor to go from units of this type to seconds.
	 * @return The conversion factor to go from units of this type to seconds.
	**/
	public double getConversionToSecondsFactor() {
		return conversionToSecondsFactor;
	}

	/**
	 * Provides a textual description of this type.
	 * @return The textual description of this type.
	**/
	public String toString() {
		return description;
	}

	/**
	 * Finds the TimeMeasurementSystem type class that uses the specified description.
	 * @param description The textual description to look for.
	 * @return The TimeMeasurementSystem object. This will be null if the specified
	 * description could not be matched.
	**/
	public static TimeMeasurementSystem getByDescription(String description) {
		for (Iterator i = allTypes.iterator(); i.hasNext();) {
			TimeMeasurementSystem iterSystem = (TimeMeasurementSystem) i.next();
			if(iterSystem.description.compareToIgnoreCase(description) == 0) {
				return iterSystem;
			}
		}

		return null;
	}

	/**
	 * Overrides Object.equals().
	 * @param other The other TimeMeasurementSystem object to check.  Allows null (but returns
	 * false).
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(other == null) {
			return false;
		}
		if(other instanceof TimeMeasurementSystem) {
			TimeMeasurementSystem otherTimeMeasurementSystem = (TimeMeasurementSystem)other;
			// These instances are unique by description
			return this.description.compareTo(otherTimeMeasurementSystem.description) == 0;
		} else {
			throw new ClassCastException();
		}
	}
}
