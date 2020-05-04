/**************************************************************************************************
 * @(#)OutputTimeStep.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * A "typesafe enum" class that represents a specifc output time step granularity.
 *
 * @author		Wesley Faler
 * @version		2009-04-04
**/
public class OutputTimeStep implements Comparable{
	/** A collection of all available instances of this type. Useful for find by description. **/
	public static LinkedList<OutputTimeStep> allTypes = new LinkedList<OutputTimeStep>();
	
	/** Hour type of classical day **/
	public static final OutputTimeStep HOUR = new OutputTimeStep("Hour", 1, 
			"Hour", 100, true, false, false, false);
	/** Day type of classical day **/
	public static final OutputTimeStep CLASSICAL_DAY = new OutputTimeStep("24-Hour Day", 24, 
			"Day", 200, true, true, false, false );
	/** Portion of a week **/
	public static final OutputTimeStep PORTION_OF_WEEK = new OutputTimeStep("Portion of Week", 24, 
			"WkPrt", 300, false, true, false, false );
	/** Month type, scaled to accomodate the noOfRealDays feature for days of the week **/
	public static final OutputTimeStep MONTH = new OutputTimeStep("Month", 
			365.25 * 24.0 / 12.0, "Month", 400, false, true, true, false);
	/** Year type, scaled to accomodate the noOfRealDays feature for days of the week **/
	public static final OutputTimeStep YEAR = new OutputTimeStep("Year", 
			365.25 * 24.0, "Year", 500, false, true, true, true);

	/** True if a traditional 24-hour day is used as reference **/
	private boolean usesClassicalDay = false;

	/** Timestep requires that all the hours be selected in the TimeSpan. **/
	private boolean requiresAllHours = false;
	/** Timestep requires that all the days and hours be selected in the TimeSpan. **/
	private boolean requiresAllDays = false;
	/** Timestep requires that all the months, days, and hours be selected in the TimeSpan. **/
	private boolean requiresAllMonths = false;

	/** The description of this type. **/
	private String description;
	/** The average number of hours that a unit of this type contains. **/
	private double averageHours;
	/** Abbreviation stored in the database **/
	private String abbreviation;

	/** Sort order for display **/
	private int sortOrder = 0;

	/**
	 * Private Constructor, overloaded to set requiresAllDays indicator
	 * @param description The description of this type.
	 * @param averageHours The average number of hours that a unit of this type contains.
	 * @param abbreviation Abbreviation stored in the database
	 * @param sortOrder sort order for display
	 * @param usesClassicalDay True if a traditional 24-hour day is used as reference
	 * @param requiresAllHours Require that all the hours be selected.
	 * @param requiresAllDays Require that all the days be selected.
	 * @param requiresAllMonths Require that all the months be selected.
	**/
	private OutputTimeStep(String description, double averageHours, 
			String abbreviation, int sortOrder,
			boolean usesClassicalDay, boolean requiresAllHours,
			boolean requiresAllDays, boolean requiresAllMonths) {
		this.description = description;
		this.averageHours = averageHours;
		this.abbreviation = abbreviation;
		this.sortOrder = sortOrder;
		this.usesClassicalDay = usesClassicalDay;
		this.requiresAllHours = requiresAllHours;
		this.requiresAllDays = requiresAllDays;
		this.requiresAllMonths = requiresAllMonths;

		allTypes.add(this);
	}

	/**
	 * Provides the average number of hours that a unit of this type contains.
	 * @return The average number of hours that a unit of this type contains.
	**/
	public double getAverageHours() {
		return averageHours;
	}

	/**
	 * Gets the TimeMeasurementSystemDefault used for this OutputTimeStep.
	 * @return TimeMeasurementSystem default.
	**/
	public TimeMeasurementSystem getTimeMeasurementSystemDefault() {
		if(description.equals("Hour")) {
			return TimeMeasurementSystem.getByDescription("Hours");
		} else if(description.equals("24-Hour Day")) {
			return TimeMeasurementSystem.getByDescription("Days");
		} else if(description.equals("Portion of Week")) {
			return TimeMeasurementSystem.getByDescription("Portions of Week");
		} else if(description.equals("Month")) {
			return TimeMeasurementSystem.getByDescription("Months");
		} else if(description.equals("Year")) {
			return TimeMeasurementSystem.getByDescription("Years");
		}
		// Should never get here.
		return null;
	}

	/**
	 * Indicates if this timestep uses a classical 24-hour day
	 * @return true if a classical 24-hour day is used as the reference
	**/
	public boolean usesClassicalDay() {
		return usesClassicalDay;
	}

	/**
	 * Indicates if this timestep requires all the hours to be selected.
	 * @return if all the hours must be selected.
	**/
	public boolean requiresAllHours() {
		return requiresAllHours;
	}

	/**
	 * Indicates if this timestep requires all the days to be selected.
	 * @return if all the days must be selected.
	**/
	public boolean requiresAllDays() {
		return requiresAllDays;
	}

	/**
	 * Indicates if this timestep requires all the months to be selected.
	 * @return if all the months must be selected.
	**/
	public boolean requiresAllMonths() {
		return requiresAllMonths;
	}

	/**
	 * Provides a textual description of this type.
	 * @return The textual description of this type.
	**/
	public String toString() {
		return description;
	}

	/**
	 * Provides the abbreviation, suitable for storing in the database
	 * @return the abbreviation
	**/
	public String getStandardAbbreviation() {
		return abbreviation;
	}

	/**
	 * Finds the OutputTimeStep type class that uses the specified description.
	 * @param description The textual description to look for.
	 * @return The OutputTimeStep object. This will be null if the specified
	 * description could not be matched.
	**/
	public static OutputTimeStep getByDescription(String description) {
		for (Iterator<OutputTimeStep> i = allTypes.iterator(); i.hasNext();) {
			OutputTimeStep iterSystem = (OutputTimeStep) i.next();
			if(iterSystem.description.compareToIgnoreCase(description) == 0) {
				return iterSystem;
			}
		}

		return null;
	}

	/**
	 * Compares two OutputTimeStep objects.
	 * Coarser granularity > finer granularity (Example: nation > link)
	 * 
	 * @param object Another OutputTimeStep object to compare against.
	 * @return A value > 0 if this is of coarser granularity than the other object.
	 * A value < 0 if this is of finer granularity. Exactly 0 if the two objects
	 * are the same.
	**/
	public int compareTo(Object object) {
		OutputTimeStep other = (OutputTimeStep) object;
		return sortOrder - other.sortOrder;
	}
}
