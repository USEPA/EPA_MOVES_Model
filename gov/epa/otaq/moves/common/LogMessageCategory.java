/**************************************************************************************************
 * @(#)LogMessageCategory.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * This class, which acts like a "typesafe enum", is used to specify the category of a message
 * being logged using the Logger and LogHandler classes. In general, the message category indicates
 * the severity of the message and it may be used in determining how the message is handled. DEBUG
 * and INFO messages, for example, may simply be logged to a file while WARNING and ERROR messages 
 * may require user acknowledgement. The RUN_ERROR category is a special category used only by the
 * Logger class to demark WARNING and ERROR messages that occur during a simulation run. Its use
 * circumvents the normal behavior of some Log Handlers to prompt the user for acknowledgement of
 * messages in these categories.
 * 
 * @author		Wesley Faler
 * @version		2009-04-04
**/
public class LogMessageCategory implements Comparable {
	/** A list of all the possible message categories. **/
	static LinkedList<LogMessageCategory> allCategories = new LinkedList<LogMessageCategory>();

	/** Value to indicate a message used during programming only. **/
	public static final LogMessageCategory DEBUG =
			new LogMessageCategory("DEBUG", 0);
	/** Value to indicate an informational message. **/
	public static final LogMessageCategory INFO =
			new LogMessageCategory("INFO", 1);
	/**
	 * Value to indicate that the program has done something unexpected, it probably can
	 * continue to run, but most likely will continue to have unexpected behavior.
	**/
	public static final LogMessageCategory WARNING =
			new LogMessageCategory("WARNING", 2);
	/** Value to indicate an error, the program cannot continue to run. **/
	public static final LogMessageCategory ERROR =
			new LogMessageCategory("ERROR", 3);
	/** 
	 * Value used only by the Logger class to indicate that a WARNING or ERROR message has occurred
	 * during a simulation run. This category supresses the normal prompting for user
	 * acknowledgement by some Log Handlers.
	**/
	public static final LogMessageCategory RUN_ERROR =
			new LogMessageCategory("RUN_ERROR", 4);

	/** The textual description of the type. **/
	String description;

	/** The numeric severity level for this type. Higher values indicate a higher severity. **/
	int severityLevel;

	/**
	 * Constructor, but private, and as such this class can not be instatiated except
	 * by the static types defined above.
	 * 
	 * @param description The textual description of the type.
	 * @param severityLevel The numeric severity level. Higher values indicate a higher severity.
	**/
	private LogMessageCategory(String description, int severityLevel) {
		this.description = description;
		this.severityLevel = severityLevel;

		allCategories.add(this);
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
	 * Gets the numeric severity level for this type. Higher values indicate a higher severity.
	 *
	 * @return The numeric severity level.
	**/
	public int getSeverityLevel() {
		return severityLevel;
	}

	/**
	 * Compares two LogMessageCategory objects by severity level. More severe > less severe.
	 * 
	 * @param object Another LogMessageCategory object to compare against.
	 * @return A value > 0 if this is more severe than the other object.
	 * A value < 0 if this is less severe. Exactly 0 if the two objects
	 * are the same severity level.
	**/
	public int compareTo(Object object) {
		LogMessageCategory other = (LogMessageCategory) object;
		return severityLevel - other.severityLevel;
	}

	/**
	 * Gets a LogMessageCategory type by name.
	 * @param name The text name that matches a specific log message category
	 * @return The LogMessageCategory object. This will be null if the given name couldn't
	 * be matched.
	**/
	public static LogMessageCategory getCategoryByName(String name) {
		for(Iterator<LogMessageCategory> i = allCategories.iterator(); i.hasNext(); ) {
			LogMessageCategory category = (LogMessageCategory)i.next();

			if(category.description.compareToIgnoreCase(name) == 0) {
				return category;
			}
		}

		return null;
	}
}
