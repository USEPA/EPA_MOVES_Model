/**************************************************************************************************
 * @(#)GeographicSelectionType.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.Iterator;
import java.util.LinkedList;

/**
 * This class, which acts like a "typesafe enum", describes the type of a GeographicSelection.
 * 
 * @author		Wesley Faler
 * @version		2009-03-30
**/
public class GeographicSelectionType implements Comparable {
	/** The list of all instances of this class. **/
	static LinkedList<GeographicSelectionType> allInstances
			= new LinkedList<GeographicSelectionType>();

	/** Enumerated type (as static instance) for a LINK selection type. **/
	public static final GeographicSelectionType LINK
			= new GeographicSelectionType("LINK", 1);
	/** Enumerated type (as static instance) for a ZONE selection type. **/
	public static final GeographicSelectionType ZONE
			= new GeographicSelectionType("ZONE", 2);
	/** Enumerated type (as static instance) for a COUNTY selection type. **/
	public static final GeographicSelectionType COUNTY
			= new GeographicSelectionType("COUNTY", 3);
	/** Enumerated type (as static instance) for a STATE selection type. **/
	public static final GeographicSelectionType STATE
			= new GeographicSelectionType("STATE", 4);
	/** Enumerated type (as static instance) for a NATION selection type. **/
	public static final GeographicSelectionType NATION
			= new GeographicSelectionType("NATION", 5);

	/** Provides textual description of the type. **/
	private String description;
	/** Numeric value that dictates sort order **/
	private int sortOrder;

	/**
	 * Constructor, but private, and as such this class can not be instatiated except
	 * by the static types defined above.
	 * 
	 * @param description The textual description of the type.
	 * @param sortOrder The position of this item in the list.
	**/
	private GeographicSelectionType(String description, int sortOrder) {
		this.description = description;
		this.sortOrder = sortOrder;
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
	 * Compares two GeographicSelectionType objects.
	 * Coarser granularity > finer granularity (Example: nation > link)
	 * 
	 * @param object Another GeographicSelectionType object to compare against.
	 * @return A value > 0 if this is of coarser granularity than the other object.
	 * A value < 0 if this is of finer granularity. Exactly 0 if the two objects
	 * are the same.
	**/
	public int compareTo(Object object) {
		GeographicSelectionType other = (GeographicSelectionType)object;
		return sortOrder - other.sortOrder;
	}

	/**
	 * Gets the constant type based on a text name.
	 * 
	 * @param name The name of the type
	 * @return The constant type. This will be null if the given name wasn't found.
	**/
	public static GeographicSelectionType getByName(String name) {
		for(Iterator<GeographicSelectionType> instanceIter = allInstances.iterator();
				instanceIter.hasNext();) {
			GeographicSelectionType iterType = (GeographicSelectionType)instanceIter.next();
			if(iterType.description.compareToIgnoreCase(name) == 0) {
				return iterType;
			}
		}

		return null;
	}
}
