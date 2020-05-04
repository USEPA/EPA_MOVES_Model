/**************************************************************************************************
 * @(#)GeographicOutputDetailLevel.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import java.util.*;

/**
 * This class, which acts like a "typesafe enum", describes a level of geographic detail that
 * data is output at.
 * 
 * @author		Wesley Faler
 * @version		2009-03-30
**/
public class GeographicOutputDetailLevel {
	/** The list of all GeographicOutputDetailLevel instances **/
	static LinkedList<GeographicOutputDetailLevel> allInstances
			= new LinkedList<GeographicOutputDetailLevel>();

	/** Constant object instance representing the nation level. **/
	public static final GeographicOutputDetailLevel NATION
			= new GeographicOutputDetailLevel("NATION");
	/** Constant object instance representing the state level. **/
	public static final GeographicOutputDetailLevel STATE
			= new GeographicOutputDetailLevel("STATE");
	/** Constant object instance representing the county level. **/
	public static final GeographicOutputDetailLevel COUNTY
			= new GeographicOutputDetailLevel("COUNTY");
	/** Constant object instance representing the zone level. **/
	public static final GeographicOutputDetailLevel ZONE
			= new GeographicOutputDetailLevel("ZONE");
	/** Constant object instance representing the roadtype level. **/
	public static final GeographicOutputDetailLevel ROADTYPE
			= new GeographicOutputDetailLevel("ROADTYPE");
	/** Constant object instance representing the link level. **/
	public static final GeographicOutputDetailLevel LINK
			= new GeographicOutputDetailLevel("LINK");

	/** Text description of the type. **/
	private String description;

	/**
	 * This is a private constructor to prevent any instances being created outside this
	 * class. The only existing instances will be the constant ones defined above in this
	 * class.
	 * 
	 * @param description The textual description of the type.
	**/
	private GeographicOutputDetailLevel(String description) {
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
	 * 
	 * @param name The name of the type
	 * @return The constant type. This will be null if the given name wasn't found.
	**/
	public static GeographicOutputDetailLevel getByName(String name) {
		for(Iterator<GeographicOutputDetailLevel> instanceIter = allInstances.iterator();
				instanceIter.hasNext();) {
			GeographicOutputDetailLevel iterLevel =
					(GeographicOutputDetailLevel)instanceIter.next();
			if(iterLevel.description.compareToIgnoreCase(name) == 0) {
				return iterLevel;
			}
		}

		return null;
	}
}
