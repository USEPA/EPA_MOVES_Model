/**************************************************************************************************
 * @(#)ModelDomain.java 
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
 * Typesafe enumeration for model domain.  The national allocation domain requires that each
 * county be a fraction of some larger national number with allocation factors used to
 * apportion activity.  Single county domain implies a singel county is being run with allocation
 * factors of unity.  Project implies a detailed link-level vehicle model.
 *
 * @author		wfaler
 * @version		2008-11-08
**/
public class ModelDomain {
	/** The list of all ModelDomain instances **/
	static TreeMapIgnoreCase allInstances = new TreeMapIgnoreCase();

	/** Enumerated type (as static instance) for a model domain value. **/
	public static final ModelDomain NATIONAL_ALLOCATION = new ModelDomain("NATIONAL");
	/** Enumerated type (as static instance) for a model domain value. **/
	public static final ModelDomain SINGLE_COUNTY = new ModelDomain("SINGLE");
	/** Enumerated type (as static instance) for a model domain value. **/
	public static final ModelDomain PROJECT = new ModelDomain("PROJECT");

	/** The textual description of the type. **/
	private String description;

	/**
	 * Constructor, but private, and as such this class can not be instatiated except
	 * by the static types defined above.
	 * 
	 * @param descriptionToUse Textual description of the type.
	**/
	private ModelDomain(String descriptionToUse) {
		description = descriptionToUse;
		allInstances.put(description,this);
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
	 * Gets the constant type based on a text name.
	 * 
	 * @param name The name of the type
	 * @return The constant type. This will be null if the given name wasn't found.
	**/
	public static ModelDomain getByName(String name) {
		return (ModelDomain)allInstances.get(name);
	}
}
