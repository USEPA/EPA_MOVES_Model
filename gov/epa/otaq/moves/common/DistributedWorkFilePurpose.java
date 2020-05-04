/**************************************************************************************************
 * @(#)DistributedWorkFilePurpose.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;

/**
 * This class, which acts like a "typesafe enum", describes the purposes for distributed work files.
 * 
 * @author		Wesley Faler
 * @version		2010-02-06
**/
public class DistributedWorkFilePurpose {
	/** The number of types supported by this class. **/
	public static final int NUM_TYPES = 2;
	/** Enumerated type (as static instance) for the Calculator purpose. **/
	public static final DistributedWorkFilePurpose CALCULATOR
			= new DistributedWorkFilePurpose("Calc");
	/** Enumerated type (as static instance) for the Generator purpose. **/
	public static final DistributedWorkFilePurpose GENERATOR
			= new DistributedWorkFilePurpose("Gen");
	/** All purpose instances **/
	public static ArrayList<DistributedWorkFilePurpose> allPurposes;

	/** The textual description of the type. **/
	private String description;

	/**
	 * Constructor, but private, and as such this class can not be instatiated except
	 * by the static types defined above.
	 * 
	 * @param descriptionToUse Textual description of the type.
	**/
	private DistributedWorkFilePurpose(String descriptionToUse) {
		description = descriptionToUse;
		if(allPurposes == null) {
			allPurposes = new ArrayList<DistributedWorkFilePurpose>();
		}
		allPurposes.add(this);
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
	 * Gets the static DistributedWorkFilePurpose instance by description, or returns null if 
	 * not found.
	 * @param description The description to check.
	 * @return The corresponding DistributedWorkFilePurpose object, or null if not found.
	**/
	public static DistributedWorkFilePurpose getByDescription(String description) {
		if(description != null) {
			if(description.compareTo(DistributedWorkFilePurpose.CALCULATOR.toString()) == 0) {
				return DistributedWorkFilePurpose.CALCULATOR;
			} else if(description.compareTo(DistributedWorkFilePurpose.GENERATOR.toString()) == 0) {
				return DistributedWorkFilePurpose.GENERATOR;
			}
		}
		return null;
	}
}
