/**************************************************************************************************
 * @(#)SCCFuelType.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * This class, which acts like a "typesafe enum", describes an SCC fuel type.
 * 
 * @author		Cimulus
 * @version		2004-01-06
**/
public class SCCFuelType {
	/** The number of SCCFuelType instances **/
	public static final int NUM_TYPES = 4;

	/** All SCCFuelType instances **/
	private static SCCFuelType[] allFuelTypes = new SCCFuelType[NUM_TYPES];
	
	/** Enumerated type for compressed natural gas. **/
	public static final SCCFuelType CNG = new SCCFuelType(0, "CNG");
	/** Enumerated type for gasoline. **/
	public static final SCCFuelType GAS = new SCCFuelType(1, "Gas");
	/** Enumerated type for diesel. **/
	public static final SCCFuelType DIESEL = new SCCFuelType(2, "Dies");
	/** Enumerated type for liquefied petroleum gas. **/
	public static final SCCFuelType LPG = new SCCFuelType(2, "LPG");

	/** Numeric array index. Must be unique and less than NUM_TYPES. **/
	private int arrayIndex;
	/** The textual name of the type. **/
	private String name;

	/**
	 * Constructor, but private, and as such this class can not be instantiated except
	 * by the static types defined above.
	 * 
	 * @param arrayIndex Numeric array index. Must be unique and less than NUM_TYPES
	 * @param name Textual name of the type.
	**/
	private SCCFuelType(int arrayIndex, String name) {
		this.arrayIndex = arrayIndex;
		this.name = name;
		
		allFuelTypes[arrayIndex] = this;
	}

	/**
	 * Provides the textual name of the type.
	 * 
	 * @return The textual name of the type.
	**/
	public String toString() {
		return name;
	}
	
	/**
	 * Gets the enum associated with the given name
	 * 
	 * @param name The target textual name.
	 * @return The matching enum value or null if there is no match.
	**/
	public static SCCFuelType getTypeByName(String name) {
		for(int i = 0; i < NUM_TYPES; i++) {
			if ((allFuelTypes[i] != null)
					&& (allFuelTypes[i].name.compareToIgnoreCase(name) == 0)) {
				return allFuelTypes[i];
			}
		}
		
		return null;
	}
}
