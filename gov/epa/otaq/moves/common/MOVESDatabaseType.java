/**************************************************************************************************
 * @(#)MOVESDatabaseType.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

/**
 * This class, which acts like a "typesafe enum", defines the three database types that are
 * frequently accessed by MOVES objects. These types are the Default, Execution, and Output
 * Databases. The Default Database contains configuration and the initial input data for MOVES.
 * It is used primarily by GUI objects and as the main source of input data to a simulation run.
 * The Execution Database is a temporary, execution-time, database initially created from a
 * selected subset of data in the Default and user-specified Input databases. This database is
 * primarily used by the MOVES Generators and Calculators. The Output Database contains MOVES
 * data in the final, "published," MOVES Output Table format. This database is populated by
 * OutputProcessor and processed by the IntegratedPostProcessors.
 *
 * @author		Wesley Faler
 * @version		2012-11-07
**/
public class MOVESDatabaseType {
	/** The number of types, should be one more than the max index used by the static objects. **/
	public static int NUM_TYPES = 3;

	/** An array of all the existing type objects. **/
	private static MOVESDatabaseType[] allTypes = new MOVESDatabaseType[NUM_TYPES];

	/** Enumerated type (as static instance) for a DEFAULT MOVES database. **/
	public static final MOVESDatabaseType DEFAULT = new MOVESDatabaseType(0, "Default");
	/** Enumerated type (as static instance) for a EXECUTION MOVES database. **/
	public static final MOVESDatabaseType EXECUTION = new MOVESDatabaseType(1, "Execution");
	/** Enumerated type (as static instance) for a OUTPUT MOVES database. **/
	public static final MOVESDatabaseType OUTPUT = new MOVESDatabaseType(2, "Output");

	// /** Enumerated type (as static instance) for a NONROAD DEFAULT MOVES database. **/
	//public static final MOVESDatabaseType NRDEFAULT = new MOVESDatabaseType(3, "NRDefault");

	/** A unique index that is less than NUM_TYPES. This can be used as an array index. **/
	private int index;
	/** A description of this type **/
	private String description;

	/**
	 * Constructor, but private, and as such this class can not be instatiated except
	 * by the static types defined above.
	 *
	 * @param index A unique index that is less than NUM_TYPES. This can be used as an array index.
	 * @param description A description of this type
	**/
	private MOVESDatabaseType(int index, String description) {
		this.index = index;
		this.description = description;
		if(!CompilationFlags.USE_NONROAD) {
			NUM_TYPES = 3;
		}
		if(CompilationFlags.USE_NONROAD || index < 3) {
			allTypes[index] = this;
		}
	}

	/**
	 * Provides an array index. Unique and less than NUM_TYPES.
	 *
	 * @return The unique array index.
	**/
	public int getIndex() {
		return index;
	}

	/**
	 * Provides an description of this type.
	 *
	 * @return A description of this type.
	**/
	public String toString() {
		return description;
	}

	/**
	 * Gets the object type from a numeric index.
	 * @param index The numeric index
	 * @return The typesafe MOVESDatabaseType.
	**/
	static public MOVESDatabaseType getByIndex(int index) {
		if(CompilationFlags.USE_NONROAD || index != 3) {
			return allTypes[index];
		}
		return null;
	}
}
