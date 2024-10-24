/**************************************************************************************************
 * @(#)OnRoadVehicleSelection.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

/**
 * This class contains a user selected fuel ID/category/MOVES Type combination that describe
 * a set of on road vehicles.
 *
 * @author		Wesley Faler
 * @version		2011-09-10
**/
public class OnRoadVehicleSelection implements Comparable<OnRoadVehicleSelection> {
	/** Identifies a fuel. **/
	public int fuelTypeID;
	/** Fuel description from database, used for toString() purposes **/
	public String fuelTypeDesc = "";

	/** Identifies a MOVES Source Use Type. **/
	public int sourceTypeID;
	/** Source Use Type name from database, used for toString() purposes **/
	public String sourceTypeName = "";

	/** Default constructor **/
	public OnRoadVehicleSelection() {
	}

	/**
	 * Checks if this instance has valid settings.
	 * @return True if valid.
	**/
	public boolean isValid() {
		// Currently, this just ensures that all ID members are non-null and non-empty.
		// Could also add database validation if necessary.
		return (fuelTypeID != 0 && sourceTypeID != 0);
	}

	/**
	 * Comparison routine used to sort these objects.
	 * Compares Source Type IDs first then then Fuel Type IDs, ignoring case.
	 * @param other another OnRoadVehicleSelection to compare to
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(OnRoadVehicleSelection other) {
		if(other == null) {
			return +1;
		}
        // first compare source types
        if(sourceTypeID > other.sourceTypeID) {
            return +1;
        }
        if(sourceTypeID < other.sourceTypeID) {
            return -1;
        }
        // if the source type are the same, compare the fuel types
        if(fuelTypeID > other.fuelTypeID) {
            return +1;
        }
        if(fuelTypeID < other.fuelTypeID) {
            return -1;
        }
        // if we get here, it is the same
		return 0;
	}

	/**
	* Returns whether two objects are equal.
	* @param other Another OnRoadVehicleSelection object.
	* @return true if the objects are equal, false if they are not.
	**/
	public boolean equals(OnRoadVehicleSelection other) {
		return compareTo(other)==0;
	}

	/**
	 * Returns a String representation of this object.
	 * @return The String representation of this object.
	**/
	public String toString() {
		if(isValid()) {
			return sourceTypeName + " - " + fuelTypeDesc;
		} else {
			return "(invalid entry)";
		}
	}
}
