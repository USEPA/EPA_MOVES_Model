/**************************************************************************************************
 * @(#)OffRoadVehicleSelection.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

/**
 * This class contains a single user selected fuel ID/sector ID combination that describes
 * a set of off road SCCs.
 *
 * @author		Wesley Faler
 * @version		2011-09-10
**/
public class OffRoadVehicleSelection implements Comparable<OffRoadVehicleSelection> {
	/** Identifies a fuel. **/
	public int fuelTypeID;
	/** Fuel description from database, used for toString() purposes **/
	public String fuelTypeDesc = "";

	/** Identifies a sector. **/
	public int sectorID;
	/** sector name from database, used for toString() purposes **/
	public String sectorName = "";

	/** Default constructor **/
	public OffRoadVehicleSelection() {
	}

	/**
	 * Checks if this instance has valid settings.
	 * @return True if valid.
	**/
	public boolean isValid() {
		// Currently, this just ensures that all ID members are non-null and non-empty.
		// Could also add database validation if necessary.
		return (fuelTypeID != 0 && sectorID != 0);
	}

	/**
	 * Comparison routine used to sort these objects.
	 * Compares Sector IDs first then Fuel IDs, ignoring case.
	 * @param other another OffRoadVehicleSelection to compare to
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(OffRoadVehicleSelection other) {
		if(other==null) {
			return +1;
		}
		int result = sectorName.compareToIgnoreCase(other.sectorName);
		if(result == 0) {
			result = fuelTypeDesc.compareToIgnoreCase(other.fuelTypeDesc);
		}
		return result;
	}

	/**
	* Returns whether two objects are equal.
	* @param other Another OffRoadVehicleSelection object.
	* @return true if the objects are equal, false if they are not.
	**/
	public boolean equals(OffRoadVehicleSelection other) {
		return compareTo(other)==0;
	}

	/**
	 * Returns a String representation of this object.
	 * @return The String representation of this object.
	**/
	public String toString() {
		if(isValid()) {
			return sectorName + " - " + fuelTypeDesc;
		} else {
			return "(invalid entry)";
		}
	}
}
