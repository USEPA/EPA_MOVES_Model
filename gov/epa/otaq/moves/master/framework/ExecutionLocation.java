/**************************************************************************************************
 * @(#)ExecutionLocation.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * One of the locations for which the Master Loop is executed. Each Geographic Selection in the
 * RunSpec translates to many Link "selections" that the Master Loop is executed for. This class
 * identifies one of these selections. It maps directly to a Link record in the database and is
 * created by the ExecutionLocationProducer just prior to executing a simulation run.
 *
 * @author		Cimulus
 * @version		2004-01-14
**/
public class ExecutionLocation implements Comparable {
	/** The primary key value used to identify the represented road type record in the database **/
	public int roadTypeRecordID;

	/** The primary key value used to identify the represented Link record in the database. **/
	public int linkRecordID;

	/**
	 * The primary key value used to identify the Zone record in the database that owns
	 * the link.
	**/
	public int zoneRecordID;

	/**
	 * The primary key value used to identify the County record in the database that owns
	 * the Zone.
	**/
	public int countyRecordID;

	/**
	 * The primary key value used to identify the State record in the database that owns
	 * the County.
	**/
	public int stateRecordID;

	/**
	 * Default constructor
	**/
	public ExecutionLocation() {
	}

	/**
	 * Returns a String representation of this object.  Added for debugging.
	 * @return The String representation.
	**/
	public String toString() {
		return roadTypeRecordID+" - "+linkRecordID+" - "+zoneRecordID
				+" - "+countyRecordID+" - "+stateRecordID;
	}

	/**
	 * This compares two ExecutionLocation objects. This provides a consistent ordering with
	 * the following sort priority:
	 *	1) State
	 *	2) County
	 *	3) Zone
	 *	4) Link
	 *	5) Road Type
	 *
	 * At each level the ordering is arbitrary but consistent.
	 *
	 * @param object The Object to be compared.
	 * @return A signed integer where the sign (-, +, 0) indicates the relation between the objects.
	**/
	public int compareTo(Object object) {
		ExecutionLocation other = (ExecutionLocation)object;
		if(stateRecordID != other.stateRecordID) {
			return stateRecordID - other.stateRecordID;
		}
		if(countyRecordID != other.countyRecordID) {
			return countyRecordID - other.countyRecordID;
		}
		if(zoneRecordID != other.zoneRecordID) {
			return zoneRecordID - other.zoneRecordID;
		}
		if(linkRecordID != other.linkRecordID) {
			return linkRecordID - other.linkRecordID;
		}
		return roadTypeRecordID - other.roadTypeRecordID;
	}
}
