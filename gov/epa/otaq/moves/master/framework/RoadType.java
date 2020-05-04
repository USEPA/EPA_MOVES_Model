/**************************************************************************************************
 * @(#)RoadType.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.common.*;

/**
 * Represents a single road type.
 *
 * @author		Wesley Faler
 * @version		2013-02-21
**/
public class RoadType implements Comparable {
	/** Set of all active RoadType instances **/
	static TreeSet<RoadType> allRoadTypes = new TreeSet<RoadType>();

	/** ID of the RoadType **/
	public int roadTypeID;

	/** Name of the RoadType **/
	public String roadTypeName = "";

	/** Used by what models **/
	public Models.ModelCombination mc = Models.ModelCombination.M1;

	/**
	 * Gets a RoadType by name
	 * @param roadTypeName The name to search for
	 * @return Returns the associated RoadType, or null if not found
	**/
	public static RoadType findByName(String roadTypeName) {
		for(Iterator<RoadType> iterRoadType = allRoadTypes.iterator();
				iterRoadType.hasNext();) {
			RoadType roadType = (RoadType)iterRoadType.next();
			if (roadType.roadTypeName.compareToIgnoreCase(roadTypeName) == 0) {
				return roadType;
			}
		}
		return null;
	}

	/**
	 * Determines if a roadtype is in the database.
	 * @param roadTypeID The roadtype ID.
	 * @param roadTypeName The roadtype name.
	 * @return Returns true if the id and name are in the database.
	**/
	public static boolean isInDatabase(int roadTypeID, String roadTypeName) {
		boolean foundRoadType = false;
		Connection db = null;

		String sql = "SELECT COUNT(*) FROM RoadType WHERE roadTypeID = " + roadTypeID + " AND roadDesc = '"
				+ roadTypeName + "'";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
			query.open(db,sql);
			if(query.rs.next()) {
				foundRoadType = (query.rs.getInt(1) > 0);
			}
		} catch(Exception e) {
			/** @explain A database error occurred while verifying a road type's information **/
			Logger.logError(e, "Verifying if roadtype is in the database.");
		} finally {
			query.onFinally();
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,db);
			}
		}

		return foundRoadType;
	}

	/** Default constructor **/
	public RoadType() {
	}

	/**
	 * Constructor
	 * @param id Sets the roadTypeID
	 * @param name Sets the roadTypeName.
	 * @param mc onroad, nonroad, or both that uses this road type
	**/
	public RoadType(int id, String name, Models.ModelCombination mc) {
		roadTypeID = id;
		roadTypeName = name;
		this.mc = mc;
		allRoadTypes.add(this);
	}

	/**
	 * Comparable Interface Implementation. Provides Tree container ordering and display
	 * order.
	 * @param other RoadType object to compare.
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		RoadType otherRoadType = (RoadType)other;
		if(otherRoadType == null) {
			return +1;
		}
		if(otherRoadType.roadTypeID == roadTypeID) {
			return 0;
		}
		return roadTypeName.compareToIgnoreCase(((RoadType)other).roadTypeName);
	}

	/**
	 * Comparable Interface Implementation. Provides Tree container ordering and display
	 * order.
	 * @param other RoadType object to compare for equal names.
	 * @return true if they are the same, and false otherwise
	**/
	public boolean equals(Object other) {
		return compareTo((RoadType)other) == 0;
	}

	/**
	 * This method is implemented to provide support for various GUI controls.
	 * Standard function to get a textual representation of an object.
	 * @return Textual description of the selection.
	**/
	public String toString() {
		return roadTypeName;
	}
}
