/**************************************************************************************************
 * @(#)GenericCounty.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

/**
 * Details for a user-supplied county.
 * 
 * @author		Wes Faler
 * @version		2009-08-03
**/
public class GenericCounty {
	/** 3 digit county identifier within the state **/
	public int shortCountyID = 1;
	/** 2 digit state identifier **/
	public int stateID = 99;
	/** 0-50 character description of the county **/
	public String description = "";
	/** Fraction of county in fuel Geographic Phase-in Area **/
	public float gpaFraction = 0;
	/** CountyYear.refuelingVaporProgramAdjust value **/
	public float refuelingVaporProgramAdjust = 0;
	/** CountyYear.refuelingSpillProgramAdjust value **/
	public float refuelingSpillProgramAdjust = 0;
	/** Barometric pressure in the county **/
	public float barometricPressure = 0;

	/**
	 * Obtain the full county ID by combining the state ID and the short county ID.
	 * @return the full county ID
	**/
	public int getCountyID() {
		return stateID*1000 + shortCountyID;
	}

	/**
	 * Convert a long countyID, such as 26161, into the short county ID and
	 * state ID (161 and 26 respecitvely).
	 * @param countyID full countyID
	**/
	public void splitCountyID(int countyID) {
		shortCountyID = countyID % 1000;
		stateID = countyID / 1000;
	}

	/**
	 * Test the member variables for validity.
	 * @return true if the member variables are within valid ranges
	**/
	public boolean isValid() {
		return shortCountyID >= 1 && shortCountyID <= 999
				&& stateID >= 1 && stateID <= 99
				&& gpaFraction >= 0 && gpaFraction <= 1.0;
	}

	/**
	 * Test the barometric pressure for altitude.  The standard pressure for 4,000ft in the US 
	 * is 25.8403 inHg.  High altitude is > 4,000ft, so high altitude is < 25.8403 inHg.
	 * Low altitude is <= 4,000ft.  If a pressure is not supplied (i.e. is <= 0 inHg), the
	 * pressure is adjusted to be the average for low altitude counties in the US (28.94 inHg).
	 * @return true if the pressure denotes a high altitude county.
	**/
	public boolean isHighAltitude() {
		if(barometricPressure <= 0) {
			barometricPressure = (float)28.94; // average for low altitude counties
		}
		// The standard pressure for 4,000ft in the US is 25.8403 inHg.
		// High altitude is > 4,000ft, so high altitude is < 25.8403 inHg
		// Low altitude is <= 4,000ft.
		return barometricPressure < 25.8403;
	}

	/**
	 * Force the barometric pressure value based on an altitude selection.  This is done
	 * for consistency with legacy runspecs that only have altitude not pressure.
	 * @param isHighAltitude true if the county's activity-weighted elevation is > 4,000ft.
	**/
	public void setPressureFromAltitude(boolean isHighAltitude) {
		if(isHighAltitude) {
			barometricPressure = (float)24.59;
		} else {
			barometricPressure = (float)28.94;
		}
	}

	/**
	 * Copy from another GenericCounty
	 * @param other another GenericCounty
	**/
	public void copyFrom(GenericCounty other) {
		shortCountyID = other.shortCountyID;
		stateID = other.stateID;
		description = other.description;
		gpaFraction = other.gpaFraction;
		refuelingVaporProgramAdjust = other.refuelingVaporProgramAdjust;
		refuelingSpillProgramAdjust = other.refuelingSpillProgramAdjust;
		barometricPressure = other.barometricPressure;
	}

	/**
	 * Clone an object, if it exists.
	 * @param reference an object to be cloned, may be null.
	 * @return a clone of the object or null if the object didn't exist
	**/
	public static GenericCounty clone(GenericCounty reference) {
		if(reference == null) {
			return null;
		}
		GenericCounty result = new GenericCounty();
		result.copyFrom(reference);
		return result;
	}
}
