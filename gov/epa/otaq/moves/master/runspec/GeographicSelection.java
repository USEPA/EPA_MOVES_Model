/**************************************************************************************************
 * @(#)GeographicSelection.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.runspec;

import gov.epa.otaq.moves.common.StringUtilities;

/**
 * Tied to the database zone/link tables.
 *
 * @author		Wesley Faler
 * @version		2011-10-16
**/
public class GeographicSelection implements Comparable {
	/**
	 * Primary Key value used to indicate the exact database record that this object relates to.
	**/
	public int databaseKey;
	/**
	 * Specifies the type of geography this object represents. This must be one of the static
	 * final constants defined within this class. Each constant corresponds to a database table.
	**/
	public GeographicSelectionType type = null;
	/** The description of geographic selection. **/
	public String textDescription = null;

	/**
	 * Comparison routine used to sort these objects for display purposes.
	 * Compares states first then counties.
	 * @param	other another GeographicSelection to compare to
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		GeographicSelection otherSelection = (GeographicSelection)other;

		int result = 0;
		if(type == null) {
			if(otherSelection.type == null) {
				result = 0;
			} else {
				return -1;
			}
		} else if(otherSelection.type == null) {
			return +1;
		} else {
			result = type.compareTo(otherSelection.type);
		}
		if(result == 0) {
			result = StringUtilities.compare(textDescription,otherSelection.textDescription);
		}
		if(result == 0) {
			result = databaseKey - otherSelection.databaseKey;
		}
		return result;
	}

	/**
	 * Comparison routine to test for equality.
	 * @param	other another GeographicSelection to compare to
	 * @return true if compareTo returns 0
	**/
	public boolean equals(Object other) {
		if(other instanceof GeographicSelection) {
			if(0 == compareTo(other)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * This method is implemented to provide support for various GUI controls.
	 * Standard function to get a textual representation of an object.
	 * @return Textual description of the selection.
	**/
	public String toString() {
		return textDescription;
	}
}
