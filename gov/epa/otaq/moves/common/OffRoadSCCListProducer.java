/**************************************************************************************************
 * @(#)OffRoadSCCListProducer.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.TreeSet;
import gov.epa.otaq.moves.common.OffRoadVehicleSelection;

/**
 * This utility class takes an OffRoadVehicleSelection object, obtains the relevant SCC codes,
 * and adds them to a specified set.
 *
 * @author		Cimulus
 * @version		2003-01-24
**/
public class OffRoadSCCListProducer {
	/**
	 * Given an OffRoadVehicleSelection, this obtains the relevant SCC codes and adds them
	 * to the target collection. The target collection is currently a set which avoids any
	 * duplicates.
	 * @param targetSCCs SCC objects.
	 * @param offRoadVehicleSelection OffRoadVehicleSelection.
	**/
	public static void appendSCCCodes(TreeSet targetSCCs,
			OffRoadVehicleSelection offRoadVehicleSelection) {
	}
}
