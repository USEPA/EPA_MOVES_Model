/**************************************************************************************************
 * @(#)MasterLoopableSubscription.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.lang.Comparable;
import gov.epa.otaq.moves.master.framework.*;

/**
 * A subscription of a MasterLoopable object to a MasterLoop. A given MasterLoopable may be 
 * subscribed to the same MasterLoop multiple times with different parameters.
 * 
 * @author		Cimulus
 * @version		2003-02-03
**/

public class MasterLoopableSubscription implements Comparable {
	/**
	 * This compares two MasterLoopableSubscription instances. This will order objects by:
	 * 1) granularity (coarse to fine)
	 * 2) priority (high to low)
	 * 3) loopable (arbitrary consistent ordering)
	 * 
	 * @param o The other MasterLoopableSubscription object to compare to.
	**/
	public int compareTo(Object o) {
		MasterLoopableSubscription other = (MasterLoopableSubscription)o;

		if (granularity != other.granularity) {
			return granularity.compareTo(other.granularity);
		}

		if (priority != other.priority) {
			return other.priority - priority;
		}

		if (loopable != other.loopable) {
			return loopable.hashCode() - other.loopable.hashCode();
		}

		return 0;
	}
	
	/** The granularity that the MasterLoopable should be invoked at **/
	public MasterLoopGranularity granularity;
	/**
	 * The priority that the MasterLoopable should be invoked at relative to other
	 * MasterLoopable's at the same granularity
	**/
	public int priority;
	/** The subscribing MasterLoopable. **/
	public MasterLoopable loopable;
}
