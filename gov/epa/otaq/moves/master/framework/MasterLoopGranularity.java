/**************************************************************************************************
 * @(#)MasterLoopGranularity.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.lang.Comparable;

/**
 * This class, which acts like a "typesafe enum", describes a specific iteration granularity
 * that the MasterLoop operates at. Different MasterLoopable implementations can subscribe
 * to be executed at different granularities. This class wraps a single constant value. All
 * possible constant values are defined as static constants in this class. The constant
 * values describe all levels of time and location. A single MasterLoopGranularity will
 * specify either a time granularity or a location granularity.
 * 
 * @author		Wesley Faler
 * @version		2010-02-26
**/
public class MasterLoopGranularity implements Comparable {
	/** Represents iterating every hour. This is the finest granularity to iterate over. **/
	public static final MasterLoopGranularity HOUR = 
			new MasterLoopGranularity("HOUR", 1);
	/** Represents iterating every day. **/
	public static final MasterLoopGranularity DAY = 
			new MasterLoopGranularity("DAY", 2);
	/** Represents iterating every month. **/
	public static final MasterLoopGranularity MONTH = 
			new MasterLoopGranularity("MONTH", 3);
	/** Represents iterating every year. **/
	public static final MasterLoopGranularity YEAR = 
			new MasterLoopGranularity("YEAR", 4);
	/** Represents iterating every link. **/
	public static final MasterLoopGranularity LINK = 
			new MasterLoopGranularity("LINK", 5);
	/** Represents iterating every zone. **/
	public static final MasterLoopGranularity ZONE = 
			new MasterLoopGranularity("ZONE", 6);
	/** Represents iterating every county. **/
	public static final MasterLoopGranularity COUNTY = 
			new MasterLoopGranularity("COUNTY", 7);
	/** Represents iterating every state. **/
	public static final MasterLoopGranularity STATE = 
			new MasterLoopGranularity("STATE", 8);
	/** Represents iterating every process. This is the coarsest granularity to iterate over. **/
	public static final MasterLoopGranularity PROCESS = 
			new MasterLoopGranularity("PROCESS", 9);

	/**
	 * Locate a granularity object by its description, as returned by toString()
	 * @param description an identifier for a granularity as returned by toString()
	 * @return a MasterLoopGranularity or null if not found
	**/
	public static MasterLoopGranularity find(String description) {
		MasterLoopGranularity[] toCheck = {
			HOUR, DAY, MONTH, YEAR, LINK, ZONE, COUNTY, STATE, PROCESS
		};
		for(int i=0;i<toCheck.length;i++) {
			if(description.equalsIgnoreCase(toCheck[i].description)) {
				return toCheck[i];
			}
		}
		return null;
	}

	/** 
	 * This value is a special case. This indicates that the finest granularity for the
	 * given process should be matched. This is often used by EmissionCalculators that
	 * wish to be run immediately after generators. 
	**/
	public static final MasterLoopGranularity MATCH_FINEST = 
			new MasterLoopGranularity("MATCH_FINEST", 0);
	
	/** Textual description of the type. **/
	private String description;
	
	/**
	 * A value used for granularity comparison. This value is only relevant to other
	 * granularityValue's.
	**/
	private int granularityValue;
	
	/** 
	 * Standard Constructor 
	 * @param description textual description of the type
	 * @param granularityValue The desired loop granularity
	**/
	private MasterLoopGranularity(String description, int granularityValue) {
		this.description = description;
		this.granularityValue = granularityValue;
	}
	
	/**
	 * Provides the textual description of the type.
	 * @return The textual description of the type.
	**/
	public String toString() {
		return description;
	}
	
	/**
	 * Compares two MasterLoopGranularity's. Finer granularity > coarser granularity.
	 * Coarse < Fine.  By sorting in this order (which is counter to the granularityValue
	 * assigned to each object), the MasterLoop can sort its MasterLoopSubscriptions
	 * lowest-to-highest (the Java default in TreeSet objects) with the "lowest" being
	 * the coarsest, which is what is desired.
	 * @param object MasterLoopGranularity object to compare against.
	 * @return A value > 0 if this is of finer granularity than the other object.
	 * A value < 0 if this is of coarser granularity. Exactly 0 if the two objects
	 * are the same.
	**/
	public int compareTo(Object object) {
		MasterLoopGranularity other = (MasterLoopGranularity)object;
		return other.granularityValue - granularityValue;
	}
}
