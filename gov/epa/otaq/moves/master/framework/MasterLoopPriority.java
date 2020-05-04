/**************************************************************************************************
 * @(#)MasterLoopPriority.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * The priority levels that a MasterLoopable can subscribe to. The priority level partially 
 * controls the sequence in which MasterLoopables are executed. Higher priority MasterLoopables are
 * completed before lower priority MasterLoopables <em>within the same loop granularity</em> are
 * started. The final sequence of execution is dependent on the MasterLoopables loop granularity.
 * MasterLoopables that subscribe at higher granularities are executed before MasterLoopables that
 * subscribe at lower granularities, regardless of their execution priority.
 *
 * @author		Wesley Faler
 * @version		2013-11-29
**/
public class MasterLoopPriority {
	/** The default execution priority for InternalControlStrategy objects. **/
	public static final int INTERNAL_CONTROL_STRATEGY = 1000;
	
	/** The default execution priority for Generator objects. **/
	public static final int GENERATOR = 100;
	
	/** The default execution priority for EmissionCalculator objects. **/
	public static final int EMISSION_CALCULATOR = 10;

	/**
	 * Convert a priority number into human-readable text.
	 * @param priority priority number to be decoded.
	 * @return textual representation of the priority.
	**/
	public static String decode(int priority) {
		switch(priority) {
			case INTERNAL_CONTROL_STRATEGY:
				return "INTERNAL_CONTROL_STRATEGY";
			case GENERATOR:
				return "GENERATOR";
			case EMISSION_CALCULATOR:
				return "EMISSION_CALCULATOR";
		}
		if(priority >= INTERNAL_CONTROL_STRATEGY - 500 && priority < INTERNAL_CONTROL_STRATEGY) {
			return "INTERNAL_CONTROL_STRATEGY-" + (INTERNAL_CONTROL_STRATEGY-priority);
		}
		if(priority <= INTERNAL_CONTROL_STRATEGY + 500 && priority > INTERNAL_CONTROL_STRATEGY) {
			return "INTERNAL_CONTROL_STRATEGY+" + (priority-INTERNAL_CONTROL_STRATEGY);
		}

		if(priority >= GENERATOR - 50 && priority < GENERATOR) {
			return "GENERATOR-" + (GENERATOR-priority);
		}
		if(priority <= GENERATOR + 50 && priority > GENERATOR) {
			return "GENERATOR+" + (priority-GENERATOR);
		}

		if(priority >= EMISSION_CALCULATOR - 5 && priority < EMISSION_CALCULATOR) {
			return "EMISSION_CALCULATOR-" + (EMISSION_CALCULATOR-priority);
		}
		if(priority <= EMISSION_CALCULATOR + 5 && priority > EMISSION_CALCULATOR) {
			return "EMISSION_CALCULATOR+" + (priority-EMISSION_CALCULATOR);
		}

		return "" + priority;
	}
}
