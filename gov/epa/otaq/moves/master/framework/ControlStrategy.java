/**************************************************************************************************
 * @(#)ControlStrategy.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * The base class for all Control Strategies. Control Strategies perform work on the input to a
 * simulation run. They often model some effect on the inputs such as a decrease or increase in  
 * population over time. "External" Control Strategies occur just outside the MOVES application.
 * They are independent applications that the user invokes to bring data from foreign sources into
 * the MOVES Input Database Schema. Another type of Control Strategy, Input Control Strategies, are
 * invoked as input data is first being copied from the MOVES input databases to the execution 
 * databases. These strategies allow for more "intelligent" transfer of data than the simple 
 * criteria-filtered copying performed by the Input Data Manager. A third location where Control
 * Strategies can be used is at the start of each execution of the Master Loop. These strategies,
 * referred to as Internal Control Strategies, can make input changes that are sensitive to the
 * loop's context.
 *
 * @author		Cimulus
 * @version		2004-04-21
**/
public abstract class ControlStrategy implements MasterLoopable {
	/**
	 * Default constructor
	**/
	public ControlStrategy() {
	}
	
	
}
