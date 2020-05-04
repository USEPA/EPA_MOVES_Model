/**************************************************************************************************
 * @(#)InternalControlStrategySimpleInstance.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * This interface is implemented by InternalControlStrategy objects that wish to limit the
 * number of instances of their type to exactly 1 per RunSpec and to provide a simplified
 * user interface without the object management operations required of full strategies.
 *
 * @author		Wesley Faler
 * @version		2009-10-05
**/
public interface InternalControlStrategySimpleInstance {
	/** There are no methods required for this interface, its mere presense is sufficient **/
}
