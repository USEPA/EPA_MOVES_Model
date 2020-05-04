/**************************************************************************************************
 * @(#)InternalControlStrategyUseImporterOnly.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * This interface is implemented by InternalControlStrategy objects that wish to use
 * the data importers for activation rather than a traditional navigation panel.  Such
 * InternalControlStrategy objects can never be part of a RunSpec as they are created
 * dynamically at the start of a simulation based on presence of their table data.
 *
 * @author		Wesley Faler
 * @version		2011-08-06
**/
public interface InternalControlStrategyUseImporterOnly {
	/** There are no methods required for this interface, its mere presense is sufficient **/
}
