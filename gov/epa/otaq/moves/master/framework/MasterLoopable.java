/**************************************************************************************************
 * @(#)MasterLoopable.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * This interface is implemented by objects that want to be executed during iterations of the
 * MasterLoop. This is implemented by generators, emission calculators and internal control
 * strategies.
 *
 * @author		Cimulus
 * @version		2003-09-05
**/
public interface MasterLoopable {
	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	void subscribeToMe(MasterLoop targetLoop);

	/**
	 * Called during each relevant iteration of the MasterLoop.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	void executeLoop(MasterLoopContext context) throws InterruptedException;

	/**
	 * Cleans up all the data created within the executeLoop() method.  Typically only
	 * Generator-derived classes will have any code within this method.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	void cleanDataLoop(MasterLoopContext context) throws InterruptedException;
}
