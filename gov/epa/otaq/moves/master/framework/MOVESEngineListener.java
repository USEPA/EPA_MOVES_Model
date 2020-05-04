/**************************************************************************************************
 * @(#)MOVESEngineListener.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

/**
 * Provides notification that MOVESEngine has changed status. This is implemented by a user
 * interface class; specifically by both a GUI class and a command line interface class.
 * 
 * @author		Cimulus
 * @version		2004-01-05
**/
public interface MOVESEngineListener {
	/**
	 * Used to send notifications about a MOVESEngines' progress to implementers.
	 * 
	 * @param inEngine The associated MOVESEngine.
	**/
	public void engineProgressUpdate(MOVESEngine inEngine);

	/**
	 * Called when the MOVESEngine object is completing
	 * 
	 * @param inEngine The associated MOVESEngine.
	**/
	public void engineIsCompleting(MOVESEngine inEngine);
}
