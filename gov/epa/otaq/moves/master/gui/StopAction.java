/**************************************************************************************************
 * @(#)StopAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Stop menu option.
 *
 * @author		Cimulus
 * @version		2003-08-21
**/
public class StopAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_STOP = "stop-command";
	/** Constant action name. **/
	static final String NAME_STOP = "Stop";
	/** Constant path and file name for small stop action icon. **/
	static final String SMALL_ICON_STOP = "toolbarButtonGraphics/media/Stop16.gif";
	/** Constant path and file name for large stop action icon. **/
	static final String LARGE_ICON_STOP = "toolbarButtonGraphics/media/Stop24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_STOP = "Stop";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_STOP = "Stop execution of active RunSpec";
//	static final int MNEMONIC_KEY_STOP = 'A';

	/**
	 * Implements the Stop menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public StopAction() {
		putValue(Action.NAME, NAME_STOP);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_STOP));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_STOP));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_STOP);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_STOP);
//		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_STOP));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_STOP);
	}
}
