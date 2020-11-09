/**************************************************************************************************
 * @(#)PauseAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Pause menu option.
 *
 * @author		Cimulus
 * @version		2003-08-21
**/
public class PauseAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_PAUSE = "pause-command";
	/** Constant action name. **/
	static final String NAME_PAUSE = "Pause";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_PAUSE = "toolbarButtonGraphics/media/Pause16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_PAUSE = "toolbarButtonGraphics/media/Pause24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_PAUSE = "Pause";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_PAUSE = "Pause execution of active RunSpec";
//	static final int MNEMONIC_KEY_PAUSE = 'A';

	/**
	 * Implements the Pause menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public PauseAction() {
		putValue(Action.NAME, NAME_PAUSE);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_PAUSE));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_PAUSE));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_PAUSE);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_PAUSE);
//		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_PAUSE));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_PAUSE);
	}
}
