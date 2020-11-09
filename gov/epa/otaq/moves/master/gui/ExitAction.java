/**************************************************************************************************
 * @(#)ExitAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Exit menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class ExitAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_EXIT = "exit-command";
	/** Constant action name. **/
	static final String NAME_EXIT = "Exit";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_EXIT = "toolbarButtonGraphics/general/Stop16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_EXIT = "toolbarButtonGraphics/general/Stop24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_EXIT = "Exit MOVES";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_EXIT = "Exit MOVES software";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_EXIT = 'X';

	/**
	 * Implements the Exit menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ExitAction() {
		putValue(Action.NAME, NAME_EXIT);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_EXIT));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_EXIT));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_EXIT);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_EXIT);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_EXIT));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_EXIT);
	}
}
