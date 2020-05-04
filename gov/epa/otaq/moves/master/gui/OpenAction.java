/**************************************************************************************************
 * @(#)OpenAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Open menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class OpenAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_OPEN = "open-command";
	/** Constant action name. **/
	static final String NAME_OPEN = "Open...";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_OPEN = "toolbarButtonGraphics/general/Open16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_OPEN = "toolbarButtonGraphics/general/Open24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_OPEN = "Open";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_OPEN = "Open an existing RunSpec";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_OPEN = 'O';

	/**
	 * Implements the Open menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public OpenAction() {
		putValue(Action.NAME, NAME_OPEN);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_OPEN));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_OPEN));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_OPEN);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_OPEN);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_OPEN));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_OPEN);
	}
}
