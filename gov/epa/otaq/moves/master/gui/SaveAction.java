/**************************************************************************************************
 * @(#)SaveAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Save menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class SaveAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_SAVE = "save-command";
	/** Constant action name. **/
	static final String NAME_SAVE = "Save";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_SAVE = "toolbarButtonGraphics/general/Save16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_SAVE = "toolbarButtonGraphics/general/Save24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_SAVE = "Save";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_SAVE = "Save active RunSpec";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_SAVE = 'S';

	/**
	 * Implements the Save menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public SaveAction() {
		putValue(Action.NAME, NAME_SAVE);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_SAVE));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_SAVE));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_SAVE);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_SAVE);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_SAVE));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_SAVE);
	}
}
