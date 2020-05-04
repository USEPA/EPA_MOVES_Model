/**************************************************************************************************
 * @(#)NewAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES New menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class NewAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_NEW = "new-command";
	/** Constant action name. **/
	static final String NAME_NEW = "New...";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_NEW = "toolbarButtonGraphics/general/New16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_NEW = "toolbarButtonGraphics/general/New24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_NEW = "New";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_NEW = "Create new RunSpec";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_NEW = 'N';

	/**
	 * Implements the New menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public NewAction() {
		putValue(Action.NAME, NAME_NEW);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_NEW));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_NEW));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_NEW);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_NEW);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_NEW));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_NEW);
	}
}
