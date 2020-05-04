/**************************************************************************************************
 * @(#)PreviousFile4Action.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES PreviousFile4 menu option.
 *
 * @author		Cimulus
 * @version		2003-09-11
**/
public class PreviousFile4Action extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_PREVIOUS_FILE4 = "previous-file4-command";
	/** Constant action name. **/
	static final String NAME_PREVIOUS_FILE4 = "4 ";
// CIM: NOT DONE
//	static final String SMALL_ICON_PREVIOUS_FILE4 = 
//			"toolbarButtonGraphics/general/PreviousFile416.gif";
//	static final String LARGE_ICON_PREVIOUS_FILE4 = 
//			"toolbarButtonGraphics/general/PreviousFile424.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_PREVIOUS_FILE4 = "4 ";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_PREVIOUS_FILE4 = "Load File: ";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_PREVIOUS_FILE4 = '4';

	/**
	 * Implements the PreviousFile4 menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public PreviousFile4Action() {
		putValue(Action.NAME, NAME_PREVIOUS_FILE4);
//		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_PREVIOUS_FILE4));
//		putValue(LARGE_ICON, getIcon(LARGE_ICON_PREVIOUS_FILE4));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_PREVIOUS_FILE4);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_PREVIOUS_FILE4);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_PREVIOUS_FILE4));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_PREVIOUS_FILE4);
	}

	/**
	 * Sets the short description text of this action, after the default prefix.
	 * @param	newText	the new short description as text to add after the default prefix.
	**/
	public void setShortDescription(String newText) {
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_PREVIOUS_FILE4 + newText);
	}
}
