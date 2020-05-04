/**************************************************************************************************
 * @(#)ClearAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Clear menu option.
 *
 * @author		Cimulus
 * @version		2003-08-20
**/
public class ClearAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_CLEAR = "clear-command";
	/** Constant action name. **/
	static final String NAME_CLEAR = "Clear";
	/** Small Icon graphic for the delete button **/
	static final String SMALL_ICON_CLEAR = "toolbarButtonGraphics/general/Delete16.gif";
	/** Large Icon graphic for the delete button **/
	static final String LARGE_ICON_CLEAR = "toolbarButtonGraphics/general/Delete24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_CLEAR = "Clear";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_CLEAR = "Clear data in current RunSpec";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_CLEAR = 'A';

	/**
	 * Implements the Clear menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ClearAction() {
		putValue(Action.NAME, NAME_CLEAR);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_CLEAR));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_CLEAR));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_CLEAR);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_CLEAR);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_CLEAR));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_CLEAR);
	}
}
