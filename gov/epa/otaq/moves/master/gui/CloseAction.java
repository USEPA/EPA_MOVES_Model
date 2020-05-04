/**************************************************************************************************
 * @(#)CloseAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Close menu option.
 *
 * @author		Cimulus
 * @version		2003-08-20
**/
public class CloseAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_CLOSE = "close-command";
	/** Constant action name. **/
	static final String NAME_CLOSE = "Close";
	/** Small icon graphic for the close button **/
	static final String SMALL_ICON_CLOSE = "gov/epa/otaq/moves/master/gui/images/Close16.gif";
	/** Large icon graphic for the close button **/
	static final String LARGE_ICON_CLOSE = "gov/epa/otaq/moves/master/gui/images/Close24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_CLOSE = "Close";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_CLOSE = "Close active RunSpec";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_CLOSE = 'C';

	/**
	 * Implements the Close menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public CloseAction() {
		putValue(Action.NAME, NAME_CLOSE);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_CLOSE));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_CLOSE));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_CLOSE);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_CLOSE);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_CLOSE));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_CLOSE);
	}
}
