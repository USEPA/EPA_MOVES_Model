/**************************************************************************************************
 * @(#)Convert4To5Action.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Convert MOVES4 Input Database to MOVES5" menu option.
 *
 * @author		Daniel Bizer-Cox
 * @version		2022-11-16
**/
public class Convert4To5Action extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"convert4To5";
	/** Constant action name. **/
	static final String NAME =
			"Convert MOVES4 Input Database to MOVES5";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Start the Database Converter GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Convert MOVES4 input databases to MOVES5 format";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'v';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public Convert4To5Action() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
