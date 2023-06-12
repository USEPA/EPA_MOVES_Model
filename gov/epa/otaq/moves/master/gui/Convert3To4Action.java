/**************************************************************************************************
 * @(#)Convert3To4Action.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Convert MOVES3 Input Database to MOVES4" menu option.
 *
 * @author		Daniel Bizer-Cox
 * @version		2022-11-16
**/
public class Convert3To4Action extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"convert3To4";
	/** Constant action name. **/
	static final String NAME =
			"Convert MOVES3 Input Database to MOVES4";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Start the Database Converter GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Convert MOVES3 input databases to MOVES4 format";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'c';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public Convert3To4Action() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
