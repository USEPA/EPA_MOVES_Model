/**************************************************************************************************
 * @(#)Converter2014Action.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Convert MOVES2010B County or Project input to MOVES2014" menu option.
 *
 * @author		Wesley Faler
 * @version		2014-07-15
**/
public class Converter2014Action extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"converter2014";
	/** Constant action name. **/
	static final String NAME =
			"Convert MOVES2010B County or Project input to MOVES2014";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Start the Database Converter GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Use scripts to convert 2010B CDM and PDM databases";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'B';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public Converter2014Action() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
