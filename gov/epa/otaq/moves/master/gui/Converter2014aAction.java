/**************************************************************************************************
 * @(#)Converter2014aAction.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Convert MOVES2014 County or Project input to MOVES2014A" menu option.
 *
 * @author		Wesley Faler
 * @version		2015-05-20
**/
public class Converter2014aAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"converter2014a";
	/** Constant action name. **/
	static final String NAME =
			"Convert MOVES2014 County or Project input to MOVES2014A";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Start the Database Converter GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Use scripts to convert 2014 CDM and PDM databases";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = '4';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public Converter2014aAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
