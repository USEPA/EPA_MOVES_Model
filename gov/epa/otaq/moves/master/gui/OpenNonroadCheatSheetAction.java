/**************************************************************************************************
 * @(#)ConverterAction.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for Open Nonroad Cheatsheet menu option.
**/
public class OpenNonroadCheatSheetAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"openNonroadCheatsheet";
	/** Constant action name. **/
	static final String NAME =
			"Open Nonroad Cheat Sheet";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Open Nonroad Cheat Sheet";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Open Nonroad Cheat Sheet";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'N';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public OpenNonroadCheatSheetAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
