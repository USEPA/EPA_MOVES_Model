/**************************************************************************************************
 * @(#)ConverterAction.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Run ONI Tool" menu option.
**/
public class ONIToolAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"oniTool";
	/** Constant action name. **/
	static final String NAME =
			"ONI Tool";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Run the ONI Tool GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Run the ONI Tool GUI";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'O';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public ONIToolAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
