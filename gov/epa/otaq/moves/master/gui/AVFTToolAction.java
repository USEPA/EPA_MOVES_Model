/**************************************************************************************************
 * @(#)AVFTToolAction.java
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
public class AVFTToolAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"AVFTTool";
	/** Constant action name. **/
	static final String NAME =
			"AVFT Tool";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Run the AVFT Tool";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Run the AVFT Tool";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'A';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public AVFTToolAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
