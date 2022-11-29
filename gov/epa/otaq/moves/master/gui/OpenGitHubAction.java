/**************************************************************************************************
 * @(#)ConverterAction.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for Open GitHub menu option.
**/
public class OpenGitHubAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"openGitHub";
	/** Constant action name. **/
	static final String NAME =
			"Open MOVES GitHub";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Open MOVES GitHub";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Open MOVES GitHub";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'G';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public OpenGitHubAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
