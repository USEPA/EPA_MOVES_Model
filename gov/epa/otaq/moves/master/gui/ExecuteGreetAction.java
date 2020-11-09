/**************************************************************************************************
 * @(#)ExecuteGreetAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Execute Greet menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class ExecuteGreetAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_EXECUTE_GREET = "execute-greet-command";
	/** Constant action name. **/
	static final String NAME_EXECUTE_GREET = "Execute Greet...";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_EXECUTE_GREET = "Execute Greet";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_EXECUTE_GREET = "Execute Greet";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_EXECUTE_GREET = 'G';

	/**
	 * Implements the Clear menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ExecuteGreetAction() {
		putValue(Action.NAME, NAME_EXECUTE_GREET);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_EXECUTE_GREET);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_EXECUTE_GREET);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_EXECUTE_GREET));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_EXECUTE_GREET);
	}
}
