/**************************************************************************************************
 * @(#)RunNonroadScriptAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES run nonroad script menu option.
 *
 * @author		Daniel Cox
 * @version		2014-06-27
**/
public class RunNonroadScriptAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_RUN_SCRIPT = "run nonroad script command";
	/** Constant action name. **/
	static final String NAME_RUN_SCRIPT = "Run MySQL Script on Nonroad Output Database";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_RUN_SCRIPT = "Run Nonroad Script";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_RUN_SCRIPT = "Run MySQL Script on Nonroad Output Database";
//	static final int MNEMONIC_KEY_RUN_SCRIPT = 'R';

	/**
	 * Implements the Run Nonroad Script menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public RunNonroadScriptAction() {
		putValue(Action.NAME, NAME_RUN_SCRIPT);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_RUN_SCRIPT);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_RUN_SCRIPT);
//		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_RUN_SCRIPT);
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_RUN_SCRIPT);
	}
}
