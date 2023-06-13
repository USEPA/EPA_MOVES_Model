/**************************************************************************************************
 * @(#)ProfileWeightScriptAction.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Run Speciation Profile Weighting Script" menu option.
**/
public class ProfileWeightScriptAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"profileWeightScript";
	/** Constant action name. **/
	static final String NAME =
			"Speciation Profile Scripts";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Run a speciation profile weighting script";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Run a speciation profile weighting script";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'S';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public ProfileWeightScriptAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
