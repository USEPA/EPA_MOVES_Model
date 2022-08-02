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
public class ProfileWeightScriptActionNR extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"profileWeightScriptNR";
	/** Constant action name. **/
	static final String NAME =
			"Nonroad Speciation Profile Script";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Run the nonroad speciation profile weighting script";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Run the nonroad speciation profile weighting script";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'a';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public ProfileWeightScriptActionNR() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
