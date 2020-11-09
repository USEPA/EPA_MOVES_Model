/**************************************************************************************************
 * @(#)ConverterAction.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Build LEV Input Database" menu option.
**/
public class BuildLEVAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY =
			"builderLEV";
	/** Constant action name. **/
	static final String NAME =
			"Build LEV Input Database";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION =
			"Start the Database Builder GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION =
			"Use scripts to build LEV input database";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'L';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public BuildLEVAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
