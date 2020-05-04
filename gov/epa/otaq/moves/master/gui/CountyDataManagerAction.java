/**************************************************************************************************
 * @(#)CountyDataManagerAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "County Data Manager" menu option.
 *
 * @author		wgfaler
 * @version		2008-09-23
**/
public class CountyDataManagerAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY = 
			"county-data-manager";
	/** Constant action name. **/
	static final String NAME = 
			"County Data Manager";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION = 
			"Start County Data Manager GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION = 
			"Select and Import County-Level Data";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'C';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public CountyDataManagerAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
