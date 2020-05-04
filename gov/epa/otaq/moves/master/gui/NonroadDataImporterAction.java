/**************************************************************************************************
 * @(#)NonroadDataImporterAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Nonroad Data Importer" menu option.
 *
 * @author		Wesley Faler
 * @version		2015-05-21
**/
public class NonroadDataImporterAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY = 
			"nonroad-data-importer";
	/** Constant action name. **/
	static final String NAME = 
			"Nonroad Data Importer";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION = 
			"Start Nonroad Data Importer GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION = 
			"Select and Nonroad Import Data";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'N';

	/**
	 * Implements the Data Importer menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public NonroadDataImporterAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
