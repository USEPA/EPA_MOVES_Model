/**************************************************************************************************
 * @(#)DataImporterAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Data Importer" menu option.
 *
 * @author		wgfaler
 * @version		2008-09-23
**/
public class DataImporterAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY = 
			"data-importer";
	/** Constant action name. **/
	static final String NAME = 
			"Data Importer";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION = 
			"Start Data Importer GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION = 
			"Select and Import Data";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'I';

	/**
	 * Implements the Data Importer menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public DataImporterAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
