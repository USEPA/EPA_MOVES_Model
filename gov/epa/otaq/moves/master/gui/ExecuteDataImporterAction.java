/**************************************************************************************************
 * @(#)ExecuteDataImporterAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Execute Data Importer menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class ExecuteDataImporterAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_EXECUTE_DATA_IMPORTER = 
			"execute-data-importer-command";
	/** Constant action name. **/
	static final String NAME_EXECUTE_DATA_IMPORTER = "Execute Data Importer...";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_EXECUTE_DATA_IMPORTER = "Execute Data Importer";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_EXECUTE_DATA_IMPORTER = "Execute Data Importer";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_EXECUTE_DATA_IMPORTER = 'D';

	/**
	 * Implements the Clear menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ExecuteDataImporterAction() {
		putValue(Action.NAME, NAME_EXECUTE_DATA_IMPORTER);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_EXECUTE_DATA_IMPORTER);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_EXECUTE_DATA_IMPORTER);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_EXECUTE_DATA_IMPORTER));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_EXECUTE_DATA_IMPORTER);
	}
}
