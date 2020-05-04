/**************************************************************************************************
 * @(#)SummaryReportAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES summary report menu option.
 *
 * @author		EPA - Mitch C.
 * @version		2005-10-03
**/
public class SummaryReportAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_RUN_SCRIPT = "summary report command";
	/** Constant action name. **/
	static final String NAME_RUN_SCRIPT = "Produce Summary Report";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_RUN_SCRIPT = "Summary Report";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_RUN_SCRIPT = "Produce Summary Report";
//	static final int MNEMONIC_KEY_RUN_SCRIPT = 'S';

	/**
	 * Implements the Run Script menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public SummaryReportAction() {
		putValue(Action.NAME, NAME_RUN_SCRIPT);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_RUN_SCRIPT);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_RUN_SCRIPT);
//		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_RUN_SCRIPT);
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_RUN_SCRIPT);
	}
}
