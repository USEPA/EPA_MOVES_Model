/**************************************************************************************************
 * @(#)PDSpecGUIAction.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Process DONE Files" menu option.
 *
 * @author	W. Aikman
 * @author	Wesley Faler
 * @version 2009-12-14
**/
public class PDSpecGUIAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_IM_GUI =
			"display-edit-pdspecs";
	/** Constant action name. **/
	static final String NAME_IM_GUI =
			"Process DONE Files";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_IM_GUI =
			"Process DONE Files";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_IM_GUI =
			"Select and optionally process DONE files from other masters";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_IM_GUI = 'P';

	/**
	 * Implements the IM GUI menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public PDSpecGUIAction() {
		putValue(Action.NAME, NAME_IM_GUI);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_IM_GUI);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_IM_GUI);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_IM_GUI));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_IM_GUI);
	}
}
