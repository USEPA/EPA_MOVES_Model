/**************************************************************************************************
 * @(#)IMGUIAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "IM/Reflash Importer" menu option.
 *
 * @author	W. Aikman
 * @author	Wesley Faler
 * @version 2009-12-14
**/
public class IMGUIAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_IM_GUI = 
			"display-edit-im-program-coverage-records";
	/** Constant action name. **/
	static final String NAME_IM_GUI = 
			"I/M Importer";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_IM_GUI = 
			"Update I/M";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_IM_GUI = 
			"Select and Import I/M Program Data";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_IM_GUI = 'M';

	/**
	 * Implements the IM GUI menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public IMGUIAction() {
		putValue(Action.NAME, NAME_IM_GUI);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_IM_GUI);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_IM_GUI);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_IM_GUI));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_IM_GUI);
	}
}
