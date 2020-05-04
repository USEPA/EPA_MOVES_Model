/**************************************************************************************************
 * @(#)SaveAsAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES SaveAs menu option.
 *
 * @author		Cimulus
 * @version		2003-08-21
**/
public class SaveAsAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_SAVE_AS = "save-as-command";
	/** Constant action name. **/
	static final String NAME_SAVE_AS = "Save As...";
	/** Constant path and file name for small icon. **/
	static final String SMALL_ICON_SAVE_AS = 
			"toolbarButtonGraphics/general/SaveAs16.gif";
	/** Constant path and file name for large icon. **/
	static final String LARGE_ICON_SAVE_AS = 
			"toolbarButtonGraphics/general/SaveAs24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_SAVE_AS = "Save As";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_SAVE_AS = 
			"Save active RunSpec as a different name";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_SAVE_AS = 'A';

	/**
	 * Implements the SaveAs menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public SaveAsAction() {
		putValue(Action.NAME, NAME_SAVE_AS);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_SAVE_AS));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_SAVE_AS));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_SAVE_AS);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_SAVE_AS);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_SAVE_AS));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_SAVE_AS);
	}
}
