/**************************************************************************************************
 * @(#)PasteAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Paste menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class PasteAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_PASTE = "paste-command";
	/** Constant action name. **/
	static final String NAME_PASTE = "Paste";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_PASTE = "toolbarButtonGraphics/general/Paste16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_PASTE = "toolbarButtonGraphics/general/Paste24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_PASTE = "Paste";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_PASTE = 
			"Paste clipboard contents into active control";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_PASTE = 'P';

	/**
	 * Implements the Paste menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public PasteAction() {
		putValue(Action.NAME, NAME_PASTE);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_PASTE));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_PASTE));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_PASTE);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_PASTE);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_PASTE));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_PASTE);
	}
}
