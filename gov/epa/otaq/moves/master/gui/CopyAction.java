/**************************************************************************************************
 * @(#)CopyAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Copy menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class CopyAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_COPY = "copy-command";
	/** Constant action name. **/
	static final String NAME_COPY = "Copy";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_COPY = "toolbarButtonGraphics/general/Copy16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_COPY = "toolbarButtonGraphics/general/Copy24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_COPY = "Copy";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_COPY = "Copy text to clipboard";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_COPY = 'C';

	/**
	 * Implements the Copy menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public CopyAction() {
		putValue(Action.NAME, NAME_COPY);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_COPY));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_COPY));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_COPY);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_COPY);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_COPY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_COPY);
	}
}
