/**************************************************************************************************
 * @(#)OpenWebsiteAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Open Website menu option.
 *
 * @author		Wesley Faler
 * @version		2014-07-27
**/
public class OpenWebsiteAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_USER_GUIDE = "open-website-command";
	/** Constant action name. **/
	static final String NAME_USER_GUIDE = "MOVES Website and Documents";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_USER_GUIDE = 
			"toolbarButtonGraphics/general/Help16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_USER_GUIDE = 
			"toolbarButtonGraphics/general/Help24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_USER_GUIDE = "MOVES Website and Documents";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_USER_GUIDE = 
			"View MOVES web page for help.";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'W';

	/**
	 * Implements the UserGuide menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public OpenWebsiteAction() {
		putValue(Action.NAME, NAME_USER_GUIDE);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_USER_GUIDE));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_USER_GUIDE));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_USER_GUIDE);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_USER_GUIDE);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_USER_GUIDE);
	}
}
