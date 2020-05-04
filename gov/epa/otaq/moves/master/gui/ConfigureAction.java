/**************************************************************************************************
 * @(#)ConfigureAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Configuration menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class ConfigureAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_CONFIGURE = "configure-command";
	/** Constant action name. **/
	static final String NAME_CONFIGURE = "Configure MOVES...";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_CONFIGURE = "toolbarButtonGraphics/general/Preferences16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_CONFIGURE = "toolbarButtonGraphics/general/Preferences24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_CONFIGURE = "Configure MOVES";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_CONFIGURE = "Adjust MOVES configuration settings...";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_CONFIGURE = 'G';

	/**
	 * Implements the Configure menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ConfigureAction() {
		putValue(Action.NAME, NAME_CONFIGURE);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_CONFIGURE));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_CONFIGURE));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_CONFIGURE);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_CONFIGURE);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_CONFIGURE));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_CONFIGURE);
	}
}
