/**************************************************************************************************
 * @(#)AboutAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES About menu option.
 *
 * @author		Cimulus
 * @author		EPA-elg 
 * @author		EPA-hmc
 * @version		2006-12-27
**/
public class AboutAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_ABOUT = "about-command";
	/** Constant action name. **/
	static final String NAME_ABOUT = "About MOVES...";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_ABOUT = "toolbarButtonGraphics/general/About16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_ABOUT = "toolbarButtonGraphics/general/About24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_ABOUT = "About MOVES";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_ABOUT = "MOVES4";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_ABOUT = 'A';

	/**
	 * Implements the About menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public AboutAction() {
		putValue(Action.NAME, NAME_ABOUT);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_ABOUT));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_ABOUT));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_ABOUT);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_ABOUT);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_ABOUT));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_ABOUT);
	}
}
