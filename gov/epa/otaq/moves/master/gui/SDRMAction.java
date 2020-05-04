/**************************************************************************************************
 * @(#)SDRMAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Software Design/Reference Manual menu option.
 *
 * @author		Cimulus
 * @version		2007-01-01
**/
public class SDRMAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_SDRM = "sdrm-command";
	/** Constant action name. **/
	static final String NAME_SDRM = "MOVES Software Design/Reference Manual";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_SDRM = 
			"toolbarButtonGraphics/general/Help16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_SDRM = 
			"toolbarButtonGraphics/general/Help24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_SDRM = "MOVES Software Design/Reference Manual";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_SDRM = 
			"View MOVES Software Design/Reference Manual for help.";

	/**
	 * Implements the SDRM menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public SDRMAction() {
		putValue(Action.NAME, NAME_SDRM);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_SDRM));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_SDRM));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_SDRM);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_SDRM);
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_SDRM);
	}
}
