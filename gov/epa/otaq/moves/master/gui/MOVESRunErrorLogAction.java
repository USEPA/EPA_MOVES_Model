/**************************************************************************************************
 * @(#)MOVESRunErrorLogAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVESRunErrorLog menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class MOVESRunErrorLogAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_THIS_ACTION = "movesrunerrorlog-command";
	/** Constant action name. **/
	static final String NAME_THIS_ACTION = "MOVES Run Error Log...";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_THIS_ACTION = "toolbarButtonGraphics/general/About16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_THIS_ACTION = "toolbarButtonGraphics/general/About24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_THIS_ACTION = "MOVES Run Error Log";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_THIS_ACTION =
			"View the run error log for various MOVES runs";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_THIS_ACTION = 'R';

	/**
	 * Implements the MOVESRunErrorLog menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public MOVESRunErrorLogAction() {
		putValue(Action.NAME, NAME_THIS_ACTION);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_THIS_ACTION));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_THIS_ACTION));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_THIS_ACTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_THIS_ACTION);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_THIS_ACTION));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_THIS_ACTION);
	}
}
