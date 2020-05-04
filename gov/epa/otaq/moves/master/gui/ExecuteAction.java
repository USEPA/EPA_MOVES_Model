/**************************************************************************************************
 * @(#)ExecuteAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Execute menu option.
 *
 * @author		Cimulus
 * @version		2003-08-20
**/
public class ExecuteAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_EXECUTE = "execute-command";
	/** Constant action name. **/
	static final String NAME_EXECUTE = "Execute";
	/** Small icon graphic used for the execute action **/
	static final String SMALL_ICON_EXECUTE = 
			"toolbarButtonGraphics/media/Play16.gif";
	/** Large icon graphic used for the execute action **/
	static final String LARGE_ICON_EXECUTE = 
			"toolbarButtonGraphics/media/Play24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_EXECUTE = "Execute";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_EXECUTE = "Execute active RunSpec";
	//static final int MNEMONIC_KEY_EXECUTE = 'A';

	/**
	 * Implements the Execute menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ExecuteAction() {
		putValue(Action.NAME, NAME_EXECUTE);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_EXECUTE));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_EXECUTE));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_EXECUTE);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_EXECUTE);
//		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_EXECUTE));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_EXECUTE);
	}
}
