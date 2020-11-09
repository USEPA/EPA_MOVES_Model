/**************************************************************************************************
 * @(#)PrintAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Print menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class PrintAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_PRINT = "print-command";
	/** Constant action name. **/
	static final String NAME_PRINT = "Print...";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_PRINT = "toolbarButtonGraphics/general/Print16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_PRINT = "toolbarButtonGraphics/general/Print24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_PRINT = "Print MOVES";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_PRINT = "Print active object";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_PRINT = 'P';

	/**
	 * Implements the Print menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public PrintAction() {
		putValue(Action.NAME, NAME_PRINT);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_PRINT));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_PRINT));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_PRINT);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_PRINT);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_PRINT));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_PRINT);
	}
}
