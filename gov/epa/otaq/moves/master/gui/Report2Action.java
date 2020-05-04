/**************************************************************************************************
 * @(#)Report2Action.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Report2 menu option.
 *
 * @author		Cimulus
 * @version		2003-09-11
**/
public class Report2Action extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_REPORT2 = "report2-command";
	/** Constant action name. **/
	static final String NAME_REPORT2 = "Report 2...";
// CIM: NOT DONE
//	static final String SMALL_ICON_REPORT2 = 
//			"toolbarButtonGraphics/general/Report216.gif";
//	static final String LARGE_ICON_REPORT2 = 
//			"toolbarButtonGraphics/general/Report224.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_REPORT2 = "Report2 MOVES";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_REPORT2 = "Generate MOVES Report 2";
//	static final int MNEMONIC_KEY_REPORT2 = 'A';

	/**
	 * Implements the Report2 menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public Report2Action() {
		putValue(Action.NAME, NAME_REPORT2);
//		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_REPORT2));
//		putValue(LARGE_ICON, getIcon(LARGE_ICON_REPORT2));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_REPORT2);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_REPORT2);
//		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_REPORT2));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_REPORT2);
	}
}
