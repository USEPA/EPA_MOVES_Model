/**************************************************************************************************
 * @(#)Report1Action.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Report1 menu option.
 *
 * @author		Cimulus
 * @version		2003-09-11
**/
public class Report1Action extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_REPORT1 = "report1-command";
	/** Constant action name. **/
	static final String NAME_REPORT1 = "Report 1...";
// CIM: NOT DONE
//	static final String SMALL_ICON_REPORT1 = 
//			"toolbarButtonGraphics/general/Report116.gif";
//	static final String LARGE_ICON_REPORT1 = 
//			"toolbarButtonGraphics/general/Report124.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_REPORT1 = "Report 1";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_REPORT1 = "Generate MOVES Report 1";
//	static final int MNEMONIC_KEY_REPORT1 = 'A';

	/**
	 * Implements the Report1 menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public Report1Action() {
		putValue(Action.NAME, NAME_REPORT1);
//		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_REPORT1));
//		putValue(LARGE_ICON, getIcon(LARGE_ICON_REPORT1));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_REPORT1);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_REPORT1);
//		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_REPORT1));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_REPORT1);
	}
}
