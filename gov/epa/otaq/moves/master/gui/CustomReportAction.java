/**************************************************************************************************
 * @(#)CustomReportAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES CustomReport menu option.
 *
 * @author		Cimulus
 * @version		2003-09-11
**/
public class CustomReportAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_CUSTOM_REPORT = "custom-report-command";
	/** Constant action name. **/
	static final String NAME_CUSTOM_REPORT = "Custom Report...";
// CIM:NOT DONE
//	static final String SMALL_ICON_CUSTOM_REPORT = 
//			"toolbarButtonGraphics/general/CustomReport16.gif";
//	static final String LARGE_ICON_CUSTOM_REPORT = 
//			"toolbarButtonGraphics/general/CustomReport24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_CUSTOM_REPORT = "Custom Report";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_CUSTOM_REPORT = 
			"Generate Custom MOVES Report";
//	static final int MNEMONIC_KEY_CUSTOM_REPORT = 'A';

	/**
	 * Implements the CustomReport menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public CustomReportAction() {
		putValue(Action.NAME, NAME_CUSTOM_REPORT);
//		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_CUSTOM_REPORT));
//		putValue(LARGE_ICON, getIcon(LARGE_ICON_CUSTOM_REPORT));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_CUSTOM_REPORT);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_CUSTOM_REPORT);
//		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_CUSTOM_REPORT));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_CUSTOM_REPORT);
	}
}
