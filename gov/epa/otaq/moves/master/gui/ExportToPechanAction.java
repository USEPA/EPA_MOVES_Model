/**************************************************************************************************
 * @(#)ExportToPechanAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES ExportToPechan menu option.
 *
 * @author		Cimulus
 * @version		2003-09-11
**/
public class ExportToPechanAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_EXPORT_TO_PECHAN = 
			"export-to-pechan-command";
	/** Constant action name. **/
	static final String NAME_EXPORT_TO_PECHAN = "Export To Pechan Format...";
	/** Constant small icon path and file name. **/
// CIM: NOT DONE
//	static final String SMALL_ICON_EXPORT_TO_PECHAN = 
//			"toolbarButtonGraphics/general/ExportToPechan16.gif";
	/** Constant large icon path and file name. **/
//	static final String LARGE_ICON_EXPORT_TO_PECHAN = 
//			"toolbarButtonGraphics/general/ExportToPechan24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_EXPORT_TO_PECHAN = 
			"Export To Pechan Format";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_EXPORT_TO_PECHAN = 
			"Export MOVES data to Pechan Format";
//	static final int MNEMONIC_KEY_EXPORT_TO_PECHAN = 'A';

	/**
	 * Implements the ExportToPechan menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ExportToPechanAction() {
		putValue(Action.NAME, NAME_EXPORT_TO_PECHAN);
//		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_EXPORT_TO_PECHAN));
//		putValue(LARGE_ICON, getIcon(LARGE_ICON_EXPORT_TO_PECHAN));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_EXPORT_TO_PECHAN);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_EXPORT_TO_PECHAN);
//		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_EXPORT_TO_PECHAN));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_EXPORT_TO_PECHAN);
	}
}
