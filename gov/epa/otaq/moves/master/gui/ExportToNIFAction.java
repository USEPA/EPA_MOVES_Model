/**************************************************************************************************
 * @(#)ExportToNIFAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES ExportToNIF menu option.
 *
 * @author		Cimulus
 * @version		2003-09-11
**/
public class ExportToNIFAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_EXPORT_TO_NIF = "export-to-nif-command";
	/** Constant action name. **/
	static final String NAME_EXPORT_TO_NIF = "Export To NIF Format...";
/** Constant small icon path and file name. **/
// CIM: NOT DONE
//	static final String SMALL_ICON_EXPORT_TO_NIF = 
//			"toolbarButtonGraphics/general/ExportToNIF16.gif";
/** Constant large icon path and file name. **/
//	static final String LARGE_ICON_EXPORT_TO_NIF = 
//			"toolbarButtonGraphics/general/ExportToNIF24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_EXPORT_TO_NIF = "Export To NIF Format";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_EXPORT_TO_NIF = 
			"Export MOVES data to NIF Format";
//	static final int MNEMONIC_KEY_EXPORT_TO_NIF = 'A';

	/**
	 * Implements the ExporttoNIF menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ExportToNIFAction() {
		putValue(Action.NAME, NAME_EXPORT_TO_NIF);
//		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_EXPORT_TO_NIF));
//		putValue(LARGE_ICON, getIcon(LARGE_ICON_EXPORT_TO_NIF));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_EXPORT_TO_NIF);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_EXPORT_TO_NIF);
//		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_EXPORT_TO_NIF));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_EXPORT_TO_NIF);
	}
}
