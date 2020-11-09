/**************************************************************************************************
 * @(#)CutAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Cut menu option.
 *
 * @author		Cimulus
 * @version		2003-03-06
**/
public class CutAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_CUT = "cut-command";
	/** Constant action name. **/
	static final String NAME_CUT = "Cut";
	/** Constant small icon file and path. **/
	static final String SMALL_ICON_CUT = "toolbarButtonGraphics/general/Cut16.gif";
	/** Constant large icon file and path. **/
	static final String LARGE_ICON_CUT = "toolbarButtonGraphics/general/Cut24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_CUT = "Cut";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_CUT = 
			"Cut(remove) text and place on clipboard";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_CUT = 'T';

	/**
	 * Implements the Cut menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public CutAction() {
		putValue(Action.NAME, NAME_CUT);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_CUT));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_CUT));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_CUT);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_CUT);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_CUT));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_CUT);
	}
}
