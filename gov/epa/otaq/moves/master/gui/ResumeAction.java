/**************************************************************************************************
 * @(#)ResumeAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES Resume menu option.
 *
 * @author		Cimulus
 * @version		2003-08-21
**/
public class ResumeAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_RESUME = "resume-command";
	/** Constant action name. **/
	static final String NAME_RESUME = "Resume";
	/** Constant small icon path and file name. **/
	static final String SMALL_ICON_RESUME = "toolbarButtonGraphics/media/StepForward16.gif";
	/** Constant large icon path and file name. **/
	static final String LARGE_ICON_RESUME = "toolbarButtonGraphics/medai/StepForward24.gif";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_RESUME = "Resume";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_RESUME = 
			"Resume execution of paused RunSpec";
//	static final int MNEMONIC_KEY_RESUME = 'A';

	/**
	 * Implements the Resume menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public ResumeAction() {
		putValue(Action.NAME, NAME_RESUME);
		putValue(Action.SMALL_ICON, getIcon(SMALL_ICON_RESUME));
		putValue(LARGE_ICON, getIcon(LARGE_ICON_RESUME));
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_RESUME);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_RESUME);
//		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_RESUME));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_RESUME);
	}
}
