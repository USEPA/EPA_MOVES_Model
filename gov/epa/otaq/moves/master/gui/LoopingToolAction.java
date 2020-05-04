/**************************************************************************************************
 * @(#)LoopingToolAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Looping Tool" menu option.
 *
 * @author		Wes Faler
 * @version		2009-03-19
**/
public class LoopingToolAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY = 
			"looping-tool";
	/** Constant action name. **/
	static final String NAME = 
			"Multiple RunSpec Creator";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION = 
			"Start the Multiple RunSpec Creator GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION = 
			"Create multiple RunSpecs from a template control file";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'M';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public LoopingToolAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
