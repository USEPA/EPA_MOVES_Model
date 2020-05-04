/**************************************************************************************************
 * @(#)ProjectDomainManagerAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES "Project Domain Manager" menu option.
 *
 * @author		wgfaler
 * @version		2008-12-15
**/
public class ProjectDomainManagerAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY = 
			"project-domain-manager";
	/** Constant action name. **/
	static final String NAME = 
	/**		"Project Domain Manager"; **/
                "Project Data Manager";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION = 
			"Start Project Domain Manager GUI";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION = 
			"Select and Import Project-Level Data";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY = 'P';

	/**
	 * Implements the menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public ProjectDomainManagerAction() {
		putValue(Action.NAME, NAME);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY);
	}
}
