/**************************************************************************************************
 * @(#)DrawStateCountyMapAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES state/county map menu option.
 *
 * @author		Wesley Faler
 * @version		2007-03-13
**/
public class DrawStateCountyMapAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_MAP = "state county map";
	/** Constant action name. **/
	static final String NAME_MAP = "Produce State/County Map";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_MAP = "State/County Map";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_MAP = "Produce State/County Map";

	/**
	 * Implements the "Draw State/County Map" menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public DrawStateCountyMapAction() {
		putValue(Action.NAME, NAME_MAP);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_MAP);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_MAP);
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_MAP);
	}
}
