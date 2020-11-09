/**************************************************************************************************
 * @(#)UpdateWellToPumpRatesAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES UpdateWellToPumpRates menu option.
 *
 * @author		Cimulus
 * @version		2004-01-05
**/
public class UpdateWellToPumpRatesAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_UPDATE_WELL_TO_PUMP_RATES = "update-well-to-pump-rates";
	/** Constant action name. **/
	static final String NAME_UPDATE_WELL_TO_PUMP_RATES = "Update Well-To-Pump rates...";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_UPDATE_WELL_TO_PUMP_RATES = "Update Well-To-Pump rates";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_UPDATE_WELL_TO_PUMP_RATES = "Update Well-To-Pump rates";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_UPDATE_WELL_TO_PUMP_RATES = 'W';

	/**
	 * Implements the Clear menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public UpdateWellToPumpRatesAction() {
		putValue(Action.NAME, NAME_UPDATE_WELL_TO_PUMP_RATES);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_UPDATE_WELL_TO_PUMP_RATES);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_UPDATE_WELL_TO_PUMP_RATES);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_UPDATE_WELL_TO_PUMP_RATES));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_UPDATE_WELL_TO_PUMP_RATES);
	}
}
