/**************************************************************************************************
 * @(#)UpdateManufactureDisposalRatesAction.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES UpdateManufactureDisposalRates menu option.
 *
 * @author		Cimulus
 * @version		2004-01-05
**/

public class UpdateManufactureDisposalRatesAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_UPDATE_MANUFACTURE_DISPOSAL_RATES = 
			"update-manufacture-disposal-rates";
	/** Constant action name. **/
	static final String NAME_UPDATE_MANUFACTURE_DISPOSAL_RATES = 
			"Update Manufacture/Disposal rates...";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_UPDATE_MANUFACTURE_DISPOSAL_RATES = 
			"Update Manufacture/Disposal rates";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_UPDATE_MANUFACTURE_DISPOSAL_RATES = 
			"Update Manufacture/Disposal rates";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_UPDATE_MANUFACTURE_DISPOSAL_RATES = 'M';

	/**
	 * Implements the Clear menu option UI, including command keys, name, 
	 * descriptions, and icons.
	**/
	public UpdateManufactureDisposalRatesAction() {
		putValue(Action.NAME, NAME_UPDATE_MANUFACTURE_DISPOSAL_RATES);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_UPDATE_MANUFACTURE_DISPOSAL_RATES);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_UPDATE_MANUFACTURE_DISPOSAL_RATES);
		putValue(Action.MNEMONIC_KEY, Integer.valueOf(MNEMONIC_KEY_UPDATE_MANUFACTURE_DISPOSAL_RATES));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_UPDATE_MANUFACTURE_DISPOSAL_RATES);
	}
}
