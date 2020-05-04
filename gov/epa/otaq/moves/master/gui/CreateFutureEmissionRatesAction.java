/**************************************************************************************************
 * @(#)CreateFutureEmissionRatesAction.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.Action;

/**
 * Class for MOVES CreateFutureEmissionRates menu option.
 *
 * @author		EPA-Ed Glover
 * @version		2004-10-25
**/
public class CreateFutureEmissionRatesAction extends JLFAbstractAction {
	/** Constant command key text. **/
	static final String ACTION_COMMAND_KEY_CREATE_FUTURE_EMISSION_RATES = "create-future-emission-rates";
	/** Constant action name. **/
	static final String NAME_CREATE_FUTURE_EMISSION_RATES = "Update Future Emission Rates";
	/** Constant short description text. **/
	static final String SHORT_DESCRIPTION_CREATE_FUTURE_EMISSION_RATES = "FERC";
	/** Constant long description text. **/
	static final String LONG_DESCRIPTION_CREATE_FUTURE_EMISSION_RATES = "Update Future Emission Rates";
	/** Constant mnemonic key. **/
	static final int MNEMONIC_KEY_CREATE_FUTURE_EMISSION_RATES = 'Z';

	/**
	 * Implements the Create Future Emission Rates menu option UI, including command keys,
	 * name, descriptions, and icons.
	**/
	public CreateFutureEmissionRatesAction() {
		putValue(Action.NAME, NAME_CREATE_FUTURE_EMISSION_RATES);
		putValue(Action.SHORT_DESCRIPTION, SHORT_DESCRIPTION_CREATE_FUTURE_EMISSION_RATES);
		putValue(Action.LONG_DESCRIPTION, LONG_DESCRIPTION_CREATE_FUTURE_EMISSION_RATES);
		putValue(Action.MNEMONIC_KEY, new Integer(MNEMONIC_KEY_CREATE_FUTURE_EMISSION_RATES));
		putValue(Action.ACTION_COMMAND_KEY, ACTION_COMMAND_KEY_CREATE_FUTURE_EMISSION_RATES);
	}
}
