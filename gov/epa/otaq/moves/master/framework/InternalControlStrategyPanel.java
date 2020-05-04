/**************************************************************************************************
 * @(#)InternalControlStrategiesPanel.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.*;
import java.sql.*;
import javax.swing.JPanel;
import gov.epa.otaq.moves.master.gui.*;
import gov.epa.otaq.moves.master.runspec.*;


/**
 * The base class for all panels providing GUI access to instance data within
 * InternalControlStrategy objects.
 * 
 * @author		Wesley Faler
 * @version		2012-11-09
**/
public abstract class InternalControlStrategyPanel extends JPanel implements RunSpecEditor {
/**
* The name of the currentClassName that the ControlStrategiesPanel is currently controlling 
**/
	String currentClassName = "" ;

	/**
	 * Push visual information into a RunSpec object.
	 * @param runspec The runspec to save the editor's values to.
	**/
	public void saveToRunSpec(RunSpec runspec) {
	}

	/**
	 * Retrieve information from a RunSpec object and push it to visual controls.
	 * @param runspec The runspec to load the editor's values from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
	}

	/**
	 * Determine the status of a portion of the RunSpec.  This status is used for displaying
	 * an icon to the user to let them know whether enough data is present to execute the RunSpec.
	 * <br>
	 * The RunSpecEditor should make note of the new status in the section map, likely using
	 * it's name as the key.  If stored by a previous call, a previously calculated
	 * status is in the map as well and will need to be removed prior to updating.
	 *
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,TreeMap sections) {
		return null ;
	}

	/**
	 * Obtain an explanation for a recent called to calculateRunSpecSectionStatus that
	 * yielded anything other than total success.
	 * @return human-readable text explaining the current state.
	**/
	public String explainRunSpecSectionStatus() {
		return null;
	}

	/**
	 * Place default values into a section of the RunSpec under control of this editor.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,TreeMap sections) {
		return null ;
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
	}

	/**
	 * Update current selections to be consistent with a newly selected Model.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	@Override
	public RunSpecSectionStatus onModelChange(RunSpec runspec,
			TreeMap<String, RunSpecSectionStatus> sections) {
		// Nothing to do here
		return null;
	}
}
