/**************************************************************************************************
 * @(#)RunSpecEditor.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import gov.epa.otaq.moves.master.runspec.RunSpec;
import java.util.TreeMap;

/**
 * Interface for the RunSpecEditor. This interface contains methods to push 
 * visual information into a RunSpec object and retrieve the RunSpec information 
 * into visual controls, to determine the status of a part of a RunSpec, and to
 * place default values into a section of a RunSpec. 

 *
 * @author		Wesley Faler
 * @version		2012-11-09
**/
public interface RunSpecEditor {
	/**
	 * Push visual information into a RunSpec object.
	 * @param runspec The runspec to save the editor's values to.
	**/
	public void saveToRunSpec(RunSpec runspec);
	/**
	 * Retrieve information from a RunSpec object and push it to visual controls.
	 * @param runspec The runspec to load the editor's values from.
	**/
	public void loadFromRunSpec(RunSpec runspec);
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
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections);

	/**
	 * Place default values into a section of the RunSpec under control of this editor.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections);
	
	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination);

	/**
	 * Update current selections to be consistent with a newly selected ModelScale.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus onScaleChange(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections);

	/**
	 * Update current selections to be consistent with a newly selected Model.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus onModelChange(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections);
}
