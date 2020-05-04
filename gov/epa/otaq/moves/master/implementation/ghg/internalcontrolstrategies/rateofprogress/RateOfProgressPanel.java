/**************************************************************************************************
 * @(#)RateOfProgressPanel.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.text.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.util.*;
import java.lang.*;
import java.sql.*;
import java.math.*;
import java.io.*;
import javax.swing.table.*;
import javax.swing.event.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.*;
import gov.epa.otaq.moves.master.runspec.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;

/**
 * Implements a Panel for Rate of Progress Modeling
 *
 * @author		Wesley Faler
 * @version		2012-06-20
**/
class RateOfProgressPanel extends InternalControlStrategyPanel
		implements ActionListener, IImportExportHandler {
	/** A reference to the strategy that is being used for this execution **/
	RateOfProgressStrategy strategy ;

	/** Title text **/
	JLabel titleLabel;

	/** Checkbox on loadedPanel for enabling use of the Rate of Progress calculations **/
	JCheckBox useSettings;
	/** Usage note label **/
	JLabel usageNoteLabel;

	/**
	 * true after the constructor has completely finished.  Used to surpress the display
	 * of a warnings before the user even sees the screen.
	**/
	boolean isLoaded = false;

	/**
	 * Constructor
	 * @param strategyToUse the strategy that owns this panel
	**/
	public RateOfProgressPanel(RateOfProgressStrategy strategyToUse) {
		strategy = strategyToUse ;

		if(strategy == null) {
			return ;
		}

		try {
			createControls();
			arrangeControls();
			populateControls();
		} catch(Exception e) {
			Logger.logError(e,"Unable to construct RateOfProgressPanel");
		}

		isLoaded = true;
	}

	/**
	 * Creates and initializes all controls on this panel. And fixes the height
	 * and width of the JTable to fit in the screen.
	**/
	public void createControls() {
		titleLabel = new JLabel("Rate of Progress");

		useSettings = new JCheckBox(
				"Compute Rate-of-Progress \"No Clean Air Act Amendments\" Emissions");
		useSettings.setName("useSettings");
		useSettings.addActionListener(this);

		int cutModelYearID = 1993;
		String sql = "select modelYearID"
				+ " from modelYearCutPoints"
				+ " where cutPointName='RateOfProgress'";
		try {
			Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
			cutModelYearID = (int)SQLRunner.executeScalar(db,sql);
		} catch(SQLException e) {
			Logger.logSqlError(e,"Unable to load ROP cutoff model year",sql);
		}
		usageNoteLabel = new JLabel(
				"<html><body><br>"
				+ "The Rate-of-Progress Calculation strategy supports users modeling vehicle<br>"
				+ "emissions for Reasonable Further Progress SIP requirements.  It models a<br>"
				+ "\"No Clean Air Act Amendments\" scenario by assigning " + cutModelYearID + " model year<br>"
				+ "emission rates to all post-" + cutModelYearID + " vehicles.<br><br>"
				+ "See the MOVES user guide and guidance documents for more details."
				+"</body></html>");
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		Box b;

		JPanel loadedPanel = new JPanel();
		loadedPanel.setLayout(new BoxLayout(loadedPanel, BoxLayout.Y_AXIS));

		b = Box.createHorizontalBox();
		b.add(useSettings);
		b.add(Box.createHorizontalGlue());
		loadedPanel.add(b);

		b = Box.createHorizontalBox();
		b.add(usageNoteLabel);
		b.add(Box.createHorizontalGlue());
		loadedPanel.add(b);

		setLayout(new BorderLayout());
		//add(titleLabel, BorderLayout.NORTH);
		add(loadedPanel, BorderLayout.CENTER);
	}

	/**
	 * Populates droplist for SourceType
	**/
	void populateControls() {
		useSettings.setSelected(strategy.useParameters);
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("Rate of Progress:\r\n");

		if(strategy.useParameters) {
			destination.append("\tRate of Progress calculations are enabled\r\n");
		} else {
			destination.append("\tRate of Progress calculations are disabled\r\n");
		}
	}

	/**
	 * Saves the description text to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		// Nothing needs to be done since all data is already in desirable data structures
	}

	/**
	 * Loads the description text from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		// Nothing needs to be done since all data is already in desirable data structures
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,TreeMap sections) {
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK) ;
	}

	/**
	 * Obtain an explanation for a recent called to calculateRunSpecSectionStatus that
	 * yielded anything other than total success.
	 * @return human-readable text explaining the current state.
	**/
	public String explainRunSpecSectionStatus() {
		return "There are no issues with the current Rate of Progress data.";
	}

	/**
	 * Sets the defaults to the RunSpec.
	 * @param	runspec the RunSpec to the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,TreeMap sections) {
		// Nothing needs to be done here since defaults are handled elsewhere for this
		// type of InternalControlStrategy.
		return null;
	}

	/**
	 * Update current selections to be consistent with a newly selected ModelScale.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus onScaleChange(RunSpec runspec,TreeMap sections) {
		// Nothing special to do here
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/**
	 * This method provides the functionality for the cancel button. It returns the memory
	 * in the tables back to the data as it was from the last Import or load.
	 * If the screen needs to be repainted, then it is the responsibility of the caller to
	 * execute this command
	**/
	public void cancel() {
		if(strategy.resetTSV != null) {
			strategy.acceptTSV("RateOfProgressPanel",strategy.resetTSV);
			populateControls();
			revalidate();
		}
	}

	/**
	 * Callback routine implementing the ActionListener interface
	 * @param evt event with action details
	**/
	public void actionPerformed( ActionEvent evt ) {
		if(evt.getSource() == useSettings) {
			processUseROPCheckbox();
		}
		MOVESNavigation.singleton.updateRunSpecSectionStatus();
	}

	/** Handle a change to useSettings **/
	void processUseROPCheckbox() {
		strategy.useParameters = useSettings.isSelected();
	}

	/**
	 * Process a user's request to initiate an import operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was imported.
	 * false if the import request should be handled by default logic instead.
	**/
	public boolean doImport(Frame ownerWindow) {
		return true;
	}

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	public boolean doExport(Frame ownerWindow) {
		return true;
	}
}
