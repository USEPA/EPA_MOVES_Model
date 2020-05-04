/**************************************************************************************************
 * @(#)Description.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.util.TreeMap;

/**
 * Class for MOVES Description panel.
 *
 * Constructs a Description panel, which consists of Description label, Description text control, 
 * Description scroll pane.  Creates and sets the layouts of the controls. Loads/Saves the 
 * description text from/to a RunSpec.  Gets text to be included in the RunSpec print out.
 *
 * @author	Wesley Faler
 * @author	EPA-elg  	(modified panel size and word wrap)
 * @version	2012-11-09
**/
public class Description extends JPanel implements RunSpecEditor {
	/** Description label. **/
	JLabel descriptionLabel;
	/** Description text control. **/
	JTextArea descriptionText;
	/** Description scroll pane. **/
	JScrollPane descriptionScrollPane;

	/**
	 *	Constructs a Description panel, also creates and sets the layouts of the controls.
	**/
	public Description() {
		createControls();
		arrangeControls();
	}
	
	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("Description:\r\n");
		if(runspec.description.length() > 0) {
			destination.append(runspec.description + "\r\n");
		}
		destination.append("\r\n");
	}
	
	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		descriptionLabel = new JLabel("Description:");
		descriptionLabel.setName("descriptionLabel");
		descriptionText = new JTextArea(15,30);
		descriptionText.setWrapStyleWord(true);
		descriptionText.setName("descriptionText");
		ToolTipHelper.add(descriptionText,"Edit description of the RunSpec");
		descriptionScrollPane = new JScrollPane(descriptionText,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, 
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		descriptionScrollPane.setName("descriptionScrollPane");
	}
	
	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 2;
		gbc.weightx = 0;
		gbc.weighty = 0;
		setLayout(new GridBagLayout());

		descriptionText.setLineWrap(true);

		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		add(descriptionLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		add(descriptionScrollPane, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0, 2, "WEST", 1, 1);
		add(new JLabel(), gbc);
	}

	/**
	 * Saves the description text to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.description = descriptionText.getText();
	}

	/**
	 * Loads the description text from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		descriptionText.setText(StringUtilities.safeGetString(runspec.description).trim());
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		// The description is acceptable even if blank.
		boolean isOK = true;
		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(isOK?RunSpecSectionStatus.OK:RunSpecSectionStatus.NOT_READY);
		sections.put(getName(),status);
		return status;
	}

	/**
	 * Sets the defaults to the RunSpec.
	 * @param	runspec the RunSpec to the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		runspec.description = "";
		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.DEFAULTS);
		sections.put(getName(),status);
		return status;
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
	public RunSpecSectionStatus onScaleChange(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		// Nothing special to do here
		return calculateRunSpecSectionStatus(runspec,sections);
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
