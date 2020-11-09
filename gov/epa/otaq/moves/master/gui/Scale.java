/**************************************************************************************************
 * @(#)Scale.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.common.*;
import java.util.TreeMap;

/**
 * Class for MOVES Scale panel. The Scale panel contains three option boxes, Macroscale, to
 * simulate at the county level and higher, the Mesoscale, to simulate links and Zones
 * (sub county level), and the Microscale, to simulate small, highly detailed structures.
 * There are two combo boxes for the input database selections when the Mesoscale or the
 * Microscale options are selected. This class loads/saves information from/to the RunSpec.
 *
 * @author		Wesley Faler
 * @author		Tim Hull
 * @author  	Bill Shaw (508 compliance mods)
 * @author  	John Covey (Task 1903)
 * @author		Mike Kender (Task 2003)
 * @author  	John Covey (Task 2003)
 * @version     2020-08-10
**/
public class Scale extends JPanel implements ActionListener, RunSpecEditor {
	/** Default domain radio button **/
	JRadioButton defaultRadioButton;
	/** Single county domain radio button **/
	JRadioButton countyRadioButton;
	/** Project domain radio button **/
	JRadioButton projectRadioButton;
	
	/** Model related **/
	JRadioButton  onroadRadioButton;
	JRadioButton nonroadRadioButton;
	JLabel modelDescriptionLabel;

	/** Inventory calculations radio button **/
	JRadioButton inventoryRadioButton;
	/** Emission Rates calculations radio button **/
	JRadioButton emissionRatesRadioButton;

	/** Models of the last loaded runspec **/
	Models loadedModels = null;
	/** Scale of the last loaded runspec **/
	ModelScale loadedScale = null;
	/** Domain of the last loaded runspec **/
	ModelDomain loadedDomain = null;

	/** Scenario label. **/
	JLabel scenarioLabel;
	JLabel scenarioRequiredLabel;
	/** Scenario text control. **/
	JTextField scenarioText;

	/** Warning about use with SIP results **/
	JPanel sipWarningPanel;

	JLabel defaultLabel;
	JLabel countyLabel;
	JLabel projectLabel;
	JPanel countyWarningPanel;
	JPanel projectWarningPanel;

	/**
	 *	Constructs a Scale panel, also creates and sets the layouts of the controls.
	**/
	public Scale() {
		createAndArrangeControls();
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("Domain/Scale: ");
		if(runspec.domain == ModelDomain.PROJECT) {
			destination.append("Project\r\n");
		} else if(runspec.domain == ModelDomain.SINGLE_COUNTY) {
			destination.append("County\r\n");
		} else {
			destination.append("Default\r\n");
		}
		destination.append("Calculation Type: ");
		if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			destination.append("Emission Rates\r\n");
			destination.append("\tMOVESScenarioID: ");
			String t = StringUtilities.safeGetString(runspec.scenarioID).trim();
			destination.append(t + "\r\n");
		//} else if(runspec.scale == ModelScale.MESOSCALE) {
		//	destination.append("\tMesoscale\r\n");
		//} else if (runspec.scale == ModelScale.MICROSCALE) {
		//	destination.append("\tMicroscale\r\n");
		} else {
			destination.append("Inventory\r\n");
		}
		destination.append("\r\n");
	}

	/** Creates and arranges all controls on this panel. **/
	public void createAndArrangeControls() {
		JPanel panel2, panel3;
		JLabel label4;
		JLabel label5;
		JPanel modelPanel;
		JLabel modelLabel;

		modelPanel = new JPanel();
		
		modelLabel = new JLabel("Model: ");
		onroadRadioButton = new JRadioButton("Onroad");
		onroadRadioButton.setMnemonic('O');
		nonroadRadioButton = new JRadioButton("Nonroad");
		
		panel2 = new JPanel();
		defaultRadioButton = new JRadioButton();
		defaultLabel = new JLabel();
		countyRadioButton = new JRadioButton();
		countyLabel = new JLabel();
		projectRadioButton = new JRadioButton();
		projectLabel = new JLabel();
		panel3 = new JPanel();
		inventoryRadioButton = new JRadioButton();
		label4 = new JLabel();
		emissionRatesRadioButton = new JRadioButton();
		label5 = new JLabel();

		scenarioLabel = new JLabel("MOVESScenarioID:");
		scenarioLabel.setName("scenarioLabel");

		scenarioText = new JTextField(40);
		scenarioText.addKeyListener(new KeyAdapter() {
            public void keyReleased(KeyEvent e) {
            	scenarioRequiredLabel.setVisible(scenarioText.isVisible() && scenarioText.getText().length() == 0);
            }
        });

		scenarioLabel.setLabelFor(scenarioText);
		ToolTipHelper.add(scenarioText,
				"The 40-character column MOVESScenarioID referenced in the rate output tables is required when Emission Rates is selected");
		scenarioText.setName("scenarioText");
		scenarioText.setColumns(40);

		//======== this ========
		JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));

		ImageIcon warningImage = null;
		JLabel warningLabel = null;
		JPanel warningPanel = null;
		sipWarningPanel = null;
		
		// add model panel
		{
			ButtonGroup modelButtonGroup = new ButtonGroup();
			modelButtonGroup.add(onroadRadioButton);
			modelButtonGroup.add(nonroadRadioButton);
			onroadRadioButton.addActionListener(this);
			nonroadRadioButton.addActionListener(this);
			modelDescriptionLabel = new JLabel();
			
			modelPanel.setBorder(BorderFactory.createTitledBorder("Model"));
			modelPanel.setLayout(new GridBagLayout());
			
			((GridBagLayout)modelPanel.getLayout()).columnWidths = new int[] {0, 0, 0};
			((GridBagLayout)modelPanel.getLayout()).rowHeights = new int[] {0, 0, 0, 0};
			((GridBagLayout)modelPanel.getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0E-4};
			((GridBagLayout)modelPanel.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};

			modelPanel.add(onroadRadioButton, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 5), 0, 0));
			JLabel onrdLable = new JLabel();
			onrdLable.setText("<html><body>"
					+ "Estimate emissions from motorcycles, cars, buses, and trucks <br>"
					+ "that operate on roads."
					+ "</html></body>");

			modelPanel.add(onrdLable, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));

			modelPanel.add(nonroadRadioButton, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 5), 0, 0));
			JLabel nonrdLable = new JLabel();
			nonrdLable.setText("<html><body>"
					+ "Estimate emissions from nonroad equipment used in applications <br>"
					+ "such as recreation, construction, lawn and garden, agriculture, mining, etc. <br>"
					+ "Nonroad does not include aircraft, railroads, or commercial marine vessels.<br>"
					+ "</html></body>");
			modelPanel.add(nonrdLable, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));
			
			result.add(modelPanel);
		}

		//======== panel2 ========
		{
			panel2.setBorder(BorderFactory.createTitledBorder("Domain/Scale"));
			panel2.setLayout(new GridBagLayout());
			((GridBagLayout)panel2.getLayout()).columnWidths = new int[] {0, 0, 0};
			((GridBagLayout)panel2.getLayout()).rowHeights = new int[] {0, 0, 0, 0};
			((GridBagLayout)panel2.getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0E-4};
			((GridBagLayout)panel2.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};

			//---- defaultRadioButton ----
			defaultRadioButton.setText("Default Scale");
			defaultRadioButton.setMnemonic('D');
			panel2.add(defaultRadioButton, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- label1 ----
			defaultLabel.setText("Use the default national database with default state and local allocation factors.");
			panel2.add(defaultLabel, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			warningImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/dataExists.gif");
			warningLabel = new JLabel(
					"<html><body>"
					+ "Caution:  Do not use this scale setting for SIP or conformity<br>"
					+ "analyses. The allocation factors and other defaults applied at<br>"
					+ "the state or county level have not been verified against specific<br>"
					+ "state or county data and do not meet regulatory requirements for<br>"
					+ "SIPs and conformity determinations."
					+ "</body></html>",
					warningImage, JLabel.LEFT);
			sipWarningPanel = new JPanel();
			sipWarningPanel.setLayout(new BoxLayout(sipWarningPanel, BoxLayout.X_AXIS));
			sipWarningPanel.add(warningLabel);
			sipWarningPanel.add(Box.createHorizontalGlue());

			panel2.add(sipWarningPanel, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//---- countyRadioButton ----
			countyRadioButton.setText("County Scale");
			panel2.add(countyRadioButton, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			warningLabel = new JLabel(
					"<html><body>"
					+ "Use this scale for SIP and regional conformity analysis. <br>"
					+ "This scale requires user-supplied local data for most activity and fleet inputs."
					+ "</body></html>",
					JLabel.LEFT);
			countyWarningPanel = new JPanel();
			countyWarningPanel.setLayout(new BoxLayout(countyWarningPanel, BoxLayout.X_AXIS));
			countyWarningPanel.add(warningLabel);
			countyWarningPanel.add(Box.createHorizontalGlue());

			panel2.add(countyWarningPanel, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//---- projectRadioButton ----
			projectRadioButton.setText("Project Scale");
			panel2.add(projectRadioButton, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 5), 0, 0));

			warningLabel = new JLabel(
					"<html><body>"
					+ "Use this scale for project-level analysis for conformity, NEPA, or other <br>"
					+ "regulatory purposes where link-level analysis is needed. This scale requires <br>"
					+ "user-supplied data at the link level for activity and fleet inputs that <br>"
					+ "describe a particular transportation project."
					+ "</body></html>",
					JLabel.LEFT);
			projectWarningPanel = new JPanel();
			projectWarningPanel.setLayout(new BoxLayout(projectWarningPanel, BoxLayout.X_AXIS));
			projectWarningPanel.add(warningLabel);
			projectWarningPanel.add(Box.createHorizontalGlue());

			panel2.add(projectWarningPanel, new GridBagConstraints(1, 4, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
		}
		result.add(panel2);

		//======== panel3 ========
		{
			panel3.setBorder(BorderFactory.createTitledBorder("Calculation Type"));
			panel3.setLayout(new GridBagLayout());
			((GridBagLayout)panel3.getLayout()).columnWidths = new int[] {0, 0, 0};
			((GridBagLayout)panel3.getLayout()).rowHeights = new int[] {0, 0, 0};
			((GridBagLayout)panel3.getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0E-4};
			((GridBagLayout)panel3.getLayout()).rowWeights = new double[] {0.0, 0.0, 1.0E-4};

			//---- inventoryRadioButton ----
			inventoryRadioButton.setText("Inventory");
			inventoryRadioButton.setMnemonic('I');
			panel3.add(inventoryRadioButton, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- label4 ----
			label4.setText("Mass and/or Energy within a region and time span.");
			panel3.add(label4, new GridBagConstraints(1, 0, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//---- emissionRatesRadioButton ----
			emissionRatesRadioButton.setText("Emission Rates");
			panel3.add(emissionRatesRadioButton, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 5), 0, 0));

			//---- label5 ----
			label5.setText("Mass and/or Energy per unit of activity.");
			panel3.add(label5, new GridBagConstraints(1, 1, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 0), 0, 0));

			panel3.add(scenarioLabel, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 0), 0, 0));
			panel3.add(scenarioText, new GridBagConstraints(1, 3, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 0), 0, 0));
			scenarioRequiredLabel = new JLabel("*Required when Emission Rates is selected*", SwingConstants.CENTER);
			panel3.add(scenarioRequiredLabel, new GridBagConstraints(2, 2, 2, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 0, 5), 0, 0));
		}
		result.add(panel3);

		//---- domainButtonGroup ----
		ButtonGroup domainButtonGroup = new ButtonGroup();
		domainButtonGroup.add(defaultRadioButton);
		domainButtonGroup.add(countyRadioButton);
		domainButtonGroup.add(projectRadioButton);

		//---- calculationButtonGroup ----
		ButtonGroup calculationButtonGroup = new ButtonGroup();
		calculationButtonGroup.add(inventoryRadioButton);
		calculationButtonGroup.add(emissionRatesRadioButton);

		// Warning image and text
		warningImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/dataExists.gif");
		warningLabel = new JLabel();
		warningLabel.setName("warningLabel");
		warningLabel = new JLabel(
				"<html><body>"
				+ "Caution: Changing these selections changes the contents of other<br>"
				+ "input panels.  These changes may include losing previous data contents."
				+ "</body></html>",
				warningImage, JLabel.LEFT);
		warningPanel = new JPanel();
		warningPanel.setLayout(new BoxLayout(warningPanel, BoxLayout.X_AXIS));
		warningPanel.add(warningLabel);
		warningPanel.add(Box.createHorizontalGlue());
		result.add(warningPanel);

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;
		setLayout(new GridBagLayout());
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		add(result, gbc);

		defaultRadioButton.setName("default");
		defaultRadioButton.addActionListener(this);

		countyRadioButton.setName("county");
		countyRadioButton.addActionListener(this);

		projectRadioButton.setName("project");
		projectRadioButton.addActionListener(this);

		inventoryRadioButton.setName("inventory");
		inventoryRadioButton.addActionListener(this);

		emissionRatesRadioButton.setName("emissionrates");
		emissionRatesRadioButton.addActionListener(this);
	}

	/**
	 * Saves the scale to a RunSpec.
	 * @param	runspec the RunSpec to save the scale to.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.scenarioID = scenarioText.getText().trim();

		if(emissionRatesRadioButton.isSelected()) {
			runspec.scale = ModelScale.MESOSCALE_LOOKUP;
			runspec.outputPopulation = true;
			runspec.outputVMTData = true;
			runspec.doNotPerformFinalAggregation = false;
			runspec.outputEmissionsBreakdownSelection.estimateUncertainty = false;
			runspec.outputEmissionsBreakdownSelection.emissionProcess = true;

			runspec.outputTimeStep = OutputTimeStep.HOUR;
			runspec.outputFactors.timeMeasurementSystem = TimeMeasurementSystem.HOURS;
		} else {
			runspec.scale = ModelScale.MACROSCALE;
		}
		loadedScale = runspec.scale;

		if(projectRadioButton.isSelected()) {
			runspec.domain = ModelDomain.PROJECT;
			runspec.outputTimeStep = OutputTimeStep.HOUR;
			runspec.outputFactors.timeMeasurementSystem = TimeMeasurementSystem.HOURS;
			runspec.geographicOutputDetail = GeographicOutputDetailLevel.LINK;
			runspec.timeSpan.aggregateBy = OutputTimeStep.HOUR;
		} else if(countyRadioButton.isSelected()) {
			runspec.domain = ModelDomain.SINGLE_COUNTY;
		} else {
			runspec.domain = ModelDomain.NATIONAL_ALLOCATION;
		}
		loadedDomain = runspec.domain;
		
		if ( onroadRadioButton.isSelected()) {
			runspec.models.clear();
			runspec.models.add(Model.ONROAD);
		} else if ( nonroadRadioButton.isSelected()) {
			runspec.models.clear();
			runspec.models.add(Model.NONROAD);
		}
		loadedModels = runspec.models;
	}

	/**
	 * Loads the scale from a RunSpec.
	 * @param	runspec the RunSpec to get the scale from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		loadedModels = runspec.models;
		loadedScale = runspec.scale;
		loadedDomain = runspec.domain;
		
		onroadRadioButton.setSelected(false);
		nonroadRadioButton.setSelected(false);
		defaultRadioButton.setSelected(false);
		countyRadioButton.setSelected(false);
		projectRadioButton.setSelected(false);
		inventoryRadioButton.setSelected(false);
		emissionRatesRadioButton.setSelected(false);

		Models.ModelCombination mc = Models.evaluateModels(loadedModels);
		switch( mc) {
		case M1:
			onroadRadioButton.setSelected(true);
			break;
		case M2:
			nonroadRadioButton.setSelected(true);
			break;
		default:
			onroadRadioButton.setSelected(true);
			break;
		}
		
		if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			emissionRatesRadioButton.setSelected(true);
		} else {
			inventoryRadioButton.setSelected(true);
		}

		if(runspec.domain == ModelDomain.PROJECT) {
			projectRadioButton.setSelected(true);
		} else if(runspec.domain == ModelDomain.SINGLE_COUNTY) {
			countyRadioButton.setSelected(true);
		} else {
			defaultRadioButton.setSelected(true);
		}

		scenarioText.setText(StringUtilities.safeGetString(runspec.scenarioID).trim());

		assessSituation();
	}

	/** Enable/Disable controls based on their interaction rules **/
	void assessSituation() {
		/*
		if(emissionRatesRadioButton.isSelected()) {
			projectRadioButton.setEnabled(false);
			if(projectRadioButton.isSelected()) {
				projectRadioButton.setSelected(false);
				defaultRadioButton.setSelected(true);
			}
		} else if(projectRadioButton.isSelected()) {
			emissionRatesRadioButton.setEnabled(false);
			if(emissionRatesRadioButton.isSelected()) {
				emissionRatesRadioButton.setSelected(false);
				inventoryRadioButton.setSelected(true);
			}
		} else {
			emissionRatesRadioButton.setEnabled(true);
			projectRadioButton.setEnabled(true);
		}
		*/
		
		boolean onroadSelected = onroadRadioButton.isSelected();
		countyRadioButton.setEnabled(onroadSelected);
		projectRadioButton.setEnabled(onroadSelected);
		emissionRatesRadioButton.setEnabled(onroadSelected);
		if(onroadSelected) {
			sipWarningPanel.setVisible(true);
			defaultLabel.setText("Use the default national database with default state and local allocation factors.");
			countyLabel.setVisible(true);
			projectLabel.setVisible(true);
			countyWarningPanel.setVisible(true);
			projectWarningPanel.setVisible(true);
			//jxc
		} else {
			sipWarningPanel.setVisible(false);
			defaultLabel.setText("Use the Nonroad Data Importer or Nonroad Post-Processing scripts to apply local data.");
			countyLabel.setVisible(false);
			projectLabel.setVisible(false);
			countyWarningPanel.setVisible(false);
			projectWarningPanel.setVisible(false);
		}
		
		boolean shouldUseScenario = emissionRatesRadioButton.isSelected();
		scenarioLabel.setEnabled(shouldUseScenario);
		scenarioText.setEnabled(shouldUseScenario);
		scenarioRequiredLabel.setVisible(shouldUseScenario && !isScenarioIdValid());
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		boolean isOK = true;
		
		Models.ModelCombination mc = Models.evaluateModels(runspec.models);
		
		switch (mc) {
		case M1: // ONROAD
			
			// When using emission rates, a scenario ID must be provided.
			if(runspec.scale == ModelScale.MESOSCALE_LOOKUP || emissionRatesRadioButton.isSelected()) {
				String t = StringUtilities.safeGetString(runspec.scenarioID).trim();
				if(t.length() <= 0) {
					isOK = false;
				}
			}

			if((defaultRadioButton.isSelected() || countyRadioButton.isSelected()
					|| projectRadioButton.isSelected())
					&& (inventoryRadioButton.isSelected()
							|| emissionRatesRadioButton.isSelected())
					) {
				// Nothing to do here
			} else {
				isOK = false;
			}
			break;
		case M2: // NONROAD
			if ( !defaultRadioButton.isSelected() || !inventoryRadioButton.isSelected())
				isOK =  false;
			break;
		default: 
			break;
		}

		RunSpecSectionStatus status = new RunSpecSectionStatus(isOK?RunSpecSectionStatus.OK:RunSpecSectionStatus.NOT_READY);
		sections.remove(getName());
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
		runspec.scale = ModelScale.MACROSCALE;
		runspec.domain = ModelDomain.NATIONAL_ALLOCATION;
		runspec.scenarioID = "";
		runspec.models.add(Model.ONROAD);
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
	 * Handles a user's action.
	 * @param	e The event caused by the user's action.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == inventoryRadioButton) {
			processInventoryButton();
		} else if(e.getSource() == emissionRatesRadioButton) {
			processEmissionRatesButton();
		} else if(e.getSource() == defaultRadioButton) {
			processDefaultButton();
		} else if(e.getSource() == countyRadioButton) {
			processCountyButton();
		} else if(e.getSource() == projectRadioButton) {
			processProjectButton();
		} else if (e.getSource() == onroadRadioButton) {
			processOnroadButton();
		} else if (e.getSource() == nonroadRadioButton) {
			processNonroadButton();
		}
		assessSituation();
		MOVESNavigation.singleton.updateRunSpecSectionStatus();
	}

	private void processNonroadButton() {
		loadedModels.getModelList().clear();
		loadedModels.add(Model.NONROAD);
		MOVESNavigation.singleton.onModelsChange(this,loadedModels);
	}

	private void processOnroadButton() {
		loadedModels.getModelList().clear();
		loadedModels.add(Model.ONROAD);
		MOVESNavigation.singleton.onModelsChange(this,loadedModels);
	}

	/**
	 * Handles the Inventory button.
	**/
	public void processInventoryButton() {
		if(loadedScale != ModelScale.MACROSCALE) {
			MOVESNavigation.singleton.onScaleChange(this,ModelScale.MACROSCALE);
			loadedScale = ModelScale.MACROSCALE;
		}
	}

	/**
	 * Handles the Emission Rates button.
	**/
	public void processEmissionRatesButton() {
		if(loadedScale != ModelScale.MESOSCALE_LOOKUP) {
			MOVESNavigation.singleton.onScaleChange(this,ModelScale.MESOSCALE_LOOKUP);
			loadedScale = ModelScale.MESOSCALE_LOOKUP;
		}
	}

	/**
	 * Handles the Default button.
	**/
	public void processDefaultButton() {
		if(loadedDomain != ModelDomain.NATIONAL_ALLOCATION) {
			MOVESNavigation.singleton.onDomainChange(this,ModelDomain.NATIONAL_ALLOCATION);
			loadedDomain = ModelDomain.NATIONAL_ALLOCATION;
		}
	}

	/**
	 * Handles the County button.
	**/
	public void processCountyButton() {
		if(loadedDomain != ModelDomain.SINGLE_COUNTY) {
			MOVESNavigation.singleton.onDomainChange(this,ModelDomain.SINGLE_COUNTY);
			loadedDomain = ModelDomain.SINGLE_COUNTY;
		}
	}

	/**
	 * Handles the Project button.
	**/
	public void processProjectButton() {
		if(loadedDomain != ModelDomain.PROJECT) {
			MOVESNavigation.singleton.onDomainChange(this,ModelDomain.PROJECT);
			loadedDomain = ModelDomain.PROJECT;
		}
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
	public boolean isScenarioIdValid() {
		if(scenarioText.getText().length() <= 0) {
			return false;
		}
		return true;
	}
}
