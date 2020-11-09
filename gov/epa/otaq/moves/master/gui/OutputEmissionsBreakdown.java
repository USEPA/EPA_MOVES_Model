 /*************************************************************************************************
 * @(#)OutputEmissionsBreakdown.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;

import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.MOVESAPI;
import gov.epa.otaq.moves.common.*;
import java.util.*;
import java.sql.*;

/**
 * Class for MOVES OutputEmissionsBreakdown panel. This Class contains several panels.
 * The Always panel contains  Time, Location, and Pollutant check boxes. the for
 * All Vehicle/Equipment Categories panel contains Model Year, FuelType, and
 * Emission Process. The On Road/Off Road panel contains an On Road/Off Road checkbox
 * and On Road and Off Road panels. the On Road panel contains Road Type, Source
 * Use Type, and SCC check boxes, ant the Off Road frame contains Sector, SCC, and HP Class
 * check boxes. The main panel contains a check box for estimating the uncertainty called
 * Estimate Uncertainty.
 *
 * @author		Wesley Faler
 * @author		Sarah Luo, ERG
 * @author		Mitch C (minor mods)
 * @author  	Bill Shaw (508 compliance mods)
 * @author		Mike Kender (Task 2003)
 * @version     2020-07-13
**/
public class OutputEmissionsBreakdown extends JPanel implements ActionListener, RunSpecEditor {
	/** Singleton for the navigation panel **/
	public static OutputEmissionsBreakdown singleton = null;
	/** Panel contains "always" controls. **/
	JPanel alwaysPanel;
	/** Panel contains "all" controls. **/
	JPanel allPanel;
	/** Panel contains on road controls. **/
	JPanel onRoadPanel;
	/** Panel contains off road controls. **/
	JPanel offRoadPanel;

	/** Panel with time options. **/
	JLabel timeLabel;
	/** Output Timestep combo control. **/
	JComboBox<OutputTimeStep> outputTimestepCombo;
	/** Output Time Step when user entered the screen. **/
	OutputTimeStep lastOutputTimeStep;
	/** Label for geographic options **/
	JLabel geographicLabel;
	/** Geographic output detail combo control. **/
	JComboBox<GeographicOutputDetailLevel> geographicOutputDetailCombo;

	/** Model Year option checkbox. **/
	JCheckBox modelYear;
	/** Fuel Type option checkbox. **/
	JCheckBox fuelType;
	/** Fuel SubType option checkbox. **/
	JCheckBox fuelSubType;
	/** Emissions Process option checkbox. **/
	JCheckBox emissionProcess;

	/** True when Nonroad options are in effect **/
	boolean useNonroadRules = false;

	/** Road Type option checkbox. **/
	JCheckBox roadType;
	/** Source Use Type option checkbox. **/
	JCheckBox sourceUseType;
	/** On Road SCC option checkbox. **/
	JCheckBox onRoadSCC;
	/** Regulatory Class option checkbox. **/
	JCheckBox regClass;

	/** Sector option checkbox. **/
	JCheckBox sector;
	/** Off Road engine tech option checkbox. **/
	JCheckBox engTech;
	/** HP Class option checkbox. **/
	JCheckBox hpClass;
	/** Holds helper text **/
	JLabel helper;

	/** true if the most recently loaded runspec uses mesoscale loop **/
	boolean isMesoscaleLookup = false;

	/**
	 *	Constructs a OutputEmissionsBreakdown panel, also creates and sets the layouts
	 *	of the controls.
	**/
	public OutputEmissionsBreakdown() {
		singleton = this;
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
		destination.append("Output Emissions Breakdown:\r\n");
		if(runspec.outputEmissionsBreakdownSelection.modelYear) {
			destination.append("\tModel Year\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.fuelType) {
			destination.append("\tFuel Type\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.fuelSubType) {
			destination.append("\tFuel SubType\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.emissionProcess) {
			destination.append("\tEmission Process\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.estimateUncertainty) {
			destination.append("\tEstimate Uncertainty\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.onRoadOffRoad) {
			destination.append("\tOn Road/Off Road\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.roadType) {
			destination.append("\tRoad Type\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.regClassID) {
			destination.append("\tRegulatory Class\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.sourceUseType) {
			destination.append("\tSource Use Type\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.onRoadSCC) {
			destination.append("\tSCC\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.sector) {
			destination.append("\tSector\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.engTechID) {
			destination.append("\tOff Road Engine Tech\r\n");
		}
		if(runspec.outputEmissionsBreakdownSelection.hpClass) {
			destination.append("\tHP Class\r\n");
		}
		if(runspec.outputTimeStep != null) {
			destination.append("\tOutput Time Step\r\n");
			destination.append("\t\t" + runspec.outputTimeStep + "\r\n");
		}
		if(runspec.geographicOutputDetail != null) {
			destination.append("\tGeographic Output Detail\r\n");
			destination.append("\t\t" + runspec.geographicOutputDetail + "\r\n");
		}
		destination.append("\r\n");
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		alwaysPanel = new JPanel();
		alwaysPanel.setName("alwaysPanel");
		alwaysPanel.setBorder(BorderFactory.createTitledBorder("Output Aggregation"));
		alwaysPanel.setPreferredSize(new Dimension(280, 240));
		outputTimestepCombo = new JComboBox<OutputTimeStep>();
		outputTimestepCombo.setName("outputTimestepCombo");
		outputTimestepCombo.addActionListener(this);
		outputTimestepCombo.setEditable(false);
		outputTimestepCombo.setSelectedIndex(-1);
		ToolTipHelper.add(outputTimestepCombo,"Select the time step to be used for the output "
				+ "data");
		timeLabel = new JLabel("Time:");
		timeLabel.setDisplayedMnemonic('m');
		timeLabel.setLabelFor(outputTimestepCombo);
		
		geographicOutputDetailCombo = new JComboBox<GeographicOutputDetailLevel>();
		geographicOutputDetailCombo.setName("locationUnitsCombo");
		geographicOutputDetailCombo.addActionListener(this);
		geographicOutputDetailCombo.setEditable(false);
		ToolTipHelper.add(geographicOutputDetailCombo,"Select the geographic detail to be used for the "
				+ "output data");
		geographicLabel = new JLabel("Geographic:");
		geographicLabel.setLabelFor(geographicOutputDetailCombo);
		
		helper = new JLabel("");
		//helper.setEditable(true);
		helper.setMinimumSize(new Dimension(280, 150));
		helper.setMaximumSize(new Dimension(280, 300));
		//helper.setBackground(alwaysPanel.getBackground());

		Dimension allPanelSize = new Dimension(240, 135);
		allPanel = new JPanel();
		allPanel.setName("allPanel");
		allPanel.setBorder(BorderFactory.createTitledBorder(
				"for All Vehicle/Equipment Categories"));
		allPanel.setPreferredSize(allPanelSize);
		modelYear = new JCheckBox("Model Year");
		modelYear.setName("modelYear");
		modelYear.setMnemonic('d');
		modelYear.setDisplayedMnemonicIndex(2);
		ToolTipHelper.add(modelYear,"Include model year in output data");
		fuelType = new JCheckBox("Fuel Type");
		fuelType.setName("fuelType");
		ToolTipHelper.add(fuelType,"Include fuel type in output data");
		fuelType.addActionListener(this);
		fuelSubType = new JCheckBox("Fuel Subtype");
		fuelSubType.setName("fuelSubType");
		ToolTipHelper.add(fuelSubType,"Include fuel subtype in output data");
		fuelSubType.addActionListener(this);
		emissionProcess = new JCheckBox("Emission Process");
		emissionProcess.setName("emissionProcess");
		ToolTipHelper.add(emissionProcess,"Include emission process in output data");

		Dimension onRoadPanelSize = new Dimension(160, 100 + (CompilationFlags.DO_RATES_FIRST? 20:0));
		onRoadPanel = new JPanel();
		onRoadPanel.setName("onRoadPanel");
		onRoadPanel.setBorder(BorderFactory.createTitledBorder(
				"Onroad"));
		onRoadPanel.setPreferredSize(onRoadPanelSize);
		roadType = new JCheckBox("Road Type");
		roadType.setName("roadType");
		roadType.setMnemonic('o');
		roadType.setDisplayedMnemonicIndex(1);
		ToolTipHelper.add(roadType,"Include road types in output data");
		sourceUseType = new JCheckBox("Source Use Type");
		sourceUseType.setName("sourceUseType");
		ToolTipHelper.add(sourceUseType,"Include source use types in output data");
		onRoadSCC = new JCheckBox("SCC");
		onRoadSCC.setName("onRoadSCC");
		onRoadSCC.addActionListener(this);
		ToolTipHelper.add(onRoadSCC,"Include SCC in output data");
		regClass = new JCheckBox("Regulatory Class");
		regClass.setName("regClass");
		ToolTipHelper.add(regClass,"Include regulatory class in output data");

		Dimension offRoadPanelSize = new Dimension(160, 100);
		offRoadPanel = new JPanel();
		offRoadPanel.setName("offRoadPanel");
		offRoadPanel.setBorder(BorderFactory.createTitledBorder(
				"Nonroad"));
		offRoadPanel.setPreferredSize(offRoadPanelSize);
		sector = new JCheckBox("Sector                ");
		sector.setName("sector");
		sector.setMnemonic('r');
		sector.setDisplayedMnemonicIndex(5);
		ToolTipHelper.add(sector,"Include sectors in output data");
		engTech = new JCheckBox("Engine Tech.");
		engTech.setName("engTech");
		ToolTipHelper.add(engTech,"Include nonroad engine tech in output data");
		hpClass = new JCheckBox("HP Class");
		hpClass.setName("hpClass");
		ToolTipHelper.add(hpClass,"Include HP class in output data");

		// Set fixed values and disabled fields
		sector.setSelected(true);
		sector.setEnabled(false);
		engTech.setSelected(true);
		engTech.setEnabled(false);
		hpClass.setSelected(true);
		hpClass.setEnabled(false);
		
		determineHelperText();
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;

 		alwaysPanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 4;
		gbc.weightx = 1;
		gbc.weighty = 0;

        LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
        alwaysPanel.add(timeLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc, 1, 0, "WEST", 1, 1);
		alwaysPanel.add(outputTimestepCombo, gbc);		
        LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
        alwaysPanel.add(geographicLabel, gbc);
        LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 1, 1);
		alwaysPanel.add(geographicOutputDetailCombo, gbc);
		LayoutUtility.setPositionOnGrid(gbc, 0, 2, "NORTH", 2, 6);
		alwaysPanel.add(helper, gbc);

		allPanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 4;
		gbc.weightx = 1;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 2, 1);
		allPanel.add(modelYear, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		allPanel.add(fuelType, gbc);
		if(CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT) {
			LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 1, 1);
			allPanel.add(fuelSubType, gbc);
		}
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 2, 1);
		allPanel.add(emissionProcess, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,3, "WEST", 2, 1);
		allPanel.add(onRoadSCC, gbc);

		onRoadPanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 3;
		gbc.weightx = 1;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		onRoadPanel.add(roadType, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		onRoadPanel.add(sourceUseType, gbc);

		if(CompilationFlags.DO_RATES_FIRST) {
			LayoutUtility.setPositionOnGrid(gbc,0,3, "WEST", 1, 1);
			onRoadPanel.add(regClass, gbc);
		}

		offRoadPanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 3;
		gbc.weightx = 1;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		offRoadPanel.add(sector, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		offRoadPanel.add(engTech, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		offRoadPanel.add(hpClass, gbc);

		setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 4;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "NORTH", 2, 2);
		add(alwaysPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,2,0, "NORTH", 1, 2);
		add(allPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,0, "NORTH", 1, 1);
		add(onRoadPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,1, "NORTH", 1, 1);
		add(offRoadPanel, gbc);
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == outputTimestepCombo) {
			determineHelperText();
		} else if (e.getSource() == geographicOutputDetailCombo) {
			determineHelperText();
		} else if(e.getSource() == onRoadSCC) {
			processOnRoadSCCButton(false);
		} else {
			enforceNonroadRules();
		}
	}

	/**
	 * Displays helper text depending on what has been selected in the
	 * outputTimestepCombo and geographicOutputDetailCombo
	**/
	public void determineHelperText() {
		if (helper == null) {
			return;
		}
		
		RunSpec runspec = MOVESAPI.getTheAPI().getRunSpec();
		String helperText = "";
		boolean isNonroad = runspec.models.contains(Model.NONROAD);
		
		// helper text for time aggregation (try block required because hasAllDays() can throw a SQL error)
		try {
			String selectedTimeStepDescription = "";
			OutputTimeStep selectedTimeStep = (OutputTimeStep)outputTimestepCombo.getSelectedItem();
			if(selectedTimeStep != null) {
				selectedTimeStepDescription = selectedTimeStep.toString();
			}
			
			if (selectedTimeStepDescription == OutputTimeStep.CLASSICAL_DAY.toString()) {
				helperText += "<p>\"24-hour day\" aggregates hourly output to estimate total emissions for a typical day for each month selected.</p>";
				if (!isNonroad && !runspec.timeSpan.hasAllHours()) {
					helperText += "<p style=\"color: red;\">Note: All hours need to be selected in the Time Spans panel for this time aggregation option.</p>";
				}
			} else if (selectedTimeStepDescription == OutputTimeStep.PORTION_OF_WEEK.toString()) {
				helperText += "<p>\"Portion of the week\" aggregates hourly output to estimate total emissions over a 2-day weekend and/or a 5-day work week for each month selected.</p>";
				if (!isNonroad && !runspec.timeSpan.hasAllHours()) {
					helperText += "<p style=\"color: red;\">Note: All hours need to be selected in the Time Spans panel for this time aggregation option.</p>";
				}
			} else if (selectedTimeStepDescription == OutputTimeStep.MONTH.toString()) {
				if (!isNonroad && (!runspec.timeSpan.hasAllHours() || !runspec.timeSpan.hasAllDays(null))) {
					helperText += "<p style=\"color: red;\">Note: All hours and days need to be selected in the Time Spans panel for this time aggregation option.</p>";
				}
			} else if (selectedTimeStepDescription == OutputTimeStep.YEAR.toString()) {
				if (!isNonroad && (!runspec.timeSpan.hasAllHours() || !runspec.timeSpan.hasAllDays(null) || !runspec.timeSpan.hasAllMonths())) {
					helperText += "<p style=\"color: red;\">Note: All hours, days, and months need to be selected in the Time Spans panel for this time aggregation option.</p>";
				}
			}
		} catch (SQLException e) {
			// nothing to do here
		}
		
		// helper text for geographic aggregation
		String selectedGeographicDescription = "";
		if(geographicOutputDetailCombo.getSelectedIndex() != -1) {
			selectedGeographicDescription = ((GeographicOutputDetailLevel)
					geographicOutputDetailCombo.getSelectedItem()).toString();
		}
		
		if (selectedGeographicDescription == GeographicOutputDetailLevel.NATION.toString()) {
			if (PreaggregationOptions.singleton.state.isSelected()) {
				helperText += "<p>Emissions will be calculated at the state level, but geographic detail will be aggregated away in the output with this option.</p>";
			} else if (PreaggregationOptions.singleton.county.isSelected()) {
				helperText += "<p>Emissions will be calculated at the county level, but geographic detail will be aggregated away in the output with this option.</p>";
			}
		} else if (selectedGeographicDescription == GeographicOutputDetailLevel.STATE.toString()) {
			if (PreaggregationOptions.singleton.county.isSelected()) {
				helperText += "<p>Emissions will be calculated at the county level, but geographic detail will be aggregated to the state level with this option. <span style=\"color: red;\">Note: If all counties in each state included in the run are not selected, the output will be for partial states.</span></p>";
			}
		}
		
		helper.setText("<html>" + helperText + "</html>");
	}

	/**
	 * Handles the On Road SCC checkbox click logic.
	 * @param pageReloaded false when a user has clicked the SCC button, true when called for a page load.
	**/
	public void processOnRoadSCCButton(boolean pageReloaded) {
		enforceNonroadRules();
		if(useNonroadRules) {
			// Nonroad SCC button behavior
			if(onRoadSCC.isSelected()) {
				sourceUseType.setSelected(false);
				sourceUseType.setEnabled(false);
				roadType.setSelected(false);
				roadType.setEnabled(false);
				/*
				if(!pageReloaded) {
					// Enable FuelType when the user clicks the SCC button.
					// Change nothing when merely loading a runspec.
					fuelType.setSelected(true);
					fuelSubType.setSelected(false);
				}
				fuelType.setEnabled(true);
				fuelSubType.setEnabled(true);
				*/
			} else {
				sourceUseType.setSelected(false);
				sourceUseType.setEnabled(false);
				roadType.setSelected(false);
				roadType.setEnabled(false);
				//fuelType.setEnabled(true);
				//fuelSubType.setEnabled(true);
			}
			// FuelSubType is only available when fuelType is checked.
			fuelSubType.setEnabled(fuelType.isSelected());
			if(!fuelType.isSelected()) {
				fuelSubType.setSelected(false);
			}
		} else {
			// Onroad SCC button behavior
			// When using onroad SCC, check and disable fuel, source, road, and process.
			if(onRoadSCC.isSelected()) {
				fuelType.setSelected(true);
				fuelType.setEnabled(false);
				sourceUseType.setSelected(true);
				sourceUseType.setEnabled(false);
				roadType.setSelected(true);
				roadType.setEnabled(false);
				emissionProcess.setSelected(true);
				emissionProcess.setEnabled(false);
			} else {
				fuelType.setEnabled(true);
				sourceUseType.setEnabled(true);
				roadType.setEnabled(!isMesoscaleLookup);
				if(isMesoscaleLookup) {
					roadType.setSelected(true);
				}
				emissionProcess.setEnabled(!isMesoscaleLookup);
			}
			// No fuelSubType for Onroad.
			fuelSubType.setSelected(false);
			fuelSubType.setEnabled(false);
		}
	}

	private boolean alreadyEnforcingNonroadRules = false;

	/** Update checkbox status based upon Nonroad's use cases. **/
	public void enforceNonroadRules() {
		if(!useNonroadRules) {
			if(modelYear != null) {
				modelYear.setEnabled(true);
				// No fuelSubType for Onroad.
				fuelSubType.setSelected(false);
				fuelSubType.setEnabled(false);
			}
		}
		if(!useNonroadRules || alreadyEnforcingNonroadRules) {
			return;
		}
		alreadyEnforcingNonroadRules = true;
		
		if(sourceUseType != null) {
			// Turn off and disable sourceUseType and roadType.
			sourceUseType.setSelected(false);
			sourceUseType.setEnabled(false);
			roadType.setSelected(false);
			roadType.setEnabled(false);

			onRoadSCC.setEnabled(true);
			sector.setEnabled(true);
			engTech.setEnabled(true);
			hpClass.setEnabled(true);
			modelYear.setEnabled(true);
			fuelType.setEnabled(true);
			emissionProcess.setEnabled(true);

			// FuelSubType is only available when fuelType is checked.
			fuelSubType.setEnabled(fuelType.isSelected());
			if(!fuelType.isSelected()) {
				fuelSubType.setSelected(false);
			}
		}
		alreadyEnforcingNonroadRules = false;
	}

	/**
	 * Saves the description text to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.outputEmissionsBreakdownSelection.modelYear = modelYear.isSelected();
		runspec.outputEmissionsBreakdownSelection.fuelType = fuelType.isSelected();
		runspec.outputEmissionsBreakdownSelection.fuelSubType = fuelSubType.isSelected();
		if(!runspec.outputEmissionsBreakdownSelection.fuelType) {
			runspec.outputEmissionsBreakdownSelection.fuelSubType = false;
		}
		if(runspec.outputEmissionsBreakdownSelection.fuelSubType) {
			runspec.outputEmissionsBreakdownSelection.fuelType = true;
		}

		if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			runspec.outputEmissionsBreakdownSelection.emissionProcess = true;
			runspec.outputEmissionsBreakdownSelection.estimateUncertainty = false;
		} else {
			runspec.outputEmissionsBreakdownSelection.emissionProcess = emissionProcess.isSelected();
		}
		// Uncertainty is disabled in this version
		runspec.outputEmissionsBreakdownSelection.estimateUncertainty = false;

		runspec.outputEmissionsBreakdownSelection.roadType = roadType.isSelected();
		runspec.outputEmissionsBreakdownSelection.regClassID = regClass.isSelected();
		runspec.outputEmissionsBreakdownSelection.sourceUseType = sourceUseType.isSelected();
		runspec.outputEmissionsBreakdownSelection.sector = sector.isSelected();
		runspec.outputEmissionsBreakdownSelection.engTechID = engTech.isSelected();
		runspec.outputEmissionsBreakdownSelection.hpClass = hpClass.isSelected();
		runspec.outputTimeStep = (OutputTimeStep)outputTimestepCombo.getSelectedItem();
		if(runspec.outputTimeStep != null) {
			runspec.outputFactors.timeMeasurementSystem =
					runspec.outputTimeStep.getTimeMeasurementSystemDefault();
		} else {
			runspec.outputFactors.timeMeasurementSystem = null;
		}

		if(runspec.domain != ModelDomain.PROJECT) {
			if(geographicOutputDetailCombo.getSelectedIndex() != -1) {
				runspec.geographicOutputDetail = (GeographicOutputDetailLevel)
						geographicOutputDetailCombo.getSelectedItem();
			}
		}

		//if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
		//	runspec.outputEmissionsBreakdownSelection.onRoadSCC = false;
		//} else {
			runspec.outputEmissionsBreakdownSelection.onRoadSCC = onRoadSCC.isSelected();
		//}
	}

	/**
	 * Loads from a RunSpec.
	 * @param	runspec the RunSpec from which data is extracted
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		if(!runspec.outputEmissionsBreakdownSelection.fuelType) {
			runspec.outputEmissionsBreakdownSelection.fuelSubType = false;
		}
		if(runspec.outputEmissionsBreakdownSelection.fuelSubType) {
			runspec.outputEmissionsBreakdownSelection.fuelType = true;
		}
		// Load Emission Breakdown Selections
		modelYear.setSelected(runspec.outputEmissionsBreakdownSelection.modelYear);
		fuelType.setSelected(runspec.outputEmissionsBreakdownSelection.fuelType);
		fuelSubType.setSelected(runspec.outputEmissionsBreakdownSelection.fuelSubType);
		if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			emissionProcess.setSelected(true);
			emissionProcess.setEnabled(false);
		} else {
			emissionProcess.setSelected(runspec.outputEmissionsBreakdownSelection.emissionProcess);
			emissionProcess.setEnabled(true);

		}
		Integer integerNumberOfIterations = Integer.valueOf(
				runspec.outputEmissionsBreakdownSelection.numberOfIterations);

		boolean hasNonroad = runspec.models.contains(Model.NONROAD);
		useNonroadRules = hasNonroad;
		sector.setEnabled(hasNonroad);
		engTech.setEnabled(hasNonroad);
		hpClass.setEnabled(hasNonroad);

		// Load the output time steps
		try {
			outputTimestepCombo.removeAllItems();
			if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
				outputTimestepCombo.addItem(OutputTimeStep.HOUR);
				runspec.outputTimeStep = OutputTimeStep.HOUR;
				lastOutputTimeStep = OutputTimeStep.HOUR;
				outputTimestepCombo.setSelectedItem(OutputTimeStep.HOUR);
			} else if(runspec.domain == ModelDomain.PROJECT) {
				outputTimestepCombo.addItem(OutputTimeStep.HOUR);
				runspec.outputTimeStep = OutputTimeStep.HOUR;
				lastOutputTimeStep = OutputTimeStep.HOUR;
				outputTimestepCombo.setSelectedItem(OutputTimeStep.HOUR);
			} else if(hasNonroad) {
				outputTimestepCombo.addItem(OutputTimeStep.CLASSICAL_DAY);
				runspec.outputTimeStep = OutputTimeStep.CLASSICAL_DAY;
				lastOutputTimeStep = OutputTimeStep.CLASSICAL_DAY;
				outputTimestepCombo.setSelectedItem(OutputTimeStep.CLASSICAL_DAY);
			} else {
				for(Iterator i = OutputTimeStep.allTypes.iterator(); i.hasNext();) {
					OutputTimeStep outputTimeStep = (OutputTimeStep)i.next();

					// Do not add timesteps that are less than the aggregate level.
					if(outputTimeStep.compareTo(runspec.timeSpan.aggregateBy) < 0) {
						continue;
					}

					/* Per EPA feedback 2003-09-10, allow the user to pick any option even
					   if they haven't chosen a large enough timespan for it.
					if(outputTimeStep.requiresAllHours() && !timeSpan.hasAllHours()) {
						continue;
					}
					if (outputTimeStep.requiresAllDays() && !timeSpan.hasAllDays()) {
						continue;
					}
					if (outputTimeStep.requiresAllMonths() && !timeSpan.hasAllMonths()) {
						continue;
					}
					*/
					outputTimestepCombo.addItem(outputTimeStep);
				}
			}
		} catch(NoSuchElementException e) {
			// If the time span information has not been entered, leave the TimeStep info blank.
		}

		if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			//onRoadSCC.setSelected(false);
			//onRoadSCC.setEnabled(false);
			onRoadSCC.setSelected(runspec.outputEmissionsBreakdownSelection.onRoadSCC);
			onRoadSCC.setEnabled(true);
		} else {
			lastOutputTimeStep = runspec.outputTimeStep;
			if(runspec.outputTimeStep != null) {
				if(runspec.outputTimeStep.compareTo(runspec.timeSpan.aggregateBy) < 0) {
					outputTimestepCombo.setSelectedItem(runspec.timeSpan.aggregateBy);
				} else {
					outputTimestepCombo.setSelectedItem(runspec.outputTimeStep);
				}
			}

			onRoadSCC.setSelected(runspec.outputEmissionsBreakdownSelection.onRoadSCC);
			onRoadSCC.setEnabled(true);
		}

		// onOffRoad is always selected in ghg1
		roadType.setSelected(runspec.outputEmissionsBreakdownSelection.roadType);
		sourceUseType.setSelected(runspec.outputEmissionsBreakdownSelection.sourceUseType);
		sector.setSelected(runspec.outputEmissionsBreakdownSelection.sector);
		engTech.setSelected(runspec.outputEmissionsBreakdownSelection.engTechID);
		hpClass.setSelected(runspec.outputEmissionsBreakdownSelection.hpClass);

		if(hasNonroad) {
			regClass.setEnabled(false);
			regClass.setSelected(false);
		} else {
			regClass.setEnabled(true);
			regClass.setSelected(runspec.outputEmissionsBreakdownSelection.regClassID);
		}

		onScaleChange(runspec,null);
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		determineHelperText();
		
		// This panel is acceptable even if there are no selections made, so there is nothing
		// to be checked here.
		boolean isOk = false;
		if(runspec.geographicOutputDetail == GeographicOutputDetailLevel.NATION
				|| runspec.geographicOutputDetail == GeographicOutputDetailLevel.STATE
				|| runspec.geographicOutputDetail == GeographicOutputDetailLevel.COUNTY
				|| runspec.geographicOutputDetail == GeographicOutputDetailLevel.ZONE
				|| runspec.geographicOutputDetail == GeographicOutputDetailLevel.LINK) {
			isOk = true;
		}

		if(isOk && runspec.outputTimeStep == null) {
			isOk = false;
		}

		if(isOk && runspec.scale != ModelScale.MESOSCALE_LOOKUP
				&& runspec.outputTimeStep.compareTo(runspec.timeSpan.aggregateBy) < 0) {
			isOk = false;
		}

		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		if(!isOk) {
			status.status = RunSpecSectionStatus.NOT_READY;
		}
		if(sections != null) {
			sections.remove(getName());
			sections.put(getName(),status);
		}
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
		runspec.outputEmissionsBreakdownSelection.modelYear = false;
		runspec.outputEmissionsBreakdownSelection.fuelType = false;
		runspec.outputEmissionsBreakdownSelection.fuelSubType = false;
		runspec.outputEmissionsBreakdownSelection.emissionProcess = false;
		runspec.outputEmissionsBreakdownSelection.estimateUncertainty =  false;
		runspec.outputEmissionsBreakdownSelection.numberOfIterations = 2;
		runspec.outputEmissionsBreakdownSelection.keepSampledData = false;
		runspec.outputEmissionsBreakdownSelection.keepIterations = false;
		runspec.outputEmissionsBreakdownSelection.onRoadOffRoad = false;
		runspec.outputEmissionsBreakdownSelection.roadType = false;
		runspec.outputEmissionsBreakdownSelection.regClassID = false;
		runspec.outputEmissionsBreakdownSelection.sourceUseType = false;
		runspec.outputEmissionsBreakdownSelection.onRoadSCC = false;
		runspec.outputEmissionsBreakdownSelection.sector = false;
		runspec.outputEmissionsBreakdownSelection.engTechID = false;
		runspec.outputEmissionsBreakdownSelection.hpClass = false;
		runspec.outputTimeStep = null;
		
		boolean isNonroad = runspec.models.contains(Model.NONROAD);

		if(ModelScale.MACROSCALE == runspec.scale) {
			geographicOutputDetailCombo.removeAllItems();
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.NATION);
			if(!isNonroad) { // disable STATE aggregation for NR
				geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.STATE);
			}
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.COUNTY);
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.ZONE);
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.LINK);
			geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.COUNTY);
			runspec.geographicOutputDetail = GeographicOutputDetailLevel.COUNTY;
		} else if(ModelScale.MESOSCALE_LOOKUP == runspec.scale) {
			geographicOutputDetailCombo.removeAllItems();
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.LINK);
			geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.LINK);
			runspec.geographicOutputDetail = GeographicOutputDetailLevel.LINK;

			runspec.outputEmissionsBreakdownSelection.roadType = true;
		}
		sections.remove(getName());

		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		sections.put(getName(),status);
		
		determineHelperText();
		
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
		boolean oldIsMesoscaleLookup = isMesoscaleLookup;
		if(ModelScale.MESOSCALE_LOOKUP == runspec.scale) {
			isMesoscaleLookup = true;
			geographicOutputDetailCombo.removeAllItems();
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.LINK);
			geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.LINK);
			geographicOutputDetailCombo.setEnabled(false);
			//onRoadSCC.setEnabled(false);
			//onRoadSCC.setSelected(false);
			onRoadSCC.setEnabled(true);
			roadType.setEnabled(false);
			roadType.setSelected(true);
			outputTimestepCombo.setEnabled(false);
		} else if(runspec.domain == ModelDomain.PROJECT) {
			geographicOutputDetailCombo.removeAllItems();
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.LINK);
			geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.LINK);
			geographicOutputDetailCombo.setEnabled(false);
			onRoadSCC.setEnabled(true);
			roadType.setEnabled(true);
			outputTimestepCombo.setEnabled(false);
		} else if(runspec.domain == ModelDomain.SINGLE_COUNTY) {
			isMesoscaleLookup = false;
			// at county scale, only let County or Zone be selected
			geographicOutputDetailCombo.removeAllItems();
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.COUNTY);
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.ZONE);
			if (runspec.geographicOutputDetail == GeographicOutputDetailLevel.COUNTY) {
				geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.COUNTY);
			} else if (runspec.geographicOutputDetail == GeographicOutputDetailLevel.ZONE) {
				geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.ZONE);
			}
		} else {
			isMesoscaleLookup = false;

			// Load Geographic Output Detail Combo Box
			boolean isNonroad = runspec.models.contains(Model.NONROAD);
			GeographicSelectionType geoType = null;
			Iterator i = runspec.geographicSelections.iterator();
			if(i.hasNext()) {
				geoType = ((GeographicSelection) i.next()).type;
			}
			if(geoType != null) {
				geographicOutputDetailCombo.removeAllItems();
				if(geoType == GeographicSelectionType.NATION) {
					geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.NATION);
				} else if (geoType == GeographicSelectionType.STATE) {
					geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.NATION);
					geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.STATE);
				} else if (geoType == GeographicSelectionType.COUNTY) {
					geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.NATION);
					if(!isNonroad) { // disable STATE aggregation for NR
						geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.STATE);
					}
					geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.COUNTY);
					geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.ZONE);
					if(ModelScale.MACROSCALE != runspec.scale) {
						geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.LINK);
					}
				}
			}

			if(runspec.geographicOutputDetail == GeographicOutputDetailLevel.NATION) {
				geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.NATION);
			} else if (runspec.geographicOutputDetail == GeographicOutputDetailLevel.STATE) {
				geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.STATE);
			} else if (runspec.geographicOutputDetail == GeographicOutputDetailLevel.COUNTY) {
				geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.COUNTY);
			} else if (runspec.geographicOutputDetail == GeographicOutputDetailLevel.ZONE) {
				geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.ZONE);
			} else if (runspec.geographicOutputDetail == GeographicOutputDetailLevel.LINK) {
				geographicOutputDetailCombo.setSelectedItem(GeographicOutputDetailLevel.LINK);
			}

			geographicOutputDetailCombo.setEnabled(true);
			onRoadSCC.setEnabled(true);
			roadType.setEnabled(true);
			outputTimestepCombo.setEnabled(true);
		}
		processOnRoadSCCButton(true);
		if(oldIsMesoscaleLookup != isMesoscaleLookup) {
			saveToRunSpec(runspec);
		}
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/**
	 * Sets the roadType check box to be selected or not
	 * @param toSelect true to select, false to unselect
	**/
	/*
	public void checkRoadType(boolean toSelect) {
		roadType.setSelected(toSelect);
	}
	*/

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
