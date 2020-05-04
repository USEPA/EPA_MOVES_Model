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
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.common.*;
import java.util.*;

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
 * @version		2015-03-16
**/
public class OutputEmissionsBreakdown extends JPanel implements ActionListener, RunSpecEditor {
	/** Panel contains "always" controls. **/
	JPanel alwaysPanel;
	/** Panel contains "all" controls. **/
	JPanel allPanel;
	/** Panel contains on and off road panels and controls. **/
	JPanel onOffRoadPanel;
	/** Panel contains on road controls. **/
	JPanel onRoadPanel;
	/** Panel contains off road controls. **/
	JPanel offRoadPanel;

	/** Panel with time options. **/
	JPanel timePanel;
	/** Time option checkbox. **/
	JCheckBox time;
	/** Time label. **/
	JLabel timeLabel;
	/** Output Timestep combo control. **/
	JComboBox<OutputTimeStep> outputTimestepCombo;
	/** Output Time Step when user entered the screen. **/
	OutputTimeStep lastOutputTimeStep;
	/** Panel with location options. **/
	JPanel locationPanel;
	/** Location option checkbox. **/
	JCheckBox location;
	/** Location label. **/
	JLabel locationLabel;
	/** Geographic output detail combo control. **/
	JComboBox<GeographicOutputDetailLevel> geographicOutputDetailCombo;
	/** Panel with pollutant options. **/
	JPanel pollutantPanel;
	/** Location option checkbox. **/
	JCheckBox pollutant;
	/** Pollutant label. **/
	JLabel pollutantLabel;
	/** Pollutant fill label. **/
	JLabel pollutantFillLabel;

	/** Model Year option checkbox. **/
	JCheckBox modelYear;
	/** Fuel Type option checkbox. **/
	JCheckBox fuelType;
	/** Fuel SubType option checkbox. **/
	JCheckBox fuelSubType;
	/** Emissions Process option checkbox. **/
	JCheckBox emissionProcess;

	/** Estimate Uncertainty option checkbox. **/
	JCheckBox estimateUncertainty;
	/** Panel with uncertainty options. **/
	JPanel uncertaintyPanel;
	/** Number of iterations label. **/
	JLabel numberOfIterationsLabel;
	/** Number of Iterations Field. **/
	JTextField numberOfIterations;
	/** Keep pseudo-randomly sampled data for each iteration. **/
	JCheckBox keepSampledData;
	/** Keep output data for each iteration. **/
	JCheckBox keepIterations;

	/** On Road/Off Road option checkbox. **/
	JCheckBox onOffRoad;
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

	/** true if the most recently loaded runspec uses mesoscale loop **/
	boolean isMesoscaleLookup = false;

	/**
	 *	Constructs a OutputEmissionsBreakdown panel, also creates and sets the layouts
	 *	of the controls.
	**/
	public OutputEmissionsBreakdown() {
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
		Dimension alwaysPanelSize = new Dimension(240, 110);
		alwaysPanel = new JPanel();
		alwaysPanel.setName("alwaysPanel");
		alwaysPanel.setBorder(BorderFactory.createTitledBorder(
				"Always"));
		alwaysPanel.setPreferredSize(alwaysPanelSize);
		timePanel = new JPanel();
		ToolTipHelper.add(timePanel,"Always generate time periods for the output data");
		time = new JCheckBox("");
		time.setName("time");
		timeLabel = new JLabel("Time");
		outputTimestepCombo = new JComboBox<OutputTimeStep>();
		outputTimestepCombo.setName("outputTimestepCombo");
		outputTimestepCombo.addActionListener(this);
		outputTimestepCombo.setEditable(false);
		outputTimestepCombo.setSelectedIndex(-1);
		ToolTipHelper.add(outputTimestepCombo,"Select the time step to be used for the output "
				+ "data");
		locationPanel = new JPanel();
		ToolTipHelper.add(locationPanel,"Always generate locations for output data");
		location = new JCheckBox("");
		location.setName("location");
		locationLabel = new JLabel("Location");
		geographicOutputDetailCombo = new JComboBox<GeographicOutputDetailLevel>();
		geographicOutputDetailCombo.setName("locationUnitsCombo");
		geographicOutputDetailCombo.addActionListener(this);
		geographicOutputDetailCombo.setEditable(false);
		ToolTipHelper.add(geographicOutputDetailCombo,"Select the geographic detail to be used for the "
				+ "output data");
		pollutantPanel = new JPanel();
		ToolTipHelper.add(pollutantPanel,"Always generate output pollutants");
		pollutant = new JCheckBox("");
		pollutant.setName("pollutant");
		pollutantLabel = new JLabel("Pollutant");
		pollutantFillLabel = new JLabel("                                                 ");

		Dimension allPanelSize = new Dimension(240, 135);
		allPanel = new JPanel();
		allPanel.setName("allPanel");
		allPanel.setBorder(BorderFactory.createTitledBorder(
				"for All Vehicle/Equipment Categories"));
		allPanel.setPreferredSize(allPanelSize);
		modelYear = new JCheckBox("Model Year");
		modelYear.setName("modelYear");
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

		Dimension uncertaintyPanelSize = new Dimension(408, 110);
		uncertaintyPanel = new JPanel();
		uncertaintyPanel.setBorder(BorderFactory.createTitledBorder(""));
		uncertaintyPanel.setPreferredSize(uncertaintyPanelSize);
		estimateUncertainty = new JCheckBox("Estimate Uncertainty");
		estimateUncertainty.setName("estimateUncertainty");
		estimateUncertainty.addActionListener(this);
		ToolTipHelper.add(estimateUncertainty,"Estimate uncertainty in output data");
		numberOfIterationsLabel = new JLabel("Number of iterations:");
		ToolTipHelper.add(numberOfIterationsLabel,"Number of iterations to include in estimate");
		numberOfIterations = new JTextField(5);
		ToolTipHelper.add(numberOfIterations,"Number of iterations to include in estimate");
		keepSampledData = new JCheckBox("Keep pseudo-randomly sampled input");
		ToolTipHelper.add(keepSampledData,"Keep pseudo-randomly generated data use in estimate");
		keepIterations = new JCheckBox("Keep output from each iteration");
		ToolTipHelper.add(keepIterations,"Keep the output of each iteration");

		Dimension onOffRoadPanelSize = new Dimension(165, 285 + (CompilationFlags.DO_RATES_FIRST? 20:0));
		onOffRoadPanel = new JPanel();
		onOffRoadPanel.setName("onOffRoadPanel");
		onOffRoadPanel.setBorder(BorderFactory.createTitledBorder(
				"On Road/Off Road"));
		onOffRoadPanel.setPreferredSize(onOffRoadPanelSize);
		onOffRoad = new JCheckBox("On Road/Off Road");
		onOffRoad.setName("onOffRoad");
		ToolTipHelper.add(onOffRoad,"Include on and off road results");

		Dimension onRoadPanelSize = new Dimension(160, 100 + (CompilationFlags.DO_RATES_FIRST? 20:0));
		onRoadPanel = new JPanel();
		onRoadPanel.setName("onRoadPanel");
		onRoadPanel.setBorder(BorderFactory.createTitledBorder(
				"On and Off Road"));
		onRoadPanel.setPreferredSize(onRoadPanelSize);
		roadType = new JCheckBox("Road Type");
		roadType.setName("roadType");
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
				"Off Road"));
		offRoadPanel.setPreferredSize(offRoadPanelSize);
		sector = new JCheckBox("Sector                ");
		sector.setName("sector");
		ToolTipHelper.add(sector,"Include sectors in output data");
		engTech = new JCheckBox("Engine Tech.");
		engTech.setName("engTech");
		ToolTipHelper.add(engTech,"Include off road engine tech in output data");
		hpClass = new JCheckBox("HP Class");
		hpClass.setName("hpClass");
		ToolTipHelper.add(hpClass,"Include HP class in output data");

		// Set fixed values and disabled fields
		onOffRoad.setSelected(true);
		onOffRoad.setEnabled(false);
		time.setSelected(true);
		time.setEnabled(false);
		location.setSelected(true);
		location.setEnabled(false);
		pollutant.setSelected(true);
		pollutant.setEnabled(false);
		sector.setSelected(true);
		sector.setEnabled(false);
		engTech.setSelected(true);
		engTech.setEnabled(false);
		hpClass.setSelected(true);
		hpClass.setEnabled(false);
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;

 		alwaysPanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 3;
		gbc.weightx = 1;
		gbc.weighty = 0;

		timePanel.setLayout(new BoxLayout(timePanel, BoxLayout.X_AXIS));
		timePanel.add(time);
		timePanel.add(timeLabel);
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		alwaysPanel.add(timePanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,0, "WEST", 1, 1);
		alwaysPanel.add(outputTimestepCombo, gbc);
		locationPanel.setLayout(new BoxLayout(locationPanel, BoxLayout.X_AXIS));
		locationPanel.add(location);
		locationPanel.add(locationLabel);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		alwaysPanel.add(locationPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 1, 1);
		alwaysPanel.add(geographicOutputDetailCombo, gbc);
		pollutantPanel.setLayout(new BoxLayout(pollutantPanel, BoxLayout.X_AXIS));
		pollutantPanel.add(pollutant);
		pollutantPanel.add(pollutantLabel);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		alwaysPanel.add(pollutantPanel, gbc);

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
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		onRoadPanel.add(onRoadSCC, gbc);

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

		onOffRoadPanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 3;
		gbc.weightx = 1;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "CENTER", 1, 1);
		onOffRoadPanel.add(onOffRoad, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "CENTER", 1, 1);
		onOffRoadPanel.add(onRoadPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "CENTER", 1, 1);
		onOffRoadPanel.add(offRoadPanel, gbc);

		uncertaintyPanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 4;
		gbc.gridheight = 3;
		gbc.weightx = 1;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		uncertaintyPanel.add(numberOfIterationsLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,0, "WEST", 3, 1);
		uncertaintyPanel.add(numberOfIterations, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 4, 1);
		uncertaintyPanel.add(keepSampledData, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 4, 1);
		uncertaintyPanel.add(keepIterations, gbc);

		setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 4;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		add(alwaysPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		add(allPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 3, 1);
		add(estimateUncertainty, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,0, "WEST", 1, 3);
		add(onOffRoadPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,3, "WEST", 3, 1);
		add(uncertaintyPanel, gbc);
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == onRoadSCC) {
			processOnRoadSCCButton(false);
		} else if(e.getSource() == estimateUncertainty) {
			processEstimateUncertaintyCheckBox();
		} else {
			enforceNonroadRules();
		}
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

	/** Handles the Estimate Uncertainty checkbox click logic. **/
	public void processEstimateUncertaintyCheckBox() {
		uncertaintyPanel.setEnabled(estimateUncertainty.isSelected());
		numberOfIterationsLabel.setEnabled(estimateUncertainty.isSelected());
		numberOfIterations.setEnabled(estimateUncertainty.isSelected());
		keepSampledData.setEnabled(estimateUncertainty.isSelected());
		keepIterations.setEnabled(estimateUncertainty.isSelected());
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
			runspec.outputEmissionsBreakdownSelection.estimateUncertainty =
					estimateUncertainty.isSelected();
		}
		// Uncertainty is disabled in this version
		runspec.outputEmissionsBreakdownSelection.estimateUncertainty = false;

		int intNumberOfIterations;
		try {
			intNumberOfIterations = Integer.parseInt(numberOfIterations.getText());
			if(intNumberOfIterations<2) {
				intNumberOfIterations = 2;
			}
		} catch (NumberFormatException e) {
			intNumberOfIterations = 2;
		}
		numberOfIterations.setText(Integer.toString(intNumberOfIterations));
		runspec.outputEmissionsBreakdownSelection.numberOfIterations = intNumberOfIterations;
		runspec.outputEmissionsBreakdownSelection.keepSampledData = keepSampledData.isSelected();
		runspec.outputEmissionsBreakdownSelection.keepIterations = keepIterations.isSelected();
		runspec.outputEmissionsBreakdownSelection.onRoadOffRoad = onOffRoad.isSelected();
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

			estimateUncertainty.setSelected(false);
			estimateUncertainty.setEnabled(false);
		} else {
			emissionProcess.setSelected(runspec.outputEmissionsBreakdownSelection.emissionProcess);
			emissionProcess.setEnabled(true);

			// Uncertainty is disabled in this version
			estimateUncertainty.setSelected(false);
			estimateUncertainty.setEnabled(false);
			/*
			estimateUncertainty.setSelected(
					runspec.outputEmissionsBreakdownSelection.estimateUncertainty);
			// enable/disable uncertainty estimation if output is being aggregated.
			if(runspec.timeSpan.aggregateBy.compareTo(OutputTimeStep.HOUR) != 0) {
				estimateUncertainty.setSelected(false);
				estimateUncertainty.setEnabled(false);
			} else {
				estimateUncertainty.setEnabled(true);
				GeographicSelectionType geoType = null;
				Iterator i = runspec.geographicSelections.iterator();
				if(i.hasNext()) {
					geoType = ((GeographicSelection) i.next()).type;
				}
				if(geoType != null) {
					if(geoType == GeographicSelectionType.NATION
							|| geoType == GeographicSelectionType.STATE) {
						estimateUncertainty.setSelected(false);
						estimateUncertainty.setEnabled(false);
					}
				}
			}
			*/
		}
		Integer integerNumberOfIterations = new Integer(
				runspec.outputEmissionsBreakdownSelection.numberOfIterations);
		numberOfIterations.setText(integerNumberOfIterations.toString());
		keepSampledData.setSelected(runspec.outputEmissionsBreakdownSelection.keepSampledData);
		keepIterations.setSelected(runspec.outputEmissionsBreakdownSelection.keepIterations);
		uncertaintyPanel.setEnabled(estimateUncertainty.isSelected());
		numberOfIterationsLabel.setEnabled(estimateUncertainty.isSelected());
		numberOfIterations.setEnabled(estimateUncertainty.isSelected());
		keepSampledData.setEnabled(estimateUncertainty.isSelected());
		keepIterations.setEnabled(estimateUncertainty.isSelected());

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
		onOffRoad.setSelected(true);
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

		if(ModelScale.MACROSCALE == runspec.scale) {
			geographicOutputDetailCombo.removeAllItems();
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.NATION);
			geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.STATE);
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
		} else {
			isMesoscaleLookup = false;

			// Load Geographic Output Detail Combo Box
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
					geographicOutputDetailCombo.addItem(GeographicOutputDetailLevel.STATE);
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
