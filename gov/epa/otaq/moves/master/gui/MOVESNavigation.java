/**************************************************************************************************
 * @(#)MOVESNavigation.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import gov.epa.otaq.moves.common.CompilationFlags;
import gov.epa.otaq.moves.common.ModelDomain;
import gov.epa.otaq.moves.common.ModelScale;
import gov.epa.otaq.moves.common.Models;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.TreeMap;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.ToolTipManager;

/**
 * Class for MOVES MOVESNavigation panel. <br>
 * <br>
 * <b>IMPORTANT:</b> This class relies upon the fact that the MOVESWindow has
 * already instantiated all panels needed for the options, so only instantiate
 * one of these after the panels have have been setup. <br>
 * This class Constructs the MOVESNavigation panel. Creates, initializes, and
 * sets the layouts of the following controls, Description, Scale, Macroscale
 * Geographic Bounds, TimeSpans , Vehicles Equipment, OnRoadVehicleEquipment,
 * RoadType, PollutantsAndProcesses, ManageInputDataSets. Output,
 * OutputEmissionsBreakdown, GeneralOutput, AdvancedPerformanceFeatures
 * 
 * @author Wesley Faler
 * @author Tim Hull
 * @version 2014-01-15
 **/
public class MOVESNavigation extends JPanel implements ActionListener,
		ItemListener {
	/** MOVESWindow that this navigation panel is within **/
	public MOVESWindow parent;
	/** Singleton for the navigation panel **/
	public static MOVESNavigation singleton = null;

	/** Panel color. **/
	Color panelColor;
	/** Text color. **/
	Color textColor;
	/** Description option checkbox. **/
	JRadioButton descriptionOption;
	/** Scale option checkbox. **/
	JRadioButton scaleOption;
	/** Macroscale Geographic Bounds option checkbox. **/
	JRadioButton macroscaleGeographicBoundsOption;
	/** TimeSpans option checkbox. **/
	JRadioButton timeSpansOption;
	/** Vehicles Equipment option checkbox. **/
	JCheckBox vehiclesEquipmentOption;
	/** Vehicles Equipment image icon. **/
	ImageIcon vehiclesEquipmentImage;
	/** OnRoadVehicleEquipment option checkbox. **/
	JRadioButton onRoadVehicleEquipmentOption;
	/** OffRoadVehicleEquipment option checkbox. **/
	JRadioButton offRoadVehicleEquipmentOption;
	/** RoadType option checkbox. **/
	JRadioButton roadTypeOption;
	/** PollutantsAndProcesses option checkbox. **/
	JRadioButton pollutantsAndProcessesOption;
	/** ManageInputDataSets option checkbox. **/
	JRadioButton manageInputDataSetsOption;
	/** OutputEmissionsBreakdown option checkbox. **/
	JRadioButton outputEmissionsBreakdownOption;
	/** GeneralOutput option checkbox. **/
	JRadioButton generalOutputOption;
	/** AdvancedPerformanceFeatures option **/
	JRadioButton advancedPerformanceFeaturesOption;
	/** Output option checkbox. **/
	JCheckBox outputOption;
	/** Output image icon. **/
	ImageIcon outputImage;
	/**
	 * Radio button group for the options, so only one option is selected at a
	 * time.
	 **/
	JCheckBox strategyOption;
	/** Output image icon. **/
	ImageIcon strategyImage;
	/**
	 * List of JRadioButton objects, each associated with one type of
	 * InternalControlStrategy
	 **/
	public LinkedList<JRadioButton> strategyOptions = new LinkedList<JRadioButton>();
	/** ButtonGroup definition **/
	ButtonGroup group;
	/**
	 * true when the next call to setNavigationSelection should be ignored. Used
	 * to avoid display of deprecated panels.
	 **/
	boolean ignoreNextSetNavigationSelection = false;
	/**
	 * true if a File|New operation has already been done, used to control
	 * splash screen display
	 **/
	boolean hasDoneFileNew = false;

	/** The RunSpecEditor object currently in use in the GUI **/
	RunSpecEditor activeEditor = null;
	/** Used with the parent's mouse event handling **/
	JRadioButton lastRadioButtonOption = null;
	/** Used with the parent's mouse event handling **/
	RunSpecEditor lastRunSpecEditor = null;

	/**
	 * Collection of JPanel objects in the order that they are displayed.
	 **/
	public LinkedList<JPanel> panels = new LinkedList<JPanel>();
	/**
	 * Collection of JRadioButton objects for the options in the order that they
	 * are displayed
	 **/
	LinkedList<JRadioButton> options = new LinkedList<JRadioButton>();
	/**
	 * Quick lookup of standard text for each option. Allows colored text to be
	 * dynamically generated. Keyed by the name of the option's radio button.
	 **/
	TreeMap<String, String> textForOptions = new TreeMap<String, String>();
	/**
	 * Quick lookup for the panel to show for each option. Allows generic panel
	 * handling. Keyed by the name of the option's radio button.
	 **/
	TreeMap<String, JPanel> panelForOptions = new TreeMap<String, JPanel>();
	/**
	 * Status of each option as calculated by RunSpecEditor. Holds
	 * RunSpecSectionStatus objects keyed by the JPanel/RunSpecEditor that set
	 * it.
	 **/
	TreeMap<String, RunSpecSectionStatus> optionStatuses = new TreeMap<String, RunSpecSectionStatus>();
	/**
	 * Icon type of each option. Used to determine if the option needs a normal
	 * or a wide icon. Keyed by the option name with data as an Integer.
	 **/
	TreeMap<String, Integer> optionIconTypes = new TreeMap<String, Integer>();

	/**
	 * Internal class representing human-displayable description of an
	 * InternalControlStrategy
	 **/
	class InternalControlStrategyDescriptor {
		public String humanDescription;
		public String className;

		public InternalControlStrategyDescriptor(String descriptionToUse,
				String classNameToUse) {
			humanDescription = descriptionToUse;
			className = classNameToUse;
		}
	};

	/** Information about which InternalControlStrategy classes to be displayed **/
	InternalControlStrategyDescriptor[] internalControlStrategyDescriptors = {
			new InternalControlStrategyDescriptor(
					"Alternative Vehicle Fuels & Technologies",
					"gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.avft.AVFTControlStrategy"),
			new InternalControlStrategyDescriptor(
					"On-Road Retrofit",
					"gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit.OnRoadRetrofitStrategy"),
			new InternalControlStrategyDescriptor(
					"Rate Of Progress",
					"gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress.RateOfProgressStrategy") };

	/** Keep a pointer to the global runSpec object **/
	// RunSpec runSpec = null;

	/**
	 * Constructs the MOVESNavigation panel, also creates, initializes, and sets
	 * the layouts of the controls.
	 * 
	 * @param p
	 *            the MOVESWindow that is the parent of this object.
	 **/
	public MOVESNavigation(MOVESWindow p) {
		parent = p;
		singleton = this;

		createControls();
		initControls();
		arrangeControls();
	}

	/**
	 * Set the run spec
	 * 
	 * @param runSpec
	 *            0f RunSpec
	 */
	// public void setRunSpec( RunSpec runSpec) {
	// this.runSpec = runSpec;
	// }

	/** Creates and initializes all controls on this panel. **/
	void createControls() {
		// Set Up colors
		panelColor = new Color(98, 176, 255);
		textColor = new Color(0, 0, 0);

		group = new ButtonGroup();

		// Set up navigation option buttons (radio buttons since only one is
		// selected at a time)
		descriptionOption = createOption("descriptionOption", group,
				"Description", parent.descriptionPanel,
				RunSpecSectionStatus.NORMAL);
		scaleOption = createOption("scaleOption", group, "Scale",
				parent.scalePanel, RunSpecSectionStatus.NORMAL);
		timeSpansOption = createOption("timeSpansOption", group, "Time Spans",
				parent.timeSpansPanel, RunSpecSectionStatus.NORMAL);
		macroscaleGeographicBoundsOption = createOption(
				"macroscaleGeographicBoundsOption", group, "Geographic Bounds",
				parent.macroscaleGeographicBoundsPanel,
				RunSpecSectionStatus.NORMAL);

		vehiclesEquipmentImage = new ImageIcon(
				"gov/epa/otaq/moves/master/gui/images/treeClosed.gif");
		vehiclesEquipmentOption = new JCheckBox("Vehicles/Equipment",
				vehiclesEquipmentImage);
		vehiclesEquipmentOption.setName("vehiclesEquipmentOption");
		vehiclesEquipmentOption.setBackground(panelColor);
		vehiclesEquipmentOption.setForeground(textColor);
		vehiclesEquipmentOption.addActionListener(this);
		ToolTipManager.sharedInstance().registerComponent(
				vehiclesEquipmentOption);

		onRoadVehicleEquipmentOption = createOption(
				"onRoadVehicleEquipmentOption", group,
				"On Road Vehicle Equipment",
				parent.onRoadVehicleEquipmentPanel, RunSpecSectionStatus.WIDE);
		if (CompilationFlags.USE_NONROAD) {
			offRoadVehicleEquipmentOption = createOption(
					"offRoadVehicleEquipmentOption", group,
					"NonRoad Vehicle Equipment",
					parent.offRoadVehicleEquipmentPanel,
					RunSpecSectionStatus.WIDE);
		}
		updateVehiclesEquipmentOptionIcon();

		roadTypeOption = createOption("roadTypeOption", group, "Road Type",
				parent.roadTypePanel, RunSpecSectionStatus.NORMAL);
		pollutantsAndProcessesOption = createOption(
				"pollutantsAndProcessesOption", group,
				"Pollutants And Processes", parent.pollutantsAndProcessesPanel,
				RunSpecSectionStatus.NORMAL);

		strategyImage = new ImageIcon(
				"gov/epa/otaq/moves/master/gui/images/treeClosed.gif");
		strategyOption = new JCheckBox("Strategies", outputImage);
		strategyOption.setName("strategyOption");
		strategyOption.setBackground(panelColor);
		strategyOption.setForeground(textColor);
		strategyOption.addActionListener(this);
		ToolTipManager.sharedInstance().registerComponent(strategyOption);
		for (int i = 0; i < internalControlStrategyDescriptors.length; i++) {
			InternalControlStrategies panel = new InternalControlStrategies();
			panel.setName(internalControlStrategyDescriptors[i].className);
			panel.finishSetup();
			JRadioButton option = createOption(
					internalControlStrategyDescriptors[i].className, group,
					internalControlStrategyDescriptors[i].humanDescription,
					panel, RunSpecSectionStatus.WIDE);
			strategyOptions.add(option);
		}

		updateStrategyOptionIcon();

		manageInputDataSetsOption = createOption("manageInputDataSetsOption",
				group, "Manage Input Data Sets",
				parent.manageInputDataSetsPanel, RunSpecSectionStatus.NORMAL);

		outputImage = new ImageIcon(
				"gov/epa/otaq/moves/master/gui/images/treeClosed.gif");
		outputOption = new JCheckBox("Output", outputImage);
		outputOption.setName("outputOption");
		outputOption.setBackground(panelColor);
		outputOption.setForeground(textColor);
		outputOption.addActionListener(this);
		ToolTipManager.sharedInstance().registerComponent(outputOption);

		generalOutputOption = createOption("generalOutputOption", group,
				"General Output", parent.generalOutputPanel,
				RunSpecSectionStatus.WIDE);
		outputEmissionsBreakdownOption = createOption(
				"outputEmissionsBreakdownOption", group,
				"Output Emissions Detail",
				parent.outputEmissionsBreakdownPanel, RunSpecSectionStatus.WIDE);

		updateOutputOptionIcon();

		advancedPerformanceFeaturesOption = createOption(
				"advancedPerformanceFeaturesOption", group,
				"Advanced Performance Features",
				parent.advancedPerformancePanel, RunSpecSectionStatus.NORMAL);
	}

	/**
	 * Utility function to build a selectable option. Not used for tree nodes,
	 * only for options that open panels.
	 * 
	 * @return a JRadioButton attached to a ButtonGroup and setup for event
	 *         handling
	 * @param name
	 *            the value passed to setName for the new JRadioButton
	 * @param g
	 *            the ButtonGroup the new option belongs to
	 * @param text
	 *            the plain (uncolored) wording to be put on the option
	 * @param panel
	 *            panel (created by parent MOVESWindow) to be shown when option
	 *            is selected
	 * @param iconSubType
	 *            icon type for passing to RunSpecSectionStatus.getIcon()
	 **/
	JRadioButton createOption(String name, ButtonGroup g, String text,
			JPanel panel, int iconSubType) {
		RunSpecSectionStatus defaultStatus = new RunSpecSectionStatus(
				RunSpecSectionStatus.DEFAULTS);
		Icon image = defaultStatus.getIcon(iconSubType);
		if (panel instanceof RunSpecEditor) {
			RunSpecEditor editor = (RunSpecEditor) panel;
			RunSpecSectionStatus status = editor.saveDefaultsToRunSpec(
					parent.runSpec, optionStatuses);
			image = status.getIcon(iconSubType);
		}
		JRadioButton option = new JRadioButton("<html>" + text + "</html>",
				image);
		// NOTE: The <html> tags are used here so that their use for highlighted
		// ----- items will be faster. The first use of HTML in text incurs a
		// delay,
		// so best to get it out of the way sooner than when the user is
		// watching.
		option.setName(name);
		option.setBackground(panelColor);
		option.setForeground(textColor);
		option.addItemListener(this);
		ToolTipManager.sharedInstance().registerComponent(option);
		option.setToolTipText(RunSpecSectionStatus.explainIcon(option.getIcon()));

		panels.add(panel);
		options.add(option);
		g.add(option);
		textForOptions.put(name, text);
		panelForOptions.put(name, panel);
		optionIconTypes.put(name, new Integer(iconSubType));

		return option;
	}

	/** Some extra controls initialization on this panel. **/
	void initControls() {
		// hide collapsed tree options
		onRoadVehicleEquipmentOption.setVisible(false);
		if (CompilationFlags.USE_NONROAD) {
			offRoadVehicleEquipmentOption.setVisible(false);
		}
		outputEmissionsBreakdownOption.setVisible(false);
		generalOutputOption.setVisible(false);
		for (Iterator i = strategyOptions.iterator(); i.hasNext();) {
			JRadioButton option = (JRadioButton) i.next();
			option.setVisible(false);
		}
	}

	/** Sets the layout of the controls. **/
	void arrangeControls() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBackground(panelColor);

		add(descriptionOption);
		add(scaleOption);
		add(timeSpansOption);
		add(macroscaleGeographicBoundsOption);
		add(vehiclesEquipmentOption);
		add(onRoadVehicleEquipmentOption);
		if (CompilationFlags.USE_NONROAD) {
			add(offRoadVehicleEquipmentOption);
		}
		add(roadTypeOption);
		add(pollutantsAndProcessesOption);
		add(manageInputDataSetsOption);
		add(strategyOption);
		for (Iterator<JRadioButton> i = strategyOptions.iterator(); i.hasNext();) {
			JRadioButton option = i.next();
			add(option);
		}
		add(outputOption);
		add(generalOutputOption);
		add(outputEmissionsBreakdownOption);
		add(advancedPerformanceFeaturesOption);
	}

	/**
	 * Listener method, calls the appropriate button handler.
	 * 
	 * @param e
	 *            the ActionEvent to be handled.
	 **/
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == outputOption) {
			processOutputButton();
		} else if (e.getSource() == vehiclesEquipmentOption) {
			processVehiclesEquipmentButton();
		} else if (e.getSource() == strategyOption) {
			processStrategyButton();
		}
	}

	/**
	 * Helper function used during the new file action. Sets the defaults to the
	 * RunSpec and loads these into the corresponding RunSpecEditor panels.
	 **/
	public void onFileNew() {
		Models.ModelCombination mc = Models.ModelCombination.M1;
		if (parent != null && parent.runSpec != null) {
			mc = parent.runSpec.getModelCombination();
		}
		for (Iterator<JRadioButton> i = options.iterator(); i.hasNext();) {
			JRadioButton option = (i.next());
			if (onRoadVehicleEquipmentOption != null && option == onRoadVehicleEquipmentOption) {
				switch (mc) {
				case M1:
				case M12:
				default:
					updateOptionDefault(option);
					break;
				case M2:
					break;
				}
			} else if (offRoadVehicleEquipmentOption != null && option == offRoadVehicleEquipmentOption) {
				switch (mc) {
				case M2:
				case M12:
					updateOptionDefault(option);
					break;
				default:
					break;
				}
			} else {
				updateOptionDefault(option);
			}
		}

		updateOutputOptionIcon();
		updateVehiclesEquipmentOptionIcon();
		updateStrategyOptionIcon();

		if (hasDoneFileNew) {
			checkStrategyAfterFileChange();
		} else {
			hasDoneFileNew = true;
		}
		
		parent.checkImporterActions();
		parent.checkExecuteAction();
	}

	private void updateOptionDefault(JRadioButton option) {
		JPanel p = panelForOptions.get(option.getName());
		if (p instanceof RunSpecEditor) {
			RunSpecEditor editor = (RunSpecEditor) p;
			RunSpecSectionStatus status = editor.saveDefaultsToRunSpec(
					parent.runSpec, optionStatuses);
			editor.loadFromRunSpec(parent.runSpec);
			int iconSubType = optionIconTypes.get(option.getName()).intValue();
			option.setIcon(status.getIcon(iconSubType));
			option.setToolTipText(RunSpecSectionStatus.explainIcon(option
					.getIcon()));
		}
	}

	/**
	 * Helper function used during any of the open file actions. Calculates the
	 * RunSpec section status and loads the RunSpec data into the corresponding
	 * RunSpecEditor panels.
	 **/
	public void onFileOpen() {
		Models.ModelCombination mc = Models.ModelCombination.M1;
		if (parent != null && parent.runSpec != null) {
			mc = parent.runSpec.getModelCombination();
		}
		for (Iterator<JRadioButton> i = options.iterator(); i.hasNext();) {
			JRadioButton option = (i.next());
			if (onRoadVehicleEquipmentOption != null && option == onRoadVehicleEquipmentOption) {
				switch (mc) {
				case M1:
				case M12:
				default:
					updateOption(option);
					break;
				case M2:
					break;
				}
			} else if (offRoadVehicleEquipmentOption != null && option == offRoadVehicleEquipmentOption) {
				switch (mc) {
				case M2:
				case M12:
					updateOption(option);
					break;
				default:
					break;
				}
			} else {
				updateOption(option);
			}

		}
		updateOutputOptionIcon();
		updateVehiclesEquipmentOptionIcon();
		updateStrategyOptionIcon();
		// Set the execute action enabled state in the parent window
		parent.checkExecuteAction();
		parent.checkImporterActions();

		checkStrategyAfterFileChange();
	}

	private void updateOption(JRadioButton option) {
		JPanel p = panelForOptions.get(option.getName());
		if (p instanceof RunSpecEditor) {
			RunSpecEditor editor = (RunSpecEditor) p;
			RunSpecSectionStatus status = editor.calculateRunSpecSectionStatus(
					parent.runSpec, optionStatuses);
			editor.loadFromRunSpec(parent.runSpec);
			int iconSubType = optionIconTypes.get(option.getName()).intValue();
			option.setIcon(status.getIcon(iconSubType));
			option.setToolTipText(RunSpecSectionStatus.explainIcon(option
					.getIcon()));
		}
	}

	/**
	 * Update the displayed strategy options after File|Open or File|New, taking
	 * care not to show a deprecated panel.
	 **/
	void checkStrategyAfterFileChange() {
		processStrategyButton();

		boolean shouldJump = true;
		if (lastRadioButtonOption != null && lastRadioButtonOption.isVisible()) {
			shouldJump = false;
		}
		if (shouldJump) {
			// Set the focus to the Description option
			group.setSelected(descriptionOption.getModel(), true);
			ignoreNextSetNavigationSelection = true;
		}
	}

	/**
	 * Listener method, watches for options to become selected/unselected and
	 * modifies their text accordingly so that the current option is
	 * highlighted.
	 * 
	 * @param e
	 *            the ItemEvent to be handled
	 **/
	@Override
	public void itemStateChanged(ItemEvent e) {
		JRadioButton option = (JRadioButton) e.getItem();
		lastRadioButtonOption = option;
		setRadioButtonHighlighting(option,
				e.getStateChange() == ItemEvent.SELECTED);

		JPanel p = panelForOptions.get(option.getName());
		if (p instanceof RunSpecEditor) {
			RunSpecEditor editor = (RunSpecEditor) p;
			lastRunSpecEditor = editor;
			if (e.getStateChange() == ItemEvent.SELECTED) {
				if (editor != activeEditor) {
					activeEditor = editor;
					editor.loadFromRunSpec(parent.runSpec);
				}
			}
			updateRunSpecSectionStatus(option, editor, false);
		}
		if (e.getStateChange() == ItemEvent.SELECTED) {
			parent.rightScrollPane.setViewportView(p);
		}
	}

	/**
	 * Used to update the current RunSpecEditor's icon based on its status.
	 **/
	public void updateRunSpecSectionStatus() {
		updateRunSpecSectionStatus(lastRadioButtonOption, lastRunSpecEditor,
				false);
	}

	/**
	 * Used to update a RunSpecEditor's icon based on its status.
	 * 
	 * @param option
	 *            The RunSpecEditor's corresponding JRadioButton control in the
	 *            nav panel.
	 * @param editor
	 *            The RunSpecEditor, may be null.
	 * @param scaleDidChange
	 *            true if the should be notified of a changed scale.
	 **/
	public void updateRunSpecSectionStatus(JRadioButton option,
			RunSpecEditor editor, boolean scaleDidChange) {
		if (editor == null && option != null) {
			JPanel p = panelForOptions.get(option.getName());
			if (p instanceof RunSpecEditor) {
				editor = (RunSpecEditor) p;
			}
		}
		if (option == null || editor == null) {
			return;
		}
		RunSpecSectionStatus status = null;
		if (scaleDidChange) {
			// equipment radio buttons need to be updated
			this.processVehiclesEquipmentButton();

			status = editor.onScaleChange(parent.runSpec, optionStatuses);
			if (status.status == RunSpecSectionStatus.OK) {
				status.status = RunSpecSectionStatus.DEFAULTS;
				JPanel p = panelForOptions.get(option.getName());
				optionStatuses.put(p.getName(), status);
			}
		} else {
			editor.saveToRunSpec(parent.runSpec);
			status = editor.calculateRunSpecSectionStatus(parent.runSpec,
					optionStatuses);
		}
		int iconSubType = optionIconTypes.get(option.getName()).intValue();
		option.setIcon(status.getIcon(iconSubType));
		option.setToolTipText(RunSpecSectionStatus.explainIcon(option.getIcon()));
		// Handle special cases for updating the vehicles/equipment tree nodes
		if (option == onRoadVehicleEquipmentOption
				|| (CompilationFlags.USE_NONROAD && option == offRoadVehicleEquipmentOption)) {
			updateVehiclesEquipmentOptionIcon();
		}
		// Handle special cases for updating the control strategies tree nodes
		for (Iterator i = strategyOptions.iterator(); i.hasNext();) {
			JRadioButton listedOption = (JRadioButton) i.next();
			if (option == listedOption) {
				updateStrategyOptionIcon();
				break;
			}
		}
		// Handle special cases for updating the output tree nodes
		if (option == outputEmissionsBreakdownOption
				|| option == generalOutputOption) {
			updateOutputOptionIcon();
		}
		// Also check if all RunSpecEditors are ready
		parent.checkExecuteAction();
		parent.checkImporterActions();
	}

	/** Commits any changes in the active panel to the current RunSpec **/
	public void commitActiveEditor() {
		if (activeEditor != null) {
			activeEditor.saveToRunSpec(parent.runSpec);
		}
	}

	/** Reload the active panel from the current RunSpec **/
	public void reloadActiveEditor() {
		if (activeEditor != null) {
			activeEditor.loadFromRunSpec(parent.runSpec);
		}
	}

	/**
	 * Sets the active navigation selection. This will invoke item highlighting
	 * and commit necessary changes to the current RunSpec.
	 * 
	 * @param button
	 *            The button to activate. Use null to deselect all.
	 * @return The previously active ButtonModel interface
	 **/
	public ButtonModel setNavigationSelection(ButtonModel button) {
		ButtonModel activeSelection = group.getSelection();
		if (ignoreNextSetNavigationSelection) {
			ignoreNextSetNavigationSelection = false;
		} else {
			group.setSelected(button, true);
		}
		return activeSelection;
	}

	/**
	 * This method is used to add visual highlighting to the radio buttons on
	 * the left splitter panel that are used for navigation.
	 * 
	 * @param radioButton
	 *            The target radio button
	 * @param highlight
	 *            Should the button be highlighted or unhighlighted
	 **/
	public void setRadioButtonHighlighting(JRadioButton radioButton,
			boolean highlight) {
		String s = textForOptions.get(radioButton.getName());
		if (s != null) {
			if (highlight) {
				radioButton.setText("<html><b><font color=white>" + s
						+ "</font></b></html>");
			} else {
				radioButton.setText("<html>" + s + "</html>");
			}
		}
	}

	/** Update the summary icon shown for the Vehicles/Equipment option **/
	public void updateVehiclesEquipmentOptionIcon() {
		// show/hide tree objects

		boolean shouldBeVisible = vehiclesEquipmentOption.isSelected();

		if (parent != null && parent.runSpec != null) {
			onRoadVehicleEquipmentOption.setVisible(false);
			if (CompilationFlags.USE_NONROAD) {
				offRoadVehicleEquipmentOption.setVisible(false);
			}
			Models.ModelCombination mc = Models.evaluateModels(parent.runSpec.models);
			switch (mc) {
			default:
			case M1:
				onRoadVehicleEquipmentOption.setVisible(shouldBeVisible);
				break;
			case M2:
				if (CompilationFlags.USE_NONROAD) {
					offRoadVehicleEquipmentOption.setVisible(shouldBeVisible);
				}
				break;
			}
		} else {
			onRoadVehicleEquipmentOption.setVisible(shouldBeVisible);
			if (CompilationFlags.USE_NONROAD) {
				offRoadVehicleEquipmentOption.setVisible(shouldBeVisible);
			}
		}

		// update the status

		RunSpecSectionStatus status = new RunSpecSectionStatus();
		status.makeWorst();
		if (parent != null && parent.runSpec != null) {
			Models.ModelCombination mc = Models
					.evaluateModels(parent.runSpec.models);
			switch (mc) {
			case M1:
				status.makeBestOfTwo(optionStatuses
						.get(parent.onRoadVehicleEquipmentPanel.getName()));
				parent.onRoadVehicleEquipmentPanel.setEnabled(true);
				parent.offRoadVehicleEquipmentPanel.setEnabled(false);
				break;
			case M2:
				if (CompilationFlags.USE_NONROAD) {
					status.makeBestOfTwo(optionStatuses
							.get(parent.offRoadVehicleEquipmentPanel.getName()));
				}
				parent.onRoadVehicleEquipmentPanel.setEnabled(false);
				parent.offRoadVehicleEquipmentPanel.setEnabled(true);
				break;
			default:
				break;
			}
		} else {
			status.makeBestOfTwo(optionStatuses
					.get(parent.onRoadVehicleEquipmentPanel.getName()));
			if (CompilationFlags.USE_NONROAD) {
				status.makeBestOfTwo(optionStatuses
						.get(parent.offRoadVehicleEquipmentPanel.getName()));
			}
		}

		int iconSubType = RunSpecSectionStatus.TREE_CLOSED;
		if (vehiclesEquipmentOption.isSelected()) {
			iconSubType = RunSpecSectionStatus.TREE_OPEN;
		}
		vehiclesEquipmentOption.setIcon(status.getIcon(iconSubType));
		vehiclesEquipmentOption.setToolTipText(RunSpecSectionStatus
				.explainIcon(vehiclesEquipmentOption.getIcon()));
	}

	/** Update the summary icon shown for the Output option **/
	public void updateOutputOptionIcon() {
		RunSpecSectionStatus status = new RunSpecSectionStatus();
		status.makeBest();
		status.makeWorstOfTwo(optionStatuses
				.get(parent.outputEmissionsBreakdownPanel.getName()));
		status.makeWorstOfTwo(optionStatuses.get(parent.generalOutputPanel
				.getName()));
		int iconSubType = RunSpecSectionStatus.TREE_CLOSED;
		if (outputOption.isSelected()) {
			iconSubType = RunSpecSectionStatus.TREE_OPEN;
		}
		outputOption.setIcon(status.getIcon(iconSubType));
		outputOption.setToolTipText(RunSpecSectionStatus
				.explainIcon(outputOption.getIcon()));
	}

	/** Update the summary icon shown for the Strategy option **/
	public void updateStrategyOptionIcon() {
		RunSpecSectionStatus status = new RunSpecSectionStatus();
		status.makeBest();
		for (Iterator<JRadioButton> i = strategyOptions.iterator(); i.hasNext();) {
			JRadioButton option = i.next();
			RunSpecSectionStatus optionStatus = optionStatuses.get(option
					.getName());
			if (optionStatus != null) {
				status.makeWorstOfTwo(optionStatus);
			}
		}
		int iconSubType = RunSpecSectionStatus.TREE_CLOSED;
		if (strategyOption.isSelected()) {
			iconSubType = RunSpecSectionStatus.TREE_OPEN;
		}
		strategyOption.setIcon(status.getIcon(iconSubType));
		strategyOption.setToolTipText(RunSpecSectionStatus
				.explainIcon(strategyOption.getIcon()));
	}

	/** Handles the Vehicles/Equipment button. **/
	public void processVehiclesEquipmentButton() {

		updateVehiclesEquipmentOptionIcon();
	}

	/** Handles the Output button. **/
	public void processOutputButton() {
		// show/hide tree objects
		boolean shouldBeVisible = outputOption.isSelected();

		outputEmissionsBreakdownOption.setVisible(shouldBeVisible);
		generalOutputOption.setVisible(shouldBeVisible);

		updateOutputOptionIcon();
	}

	/** Handles the Strategy button. **/
	public void processStrategyButton() {
		// show/hide tree objects
		boolean shouldBeVisible = strategyOption.isSelected();

		for (Iterator<JRadioButton> i = strategyOptions.iterator(); i.hasNext();) {
			JRadioButton option = i.next();
			boolean shouldOptionBeVisible = shouldBeVisible;
			if (shouldOptionBeVisible) {
				JPanel p = panelForOptions.get(option.getName());
				if (p != null && p instanceof InternalControlStrategies) {
					InternalControlStrategies ics = (InternalControlStrategies) p;
					if (ics.useImporterOnly && !ics.hasAnInstance()) {
						shouldOptionBeVisible = false;
					}
				}
			}
			option.setVisible(shouldBeVisible && shouldOptionBeVisible);
		}

		updateStrategyOptionIcon();
	}

	/**
	 * Handle the event of a deletion of an InternalControlStrategy object that
	 * should result in its panel being hidden.
	 * 
	 * @param icsPanel
	 *            panel in which the deletion occured
	 **/
	public void onDeletedLastInstance(JPanel icsPanel) {
		commitActiveEditor();
		processStrategyButton();
		clearSelection();
		// Set the focus to the Description option
		group.setSelected(descriptionOption.getModel(), true);
	}

	/** Removes any indication of a selected panel **/
	public void clearSelection() {
		activeEditor = null;
		lastRadioButtonOption = null;
		lastRunSpecEditor = null;

		for (Iterator<JRadioButton> i = options.iterator(); i.hasNext();) {
			JRadioButton option = i.next();
			setRadioButtonHighlighting(option, false);
			group.remove(option);
			option.getModel().setGroup(null);

			option.setSelected(false);
			option.getModel().setSelected(false);
		}

		for (Iterator<JRadioButton> i = options.iterator(); i.hasNext();) {
			JRadioButton option = i.next();
			group.add(option);
		}
	}

	/**
	 * Called by subpanels when the user changes the model scale.
	 * 
	 * @param newScale
	 *            scale being set, may or may not match the current RunSpec's
	 *            scale.
	 **/
	public void onScaleChange(RunSpecEditor editor, ModelScale newScale) {
		// Save the current panel's data
		if (editor != null) {
			// System.out.println("scale before: " + parent.runSpec.scale);
			editor.saveToRunSpec(parent.runSpec);
			// System.out.println("scale after: " + parent.runSpec.scale);
		}
		// Save data from panels affected by a scale change.
		// Note that to bring the user's attention to these panels, their icons
		// are set the default data icon instead of the OK icon upon success.
		// updateRunSpecSectionStatus(timeSpansOption,null,true);
		updateRunSpecSectionStatus(macroscaleGeographicBoundsOption, null, true);
		updateRunSpecSectionStatus(roadTypeOption, null, true);
		updateRunSpecSectionStatus(pollutantsAndProcessesOption, null, true);
		updateRunSpecSectionStatus(outputEmissionsBreakdownOption, null, true);
		updateRunSpecSectionStatus(generalOutputOption, null, true);
	}

	/**
	 * Called by subpanels when the user changes the model domain.
	 * 
	 * @param newDomain
	 *            domain being set, may or may not match the current RunSpec's
	 *            domain.
	 **/
	public void onDomainChange(RunSpecEditor editor, ModelDomain newDomain) {
		// Save the current panel's data
		if (editor != null) {
			// System.out.println("domain before: " + parent.runSpec.domain);
			editor.saveToRunSpec(parent.runSpec);
			// System.out.println("domain after: " + parent.runSpec.domain);
		}
		// Save data from panels affected by a domain change.
		// Note that to bring the user's attention to these panels, their icons
		// are set the default data icon instead of the OK icon upon success.
		updateRunSpecSectionStatus(timeSpansOption, null, true);
		updateRunSpecSectionStatus(macroscaleGeographicBoundsOption, null, true);
		// updateRunSpecSectionStatus(roadTypeOption,null,true);
		// updateRunSpecSectionStatus(pollutantsAndProcessesOption,null,true);
		// updateRunSpecSectionStatus(outputEmissionsBreakdownOption,null,true);
	}

	/**
	 * Called by subpanels when the user changes the model. Added by Jizhen
	 * 
	 * @param newModels
	 *            Models being set, may or may not match the current RunSpec's
	 *            models.
	 **/
	public void onModelsChange(RunSpecEditor editor, Models newModels) {
		// Save the current panel's data
		if (editor != null) {
			// System.out.println("model before: " + parent.runSpec.domain);
			editor.saveToRunSpec(parent.runSpec);
			// System.out.println("model after: " + parent.runSpec.domain);
		}
		// Check, update and save data from panels affected by a model change.
		for (JPanel panel : this.panelForOptions.values()) {
			if (panel instanceof RunSpecEditor) {
				((RunSpecEditor) panel).onModelChange(parent.runSpec,
						optionStatuses);
			}
		}

		// Save data from panels affected by a model change.
		// Note that to bring the user's attention to these panels, their icons
		// are set the default data icon instead of the OK icon upon success.
		updateRunSpecSectionStatus(timeSpansOption, null, true);
		// updateRunSpecSectionStatus(macroscaleGeographicBoundsOption,null,true);
		updateRunSpecSectionStatus(roadTypeOption, null, true);
		// parent.pollutantsAndProcessesPanel.onModelChange(parent.runSpec,
		// optionStatuses);
		updateRunSpecSectionStatus(pollutantsAndProcessesOption, null, true);
		updateRunSpecSectionStatus(generalOutputOption, null, true);
	}
}
