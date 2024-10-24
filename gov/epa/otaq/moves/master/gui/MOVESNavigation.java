/**************************************************************************************************
 * @(#)MOVESNavigation.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.Color;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.TreeMap;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.ToolTipManager;

import gov.epa.otaq.moves.common.CompilationFlags;
import gov.epa.otaq.moves.common.ModelDomain;
import gov.epa.otaq.moves.common.ModelScale;
import gov.epa.otaq.moves.common.Models;
import gov.epa.otaq.moves.master.framework.importers.ImporterManager;
import gov.epa.otaq.moves.master.framework.importers.ImporterGUI;

/**
 * Class for MOVES MOVESNavigation panel. <br>
 * <br>
 * <b>IMPORTANT:</b> This class relies upon the fact that the MOVESWindow has
 * already instantiated all panels needed for the options, so only instantiate
 * one of these after the panels have have been setup. <br>
 * This class Constructs the MOVESNavigation panel. Creates, initializes, and
 * sets the layouts of the following controls, Description, Scale, Macroscale
 * Geographic Bounds, TimeSpans , Vehicles Equipment, OnRoadVehicleEquipment,
 * RoadType, PollutantsAndProcesses. Output,
 * OutputEmissionsBreakdown, GeneralOutput, AdvancedPerformanceFeatures
 * 
 * @author Wesley Faler
 * @author Tim Hull
 * @author Mike Kender (Task 1903)
 * @author John Covey (Task 1903)
 * @author John Covey (Task 2003)
 * @version    2020-07-16 **/

public class MOVESNavigation extends JPanel implements ItemListener {
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
	/** OnRoadVehicleEquipment option checkbox. **/
	JRadioButton onRoadVehicleEquipmentOption;
	/** OffRoadVehicleEquipment option checkbox. **/
	JRadioButton offRoadVehicleEquipmentOption;
	/** RoadType option checkbox. **/
	JRadioButton roadTypeOption;
	/** PollutantsAndProcesses option checkbox. **/
	JRadioButton pollutantsAndProcessesOption;
	/** OutputEmissionsBreakdown option checkbox. **/
	JRadioButton outputEmissionsBreakdownOption;
	/** GeneralOutput option checkbox. **/
	JRadioButton generalOutputOption;
	/** Create Input Database option**/
	JRadioButton createInputDatabaseOption;
	/** AdvancedPerformanceFeatures option **/
	JRadioButton advancedFeaturesOption;
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
				"Description (Alt+1)", parent.descriptionPanel,
				RunSpecSectionStatus.NORMAL);
		descriptionOption.setMnemonic('1');
		scaleOption = createOption("scaleOption", group, "Scale",
				parent.scalePanel, RunSpecSectionStatus.NORMAL);
		timeSpansOption = createOption("timeSpansOption", group, "Time Spans",
				parent.timeSpansPanel, RunSpecSectionStatus.NORMAL);
		macroscaleGeographicBoundsOption = createOption(
				"macroscaleGeographicBoundsOption", group, "Geographic Bounds",
				parent.macroscaleGeographicBoundsPanel,
				RunSpecSectionStatus.NORMAL);

		onRoadVehicleEquipmentOption = createOption(
				"onRoadVehicleEquipmentOption", group,
				"Onroad Vehicles",
				parent.onRoadVehicleEquipmentPanel, RunSpecSectionStatus.NORMAL);
		if (CompilationFlags.USE_NONROAD) {
			offRoadVehicleEquipmentOption = createOption(
					"offRoadVehicleEquipmentOption", group,
					"Nonroad Equipment",
					parent.offRoadVehicleEquipmentPanel,
					RunSpecSectionStatus.NORMAL);
		}
		updateVehiclesEquipmentOptionIcon();

		roadTypeOption = createOption("roadTypeOption", group, "Road Type",
				parent.roadTypePanel, RunSpecSectionStatus.NORMAL);
		pollutantsAndProcessesOption = createOption(
				"pollutantsAndProcessesOption", group,
				"Pollutants and Processes", parent.pollutantsAndProcessesPanel,
				RunSpecSectionStatus.NORMAL);

		generalOutputOption = createOption("generalOutputOption", group,
				"General Output", parent.generalOutputPanel,
				RunSpecSectionStatus.NORMAL);
		outputEmissionsBreakdownOption = createOption(
				"outputEmissionsBreakdownOption", group,
				"Output Emissions Detail",
				parent.outputEmissionsBreakdownPanel, RunSpecSectionStatus.NORMAL);

		createInputDatabaseOption = createOption(
				"createInputDatabaseOption", group,
				"Create Input Database",
				parent.createInputDatabasePanel, RunSpecSectionStatus.NORMAL);
		createInputDatabaseOption.setEnabled(false);

		advancedFeaturesOption = createOption(
				"advancedPerformanceFeaturesOption", group,
				"Advanced Features",
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
		optionIconTypes.put(name, Integer.valueOf(iconSubType));

		return option;
	}

	/** Some extra controls initialization on this panel. **/
	void initControls() {
		// hide collapsed tree options
		onRoadVehicleEquipmentOption.setVisible(false);
		if (CompilationFlags.USE_NONROAD) {
			offRoadVehicleEquipmentOption.setVisible(false);
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
		add(onRoadVehicleEquipmentOption);
		if (CompilationFlags.USE_NONROAD) {
			add(offRoadVehicleEquipmentOption);
		}
		add(pollutantsAndProcessesOption);
		add(roadTypeOption);
		add(generalOutputOption);
		add(outputEmissionsBreakdownOption);
		add(createInputDatabaseOption);
		add(advancedFeaturesOption);
	}

	/**
	 * Helper function used during the new file action. Sets the defaults to the
	 * RunSpec and loads these into the corresponding RunSpecEditor panels.
	 **/
	public void onFileNew() {
		try {
			parent.setWaitCursor();
			Models.ModelCombination mc = Models.ModelCombination.M1;
			if (parent != null && parent.runSpec != null) {
				mc = parent.runSpec.getModelCombination();
			}
			parent.domainImporterNetStatus = null;
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

			updateVehiclesEquipmentOptionIcon();

		    clearSelection();
			
			hasDoneFileNew = true;
			
			parent.checkExecuteAction();
		} finally {
			parent.setDefaultCursor();
		}
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
		try {
			parent.setWaitCursor();
			Models.ModelCombination mc = Models.ModelCombination.M1;
			if (parent != null && parent.runSpec != null) {
				mc = parent.runSpec.getModelCombination();
			}
			parent.domainImporterNetStatus = null;
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
			updateVehiclesEquipmentOptionIcon();
			updateCreateInputDatabaseIcon();
			createInputDatabaseOption.setEnabled(parent.shouldCreateInputDatabaseBeEnabled());
			
			// Set the execute action enabled state in the parent window
			parent.checkExecuteAction();
		} finally {
			parent.setDefaultCursor();
		}
		
		// Check version compatibility
		if (parent != null && parent.runSpec != null) {
			if (parent.runSpec.isCustomDomain()) {
				JOptionPane.showMessageDialog(parent, "Error: The loaded RunSpec uses the Custom Domain feature,\r\n" +
													  "which is not enabled in this version of MOVES. This RunSpec\r\n" +
													  "will not produce usable results, and will need to be recreated\r\n" +
													  "from scratch using County Scale with a single county. If you\r\n" +
												      "need a reference for the selections made in this RunSpec, you\r\n" + 
												      "can use the File > Print... feature.",
						"Custom Domain Error Message",
						JOptionPane.ERROR_MESSAGE);
			} else if (!parent.runSpec.isCompatibleVersion(MOVESWindow.MOVES_VERSION)) {
			JOptionPane.showMessageDialog(parent, "Warning: The loaded RunSpec was created with " + parent.runSpec.getMajorVersionString() + ",\r\n" +
			                                      "which may not be compatible with this version of MOVES. To\r\n" +
												  "avoid compatibility issues, you may need to recreate this\r\n" +
												  "RunSpec using this version of MOVES. If you need a reference\r\n" +
												  "for the selections made in this RunSpec, you can use the\r\n" + 
												  "File > Print... feature.",
					"RunSpec Version Message",
					JOptionPane.WARNING_MESSAGE);
			}
		}
	}

	public void updateOption(JRadioButton option) {
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
		parent.checkExecuteAction();
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
		try {
			parent.setWaitCursor();
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
                parent.setView(p);
            }
		} finally {
			parent.setDefaultCursor();
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
		try {
			parent.setWaitCursor();
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
			// Special case for Create Input Database panel, which might be disabled
			createInputDatabaseOption.setEnabled(parent.shouldCreateInputDatabaseBeEnabled());
			
			// Also check if all RunSpecEditors are ready
			parent.checkExecuteAction();
		} finally {
			parent.setDefaultCursor();
		}
	}

	/** Allows the Pollutants and Processes section status to be updated from other panels (such as Road Type) **/
    public void updatePollutantProcessRunSpecSectionStatus() {
        updateRunSpecSectionStatus(pollutantsAndProcessesOption, null, false);
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
	 * Opens the Description Panel
	 **/
	public void selectDescriptionOption() {
		commitActiveEditor();
		group.setSelected(descriptionOption.getModel(), true);
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
		if (parent != null && parent.runSpec != null) {
			onRoadVehicleEquipmentOption.setVisible(false);
			offRoadVehicleEquipmentOption.setVisible(false);

			Models.ModelCombination mc = Models.evaluateModels(parent.runSpec.models);
			switch (mc) {
			default:
			case M1:
				onRoadVehicleEquipmentOption.setVisible(true);
				break;
			case M2:
				if (CompilationFlags.USE_NONROAD) {
					offRoadVehicleEquipmentOption.setVisible(true);
				}
				break;
			}
		} else {
			if (CompilationFlags.USE_NONROAD) {
				offRoadVehicleEquipmentOption.setVisible(true);
				onRoadVehicleEquipmentOption.setVisible(false);
			} else {
				onRoadVehicleEquipmentOption.setVisible(true);
				offRoadVehicleEquipmentOption.setVisible(false);
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
	}
	
	/** 
	 * If domain importer status is unknown, create importer GUI to run the importer checks.
     * Update the panel icon based on the result of those checks.
	**/
	public void updateCreateInputDatabaseIcon() {
		if(parent.domainImporterNetStatus == null) {
			ImporterManager manager = new ImporterManager();
			
			if(Models.evaluateModels(parent.runSpec.models) == Models.ModelCombination.M2) {
				manager.setAsNonroad();
			} else if(parent.runSpec.domain == ModelDomain.SINGLE_COUNTY) {
				manager.setAsCountyDomain();
			} else if(parent.runSpec.domain == ModelDomain.PROJECT) {
				manager.setAsProjectDomain();
			}
			
			manager.instantiate(null);
			ImporterGUI gui = manager.createGUI(parent);
			gui.populateControls(); // this calls refreshDomainStatusIcons which updates parent.domainImporterNetStatus
		}
		updateOption(createInputDatabaseOption);
	}

	/** Handles the Vehicles/Equipment button. **/
	public void processVehiclesEquipmentButton() {

		updateVehiclesEquipmentOptionIcon();
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
		updateRunSpecSectionStatus(advancedFeaturesOption, null, true);

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
		updateRunSpecSectionStatus(createInputDatabaseOption, null, true);
		updateRunSpecSectionStatus(advancedFeaturesOption, null, true);

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
