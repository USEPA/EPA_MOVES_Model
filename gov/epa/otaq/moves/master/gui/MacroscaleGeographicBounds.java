/**************************************************************************************************
 * @(#)MacroscaleGeographicBounds.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.importers.ImporterManager;

/**
 * Class for geography selection at Macroscale and Mescoscale Lookup.
 * The panel consists of option buttons for region (Nation, State, and County), a States list,
 * a Counties list, and a list for selections. There are Add, Delete, and Select All buttons
 * to prepare a list of States or Counties.
 *
 * @author		Wesley Faler
 * @author		Mitch C. (minor mods)
 * @author		Tim Hull
 * @version		2015-05-21
**/
public class MacroscaleGeographicBounds extends JPanel implements ListSelectionListener,
		ActionListener, FocusListener, RunSpecEditor {
	/** Region label. **/
	JLabel regionLabel;
	/** Radio button group. **/
	ButtonGroup radioButtons;
	/** Nation radio button. **/
	JRadioButton nation;
	/** State radio button. **/
	JRadioButton state;
	/** County radio button. **/
	JRadioButton county;
	/** Zone & Link radio button **/
	JRadioButton zoneLink;
	/** Custom Domain radio button **/
	JRadioButton customDomain;
	/** State label. **/
	JLabel stateLabel;
	/** County label. **/
	JLabel countyLabel;
	/** Selection label. **/
	JLabel selectionLabel;
	/** State default list model. **/
	DefaultListModel<GeographicSelection> stateListModel;
	/** County default list model. **/
	DefaultListModel<GeographicSelection> countyListModel;
	/** Selection default list model. **/
	DefaultListModel<GeographicSelection> selectionListModel;
	/** State list. **/
	JList<GeographicSelection> stateList;
	/** County list. **/
	JList<GeographicSelection> countyList;
	/** Selection list. **/
	JList<GeographicSelection> selectionList;
	/** State scroll pane. **/
	JScrollPane stateScrollPane;
	/** County scroll pane. **/
	JScrollPane countyScrollPane;
	/** Selection scroll pane. **/
	JScrollPane selectionScrollPane;
	/** State Select All button. **/
	JButton stateSelectAll;
	/** State Add button. **/
	JButton stateAdd;
	/** County Select All button. **/
	JButton countySelectAll;
	/** County Add button. **/
	JButton countyAdd;
	/** Selection Delete button. **/
	JButton selectionDelete;
	/** State button panel. **/
	JPanel stateButtonPanel;
	/** County button panel. **/
	JPanel countyButtonPanel;
	/** String to track previous radio button selection value. **/
	String previousRBSetting;
	/** Message log component. **/
	JList<String> messageLogList;
	/** DefaultListModel for the messageLogList. **/
	DefaultListModel<String> messageLogModel;
	/** JScrollPane for the messageLogList. **/
	JScrollPane messageLogPane;
	/** Message Panel **/
	JPanel messageLogPanel;
	/** Layout for Message Panel **/
	FlowLayout messageLogFlowLayout;
	/** State of the generic county **/
	JLabel genericCountyState;
	/** 3-digit ID of the generic county **/
	JTextField genericCountyID;
	/** description of the generic county **/
	JTextField genericCountyName;
	/** GPA fraction for the generic county **/
	JTextField genericCountyGPAFraction;
	/** Barometric pressure for the generic county **/
	JTextField genericCountyBarometricPressure;
	/** refuelingVaporProgramAdjust for the generic county **/
	JTextField genericCountyRefuelingVaporProgramAdjust;
	/** refuelingSpillProgramAdjust for the generic county **/
	JTextField genericCountyRefuelingSpillProgramAdjust;

	/** Panel holding list of states, counties, and user selections **/
	JPanel stateCountiesPanel;
	/** Panel holding generic county input controls **/
	JPanel genericCountyPanel;

	/** Panel holding controls for selecting a database for County or project domains **/
	JPanel databasePanel;
	/** Text describing the current scale/domain **/
	JLabel databasePurposeLabel;
	/** Database server **/
	JTextField server;
	/** List of database names **/
	ExtendedComboBox<String> databaseCombo;
	/** Button for creating/editing the database via the County Data Manager **/
	JButton editDatabaseButton;
	/** Button to refresh the list of databases **/
	JButton refreshDatabaseButton;
	/** Button to scan the current database selection for validity **/
	JButton checkDatabaseButton;
	/** Database droplist ToolTip **/
	JToolTip databaseToolTip;

	/** Panel showing warning about National-scale information **/
	JPanel nationalWarningPanel = null;
	/** Panel showing warning about County/Project-scale information **/
	JPanel localWarningPanel = null;
	/** Model domain **/
	ModelDomain domain = null;

	/** True when performing an Onroad simulation **/
	boolean onroadSelected = true;

	/**
	 * Used by the server FocusLost event handler to help determine if the server
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the server TextField losing focus when the server name hasn't changed.
	**/
	String previousServer = "localhost";
	/** database names that should not be used **/
	TreeSetIgnoreCase invalidDatabaseNames = new TreeSetIgnoreCase();

	/**
	 * Constructs a MacroscaleGeographicBounds panel, also creates and sets the layouts of
	 * the controls.
	**/
	public MacroscaleGeographicBounds() {
		createControls();
		arrangeControls();

		generateListOfInvalidDatabaseNames();
		loadDatabases();
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("Geographic Bounds:\r\n");
		if(runspec.isCustomDomain()) {
			destination.append("\tGeneric County\r\n");
			destination.append("\tState ID: " + runspec.genericCounty.stateID + "\r\n");
			destination.append("\tCounty ID: " + runspec.genericCounty.shortCountyID + "\r\n");
			destination.append("\tDescription: " + runspec.genericCounty.description + "\r\n");
			destination.append("\tAltitude: "
					+ (runspec.genericCounty.isHighAltitude()?"High":"Low") + "\r\n");
			destination.append("\tBarometric Pressure: "
					+ runspec.genericCounty.barometricPressure + "\r\n");
			destination.append("\tGPA Fraction: " + runspec.genericCounty.gpaFraction + "\r\n");
			destination.append("\tRefueling Vapor Program Adjustment Fraction: "
					+ runspec.genericCounty.refuelingVaporProgramAdjust + "\r\n");
			destination.append("\tRefueling Spill Program Adjustment Fraction: "
					+ runspec.genericCounty.refuelingSpillProgramAdjust + "\r\n");
		} else if(runspec.geographicOutputDetail!=null) {
			destination.append("\t" + runspec.geographicOutputDetail + " geography\r\n");
			if(!runspec.geographicSelections.isEmpty()) {
				for(Iterator i=runspec.geographicSelections.iterator();i.hasNext();) {
					destination.append("\tSelection: " + i.next() + "\r\n");
			}
		}

		}
		destination.append("\r\n");
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		createGenericCountyPanel();
		createDatabasePanel();

		stateButtonPanel = new JPanel();
		countyButtonPanel = new JPanel();
		Dimension buttonSize = new Dimension(87,25);
		regionLabel = new JLabel("Region:");
		regionLabel.setName("regionLabel");
		radioButtons = new ButtonGroup();
		nation = new JRadioButton("Nation");
		nation.setName("nation");
		nation.addActionListener(this);
		ToolTipHelper.add(nation,"Simulate at the nation level");
		state = new JRadioButton("State");
		state.setName("state");
		state.addActionListener(this);
		ToolTipHelper.add(state,"Simulate at the state level");
		county = new JRadioButton("County");
		county.setName("county");
		county.addActionListener(this);
		ToolTipHelper.add(county,"Simulate at the county level");
		zoneLink = new JRadioButton("Zone & Link");
		zoneLink.setName("zoneLink");
		zoneLink.addActionListener(this);
		ToolTipHelper.add(zoneLink,"Simulate at the zone & link level");
		customDomain = new JRadioButton("Custom Domain");
		customDomain.setName("customDomain");
		customDomain.addActionListener(this);
		ToolTipHelper.add(customDomain,"Define a custom county");

		stateLabel = new JLabel("States:");
		stateLabel.setName("stateLabel");
		countyLabel = new JLabel("Counties:");
		countyLabel.setName("countyLabel");
		selectionLabel = new JLabel("Selections:");
		selectionLabel.setName("selectionLabel");

		GeographicSelection prototypeValue = new GeographicSelection();
		prototypeValue.textDescription = "CharacterCountToDisplayXXXXXXX";

		stateListModel = new DefaultListModel<GeographicSelection>();
		countyListModel = new DefaultListModel<GeographicSelection>();
		selectionListModel = new DefaultListModel<GeographicSelection>();
		loadStates();
		stateList = new JListWithToolTips<GeographicSelection>(stateListModel);
		stateList.setName("stateList");
		stateList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		stateList.setSelectedIndex(-1);
		stateList.addListSelectionListener(this);
		stateList.setVisibleRowCount(9);
		stateList.setPrototypeCellValue(prototypeValue);
		stateScrollPane = new JScrollPane(stateList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		stateScrollPane.setName("stateScrollPane");

		countyList = new JListWithToolTips<GeographicSelection>(countyListModel);
		countyList.setName("countyList");
		countyList.setSelectionMode(
			ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		countyList.setSelectedIndex(-1);
		countyList.addListSelectionListener(this);
		countyList.setVisibleRowCount(9);
		countyList.setPrototypeCellValue(prototypeValue);
		countyScrollPane = new JScrollPane(countyList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		countyScrollPane.setName("countyScrollPane");

		selectionList = new JListWithToolTips<GeographicSelection>(selectionListModel);
		selectionList.setName("selectionList");
		selectionList.setSelectionMode(
			ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		selectionList.setSelectedIndex(-1);
		selectionList.addListSelectionListener(this);
		selectionList.setVisibleRowCount(9);
		selectionList.setPrototypeCellValue(prototypeValue);
		selectionScrollPane = new JScrollPane(selectionList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		selectionScrollPane.setName("selectionScrollPane");

		stateSelectAll = new JButton("Select All");
		stateSelectAll.setName("stateSelectAll");
		stateSelectAll.addActionListener(this);
		stateSelectAll.setPreferredSize(buttonSize);
		ToolTipHelper.add(stateSelectAll,"Select all states");
		stateAdd = new JButton("Add");
		stateAdd.setName("stateAdd");
		stateAdd.setEnabled(false); // not enabled until item selected from list
		stateAdd.addActionListener(this);
		stateAdd.setPreferredSize(buttonSize);
		ToolTipHelper.add(stateAdd,"Add selected states to selection list");
		countySelectAll = new JButton("Select All");
		countySelectAll.setName("countySelectAll");
		countySelectAll.addActionListener(this);
		countySelectAll.setPreferredSize(buttonSize);
		ToolTipHelper.add(countySelectAll,"Select all counties");
		countyAdd = new JButton("Add");
		countyAdd.setName("countyAdd");
		countyAdd.setEnabled(false); // not enabled until item selected from list
		countyAdd.addActionListener(this);
		countyAdd.setPreferredSize(buttonSize);
		ToolTipHelper.add(countyAdd,"Add selected counties to selection list");
		selectionDelete = new JButton("Delete");
		selectionDelete.setName("selectionDelete");
		selectionDelete.setEnabled(false); // not enabled until item selected from list
		selectionDelete.addActionListener(this);
		ToolTipHelper.add(selectionDelete,"Delete selected items from the selection list");

		previousRBSetting = "";

		messageLogModel = new DefaultListModel<String>();
		messageLogList = new JListWithToolTips<String>(messageLogModel);
		messageLogList.setName("messageLogList");
		messageLogList.setSelectedIndex(-1);
		messageLogList.setVisibleRowCount(2);
		messageLogList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
		messageLogPane = new JScrollPane(messageLogList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		messageLogPane.setName("messageLogPane");
		messageLogPanel = new JPanel();
		messageLogPanel.setVisible(true);
		ToolTipHelper.add(messageLogPane,"Requirements to complete the Macroscale Geographic"
				+ " Bounds section");

		ImageIcon warningImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/dataExists.gif");
		JLabel warningLabel = new JLabel(
				"<html><body>"
				+ "Caution: You have selected National scale with detail at the State or County level.<br>"
				+ "MOVES will use the default national database with default state and local allocation<br>"
				+ "factors.  These factors have not been verified against actual state or county level<br>"
				+ "data and do not meet regulatory requirements for SIPs and conformity determinations."
				+ "</body></html>",
				warningImage, JLabel.LEFT);
		nationalWarningPanel = new JPanel();
		nationalWarningPanel.setLayout(new BoxLayout(nationalWarningPanel, BoxLayout.X_AXIS));
		nationalWarningPanel.add(warningLabel);
		nationalWarningPanel.add(Box.createHorizontalGlue());

		warningImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/dataExists.gif");
		warningLabel = new JLabel(
				"<html><body>"
				+ "Caution:  For SIP or regional conformity analyses, you must go back to the<br>"
				+ "Scale window and select \"County\" before specifying a county in this window."
				+ "</body></html>",
				warningImage, JLabel.LEFT);
		localWarningPanel = new JPanel();
		localWarningPanel.setLayout(new BoxLayout(localWarningPanel, BoxLayout.X_AXIS));
		localWarningPanel.add(warningLabel);
		localWarningPanel.add(Box.createHorizontalGlue());
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		setLayout(new GridBagLayout());
		stateCountiesPanel = new JPanel();
		stateCountiesPanel.setLayout(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 11;
		gbc.gridheight = 8;
		gbc.weightx = 0;
		gbc.weighty = 0;

		stateButtonPanel.add(stateSelectAll);
		stateButtonPanel.add(stateAdd);
		countyButtonPanel.add(countySelectAll);
		countyButtonPanel.add(countyAdd);

		// Register a listener for the buttons.
		radioButtons.add(nation);
		radioButtons.add(state);
		radioButtons.add(county);
		radioButtons.add(zoneLink);
		radioButtons.add(customDomain);

//		Dimension view = getPreferredSize();
//		if(view==null) {
//			System.out.println("view not set");
//		} else {
//			System.out.println("Height="+view.height);
//			System.out.println("Width="+view.width);
//		}

		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		add(regionLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		add(nation, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 2, "WEST", 1, 1);
		add(state, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 3, "WEST", 1, 1);
		add(county, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 4, "WEST", 1, 1);
		add(zoneLink, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 5, "WEST", 1, 1);
		add(customDomain, gbc);
		// Leave a blank grid cell for row #6 (7th row)

		LayoutUtility.setPositionOnGrid(gbc,1-1, 0, "WEST", 2, 1);
		stateCountiesPanel.add(stateLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1-1, 1, "CENTER", 2, 5);
		stateCountiesPanel.add(stateScrollPane, gbc);

		LayoutUtility.setPositionOnGrid(gbc,1-1, 6, "WEST", 2, 1);
		stateCountiesPanel.add(stateButtonPanel, gbc);

		LayoutUtility.setPositionOnGrid(gbc,3-1, 0, "WEST", 2, 1);
		stateCountiesPanel.add(countyLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3-1, 1, "CENTER", 2, 5);
		stateCountiesPanel.add(countyScrollPane, gbc);

		LayoutUtility.setPositionOnGrid(gbc,3-1, 6, "WEST", 2, 1);
		stateCountiesPanel.add(countyButtonPanel, gbc);

		LayoutUtility.setPositionOnGrid(gbc,5-1, 0, "WEST", 2, 1);
		stateCountiesPanel.add(selectionLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,5-1, 1, "CENTER", 2, 5);
		stateCountiesPanel.add(selectionScrollPane, gbc);
		LayoutUtility.setPositionOnGrid(gbc,5-1, 6, "EAST", 2, 1);
		stateCountiesPanel.add(selectionDelete, gbc);

		JPanel centerPanel = new JPanel();
		centerPanel.setLayout(new BoxLayout(centerPanel,BoxLayout.Y_AXIS));
		centerPanel.add(stateCountiesPanel);
		centerPanel.add(genericCountyPanel);
		genericCountyPanel.setVisible(false);

		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 6, 7);
		add(centerPanel, gbc);

		messageLogPanel.setLayout(new BoxLayout(messageLogPanel, BoxLayout.Y_AXIS));
		messageLogPanel.add(new JLabel("Geographic Bounds Requirements"));
		messageLogPanel.add(messageLogPane);

		databasePanel.setVisible(false);
		LayoutUtility.setPositionOnGrid(gbc, 1, 8, "WEST", 8, 1);
		add(databasePanel, gbc);

		LayoutUtility.setPositionOnGrid(gbc, 0, 9, "WEST", 9, 1);
		add(messageLogPanel, gbc);

		localWarningPanel.setVisible(false);
		nationalWarningPanel.setVisible(false);

		LayoutUtility.setPositionOnGrid(gbc, 1, 10, "WEST", 9, 1);
		add(localWarningPanel, gbc);

		LayoutUtility.setPositionOnGrid(gbc, 1, 11, "WEST", 9, 1);
		add(nationalWarningPanel, gbc);
	}

	/**
	 * Listener method for list selection changes.
	 * @param e event signaling a change
	**/
	public void valueChanged(ListSelectionEvent e) {
		if(e.getValueIsAdjusting() == false) {
			if(e.getSource() == stateList && stateList.getSelectedIndex() >= 0
					&& (county.isSelected() || zoneLink.isSelected())) {
				loadCounties();
			}
			updateButtonStates();
		}
	}

	/** Helper method for enabling/disabling all buttons depending upon list selections **/
	public void updateButtonStates() {
		if(stateList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			stateSelectAll.setEnabled(true);
			stateAdd.setEnabled(false);
		}
		if(stateList.getSelectedIndices().length > 1) {
			//Multiple selection: enable/disable relevant controls.
			stateSelectAll.setEnabled(true);
			stateAdd.setEnabled(true);
		}
		if(stateList.getSelectedIndices().length == 1) {
			//Single selection: enable/disable relevant controls.
			stateSelectAll.setEnabled(true);
			stateAdd.setEnabled(true);
		}
		if(countyList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			countySelectAll.setEnabled(true);
			countyAdd.setEnabled(false);
		}
		if(countyList.getSelectedIndices().length > 1) {
			//Multiple selection: enable/disable relevant controls.
			countySelectAll.setEnabled(true);
			countyAdd.setEnabled(true);
		}
		if(countyList.getSelectedIndices().length == 1) {
			//Single selection: enable/disable relevant controls.
			countySelectAll.setEnabled(true);
			countyAdd.setEnabled(true);
		}
		if(selectionList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			selectionDelete.setEnabled(false);
		}
		if(selectionList.getSelectedIndices().length > 1) {
			//Multiple selection: enable/disable relevant controls.
			selectionDelete.setEnabled(true);
		}
		if(selectionList.getSelectedIndices().length == 1) {
			//Single selection: enable/disable relevant controls.
			selectionDelete.setEnabled(true);
		}
		displaySectionStatus();
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == nation) {
			processNationButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if (e.getSource() == state) {
			processStateButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if (e.getSource() == county) {
			processCountyButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == zoneLink) {
			processZoneLinkButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == customDomain) {
			processCustomDomainButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if (e.getSource() == stateSelectAll) {
			processStateSelectAllButton();
		} else if (e.getSource() == stateAdd) {
			processStateAddButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if (e.getSource() == countySelectAll) {
			processCountySelectAllButton();
		} else if (e.getSource() == countyAdd) {
			processCountyAddButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if (e.getSource() == selectionDelete) {
			processSelectionDeleteButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == editDatabaseButton) {
			handleEditDatabaseButton();
		} else if(e.getSource() == refreshDatabaseButton) {
			handleRefreshDatabaseButton();
		} else if(e.getSource() == checkDatabaseButton) {
			handleCheckDatabaseButton();
		} else if(e.getSource() == databaseCombo) {
			processDatabaseComboChange();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		}
	}

	/** Handles the Nation button. **/
	public void processNationButton() {
		selectionListModel.removeAllElements();

		regionLabel.setVisible(true);
		nation.setVisible(true);
		state.setVisible(true);
		county.setVisible(true);
		stateLabel.setVisible(false);
		countyLabel.setVisible(false);
		selectionLabel.setVisible(false);
		stateScrollPane.setVisible(false);
		countyScrollPane.setVisible(false);
		selectionScrollPane.setVisible(false);
		stateSelectAll.setVisible(false);
		stateAdd.setVisible(false);
		countySelectAll.setVisible(false);
		countyAdd.setVisible(false);
		selectionDelete.setVisible(false);
		displaySectionStatus();
	}

	/** Handles the State button. **/
	public void processStateButton() {
		if(0 != previousRBSetting.compareTo("S")) {
			selectionListModel.removeAllElements();
		}
		// repopulate the states list control
		loadStates();

		regionLabel.setVisible(true);
		nation.setVisible(true);
		state.setVisible(true);
		county.setVisible(true);
		stateLabel.setVisible(true);
		countyLabel.setVisible(false);
		selectionLabel.setVisible(true);
		stateScrollPane.setVisible(true);
		countyScrollPane.setVisible(false);
		selectionScrollPane.setVisible(true);
		stateSelectAll.setVisible(true);
		stateAdd.setVisible(true);
		countySelectAll.setVisible(false);
		countyAdd.setVisible(false);
		selectionDelete.setVisible(true);
		previousRBSetting = "S";
		displaySectionStatus();
	}

	/** Handles the County button. **/
	public void processCountyButton() {
		if(0 != previousRBSetting.compareTo("C")) {
			selectionListModel.removeAllElements();
		}
		// reload the states list
		loadStates();
		countyListModel.removeAllElements();

		regionLabel.setVisible(true);
		nation.setVisible(true);
		state.setVisible(true);
		county.setVisible(true);
		stateLabel.setVisible(true);
		countyLabel.setVisible(true);
		selectionLabel.setVisible(true);
		stateScrollPane.setVisible(true);
		countyScrollPane.setVisible(true);
		selectionScrollPane.setVisible(true);
		stateSelectAll.setVisible(false);
		stateAdd.setVisible(false);
		countySelectAll.setVisible(true);
		countyAdd.setVisible(true);
		selectionDelete.setVisible(true);
		stateList.setSelectionMode(
				ListSelectionModel.SINGLE_INTERVAL_SELECTION);

		if(!stateCountiesPanel.isVisible()) {
			genericCountyPanel.setVisible(false);
			stateCountiesPanel.setVisible(true);
		}

		previousRBSetting = "C";
		displaySectionStatus();
	}

	/** Handles the Zone & Link button. **/
	public void processZoneLinkButton() {
		if(0 != previousRBSetting.compareTo("Z")) {
			selectionListModel.removeAllElements();
		}
		// reload the states list
		loadStates();
		countyListModel.removeAllElements();

		regionLabel.setVisible(true);
		nation.setVisible(true);
		state.setVisible(true);
		county.setVisible(true);
		stateLabel.setVisible(true);
		countyLabel.setVisible(true);
		selectionLabel.setVisible(true);
		stateScrollPane.setVisible(true);
		countyScrollPane.setVisible(true);
		selectionScrollPane.setVisible(true);
		stateSelectAll.setVisible(false);
		stateAdd.setVisible(false);
		countySelectAll.setVisible(true);
		countyAdd.setVisible(true);
		selectionDelete.setVisible(true);
		stateList.setSelectionMode(
				ListSelectionModel.SINGLE_INTERVAL_SELECTION);

		if(!stateCountiesPanel.isVisible()) {
			genericCountyPanel.setVisible(false);
			stateCountiesPanel.setVisible(true);
		}

		previousRBSetting = "Z";
		displaySectionStatus();
	}

	/** Handles the Custom Domain button. **/
	public void processCustomDomainButton() {
		if(0 != previousRBSetting.compareTo("D")) {
			selectionListModel.removeAllElements();
		}

		if(!genericCountyPanel.isVisible()) {
			stateCountiesPanel.setVisible(false);
			genericCountyPanel.setVisible(true);
		}

		previousRBSetting = "D";
		displaySectionStatus();
	}

	/** Handle any changes to controls in the generic county **/
	public void handleGenericCountyChange() {
		if(customDomain.isSelected()) {
			displaySectionStatus();
		}
	}

	/** Handles the Select All button. **/
	public void processStateSelectAllButton() {
		stateList.clearSelection();
		stateList.setSelectionInterval(0,stateList.getModel().getSize()-1);
		updateButtonStates();
	}

	/** Handles the State Add button. **/
	public void processStateAddButton() {
		Object[] stateItems = stateList.getSelectedValuesList().toArray();
		for(int i=0;i<stateItems.length;i++) {
			boolean foundMatch = false;
			GeographicSelection nextStateItem = (GeographicSelection)(stateItems[i]);
			for(Enumeration e=selectionListModel.elements();e.hasMoreElements();) {
				GeographicSelection selection = (GeographicSelection)e.nextElement();
				if(selection.equals(nextStateItem)) {
					foundMatch = true;
					break;
				}
			}
			if(!foundMatch) {
				selectionListModel.addElement(nextStateItem);
			}
		}
		updateButtonStates();
	}

	/** Handles the County Select All button. **/
	public void processCountySelectAllButton() {
		countyList.clearSelection();
		countyList.setSelectionInterval(0,countyList.getModel().getSize()-1);
		updateButtonStates();
	}

	/** Handles the County Add button. **/
	public void processCountyAddButton() {
		if(stateList.getSelectedIndex() < 0 || countyList.getSelectedIndex() < 0) {
			return;
		}
		Object[] countyItems = countyList.getSelectedValuesList().toArray();

		for(int i=0;i<countyItems.length;i++) {
			boolean foundMatch = false;
			GeographicSelection nextCountyItem = (GeographicSelection)(countyItems[i]);
			for(Enumeration e=selectionListModel.elements();e.hasMoreElements();) {
				GeographicSelection selection = (GeographicSelection)e.nextElement();
				if(selection.equals(nextCountyItem)) {
					foundMatch = true;
					break;
				}
			}
			if(!foundMatch) {
				selectionListModel.addElement(nextCountyItem);
			}
		}
		updateButtonStates();
	}

	/** Handles the Delete button. **/
	public void processSelectionDeleteButton() {
		Object[] selectedItems = selectionList.getSelectedValuesList().toArray();
		for(int i=0;i<selectedItems.length;i++) {
			selectionListModel.removeElement(selectedItems[i]);
		}
		updateButtonStates();
	}

	/** Display the status of this runspec section. **/
	public void displaySectionStatus() {
		if(messageLogPanel == null || customDomain == null || databasePanel == null
				|| databaseCombo == null) {
			return;
		}
		messageLogPanel.setVisible(true);
		nationalWarningPanel.setVisible(false);
		localWarningPanel.setVisible(false);
		if(onroadSelected && domain != ModelDomain.SINGLE_COUNTY && domain != ModelDomain.PROJECT) {
			localWarningPanel.setVisible(true);
		}
		boolean didClear = false;
		if(nation.isSelected()) {
			// Nothing to do here, Nation is always valid
		} else if (state.isSelected()) {
			if(selectionList.getModel().getSize() == 0) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("Please select a state."));
			}
			if(onroadSelected && domain == ModelDomain.NATIONAL_ALLOCATION) {
				nationalWarningPanel.setVisible(true);
			}
		} else if (county.isSelected()) {
			if(selectionList.getModel().getSize() == 0) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("Please select a state and county."));
			}
			if(onroadSelected && domain == ModelDomain.NATIONAL_ALLOCATION) {
				nationalWarningPanel.setVisible(true);
			}
		} else if(customDomain.isSelected()) {
			GenericCounty g = new GenericCounty();
			toGenericCounty(g);
			if(!g.isValid()) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				if(g.stateID < 1 || g.stateID > 99) {
					messageLogModel.addElement(
							new String("Please supply a state ID from 1 to 99."));
				}
				if(g.shortCountyID < 1 || g.shortCountyID > 999) {
					messageLogModel.addElement(
							new String("Please supply a county ID from 1 to 999."));
				}
				if(g.gpaFraction < 0 || g.gpaFraction > 1.0) {
					messageLogModel.addElement(
							new String("Please supply a GPA fraction from 0.0 to 1.0"));
				}
			}
		}
		if(!didClear) {
			messageLogModel.clear();
			didClear = true;
		}
		if(databasePanel.isVisible()) {
			if(!didClear) {
				messageLogModel.clear();
				didClear = true;
			}
			boolean hasDatabase = true;
			if(databaseCombo.getSelectedItem() == null) {
				hasDatabase = false;
			} else if(databaseCombo.getSelectedItem().toString().length() == 0) {
				hasDatabase = false;
			} else {
				String newDatabaseName = databaseCombo.getSelectedItem().toString();
				if(newDatabaseName == null || newDatabaseName.length() == 0) {
					hasDatabase = false;
				}
			}
			if(!hasDatabase) {
				messageLogPanel.setVisible(true);
				messageLogModel.addElement("Please select a domain database.");
			}
		}
	}

	/**
	 * Saves the info to a RunSpec.
	 * @param	runspec the RunSpec to get the info
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.geographicSelections.clear();

		if(state.isSelected() || county.isSelected() || zoneLink.isSelected()) {
			runspec.genericCounty = null;
			for(int i=0;i<selectionListModel.getSize();i++) {
				runspec.geographicSelections.add(
						(GeographicSelection)selectionListModel.getElementAt(i));
			}
		} else if(nation.isSelected()) {
			runspec.genericCounty = null;
			GeographicSelection geographicSelection = new GeographicSelection();
			geographicSelection.type = GeographicSelectionType.NATION;
			runspec.geographicSelections.add(geographicSelection);
		} else if(customDomain.isSelected()) {
			GeographicSelection geographicSelection = new GeographicSelection();
			geographicSelection.type = GeographicSelectionType.COUNTY;
			if(runspec.genericCounty == null) {
				runspec.genericCounty = new GenericCounty();
			}
			toGenericCounty(runspec.genericCounty);
			geographicSelection.databaseKey = runspec.genericCounty.getCountyID();
			runspec.geographicSelections.add(geographicSelection);
		}
		if(nation.isSelected()) {
			runspec.outputEmissionsBreakdownSelection.estimateUncertainty=false;
			runspec.geographicOutputDetail = GeographicOutputDetailLevel.NATION;
		}
		if(state.isSelected()) {
			runspec.outputEmissionsBreakdownSelection.estimateUncertainty=false;
			if (runspec.geographicOutputDetail == GeographicOutputDetailLevel.COUNTY ||
					runspec.geographicOutputDetail == GeographicOutputDetailLevel.ZONE ||
					runspec.geographicOutputDetail == GeographicOutputDetailLevel.LINK) {
				runspec.geographicOutputDetail = GeographicOutputDetailLevel.STATE;
			}
		}
		if(databasePanel.isVisible()) {
			if(runspec.scaleInputDatabase == null) {
				runspec.scaleInputDatabase = new DatabaseSelection();
			}
			runspec.scaleInputDatabase.serverName = server.getText();
			runspec.scaleInputDatabase.databaseName = databaseCombo.getSelectedItem().toString();
		} else {
			runspec.scaleInputDatabase.serverName = "";
			runspec.scaleInputDatabase.databaseName = "";
		}
	}

	/**
	 * Loads the description text from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		onroadSelected = runspec.models.contains(Model.ONROAD);
		domain = runspec.domain;
		fromGenericCounty(runspec.genericCounty);

		// sets "none selected" in the button group
		radioButtons.remove(nation);
		radioButtons.remove(state);
		radioButtons.remove(county);
		radioButtons.remove(zoneLink);
		radioButtons.remove(customDomain);
		nation.setSelected(false);
		state.setSelected(false);
		county.setSelected(false);
		zoneLink.setSelected(false);
		customDomain.setSelected(false);
		radioButtons.add(nation);
		radioButtons.add(state);
		radioButtons.add(county);
		radioButtons.add(zoneLink);
		radioButtons.add(customDomain);

		boolean isCustomDomain = runspec.isCustomDomain();
		databasePanel.setVisible(runspec.domain != ModelDomain.NATIONAL_ALLOCATION);
		if(runspec.domain != ModelDomain.NATIONAL_ALLOCATION) {
			if(runspec.scaleInputDatabase != null) {
				server.setText(runspec.scaleInputDatabase.serverName);
				databaseCombo.setSelectedItem(runspec.scaleInputDatabase.databaseName);
			} else {
				server.setText("");
				databaseCombo.setSelectedItem("");
			}
		}

		if(runspec.domain == ModelDomain.PROJECT) {
			databasePurposeLabel.setText(
					"The Project domain scale requires a database of detailed data.");
		} else {
			databasePurposeLabel.setText(
					"The County domain scale requires a database of detailed data.");
		}

		Models.ModelCombination mc = runspec.getModelCombination();

		switch (mc) {
			case M2: // Nonroad
				nation.setEnabled(true);
				state.setEnabled(false);
				county.setEnabled(true);
				zoneLink.setEnabled(false);
				customDomain.setEnabled(false);

				if(!runspec.geographicSelections.isEmpty()) {
					Object firstElement = runspec.geographicSelections.iterator().next();
					GeographicSelection firstGS = (GeographicSelection) firstElement;
					if(firstGS.type == GeographicSelectionType.COUNTY) {
						county.setSelected(true);
						previousRBSetting = "C";
						processCountyButton();
						selectionListModel.removeAllElements();
						for(Iterator<GeographicSelection> i = runspec.geographicSelections.iterator(); i.hasNext();) {
							selectionListModel.addElement(i.next());
						}
					} else {
						nation.setSelected(true);
						previousRBSetting = "N";
						processNationButton();
					}
				} else {
					county.setSelected(true);
					previousRBSetting = "C";
					processCountyButton();
					selectionListModel.removeAllElements();
				}
				break;
			case M1: // Onroad
			default:
				if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
					nation.setEnabled(false);
					state.setEnabled(false);
					county.setEnabled(false);
					zoneLink.setEnabled(true);
					customDomain.setEnabled(runspec.domain != ModelDomain.NATIONAL_ALLOCATION);
		
					if(isCustomDomain) {
						customDomain.setSelected(true);
						previousRBSetting = "D";
						processCustomDomainButton();
						selectionListModel.removeAllElements();
					} else {
						zoneLink.setSelected(true);
						previousRBSetting = "Z";
						processZoneLinkButton();
		
						selectionListModel.removeAllElements();
						if(!runspec.geographicSelections.isEmpty()) {
							Object firstElement = runspec.geographicSelections.iterator().next();
							GeographicSelection firstGS = (GeographicSelection) firstElement;
							// Only County entries are valid at Mesoscale Lookup level since there's
							// no GUI method to select state or nation level.
							if(firstGS.type == GeographicSelectionType.COUNTY) {
								for(Iterator<GeographicSelection> i=runspec.geographicSelections.iterator();i.hasNext();) {
									selectionListModel.addElement(i.next());
								}
							}
						}
					}
				} else {
					nation.setEnabled(runspec.domain == ModelDomain.NATIONAL_ALLOCATION);
					state.setEnabled(runspec.domain == ModelDomain.NATIONAL_ALLOCATION);
					county.setEnabled(true);
					zoneLink.setEnabled(false);
					customDomain.setEnabled(runspec.domain != ModelDomain.NATIONAL_ALLOCATION);
		
					if(isCustomDomain) {
						customDomain.setSelected(true);
						previousRBSetting = "D";
						processCustomDomainButton();
						selectionListModel.removeAllElements();
					} else if(!runspec.geographicSelections.isEmpty()) {
						Object firstElement = runspec.geographicSelections.iterator().next();
						GeographicSelection firstGS = (GeographicSelection) firstElement;
						if(firstGS.type == GeographicSelectionType.STATE) {
							state.setSelected(true);
							previousRBSetting = "S";
							processStateButton();
							selectionListModel.removeAllElements();
							for(Iterator<GeographicSelection> i=runspec.geographicSelections.iterator();i.hasNext();) {
								selectionListModel.addElement(i.next());
							}
						} else if(firstGS.type == GeographicSelectionType.COUNTY) {
							county.setSelected(true);
							previousRBSetting = "C";
							processCountyButton();
							selectionListModel.removeAllElements();
							for(Iterator<GeographicSelection> i=runspec.geographicSelections.iterator();i.hasNext();) {
								selectionListModel.addElement(i.next());
							}
						} else {
							nation.setSelected(true);
							previousRBSetting = "N";
							processNationButton();
						}
					} else {
						county.setSelected(true);
						previousRBSetting = "C";
						processCountyButton();
						selectionListModel.removeAllElements();
					}
				}
				break;
		}
		displaySectionStatus();
		assessSituation(runspec);
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the info
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		boolean isOk = true;

		if(isOk && runspec.domain != ModelDomain.NATIONAL_ALLOCATION) {
			if(!(runspec.scaleInputDatabase != null
					&& (runspec.scaleInputDatabase.serverName == null
					|| runspec.scaleInputDatabase.serverName.length() >= 0)
					&& runspec.scaleInputDatabase.databaseName != null
					&& runspec.scaleInputDatabase.databaseName.length() > 0)) {
				isOk = false;
			}
		}

		if(isOk && runspec.domain != ModelDomain.NATIONAL_ALLOCATION) {
			// For County and Project domains, there must be exactly one county
			// selected.  Even if the generic county is used, it will be in
			// the list.
			if(runspec.geographicSelections.size() != 1) {
				isOk = false;
			} else {
				GeographicSelection g = (GeographicSelection)runspec.geographicSelections.first();
				if(g.type != GeographicSelectionType.COUNTY) {
					isOk = false;
				}
			}
		}

		TreeSet<GeographicSelection> geographicSelections = runspec.geographicSelections;

		if(isOk && runspec.isCustomDomain()) {
			isOk = runspec.genericCounty.isValid();
			if(isOk) {
				// Verify the custom county is in the custom database
				GeographicSelection g = new GeographicSelection();
				g.databaseKey = runspec.genericCounty.getCountyID();
				g.type = GeographicSelectionType.COUNTY;
				g.textDescription = runspec.genericCounty.description;

				geographicSelections = new TreeSet<GeographicSelection>();
				geographicSelections.add(g);
			}
		}
		if(isOk) {
			isOk = !geographicSelections.isEmpty();
			if(isOk) {
				// Verify that the locations are in the database.
				boolean shouldCloseDatabase = false;
				Connection db = null;
				if(runspec.domain != ModelDomain.NATIONAL_ALLOCATION) {
					// Check the scale input database
					shouldCloseDatabase = true;
					db = runspec.scaleInputDatabase.openConnectionOrNull();
					if(db == null) {
						isOk = false;
					}
				} else {
					db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
				}

				if(isOk && db != null) {
					if(runspec.domain != ModelDomain.NATIONAL_ALLOCATION && runspec.timeSpan.years.size() == 1) {
						// Check the year in the custom database, make sure it still matches the year selected
						String sql = "select yearID from year where yearID=" + runspec.timeSpan.years.first();
						try {
							int yearID = (int)SQLRunner.executeScalar(db,sql);
							if(yearID <= 0) {
								isOk = false;
							}
						} catch(Exception e) {
							isOk = false;
							Logger.logError(e,
									"Verifying runspec year entry for the Macroscale Geographic Bounds database failed.");
						}
					}
					for(Iterator i=runspec.geographicSelections.iterator();isOk && i.hasNext();) {
						GeographicSelection selection = (GeographicSelection)i.next();
						String sql;
						PreparedStatement statement = null;
						ResultSet results = null;
						try {
							if(selection.type == GeographicSelectionType.STATE) {
								sql = "SELECT stateID FROM state WHERE stateID = ?";
								statement = db.prepareStatement(sql);
								statement.setInt(1,selection.databaseKey);
							} else if(selection.type == GeographicSelectionType.COUNTY) {
								sql = "SELECT countyID FROM county " +
										"WHERE countyID = ?";
								statement = db.prepareStatement(sql);
								statement.setInt(1,selection.databaseKey);
							} else {
								continue;
							}
							results = SQLRunner.executeQuery(statement,sql);
							if(results == null || !results.next()) {
								isOk = false;
								break;
							}
						} catch(Exception e) {
							isOk = false;
							Logger.logError(e,
									"Verifying runspec entries for the Macroscale Geographic Bounds failed.");
						} finally {
							if(results != null) {
								try {
									results.close();
								} catch(Exception e) {
									// Nothing to do here
								}
								results = null;
							}
							if(statement != null) {
								try {
									statement.close();
								} catch(Exception e) {
									// Nothing to do here
								}
								statement = null;
							}
						}
					}
				}
				if(db != null && shouldCloseDatabase) {
					DatabaseUtilities.closeConnection(db);
					db = null;
				}
			}
		}

		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		if(!isOk) {
			status.status = RunSpecSectionStatus.NOT_READY;
		}
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
		runspec.geographicSelections.clear();
		runspec.genericCounty = null;
		runspec.scaleInputDatabase = new DatabaseSelection();
		domain = null;
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
		runspec.geographicSelections.clear();
		loadFromRunSpec(runspec); // load only scale-appropriate data
		saveToRunSpec(runspec); // and save it back
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/** load all states from the database into the stateList listbox **/
	public void loadStates() {
		stateListModel.removeAllElements();

		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(null == db) {
			JOptionPane.showMessageDialog(this, "Database Connection unavailable");
			GeographicSelection gsToAdd = new GeographicSelection();
			gsToAdd.databaseKey = 1;
			gsToAdd.textDescription = "Arkansas";
			gsToAdd.type = GeographicSelectionType.STATE;
			stateListModel.addElement(gsToAdd);
			gsToAdd = new GeographicSelection ();
			gsToAdd.databaseKey = 2;
			gsToAdd.textDescription = "California";
			gsToAdd.type = GeographicSelectionType.STATE;
			stateListModel.addElement(gsToAdd);
			gsToAdd = new GeographicSelection ();
			gsToAdd.databaseKey = 3;
			gsToAdd.textDescription = "Colorado";
			gsToAdd.type = GeographicSelectionType.STATE;
			stateListModel.addElement(gsToAdd);
			gsToAdd = new GeographicSelection ();
			gsToAdd.databaseKey = 4;
			gsToAdd.textDescription = "Florida";
			gsToAdd.type = GeographicSelectionType.STATE;
			stateListModel.addElement(gsToAdd);
			gsToAdd = new GeographicSelection ();
			gsToAdd.databaseKey = 5;
			gsToAdd.textDescription = "Michigan";
			gsToAdd.type = GeographicSelectionType.STATE;
			stateListModel.addElement(gsToAdd);
			gsToAdd = new GeographicSelection ();
			gsToAdd.databaseKey = 6;
			gsToAdd.textDescription = "Oregon";
			gsToAdd.type = GeographicSelectionType.STATE;
			stateListModel.addElement(gsToAdd);
			gsToAdd = new GeographicSelection ();
			gsToAdd.databaseKey = 7;
			gsToAdd.textDescription = "Washington";
			gsToAdd.type = GeographicSelectionType.STATE;
			stateListModel.addElement(gsToAdd);
			gsToAdd = new GeographicSelection ();
			gsToAdd.databaseKey = 8;
			gsToAdd.textDescription = "Wyoming";
			gsToAdd.type = GeographicSelectionType.STATE;
			stateListModel.addElement(gsToAdd);
			return;
		}

		String sql = "SELECT stateID, stateName FROM state ORDER BY stateName";
		try {
			PreparedStatement statement = db.prepareStatement(sql);
			ResultSet results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					GeographicSelection gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = results.getInt(1);
					gsToAdd.textDescription = results.getString(2);
					gsToAdd.type = GeographicSelectionType.STATE;
					stateListModel.addElement(gsToAdd);
				}
				results.close();
			}
			statement.close();
		} catch(Exception e) {
			Logger.logError(e, "Unable to load a list of states for the Macroscale Geographic"+
					" Bounds");
		}
	}

	/** load all the counties for all selected states into the countyList listbox **/
	public void loadCounties() {
		countyListModel.removeAllElements();
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		Object[] stateItems = stateList.getSelectedValuesList().toArray();
		for(int i=0;i<stateItems.length;i++) {
			GeographicSelection selectedStateItem = (GeographicSelection)(stateItems[i]);
			if(null == db) {
				JOptionPane.showMessageDialog(this, "Database Connection unavailable");
				if(selectedStateItem.textDescription.equals("Michigan")) {
					GeographicSelection gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 1;
					gsToAdd.textDescription = "Bay";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 2;
					gsToAdd.textDescription = "Claire";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 3;
					gsToAdd.textDescription = "Gennesee";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 4;
					gsToAdd.textDescription = "Grand Traverse";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 5;
					gsToAdd.textDescription = "Macomb";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 6;
					gsToAdd.textDescription = "Oakland";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 7;
					gsToAdd.textDescription = "Tuscola";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 8;
					gsToAdd.textDescription = "Van Buren";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 9;
					gsToAdd.textDescription = "Washtenaw";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 10;
					gsToAdd.textDescription = "Wayne";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
				} else {
					GeographicSelection gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 1;
					gsToAdd.textDescription = "County#1";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 2;
					gsToAdd.textDescription = "County#2";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 3;
					gsToAdd.textDescription = "County#3";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
					gsToAdd = new GeographicSelection();
					gsToAdd.databaseKey = 4;
					gsToAdd.textDescription = "County#4";
					gsToAdd.type = GeographicSelectionType.COUNTY;
					countyListModel.addElement(gsToAdd);
				}
				return;
			}
			String sql = "SELECT countyID, countyName, stateName FROM county, state " +
					"WHERE county.stateID = state.stateID AND state.stateID = ? " +
					"ORDER BY countyName";
			try {
				PreparedStatement statement = db.prepareStatement(sql);
				statement.setInt(1,selectedStateItem.databaseKey);
				ResultSet results = SQLRunner.executeQuery(statement,sql);
				if(results != null) {
					while(results.next()) {
						GeographicSelection gsToAdd = new GeographicSelection();
						gsToAdd.databaseKey = results.getInt(1);
						gsToAdd.textDescription = results.getString(3)
								+ " - " + results.getString(2);
						gsToAdd.type = GeographicSelectionType.COUNTY;
						countyListModel.addElement(gsToAdd);
					}
					results.close();
				}
				statement.close();
			} catch(Exception e) {
				Logger.logError(e,"Unable to load a list of counties for the Macroscale "+
						"Geographic Bounds");
			}
		}
	}

	/**
	 * Populate a generic county from the user interface values
	 * @param g GenericCounty to be populated
	**/
	void toGenericCounty(GenericCounty g) {
		if(g == null) {
			return;
		}
		// No need to push the state ID from genericCountyState

		String t = genericCountyID.getText();
		Integer i = null;
		try {
			i = new Integer(t);
		} catch(Exception e) {
			i = null;
		}
		g.shortCountyID = i == null? 0 : i.intValue();

		g.description = genericCountyName.getText();
		if(g.description.length() > 50) {
			g.description = g.description.substring(0,50);
		}

		t = genericCountyGPAFraction.getText();
		Double d = null;
		try {
			d = new Double(t);
		} catch(Exception e) {
			d = null;
		}
		g.gpaFraction = (float)(d == null? 0 : d.doubleValue());

		t = genericCountyBarometricPressure.getText();
		d = null;
		try {
			if(t.length() > 0) {
				d = new Double(t);
			}
		} catch(Exception e) {
			d = null;
		}
		g.barometricPressure = (float)(d == null? 0 : d.doubleValue());

		t = genericCountyRefuelingVaporProgramAdjust.getText();
		d = null;
		try {
			d = new Double(t);
		} catch(Exception e) {
			d = null;
		}
		g.refuelingVaporProgramAdjust = (float)(d == null? 0 : d.doubleValue());

		t = genericCountyRefuelingSpillProgramAdjust.getText();
		d = null;
		try {
			d = new Double(t);
		} catch(Exception e) {
			d = null;
		}
		g.refuelingSpillProgramAdjust = (float)(d == null? 0 : d.doubleValue());
	}

	/**
	 * Fill user interface controls with the values in a generic county.
	 * @param g source county.  If null, the default values in a new GenericCounty will be used.
	**/
	void fromGenericCounty(GenericCounty g) {
		if(g == null) {
			g = new GenericCounty();
		}
		genericCountyState.setText("" + g.stateID);
		genericCountyID.setText("" + g.shortCountyID);
		genericCountyName.setText(g.description == null? "":g.description);
		genericCountyGPAFraction.setText("" + g.gpaFraction);
		genericCountyRefuelingVaporProgramAdjust.setText("" + g.refuelingVaporProgramAdjust);
		genericCountyRefuelingSpillProgramAdjust.setText("" + g.refuelingSpillProgramAdjust);

		String pressureText = "";
		if(g.barometricPressure > 0) {
			pressureText = "" + g.barometricPressure;
		}
		genericCountyBarometricPressure.setText(pressureText);
	}

	/** Create and arrange the generic county panel **/
	void createGenericCountyPanel() {
		JLabel label1;
		JLabel label2;
		JLabel label3;
		JLabel label4;
		JLabel label5;
		JPanel panel1;
		JLabel label6;
		JLabel label7;
		JLabel label8;
		JLabel label9;
		JLabel label10;
		JLabel label11;
		JLabel label12;

		genericCountyPanel = new JPanel();
		genericCountyPanel.setBorder(BorderFactory.createTitledBorder("Generic County"));

		label1 = new JLabel();
		genericCountyState = new JLabel();
		label2 = new JLabel();
		genericCountyID = new JTextField();
		label3 = new JLabel();
		label4 = new JLabel();
		genericCountyName = new JTextField();
		label5 = new JLabel();
		panel1 = new JPanel();
		label6 = new JLabel();
		genericCountyGPAFraction = new JTextField();
		label7 = new JLabel();

		genericCountyBarometricPressure = new JTextField();
		label8 = new JLabel();
		genericCountyRefuelingVaporProgramAdjust = new JTextField();
		label9 = new JLabel();
		label11 = new JLabel();
		genericCountyRefuelingSpillProgramAdjust = new JTextField();
		label10 = new JLabel();
		label12 = new JLabel();

		//======== this ========
		genericCountyPanel.setLayout(new GridBagLayout());
		((GridBagLayout)genericCountyPanel.getLayout()).columnWidths = new int[] {0, 0, 0, 0};
		((GridBagLayout)genericCountyPanel.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0};
		((GridBagLayout)genericCountyPanel.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};
		((GridBagLayout)genericCountyPanel.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4};

		//---- label1 ----
		label1.setText("State ID:");
		genericCountyPanel.add(label1, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));
		genericCountyPanel.add(genericCountyState, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- label2 ----
		label2.setText("County ID:");
		genericCountyPanel.add(label2, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- genericCountyID ----
		genericCountyID.setColumns(3);
		genericCountyPanel.add(genericCountyID, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- label3 ----
		label3.setText("1-999, labels the county within a state.");
		genericCountyPanel.add(label3, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 0), 0, 0));

		//---- label4 ----
		label4.setText("Description:");
		genericCountyPanel.add(label4, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- genericCountyName ----
		genericCountyName.setColumns(20);
		genericCountyPanel.add(genericCountyName, new GridBagConstraints(1, 2, 2, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 0), 0, 0));

		//---- label6 ----
		label6.setText("GPA Fraction:");
		genericCountyPanel.add(label6, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));

		//---- genericCountyGPAFraction ----
		genericCountyGPAFraction.setColumns(6);
		genericCountyPanel.add(genericCountyGPAFraction, new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));

		//---- label7 ----
		label7.setText("Fraction of county within a fuel Geographic Phase-in Area");
		genericCountyPanel.add(label7, new GridBagConstraints(2, 3, 2, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 0), 0, 0));

		//---- label8 ----
		label8.setText("Bar. Pressure:");
		genericCountyPanel.add(label8, new GridBagConstraints(0, 5, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		genericCountyBarometricPressure.setColumns(6);
		genericCountyPanel.add(genericCountyBarometricPressure, new GridBagConstraints(1, 5, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- label5 ----
		label5.setText("inHg (avg. for low altitude is 28.9, avg. for high is 24.6)");
		genericCountyPanel.add(label5, new GridBagConstraints(2, 5, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- label9 ----
		label9.setText("Vapor Adjust:");
		genericCountyPanel.add(label9, new GridBagConstraints(0, 6, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		genericCountyRefuelingVaporProgramAdjust.setColumns(6);
		genericCountyPanel.add(genericCountyRefuelingVaporProgramAdjust, new GridBagConstraints(1, 6, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		label11.setText("Refueling Vapor Program Adjustment Fraction");
		genericCountyPanel.add(label11, new GridBagConstraints(2, 6, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 0), 0, 0));

		//---- label10 ----
		label10.setText("Spill Adjust:");
		genericCountyPanel.add(label10, new GridBagConstraints(0, 7, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		genericCountyRefuelingSpillProgramAdjust.setColumns(6);
		genericCountyPanel.add(genericCountyRefuelingSpillProgramAdjust, new GridBagConstraints(1, 7, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		label12.setText("Refueling Spill Program Adjustment Fraction");
		genericCountyPanel.add(label12, new GridBagConstraints(2, 7, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 0), 0, 0));

		genericCountyID.addFocusListener(this);
		genericCountyName.addFocusListener(this);
		genericCountyGPAFraction.addFocusListener(this);
		genericCountyBarometricPressure.addFocusListener(this);
		genericCountyRefuelingVaporProgramAdjust.addFocusListener(this);
		genericCountyRefuelingSpillProgramAdjust.addFocusListener(this);
	}

	/**
	 * Handles the focus lost event for the server textfield, and checkboxes.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		JComponent c = (JComponent)e.getComponent();
		if(c == genericCountyID
				|| c == genericCountyName
				|| c == genericCountyGPAFraction
				|| c == genericCountyBarometricPressure
				|| c == genericCountyRefuelingVaporProgramAdjust
				|| c == genericCountyRefuelingSpillProgramAdjust) {
			handleGenericCountyChange();
		}
		if(c == server) {
			if(previousServer.equalsIgnoreCase(server.getText())) {
				return;
			}
			previousServer = server.getText();
			loadDatabases();
			displaySectionStatus();
		}
	}

	/**
	 * Currently not used.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
	}

	/** Create and arrange the database panel **/
	void createDatabasePanel() {
		JLabel label4;
		JLabel label5;

		databasePanel = new JPanel();
		databasePanel.setBorder(BorderFactory.createTitledBorder("Domain Input Database"));

		databasePurposeLabel = new JLabel();
		label4 = new JLabel();
		label5 = new JLabel();
		editDatabaseButton = new JButton();
		editDatabaseButton.addActionListener(this);
		refreshDatabaseButton = new JButton();
		refreshDatabaseButton.addActionListener(this);
		checkDatabaseButton = new JButton();
		checkDatabaseButton.addActionListener(this);

		databasePanel.setLayout(new GridBagLayout());
		((GridBagLayout)databasePanel.getLayout()).columnWidths = new int[] {38, 73, 57, 0, 0, 0, 0};
		((GridBagLayout)databasePanel.getLayout()).rowHeights = new int[] {0, 0, 0, 0};
		((GridBagLayout)databasePanel.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0E-4};
		((GridBagLayout)databasePanel.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};

		//---- databasePurposeLabel ----
		databasePurposeLabel.setText("The County domain scale requires a database of detailed data.");
		databasePanel.add(databasePurposeLabel, new GridBagConstraints(0, 0, 6, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 0), 0, 0));

		//---- label4 ----
		label4.setText("Server:");
		databasePanel.add(label4, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- server ----
		server = new JTextField(10);
		server.setText("localhost");
		ToolTipHelper.add(server,
				"Edit the name of the server where the database will be located");
		server.setName("server");
		server.addFocusListener(this);
		server.setColumns(10);
		databasePanel.add(server, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- refreshDatabaseButton ----
		refreshDatabaseButton.setText("Refresh");
		databasePanel.add(refreshDatabaseButton, new GridBagConstraints(4, 1, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));

		//---- label5 ----
		label5.setText("Database:");
		databasePanel.add(label5, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));

		//---- databaseCombo ----
		databaseCombo = new ExtendedComboBox<String>();
		Dimension d = databaseCombo.getPreferredSize();
		databaseCombo.setPreferredSize(new Dimension(450, d.height)); // 250
		databaseCombo.setPopupWidth(databaseCombo.getPreferredSize().width);
		databaseCombo.setName("databaseCombo");
		databaseCombo.addActionListener(this);
		databaseCombo.setEditable(false); // Force users to pick or use the editDatabaseButton
		databaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(databaseCombo,
				"Edit or select the name of the database in which the data will be stored");
		databasePanel.add(databaseCombo, new GridBagConstraints(1, 2, 3, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));

		//---- editDatabaseButton ----
		editDatabaseButton.setText("Enter/Edit Data");
		databasePanel.add(editDatabaseButton, new GridBagConstraints(4, 2, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));

		/*
		//---- checkDatabaseButton ----
		checkDatabaseButton.setText("Check Database");
		databasePanel.add(checkDatabaseButton, new GridBagConstraints(3, 3, 2, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));
		*/
	}

	/**
	 * Loads the Databases droplist, based on the server setting.
	 * Also, sets the default droplist selection, if it can be found.
	**/
	public void loadDatabases() {
		databaseCombo.removeAllItems();
		// add the default item (no selection)
		databaseCombo.addItem(new String(""));
		TreeSet<String> databases = new TreeSet<String>();
		// get the available databases from the current server selection
		Connection db = openCurrentDatabase(true);
		if(null == db) {
			Logger.log(LogMessageCategory.ERROR,"Could not connect to the database");
			return;
		}
		String sql = "SHOW DATABASES";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					String nextDB = results.getString(1);
					if(!invalidDatabaseNames.contains(nextDB)) {
						databases.add(nextDB);
					}
				}
			}
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show databases (load databases) in ImporterGUI.");
			DatabaseUtilities.closeConnection(db);
			return;
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				results = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
		// second pass through the returned list of database names, must now
		// remove any databases that aren't an output database
		ArrayList<String> stringsToRemove = new ArrayList<String>();
		try {
			boolean foundOutputTables = false;
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				foundOutputTables = false;
				String nextDatabase = (String)i.next();
				if(nextDatabase.length() == 0 || invalidDatabaseNames.contains(nextDatabase)) {
					continue;
				}
				// look at all tables from the next databaseName, compare the table
				// names to one output table name
				foundOutputTables = true;
				/*
				sql = "SHOW TABLES FROM " + nextDatabase;
				try {
					statement = db.prepareStatement(sql);
					results = SQLRunner.executeQuery(statement,sql);
					if(results != null) {
						int foundCount = 0;
						while(results.next()) {
							String nextTable = results.getString(1);
							if(manager.requiredTableNames.contains(nextTable)) {
								foundCount++;
								if(foundCount >= manager.requiredTableNames.size()) {
									foundOutputTables = true;
									break;
								}
							}
						}
					}
				} catch (Exception e) {
					// SQL error here just means this database not an output database
				} finally {
					if(results != null) {
						try {
							results.close();
						} catch(Exception e) {
							// Nothing to do here
						}
						results = null;
					}
					if(statement != null) {
						try {
							statement.close();
						} catch(Exception e) {
							// Nothing to do here
						}
						statement = null;
					}
				}
				*/
				// check if this database has any output tables, if not must add
				// the databaseName to the remove names list
				if(!foundOutputTables) {
					stringsToRemove.add(nextDatabase);
				}
			}
			// now run through any database names to remove (i.e. databases that don't
			// contain an output table)
			for(int i = 0; i < stringsToRemove.size(); i++) {
				databases.remove(stringsToRemove.get(i));
			}
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				String nextDB = (String)i.next();
				databaseCombo.addItem(nextDB);
			}
			Vector<String> toolTipVector = new Vector<String>();
			for(int i=0; i<databaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) databaseCombo.getItemAt(i));
			}
			String[] toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			databaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show tables from database in ImporterGUI.");
		}
		// set the default selection
		databaseCombo.setSelectedItem("");
		DatabaseUtilities.closeConnection(db);
	}

	/**
	 * Add a database name to databaseCombo but only if it isn't already in the
	 * the list.
	 * @param newDatabaseName name of the database to attempt to place into databaseCombo
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInComboBox(String newDatabaseName) {
		newDatabaseName = newDatabaseName.trim();
		ComboBoxModel model = databaseCombo.getModel();
		for(int i = 0; i < model.getSize(); i++) {
			String t = (String)model.getElementAt(i);
			if(t.equalsIgnoreCase(newDatabaseName)) {
				return t;
			}
		}
		databaseCombo.addItem(newDatabaseName);
		return newDatabaseName;
	}

	/**
	 * Handles the database combo change.
	 * @return false if the selected database is not valid, or not yet selected.
	**/
	public boolean processDatabaseComboChange() {
		if(databaseCombo.getSelectedItem() == null) {
			displaySectionStatus();
			return false;
		}
		if(databaseCombo.getSelectedItem().toString().length() == 0) {
			displaySectionStatus();
			return false;
		}

		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			displaySectionStatus();
			return false;
		}

		saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName = StringUtilities.safeGetString(newDatabaseName);

		String status = MOVESEngine.isOutputDatabaseNameValid(dbSelection);
		if(status == null) {
			// try to connect to the new selection
			Connection db = openCurrentDatabase(false);
			if(null == db) {
				displaySectionStatus();
				return false;
			}
			try {
				ArrayList<String> messages = new ArrayList<String>();
				int result =
						ImporterManager.isCountyDomainDatabase(MOVESAPI.getTheAPI().getRunSpec(),
						messages,db,true);
				if(result < 0) {
					// Display the error messages
					String t = "Unable to use this entry as a County Domain database.";
					for(Iterator<String> i=messages.iterator();i.hasNext();) {
						t += "\r\n";
						t += i.next();
					}
					t += "\r\n\r\nUse the database anyway?";
					if(JOptionPane.showConfirmDialog(this, t, "Error",
							JOptionPane.ERROR_MESSAGE + JOptionPane.YES_NO_OPTION)
							== JOptionPane.YES_OPTION) {
						result = 0;
					}
					if(result < 0) {
						databaseCombo.setSelectedItem("");
						displaySectionStatus();
						return false;
					}
				}
			} finally {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
			newDatabaseName = addIfNotInComboBox(newDatabaseName);
			databaseCombo.setSelectedItem(newDatabaseName);
		}
		displaySectionStatus();
		if(status != null) {
			JOptionPane.showMessageDialog(this, status);
		}
		return status == null;
	}

	/**
	 * Attempt to open a connection to the server and database shown on screen.
	 * @param settleForJustServer true if it is OK to just connect to the server without a valid
	 * database named.
	 * @return a Connection object that should be closed with DatabaseUtilities.closeConnection
	 * and not by returning it to the DatabaseConnectionManager.
	**/
	Connection openCurrentDatabase(boolean settleForJustServer) {
		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)databaseCombo.getSelectedItem());
		Connection db = dbSelection.openConnectionOrNull();
		if(null == db && settleForJustServer) {
			// try again to get a connection, but specify an empty string for the database
			// name, in case the current one is invalid, this will at least allow us to get
			// a connection to the server to get the database list
			dbSelection.databaseName = "";
			db = dbSelection.openConnectionOrNull();
		}
		return db;
	}

	/** Handle the Edit Database button **/
	void handleEditDatabaseButton() {
		// Fill the RunSpec's scaleInputDatabase selection object then show the CDM GUI.
		saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());

		ArrayList<String> messages = new ArrayList<String>();
		int result = ImporterManager.isReadyForCountyDomain(null,messages);
		if(result < 0) {
			// Display the error messages
			String t = "";
			if(MOVESAPI.getTheAPI().getRunSpec().domain == ModelDomain.SINGLE_COUNTY) {
				t = "Unable to open the County Data Manager.";
			} else if(MOVESAPI.getTheAPI().getRunSpec().domain == ModelDomain.PROJECT) {
				t = "Unable to open the Project Data Manager.";
			}
			for(Iterator<String> i=messages.iterator();i.hasNext();) {
				t += "\r\n";
				t += i.next();
			}
			JOptionPane.showMessageDialog(this, t, "Error",	JOptionPane.ERROR_MESSAGE);
			return;
		}
		ImporterManager.display(MOVESNavigation.singleton.parent,null,
				MOVESNavigation.singleton.parent.getLocationOnScreen().x + 50,
				MOVESNavigation.singleton.parent.getLocationOnScreen().y + 50,
				MOVESAPI.getTheAPI().getRunSpec().domain,ImporterManager.STANDARD_MODE,
				Models.evaluateModels(MOVESAPI.getTheAPI().getRunSpec().models));

		loadDatabases();
		loadFromRunSpec(MOVESAPI.getTheAPI().getRunSpec());
	}

	/**
	 * Handle the Check Database button
	**/
	void handleCheckDatabaseButton() {
		if(!processDatabaseComboChange()) {
			JOptionPane.showMessageDialog(this, "The database cannot be used.", "Error",
					JOptionPane.ERROR_MESSAGE);
		} else {
			JOptionPane.showMessageDialog(this, "The database can be used.", "Success",
					JOptionPane.INFORMATION_MESSAGE);
		}
	}

	/** Fill invalidDatabaseNames **/
	void generateListOfInvalidDatabaseNames() {
		DatabaseSelection defaultDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
		//default database cannot be accepted as an input selection
		invalidDatabaseNames.add(defaultDB.databaseName);
		//execution database cannot be accepted as an input selection
		DatabaseSelection executionDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()];
		invalidDatabaseNames.add(executionDB.databaseName);
		//Worker database cannot be accepted as an input selection
		invalidDatabaseNames.add("MOVESWorker");
		//MySQL database cannot be accepted as an input selection
		invalidDatabaseNames.add("MySQL");
	}

	/**
	 * Check a proposed database selection for appropriateness, prompting the user
	 * if the database is not acceptable.
	 * @param dbSelection the database selection.
	 * @return true if the database name is valid.
	**/
	boolean validDatabaseName(DatabaseSelection dbSelection) {
		if(invalidDatabaseNames.contains(dbSelection.databaseName)) {
			JOptionPane.showMessageDialog(this,
					"The " + dbSelection.databaseName + " database cannot be used here.");
			return false;
		}

		return true;
	}

	/**
	 * Reload the list of databases, defaulting back to no selection.
	**/
	void handleRefreshDatabaseButton() {
		loadDatabases();
		displaySectionStatus();
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
		assessSituation(runspec);
		return null;
	}

	/**
	 * Enable/Disable controls based on their interaction rules.
	 * @param runspec the RunSpec to examine
	**/
	void assessSituation(RunSpec runspec) {
		onroadSelected = runspec.models.contains(Model.ONROAD);

		/*
		if(nationalWarningPanel != null) {
			nationalWarningPanel.setVisible(onroadSelected);
		}
		if(localWarningPanel != null) {
			localWarningPanel.setVisible(onroadSelected);
		}
		*/

		if(onroadSelected && domain != ModelDomain.SINGLE_COUNTY && domain != ModelDomain.PROJECT) {
			localWarningPanel.setVisible(true);
		}
		if(nation.isSelected()) {
			// Nothing to do here, Nation is always valid
		} else if (state.isSelected()) {
			if(onroadSelected && domain == ModelDomain.NATIONAL_ALLOCATION) {
				nationalWarningPanel.setVisible(true);
			}
		} else if (county.isSelected()) {
			if(onroadSelected && domain == ModelDomain.NATIONAL_ALLOCATION) {
				nationalWarningPanel.setVisible(true);
			}
		}
	}
}
