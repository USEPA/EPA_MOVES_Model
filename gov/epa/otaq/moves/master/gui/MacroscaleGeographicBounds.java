/**************************************************************************************************
 * @(#)MacroscaleGeographicBounds.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import gov.epa.otaq.moves.common.Constants;
import gov.epa.otaq.moves.common.DatabaseSelection;
import gov.epa.otaq.moves.common.JListWithToolTips;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import gov.epa.otaq.moves.common.Model;
import gov.epa.otaq.moves.common.ModelDomain;
import gov.epa.otaq.moves.common.ModelScale;
import gov.epa.otaq.moves.common.Models;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.common.ToolTipHelper;
import gov.epa.otaq.moves.common.TreeSetIgnoreCase;
import gov.epa.otaq.moves.master.framework.DatabaseConnectionManager;
import gov.epa.otaq.moves.master.runspec.GenericCounty;
import gov.epa.otaq.moves.master.runspec.GeographicOutputDetailLevel;
import gov.epa.otaq.moves.master.runspec.GeographicSelection;
import gov.epa.otaq.moves.master.runspec.GeographicSelectionType;
import gov.epa.otaq.moves.master.runspec.RunSpec;

/**
 * Class for geography selection at Macroscale and Mescoscale Lookup.
 * The panel consists of  States list,  a Counties list, and a list for selections.
 * There are Add, Delete, and Select All buttons
 * to prepare a list of States or Counties.
 *
 * @author		Wesley Faler
 * @author		Mitch C. (minor mods)
 * @author		Tim Hull
 * @author  	Bill Shaw (508 compliance mods)
 * @author      John Covey (Task 1903)
 * @author		Mike Kender (Task 2003)
 * @version     2020-07-27
**/
public class MacroscaleGeographicBounds extends JPanel implements ListSelectionListener,
		ActionListener, FocusListener, RunSpecEditor {	/** Singleton for the navigation panel **/
	public MOVESWindow parent;
	public static MacroscaleGeographicBounds singleton = null;
	
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
	public MacroscaleGeographicBounds(MOVESWindow p) {
		parent = p;
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

		Dimension buttonSize = new Dimension(87,25);
		stateButtonPanel = new JPanel();
		countyButtonPanel = new JPanel();

		stateLabel = new JLabel("States (Alt+2):");
		stateLabel.setName("stateLabel");
		countyLabel = new JLabel("Counties (FIPS code):");
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
		stateList.setToolTipText(Constants.GEOGRAPHIC_BOUNDS_STATES_TOOLTIP);

		stateList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		stateList.setSelectedIndex(-1);
		stateList.addListSelectionListener(this);
		stateList.setVisibleRowCount(9);
		stateList.setPrototypeCellValue(prototypeValue);
		stateList.addKeyListener(new KeyListener() {
			public void keyReleased(KeyEvent e) {
				if (stateList.getModel().getSize() == 0) {
					JOptionPane.showMessageDialog(null, "no 'States' to select");
	            } else if (stateList.isSelectionEmpty()) {
	            	stateList.setSelectedIndex(0);
	            }
			}

			public void keyTyped(KeyEvent e) {
				//nothing to do, for now
			}

			public void keyPressed(KeyEvent e) {
				//nothing to do, for now
			}
		});

		stateScrollPane = new JScrollPane(stateList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		stateScrollPane.setName("stateScrollPane");
		stateLabel.setDisplayedMnemonic('2');
		stateLabel.setLabelFor(stateScrollPane);
		
		stateLabel.addFocusListener(new FocusListener() {
			
			@Override
			public void focusLost(FocusEvent e) {
				//nothing special here, yet
			}
			
			@Override
			public void focusGained(FocusEvent e) {
				if(stateList.getSelectedIndices().length > 0) {
					stateList.setSelectedIndex(stateList.getSelectedIndices()[0]);
				}
			}
		});

		countyList = new JListWithToolTips<GeographicSelection>(countyListModel);
		countyList.setName("countyList");
		countyList.setToolTipText(Constants.GEOGRAPHIC_BOUNDS_COUNTIES_TOOLTIP);
		countyList.setSelectionMode(
			ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		countyList.setSelectedIndex(-1);
		countyList.addListSelectionListener(this);
		countyList.setVisibleRowCount(9);
		countyList.setPrototypeCellValue(prototypeValue);
		countyList.addKeyListener(new KeyListener() {
			public void keyReleased(KeyEvent e) {
				if (countyList.getModel().getSize() == 0) {
					JOptionPane.showMessageDialog(null, "no 'Counties' to select");
	            } else if (countyList.isSelectionEmpty()) {
	            	countyList.setSelectedIndex(0);
	            }
			}

			public void keyTyped(KeyEvent e) {
				//nothing to do, for now
			}

			public void keyPressed(KeyEvent e) {
				//nothing to do, for now
			}
		});

		countyScrollPane = new JScrollPane(countyList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		countyScrollPane.setName("countyScrollPane");
		countyLabel.setDisplayedMnemonic('o');
		countyLabel.setLabelFor(countyScrollPane);

		selectionList = new JListWithToolTips<GeographicSelection>(selectionListModel);
		selectionList.setName("selectionList");
		selectionList.setSelectionMode(
			ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		selectionList.setSelectedIndex(-1);
		selectionList.addListSelectionListener(this);
		selectionList.setVisibleRowCount(9);
		selectionList.setPrototypeCellValue(prototypeValue);
		selectionList.setToolTipText(Constants.GEOGRAPHIC_BOUNDS_SELECTIONS_TOOLTIP);
		selectionList.addKeyListener(new KeyListener() {
			public void keyReleased(KeyEvent e) {
				if (selectionList.getModel().getSize() == 0) {
					JOptionPane.showMessageDialog(null, "no 'Selections' to select");
	            } else if (selectionList.isSelectionEmpty()) {
	            	selectionList.setSelectedIndex(0);
	            }
			}

			public void keyTyped(KeyEvent e) {
				//nothing to do, for now
			}

			public void keyPressed(KeyEvent e) {
				//nothing to do, for now
			}
		});

		selectionScrollPane = new JScrollPane(selectionList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane. HORIZONTAL_SCROLLBAR_AS_NEEDED);
		selectionScrollPane.setName("selectionScrollPane");
		selectionLabel.setDisplayedMnemonic('i');
		selectionLabel.setLabelFor(selectionScrollPane);

		stateSelectAll = new JButton("Select All (Alt+3)");
		stateSelectAll.setMnemonic('3');
		stateSelectAll.setName("stateSelectAll");
		stateSelectAll.addActionListener(this);
//		stateSelectAll.setPreferredSize(new Dimension(150,25));
		ToolTipHelper.add(stateSelectAll,"Select all states");
		stateAdd = new JButton("Add");
		stateAdd.setName("stateAdd");
		stateAdd.setEnabled(false); // not enabled until item selected from list
		stateAdd.addActionListener(this);
		stateAdd.setMnemonic('d');
		stateAdd.setDisplayedMnemonicIndex(1);
		stateAdd.setPreferredSize(buttonSize);
		ToolTipHelper.add(stateAdd,"Add selected states to selection list");
		countySelectAll = new JButton("Select All");
		countySelectAll.setName("countySelectAll");
		countySelectAll.addActionListener(this);
		countySelectAll.setPreferredSize(buttonSize);
		countySelectAll.setMnemonic('c');
		countySelectAll.setDisplayedMnemonicIndex(4);
		ToolTipHelper.add(countySelectAll,"Select all counties");
		countyAdd = new JButton("Add");
		countyAdd.setName("countyAdd");
		countyAdd.setEnabled(false); // not enabled until item selected from list
		countyAdd.addActionListener(this);
		countyAdd.setMnemonic('d');
		countyAdd.setDisplayedMnemonicIndex(1);
		countyAdd.setPreferredSize(buttonSize);
		ToolTipHelper.add(countyAdd,"Add selected counties to selection list");
		selectionDelete = new JButton("Delete");
		selectionDelete.setName("selectionDelete");
		selectionDelete.setEnabled(false); // not enabled until item selected from list
		selectionDelete.addActionListener(this);
		selectionDelete.setMnemonic('l');
		selectionDelete.setDisplayedMnemonicIndex(2);
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
				+ "Caution: You have selected Default Scale with detail at the State or County level.<br>"
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
		
		Dimension d = genericCountyPanel.getPreferredSize();

		d.width = 800;
		genericCountyPanel.setPreferredSize(d);

		JPanel centerPanel = new JPanel();
		centerPanel.setLayout(new BoxLayout(centerPanel,BoxLayout.Y_AXIS));
		centerPanel.add(stateCountiesPanel);
		centerPanel.add(genericCountyPanel);
		genericCountyPanel.setVisible(false);

		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 6, 7);
		add(centerPanel, gbc);

		messageLogPanel.setLayout(new BoxLayout(messageLogPanel, BoxLayout.Y_AXIS));
		JLabel gbrLabel = new JLabel("Geographic Bounds Requirements");
		messageLogPanel.add(gbrLabel);
		messageLogPanel.add(messageLogPane);

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
					/*&& (county.isSelected() || zoneLink.isSelected()) */ ) {
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
		if (e.getSource() == stateSelectAll) {
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
		}
	}

	/** Handles the Nation button. **/
	public void processNationButton() {
		selectionListModel.removeAllElements();

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
			this.selectionListModel.removeAllElements();
		}
		// repopulate the states list control
		this.loadStates();

		this.stateLabel.setVisible(true);
		this.countyLabel.setVisible(false);
		this.selectionLabel.setVisible(true);
		this.stateScrollPane.setVisible(true);
		this.countyScrollPane.setVisible(false);
		this.selectionScrollPane.setVisible(true);
		this.stateSelectAll.setVisible(true);
		this.stateAdd.setVisible(true);
		this.countySelectAll.setVisible(false);
		this.countyAdd.setVisible(false);
		this.selectionDelete.setVisible(true);
		this.previousRBSetting = "S";
		this.displaySectionStatus();
	}

	/** Handles the County button. **/
	public void processCountyButton() {

		if(0 != previousRBSetting.compareTo("C")) {
			selectionListModel.removeAllElements();
		}
		// reload the states list
		loadStates();
		countyListModel.removeAllElements();

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
		if(PreaggregationOptions.singleton.customDomain.isSelected()) {
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
		MOVESNavigation.singleton.updateRunSpecSectionStatus();

		if (CreateInputDatabase.singleton.databaseCombo.getSelectedItem() != null) {
			CreateInputDatabase.singleton.processDatabaseComboChange();
			MOVESNavigation.singleton.updateOption(MOVESNavigation.singleton.createInputDatabaseOption);
		}
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
		if(messageLogPanel == null) {
			return;
		}
		messageLogPanel.setVisible(true);
		nationalWarningPanel.setVisible(false);
		localWarningPanel.setVisible(false);
		if(onroadSelected && domain != ModelDomain.SINGLE_COUNTY && domain != ModelDomain.PROJECT) {
			localWarningPanel.setVisible(true);
		}
		boolean didClear = false;
		if(PreaggregationOptions.singleton.nation.isSelected()) {
			if (domain == ModelDomain.NATIONAL_ALLOCATION) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("No selections need to be made here because Nation Preaggregation is selected on the Advanced Features Panel."));
			} else {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("Nation Preaggregation is selected on the Advanced Features Panel, which is not compatible with the current Scale selection."));
			}
		} else if (PreaggregationOptions.singleton.state.isSelected()) {
			if(selectionList.getModel().getSize() == 0) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("Please select a state."));
			}
			if(onroadSelected && domain == ModelDomain.NATIONAL_ALLOCATION) {
				nationalWarningPanel.setVisible(true);
			} else if (onroadSelected && domain != ModelDomain.NATIONAL_ALLOCATION) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("State Preaggregation is selected on the Advanced Features Panel, which is not compatible with the current Scale selection."));
			}
		} else if (PreaggregationOptions.singleton.county.isSelected()) {
			if(selectionList.getModel().getSize() == 0) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("Please select a state and county."));
			}
			if(onroadSelected && domain == ModelDomain.SINGLE_COUNTY && selectionList.getModel().getSize() > 1) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("Only one county may be selected at County Scale."));
			}
			if(onroadSelected && domain == ModelDomain.PROJECT && selectionList.getModel().getSize() > 1) {
				messageLogModel.clear();
				didClear = true;
				messageLogPanel.setVisible(true);
				messageLogModel.addElement(new String("Only one county may be selected at Project Scale."));
			}			
			if(onroadSelected && domain == ModelDomain.NATIONAL_ALLOCATION) {
				nationalWarningPanel.setVisible(true);
			}
		} else if(PreaggregationOptions.singleton.customDomain.isSelected()) {
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
	}

	/**
	 * Saves the info to a RunSpec.
	 * @param	runspec the RunSpec to get the info
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.geographicSelections.clear();
		if (PreaggregationOptions.singleton.state.isSelected() || PreaggregationOptions.singleton.county.isSelected() 
				|| PreaggregationOptions.singleton.zoneLink.isSelected()) {  
			runspec.genericCounty = null;
			for(int i=0;i<selectionListModel.getSize();i++) {
				runspec.geographicSelections.add(
						(GeographicSelection)selectionListModel.getElementAt(i));
			}
		} else if(PreaggregationOptions.singleton.nation.isSelected()) {
			runspec.genericCounty = null;
			GeographicSelection geographicSelection = new GeographicSelection();
			geographicSelection.type = GeographicSelectionType.NATION;
			runspec.geographicSelections.add(geographicSelection);
		} else if(PreaggregationOptions.singleton.customDomain.isSelected()) {
			GeographicSelection geographicSelection = new GeographicSelection();
			geographicSelection.type = GeographicSelectionType.COUNTY;
			if(runspec.genericCounty == null) {
				runspec.genericCounty = new GenericCounty();
			}
			toGenericCounty(runspec.genericCounty);
			geographicSelection.databaseKey = runspec.genericCounty.getCountyID();
			runspec.geographicSelections.add(geographicSelection);
		}
		if(PreaggregationOptions.singleton.nation.isSelected()) {
			runspec.outputEmissionsBreakdownSelection.estimateUncertainty=false;
			runspec.geographicOutputDetail = GeographicOutputDetailLevel.NATION;
		}
		if(PreaggregationOptions.singleton.state.isSelected()) {
			runspec.outputEmissionsBreakdownSelection.estimateUncertainty=false;
			if (runspec.geographicOutputDetail == GeographicOutputDetailLevel.COUNTY ||
					runspec.geographicOutputDetail == GeographicOutputDetailLevel.ZONE ||
					runspec.geographicOutputDetail == GeographicOutputDetailLevel.LINK) {
				runspec.geographicOutputDetail = GeographicOutputDetailLevel.STATE;
			}
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

		boolean isCustomDomain = runspec.isCustomDomain();

		Models.ModelCombination mc = runspec.getModelCombination();

		switch (mc) {
			case M2: // Nonroad
				PreaggregationOptions.singleton.nation.setEnabled(true);
				PreaggregationOptions.singleton.state.setEnabled(false);
				PreaggregationOptions.singleton.county.setEnabled(true);
				PreaggregationOptions.singleton.zoneLink.setEnabled(false);
				PreaggregationOptions.singleton.customDomain.setEnabled(false);

				if(!runspec.geographicSelections.isEmpty()) {
					Object firstElement = runspec.geographicSelections.iterator().next();
					GeographicSelection firstGS = (GeographicSelection) firstElement;
					if(firstGS.type == GeographicSelectionType.COUNTY) {
						PreaggregationOptions.singleton.county.setSelected(true);
						previousRBSetting = "C";
						processCountyButton();
						selectionListModel.removeAllElements();
						for(Iterator<GeographicSelection> i = runspec.geographicSelections.iterator(); i.hasNext();) {
							selectionListModel.addElement(i.next());
						}
					} else {
						PreaggregationOptions.singleton.nation.setSelected(true);
						previousRBSetting = "N";
						processNationButton();
					}
				} else {
					PreaggregationOptions.singleton.county.setSelected(true);
					previousRBSetting = "C";
					processCountyButton();
					selectionListModel.removeAllElements();
				}
				break;
			case M1: // Onroad
			default:
				if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
					PreaggregationOptions.singleton.nation.setEnabled(false);
					PreaggregationOptions.singleton.state.setEnabled(false);
					PreaggregationOptions.singleton.county.setEnabled(false);
					PreaggregationOptions.singleton.zoneLink.setEnabled(true);
					PreaggregationOptions.singleton.customDomain.setEnabled(false); // disabled Custom Domain option
					//PreaggregationOptions.singleton.customDomain.setEnabled(runspec.domain != ModelDomain.NATIONAL_ALLOCATION);
		
					if(isCustomDomain) {
						PreaggregationOptions.singleton.customDomain.setSelected(true);
						previousRBSetting = "D";
						processCustomDomainButton();
						selectionListModel.removeAllElements();
					} else {
						PreaggregationOptions.singleton.zoneLink.setSelected(true);
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
					PreaggregationOptions.singleton.nation.setEnabled(runspec.domain == ModelDomain.NATIONAL_ALLOCATION);
					PreaggregationOptions.singleton.state.setEnabled(runspec.domain == ModelDomain.NATIONAL_ALLOCATION);
					PreaggregationOptions.singleton.county.setEnabled(true);
					PreaggregationOptions.singleton.zoneLink.setEnabled(false);
					PreaggregationOptions.singleton.customDomain.setEnabled(false); // disabled Custom Domain option
					//PreaggregationOptions.singleton.customDomain.setEnabled(runspec.domain != ModelDomain.NATIONAL_ALLOCATION);
		
					if(isCustomDomain) {
						PreaggregationOptions.singleton.customDomain.setSelected(true);
						previousRBSetting = "D";
						processCustomDomainButton();
						selectionListModel.removeAllElements();
					} else if(!runspec.geographicSelections.isEmpty()) {
						Object firstElement = runspec.geographicSelections.iterator().next();
						GeographicSelection firstGS = (GeographicSelection) firstElement;
						if(firstGS.type == GeographicSelectionType.STATE) {
							PreaggregationOptions.singleton.state.setSelected(true);
							previousRBSetting = "S";
							processStateButton();
							selectionListModel.removeAllElements();
							for(Iterator<GeographicSelection> i=runspec.geographicSelections.iterator();i.hasNext();) {
								selectionListModel.addElement(i.next());
							}
						} else if(firstGS.type == GeographicSelectionType.COUNTY) {
							PreaggregationOptions.singleton.county.setSelected(true);
							previousRBSetting = "C";
							processCountyButton();
							selectionListModel.removeAllElements();
							for(Iterator<GeographicSelection> i=runspec.geographicSelections.iterator();i.hasNext();) {
								selectionListModel.addElement(i.next());
							}
						} else {
							PreaggregationOptions.singleton.nation.setSelected(true);
							previousRBSetting = "N";
							processNationButton();
						}
					} else if (PreaggregationOptions.singleton.county.isSelected()) {
						PreaggregationOptions.singleton.county.setSelected(true);
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
		
		// Set Preagg options to defaults, too
		PreaggregationOptions.singleton.nation.setEnabled(true);
		PreaggregationOptions.singleton.state.setEnabled(true);
		PreaggregationOptions.singleton.county.setEnabled(true);
		PreaggregationOptions.singleton.county.setSelected(true);
		PreaggregationOptions.singleton.zoneLink.setEnabled(false);
		PreaggregationOptions.singleton.customDomain.setEnabled(false);
		
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
			String sql = "SELECT countyID, countyName, stateAbbr FROM county, state " +
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
						gsToAdd.textDescription = results.getString(2)
								+ ", " + results.getString(3)
								// requirement to have the FIPS as 5 digits with leading 0s
								+ " (" + String.format("%05d" , results.getInt(1)) + ")";
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
			i = Integer.valueOf(t);
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
			d = Double.valueOf(t);
		} catch(Exception e) {
			d = null;
		}
		g.gpaFraction = (float)(d == null? 0 : d.doubleValue());

		t = genericCountyBarometricPressure.getText();
		d = null;
		try {
			if(t.length() > 0) {
				d = Double.valueOf(t);
			}
		} catch(Exception e) {
			d = null;
		}
		g.barometricPressure = (float)(d == null? 0 : d.doubleValue());

		t = genericCountyRefuelingVaporProgramAdjust.getText();
		d = null;
		try {
			d = Double.valueOf(t);
		} catch(Exception e) {
			d = null;
		}
		g.refuelingVaporProgramAdjust = (float)(d == null? 0 : d.doubleValue());

		t = genericCountyRefuelingSpillProgramAdjust.getText();
		d = null;
		try {
			d = Double.valueOf(t);
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
	}

	/**
	 * Currently not used.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
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
		if(PreaggregationOptions.singleton.nation.isSelected()) {
			// Nothing to do here, Nation is always valid
		} else if (PreaggregationOptions.singleton.state.isSelected()) {
			if(onroadSelected && domain == ModelDomain.NATIONAL_ALLOCATION) {
				nationalWarningPanel.setVisible(true);
			}
		} else if (PreaggregationOptions.singleton.county.isSelected()) {
			if(onroadSelected && domain == ModelDomain.NATIONAL_ALLOCATION) {
				nationalWarningPanel.setVisible(true);
			}
		}
	}	
}
