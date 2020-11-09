/**************************************************************************************************
 * @(#)RoadTypeScreen.java
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
import java.util.TreeMap;
import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;

/**
 * Class for MOVES RoadTypeScreen panel. The panel contains two list boxes and
 * several buttons, a listbox of HPMS Road Types, a listbox of selected Road Types,
 * and buttons to select and delete the selection. Initially the HPMS Road Types
 * listbox is populated with the data retrieved form the database. The Selected
 * values are loaded/saved from/to the RunSpec.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @author		EPA-elg
 * @author		EPA Mitch C.
 * @author 		Tim Hull
 * @author  	Bill Shaw (508 compliance mods)
 * @author		Mike Kender (Task 2003)
 * @author		John Covey (Task 2003)
 * @version     2020-07-30
**/
public class RoadTypeScreen extends JPanel implements ListSelectionListener,
		ActionListener, RunSpecEditor {
	/** Road Type label. **/
	JLabel roadTypeLabel;
	/** Selection label. **/
	JLabel selectionLabel;
	/** Road Type default list model. **/
	DefaultListModel<RoadType> roadTypeListModel;
	/** Selection default list model. **/
	DefaultListModel<RoadType> selectionListModel;
	/** Road Type list. **/
	JList<RoadType> roadTypeList;
	/** Selection list. **/
	JList<RoadType> selectionList;
	/** Road Type scroll pane to contain roadType UI controls. **/
	JScrollPane roadTypeScrollPane;
	/** Selection scroll pane to contain roadType UI controls. **/
	JScrollPane selectionScrollPane;
	/** Road Type Select All button. **/
	JButton roadTypeSelectAll;
	/** Selection Delete button. **/
	JButton selectionDelete;
	/** Add Road Type button. **/
	JButton addRoadType;
	/** global variable used to disable delete off-road when Extended Idle Exhaust is selected **/
	boolean hasExtendedIdleExhaust;
	/** global variable used to disable delete off-road when Start Exhaust is selected **/
	boolean hasStartExhaust;
	/** global variable used to disable delete all roads when Refueling processes are selected **/
	boolean hasRefuelingLoss;
	/** global variable used to disable delete all roads when mesoscale evap processes are selected **/
	boolean hasMesoscaleEvap;
	/** true if the runspec has offroad selections **/
	boolean hasOffRoadSelections;
	/** true after road types have been loaded **/
	boolean didLoadRoadTypes = false;
	/** roads required for refueling calculations **/
	static ArrayList<RoadType> refuelingRoads = new ArrayList<RoadType>();
	/** Owner window, used to detect current model so a filtered set of road types can be displayed **/
	MOVESWindow movesRootWindow = null;
	/** Label warning of deprecated ramp option. **/
	JLabel hasDeprecatedShouldSeparateRampsTrueLabel;

	/**
	 * Constructs a OnRoadVehicleEquipment panel, also creates
	 * and sets the layouts of the controls.
	**/
	public RoadTypeScreen() {
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
		destination.append("Road Types:\r\n");
		for(Iterator<RoadType> i=runspec.roadTypes.iterator();i.hasNext();) {
			destination.append("\t" + i.next() + "\r\n");
		}
		destination.append("\r\n");
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		roadTypeLabel = new JLabel("Available Road Types:");
		roadTypeLabel.setName("roadTypeLabel");
		roadTypeLabel.setDisplayedMnemonic('v');
		selectionLabel = new JLabel("Selected Road Types:");
		selectionLabel.setName("selectionLabel");
		selectionLabel.setDisplayedMnemonic('y');
		roadTypeListModel = new DefaultListModel<RoadType>();
		selectionListModel = new DefaultListModel<RoadType>();
		loadRoadTypes();
		roadTypeList = new JListWithToolTips<RoadType>(roadTypeListModel);
		roadTypeList.setName("roadTypeList");
		ToolTipHelper.add(roadTypeList,Constants.ROAD_TYPE_AVAILABLE_TOOLTIP);

		roadTypeList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		roadTypeList.setSelectedIndex(-1);
		roadTypeList.addListSelectionListener(this);
		roadTypeList.setVisibleRowCount(13);
		roadTypeList.setPrototypeCellValue(new RoadType() { public String toString() { return "CharacterCountToDisplayXXXXX"; }});
		roadTypeList.addKeyListener(new KeyListener() {
			public void keyReleased(KeyEvent e) {
				if (roadTypeList.getModel().getSize() == 0) {
					JOptionPane.showMessageDialog(null, "no 'Available Road Types' to select");
	            } else if (roadTypeList.isSelectionEmpty()) {
	            	roadTypeList.setSelectedIndex(0);
	            }
			}

			public void keyTyped(KeyEvent e) {
				//nothing to do, for now
			}

			public void keyPressed(KeyEvent e) {
				//nothing to do, for now
			}
		});

		roadTypeScrollPane = new JScrollPane(roadTypeList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		roadTypeScrollPane.setName("roadTypeScrollPane");
		roadTypeLabel.setLabelFor(roadTypeList);

		selectionList = new JListWithToolTips<RoadType>(selectionListModel);
		selectionList.setName("selectionList");
		ToolTipHelper.add(selectionList,Constants.ROAD_TYPE_SELECTED_TOOLTIP);
		selectionList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		selectionList.setSelectedIndex(-1);
		selectionList.addListSelectionListener(this);
		selectionList.setVisibleRowCount(13);
		selectionList.setPrototypeCellValue(new RoadType() { public String toString() { return "CharacterCountToDisplayXXXXX"; }});
		selectionList.addKeyListener(new KeyListener() {
			public void keyReleased(KeyEvent e) {
				if (selectionList.getModel().getSize() == 0) {
					JOptionPane.showMessageDialog(null, "no 'Selection Road Types' to select");
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

		selectionScrollPane = new JScrollPane(selectionList);
		selectionScrollPane = new JScrollPane(selectionList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		selectionScrollPane.setName("selectionScrollPane");
		selectionLabel.setLabelFor(selectionList);

		roadTypeSelectAll = new JButton("Select All");
		roadTypeSelectAll.setMnemonic('c');
		roadTypeSelectAll.setName("roadTypeSelectAll");
		ToolTipHelper.add(roadTypeSelectAll,"Select all road types");
		selectionDelete = new JButton("Delete");
		selectionDelete.setMnemonic('l');
		selectionDelete.setName("selectionDelete");
		selectionDelete.setEnabled(false); // disabled until selection made
		ToolTipHelper.add(selectionDelete,"Delete all selected road types");
		addRoadType = new JButton("Add");
		addRoadType.setMnemonic('d');
		addRoadType.setDisplayedMnemonicIndex(1);
		addRoadType.setName("addRoadType");
		addRoadType.setEnabled(false); // disabled until item in list
		ToolTipHelper.add(addRoadType,"Add selected road types to RunSpec");

		// Register a listener for the buttons.
		roadTypeSelectAll.addActionListener(this);
		selectionDelete.addActionListener(this);
		addRoadType.addActionListener(this);

		ImageIcon warningImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/dataExists.gif");
		hasDeprecatedShouldSeparateRampsTrueLabel = new JLabel(
				"<html><body>WARNING: Runspec uses a deprecated Ramp feature.<br>"
				+ "Update by saving to a new file and opening it.</body></html>",
				warningImage, JLabel.LEFT);
		hasDeprecatedShouldSeparateRampsTrueLabel.setName("hasDeprecatedShouldSeparateRampsTrueLabel");
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 5;
		gbc.weightx = 0;
		gbc.weighty = 0;
		setLayout(new GridBagLayout());

		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		add(roadTypeLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		add(roadTypeScrollPane, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0, 2, "WEST", 1, 1);
		add(roadTypeSelectAll, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 2, "EAST", 1, 1);
		add(addRoadType, gbc);

		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
		add(selectionLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1, 1, "WEST", 1, 1);
		add(selectionScrollPane, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1, 2, "EAST", 1, 1);
		add(selectionDelete, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0, 3, "WEST", 2, 2);
		add(hasDeprecatedShouldSeparateRampsTrueLabel, gbc);
	}

	/**
	 * Listener method for list selection changes.
	 * @param e Event caused by a selection change.
	**/
	public void valueChanged(ListSelectionEvent e) {
		if (e.getValueIsAdjusting() == false) {
			updateButtonStates();
		}
	}

	/** Helper method for enabling/disabling all buttons depending upon list selections **/
	public void updateButtonStates() {
		if(roadTypeList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			roadTypeSelectAll.setEnabled(true);
			// the following button only enabled when item(s)
			// selected in the roadType list.
			addRoadType.setEnabled(false);
		} else if(roadTypeList.getSelectedIndices().length >= 1) {
			roadTypeSelectAll.setEnabled(true);
			// the following button only enabled when item(s)
			// selected in the roadType list.
			addRoadType.setEnabled(true);
		}

		if(selectionList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			selectionDelete.setEnabled(false);
		} else if(selectionList.getSelectedIndices().length >= 1) {
			selectionDelete.setEnabled(true);
			Object[] selectedItems = selectionList.getSelectedValuesList().toArray();
			for(int i=0;i<selectedItems.length;i++) {
				RoadType r = (RoadType)selectedItems[i];
				if((r.roadTypeID == 1) &&
						(hasExtendedIdleExhaust || hasStartExhaust || hasOffRoadSelections)) {
					selectionDelete.setEnabled(false);
				}
			}
		}
	}

	/**
	 * Listener method, calls the appropriate button handler.
	 * @param	e Event caused by a button action.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == roadTypeSelectAll) {
			processRoadTypeSelectAllButton();
		} else if(e.getSource() == selectionDelete) {
			processSelectionDeleteButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == addRoadType) {
			processAddRoadTypeButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		}
		updateButtonStates();
	}

	/** Handles the Road Type Select All button. **/
	public void processRoadTypeSelectAllButton() {
		roadTypeList.clearSelection();
		roadTypeList.setSelectionInterval(0,roadTypeList.getModel().getSize()-1);
	}

	/** Handles the Delete button. **/
	public void processSelectionDeleteButton() {
		Object[] selectedItems = selectionList.getSelectedValuesList().toArray();
		for(int i=0;i<selectedItems.length;i++) {
			if(!((((RoadType)selectedItems[i]).roadTypeID == 1) &&
					(hasExtendedIdleExhaust || hasStartExhaust))) {
				selectionListModel.removeElement(selectedItems[i]);
			}
		}
	}

	/** Handles the Add Road Type button. **/
	public void processAddRoadTypeButton() {
		Object[] roadTypeItems = roadTypeList.getSelectedValuesList().toArray();
		for(int i=0;i<roadTypeItems.length;i++) {
			boolean foundMatch = false;
			RoadType nextRoadTypeItem = (RoadType)(roadTypeItems[i]);
			for(Enumeration e=selectionListModel.elements();e.hasMoreElements();) {
				RoadType selection = (RoadType)e.nextElement();
				if(selection.equals(nextRoadTypeItem)) {
					foundMatch = true;
					break;
				}
			}
			if(!foundMatch) {
				selectionListModel.addElement(nextRoadTypeItem);
			}
		}
	}

	/**
	 * Utility routine to set a runspec as using all road types
	 * @param runspec the RunSpec to be modified
	**/
	public static void setAllRoadTypes(RunSpec runspec) {
		Models.ModelCombination mc = Models.evaluateModels(runspec.models);
		TreeSet<RoadType> roadTypesLocal;

		String sql = "SELECT roadTypeID, roadDesc, isAffectedByOnroad, isAffectedByNonroad FROM roadtype";
		switch (mc) {
		case M1: // Onroad
			roadTypesLocal = runspec.roadTypes;
			sql += " WHERE isAffectedByOnroad = TRUE";
			break;
		case M2: // Nonroad
			roadTypesLocal = runspec.roadTypes;
			sql += " WHERE isAffectedByNonroad = TRUE";
			break;
		default:
			roadTypesLocal = null;
			break;
		}
		sql += " ORDER BY roadDesc";
		if (roadTypesLocal == null) {
			return;
		}

		SQLRunner.Query query = new SQLRunner.Query();
		Connection db = null;
		try {
			db = DatabaseConnectionManager
					.getGUIConnection(MOVESDatabaseType.DEFAULT);
			query.open(db, sql);
			while (query.rs.next()) {
				int roadTypeID = query.rs.getInt(1);
				boolean isAffectedByOnroad = query.rs.getBoolean(3);
				boolean isAffectedByNonroad = query.rs.getBoolean(4);
				if (isAffectedByOnroad && isAffectedByNonroad) {
					runspec.roadTypes.add(new RoadType(roadTypeID, query.rs
							.getString(2), Models.ModelCombination.M12));
				} else if (isAffectedByOnroad) {
					runspec.roadTypes.add(new RoadType(roadTypeID, query.rs
							.getString(2), Models.ModelCombination.M1));
				} else if (isAffectedByNonroad) {
					runspec.roadTypes.add(new RoadType(roadTypeID, query.rs
							.getString(2), Models.ModelCombination.M2));
				}
			}
		} catch (Exception e) {
			Logger.logError(
					e,
					"Unable to load a list of road types: "
							+ e.getMessage());
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Saves the info to a RunSpec.
	 * @param	runspec the RunSpec to get the info
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.roadTypes.clear();
		for(int i=0;i<selectionListModel.getSize();i++) {
			RoadType roadType = (RoadType)selectionListModel.getElementAt(i);
			runspec.roadTypes.add(roadType);
		}
	}

	/**
	 * Loads the info from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		loadRoadTypes();

		hasExtendedIdleExhaust = false;
		hasStartExhaust = false;
		hasRefuelingLoss = false;
		hasMesoscaleEvap = false;
		for(Iterator<PollutantProcessAssociation> i=runspec.pollutantProcessAssociations.iterator();
				i.hasNext();) {
			PollutantProcessAssociation p = (PollutantProcessAssociation)i.next();
			switch(p.emissionProcess.databaseKey) {
				case 90: // 90== Extended Idle Exhaust by definition
					hasExtendedIdleExhaust = true;
					break;
				case 2: // 2== Start Exhaust by definition
					hasStartExhaust = true;
					break;
				case 18: // 18 == Refueling Displacement Vapor Loss
					hasRefuelingLoss = true;
					break;
				case 19: // 19 == Refueling Spillage Loss
					hasRefuelingLoss = true;
					break;
				case 11: // 11 == Evap Permeation
					if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
						hasMesoscaleEvap = true;
					}
					break;
				case 12: // 12 == Evap Fuel Vapor Venting
					if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
						hasMesoscaleEvap = true;
					}
					break;
				case 13: // 13 == Evap Fuel Leaks
					if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
						hasMesoscaleEvap = true;
					}
					break;
			}
		}

		boolean hasOnRoadSelections = false;
		hasOffRoadSelections = false;

		selectionListModel.removeAllElements();
		if(!runspec.onRoadVehicleSelections.isEmpty()) {
			hasOnRoadSelections = true;
		}
		if(!runspec.offRoadVehicleSelections.isEmpty()) {
			hasOffRoadSelections = true;
		}
		if(hasOnRoadSelections || hasOffRoadSelections) {
			for(Iterator<RoadType> i=runspec.roadTypes.iterator();i.hasNext();) {
				RoadType roadType = (RoadType)i.next();
				selectionListModel.addElement(roadType);
			}
		}

		roadTypeList.setEnabled(hasOnRoadSelections);
		selectionList.setEnabled(hasOnRoadSelections);
		roadTypeSelectAll.setEnabled(hasOnRoadSelections);
		addRoadType.setEnabled(hasOnRoadSelections);
		selectionDelete.setEnabled(hasOnRoadSelections);

		hasDeprecatedShouldSeparateRampsTrueLabel.setVisible(runspec.hasDeprecatedShouldSeparateRampsTrue);

		updateButtonStates();
	}

	/**
	 * Verifies that the current roadType selections are in the database.
	 * @param	runspec the RunSpec to verify.
	 * @return	true for valid.
	**/
	boolean verifyCurrentSelections(RunSpec runspec) {
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(db == null) {
			return false;
		}
		SQLRunner.Query query = new SQLRunner.Query();
		for(Iterator<RoadType> i=runspec.roadTypes.iterator();i.hasNext();) {
			String nextRoadType = ((RoadType)(i.next())).roadTypeName;
			String sql = "";
			try {
				sql = "SELECT roadDesc FROM roadtype WHERE roadDesc = " + nextRoadType;
				query.open(db,sql);
				if(!query.rs.next()) {
					return false;
				}
			} catch(Exception e) {
				Logger.logError(e,"Unable to verify that the selected road types are in the database.");
			} finally {
				query.onFinally();
			}
		}
		return true;
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the info
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		RunSpecSectionStatus status;
		if(runspec.hasDeprecatedShouldSeparateRampsTrue) {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		} else if(runspec.roadTypes.isEmpty()) {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		} else if(!runspec.models.contains(Model.NONROAD)) {
			// Look for required road types
			boolean usesRefuelingLoss = false;
			boolean usesMesoscaleEvap = false;
			boolean needsNonOffnetworkRoad = false;
			boolean calculatesONI = false;
			for(Iterator i=runspec.pollutantProcessAssociations.iterator();i.hasNext();) {
				PollutantProcessAssociation p = (PollutantProcessAssociation)i.next();
				switch(p.emissionProcess.databaseKey) {
					case 1: // 1 == Running Exhaust
						needsNonOffnetworkRoad = true;
						// ONI (Off network idling) needs all road types when off-network
						// is selected with any Running Exhaust process
						// However, Project Scale does not calculate ONI
						for(RoadType r : runspec.roadTypes) {
							if(r.roadTypeID == 1 && runspec.domain != ModelDomain.PROJECT) {
								calculatesONI = true;
								break;
							}
						}
						break;
					case 9: // 9 == Brakewear
						needsNonOffnetworkRoad = true;
						break;
					case 10: // 10 == Tirewear
						needsNonOffnetworkRoad = true;
						break;
					case 11: // 11 == Evap Permeation
						if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
							usesMesoscaleEvap = true;
						}
						break;
					case 12: // 12 == Evap Fuel Vapor Venting
						if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
							usesMesoscaleEvap = true;
						}
						break;
					case 13: // 13 == Evap Fuel Leaks
						if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
							usesMesoscaleEvap = true;
						}
						break;
					case 18: // 18 == Refueling Displacement Vapor Loss
						usesRefuelingLoss = true;
						break;
					case 19: // 19 == Refueling Spillage Loss
						usesRefuelingLoss = true;
						break;
				}
			}
			boolean isOK = true;
			// if calculating ONI or refueling losses, all road types are required
			if(isOK && (usesRefuelingLoss || calculatesONI)) {
				boolean hasRequiredRoads = false;
				if(runspec.roadTypes.size() < roadTypeListModel.getSize()) {
					hasRequiredRoads = hasRefuelingRoads(runspec);
				} else {
					hasRequiredRoads = true;
				}

				isOK = isOK && hasRequiredRoads;
			}
			if(isOK && needsNonOffnetworkRoad) {
				boolean found = false;
				for(RoadType r : runspec.roadTypes) {
					if(r.roadTypeID >= 2 && r.roadTypeID <= 9) {
						found = true;
						break;
					}
				}
				isOK = isOK && found;
			}
			status = new RunSpecSectionStatus(isOK? RunSpecSectionStatus.OK : RunSpecSectionStatus.NOT_READY);
		} else {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		sections.remove(getName());
		sections.put(getName(),status);
		return status;
	}

	/**
	 * Sets the defaults to the RunSpec.
	 * @param	runspec the RunSpec to the info.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		runspec.hasDeprecatedShouldSeparateRampsTrue = false;
		runspec.roadTypes.clear();
		loadRoadTypes();

		// Default is to select all road types but only if there are on-road selections
		if(!runspec.onRoadVehicleSelections.isEmpty()) {
			for(int i=0;i<roadTypeListModel.getSize();i++) {
				runspec.roadTypes.add((RoadType)roadTypeListModel.getElementAt(i));
			}
		} else if(!runspec.offRoadVehicleSelections.isEmpty()) {
			for(int i=0;i<roadTypeListModel.getSize();i++) {
				RoadType r = (RoadType)roadTypeListModel.getElementAt(i);
				if(r.roadTypeID == 1) {
					runspec.roadTypes.add(r);
					break;
				}
			}
		}

		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		sections.put(getName(),status);

		if(roadTypeList != null) {
			roadTypeList.clearSelection();
		}

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
		return saveDefaultsToRunSpec(runspec,sections);
	}

	/**
	 * Load roadTypes from the database into the roadTypeList listbox
	**/
	public void loadRoadTypes() {
		Models.ModelCombination mc = Models.ModelCombination.M0;
		if (movesRootWindow != null && movesRootWindow.runSpec != null) {
			mc = Models.evaluateModels(movesRootWindow.runSpec.models);
		}

		roadTypeListModel.removeAllElements();
		selectionListModel.removeAllElements();
		refuelingRoads.clear();

		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if (null == db) {
			JOptionPane.showMessageDialog(this,
					"Database Connection unavailable");
			roadTypeListModel.addElement(new RoadType(12345,
					"Rural Interstate", Models.ModelCombination.M1));
			roadTypeListModel.addElement(new RoadType(67890,
					"Urban Interstate", Models.ModelCombination.M1));
			return;
		}

		String sql = "SELECT roadTypeID, roadDesc, isAffectedByOnroad, isAffectedByNonroad"
				+ " FROM roadtype"
				+ " WHERE shouldDisplay = 1";
		switch (mc) {
		case M1: // Onroad
			sql += " AND isAffectedByOnroad = TRUE";
			break;
		case M2: // Nonroad
			sql += " AND isAffectedByNonroad = TRUE";
		default:
			break;
		}
		sql += " ORDER BY roadDesc";

		try {
			PreparedStatement statement = db.prepareStatement(sql);
			ResultSet results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				// System.out.println("Road types:");
				while (results.next()) {
					int roadTypeID = results.getInt(1);
					// System.out.println("Adding road type " + roadTypeID);
					boolean isAffectedByOnroad = results.getBoolean(3);
					boolean isAffectedByNonroad = results.getBoolean(4);
					RoadType r = null;
					if (isAffectedByOnroad && isAffectedByNonroad) {
						r = new RoadType(roadTypeID,
								results.getString(2),
								Models.ModelCombination.M12);
					} else if (isAffectedByOnroad) {
						r = new RoadType(roadTypeID,
								results.getString(2),
								Models.ModelCombination.M1);
					} else if (isAffectedByNonroad) {
						r = new RoadType(roadTypeID,
								results.getString(2),
								Models.ModelCombination.M2);
					}
					if(r != null) {
						roadTypeListModel.addElement(r);
						if(isRefuelingRoad(r)) {
							refuelingRoads.add(r);
						}
					}
					// roadTypeListModel.addElement(
					// new RoadType(roadTypeID, results.getString(2)));
				}
				results.close();
			}
			statement.close();
		} catch (Exception e) {
			Logger.logError(e, "Unable to load a list of road types.");
		}
	}

	@Override
	public RunSpecSectionStatus onModelChange(RunSpec runspec,
			TreeMap<String, RunSpecSectionStatus> sections) {
		// Nothing to do here
		return null;
	}

	/**
	 * Determine if a road is required for refueling.
	 * @param r the road to be checked
	 * @return true if refueling requires the road to be present
	**/	
	public static boolean isRefuelingRoad(RoadType r) {
		return r.roadTypeID >= 1 && r.roadTypeID <= 9;
	}

	/**
	 * Check a RunSpec's set of selected roads against the set of roads
	 * required for any refueling operation.
	 * @param runspec RunSpec object to be scanned
	 * @return true if all required road types have been selected in the runspec.
	**/	
	public static boolean hasRefuelingRoads(RunSpec runspec) {
		boolean hasRequiredRoads = true;
		for(RoadType required : refuelingRoads) {
			boolean found = false;
			for(RoadType has : runspec.roadTypes) {
				if(has.roadTypeID == required.roadTypeID) {
					found = true;
					break;
				}
			}
			if(!found) {
				hasRequiredRoads = false;
				break;
			}
		}
		return hasRequiredRoads;
	}
}
