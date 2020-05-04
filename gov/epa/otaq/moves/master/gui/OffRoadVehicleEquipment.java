/**************************************************************************************************
 * @(#)OffRoadVehicleEquipment.java
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
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.*;
import java.sql.*;

/**
 * Class for MOVES OffRoadVehicleEquipment panel. Constructs an OffRoadVehicleEquipment panel,
 * also creates and sets the layouts of the controls. The controls are a list box of Fuels,
 * a list box of Segments, both loaded from the database, a listbox of selections,
 * buttons to select Fuels and Segments, and a button to add the combination of the
 * two to the Selections list. It loads/saves the selections from/to RunSpec.
 *
 * @author		Wes Faler
 * @version		2015-08-18
**/
public class OffRoadVehicleEquipment extends JPanel implements ListSelectionListener,
		ActionListener, RunSpecEditor {
	/** Fuel label. **/
	JLabel fuelLabel;
	/** Sector label. **/
	JLabel sectorLabel;
	/** Selection label. **/
	JLabel selectionLabel;
	/** Fuel default list model. **/
	DefaultListModel<FuelEntry> fuelListModel;
	/** Sector default list model. **/
	DefaultListModel<SectorEntry> sectorListModel;
	/** Selection default list model. **/
	DefaultListModel<OffRoadVehicleSelection> selectionListModel;
	/** Valid Fuel/Sector combinations. **/
	TreeSet<OffRoadVehicleSelection> validFuelSectorCombinations =
			new TreeSet<OffRoadVehicleSelection>();
	/** Fuel list. **/
	JList<FuelEntry> fuelList;
	/** Sector list. **/
	JList<SectorEntry> sectorList;
	/** Selection list. **/
	JList<OffRoadVehicleSelection> selectionList;
	/** Fuel scroll pane to contain fuel UI controls. **/
	JScrollPane fuelScrollPane;
	/** Sector scroll pane to contain fuel UI controls. **/
	JScrollPane sectorScrollPane;
	/** Selection scroll pane to contain fuel UI controls. **/
	JScrollPane selectionScrollPane;
	/** Fuel Select All button. **/
	JButton fuelSelectAll;
	/** Sector Select All button. **/
	JButton sectorSelectAll;
	/** Selection Delete button. **/
	JButton selectionDelete;
	/** Add Fuel Type Combinations button. **/
	JButton addFuelSectorCombinations;
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

	/** Inner class to hold the fuel types loaded from the database **/
	class FuelEntry {
		/** Identifies a fuel. **/
		public int fuelTypeID;
		/** Fuel description from database, used for toString() purposes **/
		public String fuelTypeDesc;

		/** Default constructor **/
		public FuelEntry() {
		}

		/**
		 * Constructor that requires fuel type and description.
		 * @param fuelTypeIDToUse ID from the database for the fuel
		 * @param fuelTypeDescToUse description from the database for the fuel
		**/
		public FuelEntry(int fuelTypeIDToUse,String fuelTypeDescToUse) {
			fuelTypeID = fuelTypeIDToUse;
			fuelTypeDesc = fuelTypeDescToUse;
		}

		/**
		 * Returns a String representation of this object.
		 * @return The String representation of this object.
		**/
		public String toString() {
			return fuelTypeDesc;
		}
	}

	/** Inner class to hold the sectors loaded from the database **/
	class SectorEntry {
		/** Identifies a NonRoad sector. **/
		public int sectorID;
		/** Sector name from database, used for toString() purposes **/
		public String sectorName;

		/** Default constructor **/
		public SectorEntry() {
		}

		/**
		 * Constructor that requires source use type and description.
		 * @param sectorIDToUse ID from the database for the sector
		 * @param sectorNameToUse description from the database for the sector
		**/
		public SectorEntry(int sectorIDToUse,String sectorNameToUse) {
			sectorID = sectorIDToUse;
			sectorName = sectorNameToUse;
		}

		/**
		 * Returns a String representation of this object.
		 * @return The String representation of this object.
		**/
		public String toString() {
			return sectorName;
		}
	}

	/**
	 * Constructs a OffRoadVehicleEquipment panel, also creates
	 * and sets the layouts of the controls.
	**/
	public OffRoadVehicleEquipment() {
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
		destination.append("NonRoad Vehicle Equipment:\r\n");
		for(Iterator i=runspec.offRoadVehicleSelections.iterator();i.hasNext();) {
			destination.append("\t" + i.next() + "\r\n");
		}
		destination.append("\r\n");
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		fuelLabel = new JLabel("Fuels:");
		fuelLabel.setName("fuelLabel");
		sectorLabel = new JLabel("Sectors:");
		sectorLabel.setName("sectorLabel");
		selectionLabel = new JLabel("Selections:");
		selectionLabel.setName("selectionLabel");
		fuelListModel = new DefaultListModel<FuelEntry>();
		sectorListModel = new DefaultListModel<SectorEntry>();
		selectionListModel = new DefaultListModel<OffRoadVehicleSelection>();
		loadFuels();
		loadSectors();
		loadValidFuelSectorCombinations();
		fuelList = new JListWithToolTips<FuelEntry>(fuelListModel);
		fuelList.setName("fuelList");
		fuelList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		fuelList.setSelectedIndex(-1);
		fuelList.addListSelectionListener(this);
		fuelList.setVisibleRowCount(9);
		fuelList.setPrototypeCellValue(new FuelEntry() { public String toString() { return "CharacterCountToDisplay"; }});
		fuelScrollPane = new JScrollPane(fuelList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		fuelScrollPane.setName("fuelScrollPane");

		sectorList = new JListWithToolTips<SectorEntry>(sectorListModel);

		sectorList.setName("sectorList");
		sectorList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		sectorList.setSelectedIndex(-1);
		sectorList.addListSelectionListener(this);
		sectorList.setVisibleRowCount(9);
		sectorList.setPrototypeCellValue(new SectorEntry() { public String toString() { return "CharacterCountToDisplay"; }});
		sectorScrollPane = new JScrollPane(sectorList);
		sectorScrollPane = new JScrollPane(sectorList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		sectorScrollPane.setName("sectorScrollPane");

		selectionList = new JListWithToolTips<OffRoadVehicleSelection>(selectionListModel);
		selectionList.setName("selectionList");
		selectionList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		selectionList.setSelectedIndex(-1);
		selectionList.addListSelectionListener(this);
		selectionList.setVisibleRowCount(9);
		selectionList.setPrototypeCellValue(new OffRoadVehicleSelection() { public String toString() { return "CharacterCountToDisplayXXXXXXXXXX"; }});
		selectionScrollPane = new JScrollPane(selectionList);
		selectionScrollPane = new JScrollPane(selectionList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		selectionScrollPane.setName("selectionScrollPane");

		fuelSelectAll = new JButton("Select All");
		fuelSelectAll.setName("fuelSelectAll");
		ToolTipHelper.add(fuelSelectAll,"Select all fuel types");
		sectorSelectAll = new JButton("Select All");
		sectorSelectAll.setName("sectorSelectAll");
		ToolTipHelper.add(sectorSelectAll,"Select all sectors");
		selectionDelete = new JButton("Delete");
		selectionDelete.setName("selectionDelete");
		selectionDelete.setEnabled(false); // disabled until selection made
		ToolTipHelper.add(selectionDelete,"Delete the selected Fuel and Sector combinations");
		addFuelSectorCombinations = new JButton("Add Fuel/Sector Combinations");
		addFuelSectorCombinations.setName("addFuelSectorCombinations");
		addFuelSectorCombinations.setEnabled(false); // disabled until item in list
		ToolTipHelper.add(addFuelSectorCombinations,"Add selected Fuel and Sector combinations");

		// Register a listener for the buttons.
		fuelSelectAll.addActionListener(this);
		sectorSelectAll.addActionListener(this);
		selectionDelete.addActionListener(this);
		addFuelSectorCombinations.addActionListener(this);

		messageLogModel = new DefaultListModel<String>();
		messageLogList = new JListWithToolTips<String>(messageLogModel);
		messageLogList.setName("messageLogList");
		messageLogList.setSelectedIndex(-1);
		messageLogList.setVisibleRowCount(5);
		messageLogList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
		messageLogPane = new JScrollPane(messageLogList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		messageLogPane.setName("messageLogPane");
		messageLogPanel = new JPanel();
		messageLogPanel.setVisible(false);
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 5;
		gbc.weightx = 0;
		gbc.weighty = 0;
		setLayout(new GridBagLayout());

		LayoutUtility.setPositionOnGrid(gbc,0, 3, "CENTER", 2, 1);
		add(addFuelSectorCombinations, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		add(fuelLabel, gbc);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		add(fuelScrollPane, gbc);
		gbc.fill = GridBagConstraints.NONE;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0, 2, "EAST", 1, 1);
		add(fuelSelectAll, gbc);

		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
		add(sectorLabel, gbc);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		LayoutUtility.setPositionOnGrid(gbc,1, 1, "WEST", 1, 1);
		add(sectorScrollPane, gbc);
		gbc.fill = GridBagConstraints.NONE;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,1, 2, "EAST", 1, 1);
		add(sectorSelectAll, gbc);

		LayoutUtility.setPositionOnGrid(gbc,2, 0, "WEST", 1, 1);
		add(selectionLabel, gbc);

		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		LayoutUtility.setPositionOnGrid(gbc,2, 1, "WEST", 1, 1);
		add(selectionScrollPane, gbc);
		gbc.fill = GridBagConstraints.NONE;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,2, 2, "EAST", 1, 1);
		add(selectionDelete, gbc);

		messageLogPanel.setLayout(new BoxLayout(messageLogPanel, BoxLayout.Y_AXIS));
		messageLogPanel.add(new JLabel("NonRoad Vehicle Equipment Requirements"));
		messageLogPanel.add(messageLogPane);

		gbc.fill = GridBagConstraints.HORIZONTAL;
		LayoutUtility.setPositionOnGrid(gbc, 0, 4, "WEST", 3, 1);
		add(messageLogPanel, gbc);
	}

	/**
	 * Listener method for list selection changes.
	 * @param e Event caused by a selection change.
	**/
	public void valueChanged(ListSelectionEvent e) {
		if(e.getValueIsAdjusting() == false) {
			updateButtonStates();
		}
	}

	/** Helper method for enabling/disabling all buttons depending upon list selections **/
	public void updateButtonStates() {
		if(fuelList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			fuelSelectAll.setEnabled(true);
			// the following button only enabled when item(s)
			// selected in both the fuel and sourceUseType lists.
			addFuelSectorCombinations.setEnabled(false);
		} else if(fuelList.getSelectedIndices().length >= 1) {
			fuelSelectAll.setEnabled(true);
			if(sectorList.getSelectedIndices().length < 1) {
				// the following button only enabled when item(s)
				// selected in both the fuel and sourceUseType lists.
				addFuelSectorCombinations.setEnabled(false);
			} else {
				// the following button only enabled when item(s)
				// selected in both the fuel and sourceUseType lists.
				addFuelSectorCombinations.setEnabled(true);
			}
		}
		if(sectorList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			sectorSelectAll.setEnabled(true);
			// the following button only enabled when item(s)
			// selected in both the fuel and sourceUseType lists.
			addFuelSectorCombinations.setEnabled(false);
		} else if(sectorList.getSelectedIndices().length >= 1) {
			sectorSelectAll.setEnabled(true);
			if(fuelList.getSelectedIndices().length < 1) {
				// the following button only enabled when item(s)
				// selected in both the fuel and sourceUseType lists.
				addFuelSectorCombinations.setEnabled(false);
			} else {
				// the following button only enabled when item(s)
				// selected in both the fuel and sourceUseType lists.
				addFuelSectorCombinations.setEnabled(true);
			}
		}
		if(selectionList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			selectionDelete.setEnabled(false);
		} else if(selectionList.getSelectedIndices().length >= 1) {
			selectionDelete.setEnabled(true);
		}
		displaySectionStatus();
	}

	/**
	 * Listener method, calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == fuelSelectAll) {
			processFuelSelectAllButton();
		} else if(e.getSource() == sectorSelectAll) {
			processSectorSelectAllButton();
		} else if(e.getSource() == selectionDelete) {
			processSelectionDeleteButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == addFuelSectorCombinations) {
			processFuelSectorCombinationsButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		}
		updateButtonStates();
	}

	/** Handles the Fuel Select All button. **/
	public void processFuelSelectAllButton() {
		fuelList.clearSelection();
		fuelList.setSelectionInterval(0,fuelList.getModel().getSize()-1);
	}

	/** Handles the Sector Select All button. **/
	public void processSectorSelectAllButton() {
		sectorList.clearSelection();
		sectorList.setSelectionInterval(0,sectorList.getModel().getSize()-1);
	}

	/** Handles the Delete button. **/
	public void processSelectionDeleteButton() {
		Object[] selectedItems = selectionList.getSelectedValuesList().toArray();
		for(int i=0;i<selectedItems.length;i++) {
			selectionListModel.removeElement(selectedItems[i]);
		}
	}

	/** Handles the Add Fuel/Sector Combination button. **/
	public void processFuelSectorCombinationsButton() {
		if(fuelList.getSelectedIndex() < 0 || sectorList.getSelectedIndex() < 0) {
			return;
		}
		Object[] fuelItems = fuelList.getSelectedValuesList().toArray();
		Object[] sectorItems = sectorList.getSelectedValuesList().toArray();
		for(int i=0;i<fuelItems.length;i++) {
			for(int j=0;j<sectorItems.length;j++) {
				FuelEntry fuelEntry = (FuelEntry)fuelItems[i];
				SectorEntry sectorEntry = (SectorEntry)sectorItems[j];
				OffRoadVehicleSelection fs = new OffRoadVehicleSelection();
				fs.fuelTypeID = fuelEntry.fuelTypeID;
				fs.fuelTypeDesc = fuelEntry.fuelTypeDesc;
				fs.sectorID = sectorEntry.sectorID;
				fs.sectorName = sectorEntry.sectorName;
				boolean foundMatch = false;
				for(Enumeration e=selectionListModel.elements();e.hasMoreElements();) {
					OffRoadVehicleSelection selection = (OffRoadVehicleSelection)e.nextElement();
					if(selection.equals(fs)) {
						foundMatch = true;
						break;
					}
				}
				if(!foundMatch) {
					foundMatch = false;
					for(Iterator<OffRoadVehicleSelection> vfsc=validFuelSectorCombinations.iterator();vfsc.hasNext();) {
						OffRoadVehicleSelection validCombination = (OffRoadVehicleSelection)vfsc.next();
						if(fs.equals(validCombination)) {
							foundMatch = true;
							break;
						}
					}
					if(foundMatch) {
						selectionListModel.addElement(fs);
					}
				}
			}
		}
	}

	/** Display the status of this runspec section. **/
	public void displaySectionStatus() {
		messageLogPanel.setVisible(false);
		if(selectionList.getModel().getSize() == 0) {
			messageLogModel.clear();
			messageLogPanel.setVisible(true);
			messageLogModel.addElement(new String("Please select a Fuel and Sector combination."));
		} else {
			messageLogModel.clear();
			//boolean didWarnAboutMotorcycles = false;
			for(Enumeration sl=selectionListModel.elements();sl.hasMoreElements();) {
				OffRoadVehicleSelection selection = (OffRoadVehicleSelection)sl.nextElement();
				boolean foundMatch = false;
				for(Iterator<OffRoadVehicleSelection> vfsc=validFuelSectorCombinations.iterator();
						vfsc.hasNext();) {
					OffRoadVehicleSelection validCombination = (OffRoadVehicleSelection)vfsc.next();
					if(selection.equals(validCombination)) {
						foundMatch = true;
						break;
					}
				}
				if(!foundMatch) {
					messageLogPanel.setVisible(true);
					messageLogModel.addElement(new String(selection.fuelTypeDesc+"/"
							+ selection.sectorName +" combination is not in the database."));
				}
			}
		}
	}

	/**
	 * Saves the info to a RunSpec.
	 * @param	runspec the RunSpec to get the info
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.offRoadVehicleSelections.clear();
		for(int i=0;i<selectionListModel.getSize();i++) {
			runspec.offRoadVehicleSelections.add(
					(OffRoadVehicleSelection)selectionListModel.getElementAt(i));
		}
		if(runspec.offRoadVehicleSelections.size() > 0) {
			// Ensure Off-Network road type is selected
			boolean hasNonRoad = false;
			for(Iterator<RoadType> i=runspec.roadTypes.iterator();i.hasNext();) {
				RoadType r = i.next();
				if(r.roadTypeID == 100) {
					hasNonRoad = true;
					break;
				}
			}
			if(!hasNonRoad) {
				Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
				String sql = "SELECT roadTypeID, roadDesc, isAffectedByOnroad, isAffectedByNonroad FROM roadtype WHERE roadTypeID=100";
				SQLRunner.Query query = new SQLRunner.Query();
				try {
					query.open(db,sql);
					while(query.rs.next()) {
						int roadTypeID = query.rs.getInt(1);
						boolean isAffectedByOnroad = query.rs.getBoolean(3);
						boolean isAffectedByNonroad = query.rs.getBoolean(4);
						if ( isAffectedByOnroad && isAffectedByNonroad) {
							runspec.roadTypes.add(new RoadType(roadTypeID, query.rs.getString(2), Models.ModelCombination.M12));
						} else if (isAffectedByOnroad) {
							runspec.roadTypes.add(new RoadType(roadTypeID, query.rs.getString(2), Models.ModelCombination.M1));
						} else if (isAffectedByNonroad) {
							runspec.roadTypes.add(new RoadType(roadTypeID, query.rs.getString(2), Models.ModelCombination.M2));
						}
						//runspec.roadTypes.add(new RoadType(roadTypeID, query.rs.getString(2)));
					}
				} catch(Exception e) {
					Logger.logError(e,"Unable to load nonroad road type.");
				} finally {
					query.onFinally();
				}
			}
		}
	}

	/**
	 * Loads the info from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		selectionListModel.removeAllElements();
		for(Iterator<OffRoadVehicleSelection> i=runspec.offRoadVehicleSelections.iterator();
				i.hasNext();) {
			selectionListModel.addElement(i.next());
		}
		displaySectionStatus();
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
		if(runspec.offRoadVehicleSelections.isEmpty()) {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		} else {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
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
		runspec.offRoadVehicleSelections.clear();
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
		// Nothing special to do here
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/** load all fuels from the database into the fuelList listbox **/
	public void loadFuels() {
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(null == db) {
			fuelListModel.addElement(new FuelEntry(10000,"Diesel"));
			fuelListModel.addElement(new FuelEntry(10001,"Gasoline"));
			return;
		}
		//String sql = "SELECT fuelTypeID, fuelTypeDesc FROM nrFuelType ORDER BY fuelTypeDesc";
		String sql = "SELECT DISTINCT ft.fuelTypeID, ft.fuelTypeDesc"
				+ " FROM NRSCC scc"
				+ " INNER JOIN NREquipmentType neq on (neq.NREquipTypeID=scc.NREquipTypeID)"
				+ " INNER JOIN Sector s on (s.sectorID=neq.sectorID)"
				+ " INNER JOIN nrFuelType ft on (ft.fuelTypeID=scc.fuelTypeID)"
				+ " ORDER BY ft.fuelTypeDesc";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				int ftID = query.rs.getInt(1);
				String fDesc = query.rs.getString(2);
				fuelListModel.addElement(new FuelEntry(ftID,fDesc));
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to load a list of fuels for On Road Vehicle Equipment.");
		} finally {
			query.onFinally();
		}
	}

	/** load all sectors from the database into the sectorList listbox **/
	public void loadSectors() {
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(null == db) {
			sectorListModel.addElement(new SectorEntry(5001,"Pizza Delivery Cars"));
			return;
		}
		String sql="SELECT sectorID,description FROM Sector WHERE sectorID>0 ORDER BY description";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				sectorListModel.addElement(
						new SectorEntry(query.rs.getInt(1),query.rs.getString(2)));
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to load a list of NonRoad sectors.");
		} finally {
			query.onFinally();
		}
	}

	/** Load valid fuel/sector combinations **/
	public void loadValidFuelSectorCombinations() {
		validFuelSectorCombinations.clear();
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(null == db) {
			return;
		}
		String defaultDBName = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
		String sql = "SELECT DISTINCT ft.fuelTypeID, ft.fuelTypeDesc, s.sectorID, s.description"
				+ " FROM NRSCC scc"
				+ " INNER JOIN NREquipmentType neq on (neq.NREquipTypeID=scc.NREquipTypeID)"
				+ " INNER JOIN " + defaultDBName + ".Sector s on (s.sectorID=neq.sectorID)"
				+ " INNER JOIN " + defaultDBName + ".nrFuelType ft on (ft.fuelTypeID=scc.fuelTypeID)"
				+ " ORDER BY ft.fuelTypeDesc, s.description";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				OffRoadVehicleSelection fs = new OffRoadVehicleSelection();
				fs.fuelTypeID = query.rs.getInt(1);
				fs.fuelTypeDesc = query.rs.getString(2);
				fs.sectorID = query.rs.getInt(3);
				fs.sectorName = query.rs.getString(4);
				validFuelSectorCombinations.add(fs);
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to load valid fuel/sector combinations.");
		} finally {
			query.onFinally();
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
}
