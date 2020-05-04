/**************************************************************************************************
 * @(#)OnRoadVehicleEquipment.java
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
 * Class for MOVES OnRoadVehicleEquipment panel. Constructs an OnRoadVehicleEquipment panel,
 * also creates and sets the layouts of the controls. The controls are a list box of Fuels,
 * a list box of Source Use Types, both loaded from the database, a listbox of selections,
 * buttons to select Fuels and SourceUse Types, and a button to add the combination of the
 * two to the Selections list. It loads/saves the selections from/to RunSpec.
 *
 * @author		Wes Faler
 * @author		EPA Mitch C.
 * @author      EPA William Aikman
 * @author		Tim Hull
 * @version		2015-08-18
**/
public class OnRoadVehicleEquipment extends JPanel implements ListSelectionListener,
		ActionListener, RunSpecEditor {
	/** Fuel label. **/
	JLabel fuelLabel;
	/** Source Use Type label. **/
	JLabel sourceUseTypeLabel;
	/** Selection label. **/
	JLabel selectionLabel;
	/** Fuel default list model. **/
	DefaultListModel<FuelEntry> fuelListModel;
	/** Source Use Type default list model. **/
	DefaultListModel<SourceTypeEntry> sourceUseTypeListModel;
	/** Selection default list model. **/
	DefaultListModel<OnRoadVehicleSelection> selectionListModel;
	/** Valid Fuel/Source combinations. **/
	TreeSet<OnRoadVehicleSelection> validFuelSourceCombinations = new TreeSet<OnRoadVehicleSelection>();
	/** Fuel list. **/
	JList<FuelEntry> fuelList;
	/** Source Use Type list. **/
	JList<SourceTypeEntry> sourceUseTypeList;
	/** Selection list. **/
	JList<OnRoadVehicleSelection> selectionList;
	/** Fuel scroll pane to contain fuel UI controls. **/
	JScrollPane fuelScrollPane;
	/** Source Use Type scroll pane to contain fuel UI controls. **/
	JScrollPane sourceUseTypeScrollPane;
	/** Selection scroll pane to contain fuel UI controls. **/
	JScrollPane selectionScrollPane;
	/** Fuel Select All button. **/
	JButton fuelSelectAll;
	/** Source Use Type Select All button. **/
	JButton sourceUseTypeSelectAll;
	/** Selection Delete button. **/
	JButton selectionDelete;
	/** Add Fuel Type Combinations button. **/
	JButton addFuelSourceUseTypeCombinations;
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

	/** Inner class to hold the source types loaded from the database **/
	class SourceTypeEntry {
		/** Identifies a MOVES Source Use Type. **/
		public int sourceTypeID;
		/** Source Use Type name from database, used for toString() purposes **/
		public String sourceTypeName;

		/** Default constructor **/
		public SourceTypeEntry() {
		}

		/**
		 * Constructor that requires source use type and description.
		 * @param sourceTypeIDToUse ID from the database for the source use type
		 * @param sourceTypeNameToUse description from the database for the source use type
		**/
		public SourceTypeEntry(int sourceTypeIDToUse,String sourceTypeNameToUse) {
			sourceTypeID = sourceTypeIDToUse;
			sourceTypeName = sourceTypeNameToUse;
		}

		/**
		 * Returns a String representation of this object.
		 * @return The String representation of this object.
		**/
		public String toString() {
			return sourceTypeName;
		}
	}

	/**
	 * Constructs a OnRoadVehicleEquipment panel, also creates
	 * and sets the layouts of the controls.
	**/
	public OnRoadVehicleEquipment() {
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
		destination.append("On Road Vehicle Equipment:\r\n");
		for(Iterator i=runspec.onRoadVehicleSelections.iterator();i.hasNext();) {
			destination.append("\t" + i.next() + "\r\n");
		}
		destination.append("\r\n");
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		fuelLabel = new JLabel("Fuels:");
		fuelLabel.setName("fuelLabel");
		sourceUseTypeLabel = new JLabel("Source Use Types:");
		sourceUseTypeLabel.setName("sourceUseTypeLabel");
		selectionLabel = new JLabel("Selections:");
		selectionLabel.setName("selectionLabel");
		fuelListModel = new DefaultListModel<FuelEntry>();
		sourceUseTypeListModel = new DefaultListModel<SourceTypeEntry>();
		selectionListModel = new DefaultListModel<OnRoadVehicleSelection>();
		loadFuels();
		loadSourceUseTypes();
		loadValidFuelSourceCombinations();
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

		sourceUseTypeList = new JListWithToolTips<SourceTypeEntry>(sourceUseTypeListModel);

		sourceUseTypeList.setName("sourceUseTypeList");
		sourceUseTypeList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		sourceUseTypeList.setSelectedIndex(-1);
		sourceUseTypeList.addListSelectionListener(this);
		sourceUseTypeList.setVisibleRowCount(9);
		sourceUseTypeList.setPrototypeCellValue(new SourceTypeEntry() { public String toString() { return "CharacterCountToDisplay"; }});
		sourceUseTypeScrollPane = new JScrollPane(sourceUseTypeList);
		sourceUseTypeScrollPane = new JScrollPane(sourceUseTypeList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		sourceUseTypeScrollPane.setName("sourceUseTypeScrollPane");

		selectionList = new JListWithToolTips<OnRoadVehicleSelection>(selectionListModel);
		selectionList.setName("selectionList");
		selectionList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		selectionList.setSelectedIndex(-1);
		selectionList.addListSelectionListener(this);
		selectionList.setVisibleRowCount(9);
		selectionList.setPrototypeCellValue(new OnRoadVehicleSelection() { public String toString() { return "CharacterCountToDisplayXXXXXXXXXX"; }});
		selectionScrollPane = new JScrollPane(selectionList);
		selectionScrollPane = new JScrollPane(selectionList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		selectionScrollPane.setName("selectionScrollPane");

		fuelSelectAll = new JButton("Select All");
		fuelSelectAll.setName("fuelSelectAll");
		ToolTipHelper.add(fuelSelectAll,"Select all fuel types");
		sourceUseTypeSelectAll = new JButton("Select All");
		sourceUseTypeSelectAll.setName("sourceUseTypeSelectAll");
		ToolTipHelper.add(sourceUseTypeSelectAll,"Select all source use types");
		selectionDelete = new JButton("Delete");
		selectionDelete.setName("selectionDelete");
		selectionDelete.setEnabled(false); // disabled until selection made
		ToolTipHelper.add(selectionDelete,"Delete the selected Fuel and Source Use Type combinations");
		addFuelSourceUseTypeCombinations = new JButton("Add Fuel/Type Combinations");
		addFuelSourceUseTypeCombinations.setName("addFuelSourceUseTypeCombinations");
		addFuelSourceUseTypeCombinations.setEnabled(false); // disabled until item in list
		ToolTipHelper.add(addFuelSourceUseTypeCombinations,"Add selected Fuel and Source Use Type"
				+ " combinations");

		// Register a listener for the buttons.
		fuelSelectAll.addActionListener(this);
		sourceUseTypeSelectAll.addActionListener(this);
		selectionDelete.addActionListener(this);
		addFuelSourceUseTypeCombinations.addActionListener(this);

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
		add(addFuelSourceUseTypeCombinations, gbc);
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
		add(sourceUseTypeLabel, gbc);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		LayoutUtility.setPositionOnGrid(gbc,1, 1, "WEST", 1, 1);
		add(sourceUseTypeScrollPane, gbc);
		gbc.fill = GridBagConstraints.NONE;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,1, 2, "EAST", 1, 1);
		add(sourceUseTypeSelectAll, gbc);

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
		messageLogPanel.add(new JLabel("On Road Vehicle Equipment Requirements"));
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
		if (e.getValueIsAdjusting() == false) {
			updateButtonStates();
		}
	}

	/** Helper method for enabling/disabling all buttons depending upon list selections **/
	public void updateButtonStates() {
		if (fuelList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			fuelSelectAll.setEnabled(true);
			// the following button only enabled when item(s)
			// selected in both the fuel and sourceUseType lists.
			addFuelSourceUseTypeCombinations.setEnabled(false);
		} else if (fuelList.getSelectedIndices().length >= 1) {
			fuelSelectAll.setEnabled(true);
			if (sourceUseTypeList.getSelectedIndices().length < 1) {
				// the following button only enabled when item(s)
				// selected in both the fuel and sourceUseType lists.
				addFuelSourceUseTypeCombinations.setEnabled(false);
			} else {
				// the following button only enabled when item(s)
				// selected in both the fuel and sourceUseType lists.
				addFuelSourceUseTypeCombinations.setEnabled(true);
			}
		}
		if (sourceUseTypeList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			sourceUseTypeSelectAll.setEnabled(true);
			// the following button only enabled when item(s)
			// selected in both the fuel and sourceUseType lists.
			addFuelSourceUseTypeCombinations.setEnabled(false);
		} else if (sourceUseTypeList.getSelectedIndices().length >= 1) {
			sourceUseTypeSelectAll.setEnabled(true);
			if (fuelList.getSelectedIndices().length < 1) {
				// the following button only enabled when item(s)
				// selected in both the fuel and sourceUseType lists.
				addFuelSourceUseTypeCombinations.setEnabled(false);
			} else {
				// the following button only enabled when item(s)
				// selected in both the fuel and sourceUseType lists.
				addFuelSourceUseTypeCombinations.setEnabled(true);
			}
		}
		if (selectionList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			selectionDelete.setEnabled(false);
		} else if (selectionList.getSelectedIndices().length >= 1) {
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
		} else if (e.getSource() == sourceUseTypeSelectAll) {
			processSourceUseTypeSelectAllButton();
		} else if (e.getSource() == selectionDelete) {
			processSelectionDeleteButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if (e.getSource() == addFuelSourceUseTypeCombinations) {
			processFuelSourceUseTypeCombinationsButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		}
		updateButtonStates();
	}

	/** Handles the Fuel Select All button. **/
	public void processFuelSelectAllButton() {
		fuelList.clearSelection();
		fuelList.setSelectionInterval(0,fuelList.getModel().getSize()-1);
	}

	/** Handles the Source Use Type Select All button. **/
	public void processSourceUseTypeSelectAllButton() {
		sourceUseTypeList.clearSelection();
		sourceUseTypeList.setSelectionInterval(0,sourceUseTypeList.getModel().getSize()-1);
	}

	/** Handles the Delete button. **/
	public void processSelectionDeleteButton() {
		Object[] selectedItems = selectionList.getSelectedValuesList().toArray();
		for(int i=0;i<selectedItems.length;i++) {
			selectionListModel.removeElement(selectedItems[i]);
		}
	}

	/** Handles the Add Fuel/Type Combination button. **/
	public void processFuelSourceUseTypeCombinationsButton() {
		if(fuelList.getSelectedIndex() < 0 || sourceUseTypeList.getSelectedIndex() < 0) {
			return;
		}
		Object[] fuelItems = fuelList.getSelectedValuesList().toArray();
		Object[] sourceUseTypeItems = sourceUseTypeList.getSelectedValuesList().toArray();
		for(int i=0;i<fuelItems.length;i++) {
			for(int j=0;j<sourceUseTypeItems.length;j++) {
				FuelEntry fuelEntry = (FuelEntry)fuelItems[i];
				SourceTypeEntry sourceEntry = (SourceTypeEntry)sourceUseTypeItems[j];
				OnRoadVehicleSelection fs = new OnRoadVehicleSelection();
				fs.fuelTypeID = fuelEntry.fuelTypeID;
				fs.fuelTypeDesc = fuelEntry.fuelTypeDesc;
				fs.sourceTypeID = sourceEntry.sourceTypeID;
				fs.sourceTypeName = sourceEntry.sourceTypeName;
				boolean foundMatch = false;
				for(Enumeration e=selectionListModel.elements();e.hasMoreElements();) {
					OnRoadVehicleSelection selection = (OnRoadVehicleSelection)e.nextElement();
					if(selection.equals(fs)) {
						foundMatch = true;
						break;
					}
				}
				if(!foundMatch) {
					// Only add a combination if it exists in the database
					foundMatch = false;
					for(Iterator<OnRoadVehicleSelection> vfsc=validFuelSourceCombinations.iterator();vfsc.hasNext();) {
						OnRoadVehicleSelection validCombination = (OnRoadVehicleSelection)vfsc.next();
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
			messageLogModel.addElement(new String("Please select a Fuel and Source Use Type "
					+"combination."));
		} else {
			messageLogModel.clear();
			//boolean didWarnAboutMotorcycles = false;
			for(Enumeration sl=selectionListModel.elements();sl.hasMoreElements();) {
				OnRoadVehicleSelection selection = (OnRoadVehicleSelection)sl.nextElement();
				boolean foundMatch = false;
				for(Iterator<OnRoadVehicleSelection> vfsc=validFuelSourceCombinations.iterator();
						vfsc.hasNext();) {
					OnRoadVehicleSelection validCombination = (OnRoadVehicleSelection)vfsc.next();
					if(selection.equals(validCombination)) {
						foundMatch = true;
						break;
					}
				}
				if(!foundMatch) {
					messageLogPanel.setVisible(true);
					messageLogModel.addElement(new String(selection.fuelTypeDesc+"/"
							+ selection.sourceTypeName +" combination is not in the database."));
				}
				//if(selection.sourceTypeID == 11 && !didWarnAboutMotorcycles) {
				//	didWarnAboutMotorcycles = true;
				//	messageLogPanel.setVisible(true);
				//	messageLogModel.addElement(
				//		"This version of MOVES does not include criteria pollutant and air toxics emission factors for motorcycles.  Please see current MOVES documentation for more information.");
				//}
			}
		}
	}

	/**
	 * Saves the info to a RunSpec.
	 * @param	runspec the RunSpec to get the info
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.onRoadVehicleSelections.clear();
		for(int i=0;i<selectionListModel.getSize();i++) {
			runspec.onRoadVehicleSelections.add(
					(OnRoadVehicleSelection)selectionListModel.getElementAt(i));
		}
	}

	/**
	 * Loads the info from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		selectionListModel.removeAllElements();
		for(Iterator<OnRoadVehicleSelection> i=runspec.onRoadVehicleSelections.iterator();
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
		if(runspec.onRoadVehicleSelections.isEmpty()) {
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
		runspec.onRoadVehicleSelections.clear();
		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		sections.put(getName(),status);

		if(fuelList != null) {
			fuelList.clearSelection();
		}
		if(sourceUseTypeList != null) {
			sourceUseTypeList.clearSelection();
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
		String sql = "SELECT DISTINCT ft.fuelTypeID, ft.fuelTypeDesc"
				+" FROM FuelType ft, FuelEngTechAssoc feta"
				+" WHERE ft.fuelTypeID = feta.fuelTypeID"
				+" ORDER BY ft.fuelTypeDesc";
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

	/** load all sourceUseTypes from the database into the sourceUseTypeList listbox **/
	public void loadSourceUseTypes() {
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(null == db) {
			sourceUseTypeListModel.addElement(new SourceTypeEntry(5000,"Garbage Trucks"));
			sourceUseTypeListModel.addElement(new SourceTypeEntry(5001,"Pizza Delivery Cars"));
			return;
		}
		String sql="SELECT sourceTypeID,sourceTypeName FROM SourceUseType ORDER BY SourceTypeName";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				sourceUseTypeListModel.addElement(
						new SourceTypeEntry(query.rs.getInt(1),query.rs.getString(2)));
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to load a list of On Road Vehicle Eqiupment.");
		} finally {
			query.onFinally();
		}
	}

	/** Load valid fuel/source combinations **/
	public void loadValidFuelSourceCombinations() {
		validFuelSourceCombinations.clear();
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(null == db) {
			return;
		}
		String sql = "SELECT DISTINCT ft.fuelTypeID, ft.fuelTypeDesc, sut.sourceTypeID, "
				+"sut.sourceTypeName FROM FuelType ft, SourceUseType sut, FuelEngTechAssoc "
				+"feta WHERE ft.fuelTypeID = feta.fuelTypeID AND sut.sourceTypeID = "
				+"feta.sourceTypeID ORDER BY ft.fuelTypeDesc, sut.sourceTypeName";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				OnRoadVehicleSelection fs = new OnRoadVehicleSelection();
				fs.fuelTypeID = query.rs.getInt(1);
				fs.fuelTypeDesc = query.rs.getString(2);
				fs.sourceTypeID = query.rs.getInt(3);
				fs.sourceTypeName = query.rs.getString(4);
				validFuelSourceCombinations.add(fs);
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to load valid fuel/vehicle combinations.");
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
