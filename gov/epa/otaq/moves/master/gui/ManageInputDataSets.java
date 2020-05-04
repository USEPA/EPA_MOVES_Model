/**************************************************************************************************
 * @(#)ManageInputDataSets.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.sql.*;
import java.util.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;

/**
 * Class for MOVES ManageInputDataSets panel.
 * Constructs a ManageInputDataSets panel, and creates and sets the layouts of the controls.
 * The Databases combobox is loaded based on the server setting. The Window includes a Server
 * text, a Selections listbox, a Database combobox, a Database Description text, a Create
 * Database button, and several buttons for Add, Move Up, Move Down, and Delete. It
 * Loads/Saves the selections from/to Runspec.
 * @author		Wesley Faler
 * @author		Mitch C (Task 18 item 105)
 * @author		Tim Hull
 * @version		2013-08-04
**/
public class ManageInputDataSets extends JPanel implements ListSelectionListener,
		ActionListener, FocusListener,RunSpecEditor {
	/** Server label. **/
	JLabel serverLabel;
	/** Server text control. **/
	JTextField server;

	/** Database label. **/
	JLabel databaseLabel;
	/** Database combo control. **/
	ExtendedComboBox<String> databaseCombo;
	/** Database Description label. **/
	JLabel databaseDescriptionLabel;
	/**  Database Description text control. **/
	JTextField databaseDescription;
	/** Create Database button. **/
	JButton createDatabase;

	/** Add selection button **/
	JButton add;
	/** Refresh button **/
	JButton refresh;

	/** Selection label. **/
	JLabel selectionLabel;
	/** Selection default list model. **/
	DefaultListModel<DatabaseSelection> selectionListModel;
	/** Selection scroll pane. **/
	JScrollPane selectionScrollPane;
	/** Selection list. **/
	JList<DatabaseSelection> selectionList;

	/** Move Up selection button **/
	JButton moveUp;
	/** Move Down selection button **/
	JButton moveDown;
	/** Delete selection button **/
	JButton selectionDelete;

	/**
	 * Used by the server FocusLost event handler to help determine if the server
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the server TextField losing focus when the server name hasn't changed.
	**/
	String previousServer = new String();


	/**
	 *	Constructs a ManageInputDataSets panel, also creates and sets the layouts of the controls.
	**/
	public ManageInputDataSets() {
		createControls();
		arrangeControls();
		loadDatabases();
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("Manage Input Data Sets:\r\n");
		for(Iterator i=runspec.databaseSelectionInputSets.iterator();i.hasNext();) {
			destination.append("\tselection: " + i.next() + "\r\n");
		}
		destination.append("\r\n");
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		serverLabel = new JLabel("Server:");
		serverLabel.setName("serverLabel");
		server = new JTextField(12);
		server.setName("server");
		server.addFocusListener(this);
		ToolTipHelper.add(server,"Enter server where a user input database is located");

		databaseCombo = new ExtendedComboBox<String>();
		databaseCombo.setName("databaseCombo");
		databaseCombo.addActionListener(this);
		databaseLabel = new JLabel("Database:");
		databaseLabel.setName("databaseLabel");
		databaseCombo.setEditable(true);
		databaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(databaseCombo,"Enter or select the name of a user input database");

		databaseDescriptionLabel = new JLabel("Description:");
		databaseDescriptionLabel.setName("databaseDescriptionLabel");
		databaseDescription = new JTextField(12);
		databaseDescription.setName("databaseDescription");
		databaseDescription.addFocusListener(this);
		ToolTipHelper.add(databaseDescription,"Edit the description of the use input database");

		createDatabase = new JButton("Create Database...");
		createDatabase.setName("createDatabase");
		createDatabase.addActionListener(this);
		ToolTipHelper.add(createDatabase,"Create the user input database");

		add = new JButton("Add");
		add.setName("add");
		add.addActionListener(this);
		ToolTipHelper.add(add,"Add a user input database to the selection list");

		refresh = new JButton("Refresh");
		refresh.setName("refresh");
		refresh.addActionListener(this);
		ToolTipHelper.add(refresh,"Refresh the list of available databases");

		selectionLabel = new JLabel("Selections:");
		selectionLabel.setName("selectionLabel");

		DatabaseSelection prototypeValue = new DatabaseSelection();
		prototypeValue.description = "CharacterCountToDisplayXXXXXXXXXXXXXXXXXXXXXXX";

		selectionListModel = new DefaultListModel<DatabaseSelection>();
		selectionList = new JListWithToolTips<DatabaseSelection>(selectionListModel);
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

		moveUp = new JButton("Move Up");
		moveUp.setName("moveUp");
		moveUp.setEnabled(false); // not enabled until item selected from list
		moveUp.addActionListener(this);
		ToolTipHelper.add(moveUp,"Move the database up in the order it will be input");

		moveDown = new JButton("Move Down");
		moveDown.setName("moveDown");
		moveDown.setEnabled(false); // not enabled until item selected from list
		moveDown.addActionListener(this);
		ToolTipHelper.add(moveDown,"Move the database down in the order it will be input");

		selectionDelete = new JButton("Delete");
		selectionDelete.setName("selectionDelete");
		selectionDelete.setEnabled(false); // not enabled until item selected from list
		selectionDelete.addActionListener(this);
		ToolTipHelper.add(selectionDelete,"Delete this database from the list of databases to "
				+ "be input");

		Dimension buttonSize = new Dimension(99,25);
		add.setPreferredSize(buttonSize);
		moveUp.setPreferredSize(buttonSize);
		moveDown.setPreferredSize(buttonSize);
		selectionDelete.setPreferredSize(buttonSize);
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 5;
		gbc.gridheight = 6;
		gbc.weightx = 0;
		gbc.weighty = 0;
		setLayout(new GridBagLayout());

		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		add(serverLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,0, "WEST", 1, 1);
		add(server, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		add(databaseLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 1, 1);
		add(databaseCombo, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		add(databaseDescriptionLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,2, "WEST", 1, 1);
		add(databaseDescription, gbc);

		JPanel p = new JPanel();
		p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
		p.add(Box.createHorizontalGlue());
		p.add(add);
		p.add(refresh);
		LayoutUtility.setPositionOnGrid(gbc,1,3, "EAST", 1, 1);
		add(p, gbc);
		/*
		LayoutUtility.setPositionOnGrid(gbc,2,3, "CENTER", 1, 1);
		add(add, gbc);
		LayoutUtility.setPositionOnGrid(gbc,2,3, "CENTER", 1, 1);
		add(refresh, gbc);
		*/

		LayoutUtility.setPositionOnGrid(gbc,1,4, "EAST", 2, 1);
		add(createDatabase, gbc);

		LayoutUtility.setPositionOnGrid(gbc,3,0, "WEST", 1, 1);
		add(selectionLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,1, "WEST", 3, 4);
		add(selectionScrollPane, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,5, "WEST", 1, 1);
		add(moveUp, gbc);
		LayoutUtility.setPositionOnGrid(gbc,4,5, "CENTER", 1, 1);
		add(moveDown, gbc);
		LayoutUtility.setPositionOnGrid(gbc,5,5, "EAST", 1, 1);
		add(selectionDelete, gbc);
	}

	/**
	 * Loads the Databases droplist, based on the server setting.
	 * Also, sets the default droplist selection, if it can be found.
	**/
	public void loadDatabases() {
		//long start = System.currentTimeMillis();
		//Logger.log(LogMessageCategory.INFO,"Loading input databases");
		databaseDescription.setText("");
		databaseCombo.removeAllItems();
		// add the default item (no selection)
		databaseCombo.addItem(new String(""));
		// get the available databases from the current server selection
		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.serverName = server.getText();
		dbSelection.databaseName = "";
		Connection db = dbSelection.openConnectionOrNull();
		if(null == db) {
			Logger.log(LogMessageCategory.ERROR,"Could not connect to the server");
			return;
		}
		String sql = "SHOW DATABASES";
		PreparedStatement statement;
		ResultSet results;
		try {
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					String nextDB = results.getString(1);
					databaseCombo.addItem(nextDB);
				}
				results.close();
			}
			statement.close();
		} catch(Exception e) {
			Logger.logError(e,"Loading a list of databases for Manage Input Data Sets failed.");
			DatabaseUtilities.closeConnection(db);
			return;
		}
		// second pass through the returned list of database names, must now
		// remove any databases that aren't an input database
		ComboBoxModel model = databaseCombo.getModel();
		ArrayList stringsToRemove = new ArrayList();
		try {
			boolean isInputDatabase = false;
			for(int i = 0; i < model.getSize(); i++) {
				Object nextDatabase = model.getElementAt(i);
				if(nextDatabase.toString().length() == 0) {
					continue;
				}
				/* This takes too long, so defer until the user selects the database.
				try {
					SQLRunner.executeSQL(db,"USE " + nextDatabase.toString());
					if(!SchemaInspector.isMOVESSchemaPresent(db,true,true,
							nextDatabase.toString())) {
						stringsToRemove.add(nextDatabase.toString());
					}
				} catch(Exception e) {
					// SQL exception here just means the database not an input database
					stringsToRemove.add(nextDatabase.toString());
				}
				*/
			}
			// now run through any database names to remove (i.e. databases that aren't inputs)
			for(int i = 0; i < stringsToRemove.size(); i++) {
				databaseCombo.removeItem(stringsToRemove.get(i));
			}
		} catch(Exception e) {
			Logger.logError(e,"Limiting the list of databases to just MOVES Input Databsaes "+
					"failed.");
			// Just go on and let the connection get closed and a default database get chosen
		}
		// set the default selection
		databaseCombo.setSelectedItem("");
		DatabaseUtilities.closeConnection(db);
		//long end = System.currentTimeMillis();
		//Logger.log(LogMessageCategory.INFO,"Done loading input databases: " + (end - start) + " ms");
	}

	/**
	 * Saves the description text to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.databaseSelectionInputSets.clear();
		for(int i=0;i<selectionListModel.getSize();i++) {
			runspec.databaseSelectionInputSets.add(
					(DatabaseSelection)selectionListModel.getElementAt(i));
		}
	}

	/**
	 * Loads the description text from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		selectionListModel.removeAllElements();
		for(Iterator<DatabaseSelection> i=runspec.databaseSelectionInputSets.iterator();i.hasNext();) {
			selectionListModel.addElement(i.next());
		}
		updateButtonStates();
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		boolean isOk = true;

		for(Iterator i=runspec.databaseSelectionInputSets.iterator();i.hasNext();) {
			DatabaseSelection userSelection = (DatabaseSelection)i.next();
			if(userSelection.databaseName.
					equalsIgnoreCase(runspec.outputDatabase.databaseName)) {
				isOk = false;
				break;
			}
		}

		sections.remove(getName());
		RunSpecSectionStatus status;
		if(isOk) {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		} else {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
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
		databaseCombo.setSelectedItem("");
		server.setText("");
		databaseDescription.setText("");

		runspec.databaseSelectionInputSets = new LinkedList<DatabaseSelection>();
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
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == createDatabase) {
			processCreateDatabaseButton();
		} else if(e.getSource() == databaseCombo) {
			processDatabaseComboChange();
		} else if(e.getSource() == refresh) {
			loadDatabases();
		} else if(e.getSource() == moveUp) {
			processMoveUpButton();
		} else if(e.getSource() == moveDown) {
			processMoveDownButton();
		} else if(e.getSource() == add) {
			processAddButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == selectionDelete) {
			processDeleteSelectionButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		}
		updateButtonStates();
	}

	/** Handles the create database button. **/
	public void processCreateDatabaseButton() {
		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName.length() == 0) {
			Logger.log(LogMessageCategory.WARNING, "Specify a database name.");
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)databaseCombo.getSelectedItem());
		if(validDatabaseName(dbSelection)) {
			int createStatus = dbSelection.safeCreateDatabase("database/CreateDefault.sql");
			if(DatabaseSelection.CREATED == createStatus) {
				// show a success message and add this item to the list,
				// in case the user hits the droplist button
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"MOVES Input database successfully created.", 60));
				newDatabaseName = addIfNotInComboBox(newDatabaseName);
			} else if(DatabaseSelection.EXISTS == createStatus) {
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"MOVES Input database already exists.", 60));
				newDatabaseName = addIfNotInComboBox(newDatabaseName);
			} else {
				Logger.log(LogMessageCategory.ERROR,
						"Could not create the Input database.");
			}
		}
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

	/** Handles the database combo change. **/
	public void processDatabaseComboChange() {
		// Nothing to do here
	}

	/** Handles the add button. **/
	public void processAddButton() {
		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.serverName = server.getText();
		dbSelection.databaseName = StringUtilities.safeGetString(databaseCombo.getSelectedItem());
		dbSelection.description = databaseDescription.getText();
		if(validDatabaseName(dbSelection)) {
			for(int i=0;i<selectionListModel.getSize();i++) {
				DatabaseSelection existingSelection =
						(DatabaseSelection)selectionListModel.getElementAt(i);
				if(existingSelection.equals(dbSelection)) {
					// Complain
					JOptionPane.showMessageDialog(this,
							"An entry for that database and server already exists.");
					return;
				}
			}

			Connection db = null;
			boolean isValid = false;
			try {
				db = dbSelection.openConnection();
				if(SchemaInspector.isMOVESSchemaPresent(db,true,true,dbSelection.databaseName)) {
					isValid = true;
				}
			} catch(Exception e) {
				isValid = false;
			} finally {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
			if(!isValid) {
				JOptionPane.showMessageDialog(this,
						dbSelection.databaseName + " is not a valid MOVES database.");
				return;
			}

			selectionListModel.addElement(dbSelection);
			addIfNotInComboBox(dbSelection.databaseName);
		}
	}

	/** Valid Database Name
	 * @param	dbSelection the database selection.
	 * @return boolean if database name is valid.
	 **/
	public boolean validDatabaseName(DatabaseSelection dbSelection) {
		DatabaseSelection defaultDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
		//default database cannot be accepted as an input selection
		if ((defaultDB.databaseName).equalsIgnoreCase(dbSelection.databaseName)) {
			JOptionPane.showMessageDialog(this,
					"The default database cannot be used as an input database.");
			return false;
		}
		//execution database cannot be accepted as an input selection
		DatabaseSelection executionDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()];
		if ((executionDB.databaseName).equalsIgnoreCase(dbSelection.databaseName)) {
			JOptionPane.showMessageDialog(this,
					"The execution database cannot be used as an input database.");
			return false;
		}
		//Worker database cannot be accepted as an input selection
		if (new String("MOVESWorker").equalsIgnoreCase(dbSelection.databaseName)) {
			JOptionPane.showMessageDialog(this,
					"MOVESWorker database cannot be used as an input database.");
			return false;
		}
		//MySQL database cannot be accepted as an input selection
		if (new String("MySQL").equalsIgnoreCase(dbSelection.databaseName)) {
			JOptionPane.showMessageDialog(this,
					"MySQL database cannot be used as an input database.");
			return false;
		}
		return true;
	}


	/** Handles the move up button. **/
	public void processMoveUpButton() {
		if(selectionList.getSelectedIndices().length != 1) {
			return;
		}
		int currentIndex = selectionList.getSelectedIndices()[0];
		if(currentIndex < 1) {
			// Already at the top of the list
			return;
		}
		DatabaseSelection currentItem = selectionListModel.getElementAt(currentIndex);
		DatabaseSelection aboveItem = selectionListModel.getElementAt(currentIndex-1);
		selectionListModel.setElementAt(currentItem,currentIndex-1);
		selectionListModel.setElementAt(aboveItem,currentIndex);

		selectionList.setSelectionInterval(currentIndex-1,currentIndex-1);
	}

	/** Handles the move down button. **/
	public void processMoveDownButton() {
		if(selectionList.getSelectedIndices().length != 1) {
			return;
		}
		int currentIndex = selectionList.getSelectedIndices()[0];
		if(currentIndex >= selectionListModel.size()-1) {
			// Already at the bottom of the list
			return;
		}
		DatabaseSelection currentItem = selectionListModel.getElementAt(currentIndex);
		DatabaseSelection belowItem = selectionListModel.getElementAt(currentIndex+1);
		selectionListModel.setElementAt(currentItem,currentIndex+1);
		selectionListModel.setElementAt(belowItem,currentIndex);

		selectionList.setSelectionInterval(currentIndex+1,currentIndex+1);
	}

	/** Handles the deleteSelection button. **/
	public void processDeleteSelectionButton() {
		Object[] selectedItems = selectionList.getSelectedValuesList().toArray();
		for(int i=0;i<selectedItems.length;i++) {
			selectionListModel.removeElement(selectedItems[i]);
		}
	}

	/**
	 * Listener method for list selection changes.
	 * @param e The event caused by the value change
	**/
	public void valueChanged(ListSelectionEvent e) {
		if(e.getValueIsAdjusting() == false) {
			updateButtonStates();
		}
	}

	/** Helper method for enabling/disabling all buttons depending upon list selections **/
	public void updateButtonStates() {
		if(selectionList == null || selectionDelete == null || moveUp == null || moveDown == null
				|| add == null || server == null || databaseCombo == null) {
			return;
		}
		if(selectionList.getSelectedIndex() == -1) {
			//No selection: enable/disable relevant controls.
			selectionDelete.setEnabled(false);
			moveUp.setEnabled(false);
			moveDown.setEnabled(false);
		}
		if(selectionList.getSelectedIndices().length > 1) {
			//Multiple selection: enable/disable relevant controls.
			selectionDelete.setEnabled(true);
			moveUp.setEnabled(false);
			moveDown.setEnabled(false);
		}
		if(selectionList.getSelectedIndices().length == 1) {
			//Single selection: enable/disable relevant controls.
			selectionDelete.setEnabled(true);
			int currentIndex = selectionList.getSelectedIndices()[0];
			if(currentIndex < 1) {
				// Already at the top of the list
				moveUp.setEnabled(false);
			} else {
				moveUp.setEnabled(true);
			}
			if(currentIndex >= selectionListModel.size()-1) {
				// Already at the bottom of the list
				moveDown.setEnabled(false);
			} else {
				moveDown.setEnabled(true);
			}
		}
		// The Add button requires there to be a server and a database name
		// (the description is optional)
		String serverName;
		String databaseName;
		if(server.getText().length() > 0) {
			serverName = server.getText();
		} else {
			serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		databaseName =
				StringUtilities.safeGetString((String)databaseCombo.getSelectedItem());
		if(serverName.length() > 0 && databaseName.length() > 0) {
			add.setEnabled(true);
		} else {
			add.setEnabled(false);
		}
	}

	/**
	 * Handles the focus lost event for the server textfield.
	 * @param	e The event caused by focus being lost.
	**/
	public void focusLost(FocusEvent e) {
		JComponent c = (JComponent)e.getComponent();
		if(c == server) {
			if(previousServer.equals(server.getText())) {
				return;
			}
			previousServer = server.getText();
			loadDatabases();
			updateButtonStates();
		}
	}

	/**
	 * Currently not used.
	 * @param	e The event caused by focus being gained.
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
		// Nothing to do here
		return null;
	}
}
