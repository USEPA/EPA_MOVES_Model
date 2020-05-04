/**************************************************************************************************
 * @(#)GeneralOutput.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.plaf.metal.MetalComboBoxUI;
import javax.swing.plaf.basic.BasicComboBoxRenderer;
import javax.swing.plaf.basic.*;
import java.sql.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;
import java.util.*;

/**
 * Class for MOVES GeneralOutput panel.
 *
 * Panel contains the following output database controls, the Server Textbox, the Database names
 * combo box, the Create Database button, tthe Time Factors Option box, the Emission option
 * checkbox, the Time units combo box, the Mass Units combo box, and energy Units combobox. The
 * following is the arrangement of controls on the panel.
 * <pre>
 *	Server (text)		Database (combobox)
 *					Create Database (button)
 *
 * Output Time Period (combobox: hour, day, month, year)
 *
 * Output
 *	Time factors (checkbox)
 *	Mass Units (combobox: Kilograms, Grams, Pounds, U. S. Ton)
 *	Energy Units (combobox: Joules, Million BTU)
 * </pre>
 *
 * This class creates, initializes, and sets the layouts of the controls,
 * opens a connection to the selected server and database,
 * loads/saves the server and database names from/to a RunSpec,
 * loads output time step combo box with the options available for the current Time Span
 * selections, and adjusts unit combo boxes based on the checkbox settings.
 *
 * @author		Wesley Faler
 * @author		Mitch C
 * @author      Gwo Shyu, EPA (Fix Alpha Testing Bug 31)
 * @author		Tim Hull
 * @version		2015-05-21
**/
public class GeneralOutput extends JPanel implements ActionListener,
		FocusListener, RunSpecEditor {
	/** Panel contains output database controls. **/
	JPanel outputDatabasePanel;
	/** Panel contains output. **/
	JPanel outputPanel;
	/** Panel contains activity. **/
	JPanel activityPanel;

	/** Server label. **/
	JLabel serverLabel;
	/** Server text control. **/
	JTextField server;

	/** Database label. **/
	JLabel databaseLabel;
	/** Database combo control. **/
	ExtendedComboBox<String> databaseCombo;
	/** Run droplist ToolTip **/
	JToolTip databaseToolTip;

	/** Data Exists label. **/
	JLabel dataExistsLabel;

	/** Create Database button. **/
	JButton createDatabase;
	/** Refresh databases button **/
	JButton refresh;

	/** Data Exists image icon. **/
	ImageIcon dataExistsImage;

	/** VMT option checkbox. **/
	JCheckBox VMT;
	/** SHO option checkbox **/
	JCheckBox SHO;
	/** SH option checkbox **/
	JCheckBox SH;
	/** SHP option checkbox **/
	JCheckBox SHP;
	/** SH Idling checkbox **/
	JCheckBox SHIdling;
	/** Starts checkbox **/
	JCheckBox Starts;
	/** Population checkbox **/
	JCheckBox Population;

	/** Mass Units label. **/
	JLabel massUnitsLabel;
	/** Mass Units combo control. **/
	ExtendedComboBox<MassMeasurementSystem> massUnitsCombo;

	/** Energy Units label. **/
	JLabel energyUnitsLabel;
	/** Energy Units combo control. **/
	ExtendedComboBox<EnergyMeasurementSystem> energyUnitsCombo;

	/** Distance Units label. **/
	JLabel distanceUnitsLabel;
	/** Distance Units combo control. **/
	ExtendedComboBox<DistanceMeasurementSystem> distanceUnitsCombo;

	/**
	 * Used by the server FocusLost event handler to help determine if the server
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the server TextField losing focus when the server name hasn't changed.
	**/
	String previousServer = new String();

	/**
	 *	Constructs a GeneralOutput panel, also creates and sets the layouts of the controls.
	**/
	public GeneralOutput() {
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
		destination.append("General Output:\r\n");
		String appendText = new String("");
		if((runspec.outputDatabase.serverName != null)
				&& (runspec.outputDatabase.serverName.length() > 0)) {
			appendText = runspec.outputDatabase.serverName;
		} else {
			appendText = "[using default]";
		}
		destination.append("\tOutput Database Server Name: " + appendText + "\r\n");
		if((runspec.outputDatabase.databaseName != null)
				&& (runspec.outputDatabase.databaseName.length() > 0)) {
			appendText = runspec.outputDatabase.databaseName;
		} else {
			appendText = "[using default]";
		}
		destination.append("\tOutput Database Name: " + appendText + "\r\n");

		destination.append("\tUnits:\r\n");
		if(runspec.outputFactors.massMeasurementSystem!=null) {
			destination.append("\t\tMass Units: " + runspec.outputFactors.massMeasurementSystem + "\r\n");
		}
		if(runspec.outputFactors.energyMeasurementSystem!=null) {
			destination.append("\t\tEnergy Units: " + runspec.outputFactors.energyMeasurementSystem + "\r\n");
		}
		if(runspec.outputFactors.distanceMeasurementSystem!=null) {
			destination.append("\t\tDistance Units: " + runspec.outputFactors.distanceMeasurementSystem + "\r\n");
		}

		destination.append("\tActivity Outputs:\r\n");
		boolean hasActivity = false;
		if(runspec.outputVMTData) {
			hasActivity = true;
			destination.append("\t\tDistance Traveled\r\n");
		}
		if(runspec.outputSH) {
			hasActivity = true;
			destination.append("\t\tSource Hours\r\n");
		}
		if(runspec.outputSHIdling) {
			hasActivity = true;
			destination.append("\t\tHotelling Hours\r\n");
		}
		if(runspec.outputSHO) {
			hasActivity = true;
			destination.append("\t\tSource Hours Operating\r\n");
		}
		if(runspec.outputSHP) {
			hasActivity = true;
			destination.append("\t\tSource Hours Parked\r\n");
		}
		if(runspec.outputPopulation) {
			hasActivity = true;
			destination.append("\t\tPopulation\r\n");
		}
		if(runspec.outputStarts) {
			hasActivity = true;
			destination.append("\t\tStarts\r\n");
		}
		if(!hasActivity) {
			destination.append("\t\t[No Activity Outputs Selected]\r\n");
		}

		destination.append("\r\n");
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		Dimension outputDatabasePanelSize = new Dimension(480, 130); // 480x100
		outputDatabasePanel = new JPanel();
		outputDatabasePanel.setName("outputDatabasePanel");
		outputDatabasePanel.setBorder(BorderFactory.createTitledBorder(
				"Output Database"));
		outputDatabasePanel.setPreferredSize(outputDatabasePanelSize);

		Dimension outputPanelSize = new Dimension(240,240);
		outputPanel = new JPanel();
		outputPanel.setName("outputPanel");
		outputPanel.setBorder(BorderFactory.createTitledBorder("Units"));
		outputPanel.setPreferredSize(outputPanelSize);

		Dimension activityPanelSize = new Dimension(235, 240); // 240x180
		activityPanel = new JPanel();
		activityPanel.setName("activityPanel");
		activityPanel.setBorder(BorderFactory.createTitledBorder("Activity"));
		activityPanel.setPreferredSize(activityPanelSize);

		serverLabel = new JLabel("Server:");
		serverLabel.setName("serverLabel");
		server = new JTextField(10);
		ToolTipHelper.add(server,"Edit the name of the server where the output database will be located");
		server.setName("server");
		server.addFocusListener(this);
		server.setColumns(10);

		databaseLabel = new JLabel("Database:");
		databaseLabel.setName("databaseLabel");
		databaseCombo = new ExtendedComboBox<String>();
		Dimension d = databaseCombo.getPreferredSize();
		databaseCombo.setPreferredSize(new Dimension(250, d.height)); // 150
		databaseCombo.setPopupWidth(databaseCombo.getPreferredSize().width);
		databaseCombo.setName("databaseCombo");
		databaseCombo.addActionListener(this);
		databaseCombo.setEditable(true);
		databaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(databaseCombo,"Edit or select the name of the database in which the output will be stored");

		dataExistsImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/dataExists.gif");
		dataExistsLabel = new JLabel("Data is already in this database.", dataExistsImage,
				JLabel.LEFT);
		dataExistsLabel.setName("dataExistsLabel");
		dataExistsLabel.setHorizontalTextPosition(JLabel.RIGHT);
		dataExistsLabel.setVerticalTextPosition(JLabel.CENTER);
		createDatabase = new JButton("Create Database...");
		createDatabase.setName("createDatabase");
		createDatabase.addActionListener(this);
		ToolTipHelper.add(createDatabase,"Create the output database if it does not already exist");

		refresh = new JButton("Refresh");
		refresh.setName("refresh");
		refresh.addActionListener(this);
		ToolTipHelper.add(refresh,"Refresh the list of available databases");

		VMT = new JCheckBox("Distance Traveled");
		VMT.setName("VMT");
		ToolTipHelper.add(VMT,"Include distance traveled in simulation");
        VMT.setSelected(false); // Gwo Shyu

		SHO = new JCheckBox("Source Hours Operating");
		SHO.setName("SHO");
		ToolTipHelper.add(SHO,"Include Source Hours Operating");
        SHO.setSelected(false);

		SH = new JCheckBox("Source Hours");
		SH.setName("SH");
		ToolTipHelper.add(SH,"Include Source Hours");
        SH.setSelected(false);

		SHP = new JCheckBox("Source Hours Parked");
		SHP.setName("SHP");
		ToolTipHelper.add(SHP,"Include Source Hours Parked");
        SHP.setSelected(false);

		SHIdling = new JCheckBox("Hotelling Hours");
		SHIdling.setName("SHIdling");
		ToolTipHelper.add(SHIdling,"Include Hotelling Hours");
        SHIdling.setSelected(false);

		Starts = new JCheckBox("Starts");
		Starts.setName("Starts");
		ToolTipHelper.add(Starts,"Include Starts");
        Starts.setSelected(false);

		Population = new JCheckBox("Population");
		Population.setName("Population");
		ToolTipHelper.add(Population,"Include Population");
        Population.setSelected(false);

		massUnitsLabel = new JLabel("Mass Units:");
		massUnitsLabel.setName("massUnitsLabel");
		massUnitsCombo = new ExtendedComboBox<MassMeasurementSystem>();
		massUnitsCombo.setName("massUnitsCombo");
		massUnitsCombo.addActionListener(this);
		massUnitsCombo.setEditable(false);
		for(Iterator<MassMeasurementSystem> i = MassMeasurementSystem.allTypes.iterator(); i.hasNext();) {
			massUnitsCombo.addItem(i.next());
		}
		massUnitsCombo.setSelectedIndex(-1);
		ToolTipHelper.add(massUnitsCombo,"Select the mass units to use for output data");

		energyUnitsLabel = new JLabel("Energy Units:");
		energyUnitsLabel.setName("energyUnitsLabel");
		energyUnitsCombo = new ExtendedComboBox<EnergyMeasurementSystem>();
		energyUnitsCombo.setName("energyUnitsCombo");
		energyUnitsCombo.addActionListener(this);
		energyUnitsCombo.setEditable(false);
		for(Iterator<EnergyMeasurementSystem> i = EnergyMeasurementSystem.allTypes.iterator(); i.hasNext();) {
			energyUnitsCombo.addItem(i.next());
		}
		energyUnitsCombo.setSelectedIndex(-1);
		ToolTipHelper.add(energyUnitsCombo,"Select the energy units to use for the output data");

		distanceUnitsLabel = new JLabel("Distance Units:");
		distanceUnitsLabel.setName("distanceUnitsLabel");
		distanceUnitsCombo = new ExtendedComboBox<DistanceMeasurementSystem>();
		distanceUnitsCombo.setName("distanceUnitsCombo");
		distanceUnitsCombo.addActionListener(this);
		distanceUnitsCombo.setEditable(false);
		for(Iterator<DistanceMeasurementSystem> i = DistanceMeasurementSystem.allTypes.iterator(); i.hasNext();) {
			DistanceMeasurementSystem iterItem = (DistanceMeasurementSystem) i.next();
			distanceUnitsCombo.addItem (iterItem);
		}
		distanceUnitsCombo.setSelectedIndex(-1);
		ToolTipHelper.add(distanceUnitsCombo,"Select the distance units to use for the output data");

		massUnitsCombo.setEnabled(true);
		energyUnitsCombo.setEnabled(true);
		distanceUnitsCombo.setEnabled(true);
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;

		outputDatabasePanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 5;
		gbc.gridheight = 3;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		outputDatabasePanel.add(serverLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,0, "WEST", 2, 1);
		outputDatabasePanel.add(server, gbc);
		LayoutUtility.setPositionOnGrid(gbc,4,0, "WEST", 1, 1);
		outputDatabasePanel.add(refresh, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		outputDatabasePanel.add(databaseLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 3, 1);
		outputDatabasePanel.add(databaseCombo, gbc);
		LayoutUtility.setPositionOnGrid(gbc,4,1, "WEST", 1, 1);
		outputDatabasePanel.add(createDatabase, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 5, 1);
		outputDatabasePanel.add(dataExistsLabel, gbc);


		JPanel p = new JPanel();
		p.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 3;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		p.add(massUnitsLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,0, "WEST", 1, 1);
		p.add(massUnitsCombo, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		p.add(energyUnitsLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 1, 1);
		p.add(energyUnitsCombo, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		p.add(distanceUnitsLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,2, "WEST", 1, 1);
		p.add(distanceUnitsCombo, gbc);

		outputPanel.add(p,BorderLayout.NORTH);
		outputPanel.add(Box.createVerticalGlue(),BorderLayout.CENTER);


		p = new JPanel();
		p.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 7;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		p.add(VMT, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		p.add(SH, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		p.add(SHIdling, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,3, "WEST", 1, 1);
		p.add(SHO, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,4, "WEST", 1, 1);
		p.add(SHP, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,5, "WEST", 1, 1);
		p.add(Population, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,6, "WEST", 1, 1);
		p.add(Starts, gbc);

		activityPanel.add(p,BorderLayout.NORTH);
		activityPanel.add(Box.createVerticalGlue(),BorderLayout.CENTER);


		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 5;
		gbc.gridheight = 9;
		gbc.weightx = 0;
		gbc.weighty = 0;
		setLayout(new GridBagLayout());
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 5, 2);
		add(outputDatabasePanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,3, "WEST", 2, 6);
		add(outputPanel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,3, "WEST", 2, 6);
		add(activityPanel, gbc);
	}

	/**
	 * Attempt to open a connection to the server and database shown on screen.
	 * @param settleForJustServer true if it is OK to just connect to the server without a valid
	 * database named.
	 * @return a Connection object that should be closed with DatabaseUtilities.closeConnection
	 * and not by returning it to the DatabaseConnectionManager.
	**/
	Connection openCurrentOutputDatabase(boolean settleForJustServer) {
		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)databaseCombo.getSelectedItem());
		Connection db = dbSelection.openConnectionOrNull();
		if(null == db && settleForJustServer) {
			// try again to get a connection, but specify an empty string for the database
			// name, in case the current one is invalid, this will at least allow us to get
			// a connection to the output server to get the database list
			dbSelection.databaseName = "";
			db = dbSelection.openConnectionOrNull();
		}
		return db;
	}

	/**
	 * Loads the Databases droplist, based on the server setting.
	 * Also, sets the default droplist selection, if it can be found.
	**/
	public void loadDatabases() {
		//long start = System.currentTimeMillis();
		//Logger.log(LogMessageCategory.INFO,"Loading output databases");
		databaseCombo.removeAllItems();
		// add the default item (no selection)
		databaseCombo.addItem(new String(""));
		TreeSet<String> databases = new TreeSet<String>();
		// get the available databases from the current server selection
		Connection db = openCurrentOutputDatabase(true);
		if(null == db) {
			Logger.log(LogMessageCategory.ERROR,"Could not connect to the output database");
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
					//databaseCombo.addItem(nextDB);
					databases.add(nextDB);
				}
				results.close();
			}
			statement.close();
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show databases (load databases) "
					+ "in GeneralOutput.");
			DatabaseUtilities.closeConnection(db);
			return;
		}
		// second pass through the returned list of database names, must now
		// remove any databases that aren't an output database
		//ArrayList<String> stringsToRemove = new ArrayList<String>();
		try {
			/*
			boolean foundOutputTable = false;
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				foundOutputTable = false;
				String nextDatabase = (String)i.next();
				if(nextDatabase.length() == 0) {
					continue;
				}
				// look at all tables from the next databaseName, compare the table
				// names to one output table name
				sql = "SHOW TABLES FROM " + nextDatabase;
				try {
					statement = db.prepareStatement(sql);
					results = SQLRunner.executeQuery(statement,sql);
					if(results != null) {
						while(results.next()) {
							String nextTable = results.getString(1);
							if(nextTable.equalsIgnoreCase("MOVESOUTPUT")) {
								foundOutputTable = true;
								break;
							}
						}
					}
				} catch (Exception e) {
					// SQL error here just means this database not an output database
				}
				// check if this database has any output tables, if not must add
				// the databaseName to the remove names list
				if(!foundOutputTable) {
					stringsToRemove.add(nextDatabase);
				}
			}
			// now run through any database names to remove (i.e. databases that don't
			// contain an output table)
			for(int i = 0; i < stringsToRemove.size(); i++) {
				databases.remove(stringsToRemove.get(i));
				//databaseCombo.removeItem(stringsToRemove.get(i));
			}
			*/
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
			Logger.logError(e, "Failed to show tables from database "
					+ "in GeneralOutput.");
		}
		// set the default selection
		databaseCombo.setSelectedItem("");
		DatabaseUtilities.closeConnection(db);
		//long end = System.currentTimeMillis();
		//Logger.log(LogMessageCategory.INFO,"Done loading output databases: " + (end - start) + " ms");
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == createDatabase) {
			processCreateDatabaseButton();
		} else if(e.getSource() == refresh) {
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
			loadDatabases();
		} else if(e.getSource() == databaseCombo) {
			processDatabaseComboChange();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == massUnitsCombo || e.getSource() == distanceUnitsCombo
			 || e.getSource() == energyUnitsCombo  ) {
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		}
	}

	/** Handles the database combo change. **/
	public void processDatabaseComboChange() {
		if(databaseCombo.getSelectedItem() == null) {
			showExistingDataWarning(false);
			return;
		}
		if(databaseCombo.getSelectedItem().toString().length() == 0) {
			showExistingDataWarning(false);
			return;
		}

		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			Logger.log(LogMessageCategory.WARNING, "Specify a database name.");
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].serverName;
		}
		dbSelection.databaseName = StringUtilities.safeGetString(newDatabaseName);

		String status = MOVESEngine.isOutputDatabaseNameValid(dbSelection);
		if(status == null) {
			String sql;
			PreparedStatement statement;

			// try to connect to the new selection
			Connection db = openCurrentOutputDatabase(false);
			if(null == db) {
				showExistingDataWarning(false);
				return;
			}
			try {
				sql = "SELECT COUNT(*) FROM MOVESOUTPUT";
				statement = db.prepareStatement(sql);
				ResultSet results = SQLRunner.executeQuery(statement,sql);
				if(results != null) {
					results.next();
					int count = results.getInt(1);
					showExistingDataWarning(count != 0);
					results.close();
				}
				statement.close();
			} catch(Exception e) {
				showExistingDataWarning(false);
			}
			DatabaseUtilities.closeConnection(db);
			newDatabaseName = addIfNotInComboBox(newDatabaseName);
			databaseCombo.setSelectedItem(newDatabaseName);
		} else {
			JOptionPane.showMessageDialog(this, status);
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

	/**
	 * Handles the Create Database button.  Allows the user to run this command even if a
	 * database is currently selected.  This has the effect of safely adding any missing
	 * output tables.
	**/
	public void processCreateDatabaseButton() {
		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			Logger.log(LogMessageCategory.WARNING, "Specify a database name.");
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].serverName;
		}
		dbSelection.databaseName = StringUtilities.safeGetString(newDatabaseName);
		String status = MOVESEngine.isOutputDatabaseNameValid(dbSelection);
		if(status == null) {
			int createStatus = dbSelection.safeCreateDatabase("database/CreateOutput.sql");
			if(DatabaseSelection.CREATED == createStatus) {
				// show a success message and add this item to the list,
				// in case the user hits the droplist button
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Output Database successfully created.", 60));
				newDatabaseName = addIfNotInComboBox(newDatabaseName);
				databaseCombo.setSelectedItem(newDatabaseName);
			} else if(DatabaseSelection.EXISTS == createStatus) {
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Output Database already exists.", 60));
				newDatabaseName = addIfNotInComboBox(newDatabaseName);
				databaseCombo.setSelectedItem(newDatabaseName);
			} else {
				Logger.log(LogMessageCategory.ERROR,
						"Could not create the Output Database.");
			}
		} else {
			JOptionPane.showMessageDialog(this, status);
		}
	}

	/**
	 * Handles the focus lost event for the server textfield.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		if(previousServer.equals(server.getText())) {
			return;
		}
		previousServer = server.getText();
		loadDatabases();
		MOVESNavigation.singleton.updateRunSpecSectionStatus();
	}

	/**
	 * Currently not used.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
	}

	/**
	 * Saves the server and database names to a RunSpec.
	 * @param	runspec The RunSpec to set the info.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		runspec.outputDatabase.serverName = server.getText();
		runspec.outputDatabase.databaseName = databaseCombo.getSelectedItem().toString();
		DatabaseConnectionManager.setOutputDatabase(runspec.outputDatabase.serverName, runspec.outputDatabase.databaseName);

		runspec.outputFactors.massMeasurementSystem =
				(MassMeasurementSystem)massUnitsCombo.getSelectedItem();
		runspec.outputFactors.energyMeasurementSystem =
				(EnergyMeasurementSystem)energyUnitsCombo.getSelectedItem();
		runspec.outputFactors.distanceMeasurementSystem =
				(DistanceMeasurementSystem)distanceUnitsCombo.getSelectedItem();

		if(CompilationFlags.DO_RATES_FIRST) {
			if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) { // If making Rates outputs...
				runspec.outputVMTData = runspec.usesEvapRates();
				runspec.outputPopulation = true;
				runspec.outputSHO = false;
				runspec.outputSH = false;
				runspec.outputSHP = false;
				runspec.outputSHIdling = true;
				runspec.outputStarts = true;
			} else { // If making Inventory outputs...
				runspec.outputVMTData = VMT.isSelected() && VMT.isEnabled();
				runspec.outputPopulation = Population.isSelected();
				runspec.outputSHO = SHO.isSelected();
				runspec.outputSH = SH.isSelected();
				runspec.outputSHP = SHP.isSelected();
				runspec.outputSHIdling = SHIdling.isSelected();
				runspec.outputStarts = Starts.isSelected();
			}
		} else {
			if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
				runspec.outputVMTData = true;
				runspec.outputPopulation = true;
			} else {
				runspec.outputVMTData = VMT.isSelected() && VMT.isEnabled();
				runspec.outputPopulation = Population.isSelected();
			}
			runspec.outputSHO = SHO.isSelected();
			runspec.outputSH = SH.isSelected();
			runspec.outputSHP = SHP.isSelected();
			runspec.outputSHIdling = SHIdling.isSelected();
			runspec.outputStarts = Starts.isSelected();
		}
	}

	/**
	 * Loads the server and database names from a RunSpec.
	 * @param	runspec The RunSpec to get the server and database names from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		showExistingDataWarning(false);
		if(runspec.outputDatabase.serverName != null) {
			server.setText(runspec.outputDatabase.serverName);
		} else {
			server.setText(new String(""));
		}
		// set the saved serverName, will re-init the connection pool
		previousServer = runspec.outputDatabase.serverName;
		loadDatabases();
		databaseCombo.setSelectedItem(runspec.outputDatabase.databaseName);
		massUnitsCombo.setSelectedItem(runspec.outputFactors.massMeasurementSystem);
		energyUnitsCombo.setSelectedItem(runspec.outputFactors.energyMeasurementSystem);
		distanceUnitsCombo.setSelectedItem(runspec.outputFactors.distanceMeasurementSystem);

		if(CompilationFlags.DO_RATES_FIRST) {
			if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) { // If making Rates outputs...
				runspec.outputVMTData = runspec.usesEvapRates();
				VMT.setSelected(runspec.usesEvapRates());
				VMT.setEnabled(false);
				Population.setSelected(true);
				Population.setEnabled(false);
				SHO.setSelected(false);
				SHO.setEnabled(false);
				SH.setSelected(false);
				SH.setEnabled(false);
				SHP.setSelected(false);
				SHP.setEnabled(false);
				SHIdling.setSelected(true);
				SHIdling.setEnabled(false);
				Starts.setSelected(true);
				Starts.setEnabled(false);
			} else { // making Inventory outputs...
				VMT.setSelected(runspec.outputVMTData);
				VMT.setEnabled(true);
				Population.setSelected(runspec.outputPopulation);
				Population.setEnabled(true);
				SHO.setSelected(runspec.outputSHO);
				SHO.setEnabled(true);
				SH.setSelected(runspec.outputSH);
				SH.setEnabled(true);
				SHP.setSelected(runspec.outputSHP);
				SHP.setEnabled(true);
				SHIdling.setSelected(runspec.outputSHIdling);
				SHIdling.setEnabled(true);
				Starts.setSelected(runspec.outputStarts);
				Starts.setEnabled(true);
			}
		} else {
			VMT.setSelected(runspec.outputVMTData);
			VMT.setEnabled(runspec.scale != ModelScale.MESOSCALE_LOOKUP);
			Population.setSelected(runspec.outputPopulation);
			Population.setEnabled(runspec.scale != ModelScale.MESOSCALE_LOOKUP);
			SHO.setSelected(runspec.outputSHO);
			SH.setSelected(runspec.outputSH);
			SHP.setSelected(runspec.outputSHP);
			SHIdling.setSelected(runspec.outputSHIdling);
			Starts.setSelected(runspec.outputStarts);
		}

		adjustGUI(runspec);
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
		if(runspec.outputFactors == null) {
			isOk = false;
		} else if(!runspec.outputFactors.isValid()) {
			isOk = false;
		}
		// try to connect to the runspec's output database
		/* Per EPA Task 130 Section 2.9. Database does not have to exist prior to execution.
		Connection db = openCurrentOutputDatabase(false);
		if(null == db) {
			isOk = false;
		} else {
			DatabaseUtilities.closeConnection(db);
		}
 		*/

 		if(isOk) {
			DatabaseSelection dbSelection = new DatabaseSelection();
			dbSelection.serverName = runspec.outputDatabase.serverName;
			dbSelection.databaseName = runspec.outputDatabase.databaseName;
			isOk = MOVESEngine.isOutputDatabaseNameValid(dbSelection) == null;
 		}

		// Output database name cannot be equal to input database.
		if(isOk) {
			for(Iterator i=runspec.databaseSelectionInputSets.iterator();i.hasNext();) {
				DatabaseSelection userSelection = (DatabaseSelection)i.next();
				if(userSelection.databaseName.
						equalsIgnoreCase(runspec.outputDatabase.databaseName)) {
					isOk = false;
					break;
				}
			}
		}

		// Enforce the requirement that emission rates require VMT and population
		if(isOk && runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			if(CompilationFlags.DO_RATES_FIRST) {
				runspec.outputVMTData = runspec.usesEvapRates();
				isOk = runspec.outputPopulation && runspec.outputSHIdling && runspec.outputStarts;
			} else {
				isOk = runspec.outputVMTData && runspec.outputPopulation;
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
		runspec.outputDatabase.serverName = new String("");
		runspec.outputDatabase.databaseName = new String("");
		DatabaseConnectionManager.setOutputDatabase(runspec.outputDatabase.serverName, runspec.outputDatabase.databaseName);

		runspec.outputFactors = new OutputFactors();

		runspec.outputVMTData = false;
		runspec.outputSHO = false;
		runspec.outputSH = false;
		runspec.outputSHP = false;
		runspec.outputSHIdling = false;
		runspec.outputStarts = false;
		runspec.outputPopulation = false;

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
		adjustGUI(runspec);
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/**
	 * Shows the existing data warning icon, based on the current database selection.
	 * @param	isShow boolean value indicates whether to show the warning image or not
	**/
	void showExistingDataWarning(boolean isShow) {
		if(dataExistsLabel == null) {
			return;
		}
		dataExistsLabel.setVisible(isShow);
		if(isShow) {
			dataExistsImage = new ImageIcon(
					"gov/epa/otaq/moves/master/gui/images/dataExists.gif");
		} else {
			dataExistsImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/blank.gif");
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
		Models.ModelCombination mc = runspec.getModelCombination();
		switch (mc) {
			case M2: // Nonroad
				runspec.outputVMTData = false;
				runspec.outputSHO = false;
				runspec.outputSH = false;
				runspec.outputSHP = false;
				runspec.outputSHIdling = false;
				runspec.outputStarts = false;
				runspec.outputPopulation = false;
				break;
		}
		return null;
	}

	/**
	 * Set GUI items according to the model type (Nonroad and/or Onroad).
	 * @param runspec The simulation description.
	**/
	private void adjustGUI(RunSpec runspec) {
		Models.ModelCombination mc = runspec.getModelCombination();
		switch (mc) {
			case M2: // Nonroad
				activityPanel.setVisible(false);
				VMT.setSelected(false);
				SHO.setSelected(false);
				SH.setSelected(false);
				SHP.setSelected(false);
				SHIdling.setSelected(false);
				Starts.setSelected(false);
				Population.setSelected(false);
				/*
				massUnitsCombo.setSelectedIndex(3);
				energyUnitsCombo.setSelectedIndex(1);
				distanceUnitsCombo.setSelectedIndex(0);
				*/
				break;
			default:
				activityPanel.setVisible(true);
				/*
				massUnitsCombo.setSelectedIndex(-1);
				energyUnitsCombo.setSelectedIndex(-1);
				distanceUnitsCombo.setSelectedIndex(-1);
				*/
				break;
		}
	}
}
