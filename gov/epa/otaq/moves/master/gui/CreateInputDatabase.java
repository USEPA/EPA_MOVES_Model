package gov.epa.otaq.moves.master.gui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToolTip;

import gov.epa.otaq.moves.common.Constants;
import gov.epa.otaq.moves.common.DatabaseSelection;
import gov.epa.otaq.moves.common.DatabaseUtilities;
import gov.epa.otaq.moves.common.ExtendedComboBox;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import gov.epa.otaq.moves.common.ModelDomain;
import gov.epa.otaq.moves.common.Models;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.common.StringUtilities;
import gov.epa.otaq.moves.common.ToolTipHelper;
import gov.epa.otaq.moves.common.TooltipComboBoxRenderer;
import gov.epa.otaq.moves.common.TreeSetIgnoreCase;
import gov.epa.otaq.moves.master.framework.MOVESAPI;
import gov.epa.otaq.moves.master.framework.MOVESEngine;
import gov.epa.otaq.moves.master.framework.SystemConfiguration;
import gov.epa.otaq.moves.master.framework.importers.ImporterManager;
import gov.epa.otaq.moves.master.runspec.RunSpec;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;

/**
 * Class for MOVES CreateInputDatabase panel.
 * Constructs a CreateInputDatabase panel, and creates and sets the layouts of the controls.
 * The Databases combobox is loaded based on the server setting. The Window includes a Server
 * text, a Database combobox, a Database Description text, a Create
 * Database button, and a Enter/Edit data button. It
 * Loads/Saves the selections from/to Runspec.
 *
 * @author  	Mike Kender (Task 1903)
 * @author		Mike Kender (Task 2003)
 * @author  	John Covey (Task 2003)
 * @version     2020-08-10
**/
public class CreateInputDatabase extends JPanel implements ActionListener, FocusListener, RunSpecEditor {
	public static CreateInputDatabase singleton = null;
	
	/** Panel contains table. **/
	JPanel topPanel;
	/** Associated importer manager for this GUI **/
	ImporterManager manager;
	/** Create Database button **/
	JButton createDatabaseButton;
	/** Create Database button **/
	JButton enterEditDataButton;
	/** Refresh database button **/
	JButton refreshDatabaseButton;
	/** Server text control. **/
	JTextField server;
	/** description text control. **/
	JTextField description;
	/** Database combo control. **/
	ExtendedComboBox<String> databaseCombo;
	/** Run droplist ToolTip **/
	JToolTip databaseToolTip;

	/**
	 * Used by the server FocusLost event handler to help determine if the server
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the server TextField losing focus when the server name hasn't changed.
	**/
	String previousServer = "localhost";
	/** database names that should not be used **/
	TreeSetIgnoreCase invalidDatabaseNames = new TreeSetIgnoreCase();

	/** Image shown on importers with data that is complete **/
	ImageIcon okImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/ok16.gif");
	/** Image shown on importers with data that is incomplete or erroneous **/
	ImageIcon errorImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/Error16.gif");
	/** Image shown on importers with default data **/
	ImageIcon defaultImage = new ImageIcon("gov/epa/otaq/moves/master/gui/images/default16.gif");
	
	boolean inputDatabaseConflictsWithTimeOrCounty = false;

	/**
	 * Constructs a CreateInputDatabase panel, also creates and sets the layout of the controls.
	**/
	public CreateInputDatabase(RunSpec runSpec) {
		singleton = this;
		manager = new ImporterManager(runSpec);
		createControls();
		arrangeControls();
	}

	/**
	 * Generate the Database tab.
	 * @return panel for the Database tab
	**/
	JPanel createDatabasePanel() {
		createDatabaseButton = new JButton("Create Database");
		createDatabaseButton.setMnemonic('c');
		createDatabaseButton.addActionListener(this);
		ToolTipHelper.add(createDatabaseButton, "Create the database on the specified server");
		
		enterEditDataButton = new JButton("Enter/Edit Data");
		Dimension d = enterEditDataButton.getPreferredSize();
		enterEditDataButton.setPreferredSize(new Dimension(150, d.height));
		enterEditDataButton.setMnemonic('d');
		enterEditDataButton.addActionListener(this);
		ToolTipHelper.add(enterEditDataButton, "Open the MOVES Data Importer");

		refreshDatabaseButton = new JButton("Refresh");
		refreshDatabaseButton.setMnemonic('r');
		refreshDatabaseButton.addActionListener(this);
		ToolTipHelper.add(refreshDatabaseButton, "Refresh the database list from the specified server");
		
		server = new JTextField(10);
		server.setText("localhost");
		ToolTipHelper.add(server,
				"Edit the name of the server where the database will be located");
		server.setName("server");
		server.addFocusListener(this);
		server.setColumns(10);
		
		description = new JTextField();
		description.setName("description");
		description.addFocusListener(this);

		databaseCombo = new ExtendedComboBox<String>();
		d = databaseCombo.getPreferredSize();
		databaseCombo.setPreferredSize(new Dimension(250, d.height)); // 250
		databaseCombo.setPopupWidth(databaseCombo.getPreferredSize().width);
		databaseCombo.setName("databaseCombo");
		databaseCombo.addActionListener(this);
		databaseCombo.setEditable(true);
		databaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(databaseCombo,
				"Edit or select the name of the database in which the data will be stored");

		JPanel panel = new JPanel();
		panel.setBorder(BorderFactory.createTitledBorder("Domain Input Database"));
		panel.setLayout(new BoxLayout(panel,BoxLayout.Y_AXIS));

		JPanel panel2;
		JLabel label6;
		JLabel label4;
		JLabel label5;

		panel2 = new JPanel();
		label6 = new JLabel();
		label4 = new JLabel();
		label5 = new JLabel();

		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		//======== panel2 ========
		{
			panel2.setLayout(new GridBagLayout());
			((GridBagLayout)panel2.getLayout()).columnWidths = new int[] {38, 73, 57, 0, 0, 0, 0};
			((GridBagLayout)panel2.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0};
			((GridBagLayout)panel2.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0E-4};
			((GridBagLayout)panel2.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 1.0, 1.0E-4};
			
			int yPos = 0;
			
			//---- label4 ----
			label4.setText("Server:");
			label4.setDisplayedMnemonic('v');
			label4.setLabelFor(server);
			panel2.add(label4, new GridBagConstraints(0, yPos, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- server ----
			panel2.add(server, new GridBagConstraints(1, yPos++, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- label5 ----
			label5.setText("Database:");
			label5.setDisplayedMnemonic('b');
			label5.setLabelFor(databaseCombo);
			panel2.add(label5, new GridBagConstraints(0, yPos, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- databaseCombo ----
			panel2.add(databaseCombo, new GridBagConstraints(1, yPos++, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			
			//---- label6 ----
			label6.setText("Description");
			label6.setDisplayedMnemonic('i');
			label6.setLabelFor(description);
			panel2.add(label6, new GridBagConstraints(0, yPos, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- description ----
			panel2.add(description, new GridBagConstraints(1, yPos++, 2, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 5), 0, 0));

			//---- refreshDatabaseButton ----
			panel2.add(refreshDatabaseButton, new GridBagConstraints(2, yPos++, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			int topInset = 30;
			//---- createDatabaseButton ----
			panel2.add(createDatabaseButton, new GridBagConstraints(1, yPos, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(topInset, 20, 5, 5), 0, 0));
			
			//---- enterEditDataButton ----
			panel2.add(enterEditDataButton, new GridBagConstraints(2, yPos++, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(topInset, 20, 5, 5), 0, 0));
		}
		panel.add(panel2);

		generateListOfInvalidDatabaseNames();
		loadDatabases();

		return panel;
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
				StringUtilities.safeGetString((String)databaseCombo.getSelectedItem()).trim();
		Connection db = dbSelection.databaseName.length() > 0? dbSelection.openConnectionOrNull() : null;
		if(null == db && settleForJustServer) {
			// try again to get a connection, but specify an empty string for the database
			// name, in case the current one is invalid, this will at least allow us to get
			// a connection to the server to get the database list
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
		String sql = "select table_schema as input_dbs from information_schema.tables where table_name = 'auditlog' order by input_dbs";
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
		// remove any databases that aren't the correct type
		ArrayList<String> stringsToRemove = new ArrayList<String>();
		try {
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

		// The database cannot already be in the runSpec as anything other
		// the last user input database
		for(Iterator<DatabaseSelection> i=manager.runSpec.databaseSelectionInputSets.iterator();
				i.hasNext();) {
			DatabaseSelection s = (DatabaseSelection)i.next();
			if(i.hasNext()) { // If not the last selection, record it.
				invalidDatabaseNames.add(s.databaseName);
			}
		}
	}

	/** Creates and initializes all controls on this panel. **/
	void createControls() {
		topPanel = createDatabasePanel();
	}

	/**
	 * Sets the layout of the controls.
	 * @return the container as JPanel of the controls
	**/
	void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 3;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "CENTER", 2, 1);
		add(topPanel);
	}

	/**
	 * Handles loading the screen.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
		
	}

	/**
	 * Handles the focus lost event for the server textfield, and checkboxes.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == createDatabaseButton) {
			processCreateDatabaseButton();
		} else if (e.getSource() == enterEditDataButton) {
			enterEditData();
		} else if (e.getSource() == refreshDatabaseButton) {
			loadDatabases();
		} else if(e.getSource() == databaseCombo) {
			boolean isOk = processDatabaseComboChange();
		}
		
		// update status icon with all actions (and recheck input database)
		MOVESNavigation.singleton.parent.domainImporterNetStatus = null;
		MOVESNavigation.singleton.updateCreateInputDatabaseIcon();
	}

	private void enterEditData() {
		if (server.getText().trim().length() == 0) {
			server.setText(previousServer);
			saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());
		}
		ImporterManager.display(MOVESNavigation.singleton.parent,null,
					MOVESNavigation.singleton.parent.getLocationOnScreen().x + 50,
					MOVESNavigation.singleton.parent.getLocationOnScreen().y + 50,
					MOVESAPI.getTheAPI().getRunSpec().domain,ImporterManager.STANDARD_MODE,
					Models.evaluateModels(MOVESAPI.getTheAPI().getRunSpec().models));
	}

	/** Handles the create database button. **/
	public void processCreateDatabaseButton() {
		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName.length() == 0) {
			Logger.log(LogMessageCategory.WARNING, "Specify a database name.");
			return;
		}
		else if (!DatabaseUtilities.isDatabaseNameValid(newDatabaseName)) {
			JOptionPane.showMessageDialog(this,Constants.DATABASE_NAME_VALIDATION_MESSAGE);
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
			if (!DatabaseUtilities.isServerNameValid(server.getText())) {
				JOptionPane.showMessageDialog(this,Constants.SERVER_NAME_VALIDATION_MESSAGE);
				return;		
			}
		} 
		else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)databaseCombo.getSelectedItem());
		if(validDatabaseName(dbSelection)) {
			int createStatus = ImporterManager.singleton.createDatabase(dbSelection);
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

	@Override
	public void saveToRunSpec(RunSpec runspec) {
		if(runspec.scaleInputDatabase == null) {
			runspec.scaleInputDatabase = new DatabaseSelection();
		}
		runspec.scaleInputDatabase.serverName = server.getText();
		runspec.scaleInputDatabase.databaseName = databaseCombo.getSelectedItem().toString();
		runspec.scaleInputDatabase.description = description.getText();
	}

	@Override
	public void loadFromRunSpec(RunSpec runspec) {
		if(runspec.scaleInputDatabase != null) {
			server.setText(runspec.scaleInputDatabase.serverName);
			databaseCombo.setSelectedItem(runspec.scaleInputDatabase.databaseName);
			description.setText(runspec.scaleInputDatabase.description);
		} else {
			server.setText("localhost");
			databaseCombo.setSelectedItem("");
			description.setText("");
		}
		calculateRunSpecSectionStatus(runspec, null);
		MOVESNavigation.singleton.updateRunSpecSectionStatus();
	}

	@Override
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runSpec,
			TreeMap<String, RunSpecSectionStatus> sections) {
		return getStatus(runSpec, sections);
	}

	@Override
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec, TreeMap<String, RunSpecSectionStatus> sections) {
		databaseCombo.setSelectedItem("");
		server.setText("");
		description.setText("");
		runspec.scaleInputDatabase = new DatabaseSelection();
		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.DEFAULTS);
		sections.put(getName(),status);
		return status;
	}

	@Override
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		String printText = "Scale Input Database: ";
		if (runspec.scaleInputDatabase != null) {
			if (runspec.scaleInputDatabase.databaseName != null && runspec.scaleInputDatabase.databaseName.length() > 0) {
				printText += runspec.scaleInputDatabase.databaseName;
			} else {
				printText += "[none]";
			}
		} else {
			printText += "[none]";
		}
		printText += "\r\n";
		
		if (runspec.scaleInputDatabase != null) {
			if (runspec.scaleInputDatabase.description != null && runspec.scaleInputDatabase.description.length() > 0) {
				printText += "Scale Input Database Description: " + runspec.scaleInputDatabase.description + "\r\n";
			}
		}
		printText += "\r\n";
		destination.append(printText);
	}

	private RunSpecSectionStatus getStatus(RunSpec runSpec,
			TreeMap<String, RunSpecSectionStatus> sections) {
		RunSpecSectionStatus status = null;
		boolean isNonroad = Models.evaluateModels(runSpec.models) == Models.ModelCombination.M2;
		boolean isNationalDomain = runSpec.domain == ModelDomain.NATIONAL_ALLOCATION;
		
		if(isNationalDomain || isNonroad) {
			// if database controls are filled out
			if(runSpec.scaleInputDatabase != null 
					&& (runSpec.scaleInputDatabase.serverName != null && runSpec.scaleInputDatabase.serverName.length() > 0)
					&& (runSpec.scaleInputDatabase.databaseName != null && runSpec.scaleInputDatabase.databaseName.length() > 0) ) {
				// status icon depends on if all tabs are green checks
				if(MOVESNavigation.singleton.parent.domainImporterNetStatus != null
					&& MOVESNavigation.singleton.parent.domainImporterNetStatus.status == RunSpecSectionStatus.OK) {
					status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
				} else {
					status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
				}
			} else { // if database controls are not filled out
				status = new RunSpecSectionStatus(RunSpecSectionStatus.DEFAULTS);
			}
		} else {
			// if database controls are filled out
			if(runSpec.scaleInputDatabase != null 
					&& (runSpec.scaleInputDatabase.serverName != null && runSpec.scaleInputDatabase.serverName.length() > 0)
					&& (runSpec.scaleInputDatabase.databaseName != null && runSpec.scaleInputDatabase.databaseName.length() > 0)
					&& !inputDatabaseConflictsWithTimeOrCounty) {
				// status icon depends on if all tabs are green checks
				if(MOVESNavigation.singleton.parent.domainImporterNetStatus != null
					&& MOVESNavigation.singleton.parent.domainImporterNetStatus.status == RunSpecSectionStatus.OK) {
					status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
				} else {
					status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
				}
			} else { // if database controls are not filled out
				status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			}
		}
		
		// save status for other parts of the GUI to reference if needed
		if(sections != null) {
			sections.remove(getName());
			sections.put(getName(),status);	
		}
		
		return status;
	}

	@Override
	public RunSpecSectionStatus onScaleChange(RunSpec runSpec, TreeMap<String, RunSpecSectionStatus> sections) {
		return getStatus(runSpec, sections);
	}

	@Override
	public RunSpecSectionStatus onModelChange(RunSpec runSpec, TreeMap<String, RunSpecSectionStatus> sections) {
		return getStatus(runSpec, sections);
	}
	
	/**
	 * Handles the database combo change.
	 * @return false if the selected database is not valid, or not yet selected.
	**/
	public boolean processDatabaseComboChange() {
		if(databaseCombo.getSelectedItem() == null) {
			return false;
		}
		
		// Save status to RunSpec (even if selection is not valid)
		saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());
		
		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			return false;
		}

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
				return false;
			}
			try {
				inputDatabaseConflictsWithTimeOrCounty = false;
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
					t += "\r\n\r\nKeep input database (Yes), or discard input database (No)?";
					inputDatabaseConflictsWithTimeOrCounty = true;
					if(JOptionPane.showConfirmDialog(this, t, "Error",
							JOptionPane.ERROR_MESSAGE + JOptionPane.YES_NO_OPTION)
							== JOptionPane.YES_OPTION) {
						result = 0;
					}
					if(result < 0) {
						databaseCombo.setSelectedItem("");
						saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());
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
		if(status != null) {
			JOptionPane.showMessageDialog(this, status);
		}
		return status == null;
	}
	
}
