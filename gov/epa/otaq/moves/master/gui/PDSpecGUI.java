/**************************************************************************************************
 * @(#)PDSpecGUI.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import java.sql.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.border.*;
import java.io.*;
import java.util.*;
import java.text.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.*;

/**
 * GUI for creating and PDSpec XML files.
 *
 * @author		wfaler
 * @author  	John Covey (Task 1903)
 * @author		Mike Kender (Task 2003)
 * @author  	John Covey (Task 2003)
 * @version		2020-08-10
**/
public class PDSpecGUI extends JDialog implements ActionListener, FocusListener {
	/** The parent JFrame which invokes this GUI. **/
	JFrame frame;
	/** PDSpec being edited **/
	PDSpec pdSpec = new PDSpec();
	/** true if pdSpec should be executed **/
	boolean shouldExecutePDSpec = false;

	/** holder for all controls **/
	PDSpecGUIPanel controls;
	/**
	 * Used by the server FocusLost event handler to help determine if the server
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the server TextField losing focus when the server name hasn't changed.
	**/
	String previousServer = new String();

	/** false if data has changed and needs to be saved **/
	boolean isSaved = true;

	/**
	 * Constructor.
	 * @param parent the parent frame to use for the panel.
	**/
	public PDSpecGUI(JFrame parent) {
		super(parent, "PDSpec Editor");
		frame = parent;

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(true);
	}

	/** Allows the parent to display this dialog as modal. **/
	/**
	 * Execute the PDSpec user interface.  If the UI calls for execution of a
	 * PDSpec, the UI is closed and the PDSpec returned.
	 * @return a PDSpec to be executed if the user elected to do so, null otherwise.
	**/
	public PDSpec showModal() {
		populateControls();
		pack();
		setModal(true);
		//(new WindowStateHandler(this)).setSizePositionAndStartTracking(800,600);
		(new WindowStateHandler(this)).setPositionAndStartTracking();
		setVisible(true);
		return shouldExecutePDSpec? pdSpec : null;
	}

	/** Fill controls with relevant default values **/
	void populateControls() {
		loadDatabases();
	}

	/**
	 * Creates and arranges all dialog controls.
	 * @return the container as JPanel.
	**/
	JPanel createPanel() {
		controls = new PDSpecGUIPanel();

		// Connect event handlers
		controls.browseButton.addActionListener(this);
		controls.useButton.addActionListener(this);
		controls.createDatabaseButton.addActionListener(this);
		controls.refreshDatabaseButton.addActionListener(this);
		controls.removeButton.addActionListener(this);
		controls.saveButton.addActionListener(this);
		controls.loadButton.addActionListener(this);
		controls.doneButton.addActionListener(this);
		controls.processButton.addActionListener(this);
		controls.databaseCombo.addActionListener(this);

		controls.server.addFocusListener(this);

		return controls;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == controls.browseButton) {
			handleBrowseButton();
		} else if(e.getSource() == controls.useButton) {
			handleUseButton();
		} else if(e.getSource() == controls.createDatabaseButton) {
			handleCreateDatabaseButton();
		} else if(e.getSource() == controls.refreshDatabaseButton) {
			handleRefreshDatabaseButton();
		} else if(e.getSource() == controls.removeButton) {
			handleRemoveButton();
		} else if(e.getSource() == controls.saveButton) {
			handleSaveButton();
		} else if(e.getSource() == controls.loadButton) {
			handleLoadButton();
		} else if(e.getSource() == controls.doneButton) {
			handleDoneButton();
		} else if(e.getSource() == controls.processButton) {
			handleProcessButton();
		} else if(e.getSource() == controls.databaseCombo) {
			processDatabaseComboChange();
		}
	}

	/** Process the Browse button **/
	void handleBrowseButton() {
		controls.masterIDLabel.setText("");
		controls.folderLabel.setText("");

		boolean done = false;
		while(!done) {
			done = true;

			try {
				// Prompt to open an existing DONE file
				FileDialog fd = new FileDialog(this, "Select DONE File", FileDialog.LOAD);
				fd.setVisible(true);
				if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
					return;
				}
				String filePath = fd.getDirectory() + fd.getFile();
				File selectedFile = new File(filePath);
				if(!selectedFile.exists()) {
					return;
				}

				// Verify the chosen file is a DONE file, continuing to prompt if it is not, obtaining a MasterDetails otherwise.
				EmissionCalculatorInboundUnbundler.MasterDetails masterDetails = null;
				try {
					masterDetails = EmissionCalculatorInboundUnbundler.getDetails(selectedFile);
				} catch(Exception e) {
					// Nothing to do here
				}
				if(masterDetails == null || masterDetails.fullManifest == null || masterDetails.fullManifest.destinationMasterIDNumber.length() <= 0) {
					// Complain that the file is not a usable DONE file
					Logger.log(LogMessageCategory.WARNING,selectedFile.getName() + " is not a usable DONE file.");
					continue;
				}

				// Display the MasterDetails.BundleManifest information, giving the user the option to rejecting the DONE file.
				PDSpecDetailGUI detailDialog = new PDSpecDetailGUI(this);
				if(!detailDialog.showModal(masterDetails.fullManifest,selectedFile)) {
					return;
				}

				// If accepted, then populate the controls.
				controls.masterIDLabel.setText(masterDetails.fullManifest.destinationMasterIDNumber);
				controls.folderLabel.setText(selectedFile.getParentFile().getCanonicalPath());
				return;
			} catch(Exception e) {
				Logger.logError(e,"Unable to browse for DONE files");
				break;
			}
		}
	}

	/** Process the Use button **/
	void handleUseButton() {
		// Make a new entry from the details on screen
		PDSpec.PDSpecEntry entry = new PDSpec.PDSpecEntry();
		entry.masterID = controls.masterIDLabel.getText();
		entry.pickupFolderName = controls.folderLabel.getText();
		entry.outputDatabase.serverName = controls.server.getText();
		entry.outputDatabase.databaseName = controls.databaseCombo.getSelectedItem().toString().trim();

		// Complain if anything is missing
		if(entry.masterID == null || entry.masterID.length() <= 0) {
			Logger.log(LogMessageCategory.WARNING,"Please provide as Master ID by browsing for a DONE file.");
			return;
		}
		if(entry.pickupFolderName == null || entry.pickupFolderName.length() <= 0) {
			Logger.log(LogMessageCategory.WARNING,"Please provide a pickup folder by browsing for a DONE file.");
			return;
		}
		if(entry.outputDatabase.databaseName == null || entry.outputDatabase.databaseName.length() <= 0) {
			Logger.log(LogMessageCategory.WARNING,"Please provide a database name.");
			return;
		}

		// Add the new entry to only the onscreen list
		controls.selectionsListModel.addElement(entry);

		// Mark the data as needing to be saved
		isSaved = false;

		// Clear the onscreen master ID and folder entries, leaving the database
		controls.masterIDLabel.setText("");
		controls.folderLabel.setText("");
	}

	/** Process the Create Database button **/
	void handleCreateDatabaseButton() {
		String newDatabaseName = controls.databaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			Logger.log(LogMessageCategory.WARNING, "Specify a database name.");
			return;
		}
		else if (!DatabaseUtilities.isDatabaseNameValid(newDatabaseName)) {
			JOptionPane.showMessageDialog(this,Constants.DATABASE_NAME_VALIDATION_MESSAGE);
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(controls.server.getText().length() > 0) {
			dbSelection.serverName = controls.server.getText();
			if (!DatabaseUtilities.isServerNameValid(controls.server.getText())) {
				JOptionPane.showMessageDialog(this,Constants.SERVER_NAME_VALIDATION_MESSAGE);
				return;		
			}
		} 
		else {
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
				controls.databaseCombo.setSelectedItem(newDatabaseName);
			} else if(DatabaseSelection.EXISTS == createStatus) {
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Output Database already exists.", 60));
				newDatabaseName = addIfNotInComboBox(newDatabaseName);
				controls.databaseCombo.setSelectedItem(newDatabaseName);
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
		if(previousServer.equals(controls.server.getText())) {
			return;
		}
		previousServer = controls.server.getText();
		loadDatabases();
	}

	/**
	 * Currently not used.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
	}

	/** Process the Refresh button **/
	void handleRefreshDatabaseButton() {
		loadDatabases();
	}

	/** Process the Remove button **/
	void handleRemoveButton() {
		Object[] selectedItems = controls.selectionsList.getSelectedValuesList().toArray();
		for(int i=0;i<selectedItems.length;i++) {
			controls.selectionsListModel.removeElement(selectedItems[i]);
			// Mark the data as needing to be saved
			isSaved = false;
		}
	}

	/**
	 * Process the Save button
	 * @return true if the file was saved
	**/
	boolean handleSaveButton() {
		fillPDSpec();
		if(pdSpec.entries.size() <= 0) {
			// Complain that there is nothing to save
			Logger.log(LogMessageCategory.WARNING,"There are no selections to save.");
			return true;
		}

		// Prompt for a file to write into
		FileDialog fd = new FileDialog(this, "Save PDSpec XML", FileDialog.SAVE);
		fd.setVisible(true);
		if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return false;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File selectedFile = new File(filePath);

		// Write pdSpec to the file
		try {
			pdSpec.writeXML(selectedFile);
			isSaved = true;
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to create PDSpec XML file " + selectedFile.getName());
		}
		return false;
	}

	/** Process the Load button **/
	void handleLoadButton() {
		fillPDSpec();
		if(pdSpec.entries.size() > 0 && !isSaved) {
			// Alert that data is not saved and ask about saving
			int answer = JOptionPane.showConfirmDialog(this,
					"Do you want to save your selections first?",
					"Save?",JOptionPane.YES_NO_CANCEL_OPTION);
			if(answer == JOptionPane.YES_OPTION) {
				if(!handleSaveButton()) {
					return;
				}
			} else if(answer == JOptionPane.CANCEL_OPTION) {
				return;
			}
		}

		// Prompt to open an existing PDSpec XML file
		FileDialog fd = new FileDialog(this, "Open PDSpec XML", FileDialog.LOAD);
		fd.setVisible(true);
		if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File selectedFile = new File(filePath);
		if(!selectedFile.exists()) {
			return;
		}

		// Load from XML
		try {
			if(!pdSpec.readXML(selectedFile)) {
				// Complain that the XML file was not read
				Logger.log(LogMessageCategory.ERROR,"Unable to read the PDSpec XML file.");
				return;
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to read PDSpec XML file " + selectedFile.getName());
			return;
		}

		// Add to the screen
		controls.selectionsListModel.removeAllElements();
		for(Iterator<PDSpec.PDSpecEntry> i=pdSpec.entries.iterator();i.hasNext();) {
			controls.selectionsListModel.addElement(i.next());
		}

		isSaved = true;
	}

	/** Populate pdSpec from the onscreen entries **/
	void fillPDSpec() {
		pdSpec.entries.clear();
		for(int i=0;i<controls.selectionsListModel.getSize();i++) {
			PDSpec.PDSpecEntry entry = (PDSpec.PDSpecEntry)controls.selectionsListModel.getElementAt(i);
			pdSpec.entries.add(entry);
		}
	}

	/** Process the Done button **/
	void handleDoneButton() {
		fillPDSpec();
		if(pdSpec.entries.size() > 0 && !isSaved) {
			// Alert that data is not saved and ask about saving
			int answer = JOptionPane.showConfirmDialog(this,
					"Do you want to save your selections first?",
					"Save?",JOptionPane.YES_NO_CANCEL_OPTION);
			if(answer == JOptionPane.YES_OPTION) {
				if(!handleSaveButton()) {
					return;
				}
			} else if(answer == JOptionPane.CANCEL_OPTION) {
				return;
			}
		}
		shouldExecutePDSpec = false;
		dispose();
	}

	/** Process the Process button **/
	void handleProcessButton() {
		fillPDSpec();
		if(pdSpec.entries.size() <= 0) {
			// Complain if there is nothing to process
			Logger.log(LogMessageCategory.WARNING,"There are no selections to process.");
			return;
		}

		if(pdSpec.entries.size() > 0 && !isSaved) {
			// Alert that data is not saved and ask about saving
			int answer = JOptionPane.showConfirmDialog(this,
					"Do you want to save your selections first?",
					"Save?",JOptionPane.YES_NO_CANCEL_OPTION);
			if(answer == JOptionPane.YES_OPTION) {
				if(!handleSaveButton()) {
					return;
				}
			} else if(answer == JOptionPane.CANCEL_OPTION) {
				return;
			}
		}

		shouldExecutePDSpec = true;
		dispose();
	}

	/** Handles the database combo change. **/
	public void processDatabaseComboChange() {
		if(controls.databaseCombo.getSelectedItem() == null) {
			return;
		}
		if(controls.databaseCombo.getSelectedItem().toString().length() == 0) {
			return;
		}

		String newDatabaseName = controls.databaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			Logger.log(LogMessageCategory.WARNING, "Specify a database name.");
			return;
		}
		else if (!DatabaseUtilities.isDatabaseNameValid(newDatabaseName)) {
			JOptionPane.showMessageDialog(this,Constants.DATABASE_NAME_VALIDATION_MESSAGE);
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(controls.server.getText().length() > 0) {
			dbSelection.serverName = controls.server.getText();
			if (!DatabaseUtilities.isServerNameValid(controls.server.getText())) {
				JOptionPane.showMessageDialog(this,Constants.SERVER_NAME_VALIDATION_MESSAGE);
				return;		
			}
		} 
		else {
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
				return;
			}
			try {
				sql = "SELECT COUNT(*) FROM MOVESOUTPUT";
				statement = db.prepareStatement(sql);
				ResultSet results = SQLRunner.executeQuery(statement,sql);
				if(results != null) {
					results.next();
					int count = results.getInt(1);
					//showExistingDataWarning(count != 0);
					results.close();
				}
				statement.close();
			} catch(Exception e) {
				//showExistingDataWarning(false);
			}
			DatabaseUtilities.closeConnection(db);
			newDatabaseName = addIfNotInComboBox(newDatabaseName);
			controls.databaseCombo.setSelectedItem(newDatabaseName);
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
		ComboBoxModel model = controls.databaseCombo.getModel();
		for(int i = 0; i < model.getSize(); i++) {
			String t = (String)model.getElementAt(i);
			if(t.equalsIgnoreCase(newDatabaseName)) {
				return t;
			}
		}
		controls.databaseCombo.addItem(newDatabaseName);
		return newDatabaseName;
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
		if(controls.server.getText().length() > 0) {
			dbSelection.serverName = controls.server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.OUTPUT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)controls.databaseCombo.getSelectedItem());
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
		controls.databaseCombo.removeAllItems();
		// add the default item (no selection)
		controls.databaseCombo.addItem(new String(""));
		TreeSet<String> databases = new TreeSet<String>();
		// get the available databases from the current server selection
		Connection db = openCurrentOutputDatabase(true);
		if(null == db) {
			Logger.log(LogMessageCategory.ERROR,"Could not connect to the output database");
			return;
		}
		String sql = "select table_schema as output_dbs from information_schema.tables where table_name = 'movesoutput' order by output_dbs";
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
			Logger.logError(e, "Failed to show databases (load databases) in PDSpecGUI.");
			DatabaseUtilities.closeConnection(db);
			return;
		}
		// second pass through the returned list of database names, must now
		// remove any databases that aren't an output database
		ArrayList<String> stringsToRemove = new ArrayList<String>();
		try {
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
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				String nextDB = (String)i.next();
				controls.databaseCombo.addItem(nextDB);
			}
			Vector<String> toolTipVector = new Vector<String>();
			for(int i=0; i<controls.databaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) controls.databaseCombo.getItemAt(i));
			}
			String[] toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			controls.databaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show tables from database in PDSpecGUI.");
		}
		// set the default selection
		controls.databaseCombo.setSelectedItem("");
		DatabaseUtilities.closeConnection(db);
	}

	class PDSpecDetailGUI extends JDialog implements ActionListener {
		/** holder for all controls **/
		PDSpecDetailPanel controls;
		/** true when the user wants to accept the DONE file being displayed **/
		boolean acceptedFile = false;

		/**
		 * Constructor.
		**/
		public PDSpecDetailGUI(PDSpecGUI parent) {
			super(parent, "DONE File");

			getContentPane().setLayout(new BorderLayout());
			getContentPane().add(createPanel(), BorderLayout.CENTER);
			pack();
			setResizable(true);
		}

		/**
		 * Allows the parent to display this dialog as modal.
		 * @param manifest details from the DONE file
		 * @param doneFile file that was selected and is being examined
		 * @return true if the user accepted the DONE file
		**/
		public boolean showModal(BundleManifest manifest, File doneFile) {
			acceptedFile = false;
			populateControls(manifest,doneFile);
			pack();
			setModal(true);
			(new WindowStateHandler(this)).setPositionAndStartTracking();
			setVisible(true);
			return acceptedFile;
		}

		/**
		 * Fill controls with relevant default values
			 * @param manifest details from the DONE file
		**/
		void populateControls(BundleManifest manifest, File doneFile) {
			controls.fileNameLabel.setText(doneFile.getName());

			String t = "";
			t += "Master ID: " + manifest.destinationMasterIDNumber + "\r\n";
			t += "Master Computer ID: " + manifest.destinationMasterComputerID + "\r\n";
			t += "RunSpec File: " + manifest.masterFragment.runSpecFileName + "\r\n";
			t += "RunSpec Date: " + manifest.masterFragment.runSpecFileDateTime + "\r\n";
			t += "Output: " + manifest.masterFragment.outputDatabaseName + "\r\n";
			t += "Bundles Expected: " + manifest.masterFragment.expectedDONEFiles + "\r\n";
			t += manifest.masterFragment.runSpecDescription;

			controls.textArea1.setText(t);
		}

		/**
		 * Creates and arranges all dialog controls.
		 * @return the container as JPanel.
		**/
		JPanel createPanel() {
			controls = new PDSpecDetailPanel();

			// Connect event handlers
			controls.okButton.addActionListener(this);
			controls.cancelButton.addActionListener(this);

			return controls;
		}

		/**
		 * Calls the appropriate button handler.
		 * @param e the ActionEvent to be handled.
		**/
		public void actionPerformed(ActionEvent e) {
			if(e.getSource() == controls.okButton) {
				acceptedFile = true;
				dispose();
			} else if(e.getSource() == controls.cancelButton) {
				acceptedFile = false;
				dispose();
			}
		}
	}

	public class PDSpecGUIPanel extends JPanel {
		public PDSpecGUIPanel() {
			initComponents();
		}

		private void initComponents() {
			// JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
			panel1 = new JPanel();
			panel7 = new JPanel();
			label1 = new JLabel();
			masterIDLabel = new JLabel();
			label2 = new JLabel();
			folderLabel = new JLabel();
			panel8 = new JPanel();
			label3 = new JLabel();

			server = new JTextField();
			server.setColumns(10);

			label4 = new JLabel();

			databaseCombo = new ExtendedComboBox<String>();
			Dimension d = databaseCombo.getPreferredSize();
			databaseCombo.setPreferredSize(new Dimension(150, d.height));
			databaseCombo.setPopupWidth(databaseCombo.getPreferredSize().width);
			databaseCombo.setName("databaseCombo");
			databaseCombo.setEditable(true);
			databaseCombo.setSelectedIndex(-1);
			ToolTipHelper.add(databaseCombo,"Edit or select the name of the database in which the output will be stored");

			refreshDatabaseButton = new JButton();
			ToolTipHelper.add(refreshDatabaseButton,"Refresh the list of available databases");

			createDatabaseButton = new JButton();
			ToolTipHelper.add(createDatabaseButton,"Create the output database if it does not already exist");

			panel2 = new JPanel();
			browseButton = new JButton();
			useButton = new JButton();
			panel6 = new JPanel();
			scrollPane1 = new JScrollPane();

			selectionsListModel = new DefaultListModel<PDSpec.PDSpecEntry>();
			selectionsList = new JListWithToolTips<PDSpec.PDSpecEntry>(selectionsListModel);

			panel3 = new JPanel();
			removeButton = new JButton();
			saveButton = new JButton();
			loadButton = new JButton();
			panel5 = new JPanel();
			hSpacer1 = new JPanel(null);
			processButton = new JButton();
			panel4 = new JPanel();
			doneButton = new JButton();
			hSpacer2 = new JPanel(null);

			//======== this ========
			setLayout(new GridBagLayout());
/*
			((GridBagLayout)getLayout()).columnWidths = new int[] {0, 0, 0};
			((GridBagLayout)getLayout()).rowHeights = new int[] {0, 0, 0, 0};
			((GridBagLayout)getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0E-4};
			((GridBagLayout)getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};
*/
			((GridBagLayout)getLayout()).columnWidths = new int[] {0, 0};
			((GridBagLayout)getLayout()).rowHeights = new int[] {0, 0, 0};
			((GridBagLayout)getLayout()).columnWeights = new double[] {1.0E-4, 0.0};
			((GridBagLayout)getLayout()).rowWeights = new double[] {0.0, 1.0E-4, 0.0};

			//======== panel1 ========
			{
				panel1.setBorder(new TitledBorder("Details"));
				panel1.setLayout(new BoxLayout(panel1, BoxLayout.Y_AXIS));

				//======== panel7 ========
				{
					panel7.setBorder(new TitledBorder("DONE Files"));
					panel7.setLayout(new GridBagLayout());
					((GridBagLayout)panel7.getLayout()).columnWidths = new int[] {0, 0, 0, 0};
					((GridBagLayout)panel7.getLayout()).rowHeights = new int[] {0, 0, 0};
					((GridBagLayout)panel7.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};
					((GridBagLayout)panel7.getLayout()).rowWeights = new double[] {0.0, 0.0, 1.0E-4};

					//---- label1 ----
					label1.setText("Master ID:");
					panel7.add(label1, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 5), 0, 0));
					panel7.add(masterIDLabel, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 0), 0, 0));

					//---- label2 ----
					label2.setText("Folder:");
					panel7.add(label2, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 0, 5), 0, 0));
					panel7.add(folderLabel, new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 0, 0), 0, 0));
				}
				panel1.add(panel7);

				//======== panel8 ========
				{
					panel8.setBorder(new TitledBorder("Output Database"));
					panel8.setLayout(new GridBagLayout());
					((GridBagLayout)panel8.getLayout()).columnWidths = new int[] {0, 0, 0, 0, 0, 0};
					((GridBagLayout)panel8.getLayout()).rowHeights = new int[] {0, 0, 0};
					((GridBagLayout)panel8.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4};
					((GridBagLayout)panel8.getLayout()).rowWeights = new double[] {0.0, 0.0, 1.0E-4};

					//---- label3 ----
					label3.setText("Server:");
					panel8.add(label3, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 5), 0, 0));

					//---- server ----
					server.setColumns(10);
					panel8.add(server, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 5), 0, 0));

					//---- label4 ----
					label4.setText("Database:");
					panel8.add(label4, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 5), 0, 0));
					panel8.add(databaseCombo, new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 5), 0, 0));

					//---- refreshDatabaseButton ----
					refreshDatabaseButton.setText("Refresh");
					panel8.add(refreshDatabaseButton, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 0), 0, 0));

					//---- createDatabaseButton ----
					createDatabaseButton.setText("Create Database");
					panel8.add(createDatabaseButton, new GridBagConstraints(3, 1, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 0, 5), 0, 0));
				}
				panel1.add(panel8);
			}
			add(panel1, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//======== panel2 ========
			{
				panel2.setLayout(new GridBagLayout());
/*
				((GridBagLayout)panel2.getLayout()).columnWidths = new int[] {0, 0};
				((GridBagLayout)panel2.getLayout()).rowHeights = new int[] {0, 0, 0, 0};
				((GridBagLayout)panel2.getLayout()).columnWeights = new double[] {0.0, 1.0E-4};
				((GridBagLayout)panel2.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};
*/
				((GridBagLayout)panel2.getLayout()).columnWidths = new int[] {0};
				((GridBagLayout)panel2.getLayout()).rowHeights = new int[] {0, 0, 0, 0};
				((GridBagLayout)panel2.getLayout()).columnWeights = new double[] {1.0E-4};
				((GridBagLayout)panel2.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};

				//---- browseButton ----
				browseButton.setText("Browse DONE Files...");
				panel2.add(browseButton, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));
				ToolTipHelper.add(browseButton, "Open File Explorer to load DONE files");

				//---- useButton ----
				useButton.setText("Use");
				panel2.add(useButton, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));
				ToolTipHelper.add(useButton, "Select the given Master ID");
			}
			add(panel2, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//======== panel6 ========
			{
				panel6.setBorder(new TitledBorder("Selections"));
				panel6.setLayout(new BorderLayout());

				//======== scrollPane1 ========
				{
					//---- selectionsList ----
					selectionsList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
					selectionsList.setPrototypeCellValue(new PDSpec.PDSpecEntry() { public String toString() { return "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"; }});
					scrollPane1.setViewportView(selectionsList);
				}
				panel6.add(scrollPane1, BorderLayout.CENTER);
			}
			add(panel6, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//======== panel3 ========
			{
				panel3.setLayout(new GridBagLayout());
/*
				((GridBagLayout)panel3.getLayout()).columnWidths = new int[] {0, 0};
				((GridBagLayout)panel3.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0};
				((GridBagLayout)panel3.getLayout()).columnWeights = new double[] {0.0, 1.0E-4};
				((GridBagLayout)panel3.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 1.0E-4};
*/
				((GridBagLayout)panel3.getLayout()).columnWidths = new int[] {0};
				((GridBagLayout)panel3.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0};
				((GridBagLayout)panel3.getLayout()).columnWeights = new double[] {1.0E-4};
				((GridBagLayout)panel3.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 1.0E-4};

				//---- removeButton ----
				removeButton.setText("Remove");
				panel3.add(removeButton, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));
				ToolTipHelper.add(removeButton, "Remove the highlighted selection from the list");

				//---- saveButton ----
				saveButton.setText("Save...");
				panel3.add(saveButton, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));
				ToolTipHelper.add(saveButton, "Save the PDSpec to disk");

				//---- loadButton ----
				loadButton.setText("Load...");
				panel3.add(loadButton, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));
				ToolTipHelper.add(loadButton, "Load a PDSpec from disk");
			}
			add(panel3, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//======== panel5 ========
			{
				panel5.setLayout(new GridBagLayout());
				((GridBagLayout)panel5.getLayout()).columnWidths = new int[] {0, 0};
				((GridBagLayout)panel5.getLayout()).rowHeights = new int[] {0, 0};
				((GridBagLayout)panel5.getLayout()).columnWeights = new double[] {1.0E-4, 0.0};
				((GridBagLayout)panel5.getLayout()).rowWeights = new double[] {0.0, 1.0E-4};
				panel5.add(hSpacer1, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 0, 5), 0, 0));

				//---- processButton ----
				processButton.setText("Initiate Processing");
				panel5.add(processButton, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 0, 0), 0, 0));
				ToolTipHelper.add(processButton, "Start processing the PDSpec");

				panel5.add(hSpacer2, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 0, 0), 0, 0));
			}
			add(panel5, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 5), 0, 0));

			//======== panel4 ========
			{
				panel4.setLayout(new GridBagLayout());
/*
				((GridBagLayout)panel4.getLayout()).columnWidths = new int[] {0, 0, 0};
				((GridBagLayout)panel4.getLayout()).rowHeights = new int[] {0, 0};
				((GridBagLayout)panel4.getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0E-4};
				((GridBagLayout)panel4.getLayout()).rowWeights = new double[] {0.0, 1.0E-4};
*/
				((GridBagLayout)panel4.getLayout()).columnWidths = new int[] {0};
				((GridBagLayout)panel4.getLayout()).rowHeights = new int[] {0, 0};
				((GridBagLayout)panel4.getLayout()).columnWeights = new double[] {1.0E-4};
				((GridBagLayout)panel4.getLayout()).rowWeights = new double[] {0.0, 1.0E-4};

				//---- doneButton ----
				doneButton.setText("Done");
				panel4.add(doneButton, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 0, 5), 0, 0));
				ToolTipHelper.add(doneButton, "Close the dialog");
/*
				panel4.add(hSpacer2, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 0, 0), 0, 0));
*/
			}
			add(panel4, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 0), 0, 0));
			// JFormDesigner - End of component initialization  //GEN-END:initComponents
		}

		// JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
		private JPanel panel1;
		private JPanel panel7;
		private JLabel label1;
		public JLabel masterIDLabel;
		private JLabel label2;
		public JLabel folderLabel;
		private JPanel panel8;
		private JLabel label3;
		public JTextField server;
		private JLabel label4;
		public ExtendedComboBox<String> databaseCombo;
		public JButton refreshDatabaseButton;
		public JButton createDatabaseButton;
		private JPanel panel2;
		public JButton browseButton;
		public JButton useButton;
		private JPanel panel6;
		private JScrollPane scrollPane1;
		public JList<PDSpec.PDSpecEntry> selectionsList;
		private JPanel panel3;
		public JButton removeButton;
		public JButton saveButton;
		public JButton loadButton;
		private JPanel panel5;
		private JPanel hSpacer1;
		public JButton processButton;
		private JPanel panel4;
		public JButton doneButton;
		private JPanel hSpacer2;
		// JFormDesigner - End of variables declaration  //GEN-END:variables
		public DefaultListModel<PDSpec.PDSpecEntry> selectionsListModel;
	}

	public class PDSpecDetailPanel extends JPanel {
		public PDSpecDetailPanel() {
			initComponents();
		}

		private void initComponents() {
			// JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
			label1 = new JLabel();
			fileNameLabel = new JLabel();
			okButton = new JButton();
			label2 = new JLabel();
			cancelButton = new JButton();
			scrollPane1 = new JScrollPane();
			textArea1 = new JTextArea();

			//======== this ========
			setLayout(new GridBagLayout());
			((GridBagLayout)getLayout()).columnWidths = new int[] {0, 0, 0, 0};
			((GridBagLayout)getLayout()).rowHeights = new int[] {0, 0, 0, 0};
			((GridBagLayout)getLayout()).columnWeights = new double[] {0.0, 1.0E-4, 0.0, 0.0};
			((GridBagLayout)getLayout()).rowWeights = new double[] {0.0, 0.0, 1.0E-4, 0.0};

			//---- label1 ----
			label1.setText("File:");
			add(label1, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			add(fileNameLabel, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- okButton ----
			okButton.setText("OK");
			add(okButton, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			ToolTipHelper.add(okButton, "Close the dialog");

			//---- label2 ----
			label2.setText("Details:");
			add(label2, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- cancelButton ----
			cancelButton.setText("Cancel");
			add(cancelButton, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			ToolTipHelper.add(cancelButton, "Close the dialog");

			//======== scrollPane1 ========
			{
				//---- textArea1 ----
				textArea1.setEditable(false);
				textArea1.setFont(UIManager.getFont("EditorPane.font"));
				textArea1.setBackground(UIManager.getColor("EditorPane.disabledBackground"));
				scrollPane1.setViewportView(textArea1);
			}
			add(scrollPane1, new GridBagConstraints(0, 2, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 0), 0, 0));
			// JFormDesigner - End of component initialization  //GEN-END:initComponents
		}

		// JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
		private JLabel label1;
		public JLabel fileNameLabel;
		public JButton okButton;
		private JLabel label2;
		public JButton cancelButton;
		private JScrollPane scrollPane1;
		public JTextArea textArea1;
		// JFormDesigner - End of variables declaration  //GEN-END:variables
	}
}
