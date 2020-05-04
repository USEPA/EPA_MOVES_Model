/**************************************************************************************************
 * @(#)Configure.java
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
import javax.swing.filechooser.FileFilter;
import java.io.*;
import java.util.TreeMap;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

/**
 * Class for MOVES Configure panel.
 *
 * Creates, initializes, arranges, and sets the layouts of all the dialog controls on the panel.
 * Allows the parent to display this dialog as modal.  Creates a panel containing the OK and Cancel
 * button. Calls the appropriate button handler, the OK button will save the choices before closing,
 * and the Cancel button will reset the database connection to its original values.  Handles the
 * Path Browse button.  Loads the databases drop list.
 *
 * @author		Wesley Faler
 * @author		Tim Hull
 * @version		2014-01-11
**/
public class Configure extends JDialog implements ActionListener,
		FocusListener {
	/** The dialog result, indicates true on OK button. **/
	public int result = 0;
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** OK button. **/
	JButton okButton;
	/** Cancel button. **/
	JButton cancelButton;

	/** County panel. **/
	JPanel countyPanel;
	/** Output panel. **/
	JPanel outputPanel;
	/** Run panel. **/
	JPanel sharedFolderPathPanel;

	/** County Server label. **/
	JLabel countyServerLabel;
	/** County Server text control. **/
	JTextField countyServer;
	/** County Database label. **/
	JLabel countyDatabaseLabel;
	/** County Database combo control. **/
	ExtendedComboBox<String> countyDatabaseCombo;
	
	// /** NonRoad Database label. **/
	// JLabel nonroadDatabaseLabel;
	// /** NonRoad Database combo control. **/
	// ExtendedComboBox<String> nonroadDatabaseCombo;

	/** Output Server label. **/
	JLabel outputServerLabel;
	/** Output Server text control. **/
	JTextField outputServer;

	/** Run commands text control. **/
	JTextField sharedFolderPathName;
	/** Run commands browse button. **/
	JButton sharedFolderPathBrowse;

	/**
	 * Used by the countyServer FocusLost event handler to help determine if the countyServer
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the countyServer TextField losing focus when the countyServer name
	 * hasn't changed..
	**/
	String countyPreviousServer;

	/**
	 * Used by the outputServer FocusLost event handler to help determine if the outputServer
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the outputServer TextField losing focus when the outputServer name
	 * hasn't changed..
	**/
	String outputPreviousServer;

	/**
	 * Constructs an Configure panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	**/
	public Configure(JFrame parent) {
		super(parent, "Configure MOVES");
		frame = parent;

		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(false);
		countyPreviousServer = new String("");
		outputPreviousServer = new String("");
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		pushDataToControls();
		pack();
		setModal(true);
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(-1,-1);
		setVisible(true); //show();
	}

	/** Data from member variables to dialog controls (currently unused). **/
	void pushDataToControls() {
	}

	/**
	 * Creates and arranges all dialog controls.
	 * @return the container as JPanel.
	**/
	JPanel createPanel() {
		createControls();
		return arrangeControls();
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();
		okButton = new JButton("OK");
		okButton.addActionListener(this);
		okButton.setName("okButton");
		ToolTipHelper.add(okButton,"Save the configuration settings and exit");
		cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(this);
		cancelButton.setName("cancelButton");
		ToolTipHelper.add(cancelButton,"Exit without saving changes");
		Dimension countyPanelSize = new Dimension(450,135); // 450,90
		Dimension outputPanelSize = new Dimension(450,55);
		Dimension filePanelSize = new Dimension(450,60);
		countyPanel = new JPanel();
		countyPanel.setName("countyPanel");
		countyPanel.setBorder(BorderFactory.createTitledBorder(
				"Default Input Database"));
		countyPanel.setPreferredSize(countyPanelSize);

		outputPanel = new JPanel();
		outputPanel.setName("outputPanel");
		outputPanel.setBorder(BorderFactory.createTitledBorder(
				"Default Output Database"));
		outputPanel.setPreferredSize(outputPanelSize);

		sharedFolderPathPanel = new JPanel();
		sharedFolderPathPanel.setName("sharedFolderPathPanel");
		sharedFolderPathPanel.setBorder(BorderFactory.createTitledBorder(
				"Shared Distributed Folder Path"));
		sharedFolderPathPanel.setPreferredSize(filePanelSize);

		countyServerLabel = new JLabel("Server:");
		countyServerLabel.setName("countyServerLabel");
		countyServer = new JTextField(20);
		countyServer.setName("countyServer");
		countyServer.addFocusListener(this);
		countyServer.setText(sysConfig.databaseSelections[
				MOVESDatabaseType.DEFAULT.getIndex()].serverName);
		ToolTipHelper.add(countyServer,"Edit the name of the default database server");

		outputServerLabel = new JLabel("Server:");
		outputServerLabel.setName("outputServerLabel");
		outputServer = new JTextField(20);
		outputServer.setName("outputServer");
		outputServer.addFocusListener(this);
		outputServer.setText(sysConfig.databaseSelections[
				MOVESDatabaseType.OUTPUT.getIndex()].serverName);
		ToolTipHelper.add(outputServer,"Edit the name of the output database server");

		countyDatabaseCombo = new ExtendedComboBox<String>();
		countyDatabaseCombo.setName("countyDatabaseCombo");
		countyDatabaseLabel = new JLabel("Default Database:");
		countyDatabaseLabel.setName("countyDatabaseLabel");
		countyDatabaseCombo.setEditable(true);
		countyDatabaseCombo.setSelectedItem
				(sysConfig.databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName);
		ToolTipHelper.add(countyDatabaseCombo,"Edit or select the name of the default database");

		/*
		nonroadDatabaseCombo = new ExtendedComboBox<String>();
		nonroadDatabaseCombo.setName("nonroadDatabaseCombo");
		nonroadDatabaseLabel = new JLabel("NonRoad Database:");
		nonroadDatabaseLabel.setName("nonroadDatabaseLabel");
		if(CompilationFlags.USE_NONROAD) {
			nonroadDatabaseCombo.setEditable(true);
			nonroadDatabaseCombo.setSelectedItem
					(sysConfig.databaseSelections[MOVESDatabaseType.NRDEFAULT.getIndex()].databaseName);
			ToolTipHelper.add(nonroadDatabaseCombo,"Edit or select the name of the NonRoad database");
		} else {
			nonroadDatabaseCombo.setEditable(false);
		}
		*/

		sharedFolderPathName = new JTextField(30);
		sharedFolderPathName.setName("sharedFolderPathName");
		ToolTipHelper.add(sharedFolderPathName,"Edit the folder name for the files shared with the"
				+" workers");
		sharedFolderPathBrowse = new JButton("Browse...");
		ToolTipHelper.add(sharedFolderPathBrowse,"Select folder to share files with workers");
		sharedFolderPathBrowse.addActionListener(this);
		sharedFolderPathBrowse.setName("sharedFolderPathBrowse");
		sharedFolderPathName.setText(sysConfig.sharedDistributedFolderPath.getPath());
	}

	/**
	 * Sets the layout of the controls.
	 * @return the container as JPanel of the controls
	**/
	public JPanel arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		countyPanel.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 3;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		countyPanel.add(countyServerLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
		countyPanel.add(countyServer, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		countyPanel.add(countyDatabaseLabel, gbc);
		gbc.weightx = 1.0;
		LayoutUtility.setPositionOnGrid(gbc,1, 1, "WEST", 1, 1);
		countyPanel.add(countyDatabaseCombo, gbc);
		gbc.weightx = 0;

		/*
		if(CompilationFlags.USE_NONROAD) {
			LayoutUtility.setPositionOnGrid(gbc,0, 2, "WEST", 1, 1);
			countyPanel.add(nonroadDatabaseLabel, gbc);
			gbc.weightx = 1.0;
			LayoutUtility.setPositionOnGrid(gbc,1, 2, "WEST", 1, 1);
			countyPanel.add(nonroadDatabaseCombo, gbc);
			gbc.weightx = 0;
		}
		*/

		outputPanel.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 2;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		outputPanel.add(outputServerLabel, gbc);
		gbc.weightx = 1.0;
		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
		outputPanel.add(outputServer, gbc);

		sharedFolderPathPanel.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 2, 1);
		sharedFolderPathPanel.add(sharedFolderPathName, gbc);
		gbc.weightx = 1.0;
		LayoutUtility.setPositionOnGrid(gbc,2, 0, "WEST", 1, 1);
		sharedFolderPathPanel.add(sharedFolderPathBrowse, gbc);
		gbc.weightx = 0;

		JPanel result = new JPanel();
		result.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 3;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		result.add(countyPanel,gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		result.add(outputPanel,gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		result.add(sharedFolderPathPanel, gbc);
		JPanel buttonPanel = createButtonsPanel();
		LayoutUtility.setPositionOnGrid(gbc,1,0, "NORTH", 1, 3);
		result.add(buttonPanel, gbc);

		return result;
	}

	/**
	 * Creates a panel containing the OK and Cancel buttons.
	 * @return the container as JPanel.
	**/
	JPanel createButtonsPanel() {
		JPanel box = new JPanel();
		box.setLayout(new GridLayout(2,1,5,5)); // 2 rows, 1 column, 5 pixel gaps
		box.add(okButton);
		box.add(cancelButton);

		JPanel result = new JPanel();
		result.setLayout(new BorderLayout());
		result.add(box, BorderLayout.NORTH);
		return result;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == sharedFolderPathBrowse) {
			processSharedFolderPathBrowseButton();
		} else if(e.getSource() == okButton) {
			handleOKButton();
		} else if(e.getSource() == cancelButton) {
			handleCancelButton();
		}
	}

	/**
	 * OK button handler, will save the choices to the System Configuration
	 * before closing this dialog.
	**/
	void handleOKButton() {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();

		// Check the default server and database
		String testServerName = countyServer.getText();
		String testDatabaseName =
				StringUtilities.safeGetString(countyDatabaseCombo.getSelectedItem());
		DatabaseSelection executionDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()];
		if((executionDB.databaseName).equalsIgnoreCase(testDatabaseName)) {
			JOptionPane.showMessageDialog(this,
					"The execution database cannot be used as an input database.");
			return;
		}
		String nrTestDatabaseName = "";
		/*
		if(CompilationFlags.USE_NONROAD) {
			nrTestDatabaseName = StringUtilities.safeGetString(nonroadDatabaseCombo.getSelectedItem());
			if((executionDB.databaseName).equalsIgnoreCase(nrTestDatabaseName)) {
				JOptionPane.showMessageDialog(this,
						"The execution database cannot be used as a NonRoad input database.");
				return;
			}
		}
		*/

		// If the server or database names have changed, validate and save the new change(s)
		boolean didChangeDatabase = false;
		if(!sysConfig.databaseSelections[
				MOVESDatabaseType.DEFAULT.getIndex()].databaseName.equals(
				testDatabaseName) ||
				(!sysConfig.databaseSelections[
				MOVESDatabaseType.DEFAULT.getIndex()].serverName.equals(
				testServerName))) {
			DatabaseSelection dbSelection = new DatabaseSelection();
			dbSelection.serverName = testServerName;
			dbSelection.databaseName = testDatabaseName;
			Connection db = dbSelection.openConnectionOrNull();
			if(db == null) {
				JOptionPane.showMessageDialog(this,
						StringUtilities.wrapString(
								"Cannot connect to MOVES Default Database.", 60));
				return;
			}
			boolean hasDesiredSchema = SchemaInspector.isMOVESSchemaPresent(db, false,false);
			DatabaseUtilities.closeConnection(db);
			if(!hasDesiredSchema) {
				JOptionPane.showMessageDialog(this,
						StringUtilities.wrapString("Invalid MOVES Default Database.", 60));
				return;
			}
			didChangeDatabase = true;
		}
		boolean didChangeNRDatabase = false;
		/*
		if(!sysConfig.databaseSelections[
				MOVESDatabaseType.NRDEFAULT.getIndex()].databaseName.equals(
				nrTestDatabaseName) ||
				(CompilationFlags.USE_NONROAD && !sysConfig.databaseSelections[
				MOVESDatabaseType.NRDEFAULT.getIndex()].serverName.equals(
				testServerName))) {
			if(CompilationFlags.USE_NONROAD) {
				DatabaseSelection dbSelection = new DatabaseSelection();
				dbSelection.serverName = testServerName;
				dbSelection.databaseName = nrTestDatabaseName;
				Connection db = dbSelection.openConnectionOrNull();
				if(db == null) {
					JOptionPane.showMessageDialog(this,
							StringUtilities.wrapString(
									"Cannot connect to NonRoad Database.", 60));
					return;
				}
				boolean hasDesiredSchema = SchemaInspector.isNonRoadSchemaPresent(db, false,false);
				DatabaseUtilities.closeConnection(db);
				if(!hasDesiredSchema) {
					JOptionPane.showMessageDialog(this,
							StringUtilities.wrapString("Invalid NonRoad Database.", 60));
					return;
				}
			}
			didChangeNRDatabase = true;
		}
		*/
		// Check the output server
		String testOutputServerName = StringUtilities.safeGetString(outputServer.getText());
		if(!sysConfig.databaseSelections[
				MOVESDatabaseType.OUTPUT.getIndex()].serverName.equals(
				testOutputServerName)) {
			DatabaseSelection dbSelection = new DatabaseSelection();
			dbSelection.serverName = testOutputServerName;
			dbSelection.databaseName = "";
			Connection db = dbSelection.openConnectionOrNull();
			if(db == null) {
				JOptionPane.showMessageDialog(this,
						StringUtilities.wrapString(
								"Cannot connect to output server.", 60));
				return;
			}
			DatabaseUtilities.closeConnection(db);
		}
		// persist the data
		sysConfig.databaseSelections[
				MOVESDatabaseType.DEFAULT.getIndex()].serverName = testServerName;
		sysConfig.databaseSelections[
				MOVESDatabaseType.DEFAULT.getIndex()].databaseName = testDatabaseName;
		/*
		if(CompilationFlags.USE_NONROAD) {
			sysConfig.databaseSelections[
					MOVESDatabaseType.NRDEFAULT.getIndex()].databaseName = nrTestDatabaseName;
		}
		*/
		sysConfig.sharedDistributedFolderPath = new File(sharedFolderPathName.getText());
		sysConfig.databaseSelections[
				MOVESDatabaseType.OUTPUT.getIndex()].serverName = testOutputServerName;
		try {
			sysConfig.saveConfigurationData();
			DatabaseConnectionManager.cleanupAll();
			DatabaseConnectionManager.initializeAll();
			if(didChangeDatabase) {
				// Reload cached data
				PollutantProcessLoader.loadFromDatabase();
				TimeSpan.loadTimeObjects();
			}
		} catch(Exception e) {
			Logger.logError(e, "Save configuration data failed.");
		}

		// indicates OK button
		result = 1;
		dispose();
	}

	/**
	 * Cancel button handler, will reset the database connection to its original values
	 * before closing this dialog.
	**/
	void handleCancelButton() {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();

		// reload the original system configuration from the file
		try {
			sysConfig.loadConfigurationData();
		} catch(Exception e) {
			Logger.logError(e, "Load configuration data failed.");
		}
		// get the default db connection
		DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		// indicates Cancel button
		result = 0;
		dispose();
	}

	/**
	 * Handles the focus lost event for the countyServer & outputServer textfields,
	 * and the countyDatabaseCombo.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();
		JComponent c = (JComponent)e.getComponent();
		if(c == countyServer) {
			if(countyPreviousServer.equals(countyServer.getText())) {
				return;
			}
			countyPreviousServer = countyServer.getText();
			sysConfig.databaseSelections[
					MOVESDatabaseType.DEFAULT.getIndex()].serverName = countyServer.getText();
			/*
			sysConfig.databaseSelections[
					MOVESDatabaseType.NRDEFAULT.getIndex()].serverName = countyServer.getText();
			*/
			loadDatabases();
		} else if(c == outputServer) {
			if(outputPreviousServer.equals(outputServer.getText())) {
				return;
			}
			outputPreviousServer = outputServer.getText();
			sysConfig.databaseSelections[
					MOVESDatabaseType.OUTPUT.getIndex()].serverName = outputServer.getText();
		}
	}

	/**
	 * Currently not used.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusGained(FocusEvent e) {
	}

	/** Handles the Shared Folder Path Browse button. **/
	public void processSharedFolderPathBrowseButton() {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();
		JFileChooser chooser = new JFileChooser();
		chooser.setFileFilter(new FileFilter() {
			public boolean accept(File f) {
				return f.isDirectory();
			}
			public String getDescription() {
				return "Directories";
			}
		});
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		int returnVal = chooser.showOpenDialog(this);
		if(returnVal == JFileChooser.APPROVE_OPTION) {
			sharedFolderPathName.setText(chooser.getSelectedFile().getPath());
			File tempPath = new File(sharedFolderPathName.getText());
			sysConfig.sharedDistributedFolderPath = tempPath;
		}
	}

	/**
	 * Loads the Databases droplist, based on the countyServer setting.
	 * Also, sets the default droplist selection, if it can be found.
	**/
	public void loadDatabases() {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();
		// get the available databases from the current countyServer selection.
		countyDatabaseCombo.removeAllItems();

		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(null == db) {
			return;
		}
		String sql = "SHOW Databases";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				String nextDB = query.rs.getString(1);
				countyDatabaseCombo.addItem(nextDB);
				/*
				if(CompilationFlags.USE_NONROAD) {
					nonroadDatabaseCombo.addItem(nextDB);
				}
				*/
			}
		} catch(Exception e) {
			Logger.logError(e, "Loading a list of databases failed.");
		} finally {
			query.onFinally();
		}
		// set the default selection
		countyDatabaseCombo.setSelectedItem(sysConfig.databaseSelections[
				MOVESDatabaseType.DEFAULT.getIndex()].databaseName);
		/*
		if(CompilationFlags.USE_NONROAD) {
			nonroadDatabaseCombo.setSelectedItem(sysConfig.databaseSelections[
					MOVESDatabaseType.NRDEFAULT.getIndex()].databaseName);
		}
		*/
	}
}
