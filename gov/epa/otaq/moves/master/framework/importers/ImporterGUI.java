/**************************************************************************************************
 * @(#)ImporterGUI.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.awt.*;
import java.awt.event.*;
import java.sql.*;
import javax.swing.*;
import java.io.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.*;

/**
 * GUI framework holding 1 or more importers as tabs.
 *
 * @author		Wesley Faler
 * @author		Bill Shaw
 * @author		John Covey
 * @author		Mike Kender	task 1903
 * @author		John Covey task 2003
 * @version		2020-07-24
**/
public class ImporterGUI extends JDialog implements ActionListener, FocusListener {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** Associated importer manager for this GUI **/
	ImporterManager manager;
	/** button to close the report **/
	JButton doneButton;
	/** tabbed panes **/
	JTabbedPane tabs;
	/** button on Tools tab to generate an XML file **/
	JButton generateXMLButton;
	/** Create Database button **/
	JButton createDatabaseButton;
	/** Refresh database button **/
	JButton refreshDatabaseButton;
	/** Clear All Imported Data button **/
	JButton clearAllDatabaseButton;
	/** Server text control. **/
	JTextField server;
	/** Database combo control. **/
	ExtendedComboBox<String> databaseCombo;
	/** Run droplist ToolTip **/
	JToolTip databaseToolTip;
	/** Database log scroll pane **/
	JScrollPane logScrollPane;
	/** Database log text **/
	JTextPane logTextPane;

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

	/** Tab panel Integer indexes keyed by IImporter's class name **/
	TreeMap<String,Integer> importerTabIndexes = new TreeMap<String,Integer>();

	/** True when within processDatabaseComboChange **/
	private boolean alreadyInComboChange = false;

	/**
	 * Constructs the main panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	**/
	public ImporterGUI(JFrame parent, ImporterManager managerToUse) {
		super(parent,
				managerToUse.isCountyDomain? "MOVES County Data Manager" :
				managerToUse.isProjectDomain? "MOVES Project Data Manager" : "MOVES Data Importer");
		frame = parent;
		manager = managerToUse;

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(true);
		
		// make closing window via the "X" the same as clicking the Done button
		this.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				doneButton.doClick();
			}
		});
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		if(!populateControls()) {
			return;
		}
		tabs.setSelectedIndex(1); // default to showing the Database tab
		pack();
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(800,600);
		setModal(true);
		setVisible(true);
	}

	/**
	 * Fill the controls
	 * @return false if anything could not be filled/updated
	**/
	public boolean populateControls() {
		if(manager.runSpec.scaleInputDatabase != null
				&& manager.runSpec.scaleInputDatabase.hasDatabase()) {
			server.setText(manager.runSpec.scaleInputDatabase.serverName);
			databaseCombo.setSelectedItem(manager.runSpec.scaleInputDatabase.databaseName);
			processDatabaseComboChange(false);
		}
		refreshDomainStatusIcons();
		return true;
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
	void createControls() {
		int tabIndex = 0;
		doneButton = new JButton("Done");
		doneButton.addActionListener(this);
		doneButton.setMnemonic('o');
		doneButton.setDisplayedMnemonicIndex(1);
		ToolTipHelper.add(doneButton, "Close the dialog");

		tabs = new JTabbedPane();
		tabs.addKeyListener(new TabbedPanelKeyAdapter(tabs));

		JPanel p = makeTitledPanel("RunSpec Summary",createRunSpecSummaryPanel());
		tabs.addTab("RunSpec Summary",null,p,"Summary of the RunSpec's Filters");
		tabIndex++;

		p = makeTitledPanel("Database",createDatabasePanel());
		tabs.addTab("Database",null,p,"Database selection");
//		tabs.setMnemonicAt(1, 'a');
		tabIndex++;

		for(int i=0;i<manager.importers.size();i++) {
			IImporter importer = (IImporter)manager.importers.get(i);
			IImporterPanel importerPanel = importer.getPanel();
			if(importerPanel != null) {
				p = importerPanel.getPanel();
				if(p != null) {
					p = makeTitledPanel(importerPanel.getTitle(),p);
					tabs.addTab(importerPanel.getTitle(),null,p,
							importerPanel.getTitle() + " importer");
					tabs.setEnabledAt(tabIndex, false); //disabled by default
					if(importerPanel.getTitle().charAt(0) == 'V') {
						tabs.setMnemonicAt(tabIndex, 'V');
					}
					importerTabIndexes.put(importer.getClass().getName(),Integer.valueOf(tabIndex));
					tabIndex++;
				}
			}
		}
		
		p = makeTitledPanel("Tools",createToolsPanel());
		tabs.addTab("Tools",null,p,"XML tools");
		tabIndex++;
	}

	/** Handle pollutant table key actions **/
	class TabbedPanelKeyAdapter extends KeyAdapter {
		JTabbedPane table;

		public TabbedPanelKeyAdapter(JTabbedPane tableToUse) {
			table = tableToUse;
		}

		public void keyPressed(KeyEvent e) {
			
			if (e.getKeyCode() == KeyEvent.VK_F1 && e.isControlDown()) {
				JComponent component = (JComponent) e.getSource();
				component.setToolTipText(table.getToolTipTextAt(table.getSelectedIndex()));
			}
		}
	}
	
	/**
	 * Sets the layout of the controls.
	 * @return the container as JPanel of the controls
	**/
	JPanel arrangeControls() {
		JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result,BoxLayout.Y_AXIS));

		// Add the tab control and all panels within
		result.add(tabs);

		// Add buttons
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel,BoxLayout.X_AXIS));
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(doneButton);
		result.add(buttonPanel);

		return result;
	}

	/**
	 * Colors for tab titles
	 * Color choices made by using http://design.geckotribe.com/colorwheel/
	 * with RGB of 80,FF,C0.
	**/
	Color[] titleColors = {
		new Color(255,192,128), // Tan
		new Color(128,255,192), // Aqua
		new Color(128,255,255), // Light Blue
		new Color(128,255,128), // Light Green
		new Color(255,128,191), // Rose
		new Color(255,128,128)  // Light Red
	};

	/**
	 * Next color to be used from titleColors.  Used via modulus operator,
	 * so this value never resets to 0.
	**/
	int titleColorIndex = 0;

	/** Color of title text **/
	Color titleTextColor = new Color(0,0,0);
	/** Title font **/
	java.awt.Font titleFont = null;

	/**
	 * Create a panel with colored title bar at the bottom of the panel.
	 * This is useful for tracking the user's location within the GUI.
	 * Title colors are assigned based on the sequence the panels are created.
	 * @param title title to be shown
	 * @param content panel to be encapsulated
	**/
	JPanel makeTitledPanel(String title, JPanel content) {
		if(title == null || title.length() <= 0) {
			return content;
		}
		if(titleFont == null) {
			java.awt.Font f = getFont();
			titleFont = new java.awt.Font(f.getFontName(),java.awt.Font.PLAIN,f.getSize()*2);
		}
		Color c = titleColors[titleColorIndex % titleColors.length];
		titleColorIndex++;

		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel,BoxLayout.Y_AXIS));
		panel.add(content);

		JPanel labelPanel = new JPanel();
		labelPanel.setLayout(new BoxLayout(labelPanel,BoxLayout.X_AXIS));
		labelPanel.setOpaque(true);
		labelPanel.setBackground(c);
		labelPanel.add(Box.createHorizontalGlue());
		JLabel label = new JLabel(title+" ",SwingConstants.RIGHT);
		label.setForeground(titleTextColor);
		label.setOpaque(false);
		label.setFont(titleFont);
		labelPanel.add(label);

		panel.add(labelPanel);
		return panel;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		try {
			((MOVESWindow)frame).setWaitCursor();
			if(e.getSource() == doneButton) {
				// update main window when closing
				String description = ((MOVESWindow)frame).runSpec.scaleInputDatabase.description;
				((MOVESWindow)frame).runSpec.scaleInputDatabase = new DatabaseSelection();
				((MOVESWindow)frame).runSpec.scaleInputDatabase.databaseName = (String)databaseCombo.getSelectedItem();
				((MOVESWindow)frame).runSpec.scaleInputDatabase.serverName = server.getText();
				((MOVESWindow)frame).runSpec.scaleInputDatabase.description = description;
				((MOVESWindow)frame).createInputDatabasePanel.loadFromRunSpec(((MOVESWindow)frame).runSpec);
				((MOVESWindow)frame).navigationPanel.updateCreateInputDatabaseIcon();
				dispose();
			} else if(e.getSource() == generateXMLButton) {
				handleGenerateXML();
			} else if(e.getSource() == createDatabaseButton) {
				handleCreateDatabaseButton();
			} else if(e.getSource() == refreshDatabaseButton) {
				handleRefreshDatabaseButton();
			} else if(e.getSource() == databaseCombo) {
				processDatabaseComboChange(true);
			} else if(e.getSource() == clearAllDatabaseButton) {
				handleClearAllDatabaseButton();
			}
		} finally {
			((MOVESWindow)frame).setDefaultCursor();
		}
	}

	/**
	 * Generate the RunSpec Summary tab including its text.
	 * @return panel for the RunSpec Summary tab
	**/
	JPanel createRunSpecSummaryPanel() {
		JPanel panel = new JPanel();
		JTextArea textArea = new JTextArea(getRunSpecSummaryText(),10,10);
		textArea.setWrapStyleWord(false);
//		textArea.setAlignmentX(Component.LEFT_ALIGNMENT);
		JScrollPane scrollPane = new JScrollPane(textArea,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		textArea.setEditable(false);
		scrollPane.setOpaque(false);
		textArea.setOpaque(false);
//		scrollPane.setAlignmentX(Component.LEFT_ALIGNMENT);

		panel.setLayout(new BoxLayout(panel,BoxLayout.Y_AXIS));
		panel.add(scrollPane);
		return panel;
	}

	/**
	 * Generate the summary text for the RunSpec Summary tab
	 * @return summary lines, CRLF delimited
	**/
	String getRunSpecSummaryText() {
		if(!(frame instanceof MOVESWindow)) {
			return "";
		}
		MOVESWindow w = (MOVESWindow)frame;
		StringBuffer textBuffer = new StringBuffer();

		// Display the output database
		String appendText = new String("");
		if((manager.runSpec.outputDatabase.serverName != null)
				&& (manager.runSpec.outputDatabase.serverName.length() > 0)) {
			appendText = manager.runSpec.outputDatabase.serverName;
		} else {
			appendText = "[using default]";
		}
		textBuffer.append("Output Database Server Name: " + appendText + "\r\n");
		textBuffer.append("\r\n");
		if((manager.runSpec.outputDatabase.databaseName != null)
				&& (manager.runSpec.outputDatabase.databaseName.length() > 0)) {
			appendText = manager.runSpec.outputDatabase.databaseName;
		} else {
			appendText = "[using default]";
		}
		textBuffer.append("Output Database Name: " + appendText + "\r\n");

		// Display the remaining information
		Iterator panelIterator = w.navigationPanel.panels.listIterator();
		while (panelIterator.hasNext()) {
			JPanel iterPanel = (JPanel)panelIterator.next();

			if(iterPanel instanceof RunSpecEditor) {
				if((iterPanel instanceof TimeSpans)
						|| (iterPanel instanceof MacroscaleGeographicBounds)
						|| (iterPanel instanceof OnRoadVehicleEquipment)
						|| (iterPanel instanceof RoadTypeScreen)
						|| (iterPanel instanceof PollutantsAndProcesses)
						|| (iterPanel instanceof ManageInputDataSets)
						|| (iterPanel instanceof InternalControlStrategies
								&& iterPanel.getName().endsWith(".RateOfProgressStrategy"))
					) {
					RunSpecEditor iterEditor = (RunSpecEditor)iterPanel;
					iterEditor.getPrintableDescription(manager.runSpec, textBuffer);
				}
			}
		}

		return textBuffer.toString();
	}

	/**
	 * Generate the Tools tab.
	 * @return panel for the Tools tab
	**/
	JPanel createToolsPanel() {
		JPanel panel = new JPanel();

		generateXMLButton = new JButton("Generate Importer XML File");
		generateXMLButton.addActionListener(this);
		generateXMLButton.setMnemonic('G');
		generateXMLButton.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(generateXMLButton, "Generate an XML file to use with the command line interface");

		JLabel label1;
		JLabel label2;

		label1 = new JLabel();
		label2 = new JLabel();

		panel.setLayout(new GridBagLayout());
		((GridBagLayout)panel.getLayout()).columnWidths = new int[] {66, 0, 0, 0, 0};
		((GridBagLayout)panel.getLayout()).rowHeights = new int[] {0, 0, 0, 0};
		((GridBagLayout)panel.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0, 1.0E-4};
		((GridBagLayout)panel.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};

		//---- label1 ----
		label1.setText("MOVES Data Importers can be run via the command line using an");
		panel.add(label1, new GridBagConstraints(0, 0, 3, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- label2 ----
		label2.setText("XML-based description file similar to the MOVES RunSpec file.");
		panel.add(label2, new GridBagConstraints(0, 1, 3, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- generateXMLButton ----
		panel.add(generateXMLButton, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));

		return panel;
	}

	/**
	 * Generate the Database tab.
	 * @return panel for the Database tab
	**/
	public JPanel createDatabasePanel() {
		createDatabaseButton = new JButton("Create Database");
		createDatabaseButton.addActionListener(this);
		ToolTipHelper.add(createDatabaseButton, "Create the database on the specified server");

		refreshDatabaseButton = new JButton("Refresh");
		refreshDatabaseButton.addActionListener(this);
		ToolTipHelper.add(refreshDatabaseButton, "Refresh the database listing from the specified server");

		clearAllDatabaseButton = new JButton("Clear All Imported Data");
		clearAllDatabaseButton.addActionListener(this);
		ToolTipHelper.add(clearAllDatabaseButton, "Clear all data in the specified database");

		server = new JTextField(10);
		server.setText("localhost");
		ToolTipHelper.add(server,
				"Edit the name of the server where the database will be located");
		server.setName("server");
		server.addFocusListener(this);
		server.setColumns(10);

		databaseCombo = new ExtendedComboBox<String>();
		Dimension d = databaseCombo.getPreferredSize();
		databaseCombo.setPreferredSize(new Dimension(450, d.height)); // 250
		databaseCombo.setPopupWidth(databaseCombo.getPreferredSize().width);
		databaseCombo.setName("databaseCombo");
		databaseCombo.addActionListener(this);
		databaseCombo.setEditable(true);
		databaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(databaseCombo,
				"Edit or select the name of the database in which the data will be stored");

		JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel,BoxLayout.Y_AXIS));

		JPanel panel2;
		JLabel label6;
		JLabel label4;
		JLabel label5;
		JLabel label1;

		panel2 = new JPanel();
		label6 = new JLabel();
		label4 = new JLabel();
		label5 = new JLabel();
		label1 = new JLabel();
		logScrollPane = new JScrollPane();
		logTextPane = new JTextPane();

		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

		//======== panel2 ========
		{
			panel2.setLayout(new GridBagLayout());
			((GridBagLayout)panel2.getLayout()).columnWidths = new int[] {38, 73, 57, 0, 0, 0, 0};
			((GridBagLayout)panel2.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0};
			((GridBagLayout)panel2.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0E-4};
			((GridBagLayout)panel2.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 1.0, 1.0E-4};

			//---- label6 ----
			label6.setText("Select or create a database to hold the imported data.");
			panel2.add(label6, new GridBagConstraints(0, 0, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- label4 ----
			label4.setText("Server:");
			panel2.add(label4, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- server ----
			panel2.add(server, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- label5 ----
			label5.setText("Database:");
			panel2.add(label5, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- databaseCombo ----
			panel2.add(databaseCombo, new GridBagConstraints(1, 2, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- createDatabaseButton ----
			panel2.add(createDatabaseButton, new GridBagConstraints(4, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- refreshDatabaseButton ----
			panel2.add(refreshDatabaseButton, new GridBagConstraints(4, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- label1 ----
			label1.setText("Log:");
			panel2.add(label1, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

/*
			//---- checkDatabaseButton ----
			panel2.add(checkDatabaseButton, new GridBagConstraints(3, 3, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
*/
			//---- clearAllDatabaseButton ----
			panel2.add(clearAllDatabaseButton, new GridBagConstraints(4, 3, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//======== logScrollPane ========
			{

				//---- logTextPane ----
				logTextPane.setEditable(false);
				logScrollPane.setViewportView(logTextPane);
			}
			panel2.add(logScrollPane, new GridBagConstraints(0, 4, 6, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 0), 0, 0));
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
			/* Removed due to speed concerns. The first time this step is done, MySQL is very, very slow.
			boolean foundOutputTables = false;
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				foundOutputTables = false;
				String nextDatabase = (String)i.next();
				if(nextDatabase.length() == 0 || invalidDatabaseNames.contains(nextDatabase)) {
					continue;
				}
				// look at all tables from the next databaseName, compare the table
				// names to one output table name
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
					// SQL error here just means the database is not an output database
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
				// check if this database has any output tables, if not must add
				// the databaseName to the remove names list
				if(!foundOutputTables) {
					stringsToRemove.add(nextDatabase);
				}
			}
			*/
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
	 * @param shouldDisplayErrors true if error messages should be displayed
	 * @return false if the selected database is not valid, or not yet selected.
	**/
	public boolean processDatabaseComboChange(boolean shouldDisplayErrors) {
		if(alreadyInComboChange) {
			return true;
		}
		manager.database.serverName = "";
		manager.database.databaseName = "";

		if(databaseCombo.getSelectedItem() == null) {
			refreshDomainStatusIcons();
			return false;
		}
		if(databaseCombo.getSelectedItem().toString().length() == 0) {
			refreshDomainStatusIcons();
			return false;
		}

		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			refreshDomainStatusIcons();
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
				refreshDomainStatusIcons();
				return false;
			}
			try {
				loadLog(db);

				if(manager.isCountyDomain || manager.isProjectDomain) {
					ArrayList<String> messages = new ArrayList<String>();
					int result =
							ImporterManager.isCountyDomainDatabase(manager.runSpec,
							messages,db,false);
					if(result < 0) {
						if(shouldDisplayErrors) {
							// Display the error messages
							String t = manager.isCountyDomain?
									"Unable to use this entry as a County Domain database."
									: "Unable to use this entry as a Project Domain database.";
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
						}
						if(result < 0) {
							databaseCombo.setSelectedItem("");
							return false;
						}
					}
				}
			} finally {
				DatabaseUtilities.closeConnection(db);
				db = null;
				refreshDomainStatusIcons();
			}
			newDatabaseName = addIfNotInComboBox(newDatabaseName);
			alreadyInComboChange = true;
			databaseCombo.setSelectedItem(newDatabaseName);
			alreadyInComboChange = false;

			manager.database.serverName = dbSelection.serverName;
			manager.database.databaseName = dbSelection.databaseName;
			manager.addDatabaseToRunSpec(false);
		} else {
			refreshDomainStatusIcons();
		}
		if(status != null && shouldDisplayErrors) {
			JOptionPane.showMessageDialog(this, status);
		} else {
			for(int x=0; x<tabs.getTabCount(); x++) {
				tabs.setEnabledAt(x, true);
			}
		}
		return status == null;
	}

	/**
	 * Handles the focus lost event for the server textfield, and checkboxes.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		JComponent c = (JComponent)e.getComponent();
		if(c == server) {
			if(previousServer.equalsIgnoreCase(server.getText())) {
				return;
			}
			previousServer = server.getText();
			loadDatabases();
		}
	}

	/**
	 * Currently not used.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
	}

	/**
	 * Handle the "Generate Importer XML File" button on the Tools tab
	**/
	void handleGenerateXML() {
		try {
			// Prompt for the XML file name
			FileDialog fd = new FileDialog(frame, "Importer XML File", FileDialog.SAVE);
			fd.setVisible(true); //fd.show();

			if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
				return;
			}
			String filePath = fd.getDirectory() + fd.getFile();
			File file = new File(filePath);
			filePath = file.getCanonicalPath();

			// Have the manager write xml to the named file
			manager.generateXML(filePath);
		} catch(IOException e) {
			// Complain that the file couldn't be created
			JOptionPane.showMessageDialog(null,
					"Unable to generate the XML file",
					"Data Importer", JOptionPane.ERROR_MESSAGE);
			Logger.logError(e,"Unable to import");
		}
	}

	/** Handle the Create Database button **/
	void handleCreateDatabaseButton() {
		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName.length() == 0) {
			JOptionPane.showMessageDialog(this,"Specify a database name.");
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
			int createStatus = manager.createDatabase(dbSelection);
			if(DatabaseSelection.CREATED == createStatus) {
				// show a success message and add this item to the list,
				// in case the user hits the droplist button
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Database successfully created.", 60));
				newDatabaseName = addIfNotInComboBox(newDatabaseName);
				databaseCombo.setSelectedItem(newDatabaseName);
				manager.database.serverName = dbSelection.serverName;
				manager.database.databaseName = dbSelection.databaseName;
				manager.addDatabaseToRunSpec(false);
			} else if(DatabaseSelection.EXISTS == createStatus) {
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Database already exists.", 60));
				newDatabaseName = addIfNotInComboBox(newDatabaseName);
				databaseCombo.setSelectedItem(newDatabaseName);
				manager.database.serverName = dbSelection.serverName;
				manager.database.databaseName = dbSelection.databaseName;
				manager.addDatabaseToRunSpec(false);
			} else {
				Logger.log(LogMessageCategory.ERROR,"Could not create the required schema.");
			}
			refreshDomainStatusIcons();
			clearLogDisplay();
			processDatabaseComboChange(true);
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
	 * Load the log of activity in a database
	 * @param db database to be scanned
	**/
	public void loadLog(Connection db) {
		String sql = "select * from auditLog order by whenHappened desc";
		Statement statement = null;
		ResultSet rs = null;
		String logText = "";
		try {
			statement = db.createStatement();
			rs = SQLRunner.executeQuery(statement,sql);
			while(rs.next()) {
				String whenHappened
						= StringUtilities.safeGetString(rs.getString("whenHappened"));
				String importerName
						= StringUtilities.safeGetString(rs.getString("importerName"));
				String briefDescription
						= StringUtilities.safeGetString(rs.getString("briefDescription")).trim();
				String fullDescription
						= StringUtilities.safeGetString(rs.getString("fullDescription")).trim();
				String line = whenHappened + " " + importerName + " " + briefDescription;
				if(fullDescription.length() > 0) {
					line += "\r\n" + fullDescription;
				}
				line += "\r\n";
				logText += line;
			}
		} catch(Exception e) {
			// Note: If there is no auditLog table, then silently just load an empty
			// ----- log listing.
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
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
		logTextPane.setText(logText);
	}

	/** Remove all entries from the displayed log **/
	void clearLogDisplay() {
		logTextPane.setText("");
		for(int i=0;i<manager.importers.size();i++) {
			IImporter importer = (IImporter)manager.importers.get(i);
			IImporterPanel p = importer.getPanel();
			if(p != null) {
				p.clearLogDisplay();
			}
		}
	}

	/**
	 * Reload the list of databases, defaulting back to no selection.
	**/
	void handleRefreshDatabaseButton() {
		clearLogDisplay();
		loadDatabases();
		refreshDomainStatusIcons();
	}

	/**
	 * Clear imported data in all importers.
	**/
	void handleClearAllDatabaseButton() {
		boolean shouldCloseConnection = false;
		Connection db = null;
		if(manager.activeDb != null) {
			db = manager.activeDb;
			shouldCloseConnection = false;
		} else {
			db = openCurrentDatabase(false);
			shouldCloseConnection = true;
			manager.activeDb = db;
		}
		if(db == null) {
			JOptionPane.showMessageDialog(null,
					"Please select a database first.",
					"Database Needed", JOptionPane.ERROR_MESSAGE);
			return;
		}
		int choice = JOptionPane.showConfirmDialog(null,
				"Are you sure you want to clear all the data? ",
				"",JOptionPane.YES_NO_OPTION);
		if(choice != JOptionPane.YES_OPTION) {
			if(shouldCloseConnection) {
				DatabaseUtilities.closeConnection(db);
				manager.activeDb = null;
			}
			db = null;
			return;
		}
		try {
			for(int i=0;i<manager.importers.size();i++) {
				IImporter importer = (IImporter)manager.importers.get(i);
				IDataHandler dataHandler = importer.getDataHandler();
				if(dataHandler == null) {
					continue;
				}
				try {
					dataHandler.doClear(db);
					manager.deleteLog(importer);
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		} finally {
			loadLog(db);
			if(shouldCloseConnection) {
				DatabaseUtilities.closeConnection(db);
				manager.activeDb = null;
			}
			db = null;
		}
		refreshDomainStatusIcons();
	}

	/**
	 * Examine each importer for domain completeness, updating its icon as needed.
	**/
	void refreshDomainStatusIcons() {
		try {
			((MOVESWindow)frame).setWaitCursor();
			ArrayList<ImporterBase> importersToUpdate = new ArrayList<ImporterBase>();
			for(int i=0;i<manager.importers.size();i++) {
				IImporter importer = (IImporter)manager.importers.get(i);
				if(!(importer instanceof ImporterBase)) {
					continue;
				}
				((ImporterBase)importer).removeQualityMessages();
				importersToUpdate.add((ImporterBase)importer);
			}
			if(manager.isCountyDomain) {
				refreshCountyDomainStatusIcons();
			} else if(manager.isProjectDomain) {
				refreshProjectDomainStatusIcons();
			} else {
				refreshImporterStatusIcons();
			}
			for(ImporterBase i : importersToUpdate) {
				if(i.tabBaseProvider != null) {
					i.tabBaseProvider.showMessages();
				}
			}
		} finally {
			((MOVESWindow)frame).setDefaultCursor();
		}
	}

	/**
	 * Convert a status into a display image.
	 * @param status status to be examined
	 * @param nullIsBlank true if a null status should result in a null image
	 * @return an image to be displayed or null if no image should be shown
	**/
	ImageIcon getImage(RunSpecSectionStatus status, boolean nullIsBlank) {
		if(status == null) {
			if(nullIsBlank) {
				return null;
			} else {
				errorImage.setDescription(Constants.IMPORTER_GUI_NOT_READY_TOOLTIP);
				return errorImage;
			}
		}
		if(status.status == RunSpecSectionStatus.OK) {
			okImage.setDescription(Constants.IMPORTER_GUI_READY_TOOLTIP);
			return okImage;
		} else if(status.status == RunSpecSectionStatus.DEFAULTS) {
			return defaultImage;
		} else {
			errorImage.setDescription(Constants.IMPORTER_GUI_NOT_READY_TOOLTIP);
			return errorImage;
		}
	}

	/**
	 * Examine each importer for county domain completeness, updating its icon
	 * as needed.
	**/
	void refreshCountyDomainStatusIcons() {
		if(!manager.isCountyDomain) { // Only do this if in County Domain mode
			return;
		}
		boolean shouldCloseConnection = false;
		Connection db = null;
		if(manager.activeDb != null) {
			db = manager.activeDb;
			shouldCloseConnection = false;
		} else {
			db = openCurrentDatabase(false);
			shouldCloseConnection = true;
		}
		try {
			RunSpecSectionStatus netStatus = null; // used to determine if all tabs are green checks or not
			for(int i=0;i<manager.importers.size();i++) {
				IImporter importer = (IImporter)manager.importers.get(i);
				if(!(importer instanceof ICountyDataImporter)) {
					continue;
				}
				
				// determine importer's status
				RunSpecSectionStatus status = null;
				if(db != null) {
					try {
						importer.refreshFromAuditLog(db);
						status = ((ICountyDataImporter)importer).getCountyDataStatus(db);
					} catch(Exception e) {
						Logger.logError(e,"Unable to validate County Data Status");
					}
				}
				
				// set the green check or red x icon for the importer's tab along with a descriptive tooltip
				Integer index = (Integer)importerTabIndexes.get(importer.getClass().getName());
				if(index == null) {
					continue;
				}
				tabs.setIconAt(index.intValue(),getImage(status,false));
				tabs.setToolTipTextAt(index, tabs.getTitleAt(index) + Constants.IMPORTER_GUI_BASE_TOOLTIP + ((ImageIcon) tabs.getIconAt(index)).getDescription());
				
				// calculate net status of all importers
				if(status != null) {
					if(netStatus == null) {
						netStatus = status;
					} else {
						netStatus.makeWorstOfTwo(status);
					}
					//Logger.log(LogMessageCategory.DEBUG,"After evaluating status of " + importer.getName() + ", netStatus is " + String.valueOf(netStatus.status));
				} else {
					//Logger.log(LogMessageCategory.DEBUG,"After evaluating status of " + importer.getName() + ", netStatus has no change (status was null)");
				}
			}
			
			// Save netStatus to MOVESWindow so the CreateInputDatabase panel can be appropriately updated
			((MOVESWindow)frame).domainImporterNetStatus = netStatus;
		} finally {
			if(shouldCloseConnection) {
				DatabaseUtilities.closeConnection(db);
			}
			db = null;
		}
	}

	/**
	 * Examine each importer for project domain completeness, updating its icon
	 * as needed.
	**/
	void refreshProjectDomainStatusIcons() {
		if(!manager.isProjectDomain) { // Only do this if in Project Domain mode
			return;
		}
		boolean shouldCloseConnection = false;
		Connection db = null;
		if(manager.activeDb != null) {
			db = manager.activeDb;
			shouldCloseConnection = false;
		} else {
			db = openCurrentDatabase(false);
			shouldCloseConnection = true;
		}
		try {
			RunSpecSectionStatus netStatus = null; // used to determine if all tabs are green checks or not
			for(int i=0;i<manager.importers.size();i++) {
				IImporter importer = (IImporter)manager.importers.get(i);
				if(!(importer instanceof IProjectDataImporter)) {
					continue;
				}
				
				// determine importer's status
				RunSpecSectionStatus status = null;
				if(db != null) {
					try {
						importer.refreshFromAuditLog(db);
						status = ((IProjectDataImporter)importer).getProjectDataStatus(db);
					} catch(Exception e) {
						// Nothing to do here
					}
				}
				
				// set the green check or red x icon for the importer's tab along with a descriptive tooltip
				Integer index = (Integer)importerTabIndexes.get(importer.getClass().getName());
				if(index == null) {
					continue;
				}
				tabs.setIconAt(index.intValue(),getImage(status,false));
				tabs.setToolTipTextAt(index, tabs.getTitleAt(index) + Constants.IMPORTER_GUI_BASE_TOOLTIP + ((ImageIcon) tabs.getIconAt(index)).getDescription());
				
				// calculate net status of all importers
                if(status != null) {
                    if(netStatus == null) {
                        netStatus = status;
                    } else {
                        netStatus.makeWorstOfTwo(status);
                    }
                    //Logger.log(LogMessageCategory.DEBUG,"After evaluating status of " + importer.getName() + ", netStatus is " + String.valueOf(netStatus.status));
                } else {
                    //Logger.log(LogMessageCategory.DEBUG,"After evaluating status of " + importer.getName() + ", netStatus has no change (status was null)");
                }
            }
			
			// Save net status to MOVESWindow so the CreateInputDatabase panel can be appropriately updated
			((MOVESWindow)frame).domainImporterNetStatus = netStatus;
		} finally {
			if(shouldCloseConnection) {
				DatabaseUtilities.closeConnection(db);
			}
			db = null;
		}
	}

	/**
	 * Examine each importer for generic domain completeness, updating its icon
	 * as needed.
	**/
	void refreshImporterStatusIcons() {
		boolean shouldCloseConnection = false;
		Connection db = null;
		if(manager.activeDb != null) {
			db = manager.activeDb;
			shouldCloseConnection = false;
		} else {
			db = openCurrentDatabase(false);
			shouldCloseConnection = true;
		}
		try {
			RunSpecSectionStatus netStatus = null; // used to determine if all tabs are green checks or not
			for(int i=0;i<manager.importers.size();i++) {
				IImporter importer = (IImporter)manager.importers.get(i);
				if(!(importer instanceof IDataStatus)) {
					continue;
				}
				
				// determine importer's status
				RunSpecSectionStatus status = null;
				if(db != null) {
					try {
						importer.refreshFromAuditLog(db);
						status = ((IDataStatus)importer).getImporterDataStatus(db);
					} catch(Exception e) {
						// Nothing to do here
					}
				}
				
				// set the green check or red x icon for the importer's tab along with a descriptive tooltip
				Integer index = (Integer)importerTabIndexes.get(importer.getClass().getName());
				if(index == null) {
					continue;
				}
				tabs.setIconAt(index.intValue(),getImage(status,true));
				if ((ImageIcon) tabs.getIconAt(index) != null) {
					tabs.setToolTipTextAt(index, tabs.getTitleAt(index) + Constants.IMPORTER_GUI_BASE_TOOLTIP + ((ImageIcon) tabs.getIconAt(index)).getDescription());
				}
				
				// calculate net status of all importers
				if(status != null) {
                    if(netStatus == null) {
                        netStatus = status;
                    } else {
                        netStatus.makeWorstOfTwo(status);
                    }
                    //Logger.log(LogMessageCategory.DEBUG,"After evaluating status of " + importer.getName() + ", netStatus is " + String.valueOf(netStatus.status));
                } else {
                    //Logger.log(LogMessageCategory.DEBUG,"After evaluating status of " + importer.getName() + ", netStatus has no change (status was null)");
                }
            }
			// Save net status to MOVESWindow so the CreateInputDatabase panel can be appropriately updated
			((MOVESWindow)frame).domainImporterNetStatus = netStatus;
		} finally {
			if(shouldCloseConnection) {
				DatabaseUtilities.closeConnection(db);
			}
			db = null;
		}
	}
}
