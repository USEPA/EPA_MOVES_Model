/**************************************************************************************************
 * @(#)ONITool.java
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
import java.util.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.*;

/**
 * Shows GUI window to select a CDB input database and specify an output
 * file, and then executes the ONI tool.
 * @author		Design Team
 * @version 	2020-10-14
**/
public class ONITool extends JDialog implements ActionListener {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;

	/** Instructions display **/
	JTextPane instructionsTextPane;
	/** Button to refresh the list of databases **/
	JButton refreshButton;
	/** Name of the control file **/
	JLabel saveFileText;
	/** Full path for the file shown in saveFileText **/
	String saveFileFullPath = "";
	/** Button to browse for a save file name **/
	JButton browseSaveFileButton;
	/** Prefix to be applied to the names of all created RunSpecs **/
	JTextField prefixText;
	/** Name of the directory in which RunSpecs will be created **/
	JLabel outputDirectoryText;
	/** Full path for the directory shown in outputDirectoryText **/
	String outputDirectoryFullPath = "";
	/** Button to browse for the output directory **/
	JButton browseOutputButton;
	/** List of messages **/
	JList<String> messagesList;
	/** Button to create RunSpecs from the control file **/
	JButton runButton;
	/** Button to close the window **/
	JButton doneButton;
	/** Button to open instructions **/
	JButton openInstructionsButton;

	/** Type of the control file, Text, XLS, etc **/
	String saveFileType = "";
	/** The worksheet name within the control file if it is XLS type **/
	String saveFileWorksheetName = "";

	/** DefaultListModel for messageList **/
	DefaultListModel<String> messageListModel;

	/** List of messages to be shown **/
	ArrayList<String> messages = new ArrayList<String>();
	/** True if messages contains any errors that should prevent further processing **/
	boolean messagesHasError = false;

	/** Name of the server holding the default, input, and new databases. **/
	JLabel serverLabel;
	/** Database to be created to hold the converted data. **/
	ExtendedComboBox<String> inputDatabaseCombo;

	/**
	 * Browser dialog for selecting an output directory.  Retained as a member
	 * variable so it would retain the last used directory.
	**/
	JFileChooser folderChooser = null;
	
	/** database names that should not be used **/
	TreeSetIgnoreCase invalidDatabaseNames = new TreeSetIgnoreCase();

	/**
	 * Constructs the ONI Tool window, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	**/
	public ONITool(JFrame parent) {
		super(parent, "ONI Tool");
		frame = parent;
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(true);
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		resetMessages();
		populateMessagesList();
		generateListOfInvalidDatabaseNames();
		loadDatabases();

		pack();
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(770,560);
		setModal(true);
		setVisible(true);
	}

	/**
	 * Creates and arranges all dialog controls.
	 * @return the container as JPanel.
	**/
	JPanel createPanel() {
		return createAndArrangeControls();
	}

	/**
	 * Create controls and sets their layout.
	 * @return the container as JPanel of the controls
	**/
	public JPanel createAndArrangeControls() {
		JPanel result = new JPanel();
		instructionsTextPane = new JTextPane();
		saveFileText = new JLabel("Select a save path --> ",JLabel.RIGHT);
		browseSaveFileButton = new JButton(); 
		JLabel label2 = new JLabel();
		prefixText = new JTextField();
		JLabel label3 = new JLabel();
		outputDirectoryText = new JLabel("",JLabel.RIGHT);
		browseOutputButton = new JButton();
		JLabel label4 = new JLabel();
		JLabel label5 = new JLabel();
		JScrollPane scrollPane2 = new JScrollPane();
		runButton = new JButton();
		doneButton = new JButton();
		openInstructionsButton = new JButton();
		
		messageListModel = new DefaultListModel<String>();
		messagesList = new JListWithToolTips<String>(messageListModel);
		messagesList.setSelectedIndex(-1);
		messagesList.setVisibleRowCount(6);
		messagesList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXX");
		ToolTipHelper.add(scrollPane2,"Displays messages, warnings, and errors");
		
		browseSaveFileButton.addActionListener(this);
		browseOutputButton.addActionListener(this);
		runButton.addActionListener(this);
		doneButton.addActionListener(this);
		openInstructionsButton.addActionListener(this);
		
		inputDatabaseCombo = new ExtendedComboBox<String>();
		Dimension d = inputDatabaseCombo.getPreferredSize();
		inputDatabaseCombo.setPreferredSize(new Dimension(250, d.height));
		inputDatabaseCombo.setPopupWidth(inputDatabaseCombo.getPreferredSize().width);
		inputDatabaseCombo.setName("inputDatabaseCombo");
		inputDatabaseCombo.setEditable(true);
		inputDatabaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(inputDatabaseCombo,"Select a County Scale Input Database to use");
		inputDatabaseCombo.addActionListener(this);

		refreshButton = new JButton("Refresh");
		ToolTipHelper.add(refreshButton,"Refresh the list of available databases");
		refreshButton.addActionListener(this);
		refreshButton.setMnemonic('R');
		refreshButton.setDisplayedMnemonicIndex(0);

		serverLabel = new JLabel();
		ToolTipHelper.add(serverLabel,"Server that contains the default and input databases");
		serverLabel.setText(StringUtilities.safeGetString(SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName));

		//======== result ========
		{
			result.setLayout(new GridBagLayout());
			((GridBagLayout)result.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)result.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

			//---- instructionsTextPane ----
			instructionsTextPane.setText("This tool calculates ONI activity from a County Scale input database. This"
						+ " tool is only needed when using MOVES at the County Scale in Emission Rates mode, and only"
						+ " when users do not have their own ONI activity that is granular enough to use with the MOVES"
						+ " rates output (such as by hour of day). Inventory mode users do not need to use this tool,"
						+ " as MOVES will calculate hours of ONI activity during runtime in this mode."
						+ "\r\n\r\n"
						+ "To use this tool, the RunSpec and County Scale Input database should be complete and fully populated"
						+ " first. In the RunSpec, be sure to check output by Source Type. Ensure that the County Input"
						+ " database is complete by looking for all \"green checks\" in the County Data Manager. Then,"
						+ " provide a file name for the ONI Tool output. The file should be either an .xls or .xlsx"
						+ " file for use with Excel, or a .csv file otherwise.  MOVES will save the ONI Tool output to"
						+ " this file. Then, select the input database which should be used to run the ONI Tool. Finally,"
						+ " click \"Run ONI Tool\". Depending on the nature of the run, it may take several minutes to complete."
						+ " The tool will create an Excel file with ONI activity in hours, as well as ONI activity rates"
						+ " in terms of ONI hours per onroad SHO (source hours operating) and ONI hours per VMT (vehicle"
						+ " miles travelled)."
						+ "\r\n\r\n"
						+ "For more detailed instructions, click the \"Open Instructions\" button.");
			instructionsTextPane.setEditable(false);
			instructionsTextPane.setBackground(UIManager.getColor("Panel.background"));
			JPanel p = new JPanel();
			p.setBorder(BorderFactory.createTitledBorder("Instructions"));
			p.setLayout(new GridBagLayout());
			((GridBagLayout)p.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)p.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};
			p.add(instructionsTextPane, new GridBagConstraints(0, 0, 5, 2, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			result.add(p, new GridBagConstraints(0, 0, 5, 2, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//---- label1 ----
			p = new JPanel();
			p.setBorder(BorderFactory.createTitledBorder("Output File"));
			p.setLayout(new GridBagLayout());
			((GridBagLayout)p.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 150, 0};
			((GridBagLayout)p.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)p.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};
			p.add(new JLabel("File:"), new GridBagConstraints(0,0,1,1,0.0,0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			ToolTipHelper.add(saveFileText,"Name and location of the output file (.xlsx)");
			p.add(saveFileText, new GridBagConstraints(1, 0, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- browseSaveFileButton ----
			browseSaveFileButton.setText("Set Save Path...");
			browseSaveFileButton.setMnemonic('B');
			browseSaveFileButton.setDisplayedMnemonicIndex(0);
			ToolTipHelper.add(browseSaveFileButton,"Select an output file name (.xlsx)");
			p.add(browseSaveFileButton, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			result.add(p, new GridBagConstraints(0, 4, 5, 2, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
				
			//---- label2 ----
			p = new JPanel();
			p.setBorder(BorderFactory.createTitledBorder("Database"));
			p.setLayout(new GridBagLayout());
			((GridBagLayout)p.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0E-4, 0.0, 0.0, 0.0 };
			((GridBagLayout)p.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

			label2.setText("Server:");
			p.add(label2, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			p.add(serverLabel, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			p.add(refreshButton, new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			label5.setText("County Scale Input Database:");
			p.add(label5, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			p.add(inputDatabaseCombo, new GridBagConstraints(1, 2, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			result.add(p, new GridBagConstraints(0, 6, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			label5.setDisplayedMnemonic('N');
			label5.setLabelFor(inputDatabaseCombo);

			//---- label4 ----
			label4.setText("Messages:");
			result.add(label4, new GridBagConstraints(0, 7, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
				
			//======== scrollPane2 ========
			{
				scrollPane2.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
				scrollPane2.setViewportView(messagesList);
			}
			result.add(scrollPane2, new GridBagConstraints(0, 8, 5, 2, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			label4.setDisplayedMnemonic('M');
			label4.setLabelFor(scrollPane2);
			
			//---- runButton ----
			runButton.setText("Run ONI Tool");
			ToolTipHelper.add(runButton,"Run the ONI Tool, creating the output file");
			result.add(runButton, new GridBagConstraints(2, 12, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			runButton.setMnemonic('u');
			runButton.setDisplayedMnemonicIndex(0);
			
			//---- openInstructionsButton ----
			openInstructionsButton.setText("Open Instructions");
			ToolTipHelper.add(openInstructionsButton,"Open the instructions document (.pdf)");
			result.add(openInstructionsButton, new GridBagConstraints(3, 12, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			openInstructionsButton.setMnemonic('I');

			//---- doneButton ----
			doneButton.setText("Done");
			ToolTipHelper.add(doneButton,"Close this window");
			result.add(doneButton, new GridBagConstraints(4, 12, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			doneButton.setMnemonic('D');
			doneButton.setDisplayedMnemonicIndex(0);
		}

		return result;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == refreshButton) {
			handleRefreshButton();
		} else if(e.getSource() == browseSaveFileButton) {
			handleBrowseSaveFileButton();
		} else if(e.getSource() == runButton) {
			handleRunButton();
		} else if(e.getSource() == doneButton) {
			handleDoneButton();
		} else if(e.getSource() == openInstructionsButton) {
			handleOpenInstructionsButton();
		}
	}
	
	/** Handle the Refresh button for databases **/
	void handleRefreshButton() {
		generateListOfInvalidDatabaseNames();
		loadDatabases();
	}	

	/** Handle the open button for the instructions file **/
	void handleOpenInstructionsButton() {
		try {
			File file = new File("database/ONITool/InstructionsForONITool.pdf");
			if(!file.exists()) {
				Logger.log(LogMessageCategory.ERROR, "Could not find the instructions file at: " + file.getAbsolutePath());
				return;
			}
			if(!OpenFile.open(file)) {
				return;
			}
		} catch(Exception e) {
			Logger.log(LogMessageCategory.ERROR, "Could not open the instructions file: " + e);
			return;
		}
	}

	/** Handle the Browse button for the script file **/
	void handleBrowseSaveFileButton() {
		try {
			// save dialogs automatically handle if you select a file that already exists
			FileDialog fd = new FileDialog(frame, "Save As...", FileDialog.SAVE);
			fd.setVisible(true);

			if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
				return;
			}
			String filePath = fd.getDirectory() + fd.getFile();
			File file = new File(filePath);
			
			filePath = file.getCanonicalPath();
			saveFileText.setText(""); // clear this in case something goes wrong and we
										 // cannot load the data
			saveFileFullPath = "";
			saveFileType = "";
			saveFileWorksheetName = "";

			saveFileType = "Text";

			saveFileText.setText(file.getCanonicalPath());
			saveFileFullPath = file.getCanonicalPath();
		} catch(Exception e) {
			// Nothing to do here
		}
	}

	/** Handle the Create RunSpecs button **/
	void handleRunButton() {
		boolean success = false;
		try {
			setWaitCursor();
			resetMessages();
			
			File saveFile = null;
			saveFileFullPath = StringUtilities.safeGetString(saveFileFullPath).trim();
			if(saveFileFullPath.length() == 0) {
				JOptionPane.showMessageDialog(null,
						"Please select an output file to save to.",
						"No file selected.", JOptionPane.ERROR_MESSAGE);
				return;
			} else {
				saveFile = new File(saveFileFullPath);
			}
			
			String inputDatabaseName = inputDatabaseCombo.getSelectedItem().toString();
			if(inputDatabaseName != null) {
				inputDatabaseName = inputDatabaseName.trim();
			}
			if(inputDatabaseName == null || inputDatabaseName.length() == 0) {
				JOptionPane.showMessageDialog(null,
						"Please select a fully populated County Scale Input Database to use.",
						"No database selected.", JOptionPane.ERROR_MESSAGE);
				return;
			}
			
			// Run the selected script
			DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
			DatabaseSelection inputDatabase = new DatabaseSelection();
			inputDatabase.serverName = defaultDatabase.serverName;
			inputDatabase.databaseName = inputDatabaseName;
			try {
				success = DatabaseUtilities.executeONITool(saveFile,inputDatabase,defaultDatabase,messages);
			} catch(Exception e) {
				Logger.log(LogMessageCategory.ERROR, "ONI Tool failed: " + e);
				success = false;
			}
			
			int errorCount = 0;
			for (int i = 0; i < messages.size(); i++) {
				if(messages.get(i).toLowerCase().contains("error")) {
					errorCount++;
				}
			}

			if(!success || errorCount > 0) {
				/**
				 * @issue Unable to run the ONI Tool
				**/
				JOptionPane.showMessageDialog(null,
						"The ONI Tool encountered errors. Check the messages list for details.",
						"ONI Tool Error", JOptionPane.ERROR_MESSAGE);
			} else {
				/** @nonissue **/
				JOptionPane.showMessageDialog(null,
						"ONI Tool run completed.",
						"ONI Tool", JOptionPane.INFORMATION_MESSAGE);
			}
		} catch(Exception e) {
			/**
			 * @issue Unable to create RunSpecs
			 * @explain An error was encountered while the Multiple RunSpec Creator was creating new
			 * RunSpec files.  The most likely causes are insufficient permissions to the
			 * output folder or invalid characters in the file name prefix.
			**/
			Logger.logError(e,"Unable to run the ONI Tool");
		} finally {
			setDefaultCursor();
			populateMessagesList();
		}
	}

	/** Handle the Done button **/
	void handleDoneButton() {
		dispose();
	}

	/**
	 * Add a database name to inputDatabaseCombo but only if it isn't already in the
	 * the lists.
	 * @param inputDatabaseName name of the database to attempt to place into the database combo boxes.
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInComboBox(String inputDatabaseName) {
		String a = null;
		String b = addIfNotInComboBox(inputDatabaseName,inputDatabaseCombo);
		if(a == null) {
			if(b == null) {
				return null;
			}
			return b;
		} else {
			return a;
		}
	}

	/**
	 * Add a database name to a combobox but only if it isn't already in the the lists.
	 * @param inputDatabaseName name of the database to attempt to place into the database combo box.
	 * @param combobox a listing of database names on screen
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInComboBox(String inputDatabaseName, ExtendedComboBox<String> combobox) {
		inputDatabaseName = inputDatabaseName.trim();
		ComboBoxModel model = combobox.getModel();
		for(int i = 0; i < model.getSize(); i++) {
			String t = (String)model.getElementAt(i);
			if(t.equalsIgnoreCase(inputDatabaseName)) {
				return t;
			}
		}
		combobox.addItem(inputDatabaseName);
		return inputDatabaseName;
	}

	/**
	 * Loads the Databases droplist, based on the server setting.
	 * Also, sets the default droplist selection, if it can be found.
	**/
	public void loadDatabases() {
		inputDatabaseCombo.removeAllItems();
		// add the default item (no selection)
		inputDatabaseCombo.addItem(new String(""));
		TreeSet<String> databases = new TreeSet<String>();
		// get the available databases from the current server selection
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
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
			Logger.logError(e, "Failed to show databases (load databases) in ONITool.");
			//DatabaseUtilities.closeConnection(db);
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
				inputDatabaseCombo.addItem(nextDB);
			}
			Vector<String> toolTipVector = new Vector<String>();
			for(int i=0; i<inputDatabaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) inputDatabaseCombo.getItemAt(i));
			}
			String[] toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			inputDatabaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show tables from database in ONITool.");
		}
		// set the default selection
		inputDatabaseCombo.setSelectedItem("");
		//DatabaseUtilities.closeConnection(db);
	}

	/** Reset the list of messages **/
	void resetMessages() {
		messagesHasError = false;
		messages.clear();
		populateMessagesList();
	}

	/** Push the list of messages onto the screen **/
	void populateMessagesList() {
		messageListModel.clear();
		for(Iterator i=messages.iterator();i.hasNext();) {
			String m = (String)i.next();
			messageListModel.addElement(m);
		}
	}

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
	}
	
	void setWaitCursor() {
		getRootPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
	}	
	
	void setDefaultCursor() {
		getRootPane().setCursor(Cursor.getDefaultCursor());
	}
}
