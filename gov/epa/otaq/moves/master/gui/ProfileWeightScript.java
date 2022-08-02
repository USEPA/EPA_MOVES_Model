/**************************************************************************************************
 * @(#)Converter.java
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
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import java.io.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.*;

/**
 * Select and execute a database conversion script given the current default
 * database, an existing input database, and a new database to be created.
 * @author		Wesley Faler
 * @author  	Bill Shaw (508 compliance mods)
 * @author  	John Covey (Task 1903)
 * @author		Mike Kender (Task 2003)
 * @version 	2020-08-13
**/
public class ProfileWeightScript extends JDialog implements ActionListener {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;

	/** Instructions display **/
	JTextPane instructionsTextPane;
	/** Button to refresh the list of databases **/
	JButton refreshButton;
	/** Name of the control file **/
	JLabel controlFileText;
	/** Full path for the file shown in controlFileText **/
	String controlFileFullPath = "";
	/** Button to browse for a control file **/
	JButton browseControlFileButton;
	/** Prefix to be applied to the names of all created RunSpecs **/
	JTextField prefixText;
	/** Name of the directory in which RunSpecs will be created **/
	JLabel outputDirectoryText;
	/** Full path for the directory shown in outputDirectoryText **/
	String outputDirectoryFullPath = "";
	/** List of messages **/
	JList<String> messagesList;
	/** Button to create RunSpecs from the control file **/
	JButton createRunSpecsButton;
	/** Button to close the window **/
	JButton doneButton;
	/** Button to open help **/
	JButton openHelpButton;

	/** DefaultListModel for messageList **/
	DefaultListModel<String> messageListModel;

	/** List of messages to be shown **/
	ArrayList<String> messages = new ArrayList<String>();
	/** True if messages contains any errors that should prevent further processing **/
	boolean messagesHasError = false;

	/** Name of the server holding the default, input, and new databases. **/
	JLabel serverLabel;
	/** Database containing old data to be converted. **/
	ExtendedComboBox<String> outputDatabaseCombo;
	/** Database to be created to hold the converted data. **/
	ExtendedComboBox<String> newDatabaseCombo;

	/**
	 * Browser dialog for selecting an output directory.  Retained as a member
	 * variable so it would retain the last used directory.
	**/
	JFileChooser folderChooser = null;

	/**
	 * Constructs the FERC panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	 * @param modeToUse Default conversion mode
	**/
	public ProfileWeightScript(JFrame parent) {
		super(parent, "Onroad Speciation Profile Weighting Script");
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
		loadDatabases();

		pack();
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(620,720); // 520,610
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
		JLabel label1 = new JLabel();
		controlFileText = new JLabel("",JLabel.RIGHT);
		browseControlFileButton = new JButton(); 
		JLabel label2 = new JLabel();
		prefixText = new JTextField();
		JLabel label3 = new JLabel();
		outputDirectoryText = new JLabel("",JLabel.RIGHT);
		JLabel label4 = new JLabel();
		JLabel label5 = new JLabel();
		JScrollPane scrollPane2 = new JScrollPane();
		createRunSpecsButton = new JButton();
		doneButton = new JButton();
		openHelpButton = new JButton();

		messageListModel = new DefaultListModel<String>();
		messagesList = new JListWithToolTips<String>(messageListModel);
		messagesList.setSelectedIndex(-1);
		messagesList.setVisibleRowCount(6);
		messagesList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXX");
		ToolTipHelper.add(scrollPane2,"Displays messages, warnings, and errors");

		browseControlFileButton.addActionListener(this);
		createRunSpecsButton.addActionListener(this);
		doneButton.addActionListener(this);
		openHelpButton.addActionListener(this);

		outputDatabaseCombo = new ExtendedComboBox<String>();
		Dimension d = outputDatabaseCombo.getPreferredSize();
		outputDatabaseCombo.setPreferredSize(new Dimension(250, d.height));
		outputDatabaseCombo.setPopupWidth(outputDatabaseCombo.getPreferredSize().width);
		outputDatabaseCombo.setName("outputDatabaseCombo");
		outputDatabaseCombo.setEditable(true);
		outputDatabaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(outputDatabaseCombo,"Select the name of the output database for the MOVES run(s) to be speciated.");
		outputDatabaseCombo.addActionListener(this);

		newDatabaseCombo = new ExtendedComboBox<String>();
		d = newDatabaseCombo.getPreferredSize();
		newDatabaseCombo.setPreferredSize(new Dimension(250, d.height));
		newDatabaseCombo.setPopupWidth(newDatabaseCombo.getPreferredSize().width);
		newDatabaseCombo.setName("newDatabaseCombo");
		newDatabaseCombo.setEditable(true);
		newDatabaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(newDatabaseCombo,"Enter or select the name of the database to hold the profile weight tables.");
		newDatabaseCombo.addActionListener(this);

		refreshButton = new JButton("Refresh");
		ToolTipHelper.add(refreshButton,"Refresh the list of available databases");
		refreshButton.addActionListener(this);
		refreshButton.setMnemonic('R');
		refreshButton.setDisplayedMnemonicIndex(0);

		serverLabel = new JLabel();
		ToolTipHelper.add(serverLabel,"Server that contains the default, output, and new databases.");
		serverLabel.setText(StringUtilities.safeGetString(SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName));

		StyledDocument doc = (StyledDocument) instructionsTextPane.getDocument();
		SimpleAttributeSet normal = new SimpleAttributeSet();
        StyleConstants.setFontFamily(normal, "SansSerif");
		SimpleAttributeSet bold = new SimpleAttributeSet(normal);
        StyleConstants.setBold(bold, true);
		
		//======== result ========
		try {
			result.setLayout(new GridBagLayout());
			((GridBagLayout)result.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)result.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

			//---- instructionsTextPane ----
			doc.insertString(doc.getLength(),
				"This script calculates speciation profile weights used to speciate onroad emissions"
				+ " for residual total organic gases (NonHAPTOG), total organic matter (TOM), and"
				+ " residual particulate matter (NonECNonSO4NonOM PM). This tool is designed for users"
				+ " who are interested in air quality modeling or applying chemical mechanisms."
				+ "\r\n\r\n"
				+ "The script runs against a MOVES output database with output for the three pollutants"
				+ " for a single calendar year and a single county. The MOVES runs must have output by"
				+ " SCC, source type, fuel type, model year, emission process, regulatory class and road type. It will"
				+ " write the profile weights to a different database which can be selected below."
				+ " For more detail on how to perform the MOVES runs and how the profile weighting tables"
				+ " are defined, click the \"Open Help\" button."
				+ "\r\n\r\n"
				+ "To use this tool, select the MOVES output you wish to speciate from the"
				+ " \"Output Database\" drop-down list below. Then select the database which will hold"
				+ " profile weighting tables as the \"New Database\". If the database does not exist, it will"
				+ " be created. Use the \"Run Profile Weighting Script\" button to execute the script file."
				+ "\r\n\r\n",
				normal);
					
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

			//---- label2 ----
			p = new JPanel();
			p.setBorder(BorderFactory.createTitledBorder("Databases"));
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

			label3.setText("Output Database:");
			p.add(label3, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			p.add(outputDatabaseCombo, new GridBagConstraints(1, 1, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			label3.setDisplayedMnemonic('O');
			label3.setLabelFor(outputDatabaseCombo);

			label5.setText("New Database:");
			p.add(label5, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			p.add(newDatabaseCombo, new GridBagConstraints(1, 2, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			result.add(p, new GridBagConstraints(0, 4, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			label5.setDisplayedMnemonic('N');
			label5.setLabelFor(newDatabaseCombo);

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

			//---- createRunSpecsButton ----
			createRunSpecsButton.setText("Run Profile Weighting Script");
			ToolTipHelper.add(createRunSpecsButton,"Apply the script file to the output database, creating the new database");
			result.add(createRunSpecsButton, new GridBagConstraints(2, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			createRunSpecsButton.setMnemonic('S');
			
			//---- openHelpButton ----
			openHelpButton.setText("Open Help");
			ToolTipHelper.add(openHelpButton,"Open the help document (.pdf)");
			result.add(openHelpButton, new GridBagConstraints(3, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			openHelpButton.setMnemonic('H');

			//---- doneButton ----
			doneButton.setText("Done");
			ToolTipHelper.add(doneButton,"Close this window");
			result.add(doneButton, new GridBagConstraints(4, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			doneButton.setMnemonic('D');
			doneButton.setDisplayedMnemonicIndex(0);
		} catch (BadLocationException ex) {
            ex.printStackTrace(System.err);
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
		} else if(e.getSource() == createRunSpecsButton) {
			handleConvertButton();
		} else if(e.getSource() == doneButton) {
			handleDoneButton();
		} else if(e.getSource() == openHelpButton) {
			handleOpenHelpButton();
		}
	}

	/** Handle the Refresh button for databases **/
	void handleRefreshButton() {
		loadDatabases();
	}

	/** Handle the Create RunSpecs button **/
	void handleConvertButton() {
		try {
			resetMessages();

			String outputDatabaseName = outputDatabaseCombo.getSelectedItem().toString();
			if(outputDatabaseName != null) {
				outputDatabaseName = outputDatabaseName.trim();
			}
			if(outputDatabaseName == null || outputDatabaseName.length() == 0) {
				messages.add("Specify an output database name.");
				messagesHasError = true;
			}

			String newDatabaseName = newDatabaseCombo.getSelectedItem().toString();
			if(newDatabaseName != null) {
				newDatabaseName = newDatabaseName.trim();
			}
			if(newDatabaseName == null || newDatabaseName.length() == 0) {
				messages.add("Specify a new database name.");
				messagesHasError = true;
			}

			if(!messagesHasError) {
				if(newDatabaseName.equalsIgnoreCase(outputDatabaseName)) {
					messages.add("The output database and new database cannot be the same.");
					messagesHasError = true;
				}
			}

			if(messagesHasError) {
				return;
			}

			// Convert
			DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];

			DatabaseSelection outputDatabase = new DatabaseSelection();
			outputDatabase.serverName = defaultDatabase.serverName;
			outputDatabase.databaseName = outputDatabaseName;

			DatabaseSelection newDatabase = new DatabaseSelection();
			newDatabase.serverName = defaultDatabase.serverName;
			newDatabase.databaseName = newDatabaseName;

			try {
				DatabaseUtilities.executeProfileWeightScript(outputDatabase,newDatabase,defaultDatabase,messages);
				messages.add("Script successful.");
			} catch(Exception e) {
				messages.add("Script failed: " + e.getMessage());
				messagesHasError = true;
			}

			if(messagesHasError) {
				/**
				 * @issue Unable to convert the database.
				**/
				JOptionPane.showMessageDialog(null,
						"Unable to run the speciation profile weighting script. Check the messages list for details.",
						"Speciation Profile Weighting Script", JOptionPane.ERROR_MESSAGE);
			} else {
				/** @nonissue **/
				JOptionPane.showMessageDialog(null,
						"Speciation profile weighting script has run successfully.",
						"Speciation Profile Weighting Script", JOptionPane.INFORMATION_MESSAGE);
			}
		} catch(Exception e) {
			/**
			 * @issue Unable to create RunSpecs
			 * @explain An error was encountered while the Multiple RunSpec Creator was creating new
			 * RunSpec files.  The most likely causes are insufficient permissions to the
			 * output folder or invalid characters in the file name prefix.
			**/
			Logger.logError(e,"Speciation profile weighting script failed.");
		} finally {
			populateMessagesList();
		}
	}

	/** Handle the open button for the help file **/
	void handleOpenHelpButton() {
		try {
			File file = new File("database/ProfileWeightScripts/OnroadSpeciationInstructions.pdf");
			if(!file.exists()) {
				Logger.log(LogMessageCategory.ERROR, "Could not find the help file at: " + file.getAbsolutePath());
				return;
			}
			if(!OpenFile.open(file)) {
				return;
			}
		} catch(Exception e) {
			Logger.log(LogMessageCategory.ERROR, "Could not open the help file: " + e);
			return;
		}
	}

	/** Handle the Done button **/
	void handleDoneButton() {
		dispose();
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

	/**
	 * Add a database name to outputDatabaseCombo and newDatabaseCombo but only if it isn't already in the
	 * the lists.
	 * @param newDatabaseName name of the database to attempt to place into the database combo boxes.
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInComboBox(String newDatabaseName) {
		String a = addIfNotInComboBox(newDatabaseName,outputDatabaseCombo);
		String b = addIfNotInComboBox(newDatabaseName,newDatabaseCombo);
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
	 * @param newDatabaseName name of the database to attempt to place into the database combo box.
	 * @param combobox a listing of database names on screen
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInComboBox(String newDatabaseName, ExtendedComboBox<String> combobox) {
		newDatabaseName = newDatabaseName.trim();
		ComboBoxModel model = combobox.getModel();
		for(int i = 0; i < model.getSize(); i++) {
			String t = (String)model.getElementAt(i);
			if(t.equalsIgnoreCase(newDatabaseName)) {
				return t;
			}
		}
		combobox.addItem(newDatabaseName);
		return newDatabaseName;
	}

	/**
	 * Loads the Databases combobox with all databases on the server except for those with a movesoutput table
	 * (since those are obviously output databases and should not be converted by this function).
	 * It also filters out databases with an "agecategory" table, as those are assumed to be default databases.
	**/
	public void loadDatabases() {
		outputDatabaseCombo.removeAllItems();
		newDatabaseCombo.removeAllItems();
		// add the default item (no selection)
		outputDatabaseCombo.addItem(new String(""));
		newDatabaseCombo.addItem(new String(""));

		TreeSet<String> databases = new TreeSet<String>();
		// get the available databases from the current server selection
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		if(null == db) {
			Logger.log(LogMessageCategory.ERROR,"Could not connect to the default database");
			return;
		}
		String sql = "SELECT schema_name FROM information_schema.schemata order by schema_name";
		PreparedStatement statement;
		ResultSet results;
		try {
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					String nextDB = results.getString(1);
					databases.add(nextDB);
				}
				results.close();
			}
			statement.close();
		} catch(Exception e) {
			Logger.logError(e, "Failed to show databases (load databases) in Profile Weight Script GUI.");
			DatabaseUtilities.closeConnection(db);
			return;
		}
		// second pass through the returned list of database names, must now
		// remove any databases that are an output database
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
				// names to one output table name (and agecategory, a default database table)
				sql = "SHOW TABLES FROM " + nextDatabase;
				try {
					statement = db.prepareStatement(sql);
					results = SQLRunner.executeQuery(statement,sql);
					if(results != null) {
						while(results.next()) {
							String nextTable = results.getString(1);
							if(nextTable.equalsIgnoreCase("AGECATEGORY") || nextTable.equalsIgnoreCase("AUDITLOG")) {
								foundOutputTable = true;
								break;
							}
						}
					}
				} catch (Exception e) {
					// SQL error here just means this database not an output database
				}
				// check if this database has any output tables, if so must add
				// the databaseName to the remove names list
				if(foundOutputTable) {
					stringsToRemove.add(nextDatabase);
				}
			}
			// now run through any database names to remove (i.e. databases that
			// contain an output table)
			for(int i = 0; i < stringsToRemove.size(); i++) {
				databases.remove(stringsToRemove.get(i));
			}
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				String nextDB = (String)i.next();
				outputDatabaseCombo.addItem(nextDB);
				newDatabaseCombo.addItem(nextDB);
			}

			Vector<String> toolTipVector = new Vector<String>();
			for(int i=0; i<outputDatabaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) outputDatabaseCombo.getItemAt(i));
			}
			String[] toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			outputDatabaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));

			toolTipVector = new Vector<String>();
			for(int i=0; i<newDatabaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) newDatabaseCombo.getItemAt(i));
			}
			toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			newDatabaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show tables from database in Profile Weight Script GUI.");
		}
		// set the default selection
		outputDatabaseCombo.setSelectedItem("");
		newDatabaseCombo.setSelectedItem("");
	}
}
