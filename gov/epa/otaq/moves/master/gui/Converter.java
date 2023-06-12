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
 * @version 	2022-11-16
**/
public class Converter extends JDialog implements ActionListener {
	/** Mode for conversion of a 2010A CDM/PDM database into a 2010B database **/
	private static final int MODE_2010A_TO_2010B = 0; // deprecated
	/** Mode for conversion of a 2010B CDM/PDM database into a 2014 database **/
	private static final int MODE_2010B_TO_2014 = 1; // deprecated
	/** Mode for conversion of a 2014 CDM/PDM database into a 2014A database **/
	public static final int MODE_2014_TO_3 = 2; // deprecated
	/** Mode for conversion of a 2014A CDM/PDM database into a 3 database **/
	public static final int MODE_2014A_TO_3 = 3; // deprecated
	/** Mode for conversion of a MOVES3 CDM/PDM database into a MOVES4 database **/
	public static final int MODE_3_TO_4 = 4;

	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** Default conversion mode **/
	int mode = MODE_3_TO_4;

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
	/** Button to convert the selected database **/
	JButton convertDatabaseButton;
	/** Button to save messages **/
	JButton saveMessagesButton;
	/** Button to close the window **/
	JButton doneButton;
	/** Button to open help **/
	JButton openHelpButton;

	/** Type of the control file, Text, XLS, etc **/
	String controlFileType = "";
	/** The worksheet name within the control file if it is XLS type **/
	String controlFileWorksheetName = "";

	/** DefaultListModel for messageList **/
	DefaultListModel<String> messageListModel;

	/** List of messages to be shown **/
	ArrayList<String> messages = new ArrayList<String>();
	/** True if messages contains any errors that should prevent further processing **/
	boolean messagesHasError = false;

	/** Name of the server holding the default, input, and new databases. **/
	JLabel serverLabel;
	/** Database containing old data to be converted. **/
	ExtendedComboBox<String> inputDatabaseCombo;
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
	public Converter(JFrame parent, int modeToUse) {
		super(parent, "Convert Database");
		frame = parent;
		mode = modeToUse;

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
		setDefaultConversionScript();

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
		convertDatabaseButton = new JButton();
		saveMessagesButton = new JButton();
		doneButton = new JButton();
		openHelpButton = new JButton();

		messageListModel = new DefaultListModel<String>();
		messagesList = new JListWithToolTips<String>(messageListModel);
		messagesList.setSelectedIndex(-1);
		messagesList.setVisibleRowCount(6);
		messagesList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXX");
		ToolTipHelper.add(scrollPane2,"Displays messages, warnings, and errors");

		browseControlFileButton.addActionListener(this);
		saveMessagesButton.addActionListener(this);
		convertDatabaseButton.addActionListener(this);
		doneButton.addActionListener(this);
		openHelpButton.addActionListener(this);

		inputDatabaseCombo = new ExtendedComboBox<String>();
		Dimension d = inputDatabaseCombo.getPreferredSize();
		inputDatabaseCombo.setPreferredSize(new Dimension(250, d.height));
		inputDatabaseCombo.setPopupWidth(inputDatabaseCombo.getPreferredSize().width);
		inputDatabaseCombo.setName("inputDatabaseCombo");
		inputDatabaseCombo.setEditable(true);
		inputDatabaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(inputDatabaseCombo,"Edit or select the name of the database that holds the existing data to be converted");
		inputDatabaseCombo.addActionListener(this);

		newDatabaseCombo = new ExtendedComboBox<String>();
		d = newDatabaseCombo.getPreferredSize();
		newDatabaseCombo.setPreferredSize(new Dimension(250, d.height));
		newDatabaseCombo.setPopupWidth(newDatabaseCombo.getPreferredSize().width);
		newDatabaseCombo.setName("newDatabaseCombo");
		newDatabaseCombo.setEditable(true);
		newDatabaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(newDatabaseCombo,"Edit or select the name of the database to hold the converted data");
		newDatabaseCombo.addActionListener(this);

		refreshButton = new JButton("Refresh");
		ToolTipHelper.add(refreshButton,"Refresh the list of available databases");
		refreshButton.addActionListener(this);
		refreshButton.setMnemonic('R');
		refreshButton.setDisplayedMnemonicIndex(0);

		serverLabel = new JLabel();
		ToolTipHelper.add(serverLabel,"Server that contains the default, input, and new databases.");
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
			switch(mode) {
				case MODE_3_TO_4:
					doc.insertString(doc.getLength(),
						"This tool converts MOVES3 input databases for County, Project,"
						+ " and Nonroad runs into the MOVES4 format. "
						+ "\r\n\r\n"
						+ "Use the default conversion script listed below unless you have a customized"
						+ " conversion script to use instead. In this advanced use case, use the \"Browse\""
						+ " button below to select your customized script."
						+ "\r\n\r\n"
						+ "To use this tool, select a MOVES3 input database from the"
						+ " \"Input Database\" drop-down list below. Then enter the name of a new database"
						+ " to receive the converted data as the \"New Database\". Use the \"Convert Database\""
						+ " button to execute the script file.  When you've converted all the databases needed," 
						+ " click \"Done\"."
						+ "\r\n\r\n"
						+ "To use a converted database with this RunSpec, select your new database from the"
						+ " drop-down list on the Create Input Database Panel. If it does not automatically" 
						+ " appear in the list, you may need to click the \"Refresh\" button on that panel" 
						+ " first."
						+ "\r\n\r\n",
						normal);
					doc.insertString(doc.getLength(),
						"Note that additional work is needed before using the converted input databases"
						+ " with MOVES4. Click the \"Open Help\" button for more information.",
						bold);
					break;
				default:
					instructionsTextPane.setText(
								"An error occurred and the instructions could not be loaded."
							);
					break;
			}
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
			p.setBorder(BorderFactory.createTitledBorder("Conversion Script"));
			p.setLayout(new GridBagLayout());
			((GridBagLayout)p.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 150, 0};
			((GridBagLayout)p.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)p.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};
			p.add(new JLabel("File:"), new GridBagConstraints(0,0,1,1,0.0,0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			ToolTipHelper.add(controlFileText,"Name and location of the script file (.sql)");
			p.add(controlFileText, new GridBagConstraints(1, 0, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- browseControlFileButton ----
			browseControlFileButton.setText("Browse...");
			browseControlFileButton.setMnemonic('B');
			browseControlFileButton.setDisplayedMnemonicIndex(0);
			ToolTipHelper.add(browseControlFileButton,"Select a script file (.sql)");
			p.add(browseControlFileButton, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			result.add(p, new GridBagConstraints(0, 2, 5, 2, 0.0, 0.0,
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

			label3.setText("Input Database:");
			p.add(label3, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			p.add(inputDatabaseCombo, new GridBagConstraints(1, 1, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			label3.setDisplayedMnemonic('I');
			label3.setLabelFor(inputDatabaseCombo);

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

			//---- convertDatabaseButton ----
			convertDatabaseButton.setText("Convert Database");
			ToolTipHelper.add(convertDatabaseButton,"Apply the script file to the input database, creating the new database");
			result.add(convertDatabaseButton, new GridBagConstraints(0, 10, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			convertDatabaseButton.setMnemonic('C');
			convertDatabaseButton.setDisplayedMnemonicIndex(0);
			
			//---- saveMessagesButton ----
			saveMessagesButton.setText("Save Messages");
			ToolTipHelper.add(saveMessagesButton,"Save above messages as a text file.");
			result.add(saveMessagesButton, new GridBagConstraints(2, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
                saveMessagesButton.setMnemonic('S');
                saveMessagesButton.setDisplayedMnemonicIndex(0);

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
		} else if(e.getSource() == browseControlFileButton) {
			handleBrowseScriptFileButton();
		} else if(e.getSource() == convertDatabaseButton) {
			handleConvertButton();
		} else if(e.getSource() == saveMessagesButton) {
			handleSaveMessagesButton();
		} else if(e.getSource() == openHelpButton) {
			handleOpenHelpButton();
		}else if(e.getSource() == doneButton) {
			handleDoneButton();
		} 
	}

	/** Handle the Refresh button for databases **/
	void handleRefreshButton() {
		loadDatabases();
	}

	/** Choose the default conversion script file if it is available **/
	void setDefaultConversionScript() {
		try {
			File file = null;
			switch(mode) {
				case MODE_3_TO_4:
					file = new File("database/ConversionScripts/Convert_MOVES3_input_to_MOVES4.sql");
					break;
			}
			if(file == null || !file.exists()) {
				return;
			}
			String filePath = file.getCanonicalPath();
			controlFileText.setText(""); // clear this in case something goes wrong and we
										 // cannot load the data
			controlFileFullPath = "";
			controlFileType = "";
			controlFileWorksheetName = "";

			controlFileType = "Text";

			controlFileText.setText(file.getName());
			controlFileFullPath = file.getCanonicalPath();
		} catch(Exception e) {
			// Nothing to do here
		}
	}

	/** Handle the Browse button for the script file **/
	void handleBrowseScriptFileButton() {
		try {
			FileDialog fd = new FileDialog(frame, "Select Script File", FileDialog.LOAD);
			fd.setVisible(true);

			if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
				return;
			}
			String filePath = fd.getDirectory() + fd.getFile();
			File file = new File(filePath);
			if(!file.exists()) {
				return;
			}
			filePath = file.getCanonicalPath();
			controlFileText.setText(""); // clear this in case something goes wrong and we
										 // cannot load the data
			controlFileFullPath = "";
			controlFileType = "";
			controlFileWorksheetName = "";

			controlFileType = "Text";

			controlFileText.setText(file.getName());
			controlFileFullPath = file.getCanonicalPath();
		} catch(Exception e) {
			// Nothing to do here
		}
	}

	/** Handle the Convert Databases button **/
	void handleConvertButton() {
		try {
			resetMessages();

			File scriptFile = null;
			controlFileFullPath = StringUtilities.safeGetString(controlFileFullPath).trim();
			if(controlFileFullPath.length() == 0) {
				messages.add("Specify a conversion script.");
				messagesHasError = true;
			} else {
				scriptFile = new File(controlFileFullPath);
				if(!scriptFile.exists()) {
					messages.add("Unable to find the conversion script file.");
					messagesHasError = true;
				}
			}

			String inputDatabaseName = inputDatabaseCombo.getSelectedItem().toString();
			if(inputDatabaseName != null) {
				inputDatabaseName = inputDatabaseName.trim();
			}
			if(inputDatabaseName == null || inputDatabaseName.length() == 0) {
				messages.add("Specify an input database name.");
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
				if(newDatabaseName.equalsIgnoreCase(inputDatabaseName)) {
					messages.add("The input database and new database cannot be the same.");
					messagesHasError = true;
				}
			}

			if(messagesHasError) {
				return;
			}

			// Convert
			DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];

			DatabaseSelection inputDatabase = new DatabaseSelection();
			inputDatabase.serverName = defaultDatabase.serverName;
			inputDatabase.databaseName = inputDatabaseName;

			DatabaseSelection outputDatabase = new DatabaseSelection();
			outputDatabase.serverName = defaultDatabase.serverName;
			outputDatabase.databaseName = newDatabaseName;

			try {
				DatabaseUtilities.executeConversionScript(scriptFile,outputDatabase,inputDatabase,defaultDatabase,messages);
				messages.add("Conversion successful.");
			} catch(Exception e) {
				messages.add("Conversion failed: " + e.getMessage());
				messagesHasError = true;
			}

			if(messagesHasError) {
				/**
				 * @issue Unable to convert the database.
				**/
				JOptionPane.showMessageDialog(null,
						"Unable to convert the database. Check the messages list for details.",
						"Convert Database", JOptionPane.ERROR_MESSAGE);
			} else {
				/** @nonissue **/
				JOptionPane.showMessageDialog(null,
						"Database converted.",
						"Convert Database", JOptionPane.INFORMATION_MESSAGE);
			}
		} catch(Exception e) {
			/**
			 * @issue Unable to create RunSpecs
			 * @explain An error was encountered while the Multiple RunSpec Creator was creating new
			 * RunSpec files.  The most likely causes are insufficient permissions to the
			 * output folder or invalid characters in the file name prefix.
			**/
			Logger.logError(e,"Unable to convert database");
		} finally {
			populateMessagesList();
		}
	}

	/** Handle the Save Messages button **/
	void handleSaveMessagesButton() {
        FileDialog fd = new FileDialog(frame, "Specify save file name (*.txt):", FileDialog.SAVE);
        fd.setFile("*.txt");
        fd.setVisible(true);

        if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
            return;
        }
        String filePath = fd.getDirectory() + fd.getFile();
        
        try {
            FileWriter writer = new FileWriter(filePath);
            for(Iterator i=messages.iterator();i.hasNext();) {
                writer.write((String)i.next() + "\r\n");
            }                
            writer.close();
        } catch (IOException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

	/** Handle the open button for the help file **/
	void handleOpenHelpButton() {
		try {
			File file = new File("database/ConversionScripts/InputDatabaseConversionHelp.pdf");
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
        saveMessagesButton.setEnabled(messageListModel.size() > 0);
	}

	/**
	 * Add a database name to inputDatabaseCombo and newDatabaseCombo but only if it isn't already in the
	 * the lists.
	 * @param newDatabaseName name of the database to attempt to place into the database combo boxes.
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInComboBox(String newDatabaseName) {
		String a = addIfNotInComboBox(newDatabaseName,inputDatabaseCombo);
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
		inputDatabaseCombo.removeAllItems();
		newDatabaseCombo.removeAllItems();
		// add the default item (no selection)
		inputDatabaseCombo.addItem(new String(""));
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
			Logger.logError(e, "Failed to show databases (load databases) in Converter.");
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
							if(nextTable.equalsIgnoreCase("AGECATEGORY") || nextTable.equalsIgnoreCase("MOVESOUTPUT")) {
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
				inputDatabaseCombo.addItem(nextDB);
				newDatabaseCombo.addItem(nextDB);
			}

			Vector<String> toolTipVector = new Vector<String>();
			for(int i=0; i<inputDatabaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) inputDatabaseCombo.getItemAt(i));
			}
			String[] toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			inputDatabaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));

			toolTipVector = new Vector<String>();
			for(int i=0; i<newDatabaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) newDatabaseCombo.getItemAt(i));
			}
			toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			newDatabaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show tables from database in Converter.");
		}
		// set the default selection
		inputDatabaseCombo.setSelectedItem("");
		newDatabaseCombo.setSelectedItem("");
	}
}
