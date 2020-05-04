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
import java.io.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.*;

/**
 * Select and execute a database conversion script given the current default
 * database, an existing input database, and a new database to be created.
 * @author		Wesley Faler
 * @version		2015-05-20
**/
public class Converter extends JDialog implements ActionListener {
	/** Mode for conversion of a 2010A CDM/PDM database into a 2010B database **/
	public static final int MODE_2010A_TO_2010B = 0;
	/** Mode for conversion of a 2010B CDM/PDM database into a 2014 database **/
	public static final int MODE_2010B_TO_2014 = 1;
	/** Mode for conversion of a 2014 CDM/PDM database into a 2014A database **/
	public static final int MODE_2014_TO_2014A = 2;

	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** Default conversion mode **/
	int mode = MODE_2010A_TO_2010B;

	/** Instructions display **/
	JTextPane instructionsTextPane;
	/** Button to create a control file template **/
	JButton createTemplateButton;
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
	/** Button to browse for the output directory **/
	JButton browseOutputButton;
	/** List of messages **/
	JList<String> messagesList;
	/** Button to create RunSpecs from the control file **/
	JButton createRunSpecsButton;
	/** Button to close the window **/
	JButton doneButton;

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
		createTemplateButton = new JButton();
		controlFileText = new JLabel("",JLabel.RIGHT);
		browseControlFileButton = new JButton();
		JLabel label2 = new JLabel();
		prefixText = new JTextField();
		JLabel label3 = new JLabel();
		outputDirectoryText = new JLabel("",JLabel.RIGHT);
		browseOutputButton = new JButton();
		JLabel label4 = new JLabel();
		JLabel label5 = new JLabel();
		JScrollPane scrollPane2 = new JScrollPane();
		createRunSpecsButton = new JButton();
		doneButton = new JButton();

		messageListModel = new DefaultListModel<String>();
		messagesList = new JListWithToolTips<String>(messageListModel);
		messagesList.setSelectedIndex(-1);
		messagesList.setVisibleRowCount(6);
		messagesList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXX");
		ToolTipHelper.add(scrollPane2,"Displays messages, warnings, and errors");

		createTemplateButton.addActionListener(this);
		browseControlFileButton.addActionListener(this);
		browseOutputButton.addActionListener(this);
		createRunSpecsButton.addActionListener(this);
		doneButton.addActionListener(this);

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

		serverLabel = new JLabel();
		ToolTipHelper.add(serverLabel,"Server that contains the default, input, and new databases.");
		serverLabel.setText(StringUtilities.safeGetString(SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName));

		//======== result ========
		{
			result.setLayout(new GridBagLayout());
			((GridBagLayout)result.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)result.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

			//---- instructionsTextPane ----
			switch(mode) {
				case MODE_2014_TO_2014A:
					instructionsTextPane.setText(
								"This tool converts MOVES 2014 County Domain and"
								+ " Project Domain databases into the MOVES2014A"
								+ " format."
								+ "\r\n\r\n"
								+ "Use the \"Browse\" button to select a database"
								+ " conversion script file, such as the standard"
								+ " file Convert2014_CDM_PDM.sql located in the"
								+ " database/ConversionScripts directory."
								+ "\r\n\r\n"
								+ "Select a MOVES 2014 County Domain or Project"
								+ " Domain database as the \"Input Database\"."
								+ " This database must be on the same server as the"
								+ " MOVES 2014A default database in order for the"
								+ " script to copy required data."
								+ "\r\n\r\n"
								+ "Enter the name of a new database to receive"
								+ " the converted data as the \"New Database\"."
								+ "\r\n\r\n"
								+ "Use the \"Convert Database\" button to execute"
								+ " the script file.  When you've converted all the"
								+ " databases you care to, click \"Done\"."
								+ "\r\n\r\n"
								+ "Note that additional work will be needed before "
								+ "converted MOVES2014 input databases can be used "
								+ "in MOVES2014A for SIP and conformity purposes. "
								+ "All MOVES2014 defaults in the input database "
								+ "(especially default fuel information) should be "
								+ "replaced with MOVES2014A defaults."
							);
					break;
				case MODE_2010B_TO_2014:
					instructionsTextPane.setText(
								"This tool converts MOVES 2010B County Domain and"
								+ " Project Domain databases into the MOVES2014"
								+ " format."
								+ "\r\n\r\n"
								+ "Use the \"Browse\" button to select a database"
								+ " conversion script file, such as the standard"
								+ " file Convert2010B_CDM_PDM.sql located in the"
								+ " database/ConversionScripts directory."
								+ "\r\n\r\n"
								+ "Select a MOVES 2010B County Domain or Project"
								+ " Domain database as the \"Input Database\"."
								+ " This database must be on the same server as the"
								+ " MOVES 2014 default database in order for the"
								+ " script to copy required data."
								+ "\r\n\r\n"
								+ "Enter the name of a new database to receive"
								+ " the converted data as the \"New Database\"."
								+ "\r\n\r\n"
								+ "Use the \"Convert Database\" button to execute"
								+ " the script file.  When you've converted all the"
								+ " databases you care to, click \"Done\"."
								+ "\r\n\r\n"
								+ "Note that additional work will be needed before "
								+ "converted MOVES2010B input databases can be used "
								+ "in MOVES2014 for SIP and conformity purposes. "
								+ "All MOVES2010B defaults in the input database "
								+ "(especially default fuel information) should be "
								+ "replaced with MOVES2014 defaults. Additionally, "
								+ "the monthVMTFraction table may need to be re-imported "
								+ "if a leap-year is being modeled."
							);
					break;
				default:
					instructionsTextPane.setText(
								"This tool converts MOVES 2010A County Domain and"
								+ " Project Domain databases into the MOVES2010B"
								+ " format."
								+ "\r\n\r\n"
								+ "Use the \"Browse\" button to select a database"
								+ " conversion script file, such as the standard"
								+ " file Convert2010A_CDM_PDM.sql located in the"
								+ " database/ConversionScripts directory."
								+ "\r\n\r\n"
								+ "Select a MOVES 2010A County Domain or Project"
								+ " Domain database as the \"Input Database\"."
								+ " This database must be on the same server as the"
								+ " MOVES 2010B default database in order for the"
								+ " script to copy required data."
								+ "\r\n\r\n"
								+ "Enter the name of a new database to receive"
								+ " the converted data as the \"New Database\"."
								+ "\r\n\r\n"
								+ "Use the \"Convert Database\" button to execute"
								+ " the script file.  When you've converted all the"
								+ " databases you care to, click \"Done\"."
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

			//---- createRunSpecsButton ----
			createRunSpecsButton.setText("Convert Database");
			ToolTipHelper.add(createRunSpecsButton,"Apply the script file to the input database, creating the new database");
			result.add(createRunSpecsButton, new GridBagConstraints(2, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- doneButton ----
			doneButton.setText("Done");
			ToolTipHelper.add(doneButton,"Close this window");
			result.add(doneButton, new GridBagConstraints(4, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
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
		} else if(e.getSource() == createRunSpecsButton) {
			handleConvertButton();
		} else if(e.getSource() == doneButton) {
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
				case MODE_2014_TO_2014A:
					file = new File("database/ConversionScripts/Convert2014_CDM_PDM.sql");
					break;
				case MODE_2010B_TO_2014:
					file = new File("database/ConversionScripts/Convert2010B_CDM_PDM.sql");
					break;
				default:
					file = new File("database/ConversionScripts/Convert2010A_CDM_PDM.sql");
					break;
			}
			if(!file.exists()) {
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

	/** Handle the Create RunSpecs button **/
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
	 * Loads the Databases droplist, based on the server setting.
	 * Also, sets the default droplist selection, if it can be found.
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
		String sql = "SHOW DATABASES";
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
