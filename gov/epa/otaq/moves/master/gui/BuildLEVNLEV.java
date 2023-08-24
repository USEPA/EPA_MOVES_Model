/**************************************************************************************************
 * @(#)BuildLEVNLEV.java
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
 * Select and execute a database builder script given the current default
 * database and a new database to be created.
 * @author		Design Team
 * @author		Mike Kender (Task 2003)
 * @version 	2022-11-16
**/
public class BuildLEVNLEV extends JDialog implements ActionListener {
	/** Mode building a LEV input database **/
	public static final int MOVES4_MyLEVs = 0;
	/** Mode building an NLEV input database **/
	public static final int MOVES4_MyNLEVs = 1;

	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** keeps track of the current mode (and sets default conversion mode) **/
	int mode = MOVES4_MyNLEVs;
	/** Textual display of the current mode **/
	String modeText = "NLEV";

	/** Instructions display **/
	JTextPane instructionsTextPane;
	/** Button to refresh the list of databases **/
	JButton refreshButton;
	/** Path to the template file **/
	JLabel templateFileText;
	/** Button to open the template file **/
	JButton openTemplateFileButton;
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
	/** Button to open instructions **/
	JButton openInstructionsButton;

	/** Type of the control file, Text, XLS, etc **/
	String controlFileType = "";
	/** The worksheet name within the control file if it is XLS type **/
	String controlFileWorksheetName = "";


	/** Name of the server holding the default, input, and new databases. **/
	JLabel serverLabel;
	/** Database to be created to hold the converted data. **/
	ExtendedComboBox<String> newDatabaseCombo;

	/**
	 * Browser dialog for selecting an output directory.  Retained as a member
	 * variable so it would retain the last used directory.
	**/
	JFileChooser folderChooser = null;

	/**
	 * Constructs the LEV/NLEV builder window, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	 * @param modeToUse Default mode to use
	**/
	public BuildLEVNLEV(JFrame parent, int modeToUse) {
		super(parent, modeToUse == MOVES4_MyLEVs ? "Build LEV Database" : "Build NLEV Database");
		frame = parent;
		mode = modeToUse;
		switch(mode) {
			case MOVES4_MyLEVs:
				modeText = "LEV";
				break;
			case MOVES4_MyNLEVs:
				modeText = "NLEV";
				break;			
		}
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(true);
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		loadDatabases();

		pack();
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(770,500);
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
		templateFileText = new JLabel("",JLabel.RIGHT);
		openTemplateFileButton = new JButton();
		controlFileText = new JLabel("",JLabel.RIGHT);
		browseControlFileButton = new JButton(); 
		JLabel label2 = new JLabel();
		prefixText = new JTextField();
		outputDirectoryText = new JLabel("",JLabel.RIGHT);
		JLabel label5 = new JLabel();
		createRunSpecsButton = new JButton();
		doneButton = new JButton();
		openInstructionsButton = new JButton();
		
		openTemplateFileButton.addActionListener(this);
		browseControlFileButton.addActionListener(this);
		createRunSpecsButton.addActionListener(this);
		doneButton.addActionListener(this);
		openInstructionsButton.addActionListener(this);
		
		newDatabaseCombo = new ExtendedComboBox<String>();
		Dimension d = newDatabaseCombo.getPreferredSize();
		newDatabaseCombo.setPreferredSize(new Dimension(250, d.height));
		newDatabaseCombo.setPopupWidth(newDatabaseCombo.getPreferredSize().width);
		newDatabaseCombo.setName("newDatabaseCombo");
		newDatabaseCombo.setEditable(true);
		newDatabaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(newDatabaseCombo,"Enter the name of a new database to hold the " + modeText + " table");
		newDatabaseCombo.addActionListener(this);

		refreshButton = new JButton("Refresh");
		ToolTipHelper.add(refreshButton,"Refresh the list of available databases");
		refreshButton.addActionListener(this);
		refreshButton.setMnemonic('R');
		refreshButton.setDisplayedMnemonicIndex(0);

		serverLabel = new JLabel();
		ToolTipHelper.add(serverLabel,"Server that contains the default and new databases");
		serverLabel.setText(StringUtilities.safeGetString(SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName));

		setDefaultBuilderScript();

		//======== result ========
		{
			result.setLayout(new GridBagLayout());
			((GridBagLayout)result.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)result.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

			//---- instructionsTextPane ----
			switch(mode) {
				case MOVES4_MyLEVs:
					instructionsTextPane.setText(
						"This tool builds an input database to be used by states that chose to adopt California"
						+ " LEV standards in place of federal standards. This tool creates a special input"
						+ " database which contains a set of alternate HC, CO, NOx, and PM start and running"
						+ " emission rates based on EPA and CARB analysis of the LEV programs. For detailed"
						+ " steps on how to use this tool, click the \"Open Instructions\" button below."
						+ " Note: This tool does not build NLEV input databases. To model the effects of the NLEV"
						+ " standards for the 1999 and 2000 model years in Ozone Transport Commission (OTC) states,"
						+ " select \""
						+ BuildNLEVAction.NAME 
						+ "\" from the Tools menu."
						+ "\r\n\r\n"
						+ "To use this tool, open the template script file using the \"Open File\" button. Edit"
						+ " the script as described in the instructions document and save it as a new file. Select"
						+ " this new file by clicking the \"Browse\" button below. Enter the name of the new database"
						+ " to receive the LEV program emission rates data as the \"New Database\". Use the"
						+ " \"Build Database\" button to execute the script file. When it finishes, click \"Done\"."
						+ " To use the new database with this RunSpec, go to the Advanced Features panel. Expand"
						+ " the \"Database\" drop-down menu in the \"Input Data Sets\" block, and locate the"
						+ " database created using this tool. If you don't see the database, press the \"Refresh\""
						+ " button on that panel to refresh the list.");
					break;
				case MOVES4_MyNLEVs:
					instructionsTextPane.setText(
						"This tool builds an input database to be used by Ozone Transport Commission (OTC) states"
						+ " to model the early introduction of NLEV standards in those states. This tool creates a"
						+ " special input database which contains a set of alternate HC, CO, and NOx start and"
						+ " running emission rates based on EPA analysis of the NLEV program. The input database"
						+ " provides rates only for model years 1999 and 2000. These rates replace the rates in the"
						+ " default database for these particular pollutants. For detailed steps on how to use this"
						+ " tool, click the \"Open Instructions\" button below."
						+ "\r\n\r\n"
						+ "Note: This tool does not build LEV input databases. To model the effects of a LEV"
						+ " program for any particular state, select \""
						+ BuildLEVAction.NAME 
						+ "\" from the Tools menu."
						+ "\r\n\r\n"
						+ "To use this tool, simply enter the name of the new database to receive the NLEV"
						+ " emission rates data as the \"New Database\". You do not need to change the currently"
						+ " selected builder script. Click the \"Build Database\" button to execute the script file."
						+ " When it finishes, click \"Done\"."
						+ "\r\n\r\n"
						+ "To use the new database with this RunSpec, go to the Advanced Features panel. Expand"
						+ " the \"Database\" drop-down menu in the \"Input Data Sets\" block, and locate the"
						+ " database created using this tool. If you don't see the database, press the \"Refresh\""
						+ " button on that panel to refresh the list.");
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

			//---- label0 ----
			p = new JPanel();
			p.setBorder(BorderFactory.createTitledBorder("Template Script"));
			p.setLayout(new GridBagLayout());
			((GridBagLayout)p.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 150, 0};
			((GridBagLayout)p.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)p.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};
			p.add(new JLabel("File:"), new GridBagConstraints(0,0,1,1,0.0,0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			ToolTipHelper.add(templateFileText,templateFileText.getText());
			p.add(templateFileText, new GridBagConstraints(1, 0, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- openTemplateFileButton ----
			openTemplateFileButton.setText("Open File");
			openTemplateFileButton.setMnemonic('O');
			openTemplateFileButton.setDisplayedMnemonicIndex(0);
			ToolTipHelper.add(openTemplateFileButton,"Open the template file");
			p.add(openTemplateFileButton, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			result.add(p, new GridBagConstraints(0, 2, 5, 2, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			
			// only display template block if in LEV mode
			switch(mode) {
				case MOVES4_MyLEVs:
					p.setVisible(true);
					break;
				case MOVES4_MyNLEVs:
					p.setVisible(false);
			}

			//---- label1 ----
			p = new JPanel();
			p.setBorder(BorderFactory.createTitledBorder("Builder Script"));
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

			result.add(p, new GridBagConstraints(0, 4, 5, 2, 0.0, 0.0,
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

			label5.setText("New Database:");
			p.add(label5, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			p.add(newDatabaseCombo, new GridBagConstraints(1, 2, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			result.add(p, new GridBagConstraints(0, 6, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			label5.setDisplayedMnemonic('N');
			label5.setLabelFor(newDatabaseCombo);


			//---- createRunSpecsButton ----
			createRunSpecsButton.setText("Build Database");
			ToolTipHelper.add(createRunSpecsButton,"Run the selected Builder Script, creating the new database");
			result.add(createRunSpecsButton, new GridBagConstraints(2, 12, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			createRunSpecsButton.setMnemonic('u');
			createRunSpecsButton.setDisplayedMnemonicIndex(1);
			
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
		} else if(e.getSource() == browseControlFileButton) {
			handleBrowseScriptFileButton();
		} else if(e.getSource() == createRunSpecsButton) {
			handleBuildButton();
		} else if(e.getSource() == doneButton) {
			handleDoneButton();
		} else if(e.getSource() == openTemplateFileButton) {
			handleOpenButton();
		} else if(e.getSource() == openInstructionsButton) {
			handleOpenInstructionsButton();
		}
	}
	
	/** Handle the Refresh button for databases **/
	void handleRefreshButton() {
		loadDatabases();
	}

	/** Choose the default conversion script file if it is available **/
	void setDefaultBuilderScript() {
		try {
			File file = null;
			switch(mode) {
				case MOVES4_MyLEVs:
					file = new File("database/LEV_NLEVScripts/MOVES4_MyLEVs_Template.sql");
					break;
				default:
					file = new File("database/LEV_NLEVScripts/MOVES4_MyNLEVs.sql");
					break;
			}
			if(file == null || !file.exists()) {
				return;
			}
			String filePath = file.getCanonicalPath();
			controlFileText.setText(""); // clear this in case something goes wrong and we
										 // cannot load the data
			templateFileText.setText("");
			controlFileFullPath = "";
			controlFileType = "";
			controlFileWorksheetName = "";
			controlFileType = "Text";
			
			switch(mode) {
				case MOVES4_MyLEVs:
					templateFileText.setText(file.getAbsolutePath());
					break;
				default:
					controlFileText.setText(file.getName());
					controlFileFullPath = file.getCanonicalPath();
					break;
			}
		} catch(Exception e) {
			// Nothing to do here
		}
	}	

	/** Handle the open button for the template file **/
	void handleOpenButton() {
		try {
			File file = new File(templateFileText.getText());
			if(!file.exists()) {
				Logger.log(LogMessageCategory.ERROR, "Could not find template file at: " + templateFileText.getText());
				return;
			}
			if(!OpenFile.open(file)) {
				return;
			}
		} catch(Exception e) {
			Logger.log(LogMessageCategory.ERROR, "Could not open template file: " + e);
			return;
		}
	}

	/** Handle the open button for the instructions file **/
	void handleOpenInstructionsButton() {
		try {
			File file = new File("database/LEV_NLEVScripts/InstructionsForLEV_NLEV_Tool.pdf");
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
	void handleBuildButton() {
		try {
			boolean hasError = false;
			File scriptFile = null;
			controlFileFullPath = StringUtilities.safeGetString(controlFileFullPath).trim();
			if(controlFileFullPath.length() == 0) {
				JOptionPane.showMessageDialog(null,
						"Please select a Builder Script to execute.",
						"No script selected.", JOptionPane.ERROR_MESSAGE);
				return;
			} else {
				scriptFile = new File(controlFileFullPath);
				if(!scriptFile.exists()) {
					JOptionPane.showMessageDialog(null,
							"Could not find the selected file. Please select a different script.",
							"Database Build Error", JOptionPane.ERROR_MESSAGE);
					return;
				}
			}
			
			String newDatabaseName = newDatabaseCombo.getSelectedItem().toString();
			if(newDatabaseName != null) {
				newDatabaseName = newDatabaseName.trim();
			}
			if(newDatabaseName == null || newDatabaseName.length() == 0) {
				JOptionPane.showMessageDialog(null,
						"Please select a name for the database to be built.",
						"No name selected.", JOptionPane.ERROR_MESSAGE);
				return;
			}
			
			// Run the selected script
			DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
			DatabaseSelection outputDatabase = new DatabaseSelection();
			outputDatabase.serverName = defaultDatabase.serverName;
			outputDatabase.databaseName = newDatabaseName;
			try {
				DatabaseUtilities.executeBuildScript(scriptFile,outputDatabase,defaultDatabase);
			} catch(Exception e) {
				Logger.log(LogMessageCategory.ERROR, modeText + " database build failed: " + e);
				hasError = true;
			}

			if(hasError) {
				/**
				 * @issue Unable to convert the database.
				**/
				JOptionPane.showMessageDialog(null,
						"Unable to build the " + modeText + " database. Check the MOVES log for details.",
						"Database Build Error", JOptionPane.ERROR_MESSAGE);
			} else {
				/** @nonissue **/
				JOptionPane.showMessageDialog(null,
						modeText + " Database built.",
						"Build " + modeText + " Database", JOptionPane.INFORMATION_MESSAGE);
			}
		} catch(Exception e) {
			/**
			 * @issue Unable to create RunSpecs
			 * @explain An error was encountered while the Multiple RunSpec Creator was creating new
			 * RunSpec files.  The most likely causes are insufficient permissions to the
			 * output folder or invalid characters in the file name prefix.
			**/
			Logger.logError(e,"Unable to build " + modeText + " database");
		}
	}

	/** Handle the Done button **/
	void handleDoneButton() {
		dispose();
	}

	/**
	 * Add a database name to inputDatabaseCombo and newDatabaseCombo but only if it isn't already in the
	 * the lists.
	 * @param newDatabaseName name of the database to attempt to place into the database combo boxes.
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInComboBox(String newDatabaseName) {
		String a = null;
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
		newDatabaseCombo.removeAllItems();
		// add the default item (no selection)
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
			Logger.logError(e, "Failed to show databases (load databases) in Builder.");
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
				newDatabaseCombo.addItem(nextDB);
			}

			Vector<String> toolTipVector = new Vector<String>();
			for(int i=0; i<newDatabaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) newDatabaseCombo.getItemAt(i));
			}
			String[] toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			newDatabaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show tables from database in Builder.");
		}
		// set the default selection
		newDatabaseCombo.setSelectedItem("");
	}
}
