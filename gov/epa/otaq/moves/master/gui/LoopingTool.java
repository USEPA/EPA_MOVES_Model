/**************************************************************************************************
 * @(#)LoopingTool.java
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
 * Creates multiple RunSpecs from the currently loaded RunSpec and
 * a control file.
 * @author		Wesley Faler
 * @author  	Bill Shaw (508 compliance mods)
 * @version		2011-09-10
**/
public class LoopingTool extends JDialog implements ActionListener {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;

	/** Instructions display **/
	JTextPane instructionsTextPane;
	/** Button to create a control file template **/
	JButton createTemplateButton;
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

	/** RunSpec for basis of created RunSpecs **/
	RunSpec runSpec;
	/** True if the runSpec is ready or at least has no unready sections **/
	boolean isRunSpecReady = false;

	/**
	 * Browser dialog for selecting an output directory.  Retained as a member
	 * variable so it would retain the last used directory.
	**/
	JFileChooser folderChooser = null;

	/**
	 * Constructs the FERC panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	 * @param isRunSpecReadyToUse True if the runSpec is ready or at least has no unready sections
	**/
	public LoopingTool(JFrame parent, RunSpec runSpecToUse,
			boolean isRunSpecReadyToUse) {
		super(parent, MOVESWindow.MOVES_VERSION + " - Multiple RunSpec Creator");
		frame = parent;
		runSpec = runSpecToUse;
		isRunSpecReady = isRunSpecReadyToUse;

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(true);
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		resetMessages();
		checkRunSpec();
		populateMessagesList();

		pack();
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(500,575);
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

		//======== result ========
		{
			result.setLayout(new GridBagLayout());
			((GridBagLayout)result.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)result.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)result.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

			//---- instructionsTextPane ----
			instructionsTextPane.setText(
					  "This tool creates new RunSpec files using the currently loaded "
					+ "information as a template, making substitutions for county, year, "
					+ "and database selections driven by a user-supplied control file. "
					+ "In addition, a BAT file is generated that illustrates how to "
					+ "automatically execute each created RunSpec from the command line."
					+ "\r\n\r\n"
					+ "Use the \"Create Template...\" button to get started with "
					+ "a control file that uses all the counties and years in the "
					+ "current RunSpec."
					+ "\r\n\r\n"
					+ "The final BAT file, located in your output directory, can be "
					+ "run as is, or can be edited manually for distribution onto "
					+ "multiple MOVES machines."
					);
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
			p.setBorder(BorderFactory.createTitledBorder("RunSpec Template Control File"));
			p.setLayout(new GridBagLayout());
			((GridBagLayout)p.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 150, 0};
			((GridBagLayout)p.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)p.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};
			/*
			label1.setText("RunSpec Template Control File:");
			result.add(label1, new GridBagConstraints(0, 2, 4, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			*/
			p.add(new JLabel("File:"), new GridBagConstraints(0,0,1,1,0.0,0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- createTemplateButton ----
			createTemplateButton.setText("Create Template...");
			createTemplateButton.setMnemonic('T');
			createTemplateButton.setDisplayedMnemonicIndex(7);
			ToolTipHelper.add(createTemplateButton,"Create a template a file (.xls, .txt, .csv) to be filled and then used.");
			/*
			result.add(createTemplateButton, new GridBagConstraints(4, 2, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			*/
			p.add(createTemplateButton, new GridBagConstraints(4, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			ToolTipHelper.add(controlFileText,"Name and location of the control file (.xls, .txt, .csv)");
			/*
			result.add(controlFileText, new GridBagConstraints(0, 3, 4, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			*/
			p.add(controlFileText, new GridBagConstraints(1, 0, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- browseControlFileButton ----
			browseControlFileButton.setText("Browse...");
			browseControlFileButton.setMnemonic('B');
			browseControlFileButton.setDisplayedMnemonicIndex(0);
			ToolTipHelper.add(browseControlFileButton,"Select a control file (.xls, .txt, .csv)");
			/*
			result.add(browseControlFileButton, new GridBagConstraints(4, 3, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			*/
			p.add(browseControlFileButton, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			result.add(p, new GridBagConstraints(0, 2, 5, 2, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//---- label2 ----
			p = new JPanel();
			p.setBorder(BorderFactory.createTitledBorder("File Name Prefix"));
			p.setLayout(new GridBagLayout());
			((GridBagLayout)p.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)p.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

			label2.setText("Prefix:");
			label2.setDisplayedMnemonic('P');
			label2.setLabelFor(prefixText);
			/*
			result.add(label2, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			*/
			p.add(label2, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			ToolTipHelper.add(prefixText,"Name prefix to be applied to each created RunSpec");
			/*
			result.add(prefixText, new GridBagConstraints(1, 4, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			*/
			p.add(prefixText, new GridBagConstraints(2, 0, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			result.add(p, new GridBagConstraints(0, 4, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- label3 ----
			p = new JPanel();
			p.setBorder(BorderFactory.createTitledBorder("RunSpec Output Directory"));
			p.setLayout(new GridBagLayout());
			((GridBagLayout)p.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 150, 0};
			((GridBagLayout)p.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)p.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
			((GridBagLayout)p.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

			label3.setText("Folder:");
			/*
			result.add(label3, new GridBagConstraints(0, 5, 4, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			*/
			p.add(label3, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			ToolTipHelper.add(outputDirectoryText,"A directory to hold the created RunSpecs");
			/*
			result.add(outputDirectoryText, new GridBagConstraints(0, 6, 4, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
			*/
			p.add(outputDirectoryText, new GridBagConstraints(1, 0, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- browseOutputButton ----
			browseOutputButton.setText("Browse...");
			browseOutputButton.setMnemonic('r');
			browseOutputButton.setDisplayedMnemonicIndex(1);
			ToolTipHelper.add(browseOutputButton,"Choose a directory to hold the created RunSpecs");
			/*
			result.add(browseOutputButton, new GridBagConstraints(4, 6, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
			*/
			p.add(browseOutputButton, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			result.add(p, new GridBagConstraints(0, 5, 5, 2, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//---- label4 ----
			label4.setText("Messages:");
			result.add(label4, new GridBagConstraints(0, 7, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

//			label4.setDisplayedMnemonic('M');
//			label4.setLabelFor(messagesList);
			//======== scrollPane2 ========
			{
				scrollPane2.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
				scrollPane2.setViewportView(messagesList);
			}
			result.add(scrollPane2, new GridBagConstraints(0, 8, 5, 2, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//---- createRunSpecsButton ----
			createRunSpecsButton.setText("Create RunSpecs");
			createRunSpecsButton.setMnemonic('R');
			createRunSpecsButton.setDisplayedMnemonicIndex(7);
			ToolTipHelper.add(createRunSpecsButton,"Apply the control file to the current RunSpec, generating new RunSpecs");
			result.add(createRunSpecsButton, new GridBagConstraints(2, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- doneButton ----
			doneButton.setText("Done");
			doneButton.setMnemonic('D');
			doneButton.setDisplayedMnemonicIndex(0);
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
		if(e.getSource() == createTemplateButton) {
			handleCreateTemplateButton();
		} else if(e.getSource() == browseControlFileButton) {
			handleBrowseControlFileButton();
		} else if(e.getSource() == browseOutputButton) {
			handleBrowseOutputButton();
		} else if(e.getSource() == createRunSpecsButton) {
			handleCreateRunSpecsButton();
		} else if(e.getSource() == doneButton) {
			handleDoneButton();
		}
	}

	/** Handle the Create Template button **/
	void handleCreateTemplateButton() {
		FileDialog fd = new FileDialog(frame,
				"Create Control File Template", FileDialog.SAVE);
		fd.setVisible(true); //fd.show();
		if((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File file = new File(filePath);

		CellFileWriter writer = null;
		try {
			// Create the primary destination file
			writer = new CellFileWriter(file,"LoopingTool");
			String[] headers = {
				"CountyID", "County Description",
				"Year", "Additional Text Name",
				"Primary MOVES DB",
				"Output DB",
				"Domain DB",
				"Advanced Features DB",
				"User DB"
			};
			for(int i=0;i<headers.length;i++) {
				writer.writeTextCell(headers[i]);
			}
			writer.endRow();

			// Determine the primary database to be written in the template
			String primaryDB = runSpec.inputDatabase == null? ""
					: runSpec.inputDatabase.databaseName;
			if(primaryDB == null || primaryDB.length() <= 0) {
				primaryDB = SystemConfiguration.theSystemConfiguration.
						databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
			}

			// Write records for all combinations of county and year present in the RunSpec.
			for(Iterator<Integer> yi=runSpec.timeSpan.years.iterator();yi.hasNext();) {
				Integer year = (Integer)yi.next();
				for(Iterator<GeographicSelection> gi=runSpec.geographicSelections.iterator();
						gi.hasNext();) {
					GeographicSelection geo = (GeographicSelection)gi.next();
					if(geo.type != GeographicSelectionType.COUNTY) {
						continue;
					}
					writer.writeIntCell(geo.databaseKey);
					writer.writeTextCell(geo.textDescription);
					writer.writeIntCell(year.intValue());
					writer.writeTextCell("");
					writer.writeTextCell(primaryDB);
					writer.writeTextCell(runSpec.outputDatabase == null? ""
							: runSpec.outputDatabase.databaseName);
					writer.writeTextCell(runSpec.scaleInputDatabase == null? ""
							: runSpec.scaleInputDatabase.databaseName);
					writer.writeTextCell(runSpec.generatorDatabase == null? ""
							: runSpec.generatorDatabase.databaseName);
					for(Iterator<DatabaseSelection> ui=
							runSpec.databaseSelectionInputSets.iterator();ui.hasNext();) {
						DatabaseSelection userDB = (DatabaseSelection)ui.next();
						if(userDB != null && userDB.databaseName.length() > 0) {
							writer.writeTextCell(userDB.databaseName);
						}
					}
					writer.endRow();
				}
			}
		} catch(Exception e) {
			/**
			 * @issue Unable to create the control file template.
			 * @explain An error was encountered while creating a template file for use
			 * with the Multiple RunSpec Creator.  Check that the template file's name and location
			 * are valid and that you have security permissions to write into the
			 * template file's directory.
			**/
			JOptionPane.showMessageDialog(null,
					"Unable to create the control file template.",
					"Multiple RunSpec Creator", JOptionPane.ERROR_MESSAGE);
		} finally {
			if(writer != null) {
				writer.close();
			}
		}
	}

	/** Handle the Browse button for the control file **/
	void handleBrowseControlFileButton() {
		try {
			FileDialog fd = new FileDialog(frame, "Select Control File", FileDialog.LOAD);
			fd.setVisible(true); //fd.show();

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

			String extension = FileUtilities.getFileExtension(file,true);
			if(extension.equalsIgnoreCase(".xls")) {
				// Microsoft Excel-format file, so prompt the user to select a worksheet
				XLSReader xls = new XLSReader();
				String contents = null;
				try {
					ArrayList<String> worksheets = xls.getSheets(file);
					if(worksheets.size() == 1) {
						controlFileWorksheetName = (String)worksheets.get(0);
					} else {
						// Prompt the user to select a worksheet then fill
						// controlFileWorksheetName.  If the user cancels the worksheet
						// selection, stop the operation
						WorksheetChooserDialog dlg =
								new WorksheetChooserDialog(frame,worksheets);
						// simple offset from main window origin
						dlg.setLocation(getLocationOnScreen().x + 50, getLocationOnScreen().y + 50);
						dlg.showModal();
						if(dlg.selectedWorksheetName != null
								&& dlg.selectedWorksheetName.length() > 0) {
							controlFileWorksheetName = dlg.selectedWorksheetName;
						} else {
							return;
						}
					}
				} catch(Exception e) {
					throw new IOException(e.toString());
				}
				controlFileType = "XLS";
			} else { // Likely a text file
				controlFileType = "Text";
			}

			controlFileText.setText(file.getName());
			controlFileFullPath = file.getCanonicalPath();
		} catch(Exception e) {
			// Nothing to do here
		}
	}

	/* Handle the Browse button for the output directory **/
	void handleBrowseOutputButton() {
		if(folderChooser == null) {
			folderChooser = new JFileChooser();
			folderChooser.setCurrentDirectory(new java.io.File("."));
			folderChooser.setDialogTitle("Select RunSpec Output Directory");
			folderChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			folderChooser.setAcceptAllFileFilterUsed(false);
		}
		if(folderChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			File f = folderChooser.getSelectedFile();
			if(f != null) {
				try {
					outputDirectoryText.setText(f.getName());
					outputDirectoryFullPath = f.getCanonicalPath();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
	}

	/** Class representing one line within the control file **/
	class ControlFileEntry {
		public boolean isValid = false;

		public int lineNumber = 0;
		public int countyID = 0;
		public String countyDescription = "";
		public int year = 0;
		public String additionalText = "";
		public String primaryDB = "";
		public String outputDB = "";
		public String domainDB = "";
		public String advancedFeaturesDB = "";
		// list of DatabaseSelection objects
		public ArrayList<DatabaseSelection> userDBs = new ArrayList<DatabaseSelection>();

		public boolean isExistingCounty = false;

		/**
		 * Produce a pipe-separated list of all databases that provide input
		 * to the entry.  The domain database's inclusion is options, presumably
		 * predicated upon use of the single county or project domain.
		 * @param includeDomainDatabase true if the domain database should appear
		 * in the list
		 * @return a pipe-separated list of all databases the provide input
		 * to the entry
		**/
		public String getInputDatabaseKey(boolean includeDomainDatabase) {
			String result = StringUtilities.safeGetString(primaryDB) + "|";
			if(includeDomainDatabase) {
				result += StringUtilities.safeGetString(domainDB);
			}
			for(Iterator<DatabaseSelection> i=userDBs.iterator();i.hasNext();) {
				DatabaseSelection d = (DatabaseSelection)i.next();
				result += "|" + d.databaseName;
			}
			return result;
		}
		/**
		 * Read the current line's cells into the entry.  If the current line is
		 * empty, false will be returned.  If the line is not empty, true will be
		 * returned even if isValid gets set to false.
		 * @param reader file to be read
		 * @param lineNumberToUse line number within the file
		 * @return true if this object should be retained, false if it should be
		 * discarded.
		**/
		public boolean read(CellFileReader reader, int lineNumberToUse) {
			lineNumber = lineNumberToUse;
			isValid = false;
			String s = null;
			Integer t = null;

			// Make sure the line is not blank
			try {
				s = reader.readStringCell();
				if(s == null || s.length() <= 0) {
					return false;
				}
				reader.backupOneCell();
			} catch(Exception e) {
				return false;
			}

			try {
				t = reader.readIntegerCell();
				if(t != null) {
					countyID = t.intValue();
				}
			} catch(Exception e) {
				// Nothing to do here
			}
			if(t == null || countyID < 1 || countyID > 99999) {
				/**
				 * @issue Error: CountyID must be an integer in the range 1-99999, line #
				 * @explain The Multiple RunSpec Creator was unable to read the county FIPS ID at
				 * the given line number in the control file.
				**/
				messages.add("Error: CountyID must be an integer in the range 1-99999, line "
						+ lineNumber);
				messagesHasError = true;
				return true;
			}

			try {
				countyDescription = StringUtilities.safeGetString(reader.readStringCell());
			} catch(Exception e) {
				/**
				 * @issue Error: Unable to read county description, line #
				 * @explain The Multiple RunSpec Creator was unable to read the county description at
				 * the given line number in the control file.
				**/
				messages.add("Error: Unable to read county description, line " + lineNumber);
				messagesHasError = true;
				return false;
			}
			// If the county already exists in the database, use the database's
			// countyName instead of the description provided in the spreadsheet.
			isExistingCounty = false;
			String sql = "select countyName, stateName"
					+ " from county inner join state on (county.stateID = state.stateID)"
					+ " where countyID=" + countyID;
			SQLRunner.Query query = new SQLRunner.Query();
			try {
				query.open(DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT),sql);
				if(query.rs.next()) {
					String dbCountyName = query.rs.getString(1);
					String dbStateName = query.rs.getString(2);
					if(dbCountyName != null && dbCountyName.length() > 0
							&& dbStateName != null && dbStateName.length() > 0) {
						countyDescription = dbStateName + " - " + dbCountyName;
						isExistingCounty = true;
					}
				}
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to check for existing county definition",sql);
			} finally {
				query.onFinally();
			}

			t = null;
			try {
				t = reader.readIntegerCell();
				if(t != null) {
					year = t.intValue();
					if(!TimeSpan.isValidYear(year)) {
						/**
						 * @issue Error: Invalid year, #, line #
						 * @explain The year read by the Multiple RunSpec Creator within the control
						 * file is not defined as a valid year within the default database.
						**/
						messages.add("Error: Invalid year, " + year + ", line " + lineNumber);
						messagesHasError = true;
						return true;
					}
				}
			} catch(Exception e) {
				/**
				 * @issue Error: Unable to read year, line #
				 * @explain The Multiple RunSpec Creator was unable to read the year at
				 * the given line number in the control file.
				**/
				messages.add("Error: Unable to read year, line " + lineNumber);
				messagesHasError = true;
				return false;
			}

			try {
				additionalText = StringUtilities.safeGetString(reader.readStringCell()).trim();
				if(additionalText.length() > 20) {
					additionalText = additionalText.substring(0,20).trim();
					/**
					 * @issue Warning: Trimmed Additional Text Name to 20 characters, line #
					 * @explain The Additional Text Name column in the Multiple RunSpec Creator's
					 * control file is limited to 20 characters.
					**/
					messages.add("Warning: Trimmed Additional Text Name to 20 characters, line "
							+ lineNumber);
				}
				if(additionalText.length() > 0) {
					char[] illegalCharacters = "/\\:&<>*?".toCharArray();
					for(int i=0;i<illegalCharacters.length;i++) {
						if(additionalText.indexOf(illegalCharacters[i]) >= 0) {
							/**
							 * @issue Error: The Additional Text Name cannot contain [*] characters, line #
							 * @explain The Additional Text Name field applied to the file name of
							 * each generated RunSpec within the Multiple RunSpec Creator cannot
							 * contain special characters as defined by common operating systems.
							 * These include: / \ : & < > * ?
							**/
							messages.add("Error: The Additional Text Name cannot contain "
									+ illegalCharacters[i] + " characters, line " + lineNumber);
							messagesHasError = true;
						}
					}
				}
			} catch(Exception e) {
				/**
				 * @issue Error: Unable to read Additional Text Name, line #
				 * @explain The Multiple RunSpec Creator was unable to read the Additional Text Name
				 * at the given line number in the control file.
				**/
				messages.add("Error: Unable to read Additional Text Name, line " + lineNumber);
				messagesHasError = true;
				return false;
			}

			try {
				primaryDB = StringUtilities.safeGetString(reader.readStringCell()).trim();
			} catch(Exception e) {
				/**
				 * @issue Error: Unable to read Primary MOVES DB, line #
				 * @explain The Multiple RunSpec Creator was unable to read the Primary MOVES DB at
				 * the given line number in the control file.
				**/
				messages.add("Error: Unable to read Primary MOVES DB, line " + lineNumber);
				messagesHasError = true;
				return false;
			}

			try {
				outputDB = StringUtilities.safeGetString(reader.readStringCell()).trim();
			} catch(Exception e) {
				/**
				 * @issue Error: Unable to read Output DB, line #
				 * @explain The Multiple RunSpec Creator was unable to read the Output DB at
				 * the given line number in the control file.
				**/
				messages.add("Error: Unable to read Output DB, line " + lineNumber);
				messagesHasError = true;
				return false;
			}

			try {
				domainDB = StringUtilities.safeGetString(reader.readStringCell()).trim();
			} catch(Exception e) {
				/**
				 * @issue Error: Unable to read Domain DB, line #
				 * @explain The Multiple RunSpec Creator was unable to read the Domain DB at
				 * the given line number in the control file.
				**/
				messages.add("Error: Unable to read Domain DB, line " + lineNumber);
				messagesHasError = true;
				return false;
			}

			try {
				advancedFeaturesDB = StringUtilities.safeGetString(reader.readStringCell()).trim();
			} catch(Exception e) {
				/**
				 * @issue Error: Unable to read Advanced Features DB, line #
				 * @explain The Multiple RunSpec Creator was unable to read the Advanced Features
				 * DB at the given line number in the control file.
				**/
				messages.add("Error: Unable to read Advanced Features DB, line " + lineNumber);
				messagesHasError = true;
				return false;
			}

			// Read user databases until we run out of data on the line
			while(true) {
				try {
					s = StringUtilities.safeGetString(reader.readStringCell()).trim();
					if(s.length() <= 0) {
						break;
					}
					DatabaseSelection db = new DatabaseSelection();
					db.databaseName = s;
					userDBs.add(db);
				} catch(Exception e) {
					/**
					 * @issue Error: Unable to read User DB, line #
					 * @explain The Multiple RunSpec Creator was unable to read the User DB at
					 * the given line number in the control file.
					**/
					messages.add("Error: Unable to read User DB, line " + lineNumber);
					messagesHasError = true;
					return false;
				}
			}

			isValid = true; // assume no rules are broken
			// Enforce the rules requiring all required databases to be specified herein
			// or in the RunSpec.
			s = outputDB.length() > 0? outputDB
					: runSpec.outputDatabase == null? ""
					: runSpec.outputDatabase.databaseName;
			if(s == null || s.length() <= 0) {
				/**
				 * @issue Error: The output database must be provided, line #
				 * @explain When using the Multiple RunSpec Creator, either the active RunSpec or
				 * each control file entry must name the output database.
				**/
				messages.add("Error: The output database must be provided, line " + lineNumber);
				messagesHasError = true;
				isValid = false;
			}

			if(runSpec.domain == ModelDomain.SINGLE_COUNTY
					|| runSpec.domain == ModelDomain.PROJECT) {
				s = domainDB.length() > 0? domainDB
						: runSpec.scaleInputDatabase == null? ""
						: runSpec.scaleInputDatabase.databaseName;
				if(s == null || s.length() <= 0) {
					/**
					 * @issue Error: The domain database must be provided, line #
					 * @explain When using the Multiple RunSpec Creator with the single county or
					 * project scale, an input database specific to the domain must be provided.
					 * Either the RunSpec or each control file entry can name this database.
					**/
					messages.add("Error: The domain database must be provided, line " + lineNumber);
					messagesHasError = true;
					isValid = false;
				}

				if(runSpec.isCustomDomain()) {
					if(StringUtilities.safeGetString(runSpec.genericCounty.description).length()
							<= 0 && StringUtilities.safeGetString(countyDescription).length()
							<= 0) {
						/**
						 * @issue Error: Custom domains require a description, line #
						 * @explain When using the Multiple RunSpec Creator with a custom domain in
						 * single county or project mode, the custom domain must have a description.
						 * This description can come from the RunSpec or from the control file.
						 * In this case, the RunSpec contains no description for the custom
						 * domain and neither does the control file.
						**/
						messages.add("Error: Custom domains require a description, line "
								+ lineNumber);
						messagesHasError = true;
						isValid = false;
					}
				}
			}

			if(runSpec.shouldCopySavedGeneratorData) {
				s = advancedFeaturesDB.length() > 0? advancedFeaturesDB
						: runSpec.generatorDatabase == null? ""
						: runSpec.generatorDatabase.databaseName;
				if(s == null || s.length() <= 0) {
					/**
					 * @issue Error: The advanced features save database must be provided, line #
					 * @explain When using the Multiple RunSpec Creator with a RunSpec the requires
					 * generator output to be saved into a new database, either the RunSpec or the
					 * control file entries must name the database.
					**/
					messages.add(
							"Error: The advanced features save database must be provided, line "
							+ lineNumber);
					messagesHasError = true;
					isValid = false;
				}
			}

			return true; 	// did load and should continue loading, regardless of validity
							// of this individual entry
		}

		/**
		 * Apply this object's settings to a RunSpec.
		 * @param runSpec object to be modified
		**/
		public void toRunSpec(RunSpec runSpec) {
			GeographicSelection gs = new GeographicSelection();
			gs.databaseKey = countyID;
			gs.textDescription = countyDescription;
			gs.type = GeographicSelectionType.COUNTY;
			runSpec.geographicSelections.clear();
			runSpec.geographicSelections.add(gs);

			if(runSpec.genericCounty != null) {
				runSpec.genericCounty.splitCountyID(countyID);
				if(countyDescription != null && countyDescription.length() > 0) {
					runSpec.genericCounty.description = countyDescription;
				}
			}

			runSpec.timeSpan.years.clear();
			runSpec.timeSpan.years.add(Integer.valueOf(year));

			if(additionalText.length() > 0) {
				if(runSpec.description.length() > 0) {
					runSpec.description = additionalText + "\r\n" + runSpec.description;
				} else {
					runSpec.description = additionalText;
				}
			}

			if(primaryDB.length() > 0) {
				DatabaseSelection d = new DatabaseSelection();
				d.databaseName = primaryDB;
				runSpec.inputDatabase = d;
			}
			if(outputDB.length() > 0) {
				DatabaseSelection d = new DatabaseSelection();
				d.databaseName = outputDB;
				runSpec.outputDatabase = d;
			}
			if(domainDB.length() > 0) {
				DatabaseSelection d = new DatabaseSelection();
				d.databaseName = domainDB;
				runSpec.scaleInputDatabase = d;
			}
			if(advancedFeaturesDB.length() > 0) {
				DatabaseSelection d = new DatabaseSelection();
				d.databaseName = advancedFeaturesDB;
				runSpec.generatorDatabase = d;
			}

			for(Iterator<DatabaseSelection> i=userDBs.iterator();i.hasNext();) {
				DatabaseSelection d = (DatabaseSelection)i.next();
				boolean found = false;
				for(Iterator<DatabaseSelection> j=runSpec.databaseSelectionInputSets.iterator();
						j.hasNext();) {
					DatabaseSelection existing = (DatabaseSelection)j.next();
					if(doesMatch(d,existing)) {
						found = true;
						break;
					}
				}
				if(!found) {
					runSpec.databaseSelectionInputSets.add(d);
				}
			}
		}
	}

	/** Handle the Create RunSpecs button **/
	void handleCreateRunSpecsButton() {
		try {
			resetMessages();
			checkRunSpec();
			if(messagesHasError) {
				return;
			}
			checkInputs();
			if(messagesHasError) {
				return;
			}
			ArrayList<ControlFileEntry> entries =
					readControlFile(controlFileFullPath,controlFileWorksheetName);
			if(messagesHasError) {
				return;
			}
			checkEntries(entries);
			if(messagesHasError) {
				return;
			}
			createOutputFiles(entries);
			if(messagesHasError) {
				/**
				 * @issue Unable to create the RunSpecs. Check the messages list for details.
				 * @explain An error was encountered while the Multiple RunSpec Creator was
				 * creating new RunSpec files.  The most likely causes are insufficient permissions
				 * to the output folder or invalid characters in the file name prefix.
				**/
				JOptionPane.showMessageDialog(null,
						"Unable to create the RunSpecs. Check the messages list for details.",
						"Multiple RunSpec Creator", JOptionPane.ERROR_MESSAGE);
			} else {
				/** @nonissue **/
				JOptionPane.showMessageDialog(null,
						"RunSpecs created.",
						"Multiple RunSpec Creator", JOptionPane.INFORMATION_MESSAGE);
			}
		} catch(Exception e) {
			/**
			 * @issue Unable to create RunSpecs
			 * @explain An error was encountered while the Multiple RunSpec Creator was creating new
			 * RunSpec files.  The most likely causes are insufficient permissions to the
			 * output folder or invalid characters in the file name prefix.
			**/
			Logger.logError(e,"Unable to create RunSpecs");
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
	 * Check the current RunSpec for completeness, recording issues within the messages
	 * list and to messagesHasError.
	**/
	void checkRunSpec() {
		if(!isRunSpecReady) {
			/**
			 * @issue Warning: The RunSpec is incomplete and may produce other incomplete RunSpecs.
			 * @explain The Multiple RunSpec Creator generates RunSpecs based on the currently
			 * loaded RunSpec.  The current RunSpec is incomplete and may still be incomplete after
			 * applying the user-defined data within the control file, resulting in many incomplete
			 * RunSpecs being generated.
			**/
			messages.add("Warning: "
					+ "The RunSpec is incomplete and may produce other incomplete RunSpecs.");
		}
	}

	/**
	 * Check the user input controls for completeness, recording issues within the messages
	 * list and to messagesHasError.
	**/
	void checkInputs() {
		// Check the control file
		String t = controlFileFullPath;
		if(t == null || t.length() <= 0) {
			/**
			 * @issue Error: Please select a control file first.
			 * @explain The Multiple RunSpec Creator requires a control file to be selected before
			 * it can generate new RunSpecs.
			**/
			messages.add("Error: Please select a control file first.");
			messagesHasError = true;
		} else {
			File f = new File(t);
			if(!f.exists()) {
				/**
				 * @issue Error: The control file does not exist.
				 * @explain The control file selected within the Multiple RunSpec Creator does
				 * not exist.
				**/
				messages.add("Error: The control file does not exist.");
				messagesHasError = true;
			}
		}

		// Check the prefix
		t = prefixText.getText().trim();
		if(t.length() <= 0) {
			/**
			 * @issue Error: Please supply a file name prefix.
			 * @explain The Multiple RunSpec Creator requires a file name prefix to be given
			 * before it can generate new RunSpecs.
			**/
			messages.add("Error: Please supply a file name prefix.");
			messagesHasError = true;
		} else {
			char[] illegalCharacters = "/\\:&<>*?".toCharArray();
			for(int i=0;i<illegalCharacters.length;i++) {
				if(t.indexOf(illegalCharacters[i]) >= 0) {
					/**
					 * @issue Error: The prefix cannot contain [*] characters.
					 * @explain The prefix applied to the file name of each generated RunSpec
					 * within the Multiple RunSpec Creator cannot contain special characters as
					 * defined by common operating systems.  These include: / \ : & < > * ?
					**/
					messages.add("Error: The prefix cannot contain "
							+ illegalCharacters[i] + " characters.");
					messagesHasError = true;
				}
			}
			if(t.length() > 150) {
				/**
				 * @issue Error: The prefix cannot be longer than 150 characters.
				 * @explain The prefix applied to the file name of each generated RunSpec
				 * within the Multiple RunSpec Creator cannot be longer than 150 characters.
				**/
				messages.add("Error: The prefix cannot be longer than 150 characters.");
				messagesHasError = true;
			}
		}

		// Check the output directory
		t = outputDirectoryFullPath;
		if(t == null || t.length() <= 0) {
			/**
			 * @issue Error: Please select an output directory first.
			 * @explain The Multiple RunSpec Creator requires that a directory be selected to
			 * hold its generated RunSpec files before it can create the files.
			**/
			messages.add("Error: Please select an output directory first.");
			messagesHasError = true;
		} else {
			File f = new File(t);
			if(!f.exists()) {
				f.mkdirs();
				if(!f.exists()) {
					/**
					 * @issue Error: The output directory does not exist and could not be created.
					 * @explain The directory selected within the Multiple RunSpec Creator to
					 * receive the  generated RunSpecs does not exist and could not be automatically
					 * created.
					**/
					messages.add("Error: "
							+ "The output directory does not exist and could not be created.");
					messagesHasError = true;
				} else {
					/** @nonissue **/
					messages.add("Info: The output directory was created.");
				}
			}
		}
	}

	/**
	 * Read the control file, creating list of ControlFileEntry objects.
	 * @param fileName full path and name of the file to be read
	 * @param worksheetName for XLS files, the name of the worksheet within the file
	 * @return a list of ControlFileEntry objects, never null but may be empty
	 * @throws Exception if anything goes wrong
	**/
	ArrayList<ControlFileEntry> readControlFile(String fileName, String worksheetName)
			throws Exception {
		ArrayList<ControlFileEntry> entries = new ArrayList<ControlFileEntry>();
		CellFileReader reader = null;
		try {
			reader = new CellFileReader(new File(fileName),worksheetName);
			int lineNumber = 1;
			// Find the column headers.  They should be the first non-blank, non-skippable
			// row in the file.
			while(!reader.isEndOfData() && reader.shouldSkipLine()) {
				reader.endRow();
				lineNumber++;
			}
			if(reader.isEndOfData()) {
				/**
				 * @issue Error: No header row found in control file
				 * @explain The first row in the control file should contain column headings.
				**/
				messages.add("Error: No header row found in control file");
				messagesHasError = true;
				return entries;
			}
			// Read data
			while(!reader.isEndOfData()) {
				reader.endRow(); // move to the next row, moves past the header row on first use
				lineNumber++;
				if(reader.shouldSkipLine()) {
					continue;
				}
				ControlFileEntry entry = new ControlFileEntry();
				if(entry.read(reader,lineNumber)) {
					entries.add(entry);
				} else {
					break;
				}
			}
			// Note how many entries were read
			/** @nonissue **/
			String t = "Info: Read " + entries.size()
					+ " data rows, plus the header line, from the control file.";
			messages.add(t);
		} finally {
			if(reader != null) {
				reader.close();
				reader = null;
			}
		}
		return entries;
	}

	/**
	 * Compare the server and database portions of two database selections, treating blank
	 * servers as localhost.
	 * @param a first object in the comparison
	 * @param b second object in the comparison
	 * @return true if the server (with default logic) and database match
	**/
	boolean doesMatch(DatabaseSelection a, DatabaseSelection b) {
		String aServer = a.serverName;
		if(aServer == null || aServer.length() <= 0) {
			aServer = "localhost";
		}
		String bServer = b.serverName;
		if(bServer == null || bServer.length() <= 0) {
			bServer = "localhost";
		}

		String aDatabase = a.databaseName;
		if(aDatabase == null) {
			aDatabase = "";
		}
		String bDatabase = b.databaseName;
		if(bDatabase == null) {
			bDatabase = "";
		}

		return aServer.equalsIgnoreCase(bServer) && aDatabase.equalsIgnoreCase(bDatabase);
	}

	/**
	 * Check the list of entries for issues, updating messages as needed.
	 * @param entries list of ControlFileEntry objects to be examined
	**/
	void checkEntries(ArrayList<ControlFileEntry> entries) {
		// There should be no duplicate Year/County/Database combinations
		TreeSet<String> keys = new TreeSet<String>();
		TreeSet<String> complaints = new TreeSet<String>();

		for(Iterator<ControlFileEntry> i=entries.iterator();i.hasNext();) {
			ControlFileEntry entry = (ControlFileEntry)i.next();
			String key = "" + entry.year + "|" + entry.countyID;
			key += "|" + entry.getInputDatabaseKey(runSpec.domain == ModelDomain.SINGLE_COUNTY
					|| runSpec.domain == ModelDomain.PROJECT);
			key += "|" + StringUtilities.safeGetString(entry.additionalText);
			key += "|" + StringUtilities.safeGetString(entry.outputDB);
			if(keys.contains(key)) {
				if(!complaints.contains(key)) {
					complaints.add(key);
					/**
					 * @issue Error: Duplicate county, year, and databases in the control file, line #
					 * @explain Within a Multiple RunSpec Creator control file, the combination of a
					 * county, year, input databases, and output database may only appear once.
					 * Note that a the domain database is only considered part of this
					 * check if the RunSpec is using the County or Project scale.
					**/
					messages.add("Error: Duplicate county, year, and databases in the control file,"
							+ " line " + entry.lineNumber);
					messagesHasError = true;
				}
			} else {
				keys.add(key);
			}
		}
	}

	/**
	 * Generate the RunSpecs and script file.
	 * @param entries list of ControlFileEntry objects to be examined
	**/
	void createOutputFiles(ArrayList<ControlFileEntry> entries) {
		// Remember the list of all files, RunSpec and script, that were created,
		// so we can cleanup if something goes wrong.
		ArrayList<File> createdFiles = new ArrayList<File>(); // holds File objects
		boolean shouldDeleteFiles = true;
		PrintWriter scriptWriter = null;
		String prefix = prefixText.getText().trim();
		if(prefix.length() > 0) {
			prefix += "_";
		}

		// Preserve critical data from the currently loaded RunSpec.  This data may
		// be overwritten by each ControlFileEntry.
		GenericCounty originalGenericCounty = runSpec.genericCounty;
		TreeSet<GeographicSelection> originalGeographicSelections = runSpec.geographicSelections;
		runSpec.geographicSelections = new TreeSet<GeographicSelection>();
		TreeSet<Integer> originalYears = runSpec.timeSpan.years;
		runSpec.timeSpan.years = new TreeSet<Integer>();
		String originalDescription = runSpec.description;
		LinkedList<DatabaseSelection> originalUserDatabases = runSpec.databaseSelectionInputSets;
		DatabaseSelection originalInputDatabase = runSpec.inputDatabase;
		DatabaseSelection originalOutputDatabase = runSpec.outputDatabase;
		DatabaseSelection originalScaleInputDatabase = runSpec.scaleInputDatabase;
		DatabaseSelection originalGeneratorDatabase = runSpec.generatorDatabase;

		try {
			File outputFolder = new File(outputDirectoryFullPath);
			File scriptFile = new File(outputFolder,prefix+"ExecuteRunSpecs.bat");
			createdFiles.add(scriptFile);

			// Open the script file and add any header to it
			scriptWriter = new PrintWriter(new BufferedWriter(new FileWriter(scriptFile),4096));
			scriptWriter.println("@echo off");
			scriptWriter.println("rem Script generated by the MOVES Multiple RunSpec Creator");
			scriptWriter.println("rem Based on control file: " + controlFileFullPath);
			scriptWriter.println("rem ----------------------------------------------------------- \n");
			// scriptWriter.println("echo Changing to the MOVES folder and compiling code...");
			String movesFolder = (new File(".")).getCanonicalPath();
			if(movesFolder.indexOf(":") == 1) { // If it starts with a drive letter and colon
				scriptWriter.println(movesFolder.substring(0,2));
			}
			/* scriptWriter.println("cd \"" + movesFolder + "\"");
			scriptWriter.println("call setenv.bat");
			scriptWriter.println("call ant compile");
			scriptWriter.println("rem ----------------------------------------------------------- \n");
			*/
			
			// This script template is taken from the CommandLineMOVES documentation
			scriptWriter.println("rem save the runspec folder:");
			scriptWriter.println("set RunspecDir=" + outputFolder);
			scriptWriter.println("rem set up moves environment:");
			scriptWriter.println("cd \"" + movesFolder + "\"");
			scriptWriter.println("call setenv.bat");
			scriptWriter.println("if exist moveslog.txt REN \"moveslog.txt\" \"%logfile%\"");
			scriptWriter.println("if exist %logfile% (");
			scriptWriter.println("	if not exist moveslog.zip ( ");
			scriptWriter.println("		jar cMf moveslog.zip \"%logfile%\" && del \"%logfile%\"");
			scriptWriter.println("	) else ( ");
			scriptWriter.println("		jar uMf moveslog.zip \"%logfile%\" && del \"%logfile%\"");
			scriptWriter.println("	)");
			scriptWriter.println(")");
			scriptWriter.println("rem ----------------------------------------------------------- \n");
			scriptWriter.println("rem Run MOVES for each runspec:");

			TreeMap<String,Integer> baseNameCounts = new TreeMap<String,Integer>();

			for(Iterator<ControlFileEntry> i=entries.iterator();i.hasNext();) {
				ControlFileEntry entry = (ControlFileEntry)i.next();
				// Create the target RunSpec File object with the proper name
				String rfName = prefix + entry.countyID + "_" + entry.year;
				if(entry.additionalText.length() > 0) {
					rfName += "_" + entry.additionalText;
				}
				// Ensure the file name is unique
				Integer baseCount = baseNameCounts.get(rfName);
				if(baseCount == null) {
					baseCount = Integer.valueOf(0);
					baseNameCounts.put(rfName,baseCount);
				}
				baseCount = Integer.valueOf(1+baseCount.intValue());
				baseNameCounts.put(rfName,baseCount);
				if(baseCount.intValue() > 1) {
					rfName += "_" + baseCount;
				}
				// Finish naming and creating the file
				rfName += ".mrs";
				File rf = new File(outputFolder,rfName);
				createdFiles.add(rf);

				// Complain if the target file exists and cannot be deleted
				if(rf.exists()) {
					FileUtilities.deleteFileWithRetry(rf);
					if(rf.exists()) {
						/**
						 * @issue Error: Unable to remove old RunSpec file [filename]
						 * @explain The Multiple RunSpec Creator detected an existing RunSpec file
						 * with the same name and directory as one it was about to create, but was
						 * unable to remove the original file.  RunSpec creation has been
						 * halted.
						**/
						messages.add("Error: Unable to remove old RunSpec file " + rfName);
						messagesHasError = true;
						return;
					}
				}

				// Restore items that are appended or modified instead of replaced
				runSpec.description = originalDescription;
				if(originalUserDatabases.size() > 0) {
					runSpec.databaseSelectionInputSets = new LinkedList<DatabaseSelection>();
					for(Iterator<DatabaseSelection> ui=originalUserDatabases.iterator();
							ui.hasNext();) {
						DatabaseSelection d = (DatabaseSelection)ui.next();
						runSpec.databaseSelectionInputSets.add((DatabaseSelection)d.clone());
					}
				} else {
					runSpec.databaseSelectionInputSets.clear();
				}
				runSpec.inputDatabase = originalInputDatabase;
				runSpec.outputDatabase = originalOutputDatabase;
				runSpec.scaleInputDatabase = originalScaleInputDatabase;
				runSpec.generatorDatabase = originalGeneratorDatabase;
				if(entry.isExistingCounty) {
					runSpec.genericCounty = null;
				} else {
					runSpec.genericCounty = GenericCounty.clone(originalGenericCounty);
				}

				// Apply each ControlFileEntry to the current RunSpec
				entry.toRunSpec(runSpec);

				// Save the current RunSpec
				RunSpecXML xmlWriter = new RunSpecXML(runSpec);
				xmlWriter.save(rf);

				// Append to the script file
				scriptWriter.println("if exist sharedwork\\*.* erase /Q /S /F sharedwork\\*.*");
				scriptWriter.println("if exist WorkerFolder\\*.* erase /Q /S /F WorkerFolder\\*.*");
				scriptWriter.println("echo Running " + rfName);
				scriptWriter.println("call ant run -Drunspec=\"%RunspecDir%" + "\\" + rfName + "\"\n");
			}

			// All done successfully so keep the files that were just created
			shouldDeleteFiles = false;
		} catch(Exception e) {
			Logger.logError(e,"Unable to generate RunSpecs");
			messages.add("Error: Unable to generate RunSpecs, " + e.getMessage());
			messagesHasError = true;
		} finally {
			// Close the script file writer
			if(scriptWriter != null) {
				try {
					scriptWriter.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				scriptWriter = null;
			}

			// Restore critical data preserved from the currently loaded RunSpec
			runSpec.geographicSelections = originalGeographicSelections;
			runSpec.timeSpan.years = originalYears;
			runSpec.description = originalDescription;
			runSpec.databaseSelectionInputSets = originalUserDatabases;
			runSpec.inputDatabase = originalInputDatabase;
			runSpec.outputDatabase = originalOutputDatabase;
			runSpec.scaleInputDatabase = originalScaleInputDatabase;
			runSpec.generatorDatabase = originalGeneratorDatabase;
			runSpec.genericCounty = originalGenericCounty;

			// Remove created files if something went wrong
			if(shouldDeleteFiles) {
				for(Iterator<File> i=createdFiles.iterator();i.hasNext();) {
					File f = (File)i.next();
					try {
						f.delete();
					} catch(Exception e) {
						// Nothing to do here
					}
				}
			}
		}
	}
}
