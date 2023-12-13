/**************************************************************************************************
 * @(#)ImporterTabBase.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.io.*;
import java.util.*;
import java.sql.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.MOVESDatabaseType;

/**
 * Panel for importers that require a description, a list of messages, and one
 * or more IImporterPart objects.
 *
 * @author		Wes Faler
 * @author      William Aikman
 * @author		Bill Shaw
 * @author		Jarrod Brown
 * @version		2018-06-22
**/
public class ImporterTabBase implements IImporterPanel, FocusListener, ActionListener,
		TableFileLinkagePart.IMessageHandler {
	/** the importer that owns this panel **/
	IImporter importer;
	/** provider and storage for the data managed by this panel **/
	IImporterTabBaseProvider importerDataSource;
	/** title in the ImporterGUI **/
	String title;
	/** Panel holding the controls **/
	JPanel panel;
	/** Scrolling area for the description **/
	JScrollPane descriptionScrollPane;
	/** Edit area for the description **/
	JTextArea descriptionTextArea;
	/** Scrolling area for the importer parts **/
	JScrollPane dataSourcesScrollPane;
	/** Panel holding the importer parts' panels **/
	JPanel dataSourcesPanel;
	/** Button for importing data **/
	JButton importButton;
	/** Button for exporting data from the user database **/
	JButton exportImportedDataButton;
	/** Button for exporting data from the default database **/
	JButton exportDefaultDataButton;
	/** Button for exporting data from the execution database **/
	JButton exportExecutionDataButton;
	/** Button for clearing all data **/
//	JButton clearDataButton;
	/** Scrolling are for messages **/
	JScrollPane messagesScrollPane;
	/** DefaultListModel for messageList **/
	DefaultListModel<String> messageListModel;
	/** List of messages **/
	JList<String> messagesList;
	/** Description before updates **/
	String previousDescription = "";
	/** Optional custom button(s) **/
	JButton[] customButtons = null;
	/** keep track of if data has been imported already **/
	boolean alreadyImported = false;

	/**
	 * Constructor
	 * @param importerToUse the importer that owns this panel
	 * @param titleToUse title in the ImporterGUI
	 * @param importerDataSourceToUse provider and storage for the data managed by this panel
	**/
	public ImporterTabBase(IImporter importerToUse, String titleToUse,
			IImporterTabBaseProvider importerDataSourceToUse) {
		importer = importerToUse;
		title = titleToUse;
		importerDataSource = importerDataSourceToUse;

		createControls();
		populateControls();
	}

	/**
	 * Get the actual JPanel to be shown
	 * @return the JPanel that reprents this importer
	**/
	public JPanel getPanel() {
		return panel;
	}

	/**
	 * Get the IImporter that owns the panel.
	 * @return the IImporter that owns the panel
	**/
	public IImporter getImporter() {
		return importer;
	}

	/**
	 * Get the text that titles the panel.
	 * @return the text that titles the panel
	**/
	public String getTitle() {
		return title;
	}

	/** Create and arrange the controls of this panel **/
	void createControls() {
		JLabel label1;
		JLabel label2;

		messageListModel = new DefaultListModel<String>();

		panel = new JPanel();
		label1 = new JLabel();
		descriptionScrollPane = new JScrollPane();
		descriptionTextArea = new JTextArea();
		dataSourcesScrollPane = new JScrollPane();
		dataSourcesPanel = new JPanel();

		importButton = new JButton();
		ToolTipHelper.add(importButton,"Read and import the file now");

		exportImportedDataButton = new JButton();
		ToolTipHelper.add(exportImportedDataButton,"Export previously imported data");

		exportDefaultDataButton = new JButton();
		ToolTipHelper.add(exportDefaultDataButton,"Export national-level default data");

		exportExecutionDataButton = new JButton();
		ToolTipHelper.add(exportExecutionDataButton,"Export recently used data");

//		clearDataButton = new JButton();
//		ToolTipHelper.add(clearDataButton,"Remove previously imported data");

		String[] customButtonNames = importerDataSource.getCustomButtonNames();
		if(customButtonNames != null) {
            customButtons = new JButton[customButtonNames.length];
            int i = 0;
            for (String customButtonName : customButtonNames) {
                customButtons[i] = new JButton(customButtonName);
                ToolTipHelper.add(customButtons[i],customButtonName);
                customButtons[i].addActionListener(this);
                i++;
            }
		}

		label2 = new JLabel();
		messagesScrollPane = new JScrollPane();
		messagesList = new JListWithToolTips<String>(messageListModel);

		descriptionTextArea.addFocusListener(this);
		importButton.addActionListener(this);
		exportImportedDataButton.addActionListener(this);
		exportDefaultDataButton.addActionListener(this);
		exportExecutionDataButton.addActionListener(this);
//		clearDataButton.addActionListener(this);

		//======== panel ========
		{
			panel.setLayout(new GridBagLayout());
			((GridBagLayout)panel.getLayout()).columnWidths = new int[] {0, 0, 0, 0, 0, 0, 0};
			((GridBagLayout)panel.getLayout()).rowHeights = new int[] {0, 75, 0, 0, 0, 0, 0};
			((GridBagLayout)panel.getLayout()).columnWeights = new double[] {0.0, 3.0, 0.0, 0.0, 0.0, 1.0, 1.0E-4};
			((GridBagLayout)panel.getLayout()).rowWeights = new double[] {0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0E-4};

			//---- label1 ----
			label1.setText("Description of Imported Data:");
			panel.add(label1, new GridBagConstraints(0, 0, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//======== descriptionScrollPane ========
			{
				descriptionScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

				//---- descriptionTextArea ----
				descriptionTextArea.setWrapStyleWord(true);
				descriptionScrollPane.setViewportView(descriptionTextArea);
			}
			if(customButtons != null) {
				panel.add(descriptionScrollPane, new GridBagConstraints(0, 1, 6-customButtons.length, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));

                int i = customButtons.length;
                for (JButton customButton : customButtons) {
                    panel.add(customButton, new GridBagConstraints(6-i, 1, 1, 1, 0.0, 0.0,
                        GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                        new Insets(0, 0, 5, 0), 0, 0));
                    i--;
                }
			} else {
				panel.add(descriptionScrollPane, new GridBagConstraints(0, 1, 6, 1, 0.0, 0.0,
					GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(0, 0, 5, 0), 0, 0));
			}
			label1.setDisplayedMnemonic('s');
			label1.setLabelFor(descriptionTextArea);

			//======== dataSourcesScrollPane ========
			{
				dataSourcesScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

				//======== dataSourcesPanel ========
				{
					dataSourcesPanel.setLayout(new BoxLayout(dataSourcesPanel, BoxLayout.Y_AXIS));
				}
				dataSourcesScrollPane.setViewportView(dataSourcesPanel);
			}
			panel.add(dataSourcesScrollPane, new GridBagConstraints(0, 2, 6, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));

			//---- importButton ----
			importButton.setText("Import");
			importButton.setMnemonic('I');
			importButton.setDisplayedMnemonicIndex(0);
			panel.add(importButton, new GridBagConstraints(5, 3, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//---- label2 ----
			label2.setText("Messages:");
			panel.add(label2, new GridBagConstraints(0, 4, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

			//======== messagesScrollPane ========
			{
				//---- messagesList ----
				messagesList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
				messagesList.setVisibleRowCount(5);
				messagesScrollPane.setViewportView(messagesList);
			}
			panel.add(messagesScrollPane, new GridBagConstraints(0, 5, 6, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 0, 0), 0, 0));

			if(!importerDataSource.isSubjectToExportRestrictions() 
					|| CompilationFlags.ENABLE_EXPORT_DEFAULT_DATA) {
				if(importerDataSource.allowDefaultDataExport()
						&& (!importer.getImporterManager().isCustomDomain() || importerDataSource.allowCustomDomainDefaultDataExport())) {
					//---- exportDefaultDataButton ----
					exportDefaultDataButton.setText("Export Default Data");
					exportDefaultDataButton.setMnemonic('D');
					exportDefaultDataButton.setDisplayedMnemonicIndex(7);
					panel.add(exportDefaultDataButton, new GridBagConstraints(0, 6, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 5), 0, 0));
				}
			}
/*
			if(!importer.getImporterManager().isCustomDomain()) {
				if(!importerDataSource.isSubjectToExportRestrictions()
						|| CompilationFlags.ENABLE_EXPORT_DEFAULT_DATA) {
					if(importerDataSource.allowDefaultDataExport()) {
						//---- exportDefaultDataButton ----
						exportDefaultDataButton.setText("Export Default Data");
						panel.add(exportDefaultDataButton, new GridBagConstraints(0, 6, 1, 1, 0.0, 0.0,
							GridBagConstraints.CENTER, GridBagConstraints.BOTH,
							new Insets(0, 0, 5, 5), 0, 0));
					}
				}
			}
*/
			if(!importerDataSource.isSubjectToExportRestrictions()
					|| CompilationFlags.ENABLE_EXPORT_DEFAULT_DATA) {
				if(importerDataSource.allowExecutionDataExport()) {
					//---- exportExecutionDataButton ----
					exportExecutionDataButton.setText("Export Most Recent Execution Data");
					exportExecutionDataButton.setMnemonic('E');
					exportExecutionDataButton.setDisplayedMnemonicIndex(0);
					panel.add(exportExecutionDataButton, new GridBagConstraints(1, 6, 1, 1, 0.0, 0.0,
						GridBagConstraints.CENTER, GridBagConstraints.BOTH,
						new Insets(0, 0, 5, 5), 0, 0));
				}
			}

			//---- exportImportedDataButton ----
			exportImportedDataButton.setText("Export Imported Data");
			exportImportedDataButton.setMnemonic('x');
			exportImportedDataButton.setDisplayedMnemonicIndex(1);
			panel.add(exportImportedDataButton, new GridBagConstraints(2, 6, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));

/*
			//---- clearDataButton ----
			clearDataButton.setText("Clear Imported Data");
			panel.add(clearDataButton, new GridBagConstraints(3, 6, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 5), 0, 0));
*/
		}

		ArrayList parts = importerDataSource.getParts();
		if(parts != null) {
			for(int i=0;i<parts.size();i++) {
				if(i > 0) {
					dataSourcesPanel.add(new JSeparator(JSeparator.HORIZONTAL));
				}
				IImporterPart part = (IImporterPart)parts.get(i);
				if(part instanceof TableFileLinkagePart) {
					((TableFileLinkagePart)part).messageHandler = this;
				}
				JPanel p = part.getPanel();
				if(p != null) {
					dataSourcesPanel.add(p);
				}
			}
		}
	}

	/** Fill the controls on this panel with data **/
	void populateControls() {
		String description = importerDataSource.getDescription();
		if(description == null) {
			description = "";
		}
		descriptionTextArea.setText(description);

		//Logger.log(LogMessageCategory.DEBUG,"ImporterTabBase.populateControls part.populateControls...");
		ArrayList parts = importerDataSource.getParts();
		if(parts != null) {
			for(int i=0;i<parts.size();i++) {
				IImporterPart part = (IImporterPart)parts.get(i);
				part.populateControls();
			}
		}

		//Logger.log(LogMessageCategory.DEBUG,"ImporterTabBase.populateControls show messages...");
		showMessages();

		//Logger.log(LogMessageCategory.DEBUG,"ImporterTabBase.populateControls done.");
	}

	/** Display current messages **/
	public void showMessages() {
		//Logger.log(LogMessageCategory.DEBUG,"ImporterTabBase.showMessages");
		messagesList.removeAll();
		messageListModel.clear();
		ArrayList messages = importerDataSource.getMessages();
		if(messages != null) {
			for(int i=0;i<messages.size();i++) {
				String m = (String)messages.get(i);
				messageListModel.addElement(m);
			}
		}
	}

	/**
	 * Handles the focus lost event for the description text area.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		JComponent c = (JComponent)e.getComponent();
		if(c == descriptionTextArea) {
			String description = descriptionTextArea.getText().trim();
			if(!description.equals(previousDescription)) {
				importerDataSource.setDescription(descriptionTextArea.getText());
			}
		}
	}

	/**
	 * Handles the focus gained event for the description text area.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
		JComponent c = (JComponent)e.getComponent();
		if(c == descriptionTextArea) {
			previousDescription = importerDataSource.getDescription();
		}
	}

	/**
	 * Calls the appropriate button handler.
	 * @param e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == importButton) {
			handleImportButton();
		} else if(e.getSource() == exportImportedDataButton) {
			handleExportImportedDataButton();
		} else if(e.getSource() == exportDefaultDataButton) {
			handleExportDefaultDataButton();
		} else if(e.getSource() == exportExecutionDataButton) {
			handleExportExecutionDataButton();
//		} else if(e.getSource() == clearDataButton) {
//			handleClearDataButton();
		} else if(customButtons != null) {
            for (JButton customButton : customButtons) {
                if (e.getSource() == customButton) {
			        importerDataSource.onCustomButton(customButton.getText(),panel);
                }
            }
		}
	}

	/** Handle the Import button action **/
	void handleImportButton() {
		// Complain if there is no database selected
		if(!importer.getImporterManager().database.hasDatabase()) {
			JOptionPane.showMessageDialog(null,
					"Please select a database first.",
					"Database Needed", JOptionPane.ERROR_MESSAGE);
			return;
		}

		boolean clear = false;
		//check to see if data has been imported before
		if(alreadyImported) {
			int option = JOptionPane.showConfirmDialog(null, "Clear previously imported data?");
			if(option == JOptionPane.CANCEL_OPTION) {
				return;
			} else if (option == JOptionPane.YES_OPTION) {
				//clear the data
				clear = true;
			}
		}
		
		try {
			setWaitCursor();
			Connection db = importer.getImporterManager().openDatabase();
			if(db == null) {
				return;
			}
			importer.getImporterManager().activeDb = db;

			//Check to make sure all imported tables contain data before importing any tables.
			//If tables are missing data, do not import any tables.
			if (importer.getDataHandler().doImport(db,false,importerDataSource.getMessages())) {
				if(clear) {
					importer.getDataHandler().doClear(db);
				}
				importer.getDataHandler().doImport(db,true,importerDataSource.getMessages());
			}
			// Tell the supporting GUI to refresh its log display
			importer.getImporterManager().createGUI(null).loadLog(db);
			importer.getImporterManager().createGUI(null).refreshDomainStatusIcons();
			importer.getImporterManager().activeDb = null;
			DatabaseUtilities.closeConnection(db);
			db = null;
			populateControls();
			alreadyImported = true;
		} finally {
			setDefaultCursor();
		}
	}

	/**
	 * Export from a database, prompting the user for the file name.
	 * @param type which type of MOVES database holds the exported data.  Typically, this
	 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
	 * being used.
	 * @param db database to export
	**/
	void handleExport(MOVESDatabaseType type, Connection db) {
		// Prompt for the File for the first table.  Other files are based upon this
		// one's name. If an XLS file is chosen, each affected table will get its
		// own worksheet.
		Component parent = panel.getParent();
		while(parent != null && !(parent instanceof JFrame)) {
			parent = parent.getParent();
		}
		FileDialog fd = new FileDialog((JFrame)parent,
				"Export " + importer.getName() + " Data", FileDialog.SAVE);
		fd.setVisible(true); //fd.show();
		if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File file = new File(filePath);
		if(file.exists()) {
			FileUtilities.deleteFileWithRetry(file);
			if(file.exists()) {
				Logger.log(LogMessageCategory.ERROR,"Export file cannot be deleted.");
				return;
			}
		}
	
		int exportResult = 0;
		try {
			setWaitCursor();
			importer.getImporterManager().activeDb = db;
			exportResult = importer.getDataHandler().doExport(type,db,file);
			importer.getImporterManager().activeDb = null;
		} finally {
			setDefaultCursor();
		}

		// Note: No log of the export is needed.

		if(exportResult == 0) { // If there were no errors but no data was exported...
			JOptionPane.showMessageDialog(null,
				"Could not find default data for " + importer.getName() + " using the current\nRunSpec selections.\n\nEither modify the RunSpec or click \"Create Template...\" instead.",
				"No Data for " + importer.getName(), JOptionPane.ERROR_MESSAGE);
		}
	}

	/** Handle the Export Imported Data button action **/
	void handleExportImportedDataButton() {
		// Complain if there is no database selected
		if(!importer.getImporterManager().database.hasDatabase()) {
			JOptionPane.showMessageDialog(null,
					"Please select a database first.",
					"Database Needed", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		try {
			setWaitCursor();
			Connection db = importer.getImporterManager().openDatabase();
			if(db == null) {
				return;
			}
			handleExport(null,db);
			DatabaseUtilities.closeConnection(db); // close the connection
			db = null;
		} finally {
			setDefaultCursor();
		}

	}

	/** Handle the Export Default Data button action **/
	void handleExportDefaultDataButton() {
		try {
			setWaitCursor();
			Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
			if(db == null) {
				return;
			}
			handleExport(MOVESDatabaseType.DEFAULT,db);
			db = null; // don't close the connection since it is shared by the whole GUI
		} finally {
			setDefaultCursor();
		}
	}

	/** Handle the Export Execution Data button action **/
	void handleExportExecutionDataButton() {
		try {
			setWaitCursor();
			Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.EXECUTION);
			if(db == null) {
				JOptionPane.showMessageDialog(null,
						"There is no MOVESExecution database. Please run MOVES first.",
						"Database Needed", JOptionPane.ERROR_MESSAGE);
				return;
			}
			handleExport(MOVESDatabaseType.EXECUTION,db);
			db = null; // don't close the connection since it is shared by the whole GUI
		} finally {
			setDefaultCursor();
		}
	}

	/** Handle the Clear Data button action **/
/*
	void handleClearDataButton() {
		// Complain if there is no database selected
		if(!importer.getImporterManager().database.hasDatabase()) {
			JOptionPane.showMessageDialog(null,
					"Please select a database first.",
					"Database Needed", JOptionPane.ERROR_MESSAGE);
			return;
		}
		// Prompt for confirmation, recommending an Export operation first.
        // modified 05/18/2009 to simplfy for User Enhancement 241
		int choice = JOptionPane.showConfirmDialog(null,
				"Are you sure you want to clear the data? ",
				"",JOptionPane.YES_NO_OPTION);
		if(choice != JOptionPane.YES_OPTION) {
			return;
		}

		Connection db = importer.getImporterManager().openDatabase();
		if(db == null) {
			return;
		}
		importer.getImporterManager().activeDb = db;
		// Clear data from each table, not necessarily 1-to-1 with parts
		importer.getDataHandler().doClear(db);
		// Clear the log associated with this type of importer
		importer.getImporterManager().deleteLog(importer);
		// Tell the supporting GUI to refresh its log display
		importer.getImporterManager().createGUI(null).loadLog(db);
		importer.getImporterManager().createGUI(null).refreshDomainStatusIcons();
		importer.getImporterManager().activeDb = null;
		DatabaseUtilities.closeConnection(db);
		db = null;

		messageListModel.addElement("Data cleared.");
		messagesList.ensureIndexIsVisible(messageListModel.size()-1);
	}
*/

	/**
	 * Add and scroll to a new message line.
	 * @param text information to be displayed
	**/
	public void addMessageLine(String text) {
		messageListModel.addElement(text);
		messagesList.ensureIndexIsVisible(messageListModel.size()-1);
	}

	/** Remove all entries from the displayed log **/
	public void clearLogDisplay() {
		messagesList.removeAll();
		messageListModel.clear();
		importerDataSource.clearMessages();
	}

	/** Refresh status icons shown for all tabs **/	
	public void refreshIcons() {
		try {
			setWaitCursor();
			Connection db = importer.getImporterManager().openDatabase(false);
			if(db == null) {
				return;
			}
			importer.getImporterManager().activeDb = db;
			// Tell the supporting GUI to refresh its log display
			importer.getImporterManager().createGUI(null).loadLog(db);
			importer.getImporterManager().createGUI(null).refreshDomainStatusIcons();
			importer.getImporterManager().activeDb = null;
			DatabaseUtilities.closeConnection(db);
			db = null;
		} finally {
			setDefaultCursor();
		}
	}
	
	/** Sets the wait cursor (don't have access to MOVESWindow, so it needs to be redefined here) **/
	public void setWaitCursor() {
		RootPaneContainer root = (RootPaneContainer) this.getPanel().getRootPane().getTopLevelAncestor();
		root.getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		root.getGlassPane().setVisible(true);
	}
	
	/** Sets the default cursor (don't have access to MOVESWindow, so it needs to be redefined here) **/
	public void setDefaultCursor() {
		RootPaneContainer root = (RootPaneContainer) this.getPanel().getRootPane().getTopLevelAncestor();
		root.getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		root.getGlassPane().setVisible(true);
	}
}
