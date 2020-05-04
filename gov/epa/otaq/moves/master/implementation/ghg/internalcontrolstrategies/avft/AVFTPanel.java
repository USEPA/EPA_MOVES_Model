/**************************************************************************************************
 * @(#)AVFTPanel.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.avft;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.text.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.util.*;
import java.lang.*;
import java.sql.*;
import java.math.*;
import javax.swing.table.*;
import javax.swing.event.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.*;
import gov.epa.otaq.moves.master.runspec.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import java.io.*;

/**
 * Implements a Panel for AVFT (Alternative Vehicle Fuels & Technologies) Control Strategy
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @author		EPA - John K.
 * @author		EPA - Mitch C.
 * @author		Tim Hull
 * @version		2014-01-23
**/
class AVFTPanel extends InternalControlStrategyPanel implements ActionListener {
	/** Singleton for current AVFTPanel **/
	public static AVFTPanel avftPanelCurrent = null ;

	/** A reference to the AVFTControlStrategy that is being used for this execution **/
	AVFTControlStrategy strategy;
	/** Non-visual data holder for this panel **/
	AVFTPanelData data;

	/** Title text **/
	JLabel titleLabel;
	/** Title text **/
	JLabel titleLabelLoaded;
	/** Panel shown when no input has been specified **/
	JPanel notYetLoadedPanel;
	/** Label on notYetLoadedPanel giving instructions for the "Import..." button **/
	JLabel useImportButtonLabel;
	/** Panel shown Delete AVFT data **/
	JPanel deleteAVFTDataPanel;
	/** Label on First export the old AVFT data **/
	JLabel useDeleteButtonLabel;
	/** Panel shown when input, even if invalid, has been specified. **/
	JPanel loadedPanel;
	/** "Data Source:" title on loadedPanel **/
	JLabel dataSourceTitleLabel;
	/** Original file name of the parameters **/
	JLabel dataSourceFileNameLabel;
	/** Type of the original parameters file **/
	JLabel dataSourceTypeLabel;
	/** If applicable, the name of the worksheet containing the parameters **/
	JLabel dataSourceWorksheetLabel;
	/** "Reload" button on loadedPanel **/
	JButton reloadButton;
	/** Usage note label on loadedPanel **/
	JLabel usageNoteLabel;

	/** Panel showing the list of messages, warnings, and errors **/
	JPanel messagesPanel;
	/** Title on the messages panel **/
	JLabel messageLabel;
	/** Scrolling region for the messages list box **/
	JScrollPane messagesScrollPane;
	/** DefaultListModel for messageList **/
	DefaultListModel<String> messageListModel;
	/** Messages, warnings, and errors list **/
	JList<String> messageList;

	/** Normalize button **/
	JButton buttonNormalize;

	/**
	 * true after the constructor has completely finished.  Used to surpress the display
	 * of a warning about no alternatives before the user even sees the screen.
	**/
	boolean isLoaded = false;

	/**
	 * True when the GUI's values should be loaded from FuelEngFraction.
	 * When false, the SampleVehiclePopulation table is aggregated instead.
	**/
	boolean useFuelEngFractionTable = true;

	/**
	 * Constructor
	 * @param strategyToUse the AVFTControlStrategy that owns this panel
	 * @param dataToUse Non-visual data holder for this panel
	**/
	public AVFTPanel(AVFTControlStrategy strategyToUse, AVFTPanelData dataToUse) {
		strategy = strategyToUse;
		data = dataToUse;

		if ( strategy == null ) {
			return ;
		}

		data.clearAllData() ;

		avftPanelCurrent = this ;
		EPATableModel.setDbConnectionIfNull(
				DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT ) ) ;

		data.loadTables();
		createControls();
		data.loadTablesFromFuelEngTechAssoc() ;

		arrangeControls();
		populateControls();

		isLoaded = true;
	}

	/**
	 * Returns a tab-separated-value definition for the data structure in memory
	 * @return Returns a string with text representing the object
	**/
	public String getTSV() {
		return data.getTSV();
	}

	/**
	 * Read XML values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param root XML object under which the XML returned by getXML() has been stored
	 * @return true if the XML could be read successfully
	**/
	public boolean acceptXML(String className,Node root) {
		return data.acceptXML(className,root);
	}

	/**
	 * Read Tab-Separated-Values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param text tab-separated-values as returned by getTSV()
	 * @return true if the text could be read successfully
	**/
	public boolean acceptTSV(String className,String text) {
		return data.acceptTSV(className,text);
	}

	/**
	 * Finds the appropriate cell in the memory structures and updates the amount
	 * @param sourceTypeId The Source Type ID of the cell to be updated
	 * @param year The Year of the cell to be updated
	 * @param columnName The Column Name of the cell to be updated
	 * @param amount The Amount value of the cell that will be updated
	**/
	public void setMemory( String sourceTypeId , String year , String columnName ,
			String amount ) {
		data.setMemory(sourceTypeId,year,columnName,amount);
	}

	/**
	 * Creates and initializes all controls on this panel. And fixes the height
	 * and width of the JTable to fit in the screen.
	**/
	public void createControls() {
		buttonNormalize = new JButton();
		buttonNormalize.setName("buttonNormalize");
		buttonNormalize.setText("Normalize");
		buttonNormalize.addActionListener( this ) ;
		ToolTipHelper.add(buttonNormalize,"Rescale data so that each row sums to 100%");

		titleLabel = new JLabel("AVFT");
		titleLabelLoaded = new JLabel("AVFT");

		useImportButtonLabel = new JLabel("Use the \"Import...\" button to select an AVFT parameter file.");
		useDeleteButtonLabel = new JLabel("<html><body>"
				+ "This version of the AVFT has been deprecated and this runspec will <br>"
				+ "no longer generate results.  To upgrade, first use the \"Export\" <br>"
				+ "button to create a XLS or XLSX file of your AVFT data.<br><br>"
				+ "Then, use the \"Fueltype and Technologies\" importer to read<br>"
				+ "the data file into a new database.<br><br>"
				+ "Finally, return to this panel and use the \"Delete\" button to<br>"
				+ "remove the old data.<br><br>"
				+ "It is best practice to preserve the original version of this<br>"
				+ "runspec file as well.<br>"
				+ "</body></html>");

		dataSourceTitleLabel = new JLabel("Data Source:");
		dataSourceFileNameLabel = new JLabel();
		dataSourceTypeLabel = new JLabel();
		dataSourceWorksheetLabel = new JLabel();

		reloadButton = new JButton();
		reloadButton.setName("reloadButton");
		reloadButton.setText("Reload");
		reloadButton.addActionListener(this);
		ToolTipHelper.add(reloadButton,"Recapture the data within the shown file");

		usageNoteLabel = new JLabel(
				"<html><body>Note: The above file is not required during runtime and has<br>"
				+ "already been incorporated into this runspec model file.  If the data is<br>"
				+ "changed, use the \"Reload\" button to capture the changes.</body></html>");

		messageLabel = new JLabel("Messages:");
		messageListModel = new DefaultListModel<String>();
		messageList = new JListWithToolTips<String>(messageListModel);
		messageList.setName("messageList");
		messageList.setSelectedIndex(-1);
		messageList.setVisibleRowCount(10);
		messageList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXX");
		messagesScrollPane = new JScrollPane(messageList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		messagesScrollPane.setName("messagesScrollPane");
		messagesScrollPane.setVisible(true);
		ToolTipHelper.add(messagesScrollPane,"Displays messages, warnings, and errors");
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		Box b;
		JPanel t;
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(2,2,2,2);

		deleteAVFTDataPanel = new JPanel();
		deleteAVFTDataPanel.setLayout(new BoxLayout(deleteAVFTDataPanel, BoxLayout.Y_AXIS));

		t = new JPanel();
		t.setLayout(new GridBagLayout());

		gbc.gridwidth = 2;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		t.add(titleLabel,gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		t.add(new JLabel(""),gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 2, "WEST", 1, 1);
		t.add(useDeleteButtonLabel,gbc);

		b = Box.createHorizontalBox();
		b.add(t);
		b.add(Box.createHorizontalGlue());
		deleteAVFTDataPanel.add(b);

		loadedPanel = new JPanel();
		loadedPanel.setLayout(new BoxLayout(loadedPanel, BoxLayout.Y_AXIS));
		b = Box.createHorizontalBox();
		t = new JPanel();
		t.setLayout(new GridBagLayout());

		gbc.gridwidth = 2;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		t.add(titleLabelLoaded,gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		t.add(new JLabel(""),gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 2, "WEST", 1, 1);
		t.add(dataSourceTitleLabel,gbc);
		gbc.gridwidth = 1;
		LayoutUtility.setPositionOnGrid(gbc,2, 2, "WEST", 1, 1);
		t.add(reloadButton,gbc);
		gbc.gridwidth = 4;
		LayoutUtility.setPositionOnGrid(gbc,1, 3, "WEST", 1, 1);
		t.add(dataSourceFileNameLabel,gbc);
		LayoutUtility.setPositionOnGrid(gbc,1, 4, "WEST", 1, 1);
		t.add(dataSourceTypeLabel,gbc);
		LayoutUtility.setPositionOnGrid(gbc,1, 5, "WEST", 1, 1);
		t.add(dataSourceWorksheetLabel,gbc);

		gbc.gridwidth = 2;
		LayoutUtility.setPositionOnGrid(gbc,0, 6, "WEST", 2, 1);
		t.add(usageNoteLabel,gbc);
		gbc.gridwidth = 1;
		LayoutUtility.setPositionOnGrid(gbc,2, 6, "WEST", 1, 1);
		t.add(buttonNormalize,gbc);

		b = Box.createHorizontalBox();
		b.add(t);
		b.add(Box.createHorizontalGlue());
		loadedPanel.add(b);

		messagesPanel = new JPanel();
		messagesPanel.setLayout(new BoxLayout(messagesPanel, BoxLayout.Y_AXIS));
		b = Box.createHorizontalBox();
		b.add(messageLabel);
		b.add(Box.createHorizontalGlue());
		messagesPanel.add(b);
		messagesPanel.add(messagesScrollPane);

		setLayout(new BorderLayout());
		add(deleteAVFTDataPanel, BorderLayout.CENTER);
	}

	/**
	 * Find a row in tableFuelEngTechAssoc
	 * @param sourceId source type to search for
	 * @param fuelTypeId fuel type to search for
	 * @param engTechId engine technology to search for
	 * @return 0-based index into tableFuelEngTechAssoc or -1 if now match was found
	**/
	int findFetaRow( int sourceId , int fuelTypeId , int engTechId ) {
		return data.findFetaRow(sourceId,fuelTypeId,engTechId);
	}

	/**
	 * Populates droplist for SourceType
	**/
	void populateControls() {
		// Populate labels
		if(strategy.dataSourceFileName != null && strategy.dataSourceFileName.length() > 0) {
			dataSourceFileNameLabel.setText(strategy.dataSourceFileName);
			if(strategy.dataSourceFileType != null && strategy.dataSourceFileType.length() > 0) {
				dataSourceTypeLabel.setText(strategy.dataSourceFileType);
			} else {
				dataSourceTypeLabel.setText("");
			}
			if(strategy.dataSourceWorksheetName != null
					&& strategy.dataSourceWorksheetName.length() > 0) {
				dataSourceWorksheetLabel.setText(strategy.dataSourceWorksheetName);
			} else {
				dataSourceWorksheetLabel.setText("");
			}
		} else {
			dataSourceFileNameLabel.setText("");
			dataSourceTypeLabel.setText("");
			dataSourceWorksheetLabel.setText("");
		}

		// Populate messageList
		messageListModel.clear();
		for(Iterator i=strategy.messages.iterator();i.hasNext();) {
			String m = (String)i.next();
			messageListModel.addElement(m);
		}

		// Show proper panels
		setLayout(new BorderLayout());
		removeAll();
		if(strategy.dataSourceFileName != null && strategy.dataSourceFileName.length() > 0) {
			add(loadedPanel, BorderLayout.CENTER);
		} else {
			add(deleteAVFTDataPanel, BorderLayout.CENTER);
		}
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		data.getPrintableDescription(runspec,destination);
	}

	/**
	 * Saves the description text to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		data.recentRunSpec = runspec;
		// Nothing needs to be done since all data is already in desirable data structures
	}

	/**
	 * Loads the description text from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		data.recentRunSpec = runspec;
		// Nothing needs to be done since all data is already in desirable data structures
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,TreeMap sections) {
		data.recentRunSpec = runspec;
		return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
	}

	/**
	 * Obtain an explanation for a recent called to calculateRunSpecSectionStatus that
	 * yielded anything other than total success.
	 * @return human-readable text explaining the current state.
	**/
	public String explainRunSpecSectionStatus() {
		return data.inaccurateSumsExplanation;
	}

	/**
	 * Sets the defaults to the RunSpec.
	 * @param	runspec the RunSpec to the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,TreeMap sections) {
		data.recentRunSpec = runspec;
		// Nothing needs to be done here since defaults are handled elsewhere for this
		// type of InternalControlStrategy.
		return null;
	}

	/**
	 * Update current selections to be consistent with a newly selected ModelScale.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus onScaleChange(RunSpec runspec,TreeMap sections) {
		data.recentRunSpec = runspec;
		// Nothing special to do here
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/**
	 * Unloads the data from memory in preparation for a new set of data
	**/
	public void unload() {
		data.loadTablesFromFuelEngTechAssoc() ;
	}

	/**
	 * This method provides the functionality for the cancel button. It returns the memory
	 * in the tables back to the data as it was from the last Import or load.
	 * If the screen needs to be repainted, then it is the responsibility of the caller to
	 * execute this command
	**/
	public void cancel() {
		if ( data.resetXML != null ) {
			acceptXML( "AVFTPanel" , data.resetXML ) ;
		} else if ( data.resetTSV != null ) {
			acceptTSV( "AVFTPanel" , data.resetTSV ) ;
		} else if ( data.resetFromDefaultTables == true ) {
			data.loadTablesFromFuelEngTechAssoc() ;
		}
	}

	/**
	 * Callback routine implementing the ActionListener interface
	 * @param evt event with action details
	**/
	public void actionPerformed( ActionEvent evt ) {
		if(evt.getSource() == buttonNormalize) {
			processNormalizeButton();
		} else if(evt.getSource() == reloadButton) {
			processReloadButton();
		}
	}

	/** Handle the "Reload..." button **/
	void processReloadButton() {
		strategy.messages.clear();
		data.inaccurateSumsExplanation = null;

		if(data.recentRunSpec == null) {
			MOVESNavigation.singleton.updateRunSpecSectionStatus
				(MOVESNavigation.singleton.strategyOptions.get(0),this,false);
		}
		strategy.load(data.recentRunSpec,strategy.dataSourceFileName,strategy.dataSourceFileType,strategy.dataSourceWorksheetName);
		if(data.inaccurateSumsExplanation != null && data.inaccurateSumsExplanation.length() > 0) {
			strategy.messages.add(data.inaccurateSumsExplanation);
		} else {
			strategy.messages.add("Imported.");
		}

		populateControls();
		revalidate();
		pushStatusToDisplay();
	}

	/** Handle the action of the Normalize button **/
	void processNormalizeButton() {
		strategy.messages.clear();
		data.inaccurateSumsExplanation = null;

		boolean hadIssue = false;
		int colId = data.tableSourceUseType.getColumnIndex( "SourceTypeId" ) ;
		for(int i = 0 ; i < data.tableSourceUseType.getNumRowsActive() ; i++ ) {
			int id = data.tableSourceUseType.getInt(i,colId);
			data.normalizeSourceType(id);
			if(data.inaccurateSumsExplanation != null && data.inaccurateSumsExplanation.length() > 0) {
				strategy.messages.add(data.inaccurateSumsExplanation);
				data.inaccurateSumsExplanation = null;
				hadIssue = true;
			}
		}
		if(!hadIssue) {
			strategy.messages.add("Normalized.");
		}
		populateControls();
		revalidate();
		pushStatusToDisplay();
	}

	/**
	 * Process a user's request to initiate an import operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was imported.
	 * false if the import request should be handled by default logic instead.
	**/
	public boolean doImport(Frame ownerWindow) {
		strategy.messages.clear();
		data.inaccurateSumsExplanation = null;
		try {
			internalDoImport(ownerWindow);
			if(data.inaccurateSumsExplanation != null && data.inaccurateSumsExplanation.length() > 0) {
				strategy.messages.add(data.inaccurateSumsExplanation);
			} else {
				strategy.messages.add("Imported.");
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to import");
		}
		populateControls();
		revalidate();
		pushStatusToDisplay();
		return true;
	}

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @param saveDefaultData true if the default data should be saved, false if
	 * user-supplied data should be saved
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	public boolean doExport(Frame ownerWindow, boolean saveDefaultData) {
		try {
			internalDoExport(ownerWindow,saveDefaultData);
		} catch(Exception e) {
			Logger.logError(e,"Unable to export");
		}
		return true;
	}

	/**
	 * Process a user's request to initiate an import operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	**/
	void internalDoImport(Frame ownerWindow) throws Exception {
		if(data.recentRunSpec == null) {
			MOVESNavigation.singleton.updateRunSpecSectionStatus
				(MOVESNavigation.singleton.strategyOptions.get(0),this,false);
		}
		FileDialog fd = new FileDialog(ownerWindow, "Import AVFT", FileDialog.LOAD);
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
		strategy.dataSourceFileName = null; // clear this in case something goes wrong and we
											// cannot load the data
		strategy.dataSourceFileType = null;
		strategy.dataSourceWorksheetName = null;

		String extension = FileUtilities.getFileExtension(file,true);
		if(CellFile.formatAllowsWorksheets(CellFile.getFileType(file))) {
			// Microsoft Excel-format file, so prompt the user to select a worksheet
			XLSReader xls = new XLSReader();
			String contents = null;
			try {
				ArrayList worksheets = xls.getSheets(file);
				if(worksheets.size() == 1) {
					strategy.dataSourceWorksheetName = (String)worksheets.get(0);
				} else {
					// Prompt the user to select a worksheet then fill
					// strategy.dataSourceWorksheetName.  If the user cancels the worksheet
					// selection, stop the import operation
					Component parent = getParent();
					while(parent != null && !(parent instanceof Frame)) {
						parent = parent.getParent();
					}
					WorksheetChooserDialog dlg =
							new WorksheetChooserDialog((JFrame)parent,worksheets);
					// simple offset from main window origin
					dlg.setLocation(getLocationOnScreen().x + 50, getLocationOnScreen().y + 50);
					dlg.showModal();
					if(dlg.selectedWorksheetName != null
							&& dlg.selectedWorksheetName.length() > 0) {
						strategy.dataSourceWorksheetName = dlg.selectedWorksheetName;
					} else {
						return;
					}
				}
			} catch(Exception e) {
				throw new IOException(e.toString());
			}
			strategy.dataSourceFileType = "XLS";
		} else { // Likely a text file
			strategy.dataSourceFileType = "Text";
		}

		strategy.dataSourceFileName = file.getCanonicalPath();

		strategy.load(data.recentRunSpec,strategy.dataSourceFileName,strategy.dataSourceFileType,strategy.dataSourceWorksheetName);
	}

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @param saveDefaultData true if the default data should be saved, false if
	 * user-supplied data should be saved
	**/
	void internalDoExport(Frame ownerWindow, boolean saveDefaultData) throws Exception {
		if(data.recentRunSpec == null) {
			MOVESNavigation.singleton.updateRunSpecSectionStatus
				(MOVESNavigation.singleton.strategyOptions.get(0),this,false);
		}

		FileDialog fd = new FileDialog(ownerWindow, "Export AVFT", FileDialog.SAVE);
		fd.setVisible(true); //fd.show();

		if((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File file = new File(filePath);
		strategy.save(data.recentRunSpec,file,saveDefaultData);
	}

	/**
	 * Change the status shown on the navigation display.
	**/
	void pushStatusToDisplay() {
		MOVESNavigation.singleton.updateRunSpecSectionStatus
			(MOVESNavigation.singleton.strategyOptions.get(0),this,false);
	}
}
