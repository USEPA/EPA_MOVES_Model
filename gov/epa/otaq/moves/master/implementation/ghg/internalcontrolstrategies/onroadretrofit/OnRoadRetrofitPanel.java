/**************************************************************************************************
 * @(#)OnRoadRetrofitPanel.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit;

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
import java.io.*;
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

/**
 * Implements a Panel for On-Road Retrofit Modeling
 *
 * @author		Wesley Faler
 * @version		2012-09-27
**/
class OnRoadRetrofitPanel extends InternalControlStrategyPanel
		implements ActionListener, IImportExportHandler {
	/** A reference to the strategy that is being used for this execution **/
	OnRoadRetrofitStrategy strategy ;

	/** Title text **/
	JLabel titleLabel;

	/** Panel shown when no input has been specified **/
	JPanel notYetLoadedPanel;
	/** Label on notYetLoadedPanel giving instructions for the "Import..." button **/
	JLabel useImportButtonLabel;
	/** Panel shown Delete Retrofit data **/
	JPanel deleteDataPanel;
	/** Label to first export the old Retrofit data **/
	JLabel useDeleteButtonLabel;

	/** Panel shown when input, even if invalid, has been specified. **/
	JPanel loadedPanel;
	/** Checkbox on loadedPanel for enabling use of the retrofit settings **/
	JCheckBox useRetrofitSettings;
	/** "Data Source:" title on loadedPanel **/
	JLabel dataSourceTitleLabel;
	/** Original file name of the retrofit parameters **/
	JLabel dataSourceFileNameLabel;
	/** Type of the original retrofit parameters file **/
	JLabel dataSourceTypeLabel;
	/** If applicable, the name of the worksheet containing the retrofit parameters **/
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

	/**
	 * true after the constructor has completely finished.  Used to surpress the display
	 * of a warnings before the user even sees the screen.
	**/
	boolean isLoaded = false;

	/**
	 * Constructor
	 * @param strategyToUse the strategy that owns this panel
	**/
	public OnRoadRetrofitPanel(OnRoadRetrofitStrategy strategyToUse) {
		strategy = strategyToUse ;

		if(strategy == null) {
			return ;
		}

		try {
			createControls();
			arrangeControls();
			populateControls();
		} catch(Exception e) {
			Logger.logError(e,"Unable to construct OnRoadRetrofitPanel");
		}

		isLoaded = true;
	}

	/**
	 * Creates and initializes all controls on this panel. And fixes the height
	 * and width of the JTable to fit in the screen.
	**/
	public void createControls() {
		titleLabel = new JLabel("On-Road Vehicle Retrofit");

		useImportButtonLabel = new JLabel("Use the \"Import...\" button to select a"
				+ " retofit parameter file.");
		useDeleteButtonLabel = new JLabel("<html><body>"
				+ "This version of OnRoad Retrofit has been deprecated and this runspec<br>"
				+ "will no longer generate results.  To upgrade, first use the \"Export\" <br>"
				+ "button to create a XLS or XLSX file of your retrofit data.<br><br>"
				+ "Then, use the \"Retrofit Data\" importer to read the data<br>"
				+ "file into a new database.<br><br>"
				+ "Finally, return to this panel and use the \"Delete\" button to<br>"
				+ "remove the old data.<br><br>"
				+ "It is best practice to preserve the original version of this<br>"
				+ "runspec file as well.<br>"
				+ "</body></html>");

		useRetrofitSettings = new JCheckBox("Use the On-Road Retrofit Settings");
		useRetrofitSettings.setName("useRetrofitSettings");
		useRetrofitSettings.addActionListener(this);

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

		notYetLoadedPanel = new JPanel();
		loadedPanel = new JPanel();
		messagesPanel = new JPanel();

		deleteDataPanel = new JPanel();
		deleteDataPanel.setLayout(new BoxLayout(deleteDataPanel, BoxLayout.Y_AXIS));

		JPanel t = new JPanel();
		t.setLayout(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(2,2,2,2);
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
		deleteDataPanel.add(b);

		setLayout(new BorderLayout());
		add(deleteDataPanel, BorderLayout.CENTER);
	}

	/**
	 * Populates droplist for SourceType
	**/
	void populateControls() {
		useRetrofitSettings.setSelected(strategy.useParameters);

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
		add(deleteDataPanel, BorderLayout.CENTER);
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("\t\tOn-Road Vehicle Retrofit:\r\n");

		if(strategy.dataSourceFileName == null || strategy.dataSourceFileName.length() <= 0) {
			// No data has been specified
			destination.append("\t\t\tNo on-road retrofit parameters specified.  "
						+ "No on-road retrofit will be performed\r\n");
			return;
		}

		if(strategy.useParameters) {
			destination.append("\t\t\tOn-Road Retrofit has been enabled\r\n");
		} else {
			destination.append("\t\t\tOn-Road retrofit has been DISABLED\r\n");
		}

		// Data source information
		destination.append("\t\t\tData source file name:\r\n");
		destination.append("\t\t\t" + strategy.dataSourceFileName);

		if(strategy.dataSourceFileType != null && strategy.dataSourceFileType.length() > 0) {
			destination.append("\t\t\tData source file type:\r\n");
			destination.append("\t\t\t" + strategy.dataSourceFileType);
		}

		if(strategy.dataSourceWorksheetName != null
				&& strategy.dataSourceWorksheetName.length() > 0) {
			destination.append("\t\t\tData source worksheet name:\r\n");
			destination.append("\t\t\t" + strategy.dataSourceWorksheetName);
		}

		// Message information
		if(strategy.messages.size() <= 0) {
			destination.append("\t\t\tThere are no on-road retrofit messages, warnings, "
					+ "or errors\r\n");
		} else {
			destination.append("\t\t\tMessages:\r\n");
			for(Iterator i=strategy.messages.iterator();i.hasNext();) {
				destination.append(i.next().toString() + "\r\n");
			}
		}
	}

	/**
	 * Saves the description text to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		// Nothing needs to be done since all data is already in desirable data structures
	}

	/**
	 * Loads the description text from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		// Nothing needs to be done since all data is already in desirable data structures
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,TreeMap sections) {
		return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
	}

	/**
	 * Obtain an explanation for a recent called to calculateRunSpecSectionStatus that
	 * yielded anything other than total success.
	 * @return human-readable text explaining the current state.
	**/
	public String explainRunSpecSectionStatus() {
		if(strategy.messages.size() <= 0) {
			return "There are no issues with the current retrofit data.";
		}
		if(strategy.messages.size() == 1) {
			return "There is 1 issue with the current retrofit data.";
		}
		return "There are " + strategy.messages.size()
				+ " issues with the current retrofit data.";
	}

	/**
	 * Sets the defaults to the RunSpec.
	 * @param	runspec the RunSpec to the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,TreeMap sections) {
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
		// Nothing special to do here
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/**
	 * This method provides the functionality for the cancel button. It returns the memory
	 * in the tables back to the data as it was from the last Import or load.
	 * If the screen needs to be repainted, then it is the responsibility of the caller to
	 * execute this command
	**/
	public void cancel() {
		if(strategy.resetTSV != null) {
			strategy.acceptTSV("OnRoadRetrofitPanel",strategy.resetTSV);
			populateControls();
			revalidate();
		}
	}

	/**
	 * Callback routine implementing the ActionListener interface
	 * @param evt event with action details
	**/
	public void actionPerformed( ActionEvent evt ) {
		if(evt.getSource() == reloadButton) {
			processReloadButton();
		} else if(evt.getSource() == useRetrofitSettings) {
			processUseRetrofitCheckbox();
		}
		MOVESNavigation.singleton.updateRunSpecSectionStatus();
	}

	/** Handle the "Reload..." button **/
	void processReloadButton() {
		strategy.load(strategy.dataSourceFileName,strategy.dataSourceFileType,strategy.dataSourceWorksheetName);
		populateControls();
	}

	/** Handle a change to useRetrofitSettings **/
	void processUseRetrofitCheckbox() {
		strategy.useParameters = useRetrofitSettings.isSelected();
	}

	/**
	 * Process a user's request to initiate an import operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was imported.
	 * false if the import request should be handled by default logic instead.
	**/
	public boolean doImport(Frame ownerWindow) {
		try {
			internalDoImport(ownerWindow);
		} catch(Exception e) {
			Logger.logError(e,"Unable to import");
		}
		populateControls();
		revalidate();
		return true;
	}

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	public boolean doExport(Frame ownerWindow) {
		try {
			internalDoExport(ownerWindow);
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
		FileDialog fd = new FileDialog(ownerWindow, "Import On-Road Retrofit", FileDialog.LOAD);
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

		strategy.load(strategy.dataSourceFileName,strategy.dataSourceFileType,strategy.dataSourceWorksheetName);
	}

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	**/
	void internalDoExport(Frame ownerWindow) throws Exception {
		FileDialog fd = new FileDialog(ownerWindow, "Export On-Road Retrofit", FileDialog.SAVE);
		fd.setVisible(true); //fd.show();

		if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File file = new File(filePath);
		strategy.save(file);
	}
}
