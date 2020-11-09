/**************************************************************************************************
 * @(#)TableFileLinkagePart.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.util.*;
import java.io.*;
import java.sql.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.WorksheetChooserDialog;


/**
 * Object for editing the selection of a single file to populate a single table.
 *
 * @author		Wesley Faler
 * @version		2017-09-20
**/
public class TableFileLinkagePart extends JPanel implements IImporterPart, ActionListener {
	/** Interface for data providers and event handlers **/
	public interface IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		String getTableName();

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		boolean createTemplate(File destinationFile);
	}

	/** Interface for message display **/
	public interface IMessageHandler {
		/**
		 * Add and scroll to a new message line.
		 * @param text information to be displayed
		**/
		void addMessageLine(String text);
	}

	/** Interface for providers of table sets **/
	public interface ITableProvider {
		/**
		 * Get the table names to be offered
		 * @return the set of table names
		**/
		TreeSetIgnoreCase getTableNames();

		/**
		 * Event handler for notification of table name selection,
		 * either via the GUI or via XML.
		 * @param tableName table that has been selected
		**/
		void onTableNameChanged(String tableName);
	}

	/** IImporter for this object **/
	public IImporter importer;
	/** Data provider and event handler for this object **/
	public IProvider provider;
	/** Data provider for table sets **/
	public ITableProvider tableProvider;
	/** Label showing the current file name **/
	JLabel fileNameLabel;
	/** Button for browsing for files **/
	JButton browseButton;
	/** Label showing the type of file and, if an XLS file, the tab name within. **/
	JLabel fileTypeAndTabLabel;
	/** Button for creating a template XLS or template csv/tsv file(s). **/
	JButton createTemplateButton;
	/** Button for clearing all data **/
	JButton clearDataButton;

	/** Message handler **/
	public IMessageHandler messageHandler = null;

	/** Table to be affected **/
	public String tableName = "";
	/** Other tables silently managed by this part, such as the Link table for custom Zones when not in Project mode. **/
	public TreeSet<String> otherManagedTables = new TreeSet<String>();

	/** Full name and path of the file to be used **/
	public String fileName = "";
	/** Type of the file to be used, not stored in XML just used for display **/
	public String fileType = "";
	/** Section of the file to be used **/
	public String worksheetName = "";

	/** True if only a single table can be selected, false when the user must tell us the table **/
	public boolean isSingleTable = true;

	/** List box for table names **/
	ExtendedComboBox<String> tableNamesCombo;

	/** flag allowing display of a checkbox to intentionally suppress data **/
	boolean displayNoDataNeededCheckBox = false;
	/** Optional checkbox for intentionally not loading data **/
	JCheckBox noDataNeededCheckBox;
	/** Text to be used for noDataNeededCheckBox **/
	String noDataNeededText = "Data is not needed";

	/**
	 * Constructor
	 * @param importerToUse IImporter for this object
	 * @param providerToUse Data provider and event handler for this object
	**/
	public TableFileLinkagePart(IImporter importerToUse, IProvider providerToUse) {
		importer = importerToUse;
		provider = providerToUse;
		tableName = provider.getTableName();

		createControls();
	}

	/**
	 * Constructor
	 * @param importerToUse IImporter for this object
	 * @param providerToUse Data provider and event handler for this object
	 * @param tableProviderToUse Data provider for table sets
	**/
	public TableFileLinkagePart(IImporter importerToUse, IProvider providerToUse, ITableProvider tableProviderToUse) {
		this(importerToUse,providerToUse,tableProviderToUse,false,null);
	}

	/**
	 * Constructor
	 * @param importerToUse IImporter for this object
	 * @param providerToUse Data provider and event handler for this object
	 * @param tableProviderToUse Data provider for table sets
	 * @param displayNoDataNeededCheckBoxToUse flag allowing display of a checkbox to intentionally suppress data
	 * @param noDataNeededTextToUse Text to be used for noDataNeededCheckBox
	**/
	public TableFileLinkagePart(IImporter importerToUse, IProvider providerToUse, ITableProvider tableProviderToUse,
			boolean displayNoDataNeededCheckBoxToUse, String noDataNeededTextToUse) {
		importer = importerToUse;
		provider = providerToUse;
		tableProvider = tableProviderToUse;

		tableName = provider.getTableName();
		
		displayNoDataNeededCheckBox = displayNoDataNeededCheckBoxToUse;
		if(displayNoDataNeededCheckBox && noDataNeededTextToUse != null) {
			noDataNeededText = noDataNeededTextToUse;
		}

		if(tableProvider != null) {
			if(tableName == null) {
				isSingleTable = false;
			}
		}

		createControls();
		if(!isSingleTable) {
			String firstTableName = (String)tableProvider.getTableNames().first();
			tableNamesCombo.setSelectedItem(firstTableName);
			updateTableName();
		}
	}

	/**
	 * Get the actual JPanel to be shown
	 * @return the JPanel that reprents one portion of the importer
	**/
	public JPanel getPanel() {
		return this;
	}

	/**
	 * Push data from the importer to controls returned by getPanel().
	**/
	public void populateControls() {
		determineFileType();
		if(fileNameLabel == null || fileTypeAndTabLabel == null) {
			return;
		}
		if(fileName.length() > 0) {
			File f = new File(fileName);
			fileNameLabel.setText(f.getName());
			if(worksheetName.length() > 0) {
				fileTypeAndTabLabel.setText(fileType + ", " + worksheetName);
			} else {
				fileTypeAndTabLabel.setText(fileType);
			}
		} else {
			fileNameLabel.setText("(please select a file)");
			fileTypeAndTabLabel.setText("");
		}
	}

	/** Create and arrange the controls for this panel **/
	void createControls() {
		JLabel label1 = new JLabel();
		JLabel label2 = new JLabel();
		fileNameLabel = new JLabel();
		browseButton = new JButton();
		fileTypeAndTabLabel = new JLabel();
		createTemplateButton = new JButton();
		noDataNeededCheckBox = new JCheckBox(noDataNeededText);

		browseButton.addActionListener(this);
		createTemplateButton.addActionListener(this);

		clearDataButton = new JButton();
		ToolTipHelper.add(clearDataButton,"Remove previously imported data");
		clearDataButton.addActionListener(this);

		//======== this ========
		setLayout(new GridBagLayout());
		((GridBagLayout)getLayout()).columnWidths = new int[] {0, 0, 34, 0, 0};
		((GridBagLayout)getLayout()).rowHeights = new int[] {0, 0, 0, 0};
		((GridBagLayout)getLayout()).columnWeights = new double[] {0.0, 1.0, 0.0, 0.0, 1.0E-4};
		((GridBagLayout)getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4};

		//---- label1 ----
		if(isSingleTable) {
			label1.setText(tableName + " Data Source:");
			add(label1, new GridBagConstraints(0, 0, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
		} else {
			// Create and display a lable for table name and an extended combo box offering the set of tables
			tableNamesCombo = new ExtendedComboBox<String>();
			tableNamesCombo.setName("tableNamesCombo");
			tableNamesCombo.addActionListener(this);
			tableNamesCombo.setEditable(false);
			tableNamesCombo.setSelectedIndex(-1);
			ToolTipHelper.add(tableNamesCombo, "Select a table name");

			TreeSetIgnoreCase tableNames=tableProvider.getTableNames();

			for( Iterator i=tableNames.iterator(); i.hasNext(); ){
				String name=(String)i.next();
				tableNamesCombo.addItem(name);
			}

			JPanel oneLine = new JPanel();
			oneLine.setLayout(new BoxLayout(oneLine,BoxLayout.X_AXIS));
			oneLine.add(new JLabel("Table: "));
			oneLine.add(tableNamesCombo);
			oneLine.add(Box.createHorizontalGlue());

			add(oneLine, new GridBagConstraints(0, 0, 5, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
		}

		//---- label2 ----
		label2.setText("File:");
		add(label2, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- fileNameLabel ----
		fileNameLabel.setText("(please select a file)");
		add(fileNameLabel, new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 5), 0, 0));

		//---- noDataNeededCheckBox ----
		if(displayNoDataNeededCheckBox) {
			noDataNeededCheckBox.addActionListener(this);
			ToolTipHelper.add(noDataNeededCheckBox,"No data is imported because none is needed");
			add(noDataNeededCheckBox, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 5, 0), 0, 0));
		}
		//---- browseButton ----
		browseButton.setText("Browse...");
		ToolTipHelper.add(browseButton,"Select a file (.xls, .txt, .csv) containing data to import");
		add(browseButton, new GridBagConstraints(4, 1, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 5, 0), 0, 0));

		//---- fileTypeAndTabLabel ----
		fileTypeAndTabLabel.setText("");
		add(fileTypeAndTabLabel, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 5), 0, 0));

		//---- clearDataButton ----
		clearDataButton.setText("Clear Imported Data");
		add(clearDataButton, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 0), 0, 0));

		//---- createTemplateButton ----
		createTemplateButton.setText("Create Template...");
		ToolTipHelper.add(createTemplateButton,"Create a template a file (.xls, .txt, .csv) to be filled and then imported.");
		add(createTemplateButton, new GridBagConstraints(3, 2, 2, 1, 0.0, 0.0,
			GridBagConstraints.CENTER, GridBagConstraints.BOTH,
			new Insets(0, 0, 0, 0), 0, 0));
	}

	/**
	 * Calls the appropriate button handler.
	 * @param e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == browseButton) {
			updateTableName();
			if(tableName == null) {
				// Ask the user to select a table first
				JOptionPane.showMessageDialog(null,
						"Please select a table first.",
						"Table Needed", JOptionPane.ERROR_MESSAGE);
				return;
			} else {
				handleBrowseButton();
			}
		} else if(e.getSource() == createTemplateButton) {
			updateTableName();
			if(tableName == null) {
				// Ask the user to select a table first
				JOptionPane.showMessageDialog(null,
						"Please select a table first.",
						"Table Needed", JOptionPane.ERROR_MESSAGE);
				return;
			} else {
				handleCreateTemplateButton();
			}
		} else if(e.getSource() == clearDataButton) {
			updateTableName();
			if(tableName == null) {
				// Ask the user to select a table first
				JOptionPane.showMessageDialog(null,
						"Please select a table first.",
						"Table Needed", JOptionPane.ERROR_MESSAGE);
				return;
			} else {
				handleClearDataButton(false);
			}
		} else if(e.getSource() == noDataNeededCheckBox) {
			updateTableName();
			handleNoDataNeededCheckBox();
		} else if(tableNamesCombo != null && e.getSource() == tableNamesCombo) {
			updateTableName();
		}
	}

	/** Updates tableName internally, notifying tableProvider if it has changed **/
	void updateTableName() {
		if(tableNamesCombo == null || tableProvider == null) {
			return;
		}
		String oldTableName = tableName;
		tableName = (String)tableNamesCombo.getSelectedItem();
		if(tableName != null) {
			if(oldTableName == null || !oldTableName.equals(tableName)) {
				tableProvider.onTableNameChanged(tableName);
			}
		}
	}

	/** Handle a toggle of the noDataNeededCheckBox button **/
	void handleNoDataNeededCheckBox() {
		if(!displayNoDataNeededCheckBox || noDataNeededCheckBox == null) {
			return;
		}
		boolean enabledFlag = !noDataNeededCheckBox.isSelected();
		fileNameLabel.setEnabled(enabledFlag);
		browseButton.setEnabled(enabledFlag);
		fileTypeAndTabLabel.setEnabled(enabledFlag);

		if(noDataNeededCheckBox.isSelected()) {
			// Silently try to clear data
			handleClearDataButton(true);
			// Insert Ignore the flag into the audit log
			alterNoDataNeededAuditEntry(+1);
		} else {
			// Remove flag from the audit log
			alterNoDataNeededAuditEntry(-1);
		}

		importer.getImporterManager().createGUI(null).refreshDomainStatusIcons();
	}

	/**
	 * Obtain the status of the noDataNeeded checkbox.
	 * @return true if the user has indicated that no data is needed.
	**/
	public boolean isNoDataNeeded() {
		return displayNoDataNeededCheckBox && noDataNeededCheckBox.isSelected();
	}

	/** Handle the Browse button action **/
	void handleBrowseButton() {
		boolean keepOriginal = true;
		String originalFileName = fileName;
		String originalFileType = fileType;
		String originalWorksheetName = worksheetName;

		try {
			Component parent = getParent();
			while(parent != null && !(parent instanceof JFrame)) {
				parent = parent.getParent();
			}
			FileDialog fd = new FileDialog((JFrame)parent,
					"Open " + tableName + " Data", FileDialog.LOAD);
			fd.setVisible(true); //fd.show();
			if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
				return;
			}
			String filePath = fd.getDirectory() + fd.getFile();
			File file = new File(filePath);
			if(!file.exists()) {
				return;
			}
			worksheetName = "";
			fileName = file.getCanonicalPath();
			determineFileType();
			// If an XLS is selected, prompt for the worksheet name within
			if(fileType.equalsIgnoreCase("XLS")) {
				// Microsoft Excel-format file, so prompt the user to select a worksheet
				XLSReader xls = new XLSReader();
				try {
					ArrayList worksheets = xls.getSheets(file);
					if(worksheets.size() == 1) {
						worksheetName = (String)worksheets.get(0);
						keepOriginal = false;
					} else {
						boolean shouldPromptForWorksheetAssignment = true;

						// Get list of table names used by all parts
						ArrayList<String> allPartTableNames = new ArrayList<String>();
						IImporterPanel importerPanel = importer.getPanel();
						if(importerPanel instanceof ImporterTabBase) {
							ArrayList<IImporterPart> allParts = ((ImporterTabBase)importerPanel).importerDataSource.getParts();
							if(allParts.size() > 1) { // Only bother if this part isn't the only one.
								ArrayList<TableFileLinkagePart> linkageParts = new ArrayList<TableFileLinkagePart>();
								for(IImporterPart ap : allParts) {
									if(ap instanceof TableFileLinkagePart) {
										if(((TableFileLinkagePart)ap).isSingleTable) {
											linkageParts.add((TableFileLinkagePart)ap);
										}
									}
								}
								if(linkageParts.size() > 1) { // Only bother if this part isn't the only one.
									ArrayList<String> worksheetNamesToUse = new ArrayList<String>();
									int howManyMatchingSheetNames = 0;
									// Find candidate matches.
									for(TableFileLinkagePart lp : linkageParts) {
										String nameToUse = "";
										for(int i=0;i<worksheets.size();i++) {
											String wsName = (String)worksheets.get(i);
											if(wsName.equalsIgnoreCase(lp.tableName)) {
												nameToUse = wsName;
												howManyMatchingSheetNames++;
												break;
											}
										}
										worksheetNamesToUse.add(nameToUse);
									}
									if(linkageParts.size() == worksheetNamesToUse.size() && howManyMatchingSheetNames > 0) {
										// Prompt the user, but not on the Starts or Idle tab
										if(importerPanel.getImporter().getName() != "Starts" &&
										   importerPanel.getImporter().getName() != "Idle" &&
										   JOptionPane.showConfirmDialog((JFrame)parent, 
												"Do you want to automatically assign " + howManyMatchingSheetNames
												+ " worksheets to tables?", 
												"Assign worksheets?",
												JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
											shouldPromptForWorksheetAssignment = false;
											for(int i=0;i<linkageParts.size();i++) {
												TableFileLinkagePart lp = linkageParts.get(i);
												String wsName = worksheetNamesToUse.get(i);
												if(wsName.length() > 0) {
													if(lp == this) {
														worksheetName = wsName;
														keepOriginal = false;
													} else {
														lp.fileName = fileName;
														lp.fileType = fileType;
														lp.worksheetName = wsName;
														lp.determineFileType();
														lp.populateControls();
													}
												}
											}
										}
									}
								}
							}
						}

						if(shouldPromptForWorksheetAssignment) {
							// Prompt the user to select a worksheet then fill
							// strategy.dataSourceWorksheetName.  If the user cancels the worksheet
							// selection, stop the import operation
							WorksheetChooserDialog dlg = new WorksheetChooserDialog((JFrame)parent,worksheets);
							// simple offset from main window origin
							dlg.setLocation(getLocationOnScreen().x + 50, getLocationOnScreen().y + 50);
							dlg.showModal();
							if(dlg.selectedWorksheetName != null
									&& dlg.selectedWorksheetName.length() > 0) {
								worksheetName = dlg.selectedWorksheetName;
								keepOriginal = false;
							}
						}
					}
				} catch(Exception e) {
					Logger.logError(e,"Unable to choose worksheet within XLS file");
					return;
				}
			} else {
				keepOriginal = false;
			}
		} catch(IOException e) {
			// Nothing to do here
		} finally {
			if(keepOriginal) {
				fileName = originalFileName;
				fileType = originalFileType;
				worksheetName = originalWorksheetName;
			}
			determineFileType();
			if(!keepOriginal) {
				populateControls();
			}
		}
	}

	/** Handle the Create Template button action **/
	void handleCreateTemplateButton() {
		Component parent = getParent();
		while(parent != null && !(parent instanceof Frame)) {
			parent = parent.getParent();
		}
		FileDialog fd = new FileDialog((JFrame)parent,
				"Create " + tableName + " Template", FileDialog.SAVE);
		fd.setVisible(true); //fd.show();
		if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File file = new File(filePath);

		boolean shouldDoSingleTemplate = true;
		int typeCode = CellFile.getFileType(filePath);
		if(typeCode == CellFile.XLS || typeCode == CellFile.XLSX) {
			// Get list of table names used by all parts
			ArrayList<String> allPartTableNames = new ArrayList<String>();
			IImporterPanel importerPanel = importer.getPanel();
			if(importerPanel instanceof ImporterTabBase) {
				ArrayList<IImporterPart> allParts = ((ImporterTabBase)importerPanel).importerDataSource.getParts();
				if(allParts.size() > 1) { // Only bother if this part isn't the only one.
					ArrayList<TableFileLinkagePart> linkageParts = new ArrayList<TableFileLinkagePart>();
					for(IImporterPart ap : allParts) {
						if(ap instanceof TableFileLinkagePart) {
							if(((TableFileLinkagePart)ap).isSingleTable) {
								linkageParts.add((TableFileLinkagePart)ap);
							}
						}
					}
					if(linkageParts.size() > 1) { // Only bother if this part isn't the only one.
						// Prompt the user
						if(JOptionPane.showConfirmDialog((JFrame)parent, 
								"Do you want to create templates for all tables?", 
								"Create all templates?",
								JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
							try {
								setWaitCursor();
								CellFileWriter.startMultipleWrite(file);
								shouldDoSingleTemplate = false;
								boolean success = true;
								for(int i=0;i<linkageParts.size();i++) {
									TableFileLinkagePart lp = linkageParts.get(i);
									if(!lp.provider.createTemplate(file)) {
										success = false;
									}
								}
								CellFileWriter.stopMultipleWrite();
								if(!success) {
									JOptionPane.showMessageDialog(null,
											"Unable to create the templates.",
											"Importer", JOptionPane.ERROR_MESSAGE);
								}
							} finally {
								setDefaultCursor();
							}
						}
					}
				}
			}
		}

		if(shouldDoSingleTemplate) {
			CellFileWriter.stopMultipleWrite();
			if(!provider.createTemplate(file)) {
				JOptionPane.showMessageDialog(null,
						"Unable to create the template.",
						"Importer", JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	/**
	 * Generate XML describing this part.
	 * @return XML
	**/
	public String toXML() {
		if(!isVisible()) {
			return "";
		}
		String prefix = "\t\t\t\t";
		String xml = "";
		if(isSingleTable) {
			xml = prefix + "<" + tableName + ">\r\n";
		} else {
			xml = prefix + "<anytable>\r\n";
			xml += prefix + "\t<tablename>" + tableName + "</tablename>\r\n";
		}
		xml += prefix + "\t<filename>" + fileName + "</filename>\r\n";
		if(worksheetName != null && worksheetName.length() > 0
				&& fileType.equalsIgnoreCase("XLS")) {
			xml += prefix + "\t<section>" + worksheetName + "</section>\r\n";
		}
		if(isSingleTable) {
			xml += prefix + "</" + tableName + ">\r\n";
		} else {
			xml += prefix + "</anytable>\r\n";
		}
		return xml;
	}

	/**
	 * Setup the part from XML previously created by toXML().
	 * @param node XML node to be read
	 * @return true if the node was processed successfully, false if the node was
	 * not processed or not recognized.
	**/
	public boolean fromXML(Node node) {
		if(isSingleTable) {
			if(!node.getNodeName().equalsIgnoreCase(tableName)) {
				return false;
			}
		} else {
			if(!node.getNodeName().equalsIgnoreCase("anytable")) {
				return false;
			}
		}
		fileName = "";
		fileType = "";
		worksheetName = "";
		for(Node i=node.getFirstChild(); i != null; i = i.getNextSibling()) {
			if(i.getNodeName().equalsIgnoreCase("filename")) {
				Node childNode = i.getFirstChild();
				for(Node child = i.getFirstChild(); child != null;
						child = child.getNextSibling()) {
					if(child.getNodeType() == Node.TEXT_NODE) {
						fileName = StringUtilities.safeGetString(childNode.getNodeValue());
						break;
					}
				}
				continue;
			}
			if(i.getNodeName().equalsIgnoreCase("section")) {
				Node childNode = i.getFirstChild();
				for(Node child = i.getFirstChild(); child != null;
						child = child.getNextSibling()) {
					if(child.getNodeType() == Node.TEXT_NODE) {
						worksheetName = StringUtilities.safeGetString(childNode.getNodeValue());
						break;
					}
				}
				continue;
			}
			if(!isSingleTable && i.getNodeName().equalsIgnoreCase("tablename")) {
				Node childNode = i.getFirstChild();
				for(Node child = i.getFirstChild(); child != null;
						child = child.getNextSibling()) {
					if(child.getNodeType() == Node.TEXT_NODE) {
						tableName = StringUtilities.safeGetString(childNode.getNodeValue());
						tableProvider.onTableNameChanged(tableName);
						break;
					}
				}
				continue;
			}
		}
		determineFileType();
		return true;
	}

	/** Setup fileType and worksheetName based on fileName. **/
	void determineFileType() {
		fileName = StringUtilities.safeGetString(fileName);
		int typeCode = CellFile.getFileType(fileName);
		switch(typeCode) {
			case CellFile.XLS:
			case CellFile.XLSX:
				fileType = "XLS";
				break;
			case CellFile.CSV:
				fileType = "CSV";
				worksheetName = "";
				break;
			default:
			case CellFile.TABBED_TEXT:
				fileType = "Tabbed Text";
				worksheetName = "";
				break;
		}
	}

	/**
	 * Handle the Clear Data button action
	 * @param beSilent true to supress prompts
	**/
	void handleClearDataButton(boolean beSilent) {
		// Complain if there is no database selected
		if(!importer.getImporterManager().database.hasDatabase()) {
			if(!beSilent) {
				JOptionPane.showMessageDialog(null,
						"Please select a database first.",
						"Database Needed", JOptionPane.ERROR_MESSAGE);
			}
			return;
		}
		// Prompt for confirmation
		if(!beSilent) {
			int choice = JOptionPane.showConfirmDialog(null,
					"Are you sure you want to clear the data? ",
					"",JOptionPane.YES_NO_OPTION);
			if(choice != JOptionPane.YES_OPTION) {
				return;
			}
		}

		Connection db = importer.getImporterManager().openDatabase();
		if(db == null) {
			return;
		}
		importer.getImporterManager().activeDb = db;
		boolean success = false;
		boolean tableDoesExist = false;
		
		try {
			setWaitCursor();
			String sql = "";
			try {
				sql = "truncate table " + tableName;
				SQLRunner.executeSQL(db,sql);
				success = true;
				tableDoesExist = true;
				// Clear the log associated with the importer and table
				importer.getImporterManager().deleteLog(importer,tableName);
			} catch(Exception e) {
				if(e.getMessage().indexOf("exist") > 0) {
					// tableName may not exist, it may never have been created/filled, so tolerate its absense.
					success = true;
					tableDoesExist = false;
				} else {
					Logger.logError(e,"Unable to clear importer data for table " + tableName);
				}
			}

			for(Iterator<String> i=otherManagedTables.iterator();i.hasNext();) {
				sql = "truncate table " + i.next();
				try {
					SQLRunner.executeSQL(db,sql);
				} catch(Exception e) {
					// Nothing to do here as the supporting tables may not exist.
				}
			}

			// Tell the supporting GUI to refresh its log display
			if(success && tableDoesExist) {
				importer.getImporterManager().createGUI(null).loadLog(db);
				importer.getImporterManager().createGUI(null).refreshDomainStatusIcons();
			}
		} finally {
			setDefaultCursor();
			importer.getImporterManager().activeDb = null;
			DatabaseUtilities.closeConnection(db);
			db = null;
		}
		
		if(messageHandler != null) {
			if(success) {
				if(tableDoesExist) {
					messageHandler.addMessageLine("Data cleared for " + tableName);
				} else {
					if(!beSilent) {
						messageHandler.addMessageLine("Table " + tableName + " is clear because it has never been imported");
					}
				}
			} else {
				messageHandler.addMessageLine("Error clearing data for " + tableName);
			}
		}
	}

	/** Remove audit entry related to the no data needed checkbox **/
	void removeNoDataNeededAuditEntry() {
		// TODO
	}

	/** Add audit entry for the no data needed checkbox **/
	void addNoDataNeededAuditEntry() {
		// TODO
	}

	/**
	 * Add or remove the audit log entry for the no data needed checkbox.
	 * @param actionCode -1 to remove the entry, 0 to just read the existing entry, +1 to add the entry
	 * @return true if the audit log contains the no data needed flag after performing the action
	**/
	boolean alterNoDataNeededAuditEntry(int actionCode) {
		if(!importer.getImporterManager().database.hasDatabase()) {
			return false;
		}

		Connection db = importer.getImporterManager().openDatabase(false);
		if(db == null) {
			return false;
		}

		String name = importer.getName() + " Flag";
		String briefDescription = "No data needed";
		String fullDescription = "";
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();

		try {
			setWaitCursor();
			if(actionCode < 0) {
				// Delete from the audit log
				sql = "delete from auditLog"
						+ " where importerName=" + DatabaseUtilities.escapeSQL(name,true)
						+ " and briefDescription=" + DatabaseUtilities.escapeSQL(briefDescription,true);
				SQLRunner.executeSQL(db,sql);
				return false;
			} else if(actionCode > 0) {
				sql = "select * from auditLog"
						+ " where importerName=" + DatabaseUtilities.escapeSQL(name,true)
						+ " and briefDescription=" + DatabaseUtilities.escapeSQL(briefDescription,true);
				query.open(db,sql);
				if(!query.rs.next()) {
					query.close();

					sql = "insert into auditLog (whenHappened,importerName,"
							+ "briefDescription,fullDescription) "
							+ "values (now()"
							+ "," + DatabaseUtilities.escapeSQL(name,true)
							+ "," + DatabaseUtilities.escapeSQL(briefDescription,true)
							+ "," + DatabaseUtilities.escapeSQL(fullDescription,true)
							+ ")";
					SQLRunner.executeSQL(db,sql);
				}
				return true;
			}
			sql = "select * from auditLog"
					+ " where importerName=" + DatabaseUtilities.escapeSQL(name,true)
					+ " and briefDescription=" + DatabaseUtilities.escapeSQL(briefDescription,true);
			query.open(db,sql);
			return query.rs.next();
		} catch(Exception e) {
			Logger.logError(e,"Unable to read audit log data");
			return false;
		} finally {
			query.onFinally();
			DatabaseUtilities.closeConnection(db);
			db = null;
			setDefaultCursor();
		}
	}

	/**
	 * Refresh any state information from data stored in the audit log for an input database.
	 * @param db Database to use
	**/
	public void refreshFromAuditLog(Connection db) {
		if(!displayNoDataNeededCheckBox || noDataNeededCheckBox == null) {
			return;
		}
		noDataNeededCheckBox.setSelected(alterNoDataNeededAuditEntry(0));
	}
	
	/** Sets the wait cursor (don't have access to MOVESWindow, so it needs to be redefined here) **/
	public void setWaitCursor() {
		RootPaneContainer root = (RootPaneContainer) this.getRootPane().getTopLevelAncestor();
		root.getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		root.getGlassPane().setVisible(true);
	}
	
	/** Sets the default cursor (don't have access to MOVESWindow, so it needs to be redefined here) **/
	public void setDefaultCursor() {
		RootPaneContainer root = (RootPaneContainer) this.getRootPane().getTopLevelAncestor();
		root.getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		root.getGlassPane().setVisible(true);
	}
}
