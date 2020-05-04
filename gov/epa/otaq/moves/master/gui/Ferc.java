/**************************************************************************************************
 * @(#)Ferc.java
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
import java.util.TreeMap;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

/**
 * Creates, initializes, arranges, and sets the layouts of all the dialog controls
 * on the Update Future Emissions Panel.
 * Calls the appropriate button handler. The Execute button executes SQL
 * statements that produce EmissionRate and SourceBin tables containing 
 * the future and advanced technology emission rates.
 * The database sub-panel allows the user to enter a database name into a JTextField
 * This database will contain the new EmissionRate and SourceBin output tables.
 *
 * @author		EPA-Ed Glover
 * @author		EPA-Mitch C.
 * @version		2013-04-17
**/
public class Ferc extends JDialog implements ActionListener,
		FocusListener {
	/** The dialog result, indicates true on Execute button. **/
	public int result = 0;
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** Execute button. **/
	JButton executeButton;
	/** Cancel button. **/
	JButton cancelButton;

	/** FERC panel. **/
	JPanel fERCPanel;
	/** Short Term data input file panel. **/
	JPanel shortTermSharedFolderPathPanel;
	/** Long Term data input file panel. **/
	JPanel longTermSharedFolderPathPanel;

	/** FERC Server label. **/
	JLabel fERCServerLabel;
	/** FERC Server text control. **/
	JTextField fERCServer;
	/** FERC Database label. **/
	JLabel fERCDatabaseLabel;
	/** FERC Database textfield control. **/
	JTextField fERCDatabaseTextField;

	/** Short Term input file commands text control. **/
	JTextField shortTermSharedFolderPathName;
	/** Short Term input file commands browse button. **/
	JButton shortTermSharedFolderPathBrowse;

	/** Long Term input file commands text control. **/
	JTextField longTermSharedFolderPathName;
	/** Long Term input file commands browse button. **/
	JButton longTermSharedFolderPathBrowse;

	/**
	 * Used by the Server FocusLost event handler to help determine if the Server
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the Server TextField losing focus when the Server name
	 * hasn't changed..
	**/
	
	/** Contains the fERC Server name usually localhost **/
	String fERCPreviousServer;
	/** Contains the SQL string prior to execution **/
	String sql;
	/** Contains the short Term FERC file name **/
	String shortTermFERCFile;
	/** Contains the long Term FERC file name **/
	String longTermFERCFile;
	/** Contains the starting file directory from which to start browsing for
	    Short Term and Long Term FERC Input Files **/
	String defaultFERCFileSearchPath = "C:\\";
	
	/** Insures that a valid Short Term FERC file name has been chosen **/
	boolean isShortTermFERCFile;
	/** Insures that a valid Long Term FERC file name has been chosen **/
	boolean isLongTermFERCFile;
	/** Insures that a valid FERC Database name has been chosen **/
	boolean isDatabaseError;
	/** Name of MOVES default database **/
	String defaultDB;

	/**
	 * Constructs the FERC panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	**/
	public Ferc(JFrame parent) {
		super(parent, "Future Emission Rate Calculation in MOVES");
		frame = parent;

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(false);
		fERCPreviousServer = new String("");
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		pack();
		setModal(true);
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(-1,-1);
		setVisible(true); //show();
	}


	/**
	 * Creates and arranges all dialog controls.
	 * @return the container as JPanel.
	**/
	JPanel createPanel() {
		createControls();
		return arrangeControls();
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();
		executeButton = new JButton("Execute");
		executeButton.addActionListener(this);
		executeButton.setName("executeButton");
		ToolTipHelper.add(executeButton,"Execute the Future Emission Rate Calculator");
		cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(this);
		cancelButton.setName("cancelButton");
		ToolTipHelper.add(cancelButton, 
				"Exit Without Executing the Future Emission Rate Calculator");
		Dimension fERCPanelSize = new Dimension(450,90);
		Dimension shortTermFilePanelSize = new Dimension(450,60);
		Dimension longTermFilePanelSize = new Dimension(450,60);
		fERCPanel = new JPanel();
		fERCPanel.setName("fERCPanel");
		fERCPanel.setBorder(BorderFactory.createTitledBorder(
				"Future Emission Rate Database"));
		fERCPanel.setPreferredSize(fERCPanelSize);

		shortTermSharedFolderPathPanel = new JPanel();
		shortTermSharedFolderPathPanel.setName("longTermSharedFolderPathPanel");
		shortTermSharedFolderPathPanel.setBorder(BorderFactory.createTitledBorder(
				"Short-Term Future Emission Rate File"));
		shortTermSharedFolderPathPanel.setPreferredSize(shortTermFilePanelSize);
		

		longTermSharedFolderPathPanel = new JPanel();
		longTermSharedFolderPathPanel.setName("longTermSharedFolderPathPanel");
		longTermSharedFolderPathPanel.setBorder(BorderFactory.createTitledBorder(
				"Long-Term Future Emission Rate File"));
		longTermSharedFolderPathPanel.setPreferredSize(longTermFilePanelSize);

		fERCServerLabel = new JLabel("Server:");
		fERCServerLabel.setName("fERCServerLabel");
		fERCServer = new JTextField(20);
		fERCServer.setName("fERCServer");
		fERCServer.addFocusListener(this);
		fERCServer.setText(sysConfig.databaseSelections[
				MOVESDatabaseType.DEFAULT.getIndex()].serverName);
		ToolTipHelper.add(fERCServer,"Edit the name of the default database server");


		fERCDatabaseTextField = new JTextField(20);
		fERCDatabaseTextField.setName("fERCDatabaseTextField");
		fERCDatabaseLabel = new JLabel("Database:");
		fERCDatabaseLabel.setName("fERCDatabaseLabel");
		fERCDatabaseTextField.setEditable(true);
		ToolTipHelper.add(fERCDatabaseTextField,"Enter the Future Emission Rate Database");

		shortTermSharedFolderPathName = new JTextField(30);
		shortTermSharedFolderPathName.setName("shortTermSharedFolderPathName");
		ToolTipHelper.add(shortTermSharedFolderPathName,
				"Choose the Short Term Future Emission Rate File" +" workers");
		shortTermSharedFolderPathBrowse = new JButton("Browse...");
		ToolTipHelper.add(shortTermSharedFolderPathBrowse,
				"Select the Short Term Future Emission Rate File");
		shortTermSharedFolderPathBrowse.addActionListener(this);
		shortTermSharedFolderPathBrowse.setName("shortTermSharedFolderPathBrowse");


		longTermSharedFolderPathName = new JTextField(30);
		longTermSharedFolderPathName.setName("longTermSharedFolderPathName");
		ToolTipHelper.add(longTermSharedFolderPathName,
				"Choose the Long Term Future Emission Rate File" +" workers");
		longTermSharedFolderPathBrowse = new JButton("Browse...");
		ToolTipHelper.add(longTermSharedFolderPathBrowse,
				"Select the Long Term Future Emission Rate File");
		longTermSharedFolderPathBrowse.addActionListener(this);
		longTermSharedFolderPathBrowse.setName("longTermSharedFolderPathBrowse");
	}

	/**
	 * Sets the layout of the controls.
	 * @return the container as JPanel of the controls
	**/
	public JPanel arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		fERCPanel.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 2;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		fERCPanel.add(fERCServerLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
		fERCPanel.add(fERCServer, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		fERCPanel.add(fERCDatabaseLabel, gbc);
		gbc.weightx = 1.0;
		LayoutUtility.setPositionOnGrid(gbc,1, 1, "WEST", 1, 1);
		fERCPanel.add(fERCDatabaseTextField, gbc);
		gbc.weightx = 0;

		shortTermSharedFolderPathPanel.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		shortTermSharedFolderPathPanel.add(shortTermSharedFolderPathName, gbc);
		gbc.weightx = 1.0;
		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
		shortTermSharedFolderPathPanel.add(shortTermSharedFolderPathBrowse, gbc);
		gbc.weightx = 0;

		longTermSharedFolderPathPanel.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 2, 1);
		longTermSharedFolderPathPanel.add(longTermSharedFolderPathName, gbc);
		gbc.weightx = 1.0;
		LayoutUtility.setPositionOnGrid(gbc,2, 0, "WEST", 1, 1);
		longTermSharedFolderPathPanel.add(longTermSharedFolderPathBrowse, gbc);
		gbc.weightx = 0;

		JPanel result = new JPanel();
		result.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 3;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		result.add(fERCPanel,gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		result.add(shortTermSharedFolderPathPanel,gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		result.add(longTermSharedFolderPathPanel, gbc);
		JPanel buttonPanel = createButtonsPanel();
		LayoutUtility.setPositionOnGrid(gbc,1,0, "NORTH", 1, 3);
		result.add(buttonPanel, gbc);

		return result;
	}

	/**
	 * Creates a panel containing the Execute and Cancel buttons.
	 * @return the container as JPanel.
	**/
	JPanel createButtonsPanel() {
		JPanel box = new JPanel();
		box.setLayout(new GridLayout(2,1,5,5)); // 2 rows, 1 column, 5 pixel gaps
		box.add(executeButton);
		box.add(cancelButton);

		JPanel result = new JPanel();
		result.setLayout(new BorderLayout());
		result.add(box, BorderLayout.NORTH);
		return result;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == longTermSharedFolderPathBrowse) {
			longTermProcessSharedFolderPathBrowseButton();
		} else if(e.getSource() == shortTermSharedFolderPathBrowse) {
			shortTermProcessSharedFolderPathBrowseButton();
		} else if(e.getSource() == executeButton) {
			handleExecuteButton();
		} else if(e.getSource() == cancelButton) {
			handleCancelButton();
		}
	}

	/**
	 * Execute button handler, will execute the Future Emission Rate Script  
	 * before exiting.
	**/
	void handleExecuteButton() {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();

		// Check the default server and database
		String testServerName = fERCServer.getText();
		String testDatabaseName =
				fERCDatabaseTextField.getText();		
		DatabaseSelection executionDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()];
		DatabaseSelection defaultDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
													
		if((executionDB.databaseName).equalsIgnoreCase(testDatabaseName)) {
			JOptionPane.showMessageDialog(this,
					"The Future Emission Rates cannot be inserted into the execution database.");
			return;
		}
		if((defaultDB.databaseName).equalsIgnoreCase(testDatabaseName)) {
			JOptionPane.showMessageDialog(this,
					"The Future Emission Rates cannot be inserted into the MOVES default database.");
			return;
		}

		// indicates Execute button
		result = 1;
		dispose();
		
		createFERCDatabase();
		
		DatabaseSelection dbSelection = new DatabaseSelection();
		dbSelection.serverName = testServerName;
		dbSelection.databaseName = testDatabaseName;
		Connection db = dbSelection.openConnectionOrNull();
			
		if(isShortTermFERCFile & isLongTermFERCFile & !isDatabaseError) {			
				doLoadFERFiles(db);
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
					"Future Emission Rate Input database successfully created.", 60));				
		}else if(!isShortTermFERCFile || !isLongTermFERCFile) {
			JOptionPane.showMessageDialog((Component) null, 
					"A Required Future Emission Rate Data File was NOT Specified",
					"WARNING:  Future Emission Rate Calculation Failed", 
					JOptionPane.PLAIN_MESSAGE);		
		}				
	}

	/**
	 * Cancel button handler, will cancel the Future Emission Rate Calculation and
	 * reset all data file and database choices before closing this dialog.
	**/
	void handleCancelButton() {
		// indicates Cancel button
		result = 0;
		dispose();
	}

	/**
	 * Handles the focus lost event for the fERCServer & outputServer textfields,
	 * and the fERCDatabaseTextField.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		SystemConfiguration sysConfig = SystemConfiguration.getTheSystemConfiguration();
		JComponent c = (JComponent)e.getComponent();
		if(c == fERCServer) {
			if(fERCPreviousServer.equals(fERCServer.getText())) {
				return;
			}
			fERCPreviousServer = fERCServer.getText();
		} 
	}
	
	/**
	 * Currently not used.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusGained(FocusEvent e) {
	}


	/** Handles the Long Term Shared Folder Path Browse button. **/
	public void longTermProcessSharedFolderPathBrowseButton() {
		JFileChooser chooser = new JFileChooser(defaultFERCFileSearchPath);
		isLongTermFERCFile = false;
		chooser.setFileFilter(new FileFilter() {
			public boolean accept(File f) {
				return f.isDirectory() || WildCardFileNameFilter.doesFilterMatch("*.csv",f.getName());
			}
			public String getDescription() {
				return "*.csv";
			}
		});
		int returnVal = chooser.showOpenDialog(this);
		defaultFERCFileSearchPath = chooser.getSelectedFile().getPath();
		if(returnVal == JFileChooser.APPROVE_OPTION) {
			longTermSharedFolderPathName.setText(chooser.getSelectedFile().getPath());
			File fileobjLong = chooser.getSelectedFile();						
			try {
				longTermFERCFile = fileobjLong.getCanonicalPath();
				isLongTermFERCFile = true;
			} catch(Exception e) {
				Logger.logError(e, "Failed to read the path of the Long Term FER File");
			}	
		}
	}
		
		
		/** Handles the Short Term Shared Folder Path Browse button. **/
	public void shortTermProcessSharedFolderPathBrowseButton() {
		JFileChooser chooser = new JFileChooser(defaultFERCFileSearchPath);
		isShortTermFERCFile = false;
		chooser.setFileFilter(new FileFilter() {
			public boolean accept(File f) {
				return f.isDirectory() || WildCardFileNameFilter.doesFilterMatch("*.csv",f.getName());
			}
			public String getDescription() {
				return "*.csv";
			}
		});
		int returnVal = chooser.showOpenDialog(this);
		defaultFERCFileSearchPath = chooser.getSelectedFile().getPath();
		if(returnVal == JFileChooser.APPROVE_OPTION) {
			shortTermSharedFolderPathName.setText(chooser.getSelectedFile().getPath());
			File fileobjShort = chooser.getSelectedFile();
			try {
				shortTermFERCFile = fileobjShort.getCanonicalPath();
				isShortTermFERCFile = true;	
			} catch(Exception e) {
				Logger.logError(e, "Failed to read the path of the Short Term FER File");
			}					
		}
	}

	/** createFERCDatabase creates a MySQL database named by the user that holds 
	 *  the Future Emission Rates.  This database will contain a MySQL table
	 *  called EmissionRates that can be specified as a user input database in		
	 *  the ManageInputDataSets routines. 		
	**/
	public void createFERCDatabase() {
				
		String testDatabaseName =
				fERCDatabaseTextField.getText();
		if(testDatabaseName.length() == 0) {
			Logger.log(LogMessageCategory.WARNING, "Specify a database name.");
		}
	
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
							
			try {
				sql="CREATE DATABASE IF NOT EXISTS " + testDatabaseName;
				SQLRunner.executeSQL(db,sql);				
				isDatabaseError = false;
			}catch(Exception e) {
				isDatabaseError = true;
				return;
			}
	}
	
	
	/**
	 * This method implements the Future Emission Rate Calculator. 
	 * It uses the current database for temp storage and to produce a 
	 * new EmissionRate table that contains the future emission rates.  
     * These are suitable for use as MOVES2004 User Input databases.
	 * 
	 * @param db The database connection
	**/
	void doLoadFERFiles(Connection db) {
		defaultDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
		// System.out.println(defaultDB);
		try {
			
			sql="FLUSH TABLES ";
			SQLRunner.executeSQL(db,sql);
				
			sql="DROP TABLE IF EXISTS ShortTermFERAdjustment ";
			SQLRunner.executeSQL(db,sql);
			
			sql="DROP TABLE IF EXISTS LongTermFERAdjustment ";
			SQLRunner.executeSQL(db,sql);
			
			sql="DROP TABLE IF EXISTS ShortTermFERAdjustment2 ";
			SQLRunner.executeSQL(db,sql);
			
			sql="DROP TABLE IF EXISTS LongTermFERAdjustment2 ";
			SQLRunner.executeSQL(db,sql);
			
			sql="DROP TABLE IF EXISTS  EmissionRate";
			SQLRunner.executeSQL(db,sql);
			
			sql="DROP TABLE IF EXISTS  BaseEmissionRate";
			SQLRunner.executeSQL(db,sql);
			
			sql=" CREATE TABLE ShortTermFERAdjustment ( "  	+
  				" `polProcessID` INT, " 					+ 
  				" `opModeID` SMALLINT, "					+
  				" `targetFuelTypeID` SMALLINT, "			+ 
  				" `targetEngTechID` SMALLINT, "				+
  				" `targetModelYearGroupID` INTEGER, "		+
  				" `fuelTypeID` SMALLINT, "					+ 
  				" `engTechID` SMALLINT, "					+
  				" `modelYearGroupID` INTEGER, "				+
  				" `fuelEngAdjust` FLOAT, "					+
  				" `dataSourceID` SMALLINT ) ";
  			SQLRunner.executeSQL(db,sql);	
  			
  			sql=" CREATE TABLE LongTermFERAdjustment ( "   	+
  				" `polProcessID` INT, "						+
  				" `targetFuelTypeID` SMALLINT, "			+ 
  				" `targetEngTechID` SMALLINT, "				+
  				" `targetModelYearGroupID` INTEGER, " 		+
  				" `fuelTypeID` SMALLINT, "					+
  				" `engTechID` SMALLINT, "					+
  				" `modelYearGroupID` INTEGER, "				+
  				" `fuelEngAdjust` FLOAT, "					+
  				" `dataSourceID` SMALLINT ) ";
  			SQLRunner.executeSQL(db,sql);
  			

			sql = "LOAD DATA INFILE " 
					+ DatabaseUtilities.escapeSQL(shortTermFERCFile,true)
					+ " INTO TABLE ShortTermFERAdjustment "
					+ " FIELDS TERMINATED BY ',' "
					+ " IGNORE 1 LINES ";				
			SQLRunner.executeSQL(db, sql);
		
			
			sql = "LOAD DATA INFILE " 
					+ DatabaseUtilities.escapeSQL(longTermFERCFile,true)
					+ " INTO TABLE LongTermFERAdjustment "
					+ " FIELDS TERMINATED BY ',' "
					+ " IGNORE 1 LINES ";										
			SQLRunner.executeSQL(db, sql);
	  		
	  			  		
			sql = "CREATE INDEX index1 ON ShortTermFERAdjustment(modelYearGroupID)";  	
			SQLRunner.executeSQL(db, sql);
						
			sql = 	"CREATE TABLE ShortTermFERAdjustment2 " + 
					"SELECT sta.*, myg.shortModYrGroupID " +
					"FROM ShortTermFERAdjustment AS sta INNER JOIN " +
					defaultDB + ".ModelYearGroup AS myg " + 
					"ON sta.targetModelYearGroupID = myg.modelYearGroupID ";
			SQLRunner.executeSQL(db, sql);	
					
			sql = 	"CREATE INDEX index1 on ShortTermFERAdjustment2 " +  
					"(polProcessID, opModeID, fuelTypeID, engTechID, modelYearGroupID) ";			
			SQLRunner.executeSQL(db, sql);
			
			sql = 	"CREATE TABLE BaseEmissionRate " +
					"SELECT er.sourceBinID, polProcessID, opModeID, fuelTypeID, " +
					"engTechID, modelYearGroupID,regClassID, engSizeID, " + 
					"weightClassID, meanBaseRate " + 
					"FROM " + defaultDB + ".EmissionRate AS er INNER JOIN " + 
					defaultDB + ".SourceBin USING(sourceBinID) " + 
					"WHERE regClassID <> 10 AND dataSourceID < 6000 ";
			SQLRunner.executeSQL(db, sql);
					
			sql = 	"CREATE INDEX index1 on BaseEmissionRate " + 
					"(polProcessID, opModeID, fuelTypeID, engTechID, modelYearGroupID) ";
			SQLRunner.executeSQL(db, sql);
			
			sql = 	"CREATE TABLE EmissionRate ( " +
						"sourceBinID BIGINT, " +
						"polProcessID INT, " +
						"opModeID SMALLINT, " +
						"meanBaseRate FLOAT, " +
						"meanBaseRateCV FLOAT, " +
						"meanBaseRateIM FLOAT, " +
						"meanBaseRateIMCV FLOAT, " +
						"dataSourceID SMALLINT, " +
						"fuelTypeID SMALLINT, " +
						"engTechID SMALLINT, " +
						"regClassID SMALLINT, " +
						"modelYearGroupID INTEGER, " +
						"engSizeID SMALLINT, " +
						"weightClassID SMALLINT ) ";
			SQLRunner.executeSQL(db, sql);
			
			sql = 	"INSERT INTO EmissionRate (sourceBinID, polProcessID, opModeID, " +
					"meanBaseRate, meanBaseRateCV, dataSourceID, fuelTypeID, engTechID, " + 
					"regClassID, modelYearGroupID, engSizeID, weightClassID) " +
					"SELECT (1000000000000000000 + sta.targetFuelTypeID*10000000000000000 + " +  
					"sta.targetEngTechID*100000000000000 + ber.regclassid*1000000000000 + " + 
					"sta.shortModYrGroupID*10000000000 + ber.engSizeID*1000000 +  " +
					"ber.weightClassID*100) AS sourceBinID, " +
					"sta.polProcessID, ber.opModeID, " + 
					"(ber.meanBaseRate * sta.fuelEngAdjust) AS meanBaseRate, " +
					"NULL AS meanBaseRateCV, sta.dataSourceID, " +
					"sta.targetFuelTypeID, sta.targetEngTechID, ber.regClassID, " +
					"sta.targetModelYearGroupID, ber.engSizeID, ber.weightClassID " +
					"FROM ShortTermFERAdjustment2 AS sta INNER JOIN  " +
					"BaseEmissionRate AS ber " +
					"USING (polProcessID, opModeID, fuelTypeID, engTechID, " +
					"modelYearGroupID) ";
			SQLRunner.executeSQL(db, sql);
			
			sql = "DROP INDEX index1 ON BaseEmissionRate ";
			SQLRunner.executeSQL(db, sql);
			
			sql =	"CREATE UNIQUE INDEX index2 ON BaseEmissionRate " +
					"(sourceBinID, polProcessID, opModeID) ";
			SQLRunner.executeSQL(db, sql);
					
			sql =	"REPLACE INTO BaseEmissionRate " + 
					"(sourceBinID, polProcessID, opModeID, " + 
					"fuelTypeID, engTechID, regClassiD, " +
					"modelYearGroupID, engSizeID, weightClassID, meanBaseRate) " +		
					"SELECT sourceBinID, polProcessID, opModeID, fuelTypeID, " +
					"engTechID, regClassID,modelYearGroupID, engSizeID, " +
					"weightClassID, meanBaseRate " +
					"FROM EmissionRate ";
			SQLRunner.executeSQL(db, sql);
			
			sql = 	"CREATE INDEX index4 ON BaseEmissionRate  " +
					"(polProcessID, fuelTypeID, engTechID, modelYearGroupID) ";
			SQLRunner.executeSQL(db, sql);
			
			
			sql = 	"INSERT INTO BaseEmissionRate " +
					"SELECT EmissionRate.sourceBinID, " + 
					"polProcessID, opModeID, fuelTypeID, engTechID, " +
					"modelYearGroupID, regClassID, engSizeID, weightClassID, " + 
					"meanBaseRate  " + 
					"FROM " + defaultDB + ".EmissionRate INNER JOIN  " +
					defaultDB + ".SourceBin USING(sourceBinID) " + 
					"WHERE regClassID = 10 AND dataSourceID < 6000 ";	
			SQLRunner.executeSQL(db, sql);
			
			
			sql =	"CREATE INDEX index1 ON LongTermFERAdjustment(modelYearGroupID) ";
			SQLRunner.executeSQL(db, sql);  


			sql = 	"CREATE TABLE LongTermFERAdjustment2 " +
					"SELECT sta.*, myg.shortModYrGroupID " +
					"FROM LongTermFERAdjustment AS sta INNER JOIN  " +
					defaultDB + ".ModelYearGroup AS myg " +
					"ON sta.targetModelYearGroupID = myg.modelYearGroupID " ;
			SQLRunner.executeSQL(db, sql);
					
			sql =	"CREATE INDEX index1 on LongTermFERAdjustment2 " + 
					"(polProcessID, fuelTypeID, engTechID, modelYearGroupID) ";
			SQLRunner.executeSQL(db, sql);
			
					
			sql =	"CREATE UNIQUE INDEX index3 ON EmissionRate " +
					"(sourceBinID, polProcessID, opModeID) ";
			SQLRunner.executeSQL(db, sql);
						
			sql =	"REPLACE INTO EmissionRate (sourceBinID, polProcessID, opModeID, " +
					"meanBaseRate, meanBaseRateCV, dataSourceID, " +
					"fuelTypeID, engTechID, regClassID, " +
					"modelYearGroupID, engSizeID, weightClassID) " +
					"SELECT (1000000000000000000 + lta.targetFuelTypeID*10000000000000000 + " + 
					"lta.targetEngTechID*100000000000000 + ber.regClassid*1000000000000 + " +  
					"lta.shortModYrGroupID*10000000000 + ber.engSizeID*1000000 + " +
					"ber.weightClassID*100) AS sourceBinID, " +
					"lta.polProcessID, ber.opModeID, " +
					"(ber.meanBaseRate * lta.fuelEngAdjust) AS meanBaseRate, " +
					"NULL AS meanBaseRateCV, lta.dataSourceID, " +
					"lta.targetFuelTypeID, lta.targetEngTechID, ber.regClassID, " +
					"lta.targetModelYearGroupID, ber.engSizeID, ber.weightClassID " +
					"FROM LongTermFERAdjustment2 AS lta INNER JOIN  " +
					"BaseEmissionRate AS ber  " +
					"USING (polProcessID, fuelTypeID, engTechID, modelYearGroupID) ";
			SQLRunner.executeSQL(db, sql);
			
			sql = "DROP TABLE IF EXISTS SourceBin" ;
			SQLRunner.executeSQL(db, sql);
			
			sql = "CREATE TABLE SourceBin ( " +
					"sourceBinID BIGINT, " +
					"fuelTypeID SMALLINT, " +
					"engTechID SMALLINT, " +
					"regClassID SMALLINT, " +
					"modelYearGroupID INTEGER, " +
					"engSizeID SMALLINT, " +
					"weightClassID SMALLINT)"; 
			SQLRunner.executeSQL(db, sql);

			sql = "INSERT INTO SourceBin " +
					"SELECT sourceBinID, fuelTypeID, engTechID, regClassID, " +
					"modelYearGroupID, engSizeID, weightClassID " +
					"FROM EmissionRate GROUP BY sourceBinID ";
			SQLRunner.executeSQL(db, sql);

			sql =	"ALTER TABLE EmissionRate " +
					"DROP COLUMN fuelTypeID, " +
					"DROP COLUMN engTechID, " +
					"DROP COLUMN regClassID, " +
					"DROP COLUMN modelYearGroupID, " +
					"DROP COLUMN engSizeID, " +
					"DROP COLUMN weightClassID "; 
			SQLRunner.executeSQL(db, sql);
/*				
			sql="DROP TABLE IF EXISTS ShortTermFERAdjustment ";
			SQLRunner.executeSQL(db,sql);
			
			sql="DROP TABLE IF EXISTS LongTermFERAdjustment ";
			SQLRunner.executeSQL(db,sql);
			
			sql="DROP TABLE IF EXISTS ShortTermFERAdjustment2 ";
			SQLRunner.executeSQL(db,sql);
			
			sql="DROP TABLE IF EXISTS LongTermFERAdjustment2 ";
			SQLRunner.executeSQL(db,sql);

			sql="DROP TABLE IF EXISTS  BaseEmissionRate";
			SQLRunner.executeSQL(db,sql);
*/
			sql="FLUSH TABLES ";
			SQLRunner.executeSQL(db,sql);
			
		} catch (SQLException e) {
			Logger.logSqlError(e, "SQL error in FER Adjustment File Loading", sql);
		}
	}	
}
