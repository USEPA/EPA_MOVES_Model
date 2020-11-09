/**************************************************************************************************
 * @(#)VehicleTypeVMTImporter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;

import java.io.*;
import java.sql.*;
import java.util.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.implementation.ghg.TotalActivityGenerator;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;

/**
 * MOVES HPMSVTypeYear Importer.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @version		2015-09-16
**/
public class VehicleTypeVMTImporter extends ImporterBase {
	class TableSubsetSelectorPart extends JPanel implements IImporterPart, ActionListener {
		/** Importer that owns this panel **/
		VehicleTypeVMTImporter owner;
		/** IImporter for this object **/
		public IImporter importer;
		/** Group box holding radio buttons for HPMS and Source Type **/
		JPanel hpmsVsSourceTypePanel;
		/** Radio button holder for HPMS and Source Type **/
		ButtonGroup hpmsVsSourceButtons;
		/** HPMS radio button **/
		JRadioButton hpms;
		/** Source type radio button **/
		JRadioButton sourceType;

		/** Group box holding radio buttons for Annual and Daily **/
		JPanel annualVsDayPanel;
		/** Radio button holder for Annual and Daily **/
		ButtonGroup annualVsDayButtons;
		/** Annual radio button **/
		JRadioButton annual;
		/** Daily radio button **/
		JRadioButton daily;

		/** Button to clear all imported VMT-shaping data **/
		JButton clearAll;

		/** true when data is present in HPMSVtypeYear **/
		boolean hasHPMSAnnual = false;
		/** true when data is present in SourceTypeYearVMT **/
		boolean hasSourceTypeAnnual = false;
		/** true when data is present in HPMSVtypeDay **/
		boolean hasHPMSDaily = false;
		/** true when data is present in SourceTypeDayVMT **/
		boolean hasSourceTypeDaily = false;
		/** true when data is present in MonthVMTFraction **/
		boolean hasMonthVMTFraction = false;
		/** true when data is present in DayVMTFraction **/
		boolean hasDayVMTFraction = false;

		/** greater than 0 when altering radio buttons programmatically **/
		int isChangingControls = 0;

		/**
		 * Constructor
		 * @param importerToUse IImporter for this object
		 * @param ownerToUse Importer that owns this panel
		**/
		public TableSubsetSelectorPart(IImporter importerToUse, VehicleTypeVMTImporter ownerToUse) {
			importer = importerToUse;
			owner = ownerToUse;
	
			isChangingControls++;
			createControls();
			isChangingControls--;
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
			populateControls(null);
		}

		/**
		 * Push data from the importer to controls returned by getPanel().
		 * @param dbToUse database connection to be used, may be null.
		**/
		public void populateControls(Connection dbToUse) {
			/*
			 * Once data is present, the controls are disabled until cleared. Needs callback about cleared data.
			 * This gives no chance of GUI being in a different state than the database.
			*/
			if(isChangingControls > 0) {
				return;
			}
			isChangingControls++;
			//Logger.log(LogMessageCategory.DEBUG,"VehicleTypeVMTImporter.populateControls");
			assessSituation(dbToUse);

			complain();

			// If data is present in the database, then set the GUI accordingly.
			if(hasHPMSAnnual || hasHPMSDaily || hasSourceTypeAnnual || hasSourceTypeDaily) {
				if(hasHPMSAnnual || hasHPMSDaily) {
					sourceType.setSelected(false);
					hpms.setSelected(true);
				} else {
					hpms.setSelected(false);
					sourceType.setSelected(true);
				}
				if(hasHPMSAnnual || hasSourceTypeAnnual) {
					daily.setSelected(false);
					annual.setSelected(true);
				} else {
					annual.setSelected(false);
					daily.setSelected(true);
				}
				sourceType.setEnabled(false);
				hpms.setEnabled(false);
				annual.setEnabled(false);
				daily.setEnabled(false);
			} else if(hasMonthVMTFraction || hasDayVMTFraction) {
				// Data is present in MonthVMTFraction or DayVMTFraction, but no other data has been imported.
				// Unlock the HPMS/SourceType category and lock in the annual choice.
				daily.setSelected(false);
				annual.setSelected(true);

				sourceType.setEnabled(true);
				hpms.setEnabled(true);
				annual.setEnabled(false);
				daily.setEnabled(false);
			} else {
				// No data is present in the database, so unlock the GUI.
				// Ensure a radio button is selected in each category.
				sourceType.setEnabled(true);
				hpms.setEnabled(true);
				annual.setEnabled(true);
				daily.setEnabled(true);

				if(!hpms.isSelected() && !sourceType.isSelected()) {
					hpms.setSelected(true);
				}
				if(!annual.isSelected() && !daily.isSelected()) {
					annual.setSelected(true);
				}
			}
			showPanels();
			isChangingControls--;
		}

		/**
		 * Add a message to the owner's message display.
		 * @param message text to be shown.
		**/
		void addMessage(String message) {
			if(!owner.messages.contains(message)) {
				//Logger.log(LogMessageCategory.DEBUG,"VehicleTypeVMTImporter.addMessage " + message);
				owner.messages.add(message);
			} else {
				//Logger.log(LogMessageCategory.DEBUG,"VehicleTypeVMTImporter.!addMessage " + message);
			}
		}

		/**
		 * Remove a message from the owner's message display.
		 * @param message text to be hidden.
		**/
		void removeMessage(String message) {
			if(owner.messages.contains(message)) {
				//Logger.log(LogMessageCategory.DEBUG,"VehicleTypeVMTImporter.removeMessage " + message);
				owner.messages.remove(message);
			} else {
				//Logger.log(LogMessageCategory.DEBUG,"VehicleTypeVMTImporter.!removeMessage " + message);
			}
		}

		/** Check for data in the database **/
		void assessSituation() {
			assessSituation(null);
		}

		/**
		 * Check for data in the database
		 * @param dbToUse database connection to use, may be null.
		**/
		void assessSituation(Connection dbToUse) {
			//Logger.log(LogMessageCategory.DEBUG,"VehicleTypeVMTImporter.assessSituation");
			hasHPMSAnnual = false;
			hasSourceTypeAnnual = false;
			hasHPMSDaily = false;
			hasSourceTypeDaily = false;
			hasMonthVMTFraction = false;
			hasDayVMTFraction = false;

			if(dbToUse == null && !importer.getImporterManager().database.hasDatabase()) {
				return;
			}
			boolean shouldCloseDatabase = false;
			Connection db = dbToUse;
			if(db == null) {
				db = importer.getImporterManager().openDatabase(false);
				if(db == null) {
					return;
				}
				importer.getImporterManager().activeDb = db;
				shouldCloseDatabase = true;
			}
			String sql = "";
			try {
				sql = "select count(*) from MonthVMTFraction";
				hasMonthVMTFraction = SQLRunner.executeScalar(db,sql) > 0;
			} catch(Exception e) {
				if(e.getMessage().indexOf("exist") > 0) {
					// tableName may not exist, it may never have been created/filled, so tolerate its absense.
				} else {
					Logger.logError(e,"Unable to check the MonthVMTFraction table");
				}
			}
			try {
				sql = "select count(*) from DayVMTFraction";
				hasDayVMTFraction = SQLRunner.executeScalar(db,sql) > 0;
			} catch(Exception e) {
				if(e.getMessage().indexOf("exist") > 0) {
					// tableName may not exist, it may never have been created/filled, so tolerate its absense.
				} else {
					Logger.logError(e,"Unable to check the DayVMTFraction table");
				}
			}
			try {
				sql = "select count(*) from HPMSVtypeYear";
				hasHPMSAnnual = SQLRunner.executeScalar(db,sql) > 0;
			} catch(Exception e) {
				if(e.getMessage().indexOf("exist") > 0) {
					// tableName may not exist, it may never have been created/filled, so tolerate its absense.
				} else {
					Logger.logError(e,"Unable to check the HPMSVtypeYear table");
				}
			}
			try {
				sql = "select count(*) from HPMSVtypeDay";
				hasHPMSDaily = SQLRunner.executeScalar(db,sql) > 0;
			} catch(Exception e) {
				if(e.getMessage().indexOf("exist") > 0) {
					// tableName may not exist, it may never have been created/filled, so tolerate its absense.
				} else {
					Logger.logError(e,"Unable to check the HPMSVtypeDay table");
				}
			}

			try {
				sql = "select count(*) from SourceTypeYearVMT";
				hasSourceTypeAnnual = SQLRunner.executeScalar(db,sql) > 0;
			} catch(Exception e) {
				if(e.getMessage().indexOf("exist") > 0) {
					// tableName may not exist, it may never have been created/filled, so tolerate its absense.
				} else {
					Logger.logError(e,"Unable to check the SourceTypeYearVMT table");
				}
			}
			try {
				sql = "select count(*) from SourceTypeDayVMT";
				hasSourceTypeDaily = SQLRunner.executeScalar(db,sql) > 0;
			} catch(Exception e) {
				if(e.getMessage().indexOf("exist") > 0) {
					// tableName may not exist, it may never have been created/filled, so tolerate its absense.
				} else {
					Logger.logError(e,"Unable to check the SourceTypeDayVMT table");
				}
			}
			if(shouldCloseDatabase) {
				importer.getImporterManager().activeDb = null;
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
		}	

		/** Create and arrange the controls for this panel **/
		void createControls() {
			hpmsVsSourceTypePanel = new JPanel();
			hpmsVsSourceTypePanel.setName("hpmsVsSourceTypePanel");
			hpmsVsSourceTypePanel.setBorder(BorderFactory.createTitledBorder("Input VMT by:"));

			hpmsVsSourceButtons = new ButtonGroup();

			hpms = new JRadioButton("HPMS");
			hpms.setName("hpms");
			hpms.addActionListener(this);
			ToolTipHelper.add(hpms,"Use HPMS Vehicle Types");
			hpmsVsSourceButtons.add(hpms);

			sourceType = new JRadioButton("Source Type");
			sourceType.setName("sourceType");
			sourceType.addActionListener(this);
			ToolTipHelper.add(sourceType,"Use MOVES Source Types");
			hpmsVsSourceButtons.add(sourceType);

			annualVsDayPanel = new JPanel();
			annualVsDayPanel.setName("annualVsDayPanel");
			annualVsDayPanel.setBorder(BorderFactory.createTitledBorder("VMT values are:"));

			annualVsDayButtons = new ButtonGroup();

			annual = new JRadioButton("Annual");
			annual.setName("annual");
			annual.addActionListener(this);
			ToolTipHelper.add(annual,"Provide Annual VMT");
			annualVsDayButtons.add(annual);

			daily = new JRadioButton("Daily");
			daily.setName("daily");
			daily.addActionListener(this);
			ToolTipHelper.add(daily,"Provide Daily VMT");
			annualVsDayButtons.add(daily);

			clearAll = new JButton("Clear All");
			clearAll.setName("clearall");
			clearAll.addActionListener(this);
			ToolTipHelper.add(clearAll,"Clear imported annual/daily VMT data");

			hpmsVsSourceTypePanel.setLayout(new BoxLayout(hpmsVsSourceTypePanel,BoxLayout.X_AXIS));
			hpmsVsSourceTypePanel.add(hpms);
			hpmsVsSourceTypePanel.add(sourceType);

			annualVsDayPanel.setLayout(new BoxLayout(annualVsDayPanel,BoxLayout.X_AXIS));
			annualVsDayPanel.add(annual);
			annualVsDayPanel.add(daily);

			setLayout(new BoxLayout(this,BoxLayout.X_AXIS));
			add(hpmsVsSourceTypePanel);
			add(Box.createRigidArea(new Dimension(5,0)));
			add(annualVsDayPanel);
			add(Box.createRigidArea(new Dimension(30,0)));
			add(clearAll);
		}
	
		/**
		 * Calls the appropriate button handler.
		 * @param e the ActionEvent to be handled.
		**/
		public void actionPerformed(ActionEvent e) {
 			if(isChangingControls <= 0) {
 				if(e.getSource() == clearAll) {
					clearTable("SourceTypeYearVMT");
					clearTable("HPMSVtypeYear");
					clearTable("SourceTypeDayVMT");
					clearTable("HPMSVtypeDay");
					clearTable("MonthVMTFraction");
					clearTable("DayVMTFraction");
					clearTable("HourVMTFraction");

					// Add diagnostic message
					owner.messages.clear();
					populateControls();
					addMessage("Cleared all imported data from VMT-related tables.");
					owner.importerPanel.showMessages();
					owner.importerPanel.refreshIcons();
 				} else if(e.getSource() == hpms) {
					assessSituation();
					/*
					clearTable("SourceTypeYearVMT");
					clearTable("SourceTypeDayVMT");
					clearTable(annual.isSelected()? "HPMSVtypeDay" : "HPMSVtypeYear");
					*/
					showPanels();
				} else if(e.getSource() == sourceType) {
					assessSituation();
					/*
					clearTable("HPMSVtypeYear");
					clearTable("HPMSVtypeDay");
					clearTable(annual.isSelected()? "SourceTypeDayVMT" : "SourceTypeYearVMT");
					*/
					showPanels();
				} else if(e.getSource() == annual) {
					assessSituation();
					/*
					clearTable("SourceTypeDayVMT");
					clearTable("HPMSVtypeDay");
					clearTable(hpms.isSelected()? "SourceTypeYearVMT" : "HPMSVtypeYear");
					*/
					showPanels();
				} else if(e.getSource() == daily) {
					assessSituation();
					/*
					clearTable("SourceTypeYearVMT");
					clearTable("HPMSVtypeYear");
					clearTable(hpms.isSelected()? "SourceTypeDayVMT" : "HPMSVtypeDay");
					clearTable("MonthVMTFraction");
					clearTable("DayVMTFraction");
					*/
					showPanels();
				}
 			}
			/*
			if(tableName == null) {
				// Ask the user to select a table first
				JOptionPane.showMessageDialog(null,
						"Please select a table first.",
						"Table Needed", JOptionPane.ERROR_MESSAGE);
				return;
			} else {
				handleCreateTemplateButton();
			}
			*/
		}

		/** Display the subset of panels needed according to the current radio buttons **/
		void showPanels() {
			owner.SourceTypeDayVMTPart.setVisible(daily.isSelected() && sourceType.isSelected());
			owner.SourceTypeYearVMTPart.setVisible(annual.isSelected() && sourceType.isSelected());
			owner.HPMSVtypeDayPart.setVisible(daily.isSelected() && hpms.isSelected());
			owner.HPMSVtypeYearPart.setVisible(annual.isSelected() && hpms.isSelected());
			owner.monthPart.setVisible(annual.isSelected());
			owner.dayPart.setVisible(annual.isSelected());
			owner.hourPart.setVisible(true);
		}

		/**
		 * Called when an import is beginning.
		**/
		public void onImportBegin() {
			//Logger.log(LogMessageCategory.DEBUG,"VehicleTypeVMTImporter.onImportBegin");
			assessSituation();
			showPanels();

			if(hpms.isSelected()) {
				clearTable("SourceTypeYearVMT");
				clearTable("SourceTypeDayVMT");
				clearTable(annual.isSelected()? "HPMSVtypeDay" : "HPMSVtypeYear");
			} else if(sourceType.isSelected()) {
				assessSituation();
				clearTable("HPMSVtypeYear");
				clearTable("HPMSVtypeDay");
				clearTable(annual.isSelected()? "SourceTypeDayVMT" : "SourceTypeYearVMT");
			}
			if(daily.isSelected()) {
				clearTable("MonthVMTFraction");
				clearTable("DayVMTFraction");
			}
		}

		/**
		 * Remove all data from a table.
		 * @param tableName table to be truncated
		**/
		void clearTable(String tableName) {
			if(!importer.getImporterManager().database.hasDatabase()) {
				return;
			}
			Connection db = importer.getImporterManager().openDatabase();
			if(db == null) {
				return;
			}
			importer.getImporterManager().activeDb = db;
			String sql = "";
			try {
				sql = "delete from " + tableName;
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				if(e.getMessage().indexOf("exist") > 0) {
					// tableName may not exist, it may never have been created/filled, so tolerate its absense.
				} else {
					Logger.logError(e,"Unable to clear the " + tableName + " table");
				}
			}
			importer.getImporterManager().activeDb = null;
			DatabaseUtilities.closeConnection(db);
			db = null;
		}
	
		/**
		 * Generate XML describing this part.
		 * @return XML
		**/
		public String toXML() {
			return "";
		}
	
		/**
		 * Setup the part from XML previously created by toXML().
		 * @param node XML node to be read
		 * @return true if the node was processed successfully, false if the node was
		 * not processed or not recognized.
		**/
		public boolean fromXML(Node node) {
			return false;
		}
	
		/**
		 * Refresh any state information from data stored in the audit log for an input database.
		 * @param db Database to use
		**/
		public void refreshFromAuditLog(Connection db) {
			//Logger.log(LogMessageCategory.DEBUG,"VehicleTypeVMTImporter.refreshFromAuditLog");
			assessSituation(db);
			complain();
		}

		/**
		 * Complain if conflicting data is present
		 * @return false if conflicting data is present, true if no conflicts were found.
		**/
		boolean complain() {
			String errorText = "ERROR: More than one VMT table has been populated. Clear imported data and start over.";
			if(hasHPMSAnnual || hasHPMSDaily || hasSourceTypeAnnual || hasSourceTypeDaily) {
				int howMany = 0;
				howMany += hasHPMSAnnual? 1 : 0;
				howMany += hasHPMSDaily? 1 : 0;
				howMany += hasSourceTypeAnnual? 1 : 0;
				howMany += hasSourceTypeDaily? 1 : 0;
				if(howMany > 1) {
					addMessage(errorText);
					return false;
				} else {
					removeMessage(errorText);
				}
			}
			return true;
		}
	}

	/** Data handler for this importer **/
	BasicDataHandler basicDataHandler;
	/** Part object for the HPMSVTypeYear table **/
	TableFileLinkagePart HPMSVtypeYearPart;
	TableFileLinkagePart HPMSVtypeDayPart;
	TableFileLinkagePart SourceTypeYearVMTPart;
	TableFileLinkagePart SourceTypeDayVMTPart;

	/** Part object for the monthVMTFraction table **/
	TableFileLinkagePart monthPart;
	/** Part object for the dayVMTFraction table **/
	TableFileLinkagePart dayPart;
	/** Part object for the hourVMTFraction table **/
	TableFileLinkagePart hourPart;

	/** Part object controlling the combination of tables offered to the user **/
	TableSubsetSelectorPart selectorPart;

	/**
	 * Descriptor of the table(s) imported, exported, and cleared by this importer.
	 * The format is compatible with BasicDataHandler.
	**/
	static String[] dataTableDescriptor = {
		BasicDataHandler.BEGIN_TABLE, "HPMSVTypeYear",
		"HPMSVtypeID", "HPMSVType", ImporterManager.FILTER_HPMS_VTYPE,
		"yearID", "Year", ImporterManager.FILTER_YEAR,
		//"VMTGrowthFactor", "", ImporterManager.FILTER_NON_NEGATIVE,  Since this is forced to 0
		//                                                             do not export/import it.
		"HPMSBaseYearVMT", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "HPMSVTypeDay",
		"HPMSVtypeID", "HPMSVType", ImporterManager.FILTER_HPMS_VTYPE,
		"yearID", "Year", ImporterManager.FILTER_YEAR,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"dayID", "DayOfAnyWeek", ImporterManager.FILTER_DAY,
		"VMT", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "SourceTypeYearVMT",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"yearID", "Year", ImporterManager.FILTER_YEAR,
		"VMT", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "SourceTypeDayVMT",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"yearID", "Year", ImporterManager.FILTER_YEAR,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"dayID", "DayOfAnyWeek", ImporterManager.FILTER_DAY,
		"VMT", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "MonthVMTFraction",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"monthVMTFraction", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "DayVMTFraction",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"monthID", "MonthOfAnyYear", ImporterManager.FILTER_MONTH,
		"roadTypeID", "RoadType", ImporterManager.FILTER_ROAD_TYPE,
		"dayID", "DayOfAnyWeek", ImporterManager.FILTER_DAY,
		"dayVMTFraction", "", ImporterManager.FILTER_NON_NEGATIVE,

		BasicDataHandler.BEGIN_TABLE, "HourVMTFraction",
		"sourceTypeID", "SourceUseType", ImporterManager.FILTER_SOURCE,
		"roadTypeID", "RoadType", ImporterManager.FILTER_ROAD_TYPE,
		"dayID", "DayOfAnyWeek", ImporterManager.FILTER_DAY,
		"hourID", "HourOfAnyDay", ImporterManager.FILTER_HOUR,
		"hourVMTFraction", "", ImporterManager.FILTER_NON_NEGATIVE
	};

	/** Class for editing the data source **/
	class HPMSVtypeYearPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "HPMSVtypeYear";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class HPMSVtypeDayPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "HPMSVtypeDay";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class SourceTypeYearVMTPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "SourceTypeYearVMT";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class SourceTypeDayVMTPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "SourceTypeDayVMT";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class MonthPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "monthVMTFraction";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class DayPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "dayVMTFraction";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for editing the data source **/
	class HourPartProvider implements TableFileLinkagePart.IProvider {
		/**
		 * Get the name of the table being managed
		 * @return the name of the table being managed
		**/
		public String getTableName() {
			return "hourVMTFraction";
		}

		/**
		 * Create a template file (or files).
		 * @param destinationFile file selected by the user to be created.  The file may already
		 * exist.
		 * @return true if the template was created successfully, false otherwise.
		**/
		public boolean createTemplate(File destinationFile) {
			return dataHandler.createTemplate(getTableName(),destinationFile);
		}
	}

	/** Class for interfacing to BasicDataHandler's needs during an import **/
	class BasicDataHandlerProvider implements BasicDataHandler.IProvider, BasicDataHandler.IProvider2 {
		/**
		 * Called to notify that an Import operation is starting.
		**/
		public void onImportBegin() {
			selectorPart.onImportBegin();
		}

		/**
		 * Obtain the name of the file holding data for a table.
		 * @param tableName table in question
		 * @return the name of the file holding data for a table, null or blank if
		 * no file has been specified.
		**/
		public String getTableFileSource(String tableName) {
			if(tableName.equalsIgnoreCase("HPMSVtypeYear")) {
				if(!HPMSVtypeYearPart.isVisible()) {
					return "\rSKIP\r";
				}
				return HPMSVtypeYearPart.fileName;
			} else if(tableName.equalsIgnoreCase("HPMSVtypeDay")) {
				if(!HPMSVtypeDayPart.isVisible()) {
					return "\rSKIP\r";
				}
				return HPMSVtypeDayPart.fileName;
			} else if(tableName.equalsIgnoreCase("SourceTypeYearVMT")) {
				if(!SourceTypeYearVMTPart.isVisible()) {
					return "\rSKIP\r";
				}
				return SourceTypeYearVMTPart.fileName;
			} else if(tableName.equalsIgnoreCase("SourceTypeDayVMT")) {
				if(!SourceTypeDayVMTPart.isVisible()) {
					return "\rSKIP\r";
				}
				return SourceTypeDayVMTPart.fileName;
			} else if(tableName.equalsIgnoreCase("monthVMTFraction")) {
				if(!monthPart.isVisible()) {
					return "\rSKIP\r";
				}
				return monthPart.fileName;
			} else if(tableName.equalsIgnoreCase("dayVMTFraction")) {
				if(!dayPart.isVisible()) {
					return "\rSKIP\r";
				}
				return dayPart.fileName;
			} else if(tableName.equalsIgnoreCase("hourVMTFraction")) {
				if(!hourPart.isVisible()) {
					return "\rSKIP\r";
				}
				return hourPart.fileName;
			}
			return null;
		}

		/**
		 * Obtain the name of the worksheet within an XLS file holding data for a table.
		 * @param tableName table in question
		 * @return the name of the worksheet within an XLS file, null or blank if no
		 * worksheet has been specified or if the file is not an XLS file.
		**/
		public String getTableWorksheetSource(String tableName) {
			if(tableName.equalsIgnoreCase("HPMSVtypeYear")) {
				return HPMSVtypeYearPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("HPMSVtypeDay")) {
				return HPMSVtypeDayPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("SourceTypeYearVMT")) {
				return SourceTypeYearVMTPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("SourceTypeDayVMT")) {
				return SourceTypeDayVMTPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("monthVMTFraction")) {
				return monthPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("dayVMTFraction")) {
				return dayPart.worksheetName;
			} else if(tableName.equalsIgnoreCase("hourVMTFraction")) {
				return hourPart.worksheetName;
			}
			return null;
		}

		/**
		 * Allow custom processing and SQL for exporting data.
		 * @param type which type of MOVES database holds the exported data.  Typically, this
		 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
		 * being used.
		 * @param db database holding the data to be exported
		 * @param tableName table being exported
		 * @return SQL to be used or null if there is no alternate SQL.
		**/
		public String getAlternateExportSQL(MOVESDatabaseType type, Connection db,
				String tableName) {
			if(!tableName.equalsIgnoreCase("HPMSVTypeYear")) {
				// Only HPMSVTypeYear needs custom export logic
				return null;
			}
			if(type != MOVESDatabaseType.EXECUTION) {
				// Only the execution database has all the supporting tables filled.

				// County-domains should provide no output records. Doing so prevents
				// a user from obtaining national default VMT and confusing it with
				// a single county's VMT.
				if(type == MOVESDatabaseType.DEFAULT
						&& manager.isCounty() && tableName.equalsIgnoreCase("HPMSVtypeYear")) {
					return "select HPMSVtypeID, yearID, HPMSBaseYearVMT"
							+ " from HPMSVTypeYear"
							+ " where yearID = 0";
				}
				// Other domains and databases should use the default export logic.
				return null;
			}
			String sql = "";
			try {
				TreeSet years = manager.getFilterValuesSet(ImporterManager.FILTER_YEAR);
				if(years == null || years.size() <= 0) {
					return null;
				}
				String countyCSV = manager.getFilterValuesCSV(ImporterManager.FILTER_COUNTY);
				if(countyCSV == null || countyCSV.length() <= 0) {
					return null;
				}

				Integer firstYear = (Integer)years.first();
				Integer lastYear = (Integer)years.last();
				TotalActivityGenerator.setupAnalysisYearVMTTables(db);
				TotalActivityGenerator.growVMT(db,firstYear.intValue(),lastYear.intValue());
				String yearsCSV = manager.getFilterValuesCSV(ImporterManager.FILTER_YEAR);

				String[] statements = {
					"drop table if exists fractionWithinHPMSVTypeSummary",

					"create table fractionWithinHPMSVTypeSummary"
							+ " select yearID, sourceTypeID, sum(fraction) as vmtFraction"
							+ " from fractionWithinHPMSVType"
							+ " where yearID in (" + yearsCSV + ")"
							+ " group by yearID, sourceTypeID"
							+ " order by null",

					"drop table if exists vmtBySourceTypeTemp",

					"create table vmtBySourceTypeTemp"
							+ " select ayvmt.yearID, zrt.zoneID, sut.sourceTypeID,"
							+ " 	sum(ayvmt.VMT*rtd.roadTypeVMTFraction*zrt.shoAllocFactor*f.vmtFraction) as sutVMT"
							+ " from analysisYearVMT ayvmt"
							+ " inner join sourceUseType sut on (sut.hpmsVTypeID=ayvmt.hpmsVTypeID)"
							+ " inner join fractionWithinHPMSVTypeSummary f on (f.yearID=ayvmt.yearID and f.sourceTypeID=sut.sourceTypeID)"
							+ " inner join roadTypeDistribution rtd on (rtd.sourceTypeID=f.sourceTypeID)"
							+ " inner join zoneRoadType zrt on (zrt.roadTypeID=rtd.roadTypeID)"
							+ " inner join zone z on (z.zoneID=zrt.zoneID)"
							+ " where z.countyID in (" + countyCSV + ")"
							+ " group by ayvmt.yearID, zrt.zoneID, sut.sourceTypeID"
							+ " order by null"
				};
				for(int i=0;i<statements.length;i++) {
					sql = statements[i];
					SQLRunner.executeSQL(db,sql);
				}

				return "select HPMSVtypeID, yearID, sum(sutVMT) as HPMSBaseYearVMT "
						+ " from vmtBySourceTypeTemp vst"
						+ " inner join sourceUseType sut using (sourceTypeID)"
						+ " where yearID in (" + yearsCSV + ")"
						+ " group by HPMSVtypeID, yearID"
						+ " order by HPMSVtypeID, yearID";
			} catch(Exception e) {
				Logger.logError(e,"Unable to grow VMT");
			}
			return null;
		}

		/**
		 * Cleanup custom processing and SQL for exporting data.
		 * @param type which type of MOVES database holds the exported data.  Typically, this
		 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
		 * being used.
		 * @param db database holding the data to be exported
		 * @param tableName table being exported
		**/
		public void cleanupAlternateExportSQL(MOVESDatabaseType type, Connection db,
				String tableName) {
			if(!tableName.equalsIgnoreCase("HPMSVTypeYear")) {
				// Only HPMSVTypeYear needs custom export logic
				return;
			}
			try {
				if(type != MOVESDatabaseType.EXECUTION) {
					TotalActivityGenerator.removeAnalysisYearVMTTables(db);
				}
			} catch(Exception e) {
				Logger.logError(e,"Unable to cleanup VMT after export");
			}
		}
	}

	/** Constructor **/
	public VehicleTypeVMTImporter() {
		super("Vehicle Type VMT", // common name
				"vehicletypevmt", // XML node name
				new String[] { "HPMSVTypeYear", "HPMSVtypeDay", "SourceTypeYearVMT", "SourceTypeDayVMT", "MonthVMTFraction", "DayVMTFraction", "HourVMTFraction" } // required tables
				);
		shouldDoExecutionDataExport = false;
		shouldDoDefaultDataExport = true;
		shouldDoCustomDefaultDataExport = true;
		subjectToExportRestrictions = false;

		HPMSVtypeYearPart = new TableFileLinkagePart(this,new HPMSVtypeYearPartProvider());
		HPMSVtypeDayPart = new TableFileLinkagePart(this,new HPMSVtypeDayPartProvider());
		SourceTypeYearVMTPart = new TableFileLinkagePart(this,new SourceTypeYearVMTPartProvider());
		SourceTypeDayVMTPart = new TableFileLinkagePart(this,new SourceTypeDayVMTPartProvider());

		monthPart = new TableFileLinkagePart(this,new MonthPartProvider());
		dayPart = new TableFileLinkagePart(this,new DayPartProvider());
		hourPart = new TableFileLinkagePart(this,new HourPartProvider());

		selectorPart = new TableSubsetSelectorPart(this,this);

		parts.add(selectorPart);
		parts.add(HPMSVtypeYearPart);
		parts.add(HPMSVtypeDayPart);
		parts.add(SourceTypeYearVMTPart);
		parts.add(SourceTypeDayVMTPart);
		parts.add(monthPart);
		parts.add(dayPart);
		parts.add(hourPart);

		basicDataHandler = new BasicDataHandler(this,dataTableDescriptor,new BasicDataHandlerProvider());
		dataHandler = basicDataHandler;
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.  Will be null if merely checking
	 * for whether to show the importer to the user.
	 * @return the status, or null if the importer should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getCountyDataStatus(Connection db)
			throws Exception {
		selectorPart.populateControls(db);
		if(db == null) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		}
		if(checkCountyDataStatus(db,true) && getDataStatusFromScript(db)) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		} else {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.  Will be null if merely checking
	 * for whether to show the importer to the user.
	 * @param requireUserInput true if the user must supply VMT
	 * @return true if all tables have data that matches the RunSpec.
	 * @throws Exception if anything goes wrong
	**/
	private boolean checkCountyDataStatus(Connection db, boolean requireUserInput) throws Exception {
		// selectorPart.populateControls(db) should have already been called at this point.
		
		if(!selectorPart.complain()) {
			return false;
		}

		if(selectorPart.hasHPMSAnnual) {
			// HPMSVTypeYear: hpmsVTypeID, yearID
			if(!manager.tableHasHPMSVTypes(db,"select distinct hpmsVTypeID from HPMSVTypeYear",
					this,"HPMSVTypeYear is missing hpmsVTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasYears(db,"select distinct yearID from HPMSVTypeYear",
					this,"HPMSVTypeYear is missing yearID(s)")) {
				return false;
			}
		} else if(selectorPart.hasHPMSDaily) {
			if(!manager.tableHasHPMSVTypes(db,"select distinct hpmsVTypeID from HPMSVTypeDay",
					this,"HPMSVTypeDay is missing hpmsVTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasYears(db,"select distinct yearID from HPMSVTypeDay",
					this,"HPMSVTypeDay is missing yearID(s)")) {
				return false;
			}
			if(!manager.tableHasMonths(db,"select distinct monthID from HPMSVTypeDay",
					this,"HPMSVTypeDay is missing monthID(s)")) {
				return false;
			}
			if(!manager.tableHasDays(db,"select distinct dayID from HPMSVTypeDay",
					this,"HPMSVTypeDay is missing dayID(s)")) {
				return false;
			}
		} else if(selectorPart.hasSourceTypeAnnual) {
			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from SourceTypeYearVMT",
					this,"SourceTypeYearVMT is missing sourceTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasYears(db,"select distinct yearID from SourceTypeYearVMT",
					this,"sourceTypeYearVMT is missing yearID(s)")) {
				return false;
			}
		} else if(selectorPart.hasSourceTypeDaily) {
			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from SourceTypeDayVMT",
					this,"SourceTypeDayVMT is missing sourceTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasYears(db,"select distinct yearID from SourceTypeDayVMT",
					this,"sourceTypeDayVMT is missing yearID(s)")) {
				return false;
			}
			if(!manager.tableHasMonths(db,"select distinct monthID from SourceTypeDayVMT",
					this,"SourceTypeDayVMT is missing monthID(s)")) {
				return false;
			}
			if(!manager.tableHasDays(db,"select distinct dayID from SourceTypeDayVMT",
					this,"SourceTypeDayVMT is missing dayID(s)")) {
				return false;
			}
		} else if(requireUserInput) {
			addQualityMessage("ERROR: VMT data has not been imported.");
			return false;
		}

		if(selectorPart.hasHPMSAnnual || selectorPart.hasSourceTypeAnnual) {
			// monthVMTFraction: sourceTypeID, monthID
			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from monthVMTFraction where monthVMTFraction > 0",
					this,"monthVMTFraction is missing sourceTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasMonths(db,"select distinct monthID from monthVMTFraction where monthVMTFraction > 0",
					this,"monthVMTFraction is missing monthID(s)")) {
				return false;
			}

			// dayVMTFraction: sourceTypeID, monthID, roadTypeID, dayID
			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from dayVMTFraction where dayVMTFraction > 0",
					this,"dayVMTFraction is missing sourceTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasMonths(db,"select distinct monthID from dayVMTFraction where dayVMTFraction > 0",
					this,"dayVMTFraction is missing monthID(s)")) {
				return false;
			}
			if(!manager.tableHasRoadTypes(db,"select distinct roadTypeID from dayVMTFraction where dayVMTFraction > 0",
					this,"dayVMTFraction is missing roadTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasDays(db,"select distinct dayID from dayVMTFraction where dayVMTFraction > 0",
					this,"dayVMTFraction is missing dayID(s)")) {
				return false;
			}
		}

		if(requireUserInput) {
			// hourVMTFraction: sourceTypeID, roadTypeID, dayID, hourID
			if(!manager.tableHasSourceTypes(db,"select distinct sourceTypeID from hourVMTFraction where hourVMTFraction > 0",
					this,"hourVMTFraction is missing sourceTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasRoadTypes(db,"select distinct roadTypeID from hourVMTFraction where hourVMTFraction > 0",
					this,"hourVMTFraction is missing roadTypeID(s)")) {
				return false;
			}
			if(!manager.tableHasDays(db,"select distinct dayID from hourVMTFraction where hourVMTFraction > 0",
					this,"hourVMTFraction is missing dayID(s)")) {
				return false;
			}
			if(!manager.tableHasHours(db,"select distinct hourID from hourVMTFraction where hourVMTFraction > 0",
					this,"hourVMTFraction is missing hourID(s)")) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.  Will be null if merely checking
	 * for whether to show the importer to the user.
	 * @return the status, or null if the importer should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getProjectDataStatus(Connection db)
			throws Exception {
		selectorPart.populateControls(db);
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatus(Connection db) throws Exception {
		selectorPart.populateControls(db);
		// By default, return null so that no status is shown
		return null;
		/*
		if(checkCountyDataStatus(db,false)) { // && getDataStatusFromScript(db)) {
			return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		} else {
			return new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}
		*/
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	private boolean getDataStatusFromScript(Connection db) throws Exception {
		ArrayList<String> messages = new ArrayList<String>();
		BasicDataHandler.runScript(db,this,messages,1,"database/VehicleTypeVMTImporter.sql");
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String t = i.next();
			if(t.toUpperCase().startsWith("ERROR")) {
				return false;
			}
		}
		return true;
	}
}
