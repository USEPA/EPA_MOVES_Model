/**************************************************************************************************
 * @(#)ImporterBase.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.util.*;
import javax.swing.*;
import java.awt.*;
import java.sql.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;

/**
 * Optional base class with common utility functions for importers.  Since importers technically
 * only need to implement IImporter, they are free to not use this class.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @author		Mike Kender	task 1903
 * @version		2019-10-18
**/
public class ImporterBase implements IImporter, ICountyDataImporter, IProjectDataImporter,
		IDataStatus {
	/** Manager of this importer **/
	public ImporterManager manager;
	/** Common name of this importer **/
	public String commonName = "";
	/** Type of the XML node this importer creates and reads **/
	public String xmlNodeType = "";
	/** Tables required for the importer to operate **/
	public String[] requiredTables = null;

	/** Description of the imported data **/
	public String description = "";
	/** List of String objects holding messages about the imported data **/
	public ArrayList<String> messages = new ArrayList<String>();
	/** List of String objects holding messages about the quality of the imported data **/
	public TreeSet<String> qualityMessages = new TreeSet<String>();
	/**
	 * List of IImporterPart objects for editing specific portions of the importer.
	 * Only parts that are allowed by the current user settings are listed.
	**/
	public ArrayList<IImporterPart> parts = new ArrayList<IImporterPart>();
	/**
	 * List of IImporterPart objects for editing specific portions of the importer.
	 * All items in this list are possible, though only some will be in the parts list.
	**/
	public ArrayList<IImporterPart> allParts = new ArrayList<IImporterPart>();

	/** IImporterTabBaseProvider for this importer **/
	public ImporterTabBaseProvider tabBaseProvider;
	/** GUI panel for this importer **/
	public ImporterTabBase importerPanel;
	/** Data handler for this importer **/
	public IDataHandler dataHandler;

	/** True if data can be exported from the default database **/
	public boolean shouldDoDefaultDataExport = true;

	/** True if data can be exported from MOVESExecution **/
	public boolean shouldDoExecutionDataExport = false;

	/** True if data is subject to export restrictions **/
	public boolean subjectToExportRestrictions = false;

	/** True if data can be exported from the default database **/
	public boolean shouldDoCustomDefaultDataExport = false;

	/** Name, if any, of a custom button to be shown to the user **/
	public String customButtonName = null;

	class ImporterTabBaseProvider implements IImporterTabBaseProvider {
		/**
		 * Get the description of the importer
		 * @return the description of the importer
		**/
		public String getDescription() {
			return description;
		}

		/**
		 * Set the description of the importer
		 * @param descriptionToUse the description of the importer
		**/
		public void setDescription(String descriptionToUse) {
			description = descriptionToUse;
		}

		/**
		 * Get the list of messages about the imported data
		 * @return an ArrayList holding String objects
		**/
		public ArrayList<String> getMessages() {
			return messages;
		}

		/** Clear the list of messages about the imported data. **/
		public void clearMessages() {
			messages.clear();
			qualityMessages.clear();
		}

		/** Display current messages **/
		public void showMessages() {
			if(importerPanel != null) {
				importerPanel.showMessages();
			}
		}

		/**
		 * Get the set of IImporterPart objects that allow editing importer-specific details.
		 * @return an ArrayList holding IImporterPart objects.  null or empty are nonsensical
		 * but allowed.
		**/
		public ArrayList<IImporterPart> getParts() {
			setupAllParts();
			return parts;
		}

		/**
		 * Determine if exporting data from the default database is allowed.
		 * @return true if data from the default database can be exported
		**/
		public boolean allowDefaultDataExport() {
			return shouldDoDefaultDataExport;
		}

		/**
		 * Determine if exporting data from MOVESExecution is allowed.
		 * @return true if data from MOVESExecution can be exported
		**/
		public boolean allowExecutionDataExport() {
			return shouldDoExecutionDataExport;
		}

		/**
		 * Determine if data export restrictions may be applied.
		 * @return true if data from MOVESExecution or MOVES default should be regulated
		 * according to the CompilationFlags.ENABLE_EXPORT_DEFAULT_DATA
		**/
		public boolean isSubjectToExportRestrictions() {
			return subjectToExportRestrictions;
		}

		/**
		 * Determine if exporting custom data from the default database is allowed.
		 * @return true if data from the default database can be exported
		**/
		public boolean allowCustomDomainDefaultDataExport() {
			return shouldDoCustomDefaultDataExport;
		}

		/**
		 * Obtain the name, if any, of a custom button on the GUI.
		 * @return non-null if a custom button is to be offered.
		**/
		public String getCustomButtonName() {
			return customButtonName;
		}
	
		/**
		 * Process a click on a custom button.
		 * @param name identifies the custom button.
		 * @param guiOwner window that is showing the custom button
		**/	
		public void onCustomButton(String name, JPanel guiOwner) {
			handleCustomButton(name, guiOwner);
		}
	}

	/**
	 * Constructor
	 * @param commonNameToUse Common name of this importer
	 * @param xmlNodeTypeToUse Type of the XML node this importer creates and reads
	 * @param requiredTablesToUse list of required tables the importer needs to execute
	**/
	public ImporterBase(String commonNameToUse, String xmlNodeTypeToUse,
			String[] requiredTablesToUse) {
		commonName = commonNameToUse;
		xmlNodeType = xmlNodeTypeToUse;
		requiredTables = requiredTablesToUse;
	}

	/**
	 * Get the ImporterManager object that is hosting the importer.
	 * @return the ImporterManager object that is hosting the importer
	**/
	public ImporterManager getImporterManager() {
		return manager;
	}

	/**
	 * Designate the ImporterManager object that his hosting the importer.
	 * @param managerToUse the ImporterManager object that his hosting the importer
	**/
	public void setImporterManager(ImporterManager managerToUse) {
		manager = managerToUse;
	}

	/**
	 * Get the name of the importer for log purposes.
	 * @return the name of the importer for log purposes
	**/
	public String getName() {
		return commonName;
	}

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	public String getScriptName() {
		return commonName;
	}

	/**
	 * Get the description of the imported data for log purposes.
	 * @return the description of the imported data for log purposes
	**/
	public String getDescription() {
		return description;
	}

	/**
	 * Obtain the GUI panel to be hosted for this importer.
	 * @return the GUI panel to be hosted for this importer
	**/
	public IImporterPanel getPanel() {
		if(tabBaseProvider == null) {
			tabBaseProvider = new ImporterTabBaseProvider();
		}
		if(importerPanel == null) {
			importerPanel = new ImporterTabBase(this,commonName,tabBaseProvider);
		}
		return importerPanel;
	}

	/** Fixup allParts in case it is empty **/
	public void setupAllParts() {
		if(allParts.size() == 0) {
			for(IImporterPart p : parts)
			{
				allParts.add(p);
			}
		}
	}

	/**
	 * Generate XML describing this importer.  The XML tag must be understandable
	 * by ImporterInstantiator.
	 * @return XML
	**/
	public String toXML() {
		setupAllParts();

		String xml = "\t\t<" + xmlNodeType + ">\r\n";
		xml += "\t\t\t<description><![CDATA[" + description + "]]></description>\r\n";

		if(parts.size() > 0) {
			xml += "\t\t\t<parts>\r\n";
			for(Iterator i=parts.iterator();i.hasNext();) {
				IImporterPart p = (IImporterPart)i.next();
				String t = p.toXML();
				if(t != null && t.length() > 0) {
					xml += t;
				}
			}
			xml += "\t\t\t</parts>\r\n";
		}

		xml += "\t\t</" + xmlNodeType + ">\r\n";
		return xml;
	}

	/**
	 * Setup the importer from XML previously created by toXML().
	 * @param node XML node to be read
	 * @return true if the node was processed successfully
	**/
	public boolean fromXML(Node node) {
		setupAllParts();

		description = "";

		String name = node.getNodeName();
		if(!name.equalsIgnoreCase(xmlNodeType)) {
			return false;
		}
		for(Node i=node.getFirstChild(); i != null; i = i.getNextSibling()) {
			if(i.getNodeName().equalsIgnoreCase("description")) {
				Node childNode = i.getFirstChild();
				for(Node child = i.getFirstChild(); child != null;
						child = child.getNextSibling()) {
					if(child.getNodeType() == Node.TEXT_NODE
								|| child.getNodeType() == Node.CDATA_SECTION_NODE) {
						description = StringUtilities.safeGetString(childNode.getNodeValue());
						break;
					}
				}
				continue;
			}
			if(i.getNodeName().equalsIgnoreCase("parts")) {
				for(Node child = i.getFirstChild(); child != null;
						child = child.getNextSibling()) {
					for(Iterator pi=allParts.iterator();pi.hasNext();) {
						IImporterPart p = (IImporterPart)pi.next();
						if(p.fromXML(child)) {
							break;
						}
					}
				}
				continue;
			}
		}
		return true;
	}

	/**
	 * Obtain an object for moving data to/from the database for this importer.
	 * @return an IDataHandler object tied to this importer.
	**/
	public IDataHandler getDataHandler() {
		return dataHandler;
	}

	/**
	 * Get the set of tables that the importer requires in order to execute.
	 * @return an array of table names.  Though it can be empty or null, such
	 * a result is nonsensical.
	**/
	public String[] getRequiredTables() {
		return requiredTables;
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
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
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
		return new RunSpecSectionStatus(RunSpecSectionStatus.OK);
	}

	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	public RunSpecSectionStatus getImporterDataStatus(Connection db) throws Exception {
		// By default, return null so that no status is shown
		return null;
	}

	/**
	 * Refresh any state information from data stored in the audit log for an input database.
	 * @param db Database to use
	**/
	public void refreshFromAuditLog(Connection db) {
		//Logger.log(LogMessageCategory.DEBUG,"ImporterBase.refreshFromAuditLog");
		setupAllParts();
		for(Iterator<IImporterPart> i=parts.iterator();i.hasNext();) {
			i.next().refreshFromAuditLog(db);
		}
        if(tabBaseProvider == null) {
			tabBaseProvider = new ImporterTabBaseProvider();
		}
		tabBaseProvider.showMessages();
		//Logger.log(LogMessageCategory.DEBUG,"ImporterBase.refreshFromAuditLog Done");
	}

	/**
	 * Process a click on a custom button.
	 * @param name identifies the custom button.
	 * @param guiOwner window that is showing the custom button
	**/	
	public void handleCustomButton(String name, JPanel guiOwner) {
		// Nothing to do in the base class
	}

	/**
	 * Add a message about the quality of data.
	 * @param t message to be shown.
	**/
	public void addQualityMessage(String t) {
		if(t == null || t.length() <= 0 || t.equalsIgnoreCase("OK") || t.equalsIgnoreCase("NOT_READY")) {
			return;
		}
		if(!t.toLowerCase().startsWith("error")) {
			t = "Missing: " + t;
		}
		if(!qualityMessages.contains(t)) {
			qualityMessages.add(t);
			messages.add(t);
		}
	}
	
	/**
	 * Add a custom message. Differs from addQualityMessage in that it doesn't prepend all non-error messages with "Missing: "
	 * @param t message to be shown.
	**/
	public void addCustomMessage(String t) {
		if(t == null || t.length() <= 0 || t.equalsIgnoreCase("OK") || t.equalsIgnoreCase("NOT_READY")) {
			return;
		}
		if(!qualityMessages.contains(t)) {
			qualityMessages.add(t);
			messages.add(t);
		}
	}

	/**
	 * Remove all messages about data quality, without disturbing other messages.
	**/
	public void removeQualityMessages() {
		for(String t : qualityMessages) {
			messages.remove(t);
		}
		qualityMessages.clear();
	}

    /**
     * Get the list of messages about the imported data
     * @return an ArrayList holding String objects
    **/
    public ArrayList<String> getMessages() {
        if(tabBaseProvider == null) {
			tabBaseProvider = new ImporterTabBaseProvider();
		}
        return tabBaseProvider.getMessages();
    }
}
