/**************************************************************************************************
 * @(#)IImporter.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.sql.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;

/**
 * Interface for all importer modules.
 * 
 * @author		Wesley Faler
 * @version		2015-09-16
**/
public interface IImporter {
	/**
	 * Get the ImporterManager object that is hosting the importer.
	 * @return the ImporterManager object that is hosting the importer
	**/
	ImporterManager getImporterManager();

	/**
	 * Designate the ImporterManager object that his hosting the importer.
	 * @param managerToUse the ImporterManager object that his hosting the importer
	**/
	void setImporterManager(ImporterManager managerToUse);

	/**
	 * Get the name of the importer for log purposes.
	 * @return the name of the importer for log purposes
	**/
	String getName();

	/**
	 * Get the base name of the importer's supporting database script file.
	 * @return the base name of the importer's supporting database script file
	**/
	String getScriptName();

	/**
	 * Get the description of the imported data for log purposes.
	 * @return the description of the imported data for log purposes
	**/
	String getDescription();

	/**
	 * Obtain the GUI panel to be hosted for this importer.
	 * @return the GUI panel to be hosted for this importer
	**/
	IImporterPanel getPanel();

	/**
	 * Generate XML describing this importer.  The XML tag must be understandable
	 * by ImporterInstantiator.
	 * @return XML
	**/
	String toXML();

	/**
	 * Setup the importer from XML previously created by toXML().
	 * @param node XML node to be read
	 * @return true if the node was processed successfully
	**/
	boolean fromXML(Node node);

	/**
	 * Obtain an object for moving data to/from the database for this importer.
	 * @return an IDataHandler object tied to this importer.
	**/
	IDataHandler getDataHandler();

	/**
	 * Get the set of tables that the importer requires in order to execute.
	 * @return an array of table names.  Though it can be empty or null, such
	 * a result is nonsensical.
	**/
	String[] getRequiredTables();

	/**
	 * Refresh any state information from data stored in the audit log for an input database.
	 * @param db Database to use
	**/
	void refreshFromAuditLog(Connection db);

	/**
	 * Add a message about the quality of data.
	 * @param t message to be shown.
	**/
	void addQualityMessage(String t);

	/**
	 * Remove all messages about data quality, without disturbing other messages.
	**/
	void removeQualityMessages();
}
