/**************************************************************************************************
 * @(#)IImporterPart.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import javax.swing.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import java.sql.*;

/**
 * Interface for panels that edit details about importers.
 * 
 * @author		wgfaler
 * @version		2012-01-23
**/
public interface IImporterPart {
	/**
	 * Get the actual JPanel to be shown
	 * @return the JPanel that reprents one portion of the importer
	**/
	JPanel getPanel();

	/**
	 * Push data from the importer to controls returned by getPanel().
	**/
	void populateControls();

	/**
	 * Generate XML describing this part.
	 * @return XML
	**/
	String toXML();

	/**
	 * Setup the part from XML previously created by toXML().
	 * @param node XML node to be read
	 * @return true if the node was processed successfully, false if the node was
	 * not processed or not recognized.
	**/
	boolean fromXML(Node node);

	/**
	 * Refresh any state information from data stored in the audit log for an input database.
	 * @param db Database to use
	**/
	void refreshFromAuditLog(Connection db);
}
