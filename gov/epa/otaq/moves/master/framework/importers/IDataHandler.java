/**************************************************************************************************
 * @(#)IDataHandler.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.util.*;
import java.sql.*;
import java.io.*;
import javax.swing.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.common.MOVESDatabaseType;

/**
 * Interface for moving data with importers.
 * 
 * @author		Wesley Faler
 * @version		2009-03-30
**/
public interface IDataHandler {
	/**
	 * Get a list of the tables that affected by import, export, and clear.
	 * @return an ArrayList of String objects holding table names
	**/
	ArrayList<String> getDataTableNames();

	/**
	 * Create a template file (or files).
	 * @param tableName table that a template should be created for
	 * @param destinationFile file selected by the user to be created.  The file may already
	 * exist.
	 * @return true if the template was created successfully, false otherwise.
	**/
	boolean createTemplate(String tableName, File destinationFile);

	/**
	 * Check the import source for errors and optionally store its data.
	 * Record the successful import in the database's log.
	 * @param db database to receive the imported data
	 * @param shouldCommit true if the imported data should actually be saved,
	 * false if it should only be scanned for possible issues.
	 * @param messages information, warnings, and errors to be shown to the user
	 * @return true if the data was imported successfully.
	**/
	boolean doImport(Connection db, boolean shouldCommit, ArrayList<String> messages);

	/**
	 * Export data into one or more files.  If an XLS file is specified, each exported 
	 * table will get its own worksheet.
	 * @param type which type of MOVES database holds the exported data.  Typically, this
	 * will be DEFAULT, EXECUTION, or null.  null indicates a user-supplied database is
	 * being used.
	 * @param db database holding the data to be exported
	 * @param file File to receive the exported data
	 * @return -1 if any error occured, 0 if no errors but no data, +1 for no errors and data
	**/
	int doExport(MOVESDatabaseType type, Connection db, File file);

	/**
	 * Clear data from tables.
	 * @param db database holding the data to be cleared
	 * @return true if the data was cleared
	**/
	boolean doClear(Connection db);
}
