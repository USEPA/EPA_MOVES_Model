/**************************************************************************************************
 * @(#)ExecuteScript.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.installer;

import java.awt.*;
import javax.swing.*;
import gov.epa.otaq.moves.common.*;
import java.sql.*;
import java.io.*;
import java.net.*;

/**
 * Executes a sql script.  Used to execute MOVES update sql scripts.
 *
 * @author	Wes Faler
 * @version	2009-03-16
**/
public class ExecuteScript {
	/**
	 * Entry point
	 * @param args command line arguments, consisting of the full path to a script file,
	 * followed by the database name.
	**/
	public static void main(String[] args) {
		// First get the sql script
		String sqlScript = args[0];
		String dbName = args[1];

		while(dbName.endsWith("/") || dbName.endsWith("\\")) {
			dbName = dbName.substring(0,dbName.length()-1).trim();
		}
		int slashIndex = dbName.lastIndexOf('/');
		int backSlashIndex = dbName.lastIndexOf('\\');
		int index = Math.max(slashIndex,backSlashIndex);
		if(index >= 0) {
			dbName = dbName.substring(index+1);
		}

		Connection dbConnection = null;
		File sql = new File(sqlScript);

		try {
			// Create DB connection
			DatabaseSelection db = new DatabaseSelection();
			// set database parameters
			db.serverName= "localhost";
			db.databaseName = dbName;

			//JOptionPane.showMessageDialog(null,"Updating database " + dbName
			//		+ " using script: " + sql.getCanonicalPath());

			dbConnection = db.openConnection();
			// execute script
			DatabaseUtilities.executeScript(dbConnection, sql);
		} catch(Exception e) {
			/**
			 * @issue Database update error: [*]
			 * @explain An error occurred while making database changes via an installer script.
			**/
			System.err.println(e.getMessage());
			JOptionPane.showMessageDialog(null,"Database update error: " + e.getMessage());
		} catch(Error e) {
			/**
			 * @issue Database update error: [*]
			 * @explain An error occurred while making database changes via an installer script.
			**/
			System.err.println(e.getMessage());
			JOptionPane.showMessageDialog(null,"Database update error: " + e.getMessage());
		} finally {
			if(dbConnection != null) {
				DatabaseUtilities.closeConnection(dbConnection);
			}
		}
	}
}
