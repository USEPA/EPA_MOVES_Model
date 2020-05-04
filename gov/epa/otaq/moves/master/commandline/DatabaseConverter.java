/**************************************************************************************************
 * @(#)DatabaseConverter.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.commandline;

import java.io.*;
import java.sql.*;
import java.util.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;

/**
 * Batch mode conversion of databases.
 *
 * @author		Wes Faler
 * @version		2011-11-07
**/
public class DatabaseConverter {
	/** Invokes the main application. **/
	public static void main(String[] args) {
		Configuration.allowGUI = false;

		if(!SystemConfiguration.getTheSystemConfiguration().didLoad) {
			System.exit(1);
		}

		// Find command line arguments
		String inputDatabaseName = "";
		String outputDatabaseName = "";
		String scriptName = "";
		for(int i=0;i<args.length;i++) {
			String t = args[i].toLowerCase();
			if(t.startsWith("-input")) {
				inputDatabaseName = getOptionData(args[i]);
			} else if(t.startsWith("-output")) {
				outputDatabaseName = getOptionData(args[i]);
			} else if(t.startsWith("-script")) {
				scriptName = getOptionData(args[i]);
			}
		}
		if(inputDatabaseName == null || inputDatabaseName.length() <= 0) {
			System.out.println("Missing -input database name");
			System.exit(1);
		}
		if(outputDatabaseName == null || outputDatabaseName.length() <= 0) {
			System.out.println("Missing -output database name");
			System.exit(1);
		}
		if(scriptName == null || scriptName.length() <= 0) {
			System.out.println("Missing -script file name");
			System.exit(1);
		}
		File scriptFile = findFile(scriptName);
		if(scriptFile == null) {
			System.out.println("Unable to find script file " + scriptName);
			System.exit(1);
		}

		// Convert
		DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];

		DatabaseSelection inputDatabase = new DatabaseSelection();
		inputDatabase.serverName = defaultDatabase.serverName;
		inputDatabase.databaseName = inputDatabaseName;

		DatabaseSelection outputDatabase = new DatabaseSelection();
		outputDatabase.serverName = defaultDatabase.serverName;
		outputDatabase.databaseName = outputDatabaseName;

		ArrayList<String> messages = new ArrayList<String>();
		try {
			DatabaseUtilities.executeConversionScript(scriptFile,outputDatabase,inputDatabase,defaultDatabase,messages);
			System.out.println("Conversion successful.");
		} catch(Exception e) {
			System.out.println("Conversion failed: " + e.getMessage());
			e.printStackTrace();
			System.exit(1);
		}
		for(Iterator<String> i=messages.iterator();i.hasNext();) {
			String message = i.next();
			System.out.println(message);
		}

		System.exit(0);
	}

	/**
	 * Get the data portion of an option of the form "-option=data".
	 * @param optionText full option text
	 * @return data portion of the option, never null but may be blank
	**/
	static String getOptionData(String optionText) {
		int index = optionText.indexOf('=');
		if(index < 0) {
			return "";
		}
		return StringUtilities.substring(optionText,index+1).trim();
	}

	/**
	 * Locate a script file, looking first in the current working directory
	 * then in the database folder then in the database/ConversionScripts folder.
	 * @param name name of the file to be located
	 * @return the located file or null if the file does not exist
	**/
	static File findFile(String name) {
		File f = new File(name);
		if(f.exists()) {
			return f;
		}

		f = new File("database",name);
		if(f.exists()) {
			return f;
		}

		f = new File("database/ConversionScripts",name);
		if(f.exists()) {
			return f;
		}

		return null;
	}
}
