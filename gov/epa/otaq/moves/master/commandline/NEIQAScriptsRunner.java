/**************************************************************************************************
 * @(#)NEIQAScriptsRunner.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.commandline;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.regex.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;

/**
 * Batch mode running NEI QA Scripts.
 *
 * @author		Daniel Bizer-Cox
 * @version		2021-02-10
**/
public class NEIQAScriptsRunner {
	/** Invokes the main application. **/
	public static void main(String[] args) {
		Configuration.allowGUI = false;

		if(!SystemConfiguration.getTheSystemConfiguration().didLoad) {
			System.exit(1);
		}

		// Find command line arguments
		String inputDBArg = "";
		List<String> inputDBs = null;
		String outputFileName = "";
		String scriptName = "";
		for(int i=0;i<args.length;i++) {
			String t = args[i].toLowerCase();
			if(t.startsWith("-input")) {
				inputDBArg = getOptionData(args[i]);
			} else if(t.startsWith("-output")) {
				outputFileName = getOptionData(args[i]);
			} else if(t.startsWith("-script")) {
				scriptName = getOptionData(args[i]);
			}
		}
		
		// parse -input arg
		if(inputDBArg == null || inputDBArg.length() <= 0) {
			Logger.log(LogMessageCategory.ERROR, "Missing -input argument");
			System.exit(1);
		}
		// first see if they passed a file
		try {
			File inputDBFile = findFile(inputDBArg);
			if(inputDBFile != null) {
				inputDBs = new ArrayList<String>();
				BufferedReader input = new BufferedReader(new FileReader(inputDBFile));
				for(String line = input.readLine(); line != null; line = input.readLine()) {
					if (line.trim().equals("")) {
						continue;
					}
					inputDBs.add(line.trim());
				}
				input.close();
				Logger.log(LogMessageCategory.INFO, "Found " + inputDBs.size() + " database(s) to process from " + inputDBFile.getName());
			} else if (inputDBArg.contains(".")) { // couldn't find the argument as a file... is it a file?
				Logger.log(LogMessageCategory.ERROR, "Couldn't open the file passed in -input: " + inputDBArg);
				System.exit(1);
			}
		} catch (IOException e) {
			Logger.log(LogMessageCategory.ERROR, "Couldn't open the file passed in -input: " + inputDBArg);
			System.exit(1);
		}
		// then see if they passed a direct list of dbs
		if (inputDBs == null) {
			try {
				inputDBs = Arrays.asList(inputDBArg.split(","));
				Logger.log(LogMessageCategory.INFO, inputDBs.size() + " database(s) from the command line argument will be processed");
			} catch (PatternSyntaxException e) {
				Logger.log(LogMessageCategory.ERROR, "Couldn't interpret -input argument. If you passed a file path, check it for errors; if you passed 1 or more database name(s), make sure the entire list is quoted and comma separated");
				System.exit(1);
			}
		}
		
		// parse -output arg
		if(outputFileName == null || outputFileName.length() <= 0) {
			Logger.log(LogMessageCategory.ERROR, "Missing -output file name");
			System.exit(1);
		}
		
		// parse -script arg
		if(scriptName == null || scriptName.length() <= 0) {
			Logger.log(LogMessageCategory.ERROR, "Missing -script file name");
			System.exit(1);
		}
		File scriptFile = findFile(scriptName);
		if(scriptFile == null) {
			Logger.log(LogMessageCategory.ERROR, "Unable to find script file " + scriptName);
			System.exit(1);
		}

		// get the default database
		DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];

		// run script for each input database
		for (String inputDB : inputDBs) {
			DatabaseSelection inputDatabase = new DatabaseSelection();
			inputDatabase.serverName = defaultDatabase.serverName;
			inputDatabase.databaseName = inputDB;

			try {
				Logger.log(LogMessageCategory.INFO, "Checking " + inputDB + "...");
				DatabaseUtilities.executeNEIQA(scriptFile, inputDatabase, defaultDatabase);
			} catch(Exception e) {
				Logger.log(LogMessageCategory.ERROR, "Checking " + inputDB + " failed: " + e.getMessage());
				
				if (e.getMessage().contains("Unknown column 'idleRegionID' in 'field list'")) {
					Logger.log(LogMessageCategory.ERROR, "Input database " + inputDB + " was likely created with a previous version of MOVES.");
				}
				
				//e.printStackTrace();
				Logger.log(LogMessageCategory.ERROR, "Output file will not be saved.");
				System.exit(1);
			}
		}
		
		// save results
		boolean success = false;
		File outputFile = new File(outputFileName);
		try {
			success = DatabaseUtilities.saveNEIQA(defaultDatabase, outputFile);
		} catch(Exception e) {
			Logger.log(LogMessageCategory.ERROR, "Couldn't save results to " + outputFileName + ": " + e.getMessage());
			//e.printStackTrace();
			System.exit(1);
		}
		
		if(!success) {
			Logger.log(LogMessageCategory.ERROR, "Couldn't save results to " + outputFileName + ": ");
			System.exit(1);
		} else {
			Logger.log(LogMessageCategory.INFO, "Saved results to " + outputFileName);
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
	 * then in the database folder then in the database/NEIQA folder.
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

		f = new File("database/NEIQA",name);
		if(f.exists()) {
			return f;
		}

		return null;
	}
}
