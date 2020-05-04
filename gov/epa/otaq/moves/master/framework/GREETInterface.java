/**************************************************************************************************
 * @(#)GREETInterface.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.io.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.sql.*;
import gov.epa.otaq.moves.common.StreamGobbler;

/**
 * The interface with GREET class to receive Well To Pump and 
 * Manufacture/Disposal files.  An XML file is created when the 
 * "Update Well-To-Pump Rates" or "Update Manufacture and Disposal Rates" menu options
 * are selected.  The Well to Pump file from GREET is imported into the 
 * GREETWellToPump table, and the Manufacture and Disposal file is imported into
 * the GREETManfAndDisposal table.
 *<br>
 * Sample Well to Pump XML file (input file)
 * <pre>
 *	&lt;MOVEStoGREET&gt;
 *		&lt;WellToPump uncertainty="yes"&gt;
 *			&lt;Years&gt;
 *				&lt;Year id="1999"/&gt;
 *				&lt;Year id="2000"/&gt;
 *			&lt;/Years&gt;
 *			&lt;FuelSubTypes&gt;
 *				&lt;FuelSubType FuelType="Gasoline" description="Reformulated Gasoline (RFG)"/&gt;
 *				&lt;FuelSubType FuelType="Gasoline" description="Conventional Diesel Fuel"/&gt;
 *				&lt;FuelSubType FuelType="Diesel Fuel" description="Fischer-Troppes Diesel"/&gt;
 *			&lt;/FuelSubTypes&gt;
 *		&lt;/WellToPump&gt;
 *	&lt;/MOVEStoGREET&gt;
 * </pre>
 *
 * Sample Manufacture and Disposal XML file (input file)
 * <pre>
 * 	&lt;MOVEStoGREET&gt;
 *		&lt;ManufactureAndDisposal uncertainty="no"&gt;
 *			&lt;Years&gt;
 *				&lt;Year id="1999"/&gt;
 *				&lt;Year id="2000"/&gt;
 *			&lt;/Years&gt;
 *			&lt;ModelYears&gt;
 *				&lt;ModelYear id="1969"/&gt;
 *				&lt;ModelYear id="1970"/&gt;
 *                     ........
 *				&lt;ModelYear id="2000"/&gt;
 *			&lt;/ModelYears&gt;
 *			&lt;FuelSubTypes&gt;
 *				&lt;FuelSubType FuelType="Gasoline" description="Reformulated Gasoline (RFG)"/&gt;
 *				&lt;FuelSubType FuelType="Gasoline" description="Conventional Diesel Fuel"/&gt;
 *				&lt;FuelSubType FuelType="Diesel Fuel" description="Fischer-Troppes Diesel"/&gt;
 *			&lt;/FuelSubTypes&gt;
 *		&lt;/ManufactureAndDisposal&gt;
 * 	&lt;/MOVEStoGREET&gt;
 * </pre>
 *
 * Sample Well to Pump Output Tab Separated file.  The values are yearID, pollutantID,
 * fuelSubTypeID, and emissionRate respectively.  These values are then 
 * stored into the GREETWellToPump table.  The last field if the GREETWellToPump 
 * table, emissionRateUncertainty will remain NULL.
 * <pre>
 *	1990	5	10	1476.89
 *	1991	5	10	1477.63
 *	1992	5	10	1478.37
 *	1993	5	10	1479.11
 *	1994	5	10	1479.86
 *       ............
 * </pre>
 *
 * Sample Manufacture and Disposal Output Tab Separated file.  The values are GREETVehicleType, modelYearID,
 * pollutantID, emissionStage, and emissionPerVehicle respectively.  These values are then 
 * stored into the GREETManfAndDisposal table. 
 * <pre>
 *	1	1970	5	MANF	295.258
 *	1	1970	5	CONS	295.258
 *	1	1970	5	DISP	295.258
 *	1	1971	5	MANF	295.408
 *	1	1971	5	CONS	295.408
 *       ............
 * </pre>
 *
 * There are three command line parameters for GREETWTPStub GREETMfgStub,
 * the input file name, the output file name, and the errors file name.
 * Typical command lines are as follows:
 * <pre>
 * GREETWTPStub.exe C:\WTPInput.xml C:\WTPOutput.dat C:\WTPErrors.txt
 * GREETMfgStub.exe C:\MfgInput.xml C:\MfgOutput.dat C:\MfgErrors.txt
 * </pre>
 *
 * @author		Wesley Faler
 * @author		EPA-elg
 * @author		EPA-Mitch C.
 * @version		2009-03-30
**/
public class GREETInterface {
	/** the current runspec to get the selected information **/
	RunSpec runspec;
	
	/** Integer modelYears calculated from calendar years and ages **/
	TreeSet<Integer> modelYears = new TreeSet<Integer>();
	
	/** fuelTypeID Integers **/
	TreeSet<Integer> fuelTypes = new TreeSet<Integer>();
	
	/** uncertainty as selectd by the user **/
	boolean doUncertainty = false;
	String doUncertaintyText = "no";
	
	/** name of database to store GreetWelltoPumpTable
	 *    (and eventually GREETManfAndDisposal table)
	 *  as input by user  **/
	String userDatabaseName = null;

	/** a class for the information about fuels retrieved from the database tables **/
	class FuelInfo implements Comparable {
		public String fuelTypeID;
		public String fuelTypeDesc;
		public String fuelSubTypeID;
		public String fuelSubTypeDesc;

		public int compareTo(Object other) {
			FuelInfo o = (FuelInfo)other;
			if(!fuelTypeID.equals(o.fuelTypeID)) {
				return fuelTypeID.compareTo(o.fuelTypeID);
			}
			if(!fuelTypeDesc.equalsIgnoreCase(o.fuelTypeDesc)) {
				return fuelTypeDesc.compareTo(o.fuelTypeDesc);
			}
			if(!fuelSubTypeID.equals(o.fuelSubTypeID)) {
				return fuelSubTypeID.compareTo(o.fuelSubTypeID);
			}
			if(!fuelSubTypeDesc.equalsIgnoreCase(o.fuelSubTypeDesc)) {
				return fuelSubTypeDesc.compareTo(o.fuelSubTypeDesc);
			}
			return 0;
		}
	}
	
	/** FuelInfo objects retrievesd from the MOVESDefault database **/
	TreeSet<FuelInfo> fuels = new TreeSet<FuelInfo>();

	/** Integer ageIDs retrieved from the MOVESDefault database **/
	TreeSet<Integer> ageIDs = new TreeSet<Integer>();
	
	/** 
	 * Constructor
	 * @param runspecToUse RunSpec object that should be examined for years, fuel types,
	 * and model years.
	**/
	public GREETInterface(RunSpec runspecToUse) {
		runspec = runspecToUse;
		getYearAndFuelInfo();
	}

	/**
	 * Get Years and Fuel information.  The fuelTypeID, fuelTypeDesc, fuelSubTypeID,
	 * and fuelSubTypeDesc fields are retrieved from the fuelType and fuelSubType
	 * tables, and stored into the fuels Treeset.  The ageID from the ageCategory
	 * table is stored into the ageIDs Treeset.  The ageIDs are then used to calculate the 
	 * modelYears.
 	 * @return true if the fields are retrieved successfully from the database, 
 	 * false otherwise.
	**/
	public boolean getYearAndFuelInfo() {	
		for(Iterator<OnRoadVehicleSelection> i = runspec.onRoadVehicleSelections.iterator();
				i.hasNext();) {
			OnRoadVehicleSelection onRoadVehicleSelection = (OnRoadVehicleSelection) i.next();
			fuelTypes.add(new Integer(onRoadVehicleSelection.fuelTypeID));
		}
		String sql = "SELECT FuelType.fuelTypeID, fuelTypeDesc, "
				+ "fuelSubTypeID, fuelSubTypeDesc "
				+ "FROM FuelType, FuelSubType "
				+ "WHERE FuelType.fuelTypeID = FuelSubType.fuelTypeID ";

		String andClause = "AND FuelType.fuelTypeID IN (";
		String fuelTypesSQL = "";
		boolean isFirst = true;
		for(Iterator<Integer> i=fuelTypes.iterator();i.hasNext();) {
			Integer fuelTypeID = (Integer)i.next();
			if(!isFirst) {
				fuelTypesSQL += ",";
			}
			isFirst = false;
			fuelTypesSQL += fuelTypeID;
		}
		andClause += fuelTypesSQL + ")";
		if (fuelTypesSQL != "") {
			sql += andClause;
		}

		Connection defaultDatabase = null;
		try {
			defaultDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.DEFAULT);
			ResultSet result = SQLRunner.executeQuery(defaultDatabase, sql);
			while(result.next()) {
				FuelInfo fuel = new FuelInfo();
				fuel.fuelTypeID = result.getString("fuelTypeID");
				fuel.fuelTypeDesc = result.getString("fuelTypeDesc");
				fuel.fuelSubTypeID = result.getString("fuelSubTypeID");
				fuel.fuelSubTypeDesc = result.getString("fuelSubTypeDesc");
				fuels.add(fuel);
			}
			result.close();

			sql = "SELECT ageID from AgeCategory;";
			result = SQLRunner.executeQuery(defaultDatabase, sql);
			while(result.next()) {
				ageIDs.add(new Integer(result.getInt("ageID")));
			}
			result.close();
		} catch(Exception e) {
			/** @nonissue **/
			Logger.logError(e,"Unable to get the Execution Database connection or SQL is wrong in"
					+ " GREET Interface.");
			return false;
		} finally {
			if(defaultDatabase != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
						defaultDatabase);
				defaultDatabase = null;
			}
		}
		
		for(Iterator<Integer> i = runspec.timeSpan.years.iterator(); i.hasNext(); ) {
			Integer y = (Integer)i.next();
			for(Iterator<Integer> j = ageIDs.iterator(); j.hasNext(); ){
				Integer a = (Integer)j.next();
				modelYears.add(new Integer(y.intValue() - a.intValue()));
			}
		}
		return true;
	}
	
	/**
	 * Interface with GREET for the purpose of getting updates emission rates
	 * for the Well-to-Pump process.  This involves interacting with the user
	 * and the currently loaded RunSpec.  The commandLineParameters is an array
	 * of String type variables.  The input file name is assigned to the first
	 * element, the output file name is assigned to the second element, and the
	 * third element is assigned to the third element.  The user is prompted for 
	 * Uncertainty.  Then the function importAndStoreGREETData is called to
	 * import the data and to store the data into the GREETWellToPump table.
	 * The execution file names for GREETWTPApplication application is
	 * obtained from the System Configuration file.
	**/
	public void updateWellToPumpRates() {
		// make sure runspec contains the input information that will be needed
		if(runspec.timeSpan.years.isEmpty()) {
			/** @nonissue **/
			JOptionPane.showMessageDialog(null, "RunSpec must specify at least one calendar year.",
					"Runspec Calendar Years Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		if (fuelTypes.isEmpty()) {
			/** @nonissue **/
			JOptionPane.showMessageDialog(null, "RunSpec must specify at least sourcetype-fueltype.",
					"Runspec Fuels Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		String[] commandLineParameters = new String[3];
		//first parameter is the input file
		File inputFile = new File(SystemConfiguration.getTheSystemConfiguration().GREETDirectory,
				"WTPInput.xml");
		try {
			commandLineParameters[0] = inputFile.getCanonicalPath();
		} catch (IOException exception) {
			/** @nonissue **/
			Logger.logError(exception,"Unable to get input file");
		}
		//second parameter is the output file
		File outputFile = new File(SystemConfiguration.getTheSystemConfiguration().GREETDirectory,
				"WTPOutput.dat");
		try {
			commandLineParameters[1] = outputFile.getCanonicalPath();
		} catch (IOException exception) {
			/** @nonissue **/
			Logger.logError(exception,"Unable to get output file");
		}
		//third parameter is the error file
		File errorsFile = new File(SystemConfiguration.getTheSystemConfiguration().GREETDirectory,
				"WTPErrors.txt");
		try {
			commandLineParameters[2] = errorsFile.getCanonicalPath();
		} catch(IOException exception) {
			/** @nonissue **/
			Logger.logError(exception,"Unable to get error file");
		}

		// Ensure files we will write into don't already exist
		try {
			inputFile.delete();
		} catch(Exception e) {
			// It is not an error if inputFile did not exist
		}
		try {
			outputFile.delete();
		} catch(Exception e) {
			// It is not an error if outputFile did not exist
		}
		try {
			errorsFile.delete();
		} catch(Exception e) {
			// It is not an error if errorsFile did not exist
		}
//          	This is commented out for now in the model.
//		promptForUncertainty();
		promptForDatabaseName();
		writeWellToPump(inputFile);
		try {
			runApplication(SystemConfiguration.getTheSystemConfiguration().GREETWTPApplication,
					commandLineParameters, 
					SystemConfiguration.getTheSystemConfiguration().GREETDirectory);
		} catch(IOException ioe) {
			/** @nonissue **/
			Logger.logError(ioe,"Unable to process IO command");
		} catch(InterruptedException ie) {
			/** @nonissue **/
			Logger.logError(ie,"runApplication failed");
		}
		String createFields = "(yearID SMALLINT, pollutantID SMALLINT," +
				"fuelSubTypeID SMALLINT, emissionRate FLOAT, emissionRateUncertainty FLOAT) ";
		importAndStoreGREETData(inputFile, outputFile, errorsFile, "GREETWellToPump", createFields);
	}
	
	/**
	 * Interface with GREET for the purpose of getting updates emission rates
	 * for the Manufacture and disposal process.  This involves interacting with the user
	 * and the currently loaded RunSpec.  The commandLineParameters is an array
	 * of String type variables.  The input file name is assigned to the first
	 * element, the output file name is assigned to the second element, and the
	 * third element is assigned to the third element.  The user is prompted for 
	 * Uncertainty.  Then the function importAndStoreGREETData is called to
	 * import the data and to store the data into the GREETManfAndDisposal table.
	 * The execution file names for GREETManufactureAndDisposal application is
	 * obtained from the System Configuration file.
	**/
	public void updateManufactureDisposalRates() {
		String[] commandLineParameters = new String[3];
		//first parameter is the input file
		File inputFile = new File(SystemConfiguration.getTheSystemConfiguration().GREETDirectory,
				"MfgInput.xml");
		try {		
			commandLineParameters[0] = inputFile.getCanonicalPath();
		} catch (IOException exception) {
			/** @nonissue **/
			Logger.logError(exception,"Unable to get the input file");
		}
		//second parameter is the output file
		File outputFile = new File(SystemConfiguration.getTheSystemConfiguration().GREETDirectory,
			"MfgOutput.dat");
		try {
			commandLineParameters[1] = outputFile.getCanonicalPath();
		} catch (IOException exception) {
			/** @nonissue **/
			Logger.logError(exception,"Unable to get the output file");
		}
		//third parameter is the error file
		File errorsFile = new File(SystemConfiguration.getTheSystemConfiguration().GREETDirectory,
				"MfgErrors.txt");
		try {
			commandLineParameters[2] = errorsFile.getCanonicalPath();
		} catch(IOException exception) {
			/** @nonissue **/
			Logger.logError(exception,"Unable to get the error file");
		}

		// Ensure files we will write into don't already exist
		try {
			inputFile.delete();
		} catch(Exception e) {
			// It is not an error if inputFile did not exist
		}
		try {
			outputFile.delete();
		} catch(Exception e) {
			// It is not an error if outputFile did not exist
		}
		try {
			errorsFile.delete();
		} catch(Exception e) {
			// It is not an error if errorsFile did not exist
		}
//          	This is commented out for now in the model.
//		promptForUncertainty();

		writeManufactureDisposal(inputFile);
		
		try {
			runApplication
					(SystemConfiguration.getTheSystemConfiguration().GREETManufactureApplication,
					commandLineParameters, 
					SystemConfiguration.getTheSystemConfiguration().GREETDirectory);
		} catch(IOException ioe) {
			/** @nonissue **/
			Logger.logError(ioe,"Unable to get the input, output, error files");
		} catch(InterruptedException ie) {
			/** @nonissue **/
			Logger.logError(ie,"Execution of the GREET application failed");
		}
		String createFields = "(GREETVehicleType  SMALLINT," +
				"modelYearID SMALLINT, pollutantID SMALLINT, " +
				"EmissionStage CHAR(4), emissionPerVehicle FLOAT NULL)";
		importAndStoreGREETData(inputFile, outputFile, errorsFile, "GREETManfAndDisposal", createFields);
	}

	/** 
	 * Ask the user if uncertainty calculations are desired and set doUncertainty
	 * and doUncertaintyText accordingly.
	**/
	void promptForUncertainty() {
		int answer = JOptionPane.showConfirmDialog(null,
				StringUtilities.wrapString(
				"Do you want to handle uncertainty using Crystal Ball?",25),
				"Uncertainty?",JOptionPane.YES_NO_OPTION);
		if(answer == JOptionPane.YES_OPTION) {
			doUncertainty = true;
			doUncertaintyText = "yes";
		} else {
			doUncertainty = false;
			doUncertaintyText = "no";
		}	
	}
	
	/** 
	 * Ask the user for the name of a database for the updated emission rates from GREET.
	 * Intent is that this will subsequently be used as a user input database to MOVES
	**/
	void promptForDatabaseName() {
		while(userDatabaseName == null || userDatabaseName.length() <= 0 ||
				userDatabaseName.startsWith(" "))  {
			userDatabaseName = JOptionPane.showInputDialog(
					StringUtilities.wrapString(
					"Enter name of database for updated emission rates from GREET",25));
		}
	}
	
	/**
	 * Synchronous method to execute a given application.
	 * @param applicationPath Path to the application to run
	 * @param commandLineParameters The command line parameters to pass to the application.
	 * @param workingDirectoryPath Path to the directory to use as the current working directory.
	 * be null. This is closed upon completion.
	 * @throws IOException If an IO operation fails.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	void runApplication(File applicationPath,
				String[] commandLineParameters, File workingDirectoryPath)
				throws IOException, InterruptedException {
		// Force reclaimation of memory immediately so that the application we launch has as
		// much memory to do its work as possible
		Runtime.getRuntime().runFinalization();
		Runtime.getRuntime().gc();
		try {
			/** @nonissue **/
			Logger.log(LogMessageCategory.DEBUG, "Run " + applicationPath.getName() + " in " +
					workingDirectoryPath.getCanonicalPath());
			Logger.log(LogMessageCategory.DEBUG, "Command line = ");
			for(int i = 0; i < commandLineParameters.length; i++) {
				if(i > 0) {
					/** @nonissue **/
					Logger.log(LogMessageCategory.DEBUG, ", ");
				}
				/** @nonissue **/
				Logger.log(LogMessageCategory.DEBUG, commandLineParameters[i]);
			}
			/** @nonissue **/
			Logger.log(LogMessageCategory.DEBUG, "Command line = ");
			String[] commandArray = new String[commandLineParameters.length + 1];
			commandArray[0] = applicationPath.getCanonicalPath();
			for(int i = 0; i < commandLineParameters.length; i++) {
				commandArray[i + 1] = commandLineParameters[i];
			}
			
			Runtime runtime = Runtime.getRuntime();
			Process process = runtime.exec(commandArray, null, workingDirectoryPath);

			if(process != null) {
				// Launch two StreamGobbler threads to keep the process's output stream buffers
				// empty and prevent the process from hanging.
				StreamGobbler errorGobbler = new StreamGobbler(
							process.getErrorStream(), "ERROR", null);

				errorGobbler.start();

				process.waitFor();
				errorGobbler.join();
			}
		} finally {
		}
	}
	
	/**
	 * Import a GREET Well To Pump or a GREET Manufacture and Disposal 
	 * file into a temporary table, and then replace into the GREETWellToPump, 
	 * or GREETManfAndDisposal tables.
	 * @param inputFile The input file, either WTPInput.xml or MfgInput.xml.
	 * @param outputFile The output file, either WTPOutput.dat or MfgOutput.dat.
	 * @param errorsFile The errors file, WTPErros.txt.
	 * @param tableName The table name GREETWellToPump or GREETManfAndDisposal.
	 * @param createFields A string of fields of the GREETWellToPump or GREETManfAndDisposal
	 * to be used to create the temporary GREET table.
	**/
	void importAndStoreGREETData(File inputFile, File outputFile, File errorsFile, 
			String tableName, String createFields) {
		String sql;
		Connection defaultDB = null;
		PreparedStatement statement = null;
		if(outputFile.length() != 0) {
			try {
				defaultDB = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.DEFAULT);
				// input databases are not supported by DatabaseConnectionManager
				// but we can use connection to MOVESDefault
				// it is important, however, not to USE any other database
				// or connection, which is returned to pool, is damaged
				sql = "CREATE DATABASE IF NOT EXISTS " + userDatabaseName;
				SQLRunner.executeSQL(defaultDB, sql);
				String tableName2 = userDatabaseName.trim() + "." + tableName;
				sql = "DROP TABLE IF EXISTS " + tableName2;
				SQLRunner.executeSQL(defaultDB, sql);
				sql = "CREATE TABLE IF NOT EXISTS " + tableName2 + createFields;
				SQLRunner.executeSQL(defaultDB, sql);
				sql = "LOAD DATA INFILE " 
						+ DatabaseUtilities.escapeSQL(outputFile.getCanonicalPath())
						+ " INTO TABLE " + tableName2;
				SQLRunner.executeSQL(defaultDB, sql);
				String messageString = "GREET rates imported successfully into database " + userDatabaseName;
				/** @nonissue **/
				JOptionPane.showMessageDialog(null, StringUtilities.wrapString(messageString, 35)); 
			} catch(Exception e) {
				Logger.logError(e, "Storing into " + tableName + " failed.");
			} finally {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT, defaultDB);
			}
		}
		if((outputFile.length() == 0) || (errorsFile.length() !=0)) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.ERROR, "Error receiving file " + outputFile +
			" from GREET");
		}
		try {
			//inputFile.delete();
		} catch(Exception e) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.WARNING, "Failed to delete the " + inputFile + " file");
		}
		try {
			//outputFile.delete();
		} catch(Exception e) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.WARNING, "Failed to delete the " + outputFile + " file");
		}
		try {
			//errorsFile.delete();
		} catch(Exception e) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.WARNING, "Failed to delete the " + errorsFile + " file");
		}
	}
	
	/** 
	 * Write the Well To Pump XML file
	 * @param inputFile file to be filled with XML
 	 * @return true if file is written successfully, false if the XML is not
 	 * created successfully
	**/
	boolean writeWellToPump(File inputFile) {
		PrintWriter printWriter = null;
		try {
			printWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new
					FileOutputStream(inputFile))));
		} catch(Exception e) {
			/** @nonissue **/
			Logger.logError(e,"Unable to save the Well To Pump XML file.");
			return false;
		}	
		printWriter.println("<MOVEStoGREET>");	
		printWriter.println("\t<WellToPump uncertainty=\"" + doUncertaintyText + "\">");
		printWriter.println("\t\t<Years>");

		// we have yearID above from runspec, write a loop to print <ModelYear id="2001">
		for(Iterator<Integer> i = runspec.timeSpan.years.iterator(); i.hasNext(); ) {
			Integer y = (Integer)i.next();
			printWriter.println("\t\t\t<Year id=\"" + y + "\"/>");
		}
		printWriter.println("\t\t</Years>");
		printWriter.println("\t\t<FuelSubTypes>");
		for(Iterator<FuelInfo> i=fuels.iterator();i.hasNext();) {
			FuelInfo fi = (FuelInfo) i.next();
			printWriter.println("\t\t\t<FuelSubType FuelType=\"" + 
					fi.fuelTypeDesc +
					"\" description=\"" + 
					fi.fuelSubTypeDesc + "\"/>");			
		}
		printWriter.println("\t\t</FuelSubTypes>");
		printWriter.println("\t</WellToPump>");
		printWriter.println("</MOVEStoGREET>");
		printWriter.close();
		return true;
	}
	
	/** 
	 * Write the Manufacture and Disposal XML file
	 * @param inputFile file to be filled with XML
 	 * @return true if file is written successfully, false if the XML is not
 	 * created successfully
	**/
	boolean writeManufactureDisposal(File inputFile) {
		PrintWriter printWriter = null;
		try {
			printWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(inputFile))));
		} catch(Exception e) {
			/** @nonissue **/
			Logger.logError(e,"Unable to save the Manufacture and Disposal XML file.");
			return false;
		}	
		printWriter.println("<MOVEStoGREET>");
		printWriter.println("\t<ManufactureAndDisposal uncertainty=\"" + 
				doUncertaintyText + "\">");
		// we have yearID above from runspec, write a loop to print <ModelYear id="2001">
		// and go back 30 years
		printWriter.println("\t\t<Years>");
		for(Iterator<Integer> i = runspec.timeSpan.years.iterator(); i.hasNext(); ) {
			Integer y = (Integer)i.next();	
			printWriter.println("\t\t\t<Year id=\"" + y + "\"/>");
		}
		printWriter.println("\t\t</Years>");
		printWriter.println("\t\t<ModelYears>");
		int n=0;
		for(Iterator<Integer> i = modelYears.iterator(); i.hasNext();) {	
			printWriter.println("\t\t\t<ModelYear id=\"" + (((Integer)i.next()).intValue()) + 
					"\"/>");
		}
		printWriter.println("\t\t</ModelYears>");
		
		printWriter.println("\t\t<FuelSubTypes>");
		for(Iterator<FuelInfo> i=fuels.iterator();i.hasNext();) {
			FuelInfo fi = (FuelInfo) i.next();
			printWriter.println("\t\t\t<FuelSubType FuelType=\"" + 
					fi.fuelTypeDesc +
					"\" description=\"" + 
					fi.fuelSubTypeDesc + "\"/>");
		}
		printWriter.println("\t\t</FuelSubTypes>");
		printWriter.println("\t</ManufactureAndDisposal>");
		printWriter.println("</MOVEStoGREET>");
		printWriter.close();
		return true;
	}
}
