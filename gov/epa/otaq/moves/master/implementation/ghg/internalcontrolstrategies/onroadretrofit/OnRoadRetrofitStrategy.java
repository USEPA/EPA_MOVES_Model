/**************************************************************************************************
 * @(#)OnRoadRetrofitStrategy.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.onroadretrofit;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.util.*;
import java.sql.*;
import java.io.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import java.awt.*;
import javax.swing.*;

/**
 * Implements a Control Strategy for On-Road Vehicle Retrofit Modeling
 *
 * @author		Wesley Faler
 * @author 		Tim Hull
 * @version		2014-03-19
**/
public class OnRoadRetrofitStrategy extends InternalControlStrategy
		implements InternalControlStrategySingleInstanceOnly, IImportExportHandler, InternalControlStrategyUseImporterOnly {
	private static final boolean RESTRICT_FUEL_AND_SOURCE_TYPES = true;
	private static final String[] allowedFuelTypes = { "2" };
	private static final String[] allowedSourceTypes = {
			"32", "41", "42", "43", "51", "52", "53", "61", "62"
	};
	private static TreeMap<String,String> abbreviationCache = new TreeMap<String,String>();
	private static String[] columnTitles = {
		"Pollutant", "Process", "Fuel", "Source",
		"Initial Calendar Year", "Final Calendar Year",
		"Initial Model Year", "Final Model Year",
		"Fraction/Year", "Fraction Effective"
	};
	private static String[] dbColumnTitles = {
		"pollutantID", "processID", "fuelTypeID", "sourceTypeID",
		"retrofitYearID",
		"beginModelYearID", "endModelYearID",
		"cumFractionRetrofit", "retrofitEffectiveFraction"
	};

	public static String findAbbreviationCore(String listName, String abbreviation) {
		String cacheKey = listName + "|" + abbreviation;
		String result = (String)abbreviationCache.get(cacheKey);
		if(result != null) {
			return result;
		}
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);
		abbreviation = abbreviation.toLowerCase();
		String sql = "";
		try {
			// Try the abbreviation list first
			sql = "select idealName from RetrofitInputAssociations where listName="
					+ DatabaseUtilities.escapeSQL(listName,true)
					+ " and lower(commonName)="
					+ DatabaseUtilities.escapeSQL(abbreviation,true);
			result = SQLRunner.executeScalarString(db,sql);
			if(result != null && result.length() > 0) {
				abbreviationCache.put(cacheKey,result);
				return result;
			}
			// Try the standard table, if known from the listName
			if(listName.equalsIgnoreCase("Pollutant")) {
				sql = "select pollutantID from Pollutant where lower(pollutantName)="
						+ DatabaseUtilities.escapeSQL(abbreviation,true);
			} else if(listName.equalsIgnoreCase("Process")) {
				sql = "select processID from EmissionProcess where lower(processName)="
						+ DatabaseUtilities.escapeSQL(abbreviation,true);
			} else if(listName.equalsIgnoreCase("Fuel")) {
				sql = "select fuelTypeID from FuelType where lower(fuelTypeDesc)="
						+ DatabaseUtilities.escapeSQL(abbreviation,true);
			} else if(listName.equalsIgnoreCase("Source")) {
				sql = "select sourceTypeID from SourceUseType where lower(sourceTypeName)="
						+ DatabaseUtilities.escapeSQL(abbreviation,true);
			} else {
				return null;
			}
			result = SQLRunner.executeScalarString(db,sql);
			if(result != null && result.length() > 0) {
				abbreviationCache.put(cacheKey,result);
				return result;
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to lookup retrofit abbreviation",sql);
		}
		return null;
	}

	public class Line {
		public boolean valid = true;

		public String pollutantCode;
		public String pollutantID;
		public String processCode;
		public String processID;
		public String fuelCode;
		public String fuelTypeID;
		public String sourceCode;
		public String sourceTypeID;
		public int initialCalendarYear;
		public int finalCalendarYear;
		public int initialModelYear;
		public int finalModelYear;
		public double fleetFractionPerYear;
		public double fractionEffective;

		public Line() {
			// Nothing to do here
		}

		public void fillArray(String[] parts) {
			parts[0] = pollutantCode;
			parts[1] = processCode;
			parts[2] = fuelCode;
			parts[3] = sourceCode;
			parts[4] = "" + initialCalendarYear;
			parts[5] = "" + finalCalendarYear;
			parts[6] = "" + initialModelYear;
			parts[7] = "" + finalModelYear;
			parts[8] = "" + fleetFractionPerYear;
			parts[9] = "" + fractionEffective;
		}

		public boolean load(ArrayList parts, int lineNumber, ArrayList<String> messages) {
			if(parts.size() < 10) {
				return false;
			}
			/*
			String t="";
			for(int i=0;i<parts.size();i++) {
				t += "[" + i + "]=" + parts.get(i).toString() + "\n";
			}
			Logger.log(LogMessageCategory.INFO,"line " + lineNumber + "=" + t);
			*/
			pollutantCode = toString((String)parts.get(0),
					"Pollutant", lineNumber, messages);
			pollutantID = findAbbreviation("Pollutant",pollutantCode,lineNumber,messages);
			processCode = toString((String)parts.get(1),
					"Process", lineNumber, messages);
			processID = findAbbreviation("Process",processCode,lineNumber,messages);
			fuelCode = toString((String)parts.get(2),
					"Fuel", lineNumber, messages);
			fuelTypeID = findAbbreviation("Fuel",fuelCode,lineNumber,messages);
			sourceCode = toString((String)parts.get(3),
					"Source", lineNumber, messages);
			sourceTypeID = findAbbreviation("Source",sourceCode,lineNumber,messages);
			initialCalendarYear = toInt((String)parts.get(4),0,
					"Initial Calendar Year", lineNumber, messages);
			finalCalendarYear = toInt((String)parts.get(5),0,
					"Final Calendar Year", lineNumber, messages);
			initialModelYear = toInt((String)parts.get(6),0,
					"Initial Model Year", lineNumber, messages);
			finalModelYear = toInt((String)parts.get(7),0,
					"Final Model Year", lineNumber, messages);
			fleetFractionPerYear = toDouble((String)parts.get(8),-1000,
					"Fleet Fraction Per Year", lineNumber, messages);
			fractionEffective = toDouble((String)parts.get(9),1000,
					"Percent Effective", lineNumber, messages);

			boolean calendarYearsOK = true;
			if(initialCalendarYear < 1990 || initialCalendarYear > 2050) {
				valid = false;
				calendarYearsOK = false;
				messages.add("Error: Initial Calendar Year on line " + lineNumber +
						" should be in the range of 1990-2050");
			}
			if(finalCalendarYear < 1990 || finalCalendarYear > 2050) {
				valid = false;
				calendarYearsOK = false;
				messages.add("Error: Final Calendar Year on line " + lineNumber +
						" should be in the range of 1990-2050");
			}
			if(calendarYearsOK && initialCalendarYear > finalCalendarYear) {
				valid = false;
				calendarYearsOK = false;
				messages.add("Error: Calendar years backward on line " + lineNumber);
			}

			boolean modelYearsOK = true;
			if(initialModelYear < 1990-30 || initialCalendarYear > 2050) {
				valid = false;
				modelYearsOK = false;
				messages.add("Error: Initial Model Year on line " + lineNumber +
						" should be in the range of 1990-2050");
			}
			if(finalModelYear < 1990-30 || finalCalendarYear > 2050) {
				valid = false;
				calendarYearsOK = false;
				messages.add("Error: Final Model Year on line " + lineNumber +
						" should be in the range of 1990-2050");
			}
			if(modelYearsOK && initialModelYear > finalModelYear) {
				valid = false;
				modelYearsOK = false;
				messages.add("Error: Model years backward on line " + lineNumber);
			}

			if(fleetFractionPerYear < 0 ||
						fleetFractionPerYear*(finalCalendarYear-initialCalendarYear+1) > 1.0) {
				valid = false;
				messages.add("Error: Fleet Fraction Per Year is invalid on line " + lineNumber);
			}

			if(fractionEffective > 1.0) {
				valid = false;
				messages.add("Error: Percent Effective was over 100% on line " + lineNumber);
			}

			if(modelYearsOK && calendarYearsOK) {
				if(initialModelYear < initialCalendarYear-30) {
					valid = false;
					messages.add("Error: Model year too old relative to calendar year on line " + lineNumber);
				}
			}

			if(valid && RESTRICT_FUEL_AND_SOURCE_TYPES) {
				boolean found = false;
				for(int i=0;i<allowedFuelTypes.length;i++) {
					if(fuelTypeID.equals(allowedFuelTypes[i])) {
						found = true;
						break;
					}
				}
				if(!found) {
					valid = false;
					messages.add("Error: Unsupported fuel type on line " + lineNumber);
				}

				found = false;
				for(int i=0;i<allowedSourceTypes.length;i++) {
					if(sourceTypeID.equals(allowedSourceTypes[i])) {
						found = true;
						break;
					}
				}
				if(!found) {
					valid = false;
					messages.add("Error: Unsupported source type on line " + lineNumber);
				}
			}

			return valid;
		}

		public String toString(String text, String name,
				int lineNumber, ArrayList<String> messages) {
			if(text == null) {
				text = "";
			}
			text = text.trim();
			if(text.length() <= 0) {
				String t = "Error: " + name + " on line " + lineNumber + " is empty";
				messages.add(t);
				valid = false;
				return "";
			}
			return text;
		}

		public String findAbbreviation(String listName, String abbreviation,
				int lineNumber, ArrayList<String> messages) {
			if(abbreviation == null) {
				String t = "Error: " + listName + " missing on line " + lineNumber;
				messages.add(t);
				valid = false;
				return "";
			}
			abbreviation = abbreviation.trim();
			if(abbreviation.length() <= 0) {
				String t = "Error: " + listName + " is blank on line " + lineNumber;
				messages.add(t);
				valid = false;
				return "";
			}
			String result = findAbbreviationCore(listName,abbreviation);
			if(result == null || result.length() <= 0) {
				String t = "Error: " + listName + " is invalid on line " + lineNumber;
				messages.add(t);
				valid = false;
				return "";
			}
			return result;
		}

		public int toInt(String text, int defaultValue, String name,
				int lineNumber, ArrayList<String> messages) {
			try {
				return (int)Double.parseDouble(text);
			} catch(Exception e) {
				String t = "Error: " + name + " on line " + lineNumber + " is not an integer";
				messages.add(t);
				valid = false;
				return defaultValue;
			}
		}

		public double toDouble(String text, double defaultValue, String name,
				int lineNumber, ArrayList<String> messages) {
			try {
				if(text.endsWith("%")) {
					return Double.parseDouble(text.substring(0,text.length()-1))/100.0;
				}
				return Double.parseDouble(text);
			} catch(Exception e) {
				String t = "Error: " + name + " on line " + lineNumber + " is not a number";
				messages.add(t);
				valid = false;
				return defaultValue;
			}
		}
	}

	public class DBLine {
		public boolean valid = true;

		public int pollutantID;
		public int processID;
		public int fuelTypeID;
		public int sourceTypeID;
		public int retrofitYearID;
		public int beginModelYearID;
		public int endModelYearID;
		public double cumFractionRetrofit;
		public double retrofitEffectiveFraction;

		public DBLine() {
			// Nothing to do here
		}

		public boolean load(ResultSet rs, ArrayList<String> messages) throws SQLException {
			pollutantID = rs.getInt("pollutantID");
			processID = rs.getInt("processID");
			fuelTypeID = rs.getInt("fuelTypeID");
			sourceTypeID = rs.getInt("sourceTypeID");
			retrofitYearID = rs.getInt("retrofitYearID");
			beginModelYearID = rs.getInt("beginModelYearID");
			endModelYearID = rs.getInt("endModelYearID");
			cumFractionRetrofit = rs.getDouble("cumFractionRetrofit");
			retrofitEffectiveFraction = rs.getDouble("retrofitEffectiveFraction");

			boolean calendarYearsOK = true;
			if(retrofitYearID < 1990 || retrofitYearID > 2050) {
				valid = false;
				calendarYearsOK = false;
				messages.add("Retrofit Error: Calendar Year (" + retrofitYearID + ") should be in the range of 1990-2050");
			}

			boolean modelYearsOK = true;
			if(beginModelYearID < 1990-30 || beginModelYearID > 2050) {
				valid = false;
				modelYearsOK = false;
				messages.add("Retrofit Error: beginModelYearID (" + beginModelYearID +
						") should be in the range of 1990-2050");
			}
			if(endModelYearID < 1990-30 || endModelYearID > 2050) {
				valid = false;
				calendarYearsOK = false;
				messages.add("Retrofit Error: endModelYearID (" + endModelYearID +
						") should be in the range of 1990-2050");
			}
			if(modelYearsOK && beginModelYearID > endModelYearID) {
				valid = false;
				modelYearsOK = false;
				messages.add("Retrofit Error: Model years backwards " + beginModelYearID + " to " + endModelYearID);
			}

			if(cumFractionRetrofit < 0 || cumFractionRetrofit > 1.0) {
				valid = false;
				messages.add("Retrofit Error: cumFractionRetrofit (" + cumFractionRetrofit + ") is invalid");
			}

			if(retrofitEffectiveFraction > 1.0) {
				valid = false;
				messages.add("Retrofit Error: retrofitEffectiveFraction (" + retrofitEffectiveFraction + ") is over 100%");
			}

			if(modelYearsOK && calendarYearsOK) {
				if(beginModelYearID < retrofitYearID-30) {
					valid = false;
					messages.add("Retrofit Error: Model year " + beginModelYearID + " too old relative to calendar year " + retrofitYearID);
				}
			}

			if(valid && RESTRICT_FUEL_AND_SOURCE_TYPES) {
				boolean found = false;
				String textFuelTypeID = "" + fuelTypeID;
				for(int i=0;i<allowedFuelTypes.length;i++) {
					if(textFuelTypeID.equals(allowedFuelTypes[i])) {
						found = true;
						break;
					}
				}
				if(!found) {
					valid = false;
					messages.add("Retrofit Error: Unsupported fuel type " + fuelTypeID);
				}

				found = false;
				String textSourceTypeID = "" + sourceTypeID;
				for(int i=0;i<allowedSourceTypes.length;i++) {
					if(textSourceTypeID.equals(allowedSourceTypes[i])) {
						found = true;
						break;
					}
				}
				if(!found) {
					valid = false;
					messages.add("Retrofit Error: Unsupported source type " + sourceTypeID);
				}
			}

			return valid;
		}
	}

	/** GUI for this instance **/
	OnRoadRetrofitPanel gui = null;

	/** True if the retrofit data should be applied **/
	boolean useParameters = false;
	/** Original data source file name with **/
	String dataSourceFileName = null;
	/** Type of the original data source file **/
	String dataSourceFileType = null;
	/** Worksheet, if applicable, within the original data source file **/
	String dataSourceWorksheetName = null;

	/** messages, warnings, and errors **/
	ArrayList<String> messages = new ArrayList<String>();
	/** Line objects **/
	ArrayList<Line> allLines = new ArrayList<Line>();
	/** DBLine objects loaded from the onRoadRetrofit execution table **/
	ArrayList<DBLine> dbLines = new ArrayList<DBLine>();
	/** CompiledLine objects ready for SQL generation **/
	ArrayList<CompiledLine> compiledLines = new ArrayList<CompiledLine>();

	/** TSV text to use when cancelling edits **/
	String resetTSV = null;

	/**
	 * Constructor
	**/
	public OnRoadRetrofitStrategy() {
		if(Configuration.allowGUI) {
			gui = new OnRoadRetrofitPanel(this);
			gui.setName("OnRoadRetrofitPanel");
		}
	}

	/**
	 * Gets the GUI detail panel object for the user to configure details specific to the
	 * derived subclass.
	 * @return The associated GUI JPanel for the derived subclass.
	**/
	public InternalControlStrategyPanel getDetailsPanel() {
		return gui;
	}

	/** Cancel all user edits made since last save or load from XML **/
	public void cancelEdits() {
		if(gui != null) {
			gui.cancel();
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		if(!loadFromDB() || dbLines == null || dbLines.size() <= 0) {
			/*
			if(dbLines == null) {
				System.out.println("OnRoadRetrofitStrategy.subscribeToMe: dbLines is null");
			} else {
				System.out.println("OnRoadRetrofitStrategy.subscribeToMe: dbLines.size()=" + dbLines.size());
			}
			*/
			return;
		}

		if(!compile(false)) {
			return;
		}

		TreeSet<Integer> years = ExecutionRunSpec.theExecutionRunSpec.years;
		for(Iterator<Integer> yi=years.iterator();yi.hasNext();) {
			int year = yi.next().intValue();
			for(Iterator<CompiledLine> i=compiledLines.iterator();i.hasNext();) {
				CompiledLine cl = (CompiledLine)i.next();
				if(cl.didAddSQL) {
					continue;
				}
				if((cl.maxCalendarYear == 0 && year == cl.calendarYear)
						|| (year >= cl.calendarYear && year <= cl.maxCalendarYear)) {
					cl.buildSQL(true);
				}
			}
		}
	}

	/**
	 * MasterLoopable override called during each relevant iteration of the MasterLoop.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void executeLoop(MasterLoopContext context) throws InterruptedException {
		// Nothing to do here, it was all done in subscribeToMe()
	}

	/**
	 * MasterLoopable override that cleans up all the data created within the
	 * executeLoop() method.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void cleanDataLoop(MasterLoopContext context) throws InterruptedException {
		// Nothing to do here, it was all done in subscribeToMe()
	}

	/**
	 * Creates XML representing this instance
	 * @return XML, ready to be inserted into a larger XML file under a suitable root
	**/
	public String getXML() {
		return null;
	}

	/**
	 * Creates Tab-Separated-Value text representing this instance.
	 * @return text, ready to be inserted into a text file.  null if only XML is supported.
	**/
	public String getTSV() {
		final String eol = System.getProperty("line.separator");
		StringBuffer result = new StringBuffer(200000);
		result.append("useParameters\t" + (useParameters?"Yes":"No") + eol);
		result.append("dataSourceFileName\t" + StringUtilities.safeGetString(dataSourceFileName) + eol);
		result.append("dataSourceFileType\t" + StringUtilities.safeGetString(dataSourceFileType) + eol);
		result.append("dataSourceWorksheetName\t" + StringUtilities.safeGetString(dataSourceWorksheetName) + eol);

		String[] parts = new String[10];
		int j;
		String t;
		for(Iterator<Line> i=allLines.iterator();i.hasNext();) {
			Line l = (Line)i.next();
			l.fillArray(parts);
			t = parts[0];
			for(j=1;j<parts.length;j++) {
				t += "\t";
				t += parts[j];
			}
			result.append(t + eol);
		}
		return result.toString();
	}

	/**
	 * Read XML into this instance
	 * @param className name of the class used to instantiate this object
	 * @param root XML object under which the XML returned by getXML() has been stored
	 * @return true if the XML could be read successfully
	**/
	public boolean acceptXML(String className,Node root) {
		return false;
	}

	/**
	 * Read Tab-Separated-Values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param text tab-separated-values as returned by getTSV()
	 * @return true if the text could be read successfully
	**/
	public boolean acceptTSV(String className,String text) {
		resetTSV = text;

		useParameters = false;
		dataSourceFileName = null;
		dataSourceFileType = null;
		dataSourceWorksheetName = null;

		LineNumberReader reader = null;
		try {
			reader = new LineNumberReader(new StringReader(text),2*65536);
			for(int i=0;i<4;i++) {
				String line = reader.readLine();
				if(line == null || line.length() <= 0) {
					break;
				}
				ArrayList<String> parts = StringUtilities.splitCSVremovingQuotes(line,'\t');
				if(parts == null || parts.size() <= 0) {
					break;
				}
				String name = (String)parts.get(0);
				String value = null;
				if(parts.size() > 1) {
					value = (String)parts.get(1);
				}
				if(value == null) {
					value = "";
				}
				if(name.equalsIgnoreCase("useParameters")) {
					useParameters = value.equalsIgnoreCase("Yes");
				} else if(name.equalsIgnoreCase("dataSourceFileName")) {
					dataSourceFileName = value;
				} else if(name.equalsIgnoreCase("dataSourceFileType")) {
					dataSourceFileType = value;
				} else if(name.equalsIgnoreCase("dataSourceWorksheetName")) {
					dataSourceWorksheetName = value;
				}
			}
			return load(reader,'\t',false);
		} catch(Exception e) {
			Logger.logError(e,"Unable to accept TSV in OnRoadRetrofitStrategy");
			return false;
		} finally {
			if(gui != null) {
				gui.populateControls();
				gui.revalidate();
			}
		}
	}

	/**
	 * Process a user's request to initiate an import operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was imported.
	 * false if the import request should be handled by default logic instead.
	**/
	public boolean doImport(Frame ownerWindow) {
		if(gui != null) {
			return gui.doImport(ownerWindow);
		} else {
			return false;
		}
	}

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	public boolean doExport(Frame ownerWindow) {
		if(gui != null) {
			return gui.doExport(ownerWindow);
		} else {
			return false;
		}
	}

	/**
	 * Load from the execution database's onRoadRetrofit table.
	 * @return true if the data was loaded, even if no records were found.
	**/
	boolean loadFromDB() {
		messages.clear();
		dbLines.clear();
		compiledLines.clear();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "select * from onRoadRetrofit";
		Connection db = null;
		ArrayList<String> messages = new ArrayList<String>();
		try {
			boolean result = true;
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			query.open(db,sql);
			while(query.rs.next()) {
				DBLine dl = new DBLine();
				if(!dl.load(query.rs,messages)) {
					result = false;
				}
				dbLines.add(dl);
			}
			return result;
		} catch(Exception e) {
			Logger.logError(e,"Unable to load from onRoadRetrofit table using: " + sql);
			return false;
		} finally {
			query.onFinally();
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
			for(Iterator<String> i=messages.iterator();i.hasNext();) {
				String m = i.next();
				Logger.log(LogMessageCategory.ERROR,m);
			}
		}
	}

	/**
	 * Load a data file.  The entire reading process is encapsulated including error/warning
	 * generation.
	 * @param fileName full name and path of the file to be loaded
	 * @param fileType type of the file to be loaded.  Either "XLS" or "Text".
	 * @param worksheetName for XLS files, the name of the worksheet within fileName
	 * that should loaded.  May be null in which case the first worksheet is used.
	 * @return true if the file was loaded
	**/
	boolean load(String fileName, String fileType, String worksheetName) {
		messages.clear();
		allLines.clear();
		compiledLines.clear();
		if(fileName == null || fileName.length() <= 0) {
			messages.add("Error: File does not exist: " + fileName);
			return false;
		}
		boolean result = true;
		File file = new File(fileName);
		LineNumberReader reader = null;
		try {
			char separator = '?';
			if(fileType.equalsIgnoreCase("XLS")) {
				// Microsoft Excel-format file, so prompt the user to select a worksheet
				XLSReader xls = new XLSReader();
				String contents = null;
				try {
					ArrayList<String> worksheets = xls.getSheets(file);
					int sheetIndex = 0, index = 0;
					for(Iterator<String> i=worksheets.iterator();i.hasNext();index++) {
						String t = (String)i.next();
						if(t.equalsIgnoreCase(worksheetName)) {
							sheetIndex = index;
							break;
						}
					}
					contents = xls.read(file,sheetIndex);
					separator = '\t';
				} catch(Exception e) {
					messages.add("Error: Unable to load file: " + e.getMessage());
					e.printStackTrace();
					return false;
				}
				reader = new LineNumberReader(new StringReader(contents),2*65536);
			} else { // Likely a text file
				reader = new LineNumberReader(new FileReader(file),2*65536);
				separator = '?';
			}
			result = load(reader,separator,true);
			reader = null;

			return result;
		} catch(Exception e) {
			messages.add("Error: Unable to load file: " + e.getMessage());
			e.printStackTrace();
			return false;
		} finally {
			if(reader != null) {
				try {
					reader.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				reader = null;
			}
		}
	}

	/**
	 * Load from an open stream
	 * @param reader an open stream
	 * @param separator field delimiter or '?' if unknown
	 * @param expectHeaderLine true if there is a header line in the data
	 * @return true if the file was loaded successfully
	**/
	public boolean load(LineNumberReader reader, char separator, boolean expectHeaderLine) {
		allLines.clear();
		compiledLines.clear();
		boolean result = true;
		try {
			String blankLine = "";
			ArrayList<String> lines = new ArrayList<String>();
			String line;
			int commaCount = 0, tabCount = 0;
			while((line = reader.readLine()) != null) {
				// Skip blank and comment lines
				if(line.length() > 0 && !line.trim().startsWith("#")) {
					lines.add(line);
					if(separator == '?') {
						// If a valid line has a tab character in it, then tab is the separator.
						// The same cannot be said for commas because tab-separated data may
						// have a comma embedded.
						if(line.indexOf('\t') >= 0) {
							separator = '\t';
						}
					}
				} else {
					lines.add(blankLine); // will be skipped later but keeps line numbering
				}
			}
			reader.close();
			reader = null;
			if(separator == '?') { // If no separator was found so far, assume it is a comma
				separator = ',';
			}
			// Parse the lines now that we know there are no comment lines
			// and we know the separator being used.
			ArrayList parts;
			boolean didComplainAboutMissingFields = false;
			boolean foundHeaderLine = !expectHeaderLine;
			for(int i=0;i<lines.size();i++) {
				line = (String)lines.get(i);
				if(line.length() <= 0) {
					//Logger.log(LogMessageCategory.INFO,"line1[" + i + "] is blank");
					continue; // ok to skip blank lines
				}
				parts = StringUtilities.splitCSVremovingQuotes(line,separator);
				if(parts == null || parts.size() == 0) {
					//Logger.log(LogMessageCategory.INFO,"line1[" + i + "] not enough parts after split");
					continue; // ok to skip blank lines
				}
				if(parts.size() != 10) {
					if(!didComplainAboutMissingFields) {
						String t = "Error: Missing fields on line " + (i+1)
								+ ", expected 10 but got " + parts.size();
						messages.add(t);
						didComplainAboutMissingFields = true;
					}
					continue;
				}
				if(!foundHeaderLine) {
					foundHeaderLine = true;
					continue;
				}
				Line l = new Line();
				if(l.load(parts,i+1,messages)) {
					allLines.add(l);
				} else {
					result = false;
				}
				if(messages.size() >= 50) {
					messages.add("Error: Too many messages, the file is not loaded");
					break;
				}
			}

			// Check rules and add messages accordingly
			if(result) {
				result = checkRules();
			}
		} catch(Exception e) {
			messages.add("Error: Unable to load file: " + e.getMessage());
			e.printStackTrace();
			return false;
		} finally {
			if(reader != null) {
				try {
					reader.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				reader = null;
			}
		}
		return result;
	}

	/**
	 * Perform rule checks that require searching the entire set of information
	 * @return true if the objects are suitable for use
	**/
	public boolean checkRules() {
		// Check for overlapping effects on vehicles
		return compile(true);
	}

	/**
	 * Save the current information, whether valid or not, to a file.  The type of
	 * the file is inferred from the file extension.  ".XLS" extensions create
	 * an XLS file.  ".CSV" creates a comma-separated file.  All other extensions
	 * create a tab-separated file.
	 * @param file the file to be created
	 * @return true if the data was saved successfully
	**/
	public boolean save(File file) {
		if(compiledLines.size() <= 0) {
			compile(false);
		}
		int fileType = CellFile.getFileType(file);
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			return saveAsXLSForImporter(file);
		} else if(fileType == CellFile.CSV) {
			return saveAsTextForImporter(file,',');
		} else {
			return saveAsTextForImporter(file,'\t');
		}
	}

	/**
	 * Generate an XLS format file
	 * @param file the file to be created
	 * @return true if the data was saved successfully
	**/
	boolean saveAsXLS(File file) {
		CellFileWriter writer = null;
		try {
			writer = new CellFileWriter(file,"OnRoadRetrofit");
			// Write header line
			int j;
			for(j=0;j<columnTitles.length;j++) {
				writer.writeTextCell(columnTitles[j]);
			}
			writer.endRow();
			int row = 0;
			String[] parts = new String[10];
			// Write data lines
			for(Iterator<Line> i=allLines.iterator();i.hasNext();) {
				row++;
				Line l = (Line)i.next();
				l.fillArray(parts);
				for(j=0;j<parts.length;j++) {
					if(j < 4) {
						writer.writeTextCell(parts[j]);
					} else {
						try {
							double value = Double.parseDouble(parts[j]);
							writer.writeDoubleCell(value);
						} catch(Exception e) {
							writer.skipCell();
						}
					}
				}
				writer.endRow();
			}
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to save retrofit XLS/XLSX file");
			return false;
		} finally {
			if(writer != null) {
				writer.close();
				writer = null;
			}
		}
	}

	/**
	 * Generate an XLS format file suitable for importing with the OnRoadRetrofitImporter.
	 * @param file the file to be created
	 * @return true if the data was saved successfully
	**/
	boolean saveAsXLSForImporter(File file) {
		CellFileWriter writer = null;
		try {
			writer = new CellFileWriter(file,"OnRoadRetrofit");
			// Write header line
			int j;
			for(j=0;j<dbColumnTitles.length;j++) {
				writer.writeTextCell(dbColumnTitles[j]);
			}
			writer.endRow();
			int row = 0;
			// Write data lines
			for(Iterator<CompiledLine> i=compiledLines.iterator();i.hasNext();) {
				row++;
				CompiledLine cl = i.next();

				writer.writeIntCell(toInt(cl.pollutantID));
				writer.writeIntCell(toInt(cl.processID));
				writer.writeIntCell(toInt(cl.fuelTypeID));
				writer.writeIntCell(toInt(cl.sourceTypeID));
				writer.writeIntCell(cl.maxCalendarYear >= 9999? cl.calendarYear : cl.maxCalendarYear);
				writer.writeIntCell(cl.modelYear);
				writer.writeIntCell(cl.modelYearMax);
				writer.writeDoubleCell(cl.totalFractionRetrofit);
				writer.writeDoubleCell(cl.fractionEffective);

				writer.endRow();
			}
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to save retrofit XLS/XLSX file");
			return false;
		} finally {
			if(writer != null) {
				writer.close();
				writer = null;
			}
		}
	}

	/**
	 * Utility function to convert a textual ID integer into an actual int.
	 * Assumes the input text is a real database ID value, likely to be an
	 * integer and not blank.
	 * @param t ID to be converted
	 * @return the ID as an int or 0 if ID was blank, null, or non-numeric.
	**/
	static int toInt(String t) {
		try {
			return Integer.parseInt(t);
		} catch(Exception e) {
			return 0;
		}
	}

	/**
	 * Generate a text file
	 * @param file the file to be created
	 * @param separator separator character to place between columns
	 * @return true if the data was saved successfully
	**/
	boolean saveAsText(File file, char separator) {
		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new FileWriter(file));
			String[] parts = new String[10];
			int j;
			String t;
			// Write header line
			t = columnTitles[0];
			for(j=1;j<columnTitles.length;j++) {
				t += separator;
				t += columnTitles[j];
			}
			writer.println(t);
			// Write data lines
			for(Iterator<Line> i=allLines.iterator();i.hasNext();) {
				Line l = (Line)i.next();
				l.fillArray(parts);
				t = parts[0];
				for(j=1;j<parts.length;j++) {
					t += separator;
					t += parts[j];
				}
				writer.println(t);
			}
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to save retrofit text file");
			return false;
		} finally {
			if(writer != null) {
				try {
					writer.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				writer = null;
			}
		}
	}

	/**
	 * Generate a text file suitable for importing with the OnRoadRetrofitImporter.
	 * @param file the file to be created
	 * @param separator separator character to place between columns
	 * @return true if the data was saved successfully
	**/
	boolean saveAsTextForImporter(File file, char separator) {
		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new FileWriter(file));
			int j;
			String t;
			// Write header line
			t = dbColumnTitles[0];
			for(j=1;j<dbColumnTitles.length;j++) {
				t += separator;
				t += dbColumnTitles[j];
			}
			writer.println(t);
			// Write data lines
			String[] parts = new String[9];
			for(Iterator<CompiledLine> i=compiledLines.iterator();i.hasNext();) {
				CompiledLine cl = i.next();

				parts[0] = cl.pollutantID;
				parts[1] = cl.processID;
				parts[2] = cl.fuelTypeID;
				parts[3] = cl.sourceTypeID;
				parts[4] = "" + (cl.maxCalendarYear >= 9999? cl.calendarYear : cl.maxCalendarYear);
				parts[5] = "" + cl.modelYear;
				parts[6] = "" + cl.modelYearMax;
				parts[7] = "" + cl.totalFractionRetrofit;
				parts[8] = "" + cl.fractionEffective;

				t = parts[0];
				for(j=1;j<parts.length;j++) {
					t += separator;
					t += parts[j];
				}
				writer.println(t);
			}
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to save retrofit text file");
			return false;
		} finally {
			if(writer != null) {
				try {
					writer.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				writer = null;
			}
		}
	}

	class CompiledLine {
		public String pollutantID;
		public String processID;
		public String fuelTypeID;
		public String sourceTypeID;
		public int calendarYear;
		public int maxCalendarYear;
		public int modelYear;
		public int modelYearMax;

		// retrofitFactor = totalFractionRetrofit (aka. cumFractionRetrofit) * (1.0 - fractionEffective)
		public double retrofitFactor = 0.0;
		// nonRetrofitFactor = 1.0 - totalFractionRetrofit (aka. cumFractionRetrofit)
		public double nonRetrofitFactor = 1.0;

		public double totalFractionRetrofit = 0.0;
		public double fractionEffective = 0.0;

		public boolean isMerged = false;
		public boolean didAddSQL = false;

		public CompiledLine() {
		}

		public CompiledLine(CompiledLine other) {
			pollutantID = other.pollutantID;
			processID = other.processID;
			fuelTypeID = other.fuelTypeID;
			sourceTypeID = other.sourceTypeID;
			calendarYear = other.calendarYear;
			maxCalendarYear = other.maxCalendarYear;
			modelYear = other.modelYear;
			modelYearMax = modelYear;
			retrofitFactor = other.retrofitFactor;
			nonRetrofitFactor = other.nonRetrofitFactor;
			totalFractionRetrofit = other.totalFractionRetrofit;
			fractionEffective = other.fractionEffective;
		}

		public boolean matchExceptCalendarYear(CompiledLine other) {
			return pollutantID.equals(other.pollutantID)
					&& processID.equals(other.processID)
					&& fuelTypeID.equals(other.fuelTypeID)
					&& sourceTypeID.equals(other.sourceTypeID)
					&& modelYear == other.modelYear;
		}

		public boolean matchAndMerge(CompiledLine other) {
			if(!pollutantID.equals(other.pollutantID)
					|| !processID.equals(other.processID)
					|| !fuelTypeID.equals(other.fuelTypeID)
					|| !sourceTypeID.equals(other.sourceTypeID)
					|| calendarYear != other.calendarYear
					|| retrofitFactor != other.retrofitFactor
					|| nonRetrofitFactor != other.nonRetrofitFactor
					|| other.modelYear < modelYear-1
					|| other.modelYear > modelYearMax+1) {
				return false;
			}
			if(maxCalendarYear != other.maxCalendarYear) {
				return false;
			}
			modelYear = Math.min(modelYear,other.modelYear);
			modelYearMax = Math.max(modelYearMax,other.modelYear);
			return true;
		}

		public void buildSQL(boolean addToExecutionRunSpec) {
			if(maxCalendarYear == 0) {
				maxCalendarYear = calendarYear;
			}

			// Build SQL statements and add them to the current ExecutionRunSpec

			// Build the WHERE conditions
			String clauses = " where pollutantID=" + pollutantID
					+ " and processID=" + processID
					+ " and fuelTypeID=" + fuelTypeID
					+ " and sourceTypeID=" + sourceTypeID;
			if(calendarYear == maxCalendarYear) {
				clauses += " and yearID=" + calendarYear;
			} else if(maxCalendarYear >= 9999) {
				clauses += " and yearID >= " + calendarYear;
			} else {
				clauses += " and yearID >= " + calendarYear + " and yearID <= " + maxCalendarYear;
			}
			if(modelYear >= modelYearMax) {
				clauses += " and modelYearID=" + modelYear;
			} else {
				clauses += " and modelYearID>=" + modelYear
						+ " and modelYearID<=" + modelYearMax;
			}

			// Update MOVESWorkerOutput
			boolean needSQL = false;
			String sql = "update MOVESWorkerOutput"
					+ " set emissionQuant=emissionQuant*" + (retrofitFactor+nonRetrofitFactor)
					+ (CompilationFlags.DO_RATES_FIRST? ",emissionRate=emissionRate*"+ (retrofitFactor+nonRetrofitFactor) : "")
					+ clauses
					+ ";";
			if(addToExecutionRunSpec) {
				if(!didAddSQL || needSQL) {
					didAddSQL = true;
					needSQL = true;
					ExecutionRunSpec.theExecutionRunSpec.addRetrofitSQL(sql);
				}
			} else {
				Logger.log(LogMessageCategory.INFO,sql);
			}
			// Update BaseRateOutput
			if(CompilationFlags.DO_RATES_FIRST) {
				sql = "update BaseRateOutput"
						+ " set emissionRate=emissionRate*" + (retrofitFactor+nonRetrofitFactor)
						+ " , meanBaseRate=meanBaseRate*" + (retrofitFactor+nonRetrofitFactor)
						+ clauses
						+ ";";
				if(addToExecutionRunSpec) {
					if(!didAddSQL || needSQL) {
						didAddSQL = true;
						needSQL = true;
						ExecutionRunSpec.theExecutionRunSpec.addRetrofitSQL(sql);
					}
				} else {
					Logger.log(LogMessageCategory.INFO,sql);
				}
			}
		}
	}

	/**
	 * Compile the relevant retrofit rules based on a calendar year range
	 * @param allowMessages true if messages should be recorded
	 * @return true if there were no errors, false otherwise
	**/
	boolean compile(boolean allowMessages) {
		compiledLines.clear();
		TreeMap<String,CompiledLine> tempCompiledLines = new TreeMap<String,CompiledLine>();
		// Compile by pollutantID, processID, fuelTypeID, sourceTypeID, calendarYear, modelYear
		boolean result = true;
		for(Iterator<DBLine> i=dbLines.iterator();i.hasNext();) {
			DBLine dl = i.next();
			for(int m=dl.beginModelYearID;m<=dl.endModelYearID;m++) {
				String compiledKey = "" + dl.pollutantID + "|" + dl.processID + "|" + dl.fuelTypeID
						+ "|" + dl.sourceTypeID + "|" + dl.retrofitYearID + "|" + m;
				CompiledLine cl = (CompiledLine)tempCompiledLines.get(compiledKey);
				if(cl == null) {
					cl = new CompiledLine();
					cl.pollutantID = "" + dl.pollutantID;
					cl.processID = "" + dl.processID;
					cl.fuelTypeID = "" + dl.fuelTypeID;
					cl.sourceTypeID = "" + dl.sourceTypeID;
					cl.calendarYear = dl.retrofitYearID;
					cl.maxCalendarYear = 10000;
					cl.modelYear = m;
					cl.modelYearMax = m;
					tempCompiledLines.put(compiledKey,cl);
				}
				cl.totalFractionRetrofit += dl.cumFractionRetrofit;
				cl.fractionEffective += dl.retrofitEffectiveFraction;
				cl.retrofitFactor += dl.cumFractionRetrofit*(1.0-dl.retrofitEffectiveFraction);
				cl.nonRetrofitFactor -= dl.cumFractionRetrofit;
				if(cl.nonRetrofitFactor < 0) {
					if(allowMessages) {
						messages.add("Error: total affected population exceeds 100% for"
								+ " pollutant " + dl.pollutantID
								+ ", process " + dl.processID
								+ ", fuel " + dl.fuelTypeID
								+ ", source " + dl.sourceTypeID);
					}
					result = false;
				}
			}
		}
		if(compiledLines.size() <= 0) {
			for(Iterator<Line> i=allLines.iterator();i.hasNext();) {
				Line l = (Line)i.next();
				double totalFractionRetrofit = 0;
				for(int y=l.initialCalendarYear;y<=l.finalCalendarYear;y++) {
					totalFractionRetrofit += l.fleetFractionPerYear;
					for(int m=l.initialModelYear;m<=l.finalModelYear;m++) {
						String compiledKey = "" + l.pollutantID + "|" + l.processID + "|" + l.fuelTypeID
								+ "|" + l.sourceTypeID + "|" + y + "|" + m;
						CompiledLine cl = (CompiledLine)tempCompiledLines.get(compiledKey);
						if(cl == null) {
							cl = new CompiledLine();
							cl.pollutantID = l.pollutantID;
							cl.processID = l.processID;
							cl.fuelTypeID = l.fuelTypeID;
							cl.sourceTypeID = l.sourceTypeID;
							cl.calendarYear = y;
							cl.maxCalendarYear = 10000;
							cl.modelYear = m;
							cl.modelYearMax = m;
							tempCompiledLines.put(compiledKey,cl);
						}
						cl.totalFractionRetrofit += totalFractionRetrofit;
						cl.fractionEffective += l.fractionEffective;
						cl.retrofitFactor += totalFractionRetrofit*(1.0-l.fractionEffective);
						cl.nonRetrofitFactor -= totalFractionRetrofit;
						if(cl.nonRetrofitFactor < 0) {
							if(allowMessages) {
								messages.add("Error: total affected population exceeds 100% for"
										+ " pollutant " + l.pollutantCode
										+ ", process " + l.processCode
										+ ", fuel " + l.fuelCode
										+ ", source " + l.sourceCode);
							}
							result = false;
						}
					}
				}
			}
		}
		if(!result) {
			return false;
		}
		// Find calendar years that apply to each entry
		Set<String> keys = tempCompiledLines.keySet();
		for(Iterator<String> k=keys.iterator();k.hasNext();) {
			CompiledLine cl = (CompiledLine)tempCompiledLines.get((String)k.next());
			if(cl == null) {
				continue;
			}
			for(Iterator<String> k2=keys.iterator();k2.hasNext();) {
				CompiledLine cl2 = (CompiledLine)tempCompiledLines.get((String)k2.next());
				if(cl2 == null || cl2 == cl) {
					continue;
				}
				if(cl.calendarYear < cl2.calendarYear && cl.matchExceptCalendarYear(cl2)) {
					// Note: the if statement intentionally does not use "-1" on the calendar year.
					// This ensures that two adjacent items, for instance in years 2002 and 2003,
					// will be set to 2002-2002 and 2003+
					cl.maxCalendarYear = Math.min(cl.maxCalendarYear,cl2.calendarYear-1);
				}
			}
		}

		// Coalesce into as few CompiledLine entries as possible based on model year
		// similarities.  This is akin to finding vertical rectangular regions
		// of similar factors within each 2D grid of calendar year and model year, each
		// grid being keyed by pollutant, process, fuel, and source type.
		boolean didChange = true;
		while(didChange) {
			didChange = false;

			keys = tempCompiledLines.keySet();
			for(Iterator<String> k=keys.iterator();k.hasNext();) {
				CompiledLine cl = (CompiledLine)tempCompiledLines.get((String)k.next());
				if(cl == null || cl.isMerged) {
					continue;
				}
				// Find a matching entry in compiledLines
				boolean foundMatch = false;
				for(Iterator<CompiledLine> i=compiledLines.iterator();i.hasNext();) {
					CompiledLine clFinal = (CompiledLine)i.next();
					if(clFinal.matchAndMerge(cl)) {
						didChange = true;
						cl.isMerged = true;
						foundMatch = true;
						break;
					}
				}
				// If not found, create one and add it to compiledLines
				if(!foundMatch) {
					didChange = true;
					cl.isMerged = true;
					// create a new entry
					CompiledLine newCl = new CompiledLine(cl);
					compiledLines.add(newCl);
				}
			}
		}
		/*
		for(Iterator i=compiledLines.iterator();i.hasNext();) {
			CompiledLine cl = (CompiledLine)i.next();
			cl.buildSQL(false);
		}
		*/
		return result;
	}
}
