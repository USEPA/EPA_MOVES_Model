/**************************************************************************************************
 * @(#)RunNonroadScriptActionHelper.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;
import gov.epa.otaq.moves.common.CellFileWriter;
import gov.epa.otaq.moves.common.FileUtilities;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.StringUtilities;
import gov.epa.otaq.moves.common.SQLRunner;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

/**
 * A helper class that generates reports from Nonroad post processing scripts
 *
 * @author  Daniel Cox 
 * @version	2014-10-17
**/
public class RunNonroadScriptActionHelper {
		
	/** constructor **/
	public RunNonroadScriptActionHelper() {
	}
	
	private static boolean hasErrors = false;
	
	/**
	 * Generates a report from the Nonroad post processing script output 
	 * @param scriptName The SQL script that was run and whose output needs to be put in the report
	 * @param saveFileName Where the report should be saved. Can accept .xls, .xlsx, and .txt
	 * @param oConn An open connection to the database
	**/
	public static boolean processScriptOutput(String scriptName, String saveFileName, Connection oConn) {
		Object writer = null;
		hasErrors = false;
		
		try {
			if (FileUtilities.getFileExtension(saveFileName, false).equalsIgnoreCase("xls")
					|| FileUtilities.getFileExtension(saveFileName, false).equalsIgnoreCase("xlsx")) {
				writer = new CellFileWriter(new File(saveFileName),"Sheet1");
			} else { 
				writer = new PrintWriter(new BufferedWriter(
						new OutputStreamWriter(new FileOutputStream(new File(saveFileName)))));
			}
			
			if (scriptName.equalsIgnoreCase("EmissionFactors_per_hphr_by_Equipment.sql")) {
				EmissionFactors_by_Equipment("EmissionFactors_per_hphr_by_Equipment", 
						"hp-hr", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_hphr_by_Equipment_and_Horsepower.sql")) {
				EmissionFactors_by_Equipment_and_Horsepower("EmissionFactors_per_hphr_by_Equipment_and_Horsepower", 
						"hp-hr", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_hphr_by_SCC.sql")) {
				EmissionFactors_by_SCC("EmissionFactors_per_hphr_by_SCC", 
						"hp-hr", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_hphr_by_SCC_and_ModelYear.sql")) {
				EmissionFactors_by_SCC_and_ModelYear("EmissionFactors_per_hphr_by_SCC_and_ModelYear", 
						"hp-hr", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_OperatingHour_by_Equipment.sql")) {
				EmissionFactors_by_Equipment("EmissionFactors_per_OperatingHour_by_Equipment", 
						"operating hour", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_OperatingHour_by_Equipment_and_Horsepower.sql")) {
				EmissionFactors_by_Equipment_and_Horsepower("EmissionFactors_per_OperatingHour_by_Equipment_and_Horsepower", 
						"operating hour", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_OperatingHour_by_SCC.sql")) {
				EmissionFactors_by_SCC("EmissionFactors_per_OperatingHour_by_SCC", 
						"operating hour", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_Vehicle_by_Equipment.sql")) {
				EmissionFactors_by_Equipment("EmissionFactors_per_Vehicle_by_Equipment", 
						"vehicle per day", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_Vehicle_by_Equipment_and_Horsepower.sql")) {
				EmissionFactors_by_Equipment_and_Horsepower("EmissionFactors_per_Vehicle_by_Equipment_and_Horsepower", 
						"vehicle per day", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("EmissionFactors_per_Vehicle_by_SCC.sql")) {
				EmissionFactors_by_SCC("EmissionFactors_per_Vehicle_by_SCC", 
						"vehicle per day", writer, oConn);
			} else if (scriptName.equalsIgnoreCase("Inventory_by_County_and_Pollutant.sql")) {
				Inventory_by_County_and_Pollutant(writer, oConn);
			} else if (scriptName.equalsIgnoreCase("Inventory_by_County_FuelType_Pollutant.sql")) {
				Inventory_by_County_FuelType_Pollutant(writer, oConn);
			} else if (scriptName.equalsIgnoreCase("Inventory_by_Equipment_Horsepower_Pollutant.sql")) {
				Inventory_by_Equipment_Horsepower_Pollutant(writer, oConn);
			} else if (scriptName.equalsIgnoreCase("Inventory_by_EquipmentType_Pollutant.sql")) {
				Inventory_by_EquipmentType_Pollutant(writer, oConn);
			} else if (scriptName.equalsIgnoreCase("Inventory_by_Sector_Horsepower_Pollutant.sql")) {
				Inventory_by_Sector_Horsepower_Pollutant(writer, oConn);
			} else if (scriptName.equalsIgnoreCase("Inventory_by_Sector_SCC_Pollutant.sql")) {
				Inventory_by_Sector_SCC_Pollutant(writer, oConn);
			} else if (scriptName.equalsIgnoreCase("Population_by_Sector_and_SCC.sql")) {
				Population_by_Sector_and_SCC(writer, oConn);
			}
		} catch (Exception e) {
			Logger.log(LogMessageCategory.ERROR,
					"Exception occurred running nonroad post-processing script: " + e);
			hasErrors = true;
		} finally {
			if (writer != null) {
				try {
					if (writer instanceof PrintWriter) {
						((PrintWriter)writer).close();
					}
					if (writer instanceof CellFileWriter) {
						((CellFileWriter)writer).close();
					}
				} catch (Exception e) {
					// Nothing can be done here
				}
			}
		}
		
		return hasErrors;
	}
		
	/**
	 * Queries the database and generates a report of emission factors by equipment type
	 * @param tableName The SQL table name that contains the output of the post processing script
	 * @param units The denominator units of the emission rate
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void EmissionFactors_by_Equipment(String tableName, String units, Object writer, Connection oConn) throws Exception {
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, yearID, monthID, dayID, stateID, countyID, equipDescription, fuelTypeID, pollutantID, processID, emissionRate, emissionRateUnits " +
					  "FROM " + tableName + " ORDER BY countyID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("State", 7);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Equipment Description", 40);
			header += StringUtilities.rightSpacePad("Fuel", 6);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("Emission Rate", 30);
			
			// Print header
			((PrintWriter)writer).println("Emission Factors in g/" + units + " listed by Equipment Type");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties = getCounties(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, yearID, monthID, dayID, stateID, countyID, equipDescription, fuelTypeID, pollutantID, processID, emissionRate " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("__________________________________________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("stateID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("equipDescription")), 40);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("fuelTypeID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionRate")), 30);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}

								if (hasErrors) {
									return;
								}
							}
						}
					}
			}
		}
	}
	
	/**
	 * Queries the database and generates a report of emission factors by equipment type and horsepower
	 * @param tableName The SQL table name that contains the output of the post processing script
	 * @param units The denominator units of the emission rate
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void EmissionFactors_by_Equipment_and_Horsepower(String tableName, String units, Object writer, Connection oConn) throws Exception {
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, yearID, monthID, dayID, stateID, countyID, equipDescription, hpID, hpBin, fuelTypeID, pollutantID, processID, emissionRate, emissionRateUnits " +
					  "FROM " + tableName + " ORDER BY countyID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("State", 7);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Equipment Description", 40);
			header += StringUtilities.rightSpacePad("hpID", 6);
			header += StringUtilities.rightSpacePad("hp Bin", 20);
			header += StringUtilities.rightSpacePad("Fuel", 6);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("Emission Rate", 30);
			
			// Print header
			((PrintWriter)writer).println("Emission Factors in g/" + units + " listed by Equipment Type and Horsepower Bin");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties = getCounties(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, yearID, monthID, dayID, stateID, countyID, equipDescription, hpID, hpBin, fuelTypeID, pollutantID, processID, emissionRate " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("______________________________________________________________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("stateID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("equipDescription")), 40);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("hpID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("hpBin")), 20);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("fuelTypeID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionRate")), 30);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}
								
								if (hasErrors) {
									return;
								}
							}
						}
					}
			}
		}
	}
	
	/**
	 * Queries the database and generates a report of emission factors by SCC
	 * @param tableName The SQL table name that contains the output of the post processing script
	 * @param units The denominator units of the emission rate
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void EmissionFactors_by_SCC(String tableName, String units, Object writer, Connection oConn) throws Exception {
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, yearID, monthID, dayID, stateID, countyID, scc, sccDescription, fuelTypeID, pollutantID, processID, emissionRate, emissionRateUnits " +
					  "FROM " + tableName + " ORDER BY countyID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("State", 7);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("SCC", 12);
			header += StringUtilities.rightSpacePad("Description", 40);
			header += StringUtilities.rightSpacePad("Fuel", 6);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("Emission Rate", 30);
			
			// Print header
			((PrintWriter)writer).println("Emission Factors in g/" + units + " listed by SCC");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, yearID, monthID, dayID, stateID, countyID, scc, sccDescription, fuelTypeID, pollutantID, processID, emissionRate " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("_______________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("stateID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("scc")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sccDescription")), 40);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("fuelTypeID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionRate")), 30);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}
								
								if (hasErrors) {
									return;
								}
							}
						}
					}
			}
		}
	}
	
	/**
	 * Queries the database and generates a report of emission factors by equipment type and horsepower
	 * @param tableName The SQL table name that contains the output of the post processing script
	 * @param units The denominator units of the emission rate
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void EmissionFactors_by_SCC_and_ModelYear(String tableName, String units, Object writer, Connection oConn) throws Exception {
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, yearID, monthID, dayID, stateID, countyID, SCC, sccDescription, fuelTypeID, hpID, hpBin, pollutantID, processID, modelYearID, emissionRate, emissionRateUnits " +
					  "FROM " + tableName + " ORDER BY countyID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("State", 7);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("SCC", 12);
			header += StringUtilities.rightSpacePad("Description", 40);
			header += StringUtilities.rightSpacePad("Fuel", 6);
			header += StringUtilities.rightSpacePad("hpID", 6);
			header += StringUtilities.rightSpacePad("hp Bin", 20);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("Model Year", 12);
			header += StringUtilities.rightSpacePad("Emission Rate", 30);
			
			// Print header
			((PrintWriter)writer).println("Emission Factors in g/" + units + " listed by SCC, Horsepower Bin, and Model Year");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, yearID, monthID, dayID, stateID, countyID, SCC, sccDescription, fuelTypeID, hpID, hpBin, pollutantID, processID, modelYearID, emissionRate " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("______________________________________________________________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("stateID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("SCC")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sccDescription")), 40);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("fuelTypeID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("hpID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("hpBin")), 20);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("modelYearID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionRate")), 30);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}
								
								if (hasErrors) {
									return;
								}
							}
						}
					}
			}
		}
	}
	
	/**
	 * Queries the database and generates an inventory report by county and pollutant
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void Inventory_by_County_and_Pollutant(Object writer, Connection oConn) throws Exception {
		String tableName = "Inventory_by_County_and_Pollutant";
		
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, emissionQuant, massUnits, timeUnits " +
					  "FROM " + tableName + " ORDER BY countyID, sectorID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Sector", 8);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("Emission Inventory", 30);
			header += StringUtilities.rightSpacePad("Units", 20);
			
			// Print header
			((PrintWriter)writer).println("Emission Inventories listed by County and Pollutant");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, sectors, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> sectors =  getSectors(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
				for (Integer sector : sectors) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, emissionQuant, massUnits, timeUnits " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (sector > 0)
									sql += " AND sectorID = " + sector.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("____________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Sector " + sector.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sectorID")), 8);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionQuant")), 30);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("massUnits"))
																				+ " per " 
																				+ StringUtilities.safeGetString(query.rs.getString("timeUnits")), 20);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}

								if (hasErrors) {
									return;
								}
							}
						}
					}
				}
			}
		}
	}
		
	/**
	 * Queries the database and generates an inventory report by county, fuel type, and pollutant
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void Inventory_by_County_FuelType_Pollutant(Object writer, Connection oConn) throws Exception {
		String tableName = "Inventory_by_County_FuelType_Pollutant";
		
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, fuelTypeID, emissionQuant, massUnits, timeUnits " +
					  "FROM " + tableName + " ORDER BY countyID, sectorID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Sector", 8);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("Fuel", 6);
			header += StringUtilities.rightSpacePad("Emission Inventory", 30);
			header += StringUtilities.rightSpacePad("Units", 20);
			
			// Print header
			((PrintWriter)writer).println("Emission Inventories listed by County, Fuel Type, and Pollutant");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, sectors, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> sectors =  getSectors(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
				for (Integer sector : sectors) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, fuelTypeID, emissionQuant, massUnits, timeUnits " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (sector > 0)
									sql += " AND sectorID = " + sector.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("________________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Sector " + sector.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sectorID")), 8);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("fuelTypeID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionQuant")), 30);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("massUnits"))
																				+ " per " 
																				+ StringUtilities.safeGetString(query.rs.getString("timeUnits")), 20);
										((PrintWriter)writer).println(result);
									}

									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}

								if (hasErrors) {
									return;
								}
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Queries the database and generates an inventory report by equipment type, horsepower, and pollutant
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void Inventory_by_Equipment_Horsepower_Pollutant(Object writer, Connection oConn) throws Exception {
		String tableName = "Inventory_by_Equipment_Horsepower_Pollutant";
		
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, description as equipment, hpID, hpBin, emissionQuant, massUnits, timeUnits " +
					  "FROM " + tableName + " ORDER BY countyID, sectorID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Sector", 8);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("Equipment Description", 40);
			header += StringUtilities.rightSpacePad("hpID", 6);
			header += StringUtilities.rightSpacePad("hp Bin", 20);
			header += StringUtilities.rightSpacePad("Emission Inventory", 30);
			header += StringUtilities.rightSpacePad("Units", 20);
			
			// Print header
			((PrintWriter)writer).println("Emission Inventories listed by Equipment Type, Horsepower, and Pollutant");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, sectors, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> sectors =  getSectors(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
				for (Integer sector : sectors) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, description, hpID, hpBin, emissionQuant, massUnits, timeUnits " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (sector > 0)
									sql += " AND sectorID = " + sector.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("________________________________________________________________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Sector " + sector.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sectorID")), 8);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("description")), 40);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("hpID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("hpBin")), 20);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionQuant")), 30);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("massUnits"))
																				+ " per " 
																				+ StringUtilities.safeGetString(query.rs.getString("timeUnits")), 20);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}

								if (hasErrors) {
									return;
								}
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Queries the database and generates an inventory report by equipment type and pollutant
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void Inventory_by_EquipmentType_Pollutant(Object writer, Connection oConn) throws Exception {
		String tableName = "Inventory_by_EquipmentType_Pollutant";
		
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, description as equipment, emissionQuant, massUnits, timeUnits " +
					  "FROM " + tableName + " ORDER BY countyID, sectorID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Sector", 8);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("Equipment Description", 40);
			header += StringUtilities.rightSpacePad("Emission Inventory", 30);
			header += StringUtilities.rightSpacePad("Units", 20);
			
			// Print header
			((PrintWriter)writer).println("Emission Inventories listed by Equipment Type and Pollutant");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, sectors, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> sectors =  getSectors(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
				for (Integer sector : sectors) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, description, emissionQuant, massUnits, timeUnits " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (sector > 0)
									sql += " AND sectorID = " + sector.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("____________________________________________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Sector " + sector.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sectorID")), 8);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("description")), 40);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionQuant")), 30);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("massUnits"))
																				+ " per " 
																				+ StringUtilities.safeGetString(query.rs.getString("timeUnits")), 20);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}

								if (hasErrors) {
									return;
								}
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Queries the database and generates an inventory report by sector, horsepower, and pollutant
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void Inventory_by_Sector_Horsepower_Pollutant(Object writer, Connection oConn) throws Exception {
		String tableName = "Inventory_by_Sector_Horsepower_Pollutant";
		
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, hpID, hpBin, emissionQuant, massUnits, timeUnits " +
					  "FROM " + tableName + " ORDER BY countyID, sectorID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Sector", 8);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("hpID", 6);
			header += StringUtilities.rightSpacePad("hp Bin", 20);
			header += StringUtilities.rightSpacePad("Emission Inventory", 30);
			header += StringUtilities.rightSpacePad("Units", 20);
			
			// Print header
			((PrintWriter)writer).println("Emission Inventories listed by Sector, Horsepower, and Pollutant");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, sectors, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> sectors =  getSectors(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
				for (Integer sector : sectors) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, hpID, hpBin, emissionQuant, massUnits, timeUnits " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (sector > 0)
									sql += " AND sectorID = " + sector.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("________________________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Sector " + sector.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sectorID")), 8);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("hpID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("hpBin")), 20);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionQuant")), 30);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("massUnits"))
																				+ " per " 
																				+ StringUtilities.safeGetString(query.rs.getString("timeUnits")), 20);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}

								if (hasErrors) {
									return;
								}
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Queries the database and generates an inventory report by sector, SCC, and pollutant
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void Inventory_by_Sector_SCC_Pollutant(Object writer, Connection oConn) throws Exception {
		String tableName = "Inventory_by_Sector_SCC_Pollutant";
		
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, SCC, emissionQuant, massUnits, timeUnits " +
					  "FROM " + tableName + " ORDER BY countyID, sectorID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Sector", 8);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("Pollutant", 11);
			header += StringUtilities.rightSpacePad("Process", 10);
			header += StringUtilities.rightSpacePad("SCC", 12);
			header += StringUtilities.rightSpacePad("Emission Inventory", 30);
			header += StringUtilities.rightSpacePad("Units", 20);
			
			// Print header
			((PrintWriter)writer).println("Emission Inventories listed by Sector, SCC, and Pollutant");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, sectors, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> sectors =  getSectors(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
				for (Integer sector : sectors) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, pollutantID, processID, SCC, emissionQuant, massUnits, timeUnits " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (sector > 0)
									sql += " AND sectorID = " + sector.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("________________________________________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Sector " + sector.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sectorID")), 8);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("pollutantID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("processID")), 10);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("SCC")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("emissionQuant")), 30);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("massUnits"))
																				+ " per " 
																				+ StringUtilities.safeGetString(query.rs.getString("timeUnits")), 20);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}

								if (hasErrors) {
									return;
								}
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Queries the database and generates an population report report by sector and SCC
	 * @param writer The output file writer (either a PrintWriter or a CellFileWriter)
	 * @param oConn An open connection to the database
	 * @throws Exception if there is an error writing to file
	**/
	private static void Population_by_Sector_and_SCC(Object writer, Connection oConn) throws Exception {
		String tableName = "Population_by_Sector_and_SCC";
		
		if (writer instanceof CellFileWriter) {
			String sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, fuelTypeID, SCC, population " +
					  "FROM " + tableName + " ORDER BY countyID, sectorID, yearID, monthID";
			((CellFileWriter)writer).writeSQLResults(oConn, sql, null);
		} else if (writer instanceof PrintWriter) {
			SQLRunner.Query query = new SQLRunner.Query();
			String sql = "";
			String result = "";
			String header = "";
			header += StringUtilities.rightSpacePad("MOVESRunID", 11);
			header += StringUtilities.rightSpacePad("County", 12);
			header += StringUtilities.rightSpacePad("Sector", 8);
			header += StringUtilities.rightSpacePad("Year", 6);
			header += StringUtilities.rightSpacePad("Month", 7);
			header += StringUtilities.rightSpacePad("Day", 5);
			header += StringUtilities.rightSpacePad("Fuel", 6);
			header += StringUtilities.rightSpacePad("SCC", 12);
			header += StringUtilities.rightSpacePad("Population", 30);
			
			// Print header
			((PrintWriter)writer).println("Populations listed by Sector and SCC");
			DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
			((PrintWriter)writer).println("Generated on " + dateFormat.format(new Date()));
			((PrintWriter)writer).println();
			
			// Get counties, sectors, calendar years, time periods -- spaces must be generated between each in the report
			ArrayList<Integer> counties =  getCounties(oConn, tableName);
			ArrayList<Integer> sectors =  getSectors(oConn, tableName);
			ArrayList<Integer> calendarYears = getCalendarYears(oConn, tableName);
			ArrayList<Integer> months = getMonths(oConn, tableName);
			ArrayList<Integer> days = getDays(oConn, tableName);
						
			for(Integer county : counties) {
				for (Integer sector : sectors) {
					for (Integer calendarYear : calendarYears) {
						for (Integer month : months) {
							for (Integer day : days) {
								sql = "SELECT MOVESRunID, countyID, sectorID, yearID, monthID, dayID, fuelTypeID, SCC, population " +
									  "FROM " + tableName + " WHERE 1";
								if (county > 0)
									sql += " AND countyID = " + county.toString();
								if (sector > 0)
									sql += " AND sectorID = " + sector.toString();
								if (calendarYear > 0)
									sql += " AND yearID = " + calendarYear.toString();
								if (month > 0)
									sql += " AND monthID = " + month.toString();
								if (day > 0)
									sql += " AND dayID = " + day.toString();
								
								((PrintWriter)writer).println("______________________________________________________________________________________________________");
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println();
								((PrintWriter)writer).println("Report for:");
								((PrintWriter)writer).println("     County " + county.toString());
								((PrintWriter)writer).println("     Sector " + sector.toString());
								((PrintWriter)writer).println("     Year " + calendarYear.toString());
								((PrintWriter)writer).println("     Month " + month.toString());
								((PrintWriter)writer).println("     Day " + day.toString());
								((PrintWriter)writer).println();							
								((PrintWriter)writer).println(header);
								
								try {
									result = "";
									query.open(oConn, sql);
								
									while(query.rs.next()) {
										result = "";
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("MOVESRunID")), 11);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("countyID")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("sectorID")), 8);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("yearID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("monthID")), 7);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("dayID")), 5);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("fuelTypeID")), 6);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("SCC")), 12);
										result += StringUtilities.rightSpacePad(StringUtilities.safeGetString(query.rs.getString("population")), 30);
										((PrintWriter)writer).println(result);
									}
		
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									((PrintWriter)writer).println();
									
									query.close();
								} catch(SQLException e) {
									query.onException(e,"Unable to ...",sql);
									hasErrors = true;
								} finally {
									query.onFinally();
								}

								if (hasErrors) {
									return;
								}
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Gets a list of counties in the output database so that each county can get its own section in the reports
	 * @param oConn An open connection to the database
	 * @param tableName The table name containing the post processing script's output
	**/
	private static ArrayList<Integer> getCounties(Connection oConn, String tableName) {
		ArrayList<Integer> counties = new ArrayList<Integer>();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		
		try {
			sql = "SELECT distinct countyID FROM " + tableName;
			query.open(oConn, sql);
			while(query.rs.next()) {
				counties.add(query.rs.getInt("countyID"));
			}
			query.close();
		} catch(SQLException e) {
			query.onException(e,"Unable to ...",sql);
		} finally {
			query.onFinally();
		}
		
		if (counties.size() == 0) {
			counties.add(-1);
		}
		
		return counties;
	}
	
	/**
	 * Gets a list of sectors in the output database so that each sector can get its own section in the reports
	 * @param oConn An open connection to the database
	 * @param tableName The table name containing the post processing script's output
	**/
	private static ArrayList<Integer> getSectors(Connection oConn, String tableName) {
		ArrayList<Integer> sectors = new ArrayList<Integer>();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		
		try {
			sql = "SELECT distinct sectorID FROM " + tableName;
			query.open(oConn, sql);
			while(query.rs.next()) {
				sectors.add(query.rs.getInt("sectorID"));
			}
			query.close();
		} catch(SQLException e) {
			query.onException(e,"Unable to ...",sql);
		} finally {
			query.onFinally();
		}
		
		if (sectors.size() == 0) {
			sectors.add(-1);
		}
				
		return sectors;
	}

	/**
	 * Gets a list of years in the output database so that each year can get its own section in the reports
	 * @param oConn An open connection to the database
	 * @param tableName The table name containing the post processing script's output
	**/
	private static ArrayList<Integer> getCalendarYears(Connection oConn, String tableName) {
		ArrayList<Integer> calendarYears = new ArrayList<Integer>();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		
		try {
			sql = "SELECT distinct yearID FROM " + tableName;
			query.open(oConn, sql);
			while(query.rs.next()) {
				calendarYears.add(query.rs.getInt("yearID"));
			}
			query.close();
		} catch(SQLException e) {
			query.onException(e,"Unable to ...",sql);
		} finally {
			query.onFinally();
		}
		
		if (calendarYears.size() == 0) {
			calendarYears.add(-1);
		}
		
		return calendarYears;
	}
	
	/**
	 * Gets a list of months in the output database so that each month can get its own section in the reports
	 * @param oConn An open connection to the database
	 * @param tableName The table name containing the post processing script's output
	**/
	private static ArrayList<Integer> getMonths(Connection oConn, String tableName) {
		ArrayList<Integer> months = new ArrayList<Integer>();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		
		try {
			sql = "SELECT distinct monthID FROM " + tableName;
			query.open(oConn, sql);
			while(query.rs.next()) {
				months.add(query.rs.getInt("monthID"));
			}
			query.close();
		} catch(SQLException e) {
			query.onException(e,"Unable to ...",sql);
		} finally {
			query.onFinally();
		}
		
		if (months.size() == 0) {
			months.add(-1);
		}
		
		return months;
	}
	
	/**
	 * Gets a list of days in the output database so that each day can get its own section in the reports
	 * @param oConn An open connection to the database
	 * @param tableName The table name containing the post processing script's output
	**/
	private static ArrayList<Integer> getDays(Connection oConn, String tableName) {
		ArrayList<Integer> days = new ArrayList<Integer>();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		
		try {
			sql = "SELECT distinct dayID FROM " + tableName;
			query.open(oConn, sql);
			while(query.rs.next()) {
				days.add(query.rs.getInt("dayID"));
			}
			query.close();
		} catch(SQLException e) {
			query.onException(e,"Unable to ...",sql);
		} finally {
			query.onFinally();
		}
		
		if (days.size() == 0) {
			days.add(-1);
		}
		
		return days;
	}
}