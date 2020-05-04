/**************************************************************************************************
 * @(#)WeeksInMonthHelper.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import gov.epa.otaq.moves.common.StringUtilities;
import java.sql.*;
import java.util.*;

/**
 * Utility functions for dividing months and weeks.
 *
 * @author		Wesley Faler
 * @version		2014-07-13
**/
public class WeeksInMonthHelper {
	/** Collection keyed by Integer monthID with data of Integer days in the month. **/
	TreeMap<Integer,Integer> monthsAndDays = null;
	/** keyed by Integer day count, data is String of comma separated monthIDs **/
	TreeMap<Integer,String> dayCounts = new TreeMap<Integer,String>();
	/** keyed by Integer dayID, data is Integer number of real days **/
	TreeMap<Integer,Integer> realDaysPerDay = new TreeMap<Integer,Integer>();
	/** SQL CASE...END statement using dayID for the number of real days per dayID **/
	String noOfRealDaysClause = "";
	/** SQL CASE...END statement using dayID for the portion of week per day by dayID **/
	String portionOfWeekPerDayClause = "";

	/**
	 * Get the number of real days within a dayID, which is really a portion of a week.
	 * @param dayID identifier for the portion of the week.
	 * return number of real 24-hour days contained within the portion of the week. Never 0.
	**/
	public int getRealDays(int dayID) {
		if(!loadMonthsFromDatabase() || dayCounts.size() <= 0 || realDaysPerDay.size() <= 0) {
			return 1;
		}
		Integer realDays = realDaysPerDay.get(new Integer(dayID));
		if(realDays == null || realDays.intValue() <= 0) {
			return 1;
		}
		return realDays.intValue();
	}

	/**
	 * Get the number of weeks within a month as a function of year.
	 * It is safe to divide by this number since it will always be greater than 0.
	 * @param yearID calendar year.
	 * @param monthID month from the monthOfAnyYear table.
	 * @return the number of weeks within the month
	**/
	public double getWeeksPerMonth(int yearID, int monthID) {
		if(!loadMonthsFromDatabase() || dayCounts.size() <= 0 || realDaysPerDay.size() <= 0) {
			return 1;
		}
		Integer daysPerMonth = monthsAndDays.get(new Integer(monthID));
		if(daysPerMonth == null || daysPerMonth.intValue() <= 0) {
			return 1;
		}
		return daysPerMonth.intValue() / 7.0;
	}

	/**
	 * Generate SQL for SQL expressing the number of weeks as a function of yearID and monthID.
	 * It is safe to divide by this number since the generated SQL will never return 0.
	 * @param yearColumnName SQL column name (ideally in table.column format) that contains
	 * the yearID.
	 * @param monthColumnName SQL column name (ideally in table.column format) that contains
	 * the monthID.
	 * @return SQL expressing the number of weeks as a function of yearID and monthID
	**/
	public String getWeeksPerMonthSQLClause(String yearColumnName,String monthColumnName) {
		if(!loadMonthsFromDatabase() || dayCounts.size() <= 0 || realDaysPerDay.size() <= 0) {
			return "1";
		}
		String sql = "";
		if(dayCounts.size() == 1) {
			sql = "(" + dayCounts.firstKey() + "/7.0)";
		} else {
			sql = "(CASE ";
			Set days = dayCounts.keySet();
			for(Iterator i=days.iterator();i.hasNext();) {
				Integer noOfDays = (Integer)i.next();
				String monthIDs = (String)dayCounts.get(noOfDays);
				/*
				if(monthsAndDays.size() > 1 && noOfDays.intValue() == 28) {
					sql += " WHEN " + monthColumnName + " IN (" + monthIDs
							+ ") THEN (28+IF(MOD(" + yearColumnName + ",4)=0,1,0))/7.0"; // add 1 in leap years
				} else {
					sql += " WHEN " + monthColumnName + " IN (" + monthIDs
							+ ") THEN " + noOfDays + "/7.0";
				}
				*/
				sql += " WHEN " + monthColumnName + " IN (" + monthIDs
						+ ") THEN " + noOfDays + "/7.0";
			}
			sql += " ELSE 1 END)";
		}
		//System.out.println("***** " + sql);
		return sql;
	}

	/**
	 * Generate SQL for SQL expressing the number of days as a function of yearID, monthID, and dayID.
	 * It is safe to divide by this number since the generated SQL will never return 0.
	 * @param yearColumnName SQL column name (ideally in table.column format) that contains
	 * the yearID.
	 * @param monthColumnName SQL column name (ideally in table.column format) that contains
	 * the monthID.
	 * @param dayColumnName SQL column name (ideally in table.column format) that contains
	 * the dayID.
	 * @return SQL expressing the number of days as a function of yearID, monthID, and dayID.
	**/
	public String getDaysPerMonthSQLClause(String yearColumnName,String monthColumnName,
			String dayColumnName) {
		String weeksClause = getWeeksPerMonthSQLClause(yearColumnName,monthColumnName);
		String t = StringUtilities.replace(noOfRealDaysClause,"##dayIDColumnName##",dayColumnName);
		String sql = "(" + weeksClause + "*(" + t + "))";
		return sql;
	}

	/**
	 * Generate SQL for SQL expressing the portion of a week per day as a function of dayID.
	 * This number should be multiplied by values to convert to a classical day.
	 * @param dayColumnName SQL column name (ideally in table.column format) that contains
	 * the dayID.
	 * @return SQL expressing the number of portions of a week per day as a function of dayID.
	**/
	public String getPortionOfWeekPerDayClause(String dayColumnName) {
		if(!loadMonthsFromDatabase() || dayCounts.size() <= 0 || realDaysPerDay.size() <= 0) {
			return "1";
		}
		String t = StringUtilities.replace(portionOfWeekPerDayClause,"##dayIDColumnName##",dayColumnName);
		String sql = "(" + t + ")";
		return sql;
	}

	/**
	 * Method to get monthIDs and number of days from monthOfAnyYear.
	 * Populates the monthsAndDays, dayCounts, and realDaysPerDay member variables.
	 * @return false if there is an exception
	**/
	boolean loadMonthsFromDatabase() {
		monthsAndDays = new TreeMap<Integer,Integer>();
		dayCounts = new TreeMap<Integer,String>();
		realDaysPerDay = new TreeMap<Integer,Integer>();
		noOfRealDaysClause = "case ##dayIDColumnName##";
		portionOfWeekPerDayClause = "case ##dayIDColumnName##";

		Connection executionDatabase = null;
		String sql = "";
		ResultSet rs = null;

		try {
			executionDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.EXECUTION);
			sql = "SELECT MonthID, noOfDays FROM MonthOfAnyYear";
			rs = SQLRunner.executeQuery(executionDatabase, sql);
			while(rs.next()) {
				Integer monthID = new Integer(rs.getInt(1));
				Integer noOfDays = new Integer(rs.getInt(2));
				monthsAndDays.put(monthID,noOfDays);

				String monthIDs = (String)dayCounts.get(noOfDays);
				if(monthIDs == null) {
					monthIDs = "" + monthID;
				} else {
					monthIDs += "," + monthID;
				}
				dayCounts.put(noOfDays,monthIDs);
			}
			rs.close();
			rs = null;
			if(monthsAndDays.size() != 12 && monthsAndDays.size() != 1) {
				Logger.log(LogMessageCategory.ERROR, "MonthOfAnyYear must have all 12 months or "
						+ "one pseudo month");
				return false;
			}

			sql = "select dayID, noOfRealDays from dayOfAnyWeek";
			rs = SQLRunner.executeQuery(executionDatabase, sql);
			while(rs.next()) {
				Integer dayID = new Integer(rs.getInt(1));
				Integer noOfRealDays = new Integer(rs.getInt(2));
				realDaysPerDay.put(dayID,noOfRealDays);
				noOfRealDaysClause += " when " + dayID + " then " + noOfRealDays;
				portionOfWeekPerDayClause += " when " + dayID + " then " + (1.0/noOfRealDays.doubleValue());
			}
			rs.close();
			rs = null;
		} catch(Exception e) {
			Logger.logError(e, "Failed to get monthID from MonthOfAnyYear with: " + sql);
			return false;
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(executionDatabase != null) {
				DatabaseConnectionManager.checkInConnection(
						MOVESDatabaseType.EXECUTION, executionDatabase);
				executionDatabase = null;
			}
			noOfRealDaysClause += " else 1 end";
			portionOfWeekPerDayClause += " else 1 end";
		}
		return true;
	}
}
