/**************************************************************************************************
 * @(#)TimeSpan.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.master.runspec.OutputTimeStep;
import gov.epa.otaq.moves.master.framework.DatabaseConnectionManager;

/**
 * Specifies the time interval over which a simulation is run. A simulation can be run for  
 * particular years, particular months of a year, particular days of a week, and a range of hours
 * during a day. For example, the simulation can be run for Monday through Friday, 6:00 AM to 10:00
 * AM in July and August of the years 2000 and 2003. It cannot be run for different hours on 
 * different days, or different days on different months, or different months on different years.
 * So, for example, it is not possible to run a simulation for December of 2002 and January of 2003.
 * This can only be accomplished by running the simulation more than once.
 *
 * @author		Wesley Faler
 * @author		Bob G (Task 18 Item No 99)
 * @version		2015-06-16
**/
public class TimeSpan {
	/**
	 * Names to display for the hour names in the database.
	 * Arranged as pairs, with the first being hourName in the database
	 * and the second being the information to be displayed to users.
	 * Hours not listed here should just use the name from the database.
	**/
	private static String[] standardizedHourNames = {
		"Hour beginning at 12:00 midnight", "00:00 - 00:59",
		"Hour beginning at 1:00 AM", "01:00 - 01:59",
		"Hour beginning at 2:00 AM", "02:00 - 02:59",
		"Hour beginning at 3:00 AM", "03:00 - 03:59",
		"Hour beginning at 4:00 AM", "04:00 - 04:59",
		"Hour beginning at 5:00 AM", "05:00 - 05:59",
		"Hour beginning at 6:00 AM", "06:00 - 06:59",
		"Hour beginning at 7:00 AM", "07:00 - 07:59",
		"Hour beginning at 8:00 AM", "08:00 - 08:59",
		"Hour beginning at 9:00 AM", "09:00 - 09:59",
		"Hour beginning at 10:00 AM", "10:00 - 10:59",
		"Hour beginning at 11:00 AM", "11:00 - 11:59",
		"Hour beginning at 12:00 Noon", "12:00 - 12:59",
		"Hour beginning at 1:00 PM", "13:00 - 13:59",
		"Hour beginning at 2:00 PM", "14:00 - 14:59",
		"Hour beginning at 3:00 PM", "15:00 - 15:59",
		"Hour beginning at 4:00 PM", "16:00 - 16:59",
		"Hour beginning at 5:00 PM", "17:00 - 17:59",
		"Hour beginning at 6:00 PM", "18:00 - 18:59",
		"Hour beginning at 7:00 PM", "19:00 - 19:59",
		"Hour beginning at 8:00 PM", "20:00 - 20:59",
		"Hour beginning at 9:00 PM", "21:00 - 21:59",
		"Hour beginning at 10:00 PM", "22:00 - 22:59",
		"Hour beginning at 11:00 PM", "23:00 - 23:59"
	};

	/** Describes a single entry from monthOfAnyYear **/
	public static class Month implements Comparable {
		public int monthID;
		public String name;

		public int compareTo(Object other) {
			if(other instanceof Month) {
				return monthID - ((Month)other).monthID;
			} else {
				return 1;
			}
		}

		public String toString() {
			return name;
		}
	}

	/** Describes a single entry from dayOfAnyWeek **/
	public static class Day implements Comparable {
		public int dayID;
		public String name;
		public float noOfRealDays;

		public int compareTo(Object other) {
			if(other instanceof Day) {
				return dayID - ((Day)other).dayID;
			} else {
				return 1;
			}
		}

		public String toString() {
			return name;
		}
	}

	/** Describes a single entry from hourOfAnyDay **/
	public static class Hour implements Comparable {
		public int hourID;
		public String name;

		public Hour() {
		}
		
		public Hour(int hourIDToUse, String nameToUse) {
			hourID = hourIDToUse;
			name = nameToUse;
		}

		public void standardizeName() {
			for(int i=0;i<standardizedHourNames.length;i+=2) {
				if(name.equalsIgnoreCase(standardizedHourNames[i+0])) {
					name = standardizedHourNames[i+1];
					break;
				}
			}
		}

		public int compareTo(Object other) {
			if(other instanceof Hour) {
				return hourID - ((Hour)other).hourID;
			} else {
				return 1;
			}
		}

		public String toString() {
			return name;
		}
	}

	/** List of Month objects **/
	public static ArrayList<Month> allMonths = new ArrayList<Month>();
	/** List of Day objects **/
	public static ArrayList<Day> allDays = new ArrayList<Day>();
	/** List of Hour objects **/
	public static ArrayList<Hour> allHours = new ArrayList<Hour>();
	/** Set of Integer objects for each year in the database **/
	public static TreeSet<Integer> allYears = new TreeSet<Integer>();

	/**
	 * Check a year against the set in the database.
	 * @param yearID year to be evaluated
	 * @return true if yearID exists in the database
	**/
	public static boolean isValidYear(int yearID) {
		return allYears.contains(new Integer(yearID));
	}

	/**
	 * Get a month given its 0-based position.
	 * @param index0 0-based index
	 * @return a Month object or null if index0 is out of range
	**/
	public static Month getMonthByIndex(int index0) {
		if(index0 < 0 || index0 >= allMonths.size()) {
			return null;
		}
		return (Month)allMonths.get(index0);
	}

	/**
	 * Get a day given its 0-based position.
	 * @param index0 0-based index
	 * @return a Day object or null if index0 is out of range
	**/
	public static Day getDayByIndex(int index0) {
		if(index0 < 0 || index0 >= allDays.size()) {
			return null;
		}
		return (Day)allDays.get(index0);
	}

	/**
	 * Get an hour given its 0-based position.
	 * @param index0 0-based index
	 * @return an Hour object or null if index0 is out of range
	**/
	public static Hour getHourByIndex(int index0) {
		if(index0 < 0 || index0 >= allHours.size()) {
			return null;
		}
		return (Hour)allHours.get(index0);
	}

	/**
	 * Get a month given its ID.
	 * @param id ID of the month
	 * @return a Month object or null if ID is unknown
	**/
	public static Month getMonthByID(int id) {
		for(Iterator<Month> i=allMonths.iterator();i.hasNext();) {
			Month m = (Month)i.next();
			if(m.monthID == id) {
				return m;
			}
		}
		return null;
	}

	/**
	 * Get a day given its ID.
	 * @param id ID of the day
	 * @return a Day object or null if ID is unknown
	**/
	public static Day getDayByID(int id) {
		for(Iterator<Day> i=allDays.iterator();i.hasNext();) {
			Day d = (Day)i.next();
			if(d.dayID == id) {
				return d;
			}
		}
		return null;
	}

	/**
	 * Get an hour given its ID.
	 * @param id ID of the hour
	 * @return an Hour object or null if ID is unknown
	**/
	public static Hour getHourByID(int id) {
		for(Iterator<Hour> i=allHours.iterator();i.hasNext();) {
			Hour h = (Hour)i.next();
			if(h.hourID == id) {
				return h;
			}
		}
		return null;
	}

	/**
	 * Return the status of the loaded time objects
	 * @return true if the time objects have been loaded
	**/
	public static boolean isLoaded() {
		return allMonths.size() > 0 && allDays.size() > 0 && allHours.size() > 0;
	}

	/** Load the cache of time objects (months, days, hours) from the default database **/
	public static void loadTimeObjects() {
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		
		try {
			Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);

			allMonths.clear();
			sql = "select monthID, monthName"
					+ " from monthOfAnyYear"
					+ " order by monthID";
			query.open(db,sql);
			while(query.rs.next()) {
				Month m = new Month();
				m.monthID = query.rs.getInt("monthID");
				m.name = query.rs.getString("monthName");
				allMonths.add(m);
			}
			query.close();

			allDays.clear();
			sql = "select dayID, dayName, noOfRealDays"
					+ " from dayOfAnyWeek"
					+ " order by dayID";
			query.open(db,sql);
			while(query.rs.next()) {
				Day d = new Day();
				d.dayID = query.rs.getInt("dayID");
				d.name = query.rs.getString("dayName");
				d.noOfRealDays = query.rs.getFloat("noOfRealDays");
				allDays.add(d);
			}
			query.close();

			allHours.clear();
			sql = "select hourID, hourName"
					+ " from hourOfAnyDay"
					+ " order by hourID";
			query.open(db,sql);
			while(query.rs.next()) {
				Hour h = new Hour();
				h.hourID = query.rs.getInt("hourID");
				h.name = query.rs.getString("hourName");
				h.standardizeName();
				allHours.add(h);
			}
			query.close();

			allYears.clear();
			sql = "select yearID from year";
			query.open(db,sql);
			while(query.rs.next()) {
				Integer y = new Integer(query.rs.getInt(1));
				allYears.add(y);
			}
			query.close();
		} catch(SQLException e) {
			query.onException(e,"Unable to load time objects",sql);
		} finally {
			query.onFinally();
		}
	}

	/** Aggregate Time Step **/
	public OutputTimeStep aggregateBy = OutputTimeStep.HOUR;
	/** The beginning hourID of the day **/
	public int beginHourID = 0;
	/** The ending hourID of the day **/
	public int endHourID = 0;
	/** Integer objects holding the years selected.**/
	public TreeSet<Integer> years = new TreeSet<Integer>();
	/** Month objects holding the months selected **/
	public TreeSet<Month> months = new TreeSet<Month>();
	/** Day objects holding the days selected **/
	public TreeSet<Day> days = new TreeSet<Day>();

	/** Default constructor **/
	public TimeSpan() {
		// Nothing to do here
	}

	/**
	 * Copy constructor.
	 * @param other another object to be copied, may be NULL.
	**/
	public TimeSpan(TimeSpan other) {
		if(other == null) {
			return;
		}
		aggregateBy = other.aggregateBy;
		beginHourID = other.beginHourID;
		endHourID = other.endHourID;
		years.addAll(other.years);
		months.addAll(other.months);
		days.addAll(other.days);
	}

	/**
	 * Checks if all hours have been selected.
	 * @return	If the timespan has all days selected
	**/
	public boolean hasAllHours() {
		if(allHours.size() <= 0) {
			return false;
		}
		return ((Hour)allHours.get(allHours.size()-1)).hourID == endHourID
				&& ((Hour)allHours.get(0)).hourID == beginHourID;
	}

	/**
	 * Checks if all days have been selected.
	 * @throws SQLException from any of java.sql operations.
	 * @return	If the timespan has all days selected
	**/
	public boolean hasAllDays(Connection db) throws SQLException {
		float totalDays = 0;
		for(Iterator<Day> i=days.iterator();i.hasNext();) {
			Day d = (Day)i.next();
			totalDays += d.noOfRealDays;
		}
		return totalDays == 7.0;
	}

	/**
	 * Checks if all months have been selected.
	 * @return	If the timespan has all months selected
	**/
	public boolean hasAllMonths() {
		return allMonths.size() > 0 && months.size() == allMonths.size();
	}

	/**
	 * Checks if at least one hour has been selected.
	 * @return	If an hour has been selected
	**/
	public boolean hasHours() {
		return (beginHourID > 0) && (endHourID > 0);
	}

	/**
	 * Checks if the endHour is not before the beginHour.
	 * @return	If an hour has been selected
	**/
	public boolean hasendHourGTEbeginHour() {
		return (endHourID>=beginHourID);
	}

	/**
	 * Checks if at least one days has been selected.
	 * @return	If a day has been selected
	**/
	public boolean hasDays() {
		return days.size() > 0;
	}

	/**
	 * Checks if at least one month has been selected.
	 * @return	If a month has been selected
	**/
	public boolean hasMonths() {
		return months.size() > 0;
	}

	/**
	 * Get the number of selected years
	 * @return the number of selected years
	**/
	public int getYearCount() {
		return years.size();
	}

	/**
	 * Get the number of selected months
	 * @return the number of selected months
	**/
	public int getMonthCount() {
		return months.size();
	}

	/**
	 * Get the number of selected days
	 * @return the number of selected days
	**/
	public int getDayCount() {
		return days.size();
	}

	/**
	 * Get the number of selected hours
	 * @return the number of selected hours
	**/
	public int getHourCount() {
		if(beginHourID <= 0 || endHourID <= 0) {
			return 0;
		}
		int count = 0;
		for(Iterator<Hour> i=allHours.iterator();i.hasNext();) {
			Hour h = (Hour)i.next();
			if(h.hourID >= beginHourID && h.hourID <= endHourID) {
				count++;
			}
		}
		return count;
	}

	/**
	 * Search the selected months for a given ID
	 * @param monthID ID to search for
	 * @return true if the ID is in the set of months to simulate
	**/
	public boolean hasMonthID(int monthID) {
		for(Iterator<Month> i=months.iterator();i.hasNext();) {
			Month m = (Month)i.next();
			if(m.monthID == monthID) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Search the selected days for a given ID
	 * @param dayID ID to search for
	 * @return true if the ID is in the set of days to simulate
	**/
	public boolean hasDayID(int dayID) {
		for(Iterator<Day> i=days.iterator();i.hasNext();) {
			Day d = (Day)i.next();
			if(d.dayID == dayID) {
				return true;
			}
		}
		return false;
	}
}
