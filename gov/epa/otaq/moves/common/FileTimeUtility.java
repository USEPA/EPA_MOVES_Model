 /*
 * FileTimeUtility
 */
package gov.epa.otaq.moves.common;

import java.util.Calendar;
import java.util.Date;

/** 
* This class contains a static method which converts 
* a file's last modification date-time as available to Java
* to a String form suitable for display
*  @author Mitch Cumberworth 
*  @version 10/24/2005
**/

public class FileTimeUtility {
	
	/**
	 * Method to convert the file time as provided by Java's File
	 * class to a String form suitable for display, analogous to 
	 * standard string form used by MySQL (yyyy-mm-dd hh:mm:ss)
	 *
	 * Warning:  May not work on Non-Windows systems as O'Reilly instructs that 
	 *   the long type value returned by the File.lastModified() method
	 *   "should be used for comparison with other file times only and not
	 *   interpreted as any paricular time format".  On the other hand 
	 *   this method uses only non-deprecated java methods to interpret the value 
	 * 
	 * @param fileTimeAsLong the file date-time as available from File.lastModified()
	 * @return a String displaying the date and time in format yyyy-mm-dd hh:mm:ss
	**/
	
	public static String convertFileTimeToString (long fileTimeAsLong) {
		
		Date date = new Date(fileTimeAsLong);
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		
		int intMonth = (calendar.get(Calendar.MONTH));
		String mm = (new Integer(intMonth+1)).toString();
		int intDay = (calendar.get(Calendar.DAY_OF_MONTH));
		String dd = (new Integer(intDay)).toString();
		int intYear = (calendar.get(Calendar.YEAR));
		String yyyy = (new Integer (intYear)).toString();
		int intHour = (calendar.get(Calendar.HOUR_OF_DAY));
		String hh = (new Integer(intHour)).toString();
		int intMin = (calendar.get(Calendar.MINUTE));
		String min = (new Integer(intMin)).toString();
		int intSec = (calendar.get(Calendar.SECOND));
		String ss = (new Integer(intSec)).toString();
		return yyyy + "-" + mm + "-" + dd + " " + hh + ":" + min + ":" + ss;
	}
}
