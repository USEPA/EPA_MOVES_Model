/**************************************************************************************************
 * @(#)StringComparatorIgnoreCase.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.Comparator;

/**
 * Compares Strings ignoring case.  Useful for TreeSet and TreeMap objects keyed with
 * String objects.
 *
 * @author		Wesley Faler
 * @version		2009-04-20
**/
public class StringComparatorIgnoreCase implements Comparator<String> {
	/**
	 * Compare to String objects ignoring case.
	 * @param s1 the first String to be compared.
	 * @param s2 the second String to be compared.
	 * @return a negative integer, zero, or a positive integer as the first 
	 * argument is less than, equal to, or greater than the second.
	**/
	public int compare(String s1,String s2) {
		return s1.compareToIgnoreCase(s2);
	}
}
