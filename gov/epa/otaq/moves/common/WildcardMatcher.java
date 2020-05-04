/**************************************************************************************************
 * @(#)WildcardMatcher.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;

/**
 * Match strings based on wildcards containing asterisks ('*').
 *
 * @author		Wesley Faler
 * @version		2009-04-05
**/
public class WildcardMatcher {
	/**
	 * Check a string for presence of wildcard characters that would merrit more expensive
	 * string comparisons.
	 * @param wildcardText string to be examined
	 * @return true if wildcards are present
	**/
	public static boolean hasWildcards(String wildcardText) {
		if(wildcardText == null) {
			return false;
		}
		return wildcardText.indexOf('*') >= 0;
	}

	/**
	 * Check a string to determine if it would match all values.  Such cases can be handled
	 * very efficiently and don't require pattern matching.
	 * @param wildcardText string to be examined
	 * @return true if wildcardText contains a wildcard that would match every case
	**/
	public static boolean wouldMatchEverything(String wildcardText) {
		return wildcardText != null && wildcardText.equals("*");
	}

	/**
	 * Produce a set of entries that match a wildcard pattern.
	 * @param setToTrim set of all values to be examined.  This set is not modified in any way.
	 * @param wildcardText text that may or may not contain wildcards.  Any entries in setToTrim
	 * that match wildcardText will be present in the output set.
	 * @return a set of entries from setToTrim.  If there were no matching entries, then null
	 * is returned.
	**/
	public static TreeSet filter(TreeSet setToTrim, String wildcardText) {
		TreeSet<Object> result = null;
		for(Iterator i=setToTrim.iterator();i.hasNext();) {
			Object o = (Object)i.next();
			if(o != null && doesFilterMatch(wildcardText,o.toString())) {
				if(result == null) {
					result = new TreeSet<Object>();
				}
				result.add(o);
			}
		}
		return result;
	}

	/**
	 * Checks if two strings are equal.  Asterisk '*' characters in the filter string are treated
	 * as wildcards.
	 * @param filterString '*' characters ARE treated as wildcards.
	 * @param testString '*' characters are NOT treated as wildcards.
	 * @return True if the two strings are equal.
	**/
	public static boolean doesFilterMatch(String filterString, String testString) {
		int indexA = 0;
		int indexB = 0;
		
		while (filterString.length() > indexA) {
			if(filterString.charAt(indexA) == '*') {
				for (; indexB <= testString.length(); indexB++) {
					if(doesFilterMatch(filterString.substring(indexA + 1),
							testString.substring(indexB))) {
						return true;
					}
				}
				
				return false;
			} else {
				if(testString.length() <= indexB) {
					return false;
				}
				
				if (Character.toUpperCase(filterString.charAt(indexA)) ==
						Character.toUpperCase(testString.charAt(indexB))) {
					indexA++;
					indexB++;
				} else {
					return false;
				}
			}
		}

		return (testString.length() <= indexB);
	}
}
