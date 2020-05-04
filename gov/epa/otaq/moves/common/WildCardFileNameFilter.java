/**************************************************************************************************
 * @(#)WildCardFileNameFilter.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.File;
import java.io.FilenameFilter;

/**
 * This class allows file name filtering with wild card strings. The search strings
 * can contain '*' wildcard characters.
 *
 * @author		Cimulus
 * @version		2003-08-21
**/
public class WildCardFileNameFilter implements FilenameFilter {
	/**
	 * The filter string. This can contain special wildcard characters: '*'
	**/
	String nameFilter;

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

	/**
	 * Standard Constructor.
	 * @param filter The filter string. This can contain special wildcard characters: '*'
	**/
	public WildCardFileNameFilter(String filter) {
		nameFilter = filter;
	}
	
	/**
	 * FilenameFilter implementation.
	 * @param directoryPath The directory of the file. (not used)
	 * @param fileName The file name to test against the pattern.
	 * @return True if the file name matches the pattern.
	**/
	public boolean accept(File directoryPath, String fileName) {
		return doesFilterMatch(nameFilter, fileName);
	}
}
