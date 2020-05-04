/**************************************************************************************************
 * @(#)RegExBuilder.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;
import java.util.regex.*;

/**
 * Create a regular expression pattern from a wildcard string.
 * Compatible codes:
 *		\sol		start of line (also accepts \bol)
 *		\eol		end of line
 *		\first		The following must be the first thing in the buffer
 *		\white		0 or more white space characters
 *		\nonwhite	0 or more non-white space characters
 *		\digit		a single digit character
 *		\digits		0 or more digit characters
 *		\x###		ASCII character given by 3 digits following \x.  
 *                  Use leading zeros to get 3 digits if needed.
 *		*			0 or more of any character
 *		\*			A single asterisk
 *		\\			A single backslash
 *		\space		A single space character
 *		(other)		A single character
 *
 * @author		Wesley Faler
 * @version		2009-04-05
**/
public class RegExBuilder {
	/**
	 * Create a regular expression pattern from a Kernel-compatible string
	 * @param text a Kernel-compatible string
	 * @param errors errors found while compiling an expression
	 * @return a regular expression
	**/
	public static String getRegEx(String text,ArrayList<String> errors) {
		String originalText = text;

		String result = "";
		int used = 0;
		while(text.length() > 0) {
			if(text.startsWith("\\sol") || text.startsWith("\\bol")) {
				result += "^";
				used = 4;
			} else if(text.startsWith("\\first")) {
				result += "\\A";
				used = 6;
			} else if(text.startsWith("\\space")) {
				result += "\\x20";
				used = 6;
			} else if(text.startsWith("\\eol")) {
				result += "$(\\r\\n|\\n\\r|\\r|\\n)";
				used = 4;
			} else if(text.startsWith("\\space")) {
				result += "\\x20";
				used = 6;
			} else if(text.startsWith("\\white")) {
				result += "\\s*";
				used = 6;
			} else if(text.startsWith("\\nonwhite")) {
				result += "\\S*";
				used = 9;
			} else if(text.startsWith("*")) {
				result += ".*?";
				used = 1;
			} else if(text.startsWith("\\*")) {
				result += "\\*";
				used = 2;
			} else if(text.startsWith("\\\\")) {
				result += "\\\\";
				used = 2;
			} else if(text.startsWith("\\x")) {
				if(text.length() >= 5) {
					try {
						int code = Integer.parseInt(text.substring(2,5));
						if(code >= 0 && code <= 255) {
							used = 5;
							String hex = "00" + Integer.toHexString(code);
							result += "\\x" + hex.substring(hex.length()-2,hex.length());
						} else if(errors != null) {
							errors.add("\\x found with value " + code + " but should be >= 0 and <= 255");
						}
					} catch(Exception e) {
						if(errors != null) {
							errors.add("\\x found without 3 following digits");
						}
					}
				} else {
					if(errors != null) {
						errors.add("\\x found without 3 following digits");
					}
					used = 1;
				}
			} else if(text.startsWith("\\digits")) {
				result += "\\d*";
				used = 7;
			} else if(text.startsWith("\\digit")) {
				result += "\\d";
				used = 6;
			} else {
				String hex = "00" + Integer.toHexString((int)(text.substring(0,1).toCharArray()[0]));
				result += "\\x" + hex.substring(hex.length()-2,hex.length());
				used = 1;
			}
			text = text.substring(used);
		}
		/*
		if(errors != null && errors.size() > 0) {
			for(Iterator i=errors.iterator();i.hasNext();) {
				System .out.println(i.next().toString());
			}
		}
		*/
		//System.out.println("RegExBuilder.getRegEx(\"" + originalText + "\") = \"" + result + "\"");
		return result;
	}

	/**
	 * Create a compiled Pattern object
	 * @param regularExpression expression to be compiled
	 * @return a compiled regular expression
	**/
	public static Pattern compile(String regularExpression) {
		Pattern result = Pattern.compile(regularExpression,
				Pattern.MULTILINE | Pattern.DOTALL | Pattern.CASE_INSENSITIVE);
		return result;
	}
}
