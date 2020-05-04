/**************************************************************************************************
 * @(#)StringUtilities.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;

/**
 * Static class providing several general purpose methods for working with strings.
 *
 * @author		Wesley Faler
 * @version		2013-11-29
**/
public class StringUtilities {
	/**
	 * Wraps a string at the specified length, with limited handling of embedded '\n' characters.
	 * @param inString The string to wrap.
	 * @param maxLineWidth The position to wrap at.
	 * @return The wrapped string.
	**/
	public static String wrapString(String inString, int maxLineWidth) {
		char[] bigText = inString.toCharArray();
		StringBuffer outBuffer = new StringBuffer();

		int wordStart = 0, wordLength = 0;
		int x = 0;
		int i = 0;
		int iMax = bigText.length;
		boolean foundNewline;
		boolean didReachEnd = false;
		try {
			while(true) {
				foundNewline = false;
				// Get next word
				for(i=wordStart;i<iMax;i++) {
					wordLength++;
					if(bigText[i] == '\n') {
						foundNewline = true;
						break;
					} else if(bigText[i] == ' ' || bigText[i] == '-') {
						break;
					}
				}
				// wordLength includes the separator character
				if(wordLength == 0) {
					break;
				}
				if(i == iMax) {
					didReachEnd = true;
				}
				if(wordLength <= maxLineWidth - x) {
					// All of the word and the separator will fit on the line
					outBuffer.append(bigText, wordStart, wordLength);
					x += wordLength;
					if((x >= maxLineWidth) && !foundNewline && !didReachEnd) {
						x = 0;
						outBuffer.append('\n');
					}
				} else if(wordLength == maxLineWidth - x + 1 && bigText[i] == ' ') {
					// Word ends with a space and without the space it would just fit
					outBuffer.append(bigText, wordStart, wordLength-1);
					x += wordLength - 1;
					if((x >= maxLineWidth) && !didReachEnd) {
						x = 0;
						outBuffer.append('\n');
					}
				} else if(wordLength == maxLineWidth - x + 1 && bigText[i] == '\n') {
					// Word ends with a newline and without the newline it would just fit,
					// so output the word and the newline
					outBuffer.append(bigText, wordStart, wordLength);
				} else if(wordLength >= maxLineWidth) {
					// Word is too long anyway, so split it as many times as necessary
					if(x > 0) { // if current line has character(s)
						x = 0;
						outBuffer.append('\n');
					}
					i = 0;
					while(i < wordLength) {
						for(;x<maxLineWidth && i<wordLength;x++,i++) {
							outBuffer.append(bigText[wordStart+i]);
						}
						if(x >= maxLineWidth) {
							x = 0;
							if(bigText[wordStart+i-1] != '\n') {
								outBuffer.append('\n');
							}
						}
					}
				} else { // The word needs to be wrapped or split
					// Move to the next line since all the data would still fit
					x = 0;
					outBuffer.append('\n');
					outBuffer.append(bigText, wordStart, wordLength);
					x += wordLength;
					if((x >= maxLineWidth) && !foundNewline && !didReachEnd) {
						x = 0;
						outBuffer.append('\n');
					}
				}
				if(foundNewline) {
					x = 0;
				}
				if(didReachEnd) {
					break;
				}
				wordStart += wordLength;
				wordLength = 0;
			}
		} catch(Exception e) {
			e.printStackTrace();
			/** @nonissue **/
			System.out.println("iMax = " + iMax + ", wordStart = " + wordStart + ", wordLength = "
					+ wordLength + ", x = " + x + ", i = " + i);
			return inString;
		}
		return outBuffer.toString();
	}

	/**
	 * Ensures that null String objects are treated as empty strings.
	 * @param inString The input String object to check.
	 * @return Returns the input String if non-null, other returns an empty String object.
	**/
	public static String safeGetString(String inString) {
		if(inString != null) {
			return inString;
		}
		return new String("");
	}

	/**
	 * Safely calls Object.toString().
	 * @param inObject The input Object to check.
	 * @return Returns Object.toString() if non-null, other returns an empty String object.
	**/
	public static String safeGetString(Object inObject) {
		if(inObject != null) {
			return inObject.toString();
		}
		return new String("");
	}

	/**
	 * Compare two Strings, either of which may be null.
	 * @param left leftmost string to compare
	 * @param right rightmost string to compare
	 * @return Results of String.compareTo equivalent to left.compareTo(right)
	**/
	public static int compare(String left, String right) {
		return safeGetString(left).compareTo(safeGetString(right));
	}

	/**
	 * Gets a substring starting at a given index, tolerating null inputs and out of bound
	 * conditions.
	 * @param text text to be examined
	 * @param beginIndex 0-based index of the first character to be retrieved
	 * @return text.substring(beginIndex) unless text is null or beginIndex would be out
	 * of range, in which case an empty non-null String is returned.
	**/
	public static String substring(String text, int beginIndex) {
		if(text == null || beginIndex > text.length()) {
			return "";
		}
		return text.substring(beginIndex);
	}

	/**
	 * Gets a substring starting at a given index, tolerating null inputs and out of bound
	 * conditions.
	 * @param text text to be examined
	 * @param beginIndex 0-based index of the first character to be retrieved
	 * @return text.substring(beginIndex) unless text is null or beginIndex would be out
	 * of range, in which case an empty non-null String is returned.
	**/
	public static String substring(String text, int beginIndex, int endIndex) {
		if(text == null || beginIndex > text.length()) {
			return "";
		}
		if(endIndex >= text.length()) {
			return text.substring(beginIndex);
		}
		return text.substring(beginIndex,endIndex);
	}

	/**
	 * Helper function to determine if a String is all whitespace.  Used since String.trim()
	 * doesn't seem to trim out CRLF chars as the docs indicate.
	 * @param test The String to trim whitespace off of.
	 * @return true if the specified string is whitespace only.
	**/
	public static boolean isWhitespace(String test) {
		for(int i = 0; i < test.length(); i++) {
			if(!Character.isWhitespace(test.charAt(i))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Create a string that has leading zeros, such as those used with dates for months
	 * and day ("2003-04-26").
	 * @param value value to represent as a string
	 * @param howManyDigits the maximum number of characters to be returned
	 * @return string that is howManyDigits long and filled with zeros on the left side
	**/
	public static String getLeadingZeroPaddedString(int value,int howManyDigits) {
		String result = "0000000";
		while(result.length() < howManyDigits) {
			result += "0000000";
		}
		result += value;
		return result.substring(result.length()-howManyDigits);
	}

	/** Utility string used for numeric padding **/
	static final String zeroPadding = "00000000000000000000"; // 20 zeros
						    		 //12345678901234567890
				    				 //         1         2

	/** Utility string used for space padding **/
	static final String spacePadding = "                    "; // 20 spaces
						    		  //12345678901234567890
				    				  //         1         2

	/**
	 * Place leading zeros on a number, making a specific field width
	 * @param number value to be converted and left padded with zeros
	 * @param length length to extend or clip the number and its zeros to
	 * @return text padded with zeros or clipped to be exactly length characters
	**/
	public static String leftZeroPad(int number, int length) {
		String text = "" + number;
		if(text.length() == length) {
			return text;
		} else if(text.length() < length) {
			String result = text;
			while(result.length() < length) {
				int lengthToAdd = length - result.length();
				if(lengthToAdd >= zeroPadding.length()) {
					result = zeroPadding + result;
				} else {
					result = zeroPadding.substring(0,lengthToAdd) + result;
					break;
				}
			}
			return result;
		} else {
			return text.substring(0,length);
		}
	}

	/**
	 * Place leading spaces on a number, making a specific field width
	 * @param number value to be converted and left padded with spaces
	 * @param length length to extend or clip the number and its spaces to
	 * @return text padded with spaces or clipped to be exactly length characters
	**/
	public static String leftSpacePad(int number, int length) {
		String text = "" + number;
		if(text.length() == length) {
			return text;
		} else if(text.length() < length) {
			String result = text;
			while(result.length() < length) {
				int lengthToAdd = length - result.length();
				if(lengthToAdd >= spacePadding.length()) {
					result = spacePadding + result;
				} else {
					result = spacePadding.substring(0,lengthToAdd) + result;
					break;
				}
			}
			return result;
		} else {
			return text.substring(0,length);
		}
	}

	/**
	 * Place trailing spaces on text, making a specific field width
	 * @param text value to be padded
	 * @param length length to extend or clip the text and its spaces to
	 * @return text padded with spaces or clipped to be exactly length characters
	**/
	public static String rightSpacePad(String text, int length) {
		if(text.length() == length) {
			return text;
		} else if(text.length() < length) {
			String result = text;
			while(result.length() < length) {
				int lengthToAdd = length - result.length();
				if(lengthToAdd >= spacePadding.length()) {
					result += spacePadding;
				} else {
					result += spacePadding.substring(0,lengthToAdd);
					break;
				}
			}
			return result;
		} else {
			return text.substring(0,length);
		}
	}

	/**
	 * Get the current date and time in a format suitable for embedding into file names.
	 * @return current date and time in format: yymmdd_hhMMss
	**/
	public static String datetimeForFileName() {
		Calendar c = Calendar.getInstance();
		return (leftZeroPad(c.get(Calendar.YEAR),4).substring(2))
				+ leftZeroPad(c.get(Calendar.MONTH)+1,2)
				+ leftZeroPad(c.get(Calendar.DAY_OF_MONTH),2)
				+ "_"
				+ leftZeroPad(c.get(Calendar.HOUR_OF_DAY),2)
				+ leftZeroPad(c.get(Calendar.MINUTE),2)
				+ leftZeroPad(c.get(Calendar.SECOND),2);
	}

	/**
	 * Replace placeholder text within a String with desired values.
	 * @param input text with replacable pieces
	 * @param replacements pairs of [original text, new text] Strings to be searched for
	 * and inserted into input, replacing the original text whereever found.
	 * @return String with placeholders replaced with their values.
	**/
	public static String doReplacements(String input,TreeMapIgnoreCase replacements) {
		String result = input;
		String lowerCaseResult = input.toLowerCase();

		boolean done = false;
		while(!done) {
			done = true;
			Set keys = replacements.keySet();
			for(Iterator k=keys.iterator();k.hasNext();) {
				String key = ((String)k.next()).toLowerCase();
				String value = (String)replacements.get(key);

				int index = lowerCaseResult.indexOf(key);
				if(index >= 0) {
					if(index > 0) {
						result = result.substring(0,index)
								+ value + result.substring(index+key.length());
						lowerCaseResult = lowerCaseResult.substring(0,index)
								+ value + lowerCaseResult.substring(index+key.length());
					} else {
						result = value + result.substring(index+key.length());
						lowerCaseResult = value + lowerCaseResult.substring(index+key.length());
					}
					done = false;
				}
			}
		}
		return result;
	}

	/**
	 * Replace all occurences of a substring within a String with a new substring.
	 * @param input text with replacable pieces
	 * @param searchFor substring to find
	 * @param replaceWith substring that should replace searchFor
	 * @return String with searchFor replaced with replaceWith
	**/
	public static String replace(String input,String searchFor,String replaceWith) {
		String result = input;
		String lowerCaseResult = input.toLowerCase();
		String lowerCaseSearchFor = searchFor.toLowerCase();

		boolean done = false;
		while(!done) {
			done = true;
			int index = lowerCaseResult.indexOf(lowerCaseSearchFor);
			if(index >= 0) {
				if(index > 0) {
					result = result.substring(0,index)
							+ replaceWith + result.substring(index+searchFor.length());
					lowerCaseResult = lowerCaseResult.substring(0,index)
							+ replaceWith + lowerCaseResult.substring(index+searchFor.length());
				} else {
					result = replaceWith + result.substring(index+searchFor.length());
					lowerCaseResult = replaceWith
							+ lowerCaseResult.substring(index+searchFor.length());
				}
				done = false;
			}
		}
		return result;
	}

	/**
	 * Find the comma-separated parts of a text string, removing embedded quotes if found.
	 * @param text comma-separated-values
	 * @param separator known delimiter, or 0 (not '0', but 0) if unknown
	 * @return list of values, may be empty but never null.
	**/
	public static ArrayList<String> splitCSVremovingQuotes(String text, char separator) {
		boolean hasData = false;
		StringBuffer builder = new StringBuffer(500);
		ArrayList<String> parts = new ArrayList<String>();
		int state = 0, quoteStartIndex = -1, dataStartIndex = 0;
		char[] chars = text.toCharArray();
		for(int i=0;i<chars.length;i++) {
			switch(state) {
				case -1:
					hasData = false;
					state = 0;
					// flow through to state=0 case
				case 0:
					switch(chars[i]) {
						case '\"':
							if(hasData) {
								builder.append(chars[i]);
							} else {
								quoteStartIndex = i;
								state = 1;
							}
							break;
						case '\'':
							if(hasData) {
								builder.append(chars[i]);
							} else {
								quoteStartIndex = i;
								state = 2;
							}
							break;
						case ',':
						case '\t':
						case '|':
							if(separator == 0) {
								separator = text.charAt(i);
							}
							if(separator == text.charAt(i)) {
								parts.add(builder.toString());
								builder.delete(0,builder.length());
								dataStartIndex = i+1;
								hasData = false;
								break;
							} // else, flow through to default case
						default:
							if(chars[i] != ' ') {
								hasData = true;
							}
							builder.append(chars[i]);
							break;
					}
					break;
				case 1:
					if(chars[i] == '\"' && hasFollowingSeparator(chars,i,separator)) {
						state = -1;
						break;
					}
					if(chars[i] == '\t' && separator == '\t') {
						parts.add(builder.toString());
						builder.delete(0,builder.length());
						dataStartIndex = i+1;
						state = -1;
						break;
					} else {
						builder.append(chars[i]);
					}
					break;
				case 2:
					if(chars[i] == '\'' && hasFollowingSeparator(chars,i,separator)) {
						state = -1;
						break;
					}
					if(chars[i] == '\t' && separator == '\t') {
						// Separator found without closing apostrophe ('),
						// so is likely name of form: O'Brian
						builder.delete(0,builder.length());
						builder.append(chars,dataStartIndex,i-dataStartIndex);
						parts.add(builder.toString());
						builder.delete(0,builder.length());
						dataStartIndex = i+1;
						state = -1;
						break;
					} else {
						builder.append(chars[i]);
					}
					break;
			}
		}
		if(builder.length() > 0) {
			parts.add(builder.toString());
		}
		return parts;
	}

	/**
	 * Determine if there is a separator character following a likely close quote in a CSV format.
	 * If there is no separator, then the quotation mark is likely not a close quote but rather part
	 * of the data.  Separators must follow either immediately or may have any amount of whitespace.
	 * @param chars CSV data being examined
	 * @param currentIndex index of the close quote
	 * @param currentSeparator the separator character to find or 0 if unknown (as will occur in the
	 * first entry on a CSV line)
	 * @return true if there is a following separator, including the end of data, false otherwise.
	**/
	private static boolean hasFollowingSeparator(char[] chars,int currentIndex,char currentSeparator) {
		for(int i=currentIndex+1;i<chars.length;i++) {
			if(chars[i] == ' ') {
				continue;
			}
			if(chars[i] == currentSeparator) {
				return true;
			}
			if(currentSeparator == 0 && (chars[i] == ',' || chars[i] == '\t')) {
				return true;
			}
			// non-whitespace, non-separator character found, therefore there is no following separator
			return false;
		}
		return true; // no more data, therefore there is a form of field separator
	}

	/**
	 * Convert text into a boolean true/false flag.
	 * @param text data to be examined, may be null or blank (though both generate a false
	 * return value).
	 * @return true if the text starts with "Y", "y", "T", "t", "1", or "-".  false otherwise.
	**/
	public static boolean stringToBoolean(String text) {
		if(text == null || text.length() <= 0) {
			return false;
		}
		char t = text.charAt(0);
		switch(t) {
			case 'Y':
			case 'y':
			case 'T':
			case 't':
			case '1':
			case '-': 	// "-1" is also used for "true" since it is an integer with all
						// bits set to 1.
				return true;
		}
		return false;
	}

	/**
	 * Check a string to ensure it is entirely digits.
	 * @param text string to be examined
	 * @return true if only digits are within the string, false otherwise include empty strings
	**/
	public static boolean isDigits(String text) {
		if(text == null) {
			return false;
		}
		text = text.trim();
		if(text.length() <= 0) {
			return false;
		}
		char[] characters = text.toCharArray();
		for(int i=0;i<characters.length;i++) {
			if(!Character.isDigit(characters[i])) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Create a comma separated list of values.
	 * @param items values to use
	 * @return comma separated list of values, empty if items is empty
	**/
	public static String getCSV(TreeSet<String> items) {
		String result = "";
		for(Iterator<String> i=items.iterator();i.hasNext();) {
			if(result.length() > 0) {
				result += ",";
			}
			result += i.next();
		}
		return result;
	}

	/**
	 * Create a comma separated list of values.
	 * @param items values to use
	 * @return comma separated list of values, empty if items is empty
	**/
	public static String getCSVGeneric(TreeSet<Object> items) {
		String result = "";
		for(Iterator<Object> i=items.iterator();i.hasNext();) {
			if(result.length() > 0) {
				result += ",";
			}
			result += i.next().toString();
		}
		return result;
	}
}
