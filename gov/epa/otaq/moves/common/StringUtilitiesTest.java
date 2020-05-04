/**************************************************************************************************
 * @(#)StringUtilitiesTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import junit.framework.*;
import java.util.TreeMap;

/**
 * Test Case for the StringUtilities class
 *
 * @author		wfaler
 * @version		2008-12-10
**/
public class StringUtilitiesTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public StringUtilitiesTest(String name) {
		super(name);
	}

	/** Tests the wrapString function. **/
	public void testWrapString() {
		String wrappedString;

		wrappedString = StringUtilities.wrapString(
				"abcde fghi jkl mn o pqrstuvwxyz1234567890 !@#", 5);
		assertTrue("Test 1 (with no dashes and no embedded newlines) failed.",
				wrappedString.compareTo(
				"abcde\nfghi \njkl \nmn o \npqrst\nuvwxy\nz1234\n56789\n0 !@#") == 0);

		wrappedString = StringUtilities.wrapString(
				"abc-de fghi jkl mn o pqrstuvw-xyz1234567890 !@#", 5);
		assertTrue("Test 2 (with dashes and no embedded newlines) failed.",
				wrappedString.compareTo("abc-\nde \nfghi \njkl \nmn o "
				+ "\npqrst\nuvw-\nxyz12\n34567\n890 \n!@#") == 0);

		wrappedString = StringUtilities.wrapString("abc\nde fghijklmnop", 5);
		assertTrue("Test 3 (with no dashes and one embedded newline)  failed.",
				wrappedString.compareTo("abc\nde \nfghij\nklmno\np") == 0);

		wrappedString = StringUtilities.wrapString("abcde\nf\ng\nhijk\nlmn\nop\nq\n", 5);
		assertTrue("Test 4 (with no dashes and multiple embedded newlines) failed.",
				wrappedString.compareTo("abcde\nf\ng\nhijk\nlmn\nop\nq\n") == 0);
	}

	/** Tests the safeGetString function. **/
	public void testSafeGetString() {
		String string1 = null;
		assertTrue("Returned null, but should have returned \"\".",
				StringUtilities.safeGetString(string1) != null);
		assertTrue("Did not return \"\"."
				, StringUtilities.safeGetString(string1).compareTo("") == 0);

		String string2 = "ABcd";
		assertTrue("Return value did not match in string, even though in string was non-null.",
				StringUtilities.safeGetString(string2) == "ABcd");
		assertTrue("Return value did not match in string, even though in string was non-null.",
				StringUtilities.safeGetString("EFG") == "EFG");
		assertTrue("Return value did not match in string, even though in string was non-null.",
				StringUtilities.safeGetString("EFG").compareTo("EFG") == 0);
	}

	/** Tests the safeGetString function when passing an Object as the input param. **/
	public void testSafeGetStringFromObject() {
		// Test valid object
		Integer testInteger = new Integer(42);
		String result = StringUtilities.safeGetString(testInteger);
		assertTrue(result.compareTo("42") == 0);
		// Test null object
		testInteger = null;
		result = StringUtilities.safeGetString(testInteger);
		assertTrue(result.compareTo("") == 0);
	}

	/** Tests substring() **/
	public void testSubstring() {
		assertTrue(StringUtilities.substring("abcdef",4).equals("ef"));
		assertTrue(StringUtilities.substring("abcdef",40).equals(""));
		assertTrue(StringUtilities.substring("abcdef",0,4).equals("abcd"));
		assertTrue(StringUtilities.substring("abcdef",4,5).equals("e"));
		assertTrue(StringUtilities.substring("abcdef",4,10).equals("ef"));
	}

	/** Tests isWhitespace() **/
	public void testIsWhitespace() {
		assertTrue(StringUtilities.isWhitespace(""));
		assertTrue(StringUtilities.isWhitespace(" "));
		assertTrue(StringUtilities.isWhitespace(" \t\r\n"));
		assertTrue(!StringUtilities.isWhitespace("x \t\r\n"));
		assertTrue(!StringUtilities.isWhitespace("abcdedf "));
		assertTrue(!StringUtilities.isWhitespace("\t\r\ntest"));
	}

	/** Tests getLeadingZeroPaddedString() **/
	public void testGetLeadingZeroPaddedString() {
		assertTrue(StringUtilities.getLeadingZeroPaddedString(314,3).equals("314"));
		assertTrue(StringUtilities.getLeadingZeroPaddedString(314,4).equals("0314"));
		assertTrue(StringUtilities.getLeadingZeroPaddedString(314,5).equals("00314"));
		assertTrue(StringUtilities.getLeadingZeroPaddedString(314,10).equals("0000000314"));
	}

	/** Tests doReplacements() **/
	public void testDoReplacements() {
		String input = "abc ##def## ghi ##jkl## mno";
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		replacements.put("##def##","ONE");
		replacements.put("##JKL##","TWO");
		replacements.put("NOMATCH","333");

		String expectedOutput = "abc ONE ghi TWO mno";
		String output = StringUtilities.doReplacements(input,replacements);
		assertTrue(output.equals(expectedOutput));
	}

	/** Tests replace() **/
	public void testReplace() {
		String input = "abc ##def## ghi ##jkl## mno";
		String expectedOutput = "abc ONE ghi ##jkl## mno";
		String output = StringUtilities.replace(input,"##DEF##","ONE");
		assertTrue(output.equals(expectedOutput));
	}

	/** Tests leftSpacePad **/
	public void testLeftSpacePad1() {
		int input = 3;
		String expectedOutput = "3";
		String output = StringUtilities.leftSpacePad(input,1);
		assertTrue(output.equals(expectedOutput));
	}

	/** Tests leftSpacePad **/
	public void testLeftSpacePad2() {
		int input = 3;
		String expectedOutput = " 3";
		String output = StringUtilities.leftSpacePad(input,2);
		assertTrue(output.equals(expectedOutput));
	}

	/** Tests leftSpacePad **/
	public void testLeftSpacePad3() {
		int input = 3000;
		String expectedOutput = "30";
		String output = StringUtilities.leftSpacePad(input,2);
		assertTrue(output.equals(expectedOutput));
	}

	/** Tests rightSpacePad **/
	public void testRightSpacePad1() {
		String input = "test";
		String expectedOutput = "test    ";
		String output = StringUtilities.rightSpacePad(input,8);
		assertTrue(output.equals(expectedOutput));
	}

	/** Tests rightSpacePad **/
	public void testRightSpacePad2() {
		String input = "test";
		String expectedOutput = "te";
		String output = StringUtilities.rightSpacePad(input,2);
		assertTrue(output.equals(expectedOutput));
	}

	/** Tests rightSpacePad **/
	public void testRightSpacePad3() {
		String input = "test";
		String expectedOutput = "test";
		String output = StringUtilities.rightSpacePad(input,4);
		assertTrue(output.equals(expectedOutput));
	}

	/** Tests stringToBoolean **/
	public void testStringToBoolean() {
		assertTrue("Failed stringToBoolean(T)",StringUtilities.stringToBoolean("T"));
		assertTrue("Failed stringToBoolean(True)",StringUtilities.stringToBoolean("True"));
		assertTrue("Failed stringToBoolean(Y)",StringUtilities.stringToBoolean("Y"));
		assertTrue("Failed stringToBoolean(Yes)",StringUtilities.stringToBoolean("Yes"));
		assertTrue("Failed stringToBoolean(y)",StringUtilities.stringToBoolean("y"));
		assertTrue("Failed stringToBoolean(1)",StringUtilities.stringToBoolean("1"));
		assertTrue("Failed stringToBoolean(-)",StringUtilities.stringToBoolean("-1"));

		assertTrue("Failed stringToBoolean(null)",!StringUtilities.stringToBoolean(null));
		assertTrue("Failed stringToBoolean()",!StringUtilities.stringToBoolean(""));
		assertTrue("Failed stringToBoolean( )",!StringUtilities.stringToBoolean(" "));
		assertTrue("Failed stringToBoolean(N)",!StringUtilities.stringToBoolean("N"));
		assertTrue("Failed stringToBoolean(No)",!StringUtilities.stringToBoolean("No"));
		assertTrue("Failed stringToBoolean(3)",!StringUtilities.stringToBoolean("3"));
		assertTrue("Failed stringToBoolean(0)",!StringUtilities.stringToBoolean("0"));
	}

	/** Tests isDigits **/
	public void testIsDigits() {
		assertTrue("Failed isDigits(123)",StringUtilities.isDigits("123"));
		assertTrue("Failed isDigits(ABC)",!StringUtilities.isDigits("ABC"));
		assertTrue("Failed isDigits(\"\")",!StringUtilities.isDigits(""));
		assertTrue("Failed isDigits(null)",!StringUtilities.isDigits(null));
		assertTrue("Failed isDigits(1 2 3)",!StringUtilities.isDigits("1 2 3"));
		assertTrue("Failed isDigits(123  )",StringUtilities.isDigits("123  "));
		assertTrue("Failed isDigits(  123)",StringUtilities.isDigits("  123"));
		assertTrue("Failed isDigits(  123  )",StringUtilities.isDigits("  123  "));
		assertTrue("Failed isDigits(123A)",!StringUtilities.isDigits("123A"));
	}
}
