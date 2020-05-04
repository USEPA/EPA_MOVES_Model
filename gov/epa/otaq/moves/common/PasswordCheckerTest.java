/**************************************************************************************************
 * @(#)PasswordCheckerTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import junit.framework.*;

/**
 * Test Case for the PasswordChecker class
 *
 * @author		Wesley Faler
 * @version		2013-11-17
**/
public class PasswordCheckerTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public PasswordCheckerTest(String name) {
		super(name);
	}

	/**
	 * Tests encode and decode
	**/
	public void testEncodeAndDecode() {
		String userName = "testuser";
		String password = "moves";

		String encodedPassword = PasswordChecker.encode(userName,password);
		System.out.println("encode(" + userName + "," + password + ")=" + encodedPassword);
		String recoveredPassword = PasswordChecker.decode(userName,encodedPassword);
		assertEquals("Did not recover password",password,recoveredPassword);

		recoveredPassword = PasswordChecker.decode(userName,"1448F5E74F43DD44A6A7EC6F50752542D419D3936A3E5C7F6063");
		assertNull("Corrupt password is not null",recoveredPassword);

		recoveredPassword = PasswordChecker.decode(userName + "AAA",encodedPassword);
		assertNull("Corrupt user name not detected",recoveredPassword);

		// Test anonymous user
		userName = "";
		password = "";
		encodedPassword = PasswordChecker.encode(userName,password);
		System.out.println("encode(" + userName + "," + password + ")=" + encodedPassword);
		recoveredPassword = PasswordChecker.decode(userName,encodedPassword);
		assertEquals("Did not recover anonymous password",password,recoveredPassword);

		recoveredPassword = PasswordChecker.decode(userName,"113F4DF25E5C55EA70DC4253B84FC329405DC73564");
		assertNull("Corrupt anonymous password is not null",recoveredPassword);

		recoveredPassword = PasswordChecker.decode(userName + "AAA",encodedPassword);
		assertNull("Corrupt anonymous user name not detected",recoveredPassword);
	}
}
