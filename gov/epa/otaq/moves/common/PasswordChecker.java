/**************************************************************************************************
 * @(#)PasswordChecker.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import javax.xml.bind.DatatypeConverter;
import java.security.MessageDigest;
import java.nio.charset.Charset;

/**
 * Encode and decode user-supplied passwords.
 *
 * @author		Wesley Faler
 * @version		2013-11-17
**/
public class PasswordChecker {
	/**
	 * Encode a password.
	 * @param userName account that will use the password
	 * @param password text to be encoded, may be blank
	 * @return text of the encoded password, never blank, never null
	**/
	public static String encode(String userName, String password) {
		// The final format is:
		// <encoded password><hash>
		// The hash is made from the encoded password and salted user name.
		// To recover, take last 32 characters from the combination and call them the hash. Use the remaining as the encoded password.
		String encodedPassword = getHex("MO" + password + "VES");
		String hash = makeHash(userName,encodedPassword);
		return shuffle(encodedPassword + hash,true);
	}

	/**
	 * Recover a previously encoded password.
	 * @param userName account that will use the password
	 * @param encodedPassword password previously encoded with the encode() function
	 * @return the password or null if the encoded password is corrupt
	**/
	public static String decode(String userName, String encodedPassword) {
		if(encodedPassword == null || encodedPassword.length() <= 32) {
			return null;
		}
		String fullText = shuffle(encodedPassword,false);
		String hash = fullText.substring(fullText.length()-32);
		String hexPassword = fullText.substring(0,fullText.length()-32);
		String checkHash = makeHash(userName,hexPassword);
		if(!checkHash.equals(hash)) {
			return null;
		}
		String password = fromHex(hexPassword);
		if(!password.startsWith("MO") || !password.endsWith("VES")) {
			return null;
		}
		if(password.length() == 5) {
			return "";
		}
		return password.substring(2,password.length()-3);
	}

	/**
	 * Shuffle the characters within a text string.
	 * @param text text to be altered
	 * @param isForward true to make altered text, false to recover original text
	 * @return shuffled text
	**/
	public static String shuffle(String text, boolean isForward) {
		// Based upon ideas from:
		// http://www.codinghorror.com/blog/2007/12/the-danger-of-naivete.html
		// http://stackoverflow.com/questions/2459264/why-is-fisher-yates-the-most-useful-shuffling-algorithm
		if(text == null || text.length() <= 1) {
			return text;
		}
		char[] c = text.toCharArray();
		// Get shuffle exchanges
		int[] exchanges = new int[c.length-1];
		long state = c.length + 19;
		long range = 1 << 31;
		for(int i=c.length-1;i>0;i--) {
			int n = (int)(state % (i+1));
			// Advance state using the glibc/GCC random number algorithm
			// http://en.wikipedia.org/wiki/Linear_congruential_generator
			state = (state*1103515245l+12345l) % range;
			exchanges[c.length-1-i] = n;
		}
		if(isForward) { // Shuffle
	        for(int i=c.length-1;i>0;i--) {
	        	int n = exchanges[c.length-1-i];
	        	char t = c[i];
	        	c[i] = c[n];
	        	c[n] = t;
	        }
		} else { // Deshuffle
        	for(int i=1;i<c.length;i++) {
        		int n = exchanges[c.length-1-i];
        		char t = c[i];
        		c[i] = c[n];
        		c[n] = t;
        	}
		}
		return new String(c);
	}

	/**
	 * Perform a one-way hash.
	 * @param userName input user name, salted in case the name is blank
	 * @param encodedPassword input encoded password, never blank or null
	 * @return hash text, always 32 characters.
	**/
	private static String makeHash(String userName, String encodedPassword) {
		String rawHash = makeHashCore(userName,encodedPassword);
		if(rawHash.length() > 32) {
			return rawHash.substring(0,32);
		}
		while(rawHash.length() < 32) {
			rawHash = "0" + rawHash;
		}
		return rawHash;
	}

	/**
	 * Perform a one-way hash.
	 * @param userName input user name, salted in case the name is blank
	 * @param encodedPassword input encoded password, never blank or null
	 * @return hash text
	**/
	private static String makeHashCore(String userName, String encodedPassword) {
		if(encodedPassword == null || encodedPassword.length() <= 0) {
			return "";
		}
		// Salt the user name
		String t = "epa" + userName + "MOVES";
		for(int i=0;i<3;i++) {
			t = t + t.substring(i,5+i);
		}
		// Add the encoded password
		t += encodedPassword;
		// Make hex string from a MD5 hash of the UTF-8 text
		byte[] tBytes = t.getBytes(Charset.forName("UTF-8"));
		try {
	        MessageDigest digest = MessageDigest.getInstance("MD5");
	        digest.update(tBytes,0,tBytes.length);
	        byte[] hashBytes = digest.digest();
	        return DatatypeConverter.printHexBinary(hashBytes);
		} catch(Exception e) {
			return DatatypeConverter.printHexBinary(tBytes);
		}
	}

	/**
	 * Get hex representation of the text within a string.
	 * @param text text to be converted
	 * @return hexadecimal version of the UTF-8 bytes that make the string
	**/
	private static String getHex(String text) {
		byte[] tBytes = text.getBytes(Charset.forName("UTF-8"));
		return DatatypeConverter.printHexBinary(tBytes);
	}

	/**
	 * Get a string from the hex representation of its characters.
	 * @param hex hexadecimal version of the UTF-8 bytes that make the string
	 * @return the text or blank if something went wrong, never null
	**/	
	private static String fromHex(String hex) {
		try {
			byte[] tBytes = DatatypeConverter.parseHexBinary(hex);
			if(tBytes == null) {
				return "";
			}
			return new String(tBytes,Charset.forName("UTF-8"));
		} catch(Exception e) {
			return "";
		}
	}
}
