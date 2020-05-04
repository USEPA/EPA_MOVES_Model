/**************************************************************************************************
 * @(#)SetLogin.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.commandline;

import java.io.*;
import java.sql.*;
import java.util.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;

/**
 * Apply MySQL login information to all configuration files.
 *
 * @author		Wes Faler
 * @version		2013-12-02
**/
public class SetLogin {
	/** Invokes the main application. **/
	public static void main(String[] args) {
		Configuration.allowGUI = false;

		if(!SystemConfiguration.getTheSystemConfiguration().didLoad) {
			System.exit(1);
		}

		// Find command line arguments
		String userName = null, password = null;
		for(int i=0;i<args.length;i++) {
			String t = args[i].toLowerCase();
			if(t.startsWith("-user")) {
				userName = getOptionData(args[i]);
			} else if(t.startsWith("-password")) {
				password = getOptionData(args[i]);
			}
		}
		if(userName == null) {
			System.out.println("Missing -user option");
			System.exit(1);
		}
		if(password == null) {
			System.out.println("Missing -password option");
			System.exit(1);
		}

		// Cleanup values
		userName = userName.trim();
		if(userName.equalsIgnoreCase("${user}") || userName.equalsIgnoreCase("-Dpassword")) {
			userName = "";
		}
		password = password.trim();
		if(password.equalsIgnoreCase("${password}") || password.equalsIgnoreCase("-Duser")) {
			password = "";
		}

		//System.out.println("user=" + userName);
		//System.out.println("password=" + password);

		// Apply the settings
		DatabaseSelection.userProvidedUserName = userName.trim();
		DatabaseSelection.userProvidedPassword = password.trim();

		// Save the settings
		try {
			SystemConfiguration.getTheSystemConfiguration().saveConfigurationData();
		} catch(Exception e) {
			System.out.println("ERROR: Unable to write configuration files");
			System.out.println("ERROR: " + e.getMessage());
			System.exit(1);
		}

		System.exit(0);
	}

	/**
	 * Get the data portion of an option of the form "-option=data".
	 * @param optionText full option text
	 * @return data portion of the option, never null but may be blank
	**/
	static String getOptionData(String optionText) {
		int index = optionText.indexOf('=');
		if(index < 0) {
			return "";
		}
		return StringUtilities.substring(optionText,index+1).trim();
	}
}
