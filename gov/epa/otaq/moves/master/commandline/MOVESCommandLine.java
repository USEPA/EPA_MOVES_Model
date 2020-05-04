/**************************************************************************************************
 * @(#)MOVESCommandLine.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.commandline;



import java.io.*;
import java.sql.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;

/**
 * Class for MovesCommandLine, contains main() which invokes Moves in a "batch" mode (no UI).
 *
 * @author		Wes Faler
 * @version		2010-07-05
**/
public class MOVESCommandLine {
	/** Invokes the main application. **/
	public static void main(String[] args) {
		Configuration.allowGUI = false;

		if(!SystemConfiguration.getTheSystemConfiguration().didLoad) {
			System.exit(1);
		}

		MOVESAPI theAPI = MOVESAPI.getTheAPI();
		theAPI.runApplication(args);
		DatabaseConnectionManager.flushTables();
		/** @nonissue **/
		System.out.println("MOVESCommandLine after runApplication");
		System.out.flush();
		MOVESThread.signalAllToTerminate();
		System.exit(0);
	}
}
