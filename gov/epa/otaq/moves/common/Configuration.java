/**************************************************************************************************
 * @(#)Configuration.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

/**
 * Run-time configuration flags
 *
 * @author		Wesley Faler
 * @version		2010-07-05
**/
public class Configuration {
	/** true if GUI objects are allowed to be instantiated, false when run from a command line **/
	public static boolean allowGUI = true;

	/** Check the environment settings to force command-line mode **/
	public static void checkEnvironment() {
        String isCommandLine = java.lang.System.getenv("MOVES_COMMAND_LINE_YN");

        if((isCommandLine != null) && isCommandLine.startsWith("Y")) {
        	allowGUI = false;
        }
	}
}
