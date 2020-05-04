/**************************************************************************************************
 * @(#)InstallationConfig.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.installer;

/**
 * Sets up and modifies MOVES configuration file.  
 * Will be executed during the MOVES installation package
 *
 * @author	Cimulus
 * @version 2006-07-10
**/
public class InstallationConfig {
	/**
	 * Entry point
	 * @param args Command line arguments consisting of:<ul>
	 * <li>path of MySQL data directory</li>
	 * <li>Location of worker configuration file</li>
	 * <li>Location of MOVES configuration file</li>
	 * <li>install directory</li>
	 * </ul>
	**/
	public static void main(String[] args) {
		// check if any arguments were passed to the appplication
		if(args.length <= 0) {
			System.err.println("Error: No arguments passed.");
			return;
		}
		ConfigurationWriter writer = new ConfigurationWriter(args[0], args[1], args[2], args[3]);
		writer.write();
	}
}
