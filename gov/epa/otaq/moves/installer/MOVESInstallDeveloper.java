/**************************************************************************************************
 * @(#)MOVESInstallDeveloper.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.installer;

/**
 * Main class for developer installation creation application.  
 * 
 * @author	Wesley Faler
 * @version 2009-07-18
**/
public class MOVESInstallDeveloper {
	/**
	 * Entry point
	 * @param args Command line arguments consisting of:<ul>
	 * <li>Build version, likely MOVESWindow date in YYYYMMDD format but can also
	 * be wording such as "Draft 2009"</li>
	 * <li>Date part of the database name in YYYYMMDD format</li>
	 * <li>Directory with build.xml in it.  Used to find files to modify.</li>
	 * </ul>
	**/
	public static void main(String[] args) {
		InstallationWriter writer = new InstallationWriter(args[0],args[1],args[2]);
		writer.write();
	}
}
