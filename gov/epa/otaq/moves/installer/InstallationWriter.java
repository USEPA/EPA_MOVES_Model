/**************************************************************************************************
 * @(#)InstallationWriter.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.installer;

import java.io.*;

/**
 * Modifies existing installation package configuration files.
 *
 * @author	Wesley Faler
 * @version 2014-01-15
**/
public class InstallationWriter {
	/** Build information for human display, such as date YYYY/MM/DD **/
	private String buildForHuman;
	/** Build information for computer use, with spaces removed **/
	private String buildForComputer;
	/** Date part of the database name in YYYYMMDD format **/
	private String dbVersion;
	/** Directory with build.xml in it.  Used to find files to modify. **/
	private String buildFileDir;

	/**
	 * Constructor
	 * @param build build version, likely MOVESWindow date in YYYYMMDD format but can also
	 * be wording such as "Draft 2009"
	 * @param db Date part of the database name in YYYYMMDD format
	 * @param baseDir Directory with build.xml in it.  Used to find files to modify.
	 * @param dbDir Directory holding all databases.  Must end with a backslash.
	**/
	public InstallationWriter(String build, String db, String baseDir) {
		dbVersion = db;
		buildFileDir = baseDir;

		buildForComputer = build.replace(' ','_');
		buildForHuman = build;
		if(build.length() == 8) {
			boolean isDate = true;
			for(int i=0;i<build.length();i++) {
				if(!Character.isDigit(build.charAt(i))) {
					isDate = false;
					break;
				}
			}
			if(isDate) {
				buildForHuman = build.substring(0,4)
						+ '/' + build.substring(4,6) + '/' + build.substring(6,8);
			}
		}
	}

	/** Modifies and saves changes to the installation package configuration files. **/
	public void write() {
		try {
			// Load install.xml and MOVESWindow.java
			BufferedReader installReader = new BufferedReader (new FileReader(buildFileDir
					+ "/install.xml"));
			BufferedReader windowReader = new BufferedReader (new FileReader(buildFileDir
					+ "/gov/epa/otaq/moves/master/gui/MOVESWindow.java"));
			BufferedReader workerReader = new BufferedReader (new FileReader(buildFileDir
					+ "/gov/epa/otaq/moves/worker/gui/WorkerWindow.java"));
			String installOutput = "";
			String currentLine = installReader.readLine();
			boolean addLine = false;
			while(currentLine != null) {
				if(currentLine.indexOf("<appversion>") != -1) {
					// mark where the tag ends
					int index = currentLine.indexOf("<appversion>") + 12;
					currentLine = currentLine.substring(0, index) + buildForComputer + "</appversion>";
				} else if(currentLine.indexOf("<!-- movesdb -->") != -1 && currentLine.indexOf("<arg value='") != 1) {
					int index = currentLine.indexOf("<arg value='") + 12;
					currentLine = currentLine.substring(0,index) + "movesdb" + dbVersion + "'/> <!-- movesdb -->";
				}
				currentLine += "\r\n";
				installOutput += currentLine;
				currentLine = installReader.readLine();
			}
			installReader.close();
			// Save changes to file system
			BufferedWriter installWriter = new BufferedWriter (new FileWriter (buildFileDir +
					"/install.xml"));
			installWriter.write(installOutput);
			installWriter.close();
			// Overwrite C:\Program Files\MOVES{Build Date} in path_windows.txt
			BufferedWriter pathWriter = new BufferedWriter (new FileWriter(buildFileDir +
					"/installer/MOVESInstaller/InstallationPackage/path_windows.txt"));
			pathWriter.write("C:\\Program Files\\" + buildForComputer + "\r\n");
			pathWriter.close();
			// Change version number in MOVESWindow.java
			currentLine = windowReader.readLine();
			String windowOutput = "";
			while(currentLine != null) {
				if(currentLine.indexOf("@version") != -1) {
					int index = currentLine.indexOf("@version") + 8;
					currentLine = currentLine.substring(0,index) + "\t    " + buildForHuman;
				} else if(currentLine.indexOf("public static final String MOVES_VERSION = ")
						!= -1) {
					int index =
							currentLine.indexOf("public static final String MOVES_VERSION = ")
							+ 43;
					currentLine = currentLine.substring(0,index) + '"' + buildForHuman + '"' + ';';
				}
				currentLine += "\r\n";
				windowOutput += currentLine;
				currentLine = windowReader.readLine();
			}
			windowReader.close();
			// Save file changes
			BufferedWriter windowWriter = new BufferedWriter(new FileWriter(buildFileDir
					+ "/gov/epa/otaq/moves/master/gui/MOVESWindow.java"));
			windowWriter.write(windowOutput);
			windowWriter.close();
		} catch (FileNotFoundException e) {
			System.err.println(e.getMessage());
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
	}
}
