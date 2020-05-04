/**************************************************************************************************
 * @(#)ConfigurationWriter.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.installer;

import java.io.*;

/**
 * Modifies MOVES worker configuration file and MOVES configuration file.
 * The installation and MySQL directories are added to the configuration files.
 *
 * @author 	Wesley Faler
 * @version 2014-12-17
**/
public class ConfigurationWriter {
	/** directory path to MySQL **/
	private String mySQLDirectory;
	/** location of WorkerConfiguration.txt **/
	private String workerConfig;
	/** location of MOVESConfiguration.txt **/
	private String movesConfig;
	/** location of manyworkers.txt **/
	private String manyWorkersConfig;
	/** location of maketodo.txt **/
	private String makeTodoConfig;
	/** install directory **/
	private String installDirectory;

	/**
	 * Constructor
	 * @param mySQL Directory path of MySQL data directory
	 * @param worker Location of worker configuration file
	 * @param moves Location of MOVES configuration file
	 * @param installPath install directory
	 */
	public ConfigurationWriter(String mySQL, String worker, String moves, String installPath) {
		mySQLDirectory = mySQL;
		workerConfig = worker;
		movesConfig = moves;

		manyWorkersConfig = worker;
		try {
			File f = new File(manyWorkersConfig);
			f = new File(f.getParentFile(),"manyworkers.txt");
			manyWorkersConfig = f.getCanonicalPath();
		} catch(Exception e) {
			System.out.println("Unable to create path to manyworkers.txt");
		}

		makeTodoConfig = worker;
		try {
			File f = new File(makeTodoConfig);
			f = new File(f.getParentFile(),"maketodo.txt");
			makeTodoConfig = f.getCanonicalPath();
		} catch(Exception e) {
			System.out.println("Unable to create path to maketodo.txt");
		}

		installDirectory = installPath;
	}

	/**
	 * Search above the install directory for MOVESComputerID.txt, stopping if it cannot
	 * go further up the chain or if an exception occurs.  By default, the MOVESComputerID.txt
	 * file in the install directory will be used.
	 * @param installDirectory directory MOVES is being installed into
	 * @return the full path and name of the MOVESComputerID.txt file to be used.
	**/
	private String findComputerIDPath(String installDirectory) {
		try {
			File f = new File(installDirectory);
			f = f.getParentFile(); // start searching one level above the installation directory
			while(f != null && f.exists()) {
				File candidate = new File(f,"MOVESComputerID.txt");
				if(candidate.exists()) {
					return candidate.getCanonicalPath();
				}
				f = f.getParentFile();
			}
		} catch(Exception e) {
			// Nothing to do here
		}
		// return the default if something went wrong or if nothing could be found
		return installDirectory + "\\MOVESComputerID.txt";
	}

	/**
	 * Makes changes to MOVES and worker configuration files.  MySQL data directory
	 * and location of worker configuration file added to the MOVES configuration file.
	 * The default installation directory is replaced by the actual installation directory
	 * in the worker configuration file.
	**/
	public void write() {
		try {
			String computerIDPath = findComputerIDPath(installDirectory);

			// Create BufferedReader for reading the file
			BufferedReader reader = new BufferedReader(new FileReader(movesConfig));
			String movesConfigOutput = "";
			String currentLine = reader.readLine();
			// check if reader.readLine() is null, if so the stream has ended
			while(currentLine != null) {
				currentLine = doMasterReplacements(currentLine);
				if(currentLine.length() > 0) {
					movesConfigOutput = movesConfigOutput + currentLine + "\r\n";
				}
				currentLine = reader.readLine();
			}

			// Write changes to file system
			BufferedWriter writer = new BufferedWriter(new FileWriter(movesConfig));
			writer.write(movesConfigOutput);
			writer.close();

			// WorkerConfiguration.txt changes
			reader = new BufferedReader(new FileReader(workerConfig));
			String workerConfigOutput = "";
			currentLine = reader.readLine();
			while(currentLine != null) {
				currentLine = doWorkerReplacements(currentLine);
				if(currentLine.length() > 0) {
					workerConfigOutput = workerConfigOutput + currentLine + "\r\n";
				}
				currentLine = reader.readLine();
			}
			writer = new BufferedWriter(new FileWriter(workerConfig));
			writer.write(workerConfigOutput);
			writer.close();
			reader.close();

			// manyworkers.txt changes
			File manyWorkersFile = new File(manyWorkersConfig);
			if(manyWorkersFile.exists()) {
				reader = new BufferedReader(new FileReader(manyWorkersConfig));
				String manyWorkersConfigOutput = "";
				currentLine = reader.readLine();
				while(currentLine != null) {
					currentLine = doWorkerReplacements(currentLine);
					if(currentLine.length() > 0) {
						manyWorkersConfigOutput = manyWorkersConfigOutput + currentLine + "\r\n";
					}
					currentLine = reader.readLine();
				}
				writer = new BufferedWriter(new FileWriter(manyWorkersConfig));
				writer.write(manyWorkersConfigOutput);
				writer.close();
				reader.close();
			}

			// maketodo.txt changes
			File makeTodoFile = new File(makeTodoConfig);
			if(makeTodoFile.exists()) {
				reader = new BufferedReader(new FileReader(makeTodoConfig));
				String makeTodoConfigOutput = "";
				currentLine = reader.readLine();
				while(currentLine != null) {
					currentLine = doMasterReplacements(currentLine);
					if(currentLine.length() > 0) {
						makeTodoConfigOutput = makeTodoConfigOutput + currentLine + "\r\n";
					}
					currentLine = reader.readLine();
				}
				writer = new BufferedWriter(new FileWriter(makeTodoConfig));
				writer.write(makeTodoConfigOutput);
				writer.close();
				reader.close();
			}

			// Locate Java
			String javaNativeFolderName = "C:\\Program Files\\Java";
			String javaX86FolderName = "C:\\Program Files (x86)\\Java";
			File javaNativeFolder = new File(javaNativeFolderName);
			File javaX86Folder = new File(javaX86FolderName);
			String javaFolderName = javaNativeFolderName;
			if(!javaNativeFolder.exists() && javaX86Folder.exists()) {
				javaFolderName = javaX86FolderName;
			}

			// Creating MOVES2014Home.txt to save MOVES install path.
			FileWriter setMOVESHomeWriter = null;
			try {
				setMOVESHomeWriter = new FileWriter(javaFolderName + "/MOVES2014Home.txt");
				setMOVESHomeWriter.write(installDirectory);
			} catch(Exception e) {
				System.err.println(e.getMessage());
			} finally {
				if(setMOVESHomeWriter != null) {
					setMOVESHomeWriter.close();
				}
			}

			// setenv.bat changes
			String setEnvOutput = "@echo off\r\n"
				+ "set ANT_HOME=" + installDirectory + "\\Ant\r\n"
				+ "set JAVA_HOME=" + javaFolderName + "\\jdk1.7.0_45\r\n"
				+ "set JRE_HOME=" + javaFolderName + "\\jre7\r\n"
				+ "set CLASSPATH=\r\n";

			setEnvOutput += "set CLASSPATH=%CLASSPATH%;" + installDirectory + "\\\r\n";

			String[] jars = {
				"jlfgr-1_0.jar",
				"junit-4.5.jar",
				"mysql-connector-java-5.1.17-bin.jar",
				"jaxp-api.jar",
				"xercesImpl.jar",
				"xml-apis.jar",
				"sax.jar",
				"jakarta-regexp-1.3.jar",
				"jai_core.jar",
				"jai_codec.jar",
				"commons-lang-2.2.jar",
				"geotools\\gt-api-2.5.4.jar",
				"geotools\\gt-coverage-2.5.4.jar",
				"geotools\\gt-main-2.5.4.jar",
				"geotools\\gt-render-2.5.4.jar",
				"geotools\\gt-wfs-2.5.4.jar",
				"geotools\\gt-wms-2.5.4.jar",
				"geotools\\gt2-mappane-2.3.0.jar",
				"geotools\\gt-referencing-2.5.4.jar",
				"geotools\\jsr-275-1.0-beta-2.jar",
				"geotools\\gt-shapefile-2.5.4.jar",
				"geotools\\gt-shapefile-renderer-2.5.4.jar",
				"geotools\\jts-1.9.jar",
				"geotools\\geoapi-2.2-M1.jar",
				"geotools\\gt-metadata-2.5.4.jar",
				"abbot\\abbot.jar",
				"abbot\\bsh-2.0b4.jar",
				"abbot\\costello.jar",
				"abbot\\gnu-regexp-1.1.0.jar",
				"abbot\\jdom-1.0.jar",
				"poi\\commons-codec-1.5.jar",
				"poi\\commons-logging-1.1.jar",
				"poi\\dom4j-1.6.1.jar",
				"poi\\log4j-1.2.13.jar",
				"poi\\poi-3.9-20121203.jar",
				"poi\\poi-ooxml-3.9-20121203.jar",
				"poi\\poi-ooxml-schemas-3.9-20121203.jar",
				"poi\\stax-api-1.0.1.jar",
				"poi\\xmlbeans-2.3.0.jar"
			};

			for(int i=0;i<jars.length;i++) {
				setEnvOutput += "set CLASSPATH=%CLASSPATH%;" + installDirectory
						+ "\\libs\\" + jars[i] + "\r\n";
			}

			setEnvOutput += "set PATH=%JRE_HOME%\\bin;%JAVA_HOME%\\bin;%ANT_HOME%\\bin;%PATH%\r\n";

			FileWriter setEnvWriter = new FileWriter(installDirectory + "/setenv.bat");
			setEnvWriter.write(setEnvOutput);
			setEnvWriter.close();
		} catch(IOException e) {
			System.err.println(e.getMessage());
		}
	}

	/**
	 * Update a configuration line in a master file, such as MOVESConfiguration.txt
	 * or maketodo.txt.
	 * @param currentLine text to be updated, never null
	 * @return updated line or the original line
	**/
	String doMasterReplacements(String currentLine) {
		if(currentLine.startsWith("defaultDatabaseName")) {
			String defaultDB = "";
			int lastSlash = mySQLDirectory.lastIndexOf("\\");
			if(lastSlash < 0) {
				lastSlash = mySQLDirectory.lastIndexOf("/");
			}
			if(lastSlash >= 0) {
				defaultDB = mySQLDirectory.substring(lastSlash + 1, mySQLDirectory.length());
			} else {
				defaultDB = mySQLDirectory;
			}
			currentLine = "defaultDatabaseName = " + defaultDB;
		} else if(currentLine.startsWith("sharedDistributedFolderPath")) {
			currentLine = "sharedDistributedFolderPath = " + installDirectory + "\\SharedWork";
		} else if(currentLine.startsWith("masterFolderPath")) {
			currentLine = "masterFolderPath = " + installDirectory;
		} else if(currentLine.startsWith("nonroadExePath")) {
			currentLine = "nonroadExePath = " + installDirectory + "\\NONROAD\\NR08a\\NONROAD.exe";
		} else if(currentLine.startsWith("GREETWTPApplication")) {
			//currentLine = "GREETWTPApplication = " + installDirectory +
			//	"\\GREET\\GREETGUI.exe";
			currentLine = ""; // GREET is no longer supported, do not propagate settings
		} else if(currentLine.startsWith("GREETManufactureApplication")) {
			//currentLine = "GREETManufactureApplication = " + installDirectory +
			//	"\\GREET\\GREETMfgStub.exe";
			currentLine = ""; // GREET is no longer supported, do not propagate settings
		} else if(currentLine.startsWith("GREETDirectory")) {
			//currentLine = "GREETDirectory = " + installDirectory +
			//	"\\GREET";
			currentLine = ""; // GREET is no longer supported, do not propagate settings
		} else if(currentLine.startsWith("computerIDPath")) {
			//currentLine = "computerIDPath = " + computerIDPath;
			// Skip this line, letting the system obtain a computer ID from the TCP/IP host name
			currentLine = "";
		}
		return currentLine;
	}

	/**
	 * Update a configuration line in a worker file, such as WorkerConfiguration.txt
	 * or manyworkers.txt.
	 * @param currentLine text to be updated, never null
	 * @return updated line or the original line
	**/
	String doWorkerReplacements(String currentLine) {
		if(currentLine.startsWith("sharedDistributedFolderPath")) {
			currentLine = "sharedDistributedFolderPath = " + installDirectory + "\\SharedWork";
		} else if(currentLine.startsWith("workFolderPath")) {
			currentLine = "workFolderPath = " + installDirectory + "\\WorkerFolder";
		} else if(currentLine.startsWith("nonroadApplicationPath")) {
			currentLine = "nonroadApplicationPath = " + installDirectory + "\\NONROAD\\NR08a\\NONROAD.exe";
		} else if(currentLine.startsWith("nonroadWorkingFolderPath")) {
			currentLine = "nonroadWorkingFolderPath = " + installDirectory + "\\NONROAD\\NR08a";
		} else if(currentLine.startsWith("calculatorApplicationPath")) {
			currentLine = "calculatorApplicationPath = " + installDirectory + "\\calc\\go\\externalcalculatorgo32.exe";
		} else if(currentLine.startsWith("computerIDPath")) {
			//currentLine = "computerIDPath = " + computerIDPath;
			// Skip this line, letting the system obtain a computer ID from the TCP/IP host name
			currentLine = "";
		}
		return currentLine;
	}

	/**
	 * Tries to open an InputStream for the specified file with retries if it fails, and waits
	 * in between failed attempts.  This will throw an exception if the final open attempt fails.
	 * @param f The File on which to create an InputStream.
	 * @throws IOException If the final open attempt fails.
	 * @return An InputStream object.
	**/
	public static InputStream openFileInputStreamWithWait(File f) throws IOException {
		// attempt to open the input stream, with 3 retries maximum
		FileInputStream fs = null;
		for(int i = 0; i < 3; i++) {
			try {
				fs = new FileInputStream(f);
			} catch(Exception e) {
				// If the third attempt fails, then throw an exception with the current stack
				// trace info
				if(i == 2) {
					throw new IOException();
				}
			}
			if(fs == null) {
				try {
					java.lang.Thread.sleep(250);
				} catch(InterruptedException e) {
					// Nothing to do here
				}
			}
		}
		return fs;
	}
}
