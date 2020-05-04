/**************************************************************************************************
 * @(#)ApplicationRunner.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.utils;

import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.utils.StreamGobbler;

import gov.epa.otaq.moves.worker.framework.WorkerConfiguration;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Map;
import java.util.ArrayList;

/**
 * A utility class that wraps a method to invoke an application and fetch the error/output information.
 * This is adapted from NMIM DistributedWorkerThread.java
 * 
 * @author		Wesley Faler
 * @author		Jizhen (Jason) Zhao, UNC IE
 * @version		2016-04-07
**/
public class ApplicationRunner {
	/**
	 * Synchronous method to execute a given application.
	 * @param applicationPath Path to the application to run
	 * @param commandLineParameters The command line parameters to pass to the application. Never null, but can be empty.
	 * @param workingDirectoryPath Path to the directory to use as the current working directory.
	 * @param processOutputStream The OutputStream to send process output to. This can
	 * be null. This is closed upon completion.
	 * @param inputText The text sent to the application's input stream. This can be null.
	 * @param runInCmd Run command in CMD.EXE.
	 * @param environment optional array of environment variables. This can be null.
	**/
	// this is adapted from NMIM DistributedWorkerThread.java
	public static void runApplication(File applicationPath,
				String[] commandLineParameters, File workingDirectoryPath,
				OutputStream processOutputStream, String inputText, boolean runInCmd,
				String[] environment)
				throws IOException, InterruptedException {
		// Force reclaimation of memory immediately so that the application we launch has as
		// much memory to do its work as possible
		Runtime.getRuntime().runFinalization();
		Runtime.getRuntime().gc();
		try {

			if(applicationPath.getName().toLowerCase().startsWith("nonroad")) {
				File targetApplicationPath = new File(WorkerConfiguration.theWorkerConfiguration.nonroadApplicationPath);

				try {
					if(!targetApplicationPath.exists()) {
						String updatedPath = WorkerConfiguration.theWorkerConfiguration.nonroadApplicationPath;
						if(updatedPath.startsWith("/") || updatedPath.startsWith("\\")) {
							updatedPath = ".." + updatedPath;
						} else {
							updatedPath = "../" + updatedPath;
						}
						targetApplicationPath = new File(updatedPath);
						if(!targetApplicationPath.exists()) {
							if(updatedPath.startsWith("/") || updatedPath.startsWith("\\")) {
								updatedPath = ".." + updatedPath;
							} else {
								updatedPath = "../" + updatedPath;
							}
							targetApplicationPath = new File(updatedPath);
							if(!targetApplicationPath.exists()) {
								targetApplicationPath = null;
							}
						}
					}
				} catch(Exception e) {
					targetApplicationPath = null;
				}
				if(targetApplicationPath == null) {
					targetApplicationPath = applicationPath; // new File(applicationPath.getParentFile(), "nonroad.exe");
				}
				try {
					Logger.log(LogMessageCategory.DEBUG,"Using Nonroad path: " + targetApplicationPath.getCanonicalPath());
				} catch(IOException e) {
					// Nothing to do here
				}
				applicationPath = targetApplicationPath;
			}

			Logger.log(LogMessageCategory.DEBUG, "Run " + applicationPath.getCanonicalPath() + " in " + workingDirectoryPath.getCanonicalPath());
			if(commandLineParameters.length > 0) {
				Logger.log(LogMessageCategory.DEBUG, "Command line = ");
				for (int i = 0; i < commandLineParameters.length; i++) {
					if(i > 0) {
						Logger.log(LogMessageCategory.DEBUG, ", ");
					}
					Logger.log(LogMessageCategory.DEBUG, commandLineParameters[i]);
				}
				Logger.log(LogMessageCategory.DEBUG, "");
			}
			if(inputText != null) {
				Logger.log(LogMessageCategory.DEBUG, "inputText = " + inputText);
			}

			String[] commandArray = null;
			if(runInCmd) {
				commandArray = new String[3];
				commandArray[0] = "CMD.EXE";
				commandArray[1] = "/C";
				commandArray[2] = "\"\"" + applicationPath.getCanonicalPath() + "\"";
				for (int i = 0; i < commandLineParameters.length; i++) {
					commandArray[2] += " " + commandLineParameters[i];
				}
				commandArray[2] += "\"";
			} else {
				commandArray = new String[commandLineParameters.length + 1];
				commandArray[0] = applicationPath.getCanonicalPath();
				for (int i = 0; i < commandLineParameters.length; i++) {
					commandArray[i + 1] = commandLineParameters[i];
				}
			}

//			for (int i = 0; i < commandArray.length; i++) {
//				System.out.println("command["+i+"]=" + commandArray[i]);
//			}

			//Runtime runtime = Runtime.getRuntime();
			//Process process = runtime.exec(commandArray, environment, workingDirectoryPath);
			ArrayList<String> commandList = new ArrayList<String>();
			for(int i=0;i<commandArray.length;i++) {
				commandList.add(commandArray[i]);
			}
			ProcessBuilder pb = new ProcessBuilder(commandList);
			if(environment != null) {
				Map<String,String> env = pb.environment();
				for(int i=0;i<environment.length-1;i+=2) {
					env.put(environment[i],environment[i+1]);
				}
			}
			pb.directory(workingDirectoryPath);
			Process process = pb.start();

			if(process != null) {
				// Launch two StreamGobbler threads to keep the process's output stream buffers
				// empty and prevent the process from hanging.
				StreamGobbler errorGobbler = new StreamGobbler(
							process.getErrorStream(), "ERROR", null);
				StreamGobbler outputGobbler = new StreamGobbler(
							process.getInputStream(), "OUTPUT", processOutputStream);

				errorGobbler.start();
				outputGobbler.start();

				if(inputText != null) {
					OutputStreamWriter inputWriter = new OutputStreamWriter(process.getOutputStream());
					inputWriter.write(inputText);
					inputWriter.close();
				}

				process.waitFor();
				errorGobbler.join();
				outputGobbler.join();
			}
		} finally {
			if(processOutputStream != null) {
				processOutputStream.flush();
				processOutputStream.close();
			}
		}
	}
}
