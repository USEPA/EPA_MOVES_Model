/**************************************************************************************************
 * @(#)InterconnectionTracker.java
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.util.*;
import java.io.*;

/**
 * Record interconnections between MOVES' components. These interactions are dynamic,
 * subject to compilation flags and user mode settings. As such, an automated method
 * of recording these interactions is essential for accurate documentation.
 *
 * @author		Wesley Faler
 * @version		2013-11-29
**/
public class InterconnectionTracker {
	/** Name of file to hold recorded tracking information **/
	private static final String trackingFileName = "CalculatorInfo.txt";

	/** Tracking status **/
	private static boolean isTracking = false;
	/** Writer for the tracking file **/
	private static PrintWriter writer = null;

	/** Begin tracking interconnections **/
	public static void startTracking() {
		finishTracking();
		if(CompilationFlags.GENERATE_CALCULATOR_INFO_DOCUMENTATION) {
			try {
				writer = new PrintWriter(trackingFileName);
				writer.println("// Registration\tOutputPollutantName\tOutputPollutantID\tProcessName\tProcessID\tModuleName");
				writer.println("// Subscribe\tModuleName\tProcessName\tProcessID\tGranularity\tPriority");
				writer.println("// Chain\tOutputModuleName\tInputModuleName");
				isTracking = true;
				Logger.log(LogMessageCategory.INFO,"Started tracking interconnections");
			} catch(Exception e) {
				Logger.logError(e,"Unable to write to " + trackingFileName);
			}
		}
	}

	/** End tracking interconnections **/
	public static void finishTracking() {
		if(writer != null) {
			try {
				writer.flush();
				writer.close();
			} catch(Exception e) {
				// Nothing to do here
			}
			writer = null;
		}
		isTracking = false;
	}

	/**
	 * Record a calculator's registration of an output pollutant.
	 * @param r a successful registration
	**/
	public static void recordRegistration(EmissionCalculatorRegistration r) {
		if(!isTracking) {
			return;
		}
		// Registration	OutputPollutantName	OutputPollutantID	ProcessName	ProcessID	ModuleName
		String line = "Registration"
				+ "\t" + r.pollutant.pollutantName
				+ "\t" + r.pollutant.databaseKey
				+ "\t" + r.process.processName
				+ "\t" + r.process.databaseKey
				+ "\t" + getModuleName(r.emissionCalculator);
		writeLine(line);
	}

	/**
	 * Record a subscription to the MasterLoop.
	 * @param loopable The target loopable to subscribe.
	 * @param process The process to subscribe it for.
	 * @param loopGranularity The granularity level to execute it at.
	 * @param priority The priority to execute it at relative to other loopable objects at the
	 * same granularity.
	**/
	public static void recordSubscription(MasterLoopable loopable, EmissionProcess process,
			MasterLoopGranularity loopGranularity, int priority) {
		if(!isTracking) {
			return;
		}
		// Subscribe	ModuleName	ProcessName	ProcessID	Granularity	Priority
		String line = "Subscribe"
				+ "\t" + getModuleName(loopable)
				+ "\t" + process.processName
				+ "\t" + process.databaseKey
				+ "\t" + loopGranularity.toString()
				+ "\t" + MasterLoopPriority.decode(priority);
		writeLine(line);
	}

	/**
	 * Record a calculator chaining its output to another calculator.
	 * @param output The calculator producing results that require results from a prior calculator.
	 * @param input The calculator that produces results that are then used to make the final output.
	**/
	public static void recordChain(EmissionCalculator output, EmissionCalculator input) {
		if(!isTracking) {
			return;
		}
		// Chain	OutputModuleName	InputModuleName
		String line = "Chain"
				+ "\t" + getModuleName(output)
				+ "\t" + getModuleName(input);
		writeLine(line);
	}

	/**
	 * Obtain the short name of an object's class.
	 * @param thing object to be examined
	 * @return class name without any package
	**/
	private static String getModuleName(Object thing) {
		return thing.getClass().getSimpleName();
	}

	/**
	 * Add one line to the tracking text file.
	 * @param line text to be appended.
	**/
	private static void writeLine(String line) {
		if(!isTracking || writer == null) {
			return;
		}
		writer.println(line);
	}
}
