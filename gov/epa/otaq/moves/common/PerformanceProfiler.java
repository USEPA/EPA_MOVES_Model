/**************************************************************************************************
 * @(#)PerformanceProfiler.java 
 *
 *
 *
 *************************************************************************************************/

package gov.epa.otaq.moves.common;

import java.util.*;
import java.io.*;

/**
 * Static class to collect statistics on the execution times and frequencies for sections of code.
 * The start() and stop() methods are called several times for different sections of code while the
 * application is running. When the application is done, the writeProfiles() method is called to
 * write the final statistics to a tab delimited file. The PerformanceProfiler is used by SQLRunner
 * to collect statistics on SQL query execution times and frequencies.
 *
 * @author		Wesley Faler
 * @version		2009-04-04
**/
public class PerformanceProfiler {
	/** flag controlling whether or not profiling actually happens **/
	static final boolean shouldDoProfiling = false; // true;
	/** Cache of performance statistics **/
	static TreeMap<String,Statistics> statisticsMap = new TreeMap<String,Statistics>();

	/** Performance Statistics **/
	static class Statistics {
		/** Start of current timing function **/
		long startTime;
		/** Accumulated execution time **/
		long totalTime;
		/** Time to perform first execution **/
		long firstTime;
		/** Number of times this statistic has been run **/
		long runCount = 0;
	}

	/** Standard constructor. **/
	public PerformanceProfiler() {
	}
	
	/**
	 * Starts execution timing
	 * @param stringID String identifier for the thing being profiled.
	**/
	public static void start(String stringID) {
		if(!shouldDoProfiling) {
			return;
		}

		Statistics stats = (Statistics) statisticsMap.get(stringID);

		if(stats == null) {
			stats = new Statistics();
			stats.startTime = System.currentTimeMillis();
			statisticsMap.put(stringID, stats);
		} else {
			stats.startTime = System.currentTimeMillis();
		}
	}

	/**
	 * Stops execution timing
	 * @param stringID stringID name for the thing being profiled.
	**/
	public static void stop(String stringID) {
		if(!shouldDoProfiling) {
			return;
		}

		Statistics stats = (Statistics) statisticsMap.get(stringID);
		if(stats != null) {
			long runTime = System.currentTimeMillis() - stats.startTime;
			if(++stats.runCount == 1) {
				stats.firstTime = runTime;
				stats.totalTime = runTime;
			} else {
				stats.totalTime = stats.totalTime + runTime;
			}
			stats.startTime = 0;
			statisticsMap.put(stringID, stats);
		}
	}

	/**
	 * Writes statistic profiles to system out
	 * @param filename Name of file to write profiles to.
	**/
	public static void writeProfiles(String filename) {
		if(!shouldDoProfiling) {
			return;
		}

		try {
			File performanceProfilerPath = new File(filename);
			
			final String eol = System.getProperty("line.separator");
			Writer performanceProfilerWriter = new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(performanceProfilerPath)));
	
			performanceProfilerWriter.write("First Time\tRun Count\tTotal Time\tString ID" + eol);
			for(Iterator<String> i=statisticsMap.keySet().iterator();i.hasNext();) {
				String stringID = (String)i.next();
				Statistics stats = (Statistics) statisticsMap.get(stringID);
				performanceProfilerWriter.write(stats.firstTime+"\t"+stats.runCount+"\t"+
						stats.totalTime + "\t\""+stringID+"\"" + eol);
			}
			
			performanceProfilerWriter.close();
		} catch(IOException e) {
			/**
			 * @explain An error occured while writing database performance logs to a file.
			 * These logs are typically only used by developers to fine tune their logic, but
			 * could point to a problem with drive space.
			**/
			Logger.logError(e,"Writing performance profiles to "+filename+" failed.");
		}
	}
}
