/**************************************************************************************************
 * @(#)Logger.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.util.*;
import java.text.SimpleDateFormat;

/**
 * Centralized facility for logging application messages. Applications call the static methods of
 * this class to log messages and register objects that can handle logged messages. An application
 * can register multiple objects to handle logged messages. Each object must implement the
 * LogHandler interface. Messages are logged to System.out in addition to the LogHandlers.
 *
 * @author		Wesley Faler
 * @author 		Tim Hull
 * @version		2010-09-29
 * @version		2009-08-14
**/
public class Logger {
	/** True if logging to a file is allowed **/
	public static boolean shouldLogToFile = true;
	/** Writer for the log file, unbuffered **/
	private static PrintWriter logWriter = null;

	/**
	 * Promote WARNING and ERROR message categories to RUN_ERRORs before passing them to the log
	 * handler objects. This is done to indicate that a simulation run is being performed and log
	 * handlers should not prompt the user to confirm logged messages.
	**/
	public static boolean shouldPromoteErrorLevel = false;

	/** list of objects that implement LogHandler **/
	static LinkedList<LogHandler> logHandlerList = new LinkedList<LogHandler>();

	/**
	 * Adds the specified object as a "known" LogHandler, by adding it to the
	 * LogHandler list.  When anything calls the static method Log, then the actual
	 * work is performed by all of the signed-up handlers.
	 * @param handler the object that implements LogHandler.
	**/
	public static void addLogHandler(LogHandler handler) {
		synchronized (logHandlerList) {
			if (!logHandlerList.contains(handler)) {
				logHandlerList.add(handler);
			}
		}
	}

	/**
	 * Removes the specified object as a "known" LogHandler, by removing it from the
	 * LogHandler list.
	 * @param handler the object that implements LogHandler.
	**/
	public static void removeLogHandler(LogHandler handler) {
		synchronized (logHandlerList) {
			logHandlerList.remove(handler);
		}
	}

	/**
	 * Calls the list of LogHandler objects, otherwise just outputs the message to standard out.
	 * @param category type of the message.  The values come from static final objects of
	 * the LogMessageCategory class.
	 * @param message the String to get logged.
	**/
	public static void log(LogMessageCategory category, String message) {
		if(shouldPromoteErrorLevel && (category == LogMessageCategory.WARNING
				|| category == LogMessageCategory.ERROR)) {
			category = LogMessageCategory.RUN_ERROR;
		}

		String timeStamp = (new SimpleDateFormat()).format(new java.util.Date());
		String logText = timeStamp + " " + category.toString() + ": " + message;

		synchronized(logHandlerList) {
			Iterator<LogHandler> i = logHandlerList.iterator();
			// display message on standard output
			System.out.println(logText);
			System.out.flush();

			if(shouldLogToFile) {
				try {
					if(logWriter == null) {
						// Open "moveslog.txt" for appending with a 1024 byte buffer that is flushed often.
						logWriter = new PrintWriter(new BufferedWriter(new FileWriter(new File("moveslog.txt"),true),1024),true);
					}
					logWriter.println(logText);
					logWriter.flush();
				} catch(Exception e) {
					// Nothing to do here
				} catch(Error e) {
					// Nothing to do here
				} catch(Throwable e) {
					// Nothing to do here
				}
			}

			// dispatch message to any registered LogHandlers
			for(;i.hasNext();) {
				LogHandler handler = (LogHandler)i.next();
				handler.handleLog(category, message);
			}
		}
	}

	/**
	 * Writes an exception's stack trace and error message to the logging mechanism.
	 * @param category The log message category to log at.
	 * @param e Exception object to get the info from.
	**/
	public static void logException(LogMessageCategory category, Exception e) {
		final String eol = System.getProperty("line.separator");

		ByteArrayOutputStream exceptionTraceBuffer = new ByteArrayOutputStream();
		e.printStackTrace(new PrintStream(exceptionTraceBuffer));

		String message = exceptionTraceBuffer.toString() + eol + e.getMessage();

		log(category, message);
	}

	/**
	 * Logs an exception with an associated error message.
	 * @param exception The exception object that was thrown.
	 * @param errorMessage The message describing where the error occurred.
	**/
	public static void logError(Exception exception, String errorMessage) {
		logException(LogMessageCategory.DEBUG, exception);
		log(LogMessageCategory.ERROR, errorMessage);
	}

	/**
	 * Logs an SQL exception along with an error message and SQL string.
	 * @param exception The exception object that was thrown.
	 * @param errorMessage The message describing where the error occurred.
	 * @param sql The SQL associated with the exception.
	**/
	public static void logSqlError(Exception exception, String errorMessage, String sql) {
		final String eol = System.getProperty("line.separator");
		String sqlErrorMessage = errorMessage + eol + "SQL: " + sql;
		logError(exception, sqlErrorMessage);
	}
}
