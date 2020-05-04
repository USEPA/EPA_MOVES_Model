/**************************************************************************************************
 * @(#)StreamGobbler.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

/**
 * This class' purpose is to read the output streams from an exec'd process.  The stream's data
 * is thrown away, but this keeps the stream's buffer from filling and suspending the
 * the exec'd process.
 *
 * @author		Wes Faler based on Java Traps article by Michael Daconta
 * @version		2009-03-16
**/
public class StreamGobbler extends Thread {
	/** The InputStream to flush **/
	InputStream inputStream;
	
	/** A text description of the stream **/
	String streamDescription;
	
	/** The output stream to redirect data from inputStream to. **/
	OutputStream outputStream;
	
	/**
	 * Default Constructor
	 * @param inputStream The InputStream to read.
	 * @param streamDescription A text description of the stream 
	 * @param outputStream The OutputStream to redirect data from inputStream to.
	 * This can be null if inputStream data should be discarded.
	**/
	public StreamGobbler(InputStream inputStream, String streamDescription,
			OutputStream outputStream) {
		this.inputStream = inputStream;
		this.streamDescription = streamDescription;
		this.outputStream = outputStream;
	}
	
	public void run() {
		PrintWriter writer = null;
		BufferedReader reader = null;

		try {
			if(outputStream != null) {
				writer = new PrintWriter(outputStream);
			} else {
				writer = null;
			}
			
			InputStreamReader streamReader = new InputStreamReader(inputStream);
			reader = new BufferedReader(streamReader);
			
			String line = null;
			
			while ((line = reader.readLine()) != null) {
				if(writer != null) {
					// Handle special case for Nonroad model since it outputs *so much* information
					if(!line.equals("Initializing ...|")
						&& !line.equals("Initializing .../")
						&& !line.equals("Initializing ...-")
						&& !line.equals("Initializing ...\\")) {
						writer.println(line);
					}
				}
			}
		} catch (IOException exception) {
			/**
			 * @explain A separate program was started in order to handle part of the system
			 * processing but a connection to its inputs and outputs could not be made.
			**/
			Logger.logError(exception,"Error handling I/O with launched process");
		} finally {
			if(writer != null) {
				try {
					writer.flush();
				} catch(Exception e) {
					// Nothing to do since it may be a non-issue
				}
			}
			if(reader != null) {
				try {
					reader.close();
				} catch(Exception e) {
					// Nothing to do since it may be a non-issue
				}
			}
		}
	}
}
