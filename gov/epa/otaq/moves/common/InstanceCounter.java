/**************************************************************************************************
 * @(#)InstanceCounter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.net.*;
import java.util.*;


/**
 * Determine this program's instance information on its machine.  This is used
 * when multiple workers or masters on the same machine must access a central
 * database server with unique, but repeatable, database names.
 *
 * Each instance gets its own TCP/IP socket starting with BASE_FLAG_PORT.  A socket
 * is used because, unlike a file, it is guaranteed to be removed by the operating
 * system regardless of how the JVM exits.  A master that launches a worker in
 * the same JVM will use the same instance number for both the master and worker.
 *
 * @author		Wesley Faler
 * @version		2010-07-12
**/
public class InstanceCounter {
	/** TCP/IP port used on the socket designating a running process **/
	private static final int BASE_FLAG_PORT = 13133;
	/** Mutex for instance checking **/
	private static Integer mutex = Integer.valueOf(13133);
	/** 1-based instance identifier.  A value of 0 indicates it has not been set. **/
	private static int instanceID = 0;
	/** TCP/IP socket used to indicate the instance's presence **/
	private static ServerSocket instanceSocket = null;
	/** Depth counter for designating a running instance's presense **/
	private static int instanceSocketCount = 0;
	/** Name of the machine, suitable for use as part of a database name **/
	private static String machineName = "NoComputerID";

	/** Acquire an instance identifier if not already acquired **/
	public static void setup() {
		synchronized(mutex) {
			if(instanceID > 0) {
				return;
			}
			if(instanceSocketCount == 0) {
				for(int i=0;i<8192;i++) {
					try {
						instanceSocket = new ServerSocket(BASE_FLAG_PORT+i);
						instanceID = 1 + i;
						break;
					} catch(Exception e) {
						// Nothing to do here.  This will happen as we probe for an
						// available instance ID.
					} finally {
						if(instanceID <= 0 && instanceSocket != null) {
							try {
								instanceSocket.close();
							} catch(Exception e) {
								// Nothing to do here
							}
							instanceSocket = null;
						}
					}
				}
				if(instanceID > 0) {
					try {
						machineName = InetAddress.getLocalHost().getHostName();
					} catch(UnknownHostException e) {
						machineName = "NoComputerID";
					}
					char[] c = machineName.toCharArray();
					machineName = "";
					for(int i=0;i<c.length;i++) {
						if(Character.isLetterOrDigit(c[i])) {
							machineName += c[i];
						} else {
							machineName += "_";
						}
					}
				}
			}
			instanceSocketCount++;
		}
	}

	/** Release any previously acquired instance identifier **/
	public static void shutdown() {
		synchronized(mutex) {
			if(instanceID <= 0) {
				return;
			}
			instanceSocketCount--;
			if(instanceSocketCount <= 0) {
				if(instanceSocket != null) {
					try {
						instanceSocket.close();
					} catch(Exception e) {
						// Nothing to do here
					}
					instanceSocket = null;
				}
				instanceID = 0;
			}
		}
	}

	/**
	 * Obtain the 1-based instance identifier
	 * @return the 1-based instance identifier
	**/
	public static synchronized int getInstanceID() {
		synchronized(mutex) {
			if(instanceID > 0) {
				return instanceID;
			}
			setup();
			return instanceID;
		}
	}

	/**
	 * Obtain a unique but repeatable database name for this machine
	 * @param purpose prefix used to designate the purpose of the database, such
	 * as "MOVESExecution" or "MOVESWorker".
	 * @return a unique but repeatable database name for this machine
	**/
	public static synchronized String getDB(String purpose) {
		synchronized(mutex) {
			getInstanceID();
			return purpose + instanceID + machineName;
		}
	}
}
