/**************************************************************************************************
 * @(#)GISException.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.gis.api;

/**
 * Exception class for GIS-related issues
 *
 * @author		Wesley Faler
 * @version		2008-05-03
**/
public class GISException extends Exception {
	/**
	 * Constructs a new exception with null as its detail message.
	**/
	public GISException() {
		super();
	}

	/**
	 * Constructs a new exception with the specified detail message. The cause is not initialized,
	 * and may subsequently be initialized by a call to Throwable.initCause(java.lang.Throwable).
	 * @param message the detail message. The detail message is saved for later retrieval by the 
	 * Throwable.getMessage() method.
	**/
	public GISException(String message) {
		super(message);
	}

	/**
	 * Constructs a new exception with the specified detail message and cause.
	 * Note that the detail message associated with cause is not automatically incorporated in this
	 * exception's detail message.
	 * @param message the detail message which is saved for later retrieval by the 
	 * getMessage() method.
     * @param cause the cause (which is saved for later retrieval by the Throwable.getCause() 
     * method). A null value is permitted, and indicates that the cause is nonexistent or unknown.
    **/
	public GISException(String message, Throwable cause) {
		super(message,cause);
	}

	/**
	 * Constructs a new exception with the specified cause and a detail message from the cause,
	 * if provided.  This constructor is useful for exceptions that are little more than 
	 * wrappers for other throwables (for example, PrivilegedActionException).
     * @param cause the cause (which is saved for later retrieval by the Throwable.getCause() 
     * method). (A null value is permitted, and indicates that the cause is nonexistent or unknown.)
    **/
	public GISException(Throwable cause) {
		super(cause);
	}

	/**
	 * Obtain human readable text
	 * @return human readable error message
	**/
	public String toString() {
		return getMessage();
	}
}
