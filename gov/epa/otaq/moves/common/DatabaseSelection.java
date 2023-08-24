/**************************************************************************************************
 * @(#)DatabaseSelection.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import java.io.File;
import java.util.*;

/**
 * Holds the information needed to connect to a user-selected database and provides methods to get
 * connections to that database. This class implements the Comparable interface to facilitate
 * maintaining it in a TreeSet of selected databases.
 *
 * @author		Wesley Faler
 * @version		2015-03-17
**/
public class DatabaseSelection implements Comparable {
	/**
	 * The user name for any MOVES database server.
	 * When blank, userProvidedUserName is used.
	**/
	public static final String SERVER_USER_NAME = new String("");
	/**
	 * The password for any MOVES database server.
	 * When blank, userProvidedPassword is used.
	**/
	public static final String SERVER_PASSWORD = new String("");

	/**
	 * MySQL user to access MOVES databases. This value is used
	 * after checking each instance's userName and the hard-coded
	 * SERVER_USER_NAME variable.
	**/
	public static String userProvidedUserName = "";
	/**
	 * MySQL password to access MOVES databases. This value is used
	 * after checking each instance's password and the hard-coded
	 * SERVER_PASSWORD variable.
	**/
	public static String userProvidedPassword = "";

	/** flags a database that could not be created and did not already exist **/
	public static final int NOT_CREATED = 0;
	/** flags a database that was created but did not already exist **/
	public static final int CREATED = 1;
	/** flags a database that already exists **/
	public static final int EXISTS = 2;

	/**
	 * TCP/IP port for communicating with the MySQL server.  When 0, the JDBC default (3306) will
	 * be used instead of this value.
	**/
	public static int MySQLPort = 0;
	/** True if the optional configuration file, MySQL.txt, has been read **/
	private static boolean hasReadPortConfiguration = false;

	/** The server name that serves the selected database **/
	public String serverName = "";
	/** The name of the selected database **/
	public String databaseName = "";
	/** The user name to use to access the database **/
	public String userName = SERVER_USER_NAME;
	/** The password to use to access the database **/
	public String password = SERVER_PASSWORD;
	/** Error string gets set in case of thrown exception, useful during testing. **/
	public String errorMessage = new String("");
	/** The name of the database driver used to connect to the specified database. **/
	public String driverName = "";
	/** Optional description of the database **/
	public String description = "";

	/**
	 * Test for the presence of a database name.
	 * @return true if there is a database name
	**/
	public boolean hasDatabase() {
		if(serverName == null) {
			serverName = "";
		} else {
			serverName = serverName.trim();
		}
		if(databaseName != null) {
			databaseName = databaseName.trim();
			if(databaseName.length() > 0) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Opens a JDBC connection object to the specified database.
	 * @throws ClassNotFoundException If there was an error loading the database driver class.
	 * @throws SQLException If there was a database error opening the connection.
	 * @return The database connection
	**/
	public Connection openConnection() throws ClassNotFoundException, SQLException {
		return openConnection(true);
	}

	/**
	 * Opens a JDBC connection object to the specified database.
	 * @param useDatabaseName true if the database name should be required, false if a generic connection
	 * to the server should be extablished (useful for creating databases).
	 * @throws ClassNotFoundException If there was an error loading the database driver class.
	 * @throws SQLException If there was a database error opening the connection.
	 * @return The database connection
	**/
	public Connection openConnection(boolean useDatabaseName) throws ClassNotFoundException, SQLException {
		readPortConfiguration();

		// If the user name is blank, try using the user/password
		// provided via a configuration file.
		if(userName == null || userName.length() <= 0) {
			userName = userProvidedUserName;
			password = userProvidedPassword;
		}

		try {
			//String driverNameToUse = "org.gjt.mm.mysql.Driver";
			String driverNameToUse = "com.mysql.jdbc.Driver";

			if(driverName != null && driverName.length() > 0) {
				driverNameToUse = driverName;
			}
			// load the driver
			Class.forName(driverNameToUse);
			// open the connection
			String connectionString = "jdbc:mysql://" + serverName;
			if(MySQLPort > 0) {
				connectionString += ":" + MySQLPort;
			}
			if(useDatabaseName) {
				connectionString += "/" + databaseName;
			} else {
				connectionString += "/";
			}

			Connection result = DriverManager.getConnection(connectionString, userName, password);
			DatabaseUtilities.isLocal(result);
			return result;
		} catch (ClassNotFoundException exception) {
			// Could not find the database driver
			errorMessage = "load driver: " + exception.toString();

			throw exception;
		} catch (SQLException exception) {
			// Could not connect to the database
			errorMessage = "getConnection: " + exception.toString();

			throw exception;
		}
	}

	/**
	 * Opens a JDBC connection object to the specified database. Returns null instead of throwing
	 * an exception if the open attempt fails .
	 * @return The database connection or null on error.
	**/
	public Connection openConnectionOrNull() {
		return openConnectionOrNull(true);
	}

	/**
	 * Opens a JDBC connection object to the specified database. Returns null instead of throwing
	 * an exception if the open attempt fails .
	 * @param useDatabaseName true if the database name should be required, false if a generic connection
	 * to the server should be extablished (useful for creating databases).
	 * @return The database connection or null on error.
	**/
	public Connection openConnectionOrNull(boolean useDatabaseName) {
		try {
			return openConnection(useDatabaseName);
		} catch (Exception exception) {
			return null;
		}
	}

	/**
	 * Safely create a non-output database, including running a script if the database did not
	 * already exist.
	 * @param scriptFileName The name of a script file (of SQL commands) to execute.
	 * May be null.
	 * @return NOT_CREATED, EXISTS, or CREATED.
	**/
	public int safeCreateDatabase(String scriptFileName) {
		return safeCreateDatabaseCore(scriptFileName, null);
	}
    
	/**
	 * Safely create an output database, including running a script if the database did not
	 * already exist.
	 * @param defaultDatabaseName The default database to use when creating the output database for the translate_* tables
	 * @return NOT_CREATED, EXISTS, or CREATED.
	**/
	public int safeCreateOutputDatabase(String defaultDatabaseName) {
		int result = safeCreateDatabaseCore("database/CreateOutput.sql", defaultDatabaseName);
        if(result == CREATED) {
            Connection db = openConnectionOrNull();
            if(db != null) {
                if(!CompilationFlags.ALLOW_FUELSUBTYPE_OUTPUT) {
                    try {
                        SQLRunner.executeSQL(db,"alter table MOVESOutput drop fuelSubTypeID");
                    } catch(Exception e) {
                        // Nothing to do here.
                        // This exception will occur when MOVESOutput does not already contain
                        // the fuelSubTypeID column. The net result is the desired state of
                        // a MOVESOutput table without the column, so don't complain.
                    }
                }
                try {
                    DatabaseUtilities.executeScript(db,new File("database/CreateOutputRates.sql"));
                } catch(Exception e) {
                    Logger.logError(e, "Create database, "+databaseName+", from script failed.");
                } finally {
                    DatabaseUtilities.closeConnection(db);
                }
            }
        }
		return result;
	}

	/**
	 * Safely create a database, including running a script if the database did not
	 * already exist.
	 * @param scriptFileName The name of a script file (of SQL commands) to execute.
	 * May be null.
	 * @return NOT_CREATED, EXISTS, or CREATED.
	**/
	int safeCreateDatabaseCore(String scriptFileName, String defaultDatabaseName) {
		int result = NOT_CREATED;
		if(databaseName == null || databaseName.length() == 0) {
			return result;
		}
		Connection db = openConnectionOrNull();
		if(null == db) {
			// Maybe the database doesn't exist, so try just the server
			String oldDatabaseName = databaseName;
			databaseName = new String("");
			db = openConnectionOrNull();
			databaseName = oldDatabaseName;
			if(null == db) {
				// There is no way to connect to the server, so there is nothing else to be done.
				return result;
			}
			try {
				// Create the database
				SQLRunner.executeSQL(db,"CREATE DATABASE " + databaseName);
				// Use the database
				SQLRunner.executeSQL(db,"USE " + databaseName);
                // Run the script
				if(scriptFileName != null) {
                    // replace ##defaultdb## with the actual default database name if one is specified
                    if (defaultDatabaseName != null) {
                        TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
                        replacements.put("##defaultdb##", defaultDatabaseName);
					    DatabaseUtilities.executeScript(db, new File(scriptFileName), replacements);
                    } else {
					    DatabaseUtilities.executeScript(db, new File(scriptFileName));
                    }
				}
				result = CREATED;
			} catch(Exception e) {
				/**
				 * @issue Create database, [databaseName], from script failed.
				 * @explain An attempt to create a database using a script file failed.  Most
				 * likely there is a syntax error in the script file itself.  See the SQL
				 * exception printed along with this message for details.
				**/
				Logger.logError(e, "Create database, "+databaseName+", from script failed.");
				DatabaseUtilities.closeConnection(db);
				return result;
			}
		} else {
			result = EXISTS;
		}
		DatabaseUtilities.closeConnection(db);
		return result;
	}

	/**
	 * Overrides Object.equals() to provide a real equality test.
	 * @param other The object to test for equality against.
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(!(other instanceof DatabaseSelection)) {
			return false;
		}
		return compareTo(other) == 0;
	}

	/**
	 * Overrides Object.clone() method, to provide a "deep copy".
	**/
	public Object clone() {
		DatabaseSelection newCopy = new DatabaseSelection();
		// using 'this' keyword for emphasis, though technically not necessary
		newCopy.serverName = new String(this.serverName);
		newCopy.databaseName = new String(this.databaseName);
		newCopy.userName = new String(this.userName);
		newCopy.password = new String(this.password);
		newCopy.driverName = new String(this.driverName);
		newCopy.description = new String(this.description);
		return newCopy;
	}

	/**
	 * Comparable Interface Implementation. Provides Tree container ordering and display
	 * order.
	 * @param other Another DatabaseSelection object to compare.
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		if(other instanceof DatabaseSelection) {
			DatabaseSelection otherSelection = (DatabaseSelection)other;
			int result = serverName.compareTo(otherSelection.serverName);
			if(result == 0) {
				result = databaseName.compareTo(otherSelection.databaseName);
			}
			if(result == 0) {
				result = userName.compareTo(otherSelection.userName);
			}
			if(result == 0) {
				result = password.compareTo(otherSelection.password);
			}
			return result;
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * Returns a String representation of this object.  Added for debugging.
	 * @return The String representation.
	**/
	public String toString() {
		//return serverName + "/" + databaseName + "/" + userName + "/" + password;
		return serverName + " / " + databaseName + " / " + description;
	}

	/**
	 * Opens a "simple" database connection, that is guaranteed to work by using known, good
	 * input params.  By not specifying any database name to connect to, this allows a database
	 * connection, but any subsequent sql statements should first select a database to work in.
	 * @return A Connection object, or null if JDBC is not available.
	**/
	public static Connection openKnownWorkingConnection() {
		DatabaseSelection db = new DatabaseSelection();
		db.serverName = "localhost";
		db.databaseName = "";
		db.userName = DatabaseSelection.SERVER_USER_NAME;
		db.password = DatabaseSelection.SERVER_PASSWORD;
		return db.openConnectionOrNull();
	}

	/**
	 * Read the MySQL.txt file, if it exists, looking for the port number on a single line
	 * by itself.
	**/
	private static void readPortConfiguration() {
		if(hasReadPortConfiguration) {
			return;
		}
		hasReadPortConfiguration = true;
		try {
			ArrayList<String> lines = FileUtilities.readLines(new File("MySQL.txt"));
			if(lines == null) {
				return;
			}
			for(Iterator<String> i=lines.iterator();i.hasNext();) {
				try {
					int t = Integer.parseInt(i.next().trim());
					if(t > 0) {
						MySQLPort = t;
						return;
					}
				} catch(Exception e) {
					// Nothing to do here, it may be a comment or blank line
				}
			}
		} catch(Exception e) {
			// Nothing to do here
		}
	}
}
