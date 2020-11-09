/**************************************************************************************************
 * @(#)DatabaseConnectionManager.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.io.*;
import java.sql.*;
import java.util.*;

/**
 * Static utility class that implements database connection pooling. The DatabaseConnectionManager
 * provides a central location for obtaining and releasing connections to frequently accessed
 * databases. By implementing connection pooling, the DatabaseConnectionManager limits the number
 * of simultaneous connections to a database while also minimizing the overhead associated with
 * having individual objects open and close their own connections. A check-in/check-out system is
 * used to share connections among MOVES objects.
 * <p>The DatabaseConnectionManager maintains a separate connection pool for each of the frequently
 * accessed database types. It does not maintain connections for databases that are accessed
 * infrequently or only by specific objects. The DatabaseConnectionManager maintains a seperate
 * connection to each database just for GUI objects, since these objects run within a single thread
 * and it is undesireable to have them wait on a connection from a connection pool.</p>
 * <p>The DatabaseConnectionManager creates the connection pool and GUI connection for a database
 * the first time a connection of that type is requested. If the database connection information is
 * changed, the cleanup() method must be called to free-up any existing connections before a new
 * connection can be checked-out.</p>
 * <p>A timeout is tracked for each pooled database connection.  If the connection has not been
 * used within that timeout period, it is closed and a new one created in its place.  This is
 * done to prevent TCP/IP timeouts during periods of inactivity on long runs.</p>
 *
 * @author		Wesley Faler
 * @version		2014-09-17
**/
public class DatabaseConnectionManager {
	/** CREATE TABLE IF NOT EXISTS statements for all tables in the default database **/
	public static TreeMapIgnoreCase defaultDatabaseCreateTableStatements
			= new TreeMapIgnoreCase();
	/** CREATE TABLE IF NOT EXISTS statements for all tables in the execution database **/
	public static TreeMapIgnoreCase executionDatabaseCreateTableStatements
			= new TreeMapIgnoreCase();

	/** The number of connections to create for a pool. **/
	static final int CONNECTION_POOL_SIZE = 10;

	/** The connection information for the user specified Output database. **/
	public static DatabaseSelection outputDatabaseSelection = new DatabaseSelection();

	/**
	 * Connection information for an alternate input database other than
	 * the one read from the configuration file.
	**/
	public static DatabaseSelection customInputDatabase = null;

	/**
	 * Internal class used to maintain a pool of connections for a given database type.
	**/
	static class InternalConnectionPool {
		/**
		 * The list of checked-in connections. These are available to be checked out.
		**/
		LinkedList<Connection> checkedInConnections = new LinkedList<Connection>();
		/**
		 * The list of checked out connection objects. This list is used to detect invalid check-in
		 * attempts.
		**/
		LinkedList<Connection> checkedOutConnections = new LinkedList<Connection>();
		/**
		 * The connection reserved exclusively for the GUI thread. This connection does not require
		 * a check out/check in and it use will not cause the client to block.
		**/
		Connection guiConnection;
	};

	/**
	 * The connection pools for the different database types.
	**/
	static InternalConnectionPool[] databasePools
			= new InternalConnectionPool[MOVESDatabaseType.NUM_TYPES];

	/** Maximum idle time for a Connection before it is discarded and recreated **/
	static final long CONNECTION_TIMEOUT_MILLIS = 4*60*1000;

	/**
	 * Tracks the future times for when each Connection should be discarded.
	 * Keyed by Connection, the data is a Long with the future time in milliseconds,
	 * already adjusted by CONNECTION_TIMEOUT_MILLIS.
	 * Note that a HashMap is used because TreeMap requires the compare() routine
	 * which Connection objects don't have.
	**/
	static HashMap<Connection,Long> connectionTimeouts = new HashMap<Connection,Long>(101);

	/**
	 * Initializes all connections for a specified database type. Subsequent requests to initialize
	 * the connections are ignored unless the cleanup() method is called first. The initialize()
	 * method will create a new Execution Database when it receives the first Execution Database
	 * initialization request. This method gets the connection information for the Default and
	 * Execution Database Types from the System Configuration. The setOutputDatabase() method must
	 * be called to set the user specified connection information for the Output Database Type.
	 * Failed attempts to intialize the Default or Execution Database connections are logged as
	 * these are always an error. An attempt to initialize the Output Database may not be an error
	 * and is not logged. The output database is not required until the simulation run is started.
	 * The client object should check the return status of the initialize method and log an error
	 * if required.
	 * @param type The database type to initialize for.
	 * @return This will be false if there was an error during initialization.
	**/
	public static boolean initialize(MOVESDatabaseType type) {
		synchronized(type) {
			boolean result = true;
	
			if(type == MOVESDatabaseType.EXECUTION) {
				Logger.log(LogMessageCategory.INFO,"Initializing MOVESExecution connections...");
			} else if(type == MOVESDatabaseType.OUTPUT) {
				Logger.log(LogMessageCategory.INFO,"Initializing output database connections...");
			} else if(type == MOVESDatabaseType.DEFAULT) {
				Logger.log(LogMessageCategory.INFO,"Initializing default database connections...");
			}
	
			// If initializing the Execution Database connections, first create a new Execution
			// Database.
			if(type == MOVESDatabaseType.EXECUTION) {
				//Logger.log(LogMessageCategory.DEBUG,"Creating MOVESExecution database");
				DatabaseSelection executionSelection = SystemConfiguration.getTheSystemConfiguration().databaseSelections[type.getIndex()];
				Connection knownConnection = executionSelection.openConnectionOrNull(false);
				if(knownConnection == null) {
					/**
					 * @explain An error occurred while creating the MOVESExecution database.
					**/
					Logger.log(LogMessageCategory.ERROR, "Unable to open connection to execution server db");
					result = false;
				} else {
					String sql = "";
					try {
						// Create and Use the MOVESExecution database
						Logger.log(LogMessageCategory.INFO,"Creating the execution database...");
						SQLRunner.executeSQL(knownConnection,
								"CREATE DATABASE IF NOT EXISTS " + executionSelection.databaseName);
						SQLRunner.executeSQL(knownConnection,"USE " + executionSelection.databaseName);
						SQLRunner.executeSQL(knownConnection,"FLUSH TABLES");
						Logger.log(LogMessageCategory.INFO,"Removing tables from the execution database...");
						DatabaseUtilities.dropAllTables(knownConnection);
	
						// Create all the tables contained in the default database
						// and Truncate each of them as well
						Logger.log(LogMessageCategory.INFO,"Creating empty tables in the execution database...");
						SQLRunner.addInnoDBConnection(knownConnection);
						Set tableNames = defaultDatabaseCreateTableStatements.keySet();
						for(Iterator i=tableNames.iterator();i.hasNext();) {
							String t = (String)i.next();
							sql = (String)defaultDatabaseCreateTableStatements.get(t);
							if(SQLRunner.allowInnoDB) {
								sql = StringUtilities.replace(sql,"ENGINE=MyISAM","ENGINE=InnoDB");
								//System.out.println(sql);
							}
							SQLRunner.executeSQL(knownConnection,sql);
	
							SQLRunner.executeSQL(knownConnection,"FLUSH TABLE " + t);
	
							sql = "TRUNCATE " + t;
							SQLRunner.executeSQL(knownConnection,sql);
						}
	
						// Finish creating the database
						Logger.log(LogMessageCategory.INFO,"Finalizing the execution database...");
						SQLRunner.executeSQL(knownConnection,"FLUSH TABLES");
						DatabaseUtilities.executeScript(knownConnection, new File("database/CreateExecution.sql"));
						if(CompilationFlags.DO_RATES_FIRST) {
							DatabaseUtilities.executeScript(knownConnection, new File("database/CreateExecutionRates.sql"));
						}
						SQLRunner.executeSQL(knownConnection,"FLUSH TABLES");
					} catch(SQLException exception) {
						/**
						 * @explain An error occurred while creating the MOVESExecution database.
						**/
						Logger.logSqlError(exception,"Failed to run execution creation script",sql);
						Logger.log(LogMessageCategory.DEBUG,"Example create statement: " + (String)
								defaultDatabaseCreateTableStatements.get("agecategory"));
						result = false;
					} catch(Exception exception) {
						/**
						 * @explain An error occurred while creating the MOVESExecution database.
						**/
						Logger.logError(exception,"Failed to run execution creation script");
						result = false;
					} finally {
						SQLRunner.removeInnoDBConnection(knownConnection);
						DatabaseUtilities.closeConnection(knownConnection);
					}
				}
			}
	
			if(databasePools[type.getIndex()] != null) {
				Logger.log(LogMessageCategory.INFO,"Database connections already established.");
				return true;
			}
	
			// Create the database connection pool object.
			databasePools[type.getIndex()] = new InternalConnectionPool();
			InternalConnectionPool targetPool = databasePools[type.getIndex()];
	
			// Open a pool of connections to the Output Database.
			if(type == MOVESDatabaseType.OUTPUT) {
				if(outputDatabaseSelection.databaseName.trim().length() != 0) {
					// Initialize the GUI connection.
					targetPool.guiConnection = outputDatabaseSelection.openConnectionOrNull();
					// Initialize the connection pool lists.
					if(targetPool.guiConnection != null) {
						setTimeout(targetPool.guiConnection);
						try {
							for(int j = 0; j < CONNECTION_POOL_SIZE; j++) {
								Connection c = outputDatabaseSelection.openConnection();
								if(c != null) {
									setTimeout(c);
									targetPool.checkedInConnections.addLast(c);
								}
							}
							result = true;
						} catch (Exception exception) {
							result = false;
						}
					} else {
						result = false;
					}
				} else {
					result = false;
				}
			} else { // Open a pool of connections to the Default or Execution databases.
				DatabaseSelection sourceSelection =
						SystemConfiguration.getTheSystemConfiguration().databaseSelections[type.getIndex()];
				if(type == MOVESDatabaseType.DEFAULT && customInputDatabase != null) {
					sourceSelection = customInputDatabase;
				}
	
				// Initialize the GUI connection.
				targetPool.guiConnection = sourceSelection.openConnectionOrNull();
				setTimeout(targetPool.guiConnection);
	
				// Initialize the connection pool lists.
				try {
					for(int j = 0; j < CONNECTION_POOL_SIZE; j++) {
						Connection c = sourceSelection.openConnection();
						if(c != null) {
							setTimeout(c);
							targetPool.checkedInConnections.addLast(c);
						}
					}
	
					if(type == MOVESDatabaseType.DEFAULT) { // || type == MOVESDatabaseType.NRDEFAULT) {
						Logger.log(LogMessageCategory.INFO,"Reading default database table definitions...");
						learnCreateTableStatements(targetPool.guiConnection,
								defaultDatabaseCreateTableStatements,false);
					} else if(type == MOVESDatabaseType.EXECUTION) {
						Logger.log(LogMessageCategory.INFO,"Reading execution database table definitions...");
						learnCreateTableStatements(targetPool.guiConnection,
								executionDatabaseCreateTableStatements,true);
					}
				} catch (Exception exception) {
					/**
					 * @explain An error occurred while establishing a connection to to the
					 * database server.
					**/
					Logger.logError(exception, "Failed to open connection of type, " + type + ".");
					result = false;
				}
			}
	
			if(!result) {
				databasePools[type.getIndex()] = null;
			}
	
			Logger.log(LogMessageCategory.INFO,"Done initializing database connections.");
	
			return result;
		}
	}

	/**
	 * Closes all internal connections for the specified type. This will ignore redundant
	 * invocations. Note that the current implementation does not drop the execution database
	 * when all the connections to the Execution Database have been closed.
	 * @param type The database type to initialize for.
	**/
	public static void cleanup(MOVESDatabaseType type) {
		InternalConnectionPool targetPool = databasePools[type.getIndex()];
		databasePools[type.getIndex()] = null;
		if(targetPool == null) {
			return;
		}

		try {
			for(Iterator i = targetPool.checkedInConnections.iterator(); i.hasNext(); ) {
				Connection connection = (Connection) i.next();
				connection.close();
				removeTimeout(connection);
			}

			for(Iterator i = targetPool.checkedOutConnections.iterator(); i.hasNext(); ) {
				Connection connection = (Connection) i.next();
				connection.close();
				removeTimeout(connection);
			}

			if(targetPool.guiConnection != null) {
				targetPool.guiConnection.close();
				removeTimeout(targetPool.guiConnection);
			}
		} catch (SQLException exception) {
			/**
			 * @issue Failed to close database connections for [*]
			 * @explain An error occurred while closing a connection to the database server.
			**/
			Logger.logError(exception, "Failed to close database connections for " + type);
		}
	}

	/**
	 * Initializes all database types. This will ignore types that are already initialized.
	 * @return Returns true if initialization was completely successful.
	**/
	public static boolean initializeAll() {
		boolean result = true;

		for(int i = 0; i < MOVESDatabaseType.NUM_TYPES; i++) {
			MOVESDatabaseType type = MOVESDatabaseType.getByIndex(i);
			if(type != null) {
				/*
				if(!CompilationFlags.USE_NONROAD && type == MOVESDatabaseType.NRDEFAULT) {
					continue;
				}
				*/
				result = initialize(type) && result;
			}
		}

		return result;
	}

	/**
	 * Cleans up all database types. This will ignore types that are already cleaned up.
	**/
	public static void cleanupAll() {
		for(int i = 0; i < MOVESDatabaseType.NUM_TYPES; i++) {
			cleanup(MOVESDatabaseType.getByIndex(i));
		}
	}

	/**
	 * Request a database connection from the shared connection pool. This will wait if one
	 * isn't currently available. Client code must check the connection back in when it
	 * is finished using it.
	 * @param type The type of database connection desired.
	 * @throws InterruptedException If the active thread is interrupted.
	 * @return The reserved database connection.
	**/
	public static Connection checkOutConnection(MOVESDatabaseType type)
			throws InterruptedException {
		/*
		if(!CompilationFlags.USE_NONROAD && type == MOVESDatabaseType.NRDEFAULT) {
			return null;
		}
		*/
		synchronized(type) {
			if(!isInitialized(type)) {
				initialize(type);
			}
		}
		InternalConnectionPool targetPool = databasePools[type.getIndex()];
		if(targetPool == null) {
			return null;
		}

		Connection result;
		LinkedList<Connection> targetList = targetPool.checkedInConnections;
		if(targetList == null) {
			targetPool.checkedInConnections = new LinkedList<Connection>();
			targetList = targetPool.checkedInConnections;
		}
		synchronized(targetList) {
			while(targetList.size() == 0) {
				targetList.wait();
			}
			result = targetList.removeLast();
		}
		result = checkTimeout(type,result);
		targetList = targetPool.checkedOutConnections;
		if(targetList == null) {
			targetPool.checkedOutConnections = new LinkedList<Connection>();
			targetList = targetPool.checkedOutConnections;
		}
		synchronized(targetList) {
			targetList.addLast(result);
		}
		if(result != null && type == MOVESDatabaseType.EXECUTION) {
			SQLRunner.addInnoDBConnection(result);
		}
		return result;
	}

	/**
	 * Returns a connection back to the shared connection pool.
	 * @param type The type of database connection being returned.
	 * @param connection The database connection being returned.
	 * @throws IllegalArgumentException If the connection returned is not in the current list of
	 * checked-out connections. This can occur if the database connection pool has been "cleaned"
	 * since the connection was checked-out.
	**/
	public static void checkInConnection(MOVESDatabaseType type, Connection connection)
			throws IllegalArgumentException {
		InternalConnectionPool targetPool = databasePools[type.getIndex()];
		if(targetPool == null) {
			throw new IllegalArgumentException(
					"Attempting to check-in a Connection to an uninitialized connection pool.");
		}

		if(connection != null && type == MOVESDatabaseType.EXECUTION) {
			SQLRunner.removeInnoDBConnection(connection);
		}

		LinkedList<Connection> targetList = targetPool.checkedOutConnections;
		synchronized(targetList) {
			if(!targetList.remove(connection)) {
				throw new IllegalArgumentException(
						"Attempting to check-in a Connection to the wrong list.");
			}
		}
		setTimeout(connection);
		targetList = targetPool.checkedInConnections;
		synchronized(targetList) {
			targetList.addLast(connection);
			targetList.notifyAll();
		}
	}

	/**
	 * Get the connection reserved exclusively for the GUI thread. No check-in/check-out is
	 * required.
	 * @param type The type of database connection desired.
	 * @return The reserved GUI connection.
	**/
	public static Connection getGUIConnection(MOVESDatabaseType type) {
		/*
		if(!CompilationFlags.USE_NONROAD && type == MOVESDatabaseType.NRDEFAULT) {
			return null;
		}
		*/
		synchronized(type) {
			if(!isInitialized(type)) {
				initialize(type);
			}
		}
		InternalConnectionPool targetPool = databasePools[type.getIndex()];
		targetPool.guiConnection = checkTimeout(type,targetPool.guiConnection);
		setTimeout(targetPool.guiConnection);
		return targetPool.guiConnection;
	}

	/**
	 * Check the inactivity timeout for a database connection, discarding and recreating
	 * it if the timeout period has expired.  The inactivity timeout is set as well.
	 * @param type The type of database connection desired.
	 * @param connection a database connection to be checked
	 * @return the database connection to actually be used
	**/
	static Connection checkTimeout(MOVESDatabaseType type, Connection connection) {
		synchronized(connectionTimeouts) {
			Long timeout = null;
			if(connection != null) {
				timeout = (Long)connectionTimeouts.get(connection);
			}
			if(timeout == null || timeout.longValue() <= System.currentTimeMillis()) {
				Connection newConnection = null;
				try {
					if(type == MOVESDatabaseType.OUTPUT) {
						if(outputDatabaseSelection.databaseName.trim().length() != 0) {
							newConnection = outputDatabaseSelection.openConnectionOrNull();
						}
					} else {
						newConnection = openPrivateConnection(type);
					}
				} catch(Exception e) {
					/**
					 * @explain An error occurred while attempting to reconnect to the database
					 * server after detecting a prior connection had been idle for a long time.
					**/
					Logger.logError(e,"Unable to open new connection for timed-out connection");
				}
				if(newConnection != null) {
					if(connection != null) {
						DatabaseUtilities.closeConnection(connection);
						connectionTimeouts.remove(connection);
					}
					connection = newConnection;
				}
			}
			setTimeout(connection);
		}
		return connection; // not necessarily the same object we came in with
	}

	/**
	 * Set the future inactivity timeout for a database connection.
	 * @param connection a database connection
	**/
	static void setTimeout(Connection connection) {
		if(connection != null) {
			synchronized(connectionTimeouts) {
				long timeout = System.currentTimeMillis() + CONNECTION_TIMEOUT_MILLIS;
				connectionTimeouts.put(connection,Long.valueOf(timeout));
			}
		}
	}

	/**
	 * Remove timeout data for a given connection.  Typically, this is done during
	 * a cleanup operation.
	 * @param connection a database connection
	**/
	static void removeTimeout(Connection connection) {
		if(connection != null) {
			synchronized(connectionTimeouts) {
				connectionTimeouts.remove(connection);
			}
		}
	}

	/**
	 * Open a new database connection to be exclusively owned by the caller.
	 * No timeouts are checked for the returned Connection, so it should be used
	 * quickly then closed.
	 * @param type The type of database connection desired.
	 * @throws ClassNotFoundException If a JDBC driver class isn't found.
	 * @throws SQLException If there is an error opening the database connection.
	 * @return The newly created connection.
	**/
	public static Connection openPrivateConnection(MOVESDatabaseType type)
			throws ClassNotFoundException, SQLException {
		if(type == MOVESDatabaseType.DEFAULT && customInputDatabase != null) {
			return customInputDatabase.openConnection();
		}
		return SystemConfiguration.getTheSystemConfiguration()
				.databaseSelections[type.getIndex()].openConnection();
	}

	/**
	 * Check if the connection pool is initialized for the specified type.
	 * @param type The database type to check for.
	 * @return If the specified type initialized.
	**/
	public static boolean isInitialized(MOVESDatabaseType type) {
		synchronized(type) {
			return databasePools[type.getIndex()] != null;
		}
	}

	/**
	 * Set the output database and database server name.
	 * @param serverName The output database server name.
	 * @param databaseName The output database name
	**/
	public static void setOutputDatabase(String serverName, String databaseName) {
		cleanup(MOVESDatabaseType.OUTPUT);
		outputDatabaseSelection.serverName = serverName;
		if(serverName == null || serverName.trim().length() == 0) {
			outputDatabaseSelection.serverName =
					SystemConfiguration.getTheSystemConfiguration().databaseSelections[
							MOVESDatabaseType.OUTPUT.getIndex()].serverName;
		}
		outputDatabaseSelection.databaseName = databaseName;
	}

	/** Call learnCreateTableStatements() for the DEFAULT and EXECUTION databases **/
	public static void learnCreateTableStatementsForDefaultAndExecutionDatabases() {
		Connection db = null;
		MOVESDatabaseType type = null;

		try {
			type = MOVESDatabaseType.DEFAULT;
			db = checkOutConnection(type);
			learnCreateTableStatements(db,defaultDatabaseCreateTableStatements,false);
			checkInConnection(type,db);
			db = null;

			/* NonRoad is part of the default database and is no longer separate
			if(CompilationFlags.USE_NONROAD) {
				type = MOVESDatabaseType.NRDEFAULT;
				db = checkOutConnection(type);
				learnCreateTableStatements(db,defaultDatabaseCreateTableStatements,false);
				checkInConnection(type,db);
				db = null;
			}
			*/

			type = MOVESDatabaseType.EXECUTION;
			db = checkOutConnection(type);
			learnCreateTableStatements(db,executionDatabaseCreateTableStatements,true);
			checkInConnection(type,db);
			db = null;
		} catch(Exception e) {
			/** @nonissue **/
			Logger.logException(LogMessageCategory.ERROR,e);
		} finally {
			if(db != null) {
				checkInConnection(type,db);
				db = null;
			}
		}
	}

	/**
	 * Obtain a "CREATE TABLE IF NOT EXISTS" statement for a table from the Execution database.
	 * @param tableName table to find
	 * @return SQL statement for creating the table
	**/
	public static String getExecutionCreateTableStatement(String tableName) {
		boolean shouldLog = false;
		/*
		if(tableName.equalsIgnoreCase("sourceTypeAgePopulation")
				|| tableName.equalsIgnoreCase("analysisYearVMT")) {
			shouldLog = true;
		}
		*/
		tableName = tableName.toLowerCase();
		String createStatement = (String)executionDatabaseCreateTableStatements.get(tableName);
		if(createStatement != null && createStatement.length() > 0) {
			if(shouldLog) {
				Logger.log(LogMessageCategory.INFO,tableName + " : " + createStatement);
			}
			return createStatement;
		}
		createStatement = "";

		Connection db = null;
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();

		try {
			db = checkOutConnection(MOVESDatabaseType.EXECUTION);
			sql = "SHOW CREATE TABLE " + tableName;
			query.open(db,sql);
			if(query.rs.next()) {
				createStatement = StringUtilities.replace(query.rs.getString(2),
						"CREATE TABLE `","CREATE TABLE IF NOT EXISTS `") + ";";
				executionDatabaseCreateTableStatements.put(tableName,createStatement);
			} else {
				Logger.log(LogMessageCategory.ERROR,"Unable to get CREATE TABLE for " + tableName);
			}
			query.close();
		} catch(Exception e) {
			/** @nonissue **/
			Logger.logException(LogMessageCategory.ERROR,e);
		} finally {
			query.onFinally();
			if(db != null) {
				checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
		}
		if(shouldLog) {
			Logger.log(LogMessageCategory.INFO,tableName + " : " + createStatement);
		}
		return createStatement;
	}

	/**
	 * Examine a database and generate a collection of "CREATE TABLE IF NOT EXISTS"
	 * statements that represent each table in the database.
	 * @param db Connection to the database to be examined.
	 * @param statements existing TreeMap that should hold table names and their
	 * "CREATE TABLE IF NOT EXISTS" statements.
	 * @param useInnoDB true to force the InnoDB database engine, false to use the default engine.
	**/
	public static void learnCreateTableStatements(Connection db,TreeMapIgnoreCase statements,boolean useInnoDB) {
		//statements.clear();

		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			TreeSet<String> tableNames = new TreeSet<String>();
			sql = "SHOW TABLES";
			query.open(db,sql);
			while(query.rs.next()) {
				tableNames.add(query.rs.getString(1));
			}
			query.close();

			for(Iterator<String> i=tableNames.iterator();i.hasNext();) {
				String t = (String)i.next();
				sql = "SHOW CREATE TABLE " + t;
				query.open(db,sql);
				if(query.rs.next()) {
					String createStatement = StringUtilities.replace(query.rs.getString(2),
							"CREATE TABLE `","CREATE TABLE IF NOT EXISTS `") + ";";
					t = t.toLowerCase();
					if(SQLRunner.allowInnoDB && useInnoDB) {
						createStatement = StringUtilities.replace(createStatement,"ENGINE=MyISAM","ENGINE=InnoDB");
					}

					statements.put(t,createStatement);
					//System.out.println("Learned create table for " + t);
				}
				query.close();
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to learn Create Table statements",sql);
		} finally {
			query.onFinally();
		}
	}

	/** Issue a FLUSH TABLES command to MySQL **/
	public static void flushTables() {
		try {
			Connection db = getGUIConnection(MOVESDatabaseType.DEFAULT);
			if(db != null) {
				SQLRunner.executeSQL(db,"FLUSH TABLES");
			}
		} catch(Exception e) {
			// Nothing to do here
		}
	}
}
