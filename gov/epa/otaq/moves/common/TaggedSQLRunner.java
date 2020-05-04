/**************************************************************************************************
 * @(#)TaggedSQLRunner.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.concurrent.*;

/**
 * Execute multiple SQL statements in parallel using a thread pool.
 *
 * @author		Wesley Faler
 * @version		2014-07-23
**/
public class TaggedSQLRunner {
	/** Source and sink for database connections **/
	public static interface ConnectionProvider {
		/**
		 * Obtain a connection.
		 * @returns a Connection to be used or null upon error
		**/
		public Connection checkOutConnection();

		/**
		 * Return a connection.
		 * @param c Connection object to be returned, never null.
		**/
		public void checkInConnection(Connection c);
	}

	/** Execute an override instead of SQL **/
	public static interface OverrideHandler {
		/**
		 * Attempt to do custom calculations instead of a SQL statement.
		 * @param db database connection
		 * @param sql sql statement
		 * @param data1 external data that may be meaningful to the override handler
		 * @param data2 external data that may be meaningful to the override handler
		 * @return true if the statement was handled, false to execute the SQL
		 * @throws Exception if anything goes wrong
		**/
		public boolean onOverrideSQL(Connection db,String sql,Object data1,Object data2) throws Exception;
	}

	static class StatementEntry {
		String sql;
		Object data1;
		Object data2;

		StatementEntry(String sqlToUse, Object data1ToUse, Object data2ToUse) {
			sql = sqlToUse;
			data1 = data1ToUse;
			data2 = data2ToUse;
		}
	}

	/** maximum number of connections to be used **/
	public int absoluteMaximumConnections = 4;
	/** source of database connections **/
	ConnectionProvider connectionProvider;
	/** handles optional overrides **/
	OverrideHandler overrideHandler;
	/** queue of exceptions experienced by executing threads **/
	ArrayList<Exception> exceptions = new ArrayList<Exception>();
	/** True if there were ever queued exceptions, even if they have been handled **/
	boolean hadExceptions = false;
	/** Open connections, ready to be used **/
	ArrayList<Connection> connections = new ArrayList<Connection>();
	/** Thread pool service **/
	ExecutorService sqlExecutor = null;
	/** Currently running tasks **/
	ArrayList<SQLTask> tasks = new ArrayList<SQLTask>();
	/** All queued statements, keyed by context **/
	TreeMap<String,ArrayList<StatementEntry>> allStatements = new TreeMap<String,ArrayList<StatementEntry>>();
	/** All queued contexts **/
	TreeSet<String> contexts = new TreeSet<String>();
	/** true to override contexts and use the maximum number of contexts **/
	public boolean maximizeConcurrency = false;

	public static class ConcurrencyStatistics {
		public int count = 0;
		public int maxCount = 0;

		public int totalCount = 0;
		public int observations = 0;

		public synchronized void statementStarting() {
			count++;
			if(count > maxCount) {
				maxCount = count;
			}
			totalCount += count;
			observations++;
		}

		public synchronized void statementDone() {
			count--;
		}

		public void print() {
			Logger.log(LogMessageCategory.INFO,"Statements done: " + observations);
			if(observations > 0) {
				Logger.log(LogMessageCategory.INFO,"Avg. Concurrent Statements: " + ((double)totalCount)/((double)observations));
			}
			Logger.log(LogMessageCategory.INFO,"Max. Concurrent Statements: " + maxCount);
		}
	}

	public ConcurrencyStatistics stats = new ConcurrencyStatistics();

	static class SQLTask implements Runnable {
		TaggedSQLRunner owner;
		String context;

		public SQLTask(TaggedSQLRunner ownerToUse, String contextToUse) {
			owner = ownerToUse;
			context = contextToUse;
		}

		public void run() {
			if(context == null) {
				return;
			}
			Connection db = null;
			boolean hasOpenStatement = false;
			StatementEntry entry = null;
			String sql = "";
			try {
				try {
					int howManyStatementsDone = 0;
					int howManyStatementsTotal = -1;
					while(true) {
						if(db == null) {
							db = owner.checkOutConnection();
						}
						synchronized(owner.exceptions) {
							if(owner.hadExceptions) {
								return;
							}
						}
						entry = null;
						sql = "";
						synchronized(owner.allStatements) {
							ArrayList<StatementEntry> queue = owner.allStatements.get(context);
							if(queue != null && queue.size() > 0) {
								if(howManyStatementsTotal < 0) {
									howManyStatementsTotal = queue.size();
								}
								entry = queue.get(0);
								sql = entry.sql;
								howManyStatementsDone++;
								Logger.log(LogMessageCategory.INFO,"Running Statement: " + context + ": " + howManyStatementsDone + " of " + howManyStatementsTotal);
								queue.remove(0);
							}
						}
						if(entry == null || sql == null || sql.length() <= 0) { // If there is nothing more to do, then quit
							return;
						}
						synchronized(owner.exceptions) {
							if(owner.hadExceptions) {
								return;
							}
						}
						owner.stats.statementStarting();
						hasOpenStatement = true;
						//Logger.log(LogMessageCategory.INFO,"Running: " + hashCode() + ": " + context + ": " + StringUtilities.substring(sql,0,100));
						if(owner.overrideHandler != null) {
							if(owner.overrideHandler.onOverrideSQL(db,sql,entry.data1,entry.data2)) {
								entry = null;
							}
						}
						if(entry != null) {
							SQLRunner.executeSQL(db,sql);
						}
						hasOpenStatement = false;
						owner.stats.statementDone();
					}
				} catch(Exception e) {
//					e.printStackTrace();
					synchronized(owner.exceptions) {
						owner.exceptions.add(e);
						owner.hadExceptions = true;
						Logger.log(LogMessageCategory.INFO,"" + hashCode() + ": " + context + ": " + e.getMessage());
						Logger.log(LogMessageCategory.INFO,"ERROR executing: " + hashCode() + ": " + context + ": " + sql);					}
					return;
				}
			} finally {
				if(hasOpenStatement) {
					hasOpenStatement = false;
					owner.stats.statementDone();
				}
				if(db != null) {
					owner.checkInConnection(db);
					db = null;
				}
				synchronized(owner.tasks) {
					owner.tasks.remove(this);
				}
				synchronized(owner) {
					owner.notifyAll();
				}
			}
		}
	}

	/**
	 * Constructor
	 * @param connectionProviderToUse provide source and sink for connections
	**/
	public TaggedSQLRunner(ConnectionProvider connectionProviderToUse,
			OverrideHandler overrideHandlerToUse) {
		connectionProvider = connectionProviderToUse;
		overrideHandler = overrideHandlerToUse;
	}

	/**
	 * Clear any pending SQL statements
	**/
	public void clear() {
		allStatements.clear();
		contexts.clear();
	}

	/**
	 * Add a SQL statement to the graph, executing the statements
	 * if enough have been accumulated.
	 * @param context logical context in which the statement should be enqueued
	 * @param sql statement to be added
	 * @throws Exception if any statement has a problem
	**/
	public void add(String context, String sql) throws Exception {
		add(context,sql,null,null);
	}

	/**
	 * Add a SQL statement to the graph, executing the statements
	 * if enough have been accumulated.
	 * @param context logical context in which the statement should be enqueued
	 * @param sql statement to be added
	 * @param data1 external data that may be meaningful to the override handler
	 * @param data2 external data that may be meaningful to the override handler
	 * @throws Exception if any statement has a problem
	**/
	public void add(String context, String sql, Object data1, Object data2) throws Exception {
		if(maximizeConcurrency) {
			context = "#MAX";
		}
		if(context == null) {
			context = "#ALL";
		} else {
			context = context.toUpperCase();
		}
		try {
			contexts.add(context);
			ArrayList<StatementEntry> queue = allStatements.get(context);
			if(queue == null) {
				queue = new ArrayList<StatementEntry>();
				allStatements.put(context,queue);
			}
			queue.add(new StatementEntry(sql,data1,data2));
			//Logger.log(LogMessageCategory.INFO,"Queued: " + context + ": " + StringUtilities.substring(sql,0,100));
		} catch(Exception e) {
			Logger.log(LogMessageCategory.INFO,sql);
			throw e;
		}
	}

	/**
	 * Executes the queued statements.
	 * @throws Exception if any statement has a problem
	**/
	public void execute() throws Exception {
		throwQueuedException();
		hadExceptions = false;
		exceptions.clear();
		tasks.clear();
		if(contexts.size() <= 0 || allStatements.size() <= 0) {
			return;
		}

		int maximumConcurrentStatements = Math.min(contexts.size(),absoluteMaximumConnections);
		if(maximizeConcurrency) {
			maximumConcurrentStatements = absoluteMaximumConnections;
		}
		// Ensure we have database connections already pooled
		synchronized(connections) {
			while(connections.size() < maximumConcurrentStatements) {
				checkInConnection(checkOutConnectionCore());
			}
		}

		// Create the thread pool
		if(sqlExecutor == null) {
			sqlExecutor = Executors.newFixedThreadPool(maximumConcurrentStatements);
		}

		// Initiate the thread pool
		if(maximizeConcurrency) {
			for(int i=0;i<absoluteMaximumConnections;i++) {
				SQLTask t = new SQLTask(this,"#MAX");
				tasks.add(t);
			}
		} else {
			for(String context : contexts) {
				//Logger.log(LogMessageCategory.INFO,"Adding concurrent context: " + context);
				SQLTask t = new SQLTask(this,context);
				tasks.add(t);
			}
		}
		contexts.clear();

		// Wait for there to be no more work to do
		synchronized(this) {
			synchronized(tasks) {
				for(Iterator<SQLTask> i=tasks.iterator();i.hasNext();) {
					sqlExecutor.execute(i.next());
				}
			}
			while(true) {
				wait(100);
				synchronized(tasks) {
					if(tasks.isEmpty()) {
						break;
					}
				}
			}
		}
		// Throw any exceptions that happened on child threads
		try {
			throwQueuedException();
		} catch(Exception e) {
			internalClose();
			throw e;
		}

		// Start clean with the next set of add'd statements
		allStatements.clear();
		contexts.clear();
	}

	/**
	 * Close all connections and statements without waiting for completion of all queued statements, though
	 * any statements in progress will be waited upon.
	 * @throws Exception if any statement has a problem
	**/
	public void close() throws Exception {
		internalClose();
		throwQueuedException();
	}

	/** Called to totally stop any processing and close all database connections. **/
	public void onFinally() {
		internalClose();
	}

	/**
	 * Stop all processing and close all database connections.
	**/
	private void internalClose() {
		// Stop the thread pool
		if(sqlExecutor != null) {
			sqlExecutor.shutdown();
			try {
				if(!sqlExecutor.awaitTermination(60,TimeUnit.SECONDS)) {
					sqlExecutor.shutdownNow();
					sqlExecutor.awaitTermination(30,TimeUnit.SECONDS);
				}
			} catch(InterruptedException e) {
				// Nothing to do here
			}
			sqlExecutor = null;
		}

		// Close all database connections
		synchronized(connections) {
			for(Iterator<Connection> i=connections.iterator();i.hasNext();) {
				Connection db = i.next();
				connectionProvider.checkInConnection(db);
			}
			connections.clear();
		}

		// Remove any statements
		allStatements.clear();
		contexts.clear();
	}

	/**
	 * Throw the first queued exception, if any.
	 * @throws the first queued exception, if any.
	**/
	private void throwQueuedException() throws Exception {
		synchronized(exceptions) {
			if(!exceptions.isEmpty()) {
				Exception e = exceptions.remove(0);
				throw e;
			}
		}
	}

	/**
	 * Open a connection to the database, recycling a previously open connection if possible.
	 * @throws Exception if unable to obtain a database connection
	**/
	public Connection checkOutConnection() throws Exception {
		synchronized(connections) {
			if(connections.size() > 0) {
				return connections.remove(0);
			}
		}
		return checkOutConnectionCore();
	}

	/**
	 * Open a connection to the database, recycling a previously open connection if possible.
	 * @throws Exception if unable to obtain a database connection
	**/
	private Connection checkOutConnectionCore() throws Exception {
		Connection db = connectionProvider.checkOutConnection();
		if(db == null) {
			throw new SQLException("Did not get connection from DatabaseSelection.openConnection");
		}
		return db;
	}

	/**
	 * Return an open connection to the pool of available connections.
	 * @param db connection to be returned
	**/
	public void checkInConnection(Connection db) {
		if(db != null) {
			synchronized(connections) {
				connections.add(db);
			}
		}
	}
}
