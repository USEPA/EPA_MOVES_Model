/**************************************************************************************************
 * @(#)ParallelSQLRunner.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.concurrent.*;
import gov.epa.otaq.moves.common.graph.*;
import gov.epa.otaq.moves.common.graph.sql.*;

/**
 * Execute multiple SQL statements in parallel, using the graph classes and a thread pool.
 *
 * @author		Wesley Faler
 * @version		2010-05-24
**/
public class ParallelSQLRunner {
	/** source of database connections **/
	DatabaseSelection dbSelection;
	/** maximum number of SQL statements to be run concurrently **/
	int maximumConcurrentStatements;
	/** queue of exceptions experienced by executing threads **/
	ArrayList<Exception> exceptions = new ArrayList<Exception>();
	/** True if there were ever queued exceptions, even if they have been handled **/
	boolean hadExceptions = false;
	/** Statement graph **/
	GraphHolder graph = null;
	/** Current scheduler for the graph, if any **/
	GraphScheduler scheduler = null;
	/** Open connections, ready to be used **/
	ArrayList<Connection> connections = new ArrayList<Connection>();
	/** Thread pool service **/
	ExecutorService sqlExecutor = null;
	/** Currently running tasks **/
	ArrayList<SQLTask> tasks = new ArrayList<SQLTask>();

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
		ParallelSQLRunner owner;

		public SQLTask(ParallelSQLRunner ownerToUse) {
			owner = ownerToUse;
		}

		public void run() {
			INode node = null;
			Connection db = null;
			boolean hasOpenStatement = false;
			String sql = "";
			try {
				try {
					while(true) {
						synchronized(owner.exceptions) {
							if(owner.hadExceptions) {
								return;
							}
						}
						node = owner.scheduler.getNextNodeToDo(null,node);
						if(node == null) { // If there is nothing more to do, then quit
							return;
						}
						synchronized(owner.exceptions) {
							if(owner.hadExceptions) {
								return;
							}
						}
						if(node instanceof SQLNode) {
							sql = node.toString();
							db = owner.checkOutConnection();
							owner.stats.statementStarting();
							hasOpenStatement = true;
//System.out.println("Running: " + sql);
							SQLRunner.executeSQL(db,sql);
							hasOpenStatement = false;
							owner.stats.statementDone();
							owner.checkInConnection(db); db = null;
						}
					}
				} catch(Exception e) {
//					e.printStackTrace();
					synchronized(owner.exceptions) {
						owner.exceptions.add(e);
						owner.hadExceptions = true;
						System.out.println(e.getMessage());
						System.out.println("ERROR executing: " + sql);
					}
					return;
				}
			} finally {
				if(hasOpenStatement) {
					hasOpenStatement = false;
					owner.stats.statementDone();
				}
				if(db != null) {
					owner.checkInConnection(db); db = null;
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
	 * @dbSelectionToUse source of database connections
	 * @maximumConcurrentStatementsToUse maximum number of SQL statements to be run concurrently, defaults to 2 if <= 0
	**/
	public ParallelSQLRunner(DatabaseSelection dbSelectionToUse, int maximumConcurrentStatementsToUse) {
		dbSelection = dbSelectionToUse;
		maximumConcurrentStatements = maximumConcurrentStatementsToUse;
		if(maximumConcurrentStatements <= 0) {
			maximumConcurrentStatements = 2;
		}
	}

	/**
	 * Clear any pending SQL statements
	**/
	public void clear() {
		graph = null;
		scheduler = null;
	}

	/**
	 * Add a SQL statement to the graph, executing the statements
	 * if enough have been accumulated.
	 * @param sql statement to be added
	 * @throws Exception if any statement has a problem
	**/
	public void add(String sql) throws Exception {
		if(graph == null) {
			graph = new GraphHolder();
		}
		SQLNode node = new SQLNode(graph.linearTail,sql);
		try {
			graph.add(node);
		} catch(NullPointerException e) {
			System.out.println(sql);
			throw e;
		}
		if(graph.nodeCount >= 50) {
			execute();
		}
	}

	/**
	 * Executes the graph, waiting for completion.
	 * @throws Exception if any statement has a problem
	**/
	public void execute() throws Exception {
		throwQueuedException();
		if(graph == null) {
			return;
		}
//System.out.println("PSR.execute creating debug");
//graph.generateGraphVizFile(new File("testdata/graph/Worker.dot"),false);
//graph.generateGraphVizFile(new File("testdata/graph/WorkerLinear.dot"),true);
//System.out.println("PSR.execute #1 building scheduler");
		scheduler = new GraphScheduler(graph);
		hadExceptions = false;
		exceptions.clear();
		tasks.clear();

		// Ensure we have database connections already pooled
		synchronized(connections) {
			while(connections.size() < maximumConcurrentStatements) {
//System.out.println("PSR.execute #2 adding connection");
				checkInConnection(checkOutConnectionCore());
			}
		}

		// Create the thread pool
		if(sqlExecutor == null) {
			sqlExecutor = Executors.newFixedThreadPool(maximumConcurrentStatements);
		}

		// Initiate the thread pool
		for(int i=0;i<maximumConcurrentStatements;i++) {
			SQLTask t = new SQLTask(this);
			tasks.add(t);
		}

		// Wait for there to be no more work to do
		synchronized(this) {
			synchronized(tasks) {
				for(Iterator<SQLTask> i=tasks.iterator();i.hasNext();) {
					sqlExecutor.execute(i.next());
				}
			}
			while(true) {
//System.out.println("PSR.execute #3 waiting");
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
		graph = null;
		scheduler = null;
//System.out.println("PSR.execute #Done");
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
//System.out.println("PSR.internalClose #1");
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
				DatabaseUtilities.closeConnection(db);
			}
			connections.clear();
		}

		// There are no more users of the graph or scheduler, so get rid of them.
		graph = null;
		scheduler = null;
//System.out.println("PSR.internalClose #Done");
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
		Connection db = dbSelection.openConnection();
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
