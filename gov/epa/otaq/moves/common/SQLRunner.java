/**************************************************************************************************
 * @(#)SQLRunner.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import java.util.*;

/**
 * Central location for performing SQL queries. SQLRunner uses the PerformanceProfiler to collect
 * query execution time and frequency statistics.
 *
 * @author		Wesley Faler
 * @author		EPA Mitch C. minor mod to add SQL exception trace printout
 * @version		2015-09-11
**/
public class SQLRunner {
	/** true to force use of InnoDB tables rather than MyISAM **/
	public static boolean allowInnoDB = false;

	/** Set of database connections that should use InnoDB tables **/
	private static TreeSet<Integer> innodbConnections = new TreeSet<Integer>();

	/** Clear all InnoDB connections, causing all open connections to prefer MyISAM tables. **/
	public static void resetInnoDBConnections() {
		innodbConnections.clear();
	}

	/**
	 * Flag a data connection as preferring InnoDB tables over MyISAM tables.
	 * @param db database connection
	**/	
	public static void addInnoDBConnection(Connection db) {
		if(allowInnoDB) {
			//System.out.println("INNODB: Adding db " + db.hashCode());
			innodbConnections.add(Integer.valueOf(db.hashCode()));
		}
	}

	/**
	 * Remove a database connection's preference for InnoDB tables.
	 * @param db database connection
	**/
	public static void removeInnoDBConnection(Connection db) {
		//System.out.println("INNODB: Removing db " + db.hashCode());
		innodbConnections.remove(Integer.valueOf(db.hashCode()));
	}

	/**
	 * Check a database connection's preference for InnoDB or MyISAM tables.
	 * @param db database connection
	 * @return true if InnoDB tables should be used, false if MyISAM tables should be used.
	**/
	public static boolean isInnoDBConnection(Connection db) {
		if(!allowInnoDB) {
			return false;
		}
		boolean result = innodbConnections.contains(Integer.valueOf(db.hashCode()));
		if(result) {
			//System.out.println("INNODB: Using db " + db.hashCode());
		}
		return result;
	}

	public static class Query {
		/* Sample usage:
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			sql = ....
			query.open(sql);
			while(query.rs.next()) {
				... query.rs.getInt(...)
			}
			query.close();
		} catch(SQLException e) {
			query.onException(e,"Unable to ...",sql);
		} finally {
			query.onFinally();
		}
		*/
		public Statement statement = null;
		public ResultSet rs = null;
		public boolean isLargeResultSet = false;

		public Query() {
		}

		public Query(boolean isLargeResultSetToUse) {
			isLargeResultSet = isLargeResultSetToUse;
		}

		public void open(Connection db, String sql) throws SQLException {
			if(isLargeResultSet) {
				openLargeResultSet(db,sql);
			} else {
				statement = db.createStatement();
			}
			rs = SQLRunner.executeQuery(statement,sql);
		}

		public void openLargeResultSet(Connection db, String sql) throws SQLException {
			statement = db.createStatement(ResultSet.TYPE_FORWARD_ONLY,ResultSet.CONCUR_READ_ONLY);
			statement.setFetchSize(Integer.MIN_VALUE);
			rs = SQLRunner.executeQuery(statement,sql);
		}

		public void onException(SQLException e, String text, String sql) {
			/** @nonissue **/
			Logger.logError(e,text + "\n" + sql);
			close();
		}

		public void close() {
			if(rs != null) {
				try {
					rs.close();
				} catch(SQLException e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(SQLException e) {
					// Nothing to do here
				}
				statement = null;
			}
		}

		public void onFinally() {
			close();
		}
	}

	/** Standard constructor. **/
	public SQLRunner() {
	}

	/**
	 * Examine an exception and execute FLUSH TABLES if the exception appears that it would benefit
	 * from such
	 * @param	db an open Connection object.
	 * @param	error an exception thrown while executing SQL
	 * @param	attemptNumber how many previous attempts at executing the SQL have been made
	 * @return	true if FLUSH TABLES was performed and the offending SQL should be reattempted
	**/
	private static boolean wouldFlushTablesHelp(Connection db,SQLException error,int attemptNumber) {
		if(attemptNumber >= 1) {
			/**
			 * @explain A database error was encountered and an automatic reattempt after issuing
			 * a FLUSH TABLES statement was done, but was ultimately unsuccessful.
			**/
			Logger.log(LogMessageCategory.WARNING,"Too many retries of FLUSH TABLES, giving up.");
			return false;
		}
		boolean shouldDoFlushTables = false;
		String message = error.getMessage().toUpperCase();
		if(!shouldDoFlushTables &&
				(message.indexOf(".FRM") >= 0 || message.indexOf(".MYD") >= 0
				|| message.indexOf(".MYI") >= 0
				|| message.indexOf("READ ONLY") >= 0)) {
			// Exception contains a file name, likely FLUSH TABLES will remove a lock on the file.
			shouldDoFlushTables = true;
		}
		// Check more exception types here
		// .....
		if(!shouldDoFlushTables) {
			return false;
		}
		/**
		 * @explain A database error was encountered and an automatic reattempt after issuing
		 * a FLUSH TABLES statement is about to begin.
		**/
		Logger.log(LogMessageCategory.DEBUG,
				"SQL exeception might benefit from FLUSH TABLES: "
				+ error.getMessage());
		PreparedStatement statement = null;
		try {
			statement = db.prepareStatement("FLUSH TABLES");
			statement.executeUpdate();
			return true;
		} catch(SQLException e) {
			/**
			 * @explain While attempting to automatically recover from a database error issuing
			 * a FLUSH TABLES command, another error occurred.  The database server software
			 * itself may be offline.
			**/
			Logger.log(LogMessageCategory.ERROR,
					"Exception occurred while trying to recover using FLUSH TABLES: "
					+ e.getMessage());
			return false;
		} finally {
			if(statement != null) {
				try {
					statement.close();
				} catch(SQLException e) {
					// Nothing can be done here
				}
				statement = null;
			}
		}
	}

	/**
	 * Executes a prepared SQL statement and collects timing statistics
	 * @param statement Prepared SQL statement.
	 * @param sql Original SQL to the prepared statement for identifying timing statistics
	 * @return count of rows affected
	 * @throws SQLException Thrown by prepared statement executeUpdate()
	**/
	public static int executeSQL(PreparedStatement statement, String sql) throws SQLException {
		int attemptNumber = 0;
		while(true) {
			try {
				PerformanceProfiler.start(sql);
				return statement.executeUpdate();
			} catch(SQLException e) {
				if(!wouldFlushTablesHelp(statement.getConnection(),e,attemptNumber)) {
					//System.out.println(sql);
					throw e;
				}
			} finally {
				PerformanceProfiler.stop(sql);
			}
			attemptNumber++;
		}
	}

	/**
	 * Executes a prepared SQL statement and collects timing statistics
	 * @param statement Prepared SQL statement.
	 * @param sql Original SQL to the prepared statement for identifying timing statistics
	 * @return SQL result set
	 * @throws SQLException Thrown by prepared statement executeQuery()
	**/
	public static ResultSet executeQuery(PreparedStatement statement, String sql)
			throws SQLException {
		int attemptNumber = 0;
		while(true) {
			try {
				PerformanceProfiler.start(sql);
				return statement.executeQuery();
			} catch(SQLException e) {
				if(!wouldFlushTablesHelp(statement.getConnection(),e,attemptNumber)) {
					//System.out.println(sql);
					throw e;
				}
			} finally {
				PerformanceProfiler.stop(sql);
			}
			attemptNumber++;
		}
	}

	/**
	 * Executes a prepared SQL statement and collects timing statistics
	 * @param statement Prepared SQL statement.
	 * @param sql Original SQL to the prepared statement for identifying timing statistics
	 * @return false if the statement failed
	 * @throws SQLException Thrown by prepared statement executeUpdate()
	**/
	public static boolean execute(PreparedStatement statement, String sql) throws SQLException {
		int attemptNumber = 0;
		while(true) {
			try {
				PerformanceProfiler.start(sql);
				return statement.execute();
			} catch(SQLException e) {
				if(!wouldFlushTablesHelp(statement.getConnection(),e,attemptNumber)) {
					//System.out.println(sql);
					throw e;
				}
			} finally {
				PerformanceProfiler.stop(sql);
			}
			attemptNumber++;
		}
	}

	/**
	 * Executes an SQL statement on a database and collects timing statistics.
	 * @param	db an open Connection object.
	 * @param	sql the statement to run.
	 * @return count of rows affected
	 * @throws	SQLException on error from any of the database operations.
	**/
	public static int executeSQL(Connection db, String sql) throws SQLException {
		String sqlLowerCase = sql.toLowerCase().trim();
		if(sqlLowerCase.startsWith("flush tables")) {
			return 0;
		}
		if(sqlLowerCase.indexOf("select ") >= 0) {
			/*
			if(sqlLowerCase.startsWith("create table") && sqlLowerCase.indexOf(" like ") < 0) {
				return executeCreateSelect(db,sql);
			} else if(sqlLowerCase.startsWith("insert")) {
				return executeInsertSelect(db,sql,0,true);
			}
			*/
		} else if(sqlLowerCase.startsWith("create table") && sqlLowerCase.indexOf(" like ") < 0) {
			String newSQL = addEngineAndFlags(sql,sqlLowerCase,isInnoDBConnection(db));
			if(newSQL != null) {
				sql = newSQL;
				sqlLowerCase = sql.toLowerCase().trim();
			}
		} else if(sqlLowerCase.startsWith("delete from")) {
			// After deleting many records, it is best to optimize and analyze
			// the table.
			return executeDelete(db,sql);
		}

		if(db == null) {
			return 0;
		}
		return executeSQLCore(db,sql);
	}

	/**
	 * Executes an SQL statement on a database and collects timing statistics.
	 * @param	db an open Connection object.
	 * @param	sql the statement to run.
	 * @return the number of affected records
	 * @throws	SQLException on error from any of the database operations.
	**/
	public static int executeSQLCore(Connection db, String sql) throws SQLException {
		if(db == null) {
			//System.out.println(sql);
			return 200000;
		}
		int attemptNumber = 0;
		while(true) {
			Statement statement = db.createStatement();
			try {
				PerformanceProfiler.start(sql);
				statement.execute(sql);
				return statement.getUpdateCount();
			} catch(SQLException e) {
				if(!wouldFlushTablesHelp(db,e,attemptNumber)) {
					//System.out.println("SQL Failed: " + sql);
					throw e;
				}
			} finally {
				PerformanceProfiler.stop(sql);
				statement.close();
			}
			attemptNumber++;
		}
	}

	/**
	 * Executes SQL statement on a database, collects timing statistics, and returns a single
	 * ResultSet.
	 * @param	db an open Connection object.
	 * @param	sql the statement to run.
	 * @return The opened result set. This will never be null.
	 * @throws	SQLException on error from any of the database operations.
	**/
	public static ResultSet executeQuery(Connection db, String sql) throws SQLException {
		Statement statement = db.createStatement();
		return executeQuery(statement,sql);
	}

	/**
	 * Executes unprepared SQL statement, collects timing statistics, and returns a result set.
	 * @param	statement an open and configured Statement object
	 * @param	sql the statement to run.
	 * @return The opened result set. This will never be null.
	 * @throws	SQLException on error from any of the database operations.
	**/
	public static ResultSet executeQuery(Statement statement, String sql) throws SQLException {
		int attemptNumber = 0;
		while(true) {
			try {
				PerformanceProfiler.start(sql);
				return statement.executeQuery(sql);
			} catch(SQLException e) {
				if(!wouldFlushTablesHelp(statement.getConnection(),e,attemptNumber)) {
					//System.out.println(sql);
					throw e;
				}
			} finally {
				PerformanceProfiler.stop(sql);
//				statement.close();
			}
			attemptNumber++;
		}
	}

	/**
	 * Executes SQL statement on a database, collects timing statistics, and returns a double
	 * from the value of the first column of the first row, or 0 if no data is returned.
	 * @param	db an open Connection object.
	 * @param	sql the statement to run.
	 * @return The value of the first column in the first row of the results, or 0 if no rows.
	 * @throws	SQLException on error from any of the database operations.
	**/
	public static double executeScalar(Connection db, String sql) throws SQLException {
		Statement statement = db.createStatement();
		return executeScalar(statement,sql);
	}

	/**
	 * Executes unprepared SQL statement, collects timing statistics, and returns a double
	 * from the value of the first column of the first row, or 0 if no data is returned.
	 * @param	statement an open and configured Statement object
	 * @param	sql the statement to run.
	 * @return The value of the first column in the first row of the results, or 0 if no rows.
	 * @throws	SQLException on error from any of the database operations.
	**/
	public static double executeScalar(Statement statement, String sql) throws SQLException {
		int attemptNumber = 0;
		while(true) {
			ResultSet rs = null;
			try {
				PerformanceProfiler.start(sql);
				rs = statement.executeQuery(sql);
				if(rs != null) {
					if(rs.next()) {
						return rs.getDouble(1);
					} else {
						return 0;
					}
				} else {
					return 0;
				}
			} catch(SQLException e) {
				if(!wouldFlushTablesHelp(statement.getConnection(),e,attemptNumber)) {
					//System.out.println(sql);
					throw e;
				}
			} finally {
				PerformanceProfiler.stop(sql);
				statement.close();
				if(rs != null) {
					rs.close();
				}
			}
			attemptNumber++;
		}
	}

	/**
	 * Executes SQL statement on a database, collects timing statistics, and returns a double
	 * from the value of the first column of the first row, or 0 if no data is returned.
	 * @param	db an open Connection object.
	 * @param	sql the statement to run.
	 * @return The value of the first column in the first row of the results, or 0 if no rows.
	 * @throws	SQLException on error from any of the database operations.
	**/
	public static String executeScalarString(Connection db, String sql) throws SQLException {
		Statement statement = db.createStatement();
		return executeScalarString(statement,sql);
	}

	/**
	 * Executes unprepared SQL statement, collects timing statistics, and returns a double
	 * from the value of the first column of the first row, or 0 if no data is returned.
	 * @param	statement an open and configured Statement object
	 * @param	sql the statement to run.
	 * @return The value of the first column in the first row of the results, or 0 if no rows.
	 * @throws	SQLException on error from any of the database operations.
	**/
	public static String executeScalarString(Statement statement, String sql) throws SQLException {
		int attemptNumber = 0;
		while(true) {
			ResultSet rs = null;
			try {
				PerformanceProfiler.start(sql);
				rs = statement.executeQuery(sql);
				if(rs != null) {
					if(rs.next()) {
						return rs.getString(1);
					} else {
						return null;
					}
				} else {
					return null;
				}
			} catch(SQLException e) {
				if(!wouldFlushTablesHelp(statement.getConnection(),e,attemptNumber)) {
					//System.out.println(sql);
					throw e;
				}
			} finally {
				PerformanceProfiler.stop(sql);
				statement.close();
				if(rs != null) {
					rs.close();
				}
			}
			attemptNumber++;
		}
	}

	/**
	 * Add an explicit ORDER BY clause to any SELECT statement containing a GROUP BY clause.
	 * Existing ORDER BY clauses are removed and replaced with the contents of the GROUP BY clause.
	 * This makes the resulting statement predictable in its sequence and compatible with
	 * INSERT..SELECT..LIMIT optimizations.
	 * Syntax examples:
	 *		select * from alpha
	 *			yields: null
	 *		select * from alpha group by a, b
	 *			yields: select * from alpha group by a, b order by a, b
	 *		select * from alpha group by a, b limit 1,4
	 *			yields: select * from alpha group by a, b order by a, b limit 1,4
	 *		select * from alpha group by a, b order by a
	 *			yields: select * from alpha group by a, b order by a, b
	 *		select * from alpha group by a, b order by a limit 1,4
	 *			yields: select * from alpha group by a, b order by a, b limit 1,4
	 *		select * from alpha group by a, b having a+b>3 order by a limit 1,4
	 *			yields: select * from alpha group by a, b having a+b>3 order by a, b limit 1,4
	 * @param sql sql to be examined, natural case
	 * @param sqlLowerCase lowercase version of SQL, used to improve performance
	 * @return modified sql statement (in its natural case) or null if no modifications were made
	**/
	public static String addOrderToGroup(String sql, String sqlLowerCase) {
		if(sqlLowerCase.indexOf("group by") < 0) {
			return null; // no GROUP BY, so no changes
		}

		sql = sql.trim();
		if(sql.endsWith(";")) {
			while(sql.endsWith(";")) {
				sql = sql.substring(0,sql.length()-1).trim();
			}
			sqlLowerCase = sql.toLowerCase();
		} else {
			sqlLowerCase = sqlLowerCase.trim();
		}
		int groupByIndex = sqlLowerCase.indexOf("group by");
		if(groupByIndex < 0) {
			return null; // no GROUP BY, so no changes
		}
		int orderByIndex = sqlLowerCase.indexOf("order by");
		int limitIndex = sqlLowerCase.indexOf("limit ");
		String limitClause = "";
		if(limitIndex > groupByIndex) {
			limitClause = sql.substring(limitIndex);
			sql = sql.substring(0,limitIndex).trim();
			sqlLowerCase = sql.toLowerCase();
		}
		if(orderByIndex > groupByIndex) {
			sql = sql.substring(0,orderByIndex).trim();
			sqlLowerCase = sql.toLowerCase();
		}
		String groupByClause = sql.substring(groupByIndex+8/*"group by".length()*/).trim();
		int havingIndex = groupByClause.toLowerCase().indexOf("having ");
		if(havingIndex > 0) {
			groupByClause = groupByClause.substring(0,havingIndex).trim();
		}
		String orderByClause = " order by " + groupByClause;
		if(limitClause.length() > 0) {
			return sql + orderByClause + " " + limitClause;
		} else {
			return sql + orderByClause;
		}
	}

	/**
	 * Add "ENGINE=MyISAM" (or "ENGINE=InnoDB"), "DELAY_KEY_WRITE=1", and "CHARSET=uft8" clauses to
	 * a CREATE TABLE statement	of the form "create table (xxx) ENGINE=MyISAM DELAY_KEY_WRITE=1".
	 * @param sql sql to be examined, natural case.
	 * @param sqlLowerCase lowercase version of SQL, used to improve performance.
	 * @param useInnoDB true to force InnoDB tables, false to use MyISAM.
	 * @return modified sql statement (in its natural case) or null if no modifications were made.
	**/
	public static String addEngineAndFlags(String sql, String sqlLowerCase, boolean useInnoDB) {
		sql = sql.trim();
		boolean hasSemicolon = sql.endsWith(";");
		if(hasSemicolon) {
			while(sql.endsWith(";")) {
				sql = sql.substring(0,sql.length()-1).trim();
			}
			sqlLowerCase = sql.toLowerCase();
		}
		boolean madeChanges = false;
		if(useInnoDB && sqlLowerCase.indexOf("engine=myisam") >= 0) {
			sql = StringUtilities.replace(sql,"ENGINE=MyISAM","ENGINE=InnoDB");
			madeChanges = true;
			//System.out.println("INNODB: Changing MyISAM to InnoDB");
		}
		if(sqlLowerCase.indexOf("engine=") < 0) {
			if(useInnoDB) {
				sql += " ENGINE=InnoDB";
				//System.out.println("INNODB: Adding ENGINE=InnoDB");
			} else {
				sql += " ENGINE=MyISAM";
			}
			madeChanges = true;
		}
		if(sqlLowerCase.indexOf("delay_key_write=") < 0) {
			sql += " DELAY_KEY_WRITE=1";
			madeChanges = true;
		}
		if(sqlLowerCase.indexOf("charset=") < 0) {
			sql += " CHARSET='utf8mb4'";
			madeChanges = true;
		}
		if(sqlLowerCase.indexOf("collate") < 0) {
			sql += " COLLATE 'utf8mb4_unicode_ci'";
			madeChanges = true;
		}
		if(!madeChanges) {
			return null;
		}
		if(hasSemicolon) {
			sql += ";";
		}
		return sql;
	}

	/**
	 * Perform a DELETE FROM statement, including analyzing and optimizing the table
	 * if too many records were removed.
	 * @param db database to use
	 * @param sql DELETE FROM statement to perform
	 * @return the number of records deleted
	 * @throws SQLException if any statement goes wrong
	**/
	public static int executeDelete(Connection db, String sql) throws SQLException {
		int affectedRows = executeSQLCore(db,sql);
		if(false && affectedRows >= 50000) {
			sql = sql.trim();
			String sqlLowerCase = sql.toLowerCase().trim();
			// Extract the table name
			String table = "";
			if(sqlLowerCase.startsWith("delete from")) {
				int index = "delete from".length();
				// Skip leading whitespace
				while(Character.isWhitespace(sql.charAt(index))) {
					index++;
				}
				// Read the table name
				while(index < sql.length() && !Character.isWhitespace(sql.charAt(index))) {
					table += sql.charAt(index);
					index++;
				}
				if(table.length() > 0) {
					String triedSQL = "";
					try {
						triedSQL = "optimize table " + table;
						executeSQLCore(db,triedSQL);
						triedSQL = "analyze table " + table;
						executeSQLCore(db,triedSQL);
					} catch(SQLException e) {
						// Complain about these but do not stop the processing
						Logger.log(LogMessageCategory.INFO,
								"Unable to optimize table after DELETE using: " + triedSQL
								+ "\n based on original statement: " + sql);
					}
				}
			}
		}
		return affectedRows;
	}

	/**
	 * Perform an INSERT ... SELECT statement as a series of equivalent statements
	 * with LIMIT clauses.
	 * @param db database to use
	 * @param baseSQL INSERT ... SELECT statement to perform
	 * @return the number of records inserted
	 * @throws SQLException if any statement goes wrong
	**/
	public static int executeInsertSelect(Connection db, String baseSQL) throws SQLException {
		return executeInsertSelect(db,baseSQL,0,true);
	}

	/**
	 * Perform an INSERT ... SELECT statement as a series of equivalent statements
	 * with LIMIT clauses.
	 * @param db database to use
	 * @param baseSQL INSERT ... SELECT statement to perform
	 * @param startRecord 0-based record to use within the SELECT statement
	 * @param reformatGroupBy true if addOrderToGroup() should be used
	 * @return the number of records inserted
	 * @throws SQLException if any statement goes wrong
	**/
	public static int executeInsertSelect(Connection db, String baseSQL,
			int startRecord, boolean reformatGroupBy)  throws SQLException {
		baseSQL = baseSQL.trim();
		while(baseSQL.endsWith(";")) {
			baseSQL = baseSQL.substring(0,baseSQL.length()-1).trim();
		}

		if(reformatGroupBy) {
			String newSQL = addOrderToGroup(baseSQL,baseSQL.toLowerCase());
			if(newSQL != null) {
				baseSQL = newSQL;
			}
		}

		int totalRecordsAffected = 0;
		int recordsToUse = 200000;
		int limit = -1;
		if(db == null) {
			limit = (int)(2.5*recordsToUse);
		}
		while(limit<0 || totalRecordsAffected < limit) {
			String sql = baseSQL + " limit " + startRecord + "," + recordsToUse;
			//System.out.println("NEWSQL: " + sql);
			int count = executeSQLCore(db,sql);
			if(count > 0) {
				totalRecordsAffected += count;
				startRecord += count;
			}
			if(count < recordsToUse) {
				break;
			}
		}
		return totalRecordsAffected;
	}

	/**
	 * Perform a CREATE ... SELECT statement as a CREATE ... SELECT ... LIMIT 0,1
	 * statement followed by INSERT ... SELECT ... LIMIT ... statements.
	 * @param db database to use
	 * @param baseSQL CREATE ... SELECT statement to use
	 * @return the number of records placed into the table
	 * @throws SQLException if any statement goes wrong
	**/
	public static int executeCreateSelect(Connection db, String baseSQL) throws SQLException {
		baseSQL = baseSQL.trim();
		while(baseSQL.endsWith(";")) {
			baseSQL = baseSQL.substring(0,baseSQL.length()-1).trim();
		}
		String lowercaseBaseSQL = baseSQL.toLowerCase();

		String newSQL = addOrderToGroup(baseSQL,lowercaseBaseSQL);
		if(newSQL != null) {
			baseSQL = newSQL;
			lowercaseBaseSQL = baseSQL.toLowerCase();
		}

		if(lowercaseBaseSQL.indexOf("limit ") > 0) {
			// Already contains a LIMIT clause
			return executeSQLCore(db,baseSQL);
		}
		int totalRecordsAffected = 0;
		String sql = baseSQL + " limit 0,1";
		//System.out.println("executeCreateSelect input: \"" + baseSQL + "\"");
		//System.out.println("executeCreateSelect create: \"" + sql + "\"");
		totalRecordsAffected = executeSQLCore(db,sql);
		if(totalRecordsAffected >= 1) {
			// Get the portion from the table name through the end of the statement
			// Example syntax:
			// create table m2 select monthid, monthname from monthofanyyear limit 0,1
			// create table if not exists m2 select monthid, monthname from monthofanyyear limit 0,1
			int index = lowercaseBaseSQL.indexOf("select") - 1;
			// Skip leading space before the SELECT word
			while(Character.isWhitespace(lowercaseBaseSQL.charAt(index))) {
				index--;
			}
			// index is now at the last character of the table name
			while(!Character.isWhitespace(lowercaseBaseSQL.charAt(index))) {
				index--;
			}
			index++;

			sql = "insert into " + baseSQL.substring(index);
			//System.out.println("executeCreateSelect insert: \"" + sql + "\"");
			totalRecordsAffected += executeInsertSelect(db,sql,1,false);
		}
		return totalRecordsAffected;
	}

	/**
	 * Produce a generic, pattern-centric, version of a statement.  The resulting statement
	 * won't necessarily work but will remove natural variation, such as numeric and text
	 * literals.  Case is also standardized to lower case and streaks of white space are
	 * clipped to one space.
	 * @param sql statement to be examined
	 * @return generisized statement, never null (unless sql itself was)
	**/
	public static String toGeneric(String sql) {
		if(sql == null) {
			return null;
		}
		sql = sql.toLowerCase().trim();
		if(sql.length() <= 0) {
			return "";
		}
		int index = sql.indexOf("limit ");
		if(index > 0) {
			sql = sql.substring(0,index);
		}
		String result = "";
		int state = 0;
		char[] sqlC = sql.toCharArray();
		char c;
		String delimiters = "=<>!-+|&,./*()\\:;";
		for(int i=0;i<sqlC.length;i++) {
			c = sqlC[i];
			switch(state) {
				case 0: // idle
					if(c == '\'') {
						state = 100; // parse and remove a TEXT string
						result += "\'TEXT\'";
					} else if(Character.isWhitespace(c)) {
						// Whitespace is a delimiter, skip trailing whitespace
						result += " ";
						state = 400;
					} else if(delimiters.indexOf(c) >= 0) {
						// Delimiter, just stay in this state
						result += c;
					} else if(Character.isDigit(c)) {
						state = 200; // parse and remove a number
						result += "#";
					} else { // start of a word, so read until whitespace or a delimiter
						result += c;
						state = 300;
					}
					break;
				case 100: // Skip a text string, looking for the ending ' and skipping ''
					if(c == '\'') {
						if(i+1<sqlC.length && sqlC[i+1] == '\'') {
							i++;
						} else {
							state = 0;
						}
					} else {
						// Stay in this state
					}
					break;
				case 200: // Skip a number
					if(Character.isWhitespace(c)) {
						// Whitespace is a delimiter, skip trailing whitespace
						result += " ";
						state = 400;
					} else if(Character.isDigit(c)) {
						// Stay in this state
					} else if(c == '.') {
						state = 201; // read digits after decimal
					} else if(c == 'e' || c == 'E') {
						state = 202; // read sign and digits after exponent
					} else {
						i--;
						state = 0;
					}
					break;
				case 201: // Read digits after decimal
					if(Character.isWhitespace(c)) {
						// Whitespace is a delimiter, skip trailing whitespace
						result += " ";
						state = 400;
					} else if(Character.isDigit(c)) {
						// Stay in this state
					} else if(c == 'e' || c == 'E') {
						state = 202; // read sign and digits after exponent
					} else {
						i--;
						state = 0;
					}
					break;
				case 202: // Read sign after exponent
					if(Character.isWhitespace(c)) {
						// Whitespace is a delimiter, skip trailing whitespace
						result += " ";
						state = 400;
					} else if(Character.isDigit(c)) {
						state = 203; // read digits after exponent
					} else if(c == '+' || c == '-') {
						state = 203; // read digits after exponent
					} else {
						i--;
						state = 0;
					}
					break;
				case 203: // Read digits after exponent
					if(Character.isWhitespace(c)) {
						// Whitespace is a delimiter, skip trailing whitespace
						result += " ";
						state = 400;
					} else if(Character.isDigit(c)) {
						// Stay in this state
					} else {
						i--;
						state = 0;
					}
					break;
				case 300: // Skip a word
					if(Character.isWhitespace(c)) {
						// Whitespace is a delimiter, skip trailing whitespace
						result += " ";
						state = 400;
					} else if(delimiters.indexOf(c) >= 0) {
						// Delimiter, go back to the main state
						result += c;
						state = 0;
					} else {
						result += c;
						// Stay in this state
					}
					break;
				case 400: // Skip whitespace
					if(Character.isWhitespace(c)) {
						// Nothing to do
					} else {
						i--;
						state = 0;
					}
					break;
			}
		}
		return result.trim();
	}
}
