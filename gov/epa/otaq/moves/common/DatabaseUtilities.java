/**************************************************************************************************
 * @(#)DatabaseUtilities.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.sql.*;
import java.util.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Pattern;

/**
 * This static class provides support for working with SQL Scripts and performing general-purpose
 * operations on a database and its contents.
 *
 * @author		Wes Faler
 * @author		Mitch C. (Minor mod for task 18 item 169)
 * @author      John Covey (Task 2003)
 * @version     2020-08-10
 
**/
public class DatabaseUtilities {
	/**
	 * Default argument version of the method below.
	 * @param sql The text to be escaped.
	 * @return The escaped text.
	**/
	public static String escapeSQL(String sql) {
		return escapeSQL(sql, true);
	}

	/**
	 * Escapes a string so that it can be used in the SQL statement as apposed to being passed as a
	 * parameter. This may be done for efficiency but it is more often done for SQL terms such as
	 * table, column, and export file names that cannot be passed as parameters. This routine was
	 * written to work with MySql. Unfortuantely, there isn't another Java supported way to escape
	 * SQL text in a database agnostic fashion.
	 * @param sql The text to be escaped.
	 * @param addOuterQuotes Flag indicating if result should be returned in quotes.
	 * @return The escaped text.
	**/
	public static String escapeSQL(String sql, boolean addOuterQuotes) {
		if(sql.indexOf('\'') < 0 && sql.indexOf('\\') < 0) {
			return addOuterQuotes? "'" + sql + "'" : sql;
		}

		String result = "";
		char iterCharacter;

		for(int i = 0; i < sql.length(); i++) {
			iterCharacter = sql.charAt(i);

			switch (iterCharacter) {
				// Escape ' to \'
				case '\'':
					result += "\\\'";
					break;
				// Escape \ to \\
				case '\\':
					result += "\\\\";
					break;
				default:
					result += iterCharacter;
					break;
			}
		}

		return addOuterQuotes ? "'" + result + "'" : result;
	}

	/**
	 * Simple method to close a database connection. This catches and logs database errors.
	 * @param db The database connection to close.
	**/
	public static void closeConnection(Connection db) {
		if(db != null) {
			try {
				db.close();
			} catch (SQLException exception) {
				/**
				 * @issue Close database connection failed
				 * @explain MOVES was unable to close its connection to a database.  Most likely this
				 * is due to the database server itself no longer being available, in which case there
				 * are more pressing issues with the database server than with MOVES.
				**/
				Logger.logError(exception, "Close database connection failed");
			}
		}
	}

	/**
	 * Copies a ResultSet's rows to a destination table using the specified Connection.
	 * Does not close the ResultSet object when finished.
	 * @param rs An opened ResultSet object
	 * @param db An opened Connection object containing the destination table.
	 * @param tableName The name of the destination table.
	 * @throws SQLException from any of java.sql operations.
	**/
	public static void copyResultSetToTable(ResultSet rs, Connection db, String tableName)
			throws SQLException {
		// Build the INSERT statement using the column names
		String sql = "INSERT INTO " + tableName + " (";
		String valuesClause = ") VALUES (";
		ResultSetMetaData metaData = rs.getMetaData();
		int columnCount = 0;
		for(int i = 0; i < metaData.getColumnCount(); i++) {
			if(i != 0) {
				sql += ",";
				valuesClause += ",";
			}
			sql += metaData.getColumnName(i + 1);
			valuesClause += "?";
			columnCount++;
		}
		sql += (valuesClause + ")");
		PreparedStatement statement = db.prepareStatement(sql);
		// Iterate through all rows
		while(rs.next()) {
			statement.clearParameters();
			for(int i = 0; i < columnCount; i++) {
				statement.setObject(i + 1, rs.getObject(i + 1));
			}
			// Add this row to the destination table
			SQLRunner.execute(statement,sql);
		}
		statement.close();
	}

	/**
	 * Executes an SQL script on the specified database.
	 * This currently doesn't handle multiple statements per line.
	 * @param database The target database to execute the script in.
	 * @param scriptFile The script file to execute.
	 * @throws FileNotFoundException If the script file can not be found.
	 * @throws IOException If there is a file I/O error.
	 * @throws SQLException If there is a database error.
	**/
	public static void executeScript(Connection database, File scriptFile)
			throws FileNotFoundException, IOException, SQLException {
		executeScript(database,scriptFile,null);
	}

	/**
	 * Executes an SQL script on the specified database.
	 * This currently doesn't handle multiple statements per line.
	 * @param database The target database to execute the script in.
	 * @param scriptFile The script file to execute.
	 * @param replacements Substitution text for each command.  Each command in the script
	 * is searched for matches against this map and updated values replaced into the command
	 * before execution.  May be null or empty.
	 * @throws FileNotFoundException If the script file can not be found.
	 * @throws IOException If there is a file I/O error.
	 * @throws SQLException If there is a database error.
	**/
	public static void executeScript(Connection database, File scriptFile,
			TreeMapIgnoreCase replacements)
			throws FileNotFoundException, IOException, SQLException {
		executeScript(database,scriptFile,replacements,false);
	}

	/**
	 * Executes an SQL script on the specified database.
	 * This currently doesn't handle multiple statements per line.
	 * @param database The target database to execute the script in.
	 * @param scriptFile The script file to execute.
	 * @param replacements Substitution text for each command.  Each command in the script
	 * is searched for matches against this map and updated values replaced into the command
	 * before execution.  May be null or empty.
	 * @param ignoreSQLExceptions when true, SQL errors do not stop execution.
	 * @throws FileNotFoundException If the script file can not be found.
	 * @throws IOException If there is a file I/O error.
	 * @throws SQLException If there is a database error.
	**/
	public static void executeScript(Connection database, File scriptFile,
			TreeMapIgnoreCase replacements, boolean ignoreSQLExceptions)
			throws FileNotFoundException, IOException, SQLException {
		final String eol = System.getProperty("line.separator");
		BufferedReader sqlReader = new BufferedReader(new FileReader(scriptFile));
		String currentSQLStatement = "";
		try {
			boolean isInBlock = false;

			while(true) {
				String entireSQLStatement = null;
				String iterLine = sqlReader.readLine();
				if(iterLine == null) {
					break;
				}

				iterLine = trimSQLEOLComment(iterLine);
				if(isInBlock) {
					if(iterLine.equalsIgnoreCase("EndBlock")) {
						isInBlock = false;
						iterLine = "";
						entireSQLStatement = currentSQLStatement;
					} else {
						currentSQLStatement += iterLine + "\n";
						continue;
					}
				} else if(iterLine.equalsIgnoreCase("BeginBlock")) {
					isInBlock = true;
					currentSQLStatement = "";
					continue;
				} else {
					currentSQLStatement += iterLine;
					entireSQLStatement = getCompleteSQLStatement(currentSQLStatement);
				}
				if(entireSQLStatement != null) {
					if(replacements != null && replacements.size() > 0) {
						entireSQLStatement =
								StringUtilities.doReplacements(entireSQLStatement,replacements);
					}
					if(ignoreSQLExceptions) {
						try {
							SQLRunner.executeSQL(database, entireSQLStatement);
						} catch(Exception e) {
							// Nothing to do here
						}
					} else {
						SQLRunner.executeSQL(database, entireSQLStatement);
					}
					currentSQLStatement = "";
				} else {
//					currentSQLStatement += eol;
					currentSQLStatement += " ";
				}
			}
		} catch(SQLException e) {
			/**
			 * @issue Failed to execute script: [currentSQLStatement]
			 * @explain While executing the statements within a SQL script file, MOVES found
			 * an error with a statement.
			**/
			Logger.log(LogMessageCategory.INFO,"Failed to execute script: " + currentSQLStatement);
			throw e;
		} finally {
			sqlReader.close();
		}
		if(currentSQLStatement.trim().length() > 0) {
			/**
			 * @issue Script [scriptFileName] has unexecuted SQL at the end: [sql]
			 * @explain All statements in a script must end with a semicolon (;).
			**/
			Logger.log(LogMessageCategory.ERROR,"Script " + scriptFile.getName()
					+ " has unexecuted SQL at the end: " + currentSQLStatement);
		}
	}

	/**
	 * Gets a completed SQL statement from the given text block. The statement must be terminated
	 * by a single ';' character. This doesn't handle multiple statements within the same text
	 * block. It can potentially fail if using a database system where ';' isn't an SQL terminator
	 * or if the line is in the middle of a multi-line quoted string.
	 * @param text The source SQL line.
	 * @return The entire SQL statement without the terminating character.
	**/
	public static String getCompleteSQLStatement(String text) {
		for (int i = text.length() - 1; i >= 0; i--) {
			char iterChar = text.charAt(i);

			if(iterChar == ';') {
				return text.substring(0, i);
			} else if(!Character.isWhitespace(iterChar)) {
				return null;
			}
		}

		return null;
	}


	/**
	 * Trims end-of-line style comments ("--") from SQL statements. This makes the output
	 * produced by the SQLRunner more readable while allowing section comments to be added to the
	 * SQl script. In-line comments (i.e. start with "/*") are not removed and can be used to
	 * record information in the output produced by SQLRunner.
	 * @param text The source SQL line.
	 * @return The SQL up to the start of an End-of-Line comment.
	**/
	static String trimSQLEOLComment(String text) {
		boolean startComment = false;
		for (int i = 0; i<text.length(); i++) {
			char iterChar = text.charAt(i);
			if(iterChar == '-') {
				if(startComment) {
					if(text.endsWith("*/")) {
						return text.substring(0, i-1) + "*/";
					}
					return text.substring(0, i-1);
				} else {
					startComment = true;
				}
			} else {
				startComment = false;
			}
		}

		return text;
	}

	/**
	 * Gets the row count for a table on the specified database connection.
	 * @param connection The database connection to use.
	 * @param tableName The name of the table to count rows of
	 * @return The number of rows.
	 * @throws An SQL exception
	**/
	public static int getRowCount(Connection connection, String tableName) throws SQLException {
		ResultSet rs = SQLRunner.executeQuery(connection, "SELECT COUNT(*) FROM " + tableName);
		rs.next();
		int rowCount = rs.getInt(1);
		rs.close();
		return rowCount;
	}

	/**
	 * Copies a table from one database to another. It <i>is</i> an error if the table is not
	 * present in the destination database. It is an optional error if the table is not present
	 * in the source database. This allows partial databases to be imported easier.
	 * @param source The database to get data from.
	 * @param destination The database to write data to
	 * @param tableName The table name to copy over.
	 * @throws SQLException If an SQL error occurs.
	 * @throws IOException If an IO error occurs while working with a temporary data file.
	 * @return true if the files were copied, false if they could not be because they don't
	 * exist on locally reachable paths
	**/
	public static boolean copyTableFiles(Connection source, Connection destination,
			String tableName) throws SQLException, IOException {
		if(!isLocal(source) || !isLocal(destination)) {
			return false;
		}
		String sourceDatabaseName = source.getCatalog();
		String destinationDatabaseName = destination.getCatalog();
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			// Get the data directory
			String dataFolderName = null;
			sql = "SHOW VARIABLES";
			query.open(source,sql);
			while(query.rs.next()) {
				String name = query.rs.getString("Variable_name");
				if(name != null && name.equalsIgnoreCase("datadir")) {
					dataFolderName = query.rs.getString("Value");
					break;
				}
			}
			query.close();

			if(dataFolderName == null || dataFolderName.length() <= 0) {
				throw new SQLException("Unable to find datadir variable using SHOW VARIABLES");
			}
			File dataFolder = new File(dataFolderName);
			if(!dataFolder.exists()) {
				// If the folder doesn't exist, we can't do a local copy.  Return without exception.
				return false;
			}
			File sourceFolder = new File(dataFolder,sourceDatabaseName);
			File destFolder = new File(dataFolder,destinationDatabaseName);
			if(!sourceFolder.exists() || !destFolder.exists()) {
				// If the folders don't exist, we can't do a local copy.  Return without exception.
				return false;
			}
			File sourceFRM = new File(sourceFolder,tableName + ".frm");
			File destFRM = new File(destFolder,sourceFRM.getName());

			File sourceMYD = new File(sourceFolder,tableName + ".MYD");
			File destMYD = new File(destFolder,sourceMYD.getName());

			File sourceMYI = new File(sourceFolder,tableName + ".MYI");
			File destMYI = new File(destFolder,sourceMYI.getName());

			if(!sourceFRM.exists() || !sourceMYD.exists() || !sourceMYI.exists()) {
				// If the source files don't exist, we can't do a local copy.  Return without exception.
				return false;
			}

			sql = "drop table " + tableName;
			SQLRunner.executeSQL(destination,sql);

			sql = "flush tables";
			SQLRunner.executeSQL(destination,sql);

			sql = "flush tables";
			SQLRunner.executeSQL(source,sql);

			if(!FileUtilities.copyFile(sourceMYI,destMYI,true)) {
				throw new IOException("Unable to copy " + sourceMYI.getCanonicalPath()
						+ " to " + destMYI.getCanonicalPath());
			}
			if(!FileUtilities.copyFile(sourceMYD,destMYD,true)) {
				throw new IOException("Unable to copy " + sourceMYD.getCanonicalPath()
						+ " to " + destMYD.getCanonicalPath());
			}
			if(!FileUtilities.copyFile(sourceFRM,destFRM,true)) {
				throw new IOException("Unable to copy " + sourceFRM.getCanonicalPath()
						+ " to " + destFRM.getCanonicalPath());
			}
			/**
			 * @issue Copied table file [sourceDatabaseName]/[tableName] to [destinationDatabaseName]
			 * @explain For speed sake, MOVES copied the MySQL files associated with a table directly
			 * instead of performing an INSERT..SELECT operation.  This is informational only.
			**/
			Logger.log(LogMessageCategory.INFO,"Copied table file " + sourceDatabaseName + "/"
					+ tableName + " to " + destinationDatabaseName);
			return true;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Copies a table from one database to another. It <i>is</i> an error if the table is not
	 * present in the destination database. It is an optional error if the table is not present
	 * in the source database. This allows partial databases to be imported easier.
	 * If uniquely keyed record is already present in the destination database table
	 *   new record is IGNOREd.  This was default behavior in MySQL 3.23.49
	 *   and was made explicit in this method to accomodate upgrade to MySQL 4.0.21
	 * @param source The database to get data from.
	 * @param destination The database to write data to
	 * @param tableName The table name to copy over.
	 * @param whereClause The where clause to filter the rows of the source table with.
	 * @param isErrorIfNotInSource true if a SQLException should be thrown if the table
	 * does not exist in the source database
	 * @return true if any data was copied, may be false if there were no errors but no data was
	 * copied from the source.
	 * @throws SQLException If an SQL error occurs.
	 * @throws IOException If an IO error occurs while working with a temporary data file.
	**/
	public static boolean copyTable(Connection source, Connection destination,
			String tableName, String whereClause, boolean isErrorIfNotInSource)
			throws SQLException, IOException {
		String sourceDatabaseName = source.getCatalog();
		String destinationDatabaseName = destination.getCatalog();
		String sql = "";
		String columns="";
		sql = "SELECT * FROM " + tableName + " LIMIT 0";
		boolean isExtractingFromSource = true;
		boolean canDoWholeTableCopy = false;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(destination,sql);
			ResultSetMetaData metaData = query.rs.getMetaData();
			int columnCount = metaData.getColumnCount();
			for(int i=1; i<= columnCount; i++) {
				if(i == 1) {
					columns = metaData.getColumnName(1);
				} else {
					columns += "," + metaData.getColumnName(i);
				}
			}
			query.close();

			// If there is no whereClause and the destination table is empty,
			// it can be totally replaced with the source table's files.
			if(tableName.equalsIgnoreCase("SampleVehiclePopulation")) {
				if(whereClause == null || whereClause.length() <= 0) {
					sql = "select count(*) from " + tableName;
					int count = (int)SQLRunner.executeScalar(destination,sql);
					if(count <= 0) {
						canDoWholeTableCopy = true;
					}
				}
			}

			if(areSameServer(source,destination)) {
				// setup so that if the following SQL fails, an error report will get made,
				// since it is an error for the table not to exist in the destination database.
				isExtractingFromSource = false;

				sql = "select count(*) from " + tableName;
				int sourceCount = (int)SQLRunner.executeScalar(source,sql);
				// If we're here, it means the source table does exist.
				if(sourceCount > 0) {
					if(canDoWholeTableCopy) {
						if(copyTableFiles(source,destination,tableName)) {
							return true;
						}
						// If unable to copy the files, then fall back to native SQL
					}
					sql = "INSERT IGNORE INTO " + destinationDatabaseName + "." + tableName
							+ "(" + columns + ") SELECT " + columns + " FROM "
							+ tableName;
					if(whereClause.length() > 0) {
						sql += " WHERE " + whereClause;
					}
					int rowsAffected = SQLRunner.executeSQL(source,sql);
					return rowsAffected > 0;
				}
			} else {
				query.close();
				// Do a server-to-server INSERT IGNORE INTO
				return moveDataServerToServer(source,destination,tableName,whereClause,query,columns,columnCount,"INSERT IGNORE INTO");
			}
		} catch(SQLException e) {
			if(isErrorIfNotInSource) {
				Logger.logSqlError(e, "Copy table, "+tableName+", from source database failed.",
						sql);
				throw e;
			} else if(!isExtractingFromSource || e.getMessage().indexOf("doesn't exist") < 0) {
				Logger.logSqlError(e, "Copy table, "+tableName+", to destination database failed.",
						sql);
				throw e;
			}
		} catch(IOException e) {
			if(isErrorIfNotInSource) {
				Logger.logSqlError(e, "Copy table, "+tableName+", from source database failed.",
						sql);
				throw e;
			} else if(!isExtractingFromSource || e.getMessage().indexOf("doesn't exist") < 0) {
				Logger.logSqlError(e, "Copy table, "+tableName+", to destination database failed.",
						sql);
				throw e;
			}
		} finally {
			query.onFinally();
		}
		return false;
	}

	/**
	 * Copies a table from one database to another. It <i>is</i> an error if the table is not
	 * present in the destination database. It is an optional error if the table is not present
	 * in the source database. This allows partial databases to be imported easier.
	 * If uniquely keyed record is already present in the destination database table,
	 * the existing destination record will be replaced.
	 * @param source The database to get data from.
	 * @param destination The database to write data to
	 * @param tableName The table name to copy over.
	 * @param whereClause The where clause to filter the rows of the source table with.
	 * @param isErrorIfNotInSource true if a SQLException should be thrown if the table
	 * does not exist in the source database
	 * @throws SQLException If an SQL error occurs.
	 * @throws IOException If an IO error occurs while working with a temporary data file.
	**/
	public static void replaceIntoTable(Connection source, Connection destination,
			String tableName, String whereClause, boolean isErrorIfNotInSource)
			throws SQLException, IOException {
		String sourceDatabaseName = source.getCatalog();
		String destinationDatabaseName = destination.getCatalog();
		//File transferFile = new File("DatabaseUtilities_copyTable.temp");
		String sql;
		String columns="";
		sql = "SELECT * FROM " + tableName + " LIMIT 0";
		boolean isExtractingFromSource = true;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(destination,sql);
			ResultSetMetaData metaData = query.rs.getMetaData();
			int columnCount = metaData.getColumnCount();
			for(int i=1; i<= columnCount; i++) {
				if(i == 1) {
					columns = metaData.getColumnName(1);
				} else {
					columns += "," + metaData.getColumnName(i);
				}
			}
			query.close();

			if(areSameServer(source,destination)) {
				// setup so that if the following SQL fails, an error report will get made,
				// since it is an error for the table not to exist in the destination database.
				isExtractingFromSource = false;

				sql = "REPLACE INTO " + destinationDatabaseName + "." + tableName
						+ "(" + columns + ") SELECT " + columns + " FROM "
						+ tableName;
				if(whereClause.length() > 0) {
					sql += " WHERE " + whereClause;
				}
				SQLRunner.executeSQL(source,sql);
			} else {
				query.close();
				// Do a server-to-server REPLACE INTO
				moveDataServerToServer(source,destination,tableName,whereClause,query,columns,columnCount,"REPLACE INTO");
			}
		} catch(SQLException e) {
			if(isErrorIfNotInSource) {
				Logger.logSqlError(e, "Replace into table, "+tableName
						+", from source database failed.",
						sql);
				throw e;
			} else if(!isExtractingFromSource || e.getMessage().indexOf("doesn't exist") < 0) {
				Logger.logSqlError(e, "Replace into table, "+tableName
						+", to destination database failed.",
						sql);
				throw e;
			}
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Copies a table from one server to another.
	 * @param source The database to get data from.
	 * @param destination The database to write data to
	 * @param tableName The table name to copy over.
	 * @param whereClause The where clause to filter the rows of the source table with.
	 * @param query query object to be used
	 * @param columns comma separated list of columns to be transfered
	 * @param columnCount number of columns to be transfered
	 * @param sqlBase "REPLACE INTO" or "INSERT INTO" or "INSERT IGNORE INTO" clause without table or column names
	 * @return true if any data was transferred, false if there is no data even if no errors occurred
	 * @throws SQLException If an SQL error occurs.
	**/
	public static boolean moveDataServerToServer(Connection source, Connection destination,
			String tableName, String whereClause,
			SQLRunner.Query query,
			String columns,
			int columnCount,
			String sqlBase) throws SQLException, IOException {
		sqlBase += " " + tableName + "(" + columns + ") VALUES ";
		String sql = "SELECT COUNT(*) FROM " + tableName;
		if(whereClause.length() > 0) {
			sql += " WHERE " + whereClause;
		}
		int sourceRowCount = (int)SQLRunner.executeScalar(source,sql);
		if(sourceRowCount <= 0) {
			return false; // nothing to do, no data to transfer
		}

		sql = "SELECT " + columns + " FROM " + tableName;
		if(whereClause.length() > 0) {
			sql += " WHERE " + whereClause;
		}
		boolean isLargeResultSet = false;
		if(sourceRowCount <= 100000) { // estimated to fit in 50MB at 500 bytes/record
			query.open(source,sql);
		} else {
			isLargeResultSet = true;
			query.openLargeResultSet(source,sql);
		}
		String sqlBatch = "";
		int i, batchCount = 0;
		String s, line;
		while(query.rs.next()) {
			if(sqlBatch.length() > 0) {
				line = ",(";
			} else {
				line = "(";
			}
			for(i=1;i<=columnCount;i++) {
				s = query.rs.getString(i);
				if(query.rs.wasNull()) {
					if(i == 1) {
						line += "null";
					} else {
						line += ",null";
					}
				} else {
					s = DatabaseUtilities.escapeSQL(s,false);
					if(i == 1) {
						line += "'" + s + "'";
					} else {
						line += ",'" + s + "'";
					}
				}
			}
			line += ")";
			sqlBatch += line;
			if(sqlBatch.length() >= 50000) {
				sql = sqlBase + sqlBatch;
				//System.out.println(sql);
				SQLRunner.executeSQL(destination,sql);
				sqlBatch = "";
				batchCount++;
			}
		}
		if(sqlBatch.length() > 0) {
			sql = sqlBase + sqlBatch;
			//System.out.println(sql);
			SQLRunner.executeSQL(destination,sql);
			sqlBatch = "";
			batchCount++;
		}
		if(isLargeResultSet) {
			Logger.log(LogMessageCategory.INFO,"Server-to-Server copy of " + tableName + " used a large result set.");
		}
		if(batchCount > 1) {
			Logger.log(LogMessageCategory.INFO,"Server-to-Server copy of " + tableName + " took " + batchCount + " batches.");
		}
		return true;
	}

	/**
	 * Drops all the tables in the target database.
	 * @param targetDB The database to drop the tables in.
	**/
	public static void dropAllTables(Connection targetDB) {
		String sql = null;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			sql = "SHOW TABLES";
			query.open(targetDB,sql);
			TreeSet<String> tableNames = new TreeSet<String>();
			while(query.rs.next()) {
				tableNames.add(query.rs.getString(1));
			}
			query.close();

			for(Iterator<String> iterTableNames=tableNames.iterator();iterTableNames.hasNext();) {
				try {
					String tableName = (String)iterTableNames.next();
					//Logger.log(LogMessageCategory.DEBUG,"Dropping execution database table " + tableName);

					//sql = "FLUSH TABLE " + tableName;
					//SQLRunner.executeSQL(targetDB,sql);

					sql = "DROP TABLE IF EXISTS " + tableName;
					SQLRunner.executeSQL(targetDB,sql);

					//sql = "FLUSH TABLE " + tableName;
					//SQLRunner.executeSQL(targetDB,sql);
				} catch (SQLException e) {
					Logger.logError(e, "DROP TABLE failed using: " + sql);
				}
			}
		} catch (SQLException e) {
			query.onException(e,"dropAllTables() failed", sql);
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Peform the equivalent of an INSERT IGNORE INTO table (col1,col2) SELECT col1,col2...
	 * statement.
	 * @param useTemporaryFile true if SELECT INTO OUTFILE/LOAD DATA INFILE statements should
	 * be used.  Normally left to true unless a specific test shows the file usage to be slower.
	 * @param db database to perform the work within
	 * @param destinationTableName table to INSERT IGNORE INTO
	 * @param destinationColumns names, without parenthesis, of columns to fill
	 * @param selectStatement complete SELECT statement including GROUP BY and ORDER BY clauses
	 * (if needed)
	 * @throws SQLException upon any SQL or File error.  File exceptions are translated into
	 * SQL exceptions
	**/
	public static void insertSelect(boolean useTemporaryFile, Connection db,
			String destinationTableName,String destinationColumns,
			String selectStatement) throws SQLException {
		if(!isLocal(db)) {
			useTemporaryFile = false;
		}
		File tempFile = new File("insertSelectTemp.txt");
		if(tempFile.exists()) {
			try {
				tempFile.delete();
			} catch(Exception e) {
				// Nothing to do here
			}
			if(tempFile.exists()) {
				throw new SQLException("Unable to delete temporary file used by insertSelect");
			}
		}
		String t = selectStatement.toUpperCase();
		int fromLocation = t.indexOf("FROM ");
		if(fromLocation < 0) {
			throw new SQLException("Unable to find FROM clause in statement: " + selectStatement);
		}
		try {
			if(useTemporaryFile) {
				String filePathSQL = " INTO OUTFILE '"
						+ tempFile.getCanonicalPath().replace('\\','/') + "' ";
				String selectWithOutfile = selectStatement.substring(0,fromLocation)
						+ filePathSQL
						+ selectStatement.substring(fromLocation);
				String loadStatement = "LOAD DATA INFILE '"
						+ tempFile.getCanonicalPath().replace('\\','/') + "' IGNORE INTO TABLE"
						+ " " + destinationTableName + "(" + destinationColumns + ")";
				SQLRunner.executeSQL(db,selectWithOutfile);
				SQLRunner.executeSQL(db,loadStatement);
			} else {
				String sql = "INSERT IGNORE INTO " + destinationTableName + "("
						+ destinationColumns + ") " + selectStatement;
				SQLRunner.executeSQL(db,sql);
			}
		} catch(IOException e) {
			throw new SQLException(e.toString());
		} finally {
			try {
				tempFile.delete();
			} catch(Exception e) {
				// Nothing to do here
			}
			tempFile = null;
		}
	}

	/**
	 * Obtain a set of numbers from a query.
	 * @param db database to be used
	 * @param sql statement to be used.  the first column must be an integer, with null values
	 * being treated as 0.
	 * @return a set of distinct values found, never null but maybe empty
	 * @throws SQLException if anything goes wrong
	**/
	public static TreeSet<Integer> getIntegerSet(Connection db, String sql) throws SQLException {
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			TreeSet<Integer> results = new TreeSet<Integer>();
			while(query.rs.next()) {
				results.add(Integer.valueOf(query.rs.getInt(1)));
			}
			return results;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Check a server name to determine if it is a local server.
	 * @param serverName name to be tested
	 * @return true if the name references a local computer
	**/
	public static boolean isLocal(String serverName) {
		if(serverName == null) {
			serverName = "";
		} else {
			serverName = serverName.trim();
		}
		return serverName == null || serverName.length() <= 0
				|| serverName.equals("127.0.0.1")
				|| serverName.equalsIgnoreCase("localhost") 
				|| serverName.toLowerCase().startsWith("localhost:");
	}

	/**
	 * Check the server for a connection to determine if it is a local server.
	 * @param db connection to be tested
	 * @return true if the connection references a local computer
	**/
	public static boolean isLocal(Connection db) {
		try {
			String serverName = getServer(db);
			return isLocal(serverName);
		} catch(SQLException e) {
			Logger.logError(e,"Unable to check locality of database server");
			return false;
		}
	}

	/**
	 * Determine if two connections actually reference the same server.
	 * Some copying algorithms require this.
	 * @param a a Connection to be checked
	 * @param b a Connection to be checked
	 * @return true if the connections reference the same server.  Always true if both reference
	 * the local server, even if one does so via "127.0.0.1" and another via "localhost".
	**/
	public static boolean areSameServer(Connection a, Connection b) {
		try {
			String aServerName = getServer(a);
			String bServerName = getServer(b);
			if(isLocal(aServerName) && isLocal(bServerName)) {
				// Two local references always match if they have different wording
				return true;
			}
			return aServerName.equalsIgnoreCase(bServerName);
		} catch(SQLException e) {
			Logger.logError(e,"Unable to check database servers for equality");
			return false; // return false so more cautious algorithms will be used.
		}
	}

	/**
	 * Obtain the name of the server that the a connection references.
	 * @param db connection to be checked
	 * @return the server name, never null or empty
	 * @throws SQLException if anything goes wrong
	**/
	public static String getServer(Connection db) throws SQLException {
		DatabaseMetaData metaData = db.getMetaData();
		String url = metaData.getURL();
		//System.out.println(url);
		int index = url.indexOf("://");
		if(index < 0) {
			throw new SQLException("Malformed connection string, lacks \"://\" clause");
		}
		String serverName = url.substring(index+3);
		index = serverName.indexOf('/');
		if(index == 0) {
			serverName = "";
		} else if(index > 0) {
			serverName = serverName.substring(0,index);
			// Remove any port designator, such as in "localhost:3306"
			index = serverName.indexOf(':');
			if(index > 0) {
				serverName = serverName.substring(0,index);
			}
		}
		//System.out.println("serverName \"" + serverName + "\" isLocal=" + isLocal(serverName));
		return serverName;
	}

	/**
	 * Execute a script that is designed to convert a database.  This involves three databases:
	 * the output database being created, the input database that must be converted, and a
	 * default database that supplies missing data.  The script is run in the context of the output
	 * database.  The input database's name is given in the ##inputdb## tag and the default database
	 * as ##defaultdb## in the script.
	 * @param scriptFile script to be executed
	 * @param outputDatabase output database to be created, may already exist
	 * @param inputDatabase database to be converted
	 * @param defaultDatabase database providing missing data
	 * @param messages feedback, if any, from the script are added to the end of this list.
	 * @throws FileNotFoundException when the script file cannot be found
	 * @throws IOException when the script file cannot be read
	 * @throws SQLException when a database cannot be accessed or created
	**/
	public static void executeConversionScript(File scriptFile, DatabaseSelection outputDatabase,
			DatabaseSelection inputDatabase, DatabaseSelection defaultDatabase, ArrayList<String> messages)
			throws FileNotFoundException, IOException, SQLException {
		Connection outputDB = null;
		Connection tempDB = null;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			if(outputDatabase.safeCreateDatabase(null) == DatabaseSelection.NOT_CREATED) {
				throw new SQLException("Unable to create or find the output database " + outputDatabase.databaseName);
			}
			outputDB = outputDatabase.openConnectionOrNull();
			if(outputDB == null) {
				throw new SQLException("Unable to connect to output database " + outputDatabase.databaseName);
			}

			tempDB = inputDatabase.openConnectionOrNull();
			if(tempDB == null) {
				throw new SQLException("Unable to connect to input database " + inputDatabase.databaseName);
			}
			closeConnection(tempDB);
			tempDB = null;

			tempDB = defaultDatabase.openConnectionOrNull();
			if(tempDB == null) {
				throw new SQLException("Unable to connect to default database " + defaultDatabase.databaseName);
			}
			closeConnection(tempDB);
			tempDB = null;
			
			// read the file that contains the create statement for the auditlog table
			String createAuditLogTable = new String(Files.readAllBytes(Paths.get("database/CreateAuditLogTables.sql")), StandardCharsets.UTF_8);

			// create table so script can display messages to user
			String sql = "create table if not exists convertTempMessages "
					+ "( message varchar(1000) not null )";
			SQLRunner.executeSQL(outputDB,sql);
			sql = "truncate table convertTempMessages";
			SQLRunner.executeSQL(outputDB,sql);

			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			replacements.put("##inputdb##",inputDatabase.databaseName);
			replacements.put("##defaultdb##",defaultDatabase.databaseName);
			replacements.put("##create_auditlog_table##",createAuditLogTable);
			executeScript(outputDB,scriptFile,replacements,true);

			if(messages != null) {
				// Retrieve the results from convertTempMessages
				sql = "select message from convertTempMessages";
				query.open(outputDB,sql);
				TreeSetIgnoreCase messagesAlreadySeen = new TreeSetIgnoreCase();
				while(query.rs.next()) {
					String m = query.rs.getString(1);
					if(m != null && m.length() > 0) {
						if(!messagesAlreadySeen.contains(m)) {
							messagesAlreadySeen.add(m);
							messages.add(m);
						}
					}
				}
				query.close();
			}
			
			// drop messages table
			sql = "drop table if exists convertTempMessages";
			SQLRunner.executeSQL(outputDB,sql);
			
		} catch(FileNotFoundException e) {
			throw e;
		} catch(IOException e) {
			throw e;
		} catch(SQLException e) {
			throw e;
		} finally {
			query.onFinally();
			if(outputDB != null) {
				closeConnection(outputDB);
				outputDB = null;
			}
			if(tempDB != null) {
				closeConnection(tempDB);
				tempDB = null;
			}
		}
	}
	
	/**
	 * Execute a script that is designed to build a LEV database.  This involves two databases:
	 * the output database being created, and a
	 * default database that supplies missing data.  The script is run in the context of the output
	 * database.  The default database should be given the name ##defaultdb## in the script.
	 * @param scriptFile script to be executed
	 * @param outputDatabase output database to be created, may already exist
	 * @param defaultDatabase database providing missing data
	 * @throws FileNotFoundException when the script file cannot be found
	 * @throws IOException when the script file cannot be read
	 * @throws SQLException when a database cannot be accessed or created
	**/
	public static void executeBuildScript(File scriptFile, DatabaseSelection outputDatabase,
			DatabaseSelection defaultDatabase)
			throws FileNotFoundException, IOException, SQLException {
		Connection outputDB = null;
		Connection tempDB = null;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			if(outputDatabase.safeCreateDatabase(null) == DatabaseSelection.NOT_CREATED) {
				throw new SQLException("Unable to create or find the output database " + outputDatabase.databaseName);
			}
			outputDB = outputDatabase.openConnectionOrNull();
			if(outputDB == null) {
				throw new SQLException("Unable to connect to output database " + outputDatabase.databaseName);
			}

			tempDB = defaultDatabase.openConnectionOrNull();
			if(tempDB == null) {
				throw new SQLException("Unable to connect to default database " + defaultDatabase.databaseName);
			}
			closeConnection(tempDB);
			tempDB = null;

			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			replacements.put("##defaultdb##",defaultDatabase.databaseName);
			replacements.put("##outputdb##",outputDatabase.databaseName);
			executeScript(outputDB,scriptFile,replacements,true);

		} catch(FileNotFoundException e) {
			throw e;
		} catch(IOException e) {
			throw e;
		} catch(SQLException e) {
			throw e;
		} finally {
			query.onFinally();
			if(outputDB != null) {
				closeConnection(outputDB);
				outputDB = null;
			}
			if(tempDB != null) {
				closeConnection(tempDB);
				tempDB = null;
			}
		}
	}
	
	/**
	 * Execute a script that calculates default ONI activity. This involves three databases:
	 * the input CDB database, a default database that supplies missing data, and a temporary
	 * database that holds intermediate calculations.  The script  is run in the context of
	 * the input database.
	 * ##defaultdb## is the name of the default database
	 * ##inputdb## is the name of the CDB
	 * ##tempdb## is the name of the temporary database used to hold the intermediate calculations
	 * @param saveFile file object to receive the output data
	 * @param inputDatabase input database to be used
	 * @param defaultDatabase database providing missing data
	 * @param messages holds messages to be displayed to the user
	 * @throws FileNotFoundException when the script file cannot be found
	 * @throws IOException when the script file cannot be read
	 * @throws SQLException when a database cannot be accessed or created
	 * @throws Exception if there are errors accessing or creating the output file
	**/
	public static boolean executeONITool(File saveFile, DatabaseSelection inputDatabase,
			DatabaseSelection defaultDatabase, ArrayList<String> messages)
			throws FileNotFoundException, IOException, SQLException, Exception {
		File oniToolScript = new File("./database/ONITool/ONITool.sql");
		Connection inputDB = null;
		Connection defaultDB = null;
		Connection tempDB = null;
		CellFileWriter writer = null;
		SQLRunner.Query query = new SQLRunner.Query();
		boolean success = false;
		String sql;
		String tempDatabaseName = "oniTool_" + inputDatabase.databaseName;
		
		try {
			// make connection to input db (this is the db the script will be run against)
			inputDB = inputDatabase.openConnectionOrNull();
			if(inputDB == null) {
				throw new SQLException("Unable to connect to input database " + inputDatabase.databaseName);
			}
			
			// make sure default database is accessible, but we don't need to keep the connection open
			defaultDB = defaultDatabase.openConnectionOrNull();
			if(defaultDB == null) {
				throw new SQLException("Unable to connect to default database " + defaultDatabase.databaseName);
			}
			closeConnection(defaultDB);
			defaultDB = null;
			
			// create temporary database for the tool's numerous intermediate calculations, as well as messages to user and results
			SQLRunner.executeSQL(inputDB,"DROP DATABASE IF EXISTS `" + tempDatabaseName + "`");
			SQLRunner.executeSQL(inputDB,"CREATE DATABASE `" + tempDatabaseName + "`");
			
			// create messages table so script can display messages to user
			sql = "CREATE TABLE IF NOT EXISTS `" + tempDatabaseName + "`.`oniTempMessages` ( "
					+ "message varchar(1000) not null "
					+ ") ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;";
			SQLRunner.executeSQL(inputDB,sql);
			sql = "truncate table `" + tempDatabaseName + "`.`oniTempMessages`";
			SQLRunner.executeSQL(inputDB,sql);
			
			// create output table
			sql = "CREATE TABLE IF NOT EXISTS `" + tempDatabaseName + "`.`ONIToolOutput` ( "
					+ "`yearID` smallint(6) NOT NULL, "
			        + "`monthID` smallint(6) NOT NULL, "
			        + "`dayID` smallint(6) NOT NULL, "
			        + "`hourID` smallint(6) NOT NULL DEFAULT 0, "
			        + "`sourceTypeID` smallint(6) NOT NULL, "
			        + "`minModelYearID` int(11) NOT NULL, "
			        + "`maxModelYearID` int(11) NOT NULL, "
					+ "`onroadSHO (hr)` double DEFAULT NULL, "
					+ "`VMT (mi)` double DEFAULT NULL, "
					+ "`ONI (hr)` double DEFAULT NULL, "
			        + "`ONI per VMT (hr idle/mi)` double DEFAULT NULL, "
			        + "`ONI per SHO (hr idle/hr operating)` double DEFAULT NULL "
			        + ") ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;";
			SQLRunner.executeSQL(inputDB,sql);
			sql = "truncate table `" + tempDatabaseName + "`.`ONIToolOutput`";
			SQLRunner.executeSQL(inputDB,sql);
			
			// list all strings that need to be replaced in the script here
			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			replacements.put("##defaultdb##",defaultDatabase.databaseName);
			replacements.put("##inputdb##",inputDatabase.databaseName);
			replacements.put("##tempdb##",tempDatabaseName);
			
			// run it!
			executeScript(inputDB,oniToolScript,replacements,false);
			
			// check for messages in oniTempMessages
			if(messages != null) {
				sql = "select message from `" + tempDatabaseName + "`.`oniTempMessages`";
				query.open(inputDB,sql);
				TreeSetIgnoreCase messagesAlreadySeen = new TreeSetIgnoreCase();
				while(query.rs.next()) {
					String m = query.rs.getString(1);
					if(m.toLowerCase().contains("success")) {
						success = true;
					}
					if(m != null && m.length() > 0) {
						if(!messagesAlreadySeen.contains(m)) {
							messagesAlreadySeen.add(m);
							messages.add(m);
						}
					}
				}
				query.close();
			}

			// save output and drop temp database if there was success
			if(success) {
				writer = new CellFileWriter(saveFile,"ONI Tool Output");
				sql = "SELECT * FROM `" + tempDatabaseName + "`.`ONIToolOutput`";
				writer.writeSQLResults(inputDB, sql, null);
				
				SQLRunner.executeSQL(inputDB, "DROP DATABASE IF EXISTS `" + tempDatabaseName + "`");
			}
		} catch(FileNotFoundException e) {
			messages.add(e.toString());
			success = false;
			throw e;
		} catch(IOException e) {
			messages.add(e.toString());
			success = false;
			throw e;
		} catch(SQLException e) {
			messages.add(e.toString());
			success = false;
			throw e;
		} catch(Exception e) {
			messages.add(e.toString());
			success = false;
			throw e;
		} finally {
			query.onFinally();
			if(inputDB != null) {
				closeConnection(inputDB);
				inputDB = null;
			}
			if(defaultDB != null) {
				closeConnection(defaultDB);
				defaultDB = null;
			}
			try {
				if(writer != null) {
					writer.close();
				}
			} catch (Exception e) {
				messages.add(e.toString());
				Logger.logError(e, "Close save file failed.");
				success = false;
			}
		}
		
		return success;
	}
	
	// these private variables are used in executeNEIQA() and saveNEIQA()
	/** save the script file name used in executeNEIQA() so that saveNEIQA() knows which script was used **/
	private static String qaScriptName = null;
	/** keep track of how many times executeNEIQA() is called (need to do some setup on the first time) **/
	private static int countNEIQA = 0;
	
	/**
	 * Execute a script that performs NEI QA checks.  This involves two databases:
	 * the input CDB database and a default database that supplies data definitions.  The script 
	 * is run in the context of the input database.
	 * ##defaultdb## is the name of the default database
	 * @param scriptPath file path to NEI QA Script
	 * @param inputDatabase input database to be used
	 * @param defaultDatabase database providing missing data
	 * @param saveFile file object to receive the output data
	 * @throws FileNotFoundException when the script file cannot be found
	 * @throws IOException when the script file cannot be read
	 * @throws SQLException when a database cannot be accessed or created
	**/
	public static boolean executeNEIQA(File qaScript, DatabaseSelection inputDatabase, DatabaseSelection defaultDatabase)
			throws FileNotFoundException, IOException, SQLException {
		Connection inputDB = null;
		Connection defaultDB = null;
		boolean success = false;
		String sql;
		
		// save for use in saveNEIQA()
		qaScriptName = qaScript.getName();
		
		try {
			// make connection to input db (this is the db the script will be run against)
			inputDB = inputDatabase.openConnectionOrNull();
			if(inputDB == null) {
				throw new SQLException("Unable to connect to input database " + inputDatabase.databaseName);
			}
			
			// make sure default database is accessible, but we don't need to keep the connection open
			defaultDB = defaultDatabase.openConnectionOrNull();
			if(defaultDB == null) {
				throw new SQLException("Unable to connect to default database " + defaultDatabase.databaseName);
			}
			closeConnection(defaultDB);
			defaultDB = null;
			
			// drop the results database if this is the first time we are run (start with a clean slate)
			if(countNEIQA == 0) {
				SQLRunner.executeSQL(inputDB, "DROP DATABASE IF EXISTS `all_cdb_checks`");
			}
			countNEIQA++;
			
			// list all strings that need to be replaced in the script here
			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			replacements.put("##defaultdb##",defaultDatabase.databaseName);
			
			// run it!
			executeScript(inputDB,qaScript,replacements,false);
			success = true;
			
		} finally {
			if(inputDB != null) {
				closeConnection(inputDB);
				inputDB = null;
			}
			if(defaultDB != null) {
				closeConnection(defaultDB);
				defaultDB = null;
			}
		}
		
		return success;
	}

	/**
	 * Save the result of the NEI QA checks.  
	 * @param defaultDatabase to make a connection to 
	 * @param saveFile file object to receive the output data
	 * @throws SQLException when a database cannot be accessed
	 * @throws Exception if there are errors accessing or creating the output file
	**/
	public static boolean saveNEIQA(DatabaseSelection defaultDatabase, File saveFile)
			throws SQLException, Exception {
		Connection defaultDB = null;
		CellFileWriter writer = null;
		boolean success = false;
		String sql;
		
		try {
			// make connection to database
			defaultDB = defaultDatabase.openConnectionOrNull();
			if(defaultDB == null) {
				throw new SQLException("Unable to connect to default database " + defaultDatabase.databaseName);
			}

			// CellFileWriter can handle many file types, including .xlsx, .xls, .csv, and .txt
			writer = new CellFileWriter(saveFile, qaScriptName);
			
			// results are stored in a table name dependent on which set of checks were run
			if(qaScriptName.toLowerCase().contains("nonroad")) {
				sql = "select * from all_cdb_checks.all_nrcdb_checks";
			} else {
				sql = "select * from all_cdb_checks.all_cdb_checks";
			}
			
			// write results
			writer.writeSQLResults(defaultDB, sql, null);
			success = true;
			
		} finally {
			if(defaultDB != null) {
				closeConnection(defaultDB);
				defaultDB = null;
			}
			try {
				if(writer != null) {
					writer.close();
				}
			} catch (Exception e) {
				Logger.logError(e, "Could not close " + saveFile.getName());
				success = false;
			}
		}
		
		return success;
	}

	/**
	 * Validates if server name is valid
	 * @param server name to be checked
	 * @return false if server name is invalid; else true.
	**/
	public static boolean isServerNameValid(String serverName) {
		try {
			InetAddress inet = InetAddress.getByName(serverName);
		} catch (UnknownHostException e) {
			// sererName is invalid or unreachable.
			return false;
		}
		return true;
	}

	/**
	 * Validates if database name is valid
	 * @param database name to be checked
	 * @return false if database name is invalid; else true.
	**/
	public static boolean isDatabaseNameValid(String databaseName) {
		if (databaseName.length() == 0)
			return false;
		// MYSQL only allows alpha, alpha and numeric, and optionally with dollar sign or underscore.
		String strReGex = "[a-zA-Z0-9$_]*";
        boolean testResult = Pattern.matches(strReGex, databaseName);
        if (testResult) {
        	try {
                double d = Double.parseDouble(databaseName);
                return false; // value cannot be a number without alphabetic characters
            } catch (NumberFormatException nfe) {
                // no impact as value is not a number
            }
        }
		return testResult;
	}
	
}
