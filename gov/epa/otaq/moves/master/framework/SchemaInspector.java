/**************************************************************************************************
 * @(#)SchemaInspector.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.io.*;
import java.sql.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;

/**
 * This class contains various utilities to test a variety of Schema related functionalities:
 * <br> Inspects the schema of a given database and determines whether or not the desired schema is
 * present. 
 * <br> Parses the CreateDefault.sql, creates and executes corresponding SELECT statements.
 * <br> Converts the Create statement to Select statement.
 * <br> Gets a completed SQL statement from the given text block. This must be terminated by a 
 * single ';' character. 
 * <br> Gets a table name from a query.
 * <br> Trims SQL End-of-Line comments from the SQL statements.
 * <br> Checks if the correct schema is present on the Default, Execution, and Output databases.
 * <br> Execute all the select statements in an SQL Vector 
 * 
 * @author		Wesley Faler
 * @author 		Tim Hull
 * @version		2013-01-17
*/
public class SchemaInspector {
	/**
	 * Schemas read from script files.  Keyed by the script filename as passed to
	 * executeScript.  Data is another TreeMapIgnoreCase, this one keyed by table
	 * name with each data a TreeSetIgnoreCase of column names.
	**/
	static private TreeMapIgnoreCase cachedScripts = new TreeMapIgnoreCase();

	/*
	 * Checks whether or not the MOVES schema is present.
	 * @return Whether the schema is detected or not.
	 * @param testConnection The database connection to test.
	 * @param shouldSuppressSQLExceptionLogging Boolean value indicates whether to log
	 * @param missingTableAllowed - indicates whether or not it is acceptable for a table
	 * in the script to not be present in the database. If it is in the database, then all 
	 * columns must be present.
	*/
	public static boolean isMOVESSchemaPresent(Connection testConnection
			, boolean shouldSuppressSQLExceptionLogging
			, boolean missingTableAllowed ) {
		return isMOVESSchemaPresent(testConnection,shouldSuppressSQLExceptionLogging,
				missingTableAllowed,null);
	}

	/*
	 * Checks whether or not the NonRoad schema is present.
	 * @return Whether the schema is detected or not.
	 * @param testConnection The database connection to test.
	 * @param shouldSuppressSQLExceptionLogging Boolean value indicates whether to log
	 * @param missingTableAllowed - indicates whether or not it is acceptable for a table
	 * in the script to not be present in the database. If it is in the database, then all 
	 * columns must be present.
	*/
	public static boolean isNonRoadSchemaPresent(Connection testConnection
			, boolean shouldSuppressSQLExceptionLogging
			, boolean missingTableAllowed ) {
		return isNonRoadSchemaPresent(testConnection,shouldSuppressSQLExceptionLogging,
				missingTableAllowed,null);
	}
		
	/*
	 * Checks whether or not the MOVES schema is present.
	 * @return Whether the schema is detected or not.
	 * @param testConnection The database connection to test.
	 * @param shouldSuppressSQLExceptionLogging Boolean value indicates whether to log
	 * @param missingTableAllowed - indicates whether or not it is acceptable for a table
	 * in the script to not be present in the database. If it is in the database, then all 
	 * columns must be present.
	 * @param databaseName name of the database being scanned.  May be filled or null,
	 * but should never be empty.
	*/
	public static boolean isMOVESSchemaPresent(Connection testConnection
			, boolean shouldSuppressSQLExceptionLogging
			, boolean missingTableAllowed, String databaseName ) {
		try {
			boolean b ;

			b = executeScript(testConnection, new File("database/CreateDefault.sql") , 
					missingTableAllowed, databaseName );

			// If the script executed without exceptions, then all required tables and
			// columns are present (at a minimum).
			return b ;
		} catch(FileNotFoundException e) {
			/**
			 * @explain While checking the validity of an input database, a required file,
			 * database/CreateDefault.sql, could not be found.
			**/
			Logger.logError(e, "The Schema Inspector could not find an internal file " +
					"(CreateDefault.sql)");
			// return true since we can't say for sure that the database is missing information.
			return true;
		} catch(IOException e) {
			/**
			 * @explain While checking the validity of an input database, a required file,
			 * database/CreateDefault.sql, could not be read.
			**/
			Logger.logError(e, "The Schema Inspector could not read an internal file " +
					"(CreateDefault.sql).");
			// return true since we can't say for sure that the database is missing information.
			return true;
		} catch(Exception e) {
			// something went wrong with the SQL, so there must be missing tables/columns.
			if(!shouldSuppressSQLExceptionLogging) {
				/**
				 * @issue The Schema Inspector could not verify the MOVES Schema.
				 * @explain A database error occurred while inspecting an input database for
				 * validity.  The database will not be used.
				**/
				Logger.logSqlError(e, "The Schema Inspector could not verify the MOVES Schema.",
						"(unknown)");
				Logger.logException(LogMessageCategory.ERROR,e);
			}
			return false;
		}
	}

	/*
	 * Checks whether or not the NonRoad schema is present.
	 * @return Whether the schema is detected or not.
	 * @param testConnection The database connection to test.
	 * @param shouldSuppressSQLExceptionLogging Boolean value indicates whether to log
	 * @param missingTableAllowed - indicates whether or not it is acceptable for a table
	 * in the script to not be present in the database. If it is in the database, then all 
	 * columns must be present.
	 * @param databaseName name of the database being scanned.  May be filled or null,
	 * but should never be empty.
	*/
	public static boolean isNonRoadSchemaPresent(Connection testConnection
			, boolean shouldSuppressSQLExceptionLogging
			, boolean missingTableAllowed, String databaseName ) {
		try {
			boolean b ;

			b = executeScript(testConnection, new File("database/CreateNRDefault.sql") , 
					missingTableAllowed, databaseName );

			// If the script executed without exceptions, then all required tables and
			// columns are present (at a minimum).
			return b ;
		} catch(FileNotFoundException e) {
			/**
			 * @explain While checking the validity of an input database, a required file,
			 * database/CreateNRDefault.sql, could not be found.
			**/
			Logger.logError(e, "The Schema Inspector could not find an internal file " +
					"(CreateNRDefault.sql)");
			// return true since we can't say for sure that the database is missing information.
			return true;
		} catch(IOException e) {
			/**
			 * @explain While checking the validity of an input database, a required file,
			 * database/CreateNRDefault.sql, could not be read.
			**/
			Logger.logError(e, "The Schema Inspector could not read an internal file " +
					"(CreateNRDefault.sql).");
			// return true since we can't say for sure that the database is missing information.
			return true;
		} catch(Exception e) {
			// something went wrong with the SQL, so there must be missing tables/columns.
			if(!shouldSuppressSQLExceptionLogging) {
				/**
				 * @issue The Schema Inspector could not verify the MOVES Schema.
				 * @explain A database error occurred while inspecting an input database for
				 * validity.  The database will not be used.
				**/
				Logger.logSqlError(e, "The Schema Inspector could not verify the NonRoad Schema.",
						"(unknown)");
				Logger.logException(LogMessageCategory.ERROR,e);
			}
			return false;
		}
	}

	/**
	 * Retrieve the schema stored in a script file's CREATE TABLE statements.
	 * The scripts are cached so when needed again, no file access is performed.
	 * @param scriptFile file containing CREATE TABLE statements to be scanned.
	 * @return TreeMapIgnoreCase keyed by each table name, with data being a
	 * TreeSetIgnoreCase of the column names within each table.
	 * @throws FileNotFoundException if the script file cannot be found
	 * @throws IOException if anything goes wrong reading the file
	**/
	public static TreeMapIgnoreCase readScript(File scriptFile)
			throws FileNotFoundException, IOException {
		String filePath = scriptFile.getCanonicalPath();
		synchronized(cachedScripts) {
			TreeMapIgnoreCase result = (TreeMapIgnoreCase)cachedScripts.get(filePath);
			if(result != null) {
				return result;
			}
			result = new TreeMapIgnoreCase();
			BufferedReader sqlReader = null;
			String currentSQLStatement = "";
	
			try {
				sqlReader = new BufferedReader(new FileReader(scriptFile));

				while(true) {
					String iterLine = sqlReader.readLine();
					if(iterLine == null) {
						break;
					}
					iterLine = trimSQLEOLComment(iterLine);
					currentSQLStatement += iterLine;
					String entireSQLStatement =	getCompleteSQLStatement(currentSQLStatement);
					if(entireSQLStatement == null) {
						currentSQLStatement += " ";
						continue;
					}

					currentSQLStatement = "";
	
					String[] lines = parseCreateTable(entireSQLStatement) ;
					if(lines == null) { // The statement is not a CREATE TABLE
						continue ;
					}
					
					String tableName = lines[0];
					if(tableName == null || tableName.length() == 0) {
						continue;
					}
	
					TreeSetIgnoreCase columnsInFile = new TreeSetIgnoreCase();
					for(int i=1;i<lines.length;i++) {
						if(lines[i] == null || lines[i].length() <= 0) {
							continue;
						}
						if(lines[i].equalsIgnoreCase("isUserInput")) {
							continue;
						}
						columnsInFile.add(lines[i]);
					}

					result.put(tableName,columnsInFile);
				}

				cachedScripts.put(filePath,result);
				return result;
			} finally {
				if(sqlReader != null) {
					try {
						sqlReader.close();
					} catch(Exception e) {
						// Nothing to do here
					}
					sqlReader = null;
				}
			}
		}
	}
	/**
	 * Execute Script : Parse a SQL file that creates tables and creates corresponding Select statements,
	 * executing those statements.
	 * @param testConnection The database connection to test.
	 * @param scriptFile table creation file which contains all the create statments.
	 * @param missingTableAllowed indicates whether or not it is acceptable for a table
	 * in the script to not be present in the database. If it is in the database, then all 
	 * columns must be present.
	 * @param databaseName name of the database being scanned.  May be filled or null,
	 * but should never be empty.
	**/
	public static boolean executeScript(Connection testConnection, File scriptFile 
			, boolean missingTableAllowed, String databaseName )
			throws FileNotFoundException, IOException, Exception {
		if(databaseName != null && databaseName.length() <= 0) {
			databaseName = null;
		}
//Logger.log(LogMessageCategory.INFO,"Checking schema on " + testConnection.getCatalog() 
//		+ "/" + databaseName + " with script file " + scriptFile.getName());

		DatabaseMetaData dmd = null;
		ResultSet rs = null;
		Statement statement = null;

		// keyed by table name, data is TreeSetIgnoreCase of column names
		TreeMapIgnoreCase schemaInFile = readScript(scriptFile);
		if(schemaInFile == null || schemaInFile.size() <= 0) {
//Logger.log(LogMessageCategory.ERROR,"Unable to read schema from script file");
			return false;
		}

		// keyed by table name, data is TreeSetIgnoreCase of column names
		TreeMapIgnoreCase schemaInDb = new TreeMapIgnoreCase();

		TreeSetIgnoreCase tablesInDb = new TreeSetIgnoreCase();

		try {
			String currentSQLStatement = "";

			statement = testConnection.createStatement();
			rs = SQLRunner.executeQuery(statement,"show tables");
			while(rs.next()) {
				tablesInDb.add(rs.getString(1));
			}
			rs.close();
			rs = null;
			statement.close();
			statement = null;

			int columnCountScanned = 0;
			int tableCountScanned = 0;
/*
			dmd = testConnection.getMetaData();
			rs = dmd.getColumns( databaseName , "" , "%" , "%" );
			if(rs != null) {
				while (rs.next()) {
					columnCountScanned++;
					String tableName = rs.getString(3);
					String columnNameFromDb = rs.getString(4);
					if(columnNameFromDb.equalsIgnoreCase("isUserInput")) {
						continue;
					}
					if(tableName == null || columnNameFromDb == null) {
						continue ;
					}
					TreeSetIgnoreCase t = (TreeSetIgnoreCase)schemaInDb.get(tableName);
					if(t == null) {
						t = new TreeSetIgnoreCase();
						schemaInDb.put(tableName,t);
						tableCountScanned++;
					}
					t.add(columnNameFromDb);
				}
				rs.close() ;
				rs = null;
			}
*/
			for(Iterator i=tablesInDb.iterator();i.hasNext();) {
				String tableName = (String)i.next();
				if(!schemaInFile.containsKey(tableName)) {
					continue;
				}
				TreeSetIgnoreCase t = (TreeSetIgnoreCase)schemaInDb.get(tableName);
				if(t == null) {
					t = new TreeSetIgnoreCase();
					schemaInDb.put(tableName,t);
				}

				tableCountScanned++;
				statement = testConnection.createStatement();
				rs = SQLRunner.executeQuery(statement,"describe " + tableName);
				while(rs.next()) {
					columnCountScanned++;
					String columnNameFromDb = rs.getString(1);
					if(columnNameFromDb.equalsIgnoreCase("isUserInput")) {
						continue;
					}
					t.add(columnNameFromDb);
				}
				rs.close() ;
				rs = null;
				statement.close();
				statement = null;
			}
//Logger.log(LogMessageCategory.INFO,"Scanned " + columnCountScanned + " column defintions in the database");
//Logger.log(LogMessageCategory.INFO,"Scanned " + tableCountScanned + " table defintions in the database");

			Set<String> fileTables = schemaInFile.keySet();
			Set<String> databaseTables = schemaInDb.keySet();

			// Each table that is in the database must match the definition table
			for(Iterator<String> d=databaseTables.iterator();d.hasNext();) {
				String tableName = (String)d.next();
				TreeSetIgnoreCase columnsInFile = (TreeSetIgnoreCase)schemaInFile.get(tableName);
				if(columnsInFile == null) {
					continue;
				}
				TreeSetIgnoreCase columnsInDb = (TreeSetIgnoreCase)schemaInDb.get(tableName);
				for(Iterator<String> i=columnsInFile.iterator();i.hasNext();) {
					String t = (String)i.next();
					if(!columnsInDb.contains(t)) {
						/**
						 * @issue [*].[*] not in database
						 * @explain An input database does not contain a required column.
						 * The database could be incomplete or out of date.
						**/
						Logger.log(LogMessageCategory.ERROR,tableName + "." + t + " not in database");
						return false;
					}
				}
				for(Iterator<String> i=columnsInDb.iterator();i.hasNext();) {
					String t = (String)i.next();
					if(!columnsInFile.contains(t)) {
						/**
						 * @issue [*].[*] not in default script
						 * @explain An input database table contains a column that is not
						 * in the expected definition.  The database cannot be used to
						 * provide data.
						**/
						Logger.log(LogMessageCategory.ERROR,tableName + "." + t + " not in default script");
						return false;
					}
				}
			}

			// If no missing tables are allowed, then each definition table must have
			// a corresponding database table.
			if(!missingTableAllowed) {
				for(Iterator<String> f=fileTables.iterator();f.hasNext();) {
					String tableName = (String)f.next();
					if(!schemaInDb.containsKey(tableName)) {
						/**
						 * @issue Table does not exist: [*]
						 * @explain An input database does not contain a required table.
						 * The database could be incomplete or out of date.
						**/
						Logger.log(LogMessageCategory.ERROR,"Table does not exist: " + tableName);
						return false;
					}
				}
			}
//Logger.log(LogMessageCategory.INFO,"Schema fully found.");				
			return true;
		} catch( Exception ex ) {
//ex.printStackTrace();
//Logger.log(LogMessageCategory.ERROR,ex.getMessage());
throw ex;
//			return true ;
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
	}

	/**
	 * Parse a CREATE TABLE statement into its components.
	 * @param sqlStatement a CREATE TABLE statement
	 * @return an array of tokens in the statement
	**/
	public static String[] parseCreateTable( String sqlStatement ) {
		StringBuffer sb , sb2 ;
		StringTokenizer statement ;
		String lines [] ;
		String returnLines [] ;
		String columns [] ;
		int numTokens , line , location , pos , returnLine = 0 ;
		String s , s1 , s2 , tableName , sql ;
		char c ;
		
		if ( sqlStatement == null ) {
			return null ;
		}
		
		sql = sqlStatement.replace( '\n' , ' ' ) ;
		sb = new StringBuffer( sql ) ;
		sb2 = new StringBuffer() ;
		
		statement = new StringTokenizer( sql , "\t \n(" ) ;
		lines = new String [ statement.countTokens() + 10 ] ;
		columns = new String [ statement.countTokens() + 10 ] ;

		numTokens = statement.countTokens() ;
		for ( line = 0 ; statement.hasMoreTokens() == true ; line++ ) {
			lines[ line ] = statement.nextToken() ;
		}
		
		s1 = lines[ 0 ].toUpperCase() ;
		s2 = lines[ 1 ].toUpperCase() ;
		tableName = lines[ 2 ];
		
		if ( s1.equals( "CREATE" ) == false || s2.equals( "TABLE" ) == false ) {
			return null ;
		}

		for ( location = pos = 0 ; pos < sb.length() ; pos++ ) {
			c = sb.charAt( pos ) ;
			if ( c == '\n' || c == '\t' ) c = ' ' ;
			if ( location == 0 && c != '(' ) {
				continue ;
			}
			
			if ( location > 1 && c != ')' ) {
				continue ;
			}
			
			if ( c == '(' ) {
				location++ ;
				continue ;
			}

			if ( c == ')' ) {
				location-- ;
				continue ;
			}

			sb2.append( c ) ;				
		}

		statement = new StringTokenizer( sb2.toString() , "," ) ;
		lines = new String [ statement.countTokens() + 10 ] ;
		numTokens = statement.countTokens() ;
		returnLines = new String [ numTokens + 1 ] ;
		
		for ( line = 0 ; statement.hasMoreTokens() == true ; line++ ) {
			lines[ line ] = statement.nextToken() ;
		}

		returnLines[ 0 ] = tableName ;
		returnLine = 1 ;
		
		for ( line = 0 ; line < numTokens ; line++ ) {
			s = lines[line].trim() ;
			if(s.startsWith(")")) {
				continue;
			}
			if(s.indexOf('`') >= 0) {
				while(s.indexOf('`') >= 0) {
					s = s.replace('`',' ');
				}
				s = s.trim();
			}
			if(s.length() <= 0) {
				continue;
			}
			pos = s.indexOf(' ');
			if(pos < 0) {
				pos = s.indexOf('\t');
				if(pos < 0) {
					continue;
				}
			}
			String firstWord = s.substring(0,pos);
			if(firstWord.equalsIgnoreCase("key")
					|| firstWord.equalsIgnoreCase("primary")
					|| firstWord.equalsIgnoreCase("unique")
					|| firstWord.equalsIgnoreCase("index")) {
				continue;
			}
				
			returnLines[returnLine] = firstWord;
			returnLine++ ;
		}

		return returnLines ;
	}

	/**
	 * Gets a completed SQL statement from the given text block. This must be terminated
	 * by a single ';' character. This doesn't handle multiple statements within the same
	 * text block. This can potentially fail if using a database system where ';' isn't an
	 * SQL terminator or if the line is in the middle of a multi-line quoted string.
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
	 * Converts the Create statement to Select statement.
	 * @param sqlVector contains all create statements to be converted.
	 * @return selectSQLVector, vector contains select statements.
	**/
	public static Vector<String> convertToSelect(Vector sqlVector) {
		String tableName = "";
		Vector<String> selectSQLVector = new Vector<String>();
		for(int i=0; i<sqlVector.size(); i++) {
			tableName = "";
			String sql = (String) sqlVector.elementAt(i);
			tableName = getTableName(sql);
			if(tableName == null) {
				Logger.log(LogMessageCategory.ERROR, "Table name is NULL for the query : " + sql);
			}
			int index = sql.indexOf("(");
			sql = sql.substring(index+1, sql.length());
			StringTokenizer tokenizer = new StringTokenizer(sql, ",");
			String columnName = "";
			Vector<String> columnVector = new Vector<String>();
			while(tokenizer.hasMoreElements()) {
				String token = tokenizer.nextToken();
				StringTokenizer st = new StringTokenizer(token);
				if(st.hasMoreElements()) {
					String column = st.nextToken();
					columnVector.add(column);
				}
				//columnVector.add();
			}
			String selectSQL = "SELECT " ;
			for(int j=0; j<columnVector.size(); j++) {
				String column = (String) columnVector.elementAt(j);
				selectSQL += column;
				if(j != columnVector.size()-1) {
					selectSQL += ",";
				}
			}
			selectSQL += " FROM " + tableName + " LIMIT 0";
			selectSQLVector.add(selectSQL);
		}
		return selectSQLVector;
	}

	/**
	 * This method will parse the sql statement (create) and gets the table name from the
	 * query.
	 * @param sql create statement
	 * @return tableName
	**/
	public static String getTableName(String sql) {
		String tableName = null;
		boolean gotTableName = false;
		StringTokenizer tokenizer = new StringTokenizer(sql);
		String oldValue = "";
		while(tokenizer.hasMoreTokens()) {
			String token = tokenizer.nextToken();
			if(token.equals("(") && !gotTableName) {
				tableName = oldValue;
				gotTableName = true;
			}
			oldValue = token;
		}
		return tableName;
	}

	/**
	 * This method will loop through selectSQLVector and execute all the select statements
	 * @param testConnection The database connection to test.
	 * @param selectSQLVector vector contains select statements.
	**/
	public static void executeSelectStatements(Connection testConnection, Vector selectSQLVector)
			throws SQLException {
		for(int i=0; i<selectSQLVector.size(); i++) {
			String selectStatement = (String) selectSQLVector.elementAt(i);
			SQLRunner.executeSQL(testConnection, selectStatement);
		}
	}

	/**
	 * Trims SQL End-of-Line comments from the SQL statements. This makes the output produced by
	 * the SQLRunner easier to read while allowing comments to be entered into the script file
	 * that describe what the script is doing. In-line comments are not removed allowing them
	 * to be reported in the SQLRunner output.
	 * @param text The source SQL line.
	 * @return The SQL up to the start of an End-of-Line comment.
	**/
	static String trimSQLEOLComment(String text)
	{
		boolean startComment = false;
		for (int i = 0; i<text.length(); i++) {
			char iterChar = text.charAt(i);
			if(iterChar == '-') {
				if(startComment) {
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
	 * Checks if the correct schema is present on the Default database
	 * @param testConnection A connection to a database to test.
	 * @return Boolean value indicates pass/fail.
	**/
	public static boolean isDefaultSchemaPresent(Connection testConnection) {
		return false;
	}

	/**
	 * Checks if the correct schema is present on the Execution database
	 * @param testConnection A connection to a database to test.
	 * @return Boolean value indicates pass/fail.
	**/
	public static boolean isExecutionSchemaPresent(Connection testConnection) {
		return false;
	}

	/**
	 * Checks if the correct schema is present on the Output database
	 * @param testConnection A connection to a database to test.
	 * @return Boolean value indicates pass/fail.
	**/
	public static boolean isOutputSchemaPresent(Connection testConnection) {
		return false;
	}
}
