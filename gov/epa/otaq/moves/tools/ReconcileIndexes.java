/**************************************************************************************************
 * @(#)ReconcileIndexes.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.tools;

import java.io.*;
import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.common.*;

/**
 * Generate ALTER TABLE statements to add indexes to a database so that it has at
 * least the indexes present in database/CreateDefault.sql.
 *
 * @author		Wesley Faler
 * @version		2011-03-08
**/
public class ReconcileIndexes {
	public static void main(String args[]) {
		if(args == null || args.length < 1) {
			System.out.println("ERROR: Specify a database name to reconcile.");
			return;
		}

		ReconcileIndexes r = new ReconcileIndexes(args[args.length-1]);
		r.reconcile();
	}

	Connection userDB = null;
	String userDBName = "";
	Connection referenceDB = null;
	String referenceDBName = "MOVESReconcileIndexesReference";
	ArrayList<String> referenceTableNames = new ArrayList<String>();

	/**
	 * Constructor.
	 * @param userDBNameToUse name of the database to be examined
	**/
	public ReconcileIndexes(String userDBNameToUse) {
		userDBName = userDBNameToUse;
	}

	/**
	 * Reconcile the user database with a reference database.
	 * @return true if the reconcilation occurred without exception
	**/
	public boolean reconcile() {
		try {
			System.out.println("-- Opening connection to " + userDBName);
			DatabaseSelection s = new DatabaseSelection();
			s.databaseName = userDBName;
			userDB = s.openConnectionOrNull(true);
			if(userDB == null) {
				System.out.println("ERROR: Unable to open database " + userDBName);
				return false;
			}

			System.out.println("-- Dropping " + referenceDBName);
			dropReferenceDB();
			System.out.println("-- Opening " + referenceDBName);
			openReferenceDB();
			if(referenceDB == null) {
				System.out.println("ERROR: Unable to create reference database " + referenceDBName);
			}
			System.out.println("-- Getting reference table names");
			getReferenceTableNames();
			for(Iterator<String> i=referenceTableNames.iterator();i.hasNext();) {
				String tableName = i.next();
				//System.out.println("-- Comparing " + tableName);
				compareTables(tableName);
			}
			return true;
		} catch(Exception e) {
			System.out.println("Error: " + e.getMessage());
			e.printStackTrace();
			return false;
		} finally {
			if(referenceDB != null) {
				DatabaseUtilities.closeConnection(referenceDB);
			}
			dropReferenceDB();
			if(userDB != null) {
				DatabaseUtilities.closeConnection(userDB);
			}
		}
	}

	/** Drop the reference database if it exists **/
	void dropReferenceDB() {
		Connection db = null;
		try {
			DatabaseSelection s = new DatabaseSelection();
			db = s.openConnectionOrNull(false);
			if(db == null) {
				return;
			}
			SQLRunner.executeSQL(db,"flush tables");
			SQLRunner.executeSQL(db,"drop database if exists " + referenceDBName);
		} catch(Exception e) {
			// Nothing to do here, this may happen if the database does not exist.
		} finally {
			if(db != null) {
				DatabaseUtilities.closeConnection(db);
				db = null;
			}
		}
	}

	/** Create the reference database **/
	void openReferenceDB() {
		DatabaseSelection s = new DatabaseSelection();
		s.databaseName = referenceDBName;
		if(s.safeCreateDatabase("database/CreateDefault.sql") == DatabaseSelection.NOT_CREATED) {
			return;
		}
		referenceDB = s.openConnectionOrNull();
	}

	/**
	 * Get the list of table names in the reference database.
	 * @throws SQLException if anything goes wrong
	**/
	void getReferenceTableNames() throws SQLException {
		//referenceTableNames.add("imcoverage");
		String sql = "show tables";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(referenceDB,sql);
			while(query.rs.next()) {
				referenceTableNames.add(query.rs.getString(1));
			}
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Compare a reference table with a user table, printng ALTER table statements as needed.
	 * @param tableName table to be checked
	**/
	void compareTables(String tableName) {
		try {
			//System.out.println("-- Getting user indexes on " + tableName);
			ArrayList<Index> userIndexes = getIndexes(userDB,tableName);
			//System.out.println("-- Getting reference indexes on " + tableName);
			ArrayList<Index> referenceIndexes = getIndexes(referenceDB,tableName);
			for(Iterator<Index> ri=referenceIndexes.iterator();ri.hasNext();) {
				Index rIndex = ri.next();
				boolean found = false;
				for(Iterator<Index> ui=userIndexes.iterator();ui.hasNext();) {
					Index uIndex = ui.next();
					if(rIndex.columns.equalsIgnoreCase(uIndex.columns)) {
						found = true;
						break;
					}
				}
				if(!found) {
					System.out.println("alter table " + tableName + " add key (" + rIndex.columns + ");");
				}
			}
		} catch(Exception e) {
			System.out.println("-- " + tableName + " not found in user database");
		}
	}

	static class Index {
		String creationLine = "";
		String columns = "";
	}

	/**
	 * Get indexes/keys that are on a table.
	 * @param db database to be used
	 * @param tableName table to be checked
	 * @return list of Index objects
	 * @throws SQLException if anything goes wrong, includng the table not being present
	**/
	ArrayList<Index> getIndexes(Connection db, String tableName) throws SQLException {
		String sql = "show create table " + tableName;
		SQLRunner.Query query = new SQLRunner.Query();
		ArrayList<Index> indexes = new ArrayList<Index>();
		try {
			query.open(db,sql);
			query.rs.next();
			String createSQL = query.rs.getString(2);
			query.close();
			int state = 0;
			String token = "";
			Index index = null;
			setTokenSource(createSQL);
			boolean done = false;
			while(!done) {
				//System.out.println("state=" + state);
				switch(state) {
					case 0:
						token = nextToken();
						if(token == null) {
							done = true;
						} else if(token.equals("(")) {
							state = 10;
						}
						break;
					case 10:
						token = nextToken();
						if(token == null) {
							done = true;
						} else if(token.equals(",")) {
							state = 20;
						}
						break;
					case 20:
						token = nextToken();
						if(token == null) {
							done = true;
						} else if(token.equalsIgnoreCase("primary")
								|| token.equalsIgnoreCase("unique")
								|| token.equalsIgnoreCase("index")
								|| token.equalsIgnoreCase("key")) {
							index = new Index();
							index.creationLine = token;
							state = 30;
						} else if(token.trim().length() != 0) {
							state = 10;
						}
						break;
					case 30:
						token = nextToken();
						if(token == null) {
							done = true;
						} else if(token.equals("(")) {
							index.creationLine += token;
							state = 40;
						} else {
							index.creationLine += token;
						}
						break;
					case 40:
						token = nextToken();
						if(token == null) {
							done = true;
						} else if(token.equals(")")) {
							index.creationLine += token;
							indexes.add(index);
							state = 10;
						} else {
							index.creationLine += token;
							index.columns += token.trim();
						}
						break;
				}
			}
		} finally {
			query.onFinally();
		}
		return indexes;
	}

	/** SQL statement being parsed **/
	char[] tokenSource;
	/** Current start of a token, always set to the next character to be read **/
	int tokenStart = 0;

	/**
	 * Set the source of tokens used when parsing a SQL statement.
	 * @param t CREATE TABLE statement
	**/
	void setTokenSource(String t) {
		tokenSource = t.toCharArray();
		tokenStart = 0;
	}

	/**
	 * Check against SQL delimiters
	 * @param c character to be checked
	 * @return true if c is a delimiter
	**/
	static boolean isDelimiter(char c) {
		return Character.isWhitespace(c) || c == ',' || c == '(' || c == ')' || c == ';';
	}

	/**
	 * Obtain the next token from tokenSource.
	 * @return the next token or null if none is found
	**/
	String nextToken() {
		String token = null;
		while(tokenStart < tokenSource.length) {
			char c = tokenSource[tokenStart++];
			if(c == '`') {
				continue;
			}
			if(isDelimiter(c)) {
				token = "" + c;
				break;
			} else {
				token = "" + c;
				while(tokenStart < tokenSource.length) {
					c = tokenSource[tokenStart];
					if(c == '`') {
						tokenStart++;
						continue;
					}
					if(isDelimiter(c)) {
						break;
					} else {
						token += c;
						tokenStart++;
					}
				}
				break;
			}
		}
		//System.out.println("token=\"" + token + "\"");
		return token;
	}
}
