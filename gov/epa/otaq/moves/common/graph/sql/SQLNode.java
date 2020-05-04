/**************************************************************************************************
 * @(#)SQLNode.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.graph.sql;

import java.util.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.common.graph.*;

/**
 * Encapsulates a SQL statement and its required inputs and outputs.
 * 
 * @author		Wesley Faler
 * @author		Don Smith
 * @version		2013-01-19
**/
public class SQLNode extends BasicNodeImpl {
	/**
	 * Constructor
	 * @param linearPredecessorToUse The node considered the linear predecessor of this 
	 * node, may be null
	 * @param nodeTextToUse text associated with the node
	**/
	public SQLNode(INode linearPredecessorToUse, String nodeTextToUse) {
		super(linearPredecessorToUse,nodeTextToUse);
		parse(nodeTextToUse);
	}

	/** Internal utility class to aid parsing **/
	class ParseContext {
		public TreeSetIgnoreCase inputTables = new TreeSetIgnoreCase();
		public TreeSetIgnoreCase outputTables = new TreeSetIgnoreCase();

		public String[] split(String sql) {
			return standardizeAfterSplit(sql.split("\\s"));
		}

		public String[] splitFront(String sql) {
			return standardizeAfterSplit(sql.split("\\s",10));
		}

		public void addInputTable(String tableName) {
			addInputTable(tableName,null);
		}

		public void addInputTable(String tableName, String subset) {
			String keyName = tableName;
			if(subset != null) {
				keyName += "|" + subset;
			}
			if(!inputTables.contains(keyName)) {
				IResource t = createTableResource(tableName, subset);
				addInput(t);
				inputTables.add(keyName);
				//System.out.println("input: \"" + keyName + "\"");
			}
		}

		public void addOutputTable(String tableName) {
			addOutputTable(tableName,null);
		}

		public void addOutputTable(String tableName, String subset) {
			String keyName = tableName;
			if(subset != null) {
				keyName += "|" + subset;
			}
			if(!outputTables.contains(keyName)) {
				IResource t = createTableResource(tableName, subset);
				addOutput(t);
				outputTables.add(keyName);
				//System.out.println("output: \"" + keyName + "\"");
			}
		}

		public void addOutputFile(String fileName) {
			IResource t = createFileResource(fileName);
			addOutput(t);
		}

		public void addInputFile(String fileName) {
			IResource t = createFileResource(fileName);
			addInput(t);
		}
	}

	/**
	 * Parse a SQL statement, setting input and output resources as needed.
	 * @param sql statement to be parsed
	**/
	private void parse(String sql) {
		sql = standardizeStatement(sql);
		ParseContext context = new ParseContext();
		
		if(sql.startsWith("create table")) {
			parseCreateTable(sql,context);
		} else if(sql.startsWith("drop table")) {
			parseDropTable(sql,context);
		} else if(sql.startsWith("alter table")) {
			parseAlterTable(sql,context);
		} else if(sql.startsWith("truncate")) {
			parseTruncateTable(sql,context);
		} else if(sql.startsWith("flush tables")) {
			parseFlushTables(sql,context);
		} else if(sql.startsWith("select")) {
			parseSelect(sql,context);
		} else if(sql.startsWith("cache")) {
			parseSelect(sql,context);
		} else if(sql.startsWith("analyze")) {
			parseAnalyzeTable(sql,context);
		} else if(sql.startsWith("delete")) {
			parseDeleteTable(sql,context);
		} else if(sql.startsWith("update")) {
			parseUpdateTable(sql,context);
		} else if(sql.startsWith("insert")) {
			parseInsertIntoTable(sql,context);
		} else if(sql.startsWith("create index")) {
			parseCreateIndex(sql,context);
		} else if(sql.startsWith("create unique index")) {
			parseCreateIndex(sql,context);
		} else if(sql.startsWith("load data infile")) {
			parseLoadDataInFile(sql,context);
		} else {
			setAsUnknown();
		}
	}

	/**
	 * Mark the current statement as an unknown type.  This causes full synchronization
	 * before and after, safely degrading to standard synchronous operation.
	**/
	private void setAsUnknown() {
		addInput(new BasicResourceImpl("|sql|"));
		addOutput(new BasicResourceImpl("|sql|"));
	}

	/**
	 * Parse create table statements of the form:
	 * 		create table <tablename> ( .... )
	 *		create table if not exists <tablename> ( .... )
	 *		create table <tablename> select ....
	 *		create table if not exists <tablename> select ...
	 *		create table <tablename> like <othertable>
	 *		create table if not exists <tablename> like <othertable>
	 * Versions that include an embedded SELECT statement also use the parseSelect() routine.
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseCreateTable(String sql, ParseContext context) {
		int tableNameIndex = 2;
		if(sql.startsWith("create table if not exists")) {
			tableNameIndex = 5;
		}
		String[] parts = context.splitFront(sql);
		if(parts.length <= tableNameIndex) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		String tableName = parts[tableNameIndex];
		context.addInputTable(tableName);
		context.addOutputTable(tableName);
		if(parts[tableNameIndex+1].equals("select")) {
			int index = sql.indexOf(tableName) + tableName.length() + 1;
			sql = sql.substring(index);
			parseSelect(sql,context);
		} else if(parts[tableNameIndex+1].equals("like")) {
			context.addInputTable(parts[tableNameIndex+2]);
		}
	}

	/**
	 * Parse drop table statements of the form:
	 *		drop table <tablename>
	 *		drop table if exists <tablename>
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseDropTable(String sql, ParseContext context) {
		int tableNameIndex = 2;
		if(sql.startsWith("drop table if exists")) {
			tableNameIndex = 4;
		}
		String[] parts = context.splitFront(sql);
		if(parts.length <= tableNameIndex) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		String tableName = parts[tableNameIndex];
		addInput(new BasicResourceImpl("|sql|")); // require synchronization first
		context.addInputTable(tableName);
		// Model dropping a table as a form of output to the table.
		// This should prevent the drop table from being executed
		// concurrently with code that reads from the table.
		context.addOutputTable(tableName);
	}

	/**
	 * Parse delete table statements of the form:
	 *		delete table <tablename>
	 *		delete table if exists <tablename>
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseDeleteTable(String sql, ParseContext context) {
		int tableNameIndex = 1;
		if(sql.startsWith("delete from table")) {
			tableNameIndex = 3;
		}
		String[] parts = context.splitFront(sql);
		if(parts.length <= tableNameIndex) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		String tableName = parts[tableNameIndex];
		context.addInputTable(tableName);
		context.addOutputTable(tableName);
	}


	/**
	 * Parse update table statements of the form:
	 *		update <tablename>
	 *      update <tablename>, <tablename>, ...
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseUpdateTable(String sql, ParseContext context) {
		int tableNameIndex = 1;

		boolean hasSeperator = false;
		if(sql.indexOf(",") >= 0) {
			hasSeperator = true;
		}
					
		String[] parts = context.splitFront(sql);
		if(parts.length <= tableNameIndex) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		
		do{
			String tableName = parts[tableNameIndex];
			context.addInputTable(tableName);
			context.addOutputTable(tableName);
			tableNameIndex++;
			if(parts[tableNameIndex].equalsIgnoreCase(",")) {
					tableNameIndex++;
			} else {
				hasSeperator=false;						
			}
		} while(hasSeperator);
	}

	/**
	 * Parse alter table statements of the form:
	 *		alter table <tablename> .....
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseAlterTable(String sql, ParseContext context) {
		int tableNameIndex = 1;
		if(sql.startsWith("alter table")) {
			tableNameIndex = 2;
		}
		String[] parts = context.splitFront(sql);
		if(parts.length <= tableNameIndex) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		String tableName = parts[tableNameIndex];
		context.addInputTable(tableName);
		context.addOutputTable(tableName);
	}

	/**
	 * Parse analyze table statements of the form:
	 *		analyze table <tablename> .....
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseAnalyzeTable(String sql, ParseContext context) {
		int tableNameIndex = 1;
		if(sql.startsWith("analyze table")) {
			tableNameIndex = 2;
		}
		String[] parts = context.splitFront(sql);
		if(parts.length <= tableNameIndex) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		String tableName = parts[tableNameIndex];
		context.addInputTable(tableName);
		context.addOutputTable(tableName);
	}

	/**
	 * Parse truncate table statements of the form:
	 *		truncate table <tablename>
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseTruncateTable(String sql, ParseContext context) {
		int tableNameIndex = 1;
		if(sql.startsWith("truncate table")) {
			tableNameIndex = 2;
		}
		String[] parts = context.splitFront(sql);
		if(parts.length <= tableNameIndex) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		String tableName = parts[tableNameIndex];
		addInput(new BasicResourceImpl("|sql|")); // require synchronization first
		context.addInputTable(tableName);
		context.addOutputTable(tableName);
	}

	/**
	 * Parse flush table statements of the form:
	 *		flush tables
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseFlushTables(String sql, ParseContext context) {
		addInput(new BasicResourceImpl("|sql|"));
		addOutput(new BasicResourceImpl("|sql|"));
	}

	/**
	 * Parse select table statements of the form:
	 *		select into outfile '<filename>' [columns] from ....
	 *		select [columns] from ....
	 *		(and more, including a mixture of inner join, left join, left outer join, and direct table names)
	 * 		select from table
	 * 		select from ... JOIN table
	 * 		select from ... ( skip )	(there might be commas in the middle)
	 * 		select from ... , table		(but only if not within parenthesis, see prior rule)
	 * Quit if find WHERE, GROUP, ORDER, or LIMIT
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseSelect(String sql, ParseContext context) {
		String[] parts = context.split(sql);
		String tableJoinName = "";
		boolean hasFrom = false;
		boolean ignore = false;
		boolean done = false;

		for( int i=0; i<parts.length && !done; i++ ){
			if( parts[i].equalsIgnoreCase("where")){
				done = true;
				break;
			}
			if( parts[i].equalsIgnoreCase("group")){
				done = true;
				break;
			}
			if( parts[i].equalsIgnoreCase("order")){
				done = true;
				break;
			}
			if( parts[i].equalsIgnoreCase("limit")){
				done = true;
				break;
			}
			if( parts[i].equalsIgnoreCase("(")){
				ignore = true;
			}
			if( parts[i].equalsIgnoreCase(")")){
				ignore = false;
			}
			if( ignore == false ){
				if( parts[i].equalsIgnoreCase("outfile")){
					String outputFileName = parts[i+1];
					outputFileName = outputFileName.substring(1,outputFileName.length()-1);
					context.addOutputFile(outputFileName);
				}
				if( parts[i].equalsIgnoreCase("from")){
					hasFrom = true;
					context.addInputTable(parts[i+1]);
				}
				if( parts[i].equalsIgnoreCase(",") && hasFrom ){
					context.addInputTable(parts[i+1]);
				}
				if( parts[i].equalsIgnoreCase("join")){
					context.addInputTable(parts[i+1]);
				}
			}
		}
	}

	/**
	 * Parse insert table statements of the form:
	 *		insert into <tablename> .....
	 *		insert ignore into <tablename> .....
	 *		insert replace into <tablename> .....
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseInsertIntoTable(String sql, ParseContext context) {
		String[] parts = context.split(sql);
		String tableName ="";
		if(parts.length <= 1) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		for( int i=0; i<parts.length; i++ ){
			if(parts[i].equals("select")) {
				sql = sql.substring(i);
				parseSelect(sql,context);
			} else
			if( parts[i].equalsIgnoreCase("into")){
				tableName = parts[i+1];
				context.addInputTable(tableName);
				context.addOutputTable(tableName);
			}
		}
	}


	/**
	 * Parse create index statements of the form:
	 *		create index .....
	 *		create unique index .....
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseCreateIndex(String sql, ParseContext context) {
		String[] parts = context.split(sql);
		if(parts.length <= 1) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		for( int i=0; i<parts.length; i++ ){
			if( parts[i].equalsIgnoreCase("on")){
				String tableName = parts[i+1];
				context.addInputTable(tableName);
				context.addOutputTable(tableName);
				break;
			}else{
			}
		}
	}


	/**
	 * Parse load data infile statements of the form:
	 *		load data infile <filename> into <tablename> .....
	 * @param sql standardized SQL to be examined
	 * @param context parsing context
	**/
	private void parseLoadDataInFile(String sql, ParseContext context) {
		String[] parts = context.split(sql);
		if(parts.length <= 1) {
			// There aren't enough clauses to decode
			setAsUnknown();
			return;
		}
		for( int i=0; i<parts.length; i++ ){
			if( parts[i].equalsIgnoreCase("infile")){
				String inputFileName = parts[i+1];
				inputFileName = inputFileName.substring(1,inputFileName.length()-1);
				context.addInputFile(inputFileName);
			}
			if( parts[i].equalsIgnoreCase("table")){
				String tableName = parts[i+1];
				context.addInputTable(tableName);
				context.addOutputTable(tableName);
				break;
			}else{
			}
		}
	}

	/**
	 * Standardize a statement, trimming trailing and embedded whitespace, converting
	 * tab characters, and making the entire statement lowercase.  The resulting statement 
	 * may not be valid SQL and is intended only to be easier to quickly parse than the 
	 * input statement.
	 * @param sql statement
	 * @return standardized form of the statement
	**/
	public static String standardizeStatement(String sql) {
		if(sql.endsWith(";")) {
			sql = sql.substring(0,sql.length()-1);
		}
		sql = sql.replace('`',' ');
		// Convert quote-delimited regions by changing spaces to a special character.  This
		// ensures that when split, the quoted text remains a single token.
		ArrayList<String> finalReplacements = new ArrayList<String>();
		if(sql.indexOf("'") >= 0) {
			char[] c = sql.toCharArray();
			sql = "";
			int state = 0;
			String replacement = "";
			for(int i=0;i<c.length;i++) {
				switch(state) {
					case 0: // looking for start of quoted text
						if(c[i] == '\'') {
							sql += "'REPLACEME" + finalReplacements.size() + "'";
							replacement = "'";
							state = 1;
						} else {
							sql += c[i];
						}
						break;
					case 1: // looking for end of quoted text
						if(c[i] == '\'') {
							if(i+1 < c.length && c[i+1] == '\'') {
								replacement += "''";
								i++; // skip the second quote
							} else {
								replacement += c[i];
								finalReplacements.add(replacement.toLowerCase());
								state = 0; // found the end
							}
						} else if(c[i] == ' ') {
							replacement += '\u00ff';
						} else {
							replacement += c[i];
						}
						break;
				}
			}
		}
		// Remove interior tabs and line breaks, making them spaces instead
		sql = StringUtilities.replace(StringUtilities.replace(StringUtilities.replace(sql,"\n"," "),"\r"," "),"\t"," ");
		// Convert delimiters and operators to versions that include surrounding spaces.
		// Thus "," becomes " , ".  "+" becomes " + ", etc.
		String specialMarker = "#@#@#@##@@@@@@@@####";
		char[] specialCharacters = { ',', '(', ')', '*', '/', '+', '-', '=', '<', '>' };
		for(int i=0;i<specialCharacters.length;i++) {
			if(sql.indexOf(specialCharacters[i]) >= 0) {
				sql = StringUtilities.replace(sql,""+specialCharacters[i],specialMarker);
				sql = StringUtilities.replace(sql,specialMarker," "+specialCharacters[i]+" ");
			}
		}

		// Remove interior double spaces
		sql = StringUtilities.replace(sql,"  "," ");
		// Remove extra space and make it all lowercase
		sql = sql.trim().toLowerCase();
		// Change out final replacements for string constants
		for(int i=0;i<finalReplacements.size();i++) {
			sql = StringUtilities.replace(sql,"'REPLACEME" + i + "'",finalReplacements.get(i));
		}
		return sql;
	}

	/**
	 * Finish the standardization process after splitting a previously standardized statement.
	 * Specifically, this restores internal spaces within string constants.
	 * @param parts array to be read and mofified
	 * @return the parts array
	**/
	public static String[] standardizeAfterSplit(String[] parts) {
		if(parts != null) {
			for(int i=0;i<parts.length;i++) {
				if(parts[i].indexOf('\u00ff') >= 0) {
					parts[i] = parts[i].replace('\u00ff',' ');
				}
			}
		}
		return parts;
	}

	/**
	 * Create a resource that represents a table.
	 * @param tableName table
	 * @return a resource representing the table and all subsets therein.
	**/
	public static IResource createTableResource(String tableName) {
		return createTableResource(tableName,null);
	}

	/**
	 * Create a resource that represents a table and an optional subset of data therein.
	 * @param tableName table
	 * @param subset name given to the subset of data within the data, may be null or empty
	 * @return a resource representing the table and subset
	**/
	public static IResource createTableResource(String tableName, String subset) {
		String resourceName = "|sql|" + tableName.toLowerCase() + "|";
		if(subset != null && subset.length() > 0) {
			resourceName += subset + "|";
		}
		return new BasicResourceImpl(resourceName);
	}

	/**
	 * Create a resource that represents a file.
	 * @param fileName file, may be a full path or relative just as long as all resources
	 * are referenced in the same manner
	 * @return a resource representing the file
	**/
	public static IResource createFileResource(String fileName) {
		String resourceName = "|file|" + fileName.toLowerCase() + "|";
		return new BasicResourceImpl(resourceName);
	}
}
