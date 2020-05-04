/**************************************************************************************************
 * @(#)SQLMacroExpander.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.util.*;
import java.sql.*;

/**
 * Duplicate SQL statements using macros, as needed for making tables smaller and faster.
 *
 * @author		Wesley Faler
 * @version		2015-02-14
**/
public class SQLMacroExpander {
	static class ValueSet implements PermutationCreator.IDimension {
		String id;
		String[] names;
		String[] lowerCaseNames;
		int rows = 0;
		String[] data = null;
		String[] currentValues = null;

		public ValueSet() {
			clear();
		}

		public ValueSet(String idToUse) {
			id = idToUse;
			clear();
		}

		public void clear() {
			rows = 0;
			data = null;
			currentValues = null;
		}

		public int dimensionSize() {
			return rows;
		}

		public void applyIndex(int index) {
			if(currentValues == null) {
				currentValues = new String[names.length];
			}
			if(rows <= 0) {
				for(int i=0;i<currentValues.length;i++) {
					currentValues[i] = "";
				}
			} else {
				int baseIndex = index * names.length;
				for(int i=0;i<currentValues.length;i++) {
					currentValues[i] = data[baseIndex+i];
				}
			}
		}
	}

	/** All value sets **/
	static ArrayList<ValueSet> sets = new ArrayList<ValueSet>();
	/** Names from ValueSets as keys, with the corresponding ValueSet as data. **/
	static TreeMapIgnoreCase compiledNames = null;

	/** Reset all stored macro sets **/
	public static void reset() {
		sets.clear();
	}

	/** Compile all names for quick processing **/
	public static void compile() {
		compiledNames = new TreeMapIgnoreCase();
		for(int i=0;i<sets.size();i++) {
			ValueSet set = sets.get(i);
			set.lowerCaseNames = new String[set.names.length];
			for(int j=0;j<set.names.length;j++) {
				compiledNames.put(set.names[j],set);
				set.lowerCaseNames[j] = set.names[j].toLowerCase();
			}
		}
	}

	/**
	 * Locate an existing set by ID or create a new set using the ID.
	 * @param id identifier to search
	 * @param shouldClear true if any existing data in the set should be removed
	 * @return a ValueSet object to use
	**/
	static ValueSet findOrCreate(String id, boolean shouldClear) {
		for(int i=0;i<sets.size();i++) {
			ValueSet set = sets.get(i);
			if(set.id != null && set.id.equalsIgnoreCase(id)) {
				if(shouldClear) {
					set.clear();
				}
				return set;
			}
		}
		return new ValueSet(id);
	}

	/**
	 * Add a set if it is not already in the list.
	 * @param setToAdd ValueSet to be added
	**/
	static void add(ValueSet setToAdd) {
		if(setToAdd.id != null) {
			for(int i=0;i<sets.size();i++) {
				ValueSet set = sets.get(i);
				if(set.id != null && set.id.equalsIgnoreCase(setToAdd.id)) {
					return;
				}
			}
		}
		sets.add(setToAdd);
	}

	/**
	 * Add a value set.
	 * @param prefix portion of the macro name.  Each column takes the form "##macro.PREFIXcolumn##".
	 * @param db database connection to be used
	 * @param sql statement with macro columns
	 * @throws SQLException if anything goes wrong
	**/
	public static void addData(String prefix, Connection db, String sql) throws SQLException {
		prefix = StringUtilities.safeGetString(prefix);
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			// Count rows
			query.open(db,sql);
			ValueSet set = findOrCreate(prefix + "|" + sql,true);
			while(query.rs.next()) {
				set.rows++;
			}
			query.onFinally();

			// Obtain data.  Get names here in case "select *" was used which doesn't guarantee column sequence.
			query.open(db,sql);
			ResultSetMetaData metaData = query.rs.getMetaData();
			int columnCount = metaData.getColumnCount();
			set.names = new String[columnCount];
			for(int i = 0; i < columnCount; i++) {
				set.names[i] = "##macro." + prefix + metaData.getColumnName(i + 1) + "##";
			}
			if(set.rows > 0) {
				set.data = new String[set.rows * set.names.length];
			}
			int dataIndex = 0;
			while(query.rs.next()) {
				for(int i=1;i<=columnCount;i++) {
					set.data[dataIndex++] = StringUtilities.safeGetString(query.rs.getString(i));
				}
			}

			add(set);
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Add CSV lines compatible with SQL IN clauses.  Include macros that are length limited
	 * ("##macro.csv.COLUMN##") and one that has all values ("##macro.csv.all.COLUMN##").
	 * An optional default value can be used in the event there is no data.  It is highly recommended
	 * that this be used as it prevents empty IN clauses which cause syntax errors.
	 * @param db database connection to be used
	 * @param sql statement with the macro column
	 * @param maxLength optional maximum length of each CSV line.  When set to 0 or less, no limit is used.
	 * @param shouldAddQuotes true if the retrieved data is textual and should have quotation marks placed around it.
	 * @param useDefaultValueInData true if the CSV lines should include the default value even when
	 * data has been found.  Set this to true if your data uses the default value as a form of wildcard
	 * value in the database.
	 * @param defaultValue optional default value.  Set to null if no default is desired.  Do not include quotation
	 * marks/apostrophes if shouldAddQuotes is true.  A zero length string is acceptable when shouldAddQuotes
	 * is true, otherwise it is ignored as using no default value.
	 * @throws SQLException if anything goes wrong
	**/
	public static void addCSVData(Connection db, String sql, int maxLength,
			boolean shouldAddQuotes,
			boolean useDefaultValueInData, String defaultValue) throws SQLException {
		boolean hasDefaultValue = defaultValue != null && (shouldAddQuotes || defaultValue.length() > 0);
		if(hasDefaultValue && shouldAddQuotes) {
			defaultValue = DatabaseUtilities.escapeSQL(defaultValue,true);
		}
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			ResultSetMetaData metaData = query.rs.getMetaData();
			String columnName = metaData.getColumnLabel(1);
			TreeSet<String> uniqueValues = new TreeSet<String>();
			if(hasDefaultValue) {
				uniqueValues.add(defaultValue);
			}

			while(query.rs.next()) {
				String t = query.rs.getString(1);
				if(t == null || (!shouldAddQuotes && t.length() <=0)) {
					continue;
				}
				if(shouldAddQuotes) {
					t = DatabaseUtilities.escapeSQL(t,true);
				}
				uniqueValues.add(t);
			}
			query.onFinally();

			if(!useDefaultValueInData && hasDefaultValue) {
				uniqueValues.remove(defaultValue);
			}
			String all = "", current = "";
			ArrayList<String> rows = new ArrayList<String>();
			for(Iterator<String> i=uniqueValues.iterator();i.hasNext();) {
				String t = i.next();
				if(all.length() > 0) {
					all += ",";
				}
				all += t;

				if(current.length() > 0) {
					current += ",";
				}
				current += t;
				if(maxLength > 0 && current.length() >= maxLength) {
					rows.add(current);
					current = "";
				}
			}
			if(current.length() > 0) {
				rows.add(current);
				current = "";
			}
			if(rows.size() <= 0 && hasDefaultValue) {
				rows.add(defaultValue);
				all = defaultValue;
			}

			// Add "##macro.csv.COLUMN##
			ValueSet set = findOrCreate("csv|" + sql,true);
			set.rows = rows.size();
			set.names = new String[1];
			set.names[0] = "##macro.csv." + columnName + "##";
			if(set.rows > 0) {
				set.data = new String[set.rows];
			}
			for(int i=0;i<rows.size();i++) {
				set.data[i] = rows.get(i);
			}
			add(set);

			// Add "##macro.csv.all.COLUMN##
			set = findOrCreate("csv.all|" + sql,true);
			if(all.length() > 0) {
				set.rows = 1;
				set.names = new String[1];
				set.names[0] = "##macro.csv.all." + columnName + "##";
				set.data = new String[1];
				set.data[0] = all;
				//Logger.log(LogMessageCategory.INFO,set.names[0]+"="+all);
			}
			add(set);
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Expand a string if it contains any macro elements.  The expanded strings are added
	 * to an accumulated list of lines.  If no macro elemets are found, the raw line is
	 * added to the accumulated list.
	 * @param rawLine candiate line
	 * @param lines accumulated lines
	**/
	public static void expandAndAdd(String rawLine, LinkedList<String> lines) {
		// Find all sets used by the statement.
		if(rawLine.indexOf("##macro.") < 0) { // If macros aren't used, don't bother doing more.
			lines.add(rawLine);
			return;
		}
		PermutationCreator p = null;
		ArrayList<ValueSet> usedSets = null;
		String lowerCaseRawLine = rawLine.toLowerCase();
		for(int i=0;i<sets.size();i++) {
			ValueSet set = sets.get(i);
			for(int j=0;j<set.lowerCaseNames.length;j++) {
				if(lowerCaseRawLine.indexOf(set.lowerCaseNames[j]) >= 0) {
					if(p == null) {
						p = new PermutationCreator();
						usedSets = new ArrayList<ValueSet>();
					}
					p.add(set);
					usedSets.add(set);
					break;
				}
			}
		}
		if(p == null) {
			lines.add(rawLine);
			return;
		}
		p.start();
		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		do {
			replacements.clear();
			for(Iterator<ValueSet> i=usedSets.iterator();i.hasNext();) {
				ValueSet set = i.next();
				for(int j=0;j<set.names.length;j++) {
					replacements.put(set.names[j],set.currentValues[j]);
				}
			}
			String t = StringUtilities.doReplacements(rawLine,replacements);
			lines.add(t);
		} while(p.next());
	}
}
