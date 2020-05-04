/**************************************************************************************************
 * @(#)CellFileToMySQLImporter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;
import java.sql.*;
import java.io.*;
import gov.epa.otaq.moves.common.*;

/**
 * Read CSV/XLS/XLSX files into MySQL tables, reformatting column names
 * to comply with MySQL rules and learning data types for the columns.
 *
 * @author		Wesley Faler
 * @version		2012-11-05
**/
public class CellFileToMySQLImporter {
	/**
	 * Convert a SAS-style column or table name to a MySQL-compliant name.
	 * @param rawName column or table name to be converted
	 * @return name suitable for use as a column or table name
	**/
	public static String getCompliantName(String rawName) {
		return rawName.replace('.','_').replace(' ','_');
	}

	public boolean doImport(Connection db, ArrayList<String> messages,
			CellFileReader reader, String tableName) throws Exception {
		String sql = "";
		// Get rid of the table in the database if it already exists
		tableName = getCompliantName(tableName);
		sql = "drop table if exists " + tableName;
		SQLRunner.executeSQL(db,sql);

		boolean success = true;
		File tempFile = new File("CellFileToMySQLImporterTemp.txt");
		if(tempFile.exists()) {
			try {
				tempFile.delete();
			} catch(Exception e) {
				// Nothing to do here
			}
			if(tempFile.exists()) {
				throw new SQLException("Unable to delete temporary file used by doImport");
			}
		}

		BufferedWriter tempWriter = null;
		// Find the column headers.  They should be the first non-blank, non-skippable
		// row in the file.
		while(!reader.isEndOfData() && reader.shouldSkipLine()) {
			reader.endRow();
		}
		if(reader.isEndOfData()) {
			messages.add("ERROR: No header row found for table " + tableName);
			return false;
		}
		ArrayList<String> columnNames = new ArrayList<String>();
		ArrayList<String> columnNamesInFile = new ArrayList<String>();
		ArrayList<Integer> sqlColumnIndexByFileIndexInt = new ArrayList<Integer>();
		ArrayList<Integer> fileColumnIndexBySQLIndexInt = new ArrayList<Integer>();

		int consecutiveBlankColumnCount = 0;
		while(true) {
			String t = reader.readStringCell();
			if(t == null) {
				break;
			}
			t = t.trim();
			if(t.length() <= 0) {
				sqlColumnIndexByFileIndexInt.add(new Integer(-1));
				columnNamesInFile.add("");
				consecutiveBlankColumnCount++;
				if(consecutiveBlankColumnCount >= 10) {
					break;
				}
			} else {
				sqlColumnIndexByFileIndexInt.add(new Integer(columnNames.size()));
				fileColumnIndexBySQLIndexInt.add(new Integer(columnNamesInFile.size()));

				t = getCompliantName(t);
				columnNamesInFile.add(t);
				columnNames.add(t);

				consecutiveBlankColumnCount = 0;
			}
		}
		reader.endRow(); // move past the header row

		int[] dataTypes = new int[columnNames.size()]; // 0:unknown, 1:string, 2:int, 3:double
		int[] stringLengths = new int[columnNames.size()]; // max length of string columns
		int[] sqlColumnIndexByFileIndex = new int[columnNamesInFile.size()];
		int[] fileColumnIndexBySQLIndex = new int[dataTypes.length];
		for(int i=0;i<sqlColumnIndexByFileIndex.length;i++) {
			sqlColumnIndexByFileIndex[i] = sqlColumnIndexByFileIndexInt.get(i).intValue();
		}
		for(int i=0;i<fileColumnIndexBySQLIndex.length;i++) {
			fileColumnIndexBySQLIndex[i] = fileColumnIndexBySQLIndexInt.get(i).intValue();
		}

		SQLRunner.Query query = new SQLRunner.Query();
		try {
			tempWriter = new BufferedWriter(new FileWriter(tempFile));
			String[] rowData = new String[columnNames.size()];
			Object[] rowObjects = new Object[columnNames.size()];
			int missingDataCount = 0;
			int filteredRowCount = 0;
			while(!reader.isEndOfData()) {
				if(reader.shouldSkipLine()) {
					reader.endRow();
				}
				for(int i=0;i<rowData.length;i++) {
					rowData[i] = null;
					rowObjects[i] = null;
				}
				for(int fi=0;fi<columnNamesInFile.size();fi++) {
					int si = sqlColumnIndexByFileIndex[fi];
					if(si < 0) {
						reader.skipCell();
						continue;
					}
					switch(dataTypes[si]) {
						default:
						case 0: // Unknown
							rowData[si] = StringUtilities.safeGetString(reader.readStringCell());
							rowObjects[si] = rowData[si];
							if(StringUtilities.isDigits(rowData[si])) {
								dataTypes[si] = 2; // int
							} else {
								try {
									Double.parseDouble(rowData[si]);
									dataTypes[si] = 3; // double
								} catch(Exception e) {
									try {
										Integer.parseInt(rowData[si]);
										dataTypes[si] = 2; // int
									} catch(Exception e2) {
										dataTypes[si] = 1; // string
										stringLengths[si] = rowData[si].length();
									}
								}
							}
							break;
						case 1: // string
							rowData[si] = StringUtilities.safeGetString(reader.readStringCell());
							if(rowData[si].length() > stringLengths[si]) {
								stringLengths[si] = rowData[si].length();
							}
							rowObjects[si] = rowData[si];
							break;
						case 2: // int
							rowData[si] = StringUtilities.safeGetString(reader.readStringCell());
							rowObjects[si] = rowData[si];
							// If any decimal points or scientific notation show up, then flag the cell as double
							if(rowData[si].indexOf('.') >= 0 || rowData[si].indexOf('e') > 0) {
								dataTypes[si] = 3; // double
							}
							break;
						case 3: // double
							rowData[si] = StringUtilities.safeGetString(reader.readStringCell());
							rowObjects[si] = rowData[si];
							break;
					}
				}
				reader.endRow();
				// Write each line to a text file, separating each field by \t and ending
				// each line with \n.  This makes the file compatible with MySQL's
				// LOAD DATA INFILE default settings.
				for(int i=0;i<rowData.length;i++) {
					if(i > 0) {
						tempWriter.write("\t");
					}
					if(rowData[i] != null) {
						tempWriter.write(rowData[i]);
					} else {
						tempWriter.write("\\N");
					}
				}
				tempWriter.write("\n");
			}
			if(success) {
				// Close the writer to the temporary file
				tempWriter.close();

				// Create the data table
				String columnNamesText = "";
				sql = "";
				for(int i=0;i<columnNames.size();i++) {
					String cn = columnNames.get(i);
					if(columnNamesText.length() > 0) {
						columnNamesText += ",";
					}
					columnNamesText += cn;

					if(sql.length() > 0) {
						sql += ", ";
					}
					sql += cn + " ";
					switch(dataTypes[i]) {
						default:
						case 0: // Unknown
							break; // this should not happen
						case 1: // string
							sql += "varchar(" + stringLengths[i] + ") null";
							break;
						case 2: // int
							sql += "int null";
							break;
						case 3: // double
							sql += "double null";
							break;
					}
				}
				sql = "create table if not exists " + tableName + "( " + sql + " )";
				SQLRunner.executeSQL(db,sql);
				
				// Truncate the table
				sql = "truncate table " + tableName;
				SQLRunner.executeSQL(db,sql);

				// Load the data
				sql = "LOAD DATA INFILE '"
						+ tempFile.getCanonicalPath().replace('\\','/') + "' REPLACE INTO TABLE "
						+ tableName + " (" + columnNamesText + ")";
				SQLRunner.executeSQL(db,sql);
				// After the import, analyze the table for performance reasons
				sql = "analyze table " + tableName;
				SQLRunner.executeSQL(db,sql);
			}
		} finally {
			query.onFinally();
			if(tempWriter != null) {
				try {
					tempWriter.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				tempWriter = null;
			}
			if(tempFile != null && tempFile.exists()) {
				try {
					tempFile.delete();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
		return success;
	}
}
