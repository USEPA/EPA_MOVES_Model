/**************************************************************************************************
 * @(#)CellFileWriter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;
import java.io.*;
import java.sql.*;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.xssf.usermodel.*;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;

/**
 * Encapsulates writing to an XLSX, XLS, CSV, or tabbed text file under a unified API.
 * 
 * @author		Wesley Faler
 * @version		2013-11-10
**/
public class CellFileWriter {
	/** File to written into **/
	File file;
	/** Type of the file **/
	int fileType = 0;
	/** Writer for CSV and tabbed text files **/
	PrintWriter writer = null;
	/** Current line for CSV and tabbed text files **/
	String currentLine = "";
	/** Workbook for XLS files **/
	Workbook workbook = null;
	/** Sheet for XLS files **/
	Sheet sheet = null;
	/** Row for XLS files **/
	Row row = null;
	/** Creation helper used when filling XLS/XLSX cells **/
	CreationHelper xlsCreateHelper = null;
	/** column index, zero based **/
	int columnIndex = 0;
	/** row index, zero based **/
	int rowIndex = 0;

	/**
	 * Constructor
	 * @param fileToUse file to be written into
	 * @param firstTabName if the file is an XLS file, the name of the first worksheet
	 * otherwise ignored.
	 * @throws Exception if unable to open the file
	**/
	public CellFileWriter(File fileToUse, String firstTabName) throws Exception {
		file = fileToUse;
		fileType = CellFile.getFileType(file);
		if(file.exists()) {
			FileUtilities.deleteFileWithRetry(file);
			if(file.exists()) {
				throw new IOException("Unable to delete existing file: " + file.getName());
			}
		}
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			if(fileType == CellFile.XLS) {
				workbook = new HSSFWorkbook();
			} else if(fileType == CellFile.XLSX) {
				workbook = new SXSSFWorkbook(100);
			}
			xlsCreateHelper = workbook.getCreationHelper();
			sheet = workbook.createSheet(firstTabName);
		} else {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(file)));
		}
	}

	/**
	 * Obtain the file type
	 * @return the file type, one of the CSV, XLS, or TABBED_TEXT constants
	**/
	public int getFileType() {
		return fileType;
	}

	/**
	 * Ensure that the row object exists
	 * @throws Exception if unable to create a new row should one be needed
	**/
	private void ensureRowExists() throws Exception {
		if(row == null) {
			row = sheet.createRow(rowIndex);
			if(row == null) {
				throw new Exception("Unable to create XLS/XLSX row " + rowIndex);
			}
		}
	}

	/**
	 * Create an XLS/XLSX cell.
	 * @param columnIndexToUse 0-based column index to be used
	 * @return a new Cell in the current row
	 * @throws Exception if unable to create a new Cell or if unable to create a new
	 * row should one be needed
	**/
	private Cell createCell(int columnIndexToUse) throws Exception {
		ensureRowExists();
		Cell cell = row.createCell(columnIndexToUse);
		if(cell == null) {
			throw new Exception("Unable to create XLS/XLSX cell: row="
					+ rowIndex + ", col=" + columnIndexToUse);
		}
		return cell;
	}

	/**
	 * Write a value to the file and advance the cell position in the current row.
	 * The specific type of the cell is infered by the type of the value (Integer, Double,
	 * String, or other that converts to a String via .toString()).
	 * @param value value to be written.  Must be an Integer, Double, String, or convertable
	 * to String via a call to toString().  If null, the current cell is merely skipped.
	 * @throws Exception if anything goes wrong
	**/
	public void writeCell(Object value) throws Exception {
		if(value == null) {
			skipCell();
		} else if(value instanceof Integer) {
			writeIntCell(((Integer)value).intValue());
		} else if(value instanceof Double) {
			writeDoubleCell(((Double)value).doubleValue());
		} else if(value instanceof String) {
			writeTextCell((String)value);
		} else {
			writeTextCell(value.toString());
		}
	}

	/**
	 * Write a textual value to the file and advance the cell position in the current row.
	 * @param value value to be written
	 * @throws Exception if anything goes wrong
	**/
	public void writeTextCell(String value) throws Exception {
		if(value == null) {
			value = "";
		}
		if(value.length() <= 0) {
			skipCell();
			return;
		}
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			if(value.length() > 0) {
				createCell(columnIndex).setCellValue(
						xlsCreateHelper.createRichTextString(value));
			}
		} else if(fileType == CellFile.CSV) {
			if(columnIndex > 0) {
				currentLine += ",";
			}
			currentLine += "\"";
			currentLine += value;
			currentLine += "\"";
		} else {
			if(columnIndex > 0) {
				currentLine += "\t";
			}
			currentLine += value;
		}
		columnIndex++;
	}

	/**
	 * Write an integer value to the file and advance the cell position in the current row.
	 * @param value value to be written
	 * @throws Exception if anything goes wrong
	**/
	public void writeIntCell(int value) throws Exception {
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			createCell(columnIndex).setCellValue(value);
		} else if(fileType == CellFile.CSV) {
			if(columnIndex > 0) {
				currentLine += ",";
			}
			currentLine += value;
		} else {
			if(columnIndex > 0) {
				currentLine += "\t";
			}
			currentLine += value;
		}
		columnIndex++;
	}

	/**
	 * Write a double value to the file and advance the cell position in the current row.
	 * @param value value to be written
	 * @throws Exception if anything goes wrong
	**/
	public void writeDoubleCell(double value) throws Exception {
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			createCell(columnIndex).setCellValue(value);
		} else if(fileType == CellFile.CSV) {
			if(columnIndex > 0) {
				currentLine += ",";
			}
			currentLine += value;
		} else {
			if(columnIndex > 0) {
				currentLine += "\t";
			}
			currentLine += value;
		}
		columnIndex++;
	}

	/**
	 * Advance the cursor in the current row.
	**/
	public void skipCell() {
		columnIndex++;
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			// Nothing to do here
		} else if(fileType == CellFile.CSV) {
			currentLine += ",";
		} else {
			currentLine += "\t";
		}
	}

	/**
	 * Advance to the start of the next row.
	 * @throws Exception if anything goes wrong
	**/
	public void endRow() throws Exception {
		rowIndex++;
		columnIndex = 0;
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			row = null;
			if(fileType == CellFile.XLS && rowIndex > 65000) {
				//throw new IOException("XLS/XLSX row limit reached, not all data was written.");
				throw new IOException("XLS row limit reached, not all data was written.");
			}
		} else {
			writer.println(currentLine);
			currentLine = "";
		}
	}

	/**
	 * Start a new tab within the file if XLS, otherwise a new file with a
	 * name of the form "originalFileName_tabName.ext".
	 * @param tabName name of the new data tab or file.
	 * @throws Exception if anything goes wrong
	**/
	public void startTab(String tabName) throws Exception {
		flush();
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			sheet = null;
			row = null;
			sheet = workbook.createSheet(tabName);
		} else {
			writer.close();
			writer = null;
			String tabFileName = FileUtilities.getBaseFileName(file)
					+ "_" + tabName + FileUtilities.getFileExtension(file,true);
			File tabFile = new File(file.getParentFile(),tabFileName);
			if(tabFile.exists()) {
				FileUtilities.deleteFileWithRetry(tabFile);
				if(tabFile.exists()) {
					throw new IOException("Unable to delete existing file: " + tabFile.getName());
				}
			}
			writer = new PrintWriter(new BufferedWriter(new FileWriter(tabFile)));
		}
		columnIndex = 0;
		rowIndex = 0;
		currentLine = "";
	}

	/**
	 * Flush all data, moving to the beginning of a new row on the current tab.
	 * If the current row does not have any data, no new row will be started.
	**/
	public void flush() {
		if(writer != null) {
			try {
				if(currentLine.length() > 0) {
					writer.println(currentLine);
					currentLine = "";
				}
				writer.flush();
			} catch(Exception e) {
				// Nothing to do here
			}
		}
		if(columnIndex > 0) {
			columnIndex = 0;
			rowIndex++;
			row = null;
		}
		currentLine = "";
	}

	/** Flush all data and close the file **/
	public void close() {
		flush();
		if(writer != null) {
			try {
				writer.close();
			} catch(Exception e) {
				// Nothing to do here
			}
			writer = null;
		}
		sheet = null;
		if(workbook != null) {
			FileOutputStream output = null;
			try {
				output = new FileOutputStream(file);
				workbook.write(output);
			} catch(Exception e) {
				// Nothing to do here
			} finally {
				if(output != null) {
					try {
						output.close();
					} catch(Exception e) {
						// Nothing to do here
					}
				}
				output = null;
				if(fileType == CellFile.XLSX) {
					((SXSSFWorkbook)workbook).dispose();
				}
			}
			workbook = null;
		}
	}

	/**
	 * Execute an SQL query and write its results to the file in table form.
	 * @param db Database to query
	 * @param sql statement to be executed
	 * @param filter optional filter to determine record inclusion
	 * @return number of rows of data, not including the header row, written
	 * @throws Exception if anything goes wrong
	**/
	public int writeSQLResults(Connection db, String sql, ISQLFilter filter) throws Exception {
		int[] dataTypes = null; // -1:not used, 0:getString(), 1:getInt, 2:getFloat, 3:getDouble
		SQLRunner.Query query = new SQLRunner.Query();
		int rowsWritten = 0;
		try {
			//query.open(db,sql);
			query.openLargeResultSet(db,sql);
			// Get meta data about each column and output the column names
			ResultSetMetaData metaData = query.rs.getMetaData();
			if(filter != null && !filter.onOpen(metaData)) {
				return rowsWritten;
			}
			int columnCount = metaData.getColumnCount();
			dataTypes = new int[columnCount]; // default to 0 using getString()
			for(int i=0;i<dataTypes.length;i++) {
				// Get the data type for the column
				int t = metaData.getColumnType(i+1);
				switch(t) {
					case java.sql.Types.TINYINT:
					case java.sql.Types.BIGINT:
					case java.sql.Types.INTEGER:
					case java.sql.Types.SMALLINT:
						dataTypes[i] = 1;
						break;
					case java.sql.Types.FLOAT:
						dataTypes[i] = 2;
						break;
					case java.sql.Types.DOUBLE:
					case java.sql.Types.NUMERIC:
					case java.sql.Types.REAL:
						dataTypes[i] = 3;
						break;
				}
				if(dataTypes[i] >= 0) {
					writeTextCell(metaData.getColumnName(i+1));
				}
			}
			endRow();
			// Output data from each record
			while(query.rs.next()) {
				if(filter != null && !filter.canUseRow(query.rs)) {
					continue;
				}
				for(int i=0;i<dataTypes.length;i++) {
					if(dataTypes[i] == 0) {
						writeTextCell(query.rs.getString(i+1));
					} else if(dataTypes[i] == 1) {
						writeIntCell(query.rs.getInt(i+1));
					} else if(dataTypes[i] == 2) {
						writeDoubleCell(query.rs.getFloat(i+1));
					} else if(dataTypes[i] == 3) {
						writeDoubleCell(query.rs.getDouble(i+1));
					} else {
						// Do not output the cell
					}
				}
				endRow();
				rowsWritten++;
			}
		} finally {
			query.onFinally();
			if(filter != null) {
				filter.onClose();
			}
		}
		return rowsWritten;
	}
}
