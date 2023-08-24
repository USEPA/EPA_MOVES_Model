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
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

/**
 * Encapsulates writing to an XLSX, XLS, CSV, or tabbed text file under a unified API.
 * 
 * @author		Wesley Faler
 * @version		2017-09-30
**/
public class CellFileWriter {
	/**
	 * Begin accumulating tabs into a single file across multiple instances of CellFileWriter.
	 * @param multipleFileToUse file to be created
	**/
	public static void startMultipleWrite(File multipleFileToUse) {
		//System.out.println("CellFileWriter.startMultipleWrite...");
		stopMultipleWrite();
		isMultipleWrite = true;
		multipleFile = multipleFileToUse;
		multipleWriter = null; // populated upon the next instantiation of CellFileWriter.
	}

	/** Stop accumulating tabs across multiple instances of CellFileWriter. Close any open file. **/	
	public static void stopMultipleWrite() {
		//System.out.println("CellFileWriter.stopMultipleWrite...");

		// Clear the multiple-writer mode so operations will complete.
		isMultipleWrite = false;
		multipleFile = null;

		if(multipleWriter != null) {
			multipleWriter.close();
			multipleWriter = null;
		}
	}

	/** true when accumulating multiple instances of CellFileWriter into a single file **/
	static boolean isMultipleWrite = false;
	/** file accumulating tabs for multiple-write mode **/
	static File multipleFile = null;
	/** Writer for the accumulated file **/
	static CellFileWriter multipleWriter = null;

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
	/** names of tabs in the file **/
	TreeSetIgnoreCase existingTabNames = new TreeSetIgnoreCase();
	/** true when writing is authorized to the current tab **/
	boolean allowWriteToTab = true;

	/**
	 * Constructor
	 * @param fileToUse file to be written into
	 * @param firstTabName if the file is an XLS file, the name of the first worksheet
	 * otherwise ignored.
	 * @throws Exception if unable to open the file
	**/
	public CellFileWriter(File fileToUse, String firstTabName) throws Exception {
        this(fileToUse, firstTabName, false);
    }

	/**
	 * Constructor
	 * @param fileToUse file to be written into
	 * @param firstTabName if the file is an XLS file, the name of the first worksheet
	 * otherwise ignored.
     * @param append if the file already exists, should it be appended to (true) or deleted first and recreated (false)?
	 * @throws Exception if unable to open the file
	**/
	public CellFileWriter(File fileToUse, String firstTabName, boolean append) throws Exception {
		if(isMultipleWrite && fileToUse != multipleFile) {
			//System.out.println("CellFileWriter.CellFileWriter stopping multiple write");
			stopMultipleWrite();
		}
		if(isMultipleWrite && multipleWriter == null) {
			//System.out.println("CellFileWriter.CellFileWriter claiming multipleWriter");
			multipleWriter = this;
		}
		file = fileToUse;
		fileType = CellFile.getFileType(file);
		if(shouldDeferToMultipleWriter()) {
			//System.out.println("CellFileWriter.CellFileWriter deferring first tab "+firstTabName);
			multipleWriter.startTab(firstTabName);
			return;
		}
        // delete the file if it exists and we are not appending to it
		if(file.exists() && !append) {
			FileUtilities.deleteFileWithRetry(file);
			if(file.exists()) {
				throw new IOException("Unable to delete existing file: " + file.getName());
			}
		}
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			if(fileType == CellFile.XLS) {
                if(file.exists() && append) {
                    workbook = new HSSFWorkbook(new FileInputStream(file));
                } else {
                    workbook = new HSSFWorkbook();
                }
			} else if(fileType == CellFile.XLSX) {
                if(file.exists() && append) {
                    workbook = new SXSSFWorkbook(new XSSFWorkbook(new FileInputStream(file)), 100);
                } else {
                    workbook = new SXSSFWorkbook(100);
                }
			}
			xlsCreateHelper = workbook.getCreationHelper();
			if(!existingTabNames.contains(firstTabName)) {
				//System.out.println("CellFileWriter.CellFileWriter created first tab " + firstTabName);
				existingTabNames.add(firstTabName);
				allowWriteToTab = true;
				sheet = workbook.createSheet(firstTabName);
			} else {
				//System.out.println("CellFileWriter.CellFileWriter skipping first tab " + firstTabName);
				allowWriteToTab = false;
			}
		} else {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(file, append)));
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
	 * Determine when an operation should be performed by the multiple-writer instead of the
	 * current object.
	 * @return true when multiperWriter should be invoked rather than the local function.
	**/
	private boolean shouldDeferToMultipleWriter() {
		if(isMultipleWrite && this != multipleWriter) {
			return true;
		}
		return false;
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
		if(shouldDeferToMultipleWriter()) {
			multipleWriter.writeCell(value);
			return;
		}
		if(!allowWriteToTab) {
			return;
		}
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
		if(shouldDeferToMultipleWriter()) {
			multipleWriter.writeTextCell(value);
			return;
		}
		if(!allowWriteToTab) {
			return;
		}
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
		if(shouldDeferToMultipleWriter()) {
			multipleWriter.writeIntCell(value);
			return;
		}
		if(!allowWriteToTab) {
			return;
		}
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
		if(shouldDeferToMultipleWriter()) {
			multipleWriter.writeDoubleCell(value);
			return;
		}
		if(!allowWriteToTab) {
			return;
		}
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
		if(shouldDeferToMultipleWriter()) {
			multipleWriter.skipCell();
			return;
		}
		if(!allowWriteToTab) {
			return;
		}
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
		if(shouldDeferToMultipleWriter()) {
			multipleWriter.endRow();
			return;
		}
		if(!allowWriteToTab) {
			return;
		}
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
		if(shouldDeferToMultipleWriter()) {
			//System.out.println("CellFileWriter.startTab deferring " + tabName);
			multipleWriter.startTab(tabName);
			return;
		}
		flush();
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			sheet = null;
			row = null;
			if(!existingTabNames.contains(tabName)) {
				//System.out.println("CellFileWriter.startTab creating " + tabName);
				existingTabNames.add(tabName);
				allowWriteToTab = true;
				sheet = workbook.createSheet(tabName);
			} else {
				//System.out.println("CellFileWriter.startTab skipping " + tabName);
				allowWriteToTab = false;
			}
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
		if(shouldDeferToMultipleWriter()) {
			multipleWriter.flush();
			return;
		}
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
		if(isMultipleWrite) {
			// Nothing to do here, only close the multiple writer through the stopMultipleWrite() function.
			//System.out.println("CellFileWriter.close skipped as not done with multiple write.");
			return;
		}
		//System.out.println("CellFileWriter.close closing sheet and file");
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
		//System.out.println("CellFileWriter.close done");
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
		if(shouldDeferToMultipleWriter()) {
			return multipleWriter.writeSQLResults(db,sql,filter);
		}
		if(!allowWriteToTab) {
			return 0;
		}
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
