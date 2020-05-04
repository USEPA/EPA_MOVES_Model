/**************************************************************************************************
 * @(#)CellFileReader.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.util.*;
import java.util.TimeZone;
import java.text.SimpleDateFormat;
import org.apache.poi.ss.usermodel.*;

/**
 * Read XLSX, XLS, CSV, or tabbed text files.
 *
 * @author		Wesley Faler
 * @version		2015-06-09
**/
public class CellFileReader {
	/** Time zone for converting date/time cells, set on first use **/
	private static TimeZone gmtZone = null;
	/** Date format for converting date/time cells, set of first use **/
	private static SimpleDateFormat format = null;

	/** File to read from **/
	File file;
	/** Type of the file, from CellFile.CSV, .XLS, .XLSX, .TABBED_TEXT **/
	int fileType = 0;
	/** InputStream for XLS/XLSX files **/
	InputStream inputStream = null;
	/** Workbook for XLS files **/
	Workbook workbook = null;
	/** Workbook for XLS files **/
	Sheet sheet = null;
	/** column index, zero based **/
	int columnIndex = 0;
	/** maximum column index read, zero based.  Used for end of data checking in XLS files. **/
	int maxColumnIndex = 0;
	/** row index, zero based **/
	int rowIndex = 0;
	/** For XLS files, the object holding the cells in the current row **/
	Row row = null;
	/** Current line for CSV and tabbed text files **/
	String currentLine = "";
	/** String parts of the current line for CSV and tabbed text files **/
	ArrayList<String> parts = null;
	/** Reader for CSV and tabbed text files **/
	BufferedReader reader = null;
	/** For XLS files, the 0-based index of the first column **/
	int firstColumn = 0;
	/** For XLS files, the 0-based index of the last column **/
	int lastColumn = 0;
	/** For XLS files, the 0-based index of the first row **/
	int firstRow = 0;
	/** For XLS files, the 0-based index of the last row **/
	int lastRow = 0;
	/** Number of consecutive blank lines in an XLS file **/
	int consecutiveBlankLineCount = 0;
	/**
	 * Array of maps, indexed by the column index.  Each map is keyed by the phrase
	 * that indicates a wildcard, and its data is a TreeSet of Objects that represent
	 * those values.
	**/
	TreeMapIgnoreCase[] wildcardDefinitions = new TreeMapIgnoreCase[100];

	static class WildcardIterator {
		// Values will never be empty
		public TreeSet values;
		public Iterator iterator;
		public int index;
		public String value;

		public WildcardIterator(TreeSet valuesToUse) {
			values = valuesToUse;
			iterator = values.iterator();
			index = 0;
			value = iterator.next().toString();
		}
	}

	/**
	 * Values of objects on a line that has wildcards.  Holds normal objects for
	 * cell values (String, Integer, Double) and WildcardIterator objects.
	**/
	Object[] lineWithWildcards = new Object[100];
	/** True if any wildcard values were found on the line **/
	boolean hasWildcardValuesOnLine = false;
	/** True when wildcard data is being returned instead of real file data **/
	boolean isReadingFromWildcards = false;

	/**
	 * Constructor
	 * @param fileToUse file to read from
	 * @param tabToUse for XLS files, the name of the worksheet within the file
	 * @throws Exception if unable to open the file
	**/
	public CellFileReader(File fileToUse, String tabToUse) throws Exception {
		file = fileToUse;
		fileType = CellFile.getFileType(file);
		if(!file.exists()) {
			throw new IOException("File does not exist: " + file.getName());
		}
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			inputStream = new FileInputStream(file);
			workbook = WorkbookFactory.create(inputStream);

			sheet = null;
			if(tabToUse == null || tabToUse.length() <= 0) {
				sheet = workbook.getSheetAt(0);
			} else {
				int howManySheets = workbook.getNumberOfSheets();
				for(int i=0;i<howManySheets;i++) {
					String name = workbook.getSheetName(i);
					if(name != null) {
						name = name.trim();
						if(name.equalsIgnoreCase(tabToUse)) {
							sheet = workbook.getSheetAt(i);
							break;
						}
					}
				}
			}
			if(sheet == null) {
				throw new IOException("Worksheet does not exist (" 
						+ tabToUse + ") within " + file.getName());
			}
			firstRow = sheet.getFirstRowNum();
			lastRow = sheet.getLastRowNum();
			//System.out.println("firstRow="+firstRow+", lastRow="+lastRow);
		} else {
			reader = new BufferedReader(new FileReader(file));
		}
		// Read the first row
		rowIndex = -1;
		endRow();
	}

	/**
	 * Setup a wildcard definition.
	 * @param columnIndex0 the column to be watched
	 * @param phraseToWatchFor text that indicates a wildcard has occurred
	 * @param values set of values to used when a wildcard event happens
	**/
	public void addWildcardDefinition(int columnIndex0, String phraseToWatchFor, 
			TreeSet<Object> values) {
		// Make sure our holding array is sized correctly
		if(columnIndex0 >= wildcardDefinitions.length) {
			TreeMapIgnoreCase[] t = new TreeMapIgnoreCase[
					Math.max(columnIndex0+5,wildcardDefinitions.length+10)];
			for(int i=0;i<wildcardDefinitions.length;i++) {
				t[i] = wildcardDefinitions[i];
			}
			wildcardDefinitions = t;
		}
		TreeMapIgnoreCase w = wildcardDefinitions[columnIndex0];
		if(w == null) {
			w = new TreeMapIgnoreCase();
			wildcardDefinitions[columnIndex0] = w;
		}
		w.put(phraseToWatchFor,values);
	}

	/**
	 * Check a value in the current column (as given by columnIndex)
	 * against the wildcard triggers.
	 * @param text data to be examined
	 * @return data to be used, which may be text or a value from the wildcard set.
	**/
	private String checkWildcard(String text) {
		if(isReadingFromWildcards || text == null) {
			return text;
		}
		if(columnIndex >= wildcardDefinitions.length) {
			TreeMapIgnoreCase[] t = new TreeMapIgnoreCase[
					Math.max(columnIndex+5,wildcardDefinitions.length+10)];
			for(int i=0;i<wildcardDefinitions.length;i++) {
				t[i] = wildcardDefinitions[i];
			}
			wildcardDefinitions = t;
		}
		if(columnIndex >= lineWithWildcards.length) {
			Object[] t = new Object[wildcardDefinitions.length];
			for(int i=0;i<lineWithWildcards.length;i++) {
				t[i] = lineWithWildcards[i];
			}
			lineWithWildcards = t;
		}
		lineWithWildcards[columnIndex ] = text;
		if(wildcardDefinitions[columnIndex] == null) {
			return text;
		}
		TreeSet values = (TreeSet)wildcardDefinitions[columnIndex].get(text);
		if(values == null) {
			// text did not match an explicit wildcard word, such as "ALL" or "ALL 5*"
			// so check it for generic wildcards such as "*" or "5*".  These are paired
			// with values from the "ALL" wording, if present.
			if(WildcardMatcher.hasWildcards(text)) {
				values = (TreeSet)wildcardDefinitions[columnIndex].get("ALL");
				if(values != null) {
					if(WildcardMatcher.wouldMatchEverything(text)) {
						// Leave values referencing the ALL set.
					} else {
						// Trim values to a smaller matching set
						values = WildcardMatcher.filter(values,text);
						if(values == null) {
							return "";
						}
					}
				}
			}
			if(values == null) {
				return text;
			}
		}
		if(values.size() <= 0) {
			return "";
		}
		// A wildcard has been found.
		// Note it and return the first value in set.
		lineWithWildcards[columnIndex] = (Object)(new WildcardIterator(values));
		hasWildcardValuesOnLine = true;
		return values.first().toString();
	}

	/** Reset all entries in lineWithWildcards to null **/
	private void clearLineValues() {
		for(int i=0;i<lineWithWildcards.length;i++) {
			lineWithWildcards[i] = null;
		}
		hasWildcardValuesOnLine = false;
	}

	/**
	 * Get data from the wildcard information instead of the current file.
	 * Uses columnIndex to find its information.
	 * @return String to be parsed or used, may be null.
	**/
	private String getDataFromWildcardLine() {
		Object t = lineWithWildcards[columnIndex];
		if(t == null) {
			return null;
		}
		if(t instanceof WildcardIterator) {
			return ((WildcardIterator)t).value;
		}
		return t.toString();
	}

	/**
	 * Advance the iterators for wildcards on the current line.
	 * @return true if a new wildcard line was created.
	**/
	private boolean nextWildcardLine() {
		for(int i=0;i<lineWithWildcards.length;i++) {
			Object t = lineWithWildcards[i];
			if(t instanceof WildcardIterator) {
				WildcardIterator wi = (WildcardIterator)t;
				if(wi.index+1 < wi.values.size()) {
					wi.index++;
					wi.value = wi.iterator.next().toString();
					return true;
				} else {
					wi.index = 0;
					wi.iterator = wi.values.iterator();
					wi.value = wi.iterator.next().toString();
				}
			}
		}
		return false;
	}

	/**
	 * Retrieve a cell from active sheet.
	 * @param c 0-based column index
	 * @param r 0-based row index
	 * @return a Cell object or null
	**/
	private Cell getCell(int c, int r) {
		Row tempRow = row;
		if(r != rowIndex) {
			tempRow = sheet.getRow(r);
		}
		if(tempRow == null) {
			return null;
		}
		return tempRow.getCell(c);
	}

	/**
	 * Read the contents of the next cell on the current line.  A blank string
	 * is returned if there are no more cells for CSV or tabbed text files.
	 * @return the textual contents of the next cell
	 * @throws Exception if anything goes wrong
	**/
	public String readStringCell() throws Exception {
		// If no more data on the current CSV or tabbed text line, then return ""
		if(!isReadingFromWildcards) {
			if(((fileType == CellFile.CSV || fileType == CellFile.TABBED_TEXT)
					&& columnIndex >= parts.size())
					|| ((fileType == CellFile.XLS || fileType == CellFile.XLSX)
					&& (columnIndex > lastColumn || rowIndex > lastRow))) {
				return "";
			}
		}
		try {
			String result = "";

			if(isReadingFromWildcards) {
				result = getDataFromWildcardLine();
			} else if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
				Cell cell = getCell(columnIndex,rowIndex);
				result = getContents(cell);
			} else {
				result = (String)parts.get(columnIndex);
			}
			if(result == null) {
				result = "";
			}
			result = checkWildcard(result);
			columnIndex++;
			return result;
		} finally {
			maxColumnIndex = Math.max(columnIndex,maxColumnIndex);
		}
	}

	/**
	 * Read the contents of the next cell on the current line.  null
	 * is returned if there are no more cells for CSV or tabbed text files.
	 * @return the Integer contents of the next cell or null if blank
	 * @throws Exception if anything goes wrong, which may happen if the cell
	 * contains something other than an integer
	**/
	public Integer readIntegerCell() throws Exception {
		// If no more data on the current CSV or tabbed text line, then return null
		if(!isReadingFromWildcards) {
			if(((fileType == CellFile.CSV || fileType == CellFile.TABBED_TEXT)
					&& columnIndex >= parts.size())
					|| ((fileType == CellFile.XLS || fileType == CellFile.XLSX)
					&& (columnIndex > lastColumn || rowIndex > lastRow))) {
				return null;
			}
		}
		try {
			String contents = null;

			if(isReadingFromWildcards) {
				contents = getDataFromWildcardLine();
			} else if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
				Cell cell = getCell(columnIndex,rowIndex);
				contents = getContents(cell);
			} else {
				contents = ((String)parts.get(columnIndex)).trim();
			}

			contents = checkWildcard(contents);
			columnIndex++;
			if(contents == null || contents.length() <= 0) {
				return null;
			}
			return new Integer((int)Double.parseDouble(contents));
		} finally {
			maxColumnIndex = Math.max(columnIndex,maxColumnIndex);
		}
	}

	/**
	 * Read the contents of the next cell on the current line.  null
	 * is returned if there are no more cells for CSV or tabbed text files.
	 * @return the Integer contents of the next cell or null if blank
	 * @throws Exception if anything goes wrong, which may happen if the cell
	 * contains something other than a double
	**/
	public Double readDoubleCell() throws Exception {
		// If no more data on the current CSV or tabbed text line, then return null
		if(!isReadingFromWildcards) {
			if(((fileType == CellFile.CSV || fileType == CellFile.TABBED_TEXT)
					&& columnIndex >= parts.size())
					|| ((fileType == CellFile.XLS || fileType == CellFile.XLSX)
					&& (columnIndex > lastColumn || rowIndex > lastRow))) {
				return null;
			}
		}
		try {
			String contents = null;

			if(isReadingFromWildcards) {
				contents = getDataFromWildcardLine();
			} else if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
				Cell cell = getCell(columnIndex,rowIndex);
				contents = getContents(cell);
			} else {
				contents = ((String)parts.get(columnIndex)).trim();
			}

			contents = checkWildcard(contents);
			columnIndex++;
			if(contents == null || contents.length() <= 0) {
				return null;
			}
			return new Double(Double.parseDouble(contents));
		} finally {
			maxColumnIndex = Math.max(columnIndex,maxColumnIndex);
		}
	}

	/**
	 * Advance the cursor in the current row.
	**/
	public void skipCell() {
		columnIndex++;
		maxColumnIndex = Math.max(columnIndex,maxColumnIndex);
	}

	/**
	 * Backup to the previous cell on the current row.
	**/
	public void backupOneCell() {
		if(columnIndex > 0) {
			columnIndex--;
		}
	}

	/**
	 * Advance to the start of the next row.
	 * @throws Exception if anything goes wrong
	**/
	public void endRow() throws Exception {
		if(hasWildcardValuesOnLine || isReadingFromWildcards) {
			hasWildcardValuesOnLine = false;
			isReadingFromWildcards = nextWildcardLine();
			if(isReadingFromWildcards) {
				columnIndex = 0;
				return;
			}
		}

		rowIndex++;
		clearLineValues();
		columnIndex = 0;
		currentLine = null;
		if(fileType == CellFile.XLS || fileType == CellFile.XLSX) {
			row = null;
			if(rowIndex > 65000) {
				throw new IOException("XLS/XLSX row limit reached, not all data was read.");
			}
			if(rowIndex > lastRow) {
				currentLine = null;
			} else {
				currentLine = "";
				row = sheet.getRow(rowIndex);
				if(row != null) {
					// Build currentLine
					firstColumn = Math.min(firstColumn,row.getFirstCellNum());
					lastColumn = Math.max(lastColumn,row.getLastCellNum());

					int maxColumnToCheck = Math.max(maxColumnIndex,10);
					maxColumnToCheck = Math.min(maxColumnIndex,lastColumn);
					for(int c=0;c<=maxColumnToCheck;c++) {
						Cell cell = row.getCell(c);
						if(cell != null) {
							try {
								String result = getContents(cell);
								if(result != null) {
									currentLine += result;
								}
							} catch(Exception e) {
								Logger.log(LogMessageCategory.INFO,"Error reading cell " + (c+1) + " in row " + rowIndex);
								throw e;
							}
						}
					}
				}
				// If currentLine is blank and too many rows previously had been blank
				// (not just skipped due to comments), then set currentLine to null
				// so it will flagged as the end of data.
				//System.out.println("currentLine=\""+currentLine+"\"");
				if(currentLine.length() > 0) {
					consecutiveBlankLineCount = 0;
				} else {
					consecutiveBlankLineCount++;
					if(consecutiveBlankLineCount >= 10) {
						currentLine = null;
					}
				}
			}
		} else {
			currentLine = reader.readLine();
			parts = StringUtilities.splitCSVremovingQuotes(currentLine==null?"":currentLine,
					fileType==CellFile.CSV?',':'\t');
		}
	}

	/**
	 * Check the current line's data, skipping blank lines or comment lines.
	 * Comment lines start with "//" (Java/C++ style), "--" (MySQL style), or "#".
	 * @return true if the line is blank or a comment line.
	**/
	public boolean shouldSkipLine() {
		if(currentLine == null) {
			return true;
		}
		String t = currentLine.trim();
		return t.length() == 0 || t.startsWith("//") || t.startsWith("--") || t.startsWith("#");
	}

	/**
	 * Test for end of data condition.
	 * @return true if there are no more data lines in the file
	**/
	public boolean isEndOfData() {
		return currentLine == null;
	}

	/** Close the file **/
	public void close() {
		clearLineValues();
		if(reader != null) {
			try {
				reader.close();
			} catch(Exception e) {
				// Nothing to do here
			}
			reader = null;
		}
		sheet = null;
		if(workbook != null) {
			// The workbook does not have a close method
			workbook = null;
		}
		if(inputStream != null) {
			try {
				inputStream.close();
			} catch(Exception e) {
				// Nothing to do here
			}
			inputStream = null;
		}
	}

	/**
	 * Get the contents of a cell or null if the cell is empty or blank
	 * or just spaces.
	 * @param c data to be examined
	 * @return text in the cell if the cell is not empty and not all spaces,
	 * or null.  Never zero length and never all spaces.
	 * @throws IllegalStateException if unable to read a cell value
	**/
	private static String getContents(Cell c) throws IllegalStateException {
		if(c == null) {
			return null;
		}
		String contents = null;
		Date date = null;
		switch(c.getCellType()) {
			case Cell.CELL_TYPE_BLANK:
				return null;
			case Cell.CELL_TYPE_BOOLEAN:
				return c.getBooleanCellValue()? "1":"0";
			case Cell.CELL_TYPE_ERROR:
				return null;
			case Cell.CELL_TYPE_FORMULA:
				try {
					if(DateUtil.isCellDateFormatted(c)) {
						date = c.getDateCellValue();
					}
				} catch(Exception e) {
					date = null;
				}
				if(date == null) {
					try {
						contents = c.getStringCellValue();
					} catch(Exception e) {
						try {
							return "" + c.getNumericCellValue();
						} catch(Exception e2) {
							return null;
						}
					}
				}
				break;
			case Cell.CELL_TYPE_NUMERIC:
				if(DateUtil.isCellDateFormatted(c)) {
					date = c.getDateCellValue();
				} else {
					return "" + c.getNumericCellValue();
				}
			case Cell.CELL_TYPE_STRING:
				contents = c.getStringCellValue();
				break;
			default:
				return null;
		}
		if(date != null) {
			if(gmtZone == null) {
				gmtZone = TimeZone.getTimeZone("GMT");
				format = new SimpleDateFormat("M/d/yyyy");
				format.setTimeZone(gmtZone);
			}
			return format.format(date);
		}

		if(contents != null) {
			if(contents.length() <= 0) {
				contents = null;
			} else {
				contents = contents.trim();
				if(contents.length() <= 0) {
					contents = null;
				}
			}
		}
		return contents;
	}

	/**
	 * Obtain the 1-based line number of the current row.
	 * @return the 1-based line number of the current row
	**/
	public int getLineNumber() {
		return rowIndex+1;
	}
}
