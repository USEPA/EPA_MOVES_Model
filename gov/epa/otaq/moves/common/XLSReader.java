/**************************************************************************************************
 * @(#)XLSReader.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import java.util.*;
import java.text.SimpleDateFormat;
import org.apache.poi.ss.usermodel.*;

/**
 * Utility for reading an XLS or XLSX file into memory.
 *
 * @author		Wesley Faler
 * @version		2009-04-20
**/
public class XLSReader {
	private static TimeZone gmtZone = null; // set on first use
	private static SimpleDateFormat format = null; // set on first use

	/**
	 * Retrieve a list of all the worksheet tabs within a workbook
	 * @param file XLS/Excel file to be read.
	 * @return ArrayList of String objects each naming a sheet and sequenced according
	 * sheet location within the workbook
	**/
	public ArrayList<String> getSheets(File file) throws Exception {
		ArrayList<String> result = new ArrayList<String>();
		InputStream input = null;
		Workbook workbook = null;
		try {
			input = new FileInputStream(file);
			workbook = WorkbookFactory.create(input);
			int howManySheets = workbook.getNumberOfSheets();
			for(int i=0;i<howManySheets;i++) {
				String name = workbook.getSheetName(i);
				if(name != null) {
					name = name.trim();
					if(name.length() > 0) {
						result.add(name);
					}
				}
			}
		} finally {
			if(workbook != null) {
				// Nothing to do here to close the work book
				workbook = null;
			}
			if(input != null) {
				try {
					input.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				input = null;
			}
		}
		return result;
	}

	/**
	 * Read an XLS file into memory as a tab separated string with newlines.
	 * Automatically detects starting row and column.  Also, detects end row 
	 * (5 blank rows) and columns (10 blank columns).  Skips blank lines without 
	 * reporting them.  Only the first worksheet is read.
	 * @param file XLS/Excel file to be read.
	 * @param sheetIndex 0-based index of the sheet to be read
	 * @return tab separated values with newlines at each end of line.
	 * The lines may be jagged, that is, they may differ in their number
	 * of columns.
	 * 
	**/
	public String read(File file, int sheetIndex) throws Exception {
		StringBuffer buffer = new StringBuffer(100000);
		InputStream input = null;
		Workbook workbook = null;
		
		try {
			input = new FileInputStream(file);
			workbook = WorkbookFactory.create(input);
			int howManySheets = workbook.getNumberOfSheets();
			if(sheetIndex < 0) {
				sheetIndex = 0;
			}
			Sheet sheet = workbook.getSheetAt(sheetIndex);
			// Find top-left cell
			int top = 0, left = 0;
			Cell cell;
			String contents;
			boolean found = false;
			int firstRowNumber = sheet.getFirstRowNum();
			int lastRowNumber = sheet.getLastRowNum();
			short leftColumnNumber = 0;
			Row row = null;

			for(int r=firstRowNumber;r<firstRowNumber+10 && !found && r<=lastRowNumber;r++) {
				row = sheet.getRow(r);
				if(row == null) {
					continue;
				}
				for(short c=leftColumnNumber;c<leftColumnNumber+20 && c<=row.getLastCellNum();c++) {
					cell = row.getCell(c);
					if(cell != null) {
						contents = getContents(cell);
						if(contents != null) {
							//System.out.println("cell("+c+","+r+")=\"" + contents + "\"");
							top = r;
							left = c;
							found = true;
							//System.out.println("Found topleft cell, column=" + left
							//		+ ", row=" + top);
							break;
						} else {
							//System.out.println("cell("+c+","+r+") is blank");
						}
					}
				}
			}
			if(found) {
				int lastRowWithData = top;
				int lastColumnWithData = left;
				int r = top, c = left;
				boolean rowHasData = false;

				row = sheet.getRow(r);
				while(true) {
					if((row != null && c > row.getLastCellNum()) || c >= lastColumnWithData+20) {
						if(rowHasData) {
							buffer.append('\n');
							rowHasData = false;
						}
						lastColumnWithData = left;
						c = left;
						r++;
						row = null;
					}
					if(row == null) {
						row = sheet.getRow(r);
						if(row == null) {
							c = left;
							r++;
						}
					}
					if(r > lastRowNumber || r >= lastRowWithData+10) {
						break;
					}
					if(row == null) {
						continue;
					}
					if(c > row.getLastCellNum()) {
						c = left;
						r++;
						row = null;
						continue;
					}
					cell = row.getCell(c);
					contents = getContents(cell);
					if(contents != null) {
						for(;lastColumnWithData<c;lastColumnWithData++) {
							buffer.append('\t');
						}
						lastRowWithData = r;
						buffer.append(contents);
						rowHasData = true;
					}
					c++;
				}
			}
		} finally {
			if(workbook != null) {
				// Nothing to do here to close the work book
				workbook = null;
			}
			if(input != null) {
				try {
					input.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				input = null;
			}
		}

		// Done
		String result = buffer.toString();
		//System.out.println("XLSReader.read=");
		//System.out.println(result);
		//System.out.println("End of XLSReader.read");
		return result;
	}

	/**
	 * Get the contents of a cell or null if the cell is empty or blank
	 * or just spaces.
	 * @param c data to be examined
	 * @return text in the cell if the cell is not empty and not all spaces,
	 * or null.  Never zero length and never all spaces.
	**/
	private static String getContents(Cell c) {
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
				if(DateUtil.isCellDateFormatted(c)) {
					date = c.getDateCellValue();
				} else {
					contents = c.getStringCellValue();
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
}
