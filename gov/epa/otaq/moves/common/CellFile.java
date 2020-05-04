/**************************************************************************************************
 * @(#)CellFile.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;
import java.io.*;

/**
 * Utilities for reading and writing to XLS, XLSX, CSV, or tabbed text file under a unified API.
 * 
 * @author		Wesley Faler
 * @version		2009-04-20
**/
public class CellFile {
	/** Type flag for CSV files **/
	public static final int CSV = 1;
	/** Type flag for XLS files **/
	public static final int XLS = 2;
	/** Type flag for XLSX files **/
	public static final int XLSX = 3;
	/** Type flag for Tabbed Text files **/
	public static final int TABBED_TEXT = 4;

	/**
	 * Obtain the type of a file based on its extension.
	 * @param file File to be examined
	 * @return type of the file (CSV, XLS, XLSX, TABBED_TEXT)
	**/
	public static int getFileType(File file) {
		return getFileType(file.getName());
	}

	/**
	 * Obtain the type of a file based on its extension.
	 * @param fileName name of the file, including extension
	 * @return type of the file (CSV, XLS, XLSX, TABBED_TEXT)
	**/
	public static int getFileType(String fileName) {
		String fileExtension = FileUtilities.getFileExtension(fileName,false);
		if(fileExtension.equalsIgnoreCase("XLSX")) {
			return XLSX;
		} else if(fileExtension.equalsIgnoreCase("XLS")) {
			return XLS;
		} else if(fileExtension.equalsIgnoreCase("CSV")) {
			return CSV;
		} else {
			return TABBED_TEXT;
		}
	}

	/**
	 * True if a file type (XLSX, XLS, CSV, TABBED_TEXT) contains worksheets as XLSX and XLS
	 * file do.
	 * @param fileType one of the file types (XLSX, XLS, CSV, TABBED_TEXT)
	 * @return true if the file type contains worksheets
	**/
	public static boolean formatAllowsWorksheets(int fileType) {
		return fileType == XLSX || fileType == XLS;
	}
}
