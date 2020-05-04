/**************************************************************************************************
 * @(#)EPATableColumn.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;
import java.lang.*;
import java.io.*;
import java.sql.*;
import javax.swing.table.*;
import java.awt.*;

/**
 * This class provides a generic table structure that can handle generic memory handling of data 
 * as well as display on GUI screens.
 *
 * @author		Cimulus
 * @version		2004-09-30
**/
public class EPATableColumn extends TableColumn {
	/** Indicates whether the column is Read/Write **/
	public boolean isEditable = true ;
	/** Color to display behind text in this column **/
	private Color backgroundColor = new Color( 255 , 255 , 255 ) ;

	/** 
	 * This constructor allows you to define an EPATableColumn
	 * Sets a default model index of 0, default width of 75, a null renderer and a null editor. 
	**/
	public EPATableColumn() {
		super() ;
	}

	/** 
	 * This constructor allows you to define an EPATableColumn
	 * Sets a default width of 75, a null renderer and a null editor. 
	 * @param modelIndex The Column index within a TableModel 
	**/
	public EPATableColumn(int modelIndex) {
		super( modelIndex ) ;
	}

	/** 
	 * This constructor allows you to define an EPATableColumn
	 * Sets a null renderer and a null editor. 
	 * @param modelIndex The Column index within a TableModel 
	 * @param width The width in pixels for this Column 
	**/
	public EPATableColumn(int modelIndex, int width) {
		super( modelIndex, width ) ;
	}

	/** 
	 * This constructor allows you to define an EPATableColumn
	 * @param modelIndex The Column index within a TableModel 
	 * @param width The width in pixels for this Column 
	 * @param cellRenderer The TableCellRenderer to use for the column
	 * @param cellEditor The TableCellEditor to use for the column
	**/
	public EPATableColumn(int modelIndex, int width, TableCellRenderer cellRenderer, 
			TableCellEditor cellEditor) {
		super( modelIndex , width , cellRenderer , cellEditor ) ;
	}

	/** 
	 * This constructor gets the Background Color
	 * @return Returns the background Color
	**/
	public Color getBackgroundColor() {
		return backgroundColor ;
	}

	/** 
	 * This constructor sets the Background Color
	 * @param obColor An object. The method will be ignored if the object is null or not of type Color
	**/
	public void setBackgroundColor( Object obColor ) {
		if ( obColor != null ) {
			if ( obColor instanceof Color ) {
				backgroundColor = ( Color ) obColor ;
			}
		}
	}
}
