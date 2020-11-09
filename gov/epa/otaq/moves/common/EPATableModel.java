/**************************************************************************************************
 * @(#)EPATableModel.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;
import java.lang.*;
import java.io.*;
import java.math.*;
import java.sql.*;
import javax.swing.table.*;

/**
 * This class provides a generic table structure that can handle generic memory handling of data 
 * as well as display on GUI screens.
 *
 * @author		Wes Faler
 * @version		2009-02-22
**/
public class EPATableModel extends DefaultTableModel {

	public String errorText = "" ;
	private int numRowsActive = 0 ;
	public static Connection dbConnection = null ; 
	public String sqlThatFilledMemory = "" ;
	public String debugText = "" ;
	
	public static boolean debug = false ;
	public static long debugTime = 0 ;
	
	/**
	 * This constructor allows you to define an EPATableModel with no columns
	**/
	public EPATableModel() {
		clearData() ;
	}
	
	/**
	 * This constructor allows you to define an EPATableModel and list of columns
	 * @param columns A list of columns, separated by the | character
	**/
	public EPATableModel( String columns ) {
		addColumns( columns ) ;
		clearData() ;
	}

	/**
	 * This method returns a new EPATableModel with the exact
	 * same Column structure. No data is copied over.
	 * @return A new EPATableModel with the exact same Column structure. 
	**/
	public EPATableModel copyStructure() {
		int x ;
		String colName ;
		EPATableModel newModel = new EPATableModel() ;
		
		for ( x = 0 ; x < getColumnCount() ; x++ ) {
			colName = getColumnName( x ) ;
			if ( colName == null ) {
				continue ;
			}
			newModel.addColumn( colName ) ;
	 	}
	 	
	 	return newModel ;
	}

	/**
	 * This static method sets the Connection if it is null
	 * @param con A Database Connection
	**/
	public static void setDbConnectionIfNull( Connection con ) {
		if ( con != null && dbConnection == null ) {
			dbConnection = con ;
		}
	}


	/**
	 * @return This method returns the value of numRowsActive
	**/
	public int getNumRows() {
		return numRowsActive ;
	}


	/**
	 * @return This method returns the value of numRowsActive
	**/
	public int getNumRowsActive() {
		return numRowsActive ;
	}


	/**
	 * This method sets the value of numRowsActive
	 * @param x The new value of numRowsActive
	**/
	public void setNumRowsActive( int x ) {
		int y ;
		x = x < 0 ? 0 : x ;
		numRowsActive = x ;
		if ( getRowCount() < numRowsActive ) {
			y = getRowCount() < 100 ? 100 : (int) ( getRowCount() * 1.5 ) ;
			y = y < numRowsActive ? numRowsActive : y ;
			setRowCount( y ) ;
		}
	}

	
	/**
	 * This method adds columns to the EPATableModel
	 * @param columns A list of columns, separated by the | character
	**/
	public void addColumns( String columns ) {
		String[] cols;
		String colName;
		int x , num ;

		if ( columns == null ) {
			return ;
		}

		StringTokenizer tok = new StringTokenizer( columns , "|" ) ;
		num = tok.countTokens() ;

		for ( x = 0 ; x < num ; x++ ) {
			colName = tok.nextToken() ;
			colName = colName.trim() ;
			if ( colName.length() == 0 ) {
				continue ;
			}

			addColumn( colName ) ;
		}
	}

	/**
	 * This method clears any data in the table.
	**/
	public void clearData() {
		int rowCount ;

		sqlThatFilledMemory = "" ;
		setNumRowsActive( 0 ) ;

		rowCount = getRowCount() ;
		rowCount = rowCount == 0 ? 100 : rowCount ;
		
		// These commands will clear the memory
		setRowCount( 0 ) ;
		setRowCount( rowCount ) ;
	}

	/**
	 * This method retrieves the column index of the column name specified in the parameter.
	 * @param colName The name of the Column being searched for. The search is case insensitive.
	 * @return The index value of the Column within the Column List. 
	 * The method returns -1 if the Column Name is not found.
	**/
	public int getColumnIndex( String colName ) {
		int x ;
		String col ;
		
		for ( x = 0 ; x < getColumnCount() ; x++ ) {
			col = getColumnName( x ) ;
			if ( col == null ) {
				continue ;
			}
			if ( col.equalsIgnoreCase( colName ) == true ) {
				return x ;
			}
		}

		return -1 ;
	}

	/**
	 * This method retrieves a string that describes the columns in the table
	 * @return A string with a text representation of the columns in the table.
	**/
	public String getDefinition() {
		StringBuffer sb = new StringBuffer( 5000 ) ;
		int x ;
		String newLine = System.getProperty( "line.separator" ) ;
		String colName ;

		sb.append( "Table Definition" ) ;
		sb.append( newLine ) ;

		for ( x = 0 ; x < getColumnCount() ; x++ ) {
			colName = getColumnName( x ) ;
			sb.append( x ) ;
			sb.append( ": " ) ;
			sb.append( colName ) ;
			sb.append( newLine ) ;
		}

		sb.append( newLine ) ;

		return sb.toString() ;
	}

	/**
	 * This method writes a file that describes the data in the table
	 * @param fileName The File Name that the file will be written to
	 * @return true if the file was written successfully, false if not.
	**/
	public boolean printDataRows( String fileName ) {
		return printDataRows( fileName , 0 , getNumRowsActive() ) ;
	}

	/**
	 * This method writes a file that describes the data in the table
	 * @param fileName The File Name that the file will be written to
	 * @param startRow The first row to print
	 * @return true if the file was written successfully, false if not.
	**/
	public boolean printDataRows( String fileName , int startRow ) {
		return printDataRows( fileName , startRow , getNumRows() ) ;
	}

	/**
	 * This method writes a file that describes the data in the table
	 * @param fileName The File Name that the file will be written to
	 * @param startRow The first row to print
	 * @param endRow The last row to print
	 * @return true if the file was written successfully, false if not.
	**/
	public boolean printDataRows( String fileName , int startRow , int endRow ) {
		String s = getPrintDataRows( startRow , endRow ) ;
		return FileUtilities.writeFileContents( fileName , s ) ;
	}

	/**
	 * This method retrieves a string that describes the columns in the table
	 * @return A string with a text representation of the data in the table.
	**/
	public String getPrintDataRows() {
		return getPrintDataRows( 0 , getNumRowsActive() ) ;
	}

	/**
	 * This method retrieves a string that describes the columns in the table
	 * @param startRow The first row to print
	 * @return A string with a text representation of the data in the table.
	**/
	public String getPrintDataRows( int startRow ) {
		return getPrintDataRows( startRow , getNumRowsActive() ) ;
	}

	/**
	 * This method retrieves a string that describes the data in the table
	 * @param startRow The first row to print
	 * @param endRow The last row to print
	 * @return A string with a text representation of the data in the table.
	**/
	public String getPrintDataRows( int startRow , int endRow ) {
		StringBuffer sb = new StringBuffer( 20000 ) ;
		int x , r , col , colLength = 0 , countLength ;
		String newLine = System.getProperty( "line.separator" ) ;
		String colName , stringCount , s ;
		Object ob ;
		
		startRow = startRow < 0 ? 0 : startRow ;
		endRow = endRow > getNumRowsActive() ? getNumRowsActive() : endRow ;
		endRow = endRow < startRow ? startRow : endRow ;

		sb.append( "Table Definition  NumRows: " ) ;
		sb.append( getNumRowsActive() ) ;
		sb.append( newLine ) ;

		for ( x = 0 ; x < getColumnCount() ; x++ ) {
			colName = getColumnName( x ) ;
			if ( colName.length() > colLength ) {
				colLength = colName.length() ;
			}
		}

		s = "" + getColumnCount() ;
		countLength = s.length() ;

		for ( r = startRow ; r < endRow ; r++ ) {
			sb.append( "Row " ) ;
			sb.append( ( r + 1 ) ) ;
			sb.append( ": " ) ;
			sb.append( newLine ) ;
			for ( x = 0 ; x < getColumnCount() ; x++ ) {
				colName = getColumnName( x ) ;
				s = "" + x ;
				sb.append( x ) ;
				for ( col = s.length() ; col <= countLength ; col++ ) {
					sb.append( ' ' ) ;
				}

				sb.append( colName ) ;

				for ( col = colName.length() ; col < colLength ; col++ ) {
					sb.append( ' ' ) ;
				}

				sb.append( " : " ) ;
				ob = getValueAt( r , x ) ;
				if ( ob == null ) {
					sb.append( "(null)" ) ;
				} else {
					sb.append( getString( r , x ) ) ;
				}
				sb.append( newLine ) ;
			}
			sb.append( " " + newLine ) ;
		}

		sb.append( newLine ) ;

		return sb.toString() ;
	}



	/**
	 * This method writes a file with the data in a tabular format
	 * @param fileName The File Name that the file will be written to
	 * @return true if the file was written successfully, false if not.
	**/
	public boolean printDataTable( String fileName ) {
		return printDataTable( fileName , 0 , getNumRowsActive() ) ;
	}

	/**
	 * This method writes a file with the data in a tabular format
	 * @param fileName The File Name that the file will be written to
	 * @param startRow The first row to print
	 * @return true if the file was written successfully, false if not.
	**/
	public boolean printDataTable( String fileName , int startRow ) {
		return printDataTable( fileName , startRow , getNumRows() ) ;
	}

	/**
	 * This method writes a file with the data in a tabular format
	 * @param fileName The File Name that the file will be written to
	 * @param startRow The first row to print
	 * @param endRow The last row to print
	 * @return true if the file was written successfully, false if not.
	**/
	public boolean printDataTable( String fileName , int startRow , int endRow ) {
		String s = getPrintDataTable( startRow , endRow ) ;
		return FileUtilities.writeFileContents( fileName , s ) ;
	}

	/**
	 * This method retrieves a string of the data in a tabular format
	 * @return A string with a text representation of the data in the table.
	**/
	public String getPrintDataTable() {
		return getPrintDataTable( 0 , getNumRowsActive() ) ;
	}

	/**
	 * This method retrieves a string of the data in a tabular format
	 * @param startRow The first row to print
	 * @return A string with a text representation of the data in the table.
	**/
	public String getPrintDataTable( int startRow ) {
		return getPrintDataTable( startRow , getNumRowsActive() ) ;
	}

	/**
	 * This method retrieves a string of the data in a tabular format
	 * @param startRow The first row to print
	 * @param endRow The last row to print
	 * @return A string with a text representation of the data in the table.
	**/
	public String getPrintDataTable( int startRow , int endRow ) {
		StringBuffer sb = new StringBuffer( 20000 ) ;
		int x , r , col , colLength = 0 , countLength ;
		String newLine = System.getProperty( "line.separator" ) ;
		String colName , stringCount , s ;
		Object ob ;
		
		startRow = startRow < 0 ? 0 : startRow ;
		endRow = endRow > getNumRowsActive() ? getNumRowsActive() : endRow ;
		endRow = endRow < startRow ? startRow : endRow ;

		for ( x = 0 ; x < getColumnCount() ; x++ ) {
			if ( x > 0 ) {
				sb.append( "\t" ) ;
			}
			sb.append( getColumnName( x ) ) ;
		}
		sb.append( newLine ) ;

		for ( r = startRow ; r < endRow ; r++ ) {
			for ( x = 0 ; x < getColumnCount() ; x++ ) {
				if ( x > 0 ) {
					sb.append( "\t" ) ;
				}
				ob = getValueAt( r , x ) ;
				if ( ob == null ) {
					sb.append( "(null)" ) ;
				} else {
					sb.append( getString( r , x ) ) ;
				}
			}
			sb.append( newLine ) ;
		}

		sb.append( newLine ) ;

		return sb.toString() ;
	}


	/**
	 * This method prints a debug string with timings
	 * @param txt The text of the message to display
	 * @return Returns a long integer representing the System.currentTimeMillis() of this call
	 * The user can use this to calculate timings of operations between calls to the method. 
	**/
	public static long dbg( String txt ) {
		return dbg( txt , debugTime ) ;
	}

	/**
	 * This method prints a debug string with timings
	 * @param txt The text of the message to display
	 * @param lastTime A long integer representing milliseconds from a previous call.
	 * This value is used to calculate the difference in timing for the output.
	 * @return Returns a long integer representing the System.currentTimeMillis() of this call
	 * The user can use this to calculate timings of operations between calls to the method. 
	**/
	public static long dbg( String txt , long lastTime ) {
		long li = 0 ;
		double d = 0.0 ;

		li = System.currentTimeMillis() ;

		if ( debug == false ) {
			return li ;
		}

		if ( lastTime == 0 ) { 
			/** @nonissue **/
			Logger.log(LogMessageCategory.DEBUG,txt);
			debugTime = li ;
			return debugTime ; 
		}

		debugTime = li ;

		d = ( ( double ) li - lastTime ) / 1000.0 ;
		/** @nonissue **/
		Logger.log(LogMessageCategory.DEBUG, "(" + d + " seconds) " + txt ) ;

		return debugTime ; 
	}

	/**
	 * This method fills the table with the data retrieved from the SQL call.
	 * This method will use the static class member variable 'dbConnection'.
	 * @param sql A String that contains the SQL Select Call
	 * @return A return code indicating the success of the transaction. <BR>
	 * Return  0  = Successful<BR>
	 * Return -1  = SQL Error<BR>
	 * Return -2  = Parameter Error<BR>
	 * Return 100 = No rows found.<BR>
	**/
	public int sqlSelect( String sql ) {
		return sqlSelect( dbConnection , sql , false ) ;
	}

	/**
	 * This method fills the table with the data retrieved from the SQL call.
	 * This method will NOT add columns to the TableModel
	 * @param con An SQL Connection object reference
	 * @param sql A String that contains the SQL Select Call
	 * @return A return code indicating the success of the transaction. <BR>
	 * Return  0  = Successful<BR>
	 * Return -1  = SQL Error<BR>
	 * Return -2  = Parameter Error<BR>
	 * Return 100 = No rows found.<BR>
	**/
	public int sqlSelect( Connection con , String sql ) {
		return sqlSelect( con , sql , false ) ;
	}

	/**
	 * This method fills the table with the data retrieved from the SQL call.
	 * This method will use the static class member variable 'dbConnection'.
	 * @param sql A String that contains the SQL Select Call
	 * @param addColumns A boolean - if true, add Column Names from the ResultSet into the Table.
	 * @return A return code indicating the success of the transaction. <BR>
	 * Return  0  = Successful<BR>
	 * Return -1  = SQL Error<BR>
	 * Return -2  = Parameter Error<BR>
	 * Return 100 = No rows found.<BR>
	**/
	public int sqlSelect( String sql , boolean addColumns ) {
		return sqlSelect( dbConnection , sql , addColumns ) ;
	}

	/**
	 * This method fills the table with the data retrieved from the SQL call.
	 * @param con An SQL Connection object reference
	 * @param sql A String that contains the SQL Select Call
	 * @param addColumns A boolean - if true, add Column Names from the ResultSet into the Table.
	 * @return A return code indicating the success of the transaction. <BR>
	 * Return  0  = Successful<BR>
	 * Return -1  = SQL Error<BR>
	 * Return -2  = Parameter Error<BR>
	 * Return 100 = No rows found.<BR>
	**/
	public int sqlSelect( Connection con , String sql , boolean addColumns ) {
		int numRows = 0 ;
		int row = 0 ;
		ResultSetMetaData rsmd ;
		int col , numCols ;
		String colName ;
		int tableColumn ;
		Float f ;

		errorText = "" ;
		debugText = "" ;

		clearData() ;

		if ( con == null ) {
			errorText = "Connection parameter is null" ;
			return -2 ;
		}
		if ( sql == null ) {
			errorText = "Sql parameter is null" ;
			return -2 ;
		}
		if ( sql.length() == 0 ) {
			errorText = "SQL Statement is blank" ;
			return -2 ;
		}

		try {
			sqlThatFilledMemory = sql ;

			PreparedStatement statement = con.prepareStatement(sql);
			if ( statement == null ) {
				errorText = "SQL Error: " + sql ;
				return -1 ;
			}

			ResultSet results = SQLRunner.executeQuery(statement,sql);
			if ( results == null ) {
				errorText = "SQL Error: " + sql ;
				return -1 ;
			}

			rsmd = results.getMetaData() ;
			numCols = getColumnCount() ;
			
			if ( numCols == 0 ) {
				addColumns = true ;
			}

			int[] colRef = new int[ 500 ] ;

			for ( col = 1 ; col <= rsmd.getColumnCount() ; col++ ) {
				colName = rsmd.getColumnName( col ) ;

				colRef[ col ] = getColumnIndex( colName ) ;
				if ( addColumns == true && colRef[ col ] == -1 ) {
					addColumn( colName ) ;
					colRef[ col ] = getColumnIndex( colName ) ;
				}
			}

			while( results.next() ) {
				numRows++ ;
				setNumRowsActive( numRows ) ;
				
				for ( col = 1 ; col <= rsmd.getColumnCount() ; col++ ) {
					tableColumn = colRef[ col ] ;
					if ( tableColumn == -1 ) {
						continue ;
					}

					setValueAt( results.getObject( col ) , row , tableColumn ) ;
/*					
					switch( rsmd.getColumnType( col ) ) {
						case Types.BIT: 
							break ;
//						case Types.BOOLEAN: // Not available until Java 1.4 - add it once upgraded
//							break ;
						case Types.CHAR: 
						case Types.VARCHAR: 
						case Types.LONGVARCHAR: 
							setValueAt( results.getString( col ) , row , tableColumn ) ;
							break ;
						case Types.DATE: 
							break ;
						case Types.NUMERIC: 
							setValueAt( results.getObject( col ) , row , tableColumn ) ;
							break ;
						case Types.REAL: 
							setValueAt( results.getObject( col ) , row , tableColumn ) ;
							break ;
						case Types.DECIMAL: 
							setValueAt( results.getObject( col ) , row , tableColumn ) ;
							break ;
						case Types.DOUBLE: 
							setValueAt( Double.valueOf( results.getDouble( col ) ) , row , tableColumn ) ;
							break ;
						case Types.FLOAT: 
							setValueAt( Float.valueOf( results.getFloat( col ) ) , row , tableColumn ) ;
							break ;
						case Types.BIGINT: 
							setValueAt( BigInteger.valueOf( results.getLong( col ) ) , row , tableColumn ) ;
							break ;
						case Types.INTEGER:
							setValueAt( Integer.valueOf( results.getInt( col ) ) , row , tableColumn ) ;
							break ;
						case Types.SMALLINT: 
							setValueAt( Short.valueOf( results.getShort( col ) ) , row , tableColumn ) ;
							break ;
						case Types.TIME: 
							break ;
						case Types.TIMESTAMP: 
							break ;
						case Types.TINYINT: 
							break ;
						default: 
							setValueAt( results.getObject( col ) , row , tableColumn ) ;
							break ;
						}
		*/
				}
				row++ ;
			}
			results.close();
			statement.close();

			setNumRowsActive( numRows ) ;

			return numRows > 0 ? 0 : 100 ;
		} catch( Exception ex ) {
			errorText = "SQL Error: " + sql ;
			Logger.logSqlError(ex,"Table model unable to select",sql);
			return -1 ;
		}
	}

	/**
	 * This method returns the object stored at the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnIndex The numeric column index from which the value is returned.
	 * @return The object reference of the stored value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, a null will be returned. This method
	 * will not throw an exception.
	**/
	public Object getObject( int row , int columnIndex ) {
		if ( row < 0 || row >= getNumRowsActive() ) {
			return null ;
		}
		if ( columnIndex < 0 || columnIndex >= getColumnCount() ) {
			return null ;
		}
			
		return getValueAt( row , columnIndex ) ;
	}

	/**
	 * This method returns the object stored at the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnName The column name from which the value is returned. The columnName is
	 * case insensitive.
	 * @return The object reference of the stored value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, a null will be returned. This method
	 * will not throw an exception.
	**/
	public Object getObject( int row , String columnName ) {
		int index = getColumnIndex( columnName ) ;
		return getObject( row , index ) ;
	}

	/**
	 * This method returns a String for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnName The column name from which the value is returned. The columnName is
	 * case insensitive.
	 * @return A string representation of the value.
	**/
	public String getString( int row , String columnName ) {
		int index = getColumnIndex( columnName ) ;
		return getString( row , index ) ;
	}

	/**
	 * This method returns a String for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnIndex The numeric column index from which the value is returned.
	 * @return A string representation of the value.
	**/
	public String getString( int row , int columnIndex ) {
		Object ob ;
		try {
			ob = getObject( row , columnIndex ) ;
			if ( ob == null ) {
				return "" ;
			}

			if ( ob instanceof String ) {
				return ( String ) ob ;
			}
			if ( ob instanceof Float ) {
				Float f = ( Float ) ob ;
				return FloatConversion.toString( f.floatValue() , 6 ) ;
			}

			return ob.toString() ;
		} catch( Exception ex ) {
			return "" ;
		}
	}

	/**
	 * This method returns an int representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnName The column name from which the value is returned. The columnName is
	 * case insensitive.
	 * @return The integer representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public int getInt( int row , String columnName ) {
		int index = getColumnIndex( columnName ) ;
		return getInt( row , index ) ;
	}

	/**
	 * This method returns an int representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnIndex The numeric column index from which the value is returned.
	 * @return The integer representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public int getInt( int row , int columnIndex ) {
		Object ob ; Integer i ;

		try {
			ob = getObject( row , columnIndex ) ;
			if ( ob == null ) {
				return 0 ;
			}

			if ( ob instanceof Integer ) {
				i = ( Integer ) ob ;
				return i.intValue() ;
			}

			String s = ob.toString() ;
			i = Integer.valueOf( s ) ;
			return i.intValue() ;
		} catch( Exception ex ) {
			return 0 ;
		}
	}

	/**
	 * This method returns an long representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnName The column name from which the value is returned. The columnName is
	 * case insensitive.
	 * @return The long integer representation of the value. If there are bad parameters passed,
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public long getLong( int row , String columnName ) {
		int index = getColumnIndex( columnName ) ;
		return getLong( row , index ) ;
	}


	/**
	 * This method returns an long int representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnIndex The numeric column index from which the value is returned.
	 * @return The long integer representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public long getLong( int row , int columnIndex ) {
		Object ob ; BigInteger bi ;

		try {
			ob = getObject( row , columnIndex ) ;
			if ( ob == null ) {
				return 0 ;
			}

			if ( ob instanceof BigInteger ) {
				bi = ( BigInteger ) ob ;
				return bi.longValue() ;
			}

			String s = ob.toString() ;
			bi = new BigInteger( s ) ;
			return bi.longValue() ;
		} catch( Exception ex ) {
			return ( long ) 0 ;
		}
	}

	/**
	 * This method returns a float representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnName The column name from which the value is returned. The columnName is
	 * case insensitive.
	 * @return The float representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public float getFloat( int row , String columnName ) {
		int index = getColumnIndex( columnName ) ;
		return getFloat( row , index ) ;
	}

	/**
	 * This method returns a float representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnIndex The numeric column index from which the value is returned.
	 * @return The float representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public float getFloat( int row , int columnIndex ) {
		Object ob ; Float f ;

		try {
			ob = getObject( row , columnIndex ) ;
			if ( ob == null ) {
				return ( float ) 0.0 ;
			}

			if ( ob instanceof Float ) {
				f = ( Float ) ob ;
				return f.floatValue() ;
			}

			String s = ob.toString() ;
			f = Float.valueOf( s ) ;
			return f.floatValue() ;
		} catch( Exception ex ) {
			return ( float ) 0.0 ;
		}
	}

	/**
	 * This method returns a double representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnIndex The numeric column index from which the value is returned.
	 * @return The float representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public double getDouble( int row , int columnIndex ) {
		Object ob ; Double d ;

		try {
			ob = getObject( row , columnIndex ) ;
			if ( ob == null ) {
				return 0.0 ;
			}

			if ( ob instanceof Double ) {
				d = ( Double ) ob ;
				return d.doubleValue() ;
			}

			String s = ob.toString() ;
			d = Double.valueOf( s ) ;
			return d.doubleValue() ;
		} catch( Exception ex ) {
			return 0.0 ;
		}
	}

	/**
	 * This method returns a double representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnIndex The numeric column index from which the value is returned.
	 * @return The float representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public BigDecimal getDecimal( int row , int columnIndex ) {
		Object ob ; BigDecimal bd ;

		try {
			ob = getObject( row , columnIndex ) ;
			if ( ob == null ) {
				return new BigDecimal( 0.0 ) ;
			}

			if ( ob instanceof BigDecimal ) {
				bd = ( BigDecimal ) ob ;
				return bd ;
			}

			String s = ob.toString() ;
			bd = new BigDecimal( s ) ;
			return bd ;
		} catch( Exception ex ) {
			return new BigDecimal( 0.0 ) ;
		}
	}

	/**
	 * This method returns a short representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnName The column name from which the value is returned. The columnName is
	 * case insensitive.
	 * @return The short representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public short getShort( int row , String columnName ) {
		int index = getColumnIndex( columnName ) ;
		return getShort( row , index ) ;
	}

	/**
	 * This method returns a short representation for the row and column specified
	 * @param row The row from which the value is returned.
	 * @param columnIndex The numeric column index from which the value is returned.
	 * @return The short representation of the value. If there are bad parameters passed, 
	 * or the object stored in the cell is null, the value 0 will be returned. This method
	 * will not throw an exception.
	**/
	public short getShort( int row , int columnIndex ) {
		Object ob ; Short si ;

		try {
			ob = getObject( row , columnIndex ) ;
			if ( ob == null ) {
				return 0 ;
			}

			if ( ob instanceof Short ) {
				si = ( Short ) ob ;
				return si.shortValue() ;
			}

			String s = ob.toString() ;
			si = Short.valueOf( s ) ;
			return si.shortValue() ;
		} catch( Exception ex ) {
			return 0 ;
		}
	}

	/**
	 * This method returns a boolean if the parameters for row and column 
	 * represent a valid row/column value for the table.
	 * @param row The row for the cell
	 * @param columnIndex The numeric column index for the cell
	 * @return A boolean of true if the row/column is valid, false if it is not.
	 * This method will not throw an exception.
	**/
	public boolean isValidRowColumn( int row , int columnIndex ) {
		return row < 0 || columnIndex < 0 || columnIndex >= getColumnCount() ? false : true ;
	}

	/**
	 * This method sets a cell value for an integer value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnName The name of the column (case insensitive)
	**/
	public void setValue( int value , int row , String columnName ) {
		setValue( value , row , getColumnIndex( columnName ) ) ;
	}

	/**
	 * This method sets a cell value for an integer value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnIndex The numeric column index for the cell
	**/
	public void setValue( int value , int row , int columnIndex ) {
		try {
			if ( isValidRowColumn( row , columnIndex ) == false ) {
				return ;
			}

			Integer i = Integer.valueOf( value ) ;
			setValueAt( i , row , columnIndex ) ;
		} catch( Exception ex ) {
		}
	}

	/**
	 * This method sets a cell value for an short value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnName The name of the column (case insensitive)
	**/
	public void setValue( short value , int row , String columnName ) {
		setValue( value , row , getColumnIndex( columnName ) ) ;
	}

	/**
	 * This method sets a cell value for an short value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnIndex The numeric column index for the cell
	**/
	public void setValue( short value , int row , int columnIndex ) {
		try {
			if ( isValidRowColumn( row , columnIndex ) == false ) {
				return ;
			}

			Short si = Short.valueOf( value ) ;
			setValueAt( si , row , columnIndex ) ;
		} catch( Exception ex ) {
		}
	}

	/**
	 * This method sets a cell value for an float value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnName The name of the column (case insensitive)
	**/
	public void setValue( float value , int row , String columnName ) {
		setValue( value , row , getColumnIndex( columnName ) ) ;
	}

	/**
	 * This method sets a cell value for an float value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnIndex The numeric column index for the cell
	**/
	public void setValue( float value , int row , int columnIndex ) {
		try {
			if ( isValidRowColumn( row , columnIndex ) == false ) {
				return ;
			}

			Float f = Float.valueOf( value ) ;
			setValueAt( f , row , columnIndex ) ;
		} catch( Exception ex ) {
		}
	}

	/**
	 * This method sets a cell value for a string value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnName The name of the column (case insensitive)
	**/
	public void setValue( String value , int row , String columnName ) {
		setValue( value , row , getColumnIndex( columnName ) ) ;
	}

	/**
	 * This method sets a cell value for an string value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnIndex The numeric column index for the cell
	**/
	public void setValue( String value , int row , int columnIndex ) {
		try {
			if ( isValidRowColumn( row , columnIndex ) == false ) {
				return ;
			}

			setValueAt( value , row , columnIndex ) ;
		} catch( Exception ex ) {
		}
	}

	/**
	 * This method sets a cell value for an object value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnName The name of the column (case insensitive)
	**/
	public void setValue( Object value , int row , String columnName ) {
		setValue( value , row , getColumnIndex( columnName ) ) ;
	}

	/**
	 * This method sets a cell value for an object value.
	 * @param value The value to be stored
	 * @param row The row for the cell
	 * @param columnIndex The numeric column index for the cell
	**/
	public void setValue( Object value , int row , int columnIndex ) {
		try {
			if ( isValidRowColumn( row , columnIndex ) == false ) {
				return ;
			}

			setValueAt( value , row , columnIndex ) ;
		} catch( Exception ex ) {
		}
	}


	/**
	 * This method finds the row within a specific column for a specific value
	 * @param value The value to be found
	 * @param columnName The name of the column (case insensitive)
	 * @return The row number of the record found, or -1 if not found.
	**/
	public int find( int value , String columnName ) {
		return find( value , getColumnIndex( columnName ) , 0 ) ;
	}

	/**
	 * This method finds the row within a specific column for a specific value
	 * @param value The value to be found
	 * @param columnName The name of the column (case insensitive)
	 * @param startRow The row with which to begin the search
	 * @return The row number of the record found, or -1 if not found.
	**/
	public int find( int value , String columnName , int startRow ) {
		return find( value , getColumnIndex( columnName ) , 0 ) ;
	}

	/**
	 * This method finds the row within a specific column for a specific value
	 * @param value The value to be found
	 * @param columnIndex The numeric column index for the cell
	 * @return The row number of the record found, or -1 if not found.
	**/
	public int find( int value , int columnIndex ) {
		return find( value , columnIndex , 0 ) ;
	}

	/**
	 * This method finds the row within a specific column for a specific value
	 * @param value The value to be found
	 * @param columnIndex The numeric column index for the cell
	 * @param startRow The row with which to begin the search
	 * @return The row number of the record found, or -1 if not found.
	**/
	public int find( int value , int columnIndex , int startRow ) {
		try {
			if ( startRow < 0 ) {
				startRow = 0 ;
			}
			
			if ( isValidRowColumn( startRow , columnIndex ) == false ) {
				return -1 ;
			}
			
			for ( int row = startRow ; row < getNumRowsActive() ; row++ ) {
				if ( getInt( row , columnIndex ) == value ) {
					return row ;
				}
			}

			return -1 ;
		} catch( Exception ex ) {
			return -1 ;
		}
	}

	/**
	 * This method finds the row within a specific column for a specific value
	 * @param value The value to be found
	 * @param columnName The name of the column (case insensitive)
	 * @return The row number of the record found, or -1 if not found.
	**/
	public int find( String value , String columnName ) {
		return find( value , getColumnIndex( columnName ) , 0 ) ;
	}

	/**
	 * This method finds the row within a specific column for a specific value
	 * @param value The value to be found
	 * @param columnName The name of the column (case insensitive)
	 * @param startRow The row with which to begin the search
	 * @return The row number of the record found, or -1 if not found.
	**/
	public int find( String value , String columnName , int startRow ) {
		return find( value , getColumnIndex( columnName ) , 0 ) ;
	}

	/**
	 * This method finds the row within a specific column for a specific value
	 * @param value The value to be found
	 * @param columnIndex The numeric column index for the cell
	 * @return The row number of the record found, or -1 if not found.
	**/
	public int find( String value , int columnIndex ) {
		return find( value , columnIndex , 0 ) ;
	}

	/**
	 * This method finds the row within a specific column for a specific value
	 * @param value The value to be found
	 * @param columnIndex The numeric column index for the cell
	 * @param startRow The row with which to begin the search
	 * @return The row number of the record found, or -1 if not found.
	**/
	public int find( String value , int columnIndex , int startRow ) {
		try {
			if ( startRow < 0 ) {
				startRow = 0 ;
			}

			if ( isValidRowColumn( startRow , columnIndex ) == false ) {
				return -1 ;
			}

			for ( int row = startRow ; row < getNumRows() ; row++ ) {
				if ( value.equals( getString( row , columnIndex ) ) ) {
					return row ;
				}
			}

			return -1 ;
		} catch( Exception ex ) {
			return -1 ;
		}
	}
}
