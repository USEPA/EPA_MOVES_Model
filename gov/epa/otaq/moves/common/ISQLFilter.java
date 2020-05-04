/**************************************************************************************************
 * @(#)ISQLFilter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;

/**
 * Interface for callback objects providing filtering functions during another object's
 * walk of a ResultSet.
 * 
 * @author		wgfaler
 * @version		2008-05-22
**/
public interface ISQLFilter {
	/**
	 * Called after a query has been opened to determine if it should be used at all.
	 * @param metaData column names and types for the results
	 * @return true if the results should be iterated, calling canUseRow for each.
	**/
	boolean onOpen(ResultSetMetaData metaData) throws Exception;

	/**
	 * Called before each row to determine its use.
	 * @param rs results positioned on the current row
	 * @return ture if the row should be used, false if it should be ignored
	**/
	boolean canUseRow(ResultSet rs) throws Exception;

	/**
	 * Called after the query has been completed, either successfully,  with an
	 * error, or because onOpen returned false.
	**/
	void onClose();
}
