/**************************************************************************************************
 * @(#)ModelYearMapper.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.sql.*;
import java.util.*;

/**
 * Convert user model years into standardized model years.
 *
 * @author		Wesley Faler
 * @version		2013-04-08
**/
public class ModelYearMapper {
	class MappingEntry {
		int startUserYear;
		int endUserYear;
		int startStandardYear;
		int endStandardYear;
		String expression;
		String reverseExpression;

		MappingEntry(int startUserYearToUse, int endUserYearToUse, 
				int startStandardYearToUse, int endStandardYearToUse,
				String expressionToUse, String reverseExpressionToUse) {
			startUserYear = startUserYearToUse;
			endUserYear = endUserYearToUse;
			startStandardYear = startStandardYearToUse;
			endStandardYear = endStandardYearToUse;
			expression = expressionToUse;
			reverseExpression = reverseExpressionToUse;
		}
	}

	ArrayList<MappingEntry> mappings = new ArrayList<MappingEntry>();

	/**
	 * Build model year mappings by examining the modelYearMapping table
	 * in the execution database.
	 * @param db database to be read.
	 * @throws SQLException if anything goes wrong.
	**/
	public void buildMappings(Connection db) throws SQLException {
		mappings.clear();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			// Select mapping entries ordered by startUserYear
			sql = "select startUserModelYear, endUserModelYear, startStandardModelYear, endStandardModelYear"
					+ " from modelYearMapping"
					+ " order by startUserModelYear";
			// Populate mappings via addToMappings
			query.open(db,sql);
			while(query.rs.next()) {
				int startUserModelYear = query.rs.getInt(1);
				int endUserModelYear = query.rs.getInt(2);
				int startStandardModelYear = query.rs.getInt(3);
				int endStandardModelYear = query.rs.getInt(4);
				addToMappings(startUserModelYear,endUserModelYear,startStandardModelYear,endStandardModelYear);
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to map model years using: " + sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Add data to the set of mappings. Mappings should be added in order
	 * by user year.
	 * @param startUserYear first user-space model year affected.
	 * @param endUserYear last user-space model year affected.
	 * @param startStandardYear standard model year corresponding to startUserYear.
	 * @param endStandardYear standard model year corresponding to endUserYear.
	**/
	void addToMappings(int startUserYear, int endUserYear,
			int startStandardYear, int endStandardYear) {
		String expression = "";
		String reverseExpression = "";
		// Use simple subtraction if the sizes match.
		if((endUserYear - startUserYear) == (endStandardYear - startStandardYear)) {
			// Example: UserYear 1990 is standard year 1980 has a difference of -10
			int difference = startStandardYear - startUserYear;
			if(difference == 0) {
				expression = "(##USERYEAR##)";
				reverseExpression = expression;
			} else if(difference > 0) {
				expression = "((##USERYEAR##)+" + difference + ")";
				reverseExpression = "((##USERYEAR##)-" + difference + ")";
			} else {
				expression = "((##USERYEAR##)-" + (-difference) + ")";
				reverseExpression = "((##USERYEAR##)+" + (-difference) + ")";
			}
		} else if(endStandardYear == startStandardYear) {
			// When being mapped to a single year, just use that year in the expression.
			expression = "" + startStandardYear;
			reverseExpression = expression;
		} else {
			// Create a linear interpolation if the sizes don't match.
			// output = (uy - startUY)(endSY-startSY)/(endUY-startUY) + startSY
			expression = "floor(0.5+(((##USERYEAR##)-" + startUserYear + ")*"
					+ "(" + endStandardYear + "-" + startStandardYear + ")/"
					+ "(" + endUserYear + "-" + startUserYear + ")+" + startStandardYear + "))";

			// reverse output = (sy - startSY)(endUY-startUY)/(endSY-startSY) + startUY
			reverseExpression = "floor(0.5+(((##USERYEAR##)-" + startStandardYear + ")*"
					+ "(" + endUserYear + "-" + startUserYear + ")/"
					+ "(" + endStandardYear + "-" + startStandardYear + ")+" + startUserYear + "))";
		}
		// If the expression is the same as other mappings and the user year is contiguous,
		// expand those mappings.
		// Otherwise, add a new mapping.
		boolean found = false;
		for(MappingEntry m : mappings) {
			if(m.endUserYear + 1 == startUserYear
					&& expression.equals(m.expression) && reverseExpression.equals(m.reverseExpression)) {
				m.endUserYear = endUserYear;
				m.endStandardYear = endStandardYear;
				found = true;
				break;
			}
		}
		if(!found) {
			mappings.add(new MappingEntry(startUserYear,endUserYear,
					startStandardYear,endStandardYear,expression,reverseExpression));
		}
	}

	/**
	 * Convert an expression that references user-space model years into
	 * standardized model years suitable for joining to effects tables.
	 * @param expression column or SQL expression that yields a user-space
	 * model year.
	 * @return an expression that yields a standardized model year.
	**/
	public String convertToStandard(String expression) {
		if(mappings.size() <= 0) {
			return expression;
		}
		if(mappings.size() == 1) {
			return StringUtilities.replace(mappings.get(0).expression,"##USERYEAR##",expression);
		} else {
			String sql = "(case";
			for(MappingEntry m : mappings) {
				sql += " when (" + expression + ") between " + m.startUserYear + " and " + m.endUserYear
						+ " then " + StringUtilities.replace(m.expression,"##USERYEAR##",expression);
			}
			sql += " end)";
			return sql;
		}
	}

	/**
	 * Convert an expression that references standard-space model years into
	 * user-space model years.
	 * @param expression column or SQL expression that yields a standard
	 * model year.
	 * @return an expression that yields a user-space model year.
	**/
	public String convertToUser(String expression) {
		if(mappings.size() <= 0) {
			return expression;
		}
		if(mappings.size() == 1) {
			return StringUtilities.replace(mappings.get(0).reverseExpression,"##USERYEAR##",expression);
		} else {
			String sql = "(case";
			for(MappingEntry m : mappings) {
				sql += " when (" + expression + ") between " + m.startStandardYear + " and " + m.endStandardYear
						+ " then " + StringUtilities.replace(m.reverseExpression,"##USERYEAR##",expression);
			}
			sql += " else 0 end)";
			return sql;
		}
	}

	/**
	 * Convert a SQL fragment that may or may not contain one or
	 * more references to the MYMAP pseudo function. Statements of
	 * the form MYMAP(expression) are converted to an expression
	 * that references stanardized model years.
	 * Statements of the form MYRMAP(expression) are converted to an
	 * expression that references user-model years.
	 * @param sqlLine SQL fragment that contains zero or more occurences
	 * of the MYMAP(expression) or MYRMAP(expression) statements.
	 * @return SQL
	**/
	public String findAndConvert(String sqlLine) {
		if(sqlLine == null || sqlLine.length() <= 0) {
			return sqlLine;
		}
		String lowerCaseLine = sqlLine.toLowerCase();
		int startIndex = lowerCaseLine.indexOf("mymap(");
		while(startIndex >= 0) {
			int endIndex = lowerCaseLine.indexOf(")",startIndex+1);
			String expression = sqlLine.substring(startIndex+6,endIndex);
			String inputToFind = sqlLine.substring(startIndex,endIndex+1);
			String replacement = convertToStandard(expression);
			sqlLine = StringUtilities.replace(sqlLine,inputToFind,replacement);

			lowerCaseLine = sqlLine.toLowerCase();
			startIndex = lowerCaseLine.indexOf("mymap(");
		}

		startIndex = lowerCaseLine.indexOf("myrmap(");
		while(startIndex >= 0) {
			int endIndex = lowerCaseLine.indexOf(")",startIndex+1);
			String expression = sqlLine.substring(startIndex+7,endIndex);
			String inputToFind = sqlLine.substring(startIndex,endIndex+1);
			String replacement = convertToUser(expression);
			sqlLine = StringUtilities.replace(sqlLine,inputToFind,replacement);

			lowerCaseLine = sqlLine.toLowerCase();
			startIndex = lowerCaseLine.indexOf("myrmap(");
		}

		return sqlLine;
	}
}
