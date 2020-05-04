/**************************************************************************************************
 * @(#)IVariableSource.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

import java.util.*;

/**
 * A provider of values for evaluating an expression.
 * 
 * @author		Wesley Faler
 * @version		2009-04-22
**/
public interface IVariableSource {
	/**
	 * Get the value of a variable.
	 * @param name variable name
	 * @return value of the variable
	 * @throws Exception if unable to find the variable or if its value cannot be located
	**/
	public Value getValue(String name) throws Exception;

	/**
	 * Get the textual name of a variable or its textual expression if it is not a simple value.
	 * @param name variable name
	 * @return name of the variable or its expression if not a simple value
	 * @throws Exception if unable to find the variable or if its value cannot be located
	**/
	public String getExpressionText(String name) throws Exception;
}
