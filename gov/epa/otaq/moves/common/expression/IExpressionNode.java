/**************************************************************************************************
 * @(#)IExpressionNode.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

/**
 * Describes the operations of a node within an expression
 * 
 * @author		Wesley Faler
 * @version		2009-04-22
**/
public interface IExpressionNode {
	/**
	 * Evaluate the node.
	 * @param variables provider of dynamic values
	 * @return value of the expression
	 * @throws Exception if unable to find a variable or if its value cannot be located
	**/
	public Value evaluate(IVariableSource variables) throws Exception;

	/**
	 * Get the textual expression for this node
	 * @return the textual expression for this node
	 * @throws Exception if unable to find the variable or if its value cannot be located
	**/
	public String getExpressionText(IVariableSource variables) throws Exception;

	/**
	 * Create a copy of the node and any child nodes.
	 * @return a copy of the node
	 * @throws Exception if unable to find the variable or if its value cannot be located
	**/
	public IExpressionNode deepCopy() throws Exception;

	/**
	 * Optimize the current node, returning itself or a more optimal node to be used instead.
	 * @param optimizer controller for the optimization process
	 * @return the current node or a more optimal node to be used instead
	 * @throws Exception if unable to find the variable or if its value cannot be located
	**/
	public IExpressionNode optimize(Optimizer optimizer) throws Exception;
}
