/**************************************************************************************************
 * @(#)Optimizer.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

import java.util.*;
import gov.epa.otaq.moves.common.*;

/**
 * Optimize an expression tree
 * 
 * @author		Wesley Faler
 * @version		2009-06-16
**/
public class Optimizer {
	/** Utility object for a constant value of 0.0 **/
	public static Common.LiteralNode zero = new Common.LiteralNode(0);
	/** Utility object for a constant value of 1.0 **/
	public static Common.LiteralNode one = new Common.LiteralNode(1);

	/**
	 * Check a node against a literal value
	 * @param node node to be checked
	 * @param v value to be checked
	 * @return true if the node is a LiteralNode with a matching value
	**/
	public static boolean isLiteral(IExpressionNode node, double v) {
		if(node instanceof Common.LiteralNode) {
			Common.LiteralNode l = (Common.LiteralNode)node;
			if(l.value.getNumber() == v) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Check a node's type
	 * @param node node to be checked
	 * @return true if the node is a LiteralNode
	**/
	public static boolean isLiteral(IExpressionNode node) {
		if(node instanceof Common.LiteralNode) {
			return true;
		}
		return false;
	}

	public interface IOptimizerVariableSource {
		/**
		 * Get the value of a variable and only a variable.
		 * @param name variable name
		 * @return value of the variable or null if the variable could not be found
		**/
		public Value getVariableValue(String name);
		
		/**
		 * Get the expression behind a named expression.
		 * @param name expression name
		 * @return root of the expression or null if the expression could not be found
		**/
		public IExpressionNode getExpression(String name);
	}

	/** Provider of variable and expression details **/
	IOptimizerVariableSource source = null;
	/** Count of the number of changes **/
	public int changeCounter = 0;
	/** Prefixes for variables that should not be resolved into values **/
	String[] prefixesToKeep = null;
	/** Aliases to be used during optimization, if any **/
	TreeMapIgnoreCase aliases = null;
	/** Source used to obtain textual expressions **/
	IVariableSource textSource = null;

	/**
	 * Constructor
	 * @param sourceToUse Provider of variable and expression details
	 * @param prefixesToKeepToUse Prefixes for variables that should not be resolved into values
	 * @param aliasesToUse Aliases to be used during optimization, if any
	**/
	public Optimizer(IOptimizerVariableSource sourceToUse, String[] prefixesToKeepToUse,
			TreeMapIgnoreCase aliasesToUse) {
		source = sourceToUse;
		prefixesToKeep = prefixesToKeepToUse;
		aliases = aliasesToUse;
		if(source instanceof IVariableSource) {
			textSource = (IVariableSource)source;
		}
	}

	/** Called by nodes to denote a change in their structure **/
	public void changed() {
		changeCounter++;
	}

	/**
	 * Get the unaliased name of a name
	 * @param name variable or expression name
	 * @return name to be used inplace of name
	**/
	private String getFinalName(String name) {
		if(aliases != null && aliases.containsKey(name)) {
			return (String)aliases.get(name);
		}
		return name;
	}

	/**
	 * Check a variable or expression to determine if it should be a symbolic
	 * term or converted to a literal.
	 * @param name variable or expression name
	 * @return true if the item should remain symbolic
	**/
	public boolean shouldRemainSymbolic(String name) {
		name = getFinalName(name);
		for(int i=0;i<prefixesToKeep.length;i++) {
			if(name.startsWith(prefixesToKeep[i])) {
				//System.out.println(name + " should remain symbolic based on "
				//		+ prefixesToKeep[i]);
				return true;
			}
		}
		return false;
	}

	/**
	 * Get the value of a variable and only a variable.
	 * @param name variable name
	 * @return value of the variable or null if the variable could not be found
	**/
	public Value getVariableValue(String name) {
		name = getFinalName(name);
		return source.getVariableValue(name);
	}

	/**
	 * Get the expression behind a named expression.
	 * @param name expression name
	 * @return root of the expression or null if the expression could not be found
	**/
	public IExpressionNode getExpression(String name) {
		name = getFinalName(name);
		IExpressionNode result = source.getExpression(name);
		if(result != null) {
			try {
				result = result.deepCopy();
			} catch(Exception e) {
				// Nothing to do here
			}
		}
		return result;
	}

	/**
	 * Optimize a node and its children
	 * @param root node to be optimized
	 * @return node to be used in place of the input root node
	 * @throws Exception if anything goes wrong
	**/
	public IExpressionNode optimize(IExpressionNode root) throws Exception {
		int localCounter = changeCounter + 1;
		while(localCounter != changeCounter) {
			localCounter = changeCounter;
//			try {
				root = root.optimize(this);
//			} catch(Exception e) {
//				// Nothing to do here
//				break;
//			}
		}
		return root;
	}

	/**
	 * Obtain the expression for a node
	 * @param node node to be examined
	 * @return expression text
	 * @throws Exception if anything goes wrong
	**/
	public String getExpressionText(IExpressionNode node) throws Exception {
		if(textSource != null) {
			return node.getExpressionText(textSource);
		} else {
			return "";
		}
	}
}
