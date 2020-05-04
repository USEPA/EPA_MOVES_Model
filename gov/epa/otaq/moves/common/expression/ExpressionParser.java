/**************************************************************************************************
 * @(#)ExpressionParser.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

import java.util.*;
import gov.epa.otaq.moves.common.*;


/**
 * Parse an expression generating an ExpressionNode that represents the expression.
 * 
 * @author		Wesley Faler
 * @version		2009-05-23
**/
public class ExpressionParser {
	/** Token representing the end of the expression **/
	Tokenizer.Token eofToken = new Tokenizer.Token("",0);
	/** The most recently parsed token **/
	Tokenizer.Token currentToken = null;
	/** The object responsible for generating the token stream **/
	Tokenizer tokenizer = null;
	/**
	 * Holds changes to variable name prefixes.  For instance, when used in a stencil-type
	 * method, an expression may reference "ff.mtbe" but should be applied to a specific
	 * instance and be rewritten as "ffbase.mtbe".  In this case, the prefixes should have
	 * a key of "ff." and data of "ffbase.".
	**/
	TreeMapIgnoreCase variablePrefixChanges = null;

	/**
	 * Compile an expression
	 * @param textToUse the expression
	 * @return an expression node, never null
	 * @throws Exception if anything is amiss in the expression
	**/
	public IExpressionNode compile(String textToUse) throws Exception {
		return compile(textToUse,(TreeMapIgnoreCase)null);
	}

	/**
	 * Compile an expression
	 * @param textToUse the expression
	 * @param variablePrefixChangesToUse changes to variable name prefixes, may be null,
	 * must be pairs of original prefix and output prefix
	 * @return an expression node, never null
	 * @throws Exception if anything is amiss in the expression
	**/
	public IExpressionNode compile(String textToUse,
			String[] variablePrefixChangesToUse) throws Exception {
		TreeMapIgnoreCase prefixes = null;
		if(variablePrefixChangesToUse != null) {
			prefixes = new TreeMapIgnoreCase();
			for(int i=0;i<variablePrefixChangesToUse.length;i+=2) {
				prefixes.put(variablePrefixChangesToUse[i+0],variablePrefixChangesToUse[i+1]);
			}
		}
		return compile(textToUse,prefixes);
	}

	/**
	 * Compile an expression
	 * @param textToUse the expression
	 * @param variablePrefixChangesToUse changes to variable name prefixes, may be null
	 * @return an expression node, never null
	 * @throws Exception if anything is amiss in the expression
	**/
	public IExpressionNode compile(String textToUse,
			TreeMapIgnoreCase variablePrefixChangesToUse) throws Exception {
		//System.out.println("Compiling Expression: \"" + textToUse + "\"");
		try {
			variablePrefixChanges = variablePrefixChangesToUse;
			tokenizer = new Tokenizer(textToUse);
			currentToken = eofToken;
	
			getNextToken();
			IExpressionNode result = expression();
			expect(0);
	
			if(result == null) {
				throw new Exception("Syntax error: no expression");
			}
			return result;
		} catch(Exception e) {
			Logger.log(LogMessageCategory.ERROR,
					"Unable to compile expression \"" + textToUse + "\"");
			throw e;
		}
	}

	private void getNextToken() throws Exception {
		currentToken = tokenizer.getNextToken();
		if(currentToken == null) {
			currentToken = eofToken;
		}
	}

	private boolean accept(int tokenType) throws Exception {
		if(tokenType == 0 && currentToken == null) {
			return true;
		}
		if(currentToken == null) {
			return false;
		}
		if(currentToken.type == tokenType) {
			getNextToken();
			return true;
		}
		return false;
	}

	private boolean expect(int tokenType) throws Exception {
		if(accept(tokenType)) {
			return true;
		}
		String t = "Syntax error";
		t += ": Wanted tokenType=" + tokenType + ", got ";
		if(currentToken == null) {
			t += "null";
		} else {
			t += currentToken.type + "\"" + currentToken.token + "\"";
		}
		throw new Exception(t);
	}

	private IExpressionNode expression() throws Exception {
		IExpressionNode result = null;
		if(currentToken.type == Tokenizer.TOKEN_NEGATIVE) {
			getNextToken();
			result = new Common.NegativeNode(preTerm());
		} else {
			result = preTerm();
		}
		while(currentToken.type == Tokenizer.TOKEN_OPERATOR
				&& Tokenizer.isComparisonOperator(currentToken.token)) {
			String t = currentToken.token;
			getNextToken();
			result = new Common.ComparisonNode(result,preTerm(),t);
			break;
		}
		return result;
	}

	private IExpressionNode preTerm() throws Exception {
		IExpressionNode result = term();
		while(currentToken.type == Tokenizer.TOKEN_OPERATOR) {
			if(currentToken.token.equals("+")) {
				getNextToken();
				result = new Common.AddNode(result,term());
			} else if(currentToken.token.equals("-")) {
				getNextToken();
				result = new Common.SubtractNode(result,term());
			} else {
				break;
			}
		}
		return result;
	}

	private IExpressionNode term() throws Exception {
		IExpressionNode result = powerTerm() /* factor() */;
		while(currentToken.type == Tokenizer.TOKEN_OPERATOR) {
			if(currentToken.token.equals("*")) {
				getNextToken();
				result = new Common.MultiplyNode(result,powerTerm() /* factor() */);
			} else if(currentToken.token.equals("/")) {
				getNextToken();
				result = new Common.DivideNode(result,powerTerm() /* factor() */);
			} else if(currentToken.token.equals("%")) {
				getNextToken();
				result = new Common.ModulusNode(result,powerTerm() /* factor() */);
			} else {
				break;
			}
		}
		return result;
	}

	private IExpressionNode powerTerm() throws Exception {
		IExpressionNode result = factor();
		while(currentToken.type == Tokenizer.TOKEN_OPERATOR) {
			if(currentToken.token.equals("^")) {
				getNextToken();
				result = new Common.PowNode(result,factor());
			} else {
				break;
			}
		}
		return result;
	}

	private IExpressionNode factor() throws Exception {
		IExpressionNode result = null;
		String text = currentToken.token;

		if(accept(Tokenizer.TOKEN_VARIABLE)) {
			if(variablePrefixChanges != null) {
				text = StringUtilities.doReplacements(text,variablePrefixChanges);
			}
			result = new Common.VariableNode(text);
		} else if(accept(Tokenizer.TOKEN_NUMBER) || accept(Tokenizer.TOKEN_LITERAL)) {
			result = new Common.LiteralNode(text);
		} else if(accept(Tokenizer.TOKEN_FUNCTION)) {
			if(text.equalsIgnoreCase("cos")) {
				result = new Common.CosineNode(expression());
			} else if(text.equalsIgnoreCase("sin")) {
				result = new Common.SineNode(expression());
			} else if(text.equalsIgnoreCase("tan")) {
				result = new Common.TanNode(expression());
			} else if(text.equalsIgnoreCase("exp")) {
				result = new Common.ExpNode(expression());
			} else if(text.equalsIgnoreCase("ln")) {
				result = new Common.LnNode(expression());
			} else if(text.equalsIgnoreCase("sqrt")) {
				result = new Common.SqrtNode(expression());
			} else if(text.equalsIgnoreCase("if")) {
				IExpressionNode condition = expression();
				expect(Tokenizer.TOKEN_COMMA);
				IExpressionNode trueValue = expression();
				expect(Tokenizer.TOKEN_COMMA);
				IExpressionNode falseValue = expression();
				result = new Common.IfNode(condition,trueValue,falseValue);
			} else if(text.equalsIgnoreCase("pow")) {
				IExpressionNode left = expression();
				expect(Tokenizer.TOKEN_COMMA);
				IExpressionNode right = expression();
				result = new Common.PowNode(left,right);
			} else if(text.equalsIgnoreCase("not")) {
				result = new Common.NotNode(expression());
			} else if(text.equalsIgnoreCase("and")) {
				Common.AndNode andNode = new Common.AndNode();
				result = expression();
				andNode.add(result);
				while(true) {
					if(accept(Tokenizer.TOKEN_COMMA)) {
						result = expression();
						andNode.add(result);
					} else {
						result = andNode;
						break;
					}
				}
			} else if(text.equalsIgnoreCase("or")) {
				Common.OrNode orNode = new Common.OrNode();
				result = expression();
				orNode.add(result);
				while(true) {
					if(accept(Tokenizer.TOKEN_COMMA)) {
						result = expression();
						orNode.add(result);
					} else {
						result = orNode;
						break;
					}
				}
			} else {
				throw new Exception("Unknown function: " + text);
			}
			expect(Tokenizer.TOKEN_CLOSE_PAREN);
		} else if(accept(Tokenizer.TOKEN_OPEN_PAREN)) {
			result = expression();
			expect(Tokenizer.TOKEN_CLOSE_PAREN);
		} else {
			throw new Exception("Syntax error while parsing factor from \"" + text + "\"");
		}
		return result;
	}
}
