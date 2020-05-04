/**************************************************************************************************
 * @(#)ExpressionParserTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

import junit.framework.*;
import java.util.*;

/**
 * Test Case for the ExpressionParser class
 *
 * @author		Wesley Faler
 * @version		2011-07-06
**/
public class ExpressionParserTest extends TestCase implements IVariableSource {
	/** Variable values by name **/
	TreeMap<String,Value> variableValues = new TreeMap<String,Value>();

	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public ExpressionParserTest(String name) {
		super(name);

		variableValues.put("alpha",new Value(1));
		variableValues.put("beta",new Value(2));
		variableValues.put("gamma",new Value(3));
	}

	/**
	 * Get the value of a variable.
	 * @param name variable name
	 * @return value of the variable
	 * @throws Exception if unable to find the variable or if its value cannot be located
	**/
	public Value getValue(String name) throws Exception {
		if(!variableValues.containsKey(name)) {
			throw new Exception("Unknown variable " + name);
		}
		return variableValues.get(name);
	}

	/**
	 * Get the textual name of a variable or its textual expression if it is not a simple value.
	 * @param name variable name
	 * @return name of the variable or its expression if not a simple value
	 * @throws Exception if unable to find the variable or if its value cannot be located
	**/
	public String getExpressionText(String name) throws Exception {
		if(!variableValues.containsKey(name)) {
			throw new Exception("Unknown variable " + name);
		}
		return name;
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile1() throws Exception {
		check("1+2",3);
		check("1 + 2",3);
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile2() throws Exception {
		check("alpha+beta*gamma",7);
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile3() throws Exception {
		check("(alpha+beta)*gamma",9);
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile4() throws Exception {
		check("1+2^3+4",13);
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile5() throws Exception {
		check("1+2^3*4",33);
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile6() throws Exception {
		check("1+(2-1+1)^3*4",33);
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile7() throws Exception {
		check("5*2^3-4",36);
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile8() throws Exception {
		check("if(2=0,3,5)",5);
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile9() throws Exception {
		check("2.0408163 * (147.91 - 0)",2.0408163 * (147.91 - 0));
	}

	/**
	 * Tests compile() via evaluating the resulting expression
	 * @throws Exception from compile() or evaluate()
	**/
	public void testCompile10() throws Exception {
		check("and(1<2,10<20)",1);
	}

	/**
	 * Compile and execute an expression, using the default variables of course.
	 * @param expression expression to be evaluated
	 * @param expectedValue numeric value expected from the evaluation
	 * @throws Exception from compile() or evaluate()
	**/
	private void check(String expression, double expectedValue) throws Exception {
		ExpressionParser parser = new ExpressionParser();
		IExpressionNode root = parser.compile(expression);
		assertNotNull("Expected result from expression compile of " + expression,root);
		Value value = root.evaluate(this);
		assertNotNull("Expected value from evaluate of " + expression,value);
		double t = value.getNumber();
		assertEquals("Got wrong answer from " + expression,expectedValue,t);
	}
}
