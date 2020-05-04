/**************************************************************************************************
 * @(#)TokenizerTest.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

import junit.framework.*;

/**
 * Test Case for the Tokenizer class
 *
 * @author		Wesley Faler
 * @version		2009-04-30
**/
public class TokenizerTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public TokenizerTest(String name) {
		super(name);
	}

	/**
	 * Tests getNextToken()
	 * @throws Exception from getNextToken()
	**/
	public void testGetNextToken1() throws Exception {
		Tokenizer tokenizer = new Tokenizer("1+2*3-4");
		Tokenizer.Token t = tokenizer.getNextToken();
		assertNotNull("Got null, expected 1",t);
		assertEquals("Wanted number 1",Tokenizer.TOKEN_NUMBER,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected +",t);
		assertEquals("Wanted operator +",Tokenizer.TOKEN_OPERATOR,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected 2",t);
		assertEquals("Wanted number 2",Tokenizer.TOKEN_NUMBER,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected *",t);
		assertEquals("Wanted operator *",Tokenizer.TOKEN_OPERATOR,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected 3",t);
		assertEquals("Wanted number 3",Tokenizer.TOKEN_NUMBER,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected -",t);
		assertEquals("Wanted operator -",Tokenizer.TOKEN_OPERATOR,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected 4",t);
		assertEquals("Wanted number 4",Tokenizer.TOKEN_NUMBER,t.type);
	}

	/**
	 * Tests getNextToken()
	 * @throws Exception from getNextToken()
	**/
	public void testGetNextToken2() throws Exception {
		Tokenizer tokenizer = new Tokenizer("a+b*c.e-d");
		Tokenizer.Token t = tokenizer.getNextToken();
		assertNotNull("Got null, expected a",t);
		assertEquals("Wanted variable a",Tokenizer.TOKEN_VARIABLE,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected +",t);
		assertEquals("Wanted operator +",Tokenizer.TOKEN_OPERATOR,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected b",t);
		assertEquals("Wanted variable b",Tokenizer.TOKEN_VARIABLE,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected *",t);
		assertEquals("Wanted operator *",Tokenizer.TOKEN_OPERATOR,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected c.e",t);
		assertEquals("Wanted variable c.e",Tokenizer.TOKEN_VARIABLE,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected -",t);
		assertEquals("Wanted operator -",Tokenizer.TOKEN_OPERATOR,t.type);

		t = tokenizer.getNextToken();
		assertNotNull("Got null, expected d",t);
		assertEquals("Wanted variable d",Tokenizer.TOKEN_VARIABLE,t.type);
	}
}
