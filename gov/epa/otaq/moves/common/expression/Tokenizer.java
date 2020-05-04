/**************************************************************************************************
 * @(#)Tokenizer.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common.expression;

import java.util.*;

/**
 * Convert an expression into a series of Tokens
 * 
 * @author		Wesley Faler
 * @version		2009-04-30
**/
public class Tokenizer {
	public static int TOKEN_NUMBER = 1;
	public static int TOKEN_LITERAL = 2;
	private static int TOKEN_WORD = 3;
	public static int TOKEN_WHITESPACE = 4;
	public static int TOKEN_OPEN_PAREN = 5;
	public static int TOKEN_CLOSE_PAREN = 6;
	public static int TOKEN_COMMA = 7;
	public static int TOKEN_OPERATOR = 8;
	public static int TOKEN_NEGATIVE = 9;
	public static int TOKEN_VARIABLE = 10;
	public static int TOKEN_FUNCTION = 11;

	public static class Token {
		public String token;
		public int type;

		public Token() {
		}

		public Token(String tokenToUse, int typeToUse) {
			token = tokenToUse;
			type = typeToUse;
		}

		public String toString() {
			return "" + type + ": \"" + token + "\"";
		}
	}

	private Stack<Token> tokens = new Stack<Token>();
	private char[] allText;
	private int nextTextCursor = 0;
	private String currentTokenText = "";
	private int state = 0;
	private char quoteChar = 0;

	/**
	 * Constructor
	 * @param textToUse text to be tokenized
	**/
	public Tokenizer(String textToUse) {
		allText = textToUse.toCharArray();
		nextTextCursor = 0;
		state = 0;
	}

	/**
	 * Obtain the next Token or null if no more are to be found.
	 * Whitespace tokens are skipped and functions and variables are distinguished from
	 * one another.
	 * @return a Token or null if no more are available
	 * @throws Exception if a tokenizing error occurs
	**/
	public Token getNextToken() throws Exception {
		Token result = getNextTokenNoSpace();
		if(result != null) {
			if(result.type == TOKEN_WORD) {
				result.type = TOKEN_VARIABLE;
				Token next = getNextTokenNoSpace();
				if(next != null) {
					if(next.type == TOKEN_OPEN_PAREN) {
						result.type = TOKEN_FUNCTION;
					} else {
						putBackToken(next);
					}
				}
			}
		}
		return result;
	}

	/**
	 * Obtain the next Token or null if no more are to be found.
	 * Whitespace tokens are skipped but functions and variables are not distinguished
	 * from one another.
	 * @return a Token or null if no more are available
	 * @throws Exception if a tokenizing error occurs
	**/
	private Token getNextTokenNoSpace() throws Exception {
		Token result = null;
		while(result == null) {
			if(tokens.size() > 0) {
				result = tokens.pop();
				return result;
			}
			result = getNextTokenCore();
			if(result == null) {
				return null;
			}
			if(result.type == TOKEN_WHITESPACE) {
				result = null;
			}
		}
		return result;
	}

	/** Array of basic math operators **/
	private static String[] basicOperators = {
		"+", "-", "*", "/", "^", "%" // % is integer Modulus
	};
	/** Array of comparison operators, including some common aliases **/
	private static String[] comparisonOperators = {
		"=", "==", // both = and == mean "is equal to"
		"<>", "!=", // both <> and != mean "not equal to"
		"<", "<=", ">", ">="
	};

	/**
	 * Return true if a String is a supported mathematical or comparison operator.
	 * @param t String to be tested
	 * @return true if a character is a supported mathematical operator
	**/
	public static boolean isOperator(String t) {
		for(int i=0;i<basicOperators.length;i++) {
			if(t.equals(basicOperators[i])) {
				return true;
			}
		}
		return isComparisonOperator(t);
	}

	/**
	 * Return true if a String is a supported comparison operator.
	 * @param t String to be tested
	 * @return true if a character is a supported comparison operator
	**/
	public static boolean isComparisonOperator(String t) {
		for(int i=0;i<comparisonOperators.length;i++) {
			if(t.equals(comparisonOperators[i])) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Obtain the next Token or null if no more are to be found.
	 * Whitespace tokens are returned.
	 * @return a Token or null if no more are available
	 * @throws Exception if a tokenizing error occurs
	**/
	private Token getNextTokenCore() throws Exception {
		if(allText == null || allText.length <= 0) {
			return null;
		}
		char c;
		String s;
		while(true) {
			c = getNextChar();
			switch(state) {
				default:
					state = 0;
				case 0: // looking for value
					if(c == 0) {
						return null;
					} else if(c == '+') {
						// Ignore + here, since it would be used to prefix a value
						// as being positive.
					} else if(c == '-' || Character.isDigit(c)) {
						state = 1; // start getting numeric value
						currentTokenText = "" + c;
					} else if(c == '.') {
						state = 2; // start getting numeric value after the decimal digit
						currentTokenText = "" + c;
					} else if(c == '\"' || c == '\'') {
						quoteChar = c;
						currentTokenText = "";
						state = 6;
					} else if(Character.isWhitespace(c)) {
						return new Token(" ",TOKEN_WHITESPACE);
					} else if(c == '(') {
						return new Token("(",TOKEN_OPEN_PAREN);
					} else if(c == ')') {
						state = 5;
						return new Token(")",TOKEN_CLOSE_PAREN);
					} else if(Character.isLetter(c)) {
						currentTokenText = "" + c;
						state = 8;
					} else if(isOperator("" + c)) {
						currentTokenText = "" + c;
						state = 9;
					} else {
						throw new Exception("Unexpected character \""
								+ c + "\" while getting value");
					}
					break;
				case 1:
					if(Character.isDigit(c)) {
						currentTokenText += c;
					} else if(c == '.') {
						currentTokenText += c;
						state = 2;
					} else if(c == 'e' || c == 'E') {
						currentTokenText += c;
						state = 3;
					} else {
						putBackCharacter();
						state = 5;
						if(currentTokenText.equals("-")) {
							return new Token(currentTokenText,TOKEN_NEGATIVE);
						} else {
							return new Token(currentTokenText,TOKEN_NUMBER);
						}
					}
					break;
				case 2:
					if(Character.isDigit(c)) {
						currentTokenText += c;
					} else if(c == 'e' || c == 'E') {
						currentTokenText += c;
						state = 3;
					} else {
						putBackCharacter();
						state = 5;
						if(currentTokenText.equals("-")) {
							return new Token(currentTokenText,TOKEN_NEGATIVE);
						} else {
							return new Token(currentTokenText,TOKEN_NUMBER);
						}
					}
					break;
				case 3:
					if(Character.isDigit(c)) {
						currentTokenText += c;
						state = 4;
					} else if(c == '+' || c == '-') {
						currentTokenText += c;
						state = 4;
					} else {
						putBackCharacter();
						state = 5;
						if(currentTokenText.equals("-")) {
							return new Token(currentTokenText,TOKEN_NEGATIVE);
						} else {
							return new Token(currentTokenText,TOKEN_NUMBER);
						}
					}
					break;
				case 4:
					if(Character.isDigit(c)) {
						currentTokenText += c;
					} else {
						putBackCharacter();
						state = 5;
						if(currentTokenText.equals("-")) {
							return new Token(currentTokenText,TOKEN_NEGATIVE);
						} else {
							return new Token(currentTokenText,TOKEN_NUMBER);
						}
					}
					break;
				case 5:
					if(c == 0) {
						return null;
					} else if(Character.isWhitespace(c)) {
						return new Token(" ",TOKEN_WHITESPACE);
					} else if(c == ',') {
						state = 0;
						return new Token(",",TOKEN_COMMA);
					} else if(c == '(') {
						state = 0;
						return new Token("(",TOKEN_OPEN_PAREN);
					} else if(c == ')') {
						return new Token(")",TOKEN_CLOSE_PAREN);
					} else if(isOperator("" + c)) {
						currentTokenText = "" + c;
						state = 9;
					} else {
						throw new Exception("Unexpected character \""
								+ c + "\" while getting operator");
					}
					break;
				case 6: // Reading quoted text
					if(c == 0) {
						throw new Exception("Unexpected end of text during quoted text");
					} else if(c == '\\') {
						state = 7;
					} else if(c == quoteChar) {
						state = 5;
						return new Token(currentTokenText,TOKEN_LITERAL);
					}
					break;
				case 7: // Doing escape sequence within quoted text
					if(c == 0) {
						throw new Exception("Unexpected end of text during escape sequence");
					} else if(c == 'n') {
						currentTokenText += "\n";
						state = 6;
					} else if(c == 'r') {
						currentTokenText += "\r";
						state = 6;
					} else if(c == 't') {
						currentTokenText += "\t";
						state = 6;
					} else if(c == '\\') {
						currentTokenText += "\\";
						state = 6;
					} else {
						throw new Exception("Unknown escape sequence \\" + c);
					}
					break;
				case 8: // Reading a word, such as a variable or function name
					if(Character.isLetter(c) || Character.isDigit(c) || c == '_' || c == '.') {
						currentTokenText += c;
					} else {
						putBackCharacter();
						state = 5;
						return new Token(currentTokenText,TOKEN_WORD);
					}
					break;
				case 9: // Reading operator
					s = currentTokenText+c;
					if(isOperator(s)) {
						currentTokenText = s;
					} else {
						putBackCharacter();
						state = 0;
						return new Token(currentTokenText,TOKEN_OPERATOR);
					}
					break;
			}
		}
	}

	/**
	 * Put back a Token
	 * @param t the Token to be put back
	**/
	private void putBackToken(Token t) {
		tokens.push(t);
	}

	/**
	 * Get the next character from the text stream
	 * @return the next character or 0 if there is none
	**/
	private char getNextChar() {
		if(nextTextCursor < allText.length) {
			return allText[nextTextCursor++];
		} else {
			nextTextCursor++;
		}
		return 0;
	}

	/** Add a character back to the stream **/
	private void putBackCharacter() {
		nextTextCursor--;
	}
}
