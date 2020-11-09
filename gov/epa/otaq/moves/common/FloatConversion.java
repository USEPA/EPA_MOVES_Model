/**************************************************************************************************
 * @(#)FloatConversion.java 
 *
 *
 *
 *************************************************************************************************/

package gov.epa.otaq.moves.common;

import java.io.*;
import java.util.*;
import java.math.*;
import java.text.*;

/**
 * This static class provides various general purpose float conversion utilities
 * The term "scale" refers to the number of places after the decimal point.
 *
 * @author		Cimulus
 * @version		2004-05-05
**/

public class FloatConversion {

	/**
	 * Determines if a string value is in a valid float format.
	 * @param s The string to be evaluated.
	 * @return Returns true if the string is a valid float, false if not.
	 * This method will not throw an exception.
	**/
	public static boolean isFloat( String s ) {
		try {
			float f = Float.parseFloat( s ) ;
			return true ;
		} catch( Exception ex ) {
			return false ;
		}
	}

	/**
	 * The FloatConversion class supports scales values between 0 and 6.
	 * This method takes an integer and returns a compliant value between 0 and 6.
	 * @param scale The scale that you want to validate.
	 * @return Returns 0 if the scale is less than 0, 6 if the scale is greater than 6,
	 * or returns the scale parameter passed if it is between 0 and 6.
	 * This method will not throw an exception.
	**/
	public static int getValidScale( int scale ) {
		scale = scale < 0 ? 0 : scale ;
		scale = scale > 6 ? 6 : scale ;
		return scale ;
	}
				
	/**
	 * Converts a String to a float with the specified scale.
	 * @param s The string to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a float representation of the String with the specified scale.
	 * The method returns 0.0 if the string parameter is not a valid float.
	 * This method will not throw an exception.
	**/
	public static float toFloat( String s , int scale ) {
		BigDecimal de ;

		try {
			de = toBigDecimal( s , scale ) ;
			return de.floatValue() ;

		} catch( Exception ex ) {
			return 0.0F ;
		}
		
	}

	/**
	 * Rounds a float value with the specified scale.
	 * @param f The float to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a float representation of the float with the specified scale.
	 * The method rounds as needed.
	 * This method will not throw an exception.
	**/
	public static float toFloat( float f , int scale ) {
		BigDecimal de = toBigDecimal( f , scale ) ;
		return de.floatValue() ;
	}


	/**
	 * Converts a String to a BigDecimal with the specified scale.
	 * @param s The string to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a BigDecimal representation of the String with the specified scale.
	 * The method returns 0.0 if the string parameter is not a valid number.
	 * This method will not throw an exception.
	**/
	public static BigDecimal toBigDecimal( String s , int scale ) {
		BigDecimal de , de2 ;
		
		try {
			scale = getValidScale( scale ) ;
			de = new BigDecimal( s ) ;
			de2 = de.setScale( scale , RoundingMode.HALF_UP ) ;
			return de2 ;
			
		} catch( Exception ex ) {
			return new BigDecimal( 0.0F ) ;
		}
	}

	/**
	 * Converts a String to a BigDecimal with the specified scale.
	 * @param f The float to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a BigDecimal representation of the float with the specified scale.
	 * The method returns 0.0 if the string parameter is not a valid number.
	 * This method will not throw an exception.
	**/
	public static BigDecimal toBigDecimal( float f , int scale ) {
		String s = "" , stringFloat ;			
		BigDecimal de , de2 ;
		long li ;
		double mult = 0.0 , mult2 = 0.0 ;
		float f2 ;
		StringBuffer sb = new StringBuffer( 100 ) ;
		int x , pos , posE , posEP , numDecimals = 0 ;
		char lastChar , nextChar ;

		try {
			scale = getValidScale( scale ) ;
			stringFloat = Float.toString( f ) ;

			pos = stringFloat.indexOf( '.' ) ;
			posE = stringFloat.indexOf( "E-" ) ;
			posEP = stringFloat.indexOf( "E+" ) ;
			
			if ( pos > 0 && posE > 0 ) {
				s = stringFloat.substring( posE + 2 ) ;
				numDecimals = Integer.parseInt( s ) ;
				
				if ( f < 0.0F ) {
					sb.append( "-" ) ;
				}
				sb.append( "0." ) ;
				
				for ( x = 1 ; x < numDecimals ; x++ ) {
					sb.append( "0" ) ;
				}
				
				sb.append( stringFloat.charAt( pos - 1 ) ) ;
				sb.append( stringFloat.substring( pos + 1 , posE ) ) ;
				stringFloat = sb.toString() ;

				de = new BigDecimal( sb.toString() ) ;
				de2 = de.setScale( scale , RoundingMode.HALF_UP ) ;

				return de2 ;
			} 
			
		
			// No decimal point in the string
			if ( pos == -1 ) {
				de = new BigDecimal( stringFloat ) ;
				de2 = de.setScale( scale , RoundingMode.HALF_UP ) ;
				return de2 ;
				}
			
			numDecimals = stringFloat.length() - pos - 1 ;
			if ( numDecimals <= scale ) {
				de = new BigDecimal( stringFloat ) ;
				de2 = de.setScale( scale , RoundingMode.HALF_UP ) ;
				return de2 ;
			}
			
			sb.append( stringFloat.substring( 0 , pos + scale + 1 ) ) ;

			lastChar = stringFloat.charAt( pos + scale ) ;
			nextChar = stringFloat.charAt( pos + scale + 1 ) ;

			if ( nextChar <= '4' ) {
				de = new BigDecimal( sb.toString() ) ;
				de2 = de.setScale( scale , RoundingMode.HALF_UP ) ;
				return de2 ;
			}
			
			for ( x = pos + scale ; x >= 0 ; x-- ) {
				lastChar = stringFloat.charAt( x ) ; 

				if ( lastChar == '.' ) {
					x-- ;
					lastChar = stringFloat.charAt( x ) ; 
				}
				
				if ( lastChar == '-' ) {
					sb.insert( 1 , '1' ) ;
					break ;
				}
				
				if ( lastChar != '9' ) {
					sb.setCharAt( x , ( char ) ( lastChar + 1 ) ) ;
					break ;
				}

				if ( lastChar == '9' ) {
					sb.setCharAt( x , '0' ) ;
				}

				if ( x == 0 ) {
					sb.insert( 0 , '1' ) ;
					break ;
				}
			}

			de = new BigDecimal( sb.toString() ) ;
			de2 = de.setScale( scale , RoundingMode.HALF_UP ) ;

			return de2 ;
		} catch( Exception exc ) {
			Logger.log(LogMessageCategory.INFO, "\tFloatConversion.toBigDecimal  " + f + " ; scale = " + scale + " ; s = " + s + "\nException:" + exc.getMessage() ) ;
			return new BigDecimal( "0" ) ;
		}
	}

	/**
	 * Converts a float value to a String with the specified scale.
	 * @param f The float to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a String representation of the float with the specified scale.
	 * The method rounds as needed.
	 * This method will not throw an exception.
	**/
	public static String toString( float f , int scale ) {
		BigDecimal de = toBigDecimal( f , scale ) ;
		return de.toString() ;
	}
	
	/**
	 * Converts an Object value to a String with the specified scale.
	 * @param ob The object to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a String representation of the float with the specified scale.
	 * If the object is a float, then it converts the value as a float. If the object is
	 * null, then a zero value is converted, with the specified scale. For any other object,
	 * the value is first converted to a String, and then the String will be converted to a 
	 * float, if possible and formatted with the specified scale, or 0.0 will be formatted, if the
	 * String is not a valid numeric.
	 * The method rounds as needed.
	 * This method will not throw an exception.
	**/
	public static String toString( Object ob , int scale ) {
		Float fl ;
		float f ;
		
		try {
			if ( ob instanceof Float ) {
				fl = ( Float ) ob ;
				return toString( fl.floatValue() , scale ) ;
			} 
			
			if ( ob == null ) {
				return toString( 0.0F , scale ) ;
			} 
			
			f = Float.parseFloat( ob.toString() ) ;
			return toString( f , scale ) ;
			
		} catch( Exception ex ) {
			return toString( 0.0F , scale ) ;
		}
		
	}


	/**
	 * Converts a float value to a String with the specified scale, as a percentage.
	 * @param f The float to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a String representation of the float with the specified scale, as a
	 * percentage. For example, if "0.9245" is the float and the scale is 2, then the return
	 * value would be "92.45%".
	 * The method rounds as needed.
	 * This method will not throw an exception.
	**/
	public static String toStringAsPct( float f , int scale ) {
		scale = getValidScale( scale ) ;
		BigDecimal bd = toBigDecimal( f , scale + 2 ) , bd100 = new BigDecimal( "100" ) ;
		BigDecimal bd2 = bd.multiply( bd100 ) ;
		BigDecimal bd3 = bd2.setScale( scale , RoundingMode.HALF_UP ) ;
		
		return bd3.toString() + "%" ;
	}

	/**
	 * Converts an Object value to a String with the specified scale, as a percentage.
	 * @param ob The object to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a String representation of the float with the specified scale, as a
	 * percentage. For example, if "0.9245" is the float and the scale is 2, then the return
	 * value would be "92.45%".<BR>
	 * If the object is a float, then it converts the value as a float. If the object is
	 * null, then a zero value is converted, with the specified scale. For any other object,
	 * the value is first converted to a String, and then the String will be converted to a 
	 * float, if possible and formatted with the specified scale, or 0.0 will be formatted, if the
	 * String is not a valid numeric.
	 * The method rounds as needed.
	 * This method will not throw an exception.
	**/
	public static String toStringAsPct( Object ob , int scale ) {
		Float fl ; 
		float f ; 
		
		try {	
			scale = getValidScale( scale ) ;
			
			if ( ob instanceof Float ) {
				fl = ( Float ) ob ;
				return toStringAsPct( fl.floatValue() , scale ) ;
			} 
		
			if ( ob == null ) {
				return toString( 0.0F , scale ) + "%" ;
			} 
			
			f = Float.parseFloat( ob.toString() ) ;
			return toStringAsPct( f , scale ) ;
			
		} catch( Exception ex ) {
			return toString( 0.0F , scale ) + "%" ;
		}
	}

	/**
	 * Compares two floats and returns whether they are equal when rounded to the specified scale.
	 * @param f1 The first float to be evaluated.
	 * @param f2 The first float to be evaluated.
	 * @param scale The desired scale.
	 * @return Returns a boolean true if the floats are equal when rounded to the same scale,
	 * or false if they are not.
	 * The method rounds as needed.
	 * This method will not throw an exception.
	**/
	public static boolean isEqual( float f1 , float f2 , int scale ) {
		String s1 = toString( f1 , scale ) ;
		String s2 = toString( f2 , scale ) ;
		return s1.equals( s2 ) ;
	}
	
} // end of FloatConversion class


