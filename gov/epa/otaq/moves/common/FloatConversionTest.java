/**************************************************************************************************
 * @(#)FloatConversionTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.io.*;
import junit.framework.*;
import java.util.*;
import java.math.*;
import java.text.*;

/**
 * Test Case for the FloatConversion class
 *
 * @author		Cimulus
 * @version		2004-05-03
**/
public class FloatConversionTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public FloatConversionTest(String name) {
		super(name);

		System.out.println( "FloatConversionTest Constructor" ) ;
	}
	
	/**
	 * Tests the FloatConversion routines
	**/
	public void testFloatConversion() {
		float f = 0.0F ;
		String s = "" ;
		String expected = "" , message ;
		int scale , x ;
		boolean debug = true ;
		
		
		System.out.println( "Running Float Tests" ) ;
		
		scale = 6 ;
		f = 63.4F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "63.400000" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 6.34F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "6.340000" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.634F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.634000" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.0634F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.063400" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.00634F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.006340" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.000634F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.000634" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.0000634F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.000063" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;


		f = 0.00000634F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.000006" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.0000002F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.000000" ;
		if ( debug == true ) System.out.println( message ) ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.00000049F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.000000" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.0000005F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.000001" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.00000051F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "0.000001" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;
				
		scale = 2 ;

		f = 87.645F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "87.65" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 87.6499999F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "87.65" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;


		f = 87.6449F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "87.64" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 87.64499F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "87.64" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;


		f = 87.65000F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "87.65" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;
		
		f = 87.4F ;
		s = FloatConversion.toString( f , scale ) ; 
		expected = "87.40" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;
		
		f = 63.4F;
		expected = "63" ;
		for ( x = -2 ; x <= 10 ; x++ ) {
			scale = x ;
			s = FloatConversion.toString( f , scale ) ;
			if ( x <= 0 ) {
				expected = "63" ;
			}
			if ( x == 1 ) {
				expected = expected + ".4" ;
			}
			if ( x > 1 && x <= 6 ) {
				expected = expected + "0" ;
			}
			message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
			if ( debug == true ) System.out.println( message ) ;
			assertTrue( "Failure: " + message , s.equals( expected ) ) ;
			
		}
	}


	/**
	 * Tests the FloatConversion Percentage routines
	**/
	public void testFloatConversionPct() {
		float f = 0.0F ;
		String s = "" ;
		String expected = "" , message ;
		int scale , x ;
		boolean debug = true ;
		
		
		System.out.println( "Running Float Tests" ) ;
		
		scale = 4 ;
		f = 63.4F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "6340.0000%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 6.34F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "634.0000%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.634F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "63.4000%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.0634F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "6.3400%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.00634F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "0.6340%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.000634F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "0.0634%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.0000634F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "0.0063%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;


		f = 0.00000634F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "0.0006%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.0000002F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "0.0000%" ;
		if ( debug == true ) System.out.println( message ) ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.00000049F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "0.0000%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.0000005F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "0.0001%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.00000051F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "0.0001%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;
				
		scale = 2 ;

		f = 0.8764999F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "87.65%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.876449F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "87.64%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;

		f = 0.8764499F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "87.64%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;
		
		
		f = 0.8765000F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "87.65%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;
		
		f = 0.874F ;
		s = FloatConversion.toStringAsPct( f , scale ) ; 
		expected = "87.40%" ;
		message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected ;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , s.equals( expected ) ) ;
		
		f = 0.634F;
		expected = "63" ;
		for ( x = -2 ; x <= 10 ; x++ ) {
			scale = x ;
			s = FloatConversion.toStringAsPct( f , scale ) ;
			if ( x <= 0 ) {
				expected = "63" ;
			}
			if ( x == 1 ) {
				expected = expected + ".4" ;
			}
			if ( x > 1 && x <= 6 ) {
				expected = expected + "0" ;
			}
			message = "f=" + f + " scale=" + scale + " ; return=" + s + " ; expected=" + expected + "%" ;
			if ( debug == true ) System.out.println( message ) ;
			assertTrue( "Failure: " + message , s.equals( expected + "%" ) ) ;
			
		}
	}


	/**
	 * Tests the FloatConversion Percentage routines
	**/
	public void testFloatScale() {
		int x , y , z ;
		String message ;
		
		for ( x = -1 ; x <= 10 ; x++ ) {
			y = FloatConversion.getValidScale( x ) ;
			z = x < 0 ? 0 : x ;
			z = z > 6 ? 6 : z ;
			
			message = "x = " + x + "  Return: " + y + " Expected: " + z ;
			assertTrue( "Failure on FloatScale: " + message , y == z ) ;
		}
	}
	
	/**
	 * Tests the FloatConversion String routines
	**/
	public void testFloatConversionString() {
		float f = 0.0F ;
		String s = "" , floatString , converted ;
		String expected = "" , message ;
		int scale , x ;
		boolean debug = true ;
				
		System.out.println( "Running Float String Tests" ) ;
		
		scale = 4 ;

		s = "63.400000" ;
		f = FloatConversion.toFloat( s , scale ) ; 
		expected = "63.4" ;
		floatString = f + "" ;
		converted = FloatConversion.toString( f , scale ) ;
		message = "s=" + s + " scale=" + scale + " ; return=" + f + " ; expected=" + expected + " ; converted=" + converted;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , floatString.equals( expected ) ) ;

		s = "63.12344" ;
		f = FloatConversion.toFloat( s , scale ) ; 
		expected = "63.1234" ;
		floatString = f + "" ;
		converted = FloatConversion.toString( f , scale ) ;
		message = "s=" + s + " scale=" + scale + " ; return=" + f + " ; expected=" + expected + " ; converted=" + converted;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , floatString.equals( expected ) ) ;


		s = "63.12345" ;
		f = FloatConversion.toFloat( s , scale ) ; 
		expected = "63.1235" ;
		floatString = f + "" ;
		converted = FloatConversion.toString( f , scale ) ;
		message = "s=" + s + " scale=" + scale + " ; return=" + f + " ; expected=" + expected + " ; converted=" + converted;
		if ( debug == true ) System.out.println( message ) ;
		assertTrue( "Failure: " + message , floatString.equals( expected ) ) ;
		
	}
	
	
} // end of FloatConversionTest class

