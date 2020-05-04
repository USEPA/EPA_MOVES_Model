/**************************************************************************************************
 * @(#)PermutationCreatorTest.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import junit.framework.*;

/**
 * Test Case for the PermutationCreator class
 *
 * @author		Wesley Faler
 * @version		2010-09-25
**/
public class PermutationCreatorTest extends TestCase {
	/**
	 * Standard TestCase constructor
	 * @param name Name of the test case.
	**/
	public PermutationCreatorTest(String name) {
		super(name);
	}

	class IntegerDimension implements PermutationCreator.IDimension {
		public int size = 1;
		public int currentValue = 0;

		public IntegerDimension(int sizeToUse) {
			size = sizeToUse;
		}

		public int dimensionSize() {
			return size;
		}

		public void applyIndex(int index) {
			currentValue = index;
		}
	}

	/** Implements the test case(s). **/
	public void testCase1() {
		PermutationCreator p = new PermutationCreator();
		IntegerDimension a = new IntegerDimension(3);
		p.add(a);
		IntegerDimension b = new IntegerDimension(2);
		p.add(b);
		runTest(p);
	}

	/** Implements the test case(s). **/
	public void testCase2() {
		PermutationCreator p = new PermutationCreator();
		IntegerDimension a = new IntegerDimension(1);
		p.add(a);
		IntegerDimension b = new IntegerDimension(1);
		p.add(b);
		runTest(p);
	}

	/** Implements the test case(s). **/
	public void testCase3() {
		PermutationCreator p = new PermutationCreator();
		IntegerDimension a = new IntegerDimension(1);
		p.add(a);
		runTest(p);
	}

	/** Implements the test case(s). **/
	public void testCase4() {
		PermutationCreator p = new PermutationCreator();
		IntegerDimension a = new IntegerDimension(3);
		p.add(a);
		IntegerDimension b = new IntegerDimension(2);
		p.add(b);
		IntegerDimension c = new IntegerDimension(4);
		p.add(c);
		runTest(p);
	}

	/**
	 * Ensure all permutations are visited.
	 * @param p PermutatioCreator to be checked
	**/
	void runTest(PermutationCreator p) {
		int totalSize = 1;
		for(int i=0;i<p.dimensions.size();i++) {
			totalSize *= p.dimensions.get(i).dimensionSize();
		}
		boolean[] visited = new boolean[totalSize];
		p.start();
		do {
			markVisited(visited,p);
		} while(p.next());
		for(int i=0;i<visited.length;i++) {
			if(!visited[i]) {
				fail("combination " + i + " was never visited");
			}
		}
	}

	/**
	 * Flag the current visited permutation, failing if a permutation is visited twice.
	 * @param visited array of flags used to track permutation history
	 * @param p permutation creator
	**/
	void markVisited(boolean[] visited, PermutationCreator p) {
		int index = 0;
		int base = 1;
		for(int i=0;i<p.dimensions.size();i++) {
			IntegerDimension d = (IntegerDimension)p.dimensions.get(i);
			index += d.currentValue * base;
			base *= d.dimensionSize();
		}
		assertFalse("combination " + index + " has already been visited",visited[index]);
		visited[index] = true;
	}
}
