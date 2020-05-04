/**************************************************************************************************
 * @(#)PermutationCreator.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.*;

/**
 * Iterate through all permutations of the elements in one or more dimensions.
 *
 * @author		Wesley Faler
 * @version		2010-09-25
**/
public class PermutationCreator {
	/** Hold elements in a single dimension **/
	public interface IDimension {
		int dimensionSize();
		void applyIndex(int index);
	}

	/** All dimensions **/
	ArrayList<IDimension> dimensions = new ArrayList<IDimension>();
	/** Counters used during dimension iteration **/
	int[] counters;

	/**
	 * Add a dimension.
	 * @param d dimension to be added
	**/
	public void add(IDimension d) {
		dimensions.add(d);
	}

	/** Set iteration to the starting item **/
	public void start() {
		counters = new int[dimensions.size()];
		apply();
	}

	/**
	 * Advance to the next permutation.
	 * @return false if there was no next permutation.
	**/
	public boolean next() {
		int index = 0;
		while(true) {
			counters[index]++;
			if(counters[index] >= dimensions.get(index).dimensionSize()) {
				counters[index] = 0;
				index++;
				if(index >= counters.length) {
					return false;
				}
				continue;
			}
			apply();
			return true;
		}
	}

	/** Set each dimension according to counters **/
	void apply() {
		for(int i=0;i<counters.length;i++) {
			dimensions.get(i).applyIndex(counters[i]);
		}
	}
}
