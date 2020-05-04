/**************************************************************************************************
 * @(#)TreeSetIgnoreCase.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.TreeSet;

/**
 * Version of TreeSet class specific to Strings as keys and without case sensitivity.
 *
 * @author		Cimulus
 * @version		2004-01-14
**/
public class TreeSetIgnoreCase extends TreeSet<String> {
	public TreeSetIgnoreCase() {
		super(new StringComparatorIgnoreCase());
	}
}
