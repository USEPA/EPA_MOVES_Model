/**************************************************************************************************
 * @(#)TreeMapIgnoreCase.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.util.TreeMap;

/**
 * Version of TreeMap class specific to Strings as keys and without case sensitivity.
 *
 * @author		Wesley Faler
 * @version		2009-03-29
**/
public class TreeMapIgnoreCase extends TreeMap<String,Object> {
	public TreeMapIgnoreCase() {
		super(new StringComparatorIgnoreCase());
	}
}
