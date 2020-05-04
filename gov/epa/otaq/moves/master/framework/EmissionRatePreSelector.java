/**************************************************************************************************
 * @(#)EmissionRatePreSelector.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.util.LinkedList;

/**
 * Prune EERDB using RunSpec parameters into temporary data store.
 * EERDB is the Execution Emission Rate Database explained in TotalActivityGenerator
 *
 * @author		Cimulus
 * @version		2004-03-03
**/
public class EmissionRatePreSelector {
	/**
	 * Default constructor
	**/
	public EmissionRatePreSelector() {
	}

	/**
	 * Prunes EERDB data according to parameters specified in an ExecutionRunSpec object and stores
	 * the resulting data in the specified file.
	 * @param filePath The destination file name and path.
	**/
	public void pruneEERDB(String filePath) {
	}
}
