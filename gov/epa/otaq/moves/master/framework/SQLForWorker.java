/**************************************************************************************************
 * @(#)SQLForWorker.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.LinkedList;

/**
 * Contains the SQL that is needed to provide work to a distributed worker.
 *
 * @author		Wesley Faler
 * @version		2015-02-08
**/
public class SQLForWorker {
	/**
	 * Contains a list of String objects that are SQL statements that export the data that will
	 * be needed by the worker.
	**/
	public LinkedList<String> dataExportSQL = new LinkedList<String>();

	/**
	 * Contains a list of String objects that are SQL statements that remove generated data
	 * from the Master's database.  That is, once data has been extracted and sent to workers
	 * for remote consideration, it can be removed from the local Master's database.
	 * Calculators should add their SQL to the end of this list.
	**/
	public LinkedList<String> localDataRemovalSQL = new LinkedList<String>();

	/**
	 * Contains a list of String objects that are SQL statements that the worker will need to
	 * execute.  Calculators should add their SQL to the end of this list.
	**/
	public LinkedList<String> processingSQL = new LinkedList<String>();

	/**
	 * Contains a list of String objects that are SQL statements that the worker will need to
	 * execute in order to cleanup any intermediate calculation tables.  Calculators should add
	 * their SQL to the beginning of this list.
	**/
	public LinkedList<String> cleanupSQL = new LinkedList<String>();

	/**
	 * Contains a list of String objects that are module names and flags to be passed
	 * to an external calculator.
	**/
	public LinkedList<String> externalModules = new LinkedList<String>();

	/**
	 * Default constructor
	**/
	public SQLForWorker() {
	}
}
