/**************************************************************************************************
 * @(#)IntegratedPostProcessor.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.sql.Connection;

/**
 * The base class for all classes that modify data as it is received from the distributed workers,
 * before it is passed on to the final output processing and data aggregation logic. This class
 * provides two methods: one that is performed each time a work bundle is received and one that is
 * performed only after all the work bundles have been received.
 * 
 * @author		Cimulus
 * @version		2006-11-30
**/
public abstract class IntegratedPostProcessor {
	/**
	 * Default constructor
	**/
	public IntegratedPostProcessor() {
	}

	/**
	 * Process recently imported records in the database. The specifed database will only have
	 * newly imported records and won't have records from previous imports which have already
	 * been moved to the output database.  The data is stored in the TemporaryOutputImport table.
	 * @param database a Connection to a database containing the TemporaryOutputImport table
	**/
	public void execute(Connection database) {
	}

	/**
	 * Process the MOVESOutput table in the output database.
	 * @param outputDatabase a Connection to the current output database
	 * @param executionDatabase a Connection to the current execution database
	 * @param isBeforeUnitConversion true if units have not yet been set to the user
	 * supplied values.  false after units have been converted and other aggregations
	 * performed.
	**/
	public void doFinalPostProcessingOnOutputDatabase(Connection outputDatabase,
			Connection executionDatabase, boolean isBeforeUnitConversion) {
	}
}
