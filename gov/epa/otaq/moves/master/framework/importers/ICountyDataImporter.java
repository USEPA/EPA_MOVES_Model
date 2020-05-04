/**************************************************************************************************
 * @(#)ICountyDataImporter.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.sql.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;

/**
 * Interface for all importer modules that participate in the County Data Manager.
 * 
 * @author		wgfaler
 * @version		2008-10-29
**/
public interface ICountyDataImporter {
	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.  Will be null if merely checking
	 * for whether to show the importer to the user.
	 * @return the status, or null if the importer should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	RunSpecSectionStatus getCountyDataStatus(Connection db) throws Exception;
}
