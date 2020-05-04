/**************************************************************************************************
 * @(#)IDataStatus.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.sql.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.gui.RunSpecSectionStatus;

/**
 * Interface for importers that provide a status display at domains other than County or Project.
 * 
 * @author		Wesley Faler
 * @version		2009-09-06
**/
public interface IDataStatus {
	/**
	 * Check a RunSpec against the database or for display of the importer.
	 * @param db database to be examined.
	 * @return the status, or null if the status should not be shown to the user.
	 * @throws Exception if anything goes wrong
	**/
	RunSpecSectionStatus getImporterDataStatus(Connection db) throws Exception;
}
