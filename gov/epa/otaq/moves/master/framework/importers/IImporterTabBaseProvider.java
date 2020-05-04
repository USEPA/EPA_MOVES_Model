/**************************************************************************************************
 * @(#)IImporterTabBaseProvider.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.util.*;
import javax.swing.JPanel;

/**
 * Interface for objects providing information to ImporterTabBase objects.
 * 
 * @author		Wesley Faler
 * @version		2013-09-03
**/
public interface IImporterTabBaseProvider {
	/**
	 * Get the description of the importer
	 * @return the description of the importer
	**/
	String getDescription();

	/**
	 * Set the description of the importer
	 * @param descriptionToUse the description of the importer
	**/
	void setDescription(String descriptionToUse);

	/**
	 * Get the list of messages about the imported data
	 * @return an ArrayList holding String objects
	**/
	ArrayList<String> getMessages();

	/** Clear the list of messages about the imported data. **/
	void clearMessages();

	/**
	 * Get the set of IImporterPart objects that allow editing importer-specific details.
	 * @return an ArrayList holding IImporterPart objects.  null or empty are nonsensical
	 * but allowed.
	**/
	ArrayList<IImporterPart> getParts();

	/**
	 * Determine if exporting data from the default database is allowed.
	 * @return true if data from the default database can be exported
	**/
	boolean allowDefaultDataExport();

	/**
	 * Determine if exporting data from MOVESExecution is allowed.
	 * @return true if data from MOVESExecution can be exported
	**/
	boolean allowExecutionDataExport();

	/**
	 * Determine if data export restrictions may be applied.
	 * @return true if data from MOVESExecution or MOVES default should be regulated
	 * according to the CompilationFlags.ENABLE_EXPORT_DEFAULT_DATA
	**/
	boolean isSubjectToExportRestrictions();

	/**
	 * Determine if exporting custom data from the default database is allowed.
	 * @return true if data from the default database can be exported
	**/
	boolean allowCustomDomainDefaultDataExport();

	/**
	 * Obtain the name, if any, of a custom button on the GUI.
	 * @return non-null if a custom button is to be offered.
	**/
	String getCustomButtonName();

	/**
	 * Process a click on a custom button.
	 * @param name identifies the custom button.
	 * @param guiOwner window that is showing the custom button
	**/	
	void onCustomButton(String name, JPanel guiOwner);
}
