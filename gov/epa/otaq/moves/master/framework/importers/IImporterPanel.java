/**************************************************************************************************
 * @(#)IImporterPanel.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import javax.swing.*;

/**
 * Interface for all GUI panels shown by importers.
 * 
 * @author		wgfaler
 * @version		2009-12-10
**/
public interface IImporterPanel {
	/**
	 * Get the actual JPanel to be shown
	 * @return the JPanel that reprents this importer
	**/
	JPanel getPanel();

	/**
	 * Get the IImporter that owns the panel.
	 * @return the IImporter that owns the panel
	**/
	IImporter getImporter();

	/**
	 * Get the text that titles the panel.
	 * @return the text that titles the panel
	**/
	String getTitle();

	/** Remove all entries from the displayed log **/
	void clearLogDisplay();
}
