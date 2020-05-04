/**************************************************************************************************
 * @(#)IImportExportHandler.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.awt.*;
import javax.swing.*;

/**
 * This interface is implemented by InternalControlStrategy objects that wish to perform
 * their own import and export operations.
 *
 * @author		Wesley Faler
 * @version		2007-10-07
**/
public interface IImportExportHandler {
	/**
	 * Process a user's request to initiate an import operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was imported.
	 * false if the import request should be handled by default logic instead.
	**/
	boolean doImport(Frame ownerWindow);

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	boolean doExport(Frame ownerWindow);
}
