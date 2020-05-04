/**************************************************************************************************
 * @(#)IExportDefaultHandler.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.awt.*;
import javax.swing.*;

/**
 * This interface is implemented by InternalControlStrategy objects that wish to provide
 * export operations for default data.
 *
 * @author		Wesley Faler
 * @version		2009-10-31
**/
public interface IExportDefaultHandler {
	/**
	 * Process a user's request to initiate an export default data operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	boolean doExportDefault(Frame ownerWindow);
}
