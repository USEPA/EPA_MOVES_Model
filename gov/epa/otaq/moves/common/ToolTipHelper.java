/**************************************************************************************************
 * @(#)ToolTipHelper.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Utility class for setting ToolTip support.
 *
 * @author		Cimulus
 * @version		2004-07-15
**/
public class ToolTipHelper {
	/**
	 * Setup tooltip handling for a component.
	 * @param component affected Swing item
	 * @param toolTipText text to display for the item.  null or "" if no tooltip should show
	**/
	public static void add(JComponent component,String toolTipText) {
		if(component == null) {
			return;
		}
		if(toolTipText == null || toolTipText.length() <= 0) {
			ToolTipManager.sharedInstance().unregisterComponent(component);
		} else {
			component.setToolTipText(toolTipText);
			ToolTipManager.sharedInstance().registerComponent(component);
		}
	}
}
