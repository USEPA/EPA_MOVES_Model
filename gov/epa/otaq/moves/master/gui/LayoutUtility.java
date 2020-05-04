/**************************************************************************************************
 * @(#)LayoutUtility.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 * Class for MOVES LayoutUtility.
 *
 * @author		Cimulus
 * @version		2004-03-03
**/
public class LayoutUtility {
	/**
	 * Static function, sets the position on the specified GridBagConstraints.
	 * This is a utility function to conveniently use the GridBagConstraints
	 * class of Java, It sets the gridx, gridy, gidheight, gridwidth, and the
	 * anchor of the GridBagConstraints to the parameters passed in by the 
	 * calling routine. The anchor fiels in the GridBagConstraints is used 
	 * when the component is smaller than the display area. For more information
	 * about the GridBagConstraints refer to the Java documentation.
	 *
	 * @param	gbc the GridBagConstraints to be set.
	 * @param	gridXPos int x position.
	 * @param	gridYPos int y position.
	 * @param	anchorPos String anchor position.  One of "NORTH", "SOUTH", "EAST",
	 * "WEST", or "CENTER".
	 * @param	cellsWide int number of cells wide.
	 * @param	cellsTall int number of cells tall.
	**/
	public static void setPositionOnGrid (GridBagConstraints gbc, int gridXPos, 
			int gridYPos, String anchorPos, int cellsWide, int cellsTall) {
		gbc.gridx = gridXPos;
		gbc.gridy = gridYPos;
		gbc.gridheight = cellsTall;
		gbc.gridwidth = cellsWide;
		if (anchorPos.equals("NORTH")) {
			gbc.anchor = GridBagConstraints.NORTH;
		} else if (anchorPos.equals("SOUTH")) {
			gbc.anchor = GridBagConstraints.SOUTH;
		} else if (anchorPos.equals("EAST")) {
			gbc.anchor = GridBagConstraints.EAST;
		} else if (anchorPos.equals("WEST")) {
			gbc.anchor = GridBagConstraints.WEST;
		} else if (anchorPos.equals("CENTER")) {
			gbc.anchor = GridBagConstraints.CENTER;
		}
	}
}
