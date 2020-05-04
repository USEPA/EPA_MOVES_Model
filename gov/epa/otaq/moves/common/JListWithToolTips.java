/**************************************************************************************************
 * @(#)JListWithToolTips.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.Vector;

/**
 * Class like JList but that automatically provides ToolTip support.
 *
 * @author		Wesley Faler
 * @version		2011-09-10
**/
public class JListWithToolTips<T> extends JList<T> {
	/** common point used for locations where no tooltip is needed **/
	static Point zeroPoint = new Point(0,0);

	/** Constructs a JList with an empty model. **/
	public JListWithToolTips() {
		ToolTipManager.sharedInstance().registerComponent(this);
	}

	/**
	 * Constructs a JList that displays the elements in the specified, non-null model.
	 * @param dataModel the data model for this list
	**/
	public JListWithToolTips(ListModel<T> dataModel) {
		super(dataModel);
		ToolTipManager.sharedInstance().registerComponent(this);
	}

	/**
	 * Constructs a JList that displays the elements in the specified array.
	 * @param listData the array of Objects to be loaded into the data model
	**/
	public JListWithToolTips(T[] listData) {
		super(listData);
		ToolTipManager.sharedInstance().registerComponent(this);
	}

	/**
	 * Constructs a JList that displays the elements in the specified Vector.
	 * @param listData the Vector to be loaded into the data model
	**/
	public JListWithToolTips(Vector<T> listData) {
		super(listData);
		ToolTipManager.sharedInstance().registerComponent(this);
	}

	/**
	 * Obtain the tooltip text for the data under the mouse.
	 * @param e mouse location
	 * @return tooltip text from toString() of Object under the mouse
	**/
	public String getToolTipText(MouseEvent e) {
		int row = locationToIndex(e.getPoint());
		if(row >= 0 && row < getModel().getSize()) {
			T item = getModel().getElementAt(row);
			if(item != null) {
				return item.toString();
			}
		}
		return "";
	}

	/**
	 * Obtain the preferred location to display tooltip text
	 * @param e mouse location
	 * @return starting location on screen to display tooltip text
	**/
	public Point getToolTipLocation(MouseEvent e) {
		int row = locationToIndex(e.getPoint());
		if(row >= 0 && row < getModel().getSize()) {
			return indexToLocation(row);
		} else {
			return zeroPoint;
		}
	}
}
