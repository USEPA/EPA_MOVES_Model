/**
 * TooltipComboBoxRenderer class to show tooltip for the items in the comboBox.
 * Source code reference : http://forum.java.sun.com/thread.jsp?forum=57&thread=195961
**/
package gov.epa.otaq.moves.common;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.plaf.metal.MetalComboBoxUI;
import javax.swing.plaf.basic.BasicComboBoxRenderer;
import javax.swing.plaf.basic.*;

//public class TooltipComboBoxRenderer<T> extends BasicComboBoxRenderer<T> {

public class TooltipComboBoxRenderer<T> extends JLabel implements ListCellRenderer<T> {
	String[] _tipList = null;

	public TooltipComboBoxRenderer(String[] tipList){
		super();
		_tipList = tipList;
	}

	/**
	 * Get the component which sets the tool tip for the highlighted item in the
	 * combo box.
	 * @param list contains the items
	 * @param value value of the item
	 * @param index 0-based index for component to find
	 * @param isSelected true if the cell is selected
	 * @param cellHasFocus true if the cell has the input focus
	 * @return Component after setting the tooltip
	**/
	public Component getListCellRendererComponent( JList<? extends T> list,T value,
			int index, boolean isSelected, boolean cellHasFocus) {
		if (isSelected) {
			setBackground(list.getSelectionBackground());
			setForeground(list.getSelectionForeground());
			if (index >= 0 && index < _tipList.length) {
				list.setToolTipText(_tipList[index]);
			} else if(index >= _tipList.length) {
				list.setToolTipText(value.toString());
			}
		} else {
			setBackground(list.getBackground());
			setForeground(list.getForeground());
		}
		setFont(list.getFont());
		setText((value == null) ? "" : value.toString());
		return this;
	}
}
