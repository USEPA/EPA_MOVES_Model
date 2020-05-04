/**
 * Combobox subclass, capable of displaying text in preferred size.
 * Source code ref : http://forum.java.sun.com/thread.jsp?forum=57&thread=121540
 * @author 		Tim Hull
 * @author      Wesley Faler
 * @version		2011-09-10
**/
package gov.epa.otaq.moves.common;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.plaf.metal.MetalComboBoxUI;
import javax.swing.plaf.basic.BasicComboBoxRenderer;
import javax.swing.plaf.basic.*;

public class ExtendedComboBox<T> extends JComboBox<T> {
	// Width of the popup
	protected int popupWidth = 0;

	// Flag used to determine if the actions was performed by the user
	boolean isUserAction = false;

	// Flag used to determine if an ActionEvent is already being fired
	boolean firingActionEvent = false;


	/**
	 * Constructor which sets the ExtendedComboBox in the super class
	**/
	public ExtendedComboBox() {
		setUI(new ExtendedComboBoxUI());
	}

	/**
	 * Fires action events.
	**/
	protected void fireActionEvent() {

		// Fire the action event if it is a user action
		if (isUserAction && !firingActionEvent) {
			firingActionEvent = true;
			super.fireActionEvent();
			firingActionEvent = false;
		}
		// Clear user action flag if we don't have focus
		if(!isFocusOwner() && !getEditor().getEditorComponent().isFocusOwner())
			isUserAction = false;
	}

	/**
	 * Sets the pop up width
	 * @param width width of the popup
	**/
	public void setPopupWidth(int width) {
		popupWidth = width;
	}

	/**
	 * Gets the size of the pop up
	 * @return Dimension width and height
	**/
	public Dimension getPopupSize() {
		Dimension size = getSize();
		if(popupWidth < 1) {
			popupWidth = size.width;
		}
		return new Dimension(popupWidth, size.height);
	}

	public class ExtendedComboBoxUI extends MetalComboBoxUI {
		/**
		 * Creates the focus listener
		 * @return FocusListener, focus listener for combobox.
		**/
		protected FocusListener createFocusListener() {
			return new FocusHandler() {
				/**
				 * Handles the focus lost event for the combobox.
				 * @param	e The FocusEvent to be handled.
				**/
				public void focusLost(FocusEvent e) {
					super.focusLost(e);
					// Clear user action flag if we haven't already
					isUserAction = false;
				}

				/**
				 * Handles the focus gained event for the combobox.
				 * @param e event that gave the focus.
				**/
				public void focusGained(FocusEvent e) {
					// Set user action flag
					isUserAction = true;
					super.focusGained(e);
				}

			};
		}

		/**
		 * Creates the combobox
		 * @return ComboPopUp, combobox after setting the preferred size.
		**/
		protected ComboPopup createPopup() {
			BasicComboPopup popup = new BasicComboPopup(comboBox) {
				/**
				 * Show the extended combobox
				**/
				public void show() {
					Dimension popupSize = ((ExtendedComboBox) comboBox).getPopupSize();
					popupSize.setSize(popupSize.width,
							getPopupHeightForRowCount(comboBox.getMaximumRowCount()));
					Rectangle popupBounds = computePopupBounds(0,
						comboBox.getBounds().height, popupSize.width, popupSize.height);
					scroller.setMaximumSize(popupBounds.getSize());
					scroller.setPreferredSize(popupBounds.getSize());
					scroller.setMinimumSize(popupBounds.getSize());
					list.invalidate();
					int selectedIndex = comboBox.getSelectedIndex();
					if (selectedIndex == -1) {
						list.clearSelection();
					} else {
						list.setSelectedIndex(selectedIndex);
					}
					list.ensureIndexIsVisible(list.getSelectedIndex());
					setLightWeightPopupEnabled(comboBox.isLightWeightPopupEnabled());
					show(comboBox, popupBounds.x, popupBounds.y);
				}
			};
			popup.getAccessibleContext().setAccessibleParent(comboBox);
			return popup;
		}
	}
}
