/**************************************************************************************************
 * @(#)WorksheetChooserDialog.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.text.*;
import java.util.*;
import java.io.*;
import javax.swing.table.*;
import javax.swing.event.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.gui.*;

/**
 * Select a worksheet within an XLS file
 *
 * @author		Wesley Faler
 * @author		Mike Kender (Task 2003)
 * @version 	2020-08-13
**/
public class WorksheetChooserDialog extends JDialog implements ActionListener, ListSelectionListener {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** report results, including header, separator, and records **/
	JList<String> list;
	/** data container for list **/
	DefaultListModel<String> listModel;
	/** the OK button **/
	JButton okButton;
	/** the Cancel button **/
	JButton cancelButton;

	ArrayList worksheets;
	public String selectedWorksheetName = null;

	/**
	 * Constructs the main panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	 * @param worksheetsToUse list of worksheet name String objects in the order they appear
	 * within the workbook
	**/
	public WorksheetChooserDialog(JFrame parent, ArrayList worksheetsToUse) {
		super(parent, MOVESWindow.MOVES_VERSION + " - Choose XLS Worksheet");
		frame = parent;
		worksheets = worksheetsToUse;

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(true);
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		if(!populateControls()) {
			return;
		}
		//setSize(250,250);
		pack();
		setModal(true);
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(250,250);
		setVisible(true); //show();
	}

	/**
	 * Fill the list of IMCoverage records
	 * @return true if the list was filled and should be shown
	**/
	boolean populateControls() {
		// Clear the list
		listModel.clear();
		// Load
		for(Iterator i=worksheets.iterator();i.hasNext();) {
			String t = (String)i.next();
			listModel.addElement(t);
		}
		// Enable/Disable controls as needed
		assessSituation();
		return true;
	}

	/**
	 * Creates and arranges all dialog controls.
	 * @return the container as JPanel.
	**/
	JPanel createPanel() {
		createControls();
		return arrangeControls();
	}

	/** Creates and initializes all controls on this panel. **/
	void createControls() {
		listModel = new DefaultListModel<String>();
		list = new JList<String>(listModel);
		list.addListSelectionListener(this);

		okButton = new JButton("OK");
		okButton.addActionListener(this);
		ToolTipHelper.add(okButton, "Submit the dialog");

		cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(this);
		ToolTipHelper.add(cancelButton, "Close the dialog");
	}

	/**
	 * Sets the layout of the controls.
	 * @return the container as JPanel of the controls
	**/
	JPanel arrangeControls() {
		JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result,BoxLayout.Y_AXIS));

		Box b = Box.createHorizontalBox();
		b.add(new JLabel("Select the Worksheet to read:"));
		b.add(Box.createHorizontalGlue());
		result.add(b);

		JScrollPane s = new JScrollPane(list);
		s.setHorizontalScrollBar(new JScrollBar(JScrollBar.HORIZONTAL));
		s.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		s.setVerticalScrollBar(new JScrollBar(JScrollBar.VERTICAL));
		s.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		result.add(s);

		// Add buttons
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel,BoxLayout.X_AXIS));
		buttonPanel.add(okButton);
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(cancelButton);

		result.add(buttonPanel);

		return result;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == okButton) {
			handleOKButton();
		} else if(e.getSource() == cancelButton) {
			dispose();
		}
	}

	/** Process a click on the OK button **/
	public void handleOKButton() {
		java.util.List<String> selectedRecords = list.getSelectedValuesList();
		if(selectedRecords == null || selectedRecords.isEmpty()) {
			return;
		}

		selectedWorksheetName = selectedRecords.get(0);

		if(selectedWorksheetName != null && selectedWorksheetName.length() > 0) {
			dispose();
		}
	}

	/**
	 * Called when the selection in the list changes.
	 * @param e the ListSelectionEvent to be handled
	**/
	public void valueChanged(ListSelectionEvent e) {
		assessSituation();
	}

	/** Examine the selected items and enable/disable buttons accordingly **/
	void assessSituation() {
		boolean okButtonStatus = false;

		int index = list.getSelectedIndex();
		if(index >= 0 && index < worksheets.size()) {
			okButtonStatus = true;
		}

		okButton.setEnabled(okButtonStatus);
	}
}
