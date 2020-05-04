/**************************************************************************************************
 * @(#)LoginDialog.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.sql.*;
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
import gov.epa.otaq.moves.master.framework.SystemConfiguration;
import gov.epa.otaq.moves.master.gui.*;

/**
 * Set user name and password for database login.
 *
 * @author		Daniel Cox
 * @author		Wesley Faler
 * @version		2014-06-27
**/
public class LoginDialog extends JDialog implements ActionListener {
	/**
	 * Attempt connecting to the database, prompting the user for
	 * login credentials when a connection cannot be established.
	 * @return true when a connection is established
	**/
	public static boolean checkDatabaseLogin() {
		boolean shouldUpdateConfiguration = false;
		while(true) {
			Connection db = DatabaseSelection.openKnownWorkingConnection();
			if(db != null) {
				DatabaseUtilities.closeConnection(db);
				if(shouldUpdateConfiguration) {
					try {
						SystemConfiguration.getTheSystemConfiguration().saveConfigurationData();
					} catch(Exception e) {
						Logger.logError(e,"Unable to update system configuration files.");
					}
				}
				return true;
			}
			shouldUpdateConfiguration = true;
			Logger.log(LogMessageCategory.INFO,"Prompting for database login...");
			LoginDialog d = new LoginDialog(null);
			d.showModal();
			if(!d.clickedOK) {
				return false;
			}
		}
	}

	/** true if the dialog closed by clicking OK **/
	public boolean clickedOK = false;

	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** the OK button **/
	JButton okButton;
	/** the Cancel button **/
	JButton cancelButton;

	/** Image icon. **/
	ImageIcon image;
	/** Image label. **/
	JLabel imageLabel;

	/** User label **/
	JLabel labelUser;
	/** Password label **/
	JLabel labelPassword;
	/** User name **/
	JTextField userName;
	/** Password **/
	JPasswordField password;

	/**
	 * Constructs the main panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	**/
	public LoginDialog(JFrame parent) {
		super(parent, "Set Database Login");
		frame = parent;

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(true);
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		clickedOK = false;
		if(!populateControls()) {
			return;
		}
		pack();
		setModal(true);
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(550,260);
		setVisible(true); //show();
	}

	/**
	 * Place current values into the GUI.
	 * @return true if the list was filled and should be shown
	**/
	boolean populateControls() {
		userName.setText(DatabaseSelection.userProvidedUserName);
		password.setText(DatabaseSelection.userProvidedPassword);
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
		okButton = new JButton("Login");
		okButton.addActionListener(this);
		cancelButton = new JButton("Quit MOVES");
		cancelButton.addActionListener(this);

		labelUser = new JLabel("User:");
		labelUser.setName("labelUser");

		userName = new JTextField();
		userName.setName("userName");
		userName.setColumns(20);

		labelPassword = new JLabel("Password:");
		labelPassword.setName("labelPassword");

		password = new JPasswordField();
		password.setName("password");
		password.setColumns(20);

		image = new ImageIcon("gov/epa/otaq/moves/master/gui/images/moves_logo_480.png");
		imageLabel = new JLabel(image);
		imageLabel.setName("imageLabel");
	}

	/**
	 * Sets the layout of the controls.
	 * @return the container as JPanel of the controls
	**/
	JPanel arrangeControls() {
		/*
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;

		JPanel result = new JPanel();
		result.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 3;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0, 0, "NORTH", 5, 1);
		result.add(imageLabel, gbc);

		LayoutUtility.setPositionOnGrid(gbc,1,1, "EAST", 1, 1);
		result.add(labelUser, gbc);
		LayoutUtility.setPositionOnGrid(gbc,2,1, "WEST", 2, 1);
		result.add(userName, gbc);

		LayoutUtility.setPositionOnGrid(gbc,1,2, "WEST", 1, 1);
		result.add(labelPassword, gbc);
		LayoutUtility.setPositionOnGrid(gbc,2,2, "WEST", 2, 1);
		result.add(password, gbc);

		LayoutUtility.setPositionOnGrid(gbc,1,3, "WEST", 1, 1);
		result.add(okButton, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,3, "WEST", 1, 1);
		result.add(cancelButton, gbc);

		return result;
		*/

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;

		JPanel core = new JPanel();
		core.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 3;
		gbc.gridheight = 3;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0,1, "EAST", 1, 1);
		core.add(labelUser, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 2, 1);
		core.add(userName, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0,2, "WEST", 1, 1);
		core.add(labelPassword, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,2, "WEST", 2, 1);
		core.add(password, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0,3, "WEST", 1, 1);
		core.add(okButton, gbc);
		LayoutUtility.setPositionOnGrid(gbc,2,3, "WEST", 1, 1);
		core.add(cancelButton, gbc);

		JPanel coreHorizontal = new JPanel();
		coreHorizontal.setLayout(new BoxLayout(coreHorizontal, BoxLayout.X_AXIS));
		coreHorizontal.add(Box.createHorizontalGlue());
		coreHorizontal.add(core);
		coreHorizontal.add(Box.createHorizontalGlue());

		JPanel imageHorizontal = new JPanel();
		imageHorizontal.setLayout(new BoxLayout(imageHorizontal, BoxLayout.X_AXIS));
		imageHorizontal.add(Box.createHorizontalGlue());
		imageHorizontal.add(imageLabel);
		imageHorizontal.add(Box.createHorizontalGlue());

		JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
		result.add(imageHorizontal);
		result.add(coreHorizontal);
		result.add(Box.createVerticalGlue());
		
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
		DatabaseSelection.userProvidedUserName = userName.getText();
		DatabaseSelection.userProvidedPassword = new String(password.getPassword());
		clickedOK = true;
		dispose();
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
		boolean okButtonStatus = true;
		// Nothing to do here
		okButton.setEnabled(okButtonStatus);
	}
}
