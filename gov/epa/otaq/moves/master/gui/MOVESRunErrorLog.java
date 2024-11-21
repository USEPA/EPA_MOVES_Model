/**************************************************************************************************
 * @(#)MOVESRunErrorLog.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.sql.*;
import javax.swing.*;
import javax.swing.event.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

/**
 * Class for MOVESRunErrorLog Dialog.
 *
 * @author		Wesley Faler
 * @author		Tim Hull
 * @author  	Bill Shaw (508 compliance mods)
 * @version		2013-08-20
**/
public class MOVESRunErrorLog extends JDialog implements ActionListener {
	/**
	 * Internal structure to associate a database ID with a string for display, used with the
	 * selectRunCombo control.
	**/
	class ComboItem {
		/** Database identifier for Item **/
		long id = 0;
		/** Displayed value for Item **/
		String display = "";

		/** Default constructor **/
		public ComboItem() {
		}

		/**
		 * Constructor to set the database id and displayed value
		 * @param id The database identifier for this item.
		 * @param display The displayed value for the item.
		**/
		public ComboItem(long id, String display) {
			this.id = id;
			this.display = display;
		}

		/** Standard method to convert object to a string representation **/
		public String toString() {
			return "Run: " + id + " - " + display;
		}
	};

	/** The dialog result, indicates true on OK button. **/
	public int result = 0;
	/** OK button. **/
	JButton okButton;
	/** SelectRun panel. **/
	JPanel selectRunPanel;
	/** Select Run label. **/
	JLabel selectRunLabel;
	/** Select Run combobox. **/
	ExtendedComboBox<ComboItem> selectRunCombo;
	/** Message log list. **/
	JList<String> messageLogList;
	/** DefaultListModel for the messageLogList. **/
	DefaultListModel<String> messageLogModel;
	/** JScrollPane for the messageLogList. **/
	JScrollPane messageLogPane;
	/** A shortcut to the MOVESWindow's RunSpec name. **/
	String runSpecName;
	/** The default runID to show errors for, it is an optional setting. **/
	public long runID = 0;

	/**
	 * Constructor
	 * @param parent The parent of this dialog.
	**/
	public MOVESRunErrorLog(JFrame parent) {
		super(parent, MOVESWindow.MOVES_VERSION + " - MOVES Run Error Log");
		File runSpecFile = new File(((MOVESWindow)parent).runSpecFilePath);
		//runSpecName = FileUtilities.getBaseFileName(runSpecFile);
		runSpecName = FileUtilities.safeGetPath(runSpecFile);
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(true);
	}

	/** Data from member variables to dialog controls (currently unused). **/
	void pushDataToControls() {
	}

	/**
	 * Creates and arranges all dialog controls.
	 * @return the JPanel container of the controls
	**/
	JPanel createPanel() {
		createControls();
		return arrangeControls();
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		okButton = new JButton("OK");
		okButton.addActionListener(this);
		okButton.setName("okButton");
		okButton.setMnemonic('O');
		okButton.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(okButton,"Close the dialog");

		selectRunCombo = new ExtendedComboBox<ComboItem>();
		selectRunCombo.setName("selectRunCombo");
		selectRunLabel = new JLabel("Select Run:");
		selectRunLabel.setName("selectRunLabel");
		selectRunCombo.setEditable(false);
		selectRunCombo.addActionListener(this);
		ToolTipHelper.add(selectRunCombo,"Select a run to see the messages logged during the run");
		selectRunPanel = new JPanel();
		selectRunPanel.setName("selectRunPanel");
		selectRunLabel.setDisplayedMnemonic('S');
		selectRunLabel.setLabelFor(selectRunPanel);

		messageLogModel = new DefaultListModel<String>();
		messageLogList = new JListWithToolTips<String>(messageLogModel);
		messageLogList.setName("messageLogList");
		messageLogList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		messageLogList.setSelectedIndex(-1);
		messageLogList.setVisibleRowCount(16);
		messageLogPane = new JScrollPane(messageLogList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		messageLogPane.setName("messageLogPane");
		messageLogPane.setPreferredSize(new Dimension(600, 200));
		ToolTipHelper.add(messageLogPane,"List of messages logged during a run");

		populateSelectRunCombo();
	}

	/**
	 * Sets the layout of the controls.
	 * @return the JPanel container of the controls
	**/
	public JPanel arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;
		selectRunPanel.setLayout(new GridBagLayout());
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		selectRunPanel.add(selectRunLabel, gbc);
		LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
		selectRunPanel.add(selectRunCombo, gbc);

		gbc = new GridBagConstraints();
		JPanel result = new JPanel();
		result.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 2;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		result.add(selectRunPanel, gbc);

		LayoutUtility.setPositionOnGrid(gbc,1,0, "EAST", 1, 1);
		result.add(okButton, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0, 1, "SOUTH", 2, 1);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		result.add(messageLogPane, gbc);

		return result;
	}

	/**
	 * Handles various actions.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == okButton) {
			handleOKButton();
		} else if((e.getSource() == selectRunCombo)
				&& (e.getActionCommand().equals("comboBoxChanged"))) {
			handleSelectRunComboChange();
		}
	}

	/**
	 * Populate error log from movesoutput database.
	**/
	void populateSelectRunCombo() {
		String sql = "";
		Connection outputDB = null;
		try {
			outputDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
		} catch(Exception e) {
			Logger.logError(e,"Unable to get a database connection for loading a list of runs"
					+ " for the MOVES Error Log.");
		}
		try {
			PreparedStatement statement = null;
			// Filter the run list for the current RunSpec
			if(runSpecName.length() > 0) {
				sql = "SELECT MOVESRunID, runDateTime FROM MOVESRun WHERE LOWER(RunSpecFileName) = ? "
						+ "ORDER BY runDateTime DESC";
				statement = outputDB.prepareStatement(sql);
				statement.setString(1, runSpecName);
			} else {
				sql = "SELECT MOVESRunID, runDateTime FROM MOVESRun ORDER BY runDateTime DESC";
				statement = outputDB.prepareStatement(sql);
			}
			ResultSet results = SQLRunner.executeQuery(statement,sql);
			boolean didAdd = false;
			ComboItem nextItem = null;
			int selectedIndex = 0;
			while(results.next()) {
				didAdd = true;
				nextItem = new ComboItem(results.getLong(1), results.getString(2));
				selectRunCombo.addItem(nextItem);
				if(results.getLong(1) == runID) {
					selectedIndex = selectRunCombo.getItemCount() - 1;
				}
			}
			if(!didAdd) {
				selectRunCombo.addItem(new ComboItem() { public String toString() { return "No MOVESRun data found."; }});
			} else if(runID != 0) {
				selectRunCombo.setSelectedIndex(selectedIndex);
			}
			results.close();
			statement.close();
		} catch(SQLException e) {
			Logger.logSqlError(e,"Loading a list of runs for the MOVES Error Log failed.", sql);
		}
		try {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT, outputDB);
		} catch(Exception e) {
			Logger.logError(e,"Could not check in the database connection used to get a list of "
					+ "runs.");
		}
		handleSelectRunComboChange();
	}

	/**
	 * Handles change events on the selectRunCombo, populates the messageLogList from the selected
	 * runID.
	**/
	void handleSelectRunComboChange() {
		Object selectedItem = selectRunCombo.getSelectedItem();
		if(selectedItem instanceof ComboItem) {
			populateMessageLogList(((ComboItem)selectedItem).id);
		}
	}

	/**
	 * Populates the messageLogList from the specified runID.
	 * @param runID The ID of the corresponding MOVESRun data to display errors for.
	**/
	void populateMessageLogList(long runID) {
		String sql = "";
		if(runID == 0) {
			return;
		}
		messageLogModel.removeAllElements();
		Connection outputDB = null;
		try {
			outputDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
		} catch(Exception e) {
			Logger.logError(e,"Unable to checkout the database connection needed to get a list of"
					+ "logged messages.");
		}
		try {
			sql = "SELECT MOVESErrorID, MOVESRunID, linkID, zoneID, countyID, stateID, "
					+ "hourID, dayID, monthID, yearID, pollutantID, processID, ErrorMessage "
					+ "FROM MOVESError WHERE MOVESRunID = ? "
					+ "ORDER BY MOVESErrorID DESC";
			PreparedStatement statement = outputDB.prepareStatement(sql);
			statement.setLong(1, runID);
			ResultSet results = SQLRunner.executeQuery(statement,sql);
			if(!results.next()) {
				messageLogModel.addElement("No MOVESError log entries for RunID = " + runID);
			} else {
				int linkID;
				int zoneID;
				int countyID;
				int stateID;
				int hourID;
				int dayID;
				int monthID;
				int yearID;
				int pollutantID;
				int processID;
				String messageLine = "";
				while(true) {
					messageLine = "";
					linkID = results.getInt(3);
					zoneID = results.getInt(4);
					countyID = results.getInt(5);
					stateID = results.getInt(6);
					hourID = results.getInt(7);
					dayID = results.getInt(8);
					monthID = results.getInt(9);
					yearID = results.getInt(10);
					pollutantID = results.getInt(11);
					processID = results.getInt(12);
					if(pollutantID != 0) {
						messageLine += ("Pollutant:" + pollutantID + " ");
					}
					if(processID != 0) {
						messageLine += ("Process:" + processID + " ");
					}
					if(stateID != 0 && countyID != 0) {
						messageLine += ("State:" + stateID + " County:" + countyID + " ");
					}
					if(linkID != 0 && zoneID != 0) {
						messageLine += (" Link:" + linkID + " Zone:" + zoneID + " ");
					}
					if(monthID != 0 && yearID != 0) {
						messageLine += (", " + yearID + "-" + monthID + " ");
					}
					if(dayID != 0 && hourID != 0) {
						messageLine += (" Day:" + dayID + " Hour:" + hourID + " ");
					}
					messageLine += (" " + results.getString(13));
					messageLine = messageLine.replace('\n',' ');
					messageLine = messageLine.replace('\r',' ');
					messageLine = messageLine.replace('\t',' ');
					messageLogModel.addElement(messageLine);
					if(!results.next()) {
						break;
					}
				}
			}
			results.close();
			statement.close();
		} catch(SQLException e) {
			e.printStackTrace();
			Logger.logError(e,"Unable to add the list of run errors from the database to Run Error"
					+ " Message Log");
		}
		try {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT, outputDB);
		} catch(Exception e) {
			Logger.logError(e,"Unable to check-in the database connection used to get list of run"
					+ " errors");
		}
	}

	/**
	 * OK button handler.
	**/
	void handleOKButton() {
		// indicates OK button
		result = 1;
		dispose();
	}

	/**
	 * Allows the parent to display this dialog as modal.
	**/
	public void showModal() {
		pushDataToControls();
		pack();
		setModal(true);
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(-1,-1);
		setVisible(true); //show();
	}
}
