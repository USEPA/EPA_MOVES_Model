/**************************************************************************************************
 * @(#)MOVESProgress.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.sql.*;
import javax.swing.*;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

/**
 * Class for MOVES MOVESProgress panel. Constructs the MOVESProgress panel, also creates and
 * initializes the layouts of the controls, and Shows or hides the progress bar. Progress
 * notificatins are sent by the MOVESEngine through a RunID.
 *
 * @author		Daniel Cox
 * @author		Wesley Faler
 * @author  	Bill Shaw (508 compliance mods)
 * @author      John Covey (Task 2003)
 * @version  2020-07-24
**/
public class MOVESProgress extends JPanel implements MOVESEngineListener, ActionListener {
	/** EPA Seal image icon. **/
	ImageIcon movesLogo;
	/** EPA Seal label. **/
	JLabel movesLogoLabel;
	/** The progess bar. **/
	JProgressBar progressBar;
	/** Used to put space between logo and progress bar. **/
	JLabel emptyLabel;
	/** The associated RunID of the MOVESEngine that is sending progress notifications. **/
	long runID = 0;
	/** Used with the message log timer to identify when the list should be emptied. **/
	long lastRunID = 0;
	/** Used to determine when the database has new error messages to display **/
	long lastMOVESErrorID = 0;
	/**
	 * Timer used with the run errors message display.
	**/
	javax.swing.Timer messageLogTimer = new javax.swing.Timer(2000, this);
	/** Message log component. **/
	JList<String> messageLogList;
	/** DefaultListModel for the messageLogList. **/
	DefaultListModel<String> messageLogModel;
	/** JScrollPane for the messageLogList. **/
	JScrollPane messageLogPane;
	/** Parent frame **/
	JFrame parentFrame;
	/** true when the time estimate dialog should be shown **/
	boolean allowTimeDialog = true;

	class EstimatedTimeRemainingDialog extends JDialog implements ActionListener {
		/** The dialog result, indicates true on OK button. **/
		public boolean result = false;
		/** The parent JFrame which invokes this dialog. **/
		JFrame frame;
		/** OK button. **/
		JButton okButton;
		/** Textual status display **/
		JLabel stateLabel;
		/** Textual time remaining display **/
		JLabel timeLabel;
		/** flag set when the dialog is shown for the first time **/
		boolean isShown = false;

		public EstimatedTimeRemainingDialog(JFrame parent) {
			super(parent, "Estimated Time Remaining");
			frame = parent;

			getContentPane().setLayout(new BorderLayout());
			getContentPane().add(createPanel(), BorderLayout.CENTER);
			pack();
			setResizable(false);
		}

		/** Allows the parent to display this dialog as modal. **/
		public void showModeless() {
			isShown = true;
			pushDataToControls();
			pack();
			setModal(false);
			setVisible(true); //show();
		}

		/** Data from member variables to dialog controls (currently unused). **/
		public void pushDataToControls() {
			if(result) {
				return;
			}
			MOVESEngine.CompletionEstimate estimate = MOVESEngine.theInstance.estimateCompletion();
			String newStateText = estimate.state;
			String newTimeText = "";
			stateLabel.setText(estimate.state);
			if(estimate.remainingMillis <= 0) {
				newTimeText = "estimating completion...";
				revalidate();
			} else if(estimate.remainingMillis < 720*60*1000) {
				int minutes = (int)(0.5 + estimate.remainingMillis / 60.0 / 1000.0);
				newTimeText = minutes + " minutes";
			} else {
				double hours = estimate.remainingMillis / 60.0 / 60.0 / 1000.0;
				hours = (int)(hours * 100);
				hours = hours / 100;
				newTimeText = hours + " hours";
			}
			if(!newStateText.equals(stateLabel.getText())
					|| !newTimeText.equals(timeLabel.getText())) {
				stateLabel.setText(newStateText);
				timeLabel.setText(newTimeText);
				revalidate();
				if(!isShown) {
					showModeless();
				}
			}
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
		public void createControls() {
			okButton = new JButton("OK");
			okButton.addActionListener(this);
			okButton.setName("okButton");
			okButton.setMnemonic('O');
			okButton.setDisplayedMnemonicIndex(0);
			ToolTipHelper.add(okButton, "Close this dialog");

			stateLabel = new JLabel("                ");
			stateLabel.setName("stateLabel");
			timeLabel = new JLabel("                ");
			timeLabel.setName("timeLabel");
		}

		/**
		 * Sets the layout of the controls.
		 * @return the container as JPanel of the controls
		**/
		public JPanel arrangeControls() {
			stateLabel.setPreferredSize(new Dimension(200,20));
			timeLabel.setPreferredSize(new Dimension(200,20));

			JPanel result = new JPanel();
			result.setLayout(new GridBagLayout());
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.fill = GridBagConstraints.NONE;
			gbc.insets = new Insets(2,2,2,2);
			gbc.gridwidth = 2;
			gbc.gridheight = 2;
			gbc.weightx = 0;
			gbc.weighty = 0;
			LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
			result.add(stateLabel, gbc);
			LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 2, 1);
			result.add(timeLabel, gbc);
			LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
			result.add(okButton, gbc);

			return result;
		}

		/**
		 * Calls the appropriate button handler.
		 * @param	e the ActionEvent to be handled.
		**/
		public void actionPerformed(ActionEvent e) {
			if(e.getSource() == okButton) {
				handleOKButton();
			}
		}

		/**
		 * OK button handler, will save the choices to the System Configuration
		 * before closing this dialog.
		**/
		void handleOKButton() {
			// indicates OK button
			result = true;
			dispose();
		}
	}

	/** Modeless dialog showing the estimated time remaining **/
	EstimatedTimeRemainingDialog timeRemainingDialog = null;

	/**
	 * Constructs an MOVESProgress panel, also creates and sets the layouts of the controls.
	 * @param shouldHideProgressBar True to initially hide the progress bar.
	**/
	public MOVESProgress(boolean shouldHideProgressBar) {
		createControls(shouldHideProgressBar);
		arrangeControls();
		// This class receives engine progress notifications
		MOVESEngine.subscribeToProgress(this);
	}

	/**
	 * Shows or hides the progress bar.
	 * @param showProgressBar Should the progress bar be shown or hidden.
	**/
	public void setProgressBarVisible(boolean showProgressBar) {
		progressBar.setVisible(showProgressBar);
		messageLogPane.setVisible(showProgressBar);
		if(!showProgressBar && timeRemainingDialog != null) {
			timeRemainingDialog.dispose();
			timeRemainingDialog = null;
		}
	}

	/**
	 * Creates and initializes all controls on this panel.
	 * @param shouldHideProgressBar Should the progress bar be initially hidden
	**/
	void createControls(boolean shouldHideProgressBar) {
		ImageIcon movesLogo100 = new ImageIcon("gov/epa/otaq/moves/master/gui/images/MOVES3-logo-480.png");
		ImageIcon movesLogo125 = new ImageIcon("gov/epa/otaq/moves/master/gui/images/MOVES3-logo-600.png");
		ImageIcon movesLogo150 = new ImageIcon("gov/epa/otaq/moves/master/gui/images/MOVES3-logo-720.png");
		ImageIcon movesLogo175 = new ImageIcon("gov/epa/otaq/moves/master/gui/images/MOVES3-logo-840.png");
		BaseMultiResolutionImage bmri = new BaseMultiResolutionImage(movesLogo100.getImage(), movesLogo125.getImage(), movesLogo150.getImage(), movesLogo175.getImage());
		movesLogo = new ImageIcon(bmri);
		
		movesLogoLabel = new JLabel(movesLogo);
		movesLogoLabel.setName("movesLogoLabel");
		emptyLabel = new JLabel("      ");
		emptyLabel.setName("emptyLabel");
		movesLogoLabel.setToolTipText(Constants.MOVES_LOGIN_LOGO_TOOLTIP);

		progressBar = new JProgressBar();
		progressBar.setName("progressBar");
		progressBar.setMinimum(1);
		progressBar.setMaximum(100);
		progressBar.setVisible(!shouldHideProgressBar);

		messageLogModel = new DefaultListModel<String>();
		messageLogList = new JListWithToolTips<String>(messageLogModel);
		messageLogList.setName("messageLogList");
		messageLogList.setSelectionMode(
				ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		messageLogList.setSelectedIndex(-1);
		messageLogList.setVisibleRowCount(4);
		messageLogList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
		messageLogPane = new JScrollPane(messageLogList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		messageLogPane.setName("messageLogPane");
		messageLogPane.setVisible(!shouldHideProgressBar);

		messageLogTimer.start();
	}

	/** Sets the layout of the controls. **/
	void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();
		setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 4;
		gbc.weightx = 0;
		gbc.weighty = 0;

		LayoutUtility.setPositionOnGrid(gbc,0, 0, "NORTH", 1, 1);
		add(movesLogoLabel, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0, 1, "CENTER", 1, 1);
		gbc.weightx = 0.0;
		gbc.weighty = 0.0;
		add(emptyLabel, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0, 2, "CENTER", 1, 1);
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 0.0;
		gbc.weighty = 1.0;
		add(messageLogPane, gbc);

		LayoutUtility.setPositionOnGrid(gbc,0, 3, "SOUTH", 1, 1);
		gbc.fill =  GridBagConstraints.HORIZONTAL;
		gbc.weightx = 1.0;
		gbc.weighty = 0.0;
		add(progressBar, gbc);
	}

	/**
	 * Handles MOVESEngine progress notifications
	 * @param srcEngine The MOVESEngine this notification comes from.
	**/
	public void engineProgressUpdate(MOVESEngine srcEngine) {
		runID = srcEngine.getActiveRunID();
		progressBar.setValue((int)(srcEngine.getPercentCompleted() * 100 + 0.5));
		// Update the dialog box showing estimated completion time
		if(parentFrame == null) {
			Component parent = getParent();
			while(parent != null && !(parent instanceof JFrame)) {
				parent = parent.getParent();
			}
			parentFrame = (JFrame)parent;
		}
		if(timeRemainingDialog == null && parentFrame != null && allowTimeDialog) {
			timeRemainingDialog = new EstimatedTimeRemainingDialog(parentFrame);
			if(parentFrame != null) {
				timeRemainingDialog.setLocation(parentFrame.getLocationOnScreen().x + 100,
						parentFrame.getLocationOnScreen().y + 100);
			}
		}
		if(timeRemainingDialog != null && !timeRemainingDialog.result) {
			timeRemainingDialog.pushDataToControls();
		}
	}

	/**
	 * Called when the MOVESEngine object is completing
	 * @param srcEngine The MOVESEngine this notification comes from.
	**/
	public void engineIsCompleting(MOVESEngine srcEngine) {
		// Do nothing.  MOVESWindow will call handleEngineIsCompleting()
		// at the proper time, thus avoiding issues with the non-gui thread
		// that is calling this routine accessing the GUI.
	}

	/** Utility routine called by MOVESWindow once a simulation is complete.  **/
	public void handleEngineIsCompleting() {
		runID = 0;
		// Remove the dialog box showing estimated completion time
		if(timeRemainingDialog != null && !timeRemainingDialog.result) {
			timeRemainingDialog.dispose();
			timeRemainingDialog = null;
		}
	}

	/**
	 * This method acts as the Action handler delegate for all the actions.
	 * @param evt The event caused by an action being performed.
	**/
	public void actionPerformed(ActionEvent evt) {
		String command = evt.getActionCommand();
		// Compare the action command to the known actions.
		if(messageLogTimer == evt.getSource()) {
			handleMessageLogTimer();
		}
	}

	/**
	 * Updates the JList control with the current contents of the MOVESError table for the
	 * current RunID.
	**/
	void handleMessageLogTimer() {
		String sql = "";
		if(runID == 0) {
			return;
		}
		if(lastRunID != runID) {
			messageLogModel.removeAllElements();
			lastMOVESErrorID = 0;
		}
		Connection outputDB = null;
		try {
			outputDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
		} catch(Exception e) {
			Logger.logError(e,"Attempt to update MOVES error log failed to get connection to "+
					"database.");
		}
		try {
			// The MOVESErrorID is an auto-number primary key, can be used as a sequence number,
			// show most recent first
			sql = "SELECT * FROM MOVESError "
					+ "WHERE MOVESRunID = ? "
					+ "ORDER BY MOVESErrorID DESC";
			PreparedStatement statement = outputDB.prepareStatement(sql);
			statement.setLong(1, runID);
			ResultSet results = SQLRunner.executeQuery(statement,sql);
			long MOVESErrorID = 0;
			String messageLine = "";
			boolean shouldRegenerate = false;
			boolean didRegenerate = false;

			int stateID;
			int countyID;
			int month;
			int year;
			while(results.next()) {
				MOVESErrorID = results.getLong("MOVESErrorID");
				if(!shouldRegenerate && MOVESErrorID > lastMOVESErrorID) {
					// Have new logs, so must regenerate the DefaultListModel
					shouldRegenerate = true;
					lastMOVESErrorID = MOVESErrorID;
				}
				if(shouldRegenerate) {
					if(!didRegenerate) {
						messageLogModel.removeAllElements();
						didRegenerate = true;
					}
					messageLine = "";
					stateID = results.getInt("stateID");
					countyID = results.getInt("countyID");
					year = results.getInt("yearID");
					month = results.getInt("monthID");
					if(stateID != 0 && countyID != 0) {
						messageLine += ("State:" + stateID + " County:" + countyID);
					}
					if(month != 0 && year != 0) {
						messageLine += (year + "-" + month);
					}
					messageLine += (" " + results.getString("errorMessage"));
					messageLine = messageLine.replace('\n',' ');
					messageLine = messageLine.replace('\r',' ');
					messageLine = messageLine.replace('\t',' ');
					messageLogModel.addElement(messageLine);
				} else {
					// No new logs have been found for this RunID
					break;
				}
			}
			results.close();
			statement.close();
		} catch(SQLException e) {
			e.printStackTrace();
			String errMsg = "Unable to read from the MOVESError table, does this table"
				+ " exist in the output database?";
			Logger.logSqlError(e, errMsg, sql);
			messageLogModel.addElement(errMsg);
		}
		try {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT, outputDB);
		} catch(Exception e) {
			Logger.logError(e,"MOVES Progress could not check-in its output database connection");
		}
		lastRunID = runID;
	}
}
