/**************************************************************************************************
 * @(#)WorkerWindow.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.worker.gui;

import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.MOVESThread;
import gov.epa.otaq.moves.common.WindowStateHandler;
import gov.epa.otaq.moves.worker.framework.*;
import gov.epa.otaq.moves.master.gui.MOVESWindow;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Iterator;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.ImageIcon;

/**
 * Main window for the distributed worker system.
 *
 * @author		Daniel Cox
 * @author		Wesley Faler
 * @author		Tim Hull
 * @author		Harvey Michaels
 * @version	    2014-07-17
**/
public class WorkerWindow extends JFrame {
	/** Fixed "Status" label. **/
	JLabel constantStatusLabel;
	/** Status label. **/
	JLabel statusLabel;
	/** Fixed "Count" label. **/
	JLabel constantProcessedCountLabel;
	/** Count label. **/
	JLabel processedCountLabel;
	/** Fixed "Count" label. **/
	JLabel constantInterruptedCountLabel;
	/** Count label. **/
	JLabel interruptedCountLabel;
	/** Label with computer ID and release date **/
	JLabel versionLabel;
	/** Label with shared Distributed Folder Path **/
	JLabel sharedDistributedFolderPathLabel;

	/** Standard constructor **/
	public WorkerWindow() {
		// Use MOVES icon in task bar, etc
		ArrayList<Image> iconList = new ArrayList<Image>();
        iconList.add(new ImageIcon("gov/epa/otaq/moves/master/gui/images/moves_16x16.png").getImage());
        iconList.add(new ImageIcon("gov/epa/otaq/moves/master/gui/images/moves_32x32.png").getImage());
        iconList.add(new ImageIcon("gov/epa/otaq/moves/master/gui/images/moves_48x48.png").getImage());
        iconList.add(new ImageIcon("gov/epa/otaq/moves/master/gui/images/moves_256x256.png").getImage());
        setIconImages(iconList);

		createControls();
		arrangeControls();

		// Use a local class that extends WindowAdapter which implements WindowListener.
		/**
		 * This class routes windowClosing method invocations to the WorkerWindow object.
		 * This avoids WorkerWindow having to implement the entire WindowListener interface.
		**/
		class CloseWindowAdapter extends WindowAdapter {
			/** The WorkerWindow to route windowClosing methods to **/
			WorkerWindow targetWindow;

			/**
			 * Standard constructor
			 * @param workerWindow The WorkerWindow to route windowClosing methods to
			**/
			CloseWindowAdapter(WorkerWindow workerWindow) {
				targetWindow = workerWindow;
			}

			/**
			 * Standard WindowListener method implementation.
			 * @param e The source WindowEvent
			**/
			public void windowClosing(WindowEvent e) {
				targetWindow.windowClosing(e);
			}
		}

		addWindowListener(new CloseWindowAdapter(this));

		WorkerGUI.startupThreads(this);

		setVisible(true);
	}

	/** Called when this system completes processing a work file. **/
	public void processedWorkFile() {
		processedCountLabel.setText(String.valueOf(
				Integer.parseInt(processedCountLabel.getText()) + 1));
	}

	/** Called when the progress file is deleted while this worker system is working. **/
	public void interruptedProcessingWorkFile() {
		interruptedCountLabel.setText(String.valueOf(
				Integer.parseInt(interruptedCountLabel.getText()) + 1));
	}

	/**
	 * Called when a work file is started or when an idle state is reached.
	 * @param text Describes the workers current status.
	 *
	**/
	public void changeStatusText(String text) {
		statusLabel.setText(text);
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		constantStatusLabel = new JLabel("Status:");
		statusLabel = new JLabel("Idle");
		constantProcessedCountLabel = new JLabel("Number of Files Processed:");
		processedCountLabel = new JLabel("0");
		constantInterruptedCountLabel = new JLabel("Number of Interruptions:");
		interruptedCountLabel = new JLabel("0");
		versionLabel = new JLabel("Computer ID: "
				+ WorkerConfiguration.theWorkerConfiguration.computerID
				+ "       Worker Release: " + MOVESWindow.MOVES_VERSION);
		try {
			sharedDistributedFolderPathLabel = new JLabel("Shared Distributed Folder Path: " 
					+ WorkerConfiguration.theWorkerConfiguration.sharedDistributedFolderPath.getCanonicalPath());
		} catch (IOException exception) {
			Logger.logError(exception, "Unable to acquire the shared Distributed Folder Path.");			
		}
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		JPanel panel = new JPanel();
		if(constantStatusLabel == null) {
			Logger.log(LogMessageCategory.ERROR, "control is null");
			return;
		}

		setTitle("MOVES Worker - ID "
				+ WorkerConfiguration.theWorkerConfiguration.distributedWorkerId);

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(20, 10, 20, 10);
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;
		gbc.anchor = GridBagConstraints.WEST;
		panel.setLayout(new GridBagLayout());

		gbc.gridx = 0;
		gbc.gridy = 0;
		panel.add(constantStatusLabel, gbc);
		gbc.gridx = 1;
		panel.add(statusLabel, gbc);

		gbc.gridx = 0;
		gbc.gridy = 1;
		panel.add(constantProcessedCountLabel , gbc);
		gbc.gridx = 1;
		panel.add(processedCountLabel, gbc);

		gbc.gridx = 0;
		gbc.gridy = 2;
		panel.add(constantInterruptedCountLabel , gbc);
		gbc.gridx = 1;
		panel.add(interruptedCountLabel, gbc);

		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.gridwidth = 2;
		panel.add(versionLabel , gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.gridwidth = 2;
		panel.add(sharedDistributedFolderPathLabel , gbc);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(panel, BorderLayout.WEST);

		setSize(700,310);
		setResizable(false);
		(new WindowStateHandler(this)).
			setPositionAndStartTracking();
	}

	/**
	 * Standard WindowListener method implementation.
	 * @param e The source WindowEvent
	**/
	public void windowClosing(WindowEvent e) {
		closeWindow();
	}

	/**
	 * Close the window.
	**/
	public void closeWindow() {
		Logger.log(LogMessageCategory.INFO,"WorkerWindow.closeWindow");
		System.out.flush();

		WorkerGUI.stopThreads();

		dispose();

		WorkerGUI.workerWindowIsClosing();
	}
}
