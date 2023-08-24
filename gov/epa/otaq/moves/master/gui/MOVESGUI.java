/**************************************************************************************************
 * @(#)MOVESGUI.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.plaf.metal.*;

import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.common.*;

/**
 * Class for MOVES GUI, contains main() which invokes the main application window.
 *
 * @author		Wesley Faler
 * @author		Tim Hull
 * @version		2013-12-02
**/
public class MOVESGUI {
	/**
	 * Invokes the main application window, MOVESWindow.
	 * @param args A string containing the argument list passed to the application.
	**/
	public static void main(String[] args) {
		//setupLookAndFeel();
		
		boolean okToPopupMessages = true; 
		if(args != null && args.length >= 1 && args[0].equalsIgnoreCase("NoAbout")) {
			okToPopupMessages = false;
		} //moved up here to accomodate error messages
		
		if(MOVESAPI.hasMasterOnThisComputer()){
			String message = "A MOVES instance is already running, or a firewall is blocking port(s) used by MOVES.\nStartup of this instance of MOVES cannot continue.";
			if(okToPopupMessages){
				JOptionPane errorPane = new JOptionPane(message,
				JOptionPane.ERROR_MESSAGE,
				JOptionPane.DEFAULT_OPTION);
				JDialog errorDialog = errorPane.createDialog(null,"Error");
				errorDialog.setAlwaysOnTop(true);
				errorDialog.setVisible(true);
			}
			else {
				Logger.log(LogMessageCategory.ERROR, message);
			}
			System.exit(0);
			return;
		}

		if (!SystemConfiguration.theSystemConfiguration.didLoad) {
			String message = "The system configuration failed to load.";
			if(okToPopupMessages){ //presenting system configuration error in same way
				JOptionPane errorPane = new JOptionPane(message,
				JOptionPane.ERROR_MESSAGE,
				JOptionPane.DEFAULT_OPTION);
				JDialog errorDialog = errorPane.createDialog(null,"Error");
				errorDialog.setAlwaysOnTop(true);
				errorDialog.setVisible(true);
			}
			else {
				Logger.log(LogMessageCategory.ERROR, message);
			}
			System.exit(0);
			return;
		}
		
		if(!LoginDialog.checkDatabaseLogin()) {
			System.exit(0);
			return;
		}

		new MOVESWindow(okToPopupMessages);
	}

	/** Set the system look and feel **/
	private static void setupLookAndFeel() {
		String lookAndFeel = UIManager.getSystemLookAndFeelClassName();
		try {
			UIManager.setLookAndFeel(lookAndFeel);
		
			if(lookAndFeel.equalsIgnoreCase("Metal")) {
				MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
				UIManager.setLookAndFeel(new MetalLookAndFeel()); 
			}	
		} catch (ClassNotFoundException e) {
			System.err.println("Couldn't find class for specified look and feel:"
					+ lookAndFeel);
			System.err.println("Did you include the L&F library in the class path?");
			System.err.println("Using the default look and feel.");
		} catch (UnsupportedLookAndFeelException e) {
			System.err.println("Can't use the specified look and feel ("
					+ lookAndFeel + ") on this platform.");
			System.err.println("Using the default look and feel.");
		} catch (Exception e) {
			System.err.println("Couldn't get specified look and feel ("
					+ lookAndFeel + "), for some reason.");
			System.err.println("Using the default look and feel.");
			e.printStackTrace();
		}
	}
}
