/**************************************************************************************************
 * @(#)WindowStateHandler.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;

/**
 * Store size and position of a window with the ability to restore
 * a window's position and size.
 *
 * @author		Wesley Faler
 * @version		2009-04-05
**/
public class WindowStateHandler implements ComponentListener, WindowListener {
	private static class WindowInformation {
		public int x;
		public int y;
		public int width;
		public int height;

		public WindowInformation() {
		}

		public boolean parse(String line) {
			String[] parts = line.split("\\,");
			if(parts == null || parts.length != 4) {
				return false;
			}
			int tx, ty, twidth, theight;
			try {
				tx = Integer.parseInt(parts[0].trim());
				ty = Integer.parseInt(parts[1].trim());
				twidth = Integer.parseInt(parts[2].trim());
				theight = Integer.parseInt(parts[3].trim());
			} catch(Exception e) {
				return false;
			}
			if(twidth < 0 || theight < 0) {
				return false;
			}
			x = tx;
			y = ty;
			width = twidth;
			height = theight;
			return true;
		}

		public String toString() {
			return "" + x + "," + y + "," + width + "," + height;
		}
	}

	/** Name of the file holding information about sizes and locations **/
	public static String infoFileName = "MOVESWindows.txt";

	/** Mutex to synchronize access from all windows to the information file **/
	private static Integer fileMutex = new Integer(90210);

	/** Window being controlled **/
	Window window = null;
	/** Logical name of window **/
	String name = "";
	/** True after size and position tracking has began **/
	boolean isTracking = false;
	/** True if the window being tracked is iconified **/
	boolean isIconified = false;
	/** True if the window size is being tracked **/
	boolean shouldTrackSize = true;
	/** Location on screen **/
	Point location = new Point();
	/** Size of window **/
	Dimension size = new Dimension();

	/**
	 * Constructor
	 * @param windowToUse window being controlled
	**/
	public WindowStateHandler(Window windowToUse) {
		window = windowToUse;
		name = window.getClass().getName();
	}

	/**
	 * Constructor
	 * @param windowToUse window being controlled
	 * @param nameToUse name to be used instead of the class name
	**/
	public WindowStateHandler(Window windowToUse, String nameToUse) {
		window = windowToUse;
		name = nameToUse;
	}

	/**
	 * Set the window's size and position, and begin tracking movements and resizing.
	 * @param defaultWidth default width to use if there is no stored position and size
	 * @param defaultHeight default height to use if there is no stored position and size
	**/
	public void setSizePositionAndStartTracking(int defaultWidth, int defaultHeight) {
		if(isTracking) {
			return;
		}
		window.addComponentListener(this);
		window.addWindowListener(this);
		if(loadInformation()) {
			window.setSize(size);
			window.setLocation(location);
		} else {
			if(defaultWidth > 0 && defaultHeight > 0) {
				window.setSize(defaultWidth,defaultHeight);
			}
		}
		isTracking = true;
	}

	/**
	 * Set the window's position but not size, and begin tracking movements.
	**/
	public void setPositionAndStartTracking() {
		if(isTracking) {
			return;
		}
		shouldTrackSize = false;
		window.addComponentListener(this);
		window.addWindowListener(this);
		if(loadInformation()) {
			window.setLocation(location);
		}
		isTracking = true;
	}

	/**
	 * Read the data file contents.
	 * @return a map, keyed by class name, with data being WindowInformation objects
	**/
	private static TreeMap<String,WindowInformation> readDataFile() {
		synchronized(fileMutex) {
			try {
				ArrayList<String> lines = FileUtilities.readLines(new File(infoFileName));
				if(lines == null || lines.size() <= 0) {
					return null;
				}
				TreeMap<String,WindowInformation> information = 
						new TreeMap<String,WindowInformation>();
				for(int i=0;i<lines.size();i++) {
					String line = (String)lines.get(i);
					if(line == null) {
						continue;
					}
					String[] parts = line.split("\\=");
					if(parts == null || parts.length != 2) {
						continue;
					}
					String className = parts[0].trim();
					if(className.length() <= 0) {
						continue;
					}
					String infoText = parts[1].trim();
					WindowInformation info = new WindowInformation();
					if(!info.parse(infoText)) {
						continue;
					}
					information.put(className,info);
				}
				if(information.size() <= 0) {
					return null;
				}
				return information;
			} catch(Exception e) {
				// Nothing to do here
			}
			return null;
		}
	}

	/**
	 * Write the data file contents.
	 * @param information a map, keyed by class name, with data being WindowInformation objects
	**/
	private static void writeDataFile(TreeMap<String,WindowInformation> information) {
		synchronized(fileMutex) {
			String lines = "";
			Set<String> keys = information.keySet();
			for(Iterator<String> i=keys.iterator();i.hasNext();) {
				String k = (String)i.next();
				WindowInformation info = (WindowInformation)information.get(k);
				if(info == null) {
					continue;
				}
				lines += k + "=" + info.toString() + "\r\n";
			}
			FileUtilities.writeFileContents(infoFileName,lines);
		}
	}

	/**
	 * Read the window's size and location from the data file.
	 * @return true if the information was loaded, otherwise default values should be used.
	**/
	private boolean loadInformation() {
		synchronized(fileMutex) {
			TreeMap<String,WindowInformation> information = readDataFile();
			if(information == null) {
				return false;
			}
			WindowInformation info = (WindowInformation)information.get(name);
			if(info == null) {
				return false;
			}
			location.setLocation(info.x,info.y);
			size.setSize(info.width,info.height);
			//System.out.println("Loaded window data for " + name);
			return true;
		}
	}

	/**
	 * Write the window's size and location to the data file.
	**/
	private void saveInformation() {
		synchronized(fileMutex) {
			TreeMap<String,WindowInformation> information = readDataFile();
			if(information == null) {
				information = new TreeMap<String,WindowInformation>();
			}
			WindowInformation info = new WindowInformation();
			info.x = (int)location.getX();
			info.y = (int)location.getY();
			info.width = (int)size.getWidth();
			info.height = (int)size.getHeight();
			information.put(name,info);
			writeDataFile(information);
			//System.out.println("Saved window data for " + name);
		}
	}

	/**
	 * Get the window's current size and location.
	**/
	private void getInformation() {
		if(!isIconified) {
			window.getLocation(location);
			window.getSize(size);
			//System.out.println("Captured information for " + name);
		}
	}

	/**
	 * Invoked when the component's size changes.
	 * @param e event
	**/
	public void componentResized(ComponentEvent e) {
		if(!isTracking) {
			return;
		}
		getInformation();
	}

	/**
	 * Invoked when the component's position changes.
	 * @param e event
	**/
	public void componentMoved(ComponentEvent e) {
		if(!isTracking) {
			return;
		}
		getInformation();
	}

	/**
	 * Invoked when the component has been made visible.
	 * @param e event
	**/
	public void componentShown(ComponentEvent e) {
		if(!isTracking) {
			return;
		}
		getInformation();
	}

	/**
	 * Invoked when the component has been made invisible.
	 * @param e event
	**/
	public void componentHidden(ComponentEvent e) {
		// Nothing to do here
	}

	/**
	 * Invoked when the Window is set to be the active Window.
	 * @param e event 
	**/
	public void windowActivated(WindowEvent e) {
		// Nothing to do
	}
	
	/**
	 * Invoked when a window has been closed as the result of calling dispose on the window.
	 * @param e event 
	**/
	public void windowClosed(WindowEvent e) {
		if(!isTracking) {
			return;
		}
		saveInformation();
		isTracking = false;
	}

	/**
	 * Invoked when the user attempts to close the window from the window's system menu.
	 * @param e event 
	**/
	public void windowClosing(WindowEvent e) {
		if(!isTracking) {
			return;
		}
		saveInformation();
		isTracking = false;
	}
	
	/**
	 * Invoked when a Window is no longer the active Window.
	 * @param e event 
	**/
	public void windowDeactivated(WindowEvent e) {
		// Nothing to do
	}
	
	/**
	 * Invoked when a window is changed from a minimized to a normal state.
	 * @param e event 
	**/
	public void windowDeiconified(WindowEvent e) {
		isIconified = false;
	}

	/**
	 * Invoked when a window is changed from a normal to a minimized state.
	 * @param e event 
	**/
	public void windowIconified(WindowEvent e) {
		isIconified = true;
	}

	/**
	 * Invoked the first time a window is made visible.
	 * @param e event 
	**/
	public void windowOpened(WindowEvent e) {
		// Nothing to do
	}
}
