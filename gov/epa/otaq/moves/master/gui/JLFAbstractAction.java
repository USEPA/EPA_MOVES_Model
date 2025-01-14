/*
 * @(#)JLFAbstractAction.java	1.6 00/06/12
 *
 * Copyright 2000 by Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You
 * shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement
 * you entered into with Sun.
 */
package gov.epa.otaq.moves.master.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import javax.swing.event.EventListenerList;

/**
 * Abstract Action for the JLF. Defines some useful methods.
 *
 * MODIFIED on 2002-Jun-25 by WGFALER to improve image search algorithm
 * for custom images that are not in the default folders
 *
 * @version		2003-08-20
 * @author  Mark Davidson
**/
public abstract class JLFAbstractAction extends AbstractAction {

	/** The listener to action events (usually the main UI) **/
	EventListenerList listeners;

	/** Image directory URL **/
	public static final String JLF_IMAGE_DIR = "/toolbarButtonGraphics/general/";

	/**
	 * The key used for storing a large icon for the action,
	 * used for toolbar buttons.
	 * <p>
	 * Note: Eventually this key belongs in the javax.swing.Action interface.
	**/
	public static final String LARGE_ICON = "LargeIcon";
	public static final String TITLE = "Title";

	//
	// These next public methods may belong in the AbstractAction class.
	//

	/** 
	 * Gets the value from the key Action.ACTION_COMMAND_KEY
	 * @return the value of the action command key.
	**/
	public String getActionCommand()  {
		return (String)getValue(Action.ACTION_COMMAND_KEY);
	}

	/** 
	 * Gets the value from the key Action.SHORT_DESCRIPTION
	 * @return the value of the action short description key.
	**/
	public String getShortDescription()  {
		return (String)getValue(Action.SHORT_DESCRIPTION);
	}

	/** 
	 * Gets the value from the key Action.LONG_DESCRIPTION
	 * @return the value of the action long description key.
	**/
	public String getLongDescription()  {
		return (String)getValue(Action.LONG_DESCRIPTION);
	}

	/** 
	 * Gets the value from the key Action.NAME
	 * @return the value of the action name key or "" if the value is null.
	**/
	public String getName()  {
		String result = (String)getValue(Action.NAME);
        if (result == null) {
            result = "";
        }
        return result;
	}

	/** 
	 * Gets either the TITLE (if set) or NAME (otherwise), prepended with the model name
	 * @return "" if both TITLE and NAME are blank.
	**/
	public String getTitle()  {
		String result = (String)getValue(TITLE);
        if (result == null) {
            result = getName();
        }
        if (!result.equals("")) {
            result = MOVESWindow.MOVES_VERSION + " - " + result;
        }
        return result;
	}
	
	/* Should finish the implementation and add get/set methods for all the 
	 * javax.swing.Action keys:
		
		Action.NAME
		Action.SMALL_ICON
		ActionConstants.LARGE_ICON
		Action.MNEMONIC_KEY
	 */

	// ActionListener registration and invocation.

	/** 
	 * Forwards the ActionEvent to the registered listener.
	 * @param evt event to be forwarded to listener.
	**/
	public void actionPerformed(ActionEvent evt)  {
		if (listeners != null) {
			Object[] listenerList = listeners.getListenerList();

			// Recreate the ActionEvent and stuff the value of the ACTION_COMMAND_KEY
			ActionEvent e = new ActionEvent(evt.getSource(), evt.getID(), 
					(String)getValue(Action.ACTION_COMMAND_KEY));
			for (int i = 0; i <= listenerList.length-2; i += 2) {
				((ActionListener)listenerList[i+1]).actionPerformed(e);
			}
		}
	}

	/** 
	 * Adds action listener to list.
	 * @param l the action listener.
	**/
	public void addActionListener(ActionListener l)  {
		if (listeners == null) {
			listeners = new EventListenerList();
		}
		listeners.add(ActionListener.class, l);
	}

	/** 
	 * Remove action listener from list.
	 * @param l the action listener.
	**/
	public void removeActionListener(ActionListener l)  {
		if (listeners == null) {
			return;
		}
		listeners.remove(ActionListener.class, l);
	}

	/** 
	 * Returns the Icon associated with the name from the resources.
	 * The resouce should be in the path.
	 * @param name Name of the icon file i.e., help16.gif
	 * @return the name of the image or null if the icon is not found.
	**/
	public ImageIcon getIcon(String name)  {
		String imagePath = JLF_IMAGE_DIR + name;
		URL url = this.getClass().getResource(imagePath);
		if (url != null)  {
			return new ImageIcon(url);
		} else {
			url = this.getClass().getResource("/" + name);
			if (url != null)  {
				return new ImageIcon(url);
			} else {
				url = this.getClass().getResource(name);
				if (url != null)  {
					return new ImageIcon(url);
				}
			}
		}
		return null;
	}
}
