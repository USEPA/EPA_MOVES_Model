/**************************************************************************************************
 * @(#)InternalControlStrategy.java 
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.*;
import java.sql.*;
import javax.swing.JPanel;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;

/**
 * The base class for all Control Strategies that modify core input data at the start of a Master 
 * Loop iteration. Implements Comparable to facilitate TreeSet storage. Internal Control 
 * Strategies typically model conditions (or controls) affecting simulation inputs that change with
 * changes in the Master Loop Context. They can add their own panel to the MOVES GUI to query the 
 * user for the condition parameters.
 * 
 * @author		Wesley Faler
 * @version		2007-02-13
**/
public abstract class InternalControlStrategy extends ControlStrategy
		implements MasterLoopable, Comparable {
	/** GeographicSelection objects **/
	public TreeSet geographicSelections = new TreeSet();
	/** TimeSpan objects **/
	public TreeSet timeSpans = new TreeSet();
	/** OnRoadVehicleSelection objects **/
	TreeSet onRoadVehicleSelections = new TreeSet();
	/** OffRoadVehicleSelection objects **/
	TreeSet offRoadVehicleSelections = new TreeSet();
	/** description of the instance **/
	String description = new String("");

	/**
	 * Gets the GUI detail panel object for the user to configure details specific to the
	 * derived subclass.
	 * @return The associated GUI JPanel for the derived subclass.
	**/
	abstract public InternalControlStrategyPanel getDetailsPanel();

	/**
	 * Creates XML representing this instance
	 * @return XML, ready to be inserted into a larger XML file under a suitable root.
	 * null if only tab-separated-values are supported.
	**/
	abstract public String getXML();

	/**
	 * Creates Tab-Separated-Value text representing this instance.
	 * @return text, ready to be inserted into a text file.  null if only XML is supported.
	**/
	abstract public String getTSV();

	/**
	 * Read XML into this instance
	 * @param className name of the class used to instantiate this object
	 * @param root XML object under which the XML returned by getXML() has been stored
	 * @return true if the XML could be read successfully
	**/
	abstract public boolean acceptXML(String className,Node root);

	/**
	 * Read Tab-Separated-Values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param text tab-separated-values as returned by getTSV()
	 * @return true if the text could be read successfully
	**/
	abstract public boolean acceptTSV(String className,String text);

	/** Cancel all user edits made since last save or load from XML **/
	public void cancelEdits() {
		// Default is to do nothing
	}

	/**
	 * Get a description of the object
	 * @return a description of the object, never null or ""
	**/
	public String getDescription() {
		if(description == null || description.length() <= 0) {
			return "(default)";
		}
		return description;
	}

	/**
	 * Change the description
	 * @param newDescription new description.  If null or "", getDescription() will
	 * return a default description.
	**/
	public void setDescription(String newDescription) {
		if(newDescription == null) {
			description = "";
		} else {
			description = newDescription;
		}
	}

	/**
	 * Returns the description of the object
	 * @return a description of the object, never null or ""
	**/
	public String toString() {
		return getDescription();
	}

	/**
	 * Comparable Interface Implementation. Provides Tree container ordering and display
	 * order.
	 * @param other Another InternalControlStrategy object to compare.
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		String myDescription = getDescription();
		String otherDescription = ((InternalControlStrategy)other).getDescription();
		return myDescription.compareTo(otherDescription);
	}

	/**
	 * Overrides Object.equals() to provide a real equality test.
	 * @param other The object to test for equality against.
	 * @return True if equal.
	**/
	public boolean equals(Object other) {
		if(!(other instanceof InternalControlStrategy)) {
			return false;
		}
		return compareTo(other) == 0;
	}
}
