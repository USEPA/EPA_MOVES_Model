/**************************************************************************************************
 * @(#)SourceManufacturingControlStrategy.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.general;

import gov.epa.otaq.moves.master.framework.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;

/**
 * This modifies ELDB and EERDB based on user parameters.
 * EERDB (the Execution Emission Rate Database) and ELDB (the Execution
 * Location Database) are explained in totalActivityGenerator
 *
 * @author		Cimulus
 * @version		2007-02-13
**/
public class SourceManufacturingControlStrategy extends InternalControlStrategy {
	/** Default constructor **/
	public SourceManufacturingControlStrategy() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
	}

	/**
	 * Called during each relevant iteration of the MasterLoop.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use 
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
	}

	/**
	 * InternalControlStrategy override that gets the GUI detail panel object for the user
	 * to configure details specific to the derived subclass.
	 * @return The associated GUI JPanel for the derived subclass.
	**/
	public InternalControlStrategyPanel getDetailsPanel() {
		return null;
	}

	/**
	 * Comparable Interface Implementation. Provides Tree container ordering and display
	 * order.
	 * @param other Another InternalControlStrategy object to compare.
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		if(other instanceof InternalControlStrategy) {
			InternalControlStrategy otherIcs = (InternalControlStrategy)other;
			// Sorting on these instances is trivial, but will choose the class name
			return this.getClass().getName().compareTo(otherIcs.getClass().getName());
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * Creates XML representing this instance
	 * @return XML, ready to be inserted into a larger XML file under a suitable root
	**/
	public String getXML() {
		return "";
	}

	/**
	 * Read XML into this instance
	 * @param className name of the class used to instantiate this object
	 * @param root XML object under which the XML returned by getXML() has been stored
	 * @return true if the XML could be read successfully
	**/
	public boolean acceptXML(String className,Node root) {
		return false;
	}

	/**
	 * Creates Tab-Separated-Value text representing this instance.
	 * @return text, ready to be inserted into a text file.  null if only XML is supported.
	**/
	public String getTSV() {
		return "";
	}

	/**
	 * Read Tab-Separated-Values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param text tab-separated-values as returned by getTSV()
	 * @return true if the text could be read successfully
	**/
	public boolean acceptTSV(String className,String text) {
		return false;
	}
}
