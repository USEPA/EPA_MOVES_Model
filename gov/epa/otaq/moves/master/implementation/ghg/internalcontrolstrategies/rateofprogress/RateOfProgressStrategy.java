/**************************************************************************************************
 * @(#)RateOfProgressStrategy.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.util.*;
import java.sql.*;
import java.io.*;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;
import java.awt.*;
import javax.swing.*;

/**
 * Implements a Control Strategy for Rate of Progress Modeling
 *
 * @author		Wesley Faler
 * @version		2010-07-05
**/
public class RateOfProgressStrategy extends InternalControlStrategy
		implements InternalControlStrategySimpleInstance, IImportExportHandler {

	/** GUI for this instance **/
	RateOfProgressPanel gui = null;

	/** True if changes since the Clean Air Act should be undone **/
	public boolean useParameters = false;

	/** TSV text to use when cancelling edits **/
	String resetTSV = null;

	/**
	 * Constructor
	**/
	public RateOfProgressStrategy() {
		if(Configuration.allowGUI) {
			gui = new RateOfProgressPanel(this);
			gui.setName("RateOfProgressPanel");
		}
	}

	/**
	 * Gets the GUI detail panel object for the user to configure details specific to the
	 * derived subclass.
	 * @return The associated GUI JPanel for the derived subclass.
	**/
	public InternalControlStrategyPanel getDetailsPanel() {
		return gui;
	}

	/** Cancel all user edits made since last save or load from XML **/
	public void cancelEdits() {
		if(gui != null) {
			gui.cancel();
		}
	}

	/**
	 * MasterLoopable override that performs loop registration.
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		if(!useParameters) {
			return;
		}

		removeCleanAirAct();
	}

	/**
	 * MasterLoopable override called during each relevant iteration of the MasterLoop.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void executeLoop(MasterLoopContext context) throws InterruptedException {
		// Nothing to do here, it was all done in subscribeToMe()
	}

	/**
	 * MasterLoopable override that cleans up all the data created within the
	 * executeLoop() method.
	 *
	 * @param context The current context of the loop.
	 * @throws InterruptedException If the active thread is interrupted.
	**/
	public void cleanDataLoop(MasterLoopContext context) throws InterruptedException {
		// Nothing to do here, it was all done in subscribeToMe()
	}

	/**
	 * Creates XML representing this instance
	 * @return XML, ready to be inserted into a larger XML file under a suitable root
	**/
	public String getXML() {
		return null;
	}

	/**
	 * Creates Tab-Separated-Value text representing this instance.
	 * @return text, ready to be inserted into a text file.  null if only XML is supported.
	**/
	public String getTSV() {
		final String eol = System.getProperty("line.separator");
		StringBuffer result = new StringBuffer(200000);
		result.append("useParameters\t" + (useParameters?"Yes":"No") + eol);
		return result.toString();
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
	 * Read Tab-Separated-Values into this instance
	 * @param className name of the class used to instantiate this object
	 * @param text tab-separated-values as returned by getTSV()
	 * @return true if the text could be read successfully
	**/
	public boolean acceptTSV(String className,String text) {
		resetTSV = text;

		useParameters = false;

		LineNumberReader reader = null;
		try {
			reader = new LineNumberReader(new StringReader(text),2*65536);
			for(int i=0;i<4;i++) {
				String line = reader.readLine();
				if(line == null || line.length() <= 0) {
					break;
				}
				ArrayList<String> parts = StringUtilities.splitCSVremovingQuotes(line,'\t');
				if(parts == null || parts.size() <= 0) {
					break;
				}
				String name = (String)parts.get(0);
				String value = null;
				if(parts.size() > 1) {
					value = (String)parts.get(1);
				}
				if(value == null) {
					value = "";
				}
				if(name.equalsIgnoreCase("useParameters")) {
					useParameters = value.equalsIgnoreCase("Yes");
					break;
				}
			}
			return true;
		} catch(Exception e) {
			Logger.logError(e,"Unable to accept TSV in RateOfProgressStrategy");
			return false;
		} finally {
			if(gui != null) {
				gui.populateControls();
				gui.revalidate();
			}
		}
	}

	/**
	 * Process a user's request to initiate an import operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was imported.
	 * false if the import request should be handled by default logic instead.
	**/
	public boolean doImport(Frame ownerWindow) {
		if(gui != null) {
			return gui.doImport(ownerWindow);
		} else {
			return false;
		}
	}

	/**
	 * Process a user's request to initiate an export operation.
	 * @param ownerWindow top-level window used as any file dialog owner
	 * @return true if the import request was handled, even if no data was exported.
	 * false if the export request should be handled by default logic instead.
	**/
	public boolean doExport(Frame ownerWindow) {
		if(gui != null) {
			return gui.doExport(ownerWindow);
		} else {
			return false;
		}
	}

	/**
	 * Perform rule checks that require searching the entire set of information
	 * @return true if the objects are suitable for use
	**/
	public boolean checkRules() {
		return true;
	}

	/**
	 * Save the current information, whether valid or not, to a file.  The type of
	 * the file is inferred from the file extension.  ".XLS" extensions create
	 * an XLS file.  ".CSV" creates a comma-separated file.  All other extensions
	 * create a tab-separated file.
	 * @param file the file to be created
	 * @return true if the data was saved successfully
	**/
	public boolean save(File file) {
		return false;
	}

	/**
	 * Undo the changes implemented for the Clean Air Act after 1993.
	**/
	private void removeCleanAirAct() {
		ArrayList<String> messages = new ArrayList<String>();
		Connection db = null;
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			if(db != null) {
				runScript(db,messages,0);
				for(Iterator<String> i=messages.iterator();i.hasNext();) {
					String message = i.next();
					LogMessageCategory category = LogMessageCategory.INFO;
					if(message.startsWith("ERROR:")) {
						//message = message.substring(6).trim();
						category = LogMessageCategory.ERROR;
					} else if(message.startsWith("WARNING:")) {
						//message = message.substring(8).trim();
						category = LogMessageCategory.WARNING;
					}
					Logger.log(category,message);
				}
			}
		} catch(Exception e) {
			Logger.logError(e,"Unable to run script for the Rate of Progress strategy");
		} finally {
			if(db != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,db);
				db = null;
			}
		}
	}

	/**
	 * Attempt to run an SQL script to update the database.
	 * To return messages, the script must fill a table named "tempMessages" which
	 * has only one column, "message" which is varchar(1000).  The script can know the name
	 * of the current default database by referencing ##defaultDatabase## where a constant
	 * database name would normally go.
	 * @param db database holding the new data
	 * @param messages messages to be shown to the user about the data checks
	 * @param mode 0 if run normally, other values reserved for future use.
	**/
	private void runScript(Connection db, ArrayList<String> messages, int mode) {
		String scriptFileName = "database/RateOfProgressStrategy.sql";
		String defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
		File scriptFile = new File(scriptFileName);
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			if(!scriptFile.exists()) {
				return;
			}
			sql = "create table if not exists tempMessages "
					+ "( message varchar(1000) not null )";
			SQLRunner.executeSQL(db,sql);
			sql = "truncate table tempMessages";
			SQLRunner.executeSQL(db,sql);

			// Run the script, substituting the defaultDatabaseName as ##defaultDatabase##
			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			replacements.put("##defaultDatabase##",defaultDatabaseName);
			replacements.put("##mode##",""+mode);
			try {
				DatabaseUtilities.executeScript(db,scriptFile,replacements);
			} catch(Exception e) {
				messages.add("ERROR: Unable to execute script");
				Logger.logError(e,"Unable to execute script " + scriptFileName);
			}

			if(messages != null) {
				// Retrieve the results from tempMessages
				sql = "select message from tempMessages";
				query.open(db,sql);
				TreeSetIgnoreCase messagesAlreadySeen = new TreeSetIgnoreCase();
				while(query.rs.next()) {
					String m = query.rs.getString(1);
					if(m != null && m.length() > 0) {
						if(!messagesAlreadySeen.contains(m)) {
							messagesAlreadySeen.add(m);
							messages.add(m);
						}
					}
				}
				query.close();
			}

			sql = "drop table if exists tempMessages";
			SQLRunner.executeSQL(db,sql);
		} catch(Exception e) {
			// Nothing to do here
		} finally {
			query.onFinally();
		}
	}
}
