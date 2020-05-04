/**************************************************************************************************
 * @(#)BundleManifest.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.sql.*;
import java.io.*;
import java.util.*;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;

/**
 * A file listing details for each bundle exchanged between masters and workers.
 * The file is an XML format with all data under a &lt;bundlemanifest&gt; tag.
 *
 * @author		Wesley Faler
 * @version		2014-10-13
**/
public class BundleManifest {
	/** controls event recording to the bundleTracking table **/
	private static final boolean shouldRecordEvents = true;
	/** Name used by all manifest files **/
	public static final String MANIFEST_FILE_NAME = "BundleManifest.xml";

	public static class MasterFragment {
		public String outputTimePeriod = "";
		public String timeUnits = "";
		public String distanceUnits = "";
		public String massUnits = "";
		public String energyUnits = "";
		public String runSpecFileName = "";
		public String runSpecDescription = "";
		public String runSpecFileDateTime = "";
		public String runDateTime = "";
		public String model = "onroad";
		public String scale = "";
		public String defaultDatabaseUsed = "";
		public String masterVersionDate = "";
		public String masterComputerID = "";
		public String masterIDNumber = "";
		public String domain = "";
		public int domainCountyID = 0;
		public String domainCountyName = "";
		public String domainDatabaseServer = "";
		public String domainDatabaseName = "";
		public int expectedDONEFiles = 0;
		public String outputDatabaseName = "";

		public void copyFrom(MasterFragment other) {
			outputTimePeriod = other.outputTimePeriod;
			timeUnits = other.timeUnits;
			distanceUnits = other.distanceUnits;
			massUnits = other.massUnits;
			energyUnits = other.energyUnits;
			runSpecFileName = other.runSpecFileName;
			runSpecDescription = other.runSpecDescription;
			runSpecFileDateTime = other.runSpecFileDateTime;
			runDateTime = other.runDateTime;
			model = other.model;
			scale = other.scale;
			defaultDatabaseUsed = other.defaultDatabaseUsed;
			masterVersionDate = other.masterVersionDate;
			masterComputerID = other.masterComputerID;
			masterIDNumber = other.masterIDNumber;
			domain = other.domain;
			domainCountyID = other.domainCountyID;
			domainCountyName = other.domainCountyName;
			domainDatabaseServer = other.domainDatabaseServer;
			domainDatabaseName = other.domainDatabaseName;
			expectedDONEFiles = other.expectedDONEFiles;
			outputDatabaseName = other.outputDatabaseName;
		}
	}
	
	public static class DurationFragment {
		public String loopableName = null;
		public double durationSeconds = 0;
		
		public DurationFragment() {
		}
		
		public DurationFragment(String loopableNameToUse, double durationSecondsToUse) {
			loopableName = loopableNameToUse;
			durationSeconds = durationSecondsToUse;
		}
	}

	/** Details needed to create an entry in the output database's MOVESRun table **/
	public MasterFragment masterFragment = new MasterFragment();

	/** List of tables to retrieve from workers and take to the master **/
	public TreeSetIgnoreCase tablesToRetrieve = new TreeSetIgnoreCase();
	/** Machine-readable context information, suitable for use with MasterLoopContext **/
	public String context = "";
	/** Human-readable context information **/
	public String contextForHumans = "";
	/** Name of the bundle file **/
	public String bundleName = "";
	/** When returned from a worker, the version of the worker code **/
	public String workerVersion = "";
	/** When returned from a worker, the computer ID of the worker **/
	public String workerComputerID = "";
	/** When returned from a worker, the worker ID **/
	public String workerID = "";
	/** When sent or returned from a worker, the sequence number of the bundle **/
	public int bundleNumber = 0;
	/** When returned from a worker, the duration of the task on the worker **/
	public float durationSeconds = 0;
	/** When returned from a worker that provides details, the duration of each part of a task on the worker **/
	public ArrayList<DurationFragment> durationFragments = new ArrayList<DurationFragment>();
	/**
	 * The computer ID of the master that should process the DONE files.  When blank, the
	 * computer ID of the originating master (stored in masterFragment.masterComputerID)
	 * is used.
	**/
	public String destinationMasterComputerID = "";
	/**
	 * The unique ID of the master that should process the DONE files.  When blank, the
	 * unique ID of the originating master (stored in masterFragment.masterIDNumber)
	 * is used.
	**/
	public String destinationMasterIDNumber = "";


	/**
	 * Constructor
	**/
	public BundleManifest() {
	}

	/**
	 * Copy from another MasterFragment
	 * @param other another MasterFragment
	**/
	public void copyFrom(MasterFragment other) {
		masterFragment.copyFrom(other);
	}

	/**
	 * Copy to another MasterFragment
	 * @param other another MasterFragment
	**/
	public void copyTo(MasterFragment other) {
		other.copyFrom(masterFragment);
	}

	/**
	 * Write this manifest into a folder, overwriting any manifest file that already exists there.
	 * @param folder folder to receive the new manifest file.
	 * @throws IOException if anything goes wrong
	 * @return true the file was written
	**/
	public boolean writeToFolder(File folder) throws IOException {
		if(destinationMasterComputerID == null || destinationMasterComputerID.length() <= 0) {
			destinationMasterComputerID = masterFragment.masterComputerID;
		}
		if(destinationMasterIDNumber == null || destinationMasterIDNumber.length() <= 0) {
			destinationMasterIDNumber = masterFragment.masterIDNumber;
		}

		File mf = new File(folder,MANIFEST_FILE_NAME);
		if(mf.exists()) {
			mf.delete();
		}
		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(mf),32768));
			writer.println("<bundlemanifest>");
			writer.println("\t<machinecontext>" + context + "</machinecontext>");
			writer.println("\t<humancontext>" + contextForHumans + "</humancontext>");
			writer.println("\t<bundlename>" + bundleName + "</bundlename>");
			for(Iterator i=tablesToRetrieve.iterator();i.hasNext();) {
				String t = (String)i.next();
				writer.println("\t<retrievetable>" + t + "</retrievetable>");
			}
			writer.println("\t<workerversion>" + workerVersion + "</workerversion>");
			writer.println("\t<workercomputerid>" + workerComputerID + "</workercomputerid>");
			writer.println("\t<workerid>" + workerID + "</workerid>");
			writer.println("\t<bundlenumber>" + bundleNumber + "</bundlenumber>");
			writer.println("\t<durationseconds>" + durationSeconds + "</durationseconds>");
			writer.println("\t<destinationmastercomputerid>" + destinationMasterComputerID + "</destinationmastercomputerid>");
			writer.println("\t<destinationmasteridnumber>" + destinationMasterIDNumber + "</destinationmasteridnumber>");

			writer.println("\t<master_outputtimeperiod>" + masterFragment.outputTimePeriod + "</master_outputtimeperiod>");
			writer.println("\t<master_timeunits>" + masterFragment.timeUnits + "</master_timeunits>");
			writer.println("\t<master_distanceunits>" + masterFragment.distanceUnits + "</master_distanceunits>");
			writer.println("\t<master_massunits>" + masterFragment.massUnits + "</master_massunits>");
			writer.println("\t<master_energyunits>" + masterFragment.energyUnits + "</master_energyunits>");
			writer.println("\t<master_runspecfilename>" + masterFragment.runSpecFileName + "</master_runspecfilename>");
			writer.println("\t<master_runspecdescription><![CDATA[" + masterFragment.runSpecDescription + "]]></master_runspecdescription>");
			writer.println("\t<master_runspecfiledatetime>" + masterFragment.runSpecFileDateTime + "</master_runspecfiledatetime>");
			writer.println("\t<master_rundatetime>" + masterFragment.runDateTime + "</master_rundatetime>");
			writer.println("\t<master_model>" + masterFragment.model + "</master_model>");
			writer.println("\t<master_scale>" + masterFragment.scale + "</master_scale>");
			writer.println("\t<master_defaultdatabaseused>" + masterFragment.defaultDatabaseUsed + "</master_defaultdatabaseused>");
			writer.println("\t<master_masterversiondate>" + masterFragment.masterVersionDate + "</master_masterversiondate>");
			writer.println("\t<master_mastercomputerid>" + masterFragment.masterComputerID + "</master_mastercomputerid>");
			writer.println("\t<master_masteridnumber>" + masterFragment.masterIDNumber + "</master_masteridnumber>");
			writer.println("\t<master_domain>" + masterFragment.domain + "</master_domain>");
			writer.println("\t<master_domaincountyid>" + masterFragment.domainCountyID + "</master_domaincountyid>");
			writer.println("\t<master_domaincountyname>" + masterFragment.domainCountyName + "</master_domaincountyname>");
			writer.println("\t<master_domaindatabaseserver>" + masterFragment.domainDatabaseServer + "</master_domaindatabaseserver>");
			writer.println("\t<master_domaindatabasename>" + masterFragment.domainDatabaseName + "</master_domaindatabasename>");
			writer.println("\t<master_expecteddonefiles>" + masterFragment.expectedDONEFiles + "</master_expecteddonefiles>");
			writer.println("\t<master_outputdatabasename>" + masterFragment.outputDatabaseName + "</master_outputdatabasename>");

			writer.println("\t<durations>");
			for(DurationFragment df : durationFragments) {
				writer.println("\t\t<durationfragment>");
				writer.println("\t\t\t<loopablename>" + df.loopableName + "</loopablename>");
				writer.println("\t\t\t<duration>" + df.durationSeconds + "</duration>");
				writer.println("\t\t</durationfragment>");
			}
			writer.println("\t</durations>");

			writer.println("</bundlemanifest>");
		} finally {
			if(writer != null) {
				try {
					writer.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
		return true;
	}

	/**
	 * Obtain the manifest File object
	 * @param folder folder to be hold the file
	 * @return a File object for the manifest
	**/
	public File getManifestFile(File folder) {
		return new File(folder,MANIFEST_FILE_NAME);
	}

	/**
	 * Determine if a folder contains a manifest file
	 * @param folder folder to be scanned
	 * @throws IOException if anything goes wrong
	 * @return true if the folder contains a manifest file
	**/
	public boolean containsManifest(File folder) throws IOException {
		return getManifestFile(folder).exists();
	}

	/**
	 * Read a manifest from folder
	 * @param folder folder holding a manifest file
	 * @throws Exception if anything goes wrong
	 * @return true if the file was read
	**/
	public boolean readFromFolder(File folder) throws Exception {
		File mf = getManifestFile(folder);
		if(!mf.exists()) {
			return false;
		}
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			db.setErrorHandler(new XMLErrorHandler());
			Document doc = db.parse(mf);
			// Verify that this is a bundle manifest xml file
			Node rootNode = doc.getFirstChild();
			if(!(rootNode != null && rootNode.getNodeName().equalsIgnoreCase("bundlemanifest"))) {
				/** @explain The manifest file is corrupt. **/
				Logger.log(LogMessageCategory.ERROR, "Invalid Bundle Manifest XML file.");
				return false;
			}
			// Handle each "sub-node" under the main <bundlemanifest> tag, these are immediate child nodes
			for(Node subNode = rootNode.getFirstChild(); subNode != null; subNode = subNode.getNextSibling()) {
				String nodeName = subNode.getNodeName();
				if(nodeName.equalsIgnoreCase("machinecontext")) {
					context = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("humancontext")) {
					contextForHumans = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("bundlename")) {
					bundleName = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("retrievetable")) {
					String t = getText(subNode);
					if(t != null && t.length() > 0) {
						tablesToRetrieve.add(t);
					}
				} else if(nodeName.equalsIgnoreCase("workerVersion")) {
					workerVersion = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("workerComputerID")) {
					workerComputerID = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("workerID")) {
					workerID = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("bundleNumber")) {
					bundleNumber = getInt(subNode);
				} else if(nodeName.equalsIgnoreCase("durationSeconds")) {
					durationSeconds = getFloat(subNode);
				} else if(nodeName.equalsIgnoreCase("destinationMasterComputerID")) {
					destinationMasterComputerID = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("destinationMasterIDNumber")) {
					destinationMasterIDNumber = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_outputTimePeriod")) {
					masterFragment.outputTimePeriod = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_timeUnits")) {
					masterFragment.timeUnits = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_distanceUnits")) {
					masterFragment.distanceUnits = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_massUnits")) {
					masterFragment.massUnits = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_energyUnits")) {
					masterFragment.energyUnits = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_runSpecFileName")) {
					masterFragment.runSpecFileName = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_runSpecDescription")) {
					masterFragment.runSpecDescription = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_runSpecFileDateTime")) {
					masterFragment.runSpecFileDateTime = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_runDateTime")) {
					masterFragment.runDateTime = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_model")) {
					masterFragment.model = getText(subNode); 
			    } else if(nodeName.equalsIgnoreCase("master_scale")) {
					masterFragment.scale = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_defaultDatabaseUsed")) {
					masterFragment.defaultDatabaseUsed = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_masterVersionDate")) {
					masterFragment.masterVersionDate = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_masterComputerID")) {
					masterFragment.masterComputerID = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_masterIDNumber")) {
					masterFragment.masterIDNumber = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_domain")) {
					masterFragment.domain = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_domainCountyID")) {
					masterFragment.domainCountyID = getInt(subNode);
				} else if(nodeName.equalsIgnoreCase("master_domainCountyName")) {
					masterFragment.domainCountyName = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_domainDatabaseServer")) {
					masterFragment.domainDatabaseServer = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_domainDatabaseName")) {
					masterFragment.domainDatabaseName = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("master_expectedDONEFiles")) {
					masterFragment.expectedDONEFiles = getInt(subNode);
				} else if(nodeName.equalsIgnoreCase("master_outputDatabaseName")) {
					masterFragment.outputDatabaseName = getText(subNode);
				} else if(nodeName.equalsIgnoreCase("durations")) {
					for(Node subNode2 = subNode.getFirstChild(); subNode2 != null; subNode2 = subNode2.getNextSibling()) {
						String nodeName2 = subNode2.getNodeName();
						if(nodeName2.equalsIgnoreCase("durationfragment")) {
							DurationFragment df = new DurationFragment();
							for(Node subNode3 = subNode2.getFirstChild(); subNode3 != null; subNode3 = subNode3.getNextSibling()) {
								String nodeName3 = subNode3.getNodeName();
								if(nodeName3.equalsIgnoreCase("loopablename")) {
									df.loopableName = getText(subNode3);
								} else if(nodeName3.equalsIgnoreCase("duration")) {
									df.durationSeconds = getFloat(subNode3);
								}
							}
							if(df.loopableName != null && df.loopableName.length() > 0) {
								durationFragments.add(df);
							}
						}
					}
				}
			}
		} finally {
			// Nothing to do here
		}
		return true;
	}

	/**
	 * Get text within a node.
	 * @param node XML node of the form <node>text</node>
	 * @return text within the node
	**/
	private static String getText(Node node) {
		Node childNode = node.getFirstChild();
		for(Node child = node.getFirstChild(); child != null; child = child.getNextSibling()) {
			if(child.getNodeType() == Node.TEXT_NODE || child.getNodeType() == Node.CDATA_SECTION_NODE) {
				return StringUtilities.safeGetString(childNode.getNodeValue());
			}
		}
		return "";
	}

	/**
	 * Get the float within a node.
	 * @param node XML node of the form <node>text</node>
	 * @return float within the node
	**/
	private static float getFloat(Node node) {
		String t = getText(node);
		if(t != null && t.length() > 0) {
			try {
				return (float)Double.parseDouble(t);
			} catch(Exception e) {
				// Nothing to do here
			}
		}
		return 0;
	}

	/**
	 * Get the integer within a node.
	 * @param node XML node of the form <node>text</node>
	 * @return integer within the node
	**/
	private static int getInt(Node node) {
		String t = getText(node);
		if(t != null && t.length() > 0) {
			try {
				return Integer.parseInt(t);
			} catch(Exception e) {
				// Nothing to do here
			}
		}
		return 0;
	}

	/**
	 * Internal class used to handle errors.
	**/
	public static class XMLErrorHandler implements ErrorHandler {
		/**
		 * Returns a string describing parse exception details
		 * @param spe The SAX Parse Exception that occurred.
		**/
		private String getParseExceptionInfo(SAXParseException spe) {
			String systemId = spe.getSystemId();
			if (systemId == null) {
				systemId = "null";
			}
			String info = "URI=" + systemId +
				" Line=" + spe.getLineNumber() +
				": " + spe.getMessage();
			return info;
		}

		/**
		 * Log SAX parser warning.
		 * @param spe The SAX Parse Exception that occurred.
		**/
		public void warning(SAXParseException spe) throws SAXException {
			/**
			 * @issue Warning: [SAX XML parser exception]
			 * @explain The RunSpec file is corrupt, likely due to a typo.
			**/
			Logger.log(LogMessageCategory.WARNING, "Warning: " + getParseExceptionInfo(spe));
		}

		/**
		 * Log SAX parser error.
		 * @param spe The SAX Parse Exception that occurred.
		**/
		public void error(SAXParseException spe) throws SAXException {
			String message = "Error: " + getParseExceptionInfo(spe);
			throw new SAXException(message);
		}

		/**
		 * Log SAX parser fatal error.
		 * @param spe The SAX Parse Exception that occurred.
		**/
		public void fatalError(SAXParseException spe) throws SAXException {
			String message = "Fatal Error: " + getParseExceptionInfo(spe);
			throw new SAXException(message);
		}
	}

	public static class ContextHolder {
		public int iterationID;
		public int processID;
		public int roadTypeRecordID;
		public int linkRecordID;
		public int zoneRecordID;
		public int countyRecordID;
		public int stateRecordID;
		public int year;
		public int monthID;
		public int dayID;
		public int hourID;
		public String executionGranularity = "";
		public int executionPriority;
		public boolean isCleanUp = false;
		public String loopableClassName = "";

		/**
		 * Fill this context from the machine-readable context within a BundleManifest
		 * @param context the machine-readable context within a BundleManifest
		**/
		public void fromBundleManifestContext(String context) {
			String[] parts = context.split("\\|");
			if(parts == null || parts.length <= 0) {
				return;
			}
			for(int i=0;i<parts.length;i++) {
				int index = parts[i].indexOf(":");
				if(index < 0) {
					continue;
				}
				String name = parts[i].substring(0,index);
				String valueText = parts[i].substring(index+1);
				int valueID = 0;
				if(StringUtilities.isDigits(valueText)) {
					try {
						valueID = Integer.parseInt(valueText);
					} catch(Exception e) {
						// Nothing to do here
					}
				}
				if(name.equalsIgnoreCase("iter")) {
					iterationID = valueID;
				} else if(name.equalsIgnoreCase("proc")) {
					processID = valueID;
				} else if(name.equalsIgnoreCase("road")) {
					roadTypeRecordID = valueID;
				} else if(name.equalsIgnoreCase("link")) {
					linkRecordID = valueID;
				} else if(name.equalsIgnoreCase("zone")) {
					zoneRecordID = valueID;
				} else if(name.equalsIgnoreCase("cty")) {
					countyRecordID = valueID;
				} else if(name.equalsIgnoreCase("st")) {
					stateRecordID = valueID;
				} else if(name.equalsIgnoreCase("y")) {
					year = valueID;
				} else if(name.equalsIgnoreCase("m")) {
					monthID = valueID;
				} else if(name.equalsIgnoreCase("d")) {
					dayID = valueID;
				} else if(name.equalsIgnoreCase("h")) {
					hourID = valueID;
				} else if(name.equalsIgnoreCase("gran")) {
					executionGranularity = valueText;
				} else if(name.equalsIgnoreCase("pri")) {
					executionPriority = valueID;
				} else if(name.equalsIgnoreCase("ph")) {
					isCleanUp = valueText.equalsIgnoreCase("clean");
				} else if(name.equalsIgnoreCase("class")) {
					loopableClassName = valueText;
				}
			}
			if(loopableClassName == null) {
				loopableClassName = "";
			}
		}
	}

	/**
	 * Record the occurence of an event related to this bundle's context.
	 * Use the duration encoded into the context.
	 * @param outputDB database connection to be used.
	 * @param isMaster true if the master is executing the code, false for workers
	 * @param runID identifier of the current run
	**/
	public void recordEvent(Connection outputDB, boolean isMaster, int runID) {
		if(durationFragments == null || durationFragments.size() <= 0) {
			recordEvent(outputDB,isMaster,runID,durationSeconds,null);
		} else {
			for(DurationFragment df : durationFragments) {
				recordEvent(outputDB,isMaster,runID,(float)df.durationSeconds,df.loopableName);
			}
		}
	}

	/**
	 * Remove common Java path prefixes from loopable names.
	 * This simplifies reporting to humans.
	 * @param contextText text containing a loopable name, among other items.
	 * loopable names must begin with a colon.
	**/
	static String simplifyLoopableName(String contextText) {
		String contextToUse = StringUtilities.replace(contextText,":gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.",":");
		contextToUse = StringUtilities.replace(contextToUse,":gov.epa.otaq.moves.master.implementation.ghg.",":");
		contextToUse = StringUtilities.replace(contextToUse,":gov.epa.otaq.moves.master.implementation.general.",":");
		contextToUse = StringUtilities.replace(contextToUse,":gov.epa.otaq.moves.master.implementation.",":");
		return contextToUse;
	}

	/**
	 * Record the occurence of an event related to this bundle's context.
	 * @param outputDB database connection to be used.
	 * @param isMaster true if the master is executing the code, false for workers
	 * @param runID identifier of the current run
	 * @param duration seconds require to complete the task being recorded
	 * @param loopableOverride optional name to provide detail, used instead of the overall context
	**/
	public void recordEvent(Connection outputDB, boolean isMaster, int runID, float duration, String loopableName) {
		if(!shouldRecordEvents) {
			return;
		}
		ContextHolder ch = new ContextHolder();
		String contextToUse = simplifyLoopableName(context);
		ch.fromBundleManifestContext(contextToUse);

		if(loopableName != null && loopableName.length() > 0) {
			ch.loopableClassName = simplifyLoopableName(":" + loopableName).substring(1);
		}

		String sql = "";
		try {
			sql = "insert into bundleTracking (MOVESRunID, hostType, loopableClassName, workerVersion, workerComputerID, workerID,"
					+ " bundleNumber, isCleanUp,"
					+ " iterationID, processID, roadTypeID, linkID,"
					+ " zoneID, countyID, stateID,"
					+ " yearID, monthID, dayID, hourID,"
					+ " executionGranularity, executionPriority, durationSeconds)"
					+ " values (" + runID
					+ ", " + (isMaster? "'M'":"'W'")
					+ ", " + DatabaseUtilities.escapeSQL(StringUtilities.substring(ch.loopableClassName,0,200),true)
					+ ", " + DatabaseUtilities.escapeSQL(StringUtilities.substring(workerVersion,0,100),true)
					+ ", " + DatabaseUtilities.escapeSQL(StringUtilities.substring(workerComputerID,0,20),true)
					+ ", " + DatabaseUtilities.escapeSQL(StringUtilities.substring(workerID,0,10),true)
					+ ", " + (isMaster? 0:bundleNumber)
					+ ", " + ((isMaster && ch.isCleanUp)?"'Y'":"'N'")
					+ ", " + ch.iterationID
					+ ", " + ch.processID
					+ ", " + ch.roadTypeRecordID
					+ ", " + ch.linkRecordID
					+ ", " + ch.zoneRecordID
					+ ", " + ch.countyRecordID
					+ ", " + ch.stateRecordID
					+ ", " + ch.year
					+ ", " + ch.monthID
					+ ", " + ch.dayID
					+ ", " + ch.hourID
					+ ", " + DatabaseUtilities.escapeSQL(StringUtilities.substring(ch.executionGranularity,0,10),true)
					+ ", " + ch.executionPriority
					+ ", " + duration
					+ ")";
			SQLRunner.executeSQL(outputDB,sql);
		} catch(Exception e) {
			Logger.logSqlError(e,"Cannot record event",sql);
		}
	}
}
