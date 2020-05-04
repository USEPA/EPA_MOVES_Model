/**************************************************************************************************
 * @(#)PDSpec.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import java.io.*;
import java.util.*;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.w3c.dom.*;

/**
 * Data for retrieving DONE files.
 *
 * @author		Wesley Faler
 * @version		2010-10-04
**/
public class PDSpec {
	public static class PDSpecEntry {
		public String masterID;
		public String pickupFolderName;
		public DatabaseSelection outputDatabase = new DatabaseSelection();

		public int howManyBundlesRetrieved = 0;

		public String toString() {
			return "ID " + masterID + " into " + outputDatabase.databaseName + " from " + pickupFolderName;
		}

		public boolean isWildcard() {
			return masterID.equalsIgnoreCase("*");
		}
	}

	/** Entries, each describing one master's DONE files to be retrieved and stored. **/
	public ArrayList<PDSpecEntry> entries = new ArrayList<PDSpecEntry>();

	/**
	 * Expand all PDSpecEntry objects that have wildcard master IDs then remove the wildcard entries,
	 * leaving a concrete set of PDSpecEntry objects to be executed.
	**/
	public void expandWildcards() {
		// Isolate wildcard entries
		ArrayList<PDSpecEntry> wildcards = null;
		for(int i=0;i<entries.size();i++) {
			PDSpecEntry entry = entries.get(i);
			if(entry.isWildcard()) {
				if(wildcards == null) {
					wildcards = new ArrayList<PDSpecEntry>();
				}
				wildcards.add(entry);
				entries.remove(i);
				i--;
			}
		}
		// Expand each wildcard entry
		TreeSetIgnoreCase idsAlreadySeen = new TreeSetIgnoreCase();
		for(int i=0;wildcards != null && i<wildcards.size();i++) {
			PDSpecEntry entry = wildcards.get(i);
			try {
				File pickupFolder = new File(entry.pickupFolderName);
				if(!pickupFolder.exists() || !pickupFolder.isDirectory()) {
					entries.add(entry); // will cause a failure display
					continue;
				}
				FilenameFilter doneFilter = DistributedWorkFileName.buildFileNameFilter(
						"*", "*", DistributedWorkFilePurpose.CALCULATOR, DistributedWorkFileState.DONE, "*");
				File[] doneFiles = null;
				doneFiles = pickupFolder.listFiles(doneFilter);
				if(doneFiles != null && doneFiles.length > 0) {
					for(int j=0;j<doneFiles.length;j++) {
						File doneFile = doneFiles[j];
						DistributedWorkFileName doneFileName = DistributedWorkFileName.createFrom(doneFile.getName());
						if(doneFileName != null && doneFileName.mid != null && doneFileName.mid.length() > 0) {
							if(!idsAlreadySeen.contains(doneFileName.mid)) {
								idsAlreadySeen.add(doneFileName.mid);
								PDSpecEntry newEntry = new PDSpecEntry();
								newEntry.masterID = doneFileName.mid;
								newEntry.pickupFolderName = entry.pickupFolderName;
								newEntry.outputDatabase = (DatabaseSelection)entry.outputDatabase.clone();
								entries.add(newEntry);
							}
						}
					}
				}
			} catch(Exception e) {
				// Nothing to do here
			}
		}
	}

	/**
	 * Generate an XML file from this PDSpec and its entries.
	 * @param xmlFile file to be created
	 * @throws Exception if anything goes wrong
	**/
	public void writeXML(File xmlFile) throws Exception {
		PrintWriter writer = null;
		try {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(xmlFile),32768));
			writer.println("<pdspec>");
			for(Iterator<PDSpecEntry> i=entries.iterator();i.hasNext();) {
				PDSpecEntry entry = i.next();
				writer.println("\t<pdentry>");
				writer.println("\t\t<masterid>" + entry.masterID + "</masterid>");
				writer.println("\t\t<directory>" + entry.pickupFolderName + "</directory>");
				writer.println("\t\t<output servername=\"" + entry.outputDatabase.serverName + "\" databasename=\"" + entry.outputDatabase.databaseName + "\" />");
				writer.println("\t</pdentry>");
			}
			writer.println("</pdspec>");
		} finally {
			if(writer != null) {
				try {
					writer.close();
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		}
	}

	/**
	 * Populate this PDSpec and its entries from an XML file.
	 * @param xmlFile file to be read
	 * @return true if the file was read
	 * @throws Exception if anything goes wrong
	**/
	public boolean readXML(File xmlFile) throws Exception {
		entries.clear();

		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			db.setErrorHandler(new XMLErrorHandler());
			Document doc = db.parse(xmlFile);
			// Verify that this is a bundle manifest xml file
			Node rootNode = doc.getFirstChild();
			if(!(rootNode != null && rootNode.getNodeName().equalsIgnoreCase("pdspec"))) {
				/** @explain The manifest file is corrupt. **/
				Logger.log(LogMessageCategory.ERROR, "Invalid PDSpec XML file.");
				return false;
			}
			// Handle each "sub-node" under the main <bundlemanifest> tag, these are immediate child nodes
			for(Node subNode = rootNode.getFirstChild(); subNode != null; subNode = subNode.getNextSibling()) {
				String nodeName = subNode.getNodeName();
				if(nodeName.equalsIgnoreCase("pdentry")) {
					PDSpecEntry entry = new PDSpecEntry();
					for(Node entrySubNode = subNode.getFirstChild(); entrySubNode != null; entrySubNode = entrySubNode.getNextSibling()) {
						nodeName = entrySubNode.getNodeName();
						if(nodeName.equalsIgnoreCase("masterid")) {
							entry.masterID = getText(entrySubNode);
						} else if(nodeName.equalsIgnoreCase("directory")) {
							entry.pickupFolderName = getText(entrySubNode);
						} else if(nodeName.equalsIgnoreCase("output")) {
							NamedNodeMap attributes = entrySubNode.getAttributes();
							for(int i = 0; i < attributes.getLength(); i++) {
								Node attributeNode = attributes.item(i);
								if(attributeNode.getNodeName().equalsIgnoreCase("servername")) {
									entry.outputDatabase.serverName = new String(attributeNode.getNodeValue());
								} else if(attributeNode.getNodeName().equalsIgnoreCase("databasename")) {
									entry.outputDatabase.databaseName = new String(attributeNode.getNodeValue());
								} else if(attributeNode.getNodeName().equalsIgnoreCase("description")) {
									entry.outputDatabase.description = new String(attributeNode.getNodeValue());
								}
							}
						}
					}
					entries.add(entry);
				}
			}
			return true;
		} finally {
			// Nothing to do here
		}
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
}
