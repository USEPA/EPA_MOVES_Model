/**************************************************************************************************
 * @(#)RunSpecXML.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui.avfttool;

import java.io.*;
import java.lang.Integer;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.w3c.dom.*;
import gov.epa.otaq.moves.common.*;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;


/**
 * Class for AVFT Tool Spec XML loading/saving
 * */
public class AVFTToolSpecXML {
	AVFTToolSpec spec;

    /** keeps track of the current row number when saving to an Excel tab **/
    int rowCounter;

	/** Constructor
	 * @param parent The AVFTToolSpec to load to and save from.
	**/
	public AVFTToolSpecXML(AVFTToolSpec parent) {
		spec = parent;
	}

    /**
	 * Loads from the XML file into the AVFTToolSpec.
	 * @param inFile The XML file to read from.
	 * @return True on success.
	**/
	public boolean load(File inFile) {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		try {
			DocumentBuilder db = dbf.newDocumentBuilder();
			db.setErrorHandler(new XMLErrorHandler());
			Document doc = db.parse(inFile);
			// Verify that this is a AVFTToolSpec xml file
			Node specNode = doc.getFirstChild();
			if(!(specNode != null && specNode.getNodeName().equalsIgnoreCase("AVFTTool"))) {
				Logger.log(LogMessageCategory.ERROR, "Invalid AVFTTool XML file.");
				return false;
			}
			// Handle each "sub-node" under the main <AVFTTool> tag, these are immediate child nodes
			for(Node subNode = specNode.getFirstChild(); subNode != null; subNode = subNode.getNextSibling()) {
                processSubNode(subNode);
			}
		} catch(SAXException e) {
			// If we get this exception, then we don't have a chance to read any fields
			Logger.logError(e,"Could not load AVFTTool XML.");
		} catch(Exception e) {
			// This indicates an error during one field's parsing
			Logger.logError(e,"Could not load AVFTTool XML");
			return false;
		}
		return true;
	}
    /**
	 * Saves from the AVFTToolSpec to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	public void save(PrintWriter printWriter) {
		try {
            // write opening tag
            printWriter.println("<AVFTTool version=\"" + gov.epa.otaq.moves.master.gui.MOVESWindow.MOVES_VERSION + "\">");
            // write body
            printLastCompleteModelYear(printWriter);
            printAnalysisYear(printWriter);
            printMethodEntries(printWriter);
            printInputAVFTFile(printWriter);
            printKnownFractionsInput(printWriter);
            printOutputAVFTFile(printWriter);
            // write closing tag
			printWriter.println("</AVFTTool>");
		} catch(Exception e) {
			Logger.logError(e,"Unable to save XML.");
		}
	}

    /**
	 * Saves from the AVFTToolSpec to the Excel file.
	 * @param XSSFSheet A XSSFSheet object that represents the destination tab in the Excel file.
	**/
	public void save(XSSFSheet sheet) {
		try {
            rowCounter = 0;
            // write opening tag
            XSSFRow row = sheet.createRow(rowCounter++);
            // write body
            row.createCell(0).setCellValue("<AVFTTool version=\"" + gov.epa.otaq.moves.master.gui.MOVESWindow.MOVES_VERSION + "\">");
            printLastCompleteModelYear(sheet);
            printAnalysisYear(sheet);
            printMethodEntries(sheet);
            printInputAVFTFile(sheet);
            printKnownFractionsInput(sheet);
            printOutputAVFTFile(sheet);
            // write closing tag
            row = sheet.createRow(rowCounter++);
			row.createCell(0).setCellValue("</AVFTTool>");
		} catch(Exception e) {
			Logger.logError(e,"Unable to save XML.");
		}
	}

	/**
	 * Handles a "sub-node" under the main <AVFTTool> tag, this is an immediate child node
	 * @param node A Node object as an immediate child node
	**/
	public void processSubNode(Node node) {
		String nodeName = node.getNodeName();
		if (nodeName.equalsIgnoreCase("LastCompleteModelYear")) {
			processLastCompleteModelYear(node);
		} else if (nodeName.equalsIgnoreCase("AnalysisYear")) {
			processAnalysisYear(node);
		} else if (nodeName.equalsIgnoreCase("MethodEntries")) {
			processMethodEntries(node);
		} else if (nodeName.equalsIgnoreCase("InputAVFTFile")) {
			processInputAVFTFile(node);
		} else if (nodeName.equalsIgnoreCase("KnownFractionsInput")) {
			processKnownFractionsInput(node);
		} else if (nodeName.equalsIgnoreCase("OutputAVFTFile")) {
			processOutputAVFTFile(node);
		} 
	}
	
	/**
	 * Writes the LastCompleteModelYear section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	private void printLastCompleteModelYear(PrintWriter printWriter) {
		printWriter.println(String.format("\t<LastCompleteModelYear key=\"%s\"/>", spec.lastCompleteModelYear));
	}
	
	/**
	 * Writes the LastCompleteModelYear section to the Excel file.
	 * @param XSSFSheet A XSSFSheet object that represents the destination tab in the Excel file.
	**/
	private void printLastCompleteModelYear(XSSFSheet sheet) {
        XSSFRow row = sheet.createRow(rowCounter++);
		row.createCell(1).setCellValue(String.format("<LastCompleteModelYear key=\"%s\"/>", spec.lastCompleteModelYear));
	}

	/**
	 * Reads the LastCompleteModelYear node of an XML file.
	 * @param node A Node object to be parsed.
	**/
    private void processLastCompleteModelYear(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
				spec.lastCompleteModelYear = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
		}
	}

	/**
	 * Writes the Analysis Year section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	private void printAnalysisYear(PrintWriter printWriter) {
		printWriter.println(String.format("\t<AnalysisYear key=\"%s\"/>", spec.analysisYear));
	}
	
	/**
	 * Writes the  section to the Excel file.
	 * @param XSSFSheet A XSSFSheet object that represents the destination tab in the Excel file.
	**/
	private void printAnalysisYear(XSSFSheet sheet) {
        XSSFRow row = sheet.createRow(rowCounter++);
		row.createCell(1).setCellValue(String.format("<AnalysisYear key=\"%s\"/>", spec.analysisYear));
	}

	/**
	 * Reads the AnalysisYear node of an XML file.
	 * @param node A Node object to be parsed.
	**/
    private void processAnalysisYear(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("key")) {
				spec.analysisYear = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
		}
	}

	/**
	 * Writes the Method Entries section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	private void printMethodEntries(PrintWriter printWriter) {
		printWriter.println("\t<MethodEntries>");
        for (MethodEntry entry : spec.methodEntries) {
            String xml = "\t\t<MethodEntry sourceTypeID=\"%d\" enabled=\"%b\" gapfilling=\"%s\" projection=\"%s\"/>";
            printWriter.println(String.format(xml, entry.sourceTypeID, entry.enabled, entry.gapFillingMethod, entry.projectionMethod));
        }
		printWriter.println("\t</MethodEntries>");
	}
	
	/**
	 * Writes the  section to the Excel file.
	 * @param XSSFSheet A XSSFSheet object that represents the destination tab in the Excel file.
	**/
	private void printMethodEntries(XSSFSheet sheet) {
        XSSFRow row = sheet.createRow(rowCounter++);
		row.createCell(1).setCellValue("<MethodEntries>");
        for (MethodEntry entry : spec.methodEntries) {
            row = sheet.createRow(rowCounter++);
            String xml = "<MethodEntry sourceTypeID=\"%d\" enabled=\"%b\" gapfilling=\"%s\" projection=\"%s\"/>";
            row.createCell(2).setCellValue(String.format(xml, entry.sourceTypeID, entry.enabled, entry.gapFillingMethod, entry.projectionMethod));
        }
        row = sheet.createRow(rowCounter++);
		row.createCell(1).setCellValue("</MethodEntries>");
	}
    
	/**
	 * Reads the MethodEntries node of an XML file.
	 * @param node A Node object to be parsed.
	**/
    private void processMethodEntries(Node node) {
        // MethodEntries contains children nodes
		for(Node subNode = node.getFirstChild(); subNode != null; subNode = subNode.getNextSibling()) {
			if (subNode.getNodeName().equalsIgnoreCase("MethodEntry")) {
				processMethodEntry(subNode);
            }
		}
	}
    
	/**
	 * Reads the MethodEntry node of an XML file.
	 * @param node A Node object to be parsed.
	**/
    private void processMethodEntry(Node node) {
        MethodEntry entry = new MethodEntry();
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("sourceTypeID")) {
				entry.sourceTypeID = Integer.parseInt(StringUtilities.safeGetString(attributeNode.getNodeValue()));
			}
			if(attributeNode.getNodeName().equalsIgnoreCase("enabled")) {
				entry.enabled = Boolean.parseBoolean(StringUtilities.safeGetString(attributeNode.getNodeValue()));
			}
			if(attributeNode.getNodeName().equalsIgnoreCase("gapfilling")) {
				entry.gapFillingMethod = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
			if(attributeNode.getNodeName().equalsIgnoreCase("projection")) {
				entry.projectionMethod = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
		}
        spec.methodEntries.add(entry);
	}

	/**
	 * Writes the Input AVFT section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	private void printInputAVFTFile(PrintWriter printWriter) {
		printWriter.println(String.format("\t<InputAVFTFile path=\"%s\" tab=\"%s\"/>", spec.inputAVFTFile.filePath, spec.inputAVFTFile.tabName));
	}
	
	/**
	 * Writes the  section to the Excel file.
	 * @param XSSFSheet A XSSFSheet object that represents the destination tab in the Excel file.
	**/
	private void printInputAVFTFile(XSSFSheet sheet) {
        XSSFRow row = sheet.createRow(rowCounter++);
		row.createCell(1).setCellValue(String.format("<InputAVFTFile path=\"%s\" tab=\"%s\"/>", spec.inputAVFTFile.filePath, spec.inputAVFTFile.tabName));
	}

	/**
	 * Reads the InputAVFTFile node of an XML file.
	 * @param node A Node object to be parsed.
	**/
    private void processInputAVFTFile(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("path")) {
				spec.inputAVFTFile.filePath = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
			if(attributeNode.getNodeName().equalsIgnoreCase("tab")) {
				spec.inputAVFTFile.tabName = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
		}
	}

	/**
	 * Writes the Known Fractions Input section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	private void printKnownFractionsInput(PrintWriter printWriter) {
		printWriter.println(String.format("\t<KnownFractionsInput path=\"%s\" tab=\"%s\"/>", spec.knownFractionsFile.filePath, spec.knownFractionsFile.tabName));
	}
	
	/**
	 * Writes the  section to the Excel file.
	 * @param XSSFSheet A XSSFSheet object that represents the destination tab in the Excel file.
	**/
	private void printKnownFractionsInput(XSSFSheet sheet) {
        XSSFRow row = sheet.createRow(rowCounter++);
		row.createCell(1).setCellValue(String.format("<KnownFractionsInput path=\"%s\" tab=\"%s\"/>", spec.knownFractionsFile.filePath, spec.knownFractionsFile.tabName));
	}

	/**
	 * Reads the KnownFractionsInput node of an XML file.
	 * @param node A Node object to be parsed.
	**/
    private void processKnownFractionsInput(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("path")) {
				spec.knownFractionsFile.filePath = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
			if(attributeNode.getNodeName().equalsIgnoreCase("tab")) {
				spec.knownFractionsFile.tabName = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
		}
	}

	/**
	 * Writes the Known Fractions Input section to the XML file.
	 * @param printWriter A PrintWriter object opened to the destination xml file.
	**/
	private void printOutputAVFTFile(PrintWriter printWriter) {
		printWriter.println(String.format("\t<OutputAVFTFile path=\"%s\"/>", spec.outputAVFTFile.filePath));
	}
	
	/**
	 * Writes the  section to the Excel file.
	 * @param XSSFSheet A XSSFSheet object that represents the destination tab in the Excel file.
	**/
	private void printOutputAVFTFile(XSSFSheet sheet) {
        XSSFRow row = sheet.createRow(rowCounter++);
		row.createCell(1).setCellValue(String.format("<OutputAVFTFile path=\"%s\"/>", spec.outputAVFTFile.filePath));
	}

	/**
	 * Reads the OutputAVFTFile node of an XML file.
	 * @param node A Node object to be parsed.
	**/
    private void processOutputAVFTFile(Node node) {
		NamedNodeMap attributes = node.getAttributes();
		for(int i = 0; i < attributes.getLength(); i++) {
			Node attributeNode = attributes.item(i);
			if(attributeNode.getNodeName().equalsIgnoreCase("path")) {
				spec.outputAVFTFile.filePath = StringUtilities.safeGetString(attributeNode.getNodeValue());
			}
		}
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
