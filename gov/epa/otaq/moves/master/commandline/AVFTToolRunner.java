/**************************************************************************************************
 * @(#)AVFTToolRunner.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.commandline;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.regex.*;

import javax.swing.JOptionPane;

import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.gui.avfttool.AVFTTool;
import gov.epa.otaq.moves.master.gui.avfttool.AVFTToolSpec;
import gov.epa.otaq.moves.master.gui.avfttool.AVFTToolSpecXML;
import gov.epa.otaq.moves.common.*;

/**
 * Batch mode running AVFT Tool.
**/
public class AVFTToolRunner {
	/** Invokes the main application. **/
	public static void main(String[] args) {
		Configuration.allowGUI = false;

		if(!SystemConfiguration.getTheSystemConfiguration().didLoad) {
			System.exit(1);
		}

		// Find command line arguments
		String specArg = "";
        String inputAVFTdb = "";
        String outputAVFTdb = "";
		for(int i=0;i<args.length;i++) {
			String t = args[i].toLowerCase();
			if(t.startsWith("-spec")) {
				specArg = getOptionData(args[i]);
			}
			if(t.startsWith("-inputavftdb")) {
				inputAVFTdb = getOptionData(args[i]);
			}
			if(t.startsWith("-outputavftdb")) {
				outputAVFTdb = getOptionData(args[i]);
			}
		}
		
		// parse -spec arg
		if(specArg == null || specArg.length() <= 0) {
			Logger.log(LogMessageCategory.ERROR, "Missing -spec argument");
			System.exit(1);
		}
        File specFile = new File(specArg);
        if (!specFile.isFile()) {
			Logger.log(LogMessageCategory.ERROR, "File not found: " + specArg);
			System.exit(1);
        }
        AVFTToolSpec spec = new AVFTToolSpec();
		AVFTToolSpecXML newSpecXML = new AVFTToolSpecXML(spec);
        if (!newSpecXML.load(specFile)) {
			Logger.log(LogMessageCategory.ERROR, "Error reading " + specArg);
			System.exit(1);
        }

        // check for same input/output database error condition
        if(!inputAVFTdb.equals("") && inputAVFTdb.equals(outputAVFTdb)) {
			Logger.log(LogMessageCategory.ERROR, "Cannot specify the same database for both input and output: " + inputAVFTdb);
			System.exit(1);
        }

        // do the thing
        AVFTTool avftTool = new AVFTTool(null);
        avftTool.setGUISelectionsFromSpec(spec);
        avftTool.setInputAVFTdb(inputAVFTdb);
        avftTool.setOutputAVFTdb(outputAVFTdb);
        boolean success = avftTool.handleRunAVFTToolButton();
		if (success) {
            String successMsg = "Results saved to " + spec.outputAVFTFile.filePath;
            if(!outputAVFTdb.equals("")) {
                successMsg += " and " + outputAVFTdb + ".avft";
            }
            Logger.log(LogMessageCategory.INFO, successMsg);
            System.exit(0);
        } else {
            System.exit(1);
        }
	}

	/**
	 * Get the data portion of an option of the form "-option=data".
	 * @param optionText full option text
	 * @return data portion of the option, never null but may be blank
	**/
	static String getOptionData(String optionText) {
		int index = optionText.indexOf('=');
		if(index < 0) {
			return "";
		}
		return StringUtilities.substring(optionText,index+1).trim();
	}


}
