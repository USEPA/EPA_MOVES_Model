/**************************************************************************************************
 * @(#)AVFTToolSpec.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui.avfttool;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;

import java.io.File;
import java.sql.Connection;
import java.util.*;
import gov.epa.otaq.moves.master.implementation.ghg.internalcontrolstrategies.rateofprogress.*;

/**
 * A specification of all the parameters necessary to run the AVFT Tool.
**/
public class AVFTToolSpec {
    public String lastCompleteModelYear;
    public String analysisYear;

    public LinkedList<MethodEntry> methodEntries;

    public FileEntry inputAVFTFile;
    public FileEntry knownFractionsFile;
    public FileEntry outputAVFTFile;

	/** Constructor **/
	public AVFTToolSpec() {
        methodEntries = new LinkedList<MethodEntry>();
        inputAVFTFile = new FileEntry();
        knownFractionsFile = new FileEntry();
        outputAVFTFile = new FileEntry();
    }
}
