/**************************************************************************************************
 * @(#)AVFTToolSpec.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui.avfttool;

/**
 * Tracks one file entry in the AVFTTool (input, known fractions, or output)
**/
public class FileEntry {
    public String filePath = "";
    public String tabName = "";
    
	/** Tracks one file entry in the AVFTTool (input, known fractions, or output), initializes as blank **/
	public FileEntry() {}

	/** Tracks one file entry in the AVFTTool (input, known fractions, or output), initializes filePath **/
	public FileEntry(String filePath) {
        this.filePath = (filePath != null) ? filePath : "";
        this.tabName = "";
    }

	/** Tracks one file entry in the AVFTTool (input, known fractions, or output), initializes all members **/
	public FileEntry(String filePath, String tabName) {
        this.filePath = (filePath != null) ? filePath : "";
        this.tabName = (tabName != null) ? tabName : "";
    }
}
