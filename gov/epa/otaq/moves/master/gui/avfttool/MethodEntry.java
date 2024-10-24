/**************************************************************************************************
 * @(#)AVFTToolSpec.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui.avfttool;

/**
 * Tracks one entry in the AVFT Tool (regarding source type methods)
**/
public class MethodEntry {
    public int sourceTypeID = 0;
    public boolean enabled = true;
    public String gapFillingMethod = "";
    public String projectionMethod = "";

	/** Default constructor
	**/
	public MethodEntry() {}

	/** Constructor that initializes all members
	**/
	public MethodEntry(int sourceType, boolean isEnabled, String gapFilling, String projection) {
        sourceTypeID = sourceType;
        enabled = isEnabled;
        gapFillingMethod = gapFilling;
        projectionMethod = projection;
    }
    
}
