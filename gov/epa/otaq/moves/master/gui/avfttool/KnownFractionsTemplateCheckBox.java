/**************************************************************************************************
 * @(#)AVFTToolKnownFractionsTemplateCheckBox.java
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui.avfttool;


import javax.swing.JCheckBox;

/**
 * AVFTToolKnownFractionsTemplateCheckbox is a JCheckBox that also stores an associated SourceTypeID, FuelTypeID, and EngTechID
**/
public class KnownFractionsTemplateCheckBox extends JCheckBox {
    public int sourceTypeID;
    public int fuelTypeID;
    public int engTechID;

	public KnownFractionsTemplateCheckBox(int st, int ft, int engTech) {
        super();
		sourceTypeID = st;
        fuelTypeID = ft;
        engTechID = engTech;
	}
}
