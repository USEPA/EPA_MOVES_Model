/**************************************************************************************************
 * @(#)RunSpecSectionStatus.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import javax.swing.*;
import gov.epa.otaq.moves.master.runspec.RunSpec;

/**
 * Class for RunSpec section status (int values and their corresponding images).
 *
 * @author		Cimulus
 * @version		2004-07-07
**/
public class RunSpecSectionStatus implements Comparable {
	/** Constant value for the NOT READY status. **/
	public static final int NOT_READY = -1;
	/** Constant value for the DEFAULTS status. **/
	public static final int DEFAULTS = 0;
	/** Constant value for the OK status. **/
	public static final int OK = 1;
	/** Constant value for icons with a normal width **/
	public static final int NORMAL = 0;
	/** Constant value for icons with a wide width **/
	public static final int WIDE = 1;
	/** Constant value for icons with an embedded tree closed icon **/
	public static final int TREE_CLOSED = 2;
	/** Constant value for icons with an embedded tree open icon **/
	public static final int TREE_OPEN = 3;

	/** Constant image icon file and path for the NOT READY status. **/
	static ImageIcon notReadyImage = 
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/needsData.gif");
	/** Constant image icon file and path for the DEFAULTS status. **/
	static ImageIcon defaultsImage =
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/defaultData.gif");
	/** Constant image icon file and path for the OK status. **/
	static ImageIcon okImage =
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/filledIn.gif");

	/** Constant image icon file and path for the wide NOT READY status. **/
	static ImageIcon notReadyWideImage = 
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/needsDataWide.gif");
	/** Constant image icon file and path for the wide DEFAULTS status. **/
	static ImageIcon defaultsWideImage =
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/defaultDataWide.gif");
	/** Constant image icon file and path for the wide OK status. **/
	static ImageIcon okWideImage =
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/filledInWide.gif");

	/** Constant image icon file and path for the tree closed NOT READY status. **/
	static ImageIcon notReadyTreeClosedImage = 
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/needsDataTreeClosed.gif");
	/** Constant image icon file and path for the tree closed DEFAULTS status. **/
	static ImageIcon defaultsTreeClosedImage =
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/defaultDataTreeClosed.gif");
	/** Constant image icon file and path for the tree closed OK status. **/
	static ImageIcon okTreeClosedImage =
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/filledInTreeClosed.gif");

	/** Constant image icon file and path for the tree open NOT READY status. **/
	static ImageIcon notReadyTreeOpenImage = 
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/needsDataTreeOpen.gif");
	/** Constant image icon file and path for the tree open DEFAULTS status. **/
	static ImageIcon defaultsTreeOpenImage =
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/defaultDataTreeOpen.gif");
	/** Constant image icon file and path for the tree open OK status. **/
	static ImageIcon okTreeOpenImage =
			new ImageIcon("gov/epa/otaq/moves/master/gui/images/filledInTreeOpen.gif");

	/** The RunSpec Section Status. **/
	public int status;

	/**
	 * Obtain a human readable explaination for the status an icon is meant to convey
	 * @param icon icon to be explained
	 * @return status icon is meant to convey, never null.
	**/
	public static String explainIcon(Icon icon) {
		if(icon == notReadyImage || icon == notReadyWideImage
				|| icon == notReadyTreeClosedImage || icon == notReadyTreeOpenImage) {
			return "Not Ready";
		} else if(icon == defaultsImage || icon == defaultsWideImage
				|| icon == defaultsTreeClosedImage || icon == defaultsTreeOpenImage) {
			return "Ready but with Default Settings";
		} else if(icon == okImage || icon == okWideImage
				|| icon == okTreeClosedImage || icon == okTreeOpenImage) {
			return "Ready";
		}
		return "";
	}

	/** Constructor, sets a default status of NOT_READY. **/
	public RunSpecSectionStatus() {
		status = NOT_READY;
	}

	/**
	 * Constructor, sets the status to the specified int value. 
	 * @param	s the new value as int of the status.
	**/
	public RunSpecSectionStatus(int s) {
		status = s;
	}

	/** Sets the status to the best value (OK). **/
	public void makeBest() {
		status = OK;
	}

	/** Sets the status to the worst value (NOT_READY). **/
	public void makeWorst() {
		status = NOT_READY;
	}

	/**
	 * Sets the status to the other value, if it is better than the current status.
	 * @param	other the RunSpecSectionStatus value to compare the current status with.
	**/
	public void makeBestOfTwo(RunSpecSectionStatus other) {
		if(other.status > status) {
			status = other.status;
		}
	}

	/**
	 * Sets the status to the other value, if it is worse than the current status.
	 * @param	other the RunSpecSectionStatus value to compare the current status with.
	**/
	public void makeWorstOfTwo(RunSpecSectionStatus other) {
		if(other.status < status) {
			status = other.status;
		}
	}

	/**
	 * Returns the corresponding Icon for the current status.
	 * @return	Icon the corresponding Icon based on the current status, or null,
	 * if the status is invalid.
	 * @param iconSubType set to NORMAL, WIDE, TREE_OPEN, TREE_CLOSED
	**/
	public Icon getIcon(int iconSubType) {
		switch(status) {
			case NOT_READY:
				switch(iconSubType) {
					case NORMAL:
						return notReadyImage;
					case WIDE:
						return notReadyWideImage;
					case TREE_OPEN:
						return notReadyTreeOpenImage;
					case TREE_CLOSED:
						return notReadyTreeClosedImage;
				}
				break;
			case DEFAULTS:
				switch(iconSubType) {
					case NORMAL:
						return defaultsImage;
					case WIDE:
						return defaultsWideImage;
					case TREE_OPEN:
						return defaultsTreeOpenImage;
					case TREE_CLOSED:
						return defaultsTreeClosedImage;
				}
				break;
			case OK:
				switch(iconSubType) {
					case NORMAL:
						return okImage;
					case WIDE:
						return okWideImage;
					case TREE_OPEN:
						return okTreeOpenImage;
					case TREE_CLOSED:
						return okTreeClosedImage;
				}
				break;
		}
		return null;
	}

	/**
	 * Comparison routine used to sort these objects for display purposes.
	 * Compares status with the worst status' coming before the best ones.
	 * @param	other another RunSpecSectionStatus to compare to
	 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
	 * it should go after the other
	**/
	public int compareTo(Object other) {
		if(other instanceof RunSpecSectionStatus) {
			return status - ((RunSpecSectionStatus)(other)).status;
		} else {
			throw new ClassCastException();
		}
	}

	/**
	 * Comparison routine to test for equality.
	 * @param	other another RunSpecSectionStatus to compare to
	 * @return true if compareTo returns 0
	**/
	public boolean equals(Object other) {
		if(other instanceof RunSpecSectionStatus) {
			if(0 == compareTo(other)) {
				return true;
			}
		}
		return false;
	}
}
