package gov.epa.otaq.moves.master.gui;

import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.SwingConstants;

/**
 *  3 way CheckBox.  The 3 states are Full, Partial and Unselected.
 *
 * @author		Mike Kender (Task 2003)
 * @version     2020-07-28
**/
public class TCheckBox extends JCheckBox {
	private static final String IMAGES_FOLDER = "images/";
	private static final String SELECTED = "selected.png";
	private static final String HALF_SELECTED = "halfselected.png";
	private static final String UNSELECTED = "unselected.png";
	private static final String FOCUS = "focus_";
	
	private boolean focus = false;

	public TCheckBox() {
		this("", null);
		
		setFocusable(true);
	}
	public TCheckBox(String text) {
		this(text, null);
	}

	public TCheckBox(String text, Icon icon) {
		super(text, icon);
	}
	
	public boolean booleanValue() {
		return isSelected();
	}
	
	@Override
	public void setSelected(boolean b) {
		super.setSelected(b);
	}

	public void setFullySelected() {
		setIcon(new javax.swing.ImageIcon(TCheckBox.class.getResource(IMAGES_FOLDER + (focus ? FOCUS : "") + SELECTED)));
		setHorizontalAlignment(SwingConstants.CENTER);
	}

	public void setPartialSelected() {
		setIcon(new javax.swing.ImageIcon(TCheckBox.class.getResource(IMAGES_FOLDER + (focus ? FOCUS : "") + HALF_SELECTED)));
		setHorizontalAlignment(SwingConstants.CENTER);
	}

	public void setUnselected() {
		setIcon(new javax.swing.ImageIcon(TCheckBox.class.getResource(IMAGES_FOLDER + (focus ? FOCUS : "") + UNSELECTED)));
		setHorizontalAlignment(SwingConstants.CENTER);
	}

	public void setFocus(boolean focus) {
		this.focus = focus;
	}
}