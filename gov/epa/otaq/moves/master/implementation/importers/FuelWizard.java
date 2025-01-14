/**************************************************************************************************
 * @(#)FuelWizard.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.importers;

import gov.epa.otaq.moves.master.framework.importers.*;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.gui.LayoutUtility;
import gov.epa.otaq.moves.master.gui.MOVESWindow;
import gov.epa.otaq.moves.master.implementation.general.RefineryModel;
import gov.epa.otaq.moves.master.runspec.MassMeasurementSystem;

/**
 * MOVES Fuels Wizard modal dialog.
 *
 * @author		Wesley Faler
 * @author		Don Smith
 * @author		John Covey (Task 2003)
 * @author		Mike Kender (Task 2003)
 * @version 	2020-08-13
**/
public class FuelWizard extends JDialog implements ActionListener, CellEditorListener, FocusListener, KeyListener {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** Current mode. 0 when prompting for fuels, 1 when asking to confirm fuel changes **/
	int mode = 0;
	/** Title label mode0 **/
	JLabel titleLabelMode0;
	/** Title label mode1 **/
	JLabel titleLabelMode1;
	/** list of what can be changed **/
	JLabel changeItemsLabel;
	ExtendedComboBox<String> changeableItems;
	/** Title to **/
	JLabel labelTo;
	/** value to change **/
	JTextField changeValue;
	/** units **/
	ExtendedComboBox<String> changeableUnits;

	/** Button to calculate new fuel properties **/
	JButton calculateButton;
	/** Button to end the dialog **/
	JButton doneButton;
	/** Button to accept new fuel properites, writing them to the database **/
	JButton acceptButton;
	/** Button to reject new fuel properties, reloading fuels from the database **/
	JButton rejectButton;
	/** DefaultListModel for the messageLogList. **/
	DefaultListModel<String> changeLogModel;
	/** Messages label **/
	JLabel messagesLabel;
	/** Message log component. **/
	JList<String> changeLogList;

	/** DefaultListModel for the messageLogList. **/
	DefaultListModel<String> messageLogModel;
	/** JScrollPane for the messageLogList. **/
	JScrollPane messageLogPane;
	/** Message log component. **/
	JList<String> messageLogList;

	/** Database to be used **/
	Connection db;
	/** Grid of available fuels **/
	JTable frozenTable;
	JTable frozenTableToChange;
	JTable availableFuelsTable;
	/** Grid of available fuels **/
	JTable fuelsToChangeTable;
	/** scroll pane so that fuels to change table can be scrolled **/
	JScrollPane fuelsToChangeScrollPane;
	/** scroll pane so that available fuels table can be scrolled **/
	JScrollPane availableFuelsScrollPane;
	/** List of available fuels **/
	ArrayList<RefineryModel.FuelFormulation> availableFuels = new ArrayList<RefineryModel.FuelFormulation>();
	/** Selection flags for available fuels **/
	Boolean[] availableFuelsSelection = null;
	/** Fuel changes pending and done **/
	RefineryModel.FuelChangeRequest fuelChangeRequest = new RefineryModel.FuelChangeRequest();
	/** True when using Nonroad fuel supply **/
	boolean isNonroad = false;

	/** Used to returns a blank, or null, renderer if a cell is empty **/
	class BlankRenderer extends DefaultTableCellRenderer {
		public Component getTableCellRendererComponent(JTable table, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			return null;
		}
	}

	/** Table able to show blank values. **/
	class TableWithBlanks extends JTable {
		/** Renderer returned if a cell is empty **/
		BlankRenderer blankRenderer;

		/**
		 * Constructor to set the Table Model for this table.
		 * @param model The table model used for this table.
		**/
		public TableWithBlanks(AbstractTableModel model) {
			super(model);
			blankRenderer = new BlankRenderer();
		}

		/**
		 * Overrides the cell renderer for empty cells by returning a blank renderer.
		 * @param row The row number for the cell.
		 * @param col The column number for the cell.
		 * @return The cell renderer.
		**/
		public TableCellRenderer getCellRenderer(int row,int col) {
			if(getModel().getValueAt(row,col) == null) {
				return blankRenderer;
			} else {
				return super.getCellRenderer(row,col);
			}
		}
	}

	/** Table to display the frozen table and process grid. **/
	class FrozenTable extends JTable {

		public boolean isFocusable()  {
			return true;
		}
		public TableCellRenderer getCellRenderer(int row,int col) {
				return super.getCellRenderer(row, col);
		}
	}

	/** Handle frozen table key actions **/
	class FrozenTableKeyAdapter extends KeyAdapter {
		JTable table;

		public FrozenTableKeyAdapter(JTable tableToUse) {
			table = tableToUse;
		}

		public void keyPressed(KeyEvent e) {
			int col = table.getSelectedColumn();
			int actualRow = table.getSelectedRow();
			
			if(e.getKeyCode() == KeyEvent.VK_SPACE && table.getName().equalsIgnoreCase("frozenTable")) {
				//toggle the checkbox
				int modelRow = table.convertRowIndexToModel(table.getSelectedRow());
				Boolean obj = (Boolean)table.getModel().getValueAt(modelRow, table.getSelectedColumn());
				SelectionTableModel model = (SelectionTableModel)table.getModel();
					model.setValueAt(!obj, modelRow, 0);
				((AbstractTableModel) table.getModel()).fireTableCellUpdated(modelRow, 0);
				table.requestFocus();
				table.changeSelection(actualRow, col, false, false);
			} else if(e.getKeyCode() == KeyEvent.VK_HOME) {
				//if home key (for all columns), move the focus to the beg of the frozen table
				table.requestFocus();
				table.changeSelection(table.getSelectedRow(), 0, false, false);
			} else if(col == 0 
					&& (e.getKeyCode() == KeyEvent.VK_LEFT 
						|| e.getKeyCode() == KeyEvent.VK_KP_LEFT 
						|| (e.getKeyCode() == KeyEvent.VK_TAB && e.isShiftDown()))) {
				//we are in the first column and shift/tab or left arrow is pressed - go to the availableFuelsTable table
				int row = table.getSelectedRow() == 0 ? table.getRowCount() - 1 : table.getSelectedRow() - 1;
				availableFuelsTable.requestFocus();
				availableFuelsTable.changeSelection(row, availableFuelsTable.getColumnCount() - 1, false, false);
			} else if (col == 0 
					&& ( (e.getKeyCode() == KeyEvent.VK_TAB && !e.isShiftDown())
							|| (e.getKeyCode() == KeyEvent.VK_RIGHT || e.getKeyCode() == KeyEvent.VK_KP_RIGHT) )) {

				int row = table.getSelectedRow() == table.getRowCount() - 1 ? 0 : table.getSelectedRow() + 1;
				availableFuelsTable.requestFocus();
				availableFuelsTable.changeSelection(table.getSelectedRow(), 0, false, false);
			}
		}
	}

	/** Handle available fuels table key actions **/
	class AvailableFuelsTableKeyAdapter extends KeyAdapter {
		JTable table;

		public AvailableFuelsTableKeyAdapter(JTable tableToUse) {
			table = tableToUse;
		}

		public void keyPressed(KeyEvent e) {
			int col = table.getSelectedColumn();

			if(e.getKeyCode() == KeyEvent.VK_END) {
				//if end key (for both columns), move the focus to the end of the availableFuelsTable table
				availableFuelsTable.requestFocus();
				availableFuelsTable.changeSelection(table.getSelectedRow(), availableFuelsTable.getColumnCount() - 1, false, false);
			} 
			else if(e.getKeyCode() == KeyEvent.VK_RIGHT && table.getColumnCount() == col + 1) {
				int row = table.getSelectedRow() == table.getRowCount() - 1 ? 0 : table.getSelectedRow() + 1;
				frozenTable.requestFocus();
				frozenTable.changeSelection(row, 0, false, false); 
			}
			else if(table.getSelectedColumn() == 0) {
				//only do these actions for the 1st column
				 if(e.getKeyCode() == KeyEvent.VK_TAB && e.isShiftDown()
						|| (e.getKeyCode() == KeyEvent.VK_LEFT || e.getKeyCode() == KeyEvent.VK_KP_LEFT) ) {
					frozenTable.setRequestFocusEnabled(true);
					frozenTable.requestFocus();
					frozenTable.changeSelection(table.getSelectedRow(), 0, false, false);
				}
			} 
			else if (e.isControlDown() && (e.getKeyCode() == KeyEvent.VK_ADD ||(e.isShiftDown() && e.getKeyCode() == KeyEvent.VK_EQUALS) )) {
				adjustColumn(table, col);
			}
		}
	}

	/** Handle fuels to change table key actions **/
	class FuelsToChangeTableKeyAdapter extends KeyAdapter {
		JTable table;

		public FuelsToChangeTableKeyAdapter(JTable tableToUse) {
			table = tableToUse;
		}

		public void keyPressed(KeyEvent e) {
			int col = table.getSelectedColumn();

			if (e.isControlDown() && (e.getKeyCode() == KeyEvent.VK_ADD ||(e.isShiftDown() && e.getKeyCode() == KeyEvent.VK_EQUALS) )) {
				adjustColumn(table, col);
			}

		}
	}
	
	/** Table model of a selection checkbox **/
	class SelectionTableModel extends AbstractTableModel {
		/**
		 * Get the description for an emission process column.
		 * @param col the emission process column number
		 * @return The column description
		**/
		public String getColumnName(int col) {
			switch(col) {
				case 0: // Selection flag
					return ( mode==0 )?"Select":"Status";
			}
			return "";
		}

		/**
		 * Get the number of rows in the table.
		 * @return The number of rows in the table
		**/
		public int getRowCount() {
			return (mode==0)?availableFuels.size():fuelChangeRequest.fuelsToChange.size()*2;
		}

		/**
		 * Get the number of columns in the table.
		 * @return The number of columns in the table
		**/
		public int getColumnCount() {
			return 1;
		}

		/**
		 * Get the value in a table cell.
		 * @param row The cell row number.
		 * @param col The cell column number.
		 * @return The walue stored in the table cell.
		**/
		public Object getValueAt(int row, int col) {
			// if mode0 availableFuels.get(row)
			// if mode1 (and even row/2)fuelsToChange (and odd row/2)changedFuels
			boolean isEven = (row%2) == 0;
			switch(col) {
				case 0: // selection flag, mode1==different data "old/new"
					if(0 == mode) {
						return availableFuelsSelection[row];
					} else {
						return isEven? "Old" : "New";
					}
			}
			return null;
		}

		/**
		 * Get the class, or type, of data in a column.
		 * @param col The cell column number.
		 * @return The column class
		**/
		public Class getColumnClass(int col) {
			switch(col) {
				case 0: // selection flag
					if(0 == mode) {
						return Boolean.class;
					} else {
						return String.class;
					}
			}
			return String.class;
		}

		/**
		 * Check if the cell is editable.
		 * @param row The cell row number.
		 * @param col The cell column number.
		 * @return True if the cell is editable.
		**/
		public boolean isCellEditable(int row,int col) {
			return (0 == mode && 0 == col)?true:false; // only edit the selection checkbox
		}

		/**
		 * Set the value of a table cell.
		 * @param value The value to put in the cell.
		 * @param row The cell row number.
		 * @param col The cell column number.
		**/
		public void setValueAt(Object value,int row,int col) {
			boolean didChange = false;
			switch(col) {
				case 0:
					if( 0 == mode ) {
						availableFuelsSelection[row] = (Boolean)value;
						didChange = true;
					}
					break;
				default:
					return;
			}
			if(didChange) {
				fireTableCellUpdated(row,col);
				assessSituation();
			}
		}
	}

	/** Table model to show available fuels **/
	class AvailableFuelsTableModel extends AbstractTableModel {
		/**
		 * Get the description for an emission process column.
		 * @param col the emission process column number
		 * @return The column description
		**/
		public String getColumnName(int col) {
			col++;
			switch(col) {
				case 0: // Selection flag
					return ( mode==0 )?"Select":"Status";
				case 1: // Region
					return "Region";
				case 2: // Fuel year
					return "Fuel Year";
				case 3: // Month group
					return "Month Group";
				case 4: // Fuel type
					return "Fuel Type";
				case 5: // RVP
					return "RVP";
				case 6: // Sulfur
					return "Sulfur";
				case 7: // Ethanol
					return "Ethanol";
				case 8: // T50
					return "T50";
				case 9: // T90
					return "T90";
				case 10: // Aromatics
					return "Aromatics";
				case 11: // Olefins
					return "Olefins";
				case 12: // Benzene
					return "Benzene";
				case 13: // E200
					return "E200";
				case 14: // E300
					return "E300";
				case 15: // BioDiesel Ester
					return "BioDiesel Ester";
				case 16: // CetaneIndex
					return "Cetane";
				case 17: // PAHContent
					return "PAH";
				case 18: // MTBE
					return "MTBE";
				case 19: // ETBE
					return "ETBE";
				case 20: // TAME
					return "TAME";
			}
			return "";
		}

		/**
		 * Get the number of rows in the table.
		 * @return The number of rows in the table
		**/
		public int getRowCount() {
			return (mode==0)?availableFuels.size():fuelChangeRequest.fuelsToChange.size()*2;
		}

		/**
		 * Get the number of columns in the table.
		 * @return The number of columns in the table
		**/
		public int getColumnCount() {
			return 21-1;
		}

		/**
		 * Get the value in a table cell.
		 * @param row The cell row number.
		 * @param col The cell column number.
		 * @return The walue stored in the table cell.
		**/
		public Object getValueAt(int row, int col) {
			col++;
			// if mode0 availableFuels.get(row)
			// if mode1 (and even row/2)fuelsToChange (and odd row/2)changedFuels
			boolean isEven = (row%2) == 0;
			RefineryModel.FuelFormulation f = null;
			if(0 == mode) {
				f = availableFuels.get(row); //mode1 == different data
			} else {
				int dataRow = row / 2;
				if(isEven) {
					f = fuelChangeRequest.fuelsToChange.get(dataRow);
				} else {
					f = fuelChangeRequest.changedFuels.get(dataRow);
				}
			}
			switch(col) {
				case 0: // selection flag, mode1==different data "old/new"
					if(0 == mode) {
						return availableFuelsSelection[row];
					} else {
						return isEven? "Old" : "New";
					}
				case 1: // Region
					return Integer.valueOf(f.regionID);
				case 2: // Fuel year
					return Integer.valueOf(f.fuelYearID);
				case 3: // Month group
					return Integer.valueOf(f.monthGroupID);
				case 4: // Fuel type
					return Integer.valueOf(f.fuelTypeID);
				case 5: // RVP
					return String.format("%.1f",f.RVP);
				case 6: // Sulfur
					return String.format("%.0f",f.sulfurLevel);
				case 7: // Ethanol
					return String.format("%.0f",f.ETOHVolume);
				case 8: // T50
					return String.format("%.2f",f.T50);
				case 9: // T90
					return String.format("%.2f",f.T90);
				case 10: // Aromatics
					return String.format("%.2f",f.aromaticContent);
				case 11: // Olefins
					return String.format("%.2f",f.olefinContent);
				case 12: // Benzene
					return String.format("%.2f",f.benzeneContent);
				case 13: // E200
					return String.format("%.2f",f.e200);
				case 14: // E300
					return String.format("%.2f",f.e300);
				case 15: // BioDiesel Ester
					return String.format("%.0f",f.BioDieselEsterVolume);
				case 16: // CetaneIndex
					return String.format("%.0f",f.CetaneIndex);
				case 17: // PAHContent
					return String.format("%.0f",f.PAHContent);
				case 18: // MTBE
					return String.format("%.2f",f.MTBEVolume);
				case 19: // ETBE
					return String.format("%.2f",f.ETBEVolume);
				case 20: // TAME
					return String.format("%.2f",f.TAMEVolume);
			}
			return null;
		}

		/**
		 * Get the class, or type, of data in a column.
		 * @param col The cell column number.
		 * @return The column class
		**/
		public Class getColumnClass(int col) {
			col++;
			// mode0, mode1
			switch(col) {
				case 0: // selection flag
					if(0 == mode) {
						return Boolean.class;
					} else {
						return String.class;
					}
				case 1: // Region
					return Integer.class;
				case 2: // Fuel year
					return Integer.class;
				case 3: // Month group
					return Integer.class;
				case 4: // Fuel type
					return Integer.class;
				case 5: // RVP
					return String.class;
				case 6: // Sulfur
					return String.class;
				case 7: // Ethanol
					return String.class;
				case 8: // T50
					return String.class;
				case 9: // T90
					return String.class;
				case 10: // Aromatics
					return String.class;
				case 11: // Olefins
					return String.class;
				case 12: // Benzene
					return String.class;
				case 13: // E200
					return String.class;
				case 14: // E300
					return String.class;
				case 15: // BioDiesel Ester
					return String.class;
				case 16: // CetaneIndex
					return String.class;
				case 17: // PAHContent
					return String.class;
				case 18: // MTBE
					return String.class;
				case 19: // ETBE
					return String.class;
				case 20: // TAME
					return String.class;
			}
			return String.class;
		}

		/**
		 * Check if the cell is editable.
		 * @param row The cell row number.
		 * @param col The cell column number.
		 * @return True if the cell is editable.
		**/
		public boolean isCellEditable(int row,int col) {
			col++;
			return (0 == mode && 0 == col)?true:false; // only edit the selection checkbox
		}

		/**
		 * Set the value of a table cell.
		 * @param value The value to put in the cell.
		 * @param row The cell row number.
		 * @param col The cell column number.
		**/
		public void setValueAt(Object value,int row,int col) {
			col++;
			boolean didChange = false;
			switch(col) {
				case 0:
					if( 0 == mode ) {
						availableFuelsSelection[row] = (Boolean)value;
						didChange = true;
					}
					break;
				default:
					return;
			}
			if(didChange) {
				fireTableCellUpdated(row,col);
				assessSituation();
			}
		}
	}

	// Show a table with a frozen left column.
	public class FrozenTablePane extends JScrollPane implements ChangeListener {
		public FrozenTablePane(JTable table, AbstractTableModel frozenModel, FrozenTable frozenTable) {
			super(table);

			frozenTable.setModel(frozenModel);
			frozenTable.addKeyListener(new FrozenTableKeyAdapter(frozenTable));
			frozenTable.setFocusable(true);
			frozenTable.setRowSelectionAllowed(false);
			table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

			//format the frozen table
			JTableHeader header = table.getTableHeader();
			frozenTable.setBackground(header.getBackground());
			frozenTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
			frozenTable.setEnabled(true);

			//set frozen table as row header view
			FontMetrics fm = table.getFontMetrics(table.getFont());
			javax.swing.table.TableColumnModel colModel = frozenTable.getColumnModel();
			String columnName = (String) colModel.getColumn(0).getHeaderValue();
			int columnSize = fm.stringWidth(columnName)+10;
			frozenTable.getColumn(columnName).setPreferredWidth(columnSize);
			frozenTable.setPreferredScrollableViewportSize(frozenTable.getPreferredSize());  
			setRowHeaderView(frozenTable);
			getRowHeader().addChangeListener(this);
			setCorner(JScrollPane.UPPER_LEFT_CORNER, frozenTable.getTableHeader());
		}

		/**
		 * Handles a state change event.
		 * @param	e The event caused by the state change.
		**/
		public void stateChanged(ChangeEvent e) {
			//Update table scroll position to match header scroll position
			JViewport viewport = (JViewport) e.getSource();
			getVerticalScrollBar().setValue(viewport.getViewPosition().y);
		}
	
		/**
		 * Currently not used.
		 * @param	e The event caused by the table edit.
		**/
		public void editingStopped(ChangeEvent e) {
			// Nothing to do here
		}
	
		/**
		 * Currently not used.
		 * @param	e The event caused by the table edit.
		**/
		public void editingCanceled(ChangeEvent e) {
			// Nothing to do here
		}
	}

	/**
	 * Constructs an Configure panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	 * @param dbToUse database that contains fuel formulations.
	 * @param isNonroadToUse True when using Nonroad fuel supply
	**/
	public FuelWizard(JFrame parent, Connection dbToUse, boolean isNonroadToUse) {
		super(parent, MOVESWindow.MOVES_VERSION + " - Fuels Wizard");
		frame = parent;
		db = dbToUse;
		isNonroad = isNonroadToUse;

		getContentPane().setLayout(new BorderLayout());
		changeMode(0);
		pack();
		setResizable(true); // false

		getRootPane().addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                // This is only called when the user releases the mouse button.
                //System.out.println("componentResized");
                if(0 == mode) {
                	if(availableFuelsTable != null) {
		                //System.out.println("availableFuels resized");
                		availableFuelsScrollPane.doLayout();
	                	availableFuelsTable.doLayout();
                	}
                } else if(1 == mode) {
                	if(fuelsToChangeTable != null) {
		                //System.out.println("fuelsToChange resized");
                		fuelsToChangeScrollPane.doLayout();
						fuelsToChangeTable.doLayout();
                	}
                }
            }
        });
	}

	/**
	 * Change the displayed panel and mode.
	 * @param newMode operation state of the dialog
	**/
	void changeMode(int newMode) {
		mode = newMode;
		if(0 == mode) {
			loadAvailableFuels();
		}
		getContentPane().removeAll();
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		assessSituation();
		pack();
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		pushDataToControls();
		pack();
		setModal(true);
//		(new WindowStateHandler(this)).setSizePositionAndStartTracking(-1,-1);
		setVisible(true);
	}

	/** Data from member variables to dialog controls (currently unused). **/
	void pushDataToControls() {
	}

	/**
	 * Creates and arranges all dialog controls.
	 * @return the container as JPanel.
	**/
	JPanel createPanel() {
		if(titleLabelMode0 == null) {
			createControls();
		}
		return arrangeControls();
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		titleLabelMode0 = new JLabel("Select fuels to modify");
		titleLabelMode0.setName("titleLabelMode0");

		changeItemsLabel = new JLabel("Change");
		changeItemsLabel.setName("changeItemsLabel");
		changeItemsLabel.setDisplayedMnemonic('C');
		changeableItems = new ExtendedComboBox<String>();
		changeItemsLabel.setLabelFor(changeableItems);
		changeableItems.setName("changeableItems");
		changeableItems.addActionListener(this);
		changeableItems.setEditable(false);
		changeableItems.addItem("");
		for(int i=0;i<RefineryModel.fuelPropertyNamesAndUnits.length;i+=3) {
			changeableItems.addItem(RefineryModel.fuelPropertyNamesAndUnits[i]);
		}
		changeableItems.setSelectedIndex(-1);
		ToolTipHelper.add(changeableItems,"Select fuel item");

		changeableUnits = new ExtendedComboBox<String>();
		changeableUnits.setName("changeableUnits");
		changeableUnits.addActionListener(this);
		changeableUnits.setEditable(false);
		changeableUnits.addItem("");
		changeableUnits.setSelectedIndex(-1);
		changeableUnits.setEnabled(false);
		ToolTipHelper.add(changeableUnits,"units");

		labelTo = new JLabel("to");
		labelTo.setName("labelTo");

		changeValue	= new JTextField("");
		changeValue.setName("changeValue");
		changeValue.setColumns(10);
		changeValue.setToolTipText(Constants.FUEL_WIZARD_CHANGE_VALUE_TOOLTIP);
		changeValue.addKeyListener(new KeyAdapter() {
            public void keyReleased(KeyEvent e) {
            	isChangeValueValid();
                assessSituation();
            }
        });
		
		changeValue.addFocusListener(this);
		changeValue.addKeyListener(this);

		titleLabelMode1 = new JLabel("Changes");
		titleLabelMode1.setName("titleLabelMode1");

		calculateButton = new JButton("Calculate >");
		calculateButton.setMnemonic('a');
		calculateButton.addActionListener(this);
		calculateButton.setName("calculateButton");
		ToolTipHelper.add(calculateButton,"Calculate new fuel properities.");

		doneButton = new JButton("Done");
		doneButton.setMnemonic('o');
		doneButton.addActionListener(this);
		doneButton.setName("doneButton");
		ToolTipHelper.add(doneButton,"Exit, all changes, if any, have already been saved.");

		acceptButton = new JButton("Accept >");
		acceptButton.setMnemonic('A'); 
		acceptButton.addActionListener(this);
		acceptButton.setName("acceptButton");
		ToolTipHelper.add(acceptButton,"Save changes to fuel properties.");

		rejectButton = new JButton("< Reject");
		rejectButton .setMnemonic('R');
		rejectButton.addActionListener(this);
		rejectButton.setName("rejectButton");
		ToolTipHelper.add(rejectButton,"Discard new fuel properties.");

		messagesLabel = new JLabel("Messages");
		messagesLabel.setName("messagesLabel");

		// Create message log model and object
		messageLogModel = new DefaultListModel<String>();
		messageLogList = new JListWithToolTips<String>(messageLogModel);
		messageLogList.setName("messageLogList");
		messageLogList.setSelectedIndex(-1);
		messageLogList.setVisibleRowCount(3);
		messageLogList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"); // XXXXXXXXXXXXXXXXXXXX
		messageLogPane = new JScrollPane(messageLogList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		messageLogPane.setName("messageLogPane");
		messageLogPane.setVisible(true);
		ToolTipHelper.add(messageLogPane,"Displays errors and warnings");
		messageLogPane.setMinimumSize(new Dimension(150, 150));

		int maxTableWidth = 900;
		int maxTableHeight = 600;

		availableFuelsTable = new TableWithBlanks(new AvailableFuelsTableModel());
		availableFuelsTable.setName("availableFuelsTable");
		availableFuelsTable.setRowSelectionAllowed(false);
		availableFuelsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		availableFuelsTable.getDefaultEditor(Boolean.class).addCellEditorListener(this);
		availableFuelsTable.addKeyListener(new AvailableFuelsTableKeyAdapter(availableFuelsTable));

		FontMetrics fm = availableFuelsTable.getFontMetrics(availableFuelsTable.getFont());
		javax.swing.table.TableColumnModel colModel = availableFuelsTable.getColumnModel();
		int numCols = colModel.getColumnCount();
		int tableHeight = (1*availableFuelsTable.getModel().getRowCount()) * availableFuelsTable.getRowHeight();
		int tableWidth = 0;
		int[] columnWidths = new int[numCols];
		for(int i=0; i<numCols; i++) {
			String columnName = (String) colModel.getColumn(i).getHeaderValue();
			if(i>=0) { // if not on the checkbox column
				TableColumn tableColumn = availableFuelsTable.getColumn(availableFuelsTable.getColumnName(i));
				int columnNumber = tableColumn.getModelIndex();
				int max = 20;
				int columnWidth = 0;
				int nrows = availableFuelsTable.getModel().getRowCount();
				String cell = "";
				int h = 0;
				for (int j=0; j<nrows; j++) {
					String value = availableFuelsTable.getModel().getValueAt(j, columnNumber).toString();
					columnWidth = fm.stringWidth("." + value.trim() + ".");
					if (columnWidth > max) {
						max = columnWidth;
					}
					h += availableFuelsTable.getRowHeight();
				}
				tableHeight = Math.max(h,tableHeight);
				max += 10;
				availableFuelsTable.getColumn(columnName).setPreferredWidth(max);
				tableWidth += max;
				columnWidths[i] = max;
			} else {
				int columnSize = 0;
				columnSize = fm.stringWidth(columnName)+10;
				availableFuelsTable.getColumn(columnName).setPreferredWidth(columnSize);
				tableWidth += columnSize;
				columnWidths[i] = columnSize;
			}
		}
		tableWidth = Math.min(tableWidth,maxTableWidth);
		tableHeight = Math.min(tableHeight,maxTableHeight);
		/*
		if(availableFuelsScrollPane != null) {
			availableFuelsScrollPane.getViewport().setPreferredSize(new Dimension(tableWidth, tableHeight));
			//availableFuelsTable.setPreferredSize(new Dimension(tableWidth, tableHeight));
		}
		//availableFuelsTable.getTableHeader().setResizingAllowed(false);
		*/
		availableFuelsTable.getTableHeader().setReorderingAllowed(false);
		//availableFuelsTable.setFillsViewportHeight(true);
		availableFuelsTable.setPreferredScrollableViewportSize(availableFuelsTable.getPreferredSize());
		ToolTipHelper.add(availableFuelsTable,"Available fuels and properties");

		frozenTable = new FrozenTable();
		frozenTable.setName("frozenTable");
		availableFuelsScrollPane = new FrozenTablePane(availableFuelsTable,new SelectionTableModel(),(FrozenTable) frozenTable);
		availableFuelsScrollPane.setName("availableFuelsScrollPane");


		fuelsToChangeTable = new TableWithBlanks(new AvailableFuelsTableModel());
		fuelsToChangeTable.setName("fuelsToChangeTable");
		fuelsToChangeTable.setRowSelectionAllowed(false);
		fuelsToChangeTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		fuelsToChangeTable.getDefaultEditor(Boolean.class).addCellEditorListener(this);
		fuelsToChangeTable.addKeyListener(new FuelsToChangeTableKeyAdapter(fuelsToChangeTable));
		fm = fuelsToChangeTable.getFontMetrics(fuelsToChangeTable.getFont());
		colModel = fuelsToChangeTable.getColumnModel();
		numCols = colModel.getColumnCount();
		for(int i=0; i<numCols; i++) {
			String columnName = (String) colModel.getColumn(i).getHeaderValue();
			fuelsToChangeTable.getColumn(columnName).setPreferredWidth(columnWidths[i]);
		}
		/*
		if(fuelsToChangeScrollPane != null) {
			fuelsToChangeScrollPane.getViewport().setPreferredSize(new Dimension(tableWidth, tableHeight));
			//fuelsToChangeTable.setPreferredSize(new Dimension(tableWidth, tableHeight));
		}
		//fuelsToChangeTable.getTableHeader().setResizingAllowed(false);
		*/
		fuelsToChangeTable.getTableHeader().setReorderingAllowed(false);
		//fuelsToChangeTable.setFillsViewportHeight(true);
		fuelsToChangeTable.setPreferredScrollableViewportSize(fuelsToChangeTable.getPreferredSize());
		ToolTipHelper.add(fuelsToChangeTable,"Available fuels and properties");

		frozenTableToChange = new FrozenTable();
		frozenTableToChange.setName("frozenTableToChange");

		fuelsToChangeScrollPane = new FrozenTablePane(fuelsToChangeTable,new SelectionTableModel(),(FrozenTable) frozenTableToChange);
		fuelsToChangeScrollPane.setName("fuelsToChangeScrollPane");
	}

	/**
	 * Sets the layout of the controls.
	 * @return the container as JPanel of the controls
	**/
	public JPanel arrangeControls() {
		if(0 == mode) { // If prompting the user to select fuels and a property...
			JPanel result = new JPanel();
			result.setLayout(new BoxLayout(result,BoxLayout.Y_AXIS));

			result.add(titleLabelMode0);
			result.add(availableFuelsScrollPane);

			JPanel bottomPanel = new JPanel();
			bottomPanel.setLayout(new GridBagLayout());
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.fill = GridBagConstraints.NONE;
			gbc.insets = new Insets(2,2,2,2);
			gbc.gridwidth = 5;
			gbc.gridheight = 2;
			gbc.weightx = 0;
			gbc.weighty = 0;

			LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
			bottomPanel.add(changeItemsLabel, gbc);
			LayoutUtility.setPositionOnGrid(gbc,1, 0, "WEST", 1, 1);
			bottomPanel.add(changeableItems, gbc);
			LayoutUtility.setPositionOnGrid(gbc,2, 0, "WEST", 1, 1);
			bottomPanel.add(labelTo, gbc);
			LayoutUtility.setPositionOnGrid(gbc,3, 0, "WEST", 1, 1);
			bottomPanel.add(changeValue, gbc);
			LayoutUtility.setPositionOnGrid(gbc,4, 0, "WEST", 1, 1);
			bottomPanel.add(changeableUnits, gbc);

			LayoutUtility.setPositionOnGrid(gbc,1, 1, "WEST", 1, 1);
			bottomPanel.add(doneButton, gbc);
			LayoutUtility.setPositionOnGrid(gbc,3, 1, "WEST", 1, 1);
			bottomPanel.add(calculateButton, gbc);

			Dimension d = bottomPanel.getPreferredSize();
			d.width = Short.MAX_VALUE;
			bottomPanel.setMaximumSize(d);

			result.add(bottomPanel);

			return result;
		} else if(1 == mode) { // If prompting the user to accept or reject fuel properties...
			JPanel result = new JPanel();
			result.setLayout(new BoxLayout(result,BoxLayout.Y_AXIS));

			result.add(titleLabelMode1);
			result.add(fuelsToChangeScrollPane);

			JPanel bottomPanel = new JPanel();
			bottomPanel.setLayout(new GridBagLayout());
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.fill = GridBagConstraints.NONE;
			gbc.insets = new Insets(2,2,2,2);
			gbc.gridwidth = 3;
			gbc.gridheight = 3;
			gbc.weightx = 0;
			gbc.weighty = 0;

			LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 3, 1);
			bottomPanel.add(messagesLabel, gbc);

			LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 3, 1);
			bottomPanel.add(messageLogPane, gbc);

			LayoutUtility.setPositionOnGrid(gbc,0, 2, "WEST", 1, 1);
			bottomPanel.add(rejectButton, gbc);
			LayoutUtility.setPositionOnGrid(gbc,2, 2, "WEST", 1, 1);
			bottomPanel.add(acceptButton, gbc);

			Dimension d = bottomPanel.getPreferredSize();
			d.width = Short.MAX_VALUE;
			bottomPanel.setMaximumSize(d);

			result.add(bottomPanel);

			return result;
		}
		return null;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == doneButton) {
			handleDoneButton();
		} else if(e.getSource() == calculateButton) {
			handleCalculateButton();
		} else if(e.getSource() == rejectButton) {
			handleRejectButton();
		} else if(e.getSource() == acceptButton) {
			handleAcceptButton();
		} else if(e.getSource() == changeableItems) {
			handlePropertyChange();
		}
	}

	/** Update the units for the chosen property **/
	void handlePropertyChange() {
		//System.out.println("handlePropertyChange");
		if (changeableItems.getSelectedIndex() > 0) {
			changeValue.setEnabled(true);
		}
		else {
			changeValue.setEnabled(false);
		}
		
		changeableUnits.removeAllItems();
		String property = StringUtilities.safeGetString(changeableItems.getSelectedItem());
		if(property.length() <= 0) {
			changeableUnits.addItem("");
			changeableUnits.setSelectedIndex(-1);
			return;
		}
		String units = "";
		for(int i=0;i<RefineryModel.fuelPropertyNamesAndUnits.length;i+=3) {
			if(property.equalsIgnoreCase(RefineryModel.fuelPropertyNamesAndUnits[i])) {
				units = RefineryModel.fuelPropertyNamesAndUnits[i+1];
				break;
			}
		}
		changeableUnits.addItem(units);
		if(units.equalsIgnoreCase("F")) {
			changeableUnits.addItem("C");
		}
		changeableUnits.setSelectedIndex(0);
		assessSituation();
	}

	/**
	 * Done button closed the dialog.
	**/
	void handleDoneButton() {
		dispose();
	}

	/**
	 * Calculation button handler.
	**/
	void handleCalculateButton() {
		// Populate the fuel change request
		fuelChangeRequest.resetOutput();
		fuelChangeRequest.fuelsToChange.clear();
		if(availableFuelsSelection != null) {
			for(int i=0;i<availableFuelsSelection.length;i++) {
				if(availableFuelsSelection[i] != null && availableFuelsSelection[i].booleanValue()) {
					fuelChangeRequest.fuelsToChange.add(availableFuels.get(i));
				}
			}
		}
		if(fuelChangeRequest.fuelsToChange.size() <= 0) {
			return;
		}

		fuelChangeRequest.propertyNameToChange = StringUtilities.safeGetString(changeableItems.getSelectedItem());
		fuelChangeRequest.units = StringUtilities.safeGetString(changeableUnits.getSelectedItem());

		if (!isChangeValueValid()) {
			return;
		}
		
		// Modify fuels
		RefineryModel model = new RefineryModel(db,isNonroad);
		model.changeFuels(fuelChangeRequest);
		// Show any resulting messages
		messageLogModel.clear();
		for(String m : fuelChangeRequest.messages) {
			messageLogModel.addElement(m);
		}
		// Change the wizard to show the results
		changeMode(1);
		rejectButton.requestFocus();
	}

	/**
	 * Reject button handler.
	**/
	void handleRejectButton() {
		changeMode(0);
	}

	/**
	 * Accept button handler.
	**/
	void handleAcceptButton() {
		RefineryModel model = new RefineryModel(db,isNonroad);
		model.saveFuels(fuelChangeRequest);
		changeMode(0);
	}

	/**
	 * Handles table editing stopped event.
	 * @param	e The event caused by the table edit.
	**/
	public void editingStopped(ChangeEvent e){
		// Nothing to do here
	}

	/**
	 * Currently not used.
	 * @param	e The event caused by the table edit.
	**/
	public void editingCanceled(ChangeEvent e) {
		// Nothing to do here
	}

	/** Enable/Disable controls based on selections **/
	void assessSituation() {
		if(0 == mode) { // If prompting to select a fuel
			boolean hasSelectedAFuel = false;
			if(availableFuelsSelection != null) {
				for(int i=0;i<availableFuelsSelection.length;i++) {
					if(availableFuelsSelection[i] != null && availableFuelsSelection[i].booleanValue()) {
						hasSelectedAFuel = true;
						break;
					}
				}
			}
			String property = StringUtilities.safeGetString(changeableItems.getSelectedItem());
			String units = StringUtilities.safeGetString(changeableUnits.getSelectedItem());
			String valueText = StringUtilities.safeGetString(changeValue.getText());
			boolean hasValue = false;
			try {
				double d = Double.parseDouble(valueText);
				if(d >= 0) {
					hasValue = true;
				}
			} catch(Exception e) {
				hasValue = false;
			}
			changeableUnits.setEnabled(property.length() > 0);
			calculateButton.setEnabled(hasSelectedAFuel && property.length() > 0
					&& units.length() > 0 && hasValue);
		} else if(1 == mode) { // If prompting to accept or reject new fuel properties
			// Nothing to do here.
		}
	}

	/** Load the fuel supply information from the current database **/
	void loadAvailableFuels() {
		availableFuels.clear();
		availableFuelsSelection = null;

		// Load the fuel supply and fuel formation tables from db
		RefineryModel model = new RefineryModel(db,isNonroad);
		model.loadFuels(availableFuels);

		// Setup selection information
		if(availableFuels.size() > 0) {
			availableFuelsSelection = new Boolean[availableFuels.size()];
			for(int i=0;i<availableFuelsSelection.length;i++) {
				availableFuelsSelection[i] = Boolean.FALSE;
			}
		}
	}

	/**
	 * Handles the focus lost event for the server textfield.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		assessSituation();
	}

	/**
	 * Currently not used.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
		// Nothing to do here
	}
	
	public void keyPressed(KeyEvent e) {
		assessSituation();
	}

	public void keyReleased(KeyEvent e) {
		assessSituation();
	}

	public void keyTyped(KeyEvent e) {
		assessSituation();
	}
	
	/*
	 * Adjust the width of the specified column in the table
	 */
	public void adjustColumn(JTable table, final int column) {
		TableColumn tableColumn = table.getColumnModel().getColumn(column);
		if (!tableColumn.getResizable())
			return;

		int columnHeaderWidth = getColumnHeaderWidth(table, column);
		int columnDataWidth = getColumnDataWidth(table, column);
		int preferredWidth = Math.max(columnHeaderWidth, columnDataWidth);

		updateTableColumn(table, column, preferredWidth);
	}

	/*
	 * Calculated the width based on the column name
	 */
	private int getColumnHeaderWidth(JTable table, int column) {

		TableColumn tableColumn = table.getColumnModel().getColumn(column);
		Object value = tableColumn.getHeaderValue();
		TableCellRenderer renderer = tableColumn.getHeaderRenderer();

		if (renderer == null) {
			renderer = table.getTableHeader().getDefaultRenderer();
		}

		Component c = renderer.getTableCellRendererComponent(table, value, false, false, -1, column);
		return c.getPreferredSize().width;
	}

	/*
	 * Calculate the width based on the widest cell renderer for the given
	 * column.
	 */
	private int getColumnDataWidth(JTable table, int column) {

		int preferredWidth = 0;
		int maxWidth = table.getColumnModel().getColumn(column).getMaxWidth();

		for (int row = 0; row < table.getRowCount(); row++) {
			preferredWidth = Math.max(preferredWidth, getCellDataWidth(table, row, column));

			// We've exceeded the maximum width, no need to check other rows

			if (preferredWidth >= maxWidth)
				break;
		}

		return preferredWidth;
	}

	/*
	 * Get the preferred width for the specified cell
	 */
	private int getCellDataWidth(JTable table, int row, int column) {
		// Inovke the renderer for the cell to calculate the preferred width

		TableCellRenderer cellRenderer = table.getCellRenderer(row, column);
		Component c = table.prepareRenderer(cellRenderer, row, column);
		int width = c.getPreferredSize().width + table.getIntercellSpacing().width;

		return width;
	}

	/*
	 * Update the TableColumn with the newly calculated width
	 */
	private void updateTableColumn(JTable table, int column, int width) {
		final TableColumn tableColumn = table.getColumnModel().getColumn(column);

		if (!tableColumn.getResizable())
			return;

		width = Math.max(width, tableColumn.getPreferredWidth()) + table.getIntercellSpacing().width;

		table.getTableHeader().setResizingColumn(tableColumn);
		tableColumn.setWidth(width);
	}

	/**
	 * Checks the text in changeValue to ensure it is a valid number as a Double value.
	 * 
	 * @return is false if changeValue text cannot be converted to a Double.
	 */
	private boolean isChangeValueValid() {
		if (changeValue.getText().length() > 0) {
			try {
				fuelChangeRequest.targetValue = Double.parseDouble(changeValue.getText());
			} catch (Exception ex) {
				JOptionPane.showMessageDialog(this,
						"Invalid fuel property specified for change value: " + changeValue.getText());
				return false;
			}
		}
		return true;
	}
}
