/**************************************************************************************************
 * @(#)PollutantsAndProcesses.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextPane;
import javax.swing.JViewport;
import javax.swing.LookAndFeel;
import javax.swing.RowFilter;
import javax.swing.SwingConstants;
import javax.swing.ToolTipManager;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;
import javax.swing.UIManager;

import gov.epa.otaq.moves.common.CompilationFlags;
import gov.epa.otaq.moves.common.Constants;
import gov.epa.otaq.moves.common.JListWithToolTips;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import gov.epa.otaq.moves.common.ModelScale;
import gov.epa.otaq.moves.common.Models;
import gov.epa.otaq.moves.common.ToolTipHelper;
import gov.epa.otaq.moves.master.framework.DatabaseConnectionManager;
import gov.epa.otaq.moves.master.framework.EmissionProcess;
import gov.epa.otaq.moves.master.framework.Pollutant;
import gov.epa.otaq.moves.master.framework.PollutantDisplayGroup;
import gov.epa.otaq.moves.master.framework.PollutantProcessAssociation;
import gov.epa.otaq.moves.master.framework.PollutantProcessLoader;
import gov.epa.otaq.moves.master.framework.RoadType;
import gov.epa.otaq.moves.master.implementation.ghg.NRAirToxicsCalculator;
import gov.epa.otaq.moves.master.implementation.ghg.TOGSpeciationCalculator;
import gov.epa.otaq.moves.master.nonroad.NonroadEmissionCalculator;
import gov.epa.otaq.moves.master.runspec.OutputTimeStep;
import gov.epa.otaq.moves.master.runspec.RunSpec;

/**
 * Class for MOVES PollutantsAndProcesses panel. The panel contains a table of
 * pollutants and processes. The pollutants and the processes are retrieved
 * from the database. The pollutants will be the row labels, and the processes
 * will be the column labels of the grid. The selections are loaded/saved from/to
 * the RunSpec.
 *
 * @author		Wesley Faler
 * @author		Sarah Luo
 * @author		EPA Mitch C.
 * @author      Gwo Shyu, EPA
 * @author		Tim Hull
 * @author  	Bill Shaw (508 compliance mods)
 * @author  	Mike Kender (508 compliance changes - task 1810)
 * @author		John Covey (Task 2003)
 * @author		Mike Kender (Task 2003)
 * @version     2020-07-28
**/
public class PollutantsAndProcesses extends JPanel implements RunSpecEditor, CellEditorListener, ChangeListener, ActionListener {
	/** pollutantsTable displays the tristate checkbox and the pollutant name columns  **/
	PollutantsTable pollutantsTable;
	/** processesTable displays all other columns **/
	ProcessesTable processesTable;

	/** Data model for pollutants and processes table **/
	PollutantsAndProcessesTableModel model;
	/** Sorter and filterer for processesTable (prevents checkboxes from getting sorted and allows rows to be hidden/shown) **/
	TableRowSorter<PollutantsAndProcessesTableModel> sorter;
	/** Sorter and filterer for pollutantsTable (prevents checkboxes from getting sorted and allows rows to be hidden/shown) **/
	TableRowSorter<PollutantsAndProcessesTableModel> sorter2;
	/** Persist the user's expansion of groups **/
	TreeSet<Integer> expandedGroups = new TreeSet<Integer>();

	/** scroll pane so that table can be scrolled **/
	JScrollPane tableScrollPane;

	/** Array of EmissionProcess objects **/
	Object[] processes;
	/** Mapping of which index in the processes array each EmissionProcess object is **/
	TreeMap<EmissionProcess,Integer> processIndexes;
	/** Array of Pollutant Display Group objects **/
	//Object[] pollutantDisplayGroups;
	/** Mapping of which index in the pollutants array each Pollutant object is **/
	TreeMap<Pollutant,Integer> pollutantIndexes;
	/** User selections.  First index is by pollutant, second (rightmost) by process **/
	Object[][] selections;
	/** Row selections, one per rowEntry **/
	Boolean[] rowSelections;
	/** Message log component. **/
	JList<String> messageLogList;
	/** DefaultListModel for the messageLogList. **/
	DefaultListModel<String> messageLogModel;
	/** JScrollPane for the messageLogList. **/
	JScrollPane messageLogPane;
	/** Select Prerequisites button **/
	JButton selectPrerequisistes;
	/** Clear All button **/
	JButton clearAll;

	/** split pane for table and message log   **/
	JSplitPane splitPane;

	/** Used to resave pollutant-processes that were in runspec loaded
	    but were not used **/
	TreeSet<PollutantProcessAssociation> undisplayedRunSpecSelections = new TreeSet<PollutantProcessAssociation>();

	/** 
	 * true to force pollutants and processes to be reloaded. Used after
	 * a change between Onroad and Nonroad.
	**/
	boolean forcePollutantProcessReload = false;

	/** One row in the pollutant/process grid **/
	static class RowEntry implements Comparable {
		public PollutantDisplayGroup group;
		public Pollutant pollutant;
		public String name = null;
		public boolean isVisible = true;
		public boolean couldBeVisible = true;
		public boolean childrenAreVisible = true;
		public boolean hasChildren = false;
		public RowEntry parent = null;
		public int row = 0;

		public String toString() {
			String name = "";
			if(pollutant == null) {
				name = (childrenAreVisible?" [-] ":" [+] ") + group.displayGroupName;
			} else if(group.pollutantIDsInDisplayGroup.size() == 1) {
				name = "  " + pollutant.pollutantName;
			} else {
				name = "      " + pollutant.pollutantName;
			}
			return name;
		}

		/**
		 * Comparable Interface Implementation. Provides Tree container ordering and display
		 * order.
		 * @param other PollutantDisplayGroup object to compare.
		 * @return <0 if this one should go before the other, 0 if they are the same, and >0 if
		 * it should go after the other
		**/
		public int compareTo(Object other) {
			RowEntry otherEntry = (RowEntry)other;
			int t = 0;
			if(group == null && otherEntry.group != null) {
				return -1;
			} else if(group != null && otherEntry.group == null) {
				return +1;
			} else if(group != null && otherEntry.group != null) {
				t = group.compareTo(otherEntry.group);
				if(t != 0) {
					return t;
				}
			}
			if(pollutant == null && otherEntry.pollutant != null) {
				return -1;
			} else if(pollutant != null && otherEntry.pollutant == null) {
				return +1;
			} else if(pollutant != null && otherEntry.pollutant != null) {
				//return pollutant.compareTo(otherEntry.pollutant);
				//return pollutant.databaseKey - otherEntry.pollutant.databaseKey;
				return pollutant.pollutantName.compareToIgnoreCase(otherEntry.pollutant.pollutantName);
			}
			return 0;
		}
	}
	/** All rows in the pollutant/process grid **/
	Object[] rowEntries;

	/** Handle clicks on pollutant/process checkboxes **/
	class ProcessesTableMouseAdapter extends MouseAdapter {
		JTable table;

		public ProcessesTableMouseAdapter(JTable tableToUse) {
			table = tableToUse;
		}

		public void mouseClicked(MouseEvent e) {
			Point p = e.getPoint();
			table.requestFocus();
			table.changeSelection(table.rowAtPoint(p), table.columnAtPoint(p), false, false);
		}
	}

	/** Handle pollutant table key actions **/
	class ProcessesTableKeyAdapter extends KeyAdapter {
		JTable table;

		public ProcessesTableKeyAdapter(JTable tableToUse) {
			table = tableToUse;
		}

		public void keyPressed(KeyEvent e) {
			int col = table.getSelectedColumn();
			int lvc = ((ProcessesTable)table).getLastVisibleColumn();
			int actualRow = table.getSelectedRow();

			if (e.getKeyCode() == KeyEvent.VK_F1 && e.isControlDown()) {
				JComponent component = (JComponent) e.getSource();
				Rectangle r = ((ProcessesTable) table).getCellRect(actualRow, col, true);
				MouseEvent phantom = new MouseEvent(component, MouseEvent.MOUSE_MOVED, System.currentTimeMillis(), 0,
						r.x, r.y, 0, false);

				ToolTipManager ttManager = ToolTipManager.sharedInstance();
				try {
					Field f = ttManager.getClass().getDeclaredField("tipShowing");
					f.setAccessible(true);

					boolean tipShowing = f.getBoolean(ttManager);
					if (tipShowing) {
						ToolTipManager.sharedInstance().mouseMoved(phantom);
					}

				} catch (Exception ex) {
					Logger.logError(ex, "Failed to display tooltip from CTRL+F1");
				}
			}

			else if(e.getKeyCode() == KeyEvent.VK_SPACE) {
				//toggle the checkbox
				int modelRow = table.convertRowIndexToModel(table.getSelectedRow());
				Boolean obj = (Boolean)processesTable.getModel().getValueAt(modelRow, table.getSelectedColumn() + 2);
				if(obj == null) {
					obj = false;
				}
				model.setValueAt(!obj, modelRow, table.getSelectedColumn() + 2);
				model.fireTableCellUpdated(modelRow, table.getSelectedColumn() + 2);
				MOVESNavigation.singleton.updateRunSpecSectionStatus();
				processesTable.requestFocus();
				processesTable.changeSelection(actualRow, col, false, false);
			} else if(e.getKeyCode() == KeyEvent.VK_HOME) {
				//if home key (for all columns), move the focus to the beg of the pollutantsTable table
				pollutantsTable.requestFocus();
				pollutantsTable.changeSelection(table.getSelectedRow(), 0, false, false);
			} else if( (e.getKeyCode() == KeyEvent.VK_LEFT 
						|| e.getKeyCode() == KeyEvent.VK_KP_LEFT 
						|| (e.getKeyCode() == KeyEvent.VK_TAB && e.isShiftDown()))) {
				if(col == 0) {
					//we are in the first column and shift/tab or left arrow is pressed - go to the pollutants table
					pollutantsTable.requestFocus();
					pollutantsTable.changeSelection(table.getSelectedRow(), pollutantsTable.getColumnCount() - 1, false, false);
				} else {
					//just go to the previous column
					int viewCol = ((ProcessesTable)table).getNextLeftColumn(col);
					processesTable.requestFocus();
					processesTable.changeSelection(actualRow, viewCol, false, false);
				}
			} else if ( (e.getKeyCode() == KeyEvent.VK_TAB && !e.isShiftDown())
							|| (e.getKeyCode() == KeyEvent.VK_RIGHT || e.getKeyCode() == KeyEvent.VK_KP_RIGHT) ) {
				//trying to go right
				if(col == /*table.getColumnCount() - 1*/ lvc) {
					//we are in the last column and tab - go the next row's pollutants table
					int row = table.getSelectedRow() == table.getRowCount() - 1 ? 0 : table.getSelectedRow() + 1;
					pollutantsTable.requestFocus();
					pollutantsTable.changeSelection(row, 0, false, false);
				} else {
					//just go to the next column
					int viewCol = ((ProcessesTable)table).getNextRightColumn(col);
					processesTable.requestFocus();
					processesTable.changeSelection(actualRow, viewCol, false, false);
				}
			}
		}
	}

	/** Handle pollutant table key actions **/
	class PollutantsTableKeyAdapter extends KeyAdapter {
		JTable table;

		public PollutantsTableKeyAdapter(JTable tableToUse) {
			table = tableToUse;
		}
		
		public void keyReleased(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_CONTROL && pollutantsTable.getSelectedRow() == -1 &&  pollutantsTable.getSelectedColumn() == -1) {
	            pollutantsTable.requestFocus();
	            pollutantsTable.changeSelection(0, 0, false, false);
			}
		}

		public void keyPressed(KeyEvent e) {
			int actualRow = table.getSelectedRow();
			int col = table.getSelectedColumn();

			if (e.getKeyCode() == KeyEvent.VK_F1 && e.isControlDown()) {
				JComponent component = (JComponent) e.getSource();
				Rectangle r = ((PollutantsTable) table).getCellRect(actualRow, col, true);
				MouseEvent phantom = new MouseEvent(component, MouseEvent.MOUSE_MOVED, System.currentTimeMillis(), 0,
						r.x, r.y, 0, false);
				ToolTipManager ttManager = ToolTipManager.sharedInstance();
				try {
					Field f = ttManager.getClass().getDeclaredField("tipShowing");
					f.setAccessible(true);
					boolean tipShowing = f.getBoolean(ttManager);
					if (tipShowing) {
						ToolTipManager.sharedInstance().mouseMoved(phantom);
					}
				} catch (Exception ex) {
					Logger.logError(ex, "Failed to display tooltip from CTRL+F1");
				}
			} else if(e.getKeyCode() == KeyEvent.VK_END) {
				//if end key (for both columns), move the focus to the end of the processes table
				processesTable.requestFocus();
				processesTable.changeSelection(table.getSelectedRow(), processesTable.getColumnCount() - 1, false, false);
			} else if(table.getSelectedColumn() == 0) {
				//only do these actions for the 1st column
				if(e.getKeyCode() == KeyEvent.VK_SPACE) {
					//toggle the checkbox
					int modelRow = table.convertRowIndexToModel(table.getSelectedRow());
					Boolean obj = (Boolean)pollutantsTable.getModel().getValueAt(modelRow, 0);
					model.setValueAt(!obj, modelRow, 0);
					model.fireTableCellUpdated(modelRow, 0);
					MOVESNavigation.singleton.updateRunSpecSectionStatus();
					pollutantsTable.requestFocus();
					pollutantsTable.changeSelection(actualRow, 0, false, false);
				} else if(e.getKeyCode() == KeyEvent.VK_TAB && e.isShiftDown()
						|| (e.getKeyCode() == KeyEvent.VK_LEFT || e.getKeyCode() == KeyEvent.VK_KP_LEFT) ) {
					//need to go to the end of the row above
					int row = table.getSelectedRow() == 0 ? table.getRowCount() - 1 : table.getSelectedRow() - 1;
					processesTable.requestFocus();
					processesTable.changeSelection(row, processesTable.getLastVisibleColumn(), false, false);
				}
			} else if(table.getSelectedColumn() == 1) {
				//only do these actions for the 2nd column
				if(e.getKeyCode() == KeyEvent.VK_SPACE) {
					//if space, expand/contract
					expandContract(table, table.getSelectedRow());
				} else if(e.getKeyCode() == KeyEvent.VK_RIGHT 
						|| e.getKeyCode() == KeyEvent.VK_KP_RIGHT
						|| (e.getKeyCode() == KeyEvent.VK_TAB && !e.isShiftDown()) ) {
						//shift is not pressed, go forwards
						selectFirstProcessesColumn(table.getSelectedRow());
				}
			}
		}
		
		
	}
	
	private void selectFirstProcessesColumn(int row) {
		processesTable.requestFocus();
		processesTable.changeSelection(row, 0, false, false);
	}

	/** A support function for PollutantsTableMouseAdapter, used to hide/show pollutant groups **/
	private void expandContract(JTable table, int viewRow) {
		int modelRow = table.convertRowIndexToModel(viewRow);
		if(modelRow >= 0 && modelRow < rowEntries.length) {
			RowEntry re = (RowEntry)rowEntries[modelRow];
			if(re.hasChildren) {
				re.childrenAreVisible = !re.childrenAreVisible;
				Integer groupKey = Integer.valueOf(re.group.displayGroupID);
				if(re.childrenAreVisible) {
					expandedGroups.add(groupKey);
				} else {
					expandedGroups.remove(groupKey);
				}
				TableModel model = table.getModel();
				if(model instanceof PollutantsAndProcessesTableModel) {
					((PollutantsAndProcessesTableModel)model).fireTableCellUpdated(modelRow,1);
				}
				for(modelRow++;modelRow<rowEntries.length;modelRow++) {
					RowEntry childRow = (RowEntry)rowEntries[modelRow];
					if(childRow.group != re.group) {
						break;
					}
					childRow.isVisible = re.childrenAreVisible;
				}
				updateFilters();
			}
		}
	}

	/** Handle double clicks on pollutant groups, hiding/showing their children as needed **/
	class PollutantsTableMouseAdapter extends MouseAdapter {
		JTable table;

		public PollutantsTableMouseAdapter(JTable tableToUse) {
			table = tableToUse;
		}

		public void mouseClicked(MouseEvent e) {
			Point p = e.getPoint();
			int viewColumn = table.columnAtPoint(p);
			int modelColumn = table.convertColumnIndexToModel(viewColumn);

			if(e.getComponent().isEnabled() && e.getButton() == MouseEvent.BUTTON1 && e.getClickCount() >= 1 && modelColumn == 1) {
				expandContract(table, table.rowAtPoint(p));
			}

			if(modelColumn == 0) {
				table.requestFocus();
				table.changeSelection(table.rowAtPoint(p), 0, false, false);
			}
		}
	}

	/** Renderer for a blank or null cell, if a cell is empty **/
	class BlankRenderer extends DefaultTableCellRenderer {
		public Component getTableCellRendererComponent(JTable table, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			return super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
		}
	}
	
	/** Renderer for an expandable pollutant name cell **/
	class ExpandableRenderer extends DefaultTableCellRenderer {
	    private final Border SAFE_NO_FOCUS_BORDER = new EmptyBorder(1, 1, 1, 1);
	    private final Border DEFAULT_NO_FOCUS_BORDER = new EmptyBorder(1, 1, 1, 1);

	    @Override
		public boolean isFocusable() {
			return true;
		}

		public Component getTableCellRendererComponent(JTable table, int row, int column) {
			return this;
		}

		public Component getTableCellRendererComponent(JTable table, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {

			setText(value.toString());

			if(value.toString().indexOf("[+]") > -1) {
				setToolTipText("Click to expand row for " + value.toString().substring(4));
			} else if(value.toString().indexOf("[-]") > -1) {
				setToolTipText("Click to collapse row for " + value.toString().substring(4));
			}

			if (hasFocus) {
	            Border border = null;
	            if (isSelected) {
	                border = UIManager.getBorder("Table.focusSelectedCellHighlightBorder");
	            }
	            if (border == null) {
	                border = UIManager.getBorder("Table.focusCellHighlightBorder");
	            }
	            setBorder(border);

	            if (!isSelected && table.isCellEditable(row, column)) {
	                Color col;
	                col = UIManager.getColor("Table.focusCellForeground");
	                if (col != null) {
	                    super.setForeground(col);
	                }
	                col = UIManager.getColor("Table.focusCellBackground");
	                if (col != null) {
	                    super.setBackground(col);
	                }
	            }
	        } else {
	            setBorder(getNoFocusedBorder());
	        }

			return this;
		}

	    private Border getNoFocusedBorder() {
	        Border border = UIManager.getBorder("Table.cellNoFocusBorder");
			if (border != null) {
				return border;
			}
			return SAFE_NO_FOCUS_BORDER;
	    }
	}
	
	/** Renderer for a tristate checkbox cell **/
	class TriStateRenderer extends DefaultTableCellRenderer {
		public TriStateRenderer() {
			super();
			setHorizontalAlignment(SwingConstants.CENTER);
		}

		@Override
		public boolean isFocusable() {
			return true;
		}

		public Component getTableCellRendererComponent(JTable table, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {

			int modelRow = pollutantsTable.convertRowIndexToModel(row);
			
			int total = 0;
			int totalTrue = 0;

			Object[] bools = selections[modelRow];

			for(Object o : bools) {
				Boolean b = (Boolean)o;
				if(b != null) {
					total++;
					if(b) {
						totalTrue++;
					}
				}
			}

			TCheckBox tcb = new TCheckBox();
			
			tcb.setFocus(hasFocus);

			if(totalTrue == total) {
				//if all are true, show fully selected
				tcb.setFullySelected();
				tcb.setToolTipText((String) pollutantsTable.getModel().getValueAt(pollutantsTable.convertRowIndexToModel(row), 1) 
						+ Constants.POLLUTANT_CHECKBOX_FULLY_CHECKED_TOOLTIP);
			} else if(totalTrue == 0) {
				//none are selected, show unselected
				tcb.setUnselected();
				tcb.setToolTipText((String) pollutantsTable.getModel().getValueAt(pollutantsTable.convertRowIndexToModel(row), 1) 
						+ Constants.POLLUTANT_CHECKBOX_NOT_CHECKED_TOOLTIP);

			} else {
				//some, but not all - show partial
				tcb.setPartialSelected();
				tcb.setToolTipText((String) pollutantsTable.getModel().getValueAt(pollutantsTable.convertRowIndexToModel(row), 1) 
						+ Constants.POLLUTANT_CHECKBOX_PARTIALLY_CHECKED_TOOLTIP);
			}
			
			setHorizontalAlignment(SwingConstants.CENTER);

			return tcb;
		}
	}
	
	/** Renderer for column headers **/
	class MultiLineTableHeaderRenderer extends JTextPane implements TableCellRenderer {
		public MultiLineTableHeaderRenderer() {
			setEditable(false);
			setOpaque(false);
			setFocusable(false);
			LookAndFeel.installBorder(this, "TableHeader.cellBorder");

			StyledDocument doc = getStyledDocument();
			SimpleAttributeSet center = new SimpleAttributeSet();
			StyleConstants.setAlignment(center, StyleConstants.ALIGN_CENTER);
			doc.setParagraphAttributes(0, doc.getLength(), center, false);
		}

		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {
			if(((String)value).equals("Auxiliary Power Exhaust")) {
				value = "Other Hotelling Exhaust"; // renaming for GUI use because this process actually represents more than just APUs when running MOVES
			}
			int width = table.getColumnModel().getColumn(column).getWidth();
			setText((String) value);
			setSize(width, (int) (getPreferredSize().height * 1.3));
			return this;
		}
	}

	/** Table model to display the pollutant and process grid. **/
	class PollutantsAndProcessesTableModel extends AbstractTableModel {
		/**
		 * Get the description for an emission process column.
		 * @param col the emission process column number
		 * @return The column description
		**/
		public String getColumnName(int col) {
			if(col <= 1) {
				return "";
			} else if(processes != null) {
				return ((EmissionProcess)processes[col-2]).processName;
			} else {
				return "";
			}
		}

		public EmissionProcess getEmissionProcess(int col) {
			return ((EmissionProcess) processes[col - 2]);
		}

		public Pollutant getPollutant(int row) {
			if (rowEntries != null) {
				return ((RowEntry) rowEntries[row]).pollutant;
			} else {
				return null;
			}
		}

		/**
		 * Get the number of rows in the table.
		 * @return The number of rows in the table
		**/
		public int getRowCount() {
			if(rowEntries != null) {
				return rowEntries.length;
			} else {
				return 0;
			}
		}

		/**
		 * Get the number of columns in the table.
		 * @return The number of columns in the table
		**/
		public int getColumnCount() {
			if(processes != null) {
				return 2+processes.length;
			} else {
				return 0;
			}
		}

		/**
		 * Get the value in a table cell.
		 * @param row The cell row number.
		 * @param col The cell column number.
		 * @return The value stored in the table cell.
		**/
		public Object getValueAt(int row, int col) {
			if(col < 1) {
				return rowSelections[row];
			} else if(col == 1) {
				return rowEntries[row].toString();
			} else {
				return selections[row][col-2];
			}
		}

		/**
		 * Get the class, or type, of data in a column.
		 * @param col The cell column number.
		 * @return The column class
		**/
		public Class getColumnClass(int col) {
			if(col < 1) {
				return Boolean.class;
			} else if(col == 1) {
				return String.class;
			} else {
				return Boolean.class;
			}
		}

		/**
		 * Check if the cell is editable.
		 * @param row The cell row number.
		 * @param col The cell column number.
		 * @return True if the cell is editable.
		**/
		public boolean isCellEditable(int row,int col) {
			if(col < 1) { // If it is the select all button for a pollutant, allow it to be edited
				return true;
			} else if(col == 1) { // If it is the pollutant name, don't allow it to be edited
				return false;
			} else { // Don't allow editing of non-existant combinations
				Object value = getValueAt(row,col);
				if(value != null) {
					return true;
				} else {
					return false;
				}
			}
		}

		/**
		 * Ensure all checkboxes for an entry's parent group are set properly.
		 * @param row the cell row number that changed
		 * @param col the cell column number that changed
		**/
		void recalculateParent(int row, int col) {
			RowEntry re = (RowEntry)rowEntries[row];
			// Find the parent
			int parentRow = row - 1;
			RowEntry parent = null;
			for(;parentRow >= 0;parentRow--) {
				parent = (RowEntry)rowEntries[parentRow];
				if(parent.group == re.group && parent.hasChildren) {
					break;
				}
			}
			Boolean b = null;
			boolean hasAnyProcess = false;
			int processCount = processes != null? processes.length : 0;
			for(int c=0;c<processCount;c++) {
				if(selections[parentRow][c] != null) {
					boolean hasAnyPollutant = false;
					for(int r=parentRow+1;r<rowEntries.length;r++) {
						RowEntry childRow = (RowEntry)rowEntries[r];
						if(childRow.group == re.group) {
							b = (Boolean)selections[r][c];
							if(b != null && b.booleanValue()) {
								hasAnyPollutant = true;
								hasAnyProcess = true;
								break;
							}
						}
					}
					b = (Boolean)selections[parentRow][c];
					if(b.booleanValue() != hasAnyPollutant) {
						selections[parentRow][c] = Boolean.valueOf(hasAnyPollutant);
						fireTableCellUpdated(parentRow,c+2);
					}
				}
			}
			if(rowSelections[parentRow].booleanValue() != hasAnyProcess) {
				rowSelections[parentRow] = Boolean.valueOf(hasAnyProcess);
				fireTableCellUpdated(parentRow,0);
			}
		}

		/**
		 * Set the value of a table cell.
		 * @param value The value to put in the cell.
		 * @param row The cell row number.
		 * @param col The cell column number.
		**/
		public void setValueAt(Object value,int row,int col) {
			if(col < 1) { // If the select all button has been clicked
				if(!(value instanceof Boolean)) {
					return;
				}
				Models.ModelCombination mc;
				if (MOVESNavigation.singleton != null
						&& MOVESNavigation.singleton.parent != null
						&& MOVESNavigation.singleton.parent.runSpec != null) {
					mc = MOVESNavigation.singleton.parent.runSpec.getModelCombination();
				} else {
					mc = Models.ModelCombination.M0;
				}

				Boolean rowValue = (Boolean)value;
				rowSelections[row] = rowValue;
				RowEntry re = (RowEntry)rowEntries[row];
				int processCount = processes != null? processes.length : 0;
				for(int c=0;c<processCount;c++) {
					if(selections[row][c] == null) {
						continue;
					}
					Boolean b = (Boolean)selections[row][c];
					if(b != null && b.booleanValue() != rowValue.booleanValue()) {
						boolean processApplies = false;
						switch(mc) {
							case M1: // Onroad
								processApplies = ((EmissionProcess)processes[c]).isAffectedByOnroad;
								break;
							case M2: // Nonroad
								processApplies = ((EmissionProcess)processes[c]).isAffectedByNonroad;
								break;
						}
						if(!rowValue.booleanValue() || processApplies) {
							selections[row][c] = Boolean.valueOf(rowValue.booleanValue());
							fireTableCellUpdated(row,c+2);
							// If the row clicked upon was a group with children, apply the setting to all child rows now
							if(re.hasChildren) {
								for(int r=row+1;r<rowEntries.length;r++) {
									RowEntry childRow = (RowEntry)rowEntries[r];
									if(childRow.group == re.group) {
										b = (Boolean)selections[r][c];
										if(b != null && b.booleanValue() != rowValue.booleanValue()) {
											selections[r][c] = Boolean.valueOf(rowValue.booleanValue());
											fireTableCellUpdated(r,c+2);
											if(rowSelections[r].booleanValue() != rowValue.booleanValue()) {
												rowSelections[r] = rowValue;
												fireTableCellUpdated(r,0);
											}
										}
									}
								}
							}
						}
					}
				}
				// If the row clicked upon was not a group with children, but is a child, recalculate the parent's entries
				if(!re.hasChildren && re.group.pollutantIDsInDisplayGroup.size() > 1) {
					recalculateParent(row,col);
				}
			} else {
				if(isCellEditable(row,col)) {
					RowEntry re = (RowEntry)rowEntries[row];
					selections[row][col-2] = value;
					fireTableCellUpdated(row,col);

					// Update the row header
					if(value instanceof Boolean) {
						Boolean rowValue = (Boolean)value;
						if(rowValue.booleanValue()) {
							// If something in the row was turned on, then flag the row's
							// checkbox as on.
							if(!rowSelections[row].booleanValue()) {
								rowSelections[row] = Boolean.TRUE;
								fireTableCellUpdated(row,0);
							}
						} else {
							// If something in the row was turned off, then turn off the row's
							// checkbox if everything in the row is off.
							boolean hasAnythingMarked = false;
							int processCount = processes != null? processes.length : 0;
							for(int c=0;c<processCount;c++) {
								if(selections[row][c] == null) {
									continue;
								}
								Boolean b = (Boolean)selections[row][c];
								if(b != null && b.booleanValue()) {
									hasAnythingMarked = true;
									break;
								}
							}
							if(hasAnythingMarked != rowSelections[row].booleanValue()) {
								rowSelections[row] = Boolean.valueOf(hasAnythingMarked);
								fireTableCellUpdated(row,0);
							}
						}

						// If the row clicked upon was a group with children, apply the setting to all child rows now
						if(re.hasChildren) {
							for(int r=row+1;r<rowEntries.length;r++) {
								RowEntry childRow = (RowEntry)rowEntries[r];
								if(childRow.group == re.group) {
									Boolean b = (Boolean)selections[r][col-2];
									if(b != null && b.booleanValue() != rowValue.booleanValue()) {
										selections[r][col-2] = Boolean.valueOf(rowValue.booleanValue());
										fireTableCellUpdated(r,col);
										// Calculate the child row's rowSelections entry now.
										boolean hasAnythingMarked = false;
										int processCount = processes != null? processes.length : 0;
										for(int c=0;c<processCount;c++) {
											if(selections[r][c] == null) {
												continue;
											}
											b = (Boolean)selections[r][c];
											if(b != null && b.booleanValue()) {
												hasAnythingMarked = true;
												break;
											}
										}
										if(hasAnythingMarked != rowSelections[r].booleanValue()) {
											rowSelections[r] = Boolean.valueOf(hasAnythingMarked);
											fireTableCellUpdated(r,0);
										}
									}
								}
							}
						}
					}

					// If the row clicked upon was not a group with children, but is a child, recalculate the parent's entries
					if(!re.hasChildren && re.group.pollutantIDsInDisplayGroup.size() > 1) {
						recalculateParent(row,col);
					}
				}
			}
			fireTableDataChanged();
		}
	}

	/** Table to display the tristate checkboxes and pollutant name columns **/
	class PollutantsTable extends JTable {

		public boolean isFocusable()  {
			return true;
		}
		public void updateUI() {
    		super.updateUI();
    		super.getTableHeader().setEnabled(false);

    		LookAndFeel.installColorsAndFont(this, "TableHeader.background", "TableHeader.foreground", "TableHeader.font");
		}

		/*
		 * Method added to use in Abbot testing.
		 */
		public boolean getBooleanValueAt(int row, int col) {
			return (Boolean)getModel().getValueAt(row, col);
		}

		/*
		 * Method added to use in Abbot testing.
		 */
		public boolean getChildrenVisibleAt(int row) {
			RowEntry re = (RowEntry)rowEntries[row];
			return re.childrenAreVisible;
		}
		
		public TableCellRenderer getCellRenderer(int row,int col) {
			if(col == 0) {
				return new TriStateRenderer();
			} else if(col == 1) {
				return new ExpandableRenderer();
			} else {
				return super.getCellRenderer(row, col);
			}
		}
	}

	/** Table to display the pollutant/process checkboxes **/
	class ProcessesTable extends JTable {
		/** Renderer returned if a cell is empty **/
		BlankRenderer blankRenderer;
		
		public int getLastVisibleColumn() {
			boolean onroad = MOVESNavigation.singleton.parent.scalePanel.onroadRadioButton.isSelected();

			if(onroad) {
				// return Refueling Spillage Loss index;
				return 13;
			} else { //NONROAD
				// return Running Loss Fuel Vapor Venting index;
				return 21;
			}
		}
		
		public int getNextRightColumn(int column) {
			boolean onroad = MOVESNavigation.singleton.parent.scalePanel.onroadRadioButton.isSelected();
			if(!onroad && column == 1) {
				return 11;
			}

			if(!onroad && column == 15) {
				return 18;
			}

			return column;
		}
		
		public int getNextLeftColumn(int column) {
			boolean onroad = MOVESNavigation.singleton.parent.scalePanel.onroadRadioButton.isSelected();
			if(!onroad && column == 12) {
				return 2;
			}

			if(!onroad && column == 19) {
				return 16;
			}

			return column;
		}

		/**
		 * Constructor to set the Table Model for this table.
		 * @param model The table model used for this table.
		**/
		public ProcessesTable(AbstractTableModel model) {
			super(model);
    		super.getTableHeader().setEnabled(false);
			blankRenderer = new BlankRenderer();
			blankRenderer.setToolTipText("Click");
		}

		/*
		 * Method added to use in Abbot testing.
		 */
		public boolean getBooleanValueAt(int row, int col) {
			return (Boolean)getModel().getValueAt(row, col+2);
		}

		/**
		 * Overrides the cell renderer for empty cells by returning a blank renderer.
		 * @param row The row number for the cell.
		 * @param col The column number for the cell.
		 * @return The cell renderer.
		**/
		public TableCellRenderer getCellRenderer(int row,int col) {
			if(getModel().getValueAt(convertRowIndexToModel(row),col+2) == null) {
				return blankRenderer;
			} else {
				//return super.getCellRenderer(row,col);
				TableCellRenderer v = super.getCellRenderer(row,col);
				if(v instanceof JComponent) {
//					System.out.println(getModel().getValueAt(convertRowIndexToModel(row),col+2).getClass());
					String chk = " is not checked.";
					if((Boolean)getModel().getValueAt(convertRowIndexToModel(row),col+2)) {
						chk = " is checked";
					}
					String rowValue = (String) pollutantsTable.getModel().getValueAt(convertRowIndexToModel(row),1);
					String colValue = pollutantsTable.getModel().getColumnName(col+2);
					((JComponent)v).setToolTipText(rowValue + " from " + colValue + chk);
				}
				return v;
			}
		}

		public boolean isFocusable() {
			return true;
		}
	}

	/** Show only rows that have not been hidden by their parent **/
	class RowEntryFilter extends RowFilter<PollutantsAndProcessesTableModel,Integer> {
		public boolean include(Entry<? extends PollutantsAndProcessesTableModel, ? extends Integer> entry) {
			int row = entry.getIdentifier().intValue();
			if(row >= 0 && rowEntries != null && row < rowEntries.length) {
				return ((RowEntry)rowEntries[row]).isVisible;
			}
			return false;
		}
	}


	/**
	 * Constructs a PollutantsAndProcesses panel, also creates and sets the layout of the controls.
	**/
	public PollutantsAndProcesses() {
		createControls();
		arrangeControls();
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("Pollutants and Processes:\r\n");
		for(Iterator i = runspec.pollutantProcessAssociations.iterator(); i.hasNext();) {
			PollutantProcessAssociation iterAssociation = (PollutantProcessAssociation) i.next();
			destination.append("\t");
			destination.append(iterAssociation.toString());
			destination.append("\r\n");
		}
		destination.append("\r\n");
	}

	private void setColumnWidths(JTable table, int offset, int... widths) {
	    TableColumnModel columnModel = table.getColumnModel();
	    for (int i = 0; i < widths.length; i++) {
	        if (i < columnModel.getColumnCount()) {
	        	if(widths[i] != -1) {
	        		columnModel.getColumn(i + offset).setMaxWidth(widths[i]);
	        	}
	        }
	        else break;
	    }
	}

	/**
	 * Creates and initializes all controls on this panel. And fixes the height
	 * and width of the JTable to fit in the screen.
	**/
	public void createControls() {
		Models.ModelCombination mc;
		if (MOVESNavigation.singleton != null
				&& MOVESNavigation.singleton.parent != null
				&& MOVESNavigation.singleton.parent.runSpec != null) {
			mc = MOVESNavigation.singleton.parent.runSpec.getModelCombination();
		} else {
			mc = Models.ModelCombination.M0;
		}

		splitPane = new JSplitPane();
		splitPane.setName("splitPane");

		model = new PollutantsAndProcessesTableModel();
		sorter = new TableRowSorter<PollutantsAndProcessesTableModel>(model);
		sorter2 = new TableRowSorter<PollutantsAndProcessesTableModel>(model);

		// Create processes table (holds all pollutant/process checkboxes)
		processesTable = new ProcessesTable(model);
		processesTable.setAutoCreateColumnsFromModel(false);
		processesTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		processesTable.setName("processesTable");
		processesTable.setRowSelectionAllowed(false);
		processesTable.getTableHeader().setResizingAllowed(false);
		processesTable.getTableHeader().setReorderingAllowed(false);
		processesTable.getDefaultEditor(Boolean.class).addCellEditorListener(this);
		processesTable.setRowSorter(sorter);
		processesTable.addKeyListener(new ProcessesTableKeyAdapter(processesTable));
		processesTable.addMouseListener(new ProcessesTableMouseAdapter(processesTable));

		int constantColSize = 60;
		setColumnWidths(processesTable, 
				2, 
				new int[] {constantColSize		// Running Exhaust
						, constantColSize + 7	//Crankcase Running Exhaust
						, constantColSize + 7	//Brakewear
						, constantColSize		//Tirewear
						, constantColSize		//Start Exhaust
						, constantColSize + 7	//Crankcase Start Exhaust
						, constantColSize		//Extended Idle Exhaust
						, constantColSize + 15	//Crankcase Extended Idle Exhaust
						, constantColSize    	//Auxillary Power Exhaust (renamed to be Other Hotelling Exhaust for display purposes)
						, constantColSize + 15	//Evap Permeation
						, constantColSize		//Evap Fuel Vapor Venting
						, constantColSize		//Evap Fuel Leaks
						, constantColSize + 25	//Refueling Dsiplacement Vapor Loss
						, constantColSize		//Refueling Spillage Loss
						, constantColSize + 10	//Evap Tank Permeation
						, constantColSize + 10	//Evap House Permeation
						, constantColSize		//not sure
						, constantColSize		//not sure
						, constantColSize		//not sure
						, constantColSize + 7	//Diurnal Fuel Vapor Venting
						, constantColSize + 7	//HotSoak Fuel Vapor Venting
						, constantColSize + 15	//RunningLoss Fuel Vapor Venting
						});

		MultiLineTableHeaderRenderer renderer = new MultiLineTableHeaderRenderer();
        Enumeration enumK = processesTable.getColumnModel().getColumns();
        boolean first = true;
        while (enumK.hasMoreElements()) {
        	TableColumn tc = (TableColumn) enumK.nextElement();
        	tc.setHeaderRenderer(renderer);
            if(!first) {
            	tc.setMinWidth(tc.getMinWidth()/2);
            } else {
            	first = false;
            }
        }

		// Create pollutants table (holds tristate checkboxes and pollutant names)
		pollutantsTable = new PollutantsTable();
		pollutantsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		pollutantsTable.setAutoCreateColumnsFromModel(false);
		pollutantsTable.setModel(model);
		pollutantsTable.setName("pollutantsTable");
		pollutantsTable.setRowSelectionAllowed(false);
		pollutantsTable.getTableHeader().setResizingAllowed(false);
		pollutantsTable.getTableHeader().setReorderingAllowed(false);
		pollutantsTable.getDefaultEditor(Boolean.class).addCellEditorListener(this);
		pollutantsTable.setRowSorter(sorter2);
		pollutantsTable.addMouseListener(new PollutantsTableMouseAdapter(pollutantsTable));
		pollutantsTable.addKeyListener(new PollutantsTableKeyAdapter(pollutantsTable));

		// Move pollutants to pollutants table
		if(processesTable.getColumnCount() >= 2) {
			for(int i=0; i < 2; i++){
				TableColumn column = processesTable.getColumnModel().getColumn(0);
				processesTable.getColumnModel().removeColumn(column);

				if(i == 0) {
					column.setHeaderValue("Selected");
				} else if (i == 1) {
					column.setHeaderValue("Pollutant");
				}
				pollutantsTable.getColumnModel().addColumn(column);
			}
		}

		// Fix processes table height and width
		FontMetrics fm = processesTable.getFontMetrics(processesTable.getFont());
		javax.swing.table.TableColumnModel colModel = processesTable.getColumnModel();
		int numCols = colModel.getColumnCount();
		int tableHeight = 0;
		int tableWidth = 0;
		boolean preIsM1andEmpty = false;
		boolean preIsM2andEmpty = false;
		for (int i = 0; i < numCols; i++) {
			String columnName = (String) colModel.getColumn(i).getHeaderValue();
			TableColumn tableColumn = colModel.getColumn(i);
			int columnSize = 0;
			columnSize = fm.stringWidth(columnName) + 10;
			if (1 == 1) {
				// tableColumn.setPreferredWidth(columnSize);
				EmissionProcess process = this.model.getEmissionProcess(i + 2);
				switch (mc) {
					case M2:
						if (!process.isAffectedByNonroad) {
							if (preIsM2andEmpty) {
								tableColumn.setMaxWidth(0);
								tableColumn.setMinWidth(0);
								tableColumn.setPreferredWidth(0);
							} else {
								tableColumn.setMaxWidth(1);
								tableColumn.setMinWidth(0);
								tableColumn.setPreferredWidth(1);
							}
							tableWidth += 10;
							preIsM2andEmpty = true;
						} else {
							tableWidth += columnSize;
							tableColumn.setPreferredWidth(columnSize);
							preIsM2andEmpty = false;
						}
						break;
					case M0:
						break;
					case M1:
					default:
						if (!process.isAffectedByOnroad) {
							if (preIsM1andEmpty) {
								tableColumn.setMaxWidth(0);
								tableColumn.setMinWidth(0);
								tableColumn.setPreferredWidth(0);
							} else {
								tableColumn.setMaxWidth(1);
								tableColumn.setMinWidth(0);
								tableColumn.setPreferredWidth(1);
							}
							tableWidth += 10;
							preIsM1andEmpty = true;
						} else {
							preIsM1andEmpty = false;
							tableWidth += columnSize;
							tableColumn.setPreferredWidth(columnSize);
						}
						break;
				}
			} else {
				// Code before NonRoad integration
				tableWidth += columnSize;
			}
		}
		processesTable.setPreferredScrollableViewportSize(processesTable.getPreferredSize());

		// Fix pollutants table height and width
		fm = pollutantsTable.getFontMetrics(pollutantsTable.getFont());
		colModel = pollutantsTable.getColumnModel();
		numCols = colModel.getColumnCount();
		tableHeight = 0;
		tableWidth = 0;
		for (int i = 0; i < numCols; i++) {
			sorter.setSortable(i, false);
			TableColumn tableColumn = colModel.getColumn(i);

			if (i != 1) {
				tableColumn.setPreferredWidth(constantColSize);
				tableWidth += tableColumn.getPreferredWidth();
			} else {
				int columnNumber = tableColumn.getModelIndex();
				int max = 20;
				int columnWidth = 0;
				int nrows = pollutantsTable.getModel().getRowCount();
				String cell = "";
				preIsM1andEmpty = false;
				preIsM2andEmpty = false;
				for (int j = 0; j < nrows; j++) {
					String value = (String) pollutantsTable.getModel().getValueAt(j, columnNumber);
					columnWidth = fm.stringWidth("." + value.trim() + ".");
					if (columnWidth > max) {
						max = columnWidth;
					}
					tableHeight += pollutantsTable.getRowHeight();
				}
				max += 20;
				tableColumn.setPreferredWidth(max);
				tableWidth += max;
			}
		}
		pollutantsTable.setPreferredScrollableViewportSize(pollutantsTable.getPreferredSize());

		if (1 == 1) {
			int nrows = pollutantsTable.getModel().getRowCount();
			preIsM1andEmpty = false;
			preIsM2andEmpty = false;
			tableHeight = 0;
			for (int j = 0; j < nrows; j++) {
				Pollutant pollutant = this.model.getPollutant(j);
				if(pollutant != null) {
					switch (mc) {
						case M2:
							if (!pollutant.isAffectedByNonroad) {
								if (preIsM2andEmpty) {
									pollutantsTable.setRowHeight(j, 1);
									processesTable.setRowHeight(j, 1);
								} else {
									pollutantsTable.setRowHeight(j, 1);
									processesTable.setRowHeight(j, 1);
								}
								preIsM2andEmpty = true;
							} else {
								preIsM2andEmpty = false;
							}
							break;
						case M0:
							break;
						case M1:
						default:
							if (!pollutant.isAffectedByOnroad) {
								if (preIsM1andEmpty) {
									pollutantsTable.setRowHeight(j, 1);
									processesTable.setRowHeight(j, 1);
								} else {
									pollutantsTable.setRowHeight(j, 1);
									processesTable.setRowHeight(j, 1);
								}
								preIsM1andEmpty = true;
							} else {
								preIsM1andEmpty = false;
							}
							break;
					}
				}
				tableHeight += pollutantsTable.getRowHeight();
			}
		}

		// Add everything to scroll pane
		tableScrollPane = new JScrollPane(processesTable,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		tableScrollPane.setRowHeaderView(pollutantsTable);
		tableScrollPane.getRowHeader().addChangeListener(this);
		tableScrollPane.setCorner(JScrollPane.UPPER_LEFT_CORNER, pollutantsTable.getTableHeader());
		tableScrollPane.setName("pollutantsAndProcessTableScrollPane");
		ToolTipHelper.add(tableScrollPane,"Select Pollutant and Process combinations to simulate");

		// Create message log model and object
		messageLogModel = new DefaultListModel<String>();
		messageLogList = new JListWithToolTips<String>(messageLogModel);
		messageLogList.setName("messageLogList");
		messageLogList.setSelectedIndex(-1);
		messageLogList.setVisibleRowCount(10);
		messageLogList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" +
				"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");

		messageLogPane = new JScrollPane(messageLogList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		messageLogPane.setName("messageLogPane");
		messageLogPane.setVisible(true);
		ToolTipHelper.add(messageLogPane,"Displays Pollutant/Process combinations that have not been met");

		selectPrerequisistes = new JButton("Select Prerequisites");
		selectPrerequisistes.setName("selectPrerequisistes");
		selectPrerequisistes.addActionListener(this);
		selectPrerequisistes.setMnemonic('q');
		selectPrerequisistes.setDisplayedMnemonicIndex(12);
		ToolTipHelper.add(selectPrerequisistes,"Select all required pollutant and process combinations");
		selectPrerequisistes.setEnabled(false);

		clearAll = new JButton("Clear All");
		clearAll.setName("clearAll");
		clearAll.addActionListener(this);
		clearAll.setMnemonic('C');
		clearAll.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(clearAll,"Remove all selected pollutant and process combinations");
		clearAll.setEnabled(false);
	}

	private void appendToPane(JTextPane tp, String msg, Color c) {
        StyleContext sc = StyleContext.getDefaultStyleContext();
        AttributeSet aset = sc.addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.Foreground, c);

        aset = sc.addAttribute(aset, StyleConstants.FontFamily, "Lucida Console");
        aset = sc.addAttribute(aset, StyleConstants.Alignment, StyleConstants.ALIGN_JUSTIFIED);

        int len = tp.getDocument().getLength();
        tp.setCaretPosition(len);
        tp.setCharacterAttributes(aset, false);
        tp.replaceSelection(msg);
    }

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		removeAll();

		JTextPane helper = new JTextPane();
		appendToPane(helper, "When pollutants are listed in the box at right, MOVES needs to calculate those emissions first, before calculating the pollutants you selected. In this case, click \"Select Prerequisites\" to proceed.", Color.BLACK);
		helper.setEditable(false);
		helper.setMaximumSize(new Dimension(440, 70));
		helper.setMinimumSize(new Dimension(380, 60));
		helper.setBackground(clearAll.getBackground());

		JPanel p = new JPanel();
		p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
		p.add(helper);
		p.add(selectPrerequisistes);
		p.add(Box.createHorizontalGlue());
		p.add(Box.createRigidArea(new Dimension(0,5)));
		p.add(clearAll);

		JPanel t = new JPanel();
		t.setLayout(new BoxLayout(t, BoxLayout.X_AXIS));
		t.add(p);
		messageLogPane.setMaximumSize(new Dimension(2000, 275));
		messageLogPane.setPreferredSize(new Dimension(1000, 200));
		messageLogPane.setMinimumSize(new Dimension(500, 100));
		t.add(messageLogPane);

		setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
		splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		splitPane.setTopComponent(tableScrollPane);
		splitPane.setBottomComponent(t); // messageLogPane
		splitPane.setPreferredSize(splitPane.getMinimumSize());
		splitPane.setResizeWeight(0.85);
		add(splitPane);
	}

	/**
	 * Saves the Pollutant Process Screen information to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		//System.out.println("PollutantAndProcesses.saveToRunSpec");
		boolean shouldAddOffNetworkRoadType = false;
		boolean hasRefuelingLoss = false;
		boolean hasMesoscaleEvap = false;
		runspec.pollutantProcessAssociations.clear();

		Models.ModelCombination mc = runspec.getModelCombination();

		for(int pollutantDisplayGroupIndex=0;pollutantDisplayGroupIndex < rowEntries.length;pollutantDisplayGroupIndex++) {
			RowEntry re = (RowEntry)rowEntries[pollutantDisplayGroupIndex];
			if(re.hasChildren) {
				continue;
			}
			for(int processIndex=0;processIndex<processes.length;processIndex++) {
				if(selections[pollutantDisplayGroupIndex][processIndex] != null) {
					Boolean b = (Boolean)selections[pollutantDisplayGroupIndex][processIndex];
					if(b.booleanValue()) {
						EmissionProcess process = (EmissionProcess)processes[processIndex];
						PollutantProcessAssociation assoc = new PollutantProcessAssociation();
						assoc.pollutant = re.pollutant;
						assoc.emissionProcess = process;
						assoc.isAffectedByOnroad = PollutantProcessLoader
								.getBooleanColVal("isAffectedByOnroad",
										assoc.pollutant.databaseKey,
										assoc.emissionProcess.databaseKey);
						assoc.isAffectedByNonroad = PollutantProcessLoader
								.getBooleanColVal("isAffectedByNonroad",
										assoc.pollutant.databaseKey,
										assoc.emissionProcess.databaseKey);
						assoc.isChosenForNonroad = false;
						assoc.isChosenForOnroad = false;
						switch (mc) {
							case M1:
								assoc.isChosenForOnroad = true;
								break;
							case M2:
								assoc.isChosenForNonroad = true;
								break;
							case M12:
								assoc.isChosenForNonroad = true;
								assoc.isChosenForOnroad = true;
								break;
							default:
								assoc.isChosenForOnroad = true;
								break;
						}
						runspec.pollutantProcessAssociations.add(assoc);
						//System.out.println("Selected: " + assoc.toString());
						// Ensure that processes that *must* have the off-network road
						// type available will have it
						if(process.databaseKey == 2 || process.databaseKey == 90) {
							// 90== Extended Idle Exhaust by definition
							// 2== Start Exhaust by definition
							shouldAddOffNetworkRoadType = true;
						} else if(process.databaseKey == 18 || process.databaseKey == 19) {
							hasRefuelingLoss = true;
						} else if(process.databaseKey == 11 || process.databaseKey == 12
								|| process.databaseKey == 13) {
							if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
								hasMesoscaleEvap = true;
							}
						}
					}
				}
			}
		}
		// Put back selections that the RunSpec started with but that weren't displayed
		for (Iterator<PollutantProcessAssociation> i = undisplayedRunSpecSelections.iterator(); i.hasNext();) {
			PollutantProcessAssociation assoc = i.next();
			/** NR_IMP: differentiate Noroad and Onroad **/
			assoc.isAffectedByOnroad = PollutantProcessLoader.getBooleanColVal("isAffectedByOnroad",assoc.pollutant.databaseKey,assoc.emissionProcess.databaseKey);
			assoc.isAffectedByNonroad = PollutantProcessLoader.getBooleanColVal("isAffectedByNonroad",assoc.pollutant.databaseKey,assoc.emissionProcess.databaseKey);
			assoc.isChosenForNonroad = false;
			assoc.isChosenForOnroad = false;
			switch (mc) {
				case M1:
					assoc.isChosenForOnroad = true;
					break;
				case M2:
					assoc.isChosenForNonroad = true;
					break;
				case M12:
					assoc.isChosenForNonroad = true;
					assoc.isChosenForOnroad = true;
					break;
				default:
					assoc.isChosenForOnroad = true;
					break;
			}
			/** NR_IMP **/
			runspec.pollutantProcessAssociations.add(assoc);
			// System.out.println("Selected(#2): " + assoc.toString());
		}


		if(shouldAddOffNetworkRoadType) {
			RoadType rt = new RoadType(1,"Off-Network", Models.ModelCombination.M1);
			runspec.roadTypes.add(rt);
		}

		if(hasRefuelingLoss) { // || hasMesoscaleEvap) {
			//RoadTypeScreen.setAllRoadTypes(runspec);
		}
	}

	/**
	 * Loads the Pollutant Process screen information from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		if(!PollutantProcessLoader.isLoaded() || forcePollutantProcessReload) {
			forcePollutantProcessReload = false;
			PollutantProcessLoader.loadFromDatabase();
		}
		Models.ModelCombination mc = runspec.getModelCombination();
		boolean hasNonroad = mc == Models.ModelCombination.M2;
		processes = EmissionProcess.getAllEmissionProcesses();
		processIndexes = new TreeMap<EmissionProcess,Integer>();
		for(int processIndex=0;processIndex<processes.length;processIndex++) {
			EmissionProcess process = (EmissionProcess)processes[processIndex];
			processIndexes.put(process,Integer.valueOf(processIndex));
		}

		// Nonroad Manganese (66) should be shown in the same group as Mercury Elemental Gaseous (60).
		// Onroad Manganese (66) should be shown in the same group as Nitrate (35).
		PollutantDisplayGroup manganeseGroup = PollutantDisplayGroup.pollutantDisplayGroups.get(66);
		manganeseGroup.pollutantIDsInDisplayGroup.remove(Integer.valueOf(66));
		if(hasNonroad) {
			manganeseGroup = PollutantDisplayGroup.pollutantDisplayGroups.get(60);
		} else {
			manganeseGroup = PollutantDisplayGroup.pollutantDisplayGroups.get(35);
		}
		manganeseGroup.pollutantIDsInDisplayGroup.add(Integer.valueOf(66));
		PollutantDisplayGroup.pollutantDisplayGroups.put(Integer.valueOf(66),manganeseGroup);

		Object[] pollutants = Pollutant.getAllDisplayablePollutants(hasNonroad? -70 : -100);

		// Get a list of all the pollutant display groups for the remaining pollutants.
		TreeSet<RowEntry> rowEntryList = new TreeSet<RowEntry>();
		TreeMap<Integer,PollutantDisplayGroup> groupsToUse = new TreeMap<Integer,PollutantDisplayGroup>();
		TreeMap<Integer,RowEntry> groupRows = new TreeMap<Integer,RowEntry>();
		for(int pollutantIndex=0;pollutantIndex<pollutants.length;pollutantIndex++) {
			Pollutant pollutant = (Pollutant)pollutants[pollutantIndex];
			Integer pollutantID = Integer.valueOf(pollutant.databaseKey);
			PollutantDisplayGroup group = PollutantDisplayGroup.pollutantDisplayGroups.get(pollutantID);
			RowEntry gr = null;
			if(group != null && group.pollutantIDsInDisplayGroup.size() > 1) {
				Integer groupKey = Integer.valueOf(group.displayGroupID);
				PollutantDisplayGroup t = groupsToUse.get(groupKey);
				if(t == null) {
					groupsToUse.put(groupKey,group);
				} else {
					group = t;
				}
				gr = groupRows.get(groupKey);
				if(gr == null) {
					// Give the group itself a row
					gr = new RowEntry();
					gr.group = group;
					gr.pollutant = null;
					gr.hasChildren = true;
					gr.childrenAreVisible = expandedGroups.contains(groupKey);
					gr.isVisible = true;
					rowEntryList.add(gr);
					groupRows.put(groupKey,gr);
				}
			}
			RowEntry r = new RowEntry();
			r.group = group;
			r.pollutant = pollutant;
			if(gr != null) {
				r.isVisible = gr.childrenAreVisible;
			} else {
				r.isVisible = true;
			}
			switch (mc) {
				case M2:
					if (!pollutant.isAffectedByNonroad) {
						r.isVisible = false;
						r.couldBeVisible = false;
					} else {
						r.couldBeVisible = true;
					}
					break;
				case M1:
				case M0:
				default:
					if (!pollutant.isAffectedByOnroad) {
						r.isVisible = false;
						r.couldBeVisible = false;
					} else {
						r.couldBeVisible = true;
					}
					break;
			}
			rowEntryList.add(r);
		}
		rowEntries = rowEntryList.toArray();
		for(int r=0;r<rowEntries.length;r++) {
			RowEntry re = (RowEntry)rowEntries[r];
			re.row = r;
			if(re.hasChildren) {
				re.couldBeVisible = true;
				re.isVisible = true;
				boolean hasPossibleVisibleChild = false;
				for(int k=r+1;k<rowEntries.length;k++) {
					RowEntry cr = (RowEntry)rowEntries[k];
					if(cr.group != re.group) {
						break;
					}
					cr.parent = re;
					if(cr.couldBeVisible) {
						cr.isVisible = re.childrenAreVisible;
						hasPossibleVisibleChild = true;
					} else {
						cr.isVisible = false;
					}
				}
				if(!hasPossibleVisibleChild) {
					re.couldBeVisible = false;
					re.isVisible = false;
				}
			}
		}
		/*
		for(int r=0;r<rowEntries.length;r++) {
			RowEntry re = (RowEntry)rowEntries[r];
			System.out.println("[" + re.toString() + "] row=" + re.row + ", parent.row=" + (re.parent!=null?(re.parent.toString()+"@"+re.parent.row):""));
		}
		*/

		// Determine the pollutant row indexes for the pollutant/process grid
		TreeMap<Integer,RowEntry> pollutantRowEntryIndexes = new TreeMap<Integer,RowEntry>();
		for(int i=0;i < rowEntries.length; i++) {
			RowEntry r = (RowEntry)rowEntries[i];
			if(r.pollutant != null) {
				pollutantRowEntryIndexes.put(Integer.valueOf(r.pollutant.databaseKey),r);
			}
		}

		// Create a check-box selection grid entry for all valid pollutant/process combination
		selections = null;
		rowSelections = null;
		if(processes != null && rowEntries != null) {
			selections = new Object[rowEntries.length][processes.length];
			rowSelections = new Boolean[rowEntries.length];

			for(int rowIndex=0;rowIndex<rowEntries.length;rowIndex++) {
				rowSelections[rowIndex] = Boolean.FALSE;
				RowEntry r = (RowEntry)rowEntries[rowIndex];
				for(int processIndex=0;processIndex<processes.length;processIndex++) {
					EmissionProcess process = (EmissionProcess)processes[processIndex];
					if (r.hasChildren) {
						boolean canBreak = false;
						for (Iterator<Integer> i = r.group.pollutantIDsInDisplayGroup.iterator(); i.hasNext();) {
							int pollutantID = i.next().intValue();
							Pollutant pollutant = Pollutant.findByID(pollutantID);
							if (pollutant != null) {
								switch (mc) {
									case M0:
									case M1:
										Boolean isAffectedByOnroad = PollutantProcessLoader.getBooleanColVal("isAffectedByOnroad",pollutantID,process.databaseKey);
										if (isAffectedByOnroad != null && isAffectedByOnroad) {
											if (pollutant.associatedProcesses.contains(process)) {
												selections[rowIndex][processIndex] = Boolean.FALSE;
												canBreak = true;
											}
										}
										break;
									case M2:
										Boolean isAffectedByNonroad = PollutantProcessLoader.getBooleanColVal("isAffectedByNonroad",pollutantID,process.databaseKey);
										if (isAffectedByNonroad != null && isAffectedByNonroad) {
											if (pollutant.associatedProcesses.contains(process)) {
												selections[rowIndex][processIndex] = Boolean.FALSE;
												canBreak = true;
											}
										}
										break;
									default:
										break;
								}

								// if(pollutant.associatedProcesses.contains(process))
								// {
								// selections[rowIndex][processIndex] = new
								// Boolean(false); //
								// break;
								// }
								if (canBreak) {
									break;
								}
							}
						}
					} else {
						int pollutantID = r.pollutant.databaseKey;
						int pocessID = process.databaseKey;
						switch (mc) {
							case M0:
							case M1:
								Boolean isAffectedByOnroad = PollutantProcessLoader.getBooleanColVal("isAffectedByOnroad",pollutantID, process.databaseKey);
								if (isAffectedByOnroad != null && isAffectedByOnroad) {
									if (r.pollutant.associatedProcesses.contains(process)) {
										selections[rowIndex][processIndex] = Boolean.FALSE;
									}
								}
								break;
							case M2:
								Boolean isAffectedByNonroad = PollutantProcessLoader.getBooleanColVal("isAffectedByNonroad",pollutantID, process.databaseKey);
								if (isAffectedByNonroad != null && isAffectedByNonroad) {
									if (r.pollutant.associatedProcesses.contains(process)) {
										selections[rowIndex][processIndex] = Boolean.FALSE;
									}
								}
								break;
							default:
								break;
						}

						// if(r.pollutant.associatedProcesses.contains(process))
						// {
						// selections[rowIndex][processIndex] = new
						// Boolean(false);
						// }
					}
				}
			}
		}

		// Recreate the table so that new data is loaded
		createControls();
		arrangeControls();
		revalidate();

		// Set all pollutant/process combinations selected in the runspec while keeping track of
		// the combinations that are not displayed for selection.
		undisplayedRunSpecSelections.clear();
		if(selections != null) {
			// Clear any previously shown user selections
			for(int rowIndex=0;rowIndex<rowEntries.length;rowIndex++) {
				for(int processIndex=0;processIndex<processes.length;processIndex++) {
					if(selections[rowIndex][processIndex] != null) {
						selections[rowIndex][processIndex] = Boolean.FALSE;
					}
				}
			}
			// Push the RunSpec's pollutantProcessAssociations entries to the display
			for(Iterator<PollutantProcessAssociation> i = runspec.pollutantProcessAssociations.iterator(); i.hasNext();) {
				PollutantProcessAssociation iterAssociation = (PollutantProcessAssociation) i.next();
				Integer processIndex = (Integer)processIndexes.get(iterAssociation.emissionProcess);
				RowEntry re = pollutantRowEntryIndexes.get(Integer.valueOf(iterAssociation.pollutant.databaseKey));
				if(processIndex != null && re != null) {
					if(selections[re.row][processIndex.intValue()] != null) {
						selections[re.row][processIndex.intValue()] = Boolean.TRUE;
						rowSelections[re.row] = Boolean.TRUE;
						// If there is a parent row entry, then mark it as well
						if(re.parent != null) {
							if(selections[re.parent.row][processIndex.intValue()] != null) {
								switch (mc) {
									case M0:
									case M1:
										if (iterAssociation.isAffectedByOnroad) {
											selections[re.parent.row][processIndex.intValue()] = Boolean.TRUE;
										}
										break;
									case M2:
										if (iterAssociation.isAffectedByNonroad) {
											selections[re.parent.row][processIndex.intValue()] = Boolean.TRUE;
										}
										break;
									default:
										break;
								}
								rowSelections[re.parent.row] = Boolean.TRUE;
							}
						}
					}
				} else {
					// remember iterAssociation
					undisplayedRunSpecSelections.add(iterAssociation);
				}
			}
		}

		clearAll.setEnabled(runspec.pollutantProcessAssociations.size() > 0);

		updateFilters();

		//set the focus to the first row/col
		selectFirstProcessesColumn(0);
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		if(sections != null) {
			sections.remove(getName());
		}
		messageLogModel.clear();

		TreeSet<String> messages = new TreeSet<String>();
		TreeSet<PollutantProcessAssociation> requiredAssociations = new TreeSet<PollutantProcessAssociation>();
		boolean isOk = checkPrerequisites(runspec, messages, requiredAssociations);

		for(String messageLine : messages) {
			messageLogModel.addElement(messageLine);
		}

		RunSpecSectionStatus status;
		if(isOk) {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		} else {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}

		if(sections != null) {
			sections.put(getName(),status);
		}

		selectPrerequisistes.setEnabled(!requiredAssociations.isEmpty());
		clearAll.setEnabled(runspec.pollutantProcessAssociations.size() > 0);

		return status;
	}

	/**
	 * Add a pollutant/process association to the set of required associations.
	 * @param requiredAssociations pollutant/process associations that must still be selected
	 * @param process required emission process
	 * @param pollutant required pollutant
	**/
	void requires(TreeSet<PollutantProcessAssociation> requiredAssociations,
			EmissionProcess process, Pollutant pollutant) {
		PollutantProcessAssociation ppa = PollutantProcessAssociation.findByName(pollutant.pollutantName,process.processName);
		if(ppa != null) {
			requiredAssociations.add(ppa);
		}
	}

	/**
	 * Examine the selected pollutant/process associations and generate information
	 * about any additional associations that are required.
	 * @param runspec RunSpec to be examined
	 * @param messages Human-readable messages to be displayed
	 * @param requiredAssociations pollutant/process associations that must still be selected
	 * @return true when the GUI panel should be displayed as complete
	**/	
	boolean checkPrerequisites(RunSpec runspec, TreeSet<String> messages, TreeSet<PollutantProcessAssociation> requiredAssociations) {
		Models.ModelCombination mc = runspec.getModelCombination();

		boolean isOk = runspec.pollutantProcessAssociations.size() > 0;
		Connection db = DatabaseConnectionManager.getGUIConnection(MOVESDatabaseType.DEFAULT);

		if(runspec.pollutantProcessAssociations.size() > 0) {
			EmissionProcess crankcaseRunningProcess = EmissionProcess.findByID(15);
			EmissionProcess crankcaseStartProcess = EmissionProcess.findByID(16);
			EmissionProcess crankcaseExtIdleProcess = EmissionProcess.findByID(17);

			EmissionProcess runningExhaustProcess = EmissionProcess.findByID(1);
			EmissionProcess startExhaustProcess = EmissionProcess.findByID(2);
			EmissionProcess extIdleExhaustProcess = EmissionProcess.findByID(90);

			// Verify a required road type is selected
			if(isOk && mc == Models.ModelCombination.M1) {
				boolean needsNonOffnetworkRoad = false;
				boolean needsRefuelingRoads = false;
				boolean hasRunning = false, hasTire = false, hasBrake = false;
				boolean hasRefuelingDisplacement = false, hasRefuelingSpillage = false;
				for(Iterator i=runspec.pollutantProcessAssociations.iterator();i.hasNext();) {
					PollutantProcessAssociation p = (PollutantProcessAssociation)i.next();
					switch(p.emissionProcess.databaseKey) {
						case 1: // 1 == Running Exhaust
							needsNonOffnetworkRoad = true;
							hasRunning = true;
							break;
						case 9: // 9 == Brakewear
							needsNonOffnetworkRoad = true;
							hasBrake = true;
							break;
						case 10: // 10 == Tirewear
							needsNonOffnetworkRoad = true;
							hasTire = true;
							break;
						case 18: // 18 == Refueling Displacement Vapor Loss
							needsRefuelingRoads = true;
							hasRefuelingDisplacement = true;
							break;
						case 19: // 19 == Refueling Spillage Loss
							needsRefuelingRoads = true;
							hasRefuelingSpillage = true;
							break;
					}
				}
				if(needsRefuelingRoads) {
					boolean found = RoadTypeScreen.hasRefuelingRoads(runspec);
					if(!found) {
						if(hasRefuelingDisplacement) {
							String messageLine = "Refueling Displacement Vapor Loss requires all road types to be selected";
							messages.add(messageLine);
							isOk = false;
						}
						if(hasRefuelingSpillage) {
							String messageLine = "Refueling Spillage Loss requires all road types to be selected";
							messages.add(messageLine);
							isOk = false;
						}
					}
				}
				if(needsNonOffnetworkRoad) {
					boolean found = false;
					for(RoadType r : runspec.roadTypes) {
						if(r.roadTypeID >= 2 && r.roadTypeID <= 9) {
							found = true;
							break;
						}
					}
					if(!found) {
						if(hasRunning) {
							String messageLine = "Running Exhaust requires a non-offnetwork road to be selected";
							messages.add(messageLine);
							isOk = false;
						}
						if(hasBrake) {
							String messageLine = "Brakewear requires a non-offnetwork road to be selected";
							messages.add(messageLine);
							isOk = false;
						}
						if(hasTire) {
							String messageLine = "Tirewear requires a non-offnetwork road to be selected";
							messages.add(messageLine);
							isOk = false;
						}
					}
				}
			}

			// Check for distance
			if(!runspec.outputVMTData) {
				for(Iterator<PollutantProcessAssociation> iterPollutantProcessAssociations =
						runspec.pollutantProcessAssociations.iterator();
						iterPollutantProcessAssociations.hasNext();) {
					PollutantProcessAssociation pollutantProcessAssociation =
							(PollutantProcessAssociation)iterPollutantProcessAssociations.next();
					Pollutant p = pollutantProcessAssociation.pollutant;
					PollutantDisplayGroup g = p.displayGroup;
					if(g == null) {
						g = PollutantDisplayGroup.findByPollutantID(p.databaseKey);
						p.displayGroup = g;
					}
					if(g != null && g.requiresDistance && !CompilationFlags.DO_RATES_FIRST) {
						String messageLine = pollutantProcessAssociation + " requires distance to be selected";
						messages.add(messageLine);
						isOk = false;
					}
				}
			}

			// Check for chained pollutants
			TreeMap<EmissionProcess,TreeSet<Pollutant> > selectedProcesses
					= new TreeMap<EmissionProcess,TreeSet<Pollutant> >();
			for(Iterator<PollutantProcessAssociation> iterPollutantProcessAssociations =
					runspec.pollutantProcessAssociations.iterator();
					iterPollutantProcessAssociations.hasNext();) {
				PollutantProcessAssociation pollutantProcessAssociation =
						(PollutantProcessAssociation)iterPollutantProcessAssociations.next();
				TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(
						pollutantProcessAssociation.emissionProcess);
				if(selectedProcessPollutants == null) {
					selectedProcessPollutants = new TreeSet<Pollutant>();
					selectedProcesses.put(pollutantProcessAssociation.emissionProcess,
							selectedProcessPollutants);
				}
				selectedProcessPollutants.add(pollutantProcessAssociation.pollutant);
			}

			for(Iterator<PollutantProcessAssociation> iterPollutantProcessAssociations
					= runspec.pollutantProcessAssociations.iterator();
					iterPollutantProcessAssociations.hasNext();) {
				PollutantProcessAssociation pollutantProcessAssociation =
						(PollutantProcessAssociation)iterPollutantProcessAssociations.next();
				Pollutant pollutant = pollutantProcessAssociation.pollutant;
				if(pollutant.databaseKey == 98) { // Special case for CO2 Equivalent
					TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(pollutantProcessAssociation.emissionProcess);

					Pollutant methane = Pollutant.findByID(5);
					Pollutant n2o = Pollutant.findByID(6);
					Pollutant atmCO2 = Pollutant.findByID(90);

					if(methane != null && !pollutantProcessAssociation.emissionProcess.associatedPollutants.contains(methane)) {
						methane = null;
					}
					if(n2o != null && !pollutantProcessAssociation.emissionProcess.associatedPollutants.contains(n2o)) {
						n2o = null;
					}
					if(atmCO2 != null && !pollutantProcessAssociation.emissionProcess.associatedPollutants.contains(atmCO2)) {
						atmCO2 = null;
					}

					boolean hasMethane = methane != null && selectedProcessPollutants.contains(methane);
					boolean hasN2O = n2o != null && selectedProcessPollutants.contains(n2o);
					boolean hasAtmCO2 = atmCO2 != null && selectedProcessPollutants.contains(atmCO2);

					if(!hasMethane && !hasN2O && !hasAtmCO2) {
						String messageLine = pollutantProcessAssociation.emissionProcess + "/"
								+ pollutant + " requires ";
						ArrayList<String> tempList = new ArrayList<String>();
						if(atmCO2 != null) {
							tempList.add(pollutantProcessAssociation.emissionProcess + "/" + "Atmospheric CO2");
							requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,atmCO2);
						}
						if(methane != null) {
							tempList.add(pollutantProcessAssociation.emissionProcess + "/" + "Methane");
							requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,methane);
						}
						if(n2o != null) {
							tempList.add(pollutantProcessAssociation.emissionProcess + "/" + "Nitrous Oxide");
							requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,n2o);
						}
						for(int i=0;i<tempList.size();i++) {
							if(i > 0) {
								if(i == tempList.size()-1) {
									if(i > 1) {
										messageLine += ", and/or ";
									} else {
										messageLine += " and/or ";
									}
								} else {
									messageLine += ", ";
								}
							}
							messageLine += tempList.get(i);
						}
						messages.add(messageLine);
						isOk = false;
					} else if(hasMethane && hasN2O && hasAtmCO2) {
						// All is well, nothing to complain about.
					} else {
						// Leave isOk as it is.  This is a warning message.
						String messageLine = pollutantProcessAssociation.emissionProcess + "/"
								+ pollutant + " will not include ";
						boolean hasPreviousPollutant = false;
						if(!hasAtmCO2 && atmCO2 != null) {
							if(hasPreviousPollutant) {
								messageLine += " and ";
							}
							hasPreviousPollutant = true;
							messageLine += pollutantProcessAssociation.emissionProcess + "/" + "Atmospheric CO2";
						}
						if(!hasMethane && methane != null) {
							if(hasPreviousPollutant) {
								messageLine += " and ";
							}
							hasPreviousPollutant = true;
							messageLine += pollutantProcessAssociation.emissionProcess + "/" + "Methane";
						}
						if(!hasN2O && n2o != null) {
							if(hasPreviousPollutant) {
								messageLine += " and ";
							}
							hasPreviousPollutant = true;
							messageLine += pollutantProcessAssociation.emissionProcess + "/" + "Nitrous Oxide";
						}
						if(hasPreviousPollutant) {
							messages.add(messageLine);
						}
					}
				} else if(pollutant.databaseKey == 90) { // Atmospheric CO2
					switch (mc) {
						case M2:
							// No dependencies for Nonroad
							break;
						case M1:
						default:
							for (Iterator<Pollutant> iterRequiredPollutants = pollutant.requiredPollutants.iterator(); iterRequiredPollutants.hasNext();) {
								Pollutant requiredPollutant = (Pollutant)iterRequiredPollutants.next();
								TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(pollutantProcessAssociation.emissionProcess);
								if(!selectedProcessPollutants.contains(requiredPollutant)) {
									String messageLine = pollutantProcessAssociation.emissionProcess + "/" + pollutant + " requires " + pollutantProcessAssociation.emissionProcess + "/" + requiredPollutant;
									requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,requiredPollutant);
									messages.add(messageLine);
									isOk = false;
								}
							}
							break;
					}
				} else if (pollutant.databaseKey == 100) { // Primary Exhaust PM10 - Total
					switch (mc) {
						case M2:
							// No dependencies for Nonroad
							break;
						case M1:
						default:
							for (Iterator<Pollutant> iterRequiredPollutants = pollutant.requiredPollutants.iterator(); iterRequiredPollutants.hasNext();) {
								Pollutant requiredPollutant = iterRequiredPollutants.next();
								TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(pollutantProcessAssociation.emissionProcess);
								if (!selectedProcessPollutants.contains(requiredPollutant)) {
									String messageLine = pollutantProcessAssociation.emissionProcess
											+ "/"
											+ pollutant
											+ " requires "
											+ pollutantProcessAssociation.emissionProcess + "/" + requiredPollutant;
									requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,requiredPollutant);
									messages.add(messageLine);
									isOk = false;
								}
							}
							break;
					}
				} else if (pollutant.databaseKey == 110) { // Primary Exhaust PM2.5 - Total
					switch (mc) {
						case M2:
							// No dependencies for Nonroad
							break;
						case M1:
						default:
							for (Iterator<Pollutant> iterRequiredPollutants = pollutant.requiredPollutants.iterator(); iterRequiredPollutants.hasNext();) {
								Pollutant requiredPollutant = iterRequiredPollutants.next();
								TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(pollutantProcessAssociation.emissionProcess);
								if (!selectedProcessPollutants.contains(requiredPollutant)) {
									String messageLine = pollutantProcessAssociation.emissionProcess
											+ "/"
											+ pollutant
											+ " requires "
											+ pollutantProcessAssociation.emissionProcess + "/" + requiredPollutant;
									requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,requiredPollutant);
									messages.add(messageLine);
									isOk = false;
								}
							}
							break;
					}
				} else if (pollutant.databaseKey == 31) { // SO2
					switch (mc) {
						case M2:
							break;
						case M1:
						default:
							for (Iterator<Pollutant> iterRequiredPollutants = pollutant.requiredPollutants.iterator(); iterRequiredPollutants.hasNext();) {
								Pollutant requiredPollutant = iterRequiredPollutants.next();
								TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(pollutantProcessAssociation.emissionProcess);
								if (!selectedProcessPollutants.contains(requiredPollutant)) {
									String messageLine = pollutantProcessAssociation.emissionProcess
											+ "/"
											+ pollutant
											+ " requires "
											+ pollutantProcessAssociation.emissionProcess + "/" + requiredPollutant;
									requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,requiredPollutant);
									messages.add(messageLine);
									isOk = false;
								}
							}
							break;
					}
				} else {
					for (Iterator<Pollutant> iterRequiredPollutants = pollutant.requiredPollutants.iterator(); iterRequiredPollutants.hasNext();) {
						Pollutant requiredPollutant = (Pollutant)iterRequiredPollutants.next();
						TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(pollutantProcessAssociation.emissionProcess);
						if(!selectedProcessPollutants.contains(requiredPollutant)) {
							String messageLine = pollutantProcessAssociation.emissionProcess + "/" + pollutant + " requires " + pollutantProcessAssociation.emissionProcess + "/" + requiredPollutant;
							requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,requiredPollutant);
							messages.add(messageLine);
							isOk = false;
						}
					}
					// Add special cases for crankcase VOC et al. These aren't strictly needed by the underlying
					// model, rather these associations are added to reduce confusion by end users, making the
					// crankcase HC speciations appear to be like other processes.
					// 15 == crankcase running exhaust
					// 16 == crankcase start exhaust
					// 17 == crankcase extended idle
					// 79 needs 1  NMHC <- THC
					// 87 needs 79 VOC <- NMHC
					// 80 needs 79 NMOG <- NMHC
					// 86 needs 80, 5 TOG <- NMHC, Methane
					// 5 needs 1 Methane <- THC
					int[] crankcaseProcesses = { 15, 16, 17 };
					int[] crankcaseNeeds = { // Format: Output pollutantID, input pollutant ID, input pollutant ID
						79,  1, 0,
						87, 79, 0,
						80, 79, 0,
						86, 80, 5,
						 5,  1, 0
					};
					int processID = pollutantProcessAssociation.emissionProcess.databaseKey;
					for(int i=0;i<crankcaseProcesses.length;i++) {
						if(processID == crankcaseProcesses[i]) {
							for(int j=0;j<crankcaseNeeds.length;j+=3) {
								if(crankcaseNeeds[j+0] == pollutant.databaseKey) {
									TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(pollutantProcessAssociation.emissionProcess);
									for(int k=1;k<=2;k++) {
										if(crankcaseNeeds[j+k]>0) {
											Pollutant requiredPollutant = Pollutant.findByID(crankcaseNeeds[j+k]);
											if(requiredPollutant != null && !selectedProcessPollutants.contains(requiredPollutant)) {
												String messageLine = pollutantProcessAssociation.emissionProcess + "/" + pollutant + " requires " + pollutantProcessAssociation.emissionProcess + "/" + requiredPollutant;
												requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,requiredPollutant);
												messages.add(messageLine);
												isOk = false;
											}
										}
									}
									break;
								}
							}
							break;
						}
					}
				}
			}

			// Look for nonroad
			boolean hasNonroad = mc == Models.ModelCombination.M2;
			TreeSet<Integer> nonroadPolProcessIDs = new TreeSet<Integer>();
			if(hasNonroad) {
				for(int i=0;i<NonroadEmissionCalculator.nonroadPolProcessIDs.length;i++) {
					nonroadPolProcessIDs.add(Integer.valueOf(NonroadEmissionCalculator.nonroadPolProcessIDs[i]));
				}
			}

			// Look for TOG speciation mechanisms and their inputs
			try {
				ArrayList<Pollutant> mechanismPollutants = TOGSpeciationCalculator.getMechanismPollutants(db);
				for(Iterator<PollutantProcessAssociation> iterPollutantProcessAssociations
						= runspec.pollutantProcessAssociations.iterator();
						iterPollutantProcessAssociations.hasNext();) {
					PollutantProcessAssociation pollutantProcessAssociation =
							(PollutantProcessAssociation)iterPollutantProcessAssociations.next();
					if(!mechanismPollutants.contains(pollutantProcessAssociation.pollutant)) {
						continue;
					}
					ArrayList<EmissionProcess> mechanismProcesses = TOGSpeciationCalculator.getMechanismProcesses(db,pollutantProcessAssociation.pollutant);
					if(!mechanismProcesses.contains(pollutantProcessAssociation.emissionProcess)) {
						continue;
					}
					// The PPA is a speciation mechanism in a valid process, so ensure its required input pollutants
					// are also selected.
					TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(pollutantProcessAssociation.emissionProcess);
					ArrayList<PollutantProcessAssociation> requirements = TOGSpeciationCalculator.getMechanismRequirements(db,pollutantProcessAssociation);
					for(PollutantProcessAssociation r : requirements) {
						if(!selectedProcessPollutants.contains(r.pollutant)
								&& r.pollutant.associatedProcesses.contains(pollutantProcessAssociation.emissionProcess)) {
							String messageLine = pollutantProcessAssociation.emissionProcess + "/" + pollutantProcessAssociation.pollutant + " requires " + pollutantProcessAssociation.emissionProcess + "/" + r.pollutant;
							requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,r.pollutant);
							messages.add(messageLine);
							isOk = false;
						}
					}
				}
			} catch(SQLException e) {
				Logger.logError(e,"Unable to check TOG speciation dependencies");
			}
			
			if(hasNonroad) {
				try {
					ArrayList<Pollutant> integratedSpecies = NRAirToxicsCalculator.getIntegratedSpecies(db);
					for(Iterator<PollutantProcessAssociation> iterPollutantProcessAssociations = runspec.pollutantProcessAssociations.iterator();
							iterPollutantProcessAssociations.hasNext();) {
						PollutantProcessAssociation pollutantProcessAssociation = (PollutantProcessAssociation)iterPollutantProcessAssociations.next();
						if(pollutantProcessAssociation.pollutant.databaseKey != 88) { // Look for NonHapTOG (88)
							continue;
						}
						// NonHapTOG (88) is requested, ensure all of its required pollutants are selected.
						for(Pollutant rp : integratedSpecies) {
							if(!rp.isAffectedByNonroad) {
								continue;
							}
							if(!pollutantProcessAssociation.emissionProcess.associatedPollutants.contains(rp)) {
								continue;
							}
							PollutantProcessAssociation rppa = PollutantProcessAssociation.createByID(rp.databaseKey,pollutantProcessAssociation.emissionProcess.databaseKey);
							if(rppa == null || !rppa.isAffectedByNonroad) {
								continue;
							}
							if(!runspec.pollutantProcessAssociations.contains(rppa)) {
								String messageLine = pollutantProcessAssociation.emissionProcess + "/" + pollutantProcessAssociation.pollutant + " requires " + pollutantProcessAssociation.emissionProcess + "/" + rp;
								requires(requiredAssociations,pollutantProcessAssociation.emissionProcess,rp);
								messages.add(messageLine);
								isOk = false;
							}
						}
					}
				} catch(SQLException e) {
					Logger.logError(e,"Unable to check NR NonHAPTOG speciation dependencies");
				}
			}

			for(Iterator<PollutantProcessAssociation> iterPollutantProcessAssociations = runspec.pollutantProcessAssociations.iterator();
					iterPollutantProcessAssociations.hasNext();) {
				PollutantProcessAssociation output = (PollutantProcessAssociation)iterPollutantProcessAssociations.next();
				int outputID = output.getDatabaseKey();
				if(hasNonroad && nonroadPolProcessIDs.contains(Integer.valueOf(outputID))) {
					// Nonroad's native pollutant/processes do not have chaining.
					continue;
				}
				TreeSet<Integer> inputIDs = null;
				if(hasNonroad) {
					inputIDs = PollutantProcessAssociation.chainedToNR(outputID);
				} else {
					inputIDs = PollutantProcessAssociation.chainedTo(outputID);
				}
				if(inputIDs == null || inputIDs.size() <= 0) {
					continue;
				}
				for(Iterator<Integer> iterInputID=inputIDs.iterator();iterInputID.hasNext();) {
					Integer inputIDObject = (Integer)iterInputID.next();
					int inputID = inputIDObject.intValue();
					PollutantProcessAssociation input = PollutantProcessAssociation.createByID(inputID);
					int inputPollutantID = input.pollutant.databaseKey;
					int inputProcessID = input.emissionProcess.databaseKey;
					boolean foundInput = false;
					for(Iterator<PollutantProcessAssociation> j= runspec.pollutantProcessAssociations.iterator();j.hasNext();) {
						PollutantProcessAssociation p = (PollutantProcessAssociation)j.next();
						if(p.pollutant.databaseKey == inputPollutantID
								&& p.emissionProcess.databaseKey == inputProcessID) {
							foundInput = true;
							break;
						}
					}
					if(!foundInput) {
						String messageLine = output.emissionProcess + "/" + output.pollutant + " requires ";
						//if(inputProcessID != output.emissionProcess.databaseKey) {
							messageLine += input.emissionProcess + "/";
						//}
						messageLine += input.pollutant;

						requires(requiredAssociations,input.emissionProcess,input.pollutant);
						messages.add(messageLine);
						isOk = false;
					}
				}
			}

//			if(isOk) {
				EmissionProcess emissionProcess = EmissionProcess.findByID(99); // Well-to-Pump
				if(emissionProcess != null) {
					Pollutant pollutant = Pollutant.findByID(91); // Total Energy Consumption
					if(pollutant != null) {
						String messageLine = emissionProcess + " requires " + pollutant +
	 							" to be selected for at least ";
	 					String lastProcess = null;
	 					String delimiter = "";

						TreeSet<Pollutant> selectedProcessPollutants =
								selectedProcesses.get(emissionProcess);
						if(selectedProcessPollutants != null) {
							boolean foundTotalEnergyConsumption = false;
							emissionProcess = EmissionProcess.findByID(90); // Extended Idle Exhaust
							if(emissionProcess!=null) {
								lastProcess = emissionProcess.toString();
								selectedProcessPollutants =
										selectedProcesses.get(emissionProcess);
								if(selectedProcessPollutants != null) {
									if(selectedProcessPollutants.contains(Pollutant.findByID(91))) {
										foundTotalEnergyConsumption = true;
									}
								}
							}
							emissionProcess = EmissionProcess.findByID(1); // Running Exhaust
							if(emissionProcess!=null) {
								if(lastProcess!=null) {
									messageLine += delimiter + lastProcess;
									delimiter = ", ";
								}
								lastProcess = emissionProcess.toString();
								selectedProcessPollutants =
										selectedProcesses.get(emissionProcess);
								if(selectedProcessPollutants != null) {
									if(selectedProcessPollutants.contains(Pollutant.findByID(91))) {
										foundTotalEnergyConsumption = true;
									}
								}
							}

							emissionProcess = EmissionProcess.findByID(2); // Start Exhaust
							if(emissionProcess!=null) {
								if(lastProcess!=null) {
									messageLine += delimiter + lastProcess;
									delimiter = ", ";
								}
								lastProcess = emissionProcess.toString();
								selectedProcessPollutants =
										selectedProcesses.get(emissionProcess);
								if(selectedProcessPollutants != null) {
									if(selectedProcessPollutants.contains(Pollutant.findByID(91))) {
										foundTotalEnergyConsumption = true;
									}
								}
							}

							if(!foundTotalEnergyConsumption) {
								if(lastProcess != null) {
									messageLine += delimiter + " or " + lastProcess;
								}
								messages.add(messageLine);
								isOk = false;
							}
						}
					}
				}
//			}

			// for all the Evap related processes, ONLY check the time span when
			// ONROAD model is chosen
			emissionProcess = EmissionProcess.findByID(11); // Evap Permeation
			if(emissionProcess != null) {
				TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(emissionProcess);
				if(selectedProcessPollutants!=null) {
					switch (mc) {
						case M1:
							if (runspec.timeSpan.aggregateBy != OutputTimeStep.HOUR) {
								String messageLine = emissionProcess
										+ " requires the time aggregation level to be Hour";
								messages.add(messageLine);
								isOk = false;
							}
							/*
							else if (!runspec.timeSpan.hasAllHours()) {
								String messageLine = emissionProcess
										+ " requires all hours to be selected";
								messages.add(messageLine);
								isOk = false;
							}
							*/
							break;
						case M2:
						default:
							break;
					}
				}
			}
			emissionProcess = EmissionProcess.findByID(12); // Evap Fuel Vapor Venting
			if(emissionProcess != null) {
				TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(emissionProcess);
				if(selectedProcessPollutants!=null) {
					switch (mc) {
						case M1:
							if (runspec.timeSpan.aggregateBy != OutputTimeStep.HOUR) {
								String messageLine = emissionProcess
										+ " requires the time aggregation level to be Hour";
								messages.add(messageLine);
								isOk = false;
							} else if (!runspec.timeSpan.hasAllHours()) {
								String messageLine = emissionProcess
										+ " requires all hours to be selected";
								messages.add(messageLine);
								isOk = false;
							}
							break;
						case M2:
						default:
							break;
					}
				}
			}
			emissionProcess = EmissionProcess.findByID(13); // Evap Fuel Leaks
			if(emissionProcess != null) {
				TreeSet<Pollutant> selectedProcessPollutants = selectedProcesses.get(emissionProcess);
				if(selectedProcessPollutants!=null) {
					switch (mc) {
						case M1:
							if (runspec.timeSpan.aggregateBy != OutputTimeStep.HOUR) {
								String messageLine = emissionProcess
										+ " requires the time aggregation level to be Hour";
								messages.add(messageLine);
								isOk = false;
							}
							/*
							else if (!runspec.timeSpan.hasAllHours()) {
								String messageLine = emissionProcess
										+ " requires all hours to be selected";
								messages.add(messageLine);
								isOk = false;
							}
							*/
							break;
						case M2:
						default:
							break;
					}
				}
			}
		}

		return isOk;
	}

	/**
	 * Sets the defaults to the RunSpec.
	 * @param	runspec the RunSpec to the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		runspec.pollutantProcessAssociations.clear();
		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		sections.put(getName(),status);
		return status;
	}

	/**
	 * Update current selections to be consistent with a newly selected ModelScale.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	public RunSpecSectionStatus onScaleChange(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		saveToRunSpec(runspec);
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/**
	 * Handles a state change event.
	 * @param	e The event caused by the state change.
	**/
	public void stateChanged(ChangeEvent e)
	{ //Update table scroll position to match header scroll position
		JViewport viewport = (JViewport) e.getSource();
		tableScrollPane.getVerticalScrollBar().setValue(viewport.getViewPosition().y);
	}

	/**
	 * Handles table editing stopped event.
	 * @param	e The event caused by the table edit.
	**/
	public void editingStopped(ChangeEvent e){
		MOVESNavigation.singleton.updateRunSpecSectionStatus();
	}

	/**
	 * Currently not used.
	 * @param	e The event caused by the table edit.
	**/
	public void editingCanceled(ChangeEvent e) {
		// Nothing to do here
	}

	/** Force the table's rows to be rescanned for filtering **/
	void updateFilters() {
		if(sorter != null) {
			sorter.setRowFilter(new RowEntryFilter());
		}
		if(sorter2 != null) {
			sorter2.setRowFilter(new RowEntryFilter());
		}
	}

	/**
	 * Update current selections to be consistent with a newly selected Model.
	 *
	 * The editor should also place a RunSpecSectionStatus object into sections, likely using
	 * it's name as the key.  Typically it will store a DEFAULTS or NOT_READY value.
	 * @param runspec the RunSpec to examine
	 * @param sections a table of previous status calculation results which should be updated too
	 * @return an object that can be used to determine which icon to display to the user
	**/
	@Override
	public RunSpecSectionStatus onModelChange(RunSpec runspec,
			TreeMap<String, RunSpecSectionStatus> sections) {
		forcePollutantProcessReload = true;
		// saveToRunSpec(runspec);
		loadFromRunSpec(runspec);
		return calculateRunSpecSectionStatus(runspec, sections);
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == selectPrerequisistes) {
			selectAllPrerequisites();
		} else if(e.getSource() == clearAll) {
			clearAllSelections();
		}
	}

	/** Remove all pollutant/process combinations **/
	void clearAllSelections() {
		RunSpec runspec = null;
		if (MOVESNavigation.singleton != null
				&& MOVESNavigation.singleton.parent != null
				&& MOVESNavigation.singleton.parent.runSpec != null) {
			runspec = MOVESNavigation.singleton.parent.runSpec;
		}
		if(runspec == null) {
			return;
		}

		if(runspec.pollutantProcessAssociations.size() > 0) {
			runspec.pollutantProcessAssociations.clear();
			loadFromRunSpec(runspec);
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		}
	}

	/**
	 * Select all pollutant/process combinations required by
	 * the currently selected combinations.
	**/	
	void selectAllPrerequisites() {
		RunSpec runspec = null;
		if (MOVESNavigation.singleton != null
				&& MOVESNavigation.singleton.parent != null
				&& MOVESNavigation.singleton.parent.runSpec != null) {
			runspec = MOVESNavigation.singleton.parent.runSpec;
		}
		if(runspec == null) {
			return;
		}

		boolean didMakeChanges = false;
		while(true) {
			TreeSet<String> messages = new TreeSet<String>();
			TreeSet<PollutantProcessAssociation> requiredAssociations = new TreeSet<PollutantProcessAssociation>();
			boolean isOk = checkPrerequisites(runspec, messages, requiredAssociations);
			if(requiredAssociations.size() <= 0) {
				break;
			}
			for(PollutantProcessAssociation ppa : requiredAssociations) {
				runspec.pollutantProcessAssociations.add(ppa);
				didMakeChanges = true;
			}
		}
		if(didMakeChanges) {
			loadFromRunSpec(runspec);
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		}
	}
}
