/**************************************************************************************************
 * @(#)AdvancedPerformanceFeatures.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import java.sql.*;
import javax.swing.table.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;

/**
 * Class for MOVES AdvancedPerformanceFeatures panel. The panel contains a list
 * of classes that support optional execution and data capture.
 *
 * @author		Wes Faler
 * @author		Tim Hull
 * @version     2013-12-23
**/
public class AdvancedPerformanceFeatures extends JPanel
		implements RunSpecEditor, ActionListener, CellEditorListener, FocusListener {
	/** Grid of components and options shown on screen **/
	JTable table;
	/** Panel contains table. **/
	JPanel topPanel;
	/** scroll pane so that table can be scrolled **/
	JScrollPane tableScrollPane;

	/** Copy Saved Generator Data checkbox **/
	JCheckBox copySavedDataCheckbox;
	/** Create Database button **/
	JButton createButton;
	/** Refresh button **/
	JButton refreshButton;
	/** Server text control. **/
	JTextField server;
	/** Database combo control. **/
	ExtendedComboBox<String> databaseCombo;
	/** Run droplist ToolTip **/
	JToolTip databaseToolTip;
	/**
	 * Used by the server FocusLost event handler to help determine if the server
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the server TextField losing focus when the server name hasn't changed.
	**/
	String previousServer = new String();
	/** database names that should not be used **/
	TreeSetIgnoreCase invalidDatabaseNames = new TreeSetIgnoreCase();
	/** database names that should not be used as input databases **/
	TreeSetIgnoreCase invalidInputDatabaseNames = new TreeSetIgnoreCase();

	/**
	 * array of displayed class names, one for each entry
	 * in MOVESInstanstiator.advancedPerformanceClasses
	**/
	String[] humanNames = null;
	/** index of row containing the Emissions Calculators entry **/
	int calculatorsIndex = 0;
	/**
	 * array of "Don't Execute" flags, one for each entry
	 * in MOVESInstanstiator.advancedPerformanceClasses
	**/
	Boolean[] dontExecuteFlags = null;
	/**
	 * array of "Save Data" flags, one for each entry
	 * in MOVESInstanstiator.advancedPerformanceClasses
	**/
	Boolean[] saveDataFlags = null;
	/** Do Not Perform Final Aggregation checkbox **/
	JCheckBox doNotPerformFinalAggregationCheckbox;
	/** Tied to RunSpec.shouldTruncateMOVESOutput **/
	JCheckBox truncateMOVESOutputCheckbox;
	/** Tied to RunSpec.shouldTruncateMOVESActivityOutput **/
	JCheckBox truncateMOVESActivityOutputCheckbox;
	/** Tied to RunSpec.shouldTruncateBaseRateOutput **/
	JCheckBox truncateBaseRateOutputCheckbox;

	/** Create Input Database button **/
	JButton inputCreateButton;
	/** Refresh button **/
	JButton inputRefreshButton;
	/** Input Server text control. **/
	JTextField inputServer;
	/** Input Database combo control. **/
	ExtendedComboBox<String> inputDatabaseCombo;
	/** Input Run droplist ToolTip **/
	JToolTip inputDatabaseToolTip;
	/**
	 * Used by the server FocusLost event handler to help determine if the server
	 * name actually changed.  Thus, tabbing through the controls won't cause a reload
	 * of databases from the server TextField losing focus when the server name hasn't changed.
	**/
	String inputPreviousServer = new String();
	/** database names that should not be used **/
	TreeSetIgnoreCase inputInvalidDatabaseNames = new TreeSetIgnoreCase();

	/** Used to returns a blank, or null, renderer if a cell is empty **/
	class BlankRenderer extends DefaultTableCellRenderer {
		public Component getTableCellRendererComponent(JTable table, Object value,
				boolean isSelected, boolean hasFocus, int row, int column) {
			return null;
		}
	}

	/** Table model to display the classes and options grid. **/
	class AdvancedPerformanceFeaturesTableModel extends AbstractTableModel {
		/**
		 * Get the description for an emission process column.
		 * @param col the emission process column number
		 * @return The column description
		**/
		public String getColumnName(int col) {
			switch(col) {
				case 0:
					return "Component";
				case 1:
					return "Don't Execute";
				case 2:
					return "Save Data";
			}
			return "";
		}

		/**
		 * Get the number of rows in the table.
		 * @return The number of rows in the table
		**/
		public int getRowCount() {
			return MOVESInstantiator.advancedPerformanceClasses.length;
		}

		/**
		 * Get the number of columns in the table.
		 * @return The number of columns in the table
		**/
		public int getColumnCount() {
			return 3;
		}

		/**
		 * Get the value in a table cell.
		 * @param row The cell row number.
		 * @param col The cell column number.
		 * @return The walue stored in the table cell.
		**/
		public Object getValueAt(int row, int col) {
			switch(col) {
				case 0:
					return humanNames[row];
				case 1:
					return dontExecuteFlags[row];
				case 2:
					return saveDataFlags[row];
			}
			return null;
		}

		/**
		 * Get the class, or type, of data in a column.
		 * @param col The cell column number.
		 * @return The column class
		**/
		public Class getColumnClass(int col) {
			if(col < 1) {
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
			if(col < 1) { // If it is the component name, don't allow it to be edited
				return false;
			} else {
				return true;
			}
		}

		/**
		 * Set the value of a table cell.
		 * @param value The value to put in the cell.
		 * @param row The cell row number.
		 * @param col The cell column number.
		**/
		public void setValueAt(Object value,int row,int col) {
			switch(col) {
				case 1:
					dontExecuteFlags[row] = (Boolean)value;
					break;
				case 2:
					saveDataFlags[row] = (Boolean)value;
					break;
				default:
					return;
			}
			fireTableCellUpdated(row,col);
			assessSituation();
		}
	}

	/** Table to display the component and options grid. **/
	class AdvancedPerformanceFeaturesTable extends JTable {
		/** Renderer returned if a cell is empty **/
		BlankRenderer blankRenderer;

		/**
		 * Constructor to set the Table Model for this table.
		 * @param model The table model used for this table.
		**/
		public AdvancedPerformanceFeaturesTable(AbstractTableModel model) {
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

	/**
	 * Constructs a AdvancedPerformanceFeatures panel, also creates and sets the layout of the controls.
	**/
	public AdvancedPerformanceFeatures() {
		setupArrays();
		createControls();
		arrangeControls();
	}

	/** Fills data arrays used to track options in the grid **/
	void setupArrays() {
		humanNames = new String[MOVESInstantiator.advancedPerformanceClasses.length];
		dontExecuteFlags = new Boolean[MOVESInstantiator.advancedPerformanceClasses.length];
		saveDataFlags = new Boolean[MOVESInstantiator.advancedPerformanceClasses.length];

		calculatorsIndex = 0;
		String calculatorsName = EmissionCalculator.class.getName();
		for(int i=0;i<humanNames.length;i++) {
			humanNames[i] = MOVESInstantiator.getHumanName(
					MOVESInstantiator.advancedPerformanceClasses[i]);
			dontExecuteFlags[i] = Boolean.FALSE;
			saveDataFlags[i] = Boolean.FALSE;

			if(calculatorsName.equalsIgnoreCase(MOVESInstantiator.advancedPerformanceClasses[i])) {
				calculatorsIndex = i;
			}
		}
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		destination.append("Advanced Performance Features:\r\n");

		// Masterloopable Components
		destination.append("\tDo Not Execute:\r\n");
		for(Iterator i=runspec.classesNotToExecute.iterator();i.hasNext();) {
			String className = (String)i.next();
			destination.append("\t\t");
			destination.append(MOVESInstantiator.getHumanName(className));
			destination.append("\r\n");
		}
		destination.append("\tSave Data From:\r\n");
		for(Iterator i=runspec.classesToSaveData.iterator();i.hasNext();) {
			String className = (String)i.next();
			destination.append("\t\t");
			destination.append(MOVESInstantiator.getHumanName(className));
			destination.append("\r\n");
		}

		// Destination User Dataset
		if(runspec.shouldCopySavedGeneratorData) {
			destination.append("\tSave Generator Data\r\n");
		} else {
			destination.append("\tDo Not Save Generator Data\r\n");
		}
		// Database for holding the saved data
		String appendText = new String("");
		if((runspec.generatorDatabase.serverName != null)
				&& (runspec.generatorDatabase.serverName.length() > 0)) {
			appendText = runspec.generatorDatabase.serverName;
		} else {
			appendText = "[using default]";
		}

		destination.append("\tSaved Data Database Server Name: " + appendText + "\r\n");
		if((runspec.generatorDatabase.databaseName != null)
				&& (runspec.generatorDatabase.databaseName.length() > 0)) {
			appendText = runspec.generatorDatabase.databaseName;
		} else {
			appendText = "[using default]";
		}
		destination.append("\tSaved Data Database Name: " + appendText + "\r\n");

		// Custom default database
		appendText = "";
		if((runspec.inputDatabase.serverName != null)
				&& (runspec.inputDatabase.serverName.length() > 0)) {
			appendText = runspec.inputDatabase.serverName;
		} else {
			appendText = "[using default]";
		}
		destination.append("\tCustom Default Database Server Name: " + appendText + "\r\n");
		if((runspec.inputDatabase.databaseName != null)
				&& (runspec.inputDatabase.databaseName.length() > 0)) {
			appendText = runspec.inputDatabase.databaseName;
		} else {
			appendText = "[using default]";
		}
		destination.append("\tCustom Default Database Name: " + appendText + "\r\n");

		// Data handling flags
		if(runspec.doNotPerformFinalAggregation) {
			destination.append("\tDo Not Perform Final Aggregation\r\n");
		} else {
			destination.append("\tPerform Final Aggregation (if necessary)\r\n");
		}
		if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			if(runspec.shouldTruncateMOVESOutput) {
				destination.append("\tRemove data from MOVESOutput after creating rates");
			} else {
				destination.append("\tPreserve data in MOVESOutput after creating rates");
			}
			if(runspec.shouldTruncateMOVESActivityOutput) {
				destination.append("\tRemove data from MOVESActivityOutput after creating rates");
			} else {
				destination.append("\tPreserve data in MOVESActivityOutput after creating rates");
			}
			if(runspec.shouldTruncateBaseRateOutput) {
				destination.append("\tRemove data from BaseRateOutput after creating rates");
			} else {
				destination.append("\tPreserve data in BaseRateOutput after creating rates");
			}
		}
	}

	/**
	 * Creates and initializes all controls on this panel. And fixes the height
	 * and width of the JTable to fit in the screen.
	**/
	public void createControls() {
		topPanel = new JPanel();
		topPanel.setName("topPanel");
		topPanel.setBorder(BorderFactory.createTitledBorder(
				"Masterloopable Components"));

		table = new AdvancedPerformanceFeaturesTable(new AdvancedPerformanceFeaturesTableModel());
		table.setName("advancedPerformanceFeaturesTable");
		table.setRowSelectionAllowed(false);
		table.getDefaultEditor(Boolean.class).addCellEditorListener(this);
		tableScrollPane = new JScrollPane(table,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		tableScrollPane.setName("advancedPerformanceFeaturesTableScrollPane");
		FontMetrics fm = table.getFontMetrics(table.getFont());
		javax.swing.table.TableColumnModel colModel = table.getColumnModel();
		int numCols = colModel.getColumnCount();
		int tableHeight = 0;
		int tableWidth = 0;
		for(int i=0; i<numCols; i++) {
			String columnName = (String) colModel.getColumn(i).getHeaderValue();
			if(i==0) {
				TableColumn tableColumn = table.getColumn(table.getColumnName(i));
				int columnNumber = tableColumn.getModelIndex();
				int max = 20;
				int columnWidth = 0;
				int nrows = table.getModel().getRowCount();
				String cell = "";
				for (int j=0; j<nrows; j++) {
					String value = (String) table.getModel().getValueAt(j, columnNumber);
					columnWidth = fm.stringWidth("." + value.trim() + ".");
					if (columnWidth > max) {
						max = columnWidth;
					}
					tableHeight += table.getRowHeight();
				}
				max += 10;
				table.getColumn(columnName).setPreferredWidth(max);
				tableWidth += max;
			} else {
				int columnSize = 0;
				columnSize = fm.stringWidth(columnName)+10;
				table.getColumn(columnName).setPreferredWidth(columnSize);
				tableWidth += columnSize;
			}
		}
		if(tableScrollPane != null) {
			tableScrollPane.getViewport().setPreferredSize(new Dimension(tableWidth, tableHeight));
			table.setPreferredSize(new Dimension(tableWidth, tableHeight));
		}
		table.getTableHeader().setResizingAllowed(false);
		table.getTableHeader().setReorderingAllowed(false);
		ToolTipHelper.add(table,"Select components and their execution options");

		copySavedDataCheckbox = new JCheckBox("Copy Saved Generator Data");
		ToolTipHelper.add(copySavedDataCheckbox,
				"Check this box to save generator data");
		copySavedDataCheckbox.addActionListener(this);

		createButton = new JButton("Create Database");
		createButton.addActionListener(this);

		refreshButton = new JButton("Refresh");
		refreshButton.addActionListener(this);

		server = new JTextField(10);
		ToolTipHelper.add(server,
				"Edit the name of the server where the database will be located");
		server.setName("server");
		server.addFocusListener(this);
		server.setColumns(10);

		databaseCombo = new ExtendedComboBox<String>();
		Dimension d = databaseCombo.getPreferredSize();
		databaseCombo.setPreferredSize(new Dimension(375, d.height)); // 150
		databaseCombo.setPopupWidth(databaseCombo.getPreferredSize().width);
		databaseCombo.setName("databaseCombo");
		databaseCombo.addActionListener(this);
		databaseCombo.setEditable(true);
		databaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(databaseCombo,
				"Edit or select the name of the database in which the data will be stored");

		doNotPerformFinalAggregationCheckbox = new JCheckBox("Do Not Perform Final Aggregation");
		ToolTipHelper.add(doNotPerformFinalAggregationCheckbox,
				"Check this box to prevent data aggregation,"
				+ " which can be time consuming on large output data sets.");

		truncateMOVESOutputCheckbox = new JCheckBox("Clear MOVESOutput after rate calculations");
		ToolTipHelper.add(truncateMOVESOutputCheckbox,
				"Check this box to remove inventory data from MOVESOutput once rates have been calculated.");
		truncateMOVESActivityOutputCheckbox = new JCheckBox("Clear MOVESActivityOutput after rate calculations");
		ToolTipHelper.add(truncateMOVESActivityOutputCheckbox,
				"Check this box to remove activity and population data from MOVESActivityOutput once rates have been calculated.");
		truncateBaseRateOutputCheckbox = new JCheckBox("Clear BaseRateOutput after rate calculations");
		ToolTipHelper.add(truncateBaseRateOutputCheckbox,
				"Check this box to remove data from BaseRateOutput once rates have been calculated.");

		inputCreateButton = new JButton("Create Database");
		inputCreateButton.addActionListener(this);

		inputRefreshButton = new JButton("Refresh");
		inputRefreshButton.addActionListener(this);

		inputServer = new JTextField(10);
		ToolTipHelper.add(inputServer,
				"Edit the name of the server where the database will be located");
		inputServer.setName("inputServer");
		inputServer.addFocusListener(this);
		inputServer.setColumns(10);

		inputDatabaseCombo = new ExtendedComboBox<String>();
		d = inputDatabaseCombo.getPreferredSize();
		inputDatabaseCombo.setPreferredSize(new Dimension(375, d.height)); // 150
		inputDatabaseCombo.setPopupWidth(inputDatabaseCombo.getPreferredSize().width);
		inputDatabaseCombo.setName("inputDatabaseCombo");
		inputDatabaseCombo.addActionListener(this);
		inputDatabaseCombo.setEditable(true);
		inputDatabaseCombo.setSelectedIndex(-1);
		ToolTipHelper.add(inputDatabaseCombo,
				"Edit or select the name of the custom input database");
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		removeAll();

		topPanel.setLayout(new BorderLayout());
		topPanel.add(new JLabel(" "), BorderLayout.NORTH);
		topPanel.add(new JLabel(" "), BorderLayout.EAST);
		topPanel.add(new JLabel(" "), BorderLayout.WEST);
		topPanel.add(tableScrollPane, BorderLayout.CENTER);

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 2;
		gbc.gridheight = 2;
		gbc.weightx = 0;
		gbc.weighty = 0;
		setLayout(new GridBagLayout());
		LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
		add(topPanel, gbc);

		JPanel bottomPanel = new JPanel();
		bottomPanel.setLayout(new BoxLayout(bottomPanel,BoxLayout.Y_AXIS));
		bottomPanel.setBorder(BorderFactory.createTitledBorder(
				"Destination User Dataset"));

		JPanel p = new JPanel();
		p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
		p.add(copySavedDataCheckbox);
		p.add(Box.createHorizontalGlue());

		bottomPanel.add(p);

		// database and server panel
		JPanel outputDatabasePanel = new JPanel();
		gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;

		outputDatabasePanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		outputDatabasePanel.add(new JLabel("Server:"), gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,0, "WEST", 1, 1);
		outputDatabasePanel.add(server, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,0, "WEST", 1, 1);
		outputDatabasePanel.add(refreshButton, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		outputDatabasePanel.add(new JLabel("Database:"), gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 2, 1);
		outputDatabasePanel.add(databaseCombo, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,1, "WEST", 1, 1);
		outputDatabasePanel.add(createButton, gbc);

		bottomPanel.add(outputDatabasePanel);

		LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
		add(bottomPanel, gbc);

		JPanel dataPanel = new JPanel();
		dataPanel.setLayout(new BoxLayout(dataPanel,BoxLayout.Y_AXIS));
		dataPanel.setBorder(BorderFactory.createTitledBorder(
				"Aggregation and Data Handling"));

		p = new JPanel();
		p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
		p.add(doNotPerformFinalAggregationCheckbox);
		p.add(Box.createHorizontalGlue());
		dataPanel.add(p);

		p = new JPanel();
		p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
		p.add(truncateMOVESOutputCheckbox);
		p.add(Box.createHorizontalGlue());
		dataPanel.add(p);

		p = new JPanel();
		p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
		p.add(truncateMOVESActivityOutputCheckbox);
		p.add(Box.createHorizontalGlue());
		dataPanel.add(p);

		if(CompilationFlags.DO_RATES_FIRST) {
			p = new JPanel();
			p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
			p.add(truncateBaseRateOutputCheckbox);
			p.add(Box.createHorizontalGlue());
			dataPanel.add(p);
		}

		LayoutUtility.setPositionOnGrid(gbc,0, 2, "WEST", 1, 1);
		add(dataPanel, gbc);

		// input database and server panel
		bottomPanel = new JPanel();
		bottomPanel.setLayout(new BoxLayout(bottomPanel,BoxLayout.Y_AXIS));
		bottomPanel.setBorder(BorderFactory.createTitledBorder(
				"Custom Input Database"));

		outputDatabasePanel = new JPanel();
		gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.NONE;

		outputDatabasePanel.setLayout(new GridBagLayout());
		gbc.insets = new Insets(2,2,2,2);
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		outputDatabasePanel.add(new JLabel("Server:"), gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,0, "WEST", 1, 1);
		outputDatabasePanel.add(inputServer, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,0, "WEST", 1, 1);
		outputDatabasePanel.add(inputRefreshButton, gbc);
		LayoutUtility.setPositionOnGrid(gbc,0,1, "WEST", 1, 1);
		outputDatabasePanel.add(new JLabel("Database:"), gbc);
		LayoutUtility.setPositionOnGrid(gbc,1,1, "WEST", 2, 1);
		outputDatabasePanel.add(inputDatabaseCombo, gbc);
		LayoutUtility.setPositionOnGrid(gbc,3,1, "WEST", 1, 1);
		outputDatabasePanel.add(inputCreateButton, gbc);

		bottomPanel.add(outputDatabasePanel);

		LayoutUtility.setPositionOnGrid(gbc,0, 3, "WEST", 1, 1);
		add(bottomPanel, gbc);

		generateListOfInvalidDatabaseNames();
		loadDatabases();
		loadInputDatabases();
	}

	/**
	 * Saves information to a RunSpec.
	 * @param	runspec the RunSpec to receive the settings
	**/
	public void saveToRunSpec(RunSpec runspec) {
		// Masterloopable Components
		runspec.classesNotToExecute.clear();
		runspec.classesToSaveData.clear();
		for(int i=0;i<MOVESInstantiator.advancedPerformanceClasses.length;i++) {
			if(dontExecuteFlags[i] == Boolean.TRUE) {
				runspec.classesNotToExecute.add(MOVESInstantiator.advancedPerformanceClasses[i]);
			}
			if(saveDataFlags[i] == Boolean.TRUE) {
				runspec.classesToSaveData.add(MOVESInstantiator.advancedPerformanceClasses[i]);
			}
		}

		// Destination User Dataset
		runspec.shouldCopySavedGeneratorData = copySavedDataCheckbox.isSelected();
		if(runspec.shouldCopySavedGeneratorData) {
			runspec.generatorDatabase.serverName = server.getText();
			runspec.generatorDatabase.databaseName = databaseCombo.getSelectedItem().toString();
		} else {
			runspec.generatorDatabase.serverName = "";
			runspec.generatorDatabase.databaseName = "";
		}
		// Aggregation and data handling
		if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			runspec.doNotPerformFinalAggregation = false;
			runspec.shouldTruncateMOVESOutput = truncateMOVESOutputCheckbox.isSelected();
			runspec.shouldTruncateMOVESActivityOutput = truncateMOVESActivityOutputCheckbox.isSelected();
			if(CompilationFlags.DO_RATES_FIRST) {
				runspec.shouldTruncateBaseRateOutput = truncateBaseRateOutputCheckbox.isSelected();
			}
		} else {
			runspec.doNotPerformFinalAggregation = doNotPerformFinalAggregationCheckbox.isSelected();
			runspec.shouldTruncateMOVESOutput = true;
			runspec.shouldTruncateMOVESActivityOutput = true;
			runspec.shouldTruncateBaseRateOutput = true;
		}

		// Custom input database
		runspec.inputDatabase.serverName = inputServer.getText();
		runspec.inputDatabase.databaseName = inputDatabaseCombo.getSelectedItem().toString();
	}

	/**
	 * Loads information from a RunSpec.
	 * @param	runspec the RunSpec to get the settings from
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		// Setup defaults
		for(int i=0;i<humanNames.length;i++) {
			dontExecuteFlags[i] = Boolean.FALSE;
			saveDataFlags[i] = Boolean.FALSE;
		}

		// Masterloopable Components
		for(Iterator<String> i=runspec.classesNotToExecute.iterator();i.hasNext();) {
			String className = (String)i.next();
			for(int j=0;j<MOVESInstantiator.advancedPerformanceClasses.length;j++) {
				if(className.equalsIgnoreCase(MOVESInstantiator.advancedPerformanceClasses[j])) {
					dontExecuteFlags[j] = Boolean.TRUE;
				}
			}
		}
		for(Iterator<String> i=runspec.classesToSaveData.iterator();i.hasNext();) {
			String className = (String)i.next();
			for(int j=0;j<MOVESInstantiator.advancedPerformanceClasses.length;j++) {
				if(className.equalsIgnoreCase(MOVESInstantiator.advancedPerformanceClasses[j])) {
					saveDataFlags[j] = Boolean.TRUE;
				}
			}
		}

		// Recreate the table so that new data is loaded
		createControls();
		arrangeControls();

		// Destination User Dataset
		copySavedDataCheckbox.setSelected(runspec.shouldCopySavedGeneratorData);
		if(runspec.shouldCopySavedGeneratorData) {
			server.setText(runspec.generatorDatabase.serverName);
			addIfNotInComboBox(runspec.generatorDatabase.databaseName);
			databaseCombo.setSelectedItem(runspec.generatorDatabase.databaseName);
		} else {
			server.setText("");
			databaseCombo.setSelectedItem("");
		}

		// Aggregation and data handling
		if(runspec.scale == ModelScale.MESOSCALE_LOOKUP) {
			doNotPerformFinalAggregationCheckbox.setSelected(false);
			doNotPerformFinalAggregationCheckbox.setEnabled(false);

			truncateMOVESOutputCheckbox.setSelected(runspec.shouldTruncateMOVESOutput);
			truncateMOVESOutputCheckbox.setEnabled(true);

			truncateMOVESActivityOutputCheckbox.setSelected(runspec.shouldTruncateMOVESActivityOutput);
			truncateMOVESActivityOutputCheckbox.setEnabled(true);

			if(CompilationFlags.DO_RATES_FIRST) {
				truncateBaseRateOutputCheckbox.setSelected(runspec.shouldTruncateBaseRateOutput);
				truncateBaseRateOutputCheckbox.setEnabled(true);
			}
		} else {
			doNotPerformFinalAggregationCheckbox.setSelected(runspec.doNotPerformFinalAggregation);
			doNotPerformFinalAggregationCheckbox.setEnabled(true);

			truncateMOVESOutputCheckbox.setSelected(false);
			truncateMOVESOutputCheckbox.setEnabled(false);

			truncateMOVESActivityOutputCheckbox.setSelected(false);
			truncateMOVESActivityOutputCheckbox.setEnabled(false);

			if(CompilationFlags.DO_RATES_FIRST) {
				truncateBaseRateOutputCheckbox.setSelected(false);
				truncateBaseRateOutputCheckbox.setEnabled(false);
			}
		}

		// Custom input database
		inputServer.setText(runspec.inputDatabase.serverName);
		addIfNotInInputComboBox(runspec.inputDatabase.databaseName);
		inputDatabaseCombo.setSelectedItem(runspec.inputDatabase.databaseName);

		assessSituation();
	}

	/**
	 * Gets the RunSpec status from the current sections.
	 * @param	runspec the RunSpec to get the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus calculateRunSpecSectionStatus(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		sections.remove(getName());

		boolean isOk = true;

		boolean hasNeedToSave = false;
		for(int i=0;i<humanNames.length;i++) {
			if(i == calculatorsIndex) {
				continue;
			}
			if(dontExecuteFlags[i] == Boolean.FALSE
					&& saveDataFlags[i] == Boolean.TRUE) {
				hasNeedToSave = true;
				break;
			}
		}
		if(hasNeedToSave) {
			if(copySavedDataCheckbox.isSelected()) {
				String databaseName = databaseCombo.getSelectedItem().toString().trim();
				isOk = databaseName.length() > 0;
			} else {
				isOk = true;
			}
		} else {
			isOk = true;
		}

		RunSpecSectionStatus status;
		if(isOk) {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		} else {
			status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
		}

		sections.put(getName(),status);
		return status;
	}

	/**
	 * Sets the defaults to the RunSpec.
	 * @param	runspec the RunSpec to the description text.
	 * @param	sections TreeMap containing the current sections.
	 * @return	RunSpecSectionStatus of the RunSpec based on the sections.
	**/
	public RunSpecSectionStatus saveDefaultsToRunSpec(RunSpec runspec,
			TreeMap<String,RunSpecSectionStatus> sections) {
		runspec.classesNotToExecute.clear();
		runspec.classesToSaveData.clear();
		runspec.pollutantProcessAssociations.clear();
		runspec.shouldCopySavedGeneratorData = false;
		runspec.generatorDatabase.serverName = "";
		runspec.generatorDatabase.databaseName = "";
		runspec.generatorDatabase.description = "";
		runspec.doNotPerformFinalAggregation = false;
		runspec.inputDatabase.serverName = "";
		runspec.inputDatabase.databaseName = "";
		runspec.inputDatabase.description = "";
		runspec.shouldTruncateMOVESOutput = true;
		runspec.shouldTruncateMOVESActivityOutput = true;
		runspec.shouldTruncateBaseRateOutput = true;

		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
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
		// Nothing special to do here
		return calculateRunSpecSectionStatus(runspec,sections);
	}

	/**
	 * Attempt to open a connection to the server and database shown on screen.
	 * @param settleForJustServer true if it is OK to just connect to the server without a valid
	 * database named.
	 * @return a Connection object that should be closed with DatabaseUtilities.closeConnection
	 * and not by returning it to the DatabaseConnectionManager.
	**/
	Connection openCurrentDatabase(boolean settleForJustServer) {
		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)databaseCombo.getSelectedItem());
		Connection db = dbSelection.openConnectionOrNull();
		if(null == db && settleForJustServer) {
			// try again to get a connection, but specify an empty string for the database
			// name, in case the current one is invalid, this will at least allow us to get
			// a connection to the server to get the database list
			dbSelection.databaseName = "";
			db = dbSelection.openConnectionOrNull();
		}
		return db;
	}

	/**
	 * Attempt to open a connection to the server and database shown on screen.
	 * @param settleForJustServer true if it is OK to just connect to the server without a valid
	 * database named.
	 * @return a Connection object that should be closed with DatabaseUtilities.closeConnection
	 * and not by returning it to the DatabaseConnectionManager.
	**/
	Connection openCurrentInputDatabase(boolean settleForJustServer) {
		DatabaseSelection dbSelection = new DatabaseSelection();
		if(inputServer.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)inputDatabaseCombo.getSelectedItem());
		Connection db = dbSelection.openConnectionOrNull();
		if(null == db && settleForJustServer) {
			// try again to get a connection, but specify an empty string for the database
			// name, in case the current one is invalid, this will at least allow us to get
			// a connection to the server to get the database list
			dbSelection.databaseName = "";
			db = dbSelection.openConnectionOrNull();
		}
		return db;
	}

	/**
	 * Loads the Databases droplist, based on the server setting.
	 * Also, sets the default droplist selection, if it can be found.
	**/
	public void loadDatabases() {
		databaseCombo.removeAllItems();
		// add the default item (no selection)
		databaseCombo.addItem(new String(""));
		TreeSet<String> databases = new TreeSet<String>();
		// get the available databases from the current server selection
		Connection db = openCurrentDatabase(true);
		if(null == db) {
			Logger.log(LogMessageCategory.ERROR,"Could not connect to the database");
			return;
		}
		String sql = "SHOW DATABASES";
		PreparedStatement statement;
		ResultSet results;
		try {
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					String nextDB = results.getString(1);
					if(!invalidDatabaseNames.contains(nextDB)) {
						databases.add(nextDB);
					}
				}
				results.close();
			}
			statement.close();
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show databases (load databases).");
			DatabaseUtilities.closeConnection(db);
			return;
		}
		// second pass through the returned list of database names, must now
		// remove any databases that aren't an output database
		ArrayList<String> stringsToRemove = new ArrayList<String>();
		try {
			/*
			boolean foundOutputTable = false;
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				foundOutputTable = false;
				String nextDatabase = (String)i.next();
				if(nextDatabase.length() == 0 || invalidDatabaseNames.contains(nextDatabase)) {
					continue;
				}
				// look at all tables from the next databaseName, compare the table
				// names to one output table name
				sql = "SHOW TABLES FROM " + nextDatabase;
				try {
					statement = db.prepareStatement(sql);
					results = SQLRunner.executeQuery(statement,sql);
					if(results != null) {
						while(results.next()) {
							String nextTable = results.getString(1);
							if(nextTable.equalsIgnoreCase("IMCoverage")) {
								foundOutputTable = true;
								break;
							}
						}
					}
				} catch (Exception e) {
					// SQL error here just means this database not an output database
				}
				// check if this database has any output tables, if not must add
				// the databaseName to the remove names list
				if(!foundOutputTable) {
					stringsToRemove.add(nextDatabase);
				}
			}
			// now run through any database names to remove (i.e. databases that don't
			// contain an output table)
			for(int i = 0; i < stringsToRemove.size(); i++) {
				databases.remove(stringsToRemove.get(i));
			}
			*/
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				String nextDB = (String)i.next();
				databaseCombo.addItem(nextDB);
			}
			Vector<String> toolTipVector = new Vector<String>();
			for(int i=0; i<databaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) databaseCombo.getItemAt(i));
			}
			String[] toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			databaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show tables from database.");
		}
		// set the default selection
		databaseCombo.setSelectedItem("");
		DatabaseUtilities.closeConnection(db);
	}

	/**
	 * Loads the InputDatabases droplist, based on the server setting.
	 * Also, sets the default droplist selection, if it can be found.
	**/
	public void loadInputDatabases() {
		inputDatabaseCombo.removeAllItems();
		// add the default item (no selection)
		inputDatabaseCombo.addItem(new String(""));
		TreeSet<String> databases = new TreeSet<String>();
		// get the available databases from the current server selection
		Connection db = openCurrentInputDatabase(true);
		if(null == db) {
			Logger.log(LogMessageCategory.ERROR,"Could not connect to the input database");
			return;
		}
		String sql = "SHOW DATABASES";
		PreparedStatement statement;
		ResultSet results;
		try {
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement,sql);
			if(results != null) {
				while(results.next()) {
					String nextDB = results.getString(1);
					if(!invalidInputDatabaseNames.contains(nextDB)) {
						databases.add(nextDB);
					}
				}
				results.close();
			}
			statement.close();
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show databases (load databases).");
			DatabaseUtilities.closeConnection(db);
			return;
		}
		// second pass through the returned list of database names, must now
		// remove any databases that aren't an output database
		ArrayList<String> stringsToRemove = new ArrayList<String>();
		try {
			/*
			boolean foundOutputTable = false;
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				foundOutputTable = false;
				String nextDatabase = (String)i.next();
				if(nextDatabase.length() == 0 || invalidInputDatabaseNames.contains(nextDatabase)) {
					continue;
				}
				// look at all tables from the next databaseName, compare the table
				// names to one output table name
				sql = "SHOW TABLES FROM " + nextDatabase;
				try {
					statement = db.prepareStatement(sql);
					results = SQLRunner.executeQuery(statement,sql);
					if(results != null) {
						while(results.next()) {
							String nextTable = results.getString(1);
							if(nextTable.equalsIgnoreCase("IMCoverage")) {
								foundOutputTable = true;
								break;
							}
						}
					}
				} catch (Exception e) {
					// SQL error here just means this database not an output database
				}
				// check if this database has any output tables, if not must add
				// the databaseName to the remove names list
				if(!foundOutputTable) {
					stringsToRemove.add(nextDatabase);
				}
			}
			// now run through any database names to remove (i.e. databases that don't
			// contain an output table)
			for(int i = 0; i < stringsToRemove.size(); i++) {
				databases.remove(stringsToRemove.get(i));
			}
			*/
			for(Iterator<String> i=databases.iterator();i.hasNext();) {
				String nextDB = (String)i.next();
				inputDatabaseCombo.addItem(nextDB);
			}
			Vector<String> toolTipVector = new Vector<String>();
			for(int i=0; i<inputDatabaseCombo.getItemCount(); i++) {
				toolTipVector.add((String) inputDatabaseCombo.getItemAt(i));
			}
			String[] toolTipStringArray = new String[toolTipVector.size()];
			toolTipVector.copyInto(toolTipStringArray);
			inputDatabaseCombo.setRenderer(new TooltipComboBoxRenderer<String>(toolTipStringArray));
		} catch(Exception e) {
			//Logger.logException(e);
			Logger.logError(e, "Failed to show tables from database.");
		}
		// set the default selection
		inputDatabaseCombo.setSelectedItem("");
		DatabaseUtilities.closeConnection(db);
	}

	/**
	 * Add a database name to databaseCombo but only if it isn't already in the
	 * the list.
	 * @param newDatabaseName name of the database to attempt to place into databaseCombo
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInComboBox(String newDatabaseName) {
		newDatabaseName = newDatabaseName.trim();
		ComboBoxModel model = databaseCombo.getModel();
		for(int i = 0; i < model.getSize(); i++) {
			String t = (String)model.getElementAt(i);
			if(t.equalsIgnoreCase(newDatabaseName)) {
				return t;
			}
		}
		databaseCombo.addItem(newDatabaseName);
		return newDatabaseName;
	}

	/**
	 * Add a database name to databaseCombo but only if it isn't already in the
	 * the list.
	 * @param newDatabaseName name of the database to attempt to place into databaseCombo
	 * @return the object either added to or already in the list.  This will be the object
	 * the should be selected.
	**/
	private String addIfNotInInputComboBox(String newDatabaseName) {
		newDatabaseName = newDatabaseName.trim();
		ComboBoxModel model = inputDatabaseCombo.getModel();
		for(int i = 0; i < model.getSize(); i++) {
			String t = (String)model.getElementAt(i);
			if(t.equalsIgnoreCase(newDatabaseName)) {
				return t;
			}
		}
		inputDatabaseCombo.addItem(newDatabaseName);
		return newDatabaseName;
	}

	/** Handles the database combo change. **/
	public void processDatabaseComboChange() {
		if(databaseCombo.getSelectedItem() == null) {
			return;
		}
		if(databaseCombo.getSelectedItem().toString().length() == 0) {
			return;
		}

		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName = StringUtilities.safeGetString(newDatabaseName);

		String status = MOVESEngine.isOutputDatabaseNameValid(dbSelection);
		if(status == null) {
			String sql;
			PreparedStatement statement;

			// try to connect to the new selection
			Connection db = openCurrentDatabase(false);
			if(null == db) {
				return;
			}
			try {
				sql = "SELECT COUNT(*) FROM IMCoverage";
				statement = db.prepareStatement(sql);
				ResultSet results = SQLRunner.executeQuery(statement,sql);
				if(results != null) {
					results.next();
					int count = results.getInt(1);
					results.close();
				}
				statement.close();
			} catch(Exception e) {
			}
			DatabaseUtilities.closeConnection(db);
			newDatabaseName = addIfNotInComboBox(newDatabaseName);
			databaseCombo.setSelectedItem(newDatabaseName);
		} else {
			JOptionPane.showMessageDialog(this, status);
		}
	}

	/** Handles the input database combo change. **/
	public void processInputDatabaseComboChange() {
		if(inputDatabaseCombo.getSelectedItem() == null) {
			return;
		}
		if(inputDatabaseCombo.getSelectedItem().toString().length() == 0) {
			return;
		}

		String newDatabaseName = inputDatabaseCombo.getSelectedItem().toString();
		if(newDatabaseName == null || newDatabaseName.length() == 0) {
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(inputServer.getText().length() > 0) {
			dbSelection.serverName = inputServer.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName = StringUtilities.safeGetString(newDatabaseName);

		String status = isAlternateInputDatabaseNameValid(dbSelection);
		if(status == null) {
			String sql;
			PreparedStatement statement;

			// try to connect to the new selection
			Connection db = openCurrentInputDatabase(false);
			if(null == db) {
				return;
			}
			try {
				sql = "SELECT COUNT(*) FROM IMCoverage";
				statement = db.prepareStatement(sql);
				ResultSet results = SQLRunner.executeQuery(statement,sql);
				if(results != null) {
					results.next();
					int count = results.getInt(1);
					results.close();
				}
				statement.close();
			} catch(Exception e) {
			}
			DatabaseUtilities.closeConnection(db);
			newDatabaseName = addIfNotInInputComboBox(newDatabaseName);
			inputDatabaseCombo.setSelectedItem(newDatabaseName);
		} else {
			JOptionPane.showMessageDialog(this, status);
		}
	}

	/**
	 * Validate the Alternate Input Database Name
	 * @param  dbSelection the database selection.
	 * @return String null if valid, a message if not valid.
	 **/
	public static String isAlternateInputDatabaseNameValid(DatabaseSelection dbSelection) {
		String message = null;
		if(dbSelection.databaseName.trim().length()==0) {
			/**
			 * @issue The alternate input database name cannot be blank.
			 * @explain An input database name cannot be blank.
			**/
			message = "The alternate input database name cannot be blank.";
			return message;
		}

		DatabaseSelection defaultDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
		//execution database cannot be accepted as an input selection
		DatabaseSelection executionDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()];
		if ((executionDB.databaseName).equalsIgnoreCase(dbSelection.databaseName)) {
			/**
			 * @issue The execution database cannot be used as an input database.
			 * @explain Simulation input cannot come from the temporary MOVESExecution database.
			**/
			message = "The execution database cannot be used as an input database.";
			return message;
		}
		//Worker database cannot be accepted as an input selection
		if (dbSelection.databaseName.equalsIgnoreCase("MOVESWorker")) {
			/**
			 * @issue MOVESWorker database cannot be used as an input database.
			 * @explain Simulation input cannot come from the temporary MOVESWorker database.
			**/
			message = "MOVESWorker database cannot be used as an input database.";
			return message;
		}
		//MySQL database cannot be accepted as an input selection
		if (dbSelection.databaseName.equalsIgnoreCase("MySQL")) {
			/**
			 * @issue MySQL database cannot be used as an input database.
			 * @explain Simulation input cannot come from the system MySQL database.
			**/
			message = "MySQL database cannot be used as an input database.";
			return message;
		}
		return message;
	}

	/**
	 * Handles the focus lost event for the server textfield, and checkboxes.
	 * @param	e The FocusEvent to be handled.
	**/
	public void focusLost(FocusEvent e) {
		JComponent c = (JComponent)e.getComponent();
		if(c == server) {
			if(previousServer.equals(server.getText())) {
				return;
			}
			previousServer = server.getText();
			loadDatabases();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(c == inputServer) {
			if(inputPreviousServer.equals(inputServer.getText())) {
				return;
			}
			inputPreviousServer = inputServer.getText();
			loadInputDatabases();
		}
	}

	/**
	 * Currently not used.
	 * @param e event that gave the focus.
	**/
	public void focusGained(FocusEvent e) {
	}

	/**
	 * Calls the appropriate button handler.
	 * @param e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == createButton) {
			handleCreateButton();
		} else if(e.getSource() == refreshButton) {
			loadDatabases();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == copySavedDataCheckbox) {
			assessSituation();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == inputCreateButton) {
			handleInputCreateButton();
		} else if(e.getSource() == inputRefreshButton) {
			loadInputDatabases();
		} else if(e.getSource() == databaseCombo) {
			processDatabaseComboChange();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == inputDatabaseCombo) {
			processInputDatabaseComboChange();
		}
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

	}

	/** Handle the Create button **/
	void handleCreateButton() {
		String newDatabaseName = databaseCombo.getSelectedItem().toString();
		if(newDatabaseName.length() == 0) {
			JOptionPane.showMessageDialog(this,"Specify a database name.");
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(server.getText().length() > 0) {
			dbSelection.serverName = server.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)databaseCombo.getSelectedItem());
		if(validDatabaseName(dbSelection)) {
			Connection db = dbSelection.openConnectionOrNull();
			boolean databaseAlreadyExists = db != null;
			DatabaseUtilities.closeConnection(db);
			db = null;
			if(databaseAlreadyExists) {
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Database already exists, no action taken.", 60));
			} else if(dbSelection.safeCreateDatabase("database/CreateDefault.sql") == DatabaseSelection.CREATED) {
				// show a success message and add this item to the list,
				// in case the user hits the droplist button
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Database successfully created.", 60));
				newDatabaseName = addIfNotInComboBox(newDatabaseName);
			} else {
				Logger.log(LogMessageCategory.ERROR,
						"Could not create the database.");
			}
		}
	}

	/** Handle the Input Create button **/
	void handleInputCreateButton() {
		String newDatabaseName = inputDatabaseCombo.getSelectedItem().toString();
		if(newDatabaseName.length() == 0) {
			JOptionPane.showMessageDialog(this,"Specify a database name.");
			return;
		}

		DatabaseSelection dbSelection = new DatabaseSelection();
		if(inputServer.getText().length() > 0) {
			dbSelection.serverName = inputServer.getText();
		} else {
			dbSelection.serverName = SystemConfiguration.getTheSystemConfiguration().
					databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].serverName;
		}
		dbSelection.databaseName =
				StringUtilities.safeGetString((String)inputDatabaseCombo.getSelectedItem());
		if(validInputDatabaseName(dbSelection)) {
			Connection db = dbSelection.openConnectionOrNull();
			boolean databaseAlreadyExists = db != null;
			DatabaseUtilities.closeConnection(db);
			db = null;
			if(databaseAlreadyExists) {
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Database already exists, no action taken.", 60));
			} else if(dbSelection.safeCreateDatabase("database/CreateDefault.sql") == DatabaseSelection.CREATED) {
				// show a success message and add this item to the list,
				// in case the user hits the droplist button
				JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Database successfully created.", 60));
				newDatabaseName = addIfNotInInputComboBox(newDatabaseName);
			} else {
				Logger.log(LogMessageCategory.ERROR,
						"Could not create or update the database.");
			}
		}
	}

	/** Fill invalidDatabaseNames **/
	void generateListOfInvalidDatabaseNames() {
		DatabaseSelection defaultDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
		//default database cannot be accepted as an input selection
		invalidDatabaseNames.add(defaultDB.databaseName);
		//invalidInputDatabaseNames.add(defaultDB.databaseName);
		//execution database cannot be accepted as an input selection
		DatabaseSelection executionDB = SystemConfiguration.getTheSystemConfiguration().
				databaseSelections[MOVESDatabaseType.EXECUTION.getIndex()];
		invalidDatabaseNames.add(executionDB.databaseName);
		invalidInputDatabaseNames.add(executionDB.databaseName);
		//Worker database cannot be accepted as an input selection
		invalidDatabaseNames.add("MOVESWorker");
		invalidInputDatabaseNames.add("MOVESWorker");
		//MySQL database cannot be accepted as an input selection
		invalidDatabaseNames.add("MySQL");
		invalidInputDatabaseNames.add("MySQL");
	}

	/**
	 * Check a proposed database selection for appropriateness, prompting the user
	 * if the database is not acceptable.
	 * @param dbSelection the database selection.
	 * @return true if the database name is valid.
	**/
	boolean validDatabaseName(DatabaseSelection dbSelection) {
		if(invalidDatabaseNames.contains(dbSelection.databaseName)) {
			JOptionPane.showMessageDialog(this,
					"The " + dbSelection.databaseName + " database cannot be used here.");
			return false;
		}

		return true;
	}

	/**
	 * Check a proposed database selection for appropriateness, prompting the user
	 * if the database is not acceptable.
	 * @param dbSelection the database selection.
	 * @return true if the database name is valid.
	**/
	boolean validInputDatabaseName(DatabaseSelection dbSelection) {
		if(invalidInputDatabaseNames.contains(dbSelection.databaseName)) {
			JOptionPane.showMessageDialog(this,
					"The " + dbSelection.databaseName + " database cannot be used here.");
			return false;
		}

		return true;
	}

	/** Enable/Disable controls based on selections **/
	void assessSituation() {
		boolean hasNeedToSave = false;
		for(int i=0;i<humanNames.length;i++) {
			if(i == calculatorsIndex) {
				continue;
			}
			if(dontExecuteFlags[i] == Boolean.FALSE
					&& saveDataFlags[i] == Boolean.TRUE) {
				hasNeedToSave = true;
				break;
			}
		}
		if(hasNeedToSave) {
			copySavedDataCheckbox.setEnabled(true);
			if(copySavedDataCheckbox.isSelected()) {
				createButton.setEnabled(true);
				refreshButton.setEnabled(true);
				server.setEnabled(true);
				databaseCombo.setEnabled(true);
			} else {
				createButton.setEnabled(false);
				refreshButton.setEnabled(false);
				server.setEnabled(false);
				databaseCombo.setEnabled(false);
			}
		} else {
			copySavedDataCheckbox.setEnabled(false);
			createButton.setEnabled(false);
			refreshButton.setEnabled(false);
			server.setEnabled(false);
			databaseCombo.setEnabled(false);

			copySavedDataCheckbox.setSelected(false);
		}
		if(!server.isEnabled()) {
			server.setText("");
			databaseCombo.setSelectedItem("");
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
		// Nothing to do here
		return null;
	}
}
