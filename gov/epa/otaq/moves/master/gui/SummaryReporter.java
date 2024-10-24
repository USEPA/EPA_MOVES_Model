  /**************************************************************************************************
 * @(#)SummaryReporter.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.io.*;
import java.util.*;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.sql.*;

/**
 * @author		EPA-Mitch C.
 * @author		Wesley Faler
 * @author		Ed Glover Wm Aikman NO NO2 SO2
 * @author		Tim Hull
 * @author  	Bill Shaw (508 compliance mods)
 * @author  	John Covey (Task 2003)
 * @version		2020-07-28
**/
public class SummaryReporter {
	/** the JFrame to display GUI components in **/
	JFrame win;
	/** connection to the MOVESOutput database **/
	Connection oConn;
    /** the default database to use **/
    String defaultDatabaseName;
	/** the run specification which may be assumed was
	 used to produce the MOVESOutput database **/
	RunSpec runSpec;
	/** list of all possible ReportClassificationColumns **/
	ArrayList<ReportClassificationColumn> reportClassificationColumns;
	/** a table of pollutant column names stored as a TreeMap
	 *  key is pollutant database ID, object is column name **/
	TreeMap<Integer,String> polColumnNames;

	/** the emissionProcess to be reported, null value means to sum all processes **/
	Integer emissionProcess;
	/** the report description **/
	String reportDescription = "Summary Report";
	/** saved value of report description **/
	// storing such items here implies all objects of this class will
	// use this same location, only one will exist at a time
	static String savedReportDescription;
	/** the base name for the output tables and files **/
	String baseReportTableName = "SummaryReport";
	/** saved value of the base name for output tables and files **/
	static String savedBaseReportTableName;
	/** the name of the header report output table **/
	String headerTableName;
	/** the name of the report body output table **/
	String bodyTableName;
	/** the name of the category field value decoding table **/
	String decodeTableName;
	/** an ordered list of MOVES run numbers to be included in the report **/
	ArrayList<String> runNumSelections = new ArrayList<String>();
	/** list of MOVES run numbers available to be selected to be included in the report **/
	ArrayList<String> runNumbers ;
	/** list of Strings to be displayed for run numbers available to be included in the report **/
	ArrayList<String> runDisplayStrings;
	/** an ordered list of category fields to be used in the report **/
	ArrayList<String> categoryFieldSelections = new ArrayList<String>();
	/** an ordered list of pollutant columns to be included in the report **/
	ArrayList<String> pollutantSelections = new ArrayList<String>();
	/** a list of classification columns appropriate to run spec **/
	ArrayList<String> categories = new ArrayList<String>();
	/** a TreeSet of pollutant names included in runspec for the selected process **/
	TreeSet<String> pollutants = new TreeSet<String>();
	/** header line for report header **/
	String headerColumnHeaderLine;
	/** vector holding report header data **/
	Vector<String> headerVector;
	/** header line for report body **/
	String bodyColumnHeaderLine;
	/** vector holding report body data **/
	Vector<String> bodyVector;
	/** header line for report decode **/
	String decodeColumnHeaderLine;
	/** vector holding report decode data **/
	Vector<String> decodeVector;
	/** indication whether or not distance is to be included in the report **/
	boolean isDistanceIncluded;
    // report class code
    /** report parameter dialog box **/
    ReportParamGetter reportParamGetter;
    /** indicates status of the report parameter dialog 0 = cancel 1 = ok **/
	int reportDialogResult = 0;
	private class ReportParamGetter extends JDialog {
		JLabel screenHeader;
		JLabel reportDescriptionLabel;
		JTextField reportDescriptionField;
		JLabel reportTableNameLabel;
		JTextField reportTableNameField;
		JLabel runNumLabel;
		JLabel categoryLabel;
		JLabel dataItemsLabel;
		JScrollPane runNumScrollPane;
		JList<String> runNumList;
		JScrollPane categoryScrollPane;
		JList<String> categoryList;
		JScrollPane dataItemsScrollPane;
		JList<String> dataItemsList;
		JPanel runNumAddPanel;
		JButton runNumAdd;
		JButton runNumAddAll;
		JPanel categoryAddPanel;
		JButton categoryAdd;
		JButton categoryAddAll;
		JPanel dataItemsAddPanel;
		JButton dataItemsAdd;
		JButton dataItemsAddAll;
		JLabel runNumSelectionLabel;
		JLabel categorySelectionLabel;
		JLabel dataItemsSelectionLabel;
		JScrollPane runNumSelectionScrollPane;
		JList<String> runNumSelectionList;
		JScrollPane categorySelectionScrollPane;
		JList<String> categorySelectionList;
		JPanel categoryUpDownPanel;
		JButton categoryUp;
		JButton categoryDown;
		JScrollPane dataItemsSelectionScrollPane;
		JList<String> dataItemsSelectionList;
		JPanel dataItemsUpDownPanel;
		JButton dataItemsUp;
		JButton dataItemsDown;
		JPanel runNumRemovePanel;
		JButton runNumRemove;
		JButton runNumRemoveAll;
		JPanel categoryRemovePanel;
		JButton categoryRemove;
		JButton categoryRemoveAll;
		JPanel dataItemsRemovePanel;
		JButton dataItemsRemove;
		JButton dataItemsRemoveAll;
		JPanel OKCancelPanel;
		JButton okButton;
		JButton cancelButton;
		DefaultListModel<String> runNumSelectionModel;
		DefaultListModel<String> categorySelectionModel;
		DefaultListModel<String> dataItemsSelectionModel;

		public ReportParamGetter(JFrame win) {
			super(win,MOVESWindow.MOVES_VERSION + " - Specify Parameters for Summary Report");
			createControls();
			arrangeControls();
		}

		private void createControls() {
			// Based on code generated by JFormDesigner
			// Modified to initialize run data from MOVES DB
			String eprocName = null;
			if (emissionProcess == null) {
				eprocName = "Total of All";
			} else {
				eprocName = EmissionProcess.findByID(emissionProcess.intValue()).processName;
			}
			screenHeader = new JLabel("Specify Report for Emission Process: " + eprocName);
			reportDescriptionLabel = new JLabel("Report Description:  ");
			reportDescriptionField = new JTextField(reportDescription);
			reportTableNameLabel = new JLabel("Report Table Name:  ");
			reportTableNameField = new JTextField(baseReportTableName);
			runNumLabel = new JLabel("Run Number(s)");
			categoryLabel = new JLabel("Categories");
			dataItemsLabel = new JLabel("Data Items");
			runNumScrollPane = new JScrollPane();
			runNumList = new JList<String>(runDisplayStrings.toArray(new String[0]));
			runNumList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			runNumList.setSelectedIndex(runDisplayStrings.size()-1);
			runNumList.setToolTipText(Constants.SUMMARY_REPORT_RUN_NUMBERS_OPTIONS_LIST_TOOLTIP);
			runNumAddPanel = new JPanel();
			
			runNumAdd = new JButton("Add (ALT+0)");
			runNumAdd.setMnemonic('0');
			runNumAdd.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					add(runNumList,runNumSelectionList);
				}
			});
			runNumAdd.setToolTipText(Constants.SUMMARY_REPORT_RUN_NUMBERS_ADD_TOOLTIP);

			runNumAddAll = new JButton("Add All (ALT+1)");
			runNumAddAll.setMnemonic('1');
			runNumAddAll.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					addAll(runNumList,runNumSelectionList);
				}
			});
			runNumAddAll.setToolTipText(Constants.SUMMARY_REPORT_RUN_NUMBERS_ADD_ALL_TOOLTIP);

			categoryScrollPane = new JScrollPane();
			categoryList = new JList<String>(categories.toArray(new String[0]));
			categoryList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			categoryList.setToolTipText(Constants.SUMMARY_REPORT_RUN_NUMBERS_OPTIONS_LIST_TOOLTIP);
			categoryAddPanel = new JPanel();
			
			categoryAdd = new JButton("Add (ALT+2)");
			categoryAdd.setMnemonic('2');
			categoryAdd.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					add(categoryList,categorySelectionList);
				}
			});
			categoryAdd.setToolTipText(Constants.SUMMARY_REPORT_CATEGORIES_ADD_TOOLTIP);

			categoryAddAll = new JButton("Add All (ALT+3)");
			categoryAddAll.setMnemonic('3');
			categoryAddAll.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					addAll(categoryList,categorySelectionList);
				}
			});
			categoryAddAll.setToolTipText(Constants.SUMMARY_REPORT_CATEGORIES_ADD_ALL_TOOLTIP);

			dataItemsScrollPane = new JScrollPane();
			dataItemsList = new JList<String>(pollutants.toArray(new String[0]));
			dataItemsList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			dataItemsList.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_OPTIONS_LIST_TOOLTIP);
			dataItemsAddPanel = new JPanel();
			dataItemsAdd = new JButton("Add (ALT+4)");
			dataItemsAdd.setMnemonic('4');
			dataItemsAdd.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					add(dataItemsList,dataItemsSelectionList);
				}
			});
			dataItemsAdd.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_ADD_TOOLTIP);

			dataItemsAddAll = new JButton("Add All (ALT+5)");
			dataItemsAddAll.setMnemonic('5');
			dataItemsAddAll.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					addAll(dataItemsList,dataItemsSelectionList);
				}
			});
			dataItemsAddAll.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_ADD_ALL_TOOLTIP);

			runNumSelectionLabel = new JLabel("Selection");
			categorySelectionLabel = new JLabel("Selection");
			dataItemsSelectionLabel = new JLabel("Selection");
			runNumSelectionScrollPane = new JScrollPane();
			runNumSelectionModel = new DefaultListModel<String>();
			runNumSelectionList = new JList<String>(runNumSelectionModel);
			runNumSelectionList.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_ASSIGNED_LIST_TOOLTIP);			
			runNumRemovePanel = new JPanel();
			runNumRemove = new JButton("Remove (ALT+6)");
			runNumRemove.setMnemonic('6');
			runNumRemove.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					remove(runNumSelectionList);
				}
			});
			runNumRemove.setToolTipText(Constants.SUMMARY_REPORT_RUN_NUMBERS_REMOVE_TOOLTIP);

			runNumRemoveAll = new JButton("Remove All (ALT+7)");
			runNumRemoveAll.setMnemonic('7');
			runNumRemoveAll.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					removeAll(runNumSelectionList);
				}
			});
			runNumRemoveAll.setToolTipText(Constants.SUMMARY_REPORT_RUN_NUMBERS_REMOVE_ALL_TOOLTIP);

			categorySelectionScrollPane = new JScrollPane();
			categorySelectionModel = new DefaultListModel<String>();
			categorySelectionList = new JList<String>(categorySelectionModel);
			categorySelectionList.setToolTipText(Constants.SUMMARY_REPORT_CATEGORIES_ASSIGNED_LIST_TOOLTIP);			

			categoryRemovePanel = new JPanel();
			categoryRemove = new JButton("Remove (ALT+8)");
			categoryRemove.setMnemonic('8');
			categoryRemove.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					remove(categorySelectionList);
				}
			});
			categoryRemove.setToolTipText(Constants.SUMMARY_REPORT_CATEGORIES_REMOVE_TOOLTIP);

			categoryRemoveAll = new JButton("Remove All (ALT+9)");
			categoryRemoveAll.setMnemonic('9');
			categoryRemoveAll.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					removeAll(categorySelectionList);
				}
			});
			categoryRemoveAll.setToolTipText(Constants.SUMMARY_REPORT_CATEGORIES_REMOVE_ALL_TOOLTIP);

			dataItemsSelectionScrollPane = new JScrollPane();
			dataItemsSelectionModel = new DefaultListModel<String>();
			dataItemsSelectionList = new JList<String>(dataItemsSelectionModel);
			dataItemsSelectionList.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_ASSIGNED_LIST_TOOLTIP);			

			dataItemsRemovePanel = new JPanel();
			dataItemsRemove = new JButton("Remove");
			dataItemsRemove.setMnemonic('R');
			dataItemsRemove.setDisplayedMnemonicIndex(0);
			dataItemsRemove.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					remove(dataItemsSelectionList);
				}
			});
			dataItemsRemove.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_REMOVE_TOOLTIP);

			dataItemsRemoveAll = new JButton("Remove All");
			dataItemsRemoveAll.setMnemonic('e');
			dataItemsRemoveAll.setDisplayedMnemonicIndex(1);
			dataItemsRemoveAll.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					removeAll(dataItemsSelectionList);
				}
			});
			dataItemsRemoveAll.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_REMOVE_ALL_TOOLTIP);

			categoryUpDownPanel = new JPanel();
			categoryUp = new JButton("\u2191 Up");
			categoryUp.setMnemonic('U');
			categoryUp.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					up(categorySelectionList);
				}
			});
			categoryUp.setToolTipText(Constants.SUMMARY_REPORT_CATEGORIES_MOVE_UP_TOOLTIP);
			
			categoryDown = new JButton("\u2193 Dn");
			categoryDown.setMnemonic('D');
			categoryDown.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					down(categorySelectionList);
				}
			});
			categoryDown.setToolTipText(Constants.SUMMARY_REPORT_CATEGORIES_MOVE_DOWN_TOOLTIP);

			dataItemsUpDownPanel = new JPanel();
			dataItemsUp = new JButton("\u2191 Up" );
			dataItemsUp.setMnemonic('p');
			dataItemsUp.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					up(dataItemsSelectionList);
				}
			});
			dataItemsUp.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_MOVE_UP_TOOLTIP);

			dataItemsDown = new JButton("\u2193 Dn");
			dataItemsDown.setMnemonic('n');

			dataItemsDown.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					down(dataItemsSelectionList);
				}
			});
			dataItemsDown.setToolTipText(Constants.SUMMARY_REPORT_DATA_ITEMS_MOVE_DOWN_TOOLTIP);
			
			OKCancelPanel = new JPanel();
			okButton = new JButton("OK");
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					ok();
				}
			});
			okButton.setName("okButton");
			okButton.setMnemonic('O');
			okButton.setDisplayedMnemonicIndex(0);
			okButton.setToolTipText(Constants.SUMMARY_REPORT_OK_TOOLTIP);
			cancelButton = new JButton("Cancel");
			cancelButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					cancel();
				}
			});
			cancelButton.setName("cancelButton");
			cancelButton.setMnemonic('C');
			cancelButton.setDisplayedMnemonicIndex(0);
			cancelButton.setToolTipText(Constants.SUMMARY_REPORT_CANCEL_TOOLTIP);

			if (savedReportDescription != null) {
				reportDescriptionField.setText(savedReportDescription) ;
			}
			if (savedBaseReportTableName != null) {
				reportTableNameField.setText(savedBaseReportTableName) ;
			}
		}

		private void arrangeControls() {
			// Based on code generated by JFormDesigner
			// Modified to only lay out (objects initialized in createControls() )
			//======== this ========
			setLayout(new GridBagLayout());
			((GridBagLayout)getContentPane().getLayout()).columnWidths = new int[] {3, 112, 152, 42, 262, 42, 262, 42, 1, 0};
			((GridBagLayout)getContentPane().getLayout()).rowHeights = new int[] {25, 25, 25, 12, 0, 130, 0, 12, 0, 129, 0, 22, 0, 10, 0};
			((GridBagLayout)getContentPane().getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0E-4};
			((GridBagLayout)getContentPane().getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4};

			//---- screenHeader ----
			add(screenHeader, new GridBagConstraints(1, 0, 7, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.VERTICAL,
				new Insets(0, 0, 2, 2), 0, 0));

			//---- reportDescriptionLabel ----
			add(reportDescriptionLabel, new GridBagConstraints(1, 1, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));
			add(reportDescriptionField, new GridBagConstraints(2, 1, 6, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//---- reportTableNameLabel ----
			add(reportTableNameLabel, new GridBagConstraints(1, 2, 3, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));
			add(reportTableNameField, new GridBagConstraints(2, 2, 6, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//---- runNumLabel ----
			add(runNumLabel, new GridBagConstraints(1, 4, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.VERTICAL,
				new Insets(0, 0, 2, 2), 0, 0));

			//---- categoryLabel ----
			add(categoryLabel, new GridBagConstraints(4, 4, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.VERTICAL,
				new Insets(0, 0, 2, 2), 0, 0));

			//---- dataItemsLabel ----
			add(dataItemsLabel, new GridBagConstraints(6, 4, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.VERTICAL,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== runNumScrollPane ========
			{
				runNumScrollPane.setViewportView(runNumList);
			}
			add(runNumScrollPane, new GridBagConstraints(1, 5, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== categoryScrollPane ========
			{
				categoryScrollPane.setViewportView(categoryList);
			}
			add(categoryScrollPane, new GridBagConstraints(4, 5, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== dataItemsScrollPane ========
			{
				dataItemsScrollPane.setViewportView(dataItemsList);
			}
			add(dataItemsScrollPane, new GridBagConstraints(6, 5, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== runNumAddPanel ========
			{
				runNumAddPanel.setLayout(new BoxLayout(runNumAddPanel, BoxLayout.X_AXIS));
				runNumAddPanel.add(new JPanel(null));

				//---- runNumAdd ----
				runNumAddPanel.add(runNumAdd);
				runNumAddPanel.add(new JPanel(null));

				//---- runNumAddAll ----
				runNumAddPanel.add(runNumAddAll);
				runNumAddPanel.add(new JPanel(null));
			}
			add(runNumAddPanel, new GridBagConstraints(1, 6, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== categoryAddPanel ========
			{
				categoryAddPanel.setLayout(new BoxLayout(categoryAddPanel, BoxLayout.X_AXIS));
				categoryAddPanel.add(new JPanel(null));

				//---- categoryAdd ----
				categoryAddPanel.add(categoryAdd);
				categoryAddPanel.add(new JPanel(null));

				//---- categoryAddAll ----
				categoryAddPanel.add(categoryAddAll);
				categoryAddPanel.add(new JPanel(null));
			}
			add(categoryAddPanel, new GridBagConstraints(4, 6, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== dataItemsAddPanel ========
			{
				dataItemsAddPanel.setLayout(new BoxLayout(dataItemsAddPanel, BoxLayout.X_AXIS));
				dataItemsAddPanel.add(new JPanel(null));

				//---- dataItemsAdd ----
				dataItemsAddPanel.add(dataItemsAdd);
				dataItemsAddPanel.add(new JPanel(null));

				//---- dataItemsAddAll ----
				dataItemsAddPanel.add(dataItemsAddAll);
				dataItemsAddPanel.add(new JPanel(null));
			}
			add(dataItemsAddPanel, new GridBagConstraints(6, 6, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//---- runNumSelectionLabel ----
			add(runNumSelectionLabel, new GridBagConstraints(1, 8, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.VERTICAL,
				new Insets(0, 0, 2, 2), 0, 0));

			//---- categorySelectionLabel ----
			add(categorySelectionLabel, new GridBagConstraints(4, 8, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.VERTICAL,
				new Insets(0, 0, 2, 2), 0, 0));

			//---- dataItemsSelectionLabel ----
			add(dataItemsSelectionLabel, new GridBagConstraints(6, 8, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.VERTICAL,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== runNumSelectionScrollPane ========
			{
				runNumSelectionScrollPane.setViewportView(runNumSelectionList);
			}
			add(runNumSelectionScrollPane, new GridBagConstraints(1, 9, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== categorySelectionScrollPane ========
			{
				categorySelectionScrollPane.setViewportView(categorySelectionList);
			}
			add(categorySelectionScrollPane, new GridBagConstraints(4, 9, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== categoryUpDownPanel ========
			{
				categoryUpDownPanel.setLayout(new BoxLayout(categoryUpDownPanel, BoxLayout.Y_AXIS));
				categoryUpDownPanel.add(new JPanel(null));

				//---- categoryUp ----
				categoryUpDownPanel.add(categoryUp);
				categoryUpDownPanel.add(new JPanel(null));

				//---- categoryDown ----
				categoryUpDownPanel.add(categoryDown);
				categoryUpDownPanel.add(new JPanel(null));
			}
			add(categoryUpDownPanel, new GridBagConstraints(5, 9, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== dataItemsSelectionScrollPane ========
			{
				dataItemsSelectionScrollPane.setViewportView(dataItemsSelectionList);
			}
			add(dataItemsSelectionScrollPane, new GridBagConstraints(6, 9, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== dataItemsUpDownPanel ========
			{
				dataItemsUpDownPanel.setLayout(new BoxLayout(dataItemsUpDownPanel, BoxLayout.Y_AXIS));
				dataItemsUpDownPanel.add(new JPanel(null));

				//---- dataItemsUp ----
				dataItemsUpDownPanel.add(dataItemsUp);
				dataItemsUpDownPanel.add(new JPanel(null));

				//---- dataItemsDown ----
				dataItemsUpDownPanel.add(dataItemsDown);
				dataItemsUpDownPanel.add(new JPanel(null));
			}
			add(dataItemsUpDownPanel, new GridBagConstraints(7, 9, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== runNumRemovePanel ========
			{
				runNumRemovePanel.setLayout(new BoxLayout(runNumRemovePanel, BoxLayout.X_AXIS));
				runNumRemovePanel.add(new JPanel(null));

				//---- runNumRemove ----
				runNumRemovePanel.add(runNumRemove);
				runNumRemovePanel.add(new JPanel(null));

				//---- runNumRemoveAll ----
				runNumRemovePanel.add(runNumRemoveAll);
				runNumRemovePanel.add(new JPanel(null));
			}
			add(runNumRemovePanel, new GridBagConstraints(1, 10, 2, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== categoryRemovePanel ========
			{
				categoryRemovePanel.setLayout(new BoxLayout(categoryRemovePanel, BoxLayout.X_AXIS));
				categoryRemovePanel.add(new JPanel(null));

				//---- categoryRemove ----
				categoryRemovePanel.add(categoryRemove);
				categoryRemovePanel.add(new JPanel(null));

				//---- categoryRemoveAll ----
				categoryRemovePanel.add(categoryRemoveAll);
				categoryRemovePanel.add(new JPanel(null));
			}
			add(categoryRemovePanel, new GridBagConstraints(4, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== dataItemsRemovePanel ========
			{
				dataItemsRemovePanel.setLayout(new BoxLayout(dataItemsRemovePanel, BoxLayout.X_AXIS));
				dataItemsRemovePanel.add(new JPanel(null));

				//---- dataItemsRemove ----
				dataItemsRemovePanel.add(dataItemsRemove);
				dataItemsRemovePanel.add(new JPanel(null));

				//---- dataItemsRemoveAll ----
				dataItemsRemovePanel.add(dataItemsRemoveAll);
				dataItemsRemovePanel.add(new JPanel(null));
			}
			add(dataItemsRemovePanel, new GridBagConstraints(6, 10, 1, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));

			//======== OKCancelPanel ========
			{
				OKCancelPanel.setLayout(new BoxLayout(OKCancelPanel, BoxLayout.X_AXIS));
				OKCancelPanel.add(new JPanel(null));

				//---- okButton ----
				OKCancelPanel.add(okButton);

				//---- cancelButton ----
				OKCancelPanel.add(cancelButton);
				OKCancelPanel.add(new JPanel(null));
			}
			add(OKCancelPanel, new GridBagConstraints(1, 12, 7, 1, 0.0, 0.0,
				GridBagConstraints.CENTER, GridBagConstraints.BOTH,
				new Insets(0, 0, 2, 2), 0, 0));
			// JFormDesigner - End of component initialization  //GEN-END:initComponents
		}

		private <T> void add(JList<T> from, JList<T> to)
		{
			java.util.List<T> selection = from.getSelectedValuesList();
			DefaultListModel<T> model = (DefaultListModel<T>) to.getModel();
			for(Iterator<T> i=selection.iterator();i.hasNext();) {
				boolean foundMatch = false;
				T nextSelectedItem = i.next();
				for(int j=0;j<model.getSize();j++) {
					T selectedItem = model.getElementAt(j);
					if(selectedItem.equals(nextSelectedItem)) {
						foundMatch = true;
						break;
					}
				}
				if(!foundMatch) {
					model.addElement(nextSelectedItem);
				}
			}
			from.clearSelection();
		}

		private <T> void addAll(JList<T> from, JList<T> to)
		{
			ListModel<T> fromModel = from.getModel();
			DefaultListModel<T> toModel = (DefaultListModel<T>)to.getModel();
			toModel.clear();
			for(int i=0;i<fromModel.getSize();i++) {
				T item = fromModel.getElementAt(i);
				toModel.addElement(item);
			}
			from.clearSelection();
		}

		private <T> void remove(JList<T> from)
		{
			java.util.List<T> selectedValues = from.getSelectedValuesList();
			DefaultListModel<T> model = (DefaultListModel<T>)from.getModel();
			for(Iterator<T> i=selectedValues.iterator();i.hasNext();) {
				model.removeElement(i.next());
			}
		}

		private <T> void removeAll(JList<T> from)
		{
			DefaultListModel<T> model = (DefaultListModel<T>) from.getModel();
			model.clear();
		}

		private <T> void up(JList<T> from)
		{
			java.util.List<T> selectedValues = from.getSelectedValuesList();
			DefaultListModel<T> model = (DefaultListModel<T>) from.getModel();
			int startInsertionAt = 0;
			if (from.getSelectedIndices()[0] >= 1) {
				startInsertionAt = from.getSelectedIndices()[0] - 1;
			}
			for(Iterator<T> i=selectedValues.iterator();i.hasNext();) {
				model.removeElement(i.next());
			}
			int index = 0;
			for(Iterator<T> i=selectedValues.iterator();i.hasNext();index++) {
				model.add(index+startInsertionAt, i.next());
			}
			from.setSelectionInterval(startInsertionAt,startInsertionAt+(selectedValues.size())-1);
		}

		private <T> void down(JList<T> from)
		{
			java.util.List<T> selectedValues = from.getSelectedValuesList();
			DefaultListModel<T> model = (DefaultListModel<T>) from.getModel();
			int startInsertionAt = model.getSize() - selectedValues.size();
			if(from.getSelectedIndices()[selectedValues.size() - 1] + 2 - selectedValues.size() < startInsertionAt)
					startInsertionAt = from.getSelectedIndices()[selectedValues.size() - 1] + 2 - selectedValues.size();
			for(Iterator<T> i=selectedValues.iterator();i.hasNext();) {
				model.removeElement(i.next());
			}
			int index = 0;
			for(Iterator<T> i=selectedValues.iterator();i.hasNext();index++) {
				model.add(index+startInsertionAt, i.next());
			}
			from.setSelectionInterval(startInsertionAt,startInsertionAt+(selectedValues.size())-1);
		}

		private void ok() {
			// extract report title
			reportDescription = reportParamGetter.reportDescriptionField.getText();
			savedReportDescription = reportDescription;

			// extract report base table name
			// insure not null, at least 2 chars long, and first char is non-blank
			baseReportTableName = reportParamGetter.reportTableNameField.getText();
			if (baseReportTableName == null || baseReportTableName.length()<2
				|| (baseReportTableName.contains(" ") && !baseReportTableName.endsWith(" "))) {
				JOptionPane.showMessageDialog(win,
					"Please enter valid base table name for summary report.  Table names must be a single word and may not contain spaces.",
					"Base Table Name Input Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}
			baseReportTableName = baseReportTableName.trim();
			savedBaseReportTableName = baseReportTableName;

			// extract run number selections, must be at least one
			//*****get all from runNumSelectionList and push to runs
			ListModel<String> in = reportParamGetter.runNumSelectionList.getModel();
			String[] runs = new String[in.getSize()];
			for(int i=0; i<in.getSize(); i++)
				runs[i] = in.getElementAt(i);
			if (runs == null || runs.length == 0) {
				JOptionPane.showMessageDialog(win,
					"Please select at least one MOVES run number",
					"No Run Number Selected Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}
			runNumSelections.clear();
			for (int i=0; i<runs.length; i++) {
				runNumSelections.add((String) runNumbers.get(runDisplayStrings.indexOf(runs[i])));
			}

			// extract category field selections, must be at least one
			//*****get all from categorySelectionList and push to catSelections
			ListModel<String> sn = reportParamGetter. categorySelectionList.getModel();
			String[] catSelections = new String[sn.getSize()];
			for(int i=0; i<sn.getSize(); i++)
				catSelections[i] = sn.getElementAt(i);
			if (catSelections == null || catSelections.length == 0) {
				JOptionPane.showMessageDialog(win,
					"Please select at least one report category field.",
					"No Category Selected Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}
			categoryFieldSelections.clear();
			for (int i=0; i<catSelections.length; i++) {
				categoryFieldSelections.add((String)catSelections[i]);
			}

			// extract data field selections, must be at least one
			// special handling for distance
			//*****get all from pollutantsSelectionList and push to pollutantsSelected
			sn = reportParamGetter.dataItemsSelectionList.getModel();
			String[] pollutantsSelected = new String[sn.getSize()];
			for(int i=0; i<sn.getSize(); i++)
				pollutantsSelected[i] = sn.getElementAt(i);
			if (pollutantsSelected == null || pollutantsSelected.length == 0) {
				JOptionPane.showMessageDialog(win,
					"Please select at least one report data field (besides distance)",
					"No Data Field Selected Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}
			pollutantSelections.clear();
			isDistanceIncluded = false;
			for (int i=0; i<pollutantsSelected.length; i++) {
				if(((String) pollutantsSelected[i]).equals("Distance")) {
					isDistanceIncluded = true;
				} else {
					pollutantSelections.add((String)pollutantsSelected[i]);
				}
			}
			if (pollutantSelections.size() == 0) {
				JOptionPane.showMessageDialog(win,
					"Please select at least one report data field besides distance",
					"Only Distance Selected Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}

			reportDialogResult = 1;
			dispose();
		}
		private void cancel() {
			reportDialogResult = 0;
			dispose();
		}
	}
	/** the dialog box used to display report on the screen **/
	ScreenReport screenReport;
	private class ScreenReport extends JDialog  {
		/** Visible components in the screen report JDialog **/
		JTabbedPane srTabbedPane;
		JScrollPane headerScrollPane;
		JList<String> headerReportList;
		JScrollPane bodyScrollPane;
		JList<String> bodyReportList;
		JScrollPane decodeScrollPane;
		JList<String> decodeReportList;
		JButton saveButton;
		JButton printButton;
		JButton closeButton;

		public ScreenReport(JFrame win) {
			super(win, MOVESWindow.MOVES_VERSION + " - Screen Report");
			createControls();
			arrangeControls();
		}

		private void createControls() {
			Font screenReportFont = new Font("monospaced", Font.PLAIN, 12);
			srTabbedPane = new JTabbedPane();

			headerReportList = new JList<String>(headerVector);
			headerReportList.setFont(screenReportFont);
			headerScrollPane = new JScrollPane(headerReportList);
			JLabel columnHeaderLineForHeader = new JLabel(headerColumnHeaderLine);
			columnHeaderLineForHeader.setFont(screenReportFont);
			headerScrollPane.setColumnHeaderView(columnHeaderLineForHeader);
			headerScrollPane.setHorizontalScrollBar(new JScrollBar());
			headerScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.
					HORIZONTAL_SCROLLBAR_AS_NEEDED);
			headerScrollPane.setVerticalScrollBar(new JScrollBar());
			headerScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.
					VERTICAL_SCROLLBAR_AS_NEEDED);
			srTabbedPane.addTab("Header", headerScrollPane);

			bodyReportList = new JList<String>(bodyVector);
			bodyReportList.setFont(screenReportFont);
			bodyScrollPane = new JScrollPane(bodyReportList);
			JLabel columnHeaderLineForBody = new JLabel(bodyColumnHeaderLine);
			columnHeaderLineForBody.setFont(screenReportFont);
			bodyScrollPane.setColumnHeaderView(columnHeaderLineForBody);

			bodyScrollPane.setHorizontalScrollBar(new JScrollBar());
			bodyScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.
					HORIZONTAL_SCROLLBAR_AS_NEEDED);
			bodyScrollPane.setVerticalScrollBar(new JScrollBar());
			bodyScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.
					VERTICAL_SCROLLBAR_AS_NEEDED);
			srTabbedPane.addTab("Body", bodyScrollPane);
			srTabbedPane.setSelectedComponent(bodyScrollPane);

			decodeReportList = new JList<String>(decodeVector);
			decodeReportList.setFont(screenReportFont);
			decodeScrollPane = new JScrollPane(decodeReportList);
			decodeColumnHeaderLine = " Category Field      Value  Description";
			JLabel columnHeaderLineForDecode = new JLabel(decodeColumnHeaderLine);
			columnHeaderLineForDecode.setFont(screenReportFont);
			decodeScrollPane.setColumnHeaderView(columnHeaderLineForDecode);
			decodeScrollPane.setHorizontalScrollBar(new JScrollBar());
			decodeScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.
					HORIZONTAL_SCROLLBAR_AS_NEEDED);
			decodeScrollPane.setVerticalScrollBar(new JScrollBar());
			decodeScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.
					VERTICAL_SCROLLBAR_AS_NEEDED);
			srTabbedPane.addTab("Decode", decodeScrollPane);

			saveButton = new JButton("Save");
			saveButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					save();
				}
			});
			saveButton.setName("saveButton");
			ToolTipHelper.add(saveButton, "Save the summary");

			printButton = new JButton("Print");
			printButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					print();
				}
			});
			printButton.setName("printButton");
			ToolTipHelper.add(printButton, "Print the summary");

			closeButton = new JButton("Close");
			closeButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					dispose();
				}
			});
			closeButton.setName("closeButton");
			ToolTipHelper.add(closeButton, "Close the dialog");
		}

		private void arrangeControls() {
			Container ctnr = getContentPane();
			ctnr.add(srTabbedPane,BorderLayout.CENTER);
			Box southSRBox = Box.createHorizontalBox();
			southSRBox.add(Box.createGlue());
			southSRBox.add(saveButton);
			southSRBox.add(printButton);
			southSRBox.add(closeButton);
			southSRBox.add(Box.createGlue());
			ctnr.add(southSRBox, BorderLayout.SOUTH);
		}

		private void save() {
			// get directory from user where to write the tabbed files
			File directoryToUse = new File(tabSeparatedReportDirectory);
			JFileChooser jfc = new JFileChooser();
			if (directoryToUse.exists() && directoryToUse.isDirectory()) {
				jfc.setCurrentDirectory(directoryToUse);
			}
			jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			jfc.setDialogTitle("Please specify a folder for the tabbed output report files.");
			int outDirStatus = jfc.showDialog(win,"Select Folder");
			if(outDirStatus != JFileChooser.APPROVE_OPTION) {
				return;
			}
			directoryToUse = jfc.getSelectedFile();
			tabSeparatedReportDirectory = directoryToUse.getPath();
			// System.out.println("Selected directory was: " + tabSeparatedReportDirectory);

			// delete any files that exist that have the names we are about to use
			File tabbedHeaderFile = new File(tabSeparatedReportDirectory,
					baseReportTableName + "Header.tab");
			if (tabbedHeaderFile.exists()) {
				tabbedHeaderFile.delete();
			}
			File tabbedBodyFile = new File(tabSeparatedReportDirectory,
					baseReportTableName + "Body.tab");
			if (tabbedBodyFile.exists()) {
				tabbedBodyFile.delete();
			}
			File tabbedDecodeFile = new File(tabSeparatedReportDirectory,
					baseReportTableName + "Decode.tab");
			if (tabbedDecodeFile.exists()) {
				tabbedDecodeFile.delete();
			}
			// create tabbed header report file
			FileWriter fw = null;
			BufferedWriter bw = null;
			try {
				fw = new FileWriter (tabbedHeaderFile);
				bw = new BufferedWriter (fw);
			} catch (IOException e) {
				JOptionPane.showMessageDialog(win,
					"I/O error occurred trying to create tabbed header report file.",
					"File Creation Error",
					JOptionPane.ERROR_MESSAGE);
				try {
					fw.close();
					bw.close();
				} catch (Exception ex) {
					// nothing can be done here
				}
				return;
			}
			// construct header portion of report as a Vector of Strings
			headerVector = new Vector<String>();
			PreparedStatement pstmt = null;
			String SQL = "SELECT MOVESRunID, HeaderItem, HeaderItemValue FROM " + headerTableName;
			ResultSet rs;
			try {
				pstmt = oConn.prepareStatement(SQL);
				rs = pstmt.executeQuery();
				while (rs.next()) {
					int runNo = rs.getInt(1);
					String stringRunNo = (Integer.valueOf(runNo)).toString();
					String headerItem = rs.getString(2);
					String headerItemValue = rs.getString(3);
					headerVector.add(stringRunNo + "\t" + headerItem + "\t" + headerItemValue);
				}
				rs.close();
				pstmt.close();
			} catch (SQLException e) {
				JOptionPane.showMessageDialog(win,
					"Database error occurred trying to produce tabbed output report header.",
					"Database Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}

			// write header report from Vector
			try {
				headerColumnHeaderLine = "Run\tHeader Item\tHeader Item Value";
				bw.write(headerColumnHeaderLine,0,headerColumnHeaderLine.length());
				bw.newLine();
				for (int i=0; i<headerVector.size(); i++) {
					String outLine = (String)headerVector.get(i);
					bw.write(outLine,0,outLine.length());
					bw.newLine();
				}
				bw.flush();
				fw.close();
				bw.close();
			} catch (IOException e) {
				JOptionPane.showMessageDialog(win,
					"I/O error occurred trying to write tabbed header report file.",
					"File Output Error",
					JOptionPane.ERROR_MESSAGE);
				try {
					fw.close();
					bw.close();
				} catch (Exception ex) {
					// nothing can be done here
				}
				return;
			}

			// create tabbed report body file
			FileWriter fw2 = null;
			BufferedWriter bw2 = null;
			try {
				fw2 = new FileWriter (tabbedBodyFile);
				bw2 = new BufferedWriter (fw2);
			} catch (IOException e) {
				JOptionPane.showMessageDialog(win,
					"I/O error occurred trying to create tabbed body report file.",
					"File Creation Error",
					JOptionPane.ERROR_MESSAGE);
				try {
					fw2.close();
					bw2.close();
				} catch (Exception ex) {
					// nothing can be done here
				}
				return;
			}

			// construct body portion of report as a Vector
			bodyVector = new Vector<String>();
			// we'll use JDBC metadata to figure out what columns are present rather than
			// try to correlate back to the code that created the report body table
			SQL = "SELECT * FROM " + bodyTableName;
			bodyColumnHeaderLine = "";
			try {
				pstmt = oConn.prepareStatement(SQL);
				rs = pstmt.executeQuery();
				ResultSetMetaData rsmd = rs.getMetaData();
				int colCount = rsmd.getColumnCount();
				boolean firstLine = true;
				while (rs.next()) {
					String reportLine = "";
					for (int col=1; col<=colCount; col++) {
						String colName = rsmd.getColumnLabel(col);
						int displayColWidth = findColDisplayWidth(colName);
						if (firstLine) {
							// assumes display width exceeds length of column name
							bodyColumnHeaderLine = bodyColumnHeaderLine + colName;
							if (col<colCount) {
								bodyColumnHeaderLine = bodyColumnHeaderLine + "\t";
							}
						}
						// all fields can be retrieved as Strings, including integers and floats
						String colValue = StringUtilities.safeGetString(rs.getString(col));
						reportLine = reportLine + colValue;
						if (col<colCount) {
							reportLine = reportLine + "\t";
						}
					}
					firstLine=false;
					bodyVector.add(reportLine);
				}
				rs.close();
				pstmt.close();
			} catch (SQLException e) {
				JOptionPane.showMessageDialog(win,
					"Database error occurred trying to produce tabbed report body.file",
					"Database Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}
			// write body report from Vector
			try {
				bw2.write(bodyColumnHeaderLine,0,bodyColumnHeaderLine.length());
				bw2.newLine();
				for (int i=0; i<bodyVector.size(); i++) {
					String outLine = (String)bodyVector.get(i);
					bw2.write(outLine,0,outLine.length());
					bw2.newLine();
				}
				bw2.flush();
				fw2.close();
				bw2.close();
			} catch (IOException e) {
				JOptionPane.showMessageDialog(win,
					"I/O error occurred trying to write tabbed report body file.",
					"File Output Error",
					JOptionPane.ERROR_MESSAGE);
				try {
					fw2.close();
					bw2.close();
				} catch (Exception ex) {
					// nothing can be done here
				}
				return;
			}

			// create tabbed report decode file
			FileWriter fw3 = null;
			BufferedWriter bw3 = null;
			try {
				fw3 = new FileWriter (tabbedDecodeFile);
				bw3 = new BufferedWriter (fw3);
			} catch (IOException e) {
				JOptionPane.showMessageDialog(win,
					"I/O error occurred trying to create tabbed body report file.",
					"File Creation Error",
					JOptionPane.ERROR_MESSAGE);
				try {
					fw3.close();
					bw3.close();
				} catch (Exception ex) {
					// nothing can be done here
				}
				return;
			}
			// construct decode portion of report as a Vector
			decodeVector = new Vector<String>();
			SQL = "SELECT categoryField, Value, Description FROM " + decodeTableName;
			try {
				pstmt = oConn.prepareStatement(SQL);
				rs = pstmt.executeQuery();
				while (rs.next()) {
					String catField = rs.getString(1);
					String catValue = String.valueOf(rs.getInt(2));
					String description = rs.getString(3);
					decodeVector.add(catField + "\t" + catValue + "\t" + description);
				}
				rs.close();
				pstmt.close();
			} catch (SQLException e) {
				JOptionPane.showMessageDialog(win,
					"Database error occurred trying to produce on-screen value decoding list.",
					"Database Error",
					JOptionPane.ERROR_MESSAGE);
				return;
			}

			// write decode report from Vector
			try {
				decodeColumnHeaderLine = "Category Field" + "\t" + "Value" + "\t" + "Description";
				bw3.write(decodeColumnHeaderLine,0,decodeColumnHeaderLine.length());
				bw3.newLine();
				for (int i=0; i<decodeVector.size(); i++) {
					String outLine = (String)decodeVector.get(i);
					bw3.write(outLine,0,outLine.length());
					bw3.newLine();
				}
				bw3.flush();
				fw3.close();
				bw3.close();
			} catch (IOException e) {
				JOptionPane.showMessageDialog(win,
					"I/O error occurred trying to write tabbed decode report file.",
					"File Output Error",
					JOptionPane.ERROR_MESSAGE);
				try {
					fw3.close();
					bw3.close();
				} catch (Exception ex) {
					// nothing can be done here
				}
				return;
			}
			JOptionPane.showMessageDialog(this, StringUtilities.wrapString(
						"Summary report output successfully saved to disk.", 60));
			return;
		}

		private void print()
		{
			StringBuffer textBuffer = new StringBuffer();
			textBuffer.append(headerColumnHeaderLine + "\r\n");
			for (int i=0; i<headerVector.size(); i++) {
				textBuffer.append(((String)headerVector.get(i)) + "\r\n");
			}
			textBuffer.append("\r\n\r\n");
			textBuffer.append(bodyColumnHeaderLine + "\r\n");
			for (int i=0; i<bodyVector.size(); i++) {
				textBuffer.append((String)bodyVector.get(i) + "\r\n");
			}
			textBuffer.append("\r\n\r\n");
			textBuffer.append(decodeColumnHeaderLine + "\r\n");
			for (int i=0; i<decodeVector.size(); i++) {
				textBuffer.append((String)decodeVector.get(i) + "\r\n");
			}
			// Print landscape using normal (not small) font
			TextPrinter.printText(win, textBuffer.toString(), true, false);
			return;
		}
	}

	/** the directory to which the tab-separated file reports are written **/
	static String tabSeparatedReportDirectory = "c:\\";

	/**
	 * Constructor
	 *
	 * @param win the JFrame to display GUI components in
	 * @param oConn a connection to the MOVESOutput database
	 * @param runSpec the run specification which may be assumed was
	 *           used to produce the MOVESOutput database
	**/
	public SummaryReporter(JFrame win, Connection oConn, RunSpec runSpec) {
		this.win = win;
		this.oConn = oConn;
		this.runSpec = runSpec;
        this.defaultDatabaseName = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;

		// create list of all possible report classification columns
		// order is significant
		reportClassificationColumns = new ArrayList<ReportClassificationColumn>();
		reportClassificationColumns.add(new ReportClassificationColumn
				("yearID", "Year", "SMALLINT(4)",6, null, null ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("monthID", "Month", "SMALLINT(5)",7, null, null ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("dayID", "Day", "SMALLINT(3)",5, null, null ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("hourID", "Hour", "SMALLINT(4)",6, null, null ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("stateID", "State", "SMALLINT(5)",7, "State", "stateName" ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("countyID", "County", "INTEGER(5)",7, "County", "countyName" ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("zoneID", "Zone", "INTEGER(6)",8, null, null ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("linkID", "Link", "INTEGER(7)",9, null, null ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("sourceTypeID", "Source", "SMALLINT(6)",8, "SourceUseType", "sourceTypeName"));
		reportClassificationColumns.add(new ReportClassificationColumn
				("SCC", "SCC", "CHAR(10)",12, null, null ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("fuelTypeID", "Fuel", "SMALLINT(4)",6, "FuelType", "fuelTypeDesc" ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("modelYearID", "ModelYr", "INTEGER(5)",8, null, null ));
		reportClassificationColumns.add(new ReportClassificationColumn
				("roadTypeID", "Road", "SMALLINT(4)",6, "RoadType", "roadDesc"));
		reportClassificationColumns.add(new ReportClassificationColumn
				("MOVESRunID", "Run", "SMALLINT(4)",6, null, null));

        polColumnNames = new TreeMap<Integer,String>();
        String SQL = "SELECT pollutantID, shortName FROM " + this.defaultDatabaseName + ".pollutant";
        try {
            PreparedStatement pstmt = oConn.prepareStatement(SQL);
            ResultSet rs = pstmt.executeQuery();
            while (rs.next()) {
                int pollutantID = rs.getInt(1);
                String columnName = rs.getString(2);
                polColumnNames.put(Integer.valueOf(pollutantID), columnName);
            }
        } catch (SQLException e) {
            e.printStackTrace();
			System.out.println(SQL);

			JOptionPane.showMessageDialog(win, "Database error occurred initializing the Summary Reporter.",
				"Database Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	/**
	 * principal method of this class, called to define and produce a summary report.
	 * @return true if report generation is successful, false otherwise
	 **/
	public boolean produceSummaryReport() {
		if (!getEmissionProcess()) {
			return false;
		}
		if (!getReportParameters()) {
			return false;
		}
		if (!writeHeaderTable()) {
			return false;
		}
		if (!writeBodyTable()) {
			return false;
		}
		if (!writeDecodeTable()) {
			return false;
		}
		if (!outputToScreen()) {
			return false;
		}
		return true;
	}

	private boolean getEmissionProcess() {
		// make TreeSet of emission process names included in runspec
		TreeSet<String> eProcesses = new TreeSet<String>();
		// check whether runspec called for emission processes to be distinguished
		if(runSpec.outputEmissionsBreakdownSelection.emissionProcess) {
			Iterator<PollutantProcessAssociation> i =
					runSpec.pollutantProcessAssociations.iterator();
			while (i.hasNext()) {
				PollutantProcessAssociation pp = (PollutantProcessAssociation) i.next();
				eProcesses.add(pp.emissionProcess.processName);
			}
			if (eProcesses.size() == 0 ) {
				JOptionPane.showMessageDialog(win,
					"Can't produce summary report: run spec has no emission processes",
					"No Emission Processes Error",
					JOptionPane.ERROR_MESSAGE);
				return false;
			}
		}
		eProcesses.add("All Processes");

		String eProcSelection = (String) JOptionPane.showInputDialog(win,
			"Select Emission Process to Report",
			"Emission Process Selection",
			JOptionPane.OK_CANCEL_OPTION, null,
			eProcesses.toArray(), null);
		if (eProcSelection == null) {
			return false;
		}
		// retrieve emission process number associated with selected name
		if (eProcSelection.equals("All Processes")) {
			emissionProcess = null;
		} else {
			emissionProcess = Integer.valueOf((EmissionProcess.findByName(eProcSelection)).databaseKey);
		}
		// System.out.println(eProcSelection);
		// System.out.println("translates to " + emissionProcess);

		return true;
	}

	private boolean getReportParameters() {
		// make ArrayLists of run numbers, each with associated display string,
		// to be included in the report
		runNumbers = new ArrayList<String>();
		runDisplayStrings = new ArrayList<String>();
		try {
			String SQL = "SELECT MOVESRunID, runDateTime, runSpecFileName " +
					"FROM MOVESRun ORDER BY MOVESRunID";
			Statement stmt = oConn.createStatement();
			ResultSet rs = stmt.executeQuery(SQL);
			while (rs.next()) {
				Integer runNum = Integer.valueOf(rs.getInt("MOVESRunID"));
				String runDisplay = "Run: " + runNum.toString() + " Time: " +
						rs.getString("runDateTime") + " " +
						rs.getString("runSpecFileName") + " ";
				// TreeMaps proved awkward because key had to be the display string
				// and it is unclear how they would sort. So used kludge of two
				// parrallel Arraylists, need to preserve order of SELECT
				runNumbers.add(runNum.toString());
				runDisplayStrings.add(runDisplay);
			}
		} catch (SQLException e) {
			JOptionPane.showMessageDialog(win,
				"Can't produce summary report: SQL error accessing MOVESRun table",
				"MOVESRun SQL Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}
		if(runNumbers.isEmpty()) {
			JOptionPane.showMessageDialog(win,
				"Can't produce summary report: No runs in output database",
				"Empty Output Database Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}
		// limit run number choices to last 12 runs, otherwise GUI layout problems occur
		while (runNumbers.size()>12) {
			runNumbers.remove(0);
			runDisplayStrings.remove(0);
		}

		Iterator<PollutantProcessAssociation> it = runSpec.pollutantProcessAssociations.iterator();
		while (it.hasNext()) {
			PollutantProcessAssociation pp = (PollutantProcessAssociation) it.next();
			if ((emissionProcess == null) ||
					(pp.emissionProcess.databaseKey == emissionProcess.intValue())) {
				pollutants.add(pp.pollutant.pollutantName);
			}
		}
		if (runSpec.outputVMTData) {
			pollutants.add("Distance");
		}
		if (pollutants.size() == 0) {
			JOptionPane.showMessageDialog(win,
				"Can't produce summary report: run spec would produce nothing to report",
				"No Pollutants Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}

		for (int i=0; i<reportClassificationColumns.size(); i++) {
			if (checkColumn(i)) {
				categories.add(((ReportClassificationColumn)
						reportClassificationColumns.get(i)).fieldName);
			}
		}
		reportParamGetter = new ReportParamGetter(win);
		reportParamGetter.pack();
		reportParamGetter.setResizable(true);
		reportParamGetter.setModal(true);
		(new WindowStateHandler(reportParamGetter,"SummaryReporter.reportParamGetter")).setSizePositionAndStartTracking(-1,-1);
		reportParamGetter.setVisible(true); //reportParamGetter.show();
		// returns when JDialog has been disposed
		if (reportDialogResult !=1) {
			return false;
		}
		return true;

	}
	/**
	 * helper function to check for compatibility with runspec,
	 * relies on GUI to insure that selections are compatible with
	 * each other
	 **/
	private boolean checkColumn(int i) {
		// relies upon ordering of reportClassification in a kludgy way
		TimeMeasurementSystem tms = runSpec.outputFactors.timeMeasurementSystem;
		GeographicOutputDetailLevel gold = runSpec.geographicOutputDetail;
		OutputEmissionsBreakdownSelection oebs = runSpec.outputEmissionsBreakdownSelection;
		switch(i) {
			case 0:
				//yearID is always valid reporting choice
				return true;
			case 1:
				// monthID valid choice unless results have been aggregated to year
				if (tms== null || tms == TimeMeasurementSystem.YEARS) {
					return false;
				} else {
					return true;
				}
			case 2:
				// dayID valid choice if time measurement system is hours or days
				if (tms!= null &&
				(tms == TimeMeasurementSystem.HOURS || tms == TimeMeasurementSystem.DAYS)) {
					return true;
				} else {
					return false;
				}
			case 3:
				// hourID valid choice if time measurement system is hours
				if (tms!= null && tms == TimeMeasurementSystem.HOURS) {
					return true;
				} else {
					return false;
				}
			case 4:
				// stateID valid choice unless data has been aggregated to nation
				if (gold == null || gold == GeographicOutputDetailLevel.NATION) {
					return false;
				} else {
					return true;
				}
			case 5:
				// countyID valid choice unless data has been aggregated to state or nation
				if (gold == null || gold == GeographicOutputDetailLevel.NATION
						|| gold == GeographicOutputDetailLevel.STATE) {
					return false;
				} else {
					return true;
				}
			case 6:
				// zoneID not valid choice if data has been aggregated to county, state, or nation
				// therefore not usually valid choice unless gui default was changed
				if (gold == null || gold == GeographicOutputDetailLevel.NATION
						|| gold == GeographicOutputDetailLevel.STATE
						|| gold == GeographicOutputDetailLevel.COUNTY) {
					return false;
				} else {
					return true;
				}
			case 7:
				// let's not consider linkID to be a valid choice
					return false;
			case 8:
				// sourceTypeID a valid choice if present in output
				// we're relying on GUI to have precluded SCC
				if (oebs != null && oebs.sourceUseType) {
					return true;
				} else {
					return false;
				}
			case 9:
				// SCC a valid choice if present in output
				// we're relying on GUI to have precluded source type
				if (oebs != null && oebs.onRoadSCC) {
					return true;
				} else {
					return false;
				}
			case 10:
				// fuelTypeID a valid choice if present in output
				if (oebs != null && oebs.fuelType) {
					return true;
				} else {
					return false;
				}
			case 11:
				// modelYearID a valid choice if present in output
				if (oebs != null && oebs.modelYear) {
					return true;
				} else {
					return false;
				}
			case 12:
				// roadTypeID a valid choice if present in output
				// we're relying on GUI to have precluded if output is by SCC
				if (oebs != null && oebs.roadType) {
					return true;
				} else {
					return false;
				}
			case 13:
				// always offer to include MOVESRunID
				return true;
		}
		return true;
	}

	private boolean writeHeaderTable() {

		String timeUnits = null;
		String distanceUnits = null;
		String massUnits = null;
		String energyUnits = null;
		String runSpecFileName = null;
		String runSpecDescription = null;
		String runSpecFileDateTime = null;
		String runDateTime = null;
        String runVersion = null;
        String SQL = "";

		try {
			PreparedStatement ps = oConn.prepareStatement("FLUSH TABLES");
			ps.executeUpdate();
			ps.close();
			headerTableName = baseReportTableName + "Header";
			SQL = "DROP TABLE IF EXISTS " + headerTableName;
			PreparedStatement pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			SQL = "CREATE TABLE " + headerTableName +
					" (MOVESRunID INT, HeaderItem TEXT, HeaderItemValue TEXT)";
			pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();

			SQL = "SELECT timeUnits, distanceUnits, massUnits, energyUnits, " +
					"runSpecFileName, runSpecDescription, runSpecFileDateTime, runDateTime, masterVersion  " +
					"FROM MOVESRun WHERE MOVESRunID = ?";
            PreparedStatement movesRunQuery = oConn.prepareStatement(SQL);

			String headerTableSql = "INSERT INTO " + headerTableName + " VALUES (?, ?, ?)";
			PreparedStatement headerTableInsert = oConn.prepareStatement(headerTableSql);

            String hasAllDaysSql = "SELECT DISTINCT COALESCE(dayID, 0) AS dayID FROM MOVESOutput WHERE MOVESRunID = ? ORDER BY dayID";
			PreparedStatement hasAllDays = oConn.prepareStatement(hasAllDaysSql);

            String hasAllMonthsSql = "SELECT DISTINCT COALESCE(monthID, 0) AS monthID FROM MOVESOutput WHERE MOVESRunID = ? ORDER BY monthID";
			PreparedStatement hasAllMonths = oConn.prepareStatement(hasAllMonthsSql);

			headerTableInsert.setInt(1,0);
			headerTableInsert.setString(2,"Report Description");
			headerTableInsert.setString(3,reportDescription);
			headerTableInsert.executeUpdate();
			headerTableInsert.clearParameters();
			headerTableInsert.setInt(1,0);
			headerTableInsert.setString(2,"Report Date/Time");
			headerTableInsert.setString(3,
					FileTimeUtility.convertFileTimeToString(System.currentTimeMillis()));
			headerTableInsert.executeUpdate();
			headerTableInsert.clearParameters();
			headerTableInsert.setInt(1,0);
			headerTableInsert.setString(2,"MOVES Output Database");
			headerTableInsert.setString(3,runSpec.outputDatabase.databaseName);
			headerTableInsert.executeUpdate();
			headerTableInsert.clearParameters();
			headerTableInsert.setInt(1,0);
			headerTableInsert.setString(2,"Emission Process");
			if (emissionProcess == null) {
				headerTableInsert.setString(3, "All");
			} else {
				headerTableInsert.setString(3,
					(EmissionProcess.findByID(emissionProcess.intValue())).processName);
			}
			headerTableInsert.executeUpdate();
			headerTableInsert.clearParameters();

            // see if dayID and/or monthID is included in the summary report columns. If it's not, we'll need to do
            // extra checks to make sure we have all the information we need to provide correct monthly or annual summaries
            boolean isDaySelected = false;
            boolean isMonthSelected = false;
            for (int i=1; i<categoryFieldSelections.size(); i++) {
				if (((String) categoryFieldSelections.get(i)).equalsIgnoreCase("dayID")) {
                    isDaySelected = true;
                }
				if (((String) categoryFieldSelections.get(i)).equalsIgnoreCase("monthID")) {
                    isMonthSelected = true;
                }
			}

            // provide the units and any warnings for each run
			for (int i=0; i<runNumSelections.size(); i++) {
				int selectedRunNum = 0;
				try {
					selectedRunNum = Integer.parseInt(runNumSelections.get(i));
				} catch(Exception e) {
					// Nothing to do
				}

                // if day is not selected in the columns to display, check to make sure we have output for both day types
                // or check that dayID is null (has already been aggregated over). If not, include a warning in the header
                if (!isDaySelected) {
                    hasAllDays.setInt(1,selectedRunNum);
                    ResultSet rs = hasAllDays.executeQuery();
                    String observedDayIDs = "";
                    while (rs.next()) {
                        observedDayIDs += rs.getString("dayID") + ",";
                    }
                    rs.close();
                    if (observedDayIDs.equals("2,") || observedDayIDs.equals("5,")) {
                        headerTableInsert.clearParameters();
                        headerTableInsert.setInt(1,selectedRunNum);
                        headerTableInsert.setString(2,"WARNING");
                        headerTableInsert.setString(3, "Run does not have both dayIDs selected.");
                        headerTableInsert.executeUpdate();
                        headerTableInsert.setString(3, "Therefore, the aggregated inventory may be incomplete.");
                        headerTableInsert.executeUpdate();
                    }
                }
                // if month is not selected in the columns to display, check to make sure we have output for all months
                // or check that monthID is null (has already been aggregated over). If not, include a warning in the header
                if (!isMonthSelected) {
                    hasAllMonths.setInt(1,selectedRunNum);
                    ResultSet rs = hasAllMonths.executeQuery();
                    String observedMonthIDs = "";
                    while (rs.next()) {
                        observedMonthIDs += rs.getString("monthID") + ",";
                    }
                    rs.close();
                    if (!(observedMonthIDs.equals("1,2,3,4,5,6,7,8,9,10,11,12") || observedMonthIDs.equals("0,"))) {
                        headerTableInsert.clearParameters();
                        headerTableInsert.setInt(1,selectedRunNum);
                        headerTableInsert.setString(2,"WARNING");
                        headerTableInsert.setString(3, "Run does not have all months selected.");
                        headerTableInsert.executeUpdate();
                        headerTableInsert.setString(3, "Therefore, the aggregated inventory may be incomplete.");
                        headerTableInsert.executeUpdate();
                    }
                }

				movesRunQuery.clearParameters();
				movesRunQuery.setInt(1,selectedRunNum);
				ResultSet rs = movesRunQuery.executeQuery();
				if (rs.next()) {
					timeUnits = rs.getString("timeUnits");
					distanceUnits = rs.getString("distanceUnits");
					massUnits = rs.getString("massUnits");
					energyUnits = rs.getString("energyUnits");
					runSpecFileName = rs.getString("runSpecFileName");
					runSpecDescription = rs.getString("runSpecDescription");
					runSpecFileDateTime = rs.getString("runSpecFileDateTime");
					runDateTime = rs.getString("runDateTime");
					runVersion = rs.getString("masterVersion");
				}
				rs.close();

				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"MOVES Version");
				headerTableInsert.setString(3,runVersion);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"Run Date/Time");
				headerTableInsert.setString(3,runDateTime);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"Run Specification");
				headerTableInsert.setString(3,runSpecFileName);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"Run Spec File Date/Time");
				headerTableInsert.setString(3,runSpecFileDateTime);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"Run Spec Description");
				headerTableInsert.setString(3,runSpecDescription);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"Mass Units");
				headerTableInsert.setString(3, massUnits);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"Energy Units");
				headerTableInsert.setString(3, energyUnits);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"Distance Units");
				headerTableInsert.setString(3, distanceUnits);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
				headerTableInsert.setInt(1,selectedRunNum);
				headerTableInsert.setString(2,"Time Units");
				headerTableInsert.setString(3, timeUnits);
				headerTableInsert.executeUpdate();
				headerTableInsert.clearParameters();
			}  //end for
            movesRunQuery.close();
			headerTableInsert.close();
			hasAllDays.close();

			return true;

		} catch (SQLException e) {
            e.printStackTrace();
			System.out.println(SQL);

			JOptionPane.showMessageDialog(win,
				"Database error occurred trying to produce report header table.",
				"Database Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}
	}

	private boolean writeBodyTable() {
		String SQL = "";

		try {
			PreparedStatement ps = oConn.prepareStatement("FLUSH TABLES");
			ps.executeUpdate();
			ps.close();

			SQL = "DROP TABLE IF EXISTS TempMOVESOutput" ;
			PreparedStatement pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
            // System.out.println("writeBodyTable called");

			// construct text string listing MOVESRunID values
			// suitable for inclusion in MySQL statements
			// we can assume there is at least one MOVESRunID value
			String runIDList = runNumSelections.get(0).toString();
			for (int i=1; i<runNumSelections.size(); i++) {
				runIDList = runIDList + ", " + runNumSelections.get(i).toString();
			}
			// construct text string listing category fields
			// suitable for inclusion in MySQL statements
			// we can assume there is at least one category field
            boolean isDaySelected = false;
			String catFieldList = "`" + (String) categoryFieldSelections.get(0) + "`";
			for (int i=1; i<categoryFieldSelections.size(); i++) {
				catFieldList = catFieldList + ", `" + ((String) categoryFieldSelections.get(i)) + "`";
                if (((String) categoryFieldSelections.get(i)).toLowerCase().equals("dayid")) {
                    isDaySelected = true;
                }
			}
            //System.out.println(catFieldList);
            // if dayID is not selected, we are aggregating over it. If dayID is NULL in the output database,
            // that means it has already been taken care of and we don't need to do anything. Otherwise:
            //    If outputTimePeriod is (Hour, Day), also need to multiply emissionQuant by dayID to get to "Part of Week" level
            //    Regardless of aggregation level, need to multiply the by # of weeks in the month to go from weekly to monthly
            String aggDayClause = "";
            String aggDayJoin = "";
            if (!isDaySelected) {
                aggDayClause = " * CASE WHEN dayID IS NOT NULL THEN COALESCE(noOfDays/7, 1) ELSE 1 END " +
                               " * CASE WHEN timeUnits IN ('hour', 'day') THEN dayID ELSE 1 END ";
                aggDayJoin = " JOIN movesrun USING (MOVESRunID) " +
                             " LEFT JOIN " + this.defaultDatabaseName + ".monthofanyyear USING (monthID) ";
            }
			if (emissionProcess == null) {
				SQL = "CREATE TABLE TempMOVESOutput SELECT " + catFieldList +
						", `pollutantID`, SUM(emissionQuant" + aggDayClause + ") AS `emissionQuant` " +
						"FROM MOVESOutput " + aggDayJoin + " WHERE MOVESRunID IN(" + runIDList + ") GROUP BY " +
						catFieldList + ", pollutantID";
				pstmt = oConn.prepareStatement(SQL);
			} else {
				SQL = "CREATE TABLE TempMOVESOutput SELECT " + catFieldList +
						", `pollutantID`, SUM(emissionQuant" + aggDayClause + ") AS `emissionQuant` " +
						"FROM MOVESOutput " + aggDayJoin + " WHERE MOVESRunID IN(" + runIDList +
						") AND `processID` = ? GROUP BY " + catFieldList + ", `pollutantID`"; 
				pstmt = oConn.prepareStatement(SQL);
				pstmt.setInt(1,emissionProcess.intValue());
			}
            // System.out.println(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			//
			// this section is rather complex
			// loop through all pollutants selected,
			// constructing and executing ALTER...UPDATE statement pair for each one
			// while also constructing CREATE and INSERT statement phrases for them
			String sqlForCreatePols = " ";
			String sqlForInsertPols = " ";
			for (int i=0; i<pollutantSelections.size(); i++) {
				String polName = (String) pollutantSelections.get(i);
				int polID = Pollutant.findByName(polName).databaseKey;
				String colName = (String) polColumnNames.get(Integer.valueOf(polID));
				//System.out.println("polID=" + polID + ", polName=" + polName + ", colName = " + colName);
				SQL = "ALTER TABLE TempMOVESOutput ADD COLUMN `" + colName + "` FLOAT DEFAULT 0.0";
				pstmt = oConn.prepareStatement(SQL);
				// System.out.println(SQL);
				pstmt.executeUpdate();
				pstmt.close();
				SQL = "UPDATE TempMOVESOutput SET `" + colName + "` = emissionQuant WHERE pollutantID = ?";
				pstmt = oConn.prepareStatement(SQL);
				pstmt.setInt(1,polID);
				pstmt.executeUpdate();
				pstmt.close();
				sqlForCreatePols = sqlForCreatePols + "`" + colName + "` FLOAT(18,0)";
				sqlForInsertPols = sqlForInsertPols + " SUM(`" + colName + "`) AS `" + colName + "`";
				if (i < pollutantSelections.size()-1) {
					sqlForCreatePols = sqlForCreatePols + ",";
					sqlForInsertPols = sqlForInsertPols + ",";
				}
			}
            // System.out.println(sqlForCreatePols);
            // System.out.println(sqlForInsertPols);

			// Construct category field lists suitable for inclusion in
			// CREATE TABLE statement and UPDATE TABLE .. ON statement
			String catCreateFieldList = " ";
			String catUpdateFieldList = " ";
			for (int i=0; i<categoryFieldSelections.size(); i++) {
				String fName = (String) categoryFieldSelections.get(i);
				int j = 0;
				for (j=0; j<reportClassificationColumns.size(); j++) {
					if(fName.equals(((ReportClassificationColumn)
							reportClassificationColumns.get(j)).fieldName)) {
						break;
					}
				}
				ReportClassificationColumn rcc =
						(ReportClassificationColumn) reportClassificationColumns.get(j);
				catCreateFieldList = catCreateFieldList
						+ rcc.displayAs + " " + rcc.columnType + ",";
				catUpdateFieldList = catUpdateFieldList
						+ " sr." + rcc.displayAs + "= tmao." + fName ;
				if (i < categoryFieldSelections.size()-1) {
					catUpdateFieldList = catUpdateFieldList + " AND ";
				}
			}
            // System.out.println(catCreateFieldList);
            // System.out.println(catUpdateFieldList);

			bodyTableName = baseReportTableName + "Body";
			SQL = "DROP TABLE IF EXISTS " + bodyTableName;
			pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			SQL = "CREATE TABLE " + bodyTableName + " (" + catCreateFieldList +
					sqlForCreatePols + ")";
            // System.out.println(SQL);
			pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			SQL = "INSERT INTO " + bodyTableName + " SELECT " +
					catFieldList + ", " + sqlForInsertPols +
					" FROM TempMOVESOutput GROUP BY " + catFieldList;
            // System.out.println(SQL);
			pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			if (isDistanceIncluded) {
				SQL = "DROP TABLE IF EXISTS TempMOVESActivityOutput ";
				pstmt = oConn.prepareStatement(SQL);
				pstmt.executeUpdate();
				pstmt.close();
				SQL = "CREATE TABLE TempMOVESActivityOutput SELECT " +
						catFieldList + ", SUM(activity" + aggDayClause + ") AS Distance " + 
                        "FROM MOVESActivityOutput " + aggDayJoin +
						"WHERE MOVESRunID IN(" + runIDList + ") " +
						"AND activityTypeID=1 " +
						"GROUP BY " + catFieldList;
                // System.out.println(SQL);
				pstmt = oConn.prepareStatement(SQL);
				pstmt.executeUpdate();
				pstmt.close();
				SQL = "CREATE INDEX index1 ON TempMOVESActivityOutput (" +
						catFieldList + ")";
				pstmt = oConn.prepareStatement(SQL);
				pstmt.executeUpdate();
				pstmt.close();
				SQL = "ALTER TABLE " + bodyTableName + " ADD COLUMN Distance FLOAT(13,0) NULL";
                // System.out.println(SQL);
				pstmt = oConn.prepareStatement(SQL);
				pstmt.executeUpdate();
				pstmt.close();
				SQL = "UPDATE " + bodyTableName + " AS sr " +
						"LEFT JOIN TempMOVESActivityOutput AS tmao " +
						"ON(" + catUpdateFieldList + ") SET sr.Distance = tmao.Distance";
                // System.out.println(SQL);
				pstmt = oConn.prepareStatement(SQL);
				pstmt.executeUpdate();
				pstmt.close();
				SQL = "DROP TABLE IF EXISTS TempMOVESActivityOutput";
				pstmt = oConn.prepareStatement(SQL);
				pstmt.executeUpdate();
				pstmt.close();
			}
			SQL = "DROP TABLE IF EXISTS TempMOVESOutput";
			pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			SQL = "FLUSH TABLES";
			pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			return true;

		} catch (SQLException e) {
			e.printStackTrace();
			System.out.println(SQL);

			JOptionPane.showMessageDialog(win,
				"Database error occurred trying to produce report body table.",
				"Database Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}
	}
		private boolean writeDecodeTable() {
		try {
			String defaultDBName = SystemConfiguration.getTheSystemConfiguration()
					.databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName;
			// System.out.println("Default database name is " + defaultDBName);
			decodeTableName = baseReportTableName + "Decode";
			String SQL = "DROP TABLE IF EXISTS " + decodeTableName;
			PreparedStatement pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			SQL = "CREATE TABLE " + decodeTableName +
					"(CategoryField CHAR(14), Value INTEGER(10), Description CHAR(50))";
			pstmt = oConn.prepareStatement(SQL);
			pstmt.executeUpdate();
			pstmt.close();
			for (int i=0; i<categoryFieldSelections.size(); i++) {
				String fName = (String) categoryFieldSelections.get(i);
				int j = 0;
				for (j=0; j<reportClassificationColumns.size(); j++) {
					if(fName.equals(((ReportClassificationColumn)
							reportClassificationColumns.get(j)).fieldName)) {
						break;
					}
				}
				ReportClassificationColumn rcc =
						(ReportClassificationColumn) reportClassificationColumns.get(j);
				if (rcc.decodeTable != null && rcc.decodeField != null) {
					// System.out.println (rcc.fieldName + " Needs Decoding");
					SQL = "SELECT DISTINCT " + rcc.displayAs + " FROM " + bodyTableName +
							" ORDER BY " + rcc.displayAs;
					// System.out.println(SQL);
					pstmt = oConn.prepareStatement(SQL);
					ResultSet rs = pstmt.executeQuery();
					while (rs.next()) {
						int value = rs.getInt(1);
						String SQL2 = "INSERT INTO " + decodeTableName +
						" SELECT ?,?," + rcc.decodeField +
						" FROM " + defaultDBName + "." + rcc.decodeTable +
						" WHERE " + rcc.fieldName + "= ?";
						// System.out.println(SQL2);
						PreparedStatement pstmt2 = oConn.prepareStatement(SQL2);
						pstmt2.setString(1,rcc.fieldName);
						pstmt2.setInt(2,value);
						pstmt2.setInt(3,value);
						pstmt2.executeUpdate();
						pstmt2.close();
					}
					rs.close();
					pstmt.close();
				}
			}
			return true;
		} catch (SQLException e) {
			JOptionPane.showMessageDialog(win,
				"Database error occurred trying to produce report value decoding table.",
				"Database Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}
	}
	private boolean outputToScreen() {
		// construct header portion of report as a Vector of Strings
		headerVector = new Vector<String>();
		headerColumnHeaderLine = " Run              Header Item:  Item Value";
		PreparedStatement pstmt = null;
		String SQL = "SELECT MOVESRunID, HeaderItem, HeaderItemValue FROM " + headerTableName;
		ResultSet rs;
		String spaces = "                                                  ";
		try {
			pstmt = oConn.prepareStatement(SQL);
			rs = pstmt.executeQuery();
			while (rs.next()) {
				int runNo = rs.getInt(1);
				String paddedRunNo = null;
				if (runNo==0) {
					paddedRunNo = spaces.substring(0,5);
				} else {
					String stringRunNo = (Integer.valueOf(runNo)).toString();
					paddedRunNo = spaces.substring(0,4-stringRunNo.length()) + stringRunNo + " ";
				}
				String headerItem = rs.getString(2);
				// pad first column to constant width of 25
				String paddedHeaderItem = spaces.substring(0,24-headerItem.length())
						 + headerItem.trim();
				String headerItemValue = rs.getString(3);
				// trim second column to width of 70 if necessary
				if (headerItemValue != null && headerItemValue.length() > 70) {
					headerItemValue = headerItemValue.substring(0,69);
				}
				headerVector.add(paddedRunNo + paddedHeaderItem + ":  " +headerItemValue);
			}
			rs.close();
			pstmt.close();
		} catch (SQLException e) {
			JOptionPane.showMessageDialog(win,
				"Database error occurred trying to produce screen report header.",
				"Database Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}
		// construct body portion of report as a Vector of Strings
		bodyVector = new Vector<String>();
		// we'll use JDBC metadata to figure out what columns are present rather than
		// try to correlate back to the code that created the report body table
		SQL = "SELECT * FROM " + bodyTableName;
		bodyColumnHeaderLine = "";
		try {
			pstmt = oConn.prepareStatement(SQL);
			rs = pstmt.executeQuery();
			ResultSetMetaData rsmd = rs.getMetaData();
			int colCount = rsmd.getColumnCount();
			boolean firstLine = true;
			while (rs.next()) {
				String reportLine = "";
				for (int col=1; col<=colCount; col++) {
					String colName = rsmd.getColumnLabel(col);
					int displayColWidth = findColDisplayWidth(colName);
					if (firstLine) {
						// assumes display width exceeds length of column name
						bodyColumnHeaderLine = bodyColumnHeaderLine +
							spaces.substring(0,displayColWidth-colName.length()) + colName;
					}
					// all fields can be retrieved as Strings, including integers and floats
					String colValue = StringUtilities.safeGetString(rs.getString(col));
					if (colValue.length() < displayColWidth ) {
						reportLine = reportLine
								+ spaces.substring(0,displayColWidth-colValue.length());
					}
					reportLine = reportLine + colValue;
				}
				firstLine=false;
				bodyVector.add(reportLine);
			}
			rs.close();
			pstmt.close();
		} catch (SQLException e) {
			JOptionPane.showMessageDialog(win,
				"Database error occurred trying to produce screen report body.",
				"Database Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}
		// construct decode portion of report as a Vector of Strings
		decodeVector = new Vector<String>();
		SQL = "SELECT categoryField, Value, Description FROM " + decodeTableName;
		try {
			pstmt = oConn.prepareStatement(SQL);
			rs = pstmt.executeQuery();
			while (rs.next()) {
				String catField = rs.getString(1);
				// pad first column to constant width of 15
				String paddedCatField = spaces.substring(0,15-catField.length()) + catField.trim();
				String catValue = String.valueOf(rs.getInt(2));
				// pad second column to constant width of 11
				String paddedCatValue = spaces.substring(0,11-catValue.length()) + catValue.trim();
				String description = rs.getString(3);
				decodeVector.add(paddedCatField + paddedCatValue + "  " + description);
			}
			rs.close();
			pstmt.close();
		} catch (SQLException e) {
			JOptionPane.showMessageDialog(win,
				"Database error occurred trying to produce on-screen value decoding list.",
				"Database Error",
				JOptionPane.ERROR_MESSAGE);
			return false;
		}

		// create controls
		screenReport = new ScreenReport(win);
		screenReport.pack();
		screenReport.setResizable(true);
		screenReport.setModal(true);
		(new WindowStateHandler(screenReport,"SummaryReporter.screenReport")).setSizePositionAndStartTracking(-1,-1);
		screenReport.setVisible(true); //screenReport.show();
		// return will occur when a "close report" button is selected
		return true;
	}

	private int findColDisplayWidth(String colName) {
		if (colName.equalsIgnoreCase("Distance") || colName.equalsIgnoreCase("Activity")) {
			return 15;
		}
		for (int i=0; i<reportClassificationColumns.size(); i++) {
			ReportClassificationColumn rcc = (ReportClassificationColumn)
					reportClassificationColumns.get(i);
			if (colName.equalsIgnoreCase(rcc.displayAs)) {
				return rcc.displayWidth;
			}
		}
		// if not a classification column or activity, assume to be a pollutant column
		return Math.max(20, colName.length()+1);
	}

	/** inner class representing a classification column in the summary report **/
	class ReportClassificationColumn {
		/** the name of the field eg sourceTypeID, countyID, etc. **/
		String fieldName;
		/** the name to use as a column header in the output report **/
		String displayAs;
		/** the MySQL column datatype, eg SMALLINT(6) **/
		String columnType;
		/** the report width required by the column, including column separators **/
		int displayWidth;
		/** name of the MOVESDefault table which contains a decoding string.
		 *  this table is assumed to have a field named "fieldName"
		 *  and be located in the default database identified
		 *  in the System Configuration **/
		String decodeTable;
		/** name of a String field in the decode Table which contains the
		 *  decoded form of the numeric category **/
		String decodeField;

		/** constructor **/
		ReportClassificationColumn (String fieldName, String displayAs, String columnType,
				int displayWidth, String decodeTable, String decodeField) {
			this.fieldName = fieldName;
			this.displayAs = displayAs;
			this.columnType = columnType;
			this.displayWidth = displayWidth;
			this.decodeTable = decodeTable;
			this.decodeField = decodeField;
		}
	}
}
