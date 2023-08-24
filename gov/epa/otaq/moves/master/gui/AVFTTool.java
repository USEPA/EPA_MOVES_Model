/**************************************************************************************************
 * @(#)AVFTTool.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import java.sql.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import java.io.*;
import java.util.*;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.importers.ImporterManager;
import gov.epa.otaq.moves.master.framework.importers.TableFileLinkagePart;
import gov.epa.otaq.moves.master.implementation.importers.FuelImporter;

/**
 * Runs the AVFT Tool
 * @version 	2023-02-28
**/
public class AVFTTool extends JDialog implements ActionListener {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;

	/** Label displaying the name of the input file to user **/
	JLabel inputFileText;
	/** Button to browse for the input file **/
	JButton browseInputFileButton;
	/** Button to create the input template file **/
	JButton createInputTemplateButton;
    /** the literal label for the Known Fractions **/
    JLabel knownFractionsLabel;
	/** Label displaying the name of the known fractions input file to user **/
	JLabel knownFractionsFileText;
	/** Button to browse for the known fractions input file **/
	JButton browseKnownFractionsFileButton;
	/** Button to create the known fractions input template file **/
	JButton createKnownFractionsTemplateButton;
    /** Label displaying the name of the output file to user **/
	JLabel outputFileText;
	/** Full path for the file shown in outputFileText **/
	String outputFileFullPath = "";
    /** Button to browse for the output file **/
	JButton browseOutputFileButton;
    /** Tool input combo boxes **/
	JComboBox<String> baseYearCombo;
	JComboBox<String> analysisYearCombo;
	JComboBox<String> gapFillingMethod21Combo;
	JComboBox<String> projectionMethod21Combo;
	JComboBox<String> gapFillingMethod31Combo;
	JComboBox<String> projectionMethod31Combo;
	JComboBox<String> projectionMethod32Combo;
	JComboBox<String> gapFillingMethod32Combo;
	JComboBox<String> projectionMethod41Combo;
	JComboBox<String> gapFillingMethod41Combo;
	JComboBox<String> projectionMethod42Combo;
	JComboBox<String> gapFillingMethod42Combo;
	JComboBox<String> projectionMethod43Combo;
	JComboBox<String> gapFillingMethod43Combo;
	JComboBox<String> projectionMethod51Combo;
	JComboBox<String> gapFillingMethod51Combo;
	JComboBox<String> projectionMethod52Combo;
	JComboBox<String> gapFillingMethod52Combo;
	JComboBox<String> projectionMethod53Combo;
	JComboBox<String> gapFillingMethod53Combo;
	JComboBox<String> projectionMethod54Combo;
	JComboBox<String> gapFillingMethod54Combo;
	JComboBox<String> projectionMethod61Combo;
	JComboBox<String> gapFillingMethod61Combo;
	JComboBox<String> projectionMethod62Combo;
	JComboBox<String> gapFillingMethod62Combo;
	/** List of messages **/
	JList<String> messagesList;
	/** Button to run the tool **/
	JButton runAVFTToolButton;
	/** Button to save messages **/
	JButton saveMessagesButton;
	/** Button to close the window **/
	JButton doneButton;
	/** Button to open help **/
	JButton openHelpButton;

	/** DefaultListModel for messageList **/
	DefaultListModel<String> messageListModel;

	/** List of messages to be shown **/
	ArrayList<String> messages = new ArrayList<String>();
	/** True if messages contains any errors that should prevent further processing **/
	boolean messagesHasError = false;

    /** path to the stored procedure file **/
    String storedProcedureFullPath;

    /** Importer manager used to read the input AVFT file **/
    ImporterManager manager; // manager used to run the importer
    FuelImporter fuelImporter; // the specific importer that knows how to import the AVFT file

    /** variables to hold the input AVFT file name/tab and known fractions input file/tab **/
    String inputAVFTFilePath;
    String inputAVFTFileType;
    String inputAVFTFileWorkbook;
    String knownFractionsFilePath;
    String knownFractionsFileType;
    String knowFractionsFileWorkbook;

	/**
	 * Browser dialog for selecting an output directory.  Retained as a member
	 * variable so it would retain the last used directory.
	**/
	JFileChooser folderChooser = null;

    /** define control constants */
    static final String GAP_FILLING_0S = "Fill with 0s";
    static final String GAP_FILLING_DEFAULTS = "Use defaults and renormalize";
    static final String[] GAP_FILLING_METHODS = {GAP_FILLING_0S, GAP_FILLING_DEFAULTS};
    static final String PROJECTION_CONSTANT = "Constant";
    static final String PROJECTION_NATIONAL = "National Average";
    static final String PROJECTION_PROPORTIONAL = "Proportional";
    static final String PROJECTION_KNOWN = "Known Fractions";
    static final String[] PROJECTION_METHODS = {PROJECTION_PROPORTIONAL, PROJECTION_NATIONAL, PROJECTION_KNOWN, PROJECTION_CONSTANT};

	/**
	 * Constructs the FERC panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	 * @param modeToUse Default conversion mode
	**/
	public AVFTTool(JFrame parent) {
		super(parent, "AVFT Tool");
		frame = parent;

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(false);
        setSize(620, 710);
        findAVFTToolScript();
        instantiateManager();
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		resetMessages();
		populateMessagesList();

		pack();
		(new WindowStateHandler(this)).setSizePositionAndStartTracking(620,710);
		setModal(true);
		setVisible(true);
	}

	/**
	 * Creates and arranges all dialog controls.
	 * @return the container as JPanel.
	**/
	JPanel createPanel() {
		return createAndArrangeControls();
	}

	/**
	 * Create controls and sets their layout.
	 * @return the container as JPanel of the controls
	**/
	public JPanel createAndArrangeControls() {
        // ------------ create all controls ------------
		JPanel panel = new JPanel();

        // input/output files
		inputFileText = new JLabel("Browse for the input AVFT file...", JLabel.RIGHT);
		knownFractionsFileText = new JLabel("Browse for the known fractions input file...", JLabel.RIGHT);
		outputFileText = new JLabel("Specify the output file name and location...", JLabel.RIGHT);

        // tool inputs
        baseYearCombo = new JComboBox<String>(new String[] {"", "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025","2026","2027","2028","2029","2030","2031","2032","2033","2034","2035","2036","2037","2038","2039","2040","2041","2042","2043","2044","2045","2046","2047","2048","2049","2050","2051","2052","2053","2054","2055","2056","2057","2058","2059"});
        baseYearCombo.setSelectedItem("");
		ToolTipHelper.add(baseYearCombo, "What is the last complete model year in your input data?");
        baseYearCombo.setEditable(false);
        baseYearCombo.addActionListener(this);

        analysisYearCombo = new JComboBox<String>(new String[] {"", "2012"}); // include one year so that control is sized correctly
        analysisYearCombo.setSelectedItem("");
		ToolTipHelper.add(analysisYearCombo, "Select the last complete model year first.");
        analysisYearCombo.setEnabled(false);
        analysisYearCombo.setEditable(false);
        analysisYearCombo.addActionListener(this);

        gapFillingMethod21Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod21Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod21Combo,  "What gap filling method should be used for passenger cars?");
        gapFillingMethod21Combo.setEditable(false);
        gapFillingMethod21Combo.addActionListener(this);
        
        gapFillingMethod31Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod31Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod31Combo,  "What gap filling method should be used for passenger trucks?");
        gapFillingMethod31Combo.setEditable(false);
        gapFillingMethod31Combo.addActionListener(this);
        
        gapFillingMethod32Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod32Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod32Combo,  "What gap filling method should be used for light commercial trucks?");
        gapFillingMethod32Combo.setEditable(false);
        gapFillingMethod32Combo.addActionListener(this);
        
        gapFillingMethod41Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod41Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod41Combo,  "What gap filling method should be used for other buses?");
        gapFillingMethod41Combo.setEditable(false);
        gapFillingMethod41Combo.addActionListener(this);
        
        gapFillingMethod42Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod42Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod42Combo,  "What gap filling method should be used for transit buses?");
        gapFillingMethod42Combo.setEditable(false);
        gapFillingMethod42Combo.addActionListener(this);
        
        gapFillingMethod43Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod43Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod43Combo,  "What gap filling method should be used for school buses?");
        gapFillingMethod43Combo.setEditable(false);
        gapFillingMethod43Combo.addActionListener(this);
        
        gapFillingMethod51Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod51Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod51Combo,  "What gap filling method should be used for refuse trucks?");
        gapFillingMethod51Combo.setEditable(false);
        gapFillingMethod51Combo.addActionListener(this);
        
        gapFillingMethod52Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod52Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod52Combo,  "What gap filling method should be used for single unit short-haul trucks?");
        gapFillingMethod52Combo.setEditable(false);
        gapFillingMethod52Combo.addActionListener(this);
        
        gapFillingMethod53Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod53Combo.setSelectedItem(GAP_FILLING_DEFAULTS);
		ToolTipHelper.add(gapFillingMethod53Combo,  "What gap filling method should be used for single unit long-haul trucks?");
        gapFillingMethod53Combo.setEditable(false);
        gapFillingMethod53Combo.addActionListener(this);
        
        gapFillingMethod54Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod54Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod54Combo,  "What gap filling method should be used for motor homes?");
        gapFillingMethod54Combo.setEditable(false);
        gapFillingMethod54Combo.addActionListener(this);
        
        gapFillingMethod61Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod61Combo.setSelectedItem(GAP_FILLING_0S);
		ToolTipHelper.add(gapFillingMethod61Combo,  "What gap filling method should be used for combination short-haul trucks?");
        gapFillingMethod61Combo.setEditable(false);
        gapFillingMethod61Combo.addActionListener(this);
        
        gapFillingMethod62Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod62Combo.setSelectedItem(GAP_FILLING_DEFAULTS);
		ToolTipHelper.add(gapFillingMethod62Combo,  "What gap filling method should be used for combination long-haul trucks?");
        gapFillingMethod62Combo.setEditable(false);
        gapFillingMethod62Combo.addActionListener(this);

        projectionMethod21Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod21Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod21Combo,  "What projection method should be used for passenger cars?");
        projectionMethod21Combo.setEditable(false);
        projectionMethod21Combo.addActionListener(this);

        projectionMethod31Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod31Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod31Combo,  "What projection method should be used for passenger trucks?");
        projectionMethod31Combo.setEditable(false);
        projectionMethod31Combo.addActionListener(this);

        projectionMethod32Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod32Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod32Combo,  "What projection method should be used for light commercial trucks?");
        projectionMethod32Combo.setEditable(false);
        projectionMethod32Combo.addActionListener(this);

        projectionMethod41Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod41Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod41Combo,  "What projection method should be used for other buses?");
        projectionMethod41Combo.setEditable(false);
        projectionMethod41Combo.addActionListener(this);

        projectionMethod42Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod42Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod42Combo,  "What projection method should be used for transit buses?");
        projectionMethod42Combo.setEditable(false);
        projectionMethod42Combo.addActionListener(this);

        projectionMethod43Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod43Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod43Combo,  "What projection method should be used for school buses?");
        projectionMethod43Combo.setEditable(false);
        projectionMethod43Combo.addActionListener(this);

        projectionMethod51Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod51Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod51Combo,  "What projection method should be used for refuse trucks?");
        projectionMethod51Combo.setEditable(false);
        projectionMethod51Combo.addActionListener(this);

        projectionMethod52Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod52Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod52Combo,  "What projection method should be used for single unit short-haul trucks?");
        projectionMethod52Combo.setEditable(false);
        projectionMethod52Combo.addActionListener(this);

        projectionMethod53Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod53Combo.setSelectedItem(PROJECTION_NATIONAL);
		ToolTipHelper.add(projectionMethod53Combo,  "What projection method should be used for single unit long-haul trucks?");
        projectionMethod53Combo.setEditable(false);
        projectionMethod53Combo.addActionListener(this);

        projectionMethod54Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod54Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod54Combo,  "What projection method should be used for motor homes?");
        projectionMethod54Combo.setEditable(false);
        projectionMethod54Combo.addActionListener(this);

        projectionMethod61Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod61Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod61Combo,  "What projection method should be used for combination short-haul trucks?");
        projectionMethod61Combo.setEditable(false);
        projectionMethod61Combo.addActionListener(this);

        projectionMethod62Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod62Combo.setSelectedItem(PROJECTION_NATIONAL);
		ToolTipHelper.add(projectionMethod62Combo,  "What projection method should be used for combination long-haul trucks?");
        projectionMethod62Combo.setEditable(false);
        projectionMethod62Combo.addActionListener(this);

        // buttons
        browseInputFileButton = new JButton(); 
		browseInputFileButton.addActionListener(this);
        createInputTemplateButton = new JButton(); 
		createInputTemplateButton.addActionListener(this);
        browseKnownFractionsFileButton = new JButton(); 
		browseKnownFractionsFileButton.addActionListener(this);
        createKnownFractionsTemplateButton = new JButton(); 
		createKnownFractionsTemplateButton.addActionListener(this);
        browseOutputFileButton = new JButton(); 
		browseOutputFileButton.addActionListener(this);
		runAVFTToolButton = new JButton();
		runAVFTToolButton.addActionListener(this);
		saveMessagesButton = new JButton();
		saveMessagesButton.addActionListener(this);
		doneButton = new JButton();
		doneButton.addActionListener(this);
		openHelpButton = new JButton();
		openHelpButton.addActionListener(this);
		
        // messages
        messageListModel = new DefaultListModel<String>();
        messagesList = new JListWithToolTips<String>(messageListModel);
        messagesList.setSelectedIndex(-1);
        messagesList.setVisibleRowCount(6);
        messagesList.setPrototypeCellValue("XXXXXXXXXXXXXXXXXXX"); 

		// ------------ arrange controls ------------
        panel.setLayout(new GridBagLayout());
        ((GridBagLayout)panel.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
        ((GridBagLayout)panel.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        ((GridBagLayout)panel.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 1.0E-4, 0.0, 0.0 };
        ((GridBagLayout)panel.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

        // inputs inset
        {
            JPanel inputsInset = new JPanel();
            inputsInset.setBorder(BorderFactory.createTitledBorder("Tool Input Selections"));
            inputsInset.setLayout(new GridBagLayout());
            ((GridBagLayout)inputsInset.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
            ((GridBagLayout)inputsInset.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            ((GridBagLayout)inputsInset.getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0E-4, 0.0, 0.0, 0.0 };
            ((GridBagLayout)inputsInset.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};
            int r = 0; // track current row
            int cGapFilling = 1;
            int cProjection = 4;

            openHelpButton.setText("Open Help");
            openHelpButton.setMnemonic('H');
            ToolTipHelper.add(openHelpButton,"Open the help document (.pdf)");
            inputsInset.add(openHelpButton, new GridBagConstraints(cProjection, r, 2, 2, 0.0, 0.0,
                GridBagConstraints.EAST, GridBagConstraints.BOTH, new Insets(0, 0, 1, 1), 0, 0));

            JLabel baseYearLabel = new JLabel("Last complete model year in input data:");
            baseYearLabel.setDisplayedMnemonic('y');
            baseYearLabel.setLabelFor(baseYearCombo);
            inputsInset.add(baseYearLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(baseYearCombo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 21, 1, 5), 0, 0));
                
            r++;
            JLabel analysisYearLabel = new JLabel("Analysis year:");
            analysisYearLabel.setDisplayedMnemonic('s');
            analysisYearLabel.setLabelFor(analysisYearCombo);
            inputsInset.add(analysisYearLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(analysisYearCombo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 21, 1, 5), 0, 0));
    
            r++;
            inputsInset.add(new JLabel("Gap-filling Method:"), new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0, 
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 1, 5), 0, 0));
            inputsInset.add(new JLabel("Projection Method:"), new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0, 
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 1, 5), 0, 0));

            r++;
            JLabel pcLabel = new JLabel("Passenger Cars (21):");
            pcLabel.setDisplayedMnemonic('P');
            pcLabel.setLabelFor(gapFillingMethod21Combo);
            inputsInset.add(pcLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod21Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod21Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            r++;
            JLabel ptLabel = new JLabel("Passenger Trucks (31):");
            ptLabel.setDisplayedMnemonic('a');
            ptLabel.setLabelFor(gapFillingMethod31Combo);
            inputsInset.add(ptLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod31Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod31Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        
            r++;
            JLabel ldcLabel = new JLabel("LD Commercial Trucks (32):");
            ldcLabel.setDisplayedMnemonic('L');
            ldcLabel.setLabelFor(gapFillingMethod32Combo);
            inputsInset.add(ldcLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod32Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod32Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            r++;
            JLabel otherLabel = new JLabel("Other Buses (41):");
            otherLabel.setDisplayedMnemonic('B');
            otherLabel.setLabelFor(gapFillingMethod41Combo);
            inputsInset.add(otherLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod41Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod41Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));


            r++;
            JLabel transitLabel = new JLabel("Transit Buses (42):");
            transitLabel.setDisplayedMnemonic('T');
            transitLabel.setLabelFor(gapFillingMethod42Combo);
            inputsInset.add(transitLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod42Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod42Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        
            r++;
            JLabel schoolLabel = new JLabel("School Buses (43):");
            schoolLabel.setDisplayedMnemonic('u');
            schoolLabel.setLabelFor(gapFillingMethod43Combo);
            inputsInset.add(schoolLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod43Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod43Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        

            r++;
            JLabel refuseLabel = new JLabel("Refuse Trucks (51):");
            refuseLabel.setDisplayedMnemonic('f');
            refuseLabel.setLabelFor(gapFillingMethod51Combo);
            inputsInset.add(refuseLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod51Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod51Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));


            r++;
            JLabel sushLabel = new JLabel("Single Unit Short-haul Trucks (52):");
            sushLabel.setDisplayedMnemonic('n');
            sushLabel.setLabelFor(gapFillingMethod52Combo);
            inputsInset.add(sushLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod52Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod52Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        
            r++;
            JLabel sulhLabel = new JLabel("Single Unit Long-haul Trucks (53):");
            sulhLabel.setDisplayedMnemonic('e');
            sulhLabel.setLabelFor(gapFillingMethod53Combo);
            inputsInset.add(sulhLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod53Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod53Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        

            r++;
            JLabel rvLabel = new JLabel("Motor Homes (54):");
            rvLabel.setDisplayedMnemonic('M');
            rvLabel.setLabelFor(gapFillingMethod54Combo);
            inputsInset.add(rvLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod54Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod54Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));


            r++;
            JLabel cshLabel = new JLabel("Combination Short-haul Trucks (61):");
            cshLabel.setDisplayedMnemonic('C');
            cshLabel.setLabelFor(gapFillingMethod61Combo);
            inputsInset.add(cshLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod61Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod61Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        
            r++;
            JLabel clhLabel = new JLabel("Combination Long-haul Trucks (62):");
            clhLabel.setDisplayedMnemonic('g');
            clhLabel.setLabelFor(gapFillingMethod62Combo);
            inputsInset.add(clhLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod62Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod62Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));                       

            panel.add(inputsInset, new GridBagConstraints(0, 0, 5, 3, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 6, 0), 0, 0));
        }

        // files inset
        {
            int r = 0;
            int c = 0;
            JPanel filesInset = new JPanel();
            filesInset.setBorder(BorderFactory.createTitledBorder("Input/Output Files"));
            filesInset.setLayout(new GridBagLayout());
            ((GridBagLayout)filesInset.getLayout()).columnWidths = new int[] {120, 246, 75, 75};
            ((GridBagLayout)filesInset.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            ((GridBagLayout)filesInset.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 0.0};
            ((GridBagLayout)filesInset.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

            JLabel inputAVFTLabel = new JLabel("Input AVFT File:");
            inputAVFTLabel.setDisplayedMnemonic('I');
            inputAVFTLabel.setLabelFor(browseInputFileButton);
            filesInset.add(inputAVFTLabel, new GridBagConstraints(c++, r,1,1,0.0,0.0,
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            ToolTipHelper.add(inputFileText,"Name and location of the input file (.xlsx/.xls/.csv/.txt)");
            filesInset.add(inputFileText, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            browseInputFileButton.setText("Browse...");
            ToolTipHelper.add(browseInputFileButton,"Select the input file (.xlsx/.xls/.csv/.txt)");
            filesInset.add(browseInputFileButton, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE,
                new Insets(0, 0, 1, 0), 0, 0));

            createInputTemplateButton.setText("Create Template...");
            ToolTipHelper.add(createInputTemplateButton,"Create a template AVFT file (.xlsx/.xls)");
            filesInset.add(createInputTemplateButton, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE,
                new Insets(0, 0, 1, 0), 0, 0));

            c = 0;
            r += 1;
            knownFractionsLabel = new JLabel("Known Fractions:");
            knownFractionsLabel.setDisplayedMnemonic('K');
            knownFractionsLabel.setLabelFor(browseKnownFractionsFileButton);
            filesInset.add(knownFractionsLabel, new GridBagConstraints(c++, r,1,1,0.0,0.0,
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            ToolTipHelper.add(knownFractionsFileText,"Name and location of the known fractions file (.xlsx/.xls/.csv/.txt)");
            filesInset.add(knownFractionsFileText, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            browseKnownFractionsFileButton.setText("Browse...");
            filesInset.add(browseKnownFractionsFileButton, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE,
                new Insets(0, 0, 1, 0), 0, 0));

            createKnownFractionsTemplateButton.setText("Create Template...");
            filesInset.add(createKnownFractionsTemplateButton, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE,
                new Insets(0, 0, 1, 0), 0, 0));
            setKnownFractionsControlsEnabled(false);

            c = 0;
            r += 1;
            JLabel outputAVFTLabel = new JLabel("Output AVFT File:");
            outputAVFTLabel.setDisplayedMnemonic('O');
            outputAVFTLabel.setLabelFor(browseOutputFileButton);
            filesInset.add(outputAVFTLabel, new GridBagConstraints(c++, r,1,1,0.0,0.0,
                GridBagConstraints.EAST, GridBagConstraints.NONE,
                new Insets(0, 0, 1, 5), 0, 0));

            ToolTipHelper.add(outputFileText,"Name and location of the output file (.xlsx)");
            filesInset.add(outputFileText, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.EAST, GridBagConstraints.NONE,
                new Insets(0, 0, 1, 5), 0, 0));

            browseOutputFileButton.setText("Browse...");
            ToolTipHelper.add(browseOutputFileButton,"Select the output  file (.xlsx)");
            filesInset.add(browseOutputFileButton, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE,
                new Insets(0, 0, 1, 0), 0, 0));

            panel.add(filesInset, new GridBagConstraints(0, 3, 5, 2, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE,
                new Insets(0, 0, 3, 0), 0, 0));
        }

        // messages inset
        {
            JPanel messagesInset = new JPanel();
            messagesInset.setBorder(BorderFactory.createTitledBorder("Messages"));
            messagesInset.setLayout(new GridBagLayout());
            ((GridBagLayout)messagesInset.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
            ((GridBagLayout)messagesInset.getLayout()).rowHeights = new int[] {88, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
            ((GridBagLayout)messagesInset.getLayout()).columnWeights = new double[] {0.0, 0.0, 1.0E-4, 0.0, 0.0, 0.0 };
            ((GridBagLayout)messagesInset.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0E-4, 0.0};

            JScrollPane messagesScrollPane = new JScrollPane();
            ToolTipHelper.add(messagesScrollPane,"Displays messages, warnings, and errors");
            messagesScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
            messagesScrollPane.setViewportView(messagesList);

            messagesInset.add(messagesScrollPane, new GridBagConstraints(0, 0, 5, 2, 0.0, 0.0,
            GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH,
            new Insets(0, 0, 0, 5), 0, 0));

            panel.add(messagesInset, new GridBagConstraints(0, 5, 5, 2, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 0, 6, 0), 0, 0));
        }

        // buttons
        {
            runAVFTToolButton.setText("Run AVFT Tool");
            ToolTipHelper.add(runAVFTToolButton,"Run the tool using the provided inputs and save results to the output file.");
            panel.add(runAVFTToolButton, new GridBagConstraints(0, 8, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 5, 3, 5), 0, 0));
            runAVFTToolButton.setMnemonic('R');
            runAVFTToolButton.setDisplayedMnemonicIndex(0);
            
            saveMessagesButton.setText("Save Messages");
            ToolTipHelper.add(saveMessagesButton,"Save above messages as a text file.");
            panel.add(saveMessagesButton, new GridBagConstraints(2, 8, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 0, 3, 5), 0, 0));
                saveMessagesButton.setMnemonic('S');
                saveMessagesButton.setDisplayedMnemonicIndex(0);

            doneButton.setText("Done");
            ToolTipHelper.add(doneButton,"Close this window");
            panel.add(doneButton, new GridBagConstraints(4, 8, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 0, 3, 5), 0, 0));
            doneButton.setMnemonic('D');
            doneButton.setDisplayedMnemonicIndex(0);
        }

		return panel;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == browseInputFileButton) {
			handleBrowseInputFileButton();
		} else if(e.getSource() == createInputTemplateButton) {
			handleTemplateButton();
		}  else if(e.getSource() == browseKnownFractionsFileButton) {
			handleBrowseKnownFractionsFileButton();
		} else if(e.getSource() == createKnownFractionsTemplateButton) {
			handleTemplateButton();
		} else if(e.getSource() == browseOutputFileButton) {
			handleBrowseOutputFileButton();
		} else if(e.getSource() == baseYearCombo) {
            handleBaseYearComboChange();
        } else if(e.getSource() == runAVFTToolButton) {
			handleRunAVFTToolButton();
		} else if(e.getSource() == saveMessagesButton) {
			handleSaveMessagesButton();
		} else if(e.getSource() == openHelpButton) {
			handleOpenHelpButton();
		} else if(e.getSource() == doneButton) {
			handleDoneButton();
		} else if(e.getSource() == projectionMethod21Combo) {
            handleProjectComboChange();
        }  else if(e.getSource() == projectionMethod31Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod32Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod41Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod42Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod43Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod51Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod52Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod53Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod54Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod61Combo) {
            handleProjectComboChange();
        } else if(e.getSource() == projectionMethod62Combo) {
            handleProjectComboChange();
        }
	}

	/** Make sure the AVFT Tool script is available **/
	void findAVFTToolScript() {
		try {
			File file = new File("database/AVFTTool/AVFTTool.sql");
			if(file == null || !file.exists()) {
                messages.add("Error: unable to find the AVFT stored procedures.");
                messagesHasError = true;
                return;
			}
			storedProcedureFullPath = file.getCanonicalPath();
		} catch(Exception e) {
			// Nothing to do here
		}
	}

    /** instantiate the importer manager and get the fuel importer **/
    void instantiateManager() {
        // create the manager and set AVFT mode
        manager = new ImporterManager(null);
        manager.setAsAVFTTool();

        // instantiate only the Fuel tab
        String[] importerCommonNames = {"fuel"};
        manager.instantiate(importerCommonNames);
        fuelImporter = (FuelImporter) manager.importers.get(0);

        // specifically set the parent for the fuel importer to get the icon on the popup windows
        // associated with this importer
        fuelImporter.avftPart.setParent(frame);
    }

	/** Handle the Browse button for the input file **/
	void handleBrowseInputFileButton() {
        // Let the Fuels tab in the importer manager do all the heavy lifting
        //     This will store the selected file in fuelImporter.avftPart.fileName
        //     and the selected worksheet (if applicable) in fuelImporter.avftPart.worksheetName
        //     Save these in local variables because the Known Fractions control also uses the same importer
        //     However, if the user cancels the dialog, the importer manager does nothing. To detect this case,
        //     reset the variables that hold the user selection before calling, and then check to make sure
        //     they actually contain new data. Otherwise, return without doing anything.
        fuelImporter.avftPart.fileName = "";
        fuelImporter.avftPart.fileType = "";
        fuelImporter.avftPart.worksheetName = "";
        fuelImporter.avftPart.handleBrowseButton();
        if (fuelImporter.avftPart.fileName == "") {
            return;
        }
        inputAVFTFilePath = fuelImporter.avftPart.fileName;
        inputAVFTFileType = fuelImporter.avftPart.fileType;
        inputAVFTFileWorkbook = fuelImporter.avftPart.worksheetName;

        // update GUI
        String displayText = inputAVFTFilePath;
        String toolTipText = inputAVFTFilePath;
        if (displayText.length() > 27) { // max displayed file length is 30
            displayText = "..." + displayText.substring(displayText.length()-27);
        }
        if (inputAVFTFileWorkbook != null && inputAVFTFileWorkbook != "") {
            if (displayText.length() + inputAVFTFileWorkbook.length() + 3 > 41) { // if displayed file length + " [workbook name]" is too long, truncate
                displayText += " [" + inputAVFTFileWorkbook.substring(0, 5) + "..." + "]";
            } else {
                displayText += " [" + inputAVFTFileWorkbook + "]";
            }
            toolTipText += " [" + inputAVFTFileWorkbook + "]";
        }
        inputFileText.setText(displayText);
        // store the full path as a tooltip if the displayed text is truncated, otherwise use original tooltip
        if (displayText.contains("...")) {
            ToolTipHelper.add(inputFileText,toolTipText);
        } else {
            ToolTipHelper.add(inputFileText,"Name and location of the input file (.xlsx/.xls/.csv/.txt)");
        }
	}

    /** Handle the Template button **/
	void handleTemplateButton() {
        // Let the Fuels tab in the importer manager do all the heavy lifting
        fuelImporter.avftPart.handleCreateTemplateButton();
	}

    /** Handle the Browse button for the input file **/
	void handleBrowseKnownFractionsFileButton() {
        // Let the Fuels tab in the importer manager do all the heavy lifting
        //     This will store the selected file in fuelImporter.avftPart.fileName
        //     and the selected worksheet (if applicable) in fuelImporter.avftPart.worksheetName
        //     Save these in local variables because the AVFT input file control also uses the same importer
        //     However, if the user cancels the dialog, the importer manager does nothing. To detect this case,
        //     reset the variables that hold the user selection before calling, and then check to make sure
        //     they actually contain new data. Otherwise, return without doing anything.
        fuelImporter.avftPart.fileName = "";
        fuelImporter.avftPart.fileType = "";
        fuelImporter.avftPart.worksheetName = "";
        fuelImporter.avftPart.handleBrowseButton();
        if (fuelImporter.avftPart.fileName == "") {
            return;
        }
        knownFractionsFilePath = fuelImporter.avftPart.fileName;
        knownFractionsFileType = fuelImporter.avftPart.fileType;
        knowFractionsFileWorkbook = fuelImporter.avftPart.worksheetName;

        // update GUI
        String displayText = knownFractionsFilePath;
        String toolTipText = knownFractionsFilePath;
        if (displayText.length() > 27) { // max displayed file length is 30
            displayText = "..." + displayText.substring(displayText.length()-27);
        }
        if (knowFractionsFileWorkbook != null && knowFractionsFileWorkbook != "") {
            if (displayText.length() + knowFractionsFileWorkbook.length() + 3 > 41) { // if displayed file length + " [workbook name]" is too long, truncate
                displayText += " [" + knowFractionsFileWorkbook.substring(0, 5) + "..." + "]";
            } else {
                displayText += " [" + knowFractionsFileWorkbook + "]";
            }
            toolTipText += " [" + knowFractionsFileWorkbook + "]";
        }
        knownFractionsFileText.setText(displayText);
        // store the full path as a tooltip if the displayed text is truncated, otherwise use original tooltip
        if (displayText.contains("...")) {
            ToolTipHelper.add(knownFractionsFileText,toolTipText);
        } else {
            ToolTipHelper.add(knownFractionsFileText,"Name and location of the known fractions input file (.xlsx/.xls/.csv/.txt)");
        }
	}

	/** Handle the Browse button for the output file **/
	void handleBrowseOutputFileButton() {
		try {
            // show dialog
			FileDialog fd = new FileDialog(frame, "Select Output File (*.xlsx)", FileDialog.SAVE);
			fd.setVisible(true);
			if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
				return;
			}

            // a file was selected. Clear the controls before populating them in case something goes wrong
			outputFileText.setText("");
			outputFileFullPath = "";

            // get the selected path
			String filePath = fd.getDirectory() + fd.getFile();
            
            // ensure file ends with xlsx
            if (!filePath.toLowerCase().endsWith("xlsx")) {
                JOptionPane.showMessageDialog(frame, "This tool can only output *.xlsx files. Automatically changing extension.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                filePath = filePath + ".xlsx";
            }

            // populate controls
			File file = new File(filePath);
			outputFileText.setText(file.getName());
			outputFileFullPath = file.getCanonicalPath();
		} catch(Exception e) {
			// Nothing to do here
		}
	}

    /** The analysis year combo contents depend on the base year **/
    void handleBaseYearComboChange() {
        String selectedBaseYear = baseYearCombo.getSelectedItem().toString();
        if (selectedBaseYear != null && selectedBaseYear != "") {
            String selectedAnalysisYear = analysisYearCombo.getSelectedItem().toString();
            try {
                analysisYearCombo.removeAllItems();
                analysisYearCombo.addItem("");
                for (int year = Integer.valueOf(selectedBaseYear) + 1; year <= 2060; year++) {
                    analysisYearCombo.addItem(String.valueOf(year));
                }
            } catch (NumberFormatException e) {
                // nothing to do here
            }
            analysisYearCombo.setEnabled(true);
            analysisYearCombo.setSelectedItem(selectedAnalysisYear);
            ToolTipHelper.add(analysisYearCombo, "What year should the AVFT Tool project to?");
        } else {
            analysisYearCombo.setEnabled(false);
            ToolTipHelper.add(analysisYearCombo, "Select the last complete model year first.");
        }
    }

	/** Checks to see if any projection combo box is set to PROJECTION_KNOWN and enables the corresponding user controls **/
    void handleProjectComboChange() {
        if (projectionMethod21Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod31Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod32Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod41Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod42Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod43Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod51Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod52Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod53Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod54Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod61Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod62Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            setKnownFractionsControlsEnabled(true);
        } else {
            setKnownFractionsControlsEnabled(false);
        }
    }

    /** Enable or disable the Known Fractions user controls **/
    void setKnownFractionsControlsEnabled(boolean enabled) {
        knownFractionsLabel.setEnabled(enabled);
        knownFractionsFileText.setEnabled(enabled);
        browseKnownFractionsFileButton.setEnabled(enabled);
        createKnownFractionsTemplateButton.setEnabled(enabled);

        if (enabled) {
            ToolTipHelper.add(knownFractionsLabel, "");
            ToolTipHelper.add(knownFractionsFileText,"Name and location of the known fractions input file (.xlsx/.xls/.csv/.txt)");
            ToolTipHelper.add(browseKnownFractionsFileButton,"Select the known fractions input file (.xlsx/.xls/.csv/.txt)");
            ToolTipHelper.add(createKnownFractionsTemplateButton,"Create a template AVFT file (.xlsx/.xls)");
        } else {
            ToolTipHelper.add(knownFractionsLabel, "This control is only used if the " + PROJECTION_KNOWN + " projection method is selected.");
            ToolTipHelper.add(knownFractionsFileText, "This control is only used if the " + PROJECTION_KNOWN + " projection method is selected.");
            ToolTipHelper.add(browseKnownFractionsFileButton, "This control is only used if the " + PROJECTION_KNOWN + " projection method is selected.");
            ToolTipHelper.add(createKnownFractionsTemplateButton, "This control is only used if the " + PROJECTION_KNOWN + " projection method is selected.");
        }
    } 

	/** Handle the Run AVFT Tool button **/
	void handleRunAVFTToolButton() {
        boolean loggerState = Logger.skipHandlers;
		try {
            Logger.skipHandlers = true; // don't popup logged errors (already handling popups)
			resetMessages();

            // load base year, analysis year, and output file name inputs
            String baseYear = baseYearCombo.getSelectedItem().toString();
            if (baseYear == null || baseYear == "") {
                messages.add("Error: Specify the base year.");
                JOptionPane.showMessageDialog(frame, "Missing input error. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return;
            }
            String analysisYear = analysisYearCombo.getSelectedItem().toString();
            if (analysisYear == null || analysisYear == "") {
                messages.add("Error: Specify the analysis year.");
                JOptionPane.showMessageDialog(frame, "Missing input error. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return;
            }
            if (inputAVFTFilePath == null || inputAVFTFilePath == "") {
                messages.add("Error: Specify the input file.");
                JOptionPane.showMessageDialog(frame, "Missing input error. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return;
            }
            if (knownFractionsLabel.isEnabled() && (knownFractionsFilePath == null || knownFractionsFilePath == "")) {
                messages.add("Error: Specify the known fractions input file.");
                JOptionPane.showMessageDialog(frame, "Missing input error. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return;
            }
            if (outputFileFullPath == null || outputFileFullPath == "") {
                messages.add("Error: Specify the output file.");
                JOptionPane.showMessageDialog(frame, "Missing input error. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return;
            }

            // create database connection to drop AVFTTool working database if it already exists so we always have a fresh environment
            DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
            Connection db = defaultDatabase.openConnection();
            try {
                SQLRunner.executeSQL(db, "DROP DATABASE IF EXISTS avfttool");
            } finally {
                DatabaseUtilities.closeConnection(db);
            }

            // create this tool's stored procedures and open a connection to the database that it returns
            File scriptFile = new File(storedProcedureFullPath);
			Connection avftDB = null;
			try {
                // store the returned database in the manager, so loading the input files "just works"
				manager.database = DatabaseUtilities.buildAVFTProcedure(scriptFile,defaultDatabase,messages);
                avftDB = manager.database.openConnectionOrNull(); // and open a connection that we can use
			} catch(Exception e) {
				messages.add(e.getMessage());
                JOptionPane.showMessageDialog(frame, "Error creating the AVFT Tool's stored procedures. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
				return;
			}
            
            // load input AVFT file into the avftDatabase and rename imported table to inputAVFT
            messagesHasError = doImport("inputAVFT", inputAVFTFilePath, inputAVFTFileType, inputAVFTFileWorkbook, avftDB, false);
			if(messagesHasError) {
                JOptionPane.showMessageDialog(frame, "Error importing the input AVFT table. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return;
			}

            // load Known Fractions AVFT file into the avftDatabase and rename table to knownAVFT
            // do not do regular error checking, because all the avft errors will be triggered by this input
            messagesHasError = doImport("knownAVFT", knownFractionsFilePath, knownFractionsFileType, knowFractionsFileWorkbook, avftDB, true);
            if(messagesHasError) {
                JOptionPane.showMessageDialog(frame, "Error importing the known fractions table. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return;
			}

            // run stored procedures that are always run
            String sql = "call AVFTTool_CreateDefaultAVFT();";
            DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
            sql = "call AVFTTool_NormalizeInputs();";
            DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);

            // run gap-filling stored procedures
            if (runGapFillingProcedure(21, gapFillingMethod21Combo, baseYear) &&
                runGapFillingProcedure(31, gapFillingMethod31Combo, baseYear) &&
                runGapFillingProcedure(32, gapFillingMethod32Combo, baseYear) &&
                runGapFillingProcedure(41, gapFillingMethod41Combo, baseYear) &&
                runGapFillingProcedure(42, gapFillingMethod42Combo, baseYear) &&
                runGapFillingProcedure(43, gapFillingMethod43Combo, baseYear) &&
                runGapFillingProcedure(51, gapFillingMethod51Combo, baseYear) &&
                runGapFillingProcedure(52, gapFillingMethod52Combo, baseYear) &&
                runGapFillingProcedure(53, gapFillingMethod53Combo, baseYear) &&
                runGapFillingProcedure(54, gapFillingMethod54Combo, baseYear) &&
                runGapFillingProcedure(61, gapFillingMethod61Combo, baseYear) &&
                runGapFillingProcedure(62, gapFillingMethod62Combo, baseYear)) {
                // nothing to do here
            } else {
                // there was an error and the dialog has already been shown via the Run...Procedure(), so just return
                return;
            }
            
            // run projection stored procedures
            if (runProjectionProcedure(21, projectionMethod21Combo, baseYear, analysisYear) &&
                runProjectionProcedure(31, projectionMethod31Combo, baseYear, analysisYear) &&
                runProjectionProcedure(32, projectionMethod32Combo, baseYear, analysisYear) &&
                runProjectionProcedure(41, projectionMethod41Combo, baseYear, analysisYear) &&
                runProjectionProcedure(42, projectionMethod42Combo, baseYear, analysisYear) &&
                runProjectionProcedure(43, projectionMethod43Combo, baseYear, analysisYear) &&
                runProjectionProcedure(51, projectionMethod51Combo, baseYear, analysisYear) &&
                runProjectionProcedure(52, projectionMethod52Combo, baseYear, analysisYear) &&
                runProjectionProcedure(53, projectionMethod53Combo, baseYear, analysisYear) &&
                runProjectionProcedure(54, projectionMethod54Combo, baseYear, analysisYear) &&
                runProjectionProcedure(61, projectionMethod61Combo, baseYear, analysisYear) &&
                runProjectionProcedure(62, projectionMethod62Combo, baseYear, analysisYear)) {
                // nothing to do here
            } else {
                // there was an error and the dialog has already been shown via the Run...Procedure(), so just return
                return;
            }
            
            // handle motorcycles and order results
            sql = "call AVFTTool_HandleMotorcycles();";
            DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
            sql = "call AVFTTool_OrderResults();";
            DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
            if(doesMessagesContainError()) {
                JOptionPane.showMessageDialog(frame, "Error cleaning up after running the tool; output was not saved.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return;
			}

            // No errors. Write the output data
            writeOutput();
			if(doesMessagesContainError()) {
                JOptionPane.showMessageDialog(frame, "Error saving the output of the AVFT Tool. Check the messages list for details.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
            } else {
				JOptionPane.showMessageDialog(frame, "AVFT Tool finished.", "AVFT Tool", JOptionPane.INFORMATION_MESSAGE);
			}

            // Drop working database to clean up
            db = defaultDatabase.openConnection();
            try {
                SQLRunner.executeSQL(db, "DROP DATABASE IF EXISTS avfttool");
            } finally {
                DatabaseUtilities.closeConnection(db);
            }

		} catch(Exception e) {
			Logger.logError(e,"Unable to run the AVFT Tool");
		} finally {
			populateMessagesList();
            Logger.skipHandlers = loggerState;
		}
	}

    /** Runs the gap-filling stored procedure for the given source type 
     * @param sourceTypeID: The source type that the given gap-filling method should be used on
     * @param projectionComboBox: The GUI element containing the user-selected gap-filling method for the given source type
     * @param baseYear: The string contents of the GUI element containing the user-selected base year
    **/
    boolean runGapFillingProcedure(int sourceTypeID, JComboBox<String> gapFillingComboBox, String baseYear) {
        String method = gapFillingComboBox.getSelectedItem().toString();
        if (method == null || method == "") {
            messages.add("ERROR: Could not read gap-filling selection for " + sourceTypeID + "s.");
            Logger.log(LogMessageCategory.ERROR, "Could not read gap-filling selection for " + sourceTypeID + "s.");
            return false;
        }
        String procedureName = "";
        switch (method) {
            case GAP_FILLING_DEFAULTS: 
                procedureName = "AVFTTool_GapFilling_WithDefaults"; 
                break;
            case GAP_FILLING_0S: 
                procedureName = "AVFTTool_GapFilling_With0s"; 
                break;
            default:
                break;
        }

        if (procedureName == "") {
            messages.add("ERROR: Unknown gap-filling selection for " + sourceTypeID + "s: "+ method);
            Logger.log(LogMessageCategory.ERROR, "Unknown gap-filling selection for " + sourceTypeID + "s: "+ method);
        } else {
            String sql = String.format("call %s(%s, %d)", procedureName, baseYear, sourceTypeID); 
            Logger.log(LogMessageCategory.DEBUG, sql);
            try {
                DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
            } catch (SQLException e) {
                String stackTrace = "";
                for (StackTraceElement ste : e.getStackTrace()) {
                    stackTrace += "\n" + ste.toString();
                }
                Logger.log(LogMessageCategory.ERROR, e.toString() + stackTrace);
                messages.add("ERROR: encountered SQL error: " + e.getMessage());
                messages.add("\tMore information is available in moveslog.txt");
            }
        }

        if(doesMessagesContainError()) {
            JOptionPane.showMessageDialog(frame, "Error running AVFT Tool during gap-filling. Check the messages list for details.",
                                    "AVFT Tool", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        return true;
    }

    /** Runs the projection stored procedure for the given source type 
     * @param sourceTypeID: The source type that the given projection method should be used on
     * @param projectionComboBox: The GUI element containing the user-selected projection method for the given source type
     * @param baseYear: The string contents of the GUI element containing the user-selected base year
     * @param analysisYear: The string contents of the GUI element containing the user-selected analysis year
    **/
    boolean runProjectionProcedure(int sourceTypeID, JComboBox<String> projectionComboBox, String baseYear, String analysisYear) {
        String method = projectionComboBox.getSelectedItem().toString();
        if (method == null || method == "") {
            messages.add("ERROR: Could not read projection selection for " + sourceTypeID + "s.");
            Logger.log(LogMessageCategory.ERROR, "Could not read projection selection for " + sourceTypeID + "s.");
            return false;
        }
        String procedureName = "";
        switch (method) {
            case PROJECTION_CONSTANT: 
                procedureName = "AVFTTool_Projection_Constant"; 
                break;
            case PROJECTION_NATIONAL: 
                procedureName = "AVFTTool_Projection_National"; 
                break;
            case PROJECTION_PROPORTIONAL: 
                procedureName = "AVFTTool_Projection_Proportional"; 
                break;  
            case PROJECTION_KNOWN: 
                procedureName = "AVFTTool_Projection_KnownFractions"; 
                break;  
            default:
                break;
        }

        if (procedureName == "") {
            messages.add("ERROR: Unknown projection method selection for " + sourceTypeID + "s: "+ method);
            Logger.log(LogMessageCategory.ERROR, "Unknown projection method selection for " + sourceTypeID + "s: "+ method);
        } else {
            String sql = String.format("call %s(%s, %s, %d)", procedureName, baseYear, analysisYear, sourceTypeID); 
            Logger.log(LogMessageCategory.DEBUG, sql);
            try {
                DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
            } catch (SQLException e) {
                String stackTrace = "";
                for (StackTraceElement ste : e.getStackTrace()) {
                    stackTrace += "\n" + ste.toString();
                }
                Logger.log(LogMessageCategory.ERROR, e.toString() + stackTrace);
                messages.add("ERROR: encountered SQL error: " + e.getMessage());
                messages.add("\tMore information is available in moveslog.txt");
            }
        }
        
        if(doesMessagesContainError()) {
            JOptionPane.showMessageDialog(frame, "Error running AVFT Tool while projecting. Check the messages list for details.",
                                    "AVFT Tool", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        return true;
    }

    /** Saves the output from the AVFTTool (held in the interim database's outputAVFT table) in a 
     *  copy of the OutputTemplate, which plots the results for easy QA. **/
    void writeOutput() {
        try {
            // Open output file template
            FileInputStream fis = new FileInputStream(new File("database/AVFTTool/OutputTemplate.xlsx"));
            XSSFWorkbook workbook = new XSSFWorkbook(fis);
            XSSFSheet sheet = workbook.getSheet("AVFT");
            int rowCount = sheet.getLastRowNum();
 
            // load data to write
            Connection db = manager.database.openConnection();
            SQLRunner.Query query = new SQLRunner.Query();
            query.open(db, "SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, COALESCE(fuelEngFraction, 0) AS fuelEngFraction FROM outputAVFT");

            // Write each row
            while(query.rs.next()) {
                int columnCount = 0;
                XSSFRow row = sheet.createRow(++rowCount);
                row.createCell(columnCount++).setCellValue(query.rs.getInt("sourceTypeID"));
                row.createCell(columnCount++).setCellValue(query.rs.getInt("modelYearID"));
                row.createCell(columnCount++).setCellValue(query.rs.getInt("fuelTypeID"));
                row.createCell(columnCount++).setCellValue(query.rs.getInt("engTechID"));
                row.createCell(columnCount++).setCellValue(query.rs.getDouble("fuelEngFraction")); 
            }
            fis.close();
 
            // refresh formulas
            workbook.setForceFormulaRecalculation(true);

            // Write the workbook to the output file
            FileOutputStream fos = new FileOutputStream(outputFileFullPath);
            workbook.write(fos);
            fos.close();
 
        } catch (IOException e) {
            if (e.getMessage().contains("The process cannot access the file because it is being used by another process")) {
                messages.add("ERROR: Cannot access the output file because it is being used by another process.");
                messages.add("Please close the file and try again.");
            } else {
                messages.add("Error: Could not save the the output. More details are in the MOVES log.");
                Logger.log(LogMessageCategory.ERROR, e.toString());
            }
        } catch (SQLException e) {
            messages.add("Error: Could not save the output. More details are in the MOVES log.");
            Logger.log(LogMessageCategory.ERROR, e.toString());
        } catch (ClassNotFoundException e) {
            messages.add("Error: Could not save the output. More details are in the MOVES log.");
            Logger.log(LogMessageCategory.ERROR, e.toString());
        }
    }

    /** Runs the avft importer for the given file/worksheet
     * @param sqlTableName: what should the avft importer's final table be called?
     * @param fileName: name of the file to load
     * @param fileType: file type of the file to load
     * @param worksheetName: the worksheet to load
     * @param db: a connection to the database that the importer is using
     * @param messages: the list of messages for communicating with the user
     * @return whether or not any errors were encountered
    **/
    boolean doImport(String sqlTableName, String fileName, String fileType, String worksheetName, Connection db, boolean ignoreErrors) {
        boolean hasErrors = false;

        // set up the importer to import the specified file
        fuelImporter.avftPart.fileName = fileName;
        fuelImporter.avftPart.fileType = fileType;
        fuelImporter.avftPart.worksheetName = worksheetName;

        // actually do the import
        ArrayList<String> importerMsgs = new ArrayList<String>();
        manager.doImport(importerMsgs);

        // if there were any errors (and we are not ignoring them), copy those over to the messages displayed to the user
        // all other messages are always ignored
        if (!ignoreErrors) {
            for (String msg : importerMsgs) {
                if (msg.toLowerCase().contains("error") || msg.toLowerCase().contains("unable")) {
                    if (!hasErrors) { // first error we are encountering, so prepend another error so user knows what is causing these errors
                        messages.add("Errors importing data from " + fileName);
                    }
                    hasErrors = true;
                    messages.add(msg);
                }
            }
        }

        try {
            // rename the avft table (necessary because there are multiple avft files that could be imported)
            String sql = "drop table if exists " + sqlTableName;
            SQLRunner.executeSQL(db, sql);
            sql = "create table " + sqlTableName + " like avft";
            SQLRunner.executeSQL(db, sql);
            sql = "insert into " + sqlTableName + " select * from avft";
            SQLRunner.executeSQL(db, sql);
            sql = "drop table avft";
            SQLRunner.executeSQL(db, sql);

            // clean up after importer (the FuelImporter by default also creates these other tables)
            sql = "drop table if exists fuelformulation";
            SQLRunner.executeSQL(db, sql);   
            sql = "drop table if exists fuelsupply";
            SQLRunner.executeSQL(db, sql);   
            sql = "drop table if exists fuelsupplyyear";
            SQLRunner.executeSQL(db, sql);   
            sql = "drop table if exists fuelusagefraction";
            SQLRunner.executeSQL(db, sql);  
            sql = "drop table if exists year";
            SQLRunner.executeSQL(db, sql);  

        } catch (SQLException e) {
            hasErrors = true;
            messages.add("Error: Problem importing data for the AVFT Tool. See log for more details.");
            Logger.log(LogMessageCategory.ERROR, "Error importing data for the AVFT Tool: " + e.toString());
        }

        return hasErrors;
    }

	/** Handle the Save Messages button **/
	void handleSaveMessagesButton() {
        FileDialog fd = new FileDialog(frame, "Specify save file name (*.txt):", FileDialog.SAVE);
        fd.setFile("*.txt");
        fd.setVisible(true);

        if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
            return;
        }
        String filePath = fd.getDirectory() + fd.getFile();
        
        try {
            FileWriter writer = new FileWriter(filePath);
            for(Iterator i=messages.iterator();i.hasNext();) {
                writer.write((String)i.next() + "\r\n");
            }                
            writer.close();
        } catch (IOException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

	/** Handle the open button for the help file **/
	void handleOpenHelpButton() {
		try {
			File file = new File("database/AVFTTool/AVFTToolHelp.pdf");
			if(!file.exists()) {
				Logger.log(LogMessageCategory.ERROR, "Could not find the help file at: " + file.getAbsolutePath());
				return;
			}
			if(!OpenFile.open(file)) {
				return;
			}
		} catch(Exception e) {
			Logger.log(LogMessageCategory.ERROR, "Could not open the help file: " + e);
			return;
		}
	}

	/** Handle the Done button **/
	void handleDoneButton() {
		dispose();
	}

	/** Reset the list of messages **/
	void resetMessages() {
		messagesHasError = false;
		messages.clear();
		populateMessagesList();
	}

	/** Push the list of messages onto the screen **/
	void populateMessagesList() {
		messageListModel.clear();
		for(Iterator i=messages.iterator();i.hasNext();) {
			String m = (String)i.next();
			messageListModel.addElement(m);
		}
        saveMessagesButton.setEnabled(messageListModel.size() > 0);
	}

    /**
	 * Returns whether or not the messages list contains an error. Also updates the instance variable messagesHasError
	**/
	private boolean doesMessagesContainError() {
        messagesHasError = false;

        for (String message : messages) {
            if (message.toLowerCase().contains("error:")) {
                messagesHasError = true;
                break;
            }
        }

        return messagesHasError;
    }
}
