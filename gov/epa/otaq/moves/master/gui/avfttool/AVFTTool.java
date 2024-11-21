/**************************************************************************************************
 * @(#)AVFTTool.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui.avfttool;

import java.awt.*;
import java.awt.event.*;
import java.sql.*;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.basic.BasicBorders;
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
import gov.epa.otaq.moves.master.gui.MOVESWindow;
import gov.epa.otaq.moves.master.gui.avfttool.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.importers.ImporterManager;
import gov.epa.otaq.moves.master.framework.importers.TableFileLinkagePart;
import gov.epa.otaq.moves.master.implementation.importers.FuelImporter;

/**
 * Runs the AVFT Tool
 * @version 	2023-02-28
**/
public class AVFTTool extends JDialog implements ActionListener, FocusListener {
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
    /** Tool input combo boxes & enabling checkboxes**/
	JComboBox<String> lastCompleteMYCombo;
	JComboBox<String> analysisYearCombo;
    JCheckBox allEnableCheck;
    JCheckBox enable11Check;
	JComboBox<String> gapFillingMethod11Combo;
	JComboBox<String> projectionMethod11Combo;
    JCheckBox enable21Check;
	JComboBox<String> gapFillingMethod21Combo;
	JComboBox<String> projectionMethod21Combo;
    JCheckBox enable31Check;
	JComboBox<String> gapFillingMethod31Combo;
	JComboBox<String> projectionMethod31Combo;
    JCheckBox enable32Check;
	JComboBox<String> projectionMethod32Combo;
	JComboBox<String> gapFillingMethod32Combo;
    JCheckBox enable41Check;
	JComboBox<String> projectionMethod41Combo;
	JComboBox<String> gapFillingMethod41Combo;
    JCheckBox enable42Check;
	JComboBox<String> projectionMethod42Combo;
	JComboBox<String> gapFillingMethod42Combo;
    JCheckBox enable43Check;
	JComboBox<String> projectionMethod43Combo;
	JComboBox<String> gapFillingMethod43Combo;
    JCheckBox enable51Check;
	JComboBox<String> projectionMethod51Combo;
	JComboBox<String> gapFillingMethod51Combo;
    JCheckBox enable52Check;
	JComboBox<String> projectionMethod52Combo;
	JComboBox<String> gapFillingMethod52Combo;
    JCheckBox enable53Check;
	JComboBox<String> projectionMethod53Combo;
	JComboBox<String> gapFillingMethod53Combo;
    JCheckBox enable54Check;
	JComboBox<String> projectionMethod54Combo;
	JComboBox<String> gapFillingMethod54Combo;
    JCheckBox enable61Check;
	JComboBox<String> projectionMethod61Combo;
	JComboBox<String> gapFillingMethod61Combo;
    JCheckBox enable62Check;
	JComboBox<String> projectionMethod62Combo;
	JComboBox<String> gapFillingMethod62Combo;
	/** List of messages **/
	JList<String> messagesList;
	/** Button to run the tool **/
	JButton runAVFTToolButton;
	/** Button to save messages **/
	JButton saveMessagesButton;
	/** Button to save AVFTToolSpec **/
	JButton saveAVFTToolSpecButton;
	/** Button to load AVFTToolSpec **/
	JButton loadAVFTToolSpecButton;
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
    String inputAVFTFileWorkbook;
    String knownFractionsFilePath;
    String knownFractionsFileWorkbook;

    /** variables to hold the optional input/output AVFT db names **/
    String inputAVFTdb = "";
    String outputAVFTdb = "";

	/**
	 * Browser dialog for selecting an output directory.  Retained as a member
	 * variable so it would retain the last used directory.
	**/
	JFileChooser folderChooser = null;

    /** Dialog for collecting what fuel types are known for each source type where
     *  Known Fractions is selected, for the purposes of creating a relevant template **/
    JDialog knownFractionsDialog;

    /** define control constants */
    static final String GAP_FILLING_AUTOMATIC = "Automatic";
    static final String GAP_FILLING_DEFAULTS_RENORMALIZE_INPUTS = "Use defaults, renormalize inputs";
    static final String GAP_FILLING_DEFAULTS_PRESERVE_INPUTS = "Use defaults, preserve inputs";
    static final String[] GAP_FILLING_METHODS = {GAP_FILLING_AUTOMATIC, GAP_FILLING_DEFAULTS_RENORMALIZE_INPUTS,
        GAP_FILLING_DEFAULTS_PRESERVE_INPUTS};
    static final String PROJECTION_NATIONAL = "National Average";
    static final String PROJECTION_PROPORTIONAL = "Proportional";
    static final String PROJECTION_KNOWN = "Known Fractions";
    static final String PROJECTION_CONSTANT = "Constant";
    static final String[] PROJECTION_METHODS = {PROJECTION_PROPORTIONAL, PROJECTION_NATIONAL, 
        PROJECTION_KNOWN, PROJECTION_CONSTANT};

	/**
	 * Constructs the FERC panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	 * @param modeToUse Default conversion mode
	**/
	public AVFTTool(JFrame parent) {
		super(parent, MOVESWindow.MOVES_VERSION + " - AVFT Tool");
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
        lastCompleteMYCombo = new JComboBox<String>(new String[] {"", "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025","2026","2027","2028","2029","2030","2031","2032","2033","2034","2035","2036","2037","2038","2039","2040","2041","2042","2043","2044","2045","2046","2047","2048","2049","2050","2051","2052","2053","2054","2055","2056","2057","2058","2059"});
        lastCompleteMYCombo.setSelectedItem("");
		ToolTipHelper.add(lastCompleteMYCombo, "What is the last complete model year in your input data?");
        lastCompleteMYCombo.setEditable(false);
        //lastCompleteMYCombo.addActionListener(this); // nothing to do when the user interacts with this control

        analysisYearCombo = new JComboBox<String>(new String[] {"", "1990", "1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025","2026","2027","2028","2029","2030","2031","2032","2033","2034","2035","2036","2037","2038","2039","2040","2041","2042","2043","2044","2045","2046","2047","2048","2049","2050","2051","2052","2053","2054","2055","2056","2057","2058","2059","2060"}); // include one year so that control is sized correctly
        analysisYearCombo.setSelectedItem("");
		ToolTipHelper.add(analysisYearCombo, "What year should the AVFT Tool project to?");
        analysisYearCombo.setEditable(false);
        //analysisYearCombo.addActionListener(this);  // nothing to do when the user interacts with this control

        allEnableCheck = new JCheckBox();
        allEnableCheck.setSelected(true);
		ToolTipHelper.add(allEnableCheck, "Enable/Disable all source types");
        allEnableCheck.addActionListener(this);
        allEnableCheck.addFocusListener(this);

        enable11Check = new JCheckBox();
        enable11Check.setSelected(true);
		ToolTipHelper.add(enable11Check, "Enable/Disable motorcycles. Disabled source types will not be included in the output.");
        enable11Check.addActionListener(this);
        enable11Check.addFocusListener(this);
        
        enable21Check = new JCheckBox();
        enable21Check.setSelected(true);
		ToolTipHelper.add(enable21Check, "Enable/Disable passenger cars. Disabled source types will not be included in the output.");
        enable21Check.addActionListener(this);
        enable21Check.addFocusListener(this);
        
        enable31Check = new JCheckBox();
        enable31Check.setSelected(true);
		ToolTipHelper.add(enable31Check, "Enable/Disable passenger trucks. Disabled source types will not be included in the output.");
        enable31Check.addActionListener(this);
        enable31Check.addFocusListener(this);
        
        enable32Check = new JCheckBox();
        enable32Check.setSelected(true);
		ToolTipHelper.add(enable32Check, "Enable/Disable light commercial trucks. Disabled source types will not be included in the output.");
        enable32Check.addActionListener(this);
        enable32Check.addFocusListener(this);
        
        enable41Check = new JCheckBox();
        enable41Check.setSelected(true);
		ToolTipHelper.add(enable41Check, "Enable/Disable other buses. Disabled source types will not be included in the output.");
        enable41Check.addActionListener(this);
        enable41Check.addFocusListener(this);
        
        enable42Check = new JCheckBox();
        enable42Check.setSelected(true);
		ToolTipHelper.add(enable42Check, "Enable/Disable transit buses. Disabled source types will not be included in the output.");
        enable42Check.addActionListener(this);
        enable42Check.addFocusListener(this);
        
        enable43Check = new JCheckBox();
        enable43Check.setSelected(true);
		ToolTipHelper.add(enable43Check, "Enable/Disable school buses. Disabled source types will not be included in the output.");
        enable43Check.addActionListener(this);
        enable43Check.addFocusListener(this);
        
        enable51Check = new JCheckBox();
        enable51Check.setSelected(true);
		ToolTipHelper.add(enable51Check, "Enable/Disable refuse trucks. Disabled source types will not be included in the output.");
        enable51Check.addActionListener(this);
        enable51Check.addFocusListener(this);
        
        enable52Check = new JCheckBox();
        enable52Check.setSelected(true);
		ToolTipHelper.add(enable52Check, "Enable/Disable single unit short-haul trucks. Disabled source types will not be included in the output.");
        enable52Check.addActionListener(this);
        enable52Check.addFocusListener(this);
        
        enable53Check = new JCheckBox();
        enable53Check.setSelected(true);
		ToolTipHelper.add(enable53Check, "Enable/Disable single unit long-haul trucks. Disabled source types will not be included in the output.");
        enable53Check.addActionListener(this);
        enable53Check.addFocusListener(this);
        
        enable54Check = new JCheckBox();
        enable54Check.setSelected(true);
		ToolTipHelper.add(enable54Check, "Enable/Disable motor homes. Disabled source types will not be included in the output.");
        enable54Check.addActionListener(this);
        enable54Check.addFocusListener(this);
        
        enable61Check = new JCheckBox();
        enable61Check.setSelected(true);
		ToolTipHelper.add(enable61Check, "Enable/Disable combination short-haul trucks. Disabled source types will not be included in the output.");
        enable61Check.addActionListener(this);
        enable61Check.addFocusListener(this);
        
        enable62Check = new JCheckBox();
        enable62Check.setSelected(true);
		ToolTipHelper.add(enable62Check, "Enable/Disable combination long-haul trucks. Disabled source types will not be included in the output.");
        enable62Check.addActionListener(this);
        enable62Check.addFocusListener(this);

        gapFillingMethod11Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod11Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod11Combo,  "What gap filling method should be used for motorcycles?");
        gapFillingMethod11Combo.setEditable(false);
        gapFillingMethod11Combo.addActionListener(this);
        
        gapFillingMethod21Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod21Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod21Combo,  "What gap filling method should be used for passenger cars?");
        gapFillingMethod21Combo.setEditable(false);
        gapFillingMethod21Combo.addActionListener(this);
        
        gapFillingMethod31Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod31Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod31Combo,  "What gap filling method should be used for passenger trucks?");
        gapFillingMethod31Combo.setEditable(false);
        gapFillingMethod31Combo.addActionListener(this);
        
        gapFillingMethod32Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod32Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod32Combo,  "What gap filling method should be used for light commercial trucks?");
        gapFillingMethod32Combo.setEditable(false);
        gapFillingMethod32Combo.addActionListener(this);
        
        gapFillingMethod41Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod41Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod41Combo,  "What gap filling method should be used for other buses?");
        gapFillingMethod41Combo.setEditable(false);
        gapFillingMethod41Combo.addActionListener(this);
        
        gapFillingMethod42Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod42Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod42Combo,  "What gap filling method should be used for transit buses?");
        gapFillingMethod42Combo.setEditable(false);
        gapFillingMethod42Combo.addActionListener(this);
        
        gapFillingMethod43Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod43Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod43Combo,  "What gap filling method should be used for school buses?");
        gapFillingMethod43Combo.setEditable(false);
        gapFillingMethod43Combo.addActionListener(this);
        
        gapFillingMethod51Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod51Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod51Combo,  "What gap filling method should be used for refuse trucks?");
        gapFillingMethod51Combo.setEditable(false);
        gapFillingMethod51Combo.addActionListener(this);
        
        gapFillingMethod52Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod52Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod52Combo,  "What gap filling method should be used for single unit short-haul trucks?");
        gapFillingMethod52Combo.setEditable(false);
        gapFillingMethod52Combo.addActionListener(this);
        
        gapFillingMethod53Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod53Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod53Combo,  "What gap filling method should be used for single unit long-haul trucks?");
        gapFillingMethod53Combo.setEditable(false);
        gapFillingMethod53Combo.addActionListener(this);
        
        gapFillingMethod54Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod54Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod54Combo,  "What gap filling method should be used for motor homes?");
        gapFillingMethod54Combo.setEditable(false);
        gapFillingMethod54Combo.addActionListener(this);
        
        gapFillingMethod61Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod61Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod61Combo,  "What gap filling method should be used for combination short-haul trucks?");
        gapFillingMethod61Combo.setEditable(false);
        gapFillingMethod61Combo.addActionListener(this);
        
        gapFillingMethod62Combo = new JComboBox<String>(GAP_FILLING_METHODS);
        gapFillingMethod62Combo.setSelectedItem(GAP_FILLING_AUTOMATIC);
		ToolTipHelper.add(gapFillingMethod62Combo,  "What gap filling method should be used for combination long-haul trucks?");
        gapFillingMethod62Combo.setEditable(false);
        gapFillingMethod62Combo.addActionListener(this);

        projectionMethod11Combo = new JComboBox<String>(PROJECTION_METHODS);
        projectionMethod11Combo.setSelectedItem(PROJECTION_PROPORTIONAL);
		ToolTipHelper.add(projectionMethod11Combo,  "What projection method should be used for motorcycles?");
        projectionMethod11Combo.setEditable(false);
        projectionMethod11Combo.addActionListener(this);

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
		saveAVFTToolSpecButton = new JButton();
		saveAVFTToolSpecButton.addActionListener(this);
		loadAVFTToolSpecButton = new JButton();
		loadAVFTToolSpecButton.addActionListener(this);
		doneButton = new JButton();
		doneButton.addActionListener(this);
		openHelpButton = new JButton();
		openHelpButton.addActionListener(this);
		
        // messages
        messageListModel = new DefaultListModel<String>();
        messagesList = new JListWithToolTips<String>(messageListModel);
        messagesList.setSelectedIndex(-1);
        messagesList.setVisibleRowCount(5);
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
            int cCheck = 1;
            int cGapFilling = 2;
            int cProjection = 4;

            openHelpButton.setText("Open Help");
            openHelpButton.setMnemonic('H');
            ToolTipHelper.add(openHelpButton,"Open the help document (.pdf)");
            inputsInset.add(openHelpButton, new GridBagConstraints(cProjection, r, 2, 2, 0.0, 0.0,
                GridBagConstraints.EAST, GridBagConstraints.BOTH, new Insets(0, 0, 1, 1), 0, 0));

            JLabel lcmyLabel = new JLabel("Last complete model year in input data:");
            lcmyLabel.setDisplayedMnemonic('y');
            lcmyLabel.setLabelFor(lastCompleteMYCombo);
            inputsInset.add(lcmyLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 0), 0, 0));
            inputsInset.add(lastCompleteMYCombo, new GridBagConstraints(cCheck, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 1, 0), 0, 0));
                
            r++;
            JLabel analysisYearLabel = new JLabel("Analysis year:");
            analysisYearLabel.setDisplayedMnemonic('s');
            analysisYearLabel.setLabelFor(analysisYearCombo);
            inputsInset.add(analysisYearLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 0), 0, 0));
            inputsInset.add(analysisYearCombo, new GridBagConstraints(cCheck, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 1, 0), 0, 0));

            r++;
            inputsInset.add(allEnableCheck, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 1, 5), 0, 0));
            inputsInset.add(new JLabel("Gap-filling Method:"), new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0, 
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 1, 5), 0, 0));
            inputsInset.add(new JLabel("Projection Method:"), new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0, 
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 1, 5), 0, 0));

            r++;
            JLabel mcLabel = new JLabel("Motorcycles (11):");
            mcLabel.setDisplayedMnemonic('1');
            mcLabel.setLabelFor(enable11Check);
            inputsInset.add(mcLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable11Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod11Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod11Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            r++;
            JLabel pcLabel = new JLabel("Passenger Cars (21):");
            pcLabel.setDisplayedMnemonic('P');
            pcLabel.setLabelFor(enable21Check);
            inputsInset.add(pcLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable21Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod21Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod21Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            r++;
            JLabel ptLabel = new JLabel("Passenger Trucks (31):");
            ptLabel.setDisplayedMnemonic('a');
            ptLabel.setLabelFor(enable31Check);
            inputsInset.add(ptLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable31Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod31Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod31Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        
            r++;
            JLabel ldcLabel = new JLabel("LD Commercial Trucks (32):");
            ldcLabel.setDisplayedMnemonic('L');
            ldcLabel.setLabelFor(enable32Check);
            inputsInset.add(ldcLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable32Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod32Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod32Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));

            r++;
            JLabel otherLabel = new JLabel("Other Buses (41):");
            otherLabel.setDisplayedMnemonic('B');
            otherLabel.setLabelFor(enable41Check);
            inputsInset.add(otherLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable41Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod41Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod41Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));


            r++;
            JLabel transitLabel = new JLabel("Transit Buses (42):");
            transitLabel.setDisplayedMnemonic('T');
            transitLabel.setLabelFor(enable42Check);
            inputsInset.add(transitLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable42Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod42Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod42Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        
            r++;
            JLabel schoolLabel = new JLabel("School Buses (43):");
            schoolLabel.setDisplayedMnemonic('u');
            schoolLabel.setLabelFor(enable43Check);
            inputsInset.add(schoolLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable43Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod43Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod43Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        

            r++;
            JLabel refuseLabel = new JLabel("Refuse Trucks (51):");
            refuseLabel.setDisplayedMnemonic('f');
            refuseLabel.setLabelFor(enable51Check);
            inputsInset.add(refuseLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable51Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod51Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod51Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));


            r++;
            JLabel sushLabel = new JLabel("Single Unit Short-haul (52):");
            sushLabel.setDisplayedMnemonic('5');
            sushLabel.setLabelFor(enable52Check);
            inputsInset.add(sushLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable52Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod52Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod52Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        
            r++;
            JLabel sulhLabel = new JLabel("Single Unit Long-haul (53):");
            sulhLabel.setDisplayedMnemonic('e');
            sulhLabel.setLabelFor(enable53Check);
            inputsInset.add(sulhLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable53Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod53Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod53Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        

            r++;
            JLabel rvLabel = new JLabel("Motor Homes (54):");
            rvLabel.setDisplayedMnemonic('M');
            rvLabel.setLabelFor(enable54Check);
            inputsInset.add(rvLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable54Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod54Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod54Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));


            r++;
            JLabel cshLabel = new JLabel("Combination Short-haul (61):");
            cshLabel.setDisplayedMnemonic('C');
            cshLabel.setLabelFor(enable61Check);
            inputsInset.add(cshLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable61Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod61Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod61Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
        
            r++;
            JLabel clhLabel = new JLabel("Combination Long-haul (62):");
            clhLabel.setDisplayedMnemonic('g');
            clhLabel.setLabelFor(enable62Check);
            inputsInset.add(clhLabel, new GridBagConstraints(0, r, 1, 1, 0.0, 0.0, 
                GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(enable62Check, new GridBagConstraints(cCheck, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(gapFillingMethod62Combo, new GridBagConstraints(cGapFilling, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));
            inputsInset.add(projectionMethod62Combo, new GridBagConstraints(cProjection, r, 2, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 1, 5), 0, 0));                       

            panel.add(inputsInset, new GridBagConstraints(0, 0, 5, 3, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 2, 0), 0, 0));
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
            ToolTipHelper.add(browseOutputFileButton,"Select the output file (.xlsx)");
            filesInset.add(browseOutputFileButton, new GridBagConstraints(c++, r, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE,
                new Insets(0, 0, 1, 0), 0, 0));

            panel.add(filesInset, new GridBagConstraints(0, 3, 5, 2, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE,
                new Insets(0, 0, 2, 0), 0, 0));
        }

        // messages inset
        {
            JPanel messagesInset = new JPanel();
            messagesInset.setBorder(BorderFactory.createTitledBorder("Messages"));
            messagesInset.setLayout(new GridBagLayout());
            ((GridBagLayout)messagesInset.getLayout()).columnWidths = new int[] {106, 0, 0, 0, 0, 0};
            ((GridBagLayout)messagesInset.getLayout()).rowHeights = new int[] {72, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
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
                new Insets(0, 0, 2, 0), 0, 0));
        }

        // buttons
        {
            JPanel buttonInset = new JPanel();
            buttonInset.setLayout(new GridBagLayout());
            ((GridBagLayout)buttonInset.getLayout()).columnWidths = new int[] {0, 0, 0, 0, 0, 0};
            ((GridBagLayout)buttonInset.getLayout()).rowHeights = new int[] {30};
            ((GridBagLayout)buttonInset.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 0.0, 1e-4, 0.0};
            ((GridBagLayout)buttonInset.getLayout()).rowWeights = new double[] {0.0};
            int c = 0;

            runAVFTToolButton.setText("Run AVFT Tool");
            ToolTipHelper.add(runAVFTToolButton,"Run the tool using the provided inputs and save results to the output file.");
            runAVFTToolButton.setMnemonic('R');
            runAVFTToolButton.setDisplayedMnemonicIndex(0);
            buttonInset.add(runAVFTToolButton, new GridBagConstraints(c++, 0, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 5, 2, 5), 0, 0));
            
            saveMessagesButton.setText("Save Messages");
            ToolTipHelper.add(saveMessagesButton,"Save above messages as a text file.");
            saveMessagesButton.setMnemonic('S');
            saveMessagesButton.setDisplayedMnemonicIndex(0);
            buttonInset.add(saveMessagesButton, new GridBagConstraints(c++, 0, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 2, 2, 2), 0, 0));
            
            saveAVFTToolSpecButton.setText("Save Selections");
            ToolTipHelper.add(saveAVFTToolSpecButton,"Save all tool settings to an .xml file.");
            saveAVFTToolSpecButton.setMnemonic('v');
            saveAVFTToolSpecButton.setDisplayedMnemonicIndex(0);
            buttonInset.add(saveAVFTToolSpecButton, new GridBagConstraints(c++, 0, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 2, 2, 2), 0, 0));
            
            loadAVFTToolSpecButton.setText("Load Selections");
            ToolTipHelper.add(loadAVFTToolSpecButton,"Load tool settings from an .xml file.");
            loadAVFTToolSpecButton.setMnemonic('n');
            loadAVFTToolSpecButton.setDisplayedMnemonicIndex(0);
            buttonInset.add(loadAVFTToolSpecButton, new GridBagConstraints(c++, 0, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 2, 2, 2), 0, 0));
            
            c++; // add a space before the Done button
            doneButton.setText("Done");
            ToolTipHelper.add(doneButton,"Close this window");
            doneButton.setMnemonic('D');
            doneButton.setDisplayedMnemonicIndex(0);
            buttonInset.add(doneButton, new GridBagConstraints(c++, 0, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 2, 2, 2), 0, 0));

            panel.add(buttonInset, new GridBagConstraints(0, 8, 5, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(0, 2, 2, 2), 0, 0));
        }

		return panel;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
        try {
            if(e.getSource() == browseInputFileButton) {
                handleBrowseInputFileButton();
            } else if(e.getSource() == createInputTemplateButton) {
                handleAVFTTemplateButton();
            }  else if(e.getSource() == browseKnownFractionsFileButton) {
                handleBrowseKnownFractionsFileButton();
            } else if(e.getSource() == createKnownFractionsTemplateButton) {
                handleKnownFractionsTemplateButton();
            } else if(e.getSource() == browseOutputFileButton) {
                handleBrowseOutputFileButton();
            } else if(e.getSource() == runAVFTToolButton) {
                handleRunAVFTToolButton();
            } else if(e.getSource() == saveMessagesButton) {
                handleSaveMessagesButton();
            } else if(e.getSource() == saveAVFTToolSpecButton) {
                handleSaveAVFTToolSpecButton();
            }  else if(e.getSource() == loadAVFTToolSpecButton) {
                handleLoadAVFTToolSpecButton();
            } else if(e.getSource() == openHelpButton) {
                handleOpenHelpButton();
            } else if(e.getSource() == doneButton) {
                handleDoneButton();
            } else if(e.getSource() == projectionMethod11Combo || e.getSource() == projectionMethod21Combo || e.getSource() == projectionMethod31Combo ||
                      e.getSource() == projectionMethod32Combo || e.getSource() == projectionMethod41Combo || e.getSource() == projectionMethod42Combo ||
                      e.getSource() == projectionMethod43Combo || e.getSource() == projectionMethod54Combo || e.getSource() == projectionMethod61Combo ||
                      e.getSource() == projectionMethod62Combo) {
                handleProjectComboChange();
            } else if(e.getSource() == allEnableCheck) {
                handleAllEnableCheckChange();
            } else if(e.getSource() == enable11Check) {
                handleEnableCheckChange(gapFillingMethod11Combo, projectionMethod11Combo);
            } else if(e.getSource() == enable21Check) {
                handleEnableCheckChange(gapFillingMethod21Combo, projectionMethod21Combo);
            } else if(e.getSource() == enable31Check) {
                handleEnableCheckChange(gapFillingMethod31Combo, projectionMethod31Combo);
            } else if(e.getSource() == enable32Check) {
                handleEnableCheckChange(gapFillingMethod32Combo, projectionMethod32Combo);
            } else if(e.getSource() == enable41Check) {
                handleEnableCheckChange(gapFillingMethod41Combo, projectionMethod41Combo);
            } else if(e.getSource() == enable42Check) {
                handleEnableCheckChange(gapFillingMethod42Combo, projectionMethod42Combo);
            } else if(e.getSource() == enable43Check) {
                handleEnableCheckChange(gapFillingMethod43Combo, projectionMethod43Combo);
            } else if(e.getSource() == enable51Check) {
                handleEnableCheckChange(gapFillingMethod51Combo, projectionMethod51Combo);
            } else if(e.getSource() == enable52Check) {
                handleEnableCheckChange(gapFillingMethod52Combo, projectionMethod52Combo);
            } else if(e.getSource() == enable53Check) {
                handleEnableCheckChange(gapFillingMethod53Combo, projectionMethod53Combo);
            } else if(e.getSource() == enable54Check) {
                handleEnableCheckChange(gapFillingMethod54Combo, projectionMethod54Combo);
            } else if(e.getSource() == enable61Check) {
                handleEnableCheckChange(gapFillingMethod61Combo, projectionMethod61Combo);
            } else if(e.getSource() == enable62Check) {
                handleEnableCheckChange(gapFillingMethod62Combo, projectionMethod62Combo);
            }
        } catch(Exception ex) {
            String stackTrace = "";
            for (StackTraceElement ste : ex.getStackTrace()) {
                stackTrace += "\n" + ste.toString();
            }
            Logger.log(LogMessageCategory.ERROR, ex.toString() + stackTrace);
            
            messages.add("ERROR: " + ex.getMessage());
            messages.add("\tMore information is available in moveslog.txt");
        }
	}

    public void focusGained(FocusEvent e) {
        ((JComponent)e.getSource()).setBackground(new Color(163,184,204));
    }

    public void focusLost(FocusEvent e) {
        ((JComponent)e.getSource()).setBackground(getBackground());
    }

	/** Make sure the AVFT Tool script is available **/
	void findAVFTToolScript() {
		try {
			File file = new File("gov/epa/otaq/moves/master/gui/avfttool/AVFTTool.sql");
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
        fuelImporter.avftPart.fileTypeString = "";
        fuelImporter.avftPart.fileTypeCode = 0;
        fuelImporter.avftPart.worksheetName = "";
        fuelImporter.avftPart.handleBrowseButton();
        if (fuelImporter.avftPart.fileName.equals("")) {
            return;
        }
        inputAVFTFilePath = fuelImporter.avftPart.fileName;
        inputAVFTFileWorkbook = fuelImporter.avftPart.worksheetName;

        // update GUI
        updateInputFileGUI();
    }

	/** Updates the GUI controls for the Input File based on the values of internal variables.
     *  These variables are either set by the Fuel Importer [AVFT] or by loading an AVFTSpec **/
	void updateInputFileGUI() {
        String displayText = inputAVFTFilePath;
        String toolTipText = inputAVFTFilePath;
        if (displayText.length() > 27) { // max displayed file length is 30
            displayText = "..." + displayText.substring(displayText.length()-27);
        }
        if (inputAVFTFileWorkbook != null && !inputAVFTFileWorkbook.equals("")) {
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

    /** Handle the AVFT Template button **/
	void handleAVFTTemplateButton() {
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
        fuelImporter.avftPart.fileTypeString = "";
        fuelImporter.avftPart.fileTypeCode = 0;
        fuelImporter.avftPart.worksheetName = "";
        fuelImporter.avftPart.handleBrowseButton();
        if (fuelImporter.avftPart.fileName.equals("")) {
            return;
        }
        knownFractionsFilePath = fuelImporter.avftPart.fileName;
        knownFractionsFileWorkbook = fuelImporter.avftPart.worksheetName;

        // update GUI
        updateKnownFractionsFileGUI();
    }

	/** Updates the GUI controls for the Known Fractions File based on the values of internal variables.
     *  These variables are either set by the Fuel Importer [AVFT] or by loading an AVFTSpec **/
	void updateKnownFractionsFileGUI() {
        String displayText = knownFractionsFilePath;
        String toolTipText = knownFractionsFilePath;
        if (displayText.length() > 27) { // max displayed file length is 30
            displayText = "..." + displayText.substring(displayText.length()-27);
        }
        if (knownFractionsFileWorkbook != null && !knownFractionsFileWorkbook.equals("")) {
            if (displayText.length() + knownFractionsFileWorkbook.length() + 3 > 41) { // if displayed file length + " [workbook name]" is too long, truncate
                displayText += " [" + knownFractionsFileWorkbook.substring(0, 5) + "..." + "]";
            } else {
                displayText += " [" + knownFractionsFileWorkbook + "]";
            }
            toolTipText += " [" + knownFractionsFileWorkbook + "]";
        }
        knownFractionsFileText.setText(displayText);
        // store the full path as a tooltip if the displayed text is truncated, otherwise use original tooltip
        if (displayText.contains("...")) {
            ToolTipHelper.add(knownFractionsFileText,toolTipText);
        } else {
            ToolTipHelper.add(knownFractionsFileText,"Name and location of the known fractions input file (.xlsx/.xls/.csv/.txt)");
        }
	}

    /** Handle the Known Fractions Template button **/
	void handleKnownFractionsTemplateButton() {
        // load last complete model year and analysis year inputs
        String lastCompleteMY = lastCompleteMYCombo.getSelectedItem().toString();
        if (lastCompleteMY == null || lastCompleteMY.equals("")) {
            alertUser("Please specify the last complete model year before creating the known fractions template.",
                      "AVFT Tool", JOptionPane.ERROR_MESSAGE, false);
            return;
        }
        String analysisYear = analysisYearCombo.getSelectedItem().toString();
        if (analysisYear == null || analysisYear.equals("")) {
            alertUser("Please specify the analysis year before creating the known fractions template.",
                      "AVFT Tool", JOptionPane.ERROR_MESSAGE, false);
            return;
        }
        if (Integer.parseInt(lastCompleteMY) >= Integer.parseInt(analysisYear)) {
            alertUser("No projections will be done when the last complete model year is later than the analysis year.\n\nPlease select an analysis year after the last complete model year to create a known fractions template.",
                      "AVFT Tool", JOptionPane.ERROR_MESSAGE, false);
            return;
        }
        ArrayList<Integer> knownFractionsST = new ArrayList<Integer>();
        if (projectionMethod11Combo.isEnabled() & projectionMethod11Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(11);
        }
        if (projectionMethod21Combo.isEnabled() & projectionMethod21Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(21);
        }
        if (projectionMethod31Combo.isEnabled() & projectionMethod31Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(31);
        }
        if (projectionMethod32Combo.isEnabled() & projectionMethod32Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(32);
        }
        if (projectionMethod41Combo.isEnabled() & projectionMethod41Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(41);
        }
        if (projectionMethod42Combo.isEnabled() & projectionMethod42Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(42);
        }
        if (projectionMethod43Combo.isEnabled() & projectionMethod43Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(43);
        }
        if (projectionMethod51Combo.isEnabled() & projectionMethod51Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(51);
        }
        if (projectionMethod52Combo.isEnabled() & projectionMethod52Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(52);
        }
        if (projectionMethod53Combo.isEnabled() & projectionMethod53Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(53);
        }
        if (projectionMethod54Combo.isEnabled() & projectionMethod54Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(54);
        }
        if (projectionMethod61Combo.isEnabled() & projectionMethod61Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(61);
        }
        if (projectionMethod62Combo.isEnabled() & projectionMethod62Combo.getSelectedItem().toString() == PROJECTION_KNOWN) {
            knownFractionsST.add(62);
        }
        KnownFractionsTemplate atkft = new KnownFractionsTemplate(frame, knownFractionsST, 
            Integer.parseInt(lastCompleteMY), Integer.parseInt(analysisYear));
		// simple offset from main window origin
		atkft.setLocation(getLocationOnScreen().x + 100, getLocationOnScreen().y + 100);
		atkft.showModal();
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
                alertUser("This tool can only output *.xlsx files. Automatically changing extension.",
                          "AVFT Tool", JOptionPane.WARNING_MESSAGE, false);
                filePath = filePath + ".xlsx";
            }

            // populate controls
			File file = new File(filePath);
			outputFileFullPath = file.getCanonicalPath();
            updateOutputFileGUI();
		} catch(Exception e) {
			// Nothing to do here
		}
	}

	/** Updates the GUI controls for the Output File based on the values of internal variables. **/
	void updateOutputFileGUI() {
        File file = new File(outputFileFullPath);
        outputFileText.setText(file.getName());
    }

	/** Flips the enabled/disabled state of the passed user controls **/
    void handleEnableCheckChange(JComboBox<String> gapFillingMethodCombo, JComboBox<String> projectionMethodCombo) {
        boolean currentState = gapFillingMethodCombo.isEnabled();
        gapFillingMethodCombo.setEnabled(!currentState);
        projectionMethodCombo.setEnabled(!currentState);
    }

	/** Flips the enabled/disabled state of all source types **/
    void handleAllEnableCheckChange() {
        boolean newState = allEnableCheck.isSelected();
        if (enable11Check.isSelected() != newState) { enable11Check.doClick(0); }
        if (enable21Check.isSelected() != newState) { enable21Check.doClick(0); }
        if (enable31Check.isSelected() != newState) { enable31Check.doClick(0); }
        if (enable32Check.isSelected() != newState) { enable32Check.doClick(0); }
        if (enable41Check.isSelected() != newState) { enable41Check.doClick(0); }
        if (enable42Check.isSelected() != newState) { enable42Check.doClick(0); }
        if (enable43Check.isSelected() != newState) { enable43Check.doClick(0); }
        if (enable51Check.isSelected() != newState) { enable51Check.doClick(0); }
        if (enable52Check.isSelected() != newState) { enable52Check.doClick(0); }
        if (enable53Check.isSelected() != newState) { enable53Check.doClick(0); }
        if (enable54Check.isSelected() != newState) { enable54Check.doClick(0); }
        if (enable61Check.isSelected() != newState) { enable61Check.doClick(0); }
        if (enable62Check.isSelected() != newState) { enable62Check.doClick(0); }
    }

	/** Checks to see if any projection combo box is set to PROJECTION_KNOWN and enables the corresponding user controls **/
    void handleProjectComboChange() {
        if (projectionMethod11Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
            projectionMethod21Combo.getSelectedItem().toString() == PROJECTION_KNOWN ||
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

	/** Handle the Run AVFT Tool button. Returns true if run was successful, false otherwise **/
	public boolean handleRunAVFTToolButton() {
        setWaitCursor();
        boolean loggerState = Logger.skipHandlers;
		try {
            Logger.skipHandlers = true; // don't popup logged errors (already handling popups)
			resetMessages();

            // load last complete model year, analysis year, and output file name inputs
            String lastCompleteMY = lastCompleteMYCombo.getSelectedItem().toString();
            if (lastCompleteMY == null || lastCompleteMY.equals("")) {
                messages.add("Error: Specify the last complete model year.");
                alertUser("Missing input error. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
            }
            String analysisYear = analysisYearCombo.getSelectedItem().toString();
            if (analysisYear == null || analysisYear.equals("")) {
                messages.add("Error: Specify the analysis year.");
                alertUser("Missing input error. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
            }
            if (inputAVFTdb.equals("") && (inputAVFTFilePath == null || inputAVFTFilePath.equals(""))) { // only enforce if we don't have an input AVFT db
                messages.add("Error: Specify the input file.");
                alertUser("Missing input error. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
            }
            if (inputAVFTdb.equals("") && !(new File(inputAVFTFilePath).isFile())) { // only enforce if we don't have an input AVFT db
                messages.add("Error: Cannot find " + inputAVFTFilePath);
                alertUser("Cannot find the input AVFT file. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
            }
            if (knownFractionsLabel.isEnabled() && (knownFractionsFilePath == null || knownFractionsFilePath.equals(""))) {
                messages.add("Error: Specify the known fractions input file.");
                alertUser("Missing input error. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
            }
            if (knownFractionsLabel.isEnabled() && !(new File(knownFractionsFilePath).isFile())) {
                messages.add("Error: Cannot find " + knownFractionsFilePath);
                alertUser("Cannot find the known fractions file. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
            }
            if (outputFileFullPath == null || outputFileFullPath.equals("")) {
                messages.add("Error: Specify the output file.");
                alertUser("Missing input error. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
            }
            if (!(enable11Check.isSelected() || enable21Check.isSelected() || enable31Check.isSelected() || enable32Check.isSelected() || 
                  enable41Check.isSelected() || enable42Check.isSelected() || enable43Check.isSelected() || enable51Check.isSelected() || 
                  enable52Check.isSelected() || enable53Check.isSelected() || enable54Check.isSelected() || enable61Check.isSelected() || 
                  enable62Check.isSelected())) {
                messages.add("Error: At least one source type must be selected.");
                alertUser("Missing input error. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
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
                alertUser("Error creating the AVFT Tool's stored procedures. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
				return false;
			}
            
            // load input AVFT
            if(!inputAVFTdb.equals("")) { // load from database
				messages.add("Loading input AVFT data from " + inputAVFTdb + ".avft");
                SQLRunner.executeSQL(avftDB, "CREATE TABLE IF NOT EXISTS `inputAVFT` LIKE " + defaultDatabase.databaseName + ".avft");
                SQLRunner.executeSQL(avftDB, "INSERT INTO `inputAVFT` SELECT * FROM " + inputAVFTdb + ".avft");
            } else { // load from input file into the avftDatabase and rename imported table to inputAVFT
                messagesHasError = doImport("inputAVFT", inputAVFTFilePath, inputAVFTFileWorkbook, avftDB, false);
                if(messagesHasError) {
                    alertUser("Error importing the input AVFT table. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                    return false;
                }
            }

            // load Known Fractions AVFT file into the avftDatabase and rename table to knownAVFT
            // do not do regular error checking, because all the avft errors will be triggered by this input
            messagesHasError = doImport("knownAVFT", knownFractionsFilePath, knownFractionsFileWorkbook, avftDB, true);
            if(messagesHasError) {
                alertUser("Error importing the known fractions table. Check the messages list for details.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
			}

            // Create the default AVFT
            String sql = "call AVFTTool_CreateDefaultAVFT()";
            try {
                DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
            } catch (SQLException e) {
                String stackTrace = "";
                for (StackTraceElement ste : e.getStackTrace()) {
                    stackTrace += "\n" + ste.toString();
                }
                Logger.log(LogMessageCategory.ERROR, e.toString() + stackTrace);
                messages.add("ERROR: encountered SQL error in \"" + sql + "\":");
                messages.add("\t" + e.getMessage());
                messages.add("\tMore information is available in moveslog.txt");
            }

            // run gap-filling stored procedures
            if (runGapFillingProcedure(11, gapFillingMethod11Combo, lastCompleteMY, enable11Check.isSelected()) &&
                runGapFillingProcedure(21, gapFillingMethod21Combo, lastCompleteMY, enable21Check.isSelected()) &&
                runGapFillingProcedure(31, gapFillingMethod31Combo, lastCompleteMY, enable31Check.isSelected()) &&
                runGapFillingProcedure(32, gapFillingMethod32Combo, lastCompleteMY, enable32Check.isSelected()) &&
                runGapFillingProcedure(41, gapFillingMethod41Combo, lastCompleteMY, enable41Check.isSelected()) &&
                runGapFillingProcedure(42, gapFillingMethod42Combo, lastCompleteMY, enable42Check.isSelected()) &&
                runGapFillingProcedure(43, gapFillingMethod43Combo, lastCompleteMY, enable43Check.isSelected()) &&
                runGapFillingProcedure(51, gapFillingMethod51Combo, lastCompleteMY, enable51Check.isSelected()) &&
                runGapFillingProcedure(52, gapFillingMethod52Combo, lastCompleteMY, enable52Check.isSelected()) &&
                runGapFillingProcedure(53, gapFillingMethod53Combo, lastCompleteMY, enable53Check.isSelected()) &&
                runGapFillingProcedure(54, gapFillingMethod54Combo, lastCompleteMY, enable54Check.isSelected()) &&
                runGapFillingProcedure(61, gapFillingMethod61Combo, lastCompleteMY, enable61Check.isSelected()) &&
                runGapFillingProcedure(62, gapFillingMethod62Combo, lastCompleteMY, enable62Check.isSelected())) {
                // nothing to do here
            } else {
                // there was an error and the dialog has already been shown via the Run...Procedure(), so just return
                return false;
            }
            
            // run projection stored procedures
            if (runProjectionProcedure(11, projectionMethod11Combo, lastCompleteMY, analysisYear, enable11Check.isSelected()) &&
                runProjectionProcedure(21, projectionMethod21Combo, lastCompleteMY, analysisYear, enable21Check.isSelected()) &&
                runProjectionProcedure(31, projectionMethod31Combo, lastCompleteMY, analysisYear, enable31Check.isSelected()) &&
                runProjectionProcedure(32, projectionMethod32Combo, lastCompleteMY, analysisYear, enable32Check.isSelected()) &&
                runProjectionProcedure(41, projectionMethod41Combo, lastCompleteMY, analysisYear, enable41Check.isSelected()) &&
                runProjectionProcedure(42, projectionMethod42Combo, lastCompleteMY, analysisYear, enable42Check.isSelected()) &&
                runProjectionProcedure(43, projectionMethod43Combo, lastCompleteMY, analysisYear, enable43Check.isSelected()) &&
                runProjectionProcedure(51, projectionMethod51Combo, lastCompleteMY, analysisYear, enable51Check.isSelected()) &&
                runProjectionProcedure(52, projectionMethod52Combo, lastCompleteMY, analysisYear, enable52Check.isSelected()) &&
                runProjectionProcedure(53, projectionMethod53Combo, lastCompleteMY, analysisYear, enable53Check.isSelected()) &&
                runProjectionProcedure(54, projectionMethod54Combo, lastCompleteMY, analysisYear, enable54Check.isSelected()) &&
                runProjectionProcedure(61, projectionMethod61Combo, lastCompleteMY, analysisYear, enable61Check.isSelected()) &&
                runProjectionProcedure(62, projectionMethod62Combo, lastCompleteMY, analysisYear, enable62Check.isSelected())) {
                // nothing to do here
            } else {
                // there was an error and the dialog has already been shown via the Run...Procedure(), so just return
                return false;
            }
            
            // order results
            sql = "call AVFTTool_OrderResults()";
            try {
                DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
                if(doesMessagesContainError()) {
                    alertUser("Error cleaning up after running the tool; output was not saved.", "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                    return false;
                }
            } catch (SQLException e) {
                String stackTrace = "";
                for (StackTraceElement ste : e.getStackTrace()) {
                    stackTrace += "\n" + ste.toString();
                }
                Logger.log(LogMessageCategory.ERROR, e.toString() + stackTrace);
                messages.add("ERROR: encountered SQL error in \"" + sql + "\":");
                messages.add("\t" + e.getMessage());
                messages.add("\tMore information is available in moveslog.txt");
                return false;
            }

            // No errors. Write the output data
            writeOutput();
			if(doesMessagesContainError()) {
                alertUser("Error saving the output of the AVFT Tool. Check the messages list for details.",
                               "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
                return false;
            } else {
				alertUser("AVFT Tool finished.", "AVFT Tool", JOptionPane.INFORMATION_MESSAGE, true);
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
            return false;
		} finally {
			populateMessagesList();
            setDefaultCursor();
            Logger.skipHandlers = loggerState;
		}
        return true;
	}

    /** Runs the gap-filling stored procedure for the given source type 
     * @param sourceTypeID: The source type that the given gap-filling method should be used on
     * @param projectionComboBox: The GUI element containing the user-selected gap-filling method for the given source type
     * @param lastCompleteMY: The string contents of the GUI element containing the user-selected last complete model year
    **/
    boolean runGapFillingProcedure(int sourceTypeID, JComboBox<String> gapFillingComboBox, String lastCompleteMY, boolean isEnabled) {
        if (!isEnabled) {
            return true;
        }

        String method = gapFillingComboBox.getSelectedItem().toString();
        if (method == null || method.equals("")) {
            messages.add("ERROR: Could not read gap-filling selection for " + sourceTypeID + "s.");
            Logger.log(LogMessageCategory.ERROR, "Could not read gap-filling selection for " + sourceTypeID + "s.");
            return false;
        }
        String procedureName = "";
        switch (method) {
            case GAP_FILLING_AUTOMATIC: 
                procedureName = "AVFTTool_GapFilling_Automatic"; 
                break;
            case GAP_FILLING_DEFAULTS_RENORMALIZE_INPUTS: 
                procedureName = "AVFTTool_GapFilling_Defaults_Renormalize_Inputs"; 
                break;
            case GAP_FILLING_DEFAULTS_PRESERVE_INPUTS: 
                procedureName = "AVFTTool_GapFilling_Defaults_Preserve_Inputs"; 
                break;
            default:
                break;
        }

        if (procedureName.equals("")) {
            messages.add("ERROR: Unknown gap-filling selection for " + sourceTypeID + "s: "+ method);
            Logger.log(LogMessageCategory.ERROR, "Unknown gap-filling selection for " + sourceTypeID + "s: "+ method);
        } else {
            String sql = String.format("call %s(%s, %d)", procedureName, lastCompleteMY, sourceTypeID); 
            Logger.log(LogMessageCategory.DEBUG, sql);
            try {
                DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
            } catch (SQLException e) {
                String stackTrace = "";
                for (StackTraceElement ste : e.getStackTrace()) {
                    stackTrace += "\n" + ste.toString();
                }
                Logger.log(LogMessageCategory.ERROR, e.toString() + stackTrace);
                messages.add("ERROR: encountered SQL error in \"" + sql + "\":");
                messages.add("\t" + e.getMessage());
                messages.add("\tMore information is available in moveslog.txt");
            }
        }

        if(doesMessagesContainError()) {
            alertUser("Error running AVFT Tool during gap-filling. Check the messages list for details.",
                           "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
            return false;
        }
        return true;
    }

    /** Runs the projection stored procedure for the given source type 
     * @param sourceTypeID: The source type that the given projection method should be used on
     * @param projectionComboBox: The GUI element containing the user-selected projection method for the given source type
     * @param lastCompleteMY: The string contents of the GUI element containing the user-selected last complete model year
     * @param analysisYear: The string contents of the GUI element containing the user-selected analysis year
    **/
    boolean runProjectionProcedure(int sourceTypeID, JComboBox<String> projectionComboBox, String lastCompleteMY, String analysisYear, boolean isEnabled) {
        if (!isEnabled) {
            return true;
        }

        String method = projectionComboBox.getSelectedItem().toString();
        if (method == null || method.equals("")) {
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

        if (procedureName.equals("")) {
            messages.add("ERROR: Unknown projection method selection for " + sourceTypeID + "s: "+ method);
            Logger.log(LogMessageCategory.ERROR, "Unknown projection method selection for " + sourceTypeID + "s: "+ method);
        } else {
            String sql = String.format("call %s(%s, %s, %d)", procedureName, lastCompleteMY, analysisYear, sourceTypeID); 
            Logger.log(LogMessageCategory.DEBUG, sql);
            try {
                DatabaseUtilities.executeSqlStmtWithMessages(sql, manager.database, messages);
            } catch (SQLException e) {
                String stackTrace = "";
                for (StackTraceElement ste : e.getStackTrace()) {
                    stackTrace += "\n" + ste.toString();
                }
                Logger.log(LogMessageCategory.ERROR, e.toString() + stackTrace);
                messages.add("ERROR: encountered SQL error in \"" + sql + "\":");
                messages.add("\t" + e.getMessage());
                messages.add("\tMore information is available in moveslog.txt");
            }
        }
        
        if(doesMessagesContainError()) {
            alertUser("Error running AVFT Tool while projecting. Check the messages list for details.",
                           "AVFT Tool", JOptionPane.ERROR_MESSAGE, true);
            return false;
        }
        return true;
    }

    /** Saves the output from the AVFTTool (held in the interim database's outputAVFT table) in a 
     *  copy of the OutputTemplate, which plots the results for easy QA. **/
    void writeOutput() {
        Connection db = null;
        SQLRunner.Query query = null;
        try {
            // Open output file template
            FileInputStream fis = new FileInputStream(new File("gov/epa/otaq/moves/master/gui/avfttool/OutputTemplate.xlsx"));
            XSSFWorkbook workbook = new XSSFWorkbook(fis);
            fis.close();
 
            // load AVFT data to write
            db = manager.database.openConnection();
            query = new SQLRunner.Query();
            query.open(db, "SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, COALESCE(fuelEngFraction, 0) AS fuelEngFraction FROM outputAVFT");

            // Write each AVFT row
            XSSFSheet avftSheet = workbook.getSheet("AVFT");
            int avftRowCount = avftSheet.getLastRowNum();
            while(query.rs.next()) {
                int columnCount = 0;
                XSSFRow row = avftSheet.createRow(++avftRowCount);
                row.createCell(columnCount++).setCellValue(query.rs.getInt("sourceTypeID"));
                row.createCell(columnCount++).setCellValue(query.rs.getInt("modelYearID"));
                row.createCell(columnCount++).setCellValue(query.rs.getInt("fuelTypeID"));
                row.createCell(columnCount++).setCellValue(query.rs.getInt("engTechID"));
                row.createCell(columnCount++).setCellValue(query.rs.getDouble("fuelEngFraction")); 
            }
 
            // refresh formulas
            workbook.setForceFormulaRecalculation(true);

            // write messages to tab
            XSSFSheet messagesSheet = workbook.getSheet("Messages");
            int messagesRowCount = messagesSheet.getLastRowNum();
            for(Iterator<String> i=messages.iterator();i.hasNext();) {
                XSSFRow row = messagesSheet.createRow(++messagesRowCount);
                row.createCell(0).setCellValue(i.next());
            }  

            // write spec to tab
            AVFTToolSpec spec = getSpecFromGUISelections();
            AVFTToolSpecXML xmlWriter = new AVFTToolSpecXML(spec);
            xmlWriter.save(workbook.getSheet("InputXML"));

            // Write the workbook to the output file
            FileOutputStream fos = new FileOutputStream(outputFileFullPath);
            workbook.write(fos);
            fos.close();

            // If an output database is specified, save a copy of the output data there 
            if(!outputAVFTdb.equals("")) {
                SQLRunner.executeSQL(db, "CREATE DATABASE IF NOT EXISTS " + outputAVFTdb);
                SQLRunner.executeSQL(db, "CREATE TABLE IF NOT EXISTS " + outputAVFTdb + ".avft LIKE outputAVFT");
                SQLRunner.executeSQL(db, "TRUNCATE TABLE " + outputAVFTdb + ".avft");
                SQLRunner.executeSQL(db, "INSERT INTO " + outputAVFTdb + ".avft " + 
                                         "SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, " +
                                         "       COALESCE(fuelEngFraction, 0) AS fuelEngFraction FROM outputAVFT");
            }
 
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
        } finally {
            if (query != null) {
                query.onFinally();
            }
        }
    }

    /** Returns the file type code for use with the TableFileLinkagePart **/
	private int getFileTypeCode(String fileName) {
		fileName = StringUtilities.safeGetString(fileName);
		return CellFile.getFileType(fileName);
	}

    /** Returns the file type as a string for use with the TableFileLinkagePart **/
	private String getFileTypeString(String fileName) {
		switch(getFileTypeCode(fileName)) {
			case CellFile.XLS:
                return "XLS";
			case CellFile.XLSX:
                return "XLSX";
			case CellFile.CSV:
				return "CSV";
			default:
			case CellFile.TABBED_TEXT:
				return "Tabbed Text";
		}
	}

    /** Runs the avft importer for the given file/worksheet
     * @param sqlTableName: what should the avft importer's final table be called?
     * @param fileName: name of the file to load
     * @param worksheetName: the worksheet to load
     * @param db: a connection to the database that the importer is using
     * @param messages: the list of messages for communicating with the user
     * @param ignoreErrors: if true, ignore any errors encountered when importing the file (use this when expecting errors, like for the Known Fractions input)
     * @return whether or not any errors were encountered
    **/
    boolean doImport(String sqlTableName, String fileName, String worksheetName, Connection db, boolean ignoreErrors) {
        boolean hasErrors = false;

        // set up the importer to import the specified file
        fuelImporter.avftPart.fileName = fileName;
        fuelImporter.avftPart.fileTypeString = getFileTypeString(fileName);
        fuelImporter.avftPart.fileTypeCode = getFileTypeCode(fileName);
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
    
	/** Handle the Save AVFT Spec button **/
	void handleSaveAVFTToolSpecButton() {
        // get file destination
        FileDialog fd = new FileDialog(frame, "Specify save file name (*.xml):", FileDialog.SAVE);
        fd.setFile("*.xml");
        fd.setVisible(true);

        if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
            return;
        }
        setWaitCursor();

        try {
            String filePath = fd.getDirectory() + fd.getFile();
            AVFTToolSpec spec = getSpecFromGUISelections();

            try {
                PrintWriter writer = new PrintWriter(filePath);
                new AVFTToolSpecXML(spec).save(writer);  
                writer.close();
            } catch (IOException e) {
                System.out.println("An error occurred.");
                e.printStackTrace();
            }
        } finally {
            setDefaultCursor();
        }
    }
    
	/** Handle the Load AVFT Spec button **/
	void handleLoadAVFTToolSpecButton() {
        FileDialog fd = new FileDialog(frame, "Load Tool Settings", FileDialog.LOAD);
        fd.setVisible(true); //fd.show();
        if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
            return;
        }
        String filePath = fd.getDirectory() + fd.getFile();
        File file = new File(filePath);
        if(!file.exists()) {
            return;
        }

        AVFTToolSpec spec = new AVFTToolSpec();
		AVFTToolSpecXML newSpecXML = new AVFTToolSpecXML(spec);
        if (!newSpecXML.load(file)) {
            alertUser("Error reading " + filePath, "AVFT Tool", JOptionPane.ERROR_MESSAGE, false);
            return;
        }

        setGUISelectionsFromSpec(spec);
    }


	/** Handle the open button for the help file **/
	void handleOpenHelpButton() {
		try {
			File file = new File("gov/epa/otaq/moves/master/gui/avfttool/AVFTToolHelp.pdf");
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

    void setWaitCursor() {
		getRootPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
	}	
	
	void setDefaultCursor() {
		getRootPane().setCursor(Cursor.getDefaultCursor());
	}

    /**
	 * Returns an AVFTToolSpec built from the GUI selections
	**/
    AVFTToolSpec getSpecFromGUISelections() {
        // create and fill AVFTToolSpec object to do the writing
        AVFTToolSpec spec = new AVFTToolSpec();
        MethodEntry entry;
        spec.lastCompleteModelYear = lastCompleteMYCombo.getSelectedItem().toString();
        spec.analysisYear = analysisYearCombo.getSelectedItem().toString();
        spec.inputAVFTFile = new FileEntry(inputAVFTFilePath, inputAVFTFileWorkbook);
        spec.knownFractionsFile = new FileEntry(knownFractionsFilePath, knownFractionsFileWorkbook);
        spec.outputAVFTFile = new FileEntry(outputFileFullPath);
        entry = new MethodEntry(11, 
                                enable11Check.isSelected(), 
                                gapFillingMethod11Combo.getSelectedItem().toString(), 
                                projectionMethod11Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(21, 
                                enable21Check.isSelected(), 
                                gapFillingMethod21Combo.getSelectedItem().toString(), 
                                projectionMethod21Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(31, 
                                enable31Check.isSelected(), 
                                gapFillingMethod31Combo.getSelectedItem().toString(), 
                                projectionMethod31Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(32, 
                                enable32Check.isSelected(), 
                                gapFillingMethod32Combo.getSelectedItem().toString(), 
                                projectionMethod32Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(41, 
                                enable41Check.isSelected(), 
                                gapFillingMethod41Combo.getSelectedItem().toString(), 
                                projectionMethod41Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(42, 
                                enable42Check.isSelected(), 
                                gapFillingMethod42Combo.getSelectedItem().toString(), 
                                projectionMethod42Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(43, 
                                enable43Check.isSelected(), 
                                gapFillingMethod43Combo.getSelectedItem().toString(), 
                                projectionMethod43Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(51, 
                                enable51Check.isSelected(), 
                                gapFillingMethod51Combo.getSelectedItem().toString(), 
                                projectionMethod51Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(52, 
                                enable52Check.isSelected(), 
                                gapFillingMethod52Combo.getSelectedItem().toString(), 
                                projectionMethod52Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(53, 
                                enable53Check.isSelected(), 
                                gapFillingMethod53Combo.getSelectedItem().toString(), 
                                projectionMethod53Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(54, 
                                enable54Check.isSelected(), 
                                gapFillingMethod54Combo.getSelectedItem().toString(), 
                                projectionMethod54Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(61, 
                                enable61Check.isSelected(), 
                                gapFillingMethod61Combo.getSelectedItem().toString(), 
                                projectionMethod61Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);
        entry = new MethodEntry(62, 
                                enable62Check.isSelected(), 
                                gapFillingMethod62Combo.getSelectedItem().toString(), 
                                projectionMethod62Combo.getSelectedItem().toString());
        spec.methodEntries.add(entry);

        return spec;
    }

   /**
	 * Loads a specification file and prepares the tool for running
	**/
    public void setGUISelectionsFromSpec(AVFTToolSpec spec) {
        // years
        lastCompleteMYCombo.setSelectedItem(spec.lastCompleteModelYear); // does this work, or do i need to look up the index?
        analysisYearCombo.setSelectedItem(spec.analysisYear); // does this work, or do i need to look up the index?

        // files
        inputAVFTFilePath = spec.inputAVFTFile.filePath;
        inputAVFTFileWorkbook = spec.inputAVFTFile.tabName;
        updateInputFileGUI();
        
        knownFractionsFilePath = spec.knownFractionsFile.filePath;
        knownFractionsFileWorkbook = spec.knownFractionsFile.tabName;
        updateKnownFractionsFileGUI();

        outputFileFullPath = spec.outputAVFTFile.filePath;
        updateOutputFileGUI();

        for (MethodEntry methodEntry : spec.methodEntries) {
            switch (methodEntry.sourceTypeID) {
                case 11:
                // need to make sure boxes get checked when enable11Check status is different than isEnabled
                    if (methodEntry.enabled != enable11Check.isSelected()) { enable11Check.doClick(0); }
                    gapFillingMethod11Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod11Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 21:
                    if (methodEntry.enabled != enable21Check.isSelected()) { enable21Check.doClick(0); }
                    gapFillingMethod21Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod21Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 31:
                    if (methodEntry.enabled != enable31Check.isSelected()) { enable31Check.doClick(0); }
                    gapFillingMethod31Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod31Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 32:
                    if (methodEntry.enabled != enable32Check.isSelected()) { enable32Check.doClick(0); }
                    gapFillingMethod32Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod32Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 41:
                    if (methodEntry.enabled != enable41Check.isSelected()) { enable41Check.doClick(0); }
                    gapFillingMethod41Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod41Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 42:
                    if (methodEntry.enabled != enable42Check.isSelected()) { enable42Check.doClick(0); }
                    gapFillingMethod42Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod42Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 43:
                    if (methodEntry.enabled != enable43Check.isSelected()) { enable43Check.doClick(0); }
                    gapFillingMethod43Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod43Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 51:
                    if (methodEntry.enabled != enable51Check.isSelected()) { enable51Check.doClick(0); }
                    gapFillingMethod51Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod51Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 52:
                    if (methodEntry.enabled != enable52Check.isSelected()) { enable52Check.doClick(0); }
                    gapFillingMethod52Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod52Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 53:
                    if (methodEntry.enabled != enable53Check.isSelected()) { enable53Check.doClick(0); }
                    gapFillingMethod53Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod53Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 54:
                    if (methodEntry.enabled != enable54Check.isSelected()) { enable54Check.doClick(0); }
                    gapFillingMethod54Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod54Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 61:
                    if (methodEntry.enabled != enable61Check.isSelected()) { enable61Check.doClick(0); }
                    gapFillingMethod61Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod61Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
                case 62:
                    if (methodEntry.enabled != enable62Check.isSelected()) { enable62Check.doClick(0); }
                    gapFillingMethod62Combo.setSelectedItem(methodEntry.gapFillingMethod);
                    projectionMethod62Combo.setSelectedItem(methodEntry.projectionMethod);
                    break;
            }
        }
    }
    
   /**
	 * Sets the inputAVFTdb name
	**/
    public void setInputAVFTdb(String inputAVFTdb) {
        this.inputAVFTdb = inputAVFTdb;
    }
    
    /**
      * Sets the outputAVFTdb name
     **/
     public void setOutputAVFTdb(String outputAVFTdb) {
         this.outputAVFTdb = outputAVFTdb;
     }

    /** displays a message to the user with a popup if running from the GUI, otherwise print the message to the console **/
    public void alertUser(String alert, String title, int messageType, boolean includeMessagesIfCMD) {
        if (frame == null) {
            String prefix = (messageType == 0) ? "ERROR: " : "";
            System.err.println(prefix + title + ": " + alert);
            if (includeMessagesIfCMD && !messages.isEmpty()) {
                for (String m : messages) {
                    System.err.println(m);
                }
            }
        } else {
            JOptionPane.showMessageDialog(frame, alert, title, messageType);
        }
    }
}
