/**************************************************************************************************
 * @(#)AVFTToolKnownFractionsTemplate.java
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
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.importers.ImporterManager;
import gov.epa.otaq.moves.master.framework.importers.TableFileLinkagePart;
import gov.epa.otaq.moves.master.gui.MOVESWindow;
import gov.epa.otaq.moves.master.implementation.importers.FuelImporter;

/**
 * Displays the AVFT Tool's "Known Fractions Template" Popup and creates the template
 * @version 	2023-02-28
**/
public class KnownFractionsTemplate extends JDialog implements ActionListener, FocusListener {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;

    /** What source types should be displayed in this popup **/
    ArrayList<Integer> sourceTypesToShow;

    /** holds all checkboxes displayed **/
    KnownFractionsTemplateCheckBox[] checkboxes;

    /** stores how many checkboxes there are **/
    int numCheckBoxes = 0;

    /** stores other GUI parameters **/
    int lastCompleteMY;
    int analysisYear;

	/** Button to create the input template file **/
	JButton createTemplateButton;

    /** instructions for this dialog **/
    JLabel instructions;
	
	/** Button to close the window **/
	JButton cancelButton;

	/**
	 * Browser dialog for selecting an output directory.  Retained as a member
	 * variable so it would retain the last used directory.
	**/
	JFileChooser folderChooser = null;

	/**
	 * Constructs the FERC panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	 * @param modeToUse Default conversion mode
	**/
	public KnownFractionsTemplate(JFrame parent, ArrayList<Integer> sourceTypes, int guiLastCompleteMY, int guiAnalysisYear) {
		super(parent, MOVESWindow.MOVES_VERSION + " - Create Known Fractions Template");
		frame = parent;
        sourceTypesToShow = sourceTypes;
        checkboxes = new KnownFractionsTemplateCheckBox[sourceTypes.size()*6]; // 6 fuel types
        lastCompleteMY = guiLastCompleteMY;
        analysisYear = guiAnalysisYear;

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(createPanel(), BorderLayout.CENTER);
		pack();
		setResizable(false);
        setSize(620, 140 + 30*sourceTypesToShow.size()); // TODO
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		pack();
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
		instructions = new JLabel("<html>This dialog shows the source types for which you indicated you have known fractions.<br>" + 
                                        "Please select the specific known source type and fuel type combinations here:" +
                                        "<br>&nbsp;</html>", JLabel.RIGHT);
      
        // buttons
        createTemplateButton = new JButton(); 
		createTemplateButton.addActionListener(this);
        createTemplateButton.setText("Create Template...");
        createTemplateButton.setMnemonic('T');
		cancelButton = new JButton();
		cancelButton.addActionListener(this);
        cancelButton.setText("Cancel");
        cancelButton.setMnemonic('C');

        // create database connection
        DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
        Connection db = defaultDatabase.openConnectionOrNull();
        if (db == null) {
            Logger.log(LogMessageCategory.ERROR, "Error in trying to load the known fractions template GUI. " + 
                "Could not connect to the default database " + defaultDatabase.databaseName);
            dispose();
        }

        try {
            String stNameSQL = "SELECT sourceTypeName FROM sourceusetype WHERE sourceTypeID = %d";
            String stFtExistsSQL = "SELECT COALESCE(SUM(stmyFuelEngFraction), 0) as `exists` FROM samplevehiclepopulation " + 
                                   "WHERE sourceTypeID = %d AND fuelTypeID = %d AND engTechID = %d";

            // ------------ arrange controls ------------
            panel.setLayout(new GridBagLayout());
            ((GridBagLayout)panel.getLayout()).columnWidths = new int[] {0};
            ((GridBagLayout)panel.getLayout()).rowHeights = new int[] {0, 0, 0, 0};
            ((GridBagLayout)panel.getLayout()).columnWeights = new double[] {0.0};
            ((GridBagLayout)panel.getLayout()).rowWeights = new double[] {0.0, 0.0, 0.0, 0.0};

            // header
            {
                panel.add(instructions, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
                    GridBagConstraints.WEST, GridBagConstraints.NONE,
                    new Insets(2, 5, 2, 5   ), 0, 0));
            }

            // checkboxes
            {
                JPanel cbInset = new JPanel();
                //inputsInset.setBorder(BorderFactory.createTitledBorder("Tool Input Selections"));
                cbInset.setLayout(new GridBagLayout());
                ((GridBagLayout)cbInset.getLayout()).columnWidths = new int[] {180, 65, 50, 50, 50, 50, 50};
                ((GridBagLayout)cbInset.getLayout()).rowHeights = new int[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
                ((GridBagLayout)cbInset.getLayout()).columnWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
                ((GridBagLayout)cbInset.getLayout()).rowWeights = new double[] {1.0E-4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
                
                // header row
                int r = 0;
                int c = 0;
                cbInset.add(new JLabel("<html><u>Source Type</u></html>"), new GridBagConstraints(c, r, 1, 1, 0.0, 0.0,
                    GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                cbInset.add(new JLabel("<html><u>Gasoline</u></html>"), new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                cbInset.add(new JLabel("<html><u>Diesel</u></html>"), new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                cbInset.add(new JLabel("<html><u>CNG</u></html>"), new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                cbInset.add(new JLabel("<html><u>E-85</u></html>"), new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                cbInset.add(new JLabel("<html><u>BEV</u></html>"), new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                cbInset.add(new JLabel("<html><u>FCEV</u></html>"), new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.NONE  , new Insets(0, 0, 2, 3), 0, 0));
                
                // build grid for each source type
                for (int sourceTypeID : sourceTypesToShow) {
                    ++r;
                    c = 0;
                    String sourceTypeName = SQLRunner.executeScalarString(db, String.format(stNameSQL, sourceTypeID));
                    cbInset.add(new JLabel(sourceTypeName), new GridBagConstraints(c, r, 1, 1, 0.0, 0.0,
                        GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                    // gasoline
                    checkboxes[numCheckBoxes] = new KnownFractionsTemplateCheckBox(sourceTypeID, 1, 1);
                    checkboxes[numCheckBoxes].addFocusListener(this);
                    checkboxes[numCheckBoxes].setEnabled((SQLRunner.executeScalar(db, String.format(stFtExistsSQL, sourceTypeID, 1, 1)) > 0.0));
                    ToolTipHelper.add(checkboxes[numCheckBoxes], "Gasoline " + sourceTypeName + "s" + (checkboxes[numCheckBoxes].isEnabled() ? "" : " are not modeled in MOVES"));
                    cbInset.add(checkboxes[numCheckBoxes], new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                        GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                    ++numCheckBoxes;
                    // diesel
                    checkboxes[numCheckBoxes] = new KnownFractionsTemplateCheckBox(sourceTypeID, 2, 1);
                    checkboxes[numCheckBoxes].addFocusListener(this);
                    checkboxes[numCheckBoxes].setEnabled((SQLRunner.executeScalar(db, String.format(stFtExistsSQL, sourceTypeID, 2, 1)) > 0.0));
                    ToolTipHelper.add(checkboxes[numCheckBoxes], "Diesel " + sourceTypeName + "s" + (checkboxes[numCheckBoxes].isEnabled() ? "" : " are not modeled in MOVES"));
                    cbInset.add(checkboxes[numCheckBoxes], new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                        GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                    ++numCheckBoxes;
                    // cng
                    checkboxes[numCheckBoxes] = new KnownFractionsTemplateCheckBox(sourceTypeID, 3, 1);
                    checkboxes[numCheckBoxes].addFocusListener(this);
                    checkboxes[numCheckBoxes].setEnabled((SQLRunner.executeScalar(db, String.format(stFtExistsSQL, sourceTypeID, 3, 1)) > 0.0));
                    ToolTipHelper.add(checkboxes[numCheckBoxes], "Compressed Natural Gas " + sourceTypeName + "s" + (checkboxes[numCheckBoxes].isEnabled() ? "" : " are not modeled in MOVES"));
                    cbInset.add(checkboxes[numCheckBoxes], new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                        GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                    ++numCheckBoxes;
                    // e-85
                    checkboxes[numCheckBoxes] = new KnownFractionsTemplateCheckBox(sourceTypeID, 5, 1);
                    checkboxes[numCheckBoxes].addFocusListener(this);
                    checkboxes[numCheckBoxes].setEnabled((SQLRunner.executeScalar(db, String.format(stFtExistsSQL, sourceTypeID, 5, 1)) > 0.0));
                    ToolTipHelper.add(checkboxes[numCheckBoxes], "E-85 " + sourceTypeName + "s" + (checkboxes[numCheckBoxes].isEnabled() ? "" : " are not modeled in MOVES"));
                    cbInset.add(checkboxes[numCheckBoxes], new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                        GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                    ++numCheckBoxes;
                    // bev
                    checkboxes[numCheckBoxes] = new KnownFractionsTemplateCheckBox(sourceTypeID, 9, 30);
                    checkboxes[numCheckBoxes].addFocusListener(this);
                    checkboxes[numCheckBoxes].setEnabled((SQLRunner.executeScalar(db, String.format(stFtExistsSQL, sourceTypeID, 9, 30)) > 0.0));
                    ToolTipHelper.add(checkboxes[numCheckBoxes], "Battery Electric " + sourceTypeName + "s" + (checkboxes[numCheckBoxes].isEnabled() ? "" : " are not modeled in MOVES"));
                    cbInset.add(checkboxes[numCheckBoxes], new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                        GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                    ++numCheckBoxes;
                    // fcev
                    checkboxes[numCheckBoxes] = new KnownFractionsTemplateCheckBox(sourceTypeID, 9, 40);
                    checkboxes[numCheckBoxes].addFocusListener(this);
                    checkboxes[numCheckBoxes].setEnabled((SQLRunner.executeScalar(db, String.format(stFtExistsSQL, sourceTypeID, 9, 40)) > 0.0));
                    ToolTipHelper.add(checkboxes[numCheckBoxes], "Fuel Cell " + sourceTypeName + "s" + (checkboxes[numCheckBoxes].isEnabled() ? "" : " are not modeled in MOVES"));
                    cbInset.add(checkboxes[numCheckBoxes], new GridBagConstraints(++c, r, 1, 1, 0.0, 0.0,
                        GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 3), 0, 0));
                    ++numCheckBoxes;
                }

                panel.add(cbInset, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0,
                GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 2, 0), 0, 0));
            }

            // spacer
            {
                panel.add(new JLabel(""), new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0,
                    GridBagConstraints.WEST, GridBagConstraints.NONE,
                    new Insets(2, 5, 2, 5   ), 0, 0));
            }

            // buttons
            {
                JPanel buttonInset = new JPanel();
                //inputsInset.setBorder(BorderFactory.createTitledBorder("Tool Input Selections"));
                buttonInset.setLayout(new GridBagLayout());
                ((GridBagLayout)buttonInset.getLayout()).columnWidths = new int[] {0, 0};
                ((GridBagLayout)buttonInset.getLayout()).rowHeights = new int[] {0};
                ((GridBagLayout)buttonInset.getLayout()).columnWeights = new double[] {0.0, 0.0};
                ((GridBagLayout)buttonInset.getLayout()).rowWeights = new double[] {0.0};
                
                buttonInset.add(cancelButton, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0,
                    GridBagConstraints.WEST, GridBagConstraints.NONE,
                    new Insets(0, 0, 2, 5), 0, 0));

                ToolTipHelper.add(createTemplateButton,"Create a known fractions template file (.xlsx/.xls)");
                buttonInset.add(createTemplateButton, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0,
                    GridBagConstraints.EAST, GridBagConstraints.NONE,
                    new Insets(0, 0, 2, 0), 0, 0));

                panel.add(buttonInset, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0,
                    GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 2, 0), 0, 0));
            }
        } catch (SQLException e) {
            Logger.logError(e, "Error trying to load the known fractions template GUI");
        dispose();
        } finally {
            DatabaseUtilities.closeConnection(db);
        }

		return panel;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
        if(e.getSource() == cancelButton) {
            dispose();
        } else if(e.getSource() == createTemplateButton) {
            handleCreateTemplateButton();
        }
	}

    public void focusGained(FocusEvent e) {
        ((JComponent)e.getSource()).setBackground(new Color(163,184,204));
    }

    public void focusLost(FocusEvent e) {
        ((JComponent)e.getSource()).setBackground(getBackground());
    }

    /** Handle the Create Template button **/
	void handleCreateTemplateButton() {
        // check for no selections made first
        boolean noSelections = true;
        for (KnownFractionsTemplateCheckBox cb : checkboxes) {
            if (cb.isSelected()) {
                noSelections = false;
                break;
            }
        }
        if (noSelections) {
            JOptionPane.showMessageDialog(frame, "No selections made.","AVFT Tool", JOptionPane.ERROR_MESSAGE);
            return;
        }

		try {
            // show dialog
			FileDialog fd = new FileDialog(frame, "Save File As... (*.xlsx/*.xls)", FileDialog.SAVE);
			fd.setVisible(true);
			if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
				return;
			}

            // get the selected path
			String filePath = fd.getDirectory() + fd.getFile();
            
            // ensure file ends with .xlsx or .xls
            if (!(filePath.toLowerCase().endsWith(".xlsx") || filePath.toLowerCase().endsWith(".xls"))) {
                JOptionPane.showMessageDialog(frame, "The template must have the *.xlsx or *.xls extension. Automatically changing to *.xlsx.",
                                        "AVFT Tool", JOptionPane.WARNING_MESSAGE);
                filePath = filePath + ".xlsx";
            }
			File file = new File(filePath);
			boolean success = writeOutput(file.getCanonicalPath());
            if (success) {
                dispose();
            }            
		} catch(Exception e) {
			// Nothing to do here
		}
	}

    /** Saves the output from the AVFTTool (held in the interim database's outputAVFT table) in a 
     *  copy of the OutputTemplate, which plots the results for easy QA. **/
    boolean writeOutput(String outputFileFullPath) {
        Connection db = null;
        SQLRunner.Query query = null;
        HashSet<Integer> uniqueFuelTypes = new HashSet<Integer>(); // stores unique fuel types seen so we can include them in the cheat sheet
        HashSet<Integer> uniqueEngTechs = new HashSet<Integer>(); // stores unqiue engtechs seen so we can include them in the cheat sheet
        // note, don't need to track source types here, because sourceTypesToShow already contains what we need

        try {
            // Create the output workbook
            XSSFWorkbook workbook = new XSSFWorkbook();
 
            // create connection
            DatabaseSelection defaultDatabase = SystemConfiguration.getTheSystemConfiguration().databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()];
            db = defaultDatabase.openConnectionOrNull();
            if (db == null) {
                Logger.log(LogMessageCategory.ERROR, "Error in trying to create the known fractions template. " + 
                    "Could not connect to the default database " + defaultDatabase.databaseName);
                return false;
            }
            query = new SQLRunner.Query();

            // write AVFT table
            {
                XSSFSheet avftSheet = workbook.createSheet("AVFT");
                int rowCount = 0;
                int columnCount = 0;

                // write header
                XSSFRow row = avftSheet.createRow(rowCount++);
                row.createCell(columnCount++).setCellValue("sourceTypeID");
                row.createCell(columnCount++).setCellValue("modelYearID");
                row.createCell(columnCount++).setCellValue("fuelTypeID");
                row.createCell(columnCount++).setCellValue("engTechID");
                row.createCell(columnCount++).setCellValue("fuelEngFraction");

                // build query
                String sql = "SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID " + 
                            "FROM sourceusetype, fueltype, enginetech, modelyear " +
                            "WHERE 0=1 ";
                for (KnownFractionsTemplateCheckBox cb : checkboxes) {
                    if (cb.isSelected()) {
                        sql += String.format("OR (sourceTypeID = %d AND fuelTypeID = %d AND engTechID = %d AND modelYearID BETWEEN %d AND %d) ",
                                            cb.sourceTypeID, cb.fuelTypeID, cb.engTechID, lastCompleteMY+1, analysisYear);
                        uniqueFuelTypes.add(Integer.valueOf(cb.fuelTypeID));
                        uniqueEngTechs.add(Integer.valueOf(cb.engTechID));
                    }
                }
                sql += "ORDER BY sourceTypeID, modelYearID, fuelTypeID, engTechID";

                // write body
                query.open(db, sql);
                while(query.rs.next()) {
                    columnCount = 0;
                    row = avftSheet.createRow(rowCount++);
                    row.createCell(columnCount++).setCellValue(query.rs.getInt("sourceTypeID"));
                    row.createCell(columnCount++).setCellValue(query.rs.getInt("modelYearID"));
                    row.createCell(columnCount++).setCellValue(query.rs.getInt("fuelTypeID"));
                    row.createCell(columnCount++).setCellValue(query.rs.getInt("engTechID"));
                }
            }

            // write engTech cheat sheet
            {
                XSSFSheet engTechSheet = workbook.createSheet("EngTech");
                int rowCount = 0;
                int columnCount = 0;

                // write header
                XSSFRow row = engTechSheet.createRow(rowCount++);
                row.createCell(columnCount++).setCellValue("engTechID");
                row.createCell(columnCount++).setCellValue("engTechName");

                // build query
                String sql = "SELECT engTechID, " +
                             "       CASE WHEN engTechName = 'Conventional Internal Combustion' " +
                             "             THEN 'Internal Combustion' " +
                             "            ELSE engTechName " +
                             "        END AS engTechName " +
                             "FROM engineTech " +
                             "WHERE engTechID IN " + uniqueEngTechs.toString().replace('[', '(').replace(']', ')') + " " +
                             "ORDER BY engTechID;";

                // write body
                query.open(db, sql);
                while(query.rs.next()) {
                    columnCount = 0;
                    row = engTechSheet.createRow(rowCount++);
                    row.createCell(columnCount++).setCellValue(query.rs.getInt("engTechID"));
                    row.createCell(columnCount++).setCellValue(query.rs.getString("engTechName")); 
                }
            }

            // write fuel type cheat sheet
            {
                XSSFSheet fuelTypeSheet = workbook.createSheet("FuelType");
                int rowCount = 0;
                int columnCount = 0;

                // write header
                XSSFRow row = fuelTypeSheet.createRow(rowCount++);
                row.createCell(columnCount++).setCellValue("fuelTypeID");
                row.createCell(columnCount++).setCellValue("fuelTypeDesc");
                row.createCell(columnCount++).setCellValue("fuelDensity");

                // build query
                String sql = "SELECT fuelTypeID, fuelTypeDesc, fuelDensity FROM fueltype " +
                             "WHERE fuelTypeID IN " + uniqueFuelTypes.toString().replace('[', '(').replace(']', ')') + " " +
                             "ORDER BY fuelTypeID;";

                // write body
                query.open(db, sql);
                while(query.rs.next()) {
                    columnCount = 0;
                    row = fuelTypeSheet.createRow(rowCount++);
                    row.createCell(columnCount++).setCellValue(query.rs.getInt("fuelTypeID"));
                    row.createCell(columnCount++).setCellValue(query.rs.getString("fuelTypeDesc")); 
                    row.createCell(columnCount++).setCellValue(query.rs.getString("fuelDensity"));
                }
            }

            // write source type cheat sheet
            {
                XSSFSheet sourceTypeSheet = workbook.createSheet("SourceUseType");
                int rowCount = 0;
                int columnCount = 0;

                // write header
                XSSFRow row = sourceTypeSheet.createRow(rowCount++);
                row.createCell(columnCount++).setCellValue("sourceTypeID");
                row.createCell(columnCount++).setCellValue("sourceTypeName");
                row.createCell(columnCount++).setCellValue("HPMSVtypeID");
                row.createCell(columnCount++).setCellValue("HPMSVtypeName");

                // build query
                String sql = "SELECT sourceTypeID, sourceTypeName, HPMSVtypeID, HPMSVtypeName " +
                             "FROM sourceusetype JOIN hpmsvtype USING (hpmsVTypeID) " +
                             "WHERE sourceTypeID IN " + sourceTypesToShow.toString().replace('[', '(').replace(']', ')') + " " +
                             "ORDER BY sourceTypeID;";

                // write body
                query.open(db, sql);
                while(query.rs.next()) {
                    columnCount = 0;
                    row = sourceTypeSheet.createRow(rowCount++);
                    row.createCell(columnCount++).setCellValue(query.rs.getInt("sourceTypeID"));
                    row.createCell(columnCount++).setCellValue(query.rs.getString("sourceTypeName")); 
                    row.createCell(columnCount++).setCellValue(query.rs.getInt("HPMSVtypeID"));
                    row.createCell(columnCount++).setCellValue(query.rs.getString("HPMSVtypeName"));
                }
            }

            // Write the workbook to the output file
            workbook.setSelectedTab(0);
            FileOutputStream fos = new FileOutputStream(outputFileFullPath);
            workbook.write(fos);
            fos.close();
 
        } catch (IOException e) {
            if (e.getMessage().contains("The process cannot access the file because it is being used by another process")) {
                JOptionPane.showMessageDialog(frame, "Error: Cannot access the output file because it is being used by another process.\n\nPlease close the file and try again.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return false;
            } else {
                Logger.log(LogMessageCategory.ERROR, e.toString());
                JOptionPane.showMessageDialog(frame, "Error: Could not save the the output. More details are in the MOVES log.",
                                        "AVFT Tool", JOptionPane.ERROR_MESSAGE);
                return false;
            }
        } catch (SQLException e) {
            Logger.log(LogMessageCategory.ERROR, e.toString());
            JOptionPane.showMessageDialog(frame, "Error: Could not save the output. More details are in the MOVES log.",
                "AVFT Tool", JOptionPane.ERROR_MESSAGE);
            return false;
        } finally {
            if (query != null) {
                query.onFinally();
            }
        }
        return true;
    }

}
