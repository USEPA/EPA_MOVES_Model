/**************************************************************************************************
 * @(#)StateCountyMapGUI.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.gis.api;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.sql.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.io.*;
import java.util.*;
import java.text.*;

import gov.epa.otaq.moves.common.ExtendedComboBox;
import gov.epa.otaq.moves.common.WindowStateHandler;

/**
 * GUI for creating and displaying State and County maps
 *
 * @author		Wesley Faler
 * @author		Tim Hull
 * @version		2011-09-10
 * Changed to use new ExtendedComboBox
**/
public class StateCountyMapGUI extends JDialog implements ActionListener /*, ListSelectionListener */ {
	/** The parent JFrame which invokes this dialog. **/
	JFrame frame;
	/** Database connection **/
	Connection db;
	/** Name of the database **/
	String databaseName;

	/** Panel for the Database options **/
	JPanel dbPanel;
	/** "Cancel" button on dbPanel **/
	JButton dbCancelButton;
	/** "Next" button on dbPanel **/
	JButton dbNextButton;
	/** Table listing on dbPanel **/
	ExtendedComboBox<String> dbTableList;
	/** FIPS column listing on dbPanel **/
	ExtendedComboBox<String> dbFIPSList;
	/** Value column listing on dbPanel **/
	ExtendedComboBox<String> dbValueList;

	/** Panel for the Rendering options **/
	JPanel renderPanel;
	/** "Cancel" button on renderPanel **/
	JButton renderCancelButton;
	/** "Back" button on renderPanel **/
	JButton renderBackButton;
	/** "Next" button on renderPanel **/
	JButton renderNextButton;
	/** Number of color bins on renderPanel **/
	JTextField renderBinCount;
	/** Minimum data value on renderPanel **/
	JTextField renderMinValue;
	/** Maximum data value on renderPanel **/
	JTextField renderMaxValue;
	/** Show Legend Colors checkbox on renderPanel **/
	JCheckBox renderShowColors;
	/** Show Legend text checkbox on renderPanel **/
	JCheckBox renderShowText;
	/** Show Labels checkbox on renderPanel **/
	JCheckBox renderShowLabels;
	/** Color Selection on renderPanel **/
	ExtendedComboBox<String> renderColorList;

	/** Panel for the Map display **/
	JPanel mapPanel;
	/** "Done" button on mapPanel **/
	JButton mapDoneButton;
	/** "Back" button on mapPanel **/
	JButton mapBackButton;
	/** "SaveJPG" button on mapPanel **/
	JButton mapSaveJPGButton;
	/** "Print" button on mapPanel **/
	JButton mapPrintButton;
	/** Image display on mapPanel **/
	ImageUtility.ImagePanel mapImageDisplay;
	/** Buttons panel on mapPanel **/
	JPanel mapButtonsPanel;

	/** Map renderer **/
	StateCountyMapRenderer renderer = null;
	/** Scale **/
	Scale legend = null;
	/** Map Image **/
	BufferedImage image = null;

	/**
	 * Constructs the main panel, also creates and sets the layouts of the controls.
	 * @param parent the parent frame to use for the panel.
	**/
	public StateCountyMapGUI(JFrame parent, Connection dbToUse, String databaseNameToUse) {
		super(parent, "Generate State/County Map");
		frame = parent;
		db = dbToUse;
		databaseName = databaseNameToUse;

		getContentPane().setLayout(new BorderLayout());
		createPanels();
		getContentPane().add(dbPanel, BorderLayout.CENTER);
		pack();
		setResizable(true);
	}

	/** Allows the parent to display this dialog as modal. **/
	public void showModal() {
		assessSituation();
		setSize(820,600);
		pack();
		setModal(true);
		(new WindowStateHandler(this)).setPositionAndStartTracking();
		setVisible(true); //show();
	}

	/** Create all controls and panels **/
	void createPanels() {
		createDbPanel();
		createRenderPanel();
		createMapPanel();
	}

	/** Create the dbPanel and its child controls **/
	void createDbPanel() {
		dbPanel = new JPanel();

		// Create controls
		dbCancelButton = createButton("Cancel");
		dbNextButton = createButton("Next >");
		dbTableList = createComboBox("dbTableList",false);
		dbFIPSList = createComboBox("dbFIPSList",false);
		dbValueList = createComboBox("dbValueList",false);
		JLabel dbNameLabel = new JLabel("Database: " + databaseName,SwingConstants.LEFT);
		JLabel tableLabel = new JLabel("Table:",SwingConstants.LEFT);
		JLabel fipsLabel = new JLabel("State or County FIPS Column:",SwingConstants.LEFT);
		JLabel dataLabel = new JLabel("Data Column:",SwingConstants.LEFT);

		// Get list of tables
		dbTableList.addItem("");
		String sql = "SHOW TABLES FROM " + databaseName;
		PreparedStatement statement = null;
		ResultSet rs = null;
		try {
			statement = db.prepareStatement(sql);
			rs = statement.executeQuery();
			while(rs.next()) {
				String t = rs.getString(1);
				dbTableList.addItem(t);
			}
		} catch(Exception e) {
			System.out.println("Unable to load tables in StateCountyMapGUI using " + sql);
			System.out.println(e);
			e.printStackTrace();
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}

		// Arrange controls
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel,BoxLayout.X_AXIS));
		buttonPanel.add(dbCancelButton);
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(dbNextButton);

		dbPanel.setLayout(new BoxLayout(dbPanel,BoxLayout.Y_AXIS));
		dbPanel.add(createRow1(dbNameLabel));
		dbPanel.add(createRow1(tableLabel));
		dbPanel.add(createRow1(dbTableList));
		dbPanel.add(createRow1(fipsLabel));
		dbPanel.add(createRow1(dbFIPSList));
		dbPanel.add(createRow1(dataLabel));
		dbPanel.add(createRow1(dbValueList));
		dbPanel.add(buttonPanel);
	}

	/** Create the renderPanel and its child controls **/
	void createRenderPanel() {
		renderPanel = new JPanel();

		// Create controls
		renderCancelButton = createButton("Cancel");
		renderBackButton = createButton("< Back");
		renderNextButton = createButton("Next >");
		renderColorList = createComboBox("renderColorList",false);
		renderBinCount = createTextField("renderBinCount",5);
		renderMinValue = createTextField("renderMinValue",10);
		renderMaxValue = createTextField("renderMaxValue",10);
		renderShowColors = new JCheckBox("Show legend colors");
		renderShowColors.addActionListener(this);
		renderShowText = new JCheckBox("Show legend text");
		renderShowText.addActionListener(this);
		renderShowLabels = new JCheckBox("Show FIPS codes");
		renderShowLabels.addActionListener(this);
		JLabel colorLabel = new JLabel("Colors:",SwingConstants.LEFT);
		JLabel binCountLabel = new JLabel("Number of colors:",SwingConstants.LEFT);
		JLabel minLabel = new JLabel("Minimum Data Value:",SwingConstants.LEFT);
		JLabel maxLabel = new JLabel("Maximum Data Value:",SwingConstants.LEFT);

		renderColorList.addItem("Full Color");
		renderColorList.addItem("Grayscale");
		renderColorList.setSelectedItem("Full Color");

		renderShowColors.setSelected(true);
		renderShowText.setSelected(true);
		renderShowLabels.setSelected(true);
		renderBinCount.setText("16");

		// Arrange controls
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel,BoxLayout.X_AXIS));
		buttonPanel.add(renderBackButton);
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(renderCancelButton);
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(renderNextButton);

		renderPanel.setLayout(new BoxLayout(renderPanel,BoxLayout.Y_AXIS));
		renderPanel.add(createRow1(colorLabel));
		renderPanel.add(createRow1(renderColorList));
		renderPanel.add(createRow1(renderShowLabels));
		renderPanel.add(createRow1(renderShowColors));
		renderPanel.add(createRow1(renderShowText));
		renderPanel.add(createRow2(binCountLabel,renderBinCount));
		renderPanel.add(createRow2(minLabel,renderMinValue));
		renderPanel.add(createRow2(maxLabel,renderMaxValue));
		renderPanel.add(buttonPanel);
	}

	/** Create the mapPanel and its child controls **/
	void createMapPanel() {
		mapPanel = new JPanel();

		// Create controls
		mapBackButton = createButton("< Back");
		mapSaveJPGButton = createButton("Save JPG...");
		mapPrintButton = createButton("Print");
		mapDoneButton = createButton("Done");

		// Arrange controls
		mapButtonsPanel = new JPanel();
		mapButtonsPanel.setLayout(new BoxLayout(mapButtonsPanel,BoxLayout.X_AXIS));
		mapButtonsPanel.add(mapBackButton);
		mapButtonsPanel.add(Box.createHorizontalGlue());
		mapButtonsPanel.add(mapSaveJPGButton);
		mapButtonsPanel.add(Box.createHorizontalGlue());
		mapButtonsPanel.add(mapPrintButton);
		mapButtonsPanel.add(Box.createHorizontalGlue());
		mapButtonsPanel.add(mapDoneButton);

		mapPanel.setLayout(new BoxLayout(mapPanel,BoxLayout.Y_AXIS));
		// The image and supported scroll area will be added by applyImageToMapPanel()
		mapPanel.add(mapButtonsPanel);
	}

	/** Finish the mapPanel by using the current image **/
	void applyImageToMapPanel() {
		if(mapImageDisplay == null) {
			mapImageDisplay = new ImageUtility.ImagePanel(image);
			mapPanel.removeAll();
			JScrollPane scroller = new JScrollPane();
			scroller.add(mapImageDisplay);
	        scroller.setViewportView(mapImageDisplay);
	        scroller.setPreferredSize(new Dimension(700,525));
	        mapPanel.add(scroller);
			mapPanel.add(mapButtonsPanel);
		} else {
			mapImageDisplay.image = image;
		}
	}

	/**
	 * Utility routine to create a button
	 * @param text text to show on the button
	 * @return a new button already connected with this as the ActionListener
	**/
	JButton createButton(String text) {
		JButton result = new JButton(text);
		result.addActionListener(this);
		return result;
	}

	/**
	 * Utility routine to create a combobox
	 * @param controlName Value passed to JComboBox.setName, used for automated tests
	 * @param isEditable true if the text in the combobox can be edited, false for a
	 * dropdown list only
	 * @return a new combobox already connected with this as the listener
	**/
	ExtendedComboBox<String> createComboBox(String controlName, boolean isEditable) {
		ExtendedComboBox<String> result = new ExtendedComboBox<String>();
		Dimension d = result.getPreferredSize();
		result.setPreferredSize(new Dimension(150, d.height));
		result.setPopupWidth(result.getPreferredSize().width);
		result.setName(controlName);
		result.addActionListener(this);
		result.setEditable(isEditable);
		result.setSelectedIndex(-1);
		return result;
	}

	/**
	 * Utility routine to create a text field
	 * @param controlName Value passed to JTextField.setName, used for automated tests
	 * @param numberOfCharacters number of characters to display
	 * @return a new text field
	**/
	JTextField createTextField(String controlName, int numberOfCharacters) {
		JTextField result = new JTextField(numberOfCharacters);
		result.setName(controlName);
		result.setColumns(numberOfCharacters);
		return result;
	}

	/**
	 * Utility routine to create a panel with left aligned control(s).
	 * @param control a control to add to the panel
	 * @return a panel with left aligned control(s)
	**/
	JPanel createRow1(JComponent control) {
		JPanel p = new JPanel();
		p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
		p.add(control);
		p.add(Box.createHorizontalGlue());
		return p;
	}

	/**
	 * Utility routine to create a panel with left aligned control(s).
	 * @param control1 a control to add to the panel
	 * @param control2 a control to add to the panel
	 * @return a panel with left aligned control(s)
	**/
	JPanel createRow2(JComponent control1,JComponent control2) {
		JPanel p = new JPanel();
		p.setLayout(new BoxLayout(p,BoxLayout.X_AXIS));
		p.add(control1);
		p.add(control2);
		p.add(Box.createHorizontalGlue());
		return p;
	}

	/**
	 * Calls the appropriate button handler.
	 * @param e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == dbCancelButton) {
			dispose();
		} else if(e.getSource() == dbNextButton) {
			handleDbNextButton();
		} else if(e.getSource() == dbTableList) {
			handleDbTableList();
		} else if(e.getSource() == dbFIPSList) {
			assessSituation();
		} else if(e.getSource() == dbValueList) {
			assessSituation();
		} else if(e.getSource() == renderCancelButton) {
			dispose();
		} else if(e.getSource() == renderBackButton) {
			handleRenderBackButton();
		} else if(e.getSource() == renderNextButton) {
			handleRenderNextButton();
		//} else if(e.getSource() == renderPreviewButton) {
		//	handleRenderPreviewButton();
		} else if(e.getSource() == renderShowColors) {
			assessSituation();
		} else if(e.getSource() == mapDoneButton) {
			dispose();
		} else if(e.getSource() == mapBackButton) {
			handleMapBackButton();
		} else if(e.getSource() == mapSaveJPGButton) {
			handleMapSaveJPGButton();
		} else if(e.getSource() == mapPrintButton) {
			handleMapPrintButton();
		}
	}

	/** Handle a change to dbTableList **/
	void handleDbTableList() {
		dbFIPSList.removeAllItems();
		dbValueList.removeAllItems();
		String t = (String)dbTableList.getSelectedItem();
		if(t == null || t.length() <= 0) {
			assessSituation();
			return;
		}
		// Obtain list of columns and store to dbFIPSList and dbValueList
		String sql = "select * from " + t + " limit 1";
		PreparedStatement statement = null;
		ResultSet rs = null;
		try {
			statement = db.prepareStatement(sql);
			rs = statement.executeQuery();
			ResultSetMetaData rsmd = rs.getMetaData();
			int count = rsmd.getColumnCount();
			dbFIPSList.addItem("");
			dbValueList.addItem("");
			String fipsStateColumn = null;
			String fipsCountyColumn = null;
			for(int i=1;i<=count;i++) {
				String name = rsmd.getColumnName(i);
				dbFIPSList.addItem(name);
				dbValueList.addItem(name);
				if(name.equalsIgnoreCase("county") || name.equalsIgnoreCase("countyID")) {
					fipsCountyColumn = name;
				} else if(name.equalsIgnoreCase("state") || name.equalsIgnoreCase("stateID")) {
					fipsStateColumn = name;
				}
			}
			String fipsDefaultColumn = fipsStateColumn;
			if(fipsCountyColumn != null) {
				fipsDefaultColumn = fipsCountyColumn;
			}
			if(fipsDefaultColumn != null) {
				dbFIPSList.setSelectedItem(fipsDefaultColumn);
			}
		} catch(Exception e) {
			System.out.println("Unable to load columns in StateCountyMapGUI using " + sql);
			System.out.println(e);
			e.printStackTrace();
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
		assessSituation();
	}

	/**
	 * Check a table for duplicate entries, throwing a GISException if a duplicate is found.
	 * @param table name of the table to extract data from
	 * @param fipsColumn name of the column with a state or county FIPS code
	**/
	void checkForDuplicateEntries(String table, String fipsColumn) throws Exception {
		String sql = "select " + fipsColumn + ", count(*)"
				+ " from " + table
				+ " group by " + fipsColumn
				+ " having count(*) > 1"
				+ " limit 3";
		PreparedStatement statement = null;
		ResultSet rs = null;
		try {
			statement = db.prepareStatement(sql);
			rs = statement.executeQuery();
			int countFound = 0;
			String examples = "";
			while(rs.next()) {
				String fips = rs.getString(1);
				if(fips == null || fips.length() <= 0) {
					continue;
				}
				if(examples.length() > 0) {
					examples += ",";
				}
				examples += fips;
				countFound++;
			}
			if(countFound == 1) {
				String t= "There was a duplicate FIPS code found: " + examples;
				throw new GISException(t);
			} else if(countFound > 0) {
				String t = "There were duplicate FIPS code entries found."
						+ " Examples found: " + examples;
				throw new GISException(t);
			}
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				rs = null;
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				statement = null;
			}
		}
	}

	/** Handle the dbNextButton **/
	void handleDbNextButton() {
		assessSituation();
		if(!dbNextButton.isEnabled()) {
			return;
		}
		try {
			String table = (String)dbTableList.getSelectedItem();
			String fipsColumn = (String)dbFIPSList.getSelectedItem();
			String valueColumn = (String)dbValueList.getSelectedItem();
			String sql = "select " + fipsColumn + ", sum(" + valueColumn + ") as " + valueColumn
					+ " from " + table
					+ " group by " + fipsColumn;
			//checkForDuplicateEntries(table,fipsColumn);
			renderer = null; // allows garbage collection to occur
			legend = null;
			if(image != null) {
				image = null;
				applyImageToMapPanel();
			}
			renderer = new StateCountyMapRenderer();
			renderer.addFromSQL(db,sql,fipsColumn,valueColumn);
			renderMinValue.setText("" + renderer.getMinimumValue());
			renderMaxValue.setText("" + renderer.getMaximumValue());
			assessSituation();
		} catch(Exception e) {
			/**
			 * @issue Can't load map: [message]
			 * @explain MOVES was unable to read the data for your map.  This could be due to a
			 * database irregularity or an non-numeric value within your data.
			**/
			e.printStackTrace();
			JOptionPane.showMessageDialog(frame,
				"Can't load map: " + e,
				"State/County Map",
				JOptionPane.ERROR_MESSAGE);
			renderer = null;
			return;
		}
		getContentPane().removeAll();
		getContentPane().add(renderPanel, BorderLayout.CENTER);
		pack();
	}

	/** Handle the renderBackButton **/
	void handleRenderBackButton() {
		assessSituation();
		getContentPane().removeAll();
		getContentPane().add(dbPanel, BorderLayout.CENTER);
		pack();
	}

	/** Handle the renderNextButton **/
	void handleRenderNextButton() {
		assessSituation();
		if(!renderNextButton.isEnabled()) {
			return;
		}
		try {
			double minValue = Double.parseDouble(renderMinValue.getText());
			double maxValue = Double.parseDouble(renderMaxValue.getText());
			int binCount = Integer.parseInt(renderBinCount.getText());
			if(binCount < 1) {
				/**
				 * @issue There must be at least 1 data bin
				 * @explain When prompted for how many colors to divide the map data into, provide
				 * a number of at least 1.
				**/
				JOptionPane.showMessageDialog(frame,
					"There must be at least 1 data bin",
					"State/County Map",
					JOptionPane.ERROR_MESSAGE);
				return;
			}
			int whichPalette = Scale.PALETTE_FIRE;
			String paletteText = (String)renderColorList.getSelectedItem();
			if(paletteText.equalsIgnoreCase("Grayscale")) {
				whichPalette = Scale.PALETTE_GRAY;
			}
			legend = new Scale(binCount,minValue,maxValue,whichPalette,
					renderShowText.isSelected() && renderShowColors.isSelected());
			try {
				image = renderer.render(legend, renderShowColors.isSelected(),
						renderShowLabels.isSelected());
			} catch(Exception e) {
				/**
				 * @issue Can't create image: [message]
				 * @explain MOVES is unable to generate a map image.  Try making your map window
				 * smaller, as some older graphics cards do not have the memory for high resolution
				 * images.  Also, ensure that all files in your MOVES installation are not read-only,
				 * as the map making routines require write-access to their region definition files
				 * within the lib folder.
				**/
				e.printStackTrace();
				JOptionPane.showMessageDialog(frame,
					"Can't create image: " + e,
					"State/County Map",
					JOptionPane.ERROR_MESSAGE);
				return;
			}
			applyImageToMapPanel();
			assessSituation();
		} catch(Exception e) {
			// Nothing to do here
			return;
		}
		getContentPane().removeAll();
		getContentPane().add(mapPanel, BorderLayout.CENTER);
		pack();
		//setSize(820,600);
	}

	/** Handle the mapBackButton **/
	void handleMapBackButton() {
		assessSituation();
		getContentPane().removeAll();
		getContentPane().add(renderPanel, BorderLayout.CENTER);
		pack();
	}

	/** Handle the mapSaveJPGButton **/
	void handleMapSaveJPGButton() {
		if(image == null) {
			return;
		}
		FileDialog fd = new FileDialog(frame, "Save File", FileDialog.SAVE);
		fd.setVisible(true); //fd.show();
		if((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		if(!filePath.toLowerCase().endsWith(".jpg")) {
			if(filePath.endsWith(".")) {
				filePath += "jpg";
			} else {
				filePath += ".jpg";
			}
		}
		try {
			ImageUtility.saveJPG(image,filePath);
		} catch(Exception e) {
			/**
			 * @issue Can't save image: [message]
			 * @explain MOVES is unable to save the map image.  The [message] will contain
			 * more detail, but double check the path you selected for storing the image.
			 * If there is not enough room for the image or if there is a typo in the path, the
			 * image cannot be saved.
			**/
			JOptionPane.showMessageDialog(frame,
				"Can't save image: " + e,
				"State/County Map",
				JOptionPane.ERROR_MESSAGE);
		}
	}

	/** Handle the mapPrintButton **/
	void handleMapPrintButton() {
		if(image == null) {
			return;
		}
		try {
			ImageUtility.print(image);
		} catch(Exception e) {
			/**
			 * @issue Can't print image: [message]
			 * @explain MOVES is unable to print the map image.  The [message] will contain more
			 * detail, but try checking your printer settings.  Some printer drivers cannot print
			 * large images at high resolution.
			**/
			JOptionPane.showMessageDialog(frame,
				"Can't print image: " + e,
				"State/County Map",
				JOptionPane.ERROR_MESSAGE);
		}
	}

	/** Update control states based on current data **/
	void assessSituation() {
		// dbPanel
		String fipsColumn = null;
		String valueColumn = null;
		if(dbFIPSList != null && dbValueList != null) {
			fipsColumn = (String)dbFIPSList.getSelectedItem();
			valueColumn = (String)dbValueList.getSelectedItem();
		}
		if(dbNextButton != null) {
			dbNextButton.setEnabled(fipsColumn != null && fipsColumn.length() > 0
					&& valueColumn != null && valueColumn.length() > 0);
		}

		// renderPanel
		if(renderNextButton != null) {
			boolean renderIsOK = false;
			if(renderBinCount != null && renderMinValue != null && renderMaxValue != null) {
				renderIsOK = true;
				// Check number of bins
				String countText = renderBinCount.getText();
				if(countText == null) {
					renderIsOK = false;
				} else {
					try {
						int count = Integer.parseInt(countText);
						if(count < 1) {
							renderIsOK = false;
						}
					} catch(Exception e) {
						renderIsOK = false;
					}
				}
				// Check min value
				if(renderIsOK) {
					String valueText = renderMinValue.getText();
					if(valueText == null) {
						renderIsOK = false;
					} else {
						try {
							Double.parseDouble(valueText);
						} catch(Exception e) {
							renderIsOK = false;
						}
					}
				}
				// Check max value
				if(renderIsOK) {
					String valueText = renderMaxValue.getText();
					if(valueText == null) {
						renderIsOK = false;
					} else {
						try {
							Double.parseDouble(valueText);
						} catch(Exception e) {
							renderIsOK = false;
						}
					}
				}
			}
			renderNextButton.setEnabled(renderIsOK);
		}
		if(renderShowColors != null && renderShowText != null) {
			if(renderShowColors.isSelected()) {
				renderShowText.setEnabled(true);
			} else {
				renderShowText.setEnabled(false);
				//renderShowText.setSelected(false);
			}
		}

		// mapPanel
		if(mapSaveJPGButton != null && mapPrintButton != null) {
			mapSaveJPGButton.setEnabled(image != null);
			mapPrintButton.setEnabled(image != null);
		}
	}
}
