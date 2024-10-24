/**************************************************************************************************
 * @(#)InternalControlStrategies.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.util.*;
import java.io.*;
import javax.swing.JOptionPane;
import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.InternalControlStrategy;
import gov.epa.otaq.moves.master.framework.InternalControlStrategyPanel;
import gov.epa.otaq.moves.master.framework.InternalControlStrategySimpleInstance;
import gov.epa.otaq.moves.master.framework.InternalControlStrategySingleInstanceOnly;
import gov.epa.otaq.moves.master.framework.IImportExportHandler;
import gov.epa.otaq.moves.master.framework.IExportDefaultHandler;
import gov.epa.otaq.moves.master.runspec.RunSpecXML;

/**
 * Class for MOVES InternalControlStrategies panel.
 *
 * @author		Wesley Faler
 * @author		Tim Hull
 * @author  	Bill Shaw (508 compliance mods)
 * @version		2015-03-17
**/
public class InternalControlStrategies extends JPanel implements RunSpecEditor, ActionListener,
		ListSelectionListener {
	/** Panel color. **/
	Color panelColor;
	/** Text color. **/
	Color textColor;
	/** Label above list of loaded objects **/
	JLabel listLabel;
	/** list model for list of loaded objects **/
	DefaultListModel<InternalControlStrategy> instancesListModel;
	/** instances list **/
	JList<InternalControlStrategy> instancesList;
	/** scroll pane allowing instancesList to scroll **/
	JScrollPane instancesScrollPane;
	/** Button for creating a new instance of an InternalControlStrategy **/
	JButton newButton;
	/** Button for importing an InternalControlStrategy already stored as XML **/
	JButton importButton;
	/** Button for exporting the currently selected InternalControlStrategy to XML **/
	JButton exportButton;
	/** Button for exporting the default data of the currently selected InternalControlStrategy to XML **/
	JButton exportDefaultButton;
	/** Button for removing the currently selected InternalControlStrategy **/
	JButton deleteButton;
	/** Default panel to display when no object is selected **/
	JPanel defaultPanel;
	/** currently selected object **/
	InternalControlStrategy currentStrategy;
	/** scroll pane for the details panel of the current strategy **/
	JScrollPane detailsScrollPane;
	/** Button for canceling edits made since last save or load **/
	JButton cancelButton;
	/** Button for editing the description of an InternalControlStrategy **/
	JButton descriptionButton;
	/** Button for checking the status of an InternalControlStrategy **/
	JButton checkButton;
	/**
	 * True if the strategy type being managed implements the
	 * InternalControlStrategySimpleInstance interface.
	**/
	boolean isSimpleInstance = false;
	/**
	 * True if the strategy type being managed implements the
	 * IImportExportHandler interface.
	**/
	boolean isImportExportHandler = false;
	/**
	 * True if the strategy type being managed implements the
	 * IExportDefaultHandler interface.
	**/
	boolean isExportDefaultHandler = false;
	/**
	 * True if the strategy type being managed implements the
	 * InternalControlStrategyUseImporterOnly interface.
	**/
	public boolean useImporterOnly = false;

	/**
	 * Constructs an InternalControlStrategies panel, also creates and sets the
	 * layouts of the controls.
	**/
	public InternalControlStrategies() {
	}

	/**
	 * Called after construction and after the name has been set, which denotes the class
	 * being managed.
	**/
	public void finishSetup() {
		try {
			Class c = Class.forName(getName());
			Class simpleInstanceType = Class.forName("gov.epa.otaq.moves.master.framework.InternalControlStrategySimpleInstance");
			if(c != null && simpleInstanceType != null) {
				Class[] interfaces = c.getInterfaces();
				if(interfaces != null) {
					for(int i=0;i<interfaces.length;i++) {
						if(interfaces[i].equals(simpleInstanceType)) {
							isSimpleInstance = true;
							break;
						}
					}
				}
			}

			Class importExportHandlerType = Class.forName("gov.epa.otaq.moves.master.framework.IImportExportHandler");
			if(c != null && importExportHandlerType != null) {
				Class[] interfaces = c.getInterfaces();
				if(interfaces != null) {
					for(int i=0;i<interfaces.length;i++) {
						if(interfaces[i].equals(importExportHandlerType)) {
							isImportExportHandler = true;
							break;
						}
					}
				}
			}

			Class exportDefaultHandlerType = Class.forName("gov.epa.otaq.moves.master.framework.IExportDefaultHandler");
			if(c != null && exportDefaultHandlerType != null) {
				Class[] interfaces = c.getInterfaces();
				if(interfaces != null) {
					for(int i=0;i<interfaces.length;i++) {
						if(interfaces[i].equals(exportDefaultHandlerType)) {
							isExportDefaultHandler = true;
							break;
						}
					}
				}
			}

			Class useImporterOnlyType = Class.forName("gov.epa.otaq.moves.master.framework.InternalControlStrategyUseImporterOnly");
			if(c != null && exportDefaultHandlerType != null) {
				Class[] interfaces = c.getInterfaces();
				if(interfaces != null) {
					for(int i=0;i<interfaces.length;i++) {
						if(interfaces[i].equals(useImporterOnlyType)) {
							useImporterOnly = true;
							break;
						}
					}
				}
			}
		} catch(Exception e) {
			/**
			 * @issue Unable to initialize InternalControlStrategy
			 * @explain While setting up a requested internal control strategy, an error
			 * occurred.  This is likely a case of a RunSpec that requests an internal
			 * control strategy that is not compiled into this machine's MOVES.
			**/
			Logger.log(LogMessageCategory.ERROR, "Unable to initialize InternalControlStrategy");
		}

		createControls();
		arrangeControls();
		assessSituation();
	}

	/**
	 * Gets text to be included in the RunSpec print out.
	 *
	 * @param runspec The runspec to acquire data from
	 * @param destination The StringBuffer to fill.
	**/
	public void getPrintableDescription(RunSpec runspec, StringBuffer destination) {
		LinkedList<InternalControlStrategy> instances =
				(LinkedList<InternalControlStrategy>)runspec.internalControlStrategies.get(getName());
		if(isSimpleInstance) {
			// Simple instances don't appear as add-ins the way other strategies do.  They don't
			// show descriptions and their text is shown at the root level like any of the
			// standard panels.
			if(instances != null) {
				for(Iterator<InternalControlStrategy> i=instances.iterator();i.hasNext();) {
					InternalControlStrategy strategy = (InternalControlStrategy)i.next();
					InternalControlStrategyPanel panel = strategy.getDetailsPanel();
					if(panel != null) {
						panel.getPrintableDescription(runspec,destination);
					}
					break;
				}
			}
		} else {
			boolean hasTitle = false;
			if(instances != null) {
				for(Iterator<InternalControlStrategy> i=instances.iterator();i.hasNext();) {
					InternalControlStrategy strategy = (InternalControlStrategy)i.next();
					destination.append("\t" + strategy.getDescription() + "\r\n");
					InternalControlStrategyPanel panel = strategy.getDetailsPanel();
					if(panel != null) {
						if(!hasTitle) {
							hasTitle = true;
							destination.append("Strategies:\r\n");
						}
						panel.getPrintableDescription(runspec,destination);
					}
				}
			}
		}
		destination.append("\r\n");
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {
		// Set Up colors
		panelColor = new Color(98, 176, 255);
		textColor = new Color(0, 0, 0);

		// Create list of InternalControlStrategy instances
		listLabel = new JLabel("Loaded objects:");
		listLabel.setName("listLabel");
		listLabel.setForeground(textColor);

		instancesListModel = new DefaultListModel<InternalControlStrategy>();
		instancesList = new JListWithToolTips<InternalControlStrategy>(instancesListModel);
		instancesList.setName("instancesList");
		instancesList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		instancesList.setSelectedIndex(-1);
		instancesList.addListSelectionListener(this);
		instancesList.setVisibleRowCount(6);
		//instancesList.setPrototypeCellValue("XXXXXXXXXXXXXX");
		instancesScrollPane = new JScrollPane(instancesList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		instancesScrollPane.setName("instancesScrollPane");
		listLabel.setDisplayedMnemonic('L');
		listLabel.setLabelFor(instancesList);

		// Create buttons
		newButton = new JButton("New");
		newButton.setName("newButton");
		newButton.addActionListener(this);
		ToolTipHelper.add(newButton,"Create a new object");
		newButton.setMnemonic('N');
		newButton.setDisplayedMnemonicIndex(0);

		importButton = new JButton("Import...");
		importButton.setName("importButton");
		importButton.addActionListener(this);
		ToolTipHelper.add(importButton,"Import a previously exported object");
		importButton.setMnemonic('I');
		importButton.setDisplayedMnemonicIndex(0);

		exportButton = new JButton("Export...");
		exportButton.setName("exportButton");
		exportButton.addActionListener(this);
		ToolTipHelper.add(exportButton,"Export the current object to a file");
		exportButton.setMnemonic('E');
		exportButton.setDisplayedMnemonicIndex(0);

		exportDefaultButton = new JButton("Export Default...");
		exportDefaultButton.setName("exportDefaultButton");
		exportDefaultButton.addActionListener(this);
		ToolTipHelper.add(exportDefaultButton,"Export default data to a file");
		exportDefaultButton.setMnemonic('x');
		exportDefaultButton.setDisplayedMnemonicIndex(1);

		deleteButton = new JButton("Delete...");
		deleteButton.setName("deleteButton");
		deleteButton.addActionListener(this);
		ToolTipHelper.add(deleteButton,"Delete the current object from the RunSpec");
		deleteButton.setMnemonic('e');
		deleteButton.setDisplayedMnemonicIndex(1);

		cancelButton = new JButton("Cancel");
		cancelButton.setName("cancelButton");
		cancelButton.addActionListener(this);
		ToolTipHelper.add(cancelButton,"Cancel recent edits");
		cancelButton.setMnemonic('a');
		cancelButton.setDisplayedMnemonicIndex(1);

		descriptionButton = new JButton("Description...");
		descriptionButton.setName("descriptionButton");
		descriptionButton.addActionListener(this);
		ToolTipHelper.add(descriptionButton,"Edit the description of the current object");
		descriptionButton.setMnemonic('p');
		descriptionButton.setDisplayedMnemonicIndex(6);

		checkButton = new JButton("Check...");
		checkButton.setName("checkButton");
		checkButton.addActionListener(this);
		ToolTipHelper.add(checkButton,
				"Explain reasons for the Not Ready (exclaimation point) status");
		checkButton.setMnemonic('h');
		checkButton.setDisplayedMnemonicIndex(1);

		// Panels
		defaultPanel = new JPanel();
		detailsScrollPane = new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		detailsScrollPane.setViewportView(defaultPanel);
		if(isSimpleInstance) {
			detailsScrollPane.setViewportBorder(null);
			detailsScrollPane.setBorder(null);
		}
	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		setLayout(new GridBagLayout());

		if(isSimpleInstance) {
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.fill = GridBagConstraints.NONE;
			gbc.insets = new Insets(2,2,2,2);
			gbc.weightx = 0;
			gbc.weighty = 0;

			LayoutUtility.setPositionOnGrid(gbc,0, 0, "NORTH", 1, 1);
			gbc.weightx = 1;
			gbc.weighty = 1;
			gbc.fill = GridBagConstraints.BOTH;
			add(detailsScrollPane, gbc);
		} else {
			setBackground(panelColor);

			GridBagConstraints gbc = new GridBagConstraints();
			gbc.fill = GridBagConstraints.NONE;
			gbc.insets = new Insets(2,2,2,2);
			gbc.weightx = 0;
			gbc.weighty = 0;

			JPanel leftPanel = new JPanel();
			leftPanel.setLayout(new GridBagLayout());
			leftPanel.setBackground(panelColor);

			int y = 0;

			LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
			leftPanel.add(listLabel, gbc);
			LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 8);
			leftPanel.add(instancesScrollPane, gbc);
			y += 8;
			if(!useImporterOnly) {
				LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
				leftPanel.add(descriptionButton, gbc);
			}
			//LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
			//leftPanel.add(cancelButton, gbc);
			if(!useImporterOnly) {
				LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
				leftPanel.add(newButton, gbc);
			}
			LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
			leftPanel.add(deleteButton, gbc);
			if(!useImporterOnly) {
				LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
				leftPanel.add(importButton, gbc);
			}
			LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
			leftPanel.add(exportButton, gbc);
			if(!useImporterOnly) {
				if(isExportDefaultHandler) {
					LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
					leftPanel.add(exportDefaultButton, gbc);
				}
			}
			if(!useImporterOnly) {
				LayoutUtility.setPositionOnGrid(gbc,0, y++, "WEST", 1, 1);
				leftPanel.add(checkButton, gbc);
			}

			LayoutUtility.setPositionOnGrid(gbc,0, 0, "NORTH", 1, 1);
			gbc.weightx = 0;
			gbc.weighty = 0;
			gbc.fill = GridBagConstraints.NONE;
			add(leftPanel, gbc);
			LayoutUtility.setPositionOnGrid(gbc,1, 0, "NORTH", 1, 1);
			gbc.weightx = 1;
			gbc.weighty = 1;
			gbc.fill = GridBagConstraints.BOTH;
			add(detailsScrollPane, gbc);
		}
	}

	/**
	 * Listener method for list selection changes.
	 * @param e Event caused by a user selection.
	**/
	public void valueChanged(ListSelectionEvent e) {
		if(e != null && e.getValueIsAdjusting()) {
			return;
		}
		if(instancesList.getSelectedIndex() == -1) {
			detailsScrollPane.setViewportView(defaultPanel);
			currentStrategy = null;
		} else {
			Object[] items = instancesList.getSelectedValuesList().toArray();
			if(items.length > 0) {
				currentStrategy = (InternalControlStrategy)items[0];
				InternalControlStrategyPanel panel = currentStrategy.getDetailsPanel();
				if(panel == null) {
					detailsScrollPane.setViewportView(defaultPanel);
				} else {
					if(isSimpleInstance) {
						removeAll();

						GridBagConstraints gbc = new GridBagConstraints();
						gbc.fill = GridBagConstraints.NONE;
						gbc.insets = new Insets(2,2,2,2);
						gbc.weightx = 0;
						gbc.weighty = 0;

						LayoutUtility.setPositionOnGrid(gbc,0, 0, "CENTER", 1, 1);
						//gbc.weightx = 1;
						//gbc.weighty = 1;
						//gbc.fill = GridBagConstraints.BOTH;
						add(panel, gbc);
					} else {
						detailsScrollPane.setViewportView(panel);
					}
				}
			}
		}
		assessSituation();
	}

	/**
	 * Enable/Disable controls based upon current UI state
	**/
	void assessSituation() {
		boolean hasSelection = currentStrategy != null;

		cancelButton.setEnabled(hasSelection);
		deleteButton.setEnabled(hasSelection);
		descriptionButton.setEnabled(hasSelection && !useImporterOnly);
		exportButton.setEnabled(hasSelection);
		exportDefaultButton.setEnabled(hasSelection && !useImporterOnly);
		checkButton.setEnabled(hasSelection && !useImporterOnly);

		newButton.setEnabled(!useImporterOnly && canCreateNewInstance());

		importButton.setEnabled(!useImporterOnly && (hasSelection || !isImportExportHandler));
	}

	/**
	 * Listener method, calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == newButton) {
			processNewButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == importButton) {
			processImportButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == exportButton) {
			processExportButton();
		} else if(e.getSource() == exportDefaultButton) {
			processExportDefaultButton();
		} else if(e.getSource() == deleteButton) {
			processDeleteButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
			if(useImporterOnly && !hasAnInstance()) {
				MOVESNavigation.singleton.onDeletedLastInstance(this);
			}
		} else if(e.getSource() == cancelButton) {
			processCancelButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus();
		} else if(e.getSource() == descriptionButton) {
			processDescriptionButton();
		} else if(e.getSource() == checkButton) {
			processCheckButton();
		}
		assessSituation();
	}

	/**
	 * Check for one or more instances.
	 * @return true if there is at least one instance listed.
	**/
	public boolean hasAnInstance() {
		return instancesListModel != null && instancesListModel.getSize() > 0;
	}

	/**
	 * Determines if a new instance of the current InternalControlStrategy-derived class
	 * could be instantiated.  If there are no instances, this is true.  If there is
	 * an instance but it implements the InternalControlStrategySingleInstanceOnly
	 * or InternalControlStrategySimpleInstance interface, then this is false.
	 * @return true if another instance could be created without violating any wishes
	 * for a single instance per RunSpec.
	**/
	boolean canCreateNewInstance() {
		if(instancesListModel.getSize() > 0) {
			InternalControlStrategy strategy =
					(InternalControlStrategy)instancesListModel.getElementAt(0);
			if(strategy instanceof InternalControlStrategySingleInstanceOnly
					|| strategy instanceof InternalControlStrategySimpleInstance) {
				return false;
			}
		}
		return true;
	}

	/** Handle a click on the "New" button **/
	void processNewButton() {
		if(!canCreateNewInstance()) {
			// Complain that only of the selected type is allowed
			/**
			 * @issue Only one object per RunSpec is allowed for this type
			 * @explain For some internal control strategies, the AVFT for instance, it only makes
			 * sense to have at most one of them per RunSpec.
			**/
			JOptionPane.showMessageDialog(this,
					"Only one object per RunSpec is allowed for this type");
			return;
		}

		try {
			Class<?> c = Class.forName(getName());
			InternalControlStrategy strategy = (InternalControlStrategy) c.getConstructor().newInstance();
			instancesListModel.addElement(strategy);
			instancesList.setSelectedValue(strategy,true);
		} catch(Exception e) {
			/**
			 * @issue Unable to create InternalControlStrategy
			 * @explain While setting up a requested internal control strategy, an error
			 * occurred.  This is likely a case of a RunSpec that requests an internal
			 * control strategy that is not compiled into this machine's MOVES.
			**/
			Logger.logError(e, "Unable to create InternalControlStrategy");
		}
	}

	/** Handle a click on the "Import..." button **/
	void processImportButton() {
		Component parent = getParent();
		while(parent != null && !(parent instanceof Frame)) {
			parent = parent.getParent();
		}
		if(currentStrategy != null && currentStrategy instanceof IImportExportHandler) {
			IImportExportHandler handler = (IImportExportHandler)currentStrategy;
			if(handler.doImport((Frame)parent)) {
				return;
			}
		}

		if(!canCreateNewInstance()) {
			// Prompt to confirm replacement
			/**
			 * @issue Since only one object per RunSpec is allowed for this type,
			 * importing a new one will overwrite this object. Do you really want to do this?
			 * @explain For some internal control strategies, the AVFT for instance, it only makes
			 * sense to have at most one of them per RunSpec.
			**/
			int answer = JOptionPane.showConfirmDialog(this,
					StringUtilities.wrapString(
					"Since only one object per RunSpec is allowed for this type, "
					+"importing a new one will overwrite this object. "
					+"Do you really want to do this?",35),
					"Overwrite?",JOptionPane.YES_NO_OPTION);
			if(answer != JOptionPane.YES_OPTION) {
				return;
			}
		}
		FileDialog fd = new FileDialog((Frame)parent, "Import File", FileDialog.LOAD);
		fd.setVisible(true); //fd.show();

		if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File fileObject = new File(filePath);

		RunSpecXML xml = new RunSpecXML(null);
		InternalControlStrategy strategy = xml.loadInternalControlStrategy(fileObject,
				getName());
		if(strategy == null) {
			return;
		}
		if(!strategy.getClass().getName().equalsIgnoreCase(getName())) {
			/**
			 * @issue The selected file does not contain this type of object, it
			 * has not been loaded.
			 * @explain MOVES could not find the requested internal control strategy
			 * object within the file you selected.
			**/
			JOptionPane.showMessageDialog(this,
					StringUtilities.wrapString(
					"The selected file does not contain this type of object, it "
					+ "has not been loaded.",35));
			return;
		}
		if(instancesListModel.getSize() > 0) {
			InternalControlStrategy existingStrategy =
					(InternalControlStrategy)instancesListModel.getElementAt(0);
			if(existingStrategy instanceof InternalControlStrategySingleInstanceOnly
					|| existingStrategy instanceof InternalControlStrategySimpleInstance) {
				if(strategy.getClass() == existingStrategy.getClass()) {
					instancesListModel.removeElement(existingStrategy);
				}
			}
		}
		instancesListModel.addElement(strategy);
		instancesList.setSelectedValue(strategy,true);
	}

	/**
	 * Determine if the currently selected strategy can be exported
	 * @return true if all data is valid on the current strategy, false if there is
	 * no strategy or if the strategy's panel's calculateRunSpecSectionStatus returns
	 * anything other than OK.
	**/
	boolean canExport() {
		if(currentStrategy == null) {
			return false;
		}

		InternalControlStrategyPanel panel = currentStrategy.getDetailsPanel();
		if(panel != null) {
			RunSpecSectionStatus panelStatus = panel.calculateRunSpecSectionStatus(
					null,new TreeMap());
			if(panelStatus == null || panelStatus.status != RunSpecSectionStatus.OK) {
				String explanation = panel.explainRunSpecSectionStatus();
				if(explanation == null) {
					explanation = "";
				}
				/**
				 * @issue All data is not yet ready to be exported. [*]
				 * @explain The selected internal control strategy is not ready to
				 * export its data.  Check the strategy's GUI to resolve any errors
				 * before exporting.
				**/
				JOptionPane.showMessageDialog(this,
						StringUtilities.wrapString(
						"All data is not yet ready to be exported.\n"
						+ explanation,35));
				return false;
			}
		}

		return true;
	}

	/** Handle a click on the "Export..." button **/
	void processExportButton() {
		Component parent = getParent();
		while(parent != null && !(parent instanceof Frame)) {
			parent = parent.getParent();
		}
		if(currentStrategy != null && currentStrategy instanceof IImportExportHandler) {
			IImportExportHandler handler = (IImportExportHandler)currentStrategy;
			if(handler.doExport((Frame)parent)) {
				return;
			}
		}

		if(!canExport()) {
			return;
		}

		FileDialog fd = new FileDialog((Frame)parent, "Export File", FileDialog.SAVE);
		fd.setVisible(true); //fd.show();

		if ((fd.getDirectory() == null) || (fd.getFile() == null)) {
			return;
		}
		String filePath = fd.getDirectory() + fd.getFile();
		File fileObject = new File(filePath);

		RunSpecXML xml = new RunSpecXML(null);
		xml.saveInternalControlStrategy(fileObject,currentStrategy);
	}

	/** Handle a click on the "Export Default..." button **/
	void processExportDefaultButton() {
		Component parent = getParent();
		while(parent != null && !(parent instanceof Frame)) {
			parent = parent.getParent();
		}
		if(currentStrategy != null && currentStrategy instanceof IExportDefaultHandler) {
			IExportDefaultHandler handler = (IExportDefaultHandler)currentStrategy;
			if(handler.doExportDefault((Frame)parent)) {
				return;
			}
		}
	}

	/** Handle a click on the "Delete..." button **/
	void processDeleteButton() {
		if(currentStrategy == null) {
			return;
		}
		// Prompt to confirm deletion
		/** @nonissue **/
		int answer = JOptionPane.showConfirmDialog(this,
				StringUtilities.wrapString(
				"Do you want to remove these settings from your RunSpec?",35),
				"Delete?",JOptionPane.YES_NO_OPTION);
		if(answer != JOptionPane.YES_OPTION) {
			return;
		}
		// Do deletion
		InternalControlStrategy strategy = currentStrategy;
		instancesListModel.removeElement(strategy);
		valueChanged(null);
	}

	/** Handle a click on the "Cancel" button **/
	void processCancelButton() {
		if(currentStrategy == null) {
			return;
		}
		currentStrategy.cancelEdits();
	}

	/** Handle a click on the "Description" button **/
	void processDescriptionButton() {
		if(currentStrategy == null) {
			return;
		}
		Component parent = getParent();
		while(parent != null && !(parent instanceof JFrame)) {
			parent = parent.getParent();
		}
		DescriptionDialog dialog = new DescriptionDialog((JFrame)parent);
		dialog.description = currentStrategy.getDescription();
		dialog.setLocation(((JFrame)parent).getLocationOnScreen().x + 100,
				((JFrame)parent).getLocationOnScreen().y + 100);
		dialog.showModal();
		if(dialog.result) {
			currentStrategy.setDescription(dialog.description);
			instancesList.repaint();
		}
	}

	/** Handle a click on the "Check" button **/
	void processCheckButton() {
		if(currentStrategy == null) {
			return;
		}

		InternalControlStrategyPanel panel = currentStrategy.getDetailsPanel();
		if(panel != null) {
			RunSpecSectionStatus panelStatus = panel.calculateRunSpecSectionStatus(
					null,new TreeMap());
			String explanation = panel.explainRunSpecSectionStatus();
			if(explanation == null) {
				explanation = "These settings are OK.";
			}
			/** @nonissue **/
			JOptionPane.showMessageDialog(this,StringUtilities.wrapString(explanation,35));
		}
	}

	/**
	 * Saves the description text to a RunSpec.
	 * @param	runspec the RunSpec to get the description text.
	**/
	public void saveToRunSpec(RunSpec runspec) {
		if(currentStrategy != null && currentStrategy.getDetailsPanel() != null) {
			currentStrategy.getDetailsPanel().saveToRunSpec(runspec);
		}
		LinkedList<InternalControlStrategy> instances =
				(LinkedList<InternalControlStrategy>)
				runspec.internalControlStrategies.get(getName());
		if(instances != null) {
			instances.clear();
		} else {
			instances = new LinkedList<InternalControlStrategy>();
			runspec.internalControlStrategies.put(getName(),instances);
		}
		for(int i=0;i<instancesListModel.getSize();i++) {
			instances.add((InternalControlStrategy)instancesListModel.getElementAt(i));
		}
	}

	/**
	 * Loads the description text from a RunSpec.
	 * @param	runspec the RunSpec to get the description text from.
	**/
	public void loadFromRunSpec(RunSpec runspec) {
		currentStrategy = null;
		instancesListModel.removeAllElements();
		LinkedList<InternalControlStrategy> instances = runspec.internalControlStrategies.get(getName());
		if(isSimpleInstance) {
			if(instances == null) {
				instances = new LinkedList<InternalControlStrategy>();
				runspec.internalControlStrategies.put(getName(),instances);
			}
			if(instances.isEmpty()) {
				try {
					Class<?> c = Class.forName(getName());
					InternalControlStrategy strategy = (InternalControlStrategy) c.getConstructor().newInstance();
					instances.add(strategy);
				} catch(Exception e) {
					/**
					 * @issue Unable to create InternalControlStrategy
					 * @explain While setting up a requested internal control strategy, an error
					 * occurred.  This is likely a case of a RunSpec that requests an internal
					 * control strategy that is not compiled into this machine's MOVES.
					**/
					Logger.logError(e, "Unable to create InternalControlStrategy");
				}
			}
		}
		if(instances != null) {
			for(Iterator<InternalControlStrategy> i=instances.iterator();i.hasNext();) {
				instancesListModel.addElement(i.next());
			}
		}
		if(instances != null && instances.size() > 0) {
			instancesList.setSelectedIndex(0);
		}
		valueChanged(null);
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
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);

		LinkedList<InternalControlStrategy> instances =
				(LinkedList<InternalControlStrategy>)
				runspec.internalControlStrategies.get(getName());
		if(instances != null) {
			if(useImporterOnly && instances.size() > 0) {
				status = new RunSpecSectionStatus(RunSpecSectionStatus.NOT_READY);
			} else {
				for(Iterator i=instances.iterator();i.hasNext();) {
					InternalControlStrategy strategy = (InternalControlStrategy)i.next();
					InternalControlStrategyPanel panel = strategy.getDetailsPanel();
					if(panel != null) {
						RunSpecSectionStatus panelStatus = panel.calculateRunSpecSectionStatus(
								null,sections);
						if(panelStatus != null) {
							status.makeWorstOfTwo(panelStatus);
						}
					}
				}
			}
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
		sections.remove(getName());
		RunSpecSectionStatus status = new RunSpecSectionStatus(RunSpecSectionStatus.OK);
		runspec.internalControlStrategies.clear();
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

	/** Inner class for prompting for description **/
	class DescriptionDialog extends JDialog implements ActionListener {
		/** The dialog result, indicates true on OK button. **/
		public boolean result = false;
		/** The description **/
		public String description = "";

		/** The parent JFrame which invokes this dialog. **/
		JFrame frame;
		/** OK button. **/
		JButton okButton;
		/** Cancel button. **/
		JButton cancelButton;
		/** Panel holding the description label and editbox **/
		JPanel descriptionPanel;
		/** Label for the description **/
		JLabel descriptionLabel;
		/** The description editbox **/
		JTextField descriptionField;

		/**
		 * Constructor, also creates and sets the layouts of the controls.
		 * @param parent the parent frame to use for the dialog.
		**/
		public DescriptionDialog(JFrame parent) {
			super(parent, MOVESWindow.MOVES_VERSION + " - Edit Description");
			frame = parent;

			getContentPane().setLayout(new BorderLayout());
			getContentPane().add(createPanel(), BorderLayout.CENTER);
			pack();
			setResizable(false);
		}

		/** Allows the parent to display this dialog as modal. **/
		public void showModal() {
			pushDataToControls();
			pack();
			setModal(true);
			setVisible(true); //show();
		}

		/** Data from member variables to dialog controls. **/
		void pushDataToControls() {
			descriptionField.setText(description);
		}

		/**
		 * Creates and arranges all dialog controls.
		 * @return the container as JPanel.
		**/
		JPanel createPanel() {
			createControls();
			return arrangeControls();
		}

		/** Creates and initializes all controls on this panel. **/
		void createControls() {
			okButton = new JButton("OK");
			okButton.addActionListener(this);
			okButton.setName("okButton");
			ToolTipHelper.add(okButton,"Accept the new description");
			okButton.setMnemonic('O');
			okButton.setDisplayedMnemonicIndex(0);

			cancelButton = new JButton("Cancel");
			cancelButton.addActionListener(this);
			cancelButton.setName("cancelButton");
			ToolTipHelper.add(cancelButton,"Abandon changes to the description");
			cancelButton.setMnemonic('C');
			cancelButton.setDisplayedMnemonicIndex(0);

			descriptionPanel = new JPanel();
			descriptionPanel.setName("descriptionPanel");

			descriptionLabel = new JLabel("Description:");
			descriptionLabel.setName("descriptionLabel");
			descriptionField = new JTextField(30);
			descriptionField.setName("descriptionField");
			ToolTipHelper.add(descriptionField,"A description of the current object");
			descriptionLabel.setDisplayedMnemonic('p');
			descriptionLabel.setLabelFor(descriptionField);
		}

		/**
		 * Sets the layout of the controls.
		 * @return the container as JPanel of the controls
		**/
		public JPanel arrangeControls() {
			GridBagConstraints gbc = new GridBagConstraints();
			descriptionPanel.setLayout(new GridBagLayout());
			gbc.fill = GridBagConstraints.NONE;
			gbc.insets = new Insets(2,2,2,2);
			gbc.weightx = 0;
			gbc.weighty = 0;
			LayoutUtility.setPositionOnGrid(gbc,0, 0, "WEST", 1, 1);
			descriptionPanel.add(descriptionLabel, gbc);
			LayoutUtility.setPositionOnGrid(gbc,0, 1, "WEST", 1, 1);
			descriptionPanel.add(descriptionField, gbc);

			JPanel result = new JPanel();
			result.setLayout(new GridBagLayout());
			gbc.fill = GridBagConstraints.NONE;
			gbc.insets = new Insets(2,2,2,2);
			gbc.weightx = 0;
			gbc.weighty = 0;

			LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
			result.add(descriptionPanel,gbc);
			JPanel buttonPanel = createButtonsPanel();
			LayoutUtility.setPositionOnGrid(gbc,1,0, "NORTH", 1, 1);
			result.add(buttonPanel, gbc);

			return result;
		}

		/**
		 * Creates a panel containing the OK and Cancel buttons.
		 * @return the container as JPanel.
		**/
		JPanel createButtonsPanel() {
			JPanel box = new JPanel();
			box.setLayout(new GridLayout(2,1,5,5)); // 2 rows, 1 column, 5 pixel gaps
			box.add(okButton);
			box.add(cancelButton);

			JPanel result = new JPanel();
			result.setLayout(new BorderLayout());
			result.add(box, BorderLayout.NORTH);
			return result;
		}

		/**
		 * Calls the appropriate button handler.
		 * @param	e the ActionEvent to be handled.
		**/
		public void actionPerformed(ActionEvent e) {
			if(e.getSource() == okButton) {
				processOKButton();
			} else if(e.getSource() == cancelButton) {
				processCancelButton();
			}
		}

		/**
		 * OK button handler, will save the choices to the System Configuration
		 * before closing this dialog.
		**/
		void processOKButton() {
			description = descriptionField.getText();

			// indicates OK button
			result = true;
			dispose();
		}

		/**
		 * Cancel button handler, will reset the database connection to its original values
		 * before closing this dialog.
		**/
		void processCancelButton() {
			// indicates Cancel button
			result = false;
			dispose();
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
