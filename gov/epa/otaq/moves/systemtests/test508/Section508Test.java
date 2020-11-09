/*
 * @(#)Section508Test
 */
 
package gov.epa.otaq.moves.systemtests.test508;

import java.awt.Component;
import java.awt.Container;

import javax.accessibility.AccessibleContext;
import javax.swing.AbstractButton;
import javax.swing.CellRendererPane;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.plaf.metal.MetalComboBoxButton;
import javax.swing.plaf.metal.MetalScrollButton;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;

import gov.epa.otaq.moves.common.JListWithToolTips;
import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.StringUtilities;
import gov.epa.otaq.moves.master.framework.MOVESAPI;
import gov.epa.otaq.moves.master.framework.SystemConfiguration;
import gov.epa.otaq.moves.master.gui.AdvancedPerformanceFeatures;
import gov.epa.otaq.moves.master.gui.CreateInputDatabase;
import gov.epa.otaq.moves.master.gui.Description;
import gov.epa.otaq.moves.master.gui.GeneralOutput;
import gov.epa.otaq.moves.master.gui.LoginDialog;
import gov.epa.otaq.moves.master.gui.MOVESWindow;
import gov.epa.otaq.moves.master.gui.MacroscaleGeographicBounds;
import gov.epa.otaq.moves.master.gui.ManageInputDataSets;
import gov.epa.otaq.moves.master.gui.OffRoadVehicleEquipment;
import gov.epa.otaq.moves.master.gui.OnRoadVehicleEquipment;
import gov.epa.otaq.moves.master.gui.OutputEmissionsBreakdown;
import gov.epa.otaq.moves.master.gui.PollutantsAndProcesses;
import gov.epa.otaq.moves.master.gui.PreaggregationOptions;
import gov.epa.otaq.moves.master.gui.RoadTypeScreen;
import gov.epa.otaq.moves.master.gui.Scale;
import gov.epa.otaq.moves.master.gui.TimeSpans;
import gov.epa.otaq.moves.master.runspec.RunSpec;
import gov.epa.otaq.moves.master.runspec.RunSpecTest;
import junit.extensions.TestSetup;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Section508Test. Enumerates all visible controls and tests for Section 508 Accessibility Compliance.
 * 
 * @author  	Bill Shaw
 * @author  	Mike Kender
 * @version		2020-08-12
**/
public class Section508Test extends TestCase {

	static MOVESWindow movesWindow;
	static int depthCount = 0;
	static int itemCount = 0;

	public Section508Test(String name) {
		super(name);
	}
	
	public static void main(String args[]) {
		junit.textui.TestRunner.run(Section508Test.class);
	}

	public void testStringTruncation() {
		String s = "1234567890";
		System.out.println(truncStringValue(s, 5));
		System.out.println(truncStringValue(s, 10));
		System.out.println(truncStringValue(null, 5));
	}
	private String truncStringValue(String str, int maxLength) {
		String ret = "";
		if (str != null) {
			int strLen = str.length();
			if (strLen > maxLength) {
				ret = str.substring(0, maxLength - 1);
			}
			else {
				ret = str;
			}
		}
		return ret;
	}

	public void testEnumTest() throws Exception {
		movesWindow = new MOVESWindow(false);

		assertNotNull(movesWindow);  // instantiated?

		listAllComponentsIn(movesWindow.getContentPane());
	}
	
	public void testGuiLogo() throws Exception {
		movesWindow = new MOVESWindow(false);

		LoginDialog l = new LoginDialog(movesWindow);

		int errors = listAllComponentsIn(l);
		assertTrue("Errors found in Login Dialog, see log for details", errors == 0);
	}
	
	public void testDescription() {
		Description d = new Description();

		int errors = listAllComponentsIn(d);
		assertTrue("Errors found in Description, see log for details", errors == 0);
	}
	
	public void testScale() {
		Scale component = new Scale();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in Scale, see log for details", errors == 0);
	}
	
	public void testTimeSpans() {
		TimeSpans component = new TimeSpans();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in TimeSpans, see log for details", errors == 0);
	}
	
	public void testGeoBounds() throws Exception {
		movesWindow = new MOVESWindow(false);

		MacroscaleGeographicBounds macroscaleGeographicBoundsPanel = new MacroscaleGeographicBounds(movesWindow);

		int errors = listAllComponentsIn(macroscaleGeographicBoundsPanel);
		assertTrue("Errors found in MacroscaleGeographicBounds, see log for details", errors == 0);
	}
	
	public void testOnRoadVehicles() {
		OnRoadVehicleEquipment component = new OnRoadVehicleEquipment();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in OnRoadVehicleEquipment, see log for details", errors == 0);
	}
	
	public void testOffRoadVehicles() {
		OffRoadVehicleEquipment component = new OffRoadVehicleEquipment();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in OffRoadVehicleEquipment, see log for details", errors == 0);
	}
	
	public void testRoadType() {
		RoadTypeScreen component = new RoadTypeScreen();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in RoadTypeScreen, see log for details", errors == 0);
	}

	public void testPollutantsAndProcesses() throws Exception {
		PollutantsAndProcesses pp = new PollutantsAndProcesses();

		int errors = listAllComponentsIn(pp);
		assertTrue("Errors found in PollutantsAndProcesses, see log for details", errors == 0);
	}
	
	public void testGeneralOutput() {
		GeneralOutput component = new GeneralOutput();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in GeneralOutput, see log for details", errors == 0);
	}
	
	public void testOutputEmissionsDetail() {
		OutputEmissionsBreakdown component = new OutputEmissionsBreakdown();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in OutputEmissionsBreakdown, see log for details", errors == 0);
	}
	
	public void testCreateNewDatabase() {
		RunSpec runSpec = new RunSpec();
		RunSpecTest.setSampleValues(runSpec);
		CreateInputDatabase component = new CreateInputDatabase(runSpec);

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in CreateInputDatabase, see log for details", errors == 0);
	}
	
	public void testAdvancedFeatures() {
		AdvancedPerformanceFeatures component = new AdvancedPerformanceFeatures();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in AdvancedPerformanceFeatures, see log for details", errors == 0);
	}
	
	public void testManageInputDataSets() {
		ManageInputDataSets component = new ManageInputDataSets();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in ManageInputDataSets, see log for details", errors == 0);
	}
	
	public void testPreaggregationOptions() {
		PreaggregationOptions component = new PreaggregationOptions();

		int errors = listAllComponentsIn(component);
		assertTrue("Errors found in PreaggregationOptions, see log for details", errors == 0);
	}
	
	public static Test suite() {
		TestSuite suite = new TestSuite(Section508Test.class);

		TestSetup wrapper = new TestSetup(suite) {
			protected void setUp() throws Exception {
				boolean okToPopupMessages = false; 
				
				if(MOVESAPI.hasMasterOnThisComputer()){
					String message = "A MOVES instance is already running, or a firewall is blocking port(s) used by MOVES.\nStartup of this instance of the MOVES master cannot continue.";
					Logger.log(LogMessageCategory.ERROR, message);
					System.exit(0);
					return;
				}

				if (!SystemConfiguration.theSystemConfiguration.didLoad) {
					String message = "The system configuration failed to load.";
					Logger.log(LogMessageCategory.ERROR, message);
					//System.exit(0);
					return;
				}
				
				movesWindow = new MOVESWindow(okToPopupMessages);

				//System.out.println("Enumerating Window Controls...");
				listAllComponentsIn(movesWindow.getContentPane());
			}
			protected void tearDown() throws Exception {}
		};
		return wrapper;
	}

	public static int listAllComponentsIn(Container parent) {
		depthCount++;
		itemCount++;
		
		int errorCount = 0;

		for (Component c : parent.getComponents()) {
			AccessibleContext ac = c.getAccessibleContext();  //ac is of type Accessible
//			System.out.println("Component: " + ((Component)c).toString());

			if (c.toString().indexOf("MetalSplitPaneDivider") != -1) {
				assertNotNull(true);
			} else if (c instanceof JTextField) {
				System.out.println("Text: " + ((JTextField) c).getText());
			} else if (c instanceof MetalScrollButton) {
				assertNotNull(c instanceof MetalScrollButton);
			} else if(c instanceof MetalComboBoxButton) {
				assertNotNull(c instanceof MetalComboBoxButton);
			} else if (c instanceof JButton) {
				System.out.println("JButton: " + ((JButton) c).getText());
				
				if(((JButton) c).getDisplayedMnemonicIndex() == -1
						&& ((JButton) c).getMnemonic() == 0) {
					errorCount++;
					System.err.println("Button " + ((JButton) c).getName() + " has no hotkey.");
				}
				
				if(((JButton) c).getToolTipText() == null) {
					errorCount++;
					System.err.println("Button " + ((JButton) c).getName() + " has no tooltip.");
				}
			} else if (c instanceof JViewport) {
				assertNotNull(c instanceof JViewport);
			} else if (c instanceof JListWithToolTips) {
				assertNotNull(c instanceof JListWithToolTips);
			} else if (c instanceof JScrollPane) {
				assertNotNull(c instanceof JScrollPane);
			} else if (c instanceof JScrollBar) {
				assertNotNull(c instanceof JScrollBar);
			} else if (c instanceof JProgressBar) {
				assertNotNull(c instanceof JProgressBar	);
			} else if (c instanceof JPanel) {
				assertNotNull(c instanceof JPanel);
			} else if (c instanceof JLabel) {
				String labelText = StringUtilities.safeGetString(((JLabel)c).getText()).trim();
				if(labelText.equalsIgnoreCase("Ready...")
						|| "hadDeprecatedIntercityBusesLabel".equals(c.getName()) ) {
					// Nothing to do with these special case labels.
				} else if(labelText.length() > 0) { // Only labels with text need menumonics
					if (((JLabel) c).getIcon() != null) {
						if(((JLabel) c).getToolTipText() == null
								&& !labelText.startsWith("<html><body>")) {
							errorCount++;
							System.err.println("JLabel " + ((JLabel)c).getText() + " has icon and no tooltip.");
						}
					}
					
//					if(((JLabel) c).getDisplayedMnemonicIndex() != -1) {
//						errorCount++;
//						System.err.println("JLabel " + ((JLabel)c).toString() + " has no hotkey.");
//					}
				}
			} else if (c instanceof TableCellRenderer) {
				if(((JComponent) c).getToolTipText() == null) {
					errorCount++;
					System.err.println("TableCellRenderer " + ((TableCellRenderer)c).toString() + " has no tooltip.");
				}
			} else if (c instanceof JSplitPane) {
				assertNotNull(c instanceof JSplitPane);
			} else if (c instanceof CellRendererPane) {
				assertNotNull(c instanceof CellRendererPane);
			} else if (c instanceof JTableHeader) {
				assertNotNull(c instanceof JTableHeader);
			} else if (c instanceof JTextPane) {
				assertNotNull(c instanceof JTextPane);
			} else if (c instanceof JCheckBox) {
				if(((JCheckBox) c).getDisplayedMnemonicIndex() == -1
						&& ((JCheckBox) c).getMnemonic() == -1) {
					errorCount++;
					System.err.println("Button " + ((AbstractButton) c).getText() + " has no hotkey.");
				}
			} else if (c instanceof JRadioButton) {
				String ap = ac.getAccessibleParent().toString();
				if (ap.indexOf("MOVESNavigation") != -1) { //Left Navigation Panel
					assertNotNull(true);
				} else {
					if(((AbstractButton) c).getMnemonic() == -1
							&& ((AbstractButton) c).getDisplayedMnemonicIndex() == -1) {
						errorCount++;
						System.err.println("JRadioButton " + ac.getAccessibleName() + " appeared outside of left pane.");
					}
//					/*
//					System.out.println("======================================================");
//					System.out.println("AccessibleChildrenCount: " + ac.getAccessibleChildrenCount());
//					System.out.println("AccessibleName: " + ac.getAccessibleName());
//					System.out.println("AccessibleDescription: " + ac.getAccessibleDescription());
//					System.out.println("AccessibleRole: " + ac.getAccessibleRole());
//					System.out.println("AccessibleStateSet: " + ac.getAccessibleStateSet());
//					System.out.println("Mnemonic: " + ((AbstractButton) c).getMnemonic());  
//					*/
				}
			} else {
//				System.out.println("======================================================");
//				System.out.println(c.getClass().getName() + " " + c.getName());
				/*
				System.out.println("AccessibleName: " + ac.getAccessibleName());
				System.out.println("AccessibleDescription: " + ac.getAccessibleDescription());
				*/
			}

			/*
 			if (c instanceof JComponent) {
				String ttText = ((JComponent)c).getToolTipText();
				boolean ttEmpty = (ttText == null || ttText.length() == 0);
				boolean toolTipOK = !ttEmpty
					|| c instanceof JTree
					|| c instanceof JEditorPane
					|| c instanceof JList
					|| c instanceof JRootPane
					|| c instanceof JScrollBar;
			}
			*/

			if (c instanceof Container) {
				System.out.println("getting new container " + (Object)c.getClass().getSimpleName());
			   errorCount = errorCount + listAllComponentsIn((Container)c);
			   //depthCount--;
			} 
		}
		
		return errorCount;
	}
	
	String getContainerType(Container c) {
		if (c instanceof JTree) {
			return "JTree";
		}
		return "abc";
	}
}
