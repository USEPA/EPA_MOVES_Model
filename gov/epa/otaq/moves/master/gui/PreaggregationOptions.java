/**************************************************************************************************
 * @(#)TimeSpans.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.gui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import gov.epa.otaq.moves.common.TimeSpan;
import gov.epa.otaq.moves.common.ToolTipHelper;
import gov.epa.otaq.moves.master.framework.MOVESAPI;

/**
 * Class for MOVES Preaggreation panel which has radio buttons for Time and Region Aggregation.
 *
 * @author  	John Covey (Task 1903)
 * @version     2019-09-13
**/
public class PreaggregationOptions extends JPanel implements  
		ActionListener {
	/** Singleton for the navigation panel **/
	public static PreaggregationOptions singleton = null;
	
	/** Panel contains the Time Aggregation controls. **/
	JPanel timeAggregatePanel;

	/** Time Aggregation Radio button group. **/
	ButtonGroup aggregateButtons;
	/** Aggregate by Year radio button. **/
	JRadioButton aggregateYear;
	/** Aggregate by Month radio button. **/
	JRadioButton aggregateMonth;
	/** Aggregate by Day radio button. **/
	JRadioButton aggregateDay;
	/** Aggregate by Hour radio button. **/
	JRadioButton aggregateHour;

	/** Panel contains the Region Aggregation controls. **/
	JPanel regionAggregatePanel;
	/** Region label. **/
	JLabel regionLabel;
	/** Radio button group. **/
	ButtonGroup radioButtons;
	/** Nation radio button. **/
	JRadioButton nation;
	/** State radio button. **/
	JRadioButton state;
	/** County radio button. **/
	JRadioButton county;
	/** Zone & Link radio button **/
	JRadioButton zoneLink;
	/** Custom Domain radio button **/
	JRadioButton customDomain;

	/**
	 * Constructs a TimeSpans panel, also creates and sets the layouts of the controls.
	**/
	public PreaggregationOptions() {
		singleton = this;

		if(!TimeSpan.isLoaded()) {
			TimeSpan.loadTimeObjects();
		}
		createControls();
		arrangeControls();
	}

	/** Creates and initializes all controls on this panel. **/
	public void createControls() {

		Dimension aggregatePanelSize = new Dimension(300,60);
		timeAggregatePanel = new JPanel();
		timeAggregatePanel.setName("timeAggregatePanel");
		timeAggregatePanel = new JPanel();
		timeAggregatePanel.setBorder(BorderFactory.createTitledBorder(
				"Time Aggregation"));
		timeAggregatePanel.setPreferredSize(aggregatePanelSize);

		aggregateButtons = new ButtonGroup();

		aggregateYear = new JRadioButton("Year");
		aggregateYear.setName("aggregateYear");
		aggregateYear.addActionListener(this);
		aggregateYear.setMnemonic('Y');
		aggregateYear.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(aggregateYear,"Aggregate input data by year");

		aggregateMonth = new JRadioButton("Month");
		aggregateMonth.setName("aggregateMonth");
		aggregateMonth.addActionListener(this);
		aggregateMonth.setMnemonic('M');
		aggregateMonth.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(aggregateMonth,"Aggregate input data by month");

		aggregateDay = new JRadioButton("Day");
		aggregateDay.setName("aggregateDay");
		aggregateDay.addActionListener(this);
		aggregateDay.setMnemonic('D');
		aggregateDay.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(aggregateDay,"Aggregate input data by day");

		aggregateHour = new JRadioButton("Hour");
		aggregateHour.setName("aggregateHour");
		aggregateHour.addActionListener(this);
		aggregateHour.setMnemonic('O');
		aggregateHour.setDisplayedMnemonicIndex(1);
		aggregateHour.setSelected(true);
		ToolTipHelper.add(aggregateHour,"Aggregate input data by hour");

		Dimension regionAggregatePanelSize = new Dimension(150, 200);
		regionAggregatePanel = new JPanel();
		regionAggregatePanel.setName("regionAggregatePanel");
		regionAggregatePanel = new JPanel();
		regionAggregatePanel.setBorder(BorderFactory.createTitledBorder(
				"Region Aggregation"));
		regionAggregatePanel.setPreferredSize(regionAggregatePanelSize);


		regionLabel = new JLabel("Region:");
		regionLabel.setName("regionLabel"); 
		radioButtons = new ButtonGroup();
		nation = new JRadioButton("Nation");
		nation.setName("nation");
		nation.addActionListener(this);
		nation.setMnemonic('N');
		nation.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(nation,"Simulate at the nation level");
		state = new JRadioButton("State (Alt+2)");
		state.setName("state");
		state.addActionListener(this);
		state.setMnemonic('2');
		ToolTipHelper.add(state,"Simulate at the state level");
		county = new JRadioButton("County");
		county.setName("county");
		county.addActionListener(this);
		county.setMnemonic('C');
		county.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(county,"Simulate at the county level");
		county.setSelected(true);
		zoneLink = new JRadioButton("Zone & Link");
		zoneLink.setName("zoneLink");
		zoneLink.addActionListener(this);
		zoneLink.setMnemonic('Z');
		zoneLink.setDisplayedMnemonicIndex(0);
		ToolTipHelper.add(zoneLink,"Simulate at the zone & link level");
		customDomain = new JRadioButton("Custom Domain");
		customDomain.setName("customDomain");
		customDomain.addActionListener(this);
		customDomain.setMnemonic('u');
		customDomain.setDisplayedMnemonicIndex(1);
		ToolTipHelper.add(customDomain,"This feature is disabled in this version of MOVES");
		//ToolTipHelper.add(customDomain,"Define a custom county");


	}

	/** Sets the layout of the controls. **/
	public void arrangeControls() {
		GridBagConstraints gbc = new GridBagConstraints();

		aggregateButtons.add(aggregateYear);
		aggregateButtons.add(aggregateMonth);
		aggregateButtons.add(aggregateDay);
		aggregateButtons.add(aggregateHour);

		timeAggregatePanel.setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 0;
		gbc.weighty = 0;
		LayoutUtility.setPositionOnGrid(gbc, 0, 0, "WEST", 1, 1);
		timeAggregatePanel.add(aggregateYear, gbc);
		LayoutUtility.setPositionOnGrid(gbc, 1, 0, "WEST", 1, 1);
		timeAggregatePanel.add(aggregateMonth, gbc);
		LayoutUtility.setPositionOnGrid(gbc, 2, 0, "WEST", 1, 1);
		timeAggregatePanel.add(aggregateDay, gbc);
		LayoutUtility.setPositionOnGrid(gbc, 3, 0, "WEST", 1, 1);
		timeAggregatePanel.add(aggregateHour, gbc);

		// Register a listener for the buttons.
		radioButtons.add(nation);
		radioButtons.add(state);
		radioButtons.add(county);
		radioButtons.add(zoneLink);
		radioButtons.add(customDomain);
		
		//jxc see if set layout is needed 
		regionAggregatePanel.setLayout(new BoxLayout(regionAggregatePanel, BoxLayout.Y_AXIS));
		LayoutUtility.setPositionOnGrid(gbc,4, 0, "WEST", 1, 1);
		regionAggregatePanel.add(nation,gbc);
		LayoutUtility.setPositionOnGrid(gbc,4, 1, "WEST", 1, 1);
		regionAggregatePanel.add(state,gbc);
		LayoutUtility.setPositionOnGrid(gbc,4, 2, "WEST", 1, 1);
		regionAggregatePanel.add(county,gbc);
		LayoutUtility.setPositionOnGrid(gbc,4, 3, "WEST", 1, 1);
		regionAggregatePanel.add(zoneLink,gbc);
		LayoutUtility.setPositionOnGrid(gbc,4, 4, "WEST", 1, 1);
		regionAggregatePanel.add(customDomain,gbc);
		
		gbc = new GridBagConstraints();
		setLayout(new GridBagLayout());
		gbc.fill = GridBagConstraints.NONE;
		gbc.insets = new Insets(2,2,2,2);
		gbc.gridwidth = 1;
		gbc.gridheight = 2;
		gbc.weightx = 0;
		gbc.weighty = 0;		
		LayoutUtility.setPositionOnGrid(gbc,0,0, "WEST", 1, 1);
		add(timeAggregatePanel,gbc);

 		LayoutUtility.setPositionOnGrid(gbc,0, 1, "CENTER", 1, 1);
		add(regionAggregatePanel, gbc);
	}

	/**
	 * Listener method, calls the appropriate button handler.
	 * @param	e the ActionEvent to be handled.
	**/
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == aggregateYear) {
			TimeSpans.singleton.processAggregateYearButton();
			TimeSpans.singleton.saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());                              
		} else if (e.getSource() == aggregateMonth) {
			TimeSpans.singleton.processAggregateMonthButton();
			TimeSpans.singleton.saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());                              
		} else if (e.getSource() == aggregateDay) {
			TimeSpans.singleton.processAggregateDayButton();
			TimeSpans.singleton.saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());                              
		} else if (e.getSource() == aggregateHour) {
			TimeSpans.singleton.processAggregateHourButton();
			TimeSpans.singleton.saveToRunSpec(MOVESAPI.getTheAPI().getRunSpec());                              
		} else if (e.getSource() == nation) {
			MacroscaleGeographicBounds.singleton.processNationButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus(
					MOVESNavigation.singleton.macroscaleGeographicBoundsOption,
					MOVESNavigation.singleton.lastRunSpecEditor, false);
			MOVESNavigation.singleton.updateOption(MOVESNavigation.singleton.macroscaleGeographicBoundsOption);
		} else if (e.getSource() == state) {
			MacroscaleGeographicBounds.singleton.processStateButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus(
					MOVESNavigation.singleton.macroscaleGeographicBoundsOption,
					MOVESNavigation.singleton.lastRunSpecEditor, false);
			MOVESNavigation.singleton.updateOption(MOVESNavigation.singleton.macroscaleGeographicBoundsOption);
		} else if (e.getSource() == county) {
			MacroscaleGeographicBounds.singleton.processCountyButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus(
					MOVESNavigation.singleton.macroscaleGeographicBoundsOption,
					MOVESNavigation.singleton.lastRunSpecEditor, false);
			MOVESNavigation.singleton.updateOption(MOVESNavigation.singleton.macroscaleGeographicBoundsOption);
		} else if (e.getSource() == zoneLink) {
			MacroscaleGeographicBounds.singleton.processZoneLinkButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus(
					MOVESNavigation.singleton.macroscaleGeographicBoundsOption,
					MOVESNavigation.singleton.lastRunSpecEditor, false);
			MOVESNavigation.singleton.updateOption(MOVESNavigation.singleton.macroscaleGeographicBoundsOption);
		} else if (e.getSource() == customDomain) {
			MacroscaleGeographicBounds.singleton.processCustomDomainButton();
			MOVESNavigation.singleton.updateRunSpecSectionStatus(
					MOVESNavigation.singleton.macroscaleGeographicBoundsOption,
					MOVESNavigation.singleton.lastRunSpecEditor, false);
			MOVESNavigation.singleton.updateOption(MOVESNavigation.singleton.macroscaleGeographicBoundsOption);
		}
		MOVESNavigation.singleton.updateRunSpecSectionStatus(MOVESNavigation.singleton.advancedFeaturesOption,
				MOVESNavigation.singleton.lastRunSpecEditor, false);

	}

}
