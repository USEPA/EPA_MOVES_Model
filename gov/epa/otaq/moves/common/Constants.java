/**************************************************************************************************
 * @(#)Constants.java
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.common;

import java.awt.Font;

/**
 * This class contains constant values that can be modified in one place to effect use
 * throughout the application. 
 * 
 *  
* @author John Covey (Task 2003)
* @version 2020-08-14
**/

public class Constants {
    
    private Constants() {
        
    }
    
    public static final String SCREEN_TITLE_FONT_FAMILY = "SansSerif";
    public static final int SCREEN_TITLE_FONT_STYLE = Font.BOLD;
    public static final int SCREEN_TITLE_FONT_SIZE = 28;
    
    public static final String DESCRIPTION_SCREEN_TITLE = "Description";
    public static final String SCALE_SCREEN_TITLE = "Scale";
    public static final String TIME_SPANS_SCREEN_TITLE = "Time Spans";
    public static final String GEOGRAPHIC_BOUNDS_SCREEN_TITLE = "Geographic Bounds";
    public static final String ONROAD_VEHICLES_SCREEN_TITLE = "Onroad Vehicles";
    public static final String NONROAD_EQUIPMENT_SCREEN_TITLE = "Nonroad Equipment";
    public static final String ROAD_TYPE_SCREEN_TITLE = "Road Type";
    public static final String POLLUTANTS_AND_PROCESSES_SCREEN_TITLE = "Pollutants and Processes";
    public static final String GENERAL_OUTPUT_SCREEN_TITLE = "General Output";
    public static final String OUTPUT_EMISSIONS_DETAIL_SCREEN_TITLE = "Output Emissions Detail";
    public static final String CREATE_INPUT_DATABASE_SCREEN_TITLE = "Create Input Database";
    public static final String ADVANCED_FEATURES_SCREEN_TITLE = "Advanced Features";
    
	public static final String MOVES_LOGIN_LOGO_TOOLTIP = "MOVES: Motor Vehicle Emission Simulator";
	public static final String POLLUTANT_CHECKBOX_FULLY_CHECKED_TOOLTIP = " is fully checked";
	public static final String POLLUTANT_CHECKBOX_PARTIALLY_CHECKED_TOOLTIP = " is partially checked";
	public static final String POLLUTANT_CHECKBOX_NOT_CHECKED_TOOLTIP = " is not checked";

	public static final String IMPORTER_GUI_BASE_TOOLTIP = " importer is ";
	public static final String IMPORTER_GUI_READY_TOOLTIP = "Ready";
	public static final String IMPORTER_GUI_NOT_READY_TOOLTIP = "Not Ready";
	
	public static final String SUMMARY_REPORT_RUN_NUMBERS_OPTIONS_LIST_TOOLTIP = "Optional values for Run Number parameters";
	public static final String SUMMARY_REPORT_RUN_NUMBERS_ASSIGNED_LIST_TOOLTIP = "Assigned values for Run Number parameters";
	public static final String SUMMARY_REPORT_RUN_NUMBERS_ADD_TOOLTIP = "Add selected Run Number(s) to the Selection List";
	public static final String SUMMARY_REPORT_RUN_NUMBERS_ADD_ALL_TOOLTIP = "Add all Run Number(s) to the Selection List";
	public static final String SUMMARY_REPORT_RUN_NUMBERS_REMOVE_TOOLTIP = "Remove selected Run Number(s) from the Selection List";
	public static final String SUMMARY_REPORT_RUN_NUMBERS_REMOVE_ALL_TOOLTIP = "Remove all Run Number(s) from the Selection List";

	public static final String SUMMARY_REPORT_CATEGORIES_OPTIONS_LIST_TOOLTIP = "Optional values for Category parameters";
	public static final String SUMMARY_REPORT_CATEGORIES_ASSIGNED_LIST_TOOLTIP = "Assigned values for Category parameters";
	public static final String SUMMARY_REPORT_CATEGORIES_ADD_TOOLTIP = "Add selected Categories to the Selection List";
	public static final String SUMMARY_REPORT_CATEGORIES_ADD_ALL_TOOLTIP = "Add all Categories to the Selection List";
	public static final String SUMMARY_REPORT_CATEGORIES_REMOVE_TOOLTIP = "Remove selected Categories from the Selection List";
	public static final String SUMMARY_REPORT_CATEGORIES_REMOVE_ALL_TOOLTIP = "Remove all Categories from the Selection List";
	public static final String SUMMARY_REPORT_CATEGORIES_MOVE_UP_TOOLTIP = "Move selected Category up in the Selection List";
	public static final String SUMMARY_REPORT_CATEGORIES_MOVE_DOWN_TOOLTIP = "Move selected Categories down in the Selection List";
    
	public static final String SUMMARY_REPORT_DATA_ITEMS_OPTIONS_LIST_TOOLTIP = "Optional values for Data Item parameters";
	public static final String SUMMARY_REPORT_DATA_ITEMS_ASSIGNED_LIST_TOOLTIP = "Assigned values for Data Item parameters";
	public static final String SUMMARY_REPORT_DATA_ITEMS_ADD_TOOLTIP = "Add selected Data Items to the Selection List";
	public static final String SUMMARY_REPORT_DATA_ITEMS_ADD_ALL_TOOLTIP = "Add all Data Items to the Selection List";
	public static final String SUMMARY_REPORT_DATA_ITEMS_REMOVE_TOOLTIP = "Remove selected Data Item(s) from the Selection List";
	public static final String SUMMARY_REPORT_DATA_ITEMS_REMOVE_ALL_TOOLTIP = "Remove all Data Item(s) from the Selection List";
	public static final String SUMMARY_REPORT_DATA_ITEMS_MOVE_UP_TOOLTIP = "Move selected Data Item(s) up in the Selection List";
	public static final String SUMMARY_REPORT_DATA_ITEMS_MOVE_DOWN_TOOLTIP = "Move selected Data Item(s) down in the Selection List";
	
	public static final String SUMMARY_REPORT_OK_TOOLTIP = "Click button to submit parameters for Summary Report";
	public static final String SUMMARY_REPORT_CANCEL_TOOLTIP = "Click button to close dialog for Summary Report parameters";

	public static final String ADVANCED_FEATURES_DESTINATION_REFRESH_TOOLTIP = "Refresh the list of Destination User Dataset Databases";
	public static final String ADVANCED_FEATURES_DESTINATION_CREATE_DB_TOOLTIP = "Create a Destination User Dataset Database";
	
	public static final String ADVANCED_FEATURES_DEFAULT_DB_REFRESH_TOOLTIP = "Refresh the list of Default Databases";
	public static final String ADVANCED_FEATURES_DEFAULT_DB_CREATE_DB_TOOLTIP = "Create a Default Database";

	public static final String MANAGE_INPUT_DATA_SETS_SELECTIONS_DB_TOOLTIP = "List of selected Input Data Set databases";
	
	public static final String GEOGRAPHIC_BOUNDS_STATES_TOOLTIP = "This list displays the 50 States along with the District of Columbia, Puerto Rico, and the U.S. Virgin Islands.";
	public static final String GEOGRAPHIC_BOUNDS_COUNTIES_TOOLTIP = "Displays available Counties to choose for selected state";
	public static final String GEOGRAPHIC_BOUNDS_SELECTIONS_TOOLTIP = "Displays chosen Counties for the RunSpec";
	
	public static final String ROAD_TYPE_AVAILABLE_TOOLTIP = "Displays available Road Type values";
	public static final String ROAD_TYPE_SELECTED_TOOLTIP = "Displays chosen Road Type values for the RunSpec";

	public static final String NONROAD_EQUIPMENT_FUELS_TOOLTIP = "Displays available Fuel values";
	public static final String NONROAD_EQUIPMENT_SECTORS_TOOLTIP = "Displays available Sectors values";
	public static final String NONROAD_EQUIPMENT_SELECTIONS_TOOLTIP = "Displays chosen Fuel/Sector combination values";
	
	public static final String FUEL_WIZARD_CHANGE_VALUE_TOOLTIP = "Enter only valid numeric data for fuel property selected";
	
	public static final String DATABASE_NAME_VALIDATION_MESSAGE = "Database name is invalid: must only contain contain letters, numbers, and underscores.";
	public static final String SERVER_NAME_VALIDATION_MESSAGE = "Server must be valid name; e.g. localhost, 127.0.0.1, etc.";

	public static final String ONROAD_FUELS_TOOLTIP = "Displays available Fuel values";
	public static final String ONROAD_SOURCE_USE_TYPES_TOOLTIP = "Displays available Source Use Type values";
	public static final String ONROAD_SELECTIONS_TOOLTIP = "Displays chosen Fuel/Source Use Type combination values";
	
}