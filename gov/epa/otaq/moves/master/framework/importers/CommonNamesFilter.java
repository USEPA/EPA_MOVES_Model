/**************************************************************************************************
 * @(#)CommonNamesFilter.java 
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework.importers;

import java.util.*;
import java.sql.*;
import gov.epa.otaq.moves.common.*;

/**
 * Filters SQL results using common column names against an ImporterManager.
 * 
 * @author		Wesley Faler
 * @version		2013-11-29
**/
public class CommonNamesFilter implements ISQLFilter {
	/** Filter types keyed by common names **/
	private static TreeMapIgnoreCase filterTypesByCommonName = null;

	/** Importer Manager for this filter **/
	ImporterManager manager;
	/** True if the column names should be checked against common names **/
	boolean shouldCheckCommonNames = true;
	/** Column names in the order they appear in the query results **/
	String[] columnNames;
	/** Column names to check and their ImporterManager.FILTER_XXXX types, if any. **/
	TreeMapIgnoreCase filterTypesByName = new TreeMapIgnoreCase();
	/** Filter types, if any, for each column in the order they appear in the query results **/
	String[] filterTypes;

	/**
	 * Constructor
	 * @param managerToUse manager for this filter
	**/
	public void CommonNamesFilter(ImporterManager managerToUse) {
		manager = managerToUse;
	}

	/**
	 * Constructor
	 * @param managerToUse manager for this filter
	 * @param shouldCheckCommonNamesToUse True if the column names should be checked
	 * against common names.
	**/
	public CommonNamesFilter(ImporterManager managerToUse,
			boolean shouldCheckCommonNamesToUse) {
		manager = managerToUse;
		shouldCheckCommonNames = shouldCheckCommonNamesToUse;

		if(filterTypesByCommonName == null) {
			filterTypesByCommonName = new TreeMapIgnoreCase();
			buildFilterTypesByCommonName();
		}
	}

	/** Create and populate filterTypesByCommonName **/
	private void buildFilterTypesByCommonName() {
		String[] nameFilterPairs = {
			"countyID", ImporterManager.FILTER_COUNTY,
			"dayID", ImporterManager.FILTER_DAY,
			"fuelTypeID", ImporterManager.FILTER_FUEL,
			"fuelFormulationID", ImporterManager.FILTER_FUEL_FORMULATION,
			"fuelYearID", ImporterManager.FILTER_FUEL_YEAR,
			"hourID", ImporterManager.FILTER_HOUR,
			"monthID", ImporterManager.FILTER_MONTH,
			"monthGroupID", ImporterManager.FILTER_MONTH_GROUP,
			"sourceTypeID", ImporterManager.FILTER_SOURCE,
			"stateID", ImporterManager.FILTER_STATE,
			"yearID", ImporterManager.FILTER_YEAR,
			"regionID", ImporterManager.FILTER_FUEL_REGION
		};

		for(int i=0;i<nameFilterPairs.length;i+=2) {
			filterTypesByCommonName.put(nameFilterPairs[i+0],nameFilterPairs[i+1]);
		}
	}

	/**
	 * Set the filter type, if any, for a column.
	 * @param columnName column name
	 * @param importerFilterType ImporterManager.FILTER_XXXX type or null if no filtering
	 * should be done.
	**/
	public void setFilter(String columnName, String importerFilterType) {
		filterTypesByName.put(columnName,importerFilterType);
	}

	/**
	 * Called after a query has been opened to determine if it should be used at all.
	 * @param metaData column names and types for the results
	 * @return true if the results should be iterated, calling canUseRow for each.
	**/
	public boolean onOpen(ResultSetMetaData metaData) throws Exception {
		// Build columnNames and filterTypes
		columnNames = new String[metaData.getColumnCount()];
		filterTypes = new String[columnNames.length];
		for(int i=0;i<columnNames.length;i++) {
			columnNames[i] = metaData.getColumnName(i+1);
			filterTypes[i] = null;
		}
		if(shouldCheckCommonNames) {
			for(int i=0;i<columnNames.length;i++) {
				if(filterTypesByCommonName.containsKey(columnNames[i])) {
					filterTypes[i] = (String)filterTypesByCommonName.get(columnNames[i]);
				}
			}
		}
		// Apply any overrides, which may include explicit null's
		for(int i=0;i<columnNames.length;i++) {
			if(filterTypesByName.containsKey(columnNames[i])) {
				filterTypes[i] = (String)filterTypesByName.get(columnNames[i]);
			}
		}
		return true;
	}

	/**
	 * Called before each row to determine its use.
	 * @param rs results positioned on the current row
	 * @return ture if the row should be used, false if it should be ignored
	**/
	public boolean canUseRow(ResultSet rs) throws Exception {
		for(int i=0;i<filterTypes.length;i++) {
			if(filterTypes[i] != null) {
				if(!manager.doesInclude(rs,i+1,filterTypes[i])) {
					return false;
				}
			}
		}
		return true;
	}

	/**
	 * Called after the query has been completed, either successfully,  with an
	 * error, or because onOpen returned false.
	**/
	public void onClose() {
		// Nothing to do here
	}
}
