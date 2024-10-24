/**************************************************************************************************
 * @(#)InputDataManager.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import java.io.*;
import java.util.*;
import java.sql.*;
import java.lang.*;

/**
 * Moves data from the pristine databases (National, County, and Emission Rate default) to
 * temporary databases (Execution Location Database, Execution Emission Rate Database) used for a
 * single simulation run . The InputDataManager uses the Execution RunSpec criteria to filter
 * the data moved, there by reducing the size of the temporary databases and decreasing the run
 * time.
 *
 * @author		Wesley Faler
 * @author		Sarah Luo, ERG
 * @author		Don Smith
 * @author		Mitch C.
 * @author		Ed Glover William Aikman Mods for NO NO2 SO2
 * @author		Tim Hull
 * @author 		John Covey - Task 1806 changes
 * @version 	2018-03-20
**/
public class InputDataManager {
	/** When copying tables, indicates whether missing tables not in the source
	 * database are allowed and should not cause an error that causes the merge to fail.
	**/
	static boolean allowMissingTables = true;

	/** Table-by-table filter settings **/
	public static String NONROAD_TABLE_FILTER_FILE_NAME = "NonroadTableFilter.csv";

	/** Maximum length of SQL that any portion of a WHERE clause is allowed to be **/
	static final int MAX_SINGLE_CLAUSE_SQL_LENGTH = 7500;

	/** Random number generator for generating uncertainty values **/
	static Random uncertaintyGenerator = new Random();

	/** Folder used for temporary uncertainty files **/
	static File temporaryFolderPath = null;

	/** Active merge session, if startMergeSession() has been used **/
	static MergeSession mergeSession = null;

	/**
	 * Validate that required tables are present. Does not check column names and types.
	 * This uses SchemaInspector.
	 * @param db The database connection to test.
	 * @return Boolean true if the specified database contains Default schema.
	**/
	static public boolean isDefaultSchemaPresent(Connection db) {
		String defaultDatabaseName , dbName ;
		SystemConfiguration scfg = SystemConfiguration.theSystemConfiguration ;

		defaultDatabaseName =
				scfg.databaseSelections[MOVESDatabaseType.DEFAULT.getIndex()].databaseName ;

		try {
			dbName = db.getCatalog() ;
			allowMissingTables = dbName.equalsIgnoreCase( defaultDatabaseName ) ? false : true ;

			return SchemaInspector.isMOVESSchemaPresent(db,false,allowMissingTables );
		} catch( Exception ex ) {
			/*
			System. out.println("Unable to check schema: " + ex.toString());
			ex.printStackTrace();
			*/
			return false ;
		}
	}

	/**
	 * Validate that required tables are present. Does not check column names and types.
	 * This uses SchemaInspector.
	 * @param db The database connection to test.
	 * @return Boolean true if the specified database contains Default schema.
	**/
	/*
	static public boolean isNonRoadDefaultSchemaPresent(Connection db) {
		if(!CompilationFlags.USE_NONROAD) {
			return false;
		}
		String defaultDatabaseName , dbName ;
		SystemConfiguration scfg = SystemConfiguration.theSystemConfiguration ;

		defaultDatabaseName =
				scfg.databaseSelections[MOVESDatabaseType.NRDEFAULT.getIndex()].databaseName ;

		try {
			dbName = db.getCatalog() ;
			allowMissingTables = dbName.equalsIgnoreCase( defaultDatabaseName ) ? false : true ;

			return SchemaInspector.isNonRoadSchemaPresent(db,false,allowMissingTables );
		} catch( Exception ex ) {
			//System. out.println("Unable to check schema: " + ex.toString());
			//ex.printStackTrace();
			return false ;
		}
	}
	*/

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's TimeSpan year selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForYears(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		boolean isFirst = true;
		for(Iterator<Integer> iter =
				ExecutionRunSpec.theExecutionRunSpec.years.iterator();
				iter.hasNext(); ) {
			Integer year = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += year.intValue();
			isFirst = false;
			// Force each year to be in a separate clause so as to restrict data quantities
			// moved in each query.  This speeds up MySQL
			sql += ")";
			sqls.add(sql);
			sql = new String();
			isFirst = true;
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause for fuelYearIDs within the years of the current runspec.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForFuelYears(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		boolean isFirst = true;
		for(Iterator<Integer> iter =
				ExecutionRunSpec.theExecutionRunSpec.fuelYears.iterator();
				iter.hasNext(); ) {
			Integer fuelYear = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += fuelYear.intValue();
			isFirst = false;
			// Force each fuel year to be in a separate clause so as to restrict data quantities
			// moved in each query.  This speeds up MySQL
			sql += ")";
			sqls.add(sql);
			sql = new String();
			isFirst = true;
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a modelYearID WHERE clause based on the ExecutionRunSpec's TimeSpan year selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForModelYears(String columnName) {
		// Because model years will overlap if done for each calendar year, they must
		// be OR'd together in one big query. For calendar years 2010, 2012, and 2000:
		// ( (modelYearID <= 2010 and modelYearID >= 1980)
		//   or (modelYearID <= 2012 and modelYearID >= 1982)
		//   or (modelYearID <= 2000 and modelYearID >= 1970) )
		// It is believed the above SQL will be smaller than explicitly listing all
		// model years.
		Vector<String> sqls = new Vector<String>();
		String sql = "(";
		boolean hasData = false;
		for(Iterator<Integer> iter =
				ExecutionRunSpec.theExecutionRunSpec.years.iterator();
				iter.hasNext(); ) {
			Integer year = (Integer)iter.next();
			if(hasData) {
				sql += " or ";
			}
			sql += "(" + columnName + "<=" + year + " and " + columnName + ">=" + (year.intValue() - 40) + ")";
			hasData = true;
		}
		if(hasData) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Check a table for presence of data.
	 * @param destination database connection
	 * @param tableName table to check
	 * @return true if there is any data present in the table
	**/
	static boolean hasExistingData(Connection destination, String tableName) {
		String sql = "select * from " + tableName + " limit 1";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(destination,sql);
			return query.rs.next();
		} catch(Exception e) {
			Logger.logError(e,"Unable to check for data in " + tableName + " with " + sql);
			return false;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Builds a WHERE clause based on (fuelRegionID, fuelYearID, monthGroupID) combinations
	 * that exist in the destination database's fuelSupply table.  A clause is created
	 * for each countyID.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForFuelSupply(Connection destination) {
		//System.out.println("buildSQLWhereClauseForFuelSupply...");
		/*
		Example clause, one clause per fuelRegionID:
			not (fuelRegionID=12345 and (
					(fuelYearID=2010 and monthGroupID in (1,2,3))
				or	(fuelYearID=2011 and monthGroupID in (2,4))
				))
		*/
		Vector<String> sqls = new Vector<String>();
		String searchSQL = "select distinct fuelRegionID, fuelYearID, monthGroupID"
				+ " from fuelSupply"
				+ " order by fuelRegionID, fuelYearID, monthGroupID";
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		try {
			// If all counties have the same fuel years and month groups, a fast
			// SQL statement can be used.
			boolean hasData = false;
			boolean allCountiesMatch = true;

			query.open(destination,searchSQL);
			String priorFuelRegionID = "";
			String countyParameters = "";
			String referenceParameters = "";
			while(query.rs.next()) {
				hasData = true;
				String fuelRegionID = query.rs.getString("fuelRegionID");
				String fuelYearID = query.rs.getString("fuelYearID");
				String monthGroupID = query.rs.getString("monthGroupID");
				if(priorFuelRegionID.length() > 0 && !priorFuelRegionID.equals(fuelRegionID)) {
					if(referenceParameters.length() <= 0) {
						referenceParameters = countyParameters;
					} else if(!referenceParameters.equals(countyParameters)) {
						allCountiesMatch = false;
						break;
					}
					countyParameters = "";
				}
				priorFuelRegionID = fuelRegionID;
				countyParameters += fuelYearID + "|" + monthGroupID + "-";
			}
			query.close();
			if(!hasData) {
				return sqls;
			}

			priorFuelRegionID = "";
			query.open(destination,searchSQL);
			String wholeClause = "";
			if(allCountiesMatch) {
				TreeSet<String> fuelRegionIDs = new TreeSet<String>();
				TreeSet<String> fuelYearIDs = new TreeSet<String>();
				TreeSet<String> monthGroupIDs = new TreeSet<String>();
				while(query.rs.next()) {
					fuelRegionIDs.add(query.rs.getString("fuelRegionID"));
					fuelYearIDs.add(query.rs.getString("fuelYearID"));
					monthGroupIDs.add(query.rs.getString("monthGroupID"));
				}
				wholeClause = "(not (fuelRegionID in (" + StringUtilities.getCSV(fuelRegionIDs) + ")"
						+ " and fuelYearID in (" + StringUtilities.getCSV(fuelYearIDs) + ")"
						+ " and monthGroupID in (" + StringUtilities.getCSV(monthGroupIDs) + ")))";
			} else {
				String priorFuelYearID = "";
				String monthGroupIDs = "";
				String clause = "";
				while(query.rs.next()) {
					String fuelRegionID = query.rs.getString("fuelRegionID");
					String fuelYearID = query.rs.getString("fuelYearID");
					String monthGroupID = query.rs.getString("monthGroupID");
					if(!fuelRegionID.equals(priorFuelRegionID)) {
						if(clause.length() > 0) {
							wholeClause = addToWhereClause(wholeClause,clause + monthGroupIDs + ")))");
							// sqls.add(clause + monthGroupIDs + ")))");
							// System.out.println(clause + monthGroupIDs + ")))");
						}
						priorFuelRegionID = fuelRegionID;
						priorFuelYearID = fuelYearID;
						monthGroupIDs = monthGroupID;
						clause = "not (fuelRegionID=" + fuelRegionID + " and (fuelYearID=" + fuelYearID + " and monthGroupID in (";
					} else if(!fuelYearID.equals(priorFuelYearID)) {
						clause += monthGroupIDs + ")) or (fuelYearID=" + fuelYearID + " and monthGroupID in (";
						monthGroupIDs = monthGroupID;
						priorFuelYearID = fuelYearID;
					} else {
						monthGroupIDs += "," + monthGroupID;
					}
				}
				if(clause.length() > 0) {
					wholeClause = addToWhereClause(wholeClause,clause + monthGroupIDs + ")))");
					// sqls.add(clause + monthGroupIDs + ")))");
					// System.out.println(clause + monthGroupIDs + ")))");
				}
			}
			sqls.add(wholeClause);
		} catch(Exception e) {
			Logger.logError(e,"Unable to build fuel supply clauses using: " + searchSQL);
		} finally {
			query.onFinally();
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's TimeSpan month selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForMonths(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		boolean isFirst = true;
		for(Iterator<Integer> iter =
				ExecutionRunSpec.theExecutionRunSpec.months.iterator();
				iter.hasNext(); ) {
			Integer month = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += month.intValue();
			isFirst = false;
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's TimeSpan day selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForDays(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		boolean isFirst = true;
		for(Iterator<Integer> iter =
				ExecutionRunSpec.theExecutionRunSpec.days.iterator();
				iter.hasNext(); ) {
			Integer day = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += day.intValue();
			isFirst = false;
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's TimeSpan hour selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForHours(String columnName) {
		Vector<String> sqls = new Vector<String>();
		//TreeSet hours = new TreeSet();
		String sql = "";
		boolean isFirst = true;
		for(Iterator<Integer> iter =
				ExecutionRunSpec.theExecutionRunSpec.hours.iterator();
				iter.hasNext(); ) {
			Integer hour = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += hour.intValue();
			isFirst = false;
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's GeographicSelection link selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForLinks(String columnName) {
		Vector<String> sqls = new Vector<String>();
		TreeSet<Integer> linkIDs = new TreeSet<Integer>();
		for(Iterator<ExecutionLocation> iter =
				ExecutionRunSpec.theExecutionRunSpec.executionLocations.iterator();
				iter.hasNext(); ) {
			ExecutionLocation location = (ExecutionLocation)iter.next();
			linkIDs.add(Integer.valueOf(location.linkRecordID));
		}
		String sql = "";
		boolean isFirst = true;
		for(Iterator<Integer> iter=linkIDs.iterator();iter.hasNext();) {
			Integer id = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += id;
			isFirst = false;
			if(sql.length() >= MAX_SINGLE_CLAUSE_SQL_LENGTH) {
				sql += ")";
				sqls.add(sql);
				sql = new String();
				isFirst = true;
			}
		}
		if(sql.length() > 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's GeographicSelection zone selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForZones(String columnName) {
		Vector<String> sqls = new Vector<String>();
		TreeSet<Integer> zoneIDs = new TreeSet<Integer>();
		String sql = "";
		for(Iterator<ExecutionLocation> iter =
				ExecutionRunSpec.theExecutionRunSpec.executionLocations.iterator();
				iter.hasNext(); ) {
			ExecutionLocation location = (ExecutionLocation)iter.next();
			zoneIDs.add(Integer.valueOf(location.zoneRecordID));
		}
		boolean isFirst = true;
		for(Iterator<Integer> iter=zoneIDs.iterator();iter.hasNext();) {
			Integer id = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += id;
			isFirst = false;
			if(sql.length() >= MAX_SINGLE_CLAUSE_SQL_LENGTH) {
				sql += ")";
				sqls.add(sql);
				sql = new String();
				isFirst = true;
			}
		}
		if(sql.length() > 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's PollutantProcessAssociations
	 * pollutant selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForPollutants(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		boolean isFirst = true;

		if(ExecutionRunSpec.theExecutionRunSpec.getOutputVMTData()
				&& !ExecutionRunSpec.theExecutionRunSpec.doesHaveDistancePollutantAndProcess()) {
			// Add THC to the pollutants that we bring over
			Pollutant p = Pollutant.findByID(1);
			if(p != null) {
				isFirst = false;
				sql += columnName + " IN (" + p.databaseKey;
			}
		}

		for(Iterator<PollutantProcessAssociation> iter =
				ExecutionRunSpec.theExecutionRunSpec.pollutantProcessAssociations.iterator();
				iter.hasNext(); ) {
			PollutantProcessAssociation selection = (PollutantProcessAssociation)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += selection.pollutant.databaseKey;
			isFirst = false;
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's PollutantProcessAssociations
	 * emission process selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForProcesses(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		TreeSet<EmissionProcess> processes = new TreeSet<EmissionProcess>();
		boolean isFirst = true;

		if(ExecutionRunSpec.theExecutionRunSpec.getOutputVMTData()
				&& !ExecutionRunSpec.theExecutionRunSpec.doesHaveDistancePollutantAndProcess()) {
			// Add Running Exhaust to the processes that we bring over
			EmissionProcess p = EmissionProcess.findByID(1);
			if(p != null) {
				isFirst = false;
				sql += columnName + " IN (" + p.databaseKey;
			}
		}

		for(Iterator<PollutantProcessAssociation> iter =
				ExecutionRunSpec.theExecutionRunSpec.pollutantProcessAssociations.iterator();
				iter.hasNext(); ) {
			PollutantProcessAssociation selection = (PollutantProcessAssociation)iter.next();
			if(!processes.contains(selection.emissionProcess)) {
				if(isFirst) {
					sql += columnName + " IN (";
				} else {
					sql += ",";
				}
				sql += selection.emissionProcess.databaseKey;
				isFirst = false;
				processes.add(selection.emissionProcess);
			}
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's GeographicSelection
	 * county selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForCounties(String columnName) {
		Vector<String> sqls = new Vector<String>();
		TreeSet<Integer> countyIDs = new TreeSet<Integer>();
		for(Iterator<ExecutionLocation> iter =
				ExecutionRunSpec.theExecutionRunSpec.executionLocations.iterator();
				iter.hasNext(); ) {
			ExecutionLocation location = (ExecutionLocation)iter.next();
			countyIDs.add(Integer.valueOf(location.countyRecordID));
		}
		boolean isFirst = true;;
		String sql = "";
		for(Iterator<Integer> iter=countyIDs.iterator();iter.hasNext();) {
			Integer id = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += id;
			isFirst = false;
			if(sql.length() >= MAX_SINGLE_CLAUSE_SQL_LENGTH) {
				sql += ")";
				sqls.add(sql);
				sql = new String();
				isFirst = true;
			}
		}
		if(countyIDs.size() != 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the regions of ExecutionRunSpec's GeographicSelection
	 * county selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForRegions(String columnName) {
		Vector<String> sqls = new Vector<String>();
		boolean isFirst = true;;
		String sql = "";
		for(Iterator<Integer> iter=ExecutionRunSpec.theExecutionRunSpec.regions.iterator();iter.hasNext();) {
			Integer id = (Integer)iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += id;
			isFirst = false;
			if(sql.length() >= MAX_SINGLE_CLAUSE_SQL_LENGTH) {
				sql += ")";
				sqls.add(sql);
				sql = new String();
				isFirst = true;
			}
		}
		if(ExecutionRunSpec.theExecutionRunSpec.regions.size() != 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's GeographicSelection
	 * state selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForStates(String columnName) {
		Vector<String> sqls = new Vector<String>();
		TreeSet<Integer> stateIDs = new TreeSet<Integer>();
		for(Iterator<ExecutionLocation> iter =
				ExecutionRunSpec.theExecutionRunSpec.executionLocations.iterator();
				iter.hasNext(); ) {
			ExecutionLocation location = (ExecutionLocation)iter.next();
			stateIDs.add(Integer.valueOf(location.stateRecordID));
		}
		String sql = "";
		boolean isFirst = true;
		for(Iterator<Integer> iter=stateIDs.iterator();iter.hasNext();) {
			Integer id = (Integer)iter.next();
			if(isFirst) {
				sql = columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += id;
			isFirst = false;
			if(sql.length() >= MAX_SINGLE_CLAUSE_SQL_LENGTH) {
				sql += ")";
				sqls.add(sql);
				sql = new String();
				isFirst = true;
			}
		}
		if(stateIDs.size() != 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the RunSpec's HourDayIDs
	 * selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForHourDayIDs(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		ResultSet results = null;
		Connection defaultDB = null;
		TreeSet<Integer> hourDayIDs = new TreeSet<Integer>();
		boolean isFirst = true;
		try {
			defaultDB = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.DEFAULT);
			for(Iterator<Integer> d =
					ExecutionRunSpec.theExecutionRunSpec.days.iterator();
					d.hasNext(); ) {
				Integer day = (Integer)d.next();
				for(Iterator<Integer> h =
						ExecutionRunSpec.theExecutionRunSpec.hours.iterator();
						h.hasNext(); ) {
					Integer hour = (Integer)h.next();
					if(sql.length()==0) {
						sql = "SELECT hourDayID FROM HourDay WHERE ";
					} else {
						sql += " OR ";
					}
					sql += "(dayID=" + day + " AND hourID=" + hour + ")";
				}
			}
			if(sql !="") {
				results = SQLRunner.executeQuery(defaultDB,sql);
				hourDayIDs.clear();
				while(results.next()) {
					hourDayIDs.add(Integer.valueOf(results.getInt(1)));
				}
				sql = "";
				for(Iterator<Integer> iter = hourDayIDs.iterator();iter.hasNext();) {
					Integer hourDayIDInteger = (Integer) iter.next();
					if(isFirst) {
						sql += columnName + " IN (";
					} else {
						sql += ",";
					}
					sql +=  hourDayIDInteger.intValue();
					isFirst = false;
				}
			}
		} catch(Exception e) {
			/** @explain A connection to the default database could not be established. **/
			Logger.logError(e,"Unable to get the Default Database connection in"
					+ " InputDataManager for HourDayIDs.");
			return null;
		} finally {
			if(results != null) {
				try {
					results.close();
				} catch(Exception e) {
					// Nothing can be done here
				}
				results = null;
			}
			if(defaultDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
						defaultDB);
				defaultDB = null;
			}
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's Road Types
	 * selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForRoadTypes(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		boolean isFirst = true;
		for(Iterator<RoadType> iter =
				ExecutionRunSpec.theExecutionRunSpec.getRoadTypes().iterator();
				iter.hasNext(); ) {
			RoadType roadTypeSelection = (RoadType)iter.next();
			Integer roadTypeInteger = Integer.valueOf(roadTypeSelection.roadTypeID);
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql += roadTypeInteger.intValue();
			isFirst = false;
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's Pollutant - Process
	 * selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForPollutantProcessIDs(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = columnName + " < 0"; // always bring in representing pollutant/processes
		boolean isFirst = true;

		if(ExecutionRunSpec.theExecutionRunSpec.getOutputVMTData()
				&& !ExecutionRunSpec.theExecutionRunSpec.doesHaveDistancePollutantAndProcess()) {
			// Add THC/RunningExhaust to the pollutants that we bring over
			isFirst = false;
			sql += " OR " + columnName + " IN (101";
		}

		Connection defaultDB = null;
		try {
			defaultDB = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.DEFAULT);
			for(Iterator<PollutantProcessAssociation> iter =
					ExecutionRunSpec.theExecutionRunSpec.pollutantProcessAssociations.iterator();
					iter.hasNext(); ) {
				PollutantProcessAssociation selection = (PollutantProcessAssociation)iter.next();
				int polProcessID = selection.getDatabaseKey(defaultDB);
				if(isFirst) {
					sql += " OR " + columnName + " IN (";
				} else {
					sql += ",";
				}
				sql += polProcessID;
				
				// NonECNonSO4PM (120) is silently made from NonECPM (118), so include 120 whenever 118 is needed.
				if((int)(polProcessID / 100) == 118) {
					int newValue = 120*100 + (polProcessID % 100);
					sql += "," + newValue;
				}
				isFirst = false;
			}
		} catch(Exception e) {
			/** @explain A connection to the default database could not be established. **/
			Logger.logError(e,"Unable to get the Default Database connection in"
					+ " InputDataManager for PollutantProcessIDs.");
			return null;
		} finally {
			if(defaultDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
						defaultDB);
				defaultDB = null;
			}
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's Source
	 * Use Type selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForSourceUseTypes(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		TreeSet<Integer> sourceTypes = new TreeSet<Integer>();
		for(Iterator<OnRoadVehicleSelection> iter =
				ExecutionRunSpec.theExecutionRunSpec.getOnRoadVehicleSelections().iterator();
				iter.hasNext(); ) {
			OnRoadVehicleSelection selection = (OnRoadVehicleSelection)iter.next();
			Integer sourceTypeInteger = Integer.valueOf(selection.sourceTypeID);
			if(!sourceTypes.contains(sourceTypeInteger)) {
				if(sourceTypes.size() == 0) {
					sql += columnName + " IN (";
				} else {
					sql += ",";
				}
				sql += sourceTypeInteger.intValue();
				sourceTypes.add(sourceTypeInteger);
			}
		}
		if(sourceTypes.size() != 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's NonRoad Source Use Type selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForNonRoadSourceUseTypes(String columnName) {
		Vector<String> sqls = new Vector<String>();
		// Find the source types that relate through NRSCC and NREquipmentType
		// based upon fuel/sector combinations in the RunSpec.
		String clauses = "";
		for(Iterator<OffRoadVehicleSelection> iter =
				ExecutionRunSpec.theExecutionRunSpec.getOffRoadVehicleSelections().iterator();
				iter.hasNext(); ) {
			OffRoadVehicleSelection selection = iter.next();
			String t = "(et.sectorID=" + selection.sectorID + " and s.fuelTypeID=" + selection.fuelTypeID + ")";
			if(clauses.length() > 0) {
				clauses += " or ";
			}
			clauses += t;
		}
		String sql = "select distinct sourceTypeID"
			+ " from NRSourceUseType sut"
			+ " inner join NRSCC s on (s.SCC=sut.SCC)"
			+ " inner join NREquipmentType et on (et.NREquipTypeID=s.NREquipTypeID)"
			+ " where (" + clauses + ")";
		Connection nrDefaultDatabase = null;
		String resultSQL = "";
		boolean isFirst = true;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			nrDefaultDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.DEFAULT);
			isFirst = true;
			query.open(nrDefaultDatabase,sql);
			while(query.rs.next()) {
				if(isFirst) {
					resultSQL += columnName + " IN (";
				} else {
					resultSQL += ",";
				}
				resultSQL += query.rs.getString("sourceTypeID");
				isFirst = false;
			}
			query.close();
		} catch(Exception e) {
			//do nothing
		} finally {
			query.onFinally();
			if(nrDefaultDatabase != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
						nrDefaultDatabase);
				nrDefaultDatabase = null;
			}
		}
		if(!isFirst) {
			resultSQL += ")";
			sqls.add(resultSQL);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's Source Fuel
	 * Type selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForFuelTypes(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		TreeSet<Integer> fuelTypes = new TreeSet<Integer>();
		for(Iterator<OnRoadVehicleSelection> iter =
				ExecutionRunSpec.theExecutionRunSpec.getOnRoadVehicleSelections().iterator();
				iter.hasNext(); ) {
			OnRoadVehicleSelection selection = (OnRoadVehicleSelection)iter.next();
			Integer fuelTypeInteger = Integer.valueOf(selection.fuelTypeID);
			if(!fuelTypes.contains(fuelTypeInteger)) {
				if(fuelTypes.size() == 0) {
					sql += columnName + " IN (";
				} else {
					sql += ",";
				}
				sql +=  fuelTypeInteger.intValue();
				fuelTypes.add(fuelTypeInteger);
			}
		}
		for(Iterator<OffRoadVehicleSelection> iter =
				ExecutionRunSpec.theExecutionRunSpec.getOffRoadVehicleSelections().iterator();
				iter.hasNext(); ) {
			OffRoadVehicleSelection selection = iter.next();
			Integer fuelTypeInteger = Integer.valueOf(selection.fuelTypeID);
			if(!fuelTypes.contains(fuelTypeInteger)) {
				if(fuelTypes.size() == 0) {
					sql += columnName + " IN (";
				} else {
					sql += ",";
				}
				sql +=  fuelTypeInteger.intValue();
				fuelTypes.add(fuelTypeInteger);
			}
		}
		if(fuelTypes.size() != 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's Source Fuel
	 * Type selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForNonroadFuelTypes(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		TreeSet<Integer> fuelTypes = new TreeSet<Integer>();
		for(Iterator<OffRoadVehicleSelection> iter =
				ExecutionRunSpec.theExecutionRunSpec.getOffRoadVehicleSelections().iterator();
				iter.hasNext(); ) {
			OffRoadVehicleSelection selection = iter.next();
			Integer fuelTypeInteger = Integer.valueOf(selection.fuelTypeID);
			if(!fuelTypes.contains(fuelTypeInteger)) {
				if(fuelTypes.size() == 0) {
					sql += columnName + " IN (";
				} else {
					sql += ",";
				}
				sql +=  fuelTypeInteger.intValue();
				fuelTypes.add(fuelTypeInteger);
			}
		}
		if(fuelTypes.size() != 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's
	 * Month Group ID selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForMonthGroupIDs(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		TreeSet<Integer> monthGroupIDs = new TreeSet<Integer>();
		Connection defaultDB = null;
		ResultSet results = null;
		boolean isFirst = true;
		String monthIDs[] = new String[12];
		ExecutionRunSpec executionRunSpec = ExecutionRunSpec.theExecutionRunSpec;
		if(executionRunSpec.monthGroups.size() > 0) {
			monthGroupIDs.addAll(executionRunSpec.monthGroups);
		} else {
			try {
				defaultDB = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.DEFAULT);
				monthGroupIDs.clear();
				sql = "";
				for(Iterator<Integer> iter =
						ExecutionRunSpec.theExecutionRunSpec.months.iterator();
						iter.hasNext(); ) {
					Integer month = (Integer)iter.next();
					sql = "SELECT DISTINCT monthGroupID FROM monthOfAnyYear WHERE "
							+ " monthID =" + month.intValue();
					results = SQLRunner.executeQuery(defaultDB, sql);
					while (results.next()) {
						monthGroupIDs.add(Integer.valueOf(results.getInt("monthGroupID")));
					}
				}
			} catch(Exception e) {
				//do nothing
			} finally {
				if(results != null) {
					try {
						results.close();
					} catch(Exception e) {
						// Nothing can be done here
					}
					results = null;
				}
				if(defaultDB != null) {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
							defaultDB);
					defaultDB = null;
				}
			}
		}
		sql = "";
		isFirst = true;
		for(Iterator<Integer> iter = monthGroupIDs.iterator();iter.hasNext();) {
			Integer monthGroupIDInteger = (Integer) iter.next();
			if(isFirst) {
				sql += columnName + " IN (";
			} else {
				sql += ",";
			}
			sql +=  monthGroupIDInteger.intValue();
			isFirst = false;
		}

		if(monthGroupIDs.size() != 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's
	 * Fuel Sub Type selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForFuelSubTypes(String columnName) {
		Vector<String> sqls = new Vector<String>();
		Connection defaultDatabase = null;
		String sql = "";
		boolean isFirst = true;

		// E85 air toxics requires E10 information (fuelsubtypeID=12)
		boolean forceE10 = false;
		for(Iterator<OnRoadVehicleSelection> iter =
				ExecutionRunSpec.theExecutionRunSpec.getOnRoadVehicleSelections().iterator();
				iter.hasNext(); ) {
			OnRoadVehicleSelection selection = (OnRoadVehicleSelection)iter.next();
			if(selection.fuelTypeID == 5) {
				forceE10 = true;
				break;
			}
		}
		if(!forceE10) {
			for(Iterator<OffRoadVehicleSelection> iter =
					ExecutionRunSpec.theExecutionRunSpec.getOffRoadVehicleSelections().iterator();
					iter.hasNext(); ) {
				OffRoadVehicleSelection selection = iter.next();
				if(selection.fuelTypeID == 5) {
					forceE10 = true;
					break;
				}
			}
		}

		try {
			defaultDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.DEFAULT);
			sql = "SELECT DISTINCT fuelSubTypeID FROM FuelSubType";
			Vector<String> fuelTypeClauses = buildSQLWhereClauseForFuelTypes("FuelTypeID");
			if(fuelTypeClauses.size() > 0) {
				sql += " WHERE " + fuelTypeClauses.get(0);
			}
			isFirst = true;
			ResultSet results = SQLRunner.executeQuery(defaultDatabase, sql);
			sql = "";
			if(forceE10) {
				isFirst = false;
				sql += columnName + " IN (12";
			}
			while(results.next()) {
				if(isFirst) {
					sql += columnName + " IN (";
				} else {
					sql += ",";
				}
				sql +=  results.getString("fuelSubTypeID");
				isFirst = false;
			}
			results.close();
		} catch(Exception e) {
			//do nothing
		} finally {
			if(defaultDatabase != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
						defaultDatabase);
				defaultDatabase = null;
			}
		}
		if(!isFirst) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's
	 * Nonroad Fuel Sub Type selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForNonroadFuelSubTypes(String columnName) {
		Vector<String> sqls = new Vector<String>();
		Connection defaultDatabase = null;
		String sql = "";
		boolean isFirst = true;

		try {
			defaultDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.DEFAULT);
			sql = "SELECT DISTINCT fuelSubTypeID FROM nrFuelSubType";
			Vector<String> fuelTypeClauses = buildSQLWhereClauseForNonroadFuelTypes("FuelTypeID");
			if(fuelTypeClauses.size() > 0) {
				sql += " WHERE " + fuelTypeClauses.get(0);
			}
			isFirst = true;
			ResultSet results = SQLRunner.executeQuery(defaultDatabase, sql);
			sql = "";
			while(results.next()) {
				if(isFirst) {
					sql += columnName + " IN (";
				} else {
					sql += ",";
				}
				sql +=  results.getString("fuelSubTypeID");
				isFirst = false;
			}
			results.close();
		} catch(Exception e) {
			//do nothing
		} finally {
			if(defaultDatabase != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
						defaultDatabase);
				defaultDatabase = null;
			}
		}
		if(!isFirst) {
			sql += ")";
			//Logger.log(LogMessageCategory.INFO,"buildSQLWhereClauseForNonroadFuelSubTypes=" + sql);
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's NonRoad sector selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForSectors(String columnName) {
		Vector<String> sqls = new Vector<String>();
		String sql = "";
		TreeSet<Integer> sectors = new TreeSet<Integer>();
		for(Iterator<OffRoadVehicleSelection> iter =
				ExecutionRunSpec.theExecutionRunSpec.getOffRoadVehicleSelections().iterator();
				iter.hasNext(); ) {
			OffRoadVehicleSelection selection = iter.next();
			Integer sectorInteger = Integer.valueOf(selection.sectorID);
			if(!sectors.contains(sectorInteger)) {
				if(sectors.size() == 0) {
					sql += columnName + " IN (";
				} else {
					sql += ",";
				}
				sql +=  sectorInteger.intValue();
				sectors.add(sectorInteger);
			}
		}
		if(sectors.size() != 0) {
			sql += ")";
			sqls.add(sql);
		}
		return sqls;
	}

	/**
	 * Builds a WHERE clause based on the ExecutionRunSpec's NonRoad sector selections.
	 * @param columnName The name of the column to use in the WHERE clause.  As appropriate,
	 * this could also include a table name qualifier.
	 * @return Vector of String objects each with the SQL WHERE clause as String.
	**/
	static Vector<String> buildSQLWhereClauseForNonRoadEquipmentTypes(String columnName) {
		Vector<String> sqls = new Vector<String>();
		Connection nrDefaultDatabase = null;
		String resultSQL = "";
		String sql = "";
		boolean isFirst = true;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			nrDefaultDatabase = DatabaseConnectionManager.checkOutConnection(
					MOVESDatabaseType.DEFAULT);
			sql = "SELECT DISTINCT NREquipTypeID, surrogateID FROM NREquipmentType";
			Vector<String> sectorClauses = buildSQLWhereClauseForSectors("sectorID");
			if(sectorClauses.size() > 0) {
				sql += " WHERE " + sectorClauses.get(0);
			}
			isFirst = true;
			query.open(nrDefaultDatabase,sql);
			sql = "";
			TreeSet<Integer> equipmentTypes = new TreeSet<Integer>();
			TreeSet<Integer> surrogateTypes = new TreeSet<Integer>();
			String surrogateSQL = "";
			while(query.rs.next()) {
				if(isFirst) {
					resultSQL += columnName + " IN (";
				} else {
					resultSQL += ",";
				}
				int equipmentTypeID = query.rs.getInt("NREquipTypeID");
				equipmentTypes.add(Integer.valueOf(equipmentTypeID));
				resultSQL +=  equipmentTypeID;
				isFirst = false;

				int surrogateID = query.rs.getInt("surrogateID");
				if(surrogateID > 0) {
					Integer ts = Integer.valueOf(surrogateID);
					if(!equipmentTypes.contains(ts) && !surrogateTypes.contains(ts)) {
						surrogateTypes.add(ts);
						if(surrogateSQL.length() > 0) {
							surrogateSQL += ",";
						}
						surrogateSQL += surrogateID;
					}
				}
			}
			query.close();

			if(surrogateSQL.length() > 0) {
				sql = "select distinct NREquipTypeID"
						+ " from NREquipmentType"
						+ " where NREquipTypeID in (" + surrogateSQL + ")";
				query.open(nrDefaultDatabase,sql);
				while(query.rs.next()) {
					int equipmentTypeID = query.rs.getInt("NREquipTypeID");
					Integer t = Integer.valueOf(equipmentTypeID);
					if(!equipmentTypes.contains(t)) {
						equipmentTypes.add(t);
						resultSQL += "," + equipmentTypeID;
					}
				}
				query.close();
			}
		} catch(Exception e) {
			//do nothing
		} finally {
			query.onFinally();
			if(nrDefaultDatabase != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT,
						nrDefaultDatabase);
				nrDefaultDatabase = null;
			}
		}
		if(!isFirst) {
			resultSQL += ")";
			sqls.add(resultSQL);
		}
		return sqls;
	}

	/**
	 * Default constructor
	**/
	public InputDataManager() {
	}

	/**
	 * Handles connection checkout/checkin and calls the next merge implementation which actually
	 * performs the merge. This moves a subset of data from the default database to the execution
	 * database based on the ExecutionRunSpec.
	 * @throws InterruptedException If the active thread is interrupted
	 * @throws SQLException If any SQL errors occur.
	 * @throws IOException If there is an error during any temporary file operations
	**/
	public static void merge() throws InterruptedException, SQLException, IOException, Exception {
		merge(ModelDomain.NATIONAL_ALLOCATION);
	}

	/**
	 * Handles connection checkout/checkin and calls the next merge implementation which actually
	 * performs the merge. This moves a subset of data from the NonRoad default database to the execution
	 * database based on the ExecutionRunSpec.
	 * @throws InterruptedException If the active thread is interrupted
	 * @throws SQLException If any SQL errors occur.
	 * @throws IOException If there is an error during any temporary file operations
	**/
	public static void mergeNonRoad() throws InterruptedException, SQLException, IOException, Exception {
		if(CompilationFlags.USE_NONROAD) {
			mergeNonRoad(ModelDomain.NATIONAL_ALLOCATION);
		}
	}

	/**
	 * Handles connection checkout/checkin and calls the next merge implementation which actually
	 * performs the merge. This moves a subset of data from the default database to the execution
	 * database based on the ExecutionRunSpec.
	 * @param domain model domain, required since not all tables are needed for each domain.
	 * domain may be null, in which case it is treated as a national-level domain.
	 * @throws InterruptedException If the active thread is interrupted
	 * @throws SQLException If any SQL errors occur.
	 * @throws IOException If there is an error during any temporary file operations
	 * @return true upon successful merge
	**/
	public static boolean merge(ModelDomain domain)
			throws InterruptedException, SQLException, IOException, Exception {
		Connection inputConnection = null;
		Connection executionConnection = null;
		
		try {
			InputDataManager inputDataManager = new InputDataManager();
			inputConnection = DatabaseConnectionManager.checkOutConnection
					(MOVESDatabaseType.DEFAULT);
			executionConnection = DatabaseConnectionManager.checkOutConnection
					(MOVESDatabaseType.EXECUTION);
			// Do not copy the default database's link table's contents when using
			// the project domain.  Doing so would cause unwanted links to be calculated.
			boolean includeLinkTable = domain != ModelDomain.PROJECT;
			// Single county and Project domains require the user to provide a complete fuel supply table
			// in a domain-level database.  As such, do not copy the fuel supply table for those domains.
			boolean includeFuelSupply = domain != ModelDomain.SINGLE_COUNTY && domain != ModelDomain.PROJECT;
			//System.out.println("merge(domain)");
			inputDataManager.merge(inputConnection, executionConnection, includeLinkTable, includeFuelSupply, true);
			return true;
		} catch (Exception e) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.ERROR,
					"Error while merging default database:" + e.toString());
			e.printStackTrace();
			return false;
		} finally {
			if (inputConnection != null) {
				DatabaseConnectionManager.checkInConnection
						(MOVESDatabaseType.DEFAULT, inputConnection);
				inputConnection = null;
			}
			if (executionConnection != null) {
				DatabaseConnectionManager.checkInConnection
						(MOVESDatabaseType.EXECUTION, executionConnection);
				executionConnection = null;
			}
		}
	}

	/**
	 * Handles connection checkout/checkin and calls the next merge implementation which actually
	 * performs the merge. This moves a subset of data from the NonRoad default database to the execution
	 * database based on the ExecutionRunSpec.
	 * @param domain model domain, required since not all tables are needed for each domain.
	 * domain may be null, in which case it is treated as a national-level domain.
	 * @throws InterruptedException If the active thread is interrupted
	 * @throws SQLException If any SQL errors occur.
	 * @throws IOException If there is an error during any temporary file operations
	 * @return true upon successful merge
	**/
	public static boolean mergeNonRoad(ModelDomain domain)
			throws InterruptedException, SQLException, IOException, Exception {
		if(!CompilationFlags.USE_NONROAD) {
			return true;
		}
		Connection inputConnection = null;
		Connection executionConnection = null;
		
		try {
			InputDataManager inputDataManager = new InputDataManager();
			inputConnection = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.DEFAULT);
			executionConnection = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
			inputDataManager.mergeNonRoad(inputConnection, executionConnection, true);
			return true;
		} catch (Exception e) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.ERROR,
					"Error while merging default database:" + e.toString());
			e.printStackTrace();
			return false;
		} finally {
			if (inputConnection != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.DEFAULT, inputConnection);
				inputConnection = null;
			}
			if (executionConnection != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, executionConnection);
				executionConnection = null;
			}			
		}
	}

	/**
	 * Handles connection checkout/checkin and calls the next merge implementation which actually
	 * performs the merge. This moves a subset of data from an input database to the execution
	 * database based on the ExecutionRunSpec.
	 * @param userDatabase an input database selected by a user
	 * @throws InterruptedException If the active thread is interrupted
	 * @throws SQLException If any SQL errors occur.
	 * @throws IOException If there is an error during any temporary file operations
	 * @return true upon a successful merge
	**/
	public static boolean merge(DatabaseSelection userDatabase) throws InterruptedException,
			SQLException, IOException, Exception {
		//System.out.println("Merging from user supplied database");
		Connection inputConnection = null;
		Connection executionConnection = null;
		
		try {
			InputDataManager inputDataManager = new InputDataManager();
			inputConnection = userDatabase.openConnection();
			executionConnection = DatabaseConnectionManager.checkOutConnection
					(MOVESDatabaseType.EXECUTION);
			//System.out.println("merge(userDatabase)");
			inputDataManager.merge(inputConnection, executionConnection, true, true, false);
			return true;
		} catch (Exception e) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.ERROR,
					"Error while merging a user database:" + e.toString());
			/**
			 * @issue Unable to import database [*]
			 * @explain A database could not be merged into the MOVESExecution database.
			 * Check the database availability.
			**/
			Logger.log(LogMessageCategory.ERROR,
					"Unable to import database " + userDatabase.databaseName);
			e.printStackTrace();
			return false;
		} finally {
			if (inputConnection != null) {
				inputConnection.close();
			}
			if (executionConnection != null) {
				DatabaseConnectionManager.checkInConnection
						(MOVESDatabaseType.EXECUTION, executionConnection);
				executionConnection = null;
			}
		}
	}

	/**
	 * Handles connection checkout/checkin and calls the next merge implementation which actually
	 * performs the merge. This moves a subset of data from an input database to the execution
	 * database based on the ExecutionRunSpec.
	 * @param userDatabase an input database selected by a user
	 * @throws InterruptedException If the active thread is interrupted
	 * @throws SQLException If any SQL errors occur.
	 * @throws IOException If there is an error during any temporary file operations
	 * @return true upon successful merge
	**/
	public static boolean mergeNonRoad(DatabaseSelection userDatabase) throws InterruptedException,
			SQLException, IOException, Exception {
		//System.out.println("Merging from user supplied NonRoad database");
		Connection inputConnection = null;
		Connection executionConnection = null;
		
		try {
			InputDataManager inputDataManager = new InputDataManager();
			inputConnection = userDatabase.openConnection();
			executionConnection = DatabaseConnectionManager.checkOutConnection
					(MOVESDatabaseType.EXECUTION);
			//System.out.println("merge(userDatabase)");
			inputDataManager.mergeNonRoad(inputConnection, executionConnection, false);
			return true;
		} catch (Exception e) {
			/** @nonissue **/
			Logger.log(LogMessageCategory.ERROR,
					"Error while merging a user database:" + e.toString());
			/**
			 * @issue Unable to import database [*]
			 * @explain A database could not be merged into the MOVESExecution database.
			 * Check the database availability.
			**/
			Logger.log(LogMessageCategory.ERROR,
					"Unable to import database " + userDatabase.databaseName);
			e.printStackTrace();
			return false;
		} finally {
			if (inputConnection != null) {
				inputConnection.close();
			}
			if (executionConnection != null) {
				DatabaseConnectionManager.checkInConnection
						(MOVESDatabaseType.EXECUTION, executionConnection);
				executionConnection = null;
			}
		}
	}

	/**
	 *  Make base tables used for generating input uncertainty in successive iterations.
	**/
	public static void makeBaseUncertaintyInput() {
		Connection executionConnection = null;
		String[] commands = {
			// CrankcaseEmissionRatio
			"DROP TABLE IF EXISTS CrankcaseEmissionRatioSample",
			"CREATE TABLE CrankcaseEmissionRatioSample SELECT polProcessID, minModelYearID, maxModelYearID,"
					+ " sourceTypeID,fuelTypeID,COALESCE(crankcaseRatio,0) AS crankcaseRatio,"
					+ " COALESCE(crankcaseRatioCV,0) as crankcaseRatioCV"
					+ " FROM CrankcaseEmissionRatio",

			// PM10EmissionRatio
			"DROP TABLE IF EXISTS PM10EmissionRatioSample",
			"CREATE TABLE PM10EmissionRatioSample SELECT polProcessID, "
					+ " sourceTypeID,fuelTypeID,COALESCE(PM10PM25Ratio,0) AS PM10PM25Ratio,"
					+ " COALESCE(PM10PM25RatioCV,0) as PM10PM25RatioCV"
					+ " FROM PM10EmissionRatio",

			// EmissionRate
			"DROP TABLE IF EXISTS EmissionRateSample",
			"CREATE TABLE EmissionRateSample SELECT sourceBinID, polProcessID, opModeID,"
					+ " COALESCE(meanBaseRate,0) AS meanBaseRate, COALESCE(meanBaseRateCV,0)"
					+ " AS meanBaseRateCV, COALESCE(meanBaseRateIM,0) AS meanBaseRateIM,"
					+ " COALESCE(meanBaseRateIMCV,0) AS meanBaseRateIMCV, dataSourceID "
					+ " FROM EmissionRate",

			// EmissionRateByAge
			"DROP TABLE IF EXISTS EmissionRateByAgeSample",
			"CREATE TABLE EmissionRateByAgeSample SELECT sourceBinID, polProcessID, opModeID,"
					+ " ageGroupID, COALESCE(meanBaseRate,0) AS meanBaseRate,"
					+ " COALESCE(meanBaseRateCV,0) AS meanBaseRateCV, COALESCE(meanBaseRateIM,0)"
					+ " AS meanBaseRateIM, COALESCE(meanBaseRateIMCV,0) AS meanBaseRateIMCV,"
					+ " dataSourceID"
					+ " FROM EmissionRateByAge",

			// SulfateEmissionRate
			"DROP TABLE IF EXISTS SulfateEmissionRateSample",
			"CREATE TABLE SulfateEmissionRateSample SELECT polProcessID, fuelTypeID, "
					+ "modelYearGroupID, COALESCE(meanBaseRate,0) AS meanBaseRate,"
					+ " COALESCE(meanBaseRateCV,0) AS meanBaseRateCV, "
					+ " dataSourceId"
					+ " FROM SulfateEmissionRate",

			// NONO2Ratio
			"DROP TABLE IF EXISTS NONO2RatioSample",
			"CREATE TABLE NONO2RatioSample SELECT polProcessID, sourceTypeID, "
					+ " fuelTypeID, modelYearGroupID, COALESCE(NOxRatio,0) AS NOxRatio,"
					+ " COALESCE(NOxRatioCV,0) AS NOxRatioCV, "
					+ " dataSourceId"
					+ " FROM NONO2Ratio",

			// methaneTHCRatio
			"drop table if exists methaneTHCRatio",
			"create table methaneTHCRatioSample"
					+ " select processID, fuelTypeID, sourceTypeID, modelYearGroupID, ageGroupID,"
					+ " coalesce(CH4THCRatio,0) as CH4THCRatio, coalesce(CH4THCRatioCV,0) as CH4THCRatioCV"
					+ " from methaneTHCRatio",

			// CumTVVCoeffs
			"drop table if exists CumTVVCoeffsSample",
			"create table CumTVVCoeffsSample"
					+ " select regClassID, modelYearGroupID, ageGroupID, polProcessID,"
					+ " coalesce(tvvTermA,0.0) as tvvTermA,"
					+ " coalesce(tvvTermB,0.0) as tvvTermB,"
					+ " coalesce(tvvTermC,0.0) as tvvTermC,"
					+ " coalesce(tvvTermACV,0.0) as tvvTermACV,"
					+ " coalesce(tvvTermBCV,0.0) as tvvTermBCV,"
					+ " coalesce(tvvTermCCV,0.0) as tvvTermCCV,"
					+ " coalesce(tvvTermAIM,0.0) as tvvTermAIM,"
					+ " coalesce(tvvTermBIM,0.0) as tvvTermBIM,"
					+ " coalesce(tvvTermCIM,0.0) as tvvTermCIM,"
					+ " coalesce(tvvTermAIMCV,0.0) as tvvTermAIMCV,"
					+ " coalesce(tvvTermBIMCV,0.0) as tvvTermBIMCV,"
					+ " coalesce(tvvTermCIMCV,0.0) as tvvTermCIMCV"
					+ " from CumTVVCoeffs",

			// TemperatureAdjustment
			"drop table if exists TemperatureAdjustmentSample",
			"create table TemperatureAdjustmentSample"
					+ " select polProcessID, fuelTypeID,"
					+ " coalesce(tempAdjustTermA,0.0) as tempAdjustTermA,"
					+ " coalesce(tempAdjustTermB,0.0) as tempAdjustTermB,"
					+ " coalesce(tempAdjustTermC,0.0) as tempAdjustTermC,"
					+ " coalesce(tempAdjustTermACV,0.0) as tempAdjustTermACV,"
					+ " coalesce(tempAdjustTermBCV,0.0) as tempAdjustTermBCV,"
					+ " coalesce(tempAdjustTermCCV,0.0) as tempAdjustTermCCV"
					+ " from TemperatureAdjustment",

			// StartTempAdjustment
			"drop table if exists StartTempAdjustmentSample",
			"create table StartTempAdjustmentSample"
					+ " select polProcessID, fuelTypeID, opmodeID, modelYearGroupID,"
					+ " coalesce(tempAdjustTermA,0.0) as tempAdjustTermA,"
					+ " coalesce(tempAdjustTermB,0.0) as tempAdjustTermB,"
					+ " coalesce(tempAdjustTermC,0.0) as tempAdjustTermC,"
					+ " coalesce(tempAdjustTermACV,0.0) as tempAdjustTermACV,"
					+ " coalesce(tempAdjustTermBCV,0.0) as tempAdjustTermBCV,"
					+ " coalesce(tempAdjustTermCCV,0.0) as tempAdjustTermCCV"
					+ " from StartTempAdjustment",

			// FullACAdjustment
			"drop table if exists FullACAdjustmentSample",
			"create table FullACAdjustmentSample"
					+ " select sourceTypeID, polProcessID, opmodeID,"
					+ " coalesce(fullACAdjustment,1.0) as fullACAdjustment,"
					+ " coalesce(fullACAdjustmentCV,0.0) as fullACAdjustmentCV"
					+ " from FullACAdjustment",
/*
			// FuelAdjustment
			"drop table if exists FuelAdjustmentSample",
			"create table FuelAdjustmentSample"
					+ " select  polProcessID, sourceTypeID, fuelMYGroupID, fuelFormulationID,"
					+ " coalesce(fuelAdjustment,1.0) as fuelAdjustment,"
					+ " coalesce(fuelAdjustmentCV,0.0) as fuelAdjustmentCV,"
					+ " coalesce(fuelAdjustmentGPA,1.0) as fuelAdjustmentGPA,"
					+ " coalesce(fuelAdjustmentGPACV,0.0) as fuelAdjustmentGPACV"
					+ " from FuelAdjustment",
*/
			// RefuelingFactors
			"drop table if exists RefuelingFactorsSample",
			"create table RefuelingFactorsSample"
					+ " select fuelTypeID, defaultFormulationID,"
					+ " vaporTermA, vaporTermB, vaporTermC, vaporTermD, vaporTermE, vaporTermF,"
					+ " vaporLowTLimit, vaporHighTLimit, tankTDiffLimit,"
					+ " minimumRefuelingVaporLoss, refuelingSpillRate, refuelingSpillRateCV,"
					+ " displacedVaporRateCV"
					+ " from RefuelingFactors"
		};
		String sql = "";
		try {
			executionConnection = DatabaseConnectionManager.checkOutConnection
				(MOVESDatabaseType.EXECUTION);

			for(int i=0;i<commands.length;i++) {
				sql = commands[i];
				SQLRunner.executeSQL(executionConnection, sql);
			}

			// Folder for uncertainty files.
			temporaryFolderPath = FileUtilities.createTemporaryFolder(
					null, "InputDataManagerTemp");
			if(temporaryFolderPath == null) {
				/**
				 * @explain A directory needed for uncertainty calculations could not be created.
				**/
				Logger.log(LogMessageCategory.ERROR,
					"Create temporary folder failed, unable to perform uncertainty calculations.");
			}

			// Use the same seed for each run.
			uncertaintyGenerator.setSeed(8675309);
		} catch (Exception e) {
			/**
			 * @issue Exception occurred on 'makeBaseUncertaintyInput' [*]
			 * @explain An error occurred while generating randomized data for uncertainty
			 * analysis.
			**/
			Logger.log(LogMessageCategory.ERROR,
					"Exception occurred on 'makeBaseUncertaintyInput' " + e);
		} finally {
			if (executionConnection != null) {
				DatabaseConnectionManager.checkInConnection
					(MOVESDatabaseType.EXECUTION, executionConnection);
				executionConnection = null;
			}
		}
	}

	/**
	 *  Simulate uncertainty in input by randomizing the input values.
	 * @param nextIterationID The next iteration to be performed.
	**/
	public static void simulateUncertaintyInInput(int nextIterationID) {
		if(temporaryFolderPath == null) {
			return;
		}

		//CrankcaseEmissionRatio
		randomizeTableData("polProcessID, minModelYearID, maxModelYearID, sourceTypeID, fuelTypeID,"
				+ " crankcaseRatio, crankcaseRatioCV",
				"CrankcaseEmissionRatioSample", 5,
				"TRUNCATE CrankcaseEmissionRatio", "CrankcaseEmissionRatio (polProcessID, "
				+ "minModelYearID, maxModelYearID, sourceTypeID, fuelTypeID, crankcaseRatio, crankcaseRatioCV)",1);
		//PM10EmissionRatio
		randomizeTableData("polProcessID, sourceTypeID, fuelTypeID,"
				+ " PM10PM25Ratio, PM10PM25RatioCV",
				"PM10EmissionRatioSample", 3,
				"TRUNCATE PM10EmissionRatio", "PM10EmissionRatio (polProcessID, "
				+ "sourceTypeID, fuelTypeID, PM10PM25Ratio, PM10PM25RatioCV)",1);
		//EmissionRate
		randomizeTableData(" * ", "EmissionRateSample", 3, "TRUNCATE EmissionRate",
		        " emissionRate (sourceBinID, polProcessID, opModeID,"
				+ " meanBaseRate, meanBaseRateCV, "
				+ " meanBaseRateIM, meanBaseRateIMCV, dataSourceID)",2);
		//EmissionRateByAge
		randomizeTableData(" * ", "EmissionRateByAgeSample ", 4,
				"TRUNCATE EmissionRateByAge", "emissionRateByAge (sourceBinID, polProcessID,"
				+ " opModeID, ageGroupID, meanBaseRate, meanBaseRateCV, meanBaseRateIM,"
				+ " meanBaseRateIMCV, dataSourceID)",2);
		//SulfateEmissionRate
		randomizeTableData("polProcessID, fuelTypeID, modelYearGroupID, meanBaseRate, meanBaseRateCV, dataSourceId",
				"SulfateEmissionRateSample", 3,
				"TRUNCATE SulfateEmissionRate", "SulfateEmissionRate (polProcessID, fuelTypeID,"
				+ " modelYearGroupID, meanBaseRate, meanBaseRateCV, dataSourceId)",1);
		//NONO2Ratio
		randomizeTableData("polProcessID, sourceTypeID, fuelTypeID, modelYearGroupID, NOxRatio,"
				+ " NOxRatioCV, dataSourceId",
				"NONO2RatioSample", 4,
				"TRUNCATE NONO2RatioSample", "NONO2Ratio (polProcessID, "
				+ "sourceTypeID, fuelTypeID, modelYearGroupID, NOxRatio, NOxRatioCV, dataSourceId)",1);
		// methaneTHCRatio
		randomizeTableData("processID, fuelTypeID, sourceTypeID, modelYearGroupID, ageGroupID, CH4THCRatio, CH4THCRatioCV",
				"methaneTHCRatioSample",
				5,
				"truncate methaneTHCRatio",
				"methaneTHCRatio (processID, fuelTypeID, sourceTypeID, modelYearGroupID, ageGroupID, CH4THCRatio, CH4THCRatioCV",
				1);
		//CumTVVCoeffs
		randomizeTableData("regClassID, modelYearGroupID, ageGroupID, polProcessID,"
					+ "tvvTermA, tvvTermACV, tvvTermAIM, tvvTermAIMCV,"
					+ "tvvTermB, tvvTermBCV, tvvTermBIM, tvvTermBIMCV,"
					+ "tvvTermC, tvvTermCCV, tvvTermCIM, tvvTermCIMCV",
				"CumTVVCoeffsSample",
				4,
				"truncate CumTVVCoeffs",
				"CumTVVCoeffs (regClassID, modelYearGroupID, ageGroupID, polProcessID,"
					+ "tvvTermA, tvvTermACV, tvvTermAIM, tvvTermAIMCV,"
					+ "tvvTermB, tvvTermBCV, tvvTermBIM, tvvTermBIMCV,"
					+ "tvvTermC, tvvTermCCV, tvvTermCIM, tvvTermCIMCV)",
				6);
		//TemperatureAdjustment
		randomizeTableData("polProcessID, fuelTypeID, "
					+ " tempAdjustTermA, tempAdjustTermACV,"
					+ " tempAdjustTermB, tempAdjustTermBCV,"
					+ " tempAdjustTermC, tempAdjustTermCCV",
				"TemperatureAdjustmentSample",
				2,
				"truncate TemperatureAdjustment",
				"TemperatureAdjustment (polProcessID, fuelTypeID, "
					+ " tempAdjustTermA, tempAdjustTermACV,"
					+ " tempAdjustTermB, tempAdjustTermBCV,"
					+ " tempAdjustTermC, tempAdjustTermCCV)",
				3);
		//StartTempAdjustment
		randomizeTableData("polProcessID, fuelTypeID, "
					+ " modelYearGroupID, opModeID, "
					+ " tempAdjustTermA, tempAdjustTermACV,"
					+ " tempAdjustTermB, tempAdjustTermBCV,"
					+ " tempAdjustTermC, tempAdjustTermCCV",
				"StartTempAdjustmentSample",
				4,
				"truncate StartTempAdjustment",
				"StartTempAdjustment (polProcessID, fuelTypeID, "
					+ " modelYearGroupID, opModeID, "
					+ " tempAdjustTermA, tempAdjustTermACV,"
					+ " tempAdjustTermB, tempAdjustTermBCV,"
					+ " tempAdjustTermC, tempAdjustTermCCV)",
				3);
		//FullACAdjustment
		randomizeTableData("sourceTypeID, polProcessID, opModeID,"
				+ " fullACAdjustment, fullACAdjustmentCV",
				"FullACAdjustmentSample", 3,
				"TRUNCATE FullACAdjustment", "FullACAdjustment (sourceTypeID, polProcessID,"
				+ " opModeID, fullACAdjustment, fullACAdjustmentCV)",1);
/*
		//FuelAdjustment
		randomizeTableData("polProcessID, sourceTypeID, fuelMYGroupID, fuelFormulationID,"
				+ " fuelAdjustment, fuelAdjustmentCV, fuelAdjustmentGPA, fuelAdjustmentGPACV",
				"FuelAdjustmentSample", 4,
				"TRUNCATE FuelAdjustment", "FuelAdjustment (polProcessID, sourceTypeID,"
				+ " fuelMYGroupID, fuelFormulationID, "
				+ " fuelAdjustment, fuelAdjustmentCV, "
				+ " fuelAdjustmentGPA, fuelAdjustmentGPACV)",2);
*/
		// RefuelingFactors
		randomizeTableData("fuelTypeID, defaultFormulationID,"
					+ " vaporTermA, vaporTermB, vaporTermC, vaporTermD, vaporTermE, vaporTermF,"
					+ " vaporLowTLimit, vaporHighTLimit, tankTDiffLimit,"
					+ " minimumRefuelingVaporLoss, refuelingSpillRate, refuelingSpillRateCV,"
					+ " displacedVaporRateCV",
					"RefuelingFactorsSample", 12,
					"TRUNCATE RefuelingFactors",
					"RefuelingFactors (fuelTypeID, defaultFormulationID,"
					+ " vaporTermA, vaporTermB, vaporTermC, vaporTermD, vaporTermE, vaporTermF,"
					+ " vaporLowTLimit, vaporHighTLimit, tankTDiffLimit,"
					+ " minimumRefuelingVaporLoss, refuelingSpillRate, refuelingSpillRateCV,"
					+ " displacedVaporRateCV)",
					1);
		/*
		// ATRatioGas1
		randomizeTableData("polProcessID,fuelMYGroupID,fuelTypeID,sourceTypeID,FuelFormulationID,"
				+ " ATRatio,ATRatioCV",
				"ATRatioGas1", 5,
				"TRUNCATE ATRatioGas1",
				"ATRatioGas1 (polProcessID,fuelMYGroupID,fuelTypeID,sourceTypeID,FuelFormulationID,"
				+ " ATRatio,ATRatioCV)",1);
		*/
		// ATRatioGas2
		randomizeTableData("polProcessID,sourceTypeID,fuelSubtypeID,"
				+ " ATRatio,ATRatioCV",
				"ATRatioGas2", 3,
				"TRUNCATE ATRatioGas2",
				"ATRatioGas2 (polProcessID,sourceTypeID,fuelSubtypeID,"
				+ " ATRatio,ATRatioCV)",1);
		// ATRatioNonGas
		randomizeTableData("polProcessID,sourceTypeID,fuelSubtypeID,"
				+ " ATRatio,ATRatioCV",
				"ATRatioNonGas", 3,
				"TRUNCATE ATRatioNonGas",
				"ATRatioNonGas (polProcessID,sourceTypeID,fuelSubtypeID,"
				+ " ATRatio,ATRatioCV)",1);

		OutputEmissionsBreakdownSelection outputEmissionsBreakdownSelection =
				ExecutionRunSpec.theExecutionRunSpec.getOutputEmissionsBreakdownSelection();
		if(outputEmissionsBreakdownSelection.keepSampledData) {
			int	activeRunID = MOVESEngine.theInstance.getActiveRunID();

			Connection executionConnection = null;
			Connection outputConnection = null;
			File exportTableFilePath = null;
			try {
				exportTableFilePath = new File(temporaryFolderPath, "copyTable");
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				String fileName = exportTableFilePath.getCanonicalPath().replace('\\', '/');

				executionConnection = DatabaseConnectionManager.checkOutConnection(
							MOVESDatabaseType.EXECUTION);
				outputConnection = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.OUTPUT);

				String sql = null;

				// Do CrankcaseEmissionRatio Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}
				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " polProcessID, minModelYearID, maxModelYearID, sourceTypeID, fuelTypeID, crankcaseRatio"
						+ " INTO OUTFILE '" + fileName + "' FROM CrankcaseEmissionRatio";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS CrankcaseEmissionRatioSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
					    + " polProcessID INT NOT NULL,"
					    + " minModelYearID SMALLINT NOT NULL,"
					    + " maxModelYearID SMALLINT NOT NULL,"
						+ " sourceTypeID SMALLINT NOT NULL,"
					    + " fuelTypeID SMALLINT NOT NULL,"
					    + " crankcaseRatio FLOAT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE CrankcaseEmissionRatioSample "
						+ "(MOVESRunID, iterationID, polProcessID, minModelYearID, maxModelYearID, sourceTypeID, "
						+ " fuelTypeID, crankcaseRatio)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do PM10EmissionRatio Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}
				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " polProcessID, sourceTypeID, fuelTypeID, PM10PM25Ratio"
						+ " INTO OUTFILE '" + fileName + "' FROM PM10EmissionRatio";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS PM10EmissionRatioSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
					    + " polProcessID INT NOT NULL,"
						+ " sourceTypeID SMALLINT NOT NULL,"
					    + " fuelTypeID SMALLINT NOT NULL,"
					    + " PM10PM25Ratio FLOAT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE PM10EmissionRatioSample "
						+ "(MOVESRunID, iterationID, polProcessID, sourceTypeID, "
						+ " fuelTypeID, PM10PM25Ratio)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do EmissionRate table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}
				sql = "SELECT " + activeRunID + ", " + nextIterationID
						+ ", sourceBinID, polProcessID, opModeID, meanBaseRate, meanBaseRateIM"
						+ " INTO OUTFILE '" + fileName + "' FROM emissionRate";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS EmissionRateSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " sourceBinID BIGINT NOT NULL,"
					    + " polProcessID INT NOT NULL,"
					    + " opModeID SMALLINT NOT NULL,"
					    + " meanBaseRate FLOAT NULL,"
					    + " meanBaseRateIM FLOAT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE EmissionRateSample "
						+ "(MOVESRunID, iterationID, sourceBinID, polProcessID, opModeID,"
						+ " meanBaseRate, meanBaseRateIM)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do EmissionRateByAge table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID
						+ ", sourceBinID, polProcessID, opModeID, ageGroupID, meanBaseRate,"
						+ " meanBaseRateIM INTO OUTFILE '" + fileName + "' FROM emissionRateByAge";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS EmissionRateByAgeSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " sourceBinID BIGINT NOT NULL,"
					    + " polProcessID INT NOT NULL,"
					    + " opModeID SMALLINT NOT NULL,"
						+ " ageGroupID SMALLINT NOT NULL,"
					    + " meanBaseRate FLOAT NULL,"
					    + " meanBaseRateIM FLOAT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE EmissionRateByAgeSample ("
						+ " MOVESRunID, iterationID, sourceBinID, polProcessID, opModeID,"
						+ " ageGroupID, meanBaseRate, meanBaseRateIM)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do SulfateEmissionRate table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID
						+ ", polProcessID, fuelTypeID, modelYearGroupID, meanBaseRate"
						+ " INTO OUTFILE '" + fileName + "' FROM SulfateEmissionRate";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS SulfateEmissionRateSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
					    + " polProcessID INT NOT NULL,"
					    + " fuelTypeID SMALLINT NOT NULL,"
						+ " modelYearGroupID INT NOT NULL,"
					    + " meanBaseRate FLOAT NULL,"
					    + " dataSourceId SMALLINT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE SulfateEmissionRateSample "
						+ "(MOVESRunID, iterationID, polProcessID, fuelTypeID, modelYearGroupID,"
						+ " meanBaseRate, dataSourceId)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do NONO2Ratio table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID
						+ ", polProcessID, sourceTypeID, fuelTypeID, modelYearGroupID, NOxRatio, dataSourceId "
						+ " INTO OUTFILE '" + fileName + "' FROM NONO2Ratio";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS NONO2Sample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
					    + " polProcessID INT NOT NULL,"
					    + " sourceTypeID SMALLINT NOT NULL,"
					    + " fuelTypeID SMALLINT NOT NULL,"
						+ " modelYearGroupID INT NOT NULL,"
					    + " NOxRatio FLOAT NULL,"
					    + " dataSourceId SMALLINT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE NONO2RatioSample "
						+ "(MOVESRunID, iterationID, polProcessID, sourceTypeID, fuelTypeID, "
						+ " modelYearGroupID, NOxRatio, dataSourceId)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do methaneTHCRatio table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID
						+ ", processID, fuelTypeID, sourceTypeID, modelYearGroupID, ageGroupID, CH4THCRatio"
						+ " INTO OUTFILE '" + fileName + "' FROM methaneTHCRatio";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS methaneTHCRatioSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " processID smallint(6) NOT NULL DEFAULT '0',"
						+ " fuelTypeID smallint(6) NOT NULL DEFAULT '0',"
						+ " sourceTypeID smallint(6) NOT NULL DEFAULT '0',"
						+ " modelYearGroupID int(11) NOT NULL DEFAULT '0',"
						+ " ageGroupID smallint(6) NOT NULL DEFAULT '0',"
						+ " CH4THCRatio double DEFAULT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE methaneTHCRatioSample ("
						+ " MOVESRunID, iterationID"
						+ ", processID, fuelTypeID, sourceTypeID, modelYearGroupID, ageGroupID, CH4THCRatio"
						+ ")";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do CumTVVCoeffs table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID
						+ ", regClassID, modelYearGroupID, ageGroupID, polProcessID"
						+ ", tvvTermA, tvvTermB, tvvTermC"
						+ ", tvvTermAIM, tvvTermBIM, tvvTermCIM"
						+ " INTO OUTFILE '" + fileName + "' FROM CumTVVCoeffs";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS CumTVVCoeffsSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " regClassID smallint(6) NOT NULL,"
						+ " modelYearGroupID int(11) NOT NULL,"
						+ " ageGroupID smallint(6) NOT NULL,"
						+ " polProcessID int NOT NULL,"
						+ " tvvTermA float NULL,"
						+ " tvvTermB float NULL,"
						+ " tvvTermC float NULL,"
						+ " tvvTermAIM float NULL,"
						+ " tvvTermBIM float NULL,"
						+ " tvvTermCIM float NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE CumTVVCoeffsSample ("
						+ " MOVESRunID, iterationID"
						+ ", regClassID, modelYearGroupID, ageGroupID, polProcessID"
						+ ", tvvTermA, tvvTermB, tvvTermC"
						+ ", tvvTermAIM, tvvTermBIM, tvvTermCIM"
						+ ")";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do TemperatureAdjustment Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " polProcessID, fuelTypeID, "
						+ " tempAdjustTermA, tempAdjustTermB, tempAdjustTermC"
						+ " INTO OUTFILE '" + fileName + "' FROM TemperatureAdjustment";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS TemperatureAdjustmentSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
					    + " polProcessID INT NOT NULL,"
					    + " fuelTypeID SMALLINT NOT NULL,"
					    + " tempAdjustTermA FLOAT NULL,"
					    + " tempAdjustTermB FLOAT NULL,"
					    + " tempAdjustTermC FLOAT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE TemperatureAdjustmentSample "
						+ "(MOVESRunID, iterationID, polProcessID, fuelTypeID,"
						+ " tempAdjustTermA, tempAdjustTermB, tempAdjustTermC)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do StartTempAdjustment Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}
				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " polProcessID, fuelTypeID, modelYearGroupID, opModeID,"
						+ " tempAdjustTermA, tempAdjustTermB, tempAdjustTermC"
						+ " INTO OUTFILE '" + fileName + "' FROM StartTempAdjustment";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS StartTempAdjustmentSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
					    + " polProcessID INT NOT NULL,"
					    + " fuelTypeID SMALLINT NOT NULL,"
					    + " modelYearGroupID INTEGER NOT NULL,"
					    + " opModeID SMALLINT NOT NULL,"
					    + " tempAdjustTermA FLOAT NULL,"
					    + " tempAdjustTermB FLOAT NULL,"
					    + " tempAdjustTermC FLOAT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE StartTempAdjustmentSample "
						+ "(MOVESRunID, iterationID, polProcessID, fuelTypeID,"
						+ " modelYearGroupID, opModeID,"
						+ " tempAdjustTermA, tempAdjustTermB, tempAdjustTermC)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do FullACAdjustment Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}
				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " sourceTypeID, polProcessID, opModeID, fullACAdjustment"
						+ " INTO OUTFILE '" + fileName + "' FROM FullACAdjustment";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS FullACAdjustmentSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " sourceTypeID SMALLINT NOT NULL,"
					    + " polProcessID INT NOT NULL,"
					    + " opModeID SMALLINT NOT NULL,"
					    + " fullACAdjustment FLOAT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE FULLACAdjustmentSample "
						+ "(MOVESRunID, iterationID, sourceTypeID, polProcessID, opModeID,"
						+ " fullACAdjustment)";
				SQLRunner.executeSQL(outputConnection, sql);
/*
				// Do FuelAdjustment Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}
				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " polProcessID, sourceTypeID, fuelMYGroupID, fuelFormulationID,"
						+ " fuelAdjustment, fuelAdjustmentGPA"
						+ " INTO OUTFILE '" + fileName + "' FROM fuelAdjustment";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS FuelAdjustmentSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
					    + " polProcessID INT NOT NULL,"
					    + " sourceTypeID SMALLINT NOT NULL,"
					    + " fuelMYGroupID INTEGER NOT NULL,"
					    + " fuelFormulationID int(11) NOT NULL,"
					    + " fuelAdjustment FLOAT NULL,"
					    + " fuelAdjustmentGPA FLOAT NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE FuelAdjustmentSample "
						+ " (MOVESRunID, iterationID,  polProcessID, sourceTypeID,"
						+ " fuelMYGroupID, fuelFormulationID, "
						+ " fuelAdjustment, fuelAdjustmentGPA)";
				SQLRunner.executeSQL(outputConnection, sql);
*/
				// Do RefuelingFactors table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID
						+ ", fuelTypeID, defaultFormulationID,"
						+ " vaporTermA, vaporTermB, vaporTermC, vaporTermD, vaporTermE, vaporTermF,"
						+ " vaporLowTLimit, vaporHighTLimit, tankTDiffLimit,"
						+ " minimumRefuelingVaporLoss, refuelingSpillRate, refuelingSpillRateCV,"
						+ " displacedVaporRateCV"
						+ " INTO OUTFILE '" + fileName + "' FROM RefuelingFactors";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS RefuelingFactorsSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " fuelTypeID           SMALLINT NOT NULL,"
						+ " defaultFormulationID SMALLINT NULL,"
						+ " vaporTermA           FLOAT NOT NULL DEFAULT 0,"
						+ " vaporTermB           FLOAT NOT NULL DEFAULT 0,"
						+ " vaporTermC           FLOAT NOT NULL DEFAULT 0,"
						+ " vaporTermD           FLOAT NOT NULL DEFAULT 0,"
						+ " vaporTermE           FLOAT NOT NULL DEFAULT 0,"
						+ " vaporTermF           FLOAT NOT NULL DEFAULT 0,"
						+ " vaporLowTLimit       FLOAT NOT NULL DEFAULT 0,"
						+ " vaporHighTLimit      FLOAT NOT NULL DEFAULT 0,"
						+ " tankTDiffLimit       FLOAT NOT NULL DEFAULT 0,"
						+ " minimumRefuelingVaporLoss FLOAT NOT NULL DEFAULT 0,"
						+ " refuelingSpillRate   FLOAT NOT NULL DEFAULT 0,"
						+ " refuelingSpillRateCV FLOAT NULL,"
						+ " displacedVaporRateCV FLOAT NULL"
						+ ")";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE RefuelingFactorsSample ("
						+ " MOVESRunID, iterationID"
						+ ", fuelTypeID, defaultFormulationID,"
						+ " vaporTermA, vaporTermB, vaporTermC, vaporTermD, vaporTermE, vaporTermF,"
						+ " vaporLowTLimit, vaporHighTLimit, tankTDiffLimit,"
						+ " minimumRefuelingVaporLoss, refuelingSpillRate, refuelingSpillRateCV,"
						+ " displacedVaporRateCV"
						+ ")";
				SQLRunner.executeSQL(outputConnection, sql);

/*
				// Do ATRatioGas1 Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " polProcessID, fuelMYGroupID, fuelTypeID, sourceTypeID, FuelFormulationID,"
						+ " ATRatio, ATRatioCV"
						+ " INTO OUTFILE '" + fileName + "' FROM ATRatioGas1";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS ATRatioGas1Sample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " polProcessID int NOT NULL default '0',"
						+ " fuelMYGroupID int(11) default NULL,"
						+ " fuelTypeID smallint(6) default NULL,"
						+ " sourceTypeID smallint(6) NOT NULL default '0',"
						+ " FuelFormulationID int(11) default NULL,"
						+ " ATRatio float default NULL,"
						+ " ATRatioCV float default NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE ATRatioGas1Sample "
						+ " (MOVESRunID, iterationID,  polProcessID, fuelMYGroupID,"
						+ " fuelTypeID, sourceTypeID, FuelFormulationID,"
						+ " ATRatio, ATRatioCV)";
				SQLRunner.executeSQL(outputConnection, sql);
*/
				// Do ATRatioGas2 Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " polProcessID, sourceTypeID, fuelSubtypeID,"
						+ " ATRatio, ATRatioCV"
						+ " INTO OUTFILE '" + fileName + "' FROM ATRatioGas2";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS ATRatioGas2Sample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " polProcessID int NOT NULL default '0',"
						+ " sourceTypeID smallint(6) NOT NULL default '0',"
						+ " fuelSubtypeID smallint(6) default NULL,"
						+ " ATRatio float default NULL,"
						+ " ATRatioCV float default NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE ATRatioGas2Sample "
						+ " (MOVESRunID, iterationID,  polProcessID, "
						+ " sourceTypeID, fuelSubtypeID,"
						+ " ATRatio, ATRatioCV)";
				SQLRunner.executeSQL(outputConnection, sql);

				// Do ATRatioNonGas Table
				if(exportTableFilePath.exists()) {
					exportTableFilePath.delete();
				}

				sql = "SELECT " + activeRunID + ", " + nextIterationID + ","
						+ " polProcessID, sourceTypeID, fuelSubtypeID,"
						+ " ATRatio, ATRatioCV"
						+ " INTO OUTFILE '" + fileName + "' FROM ATRatioNonGas";
				SQLRunner.executeSQL(executionConnection,sql);

				sql = "CREATE TABLE IF NOT EXISTS ATRatioNonGasSample ("
						+ " MOVESRunID SMALLINT UNSIGNED NOT NULL,"
						+ " iterationID SMALLINT UNSIGNED DEFAULT 1,"
						+ " polProcessID int NOT NULL default '0',"
						+ " sourceTypeID smallint(6) NOT NULL default '0',"
						+ " fuelSubtypeID smallint(6) default NULL,"
						+ " ATRatio float default NULL,"
						+ " ATRatioCV float default NULL)";
				SQLRunner.executeSQL(outputConnection,sql);

				sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE ATRatioNonGasSample "
						+ " (MOVESRunID, iterationID,  polProcessID, "
						+ " sourceTypeID, fuelSubtypeID,"
						+ " ATRatio, ATRatioCV)";
				SQLRunner.executeSQL(outputConnection, sql);
			} catch (Exception e) {
				/**
				 * @issue Exception occurred on 'simulateUncertaintyInInput' [*]
				 * @explain An error occurred while generating randomized data for uncertainty
				 * analysis.
				**/
				Logger.log(LogMessageCategory.ERROR,
						"Exception occurred on 'simulateUncertaintyInInput'" + e);
			} finally {
				if (executionConnection != null) {
					DatabaseConnectionManager.checkInConnection
							(MOVESDatabaseType.EXECUTION, executionConnection);
					executionConnection = null;
				}
				if (outputConnection != null) {
					DatabaseConnectionManager.checkInConnection
							(MOVESDatabaseType.OUTPUT, outputConnection);
					outputConnection = null;
				}
				if (exportTableFilePath != null) {
					exportTableFilePath.delete();
					exportTableFilePath = null;
				}
			}
		}
	}

	/**
	 * Randomize a column value in a table.
	 * @param sqlSelectColumns Columns to select from source table(s)
	 * @param sqlSelectFrom Source(s) to select columns from.
	 * @param dataColumnIndex Index to mean and covariance columns in select, can't be zero
	 * @param sqlPrepareDestination SQL statement to prepare destination table to receive data.
	 * @param sqlLoadDestination Destination table name and columns.
	 * @param howManyColumnSets number of dataItem/dataItemCV sets in the SQL
	**/
	static void randomizeTableData(String sqlSelectColumns, String sqlSelectFrom,
			int dataColumnIndex, String sqlPrepareDestination, String sqlLoadDestination,
			int howManyColumnSets) {
		String sql = null;
		String fileName = null;
		Connection executionConnection = null;
		BufferedReader tableReader = null;
		BufferedWriter tableWriter = null;
		File originalTableFilePath = null;
		File randomizedTableFilePath = null;

		try {
			originalTableFilePath = new File(temporaryFolderPath, "original" + System.currentTimeMillis());
			originalTableFilePath.delete();
			randomizedTableFilePath = new File(temporaryFolderPath, "randomized" + System.currentTimeMillis());
			randomizedTableFilePath.delete();

			fileName = originalTableFilePath.getCanonicalPath().replace('\\', '/');

			executionConnection = DatabaseConnectionManager.checkOutConnection(
						MOVESDatabaseType.EXECUTION);

			sql = "SELECT " + sqlSelectColumns + " INTO OUTFILE '" + fileName + "' FROM "
					+ sqlSelectFrom;
			SQLRunner.executeSQL(executionConnection,sql);

			tableReader = new BufferedReader(new FileReader(originalTableFilePath));
			tableWriter = new BufferedWriter(new OutputStreamWriter(
						new FileOutputStream(randomizedTableFilePath)));

			String columns = null;
			int linenum = 0;
			while((columns=tableReader.readLine())!=null) {
				linenum++;
				StringTokenizer tokenizer = new StringTokenizer(columns, "\t");
				int columnCount = tokenizer.countTokens();
				double dataItem = 0.0;
				int dataColumnIndexCopy = dataColumnIndex;
				int howManyColumnSetsCopy = howManyColumnSets;
				for (int column=0; column < columnCount; column++) {
					String columnValue = tokenizer.nextToken();
					if(column==0) {
						tableWriter.write(columnValue);
					} else if (column==dataColumnIndexCopy) {
						try {
							if(columnValue.equalsIgnoreCase("\\N")) {
								dataItem = 0.0;
							} else {
								dataItem = Double.valueOf(columnValue).doubleValue();
							}
						} catch(Exception e) {
							dataItem = 0.0;
						}
					} else if (column==dataColumnIndexCopy+1) {
						double dataItemCV = 0.0;
						try {
							if(columnValue.equalsIgnoreCase("\\N")) {
								dataItemCV = 0.0;
							} else {
								dataItemCV = Double.valueOf(columnValue).doubleValue();
								dataItem *=
										(1.0 + dataItemCV * uncertaintyGenerator.nextGaussian());
							}
						} catch(Exception e) {
							dataItemCV = 0.0;
						}
						tableWriter.write("\t"+Double.toString(dataItem));
						tableWriter.write("\t"+columnValue);
						howManyColumnSetsCopy--;
						if(howManyColumnSetsCopy > 0) {
							dataColumnIndexCopy = column+1;
						}
					} else {
						tableWriter.write("\t"+columnValue);
					}
				}
				//tableWriter.newLine();
				tableWriter.write("\n");
			}

			tableReader.close();
			tableReader = null;
			tableWriter.close();
			tableWriter = null;

			SQLRunner.executeSQL(executionConnection, sqlPrepareDestination);
			SQLRunner.executeSQL(executionConnection, "FLUSH TABLE " + sqlSelectFrom);

			fileName = randomizedTableFilePath.getCanonicalPath().replace('\\', '/');
			sql = "LOAD DATA INFILE '" + fileName + "' INTO TABLE " + sqlLoadDestination;
			SQLRunner.executeSQL(executionConnection, sql);
		} catch (Exception e) {
			/**
			 * @issue Exception occurred on 'randomizeTableData' [*] using [*]
			 * @explain An error occurred while generating randomized data for uncertainty
			 * analysis.
			**/
			Logger.log(LogMessageCategory.ERROR,
					"Exception occurred on 'randomizeTableData' " + e
					+ " using " + sql);
			e.printStackTrace();
			if (originalTableFilePath != null) {
				String t = "";
				try {
					t = originalTableFilePath.getCanonicalPath();
				} catch(Exception e2) {
					// Nothing can be done here
				}
				originalTableFilePath = null; // don't delete, leave for debugging
				/** @nonissue **/
				Logger.log(LogMessageCategory.ERROR,t);
			}
			if (randomizedTableFilePath != null) {
				String t = "";
				try {
					t = randomizedTableFilePath.getCanonicalPath();
				} catch(Exception e2) {
					// Nothing can be done here
				}
				randomizedTableFilePath = null; // don't delete, leave for debugging
				/** @nonissue **/
				Logger.log(LogMessageCategory.ERROR,t);
			}
		} finally {
			if (executionConnection != null) {
				DatabaseConnectionManager.checkInConnection
					(MOVESDatabaseType.EXECUTION, executionConnection);
				executionConnection = null;
			}
			if (tableReader != null) {
				try {
					tableReader.close();
				} catch (IOException exception) {
					Logger.logSqlError(exception,"Unable to process exported file contents", sql);
				}
			}
			if (tableWriter != null) {
				try {
					tableWriter.close();
				} catch (IOException exception) {
					Logger.logSqlError(exception,"Unable to process exported file contents", sql);
				}
			}
			if (originalTableFilePath != null) {
				originalTableFilePath.delete();
			}
			if (randomizedTableFilePath != null) {
				randomizedTableFilePath.delete();
			}
		}
	}

	/**
	 *  Conditionally "preaggregates" Execution database depending on RunSpec
	 *  perhaps combining all counties within states
	 *  or perhaps combining all counties within nation
	**/
	public static void preAggregateExecutionDB() {
		File nationFile = null;
		File stateFile = null;
		File yearFile = null;
		File monthFile = null;
		File dayFile = null;
		Connection executionConnection = null;

		ModelScale scale = ExecutionRunSpec.theExecutionRunSpec.getModelScale();
		if(ModelScale.MACROSCALE == scale || ModelScale.MESOSCALE_LOOKUP == scale) {
			// Aggregate based on Geographic Selection Types
			GeographicSelectionType geoType = null;
			Iterator<GeographicSelection>
					i = ExecutionRunSpec.theExecutionRunSpec.getGeographicSelections().iterator();
			if (i.hasNext()) {
				geoType = ((GeographicSelection) i.next()).type;
			}
			if (geoType == GeographicSelectionType.NATION){
				// run nation aggregation script;
				/** @nonissue **/
				Logger.log(LogMessageCategory.DEBUG,
						"Running script to preaggregate to NATION level");
				if (nationFile==null) {
					nationFile = new File ("database/PreAggNATION.sql");
				}
				try {
					executionConnection = DatabaseConnectionManager.checkOutConnection
						(MOVESDatabaseType.EXECUTION);
					DatabaseUtilities.executeScript(executionConnection,nationFile);
				} catch (Exception e) {
					/**
					 * @issue Exception occurred on 'PreAggNATION.sql'
					 * @explain An error occurred while aggregating data to the nation level
					 * before doing the simulation.
					**/
					Logger.log(LogMessageCategory.ERROR,
							"Exception occurred on 'PreAggNATION.sql'" + e);
				} finally {
					if (executionConnection != null) {
						DatabaseConnectionManager.checkInConnection
							(MOVESDatabaseType.EXECUTION, executionConnection);
						executionConnection = null;
					}
				}
			}else if (geoType == GeographicSelectionType.STATE){
				// run state aggregation script;
				/** @nonissue **/
				Logger.log(LogMessageCategory.DEBUG,
						"Running script to preaggregate to STATE level");
				if (stateFile==null) {
					stateFile = new File ("database/PreAggSTATE.sql");
				}
				try {
					executionConnection = DatabaseConnectionManager.checkOutConnection
						(MOVESDatabaseType.EXECUTION);
					DatabaseUtilities.executeScript(executionConnection,stateFile);
				} catch (Exception e) {
					/**
					 * @issue Exception occurred on 'PreAggSTATE.sql'
					 * @explain An error occurred while aggregating data to the state level
					 * before doing the simulation.
					**/
					Logger.log(LogMessageCategory.ERROR,
							"Exception occurred on 'PreAggSTATE.sql'" + e);
				} finally {
					if (executionConnection != null) {
						DatabaseConnectionManager.checkInConnection
							(MOVESDatabaseType.EXECUTION, executionConnection);
						executionConnection = null;
					}
				}
			}

			// Aggregate based on TimeSpan aggregation selection.
			TimeSpan timeSpan = ExecutionRunSpec.theExecutionRunSpec.getTimeSpan();
			if(timeSpan.aggregateBy != OutputTimeStep.HOUR) {
				/** @nonissue **/
				Logger.log(LogMessageCategory.DEBUG,
						"Running script to preaggregate to DAY level");

				if (dayFile==null) {
					dayFile = new File ("database/PreAggDAY.sql");
				}
				try {
					executionConnection = DatabaseConnectionManager.checkOutConnection
						(MOVESDatabaseType.EXECUTION);
					DatabaseUtilities.executeScript(executionConnection,dayFile);
				} catch (Exception e) {
					/**
					 * @issue Exception occurred on 'PreAggDAY.sql'
					 * @explain An error occurred while aggregating data to the day level
					 * before doing the simulation.
					**/
					Logger.log(LogMessageCategory.ERROR,
								"Exception occurred on 'PreAggDAY.sql'" + e);
				} finally {
					if (executionConnection != null) {
						DatabaseConnectionManager.checkInConnection
							(MOVESDatabaseType.EXECUTION, executionConnection);
						executionConnection = null;
					}
				}

				if(timeSpan.aggregateBy.compareTo(OutputTimeStep.MONTH)>=0) {
					/** @nonissue **/
					Logger.log(LogMessageCategory.DEBUG,
							"Running script to preaggregate to MONTH level");
					if (monthFile==null) {
						monthFile = new File ("database/PreAggMONTH.sql");
					}
					try {
						executionConnection = DatabaseConnectionManager.checkOutConnection
							(MOVESDatabaseType.EXECUTION);
						DatabaseUtilities.executeScript(executionConnection,monthFile);
					} catch (Exception e) {
						/**
						 * @issue Exception occurred on 'PreAggMONTH.sql'
						 * @explain An error occurred while aggregating data to the month level
						 * before doing the simulation.
						**/
						Logger.log(LogMessageCategory.ERROR,
								"Exception occurred on 'PreAggMONTH.sql'" + e);
					} finally {
						if (executionConnection != null) {
							DatabaseConnectionManager.checkInConnection
								(MOVESDatabaseType.EXECUTION, executionConnection);
							executionConnection = null;
						}
					}
				}

				if(timeSpan.aggregateBy.compareTo(OutputTimeStep.YEAR)>=0) {
					/** @nonissue **/
					Logger.log(LogMessageCategory.DEBUG,
							"Running script to preaggregate to YEAR level");
					if (yearFile==null) {
						yearFile = new File ("database/PreAggYEAR.sql");
					}
					try {
						executionConnection = DatabaseConnectionManager.checkOutConnection
							(MOVESDatabaseType.EXECUTION);
						DatabaseUtilities.executeScript(executionConnection,yearFile);
					} catch (Exception e) {
						/**
						 * @issue Exception occurred on 'PreAggYEAR.sql'
						 * @explain An error occurred while aggregating data to the year level
						 * before doing the simulation.
						**/
						Logger.log(LogMessageCategory.ERROR,
								"Exception occurred on 'PreAggYEAR.sql'" + e);
					} finally {
						if (executionConnection != null) {
							DatabaseConnectionManager.checkInConnection
								(MOVESDatabaseType.EXECUTION, executionConnection);
							executionConnection = null;
						}
					}
				}
			}
		}
	}
	/**
	 * Append a new clause onto a partially completed SQL WHERE clause.
	 * @param wholeWhereClause SQL WHERE clause (without the "WHERE" word) that has
	 * been previously built.
	 * @param textToAppend additional clause to be AND'd onto wholeWhereClause.  Parenthesis
	 * are used to ensure embedded OR statements do not affect the logic.
	 * @return updated wholeWhereClause
	**/
	static String addToWhereClause(String wholeWhereClause,String textToAppend) {
		if(textToAppend != null && textToAppend.length() > 0) {
			if(wholeWhereClause.length() > 0) {
				wholeWhereClause = wholeWhereClause + " AND ";
			}
			wholeWhereClause = wholeWhereClause + "(" + textToAppend + ")";
		}

		return wholeWhereClause;
	}

	/**
	 * Append a set of clauses to the master set of clauses but only if the passed set
	 * is not null and not empty.
	 * @param clauseSets Vector of Vector objects where each Vector holds String
	 * objects that are fragments of the total SQL WHERE clause
	 * @param clauses Vector of String objects with new fragments for one variable
	**/
	void addToClauseSets(Vector< Vector<String> > clauseSets,Vector<String> clauses) {
		if(clauses != null && clauses.size() > 0) {
			clauseSets.add(clauses);
		}
	}

	/**
	 * Merge from a "default" database to an "execution" database.
	 * @param source The "default" database to get data from.
	 * @param destination The "execution" database to write data to.
	 * @param includeLinkTable true if the Link table should be included
	 * @param includeFuelSupply true if the fuelSupply table should be included
	 * @param isDefaultDatabase true if the source is a default database, false for user databases
	 * @throws SQLException If there is an error during any java.sql operations.
	 * @throws IOException If there is an error during any temporary file operations
	**/
	public void merge(Connection source, Connection destination, boolean includeLinkTable, 
			boolean includeFuelSupply, boolean isDefaultDatabase)
			throws SQLException, IOException, Exception {
		//System.out.println("merge(includeFuelSupply=" + includeFuelSupply + ")");

		// make a copy of default db's svp, vpop, and age distribution tables for all runs
		// (these are used by the baserategenerator)
		if (isDefaultDatabase) {
			DatabaseUtilities.copyDefaultTableWithRename(source, destination, "samplevehiclepopulation", "samplevehiclepopulationdefault");
			DatabaseUtilities.copyDefaultTableWithRename(source, destination, "sourcetypeyear", "sourcetypeyeardefault");
			DatabaseUtilities.copyDefaultTableWithRename(source, destination, "sourcetypeagedistribution", "sourcetypeagedistributiondefault");
		}
		
		// Validate source

		if(!isDefaultSchemaPresent(source)) {
			throw new IllegalArgumentException("Source does not have default schema.");
		}

		boolean includeEmissionRates = true;
		if(isDefaultDatabase && CompilationFlags.USE_ONLY_USER_SUPPLIED_EMISSION_RATES) {
			includeEmissionRates = false;
		}

		boolean includeHotellingActivityDistribution = true;
		if(isDefaultDatabase && ModelDomain.PROJECT == ExecutionRunSpec.getRunSpec().domain) {
			includeHotellingActivityDistribution = false;
		}

		Models models = ExecutionRunSpec.theExecutionRunSpec.getModels();
		Models.ModelCombination mc = Models.evaluateModels(models);

		DatabaseMetaData dmd ;
		String tableTypes[] = new String[ 1 ] ;
		ResultSet rs ;
		String mdTableName = "" ;

		dmd = source.getMetaData() ;
		tableTypes[ 0 ] = "TABLE" ;

		boolean includeIMCoverage = includeFuelSupply;
		if(includeIMCoverage && isDefaultDatabase) {
			// If there is already data in the destination IMCoverage table, do not import
			// the default database's IMCoverage table.
			if(DatabaseUtilities.getRowCount(destination,"IMCoverage") > 0) {
				includeIMCoverage = false;
			}
		}

		boolean includeFuelUsageFraction = CompilationFlags.USE_FUELUSAGEFRACTION;
		if(includeFuelUsageFraction && isDefaultDatabase) {
			// If there is already data in the destination FuelUsageFraction table, do not import
			// the default database's FuelUsageFraction table.
			if(DatabaseUtilities.getRowCount(destination,"FuelUsageFraction") > 0) {
				includeFuelUsageFraction = false;
			}
		}
		
		boolean includeSourceTypeAgeDistribution = !isDefaultDatabase || ModelDomain.SINGLE_COUNTY != ExecutionRunSpec.getRunSpec().domain;
		boolean includeSourceTypeYear = !isDefaultDatabase || ModelDomain.SINGLE_COUNTY != ExecutionRunSpec.getRunSpec().domain;

		boolean includeHPMSVtypeYear = !isDefaultDatabase || ModelDomain.SINGLE_COUNTY != ExecutionRunSpec.getRunSpec().domain;
		if(includeHPMSVtypeYear && isDefaultDatabase) {
			// If there is already data in a destination table that supplies VMT, do not
			// import the default database's HPMSVtypeYear table.
			if(DatabaseUtilities.getRowCount(destination,"HPMSVtypeDay") > 0
					|| DatabaseUtilities.getRowCount(destination,"SourceTypeDayVMT") > 0
					|| DatabaseUtilities.getRowCount(destination,"SourceTypeYearVMT") > 0) {
				includeHPMSVtypeYear = false;
			}
		}

		/**
		 * Inner class used to identify the tables and table rows to be copied from the "default"
		 * database to the "execution" database. This class allows table rows to be filtered by
		 * criteria specified in the Execution Runspec. Rows can be filtered by <ul><li>year</li>
		 * <li>month</li><li>link</li><li>zone</li><li>county</li><li>state</li><li>pollutant,
		 * </li><li>emission process.</li><li>day</li><li>hour</li><li>hourDayID</li>
		 * <li>roadType</li><li>pollutantProcessID</li><li>sourceUseType</li><li>fuelType</li>
		 * <li>monthGroupID</li>, and <li>fuelSubType</li>.
		 * </ul>To filter rows by runspec criteria, place the
		 * table's column names for the criteria in the corresponding entries below.
		 * <p>Note that some tables contain data that must <em>not</em> be filtered. Tables with
		 * population data for a base year must not be filtered by the analysis year as the base
		 * year data is needed to grow the table data to the analysis year. Tables that contain
		 * distribution data for an entire population must not be filtered by selected members of
		 * the population as the full population data will be needed in performing calculations.
		 * (The exception to this is "Fraction" tables, where a member's fractional value of the
		 * entire population has already been calculated).</p>
		**/

		class TableToCopy {
			/** name of the table **/
			public String tableName = null;
			/** optional name of a column that refers to a year **/
			public String yearColumnName = null;
			/** optional name of a column that refers to a monthID **/
			public String monthColumnName = null;
			/** optional name of a column that refers to a linkID **/
			public String linkColumnName = null;
			/** optional name of a column that refers to a zoneID **/
			public String zoneColumnName = null;
			/** optional name of a column that refers to a countyID **/
			public String countyColumnName = null;
			/** optional name of a column that refers to a stateID **/
			public String stateColumnName = null;
			/** optional name of a column that refers to a pollutantID **/
			public String pollutantColumnName = null;
			/** optional name of a column that refers to a processID **/
			public String processColumnName = null;
			/** optional name of a column that refers to an hourID **/
			public String hourColumnName = null;
			/** optional name of a column that refers to a dayID **/
			public String dayColumnName = null;
			/** optional name of a column that refers to a hourDayID **/
			public String hourDayIDColumnName = null;
			/** optional name of a column that refers to a road type **/
			public String roadTypeColumnName = null;
			/** optional name of a column that refers to a pollutant process **/
			public String pollutantProcessIDColumnName = null;
			/** optional name of a column that refers to a source type use  **/
			public String sourceUseTypeColumnName = null;
			/** optional name of a column that refers to a fuel type**/
			public String fuelTypeColumnName = null;
			/** optional name of a column that refers to a fuel sub type**/
			public String fuelSubTypeIDColumnName = null;
			/** optional name of a column that refers to a monthGroupID **/
			public String monthGroupIDColumnName = null;
			/**
			 * optional name that identifies a column whose values indicate
			 * whether records were user inputs.
			**/
			public String isUserInputColumnName = null;

			/** optional name of a column that refers to a fuel year **/
			public String fuelYearColumnName = null;
			/** optional name of a column that refers to a region **/
			public String regionColumnName = null;
			/** optional name of a column that refers to a model year **/
			public String modelYearColumnName = null;

			/** Constructor for filling all parameters **/
			public TableToCopy(
					String tableNameToUse,
					String yearColumnNameToUse,
					String monthColumnNameToUse,
					String linkColumnNameToUse,
					String zoneColumnNameToUse,
					String countyColumnNameToUse,
					String stateColumnNameToUse,
					String pollutantColumnNameToUse,
					String processColumnNameToUse,
					String dayColumnNameToUse,
					String hourColumnNameToUse,
					String hourDayIDColumnNameToUse,
					String roadTypeColumnNameToUse,
					String pollutantProcessIDColumnNameToUse,
					String sourceUseTypeColumnNameToUse,
					String fuelTypeColumnNameToUse,
					String fuelSubTypeIDColumnNameToUse,
					String monthGroupIDColumnNameToUse,
					String isUserInputColumnNameToUse,
					String fuelYearColumnNameToUse,
					String regionColumnNameToUse,
					String modelYearColumnNameToUse) {
				tableName = tableNameToUse;
				yearColumnName = yearColumnNameToUse;
				monthColumnName = monthColumnNameToUse;
				linkColumnName = linkColumnNameToUse;
				zoneColumnName = zoneColumnNameToUse;
				countyColumnName = countyColumnNameToUse;
				stateColumnName = stateColumnNameToUse;
				pollutantColumnName = pollutantColumnNameToUse;
				processColumnName = processColumnNameToUse;
				dayColumnName = dayColumnNameToUse;
				hourColumnName = hourColumnNameToUse;
				hourDayIDColumnName = hourDayIDColumnNameToUse;
				roadTypeColumnName = roadTypeColumnNameToUse;
				pollutantProcessIDColumnName = pollutantProcessIDColumnNameToUse;
				sourceUseTypeColumnName = sourceUseTypeColumnNameToUse;
				fuelTypeColumnName =fuelTypeColumnNameToUse;
				fuelSubTypeIDColumnName = fuelSubTypeIDColumnNameToUse;
				monthGroupIDColumnName = monthGroupIDColumnNameToUse;
				isUserInputColumnName = isUserInputColumnNameToUse;
				fuelYearColumnName = fuelYearColumnNameToUse;
				regionColumnName = regionColumnNameToUse;
				modelYearColumnName = modelYearColumnNameToUse;
			}

			/** Constructor for filling most parameters **/
			public TableToCopy(
					String tableNameToUse,
					String yearColumnNameToUse,
					String monthColumnNameToUse,
					String linkColumnNameToUse,
					String zoneColumnNameToUse,
					String countyColumnNameToUse,
					String stateColumnNameToUse,
					String pollutantColumnNameToUse,
					String processColumnNameToUse,
					String dayColumnNameToUse,
					String hourColumnNameToUse,
					String hourDayIDColumnNameToUse,
					String roadTypeColumnNameToUse,
					String pollutantProcessIDColumnNameToUse,
					String sourceUseTypeColumnNameToUse,
					String fuelTypeColumnNameToUse,
					String fuelSubTypeIDColumnNameToUse,
					String monthGroupIDColumnNameToUse,
					String isUserInputColumnNameToUse) {
				tableName = tableNameToUse;
				yearColumnName = yearColumnNameToUse;
				monthColumnName = monthColumnNameToUse;
				linkColumnName = linkColumnNameToUse;
				zoneColumnName = zoneColumnNameToUse;
				countyColumnName = countyColumnNameToUse;
				stateColumnName = stateColumnNameToUse;
				pollutantColumnName = pollutantColumnNameToUse;
				processColumnName = processColumnNameToUse;
				dayColumnName = dayColumnNameToUse;
				hourColumnName = hourColumnNameToUse;
				hourDayIDColumnName = hourDayIDColumnNameToUse;
				roadTypeColumnName = roadTypeColumnNameToUse;
				pollutantProcessIDColumnName = pollutantProcessIDColumnNameToUse;
				sourceUseTypeColumnName = sourceUseTypeColumnNameToUse;
				fuelTypeColumnName =fuelTypeColumnNameToUse;
				fuelSubTypeIDColumnName = fuelSubTypeIDColumnNameToUse;
				monthGroupIDColumnName = monthGroupIDColumnNameToUse;
				isUserInputColumnName = isUserInputColumnNameToUse;
			}
		}
		TableToCopy[] tablesAndFilterColumns = {
			new TableToCopy("AgeCategory",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("AgeGroup",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("ATBaseEmissions",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,null,"monthGroupID",null),
			new TableToCopy("ATRatio",null,null,null,null,null,null,null,null,null,
					null,null,null,"polProcessID",null,"fuelTypeID",null,"monthGroupID",null),
			new TableToCopy("ATRatioGas2",null,null,null,null,null,null,null,null,null,
					null,null,null,"polProcessID","sourceTypeID",null,"fuelSubtypeID",null,null),
			new TableToCopy("ATRatioNonGas",null,null,null,null,null,null,null,null,null,
					null,null,null,"polProcessID","sourceTypeID",null,"fuelSubtypeID",null,null),
			new TableToCopy("AverageTankGasoline",null,null,null,"zoneID",null,null,null,null,
					null,null,null,null,null,null,"fuelTypeID",null,"monthGroupID","isUserInput","fuelYearID",null,null),
			new TableToCopy("AverageTankTemperature",null,"monthID",null,"zoneID",null,null,null,
					null,null,null,"hourDayID",null,null,null,null,null,null,"isUserInput"),
			new TableToCopy("AvgSpeedBin",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			// AvgSpeedDistribution cannot filter by roadTypeID or hourDayID because TotalActivityGenerator will
			// not be able to calculate SourceHours properly.
			new TableToCopy("AvgSpeedDistribution",null,null,null,null,null,null,null,null,
					null,null,null/*"hourDayID"*/,null/*"roadTypeID"*/,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("AVFT",null,null,null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null/*"fuelTypeID"*/,null,null,null),
			new TableToCopy("BaseFuel",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,"fuelTypeID",null,null,null),
			new TableToCopy("ColdSoakInitialHourFraction",null,"monthID",null,"zoneID",null,null,null,null,
					null,null,"hourDayID",null,null,"sourceTypeID",null,null,null,"isUserInput"),
			new TableToCopy("ColdSoakTankTemperature",null,"monthID",null,"zoneID",null,null,null,null,
					null,"hourID",null,null,null,null,null,null,null,null),
			new TableToCopy("ComplexModelParameterName",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("ComplexModelParameters",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,null,null,null),

			((mc == Models.ModelCombination.M2 || mc == Models.ModelCombination.M12) ? 
					new TableToCopy("County", null, null, null, null, null, null, null, null, 
					null, null, null, null, null, null, null, null, null, null)
						: 
					new TableToCopy("County",null,null,null,null,"countyID","stateID",null,null,
					null,null,null,null,null,null,null,null,null,null)),
			new TableToCopy("countyType",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),

			new TableToCopy("countyType",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),


			((mc == Models.ModelCombination.M2 || mc == Models.ModelCombination.M12) ? 
					new TableToCopy("CountyYear", null, null, null, null, null, null, null,	null, 
					null, null, null, null, null, null, null, null, null, null) 
						: 
					new TableToCopy("CountyYear","yearID",null,null,null,"countyID",null,null,null,
					null,null,null,null,null,null,null,null,null,null)),

			// CrankcaseEmissionRatio can't be filtered by polProcessID because PM needs
			// NonECNonSO4PM which isn't shown on the GUI.
			new TableToCopy("CrankcaseEmissionRatio", null, null, null, null, null, null, null, null,
					null, null, null, null, null/*"polProcessID"*/, "sourceTypeID", "fuelTypeID",
					null, null, null),
			new TableToCopy("criteriaRatio",null,null,null,null,null,null,null,null,
					null, null, null, null, "polProcessID", "sourceTypeID", "fuelTypeID",
					null, null, null),
			new TableToCopy("CumTVVCoeffs",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,null,null,null),
			new TableToCopy("DataSource",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("DayOfAnyWeek",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,null,null,null,null,null),
			// DayVMTFraction cannot filter by roadTypeID or TotalActivityGenerator will
			// not be able to calculate SourceHours properly.
			new TableToCopy("DayVMTFraction",null,"monthID",null,null,null,null,null,null,
					"dayID",null,null,null/*"roadTypeID"*/,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("dioxinemissionrate", null, null, null, null, null, null, null, null,
					null, null, null, null, "polProcessID", null, "fuelTypeID",
					null, null, null),
			new TableToCopy("DriveSchedule",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("DriveScheduleAssoc",null,null,null,null,null,null,null,null,
					null,null,null,"roadTypeID",null,"sourceTypeID",null,null,null,null),
			new TableToCopy("DriveScheduleSecond",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("driveScheduleSecondLink",null,null, null /*"linkID"*/,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("e10FuelProperties",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,"monthGroupID",null,"fuelYearID","fuelRegionID",null),
			new TableToCopy("EmissionProcess",null,null,null,null,null,null,null,"processID",
					null,null,null,null,null,null,null,null,null,null),

			includeEmissionRates?
					new TableToCopy("EmissionRate",null,null,null,null,null,null,null,null,
							null,null,null,null,"polProcessID",null,null,null,null,null)
					: null,
			includeEmissionRates?
					new TableToCopy("EmissionRateByAge",null,null,null,null,null,null,null,null,
							null,null,null,null,"polProcessID",null,null,null,null,null)
					: null,

			new TableToCopy("EmissionRateAdjustment",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID","sourceTypeID","fuelTypeID",null,null,null),
			new TableToCopy("EngineSize",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("EngineTech",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("ETOHBin",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("evapTemperatureAdjustment",null,null,null,null,null,null,null,"processID",
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("evapRVPTemperatureAdjustment",null,null,null,null,null,null,null,"processID",
					null,null,null,null,null,null,"fuelTypeID",null,null,null),
            new TableToCopy("evefficiency",null,null,null,null,null,null,null,null,
                    null,null,null,null,"polProcessID","sourceTypeID",null,null,null,null),
			new TableToCopy("FleetAvgAdjustment",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,null,null,null),
//			new TableToCopy("FuelAdjustment",null,null,null,null,null,null,null,null,null,
//					null,null,null,"polProcessID","sourceTypeID",null,/*"fuelSubTypeID"*/ null,null,null),
			// FuelEngTechAssoc not filtered because AVFT control strategy
			// intends to make control strategy objects which can be used with all RunSpecs
			new TableToCopy("FuelEngTechAssoc",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("FuelFormulation",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("FuelModelName",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("FuelModelWtFactor",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("ComplexModelParameters",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("FuelModelYearGroup",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("FuelParameterName",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),

			((mc == Models.ModelCombination.M2 || mc == Models.ModelCombination.M12) ? 
					new TableToCopy("FuelSubtype", null, null, null, null, null, null, null, null, 
					null, null, null, null, null, null, null, null, null, null)
						: 
					new TableToCopy("FuelSubtype",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,"fuelTypeID",null,null,null)),

			includeFuelSupply?
				new TableToCopy("FuelSupply",null,null,null,null,null,null,null,null,
						null,null,null,null,null,null,null,null,"monthGroupID",null,"fuelYearID","fuelRegionID",null)
				: null,

			new TableToCopy("FuelSupplyYear",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null,"fuelYearID",null,null),
			new TableToCopy("FuelType",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,"fuelTypeID",null,null,null),

			includeFuelUsageFraction?
					new TableToCopy("fuelUsageFraction",null,null,null,null,"countyID",null,null,null,
					null,null,null,null,null,null,"sourceBinFuelTypeID",null,null,null,"fuelYearID",null,null)
					: null,

			new TableToCopy("fuelWizardFactors",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("FullACAdjustment",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID","sourceTypeID",null,null,null,null),
			new TableToCopy("generalFuelRatio",null,null,null,null,null,null,"pollutantID","processID",
					null, null, null, null, "polProcessID", null, "fuelTypeID", null, null, null),
			new TableToCopy("generalFuelRatioExpression",null,null,null,null,null,null,null,null,
					null, null, null, null, "polProcessID", null, "fuelTypeID", null, null, null),
			new TableToCopy("GreetManfAndDisposal",null,null,null,null,null,null,"pollutantID",
					null,null,null,null,null,null,null,null ,null,null,null),
			/* Contains "base year," or bounding, values for an analysis year so this table cannot
			 * be filtered by the year, it is filtered by pollutantID and fuelSubType */
			new TableToCopy("GreetWellToPump",null,null,null,null,null,null,"pollutantID",null,
					null,null,null,null,null,null,null,"fuelSubTypeID",null,null),
			new TableToCopy("Grid",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("GridZoneAssoc",null,null,null,"zoneID",null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("HCPermeationCoeff",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,null,null,null),
			new TableToCopy("HCSpeciation",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,"fuelSubtypeID",null,null),

			(CompilationFlags.ENABLE_AUXILIARY_POWER_EXHAUST && includeHotellingActivityDistribution)?
					// hotellingActivityDistribution uses wildcards for zoneID, so it cannot be filtered by zone.
					new TableToCopy("hotellingActivityDistribution",null,null,null,null/*"zoneID"*/,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null)
					: null,
			new TableToCopy("hotellingAgeFraction",null,null,null,"zoneID",null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			CompilationFlags.ENABLE_AUXILIARY_POWER_EXHAUST?
					new TableToCopy("hotellingHours","yearID","monthID",null,"zoneID",null,null,null,
					null,null,null,"hourDayID",null,null,"sourceTypeID",null,null,null,
					"isUserInput")
					: null,

			new TableToCopy("hotellingCalendarYear","yearID",null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),

			new TableToCopy("hotellingHourFraction",null,null,null,"zoneID",null,null,null,null,
					"dayID","hourID",null,null,null,null,null,null,null,null),
			new TableToCopy("hotellingMonthAdjust",null,"monthID",null,"zoneID",null,null,null,
					null,null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("hotellingHoursPerDay","yearID",null,null,"zoneID",null,null,null,null,
					"dayID",null,null,null,null,null,null,null,null,null),

			// HourDay cannot be filtered by hourID because TotalActivityGenerator requires all hours
			new TableToCopy("HourDay",null,null,null,null,null,null,null,null,
					"dayID",null/*"hourID"*/,null,null,null,null,null,null,null,null),
			// HourOfAnyDay cannot be filtered by hourID because TotalActivityGenerator requires all hours
			new TableToCopy("HourOfAnyDay",null,null,null,null,null,null,null,null,
					null,null/*"hourID"*/,null,null,null,null,null,null,null,null),
			// HourVMTFraction cannot filter by roadTypeID or hourID because TotalActivityGenerator will
			// not be able to calculate SourceHours properly.
			new TableToCopy("HourVMTFraction",null,null,null,null,null,null,null,null,
					"dayID",null/*"hourID"*/,null,null/*"roadTypeID"*/,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("HPMSVtype",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("HPMSVtypeDay","yearID","monthID",null,null,null,null,null,null,
					"dayID",null,null,null,null,null,null,null,null,null),

			/* Contains "base year", or bounding, values for an analysis year so this table cannot
			 * be filtered by the selected runspec criteria. */
			includeHPMSVtypeYear? new TableToCopy("HPMSVtypeYear",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null)
					: null,

			includeIMCoverage?
					new TableToCopy("IMCoverage","yearID",null,null,null,"countyID","stateID",null,null,
					null,null,null,null,"polProcessID","sourceTypeID","fuelTypeID",null,null,null)
					: null,

			new TableToCopy("idleDayAdjust",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("idleModelYearGrouping",null,null,null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("idleMonthAdjust",null,"monthID",null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("idleRegion",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("IMFactor",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID","sourceTypeID","fuelTypeID",null,null,null),
			new TableToCopy("IMInspectFreq",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("IMModelYearGroup",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("IMTestStandards",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("StartsOpModeDistribution",null,null,null /*"linkID"*/,null,null,null,null,null,
					"dayID","hourID",null,null,null,"sourceTypeID",null,null,null,"isUserInput"),
			new TableToCopy("integratedSpeciesSet",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("integratedSpeciesSetName",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),

			includeLinkTable?
					new TableToCopy("Link",null,null,null,"zoneID","countyID",null,null,null,
					null,null,null,"roadTypeID",null,null,null,null,null,null)
					: null,

			new TableToCopy("LinkAverageSpeed",null,null,null /*"linkID"*/,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("LinkHourVMTFraction",null,"monthID", null /*"linkID"*/,null,null,null,null,
					"dayID","hourID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("linkSourceTypeHour",null,null, null /*"linkID"*/,null,null,null,null,
					null,null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("M6SulfurCoeff",null,null,null,null,null,null,"pollutantID",
					null,null,null,null,null,null,null,null ,null,null,null),
			new TableToCopy("MeanFuelParameters",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,"fuelTypeID",null,null,null),
			new TableToCopy("mechanismName",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("metalemissionrate", null, null, null, null, null, null, null, null,
					null, null, null, null, "polProcessID", "sourceTypeID", "fuelTypeID",
					null, null, null),
			new TableToCopy("methaneTHCRatio",null,null,null,null,null,null,null,"processID",
					null,null,null,null,null,null,null,null/*"fuelSubtypeID"*/,null,null),
			new TableToCopy("minorhapratio", null, null, null, null, null, null, null, null,
					null, null, null, null, "polProcessID", null, "fuelTypeID",
					null, null, null),
			new TableToCopy("ModelYear",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("modelYearCutPoints",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("ModelYearGroup",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("ModelYearMapping",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("MonthGroupHour",null,null,null,null,null,null,null,null,
					null,"hourID",null,null,null,null,null,null,"monthGroupID",null),
			new TableToCopy("MonthGroupOfAnyYear",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,"monthGroupID",null),
			/* AggregationSQLGenerator needs MonthOfAnyYear to have all 12 months */
			new TableToCopy("MonthofAnyYear",null,null /*"monthID"*/,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("MonthVMTFraction",null,"monthID",null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("NONO2Ratio",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID","sourceTypeID","fuelTypeID",null,null,null),
			new TableToCopy("NOxHumidityAdjust",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,"fuelTypeID",null,null,null),
			new TableToCopy("offNetworkLink",null,null,null,null,null,null,null,
					null,null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("OMDGPolProcessRepresented",null,null,null,null,null,null,null,null,
					null, null, null, null, "polProcessID", null, null, null, null, null),
			new TableToCopy("onRoadRetrofit",null,null,null,null,null,null,"pollutantID",
					"processID",null,null,null,null,null,"sourceTypeID","fuelTypeID",null,null,null),
			new TableToCopy("OperatingMode",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("OpModeDistribution",null,null,null /*"linkID"*/,null,null,null,null,null,
					null,null,"hourDayID",null,"polProcessID","sourceTypeID",null,null,null,
					"isUserInput"),
			new TableToCopy("OpModePolProcAssoc",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,null,null,null),
			new TableToCopy("OxyThreshName",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("pahGasRatio", null, null, null, null, null, null, null, null,
					null, null, null, null, "polProcessID", null, "fuelTypeID",
					null, null, null),
			new TableToCopy("pahParticleRatio", null, null, null, null, null, null, null, null,
					null, null, null, null, "polProcessID", null, "fuelTypeID",
					null, null, null),
			new TableToCopy("PM10EmissionRatio", null, null, null, null, null, null, null, null,
					null, null, null, null, "polProcessID", "sourceTypeID", "fuelTypeID",
					null, null, null),
			new TableToCopy("PMSpeciation", null, null, null, null, null, null, "outputPollutantID", "processID",
					null, null, null, null, null, "sourceTypeID", "fuelTypeID", null, null, null),
			new TableToCopy("PollutantDisplayGroup",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),

			((mc == Models.ModelCombination.M2 || mc == Models.ModelCombination.M12) ? 
					new TableToCopy("Pollutant", null, null, null, null, null, null, null,
					null, null, null, null, null, null, null, null, null, null, null)
						: 
					new TableToCopy("Pollutant",null,null,null,null,null,null,"pollutantID",null,
					null,null,null,null,null,null,null,null,null,null)),
			((mc == Models.ModelCombination.M2 || mc == Models.ModelCombination.M12) ? 
					new TableToCopy("PollutantProcessAssoc", null, null, null, null, null, null, null, 
					null, null, null, null, null, null, null, null, null, null, null) 
						: 
					new TableToCopy("PollutantProcessAssoc",null,null,null,null,null,null,"pollutantID",
					"processID",null,null,null,null,"polProcessID",null,null,null,null,null)),

			new TableToCopy("PollutantProcessModelYear",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,null,null,null),
			new TableToCopy("RefuelingControlTechnology", null, null, null, null, null, null, null,
					"processID", null, null, null, null, null, "sourceTypeID", null,null, null, null),
			new TableToCopy("RefuelingFactors", null, null, null, null, null, null, null, null,
					null, null, null, null, null, null, "fuelTypeID", null, null, null),
			new TableToCopy("RegulatoryClass",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("region",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null,null,"regionID",null),
			new TableToCopy("regionCode",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("regionCounty","fuelYearID",null,null,null,"countyID",null,null,null, //modifying filter to include fuel year in regionCounty filtering
					null,null,null,null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("RetrofitInputAssociations",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			// RoadType cannot filter by roadTypeID or TotalActivityGenerator will
			// not be able to calculate SourceHours properly.
			new TableToCopy("RoadType",null,null,null,null,null,null,null,null,
					null,null,null,null/*"roadTypeID"*/,null,null,null,null,null,null),
			// RoadTypeDistribution cannot filter by roadTypeID or TotalActivityGenerator will
			// not be able to calculate SourceHours properly.
			// RoadTypeDistribution cannot filter by roadTypeID or ActivityCalculator will
			// not be able to calculate population properly.
			new TableToCopy("RoadTypeDistribution",null,null,null,null,null,null,null,null,
					null,null,null,null/*"roadTypeID"*/,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("SampleVehicleDay",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("SampleVehicleSoaking",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("SampleVehicleSoakingDay",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("SampleVehicleSoakingDayUsed",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("SampleVehicleSoakingDayBasis",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,null,null,null,null,null),
			new TableToCopy("SampleVehicleSoakingDayBasisUsed",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,null,null,null,null,null),
			new TableToCopy("SampleVehicleTrip",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,null,null,null,null,null),
					
			// Do not filter SampleVehiclePopulation by fuel type or source type. Doing so breaks features that
			// require full distributions even when not selected in the runspec.
			new TableToCopy("SampleVehiclePopulation",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null/*"fuelTypeID"*/,null,null,null,null,null,"modelYearID"),

			new TableToCopy("SCC",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("Sector",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("SHO","yearID","monthID",null /*"linkID"*/,null,null,null,null,null,
					null,null,"hourDayID",null,null,"sourceTypeID",null,null,null,"isUserInput"),
			// AVFT needs all of these fractions so it can move vehicles from one type
			// of fuel to another.
			new TableToCopy("SizeWeightFraction",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null/*"fuelTypeID"*/,null,null,null),
			new TableToCopy("SoakActivityFraction",null,"monthID",null,"zoneID",null,null,null,
					null,null,null,"hourDayID",null,null,null,null,null,null,
					"isUserInput"),
			new TableToCopy("SourceBin",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,"fuelTypeID",null,null,null),
			new TableToCopy("SourceBinDistribution",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,null,null,null,"isUserInput"),
			new TableToCopy("SourceHours","yearID","monthID",null,null,null,null,null,
					null,null,null,"hourDayID",null,null,"sourceTypeID",null,null,null,
					"isUserInput"),
			new TableToCopy("SourceTypeAge",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),

			/* Used in base year calculations so this table cannot be filtered */
			includeSourceTypeAgeDistribution? new TableToCopy("SourceTypeAgeDistribution",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null)
					: null,

			new TableToCopy("SourceTypeDayVMT","yearID","monthID",null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
					
			// SourceTypeHour cannot be filtered by hour because hotelling shaping requires
			// all hours of a day. The TAG filters it by day.
			new TableToCopy("SourceTypeHour",null,null,null,null,null,null,null,null,
					null,null,null/*"hourDayID"*/,null,null,"sourceTypeID",null,null,null,null),

			// Used in AVFTControlStrategy calculations, so do not use filter by sourceTypeID
			new TableToCopy("SourceTypeModelYear",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null/*"sourceTypeID"*/,null,null,null,null,null,null,"modelYearID"),
			new TableToCopy("SourceTypeModelYearGroup",null,null,null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("SourceTypePolProcess",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID","sourceTypeID",null,null,null,null),
			new TableToCopy("SourceTypeTechAdjustment", null, null, null, null, null, null, null,
					"processID", null, null, null, null, null, "sourceTypeID", null,
					null, null, null),

			// "SourceTypeYear" Used in base year calculations so this table cannot be filtered
			// Also, sourceTypeID cannot be filtered in SourceTypeYear due to the need
			// to calculate relativeMAR in TotalActivityGenerator, a calculation that needs
			// data from all source use types.
			includeSourceTypeYear? new TableToCopy("SourceTypeYear",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null)
					: null,

			new TableToCopy("SourceTypeYearVMT","yearID",null,null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("SourceUseType",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("sourceUseTypePhysics",null,null,null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("Starts","yearID","monthID",null,"zoneID",null,null,null,null,
					null,null,"hourDayID",null,null,null,null,null,null,"isUserInput"),
			new TableToCopy("startsAgeAdjustment",null,null,null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("startsPerDay",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("startsPerDayPerVehicle",null,null,null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("startsHourFraction",null,null,null,null,null,null,null,null,
					"dayID","hourID",null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("startsMonthAdjust",null,"monthID",null,null,null,null,null,null,
					null,null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("StartsPerVehicle",null,null,null,null,null,null,null,null,
					null,null,"hourDayID",null,null,"sourceTypeID",null,null,null,null),

			((mc == Models.ModelCombination.M2 || mc == Models.ModelCombination.M12) ? 
					new TableToCopy("State",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null)
						:
					new TableToCopy("State",null,null,null,null,null,"stateID",null,null,
					null,null,null,null,null,null,null,null,null,null)),

			new TableToCopy("SulfateEmissionRate",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,"fuelTypeID",null,null,null),
			new TableToCopy("SulfateFractions", null, null, null, null, null, null, null, "processID",
					null, null, null, null, null, "sourceTypeID", "fuelTypeID", null, null, null),
			new TableToCopy("SulfurBase",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("sulfurCapAmount",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,"fuelTypeID",null,null,null),
			new TableToCopy("SulfurModelCoeff",null,null,null,null,null,null,null,"processID",
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("SulfurModelName",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("TankTemperatureGroup",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("TankTemperatureRise",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("TankVaporGenCoeffs",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("TemperatureAdjustment",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,"fuelTypeID",null,null,null),
			new TableToCopy("TemperatureProfileID",null,"monthID",null,"zoneID",null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("TOGSpeciationProfileName",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			new TableToCopy("totalIdleFraction",null,"monthID",null,null,null,null,null,null,
					"dayID",null,null,null,null,"sourceTypeID",null,null,null,null),
			new TableToCopy("StartTempAdjustment",null,null,null,null,null,null,null,null,
					null,null,null,null,"polProcessID",null,"fuelTypeID",null,null,null),
			new TableToCopy("WeightClass",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),
			/* "Year" Contains "base year," or bounding, values for an analysis year so this table
			 * cannot be filtered by the selected runspec criteria. */
			new TableToCopy("Year",null,null,null,null,null,null,null,null,
					null,null,null,null,null,null,null,null,null,null),

			((mc == Models.ModelCombination.M2 || mc == Models.ModelCombination.M12) ? 
					new TableToCopy("Zone", null, null, null, null, null, null, null, null,
					null, null, null, null, null, null, null, null, null, null)
						: 
					new TableToCopy("Zone",null,null,null,"zoneID","countyID",null,null,null,
					null,null,null,null,null,null,null,null,null,null)),
			((mc == Models.ModelCombination.M2 || mc == Models.ModelCombination.M12) ? 
					new TableToCopy("ZoneMonthHour", null, null, null, null, null, null,
					null, null, null, null, null, null, null, null, null, null, null, null) 
						: 
					new TableToCopy("ZoneMonthHour",null,"monthID",null,"zoneID",null,null,null,null,
					null,null/*"hourID"*/,null,null,null,null,null,null,null,null)),

			// ZoneRoadType cannot filter by roadTypeID or ActivityCalculator will
			// not be able to calculate population properly.
			// Also, the hotelling/extended idle algorithm requires data from the
			// Rural Restricted road type even if it is not in the runspec.
			new TableToCopy("ZoneRoadType",null,null,null,"zoneID",null,null,null,null,
					null,null,null,null/*"roadTypeID"*/,null,null,null,null,null,null)
		};
		TreeSetIgnoreCase tablesToFilter = new TreeSetIgnoreCase();
		boolean allowTablesToFilter = false;
		if(mergeSession != null) {
			String[] shallowTables = {
				"Year", "regionCounty", 
				"MonthGroupOfAnyYear", "MonthOfAnyYear"
			};
			for(int i=0;i<shallowTables.length;i++) {
				tablesToFilter.add(shallowTables[i]);
			}
			allowTablesToFilter = mergeSession.doShallowOnly;
		}
		for(int i=0;i<tablesAndFilterColumns.length;i++) {
			TableToCopy t = tablesAndFilterColumns[i];
			if(t == null) { // skip entries that are conditionally created
				continue;
			}
			if(tablesToFilter.size() > 0) {
				if(allowTablesToFilter && !tablesToFilter.contains(t.tableName)) {
					continue;
				}
				if(!allowTablesToFilter && tablesToFilter.contains(t.tableName)) {
					continue;
				}
			}
			boolean shouldLog = false; // t.tableName.equalsIgnoreCase("IMCoverage");
			//shouldLog = t.tableName.equalsIgnoreCase("FuelSupply");
			if(shouldLog) {
				Logger.log(LogMessageCategory.INFO,"InputDataManager transferring table " + t.tableName);
			}
			rs = dmd.getTables( null , "" , t.tableName , tableTypes );
			mdTableName = "" ;
			if( rs != null) {
				if (rs.next()) {
					mdTableName = rs.getString(3) ;
				}
				rs.close() ;
			}

			if ( mdTableName.length() == 0 && allowMissingTables == true ) {
				continue ;
			}

			if ( mdTableName.length() == 0 && allowMissingTables == false ) {
				Exception ex = new Exception(
						"The Table " + t.tableName + " does not exist in the "
						+ " source database " + source.getCatalog() + ". The merge is canceled." );
				throw ex ;
			}

			Vector< Vector<String> > clauseSets = new Vector< Vector<String> >();

			if(includeFuelSupply && t.tableName.equalsIgnoreCase("fuelSupply")) {
				Vector<String> fuelSupplyClauses = buildSQLWhereClauseForFuelSupply(destination);
				if(fuelSupplyClauses.size() > 0) {
					addToClauseSets(clauseSets,fuelSupplyClauses);
				}
			}
			if(t.tableName.equalsIgnoreCase("hotellingActivityDistribution")) {
				if(hasExistingData(destination,t.tableName)) {
					continue;
				}
			}
			if(t.yearColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForYears(t.yearColumnName));
			}
			if(t.monthColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForMonths(t.monthColumnName));
			}
			if(t.dayColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForDays(t.dayColumnName));
			}
			if(t.hourColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForHours(t.hourColumnName));
			}
			if(t.linkColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForLinks(t.linkColumnName));
			}
			if(t.zoneColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForZones(t.zoneColumnName));
			}
			if(t.countyColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForCounties(t.countyColumnName));
			}
			if(t.stateColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForStates(t.stateColumnName));
			}
			if(t.pollutantColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForPollutants(t.pollutantColumnName));
			}
			if(t.processColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForProcesses(t.processColumnName));
			}
			if(t.hourDayIDColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForHourDayIDs(t.hourDayIDColumnName));
			}
			if(t.roadTypeColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForRoadTypes(t.roadTypeColumnName));
			}
			if(t.pollutantProcessIDColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForPollutantProcessIDs(t.pollutantProcessIDColumnName));
			}
			if(t.sourceUseTypeColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForSourceUseTypes(t.sourceUseTypeColumnName));
			}
			if(t.fuelTypeColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForFuelTypes(t.fuelTypeColumnName));
			}
			if(t.fuelSubTypeIDColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForFuelSubTypes(t.fuelSubTypeIDColumnName));
			}
			if(t.monthGroupIDColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForMonthGroupIDs(t.monthGroupIDColumnName));
			}
			if(t.fuelYearColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForFuelYears(t.fuelYearColumnName));
			}
			if(t.regionColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForRegions(t.regionColumnName));
			}
			if(t.modelYearColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForModelYears(t.modelYearColumnName));
			}

			if(clauseSets.size() <= 0) {
if(shouldLog) System.out.println("IDM No clause sets for " + t.tableName);
				try {
					copyTable(source,destination,t.tableName,"",t.isUserInputColumnName,isDefaultDatabase);
				} catch( SQLException ex ) {
					/**
					 * @explain A database error occurred while copying a table from one
					 * database to another.
					**/
					Logger.logError(ex,"copyTable threw an SQLException" ) ;
					throw ex ;
				} catch( IOException ex ) {
					/**
					 * @explain A file system error occurred while copying a table from one
					 * database to another.
					**/
					Logger.logError(ex,"copyTable threw an IOException" ) ;
					throw ex ;
				}
			} else {
				// Issue a copyTable for each combination of entries within clauseSets
				String log = "IDM " + clauseSets.size() + " clause sets for " +
						t.tableName + " (";
				int [] counters = new int[clauseSets.size()];
				for(int j=0;j<clauseSets.size();j++) {
					counters[j] = 0;
					Vector<String> clauses = (Vector<String>)clauseSets.get(j);
					if(j > 0) {
						log += ",";
					}
					log += clauses.size();
				}
				log += ")";
				if(shouldLog) {
					System.out.println(log);
				}

				boolean done = false;
				do {
					String wholeWhereClause = "";
					for(int j=0;j<clauseSets.size();j++) {
						Vector<String> clauses = (Vector<String>)clauseSets.get(j);
						String clause = (String)clauses.get(counters[j]);
						wholeWhereClause = addToWhereClause(wholeWhereClause,clause);
					}

					log = "\t\tdoing clause (";
					for(int j=0;j<clauseSets.size();j++) {
						if(j > 0) {
							log += ",";
						}
						log += counters[j];
					}
					log += ") wholeWhereClause is " + wholeWhereClause.length() + " long";
					if(shouldLog) {
						System.out.println(log);
					}

					try {
						if(shouldLog) {
							System.out.println(wholeWhereClause);
						}
						copyTable(source,destination,t.tableName,
								wholeWhereClause,t.isUserInputColumnName,isDefaultDatabase);
					} catch( SQLException ex ) {
						/**
						 * @explain A database error occurred while copying a table from one
						 * database to another.
						**/
						Logger.logError(ex,"copyTable threw an SQLException" ) ;
						throw ex ;
					} catch( IOException ex ) {
						/**
						 * @explain A file system error occurred while copying a table from one
						 * database to another.
						**/
						Logger.logError(ex,"copyTable threw an IOException" ) ;
						throw ex ;
					}

					// Move to the next combination of clauses
					int index = 0;
					boolean doneIncrementing = false;
					while(!doneIncrementing) {
						doneIncrementing = true;
						counters[index]++;
						Vector<String> clauses = (Vector<String>)clauseSets.get(index);
						if(counters[index] >= clauses.size()) {
							doneIncrementing = false;
							counters[index] = 0;
							index++;
							if(index >= clauseSets.size()) {
								done = true;
								doneIncrementing = true;
							}
						}
					}
				} while(!done);
			}
			// Update MySQL's statistics on the table
			SQLRunner.executeSQL(destination,"ANALYZE TABLE " + t.tableName);
		}
		// Move data from staging tables to production tables.
		if(CompilationFlags.DO_RATES_FIRST) {
			String sql = "insert ignore into ratesOpModeDistribution (avgSpeedBinID, roadTypeID, "
					+ " sourceTypeID, hourDayID, polProcessID, opModeID, opModeFraction, opModeFractionCV)"
					+ " select 0 as avgSpeedBinID, 1 as roadTypeID, "
					+ " sourceTypeID, hourDayID, polProcessID, opModeID, opModeFraction, opModeFractionCV"
					+ " from importStartsOpModeDistribution";

			SQLRunner.executeSQL(destination,sql);
		} else {
			String sql = "insert ignore into opModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction, opModeFractionCV)"
					+ " select sourceTypeID, hourDayID, linkID, polProcessID, opModeID, opModeFraction, opModeFractionCV"
					+ " from importStartsOpModeDistribution";
			SQLRunner.executeSQL(destination,sql);
		}

		// Done
		/** @nonissue **/
		Logger.log(LogMessageCategory.INFO,"InputDataManager transferred all default tables.");
	}

	/**
	 * Merge from a NonRoad "default" database to an "execution" database.
	 * @param source The NonRoad "default" database to get data from.
	 * @param destination The "execution" database to write data to.
	 * @param isDefaultDatabase true when the default database is the source.
	 * @throws SQLException If there is an error during any java.sql operations.
	 * @throws IOException If there is an error during any temporary file operations
	**/
	public void mergeNonRoad(Connection source, Connection destination,
			boolean isDefaultDatabase)
			throws SQLException, IOException, Exception {
		if (!CompilationFlags.USE_NONROAD) {
			return;
		}

		DatabaseMetaData dmd;
		String tableTypes[] = new String[1];
		ResultSet rs;
		String mdTableName = "";

		dmd = source.getMetaData();
		tableTypes[0] = "TABLE";

		boolean includeNRFuelSupply = true;
		if(isDefaultDatabase) {
			// If there is already data in the destination nrFuelSupply table, do not import
			// the default database's nrFuelSupply table.
			if(DatabaseUtilities.getRowCount(destination,"nrFuelSupply") > 0) {
				includeNRFuelSupply = false;
			}
		}

		/**
		 * Inner class used to identify the tables and table rows to be copied
		 * from the "default" database to the "execution" database. This class
		 * allows table rows to be filtered by criteria specified in the
		 * Execution Runspec. Rows can be filtered by
		 * <ul>
		 * <li>year</li>
		 * <li>month</li>
		 * <li>link</li>
		 * <li>zone</li>
		 * <li>county</li>
		 * <li>state</li>
		 * <li>pollutant,</li>
		 * <li>emission process.</li>
		 * <li>day</li>
		 * <li>hour</li>
		 * <li>hourDayID</li>
		 * <li>roadType</li>
		 * <li>pollutantProcessID</li>
		 * <li>sourceUseType</li>
		 * <li>fuelType</li>
		 * <li>monthGroupID</li>,
		 * <li>fuelSubType</li>, and
		 * <li>sector</li>.
		 * </ul>
		 * To filter rows by runspec criteria, place the table's column names
		 * for the criteria in the corresponding entries below.
		 * <p>
		 * Note that some tables contain data that must <em>not</em> be
		 * filtered. Tables with population data for a base year must not be
		 * filtered by the analysis year as the base year data is needed to grow
		 * the table data to the analysis year. Tables that contain distribution
		 * data for an entire population must not be filtered by selected
		 * members of the population as the full population data will be needed
		 * in performing calculations. (The exception to this is "Fraction"
		 * tables, where a member's fractional value of the entire population
		 * has already been calculated).
		 * </p>
		 **/

		class NRTableToCopy {
			/** name of the table **/
			public String tableName = null;
			/** optional name of a column that refers to a year **/
			public String yearColumnName = null;
			/** optional name of a column that refers to a monthID **/
			public String monthColumnName = null;
			/** optional name of a column that refers to a zoneID **/
			public String zoneColumnName = null;
			/** optional name of a column that refers to a countyID **/
			public String countyColumnName = null;
			/** optional name of a column that refers to a stateID **/
			public String stateColumnName = null;
			/** optional name of a column that refers to a pollutantID **/
			public String pollutantColumnName = null;
			/** optional name of a column that refers to a processID **/
			public String processColumnName = null;
			/** optional name of a column that refers to an hourID **/
			public String hourColumnName = null;
			/** optional name of a column that refers to a dayID **/
			public String dayColumnName = null;
			/** optional name of a column that refers to a hourDayID **/
			public String hourDayIDColumnName = null;
			/** optional name of a column that refers to a pollutant process **/
			public String pollutantProcessIDColumnName = null;
			/** optional name of a column that refers to a source type use **/
			public String sourceUseTypeColumnName = null;
			/** optional name of a column that refers to a fuel type **/
			public String fuelTypeColumnName = null;
			/** optional name of a column that refers to a fuel sub type **/
			public String fuelSubTypeIDColumnName = null;
			/** optional name of a column that refers to a monthGroupID **/
			public String monthGroupIDColumnName = null;
			/** optional name of a column that refers to a sectorID **/
			public String sectorIDColumnName = null;
			/** optional name of a column that refers to an equipment type ID **/
			public String equipmentTypeIDColumnName = null;
			/**
			 * optional name that identifies a column whose values indicate
			 * whether records were user inputs.
			 **/
			public String isUserInputColumnName = null;

			/** Constructor for filling all parameters **/
			public NRTableToCopy(String tableNameToUse,
					String yearColumnNameToUse, String monthColumnNameToUse,
					String zoneColumnNameToUse, String countyColumnNameToUse,
					String stateColumnNameToUse,
					String pollutantColumnNameToUse,
					String processColumnNameToUse, String dayColumnNameToUse,
					String hourColumnNameToUse,
					String hourDayIDColumnNameToUse,
					String pollutantProcessIDColumnNameToUse,
					String sourceUseTypeColumnNameToUse,
					String fuelTypeColumnNameToUse,
					String fuelSubTypeIDColumnNameToUse,
					String monthGroupIDColumnNameToUse,
					String sectorIDColumnNameToUse,
					String equipmentTypeIDColumnNameToUse,
					String isUserInputColumnNameToUse) {
				tableName = tableNameToUse;
				yearColumnName = yearColumnNameToUse;
				monthColumnName = monthColumnNameToUse;
				zoneColumnName = zoneColumnNameToUse;
				countyColumnName = countyColumnNameToUse;
				stateColumnName = stateColumnNameToUse;
				pollutantColumnName = pollutantColumnNameToUse;
				processColumnName = processColumnNameToUse;
				dayColumnName = dayColumnNameToUse;
				hourColumnName = hourColumnNameToUse;
				hourDayIDColumnName = hourDayIDColumnNameToUse;
				pollutantProcessIDColumnName = pollutantProcessIDColumnNameToUse;
				sourceUseTypeColumnName = sourceUseTypeColumnNameToUse;
				fuelTypeColumnName = fuelTypeColumnNameToUse;
				fuelSubTypeIDColumnName = fuelSubTypeIDColumnNameToUse;
				monthGroupIDColumnName = monthGroupIDColumnNameToUse;
				sectorIDColumnName = sectorIDColumnNameToUse;
				equipmentTypeIDColumnName = equipmentTypeIDColumnNameToUse;
				isUserInputColumnName = isUserInputColumnNameToUse;
			}
		}

		NRTableToCopy[] tablesAndFilterColumns = {
				new NRTableToCopy("NRAgeCategory", null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, null, null),
				new NRTableToCopy("nrATRatio", null, null, null,
						null, null, "pollutantID", "processID", null, null, null,
						null, null, null, "fuelSubTypeID", null, null,
						null, null),
				new NRTableToCopy("NRBaseYearEquipPopulation", null, null,
						null, null, "stateID", null, null, null, null, null,
						null, "sourceTypeID", null, null, null, null, null,
						null),
				new NRTableToCopy("nrcrankcaseemissionrate", null, null, null,
						null, null, null, null, null, null, null,
						"polProcessID", "sourceTypeID", null, null, null, null,
						null, null),
				new NRTableToCopy("NRDayAllocation", null, null, null, null,
						null, null, null, "dayID", null, null, null, null,
						null, null, null, null, "NREquipTypeID", null),
				new NRTableToCopy("NRDeterioration", null, null, null, null,
						null, null, null, null, null, null, "polProcessID",
						null, null, null, null, null, null, null),
				new NRTableToCopy("NREngTechFraction", null, null, null, null,
						null, null, "processID", null, null, null, null,
						"sourceTypeID", null, null, null, null, null, null),
				new NRTableToCopy("NREquipmentType", null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, "sectorID", "NREquipTypeID", null),
				new NRTableToCopy("NREvapEmissionRate", null, null, null, null,
						null, null, null, null, null, null, "polProcessID",
						"sourceTypeID", null, null, null, null, null, null),
				new NRTableToCopy("NRExhaustEmissionRate", null, null, null,
						null, null, null, null, null, null, null,
						"polProcessID", "sourceTypeID", null, null, null, null,
						null, null),
				new NRTableToCopy("NRFuelOxyAdjustment", null, null, null,
						null, null, null, null, null, null, null,
						"polProcessID", null, "fuelTypeID", null, null, null,
						null, null),
				new NRTableToCopy("NRGrowthIndex", "yearID", null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, null, null),
				new NRTableToCopy("NRGrowthPattern", null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, null, null),
				new NRTableToCopy("NRGrowthPatternFinder", null, null, null,
						null, "stateID", null, null, null, null, null, null,
						null, null, null, null, null, null, null),
				new NRTableToCopy("nrHCSpeciation", null, null, null,
						null, null, "pollutantID", "processID", null, null, null,
						null, null, null, "fuelSubTypeID", null, null,
						null, null),
				new NRTableToCopy("NRHourAllocation", null, null, null, null,
						null, null, null, null, "hourID", null, null, null,
						null, null, null, null, null, null),
				new NRTableToCopy("NRHourAllocPattern", null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, null, null),
				new NRTableToCopy("NRHourPatternFinder", null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, null, "NREquipTypeID", null),
				new NRTableToCopy("NRHPRangeBin", null, null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, null),
				new NRTableToCopy("nrMethaneTHCRatio", null, null, null,
						null, null, null, "processID", null, null, null,
						null, null, null, "fuelSubTypeID", null, null,
						null, null),
				new NRTableToCopy("NRMonthAllocation", null, "monthID", null,
						null, "stateID", null, null, null, null, null, null,
						null, null, null, null, null, "NREquipTypeID", null),
				new NRTableToCopy("NRUSMonthAllocation", null, "monthID", null,
						null, null, null, null, null, null, null, null,
						null, null, null, null, null, "NREquipTypeID", null),
				new NRTableToCopy("NRPollutantProcessModelYear", null, null,
						null, null, null, null, null, null, null, null,
						"polProcessID", null, null, null, null, null, null,
						null),
				new NRTableToCopy("NRProcessEmissionRate", null, null, null,
						null, null, null, null, null, null, null,
						"polProcessID", "sourceTypeID", null, null, null, null,
						null, null),
				new NRTableToCopy("NRSCC", null, null, null, null, null, null,
						null, null, null, null, null, null, "fueltypeID", null,
						null, null, "NREquipTypeID", null),
				new NRTableToCopy("NRScrappageCurve", null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, "NREquipTypeID", null),
				new NRTableToCopy("NRSourceBin", null, null, null, null, null,
						null, null, null, null, null, null, null, "fueltypeID",
						null, null, null, null, null),
				new NRTableToCopy("NRSourceUseType", null, null, null, null,
						null, null, null, null, null, null, null,
						"sourcetypeID", null, null, null, null, null, null),
				new NRTableToCopy("NRStateSurrogateTotal", null, null, null,
						null, "stateID", null, null, null, null, null, null,
						null, null, null, null, null, null, null),
				new NRTableToCopy("NRSulfurAdjustment", null, null, null, null,
						null, null, null, null, null, null, null, null,
						"fueltypeID", null, null, null, null, null),
				new NRTableToCopy("NRSurrogate", null, null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						null, null, null, null),
				new NRTableToCopy("NRTemperatureAdjustment", null, null, null,
						null, null, null, null, null, null, null,
						"polProcessID", null, "fueltypeID", null, null, null,
						null, null),
				new NRTableToCopy("NRTransientAdjustFactor", null, null, null,
						null, null, null, null, null, null, null,
						"polProcessID", null, "fueltypeID", null, null, null,
						"NREquipTypeID", null),
				new NRTableToCopy("NRZoneAllocation", null, null, "zoneID",
						null, "stateID", null, null, null, null, null, null,
						null, null, null, null, null, null, null),

		};

		// MUST be changed later
		List<NRTableToCopy> listTableToCopy = new ArrayList<NRTableToCopy>();
		File tableFile = new File(NONROAD_TABLE_FILTER_FILE_NAME);
		BufferedReader reader = new BufferedReader(new FileReader(tableFile));
		String line = null;
		String[] cols = new String[19];
		while ((line = reader.readLine()) != null) {
			if (line.trim().isEmpty() || line.trim().startsWith("#"))
				continue;
			line = line.trim();
			int start = 0;
			int pos = line.indexOf(",", start);
			int col = 0;
			while (pos >= 0) {
				cols[col++] = line.substring(start, pos).trim();
				start = pos + 1;
				pos = line.indexOf(",", start);
			}
			cols[col++] = line.substring(start, line.length()).trim();
			if (col != 19) {
				Logger.log(LogMessageCategory.ERROR,
						"The row in NoroadTableFilter.csv is invalid: " + line);
				continue;
			}
			if (cols[0] == null || cols[0].trim().isEmpty()) {
				Logger.log(LogMessageCategory.ERROR,
						"Table name must be non-empty: " + line);
				continue;
			}
			listTableToCopy.add(new NRTableToCopy(cols[0].trim(),
					(cols[1] == null || cols[1].trim().isEmpty()) ? null
							: cols[1], (cols[2] == null || cols[2].trim()
							.isEmpty()) ? null : cols[2],
					(cols[3] == null || cols[3].trim().isEmpty()) ? null
							: cols[3], (cols[4] == null || cols[4].trim()
							.isEmpty()) ? null : cols[4],
					(cols[5] == null || cols[5].trim().isEmpty()) ? null
							: cols[5], (cols[6] == null || cols[6].trim()
							.isEmpty()) ? null : cols[6],
					(cols[7] == null || cols[7].trim().isEmpty()) ? null
							: cols[7], (cols[8] == null || cols[8].trim()
							.isEmpty()) ? null : cols[8],
					(cols[9] == null || cols[9].trim().isEmpty()) ? null
							: cols[9], (cols[10] == null || cols[10].trim()
							.isEmpty()) ? null : cols[10],
					(cols[11] == null || cols[11].trim().isEmpty()) ? null
							: cols[11], (cols[12] == null || cols[12].trim()
							.isEmpty()) ? null : cols[12],
					(cols[13] == null || cols[13].trim().isEmpty()) ? null
							: cols[13], (cols[14] == null || cols[14].trim()
							.isEmpty()) ? null : cols[14],
					(cols[15] == null || cols[15].trim().isEmpty()) ? null
							: cols[15], (cols[16] == null || cols[16].trim()
							.isEmpty()) ? null : cols[16],
					(cols[17] == null || cols[17].trim().isEmpty()) ? null
							: cols[17], (cols[18] == null || cols[18].trim()
							.isEmpty()) ? null : cols[18]));
		}
		tablesAndFilterColumns = listTableToCopy.toArray(new NRTableToCopy[0]);
		// end of MUST be changed later
		reader.close();

		for (int i = 0; i < tablesAndFilterColumns.length; i++) {
			NRTableToCopy t = tablesAndFilterColumns[i];
			if (t == null) { // skip entries that are conditionally created
				continue;
			}
			if(!includeNRFuelSupply && t.tableName.equalsIgnoreCase("nrFuelSupply")) {
				continue;
			}
			boolean shouldLog = false; // t.tableName.equalsIgnoreCase("IMCoverage");
			if (shouldLog)
				Logger.log(LogMessageCategory.INFO,
						"InputDataManager transferring NonRoad table "
								+ t.tableName);
			rs = dmd.getTables(null, "", t.tableName, tableTypes);
			mdTableName = "";
			if (rs != null) {
				if (rs.next()) {
					mdTableName = rs.getString(3);
				}
				rs.close();
			}

			if (mdTableName.length() == 0 && allowMissingTables == true) {
				continue;
			}

			if (mdTableName.length() == 0 && allowMissingTables == false) {
				Exception ex = new Exception("The Table " + t.tableName
						+ " does not exist in the " + " source database "
						+ source.getCatalog() + ". The merge is canceled.");
				throw ex;
			}

			Vector<Vector<String>> clauseSets = new Vector<Vector<String>>();

			if (t.yearColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForYears(t.yearColumnName));
			}
			if (t.monthColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForMonths(t.monthColumnName));
			}
			if (t.dayColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForDays(t.dayColumnName));
			}
			if (t.hourColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForHours(t.hourColumnName));
			}
			if (t.zoneColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForZones(t.zoneColumnName));
			}
			if (t.countyColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForCounties(t.countyColumnName));
			}
			if (t.stateColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForStates(t.stateColumnName));
			}
			if (t.pollutantColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForPollutants(t.pollutantColumnName));
			}
			if (t.processColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForProcesses(t.processColumnName));
			}
			if (t.hourDayIDColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForHourDayIDs(t.hourDayIDColumnName));
			}
			if (t.pollutantProcessIDColumnName != null) {
				addToClauseSets(
						clauseSets,
						buildSQLWhereClauseForPollutantProcessIDs(t.pollutantProcessIDColumnName));
			}
			if (t.sourceUseTypeColumnName != null) {
				addToClauseSets(
						clauseSets,
						buildSQLWhereClauseForNonRoadSourceUseTypes(t.sourceUseTypeColumnName));
			}
			if (t.fuelTypeColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForNonroadFuelTypes(t.fuelTypeColumnName));
			}
			if (t.fuelSubTypeIDColumnName != null) {
				addToClauseSets(
						clauseSets,
						buildSQLWhereClauseForNonroadFuelSubTypes(t.fuelSubTypeIDColumnName));
			}
			if (t.monthGroupIDColumnName != null) {
				addToClauseSets(
						clauseSets,
						buildSQLWhereClauseForMonthGroupIDs(t.monthGroupIDColumnName));
			}
			if (t.sectorIDColumnName != null) {
				addToClauseSets(clauseSets,
						buildSQLWhereClauseForSectors(t.sectorIDColumnName));
			}
			if (t.equipmentTypeIDColumnName != null) {
				addToClauseSets(
						clauseSets,
						buildSQLWhereClauseForNonRoadEquipmentTypes(t.equipmentTypeIDColumnName));
			}

			if (clauseSets.size() <= 0) {
				if (shouldLog)
					System.out.println("IDM No clause sets for " + t.tableName);
				try {
					copyTable(source, destination, t.tableName, "",
							t.isUserInputColumnName,isDefaultDatabase);
				} catch (SQLException ex) {
					/**
					 * @explain A database error occurred while copying a table
					 *          from one database to another.
					 **/
					Logger.logError(ex, "copyTable threw an SQLException");
					throw ex;
				} catch (IOException ex) {
					/**
					 * @explain A file system error occurred while copying a
					 *          table from one database to another.
					 **/
					Logger.logError(ex, "copyTable threw an IOException");
					throw ex;
				}
			} else {
				// Issue a copyTable for each combination of entries within
				// clauseSets
				String log = "IDM " + clauseSets.size() + " clause sets for "
						+ t.tableName + " (";
				int[] counters = new int[clauseSets.size()];
				for (int j = 0; j < clauseSets.size(); j++) {
					counters[j] = 0;
					Vector<String> clauses = clauseSets.get(j);
					if (j > 0) {
						log += ",";
					}
					log += clauses.size();
				}
				log += ")";
				if (shouldLog)
					System.out.println(log);

				boolean done = false;
				do {
					String wholeWhereClause = "";
					for (int j = 0; j < clauseSets.size(); j++) {
						Vector<String> clauses = clauseSets.get(j);
						String clause = clauses.get(counters[j]);
						wholeWhereClause = addToWhereClause(wholeWhereClause,
								clause);
					}

					log = "\t\tdoing clause (";
					for (int j = 0; j < clauseSets.size(); j++) {
						if (j > 0) {
							log += ",";
						}
						log += counters[j];
					}
					log += ") wholeWhereClause is " + wholeWhereClause.length()
							+ " long";
					if (shouldLog)
						System.out.println(log);

					try {
						if (shouldLog)
							System.out.println(wholeWhereClause);
						copyTable(source, destination, t.tableName,
								wholeWhereClause, t.isUserInputColumnName,isDefaultDatabase);
					} catch (SQLException ex) {
						/**
						 * @explain A database error occurred while copying a
						 *          table from one database to another.
						 **/
						Logger.logError(ex, "copyTable threw an SQLException");
						throw ex;
					} catch (IOException ex) {
						/**
						 * @explain A file system error occurred while copying a
						 *          table from one database to another.
						 **/
						Logger.logError(ex, "copyTable threw an IOException");
						throw ex;
					}

					// Move to the next combination of clauses
					int index = 0;
					boolean doneIncrementing = false;
					while (!doneIncrementing) {
						doneIncrementing = true;
						counters[index]++;
						Vector<String> clauses = clauseSets.get(index);
						if (counters[index] >= clauses.size()) {
							doneIncrementing = false;
							counters[index] = 0;
							index++;
							if (index >= clauseSets.size()) {
								done = true;
								doneIncrementing = true;
							}
						}
					}
				} while (!done);
			}
			// Update MySQL's statistics on the table
			SQLRunner.executeSQL(destination, "ANALYZE TABLE " + t.tableName);
		}
		/** @nonissue **/
		Logger.log(LogMessageCategory.INFO,
				"InputDataManager transferred all NonRoad tables.");
	}

	/**
	 * Copies a table from one database to another. Used internally by merge.
	 * It is <b>not</b> an error if the table is not present in the source database.
	 * It <i>is</i> an error if the table is not present in the destination database.
	 * This allows partial databases to be imported easier.
	 * @param source The database to get data from.
	 * @param destination The database to write data to
	 * @param tableName The table name to copy over.
	 * @param whereClause The where clause to filter the rows of the source table with.
	 * @param isUserInputColumnName Column name which is been populated as 'Y' if user inputs data.
	 * @param isDefaultDatabase true when the source database is the default database.
	 * This flag affects conversion of kilometers to miles.
	 * @throws SQLException If an SQL error occurs.
	 * @throws IOException If an IO error occurs while working with a temporary data file.
	**/
	void copyTable(Connection source, Connection destination, String tableName, String whereClause,
			String isUserInputColumnName, boolean isDefaultDatabase) throws SQLException, IOException {
		String sqlConvertToMiles = "";
		String sqlConvertBackToKM = "";
		boolean needToConvertBackToKM = false;
		try {
			String updateSQL = "UPDATE " + tableName + " SET isUserInput=?";
			String alterTable = "ALTER TABLE " + tableName + " ADD ( "
					+ "isUserInput CHAR(1) NOT NULL DEFAULT 'N')";
			PreparedStatement updateStatement = source.prepareStatement(updateSQL);
			String selectSQL = "SELECT * FROM " + tableName + " LIMIT 0";
			PreparedStatement selectSourceStatement = source.prepareStatement(selectSQL);
			PreparedStatement selectDestinationStatement = destination.prepareStatement(selectSQL);
			if(!isDefaultDatabase && CompilationFlags.USE_KILOMETERS_IN_USER_DATA) {
				// Look for tables that contain distance or speed data
				String[] columns = null;
				if(tableName.equalsIgnoreCase("HPMSVtypeYear")) {
					columns = new String[] {
						"HPMSBaseYearVMT"
					};
				} else if(tableName.equalsIgnoreCase("HPMSVtypeDay")) {
					columns = new String[] {
						"VMT"
					};
				} else if(tableName.equalsIgnoreCase("SourceTypeYearVMT")) {
					columns = new String[] {
						"VMT"
					};
				} else if(tableName.equalsIgnoreCase("SourceTypeDayVMT")) {
					columns = new String[] {
						"VMT"
					};
				} else if(tableName.equalsIgnoreCase("Link")) {
					columns = new String[] {
						"linkLength", "linkAvgSpeed"
					};
				} else if(tableName.equalsIgnoreCase("LinkAverageSpeed")) {
					columns = new String[] {
						"averageSpeed"
					};
				} else if(tableName.equalsIgnoreCase("SHO")) {
					columns = new String[] {
						"distance"
					};
				} else if(tableName.equalsIgnoreCase("DriveSchedule")) {
					columns = new String[] {
						"averageSpeed"
					};
				} else if(tableName.equalsIgnoreCase("DriveScheduleSecond")) {
					columns = new String[] {
						"speed"
					};
				} else if(tableName.equalsIgnoreCase("driveScheduleSecondLink")) {
					columns = new String[] {
						"speed"
					};
				}
				if(columns != null && columns.length > 0) {
					String kmPerMile = "1.609344";
					sqlConvertToMiles = "update " + tableName + " set ";
					sqlConvertBackToKM = "update " + tableName + " set ";
					for(int i=0;i<columns.length;i++) {
						if(i > 0) {
							sqlConvertToMiles += ", ";
							sqlConvertBackToKM += ", ";
						}
						sqlConvertToMiles += columns[i] + "=" + columns[i] + "/" + kmPerMile;
						sqlConvertBackToKM += columns[i] + "=" + columns[i] + "*" + kmPerMile;
					}
				}
			}
			if(sqlConvertToMiles.length() > 0) {
				SQLRunner.executeSQL(source, sqlConvertToMiles);
				needToConvertBackToKM = true;
				// Now the user's database contains MILE data and needs to be changed back
				// to KM basis ASAP.
			}
			if(isUserInputColumnName != null) {
				ResultSet result = SQLRunner.executeQuery(selectSourceStatement, selectSQL);

				ResultSetMetaData metaData = result.getMetaData();
				boolean isUserInputColumn = false;
				for(int i=1; i<=metaData.getColumnCount(); i++) {
					if(metaData.getColumnName(i).equalsIgnoreCase("isUserInput")) {
						isUserInputColumn = true;
					}
				}
				if(!isUserInputColumn) {
					SQLRunner.executeSQL(source, alterTable);
				}
				result.close();
				selectSourceStatement.close();
				result = SQLRunner.executeQuery(selectDestinationStatement, selectSQL);
				metaData = result.getMetaData();
				isUserInputColumn = false;
				for(int i=1; i<=metaData.getColumnCount(); i++) {
					if(metaData.getColumnName(i).equalsIgnoreCase("isUserInput")) {
						isUserInputColumn = true;
					}
				}
				if(!isUserInputColumn) {
					SQLRunner.executeSQL(destination, alterTable);
				}
				result.close();
				selectSourceStatement.close();
				updateStatement.setString(1, "Y");
				SQLRunner.execute(updateStatement, updateSQL);
			}
			updateStatement.close();
			boolean sourceHadCopiedData =
					DatabaseUtilities.copyTable(source,destination,tableName,whereClause,false,isDefaultDatabase);
			if(mergeSession != null && sourceHadCopiedData) {
				mergeSession.add(source,tableName);
			}
		} catch( SQLException e ) {
			/**
			 * @issue copyTable threw an SQLException while Copying table [*] from [*] to [*]
			 * @explain An error occurred while moving data withi a table into another database.
			**/
			String s = "copyTable threw an SQLException while Copying table " + tableName +
					" from " + source.getCatalog() + " to " + destination.getCatalog() ;
			Logger.logError( e , s  ) ;
			throw e;
		} finally {
			if(needToConvertBackToKM && sqlConvertBackToKM.length() > 0) {
				try {
					SQLRunner.executeSQL(source, sqlConvertBackToKM);
				} catch(SQLException e) {
					String s = "copyTable threw an SQLException while converting miles to KM in table " + tableName +
							" from " + source.getCatalog();
					Logger.logError( e , s  ) ;
				}
				needToConvertBackToKM = false;
			}
		}
	} // end of copyTable method

	/** Ensure each vehID and dayID combination gets a unique vehID. **/
	public static void createUniqueVehicleIDs() {
		String[] statements = {
			"alter table SampleVehicleDay add column originalVehID int null",
			"alter table SampleVehicleTrip add column originalVehID int null",
			"alter table SampleVehicleTrip add column isConverted int default 0",
			"alter table SampleVehicleDay drop primary key",
			"alter table SampleVehicleDay add index (vehID, dayID)",
			"alter table SampleVehicleDay add column uniqueVehID int not null auto_increment"
					+ ", add primary key (uniqueVehID)",
			"update SampleVehicleDay set originalVehID=vehID",
			"update SampleVehicleTrip set originalVehID=vehID",
			"update SampleVehicleTrip, SampleVehicleDay"
					+ " set SampleVehicleTrip.vehID=SampleVehicleDay.uniqueVehID,"
					+ " SampleVehicleTrip.isConverted=1"
					+ " where SampleVehicleTrip.dayID=SampleVehicleDay.dayID"
					+ " and SampleVehicleTrip.vehID=SampleVehicleDay.vehID"
					+ " and SampleVehicleTrip.isConverted=0",
			"update SampleVehicleDay set vehID=uniqueVehID",
			"alter table SampleVehicleDay drop column uniqueVehID",
			"alter table SampleVehicleDay drop column originalVehID",
			"alter table SampleVehicleTrip drop column originalVehID",
			"alter table SampleVehicleTrip drop column isConverted"
		};
		String sql = "";

		Connection executionConnection = null;
		try {
			executionConnection = DatabaseConnectionManager.checkOutConnection
					(MOVESDatabaseType.EXECUTION);
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(executionConnection, sql);
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Could not create unique vehicle IDs.", sql);
		} finally {
			if (executionConnection != null) {
				DatabaseConnectionManager.checkInConnection
						(MOVESDatabaseType.EXECUTION, executionConnection);
				executionConnection = null;
			}
		}
	}

	private static class MergedTable implements Comparable {
		public String serverName = "";
		public String databaseName = "";
		public String tableName = "";
		public long dataFileSize = 0;
		public long dataFileModified = 0;
		// Use FileTimeUtility.convertFileTimeToString(filePath.lastModified())

		public int compareTo(Object other) {
			if(!(other instanceof MergedTable)) {
				return 1;
			}
			MergedTable o = (MergedTable)other;
			int t = serverName.compareTo(o.serverName);
			if(t == 0) {
				t = databaseName.compareTo(o.databaseName);
				if(t == 0) {
					t = tableName.compareTo(o.tableName);
				}
			}
			return t;
		}
	}

	private static class MergeConnectionInformation {
		public Connection db;
		public String serverName = "";
		public String databaseName = "";
		public File dataFolder = null;
	}

	private static class MergeSession {
		TreeSet<MergedTable> merges = new TreeSet<MergedTable>();
		ArrayList<MergedTable> orderedMerges = new ArrayList<MergedTable>();
		ArrayList<MergeConnectionInformation>
				connections = new ArrayList<MergeConnectionInformation>();
		TreeMap<String,String> folderNameByServerName = new TreeMap<String,String>();

		boolean doShallowOnly = true;
		boolean didShallowTables = false;

		public void add(Connection db, String tableName) {
			MergeConnectionInformation info = getConnectionInformation(db);
			if(info == null) {
				return;
			}
			MergedTable mt = new MergedTable();
			mt.serverName = info.serverName;
			mt.databaseName = info.databaseName;
			mt.tableName = tableName;
			if(merges.contains(mt)) {
				return;
			}
			merges.add(mt);
			orderedMerges.add(mt);
			boolean gotToImportantStep = false;
			try {
				// Obtain the file's size and modification date
				if(info.dataFolder != null && info.dataFolder.exists()) {
					File sourceFolder = new File(info.dataFolder,info.databaseName);
					File sourceMYD = new File(sourceFolder,tableName.toLowerCase() + ".MYD");
					if(sourceMYD.exists()) {
						gotToImportantStep = true;
						mt.dataFileSize = sourceMYD.length();
						mt.dataFileModified = sourceMYD.lastModified();
					}
				}
			} catch(Exception e) {
				if(gotToImportantStep) {
					/**
					 * @explain While details for the files used by a database table, an
					 * error occurred.
					**/
					Logger.logError(e,"Unable to get file information for " + tableName
							+ " while tracking table usage.");
				}
			}
		}

		private MergeConnectionInformation getConnectionInformation(Connection db) {
			MergeConnectionInformation info = null;
			for(Iterator<MergeConnectionInformation> i=connections.iterator();i.hasNext();) {
				info = (MergeConnectionInformation)i.next();
				if(info.db == db) {
					return info;
				}
			}
			info = new MergeConnectionInformation();
			info.db = db;
			String sql = "";
			SQLRunner.Query query = new SQLRunner.Query();
			try {
				// Get database name and server name
				info.databaseName = db.getCatalog();
				info.serverName = db.getMetaData().getURL();
				// Example URL: jdbc:mysql://localhost/proj20090125input
				int startIndex = info.serverName.indexOf("//");
				if(startIndex >= 0) {
					int endIndex = info.serverName.indexOf("/",startIndex+2);
					if(endIndex >= 0) {
						info.serverName = info.serverName.substring(startIndex+2,endIndex);
					}
				}

				// Get and validate the data folder for the database's server
				String dataFolderName = (String)folderNameByServerName.get(info.serverName);
				if(dataFolderName == null) {
					sql = "SHOW VARIABLES";
					query.open(db,sql);
					while(query.rs.next()) {
						String name = query.rs.getString("Variable_name");
						if(name != null && name.equalsIgnoreCase("datadir")) {
							dataFolderName = query.rs.getString("Value");
							break;
						}
					}
					query.close();

					if(dataFolderName == null || dataFolderName.length() <= 0) {
						throw new SQLException("Unable to find datadir variable using SHOW VARIABLES");
					}
					folderNameByServerName.put(info.serverName,dataFolderName);
				}
				File dataFolder = new File(dataFolderName);
				if(!dataFolder.exists()) {
					dataFolder = null; // this happens when working with remote servers
					// throw new IOException("datadir (" + dataFolderName + ") does not exist");
				}
				info.dataFolder = dataFolder;

				connections.add(info);
				return info;
			} catch(Exception e) {
				/**
				 * @explain A database error occurred while gathering table details for auditing.
				**/
				Logger.logError(e,"Unable to get database details for table tracking");
				return null;
			} finally {
				query.onFinally();
			}
		}
	}

	/** Begin tracking tables used within merges. **/
	public static void startMergeSession() {
		mergeSession = new MergeSession();
		mergeSession.doShallowOnly = true;
		mergeSession.didShallowTables = false;
	}
	
	/** Authorize merging of deep tables after all key tables have been merged **/
	public static void advanceMergeSession() {
		mergeSession.doShallowOnly = false;
		mergeSession.didShallowTables = true;
	}

	/**
	 * Store the information tracked during merges into the output database.
	 * @param runID MOVESRun.MOVESRunID to be used within MOVESTablesUsed.MOVESRunID.
	**/
	public static void endMergeSession(int runID) {
		if(mergeSession == null) {
			return;
		}
		String sql = "";
		Connection outputDB = null;
		try {
			outputDB = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.OUTPUT);
			if(outputDB == null) {
				return;
			}
			for(Iterator<MergedTable> i=mergeSession.orderedMerges.iterator();i.hasNext();) {
				MergedTable mt = (MergedTable)i.next();
				sql = "insert into MOVESTablesUsed (MOVESRunID, databaseServer, databaseName,"
						+ "tableName, dataFileSize, dataFileModificationDate) "
						+ " values (" + runID
						+ "," + DatabaseUtilities.escapeSQL(mt.serverName,true)
						+ "," + DatabaseUtilities.escapeSQL(mt.databaseName,true)
						+ "," + DatabaseUtilities.escapeSQL(mt.tableName,true)
						+ "," + (mt.dataFileSize > 0? (""+mt.dataFileSize) : "null")
						+ "," + (mt.dataFileModified != 0? DatabaseUtilities.escapeSQL(
							FileTimeUtility.convertFileTimeToString(mt.dataFileModified),
							true) : "null")
						+ ")";
				SQLRunner.executeSQL(outputDB,sql);
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Unable to store table tracking details",sql);
		} finally {
			if(outputDB != null) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.OUTPUT,outputDB);
				outputDB = null;
			}
		}
		mergeSession = null;
	}
} // end of InputDataManager class
