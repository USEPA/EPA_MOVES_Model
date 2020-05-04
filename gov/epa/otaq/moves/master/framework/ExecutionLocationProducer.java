/**************************************************************************************************
 * @(#)ExecutionLocationProducer.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.framework;

import java.util.Iterator;
import java.util.TreeSet;
import gov.epa.otaq.moves.master.runspec.*;
import java.sql.*;
import gov.epa.otaq.moves.common.*;

/**
 * Produces a list of ExecutionLocations from the GeographicSelections in the RunSpec. Each
 * GeographicSelection identifies many individual locations over which the MasterLoop is executed.
 * The ExecutionLocationProducer uses information from the GeographicSelections to construct SQL
 * queries on the Link table. These queries return the individual link records that the
 * list of ExecutionLocations is constructed from.
 *
 * @author		Wesley Faler
 * @version		2013-09-30
**/
public class ExecutionLocationProducer {
	/**
	 * The collection of GeographicSelection objects to create the ExecutionLocation objects from
	**/
	TreeSet<GeographicSelection> geographicSelections;
	/**
	 * The collection of RoadType objects that are used to filter the links
	 * retrieved from the database.  May be null or empty to denote no restrictions.
	**/
	TreeSet<RoadType> roadTypes;
	/**
	 * SQL for appending to a WHERE clause that would limit the values of the roadType column.
	 * Of the format " AND (roadTypeID=1 OR roadTypeID=2)" so that it can be directly appended
	 * to SQL that already has a WHERE clause and an unambiguous column named "roadTypeID".
	**/
	String roadTypesSQL;
	/** Temporary connection to the default database. **/
	Connection db;

	/** ExecutionLocation objects to be looped over by MasterLoop **/
	TreeSet<ExecutionLocation> results;

	/** RunSpec providing the source for the locations **/
	RunSpec runSpec;

	/** True when the Nonroad road type is present. **/
	boolean hasNonroad = false;
	/** True when any Non-Nonroad road type is present. **/
	boolean hasOnroad = false;

	/**
	 * Constructor.
	 * @param runSpecToUse RunSpec providing the source for the locations.
	**/
	public ExecutionLocationProducer(RunSpec runSpecToUse) {
		runSpec = runSpecToUse;
		geographicSelections = runSpec.geographicSelections;
		roadTypes = ExecutionRunSpec.theExecutionRunSpec.getRoadTypes();
		results = new TreeSet<ExecutionLocation>();
		db = null;
		buildRoadTypesSQL();
	}

	/**
	 * Builds part of the SQL WHERE clause that limits the links returned to those matching the
	 * roadTypes selected in the runSpec.
	**/
	void buildRoadTypesSQL() {
		roadTypesSQL = new String("");
		if(roadTypes != null) {
			boolean hasText = false;
			for(Iterator<RoadType> i = roadTypes.iterator(); i.hasNext();) {
				RoadType roadType = (RoadType)i.next();
				if(roadType.roadTypeID == 100) {
					hasNonroad = true;
					continue;
				} else {
					hasOnroad = true;
				}
				if(hasText) {
					roadTypesSQL += " OR ";
				}
				hasText = true;
				roadTypesSQL += "roadTypeID=" + roadType.roadTypeID;
			}
			if(hasText) {
				roadTypesSQL = " AND (" + roadTypesSQL + ")";
			}
		}
	}

	/**
	 * Builds ExecutionLocation objects from the previously specified GeographicSelection objects
	 * and RoadType objects.
	 * @param targetDB The database to build the execution locations from.
	 * @throws InterruptedException If the active thread is interrupted.
	 * @return The collection of ExecutionLocation objects.
	**/
	public TreeSet<ExecutionLocation> buildExecutionLocations(Connection targetDB)
			throws InterruptedException {
		if(runSpec.isCustomDomain()) {
			db = targetDB;
			try {
				int countyID = runSpec.genericCounty.getCountyID();
				addCountyLocations(countyID);
			} finally {
				db = null;
			}
			/*
			if(runSpec.scale == ModelScale.MESOSCALE_LOOKUP || runSpec.domain == ModelDomain.PROJECT) {
				db = targetDB;
				try {
					int countyID = runSpec.genericCounty.getCountyID();
					addZoneLocations(countyID*10);
				} finally {
					db = null;
				}
			} else {
				for(Iterator<RoadType> i = roadTypes.iterator(); i.hasNext();) {
					RoadType roadType = (RoadType)i.next();
					int countyID = runSpec.genericCounty.getCountyID();
					addLinkLocation(runSpec.genericCounty.stateID,countyID,countyID*10,
							countyID*100+roadType.roadTypeID,roadType.roadTypeID);
				}
			}
			*/
		} else {
			db = targetDB;
			try {
				for(Iterator<GeographicSelection> i = geographicSelections.iterator();
						i.hasNext();) {
					GeographicSelection iterSelection = (GeographicSelection) i.next();
					buildExecutionLocations(iterSelection);
				}
			} finally {
				db = null;
			}
		}
		return results;
	}

	/**
	 * Builds ExecutionLocations based on the scale of the GeographicSelection.
	 * @param sourceSelection The GeographicSelection objects to create the ExecutionLocation
	 * objects from.
	**/
	void buildExecutionLocations(GeographicSelection sourceSelection) {
		if(sourceSelection.type == GeographicSelectionType.LINK) {
			addLinkLocations(sourceSelection.databaseKey);
		} else if(sourceSelection.type == GeographicSelectionType.ZONE) {
			addZoneLocations(sourceSelection.databaseKey);
		} else if(sourceSelection.type == GeographicSelectionType.COUNTY) {
			addCountyLocations(sourceSelection.databaseKey);
		} else if(sourceSelection.type == GeographicSelectionType.STATE) {
			addStateLocations(sourceSelection.databaseKey);
		} else if(sourceSelection.type == GeographicSelectionType.NATION) {
			addNationLocations();
		}
	}

	/**
	 * Creates and adds an ExecutionLocation to the list.
	 * @param stateID The State ID to use.
	 * @param countyID The County ID to use.
	 * @param zoneID The Zone ID to use.
	 * @param linkID The Link ID to use.
	 * @param roadTypeID The Road Type ID to use.
	**/
	void addLinkLocation(int stateID, int countyID, int zoneID, int linkID, int roadTypeID) {
		//System.out.println("addLinkLocations: s=" + stateID + ", c=" + countyID + ", z=" + zoneID + ", l=" + linkID + ", r=" + roadTypeID);
		ExecutionLocation location = new ExecutionLocation();
		location.stateRecordID = stateID;
		location.countyRecordID = countyID;
		location.zoneRecordID = zoneID;
		location.linkRecordID = linkID;
		location.roadTypeRecordID = roadTypeID;
		results.add(location);
	}

	/**
	 * Prepares an SQL statement to return the locations for a given link.
	 * @param linkID The Link ID to use
	**/
	void addLinkLocations(int linkID) {
		String sql = "";
		PreparedStatement statement = null;
		try {
			if(hasOnroad) {
				sql = "SELECT County.stateID,County.countyID,Link.zoneID,Link.linkID,Link.roadTypeID "
						+ " FROM County, Link"
						+ " WHERE Link.linkID = ? "
						+ " AND County.countyID = Link.countyID";
				sql += roadTypesSQL;
				statement = db.prepareStatement(sql);
				statement.setInt(1,linkID);
				addLocationsFromSQL(sql,statement);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e, "Adding execution locations for link, "+linkID+", failed.", sql);
		}
	}

	/**
	 * Prepares an SQL statement to return the locations for a given zone.
	 * @param zoneID The Zone ID to use
	**/
	void addZoneLocations(int zoneID) {
		String sql = "";
		PreparedStatement statement = null;
		try {
			if(hasOnroad) {
				sql = "SELECT County.stateID,County.countyID,Link.zoneID,Link.linkID,Link.roadTypeID "
						+ " FROM County, Link"
						+ " WHERE Link.zoneID = ? "
						+ " AND County.countyID = Link.countyID";
				sql += roadTypesSQL;
				statement = db.prepareStatement(sql);
				statement.setInt(1,zoneID);
				addLocationsFromSQL(sql,statement);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e, "Adding execution locations for zone, "+zoneID+", failed.", sql);
		}
	}

	/**
	 * Prepares an SQL statement to return the locations for a given county.
	 * @param countyID The County ID to use.
	**/
	void addCountyLocations(int countyID) {
		String sql = "";
		PreparedStatement statement = null;
		try {
			if(hasOnroad) {
				sql = "SELECT County.stateID,County.countyID,Link.zoneID,Link.linkID,Link.roadTypeID "
						+ " FROM County, Link"
						+ " WHERE County.countyID = ?"
						+ " AND County.countyID = Link.countyID";
				sql += roadTypesSQL;
				statement = db.prepareStatement(sql);
				statement.setInt(1,countyID);
				//System.out.println("addCountyLocations: countyID=" + countyID + ", sql=" + sql);
				addLocationsFromSQL(sql,statement);
			}
			if(hasNonroad) {
				sql = "SELECT County.stateID,County.countyID,County.countyID as zoneID,County.countyID as linkID,100 as roadTypeID "
						+ " FROM County"
						+ " WHERE County.countyID = ?";
				statement = db.prepareStatement(sql);
				statement.setInt(1,countyID);
				addLocationsFromSQL(sql,statement);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e, "Adding execution locations for county, "+countyID+", failed.",
					sql);
		}
	}

	/**
	 * Prepares an SQL statement to return the locations for a given state.
	 * @param stateID The State ID to use.
	**/
	void addStateLocations(int stateID) {
		String sql = "";
		PreparedStatement statement = null;
		try {
			if(hasOnroad) {
				sql = "SELECT County.stateID,County.countyID,Link.zoneID,Link.linkID,Link.roadTypeID "
						+ " FROM County, Link"
						+ " WHERE County.stateID = ? "
						+ " AND County.countyID = Link.countyID";
				sql += roadTypesSQL;
				statement = db.prepareStatement(sql);
				statement.setInt(1,stateID);
				//System.out.println("addStateLocations: stateID=" + stateID + ", sql=" + sql);
				addLocationsFromSQL(sql,statement);
			}
			if(hasNonroad) {
				sql = "SELECT County.stateID,County.countyID,County.countyID as zoneID,County.countyID as linkID,100 as roadTypeID "
						+ " FROM County"
						+ " WHERE County.stateID = ?";
				statement = db.prepareStatement(sql);
				statement.setInt(1,stateID);
				addLocationsFromSQL(sql,statement);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e, "Adding execution locations for state, "+stateID+", failed.",
					sql);
		}
	}

	/**
	 * Prepares an SQL statement to return the locations for the nation.
	**/
	void addNationLocations() {
		String sql = "";
		PreparedStatement statement = null;
		try {
			if(hasOnroad) {
				sql = "SELECT State.stateID,County.countyID,Link.zoneID,Link.linkID,Link.roadTypeID "
						+ " FROM State, County, Link"
						+ " WHERE State.stateID = County.stateID "
						+ " AND County.countyID = Link.countyID";
				sql += roadTypesSQL;
				statement = db.prepareStatement(sql);
				//System.out.println("addNationLocation: sql=" + sql);
				addLocationsFromSQL(sql,statement);
			}
			if(hasNonroad) {
				sql = "SELECT County.stateID,County.countyID,County.countyID as zoneID,County.countyID as linkID,100 as roadTypeID "
						+ " FROM County";
				statement = db.prepareStatement(sql);
				addLocationsFromSQL(sql,statement);
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,"Adding execution locations for the nation failed.", sql);
		}
	}

	/**
	 * Centralized routine for querying for links and adding them to the set of locations to
	 * be processed.
	 * @param sql SQL used to create the statement
	 * @param statement PreparedStatement that yields a list of links to be added to the result
	 * set.  The columns should be stateID, countyID, zoneID, linkID, roadTypeID in that sequence,
	 * though the results don't have to be ORDER BY'd in any way.  Statement is always closed
	 * by this routine.
	**/
	void addLocationsFromSQL(String sql, PreparedStatement statement) {
		ResultSet rs = null;
		try {
			rs = SQLRunner.executeQuery(statement,sql);
			if(rs != null) {
				while(rs.next()) {
					addLinkLocation(rs.getInt(1),rs.getInt(2),rs.getInt(3),
							rs.getInt(4),rs.getInt(5));
				}
			}
		} catch(SQLException e) {
			Logger.logSqlError(e,"An SQL exception occurred while adding execution locations.",
					sql);
		} finally {
			if(rs != null) {
				try {
					rs.close();
				} catch(Exception e) {
					// Nothing can be done here.  In fact, it may be a non-issue if
					// rs isn't even open.
				}
			}
			if(statement != null) {
				try {
					statement.close();
				} catch(Exception e) {
					// Nothing can be done here.  In fact, it may be a non-issue if
					// statement isn't even open.
				}
			}
		}
	}
}
