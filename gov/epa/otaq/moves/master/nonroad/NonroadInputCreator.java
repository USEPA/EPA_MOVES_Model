package gov.epa.otaq.moves.master.nonroad;

import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.OffRoadVehicleSelection;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.master.framework.ExecutionLocation;
import gov.epa.otaq.moves.master.framework.ExecutionRunSpec;
import gov.epa.otaq.moves.master.framework.MasterLoopContext;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

public class NonroadInputCreator {

	// make this a singleton
	public static NonroadInputCreator theNonroadInputCreator = new NonroadInputCreator();

	// the following will be used once for each moves-nonroad run
	NonroadDataFileHelper dataHelper = new NonroadDataFileHelper();
	NonroadDataFilesObj nonroadDataFileObj = new NonroadDataFilesObj();

	// the following will be used once for each bundle
	NonroadOptFileHelper optHelper = new NonroadOptFileHelper();

	// theExecutionRunSpec
	ExecutionRunSpec exeRunSpec = null;

	NonroadEmissionFacInfo emsInfo = null;

	int[] stateIDs;
	int[] fuelTypeIDs;
	int[] sectorIDs;
	int[] NREquipTypeIDs;

	// prevent to be created outside
	private NonroadInputCreator() {
	}

	// called once before the starting of a masterLoop, and bundle the data
	// files into a jar w/ folder structures
	public NonroadDataFilesObj reset(String baseDir, Connection executionDB) {
		Logger.log(LogMessageCategory.INFO,"NonroadInputCreator.reset called");
		exeRunSpec = ExecutionRunSpec.theExecutionRunSpec;
		stateIDs = null;
		fuelTypeIDs = null;
		sectorIDs = null;
		NREquipTypeIDs = null;
		getStateIDs();
		getEquipInfo(executionDB);
		createDataFiles(baseDir, executionDB);
		return nonroadDataFileObj;
	}

	private int[] getStateIDs() {
		TreeSet<Integer> stateIDs = new TreeSet<Integer>();

		for (Iterator<ExecutionLocation> executionLocationIter = exeRunSpec
				.getExecutionLocations().iterator(); executionLocationIter
				.hasNext();) {
			ExecutionLocation iterLocation = executionLocationIter.next();
			stateIDs.add(iterLocation.stateRecordID);
		}
		this.stateIDs = NonroadHelper.toIntArray(stateIDs);
		return this.stateIDs;
	}

	private boolean getEquipInfo(Connection db) {
		boolean isOK = true;

		TreeSet<Integer> fuelTypeIDList = new TreeSet<Integer>();
		TreeSet<Integer> sectorTypeIDList = new TreeSet<Integer>();
		TreeSet<Integer> NREquipTypeIDList = new TreeSet<Integer>();

		TreeSet<OffRoadVehicleSelection> veclSelections = exeRunSpec
				.getOffRoadVehicleSelections();
		for (OffRoadVehicleSelection veclSelection : veclSelections) {
			fuelTypeIDList.add(veclSelection.fuelTypeID);
			sectorTypeIDList.add(veclSelection.sectorID);
		}
		this.fuelTypeIDs = NonroadHelper.toIntArray(fuelTypeIDList);
		this.sectorIDs = NonroadHelper.toIntArray(sectorTypeIDList);

		String sectorIDString = "";
		for (int sectorID : sectorIDs) {
			sectorIDString += "" + sectorID + ",";
		}
		if (sectorIDString.endsWith(",")) {
			sectorIDString = sectorIDString.substring(0,
					sectorIDString.length() - 1);
		}

		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			String sql = "select distinct e.NREquipTypeID from nrequipmenttype e where e.sectorID in ("
					+ sectorIDString + ");";
			statement = db.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				while (results.next()) {
					NREquipTypeIDList.add(results.getInt(1));
				}
			}
			this.NREquipTypeIDs = NonroadHelper.toIntArray(NREquipTypeIDList);
		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in querying NREquipTypeID by sectors from nrequipmenttype table : "
							+ e);
			isOK = false;
		} finally {
			if (statement != null) {
				try {
					if (!statement.isClosed()) {
						statement.close();
					}
				} catch (SQLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				statement = null;
			}
			if (results != null) {
				try {
					if (!results.isClosed()) {
						results.close();
					}
				} catch (SQLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				results = null;
			}
		}

		return isOK;
	}

	private boolean createDataFiles(String baseDir, Connection db) {

		boolean isOK = true;

		emsInfo = new NonroadEmissionFacInfo();
		isOK = dataHelper
				.checkDetEmsFacExistence(db, emsInfo, true, true, true);

		isOK = dataHelper.generateDataFiles(db, stateIDs, baseDir, true, true,
				true, nonroadDataFileObj); // use the default relative file
											// paths and fill in POP relative
											// file paths
		return isOK;
	}

	// called once whenever a bundle generated
	public boolean generateOPTFile(MasterLoopContext context,
			PrintWriter optFileWriter, Connection executionDB) {
		boolean isOK = true;

		ExecutionLocation location = context.iterLocation;
		int stateID = location.stateRecordID;
		int countyID = location.countyRecordID;
		int yearID = context.year;
		int monthID = context.monthID;
		int dayID = context.dayID; // weekday or weekend

		NonroadOptFileHelper helper = new NonroadOptFileHelper();
		helper.setEmsInfo(emsInfo);

		try {
			helper.generateOptFile(executionDB, optFileWriter, stateID,
					countyID, " ", yearID, monthID, dayID, this.fuelTypeIDs,
					this.NREquipTypeIDs, this.nonroadDataFileObj);
		} catch (IOException e) {
			e.printStackTrace();
			isOK = false;
		}

		return isOK;
	}

}
