package gov.epa.otaq.moves.master.nonroad;

import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import gov.epa.otaq.moves.common.TimeSpan;
import gov.epa.otaq.moves.master.framework.DatabaseConnectionManager;
import gov.epa.otaq.moves.utils.FormatUtil;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.sql.Connection;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import org.apache.commons.lang.mutable.MutableDouble;

public class NonroadOptFileHelper {

	private final NonroadOptFileSqlHelper sqlHelper = new NonroadOptFileSqlHelper();

	/**
	 * Default constructor
	 **/
	public NonroadOptFileHelper() {

	}

	/**
	 * Generates the "PERIOD" packet in the OPT file. Assumes optFileWriter has
	 * been initialized.
	 * 
	 * @param monthYear
	 *            The specific month that the simulation should generated data
	 *            for.
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	public boolean generatePeriodPacket(PrintWriter optFileWriter, int yearID,
			int monthID, int dayID, Connection executionDB) throws IOException {

		boolean isOK = true;
		String dayStr = sqlHelper.getDayName(dayID, executionDB);
		String monthName = sqlHelper.getMonthName(monthID, executionDB);
		if (dayStr == null || dayStr.trim().length() <= 0) {
			Logger.log(LogMessageCategory.ERROR, "Failed to get day name.");
			isOK = false;
		}
		if (monthName == null || monthName.trim().length() <= 0) {
			Logger.log(LogMessageCategory.ERROR, "Failed to get month name.");
			isOK = false;
		}

		if (isOK) {
			optFileWriter.println("/PERIOD/");

			optFileWriter.println("Period type        : Monthly");
			optFileWriter.println("Summation type     : TYPICAL DAY"); // Period
																		// total");
			optFileWriter.println("Year of episode    : "
					+ (Integer.valueOf(yearID)).toString());
			optFileWriter.println("Season of year     : ");
			optFileWriter.println("Month of year      : " + monthName);
			optFileWriter.println("Weekday or weekend : " + dayStr);

			optFileWriter.println("/END/");
			optFileWriter.println("");
		}
		return true;
	}

	/**
	 * Generates the "OPTIONS" packet in the OPT file. Assumes optFileWriter has
	 * been initialized.
	 * 
	 * @param representingCounty
	 *            Indicates the county to run the simulation for.
	 * @param monthYear
	 *            The specific month that the simulation should generated data
	 *            for.
	 * @param countyName
	 *            The name of the representing county.
	 * @param countyDatabase
	 *            A connection to the County database.
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	public boolean generateOptionsPacket(PrintWriter optFileWriter,
			int stateID, int countyID, int yearID, int monthID, int dayID,
			String countyName, Connection db) throws IOException {

		boolean ok = true;

		double RVPmkt = 0.0;
		double oxygenWeightPercent = 0.0;
		double gasSulfurPercent = 0.0;
		double dieselSulfurPercent = 0.0;
		double marineDieselSulfurPercent = 0.0;
		double naturalGasSulfurPercent = 0.0;
		double minimumTemperature = 0.0;
		double maximumTemperature = 0.0;
		double averageTemperature = 0.0;
		String altitude = null;
		double ETOHMktShare = 0.0;
		double ETOHVolume = 0.0;

		NonroadOptfileOptionsPacket parameterObject = new NonroadOptfileOptionsPacket(
				RVPmkt,
				oxygenWeightPercent,
				gasSulfurPercent,
				// :
				dieselSulfurPercent, marineDieselSulfurPercent,
				naturalGasSulfurPercent, minimumTemperature,
				maximumTemperature, averageTemperature, altitude, ETOHMktShare,
				ETOHVolume // "EtOH Vol %         : "
		);

		if (ok) {
			ok = sqlHelper.getOptionsParameters(yearID, monthID, dayID,
					stateID, countyID,
					// add as needed
					db, parameterObject);
			if (!ok) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get OPTIONS parameters.");
				return false;
			}
		}

		if (ok) {

			ETOHVolume = parameterObject.geteTOHVolume();
			ETOHMktShare = parameterObject.geteTOHMktShare();
			altitude = parameterObject.getAltitude();
			averageTemperature = parameterObject.getAverageTemperature();
			maximumTemperature = parameterObject.getMaximumTemperature();
			minimumTemperature = parameterObject.getMinimumTemperature();
			naturalGasSulfurPercent = parameterObject
					.getNaturalGasSulfurPercent();
			marineDieselSulfurPercent = parameterObject
					.getMarineDieselSulfurPercent();
			dieselSulfurPercent = parameterObject.getDieselSulfurPercent();
			gasSulfurPercent = parameterObject.getGasSulfurPercent();
			oxygenWeightPercent = parameterObject.getOxygenWeightPercent();
			RVPmkt = parameterObject.getrVPmkt();

			optFileWriter.println("/OPTIONS/");

			optFileWriter
					.println("Title 1            : County Month Test File");
			optFileWriter.println("Title 2            : "
					+ TimeSpan.getMonthByID(monthID) + " " + countyName
					+ " All Equip");

			DecimalFormat df = new DecimalFormat("0.00000000");
			optFileWriter.println("Fuel RVP for gas   : " + df.format(RVPmkt));
			optFileWriter.println("Oxygen Weight %    : "
					+ df.format(oxygenWeightPercent));
			optFileWriter.println("Gas sulfur %       : "
					+ df.format(gasSulfurPercent));
			optFileWriter.println("Diesel sulfur %    : "
					+ df.format(dieselSulfurPercent));
			optFileWriter.println("Marine Dsl sulf %  : "
					+ df.format(marineDieselSulfurPercent));
			optFileWriter.println("CNG/LPG sulfur %   : "
					+ df.format(naturalGasSulfurPercent)); // not available in
															// database for now:
															// 2012-03-07
			optFileWriter.println("Minimum temper. (F): "
					+ df.format(minimumTemperature));
			optFileWriter.println("Maximum temper. (F): "
					+ df.format(maximumTemperature));
			optFileWriter.println("Average temper. (F): "
					+ df.format(averageTemperature));
			optFileWriter.println("Altitude of region : " + altitude);
			optFileWriter.println("EtOH Blend % Mkt   : "
					+ df.format(ETOHMktShare));
			optFileWriter.println("EtOH Vol %         : "
					+ df.format(ETOHVolume));

			optFileWriter.println("/END/");
			optFileWriter.println("");

		}
		return ok;
	}

	/**
	 * Generates the "REGION" packet in the OPT file. Assumes optFileWriter has
	 * been initialized. Assumes countyNameStatement has been initialized.
	 * 
	 * @param stateAbbreviation
	 *            Abbreviation for the state of the representing county
	 * @param representedCounties
	 *            A Set of GeographicSelection objects for the counties being
	 * @param countyDatabase
	 *            A connection to the County database.
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	boolean generateRegionPacket(PrintWriter optFileWriter, int stateID,
			int countyID, Connection executionDB) throws IOException {

		boolean isOK = true;

		String stateAbbr = null;
		String countyName = null;
		NonroadOptfileRegionPacket parameterObject = new NonroadOptfileRegionPacket(
				stateAbbr, countyName);

		isOK = sqlHelper.getRegionParameters(stateID, countyID, executionDB,
				parameterObject);

		if (!isOK) {
			Logger.log(LogMessageCategory.ERROR,
					"Failed to get REGION parameters.");
			isOK = false;
		}

		if (isOK) {
			countyName = parameterObject.getCountyName();
			stateAbbr = parameterObject.getStateAbbr();

			String fips;
			String regionName;
			if (countyID < 1000 && countyID >= 0 && stateID < 100
					&& stateID >= 0) {
				countyID = countyID + stateID * 1000;
			}
			fips = FormatUtil.formatWithLeadingZeros(countyID, 5);
			regionName = FormatUtil.ensureStringLength(countyName + " County "
					+ stateAbbr, 19, ' ');

			optFileWriter.println("/REGION/");
			if(countyID > 0) {
				optFileWriter.println("Region Level       : COUNTY");
				optFileWriter.println(regionName + ": " + fips);
			} else {
				optFileWriter.println("Region Level       : US TOTAL");
			}

			optFileWriter.println("/END/");
			optFileWriter.println("");
		}
		return true;
	}

	/**
	 * @param optFileWriter
	 * @param fuelTypeIDs
	 * @param NREquipTypeIDs
	 * @param executionDB
	 * @return
	 * @throws IOException
	 */
	boolean generateSourceCAategoryPacket(PrintWriter optFileWriter,
			int[] fuelTypeIDs, int[] NREquipTypeIDs, Connection executionDB)
			throws IOException {

		boolean isOK = true;

		TreeSet<String> SCCSelections = new TreeSet<String>();

		isOK = sqlHelper.getSCCs(fuelTypeIDs, NREquipTypeIDs, executionDB,
				SCCSelections);

		if (!isOK) {
			Logger.log(LogMessageCategory.ERROR,
					"Failed to get SOURCE CATEGORY SCCs.");
			return false;
		}

		optFileWriter.println("/SOURCE CATEGORY/");

		for (Iterator<String> i = SCCSelections.iterator(); i.hasNext();) {
			String scc = i.next();
			optFileWriter.println("                   :" + scc);
		}
		optFileWriter.println("/END/");
		optFileWriter.println("");

		return true;
	}

	/**
	 * Generates the "RUNFILES" packet in the OPT file. Assumes optFileWriter
	 * has been initialized.
	 * 
	 * @param shouldGenerateFullPacket
	 *            Whether or not to generate all file fields and close off the
	 *            packet. This should be false if there is a NONROAD packet file
	 *            defined.
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	boolean generateRunFilesPacket(PrintWriter optFileWriter,
			NonroadDataFilesObj nonroadDataFileObj, Connection executionDB)
			throws IOException {

		optFileWriter.println("/RUNFILES/");

		optFileWriter.println("MESSAGE            : "
				+ nonroadDataFileObj.outMsgFile); // nrnmim.msg");
		optFileWriter.println("OUTPUT DATA        : "
				+ nonroadDataFileObj.outDataFile); // nrnmim.out");
		optFileWriter.println("ALLOC XREF         : "
				+ nonroadDataFileObj.allocateXrfFile); // data/allocate/allocate.xrf");
		optFileWriter.println("ACTIVITY           : "
				+ nonroadDataFileObj.activityFile); // data/activity/activity.dat");
		optFileWriter.println("EXH TECHNOLOGY     : "
				+ nonroadDataFileObj.techExhFile); // data/tech/tech-exh.dat");

		optFileWriter.println("EVP TECHNOLOGY     : "
				+ nonroadDataFileObj.techEvpFile); // data/tech/tech-evp.dat");
		//optFileWriter.println("EVP TECHNOLOGY     : C:\\EPA\\NonRoad\\nr2008a\\DATA\\TECH\\TECH-EVP.DAT");

		optFileWriter.println("SEASONALITY        : "
				+ nonroadDataFileObj.seasonFile); // data/season/season.dat");
		optFileWriter.println("REGIONS            : "
				+ nonroadDataFileObj.seasonFile); // data/season/season.dat");
		optFileWriter.println("EPS2 AMS           : ");
		optFileWriter.println("US COUNTIES FIPS   : "
				+ nonroadDataFileObj.fipsFile); // data/allocate/fips.dat");
		optFileWriter.println("RETROFIT           : "
				+ (checkRetrofit(executionDB) ? ""
						: nonroadDataFileObj.retrofitFile));

		optFileWriter.println("/END/");
		optFileWriter.println("");

		return true;
	}

	/**
	 * @param executionDB
	 * @return boolean
	 */
	private boolean checkRetrofit(Connection executionDB) {
		return sqlHelper.checkRetrofit(executionDB);
	}

	/**
	 * Generates the "POP FILES" packet in the OPT file. Assumes optFileWriter
	 * has been initialized. Must be called after sqlHelper.getRegionParameters
	 * called.
	 * 
	 * @param lowerStateAbbreviation
	 *            Lower case abbreviation for the state of the representing
	 *            county
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	boolean generatePopFilesPacket(PrintWriter optFileWriter,
			NonroadDataFilesObj nonroadDataFileObj) throws IOException {

		optFileWriter.println("/POP FILES/");

		for (String file : nonroadDataFileObj.popFiles) {
			optFileWriter.println("Population File    : " + file);
		}

		optFileWriter.println("/END/");
		optFileWriter.println("");

		return true;
	}

	/**
	 * Generates the "GROWTH FILES" packet in the OPT file. Assumes
	 * optFileWriter has been initialized.
	 * 
	 * @return true on success
	 **/
	boolean generateGrowthFilesPacket(PrintWriter optFileWriter,
			NonroadDataFilesObj nonroadDataFileObj) throws IOException {
		optFileWriter.println("/GROWTH FILES/");

		optFileWriter.println("National defaults  : "
				+ nonroadDataFileObj.growthFile); // data/growth/nation.grw");

		optFileWriter.println("/END/");
		optFileWriter.println("");

		return true;
	}

	/**
	 * Generates the "ALLOC FILES" packet in the OPT file. Assumes optFileWriter
	 * has been initialized. Must be called after sqlHelper.getRegionParameters
	 * called.
	 * 
	 * @param lowerStateAbbreviation
	 *            Lower case abbreviation for the state of the representing
	 *            county
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	boolean generateAllocFilesPacket(PrintWriter optFileWriter,
			NonroadDataFilesObj nonroadDataFileObj) throws IOException {

		optFileWriter.println("/ALLOC FILES/");

		optFileWriter.println("Allocation File    :"
				+ nonroadDataFileObj.allAllocateFile);

		/*
		 * String fileName; String lowerStateAbbreviation =
		 * sqlHelper.stateAbbr.trim() .toLowerCase();
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_sbr.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_sbc.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_snowm.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_farms.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_const.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_wob.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_wib.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_golf.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_airtr.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_coal.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_holsl.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_loggn.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_lscap.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_mnfg.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_oil.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_rvprk.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_pop.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_house.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 * 
		 * fileName = "data/allocate/" + lowerStateAbbreviation + "_rail.alo";
		 * optFileWriter.println("Allocation File    :" + fileName);
		 */

		optFileWriter.println("/END/");
		optFileWriter.println("");

		return true;
	}

	/**
	 * Generates the "EMFAC FILES" packet in the OPT file. Assumes optFileWriter
	 * has been initialized.
	 * 
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	boolean generateEMFACFilesPacket(PrintWriter optFileWriter,
			NonroadDataFilesObj nonroadDataFileObj) throws IOException {
		optFileWriter.println("/EMFAC FILES/");

		optFileWriter.println("THC exhaust        : "
				+ (emsInfo.emsExhTHCExist ? nonroadDataFileObj.emsExhTHCFile
						: "")); // data/emsfac/exhthc.emf");
		//optFileWriter.println("THC exhaust        : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EXHTHC.EMF");

		optFileWriter
				.println("CO exhaust         : "
						+ (emsInfo.emsExhCOExist ? nonroadDataFileObj.emsExhCOFile
								: "")); // data/emsfac/exhco.emf");
		//optFileWriter
		//		.println("CO exhaust         : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EXHCO.EMF");

		optFileWriter.println("NOX exhaust        : "
				+ (emsInfo.emsExhNOXExist ? nonroadDataFileObj.emsExhNOXFile
						: "")); // data/emsfac/exhnox.emf");
		//optFileWriter.println("NOX exhaust        : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EXHNOX.EMF");

		optFileWriter
				.println("PM exhaust         : "
						+ (emsInfo.emsExhPMExist ? nonroadDataFileObj.emsExhPMFile
								: "")); // data/emsfac/exhpm.emf");
		//optFileWriter
		//		.println("PM exhaust         : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EXHPM.EMF");

		optFileWriter.println("BSFC               : "
				+ nonroadDataFileObj.emsBSFCFile); // data/emsfac/bsfc.emf");
		//optFileWriter.println("BSFC               : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\BSFC.EMF");

		optFileWriter
				.println("Crankcase          : "
						+ (emsInfo.emsCRANKExist ? nonroadDataFileObj.emsCRANKFile
								: "")); // data/emsfac/crank.emf");
		//optFileWriter
		//		.println("Crankcase          : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\CRANK.EMF");

		optFileWriter
				.println("Spillage           : "
						+ (emsInfo.emsSPILLAGEExist ? nonroadDataFileObj.emsSPILLAGEFile
								: "")); // data/emsfac/spillage.emf");
		//optFileWriter
		//		.println("Spillage           : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\SPILLAGE.EMF");

		optFileWriter
				.println("Diurnal            : "
						+ (emsInfo.emsEvDiuExist ? nonroadDataFileObj.emsEvDiuFile
								: "")); // data/emsfac/evdiu.emf");
		//optFileWriter
		//		.println("Diurnal            : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EVDIU.EMF");

		optFileWriter.println("TANK PERM          : "
				+ (emsInfo.emsEvTANKExist ? nonroadDataFileObj.emsEvTANKFile
						: "")); // vdata/emsfac/evtank.emf");
		//optFileWriter.println("TANK PERM          : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EVTANK.EMF");

		optFileWriter.println("NON-RM HOSE PERM   : "
				+ (emsInfo.emsEvHOSEExist ? nonroadDataFileObj.emsEvHOSEFile
						: "")); // data/emsfac/evhose.emf");
		//optFileWriter.println("NON-RM HOSE PERM   : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EVHOSE.EMF");

		optFileWriter.println("RM FILL NECK PERM  : "
				+ (emsInfo.emsEvNECKExist ? nonroadDataFileObj.emsEvNECKFile
						: "")); // data/emsfac/evneck.emf");
		//optFileWriter.println("RM FILL NECK PERM  : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EVNECK.EMF");

		optFileWriter
				.println("RM SUPPLY/RETURN   : "
						+ (emsInfo.emsEvSUPRETExist ? nonroadDataFileObj.emsEvSUPRETFile
								: "")); // data/emsfac/evsupret.emf");
		//optFileWriter
		//		.println("RM SUPPLY/RETURN   : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EVSUPRET.EMF");

		optFileWriter.println("RM VENT PERM       : "
				+ (emsInfo.emsEvVENTExist ? nonroadDataFileObj.emsEvVENTFile
						: "")); // data/emsfac/evvent.emf");
		//optFileWriter.println("RM VENT PERM       : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EVVENT.EMF");

		optFileWriter.println("HOT SOAKS          : "
				+ (emsInfo.emsEvHOTSKExist ? nonroadDataFileObj.emsEvHOTSKFile
						: "")); // data/emsfac/evhotsk.emf");
		//optFileWriter.println("HOT SOAKS          : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EVHOTSK.EMF");

		optFileWriter.println("RUNINGLOSS         : "
				+ (emsInfo.emsEvRUNLSExist ? nonroadDataFileObj.emsEvRUNLSFile
						: "")); // data/emsfac/evrunls.emf");
		//optFileWriter.println("RUNINGLOSS         : C:\\EPA\\NonRoad\\nr2008a\\DATA\\EMSFAC\\EVRUNLS.EMF");

		optFileWriter.println("/END/");
		optFileWriter.println("");

		return true;
	}

	/**
	 * Generates the "DETERIORATE FILES" packet in the OPT file. Assumes
	 * optFileWriter has been initialized.
	 * 
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	boolean generateDeteriorateFilesPacket(PrintWriter optFileWriter,
			NonroadDataFilesObj nonroadDataFileObj) throws IOException {
		optFileWriter.println("/DETERIORATE FILES/");

		optFileWriter.println("THC exhaust        : "
				+ (emsInfo.detExhTHCExist ? nonroadDataFileObj.detExhTHCFile
						: "")); // data/detfac/exhthc.det");
		optFileWriter
				.println("CO exhaust         : "
						+ (emsInfo.detExhCOExist ? nonroadDataFileObj.detExhCOFile
								: "")); // data/detfac/exhco.det");
		optFileWriter.println("NOX exhaust        : "
				+ (emsInfo.detExhNOXExist ? nonroadDataFileObj.detExhNOXFile
						: "")); // data/detfac/exhnox.det");
		optFileWriter
				.println("PM exhaust         : "
						+ (emsInfo.detExhPMExist ? nonroadDataFileObj.detExhPMFile
								: "")); // data/detfac/exhpm.det");
		optFileWriter.println("DIURNAL            : "); // data/detfac/evdiu.det");
		optFileWriter.println("TANK PERM          : "); // data/detfac/evtank.det");
		optFileWriter.println("NON-RM HOSE PERM   : "); // data/detfac/evhose.det");
		optFileWriter.println("RM FILL NECK PERM  : "); // data/detfac/evneck.det");
		optFileWriter.println("RM SUPPLY/RETURN   : "); // data/detfac/evsupret.det");
		optFileWriter.println("RM VENT PERM       : "); // data/detfac/evvent.det");
		optFileWriter.println("HOT SOAKS          : "); // data/detfac/evhotsk.det");
		optFileWriter.println("RUNINGLOSS         : "); // data/detfac/evrunls.det");

		optFileWriter.println("/END/");
		optFileWriter.println("");

		return true;
	}

	/**
	 * Generates the "STAGE II" packet in the OPT file. Assumes optFileWriter
	 * has been initialized.
	 * 
	 * @param monthYear
	 *            The specific month that the simulation should generated data
	 *            for.
	 * @param countyDatabase
	 *            A connection to the County database.
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	boolean generateStageIIPacket(PrintWriter optFileWriter, int yearID,
			int countyID, Connection executionDB) throws IOException {

		MutableDouble stgIIFac = new MutableDouble(-1);

		boolean isOK = this.sqlHelper.getStageIIParameters(yearID, countyID,
				executionDB, stgIIFac);
		DecimalFormat df = new DecimalFormat("0.0");

		if (isOK) {
			optFileWriter.println("/STAGE II/");

			optFileWriter.println("Control Factor     : "
					+ df.format(stgIIFac.doubleValue())); // 0.0");

			optFileWriter.println("/END/");
			optFileWriter.println("");
		} else {
			Logger.log(LogMessageCategory.ERROR,
					"Failed to get StageII factor.");
		}

		return isOK;
	}

	/**
	 * Generates the "MODELYEAR OUT" packet in the OPT file. Assumes
	 * optFileWriter has been initialized.
	 * 
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot written to.
	 **/
	boolean generateModelYearOutPacket(PrintWriter optFileWriter,
			NonroadDataFilesObj nonroadDataFileObj) throws IOException {

		/** NR_IMP: change to be based on GUI Model Year option **/

		optFileWriter.println("/MODELYEAR OUT/");
		optFileWriter.println("EXHAUST BMY OUT    :"
				+ nonroadDataFileObj.outExhBMYFile); // OUTPUTS/NRNMIM.BMX");
		optFileWriter.println("EVAP BMY OUT       :"
				+ nonroadDataFileObj.outEvBMYFile); // OUTPUTS/NRNMIM.BMV");
		optFileWriter.println("/END/");
		optFileWriter.println("");
		return true;
	}

	/**
	 * Generates the "SI REPORT" packet in the OPT file. Assumes optFileWriter
	 * has been initialized.
	 * 
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot be written to.
	 **/
	boolean generateSIReportPacket(PrintWriter optFileWriter)
			throws IOException {
		/*
		 * optFileWriter.println("/SI REPORT/");
		 * 
		 * optFileWriter.println("SI report file-CSV :outputs/nrpollut.csv");
		 * 
		 * optFileWriter.println("/END/"); optFileWriter.println("");
		 */

		return true;
	}

	/**
	 * Generates the "PM BASE SULFUR" packet in the OPT file. Assumes
	 * optFileWriter has been initialized.
	 * 
	 * @return true on success
	 * @throws IOException
	 *             If the OPT file cannot be written to. Default is 0.3300 and
	 *             0.02247. Value of 1.0 means no adjustment and 2nd value is
	 *             ignored.
	 **/
	boolean generatePMBaseSulfurPacket(PrintWriter optFileWriter,
			Connection executionDB) throws IOException {

		List<NonroadPMBasedSulfur> pmBasedSulfurList = new ArrayList<NonroadPMBasedSulfur>();
		boolean isOK = sqlHelper.getPMBasedSulfur(executionDB,
				pmBasedSulfurList);

		if (!isOK) {
			Logger.log(LogMessageCategory.ERROR,
					"Failed to get PM BasedSulfur packet information.");
		}

		if (pmBasedSulfurList == null || pmBasedSulfurList.size() <= 0) {
			Logger.log(LogMessageCategory.ERROR,
					"No PM Based Sulfur information available.");
		}
		optFileWriter.println("/PM BASE SULFUR/");

		String format = "%1$-10s%2$-10.4f%3$-10.5f";
		for (NonroadPMBasedSulfur sul : pmBasedSulfurList) {
			optFileWriter.printf(format, sul.techType, sul.pmBaseSulfur,
					sul.sufatePNConversionFac);
			optFileWriter.print(System.getProperty("line.separator"));
		}

		/*
		 * optFileWriter.println("T2        0.2000    0.02247");
		 * optFileWriter.println("T3        0.2000    0.02247");
		 * optFileWriter.println("T3B       0.0500    0.02247");
		 * optFileWriter.println("T4A       0.0500    0.02247");
		 * optFileWriter.println("T4B       0.0015    0.02247");
		 * optFileWriter.println("T4        0.0015    0.30");
		 * optFileWriter.println("T4N       0.0015    0.30");
		 * optFileWriter.println("T2M       0.0350    0.02247");
		 * optFileWriter.println("T3M       1.0       0.02247");
		 * optFileWriter.println("T4M       1.0       0.02247");
		 */

		optFileWriter.println("/END/");
		optFileWriter.println("");

		return true;
	}

	public boolean generateOptFile(//
			Connection executionDB,//
			// String baseDir, //
			// String fileName,//
			PrintWriter optFileWriter,//
			int stateID, //
			int countyID, //
			String countyName, //
			int yearID, //
			int monthID, //
			int dayID, // weekday or weekend
			int[] fuelTypeIDs, //
			int[] NREquipTypeIDs, //
			NonroadDataFilesObj nonroadDataFileObj //
	) throws IOException {

		boolean isOK = true;
		// PrintWriter optFileWriter = new PrintWriter(new BufferedWriter(
		// new OutputStreamWriter(new FileOutputStream(new File(baseDir
		// + System.getProperty("file.separator") + fileName)))));

		if (countyID < 1000 && countyID >= 0 && stateID < 100 && stateID >= 0) {
			countyID = countyID + stateID * 1000;
		}

		isOK = this.generatePeriodPacket(optFileWriter, yearID, monthID, dayID,
				executionDB);

		if (isOK) {
			isOK = this.generateOptionsPacket(optFileWriter, stateID, countyID,
					yearID, monthID, dayID, countyName, executionDB);
		}

		if (isOK) {
			isOK = this.generateRegionPacket(optFileWriter, stateID, countyID,
					executionDB);
		}

		if (isOK) {
			isOK = this.generateSourceCAategoryPacket(optFileWriter,
					fuelTypeIDs, NREquipTypeIDs, executionDB);
		}

		if (isOK) {
			isOK = this.generateRunFilesPacket(optFileWriter,
					nonroadDataFileObj, executionDB);
		}

		if (isOK) {
			isOK = this.generatePopFilesPacket(optFileWriter,
					nonroadDataFileObj);
		}

		if (isOK) {
			isOK = this.generateGrowthFilesPacket(optFileWriter,
					nonroadDataFileObj);
		}

		if (isOK) {
			isOK = this.generateAllocFilesPacket(optFileWriter,
					nonroadDataFileObj);
		}

		if (isOK) {
			isOK = this.generateEMFACFilesPacket(optFileWriter,
					nonroadDataFileObj);
		}

		if (isOK) {
			isOK = this.generateDeteriorateFilesPacket(optFileWriter,
					nonroadDataFileObj);
		}

		if (isOK) {
			isOK = this.generateStageIIPacket(optFileWriter, yearID, countyID,
					executionDB);
		}

		if (isOK) {
			isOK = this.generateModelYearOutPacket(optFileWriter,
					nonroadDataFileObj);
		}

		if (isOK) {
			isOK = this.generatePMBaseSulfurPacket(optFileWriter, executionDB);
		}

		optFileWriter.close();

		return isOK;
	}

	public static void main(String[] args) {
		boolean testDataGeneration = true;
		boolean testOptfileGeneration = true;

		Connection db;
		try {
			db = DatabaseConnectionManager
					.checkOutConnection(MOVESDatabaseType.DEFAULT);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return;
		}

		NonroadDataFileHelper dataHelper = new NonroadDataFileHelper();
		NonroadDataFilesObj nonroadDataFileObj = new NonroadDataFilesObj();
		NonroadOptFileHelper helper = new NonroadOptFileHelper();
		NonroadEmissionFacInfo emsInfo = new NonroadEmissionFacInfo();
		helper.setEmsInfo(emsInfo);

		if (testDataGeneration) {
			int[] statIDs = { 1, 2 };
			dataHelper.generateDataFiles(db, statIDs,
					"C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\nonroad_20120919",
					true, true, true, nonroadDataFileObj);
		}

		try {
			String baseDir = "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\nonroad_20120919";
			String fileName = "NONROAD.OPT";
			PrintWriter optFileWriter = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(new File(
							baseDir + System.getProperty("file.separator")
									+ fileName)))));
			if (testOptfileGeneration) {
				int[] fuelTypeIDs = { 1, 2 };
				int[] equipTypeIDs = { 1, 2 };
				try {
					helper.generateOptFile(db, optFileWriter, 1, 1,
							"countyName", 2002, 1, 2, fuelTypeIDs,
							equipTypeIDs, nonroadDataFileObj);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					return;
				}
			}
		} catch (FileNotFoundException e) {

		}

	}

	private NonroadEmissionFacInfo emsInfo = null;

	/**
	 * @param emsInfo
	 */
	public void setEmsInfo(NonroadEmissionFacInfo emsInfo) {
		this.emsInfo = emsInfo;
	}
}
