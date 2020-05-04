/**************************************************************************************************
 * @(#)BaseRateByAgeHelper.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.util.*;
import java.sql.*;
import java.io.*;


/**
 * This builds the BaseRateByAge table directly without using SQL joins.
 *
 * @author		Wesley Faler
 * @version		2014-07-22
**/
public class BaseRateByAgeHelper {
	public static boolean isTest = false;

	/** Database connection to the execution database **/
	Connection db;

	/**
	 * Constructor
	 * @param dbToUse Database connection to the execution database
	**/
	public BaseRateByAgeHelper(Connection dbToUse) {
		db = dbToUse;
	}

	/*
		RatesOpModeDistribution
			sourceTypeID
			roadTypeID
			avgSpeedBinID
			hourDayID
			polProcessID
			opModeID
			opModeFraction
			avgBinSpeed
			avgSpeedFraction

		SBWeightedEmissionRateByAge
			sourceTypeID
			polProcessID
			opModeID
			modelYearID
			fuelTypeID
			ageGroupID
			regClassID
			meanBaseRate
			meanBaseRateIM
			meanBaseRateACAdj
			meanBaseRateIMACAdj
			sumSBD
			sumSBDRaw

		BaseRateByAge_<processID>_<yearID>
			sourceTypeID
			roadTypeID
			avgSpeedBinID
			hourDayID
			polProcessID
			pollutantID
			processID
			modelYearID
			fuelTypeID
			ageGroupID
			regClassID
			opModeID
			meanBaseRate
			meanBaseRateIM
			meanBaseRateACAdj
			meanBaseRateIMACAdj
			emissionRate
			emissionRateIM
			emissionRateACAdj
			emissionRateIMACAdj
			opModeFraction
			opModeFractionRate
	*/

	/** Holds one row of the SBWeightedEmissionRateByAge table **/
	static class SBWeightedEmissionRateByAge {
		/*
			+ " 		er.sourceTypeID = romd.sourceTypeID" Constant
			+ " 		and er.polProcessID = romd.polProcessID" Constant
			+ " 		and er.opModeID = romd.opModeID" Key
		*/
		//int sourceTypeID, polProcessID; // should be constants since ROMD is filtered to be constant.
		//int opModeID; // key
		/*
			+ " group by"
			+ " 	romd.sourceTypeID, romd.polProcessID, romd.roadTypeID, romd.hourDayID,"
			+ (!useAvgSpeedBin? "" : " romd.avgSpeedBinID,")
			+ " 	er.modelYearID, er.fuelTypeID, er.ageGroupID, er.regClassID",
		*/
		int modelYearID, fuelTypeID, ageGroupID, regClassID; // output GROUP BY partial key
		String partialKey = "";

		double sumSBD;
		double sumSBDRaw;
		double MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj;

		void fill(ResultSet rs) throws SQLException {
			modelYearID = rs.getInt("modelYearID");
			fuelTypeID = rs.getInt("fuelTypeID");
			ageGroupID = rs.getInt("ageGroupID");
			regClassID = rs.getInt("regClassID");
			partialKey = "|" + modelYearID + "|" + fuelTypeID + "|" + ageGroupID + "|" + regClassID;
			
			sumSBD = rs.getDouble("sumSBD");
			sumSBDRaw = rs.getDouble("sumSBDRaw");

			MeanBaseRate = rs.getFloat("MeanBaseRate");
			MeanBaseRateIM = rs.getFloat("MeanBaseRateIM");
			MeanBaseRateACAdj = rs.getFloat("MeanBaseRateACAdj");
			MeanBaseRateIMACAdj = rs.getFloat("MeanBaseRateIMACAdj");
		}
	}

	static class OutputRecord {
		/*
			+ " group by"
			+ " 	romd.sourceTypeID, romd.polProcessID, romd.roadTypeID, romd.hourDayID,"
			+ (!useAvgSpeedBin? "" : " romd.avgSpeedBinID,")
			+ " 	er.modelYearID, er.fuelTypeID, er.ageGroupID, er.regClassID",

			Key is:
			romd.sourceTypeID + romd.polProcessID + romd.roadTypeID + romd.hourDayID
			+ (!useAvgSpeedBin? "" : " romd.avgSpeedBinID,")
			+ partial key from SBWeightedEmissionRateByAge.
		*/

		// All fields are in the same order as the target output table: BaseRateByAge_<processID>_<yearID>
		int sourceTypeID = 0;
		int roadTypeID = 0;
		int avgSpeedBinID = 0;
		int hourDayID = 0;
		int polProcessID = 0;
		int pollutantID = 0;
		int processID = 0;
		int modelYearID = 0;
		int fuelTypeID = 0;
		int ageGroupID = 0;
		int regClassID = 0;
		int opModeID = 0;
		double meanBaseRate = 0;
		double meanBaseRateIM = 0;
		double meanBaseRateACAdj = 0;
		double meanBaseRateIMACAdj = 0;
		double emissionRate = 0;
		double emissionRateIM = 0;
		double emissionRateACAdj = 0;
		double emissionRateIMACAdj = 0;
		double opModeFraction = 0;
		double opModeFractionRate = 0;
		
		void write(PrintWriter writer) throws IOException {
			String delimiter = "\t";
			
			writer.print("" + sourceTypeID
					+ delimiter + roadTypeID
					+ delimiter + avgSpeedBinID
					+ delimiter + hourDayID
					+ delimiter + polProcessID
					+ delimiter + pollutantID
					+ delimiter + processID
					+ delimiter + modelYearID
					+ delimiter + fuelTypeID
					+ delimiter + ageGroupID
					+ delimiter + regClassID
					+ delimiter + opModeID
					+ delimiter + (double)meanBaseRate
					+ delimiter + (double)meanBaseRateIM
					+ delimiter + (double)meanBaseRateACAdj
					+ delimiter + (double)meanBaseRateIMACAdj
					+ delimiter + (double)emissionRate
					+ delimiter + (double)emissionRateIM
					+ delimiter + (double)emissionRateACAdj
					+ delimiter + (double)emissionRateIMACAdj
					+ delimiter + (double)opModeFraction
					+ delimiter + (double)opModeFractionRate
					+ "\n");
		}
	}

	/**
	 * Emission rates keyed by opModeID. sourceTypeID and polProcessID are all constant and equal
	 * to all rows in ROMD since ROMD is filtered by these.
	**/
	TreeMap<Integer,ArrayList<SBWeightedEmissionRateByAge>> erByOpModeID = new TreeMap<Integer,ArrayList<SBWeightedEmissionRateByAge>>();

	/** Accumulated output data, keyed by the complex key of the target table and application logic **/
	TreeMap<String,OutputRecord> outputRecords = new TreeMap<String,OutputRecord>();

	public static class Flags {
		public boolean keepOpModeID;
		public boolean useAvgSpeedBin;
		public boolean useAvgSpeedFraction;
		public boolean useSumSBD;
		public boolean useSumSBDRaw;
	}
	
	public static class Context {
		public int processID;
		public int yearID;
		public int sourceTypeID;
		public int polProcessID;
		public int roadTypeID; // 0 if no specific road type is needed
		
		public void copyFrom(Context other) {
			processID = other.processID;
			yearID = other.yearID;
			sourceTypeID = other.sourceTypeID;
			polProcessID = other.polProcessID;
			roadTypeID = other.roadTypeID;
		}
	}

	/**
	 * Execute the join and calculations, populating the final output table.
	 * @param context specific process and source type
	 * @param flags control internal logic
	 * @throws SQLException if anything goes wrong
	 * @return number of records written
	**/
	int process(Context context,
				Flags flags)
			throws IOException, SQLException {
		int pollutantID = (int)(context.polProcessID/100);
		String tableName = (isTest? "Test_" : "") + "BaseRateByAge_" + context.processID + "_" + context.yearID;
		String fileName = "Temp_BaseRateByAge_" + context.processID + "_" + context.yearID 
				+ "_" + context.sourceTypeID + "_" + context.polProcessID 
				+ "_" + context.roadTypeID + ".txt";
		File file = new File(fileName);
		PrintWriter writer = null;
		outputRecords.clear();
		SQLRunner.Query query = new SQLRunner.Query();
		String sql = "";
		int howMany = 0;
		try {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(file)));
			// Make the table. If it already exists, it may contain data so do not truncate it.
			sql = "create table if not exists " + tableName + " like BaseRateByAge";
			SQLRunner.executeSQL(db,sql);
			// Get the lookup data and index it in memory.
			loadSBWeightedEmissionRateByAge(context.sourceTypeID,context.polProcessID);
			// Do the main loop
			sql = "select *"
					+ " from RatesOpModeDistribution"
					+ " where sourceTypeID=" + context.sourceTypeID
					+ " and polProcessID=" + context.polProcessID;
			if(context.roadTypeID > 0) {
				sql += " and roadTypeID=" + context.roadTypeID;
			}
			sql += " order by sourceTypeID, polProcessID, roadTypeID, hourDayID";
			if(flags.keepOpModeID) {
				sql += ", opModeID";
			}
			if(flags.useAvgSpeedBin) {
				sql += ", avgSpeedBinID";
			}
			sql += ", opModeID";
			query.open(db,sql);
			String previousROMD = null;
			while(query.rs.next()) {
				String keyROMD = "" + query.rs.getInt("sourceTypeID")
						+ "|" + query.rs.getInt("polProcessID")
						+ "|" + query.rs.getInt("roadTypeID")
						+ "|" + query.rs.getInt("hourDayID");
				Integer opModeID = new Integer(query.rs.getInt("opModeID"));
				if(flags.keepOpModeID) {
					keyROMD += "|" + opModeID;
				}
				if(flags.useAvgSpeedBin) {
					keyROMD += "|" + query.rs.getInt("avgSpeedBinID");
				}
				if(previousROMD != null && !keyROMD.equals(previousROMD)) {
					// Everytime the ROMD's portion of the output table's unique key
					// changes, all accumulated data can be written to disk as it
					// will never be needed again.
					howMany += flushOutput(writer);
				}
				previousROMD = keyROMD;

				// Get all rates that match sourceTypeID, polProcessID, and opModeID.
				// Since all tables are filtered by sourceTypeID and polProcessID, opModeID
				// is the only key needed.				
				ArrayList<SBWeightedEmissionRateByAge> rates = erByOpModeID.get(opModeID);
				if(rates == null) {
					continue;
				}
				for(SBWeightedEmissionRateByAge er : rates) {
					String fullKey = keyROMD + er.partialKey;
					OutputRecord o = outputRecords.get(fullKey);
					if(o == null) { // If there is no output record yet, fill it.
						o = new OutputRecord();
						o.sourceTypeID = context.sourceTypeID;
						o.roadTypeID = query.rs.getInt("roadTypeID");
						if(flags.useAvgSpeedBin) {
							o.avgSpeedBinID = query.rs.getInt("avgSpeedBinID");
						}
						o.hourDayID = query.rs.getInt("hourDayID");
						o.polProcessID = context.polProcessID;
						if(flags.keepOpModeID) {
							o.opModeID = opModeID.intValue();
						} else {
							o.opModeID = 0;
						}
						o.processID = context.processID;
						o.pollutantID = pollutantID;

						o.modelYearID = er.modelYearID;
						o.fuelTypeID = er.fuelTypeID;
						o.ageGroupID = er.ageGroupID;
						o.regClassID = er.regClassID;

						outputRecords.put(fullKey,o);
					}
					// Accumulate output data
					double sumSBD = flags.useSumSBD? er.sumSBD : 1;
					double sumSBDRaw = flags.useSumSBDRaw? er.sumSBDRaw : 1;
					double opModeFraction = query.rs.getFloat("opModeFraction");
					double avgBinSpeed = query.rs.getFloat("avgBinSpeed");
					double avgSpeedFraction = flags.useAvgSpeedFraction? query.rs.getFloat("avgSpeedFraction") : 1;

					double t = opModeFraction * avgSpeedFraction * sumSBD;
					if(flags.keepOpModeID) {
						o.opModeFraction += t * sumSBDRaw;
					} else {
						o.opModeFraction += t;
					}
					o.opModeFractionRate += t;
					
					t = opModeFraction * avgSpeedFraction * sumSBDRaw;
					o.meanBaseRate += er.MeanBaseRate * t;
					o.meanBaseRateIM += er.MeanBaseRateIM * t;
					o.meanBaseRateACAdj += er.MeanBaseRateACAdj * t;
					o.meanBaseRateIMACAdj += er.MeanBaseRateIMACAdj * t;
					if(flags.useAvgSpeedBin) {
						if(avgBinSpeed > 0) {
							t = opModeFraction * avgSpeedFraction / avgBinSpeed;
							o.emissionRate += er.MeanBaseRate * t;
							o.emissionRateIM += er.MeanBaseRateIM * t;
							o.emissionRateACAdj += er.MeanBaseRateACAdj * t;
							o.emissionRateIMACAdj += er.MeanBaseRateIMACAdj * t;
						}
					} else {
						t = opModeFraction * avgSpeedFraction;
						o.emissionRate += er.MeanBaseRate * t;
						o.emissionRateIM += er.MeanBaseRateIM * t;
						o.emissionRateACAdj += er.MeanBaseRateACAdj * t;
						o.emissionRateIMACAdj += er.MeanBaseRateIMACAdj * t;
					}
				}
			}
			query.close();
			howMany += flushOutput(writer);
			writer.close();
			writer = null;
			// Load records into the database
			sql = "LOAD DATA INFILE '" + file.getCanonicalPath().replace('\\','/') + "' INTO TABLE " + tableName
					+ " ("
					+ "sourceTypeID,roadTypeID,avgSpeedBinID,hourDayID,polProcessID,pollutantID,processID,modelYearID,fuelTypeID,ageGroupID,regClassID,opModeID,"
					+ "meanBaseRate,meanBaseRateIM,meanBaseRateACAdj,meanBaseRateIMACAdj,"
					+ "emissionRate,emissionRateIM,emissionRateACAdj,emissionRateIMACAdj,"
					+ "opModeFraction,opModeFractionRate"
					+ ")";
			SQLRunner.executeSQL(db,sql);

			// Remove the temporary file
			if(!isTest) {
				FileUtilities.deleteFileWithRetry(file);
			}
		} finally {
			query.onFinally();
			if(writer != null) {
				howMany += flushOutput(writer);
				try {
					writer.close();
				} catch(Exception e) {
					// Nothing to do here
				}
				writer = null;
			}
		}
		return howMany;
	}

	/**
	 * Write any remaining output records to disk. Always clears the outputRecords collection
	 * even if an error occurs.
	 * @param writer file stream
	 * @return number of records written
	**/
	int flushOutput(PrintWriter writer) {
		int howMany = 0;
		try {
			for(String key : outputRecords.keySet()) {
				OutputRecord o = outputRecords.get(key);
				if(o != null) {
					o.write(writer);
					howMany++;
				}
			}
		} catch(IOException e) {
			Logger.logError(e,"Unable to write output records");
		} finally {
			outputRecords.clear();
		}
		return howMany;
	}

	/**
	 * Populate the internal cache of SBWeightedEmissionRateByAge entries.
	 * @param sourceTypeID source use type to be loaded
	 * @param polProcessID pollutant/process to be loaded
	 * @throws SQLException if anything goes wrong
	**/
	void loadSBWeightedEmissionRateByAge(int sourceTypeID, int polProcessID) throws SQLException {
		erByOpModeID.clear();
		String sql = "select *"
				+ " from SBWeightedEmissionRateByAge"
				+ " where sourceTypeID=" + sourceTypeID
				+ " and polProcessID=" + polProcessID;
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			ArrayList<SBWeightedEmissionRateByAge> t;
			Integer opModeID;
			query.open(db,sql);
			while(query.rs.next()) {
				SBWeightedEmissionRateByAge er = new SBWeightedEmissionRateByAge();
				er.fill(query.rs);
				opModeID = new Integer(query.rs.getInt("opModeID"));
				t = erByOpModeID.get(opModeID);
				if(t == null) {
					t = new ArrayList<SBWeightedEmissionRateByAge>();
					erByOpModeID.put(opModeID,t);
				}
				t.add(er);
			}
			query.close();
		} catch(SQLException e) {
			Logger.logSqlError(e,"Unable to load SBWeightedEmissionRateByAge",sql);
			throw e;
		} finally {
			query.onFinally();
		}
	}
}
