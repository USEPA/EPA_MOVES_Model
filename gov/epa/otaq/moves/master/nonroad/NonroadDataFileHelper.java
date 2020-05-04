package gov.epa.otaq.moves.master.nonroad;

import gov.epa.otaq.moves.common.LogMessageCategory;
import gov.epa.otaq.moves.common.Logger;
import gov.epa.otaq.moves.common.MOVESDatabaseType;
import gov.epa.otaq.moves.common.SQLRunner;
import gov.epa.otaq.moves.master.framework.DatabaseConnectionManager;
import gov.epa.otaq.moves.master.framework.ExecutionRunSpec;
import gov.epa.otaq.moves.master.framework.PollutantProcessAssociation;
import gov.epa.otaq.moves.master.nonroad.SccTech_HpminmaxYear.YearTech.TechFraction;
import gov.epa.otaq.moves.utils.FormatUtil;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

public class NonroadDataFileHelper {
	private final boolean oldFormatAllocate = true;
	private final boolean oldFormatTech = false;
	private final boolean debugEmsfac = false;
	private final boolean useOldEmsfacFiles = false;
	private final boolean useOldTechFiles = false;
	private final boolean useOldSeasonFiles = false;

	private final boolean multiRecordPerLine = true; // used when oldFormatTech = true

	NonroadDataFilesObj nonroadDataFiles = null;

	/**
	 * Default constructor Must create a new one by each opt file
	 **/
	public NonroadDataFileHelper() {

	}

	/**
	 * Generate ACTIVITY/ACTIVITY.DAT file. Assumes optFileWriter has been
	 * initialized.
	 * 
	 * @param executionDB
	 * @param outputPath
	 * @return true on success
	 **/
	public boolean generateActivityDatFile(Connection executionDB,
			String baseDir) {
		boolean isOK = true;

		String sql;
		PreparedStatement statement = null;
		PrintWriter printWriter = null;
		ResultSet results = null;

		try {

			printWriter = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(new File(
							baseDir + System.getProperty("file.separator")
									+ this.nonroadDataFiles.activityFile)))));
			printWriter.print("/ACTIVITY/"
					+ System.getProperty("line.separator"));

			sql = "select u.scc,s.description,h.hpmin,h.hpmax,u.loadfactor,'Hrs/Yr',u.hoursusedperyear, 'DEFAULT' "
					+ "from nrsourceusetype u "
					+ "inner join nrscc s on u.scc = s.scc "
					+ "inner join nrhprangebin h on u.nrhprangebinid = h.nrhprangebinid ";

			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				while (results.next()) {
					double hoursPerYearDouble = results.getDouble(7);
					int hoursPerYearInt = (int) hoursPerYearDouble;
					double diff = hoursPerYearDouble - hoursPerYearInt;
					if (diff > 0) {
						printWriter
								.printf("%1$10s%2$-41s%3$5s%4$10s%5$5d%6$5d%7$5.2f%8$5s%9$9s%10$11.1f%11$10s",
										results.getString(1),
										" " + results.getString(2), " ", " ",
										results.getInt(3), results.getInt(4),
										results.getDouble(5), " ",
										results.getString(6),
										hoursPerYearDouble,
										results.getString(8));
						printWriter.print(System.getProperty("line.separator"));
					} else {
						printWriter
								.printf("%1$10s%2$-41s%3$5s%4$10s%5$5d%6$5d%7$5.2f%8$5s%9$9s%10$11d%11$10s",
										results.getString(1),
										" " + results.getString(2), " ", " ",
										results.getInt(3), results.getInt(4),
										results.getDouble(5), " ",
										results.getString(6), hoursPerYearInt,
										results.getString(8));
						printWriter.print(System.getProperty("line.separator"));
					}
				}
				results.close();
			} else {
				isOK = false;
			}
			statement.close();
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate ACTIVITY.DAT.");
			}

		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating ACTIVITY.DAT : " + e);
			isOK = false;
		} catch (FileNotFoundException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating ACTIVITY.DAT : " + e);
			isOK = false;
			// e.printStackTrace();
		} finally {
			if (printWriter != null) {
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
				printWriter = null;
			}
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

	/**
	 * Generate ALLOCATE/ files. Assumes optFileWriter has been initialized.
	 * 
	 * @param executionDB
	 * @param outputPath
	 * @return true on success
	 **/
	public boolean generateAllocatefFiles(Connection executionDB, String baseDir) {
		boolean isOK = true;

		String sql;
		PreparedStatement statement = null;
		PrintWriter printWriter = null;
		ResultSet results = null;

		try {
			// -- ALLOCATE.XRF
			printWriter = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(new File(
							baseDir + System.getProperty("file.separator")
									+ this.nonroadDataFiles.allocateXrfFile)))));
			printWriter.print("/ALLOC XREF/"
					+ System.getProperty("line.separator"));
			sql = "select s.scc, 1.0 as coeff, sg.surrogateabbr from "
					+ "nrscc s inner join nrequipmenttype e on e.nrequiptypeid = s.nrequiptypeid "
					+ "inner join nrsurrogate sg on e.surrogateid = sg.surrogateid "
					+ "order by SCC ";

			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				while (results.next()) {
					if (!oldFormatAllocate) {
						// 2260001000 1.0 RVP
						printWriter.printf("%1$10s%2$5.1f%3$14s",
								results.getString(1), results.getDouble(2),
								results.getString(3));
						printWriter.print(System.getProperty("line.separator"));
					} else {
						// old format
						printWriter.printf("%1$10s%2$10.1f",
								results.getString(1), results.getDouble(2));
						printWriter.print(System.getProperty("line.separator"));
						printWriter.printf("%1$10s%2$10s",
								results.getString(1), results.getString(3));
						printWriter.print(System.getProperty("line.separator"));
					}
				}
				results.close();
			} else {
				isOK = false;
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator"));
			printWriter.flush();
			printWriter.close();
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate ALLOCATE.XRF.");
			}

			// -- ALL.ALO
			if (isOK) {
				printWriter = new PrintWriter(
						new BufferedWriter(
								new OutputStreamWriter(
										new FileOutputStream(
												new File(
														baseDir
																+ System.getProperty("file.separator")
																+ this.nonroadDataFiles.allAllocateFile)))));
				printWriter.print("/INDICATORS/"
						+ System.getProperty("line.separator"));

				sql = "select surrogateAbbr, countyID, surrogateYearID, surrogateQuant "
						+ "from nrstatesurrogate s "
						+ "inner join nrsurrogate a on s.surrogateid = a.surrogateid "
						+ "order by countyid,s.surrogateID,surrogateYearId";
				statement = executionDB.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						// MFG 02000 2002 15139 AK
						String fips = FormatUtil.formatWithLeadingZeros(
								results.getInt(2), 5);
						printWriter.printf("%1$-3s%2$7s%3$10s%4$20.3f",
								results.getString(1), "  " + fips,
								results.getString(3), results.getDouble(4));
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();

				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate ALL.ALO.");
				}
			}

			// -- ALLOCATE/FIPS.DAT
			if (isOK) {
				printWriter = new PrintWriter(new BufferedWriter(
						new OutputStreamWriter(new FileOutputStream(new File(
								baseDir + System.getProperty("file.separator")
										+ this.nonroadDataFiles.fipsFile)))));
				printWriter.print("/FIPS/"
						+ System.getProperty("line.separator"));

				sql = "select countyid, countyname from county order by countyid ";
				statement = executionDB.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						// 04012 1983 La Paz
						String fips = FormatUtil.formatWithLeadingZeros(results.getInt(1), 5);
						if(fips.equalsIgnoreCase("00000")) { // If the single National county,
							fips = "01000"; // use 01000 as 00000 for a county ID will give Nonroad errors.
						}
						printWriter.printf("%1$5s%2$5s%3$5s%4$1s%5$-50s", fips,
								" ", " ", " ", results.getString(2));
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();
				} else {
					isOK = false;
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();

				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate FIPS.DAT.");
				}
			}

		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating ALLOCATE files : " + e);
			isOK = false;
		} catch (FileNotFoundException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating ALLOCATE files : " + e);
			isOK = false;
			// e.printStackTrace();
		} finally {
			if (printWriter != null) {
				printWriter.flush();
				printWriter.close();
				printWriter = null;
			}
			if (statement != null) {
				try {
					if (!statement.isClosed()) {
						statement.close();
					}
				} catch (SQLException e) {
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
					e.printStackTrace();
				}
				results = null;
			}
		}

		return isOK;
	}

	// Michele @ Environron
	//
	// Concerning the pollutant naming issue, as Jason suggests the GUI can
	// handle those cases where we need the literal pollutant character string
	// used in NONROAD. This will primarily occur when generating the emission
	// factor files for input to the NONROAD Model. And deterioration rate
	// files. We won't need any MOVES database changes as long as the GUI writes
	// the needed NONROAD pollutant string.
	//
	// Concerning NONROAD PM (from the NONROAD documentation):
	// For nonroad engines, all PM emissions are assumed to be smaller than 10
	// microns (PM10), and 92% of the PM from gasoline and diesel fueled engines
	// is assumed to be smaller than 2.5 microns (PM2.5). For gaseous fueled
	// engines (LPG/CNG), 100% of the PM emissions are assumed to be smaller
	// than PM2.5. The NONROAD Reporting Utility allows the user to select the
	// desired size range.
	//
	// How will MOVES calculate PM2.5 from the NONROAD PM10? I assume this will
	// be part of importing/aggregating the NONROAD output data. Similarly for
	// NMHC, NMOG, TOG, and VOC if user selected.
	//
	// Ed Glover @ EPA
	//
	// I think PM2.5 and VOC will have to be calculated post NONROAD execution.
	// Either in java or SQL. The NONROAD run is done on the worker and output
	// bundle is sent back to the master. this is where the further processing
	// takes place for this and to convert units.
	//
	// I think it is a simple equation
	//
	// PM2.5 = PM10 * X
	//
	// VOC = (THC - methane) * (a + b * oxygencontent)
	//
	/**
	 * Generate DETFAC/DETFACALL.DAT file. Assumes optFileWriter has been
	 * initialized.
	 * 
	 * @param executionDB
	 * @param baseDir
	 * @return true on success
	 **/
	public boolean generateDetfacFile(Connection executionDB, String baseDir) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		PrintWriter printWriter = null;
		ResultSet results = null;

		try {

			sql = "Select d.polprocessid, t.engtechname, d.dfcoefficient, d.dfageexponent, d.emissioncap, p.neipollutantcode "
					+ "from nrdeterioration d inner join pollutantprocessassoc pp on d.polprocessid = pp.polprocessid "
					+ "inner join pollutant p on pp.pollutantid = p.pollutantid "
					+ "inner join  enginetech t on d.engtechid = t.engtechid "
					+ "where d.polprocessid = ?";

			// THC exhaust
			statement = executionDB.prepareStatement(sql);
			printWriter = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(new File(
							baseDir + System.getProperty("file.separator")
									+ this.nonroadDataFiles.detExhTHCFile)))));
			printWriter
					.print("/DETFAC/" + System.getProperty("line.separator"));
			statement.setInt(1, 101);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				while (results.next()) {
					printWriter.printf(
							"%1$-10s          %2$9.3f %3$9.1f %4$9.1f %5$10s",
							results.getString(2), results.getDouble(3),
							results.getDouble(4), results.getDouble(5), "THC" // results.getString(6)
					// -->
					// THC
							);
					printWriter.print(System.getProperty("line.separator"));
				}
				results.close();

			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate EXHTHC.DET.");
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator"));
			printWriter.flush();
			printWriter.close();

			if (isOK) {
				// CO exhaust
				statement = executionDB.prepareStatement(sql);
				printWriter = new PrintWriter(
						new BufferedWriter(new OutputStreamWriter(
								new FileOutputStream(new File(baseDir
										+ System.getProperty("file.separator")
										+ this.nonroadDataFiles.detExhCOFile)))));
				printWriter.print("/DETFAC/"
						+ System.getProperty("line.separator"));
				statement.setInt(1, 201);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						printWriter
								.printf("%1$-10s          %2$9.3f %3$9.1f %4$9.1f %5$10s",
										results.getString(2),
										results.getDouble(3),
										results.getDouble(4),
										results.getDouble(5),
										results.getString(6) // --> CO
								);
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate EXHCO.DET.");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
			}

			// NOx exhaust
			if (isOK) {
				statement = executionDB.prepareStatement(sql);
				printWriter = new PrintWriter(
						new BufferedWriter(
								new OutputStreamWriter(
										new FileOutputStream(
												new File(
														baseDir
																+ System.getProperty("file.separator")
																+ this.nonroadDataFiles.detExhNOXFile)))));
				printWriter.print("/DETFAC/"
						+ System.getProperty("line.separator"));
				statement.setInt(1, 301);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						printWriter
								.printf("%1$-10s          %2$9.3f %3$9.1f %4$9.1f %5$10s",
										results.getString(2),
										results.getDouble(3),
										results.getDouble(4),
										results.getDouble(5),
										results.getString(6) // --> NOX
								);
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate EXHNOX.DET.");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
			}

			// PM exhaust
			if (isOK) {
				statement = executionDB.prepareStatement(sql);
				printWriter = new PrintWriter(
						new BufferedWriter(new OutputStreamWriter(
								new FileOutputStream(new File(baseDir
										+ System.getProperty("file.separator")
										+ this.nonroadDataFiles.detExhPMFile)))));
				printWriter.print("/DETFAC/"
						+ System.getProperty("line.separator"));
				statement.setInt(1, 10001);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						printWriter
								.printf("%1$-10s          %2$9.3f %3$9.1f %4$9.1f %5$10s",
										results.getString(2),
										results.getDouble(3),
										results.getDouble(4),
										results.getDouble(5), "PM" // results.getString(6)
								// --> PM
								);
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate EXHPM.DET.");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
			}

			// all the others ignored: nonroad.exe will use the default values

		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating DETFAC files : " + e);
			isOK = false;
		} catch (FileNotFoundException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating DETFAC files : " + e);
			isOK = false;
			// e.printStackTrace();
		} finally {
			if (printWriter != null) {
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
				printWriter = null;
			}
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

	/**
	 * Generate DETFAC/DETFACALL.DAT file. Assumes optFileWriter has been
	 * initialized.
	 * 
	 * @param executionDB
	 * @param baseDir
	 * @return true on success
	 **/
	public boolean checkDetEmsFacExistence(Connection executionDB,
			NonroadEmissionFacInfo emsInfo, boolean doExhaust, boolean doCrank,
			boolean doEvap) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		ResultSet results = null;

		emsInfo.reset();

		try {

			sql = "Select d.polprocessid, t.engtechname, d.dfcoefficient, d.dfageexponent, d.emissioncap, p.neipollutantcode "
					+ "from nrdeterioration d inner join pollutantprocessassoc pp on d.polprocessid = pp.polprocessid "
					+ "inner join pollutant p on pp.pollutantid = p.pollutantid "
					+ "inner join  enginetech t on d.engtechid = t.engtechid "
					+ "where d.polprocessid = ?";

			// THC exhaust
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, 101);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null && results.next()) {
				emsInfo.detExhTHCExist = true;
			} else {
				emsInfo.detExhTHCExist = false;
			}
			if (results != null)
				results.close();
			statement.close();

			// CO exhaust
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, 201);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null && results.next()) {
				emsInfo.detExhCOExist = true;
			} else {
				emsInfo.detExhCOExist = false;
			}
			if (results != null)
				results.close();
			statement.close();

			// NOx exhaust
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, 301);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null && results.next()) {
				emsInfo.detExhNOXExist = true;
			} else {
				emsInfo.detExhNOXExist = false;
			}
			if (results != null)
				results.close();
			statement.close();

			// PM exhaust
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, 10001);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null && results.next()) {
				emsInfo.detExhPMExist = true;
			} else {
				emsInfo.detExhPMExist = false;
			}
			if (results != null)
				results.close();
			statement.close();

			// all the others ignored: nonroad.exe will use the default values

			// ems fac

			if (doExhaust) {
				sql = "select e.polprocessID, e.scc, e.hpmin, e.hpmax, t.engtechName, e.modelyearID, e.meanbaserate, e.units, p.neipollutantcode "
						+ "from nremissionrate e "
						+ "inner join pollutantprocessassoc pp on e.polprocessid = pp.polprocessid "
						+ "inner join enginetech t on e.engtechid = t.engtechid "
						+ "inner join pollutant p on pp.pollutantid = p.pollutantid "
						+ "where e.polprocessid = ?";

				// THC
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, 101);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					if (results.next()) {
						emsInfo.emsExhTHCExist = true;
					}
					results.close();
				}
				statement.close();

				// CO
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, 201);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					if (results.next()) {
						emsInfo.emsExhCOExist = true;
					}
					results.close();
				}
				statement.close();

				// NOX
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, 301);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					if (results.next()) {
						emsInfo.emsExhNOXExist = true;
					}
					results.close();

				}
				statement.close();

				// PM
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, 10001);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					if (results.next()) {
						emsInfo.emsExhPMExist = true;
					}
					results.close();

				}
				statement.close();
			}

			if (doCrank) {
				sql = "select e.polprocessID, e.scc, e.hpmin, e.hpmax, t.engtechName, "
						+ "e.modelyearID, e.meanbaserate, e.units, 'THC' "
						+ "from nrcrankcaseemissionrate e "
						+ "inner join enginetech t on e.engtechid = t.engtechid ";

				statement = executionDB.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					if (results.next()) {
						emsInfo.emsCRANKExist = true;
					}
					results.close();

				}
				statement.close();
			}

			if (doEvap) {

				sql = "select e.polprocessID, e.scc, e.hpmin, e.hpmax, t.engtechName, e.modelyearID, "
						+ "e.meanbaserate, e.units, p.neipollutantcode "
						+ "from nrevapemissionrate e "
						+ "inner join pollutantprocessassoc pp on e.polprocessid = pp.polprocessid "
						+ "inner join enginetech t on e.engtechid = t.engtechid "
						+ "inner join pollutant p on pp.pollutantid = p.pollutantid "
						+ "where e.polprocessid = ? ";

				int n = 0;
				NonroadHelper helper = new NonroadHelper();
				for (Map.Entry<Integer, String> entry : helper.evapIdFilenameMap
						.entrySet()) {
					statement = executionDB.prepareStatement(sql);
					statement.setInt(1, entry.getKey());
					results = SQLRunner.executeQuery(statement, sql);
					if (results != null) {
						if (results.next()) {
							switch (entry.getKey()) {
							case 130:
								emsInfo.emsEvDiuExist = true;
								break;
							case 120:
								emsInfo.emsEvTANKExist = true;
								break;
							case 121:
								emsInfo.emsEvHOSEExist = true;
								break;
							case 122:
								emsInfo.emsEvNECKExist = true;
								break;
							case 123:
								emsInfo.emsEvSUPRETExist = true;
								break;
							case 124:
								emsInfo.emsEvVENTExist = true;
								break;
							case 131:
								emsInfo.emsEvHOTSKExist = true;
								break;
							case 132:
								emsInfo.emsEvRUNLSExist = true;
								break;
							default:
								break;
							}
							results.close();
						}
					}
				}
			}

			// spillage
			sql = "select  s.scc, sc.description, "
					+ "(CASE isPumpFilled WHEN 'Y' THEN 'PUMP' ELSE 'CONTAINER' END) AS fillMethod, 'HP', "
					+ "h.hpmin, h.hpmax, 'ALL', tankUnits, " // 'GALLONS', "
					+ "tankSize, tankFillFrac, tankMetalFrac, "
					+ "hoseLength, hoseDiameter, hoseMetalFrac, marineFillNeckHoseLength,  "
					+ "marineFillNeckHoseDiameter, marineSupplyHoseLength, marineSupplyHoseDiameter, "
					+ "marineVentHoseLength,  marineVentHoseDiameter, hotSoaksPerSHO, nonInstMarineTankFrac, "
					+ "marineInstPlasticTankTrailFrac,  marineInstPlasticTankWaterFrac, "
					+ "marineInstMetalTankTrailerFrac, marineInstMetalTankWaterFrac,  "
					+ "e10TankPermeationAdjFac, e10HosePermeationAdjFac, e10MarineFillNeckPermAdjFac, "
					+ "e10MarineSupplyHosePermAdjFac, e10MarineVentHosePermAdjFac "
					+ "from nrsourceusetype s "
					+ "inner join nrscc sc on s.scc = sc.scc "
					+ "inner join nrhprangebin h on s.nrhprangebinid = h.nrhprangebinid ";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			if (results != null) {
				if (results.next()) {
					emsInfo.emsSPILLAGEExist = true;
				}
				results.close();
			}
			statement.close();
		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating DETFAC files : " + e);
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

		// check based on THC etc.

		//emsInfo.reset();
		for (Iterator<PollutantProcessAssociation> iter = ExecutionRunSpec.theExecutionRunSpec.pollutantProcessAssociations
				.iterator(); iter.hasNext();) {
			PollutantProcessAssociation selection = iter.next();
			if (selection.pollutant.databaseKey == 1) { // THC
				emsInfo.detExhTHCExist = true;
				emsInfo.emsExhTHCExist = true;
				emsInfo.emsBSFCExist = true;
				emsInfo.emsCRANKExist = true;
				emsInfo.emsCRANKExist = true;
				emsInfo.emsSPILLAGEExist = true;
				emsInfo.emsEvDiuExist = true;
				emsInfo.emsEvTANKExist = true;
				emsInfo.emsEvHOSEExist = true;
				emsInfo.emsEvNECKExist = true;
				emsInfo.emsEvSUPRETExist = true;
				emsInfo.emsEvVENTExist = true;
				emsInfo.emsEvHOTSKExist = true;
				emsInfo.emsEvRUNLSExist = true;
			}
			if (selection.pollutant.databaseKey == 2) { // CO
				emsInfo.detExhCOExist = true;
				emsInfo.emsExhCOExist = true;
			}
			if (selection.pollutant.databaseKey == 3) { // NOX
				emsInfo.detExhNOXExist = true;
				emsInfo.emsExhNOXExist = true;
			}
			if (selection.pollutant.databaseKey == 100
					|| selection.pollutant.databaseKey == 110) { // PM10 or
																	// PM2.5
				emsInfo.detExhPMExist = true;
				emsInfo.emsExhPMExist = true;
			}
		}

		return isOK;
	}

	/**
	 * Generate EMSFAC files. Assumes optFileWriter has been initialized.
	 * 
	 * @param executionDB
	 * @param outputPath
	 * @return true on success
	 **/
	public boolean generateEmsfacFile(Connection executionDB,
			boolean doExhaust, boolean doCrank, boolean doEvap, String baseDir) {
		boolean isOK = true;

		String sql = null;
		;
		PreparedStatement statement = null;
		PrintWriter printWriter = null;
		ResultSet results = null;

		try {

			// exhaust
			if (doExhaust) {
				// sql =
				// "select e.polprocessID, s.scc, h.hpmin, h.hpmax, t.engtechName, my.modelyeargroupstartyear, e.meanbaserate, 'g/hp-hr', p.neipollutantcode "
				// + "from nremissionrate e "
				// +
				// "inner join nrsourceusetype s on e.sourcetypeid = s.sourcetypeid "
				// +
				// "inner join pollutantprocessassoc pp on e.polprocessid = pp.polprocessid "
				// +
				// "inner join nrhprangebin h on s.nrhprangebinid = h.nrhprangebinid "
				// +
				// "inner join nrsourcebin sb on e.sourcebinid = sb.sourcebinid "
				// +
				// "inner join modelyeargroup my on sb.modelyeargroupid = my.modelyeargroupid "
				// + "inner join enginetech t on sb.engtechid = t.engtechid "
				// + "inner join pollutant p on pp.pollutantid = p.pollutantid "
				// + "where e.polprocessid = ?";
				sql = "select e.polprocessID, e.scc, e.hpmin, e.hpmax, t.engtechName, e.modelyearID, e.meanbaserate, e.units, p.neipollutantcode "
						+ "from nremissionrate e "
						+ "inner join pollutantprocessassoc pp on e.polprocessid = pp.polprocessid "
						+ "inner join enginetech t on e.engtechid = t.engtechid "
						+ "inner join pollutant p on pp.pollutantid = p.pollutantid "
						+ "where e.polprocessid = ?";

				// THC
				statement = executionDB.prepareStatement(sql);
				printWriter = new PrintWriter(
						new BufferedWriter(
								new OutputStreamWriter(
										new FileOutputStream(
												new File(
														baseDir
																+ System.getProperty("file.separator")
																+ this.nonroadDataFiles.emsExhTHCFile)))));
				printWriter.print("/EMSFAC/"
						+ System.getProperty("line.separator"));
				statement.setInt(1, 101);
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						printWriter
								.printf("     %1$10s     %2$5d%3$5d    %4$-10s%5$-10s%6$-10s",
										results.getString(2),
										results.getInt(3), results.getInt(4),
										results.getString(5), results
												.getString(8), NonroadHelper
												.mapPollutantToNonroad(results
														.getString(9)));
						printWriter.print(System.getProperty("line.separator"));
						printWriter
								.printf("%1$-5d                              %2$-10.5f",
										results.getInt(6), results.getDouble(7));
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate EXHTHC.EMF.");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();

				if (isOK) {
					// CO
					statement = executionDB.prepareStatement(sql);
					printWriter = new PrintWriter(
							new BufferedWriter(
									new OutputStreamWriter(
											new FileOutputStream(
													new File(
															baseDir
																	+ System.getProperty("file.separator")
																	+ this.nonroadDataFiles.emsExhCOFile)))));
					printWriter.print("/EMSFAC/"
							+ System.getProperty("line.separator"));
					statement.setInt(1, 201);
					results = SQLRunner.executeQuery(statement, sql);
					if (results != null) {
						while (results.next()) {
							printWriter
									.printf("     %1$10s     %2$5d%3$5d    %4$-10s%5$-10s%6$-10s",
											results.getString(2),
											results.getInt(3),
											results.getInt(4),
											results.getString(5),
											results.getString(8),
											NonroadHelper
													.mapPollutantToNonroad(results
															.getString(9)));
							printWriter.print(System
									.getProperty("line.separator"));
							printWriter
									.printf("%1$-5d                              %2$-10.5f",
											results.getInt(6),
											results.getDouble(7));
							printWriter.print(System
									.getProperty("line.separator"));
						}
						results.close();

					} else {
						isOK = false;
					}
					if (!isOK) {
						Logger.log(LogMessageCategory.ERROR,
								"Failed to generate EXHCO.EMF.");
					}
					statement.close();
					printWriter.print("/END/"
							+ System.getProperty("line.separator"));
					printWriter.flush();
					printWriter.close();

				}
				if (isOK) {
					// NOX
					statement = executionDB.prepareStatement(sql);
					printWriter = new PrintWriter(
							new BufferedWriter(
									new OutputStreamWriter(
											new FileOutputStream(
													new File(
															baseDir
																	+ System.getProperty("file.separator")
																	+ this.nonroadDataFiles.emsExhNOXFile)))));
					printWriter.print("/EMSFAC/"
							+ System.getProperty("line.separator"));
					statement.setInt(1, 301);
					results = SQLRunner.executeQuery(statement, sql);
					if (results != null) {
						while (results.next()) {
							printWriter
									.printf("     %1$10s     %2$5d%3$5d    %4$-10s%5$-10s%6$-10s",
											results.getString(2),
											results.getInt(3),
											results.getInt(4),
											results.getString(5),
											results.getString(8),
											NonroadHelper
													.mapPollutantToNonroad(results
															.getString(9)));
							printWriter.print(System
									.getProperty("line.separator"));
							printWriter
									.printf("%1$-5d                              %2$-10.5f",
											results.getInt(6),
											results.getDouble(7));
							printWriter.print(System
									.getProperty("line.separator"));
						}
						results.close();

					} else {
						isOK = false;
					}
					if (!isOK) {
						Logger.log(LogMessageCategory.ERROR,
								"Failed to generate EXHNOX.EMF.");
					}
					statement.close();
					printWriter.print("/END/"
							+ System.getProperty("line.separator"));
					printWriter.flush();
					printWriter.close();

				}
				if (isOK) {
					// PM
					statement = executionDB.prepareStatement(sql);
					printWriter = new PrintWriter(
							new BufferedWriter(
									new OutputStreamWriter(
											new FileOutputStream(
													new File(
															baseDir
																	+ System.getProperty("file.separator")
																	+ this.nonroadDataFiles.emsExhPMFile)))));
					printWriter.print("/EMSFAC/"
							+ System.getProperty("line.separator"));
					statement.setInt(1, 10001);
					results = SQLRunner.executeQuery(statement, sql);
					if (results != null) {
						while (results.next()) {
							printWriter
									.printf("     %1$10s     %2$5d%3$5d    %4$-10s%5$-10s%6$-10s",
											results.getString(2),
											results.getInt(3),
											results.getInt(4),
											results.getString(5),
											results.getString(8),
											NonroadHelper
													.mapPollutantToNonroad(results
															.getString(9)));
							printWriter.print(System
									.getProperty("line.separator"));
							printWriter
									.printf("%1$-5d                              %2$-10.5f",
											results.getInt(6),
											results.getDouble(7));
							printWriter.print(System
									.getProperty("line.separator"));
						}
						results.close();

					} else {
						isOK = false;
					}
					if (!isOK) {
						Logger.log(LogMessageCategory.ERROR,
								"Failed to generate EXHPM.EMF.");
					}
					statement.close();
					printWriter.print("/END/"
							+ System.getProperty("line.separator"));
					printWriter.flush();
					printWriter.close();

				}
				if (isOK) {
					// BSFC
					statement = executionDB.prepareStatement(sql);
					printWriter = new PrintWriter(
							new BufferedWriter(
									new OutputStreamWriter(
											new FileOutputStream(
													new File(
															baseDir
																	+ System.getProperty("file.separator")
																	+ this.nonroadDataFiles.emsBSFCFile)))));
					printWriter.print("/EMSFAC/"
							+ System.getProperty("line.separator"));
					statement.setInt(1, 9901);
					results = SQLRunner.executeQuery(statement, sql);
					if (results != null) {
						while (results.next()) {
							String pol = results.getString(9);
							pol = (pol == null || pol.trim().length() == 0) ? "BSFC"
									: pol;
							pol = NonroadHelper.mapPollutantToNonroad(pol);
							printWriter
									.printf("     %1$10s     %2$5d%3$5d    %4$-10s%5$-10s%6$-10s",
											results.getString(2),
											results.getInt(3),
											results.getInt(4),
											results.getString(5), " ", // results.getString(8),
											pol);
							printWriter.print(System
									.getProperty("line.separator"));
							printWriter
									.printf("%1$-5d                              %2$-10.5f",
											results.getInt(6),
											results.getDouble(7));
							printWriter.print(System
									.getProperty("line.separator"));
						}
						results.close();

					} else {
						isOK = false;
					}
					if (!isOK) {
						Logger.log(LogMessageCategory.ERROR,
								"Failed to generate BSFC.EMF.");
					}
					statement.close();
					printWriter.print("/END/"
							+ System.getProperty("line.separator"));
					printWriter.flush();
					printWriter.close();
				}
			}
			if (debugEmsfac)
				System.out.println("before crank isOK=" + isOK);
			// crankcase
			if (doCrank && isOK) {
				// sql =
				// "select e.polprocessID, s.scc, h.hpmin, h.hpmax, t.engtechName, "
				// +
				// "my.modelyeargroupstartyear, e.NrProcessMeanBaseRate, 'MULT', 'THC' "
				// + "from nrcrankcaseemissionratio e "
				// +
				// "inner join nrsourceusetype s on e.sourcetypeid = s.sourcetypeid "
				// +
				// "inner join pollutantprocessassoc pp on e.polprocessid = pp.polprocessid "
				// +
				// "inner join nrhprangebin h on s.nrhprangebinid = h.nrhprangebinid "
				// +
				// "inner join nrsourcebin sb on e.sourcebinid = sb.sourcebinid "
				// +
				// "inner join modelyeargroup my on sb.modelyeargroupid = my.modelyeargroupid "
				// + "inner join enginetech t on sb.engtechid = t.engtechid ";
				sql = "select e.polprocessID, e.scc, e.hpmin, e.hpmax, t.engtechName, "
						+ "e.modelyearID, e.meanbaserate, e.units, 'THC' "
						+ "from nrcrankcaseemissionrate e "
						+ "inner join enginetech t on e.engtechid = t.engtechid ";

				statement = executionDB.prepareStatement(sql);
				printWriter = new PrintWriter(
						new BufferedWriter(new OutputStreamWriter(
								new FileOutputStream(new File(baseDir
										+ System.getProperty("file.separator")
										+ this.nonroadDataFiles.emsCRANKFile)))));
				printWriter.print("/EMSFAC/"
						+ System.getProperty("line.separator"));
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					while (results.next()) {
						printWriter
								.printf("     %1$10s     %2$5d%3$5d    %4$-10s%5$-10s%6$-10s",
										results.getString(2),
										results.getInt(3), results.getInt(4),
										results.getString(5),
										results.getString(8),
										results.getString(9));
						printWriter.print(System.getProperty("line.separator"));
						printWriter
								.printf("%1$-5d                              %2$-10.5f",
										results.getInt(6), results.getDouble(7));
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate EMSFAC for crankcase.");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
			}

			if (debugEmsfac)
				System.out.println("before evap isOK=" + isOK);
			// evaporative -- need to check unit
			if (doEvap && isOK) {

				// sql =
				// "select e.polprocessID, s.scc, h.hpmin, h.hpmax, t.engtechName, my.modelyeargroupstartyear, "
				// + "e.NrProcessMeanBaseRate, e.units, p.neipollutantcode "
				// + "from nrevapemissionrate e "
				// +
				// "inner join nrsourceusetype s on e.sourcetypeid = s.sourcetypeid "
				// +
				// "inner join pollutantprocessassoc pp on e.polprocessid = pp.polprocessid "
				// +
				// "inner join nrhprangebin h on s.nrhprangebinid = h.nrhprangebinid "
				// +
				// "inner join nrsourcebin sb on e.sourcebinid = sb.sourcebinid "
				// +
				// "inner join modelyeargroup my on sb.modelyeargroupid = my.modelyeargroupid "
				// + "inner join enginetech t on sb.engtechid = t.engtechid "
				// + "inner join pollutant p on pp.pollutantid = p.pollutantid "
				// + "where e.polprocessid = ? ";

				sql = "select e.polprocessID, e.scc, e.hpmin, e.hpmax, t.engtechName, e.modelyearID, "
						+ "e.meanbaserate, e.units, p.neipollutantcode "
						+ "from nrevapemissionrate e "
						+ "inner join pollutantprocessassoc pp on e.polprocessid = pp.polprocessid "
						+ "inner join enginetech t on e.engtechid = t.engtechid "
						+ "inner join pollutant p on pp.pollutantid = p.pollutantid "
						+ "where e.polprocessid = ? ";

				if (debugEmsfac)
					System.out.println("before evap loop isOK=" + isOK);
				int n = 0;
				NonroadHelper helper = new NonroadHelper();
				for (Map.Entry<Integer, String> entry : helper.evapIdFilenameMap
						.entrySet()) {
					if (debugEmsfac)
						System.out.println("isOK=" + isOK + " n=" + n++);
					if (isOK) {
						statement = executionDB.prepareStatement(sql);
						printWriter = new PrintWriter(
								new BufferedWriter(
										new OutputStreamWriter(
												new FileOutputStream(
														new File(
																baseDir
																		+ System.getProperty("file.separator")
																		+ entry.getValue())))));
						printWriter.print("/EMSFAC/"
								+ System.getProperty("line.separator"));
						statement.setInt(1, entry.getKey());
						results = SQLRunner.executeQuery(statement, sql);
						if (results != null) {
							while (results.next()) {
								printWriter
										.printf("     %1$10s     %2$5d%3$5d    %4$-10s%5$-10s%6$-10s",
												results.getString(2),
												results.getInt(3),
												results.getInt(4),
												results.getString(5),
												results.getString(8),
												NonroadHelper
														.mapPollutantToNonroad(results
																.getString(9)));
								printWriter.print(System
										.getProperty("line.separator"));
								printWriter
										.printf("%1$-5d                              %2$-10.5f",
												results.getInt(6),
												results.getDouble(7));
								printWriter.print(System
										.getProperty("line.separator"));
							}
							results.close();

						} else {
							isOK = false;
						}
						if (!isOK) {
							Logger.log(LogMessageCategory.ERROR,
									"Failed to generate " + entry.getValue()
											+ ".");
						}
						statement.close();
						printWriter.print("/END/"
								+ System.getProperty("line.separator"));
						printWriter.flush();
						printWriter.close();
					}
				}
			}

			if (debugEmsfac)
				System.out.println("before spillage isOK=" + isOK);
			// spillage
			if (isOK) {
				sql = "select  s.scc, sc.description, "
						+ "(CASE isPumpFilled WHEN 'Y' THEN 'PUMP' ELSE 'CONTAINER' END) AS fillMethod, 'HP', "
						+ "h.hpmin, h.hpmax, 'ALL', tankUnits, " // 'GALLONS', "
						+ "tankSize, tankFillFrac, tankMetalFrac, "
						+ "hoseLength, hoseDiameter, hoseMetalFrac, marineFillNeckHoseLength,  "
						+ "marineFillNeckHoseDiameter, marineSupplyHoseLength, marineSupplyHoseDiameter, "
						+ "marineVentHoseLength,  marineVentHoseDiameter, hotSoaksPerSHO, nonInstMarineTankFrac, "
						+ "marineInstPlasticTankTrailFrac,  marineInstPlasticTankWaterFrac, "
						+ "marineInstMetalTankTrailerFrac, marineInstMetalTankWaterFrac,  "
						+ "e10TankPermeationAdjFac, e10HosePermeationAdjFac, e10MarineFillNeckPermAdjFac, "
						+ "e10MarineSupplyHosePermAdjFac, e10MarineVentHosePermAdjFac "
						+ "from nrsourceusetype s "
						+ "inner join nrscc sc on s.scc = sc.scc "
						+ "inner join nrhprangebin h on s.nrhprangebinid = h.nrhprangebinid "
						+ "order by s.scc, isPumpFilled, h.hpmin, h.hpmax";
				statement = executionDB.prepareStatement(sql);
				printWriter = new PrintWriter(
						new BufferedWriter(
								new OutputStreamWriter(
										new FileOutputStream(
												new File(
														baseDir
																+ System.getProperty("file.separator")
																+ this.nonroadDataFiles.emsSPILLAGEFile)))));
				printWriter.print("/EMSFAC/"
						+ System.getProperty("line.separator"));
				results = SQLRunner.executeQuery(statement, sql);
				if (results != null) {
					LineMerger lineMerger = new LineMerger(printWriter,69-1,5,74-1,5);
					while (results.next()) {
						String line = String.format("%1$10s %2$-40s  %3$-9s %4$-4s %5$5d%6$5d%7$-10s %8$-10s   %9$10.5f%10$8.5f%11$10.5f"
										+ "%12$10.5f%13$10.5f%14$10.5f%15$10.5f%16$10.6f%17$10.5f%18$10.6f%19$10.5f%20$10.6f%21$10.6f"
										+ "%22$10.3f%23$10.3f%24$10.3f%25$10.3f%26$10.3f%27$10.3f%28$10.3f%29$10.3f%30$10.3f%31$10.3f",
										results.getString(1),
										results.getString(2),
										results.getString(3),
										results.getString(4),
										results.getInt(5), results.getInt(6),
										"    " + results.getString(7),
										results.getString(8),
										results.getDouble(9),
										results.getDouble(10),
										results.getDouble(11),

										results.getDouble(12),
										results.getDouble(13),
										results.getDouble(14),
										results.getDouble(15),
										results.getDouble(16),
										results.getDouble(17),
										results.getDouble(18),
										results.getDouble(19),
										results.getDouble(20),
										results.getDouble(21),

										results.getDouble(22),
										results.getDouble(23),
										results.getDouble(24),
										results.getDouble(25),
										results.getDouble(26),
										results.getDouble(27),
										results.getDouble(28),
										results.getDouble(29),
										results.getDouble(30),
										results.getDouble(31));
						lineMerger.println(line);
					}
					lineMerger.flush();
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate EMSFAC for crankcase.");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
			}

		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating EMSFAC files : " + e);
			isOK = false;
		} catch (FileNotFoundException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating EMSFAC files : " + e);
			isOK = false;
			// e.printStackTrace();
		} finally {
			if (printWriter != null) {
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
				printWriter = null;
			}
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

	/**
	 * Generate GROWTH file. Assumes optFileWriter has been initialized.
	 * 
	 * @param executionDB
	 * @param outputPath
	 * @return true on success
	 **/
	public boolean generateGrowthFile(Connection executionDB, String baseDir) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		PrintWriter printWriter = null;
		ResultSet results = null;

		String growthFilePath = baseDir + System.getProperty("file.separator")
				+ this.nonroadDataFiles.growthFile;
		try {

			printWriter = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(new File(
							growthFilePath)))));

			// indicators
			sql = "select concat(stateid,'000'), growthpatternid,g.scc, 0, 9999,'ALL', ' ' " // s.description
																								// "
					+ "from nrgrowthpatternfinder g "
			// + "inner join nrscc s on g.scc = s.scc "
			;
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			printWriter.print("/INDICATORS/"
					+ System.getProperty("line.separator"));
			if (results != null) {
				while (results.next()) {
					String patternid = FormatUtil.formatWithLeadingZeros(
							results.getInt(2), 3);
					printWriter.printf(
							"%1$5s %2$-4s %3$10s %4$5d%5$5d %6$-10s %7$-50s",
							FormatUtil.formatWithLeadingZeros(
									results.getInt(1), 5), patternid, results
									.getString(3), results.getInt(4), results
									.getInt(5), results.getString(6), results
									.getString(7));
					printWriter.print(System.getProperty("line.separator"));
				}
				results.close();

			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate indicator packet for file "
								+ growthFilePath + ".");
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator"));
			printWriter.flush();

			// growth
			sql = "select '00000',yearid,growthpatternid,growthindex "
					+ "from nrgrowthindex ";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			printWriter
					.print("/GROWTH/" + System.getProperty("line.separator"));
			if (results != null) {
				while (results.next()) {
					String patternid = FormatUtil.formatWithLeadingZeros(
							results.getInt(3), 3);
					printWriter.printf(
							"%1$5s%2$5s%3$5d %4$4s     %5$20d",
							results.getString(1), // don't need to fromat it
							// since it is always
							// '00000'
							" ", results.getInt(2), patternid,
							results.getInt(4));
					printWriter.print(System.getProperty("line.separator"));
				}
				results.close();

			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate growth packet for file "
								+ growthFilePath + ".");
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator"));
			printWriter.flush();

			// scrappage
			sql = "select fractionlifeused,percentagescrapped "
					+ "from nrscrappagecurve " + "where NREquipTypeID= 0 ";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			printWriter.print("/SCRAPPAGE/"
					+ System.getProperty("line.separator"));
			if (results != null) {
				while (results.next()) {
					printWriter.printf("%1$-10.5f %2$-5.2f",
							results.getDouble(1), results.getDouble(2));
					printWriter.print(System.getProperty("line.separator"));
				}
				results.close();

			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate growth packet for file "
								+ growthFilePath + ".");
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator"));
			printWriter.flush();

			// /ALTERNATE SCRAPPAGE/ -- implement later

			printWriter.close();

		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating DETFAC file : " + e);
			isOK = false;
		} catch (FileNotFoundException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating EMSFAC files : " + e);
			isOK = false;
			// e.printStackTrace();
		} finally {
			if (printWriter != null) {
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
				printWriter = null;
			}
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

	/**
	 * Generate POP file. Assumes optFileWriter has been initialized.
	 * 
	 * @param executionDB
	 * @param outputPath
	 * @return true on success
	 **/
	public boolean generatePopFile(Connection executionDB, int[] stateIDs,
			String baseDir) {
		Logger.log(LogMessageCategory.INFO,"NonroadDataFileHelper.generatePopFile called");

		boolean isOK = true;

		String sql1 = "SELECT stateAbbr FROM state where stateID = ?;";
		PreparedStatement statement1 = null;

		String sql2 = "select concat(p.stateid,'000') as state, p.nrbaseyearid, s.scc, "
				+ "s.description, h.hpmin, h.hpmax, u.hpavg, "
				+ "u.medianlifefullload as life, 'DEFAULT', p.population "
				+ "from nrbaseyearequippopulation p "
				+ "inner join nrsourceusetype u on p.sourcetypeid = u.sourcetypeid "
				+ "inner join nrscc s on u.scc = s.scc "
				+ "inner join nrhprangebin h on u.nrhprangebinid = h.nrhprangebinid "
				+ "where p.stateid = ?";
		PreparedStatement statement2 = null;
		PrintWriter printWriter = null;
		ResultSet results = null;

		try {
			this.nonroadDataFiles.popFiles.clear();
			for (int statID : stateIDs) {
				Logger.log(LogMessageCategory.INFO,"NonroadDataFileHelper.generatePopFile statID=" + statID);

				if (!isOK)
					break;

				statement1 = executionDB.prepareStatement(sql1);
				statement1.setInt(1, statID);
				results = SQLRunner.executeQuery(statement1, sql1);
				String abbr = null;
				if (results != null && results.next()) {
					abbr = results.getString(1);
				} else {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to get state abbreviation for state with ID="
									+ statID + ".");
					isOK = false;
				}
				statement1.close();

				if (isOK) {
					String relativePath = // "DATA/POP/" +
					abbr.trim().toUpperCase() + ".POP";
					this.nonroadDataFiles.popFiles.add(relativePath);
					String filePath = baseDir
							+ System.getProperty("file.separator")
							+ relativePath;
					printWriter = new PrintWriter(new BufferedWriter(
							new OutputStreamWriter(new FileOutputStream(
									new File(filePath)))));
					statement2 = executionDB.prepareStatement(sql2);
					statement2.setInt(1, statID);
					results = SQLRunner.executeQuery(statement2, sql2);
					printWriter.print("/POPULATION/"
							+ System.getProperty("line.separator"));
					if (results != null) {
						while (results.next()) {
							int hpmin = results.getInt(5);
							int hpmax = results.getInt(6);
							double hpavg = results.getDouble(7);
							if (hpavg < hpmin || hpavg > hpmax) {
								hpavg = (hpmin + hpmax) / 2.0;
							}
							double life = results.getDouble(8);
							double dblLife = 0;
							int intLife = 0;
							if (life < 1000) {
								dblLife = Math.round(life * 10) / 10.0f;
							} else {
								intLife = (int) Math.round(life);
							}

							if (life >= 1000) {
								printWriter
										// -------1-----7-----13----18-----29------70----76----82----88---93101----106---
										.printf("%1$5s %2$5s %3$4d %4$10s %5$-40s %6$5d %7$5d %8$5s %9$5d%10$9s    %11$17.1f",
												FormatUtil
														.formatWithLeadingZeros(
																results.getInt(1),
																5), // fips
												" ", // sub-region for
														// sub-county
												results.getInt(2), // year
												results.getString(3), // scc
												results.getString(4), // description
												hpmin, // results.getInt(5), //
												// hpmin
												hpmax, // results.getInt(6), //
												// hpmax
												FormatUtil.formatFloatToString(
														hpavg, 5, 3), // results.getDouble(7),
												// //
												// hpavg
												intLife, // life
												results.getString(9), // flag
												results.getDouble(10) // population
										);
							} else {
								printWriter
										// -------1-----7-----13----18-----29------70----76----82----88---93101----106---
										.printf("%1$5s %2$5s %3$4d %4$10s %5$-40s %6$5d %7$5d %8$5s %9$5.1f%10$9s    %11$17.1f",
												FormatUtil
														.formatWithLeadingZeros(
																results.getInt(1),
																5), // fips
												" ", // sub-region for
														// sub-county
												results.getInt(2), // year
												results.getString(3), // scc
												results.getString(4), // description
												hpmin, // results.getInt(5), //
												// hpmin
												hpmax, // results.getInt(6), //
												// hpmax
												FormatUtil.formatFloatToString(
														hpavg, 5, 3), // results.getDouble(7),
												// //
												// hpavg
												dblLife, // life
												results.getString(9), // flag
												results.getDouble(10) // population
										);
							}
							printWriter.print(System
									.getProperty("line.separator"));
						}
						results.close();
					} else {
						isOK = false;
					}
					if (!isOK) {
						Logger.log(LogMessageCategory.ERROR,
								"Failed to generate file " + filePath + ".");
					}
					statement2.close();
					printWriter.print("/END/"
							+ System.getProperty("line.separator"));
					printWriter.flush();
					printWriter.close();
				}
			}

		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating POPULATION file : " + e);
			isOK = false;
		} catch (FileNotFoundException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating EMSFAC files : " + e);
			isOK = false;
			// e.printStackTrace();
		} finally {
			if (printWriter != null) {
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
				printWriter = null;
			}
			if (statement2 != null) {
				try {
					if (!statement2.isClosed()) {
						statement2.close();
					}
				} catch (SQLException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				statement2 = null;
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

	/**
	 * Generate Retrofit file. Assumes optFileWriter has been initialized.
	 * 
	 * @param executionDB
	 * @param outputPath
	 * @return true on success
	 **/
	public boolean generateRetrofitFile(Connection executionDB, String baseDir) {
		boolean isOK = true;

		NonroadOptFileSqlHelper helper = new NonroadOptFileSqlHelper();
		if (!helper.checkRetrofit(executionDB)) {

			String sql = null;
			PreparedStatement statement = null;
			PrintWriter printWriter = null;
			ResultSet results = null;

			try {
				sql = "Select retrofitStartYear, retrofitEndYear, StartModelYear, EndModelYear, SCC, t.engTechName, "
						+ "hpMin, hpMax, annualFractionRetrofit, retrofitEffectiveFraction, p.neipollutantcode, retrofitID "
						+ "from nrretrofitfactors r "
						+ "inner join enginetech t on t.engtechID = r.engtechID "
						+ "inner join pollutant p on p.pollutantID = r.pollutantID;";
				statement = executionDB.prepareStatement(sql);
				results = SQLRunner.executeQuery(statement, sql);

				printWriter = new PrintWriter(
						new BufferedWriter(new OutputStreamWriter(
								new FileOutputStream(new File(baseDir
										+ System.getProperty("file.separator")
										+ this.nonroadDataFiles.retrofitFile)))));

				printWriter.print("/RETROFIT/"
						+ System.getProperty("line.separator"));
				if (results != null) {
					while (results.next()) {
						String patternid = FormatUtil.formatWithLeadingZeros(
								results.getInt(2), 3);
						printWriter
								.printf("%1$4d %2$4d %3$4d %4$4d %5$10s %6$-10s %7$5.0f%8$5.0f %9$18.2f %10$6.2f %11$-10s %12$5d %13$-50s",
										results.getInt(1), results.getInt(2),
										results.getInt(3), results.getInt(4),
										results.getString(5),
										results.getString(6),
										results.getDouble(7),
										results.getDouble(8),
										results.getDouble(9),
										results.getDouble(10),
										results.getString(11),
										results.getInt(12), " ");
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate file "
									+ this.nonroadDataFiles.retrofitFile + ".");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
			} catch (SQLException e) {
				Logger.logException(LogMessageCategory.ERROR, e);
				Logger.log(LogMessageCategory.ERROR,
						"Exception in generating RETROFIT file : " + e);
				isOK = false;
			} catch (FileNotFoundException e) {
				Logger.logException(LogMessageCategory.ERROR, e);
				Logger.log(LogMessageCategory.ERROR,
						"Exception in generating EMSFAC files : " + e);
				isOK = false;
				// e.printStackTrace();
			} finally {
				if (printWriter != null) {
					printWriter.print("/END/"
							+ System.getProperty("line.separator"));
					printWriter.flush();
					printWriter.close();
					printWriter = null;
				}
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
		}

		return isOK;
	}

	/**
	 * Generate Season file. Assumes optFileWriter has been initialized.
	 * 
	 * @param executionDB
	 * @param outputPath
	 * @return true on success
	 **/
	public boolean generateSeasonFile(Connection executionDB, String baseDir) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		PrintWriter printWriter = null;
		ResultSet results = null;

		try {
			printWriter = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(new File(
							baseDir + System.getProperty("file.separator")
									+ this.nonroadDataFiles.seasonFile)))));

			// REGIONS
			sql = "select stateID as region, stateAbbr, concat(stateID,'000') as fips, stateName from state;";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			printWriter.print("/REGIONS/"
					+ System.getProperty("line.separator"));
			if (results != null) {
				while (results.next()) {
					printWriter.printf(
							"%1$-5s%2$-40s%3$5s%4$-20s",
							// results.getString(2),
							FormatUtil.formatWithLeadingZeros(
									results.getInt(3) / 1000, 2),
							" ----------", FormatUtil.formatWithLeadingZeros(
									results.getInt(3), 5),
							" " + results.getString(4));
					printWriter.print(System.getProperty("line.separator"));
				}
				results.close();

			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate REGION packet for file SEASON.DAT.");
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator")
					+ System.getProperty("line.separator"));
			printWriter.flush();

			// MONTHLY
			sql = "select m.stateID as region, stateAbbr, SCC, "
					+ // need to check SCC later
					"SUM(IF(monthid=1,monthFraction,0)) AS jan, "
					+ "SUM(IF(monthid=2,monthFraction,0)) AS feb, "
					+ "SUM(IF(monthid=3,monthFraction,0)) AS mar, "
					+ "SUM(IF(monthid=4,monthFraction,0)) AS apr, "
					+ "SUM(IF(monthid=5,monthFraction,0)) AS may, "
					+ "SUM(IF(monthid=6,monthFraction,0)) AS jun, "
					+ "SUM(IF(monthid=7,monthFraction,0)) AS jul, "
					+ "SUM(IF(monthid=8,monthFraction,0)) AS aug, "
					+ "SUM(IF(monthid=9,monthFraction,0)) AS sep, "
					+ "SUM(IF(monthid=10,monthFraction,0)) AS oct, "
					+ "SUM(IF(monthid=11,monthFraction,0)) AS nov, "
					+ "SUM(IF(monthid=12,monthFraction,0)) AS decem "
					+ "from nrmonthallocation m inner join state s on m.stateID = s.stateID "
					+ "group by m.stateID, SCC;";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			printWriter.print("/MONTHLY/"
					+ System.getProperty("line.separator"));
			if (results != null) {
				while (results.next()) {
					printWriter
							.printf("%1$-5s %2$10s %3$-34s%4$10.3f%5$10.3f%6$10.3f%7$10.3f%8$10.3f%9$10.3f%10$10.3f%11$10.3f%12$10.3f%13$10.3f%14$10.3f%15$10.3f",
									FormatUtil.formatWithLeadingZeros(
											results.getInt(1), 2), // use the state fips rather than abbrv in results.getString(2)
									results.getString(3), " ",
									results.getDouble(4), results.getDouble(5),
									results.getDouble(6), results.getDouble(7),
									results.getDouble(8), results.getDouble(9),
									results.getDouble(10),
									results.getDouble(11),
									results.getDouble(12),
									results.getDouble(13),
									results.getDouble(14),
									results.getDouble(15));
					printWriter.print(System.getProperty("line.separator"));
				}
				results.close();
			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate MONTHLY packet for file SEASON.DAT.");
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator"));
			printWriter.flush();

			// DAILY
			sql = "select SCC, "
					+ "SUM(IF(dayid=5,dayFraction,0)) AS weekday, "
					+ "SUM(IF(dayid=2,dayFraction,0)) AS weekend "
					+ "from nrdayallocation " + "group by SCC;";
			statement = executionDB.prepareStatement(sql);
			results = SQLRunner.executeQuery(statement, sql);
			printWriter.print("/DAILY/" + System.getProperty("line.separator"));
			if (results != null) {
				while (results.next()) {
					printWriter.printf("%1$-5s %2$10s %3$-35s%4$10.7f%5$10.7f",
							" ", // sub-region code, blank = match all
							results.getString(1), // scc
							" ", results.getDouble(2), results.getDouble(3));
					printWriter.print(System.getProperty("line.separator"));
				}
				results.close();
			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate DAILY packet for file SEASON.DAT.");
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator"));
			printWriter.flush();
			printWriter.close();

		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating SEASON file : " + e);
			isOK = false;
		} catch (FileNotFoundException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating EMSFAC files : " + e);
			isOK = false;
			// e.printStackTrace();
		} finally {
			if (printWriter != null) {
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
				printWriter = null;
			}
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

	/**
	 * Generate TECH file. Assumes optFileWriter has been initialized.
	 * 
	 * @param executionDB
	 * @param outputPath
	 * @return true on success
	 **/
	public boolean generateTechFile(Connection executionDB, String baseDir) {
		boolean isOK = true;

		String sql = null;
		PreparedStatement statement = null;
		PrintWriter printWriter = null;
		ResultSet results = null;

		try {
			sql = "select f.scc, f.hpmin, f.hpmax, t.engtechName, f.modelyearID, f.nrengtechfraction "
					+ "from nrengtechfraction f "
					+ "inner join enginetech t on f.engtechid = t.engtechid "
					+ "where f.processgroupid = ? "
					+ "order by f.scc,f.hpmin,t.engtechName,f.modelyearID";

			String formatLine1 = "     %1$10s     %2$5d%3$5d    %4$-10s";
			String formatLine2 = "%1$-5d                              %2$-10.5f";

			// ex tech frac
			if (oldFormatTech) {
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, 1);
				results = SQLRunner.executeQuery(statement, sql);
				printWriter = new PrintWriter(new BufferedWriter(
						new OutputStreamWriter(new FileOutputStream(new File(
								baseDir + System.getProperty("file.separator")
										+ this.nonroadDataFiles.techExhFile)))));
				printWriter.print("/MOVES TECH FRAC/"
						+ System.getProperty("line.separator"));
				if (results != null) {
					boolean canInsert = true;
					SccTech_HpminmaxYear minmaxYear = new SccTech_HpminmaxYear();
					SccTech_HpminmaxYear.TechRecord record = null;
					SccTech_HpminmaxYear.TechRecord preRecord = null;

					while (results.next()) {
						String scc = results.getString(1);
						int hpmin = results.getInt(2);
						int hpmax = results.getInt(3);
						int year = results.getInt(5);
						String techName = results.getString(4);
						double frac = results.getDouble(6);
						preRecord = record;
						record = SccTech_HpminmaxYear.createTechRecord(scc,
								hpmin, hpmax, year, techName);

						if (this.multiRecordPerLine) {
							canInsert = minmaxYear.add(scc, hpmin, hpmax, year,
									techName, frac);
							// Logger.log(LogMessageCategory.INFO, "" +
							// minmaxYear);

							if (!canInsert) {//
								SccTech_HpminmaxYear.YearTech yearTech = minmaxYear
										.getYearTech(preRecord.hpmin,
												preRecord.hpmax); //
								// System.out.println( "" + minmaxYear);
								if (yearTech == null) {
									Logger.log(LogMessageCategory.ERROR,
											"yearTech for year " + year
													+ " should not be null!");
									// System.out.println( "yearTech for year "
									// +
									// year + " should not be null!");
									isOK = false;
								}

								// QA, sum of the tech fracs = 1, each have same
								// tech types
								TechFraction techFrac = null;
								TechFraction preTechFrac = null;
								String[] techTypes = null;
								String[] preTechTypes = null;
								for (Entry<Integer, TechFraction> entry : yearTech.yearTech
										.entrySet()) {
									int yr = entry.getKey().intValue();
									preTechFrac = techFrac;
									techFrac = entry.getValue();

									if (Math.abs(1 - techFrac.totalFrac) > NonroadHelper.delta) {
										Logger.log(LogMessageCategory.ERROR,
												"Sum of fractions of each tech types for year "
														+ yr + " is not 1!");
										isOK = false;
										break;
									}
									preTechTypes = techTypes;
									techTypes = techFrac.techFrac.keySet()
											.toArray(new String[0]);

									if (techTypes != null
											&& preTechTypes != null) {
										java.util.Arrays.sort(techTypes);
										java.util.Arrays.sort(preTechTypes);
										if (techTypes.length != preTechTypes.length) {
											Logger.log(
													LogMessageCategory.ERROR,
													"Number of tech types for year "
															+ yr
															+ " is different from other years'!");
											Logger.log(
													LogMessageCategory.INFO,
													"Tech Types:     "
															+ techTypes
																	.toString());
											Logger.log(
													LogMessageCategory.INFO,
													"Pre Tech Types: "
															+ preTechTypes
																	.toString());
											Logger.log(LogMessageCategory.INFO,
													"Tech Frac:      "
															+ techFrac);
											Logger.log(LogMessageCategory.INFO,
													"Pre Tech Frac:  "
															+ preTechFrac);
											isOK = false;
											break;
										}
										for (int i = 0; i < techTypes.length; i++) {
											if (techTypes[i] == null
													|| preTechTypes[i] == null) {
												Logger.log(
														LogMessageCategory.ERROR,
														"Tere are invalid tech type for year "
																+ yr + "!");
												isOK = false;
												break;
											}
											if (!techTypes[i]
													.equals(preTechTypes[i])) {
												Logger.log(
														LogMessageCategory.ERROR,
														"Tere are different tech type for year "
																+ yr
																+ "from other years'!");
												isOK = false;
												break;
											}
										}
									}

								}
								if (!isOK) {
									break;
								}
								// output
								int i = 0;
								for (Entry<Integer, TechFraction> entry : yearTech.yearTech
										.entrySet()) {
									if (i == 0) {
										// write header
										printWriter.printf(formatLine1,
												preRecord.scc, preRecord.hpmin,
												preRecord.hpmax, techTypes[0]);
										for (int j = 1; j < techTypes.length; j++) {
											printWriter.printf("$-10s",
													techTypes[j]);
										}
										printWriter.print(System
												.getProperty("line.separator"));
									}
									int yr = entry.getKey();
									Double[] fracs = entry.getValue().techFrac
											.values().toArray(new Double[0]);
									printWriter.printf(formatLine2, yr,
											fracs[0]);
									for (int j = 1; j < techTypes.length; j++) {
										printWriter.printf("$-10.3f", fracs[j]);
									}
									printWriter.print(System
											.getProperty("line.separator"));
								}

								minmaxYear = new SccTech_HpminmaxYear();
								canInsert = minmaxYear.add(scc, hpmin, hpmax,
										year, techName, frac);

							}

						} else {

							printWriter.printf(formatLine1,
									results.getString(1), // scc
									results.getInt(2), // min
									results.getInt(3), // max
									results.getString(4) // tech name
									);
							printWriter.print(System
									.getProperty("line.separator"));
							printWriter.printf(formatLine2, results.getInt(5), // year
									results.getDouble(6) // fraction
									);
							printWriter.print(System
									.getProperty("line.separator"));
						}
					}
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate file TECH-EXH.DAT.");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
			} else {
				statement = executionDB.prepareStatement(sql);
				statement.setInt(1, 1);
				results = SQLRunner.executeQuery(statement, sql);
				printWriter = new PrintWriter(new BufferedWriter(
						new OutputStreamWriter(new FileOutputStream(new File(
								baseDir + System.getProperty("file.separator")
										+ this.nonroadDataFiles.techExhFile)))));
				printWriter.print("/MOVES TECH FRAC/"
						+ System.getProperty("line.separator"));
				if (results != null) {
					while (results.next()) {
						printWriter.printf(formatLine1, results.getString(1), // scc
								results.getInt(2), // min
								results.getInt(3), // max
								results.getString(4) // tech name
								);
						printWriter.print(System.getProperty("line.separator"));
						printWriter.printf(formatLine2, results.getInt(5), // year
								results.getDouble(6) // fraction
								);
						printWriter.print(System.getProperty("line.separator"));
					}
					results.close();

				} else {
					isOK = false;
				}
				if (!isOK) {
					Logger.log(LogMessageCategory.ERROR,
							"Failed to generate file TECH-EXH.DAT.");
				}
				statement.close();
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
			}

			// evap tech frac
			statement = executionDB.prepareStatement(sql);
			statement.setInt(1, 2);
			results = SQLRunner.executeQuery(statement, sql);
			printWriter = new PrintWriter(new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(new File(
							baseDir + System.getProperty("file.separator")
									+ this.nonroadDataFiles.techEvpFile)))));
			printWriter.print("/MOVES EVAP TECH FRAC/"
					+ System.getProperty("line.separator"));
			if (results != null) {
				while (results.next()) {
					printWriter.printf(formatLine1, results.getString(1), // scc
							results.getInt(2), // min
							results.getInt(3), // max
							results.getString(4) // tech name
							);
					printWriter.print(System.getProperty("line.separator"));
					printWriter.printf(formatLine2, results.getInt(5), // year
							results.getDouble(6) // fraction
							);
					printWriter.print(System.getProperty("line.separator"));
				}
				results.close();

			} else {
				isOK = false;
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to generate file TECH-EVP.DAT.");
			}
			statement.close();
			printWriter.print("/END/" + System.getProperty("line.separator"));
			printWriter.flush();
			printWriter.close();
		} catch (SQLException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating TECH files : " + e);
			isOK = false;
		} catch (FileNotFoundException e) {
			Logger.logException(LogMessageCategory.ERROR, e);
			Logger.log(LogMessageCategory.ERROR,
					"Exception in generating EMSFAC files : " + e);
			isOK = false;
			// e.printStackTrace();
		} finally {
			if (printWriter != null) {
				printWriter.print("/END/"
						+ System.getProperty("line.separator"));
				printWriter.flush();
				printWriter.close();
				printWriter = null;
			}
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

	public boolean generateDataFiles(Connection executionDB, int[] stateIDs,
			String baseDir, boolean doExhaust, boolean doCrank, boolean doEvap,
			NonroadDataFilesObj nonroadDataFiles) {
		boolean isOK = true;
		this.nonroadDataFiles = nonroadDataFiles;
		this.generateActivityDatFile(executionDB, baseDir);
		this.generateAllocatefFiles(executionDB, baseDir);
		this.generateDetfacFile(executionDB, baseDir);
		if (!this.useOldEmsfacFiles) {
			this.generateEmsfacFile(executionDB, doExhaust, doCrank, doEvap,
					baseDir);
		}
		this.generateGrowthFile(executionDB, baseDir);
		this.generatePopFile(executionDB, stateIDs, baseDir);
		this.generateRetrofitFile(executionDB, baseDir);
		if (!this.useOldSeasonFiles) {
			this.generateSeasonFile(executionDB, baseDir);
		}
		if (!this.useOldTechFiles) {
			this.generateTechFile(executionDB, baseDir);
		}
		return isOK;
	}

	public static void main(String[] args) {
		try {
			Connection db = DatabaseConnectionManager
					.checkOutConnection(MOVESDatabaseType.DEFAULT);
			NonroadDataFileHelper helper = new NonroadDataFileHelper();
			NonroadDataFilesObj nonroadDataFiles = new NonroadDataFilesObj();
			// helper.generateActivityDatFile(db,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\ACTIVITY\\ACTIVITY.DAT");
			// helper.generateAllocatefFiles(db,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\ALLOCATE");
			// helper.generateDetfacFile(db,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\DETFAC");
			// helper.generateEmsfacFile(db, true, true, true,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\EMSFAC");
			// helper.generateGrowthFile(db,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\GROWTH\\NATION.GRW");
			int[] stateIDs = { 1, 2 };
			// helper.generatePopFile(db, statIDs,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\POP");
			// helper.generateRetrofitFile(db,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\RETROFIT\\retrotst.dat");
			// helper.generateSeasonFile(db,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\SEASON");
			// helper.generateTechFile(db,
			// "C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a\\DATA\\TECH");
			helper.generateDataFiles(db, stateIDs,
					"C:\\NMIM\\NMIM20090504\\NONROAD\\NR08a", true, true, true,
					nonroadDataFiles);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	static class LineMerger {
		PrintWriter writer = null;
		String unwrittenLine = null;
		int lowItemIndex, lowItemEndIndex;
		int highItemIndex, highItemEndIndex;
		int minLineLength;
		public boolean mustBeExactMatch = false;

		public LineMerger(PrintWriter writerToUse,
				int lowItemIndexToUse, int lowItemLengthToUse,
				int highItemIndexToUse, int highItemLengthToUse) {
			writer = writerToUse;
			lowItemIndex = lowItemIndexToUse;
			lowItemEndIndex = lowItemIndexToUse + lowItemLengthToUse;
			highItemIndex = highItemIndexToUse;
			highItemEndIndex = highItemIndexToUse + highItemLengthToUse;
			
			minLineLength = Math.max(lowItemEndIndex,highItemEndIndex);
		}
		
		public void flush() {
			if(unwrittenLine != null) {
				writer.println(unwrittenLine);
				unwrittenLine = null;
			}
		}
		
		public void println(String line) {
			if(unwrittenLine == null) {
				unwrittenLine = line;
				return;
			}
			if(line.length() != unwrittenLine.length() || line.length() < minLineLength) {
				flush();
				unwrittenLine = line;
				return;
			}
			String aMin = unwrittenLine.substring(lowItemIndex,lowItemEndIndex);
			String aMax = unwrittenLine.substring(highItemIndex,highItemEndIndex);
			String bMin = line.substring(lowItemIndex,lowItemEndIndex);
			String bMax = line.substring(highItemIndex,highItemEndIndex);
			if(mustBeExactMatch && !aMax.trim().equals(bMin.trim())) {
				flush();
				unwrittenLine = line;
				return;
			}
			if(!mustBeExactMatch) {
				int aMaxValue = Integer.parseInt(aMax.trim());
				int bMinValue = Integer.parseInt(bMin.trim());
				int bMaxValue = Integer.parseInt(bMax.trim());
				if(aMaxValue > bMinValue || bMaxValue <= aMaxValue) {
					flush();
					unwrittenLine = line;
					return;
				}
			}
			char[] c = unwrittenLine.toCharArray();
			char[] nc = line.toCharArray();
			for(int i=0;i<c.length;i++) {
				if(i >= lowItemIndex && i<lowItemEndIndex) {
					continue;
				} else if(i >= highItemIndex && i<highItemEndIndex) {
					continue;
				}
				if(c[i] != nc[i]) {
					flush();
					unwrittenLine = line;
					return;
				}
			}
			for(int i=0;i<bMax.length();i++) {
				c[highItemIndex+i] = bMax.charAt(i);
			}
			unwrittenLine = new String(c);
		}
	}
}
