/**
 * Project: EpaMOVES
 */
package gov.epa.otaq.moves.worker.framework;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.master.framework.MasterLoopContext;

import java.io.*;
import java.sql.*;
import java.util.*;

/**
 * Read Nonroad output files into MySQL data tables.
 *
 * @author Wesley Faler
 * @author Jizhen Zhao
 * @version 2015-08-19
**/
public class NonroadOutputDataLoader {
	/** true to load output using MySQL's "LOAD DATA INFILE" command, false to use an INSERT statement for each record **/
	static final boolean LOAD_DATA_INFILE = true;

	/** Number of grams in a U.S. ton **/
	static final double USTON_TO_GRAM = 907184.818;

	class BmyRecord {
		int countyID;
		int subRegID;
		String scc;
		int hp;
		String techType;
		int mYr;
		double population;
		double thcExhaust; // US tons/month natively, stored here as grams/day
		double coExhaust; // US tons/month natively, stored here as grams/day
		double noxExhaust; // US tons/month natively, stored here as grams/day
		double co2Exhaust; // US tons/month natively, stored here as grams/day
		double so2Exhaust; // US tons/month natively, stored here as grams/day
		double pmExhaust; // US tons/month natively, stored here as grams/day
		double crankcase; // US tons/month natively, stored here as grams/day
		double fuelCons; // gallons/month natively, stored here as gallons/day
		double activity;
		double lf;
		double hpAvg;
		double fracRetro = -1;
		double unitsRetro = -1;
	}

	class BmvRecord {
		int countyID;
		int subRegID;
		String scc;
		int hp;
		String techType;
		int mYr;
		double population;
		double hot_Soaks; // US tons/month natively, stored here as grams/day
		double diurnal; // US tons/month natively, stored here as grams/day
		double displacement; // US tons/month natively, stored here as grams/day
		double spillage; // US tons/month natively, stored here as grams/day
		double runLoss; // US tons/month natively, stored here as grams/day
		double tankPerm; // US tons/month natively, stored here as grams/day
		double hosePerm; // US tons/month natively, stored here as grams/day
		double fuelCons; // gallons/month natively, stored here as gallons/day
		double activity;
	}

	private static final int bmyColNum = 21;

	private static final int bmvColNum = 17;

	private static final int colCnty = 0;
	private static final int colSubR = 1;
	private static final int colSCC = 2;
	private static final int colHP = 3;
	private static final int colTechType = 4;
	private static final int colMYr = 5;
	private static final int colPopulation = 6;
	private static final int colTHC_Exhaust = 7;
	private static final int colCO_Exhaust = 8;
	private static final int colNOx_Exhaust = 9;
	private static final int colCO2_Exhaust = 10;
	private static final int colSO2_Exhaust = 11;
	private static final int colPM_Exhaust = 12;
	private static final int colCrankcase = 13;
	private static final int colFuelCons = 14;
	private static final int colActivity = 15;
	private static final int colLF = 16;
	private static final int colHPAvg = 17;
	private static final int colFracRetro = 18;
	private static final int colUnitsRetro = 19;

	private static final int bmvCnty = 0;
	private static final int bmvSubR = 1;
	private static final int bmvSCC = 2;
	private static final int bmvHP = 3;
	private static final int bmvTechType = 4;
	private static final int bmvMYr = 5;
	private static final int bmvPopulation = 6;
	private static final int bmvHot_Soaks = 7;
	private static final int bmvDiurnal = 8;
	private static final int bmvDisplacement = 9;
	private static final int bmvSpillage = 10;
	private static final int bmvRunLoss = 11;
	private static final int bmvTankPerm = 12;
	private static final int bmvHosePerm = 13;
	private static final int bmvFuelCons = 14;
	private static final int bmvActivity = 15;

	boolean need101 = false;
	boolean need201 = false;
	boolean need301 = false;
	boolean need9001 = false;
	boolean need3101 = false;
	boolean need10001 = false;
	boolean need115 = false;
	boolean need9901 = false;
	boolean need131 = false;
	boolean need130 = false;
	boolean need118 = false;
	boolean need119 = false;
	boolean need132 = false;
	boolean need120 = false;
	boolean need121 = false;
	boolean need3001 = false;
	boolean need11001 = false;

	double monthToRealDayFactor = 1.0;
	
	File workingFolderPath = null;

	public NonroadOutputDataLoader(File workingFolderPathToUse) {
		workingFolderPath = workingFolderPathToUse;
	}

	public void setFilter(String sqlStatement) {
		if(sqlStatement.endsWith(";")) {
			sqlStatement = sqlStatement.substring(0,sqlStatement.length()-1);
			sqlStatement += " ";
		}
		
		String factorMarker = "monthToRealDayFactor=";
		int factorIndex = sqlStatement.indexOf(factorMarker);
		if(factorIndex >= 0) {
			factorIndex += factorMarker.length();
			int endIndex = sqlStatement.indexOf(" ",factorIndex);
			if(endIndex >= 0) {
				String t = sqlStatement.substring(factorIndex,endIndex);
				try {
					monthToRealDayFactor = Double.parseDouble(t);
				} catch(Exception e) {
					Logger.logError(e,"Bad value given for nonroad monthToRealDayFactor: " + t);
				}
			}
		}

		need101 = sqlStatement.indexOf(" 101 ") >= 0;
		need201 = sqlStatement.indexOf(" 201 ") >= 0;
		need301 = sqlStatement.indexOf(" 301 ") >= 0;
		need9001 = sqlStatement.indexOf(" 9001 ") >= 0;
		need3101 = sqlStatement.indexOf(" 3101 ") >= 0;
		need10001 = sqlStatement.indexOf(" 10001 ") >= 0;
		need11001 = sqlStatement.indexOf(" 11001 ") >= 0;
		need115 = sqlStatement.indexOf(" 115 ") >= 0;
		need9901 = sqlStatement.indexOf(" 9901 ") >= 0;
		need131 = sqlStatement.indexOf(" 131 ") >= 0;
		need130 = sqlStatement.indexOf(" 130 ") >= 0;
		need118 = sqlStatement.indexOf(" 118 ") >= 0;
		need119 = sqlStatement.indexOf(" 119 ") >= 0;
		need132 = sqlStatement.indexOf(" 132 ") >= 0;
		need120 = sqlStatement.indexOf(" 120 ") >= 0;
		need121 = sqlStatement.indexOf(" 121 ") >= 0;
		need3001 = sqlStatement.indexOf(" 3001 ") >= 0;
	}

	/**
	 * Set the value of a prepared statement field, noting its value in a separate structure as well.
	 * @param textItems text values to be populated
	 * @param statement statement to be populated
	 * @param index one-based index for use in the the prepared statement
	 * @param value value to be stored
	 * @throws SQLException if unable to set the parameter
	**/
	void setInt(String[] textItems, PreparedStatement statement, int index, int value) throws SQLException {
		textItems[index-1] = "" + value;
		statement.setInt(index,value);
	}

	/**
	 * Set the value of a prepared statement field, noting its value in a separate structure as well.
	 * @param textItems text values to be populated
	 * @param statement statement to be populated
	 * @param index one-based index for use in the the prepared statement
	 * @param value value to be stored
	 * @throws SQLException if unable to set the parameter
	**/
	void setFloat(String[] textItems, PreparedStatement statement, int index, double value) throws SQLException {
		textItems[index-1] = "" + value;
		statement.setFloat(index,(float)value);
	}

	/**
	 * Set the value of a prepared statement field, noting its value in a separate structure as well.
	 * @param textItems text values to be populated
	 * @param statement statement to be populated
	 * @param index one-based index for use in the the prepared statement
	 * @param value value to be stored
	 * @throws SQLException if unable to set the parameter
	**/
	void setString(String[] textItems, PreparedStatement statement, int index, String value) throws SQLException {
		value = StringUtilities.safeGetString(value);
		textItems[index-1] = value; // No escaping is needed in the LOAD DATA INFILE file
		statement.setString(index,value);
	}

	/**
	 * Execute a prepared statement or write to a file for subsequent loading.
	 * Uses the LOAD_DATA_INFILE flag to determine internal behavior.
	 * @param textItems text values to be read
	 * @param writer interface to the file to be populated
	 * @param statement statement to be used
	 * @param sql base SQL statement
	 * @return true if data was written to the file
	 * @throws SQLException if unable to execute the statement
	 * @throws IOException if unable to write to the file
	**/
	boolean execute(String[] textItems, PrintWriter writer, PreparedStatement statement, String sql) throws SQLException {
		if(LOAD_DATA_INFILE) {
			String line = textItems[0];
			for(int i=1;i<textItems.length;i++) {
				line += "\t" + textItems[i];
			}
			writer.println(line);
			return true;
		} else {
			SQLRunner.execute(statement,sql);
			return false;
		}
	}

	public boolean loadBmyIntoDatabase(BufferedReader bmyReader,
			BundleManifest manifest, Connection database) {
		boolean isOK = true;

		MasterLoopContext context = new MasterLoopContext();
		context.fromBundleManifestContext(manifest.context);

		File pollutionFile = getTemporaryFile();
		File activityFile = getTemporaryFile();
		PrintWriter pollutionWriter = null, activityWriter = null;
		boolean pollutionFileHasData = false, activityFileHasData = false;

		PreparedStatement statement = null;
		PreparedStatement statementActivity = null;

		String[] pollutionFields = new String[21];
		String[] activityFields = new String[20];

		String sql = "insert ignore into MOVESWorkerOutput ( MOVESRunID, iterationID, yearID, monthID, dayID, " // 1-5
				+ "hourID, stateID, countyID, zoneID, linkID, " // 6-10
				+ "pollutantID, processID, sourceTypeID, fuelTypeID, modelYearID, " // 11-15
				+ "roadTypeID, SCC, engTechID, sectorID, emissionQuant, hpID) " // 16-21
				+ "values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ";

		String sqlActivity = "insert ignore into MOVESWorkerActivityOutput ( MOVESRunID, iterationID, yearID, monthID, dayID, " // 1-5
				+ "hourID, stateID, countyID, zoneID, linkID, " // 6-10
				+ "sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC, " // 11-15
				+ "engTechID, sectorID, activityTypeID, activity, hpID) " // 16-20
				+ "values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ";

		String line;
		try {
			if(LOAD_DATA_INFILE) {
				pollutionWriter = new PrintWriter(new BufferedWriter(new FileWriter(pollutionFile),1024*1024));
				activityWriter = new PrintWriter(new BufferedWriter(new FileWriter(activityFile),1024*1024));
			}

			line = bmyReader.readLine();
			String[] headers = null;

			if (line != null) {
				// process header
				headers = new String[bmyColNum];

				String token = null;
				StringTokenizer st = new StringTokenizer(line, ",");
				int col = 0;
				while (st.hasMoreTokens()) {
					headers[col] = st.nextToken().trim();
					col++;
				}

				statement = database.prepareStatement(sql);
				statementActivity = database.prepareStatement(sqlActivity);

				// output
				setInt(pollutionFields, statement, 1, context.movesRunID);
				setInt(pollutionFields, statement, 2, context.iterationID);
				setInt(pollutionFields, statement, 3, context.year);
				setInt(pollutionFields, statement, 4, context.monthID);
				setInt(pollutionFields, statement, 5, context.dayID);
				setInt(pollutionFields, statement, 6, context.hourID);
				setInt(pollutionFields, statement, 7, context.iterLocation.stateRecordID);
				setInt(pollutionFields, statement, 8, context.iterLocation.countyRecordID);
				setInt(pollutionFields, statement, 9, context.iterLocation.zoneRecordID);
				setInt(pollutionFields, statement, 10, context.iterLocation.linkRecordID);

				// activity output
				setInt(activityFields, statementActivity, 1, context.movesRunID);
				setInt(activityFields, statementActivity, 2, context.iterationID);
				setInt(activityFields, statementActivity, 3, context.year);
				setInt(activityFields, statementActivity, 4, context.monthID);
				setInt(activityFields, statementActivity, 5, context.dayID);
				setInt(activityFields, statementActivity, 6, context.hourID);
				setInt(activityFields, statementActivity, 7, context.iterLocation.stateRecordID);
				setInt(activityFields, statementActivity, 8, context.iterLocation.countyRecordID);
				setInt(activityFields, statementActivity, 9, context.iterLocation.zoneRecordID);
				setInt(activityFields, statementActivity, 10, context.iterLocation.linkRecordID);

				BmyRecord record = null;
				while ((line = bmyReader.readLine()) != null) {
					record = new BmyRecord();
					st = new StringTokenizer(line, ",");
					col = 0;
					while (st.hasMoreTokens()) {
						token = st.nextToken().trim();

						if (col == NonroadOutputDataLoader.colCnty) {
							record.countyID = Integer.parseInt(token);
						} else if (col == NonroadOutputDataLoader.colSubR) {
							record.subRegID = Integer.parseInt(token == null
									|| token.trim().isEmpty() ? "0" : token
									.trim());
						} else if (col == NonroadOutputDataLoader.colSCC) {
							record.scc = token;
						} else if (col == NonroadOutputDataLoader.colHP) {
							record.hp = Integer.parseInt(token);
						} else if (col == NonroadOutputDataLoader.colTechType) {
							record.techType = token;
						} else if (col == NonroadOutputDataLoader.colMYr) {
							record.mYr = Integer.parseInt(token);
						} else if (col == NonroadOutputDataLoader.colPopulation) {
							record.population = Double.parseDouble(token);
						} else if (col == NonroadOutputDataLoader.colTHC_Exhaust) {
							record.thcExhaust = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.colCO_Exhaust) {
							record.coExhaust = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.colNOx_Exhaust) {
							record.noxExhaust = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.colCO2_Exhaust) {
							record.co2Exhaust = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.colSO2_Exhaust) {
							record.so2Exhaust = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.colPM_Exhaust) {
							record.pmExhaust = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.colCrankcase) {
							record.crankcase = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.colFuelCons) {
							record.fuelCons = Math.max(0,Double.parseDouble(token));
						} else if (col == NonroadOutputDataLoader.colActivity) {
							record.activity = Math.max(0,Double.parseDouble(token));
						} else if (col == NonroadOutputDataLoader.colLF) {
							record.lf = Math.max(0,Double.parseDouble(token));
						} else if (col == NonroadOutputDataLoader.colHPAvg) {
							record.hpAvg = Math.max(0,Double.parseDouble(token));
						} else if (col == NonroadOutputDataLoader.colFracRetro) {
							record.fracRetro = Math.max(0,Double.parseDouble(token));
						} else if (col == NonroadOutputDataLoader.colUnitsRetro) {
							record.unitsRetro = Math.max(0,Double.parseDouble(token));
						}

						col++;
					}

					// output
					int fuelTypeID = getFuelTypeID(database, record.scc);
					
					setInt(pollutionFields, statement, 13, getSourceTypeID(database, record.scc, record.hp));
					setInt(pollutionFields, statement, 14, fuelTypeID);
					setInt(pollutionFields, statement, 15, record.mYr);
					setInt(pollutionFields, statement, 16, 100);
					setString(pollutionFields, statement, 17, record.scc);
					setInt(pollutionFields, statement, 18, getEngTechID(database, record.techType));
					setInt(pollutionFields, statement, 19, getSectorID(database, record.scc));
					setInt(pollutionFields, statement, 21, record.hp);

					// activity output
					setInt(activityFields, statementActivity, 11, getSourceTypeID(database, record.scc, record.hp));
					setInt(activityFields, statementActivity, 12, fuelTypeID);
					setInt(activityFields, statementActivity, 13, record.mYr);
					setInt(activityFields, statementActivity, 14, 100);
					setString(activityFields, statementActivity, 15, record.scc);
					setInt(activityFields, statementActivity, 16, getEngTechID(database, record.techType));
					setInt(activityFields, statementActivity, 17, getSectorID(database, record.scc));
					setInt(activityFields, statementActivity, 20, record.hp);

					// output
					if(need101) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 1);
						setFloat(pollutionFields, statement, 20, record.thcExhaust);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need201) {
						setInt(pollutionFields, statement, 11, 2);
						setInt(pollutionFields, statement, 12, 1);
						setFloat(pollutionFields, statement, 20, record.coExhaust);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need301) {
						setInt(pollutionFields, statement, 11, 3);
						setInt(pollutionFields, statement, 12, 1);
						setFloat(pollutionFields, statement, 20, record.noxExhaust);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need9001) {
						setInt(pollutionFields, statement, 11, 90);
						setInt(pollutionFields, statement, 12, 1);
						setFloat(pollutionFields, statement, 20, record.co2Exhaust);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need3101) {
						setInt(pollutionFields, statement, 11, 31);
						setInt(pollutionFields, statement, 12, 1);
						setFloat(pollutionFields, statement, 20, record.so2Exhaust);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need10001) {
						setInt(pollutionFields, statement, 11, 100);
						setInt(pollutionFields, statement, 12, 1);
						setFloat(pollutionFields, statement, 20, record.pmExhaust);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need115) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 15);
						setFloat(pollutionFields, statement, 20, record.crankcase);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need9901) {
						setInt(pollutionFields, statement, 11, 99);
						setInt(pollutionFields, statement, 12, 1);
						setFloat(pollutionFields, statement, 20, fuelGallonsToGrams(getFuelTypeID(database, record.scc),record.fuelCons));
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need11001) {
						double factor = 0;
						switch(fuelTypeID) {
							case 1:
								factor = 0.92;
								break;
							case 2:
								factor = 0.97;
								break;
							case 23:
								factor = 0.97;
								break;
							case 24:
								factor = 0.97;
								break;
							case 3:
								factor = 1;
								break;
							case 4:
								factor = 1;
								break;
						}
						if(factor > 0) {
							setInt(pollutionFields, statement, 11, 110);
							setInt(pollutionFields, statement, 12, 1);
							setFloat(pollutionFields, statement, 20, record.pmExhaust * factor);
							pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
						}
					}
					if(need3001) {
						double factor = 0;
						switch(fuelTypeID) {
							case 1:
								factor = 0.116; // each gallon of gasoline makes 116 mg of NH3/Ammonia.
								break;
							case 2:
								factor = 0.0833; // each gallon of diesel makes 83.3 mg of NH3/Ammonia.
								break;
							case 23:
								factor = 0.0833; // each gallon of diesel makes 83.3 mg of NH3/Ammonia.
								break;
							case 24:
								factor = 0.0833; // each gallon of diesel makes 83.3 mg of NH3/Ammonia.
								break;
						}
						if(factor > 0) {
							setInt(pollutionFields, statement, 11, 30);
							setInt(pollutionFields, statement, 12, 1);
							setFloat(pollutionFields, statement, 20, record.fuelCons * factor);
							pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
						}
					}

					// activity output

					setInt(activityFields, statementActivity, 18, 2); // activity
					setFloat(activityFields, statementActivity, 19, record.activity);
					activityFileHasData = execute(activityFields, activityWriter, statementActivity, sqlActivity);

					setInt(activityFields, statementActivity, 18, 6); // population
					setFloat(activityFields, statementActivity, 19, record.population);
					activityFileHasData = execute(activityFields, activityWriter, statementActivity, sqlActivity);

					setInt(activityFields, statementActivity, 18, 12); // LF
					setFloat(activityFields, statementActivity, 19, record.lf);
					activityFileHasData = execute(activityFields, activityWriter, statementActivity, sqlActivity);

					setInt(activityFields, statementActivity, 18, 9); // HPAvg
					setFloat(activityFields, statementActivity, 19, record.hpAvg);
					activityFileHasData = execute(activityFields, activityWriter, statementActivity, sqlActivity);

					if (record.fracRetro != -1) {
						setInt(activityFields, statementActivity, 18, 10); // FracRetro
						setFloat(activityFields, statementActivity, 19, record.fracRetro);
						activityFileHasData = execute(activityFields, activityWriter, statementActivity, sqlActivity);
					}

					if (record.unitsRetro != -1) {
						setInt(activityFields, statementActivity, 18, 11); // unitsRetro
						setFloat(activityFields, statementActivity, 19, record.unitsRetro);
						activityFileHasData = execute(activityFields, activityWriter, statementActivity, sqlActivity);
					}
				}
			}
			
			if(pollutionFileHasData) {
				pollutionWriter.close();
				pollutionWriter = null;
				String cmd = "LOAD DATA INFILE '" + pollutionFile.getCanonicalPath().replace('\\', '/')
						+ "' IGNORE INTO TABLE MOVESWorkerOutput ( MOVESRunID, iterationID, yearID, monthID, dayID, " // 1-5
						+ "hourID, stateID, countyID, zoneID, linkID, " // 6-10
						+ "pollutantID, processID, sourceTypeID, fuelTypeID, modelYearID, " // 11-15
						+ "roadTypeID, SCC, engTechID, sectorID, emissionQuant, hpID)"; // 16-21
				SQLRunner.executeSQL(database,cmd);
			}
			if(activityFileHasData) {
				activityWriter.close();
				activityWriter = null;
				String cmd = "LOAD DATA INFILE '" + activityFile.getCanonicalPath().replace('\\', '/')
						+ "' IGNORE INTO TABLE MOVESWorkerActivityOutput ( MOVESRunID, iterationID, yearID, monthID, dayID, " // 1-5
						+ "hourID, stateID, countyID, zoneID, linkID, " // 6-10
						+ "sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC, " // 11-15
						+ "engTechID, sectorID, activityTypeID, activity, hpID)"; // 16-20
				SQLRunner.executeSQL(database,cmd);
			}
		} catch (SQLException e) {
			e.printStackTrace();
			isOK = false;
		} catch (IOException e) {
			e.printStackTrace();
			isOK = false;
		} finally {
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (statementActivity != null) {
				try {
					statementActivity.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if(pollutionWriter != null) {
				try {
					pollutionWriter.close();
				} catch(Exception e) {
					// Failure to close this file should not be an issue
				}
				pollutionWriter = null;
			}
			if(activityWriter != null) {
				try {
					activityWriter.close();
				} catch(Exception e) {
					// Failure to close this file should not be an issue
				}
				activityWriter = null;
			}
			FileUtilities.deleteFileWithRetry(pollutionFile);
			FileUtilities.deleteFileWithRetry(activityFile);
		}

		return isOK;
	}

	public boolean loadBmvIntoDatabase(BufferedReader bmvReader,
			BundleManifest manifest, Connection database) {
		boolean isOK = true;

		File pollutionFile = getTemporaryFile();
		PrintWriter pollutionWriter = null;
		boolean pollutionFileHasData = false;

		String[] pollutionFields = new String[21];

		MasterLoopContext context = new MasterLoopContext();
		context.fromBundleManifestContext(manifest.context);

		PreparedStatement statement = null;

		String sql = "insert ignore into MOVESWorkerOutput ( MOVESRunID, iterationID, yearID, monthID, dayID, " // 1-5
				+ "hourID, stateID, countyID, zoneID, linkID, " // 6-10
				+ "pollutantID, processID, sourceTypeID, fuelTypeID, modelYearID, " // 11-15
				+ "roadTypeID, SCC, engTechID, sectorID, emissionQuant, hpID) " // 16-21
				+ "values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ";

		String line;
		try {
			if(LOAD_DATA_INFILE) {
				pollutionWriter = new PrintWriter(new BufferedWriter(new FileWriter(pollutionFile),1024*1024));
			}

			line = bmvReader.readLine();
			String[] headers = null;

			if (line != null) {
				// process header
				headers = new String[bmvColNum];

				String token = null;
				StringTokenizer st = new StringTokenizer(line, ",");
				int col = 0;
				while (st.hasMoreTokens()) {
					headers[col] = st.nextToken().trim();
					col++;
				}

				statement = database.prepareStatement(sql);

				// output
				setInt(pollutionFields, statement, 1, context.movesRunID);
				setInt(pollutionFields, statement, 2, context.iterationID);
				setInt(pollutionFields, statement, 3, context.year);
				setInt(pollutionFields, statement, 4, context.monthID);
				setInt(pollutionFields, statement, 5, context.dayID);
				setInt(pollutionFields, statement, 6, context.hourID);
				setInt(pollutionFields, statement, 7, context.iterLocation.stateRecordID);
				setInt(pollutionFields, statement, 8, context.iterLocation.countyRecordID);
				setInt(pollutionFields, statement, 9, context.iterLocation.zoneRecordID);
				setInt(pollutionFields, statement, 10, context.iterLocation.linkRecordID);

				BmvRecord record = null;
				while ((line = bmvReader.readLine()) != null) {

					record = new BmvRecord();
					st = new StringTokenizer(line, ",");
					col = 0;
					while (st.hasMoreTokens()) {
						token = st.nextToken().trim();

						if (col == NonroadOutputDataLoader.bmvCnty) {
							record.countyID = Integer.parseInt(token);
						} else if (col == NonroadOutputDataLoader.bmvSubR) {
							record.subRegID = Integer.parseInt(token == null
									|| token.trim().isEmpty() ? "0" : token
									.trim());
						} else if (col == NonroadOutputDataLoader.bmvSCC) {
							record.scc = token;
						} else if (col == NonroadOutputDataLoader.bmvHP) {
							record.hp = Integer.parseInt(token);
						} else if (col == NonroadOutputDataLoader.bmvTechType) {
							record.techType = token;
						} else if (col == NonroadOutputDataLoader.bmvMYr) {
							record.mYr = Integer.parseInt(token);
						} else if (col == NonroadOutputDataLoader.bmvPopulation) {
							record.population = Math.max(0,Double.parseDouble(token));
						} else if (col == NonroadOutputDataLoader.bmvHot_Soaks) {
							record.hot_Soaks = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.bmvDiurnal) {
							record.diurnal = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.bmvDisplacement) {
							record.displacement = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.bmvSpillage) {
							record.spillage = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.bmvRunLoss) {
							record.runLoss = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.bmvTankPerm) {
							record.tankPerm = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.bmvHosePerm) {
							record.hosePerm = Math.max(0,Double.parseDouble(token))
									* USTON_TO_GRAM;
						} else if (col == NonroadOutputDataLoader.bmvFuelCons) {
							record.fuelCons = Math.max(0,Double.parseDouble(token));
						} else if (col == NonroadOutputDataLoader.bmvActivity) {
							record.activity = Math.max(0,Double.parseDouble(token));
						}

						col++;
					}

					// output
					int fuelTypeID = getFuelTypeID(database, record.scc);
					setInt(pollutionFields, statement, 13, getSourceTypeID(database, record.scc, record.hp));
					setInt(pollutionFields, statement, 14, fuelTypeID);
					setInt(pollutionFields, statement, 15, record.mYr);
					setInt(pollutionFields, statement, 16, 100);
					setString(pollutionFields, statement, 17, record.scc);
					setInt(pollutionFields, statement, 18, getEngTechID(database, record.techType));
					setInt(pollutionFields, statement, 19, getSectorID(database, record.scc));
					setInt(pollutionFields, statement, 21, record.hp);

					// output
					if(need131) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 31);
						setFloat(pollutionFields, statement, 20, record.hot_Soaks);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need130) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 30);
						setFloat(pollutionFields, statement, 20, record.diurnal);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need118) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 18);
						setFloat(pollutionFields, statement, 20, record.displacement);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need119) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 19);
						setFloat(pollutionFields, statement, 20, record.spillage);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need132) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 32);
						setFloat(pollutionFields, statement, 20, record.runLoss);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need120) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 20);
						setFloat(pollutionFields, statement, 20, record.tankPerm);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
					if(need121) {
						setInt(pollutionFields, statement, 11, 1);
						setInt(pollutionFields, statement, 12, 21);
						setFloat(pollutionFields, statement, 20, record.hosePerm);
						pollutionFileHasData = execute(pollutionFields, pollutionWriter, statement, sql);
					}
				}
			}
			if(pollutionFileHasData) {
				pollutionWriter.close();
				pollutionWriter = null;
				String cmd = "LOAD DATA INFILE '" + pollutionFile.getCanonicalPath().replace('\\', '/')
						+ "' IGNORE INTO TABLE MOVESWorkerOutput ( MOVESRunID, iterationID, yearID, monthID, dayID, " // 1-5
						+ "hourID, stateID, countyID, zoneID, linkID, " // 6-10
						+ "pollutantID, processID, sourceTypeID, fuelTypeID, modelYearID, " // 11-15
						+ "roadTypeID, SCC, engTechID, sectorID, emissionQuant, hpID)"; // 16-21
				SQLRunner.executeSQL(database,cmd);
			}
		} catch (SQLException e) {
			e.printStackTrace();
			isOK = false;
		} catch (IOException e) {
			e.printStackTrace();
			isOK = false;
		} finally {
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if(pollutionWriter != null) {
				try {
					pollutionWriter.close();
				} catch(Exception e) {
					// Failure to close this file should not be an issue
				}
				pollutionWriter = null;
			}
			FileUtilities.deleteFileWithRetry(pollutionFile);
		}

		return isOK;
	}

	/** Cache of results of getSourceTypeID() **/
	private TreeMap<String,Integer> sourceTypeIDCache = new TreeMap<String,Integer>();

	// need to copy nrsourceusetype to client side
	private int getSourceTypeID(Connection database, String scc, int hp)
			throws SQLException {
		String cacheKey = scc + "|" + hp;
		Integer cachedResult = sourceTypeIDCache.get(cacheKey);
		if(cachedResult != null) {
			return cachedResult.intValue();
		}
		
		int id = -1;
		boolean isOK = true;

		String sql = "select sourceTypeID from nrsourceusetype where SCC = ? and NRHPRangeBinID = ?";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			statement = database.prepareStatement(sql);

			statement.setString(1, scc);
			statement.setInt(2, hp);

			results = SQLRunner.executeQuery(statement, sql);
			if ((results != null) && results.next()) {
				id = results.getInt(1);
				results.close();
			} else {
				isOK = false;
			}
		} catch (SQLException e) {
			e.printStackTrace();
			isOK = false;
			throw e;
		} finally {
			if (results != null) {
				try {
					results.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get Source Type ID for SCC=" + scc
								+ " HPBin=" + hp);
			}
		}

		cachedResult = Integer.valueOf(id);
		sourceTypeIDCache.put(cacheKey,cachedResult);
		return id;
	}

	/** Cache of results of getFuelTypeID() **/
	private TreeMap<String,Integer> fuelTypeIDCache = new TreeMap<String,Integer>();

	// need to copy nrscc to client
	private int getFuelTypeID(Connection database, String scc)
			throws SQLException {
		Integer cachedResult = fuelTypeIDCache.get(scc);
		if(cachedResult != null) {
			return cachedResult.intValue();
		}

		int id = -1;
		boolean isOK = true;

		String sql = "select fuelTypeID from nrscc where scc = ?";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			statement = database.prepareStatement(sql);
			statement.setString(1, scc);

			results = SQLRunner.executeQuery(statement, sql);
			if ((results != null) && results.next()) {
				id = results.getInt(1);
				results.close();
			} else {
				isOK = false;
			}
		} catch (SQLException e) {
			e.printStackTrace();
			isOK = false;
			throw e;
		} finally {
			if (results != null) {
				try {
					results.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get Fuel Type ID for SCC=" + scc);
			}
		}

		cachedResult = Integer.valueOf(id);
		fuelTypeIDCache.put(scc,cachedResult);
		return id;
	}

	/** Cache of results of getEngTechID() **/
	private TreeMap<String,Integer> engTechIDCache = new TreeMap<String,Integer>();

	// select engTechID from enginetech where engtechName = TechType
	private int getEngTechID(Connection database, String techType)
			throws SQLException {
		Integer cachedResult = engTechIDCache.get(techType);
		if(cachedResult != null) {
			return cachedResult.intValue();
		}

		int id = -1;
		boolean isOK = true;

		String sql = "select engTechID from enginetech where engtechName = ?";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			statement = database.prepareStatement(sql);
			statement.setString(1, techType);

			results = SQLRunner.executeQuery(statement, sql);
			if ((results != null) && results.next()) {
				id = results.getInt(1);
				results.close();
			} else {
				isOK = false;
			}
		} catch (SQLException e) {
			e.printStackTrace();
			isOK = false;
			throw e;
		} finally {
			if (results != null) {
				try {
					results.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get Eng Tech ID for TechType=" + techType);
			}
		}

		cachedResult = Integer.valueOf(id);
		engTechIDCache.put(techType,cachedResult);
		return id;
	}

	/** Cache of results of getSectorID() **/
	private TreeMap<String,Integer> sectorIDCache = new TreeMap<String,Integer>();

	// select sectorid from nrscc inner join nrequipmenttype on
	// nrscc.nrequiptypeid = nrequipmenttype.nrequiptypeid where scc = SCC
	private int getSectorID(Connection database, String scc)
			throws SQLException {
		Integer cachedResult = sectorIDCache.get(scc);
		if(cachedResult != null) {
			return cachedResult.intValue();
		}

		int id = -1;
		boolean isOK = true;

		String sql = "select sectorid from nrscc inner join nrequipmenttype on nrscc.nrequiptypeid = nrequipmenttype.nrequiptypeid where scc = ?";
		PreparedStatement statement = null;
		ResultSet results = null;
		try {
			statement = database.prepareStatement(sql);
			statement.setString(1, scc);

			results = SQLRunner.executeQuery(statement, sql);
			if ((results != null) && results.next()) {
				id = results.getInt(1);
				results.close();
			} else {
				isOK = false;
			}
		} catch (SQLException e) {
			e.printStackTrace();
			isOK = false;
			throw e;
		} finally {
			if (results != null) {
				try {
					results.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (statement != null) {
				try {
					statement.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
			}
			if (!isOK) {
				Logger.log(LogMessageCategory.ERROR,
						"Failed to get Sector ID for TechType=" + scc);
			}
		}

		cachedResult = Integer.valueOf(id);
		sectorIDCache.put(scc,cachedResult);
		return id;
	}

	/**
	 * Convert gallons of fuel into grams of fuel based upon stanard densities
	 * within Nonroad.
	 * @param fuelTypeID fuel identifier
	 * @param gallons gallons of fuel consumed
	 * @return grams of fuel consumed
	**/
	public static float fuelGallonsToGrams(int fuelTypeID, double gallons) {
		double lbs = 0;
		switch(fuelTypeID) {
			case 1: // Gasoline
				lbs = gallons * 6.17;
				break;
			case 2: // Diesel
				lbs = gallons * 7.1;
				break;
			case 23: // Diesel
				lbs = gallons * 7.1;
				break;
			case 24: // Diesel
				lbs = gallons * 7.1;
				break;
			case 3: // CNG
				lbs = gallons * 0.0061;
				break;
			case 4: // LPG
				lbs = gallons * 4.507;
				break;
		}
		return (float)(lbs * 453.592);
	}

	/**
	 * Convert gallons of fuel into grams of fuel based upon stanard densities
	 * within Nonroad.
	 * @param fuelTypeID fuel identifier
	 * @param gallons gallons of fuel consumed
	 * @return grams of fuel consumed
	**/
	public static double fuelGramsToGallons(int fuelTypeID, double grams) {
		double lbs = grams / 453.592;
		switch(fuelTypeID) {
			case 1: // Gasoline
				return lbs / 6.17;
			case 2: // Diesel
				return lbs / 7.1;
			case 23: // Diesel
				return lbs / 7.1;
			case 24: // Diesel
				return lbs / 7.1;
			case 3: // CNG
				return lbs / 0.0061;
			case 4: // LPG
				return lbs / 4.507;
		}
		return 0;
	}

	/** Counter for temporary files, used to ensure unique file names **/
	static int temporaryFileNumber = 0;
	/** Guard for static variables **/
	static Integer globalMutex = Integer.valueOf(819);

	/**
	 * Get the name of a new file that can be used for temporary data.
	 * The name will be unique though the file will not exist.
	 * @return a File with a unique name in the worker temporary folder
	**/
	File getTemporaryFile() {
		while(true) {
			int fileNumber = 0;
			synchronized (globalMutex) {
				temporaryFileNumber++;
				fileNumber = temporaryFileNumber;
			}
			File result = new File(workingFolderPath,"nonroadresults" + fileNumber + ".txt");
			if(!result.exists()) {
				return result;
			}
		}
	}
}
