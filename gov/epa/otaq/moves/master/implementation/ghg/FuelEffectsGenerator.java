/**************************************************************************************************
 * @(#)FuelEffectsGenerator.java
 *
 *
 *
 *************************************************************************************************/
package gov.epa.otaq.moves.master.implementation.ghg;

import gov.epa.otaq.moves.common.*;
import gov.epa.otaq.moves.common.expression.*;
import gov.epa.otaq.moves.master.runspec.*;
import gov.epa.otaq.moves.master.framework.*;
import java.io.*;
import java.sql.*;
import java.util.*;

/**
 * Calculate fuel effects
 *
 * @author		Wesley Faler
 * @version		2016-10-04
**/
public class FuelEffectsGenerator extends Generator {
	/**
	 * @algorithm
	 * @owner Fuel Effects Generator
	 * @generator
	**/

	/**
	 * Control the application of the predictive model to 2004 and newer model years.
	 * When true, the predictive model and sulfur model will be applied to 2004 and newer
	 * model years. When false, the predictive model is not used for these but the sulfur
	 * model is still applied.
	 * The 2004 cutoff year is controlled by a database setting.  In the modelYearCutPoints
	 * table, the row with cutPointName='HighestFuelPredictiveModelYear' sets the last year
	 * prior to this point.  HighestFuelPredictiveModelYear defaults to 2003.
	**/
	public static final boolean DO_PREDICTIVE_CALCS_FOR_2004_AND_NEWER = false;

	/**
	 * true when sulfur model input and output tables should be checked for consistency with
	 * the model rules.
	**/
	private static final boolean CHECK_SULFUR_MODEL_TABLES = true;

	/** Database connection used by all functions.  Setup by executeLoop and cleanDataLoop. **/
	public Connection db;
	/** True when tests are being conducted **/
	public boolean isTesting = false;
	/** True when Rate of Progress is being used **/
	boolean useRateOfProgress = false;
	/** Set of tables created within and that should be removed **/
	TreeSetIgnoreCase createdTables = new TreeSetIgnoreCase();
	/** True during the first executeLoop invocation **/
	boolean isFirstIteration = true;
	/** Total milliseconds spent in this generator **/
	long totalTime = 0;

	/** Current calculation engine **/
	String calculationEngine = "";
	/** IDs of fuel models that apply to the current calculation engine **/
	TreeSet<Integer> fuelModelIDs = new TreeSet<Integer>();
	/**
	 * CSV form of fuelModelIDs suitable for use with SQL.  Unlike fuelModelIDs, this is never
	 * empty, therefore it can be used in SQL without length checks.
	**/
	String fuelModelIDsCSV = "0";
	/** Cache of base formulations keyed by calcEngine|fuelTypeID|modelYearGroupID **/
	TreeMap<String,Integer> baseFormulationsCache = new TreeMap<String,Integer>();

	/** HighestFuelPredictiveModelYear parameter from modelYearCutPoints **/
	static int highestFuelPredictiveModelYear = 2003;
	/**
	 * Cached values from modelYearCutPoints, with names in the format ##cutoff.PARAMETERNAME##
	 * which are suitable for use in SQL text substitution.
	**/
	static TreeMap<String,String> modelYearCutPoints = new TreeMap<String,String>();

	/** Holds fuel and complex model parameter information **/
	static class Parameter {
		public int id;
		public String name;
		public String expression;

		public Parameter(int idToUse, String nameToUse, String expressionToUse) {
			id = idToUse;
			name = nameToUse;
			expression = expressionToUse;
		}
	}
	/** Holds fuel formulation information **/
	public static class FuelFormulation {
		int fuelFormulationID;
		int fuelSubtypeID;
		float RVP;
		float sulfurLevel;
		float ETOHVolume;
		float MTBEVolume;
		float ETBEVolume;
		float TAMEVolume;
		float aromaticContent;
		float olefinContent;
		float benzeneContent;
		float e200;
		float e300;
		float volToWtPercentOxy;
		float bioDieselEsterVolume;
		float cetaneIndex;
		float PAHContent;
		float t50;
		float t90;
		float altRVP;

		public void fromResultSet(ResultSet rs) throws SQLException {
			fuelFormulationID = rs.getInt("fuelFormulationID");
			fuelSubtypeID = rs.getInt("fuelSubtypeID");
			RVP = rs.getFloat("RVP");
			sulfurLevel = rs.getFloat("sulfurLevel");
			ETOHVolume = rs.getFloat("ETOHVolume");
			MTBEVolume = rs.getFloat("MTBEVolume");
			ETBEVolume = rs.getFloat("ETBEVolume");
			TAMEVolume = rs.getFloat("TAMEVolume");
			aromaticContent = rs.getFloat("aromaticContent");
			olefinContent = rs.getFloat("olefinContent");
			benzeneContent = rs.getFloat("benzeneContent");
			e200 = rs.getFloat("e200");
			e300 = rs.getFloat("e300");
			volToWtPercentOxy = rs.getFloat("volToWtPercentOxy");
			bioDieselEsterVolume = rs.getFloat("BioDieselEsterVolume");
			cetaneIndex = rs.getFloat("CetaneIndex");
			PAHContent = rs.getFloat("PAHContent");
			t50 = rs.getFloat("t50");
			t90 = rs.getFloat("t90");
			altRVP = rs.getFloat("altRVP");
		}
	}

	public static interface FuelHandler {
		/**
		 * Get a set of all fuel formulations that exist for a given fuel type
		 * @param fuelTypeID fuel type to be queried
		 * @return set of all fuel formulations for fuelTypeID, never null but may be empty
		 * @throws SQLException if anything goes wrong
		**/
		TreeSet<Integer> getFuelFormulations(int fuelTypeID);
	}

	/** Provider of test fuel information, typically null **/
	public FuelHandler fuelHandler = null;

	/** Fuel parameters **/
	TreeMap<Integer,Parameter> fuelParameters = new TreeMap<Integer,Parameter>();
	/** Complex Model parameters **/
	TreeMap<Integer,Parameter> cmParameters = new TreeMap<Integer,Parameter>();
	/** Fuel formulations, keyed by ID **/
	TreeMap<Integer,FuelFormulation> fuelFormulations = new TreeMap<Integer,FuelFormulation>();
	/** Model Years from RunSpecModelYear **/
	TreeSet<Integer> allowedModelYears = new TreeSet<Integer>();
	/** Pollutant/Processes in the RunSpec **/
	TreeSet<Integer> allowedPolProcesses = new TreeSet<Integer>();
	/** Processes used by the RunSpec **/
	TreeSet<Integer> allowedProcesses = new TreeSet<Integer>();
	/** Pollutant/process entries that have already been copied to CriteriaRatio **/
	TreeSet<Integer> copiedGeneralFuelRatioToCriteriaRatio = new TreeSet<Integer>();
	/** Pollutant/process entries that have already been copied to AltCriteriaRatio **/
	TreeSet<Integer> copiedGeneralFuelRatioToAltCriteriaRatio = new TreeSet<Integer>();
	/** Pollutant/process entries that have already been copied to ATRatio **/
	TreeSet<Integer> copiedGeneralFuelRatioToATRatio = new TreeSet<Integer>();

	/** Hold and execute expressions of the models **/
	public static class ExpressionHolder implements IVariableSource,
			Optimizer.IOptimizerVariableSource {
		/** Variables and their values **/
		public TreeMapIgnoreCase variables = new TreeMapIgnoreCase();
		/**
		 * Named expressions resulting from loading parameters.
		 * Data is Common.NamedExpressionNode
		**/
		public TreeMapIgnoreCase namedExpressions = new TreeMapIgnoreCase();
		/** Value objects for named expressions, cleared when variables are set **/
		public TreeMapIgnoreCase namedExpressionValues = new TreeMapIgnoreCase();

		/** Print debugging information **/
		public void printAll() {
			Logger.log(LogMessageCategory.INFO,"Holder contents, Non-Zero Variables:");
			for(Iterator<String> i=variables.keySet().iterator();i.hasNext();) {
				String name = i.next();
				Value v = (Value)variables.get(name);
				double d = v.getNumber();
				if(d != 0.0) {
					//Logger.log(LogMessageCategory.INFO,"\t\"" + name + "\" = " + (v == null? "null" : v.toString()));
					Logger.log(LogMessageCategory.INFO,"\t\"" + name + "\" = " + d);
				}
			}
			/*
			Logger.log(LogMessageCategory.INFO,"Holder contents, Named Expressions:");
			for(Iterator<String> i=namedExpressions.keySet().iterator();i.hasNext();) {
				String name = i.next();
				Logger.log(LogMessageCategory.INFO,"\t\"" + name + "\"");
			}
			*/
		}

		/**
		 * Set a variable, creating it if needed
		 * @param name variable name
		 * @param value new value for the variable
		 * @throws Exception if anything goes wrong, including use of NaN or Infinity
		**/
		public void set(String name, double value) throws Exception {
			Value v = (Value)variables.get(name);
			if(v == null) {
				v = new Value(value);
				variables.put(name,v);
				namedExpressionValues.clear();
			} else if(v.set(value)) {
				namedExpressionValues.clear();
			}
		}

		/**
		 * Set a variable, creating it if needed
		 * @param name variable name
		 * @param value new value for the variable
		**/
		public void set(String name, String value) {
			if(value == null) {
				value = "";
			}
			Value v = (Value)variables.get(name);
			if(v == null) {
				v = new Value(value);
				variables.put(name,v);
				namedExpressionValues.clear();
			} else if(v.set(value)) {
				namedExpressionValues.clear();
			}
		}

		/**
		 * Get the value of a variable and only a variable.
		 * @param name variable name
		 * @return value of the variable or null if the variable could not be found
		**/
		public Value getVariableValue(String name) {
			return (Value)variables.get(name);
		}

		/**
		 * Get the expression behind a named expression.
		 * @param name expression name
		 * @return root of the expression or null if the expression could not be found
		**/
		public IExpressionNode getExpression(String name) {
			Common.NamedExpressionNode expression =
					(Common.NamedExpressionNode)namedExpressions.get(name);
			if(expression != null) {
				return expression.expression;
			}
			return null;
		}

		/**
		 * Get the value of a variable.
		 * @param name variable name
		 * @return value of the variable
		 * @throws Exception if unable to find the variable or if its value cannot be located
		**/
		public Value getValue(String name) throws Exception {
			Value v = (Value)variables.get(name);
			if(v == null) {
				Common.NamedExpressionNode expression =
						(Common.NamedExpressionNode)namedExpressions.get(name);
				if(expression == null) {
					printAll();
					throw new Exception("Unknown variable or expression " + name);
				}
				v = (Value)namedExpressionValues.get(name);
				if(v == null) {
					v = expression.evaluate(this);
					namedExpressionValues.put(name,v);
				}
			}
			return v;
		}

		/**
		 * Get the textual name of a variable or its textual expression if it is not a simple value.
		 * @param name variable name
		 * @return name of the variable or its expression if not a simple value
		 * @throws Exception if unable to find the variable or if its value cannot be located
		**/
		public String getExpressionText(String name) throws Exception {
			if(variables.containsKey(name)) {
				return name;
			}
			Common.NamedExpressionNode expression =
					(Common.NamedExpressionNode)namedExpressions.get(name);
			if(expression == null) {
				printAll();
				throw new Exception("Unknown variable or expression " + name);
			}
			return expression.getExpressionText(this);
		}

		public void compile(String name, String expression) throws Exception {
			compile(name,expression,null);
		}

		public void compile(String name, String expression, String[] variablePrefixChanges)
				throws Exception {
			ExpressionParser parser = new ExpressionParser();
			IExpressionNode e = parser.compile(expression,variablePrefixChanges);
			Common.NamedExpressionNode namedNode = new Common.NamedExpressionNode(name,e);
			namedExpressions.put(name,namedNode);
		}
	}

	public static class FuelUsageEntry {
		int fuelFormulationID, fuelRegionID, fuelYearID, monthGroupID;
		
		FuelUsageEntry(int fuelFormulationIDToUse, int fuelRegionIDToUse, int fuelYearIDToUse, int monthGroupIDToUse) {
			fuelFormulationID = fuelFormulationIDToUse;
			fuelRegionID = fuelRegionIDToUse;
			fuelYearID = fuelYearIDToUse;
			monthGroupID = monthGroupIDToUse;
		}

		public String toString() {
			return "" + fuelFormulationID + " in " + fuelRegionID + "/" + fuelYearID + "/" + monthGroupID;
		}
	}

	private static class IntegerPair implements Comparable<IntegerPair> {
		public Integer a, b;

		public IntegerPair(Integer aToUse, Integer bToUse) {
			a = aToUse;
			b = bToUse;
		}

		public boolean equals(Object o) {
			if(!(o instanceof IntegerPair)) {
				return false;
			}
			return 0 == compareTo((IntegerPair)o);
		}

		public int compareTo(IntegerPair other) {
			if(a == null && other.a != null) {
				return -1;
			} else if(a != null && other.a == null) {
				return +1;
			} else if(a != null && other.a != null) {
				int diff = a.compareTo(other.a);
				if(diff != 0) {
					return diff;
				}
			}
			// at this point, a's are equal (either both null or both non-null and equal)
			if(b == null && other.b != null) {
				return -1;
			} else if(b != null && other.b == null) {
				return +1;
			} else if(b != null && other.b != null) {
				int diff = b.compareTo(other.b);
				if(diff != 0) {
					return diff;
				}
			}
			return 0;
		}
	}

	/**
	 * Execute a 2 integer column query, returning the results as a set of IntegerPair objects.
	 * @param db database to be used
	 * @param sql statement to be used
	 * @return a set holding all records found, never null but may be empty
	 * @throws SQLException if anything goes wrong
	**/
	public static TreeSet<IntegerPair> getIntegerPairSet(Connection db, String sql) throws SQLException {
		TreeSet<IntegerPair> results = new TreeSet<IntegerPair>();
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				Integer a = null, b = null;
				int t = query.rs.getInt(1);
				if(!query.rs.wasNull()) {
					a = Integer.valueOf(t);
				}
				t = query.rs.getInt(2);
				if(!query.rs.wasNull()) {
					b = Integer.valueOf(t);
				}
				IntegerPair pair = new IntegerPair(a,b);
				results.add(pair);
			}
		} finally {
			query.onFinally();
		}
		return results;
	}

	/**
	 * Test a set of IntegerPair objects for two integers.
	 * @param pairs set to be searched
	 * @param a first item in the pair to be found
	 * @param b second item in the pair to be found
	 * @return true if the set contains the pair
	**/
	public static boolean contains(TreeSet<IntegerPair> pairs, Integer a, Integer b) {
		IntegerPair pair = new IntegerPair(a,b);
		return pairs.contains(pair);
	}

	/** Default constructor **/
	public FuelEffectsGenerator() {
	}

	/** Drop the tables listed in createdTables **/
	public void cleanupTables() {
		if(db == null) {
			return;
		}
		for(Iterator<String> i=createdTables.iterator();i.hasNext();) {
			String tableName = i.next();
			String sql = "drop table if exists " + tableName;
			try {
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				Logger.logSqlError(e,"Unable to delete " + tableName,sql);
			}
		}
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		// Signup so this runs after TankFuelGenerator since TFG modifies fuel formulation
		// parameters.  Do so by using (MasterLoopPriority.GENERATOR-1) as the priority level.

		// Subscribe to every process in the RunSpec.  executeLoop will only do its work upon
		// the first activation though.
		for(Iterator<EmissionProcess>
				i=ExecutionRunSpec.theExecutionRunSpec.targetProcesses.iterator();i.hasNext();) {
			EmissionProcess p = i.next();
			targetLoop.subscribe(this,p,MasterLoopGranularity.PROCESS,
					MasterLoopPriority.GENERATOR-1);
		}
	}

	/**
	 * Called each time the year changes.
	 *
	 * @param context The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext context) {
		if(!isFirstIteration) {
			return;
		}
		useRateOfProgress = ExecutionRunSpec.hasRateOfProgress();

		boolean shouldCheckin = false;
		boolean didAddAltRVP = false;

		try {
			if(db == null) {
				db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
				shouldCheckin = true;
			}

			long start;
			start = System.currentTimeMillis();
			if(isFirstIteration) {
				isFirstIteration = false;
				doExtraATRatioIndexDrop = true;
				doExtraCriteriaRatioIndexDrop = true;
				setup(); // steps 000-049
				didAddAltRVP = true;
			}

			doGeneralFuelRatio(); // steps 050-099

			copyGeneralFuelRatioToDioxinEmissionRate(); // step 100
			copyGeneralFuelRatioToMetalEmissionRate(); // step 101
			copyGeneralFuelRatioToMinorHapRatio(); // step 102
			copyGeneralFuelRatioToPahGasRatio(); // step 103
			copyGeneralFuelRatioToPahParticleRatio(); // step 104

			copyAirToxicsToATRatio(); // step 110

			doAirToxicsCalculations(); // steps 200-299
			doCOCalculations(); // steps 300-349
			doHCCalculations(); // step 350
			doNOxCalculations(); // step 360
			// Predictive calculations are steps 370-399

			doMTBECalculations(); // step 400
			doEvapBenzeneCalculations(); // step 410

			createATRatioIndexes();
			createCriteriaRatioIndexes();

			/**
			 * @step 800
			 * @algorithm To keep the table size small and knowing that LEFT JOINs will be used, remove any
			 * remaining generalFuelRatio entries that have the default ratio of 1.0.
			 * @output generalFuelRatio
			**/
			String sql = "delete from GeneralFuelRatio where fuelEffectRatio=1 and fuelEffectRatioGPA=1";
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 800
			 * @algorithm For international use, adjust model years for generalFuelRatio (but not generalFuelRatioExpression).
			 * @output generalFuelRatio
			 * @input modelYearMapping
			**/
			sql = "update generalFuelRatio set minModelYearID=MYRMAP(minModelYearID), maxModelYearID=MYRMAP(maxModelYearID)";
			sql = ExecutionRunSpec.theExecutionRunSpec.findAndConvertModelYearMapping(sql);
			SQLRunner.executeSQL(db,sql);

			/**
			 * @step 800
			 * @algorithm Remove overlaps in ATRatio and ATRatioNonGas, deleting any ATRatioNonGas entries
			 * that match ATRatio entries.
			 * @output ATRatioNonGas
			 * @input ATRatio
			**/
			sql = "delete from ATRatioNonGas"
					+ " where exists ("
					+ " 	select *"
					+ " 	from ATRatio"
					+ " 	inner join fuelFormulation using (fuelFormulationID)"
					+ " 	where ATRatio.polProcessID=ATRatioNonGas.polProcessID"
					+ " 	and fuelFormulation.fuelSubtypeID=ATRatioNonGas.fuelSubtypeID"
					+ " 	and minModelYearID <= round(modelYearGroupID/10000,0)"
					+ " 	and maxModelYearID >= mod(modelYearGroupID,10000)"
					+ " )";
			SQLRunner.executeSQL(db,sql);
			Logger.log(LogMessageCategory.INFO,"Removed potential overlaps between ATRatioNonGas and ATRatio");

			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,"Fuel Effects Generation failed");
		} finally {
			if(didAddAltRVP) {
				String sql = "alter table fuelFormulation drop altRVP";
				try {
					SQLRunner.executeSQL(db,sql);
				} catch(Exception e) {
					Logger.logError(e,"Unable to remove altRVP column during fuel effect generation");
				}
			}
			if(shouldCheckin) {
				DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
				db = null;
			}
		}

		Logger.log(LogMessageCategory.INFO,"FuelEffectsGenerator totalTime=" + totalTime);
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		// Don't do any cleanup.  All data created herein could be needed across multiple processes.
	}

	/**
	 * Create all the tables needed by the Total Activity Generator and purge any data left over
	 * in them from a previous run.
	 * @throws SQLException If setup cannot be completed.
	**/
	void setup() throws SQLException {
		String sql = "";

		// Setup altRVP and do E85/E10 fuel transformations
		ArrayList<FuelUsageEntry> highEthanolUsages = cloneEthanolFuelsForRegions(db); // steps 000-009

		/**
		 * @step 010
		 * @algorithm Provide a defualt altRVP value.
		 * altRVP=RVP.
		 * @output fuelFormulation
		**/
		sql = "alter table fuelFormulation add altRVP float null";
		SQLRunner.executeSQL(db,sql);
		sql = "update fuelFormulation set altRVP=RVP";
		SQLRunner.executeSQL(db,sql);
		alterHighEthanolFuelProperties(db,highEthanolUsages); // steps 020-049

		// Load allowed pollutant/processes
		sql = "select polProcessID from RunSpecPollutantProcess";
		allowedPolProcesses = DatabaseUtilities.getIntegerSet(db,sql);
		allowedProcesses.clear();
		for(Iterator<Integer> i=allowedPolProcesses.iterator();i.hasNext();) {
			int polProcessID = i.next().intValue();
			Integer processID = Integer.valueOf(polProcessID % 100);
			allowedProcesses.add(processID);
		}

		// Cache model year cutpoints
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			/**
			 * @algorithm
			 * @input modelYearCutPoints
			**/
			sql = "select cutPointName, modelYearID from modelYearCutPoints";
			query.open(db,sql);
			while(query.rs.next()) {
				String cutPointName = query.rs.getString(1);
				String modelYearID = query.rs.getString(2);
				if(cutPointName == null || cutPointName.length() <= 0 || modelYearID == null) {
					continue;
				}
				if(cutPointName.equalsIgnoreCase("HighestFuelPredictiveModelYear")) {
					try {
						highestFuelPredictiveModelYear = Integer.parseInt(modelYearID);
					} catch(Exception e) {
						Logger.logError(e,"HighestFuelPredictiveModelYear parameter is not an integer: \"" + modelYearID + "\"");
					}
				}
				modelYearCutPoints.put("##cutoff." + cutPointName + "##",modelYearID);
			}
			query.close();
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Check a process against the set of allowed processes.
	 * @param processID process to be checked
	 * @return true if the process' data should be calculated.
	**/
	boolean isProcessAllowed(int processID) {
		if(allowedProcesses.size() <= 0) {
			return true;
		}
		return allowedProcesses.contains(Integer.valueOf(processID));
	}

	/**
	 * Check a pollutant/process against the set of allowed pollutant/processes.
	 * @param polProcessID pollutant/process to be checked
	 * @return true if the pollutant/process data should be calculated.
	**/
	boolean isPolProcessAllowed(int polProcessID) {
		if(allowedPolProcesses.size() <= 0) {
			return true;
		}
		return allowedPolProcesses.contains(Integer.valueOf(polProcessID));
	}

	/**
	 * Load allowed model years, filling allowedModelYears.
	 * @throws SQLException if anything goes wrong
	**/
	void loadAllowedModelYears() throws SQLException {
		allowedModelYears.clear();
		String sql = "select modelYearID from RunSpecModelYear";
		allowedModelYears = DatabaseUtilities.getIntegerSet(db,sql);
	}

	public static class ModelYearRange {
		int modelYearGroupID;
		int minModelYearID;
		int maxModelYearID;

		public ModelYearRange(int modelYearGroupIDToUse,
				int minModelYearIDToUse, int maxModelYearIDToUse) {
			modelYearGroupID = modelYearGroupIDToUse;
			minModelYearID = minModelYearIDToUse;
			maxModelYearID = maxModelYearIDToUse;
		}

		public String toString() {
			return "" + modelYearGroupID + " (" + minModelYearID + " to " + maxModelYearID + ")";
		}
	}

	/**
	 * Restrict a range of model years to those needed by the runspec.
	 * @param modelYearGroupID model year group
	 * @return a revised year range or null if there are no acceptable years
	**/
	ModelYearRange restrictToAllowedModelYears(int modelYearGroupID) {
		int minModelYearID = modelYearGroupID / 10000;
		int maxModelYearID = modelYearGroupID % 10000;
		if(minModelYearID < 1950) {
			minModelYearID = 1950;
		}
		if(maxModelYearID < 1950 || maxModelYearID > 2060) {
			maxModelYearID = 2060;
		}

		ModelYearRange range = new ModelYearRange(modelYearGroupID, minModelYearID, maxModelYearID);
		for(;range.minModelYearID<=range.maxModelYearID;range.minModelYearID++) {
			if(allowedModelYears.contains(Integer.valueOf(range.minModelYearID))) {
				break;
			}
		}
		if(range.minModelYearID > range.maxModelYearID) {
			/*
			Logger.log(LogMessageCategory.INFO,"restrictToAllowedModelYears found nothing for group "
					+ modelYearGroupID
					+ " using model years "
					+ getCSV(allowedModelYears));
			*/
			return null;
		}
		for(;range.maxModelYearID>=range.minModelYearID;range.maxModelYearID--) {
			if(allowedModelYears.contains(Integer.valueOf(range.maxModelYearID))) {
				break;
			}
		}
		if(range.minModelYearID > range.maxModelYearID) {
			/*
			Logger.log(LogMessageCategory.INFO,"restrictToAllowedModelYears found nothing for group "
					+ modelYearGroupID
					+ " using model years "
					+ getCSV(allowedModelYears));
			*/
			return null;
		}
		//Logger.log(LogMessageCategory.INFO,range.toString());
		return range;
	}

	/**
	 * Get the set of restricted model years
	 * @param modelYearGroupIDs model year groups
	 * @return list of range objects, never null but may be empty
	**/
	ArrayList<ModelYearRange> getModelYearRanges(TreeSet<Integer> modelYearGroupIDs) {
		ArrayList<ModelYearRange> ranges = new ArrayList<ModelYearRange>();
		for(Iterator<Integer> i=modelYearGroupIDs.iterator();i.hasNext();) {
			int modelYearGroupID = i.next().intValue();
			ModelYearRange range = restrictToAllowedModelYears(modelYearGroupID);
			if(range != null) {
				Logger.log(LogMessageCategory.INFO,range.toString());
				ranges.add(range);
			}
		}
		return ranges;
	}

	/**
	 * Load the list of fuel models that apply to the current calculation engine.
	 * Fills fuelModelIDs and fuelModelIDsCSV.
	 * @throws SQLException if anything goes wrong
	**/
	void loadFuelModelIDs() throws SQLException {
		fuelModelIDs.clear();
		fuelModelIDsCSV = "";

		String sql = "select fuelModelID from fuelModelName"
				+ " where calculationEngines like '%|" + calculationEngine + "|%'"
				+ " order by fuelModelID";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				int id = query.rs.getInt(1);
				if(fuelModelIDsCSV.length() > 0) {
					fuelModelIDsCSV += ",";
				}
				fuelModelIDsCSV += id;
				fuelModelIDs.add(Integer.valueOf(id));
			}
		} finally {
			query.onFinally();
			if(fuelModelIDsCSV.length() <= 0) {
				fuelModelIDsCSV = "0";
			}
		}
	}

	/**
	 * Fills fuelParameters from the database.
	 * @throws SQLException if anything goes wrong
	**/
	void loadFuelParameters() throws SQLException {
		loadParameters("select fuelParameterID, fuelParameterName, fuelParameterExpression"
				+ " from fuelParameterName", fuelParameters);
	}

	/**
	 * Fills cmParameters from the database.
	 * @throws SQLException if anything goes wrong
	**/
	void loadComplexModelParameters() throws SQLException {
		loadParameters("select cmpID, cmpName, cmpExpression"
				+ " from complexModelParameterName", cmParameters);
	}

	/**
	 * Internal routine called to load fuel and complex model parameters.
	 * @param sql SQL to get ID, name, and expression for the parameters
	 * @param parameters collection of Parameters to be filled
	**/
	void loadParameters(String sql, TreeMap<Integer,Parameter> parameters) throws SQLException {
		parameters.clear();
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				Parameter p = new Parameter(query.rs.getInt(1),
						query.rs.getString(2),query.rs.getString(3));
				parameters.put(Integer.valueOf(p.id),p);
			}
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Creates the "fp_base." and "fp_target." expressions from fuel parameters.
	 * @param holder container to the expressions being generated
	 * @throws Exception if an expression cannot be compiled
	**/
	void makeFuelParameterExpressions(ExpressionHolder holder) throws Exception {
		for(Iterator<Integer> i=fuelParameters.keySet().iterator();i.hasNext();) {
			Parameter p = fuelParameters.get(i.next());
			holder.compile("fp_base."+p.name,p.expression,
					new String[] { "ff.", "ff_base.", "fp.", "fp_base."});
			holder.compile("fp_target."+p.name,p.expression,
					new String[] { "ff.", "ff_target.", "fp.", "fp_target."});

			String nameBase = "fp_"+p.name;
			holder.set(nameBase+".base",0.0);
			holder.set(nameBase+".center",0.0);
			holder.set(nameBase+".stddev",1.0);
		}
	}

	/**
	 * Make the atDifferenceFraction and all requried intermediate expressions, adding them
	 * to a passed ExpressionHolder.
	 * @param holder container to the expressions being generated
	 * @throws Exception if an expression cannot be compiled
	**/
	void makeAtDifferenceFraction(ExpressionHolder holder) throws Exception {
		/*
		for each fuel model,
			for each cmp parameter, compile its expression with
				"ff."-->"ff_base.", "fp."--> "fp_base.", "cmp."-->"fm_#.cmp_#."
					name with "fm_#.cmp_base_#"
					create "fm_#.base_sum" as sum of these named items
				"ff."-->"ff_target.", "fp."--> "fp_target.", "cmp."-->"fm_#.cmp_#."
					name with "fm_#.cmp_target_#"
					create "fm_#.target_sum" as sum of these named items

			named expressions
				fm_#.base_exp = if(fm_#.base_sum=0,0,exp(fm_#.base_sum))
				fm_#.target_exp = if(fm_#.target_sum=0,0,exp(fm_#.target_sum))
				fm_#.ratio1 = (fm_#.target_exp/fm_#.base_exp)-1.0
				fm_#.ratio2 = fm_#.ratio1 * fm_#.weight

		atDifferenceFraction = fm_#.ratio2 + ... for each fm_#
		*/
		//ExpressionParser parser = new ExpressionParser();

		String atDifferenceFractionExpression = "";
		Set<Integer> cmpIDs = cmParameters.keySet();
		for(Iterator<Integer> fmi=fuelModelIDs.iterator();fmi.hasNext();) {
			Integer fuelModelID = fmi.next();
			String baseSum = "";
			String targetSum = "";
			for(Iterator<Integer> cmpi=cmpIDs.iterator();cmpi.hasNext();) {
				Parameter p = cmParameters.get(cmpi.next());

				String name = "fm_"+fuelModelID+".cmp_base_"+p.id;
				holder.compile(name,p.expression,
						new String[] { "ff.", "ff_base.", "fp.", "fp_base.",
							"cmp.", "fm_"+fuelModelID+".cmp_"+p.id+"."
						});
				if(baseSum.length() > 0) {
					baseSum += "+";
				}
				baseSum += name;

				name = "fm_"+fuelModelID+".cmp_target_"+p.id;
				holder.compile(name,p.expression,
						new String[] { "ff.", "ff_target.", "fp.", "fp_target.",
							"cmp.", "fm_"+fuelModelID+".cmp_"+p.id+"."
						});
				if(targetSum.length() > 0) {
					targetSum += "+";
				}
				targetSum += name;
			}
			holder.compile("fm_"+fuelModelID+".base_sum",baseSum);
			holder.compile("fm_"+fuelModelID+".target_sum",targetSum);

			// fm_#.base_exp = if(fm_#.base_sum=0,0,exp(fm_#.base_sum))
			holder.compile("fm_"+fuelModelID+".base_exp",
					"if(fm_"+fuelModelID+".base_sum=0,0,exp(fm_"+fuelModelID+".base_sum))");

			// fm_#.target_exp = if(fm_#.target_sum=0,0,exp(fm_#.target_sum))
			holder.compile("fm_"+fuelModelID+".target_exp",
					"if(fm_"+fuelModelID+".target_sum=0,0,exp(fm_"+fuelModelID+".target_sum))");

			// fm_#.ratio1 = (fm_#.target_exp/fm_#.base_exp)-1.0
			holder.compile("fm_"+fuelModelID+".ratio1",
					"(fm_"+fuelModelID+".target_exp/fm_"+fuelModelID+".base_exp)-1");

			// fm_#.ratio2 = fm_#.ratio1 * fm_#.weight
			holder.compile("fm_"+fuelModelID+".ratio2",
					"fm_"+fuelModelID+".ratio1*fm_"+fuelModelID+".weight");

			// atDifferenceFraction = fm_#.ratio2 + ... for each fm_#
			if(atDifferenceFractionExpression.length() > 0) {
				atDifferenceFractionExpression += "+";
			}
			atDifferenceFractionExpression += "fm_"+fuelModelID+".ratio2";
		}

		holder.compile("atDifferenceFraction",atDifferenceFractionExpression);
	}

	/**
	 * Make the ratioNoSulfur and all requried intermediate expressions, adding them
	 * to a passed ExpressionHolder.
	 * @param holder container to the expressions being generated
	 * @throws Exception if an expression cannot be compiled
	**/
	void makeRatioNoSulfur(ExpressionHolder holder) throws Exception {
		/*
		for each fuel model,
			for each cmp parameter, compile its expression with
				"ff."-->"ff_base.", "fp."--> "fp_base.", "cmp."-->"fm_#.cmp_#."
					name with "fm_#.cmp_base_#"
					create "fm_#.base_sum" as sum of these named items
				"ff."-->"ff_target.", "fp."--> "fp_target.", "cmp."-->"fm_#.cmp_#."
					name with "fm_#.cmp_target_#"
					create "fm_#.target_sum" as sum of these named items

			named expressions
				//fm_#.base_exp = if(fm_#.base_sum=0,0,exp(fm_#.base_sum))
				//fm_#.target_exp = if(fm_#.target_sum=0,0,exp(fm_#.target_sum))

				fm_#.base_exp = exp(fm_#.base_sum)
				fm_#.target_exp = exp(fm_#.target_sum)

				fm_#.weighted_base_exp = fm_#.weight * fm_#.base_exp
				fm_#.weighted_target_exp = fm_#.weight * fm_#.target_exp

		ratioNoSulfurExpression = (fm_#.weighted_target_exp + ... for each fm_#)
				/ (fm_#.weighted_base_exp + ... for each fm_#)
		*/

		String sumBaseWeightedExpression = "";
		String sumTargetWeightedExpression = "";

		Set<Integer> cmpIDs = cmParameters.keySet();
		for(Iterator<Integer> fmi=fuelModelIDs.iterator();fmi.hasNext();) {
			Integer fuelModelID = fmi.next();
			String baseSum = "";
			String targetSum = "";
			for(Iterator<Integer> cmpi=cmpIDs.iterator();cmpi.hasNext();) {
				Parameter p = cmParameters.get(cmpi.next());

				String name = "fm_"+fuelModelID+".cmp_base_"+p.id;
				holder.compile(name,p.expression,
						new String[] { "ff.", "ff_base.", "fp.", "fp_base.",
							"cmp.", "fm_"+fuelModelID+".cmp_"+p.id+"."
						});
				if(baseSum.length() > 0) {
					baseSum += "+";
				}
				baseSum += name;

				name = "fm_"+fuelModelID+".cmp_target_"+p.id;
				holder.compile(name,p.expression,
						new String[] { "ff.", "ff_target.", "fp.", "fp_target.",
							"cmp.", "fm_"+fuelModelID+".cmp_"+p.id+"."
						});
				if(targetSum.length() > 0) {
					targetSum += "+";
				}
				targetSum += name;
			}
			holder.compile("fm_"+fuelModelID+".base_sum",baseSum);
			holder.compile("fm_"+fuelModelID+".target_sum",targetSum);

			// fm_#.base_exp = if(fm_#.base_sum=0,0,exp(fm_#.base_sum))
			//holder.compile("fm_"+fuelModelID+".base_exp",
			//		"if(fm_"+fuelModelID+".base_sum=0,0,exp(fm_"+fuelModelID+".base_sum))");
			holder.compile("fm_"+fuelModelID+".base_exp","exp(fm_"+fuelModelID+".base_sum)");

			// fm_#.target_exp = if(fm_#.target_sum=0,0,exp(fm_#.target_sum))
			//holder.compile("fm_"+fuelModelID+".target_exp",
			//		"if(fm_"+fuelModelID+".target_sum=0,0,exp(fm_"+fuelModelID+".target_sum))");
			holder.compile("fm_"+fuelModelID+".target_exp","exp(fm_"+fuelModelID+".target_sum)");

			// fm_#.weighted_base_exp = fm_#.weight * fm_#.base_exp
			holder.compile("fm_"+fuelModelID+".weighted_base_exp",
					"fm_"+fuelModelID+".base_exp*fm_"+fuelModelID+".weight");

			// fm_#.weighted_target_exp = fm_#.weight * fm_#.target_exp
			holder.compile("fm_"+fuelModelID+".weighted_target_exp",
					"fm_"+fuelModelID+".target_exp*fm_"+fuelModelID+".weight");

			if(sumBaseWeightedExpression.length() > 0) {
				sumBaseWeightedExpression += "+";
			}
			sumBaseWeightedExpression += "fm_"+fuelModelID+".weighted_base_exp";

			if(sumTargetWeightedExpression.length() > 0) {
				sumTargetWeightedExpression += "+";
			}
			sumTargetWeightedExpression += "fm_"+fuelModelID+".weighted_target_exp";
		}

		holder.compile("ratioNoSulfurTarget",sumTargetWeightedExpression);
		holder.compile("ratioNoSulfurBase",sumBaseWeightedExpression);
		holder.compile("ratioNoSulfur","ratioNoSulfurTarget/ratioNoSulfurBase");
	}

	/**
	 * Create coefficients and summary expressions for each combination of fuel model and
	 * complex model parameter.  The placeholders are merely created, but filled.
	 * @param holder container to the expressions being generated
	 * @throws Exception if an expression cannot be compiled
	**/
	void createComplexModelParameterVariables(ExpressionHolder holder) throws Exception {
		createComplexModelParameterVariables(holder,3);
	}

	/**
	 * Create coefficients and summary expressions for each combination of fuel model and
	 * complex model parameter.  The placeholders are merely created, but filled.
	 * @param holder container to the expressions being generated
	 * @param coefficientCount number of coefficients to be summed
	 * @throws Exception if an expression cannot be compiled
	**/
	void createComplexModelParameterVariables(ExpressionHolder holder, int coefficientCount)
			throws Exception {
		Set<Integer> cmpIDs = cmParameters.keySet();
		for(Iterator<Integer> fmi=fuelModelIDs.iterator();fmi.hasNext();) {
			Integer fuelModelID = fmi.next();
			for(Iterator<Integer> cmpi=cmpIDs.iterator();cmpi.hasNext();) {
				Parameter p = cmParameters.get(cmpi.next());
				String nameBase = "fm_"+fuelModelID+".cmp_"+p.id;
				holder.variables.put(nameBase+".coeff1",new Value(0.0));
				holder.variables.put(nameBase+".coeff2",new Value(0.0));
				holder.variables.put(nameBase+".coeff3",new Value(0.0));
				String sumText = nameBase+".coeff1";
				if(coefficientCount > 1) {
					sumText += "+"+nameBase+".coeff2";
				}
				if(coefficientCount > 2) {
					sumText += "+"+nameBase+".coeff3";
				}
				holder.compile(nameBase+".coeff",sumText);
			}
		}
	}

	/**
	 * Set coefficients for each combination of fuel model and complex model parameter to 0.
	 * @param holder container to the expressions being updated
	 * @throws Exception if anything goes wrong
	**/
	void clearComplexModelParameterVariables(ExpressionHolder holder) throws Exception {
		Set<Integer> cmpIDs = cmParameters.keySet();
		for(Iterator<Integer> fmi=fuelModelIDs.iterator();fmi.hasNext();) {
			Integer fuelModelID = fmi.next();
			for(Iterator<Integer> cmpi=cmpIDs.iterator();cmpi.hasNext();) {
				Parameter p = cmParameters.get(cmpi.next());
				String nameBase = "fm_"+fuelModelID+".cmp_"+p.id;
				holder.set(nameBase+".coeff1",0.0);
				holder.set(nameBase+".coeff2",0.0);
				holder.set(nameBase+".coeff3",0.0);
			}
		}
	}

	/**
	 * Set the coefficients for the complex model parameters filtered by polProcessID.
	 * @param holder container to the expressions being updated
	 * @param polProcessID filter value for the complexModelParameters table
	 * @throws Exception if anything goes wrong
	**/
	void setComplexModelParameterVariables(ExpressionHolder holder, int polProcessID)
			throws Exception {
		String sql = "select fuelModelID, cmpID, coeff1, coeff2, coeff3"
				+ " from complexModelParameters"
				+ " where polProcessID=" + polProcessID
				+ " and fuelModelID in (" + fuelModelIDsCSV + ")";
		SQLRunner.Query query = new SQLRunner.Query();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				int fuelModelID = query.rs.getInt(1);
				int cmpID = query.rs.getInt(2);
				String nameBase = "fm_"+fuelModelID+".cmp_"+cmpID;
				holder.set(nameBase+".coeff1",(double)query.rs.getFloat(3));
				holder.set(nameBase+".coeff2",(double)query.rs.getFloat(4));
				holder.set(nameBase+".coeff3",(double)query.rs.getFloat(5));
			}
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Set the meanFuelParameters table variables filtered by polProcessID, fuelTypeID, and
	 * modelYearGroupID.
	 * @param holder container to the expressions being updated
	 * @param polProcessID filter value for the meanFuelParameters table
	 * @param fuelTypeID filter value for the meanFuelParameters table
	 * @param modelYearGroupID filter value for the meanFuelParameters table
	 * @throws Exception if anything goes wrong
	**/
	void setMeanFuelParameterVariables(ExpressionHolder holder, int polProcessID, int fuelTypeID,
			int modelYearGroupID) throws Exception {
		TreeSet<Integer> loadedIDs = new TreeSet<Integer>();
		String[] statements = {
			"select fuelParameterID, baseValue, centeringValue, stdDevValue"
					+ " from meanFuelParameters"
					+ " where polProcessID=" + polProcessID
					+ " and fuelTypeID=" + fuelTypeID
					+ " and modelYearGroupID=" + modelYearGroupID,
			"select fuelParameterID, baseValue, centeringValue, stdDevValue"
					+ " from meanFuelParameters"
					+ " where polProcessID=" + polProcessID
					+ " and fuelTypeID=" + fuelTypeID
					+ " and modelYearGroupID=0"
		};
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		boolean found = false;
		try {
			for(int si=0;si<statements.length;si++) {
				sql = statements[si];
				found = false;
				query.open(db,sql);
				while(query.rs.next()) {
					found = true;
					int fuelParameterID = query.rs.getInt(1);
					Integer id = Integer.valueOf(fuelParameterID);
					Parameter p = fuelParameters.get(id);
					if(p == null) {
						continue;
					}
					loadedIDs.add(id);
					String nameBase = "fp_"+p.name;
					holder.set(nameBase+".base",(double)query.rs.getFloat(2));
					holder.set(nameBase+".center",(double)query.rs.getFloat(3));
					double s = (double)query.rs.getFloat(4);
					if(s <= 0) {
						s = 1.0;
					}
					holder.set(nameBase+".stddev",s);
				}
				query.close();
				if(found) {
					break;
				}
			}
		} finally {
			query.onFinally();
		}
		Set<Integer> ids = fuelParameters.keySet();
		for(Iterator<Integer> i=ids.iterator();i.hasNext();) {
			Integer tid = i.next();
			if(loadedIDs.contains(tid)) {
				continue;
			}
			Parameter p = fuelParameters.get(tid);
			String nameBase = "fp_"+p.name;
			holder.set(nameBase+".base",0.0);
			holder.set(nameBase+".center",0.0);
			holder.set(nameBase+".stddev",1.0);
		}
		if(!found) {
			/*
			Logger.log(LogMessageCategory.INFO,"Did not find meanFuelParameters entries for"
					+ " polProcessID=" + polProcessID
					+ ", fuelTypeID=" + fuelTypeID
					+ " modelYearGroupID=" + modelYearGroupID);
			*/
		}
	}

	/**
	 * Set all fuel model weights to 0
	 * @param holder container to the expressions being updated
	 * @throws Exception if anything goes wrong
	**/
	void clearFuelModelWtFactorVariables(ExpressionHolder holder) throws Exception {
		holder.set("minModelYear",0);
		holder.set("maxModelYear",0);

		for(Iterator<Integer> fmi=fuelModelIDs.iterator();fmi.hasNext();) {
			Integer fuelModelID = fmi.next();
			holder.set("fm_"+fuelModelID+".weight",0.0);
		}
	}

	/**
	 * Set the fuelModelWtFactor table variables filtered by modelYearGroupID and ageID.
	 * @param holder container to the expressions being updated
	 * @param modelYearGroupID filter value for the fuelModelWtFactor table
	 * @param ageID filter value for the fuelModelWtFactor table
	 * @param minModelYearID minimum model year in the group
	 * @param maxModelyearID maximum model year in the group
	 * @throws Exception if anything goes wrong
	**/
	void setFuelModelWtFactorVariables(ExpressionHolder holder, int modelYearGroupID,
			int ageID, int minModelYearID, int maxModelYearID) throws Exception {
		holder.set("minModelYear",minModelYearID);
		holder.set("maxModelYear",maxModelYearID);

		String[] statements = {
			"select fuelModelID, fuelModelWtFactor"
					+ " from fuelModelWtFactor"
					+ " where modelYearGroupID=" + modelYearGroupID
					+ " and ageID=" + ageID
					+ " and fuelModelID in (" + fuelModelIDsCSV + ")",
			"select fuelModelID, fuelModelWtFactor"
					+ " from fuelModelWtFactor"
					+ " where modelYearGroupID=0"
					+ " and ageID=" + ageID
					+ " and fuelModelID in (" + fuelModelIDsCSV + ")"
		};
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		boolean found = false;
		try {
			for(int si=0;si<statements.length;si++) {
				found = false;
				sql = statements[si];
				query.open(db,sql);
				while(query.rs.next()) {
					found = true;
					int fuelModelID = query.rs.getInt(1);
					double weight = query.rs.getFloat(2);
					holder.set("fm_"+fuelModelID+".weight",weight);
				}
				query.close();
				if(found) {
					break;
				}
			}
		} finally {
			query.onFinally();
		}
		if(!found) {
			/*
			Logger.log(LogMessageCategory.INFO,"Did not find fuelModelWtFactor entries for"
					+ " modelYearGroupID=" + modelYearGroupID
					+ ", ageID=" + ageID);
			*/
		}
	}

	/**
	 * Get the set of fuel types that are to be processed.
	 * @return the set of fuel types that are to be processed
	 * @throws SQLException if anything goes wrong
	**/
	TreeSet<Integer> getFuelTypeIDs() throws SQLException {
		return getFuelTypeIDs(null);
	}

	/**
	 * Get the set of fuel types that are to be processed.
	 * @param restrictedPolProcessIDs optional comma-separated list of polProcessIDs
	 * used to restrict the output.  May be empty or null.
	 * @return the set of fuel types that are to be processed
	 * @throws SQLException if anything goes wrong
	**/
	TreeSet<Integer> getFuelTypeIDs(String restrictedPolProcessIDs) throws SQLException {
		String sql = "select distinct fuelTypeID from meanFuelParameters";
		if(restrictedPolProcessIDs != null && restrictedPolProcessIDs.length() > 0) {
			sql += " where polProcessID in (" + restrictedPolProcessIDs + ")";
		}
		return DatabaseUtilities.getIntegerSet(db,sql);
	}

	/**
	 * Get the base fuel formulation for the current calculation engine and given fuel type.
	 * @param fuelTypeID fuel type
	 * @return the base fuel formulation ID or 0 if none exists
	 * @throws SQLException if anything goes wrong
	**/
	int getBaseFormulation(int fuelTypeID) throws SQLException {
		String sql = "select fuelFormulationID"
				+ " from baseFuel"
				+ " where calculationEngine='" + calculationEngine + "'"
				+ " and fuelTypeID=" + fuelTypeID;
		return (int)SQLRunner.executeScalar(db,sql);
	}

	/**
	 * Get the base fuel formulation for the current calculation engine, given fuel type,
	 * and model year group.
	 * @param fuelTypeID fuel type
	 * @param modelYearGroupID model year group ID
	 * @return the base fuel formulation ID or 0 if none exists
	 * @throws SQLException if anything goes wrong
	**/
	int getBaseFormulation(int fuelTypeID, int modelYearGroupID) throws SQLException {
		String cacheKey = calculationEngine + "|" + fuelTypeID + "|" + modelYearGroupID;
		Integer cacheID = baseFormulationsCache.get(cacheKey);
		if(cacheID != null) {
			return cacheID.intValue();
		}
		String[] statements = {
			"select fuelFormulationID"
					+ " from baseFuel"
					+ " where calculationEngine='" + calculationEngine + "'"
					+ " and fuelTypeID=" + fuelTypeID
					+ " and modelYearGroupID=" + modelYearGroupID,
			"select fuelFormulationID"
					+ " from baseFuel"
					+ " where calculationEngine='" + calculationEngine + "'"
					+ " and fuelTypeID=" + fuelTypeID
					+ " and modelYearGroupID=0"
		};
		String sql = "";
		int result = 0;
		for(int si=0;si<statements.length;si++) {
			sql = statements[si];
			result = (int)SQLRunner.executeScalar(db,sql);
			if(result > 0) {
				break;
			}
		}
		baseFormulationsCache.put(cacheKey,Integer.valueOf(result));
		return result;
	}

	/**
	 * Get a set of all fuel formulations that exist for a given fuel type
	 * @param fuelTypeID fuel type to be queried
	 * @return set of all fuel formulations for fuelTypeID, never null but may be empty
	 * @throws SQLException if anything goes wrong
	**/
	TreeSet<Integer> getFuelFormulations(int fuelTypeID) throws SQLException {
		if(fuelHandler != null) {
			return fuelHandler.getFuelFormulations(fuelTypeID);
		}
		String sql = "select fuelFormulationID"
				+ " from fuelSubtype fst"
				+ " inner join fuelFormulation ff on ff.fuelSubtypeID=fst.fuelSubtypeID"
				+ " where fst.fuelTypeID=" + fuelTypeID;
		return DatabaseUtilities.getIntegerSet(db,sql);
	}

	/**
	 * Get a set of all fuel formulations that are made available through the FuelSupply table.
	 * This information is useful when narrowing fuel calculations to only those required.
	 * @param fuelTypeID fuel type to be queried
	 * @return set of fuel formulations, never null but may be empty
	 * @throws SQLException if anything goes wrong
	**/
	TreeSet<Integer> getFuelSupplyFormulations(int fuelTypeID) throws SQLException {
		if(fuelHandler != null) {
			return fuelHandler.getFuelFormulations(fuelTypeID);
		}
		String sql = "select distinct ff.fuelFormulationID"
				+ " from fuelSubtype fst"
				+ " inner join fuelFormulation ff on ff.fuelSubtypeID=fst.fuelSubtypeID"
				+ " inner join fuelSupply fs on fs.fuelFormulationID=ff.fuelFormulationID"
				+ " inner join year y on y.fuelYearID=fs.fuelYearID"
				+ " inner join runSpecYear rsy on rsy.yearID=y.yearID"
				+ " where fst.fuelTypeID=" + fuelTypeID
				+ " and marketShare > 0";
		return DatabaseUtilities.getIntegerSet(db,sql);
	}

	/**
	 * Get the set of polProcessIDs that apply to the current fuel models
	 * @param restrictedPolProcessIDs optional comma-separated list of polProcessIDs
	 * used to restrict the output.  May be empty or null.
	 * @return polProcessIDs, never null but may be empty
	 * @throws SQLException if anything goes wrong
	**/
	TreeSet<Integer> getPolProcessIDs(String restrictedPolProcessIDs) throws SQLException {
		String sql = "select distinct polProcessID"
				+ " from complexModelParameters"
				+ " where fuelModelID in (" + fuelModelIDsCSV + ")";
		if(restrictedPolProcessIDs != null && restrictedPolProcessIDs.length() > 0) {
			sql += " and polProcessID in (" + restrictedPolProcessIDs + ")";
		}
		return DatabaseUtilities.getIntegerSet(db,sql);
	}

	/**
	 * Get the set of modelYearGroupIDs that apply to all fuel parameters.
	 * @param restrictedPolProcessIDs optional comma-separated list of polProcessIDs
	 * used to restrict the output.  May be empty or null.
	 * @return modelYearGroupIDs, never null but may be empty
	 * @throws SQLException if anything goes wrong
	**/
	TreeSet<Integer> getModelYearGroupIDs(String restrictedPolProcessIDs) throws SQLException {
		String sql = "select distinct modelYearGroupID from meanFuelParameters";
		if(restrictedPolProcessIDs != null && restrictedPolProcessIDs.length() > 0) {
			sql += " where polProcessID in (" + restrictedPolProcessIDs + ")";
		}
		// get the model year groups, but try again in fuelModelWtFactor if the only model
		// year group found is the generic 0.
		TreeSet<Integer> results = DatabaseUtilities.getIntegerSet(db,sql);
		if(results.size() > 1) {
//Logger.log(LogMessageCategory.INFO,"Found#1 " + results.size() + " modelYearGroupIDs using " + sql);
			return results;
		}
		if(results.size() > 0 && !results.contains(Integer.valueOf(0))) {
//Logger.log(LogMessageCategory.INFO,"Found#2 " + results.size() + " modelYearGroupIDs using " + sql);
			return results;
		}
//Logger.log(LogMessageCategory.INFO,"Did not find modelYearGroupIDs using " + sql);
		sql = "select distinct modelYearGroupID"
				+ " from fuelModelWtFactor"
				+ " where fuelModelID in (" + fuelModelIDsCSV + ")";
		results = DatabaseUtilities.getIntegerSet(db,sql);
//Logger.log(LogMessageCategory.INFO,"Found#3 " + results.size() + " modelYearGroupIDs using " + sql);
		return results;
	}

	/**
	 * Get the set of ageIDs.
	 * @return ageIDs, never null but may be empty
	 * @throws SQLException if anything goes wrong
	**/
	TreeSet<Integer> getAgeIDs() throws SQLException {
		String sql = "select distinct ageID from fuelModelWtFactor";
		return DatabaseUtilities.getIntegerSet(db,sql);
	}

	/**
	 * Create variables with the data from a fuel formulation.
	 * @param holder holder of data to be loaded
	 * @param formulationID fuel formulation to be read
	 * @param namePrefix prefix, if any, including a separator dot ('.') to use on the
	 * created variables.  Such prefixes should be "ff_target." or "ff_base."
	 * @throws Exception if anything goes wrong
	**/
	void loadFuelFormulation(ExpressionHolder holder, int formulationID, String namePrefix)
			throws Exception {
		if(fuelFormulations.size() == 0) {
			String sql = "select * from fuelFormulation";
			SQLRunner.Query query = new SQLRunner.Query();
			try {
				query.open(db,sql);
				while(query.rs.next()) {
					FuelFormulation f = new FuelFormulation();
					f.fromResultSet(query.rs);
					fuelFormulations.put(Integer.valueOf(f.fuelFormulationID),f);
				}
			} finally {
				query.onFinally();
			}
		}
		FuelFormulation f = fuelFormulations.get(Integer.valueOf(formulationID));
		if(f == null) {
			return;
		}
		if(namePrefix == null || namePrefix.length() <= 0) {
			namePrefix = "ff.";
		}
		holder.set(namePrefix+"RVP",f.RVP);
		holder.set(namePrefix+"sulfurLevel",f.sulfurLevel);
		holder.set(namePrefix+"ETOHVolume",f.ETOHVolume);
		holder.set(namePrefix+"MTBEVolume",f.MTBEVolume);
		holder.set(namePrefix+"ETBEVolume",f.ETBEVolume);
		holder.set(namePrefix+"TAMEVolume",f.TAMEVolume);
		holder.set(namePrefix+"aromaticContent",f.aromaticContent);
		holder.set(namePrefix+"olefinContent",f.olefinContent);
		holder.set(namePrefix+"benzeneContent",f.benzeneContent);
		holder.set(namePrefix+"e200",f.e200);
		holder.set(namePrefix+"e300",f.e300);
		holder.set(namePrefix+"volToWtPercentOxy",f.volToWtPercentOxy);
		holder.set(namePrefix+"bioDieselEsterVolume",f.bioDieselEsterVolume);
		holder.set(namePrefix+"cetaneIndex",f.cetaneIndex);
		holder.set(namePrefix+"PAHContent",f.PAHContent);
		holder.set(namePrefix+"T50",f.t50);
		holder.set(namePrefix+"T90",f.t90);
		holder.set(namePrefix+"altRVP",f.altRVP);
	}

	/**
	 * Change any NULL-valued columns into 0-valued columns in the fuelFormulation table.
	 * @throws SQLException if anything goes wrong
	**/
	public void changeFuelFormulationNulls() throws SQLException {
		String[] columnNames = { "RVP","sulfurLevel","ETOHVolume","MTBEVolume","ETBEVolume",
				"TAMEVolume","aromaticContent","olefinContent","benzeneContent","e200","e300",
				"volToWtPercentOxy","BioDieselEsterVolume","CetaneIndex","PAHContent","T50","T90"
		};
		String sql = "update fuelFormulation set ";
		for(int i=0;i<columnNames.length;i++) {
			if(i > 0) {
				sql += ",";
			}
			sql += columnNames[i] + "=ifnull(" + columnNames[i] + ",0)";
		}
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Performs air toxics calculations for the "airtoxicsA" calculation engine.
	 * @throws Exception if anything goes wrong
	**/
	void doAirToxicsCalculations() throws Exception {
Logger.log(LogMessageCategory.INFO,"doAirToxicsCalculations");
		long startMillis, endMillis;
		changeFuelFormulationNulls();
		loadAllowedModelYears();

		/**
		 * @step 290
		 * @algorithm Lookup all Complex Model expression fragments from the database and combine them
		 * into large symbolic statements in memory. Variables within will be populated with actual
		 * values and the algebra simplified in later steps.
		 * The Complex Model uses a collection of fuel models, each referencing any fuel formulation properties
		 * and summing the subexpressions into a single number.
		 * Each fuel model is applied to a fuel formulation and to a base fuel formulation, creating
		 * intermediate varibles called target_sum and base_sum respectively.
		 * Each fuel model calculates a ratio as: ratio = (if(target_sum=0,0,exp(target_sum))/if(base_sum=0,0,exp(base_sum))) - 1.
		 * The Complex Model atDifferenceFraction = sum(fuel model ratio * fuel model weight) summed for each fuel model.
		 * Base emission rates, atBaseEmissions, are used to calculate the final atRatio which is the ratio of a pollutant's
		 * emissions to VOC emissions for a fuel formulation.
		 * atRatio = (atBaseEmissions of nonVOC *(1.0+atDifferenceFraction of nonVOC)) / (atBaseEmissions of VOC *(1.0+atDifferenceFraction of VOC)).
		 * @output Complex Model expressions
		 * @input fuelModelName
		 * @input fuelParameterName
		 * @input complexModelParameterName
		 * @condition Air Toxics pollutants
		**/
		calculationEngine = "airtoxicsA";
		loadFuelModelIDs();
		loadFuelParameters();
		loadComplexModelParameters();
		ExpressionHolder holder = new ExpressionHolder();
		makeFuelParameterExpressions(holder);
		makeAtDifferenceFraction(holder);
		createComplexModelParameterVariables(holder);

		String[] setupStatements = {
			"drop table if exists tempAirToxicsA",
			"create table if not exists tempAirToxicsA ("
					+ " fuelTypeID int not null,"
					+ " fuelFormulationID int not null,"
					+ " polProcessID int not null,"
					+ " pollutantID int not null,"
					+ " processID int not null,"
					+ " modelYearGroupID int not null,"
					+ " minModelYearID int not null,"
					+ " maxModelYearID int not null,"
					+ " ageID int not null,"
					+ " atDifferenceFraction double null"
					//+ ", key (fuelFormulationID), key(polProcessID),"
					//+ " key (pollutantID), key(processID),"
					//+ " key(modelYearGroupID), key(ageID)"
					+ " )",

			"drop table if exists tempAirToxicsAVOC",
			"create table if not exists tempAirToxicsAVOC ("
					//+ " fuelTypeID int not null,"
					+ " fuelFormulationID int not null,"
					//+ " polProcessID int not null,"
					+ " processID int not null,"
					+ " modelYearGroupID int not null,"
					//+ " minModelYearID int not null,"
					//+ " maxModelYearID int not null,"
					+ " ageID int not null,"
					+ " monthGroupID int not null,"
					+ " relATEmissionsVOC double null"
					//+ ", key (processID, fuelFormulationID, modelYearGroupID, ageID, monthGroupID)"
					//+ ", key (fuelFormulationID, modelYearGroupID, ageID, monthGroupID, relATEmissionsVOC)" // spanning index
					+ " )",

			"drop table if exists tempAirToxicsANonVOC",
			"create table if not exists tempAirToxicsANonVOC ("
					+ " fuelTypeID int not null,"
					+ " fuelFormulationID int not null,"
					+ " polProcessID int not null,"
					+ " pollutantID int not null,"
					+ " processID int not null,"
					+ " modelYearGroupID int not null,"
					+ " minModelYearID int not null,"
					+ " maxModelYearID int not null,"
					+ " ageID int not null,"
					+ " monthGroupID int not null,"
					+ " relATEmissions double null"
					//+ ", key (polProcessID)"
					//+ ", key (fuelFormulationID, modelYearGroupID, ageID, monthGroupID)"
					+ " )"
		};
		String sql = "";
		for(int i=0;i<setupStatements.length;i++) {
			sql = setupStatements[i];
			SQLRunner.executeSQL(db,sql);
		}

		boolean didDropATRatioIndexes = false;
		SQLRunner.Query query = new SQLRunner.Query();
		/**
		 * @step 290
		 * @algorithm Restrict the Complex Model in the next steps to these polProcessIDs: 2001,2002,2090,2401,2402,2490,2501,2502,2590,2601,2602,2690,8701,8702,8790
		**/
		String polProcessIDsLimitCSV = "2001,2002,2090,2401,2402,2490,2501,2502,2590,2601,2602,2690,8701,8702,8790";
		TreeSet<Integer> fuelTypeIDs = getFuelTypeIDs();
		TreeSet<Integer> ageIDs = getAgeIDs();
		TreeSet<Integer> modelYearGroupIDs = getModelYearGroupIDs(polProcessIDsLimitCSV);
		ArrayList<ModelYearRange> modelYearRanges = getModelYearRanges(modelYearGroupIDs);
		TreeSet<IntegerPair> fuelFormulationsRatioed = getIntegerPairSet(db,
				"select distinct fuelFormulationID, polProcessID from atRatio");

		TreeSet<Integer> candidatePolProcessIDs = getPolProcessIDs(polProcessIDsLimitCSV);
		TreeSet<Integer> processIDs = new TreeSet<Integer>();
		TreeSet<Integer> polProcessIDs = new TreeSet<Integer>();
		for(Iterator<Integer> ppi=candidatePolProcessIDs.iterator();ppi.hasNext();) {
			Integer iObject = ppi.next();
			int polProcessID = iObject.intValue();
			if(!isPolProcessAllowed(polProcessID)) {
				continue;
			}
			polProcessIDs.add(iObject);
			Integer processID = Integer.valueOf(polProcessID % 100);
			if(!processIDs.contains(processID)) {
				processIDs.add(processID);
			}
		}

		for(Iterator<Integer> fti=fuelTypeIDs.iterator();fti.hasNext();) {
			int fuelTypeID = fti.next().intValue();
			TreeSet<Integer> fuelFormulationIDs = getFuelFormulations(fuelTypeID);
			TreeSet<Integer> fuelFormulationsSupplied = getFuelSupplyFormulations(fuelTypeID);

			TreeSet<Integer> fuelFormulationsToUse = new TreeSet<Integer>();
			for(Iterator<Integer> ffi=fuelFormulationIDs.iterator();ffi.hasNext();) {
				Integer id = ffi.next();
				if(!fuelFormulationsSupplied.contains(id)) {
					continue;
				}
				int targetFormulationID = id.intValue();
				if(fuelFormulationsToUse.size() <= 0) {
					// Load the first formulation to ensure the ff_target.* variables
					// exist.
					loadFuelFormulation(holder,targetFormulationID,"ff_target.");
				}
				fuelFormulationsToUse.add(id);
			}
			if(fuelFormulationsToUse.size() <= 0) {
				Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + " because it is not used in the fuel supply");
				continue;
			}
			Logger.log(LogMessageCategory.INFO,"FuelTypeID " + fuelTypeID + " has " + fuelFormulationsToUse.size() + " formulations");

			for(Iterator<Integer> pi=processIDs.iterator();pi.hasNext();) {
				int currentProcessID = pi.next().intValue();
				sql = "truncate tempAirToxicsAVOC";
				SQLRunner.executeSQL(db,sql);
				sql = "truncate tempAirToxicsANonVOC";
				SQLRunner.executeSQL(db,sql);

				// Fill tempAirToxicsAVOC and tempAirToxicsANonVOC, filling VOC first
				for(int pollutantPhase=0;pollutantPhase<2;pollutantPhase++) {
					// pollutantPhase=0 is for VOCs which must be done first
					// pollutantPhase=1 is for non-VOCs which are then ratioed to VOC values

					for(Iterator<Integer> ppi=polProcessIDs.iterator();ppi.hasNext();) {
						Integer polProcessIDObject = ppi.next();
						int polProcessID = polProcessIDObject.intValue();
						int pollutantID = polProcessID / 100;
						int processID = polProcessID % 100;
						if(processID != currentProcessID) {
							continue;
						}
						if((pollutantPhase == 0 && pollutantID != 87)
								|| (pollutantPhase == 1 && pollutantID == 87)) { // 87 == VOC
							continue;
						}

						String formulationIDsCSV = "";
						for(Iterator<Integer> ffi=fuelFormulationsToUse.iterator();ffi.hasNext();) {
							Integer fuelFormulationID = ffi.next();
							if(pollutantPhase == 1 && contains(fuelFormulationsRatioed,fuelFormulationID,polProcessIDObject)) {
//								continue;
							}
							if(formulationIDsCSV.length() > 0) {
								formulationIDsCSV += ",";
							}
							formulationIDsCSV += fuelFormulationID;
						}
						if(formulationIDsCSV.length() <= 0) {
							Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + ", polProcessID " + polProcessIDObject + " due to existing ATRatio information");
							continue;
						}

						sql = "truncate tempAirToxicsA";
						SQLRunner.executeSQL(db,sql);

						/**
						 * @step 290
						 * @algorithm Lookup and apply Complex Model parameter values.
						 * @input complexModelParameters
						 * @output Complex Model expressions
						**/
						clearComplexModelParameterVariables(holder);
						setComplexModelParameterVariables(holder,polProcessID);

						for(Iterator<ModelYearRange> myri=modelYearRanges.iterator();myri.hasNext();) {
							ModelYearRange range = myri.next();
							int modelYearGroupID = range.modelYearGroupID;
							int minModelYearID = range.minModelYearID;
							int maxModelYearID = range.maxModelYearID;
							int baseFormulationID = getBaseFormulation(fuelTypeID, modelYearGroupID);
							if(baseFormulationID <= 0) {
								continue;
							}

							// Restrict model year range if there are existing entries for any of the fuel formulations.
							// These existing entries are most likely from GeneralFuelRatioExpression
							int existingMinModelYearID = 0;
							int existingMaxModelYearID = 0;
							sql = "select min(minModelYearID), max(maxModelYearID)"
									+ " from ATRatio"
									+ " where polProcessID=" + polProcessID
									+ " and fuelFormulationID in (" + formulationIDsCSV + ")";
							try {
								query.open(db,sql);
								if(query.rs.next()) {
									existingMinModelYearID = query.rs.getInt(1);
									existingMaxModelYearID = query.rs.getInt(2);
								}
							} finally {
								query.onFinally();
							}
							if(existingMinModelYearID > 0 && existingMaxModelYearID > 0) {
								// If there is any overlap with minModelYearID and maxModelYearID, trim them.
								if(existingMinModelYearID == minModelYearID && existingMaxModelYearID == maxModelYearID) {
									//System.out.println("Case 1, e: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
									continue;
								} else if(existingMinModelYearID < minModelYearID && existingMaxModelYearID > maxModelYearID) {
									//System.out.println("Case 2, e: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
									continue;
								} else if(existingMinModelYearID > minModelYearID && existingMaxModelYearID < maxModelYearID) {
									//System.out.println("Case 3, e: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
									continue;
								} else if(existingMaxModelYearID < minModelYearID) {
									//System.out.println("Case 4, e: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
									// Nothing to do
								} else if(existingMinModelYearID > maxModelYearID) {
									//System.out.println("Case 5, e: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
									// Nothing to do
								} else {
									if(existingMinModelYearID <= maxModelYearID) {
										//System.out.println("Case 6a, e: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
										maxModelYearID = existingMinModelYearID - 1;
									} else if(existingMaxModelYearID >= minModelYearID) {
										//System.out.println("Case 6b, e: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
										minModelYearID = existingMaxModelYearID + 1;
									}
									//System.out.println("Case 6z, e: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
								}
								if(minModelYearID > maxModelYearID) {
									//System.out.println("Case 1, 6: " + existingMinModelYearID + " - " + existingMaxModelYearID + ", m: " + minModelYearID + " - " + maxModelYearID);
									continue;
								}
							}

							/**
							 * @step 290
							 * @algorithm Lookup fuel properties of the base fuel and apply them to the Complex Model expressions.
							 * @input baseFuel
							 * @input fuelFormulation
							 * @output Complex Model expressions
							**/
							loadFuelFormulation(holder,baseFormulationID,"ff_base.");

							/**
							 * @step 290
							 * @algorithm Lookup mean fuel parameters and apply them to the Complex Model expressions.
							 * @input meanFuelParameters
							 * @output Complex Model expressions
							**/
							setMeanFuelParameterVariables(holder,polProcessID,fuelTypeID,modelYearGroupID);

							for(Iterator<Integer> ai=ageIDs.iterator();ai.hasNext();) {
								int ageID = ai.next().intValue();
								/**
								 * @step 290
								 * @algorithm Lookup fuel model weight factors and apply them to the Complex Model expressions.
								 * @input fuelModelWtFactor
								 * @output Complex Model expressions
								**/
								clearFuelModelWtFactorVariables(holder);
								setFuelModelWtFactorVariables(holder,modelYearGroupID,ageID,minModelYearID,maxModelYearID);

								/**
								 * @step 290
								 * @algorithm Mathematically simplify the Complex Model expressions in memory. Perform
								 * all possible arithmatic in memory, reducing the size and complexity of the resulting
								 * expression text.
								 * @output Complex Model expressions
								**/
								Optimizer optimizer = new Optimizer(holder,
										new String[] { "ff_target." },null);
								IExpressionNode node = holder.getExpression("atDifferenceFraction").deepCopy();
								node = optimizer.optimize(node);
								String expressionText = node.getExpressionText(holder);
/*
if(ageID == 0) {
	Logger.log(LogMessageCategory.INFO,"atDifferenceFraction="+expressionText);
	break;
}
*/
								// Make a table of fuelFormulationID, polProcessID, modelYearGroupID,
								// ageID, atDifferenceFraction
								
								/**
								 * @step 290
								 * @algorithm atDifferenceFraction = Complex Model expressions
								 * @output tempAirToxicsA
								 * @input fuelFormulation
								 * @input Complex Model expressions
								**/
								sql = "insert into tempAirToxicsA (fuelTypeID, fuelFormulationID,"
										+ " polProcessID, pollutantID, processID,"
										+ " modelYearGroupID, minModelYearID, maxModelYearID, ageID, "
										+ " atDifferenceFraction)"
										+ " select " + fuelTypeID + ", fuelFormulationID"
										+ "," + polProcessID + "," + pollutantID + "," + processID
										+ "," + modelYearGroupID + "," + minModelYearID
										+ "," + maxModelYearID + "," + ageID
										+ ",(" + expressionText + ") as atDifferenceFraction"
										+ " from fuelFormulation as ff_target"
										+ " where fuelFormulationID in (" + formulationIDsCSV + ")";
								//Logger.log(LogMessageCategory.INFO,"sql=" + sql);
								SQLRunner.executeSQL(db,sql);
							} // end of ageID loop
						} // enf of modelYearGroupID loop

						sql = "alter table tempAirToxicsA add key idx1 (polProcessID)";
						SQLRunner.executeSQL(db,sql);

						// Standardize entries that have no effect

						/**
						 * @step 290
						 * @algorithm Provide default atDifference entries. atDifference = 0 when null.
						 * @output tempAirToxicsA
						**/
						sql = "update tempAirToxicsA set atDifferenceFraction=0 where atDifferenceFraction is null";
						SQLRunner.executeSQL(db,sql);

						if(pollutantPhase == 0) { // If doing VOCs
							// Create RelATEmissions = ATBaseEmissions.atBaseEmissions*(1+atDifferenceFraction)
							//sql = "insert into tempAirToxicsAVOC (polProcessID, processID,"

							/**
							 * @step 290
							 * @algorithm relATEmissionsVOC = atBaseEmissions*(1.0+atDifferenceFraction)).
							 * @output tempAirToxicsANonVOC
							 * @input atBaseEmissions
							 * @input tempAirToxicsA
							 * @condition VOC
							**/
							sql = "insert into tempAirToxicsAVOC (processID,"
									+ " 	fuelFormulationID,"
									+ " 	modelYearGroupID, ageID,"
									+ " 	monthGroupID,"
									+ " 	relATEmissionsVOC)"
									//+ " select b.polProcessID, t.processID, "
									+ " select t.processID, "
									+ " 	t.fuelFormulationID,"
									+ " 	t.modelYearGroupID, t.ageID,"
									+ " 	b.monthGroupID,"
									+ " 	(atBaseEmissions*(1.0+atDifferenceFraction)) as relATEmissionsVOC"
									+ " from atBaseEmissions b"
									+ " inner join tempAirToxicsA t on t.polProcessID=b.polProcessID";
									//+ " where pollutantID=87"; filtering is already done via pollutantPhase
							SQLRunner.executeSQL(db,sql);

							//sql = "analyze table tempAirToxicsAVOC";
							//SQLRunner.executeSQL(db,sql);
						} else { // else doing non-VOCs
							/**
							 * @step 290
							 * @algorithm relATEmissions = atBaseEmissions*(1.0+atDifferenceFraction)).
							 * @output tempAirToxicsANonVOC
							 * @input atBaseEmissions
							 * @input tempAirToxicsA
							 * @condition Non-VOC pollutants
							**/
							sql = "insert into tempAirToxicsANonVOC (polProcessID, pollutantID, processID,"
									+ " 	fuelTypeID, fuelFormulationID,"
									+ " 	modelYearGroupID, minModelYearID, maxModelYearID, ageID,"
									+ " 	monthGroupID,"
									+ " 	relATEmissions)"
									+ " select b.polProcessID, t.pollutantID, t.processID, "
									+ " 	t.fuelTypeID, t.fuelFormulationID,"
									+ " 	t.modelYearGroupID, t.minModelYearID, t.maxModelYearID, t.ageID,"
									+ " 	b.monthGroupID,"
									+ " 	(atBaseEmissions*(1.0+atDifferenceFraction)) as relATEmissions"
									+ " from atBaseEmissions b"
									+ " inner join tempAirToxicsA t on t.polProcessID=b.polProcessID";
									//+ " where pollutantID<>87"; filtering is already done via pollutantPhase
							SQLRunner.executeSQL(db,sql);

							//sql = "analyze table tempAirToxicsANonVOC";
							//SQLRunner.executeSQL(db,sql);
						}

						sql = "alter table tempAirToxicsA drop index idx1";
						SQLRunner.executeSQL(db,sql);
					} // end polProcess
				} // end of pollutantPhase

				sql = "alter table tempAirToxicsAVOC"
						+ " add key idx3 (fuelFormulationID, modelYearGroupID, ageID, monthGroupID)";
				SQLRunner.executeSQL(db,sql);

				sql = "analyze table tempAirToxicsAVOC";
				SQLRunner.executeSQL(db,sql);

				sql = "alter table tempAirToxicsANonVOC"
						+ " add key idx4 (fuelFormulationID, modelYearGroupID, ageID, monthGroupID)";
				SQLRunner.executeSQL(db,sql);

				sql = "analyze table tempAirToxicsANonVOC";
				SQLRunner.executeSQL(db,sql);

				if(!didDropATRatioIndexes) {
					didDropATRatioIndexes = true;
					dropATRatioIndexes();
				}
				// Ratio non-VOC to VOC.  This is the output table from the AirToxics algorithm.

				/**
				 * @step 290
				 * @algorithm Ratio non-VOC to VOC relative air toxics complex model emissions.
				 * atRatio = relATEmissions / relATEmissionsVOC, where relATEmissionsVOC <> 0, 0 otherwise.
				 * @input tempAirToxicsANonVOC
				 * @input tempAirToxicsAVOC
				 * @output ATRatio
				**/
				sql = "insert into ATRatio (fuelTypeID, fuelFormulationID, polProcessID, "
						+ " 	minModelYearID, maxModelYearID, ageID, monthGroupID, atRatio)"
						+ " select b.fuelTypeID, b.fuelFormulationID, b.polProcessID, "
						+ " 	b.minModelYearID, b.maxModelYearID, b.ageID, b.monthGroupID,"
						+ " 	(case when relATEmissionsVOC <> 0 then (relATEmissions/relATEmissionsVOC) else 0 end) as atRatio"
						+ " from tempAirToxicsANonVOC as b"
						+ " inner join tempAirToxicsAVOC v on ("
						//+ " 	v.processID=b.processID and " the processes are always the same due to process looping logic
						+ " 	v.fuelFormulationID=b.fuelFormulationID"
						+ " 	and v.modelYearGroupID=b.modelYearGroupID and v.ageID=b.ageID"
						+ " 	and v.monthGroupID=b.monthGroupID"
						+ " )";
				SQLRunner.executeSQL(db,sql);

				sql = "alter table tempAirToxicsAVOC drop index idx3";
				SQLRunner.executeSQL(db,sql);

				sql = "alter table tempAirToxicsANonVOC drop index idx4";
				SQLRunner.executeSQL(db,sql);
			} // end pi iteration of process IDs
		} // end fuelTypeIDs iteration

		//Indexes Added back to ATRatio
		if(didDropATRatioIndexes) {
			createATRatioIndexes();
		}
	}

	/**
	 * Tracks drops and creates of indexes on the ATRatio table.  When 0 or larger, indexes
	 * should be on the table.  When less than 0, there should be no indexes.
	**/
	private int atRatioCounter = 0;
	/**
	 * When true, after the first drop of ATRatio indexes, the drop counter is decremented a
	 * second time.  This simplifies flagging in executeLoop.
	**/
	private boolean doExtraATRatioIndexDrop = false;

	/**
	 * Create indexes on the ATRatio table.
	 * @throws Exception if anything goes wrong
	**/
	void createATRatioIndexes() throws Exception {
		if(doExtraATRatioIndexDrop) {
			return;
		}
		atRatioCounter++;
		if(atRatioCounter < 0) {
			return;
		}
		Logger.log(LogMessageCategory.INFO,"Creating ATRatio indexes");
		String[] statements = {
			"alter table ATRatio add key atratio_key1 (fuelFormulationID, polProcessID, minModelYearID)",
			"alter table ATRatio add key atratio_key2 (polProcessID, fuelTypeID, monthGroupID, minModelYearID, ageID, maxModelYearID, fuelFormulationID)"
		};
		long startMillis, endMillis;
		for(int i=0;i<statements.length;i++) {
			startMillis = System.currentTimeMillis();
			String sql = statements[i];
			SQLRunner.executeSQL(db,sql);
			endMillis = System.currentTimeMillis();
//Logger.log(LogMessageCategory.INFO,"" + (endMillis-startMillis) + " ms for " + sql);
		}
	}

	/**
	 * Drop indexes on the ATRatio table.
	**/
	void dropATRatioIndexes() {
		atRatioCounter--;
		if(atRatioCounter < -1) {
			return;
		}
		// Here upon transition from 0 to -1, i.e. the first drop
		String[] statements = {
			"alter table ATRatio drop index atratio_key1",
			"alter table ATRatio drop index atratio_key2"
		};
		Logger.log(LogMessageCategory.INFO,"Dropping ATRatio indexes");
		for(int i=0;i<statements.length;i++) {
			String sql = statements[i];
			try {
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				// Nothing to do here
			}
		}
		if(doExtraATRatioIndexDrop) {
			doExtraATRatioIndexDrop = false;
			atRatioCounter--;
		}
	}

	/**
	 * Tracks drops and creates of indexes on the criteriaRatio table.  When 0 or larger, indexes
	 * should be on the table.  When less than 0, there should be no indexes.
	**/
	private int criteriaRatioCounter = 0;
	/**
	 * When true, after the first drop of criteriaRatio indexes, the drop counter is decremented a
	 * second time.  This simplifies flagging in executeLoop.
	**/
	private boolean doExtraCriteriaRatioIndexDrop = false;

	/**
	 * Create indexes on the criteriaRatio table.
	 * @throws Exception if anything goes wrong
	**/
	void createCriteriaRatioIndexes() throws Exception {
		if(doExtraCriteriaRatioIndexDrop) {
			return;
		}
		criteriaRatioCounter++;
		if(criteriaRatioCounter < 0) {
			return;
		}
		Logger.log(LogMessageCategory.INFO,"Creating criteriaRatio indexes");
		String[] statements = {
			"alter table criteriaRatio add key crFuelFormulation (polProcessID, fuelFormulationID)",
			"alter table criteriaRatio add key crCommon (polProcessID, modelYearID, ageID)"
		};
		long startMillis, endMillis;
		for(int i=0;i<statements.length;i++) {
			startMillis = System.currentTimeMillis();
			String sql = statements[i];
			SQLRunner.executeSQL(db,sql);
			endMillis = System.currentTimeMillis();
//Logger.log(LogMessageCategory.INFO,"" + (endMillis-startMillis) + " ms for " + sql);
		}
	}

	/**
	 * Drop indexes on the criteriaRatio table.
	**/
	void dropCriteriaRatioIndexes() {
		criteriaRatioCounter--;
		if(criteriaRatioCounter < -1) {
			return;
		}
		// Here upon transition from 0 to -1, i.e. the first drop
		String[] statements = {
			"alter table criteriaRatio drop index crFuelFormulation",
			"alter table criteriaRatio drop index crCommon"
		};
		Logger.log(LogMessageCategory.INFO,"Dropping criteriaRatio indexes");
		for(int i=0;i<statements.length;i++) {
			String sql = statements[i];
			try {
				SQLRunner.executeSQL(db,sql);
			} catch(Exception e) {
				// Nothing to do here
			}
		}
		if(doExtraCriteriaRatioIndexDrop) {
			doExtraCriteriaRatioIndexDrop = false;
			criteriaRatioCounter--;
		}
	}

	/**
	 * Performs Carbon Monoxide calculations for the "co" calculation engine.
	 * @throws Exception if anything goes wrong
	**/
	void doCOCalculations() throws Exception {
Logger.log(LogMessageCategory.INFO,"doCOCalculations");
		long startMillis, endMillis;
		changeFuelFormulationNulls();
		loadAllowedModelYears();

		/**
		 * @step 300
		 * @algorithm Lookup all Complex Model expression fragments from the database and combine them
		 * into large symbolic statements in memory. Variables within will be populated with actual
		 * values and the algebra simplified in later steps.
		 * The Complex Model uses a collection of fuel models, each referencing any fuel formulation properties
		 * and summing the subexpressions into a single number.
		 * Each fuel model is applied to a fuel formulation and to a base fuel formulation, creating
		 * intermediate varibles called target_sum and base_sum respectively.
		 * Each fuel model calculates a ratio as: ratio = (if(target_sum=0,0,exp(target_sum))/if(base_sum=0,0,exp(base_sum))) - 1.
		 * The Complex Model atDifferenceFraction = sum(fuel model ratio * fuel model weight) summed for each fuel model.
		 * CO ratioNoSulfur = 1 + atDifferenceFraction.
		 * CO criteriaRatio.ratio = Sulfur Model of CO ratioNoSulfer.
		 * @output Complex Model expressions
		 * @input fuelModelName
		 * @input fuelParameterName
		 * @input complexModelParameterName
		 * @condition CO pollutant
		**/
		calculationEngine = "co";
		loadFuelModelIDs();
		loadFuelParameters();
		loadComplexModelParameters();
		ExpressionHolder holder = new ExpressionHolder();
		makeFuelParameterExpressions(holder);
		makeAtDifferenceFraction(holder);
		createComplexModelParameterVariables(holder);

		String[] setupStatements = {
			"drop table if exists tempCOA",
			"create table if not exists tempCOA ("
					+ " fuelTypeID int not null,"
					+ " fuelFormulationID int not null,"
					+ " baseFuelFormulationID int not null,"
					+ " polProcessID int not null,"
					+ " pollutantID int not null,"
					+ " processID int not null,"
					+ " modelYearGroupID int not null,"
					+ " minModelYearID int not null,"
					+ " maxModelYearID int not null,"
					+ " ageID int not null,"
					+ " ratioNoSulfur double null"
					//+ ", key (fuelFormulationID), key(polProcessID),"
					//+ " key (pollutantID), key(processID),"
					//+ " key(modelYearGroupID), key(ageID)"
					+ " )"
		};
		String sql = "";
		for(int i=0;i<setupStatements.length;i++) {
			sql = setupStatements[i];
			SQLRunner.executeSQL(db,sql);
		}

		boolean didDropRatioIndexes = false;
		boolean didInsert = false;

		/**
		 * @step 300
		 * @algorithm Restrict the Complex Model in the next steps to these polProcessIDs: 201,202
		**/
		String polProcessIDsLimitCSV = "201,202";
		/**
		 * @step 300
		 * @algorithm Copy criteriaRatio entries given by expressions, deleting the entries from generalFuelRatio after the copy to criteriaRatio.
		 * ratio=fuelEffectRatio.
		 * ratioNoSulfer=1.
		 * @input generalFuelRatio
		 * @output generalFuelRatio
		 * @output criteriaRatio
		 * @condition Any of these polProcessIDs: 201,202
		**/
		copyGeneralFuelRatioToCriteriaRatio(polProcessIDsLimitCSV);

		TreeSet<Integer> fuelTypeIDs = getFuelTypeIDs();
		TreeSet<Integer> ageIDs = getAgeIDs();
		TreeSet<Integer> modelYearGroupIDs = getModelYearGroupIDs(polProcessIDsLimitCSV);
		ArrayList<ModelYearRange> modelYearRanges = getModelYearRanges(modelYearGroupIDs);
		TreeSet<Integer> candidatePolProcessIDs = getPolProcessIDs(polProcessIDsLimitCSV);
		TreeSet<IntegerPair> fuelFormulationsRatioed = getIntegerPairSet(db,
				"select distinct fuelFormulationID, polProcessID"
				+ " from criteriaRatio"
				+ " where polProcessID in (" + polProcessIDsLimitCSV + ")");
		TreeMapIgnoreCase aliases = new TreeMapIgnoreCase();
		aliases.put("ff_target.sulfurLevel","ff_base.sulfurLevel");

		TreeSet<Integer> processIDs = new TreeSet<Integer>();
		TreeSet<Integer> polProcessIDs = new TreeSet<Integer>();
		for(Iterator<Integer> ppi=candidatePolProcessIDs.iterator();ppi.hasNext();) {
			Integer iObject = ppi.next();
			int polProcessID = iObject.intValue();
			if(!isPolProcessAllowed(polProcessID)) {
				continue;
			}
			polProcessIDs.add(iObject);
			Integer processID = Integer.valueOf(polProcessID % 100);
			if(!processIDs.contains(processID)) {
				processIDs.add(processID);
			}
		}

		for(Iterator<Integer> fti=fuelTypeIDs.iterator();fti.hasNext();) {
			int fuelTypeID = fti.next().intValue();
			TreeSet<Integer> fuelFormulationIDs = getFuelFormulations(fuelTypeID);
			TreeSet<Integer> fuelFormulationsSupplied = getFuelSupplyFormulations(fuelTypeID);

			TreeSet<Integer> fuelFormulationsToUse = new TreeSet<Integer>();
			for(Iterator<Integer> ffi=fuelFormulationIDs.iterator();ffi.hasNext();) {
				Integer id = ffi.next();
				if(!fuelFormulationsSupplied.contains(id)) {
					continue;
				}
				if(fuelFormulationsToUse.size() <= 0) {
					// Load the first formulation to ensure the ff_target.* variables
					// exist.
					loadFuelFormulation(holder,id.intValue(),"ff_target.");
				}
				fuelFormulationsToUse.add(id);
			}
			if(fuelFormulationsToUse.size() <= 0) {
				Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + " because it is not used in the fuel supply");
				continue;
			}
			Logger.log(LogMessageCategory.INFO,"FuelTypeID " + fuelTypeID + " has " + fuelFormulationsToUse.size() + " formulations");

			for(Iterator<Integer> pi=processIDs.iterator();pi.hasNext();) {
				int currentProcessID = pi.next().intValue();
				for(Iterator<Integer> ppi=polProcessIDs.iterator();ppi.hasNext();) {
					Integer polProcessIDObject = ppi.next();
					int polProcessID = polProcessIDObject.intValue();
					int pollutantID = polProcessID / 100;
					int processID = polProcessID % 100;
					if(processID != currentProcessID) {
						continue;
					}

					String formulationIDsCSV = "";
					for(Iterator<Integer> ffi=fuelFormulationsToUse.iterator();ffi.hasNext();) {
						Integer fuelFormulationID = ffi.next();
						if(contains(fuelFormulationsRatioed,fuelFormulationID,polProcessIDObject)) {
//							continue;
						}
						if(formulationIDsCSV.length() > 0) {
							formulationIDsCSV += ",";
						}
						formulationIDsCSV += fuelFormulationID;
					}
					if(formulationIDsCSV.length() <= 0) {
						Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + ", polProcessID " + polProcessIDObject + " due to existing coRatio information");
						continue;
					}

					/**
					 * @step 300
					 * @algorithm Lookup and apply Complex Model parameter values.
					 * @input complexModelParameters
					 * @output Complex Model expressions
					**/
					clearComplexModelParameterVariables(holder);
					setComplexModelParameterVariables(holder,polProcessID);

					for(Iterator<ModelYearRange> myri=modelYearRanges.iterator();myri.hasNext();) {
						ModelYearRange range = myri.next();
						int modelYearGroupID = range.modelYearGroupID;
						int minModelYearID = range.minModelYearID;
						int maxModelYearID = range.maxModelYearID;
						int baseFormulationID = getBaseFormulation(fuelTypeID, modelYearGroupID);
						if(baseFormulationID <= 0) {
							continue;
						}
						/**
						 * @step 300
						 * @algorithm Lookup fuel properties of the base fuel and apply them to the Complex Model expressions.
						 * @input baseFuel
						 * @input fuelFormulation
						 * @output Complex Model expressions
						**/
						loadFuelFormulation(holder,baseFormulationID,"ff_base.");

						/**
						 * @step 300
						 * @algorithm Lookup mean fuel parameters and apply them to the Complex Model expressions.
						 * @input meanFuelParameters
						 * @output Complex Model expressions
						**/
						setMeanFuelParameterVariables(holder,polProcessID,fuelTypeID,modelYearGroupID);

						sql = "truncate tempCOA";
						SQLRunner.executeSQL(db,sql);

						for(Iterator<Integer> ai=ageIDs.iterator();ai.hasNext();) {
							int ageID = ai.next().intValue();
							/**
							 * @step 300
							 * @algorithm Lookup fuel model weight factors and apply them to the Complex Model expressions.
							 * @input fuelModelWtFactor
							 * @output Complex Model expressions
							**/
							clearFuelModelWtFactorVariables(holder);
							setFuelModelWtFactorVariables(holder,modelYearGroupID,ageID,minModelYearID,maxModelYearID);

							/**
							 * @step 300
							 * @algorithm Mathematically simplify the Complex Model expressions in memory. Perform
							 * all possible arithmatic in memory, reducing the size and complexity of the resulting
							 * expression text.
							 * @output Complex Model expressions
							**/
							Optimizer optimizer = new Optimizer(holder,
									new String[] { "ff_target." },aliases);
							IExpressionNode node = holder.getExpression("atDifferenceFraction").deepCopy();
							node = optimizer.optimize(node);
							String expressionText = node.getExpressionText(holder);
							if(expressionText.toLowerCase().indexOf("ff_target.sulfurlevel")>=0) {
								throw new Exception(
										"CO expression should not reference ff_target.sulfurLevel: "
										+ expressionText);
							}
							// Make a table of fuelFormulationID, polProcessID, modelYearGroupID,
							// ageID, atDifferenceFraction

							/**
							 * @step 300
							 * @algorithm ratioNoSulfur = 1 + (Complex Model expressions).
							 * @output tempCOA
							 * @input fuelFormulation
							 * @input Complex Model expressions
							**/
							sql = "insert into tempCOA (fuelTypeID, fuelFormulationID,"
									+ " baseFuelFormulationID,"
									+ " polProcessID, pollutantID, processID,"
									+ " modelYearGroupID, minModelYearID, maxModelYearID, ageID, "
									+ " ratioNoSulfur)"
									+ " select " + fuelTypeID + ", fuelFormulationID"
									+ "," + baseFormulationID
									+ "," + polProcessID + "," + pollutantID + "," + processID
									+ "," + modelYearGroupID + "," + minModelYearID
									+ "," + maxModelYearID + "," + ageID
									+ ",1+(" + expressionText + ") as ratioNoSulfur"
									+ " from fuelFormulation as ff_target"
									+ " where fuelFormulationID in (" + formulationIDsCSV
									+ "," + baseFormulationID + ")";
							//Logger.log(LogMessageCategory.INFO,"sql=" + sql);
							SQLRunner.executeSQL(db,sql);
/*
sql = "insert into tempCOA (fuelTypeID, fuelFormulationID,"
		+ " baseFuelFormulationID,"
		+ " polProcessID, pollutantID, processID,"
		+ " modelYearGroupID, minModelYearID, maxModelYearID, ageID, "
		+ " ratioNoSulfur)\n"
		+ " select " + fuelTypeID + ", fuelFormulationID"
		+ "," + baseFormulationID
		+ "," + polProcessID + "," + pollutantID + "," + processID
		+ "," + modelYearGroupID + "," + minModelYearID
		+ "," + maxModelYearID + "," + ageID
		+ ",1+(...) as ratioNoSulfur\n"
		+ " from fuelFormulation as ff_target"
		+ " where fuelFormulationID in (...)";
Logger.log(LogMessageCategory.INFO,"sql=" + sql);
*/
							didInsert = true;
						} // end of ageID loop

						if(didInsert) {
							if(!didDropRatioIndexes) {
								didDropRatioIndexes = true;
								dropCriteriaRatioIndexes();
							}

							// Standardize entries that have no effect

							/**
							 * @step 300
							 * @algorithm Provide default ratioNoSulfur entries. ratioNoSulfur = 1 when null.
							 * @output tempCOA
							**/
							sql = "update tempCOA set ratioNoSulfur=1 where ratioNoSulfur is null";
							SQLRunner.executeSQL(db,sql);

							// Build criteriaRatio by using the sulfur model on tempCOA's data
							
							/**
							 * @step 300
							 * @algorithm Run the sulfur model with tempCOA as the input and the criteriaRatio
							 * table as the output.
							 * @input tempCOA
							 * @output criteriaRatio
							**/
							runSulfurModel(db,"tempCOA","criteriaRatio",useRateOfProgress);

							didInsert = false;
						}
					} // enf of modelYearGroupID loop
				} // end polProcess
			} // end pi iteration of process IDs
		} // end fuelTypeIDs iteration

		//Indexes Added back to coRatio
		if(didDropRatioIndexes) {
			createCriteriaRatioIndexes();
		}
	}

	/**
	 * Performs Predictive Model HC calculations.
	 * @throws Exception if anything goes wrong
	**/
	void doHCCalculations() throws Exception {
		/**
		 * @step 350
		 * @algorithm Do Predictive Calculation steps for Total Gaseous Hydrocarbons.
		 * Use the "predictHC" calculation engine parameters, polProcessIDs 101 and 102, 
		 * and ethanol alternate polProcessIDs 1000101 and 1000102.
		**/
		doPredictiveCalculations("predictHC","101,102","HC","1000101,1000102");
	}

	/**
	 * Performs Predictive Model NOx calculations.
	 * @throws Exception if anything goes wrong
	**/
	void doNOxCalculations() throws Exception {
		/**
		 * @step 360
		 * @algorithm Do Predictive Calculation steps for NOx.
		 * Use the "predictNOx" calculation engine parameters, polProcessIDs 301 and 302, 
		 * and no ethanol alternate polProcessIDs.
		**/
		doPredictiveCalculations("predictNOx","301,302","NOx",null);
	}

	/**
	 * Performs Predictive calculations.
	 * @param engineName calculation engine to be performed
	 * @param polProcessIDsLimitCSV CSV list of pollutant/processes
	 * @param tableBaseName name to be used as root of the ratio and temporary tables
	 * @param altPolProcessIDsLimitsCSV CSV list of alternative pollutant/processes
	 * @throws Exception if anything goes wrong
	**/
	void doPredictiveCalculations(String engineName, String polProcessIDsLimitCSV,
			String tableBaseName, String altPolProcessIDsLimitsCSV) throws Exception {
		altPolProcessIDsLimitsCSV = StringUtilities.safeGetString(altPolProcessIDsLimitsCSV);
Logger.log(LogMessageCategory.INFO,"doPredictiveCalculations: " + engineName + ", " + polProcessIDsLimitCSV + " / " + altPolProcessIDsLimitsCSV);
		/**
		 * @step 370
		 * @algorithm Beginning of Predictive Calculations steps.
		 * @condition Predictive Calculations
		**/

		/**
		 * @step 370
		 * @algorithm Copy criteriaRatio entries given by expressions, deleting the entries from generalFuelRatio after the copy to criteriaRatio.
		 * ratio=fuelEffectRatio.
		 * ratioNoSulfer=1.
		 * @input generalFuelRatio
		 * @output generalFuelRatio
		 * @output criteriaRatio
		 * @condition Predictive Calculations
		 * @condition Any of the predictive calculation polProcessIDs
		**/
		copyGeneralFuelRatioToCriteriaRatio(polProcessIDsLimitCSV);
		if(altPolProcessIDsLimitsCSV.length() > 0) {
			/**
			 * @step 370
			 * @algorithm Copy criteriaRatio entries given by expressions, deleting the entries from generalFuelRatio after the copy to criteriaRatio.
			 * ratio=fuelEffectRatio.
			 * ratioNoSulfer=1.
			 * @input generalFuelRatio
			 * @output generalFuelRatio
			 * @output altCriteriaRatio
			 * @condition Predictive Calculations
			 * @condition Any of the predictive calculation ethanol alternate polProcessIDs
			 * @condition model years >= 2001
			**/
			copyGeneralFuelRatioToAltCriteriaRatio(altPolProcessIDsLimitsCSV);
		}

		/*
		ComplexModelParameterName
		(cmp.coeff*(fp.E200-fp_E200.center)^2)
		set cmp.coeff = ComplexModelParameters.coeff1
		Note that Air Toxics sets
			cmp.coeff = ComplexModelParameters.coeff1 + ComplexModelParameters.coeff2
		When loading ComplexModelParameterName.cmpExpression, rewrites are needed to
		include .stddev:
		change "(fp.E200-fp_E200.center)" to "((fp.E200-fp_E200.center)/fp_E200.stddev)"
		*/

		long startMillis, endMillis;
		changeFuelFormulationNulls();
		loadAllowedModelYears();

		/**
		 * @step 370
		 * @algorithm Lookup all Complex Model expression fragments from the database and combine them
		 * into large symbolic statements in memory. Variables within will be populated with actual
		 * values and the algebra simplified in later steps.
		 * The Complex Model uses a collection of fuel models, each referencing any fuel formulation properties
		 * and summing the subexpressions into a single number.
		 * Each fuel model is applied to a fuel formulation and to a base fuel formulation, creating
		 * intermediate varibles called target_sum and base_sum respectively.
		 * Each fuel model calculates a ratio as: ratio = (if(target_sum=0,0,exp(target_sum))/if(base_sum=0,0,exp(base_sum))) - 1.
		 * The Complex Model atDifferenceFraction = sum(fuel model ratio * fuel model weight) summed for each fuel model.
		 * atDifferenceFraction is not used by the Predictive Calculations.
		 * weightedTargetExp = sum(fuel model weight * exp(target_sum)) across all fuel models.
		 * weightedBaseExp = sum(fuel model weight * exp(base_sum)) across all fuel models.
		 * ratioNoSulfur = weightedTargetExp / weightedBaseExp.
		 * CriteriaRatio.ratio = Sulfur model of ratioNoSulfur.
		 * @output Complex Model expressions
		 * @input fuelModelName
		 * @input fuelParameterName
		 * @input complexModelParameterName
		 * @condition Predictive Calculations
		**/
		calculationEngine = engineName;
		loadFuelModelIDs();
		loadFuelParameters();
		loadComplexModelParameters();
		rewriteCmpExpressions();
		ExpressionHolder holder = new ExpressionHolder();
		makeFuelParameterExpressions(holder);
		createComplexModelParameterVariables(holder,1);

		makeRatioNoSulfur(holder);

		String[] setupStatements = {
			"drop table if exists temp" + tableBaseName + "A",
			"create table if not exists temp" + tableBaseName + "A ("
					+ " fuelTypeID int not null,"
					+ " fuelFormulationID int not null,"
					+ " baseFuelFormulationID int not null,"
					+ " polProcessID int not null,"
					+ " pollutantID int not null,"
					+ " processID int not null,"
					+ " modelYearGroupID int not null,"
					+ " minModelYearID int not null,"
					+ " maxModelYearID int not null,"
					+ " ageID int not null,"
					+ " ratioNoSulfur double null"
					//+ ", key (fuelFormulationID), key(polProcessID),"
					//+ " key (pollutantID), key(processID),"
					//+ " key(modelYearGroupID), key(ageID)"
					+ " )"
		};
		String sql = "";
		for(int i=0;i<setupStatements.length;i++) {
			sql = setupStatements[i];
			SQLRunner.executeSQL(db,sql);
		}

		boolean didDropRatioIndexes = false;
		boolean didInsert = false;

		TreeSet<Integer> fuelTypeIDs = getFuelTypeIDs();
		TreeSet<Integer> ageIDs = getAgeIDs();
		TreeSet<Integer> modelYearGroupIDs = getModelYearGroupIDs(polProcessIDsLimitCSV);
		ArrayList<ModelYearRange> modelYearRanges = getModelYearRanges(modelYearGroupIDs);
		TreeSet<Integer> candidatePolProcessIDs = getPolProcessIDs(polProcessIDsLimitCSV);
		TreeSet<IntegerPair> fuelFormulationsRatioed = getIntegerPairSet(db,
				"select distinct fuelFormulationID, polProcessID"
				+ " from criteriaRatio"
				+ " where polProcessID in (" + polProcessIDsLimitCSV + ")");

		TreeSet<Integer> processIDs = new TreeSet<Integer>();
		TreeSet<Integer> polProcessIDs = new TreeSet<Integer>();
		for(Iterator<Integer> ppi=candidatePolProcessIDs.iterator();ppi.hasNext();) {
			Integer iObject = ppi.next();
			int polProcessID = iObject.intValue();
			if(!isPolProcessAllowed(polProcessID)) {
				continue;
			}
			polProcessIDs.add(iObject);
			Integer processID = Integer.valueOf(polProcessID % 100);
			if(!processIDs.contains(processID)) {
				processIDs.add(processID);
			}
		}

		for(Iterator<Integer> fti=fuelTypeIDs.iterator();fti.hasNext();) {
			int fuelTypeID = fti.next().intValue();
			TreeSet<Integer> fuelFormulationIDs = getFuelFormulations(fuelTypeID);
			TreeSet<Integer> fuelFormulationsSupplied = getFuelSupplyFormulations(fuelTypeID);

			TreeSet<Integer> fuelFormulationsToUse = new TreeSet<Integer>();
			for(Iterator<Integer> ffi=fuelFormulationIDs.iterator();ffi.hasNext();) {
				Integer id = ffi.next();
				if(!fuelFormulationsSupplied.contains(id)) {
					continue;
				}
				if(fuelFormulationsToUse.size() <= 0) {
					// Load the first formulation to ensure the ff_target.* variables
					// exist.
					loadFuelFormulation(holder,id.intValue(),"ff_target.");
				}
				fuelFormulationsToUse.add(id);
			}
			if(fuelFormulationsToUse.size() <= 0) {
				Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + " because it is not used in the fuel supply");
				continue;
			}
			Logger.log(LogMessageCategory.INFO,"FuelTypeID " + fuelTypeID + " has " + fuelFormulationsToUse.size() + " formulations");

			for(Iterator<Integer> pi=processIDs.iterator();pi.hasNext();) {
				int currentProcessID = pi.next().intValue();
				for(Iterator<Integer> ppi=polProcessIDs.iterator();ppi.hasNext();) {
					Integer polProcessIDObject = ppi.next();
					int polProcessID = polProcessIDObject.intValue();
					int pollutantID = polProcessID / 100;
					int processID = polProcessID % 100;
					if(processID != currentProcessID) {
						continue;
					}

					String formulationIDsCSV = "";
					for(Iterator<Integer> ffi=fuelFormulationsToUse.iterator();ffi.hasNext();) {
						Integer fuelFormulationID = ffi.next();
						if(contains(fuelFormulationsRatioed,fuelFormulationID,polProcessIDObject)) {
//							continue;
						}
						if(formulationIDsCSV.length() > 0) {
							formulationIDsCSV += ",";
						}
						formulationIDsCSV += fuelFormulationID;
					}
					if(formulationIDsCSV.length() <= 0) {
						Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + ", polProcessID " + polProcessIDObject + " due to existing CriteriaRatio information");
						continue;
					}

					/**
					 * @step 370
					 * @algorithm Lookup and apply Complex Model parameter values.
					 * @input complexModelParameters
					 * @output Complex Model expressions
					 * @condition Predictive Calculations
					**/
					clearComplexModelParameterVariables(holder);
					setComplexModelParameterVariables(holder,polProcessID);

					for(Iterator<ModelYearRange> myri=modelYearRanges.iterator();myri.hasNext();) {
						ModelYearRange range = myri.next();
						int modelYearGroupID = range.modelYearGroupID;
						int minModelYearID = range.minModelYearID;
						int maxModelYearID = range.maxModelYearID;
						int baseFormulationID = getBaseFormulation(fuelTypeID, modelYearGroupID);
						if(baseFormulationID <= 0) {
							continue;
						}
						/**
						 * @step 370
						 * @algorithm Lookup fuel properties of the base fuel and apply them to the Complex Model expressions.
						 * @input baseFuel
						 * @input fuelFormulation
						 * @output Complex Model expressions
						 * @condition Predictive Calculations
						**/
						loadFuelFormulation(holder,baseFormulationID,"ff_base.");
						/**
						 * @step 370
						 * @algorithm Lookup mean fuel parameters and apply them to the Complex Model expressions.
						 * @input meanFuelParameters
						 * @output Complex Model expressions
						 * @condition Predictive Calculations
						**/
						setMeanFuelParameterVariables(holder,polProcessID,fuelTypeID,modelYearGroupID);

						for(Iterator<Integer> ai=ageIDs.iterator();ai.hasNext();) {
							int ageID = ai.next().intValue();
							/**
							 * @step 370
							 * @algorithm Lookup fuel model weight factors and apply them to the Complex Model expressions.
							 * @input fuelModelWtFactor
							 * @output Complex Model expressions
							 * @condition Predictive Calculations
							**/
							clearFuelModelWtFactorVariables(holder);
							setFuelModelWtFactorVariables(holder,modelYearGroupID,ageID,minModelYearID,maxModelYearID);

							/**
							 * @step 370
							 * @algorithm Mathematically simplify the Complex Model expressions in memory. Perform
							 * all possible arithmatic in memory, reducing the size and complexity of the resulting
							 * expression text.
							 * @output Complex Model expressions
							 * @condition Predictive Calculations
							**/
							Optimizer optimizer = new Optimizer(holder,
									new String[] { "ff_target." },null);
							IExpressionNode node = holder.getExpression("ratioNoSulfur").deepCopy();
							try {
								node = optimizer.optimize(node);
							} catch(Exception e) {
								Logger.log(LogMessageCategory.INFO,node.getExpressionText(holder));
								throw e;
							}
							String expressionText = node.getExpressionText(holder);
//Logger.log(LogMessageCategory.INFO,expressionText + " from " + targetText + "/" + baseText);
/*
Logger.log(LogMessageCategory.INFO,"mygp=" + modelYearGroupID + ",ppid=" + polProcessID + ": " + targetText + "/" + baseText);
if(ageID == 0) {
//	Logger.log(LogMessageCategory.INFO,"base="+holder.getExpression("ratioNoSulfurBase").getExpressionText(holder));
//	holder.printAll();
	break;
}
*/
							/**
							 * @step 370
							 * @algorithm ratioNoSulfur = Complex Model expressions
							 * @output temporary table
							 * @input fuelFormulation
							 * @input Complex Model expressions
							 * @condition Predictive Calculations
							**/
							try {
								if(useRateOfProgress || DO_PREDICTIVE_CALCS_FOR_2004_AND_NEWER || maxModelYearID <= highestFuelPredictiveModelYear) {
									// Make a table of fuelFormulationID, polProcessID, modelYearGroupID,
									// ageID, ratio
									sql = "insert into temp" + tableBaseName
											+ "A (fuelTypeID,fuelFormulationID,"
											+ " baseFuelFormulationID,"
											+ " polProcessID,pollutantID,processID,"
											+ " modelYearGroupID,minModelYearID,maxModelYearID,ageID,"
											+ " ratioNoSulfur)"
											+ " select " + fuelTypeID + ", fuelFormulationID"
											+ "," + baseFormulationID
											+ "," + polProcessID + "," + pollutantID + "," + processID
											+ "," + modelYearGroupID + "," + minModelYearID
											+ "," + maxModelYearID + "," + ageID
											+ ",(" + expressionText + ") as ratioNoSulfur"
											+ " from fuelFormulation as ff_target"
											+ " where fuelFormulationID in (" + formulationIDsCSV
											+ "," + baseFormulationID + ")";
									//Logger.log(LogMessageCategory.INFO,"sql=" + sql);
									SQLRunner.executeSQL(db,sql);
								} else {
									if(minModelYearID > highestFuelPredictiveModelYear) { // No split needed, use 1.0
										sql = "insert into temp" + tableBaseName
												+ "A (fuelTypeID,fuelFormulationID,"
												+ " baseFuelFormulationID,"
												+ " polProcessID,pollutantID,processID,"
												+ " modelYearGroupID,minModelYearID,maxModelYearID,ageID,"
												+ " ratioNoSulfur)"
												+ " select " + fuelTypeID + ", fuelFormulationID"
												+ "," + baseFormulationID
												+ "," + polProcessID + "," + pollutantID + "," + processID
												+ "," + modelYearGroupID + "," + minModelYearID
												+ "," + maxModelYearID + "," + ageID
												+ ", 1.0 as ratioNoSulfur"
												+ " from fuelFormulation as ff_target"
												+ " where fuelFormulationID in (" + formulationIDsCSV
												+ "," + baseFormulationID + ")";
										//Logger.log(LogMessageCategory.INFO,"sql=" + sql);
										SQLRunner.executeSQL(db,sql);
									} else {
										// minModelYear - highestFuelPredictiveModelYear get a calculated ratio
										sql = "insert into temp" + tableBaseName
												+ "A (fuelTypeID,fuelFormulationID,"
												+ " baseFuelFormulationID,"
												+ " polProcessID,pollutantID,processID,"
												+ " modelYearGroupID,minModelYearID,maxModelYearID,ageID,"
												+ " ratioNoSulfur)"
												+ " select " + fuelTypeID + ", fuelFormulationID"
												+ "," + baseFormulationID
												+ "," + polProcessID + "," + pollutantID + "," + processID
												+ "," + modelYearGroupID + "," + minModelYearID
												+ ", " + highestFuelPredictiveModelYear + "," + ageID
												+ ",(" + expressionText + ") as ratioNoSulfur"
												+ " from fuelFormulation as ff_target"
												+ " where fuelFormulationID in (" + formulationIDsCSV
												+ "," + baseFormulationID + ")";
										//Logger.log(LogMessageCategory.INFO,"sql=" + sql);
										SQLRunner.executeSQL(db,sql);

										// (highestFuelPredictiveModelYear+1) - maxModelYear get a ratio of 1.0
										sql = "insert into temp" + tableBaseName
												+ "A (fuelTypeID,fuelFormulationID,"
												+ " baseFuelFormulationID,"
												+ " polProcessID,pollutantID,processID,"
												+ " modelYearGroupID,minModelYearID,maxModelYearID,ageID,"
												+ " ratioNoSulfur)"
												+ " select " + fuelTypeID + ", fuelFormulationID"
												+ "," + baseFormulationID
												+ "," + polProcessID + "," + pollutantID + "," + processID
												+ "," + modelYearGroupID + "," + (highestFuelPredictiveModelYear+1)
												+ "," + maxModelYearID + "," + ageID
												+ ",1.0 as ratioNoSulfur"
												+ " from fuelFormulation as ff_target"
												+ " where fuelFormulationID in (" + formulationIDsCSV
												+ "," + baseFormulationID + ")";
										//Logger.log(LogMessageCategory.INFO,"sql=" + sql);
										SQLRunner.executeSQL(db,sql);
									}
								}
							} catch(SQLException e) {
								Logger.log(LogMessageCategory.INFO,"sql=" + sql);
								throw e;
							}
							didInsert = true;
						} // end of ageID loop

						if(didInsert) {
							if(!didDropRatioIndexes) {
								didDropRatioIndexes = true;
								dropCriteriaRatioIndexes();
							}

							// Standardize entries that have no effect

							/**
							 * @step 370
							 * @algorithm Provide default ratioNoSulfur entries. ratioNoSulfur = 1 when null.
							 * @output temporary table
							 * @condition Predictive Calculations
							**/
							sql = "update temp" + tableBaseName + "A set ratioNoSulfur=1 where ratioNoSulfur is null";
							SQLRunner.executeSQL(db,sql);

							// Build criteriaRatio by using the sulfur model on temp*A's data

							/**
							 * @step 370
							 * @algorithm Run the sulfur model with the temporary table as the input and the criteriaRatio
							 * table as the output.
							 * @input temporary table
							 * @output criteriaRatio
							 * @condition Predictive Calculations
							**/
							runSulfurModel(db,"temp" + tableBaseName + "A","criteriaRatio",useRateOfProgress);

							sql = "truncate temp" + tableBaseName + "A";
							SQLRunner.executeSQL(db,sql);

							didInsert = false;
						}
					} // enf of modelYearGroupID loop
				} // end polProcess
			} // end pi iteration of process IDs
		} // end fuelTypeIDs iteration

		//Indexes Added back to criteriaRatio
		if(didDropRatioIndexes) {
			createCriteriaRatioIndexes();
		}
		/**
		 * @step 370
		 * @algorithm End of Predictive Calculations steps.
		 * @condition Predictive Calculations
		**/
	}

	/**
	 * Rewrite ComplexModelParameterName.cmpExpression to include stddev for all
	 * parameters.
	**/
	void rewriteCmpExpressions() {
		Set<Integer> ids = cmParameters.keySet();
		for(Iterator<Integer> i=ids.iterator();i.hasNext();) {
			Parameter p = cmParameters.get(i.next());
			p.expression = rewriteCmpExpressionToIncludeStdDev(p.expression);
		}
	}

	/**
	 * Rewrite ComplexModelParameterName.cmpExpression to include stddev, changing
	 * "(fp.E200-fp_E200.center)" to "((fp.E200-fp_E200.center)/fp_E200.stddev)"
	 * and "(fp.E200-fp_E200.center)^2" to "((fp.E200-fp_E200.center)/fp_E200.stddev)^2".
	 * There may be multiple subexpressions within cmpExpression that should be changed.
	 * @param cmpExpression expression to be changed
	 * @return revised expression
	**/
	public static String rewriteCmpExpressionToIncludeStdDev(String cmpExpression) {
		int startIndex = 0;
		String expression = cmpExpression;
		while(true) {
			int index = expression.indexOf(".center)",startIndex);
			if(index < 0) {
				return expression;
			}
			// Backtrack to find the matching "("
			int parenStartIndex = -1;
			int howManyParensNeeded = 1;
			char c;
			for(int i=index;i>=0;i--) {
				c = expression.charAt(i);
				if(c == '(') {
					howManyParensNeeded--;
					if(howManyParensNeeded <= 0) {
						parenStartIndex = i;
						break;
					}
				} else if(c == ')') {
					howManyParensNeeded++;
				}
			}
			if(parenStartIndex < 0) { // If there was no matching "(" found, do nothing more
				return expression;
			}
			// Get the name of the variable (needed for the stddev reference)
			int priorIndex = expression.lastIndexOf("fp_",index);
			if(priorIndex < 0) {
				return expression;
			}
			String name = expression.substring(priorIndex,index);
			// Add the division by stddev and another level of closing parenthesis
			index += 8;
			String left = expression.substring(0,index);
			String right = expression.substring(index);
			expression = left + "/" + name + ".stddev)" + right;
			startIndex = index;
			// Add the new level of opening parenthesis
			left = expression.substring(0,parenStartIndex);
			right = expression.substring(parenStartIndex);
			expression = left + "(" + right;
		}
	}

	/**
	 * Create a comma-separated string from a set of values.
	 * @param values values to be processed
	 * @return a comma-separated string of values
	**/
	public static String getCSV(TreeSet<Integer> values) {
		String result = "";
		for(Iterator<Integer> i=values.iterator();i.hasNext();) {
			if(result.length() > 0) {
				result += ",";
			}
			result += i.next();
		}
		if(result.length() <= 0) {
			result = "0";
		}
		return result;
	}

	/**
	 * Performs MTBE calculations for the "mtbeA" calculation engine.
	 * @throws Exception if anything goes wrong
	**/
	void doMTBECalculations() throws Exception {
Logger.log(LogMessageCategory.INFO,"doMTBECalculations");
		/**
		 * @step 400
		 * @algorithm Copy MTBE atRatio entries given by expressions, deleting the entries from generalFuelRatio after the copy to atRatio.
		 * The atRatio table has fewer dimension than generalFuelRatio, so average the fuelEffectRatio.
		 * atRatio[fuelTypeID,fuelFormulationID,polProcessID,minModelYearID,maxModelYearID,ageID,monthGroupID]=average(fuelEffectRatio).
		 * @input generalFuelRatio
		 * @output generalFuelRatio
		 * @output atRatio
		 * @condition Any of these polProcessIDs: 2211,2212,2213,2218,2219
		**/
		copyGeneralFuelRatioToATRatio("2211,2212,2213,2218,2219");
		/*
		long startMillis, endMillis;
		changeFuelFormulationNulls();
		loadAllowedModelYears();

		calculationEngine = "mtbeA";
		loadFuelModelIDs();

		TreeSet<Integer> fuelTypeIDs = getFuelTypeIDs();
		fuelTypeIDs.add(Integer.valueOf(1)); // Force gasoline at a minimum
		TreeSet<Integer> candidatePolProcessIDs = getPolProcessIDs(null);
		TreeSet<Integer> polProcessIDs = new TreeSet<Integer>();
		for(Iterator<Integer> ppi=candidatePolProcessIDs.iterator();ppi.hasNext();) {
			Integer iObject = ppi.next();
			int polProcessID = iObject.intValue();
			if(!isPolProcessAllowed(polProcessID)) {
				continue;
			}
			polProcessIDs.add(iObject);
		}

		String[] setupStatements = {
			"drop table if exists MTBERatio",
			"create table if not exists MTBERatio ("
			+ " 	fuelTypeID int not null,"
			+ " 	fuelFormulationID int not null,"
			+ " 	polProcessID int not null,"
			+ " 	pollutantID int not null,"
			+ " 	processID int not null,"
			+ " 	mtbeRatio double null"
			+ " )"
		};
		String sql = "";
		for(int i=0;i<setupStatements.length;i++) {
			sql = setupStatements[i];
			SQLRunner.executeSQL(db,sql);
		}

		boolean didDropRatioIndexes = false;
		boolean didInsert = false;

		TreeSet<Integer> processIDs = new TreeSet<Integer>();
		String polProcessIDsCSV = "";
		for(Iterator<Integer> ppi=polProcessIDs.iterator();ppi.hasNext();) {
			int polProcessID = ppi.next().intValue();
			int pollutantID = polProcessID / 100;
			if(pollutantID != 22) { // 22 == MTBE
				continue;
			}
			Integer processID = Integer.valueOf(polProcessID % 100);
			if(!processIDs.contains(processID)) {
				processIDs.add(processID);
			}
			if(polProcessIDsCSV.length() > 0) {
				polProcessIDsCSV += ",";
			}
			polProcessIDsCSV += polProcessID;
		}
		if(polProcessIDs.size() <= 0) {
			return;
		}
		TreeSet<Integer> fuelFormulationsRatioed = DatabaseUtilities.getIntegerSet(db,
				"select distinct fuelFormulationID from ATRatio"
				+ " where polProcessID in (" + polProcessIDsCSV + ")");

//Logger.log(LogMessageCategory.INFO,"fuelTypeIDs.size=" + fuelTypeIDs.size());
//Logger.log(LogMessageCategory.INFO,"processIDs.size=" + processIDs.size());

		for(Iterator<Integer> fti=fuelTypeIDs.iterator();fti.hasNext();) {
			int fuelTypeID = fti.next().intValue();
			TreeSet<Integer> fuelFormulationIDs = getFuelFormulations(fuelTypeID);
			TreeSet<Integer> fuelFormulationsSupplied = getFuelSupplyFormulations(fuelTypeID);
//Logger.log(LogMessageCategory.INFO,"fuelFormulationIDs.size=" + fuelFormulationIDs.size());

			String formulationIDsCSV = "";
			int fuelFormulationsCount = 0;
			for(Iterator<Integer> ffi=fuelFormulationIDs.iterator();ffi.hasNext();) {
				Integer id = ffi.next();
				if(!fuelFormulationsSupplied.contains(id)
						|| fuelFormulationsRatioed.contains(id)) {
					continue;
				}
				int targetFormulationID = id.intValue();
				if(formulationIDsCSV.length() > 0) {
					formulationIDsCSV += ",";
				}
				formulationIDsCSV += targetFormulationID;
				fuelFormulationsCount++;
			}
			if(formulationIDsCSV.length() <= 0) {
Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + " due to existing mtbeRatio information");
				continue;
			}
Logger.log(LogMessageCategory.INFO,"FuelTypeID " + fuelTypeID + " has " + fuelFormulationsCount + " formulations");

			for(Iterator<Integer> pi=processIDs.iterator();pi.hasNext();) {
				int currentProcessID = pi.next().intValue();

				for(Iterator<Integer> ppi=polProcessIDs.iterator();ppi.hasNext();) {
					int polProcessID = ppi.next().intValue();
					int pollutantID = polProcessID / 100;
					int processID = polProcessID % 100;
					if(processID != currentProcessID || pollutantID != 22) { // 22 == MTBE
						continue;
					}

					sql = "select coeff1"
							+ " from complexModelParameters"
							+ " where fuelModelID in (" + fuelModelIDsCSV + ")"
							+ " and polProcessID = " + polProcessID
							+ " and cmpID = 45";
					double A = SQLRunner.executeScalar(db,sql);

					sql = "select coeff1"
							+ " from complexModelParameters"
							+ " where fuelModelID in (" + fuelModelIDsCSV + ")"
							+ " and polProcessID = " + polProcessID
							+ " and cmpID = 49";
					double B = SQLRunner.executeScalar(db,sql);

					sql = "insert into MTBERatio (fuelTypeID, fuelFormulationID, polProcessID,"
							+ " pollutantID, processID, mtbeRatio)"
							+ " select " + fuelTypeID + " as fuelTypeID,"
							+ "		fuelFormulationID, " + polProcessID + " as polProcessID,"
							+ "		" + pollutantID + " as pollutantID,"
							+ "		" + processID + " as processID,"
							+ " (MTBEVolume*(MTBEVolume*" + B + "+" + A + ")) as mtbeRatio"
							+ " from fuelFormulation"
							+ " where fuelFormulationID in (" + formulationIDsCSV + ")";
					//Logger.log(LogMessageCategory.INFO,sql);
					SQLRunner.executeSQL(db,sql);

					didInsert = true;
				} // end polProcess
			} // end pi iteration of process IDs
		} // end fuelTypeIDs iteration

		if(!didInsert) {
			return;
		}

		if(!didDropRatioIndexes) {
			dropATRatioIndexes();
		}

		sql = "select min(modelYearID) from RunSpecModelYear";
		int minModelYearID = (int)SQLRunner.executeScalar(db,sql);
		sql = "select max(modelYearID) from RunSpecModelYear";
		int maxModelYearID = (int)SQLRunner.executeScalar(db,sql);

		sql = "insert into ATRatio (fuelTypeID, fuelFormulationID, polProcessID,"
				+ " minModelYearID, maxModelYearID, ageID, monthGroupID,"
				+ " atRatio)"
				+ " select fuelTypeID, fuelFormulationID, polProcessID,"
				+ minModelYearID + " as minModelYearID,"
				+ maxModelYearID + " as maxModelYearID, ageID, monthGroupID,"
				+ " mtbeRatio as atRatio"
				+ " from MTBERatio, ageCategory, monthGroupOfAnyYear";
		SQLRunner.executeSQL(db,sql);

		//Indexes Added back to ATRatio
		if(didDropRatioIndexes) {
			createATRatioIndexes();
		}
		*/
	}

	/**
	 * Performs Benzene calculations for the "benzeneA" calculation engine.
	 * @throws Exception if anything goes wrong
	**/
	void doEvapBenzeneCalculations() throws Exception {
Logger.log(LogMessageCategory.INFO,"doEvapBenzeneCalculations");
		/**
		 * @step 410
		 * @algorithm Copy Benzene atRatio entries given by expressions, deleting the entries from generalFuelRatio after the copy to atRatio.
		 * The atRatio table has fewer dimension than generalFuelRatio, so average the fuelEffectRatio.
		 * atRatio[fuelTypeID,fuelFormulationID,polProcessID,minModelYearID,maxModelYearID,ageID,monthGroupID]=average(fuelEffectRatio).
		 * @input generalFuelRatio
		 * @output generalFuelRatio
		 * @output atRatio
		 * @condition Any of these polProcessIDs: 2011,2012,2013,2018,2019
		**/
		copyGeneralFuelRatioToATRatio("2011,2012,2013,2018,2019");
		/*
		long startMillis, endMillis;
		changeFuelFormulationNulls();
		loadAllowedModelYears();

		calculationEngine = "benzeneA";
		loadFuelModelIDs();

		TreeSet<Integer> fuelTypeIDs = getFuelTypeIDs();
		fuelTypeIDs.add(Integer.valueOf(1)); // Force gasoline at a minimum
		TreeSet<Integer> candidatePolProcessIDs = getPolProcessIDs(null);
		int[] tempIDs = {
			2011, 2012, 2013 //, 2018, 2019
		};
		for(int i=0;i<tempIDs.length;i++) {
			candidatePolProcessIDs.add(Integer.valueOf(tempIDs[i]));
		}

		TreeSet<Integer> polProcessIDs = new TreeSet<Integer>();
		for(Iterator<Integer> ppi=candidatePolProcessIDs.iterator();ppi.hasNext();) {
			Integer iObject = ppi.next();
			int polProcessID = iObject.intValue();
			if(!isPolProcessAllowed(polProcessID)) {
				continue;
			}
			polProcessIDs.add(iObject);
		}

		String[] setupStatements = {
			"drop table if exists BenzeneRatio",
			"create table if not exists BenzeneRatio ("
			+ " 	fuelTypeID int not null,"
			+ " 	fuelFormulationID int not null,"
			+ " 	polProcessID int not null,"
			+ " 	pollutantID int not null,"
			+ " 	processID int not null,"
			+ " 	benzeneRatio double null"
			+ " )"
		};
		String sql = "";
		for(int i=0;i<setupStatements.length;i++) {
			sql = setupStatements[i];
			SQLRunner.executeSQL(db,sql);
		}

		boolean didDropRatioIndexes = false;
		boolean didInsert = false;

		TreeSet<Integer> processIDs = new TreeSet<Integer>();
		String polProcessIDsCSV = "";
		for(Iterator<Integer> ppi=polProcessIDs.iterator();ppi.hasNext();) {
			int polProcessID = ppi.next().intValue();
			int pollutantID = polProcessID / 100;
			if(pollutantID != 20) { // 20 == Benzene
				continue;
			}
			Integer processID = Integer.valueOf(polProcessID % 100);
			if(!processIDs.contains(processID)) {
				processIDs.add(processID);
			}
			if(polProcessIDsCSV.length() > 0) {
				polProcessIDsCSV += ",";
			}
			polProcessIDsCSV += polProcessID;
		}
		if(polProcessIDs.size() <= 0) {
			//Logger.log(LogMessageCategory.INFO,"Found no benzene pollutant/processes for the fuel models");
			return;
		}
		TreeSet<Integer> fuelFormulationsRatioed = DatabaseUtilities.getIntegerSet(db,
				"select distinct fuelFormulationID from ATRatio"
				+ " where polProcessID in (" + polProcessIDsCSV + ")");

//Logger.log(LogMessageCategory.INFO,"fuelTypeIDs.size=" + fuelTypeIDs.size());
//Logger.log(LogMessageCategory.INFO,"processIDs.size=" + processIDs.size());

		for(Iterator<Integer> fti=fuelTypeIDs.iterator();fti.hasNext();) {
			int fuelTypeID = fti.next().intValue();
			TreeSet<Integer> fuelFormulationIDs = getFuelFormulations(fuelTypeID);
			TreeSet<Integer> fuelFormulationsSupplied = getFuelSupplyFormulations(fuelTypeID);
//Logger.log(LogMessageCategory.INFO,"fuelFormulationIDs.size=" + fuelFormulationIDs.size());

			String formulationIDsCSV = "";
			int fuelFormulationsCount = 0;
			for(Iterator<Integer> ffi=fuelFormulationIDs.iterator();ffi.hasNext();) {
				Integer id = ffi.next();
				if(!fuelFormulationsSupplied.contains(id)
						|| fuelFormulationsRatioed.contains(id)) {
					continue;
				}
				int targetFormulationID = id.intValue();
				if(formulationIDsCSV.length() > 0) {
					formulationIDsCSV += ",";
				}
				formulationIDsCSV += targetFormulationID;
				fuelFormulationsCount++;
			}
			if(formulationIDsCSV.length() <= 0) {
Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + " due to existing benzeneRatio information");
				continue;
			}
Logger.log(LogMessageCategory.INFO,"FuelTypeID " + fuelTypeID + " has " + fuelFormulationsCount + " formulations");

			for(Iterator<Integer> pi=processIDs.iterator();pi.hasNext();) {
				int currentProcessID = pi.next().intValue();

				for(Iterator<Integer> ppi=polProcessIDs.iterator();ppi.hasNext();) {
					int polProcessID = ppi.next().intValue();
					int pollutantID = polProcessID / 100;
					int processID = polProcessID % 100;
					if(processID != currentProcessID || pollutantID != 20) { // 20 == Benzene
						continue;
					}

					sql = "select coeff1"
							+ " from complexModelParameters"
							+ " where fuelModelID in (" + fuelModelIDsCSV + ")"
							+ " and polProcessID = " + polProcessID
							+ " and cmpID = 23";
					//Logger.log(LogMessageCategory.INFO,"Benzene A: " + sql);
					double A = SQLRunner.executeScalar(db,sql);

					sql = "select coeff1"
							+ " from complexModelParameters"
							+ " where fuelModelID in (" + fuelModelIDsCSV + ")"
							+ " and polProcessID = " + polProcessID
							+ " and cmpID = 34";
					//Logger.log(LogMessageCategory.INFO,"Benzene B: " + sql);
					double B = SQLRunner.executeScalar(db,sql);

					sql = "select coeff1"
							+ " from complexModelParameters"
							+ " where fuelModelID in (" + fuelModelIDsCSV + ")"
							+ " and polProcessID = " + polProcessID
							+ " and cmpID = 8";
					double C = SQLRunner.executeScalar(db,sql);
					//Logger.log(LogMessageCategory.INFO,"Benzene C: " + sql);
					//Logger.log(LogMessageCategory.INFO,"Benzene A=" + A + ", B=" + B + ", C=" + C);
					sql = "insert into BenzeneRatio (fuelTypeID, fuelFormulationID, polProcessID,"
							+ " pollutantID, processID, benzeneRatio)"
							+ " select " + fuelTypeID + " as fuelTypeID,"
							+ "		fuelFormulationID, " + polProcessID + " as polProcessID,"
							+ "		" + pollutantID + " as pollutantID,"
							+ "		" + processID + " as processID,"
							+ " ((benzeneContent*volToWtPercentOxy*ETOHVolume*" + A
							+ " + benzeneContent*RVP*" + B
							+ " + benzeneContent*" + C
							+ " )/100.0) as benzeneRatio"
							+ " from fuelFormulation"
							+ " where fuelFormulationID in (" + formulationIDsCSV + ")";
					//Logger.log(LogMessageCategory.INFO,sql);
					SQLRunner.executeSQL(db,sql);

					didInsert = true;
				} // end polProcess
			} // end pi iteration of process IDs
		} // end fuelTypeIDs iteration

		if(!didInsert) {
			return;
		}

		if(!didDropRatioIndexes) {
			dropATRatioIndexes();
		}

		sql = "select min(modelYearID) from RunSpecModelYear";
		int minModelYearID = (int)SQLRunner.executeScalar(db,sql);
		sql = "select max(modelYearID) from RunSpecModelYear";
		int maxModelYearID = (int)SQLRunner.executeScalar(db,sql);

		sql = "insert into ATRatio (fuelTypeID, fuelFormulationID, polProcessID,"
				+ " minModelYearID, maxModelYearID, ageID, monthGroupID,"
				+ " atRatio)"
				+ " select fuelTypeID, fuelFormulationID, polProcessID,"
				+ minModelYearID + " as minModelYearID,"
				+ maxModelYearID + " as maxModelYearID, ageID, monthGroupID,"
				+ " benzeneRatio as atRatio"
				+ " from BenzeneRatio, ageCategory, monthGroupOfAnyYear";
		SQLRunner.executeSQL(db,sql);

		//Indexes Added back to ATRatio
		if(didDropRatioIndexes) {
			createATRatioIndexes();
		}
		*/
	}

	public static class GeneralFuelRatioExpression {
		public int fuelTypeID;
		public int polProcessID;
		public int pollutantID;
		public int processID;
		public int minModelYearID;
		public int maxModelYearID;
		public int minAgeID;
		public int maxAgeID;
		public int sourceTypeID;
		public String fuelEffectRatioExpression;
		public String fuelEffectRatioGPAExpression;
		public String fuelSubtypes;

		public GeneralFuelRatioExpression(ResultSet rs) throws SQLException {
			fuelTypeID = rs.getInt("fuelTypeID");
			polProcessID = rs.getInt("polProcessID");
			pollutantID = polProcessID / 100;
			processID = polProcessID % 100;
			minModelYearID = rs.getInt("minModelYearID");
			maxModelYearID = rs.getInt("maxModelYearID");
			minAgeID = rs.getInt("minAgeID");
			maxAgeID = rs.getInt("maxAgeID");
			sourceTypeID = rs.getInt("sourceTypeID");
			fuelEffectRatioExpression = StringUtilities.safeGetString(rs.getString("fuelEffectRatioExpression"));
			fuelEffectRatioGPAExpression = StringUtilities.safeGetString(rs.getString("fuelEffectRatioGPAExpression"));
		}

		public GeneralFuelRatioExpression(GeneralFuelRatioExpression other) {
			fuelTypeID = other.fuelTypeID;
			polProcessID = other.polProcessID;
			pollutantID = other.pollutantID;
			processID = other.processID;
			minModelYearID = other.minModelYearID;
			maxModelYearID = other.maxModelYearID;
			minAgeID = other.minAgeID;
			maxAgeID = other.maxAgeID;
			sourceTypeID = other.sourceTypeID;
			fuelEffectRatioExpression = other.fuelEffectRatioExpression;
			fuelEffectRatioGPAExpression = other.fuelEffectRatioGPAExpression;
			fuelSubtypes = other.fuelSubtypes;
		}
	}

	/**
	 * Create general fuel ratios.
	 * @throws Exception if anything goes wrong
	**/
	void doGeneralFuelRatio() throws Exception {
		// steps 050-099
Logger.log(LogMessageCategory.INFO,"doGeneralFuelRatio");
		changeFuelFormulationNulls();
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		ArrayList<GeneralFuelRatioExpression> expressions = new ArrayList<GeneralFuelRatioExpression>();
		TreeSet<IntegerPair> fuelFormulationsRatioed = getIntegerPairSet(db,
				"select distinct fuelFormulationID, polProcessID from GeneralFuelRatio");
		try {
			/**
			 * @step 050
			 * @algorithm Read all expressions into memory.
			 * @input GeneralFuelRatioExpression
			 * @output list of expressions
			**/
			sql = "select * from GeneralFuelRatioExpression";
			query.open(db,sql);
			while(query.rs.next()) {
				expressions.add(new GeneralFuelRatioExpression(query.rs));
			}
			query.close();
		} finally {
			query.onFinally();
		}

		TreeSet<Integer> fuelTypeIDs = new TreeSet<Integer>();
		ArrayList<GeneralFuelRatioExpression> newExpressions = new ArrayList<GeneralFuelRatioExpression>();
		for(Iterator<GeneralFuelRatioExpression> i=expressions.iterator();i.hasNext();) {
			GeneralFuelRatioExpression exp = i.next();
			Integer id = Integer.valueOf(exp.fuelTypeID);
			if(!fuelTypeIDs.contains(id)) {
				fuelTypeIDs.add(id);
			}
			// Handle E85 alternate RVP effects
			if(exp.fuelTypeID == 5 && exp.pollutantID == 1 // Ethanol THC
					&& exp.maxModelYearID >= 2001 // applies to the target model year range
					&& (exp.processID == 1 || exp.processID == 2)) { // Running and Starts

				/**
				 * @step 050
				 * @algorithm Copy and alter expressions that affect some E85 fuels (see conditions).
				 * The new expression affects pollutant 10001 known as "Pseudo-THC", only fuel subtypes 51 and 52,
				 * and only model years >= 2001.
				 * The new expression references fuelFormulation.altRVP anywhere the old expression referenced RVP.
				 * Fuel formulation's have altRVP derived from the e10FuelProperties table in a previous step.
				 * @condition Ethanol fuel type (5), THC (1) pollutant, model years >= 2001, and Running or Starts.
				 * @output list of expressions
				**/
				GeneralFuelRatioExpression n = new GeneralFuelRatioExpression(exp);
				n.pollutantID = 10001; // Pseudo-THC
				n.polProcessID = n.pollutantID * 100 + n.processID;
				n.fuelSubtypes = "51,52"; // E70 and E85 subtypes
				n.minModelYearID = Math.max(2001,exp.minModelYearID); // Beginning in 2001 model year
				n.fuelEffectRatioExpression = 
						StringUtilities.replace(
						StringUtilities.replace(n.fuelEffectRatioExpression,"RVP","##R#V#P##"),
						"##R#V#P##","altRVP");
				n.fuelEffectRatioGPAExpression = 
						StringUtilities.replace(
						StringUtilities.replace(n.fuelEffectRatioGPAExpression,"RVP","##R#V#P##"),
						"##R#V#P##","altRVP");
				newExpressions.add(n);
			}
		}
		expressions.addAll(newExpressions);

		for(Iterator<Integer> fti=fuelTypeIDs.iterator();fti.hasNext();) {
			int fuelTypeID = fti.next().intValue();
			TreeSet<Integer> fuelFormulationIDs = getFuelFormulations(fuelTypeID);
			TreeSet<Integer> fuelFormulationsSupplied = getFuelSupplyFormulations(fuelTypeID);
//Logger.log(LogMessageCategory.INFO,"fuelFormulationIDs.size=" + fuelFormulationIDs.size());

			TreeSet<Integer> fuelFormulationsToUse = new TreeSet<Integer>();
			for(Iterator<Integer> ffi=fuelFormulationIDs.iterator();ffi.hasNext();) {
				Integer id = ffi.next();
				if(!fuelFormulationsSupplied.contains(id)) {
					continue;
				}
				fuelFormulationsToUse.add(id);
			}
			if(fuelFormulationsToUse.size() <= 0) {
				Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + " because it is not used in the fuel supply");
				continue;
			}
			Logger.log(LogMessageCategory.INFO,"FuelTypeID " + fuelTypeID + " has " + fuelFormulationsToUse.size() + " formulations");

			for(Iterator<GeneralFuelRatioExpression> i=expressions.iterator();i.hasNext();) {
				GeneralFuelRatioExpression exp = i.next();
				if(exp.fuelTypeID != fuelTypeID || exp.minModelYearID > exp.maxModelYearID) {
					continue;
				}

				Integer polProcessIDObject = Integer.valueOf(exp.polProcessID);

				String formulationIDsCSV = "";
				for(Iterator<Integer> ffi=fuelFormulationsToUse.iterator();ffi.hasNext();) {
					Integer fuelFormulationID = ffi.next();
					if(contains(fuelFormulationsRatioed,fuelFormulationID,polProcessIDObject)) {
						continue;
					}
					if(formulationIDsCSV.length() > 0) {
						formulationIDsCSV += ",";
					}
					formulationIDsCSV += fuelFormulationID;
				}
				if(formulationIDsCSV.length() <= 0) {
					Logger.log(LogMessageCategory.INFO,"Skipping fuelTypeID " + fuelTypeID + ", polProcessID " + polProcessIDObject + " due to existing fuelEffectRatio information");
					continue;
				}

				if(exp.fuelEffectRatioExpression.length() <= 0) {
					exp.fuelEffectRatioExpression = "1";
				}
				if(exp.fuelEffectRatioGPAExpression.length() <= 0) {
					exp.fuelEffectRatioGPAExpression = "1";
				}
				/**
				 * @step 050
				 * @algorithm Evaluate each expression against the list of fuel formulations that match its
				 * conditions for fuel type and fuel subtype. Each expression can use any column name in the
				 * fuelFormulation table.
				 * fuelEffectRatio = [expression].
				 * fuelEffectRatioGPA = [expression].
				 * @input list of expressions
				 * @input fuelFormulation
				 * @output GeneralFuelRatio
				**/
				sql = "insert into GeneralFuelRatio ("
						+ " fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,"
						+ "  minModelYearID, maxModelYearID, minAgeID, maxAgeID,"
						+ "  sourceTypeID, fuelEffectRatio, fuelEffectRatioGPA)"
						+ " select " + exp.fuelTypeID + ", fuelFormulationID"
						+ "," + exp.polProcessID + "," + exp.pollutantID + "," + exp.processID
						+ "," + exp.minModelYearID + "," + exp.maxModelYearID
						+ "," + exp.minAgeID + "," + exp.maxAgeID
						+ "," + exp.sourceTypeID
						+ ",(" + exp.fuelEffectRatioExpression + ")"
						+ ",(" + exp.fuelEffectRatioGPAExpression + ")"
						+ " from fuelFormulation"
						+ " where fuelFormulationID in (" + formulationIDsCSV + ")";
				if(exp.fuelSubtypes != null && exp.fuelSubtypes.length() > 0) {
					sql += " and fuelSubtypeID in (" + exp.fuelSubtypes + ")";
				}
				SQLRunner.executeSQL(db,sql);
			}
		}
		if(useRateOfProgress) {
			String[] statements = {
			};
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				SQLRunner.executeSQL(db,sql);
			}
		}
	}

	/**
	 * Copy all Air Toxics information from GeneralFuelRatio to ATRatio
	 * @throws Exception if anything goes wrong during the copy
	**/
	private void copyAirToxicsToATRatio() throws Exception {
		// This copy is redundant with other copies done previously but is safeguarded because
		// copyGeneralFuelRatioToATRatio ensures any polProcessID will only be copied once.

		/**
		 * @step 110
		 * @algorithm Copy atRatio entries given by expressions, deleting the entries from generalFuelRatio after the copy to atRatio.
		 * The atRatio table has fewer dimension than generalFuelRatio, so average the fuelEffectRatio.
		 * atRatio[fuelTypeID,fuelFormulationID,polProcessID,minModelYearID,maxModelYearID,ageID,monthGroupID]=average(fuelEffectRatio).
		 * @input generalFuelRatio
		 * @output generalFuelRatio
		 * @output atRatio
		 * @condition Any of these polProcessIDs: 2001,2002,2011,2012,2013,2018,2019,2090,2101,2102,2111,
		 * 2112,2113,2118,2119,2201,2202,2211,2212,2213,2218,2219,2301,2302,2311,2312,2313,2318,2319,2390,2401,2402,2490,
		 * 2501,2502,2590,2601,2602,2690,2701,2702,2790
		**/
		copyGeneralFuelRatioToATRatio(
			"2001,2002,2011,2012,2013,2018,2019,2090,2101,2102,2111,"
			+ "2112,2113,2118,2119,2201,2202,2211,2212,2213,2218,2219,"
			+ "2301,2302,2311,2312,2313,2318,2319,2390,2401,2402,2490,"
			+ "2501,2502,2590,2601,2602,2690,2701,2702,2790");
	}

	/**
	 * Remove pollutant/process combinations that have already been seen from a comma-separated list of IDs.
	 * @param polProcessIDsCSV comma-separated list of pollutant/process IDs
	 * @param polProcessIDsAlreadyDone set of IDs previously done.  This is also updated by calling this routine.
	 * @return comma-separated list of pollutant/process IDs or null if none are to be processed
	**/
	public static String getPolProcessIDsNotAlreadyDone(String polProcessIDsCSV, TreeSet<Integer> polProcessIDsAlreadyDone) {
		if(polProcessIDsCSV == null || polProcessIDsCSV.length() <= 0) {
			return null;
		}
		String[] parts = polProcessIDsCSV.split("\\,");
		String newPolProcessIDsCSV = "";
		for(int i=0;parts != null && i < parts.length;i++) {
			try {
				Integer id = Integer.valueOf(Integer.parseInt(parts[i].trim()));
				if(!polProcessIDsAlreadyDone.contains(id)) {
					polProcessIDsAlreadyDone.add(id);
					if(newPolProcessIDsCSV.length() > 0) {
						newPolProcessIDsCSV += ",";
					}
					newPolProcessIDsCSV += id;
				}
			} catch(Exception e) {
				// Parsing failed, the IDs aren't integers
				return polProcessIDsCSV;
			}
		}
		if(newPolProcessIDsCSV.length() <= 0) {
			return null;
		}
		return newPolProcessIDsCSV;
	}

	/**
	 * Copy records from the generalFuelRatio table to the criteriaRatio table.
	 * @param polProcessIDsCSV comma-separated list of pollutant/process IDs
	 * @throws SQLException if anything goes wrong
	**/
	private void copyGeneralFuelRatioToCriteriaRatio(String polProcessIDsCSV) throws SQLException {
		polProcessIDsCSV = getPolProcessIDsNotAlreadyDone(polProcessIDsCSV, copiedGeneralFuelRatioToCriteriaRatio);
		if(polProcessIDsCSV == null) {
			return;
		}
		String sql = "insert ignore into criteriaRatio ("
				+ " 	fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,"
				+ " 	sourceTypeID, modelYearID, ageID,"
				+ " 	ratio, ratioGPA, ratioNoSulfur)"
				+ " select fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,"
				+ " 	sourceTypeID, mya.modelYearID, mya.ageID,"
				+ " 	fuelEffectRatio, fuelEffectRatioGPA, 1.0 as ratioNoSulfur"
				+ " from generalFuelRatio gfr"
				+ " inner join runSpecModelYearAge mya on ("
				+ " 	mya.modelYearID >= gfr.minModelYearID"
				+ " 	and mya.modelYearID <= gfr.maxModelYearID"
				+ " 	and mya.ageID >= gfr.minAgeID"
				+ " 	and mya.ageID <= gfr.maxAgeID"
				+ " )"
				+ " where gfr.polProcessID in (" + polProcessIDsCSV + ")";
		SQLRunner.executeSQL(db,sql);

		sql = "delete from generalFuelRatio where polProcessID in (" + polProcessIDsCSV + ")";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Copy records from the generalFuelRatio table to the altCriteriaRatio table
	 * for model years beginning at 2001. Records in generalFuelRatio should have
	 * their alternate pollutantIDs in the 10000+ range. Data stored in altCriteriaRatio
	 * will have traditional pollutantIDs though, making joins faster in the calculator.
	 * @param polProcessIDsCSV comma-separated list of pollutant/process IDs
	 * @throws SQLException if anything goes wrong
	**/
	private void copyGeneralFuelRatioToAltCriteriaRatio(String polProcessIDsCSV) throws SQLException {
		polProcessIDsCSV = getPolProcessIDsNotAlreadyDone(polProcessIDsCSV, copiedGeneralFuelRatioToAltCriteriaRatio);
		if(polProcessIDsCSV == null) {
			return;
		}
		String sql = "insert ignore into altCriteriaRatio ("
				+ " 	fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,"
				+ " 	sourceTypeID, modelYearID, ageID,"
				+ " 	ratio, ratioGPA, ratioNoSulfur)"
				+ " select fuelTypeID, fuelFormulationID,"
				+ "		(if(pollutantID>10000,pollutantID-10000,pollutantID)*100+processID) as polProcessID,"
				+ "		if(pollutantID>10000,pollutantID-10000,pollutantID) as pollutantID, processID,"
				+ " 	sourceTypeID, mya.modelYearID, mya.ageID,"
				+ " 	fuelEffectRatio, fuelEffectRatioGPA, 1.0 as ratioNoSulfur"
				+ " from generalFuelRatio gfr"
				+ " inner join runSpecModelYearAge mya on ("
				+ " 	mya.modelYearID >= gfr.minModelYearID"
				+ " 	and mya.modelYearID <= gfr.maxModelYearID"
				+ " 	and mya.ageID >= gfr.minAgeID"
				+ " 	and mya.ageID <= gfr.maxAgeID"
				+ " )"
				+ " where gfr.polProcessID in (" + polProcessIDsCSV + ")"
				+ " and mya.modelYearID >= 2001";
		SQLRunner.executeSQL(db,sql);

		sql = "delete from generalFuelRatio where polProcessID in (" + polProcessIDsCSV + ")";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Copy records from the generalFuelRatio table to the ATRatio table.
	 * @param polProcessIDsCSV comma-separated list of pollutant/process IDs
	 * @throws SQLException if anything goes wrong
	**/
	private void copyGeneralFuelRatioToATRatio(String polProcessIDsCSV) throws SQLException {
		polProcessIDsCSV = getPolProcessIDsNotAlreadyDone(polProcessIDsCSV, copiedGeneralFuelRatioToATRatio);
		if(polProcessIDsCSV == null) {
			return;
		}
		String sql = "insert ignore into ATRatio ("
				+ " 	fuelTypeID, fuelFormulationID, polProcessID,"
				+ " 	minModelYearID, maxModelYearID,"
				+ " 	ageID, monthGroupID,"
				+ " 	atRatio)"
				+ " select fuelTypeID, fuelFormulationID, polProcessID,"
				+ " 	minModelYearID, maxModelYearID,"
				+ " 	mya.ageID, mg.monthGroupID,"
				+ " 	avg(fuelEffectRatio) as atRatio"
				+ " from generalFuelRatio gfr"
				+ " inner join runSpecModelYearAge mya on ("
				+ " 	mya.modelYearID >= gfr.minModelYearID"
				+ " 	and mya.modelYearID <= gfr.maxModelYearID"
				+ " 	and mya.ageID >= gfr.minAgeID"
				+ " 	and mya.ageID <= gfr.maxAgeID"
				+ " )"
				+ " inner join runSpecMonthGroup mg"
				+ " where gfr.polProcessID in (" + polProcessIDsCSV + ")"
				+ " group by fuelTypeID, fuelFormulationID, polProcessID,"
				+ " 	minModelYearID, maxModelYearID,"
				+ " 	mya.ageID, mg.monthGroupID";
		SQLRunner.executeSQL(db,sql);

		sql = "delete from generalFuelRatio where polProcessID in (" + polProcessIDsCSV + ")";
		SQLRunner.executeSQL(db,sql);
	}

	/**
	 * Copy records from the generalFuelRatio table to dioxinEmissionRate.
	 * @throws SQLException if anything goes wrong
	**/
	private void copyGeneralFuelRatioToDioxinEmissionRate() throws SQLException {
		/**
		 * @step 100
		 * @algorithm Get the set of dioxin pollutant/processes known to MOVES.
		 * @input dioxinEmissionRate
		 * @output list of dioxin polProcessIDs
		**/
		String polProcessIDsCSV = getCSV(DatabaseUtilities.getIntegerSet(db,"select distinct polProcessID from dioxinEmissionRate"));
		if(polProcessIDsCSV.length() <= 0 || polProcessIDsCSV.equals("0")) { // If there is nothing needed, then do nothing
			return;
		}
		String[] statements = {
			/**
			 * @step 100
			 * @algorithm Create a temporary table for dioxin emissions.
			 * @input dioxinEmissionRate
			 * @output tempDioxinEmissionRate
			**/
			"drop table if exists tempDioxinEmissionRate",
			"create table tempDioxinEmissionRate like dioxinEmissionRate",

			/**
			 * @step 100
			 * @algorithm Get dioxin emission rates that are given by expressions.
			 * dioxin meanBaseRate = fuelEffectRatio.
			 * @input generalFuelRatio
			 * @input list of dioxin polProcessIDs
			 * @output tempDioxinEmissionRate
			**/
			"insert into tempDioxinEmissionRate ("
				+ " 	polProcessID, fuelTypeID, modelYearGroupID, units, meanBaseRate)"
				+ " select polProcessID, fuelTypeID,"
				+ " 	(minModelYearID * 10000 + maxModelYearID) as modelYearGroupID,"
				+ " 	'TEQ/mile' as units,"
				+ " 	fuelEffectRatio as meanBaseRate"
				+ " from generalFuelRatio"
				+ " where polProcessID in (" + polProcessIDsCSV + ")"
				+ " group by fuelTypeID, polProcessID, minModelYearID, maxModelYearID",

			/**
			 * @step 100
			 * @algorithm Delete dioxin emission rates that are given by expressions, deleting
			 * all entries from dioxinEmissionRate that have a corresponding tempDioxinEmissionRate
			 * entry.
			 * @input tempDioxinEmissionRate
			 * @output dioxinEmissionRate
			**/
			"delete dioxinEmissionRate from dioxinEmissionRate"
				+ " inner join tempDioxinEmissionRate using (polProcessID, fuelTypeID)",

			/**
			 * @step 100
			 * @algorithm Store dioxin emission rates given by expressions.
			 * dioxinEmissionRate.meanBaseRate = tempDioxinEmissionRate.meanBaseRate.
			 * @input tempDioxinEmissionRate
			 * @output dioxinEmissionRate
			**/
			"replace into dioxinEmissionRate (polProcessID, fuelTypeID, modelYearGroupID, units, meanBaseRate)"
				+ " select polProcessID, fuelTypeID, modelYearGroupID, units, meanBaseRate"
				+ " from tempDioxinEmissionRate",

			"drop table tempDioxinEmissionRate",

			/**
			 * @step 100
			 * @algorithm Remove all dioxin polProcessIDs from generalFuelRatio.
			 * @output generalFuelRatio
			 * @input list of dioxin polProcessIDs
			**/
			"delete from generalFuelRatio where polProcessID in (" + polProcessIDsCSV + ")"
		};

		for(int i=0;i<statements.length;i++) {
			SQLRunner.executeSQL(db,statements[i]);
		}
	}

	/**
	 * Copy records from the generalFuelRatio table to metalEmissionRate.
	 * @throws SQLException if anything goes wrong
	**/
	private void copyGeneralFuelRatioToMetalEmissionRate() throws SQLException {
		/**
		 * @step 101
		 * @algorithm Get the set of metal pollutant/processes known to MOVES.
		 * @input metalEmissionRate
		 * @output list of metal polProcessIDs
		**/
		String polProcessIDsCSV = getCSV(DatabaseUtilities.getIntegerSet(db,"select distinct polProcessID from metalEmissionRate"));
		if(polProcessIDsCSV.length() <= 0 || polProcessIDsCSV.equals("0")) { // If there is nothing needed, then do nothing
			return;
		}
		String[] statements = {
			/**
			 * @step 101
			 * @algorithm Create a temporary table for metal emissions.
			 * @input metalEmissionRate
			 * @output tempMetalEmissionRate
			**/
			"drop table if exists tempMetalEmissionRate",
			"create table tempMetalEmissionRate like metalEmissionRate",

			/**
			 * @step 101
			 * @algorithm Get metal emission rates that are given by expressions.
			 * dioxin meanBaseRate = fuelEffectRatio.
			 * @input generalFuelRatio
			 * @input list of metal polProcessIDs
			 * @output tempMetalEmissionRate
			**/
			"insert into tempMetalEmissionRate ("
				+ " 	polProcessID, fuelTypeID, sourceTypeID, modelYearGroupID, units, meanBaseRate)"
				+ " select polProcessID, fuelTypeID, sourceTypeID,"
				+ " 	(minModelYearID * 10000 + maxModelYearID) as modelYearGroupID,"
				+ " 	'g/mile' as units,"
				+ " 	fuelEffectRatio as meanBaseRate"
				+ " from generalFuelRatio"
				+ " where polProcessID in (" + polProcessIDsCSV + ")"
				+ " group by fuelTypeID, polProcessID, sourceTypeID, minModelYearID, maxModelYearID",

			/**
			 * @step 101
			 * @algorithm Delete metal emission rates that are given by expressions, deleting
			 * all entries from metalEmissionRate that have a corresponding tempMetalEmissionRate
			 * entry.
			 * @input tempMetalEmissionRate
			 * @output metalEmissionRate
			**/
			"delete metalEmissionRate from metalEmissionRate"
				+ " inner join tempMetalEmissionRate using (polProcessID, fuelTypeID, sourceTypeID)",

			/**
			 * @step 101
			 * @algorithm Store metal emission rates given by expressions.
			 * metalEmissionRate.meanBaseRate = tempMetalEmissionRate.meanBaseRate.
			 * @input tempMetalEmissionRate
			 * @output metalEmissionRate
			**/
			"replace into metalEmissionRate (polProcessID, fuelTypeID, sourceTypeID, modelYearGroupID, units, meanBaseRate)"
				+ " select polProcessID, fuelTypeID, sourceTypeID, modelYearGroupID, units, meanBaseRate"
				+ " from tempMetalEmissionRate",

			"drop table tempMetalEmissionRate",

			/**
			 * @step 101
			 * @algorithm Remove all metal polProcessIDs from generalFuelRatio.
			 * @output generalFuelRatio
			 * @input list of metal polProcessIDs
			**/
			"delete from generalFuelRatio where polProcessID in (" + polProcessIDsCSV + ")"
		};

		for(int i=0;i<statements.length;i++) {
			SQLRunner.executeSQL(db,statements[i]);
		}
	}

	/**
	 * Copy records from the generalFuelRatio table to minorHapRatio.
	 * @throws SQLException if anything goes wrong
	**/
	private void copyGeneralFuelRatioToMinorHapRatio() throws SQLException {
		/**
		 * @step 102
		 * @algorithm Get the set of minor HAP pollutant/processes known to MOVES.
		 * @input minorHapRatio
		 * @output list of minor HAP polProcessIDs
		**/
		String polProcessIDsCSV = getCSV(DatabaseUtilities.getIntegerSet(db,"select distinct polProcessID from minorHapRatio"));
		if(polProcessIDsCSV.length() <= 0 || polProcessIDsCSV.equals("0")) { // If there is nothing needed, then do nothing
			return;
		}
		String[] statements = {
			/**
			 * @step 102
			 * @algorithm Create a temporary table for minor HAP rates.
			 * @input minorHapRatio
			 * @output tempMinorHapRatio
			**/
			"drop table if exists tempminorHapRatio",
			"create table tempminorHapRatio like minorHapRatio",

			/**
			 * @step 102
			 * @algorithm Get minor HAP emission rates that are given by expressions.
			 * dioxin atRatio = fuelEffectRatio.
			 * @input generalFuelRatio
			 * @input list of minor HAP polProcessIDs
			 * @output tempMinorHapRatio
			**/
			"insert into tempminorHapRatio ("
				+ " 	polProcessID, fuelTypeID, fuelSubtypeID, modelYearGroupID, atRatio)"
				+ " select polProcessID, fuelTypeID, fuelSubtypeID,"
				+ " 	(minModelYearID * 10000 + maxModelYearID) as modelYearGroupID,"
				+ " 	fuelEffectRatio as atRatio"
				+ " from generalFuelRatio"
				+ " inner join fuelFormulation using (fuelFormulationID)"
				+ " where polProcessID in (" + polProcessIDsCSV + ")"
				+ " group by fuelTypeID, polProcessID, minModelYearID, maxModelYearID, fuelSubtypeID",

			/**
			 * @step 102
			 * @algorithm Delete minor HAP emission rates that are given by expressions, deleting
			 * all entries from minorHapRatio that have a corresponding tempMinorHapRatio
			 * entry.
			 * @input tempMinorHapRatio
			 * @output minorHapRatio
			**/
			"delete minorHapRatio from minorHapRatio"
				+ " inner join tempminorHapRatio using (polProcessID, fuelTypeID, fuelSubtypeID)",

			/**
			 * @step 102
			 * @algorithm Store minor HAP emission rates given by expressions.
			 * minorHapRatio.atRatio = tempMinorHapRatio.atRatio.
			 * @input tempMinorHapRatio
			 * @output minorHapRatio
			**/
			"replace into minorHapRatio (polProcessID, fuelTypeID, fuelSubtypeID, modelYearGroupID, atRatio)"
				+ " select polProcessID, fuelTypeID, fuelSubtypeID, modelYearGroupID, atRatio"
				+ " from tempminorHapRatio",

			"drop table tempminorHapRatio",

			/**
			 * @step 102
			 * @algorithm Remove all min HAP polProcessIDs from generalFuelRatio.
			 * @output generalFuelRatio
			 * @input list of minor HAP polProcessIDs
			**/
			"delete from generalFuelRatio where polProcessID in (" + polProcessIDsCSV + ")"
		};

		for(int i=0;i<statements.length;i++) {
			SQLRunner.executeSQL(db,statements[i]);
		}
	}

	/**
	 * Copy records from the generalFuelRatio table to pahGasRatio.
	 * @throws SQLException if anything goes wrong
	**/
	private void copyGeneralFuelRatioToPahGasRatio() throws SQLException {
		/**
		 * @step 103
		 * @algorithm Get the set of gaseous PAH pollutant/processes known to MOVES.
		 * @input pahGasRatio
		 * @output list of gaseous PAH polProcessIDs
		**/
		String polProcessIDsCSV = getCSV(DatabaseUtilities.getIntegerSet(db,"select distinct polProcessID from pahGasRatio"));
		if(polProcessIDsCSV.length() <= 0 || polProcessIDsCSV.equals("0")) { // If there is nothing needed, then do nothing
			return;
		}
		String[] statements = {
			/**
			 * @step 103
			 * @algorithm Create a temporary table for gaseous PAH rates.
			 * @input pahGasRatio
			 * @output tempPahGasRatio
			**/
			"drop table if exists temppahGasRatio",
			"create table temppahGasRatio like pahGasRatio",

			/**
			 * @step 103
			 * @algorithm Get gaseous PAH emission rates that are given by expressions.
			 * dioxin atRatio = fuelEffectRatio.
			 * @input generalFuelRatio
			 * @input list of gaseous PAH polProcessIDs
			 * @output tempPahGasRatio
			**/
			"insert into temppahGasRatio ("
				+ " 	polProcessID, fuelTypeID, modelYearGroupID, atRatio)"
				+ " select polProcessID, fuelTypeID, "
				+ " 	(minModelYearID * 10000 + maxModelYearID) as modelYearGroupID,"
				+ " 	fuelEffectRatio as atRatio"
				+ " from generalFuelRatio"
				+ " where polProcessID in (" + polProcessIDsCSV + ")"
				+ " group by fuelTypeID, polProcessID, minModelYearID, maxModelYearID",

			/**
			 * @step 103
			 * @algorithm Delete gaseous PAH emission rates that are given by expressions, deleting
			 * all entries from pahGasRatio that have a corresponding tempPahGasRatio
			 * entry.
			 * @input tempPahGasRatio
			 * @output pahGasRatio
			**/
			"delete pahGasRatio from pahGasRatio"
				+ " inner join temppahGasRatio using (polProcessID, fuelTypeID)",

			/**
			 * @step 103
			 * @algorithm Store gaseous PAH emission rates given by expressions.
			 * pahGasRatio.atRatio = tempPahGasRatio.atRatio.
			 * @input tempPahGasRatio
			 * @output pahGasRatio
			**/
			"replace into pahGasRatio (polProcessID, fuelTypeID, modelYearGroupID, atRatio)"
				+ " select polProcessID, fuelTypeID, modelYearGroupID, atRatio"
				+ " from temppahGasRatio",

			"drop table temppahGasRatio",

			/**
			 * @step 103
			 * @algorithm Remove all gaseous PAH polProcessIDs from generalFuelRatio.
			 * @output generalFuelRatio
			 * @input list of gaseous PAH polProcessIDs
			**/
			"delete from generalFuelRatio where polProcessID in (" + polProcessIDsCSV + ")"
		};

		for(int i=0;i<statements.length;i++) {
			SQLRunner.executeSQL(db,statements[i]);
		}
	}

	/**
	 * Copy records from the generalFuelRatio table to pahParticleRatio.
	 * @throws SQLException if anything goes wrong
	**/
	private void copyGeneralFuelRatioToPahParticleRatio() throws SQLException {
		/**
		 * @step 104
		 * @algorithm Get the set of particulate PAH pollutant/processes known to MOVES.
		 * @input pahParticleRatio
		 * @output list of particulate PAH polProcessIDs
		**/
		String polProcessIDsCSV = getCSV(DatabaseUtilities.getIntegerSet(db,"select distinct polProcessID from pahParticleRatio"));
		if(polProcessIDsCSV.length() <= 0 || polProcessIDsCSV.equals("0")) { // If there is nothing needed, then do nothing
			return;
		}
		String[] statements = {
			/**
			 * @step 104
			 * @algorithm Create a temporary table for particulate PAH rates.
			 * @input pahParticleRatio
			 * @output tempPahParticleRatio
			**/
			"drop table if exists temppahParticleRatio",
			"create table temppahParticleRatio like pahParticleRatio",

			/**
			 * @step 104
			 * @algorithm Get particulate PAH emission rates that are given by expressions.
			 * dioxin atRatio = fuelEffectRatio.
			 * @input generalFuelRatio
			 * @input list of particulate PAH polProcessIDs
			 * @output tempPahParticleRatio
			**/
			"insert into temppahParticleRatio ("
				+ " 	polProcessID, fuelTypeID, modelYearGroupID, atRatio)"
				+ " select polProcessID, fuelTypeID, "
				+ " 	(minModelYearID * 10000 + maxModelYearID) as modelYearGroupID,"
				+ " 	fuelEffectRatio as atRatio"
				+ " from generalFuelRatio"
				+ " where polProcessID in (" + polProcessIDsCSV + ")"
				+ " group by fuelTypeID, polProcessID, minModelYearID, maxModelYearID",

			/**
			 * @step 104
			 * @algorithm Delete particulate PAH emission rates that are given by expressions, deleting
			 * all entries from pahParticleRatio that have a corresponding tempPahParticleRatio
			 * entry.
			 * @input tempPahParticleRatio
			 * @output pahParticleRatio
			**/
			"delete pahParticleRatio from pahParticleRatio"
				+ " inner join temppahParticleRatio using (polProcessID, fuelTypeID)",

			/**
			 * @step 104
			 * @algorithm Store particulate PAH emission rates given by expressions.
			 * pahParticleRatio.atRatio = tempPahParticleRatio.atRatio.
			 * @input tempPahParticleRatio
			 * @output pahParticleRatio
			**/
			"replace into pahParticleRatio (polProcessID, fuelTypeID, modelYearGroupID, atRatio)"
				+ " select polProcessID, fuelTypeID, modelYearGroupID, atRatio"
				+ " from temppahParticleRatio",

			"drop table temppahParticleRatio",

			/**
			 * @step 104
			 * @algorithm Remove all particulate PAH polProcessIDs from generalFuelRatio.
			 * @output generalFuelRatio
			 * @input list of particulate PAH polProcessIDs
			**/
			"delete from generalFuelRatio where polProcessID in (" + polProcessIDsCSV + ")"
		};

		for(int i=0;i<statements.length;i++) {
			SQLRunner.executeSQL(db,statements[i]);
		}
	}

	/**
	 * Execute the sulfur model script
	 * @param db database to be used
	 * @param inputTable table providing pre-sulfur model ratios
	 * @param outputTable table to holder new sulfur model ratios
	 * @param useRateOfProgress true if rate of progress calculations should be used
	 * @throws Exception if anything goes wrong
	**/
	public static void runSulfurModel(Connection db, String inputTable, String outputTable, boolean useRateOfProgress) throws Exception {
		if(CHECK_SULFUR_MODEL_TABLES) {
			checkSulfurModelInput(db,inputTable);
		}

		TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
		replacements.put("##sulfurInputTable##",inputTable);
		replacements.put("##sulfurOutputTable##",outputTable);

		String mymapText = "MYMAP(tempSulfurBaseLookup.modelYearID)";
		replacements.put(mymapText,ExecutionRunSpec.theExecutionRunSpec.findAndConvertModelYearMapping(mymapText));

		for(Iterator<String> i=modelYearCutPoints.keySet().iterator();i.hasNext();) {
			String k = i.next();
			replacements.put(k,modelYearCutPoints.get(k));
		}

		if(useRateOfProgress) {
			DatabaseUtilities.executeScript(db, new File("database/SulfurModelROP.sql"),replacements);
		} else {
			DatabaseUtilities.executeScript(db, new File("database/SulfurModel.sql"),replacements);
		}

		/*
		Logger.log(LogMessageCategory.INFO,"Sulfur Model counts:");
		String[] tableNames = {
			inputTable,
			"tempSulfurCalcs1",
			"tempSulfurCalcs2",
			"tempSulfurCalcs3",
			"tempSulfurCalcs3high",
			"tempSulfurCalcs3normal",
			"tempSulfurCalcs4",
			outputTable
		};
		for(int i=0;i<tableNames.length;i++) {
			String name = tableNames[i];
			Logger.log(LogMessageCategory.INFO,name + " count="
					+ SQLRunner.executeScalar(db,"select count(*) from " + name));
		}
		*/

		if(CHECK_SULFUR_MODEL_TABLES) {
			checkSulfurModelOutput(db,outputTable);
		}
	}

	/**
	 * Check an input table to the sulfur model.
	 * @param db database to be used
	 * @param inputTable table providing pre-sulfur model ratios
	 * @throws Exception if anything goes wrong
	**/
	public static void checkSulfurModelInput(Connection db, String inputTable) throws Exception {
		String sql = "";
		int t;

		sql = "select fuelFormulationID, baseFuelFormulationID, polProcessID,"
				+ " modelYearGroupID, minModelYearID, maxModelYearID, ageID, count(*)"
				+ " from " + inputTable
				+ " group by fuelFormulationID, baseFuelFormulationID, polProcessID,"
				+ " modelYearGroupID,minModelYearID, maxModelYearID, ageID"
				+ " having count(*) > 1"
				+ " limit 1";
		t = (int)SQLRunner.executeScalar(db,sql);
		if(t != 0) {
			throw new Exception("Sulfur model input table " + inputTable
					+ " has duplicate records");
		}

		sql = "select count(*) from " + inputTable + " where ratioNoSulfur is null";
		t = (int)SQLRunner.executeScalar(db,sql);
		if(t > 0) {
			throw new Exception("Sulfur model input table " + inputTable
					+ " has " + t + " null entries in ratioNoSulfur");
		}

		/*
		sql = "select sum(distinct ageID) from " + inputTable;
		t = (int)SQLRunner.executeScalar(db,sql);
		if(t != 465) { // 465 = Sum(0,1,2,...,30)
			Logger.log(LogMessageCategory.WARNING,"Sulfur model input table " + inputTable
					+ " is missing an age.  Got ageID sum of " + t
					+ " but expected 465");
		}
		*/

		Logger.log(LogMessageCategory.INFO,"Checked sulfur model input table " + inputTable);
	}

	/**
	 * Check an input table to the sulfur model.
	 * @param db database to be used
	 * @param outputTable table to holder new sulfur model ratios
	 * @throws Exception if anything goes wrong
	**/
	public static void checkSulfurModelOutput(Connection db, String outputTable) throws Exception {
		String sql = "";
		int t;

		sql = "select fuelFormulationID, polProcessID, sourceTypeID, modelYearID, ageID, count(*)"
				+ " from " + outputTable
				+ " group by fuelFormulationID, polProcessID, sourceTypeID, modelYearID, ageID"
				+ " having count(*) > 1"
				+ " limit 1";
		t = (int)SQLRunner.executeScalar(db,sql);
		if(t != 0) {
			throw new Exception("Sulfur model output table " + outputTable
					+ " has duplicate records");
		}

		sql = "select count(*) from " + outputTable + " where ratio is null";
		t = (int)SQLRunner.executeScalar(db,sql);
		if(t > 0) {
			throw new Exception("Sulfur model output table " + outputTable
					+ " has " + t + " null entries in ratio");
		}

		sql = "select count(*) from " + outputTable + " where ratioGPA is null";
		t = (int)SQLRunner.executeScalar(db,sql);
		if(t > 0) {
			throw new Exception("Sulfur model output table " + outputTable
					+ " has " + t + " null entries in ratioGPA");
		}

		sql = "select count(*) from " + outputTable + " where ratio > ratioGPA";
		t = (int)SQLRunner.executeScalar(db,sql);
		if(t > 0) {
			throw new Exception("Sulfur model output table " + outputTable
					+ " has " + t + " entries where ratio > ratioGPA");
		}

		/*
		sql = "select sum(distinct ageID) from " + outputTable;
		t = (int)SQLRunner.executeScalar(db,sql);
		if(t != 465) { // 465 = Sum(0,1,2,...,30)
			Logger.log(LogMessageCategory.WARNING,"Sulfur model output table " + outputTable
					+ " is missing an age.  Got ageID sum of " + t
					+ " but expected 465");
		}
		*/

		Logger.log(LogMessageCategory.INFO,"Checked sulfur model output table " + outputTable);
	}

	/**
	 * Generate unique fuel formulations for each region/fuelyear/monthgroup usage of an
	 * E70 and E85 fuel. The fuelSupply table will be altered to point to the new fuel
	 * formulations as well.
	 * @param db Execution database to be updated
	 * @return list of E70/E85 fuel formulations with when and where they are used
	 * @throws SQLException if anything goes wrong
	**/	
	public static ArrayList<FuelUsageEntry> cloneEthanolFuelsForRegions(Connection db) throws SQLException {
		/**
		 * @step 005
		 * @algorithm Find fuel formulation and supply information that should be duplicated
		 * for use with high ethanol fuel effects.
		 * @condition fuelSubtypeID 51 and 52, the high ethanol fuel subtypes.
		 * @input fuelSupply
		 * @input fuelFormulation
		**/
		String sql = "select fuelFormulationID, fuelRegionID, fuelYearID, monthGroupID"
				+ " from fuelSupply fs"
				+ " inner join fuelFormulation ff using (fuelFormulationID)"
				+ " where ff.fuelSubtypeID in (51,52)"
				+ " order by fuelFormulationID, fuelRegionID, fuelYearID, monthGroupID";
		SQLRunner.Query query = new SQLRunner.Query();
		ArrayList<FuelUsageEntry> fuelUsages = new ArrayList<FuelUsageEntry>();
		try {
			query.open(db,sql);
			while(query.rs.next()) {
				FuelUsageEntry f = new FuelUsageEntry(query.rs.getInt(1), query.rs.getInt(2), query.rs.getInt(3), query.rs.getInt(4));
				fuelUsages.add(f);
			}
			query.close();
			ArrayList<FuelUsageEntry> toBeCloned = new ArrayList<FuelUsageEntry>();
			FuelUsageEntry previous = null;
			for(FuelUsageEntry f : fuelUsages) {
				if(previous != null && f.fuelFormulationID == previous.fuelFormulationID) {
					toBeCloned.add(f);
				} else {
					previous = f;
				}
			}
			int nextFuelFormulationID = (int)SQLRunner.executeScalar(db,"select 1+max(fuelFormulationID) from fuelFormulation");
			for(FuelUsageEntry f : toBeCloned) {
				// Clone the fuel formulation

				/**
				 * @step 005
				 * @algorithm Every occurence of a high ethanol fuel within the fuel supply needs to
				 * reference a different fuel formulation. This allows region-based and month-based
				 * ethanol adjustments to be applied to a fuel formulation with assurance that it is
				 * only used in one region and month.
				 * For each fuel supply reference of a high ethanol fuel formulation, create a new
				 * fuel formulation with a new fuelFormulationID and containing all the properties of
				 * the original fuel formulation.
				 * @input fuelSupply
				 * @output fuelFormulation
				**/
				sql = "insert into fuelFormulation (fuelFormulationID,fuelSubtypeID,RVP,sulfurLevel,"
						+ " ETOHVolume,MTBEVolume,ETBEVolume,TAMEVolume,aromaticContent,olefinContent,"
						+ " benzeneContent,e200,e300,volToWtPercentOxy,BioDieselEsterVolume,CetaneIndex,"
						+ " PAHContent,T50,T90)"
						+ " select " + nextFuelFormulationID + " as fuelFormulationID,fuelSubtypeID,RVP,sulfurLevel,"
						+ " ETOHVolume,MTBEVolume,ETBEVolume,TAMEVolume,aromaticContent,olefinContent,"
						+ " benzeneContent,e200,e300,volToWtPercentOxy,BioDieselEsterVolume,CetaneIndex,"
						+ " PAHContent,T50,T90"
						+ " from fuelFormulation"
						+ " where fuelFormulationID=" + f.fuelFormulationID;
				SQLRunner.executeSQL(db,sql);
				// Bind the new formulation to the old fuel supply

				/**
				 * @step 005
				 * For each fuel supply reference of a high ethanol fuel formulation, change the
				 * fuel supply to reference the new fuel formulation created previously.
				 * @output fuelSupply
				**/
				sql = "update fuelSupply set fuelFormulationID=" + nextFuelFormulationID
						+ " where fuelFormulationID=" + f.fuelFormulationID
						+ " and fuelRegionID=" + f.fuelRegionID
						+ " and fuelYearID=" + f.fuelYearID
						+ " and monthGroupID=" + f.monthGroupID;
				SQLRunner.executeSQL(db,sql);

				// Advance
				f.fuelFormulationID = nextFuelFormulationID;
				nextFuelFormulationID++;
			}
			return fuelUsages;
		} finally {
			query.onFinally();
		}
	}

	/**
	 * Alter E70/E85 fuel formulation properties to be compatible with fuel effect equations.
	 * New property values are pulled from the E10FuelProperties table. Null values in
	 * E10FuelProperties causes no change to the user's E70/E85 fuel's column.
	 * A default set of fuel properties are checked for region 0, using the fuel year and
	 * month group given in the fuel supply. The fuel formulation's RVP remains its original
	 * RVP but its altRVP is filled from the E10 properties.
	 * @param db database to use
	 * @param usages E70/E85 formulations and when and where they are used
	 * @throws SQLException if anything goes wrong
	**/
	public static void alterHighEthanolFuelProperties(Connection db, ArrayList<FuelUsageEntry> usages) 
			throws SQLException {
		String sql = "";
		for(FuelUsageEntry f : usages) {
			/**
			 * @step 020
			 * @algorithm Update fuel formulation properties of each high ethanol fuel:
			 * sulfurLevel, ETOHVolume, MTBEVolume, ETBEVolume, TAMEVolume, aromaticContent, olefinContent, benzeneContent,
			 * e200, e300, BioDieselEsterVolume, CetaneIndex, PAHContent, T50, and T90.
			 * altRVP = RVP.
			 * When there is a matching e10FuelProperties entry for the fuel region (e1) and it's property column is not null, use its value.
			 * Otherwise, where there is a matching e10FuelProperties entry for the nation (e0) and it's property column is not null, use its value.
			 * Otherwise, continue to use the value of the fuel formulation.
			 * In general, fuelFormulation.[property]=coalesce(e1.[property],e0.[property],fuelFormulation.[property]).
			 * @output fuelFormulation
			 * @input e10FuelProperties as e0 where e0.fuelRegionID=0.
			 * @input e10FuelProperties as e1 where e1.fuelRegionID= fuelSupply.fuelRegionID.
			 * @input fuelSupply
			**/
			sql = "update fuelFormulation ff,"
					+ " e10FuelProperties e0 left outer join e10FuelProperties e1 on ("
					+ " 	e1.fuelYearID=e0.fuelYearID"
					+ " 	and e1.monthGroupID=e0.monthGroupID"
					+ " 	and e1.fuelRegionID=" + f.fuelRegionID + ")"
					+ " set ff.altRVP=coalesce(e1.RVP,e0.RVP,ff.RVP),"
					+ " ff.sulfurLevel=coalesce(e1.sulfurLevel,e0.sulfurLevel,ff.sulfurLevel),"
					+ " ff.ETOHVolume=coalesce(e1.ETOHVolume,e0.ETOHVolume,ff.ETOHVolume),"
					+ " ff.MTBEVolume=coalesce(e1.MTBEVolume,e0.MTBEVolume,ff.MTBEVolume),"
					+ " ff.ETBEVolume=coalesce(e1.ETBEVolume,e0.ETBEVolume,ff.ETBEVolume),"
					+ " ff.TAMEVolume=coalesce(e1.TAMEVolume,e0.TAMEVolume,ff.TAMEVolume),"
					+ " ff.aromaticContent=coalesce(e1.aromaticContent,e0.aromaticContent,ff.aromaticContent),"
					+ " ff.olefinContent=coalesce(e1.olefinContent,e0.olefinContent,ff.olefinContent),"
					+ " ff.benzeneContent=coalesce(e1.benzeneContent,e0.benzeneContent,ff.benzeneContent),"
					+ " ff.e200=coalesce(e1.e200,e0.e200,ff.e200),"
					+ " ff.e300=coalesce(e1.e300,e0.e300,ff.e300),"
					+ " ff.BioDieselEsterVolume=coalesce(e1.BioDieselEsterVolume,e0.BioDieselEsterVolume,ff.BioDieselEsterVolume),"
					+ " ff.CetaneIndex=coalesce(e1.CetaneIndex,e0.CetaneIndex,ff.CetaneIndex),"
					+ " ff.PAHContent=coalesce(e1.PAHContent,e0.PAHContent,ff.PAHContent),"
					+ " ff.T50=coalesce(e1.T50,e0.T50,ff.T50),"
					+ " ff.T90=coalesce(e1.T90,e0.T90,ff.T90)"
					+ " where ff.fuelFormulationID=" + f.fuelFormulationID
					+ " and e0.fuelRegionID=0"
					+ " and e0.fuelYearID=" + f.fuelYearID
					+ " and e0.monthGroupID=" + f.monthGroupID;
			SQLRunner.executeSQL(db,sql);
		}
		/**
		 * @step 025
		 * @algorithm volToWtPercentOxy = (ETOHVolume*0.3653 + MTBEVolume*0.1792 + ETBEVolume*0.1537 + TAMEVolume*0.1651) / (ETOHVolume+MTBEVolume+ETBEVolume+TAMEVolume)
		 * , when conditions are met, 0 otherwise.
		 * @condition (ETOHVolume+MTBEVolume+ETBEVolume+TAMEVolume) > 0.
		 * @output fuelFormulation
		**/
		DefaultDataMaker.calculateVolToWtPercentOxy(db);
	}
}
