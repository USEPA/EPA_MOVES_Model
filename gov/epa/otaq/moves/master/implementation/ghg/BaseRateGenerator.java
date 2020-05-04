/**************************************************************************************************
 * @(#)BaseRateGenerator.java
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

/**
 * This builds the BaseRate and BaseRateByAge tables
 *
 * @author		Wesley Faler
 * @version		2014-07-26
**/
public class BaseRateGenerator extends Generator {
	/**
	 * @algorithm
	 * @owner Base Rate Generator
	 * @generator
	**/

	/** true to use BaseRateByAgeHelper to perform direct calculations instead of SQL **/
	static final boolean useBaseRateByAgeHelper = true;

	/** Flag for whether the data tables have been cleared/setup **/
	boolean hasBeenSetup = false;
	/** Flag to validate the data before determining drive schedule distribution **/
	boolean isValid = true;
	/** Database connection used by all functions.  Setup by executeLoop and cleanDataLoop. **/
	Connection db;
	/** milliseconds spent during one time operations **/
	long setupTime = 0;
	/** milliseconds spent during non-one-time operations **/
	long totalTime = 0;
	/** Processes that have already been calculated **/
	TreeSet<Integer> processesDone = new TreeSet<Integer>();
	/** Links that have already been calculated **/
	TreeSet<Integer> linksDone = new TreeSet<Integer>();
	/** SourceBinDistribution tables that have been processed. **/
	TreeSet<String> sourceBinTablesDone = new TreeSet<String>();
	/** true when used for a Project domain run **/
	boolean isProjectDomain = false;
	/** road type used in a previous run of generateBaseRates() **/
	int previousRoadTypeID = 0;

	/** Default constructor **/
	public BaseRateGenerator() {
	}

	/**
	 * Requests that this object subscribe to the given loop at desired looping points.
	 * Objects can assume that all necessary MasterLoopable objects have been instantiated.
	 *
	 * @param targetLoop The loop to subscribe to.
	**/
	public void subscribeToMe(MasterLoop targetLoop) {
		isProjectDomain = ExecutionRunSpec.theExecutionRunSpec.getModelDomain() == ModelDomain.PROJECT;

		String[] processNames = {
			"Running Exhaust",
			"Start Exhaust",
			"Extended Idle Exhaust",
			"Auxiliary Power Exhaust",
			"Brakewear",
			"Tirewear"
		};
		for(int i=0;i<processNames.length;i++) {
			EmissionProcess process = EmissionProcess.findByName(processNames[i]);
			if(process != null) {
				// Signup at the YEAR level to be compatible with the SBDG but we only do our work
				// once per process.
				targetLoop.subscribe(this, process, MasterLoopGranularity.YEAR,
						MasterLoopPriority.GENERATOR-2); // Run after SBDG, OMDG, StartOMDG, and FuelEffectsGenerator.
			}
		}
	}

	/**
	 * Called each time the link changes.
	 *
	 * @param inContext The current context of the loop.
	**/
	public void executeLoop(MasterLoopContext inContext) {
		try {
			db = DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);

			long start;

			// The following only has to be done once for each run.
			if(!hasBeenSetup) {
				start = System.currentTimeMillis();
				// Do any setup items here.
				isValid = true;
				hasBeenSetup = true;
				addIndexes();
				setupTime += System.currentTimeMillis() - start;
			}

			start = System.currentTimeMillis();

			Integer processID = new Integer(inContext.iterProcess.databaseKey);
			boolean isNewProcess = !processesDone.contains(processID);
			if(isNewProcess) {
				processesDone.add(processID);
				linksDone.clear();
				Logger.log(LogMessageCategory.DEBUG,"Base Rate Generator called for process: " +
					 inContext.iterProcess.toString());
			}
			Integer linkID = new Integer(inContext.iterLocation.linkRecordID);
			boolean isNewLink = !linksDone.contains(linkID);
			if(isProjectDomain && isNewLink) {
				linksDone.add(linkID);
				Logger.log(LogMessageCategory.DEBUG,"Base Rate Generator called for link: " + linkID);
			}

			if(isProjectDomain) {
				boolean roadTypeIsUseful = true;
				if(inContext.iterLocation.roadTypeRecordID != 1
						&& (inContext.iterProcess.databaseKey == 2
							|| inContext.iterProcess.databaseKey == 90
							|| inContext.iterProcess.databaseKey == 91)) {
					roadTypeIsUseful = false;
				} else if(inContext.iterProcess.databaseKey == 1 && inContext.iterLocation.roadTypeRecordID == 1) {
					roadTypeIsUseful = false;
				}
				boolean madeNewRates = false;
				if(isValid) {
					madeNewRates = generateSBWeightedEmissionRates(inContext.iterProcess.databaseKey,inContext.iterLocation.countyRecordID,inContext.year);
				}
				if((madeNewRates && roadTypeIsUseful) || (isValid && isNewLink && roadTypeIsUseful)) {
					generateBaseRates(inContext.iterProcess.databaseKey,inContext.iterLocation.roadTypeRecordID,inContext.year);
				}
			} else {
				boolean madeNewRates = false;
				if(isValid) {
					madeNewRates = generateSBWeightedEmissionRates(inContext.iterProcess.databaseKey,inContext.iterLocation.countyRecordID,inContext.year);
				}
				if(madeNewRates || (isValid && isNewProcess)) {
					generateBaseRates(inContext.iterProcess.databaseKey,0,inContext.year);
				}
			}
			totalTime += System.currentTimeMillis() - start;
		} catch (Exception e) {
			Logger.logError(e,"Base Rate Generation failed.");
		} finally {
			DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION, db);
			db = null;
		}

		Logger.log(LogMessageCategory.INFO,"BRG setupTime=" + setupTime + " bundleTime=" + totalTime);
	}

	/**
	 * Removes data from the execution database that was created by this object within executeLoop
	 * for the same context. This is only called after all other loopable objects that might use
	 * data created by executeLoop have had their executeLoop and cleanDataLoop functions called.
	 * @param context The MasterLoopContext that applies to this execution.
	**/
	public void cleanDataLoop(MasterLoopContext context) {
		// Do not remove data since it is needed across multiple processes
	}

	/** Add indexes to key tables. **/
	void addIndexes() {
		String[] statements = {
			// "alter table ratesOpModeDistribution add key speed1 (polProcessID, sourceTypeID, opModeID, avgSpeedBinID)",
			"alter table ratesOpModeDistribution add key speed1 (sourceTypeID,polProcessID,roadTypeID,hourDayID,opModeID,avgSpeedBinID)",
			"analyze table ratesOpModeDistribution",
			"alter table avgSpeedBin add key speed1 (avgSpeedBinID, avgBinSpeed)",
			"alter table avgSpeedBin add key speed2 (avgBinSpeed, avgSpeedBinID)",
			"analyze table avgSpeedBin"
		};
		for(int i=0;i<statements.length;i++) {
			try {
				SQLRunner.executeSQL(db,statements[i]);
			} catch(Exception e) {
				// Ignore these exceptions as they are likely due to the index already existing,
				// which is acceptable.
			}
		}
	}

	/**
	 * Create and fill SBWeightedEmissionRate and SBWeightedEmissionRateByAge tables.
	 * @param processID emission process
	 * @param countyID county identifier
	 * @param year calendar year
	 * @return true if new rates were created
	**/
	boolean generateSBWeightedEmissionRates(int processID, int countyID, int year) {
		String sbdKey = "";
		String sbdTable = "";
		if(CompilationFlags.USE_FUELUSAGEFRACTION) {
			sbdTable = "sourceBinDistributionFuelUsage_" + processID + "_" + countyID + "_" + year;
			sbdKey = sbdTable;
		} else {
			sbdTable = "sourceBinDistribution";
			sbdKey = "sourceBinDistribution" + processID;
		}
		if(sourceBinTablesDone.contains(sbdKey)) {
			return false;
		}
		sourceBinTablesDone.add(sbdKey);

		String normalize = "";

		boolean isStartsOrExtIdleOrAPU = (processID == 2 || processID == 90 || processID == 91);
		if(ExecutionRunSpec.theExecutionRunSpec.getModelScale() == ModelScale.MESOSCALE_LOOKUP) {
			/**
			 * @step 008
			 * @algorithm normalizationFactor = sum(sourceBinActivityFraction).
			 * @condition Rates creation
			**/
			Logger.log(LogMessageCategory.DEBUG,"Normalizing sourcebin-weighted emission rates");
			normalize = "/ SUM(sbd.SourceBinActivityFraction)";
		} else {
			/**
			 * @step 008
			 * @algorithm normalizationFactor = 1.
			 * @condition Inventory creation
			**/
			Logger.log(LogMessageCategory.DEBUG,"Not normalizing sourcebin-weighted emission rates");
		}

		boolean applyHotelling = processID == 91;
		
		String[] statements = {
			"#CORE",
			"TRUNCATE TABLE BaseRateByAge",
			"TRUNCATE TABLE BaseRate",

			"create table if not exists BaseRateByAge_" + processID + "_" + year + " like BaseRateByAge",
			"create table if not exists BaseRate_" + processID + "_" + year + " like BaseRate",
			"truncate table BaseRateByAge_" + processID + "_" + year,
			"truncate table BaseRate_" + processID + "_" + year,

			"#SBWeightedEmissionRateByAge",
			"TRUNCATE TABLE SBWeightedEmissionRateByAge",
			"#SBWeightedEmissionRate",
			"TRUNCATE TABLE SBWeightedEmissionRate",
			"#SBWeightedDistanceRate",
			"TRUNCATE TABLE SBWeightedDistanceRate",

			/**
			 * @step 010
			 * @algorithm Weight age-based rates by sourcebin distribution.
			 * MeanBaseRate=sum(SourceBinActivityFraction * MeanBaseRate)/normalizationFactor.
			 * MeanBaseRateIM=sum(SourceBinActivityFraction * MeanBaseRateIM)/normalizationFactor.
			 * MeanBaseRateACAdj=sum(SourceBinActivityFraction * MeanBaseRate * (coalesce(fullACAdjustment,1.0)-1.0))/normalizationFactor.
			 * MeanBaseRateIMACAdj=sum(SourceBinActivityFraction * MeanBaseRateIM * (coalesce(fullACAdjustment,1.0)-1.0))/normalizationFactor.
			 * sumSBD=sum(SourceBinActivityFraction)/normalizationFactor.
			 * sumSBDRaw=sum(SourceBinActivityFraction).
			 * @output SBWeightedEmissionRateByAge
			 * @input EmissionRateByAge
			 * @input PollutantProcessModelYear
			 * @input SourceBin
			 * @input SourceBinDistribution
			 * @input SourceTypeModelYear
			 * @input RunspecModelYearAgeGroup
			 * @input PollutantProcessAssoc
			 * @input fullACAdjustment
			**/
			"#SBWeightedEmissionRateByAge",
			"INSERT INTO SBWeightedEmissionRateByAge ("
			+ " 	sourceTypeID,"
			+ " 	polProcessID,"
			+ " 	modelYearID,"
			+ " 	fuelTypeID,"
			+ " 	opModeID,"
			+ " 	ageGroupID,"
			+ "		regClassID,"
			+ " 	MeanBaseRate,"
			+ " 	MeanBaseRateIM,"
			+ " 	MeanBaseRateACAdj,"
			+ " 	MeanBaseRateIMACAdj,"
			+ "		sumSBD, sumSBDRaw)"
			+ " SELECT"
			+ " 	stmy.sourceTypeID,"
			+ " 	er.polProcessID,"
			+ " 	stmy.modelYearID,"
			+ " 	sb.fuelTypeID,"
			+ " 	er.opModeID,"
			+ " 	er.ageGroupID,"
			+ "		sb.regClassID,"
			+ " 	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRate)" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRateIM)" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRate * (coalesce(fullACAdjustment,1.0)-1.0))" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRateIM * (coalesce(fullACAdjustment,1.0)-1.0))" + normalize + ","
			+ "		SUM(sbd.SourceBinActivityFraction)" + normalize + ","
			+ "		SUM(sbd.SourceBinActivityFraction)"
			+ " FROM"
			+ " 	EmissionRateByAge er"
			+ " 	inner join PollutantProcessModelYear ppmy"
			+ " 	inner join SourceBin sb"
			+ " 	inner join " + sbdTable + " sbd"
			+ " 	inner join SourceTypeModelYear stmy"
			+ " 	inner join RunspecModelYearAgeGroup rsmy"
			+ " 	inner join PollutantProcessAssoc ppa"
			+ " 	left outer join fullACAdjustment fac on ("
			+ " 		fac.sourceTypeID = stmy.sourceTypeID"
			+ " 		and fac.polProcessID = er.polProcessID"
			+ " 		and fac.opModeID = er.opModeID)"
			+ " WHERE"
			+ " 	ppmy.modelYearGroupID = sb.modelYearGroupID AND"
			+ " 	ppmy.modelYearID = stmy.modelYearID AND"
			+ " 	er.polProcessID = ppmy.polProcessID AND"
			+ " 	er.polProcessID = sbd.polProcessID AND"
			+ " 	ppmy.polProcessID = sbd.polProcessID AND"
			+ " 	er.sourceBinID = sb.sourceBinID AND"
			+ " 	er.sourceBinID = sbd.sourceBinID AND"
			+ " 	sb.sourceBinID = sbd.sourceBinID AND"
			+ " 	sbd.sourceTypeModelYearID = stmy.sourceTypeModelYearID AND"
			+ " 	stmy.modelYearID = rsmy.modelYearID AND"
			+ " 	ppa.polProcessID = er.polProcessID AND"
			+ " 	er.ageGroupID = rsmy.ageGroupID AND"
			+ " 	ppa.processID = " + processID + " AND"
			+ " 	rsmy.yearID = " + year
			+ " GROUP BY"
			+ "		stmy.sourceTypeID,"
			+ " 	er.polProcessID,"
			+ " 	stmy.modelYearID,"
			+ " 	sb.fuelTypeID,"
			+ " 	er.opModeID,"
			+ " 	er.ageGroupID,"
			+ "		sb.regClassID"
			+ " HAVING SUM(sbd.SourceBinActivityFraction) > 0",

			/**
			 * @step 010
			 * @algorithm Weight non-age-based rates by sourcebin distribution.
			 * MeanBaseRate=sum(SourceBinActivityFraction * MeanBaseRate)/normalizationFactor.
			 * MeanBaseRateIM=sum(SourceBinActivityFraction * MeanBaseRateIM)/normalizationFactor.
			 * MeanBaseRateACAdj=sum(SourceBinActivityFraction * MeanBaseRate * (coalesce(fullACAdjustment,1.0)-1.0))/normalizationFactor.
			 * MeanBaseRateIMACAdj=sum(SourceBinActivityFraction * MeanBaseRateIM * (coalesce(fullACAdjustment,1.0)-1.0))/normalizationFactor.
			 * sumSBD=sum(SourceBinActivityFraction)/normalizationFactor.
			 * sumSBDRaw=sum(SourceBinActivityFraction).
			 * @output SBWeightedEmissionRate
			 * @input EmissionRate
			 * @input PollutantProcessModelYear
			 * @input SourceBin
			 * @input SourceBinDistribution
			 * @input SourceTypeModelYear
			 * @input RunspecModelYear
			 * @input PollutantProcessAssoc
			 * @input fullACAdjustment
			**/
			"#SBWeightedEmissionRate",
			"INSERT INTO SBWeightedEmissionRate ("
			+ " 	sourceTypeID,"
			+ " 	polProcessID,"
			+ " 	modelYearID,"
			+ " 	fuelTypeID,"
			+ " 	opModeID,"
			+ " 	regClassID,"
			+ " 	MeanBaseRate,"
			+ " 	MeanBaseRateIM,"
			+ " 	MeanBaseRateACAdj,"
			+ " 	MeanBaseRateIMACAdj,"
			+ "		sumSBD, sumSBDRaw)"
			+ " SELECT"
			+ " 	stmy.sourceTypeID,"
			+ " 	er.polProcessID,"
			+ " 	stmy.modelYearID,"
			+ " 	sb.fuelTypeID,"
			+ " 	er.opModeID,"
			+ "		sb.regClassID,"
			+ " 	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRate)" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRateIM)" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRate * (coalesce(fullACAdjustment,1.0)-1.0))" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRateIM * (coalesce(fullACAdjustment,1.0)-1.0))" + normalize + ","
			+ "		SUM(sbd.SourceBinActivityFraction)" + normalize + ","
			+ "		SUM(sbd.SourceBinActivityFraction)"
			+ " FROM"
			+ " 	EmissionRate er"
			+ " 	inner join PollutantProcessModelYear ppmy"
			+ " 	inner join SourceBin sb"
			+ " 	inner join " + sbdTable + " sbd"
			+ " 	inner join SourceTypeModelYear stmy"
			+ " 	inner join RunspecModelYear rsmy"
			+ " 	inner join PollutantProcessAssoc ppa"
			+ " 	left outer join fullACAdjustment fac on ("
			+ " 		fac.sourceTypeID = stmy.sourceTypeID"
			+ " 		and fac.polProcessID = er.polProcessID"
			+ " 		and fac.opModeID = er.opModeID)"
			+ " WHERE"
			+ " 	ppmy.modelYearGroupID = sb.modelYearGroupID AND"
			+ " 	ppmy.modelYearID = stmy.modelYearID AND"
			+ " 	er.polProcessID = ppmy.polProcessID AND"
			+ " 	er.polProcessID = sbd.polProcessID AND"
			+ " 	ppmy.polProcessID = sbd.polProcessID AND"
			+ " 	er.sourceBinID = sb.sourceBinID AND"
			+ " 	er.sourceBinID = sbd.sourceBinID AND"
			+ " 	sb.sourceBinID = sbd.sourceBinID AND"
			+ " 	sbd.sourceTypeModelYearID = stmy.sourceTypeModelYearID AND"
			+ " 	stmy.modelYearID = rsmy.modelYearID AND"
			+ " 	ppa.polProcessID = er.polProcessID AND"
			+ " 	ppa.processID = " + processID
			+ " GROUP BY"
			+ "		stmy.sourceTypeID,"
			+ " 	er.polProcessID,"
			+ " 	stmy.modelYearID,"
			+ " 	sb.fuelTypeID,"
			+ " 	er.opModeID,"
			+ "		sb.regClassID"
			+ " HAVING SUM(sbd.SourceBinActivityFraction) > 0",

			/**
			 * @step 020
			 * @algorithm Weight distance-based rates by sourcebin distribution.
			 * Use fullACAdjustment for opModeID=300 (All Running).
			 * Use the SourceBinDistribution for Running Exhaust Total Gaseous Hydrocarbons (pol/proc 101).
			 * MeanBaseRate=sum(SourceBinActivityFraction * ratePerSHO)/normalizationFactor.
			 * MeanBaseRateIM=sum(SourceBinActivityFraction * ratePerSHO)/normalizationFactor.
			 * MeanBaseRateACAdj=sum(SourceBinActivityFraction * ratePerSHO * (coalesce(fullACAdjustment,1.0)-1.0))/normalizationFactor.
			 * MeanBaseRateIMACAdj=sum(SourceBinActivityFraction * ratePerSHO * (coalesce(fullACAdjustment,1.0)-1.0))/normalizationFactor.
			 * sumSBD=sum(SourceBinActivityFraction)/normalizationFactor.
			 * sumSBDRaw=sum(SourceBinActivityFraction).
			 * @output SBWeightedDistanceRate
			 * @input distanceEmissionRate
			 * @input PollutantProcessModelYear
			 * @input SourceBin
			 * @input SourceBinDistribution
			 * @input SourceTypeModelYear
			 * @input PollutantProcessAssoc
			 * @input modelYearGroup
			 * @input fullACAdjustment
			 * @condition Running Exhaust
			**/
			"#SBWeightedDistanceRate",
			(processID == 1?
			"INSERT INTO SBWeightedDistanceRate ("
			+ " 	sourceTypeID,"
			+ " 	polProcessID,"
			+ " 	modelYearID,"
			+ " 	fuelTypeID,"
			+ " 	regClassID,"
			+ " 	avgSpeedBinID,"
			+ " 	MeanBaseRate,"
			+ " 	MeanBaseRateIM,"
			+ " 	MeanBaseRateACAdj,"
			+ " 	MeanBaseRateIMACAdj,"
			+ "		sumSBD, sumSBDRaw)"
			+ " SELECT"
			+ " 	er.sourceTypeID,"
			+ " 	er.polProcessID,"
			+ " 	er.modelYearID,"
			+ " 	er.fuelTypeID,"
			+ "		sb.regClassID,"
			+ " 	er.avgSpeedBinID,"
			+ " 	SUM(sbd.SourceBinActivityFraction * er.ratePerSHO)" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.ratePerSHO)" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.ratePerSHO * (coalesce(fullACAdjustment,1.0)-1.0))" + normalize + ","
			+ " 	SUM(sbd.SourceBinActivityFraction * er.ratePerSHO * (coalesce(fullACAdjustment,1.0)-1.0))" + normalize + ","
			+ "		SUM(sbd.SourceBinActivityFraction)" + normalize + ","
			+ "		SUM(sbd.SourceBinActivityFraction)"
			+ " FROM"
			+ " 	distanceEmissionRate er"
			+ " 	inner join SourceBin sb"
			+ " 	inner join " + sbdTable + " sbd"
			+ " 	inner join SourceTypeModelYear stmy"
			+ " 	inner join PollutantProcessAssoc ppa"
			+ "		inner join modelYearGroup myg on ("
			+ "			myg.modelYearGroupID = sb.modelYearGroupID"
			+ "			and myg.modelYearGroupStartYear <= er.modelYearID"
			+ "			and myg.modelYearGroupEndYear >= er.modelYearID)"
			+ " 	left outer join fullACAdjustment fac on ("
			+ " 		fac.sourceTypeID = stmy.sourceTypeID"
			+ " 		and fac.polProcessID = er.polProcessID"
			+ " 		and fac.opModeID = 300)" // opModeID=300 is All Running
			+ " WHERE"
			+ " 	er.modelYearID = stmy.modelYearID AND"
			+ " 	er.fuelTypeID = sb.fuelTypeID AND"
			+ " 	sbd.polProcessID = 101 AND" // use Running Exhaust Total Gaseous Hydrocarbons (101)
			+ " 	sb.sourceBinID = sbd.sourceBinID AND"
			+ " 	sbd.sourceTypeModelYearID = stmy.sourceTypeModelYearID AND"
			+ " 	stmy.modelYearID = er.modelYearID AND"
			+ "		stmy.sourceTypeID = er.sourceTypeID AND"
			+ " 	ppa.polProcessID = er.polProcessID AND"
			+ " 	ppa.processID = " + processID
			+ " GROUP BY"
			+ "		er.sourceTypeID,"
			+ " 	er.polProcessID,"
			+ " 	er.modelYearID,"
			+ " 	er.fuelTypeID,"
			+ "		sb.regClassID,"
			+ " 	er.avgSpeedBinID"
			+ " HAVING SUM(sbd.SourceBinActivityFraction) > 0"
			:
			""),

			/**
			 * @step 025
			 * @algorithm Apply hotelling activity distribution to age-weighted rates.
			 * MeanBaseRate = MeanBaseRate * opModeFraction, for MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, and MeanBaseRateIMACAdj.
			 * @output SBWeightedEmissionRateByAge
			 * @input hotellingActivityDistribution
			 * @condition Auxiliary Power Exhaust
			**/
			"#SBWeightedEmissionRateByAge",
			applyHotelling?
				"update SBWeightedEmissionRateByAge, hotellingActivityDistribution"
				+ " set MeanBaseRate = MeanBaseRate * opModeFraction,"
				+ " 	MeanBaseRateIM = MeanBaseRateIM * opModeFraction,"
				+ " 	MeanBaseRateACAdj = MeanBaseRateACAdj * opModeFraction,"
				+ " 	MeanBaseRateIMACAdj = MeanBaseRateIMACAdj * opModeFraction"
				+ " where beginModelYearID <= modelYearID"
				+ " and endModelYearID >= modelYearID"
				+ " and hotellingActivityDistribution.opModeID = SBWeightedEmissionRateByAge.opModeID"
				: "",

			/**
			 * @step 025
			 * @algorithm Apply hotelling activity distribution to non-age-weighted rates.
			 * MeanBaseRate = MeanBaseRate * opModeFraction, for MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, and MeanBaseRateIMACAdj.
			 * @output SBWeightedEmissionRate
			 * @input hotellingActivityDistribution
			 * @condition Auxiliary Power Exhaust
			**/
			"#SBWeightedEmissionRate",
			applyHotelling?
				"update SBWeightedEmissionRate, hotellingActivityDistribution"
				+ " set MeanBaseRate = MeanBaseRate * opModeFraction,"
				+ " 	MeanBaseRateIM = MeanBaseRateIM * opModeFraction,"
				+ " 	MeanBaseRateACAdj = MeanBaseRateACAdj * opModeFraction,"
				+ " 	MeanBaseRateIMACAdj = MeanBaseRateIMACAdj * opModeFraction"
				+ " where beginModelYearID <= modelYearID"
				+ " and endModelYearID >= modelYearID"
				+ " and hotellingActivityDistribution.opModeID = SBWeightedEmissionRate.opModeID"
				: ""
		};
		String sql = "";
		TaggedSQLRunner concurrentSQL = null;
		try {
			if(processID == 1) {
				makeDistanceRates();
			}
			String context = "#CORE";
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				if(sql != null && sql.length() > 0) {
					if(sql.startsWith("#")) {
						if(context.length() <= 0 || context.equalsIgnoreCase("#CORE")) {
							if(concurrentSQL != null) {
								concurrentSQL.execute();
								concurrentSQL.clear();
								concurrentSQL.close();
							}
						}
						context = sql;
						continue;
					}
					if(concurrentSQL == null) {
						concurrentSQL = getSQLRunner();
					}
					if(concurrentSQL == null) {
						Logger.log(LogMessageCategory.INFO,"Directly Running: " + context + ": " + StringUtilities.substring(sql,0,100));
						SQLRunner.executeSQL(db,sql);
					} else {
						concurrentSQL.add(context,sql);
					}
				}
			}
			if(concurrentSQL != null) {
				Logger.log(LogMessageCategory.INFO,"BRG SBW running concurrent statements...");
				concurrentSQL.execute();
				Logger.log(LogMessageCategory.INFO,"BRG SBW done running concurrent statements.");
			}
		} catch(Exception e) {
			Logger.logSqlError(e,"Could not generate SBWeightedEmissionRate",sql);
		} finally {
			if(concurrentSQL != null) {
				concurrentSQL.stats.print();
				concurrentSQL.onFinally();
			}
		}
		return true;
	}

	/**
	 * Populate BaseRateByAge and BaseRate tables.
	 * @param processID emission process
	 * @param roadTypeID road type to be processed, 0 for all road types
	 * @param yearID calendar year, may be 0
	**/
	void generateBaseRates(int processID, int roadTypeID, int yearID) {
		BaseRateByAgeHelper.Flags brbaFlags = new BaseRateByAgeHelper.Flags();
		boolean applyAvgSpeedDistribution = 
				(processID == 1 || processID == 9 || processID == 10)
				&& 
				(ExecutionRunSpec.theExecutionRunSpec.getModelScale() == ModelScale.MACROSCALE);
		boolean useAvgSpeedBin = (!applyAvgSpeedDistribution) && (processID == 1 || processID == 9 || processID == 10);
		boolean applySourceBinDistribution = ExecutionRunSpec.theExecutionRunSpec.getModelScale() == ModelScale.MACROSCALE;
		boolean keepOpModeID = false;

		String quantAdjust = "";
		if(ExecutionRunSpec.theExecutionRunSpec.getModelScale() == ModelScale.MESOSCALE_LOOKUP) {
			if(processID == 2 || processID == 90 || processID == 91) {
				quantAdjust = "* sumSBDRaw";
				brbaFlags.useSumSBDRaw = true;
			}
		}

		if(processID == 2 || processID == 90 || processID == 91) {
			applySourceBinDistribution = true;
		}

		keepOpModeID = processID == 2;

		if(isProjectDomain && (processID == 1 || processID == 9 || processID == 10)) {
			applyAvgSpeedDistribution = false;
			useAvgSpeedBin = false;
			keepOpModeID = false;

			brbaFlags.useAvgSpeedBin = false;
		}

		String avgSpeedFraction = "";
		if(applyAvgSpeedDistribution) {
			avgSpeedFraction = "*coalesce(avgSpeedFraction,0)";
			brbaFlags.useAvgSpeedFraction = true;
		}

		String sumSBD = "";
		if(applySourceBinDistribution) {
			sumSBD = "*sumSBD";
			brbaFlags.useSumSBD = true;
		}

		brbaFlags.keepOpModeID = keepOpModeID;
		brbaFlags.useAvgSpeedBin = useAvgSpeedBin;

		/**
		 * @step 101
		 * @algorithm avgSpeedFractionClause=coalesce(avgSpeedFraction,0) when conditions are met, 1 otherwise.
		 * @condition Non-Project domain; Inventory; Running exhaust, Brakewear, or Tirewear.
		**/

		/**
		 * @step 101
		 * @algorithm sumSBDClause=sumSBD when conditions are met, 1 otherwise.
		 * @condition Inventory or Starts or Extended Idling or Auxiliary Power.
		**/
		
		/**
		 * @step 101
		 * @algorithm quantAdjustClause=sumSBDRaw when conditions are met, 1 otherwise.
		 * @condition Rates for Starts, Extended Idle, or Auxiliary Power.
		**/

		Logger.log(LogMessageCategory.DEBUG,"BaseRateGenerator: processID=" + processID
				+ ", applyAvgSpeedDistribution=" + applyAvgSpeedDistribution
				+ ", useAvgSpeedBin=" + useAvgSpeedBin
				+ ", applySourceBinDistribution=" + applySourceBinDistribution
				+ ", keepOpModeID=" + keepOpModeID
				+ ", avgSpeedFraction=\"" + avgSpeedFraction + "\""
				+ ", sumSBD=\"" + sumSBD + "\"");

		String[] noOpModeStatements = {
			/**
			 * @step 110
			 * @algorithm Calculate BaseRateByAge without operating mode, retaining average speed bin.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRateByAge
			 * @condition Not Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRateByAge without operating mode, aggregating average speed bins.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRateByAge
			 * @condition Not Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			"#BaseRateByAge",
			"insert into BaseRateByAge_" + processID + "_" + yearID + " ("
			+ " 	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, modelYearID, fuelTypeID, ageGroupID, regClassID, opModeID,"
			+ " 	opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID)"
			+ " select"
			+ " 	romd.sourceTypeID, romd.roadTypeID,"
			+ (!useAvgSpeedBin? " 0 as avgSpeedBinID," : " romd.avgSpeedBinID,")
			+ "		romd.hourDayID, romd.polProcessID,"
			+ " 	er.modelYearID, er.fuelTypeID, er.ageGroupID, er.regClassID, 0 as opModeID,"
			+ "		sum(opModeFraction" + avgSpeedFraction + sumSBD + "),"
			+ "		sum(opModeFraction" + avgSpeedFraction + sumSBD + "),"
			+ " 	sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + "),")
				)
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + "),")
				)
			+ " 	mod(romd.polProcessID,100) as processID, floor(romd.polProcessID/100) as pollutantID"
			+ " from"
			+ " 	RatesOpModeDistribution romd"
			+ " 	inner join SBWeightedEmissionRateByAge er on ("
			+ " 		er.sourceTypeID = romd.sourceTypeID"
			+ " 		and er.polProcessID = romd.polProcessID"
			+ " 		and er.opModeID = romd.opModeID"
			+ " 	)"
			+ " where romd.sourceTypeID = ##sourceTypeID## and romd.polProcessID = ##polProcessID##"
			+ (isProjectDomain && roadTypeID > 0? " and romd.roadTypeID=" + roadTypeID : "")
			+ " group by"
			+ " 	romd.sourceTypeID, romd.polProcessID, romd.roadTypeID, romd.hourDayID,"
			+ (!useAvgSpeedBin? "" : " romd.avgSpeedBinID,")
			+ " 	er.modelYearID, er.fuelTypeID, er.ageGroupID, er.regClassID",

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate without operating mode, retaining average speed bin.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRate
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRate
			 * @condition Not Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate without operating mode, aggregating average speed bins.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRate
			 * @condition Not Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			"#BaseRate",
			"insert into BaseRate_" + processID + "_" + yearID + " ("
			+ " 	sourceTypeID, roadTypeID, avgSpeedBinID, polProcessID, hourDayID, modelYearID, fuelTypeID, regClassID, opModeID,"
			+ " 	opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID)"
			+ " select"
			+ " 	romd.sourceTypeID, romd.roadTypeID,"
			+ (!useAvgSpeedBin? " 0 as avgSpeedBinID," : " romd.avgSpeedBinID,")
			+ "		romd.polProcessID, romd.hourDayID,"
			+ " 	er.modelYearID, er.fuelTypeID, er.regClassID, 0 as opModeID,"
			+ "		sum(opModeFraction" + avgSpeedFraction + sumSBD + "),"
			+ "		sum(opModeFraction" + avgSpeedFraction + sumSBD + "),"
			+ " 	sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + "),")
				)
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + "),")
				)
			+ " 	mod(romd.polProcessID,100) as processID, floor(romd.polProcessID/100) as pollutantID"
			+ " from"
			+ " 	RatesOpModeDistribution romd"
			+ " 	inner join SBWeightedEmissionRate er on ("
			+ " 		er.sourceTypeID = romd.sourceTypeID"
			+ " 		and er.polProcessID = romd.polProcessID"
			+ " 		and er.opModeID = romd.opModeID"
			+ " 	)"
			+ " where romd.sourceTypeID = ##sourceTypeID## and romd.polProcessID = ##polProcessID##"
			+ (isProjectDomain && roadTypeID > 0? " and romd.roadTypeID=" + roadTypeID : "")
			+ " group by"
			+ " 	romd.sourceTypeID, romd.polProcessID, romd.roadTypeID, romd.hourDayID,"
			+ (!useAvgSpeedBin? "" : " romd.avgSpeedBinID,")
			+ " 	er.modelYearID, er.fuelTypeID, er.regClassID",

			// Add from distanceEmissionRate.

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate without operating mode, retaining average speed bin.
			 * opModeFraction=sum(avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRate
			 * @input RatesOpModeDistribution
			 * @input SBWeightedDistanceRate
			 * @condition Not Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate without operating mode, aggregating average speed bins.
			 * opModeFraction=sum(avgSpeedFractionClause * sumSBDClause).
			 * opModeFractionRate=sum(avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedDistanceRate
			 * @condition Not Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/
			"#BaseRate",
			"insert into BaseRate_" + processID + "_" + yearID + " ("
			+ " 	sourceTypeID, roadTypeID, avgSpeedBinID, polProcessID, hourDayID, modelYearID, fuelTypeID, regClassID, opModeID,"
			+ " 	opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID)"
			+ " select"
			+ " 	er.sourceTypeID, rsrt.roadTypeID,"
			+ (!useAvgSpeedBin? " 0 as avgSpeedBinID," : " er.avgSpeedBinID,")
			+ "		er.polProcessID, rshd.hourDayID,"
			+ " 	er.modelYearID, er.fuelTypeID, er.regClassID, 0 as opModeID,"
			+ "		sum(1" + avgSpeedFraction + sumSBD + "),"
			+ "		sum(1" + avgSpeedFraction + sumSBD + "),"
			+ " 	sum(MeanBaseRate" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIM" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateACAdj" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIMACAdj" + avgSpeedFraction + quantAdjust + "),"
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRate" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIM" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRate" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIM" + avgSpeedFraction + "),")
				)
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRateACAdj" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRateACAdj" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIMACAdj" + avgSpeedFraction + "),")
				)
			+ " 	mod(er.polProcessID,100) as processID, floor(er.polProcessID/100) as pollutantID"
			+ " from"
			+ " 	SBWeightedDistanceRate er"
			+ " 	inner join runSpecRoadType rsrt"
			+ " 	inner join runSpecHourDay rshd"
			+ (applyAvgSpeedDistribution?
			  "		left outer join avgSpeedDistribution asd on ("
			+ "			asd.sourceTypeID = er.sourceTypeID"
			+ "			and asd.roadTypeID = rsrt.roadTypeID"
			+ "			and asd.hourDayID = rshd.hourDayID"
			+ "			and asd.avgSpeedBinID = er.avgSpeedBinID)"
				: ""
			)
			+ (useAvgSpeedBin?
				" inner join avgSpeedBin asb on (asb.avgSpeedBinID = er.avgSpeedBinID)"
				:
				"")
			+ " where rsrt.roadTypeID > 1 and rsrt.roadTypeID < 100"
			+ " and er.sourceTypeID = ##sourceTypeID## and er.polProcessID = ##polProcessID##"
			+ (isProjectDomain && roadTypeID > 0? " and rsrt.roadTypeID=" + roadTypeID : "")
			+ " group by"
			+ "		rsrt.roadTypeID, rshd.hourDayID,"
			+ " 	er.sourceTypeID,"
			+ "		er.polProcessID,"
			+ " 	er.modelYearID, er.fuelTypeID, er.regClassID"
			+ (!useAvgSpeedBin? "" : ", er.avgSpeedBinID")
		};

		String[] withOpModeStatements = {
			/**
			 * @step 110
			 * @algorithm Calculate BaseRateByAge with operating mode, retaining average speed bin.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRateByAge
			 * @condition Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRateByAge with operating mode, aggregating average speed bins.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRateByAge
			 * @condition Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			"#BaseRateByAge",
			"insert into BaseRateByAge_" + processID + "_" + yearID + " ("
			+ " 	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, modelYearID, fuelTypeID, ageGroupID, regClassID, opModeID,"
			+ " 	opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID)"
			+ " select"
			+ " 	romd.sourceTypeID, romd.roadTypeID,"
			+ (!useAvgSpeedBin? " 0 as avgSpeedBinID," : " romd.avgSpeedBinID,")
			+ "		romd.hourDayID, romd.polProcessID,"
			+ " 	er.modelYearID, er.fuelTypeID, er.ageGroupID, er.regClassID, romd.opModeID,"
			+ "		sum(opModeFraction" + avgSpeedFraction + sumSBD + quantAdjust + "),"
			+ "		sum(opModeFraction" + avgSpeedFraction + sumSBD + "),"

			+ " 	sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + "),")
				)
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + "),")
				)
			+ " 	mod(romd.polProcessID,100) as processID, floor(romd.polProcessID/100) as pollutantID"
			+ " from"
			+ " 	RatesOpModeDistribution romd"
			+ " 	inner join SBWeightedEmissionRateByAge er on ("
			+ " 		er.sourceTypeID = romd.sourceTypeID"
			+ " 		and er.polProcessID = romd.polProcessID"
			+ " 		and er.opModeID = romd.opModeID"
			+ " 	)"
			+ " where romd.sourceTypeID = ##sourceTypeID## and romd.polProcessID = ##polProcessID##"
			+ (isProjectDomain && roadTypeID > 0? " and romd.roadTypeID=" + roadTypeID : "")
			+ " group by"
			+ " 	romd.sourceTypeID, romd.polProcessID, romd.roadTypeID, romd.hourDayID, romd.opModeID,"
			+ (!useAvgSpeedBin? "" : " romd.avgSpeedBinID,")
			+ " 	er.modelYearID, er.fuelTypeID, er.ageGroupID, er.regClassID",

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate with operating mode, retaining average speed bin.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRate
			 * @condition Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate with operating mode, aggregating average speed bins.
			 * opModeFraction=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(opModeFraction * avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * opModeFraction * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * opModeFraction * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * opModeFraction * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedEmissionRate
			 * @condition Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			"#BaseRate",
			"insert into BaseRate_" + processID + "_" + yearID + " ("
			+ " 	sourceTypeID, roadTypeID, avgSpeedBinID, polProcessID, hourDayID, modelYearID, fuelTypeID, regClassID, opModeID,"
			+ " 	opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID)"
			+ " select"
			+ " 	romd.sourceTypeID, romd.roadTypeID,"
			+ (!useAvgSpeedBin? " 0 as avgSpeedBinID," : " romd.avgSpeedBinID,")
			+ "		romd.polProcessID, romd.hourDayID,"
			+ " 	er.modelYearID, er.fuelTypeID, er.regClassID, romd.opModeID,"
			+ "		sum(opModeFraction" + avgSpeedFraction + sumSBD + quantAdjust + "),"
			+ "		sum(opModeFraction" + avgSpeedFraction + sumSBD + "),"

			+ " 	sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + quantAdjust + "),"
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRate * opModeFraction" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIM * opModeFraction" + avgSpeedFraction + "),")
				)
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRateACAdj * opModeFraction" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIMACAdj * opModeFraction" + avgSpeedFraction + "),")
				)
			+ " 	mod(romd.polProcessID,100) as processID, floor(romd.polProcessID/100) as pollutantID"
			+ " from"
			+ " 	RatesOpModeDistribution romd"
			+ " 	inner join SBWeightedEmissionRate er on ("
			+ " 		er.sourceTypeID = romd.sourceTypeID"
			+ " 		and er.polProcessID = romd.polProcessID"
			+ " 		and er.opModeID = romd.opModeID"
			+ " 	)"
			+ " where romd.sourceTypeID = ##sourceTypeID## and romd.polProcessID = ##polProcessID##"
			+ (isProjectDomain && roadTypeID > 0? " and romd.roadTypeID=" + roadTypeID : "")
			+ " group by"
			+ " 	romd.sourceTypeID, romd.polProcessID, romd.roadTypeID, romd.hourDayID, romd.opModeID,"
			+ (!useAvgSpeedBin? "" : " romd.avgSpeedBinID,")
			+ " 	er.modelYearID, er.fuelTypeID, er.regClassID",

			// Add from distanceEmissionRate. Use 300 as the opModeID.

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate with operating mode 300, retaining average speed bin.
			 * opModeFraction=sum(avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=case when avgBinSpeed>0 then sum(MeanBaseRate * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIM=case when avgBinSpeed>0 then sum(MeanBaseRateIM * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateACAdj * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * emissionRateIMACAdj=case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj * avgSpeedFractionClause) / avgBinSpeed else null end.
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedDistanceRate
			 * @condition Start Exhaust
			 * @condition Retaining average speed bin (Non-Project domain; Rates; Running Exhaust, Brakewear, or Tirewear)
			**/

			/**
			 * @step 110
			 * @algorithm Calculate BaseRate with operating mode 300, aggregating average speed bins.
			 * opModeFraction=sum(avgSpeedFractionClause * sumSBDClause * quantAdjustClause).
			 * opModeFractionRate=sum(avgSpeedFractionClause * sumSBDClause).
			 * MeanBaseRate=sum(MeanBaseRate * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause * quantAdjustClause).
			 * MeanBaseRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause * quantAdjustClause).
			 * emissionRate=sum(MeanBaseRate * avgSpeedFractionClause).
			 * emissionRateIM=sum(MeanBaseRateIM * avgSpeedFractionClause).
			 * emissionRateACAdj=sum(MeanBaseRateACAdj * avgSpeedFractionClause).
			 * emissionRateIMACAdj=sum(MeanBaseRateIMACAdj * avgSpeedFractionClause).
			 * @output BaseRateByAge
			 * @input RatesOpModeDistribution
			 * @input SBWeightedDistanceRate
			 * @condition Start Exhaust
			 * @condition Aggregate average speed bins (Project domain or Inventory or Rates for Non-(Running, Brakewear, or Tirewear))
			**/

			"#BaseRate",
			"insert into BaseRate_" + processID + "_" + yearID + " ("
			+ " 	sourceTypeID, roadTypeID, avgSpeedBinID, polProcessID, hourDayID, modelYearID, fuelTypeID, regClassID, opModeID,"
			+ " 	opModeFraction, opModeFractionRate, MeanBaseRate, MeanBaseRateIM, MeanBaseRateACAdj, MeanBaseRateIMACAdj, emissionRate, emissionRateIM, emissionRateACAdj, emissionRateIMACAdj, processID, pollutantID)"
			+ " select"
			+ " 	er.sourceTypeID, rsrt.roadTypeID,"
			+ (!useAvgSpeedBin? " 0 as avgSpeedBinID," : " er.avgSpeedBinID,")
			+ "		er.polProcessID, rshd.hourDayID,"
			+ " 	er.modelYearID, er.fuelTypeID, er.regClassID, 300 as opModeID,"
			+ "		sum(1" + avgSpeedFraction + sumSBD + "),"
			+ "		sum(1" + avgSpeedFraction + sumSBD + "),"
			+ " 	sum(MeanBaseRate" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIM" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateACAdj" + avgSpeedFraction + quantAdjust + "),"
			+ " 	sum(MeanBaseRateIMACAdj" + avgSpeedFraction + quantAdjust + "),"
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRate" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIM" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRate" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIM" + avgSpeedFraction + "),")
				)
			+ (useAvgSpeedBin?
				( " 	case when avgBinSpeed>0 then sum(MeanBaseRateACAdj" + avgSpeedFraction + ") / avgBinSpeed else null end,"
				+ " 	case when avgBinSpeed>0 then sum(MeanBaseRateIMACAdj" + avgSpeedFraction + ") / avgBinSpeed else null end,")
				:
				( " 	sum(MeanBaseRateACAdj" + avgSpeedFraction + "),"
				+ " 	sum(MeanBaseRateIMACAdj" + avgSpeedFraction + "),")
				)
			+ " 	mod(er.polProcessID,100) as processID, floor(er.polProcessID/100) as pollutantID"
			+ " from"
			+ " 	SBWeightedDistanceRate er"
			+ " 	inner join runSpecRoadType rsrt"
			+ " 	inner join runSpecHourDay rshd"
			+ (applyAvgSpeedDistribution?
			  "		left outer join avgSpeedDistribution asd on ("
			+ "			asd.sourceTypeID = er.sourceTypeID"
			+ "			and asd.roadTypeID = rsrt.roadTypeID"
			+ "			and asd.hourDayID = rshd.hourDayID"
			+ "			and asd.avgSpeedBinID = er.avgSpeedBinID)"
				: ""
			)
			+ (useAvgSpeedBin?
				" inner join avgSpeedBin asb on (asb.avgSpeedBinID = er.avgSpeedBinID)"
				:
				"")
			+ " where rsrt.roadTypeID > 1 and rsrt.roadTypeID < 100"
			+ " and er.sourceTypeID = ##sourceTypeID## and er.polProcessID = ##polProcessID##"
			+ (isProjectDomain && roadTypeID > 0? " and rsrt.roadTypeID=" + roadTypeID : "")
			+ " group by"
			+ "		rsrt.roadTypeID, rshd.hourDayID,"
			+ " 	er.sourceTypeID,"
			+ "		er.polProcessID,"
			+ " 	er.modelYearID, er.fuelTypeID, er.regClassID"
			+ (!useAvgSpeedBin? "" : ", er.avgSpeedBinID")
		};

		String[] statements = keepOpModeID? withOpModeStatements : noOpModeStatements;
		String sql = "";
		SQLRunner.Query query = new SQLRunner.Query();
		TaggedSQLRunner concurrentSQL = null;
		try {
			concurrentSQL = getSQLRunner();
			String context = "#CORE";

			if(isProjectDomain && previousRoadTypeID > 0) {
				context = "#CORE";
				String[] cleanupStatements = {
					"#BaseRateByAge",
					"delete from BaseRateByAge_" + processID + "_" + yearID + " where roadTypeID=" + previousRoadTypeID,
					"#BaseRate",
					"delete from BaseRate_" + processID + "_" + yearID + " where roadTypeID=" + previousRoadTypeID
				};
				concurrentSQL.clear();
				for(int i=0;i<cleanupStatements.length;i++) {
					sql = cleanupStatements[i];
					if(sql != null && sql.length() > 0) {
						if(sql.startsWith("#")) {
							context = sql;
							continue;
						}
						//Logger.log(LogMessageCategory.DEBUG,sql);
						if(context.length() <= 0 || context.equalsIgnoreCase("#CORE")) {
							//Logger.log(LogMessageCategory.INFO,"Running #CORE: " + StringUtilities.substring(sql,0,100));
							SQLRunner.executeSQL(db,sql);
						} else {
							if(concurrentSQL == null) {
								Logger.log(LogMessageCategory.INFO,"Directly Running: " + context + ": " + StringUtilities.substring(sql,0,100));
								SQLRunner.executeSQL(db,sql);
							} else {
								concurrentSQL.add(context,sql);
							}
						}
					}
				}
				concurrentSQL.execute();
				concurrentSQL.clear();
				concurrentSQL.close();
				previousRoadTypeID = 0;
			}
			if(applyAvgSpeedDistribution) {
				sql = "update RatesOpModeDistribution, avgSpeedDistribution"
						+ " 	set RatesOpModeDistribution.avgSpeedFraction = avgSpeedDistribution.avgSpeedFraction"
						+ " where RatesOpModeDistribution.sourceTypeID = avgSpeedDistribution.sourceTypeID"
						+ " and RatesOpModeDistribution.roadTypeID = avgSpeedDistribution.roadTypeID"
						+ " and RatesOpModeDistribution.hourDayID = avgSpeedDistribution.hourDayID"
						+ " and RatesOpModeDistribution.avgSpeedBinID = avgSpeedDistribution.avgSpeedBinID"
						+ " and RatesOpModeDistribution.avgSpeedFraction <= 0";
				long startMillis = System.currentTimeMillis();
				SQLRunner.executeSQL(db,sql);
				long endMillis = System.currentTimeMillis();
				Logger.log(LogMessageCategory.INFO,"BRG update ROMD,ASD: " + (endMillis-startMillis) + " ms");
			}
			// Get iteration dimensions, keeping only those that belong to the desired process.
			ArrayList<String> tuples = new ArrayList<String>();
			sql = "select distinct sourceTypeID, polProcessID from SBWeightedEmissionRateByAge"
					+ " union"
					+ " select distinct sourceTypeID, polProcessID from SBWeightedEmissionRate"
					+ " union"
					+ " select distinct sourceTypeID, polProcessID from SBWeightedDistanceRate";
			query.open(db,sql);
			while(query.rs.next()) {
				String sourceTypeID = query.rs.getString(1);
				int polProcessID = query.rs.getInt(2);
				if(polProcessID % 100 == processID) {
					tuples.add(sourceTypeID);
					tuples.add("" + polProcessID);
				}
			}
			query.close();
			Logger.log(LogMessageCategory.DEBUG,"BaseRateGenerator processID=" + processID + ", roadTypeID=" + roadTypeID + ", yearID=" + yearID + " has " + (tuples.size()/2) + " iterations");

			concurrentSQL.clear();
			TreeMapIgnoreCase replacements = new TreeMapIgnoreCase();
			context = "#CORE";
			for(int ti=0;ti<tuples.size();ti+=2) {
				String sourceTypeID = tuples.get(ti+0);
				String polProcessID = tuples.get(ti+1);
				//Logger.log(LogMessageCategory.DEBUG,"BaseRateGenerator for sourceTypeID=" + sourceTypeID + ", polProcessID=" + polProcessID);
				replacements.clear();
				replacements.put("##sourceTypeID##",sourceTypeID);
				replacements.put("##polProcessID##",polProcessID);
				for(int i=0;i<statements.length;i++) {
					sql = statements[i];
					if(sql != null && sql.length() > 0) {
						sql = StringUtilities.doReplacements(sql,replacements);
						//Logger.log(LogMessageCategory.INFO,sql);

						if(sql.startsWith("#")) {
							if(context.length() <= 0 || context.equalsIgnoreCase("#CORE")) {
								if(concurrentSQL != null) {
									concurrentSQL.execute();
									concurrentSQL.clear();
									concurrentSQL.close();
								}
							}
							context = sql;
							continue;
						}
						if(concurrentSQL == null) {
							SQLRunner.executeSQL(db,sql);
						} else {
							BaseRateByAgeHelper.Context brbaContext = new BaseRateByAgeHelper.Context();
							brbaContext.polProcessID = Integer.parseInt(polProcessID);
							brbaContext.processID = processID;
							brbaContext.roadTypeID = isProjectDomain && roadTypeID > 0? roadTypeID : 0;
							brbaContext.sourceTypeID = Integer.parseInt(sourceTypeID);
							brbaContext.yearID = yearID;
							concurrentSQL.add(context,sql,brbaContext,brbaFlags);
						}
					}
				}
			}
			Logger.log(LogMessageCategory.DEBUG,"BaseRateGenerator executing concurrent SQL...");
			concurrentSQL.execute();
			Logger.log(LogMessageCategory.DEBUG,"BaseRateGenerator concurrent SQL done.");
		} catch(Exception e) {
			Logger.logSqlError(e,"Could not generate base rates",sql);
		} finally {
			query.onFinally();
			if(concurrentSQL != null) {
				concurrentSQL.stats.print();
				concurrentSQL.onFinally();
			}
			Logger.log(LogMessageCategory.DEBUG,"Done with BaseRateGenerator processID=" + processID + ", roadTypeID=" + roadTypeID + ", yearID=" + yearID);
		}
		previousRoadTypeID = roadTypeID;
	}

	/**
	 * Combine metalEmissionRate and dioxinEmissionRate tables into a single format
	 * useful to both rate/SHO and rate/mile calculations. Do this after any GFRE adjustments
	 * to either table. The output table is distanceEmissionRate.
	**/
	void makeDistanceRates() {
		String normalize = "";

		if(ExecutionRunSpec.theExecutionRunSpec.getModelScale() == ModelScale.MESOSCALE_LOOKUP) {
			Logger.log(LogMessageCategory.DEBUG,"Normalizing sourcebin-weighted distance rates");
			normalize = "/ SUM(sbd.SourceBinActivityFraction)";
		} else {
			Logger.log(LogMessageCategory.DEBUG,"Not normalizing sourcebin-weighted distance rates");
		}

		String[] statements = {
			"truncate table distanceEmissionRate",

			/**
			 * @step 015
			 * @algorithm Make distance emission rates for metals.
			 * ratePerMile=meanBaseRate * (1.0 when units of g/mile, 1.609344 when g/km, 1.0 when TEQ/mile, 1.609344 when TEQ/km).
			 * ratePerSHO=meanBaseRate * avgBinSpeed * (1.0 when units of g/mile, 1.609344 when g/km, 1.0 when TEQ/mile, 1.609344 when TEQ/km).
			 * @output distanceEmissionRate
			 * @input metalEmissionRate
			 * @input averageSpeedBin
			**/

			// metalEmissionRate
			"insert into distanceEmissionRate ("
			+ " 	polProcessID,"
			+ " 	fuelTypeID, sourceTypeID,"
			+ " 	modelYearID,"
			+ " 	avgSpeedBinID,"
			+ " 	ratePerMile, ratePerSHO)"
			+ " select r.polProcessID,"
			+ " 	rssf.fuelTypeID, rssf.sourceTypeID, "
			+ " 	rsmy.modelYearID,"
			+ " 	asb.avgSpeedBinID,"
			+ " 	(case units when 'g/mile' then 1.0"
			+ " 		when 'g/km' then 1.609344"
			+ " 		when 'TEQ/mile' then 1.0"
			+ " 		when 'TEQ/km' then 1.609344"
			+ " 		else 1.0"
			+ " 	end)*(r.meanBaseRate) as ratePerMile,"
			+ " 	(case units when 'g/mile' then 1.0"
			+ " 		when 'g/km' then 1.609344"
			+ " 		when 'TEQ/mile' then 1.0"
			+ " 		when 'TEQ/km' then 1.609344"
			+ " 		else 1.0"
			+ " 	end)*(r.meanBaseRate * asb.avgBinSpeed) as ratePerSHO"
			+ " from metalEmissionRate r"
			+ " inner join runSpecSourceFueltype rssf on ("
			+ " 	rssf.sourceTypeID = r.sourceTypeID"
			+ " 	and rssf.fuelTypeID = r.fuelTypeID)"
			+ " inner join runSpecModelYear rsmy on ("
			+ " 	rsmy.modelYearID >= floor(r.modelYearGroupID/10000)"
			+ " 	and rsmy.modelYearID <= mod(r.modelYearGroupID,10000))"
			+ " inner join avgSpeedBin asb",

			/**
			 * @step 015
			 * @algorithm Make distance emission rates for dioxins.
			 * ratePerMile=meanBaseRate * (1.0 when units of g/mile, 1.609344 when g/km, 1.0 when TEQ/mile, 1.609344 when TEQ/km).
			 * ratePerSHO=meanBaseRate * avgBinSpeed * (1.0 when units of g/mile, 1.609344 when g/km, 1.0 when TEQ/mile, 1.609344 when TEQ/km).
			 * @output distanceEmissionRate
			 * @input dioxinEmissionRate
			 * @input averageSpeedBin
			**/
			
			// dioxinEmissionRate
			"insert into distanceEmissionRate ("
			+ " 	polProcessID,"
			+ " 	fuelTypeID, sourceTypeID,"
			+ " 	modelYearID,"
			+ " 	avgSpeedBinID,"
			+ " 	ratePerMile, ratePerSHO)"
			+ " select r.polProcessID,"
			+ " 	rssf.fuelTypeID, rssf.sourceTypeID,"
			+ " 	rsmy.modelYearID,"
			+ " 	asb.avgSpeedBinID,"
			+ " 	(case units when 'g/mile' then 1.0"
			+ " 		when 'g/km' then 1.609344"
			+ " 		when 'TEQ/mile' then 1.0"
			+ " 		when 'TEQ/km' then 1.609344"
			+ " 		else 1.0"
			+ " 	end)*(r.meanBaseRate) as ratePerMile,"
			+ " 	(case units when 'g/mile' then 1.0"
			+ " 		when 'g/km' then 1.609344"
			+ " 		when 'TEQ/mile' then 1.0"
			+ " 		when 'TEQ/km' then 1.609344"
			+ " 		else 1.0"
			+ " 	end)*(r.meanBaseRate * asb.avgBinSpeed) as ratePerSHO"
			+ " from dioxinEmissionRate r"
			+ " inner join runSpecSourceFueltype rssf on ("
			+ " 	rssf.fuelTypeID = r.fuelTypeID)"
			+ " inner join runSpecModelYear rsmy on ("
			+ " 	rsmy.modelYearID >= floor(r.modelYearGroupID/10000)"
			+ " 	and rsmy.modelYearID <= mod(r.modelYearGroupID,10000))"
			+ " inner join avgSpeedBin asb"
		};

		String sql = "";
		try {
			for(int i=0;i<statements.length;i++) {
				sql = statements[i];
				if(sql != null && sql.length() > 0) {
					//Logger.log(LogMessageCategory.DEBUG,sql);
					SQLRunner.executeSQL(db,sql);
				}
			}
		} catch (SQLException e) {
			Logger.logSqlError(e,"Could not generate distance rates",sql);
		}
	}

	/**
	 * Obtain a TaggedSQLRunner that is connected to the execution database.
	 * @returns a TaggedSQLRunner, never null
	**/	
	TaggedSQLRunner getSQLRunner() {
		return new TaggedSQLRunner(new TaggedSQLRunner.ConnectionProvider() {
			public Connection checkOutConnection() {
				try {
					return DatabaseConnectionManager.checkOutConnection(MOVESDatabaseType.EXECUTION);
				} catch(Exception e) {
					return null;
				}
			}
			
			public void checkInConnection(Connection c) {
				try {
					DatabaseConnectionManager.checkInConnection(MOVESDatabaseType.EXECUTION,c);
				} catch(Exception e) {
					// Nothing to do here
				}
			}
		},
		new TaggedSQLRunner.OverrideHandler() {
			public boolean onOverrideSQL(Connection db,String sql,Object data1,Object data2) throws Exception {
				if(!useBaseRateByAgeHelper
						|| !sql.startsWith("insert into BaseRateByAge_")
						|| !(data1 instanceof BaseRateByAgeHelper.Context)
						|| !(data2 instanceof BaseRateByAgeHelper.Flags)) {
					return false;
				}
				Logger.log(LogMessageCategory.DEBUG,"BaseRateGenerator executing BaseRateByAgeHelper...");
				BaseRateByAgeHelper helper = new BaseRateByAgeHelper(db);
				helper.process((BaseRateByAgeHelper.Context)data1,(BaseRateByAgeHelper.Flags)data2);
				Logger.log(LogMessageCategory.DEBUG,"BaseRateGenerator executing BaseRateByAgeHelper Done");
				return true;
			}
		});
	}
}
