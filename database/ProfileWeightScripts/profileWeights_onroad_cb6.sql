CREATE DATABASE IF NOT EXISTS ##newDB##;

/* -------------------------------------- PREP COLLECTOR DATABASE IF NECESSARY ------------------------------------------------ */
CREATE TABLE IF NOT EXISTS ##newDB##.SMOKE_MOVES_mapping (
	processID 		SMALLINT(6),
	processName 	VARCHAR(50),
	roadTypeID 		SMALLINT(6),
	rateTable 		VARCHAR(10),
	SMOKE_process 	SMALLINT(6),
	SMOKE_mode 		VARCHAR(20),
	PRIMARY KEY (processID, processName, roadTypeID, rateTable, SMOKE_process, SMOKE_mode)
);

CREATE TABLE IF NOT EXISTS ##newDB##.base_schema (
	countyID 				INT(11),
	monthID 				SMALLINT(6),
	SMOKE_SCC 				VARCHAR(10),
	SMOKE_mode 				VARCHAR(20),
	pollutantID 			SMALLINT(6),
	pollutantName 			VARCHAR(50),
	togSpeciationProfileID  VARCHAR(10),
	profileContribution		DOUBLE,
	PRIMARY KEY (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, togSpeciationProfileID)
);

CREATE TABLE IF NOT EXISTS ##newDB##.exh_nhtog LIKE ##newDB##.base_schema;
CREATE TABLE IF NOT EXISTS ##newDB##.epm_nhtog LIKE ##newDB##.base_schema;
CREATE TABLE IF NOT EXISTS ##newDB##.evp_nhtog LIKE ##newDB##.base_schema;
CREATE TABLE IF NOT EXISTS ##newDB##.rfl_nhtog LIKE ##newDB##.base_schema;

-- PM only has exhuast emissions (no evap or refueling) so we only need one table for each PM pollutant
CREATE TABLE IF NOT EXISTS ##newDB##.exh_tom (
	countyID 				INT(11),
	monthID 				SMALLINT(6),
	SMOKE_SCC 				VARCHAR(10),
	SMOKE_mode 				VARCHAR(20),
	pollutantID 			SMALLINT(6),
	pollutantName 			VARCHAR(50),
	pmSpeciationProfileID   VARCHAR(10),
	profileContribution		DOUBLE,
	PRIMARY KEY (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, pmSpeciationProfileID)
);

CREATE TABLE IF NOT EXISTS ##newDB##.exh_residpm (
	countyID 				INT(11),
	monthID 				SMALLINT(6),
	SMOKE_SCC 				VARCHAR(10),
	SMOKE_mode 				VARCHAR(20),
	pollutantID 			SMALLINT(6),
	pollutantName 			VARCHAR(50),
	pmSpeciationProfileID   VARCHAR(10),
	profileContribution		DOUBLE,
	PRIMARY KEY (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, pmSpeciationProfileID)
);



-- The values in this table are hardcoded into the script, which may seem like an odd choice. However, having
--  this table is very helpful in converting MOVES SCCs to SMOKE SCCs, and keeping track of a csv file in 
-- 	addition to this script is probably more work than its worth. If the mapping of MOVES SCCs to SMOKE SCCs
-- 	changes, then this script will have to change accordingly.
INSERT IGNORE INTO ##newDB##.SMOKE_MOVES_mapping 
	(processID, processName, roadTypeID, rateTable, SMOKE_process, SMOKE_mode) VALUES 
	(1, "Running Exhaust", 1, "RPHO", 92, "exh_nhtog"),
	(1, "Running Exhaust", 2, "RPD", 72, "exh_nhtog"),
	(1, "Running Exhaust", 3, "RPD", 72, "exh_nhtog"),
	(1, "Running Exhaust", 4, "RPD", 72, "exh_nhtog"),
	(1, "Running Exhaust", 5, "RPD", 72, "exh_nhtog"),
	
	(15, "Crankcase Running Exhaust", 1, "RPHO", 92, "exh_nhtog"),
	(15, "Crankcase Running Exhaust", 2, "RPD", 72, "exh_nhtog"),
	(15, "Crankcase Running Exhaust", 3, "RPD", 72, "exh_nhtog"),
	(15, "Crankcase Running Exhaust", 4, "RPD", 72, "exh_nhtog"),
	(15, "Crankcase Running Exhaust", 5, "RPD", 72, "exh_nhtog"),
	
	(2, "Start Exhaust", 1, "RPS", 72, "exh_nhtog"),
	(16, "Crankcase Start Exhaust", 1, "RPS", 72, "exh_nhtog"),
	
	(11, "Evap Permeation", 1, "RPV", 72, "epm_nhtog"),
	(11, "Evap Permeation", 2, "RPD", 72, "epm_nhtog"),
	(11, "Evap Permeation", 3, "RPD", 72, "epm_nhtog"),
	(11, "Evap Permeation", 4, "RPD", 72, "epm_nhtog"),
	(11, "Evap Permeation", 5, "RPD", 72, "epm_nhtog"),
	
	(12, "Evap Fuel Vapor Venting", 1, "RPP", 72, "evp_nhtog"),
	
	(13, "Evap Fuel Leaks", 1, "RPV", 72, "evp_nhtog"),
	(13, "Evap Fuel Leaks", 2, "RPD", 72, "evp_nhtog"),
	(13, "Evap Fuel Leaks", 3, "RPD", 72, "evp_nhtog"),
	(13, "Evap Fuel Leaks", 4, "RPD", 72, "evp_nhtog"),
	(13, "Evap Fuel Leaks", 5, "RPD", 72, "evp_nhtog"),
	
	(18, "Refueling Displacement Vapor Loss", 2, "RPD", 62, "rfl_nhtog"),
	(18, "Refueling Displacement Vapor Loss", 3, "RPD", 62, "rfl_nhtog"),
	(18, "Refueling Displacement Vapor Loss", 4, "RPD", 62, "rfl_nhtog"),
	(18, "Refueling Displacement Vapor Loss", 5, "RPD", 62, "rfl_nhtog"),
	
	(19, "Refueling Spillage Loss", 2, "RPD", 62, "rfl_nhtog"),
	(19, "Refueling Spillage Loss", 3, "RPD", 62, "rfl_nhtog"),
	(19, "Refueling Spillage Loss", 4, "RPD", 62, "rfl_nhtog"),
	(19, "Refueling Spillage Loss", 5, "RPD", 62, "rfl_nhtog"),
	
	(90, "Extended Idle Exhaust", 1, "RPH", 53, "exh_nhtog"),
	(91, "Auxiliary Power Exhaust", 1, "RPH", 91, "exh_nhtog"),
	(17, "Crankcase Extended Idle Exhaust", 1, "RPH", 53, "exh_nhtog")
	
	-- for evap vapor venting, the script only calculates emissions from road type 1, all others are ignored
	-- for refueling, the script only calculates emissions from on-network road types, road type 1 (aka refueling from ONI) is ignored
	-- this means that emissions before/after applying this table are not necessarily conserved
	-- uncomment the following lines to run the script ensuring conservation of emissions (useful for debugging/testing)
	/*,(12, "Evap Fuel Vapor Venting", 2, "RPP", 72, "evp_nhtog"),
	(12, "Evap Fuel Vapor Venting", 3, "RPP", 72, "evp_nhtog"),
	(12, "Evap Fuel Vapor Venting", 4, "RPP", 72, "evp_nhtog"),
	(12, "Evap Fuel Vapor Venting", 5, "RPP", 72, "evp_nhtog"),
	(18, "Refueling Displacement Vapor Loss", 1, "RPD", 62, "rfl_nhtog"),
	(19, "Refueling Spillage Loss", 1, "RPD", 62, "rfl_nhtog") */
	;
	
/* -------------------------------------- GET REQUIRED VARS FROM OUTPUT DB ------------------------------------------------ */
-- This section gets the default database info, county, and year used in the MOVES run. Note that this assumes 
--  that there is only year used. If there are multiple, the script will only run the first year.
SET @calendarYear = (SELECT DISTINCT yearID FROM ##outputDB##.movesoutput LIMIT 1);
CREATE TABLE ##newDB##.countylist SELECT DISTINCT countyID AS countyID FROM ##outputDB##.movesoutput; 
	
/* -------------------------------------- GET REQUIRED DATA FROM DEFAULT DB ------------------------------------------------ */
DROP TABLE IF EXISTS ##newDB##.fuelsByCounty;
CREATE TABLE ##newDB##.fuelsByCounty
	SELECT countyID, rc.fuelYearID as yearID, fuelRegionID, monthGroupID as monthID, 
		fuelTypeID, fst.fuelSubtypeID, ff.fuelFormulationID, marketShare
	FROM ##defDB##.fuelsupply fs
	JOIN ##defDB##.regioncounty rc on (regionID = fuelRegionID and rc.fuelYearID = fs.fuelYearID)
	JOIN ##defDB##.fuelformulation ff on (ff.fuelFormulationID = fs.fuelFormulationID)
	JOIN ##defDB##.fuelsubtype fst on (ff.fuelSubtypeID = fst.fuelSubtypeID)
	WHERE rc.countyID = @ctyID and rc.fuelYearID = @calendarYear and regionCodeID = 1 and marketShare > 0;

DROP TABLE IF EXISTS ##newDB##.togspeciation;
CREATE TABLE ##newDB##.togspeciation
	SELECT fuelSubtypeID,regClassID,processID,minModelYearID,maxModelYearID,togSpeciationProfileID 
	FROM ##defDB##.rocspeciation;

DROP TABLE IF EXISTS ##newDB##.pmspeciation;
CREATE TABLE ##newDB##.pmspeciation
	SELECT fuelSubtypeID,regClassID,processID,minModelYearID,maxModelYearID,pmSpeciationProfileID
	FROM ##defDB##.rocspeciation;
	
DROP TABLE ##newDB##.countylist;

/* -------------------------------------- GET MOVES RUN OUTPUT ------------------------------------------------ */
DROP TABLE IF EXISTS ##newDB##.movesoutput_nhtog;
CREATE TABLE ##newDB##.movesoutput_nhtog 
	SELECT yearID, monthID, stateID, countyID, 
		pollutantID, processID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, 
		emissionQuant
	FROM ##outputDB##.movesoutput WHERE pollutantID = 88;

DROP TABLE IF EXISTS ##newDB##.movesoutput_tom;
CREATE TABLE ##newDB##.movesoutput_tom 
	SELECT yearID, monthID, stateID, countyID, 
		pollutantID, processID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, 
		emissionQuant
	FROM ##outputDB##.movesoutput WHERE pollutantID = 123;

DROP TABLE IF EXISTS ##newDB##.movesoutput_residpm;
CREATE TABLE ##newDB##.movesoutput_residpm 
	SELECT yearID, monthID, stateID, countyID, 
		pollutantID, processID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, 
		emissionQuant
	FROM ##outputDB##.movesoutput WHERE pollutantID = 124;
	
/* -------------------------------------- SPECIATE THE NonHAPTOG OUTPUT ------------------------------------------------ */
DROP TABLE IF EXISTS ##newDB##.nonhaptog_byrg_bymy;
CREATE TABLE ##newDB##.nonhaptog_byrg_bymy 
	SELECT pollutantID, yearID, countyID, monthID, regClassID ,modelYearID, SCC, fuelTypeID, processID, roadTypeID, sourceTypeID, 
		sum(emissionQuant) as monthlyEmissions
	FROM ##newDB##.movesoutput_nhtog
	GROUP BY yearID, countyID, monthID, regClassID, modelYearID, SCC, fuelTypeID, processID, roadTypeID, sourceTypeID;

DROP TABLE IF EXISTS ##newDB##.nonhaptog_complete;
CREATE TABLE ##newDB##.nonhaptog_complete
	SELECT *, monthlyEmissions * marketShare as weightedMonthlyEmissions
	FROM ##newDB##.nonhaptog_byrg_bymy 
	JOIN ##newDB##.fuelsByCounty USING (countyID, yearID, monthID, fuelTypeID);

DROP TABLE IF EXISTS ##newDB##.nonhaptog_speciated;
CREATE TABLE ##newDB##.nonhaptog_speciated
	SELECT * FROM ##newDB##.togspeciation
	JOIN (SELECT pollutantID, fuelSubtypeID, processID, countyID, yearID, monthID, fuelTypeID, modelYearID, SCC, roadTypeID,
			sourceTypeID,fuelRegionID, fuelFormulationID, marketShare, monthlyEmissions, weightedMonthlyEmissions,
			CASE WHEN processID = 11 THEN 0 ELSE regClassID END as regClassID,
			regClassID as regClassID_orig
		  FROM ##newDB##.nonhaptog_complete) t2 USING (fuelSubtypeID, regClassID, processID)
	WHERE modelYearID BETWEEN minModelYearID and maxModelYearID; 

DROP TABLE IF EXISTS ##newDB##.nonhaptog_speciated_smoke;
CREATE TABLE ##newDB##.nonhaptog_speciated_smoke
	SELECT *, CONCAT(SUBSTR(SCC, 1, 8), SMOKE_process) as SMOKE_SCC
	FROM ##newDB##.nonhaptog_speciated
	JOIN ##newDB##.SMOKE_MOVES_mapping USING (processID, roadTypeID);
/* ---------------------------------------- CLEANUP NonHAPTOG ----------------------------------------------------------------------- */
DROP TABLE ##newDB##.movesoutput_nhtog;
DROP TABLE ##newDB##.nonhaptog_byrg_bymy;
DROP TABLE ##newDB##.nonhaptog_complete;
DROP TABLE ##newDB##.nonhaptog_speciated;
	
/* -------------------------------------- SPECIATE THE Total Oganic Matter PM OUTPUT ------------------------------------------------ */
DROP TABLE IF EXISTS ##newDB##.tom_byrg_bymy;
CREATE TABLE ##newDB##.tom_byrg_bymy 
	SELECT pollutantID, yearID, countyID, monthID, regClassID ,modelYearID, SCC, fuelTypeID, processID, roadTypeID, sourceTypeID, 
		sum(emissionQuant) as monthlyEmissions
	FROM ##newDB##.movesoutput_tom
	GROUP BY yearID, countyID, monthID, regClassID, modelYearID, SCC, fuelTypeID, processID, roadTypeID, sourceTypeID;

DROP TABLE IF EXISTS ##newDB##.tom_complete;
CREATE TABLE ##newDB##.tom_complete
	SELECT *, monthlyEmissions * marketShare as weightedMonthlyEmissions
	FROM ##newDB##.tom_byrg_bymy 
	JOIN ##newDB##.fuelsByCounty USING (countyID, yearID, monthID, fuelTypeID);

DROP TABLE IF EXISTS ##newDB##.tom_speciated;
CREATE TABLE ##newDB##.tom_speciated
	SELECT * FROM ##newDB##.pmspeciation
	JOIN (SELECT pollutantID, fuelSubtypeID, processID, countyID, yearID, monthID, fuelTypeID, modelYearID, SCC, roadTypeID,
			sourceTypeID,fuelRegionID, fuelFormulationID, marketShare, monthlyEmissions, weightedMonthlyEmissions,
			CASE WHEN processID = 11 THEN 0 ELSE regClassID END as regClassID,
			regClassID as regClassID_orig
		  FROM ##newDB##.tom_complete) t2 USING (fuelSubtypeID, regClassID, processID)
	WHERE modelYearID BETWEEN minModelYearID and maxModelYearID; 

DROP TABLE IF EXISTS ##newDB##.tom_speciated_smoke;
CREATE TABLE ##newDB##.tom_speciated_smoke
	SELECT *, CONCAT(SUBSTR(SCC, 1, 8), SMOKE_process) as SMOKE_SCC
	FROM ##newDB##.tom_speciated
	JOIN ##newDB##.SMOKE_MOVES_mapping USING (processID, roadTypeID);
/* ---------------------------------------- CLEANUP TOM ----------------------------------------------------------------------- */
DROP TABLE ##newDB##.movesoutput_tom;
DROP TABLE ##newDB##.tom_byrg_bymy;
DROP TABLE ##newDB##.tom_complete;
DROP TABLE ##newDB##.tom_speciated;

/* -------------------------------------- SPECIATE THE Residual PM OUTPUT ------------------------------------------------ */
DROP TABLE IF EXISTS ##newDB##.residpm_byrg_bymy;
CREATE TABLE ##newDB##.residpm_byrg_bymy 
	SELECT pollutantID, yearID, countyID, monthID, regClassID ,modelYearID, SCC, fuelTypeID, processID, roadTypeID, sourceTypeID, 
		sum(emissionQuant) as monthlyEmissions
	FROM ##newDB##.movesoutput_residpm
	GROUP BY yearID, countyID, monthID, regClassID, modelYearID, SCC, fuelTypeID, processID, roadTypeID, sourceTypeID;

DROP TABLE IF EXISTS ##newDB##.residpm_complete;
CREATE TABLE ##newDB##.residpm_complete
	SELECT *, monthlyEmissions * marketShare as weightedMonthlyEmissions
	FROM ##newDB##.residpm_byrg_bymy 
	JOIN ##newDB##.fuelsByCounty USING (countyID, yearID, monthID, fuelTypeID);

DROP TABLE IF EXISTS ##newDB##.residpm_speciated;
CREATE TABLE ##newDB##.residpm_speciated
	SELECT * FROM ##newDB##.pmspeciation
	JOIN (SELECT pollutantID, fuelSubtypeID, processID, countyID, yearID, monthID, fuelTypeID, modelYearID, SCC, roadTypeID,
			sourceTypeID,fuelRegionID, fuelFormulationID, marketShare, monthlyEmissions, weightedMonthlyEmissions,
			CASE WHEN processID = 11 THEN 0 ELSE regClassID END as regClassID,
			regClassID as regClassID_orig
		  FROM ##newDB##.residpm_complete) t2 USING (fuelSubtypeID, regClassID, processID)
	WHERE modelYearID BETWEEN minModelYearID and maxModelYearID; 

DROP TABLE IF EXISTS ##newDB##.residpm_speciated_smoke;
CREATE TABLE ##newDB##.residpm_speciated_smoke
	SELECT *, CONCAT(SUBSTR(SCC, 1, 8), SMOKE_process) as SMOKE_SCC 
	FROM ##newDB##.residpm_speciated
	JOIN ##newDB##.SMOKE_MOVES_mapping USING (processID, roadTypeID);
/* ---------------------------------------- CLEANUP Residual PM ----------------------------------------------------------------------- */
DROP TABLE ##newDB##.movesoutput_residpm;
DROP TABLE ##newDB##.residpm_byrg_bymy;
DROP TABLE ##newDB##.residpm_complete;
DROP TABLE ##newDB##.residpm_speciated;

/* -------------------------------------- TRANSFER TO COLLECTED DATABASE NONHAPTOG ------------------------------------------------ */
TRUNCATE ##newDB##.exh_nhtog;
INSERT INTO ##newDB##.exh_nhtog (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, togSpeciationProfileID, profileContribution)
	SELECT t1.countyID, t1.monthID, t1.SMOKE_SCC, t1.SMOKE_mode, 
		t1.pollutantID, 'Residual TOG (NonHAPTOG)' as pollutantName, 
		t1.togSpeciationProfileID,
		COALESCE(SUM(weightedMonthlyEmissions) / NULLIF(total, 0), 0) as profileContribution
	FROM ##newDB##.nonhaptog_speciated_smoke t1
	JOIN (SELECT countyID, yearID, monthID, SMOKE_SCC, SMOKE_mode, 
			sum(weightedMonthlyEmissions) as total 
		  FROM ##newDB##.nonhaptog_speciated_smoke
		  GROUP BY countyID, yearID, monthID, SMOKE_SCC, SMOKE_mode) t2
	WHERE t1.processID in (1,15, 2,16, 17, 90,91) AND
		t1.countyID = t2.countyID AND t1.yearID = t2.yearID AND t1.monthID = t2.monthID AND 
		t1.SMOKE_SCC = t2.SMOKE_SCC AND t1.SMOKE_mode = t2.SMOKE_mode
	GROUP BY t1.monthID, t1.countyID, t1.yearID, t1.monthID, t1.SMOKE_SCC, t1.togspeciationprofileID;

TRUNCATE ##newDB##.epm_nhtog;
INSERT INTO ##newDB##.epm_nhtog (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, togSpeciationProfileID, profileContribution)
	SELECT t1.countyID, t1.monthID, t1.SMOKE_SCC, t1.SMOKE_mode, 
		t1.pollutantID, 'Residual TOG (NonHAPTOG)' as pollutantName, 	
		t1.togSpeciationProfileID, 
		COALESCE(SUM(weightedMonthlyEmissions) / NULLIF(total, 0), 0) as profileContribution
	FROM ##newDB##.nonhaptog_speciated_smoke t1
	JOIN (SELECT countyID, yearID, monthID, SMOKE_SCC, SMOKE_mode, 
			sum(weightedMonthlyEmissions) as total 
		  FROM ##newDB##.nonhaptog_speciated_smoke
		  GROUP BY countyID, yearID, monthID, SMOKE_SCC, SMOKE_mode) t2
	WHERE t1.processID in (11) AND
		t1.countyID = t2.countyID AND t1.yearID = t2.yearID AND t1.monthID = t2.monthID AND 
		t1.SMOKE_SCC = t2.SMOKE_SCC AND t1.SMOKE_mode = t2.SMOKE_mode
	GROUP BY t1.monthID, t1.countyID, t1.yearID, t1.monthID, t1.SMOKE_SCC, t1.togspeciationprofileID;

TRUNCATE ##newDB##.evp_nhtog;
INSERT INTO ##newDB##.evp_nhtog (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, togSpeciationProfileID, profileContribution)
	SELECT t1.countyID, t1.monthID, t1.SMOKE_SCC, t1.SMOKE_mode, 
		t1.pollutantID, 'Residual TOG (NonHAPTOG)' as pollutantName, 	
		t1.togSpeciationProfileID, 
		COALESCE(SUM(weightedMonthlyEmissions) / NULLIF(total, 0), 0) as profileContribution
	FROM ##newDB##.nonhaptog_speciated_smoke t1
	JOIN (SELECT countyID, yearID, monthID, SMOKE_SCC, SMOKE_mode, 
			sum(weightedMonthlyEmissions) as total 
		  FROM ##newDB##.nonhaptog_speciated_smoke
		  GROUP BY countyID, yearID, monthID, SMOKE_SCC, SMOKE_mode) t2
	WHERE t1.processID in (12,13) AND
		t1.countyID = t2.countyID AND t1.yearID = t2.yearID AND t1.monthID = t2.monthID AND 
		t1.SMOKE_SCC = t2.SMOKE_SCC AND t1.SMOKE_mode = t2.SMOKE_mode
	GROUP BY t1.monthID, t1.countyID, t1.yearID, t1.monthID, t1.SMOKE_SCC, t1.togspeciationprofileID;

TRUNCATE ##newDB##.rfl_nhtog;
INSERT INTO ##newDB##.rfl_nhtog (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, togSpeciationProfileID, profileContribution)
	SELECT t1.countyID, t1.monthID, t1.SMOKE_SCC, t1.SMOKE_mode, 
		t1.pollutantID, 'Residual TOG (NonHAPTOG)' as pollutantName, 	
		t1.togSpeciationProfileID, 
		COALESCE(SUM(weightedMonthlyEmissions) / NULLIF(total, 0), 0) as profileContribution
	FROM ##newDB##.nonhaptog_speciated_smoke t1
	JOIN (SELECT countyID, yearID, monthID, SMOKE_SCC, SMOKE_mode, 
			sum(weightedMonthlyEmissions) as total 
		  FROM ##newDB##.nonhaptog_speciated_smoke
		  GROUP BY countyID, yearID, monthID, SMOKE_SCC, SMOKE_mode) t2
	WHERE t1.processID in (18,19) AND
		t1.countyID = t2.countyID AND t1.yearID = t2.yearID AND t1.monthID = t2.monthID AND 
		t1.SMOKE_SCC = t2.SMOKE_SCC AND t1.SMOKE_mode = t2.SMOKE_mode
	GROUP BY t1.monthID, t1.countyID, t1.yearID, t1.monthID, t1.SMOKE_SCC, t1.togspeciationprofileID;
	
-- If the calendar year is before 2010, the starts for diesel source types 51, 61, and 62 in July need to be replaced
-- 	with January data.
REPLACE INTO ##newDB##.exh_nhtog 
	SELECT countyID, moay.monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, togSpeciationProfileID, profileContribution
	FROM ##newDB##.exh_nhtog t1
	JOIN ##defDB##.monthOfAnyYear moay
	WHERE @calendarYear < 2010 
	AND t1.monthID = 1 AND SUBSTR(SMOKE_SCC, 5, 4) = '0172'
	AND SUBSTR(SMOKE_SCC, 3, 2) = 2 AND SUBSTR(SMOKE_SCC, 5, 2) in (51,61,62);

-- Also if the calendar year is before 2010, there is no CNG in July, so we use the January profiles
REPLACE INTO ##newDB##.exh_nhtog 
	SELECT countyID, moay.monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, togSpeciationProfileID, profileContribution
	FROM ##newDB##.exh_nhtog t1
	JOIN ##defDB##.monthofanyyear moay
	WHERE t1.monthID = 1 AND SUBSTR(SMOKE_SCC, 5, 4) = '0172'
	AND SUBSTR(SMOKE_SCC, 3, 2) = 3;
	
/* -------------------------------------- TRANSFER TO COLLECTED DATABASE PM ------------------------------------------------ */
TRUNCATE ##newDB##.exh_tom;
INSERT INTO ##newDB##.exh_tom (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, pmSpeciationProfileID, profileContribution)
	SELECT t1.countyID, t1.monthID, t1.SMOKE_SCC, 'EXH_TOM' as SMOKE_mode, 
		t1.pollutantID, 'Total Organic Matter (TOM)' as pollutantName, 
		t1.pmSpeciationProfileID,  	
		COALESCE(SUM(weightedMonthlyEmissions) / NULLIF(totalTOM, 0), 0) as profileContribution
	FROM ##newDB##.tom_speciated_smoke t1
	JOIN (SELECT countyID, yearID, monthID, SMOKE_SCC, 
			sum(weightedMonthlyEmissions) as totalTOM
		  FROM ##newDB##.tom_speciated_smoke
		  GROUP BY countyID, yearID, monthID, SMOKE_SCC) t2
	WHERE t1.processID in (1,15, 2,16, 17, 90,91) AND
		t1.countyID = t2.countyID AND t1.yearID = t2.yearID AND t1.monthID = t2.monthID AND 
		t1.SMOKE_SCC = t2.SMOKE_SCC
	GROUP BY t1.monthID, t1.countyID, t1.yearID, t1.monthID, t1.SMOKE_SCC, t1.pmSpeciationprofileID;

TRUNCATE ##newDB##.exh_residpm;
INSERT INTO ##newDB##.exh_residpm (countyID, monthID, SMOKE_SCC, SMOKE_mode, pollutantID, pollutantName, pmSpeciationProfileID, profileContribution)
	SELECT t1.countyID, t1.monthID, t1.SMOKE_SCC, 'EXH_PM' as SMOKE_mode, 
		t1.pollutantID, 'Residual PM (NonECNonSO4NonOM)' as pollutantName, 
		t1.pmSpeciationProfileID, 
		COALESCE(SUM(weightedMonthlyEmissions) / NULLIF(totalTOM, 0), 0) as profileContribution
	FROM ##newDB##.residpm_speciated_smoke t1
	JOIN (SELECT countyID, yearID, monthID, SMOKE_SCC, 
			sum(weightedMonthlyEmissions) as totalTOM
		  FROM ##newDB##.residpm_speciated_smoke
		  GROUP BY countyID, yearID, monthID, SMOKE_SCC) t2
	WHERE t1.processID in (1,15, 2,16, 17, 90,91) AND
		t1.countyID = t2.countyID AND t1.yearID = t2.yearID AND t1.monthID = t2.monthID AND 
		t1.SMOKE_SCC = t2.SMOKE_SCC
	GROUP BY t1.monthID, t1.countyID, t1.yearID, t1.monthID, t1.SMOKE_SCC, t1.pmSpeciationprofileID;
	
/* ---------------------- FINAL CLEANUP ----------------------------- */
DROP TABLE ##newDB##.nonhaptog_speciated_smoke;
DROP TABLE ##newDB##.tom_speciated_smoke;
DROP TABLE ##newDB##.residpm_speciated_smoke;
DROP TABLE ##newDB##.fuelsByCounty;
DROP TABLE ##newDB##.togspeciation;
DROP TABLE ##newDB##.pmspeciation;
DROP TABLE ##newDB##.base_schema;

