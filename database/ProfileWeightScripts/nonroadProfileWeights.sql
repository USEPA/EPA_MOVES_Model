CREATE DATABASE IF NOT EXISTS ##newDB##;

/* -------------------------------------- PREP COLLECTOR DATABASE IF NECESSARY ------------------------------------------------ */
-- create a runtime table for use to aid joints
CREATE TABLE IF NOT EXISTS ##newDB##.movesoutput (
	yearID					SMALLINT(5),
	monthID 				SMALLINT(5),
	countyID 				INT(10),
	pollutantID 			SMALLINT(5),
	processID	 			SMALLINT(5),
	fuelSubtypeID			SMALLINT(5),
	SCC		 				VARCHAR(10),
	engTechID 				SMALLINT(5),
	hpID	 				SMALLINT(5),
	tierID	 				SMALLINT(5),
	strokes	 				SMALLINT(5),
	emissionQuant			DOUBLE,
	PRIMARY KEY (yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes)
);

-- create one table each for NonHAPTOG and PM
CREATE TABLE IF NOT EXISTS ##newDB##.movesoutput_pm (
	yearID					SMALLINT(5),
	monthID 				SMALLINT(5),
	countyID 				INT(10),
	pollutantID 			SMALLINT(5),
	pollutantName 			VARCHAR(50),
	processID	 			SMALLINT(5),
	fuelSubtypeID			SMALLINT(5),
	SCC		 				VARCHAR(10),
	engTechID 				SMALLINT(5),
	hpID	 				SMALLINT(5),
	tierID	 				SMALLINT(5),
	strokes	 				SMALLINT(5),
	emissionQuant			DOUBLE,
	pmSpeciationProfileID	VARCHAR(10),
	CROCOMRatio 			DOUBLE,
	PRIMARY KEY (yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes)
);

CREATE TABLE IF NOT EXISTS ##newDB##.movesoutput_nhtog (
	yearID					SMALLINT(5),
	monthID 				SMALLINT(5),
	countyID 				INT(10),
	pollutantID 			SMALLINT(5),
	pollutantName 			VARCHAR(50),
	processID	 			SMALLINT(5),
	fuelSubtypeID			SMALLINT(5),
	SCC		 				VARCHAR(10),
	engTechID 				SMALLINT(5),
	hpID	 				SMALLINT(5),
	tierID	 				SMALLINT(5),
	strokes	 				SMALLINT(5),
	emissionQuant			DOUBLE,
	togSpeciationProfileID	VARCHAR(10),
	GROCNMOGRatio 			DOUBLE,
	PRIMARY KEY (yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes)
);

/* -------------------------------------- PROCESS NONHAPTOG ------------------------------------------------ */
TRUNCATE ##newDB##.movesoutput;
INSERT INTO ##newDB##.movesoutput 
		  (yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes, emissionQuant)
	SELECT yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes,
		sum(emissionQuant * dayID * noOfDays / 7) as emissionQuant
	FROM ##outputDB##.movesoutput 
	JOIN ##defDB##.monthofanyyear using (monthID)
	JOIN ##defDB##.enginetech using (engTechID)
	WHERE pollutantID = 88 AND emissionQuant > 0
	GROUP BY yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes;

-- add the speciation profile and GROC factor
INSERT INTO ##newDB##.movesoutput_nhtog 
	SELECT yearID, monthID, countyID, pollutantID, CONCAT('NONHAPTOG',togSpeciationProfileID) as pollutantName, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes, emissionQuant, togSpeciationProfileID, GROCNMOGRatio 
	FROM ##newDB##.movesoutput
	JOIN ##defDB##.nrrocspeciation USING (fuelSubtypeID, engTechID, tierID, strokes, processID);

/* -------------------------------------- PROCESS PM ------------------------------------------------ */
TRUNCATE ##newDB##.movesoutput;
INSERT INTO ##newDB##.movesoutput 
		  (yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes, emissionQuant)
	SELECT yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes,
		sum(emissionQuant * dayID * noOfDays / 7) as emissionQuant
	FROM ##outputDB##.movesoutput 
	JOIN ##defDB##.monthofanyyear using (monthID)
	JOIN ##defDB##.enginetech using (engTechID)
	WHERE pollutantID = 110 AND emissionQuant > 0
	GROUP BY yearID, monthID, countyID, pollutantID, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes;

-- add the speciation profile and GROC factor
INSERT INTO ##newDB##.movesoutput_pm
	SELECT yearID, monthID, countyID, pollutantID, CONCAT('PM2_5',pmSpeciationProfileID) as pollutantName, processID, fuelSubtypeID, SCC, engTechID, hpID, tierID, strokes, emissionQuant, pmSpeciationProfileID, CROCOMRatio
	FROM ##newDB##.movesoutput
	JOIN ##defDB##.nrrocspeciation USING (fuelSubtypeID, engTechID, tierID, strokes, processID);
	
-- cleanup
DROP TABLE ##newDB##.movesoutput;

