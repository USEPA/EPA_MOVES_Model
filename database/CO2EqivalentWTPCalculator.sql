-- Version 2008-01-21
-- Purpose: Calculate CO2 Equivalent for Well-Tp-Pump
-- Gwo Shyu, EPA

-- Section Create Remote Tables for Extracted Data
DROP TABLE IF EXISTS CO2EqWTPStep2Pollutant;
CREATE TABLE IF NOT EXISTS CO2EqWTPStep2Pollutant(
	pollutantID		SMALLINT NOT NULL,
	pollutantName		CHAR(50) NULL,
	energyOrMass		CHAR(6) NULL,
	globalWarmingPotential	SMALLINT NULL, 
	PRIMARY KEY (pollutantID)
);
TRUNCATE TABLE CO2EqWTPStep2Pollutant;
-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
SELECT * INTO OUTFILE '##CO2EqWTPStep2Pollutant##' FROM Pollutant;
-- FLUSH TABLES;
-- End Section Extract Data

-- Section Local Data Removal
-- End Section Local Data Removal

-- Section Processing
-- 	SubSection Task 122 step 2 for WTP: Calculate Equivalent CO2 from
-- 					CO2, Methane, and N2O for WTP
-- 	MOVESWorkerOutput mwo INNER JOIN CO2EqWTPStep2Pollutant pol on mwo.pollutantID = pol.pollutantID 

DROP TABLE IF EXISTS MOVESOutputCO2Temp2eq;
CREATE TABLE MOVESOutputCO2Temp2eq 
SELECT 
	mwo.MOVESRunID, mwo.yearID, mwo.monthID, mwo.dayID, 
	mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID, 
	mwo.linkID,##CO2EqpollutantID## AS pollutantID,mwo.processID, 
	mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID, 
	mwo.roadTypeID,mwo.SCC, 
	SUM(mwo.emissionQuant * pol.globalWarmingPotential) AS emissionQuant
FROM 
 	MOVESWorkerOutput mwo INNER JOIN CO2EqWTPStep2Pollutant pol on mwo.pollutantID = pol.pollutantID 
WHERE 	mwo.pollutantID IN (##CO2Step2EqpollutantIDs##) AND ##CO2Step2EqprocessIDs##  
GROUP BY 
	mwo.MOVESRunID,mwo.yearID,mwo.monthID,mwo.dayID, mwo.hourID, 
	mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,mwo.pollutantID, mwo.processID, 
	mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID, mwo.SCC;
-- FLUSH TABLES;
-- 	End of SubSection Task 122 step 2 for WTP:

INSERT INTO MOVESWorkerOutput ( 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant) 
SELECT 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant 
FROM MOVESOutputCO2Temp2eq;
ANALYZE TABLE MOVESWorkerOutput;
-- FLUSH TABLES;

-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS MOVESOutputCO2Temp2eq;
-- End Section Cleanup
