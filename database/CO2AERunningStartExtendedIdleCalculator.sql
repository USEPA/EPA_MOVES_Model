-- Version 2014-05-31
-- Purpose: Calculate Atmospheric CO2 and CO2 Equivalent for 
--	    Running, Start, and Extended Idel Exhaust emissions
-- Gwo Shyu, EPA
-- Wesley Faler

-- Section Create Remote Tables for Extracted Data
DROP TABLE IF EXISTS CO2EqPollutant;
CREATE TABLE IF NOT EXISTS CO2EqPollutant(
	pollutantID		SMALLINT NOT NULL,
	pollutantName		CHAR(50) NULL,
	energyOrMass		CHAR(6) NULL,
	globalWarmingPotential	SMALLINT NULL, 
	PRIMARY KEY (pollutantID)
);
TRUNCATE TABLE CO2EqPollutant;

DROP TABLE IF EXISTS CO2MonthofAnyYear;
CREATE TABLE IF NOT EXISTS CO2MonthofAnyYear (
       	monthID              SMALLINT NOT NULL,
       	monthName            CHAR(10) NULL,
       	noOfDays             SMALLINT NULL,
       	monthGroupID         SMALLINT NOT NULL,
	PRIMARY KEY (monthID)
);
TRUNCATE TABLE CO2MonthofAnyYear;

-- DROP TABLE IF EXISTS GWTPCO2FactorByFuelType;
-- CREATE TABLE IF NOT EXISTS GWTPCO2FactorByFuelType ( 
-- 	countyID		INTEGER NOT NULL, 
-- 	yearID			SMALLINT NOT NULL, 
-- 	monthGroupID		SMALLINT NOT NULL, 
-- 	pollutantID		SMALLINT NOT NULL,
-- 	fuelTypeID		SMALLINT NOT NULL,
-- 	sumCO2EmissionRate	FLOAT,
-- 	PRIMARY KEY (countyID, yearID, monthGroupID, pollutantID, fuelTypeID)
-- );
-- TRUNCATE TABLE GWTPCO2FactorByFuelType;

DROP TABLE IF EXISTS CarbonOxidationByFuelType;
CREATE TABLE IF NOT EXISTS CarbonOxidationByFuelType ( 
	countyID		INTEGER NOT NULL, 
	yearID			SMALLINT NOT NULL, 
	monthGroupID		SMALLINT NOT NULL, 
	pollutantID		SMALLINT NOT NULL,
	fuelTypeID		SMALLINT NOT NULL,
	sumcarbonContent	FLOAT,
	sumoxidationFraction	FLOAT,
	PRIMARY KEY (countyID, yearID, monthGroupID, pollutantID, fuelTypeID)
);
TRUNCATE TABLE CarbonOxidationByFuelType;
-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

-- DROP TABLE IF EXISTS GWTPCO2FactorByFuelType;
-- CREATE TABLE IF NOT EXISTS GWTPCO2FactorByFuelType ( 
-- 	countyID		INTEGER NOT NULL, 
-- 	yearID			SMALLINT NOT NULL, 
-- 	monthGroupID		SMALLINT NOT NULL, 
-- 	pollutantID		SMALLINT NOT NULL,
-- 	fuelTypeID		SMALLINT NOT NULL,
-- 	sumCO2EmissionRate	FLOAT,
-- 	PRIMARY KEY (countyID, yearID, monthGroupID, pollutantID, fuelTypeID)
-- );

-- SubSection Task 122 step 1a: Calculate atmospheric CO2 from total energy for running exhaust
DROP TABLE IF EXISTS CarbonOxidationByFuelType;
CREATE TABLE IF NOT EXISTS CarbonOxidationByFuelType ( 
	countyID		INTEGER NOT NULL, 
	yearID			SMALLINT NOT NULL, 
	monthGroupID		SMALLINT NOT NULL, 
	pollutantID		SMALLINT NOT NULL,
	fuelTypeID		SMALLINT NOT NULL,
	sumcarbonContent	FLOAT,
	sumoxidationFraction	FLOAT,
	PRIMARY KEY (countyID, yearID, monthGroupID, pollutantID, fuelTypeID)
);

-- @algorithm sumCarbonContent[countyID,yearID,monthGroupID,pollutantID,fuelTypeID]=sum(marketShare * carbonContent).
-- sumOxidationFraction[countyID,yearID,monthGroupID,pollutantID,fuelTypeID]=sum(marketShare * oxidationFraction).
INSERT INTO CarbonOxidationByFuelType (
	countyID, 
	yearID, 
	monthGroupID, 
	pollutantID, 
	fuelTypeID,
	sumcarbonContent,
	sumoxidationFraction)
SELECT 
	##context.iterLocation.countyRecordID## as countyID, y.yearID, fs.monthGroupID, 
	##atmoshpericCO2pollutantID## AS pollutantID, fst.fuelTypeID,
	Sum(fs.marketShare*fst.carbonContent) AS sumcarbonContent, 
	Sum(fs.marketShare*fst.oxidationFraction) AS sumoxidationFraction
FROM FuelSupply fs
INNER JOIN FuelFormulation ff ON ff.fuelFormulationID = fs.fuelFormulationID
INNER JOIN FuelSubtype fst ON fst.fuelSubtypeID = ff.fuelSubtypeID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID
WHERE y.yearID = ##context.year##
AND fs.fuelRegionID = ##context.fuelRegionID##
GROUP BY fs.fuelRegionID, y.yearID, fs.monthGroupID, fst.fuelTypeID;
-- FLUSH TABLES;

SELECT * INTO OUTFILE '##CarbonOxidationByFuelType##'
FROM CarbonOxidationByFuelType;
-- FLUSH TABLES;
-- End SubSection Task 122 step 1a

SELECT pollutantID,pollutantName,energyOrMass,globalWarmingPotential
INTO OUTFILE '##CO2EqPollutant##' FROM Pollutant where globalWarmingPotential is not null and globalWarmingPotential > 0;

SELECT * INTO OUTFILE '##CO2MonthofAnyYear##' FROM MonthOfAnyYear;
-- FLUSH TABLES;
-- End Section Extract Data

-- Section Local Data Removal
-- TRUNCATE GWTPCO2FactorByFuelType;

-- TRUNCATE CarbonOxidationByFuelType;

-- End Section Local Data Removal

-- Section Processing

-- 	SubSection Task 122 step 1a: Calculate atmospheric CO2 from total energy from
-- 		running, start, and Extended Idle exhaust
DROP TABLE IF EXISTS MOVESOutputCO2Temp1a;
-- 	7/31/2007 Gwo added "coft.monthgroupid=may.monthgroupid AND" in WHERE clause

-- @algorithm Atmosphereic CO2 = sum(Total Energy Consumption * sumCarbonContent * sumOxidationFraction * (44/12)).
CREATE TABLE MOVESOutputCO2Temp1a 
SELECT 
	mwo.MOVESRunID, mwo.yearID, mwo.monthID, mwo.dayID, 
	mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID, 
	mwo.linkID,##atmoshpericCO2pollutantID## AS pollutantID,mwo.processID, 
	mwo.sourceTypeID,mwo.regClassID,mwo.fuelTypeID,mwo.modelYearID, 
	mwo.roadTypeID,mwo.SCC, 
	SUM(mwo.emissionQuant * coft.sumcarbonContent * coft.sumoxidationFraction * (44/12)) AS emissionQuant,
	SUM(mwo.emissionRate  * coft.sumcarbonContent * coft.sumoxidationFraction * (44/12)) AS emissionRate
FROM 
	MOVESWorkerOutput mwo, CarbonOxidationByFuelType coft, CO2MonthofAnyYear may 
WHERE 
	coft.countyID = mwo.countyID AND 
	coft.yearID = mwo.yearID AND 
	coft.monthgroupid=may.monthgroupid AND
	may.monthID = mwo.monthID AND 
	coft.fuelTypeID = mwo.fuelTypeID AND
	mwo.pollutantID = ##totalEnergyConsumptionID## AND
	##CO2Step1AprocessIDs## 
GROUP BY 
	mwo.MOVESRunID,mwo.yearID,mwo.monthID,mwo.dayID, mwo.hourID, 
	mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID, mwo.processID, 
	mwo.sourceTypeID,mwo.regClassID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID, 
	mwo.SCC;
-- FLUSH TABLES;
-- 	End of SubSection Task 122 step 1a:

INSERT INTO MOVESWorkerOutput ( 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant,emissionRate) 
SELECT 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant,emissionRate
FROM MOVESOutputCO2Temp1a;
ANALYZE TABLE MOVESWorkerOutput;
-- FLUSH TABLES;

-- 	SubSection Task 122 step 2: Calculate Equivalent CO2 from CO2, Methane, and N2O
DROP TABLE IF EXISTS MOVESOutputCO2Temp2;

-- @algorithm Equivalent CO2 = sum(emissions[polutant=CO2 or Methane or N2O] * globalWarmingPotential).
CREATE TABLE MOVESOutputCO2Temp2 
SELECT 
	mwo.MOVESRunID, mwo.yearID, mwo.monthID, mwo.dayID, 
	mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID, 
	mwo.linkID,##equivalentCO2pollutantID## AS pollutantID,mwo.processID, 
	mwo.sourceTypeID,mwo.regClassID,mwo.fuelTypeID,mwo.modelYearID, 
	mwo.roadTypeID,mwo.SCC, 
	SUM(mwo.emissionQuant * pol.globalWarmingPotential) AS emissionQuant,
	SUM(mwo.emissionRate  * pol.globalWarmingPotential) AS emissionRate
FROM 
	MOVESWorkerOutput mwo INNER JOIN CO2EqPollutant pol on mwo.pollutantID = pol.pollutantID 
WHERE 	mwo.pollutantID IN (##CO2Step2pollutantIDs##) AND ##CO2Step2processIDs##  
GROUP BY 
	mwo.MOVESRunID,mwo.yearID,mwo.monthID,mwo.dayID, mwo.hourID, 
	mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID, mwo.processID, 
	mwo.sourceTypeID,mwo.regClassID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID, mwo.SCC;
-- FLUSH TABLES;
-- 	End of SubSection Task 122 step 2:

INSERT INTO MOVESWorkerOutput ( 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant,emissionRate) 
SELECT 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant,emissionRate
FROM MOVESOutputCO2Temp2;
ANALYZE TABLE MOVESWorkerOutput;
-- FLUSH TABLES;


-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS MOVESOutputCO2Temp1a;
DROP TABLE IF EXISTS MOVESOutputCO2Temp2;

-- End Section Cleanup
