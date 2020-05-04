-- Version 2013-09-14
-- Purpose: Calculate Atmospheric CO2 emission from Well-To-Pump
-- Modified 2007-07-31 to fix the error of CO2 calculation
-- Gwo Shyu, EPA

-- Section Create Remote Tables for Extracted Data
DROP TABLE IF EXISTS WTPCO2MonthofAnyYear;
CREATE TABLE IF NOT EXISTS WTPCO2MonthofAnyYear (
       	monthID              SMALLINT NOT NULL,
       	monthName            CHAR(10) NULL,
       	noOfDays             SMALLINT NULL,
       	monthGroupID         SMALLINT NOT NULL,
	PRIMARY KEY (monthID)
);
TRUNCATE TABLE WTPCO2MonthofAnyYear;

DROP TABLE IF EXISTS GWTPCO2FactorByFuelType;
CREATE TABLE IF NOT EXISTS GWTPCO2FactorByFuelType ( 
	countyID		INTEGER NOT NULL, 
	yearID			SMALLINT NOT NULL, 
	monthGroupID		SMALLINT NOT NULL, 
	pollutantID		SMALLINT NOT NULL,
	fuelTypeID		SMALLINT NOT NULL,
	sumCO2EmissionRate	FLOAT,
	PRIMARY KEY (countyID, yearID, monthGroupID, pollutantID, fuelTypeID)
);
TRUNCATE TABLE GWTPCO2FactorByFuelType;
-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
DROP TABLE IF EXISTS GWTPCO2FactorByFuelType;
CREATE TABLE IF NOT EXISTS GWTPCO2FactorByFuelType ( 
	countyID		INTEGER NOT NULL, 
	yearID			SMALLINT NOT NULL, 
	monthGroupID		SMALLINT NOT NULL, 
	pollutantID		SMALLINT NOT NULL,
	fuelTypeID		SMALLINT NOT NULL,
	sumCO2EmissionRate	FLOAT,
	PRIMARY KEY (countyID, yearID, monthGroupID, pollutantID, fuelTypeID)
);

INSERT INTO GWTPCO2FactorByFuelType (
	countyID, 
	yearID, 
	monthGroupID, 
	pollutantID, 
	fuelTypeID,
	sumCO2EmissionRate)
SELECT ##context.iterLocation.countyRecordID## as countyID, y.yearID, fs.monthGroupID, 
	##atmoshpericCO2pollutantID## AS pollutantID, fst.fuelTypeID,  
	Sum(fs.marketShare*gwtp.emissionRate) AS sumCO2EmissionRate 
FROM (FuelSupply fs 
INNER JOIN FuelFormulation ff ON ff.fuelFormulationID = fs.fuelFormulationID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID
INNER JOIN greetwelltopump gwtp ON gwtp.fuelSubtypeID = ff.fuelSubtypeID
AND y.yearID = gwtp.yearID)
INNER JOIN fuelsubtype fst ON	fst.fuelSubTypeID=ff.fuelSubTypeID
WHERE gwtp.pollutantID = ##atmoshpericCO2pollutantID##
AND y.yearID = ##context.year##
AND fs.fuelRegionID = ##context.fuelRegionID##
GROUP BY fs.fuelRegionID, y.yearID, fs.monthGroupID, fst.fuelTypeID
LIMIT 10;
-- FLUSH TABLES;

SELECT * INTO OUTFILE '##GWTPCO2FactorByFuelType##' FROM GWTPCO2FactorByFuelType;
-- FLUSH TABLES;

SELECT * INTO OUTFILE '##WTPCO2MonthofAnyYear##' FROM MonthOfAnyYear;
-- FLUSH TABLES;
-- End Section Extract Data

-- Section Local Data Removal
TRUNCATE GWTPCO2FactorByFuelType;
-- End Section Local Data Removal

-- Section Processing
-- 	** SubSection Task 122 step 1b: Calculate atmospheric CO2 from Well-to-Pump
DROP TABLE IF EXISTS MOVESOutputTemp1b;
-- 	7/31/2007 Gwo added "gwtp.monthGroupid=may.monthGroupid AND" in WHERE clause
CREATE TABLE MOVESOutputTemp1b 
SELECT 
	mwo.MOVESRunID, mwo.yearID, mwo.monthID, mwo.dayID, 
	mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID, 
	mwo.linkID, ##atmoshpericCO2pollutantID## AS pollutantID, ##Well-To-PumpID## AS processID, 
	mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID, 
	mwo.roadTypeID,mwo.SCC, 
	SUM(mwo.emissionQuant * gwtp.sumCO2EmissionRate) AS emissionQuant
FROM 
	MOVESWorkerOutput mwo, GWTPCO2FactorByFuelType gwtp, WTPCO2MonthofAnyYear may 
WHERE 
	gwtp.countyID = mwo.countyID AND 
	gwtp.yearID = mwo.yearID AND 
	gwtp.monthGroupid=may.monthGroupid AND
	may.monthID = mwo.monthID AND 
	mwo.pollutantID = ##totalEnergyConsumptionID## AND
	(mwo.processID = ##RunningExhaustID## OR 
	 mwo.processID = ##StartExhaustID## OR 
	 mwo.processID = ##ExtendedIdleExhaustID##) AND
	gwtp.fuelTypeID = mwo.fuelTypeID
GROUP BY 
	mwo.MOVESRunID,mwo.yearID,mwo.monthID,mwo.dayID, mwo.hourID, 
	mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,mwo.pollutantID, mwo.processID, 
	mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID, 
	mwo.SCC;
-- FLUSH TABLES;
-- 	** End of SubSection Task 122 step 1b:

INSERT INTO MOVESWorkerOutput ( 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant) 
SELECT 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant 
FROM MOVESOutputTemp1b;
ANALYZE TABLE MOVESWorkerOutput;
-- FLUSH TABLES;

-- End Section Processing

-- Section Cleanup
-- DROP TABLE IF EXISTS MOVESOutputTemp1b;
-- End Section Cleanup
