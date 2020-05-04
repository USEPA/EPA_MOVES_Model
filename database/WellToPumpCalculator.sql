-- Version 2013-09-15

-- @notused

-- Section Create Remote Tables for Extracted Data
CREATE TABLE IF NOT EXISTS WTPMonthofAnyYear (
       monthID              SMALLINT NOT NULL,
       monthName            CHAR(10) NULL,
       noOfDays             SMALLINT NULL,
       monthGroupID         SMALLINT NOT NULL,
	UNIQUE INDEX XPKWTPMonthofAnyYear (
       monthID                        ASC)
);
TRUNCATE TABLE WTPMonthofAnyYear;

DROP TABLE IF EXISTS WTPFactorByFuelType;
CREATE TABLE IF NOT EXISTS WTPFactorByFuelType ( 
	countyID		INTEGER, 
	yearID			SMALLINT, 
	monthGroupID	SMALLINT, 
	pollutantID		SMALLINT, 
	fuelTypeID		SMALLINT,
	WTPFactor		FLOAT,
    WTPFactorCV     FLOAT, 
	UNIQUE INDEX XPKWTPFactor ( 
		countyID, yearID, monthGroupID, pollutantID, fuelTypeID ) 
);
TRUNCATE TABLE WTPFactorByFuelType;
-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
CREATE TABLE IF NOT EXISTS GREETWellToPumpBounds ( 
	pollutantID		SMALLINT, 
	fuelSubTypeID	SMALLINT, 
	minYearID		SMALLINT, 
	maxYearID		SMALLINT, 
	UNIQUE INDEX XPKGREETWellToPumpBounds ( 
		pollutantID, fuelSubTypeID ) 
);
CREATE TABLE IF NOT EXISTS GREETWellToPumpLo ( 
	pollutantID		SMALLINT, 
	fuelSubTypeID	SMALLINT, 
	yearID			SMALLINT, 
	UNIQUE INDEX XPKGREETWellToPumpLo ( 
		pollutantID, fuelSubTypeID ) 
);
CREATE TABLE IF NOT EXISTS GREETWellToPumpHi ( 
	pollutantID		SMALLINT, 
	fuelSubTypeID	SMALLINT, 
	yearID			SMALLINT, 
	UNIQUE INDEX XPKGREETWellToPumpHi ( 
		pollutantID, fuelSubTypeID ) 
);

DROP TABLE IF EXISTS WTPFactor;
CREATE TABLE IF NOT EXISTS WTPFactor ( 
	pollutantID		SMALLINT, 
	fuelSubTypeID	SMALLINT, 
	yearID			SMALLINT, 
	WTPFactor		FLOAT, 
    WTPFactorV      FLOAT,
	UNIQUE INDEX XPKWTPFactor ( 
		pollutantID, fuelSubTypeID, yearID ) 
);

DROP TABLE IF EXISTS WTPFactorByFuelType;
CREATE TABLE IF NOT EXISTS WTPFactorByFuelType ( 
	countyID		INTEGER, 
	yearID			SMALLINT, 
	monthGroupID	SMALLINT, 
	pollutantID		SMALLINT, 
	fuelTypeID		SMALLINT,
	WTPFactor		FLOAT,
    WTPFactorCV              FLOAT, 
	UNIQUE INDEX XPKWTPFactor ( 
		countyID, yearID, monthGroupID, pollutantID, fuelTypeID ) 
);

TRUNCATE GREETWellToPumpBounds;

INSERT INTO GREETWellToPumpBounds ( 
pollutantID,fuelSubTypeID,minYearID,maxYearID )
SELECT pollutantID,fuelSubTypeID,MIN(yearID),MAX(yearID) 
FROM GREETWellToPump 
WHERE pollutantID IN ( ##pollutantIDs## ) 
GROUP BY pollutantID, fuelSubTypeID;

TRUNCATE GREETWellToPumpLo;

INSERT INTO GREETWellToPumpLo (pollutantID,fuelSubTypeID,yearID ) 
SELECT pollutantID,fuelSubTypeID,minYearID 
FROM GREETWellToPumpBounds 
WHERE minYearID >=  ##context.year##
GROUP BY pollutantID,fuelSubTypeID;

INSERT IGNORE INTO GREETWellToPumpLo (pollutantID,fuelSubTypeID,yearID ) 
SELECT pollutantID,fuelSubTypeID,MAX(yearID) 
FROM GREETWellToPump 
WHERE yearID <=  ##context.year## AND 
pollutantID IN ( ##pollutantIDs## ) 
GROUP BY pollutantID,fuelSubTypeID;

TRUNCATE GREETWellToPumpHi;
INSERT INTO GREETWellToPumpHi (pollutantID,fuelSubTypeID,yearID ) 
SELECT pollutantID,fuelSubTypeID,maxYearID 
FROM GREETWellToPumpBounds 
WHERE maxYearID <=  ##context.year##
GROUP BY pollutantID,fuelSubTypeID;

INSERT IGNORE INTO GREETWellToPumpHi (pollutantID,fuelSubTypeID,yearID ) 
SELECT pollutantID,fuelSubTypeID,MIN(yearID) 
FROM GREETWellToPump 
WHERE yearID >  ##context.year## AND 
pollutantID IN ( ##pollutantIDs## ) 
GROUP BY pollutantID,fuelSubTypeID;

INSERT INTO WTPFactor ( 
pollutantID,fuelSubTypeID,yearID,WTPFactor, WTPFactorV) 
SELECT wtpflb.pollutantID,wtpflb.fuelSubTypeID, ##context.year##,
wtpflo.emissionRate + 
(wtpfhi.emissionRate - wtpflo.emissionRate) * 
((##context.year## - wtpflo.yearID)/IF(wtpfhi.yearID<>wtpflo.yearID,wtpfhi.yearID-wtpflo.yearID,1)),
null
FROM GREETWellToPump wtpflo,GREETWellToPumpLo wtpflb, 
GREETWellToPump wtpfhi,GREETWellToPumpHi wtpfhb 
WHERE 
wtpflb.pollutantID = wtpfhb.pollutantID AND 
wtpflb.fuelSubTypeID = wtpfhb.fuelSubTypeID AND 
wtpflb.pollutantID = wtpflo.pollutantID AND 
wtpflb.fuelSubTypeID = wtpflo.fuelSubTypeID AND 
wtpflb.yearID = wtpflo.yearID AND 
wtpfhb.pollutantID = wtpfhi.pollutantID AND 
wtpfhb.fuelSubTypeID = wtpfhi.fuelSubTypeID AND 
wtpfhb.yearID = wtpfhi.yearID;

INSERT INTO WTPFactorByFuelType ( 
countyID,yearID,monthGroupID,pollutantID,fuelTypeID,WTPFactor,WTPFactorCV) 
SELECT 
##context.iterLocation.countyRecordID## as countyID,y.yearID,fs.monthGroupID, 
wf.pollutantID,fst.fuelTypeID,SUM(wf.WTPFactor * fs.marketShare), null 
FROM 
FuelSubtype fst, FuelFormulation ff, Year y, FuelSupply fs, WTPFactor wf 
WHERE 
fs.fuelYearID = y.fuelYearID AND
y.yearID = wf.yearID AND
fs.fuelRegionID = ##context.fuelRegionID## AND
fs.fuelFormulationID = ff.fuelFormulationID AND
ff.fuelSubTypeID = wf.fuelSubTypeID AND 
ff.fuelSubTypeID = fst.fuelSubtypeID
GROUP BY 
fs.fuelRegionID,y.yearID,fs.monthGroupID,wf.pollutantID,fst.fuelTypeID;

SELECT * INTO OUTFILE '##WTPFactorByFuelType##'
FROM WTPFactorByFuelType;

SELECT * INTO OUTFILE '##WTPMonthOfAnyYear##'
FROM MonthOfAnyYear;
-- End Section Extract Data

-- Section Local Data Removal
TRUNCATE GREETWellToPumpBounds;
TRUNCATE GREETWellToPumpLo;
TRUNCATE GREETWellToPumpHi;
TRUNCATE WTPFactor;
TRUNCATE WTPFactorByFuelType;
-- End Section Local Data Removal

-- Section Processing
DROP TABLE IF EXISTS MOVESOutputTemp;

CREATE TABLE MOVESOutputTemp 
SELECT 
	mwo.MOVESRunID, mwo.yearID, mwo.monthID, mwo.dayID, 
	mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID, 
	mwo.linkID,wfft.pollutantID,99 AS processID, 
	mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID, 
	mwo.roadTypeID,mwo.SCC,
	SUM(mwo.emissionQuant) * wfft.WTPFactor AS emissionQuant
FROM
	MOVESWorkerOutput mwo, WTPFactorByFuelType wfft, WTPMonthOfAnyYear may 
WHERE 
	wfft.countyID = mwo.countyID AND 
	wfft.yearID = mwo.yearID AND 
	may.monthID = mwo.monthID AND 
	wfft.monthGroupID = may.monthGroupID AND 
	mwo.pollutantID = 91 AND
	mwo.fuelTypeID = wfft.fuelTypeID AND
	mwo.processID <> 99
GROUP BY 
	mwo.MOVESRunID,mwo.yearID,mwo.monthID,mwo.dayID, mwo.hourID, 
	mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,wfft.pollutantID, 
	mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID, 
	mwo.SCC;

INSERT INTO MOVESWorkerOutput ( 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant) 
SELECT 
	MOVESRunID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant 
FROM 
	MOVESOutputTemp;
-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS MOVESOutputTemp;
-- End Section Cleanup
