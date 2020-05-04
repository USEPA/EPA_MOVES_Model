-- Version 2013-09-14

-- Section Create Remote Tables for Extracted Data
CREATE TABLE IF NOT EXISTS WTPMonthofAnyYearCH4N2O (
       monthID              SMALLINT NOT NULL,
       monthName            CHAR(10) NULL,
       noOfDays             SMALLINT NULL,
       monthGroupID         SMALLINT NOT NULL,
	UNIQUE INDEX XPKWTPMonthofAnyYear (
       monthID                        ASC)
);
TRUNCATE TABLE WTPMonthofAnyYearCH4N2O;

DROP TABLE IF EXISTS WTPFactorByFuelTypeCH4N2O;
CREATE TABLE IF NOT EXISTS WTPFactorByFuelTypeCH4N2O ( 
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
TRUNCATE TABLE WTPFactorByFuelTypeCH4N2O;
-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
CREATE TABLE IF NOT EXISTS GREETWellToPumpBoundsCH4N2O ( 
	pollutantID		SMALLINT, 
	fuelSubTypeID	        SMALLINT, 
	minYearID		SMALLINT, 
	maxYearID		SMALLINT, 
	UNIQUE INDEX XPKGREETWellToPumpBoundsCH4N2O ( 
		pollutantID, fuelSubTypeID ) 
);
CREATE TABLE IF NOT EXISTS GREETWellToPumpLoCH4N2O ( 
	pollutantID		SMALLINT, 
	fuelSubTypeID	SMALLINT, 
	yearID			SMALLINT, 
	UNIQUE INDEX XPKGREETWellToPumpLoCH4N2O ( 
		pollutantID, fuelSubTypeID ) 
);
CREATE TABLE IF NOT EXISTS GREETWellToPumpHiCH4N2O ( 
	pollutantID		SMALLINT, 
	fuelSubTypeID	SMALLINT, 
	yearID			SMALLINT, 
	UNIQUE INDEX XPKGREETWellToPumpHiCH4N2O ( 
		pollutantID, fuelSubTypeID ) 
);

DROP TABLE IF EXISTS WTPFactorCH4N2O;
CREATE TABLE IF NOT EXISTS WTPFactorCH4N2O ( 
	pollutantID		SMALLINT, 
	fuelSubTypeID	SMALLINT, 
	yearID			SMALLINT, 
	WTPFactor		FLOAT, 
    WTPFactorV      FLOAT,
	UNIQUE INDEX XPKWTPFactorCH4N2O ( 
		pollutantID, fuelSubTypeID, yearID ) 
);

DROP TABLE IF EXISTS WTPFactorByFuelTypeCH4N2O;
CREATE TABLE IF NOT EXISTS WTPFactorByFuelTypeCH4N2O ( 
	countyID		INTEGER, 
	yearID			SMALLINT, 
	monthGroupID		SMALLINT, 
	pollutantID		SMALLINT, 
	fuelTypeID		SMALLINT,
	WTPFactor		FLOAT,
    WTPFactorCV              FLOAT, 
	UNIQUE INDEX XPKWTPFactorCH4N2O ( 
		countyID, yearID, monthGroupID, pollutantID, fuelTypeID ) 
);

TRUNCATE GREETWellToPumpBoundsCH4N2O;

INSERT INTO GREETWellToPumpBoundsCH4N2O ( 
pollutantID,fuelSubTypeID,minYearID,maxYearID ) 
SELECT pollutantID,fuelSubTypeID,MIN(yearID),MAX(yearID) 
FROM GREETWellToPump
WHERE pollutantID IN ( ##pollutantIDs## ) 
GROUP BY pollutantID, fuelSubTypeID;

TRUNCATE GREETWellToPumpLoCH4N2O;

INSERT INTO GREETWellToPumpLoCH4N2O (pollutantID,fuelSubTypeID,yearID ) 
SELECT pollutantID,fuelSubTypeID,minYearID 
FROM GREETWellToPumpBoundsCH4N2O 
WHERE minYearID >=  ##context.year##
GROUP BY pollutantID,fuelSubTypeID;

INSERT IGNORE INTO GREETWellToPumpLoCH4N2O (pollutantID,fuelSubTypeID,yearID ) 
SELECT pollutantID,fuelSubTypeID,MAX(yearID) 
FROM GREETWellToPump 
WHERE yearID <=  ##context.year## AND 
pollutantID IN ( ##pollutantIDs## ) 
GROUP BY pollutantID,fuelSubTypeID;

TRUNCATE GREETWellToPumpHiCH4N2O;

INSERT INTO GREETWellToPumpHiCH4N2O (pollutantID,fuelSubTypeID,yearID ) 
SELECT pollutantID,fuelSubTypeID,maxYearID 
FROM GREETWellToPumpBoundsCH4N2O 
WHERE maxYearID <=  ##context.year##
GROUP BY pollutantID,fuelSubTypeID;

INSERT IGNORE INTO GREETWellToPumpHiCH4N2O (pollutantID,fuelSubTypeID,yearID ) 
SELECT pollutantID,fuelSubTypeID,MIN(yearID) 
FROM GREETWellToPump
WHERE yearID >  ##context.year## AND 
pollutantID IN ( ##pollutantIDs## ) 
GROUP BY pollutantID,fuelSubTypeID;

INSERT INTO WTPFactorCH4N2O ( 
pollutantID,fuelSubTypeID,yearID,WTPFactor, WTPFactorV) 
SELECT wtpflb.pollutantID,wtpflb.fuelSubTypeID, ##context.year##,
wtpflo.emissionRate + 
(wtpfhi.emissionRate - wtpflo.emissionRate) * 
((##context.year## - wtpflo.yearID)/IF(wtpfhi.yearID<>wtpflo.yearID,wtpfhi.yearID-wtpflo.yearID,1)),
null
FROM GREETWellToPump wtpflo,GREETWellToPumpLoCH4N2O wtpflb, 
GREETWellToPump wtpfhi,GREETWellToPumpHiCH4N2O wtpfhb 
WHERE 
wtpflb.pollutantID = wtpfhb.pollutantID AND 
wtpflb.fuelSubTypeID = wtpfhb.fuelSubTypeID AND 
wtpflb.pollutantID = wtpflo.pollutantID AND 
wtpflb.fuelSubTypeID = wtpflo.fuelSubTypeID AND 
wtpflb.yearID = wtpflo.yearID AND 
wtpfhb.pollutantID = wtpfhi.pollutantID AND 
wtpfhb.fuelSubTypeID = wtpfhi.fuelSubTypeID AND 
wtpfhb.yearID = wtpfhi.yearID;

INSERT INTO WTPFactorByFuelTypeCH4N2O ( 
countyID, yearID, monthGroupID, pollutantID, fuelTypeID, WTPFactor, WTPFactorCV)
SELECT 
##context.iterLocation.countyRecordID## as countyID, y.yearID, fs.monthGroupID, wf.pollutantID,
fst.fuelTypeID, SUM(wf.WTPFactor * fs.marketShare), null
FROM 
FuelSubtype fst, FuelFormulation ff, FuelSupply fs, Year y, WTPFactorCH4N2O wf 
WHERE 
fst.fuelSubTypeID = wf.fuelSubTypeID AND 
ff.fuelSubTypeID = fst.fuelSubtypeID AND
fs.fuelFormulationID = ff.fuelFormulationID AND
fs.fuelYearID = y.fuelYearID AND
y.yearID = wf.yearID AND
fs.fuelRegionID = ##context.fuelRegionID##
GROUP BY 
fs.fuelRegionID,y.yearID,fs.monthGroupID,wf.pollutantID,fst.fuelTypeID;

SELECT * INTO OUTFILE '##WTPFactorByFuelTypeCH4N2O##'
FROM WTPFactorByFuelTypeCH4N2O;

SELECT * INTO OUTFILE '##WTPMonthOfAnyYearCH4N2O##'
FROM MonthOfAnyYear;
-- End Section Extract Data

-- Section Local Data Removal
TRUNCATE GREETWellToPumpBoundsCH4N2O;
TRUNCATE GREETWellToPumpLoCH4N2O;
TRUNCATE GREETWellToPumpHiCH4N2O;
TRUNCATE WTPFactorCH4N2O;
TRUNCATE WTPFactorByFuelTypeCH4N2O;
-- End Section Local Data Removal

-- Section Processing
DROP TABLE IF EXISTS MOVESOutputTemp;

CREATE TABLE MOVESOutputTemp 
SELECT 
	mwo.yearID, mwo.monthID, mwo.dayID, 
	mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID, 
	mwo.linkID,wfft.pollutantID,99 AS processID, 
	mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID, 
	mwo.roadTypeID,mwo.SCC, 
	SUM(mwo.emissionQuant * wfft.WTPFactor) AS emissionQuant
FROM 
	MOVESWorkerOutput mwo, WTPFactorByFuelTypeCH4N2O wfft, WTPMonthOfAnyYearCH4N2O may 
WHERE 
	wfft.countyID = mwo.countyID AND 
	wfft.yearID = mwo.yearID AND 
	may.monthID = mwo.monthID AND 
	wfft.monthGroupID = may.monthGroupID AND 
	mwo.pollutantID = 91 AND
	mwo.fuelTypeID = wfft.fuelTypeID AND
	mwo.processID <> 99
GROUP BY 
	mwo.yearID,mwo.monthID,mwo.dayID, mwo.hourID, 
	mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,wfft.pollutantID, 
	mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID, 
	mwo.SCC;

INSERT INTO MOVESWorkerOutput ( 
	yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant) 
SELECT 
	yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant 
FROM 
	MOVESOutputTemp;
-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS MOVESOutputTemp;
-- End Section Cleanup
