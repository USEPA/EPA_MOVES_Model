-- Version 2013-09-15

-- Section Create Remote Tables for Extracted Data
drop table if exists AdjustFuelSupply;
create table AdjustFuelSupply (
	monthID integer not null,
	fuelTypeID integer not null,
	fuelFormulationID integer not null,
	marketShare double null,
	
	key (monthID, fuelTypeID, fuelFormulationID),
	key (monthID, fuelFormulationID, fuelTypeID),
	key (fuelTypeID, fuelFormulationID, monthID),
	key (fuelTypeID, monthID, fuelFormulationID),
	key (fuelFormulationID, monthID, fuelTypeID),
	key (fuelFormulationID, fuelTypeID, monthID)
) Engine=MEMORY;
truncate table AdjustFuelSupply;

##memory.create.County##;
TRUNCATE TABLE County;

##memory.create.HourDay##;
TRUNCATE TABLE HourDay;

##memory.create.Link##;
TRUNCATE TABLE Link;

##memory.create.Zone##;
TRUNCATE TABLE Zone;

##memory.create.Pollutant##;
TRUNCATE TABLE Pollutant;

##memory.create.EmissionProcess##;
TRUNCATE TABLE EmissionProcess;

##create.EmissionRate##;
TRUNCATE TABLE EmissionRate;

##create.SourceBin##;
TRUNCATE TABLE SourceBin;

##create.SourceBinDistribution##;
TRUNCATE TABLE SourceBinDistribution;

##create.SourceTypeModelYear##;
TRUNCATE TABLE SourceTypeModelYear;

##memory.create.PollutantProcessAssoc##;
TRUNCATE TABLE PollutantProcessAssoc;

-- Section Running Exhaust

##create.SHO##;
TRUNCATE TABLE SHO;

-- End Section Running Exhaust

-- Section Start Exhaust

##create.Starts##;
TRUNCATE TABLE Starts;

-- End Section Start Exhaust

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

cache SELECT MonthOfAnyYear.monthID, FuelSubtype.fuelTypeID, FuelSupply.fuelFormulationID, marketShare
INTO OUTFILE '##AdjustFuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN FuelFormulation ON (FuelFormulation.fuelFormulationID = FuelSupply.fuelFormulationID)
INNER JOIN FuelSubtype ON (FuelSubtype.fuelSubtypeID = FuelFormulation.fuelSubtypeID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)
WHERE FuelSupply.fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND MonthOfAnyYear.monthID = ##context.monthID##;

SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

SELECT Link.*
INTO OUTFILE '##Link##'
FROM Link
WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT * 
INTO OUTFILE '##EmissionProcess##'
FROM EmissionProcess
WHERE processID=##context.iterProcess.databaseKey##;

cache SELECT *
INTO OUTFILE '##Pollutant##'
FROM Pollutant;

-- SELECT DISTINCT SourceBinDistribution.* INTO OUTFILE '??SourceBinDistribution??'
-- FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
-- WHERE polProcessID IN (??pollutantProcessIDs??)
-- AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
-- AND SourceTypeModelYear.modelYearID <= ??context.year??
-- AND SourceTypeModelYear.modelYearID >= ??context.year?? - 30
-- AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
-- AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
-- AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT SourceBinDistribution.* INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30;

-- SELECT DISTINCT SourceBin.* 
-- INTO OUTFILE '??SourceBin??'
-- FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
-- WHERE polProcessID IN (??pollutantProcessIDs??)
-- AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
-- AND SourceTypeModelYear.modelYearID <= ??context.year??
-- AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
-- AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
-- AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT SourceBin.* INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30;

-- SELECT DISTINCT EmissionRate.* 
-- INTO OUTFILE '??EmissionRate??'
-- FROM EmissionRate, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
-- WHERE EmissionRate.polProcessID IN (??pollutantProcessIDs??)
-- AND EmissionRate.polProcessID = SourceBinDistribution.polProcessID
-- AND EmissionRate.SourceBinID = SourceBinDistribution.SourceBinID
-- AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
-- AND SourceTypeModelYear.modelYearID <= ??context.year??
-- AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
-- AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
-- AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT EmissionRate.* INTO OUTFILE '##EmissionRate##'
FROM EmissionRate
WHERE EmissionRate.polProcessID IN (##pollutantProcessIDs##)
AND EmissionRate.sourceBinID in (##macro.csv.all.sourceBinID##);

cache SELECT SourceTypeModelYear.* INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear
WHERE SourceTypeModelYear.sourceTypeID in (##macro.csv.all.sourceTypeID##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT HourDay.* INTO OUTFILE '##HourDay##'
FROM HourDay
WHERE hourDayID in (##macro.csv.all.hourDayID##);

cache SELECT * INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE processID=##context.iterProcess.databaseKey##;

-- Section Running Exhaust

SELECT SHO.* 
INTO OUTFILE '##SHO##'
FROM SHO
WHERE yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##
AND monthID = ##context.monthID##;

-- End Section Running Exhaust

-- Section Start Exhaust

SELECT Starts.* 
INTO OUTFILE '##Starts##'
FROM Starts
WHERE yearID = ##context.year##
AND zoneID = ##context.iterLocation.zoneRecordID##
AND monthID = ##context.monthID##;

-- End Section Start Exhaust

-- End Section Extract Data
--
-- Section Processing

drop table if exists MOVESWorkerOutputTemp;
create table if not exists MOVESWorkerOutputTemp (
	yearID               SMALLINT UNSIGNED NULL,
	monthID              SMALLINT UNSIGNED NULL,
	dayID                SMALLINT UNSIGNED NULL,
	hourID               SMALLINT UNSIGNED NULL,
	stateID              SMALLINT UNSIGNED NULL,
	countyID             INTEGER UNSIGNED NULL,
	zoneID               INTEGER UNSIGNED NULL,
	linkID               INTEGER UNSIGNED NULL,
	pollutantID          SMALLINT UNSIGNED NULL,
	processID            SMALLINT UNSIGNED NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,
	roadTypeID           SMALLINT UNSIGNED NULL,
	SCC                  CHAR(10) NULL,
	emissionQuant        FLOAT NULL
);


--
-- Section Running Exhaust
--
-- Calculate the Running Emissions
--  This Section originally contained a single SQL statement joining 14 tables
--   It was rewritten 10/1/2004 as a series of equivalent statements to improve performance.
--  

DROP TABLE IF EXISTS SHO2 ;
DROP TABLE IF EXISTS Link2 ;
DROP TABLE IF EXISTS SHO3 ;
DROP TABLE IF EXISTS EmissionRate2 ;
DROP TABLE IF EXISTS SourceBinDistribution2 ;
DROP TABLE IF EXISTS SourceBinDistribution3 ;
DROP TABLE IF EXISTS EmissionRate3 ;
DROP TABLE IF EXISTS WorkerOutputBySourceType ;
CREATE TABLE SHO2 (
	yearID SMALLINT,
	monthID SMALLINT,
	hourID SMALLINT,
	dayID SMALLINT,
	linkID INTEGER,
	sourceTypeID SMALLINT,
	modelYearID SMALLINT,
	SHO FLOAT) ;
INSERT INTO SHO2 
	SELECT sho.yearID, sho.monthID, hd.hourID, hd.dayID, sho.linkID, 
		sho.sourceTypeID, (sho.yearID - sho.ageID), sho.SHO
	FROM SHO AS sho INNER JOIN HourDay AS hd USING(hourDayID);
CREATE TABLE Link2 (
	stateID SMALLINT,
	countyID INTEGER,
	zoneID INTEGER,
	linkID INTEGER,
	roadTypeID SMALLINT) Engine=MEMORY;
INSERT INTO Link2
	SELECT	c.stateID, l.countyID, l.zoneID, l.linkID, l.roadTypeID 
	FROM County AS c INNER JOIN Link AS l USING (countyID);
CREATE TABLE SHO3
	SELECT sho.*, l.stateID, l.countyID, l.zoneID, l.roadTypeID 
	FROM SHO2 AS sho INNER JOIN Link2 AS l USING(linkID) ;
CREATE TABLE EmissionRate2
	SELECT er.*, ppa.pollutantID, ppa.processID 
	FROM EmissionRate AS er INNER JOIN PollutantProcessAssoc AS ppa 
	USING (polProcessID)  
	WHERE ppa.pollutantID = 6 AND er.opModeID >= 0 AND er.OpModeID < 100;
CREATE TABLE SourceBinDistribution2
	SELECT sbd.*, sb.fuelTypeID
	FROM SourceBinDistribution AS sbd INNER JOIN SourceBin AS sb USING (sourceBinID);
CREATE TABLE SourceBinDistribution3
	SELECT sbd.*, stmy.sourceTypeID, stmy.modelYearID
	FROM SourceBinDistribution2 AS sbd INNER JOIN SourceTypeModelYear AS stmy
	USING (sourceTypeModelYearID);
CREATE INDEX index1 ON EmissionRate2 (polProcessID, sourceBinID);
CREATE INDEX index1 ON SourceBinDistribution3 (polProcessID, sourceBinID);
CREATE TABLE EmissionRate3 (
	sourceTypeID SMALLINT,
	fuelTypeID SMALLINT,
	modelYearID SMALLINT,
	sourceTypeModelYearID INTEGER,
	pollutantID SMALLINT,
	processID SMALLINT,
	sbafXmbr FLOAT );
INSERT INTO EmissionRate3
	SELECT sbd.sourceTypeID, sbd.fuelTypeID, sbd.modelYearID, sbd.sourceTypeModelYearID,
		er.pollutantID, er.processID, 
		sum(sbd.sourceBinActivityFraction * er.meanBaseRate) AS sbafXmbr
	FROM SourceBinDistribution3 AS sbd INNER JOIN EmissionRate2 AS er
			USING (polProcessID, sourceBinID) 
	GROUP BY sbd.sourceTypeModelYearID, sbd.fuelTypeID, er.pollutantID, er.processID;
CREATE INDEX index1 ON SHO3 (sourceTypeID, modelYearID);
CREATE INDEX index1 ON EmissionRate3 (sourceTypeID, modelYearID);
CREATE TABLE WorkerOutputBySourceType
	SELECT sho.*, er.fuelTypeID, er.pollutantID, er.processID, 
		er.sourceTypeModelYearID, er.sbafXmbr
	FROM SHO3 AS sho INNER JOIN EmissionRate3 AS er 
	  USING (sourceTypeID, modelYearID);
CREATE INDEX index1 ON WorkerOutputBySourceType (sourceTypeModelYearID, fuelTypeID);

INSERT INTO MOVESWorkerOutputTemp (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    emissionQuant)
SELECT
	wobst.yearID,
	wobst.monthID,
	wobst.dayID,
	wobst.hourID,
	wobst.stateID,
	wobst.countyID,
	wobst.zoneID,
	wobst.linkID,
	wobst.pollutantID,
	wobst.processID,
	wobst.sourceTypeID,
	wobst.fuelTypeID,
	wobst.modelYearID,
	wobst.roadTypeID,
	(wobst.sbafXmbr * wobst.sho)
FROM 
	WorkerOutputBySourceType AS wobst;

-- End Section Running Exhaust

--
-- Section Start Exhaust
--
--
-- Calculate the Start Emissions
--

INSERT INTO MOVESWorkerOutputTemp (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    emissionQuant)
SELECT
	st.yearID,
	st.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	z.zoneID,
	l.linkID,
	ppa.pollutantID,
	ppa.processID,
	st.sourceTypeID,
	sb.fuelTypeID,
	stmy.modelYearID,
	l.roadTypeID,
	sum(sbd.sourceBinActivityFraction * st.starts * er.meanBaseRate)
FROM
	SourceBinDistribution sbd,
	starts st,
	emissionRate er,
	county c,
	zone z, 
	link l,
	PollutantProcessAssoc ppa,
	EmissionProcess ep,
	hourDay hd,
	sourceTypeModelYear stmy,
	sourceBin sb
WHERE
	sbd.sourceTypeModelYearID = stmy.sourceTypeModelYearID AND
	sbd.polProcessID = ppa.polProcessID AND
	sbd.polProcessID = er.polProcessID AND
	sbd.sourceBinID = er.sourceBinID AND
	sbd.sourceBinID = sb.sourceBinID AND
	st.hourDayID = hd.hourDayID AND
	st.ageID = (st.yearID - stmy.modelYearID) AND
	st.zoneID = z.zoneID AND
	st.zoneID = l.zoneID AND
	st.sourceTypeID = stmy.sourceTypeID AND
	er.sourceBinID = sb.sourceBinID AND
	er.polProcessID = ppa.polProcessID AND
	er.opModeID = 100 AND
	c.countyID = l.countyID AND
	c.countyID = z.countyID AND
	z.zoneID = l.zoneID AND
	ppa.pollutantID = 6 AND
	ppa.processID = ep.processID AND
	sbd.sourceBinID = sb.sourceBinID
GROUP BY
	st.yearID,
	st.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	z.zoneID,
	l.linkID,
	ppa.pollutantID,
	ppa.processID,
	st.sourceTypeID,
	sb.fuelTypeID,
	stmy.modelYearID,
	l.roadTypeID;

-- End Section Start Exhaust

INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant
FROM MOVESWorkerOutputTemp;

-- End Section Processing

-- Section Cleanup
drop table if exists MOVESWorkerOutputTemp;
DROP TABLE IF EXISTS SHO2 ;
DROP TABLE IF EXISTS Link2 ;
DROP TABLE IF EXISTS SHO3 ;
DROP TABLE IF EXISTS EmissionRate2 ;
DROP TABLE IF EXISTS SourceBinDistribution2 ;
DROP TABLE IF EXISTS SourceBinDistribution3 ;
DROP TABLE IF EXISTS EmissionRate3 ;
DROP TABLE IF EXISTS WorkerOutputBySourceType ;
-- End Section Cleanup
