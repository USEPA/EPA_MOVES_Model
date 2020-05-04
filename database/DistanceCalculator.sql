-- Version 2014-06-10

-- @algorithm
-- @owner Distance Calculator

-- Section Create Remote Tables for Extracted Data
##create.County##;
TRUNCATE TABLE County;

##create.HourDay##;
TRUNCATE TABLE HourDay;

##create.Link##;
TRUNCATE TABLE Link;

##create.SourceBin##;
TRUNCATE TABLE SourceBin;

##create.SourceBinDistribution##;
TRUNCATE TABLE SourceBinDistribution;

##create.SourceTypeModelYear##;
TRUNCATE TABLE SourceTypeModelYear;

##create.EmissionProcess##;
TRUNCATE TABLE EmissionProcess;

##create.SHO##;
TRUNCATE TABLE SHO;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

SELECT Link.*
INTO OUTFILE '##Link##'
FROM Link
WHERE linkID = ##context.iterLocation.linkRecordID##;

SELECT * 
INTO OUTFILE '##EmissionProcess##'
FROM EmissionProcess
WHERE processID=##context.iterProcess.databaseKey##;

cache SELECT DISTINCT SourceBinDistribution.* 
INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT SourceBin.* 
INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

SELECT DISTINCT SHO.* 
INTO OUTFILE '##SHO##'
FROM SHO, RunSpecMonth, RunSpecDay, RunSpecHour, HourDay
WHERE yearID = ##context.year##
AND SHO.linkID = ##context.iterLocation.linkRecordID##
AND SHO.monthID=RunSpecMonth.monthID
AND SHO.hourDayID = HourDay.hourDayID
AND HourDay.dayID = RunSpecDay.dayID
AND HourDay.hourID = RunSpecHour.hourID;

cache SELECT SourceTypeModelYear.* 
INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear,RunSpecSourceType
WHERE SourceTypeModelYear.sourceTypeID = RunSpecSourceType.sourceTypeID
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT DISTINCT HourDay.* 
INTO OUTFILE '##HourDay##'
FROM HourDay,RunSpecHour,RunSpecDay
WHERE HourDay.dayID = RunSpecDay.dayID
AND HourDay.hourID = RunSpecHour.hourID;

-- End Section Extract Data
--
-- Section Processing
--

TRUNCATE MOVESWorkerActivityOutput;
--
-- Calculate the distance
--
DROP TABLE IF EXISTS SBD2;
DROP TABLE IF EXISTS SVTD2;
DROP TABLE IF EXISTS SVTD3;
DROP TABLE IF EXISTS DistFracts;
DROP TABLE IF EXISTS SHO2;
DROP TABLE IF EXISTS Link2;
DROP TABLE IF EXISTS SHO3;

-- @algorithm fuelTypeActivityFraction[sourceTypeID,modelYearID,regClassID,fuelTypeID] = sum(sourceBinActivityFraction[processID=1,pollutantID=any,sourceBinID[fuelTypeID,engTechID,regClassID,modelYearGroupID,engSizeID]]).
CREATE TABLE SBD2 (
	sourceTypeModelYearID INTEGER,
	regClassID SMALLINT,
	fuelTypeID SMALLINT,
	fuelTypeActivityFraction FLOAT);
INSERT INTO SBD2 (
	sourceTypeModelYearID,regClassID,fuelTypeID,fuelTypeActivityFraction )
	SELECT sbd.sourceTypeModelYearID,sb.regClassID,sb.fuelTypeID,
		sum(sbd.sourceBinActivityFraction)
	FROM sourceBinDistribution AS sbd INNER JOIN SourceBin AS sb
	USING (sourceBinID)
	GROUP BY sourceTypeModelYearID, regClassID, fuelTypeID;
CREATE INDEX index1 ON SBD2 (sourceTypeModelYearID, fuelTypeID);

-- @algorithm Add sourceTypeModelYearID to fuelTypeActivityFraction's dimensions.
CREATE TABLE DistFracts
	SELECT stmy.sourceTypeModelYearID, stmy.sourceTypeID, sbd.regClassID, stmy.modelYearID, sbd.fuelTypeID,
	sbd.fuelTypeActivityFraction
	FROM SourceTypeModelYear AS stmy INNER JOIN SBD2 AS sbd USING (sourceTypeModelYearID);

-- @algorithm Add modelYearID to SHO's dimensions.
CREATE TABLE SHO2 (
	yearID SMALLINT, 
	monthID SMALLINT,
	dayID SMALLINT,
	hourID SMALLINT,
	modelYearID SMALLINT,
	linkID INTEGER,
	sourceTypeID SMALLINT,
	distance FLOAT);
INSERT INTO SHO2
	SELECT sho.yearID, sho.monthID, hd.dayID, hd.hourID, (sho.yearID - sho.ageID), 
		sho.linkID, sho.sourceTypeID, sho.distance
	FROM SHO AS sho INNER JOIN HourDay AS hd USING (hourDayID);
	
CREATE TABLE Link2
	SELECT link.*, c.stateID
	FROM Link AS link INNER JOIN County AS c USING (countyID);
	
CREATE INDEX index1 ON SHO2 (linkID);
CREATE TABLE SHO3
	SELECT sho.*, link.stateID, link.countyID, link.zoneID, link.roadTypeID 
	FROM SHO2 AS sho INNER JOIN Link2 AS link USING (linkID);
	
CREATE INDEX index1 ON SHO3 (sourceTypeID, modelYearID, roadTypeID);

-- @algorithm distance = distance[sourceTypeID,yearID,monthID,hourDayID,ageID,linkID]*fuelTypeActivityFraction.
INSERT INTO MOVESWorkerActivityOutput (
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	regClassID,sourceTypeID,fuelTypeID,modelYearID,
	roadTypeID,
	activityTypeID,
	activity) 
SELECT
	sho.yearID,sho.monthID,sho.dayID,sho.hourID,
	sho.stateID,sho.countyID,sho.zoneID,sho.linkID,
	df.regClassID,sho.sourceTypeID,df.fuelTypeID,sho.modelYearID,
	sho.roadTypeID,
	1,
	(sho.distance * df.fuelTypeActivityFraction) as activity
FROM DistFracts AS df 
INNER JOIN SHO3 AS sho USING (sourceTypeID, modelYearID);

-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS SBD2;
DROP TABLE IF EXISTS SVTD2;
DROP TABLE IF EXISTS SVTD3;
DROP TABLE IF EXISTS DistFracts;
DROP TABLE IF EXISTS SHO2;
DROP TABLE IF EXISTS Link2;
DROP TABLE IF EXISTS SHO3;
-- End Section Cleanup
