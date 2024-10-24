-- Author Wesley Faler
-- Version 2013-09-15

-- @notused

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

-- Section UseDioxinEmissionRate
CREATE TABLE dioxinemissionrate (
  processID smallint(6) NOT NULL,
  pollutantID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL DEFAULT '0',
  modelYearID smallint(6) NOT NULL,
  meanBaseRate double DEFAULT NULL,
  PRIMARY KEY (fuelTypeID,modelYearID,processID,pollutantID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

truncate table dioxinEmissionRate;
-- End Section UseDioxinEmissionRate

-- Section UseMetalEmissionRate
CREATE TABLE metalemissionrate (
  processID smallint(6) NOT NULL,
  pollutantID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL DEFAULT '0',
  sourceTypeID smallint(6) NOT NULL DEFAULT '0',
  modelYearID smallint(6) NOT NULL,
  meanBaseRate double DEFAULT NULL,
  PRIMARY KEY (sourceTypeID,fuelTypeID,modelYearID,processID,pollutantID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

truncate table metalEmissionRate;
-- End Section UseMetalEmissionRate

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

cache SELECT Link.*
INTO OUTFILE '##Link##'
FROM Link
WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT * 
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
AND SourceTypeModelYear.modelYearID >= ##context.year## - 40
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT SourceBin.* 
INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 40
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT SHO.* 
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
AND modelYearID >= ##context.year## - 40;

SELECT DISTINCT HourDay.* 
INTO OUTFILE '##HourDay##'
FROM HourDay,RunSpecHour,RunSpecDay
WHERE HourDay.dayID = RunSpecDay.dayID
AND HourDay.hourID = RunSpecHour.hourID;

-- Section UseDioxinEmissionRate

-- native distance units are miles
-- native mass units are grams
-- native energy units are kilojoules

cache select ppa.processID, ppa.pollutantID, r.fuelTypeID, modelYearID, 
	meanBaseRate * (
		case units when 'g/mile' then 1.0
			when 'g/km' then 1.609344
			when 'TEQ/mile' then 1.0
			when 'TEQ/km' then 1.609344
			else 1.0
		end
	) as meanBaseRate
into outfile '##dioxinEmissionRate##'
from dioxinEmissionRate r
inner join pollutantProcessAssoc ppa using (polProcessID)
inner join modelYear my on (
	MYMAP(modelYearID) >= round(modelYearGroupID/10000,0)
	and MYMAP(modelYearID) <= mod(modelYearGroupID,10000)
	and modelYearID <= ##context.year##
	and modelYearID >= ##context.year## - 40
)
where polProcessID in (##outputDioxinEmissionRate##);
-- End Section UseDioxinEmissionRate

-- Section UseMetalEmissionRate

-- native distance units are miles
-- native mass units are grams
-- native energy units are kilojoules

cache select ppa.processID, ppa.pollutantID, r.fuelTypeID, r.sourceTypeID, modelYearID,
	meanBaseRate * (
		case units when 'g/mile' then 1.0
			when 'g/km' then 1.609344
			when 'TEQ/mile' then 1.0
			when 'TEQ/km' then 1.609344
			else 1.0
		end
	) as meanBaseRate
into outfile '##metalEmissionRate##'
from metalEmissionRate r
inner join RunSpecSourceFuelType rs on (
	rs.sourceTypeID = r.sourceTypeID
	and rs.fuelTypeID = r.fuelTypeID)
inner join pollutantProcessAssoc ppa using (polProcessID)
inner join modelYear my on (
	MYMAP(modelYearID) >= round(modelYearGroupID/10000,0)
	and MYMAP(modelYearID) <= mod(modelYearGroupID,10000)
	and modelYearID <= ##context.year##
	and modelYearID >= ##context.year## - 40
)
where polProcessID in (##outputMetalEmissionRate##);
-- End Section UseMetalEmissionRate

-- End Section Extract Data
--
-- Section Processing
--

drop table if exists ATActivityOutput;
create table ATActivityOutput like MOVESWorkerActivityOutput;

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

CREATE TABLE SBD2 (
	sourceTypeModelYearID INTEGER,
	fuelTypeID SMALLINT,
	fuelTypeActivityFraction FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO SBD2 (
	sourceTypeModelYearID,fuelTypeID,fuelTypeActivityFraction )
	SELECT sbd.sourceTypeModelYearID,sb.fuelTypeID,
		sum(sbd.sourceBinActivityFraction)
	FROM sourceBinDistribution AS sbd INNER JOIN SourceBin AS sb
	USING (sourceBinID)
	GROUP BY sourceTypeModelYearID, fuelTypeID;
CREATE INDEX index1 ON SBD2 (sourceTypeModelYearID, fuelTypeID);

CREATE TABLE DistFracts
	SELECT stmy.sourceTypeModelYearID, stmy.sourceTypeID, stmy.modelYearID, sbd.fuelTypeID,
	sbd.fuelTypeActivityFraction
	FROM SourceTypeModelYear AS stmy INNER JOIN SBD2 AS sbd USING (sourceTypeModelYearID);
	
CREATE TABLE SHO2 (
	yearID SMALLINT, 
	monthID SMALLINT,
	dayID SMALLINT,
	hourID SMALLINT,
	modelYearID SMALLINT,
	linkID INTEGER,
	sourceTypeID SMALLINT,
	distance FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
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

INSERT INTO ATActivityOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    activityTypeID,
    activity) 
SELECT
	sho.yearID,
	sho.monthID,
	sho.dayID,
	sho.hourID,
	sho.stateID,
	sho.countyID,
	sho.zoneID,
	sho.linkID,
	sho.sourceTypeID,
	df.fuelTypeID,
	sho.modelYearID,
	sho.roadTypeID,
	1,
	(sho.distance * df.fuelTypeActivityFraction) as activity
FROM DistFracts AS df INNER JOIN SHO3 AS sho USING (sourceTypeID, modelYearID);

-- Section UseDioxinEmissionRate
insert into MOVESWorkerOutput (
	processID, pollutantID,
	yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,
	emissionQuant) 
select
	r.processID, r.pollutantID,
	yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,sourceTypeID,a.fuelTypeID,a.modelYearID,roadTypeID,SCC,
	(activity*meanBaseRate) as emissionQuant
from ATActivityOutput a
inner join dioxinEmissionRate r on (
	r.fuelTypeID = a.fuelTypeID
	and r.modelYearID = a.modelYearID
);
-- End Section UseDioxinEmissionRate

-- Section UseMetalEmissionRate
insert into MOVESWorkerOutput (
	processID, pollutantID,
	yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,
	emissionQuant) 
select
	r.processID, r.pollutantID,
	yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,a.sourceTypeID,a.fuelTypeID,a.modelYearID,roadTypeID,SCC,
	(activity*meanBaseRate) as emissionQuant
from ATActivityOutput a
inner join metalEmissionRate r on (
	r.sourceTypeID = a.sourceTypeID
	and r.fuelTypeID = a.fuelTypeID
	and r.modelYearID = a.modelYearID
);
-- End Section UseMetalEmissionRate

-- End Section Processing

-- Section Cleanup
drop table if exists ATActivityOutput;
DROP TABLE IF EXISTS SBD2;
DROP TABLE IF EXISTS SVTD2;
DROP TABLE IF EXISTS SVTD3;
DROP TABLE IF EXISTS DistFracts;
DROP TABLE IF EXISTS SHO2;
DROP TABLE IF EXISTS Link2;
DROP TABLE IF EXISTS SHO3;
-- End Section Cleanup
