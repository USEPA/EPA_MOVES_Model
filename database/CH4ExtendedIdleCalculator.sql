-- Version 2013-09-15
-- Author Wesley Faler
-- Purpose: Calculate methane emissions during extended idle

-- Section Create Remote Tables for Extracted Data

##create.RunSpecYear##;
TRUNCATE RunSpecYear;

##create.AgeCategory##;
TRUNCATE AgeCategory;

##create.County##;
TRUNCATE County;

##create.EmissionRateByAge##;
TRUNCATE EmissionRateByAge;

##create.FuelFormulation##;
TRUNCATE FuelFormulation;

##create.FuelSubType##;
TRUNCATE FuelSubType;

##create.FuelSupply##;
TRUNCATE FuelSupply;

##create.FuelType##;
TRUNCATE FuelType;

##create.FullACAdjustment##;
TRUNCATE FullACAdjustment;

##create.HourDay##;
TRUNCATE HourDay;

##create.IMCoverage##;
TRUNCATE IMCoverage;

##create.IMFactor##;
TRUNCATE IMFactor;

##create.Link##;
TRUNCATE Link;

##create.MonthGroupHour##;
TRUNCATE MonthGroupHour;

##create.MonthOfAnyYear##;
TRUNCATE MonthOfAnyYear;

##create.OpModeDistribution##;
TRUNCATE OpModeDistribution;

##create.PollutantProcessAssoc##;
TRUNCATE PollutantProcessAssoc;

##create.PollutantProcessModelYear##;
TRUNCATE PollutantProcessModelYear;

##create.RegulatoryClass##;
TRUNCATE RegulatoryClass;

##create.sho##;
TRUNCATE sho;

##create.ExtendedIdleHours##;
TRUNCATE ExtendedIdleHours;

##create.SourceBin##;
TRUNCATE SourceBin;

##create.SourceBinDistribution##;
TRUNCATE SourceBinDistribution;

##create.SourceTypeAge##;
TRUNCATE SourceTypeAge;

##create.SourceTypeModelYear##;
TRUNCATE SourceTypeModelYear;

##create.TemperatureAdjustment##;
TRUNCATE TemperatureAdjustment;

##create.Year##;
TRUNCATE Year;

##create.Zone##;
TRUNCATE Zone;

##create.ZoneMonthHour##;
TRUNCATE ZoneMonthHour;

drop table if exists oneCountyYearGeneralFuelRatio;
create table if not exists oneCountyYearGeneralFuelRatio (
	fuelTypeID int not null,
	sourceTypeID int not null,
	monthID int not null,
	pollutantID int not null,
	processID int not null,
	modelYearID int not null,
	yearID int not null,
	fuelEffectRatio double not null default '0',
	primary key (fuelTypeID, sourceTypeID, monthID, pollutantID, modelYearID, yearID)
);
truncate oneCountyYearGeneralFuelRatio;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

SELECT DISTINCT EmissionRateByAge.* INTO OUTFILE '##EmissionRateByAge##'
FROM EmissionRateByAge, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType 
WHERE 
    EmissionRateByAge.opModeID = 200	
    AND RunSpecSourceFuelType.fuelTypeID = SourceBin.fuelTypeID
	AND EmissionRateByAge.polProcessID = SourceBinDistribution.polProcessID
	AND EmissionRateByAge.sourceBinID = SourceBin.sourceBinID
	AND EmissionRateByAge.sourceBinID = SourceBinDistribution.sourceBinID
	AND SourceBin.sourceBinID = SourceBinDistribution.sourceBinID
	AND RunSpecSourceFuelType.sourceTypeID = SourceTypeModelYear.sourceTypeID
	AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
	AND SourceTypeModelYear.modelYearID <= ##context.year## 
	AND EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##);


SELECT ff.* INTO OUTFILE '##FuelFormulation##'
FROM FuelFormulation ff
INNER JOIN FuelSupply fs ON fs.fuelFormulationID = ff.fuelFormulationID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID
INNER JOIN RunSpecMonthGroup rsmg ON rsmg.monthGroupID = fs.monthGroupID
WHERE fuelRegionID = ##context.fuelRegionID## AND y.yearID = ##context.year##  
GROUP BY ff.FuelFormulationID ORDER BY NULL;

SELECT * INTO OUTFILE '##FuelSubtype##'
FROM FuelSubtype;

SELECT FuelSupply.* INTO OUTFILE '##FuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
WHERE fuelRegionID = ##context.fuelRegionID##
	AND Year.yearID = ##context.year##;

SELECT DISTINCT FuelType.* INTO OUTFILE '##FuelType##'
FROM FuelType
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = FuelType.fuelTypeID);

SELECT faca.* INTO OUTFILE '##FullACAdjustment##'
FROM FullACAdjustment faca
INNER JOIN RunSpecSourceType rsst ON (rsst.sourceTypeID=faca.sourceTypeID)
INNER JOIN RunSpecPollutantProcess rspp ON (rspp.polProcessID=faca.polProcessID);

SELECT DISTINCT HourDay.* INTO OUTFILE '##HourDay##'
FROM HourDay,RunSpecHour,RunSpecDay
WHERE HourDay.dayID = RunSpecDay.dayID AND HourDay.hourID = RunSpecHour.hourID;

SELECT DISTINCT IMCoverage.* INTO OUTFILE '##IMCoverage##'
FROM IMCoverage
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = IMCoverage.fuelTypeID
	and RunSpecSourceFuelType.sourceTypeID = IMCoverage.sourceTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##)
AND countyID = ##context.iterLocation.countyRecordID## 
AND yearID = ##context.year##
AND useIMyn = 'Y';

SELECT DISTINCT IMFactor.* INTO OUTFILE '##IMFactor##'
FROM IMFactor
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = IMFactor.fuelTypeID
	and RunSpecSourceFuelType.sourceTypeID = IMFactor.sourceTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

SELECT Link.* INTO OUTFILE '##Link##'
FROM Link 
WHERE roadTypeID = 1 AND 
	zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT MonthGroupHour.* INTO OUTFILE '##MonthGroupHour##' 
FROM MonthGroupHour INNER JOIN RunSpecHour USING (hourID);

cache SELECT MonthOfAnyYear.* INTO OUTFILE '##MonthOfAnyYear##'
FROM MonthOfAnyYear,RunSpecMonth
WHERE MonthOfAnyYear.monthID = RunSpecMonth.monthID;

SELECT OpModeDistribution.* INTO OUTFILE '##OpModeDistribution##'
FROM OpModeDistribution, RunSpecSourceType
WHERE polProcessID IN (##pollutantProcessIDs##)
	AND OpModeDistribution.opModeID = 200 
	AND linkID = (##context.iterLocation.linkRecordID##) 
	AND RunSpecSourceType.sourceTypeID = OpModeDistribution.sourceTypeID;

cache SELECT * INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE processID=##context.iterProcess.databaseKey##;

cache SELECT * INTO OUTFILE '##PollutantProcessModelYear##'
FROM PollutantProcessModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT * INTO OUTFILE '##RegulatoryClass##' 
FROM RegulatoryClass;

SELECT * INTO OUTFILE '##SHO##' 
FROM SHO 
WHERE linkID = (##context.iterLocation.linkRecordID##) 
	AND yearID = ##context.year##;

SELECT * INTO OUTFILE '##ExtendedIdleHours##' 
FROM ExtendedIdleHours 
WHERE yearID = ##context.year## 
	AND zoneID = ##context.iterLocation.zoneRecordID##;

SELECT DISTINCT SourceBin.* INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
	AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
	AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
	AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
	AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT SourceBinDistribution.* INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear, SourceBin, RunSpecSourceFuelType 
WHERE polProcessID IN (##pollutantProcessIDs##)
	AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
	AND SourceTypeModelYear.modelYearID <= ##context.year## 
	AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
	AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
	AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT SourceTypeAge.* INTO OUTFILE '##SourceTypeAge##'
FROM SourceTypeAge,RunSpecSourceType
WHERE SourceTypeAge.sourceTypeID = RunSpecSourceType.sourceTypeID;

cache SELECT SourceTypeModelYear.* INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear,RunSpecSourceType 
WHERE 	SourceTypeModelYear.sourceTypeID = RunSpecSourceType.sourceTypeID  
	AND SourceTypeModelYear.modelYearID <= ##context.year##
	AND SourceTypeModelYear.modelYearID >= ##context.year## - 30;

cache SELECT DISTINCT TemperatureAdjustment.* INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = TemperatureAdjustment.fuelTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT Year.* INTO OUTFILE '##Year##'
FROM Year 
WHERE yearID = ##context.year##;

cache SELECT RunSpecYear.* INTO OUTFILE '##RunSpecYear##'
FROM RunSpecYear;

SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

SELECT DISTINCT ZoneMonthHour.* INTO OUTFILE '##ZoneMonthHour##'
FROM ZoneMonthHour,RunSpecMonth,RunSpecHour
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND RunSpecMonth.monthID = ZoneMonthHour.monthID
AND RunSpecHour.hourID = ZoneMonthHour.hourID;

select gfr.fuelTypeID, gfr.sourceTypeID, may.monthID, gfr.pollutantID, gfr.processID, mya.modelYearID, mya.yearID,
	sum((ifnull(fuelEffectRatio,1)+GPAFract*(ifnull(fuelEffectRatioGPA,1)-ifnull(fuelEffectRatio,1)))*marketShare) as fuelEffectRatio
	INTO OUTFILE '##oneCountyYearGeneralFuelRatio##'
from RunSpecMonthGroup rsmg
inner join RunSpecModelYearAge mya on (mya.yearID = ##context.year##)
inner join County c on (c.countyID = ##context.iterLocation.countyRecordID##)
inner join Year y on (y.yearID = mya.yearID)
inner join FuelSupply fs on (fs.fuelRegionID = ##context.fuelRegionID##
	and fs.fuelYearID = y.fuelYearID
	and fs.monthGroupID = rsmg.monthGroupID)
inner join MonthOfAnyYear may on (may.monthGroupID = fs.monthGroupID)
inner join RunSpecSourceFuelType rssf
inner join generalFuelRatio gfr on (gfr.fuelFormulationID = fs.fuelFormulationID
	and gfr.polProcessID in (##pollutantProcessIDs##)
	and gfr.minModelYearID <= mya.modelYearID
	and gfr.maxModelYearID >= mya.modelYearID
	and gfr.minAgeID <= mya.ageID
	and gfr.maxAgeID >= mya.ageID
	and gfr.fuelTypeID = rssf.fuelTypeID
	and gfr.sourceTypeID = rssf.sourceTypeID)
group by gfr.fuelTypeID, gfr.sourceTypeID, may.monthID, gfr.pollutantID, gfr.processID, mya.modelYearID, mya.yearID
;

-- End Section Extract Data

-- Section Processing

-- SourceBin-Weighted Weight Emission Rates

DROP TABLE IF EXISTS EmissionRateByAge2;

CREATE TABLE IF NOT EXISTS EmissionRateByAge2(
	sourceBinID	bigint(20) NOT NULL,
	polProcessID	int NOT NULL,
	opModeID	smallint NOT NULL,
	ageGroupID	smallint NOT NULL,
	modelYearID	smallint NOT NULL,
	fuelTypeID	smallint NOT NULL,
	sourceTypeID	smallint NOT NULL,
	meanBaseRate	float NULL,
	sourceBinActivityFraction	float NULL,
	PRIMARY KEY (sourceBinID, polProcessID, opModeID, ageGroupID, modelYearID, fuelTypeID, sourceTypeID)
);

TRUNCATE TABLE EmissionRateByAge2;

INSERT IGNORE INTO EmissionRateByAge2 (sourceBinID, polProcessID, opModeID, ageGroupID, meanBaseRate, 
	modelYearID, fuelTypeID, sourceTypeID, sourceBinActivityFraction) 
SELECT DISTINCT er.sourceBinID, er.polProcessID, er.opModeID, er.ageGroupID, er.meanBaseRate, 
	stmy.modelYearID, sb.fuelTypeID, stmy.sourceTypeID, sbd.sourceBinActivityFraction
FROM EmissionRateByAge er, SourceBin sb, RunSpecSourceFuelType rsft, SourceBinDistribution sbd, SourceTypeModelYear stmy 
WHERE 	
	er.opModeID		= 200 
	AND er.sourceBinID 	= sb.sourceBinID 
	AND sb.fuelTypeID 	= rsft.fuelTypeID 
	AND er.polProcessID IN (##pollutantProcessIDs##) 
	AND sbd.sourceBinID = sb.sourceBinID 
	AND sbd.polProcessID IN (##pollutantProcessIDs##) 
	AND sbd.sourceTypeModelYearID = stmy.sourceTypeModelYearID 
;

DROP TABLE IF EXISTS SBWeightedEmissionRate;
CREATE TABLE SBWeightedEmissionRate (
       sourceBinID      BIGINT(20) NOT NULL,
       polProcessID 	int NOT NULL,
       sourceTypeID 	SMALLINT NOT NULL,
       modelYearID 	SMALLINT NOT NULL,
       fuelTypeID 	SMALLINT NOT NULL,
       ageGroupID	SMALLINT NOT NULL,
       meanBaseRate 	FLOAT
);


CREATE UNIQUE INDEX XPKSBWeightedEmissionRate ON SBWeightedEmissionRate (
       sourceBinID	ASC,
       polProcessID 	ASC,
       sourceTypeID 	ASC,
       modelYearID 	ASC,
       fuelTypeID 	ASC,
       ageGroupID	ASC
);
ANALYZE TABLE SBWeightedEmissionRate;

INSERT INTO SBWeightedEmissionRate (sourceBinID, polProcessID, sourceTypeID, modelYearID, fuelTypeID, ageGroupID, meanBaseRate)
SELECT er.sourceBinID, er.polProcessID, er.sourceTypeID, er.modelYearID, er.fuelTypeID, er.ageGroupID, 
	SUM(er.sourceBinActivityFraction * er.meanBaseRate) AS meanBaseRate 
FROM EmissionRateByAge2 er
GROUP BY er.polProcessID, er.sourceTypeID, er.modelYearID, er.fuelTypeID, er.ageGroupID 
order by null;


-- FLUSH TABLES;

-- Multiply Emission Rates by Activity

DROP TABLE IF EXISTS EIH2;
CREATE TABLE EIH2 (
       zoneID		INT NOT NULL,
       monthID		SMALLINT NOT NULL,
       hourDayID        SMALLINT NOT NULL,
       hourID		SMALLINT,
       dayID		SMALLINT,
       yearID		SMALLINT NOT NULL,
       ageID		SMALLINT NOT NULL,
       ageGroupID		SMALLINT NOT NULL,
       sourceTypeID 	SMALLINT NOT NULL,
       extendedIdleHours 	FLOAT
);

CREATE UNIQUE INDEX XPKEIH2 ON EIH2 (
      zoneID		ASC,
       monthID          ASC,
       hourDayID	ASC,
       yearID		ASC,
       ageID		ASC,
       ageGroupID   ASC,
       sourceTypeID	ASC);

ANALYZE TABLE EIH2;

-- FLUSH TABLES;

truncate table EIH2;


INSERT INTO EIH2 (zoneID, monthID, hourDayID, hourID, dayID, yearID, ageID, ageGroupID, sourceTypeID, extendedIdleHours) 
SELECT eih.zoneID, eih.monthID, eih.hourDayID, hrdy.hourID, hrdy.dayID, eih.yearID, eih.ageID, ac.ageGroupID, eih.sourceTypeID, eih.extendedIdleHours 
FROM extendedIdleHours eih 
INNER JOIN hourday hrdy ON (hrdy.hourDayID=eih.hourDayID)
INNER JOIN ageCategory ac ON (eih.ageID=ac.ageID)
;

-- FLUSH TABLES;

DROP TABLE IF EXISTS EmissionResultsWithTime;
CREATE TABLE EmissionResultsWithTime (
       polProcessID 	int NOT NULL,
       sourceTypeID 	SMALLINT NOT NULL,
       modelYearID 	SMALLINT NOT NULL,
       fuelTypeID 	SMALLINT NOT NULL,
       zoneID		INT NOT NULL,
       monthID		SMALLINT NOT NULL,
       hourID		SMALLINT NOT NULL,
       dayID		SMALLINT NOT NULL,
       yearID		SMALLINT NOT NULL,
       ageID		SMALLINT NOT NULL,
       ageGroupID		SMALLINT NOT NULL,
       emissionQuant 	FLOAT
);

CREATE UNIQUE INDEX XPKEmissionResultsWithTime ON EmissionResultsWithTime (
       polProcessID 	ASC,
       sourceTypeID 	ASC,
       modelYearID 	ASC,
       fuelTypeID 	ASC,
       zoneID		ASC,
       monthID      ASC,
       hourID		ASC,
       dayID		ASC,
       yearID		ASC,
       ageID        ASC,
       ageGroupID   ASC
);
ANALYZE TABLE EmissionResultsWithTime;

-- ------------------------------

INSERT INTO EmissionResultsWithTime (polProcessID, sourceTypeID, modelYearID, fuelTypeID, zoneID, monthID, 
				hourID, dayID, yearID, ageID, ageGroupID, emissionQuant)
SELECT waer.polProcessID, waer.sourceTypeID, waer.modelYearID, waer.fuelTypeID, eih.zoneID, eih.monthID, 
				eih.hourID, eih.dayID, eih.yearID, eih.ageID, waer.ageGroupID,  
	(waer.meanBaseRate * eih.extendedIdleHours) AS emissionQuant 
FROM SBWeightedEmissionRate waer, EIH2 eih
WHERE eih.sourceTypeID=waer.sourceTypeID AND eih.ageGroupID=waer.ageGroupID 
		AND eih.ageID = eih.yearID - waer.modelYearID
;

-- Multiply Emission Rates for the year


DROP TABLE IF EXISTS EmissionResults;
CREATE TABLE EmissionResults (
       polProcessID 	int NOT NULL,
       sourceTypeID 	SMALLINT NOT NULL,
       modelYearID 	SMALLINT NOT NULL,
       fuelTypeID 	SMALLINT NOT NULL,
       zoneID		INT NOT NULL,
       monthID		SMALLINT NOT NULL,
       hourID		SMALLINT NOT NULL,
       dayID		SMALLINT NOT NULL,
       yearID		SMALLINT NOT NULL,
       ageID		SMALLINT NOT NULL,
       emissionQuant 	FLOAT
);

CREATE UNIQUE INDEX XPKEmissionResults ON EmissionResults (
       polProcessID 	ASC,
       sourceTypeID 	ASC,
       modelYearID 	ASC,
       fuelTypeID 	ASC,
       zoneID		ASC,
       monthID          ASC,
       hourID		ASC,
       dayID		ASC,
       yearID		ASC,
       ageID            ASC
);
ANALYZE TABLE EmissionResults;



INSERT INTO EmissionResults (polProcessID, sourceTypeID, modelYearID, fuelTypeID, zoneID, monthID, 
				hourID, dayID, yearID, ageID, emissionQuant)
SELECT erwt.polProcessID, erwt.sourceTypeID, erwt.modelYearID, erwt.fuelTypeID, erwt.zoneID, erwt.monthID, 
				erwt.hourID, erwt.dayID, erwt.yearID, erwt.ageID,  
	SUM(erwt.emissionQuant) AS emissionQuant 
FROM EmissionResultsWithTime erwt INNER JOIN RunSpecYear ry USING (yearID)
GROUP BY erwt.polProcessID, erwt.sourceTypeID, erwt.modelYearID, erwt.fuelTypeID, 
		erwt.zoneID, erwt.monthID, erwt.hourID, erwt.dayID, erwt.yearID 
order by null;

-- Convert Results to Structure of MOVESWorkerOutput by sourceTypeID

TRUNCATE MOVESWorkerOutput;
INSERT INTO MOVESWorkerOutput (
	stateID, countyID, zoneID, linkID, roadTypeID, yearID, monthID, dayID, hourID, pollutantID, 
	processID, sourceTypeID, fuelTypeID, modelYearID, SCC, emissionQuant)
SELECT ##context.iterLocation.stateRecordID## AS stateID, 
	##context.iterLocation.countyRecordID## AS countyID, aer.zoneID, lnk.linkID, lnk.roadTypeID,
	aer.yearID, aer.monthID, aer.dayID, aer.hourID, ppa.pollutantID, ppa.processID, aer.sourceTypeID, 
	aer.fuelTypeID, aer.modelYearID, NULL AS SCC, aer.emissionQuant AS emissionQuant 
FROM EmissionResults aer 
INNER JOIN PollutantProcessAssoc ppa ON (ppa.polProcessID = aer.polProcessID)
INNER JOIN Link lnk ON (lnk.zoneID=aer.zoneID)
WHERE lnk.roadTypeID=1 
order by null;

update MOVESWorkerOutput, oneCountyYearGeneralFuelRatio set emissionQuant=emissionQuant*fuelEffectRatio
where oneCountyYearGeneralFuelRatio.fuelTypeID = MOVESWorkerOutput.fuelTypeID
and oneCountyYearGeneralFuelRatio.sourceTypeID = MOVESWorkerOutput.sourceTypeID
and oneCountyYearGeneralFuelRatio.monthID = MOVESWorkerOutput.monthID
and oneCountyYearGeneralFuelRatio.pollutantID = MOVESWorkerOutput.pollutantID
and oneCountyYearGeneralFuelRatio.processID = MOVESWorkerOutput.processID
and oneCountyYearGeneralFuelRatio.modelYearID = MOVESWorkerOutput.modelYearID
and oneCountyYearGeneralFuelRatio.yearID = MOVESWorkerOutput.yearID;

-- End Section Processing

-- Section Cleanup

DROP TABLE IF EXISTS EIH2;
DROP TABLE IF EXISTS EmissionRateByAge2;
DROP TABLE IF EXISTS SBWeightedEmissionRate;
DROP TABLE IF EXISTS EmissionResults;
drop table if exists oneCountyYearGeneralFuelRatio;
-- End Section Cleanup
