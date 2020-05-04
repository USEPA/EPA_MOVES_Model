-- Version 2013-09-15
-- Author Wes Faler
-- Author Ed Glover

-- Section Create Remote Tables for Extracted Data

##create.RunSpecYear##;
TRUNCATE RunSpecYear;

##create.AgeCategory##;
TRUNCATE AgeCategory;

##create.County##;
TRUNCATE County;

##create.EmissionRateByAge##;
TRUNCATE EmissionRateByAge;

##create.EmissionRate##;
TRUNCATE EmissionRate;

##create.FuelType##;
TRUNCATE FuelType;

##create.FullACAdjustment##;
TRUNCATE FullACAdjustment;

##create.hotellingActivityDistribution##;
TRUNCATE hotellingActivityDistribution;

##create.HourDay##;
TRUNCATE HourDay;

##create.IMCoverage##;
TRUNCATE IMCoverage;

##create.IMFactor##;
TRUNCATE IMFactor;

##create.Link##;
TRUNCATE Link;

##create.ModelYear##
TRUNCATE ModelYear;

##create.MonthGroupHour##;
TRUNCATE MonthGroupHour;

##create.MonthOfAnyYear##;
TRUNCATE MonthOfAnyYear;

##create.PollutantProcessAssoc##;
TRUNCATE PollutantProcessAssoc;

##create.hotellingHours##;
TRUNCATE hotellingHours;

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

cache SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

cache SELECT * INTO OUTFILE '##EmissionRate##'
FROM EmissionRate
WHERE opModeID >= 201 and opModeID <= 299
	AND EmissionRate.polProcessID IN (##pollutantProcessIDs##);

cache SELECT DISTINCT EmissionRateByAge.* INTO OUTFILE '##EmissionRateByAge##'
FROM EmissionRateByAge, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType 
WHERE 	RunSpecSourceFuelType.fuelTypeID = SourceBin.fuelTypeID
	AND EmissionRateByAge.polProcessID = SourceBinDistribution.polProcessID
	AND EmissionRateByAge.sourceBinID = SourceBin.sourceBinID
	AND EmissionRateByAge.sourceBinID = SourceBinDistribution.sourceBinID
	AND SourceBin.sourceBinID = SourceBinDistribution.sourceBinID
	AND RunSpecSourceFuelType.sourceTypeID = SourceTypeModelYear.sourceTypeID
	AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
	AND SourceTypeModelYear.modelYearID <= ##context.year##
	AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
	AND EmissionRateByAge.opModeID >= 201 and EmissionRateByAge.opModeID <= 299 
	AND EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##);

cache SELECT DISTINCT FuelType.* INTO OUTFILE '##FuelType##'
FROM FuelType
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = FuelType.fuelTypeID);

SELECT faca.* INTO OUTFILE '##FullACAdjustment##'
FROM FullACAdjustment faca
INNER JOIN RunSpecSourceType rsst ON (rsst.sourceTypeID=faca.sourceTypeID)
INNER JOIN RunSpecPollutantProcess rspp ON (rspp.polProcessID=faca.polProcessID)
where rspp.polProcessID in (##pollutantProcessIDs##);

cache select * into outfile '##hotellingActivityDistribution##'
from hotellingActivityDistribution
where (beginModelYearID <= ##context.year## - 30 and endModelYearID >= ##context.year## - 30)
or (beginModelYearID <= ##context.year## and endModelYearID >= ##context.year##)
or (beginModelYearID >= ##context.year## - 30 and endModelYearID <= ##context.year##);

cache SELECT DISTINCT HourDay.* INTO OUTFILE '##HourDay##'
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

cache SELECT Link.* INTO OUTFILE '##Link##'
FROM Link 
WHERE roadTypeID = 1 AND 
	zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT * INTO OUTFILE '##ModelYear##'
FROM ModelYear;

cache SELECT MonthGroupHour.* INTO OUTFILE '##MonthGroupHour##' 
FROM MonthGroupHour INNER JOIN RunSpecHour USING (hourID);

cache SELECT MonthOfAnyYear.* INTO OUTFILE '##MonthOfAnyYear##'
FROM MonthOfAnyYear,RunSpecMonth
WHERE MonthOfAnyYear.monthID = RunSpecMonth.monthID;

cache SELECT * INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE processID=##context.iterProcess.databaseKey##;

cache SELECT * INTO OUTFILE '##hotellingHours##' 
FROM hotellingHours 
WHERE yearID = ##context.year## 
	AND zoneID = ##context.iterLocation.zoneRecordID##;

SELECT DISTINCT SourceBin.* INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
	AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
	AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
	AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
	AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID
	AND SourceTypeModelYear.modelYearID <= ##context.year##
	AND SourceTypeModelYear.modelYearID >= ##context.year## - 30;

SELECT DISTINCT SourceBinDistribution.* INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear, SourceBin, RunSpecSourceFuelType 
WHERE polProcessID IN (##pollutantProcessIDs##)
	AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
	AND SourceTypeModelYear.modelYearID <= ##context.year## 
	AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
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

SELECT DISTINCT TemperatureAdjustment.* INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = TemperatureAdjustment.fuelTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT Year.* INTO OUTFILE '##Year##'
FROM Year 
WHERE yearID = ##context.year##;

cache SELECT RunSpecYear.* INTO OUTFILE '##RunSpecYear##'
FROM RunSpecYear;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT DISTINCT ZoneMonthHour.* INTO OUTFILE '##ZoneMonthHour##'
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

-- CEIC-1: Calculate Temperature and NOx Humidity Adjustments


DROP TABLE IF EXISTS tappaft;
CREATE TABLE tappaft (
       polProcessID 	int NOT NULL,
       fuelTypeID	SMALLINT NOT NULL,
       minModelYearID	INT NOT NULL,
       maxModelYearID	INT NOT NULL,
       humidityCorrectionCoeff FLOAT NULL,
       tempAdjustTermA	FLOAT NULL,
       tempAdjustTermB  FLOAT NULL
);

CREATE UNIQUE INDEX XPKtappaft ON tappaft
(
       polProcessID ASC,
       fuelTypeID   ASC,
       minModelYearID	ASC,
       maxModelYearID	ASC
);

ANALYZE TABLE tappaft;

INSERT INTO tappaft(polProcessID, fuelTypeID, minModelYearID, maxModelYearID, humidityCorrectionCoeff, tempAdjustTermA, tempAdjustTermB) 
SELECT ta.polProcessID, ta.fuelTypeID, ta.minModelYearID, ta.maxModelYearID, ft.humidityCorrectionCoeff, ta.tempAdjustTermA, 
				ta.tempAdjustTermB 
FROM TemperatureAdjustment ta 
INNER JOIN fueltype ft ON (ta.fueltypeID = ft.fueltypeID);


DROP TABLE IF EXISTS METADjustment;
CREATE TABLE METAdjustment (
       zoneID		INT  NOT NULL,
       monthID		SMALLINT NOT NULL,
       hourID		SMALLINT NOT NULL,
       polProcessID 	int NOT NULL,
       fuelTypeID	SMALLINT NOT NULL,
       modelYearID	SMALLINT NOT NULL,
       minModelYearID	INT	NOT NULL,
       maxModelYearID	INT NOT NULL,
       temperatureAdjustment FLOAT NULL,
       K Float NULL,
       temperature Float NULL,
       tempAdjustTermA Float NULL,
       tempAdjustTermB Float NULL,
       specificHumidity Float NULL,
       humidityCorrectionCoeff Float NULL 
);

CREATE UNIQUE INDEX XPKMETAdjustment ON METAdjustment
(
       polProcessID     ASC,
       modelYearID	ASC,
       fuelTypeID    ASC,
       monthID     ASC,
       hourID     ASC
);

ANALYZE TABLE METADjustment;

INSERT INTO METAdjustment(zoneID, monthID, hourID ,polProcessID, fuelTypeID, modelYearID, minModelYearID, maxModelYearID, 
			temperatureAdjustment, K, temperature, tempAdjustTermA, tempAdjustTermB, specificHumidity, humidityCorrectionCoeff) 
SELECT zmh.zoneID, zmh.monthID, zmh.hourID, ta.polProcessID, ta.fuelTypeID, my.modelyearid, ta.minModelYearID, ta.maxModelYearID, 
	((zmh.temperature - 75.0) * ta.tempAdjustTermA) AS temperatureAdjustment, 
	(GREATEST(21.0, LEAST(zmh.specificHumidity, 124.0))) AS K,
	zmh.temperature, ta.tempAdjustTermA, ta.tempAdjustTermB, zmh.specificHumidity, ta.humidityCorrectionCoeff 
FROM ZoneMonthHour zmh
INNER JOIN tappaft ta 
JOIN modelyear my
WHERE zmh.zoneID = 	##context.iterLocation.zoneRecordID## 
	AND MOD(ta.polProcessID,100) = 91
	AND my.modelYearID between minModelYearID and maxModelYearID;

update METAdjustment set temperatureAdjustment = 1.0 + tempAdjustTermB * (temperature - 75.0) * (temperature - 75.0) + temperatureAdjustment;
update METAdjustment set K = 1.0 - (K - 75.0) * humidityCorrectionCoeff;

-- flush tables;

DROP TABLE IF EXISTS tappaft;

-- CEIC 2  : caculate AC Adjustment Factor

-- CEIC 2-a: Calculate AC On Fraction

DROP TABLE IF EXISTS ACOnFraction;
CREATE TABLE ACOnFraction (
       zoneID	INT NOT NULL,
       monthID	SMALLINT NOT NULL,
       hourID	SMALLINT NOT NULL, 
       ACOnFraction FLOAT,
       ACActivityTermB FLOAT,
       ACActivityTermC FLOAT,
       heatIndex FLOAT
); 

CREATE UNIQUE INDEX XPKACOnFraction ON ACOnFraction
(
       zoneID ASC,
       monthID ASC,
       hourID ASC
);

ANALYZE TABLE ACOnFraction;

INSERT INTO ACOnFraction(zoneID, monthID, hourID, ACOnFraction, ACActivityTermB, ACActivityTermC, heatIndex)
SELECT zmh.zoneID, zmh.monthID, zmh.hourID, 
       mgh.ACActivityTermA AS ACOnFraction, mgh.ACActivityTermB, mgh.ACActivityTermC, zmh.heatIndex 
FROM ZoneMonthHour zmh
INNER JOIN MonthOfAnyYear may ON (may.monthID = zmh.monthID)
INNER JOIN MonthGroupHour mgh ON (mgh.monthGroupID = may.monthGroupID AND mgh.hourID = zmh.hourID);

-- FLUSH TABLES;

update ACOnFraction set ACOnFraction = ACOnFraction + (heatIndex * ACActivityTermB) + (ACActivityTermC * heatIndex * heatIndex);
update ACOnFraction set ACOnFraction = IF(ACOnFraction > 1.0, 1.0, ACOnFraction);
update ACOnFraction set ACOnFraction = IF(ACOnFraction < 0.0, 0.0, ACOnFraction);

-- FLUSH TABLES;

-- CREC 2-b: Calculate AC Activity Fraction

DROP TABLE IF EXISTS temp1;
CREATE TABLE temp1 (
       zoneID  INT NOT NULL,
       yearID SMALLINT NOT NULL,
       monthID SMALLINT NOT NULL,
       hourID SMALLINT NOT NULL,
       ACOnFraction FLOAT
);

CREATE UNIQUE INDEX XPKtemp1 ON temp1 (
       zoneID ASC,
       yearID ASC,
       monthID ASC,
       hourID ASC
);

INSERT INTO temp1 (zoneID, yearID, monthID, hourID, ACOnFraction)
SELECT acof.zoneID, ry.yearID, acof.monthID, acof.hourID, acof.ACOnFraction 
FROM ACOnFraction acof
INNER JOIN RunSpecYear ry
WHERE ry.yearID = ##context.year##;

DROP TABLE IF EXISTS temp2;
CREATE TABLE temp2 (
       zoneID  INT NOT NULL,
       yearID SMALLINT NOT NULL,
       monthID SMALLINT NOT NULL,
       hourID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
       ACOnFraction FLOAT,
	ACPenetrationFraction FLOAT, 
	ageID SMALLINT NOT NULL
);

CREATE UNIQUE INDEX XPKtemp2 ON temp2 (
       zoneID ASC,
       yearID ASC,
       monthID ASC,
       hourID ASC,
	modelYearID ASC,
	sourceTypeID ASC
);


INSERT INTO temp2 (zoneID, yearID, monthID, hourID, modelYearID, sourceTypeID, ACOnFraction, ACPenetrationFraction, ageID)
SELECT acof.zoneID, acof.yearID, acof.monthID, acof.hourID, stmy.modelYearID, stmy.sourceTypeID, acof.ACOnFraction, stmy.ACPenetrationFraction,
	(acof.yearID - stmy.modelYearID) AS ageID
FROM temp1 acof
INNER JOIN SourceTypeModelYear stmy;

-- !!! Gwo Shyu 11/4/2009  the follwoing lines should equal the above.
-- INSERT INTO temp2 (zoneID, yearID, monthID, hourID, modelYearID, sourceTypeID, ACOnFraction, ACPenetrationFraction, ageID)
-- SELECT acof.zoneID, acof.yearID, acof.monthID, acof.hourID, stmy.modelYearID, stmy.sourceTypeID, acof.ACOnFraction, stmy.ACPenetrationFraction,
-- 	(acof.yearID - stmy.modelYearID) AS ageID
-- FROM temp1 acof, SourceTypeModelYear stmy;

DROP TABLE IF EXISTS ACActivityFraction;
CREATE TABLE ACActivityFraction (
       zoneID  INT NOT NULL,
       yearID SMALLINT NOT NULL,
       monthID SMALLINT NOT NULL,
       hourID SMALLINT NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       modelYearID SMALLINT NOT NULL,
       ACActivityFraction FLOAT
);

CREATE UNIQUE INDEX XPKACActivityFraction ON ACActivityFraction (
       zoneID ASC,
       yearID ASC,
       monthID ASC,
       hourID ASC,
       sourceTypeID ASC,
       modelYearID ASC
);
ANALYZE TABLE ACActivityFraction;

INSERT INTO ACActivityFraction (zoneID, yearID, monthID, hourID, sourceTypeID, modelYearID, ACActivityFraction)
SELECT acof.zoneID, acof.yearID, acof.monthID, acof.hourID, acof.sourceTypeID, acof.modelYearID, 
	(acof.ACOnFraction * acof.ACPenetrationFraction * sta.functioningACFraction) AS ACActivityFraction 
FROM temp2 acof
INNER JOIN SourceTypeAge sta ON (sta.sourceTypeID = acof.sourceTypeID AND sta.ageID=acof.ageID)
order by null;

-- FLUSH TABLES;

-- CEIC 2-c: Calculate AC Adjustment Factor

DROP TABLE IF EXISTS ACAdjustment;
CREATE TABLE ACAdjustment (
       zoneID  INT NOT NULL,
       monthID  SMALLINT NOT NULL,
       hourID  SMALLINT NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       modelYearID  SMALLINT NOT NULL,
       polProcessID int NOT NULL,
       ACAdjustment FLOAT
);

CREATE UNIQUE INDEX XPKACAdjustment ON ACAdjustment (
       sourceTypeID ASC,
       polProcessID ASC,
       modelYearID ASC,
       monthID ASC,
       hourID  ASC
);
ANALYZE TABLE ACAdjustment;

insert ignore into fullACAdjustment (sourceTypeID, polProcessID, opModeID, fullACAdjustment)
select distinct sourceTypeID, polProcessID, 201, 1.0
from runSpecSourceFuelType, pollutantProcessAssoc
where (chainedTo1 is null or pollutantID in (118));

INSERT INTO ACAdjustment (zoneID,monthID,hourID,sourceTypeID,modelYearID,polProcessID,ACAdjustment)
SELECT acaf.zoneID, acaf.monthID, acaf.hourID, 
	acaf.sourceTypeID, acaf.modelYearID, faca.polProcessID,
	(((faca.fullACAdjustment - 1.0) * acaf.ACActivityFraction) + 1.0) AS ACAdjustment 
FROM ACActivityFraction acaf
INNER JOIN FullACAdjustment faca ON (acaf.sourceTypeID=faca.sourceTypeID
		and faca.opModeID=201 AND MOD(faca.polProcessID,100)=91)
order by null;
-- FLUSH TABLES;

-- CEIC-3: SourceBin-Weighted Weight Emission Rates

DROP TABLE IF EXISTS EmissionRate2;

CREATE TABLE IF NOT EXISTS EmissionRate2(
	sourceBinID	bigint(20) NOT NULL,
	polProcessID	int NOT NULL,
	opModeID	smallint NOT NULL,
	modelYearID	smallint NOT NULL,
	fuelTypeID	smallint NOT NULL,
	sourceTypeID	smallint NOT NULL,
	meanBaseRate	float NULL,
	sourceBinActivityFraction	float NULL,
	opModeFraction float not null,
	PRIMARY KEY (sourceBinID, polProcessID, opModeID, modelYearID, fuelTypeID, sourceTypeID)
);

TRUNCATE TABLE EmissionRate2;

INSERT IGNORE INTO EmissionRate2 (sourceBinID, polProcessID, opModeID, meanBaseRate, 
	modelYearID, fuelTypeID, sourceTypeID, sourceBinActivityFraction, opModeFraction) 
SELECT DISTINCT er.sourceBinID, er.polProcessID, er.opModeID, er.meanBaseRate, 
	stmy.modelYearID, sb.fuelTypeID, stmy.sourceTypeID, sbd.sourceBinActivityFraction, hac.opModeFraction
FROM EmissionRate er, SourceBin sb, RunSpecSourceFuelType rsft, SourceBinDistribution sbd, SourceTypeModelYear stmy,
	hotellingActivityDistribution hac
WHERE 	
	er.sourceBinID 	= sb.sourceBinID 
	AND sb.fuelTypeID 	= rsft.fuelTypeID 
	AND er.polProcessID 	IN (##pollutantProcessIDs##) 
	AND sbd.sourceBinID 	= sb.sourceBinID 
	AND sbd.polProcessID 	IN (##pollutantProcessIDs##) 
	AND sbd.sourceTypeModelYearID = stmy.sourceTypeModelYearID 
	AND stmy.modelYearID 	<= ##context.year##
	AND stmy.modelYearID    >= ##context.year## - 30
	AND hac.beginModelYearID <= stmy.modelYearID
	AND hac.endModelYearID >= stmy.modelYearID
	AND hac.opModeID = er.opModeID
;

DROP TABLE IF EXISTS SBWeightedEmissionRate;
CREATE TABLE SBWeightedEmissionRate (
       sourceBinID      BIGINT(20) NOT NULL,
       polProcessID 	int NOT NULL,
       sourceTypeID 	SMALLINT NOT NULL,
       modelYearID 	SMALLINT NOT NULL,
       fuelTypeID 	SMALLINT NOT NULL,
       meanBaseRate 	FLOAT
);

CREATE UNIQUE INDEX XPKSBWeightedEmissionRate ON SBWeightedEmissionRate (
       polProcessID    ASC,
       sourceTypeID    ASC,
       modelYearID    ASC,
       fuelTypeID     ASC
);
ANALYZE TABLE SBWeightedEmissionRate;

INSERT INTO SBWeightedEmissionRate (sourceBinID, polProcessID, sourceTypeID, modelYearID, fuelTypeID, meanBaseRate)
SELECT er.sourceBinID, er.polProcessID, er.sourceTypeID, er.modelYearID, er.fuelTypeID,
	SUM(er.sourceBinActivityFraction * er.meanBaseRate * er.opModeFraction) AS meanBaseRate 
FROM EmissionRate2 er
GROUP BY er.polProcessID, er.sourceTypeID, er.modelYearID, er.fuelTypeID 
order by null;

-- FLUSH TABLES;

-- CEIC-4: Apply Adjustment Factors to Emission Rates

DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate;
CREATE TABLE WeightedAndAdjustedEmissionRate (
       polProcessID 	int NOT NULL,
       sourceTypeID 	SMALLINT NOT NULL,
       modelYearID 	SMALLINT NOT NULL,
       fuelTypeID 	SMALLINT NOT NULL,
       zoneID		INT NOT NULL,
       monthID		SMALLINT NOT NULL,
       hourID		SMALLINT NOT NULL,
       meanBaseRate 	FLOAT
);

-- CREATE UNIQUE INDEX XPKWeightedAndAdjustedEmissionRate ON WeightedAndAdjustedEmissionRate (
--        modelYearID     ASC,
--        zoneID        ASC,
--        monthID          ASC,
--        hourID        ASC,
--        sourceTypeID     ASC
-- );

CREATE UNIQUE INDEX XPKWeightedAndAdjustedEmissionRate ON WeightedAndAdjustedEmissionRate (
       polProcessID 	ASC,
       sourceTypeID 	ASC,
       modelYearID 	ASC,
       fuelTypeID 	ASC,
       zoneID		ASC,
       monthID       ASC,
       hourID		ASC
);

CREATE INDEX XPKWeightedAndAdjustedEmissionRate_A1 ON WeightedAndAdjustedEmissionRate (
	zoneID        ASC,
	monthID          ASC,
	hourID        ASC,
	sourceTypeID     ASC,
	modelYearID ASC
);

ANALYZE TABLE WeightedAndAdjustedEmissionRate;

CREATE TABLE IF NOT EXISTS CriteriaAndPMAuxiliaryPowerEmissions ( someValue int not null primary key );


-- 11/4/2009 by Gwo S. - Changed from "LEFT JOIN ACAdjustment" to "INNER JOIN ACAdjustment" 
--                       because modelYearIDs with ageIDs > 31 need to be dropped. 
--                       This is to prevent from null in the later tables.
-- 04/30/2013 by Ed G. - modelYearID added to METAadjustment table.

INSERT INTO WeightedAndAdjustedEmissionRate (polProcessID, sourceTypeID, modelYearID, fuelTypeID, zoneID, monthID, hourID, meanBaseRate)
SELECT er.polProcessID, er.sourceTypeID, er.modelYearID, er.fuelTypeID, 
	##context.iterLocation.zoneRecordID## as zoneID, aca.monthID, aca.hourID,
	(er.meanBaseRate * aca.ACAdjustment * meta.temperatureAdjustment * IF(ppa.pollutantID=3,meta.K,1.0)) AS meanBaseRate 
FROM SBWeightedEmissionRate er
INNER JOIN pollutantprocessassoc ppa ON (ppa.polProcessID=er.polProcessID)
INNER JOIN ACAdjustment aca ON (aca.sourceTypeID=er.sourceTypeID AND aca.polProcessID=er.polProcessID AND aca.modelYearID=er.modelYearID)
LEFT JOIN METAdjustment meta ON (meta.polProcessID=er.polProcessID AND meta.fuelTypeID=er.fuelTypeID AND meta.modelYearID=er.modelYearID  
		AND meta.monthID=aca.monthID AND meta.hourID=aca.hourID)
GROUP BY er.polProcessID, er.sourceTypeID, er.modelYearID, er.fuelTypeID, aca.monthID, aca.hourID 
order by null;

-- meta.zoneID has already been locked down to the context's zone, same with ACAdjustment's zone

-- Apply humidity effects for NOx (pollutant/process 391)
-- update WeightedAndAdjustedEmissionRate, ZoneMonthHour, FuelType
-- 	set meanBaseRate=(1.0 - (GREATEST(21.0,LEAST(specificHumidity,124.0))-75.0)*humidityCorrectionCoeff)*meanBaseRate
-- where WeightedAndAdjustedEmissionRate.polProcessID=391
-- and WeightedAndAdjustedEmissionRate.zoneID=ZoneMonthHour.zoneID
-- and WeightedAndAdjustedEmissionRate.monthID=ZoneMonthHour.monthID
-- and WeightedAndAdjustedEmissionRate.hourID=ZoneMonthHour.hourID
-- and WeightedAndAdjustedEmissionRate.fuelTypeID=FuelType.fuelTypeID;


-- FLUSH TABLES;

-- CEIC-5: Multiply Emission Rates by Activity

DROP TABLE IF EXISTS HH2;
CREATE TABLE HH2 (
       zoneID		INT NOT NULL,
       monthID		SMALLINT NOT NULL,
       hourDayID        SMALLINT NOT NULL,
       hourID		SMALLINT,
       dayID		SMALLINT,
       yearID		SMALLINT NOT NULL,
       ageID		SMALLINT NOT NULL,
       sourceTypeID 	SMALLINT NOT NULL,
       hotellingHours 	FLOAT
);

CREATE UNIQUE INDEX XPKHH2 ON HH2 (
       ageID        ASC,
       yearID        ASC,
       zoneID        ASC,
       monthID          ASC,
       hourDayID    ASC,
       sourceTypeID    ASC);

ANALYZE TABLE HH2;

-- FLUSH TABLES;

truncate table HH2;


INSERT INTO HH2 (zoneID, monthID, hourDayID, hourID, dayID, yearID, ageID, sourceTypeID, hotellingHours) 
SELECT hh.zoneID, hh.monthID, hh.hourDayID, hrdy.hourID, hrdy.dayID, hh.yearID, hh.ageID, hh.sourceTypeID, hh.hotellingHours 
FROM hotellingHours hh 
INNER JOIN hourday hrdy ON (hrdy.hourDayID=hh.hourDayID);

-- FLUSH TABLES;

DROP TABLE IF EXISTS AdjustedEmissionResults;
CREATE TABLE AdjustedEmissionResults (
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

CREATE UNIQUE INDEX XPKAdjustedEmissionResults ON AdjustedEmissionResults (
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
ANALYZE TABLE AdjustedEmissionResults;

INSERT INTO AdjustedEmissionResults (polProcessID, sourceTypeID, modelYearID, fuelTypeID, zoneID, monthID, 
				hourID, dayID, yearID, ageID, emissionQuant)
SELECT waer.polProcessID, waer.sourceTypeID, waer.modelYearID, waer.fuelTypeID, waer.zoneID, waer.monthID, 
				waer.hourID, hh.dayID, hh.yearID, hh.ageID,  
	(waer.meanBaseRate * hh.hotellingHours) AS emissionQuant 
FROM WeightedAndAdjustedEmissionRate waer
INNER JOIN HH2 hh ON (hh.zoneID=waer.zoneID AND hh.monthID=waer.monthID  
			AND hh.hourID=waer.hourID AND hh.sourceTypeID=waer.sourceTypeID)
INNER JOIN RunSpecYear ry on (hh.yearID=ry.yearID)
WHERE hh.ageID = hh.yearID - waer.modelYearID
GROUP BY waer.polProcessID, waer.sourceTypeID, waer.modelYearID, waer.fuelTypeID, 
		hh.zoneID, hh.monthID, hh.hourID, hh.dayID, yearID 
order by null;
-- FLUSH TABLES;

-- CEIC-6: Convert Results to Structure of MOVESWorkerOutput by sourceTypeID

DROP TABLE IF EXISTS MOVESWorkerOutputTmp216;
TRUNCATE MOVESWorkerOutput;
INSERT INTO MOVESWorkerOutput (
	stateID, countyID, zoneID, linkID, roadTypeID, yearID, monthID, dayID, hourID, pollutantID, 
	processID, sourceTypeID, fuelTypeID, modelYearID, SCC, emissionQuant)
SELECT ##context.iterLocation.stateRecordID## AS stateID, 
	##context.iterLocation.countyRecordID## AS countyID, aer.zoneID, lnk.linkID, lnk.roadTypeID,
	aer.yearID, aer.monthID, aer.dayID, aer.hourID, ppa.pollutantID, ppa.processID, aer.sourceTypeID, 
	aer.fuelTypeID, aer.modelYearID, NULL AS SCC, aer.emissionQuant AS emissionQuant 
FROM AdjustedEmissionResults aer 
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

DROP TABLE IF EXISTS HH2;
DROP TABLE IF EXISTS temp1;
DROP TABLE IF EXISTS temp2;
DROP TABLE IF EXISTS tmp216;
DROP TABLE IF EXISTS tappaft;
DROP TABLE IF EXISTS EmissionRate2;
DROP TABLE IF EXISTS METADjustment;
DROP TABLE IF EXISTS ACOnFraction;
DROP TABLE IF EXISTS ACActivityFraction;
DROP TABLE IF EXISTS ACAdjustment;
DROP TABLE IF EXISTS SBWeightedEmissionRate;
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate;
DROP TABLE IF EXISTS MOVESWorkerOutputTmp216;
DROP TABLE IF EXISTS adjustedemissionresults;
drop table if exists oneCountyYearGeneralFuelRatio;
drop table if exists CriteriaAndPMAuxiliaryPowerEmissions;
-- End Section Cleanup

