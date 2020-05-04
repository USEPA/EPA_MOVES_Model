-- Author Wesley Faler
-- Author Ed Campbell
-- Version 2014-04-28

-- @algorithm
-- @owner Evaporative Permeation Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

##create.AgeCategory##;
TRUNCATE AgeCategory;

##create.AverageTankTemperature##;
TRUNCATE AverageTankTemperature;

##create.County##;
TRUNCATE County;

##create.EmissionRateByAge##;
TRUNCATE EmissionRateByAge;

##create.ETOHBin##;
TRUNCATE ETOHBin;

##create.FuelSupply##;
TRUNCATE FuelSupply;

##create.FuelSubType##;
TRUNCATE FuelSubType;

##create.FuelFormulation##;
TRUNCATE FuelFormulation;

##create.HCPermeationCoeff##;
TRUNCATE HCPermeationCoeff;

##create.HourDay##;
TRUNCATE HourDay;

##create.Link##;
TRUNCATE Link;

##create.ModelYear##;
TRUNCATE ModelYear;

##create.OpModeDistribution##;
TRUNCATE OpModeDistribution;

##create.PollutantProcessAssoc##;
TRUNCATE PollutantProcessAssoc;

##create.PollutantProcessModelYear##;
TRUNCATE PollutantProcessModelYear;

##create.PollutantProcessMappedModelYear##;
TRUNCATE PollutantProcessMappedModelYear;

##create.RunSpecSourceType##;
TRUNCATE RunSpecSourceType;

##create.SourceBinDistribution##;
TRUNCATE SourceBinDistribution;

##create.SourceBin##;
TRUNCATE SourceBin;

##create.SourceHours##;
TRUNCATE SourceHours;

##create.SourceTypeModelYear##;
TRUNCATE SourceTypeModelYear;

##create.SourceTypeModelYearGroup##;
TRUNCATE SourceTypeModelYearGroup;

##create.TemperatureAdjustment##;
TRUNCATE TemperatureAdjustment;

##create.Year##;
TRUNCATE Year;

-- Section WithRegClassID
##create.RegClassSourceTypeFraction##;
TRUNCATE TABLE RegClassSourceTypeFraction;
-- End Section WithRegClassID

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
-- CREATE TABLE IF NOT EXISTS EventLog (eventRowID INTEGER UNSIGNED NOT NULL AUTO_INCREMENT, PRIMARY KEY (eventRowID), eventTime DATETIME, eventName VARCHAR(120));
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'Extracting Data';

cache SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

cache SELECT * INTO OUTFILE '##AverageTankTemperature##' FROM AverageTankTemperature
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND monthID = ##context.monthID##;

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

cache SELECT DISTINCT EmissionRateByAge.* INTO OUTFILE '##EmissionRateByAge##'
FROM EmissionRateByAge, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE RunSpecSourceFuelType.fuelTypeID = SourceBin.fuelTypeID
AND EmissionRateByAge.polProcessID = SourceBinDistribution.polProcessID
AND EmissionRateByAge.sourceBinID = SourceBin.sourceBinID
AND EmissionRateByAge.sourceBinID = SourceBinDistribution.sourceBinID
AND SourceBin.sourceBinID = SourceBinDistribution.sourceBinID
AND RunSpecSourceFuelType.sourceTypeID = SourceTypeModelYear.sourceTypeID
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##);

cache SELECT * INTO OUTFILE '##ETOHBin##'
FROM ETOHBin;

cache SELECT FuelSupply.* INTO OUTFILE '##FuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID=FuelSupply.monthGroupID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND monthID = ##context.monthID##;

cache SELECT DISTINCT FuelSubType.* INTO OUTFILE '##FuelSubType##'
FROM FuelSubType
INNER JOIN RunSpecFuelType ON (RunSpecFuelType.fuelTypeID = FuelSubType.fuelTypeID);

cache SELECT ff.* INTO OUTFILE '##FuelFormulation##'
FROM FuelFormulation ff
INNER JOIN FuelSupply fs ON fs.fuelFormulationID = ff.fuelFormulationID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID
INNER JOIN RunSpecMonthGroup rsmg ON rsmg.monthGroupID = fs.monthGroupID
WHERE fuelRegionID = ##context.fuelRegionID## AND
yearID = ##context.year##
GROUP BY ff.FuelFormulationID ORDER BY NULL;

cache SELECT * INTO OUTFILE '##HCPermeationCoeff##'
FROM HCPermeationCoeff
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT DISTINCT HourDay.* INTO OUTFILE '##HourDay##'
FROM HourDay,RunSpecHour,RunSpecDay
WHERE HourDay.dayID = RunSpecDay.dayID
AND HourDay.hourID = RunSpecHour.hourID;

cache SELECT Link.* INTO OUTFILE '##Link##'
FROM Link WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT * INTO OUTFILE '##ModelYear##'
FROM ModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT OpModeDistribution.* INTO OUTFILE '##OpModeDistribution##'
FROM OpModeDistribution, RunSpecSourceType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND linkID = ##context.iterLocation.linkRecordID##
AND RunSpecSourceType.sourceTypeID = OpModeDistribution.sourceTypeID;

cache SELECT * INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE processID=##context.iterProcess.databaseKey##;

cache SELECT * INTO OUTFILE '##PollutantProcessModelYear##'
FROM PollutantProcessModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND polProcessID IN (##pollutantProcessIDs##);

cache SELECT * INTO OUTFILE '##PollutantProcessMappedModelYear##'
FROM PollutantProcessMappedModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND polProcessID IN (##pollutantProcessIDs##);

cache SELECT * INTO OUTFILE '##RunSpecSourceType##'
FROM RunSpecSourceType;

cache SELECT DISTINCT SourceBinDistribution.* INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT SourceBin.* INTO OUTFILE '##SourceBin##'
FROM RunSpecFuelType
INNER JOIN SourceBin ON (SourceBin.fuelTypeID = RunSpecFuelType.fuelTypeID);

cache SELECT * INTO OUTFILE '##SourceHours##' FROM SourceHours
WHERE monthID = ##context.monthID##
AND yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##;

cache SELECT SourceTypeModelYear.* INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear,RunSpecSourceType
WHERE SourceTypeModelYear.sourceTypeID = RunSpecSourceType.sourceTypeID
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT SourceTypeModelYearGroup.* INTO OUTFILE '##SourceTypeModelYearGroup##'
FROM SourceTypeModelYearGroup,RunSpecSourceType
WHERE SourceTypeModelYearGroup.sourceTypeID = RunSpecSourceType.sourceTypeID;

cache SELECT DISTINCT TemperatureAdjustment.* INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
INNER JOIN RunSpecFuelType ON (RunSpecFuelType.fuelTypeID = TemperatureAdjustment.fuelTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT Year.* INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

-- Section WithRegClassID
cache select *
into outfile '##RegClassSourceTypeFraction##'
from RegClassSourceTypeFraction
where modelYearID <= ##context.year##
and modelYearID >= ##context.year## - 30;
-- End Section WithRegClassID

-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'End Extracting Data';

-- End Section Extract Data

-- Section Processing

-- Create tables needed for processing
-- CREATE TABLE IF NOT EXISTS EventLog (eventRowID INTEGER UNSIGNED NOT NULL AUTO_INCREMENT, PRIMARY KEY (eventRowID), eventTime DATETIME, eventName VARCHAR(120));

DROP TABLE IF EXISTS SourceBinDistributionByAge;
CREATE TABLE SourceBinDistributionByAge (
	sourceTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	ageGroupID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceBinID	BIGINT NOT NULL,
	sourceBinActivityFraction FLOAT
);
	
CREATE UNIQUE INDEX XPKSourceBinDistributionByAge ON SourceBinDistributionByAge (
	sourceTypeID ASC,
	modelYearID ASC,
	ageGroupID ASC,
	polProcessID ASC,
	sourceBinID	ASC
);

DROP TABLE IF EXISTS SBWeightedPermeationRate;
CREATE TABLE SBWeightedPermeationRate (
	zoneID BIGINT NOT NULL, 
	yearID SMALLINT NOT NULL, 
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL, 
	regClassID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL, 
	fuelTypeID SMALLINT NOT NULL,
	meanBaseRate FLOAT
);

CREATE UNIQUE INDEX XPKSBWeightedPermeationRate ON SBWeightedPermeationRate (
	zoneID ASC, 
	yearID ASC, 
	polProcessID ASC,
	sourceTypeID ASC, 
	regClassID ASC,
	modelYearID ASC, 
	fuelTypeID ASC
);

DROP TABLE IF EXISTS TemperatureAdjustByOpMode;
CREATE TABLE TemperatureAdjustByOpMode (
    zoneID	INTEGER NOT NULL,
    monthID	SMALLINT NOT NULL,
    hourDayID SMALLINT NOT NULL,
    tankTemperatureGroupID SMALLINT NOT NULL,
    opModeID SMALLINT NOT NULL,
    polProcessID int NOT NULL,
    fuelTypeID SMALLINT NOT NULL,
    modelYearID SMALLINT NOT NULL,
    temperatureAdjustByOpMode	FLOAT
);

DROP TABLE IF EXISTS WeightedTemperatureAdjust;
CREATE TABLE WeightedTemperatureAdjust (
    linkID INTEGER NOT NULL,
    monthID SMALLINT NOT NULL,
    hourDayID SMALLINT NOT NULL,
    tankTemperatureGroupID SMALLINT NOT NULL,
    sourceTypeID SMALLINT NOT NULL,
    polProcessID int NOT NULL,
    fuelTypeID SMALLINT NOT NULL,
    modelYearID SMALLINT NOT NULL,
    weightedTemperatureAdjust FLOAT
);

CREATE UNIQUE INDEX XPKWeightedTemperatureAdjust ON WeightedTemperatureAdjust (
    linkID ASC,
    monthID ASC,
    hourDayID ASC,
    tankTemperatureGroupID ASC,
    sourceTypeID ASC,
    polProcessID ASC,
    fuelTypeID ASC,
    modelYearID ASC
);

DROP TABLE IF EXISTS WeightedFuelAdjustment;
CREATE TABLE WeightedFuelAdjustment (
    countyID INTEGER NOT NULL,
    fuelYearID	SMALLINT NOT NULL,
    monthGroupID SMALLINT NOT NULL,
    polProcessID int NOT NULL,
    modelYearID SMALLINT NOT NULL,
    sourceTypeID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
    weightedFuelAdjustment FLOAT
);

CREATE UNIQUE INDEX XPKWeightedFuelAdjustment ON WeightedFuelAdjustment (
    countyID ASC,
    fuelYearID	ASC,
    monthGroupID ASC,
    polProcessID ASC,
    modelYearID ASC,
    fuelTypeID ASC,
    sourceTypeID ASC
);

DROP TABLE IF EXISTS FuelAdjustedEmissionRate;
CREATE TABLE FuelAdjustedEmissionRate (
    zoneID INTEGER NOT NULL,
	yearID SMALLINT NOT NULL,
    polProcessID int NOT NULL,
    sourceTypeID SMALLINT NOT NULL,
	regClassID SMALLINT NOT NULL,
    modelYearID	SMALLINT NOT NULL,
	fuelTypeID	SMALLINT NOT NULL,
    fuelAdjustedEmissionRate FLOAT
);

CREATE UNIQUE INDEX XPKFuelAdjustedEmissionRate ON FuelAdjustedEmissionRate (
    zoneID ASC,
	yearID ASC,
    polProcessID ASC,
    sourceTypeID ASC,
    regClassID ASC,
    modelYearID	ASC,
	fuelTypeID ASC
);

DROP TABLE IF EXISTS FuelAdjustedEmissionQuant;
CREATE TABLE FuelAdjustedEmissionQuant (
    linkID INTEGER NOT NULL,
    hourDayID SMALLINT NOT NULL,
    monthID SMALLINT NOT NULL,
    yearID SMALLINT NOT NULL,
    modelYearID SMALLINT NOT NULL,
    sourceTypeID SMALLINT NOT NULL,
    regClassID SMALLINT NOT NULL,
    polProcessID int NOT NULL,
    fuelTypeID SMALLINT NOT NULL,
    fuelAdjustedEmissionQuant FLOAT
);

CREATE UNIQUE INDEX XPKFuelAdjustedEmissionQuant ON FuelAdjustedEmissionQuant (
    linkID ASC, 
    hourDayID ASC,
    monthID ASC, 
    yearID ASC,
    modelYearID ASC,
    sourceTypeID ASC,
    regClassID ASC,
    polProcessID ASC,
    fuelTypeID ASC
);

update FuelFormulation set ETOHVolume=0 where ETOHVolume is null;

loop ##loop.sourceTypeID##;
select sourceTypeID from RunSpecSourceType;

--
-- PC-1: Weight Emission Rates by Source Bin
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'PC-1a: Weight Emission Rates by Source Bin';

TRUNCATE SourceBinDistributionByAge;

-- @algorithm Add ageGroupID to SourceBinDistribution.sourceBinActivityFraction using model year and the calendar year.
INSERT INTO SourceBinDistributionByAge (sourceTypeID, modelYearID, ageGroupID,
	polProcessID, sourceBinID, sourceBinActivityFraction)
SELECT sourceTypeID, modelYearID, ageGroupID, polProcessID, 
	sourceBinID, sourceBinActivityFraction
FROM SourceBinDistribution sbd
INNER JOIN SourceTypeModelYear stmy ON (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID)
INNER JOIN AgeCategory ac ON (ac.ageID = ##context.year## - modelYearID)
WHERE sourceTypeID = ##loop.sourceTypeID##;

ANALYZE TABLE SourceBinDistributionByAge;

-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'PC-1b: Weight Emission Rates by Source Bin';

TRUNCATE SBWeightedPermeationRate;

-- Section WithRegClassID

-- @algorithm SBWeightedPermeationRate.meanBaseRate = sourceBinActivityFraction * EmissionRateByAge.meanBaseRate * regClassFraction
INSERT INTO SBWeightedPermeationRate (zoneID, yearID, polProcessID, sourceTypeID, regClassID,
	modelYearID, fuelTypeID, meanBaseRate)
SELECT ##context.iterLocation.zoneRecordID##, ##context.year## AS yearID, sbda.polProcessID, 
	sbda.sourceTypeID, stf.regClassID, sbda.modelYearID, sb.fuelTypeID, 
	SUM(sourceBinActivityFraction*meanBaseRate*stf.regClassFraction) AS meanBaseRate
FROM SourceBinDistributionByAge sbda
INNER JOIN EmissionRateByAge era ON (era.sourceBinID=sbda.sourceBinID AND
	era.polProcessID=sbda.polProcessID AND era.ageGroupID=sbda.ageGroupID)
INNER JOIN SourceBin sb ON (sb.sourceBinID=era.sourceBinID)
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = sbda.sourceTypeID
	and stf.fuelTypeID = sb.fuelTypeID
	and stf.modelYearID = sbda.modelYearID)
WHERE sbda.sourceTypeID = ##loop.sourceTypeID##
GROUP BY sbda.polProcessID, sbda.sourceTypeID, stf.regClassID, sbda.modelYearID, sb.fuelTypeID
order by null;
-- End Section WithRegClassID

-- Section NoRegClassID
INSERT INTO SBWeightedPermeationRate (zoneID, yearID, polProcessID, sourceTypeID, regClassID,
	modelYearID, fuelTypeID, meanBaseRate)
SELECT ##context.iterLocation.zoneRecordID##, ##context.year## AS yearID, sbda.polProcessID, 
	sbda.sourceTypeID, 0 as regClassID, modelYearID, sb.fuelTypeID, 
	SUM(sourceBinActivityFraction*meanBaseRate) AS meanBaseRate
FROM SourceBinDistributionByAge sbda
INNER JOIN EmissionRateByAge era ON (era.sourceBinID=sbda.sourceBinID AND
	era.polProcessID=sbda.polProcessID AND era.ageGroupID=sbda.ageGroupID)
INNER JOIN SourceBin sb ON (sb.sourceBinID=era.sourceBinID)
WHERE sbda.sourceTypeID = ##loop.sourceTypeID##
GROUP BY sbda.polProcessID, sbda.sourceTypeID, modelYearID, sb.fuelTypeID
order by null;
-- End Section NoRegClassID

ANALYZE TABLE SBWeightedPermeationRate;

--
-- PC-2: Calculate weighted temperature adjustment
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'PC-2a: Calculate weighted temperature adjustment';

TRUNCATE TemperatureAdjustByOpMode;

-- @algorithm temperatureAdjustByOpMode = tempAdjustTermA*EXP(tempAdjustTermB*averageTankTemperature)
INSERT INTO TemperatureAdjustByOpMode (
	zoneID, monthID, hourDayID, tankTemperatureGroupID, opModeID, polProcessID,
	fuelTypeID, temperatureAdjustByOpMode,
	modelYearID )
SELECT zoneID, monthID, hourDayID, tankTemperatureGroupID, opModeID, polProcessID, fuelTypeID,
	tempAdjustTermA*EXP(tempAdjustTermB*averageTankTemperature) AS temperatureAdjustByOpMode,
	modelYearID
FROM AverageTankTemperature
INNER JOIN TemperatureAdjustment
INNER JOIN ModelYear my on (modelYearID between minModelYearID and maxModelYearID);

create index ixTemperatureAdjustByOpMode1 on TemperatureAdjustByOpMode (
	hourDayID asc,
	polProcessID asc,
	opModeID asc,
	zoneID asc,
	modelYearID asc
);


-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'PC-2b: Calculate weighted temperature adjustment';

TRUNCATE WeightedTemperatureAdjust;

-- @algorithm weightedTemperatureAdjust = sum(temperatureAdjustByOpMode * opModeFraction) across all operating modes.
INSERT INTO WeightedTemperatureAdjust (
	linkID, monthID, hourDayID, tankTemperatureGroupID, sourceTypeID,
	polProcessID, fuelTypeID, modelYearID, weightedTemperatureAdjust)
SELECT omd.linkID, monthID, taom.hourDayID, tankTemperatureGroupID,
	sourceTypeID, taom.polProcessID, fuelTypeID, modelYearID,
	SUM(temperatureAdjustByOpMode*opModeFraction) AS weightedTemperatureAdjust
FROM TemperatureAdjustByOpMode taom
INNER JOIN OpModeDistribution omd ON (omd.hourDayID=taom.hourDayID AND
	omd.polProcessID=taom.polProcessID AND taom.opModeID=omd.opModeID)
INNER JOIN Link l ON (l.linkID=omd.linkID AND l.zoneID=taom.zoneID)
WHERE sourceTypeID = ##loop.sourceTypeID##
GROUP BY omd.linkID, monthID, taom.hourDayID, tankTemperatureGroupID,
	sourceTypeID, taom.polProcessID, fuelTypeID, modelYearID
ORDER BY NULL; 

ANALYZE TABLE WeightedTemperatureAdjust;

--
-- PC-3: Calculate weighted fuel adjustment
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'PC-3: Calculate weighted fuel adjustment';

TRUNCATE WeightedFuelAdjustment;

-- INSERT INTO WeightedFuelAdjustment (
-- countyID, fuelYearID, monthGroupID, polProcessID, modelYearID, fuelTypeID,
-- sourceTypeID, weightedFuelAdjustment)
-- SELECT countyID, fs.fuelYearID, monthGroupID, fa.polProcessID, modelYearID, fst.fuelTypeID,
-- sourceTypeID, SUM(marketShare*fuelAdjustment) AS weightedFuelAdjustment
-- FROM FuelAdjustment fa
-- INNER JOIN PollutantProcessModelYear ppmy ON (ppmy.polProcessID=fa.polProcessID AND
-- ppmy.fuelMYGroupID=fa.fuelMYGroupid)
-- INNER JOIN FuelSupply fs ON (fs.fuelFormulationID=fa.fuelFormulationID)
-- INNER JOIN Year y ON (y.fuelYearID=fs.fuelYearID)
-- INNER JOIN FuelFormulation ff ON (ff.fuelFormulationID=fs.fuelFormulationID)
-- INNER JOIN FuelSubType fst ON (fst.fuelSubTypeID=ff.fuelSubTypeID)
-- WHERE y.yearID=??context.year??
-- AND sourceTypeID = ##loop.sourceTypeID##
-- GROUP BY countyID, fs.fuelYearID, monthGroupID, fa.polProcessID, modelYearID, fst.fuelTypeID,
-- sourceTypeID
-- ORDER BY NULL

-- @algorithm weightedFuelAdjustment = sum(marketShare*(fuelAdjustment+GPAFract*(fuelAdjustmentGPA-fuelAdjustment))) across fuel formulations in the fuel supply.
INSERT INTO WeightedFuelAdjustment (
	countyID, fuelYearID, monthGroupID, polProcessID, modelYearID, fuelTypeID,
	sourceTypeID, weightedFuelAdjustment)
SELECT c.countyID, fs.fuelYearID, fs.monthGroupID, fa.polProcessID, ppmy.modelYearID, fst.fuelTypeID,
	##loop.sourceTypeID## as sourceTypeID, 
	SUM(marketShare*(fuelAdjustment+GPAFract*(fuelAdjustmentGPA-fuelAdjustment))) AS weightedFuelAdjustment
FROM FuelSupply fs
INNER JOIN County c
INNER JOIN HCPermeationCoeff fa
INNER JOIN PollutantProcessMappedModelYear ppmy ON (ppmy.polProcessID=fa.polProcessID
	AND ppmy.fuelMYGroupID=fa.fuelMYGroupid)
INNER JOIN Year y ON (y.fuelYearID=fs.fuelYearID)
INNER JOIN FuelFormulation ff ON (ff.fuelFormulationID=fs.fuelFormulationID)
INNER JOIN ETOHBin ebin ON (ebin.etohThreshID=fa.etohThreshID
	AND etohThreshLow <= ff.ETOHVolume AND ff.ETOHVolume < etohThreshHigh)
INNER JOIN FuelSubType fst ON (fst.fuelSubTypeID=ff.fuelSubTypeID)
WHERE y.yearID=##context.year##
AND fs.fuelRegionID = ##context.fuelRegionID##
GROUP BY c.countyID, fs.fuelYearID, fs.monthGroupID, fa.polProcessID, ppmy.modelYearID, fst.fuelTypeID, sourceTypeID
ORDER BY NULL;

--AND sourceTypeID = ##loop.sourceTypeID##  -- sourceTypeID is not in any of the new tables

ANALYZE TABLE WeightedFuelAdjustment;

--
-- PC-4: Calculate Fuel Adjusted Mean Base Rate
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'PC-4: Calculate Fuel Adjusted Mean Base Rate';

TRUNCATE FuelAdjustedEmissionRate;

-- @algorithm fuelAdjustedEmissionRate = SBWeightedPermeationRate.meanBaseRate * weightedFuelAdjustment
INSERT INTO FuelAdjustedEmissionRate (zoneID, yearID, polProcessID,
	sourceTypeID, regClassID, modelYearID, fuelTypeID, fuelAdjustedEmissionRate)
SELECT zoneID, sbwpr.yearID, sbwpr.polProcessID, sbwpr.sourceTypeID, sbwpr.regClassID,
	sbwpr.modelYearID, sbwpr.fuelTypeID, meanBaseRate*weightedFuelAdjustment 
FROM SBWeightedPermeationRate sbwpr
INNER JOIN WeightedFuelAdjustment wfa ON (wfa.polProcessID=sbwpr.polProcessID AND
	wfa.modelYearID=sbwpr.modelYearID AND wfa.sourceTypeID=sbwpr.sourceTypeID AND
	wfa.fuelTypeID=sbwpr.fuelTypeID)
INNER JOIN Year y ON (y.yearID=sbwpr.yearID AND y.fuelYearID=wfa.fuelYearID)
WHERE sbwpr.sourceTypeID = ##loop.sourceTypeID##;

ANALYZE TABLE FuelAdjustedEmissionRate;

--
-- PC-5: Calculate fuel adjusted emissionQuant
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'PC-5: Calculate fuel adjusted emissionQuant';

TRUNCATE FuelAdjustedEmissionQuant;

-- @algorithm fuelAdjustedEmissionQuant = fuelAdjustedEmissionRate * sourceHours
INSERT INTO FuelAdjustedEmissionQuant (
	linkID, hourDayID, monthID, yearID, modelYearID,
	sourceTypeID, regClassID, polProcessID, fuelTypeID, fuelAdjustedEmissionQuant) 
SELECT sh.linkID, sh.hourDayID, sh.monthID, fambr.yearID, 
	fambr.modelYearID, fambr.sourceTypeID, fambr.regClassID, fambr.polProcessID, fambr.fuelTypeID,
	fuelAdjustedEmissionRate*sourceHours AS fuelAdjustedEmissionQuant
FROM SourceHours sh 
INNER JOIN FuelAdjustedEmissionRate fambr ON (fambr.yearID=sh.yearID 
	AND fambr.modelYearID=sh.yearID-sh.ageID AND fambr.sourceTypeID=sh.sourceTypeID)
INNER JOIN Link l ON (l.linkID=sh.linkID AND l.zoneID=fambr.zoneID)
WHERE sh.yearID=##context.year##
AND fambr.sourceTypeID = ##loop.sourceTypeID##;

ANALYZE TABLE FuelAdjustedEmissionQuant;

--
-- PC-6: Calculate EmissionQuant with Temperature Adjustment
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'PC-6: Calculate EmissionQuant with Temperature Adjustment';

-- @algorithm emissionQuant = weightedTemperatureAdjust * fuelAdjustedEmissionQuant
INSERT INTO MOVESWorkerOutput (
	stateID, countyID, zoneID, linkID, roadTypeID, yearID, monthID, dayID, hourID,
	pollutantID, processID, sourceTypeID, regClassID, modelYearID, fuelTypeID, SCC, emissionQuant)
SELECT stateID, l.countyID, l.zoneID, faeq.linkID, roadTypeID, yearID, faeq.monthID, dayID, hourID, pollutantID,
	processID, faeq.sourceTypeID, faeq.regClassID, faeq.modelYearID, faeq.fuelTypeID, NULL AS SCC, 
	weightedTemperatureAdjust*fuelAdjustedEmissionQuant AS emissionQuant
FROM FuelAdjustedEmissionQuant faeq 
INNER JOIN WeightedTemperatureAdjust wta ON (
	wta.linkID=faeq.linkID AND wta.hourDayID=faeq.hourDayID AND
	wta.monthID=faeq.monthID AND wta.sourceTypeID=faeq.sourceTypeID AND
	wta.polProcessID=faeq.polProcessID AND wta.fuelTypeID=faeq.fuelTypeID AND
	wta.modelYearID=faeq.modelYearID)
INNER JOIN PollutantProcessAssoc ppa ON (
	ppa.polProcessID=faeq.polProcessID)
INNER JOIN PollutantProcessModelYear ppmy ON (
	ppmy.polProcessID=ppa.polProcessID AND
	ppmy.modelYearID=faeq.modelYearID)
INNER JOIN SourceTypeModelYearGroup stmyg ON (
	stmyg.sourceTypeID=faeq.sourceTypeID AND
	stmyg.modelYearGroupID=ppmy.modelYearGroupID AND
	stmyg.tankTemperatureGroupID=wta.tankTemperatureGroupID)
INNER JOIN HourDay hd ON (hd.hourDayID=faeq.hourDayID)
INNER JOIN Link l ON (l.linkID=faeq.linkID)
INNER JOIN County c ON (c.countyID=l.countyID)
WHERE faeq.sourceTypeID = ##loop.sourceTypeID##;

end loop ##loop.sourceTypeID##;

alter table TemperatureAdjustByOpMode drop index ixTemperatureAdjustByOpMode1;

-- End Section Processing
-- Section Cleanup
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'Section Cleanup';

DROP TABLE IF EXISTS FuelAdjustedEmissionQuant;
DROP TABLE IF EXISTS FuelAdjustedEmissionRate;
DROP TABLE IF EXISTS SBWeightedPermeationRate;
DROP TABLE IF EXISTS SourceBinDistributionByAge;
DROP TABLE IF EXISTS TemperatureAdjustByOpMode;
DROP TABLE IF EXISTS WeightedFuelAdjustment;
DROP TABLE IF EXISTS WeightedTemperatureAdjust;
-- End Section Cleanup
