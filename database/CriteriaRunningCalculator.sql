-- Version 2013-11-19
-- Author Wesley Faler
-- Author Ed Glover, EPA
-- Author epa - Mitch C. (performance rewrites)
-- Author epa - ahuang (Bug 431 - Modified CREC 8 to disable humidity effects for pollutants other than
--                      NOx. Add Temporary table WeightedAndAdjustedEmissionRate_TEMP1
--                      and WeightedAndAdjustedEmissionRate_TEMP2).
-- Data extraction into SHO and steps 5,6,and 7b modified by EPA-Mitch C 
--    in attempt to fix bug 205
-- Step 4d join condition to Link fixed by EPA Mitch C and Gwo S.

-- Section Create Remote Tables for Extracted Data

##create.AgeCategory##;
TRUNCATE AgeCategory;

##create.County##;
TRUNCATE County;

##create.criteriaRatio##;
TRUNCATE criteriaRatio;

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

##create.ModelYear##;
TRUNCATE ModelYear;

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

##create.PollutantProcessMappedModelYear##;
TRUNCATE PollutantProcessMappedModelYear;

##create.sho##;
TRUNCATE sho;

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

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

cache SELECT fuelTypeID,
	fuelFormulationID,
	polProcessID,
	pollutantID,
	processID,
	sourceTypeID,
	MYRMAP(modelYearID) as modelYearID,
	ageID,
	ratio,
	ratioGPA,
	ratioNoSulfur
INTO OUTFILE '##criteriaRatio##'
FROM criteriaRatio
WHERE polProcessID IN (##pollutantProcessIDs##)
AND modelYearID = MYMAP(##context.year## - ageID);

-- SELECT DISTINCT EmissionRateByAge.* INTO OUTFILE '??EmissionRateByAge??'
-- FROM EmissionRateByAge, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
-- WHERE RunSpecSourceFuelType.fuelTypeID = SourceBin.fuelTypeID
-- AND EmissionRateByAge.polProcessID = SourceBinDistribution.polProcessID
-- AND EmissionRateByAge.sourceBinID = SourceBin.sourceBinID
-- AND EmissionRateByAge.sourceBinID = SourceBinDistribution.sourceBinID
-- AND SourceBin.sourceBinID = SourceBinDistribution.sourceBinID
-- AND RunSpecSourceFuelType.sourceTypeID = SourceTypeModelYear.sourceTypeID
-- AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
-- AND SourceTypeModelYear.modelYearID <= ??context.year??
-- AND SourceTypeModelYear.modelYearID >= ??context.year?? - 30
-- AND EmissionRateByAge.polProcessID IN (??pollutantProcessIDs??);

-- Section FirstBundle
drop table if exists CriteriaRunningEmissionRateByAge;

create table CriteriaRunningEmissionRateByAge
SELECT EmissionRateByAge.*
FROM EmissionRateByAge
WHERE EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##)
AND EmissionRateByAge.sourceBinID in (##macro.csv.all.sourceBinID##);
-- End Section FirstBundle

cache SELECT CriteriaRunningEmissionRateByAge.* INTO OUTFILE '##EmissionRateByAge##' FROM CriteriaRunningEmissionRateByAge;

SELECT ff.* INTO OUTFILE '##FuelFormulation##'
FROM FuelFormulation ff
INNER JOIN FuelSupply fs ON fs.fuelFormulationID = ff.fuelFormulationID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID
INNER JOIN RunSpecMonthGroup rsmg ON rsmg.monthGroupID = fs.monthGroupID
INNER JOIN MonthOfAnyYear moy ON moy.monthGroupID = rsmg.monthGroupID
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND moy.monthID = ##context.monthID##
GROUP BY ff.FuelFormulationID ORDER BY NULL;

cache SELECT * INTO OUTFILE '##FuelSubtype##'
FROM FuelSubtype;

SELECT FuelSupply.* INTO OUTFILE '##FuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN MonthOfAnyYear moy ON (moy.monthGroupID = RunSpecMonthGroup.monthGroupID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND moy.monthID = ##context.monthID##;

cache SELECT FuelType.* INTO OUTFILE '##FuelType##'
FROM FuelType
WHERE fuelTypeID in (##macro.csv.all.fuelTypeID##);

cache SELECT faca.* INTO OUTFILE '##FullACAdjustment##'
FROM FullACAdjustment faca
WHERE faca.sourceTypeID in (##macro.csv.all.sourceTypeID##)
AND faca.polProcessID in (##macro.csv.all.polProcessID##);

cache SELECT HourDay.* INTO OUTFILE '##HourDay##'
FROM HourDay
WHERE hourDayID in (##macro.csv.all.hourDayID##);

cache SELECT DISTINCT IMCoverage.* INTO OUTFILE '##IMCoverage##'
FROM IMCoverage
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = IMCoverage.fuelTypeID
	and RunSpecSourceFuelType.sourceTypeID = IMCoverage.sourceTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##)
AND countyID = ##context.iterLocation.countyRecordID## 
AND yearID = ##context.year##
AND useIMyn = 'Y';

cache SELECT DISTINCT IMFactor.* INTO OUTFILE '##IMFactor##'
FROM IMFactor
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = IMFactor.fuelTypeID
	and RunSpecSourceFuelType.sourceTypeID = IMFactor.sourceTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

SELECT Link.* INTO OUTFILE '##Link##'
FROM Link WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT * INTO OUTFILE '##ModelYear##'
FROM ModelYear;

SELECT MonthGroupHour.* INTO OUTFILE '##MonthGroupHour##'
FROM MonthGroupHour
INNER JOIN MonthOfAnyYear moy ON (moy.monthGroupID = MonthGroupHour.monthGroupID)
WHERE moy.monthID = ##context.monthID##
AND MonthGroupHour.hourID in (##macro.csv.all.hourID##);

SELECT MonthOfAnyYear.* INTO OUTFILE '##MonthOfAnyYear##'
FROM MonthOfAnyYear
WHERE MonthOfAnyYear.monthID = ##context.monthID##;

cache SELECT OpModeDistribution.* INTO OUTFILE '##OpModeDistribution##'
FROM OpModeDistribution
WHERE polProcessID IN (##pollutantProcessIDs##)
AND linkID = ##context.iterLocation.linkRecordID##
AND sourceTypeID in (##macro.csv.all.sourceTypeID##);

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

SELECT * INTO OUTFILE '##SHO##'
FROM SHO
WHERE linkID = ##context.iterLocation.linkRecordID##
AND monthID = ##context.monthID##
AND yearID = ##context.year##;

-- SELECT DISTINCT SourceBin.* INTO OUTFILE '??SourceBin??'
-- FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
-- WHERE polProcessID IN (??pollutantProcessIDs??)
-- AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
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
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, SourceTypeModelYear
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30;

cache SELECT SourceTypeAge.* INTO OUTFILE '##SourceTypeAge##'
FROM SourceTypeAge
WHERE SourceTypeAge.sourceTypeID in (##macro.csv.all.sourceTypeID##);

cache SELECT SourceTypeModelYear.* INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear
WHERE SourceTypeModelYear.sourceTypeID in (##macro.csv.all.sourceTypeID##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT TemperatureAdjustment.* INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
WHERE polProcessID IN (##pollutantProcessIDs##)
AND fuelTypeID in (##macro.csv.all.fuelTypeID##);

cache SELECT Year.* INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

SELECT DISTINCT ZoneMonthHour.* INTO OUTFILE '##ZoneMonthHour##'
FROM ZoneMonthHour
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND ZoneMonthHour.monthID = ##context.monthID##
AND ZoneMonthHour.hourID in (##macro.csv.all.hourID##);

-- End Section Extract Data

-- Section Processing

-- 
-- CREC 1-a: Complete I/M adjustment fraction information
--
DROP TABLE IF EXISTS IMCoverageMergedUngrouped;
CREATE TABLE IMCoverageMergedUngrouped (
       processID SMALLINT NOT NULL,
       pollutantID SMALLINT NOT NULL,
       modelYearID SMALLINT NOT NULL,
       fuelTypeID SMALLINT NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       IMAdjustFract FLOAT
);

CREATE INDEX XPKIMCoverageMergedUngrouped ON IMCoverageMergedUngrouped
(
       processID ASC,
       pollutantID ASC,
       modelYearID ASC,
       fuelTypeID ASC,
       sourceTypeID ASC
);

INSERT INTO IMCoverageMergedUngrouped (
	processID,pollutantID,modelYearID,fuelTypeID,sourceTypeID,IMAdjustFract)
SELECT
 ppa.processID,
 ppa.pollutantID,
 ppmy.modelYearID,
 imf.fuelTypeID,
 imc.sourceTypeID,
 sum(IMFactor*complianceFactor*.01) AS IMAdjustFract
FROM PollutantProcessMappedModelYear ppmy
INNER JOIN PollutantProcessAssoc ppa on (ppa.polProcessID=ppmy.polProcessID)
INNER JOIN IMFactor imf ON (
	imf.polProcessID = ppa.polProcessID
	AND imf.IMModelYearGroupID = ppmy.IMModelYearGroupID)
INNER JOIN AgeCategory ac ON (
	ac.ageGroupID = imf.ageGroupID)
INNER JOIN IMCoverage imc ON (
	imc.polProcessID = imf.polProcessID
	AND imc.inspectFreq = imf.inspectFreq
	AND imc.testStandardsID = imf.testStandardsID
	AND imc.sourceTypeID = imf.sourceTypeID
	AND imc.fuelTypeID = imf.fuelTypeID
	AND imc.begModelYearID <= ppmy.modelYearID
	AND imc.endModelYearID >= ppmy.modelYearID)
WHERE imc.countyID = ##context.iterLocation.countyRecordID##
AND imc.yearID = ##context.year##
AND ppmy.modelYearID = ##context.year##-ageID
AND ppmy.polProcessID IN (##pollutantProcessIDs##)
GROUP BY  ppa.processID,
 ppa.pollutantID,
 ppmy.modelYearID,
 imf.fuelTypeID,
 imc.sourceTypeID;

-- 
-- CREC 2-a: Combine GPA and non GPA fuel adjustment factors 
--
DROP TABLE IF EXISTS CountyFuelAdjustment;
CREATE TABLE CountyFuelAdjustment (
       countyID INTEGER NOT NULL,
       polProcessID int NOT NULL,
       modelYearID INTEGER NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       fuelFormulationID SMALLINT NOT NULL,
       fuelAdjustment FLOAT
);

CREATE INDEX CountyFuelAdjustment1 ON CountyFuelAdjustment
(
       polProcessID ASC,
       modelYearID ASC
);

CREATE INDEX CountyFuelAdjustment2 ON CountyFuelAdjustment
(
       fuelFormulationID ASC
);

INSERT INTO CountyFuelAdjustment
SELECT countyID, polProcessID, modelYearID, sourceTypeID,
fuelFormulationID, ratio+GPAFract*(ratioGPA-ratio)
FROM criteriaRatio
INNER JOIN County c;

-- 
-- CREC 2-b: Aggregate county fuel adjustments to fuel type
--
DROP TABLE IF EXISTS FuelSupplyWithFuelType;
CREATE TABLE FuelSupplyWithFuelType (
	countyID INTEGER NOT NULL,
	yearID SMALLINT NOT NULL,
	monthID SMALLINT NOT NULL,
	fuelFormulationID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	marketShare FLOAT
);

CREATE INDEX FuelSupplyWithFuelType1 ON FuelSupplyWithFuelType
(
       fuelFormulationID ASC
);

INSERT INTO FuelSupplyWithFuelType
SELECT ##context.iterLocation.countyRecordID## as countyID, yearID, monthID, fs.fuelFormulationID, fuelTypeID, marketShare
FROM FuelSupply fs
INNER JOIN FuelFormulation ff ON ff.fuelFormulationID = fs.fuelFormulationID
INNER JOIN FuelSubType fst ON fst.fuelSubTypeID = ff.fuelSubTypeID
INNER JOIN MonthOfAnyYear may ON fs.monthGroupID = may.monthGroupID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID 
WHERE y.yearID = ##context.year##;

DROP TABLE IF EXISTS FuelSupplyAdjustment;
CREATE TABLE FuelSupplyAdjustment (
	countyID INTEGER NOT NULL,
	yearID SMALLINT NOT NULL,
	monthID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	modelYearID SMALLINT NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	fuelAdjustment FLOAT
);

CREATE UNIQUE INDEX XPKFuelSupplyAdjustment ON FuelSupplyAdjustment
(
       countyID ASC,
       yearID ASC,
       monthID ASC,
       polProcessID ASC,
       modelYearID ASC,
       sourceTypeID ASC,
       fuelTypeID ASC
);

INSERT INTO FuelSupplyAdjustment
SELECT cfa.countyID, yearID, monthID, cfa.polProcessID, cfa.modelYearID,
	sourceTypeID, fuelTypeID, SUM(fuelAdjustment*marketShare)
FROM CountyFuelAdjustment cfa 
INNER JOIN FuelSupplyWithFuelType fsft ON (
	fsft.fuelFormulationID = cfa.fuelFormulationID)
GROUP BY cfa.countyID, yearID, monthID, cfa.polProcessID, cfa.modelYearID,
	sourceTypeID, fuelTypeID ORDER BY modelYearID ASC;
-- Note no need to join fsft and cfa on countyID since both tables already filtered to 1 county
-- 
-- CREC 3: Calculate temperature adjustment factors
--
DROP TABLE IF EXISTS METADjustment;
CREATE TABLE METAdjustment (
       zoneID	INTEGER NOT NULL,
       monthID	SMALLINT NOT NULL,
       hourID	SMALLINT NOT NULL,
       polProcessID int NOT NULL,
       fuelTypeID	SMALLINT NOT NULL,
       modelYearID	SMALLINT NOT NULL,
       minModelYearID	INT	NOT NULL,
       maxModelYearID	INT NOT NULL,
       temperatureAdjustment FLOAT
);

CREATE INDEX METAdjustment1 ON METAdjustment
(
       zoneID ASC,
       monthID ASC,
       hourID ASC,
       modelYearID	ASC,
       polProcessID ASC
);

INSERT INTO METAdjustment (zoneID, monthID, hourID, polProcessID, fuelTypeID, modelYearID, minModelYearID, maxModelYearID, 
							temperatureAdjustment)
SELECT zoneID, monthID, hourID, ta.polProcessID, fuelTypeID, my.modelYearID, ta.minModelYearID, ta.maxModelYearID, 
1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB)
FROM ZoneMonthHour zmh
INNER JOIN TemperatureAdjustment ta
INNER JOIN PollutantProcessAssoc ppa ON (ppa.polProcessID = ta.polProcessID)
JOIN modelyear my
WHERE zmh.zoneID = ##context.iterLocation.zoneRecordID##
AND ppa.processID = 1
AND my.modelyearid between ta.minModelYearID and ta.maxModelYearID;

-- 
-- CREC 4-a: Calculate AC On Fraction
--
DROP TABLE IF EXISTS ACOnFraction;
CREATE TABLE ACOnFraction (
       zoneID	INTEGER NOT NULL,
       monthID	SMALLINT NOT NULL,
       hourID	SMALLINT NOT NULL, 
       ACOnFraction FLOAT
); 

CREATE UNIQUE INDEX XPKACOnFraction ON ACOnFraction
(
       zoneID ASC,
       monthID ASC,
       hourID ASC
);

-- 	LEAST(GREATEST(ACActivityTermA+heatIndex*(ACActivityTermB+ACActivityTermC*heatIndex),0),1.0,0.0)

INSERT INTO ACOnFraction
SELECT 
	zoneID, zmh.monthID, zmh.hourID,
	LEAST(GREATEST(ACActivityTermA+heatIndex*(ACActivityTermB+ACActivityTermC*heatIndex),0),1.0) as ACOnFraction
FROM ZoneMonthHour zmh
INNER JOIN MonthOfAnyYear may ON (may.monthID = zmh.monthID)
INNER JOIN MonthGroupHour mgh ON (mgh.monthGroupID = may.monthGroupID
AND mgh.hourID = zmh.hourID);

-- 
-- CREC 4-b: Calculate AC Activity Fraction
--
DROP TABLE IF EXISTS ACActivityFraction;
CREATE TABLE ACActivityFraction (
       zoneID  INTEGER NOT NULL,
       monthID SMALLINT NOT NULL,
       hourID SMALLINT NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       modelYearID SMALLINT NOT NULL,
       ACActivityFraction FLOAT
);

CREATE INDEX ACActivityFraction1 ON ACActivityFraction (
       hourID ASC
);
CREATE INDEX ACActivityFraction2 ON ACActivityFraction (
       sourceTypeID ASC
);


INSERT INTO ACActivityFraction 
SELECT zoneID, monthID, hourID, sta.sourceTypeID, modelYearID, 
ACOnFraction*ACPenetrationFraction*functioningACFraction
FROM ACOnFraction acof
INNER JOIN SourceTypeModelYear stmy
INNER JOIN SourceTypeAge sta ON (
sta.sourceTypeID = stmy.sourceTypeID AND
sta.ageID = ##context.year## - stmy.modelYearID);

-- 
-- CREC 4-c: Weight FullACAdjustment Factors by Operating Mode
--
DROP TABLE IF EXISTS WeightedFullACAdjustment;
CREATE TABLE WeightedFullACAdjustment (
       sourceTypeID SMALLINT NOT NULL,
       polProcessID int NOT NULL,
       linkID INTEGER NOT NULL,
       hourDayID SMALLINT NOT NULL,
       opModeID SMALLINT NOT NULL,
       weightedFullACAdjustment FLOAT
);

CREATE INDEX WeightedFullACAdjustment1 ON WeightedFullACAdjustment (
       hourDayID
);
CREATE INDEX WeightedFullACAdjustment2 ON WeightedFullACAdjustment (
       sourceTypeID
);

/*
INSERT INTO WeightedFullACAdjustment
SELECT omd.sourceTypeID, omd.polProcessID, linkID, hourDayID, 
	SUM(fullACAdjustment*opModeFraction)
FROM OpModeDistribution omd
INNER JOIN FullACAdjustment faca ON (faca.sourceTypeID=omd.sourceTypeID
AND faca.polProcessID=omd.polProcessID AND faca.opModeID=omd.opModeID
AND faca.opModeID < 1000)
GROUP BY omd.sourceTypeID, omd.polProcessID, linkID, hourDayID 
ORDER BY NULL
*/

INSERT INTO WeightedFullACAdjustment
SELECT omd.sourceTypeID, omd.polProcessID, linkID, hourDayID, omd.opModeID,
	fullACAdjustment
FROM OpModeDistribution omd
INNER JOIN FullACAdjustment faca ON (faca.sourceTypeID=omd.sourceTypeID
AND faca.polProcessID=omd.polProcessID AND faca.opModeID=omd.opModeID);

-- 
-- CREC 4-d: Calculate AC Adjustment Factor
--
DROP TABLE IF EXISTS ACAdjustment;
CREATE TABLE ACAdjustment (
       zoneID  INTEGER NOT NULL,
       monthID  SMALLINT NOT NULL,
       hourID  SMALLINT NOT NULL,
       dayID  SMALLINT NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       modelYearID  SMALLINT NOT NULL,
       polProcessID int NOT NULL,
       opModeID SMALLINT NOT NULL,
       ACAdjustment FLOAT
);

CREATE  INDEX ACAdjustment1 ON ACAdjustment (
       zoneID ASC,
       monthID ASC,
       hourID  ASC,
       polProcessID ASC,
       opModeID ASC
);

-- Following statement formerly ordered results by modelYearID descending
--  this seems unnecessary and was removed
INSERT INTO ACAdjustment
SELECT acaf.zoneID, monthID, hd.hourID, hd.dayID, 
acaf.sourceTypeID, modelYearID, polProcessID, opModeID,
	1+((weightedFullACAdjustment-1)*ACActivityFraction)
FROM ACActivityFraction acaf
INNER JOIN Link l ON (acaf.zoneID=l.zoneID)
INNER JOIN HourDay hd ON (hd.hourID=acaf.hourID)
INNER JOIN WeightedFullACAdjustment wfaca ON (
wfaca.sourceTypeID = acaf.sourceTypeID AND
wfaca.linkID = l.linkID AND
wfaca.hourDayID = hd.hourDayID);

-- 
-- CREC-5: Weight emission rates by source bin
--
DROP TABLE IF EXISTS SBWeightedEmissionRate;
CREATE TABLE SBWeightedEmissionRate (
       zoneID INTEGER NOT NULL,
       yearID SMALLINT NOT NULL,
       polProcessID int NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       modelYearID SMALLINT NOT NULL,
       fuelTypeID SMALLINT NOT NULL,
       opModeID SMALLINT NOT NULL,
       meanBaseRate FLOAT,
       meanBaseRateIM FLOAT
);

CREATE INDEX XPKSBWeightedEmissionRate ON SBWeightedEmissionRate (
       polProcessID ASC,
       sourceTypeID ASC,
       opModeID ASC
);

INSERT INTO SBWeightedEmissionRate
SELECT ##context.iterLocation.zoneRecordID## as zoneID, ##context.year## as yearID, 
	erim.polProcessID, sourceTypeID, (##context.year##-age.ageID) as modelYearID, sb.fuelTypeID, erim.opModeID,
	SUM(sourceBinActivityFraction*meanBaseRate), SUM(sourceBinActivityFraction*meanBaseRateIM)
FROM EmissionRateByAge erim
INNER JOIN AgeCategory age ON (age.ageGroupID=erim.ageGroupID)
INNER JOIN SourceTypeModelYear stmy ON (stmy.modelYearID=##context.year##-age.ageID)
INNER JOIN SourceBinDistribution sbd ON (sbd.sourceTypeModelYearID=stmy.sourceTypeModelYearID
AND sbd.polProcessID=erim.polProcessID AND sbd.sourceBinID=erim.sourceBinID)
INNER JOIN SourceBin sb ON (sbd.sourceBinID=sb.sourceBinID) 
GROUP BY erim.polProcessID, sourceTypeID, age.ageID, sb.fuelTypeID, erim.opModeID
ORDER BY NULL;

-- 
-- CREC-6: Weight emission rates by operating mode
--
DROP TABLE IF EXISTS FullyWeightedEmissionRate;
CREATE TABLE FullyWeightedEmissionRate (
       linkID INTEGER NOT NULL,
       yearID SMALLINT NOT NULL,
       polProcessID int NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       modelYearID SMALLINT NOT NULL,
       fuelTypeID SMALLINT NOT NULL,
       hourDayID SMALLINT NOT NULL,
       opModeID SMALLINT NOT NULL,
       meanBaseRate FLOAT,
       meanBaseRateIM FLOAT,
       opModeFraction FLOAT
);

CREATE UNIQUE INDEX XPKFullyWeightedEmissionRate ON FullyWeightedEmissionRate (
       linkID ASC,
       yearID ASC,
       polProcessID ASC,
       sourceTypeID ASC,
       modelYearID ASC,
       fuelTypeID ASC,
       hourDayID ASC,
       opModeID ASC
);

CREATE INDEX OPModeDistributionSpecial ON OpModeDistribution (
	polProcessID ASC,
	sourceTypeID ASC,
	opModeID ASC
);

INSERT INTO FullyWeightedEmissionRate
SELECT linkID, yearID, sbwer.polProcessID, sbwer.sourceTypeID, modelYearID, fuelTypeID, hourDayID, opModeID,
	meanBaseRate, meanBaseRateIM, opModeFraction
FROM SBWeightedEmissionRate sbwer 
INNER JOIN OpModeDistribution omd 
  USING(polProcessID, sourceTypeID, opModeID);

-- 
-- CREC-7-a: Combine Temperature and AC Adjustment Factors
--
DROP TABLE IF EXISTS TempAndACAdjustment;
CREATE TABLE TempAndACAdjustment (
       zoneID INTEGER NOT NULL,
       polProcessID int NOT NULL, 
       sourceTypeID SMALLINT NOT NULL, 
       modelYearID SMALLINT NOT NULL,
       fuelTypeID SMALLINT NOT NULL,
       monthID SMALLINT NOT NULL,
       hourID SMALLINT NOT NULL,
       dayID SMALLINT NOT NULL,
       opModeID SMALLINT NOT NULL,
       tempAndACAdjustment FLOAT
);

CREATE UNIQUE INDEX XPKTempAndACAdjustment ON TempAndACAdjustment (
       zoneID ASC,
       polProcessID ASC, 
       sourceTypeID ASC, 
       modelYearID ASC,
       fuelTypeID ASC,
       monthID ASC,
       hourID ASC,
       dayID ASC,
       opModeID ASC,
       tempAndACAdjustment
);

INSERT INTO TempAndACAdjustment
SELECT ma.zoneID, ma.polProcessID, sourceTypeID, aca.modelYearID, 
	fuelTypeID, ma.monthID, ma.hourID, aca.dayID, aca.opModeID,
	temperatureAdjustment*ACAdjustment 
FROM METAdjustment ma
INNER JOIN ACAdjustment aca ON (
aca.zoneID=ma.zoneID AND
aca.monthID=ma.monthID AND
aca.hourID=ma.hourID AND
aca.polProcessID=ma.polProcessID AND 
aca.modelYearID=ma.modelYearID);

-- 
-- CREC 7-b: Apply fuel adjustment to fully weighted emission rates
--
DROP TABLE IF EXISTS FuelAdjustedRate;
CREATE TABLE FuelAdjustedRate (
	linkID INTEGER NOT NULL,
	yearID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	monthID SMALLINT NOT NULL,
	hourDayID SMALLINT NOT NULL,
	opModeID SMALLINT NOT NULL,
	fuelAdjustedRate FLOAT,
	fuelAdjustedRateIM FLOAT,
	opModeFraction FLOAT
);

CREATE UNIQUE INDEX XPKFuelAdjustedRate ON FuelAdjustedRate (
	linkID ASC,
	yearID ASC,
	polProcessID ASC,
	sourceTypeID ASC,
	modelYearID ASC,
	fuelTypeID ASC,
	monthID ASC,
	hourDayID ASC,
	opModeID ASC
);

CREATE INDEX XPKFullyWeightedEmissionRate2 ON FullyWeightedEmissionRate (
	sourceTypeID ASC,
	yearID ASC,
	polProcessID ASC,
	modelYearID ASC,
	fuelTypeID ASC,
	opModeID ASC
);

CREATE INDEX XPKFuelSupplyAdjustment2 ON FuelSupplyAdjustment (
	sourceTypeID ASC,
	yearID ASC,
	polProcessID ASC,
	modelYearID ASC,
	fuelTypeID ASC
);

ANALYZE TABLE FullyWeightedEmissionRate;
ANALYZE TABLE FuelSupplyAdjustment;

INSERT INTO FuelAdjustedRate
SELECT linkID, fwer.yearID, fwer.polProcessID, fwer.sourceTypeID, fwer.modelYearID,
	fwer.fuelTypeID, m.monthID, fwer.hourDayID, fwer.opModeID,
	meanBaseRate * ifnull(fuelAdjustment,1.0), 
	meanBaseRateIM * ifnull(fuelAdjustment,1.0),
	opModeFraction
FROM
MonthOfAnyYear m
INNER JOIN FullyWeightedEmissionRate fwer
LEFT OUTER JOIN FuelSupplyAdjustment fsa ON (
	fsa.yearID = fwer.yearID AND
	fsa.polProcessID = fwer.polProcessID AND
	fsa.modelYearID = fwer.modelYearID AND
	fsa.sourceTypeID = fwer.sourceTypeID AND
	fsa.fuelTypeID = fwer.fuelTypeID AND
	fsa.monthID = m.monthID
);

-- 
-- CREC 7-c: Apply temperature and AC adjustment to fuel-adjusted emission rate.
--
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate;
CREATE TABLE WeightedAndAdjustedEmissionRate (
	linkID INTEGER NOT NULL,
	yearID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	hourID SMALLINT NOT NULL,
	dayID SMALLINT NOT NULL,
	monthID SMALLINT NOT NULL,
	meanBaseRate FLOAT,
	meanBaseRateIM FLOAT
);

CREATE UNIQUE INDEX XPKWeightedAndAdjustedEmissionRate ON WeightedAndAdjustedEmissionRate (
	linkID ASC,
	yearID ASC,
	polProcessID ASC,
	sourceTypeID ASC,
	modelYearID ASC,
	fuelTypeID ASC,
	hourID ASC,
	dayID ASC,
	monthID ASC
);

CREATE INDEX XPKFuelAdjustedRate2 ON FuelAdjustedRate (
    polProcessID ASC,
    sourceTypeID ASC,
    modelYearID ASC,
    fuelTypeID ASC,
    monthID ASC
);

ANALYZE TABLE FuelAdjustedRate;
ANALYZE TABLE Link;
ANALYZE TABLE HourDay;
ANALYZE TABLE TempAndACAdjustment;

INSERT INTO WeightedAndAdjustedEmissionRate
SELECT l.linkID, yearID, taca.polProcessID, taca.sourceTypeID, taca.modelYearID, 
	taca.fuelTypeID, taca.hourID, taca.dayID, taca.monthID,
	sum(fuelAdjustedRate*tempAndACAdjustment*opModeFraction),
	sum(fuelAdjustedRateIM*tempAndACAdjustment*opModeFraction)
FROM FuelAdjustedRate far 
INNER JOIN Link l ON (l.linkID=far.linkID)
INNER JOIN HourDay hd ON (hd.hourDayID=far.hourDayID)
INNER JOIN TempAndACAdjustment taca ON (
	taca.zoneID=l.zoneID AND
	taca.polProcessID=far.polProcessID AND
	taca.sourceTypeID=far.sourceTypeID AND
	taca.modelYearID=far.modelYearID AND
	taca.fuelTypeID=far.fuelTypeID AND
	taca.monthID=far.monthID AND
	taca.dayID=hd.dayID AND
	taca.hourID=hd.hourID AND
	taca.opModeID=far.opModeID)
group by l.linkID, yearID, taca.polProcessID, taca.sourceTypeID, taca.modelYearID, 
	taca.fuelTypeID, taca.hourID, taca.dayID, taca.monthID
order by null;

-- 
-- CREC 8: Calculate and Apply Humidity Correction Factor to NOx Emissions
--

DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate2_TEMP1;
CREATE TABLE WeightedAndAdjustedEmissionRate2_TEMP1 (
	linkID INTEGER NOT NULL,
	yearID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	monthID SMALLINT NOT NULL,
	dayID SMALLINT NOT NULL,
	hourID SMALLINT NOT NULL,
	meanBaseRate FLOAT,
	meanBaseRateIM FLOAT
);

CREATE UNIQUE INDEX XPKWeightedAndAdjustedEmissionRate2_TEMP1 ON WeightedAndAdjustedEmissionRate2_TEMP1 (
	linkID ASC,
	yearID ASC,
	polProcessID ASC,
	sourceTypeID ASC,
	fuelTypeID ASC,
	modelYearID ASC,
	monthID ASC,
	dayID ASC,
	hourID ASC
);

ANALYZE TABLE WeightedAndAdjustedEmissionRate;
ANALYZE TABLE Link;
ANALYZE TABLE ZoneMonthHour;
ANALYZE TABLE FuelType;

INSERT INTO WeightedAndAdjustedEmissionRate2_TEMP1
SELECT l.linkID, yearID, polProcessID, sourceTypeID, waer.fuelTypeID, modelYearID, waer.monthID, dayID, 
	waer.hourID,
	(1.0 - (GREATEST(21.0,LEAST(specificHumidity,124.0))-75.0)*humidityCorrectionCoeff)*meanBaseRate,
	(1.0 - (GREATEST(21.0,LEAST(specificHumidity,124.0))-75.0)*humidityCorrectionCoeff)*meanBaseRateIM
FROM WeightedAndAdjustedEmissionRate waer 
INNER JOIN Link l ON (l.linkID=waer.linkID)
INNER JOIN ZoneMonthHour zmh ON (
zmh.monthID=waer.monthID AND
zmh.zoneID=l.zoneID AND 
zmh.hourID=waer.hourID) AND
polprocessid = 301
INNER JOIN FuelType ft ON (ft.fuelTypeID=waer.fuelTypeID);

DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate2_TEMP2;
CREATE TABLE WeightedAndAdjustedEmissionRate2_TEMP2 (
	linkID INTEGER NOT NULL,
	yearID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	monthID SMALLINT NOT NULL,
	dayID SMALLINT NOT NULL,
	hourID SMALLINT NOT NULL,
	meanBaseRate FLOAT,
	meanBaseRateIM FLOAT
);

CREATE UNIQUE INDEX XPKWeightedAndAdjustedEmissionRate_TEMP2 ON WeightedAndAdjustedEmissionRate2_TEMP2 (
	linkID ASC,
	yearID ASC,
	polProcessID ASC,
	sourceTypeID ASC,
    fuelTypeID ASC,
	modelYearID ASC,
	monthID ASC,
	dayID ASC,
	hourID ASC
);

INSERT INTO WeightedAndAdjustedEmissionRate2_TEMP2
SELECT linkID, yearID, polProcessID, sourceTypeID, fuelTypeID, 
       modelYearID, monthID, dayID, hourID, meanBaseRate, meanBaseRateIM
FROM WeightedAndAdjustedEmissionRate
WHERE polProcessID != 301;

DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate2;
CREATE TABLE WeightedAndAdjustedEmissionRate2 (
	linkID INTEGER NOT NULL,
	yearID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	monthID SMALLINT NOT NULL,
	dayID SMALLINT NOT NULL,
	hourID SMALLINT NOT NULL,
	meanBaseRate FLOAT,
	meanBaseRateIM FLOAT
);

CREATE UNIQUE INDEX XPKWeightedAndAdjustedEmissionRate2 ON WeightedAndAdjustedEmissionRate2 (
	linkID ASC,
	yearID ASC,
	polProcessID ASC,
	sourceTypeID ASC,
    fuelTypeID ASC,
	modelYearID ASC,
	monthID ASC,
	dayID ASC,
	hourID ASC
);

INSERT INTO WeightedAndAdjustedEmissionRate2
(SELECT * FROM WeightedAndAdjustedEmissionRate2_TEMP1)
UNION
(SELECT * FROM WeightedAndAdjustedEmissionRate2_TEMP2);

-- 
-- CREC 9: Multiply fully weighted and adjusted emission rates by source hour operating (SHO)
--	     activity to generate inventory.
--
DROP TABLE IF EXISTS SHO2;
CREATE TABLE SHO2 (
       yearID               SMALLINT NOT NULL,
       monthID              SMALLINT NOT NULL,
       dayID                SMALLINT NOT NULL,
       hourID               SMALLINT NOT NULL,
       sourceTypeID         SMALLINT NOT NULL,
       modelYearID          SMALLINT NOT NULL,
       SCC                  CHAR(10),
       SHO                  FLOAT NULL
);
CREATE INDEX XPKSHO2 ON SHO2 (
	yearID asc,
	monthID asc,
	dayID asc,
	hourID asc,
	sourceTypeID asc,
	modelYearID asc
);
ANALYZE TABLE sho;

INSERT INTO SHO2 SELECT 
yearID, monthID, hd.dayID, hd.hourID, sourceTypeID,
yearID - ageID AS modelYearID, NULL AS SCC, SHO
FROM SHO sho
INNER JOIN HourDay hd ON (hd.hourDayID=sho.hourDayID);

DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate3;
CREATE TABLE WeightedAndAdjustedEmissionRate3 (
		linkID	INTEGER NOT NULL,
		yearID	SMALLINT NOT NULL,
		pollutantID SMALLINT NOT NULL,
		processID SMALLINT NOT NULL,
		sourceTypeID SMALLINT NOT NULL,
		fuelTypeID SMALLINT NOT NULL,
		modelYearID SMALLINT NOT NULL,
		monthID SMALLINT NOT NULL,
		dayID SMALLINT NOT NULL,
		hourID SMALLINT NOT NULL,
		meanBaseRate FLOAT,
		meanBaseRateIM FLOAT
);

CREATE UNIQUE INDEX XPKWeightedAndAdjustedEmissionRate31 ON WeightedAndAdjustedEmissionRate3 (
		linkID	ASC,
		yearID	ASC,
		pollutantID ASC,
		processID ASC,
		sourceTypeID ASC,
		fuelTypeID ASC,
		modelYearID ASC,
		monthID ASC,
		dayID ASC,
		hourID ASC
);

INSERT INTO WeightedAndAdjustedEmissionRate3
SELECT linkID, yearID, pollutantID, processID, sourceTypeID, fuelTypeID,
	modelYearID, monthID, dayID, hourID, meanBaseRate, meanBaseRateIM
FROM WeightedAndAdjustedEmissionRate2 waer
INNER JOIN PollutantProcessAssoc ppa ON (ppa.polProcessID=waer.polProcessID);

DROP TABLE IF EXISTS SHO3;
CREATE TABLE SHO3 (
	   linkID				INTEGER NOT NULL,
       yearID               SMALLINT NOT NULL,
       monthID              SMALLINT NOT NULL,
       dayID                SMALLINT NOT NULL,
       hourID               SMALLINT NOT NULL,
       sourceTypeID         SMALLINT NOT NULL,
       modelYearID          SMALLINT NOT NULL,
       pollutantID          SMALLINT NOT NULL,
       processID            SMALLINT NOT NULL,
       fuelTypeID           SMALLINT NOT NULL,
       SCC                  CHAR(10),
       emissionQuant        FLOAT NULL,
       emissionQuantIM		FLOAT NULL
);
CREATE INDEX XPKSHO3 ON SHO3 (
	linkID asc,
	yearID asc,
	monthID asc,
	dayID asc,
	hourID asc,
	sourceTypeID asc,
	modelYearID asc,
	pollutantID asc,
	processID asc,
	fuelTypeID asc
);

CREATE INDEX XPKSHO22 ON SHO2 (
	yearID asc,
	monthID asc,
	dayID asc,
	hourID asc,
	sourceTypeID asc,
	modelYearID asc
);

CREATE INDEX XPKWeightedAndAdjustedEmissionRate32 ON WeightedAndAdjustedEmissionRate3 (
	yearID asc,
	monthID asc,
	dayID asc,
	hourID asc,
	sourceTypeID asc,
	modelYearID asc
);

ANALYZE TABLE SHO2;

INSERT INTO SHO3
SELECT linkID, sho2.yearID, sho2.monthID, sho2.dayID, sho2.hourID, 
	sho2.sourceTypeID, sho2.modelYearID, pollutantID, processID, fuelTypeID, 
	scc, 
	sho * meanBaseRate AS emissionQuant,
	sho * meanBaseRateIM AS emissionQuantIM
FROM SHO2 sho2
INNER JOIN WeightedAndAdjustedEmissionRate3 waer ON (
	waer.yearID=sho2.yearID AND
	waer.monthID=sho2.monthID AND
	waer.dayID=sho2.dayID AND
	waer.hourID=sho2.hourID AND
	waer.sourceTypeID=sho2.sourceTypeID AND
	waer.modelYearID=sho2.modelYearID);

alter table MOVESWorkerOutput add emissionQuantIM float null;

ANALYZE TABLE sho3;
INSERT INTO MOVESWorkerOutput (
	stateID, countyID, zoneID, linkID, roadTypeID, yearID, monthID, dayID, hourID,
	pollutantID, processID, sourceTypeID, modelYearID, fuelTypeID, SCC, emissionQuant, emissionQuantIM)
SELECT ##context.iterLocation.stateRecordID##, ##context.iterLocation.countyRecordID##,
	l.zoneID, l.linkID, l.roadTypeID, yearID, monthID, dayID, hourID,
	pollutantID, processID, sourceTypeID, modelYearID, fuelTypeID, SCC, emissionQuant, emissionQuantIM
FROM SHO3 sho3
INNER JOIN Link l ON (l.linkID = sho3.linkID);

-- Apply IM
update MOVESWorkerOutput, IMCoverageMergedUngrouped set emissionQuant=GREATEST(emissionQuantIM*IMAdjustFract + emissionQuant*(1.0-IMAdjustFract),0.0)
where MOVESWorkerOutput.processID = IMCoverageMergedUngrouped.processID
	and MOVESWorkerOutput.pollutantID = IMCoverageMergedUngrouped.pollutantID
	and MOVESWorkerOutput.modelYearID = IMCoverageMergedUngrouped.modelYearID
	and MOVESWorkerOutput.fuelTypeID = IMCoverageMergedUngrouped.fuelTypeID
	and MOVESWorkerOutput.sourceTypeID = IMCoverageMergedUngrouped.sourceTypeID;

alter table MOVESWorkerOutput drop emissionQuantIM;

-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS ACActivityFraction;
DROP TABLE IF EXISTS ACAdjustment;
DROP TABLE IF EXISTS ACOnFraction;
DROP TABLE IF EXISTS CountyFuelAdjustment;
DROP TABLE IF EXISTS EmissionRateWithIM;
DROP TABLE IF EXISTS FuelAdjustedRate;
DROP TABLE IF EXISTS FuelSupplyAdjustment;
DROP TABLE IF EXISTS FuelSupplyWithFuelType;
DROP TABLE IF EXISTS FullyWeightedEmissionRate;
drop table if exists IMCoverageMergedUngrouped;
DROP TABLE IF EXISTS METAdjustment;
DROP TABLE IF EXISTS SBWeightedEmissionRate;
DROP TABLE IF EXISTS SHO2;
DROP TABLE IF EXISTS SHO3;
DROP TABLE IF EXISTS TempAndACAdjustment;
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate;
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate2;
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate2_TEMP1;
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate2_TEMP2;
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate3;
DROP TABLE IF EXISTS WeightedFullACAdjustment;
-- End Section Cleanup
