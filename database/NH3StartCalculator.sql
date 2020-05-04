-- Version 2013-09-15
-- Authors: Gwo S. and Wes F.
-- The Ammonia (NH3) calculator shall not have the ability to calculate fuel formulation effects, 
-- temperature effects, AC on effects or humidity effects.

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

##create.HourDay##;
TRUNCATE HourDay;

##create.IMCoverage##;
TRUNCATE IMCoverage;

##create.IMFactor##;
TRUNCATE IMFactor;

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

##create.RunspecHour##;
TRUNCATE RunspecHour;

##create.RunspecMonth##;
TRUNCATE RunspecMonth;

##create.SourceBin##;
TRUNCATE SourceBin;

##create.SourceBinDistribution##;
TRUNCATE SourceBinDistribution;

##create.SourceTypeModelYear##;
TRUNCATE SourceTypeModelYear;

##create.StartTempAdjustment##;
TRUNCATE TABLE StartTempAdjustment;

##create.Starts##;
TRUNCATE TABLE Starts;

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

cache SELECT ff.* INTO OUTFILE '##FuelFormulation##'
FROM FuelFormulation ff
INNER JOIN FuelSupply fs ON fs.fuelFormulationID = ff.fuelFormulationID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID
INNER JOIN RunSpecMonthGroup rsmg ON rsmg.monthGroupID = fs.monthGroupID
WHERE fuelRegionID = ##context.fuelRegionID## AND
yearID = ##context.year##
GROUP BY ff.FuelFormulationID ORDER BY NULL;

cache SELECT * INTO OUTFILE '##FuelSubtype##'
FROM FuelSubtype;

cache SELECT FuelSupply.* INTO OUTFILE '##FuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##;

cache SELECT DISTINCT FuelType.* INTO OUTFILE '##FuelType##'
FROM FuelType
INNER JOIN RunSpecSourceFuelType ON (RunSpecSourceFuelType.fuelTypeID = FuelType.fuelTypeID);

cache SELECT DISTINCT HourDay.* INTO OUTFILE '##HourDay##'
FROM HourDay,RunSpecHour,RunSpecDay
WHERE HourDay.dayID = RunSpecDay.dayID
AND HourDay.hourID = RunSpecHour.hourID;

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

cache SELECT MonthOfAnyYear.* INTO OUTFILE '##MonthOfAnyYear##'
FROM MonthOfAnyYear,RunSpecMonth
WHERE MonthOfAnyYear.monthID = RunSpecMonth.monthID;

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

cache SELECT * INTO OUTFILE '##RunspecHour##' FROM RunspecHour;

cache SELECT * INTO OUTFILE '##RunspecMonth##' FROM RunspecMonth;

cache SELECT DISTINCT SourceBin.* INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

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

cache SELECT SourceTypeModelYear.* INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear,RunSpecSourceType
WHERE SourceTypeModelYear.sourceTypeID = RunSpecSourceType.sourceTypeID
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT Starts.* INTO OUTFILE '##Starts##'
FROM Starts
WHERE yearID = ##context.year##
AND zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT StartTempAdjustment.* INTO OUTFILE '##StartTempAdjustment##'
FROM StartTempAdjustment
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT Year.* INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT DISTINCT ZoneMonthHour.* INTO OUTFILE '##ZoneMonthHour##'
FROM ZoneMonthHour,RunSpecMonth,RunSpecHour
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND RunSpecMonth.monthID = ZoneMonthHour.monthID
AND RunSpecHour.hourID = ZoneMonthHour.hourID;

-- End Section Extract Data
--
-- Section Processing

--
-- NH3SEC 1: Complete I/M adjustment fraction information
--
DROP TABLE IF EXISTS IMCoverageMergedUngrouped;
CREATE TABLE IMCoverageMergedUngrouped (
       polProcessID int NOT NULL,
       pollutantID  SMALLINT NOT NULL,
       processID  SMALLINT NOT NULL,
       modelYearID SMALLINT NOT NULL,
       fuelTypeID SMALLINT NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       IMAdjustFract FLOAT,
       weightFactor FLOAT
);


INSERT INTO IMCoverageMergedUngrouped (
 polProcessID, pollutantID, processID, modelYearID,
 fuelTypeID,sourceTypeID,IMAdjustFract,weightFactor)
SELECT
 ppmy.polProcessID, 
 0, 
 0, 
 ppmy.modelYearID,
 imf.fuelTypeID,
 imc.sourceTypeID,
 sum(IMFactor*complianceFactor*.01) AS IMAdjustFract,
 sum(complianceFactor) as weightFactor
FROM 
PollutantProcessMappedModelYear ppmy
INNER JOIN IMFactor imf ON (
	imf.polProcessID = ppmy.polProcessID
	AND imf.IMModelYearGroupID = ppmy.IMModelYearGroupID)
INNER JOIN AgeCategory ac ON (
	ac.ageGroupID = imf.ageGroupID)
INNER JOIN IMCoverage imc ON (
	imc.polProcessID = imf.polProcessID
	AND imc.inspectFreq = imf.inspectFreq
	AND imc.testStandardsID = imf.testStandardsID
	AND imc.sourceTypeID = imf.sourceTypeID
	AND imc.fuelTypeID = imf.fuelTypeID
	AND imc.begModelYearID <= ##context.year##-ageID
	AND imc.endModelYearID >= ##context.year##-ageID)
WHERE imc.countyID = ##context.iterLocation.countyRecordID##
AND imc.yearID = ##context.year##
AND ppmy.modelYearID = ##context.year##-ageID
AND ppmy.polProcessID IN (##pollutantProcessIDs##)
GROUP BY ppmy.polProcessID, 
 ppmy.modelYearID,
 imf.fuelTypeID,
 imc.sourceTypeID;

UPDATE IMCoverageMergedUngrouped, pollutantprocessassoc
SET IMCoverageMergedUngrouped.pollutantID=pollutantprocessassoc.pollutantID, 
    IMCoverageMergedUngrouped.processID=pollutantprocessassoc.processID 
WHERE IMCoverageMergedUngrouped.polProcessID=pollutantprocessassoc.polProcessID
; 


CREATE INDEX XPKIMCoverageMergedUngrouped ON IMCoverageMergedUngrouped
(
       polProcessID ASC,
       pollutantID ASC,
       processID ASC,
       modelYearID ASC,
       fuelTypeID ASC,
       sourceTypeID ASC
);

--
-- NH3SEC-2: Weight Emission Rates by Source Bin.
--

DROP TABLE IF EXISTS SourceBinEmissionRates0;
CREATE TABLE SourceBinEmissionRates0 (
	zoneID INT NOT NULL,
	yearID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	opModeID SMALLINT NOT NULL,
	meanBaseRate FLOAT,
	meanBaseRateIM FLOAT
);

ALTER TABLE SourceBinEmissionRates0 ADD INDEX SourceBinEmissionRates0 (
	zoneID, yearID, polProcessID, sourceTypeID, modelYearID, fuelTypeID, opModeID);

INSERT INTO SourceBinEmissionRates0 (
      zoneID, yearID, polProcessID,
      sourceTypeID, modelYearID, fuelTypeID, opModeID,
	  meanBaseRate, meanBaseRateIM)
SELECT 
      ##context.iterLocation.zoneRecordID## as zoneID,
      ##context.year## as yearID,
      er.polProcessID, 
      stmy.sourceTypeID, stmy.modelYearID, sb.fuelTypeID, er.opModeID,
	  sum(meanBaseRate*sourceBinActivityFraction) AS meanBaseRate,
	  sum(meanBaseRateIM*sourceBinActivityFraction) AS meanBaseRateIM
FROM EmissionRateByAge er
INNER JOIN AgeCategory age ON (age.ageGroupID=er.ageGroupID)
INNER JOIN SourceTypeModelYear stmy ON (stmy.modelYearID=##context.year##-age.ageID)
INNER JOIN SourceBinDistribution sbd ON (sbd.sourceTypeModelYearID=stmy.sourceTypeModelYearID
	AND sbd.polProcessID=er.polProcessID AND sbd.sourceBinID=er.sourceBinID)
INNER JOIN SourceBin sb ON (sbd.sourceBinID=sb.sourceBinID) 
GROUP BY er.polProcessID, stmy.sourceTypeID, stmy.modelYearID, sb.fuelTypeID, er.opModeID
ORDER BY NULL;


DROP TABLE IF EXISTS SourceBinEmissionRates;
CREATE TABLE SourceBinEmissionRates (
	zoneID INT NOT NULL,
	monthID SMALLINT NOT NULL,
	hourID SMALLINT NOT NULL,
	yearID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	opModeID SMALLINT NOT NULL,
	meanBaseRate FLOAT,
	meanBaseRateIM FLOAT
);

ALTER TABLE SourceBinEmissionRates ADD INDEX SourceBinEmissionRates1 (
	zoneID, monthID, hourID, yearID, polProcessID, sourceTypeID, modelYearID, fuelTypeID, opModeID);

INSERT INTO SourceBinEmissionRates (
      zoneID, monthID, hourID, yearID, polProcessID,
      sourceTypeID, modelYearID, fuelTypeID, opModeID,
	  meanBaseRate, meanBaseRateIM)
SELECT 
      er.zoneID,
      rm.monthID,
      rh.hourID,
      er.yearID,
      er.polProcessID, 
      er.sourceTypeID, 
      er.modelYearID, 
      er.fuelTypeID, 
      er.opModeID,
	    er.meanBaseRate,
	    er.meanBaseRateIM
FROM SourceBinEmissionRates0 er, RunspecMonth rm, RunspecHour rh
ORDER BY NULL;


--
-- NH3SEC-3: Weight adjusted emission rates by operating mode.
--
DROP TABLE IF EXISTS ActivityWeightedEmissionRate;
CREATE TABLE ActivityWeightedEmissionRate (
	zoneID INT NOT NULL,
	yearID SMALLINT NOT NULL,
	monthID SMALLINT NOT NULL,
	dayID SMALLINT NOT NULL,
	hourID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	meanBaseRate FLOAT,
	meanBaseRateIM FLOAT
);

CREATE UNIQUE INDEX XPKActivityWeightedEmissionRate ON ActivityWeightedEmissionRate (
	zoneID ASC,
	yearID ASC,
	monthID ASC,
	dayID ASC,
	hourID ASC,
	polProcessID ASC,
	sourceTypeID ASC,
	modelYearID ASC,
	fuelTypeID ASC
);

INSERT INTO ActivityWeightedEmissionRate (
      zoneID, yearID, monthID, dayID, hourID, polProcessID,
      sourceTypeID, modelYearID, fuelTypeID, meanBaseRate, meanBaseRateIM )
SELECT 
      zoneID, ##context.year##, monthID, hd.dayID, msber.hourID, msber.polProcessID,
      msber.sourceTypeID, modelYearID, fuelTypeID, 
      SUM(meanBaseRate * opModeFraction), SUM(meanBaseRateIM * opModeFraction)
FROM SourceBinEmissionRates msber
INNER JOIN HourDay hd ON (hd.hourID = msber.hourID)
INNER JOIN OpModeDistribution omd ON (
omd.sourceTypeID = msber.sourceTypeID
AND omd.hourDayID = hd.hourDayID 
AND omd.polProcessID = msber.polProcessID 
AND omd.opModeID = msber.opModeID)
GROUP BY zoneID, yearID, monthID, hd.dayID, msber.hourID, msber.polProcessID, 
msber.sourceTypeID, modelYearID, fuelTypeID
ORDER BY NULL;

-- 
-- NH3SEC-4: Multiply emission rates by start activity to generate inventory.
--
-- Make version of Starts table that is optimized to subsequent steps

DROP TABLE IF EXISTS Starts2;
CREATE TABLE Starts2 (
	zoneID INT NOT NULL,
	monthID SMALLINT NOT NULL,
	hourID SMALLINT NOT NULL,
	dayID SMALLINT NOT NULL,
	yearID SMALLINT NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	starts FLOAT
);

CREATE UNIQUE INDEX XPKStarts2 ON Starts2 (
      zoneID ASC,
      monthID ASC,
      hourID ASC,
	  dayID ASC,
      yearID ASC,
      sourceTypeID ASC,
      modelYearID ASC
);

INSERT INTO Starts2 (zoneID, monthID, hourID, dayID, yearID, 
	sourceTypeID, modelYearID, starts) 
SELECT zoneID, monthID, hourID, dayID, yearID, sourceTypeID, 
	(##context.year## - ageID) AS modelYearID, starts
FROM Starts INNER JOIN HourDay ON (Starts.hourDayID= HourDay.hourDayID);

alter table MOVESWorkerOutput add emissionQuantIM float null default 0.0;

INSERT INTO MOVESWorkerOutput (
	stateID, countyID, zoneID, linkID, roadTypeID, yearID, monthID, dayID,
      hourID, pollutantID, processID, sourceTypeID, modelYearID, fuelTypeID,
	SCC, emissionQuant, emissionQuantIM)
SELECT
  ##context.iterLocation.stateRecordID##, 
  ##context.iterLocation.countyRecordID##,
  s.zoneID, 
  ##context.iterLocation.linkRecordID##, 
  1 AS roadTypeID,
  s.yearID, s.monthID, s.dayID, s.hourID, pollutantID, processID, 
  s.sourceTypeID, s.modelYearID, fuelTypeID, NULL AS SCC, 
  (meanBaseRate * starts) AS emissionQuant,
  (meanBaseRateIM * starts) AS emissionQuantIM
FROM Starts2 s, ActivityWeightedEmissionRate awer, PollutantProcessAssoc ppa
WHERE
     s.zoneID=awer.zoneID AND
     s.monthID=awer.monthID AND
     s.hourID=awer.hourID AND
     s.dayID=awer.dayID AND
     s.yearID=awer.yearID AND
     s.sourceTypeID=awer.sourceTypeID AND
     s.modelYearID=awer.modelYearID AND
     awer.polProcessID=ppa.polProcessID;

-- Apply IM
update MOVESWorkerOutput, IMCoverageMergedUngrouped 
set emissionQuant=GREATEST(emissionQuantIM*IMAdjustFract + emissionQuant*(1.0-IMAdjustFract),0.0)
where MOVESWorkerOutput.processID = IMCoverageMergedUngrouped.processID
	and MOVESWorkerOutput.pollutantID = IMCoverageMergedUngrouped.pollutantID
	and MOVESWorkerOutput.modelYearID = IMCoverageMergedUngrouped.modelYearID
	and MOVESWorkerOutput.fuelTypeID = IMCoverageMergedUngrouped.fuelTypeID
	and MOVESWorkerOutput.sourceTypeID = IMCoverageMergedUngrouped.sourceTypeID;

alter table MOVESWorkerOutput drop emissionQuantIM;

-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS ActivityWeightedEmissionRate;
DROP TABLE IF EXISTS CountyFuelAdjustment;
DROP TABLE IF EXISTS CountyFuelAdjustmentWithFuelType;
DROP TABLE IF EXISTS EmissionRatesWithIM;
DROP TABLE IF EXISTS FuelSupplyAdjustment;
drop table if exists IMCoverageMergedUngrouped;
drop table if exists IMCoverageMerged;
DROP TABLE IF EXISTS IMAdjustment;
DROP TABLE IF EXISTS IMAdjustmentWithSourceBin;
DROP TABLE IF EXISTS SourceBinEmissionRates0;
DROP TABLE IF EXISTS SourceBinEmissionRates;
DROP TABLE IF EXISTS Starts2;
FLUSH TABLES;
-- End Section Cleanup

