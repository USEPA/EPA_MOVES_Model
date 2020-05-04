-- Version 2013-09-15
-- Authors: Gwo S. and Wes F. 
-- The Ammonia calculator shall not have the ability to calculate fuel formulation effects, 
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

##create.PollutantProcessMappedModelYear##;
TRUNCATE PollutantProcessMappedModelYear;

##create.RunspecHour##;
TRUNCATE RunspecHour;

##create.RunspecMonth##;
TRUNCATE RunspecMonth;

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
-- AND EmissionRateByAge.polProcessID IN (??pollutantProcessIDs??);

cache SELECT EmissionRateByAge.* INTO OUTFILE '##EmissionRateByAge##'
FROM EmissionRateByAge
WHERE EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##)
AND EmissionRateByAge.sourceBinID in (##macro.csv.all.sourceBinID##);

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
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND FuelSupply.monthGroupID in (##macro.csv.all.monthGroupID##);

cache SELECT FuelType.* INTO OUTFILE '##FuelType##'
FROM FuelType
WHERE fuelTypeID in (##macro.csv.all.fuelTypeID##);

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

cache SELECT Link.* INTO OUTFILE '##Link##'
FROM Link WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT MonthGroupHour.* INTO OUTFILE '##MonthGroupHour##'
FROM MonthGroupHour
WHERE hourID in (##macro.csv.all.hourID##);

cache SELECT MonthOfAnyYear.* INTO OUTFILE '##MonthOfAnyYear##'
FROM MonthOfAnyYear
WHERE MonthOfAnyYear.monthID in (##macro.csv.all.monthID##);

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

SELECT * INTO OUTFILE '##SHO##'
FROM SHO
WHERE linkID = ##context.iterLocation.linkRecordID##
AND yearID = ##context.year##;

cache SELECT DISTINCT SourceBin.* INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30;

cache SELECT SourceBinDistribution.* INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear
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

-- End Section Extract Data

-- Section Processing

-- 
-- NH3REC 1: Complete I/M adjustment fraction information
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

-- CREATE INDEX XPKIMCoverageMergedUngrouped ON IMCoverageMergedUngrouped
-- (
--        polProcessID ASC,
--        pollutantID ASC,
--        processID ASC,
--        modelYearID ASC,
--        fuelTypeID ASC,
--        sourceTypeID ASC
-- );

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
-- NH3REC-2: Weight emission rates by source bin
--

DROP TABLE IF EXISTS SourceBinEmissionRates0;
CREATE TABLE SourceBinEmissionRates0 (
	zoneID INT NOT NULL,
	linkID INT NOT NULL,
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
	zoneID, linkID, yearID, polProcessID, sourceTypeID, modelYearID, fuelTypeID, opModeID);

INSERT INTO SourceBinEmissionRates0 (
      zoneID, linkID, yearID, polProcessID,
      sourceTypeID, modelYearID, fuelTypeID, opModeID,
	  meanBaseRate, meanBaseRateIM)
SELECT 
      ##context.iterLocation.zoneRecordID## as zoneID,
      ##context.iterLocation.linkRecordID##,
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


DROP TABLE IF EXISTS SBWeightedEmissionRate;
CREATE TABLE SBWeightedEmissionRate (
       zoneID INTEGER NOT NULL,
       linkID INTEGER NOT NULL,
       monthID SMALLINT NOT NULL,
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
       zoneID ASC, 
       linkID ASC,
       monthID ASC, 
       yearID ASC, 
       polProcessID ASC, 
       sourceTypeID ASC, 
       modelYearID ASC, 
       fuelTypeID ASC, 
       opModeID ASC
);

INSERT INTO SBWeightedEmissionRate(
       zoneID,
       linkID,
       monthID,
       yearID,
       polProcessID,
       sourceTypeID,
       modelYearID,
       fuelTypeID,
       opModeID,
       meanBaseRate,
       meanBaseRateIM
)
SELECT 
      er.zoneID, 
      er.linkID,
      rm.monthID,
      er.yearID, 
	    er.polProcessID, 
	    er.sourceTypeID, 
	    er.modelYearID, 
	    er.fuelTypeID, 
	    er.opModeID,
	    er.meanBaseRate, 
	    er.meanBaseRateIM
FROM SourceBinEmissionRates0 er, RunspecMonth rm
ORDER BY NULL;

-- 
-- NH3REC-3: Weight emission rates by operating mode
--
DROP TABLE IF EXISTS FullyWeightedEmissionRate;
CREATE TABLE FullyWeightedEmissionRate (
       zoneID INTEGER NOT NULL,
       linkID INTEGER NOT NULL,
       yearID SMALLINT NOT NULL,
       monthID SMALLINT NOT NULL,
       dayID SMALLINT NOT NULL,
       hourID SMALLINT NOT NULL,
       polProcessID int NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       modelYearID SMALLINT NOT NULL,
       fuelTypeID SMALLINT NOT NULL,
       hourDayID SMALLINT NOT NULL,
       meanBaseRate FLOAT,
       meanBaseRateIM FLOAT
);

CREATE UNIQUE INDEX XPKFullyWeightedEmissionRate ON FullyWeightedEmissionRate (
       zoneID ASC,
       linkID ASC,
       yearID ASC,
       monthID ASC,
       polProcessID ASC,
       sourceTypeID ASC,
       modelYearID ASC,
       fuelTypeID ASC,
       hourDayID ASC
);

CREATE INDEX OPModeDistributionSpecial ON OpModeDistribution (
	polProcessID ASC,
	sourceTypeID ASC,
	linkID ASC,
	opModeID ASC
);

INSERT INTO FullyWeightedEmissionRate(
       zoneID,
       linkID,
       yearID,
       monthID,
       dayID,
       hourID,
       polProcessID,
       sourceTypeID,
       modelYearID,
       fuelTypeID,
       hourDayID,
       meanBaseRate,
       meanBaseRateIM
)
SELECT sbwer.zoneID, sbwer.linkID, sbwer.yearID, sbwer.monthID, 
       0, 0, 
       sbwer.polProcessID, sbwer.sourceTypeID, sbwer.modelYearID, sbwer.fuelTypeID, 
       omd.hourDayID,
	SUM(opModeFraction*meanBaseRate),
	SUM(opModeFraction*meanBaseRateIM)
FROM SBWeightedEmissionRate sbwer 
INNER JOIN OpModeDistribution omd 
  USING(polProcessID, sourceTypeID, linkID, opModeID) 
GROUP BY zoneID, linkID, yearID, monthID, polProcessID, sourceTypeID, modelYearID, fuelTypeID, hourDayID;

UPDATE FullyWeightedEmissionRate, HourDay
SET FullyWeightedEmissionRate.dayID=HourDay.dayID,
    FullyWeightedEmissionRate.hourID=HourDay.hourID
WHERE FullyWeightedEmissionRate.hourDayID=HourDay.hourDayID;   

ANALYZE TABLE Link;
ANALYZE TABLE HourDay;

-- 
-- NH3REC 4: Multiply fully weighted and adjusted emission rates by source hour operating (SHO)
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
		zoneID INTEGER NOT NULL,
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
		zoneID ASC,
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

INSERT INTO WeightedAndAdjustedEmissionRate3(
		zoneID,
		linkID,
		yearID,
		pollutantID,
		processID,
		sourceTypeID,
		fuelTypeID,
		modelYearID,
		monthID,
		dayID,
		hourID,
		meanBaseRate,
		meanBaseRateIM
)
SELECT fwer.zoneID, fwer.linkID, fwer.yearID, ppa.pollutantID, ppa.processID, 
       fwer.sourceTypeID, fwer.fuelTypeID,
       fwer.modelYearID, fwer.monthID, hd.dayID, hd.hourID, 
       fwer.meanBaseRate, fwer.meanBaseRateIM
FROM FullyWeightedEmissionRate fwer
INNER JOIN HourDay hd ON (hd.hourDayID = fwer.hourDayID)
INNER JOIN PollutantProcessAssoc ppa ON (ppa.polProcessID=fwer.polProcessID);


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
       emissionQuantIM      FLOAT NULL
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
scc, sho * meanBaseRate AS emissionQuant, sho * meanBaseRateIM AS emissionQuantIM
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
FROM SHO3 sho3 INNER JOIN Link l ON (l.linkID = sho3.linkID);

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
DROP TABLE IF EXISTS CountyFuelAdjustment;
DROP TABLE IF EXISTS FuelAdjustedRate;
DROP TABLE IF EXISTS FuelSupplyAdjustment;
DROP TABLE IF EXISTS FuelSupplyWithFuelType;
DROP TABLE IF EXISTS FullyWeightedEmissionRate;
drop table if exists IMCoverageMergedUngrouped;
DROP TABLE IF EXISTS IMAdjustment;
DROP TABLE IF EXISTS IMAdjustmentWithSourceBin;
DROP TABLE IF EXISTS SourceBinEmissionRates0;
DROP TABLE IF EXISTS SBWeightedEmissionRate;
DROP TABLE IF EXISTS SHO2;
DROP TABLE IF EXISTS SHO3;
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate;
DROP TABLE IF EXISTS WeightedAndAdjustedEmissionRate3;
FLUSH TABLES;
-- End Section Cleanup
