-- Version 2013-09-15
-- Author Ed Glover
-- Author Wesley Faler
-- Author Ed Campbell
-- Add deterioration to HC,CO and NOx for starts - Gwo Shyu, EPA, 11/12/2008
-- Modified to add exponential start temperature equation - Ed Glover & David Hawkins  3/26/2013

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
INNER JOIN MonthOfAnyYear ON MonthOfAnyYear.monthGroupID = rsmg.monthGroupID
WHERE fuelRegionID = ##context.fuelRegionID## AND
yearID = ##context.year##
AND MonthOfAnyYear.monthID = ##context.monthID##
GROUP BY ff.FuelFormulationID ORDER BY NULL;

cache SELECT * INTO OUTFILE '##FuelSubtype##'
FROM FuelSubtype;

cache SELECT FuelSupply.* INTO OUTFILE '##FuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND MonthOfAnyYear.monthID = ##context.monthID##;

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
FROM MonthOfAnyYear
WHERE MonthOfAnyYear.monthID = ##context.monthID##;

SELECT OpModeDistribution.* INTO OUTFILE '##OpModeDistribution##'
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

SELECT Starts.* INTO OUTFILE '##Starts##'
FROM Starts
WHERE yearID = ##context.year##
AND monthID = ##context.monthID##
AND zoneID = ##context.iterLocation.zoneRecordID##;

SELECT StartTempAdjustment.* INTO OUTFILE '##StartTempAdjustment##'
FROM StartTempAdjustment
WHERE polProcessID IN (##pollutantProcessIDs##);

SELECT Year.* INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT DISTINCT ZoneMonthHour.* INTO OUTFILE '##ZoneMonthHour##'
FROM ZoneMonthHour,RunSpecHour
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND ZoneMonthHour.monthID = ##context.monthID##
AND RunSpecHour.hourID = ZoneMonthHour.hourID;

-- End Section Extract Data
--
-- Section Processing

--
-- CSEC 1-a Complete I/M adjustment fraction information
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
GROUP BY ppa.processID,
 ppa.pollutantID,
 ppmy.modelYearID,
 imf.fuelTypeID,
 imc.sourceTypeID;

--
-- CSEC 2-a: Combine GPA and non GPA fuel adjustment factors.
--

DROP TABLE IF EXISTS CountyFuelAdjustment;
CREATE TABLE CountyFuelAdjustment (
	fuelRegionID INTEGER NOT NULL,
	polProcessID int NOT NULL,
	modelYearID INTEGER NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	fuelFormulationID SMALLINT NOT NULL,
	fuelAdjustment FLOAT
);

CREATE UNIQUE INDEX XPKCountyFuelAdjustment ON CountyFuelAdjustment
(
	fuelRegionID ASC,
	polProcessID ASC,
	modelYearID ASC,
	sourceTypeID ASC,
	fuelFormulationID ASC
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

INSERT INTO CountyFuelAdjustment (
  fuelRegionID, polProcessID, modelYearID, sourceTypeID, fuelFormulationID, fuelAdjustment)
SELECT 
	##context.fuelRegionID##,
	ppa.polProcessID,
	stmy.modelYearID,
	stmy.sourceTypeID,
	ff. fuelFormulationID,
	ifnull(ratio,1) + GPAFract * (ifnull(ratioGPA,1)-ifnull(ratio,1))
FROM County c
inner join PollutantProcessAssoc ppa
inner join FuelFormulation ff
inner join SourceTypeModelYear stmy
LEFT OUTER JOIN criteriaRatio fa ON (fa.polProcessID = ppa.polProcessID
	and fa.fuelFormulationID = ff.fuelFormulationID
	and fa.sourceTypeID = stmy.sourceTypeID
	and fa.modelYearID = stmy.modelYearID
)
WHERE ppa.polProcessID IN (##pollutantProcessIDs##)
AND c.countyID = ##context.iterLocation.countyRecordID##
;


--
-- CSEC 2-b: Aggregate county fuel adjustments to fuel type
--

DROP TABLE IF EXISTS CountyFuelAdjustmentWithFuelType;
CREATE TABLE CountyFuelAdjustmentWithFuelType (
	fuelRegionID INTEGER NOT NULL,
	polProcessID int NOT NULL,
	modelYearID SMALLINT NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	fuelFormulationID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	fuelAdjustment FLOAT
);

CREATE UNIQUE INDEX XPKCountyFuelAdjustmentWithFuelType ON CountyFuelAdjustmentWithFuelType 
(
	fuelRegionID ASC,
	polProcessID ASC,
	modelYearID ASC,
	sourceTypeID ASC,
	fuelFormulationID ASC,
	fuelTypeID ASC
);

INSERT INTO CountyFuelAdjustmentWithFuelType (
	fuelRegionID, polProcessID, modelYearID, sourceTypeID,
      fuelFormulationID, fuelTypeID, fuelAdjustment)
SELECT 
  fuelRegionID, cfa.polProcessID, cfa.modelYearID, sourceTypeID,
  cfa.fuelFormulationID, fuelTypeID, fuelAdjustment
FROM CountyFuelAdjustment cfa
INNER JOIN FuelFormulation ff ON (ff.fuelFormulationID = cfa.fuelFormulationID)
INNER JOIN FuelSubType fst ON (fst.fuelSubtypeID = ff.fuelSubtypeID);

DROP TABLE IF EXISTS FuelSupplyAdjustment;
CREATE TABLE FuelSupplyAdjustment (
	yearID SMALLINT NOT NULL,
	countyID INTEGER NOT NULL,
	monthID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	modelYearID SMALLINT NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	fuelAdjustment FLOAT
);

CREATE UNIQUE INDEX XPKFuelSupplyAdjustment ON FuelSupplyAdjustment (
	yearID ASC,
	countyID ASC,
	monthID ASC,
	polProcessID ASC,
	modelYearID ASC,
	sourceTypeID ASC,
	fuelTypeID ASC
);

INSERT INTO FuelSupplyAdjustment (
	yearID, countyID, monthID, polProcessID, modelYearID,
	sourceTypeID, fuelTypeID, fuelAdjustment)
SELECT
	yearID, ##context.iterLocation.countyRecordID## as countyID, monthID, cfa.polProcessID, cfa.modelYearID, 
	cfa.sourceTypeID, cfa.fuelTypeID, SUM(fuelAdjustment * marketShare)
FROM CountyFuelAdjustmentWithFuelType cfa
INNER JOIN Year y
INNER JOIN MonthOfAnyYear may
INNER JOIN FuelSupply fs ON (
	fs.fuelRegionID = cfa.fuelRegionID
	AND fs.fuelYearID = y.fuelYearID
	AND fs.monthGroupID = may.monthGroupID
	AND fs.fuelFormulationID = cfa.fuelFormulationID)
WHERE y.YearID = ##context.year##
GROUP BY yearID, cfa.fuelRegionID, monthID, cfa.polProcessID, 
	cfa.modelYearID, cfa.sourceTypeID, cfa.fuelTypeID
ORDER BY NULL;

--
-- CSEC-3 Calculate temperature adjustment factors.
--
DROP TABLE IF EXISTS METStartAdjustment;
CREATE TABLE METStartAdjustment (
	zoneID INT NOT NULL,
	monthID SMALLINT NOT NULL,
	hourID SMALLINT NOT NULL,
	polProcessID int NOT NULL,
	modelYearID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	opModeID SMALLINT NOT NULL,
	temperatureAdjustment FLOAT
);


INSERT INTO METStartAdjustment (
      zoneID, monthID, hourID, polProcessID, modelYearID, 
	fuelTypeID, opModeID, temperatureAdjustment)
SELECT
	zmh.zoneID,
	zmh.monthID,
	zmh.hourID,
	sta.polProcessID,
	ppmy.modelYearID,
	sta.fuelTypeID,
	sta.opModeID,
     CASE          
		   WHEN sta.startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
		   WHEN sta.startTempEquationType = 'POLY' THEN
                (LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * 
                (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
           ELSE
                (LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * 
                (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
           END AS temperatureAdjustment
FROM StartTempAdjustment sta
INNER JOIN PollutantProcessMappedModelYear ppmy ON (
	ppmy.polProcessID = sta.polProcessID 
	AND ppmy.modelYearGroupID = sta.modelYearGroupID)
INNER JOIN ZoneMonthHour zmh
WHERE zmh.zoneID = ##context.iterLocation.zoneRecordID##;

--
-- CSEC-4: Apply Start Temperature Adjustment to Emission Rates
--

DROP TABLE IF EXISTS EmissionRatesWithIMAndTemp;
CREATE TABLE EmissionRatesWithIMAndTemp (
      zoneID INT NOT NULL,
      monthID SMALLINT NOT NULL,
      hourID SMALLINT NOT NULL,
      yearID SMALLINT NOT NULL,
      polProcessID int NOT NULL,
      modelYearID SMALLINT NOT NULL,
      sourceBinID BIGINT NOT NULL,
      opModeID SMALLINT NOT NULL,
      fuelTypeID SMALLINT NOT NULL,
	  meanBaseRate FLOAT,
	  meanBaseRateIM FLOAT
);

CREATE INDEX METStartAdjustment_New1 ON METStartAdjustment (
	polProcessID ASC,
	modelYearID ASC,
	opModeID ASC,
	fuelTypeID ASC,
	zoneID asc, 
	monthID asc, 
	hourID asc,
	temperatureAdjustment asc
);
CREATE INDEX EmissionRateByAge_New1 ON EmissionRateByAge (
	sourceBinID ASC,
	ageGroupID ASC,
	polProcessID asc,
	opModeID ASC,
	meanBaseRate asc,
	meanBaseRateIM
);
CREATE INDEX SourceBin_New1 ON SourceBin (
      sourceBinID ASC,
      fuelTypeID ASC
);
CREATE INDEX AgeCategory_New1 ON AgeCategory (
	ageGroupID asc,
	ageID asc
);

-- Note: Below, add "0*" to make the expressions ".. + 0*msa.temperatureAdjustment" to disable starts addititive temperature adjustment.
INSERT INTO EmissionRatesWithIMAndTemp (
      zoneID, monthID, hourID, yearID, polProcessID, modelYearID, 
	  sourceBinID, opModeID, fuelTypeID, meanBaseRate, meanBaseRateIM )
SELECT 
      msa.zoneID, msa.monthID, msa.hourID, ##context.year## as yearID, msa.polProcessID, 
      msa.modelYearID, erim.sourceBinID, msa.opModeID, msa.fuelTypeID,
	  (erim.meanBaseRate + msa.temperatureAdjustment) AS meanBaseRate,
	  (erim.meanBaseRateIM + msa.temperatureAdjustment) AS meanBaseRateIM
FROM SourceBin sb 
INNER JOIN EmissionRateByAge erim ON (erim.sourceBinID=sb.sourceBinID)
INNER JOIN AgeCategory age ON (age.ageGroupID=erim.ageGroupID)
INNER JOIN METStartAdjustment msa ON (msa.polProcessID=erim.polProcessID
	AND msa.modelYearID=##context.year##-age.ageID
	AND msa.opModeID=erim.opModeID
	AND msa.fuelTypeID=sb.fuelTypeID);

--
-- CSEC-5: Weight Emission Rates by Source Bin.
--
DROP TABLE IF EXISTS METSourceBinEmissionRates;
CREATE TABLE METSourceBinEmissionRates (
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

CREATE INDEX EmissionRatesWithIMAndTemp3 ON EmissionRatesWithIMAndTemp (
      polProcessID ASC,
      sourceBinID ASC,
      modelYearID ASC
);
CREATE INDEX sourceTypeModelYearID_1 ON sourcetypemodelyear (
      sourceTypeModelYearID ASC,
      modelYearID ASC
);
analyze table SourceBinDistribution;

INSERT INTO METSourceBinEmissionRates (
      zoneID, monthID, hourID, yearID, polProcessID,
      sourceTypeID, modelYearID, fuelTypeID, opModeID,
	  meanBaseRate, meanBaseRateIM)
SELECT 
      er.zoneID, er.monthID, er.hourID, er.yearID, er.polProcessID, 
      stmy.sourceTypeID, stmy.modelYearID, er.fuelTypeID, er.opModeID,
	  sum(meanBaseRate*sourceBinActivityFraction) AS meanBaseRate,
	  sum(meanBaseRateIM*sourceBinActivityFraction) AS meanBaseRateIM
FROM EmissionRatesWithIMAndTemp er, SourceBinDistribution sbd, SourceTypeModelYear stmy
WHERE er.polProcessID=sbd.polProcessID AND
	  er.sourceBinID=sbd.sourceBinID AND
	  sbd.sourceTypeModelYearID=stmy.sourceTypeModelYearID AND
	  er.modelYearID=stmy.modelYearID
GROUP BY zoneID, monthID, hourID, yearID, polProcessID, sourceTypeID, modelYearID, fuelTypeID, opModeID
ORDER BY NULL;

ALTER TABLE METSourceBinEmissionRates ADD INDEX metsourcebinemissionrates1 (
	hourID, sourceTypeID, polProcessID, opModeID);

--
-- CSEC-6 Weight temperature-adjusted emission rates by operating mode.
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

INSERT INTO ActivityWeightedEmissionRate (
      zoneID, yearID, monthID, dayID, hourID, polProcessID,
      sourceTypeID, modelYearID, fuelTypeID, meanBaseRate, meanBaseRateIM )
SELECT 
      zoneID, ##context.year##, monthID, hd.dayID, msber.hourID, msber.polProcessID,
      msber.sourceTypeID, modelYearID, fuelTypeID, 
      SUM(meanBaseRate * opModeFraction),
      SUM(meanBaseRateIM * opModeFraction)
FROM METSourceBinEmissionRates msber
INNER JOIN HourDay hd ON (hd.hourID = msber.hourID)
INNER JOIN OpModeDistribution omd ON (
	omd.sourceTypeID = msber.sourceTypeID
	AND omd.hourDayID = hd.hourDayID 
	AND omd.polProcessID = msber.polProcessID 
	AND omd.opModeID = msber.opModeID)
GROUP BY zoneID, yearID, monthID, hd.dayID, msber.hourID, msber.polProcessID, 
	msber.sourceTypeID, modelYearID, fuelTypeID
ORDER BY NULL;

CREATE UNIQUE INDEX XPKActivityWeightedEmissionRate ON ActivityWeightedEmissionRate (
	yearID ASC,
	monthID ASC,
	polProcessID ASC,
	modelYearID ASC,	
	sourceTypeID ASC,
	fuelTypeID ASC,
	zoneID ASC,
	dayID ASC,
	hourID ASC
);	

-- 
-- CSEC-7: Apply fuel adjustment factor
--

DROP TABLE IF EXISTS ActivityWeightedEmissionRate2;
CREATE TABLE ActivityWeightedEmissionRate2 (
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

INSERT INTO ActivityWeightedEmissionRate2 (
  zoneID, yearID, monthID, dayID, hourID, polProcessID,
  sourceTypeID , modelYearID, fuelTypeID, meanBaseRate, meanBaseRateIM)
SELECT 
  awer.zoneID, awer.yearID, awer.monthID, dayID, hourID, awer.polProcessID,
  awer.sourceTypeID, awer.modelYearID, awer.fuelTypeID,
  meanBaseRate * fuelAdjustment,
  meanBaseRateIM * fuelAdjustment
FROM ActivityWeightedEmissionRate awer 
INNER JOIN FuelSupplyAdjustment fsa ON (
	fsa.yearID = awer.yearID AND fsa.monthID = awer.monthID
	AND fsa.polProcessID = awer.polProcessID AND fsa.modelYearID = awer.modelYearID
	AND fsa.sourceTypeID = awer.sourceTypeID AND fsa.fuelTypeID = awer.fuelTypeID)
INNER JOIN Zone z ON (z.countyID = fsa.countyID AND z.zoneID = awer.zoneID);

CREATE UNIQUE INDEX XPKActivityWeightedEmissionRate ON ActivityWeightedEmissionRate2 (
      zoneID ASC,
      monthID ASC,
      hourID ASC,
	  dayID ASC,
      yearID ASC,
      sourceTypeID ASC,
      modelYearID ASC,
      fuelTypeID ASC,
      polProcessID ASC
);

-- 
-- CSEC-8: Multiply emission rates by start activity to generate inventory.
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

INSERT INTO Starts2 (zoneID, monthID, hourID, dayID, yearID, 
	sourceTypeID, modelYearID, starts) 
SELECT zoneID, monthID, hourID, dayID, yearID, sourceTypeID, 
	(##context.year## - ageID) AS modelYearID, starts
FROM Starts INNER JOIN HourDay ON (Starts.hourDayID= HourDay.hourDayID);

CREATE UNIQUE INDEX XPKStarts2 ON Starts2 (
      zoneID ASC,
      monthID ASC,
      hourID ASC,
	  dayID ASC,
      yearID ASC,
      sourceTypeID ASC,
      modelYearID ASC
);

alter table MOVESWorkerOutput add emissionQuantIM float null default 0.0;

-- alter table MOVESWorkerOutput add emissionQuantIM float null;

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
  (meanBaseRateIM * starts) AS emissionQuant
FROM Starts2 s, ActivityWeightedEmissionRate2 awer, PollutantProcessAssoc ppa
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
update MOVESWorkerOutput, IMCoverageMergedUngrouped set emissionQuant=GREATEST(emissionQuantIM*IMAdjustFract + emissionQuant*(1.0-IMAdjustFract),0.0)
where MOVESWorkerOutput.processID = IMCoverageMergedUngrouped.processID
	and MOVESWorkerOutput.pollutantID = IMCoverageMergedUngrouped.pollutantID
	and MOVESWorkerOutput.modelYearID = IMCoverageMergedUngrouped.modelYearID
	and MOVESWorkerOutput.fuelTypeID = IMCoverageMergedUngrouped.fuelTypeID
	and MOVESWorkerOutput.sourceTypeID = IMCoverageMergedUngrouped.sourceTypeID;

alter table MOVESWorkerOutput drop emissionQuantIM;

flush tables;

-- End Section Processing

-- Section Cleanup

DROP TABLE IF EXISTS ActivityWeightedEmissionRate;
DROP TABLE IF EXISTS ActivityWeightedEmissionRate2;
DROP TABLE IF EXISTS CountyFuelAdjustment;
DROP TABLE IF EXISTS CountyFuelAdjustmentWithFuelType;
DROP TABLE IF EXISTS EmissionRatesWithIM;
DROP TABLE IF EXISTS EmissionRatesWithIMAndTemp;
DROP TABLE IF EXISTS FuelSupplyAdjustment;
drop table if exists IMCoverageMergedUngrouped;
drop table if exists IMCoverageMerged;
DROP TABLE IF EXISTS IMAdjustment;
DROP TABLE IF EXISTS IMAdjustmentWithSourceBin;
DROP TABLE IF EXISTS MetSourceBinEmissionRates;
DROP TABLE IF EXISTS MetStartAdjustment;
DROP TABLE IF EXISTS Starts2;
-- End Section Cleanup


