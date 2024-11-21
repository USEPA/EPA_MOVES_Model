-- Version 2014-04-28
-- Author Wesley Faler

-- @algorithm
-- @owner Liquid Leaking Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

##create.AgeCategory##;
TRUNCATE AgeCategory;

##create.County##;
TRUNCATE County;

##create.EmissionRateByAge##;
TRUNCATE EmissionRateByAge;

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

##create.RunSpecHourDay##;
TRUNCATE RunSpecHourDay;

##create.RunSpecMonth##;
TRUNCATE RunSpecMonth;

##create.RunSpecSourceType##;
TRUNCATE RunSpecSourceType;

##create.SourceBin##;
TRUNCATE SourceBin;

##create.SourceBinDistribution##;
TRUNCATE SourceBinDistribution;

##create.SourceHours##;
TRUNCATE SourceHours;

##create.SourceTypeModelYear##;
TRUNCATE SourceTypeModelYear;

##create.Year##;
TRUNCATE Year;

##create.Zone##;
TRUNCATE Zone;

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

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

-- @algorithm Filter emissionRateByAge to only operating modes 150, 151, and 300.
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
AND SourceTypeModelYear.modelYearID >= ##context.year## - 40
AND EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##)
AND EmissionRateByAge.opModeID IN (150, 151, 300);

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

cache SELECT Link.* INTO OUTFILE '##Link##'
FROM Link WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT * INTO OUTFILE '##MonthOfAnyYear##'
FROM MonthOfAnyYear
WHERE monthID = ##context.monthID##;

-- @algorithm Filter OpModeDistribution to only operating modes 150, 151, and 300.
cache(monthID=##context.monthID##) SELECT OpModeDistribution.* INTO OUTFILE '##OpModeDistribution##'
FROM OpModeDistribution, RunSpecSourceType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND linkID = ##context.iterLocation.linkRecordID##
AND RunSpecSourceType.sourceTypeID = OpModeDistribution.sourceTypeID
AND opModeID IN (150, 151, 300);

cache SELECT * INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE processID=##context.iterProcess.databaseKey##;

cache SELECT * INTO OUTFILE '##PollutantProcessModelYear##'
FROM PollutantProcessModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 40
AND polProcessID IN (##pollutantProcessIDs##);

cache SELECT * INTO OUTFILE '##PollutantProcessMappedModelYear##'
FROM PollutantProcessMappedModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 40
AND polProcessID IN (##pollutantProcessIDs##);

cache SELECT * INTO OUTFILE '##RunSpecMonth##'
FROM RunSpecMonth
WHERE monthID = ##context.monthID##;

cache SELECT * INTO OUTFILE '##RunSpecHourDay##'
FROM RunSpecHourDay;

cache SELECT * INTO OUTFILE '##RunSpecSourceType##'
FROM RunSpecSourceType;

cache SELECT DISTINCT SourceBin.* INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 40
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT SourceBinDistribution.* INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 40
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT * INTO OUTFILE '##SourceHours##' FROM SourceHours
WHERE monthID = ##context.monthID##
AND yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##;

cache SELECT SourceTypeModelYear.* INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear,RunSpecSourceType
WHERE SourceTypeModelYear.sourceTypeID = RunSpecSourceType.sourceTypeID
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 40;

cache SELECT Year.* INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

-- Section WithRegClassID
cache select *
into outfile '##RegClassSourceTypeFraction##'
from RegClassSourceTypeFraction
where modelYearID <= ##context.year##
and modelYearID >= ##context.year## - 40;
-- End Section WithRegClassID

-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'End Extracting Data';

-- End Section Extract Data

-- Section Processing

-- Create tables needed for processing
-- CREATE TABLE IF NOT EXISTS EventLog (eventRowID INTEGER UNSIGNED NOT NULL AUTO_INCREMENT, PRIMARY KEY (eventRowID), eventTime DATETIME, eventName VARCHAR(120));

-- 
-- LL-1: Complete I/M adjustment fraction information (like CREC 1-a)
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'LL-1';
DROP TABLE IF EXISTS IMCoverageMergedUngrouped;
CREATE TABLE IMCoverageMergedUngrouped (
       processID SMALLINT NOT NULL,
       pollutantID SMALLINT NOT NULL,
       modelYearID SMALLINT NOT NULL,
       fuelTypeID SMALLINT NOT NULL,
       sourceTypeID SMALLINT NOT NULL,
       IMAdjustFract FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

CREATE INDEX XPKIMCoverageMergedUngrouped ON IMCoverageMergedUngrouped
(
       processID ASC,
       pollutantID ASC,
       modelYearID ASC,
       fuelTypeID ASC,
       sourceTypeID ASC
);

-- @algorithm Disaggregate IMCoverage records, expanding model year ranges into individual model years. 
-- IMAdjustFract[processID,pollutantID,modelYearID,fuelTypeID,sourceTypeID]=IMFactor*complianceFactor*0.01.
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
-- LL-8: Calculate Adjusted MeanBaseRates
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'LL-8';
drop table if exists WeightedMeanBaseRate;

create table WeightedMeanBaseRate (
	polProcessID int not null,
	sourceTypeID smallint(6) not null,
	regClassID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	monthID smallint(6) not null,
	hourDayID smallint(6) not null,
	modelYearID smallint(6) not null,
	opModeID smallint(6) not null,
	weightedMeanBaseRate float not null,
	weightedMeanBaseRateIM float not null,
	primary key (polProcessID, sourceTypeID, regClassID, fuelTypeID, monthID, hourDayID, modelYearID, opModeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Now for all operating modes (formerly only for hot soaking and operating)
-- Section WithRegClassID

-- @algorithm weightedMeanBaseRate = sourceBinActivityFraction * meanBaseRate.
-- weightedMeanBaseRateIM = sourceBinActivityFraction * meanBaseRateIM.
insert into WeightedMeanBaseRate (polProcessID, sourceTypeID, regClassID, fuelTypeID, monthID, hourDayID, 
	modelYearID, opModeID, weightedMeanBaseRate, weightedMeanBaseRateIM)
select er.polProcessID, stmy.sourceTypeID, sb.regClassID, sb.fuelTypeID, rsm.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID,
	sum(sourceBinActivityFraction*meanBaseRate) as weightedMeanBaseRate,
	sum(sourceBinActivityFraction*meanBaseRateIM) as weightedMeanBaseRateIM
from EmissionRateByAge er
inner join AgeCategory acat on (acat.ageGroupID=er.ageGroupID)
inner join SourceBin sb on (sb.sourceBinID=er.sourceBinID)
inner join FuelType on (FuelType.fuelTypeID = sb.fuelTypeID and subjectToEvapCalculations = 'Y')
inner join SourceBinDistribution sbd on (sbd.sourceBinID=sb.sourceBinID
	and sbd.polProcessID=er.polProcessID)
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
	and stmy.modelYearID=##context.year##-acat.ageID)
inner join PollutantProcessModelYear ppmy on (ppmy.polProcessID=sbd.polProcessID
	and ppmy.modelYearID=stmy.modelYearID and ppmy.modelYearGroupID=sb.modelYearGroupID)
inner join RunSpecSourceType rsst on (rsst.sourceTypeID=stmy.sourceTypeID)
inner join RunSpecMonth rsm
inner join RunSpecHourDay rshd
where er.polProcessID in (##pollutantProcessIDs##)
group by er.polProcessID, stmy.sourceTypeID, sb.regClassID, sb.fuelTypeID, rsm.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID
order by null;
-- End Section WithRegClassID

-- Section NoRegClassID
insert into WeightedMeanBaseRate (polProcessID, sourceTypeID, regClassID, fuelTypeID, monthID, hourDayID, 
	modelYearID, opModeID, weightedMeanBaseRate, weightedMeanBaseRateIM)
select er.polProcessID, stmy.sourceTypeID, 0 as regClassID, sb.fuelTypeID, rsm.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID,
	sum(sourceBinActivityFraction*meanBaseRate) as weightedMeanBaseRate,
	sum(sourceBinActivityFraction*meanBaseRateIM) as weightedMeanBaseRateIM
from EmissionRateByAge er
inner join AgeCategory acat on (acat.ageGroupID=er.ageGroupID)
inner join SourceBin sb on (sb.sourceBinID=er.sourceBinID)
inner join FuelType on (FuelType.fuelTypeID = sb.fuelTypeID and subjectToEvapCalculations = 'Y')
inner join SourceBinDistribution sbd on (sbd.sourceBinID=sb.sourceBinID
	and sbd.polProcessID=er.polProcessID)
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
	and stmy.modelYearID=##context.year##-acat.ageID)
inner join PollutantProcessModelYear ppmy on (ppmy.polProcessID=sbd.polProcessID
	and ppmy.modelYearID=stmy.modelYearID and ppmy.modelYearGroupID=sb.modelYearGroupID)
inner join RunSpecSourceType rsst on (rsst.sourceTypeID=stmy.sourceTypeID)
inner join RunSpecMonth rsm
inner join RunSpecHourDay rshd
where er.polProcessID in (##pollutantProcessIDs##)
group by er.polProcessID, stmy.sourceTypeID, sb.fuelTypeID, rsm.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID
order by null;
-- End Section NoRegClassID

analyze table WeightedMeanBaseRate;

alter table MOVESWorkerOutput add emissionQuantIM float null;

-- 
-- LL-9: Calculate MOVESWorkerOutput by Source Type
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'LL-9 without SCC';

-- @algorithm emissionQuant = weightedMeanBaseRate * sourceHours * opModeFraction.
-- emissionQuantIM = weightedMeanBaseRateIM * sourceHours * opModeFraction.
insert into MOVESWorkerOutput (yearID, monthID, dayID, hourID, stateID, countyID,
	zoneID, linkID, pollutantID, processID, sourceTypeID, regClassID, fuelTypeID, modelYearID,
	roadTypeID, SCC, emissionQuant, emissionQuantIM)
select ##context.year## as yearID, w.monthID, hd.dayID, hd.hourID,
	##context.iterLocation.stateRecordID## as stateID,
	##context.iterLocation.countyRecordID## as countyID,
	##context.iterLocation.zoneRecordID## as zoneID,
	##context.iterLocation.linkRecordID## as linkID,
	ppa.pollutantID, ppa.processID, w.sourceTypeID, w.regClassID, w.fuelTypeID, w.modelYearID,
	l.roadTypeID, null as SCC,
	(weightedMeanBaseRate*sourceHours*opModeFraction) as emissionQuant,
	(weightedMeanBaseRateIM*sourceHours*opModeFraction) as emissionQuantIM
from WeightedMeanBaseRate w
inner join SourceHours sh on (sh.hourDayID=w.hourDayID and sh.monthID=w.monthID
	and sh.yearID=##context.year## and sh.ageID=##context.year##-w.modelYearID
	and sh.linkID=##context.iterLocation.linkRecordID## and sh.sourceTypeID=w.sourceTypeID)
inner join OpModeDistribution omd on (omd.sourceTypeID=sh.sourceTypeID
	and omd.hourDayID=w.hourDayID and omd.linkID=##context.iterLocation.linkRecordID##
	and omd.polProcessID=w.polProcessID and omd.opModeID=w.opModeID)
inner join PollutantProcessAssoc ppa on (ppa.polProcessID=omd.polProcessID)
inner join Link l on (l.linkID=##context.iterLocation.linkRecordID##)
inner join HourDay hd on (hd.hourDayID=omd.hourDayID);

-- Apply IM

-- @algorithm Apply I/M programs.
-- emissionQuant=emissionQuantIM*IMAdjustFract + emissionQuant*(1-IMAdjustFract).
update MOVESWorkerOutput, IMCoverageMergedUngrouped set emissionQuant=GREATEST(emissionQuantIM*IMAdjustFract + emissionQuant*(1.0-IMAdjustFract),0.0)
where MOVESWorkerOutput.processID = IMCoverageMergedUngrouped.processID
	and MOVESWorkerOutput.pollutantID = IMCoverageMergedUngrouped.pollutantID
	and MOVESWorkerOutput.modelYearID = IMCoverageMergedUngrouped.modelYearID
	and MOVESWorkerOutput.fuelTypeID = IMCoverageMergedUngrouped.fuelTypeID
	and MOVESWorkerOutput.sourceTypeID = IMCoverageMergedUngrouped.sourceTypeID;

alter table MOVESWorkerOutput drop emissionQuantIM;

-- End Section Processing
-- Section Cleanup
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'Section Cleanup';

drop table if exists WeightedMeanBaseRate;
drop table if exists IMCoverageMergedUngrouped;
-- End Section Cleanup
