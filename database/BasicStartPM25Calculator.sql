-- Version 2013-09-15
-- Wesley Faler
-- David Brzezinski (EPA)

-- @notused

-- Special section names invoked by BasicStartPMEmissionCalculator.java:
--              HasManyOpModes
--              EmissionRateByAgeRate
--              StartsActivity
--              ApplyTemperatureAdjustment

-- Section Create Remote Tables for Extracted Data
##create.AgeCategory##;
TRUNCATE TABLE AgeCategory;

##create.County##;
TRUNCATE TABLE County;

##create.HourDay##;
TRUNCATE TABLE HourDay;

##create.Link##;
TRUNCATE TABLE Link;

##create.Zone##;
TRUNCATE TABLE Zone;

##create.Pollutant##;
TRUNCATE TABLE Pollutant;

##create.EmissionProcess##;
TRUNCATE TABLE EmissionProcess;

-- Section EmissionRateByAgeRates
##create.EmissionRateByAge##;
TRUNCATE TABLE EmissionRateByAge;
-- End Section EmissionRateByAgeRates

-- Section HasManyOpModes
##create.OpModeDistribution##;
TRUNCATE TABLE OpModeDistribution;
-- End Section HasManyOpModes

##create.SourceBin##;
TRUNCATE TABLE SourceBin;

##create.SourceBinDistribution##;
TRUNCATE TABLE SourceBinDistribution;

##create.SourceTypeModelYear##;
TRUNCATE TABLE SourceTypeModelYear;

##create.PollutantProcessAssoc##;
TRUNCATE TABLE PollutantProcessAssoc;

##create.PollutantProcessModelYear##;
TRUNCATE TABLE PollutantProcessModelYear;

##create.PollutantProcessMappedModelYear##;
TRUNCATE PollutantProcessMappedModelYear;

-- Section StartsActivity
##create.Starts##;
TRUNCATE TABLE Starts;
-- End Section StartsActivity

-- Section ApplyTemperatureAdjustment
##create.ModelYearGroup##;
TRUNCATE TABLE ModelYearGroup;

##create.TemperatureAdjustment##;
TRUNCATE TABLE TemperatureAdjustment;

##create.StartTempAdjustment##;
TRUNCATE TABLE StartTempAdjustment;

##create.ZoneMonthHour##;
TRUNCATE TABLE ZoneMonthHour;
-- End Section ApplyTemperatureAdjustment

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
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
truncate oneCountyYearGeneralFuelRatio;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
cache SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT Link.*
INTO OUTFILE '##Link##'
FROM Link
WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT * 
INTO OUTFILE '##EmissionProcess##'
FROM EmissionProcess
WHERE processID=##context.iterProcess.databaseKey##;

-- Section HasManyOpModes
SELECT * 
INTO OUTFILE '##OpModeDistribution##'
FROM OpModeDistribution, RunSpecSourceType, RunSpecHourDay
WHERE polProcessID IN (##pollutantProcessIDs##)
AND linkID = ##context.iterLocation.linkRecordID##
AND RunSpecSourceType.sourceTypeID = OpModeDistribution.sourceTypeID
AND RunSpecHourDay.hourDayID = OpModeDistribution.hourDayID;
-- End Section HasManyOpModes

cache SELECT *
INTO OUTFILE '##Pollutant##'
FROM Pollutant;

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

-- Section EmissionRateByAgeRates
SELECT DISTINCT EmissionRateByAge.* 
INTO OUTFILE '##EmissionRateByAge##'
FROM EmissionRateByAge, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##)
AND EmissionRateByAge.polProcessID = SourceBinDistribution.polProcessID
AND EmissionRateByAge.SourceBinID = SourceBinDistribution.SourceBinID
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 40
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;
-- End Section EmissionRateByAgeRates

cache SELECT SourceTypeModelYear.* 
INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear,RunSpecSourceType
WHERE SourceTypeModelYear.sourceTypeID = RunSpecSourceType.sourceTypeID
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 40;

cache SELECT DISTINCT HourDay.* 
INTO OUTFILE '##HourDay##'
FROM HourDay,RunSpecHour,RunSpecDay
WHERE HourDay.dayID = RunSpecDay.dayID
AND HourDay.hourID = RunSpecHour.hourID;

cache SELECT * 
INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT * 
INTO OUTFILE '##PollutantProcessModelYear##'
FROM PollutantProcessModelYear
WHERE polProcessID IN (##pollutantProcessIDs##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 40;

cache SELECT * INTO OUTFILE '##PollutantProcessMappedModelYear##'
FROM PollutantProcessMappedModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 40
AND polProcessID IN (##pollutantProcessIDs##);

-- Section StartsActivity
SELECT Starts.* INTO OUTFILE '##Starts##'
FROM Starts
WHERE yearID = ##context.year##
AND zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section StartsActivity

-- Section ApplyTemperatureAdjustment
cache SELECT DISTINCT ModelYearGroup.*
INTO OUTFILE '##ModelYearGroup##'
FROM ModelYearGroup;

cache SELECT DISTINCT TemperatureAdjustment.*
INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
INNER JOIN RunSpecSourceFuelType USING (fuelTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT DISTINCT StartTempAdjustment.*
INTO OUTFILE '##StartTempAdjustment##'
FROM StartTempAdjustment
INNER JOIN RunSpecSourceFuelType USING (fuelTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT ZoneMonthHour.*
INTO OUTFILE '##ZoneMonthHour##'
FROM RunSpecMonth
INNER JOIN RunSpecHour
INNER JOIN ZoneMonthHour ON (ZoneMonthHour.monthID = RunSpecMonth.monthID AND ZoneMonthHour.hourID = RunSpecHour.hourID)
WHERE zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section ApplyTemperatureAdjustment

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

-- Section Local Data Removal
--TRUNCATE XXXXXX;
-- End Section Local Data Removal

-- Section Processing

-- --------------------------------------------------------------
-- Step 1: Weight emission rates by operating mode
-- --------------------------------------------------------------
drop table if exists OpModeWeightedEmissionRateTemp;
create table OpModeWeightedEmissionRateTemp (
	hourDayID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	sourceBinID bigint(20) not null,
	ageGroupID smallint(6) not null,
	polProcessID int not null,
	opModeID smallint(6) not null,
	opModeWeightedMeanBaseRate float
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Section EmissionRateByAgeRates

-- Section HasManyOpModes
insert into OpModeWeightedEmissionRateTemp (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeID, opModeWeightedMeanBaseRate)
select distinct omd.hourDayID, omd.sourceTypeID, er.sourceBinID, er.ageGroupID, omd.polProcessID, omd.opModeID,
(opModeFraction * meanBaseRate) as opModeWeightedMeanBaseRate
from OpModeDistribution omd
inner join EmissionRateByAge er using (polProcessID, opModeID)
inner join SourceBinDistribution sbd on (
	sbd.polProcessID=er.polProcessID and sbd.sourceBinID=er.sourceBinID)
inner join AgeCategory acat on (acat.ageGroupID=er.ageGroupID)
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
and stmy.sourceTypeID=omd.sourceTypeID
and stmy.modelYearID=##context.year##-ageID);
-- End Section HasManyOpModes
-- End Section EmissionRateByAgeRates

drop table if exists OpModeWeightedEmissionRate;
create table OpModeWeightedEmissionRate (
	hourDayID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	sourceBinID bigint(20) not null,
	ageGroupID smallint(6) not null,
	polProcessID int not null,
	opModeID smallint(6) not null,
	opModeWeightedMeanBaseRate float,
	primary key (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeID),
	index (hourDayID),
	index (sourceTypeID),
	index (sourceBinID),
	index (ageGroupID),
	index (polProcessID),
	index (opModeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into OpModeWeightedEmissionRate (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeID, opModeWeightedMeanBaseRate)
select hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeID, sum(opModeWeightedMeanBaseRate)
from OpModeWeightedEmissionRateTemp
group by hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeID
order by null;

-- --------------------------------------------------------------
-- Step 2: Weight Emission Rates by Source Bin
-- --------------------------------------------------------------
drop table if exists FullyWeightedEmissionRate;
create table FullyWeightedEmissionRate (
	yearID smallint(6) not null,
	hourDayID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint(6) not null,
	polProcessID int not null,
	fullyWeightedMeanBaseRate float,
	ageID smallint(6) not null,
	opModeID smallint(6) not null,
	primary key (yearID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, opModeID),
	index (yearID),
	index (hourDayID),
	index (sourceTypeID),
	index (fuelTypeID),
	index (modelYearID),
	index (polProcessID),
	index (ageID),
	index (opModeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into FullyWeightedEmissionRate (yearID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, fullyWeightedMeanBaseRate, ageID, opModeID)
select ##context.year## as yearID, omer.hourDayID, omer.sourceTypeID, sb.fuelTypeID, stmy.modelYearID, omer.polProcessID,
sum(sourceBinActivityFraction*opModeWeightedMeanBaseRate) as fullyWeightedMeanBaseRate,
acat.ageID, omer.opModeID
from OpModeWeightedEmissionRate omer
inner join SourceBinDistribution sbd on (sbd.sourceBinID=omer.sourceBinID and sbd.polProcessID=omer.polProcessID)
inner join AgeCategory acat on (acat.ageGroupID=omer.ageGroupID)
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
and stmy.sourceTypeID=omer.sourceTypeID and stmy.modelYearID=##context.year##-acat.ageID)
inner join PollutantProcessModelYear ppmy on (ppmy.polProcessID=sbd.polProcessID and ppmy.modelYearID=stmy.modelYearID)
inner join SourceBin sb on (sb.sourceBinID=sbd.sourceBinID and sb.modelYearGroupID=ppmy.modelYearGroupID)
group by omer.hourDayID, omer.sourceTypeID, sb.fuelTypeID, stmy.modelYearID, omer.polProcessID, acat.ageID
order by null;

-- --------------------------------------------------------------
-- Step 3: Multiply Emission Rates by Activity
-- --------------------------------------------------------------
drop table if exists UnadjustedEmissionResults;
create table UnadjustedEmissionResults (
	yearID smallint(6) not null,
	monthID smallint(6) not null,
	hourDayID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint(6) not null,
	polProcessID int not null,
	opModeID smallint(6) not null,
	unadjustedEmissionQuant float,
	primary key (yearID, monthID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, opModeID),
	index (yearID),
	index (monthID),
	index (hourDayID),
	index (sourceTypeID),
	index (fuelTypeID),
	index (modelYearID),
	index (polProcessID),
	index (opModeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Section StartsActivity
insert into UnadjustedEmissionResults (yearID, monthID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, opModeID, unadjustedEmissionQuant)
select f.yearID, Starts.monthID, f.hourDayID, f.sourceTypeID, f.fuelTypeID, f.modelYearID, f.polProcessID, f.opModeID,
(fullyWeightedMeanBaseRate*Starts.starts) as unadjustedEmissionQuant
from FullyWeightedEmissionRate f
inner join Starts using (hourDayID, yearID, ageID, sourceTypeID);
-- End Section StartsActivity

-- --------------------------------------------------------------
-- Step 4: Apply Temperature Adjustment
-- --------------------------------------------------------------
drop table if exists AdjustedEmissionResults;
create table AdjustedEmissionResults (
	yearID smallint(6) not null,
	monthID smallint(6) not null,
	dayID smallint(6) not null,
	hourID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint(6) not null,
	polProcessID int not null,
	emissionQuant float,
	primary key (yearID, monthID, dayID, hourID, sourceTypeID, fuelTypeID, modelYearID, polProcessID),
	index (yearID),
	index (monthID),
	index (dayID),
	index (hourID),
	index (sourceTypeID),
	index (fuelTypeID),
	index (modelYearID),
	index (polProcessID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Section ApplyTemperatureAdjustment
insert into AdjustedEmissionResults (yearID, monthID, dayID, hourID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, emissionQuant)
select u.yearID, u.monthID, hd.dayID, hd.hourID, u.sourceTypeID, u.fuelTypeID, u.modelYearID, u.polProcessID,
sum(coalesce(
	unadjustedEmissionQuant*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
	,unadjustedEmissionQuant)) as emissionQuant
from UnadjustedEmissionResults u
inner join HourDay hd using (hourDayID)
inner join ZoneMonthHour zmh on (zmh.monthID=u.monthID and zmh.hourID=hd.hourID)
inner join PollutantProcessMappedModelYear ppmy on (ppmy.polProcessId=u.polProcessID and ppmy.modelYearID=u.modelYearID)
left outer join StartTempAdjustment ta on (
	ta.polProcessID=u.polProcessID
	and ta.fuelTypeID=u.fuelTypeID
	and ta.opModeID=u.opModeID
	and ta.modelYearGroupID=ppmy.modelYearGroupID
)
group by u.yearID, u.monthID, hd.dayID, hd.hourID, u.sourceTypeID, u.fuelTypeID, u.modelYearID, u.polProcessID
order by null;
-- End Section ApplyTemperatureAdjustment

-- --------------------------------------------------------------
-- Step 5: Convert Results to Structure of MOVESWorkerOutput by sourceTypeID
-- --------------------------------------------------------------
insert into MOVESWorkerOutput (yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID,
pollutantID, processID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, emissionQuant)
select a.yearID, a.monthID, a.dayID, a.hourID,
##context.iterLocation.stateRecordID## as stateID,
##context.iterLocation.countyRecordID## as countyID,
##context.iterLocation.zoneRecordID## as zoneID,
##context.iterLocation.linkRecordID## as linkID,
ppa.pollutantID, ppa.processID,
a.sourceTypeID, a.fuelTypeID, a.modelYearID, l.roadTypeID, a.emissionQuant
from AdjustedEmissionResults a
inner join PollutantProcessAssoc ppa on (ppa.polProcessID=a.polProcessID)
inner join Link l;

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
drop table if exists OpModeWeightedEmissionRateTemp;
drop table if exists OpModeWeightedEmissionRate;
drop table if exists FullyWeightedEmissionRate;
drop table if exists UnadjustedEmissionResults;
drop table if exists AdjustedEmissionResults;
drop table if exists oneCountyYearGeneralFuelRatio;
-- End Section Cleanup
