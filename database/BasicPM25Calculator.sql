-- Version 2013-09-15
-- Wesley Faler
-- Ed Campbell
-- Gwo Shyu/EPA 2008-07-025 - Added PM2.5 fuel adjustment
-- Supported special section names:
-- 		HasManyOpModes
-- 		HasOneOpMode
-- 		EmissionRateByAgeRates
-- 		EmissionRateRates
-- 		SourceHoursOperatingActivity
-- 		SourceHoursActivity
--		StartsActivity
-- 		ApplyTemperatureAdjustment
--		ApplyLinearTemperatureAdjustment
-- 		NoTemperatureAdjustment

-- @notused

-- Section Create Remote Tables for Extracted Data
##create.AgeCategory##;
TRUNCATE TABLE AgeCategory;

##create.County##;
TRUNCATE TABLE County;

##create.generalFuelRatio##;
TRUNCATE TABLE generalFuelRatio;

##create.HourDay##;
TRUNCATE TABLE HourDay;

##create.Link##;
TRUNCATE TABLE Link;

##create.MonthOfAnyYear##;
TRUNCATE MonthOfAnyYear;

##create.RunSpecSourceType##;
TRUNCATE RunSpecSourceType;

##create.Zone##;
TRUNCATE TABLE Zone;

##create.Pollutant##;
TRUNCATE TABLE Pollutant;

##create.EmissionProcess##;
TRUNCATE TABLE EmissionProcess;

-- Section EmissionRateRates
##create.EmissionRate##;
TRUNCATE TABLE EmissionRate;
-- End Section EmissionRateRates

-- Section EmissionRateByAgeRates
##create.EmissionRateByAge##;
TRUNCATE TABLE EmissionRateByAge;
-- End Section EmissionRateByAgeRates

##create.Year##;
TRUNCATE TABLE Year;

##create.FuelFormulation##;
TRUNCATE FuelFormulation;

##create.FuelSubType##;
TRUNCATE FuelSubType;

##create.FuelSupply##;
TRUNCATE FuelSupply;

##create.FuelType##;
TRUNCATE FuelType;

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

-- Section SourceHoursOperatingActivity
##create.SHO##;
TRUNCATE TABLE SHO;
-- End Section SourceHoursOperatingActivity

-- Section SourceHoursActivity
##create.SourceHours##;
TRUNCATE TABLE SourceHours;
-- End Section SourceHoursActivity

-- Section StartsActivity
##create.Starts##;
TRUNCATE TABLE Starts;
-- End Section StartsActivity

-- Section ApplyTemperatureAdjustment
##create.TemperatureAdjustment##;
TRUNCATE TABLE TemperatureAdjustment;

##create.ZoneMonthHour##;
TRUNCATE TABLE ZoneMonthHour;
-- End Section ApplyTemperatureAdjustment

-- Section ApplyLinearTemperatureAdjustment
##create.TemperatureAdjustment##;
TRUNCATE TABLE TemperatureAdjustment;

##create.ZoneMonthHour##;
TRUNCATE TABLE ZoneMonthHour;
-- End Section ApplyLinearTemperatureAdjustment

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
cache SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

drop table if exists tempFuelFormulation;
create table if not exists tempFuelFormulation (
	fuelFormulationID int not null primary key
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
insert into tempFuelFormulation (fuelFormulationID)
SELECT distinct fuelFormulationID
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##;

SELECT gfr.* INTO OUTFILE '##generalFuelRatio##'
FROM generalFuelRatio gfr
INNER JOIN tempFuelFormulation tff on tff.fuelFormulationID = gfr.fuelFormulationID
WHERE polProcessID IN (##pollutantProcessIDs##)
AND minModelYearID <= ##context.year##;

drop table tempFuelFormulation;

cache SELECT * INTO OUTFILE '##RunSpecSourceType##'
FROM RunSpecSourceType;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT Link.*
INTO OUTFILE '##Link##'
FROM Link
WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT MonthOfAnyYear.*
INTO OUTFILE '##MonthOfAnyYear##'
FROM MonthOfAnyYear;

cache SELECT * 
INTO OUTFILE '##EmissionProcess##'
FROM EmissionProcess
WHERE processID=##context.iterProcess.databaseKey##;

-- Section HasManyOpModes
cache SELECT * 
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

-- Section EmissionRateRates
cache SELECT DISTINCT EmissionRate.* 
INTO OUTFILE '##EmissionRate##'
FROM EmissionRate, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE EmissionRate.polProcessID IN (##pollutantProcessIDs##)
AND EmissionRate.polProcessID = SourceBinDistribution.polProcessID
AND EmissionRate.SourceBinID = SourceBinDistribution.SourceBinID
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 40
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;
-- End Section EmissionRateRates

-- Section EmissionRateByAgeRates
cache SELECT DISTINCT EmissionRateByAge.* INTO OUTFILE '##EmissionRateByAge##'
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

cache SELECT * INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

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

-- Section SourceHoursOperatingActivity
SELECT SHO.* 
INTO OUTFILE '##SHO##'
FROM SHO
INNER JOIN RunSpecMonth USING (monthID)
WHERE yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##;
-- End Section SourceHoursOperatingActivity

-- Section SourceHoursActivity
SELECT SourceHours.* 
INTO OUTFILE '##SourceHours##'
FROM SourceHours
INNER JOIN RunSpecMonth USING (monthID)
WHERE yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##;
-- End Section SourceHoursActivity

-- Section StartsActivity
SELECT Starts.* INTO OUTFILE '##Starts##'
FROM Starts
WHERE yearID = ##context.year##
AND zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section StartsActivity

-- Section ApplyTemperatureAdjustment
cache SELECT DISTINCT TemperatureAdjustment.*
INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
INNER JOIN RunSpecSourceFuelType USING (fuelTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT ZoneMonthHour.*
INTO OUTFILE '##ZoneMonthHour##'
FROM RunSpecMonth
INNER JOIN RunSpecHour
INNER JOIN ZoneMonthHour ON (ZoneMonthHour.monthID = RunSpecMonth.monthID AND ZoneMonthHour.hourID = RunSpecHour.hourID)
WHERE zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section ApplyTemperatureAdjustment

-- Section ApplyLinearTemperatureAdjustment
cache SELECT DISTINCT TemperatureAdjustment.*
INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
INNER JOIN RunSpecSourceFuelType USING (fuelTypeID)
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT ZoneMonthHour.*
INTO OUTFILE '##ZoneMonthHour##'
FROM RunSpecMonth
INNER JOIN RunSpecHour
INNER JOIN ZoneMonthHour ON (ZoneMonthHour.monthID = RunSpecMonth.monthID AND ZoneMonthHour.hourID = RunSpecHour.hourID)
WHERE zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section ApplyLinearTemperatureAdjustment

-- End Section Extract Data

-- Section Local Data Removal
--TRUNCATE XXXXXX;
-- End Section Local Data Removal

-- Section Processing

-- -----------------------------------------------------
-- BRPMC Step 1: Weight emission rates by operating mode
-- -----------------------------------------------------

drop table if exists OpModeWeightedEmissionRateTemp;
create table OpModeWeightedEmissionRateTemp (
	hourDayID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	sourceBinID bigint(20) not null,
	ageGroupID smallint(6) not null,
	polProcessID int not null,
	opModeWeightedMeanBaseRate float
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';


-- Section EmissionRateByAgeRates
-- Section HasOneOpMode
insert into OpModeWeightedEmissionRateTemp (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeWeightedMeanBaseRate)
select distinct hd.hourDayID, stmy.sourceTypeID, er.sourceBinID, er.ageGroupID, sbd.polProcessID,
meanBaseRate as opModeWeightedMeanBaseRate
from HourDay hd
inner join EmissionRateByAge er
inner join SourceBinDistribution sbd on (sbd.polProcessID=er.polProcessID
and sbd.sourceBinID=er.sourceBinID)
inner join AgeCategory acat on (acat.ageGroupID=er.ageGroupID)
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
and stmy.modelYearID=##context.year##-ageID);
-- End Section HasOneOpMode

-- Section HasManyOpModes
insert into OpModeWeightedEmissionRateTemp (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeWeightedMeanBaseRate)
select distinct omd.hourDayID, omd.sourceTypeID, er.sourceBinID, er.ageGroupID, omd.polProcessID,
(opModeFraction * meanBaseRate) as opModeWeightedMeanBaseRate
from OpModeDistribution omd
inner join EmissionRateByAge er using (polProcessID, opModeID)
inner join SourceBinDistribution sbd on (sbd.polProcessID=er.polProcessID
and sbd.sourceBinID=er.sourceBinID)
inner join AgeCategory acat on (acat.ageGroupID=er.ageGroupID)
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
and stmy.sourceTypeID=omd.sourceTypeID
and stmy.modelYearID=##context.year##-ageID);
-- End Section HasManyOpModes
-- End Section EmissionRateByAgeRates

-- Section EmissionRateRates
-- Section HasOneOpMode
insert into OpModeWeightedEmissionRateTemp (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeWeightedMeanBaseRate)
select distinct hd.hourDayID, stmy.sourceTypeID, er.sourceBinID, acat.ageGroupID, sbd.polProcessID,
(meanBaseRate) as opModeWeightedMeanBaseRate
from HourDay hd
inner join EmissionRate er
inner join SourceBinDistribution sbd on (sbd.polProcessID=er.polProcessID
and sbd.sourceBinID=er.sourceBinID)
inner join AgeCategory acat
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
and stmy.modelYearID=##context.year##-ageID);
-- End Section HasOneOpMode

-- Section HasManyOpModes
insert into OpModeWeightedEmissionRateTemp (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeWeightedMeanBaseRate)
select distinct omd.hourDayID, omd.sourceTypeID, er.sourceBinID, acat.ageGroupID, omd.polProcessID,
(opModeFraction * meanBaseRate) as opModeWeightedMeanBaseRate
from OpModeDistribution omd
inner join EmissionRate er using (polProcessID, opModeID)
inner join SourceBinDistribution sbd on (sbd.polProcessID=er.polProcessID
and sbd.sourceBinID=er.sourceBinID)
inner join AgeCategory acat
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
and stmy.sourceTypeID=omd.sourceTypeID
and stmy.modelYearID=##context.year##-ageID);
-- End Section HasManyOpModes
-- End Section EmissionRateRates

drop table if exists OpModeWeightedEmissionRate;
create table OpModeWeightedEmissionRate (
	hourDayID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	sourceBinID bigint(20) not null,
	ageGroupID smallint(6) not null,
	polProcessID int not null,
	opModeWeightedMeanBaseRate float,
	primary key (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID),
	index (hourDayID),
	index (sourceTypeID),
	index (sourceBinID),
	index (ageGroupID),
	index (polProcessID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into OpModeWeightedEmissionRate (hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, opModeWeightedMeanBaseRate)
select hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID, sum(opModeWeightedMeanBaseRate)
from OpModeWeightedEmissionRateTemp
group by hourDayID, sourceTypeID, sourceBinID, ageGroupID, polProcessID
order by null;

-- --------------------------------------------------------------
-- BRPMC Step 2: Weight Emission Rates by Source Bin
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
	ageID smallint(6) not null
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into FullyWeightedEmissionRate (yearID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, fullyWeightedMeanBaseRate, ageID)
select ##context.year## as yearID, omer.hourDayID, omer.sourceTypeID, sb.fuelTypeID, stmy.modelYearID, omer.polProcessID,
sum(sourceBinActivityFraction*opModeWeightedMeanBaseRate) as fullyWeightedMeanBaseRate,
acat.ageID
from OpModeWeightedEmissionRate omer
inner join SourceBinDistribution sbd on (sbd.sourceBinID=omer.sourceBinID and sbd.polProcessID=omer.polProcessID)
inner join AgeCategory acat on (acat.ageGroupID=omer.ageGroupID)
inner join SourceTypeModelYear stmy on (stmy.sourceTypeModelYearID=sbd.sourceTypeModelYearID
and stmy.sourceTypeID=omer.sourceTypeID and stmy.modelYearID=##context.year##-acat.ageID)
inner join PollutantProcessModelYear ppmy on (ppmy.polProcessID=sbd.polProcessID and ppmy.modelYearID=stmy.modelYearID)
inner join SourceBin sb on (sb.sourceBinID=sbd.sourceBinID and sb.modelYearGroupID=ppmy.modelYearGroupID)
group by omer.hourDayID, omer.sourceTypeID, sb.fuelTypeID, stmy.modelYearID, omer.polProcessID, acat.ageID
order by null;

create index ixFullyWeightedEmissionRate1 on FullyWeightedEmissionRate (
	hourDayID asc, 
	yearID asc, 
	ageID asc, 
	sourceTypeID asc
);	

-- --------------------------------------------------------------
-- BRPMC Step 3: Multiply Emission Rates by Activity
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
	unadjustedEmissionQuant float
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Section SourceHoursOperatingActivity
insert ignore into UnadjustedEmissionResults (yearID, monthID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, unadjustedEmissionQuant)
select f.yearID, sho.monthID, f.hourDayID, f.sourceTypeID, f.fuelTypeID, f.modelYearID, f.polProcessID,
(fullyWeightedMeanBaseRate*sho.SHO) as unadjustedEmissionQuant
from FullyWeightedEmissionRate f
inner join sho using (hourDayID, yearID, ageID, sourceTypeID);
-- End Section SourceHoursOperatingActivity

-- Section SourceHoursActivity
insert into UnadjustedEmissionResults (yearID, monthID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, unadjustedEmissionQuant)
select f.yearID, SourceHours.monthID, f.hourDayID, f.sourceTypeID, f.fuelTypeID, f.modelYearID, f.polProcessID,
(fullyWeightedMeanBaseRate*SourceHours.sourceHours) as unadjustedEmissionQuant
from FullyWeightedEmissionRate f
inner join SourceHours using (hourDayID, yearID, ageID, sourceTypeID);
-- End Section SourceHoursActivity

-- Section StartsActivity
insert into UnadjustedEmissionResults (yearID, monthID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, unadjustedEmissionQuant)
select f.yearID, Starts.monthID, f.hourDayID, f.sourceTypeID, f.fuelTypeID, f.modelYearID, f.polProcessID,
(fullyWeightedMeanBaseRate*Starts.starts) as unadjustedEmissionQuant
from FullyWeightedEmissionRate f
inner join Starts using (hourDayID, yearID, ageID, sourceTypeID);
-- End Section StartsActivity

create index ixUnadjustedEmissionResults1 on UnadjustedEmissionResults (
	yearID asc, 
	monthID asc, 
	sourceTypeID asc, 
	fuelTypeID asc, 
	modelYearID asc, 
	polProcessID asc
);

-- --------------------------------------------------------------
-- BRPMC Step 4: Weight emission rates by fuel adjustment
-- --------------------------------------------------------------
-- 
-- BRPMC 4-a: Combine GPA and non GPA fuel adjustment factors 
--

-- 
-- BRPMC 4-b: Aggregate county fuel adjustments to fuel type
--
DROP TABLE IF EXISTS FuelSupplyWithFuelType;
CREATE TABLE FuelSupplyWithFuelType (
       countyID INTEGER NOT NULL,
       yearID SMALLINT NOT NULL,
       monthID SMALLINT NOT NULL,
       fuelFormulationID int(11) NOT NULL,
       fuelTypeID SMALLINT NOT NULL,
	 marketShare FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

CREATE INDEX FuelSupplyWithFuelType1 ON FuelSupplyWithFuelType
(
       fuelFormulationID ASC
);

INSERT INTO FuelSupplyWithFuelType
SELECT ##context.iterLocation.countyRecordID## as countyID, yearID, may.monthID, fs.fuelFormulationID, fst.fuelTypeID, fs.marketShare
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
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

CREATE UNIQUE INDEX XPKFuelSupplyAdjustment ON FuelSupplyAdjustment
(
       yearID ASC,
       monthID ASC,
       sourceTypeID ASC,
       fuelTypeID ASC,
       modelYearID ASC,
       polProcessID ASC
);

ALTER TABLE `generalfuelratio` ADD INDEX `idx_2fuelFormulationID`(`fuelFormulationID`),
 ADD INDEX `idx_3sourceTypeID`(`sourceTypeID`), ADD INDEX `idx_4fuelTypeID`(`fuelTypeID`);


INSERT INTO FuelSupplyAdjustment (countyID, yearID, monthID, polProcessID, modelYearID,
	sourceTypeID, fuelTypeID, fuelAdjustment)
SELECT c.countyID, fsft.yearID, fsft.monthID, ppmy.polProcessID, ppmy.modelYearID, 
	rst.sourceTypeID, fsft.fuelTypeID, 
	SUM((ifnull(fuelEffectRatio,1)+GPAFract*(ifnull(fuelEffectRatioGPA,1)-ifnull(fuelEffectRatio,1)))*marketShare)
FROM County c
INNER JOIN PollutantProcessModelYear ppmy
INNER JOIN FuelSupplyWithFuelType fsft
INNER JOIN RunSpecSourceType rst
LEFT OUTER JOIN generalFuelRatio gfr on (
	gfr.fuelFormulationID = fsft.fuelFormulationID
	and gfr.polProcessID = ppmy.polProcessID
	and gfr.minModelYearID <= ppmy.modelYearID
	and gfr.maxModelYearID >= ppmy.modelYearID
	and gfr.minAgeID <= ##context.year## - ppmy.modelYearID
	and gfr.maxAgeID >= ##context.year## - ppmy.modelYearID
	and gfr.sourceTypeID = rst.sourceTypeID
)
GROUP BY c.countyID, fsft.yearID, fsft.monthID, ppmy.polProcessID, ppmy.modelYearID, 
	rst.sourceTypeID, fsft.fuelTypeID
ORDER BY modelYearID ASC;

-- 
-- BRPMC 4-c: Apply fuel adjustment to weighted emission rates
--

drop table if exists FuelAdjustedEmissionRate;
create table FuelAdjustedEmissionRate (
	yearID smallint(6) not null,
	monthID smallint(6) not null,
	hourDayID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint(6) not null,
	polProcessID int not null,
	unadjustedEmissionQuant float
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into FuelAdjustedEmissionRate (yearID, monthID, hourDayID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, unadjustedEmissionQuant)
select distinct u.yearID, u.monthID, u.hourDayID, u.sourceTypeID, u.fuelTypeID, u.modelYearID, u.polProcessID, 
coalesce((f.fuelAdjustment * u.unadjustedEmissionQuant), u.unadjustedEmissionQuant) as unadjustedEmissionQuant 
from UnadjustedEmissionResults u
inner join FuelSupplyAdjustment f using (yearID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID);

create index ixFuelAdjustedEmissionRate1 on FuelAdjustedEmissionRate (
	hourDayID asc, 
	monthID asc, 
	polProcessID asc, 
	fuelTypeID
);

create index ixFuelAdjustedEmissionRate2 on FuelAdjustedEmissionRate (
	hourDayID asc
);		

-- --------------------------------------------------------------
-- BRPMC Step 5: Apply Temperature Adjustment
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
	emissionQuant float
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';


-- Section ApplyLinearTemperatureAdjustment
insert into AdjustedEmissionResults (yearID, monthID, dayID, hourID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, emissionQuant)
select u.yearID, u.monthID, hd.dayID, hd.hourID, u.sourceTypeID, u.fuelTypeID, u.modelYearID, u.polProcessID,
coalesce(
unadjustedEmissionQuant*(1.0+
(CASE WHEN temperature <= 72.0 THEN (temperature-75.0)*(tempAdjustTermA+tempAdjustTermB*(temperature-75.0)) ELSE 0 END)), unadjustedEmissionQuant) as emissionQuant
from FuelAdjustedEmissionRate u
inner join HourDay hd using (hourDayID)
inner join ZoneMonthHour zmh on (zmh.monthID=u.monthID and zmh.hourID=hd.hourID)
left outer join TemperatureAdjustment ta on (ta.polProcessID=u.polProcessID and ta.fuelTypeID=u.fuelTypeID);
-- End Section ApplyLinearTemperatureAdjustment

-- Section ApplyTemperatureAdjustment
insert into AdjustedEmissionResults (yearID, monthID, dayID, hourID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, emissionQuant)
select u.yearID, u.monthID, hd.dayID, hd.hourID, u.sourceTypeID, u.fuelTypeID, u.modelYearID, u.polProcessID,
coalesce(
unadjustedEmissionQuant*exp((CASE WHEN temperature <= 72.0 AND modelYearID between minModelYearID and maxModelYearID  
	THEN tempAdjustTermA*(72.0-temperature) ELSE 0 END)),  unadjustedEmissionQuant) as emissionQuant
from FuelAdjustedEmissionRate u
inner join HourDay hd using (hourDayID)
inner join ZoneMonthHour zmh on (zmh.monthID=u.monthID and zmh.hourID=hd.hourID)
left outer join TemperatureAdjustment ta on (
	ta.polProcessID=u.polProcessID
	and ta.fuelTypeID=u.fuelTypeID
	and u.modelYearID between ta.minModelYearID and ta.maxModelYearID);
-- End Section ApplyTemperatureAdjustment

-- Section NoTemperatureAdjustment
insert into AdjustedEmissionResults (yearID, monthID, dayID, hourID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, emissionQuant)
select u.yearID, u.monthID, hd.dayID, hd.hourID, u.sourceTypeID, u.fuelTypeID, u.modelYearID, u.polProcessID,
unadjustedEmissionQuant as emissionQuant
from FuelAdjustedEmissionRate u
inner join HourDay hd using (hourDayID);
-- End Section NoTemperatureAdjustment

create index ixAdjustedEmissionResults1 on AdjustedEmissionResults (
	polProcessID asc
);

-- -------------------------------------------------------------------------------
-- BRPMC Step 6: Convert Results to Structure of MOVESWorkerOutput by sourceTypeID
-- -------------------------------------------------------------------------------
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

alter table FullyWeightedEmissionRate drop index ixFullyWeightedEmissionRate1;
alter table FuelAdjustedEmissionRate drop index ixFuelAdjustedEmissionRate1;
alter table FuelAdjustedEmissionRate drop index ixFuelAdjustedEmissionRate2;
alter table AdjustedEmissionResults drop index ixAdjustedEmissionResults1;	
alter table UnadjustedEmissionResults drop index ixUnadjustedEmissionResults1;

-- End Section Processing

-- Section Cleanup
drop table if exists OpModeWeightedEmissionRateTemp;
drop table if exists OpModeWeightedEmissionRate;
drop table if exists FullyWeightedEmissionRate;
drop table if exists UnadjustedEmissionResults;
drop table if exists AdjustedEmissionResults;
drop table if exists FuelAdjustedEmissionRate;
-- End Section Cleanup
