-- Version 2013-09-15
-- Author Wesley Faler

-- @algorithm
-- @owner Single-day Tank Vapor Venting Calculator
-- @notused

-- Section Create Remote Tables for Extracted Data

##create.AgeCategory##;
TRUNCATE AgeCategory;

##create.AverageTankGasoline##;
TRUNCATE AverageTankGasoline;

##create.AverageTankTemperature##;
TRUNCATE AverageTankTemperature;

##create.ColdSoakInitialHourFraction##;
TRUNCATE ColdSoakInitialHourFraction;

##create.ColdSoakTankTemperature##;
TRUNCATE ColdSoakTankTemperature;

##create.County##;
TRUNCATE County;

##create.CumTVVCoeffs##;
TRUNCATE CumTVVCoeffs;

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

##create.RunSpecDay##;
TRUNCATE RunSpecDay;

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

##create.SourceTypeModelYearGroup##;
TRUNCATE SourceTypeModelYearGroup;

##create.TankVaporGenCoeffs##;
TRUNCATE TankVaporGenCoeffs;

##create.Year##;
TRUNCATE Year;

##create.Zone##;
TRUNCATE Zone;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
-- CREATE TABLE IF NOT EXISTS EventLog (eventRowID INTEGER UNSIGNED NOT NULL AUTO_INCREMENT, PRIMARY KEY (eventRowID), eventTime DATETIME, eventName VARCHAR(120));
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'Extracting Data';

cache SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

cache SELECT AverageTankGasoline.* INTO OUTFILE '##AverageTankGasoline##'
FROM AverageTankGasoline
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = AverageTankGasoline.monthGroupID)
INNER JOIN Year ON (Year.yearID = ##context.year##)
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND AverageTankGasoline.fuelYearID = Year.fuelYearID
AND monthID = ##context.monthID##;

cache SELECT * INTO OUTFILE '##AverageTankTemperature##' FROM AverageTankTemperature
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND monthID = ##context.monthID##;

cache SELECT * INTO OUTFILE '##ColdSoakInitialHourFraction##' FROM ColdSoakInitialHourFraction
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND monthID = ##context.monthID##;

cache SELECT * INTO OUTFILE '##ColdSoakTankTemperature##' FROM ColdSoakTankTemperature
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND monthID = ##context.monthID##;

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

cache SELECT DISTINCT CumTVVCoeffs.* INTO OUTFILE '##CumTVVCoeffs##'
FROM CumTVVCoeffs, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE SourceBinDistribution.polProcessID IN (##pollutantProcessIDs##)
AND CumTVVCoeffs.polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 40
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID
AND SourceBin.regClassID = CumTVVCoeffs.regClassID;

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
AND EmissionRateByAge.opModeID IN (150, 300);

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

cache SELECT * INTO OUTFILE '##RunSpecMonth##'
FROM RunSpecMonth
WHERE monthID = ##context.monthID##;

cache SELECT * INTO OUTFILE '##RunSpecDay##'
FROM RunSpecDay;

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

cache SELECT SourceTypeModelYearGroup.* INTO OUTFILE '##SourceTypeModelYearGroup##'
FROM SourceTypeModelYearGroup,RunSpecSourceType
WHERE SourceTypeModelYearGroup.sourceTypeID = RunSpecSourceType.sourceTypeID;

cache SELECT * INTO OUTFILE '##TankVaporGenCoeffs##' FROM TankVaporGenCoeffs;

cache SELECT Year.* INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'End Extracting Data';

-- End Section Extract Data

-- Section Processing

alter table ColdSoakTankTemperature add key speed1 (hourID);
analyze table ColdSoakTankTemperature;

-- Create tables needed for processing
-- CREATE TABLE IF NOT EXISTS EventLog (eventRowID INTEGER UNSIGNED NOT NULL AUTO_INCREMENT, PRIMARY KEY (eventRowID), eventTime DATETIME, eventName VARCHAR(120));

-- 
-- TVV-1: Complete I/M adjustment fraction information (like CREC 1-a)
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-1';
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

INSERT INTO IMCoverageMergedUngrouped (
	processID,pollutantID,modelYearID,fuelTypeID,sourceTypeID,IMAdjustFract)
SELECT
 ppa.processID,
 ppa.pollutantID,
 ppmy.modelYearID,
 imf.fuelTypeID,
 imc.sourceTypeID,
 sum(IMFactor*complianceFactor*.01) AS IMAdjustFract
FROM PollutantProcessModelYear ppmy
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
-- TVV-2: Determine Hour of Peak Cold Soak Tank Temperature
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-2';
drop table if exists PeakHourOfColdSoak;

--zoneID int not null,  NOTE: since calc is at year level, there is only 1 zone
create table PeakHourOfColdSoak (
    monthID smallint(6) not null,
    peakHourID smallint(6) not null,
    primary key (monthID),
    index (peakHourID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into PeakHourOfColdSoak (monthID, peakHourID)
select monthID,
999-mod(max(round(coldSoakTankTemperature,2)*100000+(999-hourID)),1000) as peakHourID
from ColdSoakTankTemperature
group by monthID
order by null;

analyze table PeakHourOfColdSoak;

-- 
-- TVV-3: Calculate TankVaporGenerated (TVG) by Ethanol Level
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-3';
drop table if exists TankVaporGenerated;

create table TankVaporGenerated (
	hourDayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	ethanolLevelID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelYearID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	tankVaporGenerated float null,
	primary key (hourDayID, initialHourDayID, ethanolLevelID, monthID, sourceTypeID, fuelYearID, fuelTypeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- NOTE: "k" is set to 1.0 in the calculation below
insert into TankVaporGenerated (hourDayID, initialHourDayID, ethanolLevelID,
monthID, sourceTypeID, fuelYearID, fuelTypeID, tankVaporGenerated)
select ihf.hourDayID, ihf.initialHourDayID, coeffs.ethanolLevelID,
ihf.monthID, ihf.sourceTypeID, avggas.fuelYearID, avggas.fuelTypeID,
case when t1.coldSoakTankTemperature >= t2.coldSoakTankTemperature then 0.0
else
1.0*(tvgTermA*exp(tvgTermB*RVP)*(exp(tvgTermC*t2.coldSoakTankTemperature)-exp(tvgTermC*t1.coldSoakTankTemperature)))
end as tankVaporGenerated
from ColdSoakInitialHourFraction ihf
inner join HourDay hd on (hd.hourDayID = ihf.hourDayID)
inner join HourDay ihd on (ihd.hourDayID = ihf.initialHourDayID)
inner join PeakHourOfColdSoak ph on (ihf.monthID = ph.monthID)
inner join ColdSoakTankTemperature t2 on (ihf.monthID = t2.monthID and hd.hourID = t2.hourID)
inner join ColdSoakTankTemperature t1 on (ihf.monthID = t1.monthID and ihd.hourID = t1.hourID)
inner join Zone on (ihf.zoneID = Zone.zoneID)
inner join County on (Zone.countyID = County.countyID)
inner join TankVaporGenCoeffs coeffs on (County.altitude = coeffs.altitude)
inner join MonthOfAnyYear m on (ihf.monthID = m.monthID)
inner join AverageTankGasoline avggas on (m.monthGroupID = avggas.monthGroupID)
where ihf.hourDayID <> ihf.initialHourDayID
and hd.hourID <= ph.peakHourID
and ihf.coldSoakInitialHourFraction > 0;

analyze table TankVaporGenerated;

-- 
-- TVV-4: Calculate Ethanol-weighted TVG
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-4';
drop table if exists EthanolWeightedTVG;

create table EthanolWeightedTVG (
	hourDayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelYearID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	ethanolWeightedTVG float null,
	primary key (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelYearID, fuelTypeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into EthanolWeightedTVG (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelYearID, fuelTypeID, ethanolWeightedTVG)
select t0.hourDayID, t0.initialHourDayID, t0.monthID, t0.sourceTypeiD, t0.fuelYearID, t0.fuelTypeID,
(t10.tankVaporGenerated*(least(10.0,ETOHVolume)/10.0)
+t0.tankVaporGenerated*(1.0-least(10.0,ETOHVolume)/10.0)) as ethanolWeightedTVG
from TankVaporGenerated t0
inner join TankVaporGenerated t10 on (t0.hourDayID=t10.hourDayID and t0.initialHourDayID=t10.initialHourDayID
and t0.monthID=t10.monthID and t0.sourceTypeID=t10.sourceTypeID and t0.fuelYearID=t10.fuelYearID
and t0.fuelTypeID=t10.fuelTypeID)
inner join MonthOfAnyYear m on (t10.monthID = m.monthID)
inner join AverageTankGasoline avggas on (m.monthGroupID = avggas.monthGroupID and t10.fuelYearID = avggas.fuelYearID
and t10.fuelTypeID=avggas.fuelTypeID)
where t0.ethanolLevelID = 0
and t10.ethanolLevelID = 10;

analyze table EthanolWeightedTVG;

-- 
-- TVV-5: Calculate Cummulative Tank Vapor Vented (TVV)
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-5';
drop table if exists CummulativeTankVaporVented;

create table CummulativeTankVaporVented (
	regClassID smallint(6) not null,
	ageID smallint(6) not null,
	polProcessID int not null,
	dayID smallint(6) not null,
	hourID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	tankVaporVented float null,
	tankVaporVentedIM float null,
	hourDayID smallint(6) not null,
	priorHourID smallint(6) not null,
	primary key (regClassID, ageID, polProcessID, dayID, hourID, initialHourDayID, monthID, sourceTypeID, fuelTypeID),
	index (priorHourID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into CummulativeTankVaporVented (regClassID, ageID, polProcessID, dayID, hourID, initialHourDayID, 
monthID, sourceTypeID, fuelTypeID, tankVaporVented, tankVaporVentedIM,
hourDayID, priorHourID)
select coeffs.regClassID, acat.ageID, coeffs.polProcessID, hd.dayID, hd.hourID, ew.initialHourDayID,
ew.monthID, ew.sourceTypeID, ew.fuelTypeID,
greatest(tvvTermA+ethanolWeightedTVG*(tvvTermB+tvvTermC*ethanolWeightedTVG),0.0) as tankVaporVented,
greatest(tvvTermAIM+ethanolWeightedTVG*(tvvTermBIM+tvvTermCIM*ethanolWeightedTVG),0.0) as tankVaporVentedIM,
ew.hourDayID,
mod(hd.hourID-1-1+24,24)+1
from CumTVVCoeffs coeffs
inner join AgeCategory acat on (coeffs.ageGroupID = acat.ageGroupID)
inner join PollutantProcessModelYear ppmy 
on (coeffs.polProcessID=ppmy.polProcessID and coeffs.modelYearGroupID = ppmy.modelYearGroupID)
inner join EthanolWeightedTVG ew
inner join HourDay hd on (hd.hourDayID = ew.hourDayID)
inner join Year y on (y.fuelYearID = ew.fuelYearID)
where coeffs.polProcessID in (##pollutantProcessIDs##)
and acat.ageID = y.yearID - ppmy.modelYearID;

analyze table CummulativeTankVaporVented;

-- 
-- TVV-6: Calculate Unweighted Hourly TVV Emission by Regulatory Class and Vehicle Age
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-6';
drop table if exists UnweightedHourlyTVV;

create table UnweightedHourlyTVV (
	zoneID int(11) not null default ##context.iterLocation.zoneRecordID##,
	regClassID smallint(6) not null,
	ageID smallint(6) not null,
	polProcessID int not null,
	hourDayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	unweightedHourlyTVV float null,
	unweightedHourlyTVVIM float null,
	index (sourceTypeID, zoneID, monthID, hourDayID, initialHourDayID, fuelTypeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into UnweightedHourlyTVV (regClassID, ageID, polProcessID, hourDayID, initialHourDayID, 
monthID, sourceTypeID, fuelTypeID, unweightedHourlyTVV, unweightedHourlyTVVIM)
select ctv1.regClassID, ctv1.ageID, ctv1.polProcessID, ctv1.hourDayID, ctv1.initialHourDayID, 
ctv1.monthID, ctv1.sourceTypeID, ctv1.fuelTypeID, 
greatest(ctv1.tankVaporVented-coalesce(ctv2.tankVaporVented,0.0),0.0) as unweightedHourlyTVV,
greatest(ctv1.tankVaporVentedIM-coalesce(ctv2.tankVaporVentedIM,0.0),0.0) as unweightedHourlyTVVIM 
from CummulativeTankVaporVented ctv1 left join CummulativeTankVaporVented ctv2 
on (ctv1.regClassID = ctv2.regClassID and ctv1.ageID = ctv2.ageID
and ctv1.polProcessID = ctv2.polProcessID and ctv1.initialHourDayID = ctv2.initialHourDayID 
and ctv1.monthID = ctv2.monthID and ctv1.sourceTypeID = ctv2.sourceTypeID
and ctv1.fuelTypeID = ctv2.fuelTypeID
and ctv1.priorHourID = ctv2.hourID
and ctv1.dayID = ctv2.dayID);

analyze table UnweightedHourlyTVV;

-- 
-- TVV-7: Calculate Weighted Hourly TVV Across Initial/Current pair
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-7';
drop table if exists HourlyTVV;

create table HourlyTVV (
	regClassID smallint(6) not null,
	ageID smallint(6) not null,
	polProcessID int not null,
	hourDayID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	hourlyTVV float null,
	hourlyTVVIM float null,
	primary key (regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Handle hourDayIDs <= the peak hour
drop table if exists HourlyTVVTemp;

create table HourlyTVVTemp (
	regClassID smallint(6) not null,
	ageID smallint(6) not null,
	polProcessID int not null,
	hourDayID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	hourlyTVV float null,
	hourlyTVVIM float null,
	index (regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into HourlyTVVTemp (regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID,
hourlyTVV, hourlyTVVIM)
select uhtvv.regClassID, uhtvv.ageID, uhtvv.polProcessID, uhtvv.hourDayID, uhtvv.monthID, uhtvv.sourceTypeID, uhtvv.fuelTypeID,
(unweightedHourlyTVV*coldSoakInitialHourFraction) as hourlyTVV,
(unweightedHourlyTVVIM*coldSoakInitialHourFraction) as hourlyTVVIM
from UnweightedHourlyTVV uhtvv
inner join ColdSoakInitialHourFraction ihf on (uhtvv.sourceTypeID = ihf.sourceTypeID
and uhtvv.zoneID = ihf.zoneID and uhtvv.monthID = ihf.monthID 
and uhtvv.hourDayID = ihf.hourDayID and uhtvv.initialHourDayID = ihf.initialHourDayID);

insert into HourlyTVV (regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID, hourlyTVV, hourlyTVVIM)
select regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID,
sum(hourlyTVV) as hourlyTVV,
sum(hourlyTVVIM) as hourlyTVVIM
from HourlyTVVTemp
group by regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID
order by null;

drop table if exists HourlyTVVTemp;

-- Handle hourDayIDs > the peak hour
drop table if exists CopyOfHourlyTVV;
create table CopyOfHourlyTVV (
	regClassID smallint(6) not null,
	ageID smallint(6) not null,
	polProcessID int not null,
	dayID smallint(6) not null,
	hourID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	hourlyTVV float null,
	hourlyTVVIM float null,
	primary key (regClassID, ageID, polProcessID, dayID, hourID, monthID, sourceTypeID, fuelTypeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
insert into CopyOfHourlyTVV (regClassID, ageID, polProcessID, dayID, hourID, 
monthID, sourceTypeID, fuelTypeID, hourlyTVV, hourlyTVVIM)
select regClassID, ageID, polProcessID, dayID, hourID,
monthID, sourceTypeID, fuelTypeID, hourlyTVV, hourlyTVVIM 
from HourlyTVV
inner join HourDay on (HourDay.hourDayID=HourlyTVV.hourDayID);

insert into HourlyTVV (regClassID, ageID, polProcessID, hourDayID, monthID,
sourceTypeID, fuelTypeID, hourlyTVV, hourlyTVVIM)
select htvv.regClassID, htvv.ageID, htvv.polProcessID, hd.hourDayID, htvv.monthID, htvv.sourceTypeID, htvv.fuelTypeID,
hourlyTVV * case 
	when (hd.hourID-ph.peakHourID) >= 4 then 0.0005
	when (hd.hourID-ph.peakHourID) >= 3 then 0.0040
	when (hd.hourID-ph.peakHourID) >= 2 then 0.0100
	when (hd.hourID-ph.peakHourID) >= 1 then 0.0200
	end as hourlyTVV,
hourlyTVVIM * case 
	when (hd.hourID-ph.peakHourID) >= 4 then 0.0005
	when (hd.hourID-ph.peakHourID) >= 3 then 0.0040
	when (hd.hourID-ph.peakHourID) >= 2 then 0.0100
	when (hd.hourID-ph.peakHourID) >= 1 then 0.0200
	end as hourlyTVVIM
from CopyOfHourlyTVV htvv
inner join PeakHourOfColdSoak ph on (htvv.monthID = ph.monthID and htvv.hourID = ph.peakHourID)
inner join HourDay hd on (
	hd.hourID > ph.peakHourID 
	and hd.hourID < ph.peakHourID + 5 
	and hd.dayID = htvv.dayID
);

analyze table HourlyTVV;

-- TVV-8: Calculate I/M-Adjusted MeanBaseRates
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-8';
drop table if exists WeightedMeanBaseRate;

create table WeightedMeanBaseRate (
	polProcessID int not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	monthID smallint(6) not null,
	hourDayID smallint(6) not null,
	modelYearID smallint(6) not null,
	opModeID smallint(6) not null,
	weightedMeanBaseRate float not null,
	weightedMeanBaseRateIM float not null,
	primary key (polProcessID, sourceTypeID, fuelTypeID, monthID, hourDayID, modelYearID, opModeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- For cold soak mode (opModeID=151)
insert into WeightedMeanBaseRate (polProcessID, sourceTypeID, fuelTypeID, monthID, hourDayID, 
	modelYearID, opModeID, weightedMeanBaseRate, weightedMeanBaseRateIM)
select htvv.polProcessID, htvv.sourceTypeID, sb.fuelTypeID, htvv.monthID, htvv.hourDayID, 
	stmy.modelYearID, 151 as opModeID,
	sum(sourceBinActivityFraction*hourlyTVV) as weightedMeanBaseRate,
	sum(sourceBinActivityFraction*hourlyTVVIM) as weightedMeanBaseRateIM
from HourlyTVV htvv
inner join SourceTypeModelYear stmy on (stmy.modelYearID=##context.year##-htvv.ageID
	and stmy.sourceTypeID=htvv.sourceTypeID)
inner join SourceBinDistribution sbd on (sbd.sourceTypeModelYearID=stmy.sourceTypeModelYearID
	and sbd.polProcessID=htvv.polProcessID)
inner join SourceBin sb on (sb.sourceBinID=sbd.sourceBinID and sb.fuelTypeID=htvv.fuelTypeID
	and sb.regClassID=htvv.regClassID)
inner join FuelType on (FuelType.fuelTypeID = sb.fuelTypeID and subjectToEvapCalculations = 'Y')
inner join PollutantProcessModelYear ppmy on (ppmy.polProcessID=sbd.polProcessID
	and ppmy.modelYearID=stmy.modelYearID and ppmy.modelYearGroupID=sb.modelYearGroupID)
group by htvv.polProcessID, htvv.sourceTypeID, sb.fuelTypeID, htvv.monthID, htvv.hourDayID, 
	stmy.modelYearID
order by null;

-- For operating and hot soak modes (opModeIDs 300 and 150)
insert into WeightedMeanBaseRate (polProcessID, sourceTypeID, fuelTypeID, monthID, hourDayID, 
	modelYearID, opModeID, weightedMeanBaseRate, weightedMeanBaseRateIM)
select er.polProcessID, stmy.sourceTypeID, sb.fuelTypeID, rsm.monthID, rshd.hourDayID, 
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
and opModeID in (150, 300)
group by er.polProcessID, stmy.sourceTypeID, sb.fuelTypeID, rsm.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID
order by null;

-- analyze table WeightedMeanBaseRate;

alter table MOVESWorkerOutput add emissionQuantIM float null;

alter table WeightedMeanBaseRate add key speed1 (sourceTypeID, hourDayID, polProcessID, opModeID);
analyze table WeightedMeanBaseRate;

alter table SourceHours add key speed1 (hourDayID, monthID, sourceTypeID, ageID);
analyze table SourceHours;

-- 
-- TVV-9: Calculate MOVESWorkerOutput by Source Type
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-9 without SCC';
insert into MOVESWorkerOutput (yearID, monthID, dayID, hourID, stateID, countyID,
zoneID, linkID, pollutantID, processID, sourceTypeID, fuelTypeID, modelYearID,
roadTypeID, SCC, emissionQuant, emissionQuantIM)
select ##context.year## as yearID, w.monthID, hd.dayID, hd.hourID,
##context.iterLocation.stateRecordID## as stateID,
##context.iterLocation.countyRecordID## as countyID,
##context.iterLocation.zoneRecordID## as zoneID,
##context.iterLocation.linkRecordID## as linkID,
ppa.pollutantID, ppa.processID, w.sourceTypeID, w.fuelTypeID, w.modelYearID,
##context.iterLocation.roadTypeRecordID##, null as SCC,
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
inner join HourDay hd on (hd.hourDayID=omd.hourDayID);

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
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'Section Cleanup';

drop table if exists PeakHourOfColdSoak;
drop table if exists TankVaporGenerated;
drop table if exists EthanolWeightedTVG;
drop table if exists CummulativeTankVaporVented;
drop table if exists UnWeightedHourlyTVV;
drop table if exists HourlyTVV;
drop table if exists WeightedMeanBaseRate;
drop table if exists IMCoverageMergedUngrouped;
drop table if exists CopyOfHourlyTVV;
-- End Section Cleanup
