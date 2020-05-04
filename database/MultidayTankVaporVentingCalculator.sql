-- Author Wesley Faler
-- Author David Hawkins
-- Version 2014-06-24

-- @algorithm
-- @owner Tank Vapor Venting Calculator
-- @calculator

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

##create.evapTemperatureAdjustment##;
TRUNCATE evapTemperatureAdjustment;

##create.evapRVPTemperatureAdjustment##;
TRUNCATE evapRVPTemperatureAdjustment;

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

##create.RunSpecDay##;
TRUNCATE RunSpecDay;

##create.RunSpecHourDay##;
TRUNCATE RunSpecHourDay;

##create.RunSpecMonth##;
TRUNCATE RunSpecMonth;

##create.RunSpecSourceType##;
TRUNCATE RunSpecSourceType;

##create.sampleVehicleSoaking##;
TRUNCATE sampleVehicleSoaking;

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

create table if not exists stmyTVVCoeffs (
  sourceTypeID smallint not null,
  modelYearID smallint not null,
  fuelTypeID smallint not null,
  polProcessID int NOT NULL DEFAULT '0',
  backPurgeFactor double DEFAULT NULL,
  averageCanisterCapacity double DEFAULT NULL,
  leakFraction double DEFAULT NULL,
  leakFractionIM double DEFAULT NULL,
  tankSize double DEFAULT NULL,
  tankFillFraction double DEFAULT NULL,
  PRIMARY KEY (sourceTypeID,modelYearID,fuelTypeID,polProcessID)
);
truncate stmyTVVCoeffs;

create table if not exists stmyTVVEquations (
  sourceTypeID smallint not null,
  modelYearID smallint not null,
  fuelTypeID smallint not null,
  polProcessID int NOT NULL DEFAULT '0',
  regClassID smallint not null,
  backPurgeFactor double DEFAULT NULL,
  averageCanisterCapacity double DEFAULT NULL,
  regClassFractionOfSourceTypeModelYearFuel double not null,
  tvvEquation varchar(100) NOT NULL DEFAULT '',
  leakEquation varchar(100) NOT NULL DEFAULT '',
  leakFraction double DEFAULT NULL,
  leakFractionIM double DEFAULT NULL,
  tankSize double DEFAULT NULL,
  tankFillFraction double DEFAULT NULL,
  PRIMARY KEY (sourceTypeID,modelYearID,fuelTypeID,polProcessID,regClassID,tvvEquation,leakEquation)
);
truncate stmyTVVEquations;

##create.TankVaporGenCoeffs##;
TRUNCATE TankVaporGenCoeffs;

##create.Year##;
TRUNCATE Year;

##create.Zone##;
TRUNCATE Zone;

##create.ZoneMonthHour##;
TRUNCATE ZoneMonthHour;

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
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
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
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##)
AND EmissionRateByAge.opModeID IN (150, 300);

cache select * into outfile '##evapTemperatureAdjustment##'
from evapTemperatureAdjustment
where processID=12;

cache select * into outfile '##evapRVPTemperatureAdjustment##'
from evapRVPTemperatureAdjustment
where processID=12
and fuelTypeID in (1,5);

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
AND modelYearID >= ##context.year## - 30
AND polProcessID IN (##pollutantProcessIDs##);

cache SELECT * INTO OUTFILE '##PollutantProcessMappedModelYear##'
FROM PollutantProcessMappedModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
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

cache SELECT * INTO OUTFILE '##TankVaporGenCoeffs##' FROM TankVaporGenCoeffs;

cache SELECT Year.* INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'End Extracting Data';

-- Section NewTVVYear
drop table if exists regClassFractionOfSTMY##context.year##;

create table regClassFractionOfSTMY##context.year## (
	sourceTypeID smallint not null,
	modelYearID smallint not null,
	fuelTypeID smallint not null,
	regClassID smallint not null,
	regClassFractionOfSourceTypeModelYearFuel double not null,
	primary key (sourceTypeID, modelYearID, fuelTypeID, regClassID)
);

insert into regClassFractionOfSTMY##context.year## (sourceTypeID, modelYearID, fuelTypeID, regClassID, regClassFractionOfSourceTypeModelYearFuel)
select sourceTypeID, modelYearID, fuelTypeID, regClassID, sum(stmyFraction) as regClassFractionOfSourceTypeModelYearFuel
from SampleVehiclePopulation svp
where sourceTypeID in (##macro.csv.all.sourceTypeID##)
and modelYearID in (##macro.csv.all.modelYearID##)
group by sourceTypeModelYearID, fuelTypeID, regClassID
having sum(stmyFraction) > 0;

drop table if exists stmyTVVEquations##context.year##;

create table stmyTVVEquations##context.year## (
  sourceTypeID smallint not null,
  modelYearID smallint not null,
  fuelTypeID smallint not null,
  polProcessID int NOT NULL DEFAULT '0',
  regClassID smallint not null,
  backPurgeFactor double DEFAULT NULL,
  averageCanisterCapacity double DEFAULT NULL,
  regClassFractionOfSourceTypeModelYearFuel double not null,
  tvvEquation varchar(100) NOT NULL DEFAULT '',
  leakEquation varchar(100) NOT NULL DEFAULT '',
  leakFraction double DEFAULT NULL,
  leakFractionIM double DEFAULT NULL,
  tankSize double DEFAULT NULL,
  tankFillFraction double DEFAULT NULL,
  PRIMARY KEY (sourceTypeID,modelYearID,fuelTypeID,polProcessID,regClassID,tvvEquation,leakEquation)
);

insert into stmyTVVEquations##context.year## (sourceTypeID, modelYearID, fuelTypeID, polProcessID, regClassID,
	backPurgeFactor, averageCanisterCapacity, regClassFractionOfSourceTypeModelYearFuel,
	tvvEquation, leakEquation, leakFraction, leakFractionIM, tankSize, tankFillFraction)
select rf.sourceTypeID, rf.modelYearID, rf.fuelTypeID,
	c.polProcessID, c.regClassID,
	sum(backPurgeFactor*regClassFractionOfSourceTypeModelYearFuel) as backPurgeFactor,
	sum(averageCanisterCapacity*regClassFractionOfSourceTypeModelYearFuel) as averageCanisterCapacity,
	sum(regClassFractionOfSourceTypeModelYearFuel) as regClassFractionOfSourceTypeModelYearFuel,
	c.tvvEquation,
	c.leakEquation,
	sum(leakFraction*regClassFractionOfSourceTypeModelYearFuel) as leakFraction,
	sum(leakFractionIM*regClassFractionOfSourceTypeModelYearFuel) as leakFractionIM,
	sum(tankSize*regClassFractionOfSourceTypeModelYearFuel) as tankSize,
	sum(tankFillFraction*regClassFractionOfSourceTypeModelYearFuel) as tankFillFraction
from cumTVVCoeffs c
inner join pollutantProcessMappedModelYear ppmy on (
	ppmy.polProcessID = c.polProcessID
	and ppmy.modelYearGroupID = c.modelYearGroupID)
inner join ageCategory a on (
	a.ageGroupID = c.ageGroupID)
inner join regClassFractionOfSTMY##context.year## rf on (
	rf.modelYearID = ppmy.modelYearID
	and rf.regClassID = c.regClassID)
where ppmy.modelYearID = ##context.year## - a.ageID
and c.polProcessID in (##pollutantProcessIDs##)
group by rf.sourceTypeID, rf.modelYearID, rf.fuelTypeID,
	c.polProcessID, c.regClassID,
	c.tvvEquation,
	c.leakEquation;

drop table if exists stmyTVVCoeffs##context.year##;

create table stmyTVVCoeffs##context.year## (
  sourceTypeID smallint not null,
  modelYearID smallint not null,
  fuelTypeID smallint not null,
  polProcessID int NOT NULL DEFAULT '0',
  backPurgeFactor double DEFAULT NULL,
  averageCanisterCapacity double DEFAULT NULL,
  leakFraction double DEFAULT NULL,
  leakFractionIM double DEFAULT NULL,
  tankSize double DEFAULT NULL,
  tankFillFraction double DEFAULT NULL,
  PRIMARY KEY (sourceTypeID,modelYearID,fuelTypeID,polProcessID)
);

insert into stmyTVVCoeffs##context.year## (sourceTypeID, modelYearID, fuelTypeID, polProcessID,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM, tankSize, tankFillFraction)
select sourceTypeID, modelYearID, fuelTypeID, polProcessID,
	sum(backPurgeFactor),
	sum(averageCanisterCapacity),
	sum(leakFraction),
	sum(leakFractionIM),
	sum(tankSize),
	sum(tankFillFraction)
from stmyTVVEquations##context.year##
group by sourceTypeiD, modelYearID, fuelTypeID, polProcessID;
-- End Section NewTVVYear

cache(yearID=##context.year##) select * into outfile '##stmyTVVEquations##' from stmyTVVEquations##context.year##;
cache(yearID=##context.year##) select * into outfile '##stmyTVVCoeffs##' from stmyTVVCoeffs##context.year##;

-- Section FirstBundle

-- Section FillSampleVehicleSoaking

-- Fill sampleVehicleSoaking
truncate sampleVehicleSoaking;

-- Get total vehicle counts
drop table if exists sampleVehicleCount;

create table sampleVehicleCount (
	dayID smallint not null,
	sourceTypeID smallint not null,
	totalVehicles int not null,
	primary key (dayID, sourceTypeID),
	unique key (sourceTypeID, dayID)
);

-- @algorithm totalVehicles = count(dayID, sourceTypeID) from sampleVehicleDay.
-- @condition First bundle only.
insert into sampleVehicleCount (dayID, sourceTypeID, totalVehicles)
select svd.dayID, sourceTypeID, count(*)
from sampleVehicleDay svd
group by svd.dayID, sourceTypeID
order by null;

-- Count vehicles that never started
drop table if exists sampleVehicleNeverStarted;

create table sampleVehicleNeverStarted (
	dayID smallint not null,
	sourceTypeID smallint not null,
	vehiclesNeverStarted int not null,
	fractionNeverStarted double,
	primary key (dayID, sourceTypeID),
	unique key (sourceTypeID, dayID)
);

-- @algorithm vehiclesNeverStarted = count(dayID, sourceTypeID) from sampleVehicleDay without any corresponding entry in sampleVehicleTrip.
-- @condition First bundle only.
insert into sampleVehicleNeverStarted (dayID, sourceTypeID, vehiclesNeverStarted)
select svd.dayID, sourceTypeID, count(*)
from sampleVehicleDay svd
left outer join sampleVehicleTrip svt using (vehID, dayID)
where svt.vehID is null and svt.dayID is null
group by svd.dayID, sourceTypeID
order by null;

-- @algorithm fractionNeverStarted[dayID,sourceTypeID] = vehiclesNeverStarted/totalVehicles.
-- @condition First bundle only.
update sampleVehicleNeverStarted, sampleVehicleCount
	set fractionNeverStarted=case when totalVehicles <= 0 then 0 else vehiclesNeverStarted*1.0/totalVehicles end
where sampleVehicleNeverStarted.dayID = sampleVehicleCount.dayID
and sampleVehicleNeverStarted.sourceTypeID = sampleVehicleCount.sourceTypeID;

-- Count vehicles by when they started
drop table if exists sampleVehicleFirstStart;

create table sampleVehicleFirstStart (
	vehID int not null,
	dayID smallint not null,
	sourceTypeID smallint not null,
	hourID smallint not null,
	primary key (dayID, sourceTypeID, hourID, vehID),
	unique key (sourceTypeID, dayID, hourID, vehID)
);

-- @algorithm hour of first start[vehID,dayID,sourceTypeID] = min(SampleVehicleTrip.hourID).
-- @condition First bundle only.
insert into sampleVehicleFirstStart (vehID, dayID, sourceTypeID, hourID)
select svd.vehID, svd.dayID, svd.sourceTypeID, min(svt.hourID)
from sampleVehicleDay svd
inner join sampleVehicleTrip svt using (vehID, dayID)
where svt.keyontime is not null
group by svd.vehID, svd.dayID
order by null;

drop table if exists sampleVehicleSoakTemp;

create table sampleVehicleSoakTemp (
	dayID smallint not null,
	sourceTypeID smallint not null,
	hourID smallint not null,
	vehiclesFirstStarted int not null,
	primary key (dayID, sourceTypeID, hourID),
	unique key (sourceTypeID, dayID, hourID)
);

-- @algorithm In each day and hour, count the number of vehicles that start for the first time.
-- vehiclesFirstStarted[dayID,sourceTypeID,hourID] = count(dayID,sourceTypeID,hourID) by hour of first start.
-- @condition First bundle only.
insert into sampleVehicleSoakTemp (dayID, sourceTypeID, hourID, vehiclesFirstStarted)
select dayID, sourceTypeID, hourID, count(*) as vehiclesFirstStarted
from sampleVehicleFirstStart
group by dayID, sourceTypeID, hourID
order by null;

drop table if exists sampleVehicleSoakTemp2;

create table sampleVehicleSoakTemp2 (
	dayID smallint not null,
	sourceTypeID smallint not null,
	hourID smallint not null,
	totalVehiclesStarted int not null,
	primary key (dayID, sourceTypeID, hourID),
	unique key (sourceTypeID, dayID, hourID)
);

-- @algorithm In each day and hour, sum the number of vehicles that have started up to then.
-- totalVehiclesStarted[dayID,sourceTypeID,hourID]=sum(vehiclesFirstStarted) for all hours up to hourID.
-- @condition First bundle only.
insert into sampleVehicleSoakTemp2 (dayID, sourceTypeID, hourID, totalVehiclesStarted)
select s.dayID, s.sourceTypeID, h.hourID, sum(vehiclesFirstStarted) as totalVehiclesStarted
from sampleVehicleSoakTemp s, hourOfAnyDay h
where s.hourID <= h.hourID
group by s.dayID, s.sourceTypeID, h.hourID
order by null;

truncate sampleVehicleSoaking;

-- @algorithm Obtain the fraction of vehicles soaking, awaiting their first start, for each day and hour.
-- soakFraction[soakDayID=0,sourceTypeID,dayID,hourID]=(totalVehicles-totalVehiclesStarted)/totalVehicles.
-- @condition First bundle only.
insert into sampleVehicleSoaking (soakDayID, sourceTypeID, dayID, hourID, soakFraction)
select 0, st.sourceTypeID, st.dayID, st.hourID, 
	case when totalVehicles <= 0 then 0
	else (totalVehicles - totalVehiclesStarted)*1.0/totalVehicles end as soakFraction
from sampleVehicleSoakTemp2 st
inner join sampleVehicleCount c using (dayID, sourceTypeID);

-- @algorithm Assume a default soakFraction of 1 for any day and hour that has no vehicles started previously.
-- @condition First bundle only.
insert ignore into sampleVehicleSoaking (soakDayID, sourceTypeID, dayID, hourID, soakFraction)
select distinct 0, s.sourceTypeID, s.dayID, h.hourID, 1.0 as soakFraction
from sampleVehicleSoakTemp s, hourOfAnyDay h;

drop table if exists sampleVehicleSoakingF;

create table if not exists sampleVehicleSoakingF (
	soakDayID smallint not null,
	sourceTypeID smallint not null,
	dayID smallint not null,
	F double,
	GammaFn1 double,
	GammaFn double,
	NextF double,
	P1 double,
	P24 double,
	primary key (soakDayID, sourceTypeID, dayID)
);

-- SampleVehicleSoaking Day 1

-- @algorithm dayF[soakDayID=1,sourceTypeID,dayID]=soakFraction[soakDayID=0,sourceTypeID,dayID,hourID=24]
-- @condition First bundle only.
insert ignore into sampleVehicleSoakingDay (soakDayID, sourceTypeID, dayID, F)
select 1, sourceTypeID, dayID, soakFraction as F
from sampleVehicleSoaking
where soakDayID=0
and hourID=24;

-- @algorithm soakF[soakDayID=1,sourceTypeID,dayID]=dayF[soakDayID=1,sourceTypeID,dayID].
-- GammaFn1[soakDayID=1,sourceTypeID,dayID]=dayF[soakDayID=1,sourceTypeID,dayID].
-- GammaFn[soakDayID=1,sourceTypeID,dayID]=dayF[soakDayID=1,sourceTypeID,dayID].
-- @condition First bundle only.
insert into sampleVehicleSoakingF (soakDayID, sourceTypeID, dayID, F, GammaFn1, GammaFn)
select soakDayID, sourceTypeID, dayID, F, F as GammaFn1, F as GammaFn
from sampleVehicleSoakingDay
where soakDayID=1;

-- SampleVehicleSoaking Day 2

-- @algorithm dayF[soakDayID=2,sourceTypeID,dayID]=greatest(dayF[soakDayID=1,sourceTypeID,dayID], basisF[soakDayID=2,dayID]).
-- @condition First bundle only.
insert ignore into sampleVehicleSoakingDay (soakDayID, sourceTypeID, dayID, F)
select 2, d.sourceTypeID, d.dayID, if(b.F<d.F,d.F,b.F)
from sampleVehicleSoakingDay d
inner join sampleVehicleSoakingDayBasis b on (
	b.soakDayID = d.soakDayID+1
	and b.dayID = d.dayID)
where d.soakDayID=2-1;

-- @algorithm soakF[soakDayID=2,sourceTypeID,dayID]=dayF[soakDayID=2,sourceTypeID,dayID].
-- GammaFn1[soakDayID=2,sourceTypeID,dayID]=GammaFn[soakDayID=1,sourceTypeID,dayID].
-- GammaFn[soakDayID=2,sourceTypeID,dayID]=dayF[soakDayID=2,sourceTypeID,dayID] * GammaFn[soakDayID=1,sourceTypeID,dayID].
-- @condition First bundle only.
insert into sampleVehicleSoakingF (soakDayID, sourceTypeID, dayID, F, GammaFn1, GammaFn)
select sd.soakDayID, sd.sourceTypeID, sd.dayID, sd.F, sf.GammaFn as GammaFn1, sd.F*sf.GammaFn as GammaFn
from sampleVehicleSoakingDay sd
inner join sampleVehicleSoakingF sf on (
	sf.soakDayID=sd.soakDayID-1
	and sf.sourceTypeID=sd.sourceTypeID
	and sf.dayID=sd.dayID)
where sd.soakDayID=2;

-- SampleVehicleSoaking Day 3

-- @algorithm dayF[soakDayID=3,sourceTypeID,dayID]=greatest(dayF[soakDayID=2,sourceTypeID,dayID], basisF[soakDayID=3,dayID]).
-- @condition First bundle only.
insert ignore into sampleVehicleSoakingDay (soakDayID, sourceTypeID, dayID, F)
select 3, d.sourceTypeID, d.dayID, if(b.F<d.F,d.F,b.F)
from sampleVehicleSoakingDay d
inner join sampleVehicleSoakingDayBasis b on (
	b.soakDayID = d.soakDayID+1
	and b.dayID = d.dayID)
where d.soakDayID=3-1;

-- @algorithm soakF[soakDayID=3,sourceTypeID,dayID]=dayF[soakDayID=3,sourceTypeID,dayID].
-- GammaFn1[soakDayID=3,sourceTypeID,dayID]=GammaFn[soakDayID=2,sourceTypeID,dayID].
-- GammaFn[soakDayID=3,sourceTypeID,dayID]=dayF[soakDayID=3,sourceTypeID,dayID] * GammaFn[soakDayID=2,sourceTypeID,dayID].
-- @condition First bundle only.
insert into sampleVehicleSoakingF (soakDayID, sourceTypeID, dayID, F, GammaFn1, GammaFn)
select sd.soakDayID, sd.sourceTypeID, sd.dayID, sd.F, sf.GammaFn as GammaFn1, sd.F*sf.GammaFn as GammaFn
from sampleVehicleSoakingDay sd
inner join sampleVehicleSoakingF sf on (
	sf.soakDayID=sd.soakDayID-1
	and sf.sourceTypeID=sd.sourceTypeID
	and sf.dayID=sd.dayID)
where sd.soakDayID=3;

-- SampleVehicleSoaking Day 4

-- @algorithm dayF[soakDayID=4,sourceTypeID,dayID]=greatest(dayF[soakDayID=3,sourceTypeID,dayID], basisF[soakDayID=4,dayID]).
-- @condition First bundle only.
insert ignore into sampleVehicleSoakingDay (soakDayID, sourceTypeID, dayID, F)
select 4, d.sourceTypeID, d.dayID, if(b.F<d.F,d.F,b.F)
from sampleVehicleSoakingDay d
inner join sampleVehicleSoakingDayBasis b on (
	b.soakDayID = d.soakDayID+1
	and b.dayID = d.dayID)
where d.soakDayID=4-1;

-- @algorithm soakF[soakDayID=4,sourceTypeID,dayID]=dayF[soakDayID=4,sourceTypeID,dayID].
-- GammaFn1[soakDayID=4,sourceTypeID,dayID]=GammaFn[soakDayID=3,sourceTypeID,dayID].
-- GammaFn[soakDayID=4,sourceTypeID,dayID]=dayF[soakDayID=4,sourceTypeID,dayID] * GammaFn[soakDayID=3,sourceTypeID,dayID].
-- @condition First bundle only.
insert into sampleVehicleSoakingF (soakDayID, sourceTypeID, dayID, F, GammaFn1, GammaFn)
select sd.soakDayID, sd.sourceTypeID, sd.dayID, sd.F, sf.GammaFn as GammaFn1, sd.F*sf.GammaFn as GammaFn
from sampleVehicleSoakingDay sd
inner join sampleVehicleSoakingF sf on (
	sf.soakDayID=sd.soakDayID-1
	and sf.sourceTypeID=sd.sourceTypeID
	and sf.dayID=sd.dayID)
where sd.soakDayID=4;

-- SampleVehicleSoaking Day 5

-- @algorithm dayF[soakDayID=5,sourceTypeID,dayID]=greatest(dayF[soakDayID=4,sourceTypeID,dayID], basisF[soakDayID=5,dayID]).
-- @condition First bundle only.
insert ignore into sampleVehicleSoakingDay (soakDayID, sourceTypeID, dayID, F)
select 5, d.sourceTypeID, d.dayID, if(b.F<d.F,d.F,b.F)
from sampleVehicleSoakingDay d
inner join sampleVehicleSoakingDayBasis b on (
	b.soakDayID = d.soakDayID+1
	and b.dayID = d.dayID)
where d.soakDayID=5-1;

-- @algorithm soakF[soakDayID=5,sourceTypeID,dayID]=dayF[soakDayID=5,sourceTypeID,dayID].
-- GammaFn1[soakDayID=5,sourceTypeID,dayID]=GammaFn[soakDayID=4,sourceTypeID,dayID].
-- GammaFn[soakDayID=5,sourceTypeID,dayID]=dayF[soakDayID=5,sourceTypeID,dayID] * GammaFn[soakDayID=4,sourceTypeID,dayID].
-- @condition First bundle only.
insert into sampleVehicleSoakingF (soakDayID, sourceTypeID, dayID, F, GammaFn1, GammaFn)
select sd.soakDayID, sd.sourceTypeID, sd.dayID, sd.F, sf.GammaFn as GammaFn1, sd.F*sf.GammaFn as GammaFn
from sampleVehicleSoakingDay sd
inner join sampleVehicleSoakingF sf on (
	sf.soakDayID=sd.soakDayID-1
	and sf.sourceTypeID=sd.sourceTypeID
	and sf.dayID=sd.dayID)
where sd.soakDayID=5;


drop table if exists sampleVehicleSoakingF2;

create table if not exists sampleVehicleSoakingF2 (
	soakDayID smallint not null,
	sourceTypeID smallint not null,
	dayID smallint not null,
	F double,
	primary key (soakDayID, sourceTypeID, dayID)
);

insert into sampleVehicleSoakingF2 (soakDayID, sourceTypeID, dayID, F)
select soakDayID, sourceTypeID, dayID, F
from sampleVehicleSoakingF;

-- @algorithm nextF[soakDayID,sourceTypeID,dayID]=soakF[soakDayID+1,sourceTypeID,dayID].
-- @condition First bundle only.
update sampleVehicleSoakingF, sampleVehicleSoakingF2 set sampleVehicleSoakingF.NextF=sampleVehicleSoakingF2.F
where sampleVehicleSoakingF2.soakDayID=sampleVehicleSoakingF.soakDayID+1
and sampleVehicleSoakingF2.sourceTypeID=sampleVehicleSoakingF.sourceTypeID
and sampleVehicleSoakingF2.dayID=sampleVehicleSoakingF.dayID;

drop table if exists sampleVehicleSoakingF2;

drop table if exists sampleVehicleSoakingProportion;

create table sampleVehicleSoakingProportion (
	soakDayID smallint not null,
	sourceTypeID smallint not null,
	dayID smallint not null,
	hourID smallint not null,
	P double,
	primary key (soakDayID, sourceTypeID, dayID, hourID)
);

-- @algorithm P[n,1] = 1 for each soaking day
-- @condition First bundle only.
insert into sampleVehicleSoakingProportion (soakDayID, sourceTypeID, dayID, hourID, P)
select soakDayID, sourceTypeID, dayID, 1, 1
from sampleVehicleSoakingDay;

-- @algorithm D[5,1] = Gamma[F4]
-- @condition First bundle only.
insert ignore into sampleVehicleSoaking (soakDayID, sourceTypeID, dayID, hourID, soakFraction)
select soakDayID, sourceTypeID, dayID, 1, GammaFn1
from sampleVehicleSoakingF
where soakDayID=5;

-- @algorithm D[5,24] = Gamma[F5], for soakday 5
-- @condition First bundle only.
insert ignore into sampleVehicleSoaking (soakDayID, sourceTypeID, dayID, hourID, soakFraction)
select soakDayID, sourceTypeID, dayID, 24, GammaFn
from sampleVehicleSoakingF
where soakDayID=5;

-- @algorithm D[n,24] = Gamma[Fn] * (1-F[n+1]), for soakdays 1-4
-- @condition First bundle only.
insert ignore into sampleVehicleSoaking (soakDayID, sourceTypeID, dayID, hourID, soakFraction)
select soakDayID, sourceTypeID, dayID, 24, GammaFn * (1-NextF)
from sampleVehicleSoakingF
where soakDayID in (1,2,3,4);

-- @algorithm D[1,1] = D1-D24 for soakday 1
-- @condition First bundle only.
insert ignore into sampleVehicleSoaking (soakDayID, sourceTypeID, dayID, hourID, soakFraction)
select 1 as soakDayID, d1.sourceTypeID, d1.dayID, 1 as hourID, d1.soakFraction-d24.soakFraction
from sampleVehicleSoaking as d1
inner join sampleVehicleSoaking as d24 using (sourceTypeID, dayID)
where d1.soakDayID=0 and d1.hourID=1
and d24.soakDayID=0 and d24.hourID=24;

-- @algorithm D[n,1] = D[n-1,24] for soakday 2,3,4
-- @condition First bundle only.
insert ignore into sampleVehicleSoaking (soakDayID, sourceTypeID, dayID, hourID, soakFraction)
select soakDayID+1, sourceTypeID, dayID, 1 as hourID, soakFraction
from sampleVehicleSoaking
where soakDayID in (2-1,3-1,4-1) and hourID=24;

-- @algorithm P[n,24]=D[n,24]/D[n,1]
-- @condition First bundle only.
insert ignore into sampleVehicleSoakingProportion (soakDayID, sourceTypeID, dayID, hourID, P)
select d1.soakDayID, d1.sourceTypeID, d1.dayID, 24 as hourID, 
	case when d1.soakFraction <= 0 then 0 
	else d24.soakFraction/d1.soakFraction end
from sampleVehicleSoaking as d1
inner join sampleVehicleSoaking as d24 using (soakDayID, sourceTypeID, dayID)
where d1.hourID=1
and d24.hourID=24
and d1.soakDayID>0;

drop table if exists sampleVehicleSoakingDH;

create table sampleVehicleSoakingDH (
	sourceTypeID smallint not null,
	dayID smallint not null,
	hourID smallint not null,
	DFraction double,
	primary key (sourceTypeID, dayID, hourID)
);

-- @algorithm Precalc[h] = (D[0,h]-D[0,24]) / (D[0,1]-D[0,24]) because it is used in P[n,1 < h < 24]
-- @condition First bundle only.
insert into sampleVehicleSoakingDH (sourceTypeID, dayID, hourID, DFraction)
select d1.sourceTypeID, d1.dayID, d.hourID, 
	case when (d1.soakFraction-d24.soakFraction) <= 0 then 0 
	else (d.soakFraction-d24.soakFraction)/(d1.soakFraction-d24.soakFraction) end
from sampleVehicleSoaking as d
inner join sampleVehicleSoaking as d1 using (sourceTypeID, dayID)
inner join sampleVehicleSoaking as d24 using (sourceTypeID, dayID)
where d.soakDayID=0
and d1.soakDayID=0 and d1.hourID=1
and d24.soakDayID=0 and d24.hourID=24
and d.hourID > 1 and d.hourID < 24;

-- @algorithm Store P[n,1] for each day alongside P[n,24] since they are used together
-- @condition First bundle only.
update sampleVehicleSoakingF, sampleVehicleSoakingProportion set P1=P
where sampleVehicleSoakingProportion.soakDayID=sampleVehicleSoakingF.soakDayID
and sampleVehicleSoakingProportion.dayID=sampleVehicleSoakingF.dayID
and sampleVehicleSoakingProportion.sourceTypeID=sampleVehicleSoakingF.sourceTypeID
and sampleVehicleSoakingProportion.hourID=1;

update sampleVehicleSoakingF, sampleVehicleSoakingProportion set P24=P
where sampleVehicleSoakingProportion.soakDayID=sampleVehicleSoakingF.soakDayID
and sampleVehicleSoakingProportion.dayID=sampleVehicleSoakingF.dayID
and sampleVehicleSoakingProportion.sourceTypeID=sampleVehicleSoakingF.sourceTypeID
and sampleVehicleSoakingProportion.hourID=24;

-- @algorithm P[n,h]=Precalc[h]*(P[n,1]-P[n,24]) + P[n,24] for 1 <= n <= 5, 1 < h < 24
-- @condition First bundle only.
insert into sampleVehicleSoakingProportion (soakDayID, sourceTypeID, dayID, hourID, P)
select p1.soakDayID, p1.sourceTypeID, p1.dayID, dh.hourID, DFraction*(p1.P-p24.P) + p24.P
from sampleVehicleSoakingDH dh
inner join sampleVehicleSoakingProportion p1 using (sourceTypeID,dayID)
inner join sampleVehicleSoakingProportion p24 using (soakDayID,sourceTypeID,dayID)
where p1.hourID=1
and p24.hourID=24;

-- @algorithm D[n,h] = D[n,1]*P[n,h] for 1 <= n <= 5, 1 < h < 24
-- @condition First bundle only.
insert ignore into sampleVehicleSoaking (soakDayID, sourceTypeID, dayID, hourID, soakFraction)
select p.soakDayID, p.sourceTypeID, p.dayID, p.hourID, s.soakFraction * p.P
from sampleVehicleSoakingProportion p
inner join sampleVehicleSoaking s using (soakDayID, sourceTypeID, dayID)
where s.hourID=1;

-- End Section FillSampleVehicleSoaking

-- End Section FirstBundle

cache select * into outfile '##sampleVehicleSoaking##' from sampleVehicleSoaking;

cache SELECT DISTINCT ZoneMonthHour.* INTO OUTFILE '##ZoneMonthHour##'
FROM ZoneMonthHour,RunSpecHour
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND ZoneMonthHour.monthID = ##context.monthID##
AND RunSpecHour.hourID = ZoneMonthHour.hourID;

-- Section WithRegClassID
cache select *
into outfile '##RegClassSourceTypeFraction##'
from RegClassSourceTypeFraction
where modelYearID <= ##context.year##
and modelYearID >= ##context.year## - 30;
-- End Section WithRegClassID

-- End Section Extract Data

-- Section Processing

-- @algorithm Index by hourID to speedup later joins.
-- @input ColdSoakTankTemperature
alter table ColdSoakTankTemperature add key speed1 (hourID);
analyze table ColdSoakTankTemperature;

-- Create tables needed for processing
-- CREATE TABLE IF NOT EXISTS EventLog (eventRowID INTEGER UNSIGNED NOT NULL AUTO_INCREMENT, PRIMARY KEY (eventRowID), eventTime DATETIME, eventName VARCHAR(120));

-- @algorithm Include soakDayID in ColdSoakInitialHourFraction.
-- @input ColdSoakInitialHourFraction
alter table ColdSoakInitialHourFraction add column soakDayID smallint not null default 1;
alter table coldSoakInitialHourFraction drop primary key;
alter table coldSoakInitialHourFraction add primary key (sourceTypeID,zoneID,monthID,hourDayID,initialHourDayID,soakDayID);

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
);

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
);

-- @algorithm Compute peakHourID by monthID. The maximum value of the expression <coldSoakTankTemperature*
-- 100000 + (999-hourID)> is found. This finds the maximum temperature and the earliest time of day
-- in which the temperature occurs, storing both as a single number.
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

-- Section DebugTankVaporGenerated
drop table if exists DebugTankVaporGenerated;

create table DebugTankVaporGenerated (
	hourDayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	ethanolLevelID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint not null,
	polProcessID int NOT NULL DEFAULT '0',
	tankVaporGenerated float null,
	backPurgeFactor double DEFAULT NULL,
	averageCanisterCapacity double DEFAULT NULL,
	leakFraction double DEFAULT NULL,
	leakFractionIM double DEFAULT NULL,
	primary key (hourDayID, initialHourDayID, ethanolLevelID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID)
);

insert into DebugTankVaporGenerated (hourDayID, initialHourDayID, ethanolLevelID,
	monthID, sourceTypeID, fuelTypeID, 
	modelYearID, polProcessID,
	tankVaporGenerated,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM)
select ihf.hourDayID, ihf.initialHourDayID, coeffs.ethanolLevelID,
	ihf.monthID, ihf.sourceTypeID, avggas.fuelTypeID,
	stmycoeffs.modelYearID, stmycoeffs.polProcessID,
	case when t1.coldSoakTankTemperature >= t2.coldSoakTankTemperature then 0.0
		else
		(stmycoeffs.tankSize*(1-stmycoeffs.tankFillFraction))*(tvgTermA*exp(tvgTermB*RVP)*(exp(tvgTermC*t2.coldSoakTankTemperature)-exp(tvgTermC*t1.coldSoakTankTemperature)))
	end as tankVaporGenerated,
	stmycoeffs.backPurgeFactor, stmycoeffs.averageCanisterCapacity, stmycoeffs.leakFraction, stmycoeffs.leakFractionIM
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
inner join stmyTVVCoeffs stmycoeffs on (
	stmycoeffs.sourceTypeID = ihf.sourceTypeID
	and stmycoeffs.fuelTypeID = avggas.fuelTypeID
)
where ihf.hourDayID <> ihf.initialHourDayID
	and hd.hourID <= ph.peakHourID
	and ihf.coldSoakInitialHourFraction > 0;

analyze table DebugTankVaporGenerated;

-- End Section DebugTankVaporGenerated

drop table if exists TankVaporGeneratedHighAndLow;

create table TankVaporGeneratedHighAndLow (
	altitudeFlag varchar(1) not null,
	hourDayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	ethanolLevelID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint not null,
	polProcessID int NOT NULL DEFAULT '0',
	tankVaporGenerated float null,
	backPurgeFactor double DEFAULT NULL,
	averageCanisterCapacity double DEFAULT NULL,
	leakFraction double DEFAULT NULL,
	leakFractionIM double DEFAULT NULL,
	primary key (altitudeFlag, hourDayID, initialHourDayID, ethanolLevelID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID)
);

-- @algorithm Calculate tankVaporGenerated for high and low altitudes.
-- The time spans t1 (based upon ColdSoakInitialHourFraction.initialHourDayID) to t2 (based upon ColdSoakInitialHourFraction.hourDayID).
-- tankVaporGenerated[high and low altitudes] = (tankSize*(1-tankFillFraction))*(tvgTermA*exp(tvgTermB*RVP)*(exp(tvgTermC*t2.coldSoakTankTemperature)-exp(tvgTermC*t1.coldSoakTankTemperature)))
-- @condition t1.coldSoakTankTemperature < t2.coldSoakTankTemperature, 0 otherwise.
insert into TankVaporGeneratedHighAndLow (altitudeFlag, hourDayID, initialHourDayID, ethanolLevelID,
	monthID, sourceTypeID, fuelTypeID, 
	modelYearID, polProcessID,
	tankVaporGenerated,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM)
select coeffs.altitude, ihf.hourDayID, ihf.initialHourDayID, coeffs.ethanolLevelID,
	ihf.monthID, ihf.sourceTypeID, avggas.fuelTypeID,
	stmycoeffs.modelYearID, stmycoeffs.polProcessID,
	case when t1.coldSoakTankTemperature >= t2.coldSoakTankTemperature then 0.0
		else
		(stmycoeffs.tankSize*(1-stmycoeffs.tankFillFraction))*(tvgTermA*exp(tvgTermB*RVP)*(exp(tvgTermC*t2.coldSoakTankTemperature)-exp(tvgTermC*t1.coldSoakTankTemperature)))
	end as tankVaporGenerated,
	stmycoeffs.backPurgeFactor, stmycoeffs.averageCanisterCapacity, stmycoeffs.leakFraction, stmycoeffs.leakFractionIM
from TankVaporGenCoeffs coeffs, ColdSoakInitialHourFraction ihf
inner join HourDay hd on (hd.hourDayID = ihf.hourDayID)
inner join HourDay ihd on (ihd.hourDayID = ihf.initialHourDayID)
inner join PeakHourOfColdSoak ph on (ihf.monthID = ph.monthID)
inner join ColdSoakTankTemperature t2 on (ihf.monthID = t2.monthID and hd.hourID = t2.hourID)
inner join ColdSoakTankTemperature t1 on (ihf.monthID = t1.monthID and ihd.hourID = t1.hourID)
inner join MonthOfAnyYear m on (ihf.monthID = m.monthID)
inner join AverageTankGasoline avggas on (m.monthGroupID = avggas.monthGroupID)
inner join stmyTVVCoeffs stmycoeffs on (
	stmycoeffs.sourceTypeID = ihf.sourceTypeID
	and stmycoeffs.fuelTypeID = avggas.fuelTypeID
)
where (ihf.hourDayID <> ihf.initialHourDayID or (hd.hourID=1 and ihd.hourID=1 and hd.dayID=ihd.dayID))
	and hd.hourID <= ph.peakHourID
	and ihf.coldSoakInitialHourFraction > 0;

analyze table TankVaporGeneratedHighAndLow;

drop table if exists TankVaporGenerated;

create table TankVaporGenerated (
	hourDayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	ethanolLevelID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint not null,
	polProcessID int NOT NULL DEFAULT '0',
	tankVaporGenerated float null,
	backPurgeFactor double DEFAULT NULL,
	averageCanisterCapacity double DEFAULT NULL,
	leakFraction double DEFAULT NULL,
	leakFractionIM double DEFAULT NULL,
	primary key (hourDayID, initialHourDayID, ethanolLevelID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID)
);

-- @algorithm Interpolate tankVaporGenerated based upon the county's barometric pressure.
-- Don't allow negative values.
-- Low altitude is based upon Wayne County, Michigan (26163) with a barometric pressure of 29.069.
-- High altitude is based upon Denver County, Colorado (8031) with a barometric pressure of 24.087.
-- tankVaporGenerated = greatest(((barometricPressure - 29.069) / (24.087 - 29.069)) * (high.tankVaporGenerated - low.tankVaporGenerated) + low.tankVaporGenerated,0).
insert into TankVaporGenerated (hourDayID, initialHourDayID, ethanolLevelID,
	monthID, sourceTypeID, fuelTypeID, 
	modelYearID, polProcessID,
	tankVaporGenerated,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM)
select low.hourDayID, low.initialHourDayID, low.ethanolLevelID,
	low.monthID, low.sourceTypeID, low.fuelTypeID,
	low.modelYearID, low.polProcessID,
	greatest(((c.barometricPressure - 29.069) / (24.087 - 29.069)) * (high.tankVaporGenerated - low.tankVaporGenerated) + low.tankVaporGenerated,0) as tankVaporGenerated,
	low.backPurgeFactor, low.averageCanisterCapacity, low.leakFraction, low.leakFractionIM
from County c, 
TankVaporGeneratedHighAndLow low
inner join TankVaporGeneratedHighAndLow high using (hourDayID, initialHourDayID, ethanolLevelID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID)
where low.altitudeFlag='L'
and high.altitudeFlag='H';


analyze table TankVaporGenerated;

-- 
-- TVV-4: Calculate Ethanol-weighted TVG
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-4';
drop table if exists EthanolWeightedTVG;

create table EthanolWeightedTVGTemp (
	hourDayID smallint(6) not null,
	hourID smallint(6) not null,
	dayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint not null,
	polProcessID int NOT NULL DEFAULT '0',
	ethanolWeightedTVG float null,
	backPurgeFactor double DEFAULT NULL,
	averageCanisterCapacity double DEFAULT NULL,
	leakFraction double DEFAULT NULL,
	leakFractionIM double DEFAULT NULL,
	primary key (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID),
	key (hourID, dayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID)
);

-- @algorithm Calculated the ethanol weighted TVG using tankVaporGenerated for E10 fuel (t10) and that of E0 fuel (t0).
-- ethanolWeightedTVG=(t10.tankVaporGenerated*(least(10.0,ETOHVolume)/10.0)+t0.tankVaporGenerated*(1.0-least(10.0,ETOHVolume)/10.0)).
insert into EthanolWeightedTVGTemp (hourDayID, hourID, dayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
	modelYearID, polProcessID,
	ethanolWeightedTVG,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM)
select t0.hourDayID, floor(t0.hourDayID/10) as hourID, mod(t0.hourDayID,10) as dayID,
	t0.initialHourDayID, t0.monthID, t0.sourceTypeiD, t0.fuelTypeID,
	t0.modelYearID, t0.polProcessID,
	(t10.tankVaporGenerated*(least(10.0,ETOHVolume)/10.0)+t0.tankVaporGenerated*(1.0-least(10.0,ETOHVolume)/10.0)) as ethanolWeightedTVG,
	t0.backPurgeFactor, t0.averageCanisterCapacity, t0.leakFraction, t0.leakFractionIM
from TankVaporGenerated t0
inner join TankVaporGenerated t10 on (t0.hourDayID=t10.hourDayID and t0.initialHourDayID=t10.initialHourDayID
	and t0.monthID=t10.monthID and t0.sourceTypeID=t10.sourceTypeID
	and t0.fuelTypeID=t10.fuelTypeID
	and t0.modelYearID=t10.modelYearID
	and t0.polProcessID=t10.polProcessID)
inner join MonthOfAnyYear m on (t10.monthID = m.monthID)
inner join AverageTankGasoline avggas on (m.monthGroupID = avggas.monthGroupID
	and t10.fuelTypeID=avggas.fuelTypeID)
where t0.ethanolLevelID = 0
and t10.ethanolLevelID = 10;

analyze table EthanolWeightedTVGTemp;

create table EthanolWeightedTVG (
	hourDayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint not null,
	polProcessID int NOT NULL DEFAULT '0',
	ethanolWeightedTVG float null,
	backPurgeFactor double DEFAULT NULL,
	averageCanisterCapacity double DEFAULT NULL,
	leakFraction double DEFAULT NULL,
	leakFractionIM double DEFAULT NULL,
	primary key (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID)
);

-- @algorithm Until now, ethanolWeightedTVG has been a cummulative total.
-- Convert it to an hourly increment by subtracting the total of the previous hour.
-- ethanolWeightedTVG[hourID]=ethanolWeightedTVG[hourID]-ethanolWeightedTVG[hourID-1].
insert into EthanolWeightedTVG (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
	modelYearID, polProcessID,
	ethanolWeightedTVG,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM)
select h.hourDayID, h.initialHourDayID, h.monthID, h.sourceTypeID, h.fuelTypeID, 
	h.modelYearID, h.polProcessID,
	greatest(0,h.ethanolWeightedTVG - coalesce(hm1.ethanolWeightedTVG,0)) as ethanolWeightedTVG,
	h.backPurgeFactor, h.averageCanisterCapacity, h.leakFraction, h.leakFractionIM
from EthanolWeightedTVGTemp h
left outer join EthanolWeightedTVGTemp hm1 on (
	h.hourID-1 = hm1.hourID
	and h.dayID = hm1.dayID
	and h.initialHourDayID = hm1.initialHourDayID
	and h.monthID = hm1.monthID
	and h.sourceTypeID = hm1.sourceTypeID
	and h.fuelTypeID = hm1.fuelTypeID
	and h.modelYearID = hm1.modelYearID
	and h.polProcessID = hm1.polProcessID
);

analyze table EthanolWeightedTVG;

drop table if exists TVG;

create table TVG (
	soakDayID smallint(6) not null,
	hourDayID smallint(6) not null,
	initialHourDayID smallint(6) not null,
	monthID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	modelYearID smallint not null,
	polProcessID int NOT NULL DEFAULT '0',
	TVGdaily float null,
	Xn double null,
	backPurgeFactor double DEFAULT NULL,
	averageCanisterCapacity double DEFAULT NULL,
	leakFraction double DEFAULT NULL,
	leakFractionIM double DEFAULT NULL,
	tvgSum1H double default null,
	tvgSumH24 double default null,
	primary key (sourceTypeID, modelYearID, fuelTypeID, polProcessID, soakDayID, hourDayID, initialHourDayID, monthID)
);

drop table if exists tvgSumIH;
drop table if exists tvgSumI24;
drop table if exists tvgSum1H;
drop table if exists tvgSumH24;

-- Sum of TVG hourly from initial hour I to hour H
create table tvgSumIH (
	hourDayID smallint(6) not null default '0',
	initialHourDayID smallint(6) not null default '0',
	monthID smallint(6) not null default '0',
	sourceTypeID smallint(6) not null default '0',
	fuelTypeID smallint(6) not null default '0',
	modelYearID smallint(6) not null default '0',
	polProcessID int not null default '0',
	tvgSum double,
	primary key (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID)
);

-- Sum of TVG hourly from initial hour I to the last hour of the day
create table tvgSumI24 like tvgSumIH;

-- Sum of TVG hourly from the first hour of the day to hour H
create table tvgSum1H like tvgSumIH;

-- Sum of TVG hourly from after hour H to the last hour of the day
create table tvgSumH24 like tvgSumIH;

-- @algorithm tvgSumIH = Sum of TVG hourly from initial hour I to hour H
insert into tvgSumIH (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, tvgSum)
select e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID,
	sum(e2.ethanolWeightedTVG) as tvgSum
from EthanolWeightedTVG as e
inner join hourDay as hd on (hd.hourDayID = e.hourDayID)
inner join hourDay as ihd on (ihd.hourDayID = e.initialHourDayID)
inner join hourDay as ahd on (ahd.dayID = hd.dayID and ahd.hourID >= ihd.hourID and ahd.hourID <= hd.hourID)
inner join EthanolWeightedTVG e2 on (e2.hourDayID = ahd.hourDayID and e2.initialHourDayID = e.initialHourDayID
and e2.monthID = e.monthID and e2.sourceTypeID = e.sourceTypeID and e2.fuelTypeID = e.fuelTypeID and e2.modelYearID = e.modelYearID and e2.polProcessID = e.polProcessID)
where hd.hourID >= ihd.hourID
group by e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID;

insert into tvgSumIH (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, tvgSum)
select e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID,
	0 as tvgSum
from EthanolWeightedTVG as e
inner join hourDay as hd on (hd.hourDayID = e.hourDayID)
inner join hourDay as ihd on (ihd.hourDayID = e.initialHourDayID)
where hd.hourID < ihd.hourID
group by e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID;

-- @algorithm tvgSumI24 = Sum of TVG hourly from initial hour I to the last hour of the day
insert into tvgSumI24 (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, tvgSum)
select e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID,
	sum(e2.ethanolWeightedTVG) as tvgSum
from EthanolWeightedTVG as e
inner join hourDay as hd on (hd.hourDayID = e.hourDayID)
inner join hourDay as ihd on (ihd.hourDayID = e.initialHourDayID)
inner join hourDay as ahd on (ahd.dayID = hd.dayID and ahd.hourID >= ihd.hourID and ahd.hourID <= 24)
inner join EthanolWeightedTVG e2 on (e2.hourDayID = ahd.hourDayID and e2.initialHourDayID = e.initialHourDayID
and e2.monthID = e.monthID and e2.sourceTypeID = e.sourceTypeID and e2.fuelTypeID = e.fuelTypeID and e2.modelYearID = e.modelYearID and e2.polProcessID = e.polProcessID)
group by e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID;

-- @algorithm tvgSum1H = Sum of TVG hourly from the first hour of the day to hour H
insert into tvgSum1H (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, tvgSum)
select e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID,
	sum(e2.ethanolWeightedTVG) as tvgSum
from EthanolWeightedTVG as e
inner join hourDay as hd on (hd.hourDayID = e.hourDayID)
inner join hourDay as ihd on (ihd.hourDayID = e.initialHourDayID)
inner join hourDay as ahd on (ahd.dayID = hd.dayID and ahd.hourID >= 1 and ahd.hourID <= hd.hourID)
inner join hourDay as hd1 on (hd1.dayID = hd.dayID and hd1.hourID = 1)
inner join EthanolWeightedTVG e2 on (e2.hourDayID = ahd.hourDayID and e2.initialHourDayID = hd1.hourDayID
and e2.monthID = e.monthID and e2.sourceTypeID = e.sourceTypeID and e2.fuelTypeID = e.fuelTypeID and e2.modelYearID = e.modelYearID and e2.polProcessID = e.polProcessID)
group by e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID;

-- @algorithm tvgSumH24 = Sum of TVG hourly from after hour H to the last hour of the day
insert into tvgSumH24 (hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, modelYearID, polProcessID, tvgSum)
select e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID,
	sum(e2.ethanolWeightedTVG) as tvgSum
from EthanolWeightedTVG as e
inner join hourDay as hd on (hd.hourDayID = e.hourDayID)
inner join hourDay as ihd on (ihd.hourDayID = e.initialHourDayID)
inner join hourDay as ahd on (ahd.dayID = hd.dayID and ahd.hourID > hd.hourID and ahd.hourID <= 24)
inner join EthanolWeightedTVG e2 on (e2.hourDayID = ahd.hourDayID and e2.initialHourDayID = e.initialHourDayID
and e2.monthID = e.monthID and e2.sourceTypeID = e.sourceTypeID and e2.fuelTypeID = e.fuelTypeID and e2.modelYearID = e.modelYearID and e2.polProcessID = e.polProcessID)
group by e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, e.modelYearID, e.polProcessID;

-- Fill the first soaking day (soakDayID=1)
-- Soak Day 1, including X0 (TVGDaily)

-- @algorithm TVGdaily[soakDayID=1] = tvgSumIH.
-- Xn[soakDayID=1] = tvgSumIH.
insert into TVG (soakDayID, hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
	modelYearID, polProcessID,
	TVGdaily, Xn,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM, tvgSum1H, tvgSumH24)
select 1 as soakDayID, e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, 
	e.modelYearID, e.polProcessID,
	sih.tvgSum as TVGdaily,
	sih.tvgSum as Xn,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM,
	s1h.tvgSum as tvgSum1H,
	coalesce(sh24.tvgSum,0) as tvgSumH24
from EthanolWeightedTVG e
inner join tvgSum1H s1h on (s1h.hourDayID = e.hourDayID and s1h.initialHourDayID = e.initialHourDayID and s1h.monthID = e.monthID and s1h.sourceTypeID = e.sourceTypeID and s1h.fuelTypeID = e.fuelTypeID and s1h.modelYearID = e.modelYearID and s1h.polProcessID = e.polProcessID)
inner join tvgSumIH sih on (sih.hourDayID = e.hourDayID and sih.initialHourDayID = e.initialHourDayID and sih.monthID = e.monthID and sih.sourceTypeID = e.sourceTypeID and sih.fuelTypeID = e.fuelTypeID and sih.modelYearID = e.modelYearID and sih.polProcessID = e.polProcessID)
left outer join tvgSumH24 sh24 on (sh24.hourDayID = e.hourDayID and sh24.initialHourDayID = e.initialHourDayID and sh24.monthID = e.monthID and sh24.sourceTypeID = e.sourceTypeID and sh24.fuelTypeID = e.fuelTypeID and sh24.modelYearID = e.modelYearID and sh24.polProcessID = e.polProcessID);

-- inner join tvgSumH24 sh24 on (sh24.hourDayID = e.hourDayID and sh24.initialHourDayID = e.initialHourDayID and sh24.monthID = e.monthID and sh24.sourceTypeID = e.sourceTypeID and sh24.fuelTypeID = e.fuelTypeID and sh24.modelYearID = e.modelYearID and sh24.polProcessID = e.polProcessID)

-- Soak Day 2

-- @algorithm TVGdaily[soakDayID=2] = TVGdaily[soakDayID=1].
-- Xn[soakDayID=2] = ((1-backPurgeFactor)*least(tvgSumI24,averageCanisterCapacity))+tvgSum1H.
insert into TVG (soakDayID, hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
	modelYearID, polProcessID,
	TVGdaily, Xn,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM, tvgSum1H, tvgSumH24)
select 2 as soakDayID, e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, 
	e.modelYearID, e.polProcessID,
	TVGdaily as TVGdaily,
	(((1-backPurgeFactor)*least(si24.tvgSum,averageCanisterCapacity))+tvgSum1H) as Xn,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM, tvgSum1H, tvgSumH24
from TVG e
inner join tvgSumI24 si24 on (si24.hourDayID = e.hourDayID and si24.initialHourDayID = e.initialHourDayID and si24.monthID = e.monthID and si24.sourceTypeID = e.sourceTypeID and si24.fuelTypeID = e.fuelTypeID and si24.modelYearID = e.modelYearID and si24.polProcessID = e.polProcessID)
where soakDayID=2-1;

-- Fill all subsequent soak days
loop ##loop.soakDayID##;
select distinct soakDayID from sampleVehicleSoaking where soakDayID > 2 order by soakDayID;

-- insert into TVG (soakDayID, hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
-- 	modelYearID, polProcessID,
-- 	TVGdaily, Xn,
-- 	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM)
-- select ##loop.soakDayID## as soakDayID, hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
-- 	modelYearID, polProcessID,
-- 	TVGdaily as TVGdaily,
-- 	Xn,
-- 	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM
-- from TVG
-- where soakDayID=##loop.soakDayID##-1

-- insert into TVG (soakDayID, hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
-- 	modelYearID, polProcessID,
-- 	TVGdaily, Xn,
-- 	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM)
-- select ##loop.soakDayID## as soakDayID, hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
-- 	modelYearID, polProcessID,
-- 	TVGdaily as TVGdaily,
-- 	(((1-backPurgeFactor)*least(Xn,averageCanisterCapacity))+TVGdaily) as Xn,
-- 	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM
-- from TVG
-- where soakDayID=##loop.soakDayID##-1

-- @algorithm TVGdaily[soakDayID>2] = TVGdaily[soakDayID-1].
-- Xn[soakDayID>2] = ((1-backPurgeFactor)*least(Xn[soakDayID-1] + tvgSumH24,averageCanisterCapacity))+tvgSum1H.
insert into TVG (soakDayID, hourDayID, initialHourDayID, monthID, sourceTypeID, fuelTypeID, 
	modelYearID, polProcessID,
	TVGdaily, Xn,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM, tvgSum1H, tvgSumH24)
select ##loop.soakDayID## as soakDayID, e.hourDayID, e.initialHourDayID, e.monthID, e.sourceTypeID, e.fuelTypeID, 
	e.modelYearID, e.polProcessID,
	TVGdaily as TVGdaily,
	(((1-backPurgeFactor)*least(Xn + tvgSumH24,averageCanisterCapacity))+tvgSum1H) as Xn,
	backPurgeFactor, averageCanisterCapacity, leakFraction, leakFractionIM, tvgSum1H, tvgSumH24
from TVG e
where soakDayID=##loop.soakDayID##-1;

end loop ##loop.soakDayID##;

-- 
-- TVV-5: Calculate Cummulative Tank Vapor Vented (TVV)
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-5';
drop table if exists CummulativeTankVaporVented;

create table CummulativeTankVaporVented (
	soakDayID smallint(6) not null,
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
	primary key (soakDayID, regClassID, ageID, polProcessID, dayID, hourID, initialHourDayID, monthID, sourceTypeID, fuelTypeID),
	index (priorHourID)
);

-- @algorithm Include leaking using database-driven TVV and Leak equations.
-- tankVaporVented = (1-leakFraction)*(TVV equation) + leakFraction*(Leak equation).
-- tankVaporVentedIM = (1-leakFractionIM)*(TVV equation) + leakFractionIM*(Leak equation).
insert into CummulativeTankVaporVented (soakDayID, regClassID, ageID, polProcessID, dayID, hourID, initialHourDayID, 
	monthID, sourceTypeID, fuelTypeID, tankVaporVented, tankVaporVentedIM,
	hourDayID, priorHourID)
select t.soakDayID, coeffs.regClassID, acat.ageID, coeffs.polProcessID, hd.dayID, hd.hourID, t.initialHourDayID,
	t.monthID, t.sourceTypeID, t.fuelTypeID,
	regClassFractionOfSourceTypeModelYearFuel*greatest(0.0,
		(1.0-coeffs.leakFraction)*(##tvvEquations##)
		+ (coeffs.leakFraction)*(##leakEquations##)
	) as tankVaporVented,
	regClassFractionOfSourceTypeModelYearFuel*greatest(0.0,
		(1.0-coalesce(coeffs.leakFractionIM,coeffs.leakFraction))*(##tvvEquations##)
		+ coalesce(coeffs.leakFractionIM,coeffs.leakFraction)*(##leakEquations##)
	) as tankVaporVentedIM,
	t.hourDayID,
	mod(hd.hourID-1-1+24,24)+1
from stmyTVVEquations coeffs
inner join TVG t on (
	t.sourceTypeID=coeffs.sourceTypeID
	and t.modelYearID=coeffs.modelYearID
	and t.fuelTypeID=coeffs.fuelTypeID
	and t.polProcessID=coeffs.polProcessID)
inner join AgeCategory acat on (acat.ageID = ##context.year## - coeffs.modelYearID)
inner join HourDay hd on (hd.hourDayID = t.hourDayID);

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
	soakDayID smallint(6) not null,
	unweightedHourlyTVV float null,
	unweightedHourlyTVVIM float null,
	index (sourceTypeID, zoneID, monthID, hourDayID, initialHourDayID, fuelTypeID, soakDayID),
	index (soakDayID, sourceTypeID, zoneID, monthID, hourDayID, initialHourDayID, fuelTypeID)
);

-- @algorithm unweightedHourlyTVV = tankVaporVented[hourID] - tankVaporVented[hourID-1].
-- unweightedHourlyTVVIM = tankVaporVentedIM[hourID] - tankVaporVentedIM[hourID-1].
-- @condition only when coldSoakTankTemperature[hourID] > coldSoakTankTemperature[hourID-1], 0 otherwise.
insert into UnweightedHourlyTVV (soakDayID, regClassID, ageID, polProcessID, hourDayID, initialHourDayID, 
	monthID, sourceTypeID, fuelTypeID, unweightedHourlyTVV, unweightedHourlyTVVIM)
select ctv1.soakDayID, ctv1.regClassID, ctv1.ageID, ctv1.polProcessID, ctv1.hourDayID, ctv1.initialHourDayID, 
	ctv1.monthID, ctv1.sourceTypeID, ctv1.fuelTypeID,
  	case when (ctt1.coldSoakTankTemperature <= ctt2.coldSoakTankTemperature) then
    	0.0
  	else greatest(0,(ctv1.tankVaporVented-coalesce(ctv2.tankVaporVented,0.0)))
  	end
  	as unweightedHourlyTVV,
  	case when (ctt1.coldSoakTankTemperature <= ctt2.coldSoakTankTemperature) then
  	0.0
  	else greatest(0,(ctv1.tankVaporVentedIM-coalesce(ctv2.tankVaporVentedIM,0.0)))
  	end
  	as unweightedHourlyTVVIM
from CummulativeTankVaporVented ctv1
left join CummulativeTankVaporVented ctv2 on (
	ctv1.soakDayID = ctv2.soakDayID
	and ctv1.regClassID = ctv2.regClassID and ctv1.ageID = ctv2.ageID
	and ctv1.polProcessID = ctv2.polProcessID and ctv1.initialHourDayID = ctv2.initialHourDayID 
	and ctv1.monthID = ctv2.monthID and ctv1.sourceTypeID = ctv2.sourceTypeID
	and ctv1.fuelTypeID = ctv2.fuelTypeID
	and ctv1.priorHourID = ctv2.hourID
	and ctv1.dayID = ctv2.dayID)
left join coldsoaktanktemperature ctt1 on (
  	ctt1.hourID = ctv1.HourID and
  	ctt1.monthID = ctv1.monthID)
left join coldsoaktanktemperature ctt2 on (
  	ctt2.hourID = ctv1.priorHourID and
  	ctt2.monthID = ctv1.monthID);

analyze table UnweightedHourlyTVV;

-- 
-- TVV-7: Calculate Weighted Hourly TVV Across Initial/Current pair
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-7';

-- @algorithm Add coldSoakInitialHourFraction entries for soaking days beyond the first day
insert into coldSoakInitialHourFraction (soakDayID, sourceTypeID, zoneID, monthID,
	hourDayID,
	initialHourDayID,
	coldSoakInitialHourFraction)
select s.soakDayID, s.sourceTypeID, ##context.iterLocation.zoneRecordID## as zoneID, ##context.monthID## as monthID,
	hd.hourDayID,
	ihd.hourDayID as initialHourDayID,
	soakFraction as coldSoakInitialHourFraction
from sampleVehicleSoaking s 
inner join HourDay hd on (hd.hourID=s.hourID and hd.dayID=s.dayID)
inner join HourDay ihd on (ihd.hourID=1 and ihd.dayID=s.dayID)
where s.soakDayID > 1;

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
);

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
	soakDayID smallint not null,
	hourlyTVV float null,
	hourlyTVVIM float null,
	index (regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID, soakDayID)
);

-- @algorithm hourlyTVV = unweightedHourlyTVV * coldSoakInitialHourFraction.
-- hourlyTVVIM = unweightedHourlyTVVIM * coldSoakInitialHourFraction.
insert into HourlyTVVTemp (soakDayID, regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID,
	hourlyTVV, hourlyTVVIM)
select uhtvv.soakDayID, uhtvv.regClassID, uhtvv.ageID, uhtvv.polProcessID, uhtvv.hourDayID, uhtvv.monthID, uhtvv.sourceTypeID, uhtvv.fuelTypeID,
	(unweightedHourlyTVV*coldSoakInitialHourFraction) as hourlyTVV,
	(unweightedHourlyTVVIM*coldSoakInitialHourFraction) as hourlyTVVIM
from UnweightedHourlyTVV uhtvv
inner join ColdSoakInitialHourFraction ihf on (
	uhtvv.soakDayID = ihf.soakDayID
	and uhtvv.sourceTypeID = ihf.sourceTypeID
	and uhtvv.zoneID = ihf.zoneID and uhtvv.monthID = ihf.monthID 
	and uhtvv.hourDayID = ihf.hourDayID and uhtvv.initialHourDayID = ihf.initialHourDayID);

-- @algorithm hourlyTVV = sum(hourlyTVV).
-- hourlyTVVIM = sum(hourlyTVVIM).
insert into HourlyTVV (regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID, hourlyTVV, hourlyTVVIM)
select regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID,
	sum(hourlyTVV) as hourlyTVV,
	sum(hourlyTVVIM) as hourlyTVVIM
from HourlyTVVTemp
group by regClassID, ageID, polProcessID, hourDayID, monthID, sourceTypeID, fuelTypeID
order by null;

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
);
insert into CopyOfHourlyTVV (regClassID, ageID, polProcessID, dayID, hourID, 
	monthID, sourceTypeID, fuelTypeID, hourlyTVV, hourlyTVVIM)
select regClassID, ageID, polProcessID, dayID, hourID,
	monthID, sourceTypeID, fuelTypeID, hourlyTVV, hourlyTVVIM 
from HourlyTVV
inner join HourDay on (HourDay.hourDayID=HourlyTVV.hourDayID);

-- @algorithm Reduce TVV emissions for hours past the hour with the peak temperature.
-- hourlyTVV(with and without I/M) = hourlyTVV(with and without I/M) * 
-- (0 when >= 5 hours past the peak, 0.0005 when >= 4 hours, 0.0040 when >= 3 hours, 0.0100 when >= 2 hours, 
-- 0.0200 when >= 1 hour, 1.0 otherwise).
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
	regClassID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	monthID smallint(6) not null,
	hourDayID smallint(6) not null,
	modelYearID smallint(6) not null,
	opModeID smallint(6) not null,
	weightedMeanBaseRate float not null,
	weightedMeanBaseRateIM float not null,
	tempAdjustment float null,
	rvpAdjustment float null,
	unadjustedWeightedMeanBaseRate float null,
	primary key (polProcessID, sourceTypeID, regClassID, fuelTypeID, monthID, hourDayID, modelYearID, opModeID)
);

-- For cold soak mode (opModeID=151)
-- Section WithRegClassID

-- @algorithm THC rate[opModeID=151 cold soak]=sum(sourceBinActivityFraction * hourlyTVV).
-- THC I/M rate[opModeID=151 cold soak]=sum(sourceBinActivityFraction * hourlyTVVIM).
insert into WeightedMeanBaseRate (polProcessID, sourceTypeID, regClassID, fuelTypeID, monthID, hourDayID, 
	modelYearID, opModeID, weightedMeanBaseRate, weightedMeanBaseRateIM)
select htvv.polProcessID, htvv.sourceTypeID, sb.regClassID, sb.fuelTypeID, htvv.monthID, htvv.hourDayID, 
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
group by htvv.polProcessID, htvv.sourceTypeID, sb.regClassID, sb.fuelTypeID, htvv.monthID, htvv.hourDayID, 
	stmy.modelYearID
order by null;
-- End Section WithRegClassID

-- Section NoRegClassID
insert into WeightedMeanBaseRate (polProcessID, sourceTypeID, regClassID, fuelTypeID, monthID, hourDayID, 
	modelYearID, opModeID, weightedMeanBaseRate, weightedMeanBaseRateIM)
select htvv.polProcessID, htvv.sourceTypeID, 0 as regClassID, sb.fuelTypeID, htvv.monthID, htvv.hourDayID, 
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
-- End Section NoRegClassID

-- For operating and hot soak modes (opModeIDs 300 and 150)
alter table averageTankGasoline 
	add column adjustTerm3 double not null default 0,
	add column adjustTerm2 double not null default 0,
	add column adjustTerm1 double not null default 0,
	add column adjustConstant double not null default 0;

drop table if exists evapRVPTemperatureAdjustmentSummary;

-- @algorithm Find bounds on RVP-based operating and hot soak adjustments (opModeIDs 300 and 150).
create table evapRVPTemperatureAdjustmentSummary
select processID, fuelTypeID, min(RVP) as minRVP, max(RVP) as maxRVP
from evapRVPTemperatureAdjustment
group by processID, fuelTypeID;

alter table evapRVPTemperatureAdjustmentSummary add key (processID, fuelTypeID);

insert ignore into evapRVPTemperatureAdjustment (processID, fuelTypeID, RVP, adjustTerm3, adjustTerm2, adjustTerm1, adjustConstant)
select adj.processID, adj.fuelTypeID, -1 as RVP, adj.adjustTerm3, adj.adjustTerm2, adj.adjustTerm1, adj.adjustConstant
from evapRVPTemperatureAdjustment adj
inner join evapRVPTemperatureAdjustmentSummary sadj on (
	sadj.processID=adj.processID
	and sadj.fuelTypeID=adj.fuelTypeID
	and sadj.minRVP=adj.RVP);

insert ignore into evapRVPTemperatureAdjustment (processID, fuelTypeID, RVP, adjustTerm3, adjustTerm2, adjustTerm1, adjustConstant)
select adj.processID, adj.fuelTypeID, 1000 as RVP, adj.adjustTerm3, adj.adjustTerm2, adj.adjustTerm1, adj.adjustConstant
from evapRVPTemperatureAdjustment adj
inner join evapRVPTemperatureAdjustmentSummary sadj on (
	sadj.processID=adj.processID
	and sadj.fuelTypeID=adj.fuelTypeID
	and sadj.maxRVP=adj.RVP);

drop table if exists tempATG;
create table tempATG like averageTankGasoline;
insert into tempATG select * from averageTankGasoline;
truncate averageTankGasoline;

-- @algorithm Linearly interpolate RVP adjustment terms for each entry in averageTankGasoline, where lowAdj is the evapRVPTemperatureAdjustment
-- record such that lowAdj.RVP <= averageTankGasoline.RVP and highAdj is similar with highAdj.RVP > averageTankGasoline.RVP.
-- adjustTerm3 = lowAdj.adjustTerm3 + (highAdj.adjustTerm3 - lowAdj.adjustTerm3)/(highAdj.RVP - lowAdj.RVP) * (atg.RVP - lowAdj.RVP)).
-- adjustTerm2 = lowAdj.adjustTerm2 + (highAdj.adjustTerm2 - lowAdj.adjustTerm2)/(highAdj.RVP - lowAdj.RVP) * (atg.RVP - lowAdj.RVP).
-- adjustTerm1 = lowAdj.adjustTerm1 + (highAdj.adjustTerm1 - lowAdj.adjustTerm1)/(highAdj.RVP - lowAdj.RVP) * (atg.RVP - lowAdj.RVP).
-- adjustConstant = lowAdj.adjustConstant + (highAdj.adjustConstant - lowAdj.adjustConstant)/(highAdj.RVP - lowAdj.RVP) * (atg.RVP - lowAdj.RVP).
insert ignore into averageTankGasoline (zoneID, fuelYearID, monthGroupID, ETOHVolume, RVP, fuelTypeID, isUserInput,
	adjustTerm3, adjustTerm2, adjustTerm1, adjustConstant)
select atg.zoneID, atg.fuelYearID, atg.monthGroupID, atg.ETOHVolume, atg.RVP, atg.fuelTypeID, atg.isUserInput,
	(lowAdj.adjustTerm3 + (highAdj.adjustTerm3 - lowAdj.adjustTerm3)/(highAdj.RVP - lowAdj.RVP) * (atg.RVP - lowAdj.RVP)) as adjustTerm3,
	(lowAdj.adjustTerm2 + (highAdj.adjustTerm2 - lowAdj.adjustTerm2)/(highAdj.RVP - lowAdj.RVP) * (atg.RVP - lowAdj.RVP)) as adjustTerm2,
	(lowAdj.adjustTerm1 + (highAdj.adjustTerm1 - lowAdj.adjustTerm1)/(highAdj.RVP - lowAdj.RVP) * (atg.RVP - lowAdj.RVP)) as adjustTerm1,
	(lowAdj.adjustConstant + (highAdj.adjustConstant - lowAdj.adjustConstant)/(highAdj.RVP - lowAdj.RVP) * (atg.RVP - lowAdj.RVP)) as adjustConstant
from tempATG atg
inner join evapRVPTemperatureAdjustment lowAdj on (lowAdj.RVP <= atg.RVP and lowAdj.processID=12 and lowAdj.fuelTypeID=atg.fuelTypeID)
inner join evapRVPTemperatureAdjustment highAdj on (highAdj.RVP > atg.RVP and highAdj.processID=12 and highAdj.fuelTypeID=atg.fuelTypeID)
where lowAdj.RVP = (select max(RVP) from evapRVPTemperatureAdjustment lowAdj2 where lowAdj2.RVP <= atg.RVP and lowAdj2.processID=12 and lowAdj2.fuelTypeID=atg.fuelTypeID)
and highAdj.RVP = (select min(RVP) from evapRVPTemperatureAdjustment highAdj2 where highAdj2.RVP > atg.RVP and highAdj2.processID=12 and highAdj2.fuelTypeID=atg.fuelTypeID);

-- Section WithRegClassID

-- @algorithm Adjust hot soak (opModeID 150) and running (opModeID 300) by temperature and RVP effects.
-- tempAdjustment[opModeID=300]=
-- (tempAdjustTerm3*power(greatest(temperature,40.0),3)
-- + tempAdjustTerm2*power(greatest(temperature,40.0),2)
-- + tempAdjustTerm1*greatest(temperature,40.0)
-- + tempAdjustConstant).
-- tempAdjustment[opModeID=150]=1.
-- rvpAdjustment[opModeID=300]=1 for temperature < 40F, otherwise for temperature >= 40F:
-- (adjustTerm3*power(temperature,3)
-- + adjustTerm2*power(temperature,2)
-- + adjustTerm1*temperature
-- + adjustConstant).
-- rvpAdjustment[opModeID=150]=1.
-- weightedMeanBaseRate=sum(sourceBinActivityFraction*meanBaseRate)*rvpAdjustment*tempAdjustment.
-- weightedMeanBaseRateIM=sum(sourceBinActivityFraction*meanBaseRateIM)*rvpAdjustment*tempAdjustment.
-- @condition Only Gasoline (1) and Ethanol (5) fuel types, all others have no adjustments in this step.
insert into WeightedMeanBaseRate (polProcessID, sourceTypeID, regClassID, fuelTypeID, monthID, hourDayID, 
	modelYearID, opModeID, weightedMeanBaseRate, weightedMeanBaseRateIM,
	tempAdjustment, rvpAdjustment, unadjustedWeightedMeanBaseRate)
select er.polProcessID, stmy.sourceTypeID, sb.regClassID, sb.fuelTypeID, zmh.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID,
	sum(sourceBinActivityFraction*meanBaseRate)
		* case when (er.opModeID=300 and sb.fuelTypeID in(1,5)) then
			(eta.tempAdjustTerm3*power(greatest(zmh.temperature,40.0),3)
			+ eta.tempAdjustTerm2*power(greatest(zmh.temperature,40.0),2)
			+ eta.tempAdjustTerm1*greatest(zmh.temperature,40.0)
			+ eta.tempAdjustConstant)
			* case when zmh.temperature >= 40.0 then
				(atg.adjustTerm3*power(zmh.temperature,3)
				+ atg.adjustTerm2*power(zmh.temperature,2)
				+ atg.adjustTerm1*zmh.temperature
				+ atg.adjustConstant)
			else 1.0
			end
		else 1.0
		end 
		as weightedMeanBaseRate,
	sum(sourceBinActivityFraction*meanBaseRateIM)
		* case when (er.opModeID=300 and sb.fuelTypeID in (1,5)) then
			(eta.tempAdjustTerm3*power(greatest(zmh.temperature,40.0),3)
			+ eta.tempAdjustTerm2*power(greatest(zmh.temperature,40.0),2)
			+ eta.tempAdjustTerm1*greatest(zmh.temperature,40.0)
			+ eta.tempAdjustConstant)
			* case when zmh.temperature >= 40.0 then
				(atg.adjustTerm3*power(zmh.temperature,3)
				+ atg.adjustTerm2*power(zmh.temperature,2)
				+ atg.adjustTerm1*zmh.temperature
				+ atg.adjustConstant)
			else 1.0
			end
		else 1.0
		end 
		as weightedMeanBaseRateIM,
	case when (er.opModeID=300 and sb.fuelTypeID in (1,5)) then
			(eta.tempAdjustTerm3*power(greatest(zmh.temperature,40.0),3)
			+ eta.tempAdjustTerm2*power(greatest(zmh.temperature,40.0),2)
			+ eta.tempAdjustTerm1*greatest(zmh.temperature,40.0)
			+ eta.tempAdjustConstant)
		else 1.0
		end as tempAdjustment,
	case when (er.opModeID=300 and sb.fuelTypeID in (1,5)) then
			case when zmh.temperature >= 40.0 then
				(atg.adjustTerm3*power(zmh.temperature,3)
				+ atg.adjustTerm2*power(zmh.temperature,2)
				+ atg.adjustTerm1*zmh.temperature
				+ atg.adjustConstant)
			else 1.0
			end
		else 1.0
		end as rvpAdjustment,
	sum(sourceBinActivityFraction*meanBaseRate) as unadjustedWeightedMeanBaseRate
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
inner join RunSpecHourDay rshd
inner join HourDay hd on (hd.hourDayID = rshd.hourDayID)
inner join ZoneMonthHour zmh on (zmh.hourID = hd.hourID)
inner join averageTankGasoline atg on (atg.fuelTypeID = FuelType.fuelTypeID)
inner join evapTemperatureAdjustment eta
where er.polProcessID in (##pollutantProcessIDs##)
and opModeID in (150, 300)
group by er.polProcessID, stmy.sourceTypeID, sb.regClassID, sb.fuelTypeID, zmh.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID
order by null;
-- End Section WithRegClassID

-- Section NoRegClassID
insert into WeightedMeanBaseRate (polProcessID, sourceTypeID, regClassID, fuelTypeID, monthID, hourDayID, 
	modelYearID, opModeID, weightedMeanBaseRate, weightedMeanBaseRateIM,
	tempAdjustment, rvpAdjustment, unadjustedWeightedMeanBaseRate)
select er.polProcessID, stmy.sourceTypeID, 0 as regClassID, sb.fuelTypeID, zmh.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID,
	sum(sourceBinActivityFraction*meanBaseRate)
		* case when (er.opModeID=300 and sb.fuelTypeID in(1,5)) then
			(eta.tempAdjustTerm3*power(greatest(zmh.temperature,40.0),3)
			+ eta.tempAdjustTerm2*power(greatest(zmh.temperature,40.0),2)
			+ eta.tempAdjustTerm1*greatest(zmh.temperature,40.0)
			+ eta.tempAdjustConstant)
			* case when zmh.temperature >= 40.0 then
				(atg.adjustTerm3*power(zmh.temperature,3)
				+ atg.adjustTerm2*power(zmh.temperature,2)
				+ atg.adjustTerm1*zmh.temperature
				+ atg.adjustConstant)
			else 1.0
			end
		else 1.0
		end 
		as weightedMeanBaseRate,
	sum(sourceBinActivityFraction*meanBaseRateIM)
		* case when (er.opModeID=300 and sb.fuelTypeID in (1,5)) then
			(eta.tempAdjustTerm3*power(greatest(zmh.temperature,40.0),3)
			+ eta.tempAdjustTerm2*power(greatest(zmh.temperature,40.0),2)
			+ eta.tempAdjustTerm1*greatest(zmh.temperature,40.0)
			+ eta.tempAdjustConstant)
			* case when zmh.temperature >= 40.0 then
				(atg.adjustTerm3*power(zmh.temperature,3)
				+ atg.adjustTerm2*power(zmh.temperature,2)
				+ atg.adjustTerm1*zmh.temperature
				+ atg.adjustConstant)
			else 1.0
			end
		else 1.0
		end 
		as weightedMeanBaseRateIM,
	case when (er.opModeID=300 and sb.fuelTypeID in (1,5)) then
			(eta.tempAdjustTerm3*power(greatest(zmh.temperature,40.0),3)
			+ eta.tempAdjustTerm2*power(greatest(zmh.temperature,40.0),2)
			+ eta.tempAdjustTerm1*greatest(zmh.temperature,40.0)
			+ eta.tempAdjustConstant)
		else 1.0
		end as tempAdjustment,
	case when (er.opModeID=300 and sb.fuelTypeID in (1,5)) then
			case when zmh.temperature >= 40.0 then
				(atg.adjustTerm3*power(zmh.temperature,3)
				+ atg.adjustTerm2*power(zmh.temperature,2)
				+ atg.adjustTerm1*zmh.temperature
				+ atg.adjustConstant)
			else 1.0
			end
		else 1.0
		end as rvpAdjustment,
	sum(sourceBinActivityFraction*meanBaseRate) as unadjustedWeightedMeanBaseRate
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
inner join RunSpecHourDay rshd
inner join HourDay hd on (hd.hourDayID = rshd.hourDayID)
inner join ZoneMonthHour zmh on (zmh.hourID = hd.hourID)
inner join averageTankGasoline atg on (atg.fuelTypeID = FuelType.fuelTypeID)
inner join evapTemperatureAdjustment eta
where er.polProcessID in (##pollutantProcessIDs##)
and opModeID in (150, 300)
group by er.polProcessID, stmy.sourceTypeID, sb.fuelTypeID, zmh.monthID, rshd.hourDayID, 
	stmy.modelYearID, er.opModeID
order by null;
-- End Section NoRegClassID

analyze table WeightedMeanBaseRate;

alter table MOVESWorkerOutput add emissionQuantIM float null;

-- Section Debug
-- Keep debug information by operating mode
drop table if exists DebugTVVMOVESWorkerOutput;

create table DebugTVVMOVESWorkerOutput like MOVESWorkerOutput;
alter table DebugTVVMOVESWorkerOutput add column opModeID smallint(6) null;

insert into DebugTVVMOVESWorkerOutput (yearID, monthID, dayID, hourID, stateID, countyID,
	zoneID, linkID, pollutantID, processID, sourceTypeID, regClassID, fuelTypeID, modelYearID,
	roadTypeID, SCC, emissionQuant, emissionQuantIM, opModeID)
select ##context.year## as yearID, w.monthID, hd.dayID, hd.hourID,
	##context.iterLocation.stateRecordID## as stateID,
	##context.iterLocation.countyRecordID## as countyID,
	##context.iterLocation.zoneRecordID## as zoneID,
	##context.iterLocation.linkRecordID## as linkID,
	ppa.pollutantID, ppa.processID, w.sourceTypeID, w.regClassID, w.fuelTypeID, w.modelYearID,
	##context.iterLocation.roadTypeRecordID##, null as SCC,
	(weightedMeanBaseRate*sourceHours*opModeFraction) as emissionQuant,
	(weightedMeanBaseRateIM*sourceHours*opModeFraction) as emissionQuantIM,
	omd.opModeID
from WeightedMeanBaseRate w
inner join SourceHours sh on (sh.hourDayID=w.hourDayID and sh.monthID=w.monthID
	and sh.ageID=##context.year##-w.modelYearID
	and sh.sourceTypeID=w.sourceTypeID)
inner join OpModeDistribution omd on (omd.sourceTypeID=sh.sourceTypeID
	and omd.hourDayID=w.hourDayID
	and omd.polProcessID=w.polProcessID and omd.opModeID=w.opModeID)
inner join PollutantProcessAssoc ppa on (ppa.polProcessID=omd.polProcessID)
inner join HourDay hd on (hd.hourDayID=omd.hourDayID);
-- End Section Debug

alter table WeightedMeanBaseRate add key speed1 (sourceTypeID, hourDayID, polProcessID, opModeID);
analyze table WeightedMeanBaseRate;

alter table SourceHours add key speed1 (hourDayID, monthID, sourceTypeID, ageID);
analyze table SourceHours;

-- 
-- TVV-9: Calculate MOVESWorkerOutput by Source Type
--
-- INSERT INTO EventLog (eventTime, eventName) SELECT NOW(), 'TVV-9 without SCC';

-- @algorithm Combine cold soaking (opModeID=151), hot soaking (150), and running (300) evaporative emissions.
-- THC=weightedMeanBaseRate*sourceHours*opModeFraction.
-- THC I/M=weightedMeanBaseRateIM*sourceHours*opModeFraction.
insert into MOVESWorkerOutput (yearID, monthID, dayID, hourID, stateID, countyID,
	zoneID, linkID, pollutantID, processID, sourceTypeID, regClassID, fuelTypeID, modelYearID,
	roadTypeID, SCC, emissionQuant, emissionQuantIM)
select ##context.year## as yearID, w.monthID, hd.dayID, hd.hourID,
	##context.iterLocation.stateRecordID## as stateID,
	##context.iterLocation.countyRecordID## as countyID,
	##context.iterLocation.zoneRecordID## as zoneID,
	##context.iterLocation.linkRecordID## as linkID,
	ppa.pollutantID, ppa.processID, w.sourceTypeID, w.regClassID, w.fuelTypeID, w.modelYearID,
	##context.iterLocation.roadTypeRecordID##, null as SCC,
	(weightedMeanBaseRate*sourceHours*opModeFraction) as emissionQuant,
	(weightedMeanBaseRateIM*sourceHours*opModeFraction) as emissionQuantIM
from WeightedMeanBaseRate w
inner join SourceHours sh on (sh.hourDayID=w.hourDayID and sh.monthID=w.monthID
	and sh.ageID=##context.year##-w.modelYearID
	and sh.sourceTypeID=w.sourceTypeID)
inner join OpModeDistribution omd on (omd.sourceTypeID=sh.sourceTypeID
	and omd.hourDayID=w.hourDayID
	and omd.polProcessID=w.polProcessID and omd.opModeID=w.opModeID)
inner join PollutantProcessAssoc ppa on (ppa.polProcessID=omd.polProcessID)
inner join HourDay hd on (hd.hourDayID=omd.hourDayID);

-- @algorithm Apply I/M programs to the aggregat combination of cold soak, hot soaking, and running evap.
-- THC=THC I/M*IMAdjustFract + THC*(1-IMAdjustFract).
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
drop table if exists DebugTankVaporGenerated;
drop table if exists TankVaporGeneratedHighAndLow;
drop table if exists TankVaporGenerated;
drop table if exists EthanolWeightedTVG;
drop table if exists CummulativeTankVaporVented;
drop table if exists UnWeightedHourlyTVV;
drop table if exists HourlyTVV;
drop table if exists WeightedMeanBaseRate;
drop table if exists IMCoverageMergedUngrouped;
drop table if exists CopyOfHourlyTVV;
drop table if exists TVG;
drop table if exists evapRVPTemperatureAdjustmentSummary;
drop table if exists tempATG;
drop table if exists HourlyTVVTemp;

drop table if exists tvgSumIH;
drop table if exists tvgSumI24;
drop table if exists tvgSum1H;
drop table if exists tvgSumH24;

drop table if exists DebugTVVMOVESWorkerOutput;
-- End Section Cleanup
