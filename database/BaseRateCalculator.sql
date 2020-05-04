-- Directly calculate rates.
-- Version 2015-01-01
-- Author Wesley Faler

-- @calculator

-- Section Create Remote Tables for Extracted Data

##create.AgeCategory##;
TRUNCATE AgeCategory;

##create.BaseRateByAge##;
TRUNCATE BaseRateByAge;

##create.BaseRate##;
TRUNCATE BaseRate;

##create.County##;
TRUNCATE TABLE County;

CREATE TABLE EmissionRateAdjustmentWorker
(	
	polProcessID			int(11)			not null,
	sourceTypeID			smallint(6)		not null,
	regClassID				smallint(6)		not null,
	fuelTypeID				smallint(6)		not null,
	modelYearID				smallint(6)		not null,
	EmissionRateAdjustment	double null default NULL,
	primary key (polProcessID, sourceTypeID, fuelTypeID, regClassID, modelYearID)
);
TRUNCATE EmissionRateAdjustmentWorker;

##create.FuelType##;
TRUNCATE TABLE FuelType;

create table if not exists LocalFuelSupply (
	fuelTypeID smallint not null,
	fuelSubtypeID smallint not null,
	fuelFormulationID smallint not null,
	marketShare double not null,
	fuelSubtypePetroleumFraction double null,
	fuelSubtypeFossilFraction double null,
	primary key (fuelFormulationID),
	key (fuelTypeID, fuelSubtypeID, fuelFormulationID, marketShare),
	key (fuelFormulationID)
);
TRUNCATE TABLE LocalFuelSupply;

##create.generalFuelRatio##;
TRUNCATE TABLE generalFuelRatio;

##create.IMCoverage##;
TRUNCATE IMCoverage;

##create.IMFactor##;
TRUNCATE IMFactor;

##create.PollutantProcessAssoc##;
TRUNCATE PollutantProcessAssoc;

##create.PollutantProcessMappedModelYear##;
TRUNCATE PollutantProcessMappedModelYear;

##create.RunspecModelYearAge##;
TRUNCATE TABLE RunspecModelYearAge;

##create.RunSpecSourceFuelType##;
TRUNCATE TABLE RunSpecSourceFuelType;

-- Section Process1_2
##create.criteriaRatio##;
TRUNCATE criteriaRatio;

##create.altCriteriaRatio##;
TRUNCATE altCriteriaRatio;
-- End Section Process1_2

-- Section Process2
##create.StartTempAdjustment##;
TRUNCATE TABLE StartTempAdjustment;
-- End Section Process2

##create.TemperatureAdjustment##;
TRUNCATE TABLE TemperatureAdjustment;

-- Section GetActivity
create table if not exists universalActivity (
	hourDayID smallint not null,
	modelYearID smallint not null,
	sourceTypeID smallint not null,
	activity double,
	primary key (sourceTypeID, hourDayID, modelYearID),
	key (hourDayID, modelYearID)
);

truncate table universalActivity;

##create.RunSpecHourDay##;
TRUNCATE TABLE RunSpecHourDay;

##create.RunSpecSourceType##;
TRUNCATE TABLE RunSpecSourceType;

-- End Section GetActivity

##create.ZoneMonthHour##;
TRUNCATE ZoneMonthHour;

create table if not exists zoneACFactor (
	hourID smallint(6) NOT NULL DEFAULT 0,
	sourceTypeID smallint(6) NOT NULL DEFAULT 0,
	modelYearID smallint(6) NOT NULL DEFAULT 0,
	ACFactor double NOT NULL DEFAULT 0,
	primary key (hourID, sourceTypeID, modelYearID)
);
TRUNCATE zoneACFactor;

-- Section AggregateSMFR
create table if not exists smfrSBDSummary (
	sourceTypeID smallint not null,
	modelYearID smallint not null,
	fuelTypeID smallint not null,
	regClassID smallint not null,
	sbdTotal double not null,
	primary key (sourceTypeID, modelYearID, fuelTypeID, regClassID),
	key (modelYearID, sourceTypeID, fuelTypeID, regClassID)
);

truncate smfrSBDSummary;
-- End Section AggregateSMFR

-- Section Process91
-- Section AdjustAPUEmissionRate
create table if not exists apuEmissionRateFraction (
	modelYearID smallint not null,
	hourFractionAdjust double not null,
	primary key (modelYearID)
);
-- End Section AdjustAPUEmissionRate
-- End Section Process91


-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

-- @algorithm Create EmissionRateAdjustment by modelyear.
-- @output EmissionRateAdjustmentWorker
cache SELECT 
	era.polProcessID,
	sourceTypeID,
	regClassID,
	fuelTypeID,
	modelYearID,
	emissionRateAdjustment
INTO OUTFILE '##EmissionRateAdjustmentWorker##'
FROM EmissionRateAdjustment era 
INNER JOIN pollutantprocessassoc USING (polprocessID)
INNER JOIN modelyear
WHERE processID = ##context.iterProcess.databaseKey##
AND pollutantID in (##pollutantIDs##)
AND modelYearID >= ##context.year##-30
AND modelYearID <= ##context.year##
AND endmodelYearID >= modelYearID
AND beginmodelYearID <= modelYearID;

-- Section Inventory

cache(linkID=##context.iterLocation.linkRecordID##) SELECT br.* INTO OUTFILE '##BaseRateByAge##'
FROM BaseRateByAge_##context.iterProcess.databaseKey##_##context.year## br
INNER JOIN ageCategory ac on (ac.ageGroupID = br.ageGroupID)
WHERE processID = ##context.iterProcess.databaseKey##
AND pollutantID in (##pollutantIDs##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND ageID = ##context.year## - modelYearID
AND roadTypeID = ##context.iterLocation.roadTypeRecordID##;

cache(linkID=##context.iterLocation.linkRecordID##) SELECT * INTO OUTFILE '##BaseRate##'
FROM BaseRate_##context.iterProcess.databaseKey##_##context.year## br
WHERE processID = ##context.iterProcess.databaseKey##
AND pollutantID in (##pollutantIDs##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND roadTypeID = ##context.iterLocation.roadTypeRecordID##;
-- End Section Inventory

-- Section Rates
-- Section NotProject
cache(linkID=##context.iterLocation.linkRecordID##) SELECT br.* INTO OUTFILE '##BaseRateByAge##'
FROM BaseRateByAge_##context.iterProcess.databaseKey##_##context.year## br
INNER JOIN ageCategory ac on (ac.ageGroupID = br.ageGroupID)
WHERE processID = ##context.iterProcess.databaseKey##
AND pollutantID in (##pollutantIDs##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND ageID = ##context.year## - modelYearID
AND roadTypeID = ##context.iterLocation.roadTypeRecordID##
AND avgSpeedBinID = mod(##context.iterLocation.linkRecordID##,100);

cache(linkID=##context.iterLocation.linkRecordID##) SELECT * INTO OUTFILE '##BaseRate##'
FROM BaseRate_##context.iterProcess.databaseKey##_##context.year## br
WHERE processID = ##context.iterProcess.databaseKey##
AND pollutantID in (##pollutantIDs##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND roadTypeID = ##context.iterLocation.roadTypeRecordID##
AND avgSpeedBinID = mod(##context.iterLocation.linkRecordID##,100);
-- End Section NotProject

-- Section Project
cache(linkID=##context.iterLocation.linkRecordID##) SELECT br.* INTO OUTFILE '##BaseRateByAge##'
FROM BaseRateByAge_##context.iterProcess.databaseKey##_##context.year## br
INNER JOIN ageCategory ac on (ac.ageGroupID = br.ageGroupID)
WHERE processID = ##context.iterProcess.databaseKey##
AND pollutantID in (##pollutantIDs##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND ageID = ##context.year## - modelYearID
AND roadTypeID = ##context.iterLocation.roadTypeRecordID##;

cache(linkID=##context.iterLocation.linkRecordID##) SELECT * INTO OUTFILE '##BaseRate##'
FROM BaseRate_##context.iterProcess.databaseKey##_##context.year## br
WHERE processID = ##context.iterProcess.databaseKey##
AND pollutantID in (##pollutantIDs##)
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND roadTypeID = ##context.iterLocation.roadTypeRecordID##;
-- End Section Project
-- End Section Rates

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

-- Section Process1_2
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
INTO OUTFILE '##altCriteriaRatio##'
FROM altCriteriaRatio
WHERE polProcessID IN (##pollutantProcessIDs##)
AND modelYearID = MYMAP(##context.year## - ageID);
-- End Section Process1_2

cache SELECT * INTO OUTFILE '##FuelType##'
FROM FuelType
WHERE fuelTypeID in (##macro.csv.all.fuelTypeID##);

cache select fst.fuelTypeID, fst.fuelSubTypeID, ff.fuelFormulationID, fs.marketShare,
	fst.fuelSubtypePetroleumFraction, fst.fuelSubtypeFossilFraction
into outfile '##LocalFuelSupply##'
from year
inner join fuelSupply fs on (fs.fuelYearID=year.fuelYearID)
inner join monthOfAnyYear moay on (moay.monthGroupID=fs.monthGroupID)
inner join fuelFormulation ff on (ff.fuelFormulationID=fs.fuelFormulationID)
inner join fuelSubtype fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)
where yearID = ##context.year##
and fs.fuelRegionID = ##context.fuelRegionID##
and moay.monthID = ##context.monthID##
and fst.fuelTypeID in (##macro.csv.all.fuelTypeID##);

cache select gfr.* into outfile '##generalFuelRatio##'
from generalFuelRatio gfr
where polProcessID in (##pollutantProcessIDs##)
and minModelYearID <= ##context.year##
and maxModelYearID >= ##context.year##-30
and fuelFormulationID in (
	select ff.fuelFormulationID
	from year
	inner join fuelSupply fs on (fs.fuelYearID=year.fuelYearID)
	inner join monthOfAnyYear moay on (moay.monthGroupID=fs.monthGroupID)
	inner join fuelFormulation ff on (ff.fuelFormulationID=fs.fuelFormulationID)
	inner join fuelSubtype fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)
	where yearID = ##context.year##
	and fs.fuelRegionID = ##context.fuelRegionID##
	and moay.monthID = ##context.monthID##
	and fst.fuelTypeID in (##macro.csv.all.fuelTypeID##)
);

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

cache SELECT * INTO OUTFILE '##PollutantProcessMappedModelYear##'
FROM PollutantProcessMappedModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND polProcessID IN (##pollutantProcessIDs##);

cache SELECT * INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE processID=##context.iterProcess.databaseKey##
AND polProcessID IN (##pollutantProcessIDs##);

cache select * into outfile '##RunspecModelYearAge##'
from RunspecModelYearAge
where modelYearID <= ##context.year##
and modelYearID >= ##context.year## - 30
and yearID = ##context.year##;

cache select * into outfile '##RunSpecSourceFuelType##'
from RunSpecSourceFuelType;

-- Section Process2
cache SELECT StartTempAdjustment.* INTO OUTFILE '##StartTempAdjustment##'
FROM StartTempAdjustment
WHERE polProcessID IN (##pollutantProcessIDs##)
AND fuelTypeID in (##macro.csv.all.fuelTypeID##);
-- End Section Process2

cache SELECT TemperatureAdjustment.* INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
WHERE polProcessID IN (##pollutantProcessIDs##)
AND fuelTypeID in (##macro.csv.all.fuelTypeID##);

SELECT ZoneMonthHour.* INTO OUTFILE '##ZoneMonthHour##'
FROM ZoneMonthHour
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND ZoneMonthHour.monthID = ##context.monthID##
AND ZoneMonthHour.hourID in (##macro.csv.all.hourID##);

-- @algorithm ACFactor[hourID,sourceTypeID,modelYearID]=LEAST(GREATEST(ACActivityTermA+heatIndex*(ACActivityTermB+ACActivityTermC*heatIndex),0),1.0)*ACPenetrationFraction*functioningACFraction.
-- @output zoneACFactor
-- @input ZoneMonthHour
-- @input SourceTypeAge
cache SELECT zmh.hourID, sta.sourceTypeID, modelYearID, 
	LEAST(GREATEST(ACActivityTermA+heatIndex*(ACActivityTermB+ACActivityTermC*heatIndex),0),1.0)*ACPenetrationFraction*functioningACFraction as ACFactor
	INTO OUTFILE '##zoneACFactor##'
FROM ZoneMonthHour zmh
INNER JOIN MonthOfAnyYear may ON (may.monthID = zmh.monthID)
INNER JOIN MonthGroupHour mgh ON (mgh.monthGroupID = may.monthGroupID AND mgh.hourID = zmh.hourID)
INNER JOIN SourceTypeModelYear stmy
INNER JOIN SourceTypeAge sta ON (
	sta.sourceTypeID = stmy.sourceTypeID AND
	sta.ageID = ##context.year## - stmy.modelYearID)
WHERE zmh.zoneID = ##context.iterLocation.zoneRecordID##
and zmh.monthID = ##context.monthID##
and sta.sourceTypeID in (##macro.csv.all.sourceTypeID##);

-- Section GetActivity
-- Extract activity at the Month context.

-- Section Process1_9_10

-- @algorithm activity=SHO
-- @condition Running Exhaust, Brakewear, Tirewear
select SHO.hourDayID, ##context.year##-ageID as modelYearID, sourceTypeID, 
	SHO as activity
	into outfile '##universalActivity##'
from SHO
inner join RunSpecHourDay using (hourDayID)
where monthID = ##context.monthID##
and yearID = ##context.year##
and linkID = ##context.iterLocation.linkRecordID##;
-- End Section Process1_9_10

-- Section Process2

-- @algorithm activity=starts
-- @condition Starts
select Starts.hourDayID, ##context.year##-ageID as modelYearID, sourceTypeID, starts as activity
	into outfile '##universalActivity##'
from Starts
inner join RunSpecHourDay using (hourDayID)
where monthID = ##context.monthID##
and yearID = ##context.year##
and zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section Process2

-- Section Process90

-- @algorithm activity=extendedIdleHours
-- @condition Extended Idle Exhaust
select ExtendedIdleHours.hourDayID, ##context.year##-ageID as modelYearID, sourceTypeID, extendedIdleHours as activity
	into outfile '##universalActivity##'
from ExtendedIdleHours
inner join RunSpecHourDay using (hourDayID)
where monthID = ##context.monthID##
and yearID = ##context.year##
and zoneID = ##context.iterLocation.zoneRecordID##
and sourceTypeID=62;
-- End Section Process90

-- Section Process91

-- @algorithm activity=hotellingHours
-- @condition Auxiliary Power Exhaust
select HotellingHours.hourDayID, ##context.year##-ageID as modelYearID, sourceTypeID, hotellingHours as activity
	into outfile '##universalActivity##'
from HotellingHours
inner join RunSpecHourDay using (hourDayID)
where monthID = ##context.monthID##
and yearID = ##context.year##
and zoneID = ##context.iterLocation.zoneRecordID##
and sourceTypeID=62;

-- Section AdjustAPUEmissionRate

-- @algorithm hourFractionAdjust=1.0/opModeFraction[opModeID=201].
-- @input hotellingActivityDistribution
-- @output apuEmissionRateFraction
-- @condition Auxiliary Power Exhaust
cache select modelYearID, case when opModeFraction <= 0 then 0.0 else (1.0/opModeFraction) end as hourFractionAdjust
	into outfile '##apuEmissionRateFraction##'
from hotellingActivityDistribution
inner join RunspecModelYearAge on (
	beginModelYearID <= modelYearID
	and endModelYearID >= modelYearID)
where modelYearID <= ##context.year##
and modelYearID >= ##context.year## - 30
and yearID = ##context.year##
and opModeID = 201;

-- End Section AdjustAPUEmissionRate
-- End Section Process91

cache select * into outfile '##RunSpecHourDay##'
from RunSpecHourDay;

cache select * into outfile '##RunSpecSourceType##'
from RunSpecSourceType;

-- End Section GetActivity

-- Section AggregateSMFR
cache select round(SourceBinDistribution.sourceTypeModelYearID/10000,0) as sourceTypeID,
	mod(SourceBinDistribution.sourceTypeModelYearID,10000) as modelYearID,
	SourceBin.fuelTypeID, regClassID,
	sum(sourceBinActivityFraction) as sbdTotal
into outfile '##smfrSBDSummary##'
from sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##sbdPolProcessID##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID
group by SourceBinDistribution.sourceTypeModelYearID, SourceBin.fuelTypeID, regClassID
order by null;
-- End Section AggregateSMFR

-- End Section Extract Data

-- Section Processing

alter table FuelType add key speed1 (fuelTypeID, humidityCorrectionCoeff);
analyze table FuelType;

alter table ZoneMonthHour add key speed1 (hourID, monthID, zoneID, temperature, specificHumidity, heatIndex);
analyze table ZoneMonthHour;

DROP TABLE IF EXISTS IMCoverageMergedUngrouped;
CREATE TABLE IMCoverageMergedUngrouped (
	processID SMALLINT NOT NULL,
	pollutantID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	fuelTypeID SMALLINT NOT NULL,
	sourceTypeID SMALLINT NOT NULL,
	IMAdjustFract FLOAT,
	key (processID,pollutantID,modelYearID,fuelTypeID,sourceTypeID)
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

-- Add columns
drop table if exists tempBaseRateOutput;
create table tempBaseRateOutput like BaseRateOutput;
alter table tempBaseRateOutput add column meanBaseRateIM float default '0';
alter table tempBaseRateOutput add column emissionRateIM float default '0';

alter table tempBaseRateOutput add column opModeID smallint not null default '0';
alter table tempBaseRateOutput add column generalFraction float not null default '0';
alter table tempBaseRateOutput add column generalFractionRate float not null default '0';

alter table tempBaseRateOutput add column meanBaseRateACAdj float default '0';
alter table tempBaseRateOutput add column meanBaseRateIMACAdj float default '0';
alter table tempBaseRateOutput add column emissionRateACAdj float default '0';
alter table tempBaseRateOutput add column emissionRateIMACAdj float default '0';

-- @algorithm Add age-based rates
insert into tempBaseRateOutput (movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, pollutantID, processID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	regClassID,
	opModeID, generalFraction, generalFractionRate,
	meanBaseRate, meanBaseRateIM,
	emissionRate, emissionRateIM,
	meanBaseRateACAdj, meanBaseRateIMACAdj,
	emissionRateACAdj, emissionRateIMACAdj)
select 0, 0,
	##context.iterLocation.zoneRecordID##, 
	br.sourceTypeID, br.roadTypeID, br.avgSpeedBinID, br.hourDayID, br.pollutantID, br.processID,
	br.modelYearID, 
	##context.year##, ##context.monthID##,
	fuelTypeID,
	br.regClassID,
	br.opModeID, br.opModeFraction, br.opModeFractionRate,
	br.meanBaseRate, br.meanBaseRateIM,
	br.emissionRate, br.emissionRateIM,
	br.meanBaseRateACAdj, br.meanBaseRateIMACAdj,
	br.emissionRateACAdj, br.emissionRateIMACAdj
from BaseRateByAge br
inner join AgeCategory ac on (ac.ageGroupID = br.ageGroupID)
inner join RunspecModelYearAge mya on (
	mya.yearID = ##context.year##
	and mya.modelYearID = br.modelYearID
	and mya.ageID = ac.ageID);

-- @algorithm Add rates that don't depend upon age
insert into tempBaseRateOutput (movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, pollutantID, processID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	regClassID,
	opModeID, generalFraction, generalFractionRate,
	meanBaseRate, meanBaseRateIM,
	emissionRate, emissionRateIM,
	meanBaseRateACAdj, meanBaseRateIMACAdj,
	emissionRateACAdj, emissionRateIMACAdj)
select 0, 0,
	##context.iterLocation.zoneRecordID##, 
	br.sourceTypeID, br.roadTypeID, br.avgSpeedBinID, br.hourDayID, br.pollutantID, br.processID,
	br.modelYearID, 
	##context.year##, ##context.monthID##,
	br.fuelTypeID,
	br.regClassID,
	br.opModeID, br.opModeFraction, br.opModeFractionRate,
	br.meanBaseRate, br.meanBaseRateIM,
	br.emissionRate, br.emissionRateIM,
	br.meanBaseRateACAdj, br.meanBaseRateIMACAdj,
	br.emissionRateACAdj, br.emissionRateIMACAdj
from BaseRate br;

-- Section AdjustAPUEmissionRate
insert ignore into apuEmissionRateFraction (modelYearID, hourFractionAdjust)
select modelYearID, 0 from RunspecModelYearAge;

-- @algorithm APU hourly rates have already been scaled by the APU operating mode (201) fraction.
-- This works well for inventory, but not so for rate by APU operating hour.
-- To compensate, emission rates must be divided by the opModeFraction for opMode 201.
-- hourFractionAdjust is 1/opModeFraction with a safeguard for a fraction of 0.
-- @condition APU process hourly rates
update tempBaseRateOutput, apuEmissionRateFraction
	set emissionRate = emissionRate * hourFractionAdjust,
	emissionRateIM = emissionRateIM * hourFractionAdjust,
	emissionRateACAdj = emissionRateACAdj * hourFractionAdjust,
	emissionRateIMACAdj = emissionRateIMACAdj * hourFractionAdjust
where apuEmissionRateFraction.modelYearID = tempBaseRateOutput.modelYearID
and tempBaseRateOutput.processID = 91;
-- End Section AdjustAPUEmissionRate

-- Add fuel formulation, hour, and polProcess
drop table if exists BaseRateOutputWithFuel;
create table BaseRateOutputWithFuel like BaseRateOutput;
alter table BaseRateOutputWithFuel add column meanBaseRateIM float default '0';
alter table BaseRateOutputWithFuel add column emissionRateIM float default '0';
alter table BaseRateOutputWithFuel add column opModeID smallint not null default '0';
alter table BaseRateOutputWithFuel add column generalFraction float not null default '0';
alter table BaseRateOutputWithFuel add column generalFractionRate float not null default '0';
alter table BaseRateOutputWithFuel add column fuelFormulationID smallint not null default '0';
alter table BaseRateOutputWithFuel add column fuelSubtypeID smallint not null default '0';
alter table BaseRateOutputWithFuel add column polProcessID int not null default '0';
alter table BaseRateOutputWithFuel add column fuelMarketShare double not null default '0';
alter table BaseRateOutputWithFuel add column hourID smallint not null default '0';
alter table BaseRateOutputWithFuel add column dayID smallint not null default '0';

alter table BaseRateOutputWithFuel add column meanBaseRateACAdj float default '0';
alter table BaseRateOutputWithFuel add column meanBaseRateIMACAdj float default '0';
alter table BaseRateOutputWithFuel add column emissionRateACAdj float default '0';
alter table BaseRateOutputWithFuel add column emissionRateIMACAdj float default '0';

analyze table BaseRateOutputWithFuel;

-- @algorithm Obtain fuel market share.
insert into BaseRateOutputWithFuel (movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, hourID, dayID,
	pollutantID, processID,
	polProcessID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	fuelSubtypeID,
	fuelFormulationID,
	fuelMarketShare,
	regClassID,
	opModeID, generalFraction, generalFractionRate,
	meanBaseRate, meanBaseRateIM,
	emissionRate, emissionRateIM,
	meanBaseRateACAdj, meanBaseRateIMACAdj,
	emissionRateACAdj, emissionRateIMACAdj)
select
	movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, floor(hourDayID/10) as hourID, mod(hourDayID,10) as dayID,
	pollutantID, processID,
	(pollutantID * 100 + processID) as polProcessID,
	modelYearID, 
	yearID, monthID,
	fs.fuelTypeID,
	fs.fuelSubtypeID,
	fs.fuelFormulationID,
	fs.marketShare as fuelMarketShare,
	regClassID,
	opModeID, generalFraction, generalFractionRate,
	meanBaseRate, meanBaseRateIM,
	emissionRate, emissionRateIM,
	meanBaseRateACAdj, meanBaseRateIMACAdj,
	emissionRateACAdj, emissionRateIMACAdj
from tempBaseRateOutput tbro
inner join LocalFuelSupply fs on (fs.fuelTypeID=tbro.fuelTypeID);

-- create table step1 select * from BaseRateOutputWithFuel;

-- Section Process2
alter table BaseRateOutputWithFuel add column temperature float null;
alter table BaseRateOutputWithFuel add column specificHumidity float null;
alter table BaseRateOutputWithFuel add column K float null;
alter table BaseRateOutputWithFuel add column heatIndex float null;

-- Note: Uncomment the following line to disable starts additive temperature adjustment.
-- update BaseRateOutputWithFuel set generalFraction = 0, generalFractionRate = 0;

-- @algorithm Calculate humidity adjustment factor K.
-- K = 1.0 - ((greatest(21.0, least(specificHumidity, 124.0))) - 75.0) * humidityCorrectionCoeff
-- @condition Start Exhaust (2).
update BaseRateOutputWithFuel, ZoneMonthHour, FuelType
set BaseRateOutputWithFuel.temperature = ZoneMonthHour.temperature,
	BaseRateOutputWithFuel.specificHumidity = ZoneMonthHour.specificHumidity,
	BaseRateOutputWithFuel.K = 1.0 - ((greatest(21.0, least(ZoneMonthHour.specificHumidity, 124.0))) - 75.0) * FuelType.humidityCorrectionCoeff,
	BaseRateOutputWithFuel.heatIndex = ZoneMonthHour.heatIndex
where BaseRateOutputWithFuel.zoneID = ZoneMonthHour.zoneID
and BaseRateOutputWithFuel.monthID = ZoneMonthHour.monthID
and BaseRateOutputWithFuel.hourID = ZoneMonthHour.hourID
and BaseRateOutputWithFuel.fuelTypeID = FuelType.fuelTypeID;

-- @algorithm Do Start Temperature adjustments by opModeID. PM uses multiplicative factors.
-- Everything else uses additive factors.
-- The additive part needs to be weighted by opModeFraction (stored in generalFraction).  Being a rate, sourceBinActivityFraction
-- is not required for the weighting since activity would have been weighted similarly.
-- For polProcessIDs (11202,11802): rate = rate*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC.
-- For all other polProcessIDs with startTempEquationType of 'LOG': rate = rate + generalFraction * (tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC).
-- For all other polProcessIDs with startTempEquationType of 'POLY': rate = rate + generalFraction * ((LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))).
-- @condition Start Exhaust (2) process.
update BaseRateOutputWithFuel, StartTempAdjustment, PollutantProcessMappedModelYear
set
	meanBaseRate   = 
		CASE WHEN BaseRateOutputWithFuel.polProcessID in (11202,11802) THEN
			meanBaseRate*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
		ELSE
			meanBaseRate   + generalFraction * 
			CASE WHEN startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
			WHEN startTempEquationType = 'POLY' THEN
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
			ELSE
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
			END
		END,
	meanBaseRateIM = 
		CASE WHEN BaseRateOutputWithFuel.polProcessID in (11202,11802) THEN
			meanBaseRateIM*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
		ELSE
			meanBaseRateIM + generalFraction * 
			CASE WHEN startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
			WHEN startTempEquationType = 'POLY' THEN
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
			ELSE
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
			END
		END,
	emissionRate   = 
		CASE WHEN BaseRateOutputWithFuel.polProcessID in (11202,11802) THEN
			emissionRate*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
		ELSE
			emissionRate   + generalFractionRate * 
			CASE WHEN startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
			WHEN startTempEquationType = 'POLY' THEN
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
			ELSE
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
			END
		END,
	emissionRateIM = 
		CASE WHEN BaseRateOutputWithFuel.polProcessID in (11202,11802) THEN
			emissionRateIM*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
		ELSE
			emissionRateIM + generalFractionRate * 
			CASE WHEN startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
			WHEN startTempEquationType = 'POLY' THEN
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
			ELSE
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
			END
		END,
	meanBaseRateACAdj   = 
		CASE WHEN BaseRateOutputWithFuel.polProcessID in (11202,11802) THEN
			meanBaseRateACAdj*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
		ELSE
			meanBaseRateACAdj   + generalFraction * 
			CASE WHEN startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
			WHEN startTempEquationType = 'POLY' THEN
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
			ELSE
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
			END
		END,
	meanBaseRateIMACAdj = 
		CASE WHEN BaseRateOutputWithFuel.polProcessID in (11202,11802) THEN
			meanBaseRateIMACAdj*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
		ELSE
			meanBaseRateIMACAdj + generalFraction * 
			CASE WHEN startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
			WHEN startTempEquationType = 'POLY' THEN
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
			ELSE
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
			END
		END,
	emissionRateACAdj   = 
		CASE WHEN BaseRateOutputWithFuel.polProcessID in (11202,11802) THEN
			emissionRateACAdj*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
		ELSE
			emissionRateACAdj   + generalFractionRate * 
			CASE WHEN startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
			WHEN startTempEquationType = 'POLY' THEN
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
			ELSE
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
			END
		END,
	emissionRateIMACAdj = 
		CASE WHEN BaseRateOutputWithFuel.polProcessID in (11202,11802) THEN
			emissionRateIMACAdj*tempAdjustTermB*exp(tempAdjustTermA*(72.0-least(temperature,72)))+tempAdjustTermC
		ELSE
			emissionRateIMACAdj + generalFractionRate * 
			CASE WHEN startTempEquationType = 'LOG' THEN
				(tempAdjustTermB*exp(tempAdjustTermA*(LEAST(temperature,75)-75))+ tempAdjustTermC)	   
			WHEN startTempEquationType = 'POLY' THEN
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC)) 
			ELSE
				(LEAST(temperature,75)-75) * (tempAdjustTermA+(LEAST(temperature,75)-75) * (tempAdjustTermB+(LEAST(temperature,75)-75) * tempAdjustTermC))
			END
		END
where BaseRateOutputWithFuel.polProcessID=StartTempAdjustment.polProcessID
and BaseRateOutputWithFuel.fuelTypeID=StartTempAdjustment.fuelTypeID
and BaseRateOutputWithFuel.opModeID=StartTempAdjustment.opModeID
and BaseRateOutputWithFuel.modelYearID=PollutantProcessMappedModelYear.modelYearID
and StartTempAdjustment.polProcessID=PollutantProcessMappedModelYear.polProcessID
and StartTempAdjustment.modelYearGroupID=PollutantProcessMappedModelYear.modelYearGroupID;
-- End Section Process2

-- @algorithm Apply the County's GPAFract to the general fuel adjustment.
-- fuelEffectRatio=ifnull(fuelEffectRatio,1)+GPAFract*(ifnull(fuelEffectRatioGPA,1)-ifnull(fuelEffectRatio,1))
update GeneralFuelRatio, County
set fuelEffectRatio=ifnull(fuelEffectRatio,1)+GPAFract*(ifnull(fuelEffectRatioGPA,1)-ifnull(fuelEffectRatio,1));

-- Apply GeneralFuelRatio to BaseRateOutputWithFuel
alter table generalFuelRatio add key (fuelTypeID, fuelFormulationID, sourceTypeID, polProcessID);

-- @algorithm Apply GeneralFuelRatio to BaseRateOutputWithFuel. rate = rate * fuelEffectRatio.
-- fuelEffectRatio is the GPA-weighted GeneralFuelRatio.
update BaseRateOutputWithFuel, GeneralFuelRatio
set meanBaseRate=meanBaseRate*fuelEffectRatio, meanBaseRateIM=meanBaseRateIM*fuelEffectRatio,
	emissionRate=emissionRate*fuelEffectRatio, emissionRateIM=emissionRateIM*fuelEffectRatio,
	meanBaseRateACAdj=meanBaseRateACAdj*fuelEffectRatio, meanBaseRateIMACAdj=meanBaseRateIMACAdj*fuelEffectRatio,
	emissionRateACAdj=emissionRateACAdj*fuelEffectRatio, emissionRateIMACAdj=emissionRateIMACAdj*fuelEffectRatio
where GeneralFuelRatio.fuelTypeID = BaseRateOutputWithFuel.fuelTypeID
and GeneralFuelRatio.fuelFormulationID = BaseRateOutputWithFuel.fuelFormulationID
and GeneralFuelRatio.polProcessID = BaseRateOutputWithFuel.polProcessID
and GeneralFuelRatio.minModelYearID <= BaseRateOutputWithFuel.modelYearID
and GeneralFuelRatio.maxModelYearID >= BaseRateOutputWithFuel.modelYearID
and GeneralFuelRatio.minAgeID <= BaseRateOutputWithFuel.yearID - BaseRateOutputWithFuel.modelYearID
and GeneralFuelRatio.maxAgeID >= BaseRateOutputWithFuel.yearID - BaseRateOutputWithFuel.modelYearID
and GeneralFuelRatio.sourceTypeID = BaseRateOutputWithFuel.sourceTypeID;

-- create table step2 select * from BaseRateOutputWithFuel;

-- Section Process1_2
-- @algorithm Apply the County's GPAFract to the criteriaRatio fuel adjustment.
-- criteria ratio = ifnull(ratio,1)+GPAFract*(ifnull(ratioGPA,1)-ifnull(ratio,1))
-- @condition Running Exhaust (1) and Start Exhaust (2).
update CriteriaRatio, County
set ratio=ifnull(ratio,1)+GPAFract*(ifnull(ratioGPA,1)-ifnull(ratio,1));

alter table CriteriaRatio add key speed1 (fuelTypeID,fuelFormulationID,polProcessID,sourceTypeID,modelYearID,ageID);
analyze table CriteriaRatio;

-- @algorithm Apply CriteriaRatio to BaseRateOutputWithFuel. 
-- rate = rate * criteria ratio[fuelTypeID,fuelFormulationID,polProcessID,sourceTypeID,modelYearID,ageID]
-- @condition Running Exhaust (1) and Start Exhaust (2).
update BaseRateOutputWithFuel, CriteriaRatio
set meanBaseRate=meanBaseRate*ratio, meanBaseRateIM=meanBaseRateIM*ratio,
	emissionRate=emissionRate*ratio, emissionRateIM=emissionRateIM*ratio,
	meanBaseRateACAdj=meanBaseRateACAdj*ratio, meanBaseRateIMACAdj=meanBaseRateIMACAdj*ratio,
	emissionRateACAdj=emissionRateACAdj*ratio, emissionRateIMACAdj=emissionRateIMACAdj*ratio
where CriteriaRatio.fuelTypeID = BaseRateOutputWithFuel.fuelTypeID
and CriteriaRatio.fuelFormulationID = BaseRateOutputWithFuel.fuelFormulationID
and CriteriaRatio.polProcessID = BaseRateOutputWithFuel.polProcessID
and CriteriaRatio.sourceTypeID = BaseRateOutputWithFuel.sourceTypeID
and CriteriaRatio.modelYearID = BaseRateOutputWithFuel.modelYearID
and CriteriaRatio.ageID = BaseRateOutputWithFuel.yearID - BaseRateOutputWithFuel.modelYearID;

-- create table step3 select * from BaseRateOutputWithFuel;

-- End Section Process1_2

-- Apply temperature effects
-- Section NotProcess2
alter table BaseRateOutputWithFuel add column temperature float null;
alter table BaseRateOutputWithFuel add column specificHumidity float null;
alter table BaseRateOutputWithFuel add column K float null;
alter table BaseRateOutputWithFuel add column heatIndex float null;

-- @algorithm Calculate humidity adjustment factor K.
-- K = 1.0 - ((greatest(21.0, least(specificHumidity, 124.0))) - 75.0) * humidityCorrectionCoeff
-- @condition Not Start Exhaust (2).
update BaseRateOutputWithFuel, ZoneMonthHour, FuelType
set BaseRateOutputWithFuel.temperature = ZoneMonthHour.temperature,
	BaseRateOutputWithFuel.specificHumidity = ZoneMonthHour.specificHumidity,
	BaseRateOutputWithFuel.K = 1.0 - ((greatest(21.0, least(ZoneMonthHour.specificHumidity, 124.0))) - 75.0) * FuelType.humidityCorrectionCoeff,
	BaseRateOutputWithFuel.heatIndex = ZoneMonthHour.heatIndex
where BaseRateOutputWithFuel.zoneID = ZoneMonthHour.zoneID
and BaseRateOutputWithFuel.monthID = ZoneMonthHour.monthID
and BaseRateOutputWithFuel.hourID = ZoneMonthHour.hourID
and BaseRateOutputWithFuel.fuelTypeID = FuelType.fuelTypeID;

-- create table step4 select * from BaseRateOutputWithFuel;

-- End Section NotProcess2

-- @algorithm Apply temperature adjustment.
-- For processes (1,2) and pollutants (118,112): rate=rate*exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end)).
-- For all others: rate=rate*((1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)).
-- @input TemperatureAdjustment
-- @output BaseRateOutputWithFuel
-- @condition Testing
update BaseRateOutputWithFuel, TemperatureAdjustment
set 
	meanBaseRate=meanBaseRate*
		case when (processID in (1,2) and pollutantID in (118,112) and modelYearID between minModelYearID and maxModelYearID) then
			exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end))
		else
			(1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))
			*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)
		end,
	meanBaseRateIM=meanBaseRateIM*
		case when (processID in (1,2) and pollutantID in (118,112) and modelYearID between minModelYearID and maxModelYearID) then
			exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end))
		else
			(1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))
			*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)
		end,
	emissionRate=emissionRate*
		case when (processID in (1,2) and pollutantID in (118,112) and modelYearID between minModelYearID and maxModelYearID) then
			exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end))
		else
			(1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))
			*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)
		end,
	emissionRateIM=emissionRateIM*
		case when (processID in (1,2) and pollutantID in (118,112) and modelYearID between minModelYearID and maxModelYearID) then
			exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end))
		else
			(1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))
			*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)
		end,
	meanBaseRateACAdj=meanBaseRateACAdj*
		case when (processID in (1,2) and pollutantID in (118,112) and modelYearID between minModelYearID and maxModelYearID) then
			exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end))
		else
			(1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))
			*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)
		end,
	meanBaseRateIMACAdj=meanBaseRateIMACAdj*
		case when (processID in (1,2) and pollutantID in (118,112) and modelYearID between minModelYearID and maxModelYearID) then
			exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end))
		else
			(1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))
			*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)
		end,
	emissionRateACAdj=emissionRateACAdj*
		case when (processID in (1,2) and pollutantID in (118,112) and modelYearID between minModelYearID and maxModelYearID) then
			exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end))
		else
			(1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))
			*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)
		end,
	emissionRateIMACAdj=emissionRateIMACAdj*
		case when (processID in (1,2) and pollutantID in (118,112) and modelYearID between minModelYearID and maxModelYearID) then
			exp((case when temperature <= 72.0 then tempAdjustTermA*(72.0-temperature) else 0 end))
		else
			(1.0 + (temperature-75)*(tempAdjustTermA + (temperature-75)*tempAdjustTermB))
			*if(BaseRateOutputWithFuel.processID in (1,90,91),if(BaseRateOutputWithFuel.pollutantID=3,K,1.0),1.0)
		end
where BaseRateOutputWithFuel.polProcessID = TemperatureAdjustment.polProcessID
and BaseRateOutputWithFuel.fuelTypeID = TemperatureAdjustment.fuelTypeID 
and modelYearID between TemperatureAdjustment.minModelYearID and TemperatureAdjustment.maxModelYearID;

-- create table step5_BaseRateOutputWithFuel select * from BaseRateOutputWithFuel

-- Section NotProcess2
-- Apply Air Conditioning to BaseRateOutputWithFuel
-- Build the AC update in two steps.  First set the zoneACFactor (hour, source, modelyear).
-- Then multiply the factor by the full AC adjustment addition (i.e. fullACAdjustment-1).
-- When all done, change the emissions for any non-zero factor.
update BaseRateOutputWithFuel set generalFraction = 0, generalFractionRate = 0;

-- @algorithm generalFraction = ACFactor[hourID,sourceTypeID,modelYearID].
-- @condition Not Start Exhaust (2).
-- @input zoneACFactor
-- @output BaseRateOutputWithFuel
update BaseRateOutputWithFuel, zoneACFactor
set generalFraction = ACFactor
where BaseRateOutputWithFuel.hourID = zoneACFactor.hourID
and BaseRateOutputWithFuel.sourceTypeID = zoneACFactor.sourceTypeID
and BaseRateOutputWithFuel.modelYearID = zoneACFactor.modelYearID;

-- @algorithm meanBaseRate = meanBaseRate + (meanBaseRateACAdj * generalFraction[hourID,sourceTypeID,modelYearID]).
-- meanBaseRateIM = meanBaseRateIM + (meanBaseRateIMACAdj * generalFraction[hourID,sourceTypeID,modelYearID]).
-- emissionRate = emissionRate + (emissionRateACAdj * generalFraction[hourID,sourceTypeID,modelYearID]).
-- emissionRateIM = emissionRateIM + (emissionRateIMACAdj * generalFraction[hourID,sourceTypeID,modelYearID]).
-- @condition Not Start Exhaust (2).
update BaseRateOutputWithFuel
set meanBaseRate = meanBaseRate + (meanBaseRateACAdj*generalFraction),
	meanBaseRateIM = meanBaseRateIM + (meanBaseRateIMACAdj*generalFraction),
	emissionRate = emissionRate + (emissionRateACAdj*generalFraction),
	emissionRateIM = emissionRateIM + (emissionRateIMACAdj*generalFraction)
where generalFraction <> 0;

-- create table step6_BaseRateOutputWithFuel select * from BaseRateOutputWithFuel

-- End Section NotProcess2

-- @algorithm Apply I/M programs to BaseRateOutputWithFuel.
-- meanBaseRate=meanBaseRateIM*IMAdjustFract + meanBaseRate*(1-IMAdjustFract).
-- emissionRate=emissionRateIM*IMAdjustFract + emissionRate*(1-IMAdjustFract).
-- @input IMCoverageMergedUngrouped
-- @output BaseRateOutputWithFuel
update BaseRateOutputWithFuel, IMCoverageMergedUngrouped
set
	meanBaseRate=GREATEST(meanBaseRateIM*IMAdjustFract + meanBaseRate*(1.0-IMAdjustFract),0.0),
	emissionRate=GREATEST(emissionRateIM*IMAdjustFract + emissionRate*(1.0-IMAdjustFract),0.0)
where BaseRateOutputWithFuel.processID = IMCoverageMergedUngrouped.processID
	and BaseRateOutputWithFuel.pollutantID = IMCoverageMergedUngrouped.pollutantID
	and BaseRateOutputWithFuel.modelYearID = IMCoverageMergedUngrouped.modelYearID
	and BaseRateOutputWithFuel.fuelTypeID = IMCoverageMergedUngrouped.fuelTypeID
	and BaseRateOutputWithFuel.sourceTypeID = IMCoverageMergedUngrouped.sourceTypeID;

-- create table step8 select * from BaseRateOutputWithFuel;

-- Section Process1_2
-- Handle E85 THC that is created from E10's RVP instead of E85's RVP.

-- @algorithm Handle E85 THC that is created from E10's RVP instead of E85's RVP.
-- Weight the fuel effect ratio by the county's GPA fraction
-- alt criteria ratio=ifnull(ratio,1)+GPAFract*(ifnull(ratioGPA,1)-ifnull(ratio,1))
-- @condition Running Exhaust (1) and Start Exhaust (2).
-- @input County
-- @output altCriteriaRatio
update altCriteriaRatio, County
set ratio=ifnull(ratio,1)+GPAFract*(ifnull(ratioGPA,1)-ifnull(ratio,1));

-- @algorithm Determine the scaling effect of E10-RVP-based fuel effects to E85-RVP-based fuel effects.
-- alt criteria ratio = alt criteria ratio / criteria ratio.
-- @condition Running Exhaust (1) and Start Exhaust (2).
-- @input criteriaRatio
-- @output altCriteriaRatio
update altCriteriaRatio, criteriaRatio
set altCriteriaRatio.ratio = case when criteriaRatio.ratio > 0 then altCriteriaRatio.ratio / criteriaRatio.ratio else 0 end
where altCriteriaRatio.fuelTypeID = criteriaRatio.fuelTypeID
and altCriteriaRatio.fuelFormulationID = criteriaRatio.fuelFormulationID
and altCriteriaRatio.polProcessID = criteriaRatio.polProcessID
and altCriteriaRatio.sourceTypeID = criteriaRatio.sourceTypeID
and altCriteriaRatio.modelYearID = criteriaRatio.modelYearID
and altCriteriaRatio.ageID = criteriaRatio.ageID;

alter table altCriteriaRatio add key speed1 (fuelTypeID,fuelFormulationID,polProcessID,sourceTypeID,modelYearID,ageID);
analyze table altCriteriaRatio;

-- @algorithm Make THC records from the E10 RVP by using the E85-based THC.
-- The output pollutant is 10001.
-- rate for pollutant 10001 = rate * alt criteria ratio.
-- @condition Running Exhaust (1) and Start Exhaust (2).
insert into BaseRateOutputWithFuel (movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, hourID, dayID,
	pollutantID, processID,
	polProcessID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	fuelSubtypeID,
	fuelFormulationID,
	fuelMarketShare,
	regClassID,
	opModeID, generalFraction, generalFractionRate,
	meanBaseRate,
	emissionRate)
select b.movesRunID, b.iterationID,
	b.zoneID, 
	b.sourceTypeID, b.roadTypeID, b.avgSpeedBinID, b.hourDayID, b.hourID, b.dayID,
	(10000 + b.pollutantID) as pollutantID, b.processID,
	((10000 + b.pollutantID)*100 + b.processID) as polProcessID,
	b.modelYearID, 
	b.yearID, b.monthID,
	b.fuelTypeID,
	b.fuelSubtypeID,
	b.fuelFormulationID,
	b.fuelMarketShare,
	b.regClassID,
	b.opModeID, b.generalFraction, b.generalFractionRate,
	meanBaseRate*ratio,
	emissionRate*ratio
from BaseRateOutputWithFuel b
inner join altCriteriaRatio a on (
	b.fuelTypeID = a.fuelTypeID
	and b.fuelFormulationID = a.fuelFormulationID
	and b.polProcessID = a.polProcessID
	and b.sourceTypeID = a.sourceTypeID
	and b.modelYearID = a.modelYearID
	and a.ageID = b.yearID - b.modelYearID)
where b.fuelSubtypeID in (51,52)
and b.modelYearID >= 2001;

-- End Section Process1_2

-- Section EmissionRateAdjustment

-- @algorithm emissionRate=emissionRate*EmissionRateAdjustment,
-- meanbaserate=meanbaserate*EmissionRateAdjustment
update BaseRateOutputWithFuel, EmissionRateAdjustmentWorker
set
	emissionRate=emissionRate*EmissionRateAdjustment,
	meanbaserate=meanbaserate*EmissionRateAdjustment
where BaseRateOutputWithFuel.polprocessID = EmissionRateAdjustmentWorker.polprocessID
	and BaseRateOutputWithFuel.sourceTypeID = EmissionRateAdjustmentWorker.sourceTypeID
	and BaseRateOutputWithFuel.fuelTypeID = EmissionRateAdjustmentWorker.fuelTypeID
	and BaseRateOutputWithFuel.regClassID = EmissionRateAdjustmentWorker.regClassID
	and BaseRateOutputWithFuel.modelYearID = EmissionRateAdjustmentWorker.modelYearID;

-- End Section EmissionRateAdjustment

-- Section Chain92
-- Do Petroleum Energy (92)

-- @algorithm Petroleum Energy (92) = Total Energy Consumption (91) * fuelSubtypePetroleumFraction.
insert into BaseRateOutputWithFuel (movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, hourID, dayID,
	pollutantID, processID,
	polProcessID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	fuelSubtypeID,
	fuelFormulationID,
	fuelMarketShare,
	regClassID,
	opModeID, generalFraction, generalFractionRate,
	meanBaseRate,
	emissionRate)
select b.movesRunID, b.iterationID,
	b.zoneID, 
	b.sourceTypeID, b.roadTypeID, b.avgSpeedBinID, b.hourDayID, b.hourID, b.dayID,
	92 as pollutantID, b.processID,
	(92*100 + b.processID) as polProcessID,
	b.modelYearID, 
	b.yearID, b.monthID,
	b.fuelTypeID,
	b.fuelSubtypeID,
	b.fuelFormulationID,
	b.fuelMarketShare,
	b.regClassID,
	b.opModeID, b.generalFraction, b.generalFractionRate,
	meanBaseRate*fs.fuelSubtypePetroleumFraction,
	emissionRate*fs.fuelSubtypePetroleumFraction
from BaseRateOutputWithFuel b
inner join LocalFuelSupply fs using (fuelFormulationID)
where b.pollutantID=91;
-- End Section Chain92

-- Section Chain93
-- Do Fossil Energy (93)

-- @algorithm Fossil Energy (93) = Total Energy Consumption (91) * fuelSubtypeFossilFraction.
insert into BaseRateOutputWithFuel (movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, hourID, dayID,
	pollutantID, processID,
	polProcessID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	fuelSubtypeID,
	fuelFormulationID,
	fuelMarketShare,
	regClassID,
	opModeID, generalFraction, generalFractionRate,
	meanBaseRate,
	emissionRate)
select b.movesRunID, b.iterationID,
	b.zoneID, 
	b.sourceTypeID, b.roadTypeID, b.avgSpeedBinID, b.hourDayID, b.hourID, b.dayID,
	93 as pollutantID, b.processID,
	(93*100 + b.processID) as polProcessID,
	b.modelYearID, 
	b.yearID, b.monthID,
	b.fuelTypeID,
	b.fuelSubtypeID,
	b.fuelFormulationID,
	b.fuelMarketShare,
	b.regClassID,
	b.opModeID, b.generalFraction, b.generalFractionRate,
	meanBaseRate*fs.fuelSubtypeFossilFraction,
	emissionRate*fs.fuelSubtypeFossilFraction
from BaseRateOutputWithFuel b
inner join LocalFuelSupply fs using (fuelFormulationID)
where b.pollutantID=91;
-- End Section Chain93

alter table BaseRateOutputWithFuel add key (
	sourceTypeID, avgSpeedBinID, hourDayID,
	pollutantID,
	modelYearID, 
	fuelTypeID,
	regClassID);

analyze table BaseRateOutputWithFuel;

-- @algorithm Remove fuel formulation and opModeID from BaseRateOutputWithFuel, filling BaseRateOutput.
-- Note: This top-level calculator executes at the Month level. That means there will be exactly one
-- distinct value in each of these columns:
-- processID, stateID, countyID, zoneID, linkID, roadtypeID, yearID, monthID.
-- As such, these columns do not need to be indexed or included in a GROUP BY.
insert into BaseRateOutput (movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID,
	pollutantID, processID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	regClassID,
	meanBaseRate,
	emissionRate)
select
	movesRunID, iterationID,
	zoneID, 
	sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID,
	pollutantID, processID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	regClassID,
	sum(fuelMarketShare*meanBaseRate) as meanBaseRate,
	sum(fuelMarketShare*emissionRate) as emissionRate
from BaseRateOutputWithFuel
group by 
	sourceTypeID, avgSpeedBinID, hourDayID,
	pollutantID,
	modelYearID, 
	fuelTypeID,
	regClassID;

-- Section GetActivity
-- @algorithm Ensure all activity slots have data. Use a default value of 0
-- when not provided by the input table.
insert ignore into universalActivity (hourDayID, modelYearID, sourceTypeID, activity)
select hourDayID, modelYearID, sourceTypeID, 0
from RunSpecHourDay, RunSpecModelyearAge, RunSpecSourceType;
-- End Section GetActivity

-- Section AggregateSMFR
drop table if exists activityTotal;
drop table if exists activityDetail;

create table if not exists activityDetail (
	hourDayID smallint not null,
	modelYearID smallint not null,
	sourceTypeID smallint not null,
	fuelTypeID smallint not null,
	regClassID smallint not null,
	activity double,
	activityRates double,
	primary key (sourceTypeID, hourDayID, modelYearID, fuelTypeID, regClassID),
	key (hourDayID, modelYearID, fuelTypeID, regClassID)
);

insert into activityDetail(hourDayID,modelYearID,sourceTypeID,fuelTypeID,regClassID,activity,activityRates)
select u.hourDayID,u.modelYearID,u.sourceTypeID,fuelTypeID,regClassID,
	sum(activity*sbdTotal) as activity,
	sum(activity*sbdTotal) as activityRates
from smfrSBDSummary s
inner join universalActivity u using (sourceTypeID, modelYearID)
group by u.sourceTypeID, u.hourDayID, u.modelYearID, fuelTypeID, regClassID
order by null;

-- Section AdjustAPUEmissionRate

-- @algorithm When aggregating APU emission rates to remove source type, model year, fuel type, or regclass,
-- the activity used to weight the rates must be adjusted. The input activity includes extended idling
-- and instead must be restricted to just hours spent using a diesel APU. This is a model year-based effect.
update activityDetail, apuEmissionRateFraction
	set activityRates = activityRates * hourFractionAdjust
where apuEmissionRateFraction.modelYearID = activityDetail.modelYearID;
-- End Section AdjustAPUEmissionRate

create table activityTotal (
	hourDayID smallint not null,
	sourceTypeID smallint not null,
	modelYearID smallint not null,
	fuelTypeID smallint not null,
	regClassID smallint not null,
	activityTotal double,
	activityRatesTotal double,
	primary key (hourDayID, modelYearID, sourceTypeID, fuelTypeID, regClassID),
	key (hourDayID, sourceTypeID, modelYearID, fuelTypeID, regClassID)
);

insert into activityTotal (hourDayID,sourceTypeID,modelYearID,fuelTypeID,regClassID,activityTotal,activityRatesTotal)
select hourDayID
	##activityTotalSelect##
	, sum(activity) as activityTotal
	, sum(activityRates) as activityRatesTotal
from activityDetail u
group by hourDayID
	##activityTotalGroup##
order by null;

drop table if exists activityWeight;

create table if not exists activityWeight (
	hourDayID smallint not null,
	sourceTypeID smallint not null,
	modelYearID smallint not null,
	fuelTypeID smallint not null,
	regClassID smallint not null,
	smfrFraction double,
	smfrRatesFraction double,
	primary key (sourceTypeID, hourDayID, modelYearID, fuelTypeID, regClassID),
	key (modelYearID, sourceTypeID, hourDayID, fuelTypeID, regClassID),
	key (hourDayID, sourceTypeID, modelYearID, fuelTypeID, regClassID)
);

-- @algorithm When aggregating rates to remove source type, model year, fuel type, or regclass, calculate an activity distribution.
-- smfrFraction[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID] = activity[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID] / activityTotal[aggregated]
insert into activityWeight (hourDayID,sourceTypeID,modelYearID,fuelTypeID,regClassID,smfrFraction,smfrRatesFraction)
select u.hourDayID, u.sourceTypeID, u.modelYearID, u.fuelTypeID, u.regClassID,
	case when activityTotal > 0 then activity/activityTotal else 0.0 end as smfrFraction,
	case when activityRatesTotal > 0 then activityRates/activityRatesTotal else 0.0 end as smfrRatesFraction
from activityDetail u
inner join activityTotal t using (hourDayID
	##activityWeightJoin##
);

-- Section AdjustEmissionRateOnly
-- @algorithm When aggregating rates to remove source type, model year, fuel type, or regclass, weight emissions by the activity distribution.
-- BaseRateOutput = BaseRateOutput * smfrFraction[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID]
update BaseRateOutput, activityWeight
set
	emissionRate=emissionRate*smfrRatesFraction
where BaseRateOutput.modelYearID = activityWeight.modelYearID
	and BaseRateOutput.sourceTypeID = activityWeight.sourceTypeID
	and BaseRateOutput.hourDayID = activityWeight.hourDayID
	and BaseRateOutput.fuelTypeID = activityWeight.fuelTypeID
	and BaseRateOutput.regClassID = activityWeight.regClassID;
-- End Section AdjustEmissionRateOnly

-- Section AdjustMeanBaseRateAndEmissionRate
-- @algorithm When aggregating rates to remove source type, model year, fuel type, or regclass, weight emissions by the activity distribution.
-- BaseRateOutput = BaseRateOutput * smfrFraction[sourceTypeID,modelYearID,hourDayID,fuelTypeID,regClassID]
update BaseRateOutput, activityWeight
set
	meanBaseRate=meanBaseRate*smfrFraction,
	emissionRate=emissionRate*smfrRatesFraction
where BaseRateOutput.modelYearID = activityWeight.modelYearID
	and BaseRateOutput.sourceTypeID = activityWeight.sourceTypeID
	and BaseRateOutput.hourDayID = activityWeight.hourDayID
	and BaseRateOutput.fuelTypeID = activityWeight.fuelTypeID
	and BaseRateOutput.regClassID = activityWeight.regClassID;
-- End Section AdjustMeanBaseRateAndEmissionRate

-- End Section AggregateSMFR

-- Section ApplyActivity

-- @algorithm When creating an inventory or certain rates, convert BaseRateOutput to an inventory.
-- BaseRateOutput.meanBaseRate = BaseRateOutput.meanBaseRate * activity[processID,hourDayID,modelYearID,sourceTypeID(,month,year,location)]
update BaseRateOutput, universalActivity
set
	meanBaseRate=meanBaseRate*activity
where BaseRateOutput.processID = ##context.iterProcess.databaseKey##
	and BaseRateOutput.hourDayID = universalActivity.hourDayID
	and BaseRateOutput.modelYearID = universalActivity.modelYearID
	and BaseRateOutput.sourceTypeID = universalActivity.sourceTypeID;

-- End Section ApplyActivity

-- @algorithm Populate MOVESWorkerOutput from BaseRateOutput.
insert into MOVESWorkerOutput (movesRunID, iterationID,
	zoneID, linkID, countyID, stateID,
	sourceTypeID, SCC, roadTypeID,
	hourID, dayID,
	pollutantID, processID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	regClassID,
	emissionQuant,
	emissionRate)
select movesRunID, iterationID,
	zoneID, 
	##context.iterLocation.linkRecordID## as linkID, 
	##context.iterLocation.countyRecordID## as countyID, 
	##context.iterLocation.stateRecordID## as stateID,
	sourceTypeID, SCC, roadTypeID, 
	floor(hourDayID/10) as hourID, mod(hourDayID,10) as dayID,
	pollutantID, processID,
	modelYearID, 
	yearID, monthID,
	fuelTypeID,
	regClassID,
	meanBaseRate as emissionQuant,
	emissionRate
from BaseRateOutput;

-- End Section Processing

-- Section Cleanup
drop table if exists tempBaseRateOutput;
drop table if exists BaseRateOutputWithFuel;
drop table if exists IMCoverageMergedUngrouped;
drop table if exists zoneACFactor;
drop table if exists LocalFuelSupply;
drop table if exists modelYearWeight;
drop table if exists vmtByMYRoadHourFraction;
drop table if exists activityTotal;
drop table if exists activityWeight;
drop table if exists apuEmissionRateFraction;

drop table if exists step1;
drop table if exists step2;
drop table if exists step3;
drop table if exists step4;
drop table if exists step5;
drop table if exists step6;
drop table if exists step7;
drop table if exists step8;

-- End Section Cleanup

-- Section Final Cleanup

-- Remove any debugging pollutants.
delete from MOVESWorkerOutput where pollutantID >= 10000;
delete from BaseRateOutput where pollutantID >= 10000;

-- Section HasChainedCalculators
-- @algorithm When chained calculators are used, data must be moved back from MOVESWorkerOutput.
-- avgSpeedBinID must be recovered in this process.
-- @condition Chained calculators are present.

-- @algorithm Remove entries from BaseRateOutput. These will be reinserted later along with
-- the results from chained calculators.
-- @condition Chained calculators are present.
truncate table BaseRateOutput;

-- Section Rates
-- Section NotProject
-- @algorithm Populate BaseRateOutput from MOVESWorkerOutput.
-- @condition Non-Project domain rates chained calculators are present.
insert into BaseRateOutput(MOVESRunID,iterationID,
	zoneID,linkID,sourceTypeID,SCC,roadTypeID,
	avgSpeedBinID,
	monthID,
	hourDayID,
	pollutantID,processID,modelYearID,yearID,fuelTypeID,regClassID,
	meanBaseRate,emissionRate)
select MOVESRunID,iterationID,
	zoneID,linkID,sourceTypeID,SCC,roadTypeID,
	mod(linkID,100) as avgSpeedBinID,
	monthID,
	(hourID*10 + dayID) as hourDayID,
	pollutantID,processID,modelYearID,yearID,fuelTypeID,regClassID,
	emissionQuant as meanBaseRate,emissionRate
from MOVESWorkerOutput;
-- End Section NotProject

-- Section Project
-- @algorithm Populate BaseRateOutput from MOVESWorkerOutput. avgSpeedBinID is always 0 in project mode.
-- @condition Project domain rates chained calculators are present.
insert into BaseRateOutput(MOVESRunID,iterationID,
	zoneID,linkID,sourceTypeID,SCC,roadTypeID,
	avgSpeedBinID,
	monthID,
	hourDayID,
	pollutantID,processID,modelYearID,yearID,fuelTypeID,regClassID,
	meanBaseRate,emissionRate)
select MOVESRunID,iterationID,
	zoneID,linkID,sourceTypeID,SCC,roadTypeID,
	0 as avgSpeedBinID,
	monthID,
	(hourID*10 + dayID) as hourDayID,
	pollutantID,processID,modelYearID,yearID,fuelTypeID,regClassID,
	emissionQuant as meanBaseRate,emissionRate
from MOVESWorkerOutput;
-- End Section Project
-- End Section Rates

-- Section Inventory
-- @algorithm Populate BaseRateOutput from MOVESWorkerOutput. avgSpeedBinID is always 0 in inventory mode.
-- @condition Inventory chained calculators are present.
insert into BaseRateOutput(MOVESRunID,iterationID,
	zoneID,linkID,sourceTypeID,SCC,roadTypeID,
	avgSpeedBinID,
	monthID,
	hourDayID,
	pollutantID,processID,modelYearID,yearID,fuelTypeID,regClassID,
	meanBaseRate,emissionRate)
select MOVESRunID,iterationID,
	zoneID,linkID,sourceTypeID,SCC,roadTypeID,
	0 as avgSpeedBinID,
	monthID,
	(hourID*10 + dayID) as hourDayID,
	pollutantID,processID,modelYearID,yearID,fuelTypeID,regClassID,
	emissionQuant as meanBaseRate,emissionRate
from MOVESWorkerOutput;
-- End Section Inventory
-- End Section HasChainedCalculators

-- Section HasNoChainedCalculators
update BaseRateOutput set linkID=##context.iterLocation.linkRecordID##
where linkID is null or linkID=0;
-- End Section HasNoChainedCalculators

-- End Section Final Cleanup
