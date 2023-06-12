-- Version 2014-10-14
-- Author Wesley Faler
-- Supported special section names:
--		RefuelingDisplacementVaporLoss
--		RefuelingSpillageLoss

-- @algorithm
-- @owner Refueling Loss Calculator
-- @calculator

-- Supported special variable names:
--		##refuelingDisplacement.pollutantIDs##
--		##refuelingSpillage.pollutantIDs##
--		##refuelingProcessIDs##

-- Section Create Remote Tables for Extracted Data
##create.RefuelingControlTechnology##;
TRUNCATE TABLE RefuelingControlTechnology;

##create.RefuelingFactors##;
TRUNCATE TABLE RefuelingFactors;

##create.SourceTypeTechAdjustment##;
TRUNCATE TABLE SourceTypeTechAdjustment;

drop table if exists RefuelingFuelType;
create table RefuelingFuelType (
	fuelTypeID           SMALLINT NOT NULL,
	defaultFormulationID SMALLINT NOT NULL,
	fuelTypeDesc         CHAR(50) NULL,
	energyContent			FLOAT	NULL,
	fuelDensity				FLOAT	NULL,
	monthID SMALLINT NOT NULL,
	unique index XPKRefuelingFuelType (fuelTypeID, monthID),
	key (monthID, fuelTypeID)
);
truncate table RefuelingFuelType;

drop table if exists RefuelingRunSpecHour;
create table RefuelingRunSpecHour (
	hourID smallint(6) not null,
	unique index XPKRefuelingRunSpecHour ( hourID )
);
truncate table RefuelingRunSpecHour;

drop table if exists RefuelingRunSpecMonth;
create table RefuelingRunSpecMonth (
	monthID smallint(6) not null,
	unique index XPKRefuelingRunSpecMonth ( monthID )
);
truncate table RefuelingRunSpecMonth;

drop table if exists RefuelingCountyYear;
create table RefuelingCountyYear (
	countyID             INTEGER NOT NULL,
	yearID               SMALLINT NOT NULL,
	refuelingVaporProgramAdjust FLOAT NOT NULL DEFAULT 0.0,
	refuelingSpillProgramAdjust FLOAT NOT NULL DEFAULT 0.0
);
truncate table RefuelingCountyYear;

-- Section RefuelingDisplacementVaporLoss
drop table if exists RefuelingDisplacementPollutant;
create table RefuelingDisplacementPollutant (
	pollutantID smallint(6) not null,
	unique index XPKRefuelingDisplacementPollutant (pollutantID)
);
truncate table RefuelingDisplacementPollutant;
-- End Section RefuelingDisplacementVaporLoss

-- Section RefuelingSpillageLoss
drop table if exists RefuelingSpillagePollutant;
create table RefuelingSpillagePollutant (
	pollutantID smallint(6) not null,
	unique index XPKRefuelingSpillagePollutant (pollutantID)
);
truncate table RefuelingSpillagePollutant;
-- End Section RefuelingSpillageLoss

drop table if exists refuelingFuelFormulation;
CREATE TABLE refuelingFuelFormulation (
  fuelFormulationID INT(11) NOT NULL DEFAULT '0',
  fuelSubtypeID smallint(6) NOT NULL DEFAULT '0',
  RVP float DEFAULT NULL,
  sulfurLevel float NOT NULL DEFAULT '30',
  ETOHVolume float DEFAULT NULL,
  MTBEVolume float DEFAULT NULL,
  ETBEVolume float DEFAULT NULL,
  TAMEVolume float DEFAULT NULL,
  aromaticContent float DEFAULT NULL,
  olefinContent float DEFAULT NULL,
  benzeneContent float DEFAULT NULL,
  e200 float DEFAULT NULL,
  e300 float DEFAULT NULL,
  volToWtPercentOxy float DEFAULT NULL,
  BioDieselEsterVolume float DEFAULT NULL,
  CetaneIndex float DEFAULT NULL,
  PAHContent float DEFAULT NULL,
  T50 float DEFAULT NULL,
  T90 float DEFAULT NULL,
  PRIMARY KEY (fuelFormulationID)
);
truncate table refuelingFuelFormulation;

drop table if exists refuelingFuelSubtype;
CREATE TABLE refuelingFuelSubtype (
  fuelSubtypeID smallint(6) NOT NULL DEFAULT '0',
  fuelTypeID smallint(6) NOT NULL DEFAULT '0',
  fuelSubtypeDesc char(50) DEFAULT NULL,
  fuelSubtypePetroleumFraction float DEFAULT NULL,
  fuelSubtypePetroleumFractionCV float DEFAULT NULL,
  fuelSubtypeFossilFraction float DEFAULT NULL,
  fuelSubtypeFossilFractionCV float DEFAULT NULL,
  carbonContent float DEFAULT NULL,
  oxidationFraction float DEFAULT NULL,
  carbonContentCV float DEFAULT NULL,
  oxidationFractionCV float DEFAULT NULL,
  energyContent float DEFAULT NULL,
  PRIMARY KEY (fuelSubtypeID),
  KEY fuelTypeID (fuelTypeID,fuelSubtypeID)
);
truncate table refuelingFuelSubtype;

drop table if exists refuelingFuelSupply;
CREATE TABLE refuelingFuelSupply (
  fuelRegionID int(11) NOT NULL DEFAULT '0',
  fuelYearID int(11) NOT NULL DEFAULT '0',
  monthGroupID smallint(6) NOT NULL DEFAULT '0',
  fuelFormulationID INT(11) NOT NULL DEFAULT '0',
  marketShare float DEFAULT NULL,
  marketShareCV float DEFAULT NULL,
  PRIMARY KEY (fuelRegionID,fuelFormulationID,monthGroupID,fuelYearID),
  KEY countyID (fuelRegionID),
  KEY yearID (fuelYearID),
  KEY monthGroupID (monthGroupID),
  KEY fuelSubtypeID (fuelFormulationID)
);
truncate table refuelingFuelSupply;

drop table if exists refuelingMonthOfAnyYear;
CREATE TABLE refuelingMonthOfAnyYear (
  monthID smallint(6) NOT NULL DEFAULT '0',
  monthName char(10) DEFAULT NULL,
  noOfDays smallint(6) DEFAULT NULL,
  monthGroupID smallint(6) NOT NULL DEFAULT '0',
  PRIMARY KEY (monthID),
  KEY monthGroupID (monthGroupID),
  KEY monthGroupID_2 (monthGroupID,monthID),
  KEY monthID (monthID,monthGroupID)
);
truncate table refuelingMonthOfAnyYear;

drop table if exists refuelingZoneMonthHour;
CREATE TABLE refuelingZoneMonthHour (
  monthID smallint(6) NOT NULL DEFAULT '0',
  zoneID int(11) NOT NULL DEFAULT '0',
  hourID smallint(6) NOT NULL DEFAULT '0',
  temperature DOUBLE DEFAULT NULL,
  relHumidity DOUBLE DEFAULT NULL,
  heatIndex DOUBLE DEFAULT NULL,
  specificHumidity DOUBLE DEFAULT NULL,
  molWaterFraction DOUBLE DEFAULT NULL,
  PRIMARY KEY (hourID,monthID,zoneID),
  KEY monthID (monthID),
  KEY zoneID (zoneID),
  KEY hourID (hourID)
);
truncate table refuelingZoneMonthHour;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
cache SELECT * INTO OUTFILE '##RefuelingFactors##'
FROM RefuelingFactors;

-- @algorithm energyContent = sum(marketShare * energyContent) across all the fuel supply.
cache SELECT ft.fuelTypeID, defaultFormulationID, fuelTypeDesc, 
	sum(marketShare*energyContent) as energyContent, fuelDensity, rsm.monthID
INTO OUTFILE '##RefuelingFuelType##'
FROM FuelType ft
inner join fuelSubtype fst on (fst.fuelTypeID=ft.fuelTypeID)
inner join fuelFormulation ff on (ff.fuelSubtypeID=fst.fuelSubtypeID)
inner join fuelSupply fs on (
	fs.fuelRegionID=##context.fuelRegionID##
	and fs.fuelFormulationID=ff.fuelFormulationID)
inner join year y on (
	y.yearID=##context.year##
	and y.fuelYearID=fs.fuelYearID)
inner join monthOfAnyYear m on (m.monthGroupID=fs.monthGroupID)
inner join runspecMonth rsm on (rsm.monthID=m.monthID)
WHERE energyContent is not null
AND energyContent > 0
AND fuelDensity is not null
AND fuelDensity > 0
group by ft.fuelTypeID, rsm.monthID;

cache SELECT
	processID, sourceTypeID, 
	MYRMAP(modelYearID) as modelYearID, 
	refuelingTechAdjustment
INTO OUTFILE '##SourceTypeTechAdjustment##'
FROM SourceTypeTechAdjustment
WHERE processID IN (##refuelingProcessIDs##)
AND modelYearID <= MYMAP(##context.year##)
AND modelYearID >= MYMAP(##context.year## - 30);

cache SELECT
	processID, MYRMAP(modelYearID) as modelYearID,
	regClassID, sourceTypeID, fuelTypeID, ageID, 	 
	refuelingTechAdjustment, controlledRefuelingRate
INTO OUTFILE '##RefuelingControlTechnology##'
FROM RefuelingControlTechnology
WHERE processID IN (##refuelingProcessIDs##)
AND modelYearID <= MYMAP(##context.year##)
AND modelYearID >= MYMAP(##context.year## - 30);

cache SELECT * INTO OUTFILE '##RefuelingRunSpecHour##'
FROM RunSpecHour;

cache SELECT * INTO OUTFILE '##RefuelingRunSpecMonth##'
FROM RunSpecMonth;

cache SELECT * INTO OUTFILE '##RefuelingCountyYear##'
FROM CountyYear
WHERE countyID=##context.iterLocation.countyRecordID##
AND yearID=##context.year##;

-- Section RefuelingDisplacementVaporLoss
cache SELECT pollutantID INTO OUTFILE '##RefuelingDisplacementPollutant##'
FROM Pollutant
WHERE pollutantID in (##refuelingDisplacement.pollutantIDs##);
-- End Section RefuelingDisplacementVaporLoss

-- Section RefuelingSpillageLoss
cache SELECT pollutantID INTO OUTFILE '##RefuelingSpillagePollutant##'
FROM Pollutant
WHERE pollutantID in (##refuelingSpillage.pollutantIDs##);
-- End Section RefuelingSpillageLoss

cache SELECT ff.* INTO OUTFILE '##RefuelingFuelFormulation##'
FROM FuelFormulation ff
INNER JOIN FuelSupply fs ON fs.fuelFormulationID = ff.fuelFormulationID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID
INNER JOIN RunSpecMonthGroup rsmg ON rsmg.monthGroupID = fs.monthGroupID
WHERE fuelRegionID = ##context.fuelRegionID## AND
yearID = ##context.year##
GROUP BY ff.FuelFormulationID ORDER BY NULL;

cache SELECT * INTO OUTFILE '##RefuelingFuelSubtype##'
FROM FuelSubtype;

cache SELECT FuelSupply.* INTO OUTFILE '##RefuelingFuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##;

cache SELECT MonthOfAnyYear.*
INTO OUTFILE '##RefuelingMonthOfAnyYear##'
FROM MonthOfAnyYear;

cache SELECT ZoneMonthHour.*
INTO OUTFILE '##RefuelingZoneMonthHour##'
FROM RunSpecMonth
INNER JOIN RunSpecHour
INNER JOIN ZoneMonthHour ON (ZoneMonthHour.monthID = RunSpecMonth.monthID AND ZoneMonthHour.hourID = RunSpecHour.hourID)
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

-- End Section Extract Data

-- Section Local Data Removal
--TRUNCATE XXXXXX;
-- Section RefuelingDisplacementVaporLoss
-- End Section RefuelingDisplacementVaporLoss

-- Section RefuelingSpillageLoss
-- End Section RefuelingSpillageLoss
-- End Section Local Data Removal

-- Section Processing

-- Section RefuelingDisplacementVaporLoss
-- --------------------------------------------------------------
-- REFEC-1: Determine the refueling temperatures
-- --------------------------------------------------------------

-- @algorithm Determine the refueling temperatures
drop table if exists RefuelingTemp;

create table RefuelingTemp (
	monthID smallint(6) not null,
	hourID smallint(6) not null,
	fuelTypeID smallint(6) not null,

	refuelingTemperature float not null default 0.0,
	tankTemperatureDif float not null default 0.0,
	displacedVaporRate float not null default 0.0,

	key(monthID),
	key(hourID),
	key(fuelTypeID),
	unique index XPKRefuelingTemp (monthID,hourID,fuelTypeID)
);

-- @algorithm refuelingTemperature = 20.30 + 0.81 * hourly ambient temperature, 
--            with the hourly ambient temperature bounded between vaporLowTLimit and upper bound of vaporHighTLimit
--            based on the 2008 California study. Uses a CASE statement to only use this equation when the temperature limits are set, otherwise the ambient is used.
insert into RefuelingTemp (monthID, hourID, fuelTypeID, refuelingTemperature)
select monthID, hourID, fuelTypeID,
       CASE 
	        WHEN (vaporHighTLimit <> 0 AND vaporLowTLimit <> 0) THEN 20.30 + 0.81*LEAST(vaporHighTLimit, GREATEST(vaporLowTLimit, temperature))
			ELSE temperature
	   END as refuelingTemperature
from RefuelingZoneMonthHour, RefuelingFactors;

analyze table RefuelingTemp;

-- @algorithm tankTemperatureDif = ((vaporTermE*refuelingTemperature) + vaporTermF) subject to lower bound of 0 and upper bound of tankDiffLimit.
update RefuelingTemp, RefuelingFactors
set RefuelingTemp.tankTemperatureDif = 
	case
		when ((vaporTermE*refuelingTemperature) + vaporTermF) >= tankTDiffLimit then tankTDiffLimit
		when ((vaporTermE*refuelingTemperature) + vaporTermF) <= 0.0 then 0.0
		else ((vaporTermE*refuelingTemperature) + vaporTermF)
	end
where RefuelingFactors.fuelTypeID=RefuelingTemp.fuelTypeID;

-- --------------------------------------------------------------
-- REFEC-2: Determine the unadjusted refueling displacement vapor loss rate
-- --------------------------------------------------------------

-- @algorithm Determine the unadjusted refueling displacement vapor loss rate
drop table if exists RefuelingAverageRVP;

create table RefuelingAverageRVP (
	monthID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	averageRVP float not null,
	key(monthID),
	key(fuelTypeID),
	unique index XPKRefuelingAverageRVP (monthID, fuelTypeID)
);

-- @algorithm averageRVP = sum(RVP * marketShare) across fuel formulations.
insert into RefuelingAverageRVP (monthID, fuelTypeID, averageRVP)
select monthID, fuelTypeID, coalesce(sum(rvp*marketShare),0.0) as averageRVP
from RefuelingFuelSupply
inner join RefuelingFuelFormulation on RefuelingFuelFormulation.fuelFormulationID=RefuelingFuelSupply.fuelFormulationID
inner join RefuelingMonthOfAnyYear on RefuelingMonthOfAnyYear.monthGroupID=RefuelingFuelSupply.monthGroupID
inner join RefuelingFuelSubtype on RefuelingFuelSubtype.fuelSubtypeID=RefuelingFuelFormulation.fuelSubtypeID
group by monthID, fuelTypeID;

analyze table RefuelingAverageRVP;

-- @algorithm Provide default averageRVP of 0.
insert ignore into RefuelingAverageRVP (monthID, fuelTypeID, averageRVP)
select monthID, fuelTypeID, 0.0 as averageRVP
from RefuelingFactors, RefuelingMonthOfAnyYear;

analyze table RefuelingAverageRVP;

-- @algorithm displacedVaporRate = exp(vaporTermA + vaporTermB * tankTemperatureDif + vaporTermC * refuelingTemperature + vaporTermD * averageRVP)
update RefuelingTemp, RefuelingFactors, RefuelingAverageRVP
set displacedVaporRate = exp(vaporTermA
	+ vaporTermB * tankTemperatureDif
	+ vaporTermC * refuelingTemperature
	+ vaporTermD * averageRVP)
where RefuelingFactors.fuelTypeID=RefuelingTemp.fuelTypeID
and RefuelingAverageRVP.fuelTypeID=RefuelingTemp.fuelTypeID
and RefuelingAverageRVP.monthID=RefuelingTemp.monthID;

-- @algorithm Limit displacedVaporRate to no less than minimumRefuelingVaporLoss.
update RefuelingTemp, RefuelingFactors
set displacedVaporRate = case when minimumRefuelingVaporLoss <= -1 then 0 else minimumRefuelingVaporLoss end
where RefuelingFactors.fuelTypeID=RefuelingTemp.fuelTypeID
and (displacedVaporRate < minimumRefuelingVaporLoss or minimumRefuelingVaporLoss <= -1);

-- --------------------------------------------------------------
-- REFEC-3: Technology adjustment of the refueling displacement vapor loss rate
-- REFEC-4: Program adjustment of the refueling displacement vapor loss rate
-- --------------------------------------------------------------
drop table if exists RefuelingDisplacement;
create table RefuelingDisplacement (
	modelYearID smallint(6) not null,
	regClassID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	fuelTypeID smallint(6) not null,
	ageID smallint(6) not null,
	monthID smallint(6) not null,
	hourID smallint(6) not null,

	adjustedVaporRate float not null default 0.0,

	key(modelYearID),
	key(regClassID),
	key(sourceTypeID),
	key(fuelTypeID),
	key(ageID),
	key(monthID),
	key(hourID),
	unique index XPKRefuelingDisplacement (modelYearID, regClassID, sourceTypeID, fuelTypeID, ageID, monthID, hourID)
);

-- @algorithm Technology and Program adjustment of the refueling displacement vapor loss rate.
-- adjustedVaporRate = displacedVaporRate * (1.0-refuelingVaporProgramAdjust)*(1.0-refuelingTechAdjustment) + controlledRefuelingRate*(1.0-refuelingVaporProgramAdjust)*refuelingTechAdjustment
-- @condition Refueling Displacement Vapor Loss (18).
insert into RefuelingDisplacement (modelYearID, regClassID, sourceTypeID, fuelTypeID, ageID, monthID, hourID, adjustedVaporRate)
select modelYearID, regClassID, rct.sourceTypeID, rct.fuelTypeID, ageID, monthID, hourID,
	   displacedVaporRate * (1.0-refuelingVaporProgramAdjust)*(1.0-refuelingTechAdjustment) + controlledRefuelingRate*(1.0-refuelingVaporProgramAdjust)*refuelingTechAdjustment as adjustedVaporRate
from RefuelingCountyYear rcy,
     RefuelingControlTechnology rct, 
	 RefuelingTemp rt
where rct.processID = 18 and rct.fuelTypeID = rt.fuelTypeID;

-- End Section RefuelingDisplacementVaporLoss

-- Section RefuelingSpillageLoss
-- --------------------------------------------------------------
-- REFEC-5: Technology adjustment of the refueling spillage rate
-- REFEC-6: Program adjustment of the refueling spillage rate
-- --------------------------------------------------------------

-- first, adjust RefuelingCountyYear to account for the fact that Stage II controls only affect gas/e85
-- (we do not model refueling displacement vapor from other fuel types, so we don't need to worry about Stage II affecting them)

-- the default value makes all existing rows apply to gasoline
ALTER TABLE RefuelingCountyYear ADD COLUMN fuelTypeID smallint(6) not null default 1 AFTER yearID;
-- duplicate gasoline values for E85 since most stage II applies to both fuels
INSERT INTO RefuelingCountyYear (countyID, yearID, fuelTypeID, refuelingVaporProgramAdjust, refuelingSpillProgramAdjust)
SELECT countyID, yearID, 5 as fuelTypeID, refuelingVaporProgramAdjust, refuelingSpillProgramAdjust 
FROM RefuelingCountyYear WHERE fuelTypeID = 1; 
-- insert 0s for all other fuel types
INSERT INTO RefuelingCountyYear (countyID, yearID, fuelTypeID, refuelingVaporProgramAdjust, refuelingSpillProgramAdjust)
SELECT countyID, yearID, 2 as fuelTypeID, 0 as refuelingVaporProgramAdjust, 0 as refuelingSpillProgramAdjust 
FROM RefuelingCountyYear WHERE fuelTypeID = 1;
INSERT INTO RefuelingCountyYear (countyID, yearID, fuelTypeID, refuelingVaporProgramAdjust, refuelingSpillProgramAdjust)
SELECT countyID, yearID, 3 as fuelTypeID, 0 as refuelingVaporProgramAdjust, 0 as refuelingSpillProgramAdjust 
FROM RefuelingCountyYear WHERE fuelTypeID = 1;
INSERT INTO RefuelingCountyYear (countyID, yearID, fuelTypeID, refuelingVaporProgramAdjust, refuelingSpillProgramAdjust)
SELECT countyID, yearID, 9 as fuelTypeID, 0 as refuelingVaporProgramAdjust, 0 as refuelingSpillProgramAdjust 
FROM RefuelingCountyYear WHERE fuelTypeID = 1;

-- Now calculate refueling spillage based on the tech adjustment and the program adjustment
drop table if exists RefuelingSpillage;
create table RefuelingSpillage (
	fuelTypeID smallint(6) not null,
	sourceTypeID smallint(6) not null,
	modelYearID smallint(6) not null,
	adjustedSpillRate float not null default 0.0,
	key(fuelTypeID),
	key(sourceTypeID),
	key(modelYearID),
	unique index XPKRefuelingSpillage (fuelTypeID, sourceTypeID, modelYearID)
);

-- @algorithm Technology and Program adjustment of the refueling spillage rate.
-- adjustedSpillRate = (1.0-refuelingSpillProgramAdjust)*((1.0-refuelingTechAdjustment)*refuelingSpillRate).
-- @condition Refueling Spillage Loss (19).
insert into RefuelingSpillage (fuelTypeID, sourceTypeID, modelYearID, adjustedSpillRate)
select RefuelingFactors.fuelTypeID, sourceTypeID, modelYearID, 
	(1.0-refuelingSpillProgramAdjust)*((1.0-refuelingTechAdjustment)*refuelingSpillRate) as adjustedSpillRate
from RefuelingCountyYear, SourceTypeTechAdjustment, RefuelingFactors
where SourceTypeTechAdjustment.processID=19
  and RefuelingCountyYear.fuelTypeID=RefuelingFactors.fuelTypeID;

-- End Section RefuelingSpillageLoss

-- --------------------------------------------------------------
-- REFEC-7: Calculate total fuel consumption from Total Energy
-- REFEC-8: Refueling loss emission results
-- --------------------------------------------------------------
drop table if exists RefuelingWorkerOutputTemp;
create table RefuelingWorkerOutputTemp (
	yearID               SMALLINT UNSIGNED NULL,
	monthID              SMALLINT UNSIGNED NULL,
	dayID                SMALLINT UNSIGNED NULL,
	hourID               SMALLINT UNSIGNED NULL,
	stateID              SMALLINT UNSIGNED NULL,
	countyID             INTEGER UNSIGNED NULL,
	zoneID               INTEGER UNSIGNED NULL,
	linkID               INTEGER UNSIGNED NULL,
	pollutantID          SMALLINT UNSIGNED NULL,
	processID            SMALLINT UNSIGNED NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL,
	regClassID			 SMALLINT UNSIGNED NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,
	roadTypeID           SMALLINT UNSIGNED NULL,
	SCC                  CHAR(10) NULL,
	emissionQuant        DOUBLE NULL,
	emissionRate		 DOUBLE NULL
);

-- Section RefuelingDisplacementVaporLoss
truncate RefuelingWorkerOutputTemp;

-- @algorithm emissions = (adjustedVaporRate * Total Energy Consumption (91)) / (energyContent * fuelDensity).
-- @condition Refueling Displacement Vapor Loss (18).
insert into RefuelingWorkerOutputTemp (
	yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, pollutantID, processID,
	sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate
)
select
	mwo.yearID, mwo.monthID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, mwo.zoneID, mwo.linkID, 0 as pollutantID, 18 as processID,
	mwo.sourceTypeID, mwo.regClassID, mwo.fuelTypeID, mwo.modelYearID, mwo.roadTypeID, 
	null as SCC,
	(adjustedVaporRate * mwo.emissionQuant / (energyContent * fuelDensity)) as emissionQuant,
	(adjustedVaporRate * mwo.emissionRate  / (energyContent * fuelDensity)) as emissionRate
from MOVESWorkerOutput mwo
inner join RefuelingFuelType rft on (rft.fuelTypeID=mwo.fuelTypeID and rft.monthID=mwo.monthID)
inner join RefuelingDisplacement rd on (rd.modelYearID = mwo.modelYearID AND
                                        rd.regClassID = mwo.regClassID and
                                        rd.sourceTypeID = mwo.sourceTypeID and
										rd.fuelTypeID=rft.fuelTypeID and 
										rd.ageID=mwo.yearID - mwo.modelYearID AND
										rd.monthID=mwo.monthID and 
										rd.hourID=mwo.hourID)
where mwo.processID in (1,2,90,91)
and mwo.pollutantID=91;

insert into MOVESWorkerOutput (
	yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, pollutantID, processID,
	sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate
)
select
	yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, rdp.pollutantID, processID,
	sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate
from RefuelingWorkerOutputTemp, RefuelingDisplacementPollutant rdp;
-- End Section RefuelingDisplacementVaporLoss

-- Section RefuelingSpillageLoss
truncate RefuelingWorkerOutputTemp;

-- @algorithm emissions = (adjustedSpillRate * Total Energy Consumption (91)) / (energyContent * fuelDensity).
-- @condition Refueling Spillage Loss (19).
insert into RefuelingWorkerOutputTemp (
	yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, pollutantID, processID,
	sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate
)
select
	mwo.yearID, mwo.monthID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, mwo.zoneID, mwo.linkID, 0 as pollutantID, 19 as processID,
	mwo.sourceTypeID, mwo.regClassID, mwo.fuelTypeID, mwo.modelYearID, mwo.roadTypeID, 
	null as SCC,
	(adjustedSpillRate * mwo.emissionQuant / (energyContent * fuelDensity)) as emissionQuant,
	(adjustedSpillRate * mwo.emissionRate  / (energyContent * fuelDensity)) as emissionRate
from MOVESWorkerOutput mwo
inner join RefuelingFuelType rft on (rft.fuelTypeID=mwo.fuelTypeID and rft.monthID=mwo.monthID)
inner join RefuelingSpillage rs on (
	rs.fuelTypeID=rft.fuelTypeID and rs.sourceTypeID=mwo.sourceTypeID and rs.modelYearID=mwo.modelYearID)
where mwo.processID in (1,2,90,91)
and mwo.pollutantID=91;

insert into MOVESWorkerOutput (
	yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, pollutantID, processID,
	sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate
)
select
	yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, rsp.pollutantID, processID,
	sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate
from RefuelingWorkerOutputTemp, RefuelingSpillagePollutant rsp;
-- End Section RefuelingSpillageLoss

-- End Section Processing

-- Section Cleanup
drop table if exists RefuelingRunSpecHour;
drop table if exists RefuelingRunSpecMonth;
drop table if exists RefuelingCountyYear;
drop table if exists RefuelingFuelType;

-- Section RefuelingDisplacementVaporLoss
drop table if exists RefuelingTemp;
drop table if exists RefuelingAverageRVP;
drop table if exists RefuelingDisplacement;
drop table if exists RefuelingDisplacementPollutant;
-- End Section RefuelingDisplacementVaporLoss

-- Section RefuelingSpillageLoss
drop table if exists RefuelingSpillage;
drop table if exists RefuelingSpillagePollutant;
-- End Section RefuelingSpillageLoss

drop table if exists RefuelingWorkerOutputTemp;
-- End Section Cleanup
