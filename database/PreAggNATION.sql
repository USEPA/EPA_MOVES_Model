/* ***********************************************************************************************************************
-- MySQL Script file to pre-aggregate the county level database to the National Level
--    (but preserving the roadtype (Link) distinctions which exist within counties.)
-- An attempt is made to weight some aggregations by activity.
-- These weightings assume that all states and counties are included
--
-- Author Wesley Faler
-- Author Mitch Cumberworth
-- Author Gwo Shyu
-- Author Jarrod Brown, Michael Aldridge, Daniel Bizer-Cox, Evan Murray
-- Version 2019-04-22
-- *********************************************************************************************************************** */


DROP TABLE IF EXISTS SurrogateActivity;
DROP TABLE IF EXISTS OldCounty;
DROP TABLE IF EXISTS OldCountyYear;
DROP TABLE IF EXISTS OldLink;
DROP TABLE IF EXISTS AggZoneMonthHour;   
DROP TABLE IF EXISTS OldOpModeDistribution; 
DROP TABLE IF EXISTS AggZoneRoadType;
DROP TABLE IF EXISTS AggFuelSupply;
DROP TABLE IF EXISTS AggNRFuelSupply;
DROP TABLE IF EXISTS OldIMCoverage;  
DROP TABLE IF EXISTS AggSHO;
DROP TABLE IF EXISTS AggSourceHours;
DROP TABLE IF EXISTS AggStarts;
DROP TABLE IF EXISTS AggAverageTankTemperature;
DROP TABLE IF EXISTS AggSoakActivityFraction;
DROP TABLE IF EXISTS AggFuelUsageFraction;
DROP TABLE IF EXISTS OldTotalIdleFraction;
DROP TABLE IF EXISTS OldHotellingHourFraction;
DROP TABLE IF EXISTS AggHotellingHourFraction;
DROP TABLE IF EXISTS OldHotellingMonthAdjust;
DROP TABLE IF EXISTS AggHotellingMonthAdjust;
DROP TABLE IF EXISTS OldHotellingHoursPerDay;
DROP TABLE IF EXISTS AggHotellingHoursPerDay;
DROP TABLE IF EXISTS OldHotellingAgeFraction;
DROP TABLE IF EXISTS AggHotellingAgeFraction;
DROP TABLE IF EXISTS OldHotellingActivityDistribution;
DROP TABLE IF EXISTS AggHotellingActivityDistribution;

-- Since "Nation" does not include the Virgin Islands or
-- Puerto Rico, remove their information from State, County, Zone,
-- ZoneRoadType, and all Nonroad tables as well.
delete from zone
using state
inner join county on (county.stateID=state.stateID)
inner join zone on (zone.countyID=county.countyID)
where state.stateID in (72,78);

delete from zoneroadtype
where round(zoneID/10000, 0) in (72,78);

delete from nrBaseYearEquipPopulation where stateID in (72,78);
delete from nrGrowthPatternFinder where stateID in (72,78);
delete from nrMonthAllocation where stateID in (72,78);
delete from nrStateSurrogate where stateID in (72,78);

delete from county
using state
inner join county on (county.stateID=state.stateID)
where state.stateID in (72,78);

delete from state
where state.stateID in (72,78);


--
-- FLUSH TABLES;
--
-- Create a table to be used for activity-weighting by zone or county
--
-- SELECT "Making SurrogateActivity" AS MARKER_POINT;

create table SurrogateActivity (
	zoneID int not null,
	countyID int not null,
	actFract double not null,
	primary key (zoneID, countyID),
	key (countyID, zoneID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into SurrogateActivity (zoneID, countyID, actFract)
  SELECT zoneID, countyID, startAllocFactor as actFract from Zone;
  
drop table if exists SurrogateActivityTotal;
create table SurrogateActivityTotal
select sum(actFract) as nationalActivityFraction
from SurrogateActivity;


-- 
-- SurrogateStateActivity Table
-- 
-- SELECT "Making SurrogateStateActivity" AS MARKER_POINT;

drop table if exists SurrogateStateActivity;
create table SurrogateStateActivity (
	stateID int not null,
	actFract double not null,
	primary key (stateID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into SurrogateStateActivity (stateID, actFract)
select c.stateID, 
	case when nationalActivityFraction <= 0 then 0 
	else sum(actFract)/nationalActivityFraction
	end as actFract
from SurrogateActivity sa
inner join County c using (countyID)
inner join SurrogateActivityTotal t
where t.nationalActivityFraction > 0
group by c.stateID;

-- 
-- SurrogateCountyActivity Table
-- 
-- SELECT "Making SurrogateCountyActivity" AS MARKER_POINT;
drop table if exists SurrogateCountyActivity;
create table SurrogateCountyActivity (
	countyID int not null,
	actFract double not null,
	primary key (countyID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into SurrogateCountyActivity (countyID, actFract)
select countyID, 
	case when nationalActivityFraction <= 0 then 0 
	else sum(actFract)/nationalActivityFraction
	end as actFract
from SurrogateActivity sa
inner join SurrogateActivityTotal t
where t.nationalActivityFraction > 0
group by countyID;

-- 
-- SurrogateRegionActivity Table
-- 
-- SELECT "Making SurrogateRegionActivity" AS MARKER_POINT;
drop table if exists SurrogateRegionActivity;
create table SurrogateRegionActivity (
	fuelRegionID int not null,
	fuelYearID int not null,
	actFract double not null,
	primary key (fuelRegionID, fuelYearID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into SurrogateRegionActivity (fuelRegionID, fuelYearID, actFract)
select regionID as fuelRegionID, fuelYearID, sum(actFract)
from SurrogateActivity
inner join RegionCounty using (countyID)
where regionCodeID=1
group by regionID, fuelYearID;

drop table if exists SurrogateRegionActivityTotal;
create table SurrogateRegionActivityTotal (
	fuelYearID int not null primary key,
	actFractTotal double not null
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
insert into SurrogateRegionActivityTotal (fuelYearID, actFractTotal)
select fuelYearID, sum(actFract)
from SurrogateRegionActivity
group by fuelYearID;

update SurrogateRegionActivity, SurrogateRegionActivityTotal
set actFract = actFract/actFractTotal
where SurrogateRegionActivity.fuelYearID = SurrogateRegionActivityTotal.fuelYearID;
    
--
-- State Table
--
-- SELECT "Making State" AS MARKER_POINT;
TRUNCATE State;
INSERT INTO State (stateID, stateName, stateAbbr, idleRegionID)
  VALUES (0, "Nation", "US", 1);
FLUSH TABLE State;
  
--
-- County Table
--
-- SELECT "Making County" AS MARKER_POINT;
CREATE TABLE OldCounty SELECT * FROM County;
TRUNCATE County;
INSERT INTO County (countyID, stateID, countyName, altitude, GPAFract, 
		barometricPressure, barometricPressureCV, countyTypeID)
  SELECT 0, 0, "Nation", "L", SUM(GPAFract*ActFract) AS GPAFract,
  		SUM(barometricPressure*ActFract) AS barometricPressure, NULL AS barometricPressureCV,
  		1 as countyTypeID
  FROM OldCounty INNER JOIN SurrogateActivity USING (countyID);
FLUSH TABLE County;
  
--
-- CountyYear Table
--
-- SELECT "Making CountyYear" AS MARKER_POINT;
CREATE TABLE OldCountyYear SELECT * from CountyYear;
TRUNCATE CountyYear;
INSERT INTO CountyYear (countyID, yearID, refuelingVaporProgramAdjust, refuelingSpillProgramAdjust)
	SELECT 0 AS countyID, yearID, sum(refuelingVaporProgramAdjust*ActFract), sum(refuelingSpillProgramAdjust*ActFract)
	FROM OldCountyYear INNER JOIN SurrogateActivity USING (countyID)
    GROUP BY yearID;
FLUSH TABLE CountyYear;
  
--
-- Zone Table
--
-- SELECT "Making Zone" AS MARKER_POINT;
TRUNCATE Zone;
INSERT INTO Zone (zoneID, countyID, startAllocFactor, idleAllocFactor, SHPAllocFactor)
  VALUES (0, 0, 1.0, 1.0, 1.0);
FLUSH TABLE Zone;
  
-- 
-- Link Table
-- 
-- SELECT "Making Link" AS MARKER_POINT;
CREATE Table OldLink
  SELECT * from Link;  
TRUNCATE Link;
INSERT INTO Link (linkID, countyID, zoneID, roadTypeID, 
    linkLength,linkVolume, linkAvgSpeed, linkDescription, linkAvgGrade)
  SELECT roadTypeID AS linkID,0 as countyID, 0 AS zoneID, roadTypeID as roadTypeID,
    NULL AS linkLength, NULL AS linkVolume, SUM(linkAvgSpeed * actFract) AS linkAvgSpeed,
    NULL AS linkDescription,
    SUM(linkAvgGrade * actFract) AS linkAvgGrade
  FROM OldLink INNER JOIN SurrogateActivity USING(zoneID)
  GROUP BY roadTypeID;
FLUSH TABLE Link;

-- 
-- ZoneMonthHour
--
-- SELECT "Making ZoneMonthHour" AS MARKER_POINT;
CREATE Table AggZoneMonthHour (
	monthID SMALLINT,
	hourID SMALLINT,
	temperature DOUBLE,
	relHumidity DOUBLE
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggZoneMonthHour	
  SELECT monthID, hourID, sum(temperature*actFract)/nationalActivityFraction as temperature,
    sum(relHumidity*actFract)/nationalActivityFraction AS relHumidity
  FROM ZoneMonthHour INNER JOIN SurrogateActivity USING (zoneID) JOIN SurrogateActivityTotal
  GROUP BY monthID, hourID;
CREATE UNIQUE INDEX index1 ON AggZoneMonthHour (monthID, hourID);  
TRUNCATE ZoneMonthHour;
REPLACE INTO ZoneMonthHour (monthID, zoneID, hourID, temperature,
    relHumidity, molWaterFraction, heatIndex, specificHumidity)
  SELECT monthID, 0 AS zoneID, hourID, temperature,
    relHumidity, 0.0 AS molWaterFraction, 
    0.0 as heatIndex, 0.0 as specificHumidity 
  FROM AggZoneMonthHour 
  GROUP BY monthID, hourID;
FLUSH TABLE ZoneMonthHour;

--
-- OpModeDistribution
--
-- SELECT "Making OpModeDistribution" AS MARKER_POINT;
CREATE Table OldOpModeDistribution 
  SELECT omd.*, link.roadTypeID, link.zoneID 
  FROM OpModeDistribution AS omd INNER JOIN OldLink as link USING (linkID);
TRUNCATE OpModeDistribution;
INSERT INTO OpModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, 
    opModeFraction, opModeFractionCV, isUserInput)
  SELECT sourceTypeID, hourDayID, roadTypeID AS linkID, polProcessID, opModeID,
    SUM(opModeFraction * actFract) AS opModeFraction, NULL AS opModeFractionCV,
    "Y" AS isUserInput
  FROM OldOpModeDistribution INNER JOIN SurrogateActivity USING (zoneID)
  GROUP BY sourceTypeID, hourDayID, roadTypeID, polProcessID, opModeID;
FLUSH TABLE OpModeDistribution;
  
--
-- ZoneRoadType
--
-- SELECT "Making ZoneRoadType" AS MARKER_POINT;
CREATE TABLE AggZoneRoadType (
	roadTypeID SMALLINT,
	SHOAllocFactor double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggZoneRoadType	
  SELECT  roadTypeID, sum(1.0000000000000 * SHOAllocFactor) AS SHOAllocFactor
  FROM ZoneRoadType 
  GROUP BY roadTypeID ;
TRUNCATE ZoneRoadType;
REPLACE INTO ZoneRoadType (zoneID, roadTypeID, SHOAllocFactor)
  SELECT 0 AS zoneID,  roadTypeID, SHOAllocFactor
  FROM AggZoneRoadType;
FLUSH TABLE ZoneRoadType;
   
--
-- Fuel Supply
--
-- Note: algorithm is specific to particular default values used.
-- SELECT "Making FuelSupply" AS MARKER_POINT;
--  Creating table explicitly to control column types and avoid significance problems
CREATE TABLE AggFuelSupply (
	fuelYearID SMALLINT,
	monthGroupID SMALLINT,
	fuelFormulationID INT(11),
	haveFract double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggFuelSupply
  SELECT FuelSupply.fuelYearID, monthGroupID,fuelFormulationID, 
    sum(marketShare*actFract) as haveFract
  FROM FuelSupply
  INNER JOIN SurrogateRegionActivity USING (fuelRegionID,fuelYearID)
  GROUP BY fuelYearID, monthGroupID, fuelFormulationID;
TRUNCATE FuelSupply;  
REPLACE INTO FuelSupply (fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID, 
   marketShare, marketShareCV)
  SELECT 0 AS fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID, 
    haveFract AS marketShare, 
    NULL AS marketShareCV
  FROM AggFuelSupply; 
FLUSH TABLE FuelSupply;

--
-- Nonroad Fuel Supply
--
-- Note: algorithm is specific to particular default values used.
-- SELECT "Making NRFuelSupply" AS MARKER_POINT;
--  Creating table explicitly to control column types and avoid significance problems
CREATE TABLE AggNRFuelSupply (
	fuelYearID SMALLINT,
	monthGroupID SMALLINT,
	fuelFormulationID INT(11),
	haveFract double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggNRFuelSupply
  SELECT NRFuelSupply.fuelYearID, monthGroupID,fuelFormulationID, 
    sum(marketShare*actFract) as haveFract
  FROM NRFuelSupply
  INNER JOIN SurrogateRegionActivity USING (fuelRegionID,fuelYearID)
  GROUP BY fuelYearID, monthGroupID, fuelFormulationID;
TRUNCATE NRFuelSupply;  
REPLACE INTO NRFuelSupply (fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID, 
   marketShare, marketShareCV)
  SELECT 0 AS fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID, 
    haveFract AS marketShare, 
    NULL AS marketShareCV
  FROM AggNRFuelSupply; 
FLUSH TABLE NRFuelSupply;

TRUNCATE Region;
insert into Region (regionID, description) values (0,'Aggregated Nation');

TRUNCATE RegionCounty;
insert into RegionCounty (regionID, countyID, regionCodeID, fuelYearID)
select distinct 0 as regionID, 0 as countyID, 1 as regionCodeID, fuelYearID
from fuelSupply;

insert into RegionCounty (regionID, countyID, regionCodeID, fuelYearID)
select distinct 0 as regionID, 0 as countyID, 2 as regionCodeID, fuelYearID
from fuelSupply;

--
-- Fuel Usage
--
-- SELECT "Making FuelUsageFraction" AS MARKER_POINT;
--  Creating table explicitly to control column types and avoid significance problems
-- 
-- Because fuel usage fraction is used early in the TAG, reductions here for PR/VI propagate to all
-- activity calculations (i.e., for every activityTypeID). This is why this table is not normalized
-- to total national activity.
-- 
CREATE TABLE AggFuelUsageFraction (
	fuelYearID SMALLINT,
	modelYearGroupID int,
	sourceBinFuelTypeID smallint,
	fuelSupplyFuelTypeID smallint,
	usageFraction double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggFuelUsageFraction
  SELECT f.fuelYearID, modelYearGroupID, sourceBinFuelTypeID, fuelSupplyFuelTypeID,
    sum(usageFraction*actFract) as usageFraction
  FROM FuelUsageFraction AS f INNER JOIN SurrogateActivity USING(countyID)
  GROUP BY fuelYearID, modelYearGroupID, sourceBinFuelTypeID, fuelSupplyFuelTypeID;
TRUNCATE FuelUsageFraction;
REPLACE INTO FuelUsageFraction (countyID, fuelYearID, modelYearGroupID, sourceBinFuelTypeID, fuelSupplyFuelTypeID,
   usageFraction)
  SELECT 0 AS countyID, fuelYearID, modelYearGroupID, sourceBinFuelTypeID, fuelSupplyFuelTypeID,
    least(usageFraction,1)
  FROM AggFuelUsageFraction;
FLUSH TABLE FuelUsageFraction;
  
--
-- IMCoverage
--
-- SELECT "Making IMCoverage Table" AS MARKER_POINT;
CREATE TABLE OldIMCoverage SELECT * FROM IMCoverage WHERE useIMyn = 'Y';
CREATE INDEX OldIMCoverageIndex1 ON OldIMCoverage (countyID);
TRUNCATE IMCoverage;  
-- When aggregated, IM programs may overlap.  This is better than extending model years
-- or forcing all to a particular IMFactor entry.  The overlap is handled within each
-- calculator.
DROP TABLE IMCoverage;
CREATE TABLE IMCoverage (
  stateID int(11) default NULL,
  countyID int(11) NOT NULL,
  yearID smallint(6) NOT NULL,
  polProcessID int NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  sourceTypeID smallint(6) NOT NULL,
  IMProgramID smallint(6) NOT NULL default '0',
  inspectFreq smallint(6) default NULL,
  testStandardsID smallint(6) default NULL,
  begModelYearID smallint(4) default NULL,
  endModelYearID smallint(4) default NULL,
  useIMyn char(1) NOT NULL default 'Y',
  complianceFactor float default NULL,
  KEY XPKIMCoverage (polProcessID,countyID,yearID,sourceTypeID,fuelTypeID,IMProgramID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Add back all of the old IMCoverage records, but use the pseudo county's ID
INSERT INTO IMCoverage (stateID, countyID, yearID, polProcessID, fuelTypeID,
	sourceTypeID, IMProgramID, inspectFreq, testStandardsID,
	begModelYearID, endModelYearID, useIMyn,
	complianceFactor)
SELECT 0 as stateID, 0 as countyID, yearID, polProcessID, fuelTypeID,
	sourceTypeID, IMProgramID, inspectFreq, testStandardsID,
	begModelYearID, endModelYearID, useIMyn,
	(complianceFactor*actFract)/nationalActivityFraction as complianceFactor
FROM OldIMCoverage
INNER JOIN SurrogateActivity USING(countyID) JOIN SurrogateActivityTotal;

--
--  SHO    
--
-- SELECT "Making SHO" AS MARKER_POINT;

CREATE TABLE AggSHO (
	hourDayID SMALLINT,
	monthID SMALLINT,
	yearID SMALLINT,
	ageID SMALLINT,
	roadTypeID SMALLINT,
	sourceTypeID SMALLINT,
	SHO double,
	distance double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggSHO
  SELECT hourDayID, monthID, yearID, ageID, roadTypeID, sourceTypeID, 
    sum(SHO) AS SHO, sum(distance) AS distance
  FROM SHO INNER JOIN OldLink USING(linkID)
  GROUP BY hourDayID, monthID, yearID, ageID, roadTypeID, sourceTypeID;
TRUNCATE SHO;
INSERT INTO SHO (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, SHO, SHOCV, distance, isUserInput)
  SELECT hourDayID, monthID, yearID, ageID, roadTypeID AS linkID, 
    sourceTypeID, SHO, NULL AS SHOCV, distance, "Y" AS isUserInput
  FROM AggSHO;
FLUSH TABLE SHO;

--
--  SourceHours    
--
-- SELECT "Making SourceHours" AS MARKER_POINT;

CREATE TABLE AggSourceHours (
	hourDayID SMALLINT,
	monthID SMALLINT,
	yearID SMALLINT,
	ageID SMALLINT,
	roadTypeID SMALLINT,
	sourceTypeID SMALLINT,
	sourceHours double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggSourceHours
  SELECT hourDayID, monthID, yearID, ageID, roadTypeID, sourceTypeID, 
    sum(sourceHours) AS sourceHours
  FROM SourceHours INNER JOIN OldLink USING(linkID)
  GROUP BY hourDayID, monthID, yearID, ageID, roadTypeID, sourceTypeID;
TRUNCATE SourceHours;
INSERT INTO SourceHours (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, sourceHours, sourceHoursCV,isUserInput)
  SELECT hourDayID, monthID, yearID, ageID, roadTypeID AS linkID, 
    sourceTypeID, sourceHours, NULL AS sourceHoursCV, "Y" AS isUserInput
  FROM AggSourceHours;
FLUSH TABLE SourceHours;
  
--
--  Starts
--
-- SELECT "Making Starts" AS MARKER_POINT;
CREATE TABLE AggStarts (
	hourDayID SMALLINT,
	monthID SMALLINT,
	yearID SMALLINT,
	ageID SMALLINT,
	sourceTypeID SMALLINT,
	starts double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggStarts
  SELECT hourDayID, monthID, yearID, ageID, sourceTypeID, 
    sum(starts) AS starts
  FROM Starts 
  GROUP BY hourDayID, monthID, yearID, ageID, sourceTypeID;
TRUNCATE Starts;
REPLACE INTO Starts (hourDayID, monthID, yearID, ageID, zoneID, 
    sourceTypeID, starts, startsCV, isUserInput)
  SELECT hourDayID, monthID, yearID, ageID, 0 AS zoneID, 
    sourceTypeID, starts, NULL AS startsCV, "Y" AS isUserInput
  FROM AggStarts;
FLUSH TABLE Starts;

-- 
-- AverageTankTemperature
--
-- SELECT "Making AverageTankTemperature" AS MARKER_POINT;
CREATE Table AggAverageTankTemperature (
	tankTemperatureGroupID SMALLINT,
	monthID SMALLINT,
	hourDayID SMALLINT,
	opModeID SMALLINT,
	averageTankTemperature FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggAverageTankTemperature
  SELECT tankTemperatureGroupID, monthID, hourDayID, opModeID,
    sum(averageTankTemperature*actFract)/nationalActivityFraction as averageTankTemperature
  FROM AverageTankTemperature INNER JOIN SurrogateActivity USING (zoneID) JOIN SurrogateActivityTotal
  GROUP BY tankTemperatureGroupID, monthID, hourDayID, opModeID;


TRUNCATE AverageTankTemperature;
REPLACE INTO AverageTankTemperature (tankTemperatureGroupID, zoneID, 
		monthID, hourDayID, opModeID, averageTankTemperature, averageTankTemperatureCV,
		isUserInput)
  SELECT tankTemperatureGroupID, 0 AS zoneID, monthID, hourDayID, opModeID, 
    averageTankTemperature, NULL AS averageTankTemperatureCV, 'Y' AS isUserInput
  FROM AggAverageTankTemperature;
FLUSH TABLE AverageTankTemperature;

-- 
-- SoakActivityFraction
--
-- SELECT "Making SoakActivityFraction" AS MARKER_POINT;
CREATE Table AggSoakActivityFraction (
	sourceTypeID SMALLINT,
	monthID SMALLINT,
	hourDayID SMALLINT,
	opModeID SMALLINT,
	soakActivityFraction double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggSoakActivityFraction
  SELECT sourceTypeID, monthID, hourDayID, opModeID,
    sum(soakActivityFraction*actFract)/nationalActivityFraction as soakActivityFraction
  FROM SoakActivityFraction INNER JOIN SurrogateActivity USING (zoneID) JOIN SurrogateActivityTotal
  GROUP BY sourceTypeID, monthID, hourDayID, opModeID;
TRUNCATE SoakActivityFraction;
REPLACE INTO SoakActivityFraction (sourceTypeID, zoneID, 
		monthID, hourDayID, opModeID, soakActivityFraction, soakActivityFractionCV, isUserInput)
  SELECT sourceTypeID, 0 AS zoneID, monthID, hourDayID, opModeID, 
    soakActivityFraction, NULL AS soakActivityFractionCV, 'Y' AS isUserInput
  FROM AggSoakActivityFraction;
FLUSH TABLE SoakActivityFraction;

-- 
-- ColdSoakTankTemperature
--
-- SELECT "Making ColdSoakTankTemperature" AS MARKER_POINT;
CREATE Table AggColdSoakTankTemperature (
	monthID SMALLINT,
	hourID SMALLINT,
	coldSoakTankTemperature FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggColdSoakTankTemperature
  SELECT monthID, hourID, sum(coldSoakTankTemperature*actFract)/nationalActivityFraction as coldSoakTankTemperature
  FROM ColdSoakTankTemperature INNER JOIN SurrogateActivity USING (zoneID) JOIN SurrogateActivityTotal
  GROUP BY monthID, hourID;
CREATE UNIQUE INDEX index1 ON AggColdSoakTankTemperature (monthID, hourID);  
TRUNCATE ColdSoakTankTemperature;
REPLACE INTO ColdSoakTankTemperature (monthID, zoneID, hourID, coldSoakTankTemperature)
  SELECT monthID, 0 AS zoneID, hourID, coldSoakTankTemperature
  FROM AggColdSoakTankTemperature
  GROUP BY monthID, hourID;
FLUSH TABLE ColdSoakTankTemperature;

-- 
-- ColdSoakInitialHourFraction
--
-- SELECT "Making ColdSoakInitialHourFraction" AS MARKER_POINT;
CREATE Table AggColdSoakInitialHourFraction (
	sourceTypeID SMALLINT,
	monthID SMALLINT,
	hourDayID SMALLINT,
	initialHourDayID SMALLINT,
	coldSoakInitialHourFraction FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggColdSoakInitialHourFraction
  SELECT sourceTypeID, monthID, hourDayID, initialHourDayID, sum(coldSoakInitialHourFraction*actFract)/nationalActivityFraction as coldSoakInitialHourFraction
  FROM ColdSoakInitialHourFraction INNER JOIN SurrogateActivity USING (zoneID) JOIN SurrogateActivityTotal
  GROUP BY sourceTypeID, monthID, hourDayID, initialHourDayID;
CREATE UNIQUE INDEX index1 ON AggColdSoakInitialHourFraction (sourceTypeID, monthID, hourDayID, initialHourDayID);
TRUNCATE ColdSoakInitialHourFraction;
REPLACE INTO ColdSoakInitialHourFraction (sourceTypeID, monthID, zoneID, hourDayID, initialHourDayID, 
	coldSoakInitialHourFraction, isUserInput)
  SELECT sourceTypeID, monthID, 0 AS zoneID, hourDayID, initialHourDayID, coldSoakInitialHourFraction,
  	'Y' as isUserInput
  FROM AggColdSoakInitialHourFraction 
  GROUP BY sourceTypeID, monthID, hourDayID, initialHourDayID;
FLUSH TABLE ColdSoakInitialHourFraction;

-- 
-- AverageTankGasoline
--
-- SELECT "Making AverageTankGasoline" AS MARKER_POINT;
CREATE Table AggAverageTankGasoline (
	fuelTypeID SMALLINT,
	fuelYearID SMALLINT,
	monthGroupID SMALLINT,
	ETOHVolume FLOAT,
	RVP FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggAverageTankGasoline
  SELECT fuelTypeID, fuelYearID, monthGroupID, 
  	sum(ETOHVolume*actFract)/nationalActivityFraction as ETOHVolume,
  	sum(RVP*actFract)/nationalActivityFraction as RVP
  FROM AverageTankGasoline INNER JOIN SurrogateActivity USING (zoneID) JOIN SurrogateActivityTotal
  GROUP BY fuelTypeID, fuelYearID, monthGroupID;
CREATE UNIQUE INDEX index1 ON AggAverageTankGasoline (fuelTypeID, fuelYearID, monthGroupID);
TRUNCATE AverageTankGasoline;
REPLACE INTO AverageTankGasoline (zoneID, fuelTypeID, fuelYearID, monthGroupID, ETOHVolume, RVP, isUserInput)
  SELECT 0 AS zoneID, fuelTypeID, fuelYearID, monthGroupID, ETOHVolume, RVP, 'Y' as isUserInput
  FROM AggAverageTankGasoline 
  GROUP BY fuelTypeID, fuelYearID, monthGroupID;
FLUSH TABLE AverageTankGasoline;

-- 
-- nrBaseYearEquipPopulation Table
-- 
-- SELECT "Making nrBaseYearEquipPopulation" AS MARKER_POINT;
DROP TABLE IF EXISTS OldnrBaseYearEquipPopulation;
CREATE Table OldnrBaseYearEquipPopulation
  SELECT * from nrBaseYearEquipPopulation;
TRUNCATE nrBaseYearEquipPopulation;
INSERT INTO nrBaseYearEquipPopulation (sourceTypeID, stateID, population, NRBaseYearID)
	SELECT sourceTypeID, 0 as stateID, sum(population) as population, NRBaseYearID
	FROM OldnrBaseYearEquipPopulation
	GROUP BY sourceTypeID, NRBaseYearID;
FLUSH TABLE nrBaseYearEquipPopulation;

-- 
-- nrGrowthPatternFinder Table
-- 
-- SELECT "Making nrGrowthPatternFinder" AS MARKER_POINT;
DROP TABLE IF EXISTS OldnrGrowthPatternFinder;
CREATE Table OldnrGrowthPatternFinder
  SELECT * from nrGrowthPatternFinder;
TRUNCATE nrGrowthPatternFinder;
INSERT INTO nrGrowthPatternFinder (SCC, stateID, growthPatternID)
	select SCC, 0 as stateID, min(growthPatternID) as growthPatternID
	from OldnrGrowthPatternFinder
	group by SCC;
FLUSH TABLE nrGrowthPatternFinder;

-- 
-- nrMonthAllocation Table
-- 
-- SELECT "Making nrMonthAllocation" AS MARKER_POINT;
TRUNCATE nrMonthAllocation;
INSERT INTO nrMonthAllocation (SCC, stateID, monthID, monthFraction)
	select SCC, stateID, monthID, monthFraction
	from nrUSMonthAllocation
	where stateID=0;
FLUSH TABLE nrMonthAllocation;

-- 
-- nrStateSurrogate Table
-- 
-- SELECT "Making nrStateSurrogate" AS MARKER_POINT;
DROP TABLE IF EXISTS OldnrStateSurrogate;
CREATE Table OldnrStateSurrogate
  SELECT * from nrStateSurrogate;
TRUNCATE nrStateSurrogate;
INSERT INTO nrStateSurrogate (surrogateID,stateID,countyID,surrogatequant,surrogateYearID)
	select surrogateID, 0 as stateID, 0 as countyID, sum(surrogatequant) as surrogatequant,surrogateYearID
	from OldnrStateSurrogate
	where stateID > 0 and countyID > 0
	and mod(countyID,1000) > 0
	group by surrogateID, surrogateYearID;
FLUSH TABLE nrStateSurrogate;

--
-- TotalIdleFraction
--
-- SELECT "Making TotalIdleFraction" AS MARKER_POINT;
CREATE TABLE OldTotalIdleFraction
  SELECT sourceTypeID, minModelYearID, maxModelYearID, monthID, dayID, idleRegionID, countyTypeID, totalIdleFraction
    FROM TotalIdleFraction;
TRUNCATE TotalIdleFraction;
INSERT INTO TotalIdleFraction (sourceTypeID, minModelYearID, maxModelYearID, monthID, dayID, idleRegionID, countyTypeID, totalIdleFraction)
  SELECT sourceTypeID,  minModelYearID, maxModelYearID, monthID, dayID, 1 as idleRegionID, countyTypeID, totalIdleFraction
  FROM OldTotalIdleFraction
  WHERE idleRegionID = 103;
FLUSH TABLE TotalIdleFraction;


--
-- HotellingHourFraction
--
CREATE TABLE OldHotellingHourFraction
  SELECT * FROM HotellingHourFraction;
-- Creating table explicitly to control column type and avoid significance problem
CREATE TABLE AggHotellingHourFraction (
	dayID SMALLINT,
	hourID SMALLINT,
	hourFraction double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggHotellingHourFraction
  SELECT dayID, hourID, sum(hourFraction*COALESCE(actFract, 1.0)) as hourFraction
  FROM OldHotellingHourFraction 
  LEFT JOIN SurrogateActivity USING(zoneID)
  GROUP BY dayID, hourID;
TRUNCATE HotellingHourFraction;
INSERT INTO HotellingHourFraction (zoneID, dayID, hourID, hourFraction)
  SELECT 0 AS zoneID, dayID, hourID, least(hourFraction,1.0)
  FROM AggHotellingHourFraction;
FLUSH TABLE HotellingHourFraction;

--
-- HotellingMonthAdjust
--
CREATE TABLE OldHotellingMonthAdjust
  SELECT * FROM HotellingMonthAdjust;
-- Creating table explicitly to control column type and avoid significance problem
CREATE TABLE AggHotellingMonthAdjust (
	monthID SMALLINT,
	monthAdjustment double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggHotellingMonthAdjust
  SELECT monthID, sum(monthAdjustment*COALESCE(actFract, 1.0)) as monthAdjustment
  FROM OldHotellingMonthAdjust 
  LEFT JOIN SurrogateActivity USING(zoneID)
  GROUP BY monthID;
TRUNCATE HotellingMonthAdjust;
INSERT INTO HotellingMonthAdjust (zoneID, monthID, monthAdjustment)
  SELECT 0 AS zoneID, monthID, monthAdjustment
  FROM AggHotellingMonthAdjust;
FLUSH TABLE HotellingMonthAdjust;

--
-- HotellingHoursPerDay. Do not need to weight activity, since HHPD contains total hours per typical day
--                       (not hours per day per vehicle), so just aggregate everything together.
--
CREATE TABLE OldHotellingHoursPerDay
  SELECT * FROM HotellingHoursPerDay AS hhpd;
-- Creating table explicitly to control column type and avoid significance problem
CREATE TABLE AggHotellingHoursPerDay (
    yearID SMALLINT,
	dayID SMALLINT,
	hotellinghoursperday double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggHotellingHoursPerDay
  SELECT yearID, dayID, sum(hotellinghoursperday) as hotellinghoursperday
  FROM OldHotellingHoursPerDay
  GROUP BY yearID, dayID;
TRUNCATE HotellingHoursPerDay;
INSERT INTO HotellingHoursPerDay (yearID, zoneID, dayID, hotellinghoursperday)
  SELECT yearID, 0 AS zoneID, dayID, hotellinghoursperday
  FROM AggHotellingHoursPerDay;
FLUSH TABLE HotellingHoursPerDay;

--
-- HotellingAgeFraction
--
CREATE TABLE OldHotellingAgeFraction
  SELECT * FROM HotellingAgeFraction;
-- Creating table explicitly to control column type and avoid significance problem
CREATE TABLE AggHotellingAgeFraction (
	ageID SMALLINT,
	ageFraction double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggHotellingAgeFraction
  SELECT ageID, sum(ageFraction*COALESCE(actFract, 1.0)) as ageFraction
  FROM OldHotellingAgeFraction 
  LEFT JOIN SurrogateActivity USING(zoneID)
  GROUP BY ageID;
TRUNCATE HotellingAgeFraction;
INSERT INTO HotellingAgeFraction (zoneID, ageID, ageFraction)
  SELECT 0 AS zoneID, ageID, least(ageFraction, 1.0) as ageFraction
  FROM AggHotellingAgeFraction;
FLUSH TABLE HotellingAgeFraction;

--
-- HotellingActivityDistribution
--
CREATE TABLE OldHotellingActivityDistribution
  SELECT * FROM HotellingActivityDistribution;
-- Creating table explicitly to control column type and avoid significance problem
CREATE TABLE AggHotellingActivityDistribution (
	fuelTypeID SMALLINT,
    beginModelYearID SMALLINT,
    endModelYearID SMALLINT,
    opModeID SMALLINT,
	opModeFraction FLOAT
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
INSERT INTO AggHotellingActivityDistribution
  SELECT fuelTypeID, beginModelYearID, endModelYearID, opModeID, sum(opModeFraction*COALESCE(actFract, 1.0)) as opModeFraction
  FROM OldHotellingActivityDistribution 
  LEFT JOIN SurrogateActivity USING(zoneID)
  GROUP BY fuelTypeID, beginModelYearID, endModelYearID, opModeID;
TRUNCATE HotellingActivityDistribution;
INSERT INTO HotellingActivityDistribution (zoneID, fuelTypeID, beginModelYearID, endModelYearID, opModeID, opModeFraction)
  SELECT 990000 AS zoneID, fuelTypeID, beginModelYearID, endModelYearID, opModeID, least(opModeFraction, 1.0) as opModeFraction
  FROM AggHotellingActivityDistribution;
FLUSH TABLE HotellingActivityDistribution;

--
-- Drop any New Tables Created 
--
-- DROP TABLE IF EXISTS SurrogateActivity;
DROP TABLE IF EXISTS OldCounty;
DROP TABLE IF EXISTS OldCountyYear;
DROP TABLE IF EXISTS OldLink;
DROP TABLE IF EXISTS AggZoneMonthHour;   
DROP TABLE IF EXISTS OldOpModeDistribution; 
DROP TABLE IF EXISTS AggZoneRoadType;
DROP TABLE IF EXISTS AggFuelSupply;
DROP TABLE IF EXISTS OldIMCoverage;  
DROP TABLE IF EXISTS AggSHO;
DROP TABLE IF EXISTS AggSourceHours;
DROP TABLE IF EXISTS AggStarts;
DROP TABLE IF EXISTS AggAverageTankTemperature;
DROP TABLE IF EXISTS AggSoakActivityFraction;
DROP TABLE IF EXISTS AggFuelUsageFraction;

drop table if exists SurrogateActivityTotal;
drop table if exists SurrogateStateActivity;
drop table if exists SurrogateCountyActivity;
DROP TABLE IF EXISTS OldnrBaseYearEquipPopulation;
DROP TABLE IF EXISTS OldnrGrowthPatternFinder;
DROP TABLE IF EXISTS OldnrFuelSupply;
DROP TABLE IF EXISTS OldnrStateSurrogate;
DROP TABLE IF EXISTS OldTotalIdleFraction;

DROP TABLE IF EXISTS OldHotellingHourFraction;
DROP TABLE IF EXISTS AggHotellingHourFraction;
DROP TABLE IF EXISTS OldHotellingMonthAdjust;
DROP TABLE IF EXISTS AggHotellingMonthAdjust;
DROP TABLE IF EXISTS OldHotellingHoursPerDay;
DROP TABLE IF EXISTS AggHotellingHoursPerDay;
DROP TABLE IF EXISTS OldHotellingAgeFraction;
DROP TABLE IF EXISTS AggHotellingAgeFraction;
DROP TABLE IF EXISTS OldHotellingActivityDistribution;
DROP TABLE IF EXISTS AggHotellingActivityDistribution;


-- FLUSH TABLES;

  
