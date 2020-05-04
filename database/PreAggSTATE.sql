/* ***********************************************************************************************************************
-- MySQL Script preaggregate the MOVESExecution database
--    to the STATE level after merging of user input databases by InputDataManager
--    and before any MasterLoopable objects are executed.
-- An attempt is made to weight some aggregations by activity.
-- Two tables involved have missing data default data handling.
--
-- Author Mitch Cumberworth
-- Author Gwo Shyu
-- Author Wesley Faler
-- Version 2015-04-22
-- *********************************************************************************************************************** */

-- FLUSH TABLES;

DROP TABLE IF EXISTS SurrogateActivity;
DROP TABLE IF EXISTS OldCounty;
DROP TABLE IF EXISTS OldLink;
DROP TABLE IF EXISTS OldZone;
DROP TABLE IF EXISTS AggZone;
DROP TABLE IF EXISTS OldYear;
DROP TABLE IF EXISTS OldZoneMonthHour;
DROP TABLE IF EXISTS OldOpModeDistribution; 
DROP TABLE IF EXISTS AggZoneRoadType;
DROP TABLE IF EXISTS OldFuelSupply;
DROP TABLE IF EXISTS AggFuelSupply; 
DROP TABLE IF EXISTS OldIMCoverage; 
DROP TABLE IF EXISTS AggSHO;
DROP TABLE IF EXISTS AggSourceHours;
DROP TABLE IF EXISTS AggStarts;
DROP TABLE IF EXISTS AggExtendedIdleHours; 
DROP TABLE IF EXISTS OldAverageTankTemperature;
DROP TABLE IF EXISTS OldSoakActivityFraction;
DROP TABLE IF EXISTS OldFuelUsageFraction;
DROP TABLE IF EXISTS AggFuelUsageFraction;

-- 
-- Create a table to be used for activity-weighting by county or zone 
--
-- Note that activity factors for a state must sum to unity.
-- First make a version of Zone that includes stateID

-- SELECT "Making SurrogateActivity Table" AS MARKER_POINT;

CREATE TABLE OldZone
  SELECT zoneID, County.countyID, stateID, startAllocFactor, idleAllocFactor, SHPAllocFactor
  FROM Zone INNER JOIN County USING (countyID);
CREATE UNIQUE INDEX index1 ON OldZone (zoneID);
CREATE INDEX index2 ON OldZone (countyID);

-- Now we can aggregate the Zone table by stateID
CREATE TABLE AggZone (
	zoneID INTEGER,
	startAllocFactor FLOAT,
	idleAllocFactor FLOAT,
	SHPAllocFactor FLOAT,
	stateID SMALLINT );
INSERT INTO AggZone
  SELECT stateID*10000 AS zoneID, 
    sum(startAllocFactor) AS startAllocFactor, sum(idleAllocFactor) AS idleAllocFactor,
    sum(SHPAllocFactor) AS SHPAllocFactor, stateID
  FROM OldZone GROUP BY stateID;
CREATE UNIQUE INDEX index1 ON AggZone (stateID);

-- Finally we can make the table we want
CREATE Table SurrogateActivity (
	zoneID INTEGER,
	countyID INTEGER,
	actFract FLOAT,
	primary key (zoneID, countyID),
	key (countyID) );
INSERT INTO SurrogateActivity	
  SELECT oz.zoneID, oz.countyID,  
  (ROUND(oz.startAllocFactor,6)/ROUND(az.startAllocFactor,6)) AS actFract
  FROM OldZone as oz INNER JOIN AggZone as az USING(stateID);
CREATE UNIQUE INDEX index1 ON SurrogateActivity (zoneID);
CREATE INDEX index2 ON SurrogateActivity (countyID);

-- 
-- SurrogateRegionActivity Table
-- 
-- SELECT "Making SurrogateRegionActivity" AS MARKER_POINT;
drop table if exists SurrogateRegionActivity;
create table SurrogateRegionActivity (
	stateID int not null,
	fuelRegionID int not null,
	fuelYearID int not null,
	actFract double not null,
	primary key (stateID, fuelYearID, fuelRegionID)
);

insert into SurrogateRegionActivity (stateID, fuelRegionID, fuelYearID, actFract)
select stateID, regionID as fuelRegionID, fuelYearID, sum(actFract)
from SurrogateActivity
inner join RegionCounty using (countyID)
inner join County using (countyID)
where regionCodeID=1
group by stateID, regionID, fuelYearID;

drop table if exists SurrogateRegionActivityTotal;
create table SurrogateRegionActivityTotal (
	stateID int not null,
	fuelYearID int not null,
	actFractTotal double not null,
	primary key (stateID, fuelYearID)
);
insert into SurrogateRegionActivityTotal (stateID, fuelYearID, actFractTotal)
select stateID, fuelYearID, sum(actFract)
from SurrogateRegionActivity
group by stateID, fuelYearID;

update SurrogateRegionActivity, SurrogateRegionActivityTotal
set actFract = actFract/actFractTotal
where SurrogateRegionActivity.fuelYearID = SurrogateRegionActivityTotal.fuelYearID
and SurrogateRegionActivity.stateID = SurrogateRegionActivityTotal.stateID;

--
-- State Table  - No changes required
--
  
--
-- County Table
--
-- SELECT "Making Aggregate County and Zone Tables" AS MARKER_POINT;
CREATE TABLE OldCounty SELECT County.*, stateName 
	FROM County INNER JOIN State USING (stateID);
TRUNCATE County;
INSERT INTO County (countyID, stateID, countyName, altitude, GPAFract, 
		barometricPressure, barometricPressureCV)
  SELECT stateID*1000 AS countyID, stateID, 
    stateName AS countyName, "L" AS altitude, sum(GPAFract*actFract) AS GPAFract,
    sum(barometricPressure*actFract) AS barometricPressure, NULL AS barometricPressureCV
  FROM OldCounty INNER JOIN SurrogateActivity USING (countyID)
  GROUP BY stateID;
FLUSH TABLE County;
  
--
-- CountyYear Table
--
CREATE TABLE OldYear SELECT DISTINCT yearID from CountyYear;
TRUNCATE CountyYear;
INSERT INTO CountyYear (countyID, yearID)
  SELECT countyID, yearID
  FROM County, OldYear;
FLUSH TABLE CountyYear;
  
--
-- Zone Table
--
TRUNCATE Zone;
INSERT INTO Zone (zoneID, countyID, startAllocFactor, idleAllocFactor, SHPAllocFactor)
  SELECT  zoneID, stateID*1000 AS countyID, startAllocFactor, idleAllocFactor, SHPAllocFactor
  FROM AggZone;
FLUSH TABLE Zone;
  
-- 
-- Link Table
-- 
-- SELECT "Making Aggregate Link Table"AS MARKER_POINT;
CREATE Table OldLink
  SELECT Link.*, stateID
  FROM Link INNER JOIN OldZone USING(zoneID);
CREATE UNIQUE INDEX index1 ON OldLink (linkID); 
TRUNCATE Link;
INSERT INTO Link (linkID, countyID, zoneID, roadTypeID, 
    linkLength,linkVolume, linkAvgSpeed, linkDescription, linkAvgGrade)
  SELECT (stateID*100000+roadTypeID) AS linkID,
    stateID*1000 AS countyID, 
    stateID*10000 AS zoneID,
    roadTypeID as roadTypeID,
    NULL AS linkLength, NULL AS linkVolume, 
    SUM(linkAvgSpeed * actFract) AS linkAvgSpeed,
    NULL as linkDescription,
    SUM(linkAvgGrade * actFract) AS linkAvgGrade
  FROM OldLink INNER JOIN SurrogateActivity USING(zoneID)
  GROUP BY stateID, roadTypeID;
FLUSH TABLE Link;


-- 
-- ZoneMonthHour
--
-- SELECT "Making Aggregate ZoneMonthHour Table" AS MARKER_POINT;
CREATE Table OldZoneMonthHour
  SELECT monthID, stateID, zmh.zoneid, hourID, temperature, relhumidity
  FROM ZoneMonthHour AS zmh INNER JOIN OldZone USING (zoneID);
TRUNCATE ZoneMonthHour;
INSERT INTO ZoneMonthHour (monthID, zoneID, hourID, temperature, temperatureCV,
    relHumidity, relativeHumidityCV, heatIndex, specificHumidity)
  SELECT monthID, stateID*10000 AS zoneID, hourID, 
  sum(temperature*actFract) AS temperature, NULL AS temperatureCV, 
  sum(relHumidity*actFract) AS relHumidity, NULL AS relativeHumidityCV,
    0.0 as heatIndex, 0.0 AS specificHumidity
  FROM OldZoneMonthHour INNER JOIN SurrogateActivity USING(zoneID)
  GROUP BY monthID, hourID, stateID;
FLUSH TABLE ZoneMonthHour;

--
-- OpModeDistribution
--
-- SELECT "Making Aggregate OpModeDistribution Table" AS MARKER_POINT;
CREATE Table OldOpModeDistribution 
  SELECT omd.*, link.roadTypeID, link.stateID, link.zoneID 
  FROM OpModeDistribution AS omd INNER JOIN OldLink as link USING (linkID);
TRUNCATE OpModeDistribution;
INSERT INTO OpModeDistribution (sourceTypeID, hourDayID,linkID, polProcessID, opModeID, 
    opModeFraction, opModeFractionCV, isUserInput)
  SELECT sourceTypeID, hourDayID, 
    (stateID*100000+roadTypeID) AS linkID, polProcessID, opModeID,
    SUM(opModeFraction * actFract) AS opModeFraction, NULL AS opModeFractionCV,
    "Y" AS isUserInput
  FROM OldOpModeDistribution INNER JOIN SurrogateActivity USING (zoneID)
  GROUP BY sourceTypeID, hourDayID, stateID, roadTypeID, polProcessID, opModeID;
FLUSH TABLE OpModeDistribution;
  
--
-- ZoneRoadType
--
-- SELECT "Making Aggregate ZoneRoadType Table" AS MARKER_POINT;
CREATE TABLE AggZoneRoadType (
	zoneID INTEGER,
	roadTypeID SMALLINT,
	SHOAllocFactor double);
INSERT INTO AggZoneRoadType	
  SELECT (stateID*10000) AS zoneID, roadTypeID,
    sum(SHOAllocFactor) AS SHOAllocFactor
  FROM ZoneRoadType INNER JOIN OldZone USING(zoneID)
  GROUP BY stateID, roadTypeID ;
TRUNCATE ZoneRoadType;
INSERT INTO ZoneRoadType (zoneID, roadTypeID, SHOAllocFactor)
  SELECT * FROM AggZoneRoadType;
FLUSH TABLE ZoneRoadType;
  
--
-- Fuel Supply
--
-- Note: algorithm is specific to particular default values used.
-- SELECT "Making Aggregate FuelSupply Table" AS MARKER_POINT;
CREATE TABLE OldFuelSupply
  SELECT distinct fs.*, stateID
  FROM FuelSupply AS fs
  INNER JOIN RegionCounty ON (regionID=fuelRegionID and regionCodeID=1 and RegionCounty.fuelYearID=fs.fuelYearID)
  INNER JOIN OldCounty USING(countyID);
-- Creating table explicitly to control column type and avoid significance problem
CREATE TABLE AggFuelSupply (
	stateID SMALLINT,
	fuelYearID SMALLINT,
	monthGroupID SMALLINT,
	fuelFormulationID SMALLINT,
	haveFract DOUBLE);
INSERT INTO AggFuelSupply
  SELECT stateID, fs.fuelYearID, monthGroupID,fuelFormulationID, 
    sum(marketShare*actFract) as haveFract
  FROM OldFuelSupply AS fs
  INNER JOIN SurrogateRegionActivity USING (stateID,fuelRegionID,fuelYearID)
  GROUP BY stateID, fs.fuelYearID, monthGroupID, fuelFormulationID;
TRUNCATE FuelSupply;  
INSERT INTO FuelSupply (fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID, 
   marketShare, marketShareCV)
  SELECT stateID AS fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID, 
    haveFract AS marketShare, 
    NULL AS marketShareCV
  FROM AggFuelSupply;
FLUSH TABLE FuelSupply;

truncate Region;
insert into Region (regionID,description)
select distinct stateID, 'State region'
from AggFuelSupply;

truncate RegionCounty;
insert into RegionCounty (regionID, countyID, regionCodeID, fuelYearID)
select distinct fuelRegionID as regionID, fuelRegionID*1000 as countyID, 1 as regionCodeID, fuelYearID
from fuelSupply;

insert into RegionCounty (regionID, countyID, regionCodeID, fuelYearID)
select distinct fuelRegionID as regionID, fuelRegionID*1000 as countyID, 2 as regionCodeID, fuelYearID
from fuelSupply;

--
-- Fuel Usage
--
-- SELECT "Making Aggregate FuelUsageFraction Table" AS MARKER_POINT;
CREATE TABLE OldFuelUsageFraction
  SELECT f.*, stateID
  FROM FuelUsageFraction AS f INNER JOIN OldCounty USING(countyID) ;
-- Creating table explicitly to control column type and avoid significance problem
CREATE TABLE AggFuelUsageFraction (
	stateID SMALLINT,
	fuelYearID SMALLINT,
	modelYearGroupID int,
	sourceBinFuelTypeID smallint,
	fuelSupplyFuelTypeID smallint,
	usageFraction double
);
INSERT INTO AggFuelUsageFraction
  SELECT stateID, f.fuelYearID, modelYearGroupID, sourceBinFuelTypeID, fuelSupplyFuelTypeID,
    sum(usageFraction*actFract) as usageFraction
  FROM OldFuelUsageFraction AS f INNER JOIN SurrogateActivity USING(countyID)
  GROUP BY stateID, fuelYearID, modelYearGroupID, sourceBinFuelTypeID, fuelSupplyFuelTypeID;
TRUNCATE FuelUsageFraction;
INSERT INTO FuelUsageFraction (countyID, fuelYearID, modelYearGroupID, sourceBinFuelTypeID, fuelSupplyFuelTypeID,
   usageFraction)
  SELECT (stateID*1000) AS countyID, fuelYearID, modelYearGroupID, sourceBinFuelTypeID, fuelSupplyFuelTypeID,
    least(usageFraction,1.0)
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
  useIMyn char(1) NOT NULL default 'N',
  complianceFactor float default NULL,
  KEY XPKIMCoverage (polProcessID,countyID,yearID,sourceTypeID,fuelTypeID,IMProgramID)
);

-- Add back all of the old IMCoverage records, but use the pseudo county's ID
INSERT INTO IMCoverage (stateID, countyID, yearID, polProcessID, fuelTypeID,
	sourceTypeID, IMProgramID, inspectFreq, testStandardsID,
	begModelYearID, endModelYearID, useIMyn,
	complianceFactor)
SELECT stateID, (stateID*1000) as countyID, yearID, polProcessID, fuelTypeID,
	sourceTypeID, IMProgramID, inspectFreq, testStandardsID,
	begModelYearID, endModelYearID, useIMyn,
	(complianceFactor*actFract) as complianceFactor
FROM OldIMCoverage
INNER JOIN SurrogateActivity USING(countyID);
   
--
--  SHO    
--
-- SELECT "Making Aggregate SHO Table" AS MARKER_POINT;
CREATE TABLE AggSHO (
	hourDayID SMALLINT,
	monthID SMALLINT,
	yearID SMALLINT,
	ageID SMALLINT,
	stateID SMALLINT,
	roadTypeID SMALLINT,
	sourceTypeID SMALLINT,
	SHO FLOAT,
	distance FLOAT);
INSERT INTO AggSHO	
  SELECT hourDayID, monthID, yearID, ageID, stateID, roadTypeID, sourceTypeID, 
    sum(SHO) AS SHO, sum(distance) AS distance
  FROM SHO INNER JOIN OldLink USING(linkID)
  GROUP BY hourDayID, monthID, yearID, ageID, stateID, roadTypeID, sourceTypeID;
TRUNCATE SHO;
INSERT INTO SHO (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, SHO, SHOCV, distance, isUserInput)
  SELECT hourDayID, monthID, yearID, ageID,
    (stateID*100000+roadTypeID) AS linkID, 
    sourceTypeID, SHO, NULL AS SHOCV, distance, "Y" AS isUserInput
  FROM AggSHO;
FLUSH TABLE SHO;

--
--  SourceHours    
--
-- SELECT "Making Aggregate SourceHours Table" AS MARKER_POINT;
CREATE TABLE AggSourceHours (
	hourDayID SMALLINT,
	monthID SMALLINT,
	yearID SMALLINT,
	ageID SMALLINT,
	stateID SMALLINT,
	roadTypeID SMALLINT,
	sourceTypeID SMALLINT,
	sourceHours FLOAT);
INSERT INTO AggSourceHours	
  SELECT hourDayID, monthID, yearID, ageID, stateID, roadTypeID, sourceTypeID, 
    sum(sourceHours) AS sourceHours
  FROM SourceHours INNER JOIN OldLink USING(linkID)
  GROUP BY hourDayID, monthID, yearID, ageID, stateID, roadTypeID, sourceTypeID;
TRUNCATE SourceHours;
INSERT INTO SourceHours (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, sourceHours, sourceHoursCV,isUserInput)
  SELECT hourDayID, monthID, yearID, ageID,
    (stateID*100000+roadTypeID) AS linkID, 
    sourceTypeID, sourceHours, NULL AS sourceHoursCV,"Y" AS isUserInput
  FROM AggSourceHours;
FLUSH TABLE SourceHours;

    
--
--  Starts
--
-- SELECT "Making Aggregate Starts Table" AS MARKER_POINT;
CREATE TABLE AggStarts (
	hourDayID SMALLINT,
	monthID SMALLINT,
	yearID SMALLINT,
	ageID SMALLINT,
	stateID SMALLINT,
	sourceTypeID SMALLINT,
	starts FLOAT
	);
INSERT INTO AggStarts
  SELECT hourDayID, monthID, yearID, ageID, stateID, sourceTypeID, 
    sum(starts) AS starts
  FROM Starts INNER JOIN OldZone USING (zoneID) 
  GROUP BY hourDayID, monthID, yearID, ageID, stateID, sourceTypeID;
TRUNCATE Starts;
INSERT INTO Starts (hourDayID, monthID, yearID, ageID, zoneID, 
    sourceTypeID, starts, startsCV, isUserInput)
  SELECT hourDayID, monthID, yearID, ageID, 
    (stateID*10000) AS zoneID, 
    sourceTypeID, starts, NULL AS startsCV, "Y" AS isUserInput
  FROM AggStarts;
FLUSH TABLE Starts;

--
--  ExtendedIdleHours
--
-- SELECT "Making Aggregate ExtendedIdleHours Table" AS MARKER_POINT;
CREATE TABLE AggExtendedIdleHours (
	sourceTypeID SMALLINT,
	hourDayID SMALLINT,
	monthID SMALLINT,
	yearID SMALLINT,
	ageID SMALLINT,
	stateID SMALLINT,
	extendedIdleHours FLOAT);
INSERT INTO AggExtendedIdleHours	
  SELECT sourceTypeID, hourDayID, monthID, yearID, ageID, stateID, 
    sum(extendedIdleHours) AS extendedIdleHours
  FROM ExtendedIdleHours INNER JOIN OldZone USING(zoneID)
  GROUP BY sourceTypeID, hourDayID, monthID, yearID, ageID, stateID; 
TRUNCATE ExtendedIdleHours;
INSERT INTO ExtendedIdleHours (sourceTypeID, hourDayID, monthID, yearID, ageID, zoneID, 
     extendedIdleHours, extendedIdleHoursCV, isUserInput)
  SELECT sourceTypeID, hourDayID, monthID, yearID, ageID, 
    (stateID*10000) AS zoneID, 
    extendedIdleHours, NULL AS extendedIdleHoursCV, "Y" AS isUserInput
  FROM AggExtendedIdleHours;  
FLUSH TABLE ExtendedIdleHours;
  
-- 
-- AverageTankTemperature
--
-- SELECT "Making AverageTankTemperature Table" AS MARKER_POINT;
CREATE Table OldAverageTankTemperature
  SELECT tankTemperatureGroupID, stateID, monthID, att.zoneid, hourDayID,
  		opModeID, averageTankTemperature
  FROM AverageTankTemperature AS att INNER JOIN OldZone USING (zoneID);
TRUNCATE AverageTankTemperature;
INSERT INTO AverageTankTemperature (tankTemperatureGroupID, zoneID, monthID, hourDayID,
		opModeID, averageTankTemperature, averageTankTemperatureCV, isUserInput)
  SELECT tankTemperatureGroupID, stateID*10000 AS zoneID, monthID, hourDayID, opModeID,
  sum(averageTankTemperature*actFract) AS averageTankTemperature, 
  NULL AS averageTankTemperatureCV, 'Y' AS isUserInput
  FROM OldAverageTankTemperature INNER JOIN SurrogateActivity USING(zoneID)
  GROUP BY tankTemperatureGroupID, stateID, monthID, hourdayID, opModeID;
FLUSH TABLE AverageTankTemperature;

-- 
-- SoakActivityFraction
--
-- SELECT "Making SoakActivityFraction" AS MARKER_POINT;
CREATE Table OldSoakActivityFraction
  SELECT sourceTypeID, stateID, saf.zoneID, monthID, hourDayID, opModeID, soakActivityFraction
    FROM SoakActivityFraction AS saf INNER JOIN OldZone USING (zoneID);
TRUNCATE SoakActivityFraction;
INSERT INTO SoakActivityFraction (sourceTypeID, zoneID, monthID, hourDayID, opModeID, 
		soakActivityFraction, soakActivityFractionCV, isUserInput)
  SELECT sourceTypeID, stateID*10000 AS zoneID, monthID, hourDayID, opModeID,
  sum(soakActivityFraction*actFract) AS soakActivityFraction, NULL AS soakActivityFractionCV,
  'Y' AS isUserInput 
  FROM OldSoakActivityFraction INNER JOIN SurrogateActivity USING(zoneID)
  GROUP BY sourceTypeID, stateID, monthID, hourDayID, opModeID;
FLUSH TABLE SoakActivityFraction;

-- 
-- ColdSoakTankTemperature
--
-- SELECT "Making Aggregate ColdSoakTankTemperature Table" AS MARKER_POINT;
CREATE Table OldColdSoakTankTemperature
  SELECT monthID, stateID, cstt.zoneid, hourID, coldSoakTankTemperature
  FROM ColdSoakTankTemperature AS cstt INNER JOIN OldZone USING (zoneID);
TRUNCATE ColdSoakTankTemperature;
INSERT INTO ColdSoakTankTemperature (monthID, zoneID, hourID, coldSoakTankTemperature)
  SELECT monthID, stateID*10000 AS zoneID, hourID, 
  sum(coldSoakTankTemperature*actFract) AS coldSoakTankTemperature
  FROM OldColdSoakTankTemperature INNER JOIN SurrogateActivity USING(zoneID)
  GROUP BY monthID, hourID, stateID;
FLUSH TABLE ColdSoakTankTemperature;

-- 
-- ColdSoakInitialHourFraction
--
-- SELECT "Making ColdSoakInitialHourFraction" AS MARKER_POINT;
CREATE Table OldColdSoakInitialHourFraction
  SELECT sourceTypeID, monthID, stateID, old.zoneid, hourDayID, initialHourDayID, coldSoakInitialHourFraction
  FROM ColdSoakInitialHourFraction AS old INNER JOIN OldZone USING (zoneID);
TRUNCATE ColdSoakInitialHourFraction;
INSERT INTO ColdSoakInitialHourFraction (sourceTypeID, monthID, zoneID, hourDayID, initialHourDayID, 
	coldSoakInitialHourFraction, isUserInput)
  SELECT sourceTypeID, monthID, stateID*10000 AS zoneID, hourDayID, initialHourDayID, 
  sum(coldSoakInitialHourFraction*actFract) AS coldSoakInitialHourFraction,
  'Y' as isUserInput
  FROM OldColdSoakInitialHourFraction INNER JOIN SurrogateActivity USING(zoneID)
  GROUP BY sourceTypeID, monthID, stateID, hourDayID, initialHourDayID;
FLUSH TABLE ColdSoakInitialHourFraction;

-- 
-- AverageTankGasoline
--
-- SELECT "Making AverageTankGasoline" AS MARKER_POINT;
CREATE Table OldAverageTankGasoline
  SELECT stateID, old.zoneid, fuelTypeID, fuelYearID, monthGroupID, ETOHVolume, RVP
  FROM AverageTankGasoline AS old INNER JOIN OldZone USING (zoneID);
TRUNCATE AverageTankGasoline;
INSERT INTO AverageTankGasoline (zoneID, fuelTypeID, fuelYearID, monthGroupID, ETOHVolume, RVP, isUserInput)
  SELECT stateID*10000 AS zoneID, fuelTypeID, fuelYearID, monthGroupID, 
  sum(ETOHVolume*actFract) AS ETOHVolume,
  sum(RVP*actFract) AS RVP,
  'Y' as isUserInput
  FROM OldAverageTankGasoline INNER JOIN SurrogateActivity USING(zoneID)
  GROUP BY stateID, fuelTypeID, fuelYearID, monthGroupID;
FLUSH TABLE AverageTankGasoline;


--
-- Drop any New Tables Created 
--
-- SELECT "Dropping Temporary Tables";
-- DROP TABLE IF EXISTS SurrogateActivity;

DROP TABLE IF EXISTS OldCounty;
DROP TABLE IF EXISTS OldLink;
DROP TABLE IF EXISTS OldZone;
DROP TABLE IF EXISTS AggZone;
DROP TABLE IF EXISTS OldYear;
DROP TABLE IF EXISTS OldZoneMonthHour;
DROP TABLE IF EXISTS OldOpModeDistribution; 
DROP TABLE IF EXISTS AggZoneRoadType;
DROP TABLE IF EXISTS OldFuelSupply;
DROP TABLE IF EXISTS AggFuelSupply; 
DROP TABLE IF EXISTS OldIMCoverage; 
DROP TABLE IF EXISTS AggSHO;
DROP TABLE IF EXISTS AggSourceHours;
DROP TABLE IF EXISTS AggStarts;
DROP TABLE IF EXISTS AggExtendedIdleHours; 
DROP TABLE IF EXISTS OldAverageTankTemperature;
DROP TABLE IF EXISTS OldSoakActivityFraction;
DROP TABLE IF EXISTS OldFuelUsageFraction;
DROP TABLE IF EXISTS AggFuelUsageFraction;

-- FLUSH TABLES;
