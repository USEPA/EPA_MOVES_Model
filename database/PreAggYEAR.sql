/* ***********************************************************************************************************************
-- MySQL Script file to preaggregate the MOVESExecution database
--    from the MONTH level database to the YEAR level
-- An attempt is made to weight some aggregations by activity.
--
-- Version 2015-05-20
--
-- Written By Mitch Cumberworth, April, 2004
-- Change history:
--   Corrected By Mitch Cumberworth, June 24, 2004
--   Fuel Supply calculation fixed by Mitch Cumberworth, October 28, 2004
--   MonthGroupHour calculation fixed by Mitch Cumberworth, December 2, 2004
--   Adapted by Mitch Cumberworth, June, 2005 for Task 207
--   Adapted by Mitch Cumberworth, July, 2005 for Task 208
--   Adapted by Mitch Cumberworth, November, 2005 for Task 210
--   Modified by Cimulus, January, 2005 for Task 210
--   Modified by wfaler, Dec, 2007 for Task 804, changed "Entire Year" to "Whole Year" in order to fit within 10 characters
--   Modified by Gwo Shyu, March 26, 2008 to fix the errors of duplicate entry and table existing
--   Modified by Wes Faler, June 15, 2009 for Task 912 Fuel Adjustments
-- *********************************************************************************************************************** */

DROP TABLE IF EXISTS MonthWeighting;
DROP TABLE IF EXISTS MonthGroupWeighting;
DROP TABLE IF EXISTS AggDayVMTFraction;
DROP TABLE IF EXISTS AggMonthVMTFraction;
DROP TABLE IF EXISTS AggSourceTypeDayVMT;
DROP TABLE IF EXISTS AggHPMSVTypeDay;
DROP TABLE IF EXISTS OldSHO;
DROP TABLE IF EXISTS OldSourceHours;
DROP TABLE IF EXISTS OldStarts;
DROP TABLE IF EXISTS OldExtendedIdleHours;
DROP TABLE IF EXISTS AggZoneMonthHour;
DROP TABLE IF EXISTS AggMonthGroupHour;
DROP TABLE IF EXISTS AggFuelSupply;
DROP TABLE IF EXISTS OldAverageTankTemperature;
DROP TABLE IF EXISTS OldSoakActivityFraction;
drop table if exists AggATBaseEmissions;
drop table if exists AggATRatio;

--
-- Create MonthWeightings to be used to weight monthly activity 
-- 
-- Note:  these weightings will not sum to unity if all months are not included
-- SELECT "Making MonthWeighting" AS MARKER_POINT;
-- Create table explicitly to control column types and avoid significance problems
CREATE TABLE MonthWeighting (
	monthID SMALLINT,
	actFract FLOAT);
INSERT INTO MonthWeighting
  SELECT monthID, monthVMTFraction AS actFract
    FROM MonthVMTFraction 
  WHERE sourceTypeID=21;
CREATE UNIQUE INDEX index1 ON MonthWeighting (monthID);

-- SELECT "Making MonthGroupWeighting" AS MARKER_POINT;
-- Create table explicitly to control column types and avoid significance problems
CREATE TABLE MonthGroupWeighting (
	monthGroupID SMALLINT,
	actFract FLOAT);
INSERT INTO MonthGroupWeighting
  SELECT monthGroupID, SUM(monthVMTFraction) AS actFract
    FROM MonthVMTFraction INNER JOIN MonthOfAnyYear USING (monthID)
  WHERE sourceTypeID=21
  GROUP BY monthGroupID;
CREATE UNIQUE INDEX index1 ON MonthGroupWeighting (monthGroupID);

--
-- MonthOfAnyYear Table
--
-- SELECT "Making MonthOfAnyYear" AS MARKER_POINT;
TRUNCATE MonthOfAnyYear;
INSERT INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID)
  VALUES (0, "Whole Year", 365, 0);
FLUSH TABLE MonthOfAnyYear;
  
--
-- MonthGroupOfAnyYear Table
--
-- SELECT "Making MonthGroupOfAnyYear" AS MARKER_POINT;
TRUNCATE MonthGroupOfAnyYear;
INSERT INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName)
  VALUES (0, "Whole Year");  
FLUSH TABLE MonthGroupOfAnyYear;
  
--
-- DayVMTFraction Table
--
-- SELECT "Making DayVMTFraction" AS MARKER_POINT;
CREATE TABLE AggDayVMTFraction 
  SELECT sourceTypeID, roadTypeID
    FROM DayVMTFraction 
    GROUP BY sourceTypeID, roadTypeID;
TRUNCATE DayVMTFraction;
REPLACE INTO DayVMTFraction (sourceTypeID, monthID, roadTypeID, dayID, dayVMTFraction)
  SELECT sourceTypeID, 0 AS monthID, roadTypeID, 0 AS dayID, 1.0 AS dayVMTFraction
  FROM AggDayVMTFraction;
FLUSH TABLE DayVMTFraction;
  
--
-- MonthVMTFraction Table
--
-- SELECT "Making MonthVMTFraction" AS MARKER_POINT;
CREATE TABLE AggMonthVMTFraction 
  SELECT sourceTypeID 
    FROM MonthVMTFraction 
    GROUP BY sourceTypeID;
TRUNCATE MonthVMTFraction;
REPLACE INTO MonthVMTFraction (sourceTypeID, monthID, monthVMTFraction)
  SELECT sourceTypeID, 0 AS monthID, 1.0 AS monthVMTFraction
  FROM AggMonthVMTFraction;  
FLUSH TABLE MonthVMTFraction;

--
-- SourceTypeDayVMT Table
--
-- SELECT "Making SourceTypeDayVMT" AS MARKER_POINT;
CREATE TABLE AggSourceTypeDayVMT 
  SELECT yearID, 0 as monthID, 0 as dayID, sourceTypeID, sum(VMT*actFract) as VMT
    FROM SourceTypeDayVMT
    INNER JOIN MonthWeighting USING (monthID)
    GROUP BY yearID, sourceTypeID;
TRUNCATE SourceTypeDayVMT;
REPLACE INTO SourceTypeDayVMT (yearID, monthID, dayID, sourceTypeID, VMT)
  SELECT yearID, monthID, dayID, sourceTypeID, VMT
  FROM AggSourceTypeDayVMT;
FLUSH TABLE SourceTypeDayVMT;

--
-- HPMSVTypeDay Table
--
-- SELECT "Making HPMSVTypeDay" AS MARKER_POINT;
CREATE TABLE AggHPMSVTypeDay 
  SELECT yearID, 0 as monthID, 0 as dayID, hpmsVTypeID, sum(VMT*actFract) as VMT
    FROM HPMSVTypeDay
    INNER JOIN MonthWeighting USING (monthID)
    GROUP BY yearID, hpmsVTypeID;
TRUNCATE HPMSVTypeDay;
REPLACE INTO HPMSVTypeDay (yearID, monthID, dayID, hpmsVTypeID, VMT)
  SELECT yearID, monthID, dayID, hpmsVTypeID, VMT
  FROM AggHPMSVTypeDay;
FLUSH TABLE HPMSVTypeDay;

--
--  SHO    
--
-- SELECT "Making SHO" AS MARKER_POINT;
CREATE TABLE OldSHO
  SELECT monthID, yearID, ageID, linkID, sourceTypeID, SHO, distance 
    FROM SHO ;
CREATE INDEX index1 ON OldSHO (yearID, ageID, linkID, sourceTypeID);
TRUNCATE SHO;
REPLACE INTO SHO (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, SHO, SHOCV, distance, isUserInput)
  SELECT 0 AS hourDayID, 0 AS monthID, yearID, ageID, linkID, sourceTypeID, 
    sum(SHO) AS SHO, NULL AS SHOCV, sum(distance) AS distance, "Y" AS isUserInput
  FROM OldSHO 
  GROUP BY yearID, ageID, linkID, sourceTypeID;
FLUSH TABLE SHO;

--
--  SourceHours   
--
-- SELECT "Making SourceHours" AS MARKER_POINT;
CREATE TABLE OldSourceHours
  SELECT monthID, yearID, ageID, linkID, sourceTypeID, sourceHours
    FROM SourceHours ;
CREATE INDEX index1 ON OldSourceHours (yearID, ageID, linkID, sourceTypeID);
TRUNCATE SourceHours;
REPLACE INTO SourceHours (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, sourceHours, sourceHoursCV, isUserInput)
  SELECT 0 AS hourDayID, 0 AS monthID, yearID, ageID, linkID, sourceTypeID, 
    sum(sourceHours) AS sourceHours, NULL AS sourceHoursCV, "Y" AS isUserInput
  FROM OldSourceHours 
  GROUP BY yearID, ageID, linkID, sourceTypeID;
FLUSH TABLE SourceHours;

--
--  Starts
--
-- SELECT "Making Starts" AS MARKER_POINT;
CREATE TABLE OldStarts
  SELECT monthID, yearID, ageID, zoneID, sourceTypeID, starts
  FROM Starts ;
CREATE INDEX index1 ON OldStarts (yearID, ageID, zoneID, sourceTypeID);
TRUNCATE Starts;
REPLACE INTO Starts (hourDayID, monthID, yearID, ageID, zoneID, 
    sourceTypeID, starts, startsCV, isUserInput)
  SELECT 0 AS hourDayID, 0 AS monthID, yearID, ageID, zoneID, sourceTypeID, 
    sum(starts) AS starts, NULL AS startsCV, "Y" AS isUserInput
  FROM OldStarts
  GROUP BY yearID, ageID, zoneID, sourceTypeID;
FLUSH TABLE Starts;
  
--
--  ExtendedIdleHours
--
-- SELECT "Making ExtendedIdleHours" AS MARKER_POINT;
CREATE TABLE OldExtendedIdleHours
  SELECT sourceTypeID, monthID, yearID, ageID, zoneID, extendedIdleHours
  FROM ExtendedIdleHours ;
CREATE INDEX index1 ON OldExtendedIdleHours (sourceTypeID, yearID, ageID, zoneID);
TRUNCATE ExtendedIdleHours;
REPLACE INTO ExtendedIdleHours (sourceTypeID, hourDayID, monthID, yearID, ageID, zoneID, 
    extendedIdleHours, extendedIdleHoursCV, isUserInput)
  SELECT sourceTypeID, 0 AS hourDayID, 0 AS monthID, yearID, ageID, zoneID,  
    sum(extendedIdleHours) AS extendedIdleHours, NULL AS extendedIdleHoursCV, "Y" AS isUserInput
  FROM OldExtendedIdleHours
  GROUP BY sourceTypeID, yearID, ageID, zoneID;

FLUSH TABLE ExtendedIdleHours;
  
-- 
-- ZoneMonthHour
--
-- SELECT "Making ZoneMonthHour" AS MARKER_POINT;
-- Explicit Creation of Intermediate File Found necessary to avoid significance problems
CREATE TABLE AggZoneMonthHour (
	zoneID INTEGER,
	temperature FLOAT,
	relHumidity FLOAT);
INSERT INTO AggZoneMonthHour (zoneID,temperature,relHumidity)
  SELECT zoneID, 
    (sum(temperature*actFract)/sum(actFract)) AS temperature,
    (sum(relHumidity*actFract)/sum(actFract)) AS relHumidity
  FROM ZoneMonthHour INNER JOIN MonthWeighting USING (monthID)
  GROUP BY zoneID;
TRUNCATE ZoneMonthHour;
REPLACE INTO ZoneMonthHour (monthID, zoneID, hourID, temperature, temperatureCV,
    relHumidity, relativeHumidityCV, heatIndex, specificHumidity)
  SELECT 0 AS monthID, zoneID, 0 AS hourID, temperature,
    NULL AS temperatureCV, relHumidity, NULL AS relativeHumidityCV,
    0.0 AS heatIndex, 0.0 AS specificHumidity 
  FROM AggZoneMonthHour;

FLUSH TABLE ZoneMonthHour;

-- 
-- MonthGroupHour
--
-- SELECT "Making MonthGroupHour" AS MARKER_POINT;
-- Explicit Creation of Intermediate File Found necessary to avoid significance problems
CREATE TABLE AggMonthGroupHour (
	ACActivityTermA FLOAT,
	ACActivityTermB FLOAT,
	ACActivityTermC FLOAT);
INSERT INTO AggMonthGroupHour (ACActivityTermA,ACActivityTermB,ACActivityTermC)
  SELECT 
    (sum(ACActivityTermA*actFract)/sum(actFract)) AS ACActivityTermA,
    (sum(ACActivityTermB*actFract)/sum(actFract)) AS ACActivityTermB,
    (sum(ACActivityTermC*actFract)/sum(actFract)) AS ACActivityTermC
  FROM MonthGroupHour AS mgh INNER JOIN MonthGroupWeighting USING (monthGroupID);
TRUNCATE MonthGroupHour;
REPLACE INTO MonthGroupHour (monthGroupID, hourID,
	ACActivityTermA, ACActivityTermACV, 
	ACActivityTermB, ACActivityTermBCV, 
	ACActivityTermC, ACActivityTermCCV 
	)
  SELECT 0 AS monthGroupID, 0 AS hourID,
    ACActivityTermA, NULL AS ACActivityTermACV,
    ACActivityTermB, NULL AS ACActivityTermBCV,
    ACActivityTermC, NULL AS ACActivityTermCCV
  FROM AggMonthGroupHour; 

FLUSH TABLE MonthGroupHour;

create table AggATRatio (
	fuelTypeID int not null,
	fuelFormulationID int not null,
	polProcessID int not null,
	minModelYearID int not null,
	maxModelYearID int not null,
	ageID int not null,
	atRatio double null
);
insert into AggATRatio (fuelTypeID, fuelFormulationID, polProcessID, minModelYearID, maxModelYearID, ageID, 
		atRatio)
select fuelTypeID, fuelFormulationID, polProcessID, minModelYearID, maxModelYearID, ageID, 
		(sum(atRatio*actFract)/sum(actFract)) as atRatio
from atRatio
inner join MonthGroupWeighting using (monthGroupID)
group by fuelTypeID, fuelFormulationID, polProcessID, minModelYearID, maxModelYearID, ageID;
truncate atRatio;
insert into atRatio (fuelTypeID, fuelFormulationID, polProcessID, minModelYearID, maxModelYearID,
		ageID, monthGroupID, atRatio)
select fuelTypeID, fuelFormulationID, polProcessID, minModelYearID, maxModelYearID,
	ageID, 0 as monthGroupID, atRatio
from AggATRatio;

CREATE TABLE AggATBaseEmissions 
(
	polProcessID			int		NOT NULL	default '0',
	atBaseEmissions			float	NOT NULL	default '0',
	primary key (polProcessID)
);
insert into AggATBaseEmissions (polProcessID, atBaseEmissions)
select polProcessID, (sum(atBaseEmissions*actFract)/sum(actFract)) as atBaseEmissions
from ATBaseEmissions
inner join MonthGroupWeighting using (monthGroupID)
group by polProcessID;
truncate ATBaseEmissions;
insert into ATBaseEmissions (polProcessID, monthGroupID, atBaseEmissions, dataSourceID)
select polProcessID, 0 as monthGroupID, atBaseEmissions, 0 as dataSourceID
from AggATBaseEmissions;

--
-- Fuel Supply
--
-- Note: algorithm is specific to particular default values used.
-- SELECT "Making Aggregate FuelSupply Table" AS MARKER_POINT;
-- Creating table explicitly to control column type and avoid significance problem
CREATE TABLE AggFuelSupply (
	fuelRegionID INTEGER,
	fuelYearID SMALLINT,
	fuelFormulationID SMALLINT,
	haveFract FLOAT,
	fractDontHave FLOAT);
INSERT INTO AggFuelSupply
  SELECT fuelRegionID, fuelYearID, fuelFormulationID, 
    (sum(marketShare*actFract)/sum(actFract)) as haveFract,
    (1.0 - SUM(actFract)) AS fractDontHave
  FROM FuelSupply INNER JOIN MonthGroupWeighting USING(monthGroupID)
  GROUP BY fuelRegionID, fuelYearID, fuelFormulationID;
TRUNCATE FuelSupply;  
REPLACE INTO FuelSupply (fuelRegionID, fuelYearID, monthGroupID, fuelFormulationID, 
   marketShare, marketShareCV)
  SELECT  fuelRegionID, fuelYearID, 0 AS monthGroupID, fuelFormulationID, 
    ((1.0-fractDontHave)*haveFract) AS marketShare,
    NULL AS marketShareCV
  FROM AggFuelSupply;

FLUSH TABLE FuelSupply;
   
--
-- AverageTankTemperature
--
-- SELECT "Making AverageTankTemperature" AS MARKER_POINT;
CREATE TABLE OldAverageTankTemperature
  SELECT tankTemperatureGroupID, zoneID, monthID, hourDayID, opModeID, averageTankTemperature
  FROM AverageTankTemperature;
TRUNCATE AverageTankTemperature;
REPLACE INTO AverageTankTemperature (tankTemperatureGroupID, zoneID, monthID,
    hourDayID, opModeID, averageTankTemperature, averageTankTemperatureCV, isUserInput) 
  SELECT tankTemperatureGroupID, zoneID, 0 AS monthID, 0 AS hourDayID, opModeID,
    sum(averageTankTemperature*actFract) AS averageTankTemperature, 
    NULL AS averageTankTemperatureCV, 'Y' AS isUserInput
  FROM OldAverageTankTemperature AS oatt INNER JOIN monthWeighting USING(monthID)
  GROUP BY tankTemperatureGroupID, zoneID, opModeID ;

FLUSH TABLE AverageTankTemperature;

--
-- SoakActivityFraction
--
-- SELECT "Making SoakActivityFraction" AS MARKER_POINT;
CREATE TABLE OldSoakActivityFraction
  SELECT sourceTypeID, zoneID, monthID, hourDayID, opModeID, soakActivityFraction
  FROM SoakActivityFraction;
TRUNCATE SoakActivityFraction;
REPLACE INTO SoakActivityFraction (sourceTypeID, zoneID, monthID,
    hourDayID, opModeID, soakActivityFraction, soakActivityFractionCV, isUserInput) 
  SELECT sourceTypeID, zoneID, 0 AS monthID, 0 AS hourDayID, opModeID,
    sum(soakActivityFraction*actFract) AS soakActivityFraction, 
    NULL AS soakActivityFractionCV, 'Y' AS isUserInput
  FROM OldSoakActivityFraction INNER JOIN monthWeighting USING(monthID)
  GROUP BY sourceTypeID, zoneID, opModeID ;

FLUSH TABLE SoakActivityFraction;
  
--
-- Drop any New Tables Created 
--
-- FLUSH TABLES;

DROP TABLE IF EXISTS MonthWeighting;
DROP TABLE IF EXISTS MonthGroupWeighting;
DROP TABLE IF EXISTS AggDayVMTFraction;
DROP TABLE IF EXISTS AggMonthVMTFraction;
DROP TABLE IF EXISTS AggSourceTypeDayVMT;
DROP TABLE IF EXISTS AggHPMSVTypeDay;
DROP TABLE IF EXISTS OldSHO;
DROP TABLE IF EXISTS OldSourceHours;
DROP TABLE IF EXISTS OldStarts;
DROP TABLE IF EXISTS OldExtendedIdleHours;
DROP TABLE IF EXISTS AggZoneMonthHour;
DROP TABLE IF EXISTS AggMonthGroupHour;
DROP TABLE IF EXISTS AggFuelSupply;
DROP TABLE IF EXISTS OldAverageTankTemperature;
DROP TABLE IF EXISTS OldSoakActivityFraction;

-- FLUSH TABLES;
  
