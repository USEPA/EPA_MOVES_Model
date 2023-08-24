/* ***********************************************************************************************************************
-- MySQL Script file to aggregate the separate hours of the day
--    out of the MOVESExecution database
--    after input databases have been merged by the InputDataManager
--    and before any MasterLoopable objects are executed.
-- An attempt is made to weight some aggregations by activity.
--
-- Author Wesley Faler
-- Author Gwo Shyu
-- Version 2014-04-24
-- Change history:
--   Modified by Gwo Shyu, March 26, 2008 to fix the errors of duplicate entry and table existing
   *********************************************************************************************************************** */
DROP TABLE IF EXISTS HourWeighting1;
DROP TABLE IF EXISTS HourWeighting2;
DROP TABLE IF EXISTS HourWeighting3;
DROP TABLE IF EXISTS HourWeighting4;
DROP TABLE IF EXISTS OldHourDay;
DROP TABLE IF EXISTS OldAvgSpeedDistribution;
DROP TABLE IF EXISTS OldOpModeDistribution;
DROP TABLE IF EXISTS OldOpModeDistribution2;
DROP TABLE IF EXISTS OldSourceTypeHour;
DROP TABLE IF EXISTS OldSHO;
DROP TABLE IF EXISTS OldSourceHours;
DROP TABLE IF EXISTS OldStarts;
DROP TABLE IF EXISTS OldHotellingHourFraction;
DROP TABLE IF EXISTS AggZoneMonthHour;
DROP TABLE IF EXISTS AggMonthGroupHour;
DROP TABLE IF EXISTS OldSampleVehicleTrip;
DROP TABLE IF EXISTS OldStartsPerVehicle;
DROP TABLE IF EXISTS OldStartsOpModeDistribution;
DROP TABLE IF EXISTS OldStartsHourFraction;
DROP TABLE IF EXISTS OldSoakActivityFraction;
DROP TABLE IF EXISTS OldAverageTankTemperature;

--
-- Create HourWeighting1 to be used to weight hourly activity 
--    by sourceTypeID, dayID, and RoadtypeID.
--    HourVMTFraction itself could be used, except that it will be modified later
-- 
-- SELECT "Making HourWeighting1" AS MARKER_POINT;
CREATE Table HourWeighting1
  SELECT sourceTypeID, roadTypeID, dayID, hourID, hourVMTFraction as actFract 
    FROM HourVMTFraction;
CREATE UNIQUE INDEX index1 ON HourWeighting1 (sourceTypeID, roadTypeID, dayID, hourID);

--
-- Create HourWeighting2 to be used to weight hourly activity by
--    sourceTypeID, and dayID
--
-- SELECT "Making HourWeighting2" AS MARKER_POINT;
-- Explicit Creation of Intermediate File Found necessary to avoid significance problems
CREATE TABLE  HourWeighting2 (
	sourceTypeID SMALLINT, 
	dayID SMALLINT,
	hourID SMALLINT,
	actFract FLOAT);
INSERT INTO HourWeighting2 (sourceTypeID, dayID, hourID, actFract)
  SELECT hvmtf.sourceTypeID, dayID, hourID, 
    ((sum(hourVMTFraction*roadTypeVMTFraction))/sum(roadTypeVMTFraction)) as actFract 
    FROM HourVMTFraction AS hvmtf INNER JOIN RoadTypeDistribution USING (sourceTypeID, roadTypeID)
    GROUP BY hvmtf.sourceTypeID, dayID, hourID;
CREATE UNIQUE INDEX index1 ON HourWeighting2 (sourceTypeID, dayID, hourID);

--
-- Create HourWeighting3 to be used to weight hourly activity 
--    when no keys (besides hourID) are shared with HourVMTFraction
--
-- SELECT "Making HourWeighting3" AS MARKER_POINT;
-- Explicit Creation of Intermediate File Found necessary to avoid significance problems
CREATE Table  HourWeighting3 (
	hourID SMALLINT,
	actFract FLOAT);
INSERT INTO HourWeighting3
  SELECT hourID, avg(actFract) as actFract 
    FROM HourWeighting2
    WHERE sourceTypeID=21 
    GROUP BY hourID;
CREATE UNIQUE INDEX index1 ON HourWeighting3 (hourID);

-- Create HourWeighting4 to be used to weight hourly activity
--  when only dayID (besides hourID) is shared with HourVMTFraction
--
SELECT "Making HourWeighting4" AS MARKER_POINT;
CREATE Table HourWeighting4 (
	dayID SMALLINT,
	hourID SMALLINT,
	actFract FLOAT);
INSERT INTO HourWeighting4
  SELECT dayID, hourID, actFract 
  FROM HourWeighting2 WHERE sourceTypeID=21;
CREATE UNIQUE INDEX index1 ON HourWeighting4 (dayID, hourID);

--
-- HourOfAnyDay Table
--
-- SELECT "Making HourOfAnyDay" AS MARKER_POINT;
TRUNCATE HourOfAnyDay;
INSERT INTO HourOfAnyDay (hourID, hourName)
  VALUES (0, "Entire Day");

FLUSH TABLE HourOfAnyDay;
  
--
-- HourDay Table  (save old version as it is needed later)
--
-- SELECT "Making HourDay" AS MARKER_POINT;
CREATE Table OldHourDay SELECT * from HourDay;
CREATE UNIQUE INDEX index1 ON OldHourDay(hourDayID);
TRUNCATE HourDay;
INSERT INTO HourDay (hourDayID, dayID, hourID)
  SELECT dayID AS hourDayID, dayID, 0 AS hourID
  FROM DayOfAnyWeek;
CREATE UNIQUE INDEX index1 ON HourDay (hourDayID);
FLUSH TABLE HourDay;
--
-- HourVMTFraction Table
--
-- SELECT "Making HourVMTFraction" AS MARKER_POINT;
TRUNCATE HourVMTFraction;
REPLACE INTO HourVMTFraction (sourceTypeID, roadTypeID, dayID, hourID, hourVMTFraction)
  SELECT sourceTypeID, roadTypeID, dayID, 0 AS hourID, 1.0 AS hourVMTFraction
  FROM HourWeighting1
  GROUP BY sourceTypeID, roadTypeID, dayID;
CREATE UNIQUE INDEX index1 ON HourVMTFraction (sourceTypeID, roadTypeID, dayID);  
FLUSH TABLE HourVMTFraction;

--
-- AvgSpeedDistribution Table
--
-- SELECT "Making AvgSpeedDistribution" AS MARKER_POINT;
CREATE TABLE OldAvgSpeedDistribution
  SELECT sourceTypeID, roadTypeID, dayID, hourID, avgSpeedBinID, avgSpeedFraction
  FROM AvgSpeedDistribution INNER JOIN OldHourDay USING (hourDayID);
TRUNCATE AvgSpeedDistribution;
INSERT INTO AvgSpeedDistribution 
	(sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID, avgSpeedFraction)
  SELECT asd.sourceTypeID, asd.roadTypeID, asd.dayID AS hourDayID, asd.avgSpeedBinID, 
    ((sum(asd.avgSpeedFraction*hw.actFract))/sum(hw.actFract)) AS avgSpeedFraction
  FROM OldAvgSpeedDistribution AS asd INNER JOIN HourWeighting1 AS hw 
      USING (sourceTypeID, roadTypeID, dayID, hourID)
    GROUP BY sourceTypeID, roadTypeID, asd.dayID, avgSpeedBinID;
CREATE UNIQUE INDEX index1 ON AvgSpeedDistribution 
    (sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID);
FLUSH TABLE AvgSpeedDistribution;

--
-- OpModeDistribution
--
-- SELECT "Making OpModeDistribution" AS MARKER_POINT;
CREATE Table OldOpModeDistribution 
  SELECT omd.*, Link.roadTypeID
  FROM OpModeDistribution AS omd INNER JOIN Link USING (linkID);
CREATE Table OldOpModeDistribution2
  SELECT omd.*, OldHourDay.dayID,OldHourDay.hourID
  FROM OldOpModeDistribution AS omd INNER JOIN OldHourDay USING (hourDayID);
CREATE UNIQUE INDEX index1 ON OldOpModeDistribution2 (sourceTypeID, roadTypeID, dayID, hourID);
TRUNCATE OpModeDistribution;
INSERT INTO OpModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, 
    opModeFraction, opModeFractionCV, isUserInput)
  SELECT omd.sourceTypeID, omd.dayID AS hourDayID, omd.linkID, omd.polProcessID, omd.opModeID,
    (SUM(omd.opModeFraction * hw.actFract)/SUM(hw.actFract)) AS opModeFraction, 
    NULL AS opModeFractionCV, "Y" AS isUserInput
  FROM OldOpModeDistribution2 AS omd INNER JOIN HourWeighting1 AS hw
    USING (sourceTypeID, roadTypeID, dayID, hourID )
  GROUP BY omd.sourceTypeID, omd.dayID, omd.linkID, omd.polProcessID, omd.opModeID; 
FLUSH TABLE OpModeDistribution;
  
--
-- SourceTypeHour Table
--
-- Note:  idleSHOFactors are to be summed, not averaged. 
-- SELECT "Making SourceTypeHour" AS MARKER_POINT;
CREATE TABLE OldSourceTypeHour
  SELECT sourceTypeID, dayID, hourID, idleSHOFactor, hotellingDist
  FROM SourceTypeHour INNER JOIN OldHourDay USING (hourDayID);
TRUNCATE SourceTypeHour;
INSERT INTO SourceTypeHour 
	(sourceTypeID, hourDayID, idleSHOFactor, hotellingDist)
  SELECT sth.sourceTypeID, sth.dayID AS hourDayID, 
    sum(sth.idleSHOFactor) AS idleSHOFactor, sum(sth.hotellingDist) as hotellingDist
  FROM OldSourceTypeHour AS sth 
  GROUP BY sourceTypeID, sth.dayID;
CREATE UNIQUE INDEX index1 ON SourceTypeHour (sourceTypeID, hourDayID);  
FLUSH TABLE SourceTypeHour;

--
--  SHO    
--
-- SELECT "Making SHO" AS MARKER_POINT;
CREATE TABLE OldSHO
  SELECT SHO.*, dayID, hourID
  FROM SHO INNER JOIN OldHourDay USING(hourDayID);
CREATE INDEX index10 ON OldSHO (dayID, monthID, yearID, ageID, linkID, sourceTypeID);
TRUNCATE SHO;
INSERT INTO SHO (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, SHO, SHOCV, distance, isUserInput)
  SELECT dayID AS hourDayID, monthID, yearID, ageID,linkID, sourceTypeID, 
    sum(SHO) AS SHO, NULL AS SHOCV, sum(distance) AS distance, "Y" AS isUserInput
  FROM OldSHO 
  GROUP BY dayID, monthID, yearID, ageID, linkID, sourceTypeID;
FLUSH TABLE SHO;
  
--
--  SourceHours    
--
-- SELECT "Making SourceHours" AS MARKER_POINT;
CREATE TABLE OldSourceHours
  SELECT SourceHours.*, dayID, hourID
  FROM SourceHours INNER JOIN OldHourDay USING(hourDayID);
CREATE INDEX index10 ON OldSourceHours (dayID, monthID, yearID, ageID, linkID, sourceTypeID);
TRUNCATE SourceHours;
INSERT INTO SourceHours (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, sourceHours, sourceHoursCV, isUserInput)
  SELECT dayID AS hourDayID, monthID, yearID, ageID,linkID, sourceTypeID, 
    sum(sourceHours) AS sourceHours, NULL AS sourceHoursCV,"Y" AS isUserInput
  FROM OldSourceHours 
  GROUP BY dayID, monthID, yearID, ageID, linkID, sourceTypeID;
FLUSH TABLE SourceHours;
  
--
--  Starts
--
-- SELECT "Making Starts" AS MARKER_POINT;
CREATE TABLE  OldStarts
  SELECT Starts.*, dayID, hourID
  FROM Starts INNER JOIN OldHourDay USING(hourDayID);
CREATE INDEX index11 ON OldStarts (dayID, monthID, yearID, ageID, zoneID, sourceTypeID);
TRUNCATE Starts;
INSERT INTO Starts (hourDayID, monthID, yearID, ageID, zoneID, 
    sourceTypeID, starts, startsCV, isUserInput)
  SELECT dayID AS hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID, 
    sum(starts) AS starts, NULL AS startsCV, "Y" AS isUserInput
  FROM OldStarts
  GROUP BY dayID, monthID, yearID, ageID, zoneID, sourceTypeID;
FLUSH TABLE Starts;
 

-- HotellingHourFraction
-- 
-- SELECT "Making StartsHourFraction" AS MARKER_POINT;
CREATE TABLE OldHotellingHourFraction
  SELECT * FROM HotellingHourFraction;
TRUNCATE HotellingHourFraction;
INSERT INTO HotellingHourFraction (zoneID, dayID, hourID, hourFraction)
  SELECT zoneID, dayID, 0 as hourID, sum(hourFraction) as hourFraction
  FROM OldHotellingHourFraction
  GROUP BY zoneID, dayID;
FLUSH TABLE HotellingHourFraction;
  
-- 
-- ZoneMonthHour
--
-- SELECT "Making ZoneMonthHour" AS MARKER_POINT;
-- Explicit Creation of Intermediate File Found necessary to avoid significance problems
CREATE TABLE  AggZoneMonthHour (
	monthID SMALLINT,
	zoneID INTEGER,
	temperature DOUBLE,
	relHumidity DOUBLE);
INSERT INTO AggZoneMonthHour (monthID,zoneID,temperature,relHumidity)
  SELECT monthID, zoneID, 
    (sum(temperature*actFract)/sum(actFract)) AS temperature,
    (sum(relHumidity*actFract)/sum(actFract)) AS relHumidity
  FROM ZoneMonthHour INNER JOIN HourWeighting3 USING (hourID)
  GROUP BY monthID, zoneID;
TRUNCATE ZoneMonthHour;
REPLACE INTO ZoneMonthHour (monthID, zoneID, hourID, temperature,
    relHumidity, molWaterFraction, heatIndex, specificHumidity)
  SELECT monthID, zoneID, 0 AS hourID, temperature,
    relHumidity, 0.0 AS molWaterFraction,
    0.0 as heatIndex, 0.0 as specificHumidity 
  FROM AggZoneMonthHour;
FLUSH TABLE ZoneMonthHour;
  
-- 
-- MonthGroupHour
--
-- SELECT "Making MonthGroupHour" AS MARKER_POINT;
-- Explicit Creation of Intermediate File Found necessary to avoid significance problems
CREATE TABLE AggMonthGroupHour (
	monthGroupID SMALLINT,
	ACActivityTermA FLOAT,
	ACActivityTermB FLOAT,
	ACActivityTermC FLOAT);
INSERT INTO AggMonthGroupHour (monthGroupID,ACActivityTermA,ACActivityTermB,ACActivityTermC)
  SELECT monthGroupID, 
    (sum(ACActivityTermA*actFract)/sum(actFract)) AS ACActivityTermA,
    (sum(ACActivityTermB*actFract)/sum(actFract)) AS ACActivityTermB,
    (sum(ACActivityTermC*actFract)/sum(actFract)) AS ACActivityTermC
  FROM MonthGroupHour INNER JOIN HourWeighting3 USING (hourID)
  GROUP BY monthGroupID;
TRUNCATE MonthGroupHour;
REPLACE INTO MonthGroupHour (monthGroupID, hourID,
	ACActivityTermA, ACActivityTermACV, 
	ACActivityTermB, ACActivityTermBCV, 
	ACActivityTermC, ACActivityTermCCV 
	)
  SELECT monthGroupID, 0 AS hourID,
    ACActivityTermA, NULL AS ACActivityTermACV,
    ACActivityTermB, NULL AS ACActivityTermBCV,
    ACActivityTermC, NULL AS ACActivityTermCCV
  FROM AggMonthGroupHour;  
FLUSH TABLE MonthGroupHour;
  
--
-- SampleVehicleTrip
-- 
-- SELECT "Making SampleVehicleTrip" AS MARKER_POINT;
CREATE TABLE OldSampleVehicleTrip 
  SELECT *
  FROM SampleVehicleTrip;
-- ***************************
--  SELECT SampleVehicleTrip.*, dayID 
--  FROM SampleVehicleTrip INNER JOIN OldHourDay USING (hourDayID);
-- ***************************

TRUNCATE SampleVehicleTrip;
INSERT INTO SampleVehicleTrip (vehID, tripID, dayID, hourID, priorTripID, 
	keyOnTime, keyOffTime) 
  SELECT vehID, tripID, dayID, 0 as hourID, priorTripID, keyOnTime, keyOffTime
  FROM OldSampleVehicleTrip;
-- ***************************
--  SELECT vehID, tripID, dayID AS hourDayID, priorTripID, keyOnTime, keyOffTime
--  FROM OldSampleVehicleTrip;
-- ***************************
FLUSH TABLE SampleVehicleTrip;

--
-- StartsPerVehicle
-- 
-- SELECT "Making StartsPerVehicle" AS MARKER_POINT;
CREATE TABLE OldStartsPerVehicle 
  SELECT StartsPerVehicle.*, dayID 
  FROM StartsPerVehicle INNER JOIN OldHourDay USING (hourDayID);
TRUNCATE StartsPerVehicle;
INSERT INTO StartsPerVehicle (sourceTypeID, hourDayID, 
	startsPerVehicle, startsPerVehicleCV) 
  SELECT sourceTypeID, dayID AS hourDayID, sum(startsPerVehicle) as startsPerVehicle,
    NULL AS startsPerVehicleCV
  FROM OldStartsPerVehicle GROUP BY sourceTypeID, hourDayID;
FLUSH TABLE StartsPerVehicle;

-- This MUST happen before changes are made to StartsHourFraction (Jarrod/Evan/Michael Integration Sprint 27 March 2019)
-- StartsOpModeDistribution
-- 
-- SELECT "Making StartsOpModeDistribution" AS MARKER_POINT;
CREATE TABLE OldStartsOpModeDistribution
  SELECT * FROM StartsOpModeDistribution;
TRUNCATE StartsOpModeDistribution;
INSERT INTO StartsOpModeDistribution (dayID, hourID, sourceTypeID, ageID,
  opModeID, opModeFraction, isUserInput)
  SELECT dayID, 0 as hourID, sourceTypeID, ageID, opModeID,
    sum(opModeFraction * allocationFraction) as opModeFraction, 'Y' as isUserInput
  FROM OldStartsOpModeDistribution INNER JOIN startshourfraction USING (dayID, hourID, sourceTypeID)
  GROUP BY dayID, sourceTypeID, ageID, opModeID;
FLUSH TABLE StartsOpModeDistribution;

-- StartsHourFraction
-- 
-- SELECT "Making StartsHourFraction" AS MARKER_POINT;
CREATE TABLE OldStartsHourFraction
  SELECT * FROM StartsHourFraction;
TRUNCATE StartsHourFraction;
INSERT INTO StartsHourFraction (dayID, hourID, sourceTypeID, allocationFraction)
  SELECT dayID, 0 as hourID, sourceTypeID, sum(allocationFraction) as allocationFraction
  FROM OldStartsHourFraction
  GROUP BY dayID, sourceTypeID;
FLUSH TABLE StartsHourFraction;
     
--
-- AverageTankTemperature
--
-- SELECT "Making AverageTankTemperature" AS MARKER_POINT;
CREATE TABLE OldAverageTankTemperature
  SELECT tankTemperatureGroupID, zoneID, monthID, hourID, dayID, opModeID, averageTankTemperature
  FROM AverageTankTemperature INNER JOIN OldHourDay USING(hourDayID);
TRUNCATE AverageTankTemperature;
INSERT INTO AverageTankTemperature (tankTemperatureGroupID, zoneID, monthID,
    hourDayID, opModeID, averageTankTemperature, averageTankTemperatureCV, isUserInput) 
  SELECT tankTemperatureGroupID, zoneID, monthID, oatt.dayID AS hourDayID, opModeID,
    sum(averageTankTemperature*actFract) AS averageTankTemperature, 
    NULL AS averageTankTemperatureCV, 'Y' AS isUserInput
  FROM OldAverageTankTemperature AS oatt INNER JOIN HourWeighting4 USING(dayID, hourID)
  GROUP BY tankTemperatureGroupID, zoneID, monthID, oatt.dayID, opModeID ;
FLUSH TABLE AverageTankTemperature;

--
-- SoakActivityFraction
--
-- SELECT "Making SoakActivityFraction" AS MARKER_POINT;
CREATE TABLE OldSoakActivityFraction
  SELECT sourceTypeID, zoneID, monthID, hourID, dayID, opModeID, soakActivityFraction
  FROM SoakActivityFraction INNER JOIN OldHourDay USING(hourDayID);
TRUNCATE SoakActivityFraction;
INSERT INTO SoakActivityFraction (sourceTypeID, zoneID, monthID,
    hourDayID, opModeID, soakActivityFraction, soakActivityFractionCV, isUserInput) 
  SELECT osaf.sourceTypeID, zoneID, monthID, osaf.dayID AS hourDayID, opModeID,
    sum(soakActivityFraction*actFract) AS soakActivityFraction, 
    NULL AS soakActivityFractionCV, 'Y' AS isUserInput
  FROM OldSoakActivityFraction AS osaf INNER JOIN HourWeighting2 USING(sourceTypeID, dayID, hourID)
  GROUP BY osaf.sourceTypeID, zoneID, monthID, osaf.dayID, opModeID ;
FLUSH TABLE SoakActivityFraction;
  
--
-- Drop any New Tables Created 
--

-- FLUSH TABLES;

DROP TABLE IF EXISTS HourWeighting1;
DROP TABLE IF EXISTS HourWeighting2;
DROP TABLE IF EXISTS HourWeighting3;
DROP TABLE IF EXISTS HourWeighting4;
DROP TABLE IF EXISTS OldHourDay;
DROP TABLE IF EXISTS OldAvgSpeedDistribution;
DROP TABLE IF EXISTS OldOpModeDistribution;
DROP TABLE IF EXISTS OldOpModeDistribution2;
DROP TABLE IF EXISTS OldSourceTypeHour;
DROP TABLE IF EXISTS OldSHO;
DROP TABLE IF EXISTS OldSourceHours;
DROP TABLE IF EXISTS OldStarts;
DROP TABLE IF EXISTS OldHotellingHourFraction;
DROP TABLE IF EXISTS AggZoneMonthHour;
DROP TABLE IF EXISTS AggMonthGroupHour;
DROP TABLE IF EXISTS OldSampleVehicleTrip;
DROP TABLE IF EXISTS OldStartsPerVehicle;
DROP TABLE IF EXISTS OldStartsOpModeDistribution;
DROP TABLE IF EXISTS OldStartsHourFraction;
DROP TABLE IF EXISTS OldSoakActivityFraction;
DROP TABLE IF EXISTS OldAverageTankTemperature;

-- FLUSH TABLES;
