/* ***********************************************************************************************************************
-- MySQL Script file to aggregate the DAY level database to the MONTH level
--    performed on the MOVESExecution database
--    after merging of input databases by the InputDataManager
--    and before any MasterLoopable objects are executed.
-- An attempt is made to weight some aggregations by activity.
--
-- Author Wesley Faler
-- Author Gwo Shyu
-- Author Mitch Cumberworth
-- Author Jarrod Brown, Michael Aldridge, Daniel Bizer-Cox, Evan Murray
-- Version 2019-05-27
-- Written By Mitch Cumberworth, April, 2004
-- Change history:
--   Updated by Mitch Cumberworth, July, 2005 for Task 208
--   Updated by Mitch Cumberworth, November, 2005 for Task 210
--   Modified by Cimulus, January, 2005 for Task 210
--   Modified by Gwo Shyu, March 26, 2008 to fix the errors of duplicate entry and table existing
   *********************************************************************************************************************** */

DROP TABLE IF EXISTS SourceTypeOrdering;
DROP TABLE IF EXISTS DayWeighting1;
DROP TABLE IF EXISTS DayWeighting1Sum;
DROP TABLE IF EXISTS DayWeighting1Normalized;
DROP TABLE IF EXISTS DayWeighting2;
DROP TABLE IF EXISTS DayWeighting2Normalized;
DROP TABLE IF EXISTS DayWeighting3;
DROP TABLE IF EXISTS DayWeighting3Normalized;
DROP TABLE IF EXISTS OldHourDay;
DROP TABLE IF EXISTS AggDayVMTFraction;
DROP TABLE IF EXISTS AggSourceTypeDayVMT;
DROP TABLE IF EXISTS AggHPMSVTypeDay;
DROP TABLE IF EXISTS OldAvgSpeedDistribution;
DROP TABLE IF EXISTS OldOpModeDistribution;    
DROP TABLE IF EXISTS OldOpModeDistribution2;
DROP TABLE IF EXISTS OldSourceTypeHour;
DROP TABLE IF EXISTS OldSHO;
DROP TABLE IF EXISTS OldSourceHours;
DROP TABLE IF EXISTS OldStarts;
DROP TABLE IF EXISTS OldHotellingHourFraction;
DROP TABLE IF EXISTS OldSampleVehicleSoakingDay;
DROP TABLE IF EXISTS OldSampleVehicleTrip;
DROP TABLE IF EXISTS OldSampleVehicleDay;
DROP TABLE IF EXISTS OldStartsPerVehicle;
DROP TABLE IF EXISTS OldStartsOpModeDistribution;
DROP TABLE IF EXISTS OldStartsHourFraction;
DROP TABLE IF EXISTS OldStartsPerDayPerVehicle;
DROP TABLE IF EXISTS OldStartsPerDay;
DROP TABLE IF EXISTS OldAverageTankTemperature;
DROP TABLE IF EXISTS OldSoakActivityFraction;
DROP TABLE IF EXISTS OldTotalIdleFraction;
DROP TABLE IF EXISTS OldIdleDayAdjust;

-- 
-- SourceTypeOrdering sets the preference order for what source type should be used as a surrogate for weighting,
-- based on their populations in the default database with the following exceptions: 21s are always preferred if 
-- present for consistency with previous versions of MOVES, and 11s are always preferred last because they typically
-- have the most different temporal distributions compared to all other source types
-- 
CREATE TABLE SourceTypeOrdering (
    sourceTypeID SMALLINT,
    orderPreference SMALLINT
);
INSERT INTO SourceTypeOrdering VALUES (21,1),(31,2),(32,3),(52,4),(61,5),(54,6),(62,7),(43,8),(53,9),(41,10),(42, 1),(51, 2),(11,13);


--
-- Create DayWeighting1 to be used to weight daily activity 
--    by sourceTypeID and RoadtypeID.
-- 
-- Note:  will not sum to unity if all days not included
-- SELECT "Making DayWeighting1" AS MARKER_POINT;
-- Create table explicitly to control column types and avoid significance problems
CREATE TABLE DayWeighting1 (
	sourceTypeID SMALLINT,
	roadTypeID SMALLINT,
	dayID SMALLINT,
	actFract FLOAT);
INSERT INTO DayWeighting1
SELECT day.sourceTypeID, roadTypeID, day.dayID, 
((sum(dayVMTFraction*noOfRealDays*monthVMTFraction))/sum(monthVMTFraction)) as actFract 
FROM DayVMTFraction AS day
INNER JOIN DayOfAnyWeek dow ON dow.dayID = day.dayID
INNER JOIN MonthVMTFraction month ON month.sourceTypeID = day.sourceTypeID
AND month.monthID = day.monthID
GROUP BY day.sourceTypeID,roadTypeID,day.dayID;


CREATE UNIQUE INDEX index1 ON DayWeighting1 (sourceTypeID, roadTypeID, dayID);

create table DayWeighting1Sum (
	sourceTypeID smallint not null,
	roadTypeID smallint not null,
	actFractSum double,
	primary key (sourceTypeID, roadTypeID),
	key (roadTypeID, sourceTypeID)
);

insert into DayWeighting1Sum (sourceTypeID, roadTypeID, actFractSum)
select sourceTypeID, roadTypeID, sum(actFract) as actFractSum
from DayWeighting1
group by sourceTypeID, roadTypeID;

create table DayWeighting1Normalized (
	sourceTypeID smallint not null,
	roadTypeID smallint not null,
	dayID smallint not null,
	actFract double,
	primary key (sourceTypeID, roadTypeID, dayID)
);

insert into DayWeighting1Normalized (sourceTypeID, roadTypeID, dayID, actFract)
select dw.sourceTypeID, dw.roadTypeID, dw.dayID, 
	case when ds.actFractSum > 0 then (dw.actFract / ds.actFractSum) else 0.0 end as actFract
from DayWeighting1 dw
inner join DayWeighting1Sum ds using (sourceTypeID, roadTypeID);

--
-- Create DayWeighting2 to be used to weight daily activity by sourceTypeID only
--
-- Note: will not sum to unity if all days not included
-- SELECT "Making DayWeighting2" AS MARKER_POINT;
-- Create table explicitly to control column types and avoid significance problems
CREATE TABLE DayWeighting2 (
	sourceTypeID SMALLINT,
	dayID SMALLINT,
	actFract FLOAT);
INSERT INTO DayWeighting2
  SELECT dw.sourceTypeID, dayID, 
    (sum(actFract*roadTypeVMTFraction)/sum(roadTypeVMTFraction)) as actFract 
    FROM DayWeighting1 AS dw INNER JOIN RoadTypeDistribution USING (sourceTypeID, roadTypeID)
    GROUP BY dw.sourceTypeID, dayID;
CREATE UNIQUE INDEX index1 ON DayWeighting2 (sourceTypeID, dayID);

--
-- Create DayWeighting2Normalized to be used to weight daily activity by sourceTypeID only
--
-- Note: will not sum to unity if all days not included
-- SELECT "Making DayWeighting2Normalized" AS MARKER_POINT;
-- Create table explicitly to control column types and avoid significance problems
CREATE TABLE DayWeighting2Normalized (
	sourceTypeID SMALLINT,
	dayID SMALLINT,
	actFract FLOAT);
INSERT INTO DayWeighting2Normalized
  SELECT dw.sourceTypeID, dayID, 
    (sum(actFract*roadTypeVMTFraction)/sum(roadTypeVMTFraction)) as actFract 
    FROM DayWeighting1Normalized AS dw INNER JOIN RoadTypeDistribution USING (sourceTypeID, roadTypeID)
    GROUP BY dw.sourceTypeID, dayID;
CREATE UNIQUE INDEX index1 ON DayWeighting2Normalized (sourceTypeID, dayID);

--
-- Create DayWeighting3 to be used to weight daily activity when only dayID field present
--
-- Note: will not sum to unity if all days not included
SELECT "Making DayWeighting3" AS MARKER_POINT;
-- Create table explicitly to control column types and avoid significance problems
CREATE TABLE DayWeighting3 (
	dayID SMALLINT,
	actFract FLOAT);
INSERT INTO DayWeighting3
  SELECT dayID, actFract 
  FROM DayWeighting2 
  WHERE sourceTypeID = (SELECT sourceTypeID FROM DayWeighting2 JOIN SourceTypeOrdering USING (sourceTypeID) GROUP BY sourceTypeID HAVING SUM(actFract) > 0 ORDER BY orderPreference LIMIT 1);
CREATE UNIQUE INDEX index1 ON DayWeighting3 (dayID);

--
-- Create DayWeighting3Normalized to be used to weight daily activity when only dayID field present
--
-- Note: will not sum to unity if all days not included
SELECT "Making DayWeighting3Normalized" AS MARKER_POINT;
-- Create table explicitly to control column types and avoid significance problems
CREATE TABLE DayWeighting3Normalized (
	dayID SMALLINT,
	actFract FLOAT);
INSERT INTO DayWeighting3Normalized
  SELECT dayID, actFract 
  FROM DayWeighting2Normalized
  WHERE sourceTypeID = (SELECT sourceTypeID FROM DayWeighting2Normalized JOIN SourceTypeOrdering USING (sourceTypeID) GROUP BY sourceTypeID HAVING SUM(actFract) > 0 ORDER BY orderPreference LIMIT 1);
CREATE UNIQUE INDEX index1 ON DayWeighting3Normalized (dayID);

  
--
-- HourDay Table  (save old version as it is needed later)
--
-- SELECT "Making HourDay" AS MARKER_POINT;
CREATE TABLE OldHourDay SELECT * from HourDay;
CREATE UNIQUE INDEX index2 ON OldHourDay(hourDayID);
TRUNCATE HourDay;
INSERT INTO HourDay (hourDayID, dayID, hourID)
    VALUES (0,0,0);
FLUSH TABLE HourDay;
  
--
-- HourVMTFraction Table
--
-- Already has index from DAY script.
-- SELECT "Making HourVMTFraction" AS MARKER_POINT;
TRUNCATE HourVMTFraction;
REPLACE INTO HourVMTFraction (sourceTypeID, roadTypeID, dayID, hourID, hourVMTFraction)
  SELECT sourceTypeID, roadTypeID, 0 AS dayID, 0 AS hourID, 1.0 AS hourVMTFraction
  FROM DayWeighting1
  GROUP BY sourceTypeID, roadTypeID;
FLUSH TABLE HourVMTFraction;
  
--
-- DayVMTFraction Table
--
-- SELECT "Making DayVMTFraction" AS MARKER_POINT;
CREATE TABLE AggDayVMTFraction 
  SELECT sourceTypeID, monthID, roadTypeID
    FROM DayVMTFraction 
    GROUP BY sourceTypeID, monthID, roadTypeID;
TRUNCATE DayVMTFraction;
REPLACE INTO DayVMTFraction (sourceTypeID, monthID, roadTypeID, dayID, dayVMTFraction)
  SELECT sourceTypeID, monthID, roadTypeID, 0 AS dayID, 1.0 AS dayVMTFraction
  FROM AggDayVMTFraction;
FLUSH TABLE DayVMTFraction;

--
-- SourceTypeDayVMT Table
--
-- SELECT "Making SourceTypeDayVMT" AS MARKER_POINT;
CREATE TABLE AggSourceTypeDayVMT 
  SELECT yearID, monthID, 0 as dayID, sourceTypeID, sum(VMT*actFract) as VMT
    FROM SourceTypeDayVMT
    INNER JOIN DayWeighting2Normalized USING (sourceTypeID, dayID)
    GROUP BY yearID, monthID, sourceTypeID;
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
  SELECT yearID, monthID, 0 as dayID, hpmsVTypeID, sum(VMT*actFract) as VMT
    FROM HPMSVTypeDay
    INNER JOIN DayWeighting3Normalized USING (dayID)
    GROUP BY yearID, monthID, hpmsVTypeID;
TRUNCATE HPMSVTypeDay;
REPLACE INTO HPMSVTypeDay (yearID, monthID, dayID, hpmsVTypeID, VMT)
  SELECT yearID, monthID, dayID, hpmsVTypeID, VMT
  FROM AggHPMSVTypeDay;
FLUSH TABLE HPMSVTypeDay;

--
-- AvgSpeedDistribution Table
--
-- Table index already created in DAY aggregation 
-- SELECT "Making AvgSpeedDistribution" AS MARKER_POINT;
CREATE TABLE OldAvgSpeedDistribution
  SELECT sourceTypeID, roadTypeID, dayID, avgSpeedBinID, avgSpeedFraction
  FROM AvgSpeedDistribution INNER JOIN OldHourDay USING (hourDayID);
TRUNCATE AvgSpeedDistribution;
REPLACE INTO AvgSpeedDistribution 
	(sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID, avgSpeedFraction)
  SELECT asd.sourceTypeID, asd.roadTypeID, 0 AS hourDayID, asd.avgSpeedBinID, 
    (sum(asd.avgSpeedFraction*dw.actFract)/sum(dw.actFract)) AS avgSpeedFraction
  FROM OldAvgSpeedDistribution AS asd INNER JOIN DayWeighting1 AS dw 
      USING (sourceTypeID, roadTypeID, dayID)
    GROUP BY sourceTypeID, roadTypeID,  avgSpeedBinID;
FLUSH TABLE AvgSpeedDistribution;

--
-- OpModeDistribution
--
-- SELECT "Making OpModeDistribution" AS MARKER_POINT;
CREATE Table OldOpModeDistribution 
  SELECT omd.*, Link.roadTypeID
  FROM OpModeDistribution AS omd INNER JOIN Link USING (linkID);
CREATE Table OldOpModeDistribution2
  SELECT omd.*, OldHourDay.dayID
  FROM OldOpModeDistribution AS omd INNER JOIN OldHourDay USING (hourDayID);
CREATE UNIQUE INDEX index1 ON OldOpModeDistribution2 (sourceTypeID, roadTypeID, dayID);
TRUNCATE OpModeDistribution;
REPLACE INTO OpModeDistribution (sourceTypeID, hourDayID, linkID, polProcessID, opModeID, 
    opModeFraction, opModeFractionCV, isUserInput)
  SELECT omd.sourceTypeID, 0 AS hourDayID, omd.linkID, omd.polProcessID, omd.opModeID,
    (SUM(omd.opModeFraction * dw.actFract)/SUM(dw.actFract)) AS opModeFraction, 
    NULL AS opModeFractionCV, "Y" AS isUserInput
  FROM OldOpModeDistribution2 AS omd INNER JOIN DayWeighting1 AS dw
    USING (sourceTypeID, roadTypeID, dayID )
  GROUP BY omd.sourceTypeID, omd.linkID, omd.polProcessID, omd.opModeID; 
FLUSH TABLE OpModeDistribution;
  
--
-- SourceTypeHour Table
--
-- Table index already created in DAY aggregation 
-- SELECT "Making SourceTypeHour" AS MARKER_POINT;
CREATE TABLE OldSourceTypeHour
  SELECT sourceTypeID, dayID, idleSHOFactor, hotellingDist
  FROM SourceTypeHour INNER JOIN OldHourDay USING (hourDayID);
TRUNCATE SourceTypeHour;
REPLACE INTO SourceTypeHour 
	(sourceTypeID, hourDayID, idleSHOFactor, hotellingDist)
  SELECT sth.sourceTypeID, 0 AS hourDayID, 
    (sum(sth.idleSHOFactor*dw.actFract)/sum(dw.actFract)) AS idleSHOFactor,
    (sum(sth.hotellingDist*dw.actFract)/sum(dw.actFract)) AS hotellingDist
  FROM OldSourceTypeHour AS sth INNER JOIN DayWeighting2 AS dw 
      USING (sourceTypeID, dayID)
    GROUP BY sourceTypeID;
FLUSH TABLE SourceTypeHour;

--
--  SHO    
--
-- SELECT "Making SHO" AS MARKER_POINT;
CREATE TABLE OldSHO
  SELECT SHO.*, dayID
  FROM SHO INNER JOIN OldHourDay USING(hourDayID);
CREATE INDEX index1 ON OldSHO (monthID, yearID, ageID, linkID, sourceTypeID);
TRUNCATE SHO;
REPLACE INTO SHO (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, SHO, SHOCV, distance, isUserInput)
  SELECT 0 AS hourDayID, monthID, yearID, ageID, linkID, sourceTypeID, 
    sum(SHO) AS SHO, NULL AS SHOCV, sum(distance) AS distance, "Y" AS isUserInput
  FROM OldSHO 
  GROUP BY monthID, yearID, ageID, linkID, sourceTypeID;
FLUSH TABLE SHO;
  
--
--  SourceHours    
--
-- SELECT "Making SourceHours" AS MARKER_POINT;
CREATE TABLE OldSourceHours
  SELECT SourceHours.*, dayID
  FROM SourceHours INNER JOIN OldHourDay USING(hourDayID);
CREATE INDEX index1 ON OldSourceHours (monthID, yearID, ageID, linkID, sourceTypeID);
TRUNCATE SourceHours;
REPLACE INTO SourceHours (hourDayID, monthID, yearID, ageID, linkID, 
    sourceTypeID, sourceHours, sourceHoursCV, isUserInput)
  SELECT 0 AS hourDayID, monthID, yearID, ageID, linkID, sourceTypeID, 
    sum(sourceHours) AS sourceHours, NULL AS sourceHoursCV, "Y" AS isUserInput
  FROM OldSourceHours
  GROUP BY monthID, yearID, ageID, linkID, sourceTypeID;
FLUSH TABLE SourceHours;
  
--
--  Starts
--
-- SELECT "Making Starts" AS MARKER_POINT;
CREATE TABLE OldStarts
  SELECT Starts.*, dayID
  FROM Starts INNER JOIN OldHourDay USING(hourDayID);
CREATE INDEX index1 ON OldStarts (monthID, yearID, ageID, zoneID, sourceTypeID);
TRUNCATE Starts;
REPLACE INTO Starts (hourDayID, monthID, yearID, ageID, zoneID, 
    sourceTypeID, starts, startsCV, isUserInput)
  SELECT 0 AS hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID, 
    sum(starts) AS starts, NULL AS startsCV, "Y" AS isUserInput
  FROM OldStarts
  GROUP BY monthID, yearID, ageID, zoneID, sourceTypeID;
FLUSH TABLE Starts;

-- HotellingHourFraction
--
-- SELECT "Making HotellingHourFraction" AS MARKER_POINT;
CREATE TABLE OldHotellingHourFraction
  SELECT * FROM HotellingHourFraction;
TRUNCATE HotellingHourFraction;
INSERT INTO HotellingHourFraction (zoneID, dayID, hourID, hourFraction)
  SELECT zoneID, 0 as dayID, 0 as hourID, sum(hourFraction) / aggregation.hourFractionTotal as hourFraction
  FROM OldHotellingHourFraction INNER JOIN
       (SELECT zoneID, sum(hourFraction) as hourFractionTotal
        FROM OldHotellingHourFraction
        GROUP BY zoneID) AS aggregation
  USING (zoneID)
  GROUP BY zoneID;
FLUSH TABLE HotellingHourFraction;

-- HotellingHoursPerDay
-- 
-- SELECT "Making HotellingHoursPerDay" AS MARKER_POINT;
CREATE TABLE OldHotellingHoursPerDay
	SELECT * FROM HotellingHoursPerDay;
TRUNCATE HotellingHoursPerDay;
INSERT INTO HotellingHoursPerDay (yearID, zoneID, dayID, hotellinghoursperday)
	SELECT yearID, zoneID, 0 as dayID, sum(hotellinghoursperday * noOfRealDays / 7) as hotellinghoursperday
	FROM OldHotellingHoursPerDay
	INNER JOIN dayOfAnyWeek using (dayID)
	GROUP BY yearID, zoneID;
FLUSH TABLE HotellingHoursPerDay;

--
-- SampleVehicleSoakingDay
-- 
-- SELECT "Making SampleVehicleSoakingDay" AS MARKER_POINT;
CREATE TABLE OldSampleVehicleSoakingDay SELECT * FROM SampleVehicleSoakingDay;
TRUNCATE SampleVehicleSoakingDay;
REPLACE INTO SampleVehicleSoakingDay (soakDayID, sourceTypeID, dayID, F)
  SELECT soakDayID, sourceTypeID, 0 as dayID, sum(F* (CASE WHEN dayID=5 THEN 5 ELSE 2 END))/sum(CASE WHEN dayID=5 THEN 5 ELSE 2 END)
  FROM OldSampleVehicleSoakingDay
  GROUP BY soakDayID, sourceTypeID
  ORDER BY NULL;
FLUSH TABLE SampleVehicleSoakingDay;

--
-- SampleVehicleTrip
-- 
-- SELECT "Making SampleVehicleTrip" AS MARKER_POINT;
CREATE TABLE OldSampleVehicleTrip SELECT * FROM SampleVehicleTrip;
TRUNCATE SampleVehicleTrip;
REPLACE INTO SampleVehicleTrip (vehID, tripID, dayID, hourID, priorTripID, 
	keyOnTime, keyOffTime) 
  SELECT vehID, tripID, 0 AS dayID, 0 as hourID, priorTripID, keyOnTime, keyOffTime
  FROM OldSampleVehicleTrip;
FLUSH TABLE SampleVehicleTrip;

--
-- SampleVehicleDay
-- 
-- SELECT "Making SampleVehicleDay" AS MARKER_POINT;
CREATE TABLE OldSampleVehicleDay SELECT * FROM SampleVehicleDay;
TRUNCATE SampleVehicleDay;
REPLACE INTO SampleVehicleDay (vehID, dayID, sourceTypeID) 
  SELECT vehID, 0 AS dayID, sourceTypeID
  FROM OldSampleVehicleDay;
FLUSH TABLE SampleVehicleDay;

--
-- StartsPerVehicle
-- 
-- SELECT "Making StartsPerVehicle" AS MARKER_POINT;
CREATE TABLE OldStartsPerVehicle SELECT * FROM StartsPerVehicle ;
TRUNCATE StartsPerVehicle;
REPLACE INTO StartsPerVehicle (sourceTypeID, hourDayID, 
	startsPerVehicle, startsPerVehicleCV) 
  SELECT sourceTypeID, 0 AS hourDayID, sum(startsPerVehicle) as startsPerVehicle,
    NULL AS startsPerVehicleCV
  FROM OldStartsPerVehicle GROUP BY sourceTypeID;
FLUSH TABLE StartsPerVehicle;

-- StartsOpModeDistribution
-- 
-- SELECT "Making StartsOpModeDistribution" AS MARKER_POINT;
CREATE TABLE OldStartsOpModeDistribution
  SELECT * FROM StartsOpModeDistribution;
TRUNCATE StartsOpModeDistribution;
INSERT INTO StartsOpModeDistribution (dayID, hourID, sourceTypeID, ageID,
  opModeID, opModeFraction, isUserInput)
  SELECT 0 as dayID, 0 as hourID, sourceTypeID, ageID, opModeID,
    sum(opModeFraction * dayID * startsPerDayPerVehicle) / aggregation.opModeFractionTotal as opModeFraction, 'Y' as isUserInput
  FROM OldStartsOpModeDistribution INNER JOIN
       (SELECT sourceTypeID, ageID, sum(opModeFraction * dayID * startsPerDayPerVehicle) as opModeFractionTotal
        FROM OldStartsOpModeDistribution INNER JOIN startsperdaypervehicle
        USING (dayID, sourceTypeID)
        GROUP BY sourceTypeID, ageID) AS aggregation
  USING (sourceTypeID, ageID) INNER JOIN startsperdaypervehicle
  USING (dayID, sourceTypeID)
  GROUP BY sourceTypeID, ageID, opModeID;
FLUSH TABLE StartsOpModeDistribution;

-- StartsHourFraction
--
-- SELECT "Making StartsHourFraction" AS MARKER_POINT;
CREATE TABLE OldStartsHourFraction
  SELECT * FROM StartsHourFraction;
TRUNCATE StartsHourFraction;
INSERT INTO StartsHourFraction (dayID, hourID, sourceTypeID, allocationFraction)
  SELECT 0 as dayID, 0 as hourID, sourceTypeID, sum(allocationFraction) / aggregation.allocationFractionTotal as allocationFraction
  FROM OldStartsHourFraction INNER JOIN
       (SELECT sourceTypeID, sum(allocationFraction) as allocationFractionTotal
        FROM OldStartsHourFraction
        GROUP BY sourceTypeID) AS aggregation
  USING (sourceTypeID)
  GROUP BY sourceTypeID;
FLUSH TABLE StartsHourFraction;

-- StartsPerDayPerVehicle
-- 
-- SELECT "Making StartsPerDayPerVehicle" AS MARKER_POINT;
CREATE TABLE OldStartsPerDayPerVehicle
  SELECT * FROM StartsPerDayPerVehicle;
TRUNCATE StartsPerDayPerVehicle;
INSERT INTO StartsPerDayPerVehicle (dayID, sourceTypeID, startsPerDayPerVehicle)
  SELECT 0 as dayID, sourceTypeID, sum((startsPerDayPerVehicle * dayID) / 7) as startsPerDayPerVehicle
  FROM OldStartsPerDayPerVehicle
  GROUP BY sourceTypeID;
FLUSH TABLE StartsPerDayPerVehicle;

-- StartsPerDay
-- 
-- SELECT "Making StartsPerDay" AS MARKER_POINT;
CREATE TABLE OldStartsPerDay
  SELECT * FROM StartsPerDay;
TRUNCATE StartsPerDay;
INSERT INTO StartsPerDay (dayID, sourceTypeID, startsPerDay)
  SELECT 0 as dayID, sourceTypeID, sum((startsPerDay * dayID) / 7) as startsPerDay
  FROM OldStartsPerDay
  GROUP BY sourceTypeID;
FLUSH TABLE StartsPerDay;
  
--
-- AverageTankTemperature
--
-- SELECT "Making AverageTankTemperature" AS MARKER_POINT;
CREATE TABLE OldAverageTankTemperature
  SELECT tankTemperatureGroupID, zoneID, monthID, dayID, opModeID, averageTankTemperature
  FROM AverageTankTemperature INNER JOIN OldHourDay USING (hourDayID);
TRUNCATE AverageTankTemperature;
REPLACE INTO AverageTankTemperature (tankTemperatureGroupID, zoneID, monthID,
    hourDayID, opModeID, averageTankTemperature, averageTankTemperatureCV, isUserInput) 
  SELECT tankTemperatureGroupID, zoneID, monthID, 0 AS hourDayID, opModeID,
    sum(averageTankTemperature*actFract) AS averageTankTemperature, 
    NULL AS averageTankTemperatureCV, 'Y' AS isUserInput
  FROM OldAverageTankTemperature AS oatt INNER JOIN DayWeighting3 USING(dayID)
  GROUP BY tankTemperatureGroupID, zoneID, monthID, opModeID ;
FLUSH TABLE AverageTankTemperature;

--
-- SoakActivityFraction
--
-- SELECT "Making SoakActivityFraction" AS MARKER_POINT;
CREATE TABLE OldSoakActivityFraction
  SELECT sourceTypeID, zoneID, monthID, dayID, opModeID, soakActivityFraction
  FROM SoakActivityFraction INNER JOIN OldHourDay USING(hourDayID);
TRUNCATE SoakActivityFraction;
REPLACE INTO SoakActivityFraction (sourceTypeID, zoneID, monthID,
    hourDayID, opModeID, soakActivityFraction, soakActivityFractionCV, isUserInput) 
  SELECT osaf.sourceTypeID, zoneID, monthID, 0 AS hourDayID, opModeID,
    sum(soakActivityFraction*actFract) AS soakActivityFraction, 
    NULL AS soakActivityFractionCV, 'Y' AS isUserInput
  FROM OldSoakActivityFraction AS osaf INNER JOIN DayWeighting2 USING(sourceTypeID, dayID)
  GROUP BY osaf.sourceTypeID, zoneID, monthID, opModeID ;
FLUSH TABLE SoakActivityFraction;

--
-- TotalIdleFraction
--
-- SELECT "Making TotalIdleFraction" AS MARKER_POINT;
CREATE TABLE OldTotalIdleFraction
  SELECT sourceTypeID, minModelYearID, maxModelYearID, monthID, dayID, idleRegionID, countyTypeID, totalIdleFraction
    FROM TotalIdleFraction;
TRUNCATE TotalIdleFraction;
INSERT INTO TotalIdleFraction (sourceTypeID, minModelYearID, maxModelYearID, monthID, dayID, idleRegionID, countyTypeID, totalIdleFraction)
  SELECT sourceTypeID,  minModelYearID, maxModelYearID, monthID, 0 as dayID, idleRegionID, countyTypeID, sum(totalIdleFraction*DayWeighting2Normalized.actFract)
  FROM OldTotalIdleFraction
  LEFT JOIN DayWeighting2Normalized
  USING (sourceTypeID, dayID)
  GROUP BY sourceTypeID,  minModelYearID, maxModelYearID, monthID, idleRegionID, countyTypeID;
FLUSH TABLE TotalIdleFraction;

-- IdleDayAdjust
-- 
-- SELECT "Making IdleDayAdjust" AS MARKER_POINT;
CREATE TABLE OldIdleDayAdjust
	SELECT * FROM IdleDayAdjust;
TRUNCATE IdleDayAdjust;
INSERT INTO IdleDayAdjust (sourceTypeID, dayID, idleDayAdjust)
	SELECT sourceTypeID, 0 as dayID, sum(idleDayAdjust * noOfRealDays / 7) as idleDayAdjust
	FROM OldIdleDayAdjust
	JOIN dayOfAnyWeek using(dayID)
	GROUP BY sourceTypeID;
FLUSH TABLE IdleDayAdjust;
	
	
--
-- DayOfAnyWeek Table
--
-- SELECT "Making DayOfAnyWeek" AS MARKER_POINT;
TRUNCATE DayOfAnyWeek;
INSERT INTO DayOfAnyWeek (dayID, dayName, noOfRealDays)
  VALUES (0, "Whole Week", 7);
FLUSH TABLE DayOfAnyWeek;

--
-- Drop any New Tables Created 
--

-- FLUSH TABLES;

DROP TABLE IF EXISTS SourceTypeOrdering;
DROP TABLE IF EXISTS DayWeighting1;
DROP TABLE IF EXISTS DayWeighting1Sum;
DROP TABLE IF EXISTS DayWeighting1Normalized;
DROP TABLE IF EXISTS DayWeighting2;
DROP TABLE IF EXISTS DayWeighting2Normalized;
DROP TABLE IF EXISTS DayWeighting3;
DROP TABLE IF EXISTS DayWeighting3Normalized;
DROP TABLE IF EXISTS OldHourDay;
DROP TABLE IF EXISTS AggDayVMTFraction;
DROP TABLE IF EXISTS AggSourceTypeDayVMT;
DROP TABLE IF EXISTS AggHPMSVTypeDay;
DROP TABLE IF EXISTS OldAvgSpeedDistribution;
DROP TABLE IF EXISTS OldOpModeDistribution;    
DROP TABLE IF EXISTS OldOpModeDistribution2;
DROP TABLE IF EXISTS OldSourceTypeHour;
DROP TABLE IF EXISTS OldSHO;
DROP TABLE IF EXISTS OldSourceHours;
DROP TABLE IF EXISTS OldStarts;
DROP TABLE IF EXISTS OldHotellingHourFraction;
DROP TABLE IF EXISTS OldHotellingHoursPerDay;
DROP TABLE IF EXISTS OldSampleVehicleSoakingDay;
DROP TABLE IF EXISTS OldSampleVehicleTrip;
DROP TABLE IF EXISTS OldSampleVehicleDay;
DROP TABLE IF EXISTS OldStartsPerVehicle;
DROP TABLE IF EXISTS OldStartsOpModeDistribution;
DROP TABLE IF EXISTS OldStartsHourFraction;
DROP TABLE IF EXISTS OldStartsPerDayPerVehicle;
DROP TABLE IF EXISTS OldStartsPerDay;
DROP TABLE IF EXISTS OldAverageTankTemperature;
DROP TABLE IF EXISTS OldSoakActivityFraction;
DROP TABLE IF EXISTS OldTotalIdleFraction;
DROP TABLE IF EXISTS OldIdleDayAdjust;

-- FLUSH TABLES;
  
