-- Version 2013-09-15
-- Two SQL statements at lines 828-862 optimized per consultant recommendations

-- @algorithm
-- @owner Energy Consumption Calculator

-- Section Create Remote Tables for Extracted Data

##create.County##;
TRUNCATE TABLE County;

##create.EmissionProcess##;
TRUNCATE TABLE EmissionProcess;

##create.EmissionRate##;
TRUNCATE TABLE EmissionRate;

##create.FuelFormulation##;
TRUNCATE TABLE FuelFormulation;

##create.FuelSubtype##;
TRUNCATE TABLE FuelSubtype;

##create.FuelSupply##;
TRUNCATE TABLE FuelSupply;

##create.FullACAdjustment##;
TRUNCATE TABLE FullACAdjustment;

##create.generalFuelRatio##;
TRUNCATE TABLE generalFuelRatio;

##create.HourDay##;
TRUNCATE TABLE HourDay;

##create.Link##;
TRUNCATE TABLE Link;

##create.ModelYear##;
TRUNCATE ModelYear;

##create.MonthGroupHour##;
TRUNCATE TABLE MonthGroupHour;

##create.MonthofAnyYear##;
TRUNCATE TABLE MonthofAnyYear;

##create.OpModeDistribution##;
TRUNCATE TABLE OpModeDistribution;

##create.Pollutant##;
TRUNCATE TABLE Pollutant;

##create.PollutantProcessAssoc##;
TRUNCATE TABLE PollutantProcessAssoc;

##create.PollutantProcessModelYear##;
TRUNCATE TABLE PollutantProcessModelYear;

##create.RunSpecSourceType##;
TRUNCATE RunSpecSourceType;

##create.SourceBin##;
TRUNCATE TABLE SourceBin;

##create.SourceBinDistribution##;
TRUNCATE TABLE SourceBinDistribution;

##create.SourceTypeAge##;
TRUNCATE TABLE SourceTypeAge;

##create.SourceTypeModelYear##;
TRUNCATE TABLE SourceTypeModelYear;

##create.TemperatureAdjustment##;
TRUNCATE TABLE TemperatureAdjustment;

##create.Year##;
TRUNCATE TABLE Year;

##create.Zone##;
TRUNCATE TABLE Zone;

##create.ZoneMonthHour##;
TRUNCATE TABLE ZoneMonthHour;

-- Section Running Exhaust
##create.SHO##;
TRUNCATE TABLE SHO;
-- End Section Running Exhaust
-- Section Start Exhaust
##create.Starts##;
TRUNCATE TABLE Starts;
-- End Section Start Exhaust
-- Section Extended Idle Exhaust
##create.ExtendedIdleHours##;
TRUNCATE TABLE ExtendedIdleHours;
-- End Section Extended Idle Exhaust
-- Section Auxiliary Power Exhaust
##create.hotellingHours##;
TRUNCATE TABLE hotellingHours;

-- @input HotellingActivityDistribution
##create.hotellingActivityDistribution##;
TRUNCATE TABLE hotellingActivityDistribution;

##create.RunSpecHourDay##;
TRUNCATE TABLE RunSpecHourDay;

create table if not exists hotellingOperatingMode (
	opModeID smallint(6) not null,
	primary key (opModeID)
);
-- End Section Auxiliary Power Exhaust
-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache SELECT * INTO OUTFILE '##County##'
FROM County
WHERE countyID = ##context.iterLocation.countyRecordID##;

cache SELECT * INTO OUTFILE '##Zone##'
FROM Zone
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT Link.*
INTO OUTFILE '##Link##'
FROM Link
WHERE linkID = ##context.iterLocation.linkRecordID##;

cache SELECT * INTO OUTFILE '##ModelYear##'
FROM ModelYear;

cache SELECT * 
INTO OUTFILE '##EmissionProcess##'
FROM EmissionProcess
WHERE processID=##context.iterProcess.databaseKey##;

-- @input FuelSupply
-- @input MonthOfAnyYear
-- @input Year
-- @input RunSpecMonthGroup
drop table if exists tempFuelFormulation;
create table if not exists tempFuelFormulation (
	fuelFormulationID int not null primary key
);
insert into tempFuelFormulation (fuelFormulationID)
SELECT distinct fuelFormulationID
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
and MonthOfAnyYear.monthID = ##context.monthID##;

SELECT gfr.* INTO OUTFILE '##generalFuelRatio##'
FROM generalFuelRatio gfr
INNER JOIN tempFuelFormulation tff on tff.fuelFormulationID = gfr.fuelFormulationID
WHERE polProcessID IN (##pollutantProcessIDs##)
AND minModelYearID <= ##context.year##;

drop table tempFuelFormulation;

cache SELECT MonthOfAnyYear.*
INTO OUTFILE '##MonthOfAnyYear##'
FROM MonthOfAnyYear
WHERE MonthOfAnyYear.monthID = ##context.monthID##;

cache SELECT DISTINCT MonthGroupHour.* 
INTO OUTFILE '##MonthGroupHour##'
FROM MonthGroupHour, RunSpecMonthGroup, RunSpecHour, MonthOfAnyYear
WHERE RunSpecMonthGroup.monthGroupID = MonthGroupHour.monthGroupID
AND MonthGroupHour.hourID = RunSpecHour.hourID
AND MonthOfAnyYear.monthGroupID = RunSpecMonthGroup.monthGroupID
AND MonthOfAnyYear.monthID = ##context.monthID##;

cache SELECT *
INTO OUTFILE '##Pollutant##'
FROM Pollutant;

cache SELECT DISTINCT ZoneMonthHour.* 
INTO OUTFILE '##ZoneMonthHour##'
FROM ZoneMonthHour,RunSpecHour
WHERE zoneID = ##context.iterLocation.zoneRecordID##
AND ZoneMonthHour.monthID = ##context.monthID##
AND RunSpecHour.hourID = ZoneMonthHour.hourID;

cache SELECT DISTINCT HourDay.* 
INTO OUTFILE '##HourDay##'
FROM HourDay,RunSpecHour,RunSpecDay
WHERE HourDay.dayID = RunSpecDay.dayID
AND HourDay.hourID = RunSpecHour.hourID;

cache SELECT DISTINCT SourceBinDistribution.* 
INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT SourceBin.* 
INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

SELECT * 
INTO OUTFILE '##OpModeDistribution##'
FROM OpModeDistribution, RunSpecSourceType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND linkID = ##context.iterLocation.linkRecordID##
AND RunSpecSourceType.sourceTypeID = OpModeDistribution.sourceTypeID;

cache SELECT TemperatureAdjustment.* 
INTO OUTFILE '##TemperatureAdjustment##'
FROM TemperatureAdjustment
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT * 
INTO OUTFILE '##FullACAdjustment##'
FROM FullACAdjustment, RunSpecSourceType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND RunSpecSourceType.sourceTypeID = FullACAdjustment.sourceTypeID;

cache SELECT DISTINCT EmissionRate.* 
INTO OUTFILE '##EmissionRate##'
FROM EmissionRate, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE RunSpecSourceFuelType.fuelTypeID = SourceBin.fuelTypeID
AND EmissionRate.polProcessID = SourceBinDistribution.polProcessID
AND EmissionRate.sourceBinID = SourceBin.sourceBinID
AND EmissionRate.sourceBinID = SourceBinDistribution.sourceBinID
AND SourceBin.sourceBinID = SourceBinDistribution.sourceBinID
AND RunSpecSourceFuelType.sourceTypeID = SourceTypeModelYear.sourceTypeID
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.modelYearID >= ##context.year## - 30
AND EmissionRate.polProcessID IN (##pollutantProcessIDs##);

cache SELECT ff.* INTO OUTFILE '##FuelFormulation##'
FROM FuelFormulation ff
INNER JOIN FuelSupply fs ON fs.fuelFormulationID = ff.fuelFormulationID
INNER JOIN Year y ON y.fuelYearID = fs.fuelYearID
INNER JOIN RunSpecMonthGroup rsmg ON rsmg.monthGroupID = fs.monthGroupID
INNER JOIN MonthOfAnyYear ON MonthOfAnyYear.monthGroupID = rsmg.monthGroupID
WHERE fuelRegionID = ##context.fuelRegionID## AND
yearID = ##context.year##
AND MonthOfAnyYear.monthID = ##context.monthID##
GROUP BY ff.FuelFormulationID;

cache SELECT FuelSupply.* 
INTO OUTFILE '##FuelSupply##'
FROM FuelSupply, RunSpecMonthGroup, Year, MonthOfAnyYear
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND FuelSupply.fuelYearID = Year.fuelYearID
AND FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID
AND MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID
AND MonthOfAnyYear.monthID = ##context.monthID##;

cache SELECT * 
INTO OUTFILE '##FuelSubtype##'
FROM FuelSubtype;

cache SELECT * 
INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE processID=##context.iterProcess.databaseKey##;

cache SELECT *
INTO OUTFILE '##PollutantProcessModelYear##'
FROM PollutantProcessModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT * INTO OUTFILE '##RunSpecSourceType##'
FROM RunSpecSourceType;

cache SELECT SourceTypeModelYear.* 
INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear,RunSpecSourceType
WHERE SourceTypeModelYear.sourceTypeID = RunSpecSourceType.sourceTypeID
AND modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30;

cache SELECT SourceTypeAge.* 
INTO OUTFILE '##SourceTypeAge##'
FROM SourceTypeAge,RunSpecSourceType
WHERE SourceTypeAge.sourceTypeID = RunSpecSourceType.sourceTypeID;

cache SELECT Year.*
INTO OUTFILE '##Year##'
FROM Year
WHERE yearID = ##context.year##;

-- Section Running Exhaust
SELECT  SHO.* 
INTO OUTFILE '##SHO##'
FROM SHO
WHERE yearID = ##context.year##
AND monthID = ##context.monthID##
AND linkID = ##context.iterLocation.linkRecordID##;
-- End Section Running Exhaust

-- Section Start Exhaust
SELECT Starts.* 
INTO OUTFILE '##Starts##'
FROM Starts
WHERE yearID = ##context.year##
AND monthID = ##context.monthID##
AND zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section Start Exhaust

-- Section Extended Idle Exhaust
SELECT ExtendedIdleHours.* 
INTO OUTFILE '##ExtendedIdleHours##'
FROM ExtendedIdleHours
WHERE yearID = ##context.year##
AND monthID = ##context.monthID##
AND zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section Extended Idle Exhaust

-- Section Auxiliary Power Exhaust
SELECT hotellingHours.* 
INTO OUTFILE '##hotellingHours##'
FROM hotellingHours
WHERE yearID = ##context.year##
AND monthID = ##context.monthID##
AND zoneID = ##context.iterLocation.zoneRecordID##;

cache select * into outfile '##hotellingActivityDistribution##'
from hotellingActivityDistribution
where (beginModelYearID <= ##context.year## - 30 and endModelYearID >= ##context.year## - 30)
or (beginModelYearID <= ##context.year## and endModelYearID >= ##context.year##)
or (beginModelYearID >= ##context.year## - 30 and endModelYearID <= ##context.year##);

cache select * into outfile '##RunSpecHourDay##'
from RunSpecHourDay;

cache select opModeID into outfile '##hotellingOperatingMode##'
from operatingMode
where opModeID >= 201 and opModeID <= 299;
-- End Section Auxiliary Power Exhaust

-- End Section Extract Data

-- Section Local Data Removal
-- (common code here)
-- Section Running Exhaust
-- End Section Running Exhaust
-- Section Start Exhaust
-- End Section Start Exhaust
-- Section Extended Idle Exhaust
-- End Section Extended Idle Exhaust
-- Section Auxiliary Power Exhaust
-- End Section Auxiliary Power Exhaust
-- End Section Local Data Removal

-- Section Processing

-- Add default OpModeDistribution entries if needed
-- Section Fossil Energy Without OpModeDistribution
-- Section Running Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9301, ##FossilEnergyOpModeID##, 1, 0 FROM SHO;
-- End Section Running Exhaust
-- Section Start Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9302, ##FossilEnergyOpModeID##, 1, 0 FROM Starts;
-- End Section Start Exhaust
-- Section Extended Idle Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9390, ##FossilEnergyOpModeID##, 1, 0 FROM ExtendedIdleHours;
-- End Section Extended Idle Exhaust
-- Section Auxiliary Power Exhaust
INSERT IGNORE INTO FullACAdjustment (sourceTypeID,polProcessID,opModeID,fullACAdjustment,fullACAdjustmentCV)
select 62,9391,opModeID,1,null from hotellingOperatingMode;
-- End Section Auxiliary Power Exhaust
-- End Section Fossil Energy Without OpModeDistribution

-- Section Petroleum Energy Without OpModeDistribution
-- Section Running Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9201, ##PetroleumEnergyOpModeID##, 1, 0 FROM SHO;
-- End Section Running Exhaust
-- Section Start Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9202, ##PetroleumEnergyOpModeID##, 1, 0 FROM Starts;
-- End Section Start Exhaust
-- Section Extended Idle Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9290, ##PetroleumEnergyOpModeID##, 1, 0 FROM ExtendedIdleHours;
-- End Section Extended Idle Exhaust
-- Section Auxiliary Power Exhaust
INSERT IGNORE INTO FullACAdjustment (sourceTypeID,polProcessID,opModeID,fullACAdjustment,fullACAdjustmentCV)
select 62,9291,opModeID,1,null from hotellingOperatingMode;
-- End Section Auxiliary Power Exhaust
-- End Section Petroleum Energy Without OpModeDistribution

-- Section Total Energy Without OpModeDistribution
-- Section Running Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9101, ##TotalEnergyOpModeID##, 1, 0 FROM SHO;
-- End Section Running Exhaust
-- Section Start Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9102, ##TotalEnergyOpModeID##, 1, 0 FROM Starts;
-- End Section Start Exhaust
-- Section Extended Idle Exhaust
INSERT IGNORE INTO OpModeDistribution ( sourceTypeID,hourDayID,linkID,polProcessID,opModeID,opModeFraction,opModeFractionCV)
SELECT DISTINCT sourceTypeID, hourDayID, ##context.iterLocation.linkRecordID##, 9190, ##TotalEnergyOpModeID##, 1, 0 FROM ExtendedIdleHours;
-- End Section Extended Idle Exhaust
-- Section Auxiliary Power Exhaust
INSERT IGNORE INTO FullACAdjustment (sourceTypeID,polProcessID,opModeID,fullACAdjustment,fullACAdjustmentCV)
select 62,9191,opModeID,1,null from hotellingOperatingMode;
-- End Section Auxiliary Power Exhaust

-- End Section Total Energy Without OpModeDistribution

ANALYZE TABLE OpModeDistribution;

-- Section Petroleum Energy
-- @algorithm Create PetroleumFraction table.
-- petroleumFraction(countyID, yearID, monthGroupID, fuelTypeID) = sum(marketShare * fuelSubtypePetroleumFraction)
CREATE TABLE IF NOT EXISTS PetroleumFraction (
	countyID		INTEGER		NOT NULL,
	yearID			SMALLINT	NOT NULL,
	monthGroupID		SMALLINT	NOT NULL,
	fuelTypeID 		SMALLINT	NOT NULL,
	petroleumFraction 	FLOAT		NULL,
	petroleumFractionV	FLOAT		NULL,
	INDEX XPKPetroleumFraction (
		countyID, yearID, monthGroupID, fuelTypeID)
);

TRUNCATE PetroleumFraction;

INSERT INTO PetroleumFraction (
	countyID,
	yearID,
	monthGroupID,
	fuelTypeID,
	petroleumFraction,
    petroleumFractionV )
SELECT 
	##context.iterLocation.countyRecordID## as countyID,
	y.yearID,
	fs.monthGroupID,
	fst.fuelTypeID,
	SUM(fs.marketShare * fst.fuelSubtypePetroleumFraction),
	null
FROM
	FuelSupply fs,
	FuelFormulation ff,
	Year y,
	FuelSubType fst
WHERE
	ff.fuelFormulationID = fs.fuelFormulationID AND
	fst.fuelSubTypeID = ff.fuelSubTypeID AND
	fs.fuelYearID = y.fuelYearID AND
	y.yearID = ##context.year##
GROUP BY
	fs.fuelRegionID,
	y.yearID,
	fs.monthGroupID,
	fst.fuelTypeID;

ANALYZE TABLE PetroleumFraction;

-- End Section Petroleum Energy

-- Section Fossil Fuel Energy
-- @algorithm Create FossilFraction table.
-- fossilFraction(countyID, yearID, monthGroupID, fuelTypeID) = sum(marketShare * fuelSubtypeFossilFraction)
CREATE TABLE IF NOT EXISTS FossilFraction (
	countyID		INTEGER		NOT NULL,
	yearID			SMALLINT	NOT NULL,
	monthGroupID		SMALLINT	NOT NULL,
	fuelTypeID		SMALLINT	NOT NULL,
	fossilFraction 		FLOAT		NULL,
	fossilFractionV		FLOAT		NULL,
	INDEX XPKFossilFraction (
		countyID, yearID, monthGroupID, fuelTypeID)
);

TRUNCATE FossilFraction;

INSERT INTO FossilFraction (
	countyID,
	yearID,
	monthGroupID,
	fuelTypeID,
	fossilFraction,
	fossilFractionV)
SELECT 
	##context.iterLocation.countyRecordID## as countyID,
	y.yearID,
	fs.monthGroupID,
	fst.fuelTypeID,
	SUM(fs.marketShare * fst.fuelSubtypeFossilFraction),
	null
FROM
	FuelSupply fs,
	FuelFormulation ff,
	Year y,
	FuelSubType fst
WHERE
	ff.fuelFormulationID = fs.fuelFormulationID AND
	fst.fuelSubTypeID = ff.fuelSubTypeID AND
	y.fuelYearID = fs.fuelYearID AND
	y.yearID = ##context.year##
GROUP BY
	fs.fuelRegionID,
	y.yearID,
	fs.monthGroupID,
	fst.fuelTypeID;

ANALYZE TABLE FossilFraction;

-- End Section Fossil Fuel Energy

--
-- ECCP-1b: Convert Age to Model Year For Analysis Year
--
--  ModelYearID = YearID - AgeID (or AgeID = YearID - ModelYearID, where YearID = analysisYear)
--	Can't find anything to do here, yet.
--

--
-- ECCP-2: Calculate adjustments
--
--	Note: Adjustments are multiplicative, with a value of 1.0 meaning no effect
--
-- ECCP-2a: Calculate air conditioning adjustment (for analysisYear)
--

-- Preliminary calculation (1): ACOnFraction
-- @algorithm ACOnFraction(monthID,zoneID,hourID) = ACActivityTermA+zmh.heatIndex*(mgh.ACActivityTermB+mgh.ACActivityTermC*zmh.heatIndex)
-- @condition 0 <= ACOnFraction <= 1
CREATE TABLE IF NOT EXISTS ACOnFraction (
	monthID			SMALLINT NOT NULL,
	zoneID			INTEGER NOT NULL,
	hourID			SMALLINT NOT NULL,
	ACOnFraction	FLOAT NULL,
	INDEX XPKACOnFraction (monthID, zoneID, hourID)
);

TRUNCATE ACOnFraction;
INSERT INTO ACOnFraction (
	monthID,
	zoneID,
	hourID,
	ACOnFraction)
SELECT
	zmh.monthID,
	zmh.zoneID,
	mgh.hourID,
	(ACActivityTermA+zmh.heatIndex*(mgh.ACActivityTermB+mgh.ACActivityTermC*zmh.heatIndex))
FROM 
	ZoneMonthHour zmh,
	MonthOfAnyYear may,
	MonthGroupHour mgh
WHERE
	may.monthID = zmh.monthID AND
	mgh.monthGroupID = may.monthGroupID AND
	mgh.hourID = zmh.hourID;

ANALYZE TABLE ACOnFraction;

UPDATE ACOnFraction SET ACOnFraction = 1 WHERE ACOnFraction > 1;
UPDATE ACOnFraction SET ACOnFraction = 0 WHERE ACOnFraction < 0;

-- Preliminary calculation (2): ACActivityFraction
-- @algorithm ACActivityFraction(sourceTypeID,modelYearID,monthID,zoneID,hourID)=ACOnFraction * ACPenetrationFraction * FunctioningACFraction
CREATE TABLE IF NOT EXISTS ACActivityFraction (
	sourceTypeID	SMALLINT NOT NULL,
	modelYearID		SMALLINT NOT NULL,
	monthID			SMALLINT NOT NULL,
	zoneID			INTEGER NOT NULL,
	hourID			SMALLINT NOT NULL,
	ACActivityFraction	FLOAT NULL,
	INDEX XPKACActivityFraction (sourceTypeID, modelYearID, monthID, zoneID, hourID)
);

TRUNCATE ACActivityFraction;
INSERT INTO ACActivityFraction (
	sourceTypeID,
	modelYearID,
	monthID,
	zoneID,
	hourID,
	ACActivityFraction)
SELECT
	stmy.sourceTypeID,
	stmy.modelYearID,
	af.monthID,
	af.zoneID,
	af.hourID,
	ACOnFraction * ACPenetrationFraction * FunctioningACFraction
FROM
	ACOnFraction af,
	SourceTypeModelYear stmy,
	SourceTypeAge sta
WHERE
	sta.sourceTypeID = stmy.sourceTypeID AND
	sta.ageID = ##context.year## - stmy.modelYearID AND
	stmy.modelYearID <= ##context.year## AND
	stmy.modelYearID >= ##context.year## - 30;
	

ANALYZE TABLE ACActivityFraction;

-- ACAdjustment
DROP TABLE IF EXISTS ACAdjustment;
CREATE TABLE ACAdjustment (
	sourceTypeID	SMALLINT NOT NULL,
	modelYearID		SMALLINT NOT NULL,
	polProcessID	int NOT NULL,
	opModeID		SMALLINT NOT NULL,
	monthID			SMALLINT NOT NULL,
	zoneID			INTEGER NOT NULL,
	hourID			SMALLINT NOT NULL,
	ACAdjustment	FLOAT NULL
);

-- Insert default FullACAdjustment records in case some were omitted on the Master
-- @algorithm Add default fullACAdjustments of 1.0 for any missing value.  fullACAdjustment(sourceTypeID,polProcessID,opModeID)=1.
-- @condition Only for missing fullACAdjustment entries.
INSERT IGNORE INTO FullACAdjustment (
	sourceTypeID,polProcessID,opModeID,fullACAdjustment,fullACAdjustmentCV)
SELECT
	omd.sourceTypeID,
	omd.polProcessID,
	omd.opModeID,
	1,
	null
FROM
	OpModeDistribution omd;

ANALYZE TABLE FullACAdjustment;

-- @algorithm ACAdjustment(sourceTypeID,modelYearID,polProcessID,opModeID,monthID,zoneID,hourID)=1+((FullACAdjustment-1)*ACActivityFraction)
TRUNCATE ACAdjustment;
INSERT INTO ACAdjustment (
	sourceTypeID,
	modelYearID,
	polProcessID,
	opModeID,
	monthID,
	zoneID,
	hourID,
	ACAdjustment)
SELECT
	aaf.sourceTypeID,
	aaf.modelYearID,
	faa.polProcessID,
	faa.opModeID,
	aaf.monthID,
	aaf.zoneID,
	aaf.hourID,
	1+((FullACAdjustment-1)*ACActivityFraction)
FROM 
	FullACAdjustment faa,
	ACActivityFraction aaf
WHERE
	faa.sourceTypeID = aaf.sourceTypeID
	AND aaf.modelYearID <= ##context.year##;

create index XPKACAdjustment on ACAdjustment (
	sourceTypeID asc, 
	modelYearID asc, 
	polProcessID asc, 
	opModeID asc, 
	hourID asc
);	

--
-- ECCP-2b: Calculate temperature adjustment
--

CREATE TABLE IF NOT EXISTS TempAdjustmentByType (
	polProcessID		int NOT NULL,
	fuelTypeID			SMALLINT NOT NULL,
	modelYearID			SMALLINT NOT NULL,
	monthID				SMALLINT NOT NULL,
	zoneID				INTEGER NOT NULL,
	hourID				SMALLINT NOT NULL,
	tempAdjustment 		FLOAT NULL,
	tempAdjustmentV		FLOAT NULL
);

-- @algorithm tempAdjustment(polProcessID,fuelTypeID,monthID,zoneID,hourID) = 1 + (temperature - 75) * (tempAdjustTermA + (temperature - 75) * tempAdjustTermB)
TRUNCATE TempAdjustmentByType;
INSERT INTO TempAdjustmentByType (
	polProcessID,
	fuelTypeID,
	modelYearID,
	monthID,
	zoneID,
	hourID,
	tempAdjustment,
	tempAdjustmentV )
SELECT
	ta.polProcessID,
	ta.fuelTypeID,
	my.modelYearID,
	zmh.monthID,
	zmh.zoneID,
	zmh.hourID,
	1 + (zmh.temperature - 75) * (ta.tempAdjustTermA + (zmh.temperature - 75) * ta.tempAdjustTermB),
	null
FROM
	TemperatureAdjustment ta,
	ZoneMonthHour zmh,
	ModelYear my
WHERE 
	my.modelyearid between ta.minModelYearID and ta.maxModelYearID;

create index XPKTempAdjustmentByType on TempAdjustmentByType (
	fuelTypeID asc,
	modelYearID asc,
	hourID asc,
	monthID asc,
	polProcessID asc,
	zoneID asc
);	

--
-- ECCP-2c: Calculate fuel adjustment
--	

--
-- ECCP-3a:  Aggregate Base Emission Rates to Source Type/Fuel Type/Model Year/Operating Mode level
--

CREATE TABLE IF NOT EXISTS MeanBaseRateByType (
	sourceTypeID		SMALLINT NOT NULL,
	polProcessID		int NOT NULL,
	modelYearID			SMALLINT NOT NULL,
	fuelTypeID			SMALLINT NOT NULL,
	opModeID			SMALLINT NOT NULL,
	MeanBaseRateByType	FLOAT NULL
);

TRUNCATE TABLE MeanBaseRateByType;

loop ##loop.sourceTypeID##;
select sourceTypeID from RunSpecSourceType;

-- @algorithm MeanBaseRateByType(sourceTypeID,polProcessID,modelYearID,fuelTypeID,opModeID)=sum(sourceBinActivityFraction * meanBaseRate)
INSERT INTO MeanBaseRateByType (
	sourceTypeID,
	polProcessID,
	modelYearID,
	fuelTypeID,
	opModeID,
	MeanBaseRateByType)
SELECT
	stmy.sourceTypeID,
	er.polProcessID,
	stmy.modelYearID,
	sb.fuelTypeID,
	er.opModeID,
	SUM(sbd.SourceBinActivityFraction * er.MeanBaseRate)
FROM
	EmissionRate er,
	PollutantProcessModelYear ppmy,
	SourceBin sb,
	SourceBinDistribution sbd,
	SourceTypeModelYear stmy
WHERE
	ppmy.modelYearGroupID = sb.modelYearGroupID AND
	ppmy.modelYearID = stmy.modelYearID AND
	er.polProcessID = ppmy.polProcessID AND
	er.polProcessID = sbd.polProcessID AND
	ppmy.polProcessID = sbd.polProcessID AND
	er.sourceBinID = sb.sourceBinID AND
	er.sourceBinID = sbd.sourceBinID AND
	sb.sourceBinID = sbd.sourceBinID AND
	sbd.sourceTypeModelYearID = stmy.sourceTypeModelYearID AND
	stmy.modelYearID <= ##context.year## AND
	stmy.sourceTypeID = ##loop.sourceTypeID##
GROUP BY
	er.polProcessID,
	stmy.modelYearID,
	sb.fuelTypeID,
	er.opModeID;

--GROUP BY
--	stmy.sourceTypeID,

end loop ##loop.sourceTypeID##;

create index XPKMeanBaseRateByType on MeanBaseRateByType (
	sourceTypeID asc, 
	polProcessID asc, 
	opModeID asc
);


--
-- ECCP-3b:  Aggregate emission rates to SourceType level, Apply A/C adjustment
--

CREATE TABLE IF NOT EXISTS SourceTypeEnergy (
	sourceTypeID		SMALLINT NOT NULL,
	polProcessID		int NOT NULL,
	zoneID				INTEGER NOT NULL,
	linkID				INTEGER NOT NULL,
	modelYearID			SMALLINT NOT NULL,
	monthID				SMALLINT NOT NULL,
	hourDayID			SMALLINT NOT NULL,
	fuelTypeID			SMALLINT NOT NULL,
	sourceTypeEnergy	FLOAT NULL
);

TRUNCATE TABLE SourceTypeEnergy;

-- This SQL Statement Rewritten As Series of Simpler Statements to Improve Performance
-- INSERT INTO SourceTypeEnergy (
-- 	sourceTypeID,
-- 	polProcessID,
-- 	zoneID,
-- 	linkID,
-- 	modelYearID,
-- 	monthID,
-- 	hourDayID,
-- 	fuelTypeID,
-- 	sourceTypeEnergy)
-- SELECT STRAIGHT_JOIN
-- 	aca.sourceTypeID,
-- 	aca.polProcessID,
-- 	omd.linkID,
-- 	aca.modelYearID,
-- 	aca.monthID,
-- 	hd.hourDayID,
-- 	mbrt.fuelTypeID,
-- 	SUM(omd.opModeFraction * mbrt.MeanBaseRateByType * aca.ACAdjustment)
-- FROM
-- 	HourDay hd,
-- 	ACAdjustment aca,
-- 	MeanBaseRateByType mbrt,
-- 	OpModeDistribution omd
-- WHERE
-- 	hd.hourDayID = omd.hourDayID AND
-- 	aca.hourID = hd.hourID AND
-- 	aca.modelYearID = mbrt.modelYearID AND
-- 	aca.opModeID = mbrt.opModeID AND
-- 	aca.opModeID = omd.opModeID AND
-- 	mbrt.opModeID = omd.opModeID AND
-- 	aca.polProcessID = mbrt.polProcessID AND
-- 	aca.polProcessID = omd.polProcessID AND
-- 	mbrt.polProcessID = omd.polProcessID AND
-- 	aca.sourceTypeID = mbrt.sourceTypeID AND
-- 	aca.sourceTypeID = omd.sourceTypeID AND
-- 	mbrt.sourceTypeID = omd.sourceTypeID
-- GROUP BY
-- 	hd.hourDayID,
-- 	aca.monthID,
-- 	aca.zoneID,
-- 	aca.polProcessID,
-- 	aca.sourceTypeID,
-- 	aca.modelYearID,
-- 	mbrt.fuelTypeID,
-- 	omd.linkID

loop ##loop.sourceTypeID##;
select sourceTypeID from RunSpecSourceType;

DROP TABLE IF EXISTS OMD2;
DROP TABLE IF EXISTS OMDMBR;

-- @algorithm Add hourID to opModeDistribution.
CREATE TABLE OMD2
  SELECT omd.*, hd.hourID
  FROM OpModeDistribution AS omd INNER JOIN HourDay AS hd USING (hourDayID)
  WHERE omd.sourceTypeID=##loop.sourceTypeID##;
CREATE INDEX index1 ON OMD2 
  (polProcessID, opModeID);

-- @algorithm Combine meanBaseRateByType keys with opModeFraction keys.
-- @condition all processes except Auxiliary Power Exhaust
CREATE TABLE OMDMBR
  SELECT mbrt.sourceTypeID, mbrt.modelYearID, mbrt.polProcessID, mbrt.opModeID,
         mbrt.fuelTypeID, omd.hourDayID, omd.hourID, omd.linkID,
         omd.opModeFraction, mbrt.meanBaseRateByType
  FROM OMD2 AS omd INNER JOIN MeanBaseRateByType AS mbrt
         USING (polProcessID, opModeID)
  WHERE mbrt.sourceTypeID=##loop.sourceTypeID##;

CREATE INDEX index1 ON OMDMBR (modelYearID, polProcessID, opModeID, hourID);

-- Section Auxiliary Power Exhaust
CREATE UNIQUE INDEX index2 ON OMDMBR (sourceTypeID, modelYearID, polProcessID, opModeID, fuelTypeID, hourDayID, hourID, linkID);

-- @algorithm Combine meanBaseRateByType keys with opModeFraction keys.
-- @condition for Auxiliary Power Exhaust process
insert ignore into OMDMBR (sourceTypeID, modelYearID, polProcessID, opModeID,
         fuelTypeID, hourDayID, hourID, linkID,
         opModeFraction, meanBaseRateByType)
select mbrt.sourceTypeID, mbrt.modelYearID, mbrt.polProcessID, mbrt.opModeID,
         mbrt.fuelTypeID, rshd.hourDayID, hd.hourID, 
         ##context.iterLocation.linkRecordID## as linkID,
         hac.opModeFraction, mbrt.meanBaseRateByType
from hotellingActivityDistribution hac
	inner join MeanBaseRateByType mbrt on (
		mbrt.opModeID = hac.opModeID
		and hac.beginModelYearID <= mbrt.modelYearID
		and hac.endModelYearID >= mbrt.modelYearID),
	RunSpecHourDay rshd inner join HourDay hd using (hourDayID)
where mbrt.sourceTypeID=##loop.sourceTypeID##
and 62=##loop.sourceTypeID##;
-- End Section Auxiliary Power Exhaust

-- @algorithm sourceTypeEnergy(sourceTypeID,modelYearID,polProcessID,monthID,zoneID,fuelTypeID,linkID,hourDayID) = sum(opModeFraction * meanBaseRateByType * ACAdjustment)
INSERT INTO SourceTypeEnergy (
	sourceTypeID,
	modelYearID,
	polProcessID,
	monthID,
	zoneID,
	fuelTypeID,
	linkID,
	hourDayID,
	sourceTypeEnergy)
SELECT 
	aca.sourceTypeID,
	aca.modelYearID,
	aca.polProcessID,
	aca.monthID,
	aca.zoneID,
	omdmbr.fuelTypeID,
	omdmbr.linkID,
	omdmbr.hourDayID,
	SUM(omdmbr.opModeFraction * omdmbr.MeanBaseRateByType * aca.ACAdjustment)
FROM
	OMDMBR AS omdmbr INNER JOIN ACAdjustment AS aca
	USING (modelYearID, polProcessID, opModeID, hourID)
WHERE
	aca.sourceTypeID = ##loop.sourceTypeID##
GROUP BY
	omdmbr.modelYearID,
	omdmbr.polProcessID,
	omdmbr.hourDayID,
	omdmbr.fuelTypeID,
	omdmbr.linkID,
	aca.monthID,
	aca.zoneID 
 ORDER BY NULL;

end loop ##loop.sourceTypeID##;

create index XPKSourceTypeEnergy on SourceTypeEnergy (
	sourceTypeID asc,
	polProcessID asc,
	zoneID asc,
	linkID asc,
	modelYearID asc,
	monthID asc,
	hourDayID asc,
	fuelTypeID asc
);

create index XPKSourceTypeEnergy2 on SourceTypeEnergy (
	fuelTypeID asc,
	hourDayID asc,
	linkID asc,
	modelYearID asc,
	monthID asc,
	polProcessID asc,
	zoneID asc,
	sourceTypeID asc
);


--
-- End of Rewritten Statement
--
ANALYZE TABLE SourceTypeEnergy;

CREATE TABLE IF NOT EXISTS MOVESWorkerOutputTemp (
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
	fuelTypeID           SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,
	roadTypeID           SMALLINT UNSIGNED NULL,
	SCC                  CHAR(10) NULL,
	emissionQuant        FLOAT NULL
);

-- Section Running Exhaust
--
-- ECCP-3c:  Calculate running energy consumption
--

TRUNCATE MOVESWorkerOutputTemp;

--
-- Calculate the Total Energy
--

loop ##loop.sourceTypeID##;
select sourceTypeID from RunSpecSourceType;

-- @algorithm totalEnergy(yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID) = SUM(SHO * sourceTypeEnergy * TempAdjustment)
-- @condition for sourceType output
INSERT INTO MOVESWorkerOutputTemp (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    emissionQuant)
SELECT
	sho.yearID,
	ste.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	ste.zoneID,
	ste.linkID,
	ppa.pollutantID,
	ppa.processID,
	ste.sourceTypeID,
	ste.fuelTypeID,
	ste.modelYearID,
	l.roadTypeID,
	SUM(sho.sho * ste.sourceTypeEnergy * tat.TempAdjustment )
FROM
	County c,
	EmissionProcess ep,
	HourDay hd,
	Link l,
	MonthOfAnyYear may,
	PollutantProcessAssoc ppa,
	SHO sho,
	SourceTypeEnergy ste,
	SourceTypeModelYear stmy,
	TempAdjustmentByType tat
WHERE
	c.countyID = l.countyID AND
	ste.fuelTypeID = tat.fuelTypeID AND
	hd.hourDayID = sho.hourDayID AND
	hd.hourDayID = ste.hourDayID AND
	sho.hourDayID = ste.hourDayID AND
	hd.hourID = tat.hourID AND
	l.linkID = sho.linkID AND
	l.linkID = ste.linkID AND
	sho.linkID = ste.linkID AND
	(sho.yearID - sho.ageID) = ste.modelYearID AND
	(sho.yearID - sho.ageID) = stmy.modelYearID AND
	ste.modelYearID = stmy.modelYearID AND
	ste.modelYearID = tat.modelYearID AND
	may.monthID = sho.monthID AND
	may.monthID = ste.monthID AND
	may.monthID = tat.monthID AND
	sho.monthID = ste.monthID AND
	sho.monthID = tat.monthID AND
	ste.monthID = tat.monthID AND
	ppa.polProcessID = ste.polProcessID AND
	ppa.polProcessID = tat.polProcessID AND
	ste.polProcessID = tat.polProcessID AND
	ep.processID = ppa.processID AND
	l.zoneID = ste.zoneID AND
	l.zoneID = tat.zoneID AND
	ste.zoneID = tat.zoneID AND
	sho.sourceTypeID = ##loop.sourceTypeID## AND
	stmy.sourceTypeID = ##loop.sourceTypeID## AND
	ste.sourceTypeID = ##loop.sourceTypeID##
GROUP BY
	sho.yearID,
	ste.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	ste.zoneID,
	ste.linkID,
	ppa.pollutantID,
	ppa.processID,
	ste.fuelTypeID,
	ste.modelYearID,
	l.roadTypeID;

--	sho.sourceTypeID = ste.sourceTypeID AND
--	sho.sourceTypeID = stmy.sourceTypeID AND
--	ste.sourceTypeID = stmy.sourceTypeID AND

--	ppa.processID,
--	ste.sourceTypeID,
--	ste.fuelTypeID,

end loop ##loop.sourceTypeID##;

ANALYZE TABLE MOVESWorkerOutputTemp;

-- Section Total Energy
--
-- Copy temporary results for Total Energy to final table. We are done for total energy.
--

-- @algorithm emissionQuant[totalEnergy](yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC) = totalEnergy
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant
FROM 
    MOVESWorkerOutputTemp;

-- End Section Total Energy

-- Section Petroleum Energy
--
-- Calculate Petroleum Energy
--

-- @algorithm emissionQuant[petroleumEnergy](yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC) = totalEnergy * petroleumFraction
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    mwot.yearID,
    mwot.monthID,
    mwot.dayID,
    mwot.hourID,
    mwot.stateID,
    mwot.countyID,
    mwot.zoneID,
    mwot.linkID,
    92,
    mwot.processID,
    mwot.sourceTypeID,
    mwot.fuelTypeID,
    mwot.modelYearID,
    mwot.roadTypeID,
    mwot.SCC,
    mwot.emissionQuant * pf.petroleumFraction
FROM 
    MOVESWorkerOutputTemp mwot,
    PetroleumFraction pf,
    MonthOfAnyYear may
WHERE
    may.monthID = mwot.monthID AND
    pf.countyID = mwot.countyID AND
    pf.yearID = mwot.yearID AND 
    pf.monthGroupID = may.monthGroupID AND
    pf.fuelTypeID = mwot.fuelTypeID AND
    mwot.processID = 1;

-- End Section Petroleum Energy

-- Section Fossil Fuel Energy
--
-- Calculate Fossil Energy
--

-- @algorithm emissionQuant[fossilEnergy](yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC) = totalEnergy * fossilFraction
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    mwot.yearID,
    mwot.monthID,
    mwot.dayID,
    mwot.hourID,
    mwot.stateID,
    mwot.countyID,
    mwot.zoneID,
    mwot.linkID,
    93,
    mwot.processID,
    mwot.sourceTypeID,
    mwot.fuelTypeID,
    mwot.modelYearID,
    mwot.roadTypeID,
    mwot.SCC,
    mwot.emissionQuant * ff.fossilFraction
FROM 
    MOVESWorkerOutputTemp mwot,
    FossilFraction ff,
    MonthOfAnyYear may
WHERE
    may.monthID = mwot.monthID AND
    ff.countyID = mwot.countyID AND
    ff.yearID = mwot.yearID AND 
    ff.monthGroupID = may.monthGroupID AND
    ff.fuelTypeID = mwot.fuelTypeID AND
    mwot.processID = 1;
-- End Section Fossil Fuel Energy
-- End Section Running Exhaust

ANALYZE TABLE MOVESWorkerOutput;

-- Section Start Exhaust
--
-- ECCP-4:  Calculate start energy consumption
--

TRUNCATE MOVESWorkerOutputTemp;

loop ##loop.sourceTypeID##;
select sourceTypeID from RunSpecSourceType;

-- @algorithm totalEnergy(yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID) = SUM(starts * sourceTypeEnergy * TempAdjustment)
-- @condition for sourceType output
-- @condition for Starts process
INSERT INTO MOVESWorkerOutputTemp (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    emissionQuant)
SELECT
	s.yearID,
	ste.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	ste.zoneID,
	ste.linkID,
	ppa.pollutantID,
	ppa.processID,
	ste.sourceTypeID,
	ste.fuelTypeID,
	ste.modelYearID,
	l.roadTypeID,
	SUM(s.starts * ste.sourceTypeEnergy * tat.TempAdjustment)
FROM
	County c,
	EmissionProcess ep,
	HourDay hd,
	Link l,
	MonthOfAnyYear may,
	PollutantProcessAssoc ppa,
	Starts s,
	SourceTypeEnergy ste,
	SourceTypeModelYear stmy,
	TempAdjustmentByType tat
WHERE
	c.countyID = l.countyID AND
	ste.fuelTypeID = tat.fuelTypeID AND
	hd.hourDayID = ste.hourDayID AND
	hd.hourDayID = s.hourDayID AND
	ste.hourDayID = s.hourDayID AND
	hd.hourID = tat.hourID AND
	l.linkID = ste.linkID AND
	ste.modelYearID = stmy.modelYearID AND
	ste.modelYearID = (s.yearID - s.ageID) AND
	stmy.modelYearID = (s.yearID - s.ageID) AND
	ste.modelYearID = tat.modelYearID AND
	may.monthID = ste.monthID AND
	may.monthID = s.monthID AND
	may.monthID = tat.monthID AND
	ste.monthID = s.monthID AND
	ste.monthID = tat.monthID AND
	s.monthID = tat.monthID AND
	ppa.polProcessID = ste.polProcessID AND
	ppa.polProcessID = tat.polProcessID AND
	ste.polProcessID = tat.polProcessID AND
	ep.processID = ppa.processID AND
	ste.sourceTypeID = ##loop.sourceTypeID## AND
	stmy.sourceTypeID = ##loop.sourceTypeID## AND
	s.sourceTypeID = ##loop.sourceTypeID## AND
	l.zoneID = ste.zoneID AND
	l.zoneID = s.zoneID AND
	l.zoneID = tat.zoneID AND
	ste.zoneID = s.zoneID AND
	ste.zoneID = tat.zoneID AND
	s.zoneID = tat.zoneID
GROUP BY
	s.yearID,
	ste.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	ste.zoneID,
	ste.linkID,
	ppa.pollutantID,
	ppa.processID,
	ste.fuelTypeID,
	ste.modelYearID,
	l.roadTypeID;

--	ste.sourceTypeID = stmy.sourceTypeID AND
--	ste.sourceTypeID = s.sourceTypeID AND
--	stmy.sourceTypeID = s.sourceTypeID AND

--	ppa.processID,
--	ste.sourceTypeID,
--	ste.fuelTypeID,

end loop ##loop.sourceTypeID##;

-- Section Total Energy
--
-- Copy temporary results for Total Energy to final table. We are done for total energy.
--

-- @algorithm emissionQuant[totalEnergy](yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC) = totalEnergy
-- @condition for Starts process
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant
FROM 
    MOVESWorkerOutputTemp;
-- End Section Total Energy

-- Section Petroleum Energy
--
-- Calculate Petroleum Energy
--

-- @algorithm emissionQuant[petroleumEnergy](yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC) = totalEnergy * petroleumFraction
-- @condition for Starts process
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    mwot.yearID,
    mwot.monthID,
    mwot.dayID,
    mwot.hourID,
    mwot.stateID,
    mwot.countyID,
    mwot.zoneID,
    mwot.linkID,
    92,
    mwot.processID,
    mwot.sourceTypeID,
    mwot.fuelTypeID,
    mwot.modelYearID,
    mwot.roadTypeID,
    mwot.SCC,
    mwot.emissionQuant * pf.petroleumFraction
FROM 
    MOVESWorkerOutputTemp mwot,
    PetroleumFraction pf,
    MonthOfAnyYear may
WHERE
    may.monthID = mwot.monthID AND
    pf.countyID = mwot.countyID AND
    pf.yearID = mwot.yearID AND 
    pf.monthGroupID = may.monthGroupID AND
    pf.fuelTypeID = mwot.fuelTypeID AND
    mwot.processID = 2;
-- End Section Petroleum Energy

-- Section Fossil Fuel Energy
--
-- Calculate Fossil Energy
--

-- @algorithm emissionQuant[fossilEnergy](yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC) = totalEnergy * fossilFraction
-- @condition for Starts process
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    mwot.yearID,
    mwot.monthID,
    mwot.dayID,
    mwot.hourID,
    mwot.stateID,
    mwot.countyID,
    mwot.zoneID,
    mwot.linkID,
    93,
    mwot.processID,
    mwot.sourceTypeID,
    mwot.fuelTypeID,
    mwot.modelYearID,
    mwot.roadTypeID,
    mwot.SCC,
    mwot.emissionQuant * ff.fossilFraction
FROM 
    MOVESWorkerOutputTemp mwot,
    FossilFraction ff,
    MonthOfAnyYear may
WHERE
    may.monthID = mwot.monthID AND
    ff.countyID = mwot.countyID AND
    ff.yearID = mwot.yearID AND 
    ff.monthGroupID = may.monthGroupID AND
    ff.fuelTypeID = mwot.fuelTypeID AND
    mwot.processID = 2;
-- End Section Fossil Fuel Energy
-- End Section Start Exhaust

-- Section Extended Idle Exhaust
--
-- ECCP-5:  Calculate extended idle energy consumption
--

TRUNCATE MOVESWorkerOutputTemp;

loop ##loop.sourceTypeID##;
select sourceTypeID from RunSpecSourceType;

INSERT INTO MOVESWorkerOutputTemp (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    emissionQuant)
SELECT
	eih.yearID,
	ste.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	ste.zoneID,
	ste.linkID,
	ppa.pollutantID,
	ppa.processID,
	ste.sourceTypeID,
	ste.fuelTypeID,
	ste.modelYearID,
	l.roadTypeID,
	SUM(eih.extendedIdleHours * ste.sourceTypeEnergy * tat.TempAdjustment)
FROM
	County c,
	EmissionProcess ep,
	ExtendedIdleHours eih,
	HourDay hd,
	Link l,
	MonthOfAnyYear may,
	PollutantProcessAssoc ppa,
	SourceTypeEnergy ste,
	SourceTypeModelYear stmy,
	TempAdjustmentByType tat
WHERE
	c.countyID = l.countyID AND
	ste.fuelTypeID = tat.fuelTypeID AND
	eih.hourDayID = hd.hourDayID AND
	eih.hourDayID = ste.hourDayID AND
	hd.hourDayID = ste.hourDayID AND
	hd.hourID = tat.hourID AND
	l.linkID = ste.linkID AND
	(eih.yearID - eih.ageID) = ste.modelYearID AND
	(eih.yearID - eih.ageID) = stmy.modelYearID AND
	ste.modelYearID = stmy.modelYearID AND
	ste.modelYearID = tat.modelYearID AND
	eih.monthID = may.monthID AND
	eih.monthID = ste.monthID AND
	eih.monthID = tat.monthID AND
	may.monthID = ste.monthID AND
	may.monthID = tat.monthID AND
	ste.monthID = tat.monthID AND
	ppa.polProcessID = ste.polProcessID AND
	ppa.polProcessID = tat.polProcessID AND
	ste.polProcessID = tat.polProcessID AND
	ep.processID = ppa.processID AND
	eih.sourceTypeID = ##loop.sourceTypeID## AND
	ste.sourceTypeID = ##loop.sourceTypeID## AND
	stmy.sourceTypeID = ##loop.sourceTypeID## AND
	eih.zoneID = l.zoneID AND
	eih.zoneID = ste.zoneID AND
	eih.zoneID = tat.zoneID AND
	l.zoneID = ste.zoneID AND
	l.zoneID = tat.zoneID AND
	ste.zoneID = tat.zoneID
GROUP BY
	eih.yearID,
	ste.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	ste.zoneID,
	ste.linkID,
	ppa.pollutantID,
	ppa.processID,
	ste.fuelTypeID,
	ste.modelYearID,
	l.roadTypeID;

--	eih.sourceTypeID = ste.sourceTypeID AND
--	eih.sourceTypeID = stmy.sourceTypeID AND
--	ste.sourceTypeID = stmy.sourceTypeID AND

--	ppa.processID,
--	ste.sourceTypeID,
--	ste.fuelTypeID,

end loop ##loop.sourceTypeID##;

-- Section Total Energy
--
-- Copy temporary results for Total Energy to final table. We are done for total energy.
--

INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant
FROM 
    MOVESWorkerOutputTemp;
-- End Section Total Energy

-- Section Petroleum Energy
--
-- Calculate Petroleum Energy
--
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    mwot.yearID,
    mwot.monthID,
    mwot.dayID,
    mwot.hourID,
    mwot.stateID,
    mwot.countyID,
    mwot.zoneID,
    mwot.linkID,
    92,
    mwot.processID,
    mwot.sourceTypeID,
    mwot.fuelTypeID,
    mwot.modelYearID,
    mwot.roadTypeID,
    mwot.SCC,
    mwot.emissionQuant * pf.petroleumFraction
FROM 
	MOVESWorkerOutputTemp mwot,
	PetroleumFraction pf,
	MonthOfAnyYear may
WHERE
	may.monthID = mwot.monthID AND
	pf.countyID = mwot.countyID AND
	pf.yearID = mwot.yearID AND 
	pf.monthGroupID = may.monthGroupID AND
	pf.fuelTypeID = mwot.fuelTypeID AND
	mwot.processID = 90;
-- End Section Petroleum Energy

-- Section Fossil Fuel Energy
--
-- Calculate Fossil Energy
--
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    mwot.yearID,
    mwot.monthID,
    mwot.dayID,
    mwot.hourID,
    mwot.stateID,
    mwot.countyID,
    mwot.zoneID,
    mwot.linkID,
    93,
    mwot.processID,
    mwot.sourceTypeID,
    mwot.fuelTypeID,
    mwot.modelYearID,
    mwot.roadTypeID,
    mwot.SCC,
    mwot.emissionQuant * ff.fossilFraction
FROM 
    MOVESWorkerOutputTemp mwot,
    FossilFraction ff,
    MonthOfAnyYear may
WHERE
    may.monthID = mwot.monthID AND
    ff.countyID = mwot.countyID AND
    ff.yearID = mwot.yearID AND 
    ff.monthGroupID = may.monthGroupID AND
    ff.fuelTypeID = mwot.fuelTypeID AND
    mwot.processID = 90;
-- End Section Fossil Fuel Energy
-- End Section Extended Idle Exhaust

-- Section Auxiliary Power Exhaust
--
-- ECCP-5:  Calculate extended idle energy consumption
--

TRUNCATE MOVESWorkerOutputTemp;

loop ##loop.sourceTypeID##;
select sourceTypeID from RunSpecSourceType where sourceTypeID=62;

INSERT INTO MOVESWorkerOutputTemp (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    emissionQuant)
SELECT
	eih.yearID,
	ste.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	ste.zoneID,
	ste.linkID,
	ppa.pollutantID,
	ppa.processID,
	ste.sourceTypeID,
	ste.fuelTypeID,
	ste.modelYearID,
	l.roadTypeID,
	SUM(eih.hotellingHours * ste.sourceTypeEnergy * tat.TempAdjustment)
FROM
	County c,
	EmissionProcess ep,
	hotellingHours eih,
	HourDay hd,
	Link l,
	MonthOfAnyYear may,
	PollutantProcessAssoc ppa,
	SourceTypeEnergy ste,
	SourceTypeModelYear stmy,
	TempAdjustmentByType tat
WHERE
	c.countyID = l.countyID AND
	ste.fuelTypeID = tat.fuelTypeID AND
	eih.hourDayID = hd.hourDayID AND
	eih.hourDayID = ste.hourDayID AND
	hd.hourDayID = ste.hourDayID AND
	hd.hourID = tat.hourID AND
	l.linkID = ste.linkID AND
	(eih.yearID - eih.ageID) = ste.modelYearID AND
	(eih.yearID - eih.ageID) = stmy.modelYearID AND
	ste.modelYearID = stmy.modelYearID AND
	ste.modelYearID = tat.modelYearID AND 
	eih.monthID = may.monthID AND
	eih.monthID = ste.monthID AND
	eih.monthID = tat.monthID AND
	may.monthID = ste.monthID AND
	may.monthID = tat.monthID AND
	ste.monthID = tat.monthID AND
	ppa.polProcessID = ste.polProcessID AND
	ppa.polProcessID = tat.polProcessID AND
	ste.polProcessID = tat.polProcessID AND
	ep.processID = ppa.processID AND
	eih.sourceTypeID = ##loop.sourceTypeID## AND
	ste.sourceTypeID = ##loop.sourceTypeID## AND
	stmy.sourceTypeID = ##loop.sourceTypeID## AND
	eih.zoneID = l.zoneID AND
	eih.zoneID = ste.zoneID AND
	eih.zoneID = tat.zoneID AND
	l.zoneID = ste.zoneID AND
	l.zoneID = tat.zoneID AND
	ste.zoneID = tat.zoneID
GROUP BY
	eih.yearID,
	ste.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	ste.zoneID,
	ste.linkID,
	ppa.pollutantID,
	ppa.processID,
	ste.fuelTypeID,
	ste.modelYearID,
	l.roadTypeID;

--	eih.sourceTypeID = ste.sourceTypeID AND
--	eih.sourceTypeID = stmy.sourceTypeID AND
--	ste.sourceTypeID = stmy.sourceTypeID AND

--	ppa.processID,
--	ste.sourceTypeID,
--	ste.fuelTypeID,

end loop ##loop.sourceTypeID##;

-- Section Total Energy
--
-- Copy temporary results for Total Energy to final table. We are done for total energy.
--

INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant
FROM 
    MOVESWorkerOutputTemp;
-- End Section Total Energy

-- Section Petroleum Energy
--
-- Calculate Petroleum Energy
--
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    mwot.yearID,
    mwot.monthID,
    mwot.dayID,
    mwot.hourID,
    mwot.stateID,
    mwot.countyID,
    mwot.zoneID,
    mwot.linkID,
    92,
    mwot.processID,
    mwot.sourceTypeID,
    mwot.fuelTypeID,
    mwot.modelYearID,
    mwot.roadTypeID,
    mwot.SCC,
    mwot.emissionQuant * pf.petroleumFraction
FROM 
	MOVESWorkerOutputTemp mwot,
	PetroleumFraction pf,
	MonthOfAnyYear may
WHERE
	may.monthID = mwot.monthID AND
	pf.countyID = mwot.countyID AND
	pf.yearID = mwot.yearID AND 
	pf.monthGroupID = may.monthGroupID AND
	pf.fuelTypeID = mwot.fuelTypeID AND
	mwot.processID = 91;
-- End Section Petroleum Energy

-- Section Fossil Fuel Energy
--
-- Calculate Fossil Energy
--
INSERT INTO MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
SELECT
    mwot.yearID,
    mwot.monthID,
    mwot.dayID,
    mwot.hourID,
    mwot.stateID,
    mwot.countyID,
    mwot.zoneID,
    mwot.linkID,
    93,
    mwot.processID,
    mwot.sourceTypeID,
    mwot.fuelTypeID,
    mwot.modelYearID,
    mwot.roadTypeID,
    mwot.SCC,
    mwot.emissionQuant * ff.fossilFraction
FROM 
    MOVESWorkerOutputTemp mwot,
    FossilFraction ff,
    MonthOfAnyYear may
WHERE
    may.monthID = mwot.monthID AND
    ff.countyID = mwot.countyID AND
    ff.yearID = mwot.yearID AND 
    ff.monthGroupID = may.monthGroupID AND
    ff.fuelTypeID = mwot.fuelTypeID AND
    mwot.processID = 91;
-- End Section Fossil Fuel Energy
-- End Section Auxiliary Power Exhaust

ANALYZE TABLE MOVESWorkerOutput;

alter table SourceTypeEnergy drop index XPKSourceTypeEnergy;
alter table SourceTypeEnergy drop index XPKSourceTypeEnergy2;	
alter table ACAdjustment drop index XPKACAdjustment;	
alter table MeanBaseRateByType drop index XPKMeanBaseRateByType;
alter table TempAdjustmentByType drop index XPKTempAdjustmentByType;

-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS ACOnFraction;
DROP TABLE IF EXISTS ACActivityFraction;
DROP TABLE IF EXISTS ACAdjustment;
DROP TABLE IF EXISTS SourceTypeEnergy;
DROP TABLE IF EXISTS OMD2;
DROP TABLE IF EXISTS OMDMBR;
DROP TABLE IF EXISTS MeanBaseRateByType;
DROP TABLE IF EXISTS FuelAdjustmentByType;
DROP TABLE IF EXISTS TempAdjustmentByType;
DROP TABLE IF EXISTS MOVESWorkerOutputTemp;
-- Section Fossil Fuel Energy
DROP TABLE IF EXISTS FossilFraction;
-- End Section Fossil Fuel Energy
-- Section Petroleum Energy
DROP TABLE IF EXISTS PetroleumFraction;
-- End Section Petroleum Energy
-- Section Running Exhaust
-- End Section Running Exhaust
-- Section Start Exhaust
-- End Section Start Exhaust
-- Section Extended Idle Exhaust
-- End Section Extended Idle Exhaust
-- End Section Cleanup
