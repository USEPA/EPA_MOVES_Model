-- Version 2013-09-15

-- Section Create Remote Tables for Extracted Data
##create.AgeCategory##;
TRUNCATE TABLE AgeCategory;

##create.County##;
TRUNCATE TABLE County;

##create.HourDay##;
TRUNCATE TABLE HourDay;

##create.Link##;
TRUNCATE TABLE Link;

##create.Zone##;
TRUNCATE TABLE Zone;

##create.Pollutant##;
TRUNCATE TABLE Pollutant;

##create.EmissionProcess##;
TRUNCATE TABLE EmissionProcess;

##create.EmissionRateByAge##;
TRUNCATE TABLE EmissionRateByAge;

##create.OpModeDistribution##;
TRUNCATE TABLE OpModeDistribution;

##create.SourceBin##;
TRUNCATE TABLE SourceBin;

##create.SourceBinDistribution##;
TRUNCATE TABLE SourceBinDistribution;

##create.SourceTypeModelYear##;
TRUNCATE TABLE SourceTypeModelYear;

##create.PollutantProcessAssoc##;
TRUNCATE TABLE PollutantProcessAssoc;

-- Section Running Exhaust

##create.SHO##;
TRUNCATE TABLE SHO;

-- End Section Running Exhaust

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
cache SELECT * INTO OUTFILE '##AgeCategory##'
FROM AgeCategory;

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

cache SELECT * 
INTO OUTFILE '##EmissionProcess##'
FROM EmissionProcess
WHERE processID=##context.iterProcess.databaseKey##;

cache SELECT * 
INTO OUTFILE '##OpModeDistribution##'
FROM OpModeDistribution, RunSpecSourceType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND linkID = ##context.iterLocation.linkRecordID##
AND RunSpecSourceType.sourceTypeID = OpModeDistribution.sourceTypeID;

cache SELECT *
INTO OUTFILE '##Pollutant##'
FROM Pollutant;

cache SELECT DISTINCT SourceBinDistribution.* 
INTO OUTFILE '##SourceBinDistribution##'
FROM sourceBinDistributionFuelUsage_##context.iterProcess.databaseKey##_##context.iterLocation.countyRecordID##_##context.year## as SourceBinDistribution, 
SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT SourceBin.* 
INTO OUTFILE '##SourceBin##'
FROM SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE polProcessID IN (##pollutantProcessIDs##)
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT DISTINCT EmissionRateByAge.* 
INTO OUTFILE '##EmissionRateByAge##'
FROM EmissionRateByAge, SourceBinDistribution, SourceTypeModelYear, SourceBin, RunSpecSourceFuelType
WHERE EmissionRateByAge.polProcessID IN (##pollutantProcessIDs##)
AND EmissionRateByAge.polProcessID = SourceBinDistribution.polProcessID
AND EmissionRateByAge.SourceBinID = SourceBinDistribution.SourceBinID
AND SourceBinDistribution.sourceTypeModelYearID = SourceTypeModelYear.sourceTypeModelYearID
AND SourceTypeModelYear.modelYearID <= ##context.year##
AND SourceTypeModelYear.sourceTypeID = RunSpecSourceFuelType.sourceTypeID
AND SourceBinDistribution.SourceBinID = SourceBin.SourceBinID
AND SourceBin.fuelTypeID = RunSpecSourceFuelType.fuelTypeID;

cache SELECT SourceTypeModelYear.* 
INTO OUTFILE '##SourceTypeModelYear##'
FROM SourceTypeModelYear,RunSpecSourceType
WHERE SourceTypeModelYear.sourceTypeID = RunSpecSourceType.sourceTypeID
AND modelYearID <= ##context.year##;

cache SELECT DISTINCT HourDay.* 
INTO OUTFILE '##HourDay##'
FROM HourDay,RunSpecHour,RunSpecDay
WHERE HourDay.dayID = RunSpecDay.dayID
AND HourDay.hourID = RunSpecHour.hourID;

cache SELECT * 
INTO OUTFILE '##PollutantProcessAssoc##'
FROM PollutantProcessAssoc
WHERE processID=##context.iterProcess.databaseKey##;

-- Section Running Exhaust

SELECT SHO.* 
INTO OUTFILE '##SHO##'
FROM SHO
WHERE yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##;

-- End Section Running Exhaust

-- End Section Extract Data
--
-- Section Processing
--
-- Section Running Exhaust

--
-- Calculate the Running Emissions
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
    emissionQuant)
SELECT
	sho.yearID,
	sho.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	z.zoneID,
	sho.linkID,
	ppa.pollutantID,
	ppa.processID,
	sho.sourceTypeID,
	sb.fuelTypeID,
	stmy.modelYearID,
	l.roadTypeID,
	sum(sbd.sourceBinActivityFraction * sho.sho * era.meanBaseRate * omd.opModeFraction)
FROM
	SHO sho,
	AgeCategory ac,
	SourceBinDistribution sbd,
	emissionRateByAge era,
	county c,
	zone z, 
	link l,
	PollutantProcessAssoc ppa,
	EmissionProcess ep,
	hourDay hd,
	sourceTypeModelYear stmy,
	sourceBin sb,
	OpModeDistribution omd
WHERE
	sho.ageID = ac.ageID AND
	ac.ageGroupID = era.ageGroupID AND
	c.countyID = l.countyID AND
	c.countyID = z.countyID AND
	l.countyID = z.countyID AND
	hd.hourDayID = omd.hourDayID AND
	hd.hourDayID = sho.hourDayID AND
	omd.hourDayID = sho.hourDayID AND
	omd.isUserInput = sho.isUserInput AND
	omd.isUserInput = sbd.isUserInput AND
	sho.isUserInput = sbd.isUserInput AND
	l.linkID = omd.linkID AND
	l.linkID = sho.linkID AND
	omd.linkID = sho.linkID AND
	(sho.yearID - sho.ageID) = stmy.modelYearID AND
	era.opModeID = omd.opModeID AND
	era.polProcessID = omd.polProcessID AND
	era.polProcessID = ppa.polProcessID AND
	era.polProcessID = sbd.polProcessID AND
	omd.polProcessID = ppa.polProcessID AND
	omd.polProcessID = sbd.polProcessID AND
	ppa.polProcessID = sbd.polProcessID AND
	ep.processID = ppa.processID AND
	era.sourceBinID = sb.sourceBinID AND
	era.sourceBinID = sbd.sourceBinID AND
	sb.sourceBinID = sbd.sourceBinID AND
	omd.sourceTypeID = sho.sourceTypeID AND
	omd.sourceTypeID = stmy.sourceTypeID AND
	sho.sourceTypeID = stmy.sourceTypeID AND
	sbd.sourceTypeModelYearID = stmy.sourceTypeModelYearID AND
	l.zoneID = z.zoneID AND
	ppa.pollutantID IN (##pollutantIDs##)
GROUP BY
	sho.yearID,
	sho.monthID,
	hd.dayID,
	hd.hourID,
	c.stateID,
	c.countyID,
	z.zoneID,
	sho.linkID,
	ppa.pollutantID,
	ppa.processID,
	sho.sourceTypeID,
	sb.fuelTypeID,
	stmy.modelYearID,
	l.roadTypeID;

-- End Section Running Exhaust

-- End Section Processing
