-- Author Wesley Faler
-- Version 2013-09-23

-- @algorithm
-- @owner PM10 Brake Tire Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data
##create.PM10EmissionRatio##;
TRUNCATE TABLE PM10EmissionRatio;

CREATE TABLE IF NOT EXISTS PM10PollutantProcessAssoc (
       polProcessID         int NOT NULL,
       processID            SMALLINT NOT NULL,
       pollutantID          SMALLINT NOT NULL,
	   isAffectedByExhaustIM CHAR(1) NOT NULL DEFAULT 'N',
       isAffectedByEvapIM CHAR(1) NOT NULL DEFAULT 'N',
       PRIMARY KEY (polProcessID, processID, pollutantID),
       KEY (processID, pollutantID, polProcessID),
       KEY (pollutantID, processID, polProcessID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
TRUNCATE TABLE PM10PollutantProcessAssoc;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache select p.* into outfile '##PM10EmissionRatio##'
from PollutantProcessAssoc ppa
inner join PM10EmissionRatio p on (p.polProcessID=ppa.polProcessID)
inner join RunSpecSourceFuelType r on (r.sourceTypeID=p.sourceTypeID and r.fuelTypeID=p.fuelTypeID)
where ppa.pollutantID in (106,107)
and ppa.processID in (##context.iterProcess.databaseKey##);

cache select distinct ppa.polProcessID, ppa.processID, ppa.pollutantID, ppa.isAffectedByExhaustIM, ppa.isAffectedByEvapIM
into outfile '##PM10PollutantProcessAssoc##'
from PollutantProcessAssoc ppa
inner join PM10EmissionRatio p on (p.polProcessID=ppa.polProcessID)
inner join RunSpecSourceFuelType r on (r.sourceTypeID=p.sourceTypeID and r.fuelTypeID=p.fuelTypeID)
where ppa.pollutantID in (106,107)
and ppa.processID in (##context.iterProcess.databaseKey##);

-- End Section Extract Data

-- Section Processing

drop table if exists PM10BrakeTireMOVESWorkerOutputTemp;
create table if not exists PM10BrakeTireMOVESWorkerOutputTemp (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL DEFAULT 0,
	iterationID			SMALLINT UNSIGNED DEFAULT 1,
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
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- @algorithm PM10 Brakewear (106) = PM2.5 Brakewear (116) * PM10PM25Ratio.
-- PM10 Tirewear (107) = PM2.5 Tirewear (117) * PM10PM25Ratio.
insert into PM10BrakeTireMOVESWorkerOutputTemp (
	MOVESRunID, iterationID,
    yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID,
	pollutantID,
	processID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
	emissionQuant, emissionRate)
select
	MOVESRunID, iterationID,
    yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID,
	ppa.pollutantID as pollutantID,
	mwo.processID, mwo.sourceTypeID, mwo.regClassID, mwo.fuelTypeID, modelYearID, roadTypeID, SCC,
	(emissionQuant * PM10PM25Ratio) as emissionQuant,
	(emissionRate * PM10PM25Ratio) as emissionRate
from MOVESWorkerOutput mwo
inner join PM10PollutantProcessAssoc ppa on (ppa.processID=mwo.processID)
inner join PM10EmissionRatio r on (
	r.polProcessID=ppa.polProcessID 
	and r.sourceTypeID=mwo.sourceTypeID 
	and r.fuelTypeID=mwo.fuelTypeID
	and r.minModelYearID <= mwo.modelYearID
	and r.maxModelYearID >= mwo.modelYearID)
where ((mwo.pollutantID=116 and ppa.pollutantID=106)
	or (mwo.pollutantID=117 and ppa.pollutantID=107));

insert into MOVESWorkerOutput (
	MOVESRunID, iterationID,
    yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID,
	pollutantID,
	processID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
	emissionQuant, emissionRate)
select
	MOVESRunID, iterationID,
    yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID,
	pollutantID,
	processID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
	emissionQuant, emissionRate
from PM10BrakeTireMOVESWorkerOutputTemp;

-- End Section Processing

-- Section Cleanup
drop table if exists PM10BrakeTireMOVESWorkerOutputTemp;
drop table if exists PM10PollutantProcessAssoc;
-- End Section Cleanup
