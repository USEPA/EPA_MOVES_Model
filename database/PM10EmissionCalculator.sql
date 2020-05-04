-- Author Wesley Faler
-- Version 2013-09-23

-- @algorithm
-- @owner PM10 Emission Calculator
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
);
TRUNCATE TABLE PM10PollutantProcessAssoc;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
cache select p.* into outfile '##PM10EmissionRatio##'
from PollutantProcessAssoc ppa
inner join PM10EmissionRatio p on (p.polProcessID=ppa.polProcessID)
inner join RunSpecSourceFuelType r on (r.sourceTypeID=p.sourceTypeID and r.fuelTypeID=p.fuelTypeID)
where ppa.pollutantID in (##pollutantIDs##)
and ppa.processID in (##context.iterProcess.databaseKey##, 15, 16, 17);

cache select distinct ppa.polProcessID, ppa.processID, ppa.pollutantID, ppa.isAffectedByExhaustIM, ppa.isAffectedByEvapIM
into outfile '##PM10PollutantProcessAssoc##'
from PollutantProcessAssoc ppa
inner join PM10EmissionRatio p on (p.polProcessID=ppa.polProcessID)
inner join RunSpecSourceFuelType r on (r.sourceTypeID=p.sourceTypeID and r.fuelTypeID=p.fuelTypeID)
where ppa.pollutantID in (##pollutantIDs##)
and ppa.processID in (##context.iterProcess.databaseKey##, 15, 16, 17);

-- End Section Extract Data

-- Section Processing

drop table if exists PM10MOVESWorkerOutputTemp;
create table if not exists PM10MOVESWorkerOutputTemp (
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
	emissionRate		 DOUBLE NULL,
	
	index (fuelTypeID),
	index (sourceTypeID),
	index (roadTypeID),
	index (zoneID)
);

-- @algorithm PM10 Total = PM2.5 Total * PM10PM25Ratio.
insert into PM10MOVESWorkerOutputTemp (
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
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate)
select
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    ppa.pollutantID,
    ppa.processID,
    r.sourceTypeID,
    regClassID,
    r.fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
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
where ((mwo.pollutantID=112 and ppa.pollutantID=102)
	or (mwo.pollutantID=111 and ppa.pollutantID=101)
	or (mwo.pollutantID=110 and ppa.pollutantID=100)
	or (mwo.pollutantID=115 and ppa.pollutantID=105 and mwo.processID in (1,2,90,91))
	)
	and mwo.pollutantID in (##sourcePollutantIDs##);

insert into MOVESWorkerOutput (
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
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate)
select
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
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate
from PM10MOVESWorkerOutputTemp;

-- End Section Processing

-- Section Cleanup
drop table if exists PM10MOVESWorkerOutputTemp;
drop table if exists PM10PollutantProcessAssoc;
-- End Section Cleanup
