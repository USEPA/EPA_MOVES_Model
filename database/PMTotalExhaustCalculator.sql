-- Version 2008-03-29

-- Section Create Remote Tables for Extracted Data

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

-- End Section Extract Data

-- Section Processing

drop table if exists PMTotalMOVESWorkerOutputTemp;
create table if not exists PMTotalMOVESWorkerOutputTemp (
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
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Section PM10Total

insert into PMTotalMOVESWorkerOutputTemp (
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
select
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    100 as pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant
from MOVESWorkerOutput mwo
where mwo.pollutantID in (101,102,105);

-- End Section PM10Total

-- Section PM25Total

insert into PMTotalMOVESWorkerOutputTemp (
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
select
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    110 as pollutantID,
    processID,
    sourceTypeID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant
from MOVESWorkerOutput mwo
where mwo.pollutantID in (111,112,115);

-- End Section PM25Total

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
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant)
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
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant
from PMTotalMOVESWorkerOutputTemp;

-- End Section Processing

-- Section Cleanup
drop table if exists PMTotalMOVESWorkerOutputTemp;
-- End Section Cleanup
