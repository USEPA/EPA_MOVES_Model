-- Create the MOVESWorker database and schema.
-- Author Wesley Faler
-- Version 2015-03-16

DROP TABLE IF EXISTS MOVESOutput;
DROP TABLE IF EXISTS MOVESWorkerOutput;
DROP TABLE IF EXISTS MOVESWorkerActivityOutput;

-- ***********************************************************************************
-- ***********************************************************************************
-- MOVESOutput table.  Stores one row for each combination
-- of dimension field values, which includes pollutant. 
--
-- Note that dimension fields will never be null but they
-- may hold a default value that indicates an "all" selection.
-- ***********************************************************************************
-- ***********************************************************************************
CREATE TABLE IF NOT EXISTS MOVESWorkerOutput (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL DEFAULT 0,
	iterationID			SMALLINT UNSIGNED DEFAULT 1,
	
	yearID               SMALLINT UNSIGNED NULL,
	monthID              SMALLINT UNSIGNED NULL,
	dayID                SMALLINT UNSIGNED NULL,
	hourID               SMALLINT UNSIGNED NULL,

	-- ******************************************************
	-- stateID, locationID, zoneID, and linkID can all be default
	-- in the case where the user selected "Nation" as the 
	-- geographic granularity for the output.
	-- linkID and/or zoneID will be default otherwise if "County" 
	-- level granularity was selected depending upon scale.
	-- locationID will be default otherwise if "State" level
	-- granularity was selected.
	-- ******************************************************
	stateID              SMALLINT UNSIGNED NULL,
	countyID             INTEGER UNSIGNED NULL,
	zoneID               INTEGER UNSIGNED NULL,
	linkID               INTEGER UNSIGNED NULL,
	
	pollutantID          SMALLINT UNSIGNED NULL,
	processID            SMALLINT UNSIGNED NULL,
	
	sourceTypeID         SMALLINT UNSIGNED NULL,
	regClassID           SMALLINT UNSIGNED NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL,
	fuelSubTypeID        SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,

	-- ******************************************************
	-- roadTypeID is not redundant with linkID in the cases where
	-- the user wants road type as a dimension but does not want
	-- geographic detail to the link/zone (or perhaps even to
	-- the County) level.
	-- ******************************************************
	roadTypeID           SMALLINT UNSIGNED NULL,

	-- ******************************************************
	-- SCC holds both OnRoad and OffRoad SCC codes and may be
	-- all 0's (zeroes) to represent "all" SCC codes at once.
	-- ******************************************************
	SCC                  CHAR(10) NULL,

	-- ******************************************************
	-- OffRoad keys
	-- ******************************************************
	engTechID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sectorID             SMALLINT UNSIGNED NULL DEFAULT NULL,
	hpID                 SMALLINT UNSIGNED NULL DEFAULT NULL,

	-- ******************************************************
	-- The emission columns are the actual values produced,
	-- not dimensions to the data.  These will be NULL if the
	-- user chose not to generate them.
	-- ******************************************************

	-- Pollutant [mass,energy,moles,etc] in the time period and region.
	-- Reflects mixture of I/M and non-I/M vehicles.
	emissionQuant        FLOAT NULL,

	-- Pollutant [mass,energy,moles,etc] per activity unit such
	-- as distance, start, and idle hour.
	-- Reflects mixture of I/M and non-I/M vehicles.
	emissionRate         FLOAT NULL
);

TRUNCATE TABLE MOVESWorkerOutput;

CREATE TABLE IF NOT EXISTS MOVESWorkerActivityOutput (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL DEFAULT 0,
	iterationID			SMALLINT UNSIGNED DEFAULT 1,
	
	yearID               SMALLINT UNSIGNED NULL,
	monthID              SMALLINT UNSIGNED NULL,
	dayID                SMALLINT UNSIGNED NULL,
	hourID               SMALLINT UNSIGNED NULL,

	-- ******************************************************
	-- stateID, locationID, zoneID, and linkID can all be default
	-- in the case where the user selected "Nation" as the 
	-- geographic granularity for the output.
	-- linkID and/or zoneID will be default otherwise if "County" 
	-- level granularity was selected depending upon scale.
	-- locationID will be default otherwise if "State" level
	-- granularity was selected.
	-- ******************************************************
	stateID              SMALLINT UNSIGNED NULL,
	countyID             INTEGER UNSIGNED NULL,
	zoneID               INTEGER UNSIGNED NULL,
	linkID               INTEGER UNSIGNED NULL,
	
	sourceTypeID         SMALLINT UNSIGNED NULL,
	regClassID           SMALLINT UNSIGNED NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,

	-- ******************************************************
	-- roadTypeID is not redundant with linkID in the cases where
	-- the user wants road type as a dimension but does not want
	-- geographic detail to the link/zone (or perhaps even to
	-- the County) level.
	-- ******************************************************
	roadTypeID           SMALLINT UNSIGNED NULL,

	-- ******************************************************
	-- SCC holds both OnRoad and OffRoad SCC codes and may be
	-- all 0's (zeroes) to represent "all" SCC codes at once.
	-- ******************************************************
	SCC                  CHAR(10) NULL,

	-- ******************************************************
	-- OffRoad keys
	-- ******************************************************
	engTechID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sectorID             SMALLINT UNSIGNED NULL DEFAULT NULL,
	hpID                 SMALLINT UNSIGNED NULL DEFAULT NULL,

	activityTypeID       SMALLINT NOT NULL,
	activity             FLOAT NULL DEFAULT NULL
);

TRUNCATE TABLE MOVESWorkerActivityOutput;
