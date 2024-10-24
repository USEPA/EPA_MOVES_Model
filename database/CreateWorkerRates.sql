/*
   Version 2013-10-23
   Author Wesley Faler
*/
DROP TABLE IF EXISTS BaseRateOutput;

CREATE TABLE IF NOT EXISTS BaseRateOutput (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	iterationID          SMALLINT UNSIGNED NULL DEFAULT 1,

	zoneID				 INTEGER NOT NULL DEFAULT '0',
	linkID				 INTEGER NOT NULL DEFAULT '0',
	sourceTypeID         SMALLINT NOT NULL DEFAULT '0',
    SCC                  CHAR(10) NOT NULL DEFAULT '',
	roadTypeID           SMALLINT NOT NULL DEFAULT '0',
	avgSpeedBinID        SMALLINT NOT NULL DEFAULT '0',
	monthID              SMALLINT NOT NULL DEFAULT '0',
	hourDayID            SMALLINT NOT NULL DEFAULT '0',
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID			 SMALLINT NOT NULL DEFAULT '0',
	yearID               SMALLINT NOT NULL,
	fuelTypeID			 SMALLINT NOT NULL DEFAULT '0',
	regClassID			 SMALLINT NOT NULL DEFAULT '0',

	-- Pollutant [mass,energy,moles,etc] in the time period and region.
	-- Reflects mixture of I/M and non-I/M vehicles.
	meanBaseRate		 FLOAT NULL,

	-- Pollutant [mass,energy,moles,etc] per activity unit such
	-- as distance, start, and idle hour.
	-- Reflects mixture of I/M and non-I/M vehicles.
	emissionRate		 FLOAT NULL
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
