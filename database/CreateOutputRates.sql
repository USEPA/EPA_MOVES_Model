/*
   Version 2013-10-23
   Author Wesley Faler
*/
DROP TABLE IF EXISTS BaseRateOutput;
DROP TABLE IF EXISTS BaseRateUnits;

CREATE TABLE IF NOT EXISTS BaseRateUnits (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,

	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,

	meanBaseRateUnitsNumerator varchar(50) null default '',
	meanBaseRateUnitsDenominator varchar(50) null default '',
	emissionBaseRateUnitsNumerator varchar(50) null default '',
	emissionBaseRateUnitsDenominator varchar(50) null default ''
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

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

	meanBaseRate		 FLOAT NULL,
	emissionRate		 FLOAT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;
