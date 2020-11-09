/*
   Version 2017-09-29
   -- MOVESRun table structure modified by Mitch Cumberworth per Task 206
   -- Foreign keys removed by Wesley Faler Oct. 2007 to speedup Master-side INSERTs
   -- Output and activity primary keys and unique keys removed by Wesley Faler Jan. 2008 to speedup Master-side INSERTs
   -- MOVESRun table structure modified by Gwo Shyu per Task 812 "MOVES performance Improvement ...": 
		(1) Added a new table ActivityType
		(2) Structure of MOVESActivityOutput and MOVESOutput were modified - fields changed, and no primary key nor indexes
   -- MOVESRun table structure modified by Wes Faler per Task 902 to add Domain information
   -- MOVESOutput table structure modified by MJimenez 29Feb2012 add engTechID and sectorID for NONROAD
   -- MOVESActivityOutput table structure modified by MJimenez 29Feb2012 add engTechID and sectorID for NONROAD
   -- Merged Michele's changes with the changes done by Wes etc.
*/
/* Creates tables in the MOVESOutput Database */
DROP TABLE IF EXISTS MOVESOutput;
DROP TABLE IF EXISTS MOVESActivityOutput;
DROP TABLE IF EXISTS MOVESRun;
DROP TABLE IF EXISTS MOVESError;
DROP TABLE IF EXISTS MOVESEventLog;
DROP TABLE IF EXISTS MOVESWorkersUsed;
DROP TABLE IF EXISTS bundleTracking;
DROP TABLE IF EXISTS ActivityType;
DROP TABLE IF EXISTS MOVESTablesUsed;
DROP TABLE IF EXISTS RatePerDistance;
DROP TABLE IF EXISTS RatePerVehicle;
DROP TABLE IF EXISTS RatePerProfile;
DROP TABLE IF EXISTS StartsPerVehicle;
DROP TABLE IF EXISTS RatePerStart;
DROP TABLE IF EXISTS RatePerHour;

CREATE TABLE MOVESEventLog (
	EventRecordID        INT UNSIGNED NOT NULL,
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
    PRIMARY KEY (EventRecordID, MOVESRunID),
	EventName            CHAR(255) NOT NULL,
	WhenStarted          INT UNSIGNED NOT NULL,
	WhenStopped          INT UNSIGNED NULL,
	Duration             INT UNSIGNED NULL
);

-- ***********************************************************************************
-- ***********************************************************************************
-- MOVESOutput table.  Stores one row for each combination
-- of dimension field values, which includes pollutant. 
--
-- Note that dimension fields will never be null but they
-- may hold a default value that indicates an "all" selection.
-- ***********************************************************************************
-- ***********************************************************************************
-- No PK nor indexes
CREATE TABLE MOVESOutput (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	iterationID          SMALLINT UNSIGNED NULL DEFAULT 1,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
-- ******************************************************
-- stateID, locationID, zoneID, and linkID can all be default
-- in the case where the user selected "Nation" as the 
-- geographic granularity for the output.
-- linkID and/or zoneID will be default otherwise if "County" 
-- level granularity was selected depending upon scale.
-- locationID will be default otherwise if "State" level
-- granularity was selected.
-- ******************************************************
	stateID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	countyID             INTEGER  UNSIGNED NULL DEFAULT NULL,
	zoneID               INTEGER  UNSIGNED NULL DEFAULT NULL,
	linkID               INTEGER  UNSIGNED NULL DEFAULT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
	regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	fuelSubTypeID        SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
-- ******************************************************
-- roadTypeID is not redundant with linkID in the cases where
-- the user wants road type as a dimension but does not want
-- geographic detail to the link/zone (or perhaps even to
-- the County) level.
-- ******************************************************
	roadTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
-- ******************************************************
-- SCC holds both OnRoad and OffRoad SCC codes and may be
-- all 0's (zeroes) to represent "all" SCC codes at once.
-- ******************************************************
	SCC                  CHAR(10) NULL DEFAULT NULL,
-- ******************************************************
-- OffRoad keys
-- ******************************************************
	engTechID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sectorID             SMALLINT UNSIGNED NULL DEFAULT NULL,
	hpID                 SMALLINT UNSIGNED NULL DEFAULT NULL,
-- ******************************************************
-- The emission* columns are the actual values produced,
-- not dimensions to the data.  These will be NULL if the
-- user chose not to generate them.
-- ******************************************************
	emissionQuant        FLOAT NULL DEFAULT NULL,
	emissionQuantMean    FLOAT NULL DEFAULT NULL,
	emissionQuantSigma   FLOAT NULL  DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

CREATE TABLE MOVESRun (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL auto_increment,
-- ******************************************************
-- outputTimePeriod has values 'Hour', 'Day', 'Month', or 'Year'
-- ******************************************************
	outputTimePeriod     CHAR(5) NULL DEFAULT NULL,
	timeUnits            CHAR(5) NULL DEFAULT NULL,
	distanceUnits        CHAR(5) NULL DEFAULT NULL,
	massUnits            CHAR(5) NULL DEFAULT NULL,
	energyUnits          CHAR(5) NULL DEFAULT NULL,
-- ******************************************************
-- runSpecFileName can be null if the user has not saved
-- their runspec prior to launching the simulation.
-- ******************************************************
	runSpecFileName      VARCHAR(500) NULL DEFAULT NULL,
	runSpecDescription   TEXT NULL,
	runSpecFileDateTime  DATETIME NULL DEFAULT NULL,
	runDateTime          DATETIME NULL DEFAULT NULL,
-- ******************************************************
-- scale has values 'MACRO', 'MESO', 'MICRO'
-- ******************************************************
	scale                CHAR(5) NULL DEFAULT NULL,
	minutesDuration      FLOAT NULL  DEFAULT NULL,
	defaultDatabaseUsed  VARCHAR(200) NULL DEFAULT NULL,
	masterVersion        VARCHAR(100) NULL DEFAULT NULL,
	masterComputerID     VARCHAR(255) NULL DEFAULT NULL,
	masterIDNumber       VARCHAR(255) NULL DEFAULT NULL,
-- ******************************************************
-- domain has values 'NATIONAL', 'SINGLE', 'PROJECT'
-- ******************************************************
	domain               CHAR(10) NULL DEFAULT 'NATIONAL',
	domainCountyID		 INTEGER UNSIGNED NULL DEFAULT NULL,
	domainCountyName     VARCHAR(50) NULL DEFAULT NULL,
	domainDatabaseServer VARCHAR(100) NULL DEFAULT NULL,
	domainDatabaseName   VARCHAR(200) NULL DEFAULT NULL,

	expectedDONEFiles    INTEGER UNSIGNED NULL DEFAULT NULL,
	retrievedDONEFiles   INTEGER UNSIGNED NULL DEFAULT NULL,

	models               VARCHAR(40) NOT NULL DEFAULT 'onroad',

	PRIMARY KEY (MOVESRunID)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

CREATE TABLE MOVESError (
	MOVESErrorID         INTEGER  UNSIGNED NOT NULL AUTO_INCREMENT,
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	stateID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	countyID             INTEGER UNSIGNED NULL DEFAULT NULL,
	zoneID               INTEGER UNSIGNED NULL DEFAULT NULL,
	linkID               INTEGER UNSIGNED NULL DEFAULT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	errorMessage         VARCHAR(255) NOT NULL,
	PRIMARY KEY (MOVESErrorID),
	KEY IX_MOVES_ERROR_ID (MOVESErrorID),
	KEY IX_MOVES_RUN_ID (MOVESRunID)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- ***********************************************************************************
-- ***********************************************************************************
-- MOVESActivityOutput table. Used for "Additional Outputs" which are not
-- pollutant dependent such as distance. 
-- ***********************************************************************************
-- ***********************************************************************************
-- No PK nor indexes
CREATE TABLE MOVESActivityOutput (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	iterationID          SMALLINT UNSIGNED NULL DEFAULT 1,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
-- ******************************************************
-- stateID, locationID, zoneID, and linkID can all be default
-- in the case where the user selected "Nation" as the 
-- geographic granularity for the output.
-- linkID and/or zoneID will be default otherwise if "County" 
-- level granularity was selected depending upon scale.
-- locationID will be default otherwise if "State" level
-- granularity was selected.
-- ******************************************************
	stateID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	countyID             INTEGER UNSIGNED NULL DEFAULT NULL,
	zoneID               INTEGER UNSIGNED NULL DEFAULT NULL,
	linkID               INTEGER UNSIGNED NULL DEFAULT NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
	regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	fuelSubTypeID        SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
-- ******************************************************
-- roadTypeID is not redundant with linkID in the cases where
-- the user wants road type as a dimension but does not want
-- geographic detail to the link/zone (or perhaps even to
-- the County) level.
-- ******************************************************
	roadTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
-- ******************************************************
-- SCC holds both OnRoad and OffRoad SCC codes and may be
-- all 0's (zeroes) to represent "all" SCC codes at once.
-- ******************************************************
	SCC                  CHAR(10) NULL DEFAULT NULL,
	engTechID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sectorID             SMALLINT UNSIGNED NULL DEFAULT NULL,
	hpID                 SMALLINT UNSIGNED NULL DEFAULT NULL,
	activityTypeID       SMALLINT NOT NULL,
	activity             FLOAT NULL DEFAULT NULL,
	activityMean         FLOAT NULL DEFAULT NULL,
	activitySigma        FLOAT NULL DEFAULT NULL 
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

CREATE TABLE ActivityType (
	activityTypeID       SMALLINT UNSIGNED NOT NULL,
	activityType         CHAR(20) NOT NULL,
	activityTypeDesc     CHAR(50) NULL DEFAULT NULL,
	PRIMARY KEY (activityTypeID)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- add records
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (1, "distance", "Distance traveled");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (2, "sourcehours", "Source Hours");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (3, "extidle", "Extended Idle Hours");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (4, "sho", "Source Hours Operating");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (5, "shp", "Source Hours Parked");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (6, "population", "Population");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (7, "starts", "Starts");
-- INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
-- VALUES (8, "hotelling", "Hotelling Hours")
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (9, "avghp", "Average Horsepower");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (10, "retrofrac", "Fraction Retrofitted");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (11, "retrocnt", "Number Units Retrofitted");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (12, "loadfactor", "Load Factor");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (13, "hotellingAux", "Hotelling Diesel Aux");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (14, "hotellingElectric", "Hotelling Battery or AC");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (15, "hotellingOff", "Hotelling All Engines Off");
INSERT IGNORE INTO ActivityType (activityTypeID, activityType, activityTypeDesc)
VALUES (16, "shi", "Source Hours Idle");



CREATE TABLE MOVESWorkersUsed (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	workerVersion        VARCHAR(100) NOT NULL,
	workerComputerID     VARCHAR(255) NOT NULL,
	workerID             VARCHAR(255) NOT NULL DEFAULT '',
	bundleCount          INTEGER UNSIGNED NOT NULL DEFAULT '0',
	failedBundleCount    INTEGER UNSIGNED NOT NULL DEFAULT '0',
	PRIMARY KEY (MOVESRunID, workerVersion, workerComputerID, workerID)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

CREATE TABLE bundleTracking (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	-- 'M' for master, 'W' for worker
	hostType             char(1) not null default ' ',
	loopableClassName 	 varchar(200) not null default '',

	-- worker fields will be blank ('') for tasks done on a master
	workerVersion        VARCHAR(100) NOT NULL,
	workerComputerID     VARCHAR(255) NOT NULL,
	workerID             VARCHAR(255) NOT NULL DEFAULT '',
	-- bundleNumber will be 0 for tasks done on a master, even if the task is done on behalf of a calculator
	bundleNumber		 int not null default '0',
	-- isCleanUp is set to 'N' for bundles done on a worker
	isCleanUp 			 char(1) not null default 'N', 

	iterationID 		 smallint unsigned null default null,
	processID 			 smallint unsigned null default null,
	roadTypeID		 	 smallint unsigned null default null,
	linkID		 		 integer unsigned null default null,
	zoneID 				 integer unsigned null default null,
	countyID 			 integer unsigned null default null,
	stateID 			 smallint unsigned null default null,
	yearID 				 smallint unsigned null default null,
	monthID 			 smallint unsigned null default null,
	dayID 				 smallint unsigned null default null,
	hourID 				 smallint unsigned null default null,
	executionGranularity varchar(10) null default null,
	executionPriority 	 smallint unsigned null,

	durationSeconds		 FLOAT NULL DEFAULT NULL,

	-- There is no primary key in this table, but the following KEY is
	-- useful when searching for performance bottlenecks.
	KEY (MOVESRunID, hostType, loopableClassName)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

CREATE TABLE MOVESTablesUsed (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	databaseServer		 VARCHAR(100) NOT NULL DEFAULT '',
	databaseName		 VARCHAR(200) NOT NULL,
	tableName			 VARCHAR(200) NOT NULL,
	dataFileSize	     INTEGER UNSIGNED NULL DEFAULT NULL,
	dataFileModificationDate DATETIME NULL DEFAULT NULL,
	tableUseSequence	 INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (MOVESRunID, databaseServer, databaseName, tableName),
	KEY (MOVESRunID, tableUseSequence)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- ***********************************************************************************
-- ***********************************************************************************
-- RatePerDistance table
-- Includes emissions for the processes:  Running exhaust, tire wear, brake wear,
-- crankcase, and refueling
-- ***********************************************************************************
-- ***********************************************************************************
-- No PK nor indexes
CREATE TABLE RatePerDistance (
	MOVESScenarioID		 VARCHAR(40) NOT NULL DEFAULT '',
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	linkID               INTEGER  UNSIGNED NULL DEFAULT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
	regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	SCC                  CHAR(10) NULL DEFAULT NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	roadTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	avgSpeedBinID        SMALLINT NULL DEFAULT NULL,
	temperature          FLOAT NULL DEFAULT NULL,
	relHumidity          FLOAT NULL DEFAULT NULL,
	ratePerDistance      FLOAT NULL DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- ***********************************************************************************
-- ***********************************************************************************
-- RatePerVehicle table
-- Includes emissions for processes:  Start exhaust, start crankcase, permeation,
-- liquid leaks, and extended idle
-- ***********************************************************************************
-- ***********************************************************************************
-- No PK nor indexes
CREATE TABLE RatePerVehicle (
	MOVESScenarioID		 VARCHAR(40) NOT NULL DEFAULT '',
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	zoneID               INTEGER  UNSIGNED NULL DEFAULT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
	regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	SCC                  CHAR(10) NULL DEFAULT NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	temperature          FLOAT NULL DEFAULT NULL,
	relHumidity          FLOAT NULL DEFAULT NULL,
	ratePerVehicle       FLOAT NULL DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- ***********************************************************************************
-- ***********************************************************************************
-- RatePerProfile table
-- Includes emissions from vapor venting process
-- ***********************************************************************************
-- ***********************************************************************************
-- No PK nor indexes
CREATE TABLE RatePerProfile (
	MOVESScenarioID		 VARCHAR(40) NOT NULL DEFAULT '',
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	temperatureProfileID BIGINT NULL DEFAULT NULL,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
	regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	SCC                  CHAR(10) NULL DEFAULT NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	temperature          FLOAT NULL DEFAULT NULL,
	relHumidity          FLOAT NULL DEFAULT NULL,
	ratePerVehicle       FLOAT NULL DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- ***********************************************************************************
-- ***********************************************************************************
-- StartsPerVehicle table. Starts per existing vehicle, even if the vehicle did not start.
-- ***********************************************************************************
-- ***********************************************************************************
-- No PK nor indexes
CREATE TABLE StartsPerVehicle (
	MOVESScenarioID		 VARCHAR(40) NOT NULL DEFAULT '',
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	zoneID               INTEGER  UNSIGNED NULL DEFAULT NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
	regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	SCC                  CHAR(10) NULL DEFAULT NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	startsPerVehicle     FLOAT NULL DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- ***********************************************************************************
-- ***********************************************************************************
-- RatePerStart table. Emissions per start.
-- ***********************************************************************************
-- ***********************************************************************************
-- No PK nor indexes
CREATE TABLE RatePerStart (
	MOVESScenarioID		 VARCHAR(40) NOT NULL DEFAULT '',
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	zoneID               INTEGER  UNSIGNED NULL DEFAULT NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
	regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	SCC                  CHAR(10) NULL DEFAULT NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	temperature          FLOAT NULL DEFAULT NULL,
	relHumidity          FLOAT NULL DEFAULT NULL,
	ratePerStart         FLOAT NULL DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- ***********************************************************************************
-- ***********************************************************************************
-- RatePerHour table
-- Includes emissions for the processes: Extended Idle (90), APU (91)
-- ***********************************************************************************
-- ***********************************************************************************
-- No PK nor indexes
CREATE TABLE RatePerHour (
	MOVESScenarioID		 VARCHAR(40) NOT NULL DEFAULT '',
	MOVESRunID           SMALLINT UNSIGNED NOT NULL,
	yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
	dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
	hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
	linkID               INTEGER  UNSIGNED NULL DEFAULT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
	regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	SCC                  CHAR(10) NULL DEFAULT NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	roadTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
	temperature          FLOAT NULL DEFAULT NULL,
	relHumidity          FLOAT NULL DEFAULT NULL,
	ratePerHour          FLOAT NULL DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;
