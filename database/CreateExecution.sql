-- Author Wesley Faler
-- Version 2017-09-28

create table if not exists drivingIdleFraction (
	hourDayID smallint not null,
	yearID smallint not null,
	roadTypeID smallint not null,
	sourceTypeID smallint not null,
	drivingIdleFraction double not null,
	primary key (hourDayID, roadTypeID, sourceTypeID, yearID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- *************************************************************************************
-- The following tables are used in joins for calculations to filter the number of items
-- used in the calculations.
-- *************************************************************************************

drop table if exists RunSpecSourceType;

CREATE TABLE IF NOT EXISTS RunSpecSourceType (
	sourceTypeID SMALLINT NOT NULL,
	UNIQUE INDEX NdxSourceTypeID (
	sourceTypeID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecSourceType;

drop table if exists RunSpecRoadType;

CREATE TABLE IF NOT EXISTS RunSpecRoadType (
	roadTypeID SMALLINT NOT NULL,
	UNIQUE INDEX NdxRoadTypeID (
	roadTypeID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecRoadType;

drop table if exists RunSpecMonth;

CREATE TABLE IF NOT EXISTS RunSpecMonth (
	monthID SMALLINT NOT NULL,
	UNIQUE INDEX NdxMonthID (
	monthID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecMonth;

drop table if exists RunSpecDay;

CREATE TABLE IF NOT EXISTS RunSpecDay (
	dayID SMALLINT NOT NULL,
	UNIQUE INDEX NdxDayID (
	dayID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecDay;

drop table if exists RunSpecHour;

CREATE TABLE IF NOT EXISTS RunSpecHour (
	hourID SMALLINT NOT NULL,
	UNIQUE INDEX NdxHourID (
	hourID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecHour;

drop table if exists RunSpecMonthGroup;

CREATE TABLE IF NOT EXISTS RunSpecMonthGroup (
	monthGroupID SMALLINT NOT NULL,
	UNIQUE INDEX NdxMonthGroupID (
	monthGroupID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecMonthGroup;

drop table if exists RunSpecYear;

CREATE TABLE IF NOT EXISTS RunSpecYear (
	yearID SMALLINT NOT NULL,
	UNIQUE INDEX NdxHourID (
	yearID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecYear;

drop table if exists RunSpecModelYearAge;

CREATE TABLE IF NOT EXISTS RunSpecModelYearAge (
	yearID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	ageID SMALLINT NOT NULL,
	
	primary key (modelYearID, ageID, yearID),
	key (yearID, modelYearID, ageID),
	key (ageID, modelYearID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecModelYearAge;

drop table if exists RunSpecModelYearAgeGroup;

CREATE TABLE IF NOT EXISTS RunSpecModelYearAgeGroup (
	yearID smallint(6) NOT NULL,
	modelYearID smallint(6) NOT NULL,
	ageGroupID smallint(6) NOT NULL,
	PRIMARY KEY (modelYearID,ageGroupID,yearID),
	KEY yearID (yearID,modelYearID,ageGroupID),
	KEY yearID2 (yearID,ageGroupID,modelYearID),
	KEY ageID (ageGroupID,modelYearID,yearID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecModelYearAgeGroup;

drop table if exists RunSpecModelYear;

CREATE TABLE IF NOT EXISTS RunSpecModelYear (
	modelYearID SMALLINT NOT NULL primary key
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecModelYear;

drop table if exists RunSpecSourceFuelType;

CREATE TABLE IF NOT EXISTS RunSpecSourceFuelType (
	sourceTypeID SMALLINT NOT NULL,
	fuelTypeID TINYINT NOT NULL,
	UNIQUE INDEX NdxSourceFuelTypeID (
	sourceTypeID, fuelTypeID),
	unique key (fuelTypeID, sourceTypeID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecSourceFuelType;

drop table if exists RunSpecHourDay;

CREATE TABLE IF NOT EXISTS RunSpecHourDay (
	hourDayID SMALLINT NOT NULL,
	UNIQUE INDEX NdxHourDayID (
	hourDayID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecHourDay;

drop table if exists RunSpecState;

CREATE TABLE IF NOT EXISTS RunSpecState (
	stateID SMALLINT NOT NULL,
	UNIQUE INDEX NdxState (
	stateID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecState;

drop table if exists RunSpecCounty;

CREATE TABLE IF NOT EXISTS RunSpecCounty (
	countyID INTEGER NOT NULL,
	UNIQUE INDEX NDXCounty (
	countyID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecCounty;

drop table if exists RunSpecFuelRegion;

CREATE TABLE IF NOT EXISTS RunSpecFuelRegion (
	fuelRegionID INTEGER NOT NULL,
	UNIQUE INDEX NDXFuelRegion (
	fuelRegionID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecFuelRegion;

drop table if exists RunSpecZone;

CREATE TABLE IF NOT EXISTS RunSpecZone (
	zoneID INTEGER NOT NULL,
	UNIQUE INDEX NdxZone (
	zoneID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecZone;

drop table if exists RunSpecLink;

CREATE TABLE IF NOT EXISTS RunSpecLink (
	linkID INTEGER NOT NULL,
	UNIQUE INDEX NdxLink (
	linkID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecLink;

drop table if exists RunSpecPollutant;

CREATE TABLE IF NOT EXISTS RunSpecPollutant (
	pollutantID SMALLINT NOT NULL,
	UNIQUE INDEX NdxPollutant (
	pollutantID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecPollutant;

drop table if exists RunSpecProcess;

CREATE TABLE IF NOT EXISTS RunSpecProcess (
	processID SMALLINT NOT NULL,
	UNIQUE INDEX NdxProcess (
	processID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecProcess;

drop table if exists RunSpecPollutantProcess;

CREATE TABLE IF NOT EXISTS RunSpecPollutantProcess (
	polProcessID int NOT NULL,
	UNIQUE INDEX NdxPolProcess (
	polProcessID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecPollutantProcess;

drop table if exists RunSpecChainedTo;

CREATE TABLE IF NOT EXISTS RunSpecChainedTo (
	outputPolProcessID int not null,
	outputPollutantID smallint not null,
	outputProcessID smallint not null,
	inputPolProcessID int not null,
	inputPollutantID smallint not null,
	inputProcessID smallint not null,
	index InputChainedToIndex (
		inputPollutantID,
		inputProcessID
	),
	index InputChainedToProcessIndex (
		inputProcessID
	),
	index OutputChainedToPolProcessIndex (
		outputPolProcessID
	),
	index InputOutputChainedToIndex (
		outputPolProcessID,
		inputPolProcessID
	),
	index InputOutputChainedToIndex2 (
		inputPolProcessID,
		outputPolProcessID
	)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecChainedTo;

drop table if exists RunSpecSectorFuelType;

CREATE TABLE IF NOT EXISTS RunSpecSectorFuelType (
	sectorID SMALLINT NOT NULL,
	fuelTypeID TINYINT NOT NULL,
	UNIQUE INDEX NdxSectorFuelTypeID (
	sectorID, fuelTypeID),
	unique key (fuelTypeID, sectorID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecSectorFuelType;

drop table if exists RunSpecSector;

CREATE TABLE IF NOT EXISTS RunSpecSector (
	sectorID SMALLINT NOT NULL,
	UNIQUE INDEX NdxSectorID (
	sectorID ASC)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecSector;

drop table if exists RunSpecNonRoadModelYearAge;

CREATE TABLE IF NOT EXISTS RunSpecNonRoadModelYearAge (
	yearID SMALLINT NOT NULL,
	modelYearID SMALLINT NOT NULL,
	ageID SMALLINT NOT NULL,
	
	primary key (modelYearID, ageID, yearID),
	key (yearID, modelYearID, ageID),
	key (ageID, modelYearID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecNonRoadModelYearAge;

drop table if exists RunSpecNonRoadModelYear;

CREATE TABLE IF NOT EXISTS RunSpecNonRoadModelYear (
	modelYearID SMALLINT NOT NULL primary key
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecNonRoadModelYear;

drop table if exists RunSpecNonRoadChainedTo;

CREATE TABLE IF NOT EXISTS RunSpecNonRoadChainedTo (
	outputPolProcessID int not null,
	outputPollutantID smallint not null,
	outputProcessID smallint not null,
	inputPolProcessID int not null,
	inputPollutantID smallint not null,
	inputProcessID smallint not null,
	index InputChainedToIndex (
		inputPollutantID,
		inputProcessID
	),
	index InputChainedToProcessIndex (
		inputProcessID
	),
	index OutputChainedToPolProcessIndex (
		outputPolProcessID
	),
	index InputOutputChainedToIndex (
		outputPolProcessID,
		inputPolProcessID
	),
	index InputOutputChainedToIndex2 (
		inputPolProcessID,
		outputPolProcessID
	)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE RunSpecNonRoadChainedTo;

-- Example: If a 1995 Euro car should be treated as a 1991 US car,
-- then the 1991 PollutantProcessModelYear should be used for the
-- the 1995 modelyear. So, use reverse model year mapping so the
-- modelYearID in PollutantProcessMappedModelYear maps older
-- model year groups to a newer model year.

drop table if exists PollutantProcessMappedModelYear;

CREATE TABLE IF NOT EXISTS PollutantProcessMappedModelYear (
    polProcessID int NOT NULL ,
    modelYearID SMALLINT NOT NULL ,
    modelYearGroupID INT NOT NULL ,
    fuelMYGroupID INTEGER NULL,
    IMModelYearGroupID INTEGER NULL,
    key (modelYearID, polProcessID),
    key (polProcessID),
    key (modelYearID),
    primary key (polProcessID, modelYearID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE PollutantProcessMappedModelYear;
