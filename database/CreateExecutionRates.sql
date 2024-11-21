-- Author Wesley Faler
-- Version 2016-03-14

drop table if exists RatesOpModeDistribution;

CREATE TABLE IF NOT EXISTS RatesOpModeDistribution (
	sourceTypeID         SMALLINT NOT NULL,
	roadTypeID           SMALLINT NOT NULL,
	avgSpeedBinID        SMALLINT NOT NULL DEFAULT '0',
	hourDayID            SMALLINT NOT NULL DEFAULT '0',
	polProcessID         int NOT NULL,
	opModeID             SMALLINT NOT NULL,
	opModeFraction       FLOAT NULL,
	opModeFractionCV     FLOAT NULL,
	avgBinSpeed			 FLOAT NULL,
	avgSpeedFraction 	 float not null default '0',

	PRIMARY KEY (sourceTypeID, polProcessID, roadTypeID, hourDayID, opModeID, avgSpeedBinID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Go-based	P RIMARY KEY (sourceTypeID, polProcessID, roadTypeID, hourDayID, opModeID, avgSpeedBinID)

-- Keys before Go-based speedup of SourceUseTypePhysics:
-- 	P RIMARY KEY (sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, opModeID)
-- 	K EY (sourceTypeID)
-- 	K EY (roadTypeID)
-- 	K EY (avgSpeedBinID)
-- 	K EY (hourDayID)
-- 	K EY (polProcessID)
-- 	K EY (opModeID)

TRUNCATE TABLE RatesOpModeDistribution;

drop table if exists SBWeightedEmissionRateByAge;

CREATE TABLE IF NOT EXISTS SBWeightedEmissionRateByAge (
	sourceTypeID		SMALLINT NOT NULL,
	polProcessID		int NOT NULL,
	opModeID			SMALLINT NOT NULL,
	modelYearID			SMALLINT NOT NULL,
	fuelTypeID			SMALLINT NOT NULL,
	ageGroupID			SMALLINT NOT NULL,
	regClassID			SMALLINT NOT NULL,

	meanBaseRate		FLOAT NULL,
	meanBaseRateIM		FLOAT NULL,
	meanBaseRateACAdj	FLOAT NULL,
	meanBaseRateIMACAdj	FLOAT NULL,
	sumSBD				DOUBLE NULL,
	sumSBDRaw			DOUBLE NULL,
	unique key (sourceTypeID, polProcessID, opModeID, modelYearID, fuelTypeID, ageGroupID, regClassID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE SBWeightedEmissionRateByAge;

drop table if exists SBWeightedEmissionRate;

CREATE TABLE IF NOT EXISTS SBWeightedEmissionRate (
	sourceTypeID		SMALLINT NOT NULL,
	polProcessID		int NOT NULL,
	opModeID			SMALLINT NOT NULL,
	modelYearID			SMALLINT NOT NULL,
	fuelTypeID			SMALLINT NOT NULL,
	regClassID			SMALLINT NOT NULL,

	meanBaseRate		FLOAT NULL,
	meanBaseRateIM		FLOAT NULL,
	meanBaseRateACAdj	FLOAT NULL,
	meanBaseRateIMACAdj	FLOAT NULL,
	sumSBD				DOUBLE NULL,
	sumSBDRaw			DOUBLE NULL,
	unique key (sourceTypeID, polProcessID, opModeID, modelYearID, fuelTypeID, regClassID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE SBWeightedEmissionRate;

drop table if exists SBWeightedDistanceRate;

CREATE TABLE IF NOT EXISTS SBWeightedDistanceRate (
	sourceTypeID		SMALLINT NOT NULL,
	polProcessID		int NOT NULL,
	modelYearID			SMALLINT NOT NULL,
	fuelTypeID			SMALLINT NOT NULL,
	regClassID			SMALLINT NOT NULL,
	avgSpeedBinID 		smallint not null,

	meanBaseRate		FLOAT NULL,
	meanBaseRateIM		FLOAT NULL,
	meanBaseRateACAdj	FLOAT NULL,
	meanBaseRateIMACAdj	FLOAT NULL,
	sumSBD				DOUBLE NULL,
	sumSBDRaw			DOUBLE NULL,
	primary key (sourceTypeID, polProcessID, modelYearID, fuelTypeID, regClassID, avgSpeedBinID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE SBWeightedDistanceRate;

drop table if exists distanceEmissionRate;

create table if not exists distanceEmissionRate (
	polProcessID int not null,
	fuelTypeID smallint not null,
	sourceTypeID smallint not null,
	modelYearID smallint not null,
	avgSpeedBinID smallint not null,
	ratePerMile double not null,
	ratePerSHO double not null,
	primary key (sourceTypeID, polProcessID, modelYearID, fuelTypeID, avgSpeedBinID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

--	regClassID smallint not null,
--	primary key (sourceTypeID, polProcessID, modelYearID, fuelTypeID, regClassID, avgSpeedBinID)

truncate table distanceEmissionRate;

drop table if exists BaseRateByAge;

CREATE TABLE IF NOT EXISTS BaseRateByAge (
	sourceTypeID         SMALLINT NOT NULL,
	roadTypeID           SMALLINT NOT NULL,
	avgSpeedBinID        SMALLINT NOT NULL DEFAULT '0',
	hourDayID            SMALLINT NOT NULL DEFAULT '0',
	polProcessID         int NOT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID			 SMALLINT NOT NULL,
	fuelTypeID			 SMALLINT NOT NULL,
	ageGroupID			 SMALLINT NOT NULL,
	regClassID			 SMALLINT NOT NULL,
	opModeID			 SMALLINT NOT NULL,

	meanBaseRate		 FLOAT NULL,
	meanBaseRateIM		 FLOAT NULL,
	emissionRate		 FLOAT NULL,
	emissionRateIM		 FLOAT NULL,

	meanBaseRateACAdj	 FLOAT NULL,
	meanBaseRateIMACAdj	 FLOAT NULL,
	emissionRateACAdj    FLOAT NULL,
	emissionRateIMACAdj  FLOAT NULL,

	opModeFraction       FLOAT NULL,
	opModeFractionRate   FLOAT NULL,
	PRIMARY KEY (sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, modelYearID, fuelTypeID, ageGroupID, regClassID, opModeID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE BaseRateByAge;

drop table if exists BaseRate;

CREATE TABLE IF NOT EXISTS BaseRate (
	sourceTypeID         SMALLINT NOT NULL,
	roadTypeID           SMALLINT NOT NULL,
	avgSpeedBinID        SMALLINT NOT NULL DEFAULT '0',
	hourDayID            SMALLINT NOT NULL DEFAULT '0',
	polProcessID         int NOT NULL,
	pollutantID          SMALLINT UNSIGNED NULL DEFAULT NULL,
	processID            SMALLINT UNSIGNED NULL DEFAULT NULL,
	modelYearID			 SMALLINT NOT NULL,
	fuelTypeID			 SMALLINT NOT NULL,
	regClassID			 SMALLINT NOT NULL,
	opModeID			 SMALLINT NOT NULL,

	meanBaseRate		 FLOAT NULL,
	meanBaseRateIM		 FLOAT NULL,
	emissionRate		 FLOAT NULL,
	emissionRateIM		 FLOAT NULL,

	meanBaseRateACAdj	 FLOAT NULL,
	meanBaseRateIMACAdj	 FLOAT NULL,
	emissionRateACAdj    FLOAT NULL,
	emissionRateIMACAdj  FLOAT NULL,

	opModeFraction       FLOAT NULL,
	opModeFractionRate   FLOAT NULL,
	PRIMARY KEY (sourceTypeID, roadTypeID, avgSpeedBinID, hourDayID, polProcessID, modelYearID, fuelTypeID, regClassID, opModeID)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

TRUNCATE TABLE BaseRate;
