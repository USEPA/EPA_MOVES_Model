-- Version 2014-05-20

-- @algorithm
-- @owner NO2 Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data
DROP TABLE IF EXISTS NO2CopyOfSourceUseType;
CREATE TABLE NO2CopyOfSourceUseType (
	sourceTypeID	smallint(6)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

DROP TABLE IF EXISTS NO2CopyOfPPA;
CREATE TABLE NO2CopyOfPPA (
	polProcessID	int,
	processID		smallint(6),	
	pollutantID		smallint(6)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

DROP TABLE IF EXISTS NO2CopyOfPPMY;
CREATE TABLE NO2CopyOfPPMY (
	polProcessID		int,
	modelYearID			smallint(6),	
	modelYearGroupID	int(11),
	fuelMYGroupID		int(11)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

DROP TABLE IF EXISTS NO2CopyOfFuelType;
CREATE TABLE NO2CopyOfFuelType (
       fuelTypeID        smallint(6)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

DROP TABLE IF EXISTS NO2CopyOfNONO2Ratio;
CREATE TABLE NO2CopyOfNONO2Ratio (
	polProcessID		int,
	sourceTypeID		smallint(6),
	fuelTypeID			smallint(6),
	modelYearGroupID	int(11),
	NOxRatio 			float,
	NOxRatioCV			float,
	dataSourceId		smallint(6)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
cache SELECT distinct sourceTypeID  INTO OUTFILE '##NO2CopyOfSourceUseType##'
	FROM SourceUseType;

cache SELECT distinct fuelTypeID  INTO OUTFILE '##NO2CopyOfFuelType##'
	FROM FuelType;

cache SELECT polProcessID,sourceTypeID,fuelTypeID,modelYearGroupID,NOxRatio,NOxRatioCV,dataSourceId
INTO OUTFILE '##NO2CopyOfNONO2Ratio##' FROM NONO2Ratio 
WHERE polProcessID IN (3301, 3302, 3390, 3391);

cache SELECT polProcessID,processID,pollutantID
INTO OUTFILE '##NO2CopyOfPPA##' FROM pollutantprocessassoc
WHERE processID=##context.iterProcess.databaseKey##
AND pollutantID=33;

cache SELECT polProcessID,modelYearID,modelYearGroupID,fuelMYGroupID
INTO OUTFILE '##NO2CopyOfPPMY##' FROM pollutantprocessmappedmodelyear 
WHERE polProcessID IN (3301, 3302, 3390, 3391)
and modelYearID <= ##context.year##
and modelYearID >= ##context.year## - 40;
-- End Section Extract Data

-- Section Local Data Removal
-- End Section Local Data Removal

-- Section Processing
DROP TABLE IF EXISTS NO2Calculation1;
CREATE TABLE NO2Calculation1 (
	polProcessID			int,
	processID				smallint(6),
	pollutantID				smallint(6),
	sourceTypeID			smallint(6),
	fuelTypeID				smallint(6),
	modelYearID				smallint(6),
	NOxRatio				float
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- @algorithm To simplify future table joins, add dimensions to NOxRatio.
INSERT INTO NO2Calculation1 (
	polProcessID,
	processID,
	pollutantID,
	sourceTypeID,
	fuelTypeID,
	modelYearID,
	NOxRatio     ) 
SELECT 
	nnr.polProcessID,
	ppa.processID,
	ppa.pollutantID,
	nnr.sourceTypeID,
	nnr.fuelTypeID,
	ppmy.modelYearID,
	nnr.NOxRatio 
FROM 	NO2CopyOfNONO2Ratio nnr  
		INNER JOIN 	NO2CopyOfPPA  ppa 	 	 	ON nnr.polProcessID = ppa.polProcessID 
		INNER JOIN 	NO2CopyOfSourceUseType ns 	ON nnr.sourceTypeID = ns.sourceTypeID
		INNER JOIN 	NO2CopyOfPPMY ppmy		 	ON nnr.modelYearGroupID = ppmy.modelYearGroupID 
		                                     		AND ppa.polProcessID = ppmy.polProcessID;

create index index1 on NO2Calculation1 (processID, sourceTypeID, pollutantID, modelYearID, fuelTypeID);

CREATE INDEX NO2Calculation1_New1 ON NO2Calculation1 (
	fuelTypeID ASC,
	modelyearID ASC,
	sourceTypeID ASC
);

DROP TABLE IF EXISTS NO2MOVESOutputTemp1;

-- @algorithm emissionQuant = NOxRatio * Oxides of Nitrogen (3).
CREATE TABLE NO2MOVESOutputTemp1
SELECT 
	mwo.MOVESRunID, mwo.iterationID, mwo.yearID, mwo.monthID, mwo.dayID, 
	mwo.hourID, mwo.stateID, mwo.countyID, mwo.zoneID, 
	mwo.linkID, noc.pollutantID, noc.processID, 
	noc.sourceTypeID, mwo.regClassID, mwo.fuelTypeID, mwo.modelYearID, 
	mwo.roadTypeID, mwo.SCC,
	mwo.emissionQuant as NOx,
	noc.NOxRatio,
	(noc.NOxRatio * mwo.emissionQuant) as emissionQuant,
	(noc.NOxRatio * mwo.emissionRate) as emissionRate
FROM
	MOVESWorkerOutput mwo, NO2Calculation1 noc  
WHERE  
	mwo.fuelTypeID			=	noc.fuelTypeID		AND 
	mwo.modelyearID			=	noc.modelyearID		AND
	mwo.sourceTypeID		=	noc.sourceTypeID	AND 
	mwo.pollutantID = 3 	AND
	mwo.processID = ##context.iterProcess.databaseKey##;

INSERT INTO MOVESWorkerOutput ( 
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant,emissionRate) 
SELECT 
	MOVESRunID,iterationID, yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant,emissionRate
FROM NO2MOVESOutputTemp1;
-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS NO2CopyOfSourceUseType;
DROP TABLE IF EXISTS NO2MOVESOutputTemp1;
DROP TABLE IF EXISTS NO2Calculation1;
DROP TABLE IF EXISTS NO2CopyOfNONO2Ratio;
DROP TABLE IF EXISTS NO2CopyOfFuelType;
DROP TABLE IF EXISTS NO2CopyOfPPA;
DROP TABLE IF EXISTS NO2CopyOfPPMY;
-- End Section Cleanup
