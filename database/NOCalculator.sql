-- Author Wesley Faler
-- Author Ed Glover EPA
-- Version 2014-08-20

-- @algorithm
-- @owner NO Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data
DROP TABLE IF EXISTS NOCopyOfSourceUseType;
CREATE TABLE NOCopyOfSourceUseType (
	sourceTypeID	smallint(6)
);

DROP TABLE IF EXISTS NOCopyOfPPA;
CREATE TABLE NOCopyOfPPA (
	polProcessID	int,
	processID		smallint(6),	
	pollutantID		smallint(6)
);

DROP TABLE IF EXISTS NOCopyOfPPMY;
CREATE TABLE NOCopyOfPPMY (
	polProcessID		int,
	modelYearID			smallint(6),	
	modelYearGroupID	int(11),
	fuelMYGroupID		int(11)
);

DROP TABLE IF EXISTS NOCopyOfFuelType;
CREATE TABLE NOCopyOfFuelType (
       fuelTypeID        smallint(6)
);

DROP TABLE IF EXISTS NOCopyOfNONO2Ratio;
CREATE TABLE NOCopyOfNONO2Ratio (
	polProcessID		int,
	sourceTypeID		smallint(6),
	fuelTypeID			smallint(6),
	modelYearGroupID	int(11),
	NOxRatio 			float,
	NOxRatioCV			float,
	dataSourceId		smallint(6)
);
-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data
cache SELECT distinct sourceTypeID  INTO OUTFILE '##NOCopyOfSourceUseType##'
	FROM SourceUseType;

cache SELECT distinct fuelTypeID  INTO OUTFILE '##NOCopyOfFuelType##'
	FROM FuelType;

cache SELECT polProcessID,sourceTypeID,fuelTypeID,modelYearGroupID,NOxRatio,NOxRatioCV,dataSourceId
INTO OUTFILE '##NOCopyOfNONO2Ratio##' FROM NONO2Ratio
WHERE polProcessID IN (##pollutantProcessIDs##);

cache SELECT polProcessID,processID,pollutantID
INTO OUTFILE '##NOCopyOfPPA##' FROM pollutantprocessassoc 
WHERE processID=##context.iterProcess.databaseKey##
AND pollutantID in (##pollutantIDs##);

cache SELECT polProcessID,modelYearID,modelYearGroupID,fuelMYGroupID
INTO OUTFILE '##NOCopyOfPPMY##' FROM pollutantprocessmappedmodelyear 
WHERE polProcessID IN (##pollutantProcessIDs##)
and modelYearID <= ##context.year##
and modelYearID >= ##context.year## - 30;
-- End Section Extract Data

-- Section Local Data Removal
-- End Section Local Data Removal

-- Section Processing
DROP TABLE IF EXISTS NOCalculation1;

CREATE TABLE NOCalculation1 (
	polProcessID			int,
	processID				smallint(6),
	pollutantID				smallint(6),
	sourceTypeID			smallint(6),
	fuelTypeID				smallint(6),
	modelYearID				smallint(6),
	NOxRatio				float
);

-- @algorithm To simplify future table joins, add dimensions to NOxRatio.
INSERT INTO NOCalculation1 (
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
FROM 	NOCopyOfNONO2Ratio nnr  
		INNER JOIN 	NOCopyOfPPA  ppa 	 	 	ON nnr.polProcessID = ppa.polProcessID 
		INNER JOIN 	NOCopyOfSourceUseType ns 	ON nnr.sourceTypeID = ns.sourceTypeID
		INNER JOIN 	NOCopyOfPPMY ppmy		 	ON nnr.modelYearGroupID = ppmy.modelYearGroupID 
		                                     		AND ppa.polProcessID = ppmy.polProcessID;

create index index1 on NOCalculation1 (processID, sourceTypeID, pollutantID, modelYearID, fuelTypeID);

CREATE INDEX MOVESWorkerOutput_New1 ON MOVESWorkerOutput (
	fuelTypeID ASC,
	modelyearID ASC,
	sourceTypeID ASC,
	pollutantID ASC,
	processID ASC
);
CREATE INDEX NOCalculation1_New1 ON NOCalculation1 (
	fuelTypeID ASC,
	modelyearID ASC,
	sourceTypeID ASC
);

DROP TABLE IF EXISTS NOMOVESOutputTemp1;

-- @algorithm emissionQuant = NOxRatio * Oxides of Nitrogen (3).
CREATE TABLE NOMOVESOutputTemp1
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
	MOVESWorkerOutput mwo, NOCalculation1 noc  
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
FROM NOMOVESOutputTemp1;

alter table MOVESWorkerOutput drop index MOVESWorkerOutput_New1;
-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS NOCopyOfSourceUseType;
DROP TABLE IF EXISTS NOMOVESOutputTemp1;
DROP TABLE IF EXISTS NOCalculation1;
DROP TABLE IF EXISTS NOCopyOfNONO2Ratio;
DROP TABLE IF EXISTS NOCopyOfFuelType;
DROP TABLE IF EXISTS NOCopyOfPPA;
DROP TABLE IF EXISTS NOCopyOfPPMY;
-- End Section Cleanup
