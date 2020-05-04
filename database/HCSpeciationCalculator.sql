-- Version 2014-12-11
-- Wes Faler

-- @algorithm
-- @owner HC Speciation Calculator
-- @calculator
-- @fileNotUsed

-- Section Create Remote Tables for Extracted Data

-- Section OldCode
drop table if exists extagecategory;
create table extagecategory (
  ageID smallint(6) NOT NULL DEFAULT '0',
  ageGroupID smallint(6) NOT NULL DEFAULT '0',
  PRIMARY KEY (ageID),
  KEY ageGroupID (ageGroupID,ageID),
  KEY ageID (ageID,ageGroupID)
);

drop table if exists extfuelsupply;
create table extfuelsupply (
	countyID smallint not null,
	yearID smallint not null,
	monthID smallint not null,
	fuelTypeID smallint not null,
	fuelSubTypeID smallint not null,
	fuelFormulationID int not null,
	marketShare double not null
);

drop table if exists extfueltype;
create table extfueltype (
       fuelTypeID           SMALLINT NOT NULL,
       humidityCorrectionCoeff FLOAT NULL,
 	   fuelDensity				FLOAT	NULL,
 	   subjectToEvapCalculations CHAR(1) NOT NULL DEFAULT 'N'
);

drop table if exists extfuelsubtype;
create table extfuelsubtype (
       fuelSubtypeID        SMALLINT NOT NULL,
       fuelTypeID           SMALLINT NOT NULL,
       fuelSubtypePetroleumFraction FLOAT NULL,
       fuelSubtypeFossilFraction FLOAT NULL,
       carbonContent        FLOAT NULL,
       oxidationFraction    FLOAT NULL,
	   energyContent		FLOAT NULL,
       key (fuelTypeID, fuelSubtypeID)
);

drop table if exists extfuelformulation;
create table extfuelformulation (
    fuelFormulationID SMALLINT NOT NULL PRIMARY KEY,
    fuelSubtypeID SMALLINT NOT NULL,
    RVP FLOAT NULL,
    sulfurLevel FLOAT NULL,
    ETOHVolume FLOAT NULL,
    MTBEVolume FLOAT NULL,
    ETBEVolume FLOAT NULL,
    TAMEVolume FLOAT NULL,
    aromaticContent FLOAT NULL,
    olefinContent FLOAT NULL,
    benzeneContent FLOAT NULL,
    e200 FLOAT NULL,
    e300 FLOAT NULL,
	volToWtPercentOxy FLOAT NULL,
	BioDieselEsterVolume float DEFAULT NULL,
	CetaneIndex float DEFAULT NULL,
	PAHContent float DEFAULT NULL,
	T50 float DEFAULT NULL,
	T90 float DEFAULT NULL,
	key (fuelSubTypeID, fuelFormulationID)
);

-- End Section OldCode

drop table if exists HCAgeCategory;
CREATE TABLE HCAgeCategory (
  ageID smallint(6) NOT NULL DEFAULT '0',
  ageGroupID smallint(6) NOT NULL DEFAULT '0',
  ageCategoryName char(50) DEFAULT NULL,
  PRIMARY KEY (ageID),
  KEY ageGroupID (ageGroupID,ageID),
  KEY ageID (ageID,ageGroupID)
);

drop table if exists HCETOHBin;
CREATE TABLE IF NOT EXISTS HCETOHBin (
  etohThreshID smallint(6) NOT NULL DEFAULT '0',
  etohThreshLow float DEFAULT NULL,
  etohThreshHigh float DEFAULT NULL,
  etohNominalValue float DEFAULT NULL,
  PRIMARY KEY (etohThreshID),
  key (etohThreshLow, etohThreshHigh, etohThreshID)
);

drop table if exists HCFuelSupply;
CREATE TABLE IF NOT EXISTS HCFuelSupply (
  countyID int(11) NOT NULL,
  monthID smallint(6) NOT NULL,
  fuelFormulationID smallint(6) NOT NULL,
  marketShare float default NULL,
  yearID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  fuelSubtypeID smallint(6) NOT NULL,
  KEY (countyID,yearID,monthID,fuelTypeID,fuelSubtypeID,fuelFormulationID)
);
TRUNCATE TABLE HCFuelSupply;

drop table if exists HCFuelFormulation;
CREATE TABLE IF NOT EXISTS HCFuelFormulation (
    fuelFormulationID SMALLINT NOT NULL PRIMARY KEY,
    fuelSubtypeID SMALLINT NOT NULL,
    RVP FLOAT NULL,
    sulfurLevel FLOAT NULL,
    ETOHVolume FLOAT NULL,
    MTBEVolume FLOAT NULL,
    ETBEVolume FLOAT NULL,
    TAMEVolume FLOAT NULL,
    aromaticContent FLOAT NULL,
    olefinContent FLOAT NULL,
    benzeneContent FLOAT NULL,
    e200 FLOAT NULL,
    e300 FLOAT NULL,
	volToWtPercentOxy FLOAT NULL,
	BioDieselEsterVolume float DEFAULT NULL,
	CetaneIndex float DEFAULT NULL,
	PAHContent float DEFAULT NULL,
	oxyThreshID int null,
	key (fuelFormulationID, fuelSubtypeID, oxyThreshID),
	key (fuelSubtypeID, fuelFormulationID, oxyThreshID),
	key (fuelSubtypeID, oxyThreshID, fuelFormulationID),
	key (oxyThreshID, fuelSubtypeID, fuelFormulationID)
);
TRUNCATE TABLE HCFuelFormulation;

drop table if exists HCOxyThreshName;
CREATE TABLE IF NOT EXISTS HCOxyThreshName 
(
	oxyThreshID				smallint(6)		NOT NULL	default '0' primary key
);
TRUNCATE TABLE HCOxyThreshName;

drop table if exists HCspeciation;
##create.HCspeciation##;
TRUNCATE TABLE HCspeciation;

drop table if exists HCPollutantProcessModelYear;
CREATE TABLE IF NOT EXISTS HCPollutantProcessModelYear (
    polProcessID int NOT NULL ,
    modelYearID SMALLINT NOT NULL ,
    modelYearGroupID INT NOT NULL ,
    fuelMYGroupID INTEGER NULL,
    IMModelYearGroupID INTEGER NULL,
    key (polProcessID),
    key (modelYearID),
    key (fuelMYGroupID),
    key (polProcessID, modelYearID, fuelMYGroupID)
);
truncate table HCPollutantProcessModelYear;

drop table if exists HCPollutantProcessMappedModelYear;
CREATE TABLE IF NOT EXISTS HCPollutantProcessMappedModelYear (
    polProcessID int NOT NULL ,
    modelYearID SMALLINT NOT NULL ,
    modelYearGroupID INT NOT NULL ,
    fuelMYGroupID INTEGER NULL,
    IMModelYearGroupID INTEGER NULL,
    key (polProcessID),
    key (modelYearID),
    key (fuelMYGroupID),
    key (polProcessID, modelYearID, fuelMYGroupID)
);
truncate table HCPollutantProcessMappedModelYear;

drop table if exists THCPollutantProcessModelYear;
CREATE TABLE IF NOT EXISTS THCPollutantProcessModelYear (
    polProcessID int NOT NULL ,
    modelYearID SMALLINT NOT NULL ,
    modelYearGroupID INT NOT NULL ,
    fuelMYGroupID INTEGER NULL,
    IMModelYearGroupID INTEGER NULL,
    key (polProcessID),
    key (modelYearID),
    key (fuelMYGroupID),
    key (polProcessID, modelYearID, fuelMYGroupID)
);
truncate table THCPollutantProcessModelYear;

drop table if exists THCPollutantProcessMappedModelYear;
CREATE TABLE IF NOT EXISTS THCPollutantProcessMappedModelYear (
    polProcessID int NOT NULL ,
    modelYearID SMALLINT NOT NULL ,
    modelYearGroupID INT NOT NULL ,
    fuelMYGroupID INTEGER NULL,
    IMModelYearGroupID INTEGER NULL,
    key (polProcessID),
    key (modelYearID),
    key (fuelMYGroupID),
    key (polProcessID, modelYearID, fuelMYGroupID)
);
truncate table THCPollutantProcessMappedModelYear;

drop table if exists HCPollutantProcessAssoc;
CREATE TABLE IF NOT EXISTS HCPollutantProcessAssoc (
       polProcessID         int NOT NULL,
       processID            SMALLINT NOT NULL,
       pollutantID          SMALLINT NOT NULL,
	   isAffectedByExhaustIM CHAR(1) NOT NULL DEFAULT "N",
       isAffectedByEvapIM CHAR(1) NOT NULL DEFAULT "N",
       chainedto1 int NULL DEFAULT NULL,
       chainedto2 int NULL DEFAULT NULL,
       key (processID),
       key (pollutantID),
       key (polProcessID),
       key (polProcessID, processID, pollutantID),
       key (pollutantID, processID, polProcessID)
);
truncate table HCPollutantProcessAssoc;

##create.methaneTHCRatio##;
TRUNCATE TABLE methaneTHCRatio;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

-- Section OldCode
cache select ageID, ageGroupID
into outfile '##extagecategory##'
from AgeCategory;

cache select ##context.iterLocation.countyRecordID##, ##context.year##, ##context.monthID##, 
		fst.fuelTypeID, fst.fuelSubTypeID, ff.fuelFormulationID, fs.marketShare
into outfile '##extfuelsupply##'
from year
inner join fuelSupply fs on (fs.fuelYearID=year.fuelYearID)
inner join monthOfAnyYear moay on (moay.monthGroupID=fs.monthGroupID)
inner join fuelFormulation ff on (ff.fuelFormulationID=fs.fuelFormulationID)
inner join fuelSubtype fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)
where yearID = ##context.year##
and fs.fuelRegionID = ##context.fuelRegionID##
and moay.monthID = ##context.monthID##
and fst.fuelTypeID in (##macro.csv.all.fuelTypeID##);

cache select fuelTypeID, humidityCorrectionCoeff, fuelDensity, subjectToEvapCalculations
into outfile '##extfueltype##'
from fuelType;

cache select fuelSubtypeID, fuelTypeID, fuelSubtypePetroleumFraction, fuelSubtypeFossilFraction,
	carbonContent, oxidationFraction, energyContent
into outfile '##extfuelsubtype##'
from fuelSubtype;

cache select distinct
	FuelFormulation.fuelFormulationID,
	FuelFormulation.fuelSubtypeID,
	ifnull(FuelFormulation.RVP,0),
	ifnull(FuelFormulation.sulfurLevel,0),
	ifnull(FuelFormulation.ETOHVolume,0),
	ifnull(FuelFormulation.MTBEVolume,0),
	ifnull(FuelFormulation.ETBEVolume,0),
	ifnull(FuelFormulation.TAMEVolume,0),
	ifnull(FuelFormulation.aromaticContent,0),
	ifnull(FuelFormulation.olefinContent,0),
	ifnull(FuelFormulation.benzeneContent,0),
	ifnull(FuelFormulation.e200,0),
	ifnull(FuelFormulation.e300,0),
	ifnull(FuelFormulation.volToWtPercentOxy,0),
	ifnull(FuelFormulation.BioDieselEsterVolume,0),
	ifnull(FuelFormulation.CetaneIndex,0),
	ifnull(FuelFormulation.PAHContent,0),
	ifnull(FuelFormulation.T50,0),
	ifnull(FuelFormulation.T90,0)
INTO OUTFILE '##extfuelformulation##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN FuelFormulation ON (FuelFormulation.fuelFormulationID = FuelSupply.fuelFormulationID)
INNER JOIN FuelSubtype ON (FuelSubtype.fuelSubtypeID = FuelFormulation.fuelSubtypeID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)
INNER JOIN RunSpecMonth ON (RunSpecMonth.monthID = MonthOfAnyYear.monthID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND MonthOfAnyYear.monthID = ##context.monthID##
AND FuelSubtype.fuelTypeID in (##macro.csv.all.fuelTypeID##);

-- End Section OldCode

-- -----------------------------


cache SELECT *
INTO OUTFILE '##HCAgeCategory##'
FROM AgeCategory;

cache SELECT *
INTO OUTFILE '##HCETOHBin##'
FROM ETOHBin;

cache SELECT oxyThreshID
INTO OUTFILE '##HCOxyThreshName##'
FROM OxyThreshName;

cache SELECT ##context.iterLocation.countyRecordID## as countyID, MonthOfAnyYear.monthID, FuelSupply.fuelFormulationID, FuelSupply.marketShare, Year.yearID, FuelSubtype.fuelTypeID, FuelSubtype.fuelSubtypeID
INTO OUTFILE '##HCFuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN FuelFormulation ON (FuelFormulation.fuelFormulationID = FuelSupply.fuelFormulationID)
INNER JOIN FuelSubtype ON (FuelSubtype.fuelSubtypeID = FuelFormulation.fuelSubtypeID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)
INNER JOIN RunSpecMonth ON (RunSpecMonth.monthID = MonthOfAnyYear.monthID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##;

cache SELECT polProcessID, modelYearID, modelYearGroupID, fuelMYGroupID, IMModelYearGroupID
INTO OUTFILE '##HCPollutantProcessModelYear##'
FROM PollutantProcessModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND polProcessID IN (##hcPolProcessIDs##);

cache SELECT polProcessID, modelYearID, modelYearGroupID, fuelMYGroupID, IMModelYearGroupID
INTO OUTFILE '##HCPollutantProcessMappedModelYear##'
FROM PollutantProcessMappedModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND polProcessID IN (##hcPolProcessIDs##);

cache SELECT ppmy.polProcessID, ppmy.modelYearID, ppmy.modelYearGroupID, ppmy.fuelMYGroupID, ppmy.IMModelYearGroupID
INTO OUTFILE '##THCPollutantProcessModelYear##'
FROM PollutantProcessModelYear ppmy
INNER JOIN PollutantProcessAssoc ppa using (polProcessID)
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND pollutantID = 1;

cache SELECT ppmy.polProcessID, ppmy.modelYearID, ppmy.modelYearGroupID, ppmy.fuelMYGroupID, ppmy.IMModelYearGroupID
INTO OUTFILE '##THCPollutantProcessMappedModelYear##'
FROM PollutantProcessMappedModelYear ppmy
INNER JOIN PollutantProcessAssoc ppa using (polProcessID)
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND pollutantID = 1;

cache select HCSpeciation.*
into outfile '##HCspeciation##'
from HCSpeciation
where polProcessID in (##hcPolProcessIDs##);

cache select polProcessID,processID,pollutantID,isAffectedByExhaustIM,isAffectedByEvapIM,chainedto1,chainedto2
into outfile '##HCPollutantProcessAssoc##'
from PollutantProcessAssoc
where polProcessID in (##hcPolProcessIDs##)
or pollutantID = 1;

cache SELECT DISTINCT
	FuelFormulation.fuelFormulationID,
	FuelFormulation.fuelSubtypeID,
	ifnull(FuelFormulation.RVP,0),
	ifnull(FuelFormulation.sulfurLevel,0),
	ifnull(FuelFormulation.ETOHVolume,0),
	ifnull(FuelFormulation.MTBEVolume,0),
	ifnull(FuelFormulation.ETBEVolume,0),
	ifnull(FuelFormulation.TAMEVolume,0),
	ifnull(FuelFormulation.aromaticContent,0),
	ifnull(FuelFormulation.olefinContent,0),
	ifnull(FuelFormulation.benzeneContent,0),
	ifnull(FuelFormulation.e200,0),
	ifnull(FuelFormulation.e300,0),
	ifnull(FuelFormulation.volToWtPercentOxy,0),
	ifnull(FuelFormulation.BioDieselEsterVolume,0),
	ifnull(FuelFormulation.CetaneIndex,0),
	ifnull(FuelFormulation.PAHContent,0),
	0 as oxyThreshID
INTO OUTFILE '##HCFuelFormulation##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN FuelFormulation ON (FuelFormulation.fuelFormulationID = FuelSupply.fuelFormulationID)
INNER JOIN FuelSubtype ON (FuelSubtype.fuelSubtypeID = FuelFormulation.fuelSubtypeID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)
INNER JOIN RunSpecMonth ON (RunSpecMonth.monthID = MonthOfAnyYear.monthID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##;

cache select methaneTHCRatio.*
into outfile '##methaneTHCRatio##'
from methaneTHCRatio
where processID in (##hcProcessIDs##);

-- End Section Extract Data

-- Section Processing

starttimer savemwo;
savemwo;
starttimer HCSpeciationCalculator;

update HCFuelFormulation set oxyThreshID = (
##oxyThreshCase##
);

alter table HCFuelFormulation add etohThreshID smallint(6) NULL default '0';

-- @algorithm Assign etohThreshID to each fuel formulation.
-- etohThreshLow <= ETOHVolume < etohThreshHigh
update HCFuelFormulation, HCETOHBin set HCFuelFormulation.etohThreshID = HCETOHBin.etohThreshID
where etohThreshLow <= ETOHVolume and ETOHVolume < etohThreshHigh;

-- alter table MOVESWorkerOutput add key HCPollutantID (pollutantID);

-- @algorithm Fill in missing HCSpeciation entries so that joins to the table are valid.
-- Use speciationConstant of 0 and oxySpeciation of 0 for missing entries.
insert ignore into HCSpeciation (polProcessID, fuelMYGroupID, fuelSubtypeID, etohThreshID, oxyThreshID, speciationConstant, oxySpeciation)
select distinct ppmy.polProcessID, ppmy.fuelMYGroupID, fs.fuelSubtypeID, etohThreshID, oxyThreshID, 0.0 as speciationConstant, 0.0 as oxySpeciation
from HCPollutantProcessModelYear ppmy,
HCFuelSupply fs,
HCETOHBin,
HCOxyThreshName;

drop table if exists HCWorkerOutput;
CREATE TABLE IF NOT EXISTS HCWorkerOutput (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL DEFAULT 0,
	iterationID			 SMALLINT UNSIGNED DEFAULT 1,
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
	fuelSubtypeID		 SMALLINT UNSIGNED NULL,
	fuelFormulationID	 SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,
	roadTypeID           SMALLINT UNSIGNED NULL,
	SCC                  CHAR(10) NULL,
	regClassID			 SMALLINT UNSIGNED NULL,
	emissionQuant        FLOAT NULL,
	emissionRate		 FLOAT NULL,
	
	key (yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
			processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,regClassID)
);
truncate table HCWorkerOutput;

drop table if exists HCWorkerOutputAll;

-- @algorithm HCWorkerOutputAll holds all data generated by HC speciation, plus THC data used for Methane and NMHC.
-- It is for quick lookups, avoiding long scans of MOVESWorkerOutput.  Data is first placed into HCWorkerOutput
-- then copied to both MOVESWorkerOutput and to HCWorkerOutputAll.
CREATE TABLE IF NOT EXISTS HCWorkerOutputAll (
	MOVESRunID           SMALLINT UNSIGNED NOT NULL DEFAULT 0,
	iterationID			 SMALLINT UNSIGNED DEFAULT 1,
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
	fuelSubtypeID		 SMALLINT UNSIGNED NULL,
	fuelFormulationID	 SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,
	roadTypeID           SMALLINT UNSIGNED NULL,
	SCC                  CHAR(10) NULL,
	regClassID			 SMALLINT UNSIGNED NULL,
	emissionQuant        FLOAT NULL,
	emissionRate		 FLOAT NULL,

	key (pollutantID),
	key (pollutantID, processID),
	key (
		pollutantID asc,
		fuelTypeID asc,
		countyID asc, 
		yearID asc,
		monthID asc,
		modelYearID asc, 
		processID asc)
);
truncate table HCWorkerOutputAll;

-- CREATE INDEX MOVESWorkerOutput_A2 ON MOVESWorkerOutput (
-- 	pollutantID asc,
-- 	fuelTypeID asc,
-- 	countyID asc, 
-- 	yearID asc,
-- 	monthID asc,
-- 	modelYearID asc, 
-- 	processID asc
-- );

CREATE INDEX HCFuelSupply_A1 ON HCFuelSupply (
	fuelTypeID asc, 
	countyID asc, 
	yearID asc, 
	monthID asc, 
	fuelFormulationID asc
);

CREATE INDEX HCspeciation_A1 ON HCspeciation (
	oxyThreshID asc, 
	fuelSubtypeID asc, 
	etohThreshID asc, 
	polProcessID asc, 
	fuelMYGroupID asc
);

CREATE INDEX HCETOHBin_A1 ON HCETOHBin (
	etohThreshID asc, 
	etohThreshLow asc, 
	etohThreshHigh asc
);

CREATE INDEX HCPollutantProcessAssoc_A1 ON HCPollutantProcessAssoc (
	processID asc, 
	polProcessID asc, 
	pollutantID asc
);

create index HCFuelFormulation_A1 on HCFuelFormulation (
	fuelFormulationID asc,
	fuelSubtypeID asc
);

-- @algorithm Extract THC (1) and altTHC (10001) into HCWorkerOutputAll to make tables faster to search
insert into HCWorkerOutputAll (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,
	regClassID,emissionRate,
	fuelFormulationID,fuelSubtypeID
)
select MOVESRunID,iterationID,mwo.yearID,mwo.monthID,dayID,hourID,stateID,mwo.countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,mwo.fuelTypeID,modelYearID,roadTypeID,SCC,marketShare*emissionQuant,
	regClassID,marketShare*emissionRate,
	fuelFormulationID,fuelSubtypeID
from MOVESWorkerOutput mwo
inner join HCFuelSupply fs using (countyID, monthID, fuelTypeID, yearID)
where pollutantID in (1, 10001);

-- Section Methane
truncate HCWorkerOutput;

-- @algorithm methane = THC * CH4THCRatio
insert into HCWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,
	emissionRate,
	regClassID,fuelFormulationID,fuelSubtypeID
)
select mwo.MOVESRunID,mwo.iterationID,mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	5 as pollutantID,
	mwo.processID,mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID,mwo.SCC,
	(emissionQuant * CH4THCRatio) as emissionQuant,
	(emissionRate * CH4THCRatio) as emissionRate,
	mwo.regClassID,mwo.fuelFormulationID,mwo.fuelSubtypeID
from HCWorkerOutputAll mwo
inner join HCAgeCategory acat on (mwo.modelYearID=##context.year##-acat.ageID)
inner join HCPollutantProcessAssoc ppa on (
	ppa.processID=mwo.processID
	and ppa.pollutantID=1)
inner join THCPollutantProcessMappedModelYear ppmy on (
	ppmy.polProcessID=ppa.polProcessID
	and ppmy.modelYearID=mwo.modelYearID)
inner join methaneTHCRatio r on (
	r.processID = mwo.processID
	and r.fuelTypeID = mwo.fuelTypeID
	and r.sourceTypeID = mwo.sourceTypeID
	and r.modelYearGroupID = ppmy.modelYearGroupID
	and r.ageGroupID = acat.ageGroupID)
where mwo.pollutantID = 1
and mwo.processID in (##methaneProcessIDs##);

-- Move values back into MOVESWorkerOutput
insert into MOVESWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	sum(emissionQuant),sum(emissionRate)
from HCWorkerOutput
where emissionQuant >= 0
group by yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,regClassID;

insert into HCWorkerOutputAll (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate,
	fuelFormulationID,fuelSubtypeID
)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate,
	fuelFormulationID,fuelSubtypeID
from HCWorkerOutput
where emissionQuant >= 0;
-- End Section Methane

-- Section NMHC
truncate HCWorkerOutput;

-- @algorithm NMHC = THC * (1-CH4THCRatio)
insert into HCWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,
	emissionRate,
	regClassID,fuelFormulationID,fuelSubtypeID
)
select mwo.MOVESRunID,mwo.iterationID,mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	79 as pollutantID,
	mwo.processID,mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID,mwo.SCC,
	(emissionQuant * (1-CH4THCRatio)) as emissionQuant,
	(emissionRate * (1-CH4THCRatio)) as emissionRate,
	regClassID,fuelFormulationID,fuelSubtypeID
from HCWorkerOutputAll mwo
inner join HCAgeCategory acat on (mwo.modelYearID=##context.year##-acat.ageID)
inner join HCPollutantProcessAssoc ppa on (
	ppa.processID=mwo.processID
	and ppa.pollutantID=1)
inner join THCPollutantProcessMappedModelYear ppmy on (
	ppmy.polProcessID=ppa.polProcessID
	and ppmy.modelYearID=mwo.modelYearID)
inner join methaneTHCRatio r on (
	r.processID = mwo.processID
	and r.fuelTypeID = mwo.fuelTypeID
	and r.sourceTypeID = mwo.sourceTypeID
	and r.modelYearGroupID = ppmy.modelYearGroupID
	and r.ageGroupID = acat.ageGroupID)
where mwo.pollutantID = 1
and mwo.processID in (##nmhcProcessIDs##);

-- and not (mwo.processID in (1,2) and mwo.fuelTypeID=5 
--	and mwo.fuelSubtypeID in (??e85E70FuelSubtypeIDs??) and mwo.modelYearID >= 2001)

-- @algorithm Calculate altNMHC (10079) from altTHC (10001) using E10's ratios.
-- altNMHC (pollutant 10079) = altTHC (10001) * (1-CH4THCRatio[E10 fuel subtype]).
-- @condition Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001.
insert into HCWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,
	emissionRate,
	regClassID,fuelFormulationID,fuelSubtypeID
)
select mwo.MOVESRunID,mwo.iterationID,mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	10079 as pollutantID,
	mwo.processID,mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID,mwo.SCC,
	(emissionQuant * (1-CH4THCRatio)) as emissionQuant,
	(emissionRate * (1-CH4THCRatio)) as emissionRate,
	regClassID,fuelFormulationID,fuelSubtypeID
from HCWorkerOutputAll mwo
inner join HCAgeCategory acat on (mwo.modelYearID=##context.year##-acat.ageID)
inner join HCPollutantProcessAssoc ppa on (
	ppa.processID=mwo.processID
	and ppa.pollutantID=1)
inner join THCPollutantProcessMappedModelYear ppmy on (
	ppmy.polProcessID=ppa.polProcessID
	and ppmy.modelYearID=mwo.modelYearID)
inner join methaneTHCRatio r on (
	r.processID = mwo.processID
	and r.fuelTypeID = 1
	and r.sourceTypeID = mwo.sourceTypeID
	and r.modelYearGroupID = ppmy.modelYearGroupID
	and r.ageGroupID = acat.ageGroupID)
where mwo.pollutantID = 10001
and mwo.processID in (##nmhcProcessIDs##)
and (mwo.processID in (1,2) and mwo.fuelTypeID=5 
	and mwo.fuelSubtypeID in (##e85E70FuelSubtypeIDs##) and mwo.modelYearID >= 2001);

-- Move values back into MOVESWorkerOutput
insert into MOVESWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	sum(emissionQuant),sum(emissionRate)
from HCWorkerOutput
where emissionQuant >= 0
group by yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,regClassID;

insert into HCWorkerOutputAll (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate,
	fuelFormulationID,fuelSubtypeID
)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate,
	fuelFormulationID,fuelSubtypeID
from HCWorkerOutput
where emissionQuant >= 0;
-- End Section NMHC

-- Section NMOG
truncate HCWorkerOutput;

-- @algorithm NMOG = NMHC*(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume)).
-- @condition When (MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume) >= 0. Otherwise, NMOG = 0.
-- @condition NOT (Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001).
insert into HCWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,
	regClassID,fuelFormulationID,fuelSubtypeID,
	emissionQuant,emissionRate
)
select mwo.MOVESRunID,mwo.iterationID,mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	ppa.pollutantID,
	mwo.processID,mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID,mwo.SCC,
	mwo.regClassID,mwo.fuelFormulationID,mwo.fuelSubtypeID,
	emissionQuant*(
		(case when (MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume) >= 0 then
			(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume))
		 else 0 end)
	) as emissionQuant,
	emissionRate*(
		(case when (MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume) >= 0 then
			(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume))
		 else 0 end)
	) as emissionRate
from HCWorkerOutputAll mwo
inner join HCFuelFormulation ff on (ff.fuelFormulationID=mwo.fuelFormulationID)
inner join HCspeciation hcs on (hcs.oxyThreshID=ff.oxyThreshID and hcs.fuelSubtypeID=ff.fuelSubtypeID and hcs.etohThreshID=ff.etohThreshID)
inner join HCPollutantProcessModelYear ppmy on (ppmy.polProcessID=hcs.polProcessID
	and ppmy.modelYearID=mwo.modelYearID and ppmy.fuelMYGroupID=hcs.fuelMYGroupID)
inner join HCPollutantProcessAssoc ppa on (ppa.processID=mwo.processID
	and ppa.polProcessID=ppmy.polProcessID
	and ppa.processID in (##nmogProcessIDs##)
	and ppa.pollutantID = 80)
where mwo.pollutantID = 79
and not (mwo.processID in (1,2) and mwo.fuelTypeID=5 
	and mwo.fuelSubtypeID in (##e85E70FuelSubtypeIDs##) and mwo.modelYearID >= 2001);

-- @algorithm Calculate NMOG from altNMHC (10079) that originates from altTHC (10001). Use E10's factors even though the fuel is Ethanol.
-- This is done by joining to HCSpeciation using E10's values rather than the current fuel formulation's values.
-- NMOG = altNMHC*(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+10)).
-- @condition Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001.
insert into HCWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,
	regClassID,fuelFormulationID,fuelSubtypeID,
	emissionQuant,emissionRate
)
select mwo.MOVESRunID,mwo.iterationID,mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	ppa.pollutantID,
	mwo.processID,mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID,mwo.SCC,
	mwo.regClassID,mwo.fuelFormulationID,mwo.fuelSubtypeID,
	emissionQuant*(
		(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+10))
	) as emissionQuant,
	emissionRate*(
		(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+10))
	) as emissionRate
from HCWorkerOutputAll mwo
inner join HCFuelFormulation ff on (ff.fuelFormulationID=mwo.fuelFormulationID)
inner join HCspeciation hcs on (hcs.oxyThreshID=0 and hcs.fuelSubtypeID=12 and hcs.etohThreshID=3)
inner join HCPollutantProcessModelYear ppmy on (ppmy.polProcessID=hcs.polProcessID
	and ppmy.modelYearID=mwo.modelYearID and ppmy.fuelMYGroupID=hcs.fuelMYGroupID)
inner join HCPollutantProcessAssoc ppa on (ppa.processID=mwo.processID
	and ppa.polProcessID=ppmy.polProcessID
	and ppa.processID in (##nmogProcessIDs##)
	and ppa.pollutantID = 80)
where mwo.pollutantID = 10079
and (mwo.processID in (1,2) and mwo.fuelTypeID=5 
	and mwo.fuelSubtypeID in (##e85E70FuelSubtypeIDs##) and mwo.modelYearID >= 2001);

-- Move values back into MOVESWorkerOutput
insert into MOVESWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	sum(emissionQuant),sum(emissionRate)
from HCWorkerOutput
where emissionQuant >= 0
group by yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,regClassID;

insert into HCWorkerOutputAll (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate,
	fuelFormulationID,fuelSubtypeID
)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate,
	fuelFormulationID,fuelSubtypeID
from HCWorkerOutput
where emissionQuant >= 0;
-- End Section NMOG

-- Section VOC
truncate HCWorkerOutput;

-- @algorithm VOC = NMHC*(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume)).
-- @condition When (MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume) >= 0. Otherwise, VOC = 0.
-- @condition NOT (Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001).
insert into HCWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,
	regClassID,fuelFormulationID,fuelSubtypeID,
	emissionQuant,emissionRate
)
select mwo.MOVESRunID,mwo.iterationID,mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	ppa.pollutantID,
	mwo.processID,mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID,mwo.SCC,
	mwo.regClassID,mwo.fuelFormulationID,mwo.fuelSubtypeID,
	emissionQuant*(
		(case when (MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume) >= 0 then
			(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume))
		 else 0 end)
	) as emissionQuant,
	emissionRate*(
		(case when (MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume) >= 0 then
			(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+ETOHVolume))
		 else 0 end)
	) as emissionRate
from HCWorkerOutputAll mwo
inner join HCFuelFormulation ff on (ff.fuelFormulationID=mwo.fuelFormulationID)
inner join HCspeciation hcs on (hcs.oxyThreshID=ff.oxyThreshID and hcs.fuelSubtypeID=ff.fuelSubtypeID and hcs.etohThreshID=ff.etohThreshID)
inner join HCPollutantProcessMappedModelYear ppmy on (ppmy.polProcessID=hcs.polProcessID
	and ppmy.modelYearID=mwo.modelYearID and ppmy.fuelMYGroupID=hcs.fuelMYGroupID)
inner join HCPollutantProcessAssoc ppa on (ppa.processID=mwo.processID
	and ppa.polProcessID=ppmy.polProcessID
	and ppa.processID in (##vocProcessIDs##)
	and ppa.pollutantID = 87)
where mwo.pollutantID = 79
and not (mwo.processID in (1,2) and mwo.fuelTypeID=5 
	and mwo.fuelSubtypeID in (##e85E70FuelSubtypeIDs##) and mwo.modelYearID >= 2001);

-- @algorithm Calculate VOC from altNMHC (10079) that originates from altTHC (10001). Use E10's factors even though the fuel is Ethanol.
-- This is done by joining to HCSpeciation using E10's values rather than the current fuel formulation's values.
-- VOC = altNMHC*(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+10)).
-- @condition Running Exhaust, Start Exhaust, Ethanol fuel type, E70 and E85 fuel subtypes, Model years >= 2001.
insert into HCWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,
	regClassID,fuelFormulationID,fuelSubtypeID,
	emissionQuant,emissionRate
)
select mwo.MOVESRunID,mwo.iterationID,mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	ppa.pollutantID,
	mwo.processID,mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID,mwo.SCC,
	mwo.regClassID,mwo.fuelFormulationID,mwo.fuelSubtypeID,
	emissionQuant*(
		(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+10))
	) as emissionQuant,
	emissionRate*(
		(speciationConstant + oxySpeciation* volToWtPercentOxy*(MTBEVolume+ETBEVolume+TAMEVolume+10))
	) as emissionRate
from HCWorkerOutputAll mwo
inner join HCFuelFormulation ff on (ff.fuelFormulationID=mwo.fuelFormulationID)
inner join HCspeciation hcs on (hcs.oxyThreshID=0 and hcs.fuelSubtypeID=12 and hcs.etohThreshID=3)
inner join HCPollutantProcessMappedModelYear ppmy on (ppmy.polProcessID=hcs.polProcessID
	and ppmy.modelYearID=mwo.modelYearID and ppmy.fuelMYGroupID=hcs.fuelMYGroupID)
inner join HCPollutantProcessAssoc ppa on (ppa.processID=mwo.processID
	and ppa.polProcessID=ppmy.polProcessID
	and ppa.processID in (##vocProcessIDs##)
	and ppa.pollutantID = 87)
where mwo.pollutantID = 10079
and (mwo.processID in (1,2) and mwo.fuelTypeID=5 
	and mwo.fuelSubtypeID in (##e85E70FuelSubtypeIDs##) and mwo.modelYearID >= 2001);

-- Move values back into MOVESWorkerOutput
insert into MOVESWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	sum(emissionQuant),sum(emissionRate)
from HCWorkerOutput
where emissionQuant >= 0
group by yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,regClassID;

-- VOC is not an input to further HC speciation calculations.
-- If so, uncomment the following.
-- insert into HCWorkerOutputAll (
-- 	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
-- 	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
-- 	emissionQuant,emissionRate,
-- 	fuelFormulationID,fuelSubtypeID
-- )
-- select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
-- 	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
-- 	emissionQuant,emissionRate,
-- 	fuelFormulationID,fuelSubtypeID
-- from HCWorkerOutput
-- where emissionQuant >= 0

-- End Section VOC

-- Section TOG
truncate HCWorkerOutput;
-- @algorithm TOG=NMOG+Methane
insert into HCWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,
	regClassID,fuelFormulationID,fuelSubtypeID,
	emissionQuant,emissionRate
)
select mwo.MOVESRunID,mwo.iterationID,mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	ppa.pollutantID,
	mwo.processID,mwo.sourceTypeID,mwo.fuelTypeID,mwo.modelYearID,mwo.roadTypeID,mwo.SCC,
	mwo.regClassID,mwo.fuelFormulationID,mwo.fuelSubtypeID,
	emissionQuant,emissionRate
from HCWorkerOutputAll mwo
inner join HCPollutantProcessAssoc ppa on (ppa.processID=mwo.processID
	and ppa.processID in (##togProcessIDs##)
	and ppa.pollutantID = 86)
where mwo.pollutantID in (80,5);

-- Move values back into MOVESWorkerOutput
insert into MOVESWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
	sum(emissionQuant),sum(emissionRate)
from HCWorkerOutput
where emissionQuant >= 0
group by yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,regClassID;

-- TOG values aren't needed by subsequent steps.
-- If they are needed, uncomment the following.
-- insert into HCWorkerOutputAll (
-- 	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
-- 	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
-- 	emissionQuant,emissionRate,
-- 	fuelFormulationID,fuelSubtypeID
-- )
-- select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
-- 	processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC,regClassID,
-- 	emissionQuant,emissionRate,
-- 	fuelFormulationID,fuelSubtypeID
-- from HCWorkerOutput
-- where emissionQuant >= 0

-- End Section TOG

-- alter table MOVESWorkerOutput drop key HCPollutantID;
-- alter table MOVESWorkerOutput drop index MOVESWorkerOutput_A2;

alter table HCFuelSupply drop index HCFuelSupply_A1;
alter table HCspeciation drop index HCspeciation_A1;
alter table HCETOHBin drop index HCETOHBin_A1;
alter table HCPollutantProcessAssoc drop index HCPollutantProcessAssoc_A1;
alter table HCFuelFormulation drop index HCFuelFormulation_A1;

starttimer savemwo2;
savemwo2;
starttimer HCSpeciationCalculator;

-- End Section Processing

-- Section Cleanup
drop table if exists HCWorkerOutput;
drop table if exists HCWorkerOutputAll;
-- End Section Cleanup

-- Section Final Cleanup
-- End Section Final Cleanup
