-- Author Wesley Faler
-- Author Ed Campbell
-- Version 2015-04-07

-- @algorithm
-- @owner Air toxics Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

drop table if exists ATMonthGroup;
create table if not exists ATMonthGroup (
	monthID smallint(6) NOT NULL DEFAULT '0',
	monthGroupID smallint(6) NOT NULL DEFAULT '0',
	key (monthID, monthGroupID),
	key (monthGroupID, monthID)
) Engine=MEMORY;

-- Section UseMinorHAPRatio
-- minorHAPRatio has fuelSubtypeID in the execution database, but the version
-- submitted to the calculator has been joined to the fuel supply and weighted
-- by the fuel formulation market share.

CREATE TABLE minorHAPRatio (
  processID smallint(6) NOT NULL,
  outputPollutantID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL DEFAULT '0',
  modelYearID smallint(6) NOT NULL,
  monthID smallint(6) NOT NULL,
  atRatio double DEFAULT NULL,
  KEY (processID,fuelTypeID,modelYearID,monthID)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;
-- End Section UseMinorHAPRatio

-- Section UsePAHGasRatio
CREATE TABLE pahGasRatio (
  processID smallint(6) NOT NULL,
  outputPollutantID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL DEFAULT '0',
  modelYearID smallint(6) NOT NULL DEFAULT '0',
  atRatio double DEFAULT NULL,
  KEY (processID,fuelTypeID,modelYearID)
);
-- End Section UsePAHGasRatio

-- Section UsePAHParticleRatio
CREATE TABLE pahParticleRatio (
  processID smallint(6) NOT NULL,
  outputPollutantID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL DEFAULT '0',
  modelYearID smallint(6) NOT NULL DEFAULT '0',
  atRatio double DEFAULT NULL,
  KEY (processID,fuelTypeID,modelYearID)
);
-- End Section UsePAHParticleRatio

-- Section UseATRatioGas1
drop table if exists ATRatioGas1ChainedTo;

CREATE TABLE IF NOT EXISTS ATRatioGas1ChainedTo (
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
	),
	key (
		inputPollutantID,
		inputProcessID,
		inputPolProcessID,
		outputPolProcessID
	)
) Engine=MEMORY;

CREATE TABLE IF NOT EXISTS ATRatio (
  fuelTypeID smallint(6) NOT NULL,
  fuelFormulationID smallint(6) NOT NULL,
  polProcessID int NOT NULL,
  minModelYearID smallint(6) NOT NULL,
  maxModelYearID smallint(6) NOT NULL,
  ageID smallint(6) NOT NULL,
  monthID smallint(6) NOT NULL,
  atRatio double DEFAULT NULL,
  modelYearID smallint(6) NOT NULL,
  PRIMARY KEY (fuelTypeID,fuelFormulationID,polProcessID,minModelYearID,maxModelYearID,ageID,monthID),
  KEY atratio_key1 (fuelFormulationID,polProcessID,minModelYearID),
  KEY atratio_key2 (polProcessID,fuelTypeID,monthID,minModelYearID,ageID,maxModelYearID,fuelFormulationID),
  KEY atratio_key3 (polProcessID,fuelTypeID,monthID,modelYearID,minModelYearID,maxModelYearID,fuelFormulationID)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

TRUNCATE TABLE ATRatio;

CREATE TABLE IF NOT EXISTS AT1FuelSupply (
  countyID int(11) NOT NULL,
  monthID smallint(6) NOT NULL,
  fuelFormulationID smallint(6) NOT NULL,
  marketShare double default NULL,
  yearID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  KEY (monthID,fuelTypeID,fuelFormulationID,marketShare)
) Engine=MEMORY;

TRUNCATE TABLE AT1FuelSupply;

CREATE TABLE IF NOT EXISTS AT1PollutantProcessModelYear (
  polProcessID int NOT NULL default '0',
  modelYearID smallint(6) NOT NULL default '0',
  fuelMYGroupID int(11) default NULL,
  key (polProcessID),
  key (modelYearID),
  key (fuelMYGroupID),
  key (polProcessID, modelYearID, fuelMYGroupID)
) Engine=MEMORY;

TRUNCATE TABLE AT1PollutantProcessModelYear;

-- End Section UseATRatioGas1

-- Section UseATRatioGas2
drop table if exists ATRatioGas2ChainedTo;

CREATE TABLE IF NOT EXISTS ATRatioGas2ChainedTo (
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
	),
	key (
		inputPollutantID,
		inputProcessID,
		inputPolProcessID,
		outputPolProcessID
	)
) Engine=MEMORY;

##create.ATRatioGas2##;
TRUNCATE TABLE ATRatioGas2;

CREATE TABLE IF NOT EXISTS AT2FuelSupply (
  fuelRegionID int(11) NOT NULL,
  monthID smallint(6) NOT NULL,
  fuelSubtypeID smallint(6) NOT NULL,
  marketShare float default NULL,
  yearID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  KEY (countyID,yearID,monthID,fuelTypeID,fuelSubtypeID),
  KEY (countyID,yearID,monthID,fuelSubtypeID)
) Engine=MEMORY;

TRUNCATE TABLE AT2FuelSupply;
-- End Section UseATRatioGas2

-- Section UseATRatioNonGas
drop table if exists ATRatioNonGasChainedTo;

CREATE TABLE IF NOT EXISTS ATRatioNonGasChainedTo (
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
	),
	key (
		inputPollutantID,
		inputProcessID,
		inputPolProcessID,
		outputPolProcessID
	)
) Engine=MEMORY;

CREATE TABLE ATRatioNonGas (
  polProcessID int NOT NULL DEFAULT '0',
  sourceTypeID smallint(6) NOT NULL DEFAULT '0',
  fuelSubtypeID smallint(6) NOT NULL DEFAULT '0',
  modelYearID smallint(6) NOT NULL DEFAULT '0',
  ATRatio double DEFAULT NULL,
  PRIMARY KEY (polProcessID,sourceTypeID,fuelSubtypeID,modelYearID),
  KEY (polProcessID,sourceTypeID,modelYearID,fuelSubtypeID)
);

TRUNCATE TABLE ATRatioNonGas;

CREATE TABLE IF NOT EXISTS ATNonGasFuelSupply (
  countyID int(11) NOT NULL,
  monthID smallint(6) NOT NULL,
  fuelFormulationID smallint(6) NOT NULL,
  marketShare float default NULL,
  yearID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  fuelSubtypeID smallint(6) NOT NULL,
  KEY (countyID,yearID,monthID,fuelTypeID,fuelSubtypeID),
  KEY (countyID,yearID,monthID,fuelSubtypeID)
);

TRUNCATE TABLE ATNonGasFuelSupply;
-- End Section UseATRatioNonGas

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache SELECT monthID, monthGroupID
INTO OUTFILE '##ATMonthGroup##'
FROM monthOfAnyYear
WHERE (##context.monthID## <= 0 OR monthID = ##context.monthID##);

-- Section UseMinorHAPRatio

-- minorHAPRatio has fuelSubtypeID in the execution database, but the version
-- submitted to the calculator has been joined to the fuel supply and weighted
-- by the fuel formulation market share.

cache select ppa.processID, ppa.pollutantID as outputPollutantID, fuelTypeID, modelYearID, monthID, sum(atRatio*marketShare) as atRatio
into outfile '##minorHAPRatio##'
from minorHAPRatio r
inner join pollutantProcessAssoc ppa using (polProcessID)
inner join modelYear my on (
	MYMAP(modelYearID) >= round(modelYearGroupID/10000,0)
	and MYMAP(modelYearID) <= mod(modelYearGroupID,10000)
	and modelYearID <= ##context.year##
	and modelYearID >= ##context.year## - 30
)
inner join fuelFormulation ff using (fuelSubtypeID)
inner join fuelSupply fs on (
	fs.fuelRegionID=##context.fuelRegionID##
	and fuelYearID=(select fuelYearID from year where yearID=##context.year##)
	and fs.fuelFormulationID=ff.fuelFormulationID
)
inner join monthOfAnyYear m on (
	m.monthGroupID=fs.monthGroupID
	and (##context.monthID## <= 0 OR monthID = ##context.monthID##)
)
where polProcessID in (##outputMinorHAPRatio##)
group by ppa.polProcessID, fuelTypeID, modelYearID, monthID;
-- End Section UseMinorHAPRatio

-- Section UsePAHGasRatio
cache select ppa.processID, ppa.pollutantID as outputPollutantID, fuelTypeID, modelYearID, atRatio
into outfile '##pahGasRatio##'
from pahGasRatio r
inner join pollutantProcessAssoc ppa using (polProcessID)
inner join modelYear my on (
	MYMAP(modelYearID) >= round(modelYearGroupID/10000,0)
	and MYMAP(modelYearID) <= mod(modelYearGroupID,10000)
	and modelYearID <= ##context.year##
	and modelYearID >= ##context.year## - 30
)
where polProcessID in (##outputPAHGasRatio##);
-- End Section UsePAHGasRatio

-- Section UsePAHParticleRatio
cache select ppa.processID, ppa.pollutantID as outputPollutantID, fuelTypeID, modelYearID, atRatio
into outfile '##pahParticleRatio##'
from pahParticleRatio r
inner join pollutantProcessAssoc ppa using (polProcessID)
inner join modelYear my on (
	MYMAP(modelYearID) >= round(modelYearGroupID/10000,0)
	and MYMAP(modelYearID) <= mod(modelYearGroupID,10000)
	and modelYearID <= ##context.year##
	and modelYearID >= ##context.year## - 30
)
where polProcessID in (##outputPAHParticleRatio##);
-- End Section UsePAHParticleRatio

-- Section UseATRatioGas1
cache SELECT DISTINCT
	ATRatio.fuelTypeID,
	ATRatio.fuelFormulationID,
	ATRatio.polProcessID,
	MYRMAP(ATRatio.minModelYearID) as minModelYearID,
	MYRMAP(ATRatio.maxModelYearID) as maxModelYearID,
	ATRatio.ageID,
	MonthOfAnyYear.monthID,
	ATRatio.atRatio,
	(##context.year## - ATRatio.ageID) as modelYearID
INTO OUTFILE '##ATRatio##'
FROM ATRatio
INNER JOIN FuelSupply ON (FuelSupply.fuelFormulationID = ATRatio.fuelFormulationID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN RunSpecMonthGroup ON (RunSpecMonthGroup.monthGroupID = FuelSupply.monthGroupID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = RunSpecMonthGroup.monthGroupID)
WHERE polProcessID in (##outputATRatioGas1##)
AND fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND minModelYearID <= MYMAP(##context.year## - ATRatio.ageID)
AND maxModelYearID >= MYMAP(##context.year## - ATRatio.ageID);

-- AND minModelYearID <= ##context.year##

cache SELECT *
INTO OUTFILE '##ATRatioGas1ChainedTo##'
FROM RunSpecChainedTo
WHERE outputPolProcessID in (##outputATRatioGas1##);

cache SELECT ##context.iterLocation.countyRecordID## as countyID, MonthOfAnyYear.monthID, FuelSupply.fuelFormulationID, FuelSupply.marketShare, Year.yearID, FuelSubtype.fuelTypeID
INTO OUTFILE '##AT1FuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN FuelFormulation ON (FuelFormulation.fuelFormulationID = FuelSupply.fuelFormulationID)
INNER JOIN FuelSubtype ON (FuelSubtype.fuelSubtypeID = FuelFormulation.fuelSubtypeID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)
INNER JOIN RunSpecMonth ON (RunSpecMonth.monthID = MonthOfAnyYear.monthID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND (##context.monthID## <= 0 OR RunSpecMonth.monthID = ##context.monthID##);

cache SELECT polProcessID, modelYearID, fuelMYGroupID INTO OUTFILE '##AT1PollutantProcessModelYear##'
FROM PollutantProcessModelYear
WHERE modelYearID <= ##context.year##
AND modelYearID >= ##context.year## - 30
AND polProcessID IN (##inputATRatioGas1##);

-- End Section UseATRatioGas1

-- Section UseATRatioGas2
cache SELECT *
INTO OUTFILE '##ATRatioGas2##'
FROM ATRatioGas2
WHERE polProcessID in (##outputATRatioGas2##);

cache SELECT *
INTO OUTFILE '##ATRatioGas2ChainedTo##'
FROM RunSpecChainedTo
WHERE outputPolProcessID in (##outputATRatioGas2##);

cache SELECT ##context.iterLocation.countyRecordID## as countyID, MonthOfAnyYear.monthID, FuelSubtype.fuelSubtypeID, FuelSupply.marketShare, Year.yearID, FuelSubtype.fuelTypeID
INTO OUTFILE '##AT2FuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN FuelFormulation ON (FuelFormulation.fuelFormulationID = FuelSupply.fuelFormulationID)
INNER JOIN FuelSubtype ON (FuelSubtype.fuelSubtypeID = FuelFormulation.fuelSubtypeID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)
INNER JOIN RunSpecMonth ON (RunSpecMonth.monthID = MonthOfAnyYear.monthID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND (##context.monthID## <= 0 OR RunSpecMonth.monthID = ##context.monthID##);
-- End Section UseATRatioGas2

-- Section UseATRatioNonGas
cache SELECT r.polProcessID, r.sourceTypeID, r.fuelSubtypeID, my.modelYearID, r.ATRatio
INTO OUTFILE '##ATRatioNonGas##'
FROM ATRatioNonGas r
inner join modelYear my on (
	MYMAP(modelYearID) >= round(modelYearGroupID/10000,0)
	and MYMAP(modelYearID) <= mod(modelYearGroupID,10000)
	and modelYearID <= ##context.year##
	and modelYearID >= ##context.year## - 30
)
WHERE polProcessID in (##outputATRatioNonGas##);

cache SELECT *
INTO OUTFILE '##ATRatioNonGasChainedTo##'
FROM RunSpecChainedTo
WHERE outputPolProcessID in (##outputATRatioNonGas##);

cache SELECT ##context.iterLocation.countyRecordID## as countyID, MonthOfAnyYear.monthID, FuelSupply.fuelFormulationID, FuelSupply.marketShare, Year.yearID, FuelSubtype.fuelTypeID, FuelSubtype.fuelSubtypeID
INTO OUTFILE '##ATNonGasFuelSupply##'
FROM FuelSupply
INNER JOIN RunSpecMonthGroup ON (FuelSupply.monthGroupID = RunSpecMonthGroup.monthGroupID)
INNER JOIN Year ON (FuelSupply.fuelYearID = Year.fuelYearID)
INNER JOIN FuelFormulation ON (FuelFormulation.fuelFormulationID = FuelSupply.fuelFormulationID)
INNER JOIN FuelSubtype ON (FuelSubtype.fuelSubtypeID = FuelFormulation.fuelSubtypeID)
INNER JOIN MonthOfAnyYear ON (MonthOfAnyYear.monthGroupID = FuelSupply.monthGroupID)
INNER JOIN RunSpecMonth ON (RunSpecMonth.monthID = MonthOfAnyYear.monthID)
WHERE fuelRegionID = ##context.fuelRegionID##
AND yearID = ##context.year##
AND (##context.monthID## <= 0 OR RunSpecMonth.monthID = ##context.monthID##);
-- End Section UseATRatioNonGas

-- End Section Extract Data

-- Section Processing

-- @algorithm create AirToxicsMOVESWorkerOutputTemp table
drop table if exists AirToxicsMOVESWorkerOutputTemp;
create table if not exists AirToxicsMOVESWorkerOutputTemp (
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
	regClassID			 SMALLINT UNSIGNED NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,
	roadTypeID           SMALLINT UNSIGNED NULL,
	SCC                  CHAR(10) NULL,
	emissionQuant        double NULL,
	emissionRate		 double NULL,
	key (yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,fuelTypeID,modelYearID,roadTypeID,SCC)
);

CREATE INDEX MOVESWorkerOutput_A3 ON MOVESWorkerOutput (
	pollutantID asc,
	processID asc,
	sourceTypeID asc,
	yearID asc,
	monthID asc,
	fuelTypeID asc
);

-- Section UseMinorHAPRatio

-- @algorithm minor HAP emissions[outputPollutantID] = VOC (87) * ATRatio
insert into AirToxicsMOVESWorkerOutputTemp (
	monthID, modelYearID, yearID, fuelTypeID, dayID, hourID, stateID, countyID, 
	zoneID, linkID, pollutantID, processID, sourceTypeID, regClassID, roadTypeID, SCC, 
	emissionQuant, emissionRate)
select
   	mwo.monthID, mwo.modelYearID, mwo.yearID, mwo.fuelTypeID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, 
	mwo.zoneID, mwo.linkID,	r.outputPollutantID, mwo.processID, mwo.sourceTypeID, mwo.regClassID, mwo.roadTypeID, mwo.SCC,
	r.atRatio*emissionQuant, r.atRatio*emissionRate
from MOVESWorkerOutput mwo
inner join minorHAPRatio r on (
	r.processID = mwo.processID
	and mwo.pollutantID = 87
	and r.fuelTypeID = mwo.fuelTypeID
	and r.modelYearID = mwo.modelYearID
	and r.monthID = mwo.monthID
);

-- End Section UseMinorHAPRatio

-- Section UsePAHGasRatio

-- @algorithm PAH gas emissions[outputPollutantID] = VOC (87) * ATRatio
insert into AirToxicsMOVESWorkerOutputTemp (
	monthID, modelYearID, yearID, fuelTypeID, dayID, hourID, stateID, countyID, 
	zoneID, linkID, pollutantID, processID, sourceTypeID, regClassID, roadTypeID, SCC, 
	emissionQuant, emissionRate)
select
   	mwo.monthID, mwo.modelYearID, mwo.yearID, mwo.fuelTypeID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, 
	mwo.zoneID, mwo.linkID,	r.outputPollutantID, mwo.processID, mwo.sourceTypeID, mwo.regClassID, mwo.roadTypeID, mwo.SCC,
	r.atRatio*emissionQuant, r.atRatio*emissionRate
from MOVESWorkerOutput mwo
inner join pahGasRatio r on (
	r.processID = mwo.processID
	and mwo.pollutantID = 87
	and r.fuelTypeID = mwo.fuelTypeID
	and r.modelYearID = mwo.modelYearID
);

-- End Section UsePAHGasRatio

-- Section UsePAHParticleRatio

-- @algorithm PAH particle emissions[outputPollutantID] = VOC (87) * ATRatio
insert into AirToxicsMOVESWorkerOutputTemp (
	monthID, modelYearID, yearID, fuelTypeID, dayID, hourID, stateID, countyID, 
	zoneID, linkID, pollutantID, processID, sourceTypeID, regClassID, roadTypeID, SCC, 
	emissionQuant, emissionRate)
select
   	mwo.monthID, mwo.modelYearID, mwo.yearID, mwo.fuelTypeID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, 
	mwo.zoneID, mwo.linkID,	r.outputPollutantID, mwo.processID, mwo.sourceTypeID, mwo.regClassID, mwo.roadTypeID, mwo.SCC,
	r.atRatio*emissionQuant, r.atRatio*emissionRate
from MOVESWorkerOutput mwo
inner join pahParticleRatio r on (
	r.processID = mwo.processID
	and mwo.pollutantID = 111
	and r.fuelTypeID = mwo.fuelTypeID
	and r.modelYearID = mwo.modelYearID
);

-- End Section UsePAHParticleRatio

-- Section UseATRatioGas1

-- @algorithm emissions[outputPollutantID] = emissions[inputPollutantID] * marketShare * ATRatio
-- @input FuelSupply
-- @input PollutantProcessAssoc
insert into AirToxicsMOVESWorkerOutputTemp (
	monthID, modelYearID, yearID, fuelTypeID, dayID, hourID, stateID, countyID, 
	zoneID, linkID, pollutantID, processID, sourceTypeID, regClassID, roadTypeID, SCC, 
	emissionQuant, emissionRate)
select
   	mwo.monthID, mwo.modelYearID, mwo.yearID, mwo.fuelTypeID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, 
	mwo.zoneID, mwo.linkID,	ct.outputPollutantID, ct.outputProcessID, mwo.sourceTypeID, mwo.regClassID, mwo.roadTypeID, mwo.SCC,
	r.ATRatio*fs.marketShare*emissionQuant, r.ATRatio*fs.marketShare*emissionRate
from MOVESWorkerOutput mwo
inner join ATRatioGas1ChainedTo ct on (
	ct.inputPollutantID=mwo.pollutantID
	and ct.inputProcessID=mwo.processID)
inner join ATRatio r on (
	r.minModelYearID <= mwo.modelYearID and r.maxModelYearID >= mwo.modelYearID
	and r.modelYearID = mwo.modelYearID
	and r.fuelTypeID=mwo.fuelTypeID
	and r.polProcessID=ct.outputPolProcessID
	and r.monthID = mwo.monthID
	)
inner join AT1FuelSupply fs on (
	fs.monthID = mwo.monthID
	and fs.fuelTypeID = mwo.fuelTypeID
	and fs.fuelFormulationID = r.fuelFormulationID
);

-- End Section UseATRatioGas1

-- Section UseATRatioGas2
create index ATRatioGas2ChainedTo_A1 on ATRatioGas2ChainedTo (
	inputPollutantID asc,
	inputProcessID asc,
	outputPolProcessID asc
);

-- create index AT2FuelSupply_A1 on AT2FuelSupply (
-- 	yearID asc,
-- 	monthID asc,
-- 	fuelSubtypeID asc,
-- 	countyID asc,
-- 	fuelTypeID asc
-- );

-- insert into AirToxicsMOVESWorkerOutputTemp (
-- yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, pollutantID, processID,
-- sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate)
-- select
--     mwo.yearID, mwo.monthID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, mwo.zoneID, mwo.linkID,
-- 	ct.outputPollutantID,
-- 	ct.outputProcessID,
-- 	mwo.sourceTypeID, mwo.regClassID, mwo.fuelTypeID, mwo.modelYearID, mwo.roadTypeID, mwo.SCC,
-- 	r.ATRatio*fs.marketShare*emissionQuant, r.ATRatio*fs.marketShare*emissionRate
-- from MOVESWorkerOutput mwo
-- inner join ATRatioGas2ChainedTo ct on (
-- 	ct.inputPollutantID=mwo.pollutantID
-- 	and ct.inputProcessID=mwo.processID)
-- inner join ATRatioGas2 r on (
-- 	r.polProcessID=ct.outputPolProcessID
-- 	and r.sourceTypeID=mwo.sourceTypeID)
-- inner join AT2FuelSupply fs on (
-- 	fs.yearID = mwo.yearID
-- 	and fs.monthID = mwo.monthID
-- 	and fs.fuelSubtypeID = r.fuelSubtypeID
-- 	and fs.countyID = # # context.iterLocation . countyRecordID # #
-- 	and fs.fuelTypeID = mwo.fuelTypeID);

alter table ATRatioGas2ChainedTo add key (inputPollutantID, inputProcessID, outputPolProcessID, outputPollutantID, outputProcessID);
analyze table ATRatioGas2ChainedTo;

alter table ATRatioGas2 add key (polProcessID, sourceTypeID, fuelSubtypeID, ATRatio);
alter table ATRatioGas2 add key (sourceTypeID, fuelSubtypeID, polProcessID, ATRatio);
alter table ATRatioGas2 add key (fuelSubtypeID, polProcessID, sourceTypeID, ATRatio);
alter table ATRatioGas2 add key (polProcessID, fuelSubtypeID, sourceTypeID, ATRatio);
alter table ATRatioGas2 add key (sourceTypeID, polProcessID, fuelSubtypeID, ATRatio);
alter table ATRatioGas2 add key (fuelSubtypeID, sourceTypeID, polProcessID, ATRatio);
analyze table ATRatioGas2;

alter table AT2FuelSupply drop key countyID;
alter table AT2FuelSupply drop key countyID_2;
alter table AT2FuelSupply add key (yearID,monthID,countyID,fuelTypeID,fuelSubtypeID, marketShare);
analyze table AT2FuelSupply;

drop table if exists AT2FuelSupplyRatioGas2;

-- @algorithm marketShareATRatio[outputPollutantID] = marketShare * ATRatio
-- @input FuelSupply
-- @input PollutantProcessAssoc
create table AT2FuelSupplyRatioGas2
select ct.inputPollutantID, ct.inputProcessID, r.polProcessID, r.sourceTypeID,
	fs.yearID, fs.monthID, fs.fuelTypeID, fs.marketShare*r.ATRatio as marketShareATRatio,
	ct.outputPollutantID,
	ct.outputProcessID
from ATRatioGas2 r
inner join AT2FuelSupply fs on (
	fs.countyID = ##context.iterLocation.countyRecordID##
	and fs.fuelSubtypeID = r.fuelSubtypeID)
inner join ATRatioGas2ChainedTo ct on (
	r.polProcessID=ct.outputPolProcessID);

alter table AT2FuelSupplyRatioGas2 add key (inputPollutantID, inputProcessID, fuelTypeID, sourceTypeID, yearID, monthID, outputPollutantID, outputProcessID, marketShareATRatio);
analyze table AT2FuelSupplyRatioGas2;

-- @algorithm emissions[outputPollutantID] = emissions[inputPollutantID] * marketShareATRatio[outputPollutantID]
insert into AirToxicsMOVESWorkerOutputTemp (
	yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, pollutantID, processID,
	sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate)
select
    mwo.yearID, mwo.monthID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, mwo.zoneID, mwo.linkID,
	r.outputPollutantID,
	r.outputProcessID,
	mwo.sourceTypeID, mwo.regClassID, mwo.fuelTypeID, mwo.modelYearID, mwo.roadTypeID, mwo.SCC,
	r.marketShareATRatio*emissionQuant, r.marketShareATRatio*emissionRate
from MOVESWorkerOutput mwo
inner join AT2FuelSupplyRatioGas2 r on (
	r.inputPollutantID=mwo.pollutantID
	and r.inputProcessID=mwo.processID
	and r.sourceTypeID=mwo.sourceTypeID
	and r.yearID = mwo.yearID
	and r.monthID = mwo.monthID
	and r.fuelTypeID = mwo.fuelTypeID);
-- End Section UseATRatioGas2

-- Section UseATRatioNonGas
CREATE INDEX ATRatioNonGasChainedTo_A1 ON ATRatioNonGasChainedTo (
	inputPollutantID asc,
	inputProcessID asc,
	outputPolProcessID asc
);

CREATE INDEX ATNonGasFuelSupply_A1 ON ATNonGasFuelSupply (
	yearID asc,
	monthID asc,
	fuelSubtypeID asc,
	countyID asc, 
	fuelTypeID asc 
);

-- @algorithm emissions[outputPollutantID] = emissions[inputPollutantID] * marketShare * ATRatio
-- @input FuelSupply
-- @input PollutantProcessAssoc
insert into AirToxicsMOVESWorkerOutputTemp (
	yearID, monthID, dayID, hourID, stateID, countyID, zoneID, linkID, pollutantID, processID,
	sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC, emissionQuant, emissionRate)
select
    mwo.yearID, mwo.monthID, mwo.dayID, mwo.hourID, mwo.stateID, mwo.countyID, mwo.zoneID, mwo.linkID,
	ct.outputPollutantID,
	ct.outputProcessID,
	mwo.sourceTypeID, mwo.regClassID, mwo.fuelTypeID, mwo.modelYearID, mwo.roadTypeID, mwo.SCC,
	r.ATRatio*fs.marketShare*emissionQuant, r.ATRatio*fs.marketShare*emissionRate
from MOVESWorkerOutput mwo
inner join ATRatioNonGasChainedTo ct on (
	ct.inputPollutantID=mwo.pollutantID
	and ct.inputProcessID=mwo.processID)
inner join ATRatioNonGas r on (
	r.polProcessID=ct.outputPolProcessID
	and r.sourceTypeID=mwo.sourceTypeID
	and r.modelYearID=mwo.modelYearID)
inner join ATNonGasFuelSupply fs on (
	fs.yearID = mwo.yearID
	and fs.monthID = mwo.monthID
	and fs.fuelSubtypeID = r.fuelSubtypeID
	and fs.countyID = ##context.iterLocation.countyRecordID##
	and fs.fuelTypeID = mwo.fuelTypeID);

-- End Section UseATRatioNonGas

-- insert into MOVESWorkerOutput (
-- yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,
-- sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,emissionRate)
-- select
-- yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,
-- sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,
-- sum(emissionQuant) as emissionQuant, sum(emissionRate) as emissionRate
-- from AirToxicsMOVESWorkerOutputTemp
-- group by yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC
-- order by null

-- @algorithm Add emisions in AirToxicsMOVESWorkerOutputTemp to MOVESWorkerOutput
insert into MOVESWorkerOutput (
	yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,emissionRate)
select
	yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,
	emissionQuant,emissionRate
from AirToxicsMOVESWorkerOutputTemp;

alter table MOVESWorkerOutput drop index MOVESWorkerOutput_A3;

-- End Section Processing

-- Section Cleanup
drop table if exists AirToxicsMOVESWorkerOutputTemp;
drop table if exists ATRatioGas1ChainedTo;
drop table if exists ATRatioGas2ChainedTo;
drop table if exists ATRatioNonGasChainedTo;
drop table if exists ATMonthGroup;
drop table if exists AT2FuelSupplyRatioGas2;
-- End Section Cleanup
