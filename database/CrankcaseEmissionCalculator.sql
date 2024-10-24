-- Author Wesley Faler
-- Version 2013-09-23

-- @algorithm
-- @owner Crankcase Emission Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data
CREATE TABLE IF NOT EXISTS ##prefix##CrankcaseEmissionRatio (
  polProcessID int NOT NULL,
  minModelYearID smallint(6) NOT NULL,
  maxModelYearID smallint(6) NOT NULL,
  sourceTypeID smallint(6) NOT NULL,
  regClassID smallint(6) NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  crankcaseRatio float NOT NULL,
  crankcaseRatioCV float DEFAULT NULL,
  primary key (polProcessID, minModelYearID, maxModelYearID, sourceTypeID, regClassID, fuelTypeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
TRUNCATE TABLE ##prefix##CrankcaseEmissionRatio;

CREATE TABLE IF NOT EXISTS ##prefix##CrankcasePollutantProcessAssoc (
       polProcessID         int NOT NULL,
       processID            SMALLINT NOT NULL,
       pollutantID          SMALLINT NOT NULL,
	   isAffectedByExhaustIM CHAR(1) NOT NULL DEFAULT "N",
       isAffectedByEvapIM CHAR(1) NOT NULL DEFAULT "N",
       PRIMARY KEY (polProcessID),
       KEY (processID),
       KEY (pollutantID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
TRUNCATE TABLE ##prefix##CrankcasePollutantProcessAssoc;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

-- Section PM
cache select c.polProcessID,
	MYRMAP(c.minModelYearID) as minModelYearID,
	MYRMAP(c.maxModelYearID) as maxModelYearID,
	c.sourceTypeID,
	c.regClassID,
	c.fuelTypeID,
	c.crankcaseRatio,
	c.crankcaseRatioCV
into outfile '##PMCrankcaseEmissionRatio##'
from PollutantProcessAssoc ppa
inner join CrankcaseEmissionRatio c on (c.polProcessID=ppa.polProcessID)
inner join RunSpecSourceFuelType r on (r.sourceTypeID=c.sourceTypeID and r.fuelTypeID=c.fuelTypeID)
where ppa.pollutantID in (##pollutantIDs##)
and ppa.processID = ##outputProcessID##
and (
	(c.minModelYearID >= MYMAP(##context.year## - 40) and c.minModelYearID <= MYMAP(##context.year##))
	or
	(c.maxModelYearID >= MYMAP(##context.year## - 40) and c.maxModelYearID <= MYMAP(##context.year##))
	or
	(c.minModelYearID < MYMAP(##context.year## - 40) and c.maxModelYearID > MYMAP(##context.year##))
);

cache select distinct ppa.polProcessID, ppa.processID, ppa.pollutantID, ppa.isAffectedByExhaustIM, ppa.isAffectedByEvapIM
into outfile '##PMCrankcasePollutantProcessAssoc##'
from PollutantProcessAssoc ppa
inner join CrankcaseEmissionRatio c on (c.polProcessID=ppa.polProcessID)
inner join RunSpecSourceFuelType r on (r.sourceTypeID=c.sourceTypeID and r.fuelTypeID=c.fuelTypeID)
where ppa.pollutantID in (##pollutantIDs##)
and ppa.processID = ##outputProcessID##
and (
	(c.minModelYearID >= MYMAP(##context.year## - 40) and c.minModelYearID <= MYMAP(##context.year##))
	or
	(c.maxModelYearID >= MYMAP(##context.year## - 40) and c.maxModelYearID <= MYMAP(##context.year##))
	or
	(c.minModelYearID < MYMAP(##context.year## - 40) and c.maxModelYearID > MYMAP(##context.year##))
);
-- End Section PM

-- Section NonPM
cache select c.polProcessID,
	MYRMAP(c.minModelYearID) as minModelYearID,
	MYRMAP(c.maxModelYearID) as maxModelYearID,
	c.sourceTypeID,
	c.regClassID,
	c.fuelTypeID,
	c.crankcaseRatio,
	c.crankcaseRatioCV
into outfile '##NonPMCrankcaseEmissionRatio##'
from PollutantProcessAssoc ppa
inner join CrankcaseEmissionRatio c on (c.polProcessID=ppa.polProcessID)
inner join RunSpecSourceFuelType r on (r.sourceTypeID=c.sourceTypeID and r.fuelTypeID=c.fuelTypeID)
where ppa.pollutantID in (##pollutantIDs##)
and ppa.processID = ##outputProcessID##
and (
	(c.minModelYearID >= MYMAP(##context.year## - 40) and c.minModelYearID <= MYMAP(##context.year##))
	or
	(c.maxModelYearID >= MYMAP(##context.year## - 40) and c.maxModelYearID <= MYMAP(##context.year##))
	or
	(c.minModelYearID < MYMAP(##context.year## - 40) and c.maxModelYearID > MYMAP(##context.year##))
);

cache select distinct ppa.polProcessID, ppa.processID, ppa.pollutantID, ppa.isAffectedByExhaustIM, ppa.isAffectedByEvapIM
into outfile '##NonPMCrankcasePollutantProcessAssoc##'
from PollutantProcessAssoc ppa
inner join CrankcaseEmissionRatio c on (c.polProcessID=ppa.polProcessID)
inner join RunSpecSourceFuelType r on (r.sourceTypeID=c.sourceTypeID and r.fuelTypeID=c.fuelTypeID)
where ppa.pollutantID in (##pollutantIDs##)
and ppa.processID = ##outputProcessID##
and (
	(c.minModelYearID >= MYMAP(##context.year## - 40) and c.minModelYearID <= MYMAP(##context.year##))
	or
	(c.maxModelYearID >= MYMAP(##context.year## - 40) and c.maxModelYearID <= MYMAP(##context.year##))
	or
	(c.minModelYearID < MYMAP(##context.year## - 40) and c.maxModelYearID > MYMAP(##context.year##))
);
-- End Section NonPM

-- End Section Extract Data

-- Section Processing

drop table if exists ##prefix##CrankcaseMOVESWorkerOutputTemp;
create table if not exists ##prefix##CrankcaseMOVESWorkerOutputTemp (
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
	emissionQuant        DOUBLE NULL,
	emissionRate		 DOUBLE NULL,
	
	index (fuelTypeID),
	index (sourceTypeID),
	index (regClasSID),
	index (roadTypeID),
	index (zoneID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

CREATE INDEX ##prefix##MOVESWorkerOutput_New2 ON MOVESWorkerOutput (
	pollutantID ASC,
	sourceTypeID ASC,
	regClassID ASC,
	fuelTypeID ASC,
	modelYearID ASC,
	processID ASC
);
CREATE INDEX ##prefix##CrankcasePollutantProcessAssoc_New1 ON ##prefix##CrankcasePollutantProcessAssoc (
	pollutantID ASC,
	polProcessID ASC,
	processID ASC
);
CREATE INDEX ##prefix##CrankcaseEmissionRatio_New1 ON ##prefix##CrankcaseEmissionRatio (
	polProcessID ASC,
	sourceTypeID ASC,
	regClassID ASC,
	fuelTypeID ASC,
	minModelYearID ASC,
	maxModelYearID ASC
);

-- @algorithm crankcase emissions[output pollutantID,processID,modelYearID,sourceTypeID,regClassID,fuelTypeID] = emissions[input pollutantID,processID,modelYearID,sourceTypeID,regClassID,fuelTypeID] *
-- crankcaseRatio[output pollutantID,input polluantID,processID,modelYearID,sourceTypeID,regClassID,fuelTypeID]
insert into ##prefix##CrankcaseMOVESWorkerOutputTemp (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate)
select
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    ppa.pollutantID,
    ppa.processID,
    r.sourceTypeID,
    r.regClassID,
    r.fuelTypeID,
    mwo.modelYearID,
    roadTypeID,
    SCC,
    (emissionQuant * crankcaseRatio) as emissionQuant,
    (emissionRate * crankcaseRatio) as emissionRate
from MOVESWorkerOutput mwo
inner join ##prefix##CrankcasePollutantProcessAssoc ppa on (ppa.pollutantID=mwo.pollutantID)
inner join ##prefix##CrankcaseEmissionRatio r on (
	r.polProcessID=ppa.polProcessID
	and r.sourceTypeID=mwo.sourceTypeID
	and r.regClassID=mwo.regClassID
	and r.fuelTypeID=mwo.fuelTypeID
	and r.minModelYearID <= mwo.modelYearID
	and r.maxModelYearID >= mwo.modelYearID
	)
where ((mwo.processID=1 and ppa.processID=15)
	or (mwo.processID=2 and ppa.processID=16)
	or (mwo.processID=90 and ppa.processID=17))
	and mwo.pollutantID in (##pollutantIDs##);

insert into MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate)
select
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate
from ##prefix##CrankcaseMOVESWorkerOutputTemp;

-- Section SulfatePM10
truncate table ##prefix##CrankcaseMOVESWorkerOutputTemp;
insert into ##prefix##CrankcaseMOVESWorkerOutputTemp (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate)
select
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    105 as pollutantID,
    processID,
    sourceTypeID,
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate
from MOVESWorkerOutput
where pollutantID=115
and processID in (15,16,17);

insert into MOVESWorkerOutput (
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate)
select
    yearID,
    monthID,
    dayID,
    hourID,
    stateID,
    countyID,
    zoneID,
    linkID,
    pollutantID,
    processID,
    sourceTypeID,
    regClassID,
    fuelTypeID,
    modelYearID,
    roadTypeID,
    SCC,
    emissionQuant,
    emissionRate
from ##prefix##CrankcaseMOVESWorkerOutputTemp;
-- End Section SulfatePM10

alter table MOVESWorkerOutput drop index ##prefix##MOVESWorkerOutput_New2;
alter table ##prefix##CrankcasePollutantProcessAssoc drop index ##prefix##CrankcasePollutantProcessAssoc_New1;
alter table ##prefix##CrankcaseEmissionRatio drop index ##prefix##CrankcaseEmissionRatio_New1;

-- End Section Processing

-- Section Cleanup
drop table if exists ##prefix##CrankcaseMOVESWorkerOutputTemp;
drop table if exists ##prefix##CrankcasePollutantProcessAssoc;
-- End Section Cleanup
