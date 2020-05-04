-- TOG Speciation calculator
-- Version 2015-07-15
-- Author Wes Faler

-- @algorithm
-- @owner TOG Speciation Calculator

-- Section Create Remote Tables for Extracted Data

##create.integratedSpeciesSet##;
TRUNCATE TABLE integratedSpeciesSet;

##create.lumpedSpeciesName##;
TRUNCATE TABLE lumpedSpeciesName;

##create.togSpeciation##;
TRUNCATE TABLE togSpeciation;

##create.TOGSpeciationProfile##;
TRUNCATE TABLE TOGSpeciationProfile;

drop table if exists togSpeciationCountyYear;
create table if not exists togSpeciationCountyYear (
	mechanismID smallint not null default 0,
	integratedSpeciesSetID smallint not null default 0,
	countyID int not null default 0,
	monthID smallint not null default 0,
	yearID smallint not null default 0,
	inProcessID smallint not null default 0,
	inPollutantID smallint not null default 0,
	fuelTypeID smallint not null default 0,
	minModelYearID smallint not null default 0,
	maxModelYearID smallint not null default 0,
	regClassID smallint not null default 0,
	outPollutantID smallint not null default 0,
	factor double not null default 0,
	primary key (mechanismID, integratedSpeciesSetID,
		countyID, monthID, yearID, inProcessID, inPollutantID,
		fuelTypeID, minModelYearID, maxModelYearID, regClassID, outPollutantID)
);
TRUNCATE TABLE togSpeciationCountyYear;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache select integratedSpeciesSet.*
into outfile '##integratedSpeciesSet##'
from integratedSpeciesSet
where mechanismID in (##mechanismIDs##);

cache select lumpedSpeciesName.*
into outfile '##lumpedSpeciesName##'
from lumpedSpeciesName;

cache select togSpeciation.*
into outfile '##togSpeciation##'
from togSpeciation
where processID in (##context.allCurrentProcesses##);

cache select distinct tsp.*
into outfile '##TOGSpeciationProfile##'
from TOGSpeciationProfile tsp
inner join togSpeciation ts on (
	ts.togSpeciationProfileID = tsp.togSpeciationProfileID
 	and ts.processID in (##context.allCurrentProcesses##))
where mechanismID in (##mechanismIDs##)
and (integratedSpeciesSetID = 0 or integratedSpeciesSetID in (
	select distinct integratedSpeciesSetID
	from integratedSpeciesSet
	where mechanismID in (##mechanismIDs##)));

cache select mechanismID, integratedSpeciesSetID,
		##context.iterLocation.countyRecordID## as countyID, monthID, yearID, 
		ts.processID as inProcessID, tsp.pollutantID as inPollutantID,
		fst.fuelTypeID,
		ts.minModelYearID, ts.maxModelYearID,
		ts.regClassID,
		1000+(tsp.mechanismID-1)*500+lsn.lumpedSpeciesID as outPollutantID,
		sum(fs.marketShare*tsp.TOGSpeciationMassFraction/tsp.TOGSpeciationDivisor) as factor
into outfile '##togSpeciationCountyYear##'
from togSpeciation ts
inner join togSpeciationProfile tsp on (tsp.togSpeciationProfileID=ts.togSpeciationProfileID)
inner join lumpedSpeciesName lsn on (lsn.lumpedSpeciesName=tsp.lumpedSpeciesName)
inner join fuelFormulation ff on (ff.fuelSubtypeID=ts.fuelSubtypeID)
inner join fuelSubtype fst on (fst.fuelSubtypeID=ff.fuelSubtypeID)
inner join fuelSupply fs on (fs.fuelFormulationID=ff.fuelFormulationID)
inner join monthOfAnyYear may on (may.monthGroupID=fs.monthGroupID)
inner join year y on (y.fuelYearID=fs.fuelYearID)
where tsp.mechanismID in (##mechanismIDs##)
and y.yearID=##context.year##
and fs.fuelRegionID=##context.fuelRegionID##
and ts.minModelYearID <= ##context.year##
and ts.maxModelYearID >= ##context.year##-30
and ts.processID in (##context.allCurrentProcesses##)
group by
	mechanismID, integratedSpeciesSetID,
	monthID, yearID, 
	ts.processID, tsp.pollutantID,
	fst.fuelTypeID, ts.minModelYearID, ts.maxModelYearID, ts.regClassID,
	lsn.lumpedSpeciesID;

-- End Section Extract Data

-- Section Processing

-- @algorithm
drop table if exists TOGWorkerOutput;
CREATE TABLE IF NOT EXISTS TOGWorkerOutput (
	mechanismID		   SMALLINT NOT NULL DEFAULT 0,
	integratedSpeciesSetID SMALLINT NOT NULL DEFAULT 0,
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
	regClassID			 SMALLINT UNSIGNED NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,
	roadTypeID           SMALLINT UNSIGNED NULL,
	SCC                  CHAR(10) NULL,
	engTechID			 SMALLINT UNSIGNED NULL,
	sectorID 			 SMALLINT UNSIGNED NULL,
	hpID 				 SMALLINT UNSIGNED NULL,
	emissionQuant        DOUBLE NULL,
	emissionRate		 DOUBLE NULL
);
truncate table TOGWorkerOutput;

-- @algorithm Add NMOG (80) to the set of chained input pollutants.
insert ignore into integratedSpeciesSet (mechanismID, integratedSpeciesSetID, pollutantID, useISSyn)
select distinct mechanismID, integratedSpeciesSetID, 80 as pollutantID, 'Y' as useISSyn
from integratedSpeciesSet
where pollutantID <> 80;

-- @algorithm Index integratedSpeciesSet by pollutantID to increase speed.
alter table integratedSpeciesSet add key ISS_pollutantID (pollutantID);

-- @algorithm Find the subset of records that are actually needed.
-- These include NMOG (80) and anything listed in integratedSpeciesSet.
insert into TOGWorkerOutput (
	mechanismID,integratedSpeciesSetID,
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select 0 as mechanismID, 0 as integratedSpeciesSetID,
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate
from MOVESWorkerOutput
where pollutantID in (
	select distinct pollutantID from integratedSpeciesSet
);

-- @algorithm Convert integrated species to negative NonHapTOG (88) entries
-- and NMOG (80) values to positive NonHAPTOG (88) entries. When summed, 
-- this will complete the integration.
insert into TOGWorkerOutput (
	mechanismID,integratedSpeciesSetID,
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select iss.mechanismID, iss.integratedSpeciesSetID,
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	88 as pollutantID,
	processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	case
		when two.pollutantID=80 then emissionQuant
		else -emissionQuant
	end as emissionQuant,
	case
		when two.pollutantID=80 then emissionRate
		else -emissionRate
	end as emissionRate
from TOGWorkerOutput two
inner join integratedSpeciesSet iss using (pollutantID)
where two.mechanismID = 0 and two.integratedSpeciesSetID = 0;

-- @algorithm
drop table if exists TOGWorkerOutputIntegrated;
CREATE TABLE IF NOT EXISTS TOGWorkerOutputIntegrated (
	mechanismID		   SMALLINT NOT NULL DEFAULT 0,
	integratedSpeciesSetID SMALLINT NOT NULL DEFAULT 0,
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
	regClassID			 SMALLINT UNSIGNED NULL,
	fuelTypeID           SMALLINT UNSIGNED NULL,
	modelYearID          SMALLINT UNSIGNED NULL,
	roadTypeID           SMALLINT UNSIGNED NULL,
	SCC                  CHAR(10) NULL,
	engTechID			 SMALLINT UNSIGNED NULL,
	sectorID 			 SMALLINT UNSIGNED NULL,
	hpID 				 SMALLINT UNSIGNED NULL,
	emissionQuant        DOUBLE NULL,
	emissionRate		 DOUBLE NULL,
	
	key (mechanismID, integratedSpeciesSetID, pollutantID)
);
truncate table TOGWorkerOutputIntegrated;

-- @algorithm Sum the NonHAPTOG (88) records, reducing record count
-- and completing the species integration.
insert into TOGWorkerOutputIntegrated (mechanismID, integratedSpeciesSetID,
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select mechanismID, integratedSpeciesSetID,
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	greatest(sum(emissionQuant),0) as emissionQuant,
	greatest(sum(emissionRate),0) as emissionRate
from TOGWorkerOutput
where mechanismID <> 0
group by mechanismID, integratedSpeciesSetID,
	MOVESRunID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC
order by null;

--	engTechID,sectorID,hpID

-- @algorithm Copy NonHAPTOG (88) entries into MOVESWorkerOutput.
insert into MOVESWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,emissionRate,engTechID,sectorID,hpID
)
select MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,emissionRate,engTechID,sectorID,hpID
from TOGWorkerOutputIntegrated
where pollutantID=88 and integratedSpeciesSetID>0
and mechanismID = (select min(mechanismID) from TOGWorkerOutputIntegrated where mechanismID>0);

-- @algorithm Index togSpeciationProfile by pollutantID to increase speed.
alter table togSpeciationProfile add key TSP_pollutantID (pollutantID);

-- @algorithm
drop table if exists togSpeciationPollutants;

create table if not exists togSpeciationPollutants (
	mechanismID smallint not null,
	pollutantID smallint not null,
	primary key (pollutantID, mechanismID)
);

-- @algorithm Get the set of distinct pollutants that are needed for speciation input.
insert into togSpeciationPollutants (mechanismID, pollutantID)
select distinct mechanismID, pollutantID from togSpeciationProfile;

-- @algorithm Find the subset of records that are needed for speciation input.
-- These are the pollutants listed in the togSpeciationProfile table.
insert into TOGWorkerOutputIntegrated (
	mechanismID,integratedSpeciesSetID,
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select mechanismID, 0 as integratedSpeciesSetID,
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	mwo.pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate
from MOVESWorkerOutput mwo
inner join togSpeciationPollutants tsp on (tsp.pollutantID = mwo.pollutantID);

-- @algorithm Speciate NonHAPTOG (88) and anything else listed as an input in togSpeciationProfile.

create table togTemp like MOVESWorkerOutput;

alter table togTemp add key speed1 (
	yearID,monthID,dayID,hourID,linkID,
	pollutantID,
	processID,sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,
	engTechID,sectorID,hpID
);

insert into togTemp (MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,emissionRate,engTechID,sectorID,hpID)
select a.MOVESRunID,a.iterationID,a.yearID,a.monthID,a.dayID,a.hourID,a.stateID,a.countyID,a.zoneID,a.linkID,
	b.outPollutantID as pollutantID,
	a.processID,a.sourceTypeID,a.regClassID,a.fuelTypeID,a.modelYearID,a.roadTypeID,a.SCC,
	emissionQuant*factor as emissionQuant,
	emissionRate*factor as emissionRate,
	a.engTechID,a.sectorID,a.hpID
from TOGWorkerOutputIntegrated a
inner join togSpeciationCountyYear b on (
	a.mechanismID = b.mechanismID
	and a.integratedSpeciesSetID = b.integratedSpeciesSetID
	and a.countyID = b.countyID
	and a.monthID = b.monthID
	and a.yearID = b.yearID
	and a.processID = b.inProcessID
	and a.pollutantID = b.inPollutantID
	and a.fuelTypeID = b.fuelTypeID
	and a.modelYearID >= b.minModelYearID
	and a.modelYearID <= b.maxModelYearID
	and (a.regClassID = b.regClassID or b.regClassID=0));

insert into MOVESWorkerOutput (
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID,linkID,pollutantID,
	processID,sourceTypeID,regClassID,fuelTypeID,modelYearID,roadTypeID,SCC,emissionQuant,emissionRate,engTechID,sectorID,hpID
)
select a.MOVESRunID,a.iterationID,a.yearID,a.monthID,a.dayID,a.hourID,a.stateID,a.countyID,a.zoneID,a.linkID,
	pollutantID,
	a.processID,a.sourceTypeID,a.regClassID,a.fuelTypeID,a.modelYearID,a.roadTypeID,a.SCC,
	sum(emissionQuant) as emissionQuant,
	sum(emissionRate) as emissionRate,
	a.engTechID,a.sectorID,a.hpID
from togTemp a
group by a.yearID,a.monthID,a.dayID,a.hourID,a.linkID,
	pollutantID,
	a.processID,a.sourceTypeID,a.regClassID,a.fuelTypeID,a.modelYearID,a.roadTypeID,a.SCC
order by null;

--	a.engTechID,a.sectorID,a.hpID

-- End Section Processing

-- Section Cleanup
drop table if exists TOGWorkerOutput;
drop table if exists TOGWorkerOutputIntegrated;
drop table if exists togSpeciationCountyYear;
-- End Section Cleanup

-- Section Final Cleanup
-- End Section Final Cleanup
