-- TOG Speciation calculator
-- Version 2015-07-15
-- Author Wes Faler

-- @algorithm
-- @owner TOG Speciation Calculator

-- Section Create Remote Tables for Extracted Data

##create.integratedSpeciesSet##;
TRUNCATE TABLE integratedSpeciesSet;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache select integratedSpeciesSet.*
into outfile '##integratedSpeciesSet##'
from integratedSpeciesSet
where mechanismID in (##mechanismIDs##);

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
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
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

-- @algorithm Sum the NonHAPTOG (88) records, reducing record count
-- and completing the species integration.
insert into MOVESWorkerOutput (
	MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select
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

-- End Section Processing

-- Section Cleanup
drop table if exists TOGWorkerOutput;
-- End Section Cleanup

-- Section Final Cleanup
-- End Section Final Cleanup
