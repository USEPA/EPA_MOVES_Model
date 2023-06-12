-- Decoded Nonroad Output Post-Processing Script
-- 
-- Decodes the key fields of MOVESOutput and MOVESActivityOutput tables
-- and creates tables decodedNonroadOutput and decodedNonroadActivityOutput
-- that contain the additional descriptive character fields. These
-- tables get saved to the same output database.
-- 
-- Note that this script does not generate a spreadsheet of results,
-- like other Nonroad Post-Processing Scripts.

FLUSH TABLES;
SELECT CURRENT_TIME;

--
-- Make DecodedNonroadActivityOutput
--

drop table if exists       decodednonroadactivityoutput;
CREATE TABLE IF NOT EXISTS decodedNonroadActivityOutput
select MovesRunID,
       iterationID,
       yearID,
       monthID,
       dayID,
       cast(' ' as varchar( 10)) as dayName,
       hourID,
       stateID,
       cast(' ' as char(  2)) as stateAbbr,
       countyID,
       cast(' ' as varchar( 45)) as countyName,
       zoneID,
       linkID,
       fuelTypeID,
       cast(' ' as varchar( 30)) as fuelTypeName,
	   fuelSubTypeID,
       cast(' ' as varchar( 50)) as fuelSubTypeName,
       modelYearID,
       SCC,
       cast(' ' as varchar( 40)) as sccName,
       engTechID,
       cast(' ' as varchar( 80)) as engTechName,
       sectorID,
       cast(' ' as varchar( 40)) as sectorName,
       hpID,
       cast(' ' as varchar( 20)) as hpName,
       activityTypeID,
       cast(' ' as varchar( 50)) as activityTypeDesc,
       activity
from   movesActivityOutput;
create index dayindex on decodedNonroadActivityOutput (dayID);
create index stateindex on decodedNonroadActivityOutput (stateID);
create index countyindex on decodedNonroadActivityOutput (countyID);
create index fueltypeindex on decodedNonroadActivityOutput (fuelTypeID);
create index fuelsubtypeindex on decodedNonroadActivityOutput (fuelSubtypeID);
create index sccindex on decodedNonroadActivityOutput (scc);
create index engtechindex on decodedNonroadActivityOutput (engTechID);
create index sectorindex on decodedNonroadActivityOutput (sectorID);
create index hpindex on decodedNonroadActivityOutput (hpID);
create index activityindex on decodedNonroadActivityOutput (activityTypeID);

update decodedNonroadActivityOutput a, translate_day b
SET a.dayName = b.dayName
WHERE a.dayID = b.dayID;
                                                             
update decodedNonroadActivityOutput a, translate_state b
SET a.stateAbbr = b.stateAbbr
WHERE a.stateID = b.stateID;
  
update decodedNonroadActivityOutput a, translate_county b
SET a.countyName = b.countyName
WHERE a.countyID = b.countyID;

update decodedNonroadActivityOutput a, translate_fueltype b
SET a.fuelTypeName = b.fuelTypeName
WHERE a.fuelTypeID = b.fuelTypeID;

update decodedNonroadActivityOutput a, translate_fuelsubtype b
SET a.fuelSubtypeName = b.fuelSubtypeName
WHERE a.fuelSubtypeID = b.fuelSubtypeID;

update decodedNonroadActivityOutput a, translate_nrscc b
SET a.sccName = b.sccName
WHERE a.scc = b.scc;

update decodedNonroadActivityOutput a, translate_engtech b
SET a.engTechName = b.engTechName
WHERE a.engTechID = b.engTechID;

update decodedNonroadActivityOutput a, translate_sector b
SET a.sectorName = b.sectorName
WHERE a.sectorID = b.sectorID;

update decodedNonroadActivityOutput a, translate_hp b
SET a.hpName = b.hpName
WHERE a.hpID = b.hpID;

update decodedNonroadActivityOutput a, translate_activitytype b
SET a.activityTypeDesc = b.activityTypeDesc
WHERE a.activityTypeID = b.activityTypeID;

--
-- Make decodedNonroadOutput table
--

drop   table if     exists decodedNonroadOutput;
CREATE TABLE IF NOT EXISTS decodedNonroadOutput
select MOVESRunID,
	     iterationID,
	     yearID,
	     monthID,
	     dayID,
       cast(' ' as varchar(10))  as dayName,
	     hourID,
	     stateID,
       cast(' ' as char(  2)) as stateABBR,
	     countyID,
       cast(' ' as varchar( 45)) as countyName,
	     zoneID,
	     linkID,
	     pollutantID,
       cast(' ' as varchar( 50)) as pollutantName,
	     processID,
       cast(' ' as varchar( 50)) as processName,
	     fuelTypeID,
       cast(' ' as varchar( 30)) as fuelTypeName,
         fuelSubTypeID,
       cast(' ' as varchar(50))  as fuelSubTypeName,
	     modelYearID,
	     roadTypeID,
	     SCC,
       cast(' ' as varchar( 50)) as sccName,
         engTechID,
       cast(' ' as varchar( 80)) as engTechName,
         sectorID,
       cast(' ' as varchar( 40)) as sectorName,
         hpID,
       cast(' ' as varchar( 20)) as hpName,
	     emissionQuant
from   movesOutput;
create index dayindex on decodedNonroadOutput (dayID);
create index stateindex on decodedNonroadOutput (stateID);
create index countyindex on decodedNonroadOutput (countyID);
create index pollutantindex on decodedNonroadOutput (pollutantID);
create index processindex on decodedNonroadOutput (processID);
create index fueltypeindex on decodedNonroadOutput (fuelTypeID);
create index fuelsubtypeindex on decodedNonroadOutput (fuelSubtypeID);
create index sccindex on decodedNonroadOutput (scc);
create index engtechindex on decodedNonroadOutput (engTechID);
create index sectorindex on decodedNonroadOutput (sectorID);
create index hpindex on decodedNonroadOutput (hpID);

update decodedNonroadOutput a, translate_day b
SET a.dayName = b.dayName
WHERE a.dayID = b.dayID;
                                                             
update decodedNonroadOutput a, translate_state b
SET a.stateAbbr = b.stateAbbr
WHERE a.stateID = b.stateID;
  
update decodedNonroadOutput a, translate_county b
SET a.countyName = b.countyName
WHERE a.countyID = b.countyID;

update decodedNonroadOutput a, translate_pollutant b
SET a.pollutantName = b.pollutantName
WHERE a.pollutantID = b.pollutantID;

update decodedNonroadOutput a, translate_process b
SET a.processName = b.processName
WHERE a.processID = b.processID;

update decodedNonroadOutput a, translate_fueltype b
SET a.fuelTypeName = b.fuelTypeName
WHERE a.fuelTypeID = b.fuelTypeID;

update decodedNonroadOutput a, translate_fuelsubtype b
SET a.fuelSubtypeName = b.fuelSubtypeName
WHERE a.fuelSubtypeID = b.fuelSubtypeID;

update decodedNonroadOutput a, translate_nrscc b
SET a.sccName = b.sccName
WHERE a.scc = b.scc;

update decodedNonroadOutput a, translate_engtech b
SET a.engTechName = b.engTechName
WHERE a.engTechID = b.engTechID;

update decodedNonroadOutput a, translate_sector b
SET a.sectorName = b.sectorName
WHERE a.sectorID = b.sectorID;

update decodedNonroadOutput a, translate_hp b
SET a.hpName = b.hpName
WHERE a.hpID = b.hpID;

FLUSH TABLES;
