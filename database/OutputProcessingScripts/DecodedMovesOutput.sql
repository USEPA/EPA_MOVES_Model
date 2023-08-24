-- Decoded MOVES Output Post-Processing Script
-- 
-- Decodes the key fields of MOVESOutput and MOVESActivityOutput tables
-- and creates tables decodedMOVESOutput and decodedMOVESActivityOutput
-- that contain the additional descriptive character fields. These
-- tables get saved to the same output database.

FLUSH TABLES;
SELECT CURRENT_TIME;

--
-- Make DecodedMOVESActivityOutput
--

drop table if exists       decodedmovesactivityoutput;
CREATE TABLE IF NOT EXISTS decodedMOVESActivityOutput
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
       sourceTypeID,
       cast(' ' as varchar( 30)) as sourceTypeName,
       regClassID,
       cast(' ' as varchar(100)) as regClassName,
       fuelTypeID,
       cast(' ' as varchar( 30)) as fuelTypeName,
       modelYearID,
       roadTypeID,
       cast(' ' as varchar( 25)) as roadTypeName,
       SCC,
       activityTypeID,
       cast(' ' as varchar( 50)) as activityTypeDesc,
       activity
from   movesActivityOutput;
create index dayindex on decodedMOVESActivityOutput (dayID);
create index stateindex on decodedMOVESActivityOutput (stateID);
create index countyindex on decodedMOVESActivityOutput (countyID);
create index sourcetypeindex on decodedMOVESActivityOutput (sourceTypeID);
create index regclassindex on decodedMOVESActivityOutput (regClassID);
create index fueltypeindex on decodedMOVESActivityOutput (fuelTypeID);
create index roadtypeindex on decodedMOVESActivityOutput (roadTypeID);
create index activityindex on decodedMOVESActivityOutput (activityTypeID);

update decodedmovesactivityoutput a, translate_day b
SET a.dayName = b.dayName
WHERE a.dayID = b.dayID;
                                                             
update decodedmovesactivityoutput a, translate_state b
SET a.stateAbbr = b.stateAbbr
WHERE a.stateID = b.stateID;
  
update decodedmovesactivityoutput a, translate_county b
SET a.countyName = b.countyName
WHERE a.countyID = b.countyID;

update decodedmovesactivityoutput a, translate_sourcetype b
SET a.sourceTypeName = b.sourceTypeName
WHERE a.sourceTypeID = b.sourceTypeID;
  
update decodedmovesactivityoutput a, translate_regclass b
SET a.regClassName = b.regClassName
WHERE a.regClassID = b.regClassID;
  
update decodedmovesactivityoutput a, translate_fueltype b
SET a.fuelTypeName = b.fuelTypeName
WHERE a.fuelTypeID = b.fuelTypeID;
  
update decodedmovesactivityoutput a, translate_roadtype b
SET a.roadTypeName = b.roadTypeName
WHERE a.roadTypeID = b.roadTypeID;
  
update decodedmovesactivityoutput a, translate_activitytype b
SET a.activityTypeDesc = b.activityTypeDesc
WHERE a.activityTypeID = b.activityTypeID;

--
-- Make DecodedMOVESOutput table
--

drop   table if     exists decodedMOVESoutput;
CREATE TABLE IF NOT EXISTS decodedMOVESoutput
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
	     sourceTypeID,
       cast(' ' as varchar( 30)) as sourceTypeName,
       regClassID,
       cast(' ' as varchar(100)) as regClassName,
	     fuelTypeID,
       cast(' ' as varchar( 30)) as fuelTypeName,
	     modelYearID,
	     roadTypeID,
       cast(' ' as varchar( 25)) as roadTypeName,
	     SCC,
	     emissionQuant
from   movesOutput;
create index dayindex on decodedMOVESOutput (dayID);
create index stateindex on decodedMOVESOutput (stateID);
create index countyindex on decodedMOVESOutput (countyID);
create index pollutantindex on decodedMOVESOutput (pollutantID);
create index processindex on decodedMOVESOutput (processID);
create index sourcetypeindex on decodedMOVESOutput (sourceTypeID);
create index regclassindex on decodedMOVESOutput (regClassID);
create index fueltypeindex on decodedMOVESOutput (fuelTypeID);
create index roadtypeindex on decodedMOVESOutput (roadTypeID);

update decodedMOVESOutput a, translate_day b
SET a.dayName = b.dayName
WHERE a.dayID = b.dayID;
                                                             
update decodedMOVESOutput a, translate_state b
SET a.stateAbbr = b.stateAbbr
WHERE a.stateID = b.stateID;
  
update decodedMOVESOutput a, translate_county b
SET a.countyName = b.countyName
WHERE a.countyID = b.countyID;

update decodedMOVESOutput a, translate_pollutant b
SET a.pollutantName = b.pollutantName
WHERE a.pollutantID = b.pollutantID;

update decodedMOVESOutput a, translate_process b
SET a.processName = b.processName
WHERE a.processID = b.processID;

update decodedMOVESOutput a, translate_sourcetype b
SET a.sourceTypeName = b.sourceTypeName
WHERE a.sourceTypeID = b.sourceTypeID;
  
update decodedMOVESOutput a, translate_regclass b
SET a.regClassName = b.regClassName
WHERE a.regClassID = b.regClassID;
  
update decodedMOVESOutput a, translate_fueltype b
SET a.fuelTypeName = b.fuelTypeName
WHERE a.fuelTypeID = b.fuelTypeID;
  
update decodedMOVESOutput a, translate_roadtype b
SET a.roadTypeName = b.roadTypeName
WHERE a.roadTypeID = b.roadTypeID;

FLUSH TABLES;
