-- MOVES Post-Processing MySQL Script
-- Decodes most key fields of MOVESOutput and MOVESActivityOutput Tables
-- Creating tables DecodedMOVESOutput and DecodedMOVESActivityOutput
--   that contain the additional descriptive character fields.
-- This version written 9/22/2005
-- This version rewritten 2/12/2015 to include additional fields.
-- Updated 5/20/2015 to restore output for activity.
-- Updated 9/10/2015 to add fuelsubtype.

--
--
-- uses text replacement of MOVES where ##defaultdb## is replaced with the Current Default Database,
-- to serve as a source of the relevant category tables listing legal values.  updated 6/20/2014

--
--
-- decodeMOVESoutput.sql

FLUSH TABLES;
SELECT CURRENT_TIME;

--
-- Make DecodedMOVESActivityOutput
--

drop table if exists       decodedmovesactivityoutput;
CREATE TABLE IF NOT EXISTS decodedMOVESActivityOutput
select MovesRunId,
       iterationId,
       yearId,
       monthId,
       dayId,
       cast(' ' as char( 10)) as dayName,
       hourId,
       stateId,
       cast(' ' as char(  2)) as stateAbbr,
       countyId,
       cast(' ' as char( 45)) as countyName,
       zoneId,
       linkId,
       sourceTypeId,
       cast(' ' as char( 30)) as sourceTypeName,
       regClassId,
       cast(' ' as char(100)) as regClassName,
       fuelTypeId,
       cast(' ' as char( 30)) as fuelTypeDesc,
       modelYearId,
       roadTypeId,
       cast(' ' as char( 25)) as roadDesc,
       SCC,
       engTechId,
       cast(' ' as char( 50)) as engTechName,
       sectorId,
       cast(' ' as char( 40)) as sectorDesc,
       hpId,
       activityTypeId,
       cast(' ' as char( 50)) as activityTypeDesc,
       activity,
       activityMean,
       activitySigma
from   movesActivityOutput;

update DecodedMOVESActivityOutput as a set dayName        = (select b.dayName
                                                             from   ##defaultdb##.dayOfAnyWeek as b
                                                             where  a.dayId = b.dayId);

update DecodedMOVESActivityOutput as a set stateABBR      = (select b.stateAbbr
                                                             from   ##defaultdb##.state as b
                                                             where  a.stateId = b.stateId);

update DecodedMOVESActivityOutput as a set countyName     = (select b.countyName
                                                             from   ##defaultdb##.county as b
                                                             where  a.countyId = b.countyId);

update DecodedMOVESActivityOutput as a set sourceTypeName = (select b.sourceTypeName
                                                             from   ##defaultdb##.sourceUseType as b
                                                             where  a.sourceTypeId = b.sourceTypeId);

update DecodedMOVESActivityOutput as a set regClassName   = (select b.regClassName
                                                              from  ##defaultdb##.regulatoryClass as b
                                                              where a.regClassId = b.regClassId);

update DecodedMOVESActivityOutput as a set fuelTypeDesc   = (select b.fuelTypeDesc
                                                             from   ##defaultdb##.fuelType as b
                                                             where  a.fuelTypeId = b.fuelTypeId);


update DecodedMOVESActivityOutput as a set roadDesc       = (select b.roadDesc
                                                             from   ##defaultdb##.roadType as b
                                                             where  a.roadTypeId = b.roadTypeId);

update DecodedMOVESActivityOutput as a set engTechName    = (select b.engTechName
                                                             from   ##defaultdb##.engineTech as b
                                                             where  a.engTechId = b.engTechId);

update DecodedMOVESActivityOutput as a set sectorDesc     = (select b.description
                                                             from   ##defaultdb##.sector as b
                                                             where  a.sectorId = b.sectorId);

update DecodedMOVESActivityOutput as a set activityTypeDesc
                                                          = (select b.activityTypeDesc
                                                             from   .activityType as b
                                                             where  a.activityTypeId = b.activityTypeId);

-- select * from DecodedMOVESActivityOutput;



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
       cast(' ' as char(10))  as dayName,
	     hourID,
	     stateID,
       cast(' ' as char(  2)) as stateABBR,
	     countyID,
       cast(' ' as char( 45)) as countyName,
	     zoneID,
	     linkID,
	     pollutantID,
       cast(' ' as char( 50)) as pollutantName,
	     processID,
       cast(' ' as char( 50)) as processName,
	     sourceTypeID,
       cast(' ' as char( 30)) as sourceTypeName,
       regClassId,
       cast(' ' as char(100)) as regClassName,
	     fuelTypeID,
       cast(' ' as char( 30)) as fuelTypeDesc,
       fuelSubTypeId,
       cast(' ' as char(50))  as fuelSubTypeDesc,
	     modelYearID,
	     roadTypeID,
       cast(' ' as char( 25)) as roadDesc,
	     SCC,
       engTechId,
       cast(' ' as char( 50)) as engTechName,
       sectorId,
       cast(' ' as char( 40)) as sectorDesc,
       hpId,
	     emissionQuant,
	     emissionQuantMean,
	     emissionQuantSigma
from   movesOutput;

update DecodedMOVESOutput as a set dayName        = (select b.dayName
                                                     from   ##defaultdb##.dayOfAnyWeek as b
                                                     where  a.dayId = b.dayId);

update DecodedMOVESOutput as a set stateABBR      = (select b.stateAbbr
                                                     from   ##defaultdb##.state as b
                                                             where  a.stateId = b.stateId);

update DecodedMOVESOutput as a set countyName     = (select b.countyName
                                                     from   ##defaultdb##.county as b
                                                     where  a.countyId = b.countyId);

update DecodedMOVESOutput as a set pollutantName  = (select b.pollutantName
                                                     from   ##defaultdb##.pollutant as b
                                                     where  a.pollutantId = b.pollutantId);

update DecodedMOVESOutput as a set processName    = (select b.processName
                                                     from   ##defaultdb##.emissionProcess as b
                                                     where  a.processId = b.processId);

update DecodedMOVESOutput as a set sourceTypeName = (select b.sourceTypeName
                                                     from   ##defaultdb##.sourceUseType as b
                                                     where  a.sourceTypeId = b.sourceTypeId);

update DecodedMOVESOutput as a set regClassName   = (select b.regClassName
                                                     from  ##defaultdb##.regulatoryClass as b
                                                     where a.regClassId = b.regClassId);

update DecodedMOVESOutput as a set fuelTypeDesc   = (select b.fuelTypeDesc
                                                     from   ##defaultdb##.fuelType as b
                                                     where  a.fuelTypeId = b.fuelTypeId);

update DecodedMOVESOutput as a set fuelSubtypeDesc =(select b.fuelSubTypeDesc
                                                     from   ##defaultdb##.fuelsubtype as b
                                                     where  a.fuelSubtypeId = b.fuelSubTypeId);

update DecodedMOVESOutput as a set roadDesc       = (select b.roadDesc
                                                     from   ##defaultdb##.roadType as b
                                                     where  a.roadTypeId = b.roadTypeId);

update DecodedMOVESOutput as a set engTechName    = (select b.engTechName
                                                     from   ##defaultdb##.engineTech as b
                                                     where  a.engTechId = b.engTechId);

update DecodedMOVESOutput as a set sectorDesc     = (select b.description
                                                     from   ##defaultdb##.sector as b
                                                     where  a.sectorId = b.sectorId);



-- select * from DecodedMOVESOutput;

FLUSH TABLES;