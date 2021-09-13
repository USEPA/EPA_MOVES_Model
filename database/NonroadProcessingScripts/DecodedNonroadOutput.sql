-- Decoded Nonroad Output
-- 
-- Decodes the key fields of MOVESOutput and MOVESActivityOutput tables
-- and creates tables DecodedMOVESOutput and DecodedMOVESActivityOutput
-- that contain the additional descriptive character fields. These
-- tables get saved to the same output database.
-- 
-- Note that this script does not generate a spreadsheet of results,
-- like other Nonroad Post-Processing Scripts.

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
       regClassId,
       fuelTypeId,
       cast(' ' as char( 30)) as fuelTypeDesc,
	   fuelSubTypeId,
       cast(' ' as char( 50)) as fuelSubTypeDesc,
       modelYearId,
       roadTypeId,
       SCC,
       cast(' ' as char( 40)) as sccDesc,
       engTechId,
       cast(' ' as char( 80)) as engTechDesc,
       sectorId,
       cast(' ' as char( 40)) as sectorDesc,
       hpId,
       cast(' ' as char( 20)) as hpBinName,
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

update DecodedMOVESActivityOutput as a set fuelTypeDesc   = (select b.fuelTypeDesc
                                                             from   ##defaultdb##.nrfuelType as b
                                                             where  a.fuelTypeId = b.fuelTypeId);

update DecodedMOVESActivityOutput as a set fuelSubTypeDesc= (select b.fuelSubTypeDesc
                                                             from   ##defaultdb##.nrfuelsubType as b
                                                             where  a.fuelSubTypeId = b.fuelSubTypeId);

update DecodedMOVESActivityOutput as a set sccDesc        = (select b.description
                                                             from   ##defaultdb##.nrscc as b
                                                             where  a.scc = b.scc);

update DecodedMOVESActivityOutput as a set engTechDesc    = (select b.engTechDesc
                                                             from   ##defaultdb##.engineTech as b
                                                             where  a.engTechId = b.engTechId);

update DecodedMOVESActivityOutput as a set sectorDesc     = (select b.description
                                                             from   ##defaultdb##.sector as b
                                                             where  a.sectorId = b.sectorId);

update DecodedMOVESActivityOutput as a set hpBinName      = (select b.binName
                                                             from   ##defaultdb##.nrhprangebin as b
                                                             where  a.hpID = b.NRHPRangeBinID);

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
       regClassId,
	     fuelTypeID,
       cast(' ' as char( 30)) as fuelTypeDesc,
       fuelSubTypeId,
       cast(' ' as char(50))  as fuelSubTypeDesc,
	     modelYearID,
	     roadTypeID,
	     SCC,
       cast(' ' as char( 50)) as sccDesc,
       engTechId,
       cast(' ' as char( 80)) as engTechDesc,
       sectorId,
       cast(' ' as char( 40)) as sectorDesc,
       hpId,
       cast(' ' as char( 20)) as hpBinName,
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

update DecodedMOVESOutput as a set fuelTypeDesc   = (select b.fuelTypeDesc
                                                     from   ##defaultdb##.nrfuelType as b
                                                     where  a.fuelTypeId = b.fuelTypeId);

update DecodedMOVESOutput as a set fuelSubtypeDesc =(select b.fuelSubTypeDesc
                                                     from   ##defaultdb##.nrfuelsubtype as b
                                                     where  a.fuelSubtypeId = b.fuelSubTypeId);

update DecodedMOVESOutput as a set sccDesc         = (select b.description
                                                      from   ##defaultdb##.nrscc as b
                                                      where  a.scc = b.scc);

update DecodedMOVESOutput as a set engTechDesc    = (select b.engTechDesc
                                                     from   ##defaultdb##.engineTech as b
                                                     where  a.engTechId = b.engTechId);

update DecodedMOVESOutput as a set sectorDesc     = (select b.description
                                                     from   ##defaultdb##.sector as b
                                                     where  a.sectorId = b.sectorId);

update DecodedMOVESOutput as a set hpBinName      = (select b.binName
                                                     from   ##defaultdb##.nrhprangebin as b
                                                     where  a.hpID = b.NRHPRangeBinID);



-- select * from DecodedMOVESOutput;

FLUSH TABLES;