-- Create utility tables in the MOVESExecution database.
-- Author Harvey Michaels
-- Author Wesley Faler
-- Version 2017-03-22

-- --------------------------------------------------------------------
-- Fill the set of regions used by the runspec
-- --------------------------------------------------------------------
insert into runSpecFuelRegion (fuelRegionID)
select distinct regionID as fuelRegionID
from regionCounty
inner join runSpecCounty using (countyID)
where regionCodeID=1;

-- --------------------------------------------------------------------
-- Add indexes and extra columns needed for runtime but not in the default database.
-- --------------------------------------------------------------------
alter table OpModeDistribution add key (linkID, polProcessID, sourceTypeID);

-- --------------------------------------------------------------------
-- Filter any disabled mechanism and speciation profile data
-- --------------------------------------------------------------------
delete from mechanismName
where not exists (
	select *
	from runSpecPollutant
	inner join pollutant using (pollutantID)
	inner join pollutantDisplayGroup using (pollutantDisplayGroupID)
	where pollutantDisplayGroupName='Mechanisms'
	and pollutantID = 1000 + ((mechanismID-1) * 500)
);

delete from integratedSpeciesSet where useISSyn<>'Y';

delete from integratedSpeciesSet
where not exists (
	select * from mechanismName
	where mechanismName.mechanismID = integratedSpeciesSet.mechanismID);

delete from integratedSpeciesSetName
where not exists (
	select * from integratedSpeciesSet 
	where integratedSpeciesSet.integratedSpeciesSetID = integratedSpeciesSetName.integratedSpeciesSetID);

-- Remove TOGSpeciationProfile entries that use inactive integratedSpeciesSet entries.
-- Keep any TOGSpeciationProfile entries that are not tied to any integratedSpeciesSet.
delete from TOGSpeciationProfile
where integratedSpeciesSetID <> 0
and not exists (
	select * from integratedSpeciesSet 
	where integratedSpeciesSet.integratedSpeciesSetID = TOGSpeciationProfile.integratedSpeciesSetID);

delete from lumpedSpeciesName
where not exists (
	select * from TOGSpeciationProfile
	where TOGSpeciationProfile.lumpedSpeciesName = lumpedSpeciesName.lumpedSpeciesName);

delete from togSpeciation
where not exists (
	select * from TOGSpeciationProfile
	where TOGSpeciationProfile.togSpeciationProfileID = togSpeciation.togSpeciationProfileID);

-- Expand togSpeciationProfile entries that use wildcard togSpeciationPofileID=0
drop table if exists TOGTemp;
create table TOGTemp
select distinct mechanismID, TOGspeciationProfileID, pollutantID, lumpedSpeciesName
from togSpeciationProfile;

alter table TOGTemp add unique key (mechanismID, TOGspeciationProfileID, pollutantID, lumpedSpeciesName);

insert ignore into TOGSpeciationProfile (mechanismID, TOGspeciationProfileID, integratedSpeciesSetID, 
	pollutantID, lumpedSpeciesName,
	TOGSpeciationDivisor, TOGSpeciationMassFraction)
select distinct tsp.mechanismID, ts.TOGspeciationProfileID, tsp.integratedSpeciesSetID, 
	tsp.pollutantID, tsp.lumpedSpeciesName,
	tsp.TOGSpeciationDivisor, tsp.TOGSpeciationMassFraction
from togSpeciationProfile tsp, togSpeciation ts
where tsp.togSpeciationProfileID='0'
and not exists (
	select *
	from TOGTemp t
	where t.mechanismID=tsp.mechanismID
	and t.TOGspeciationProfileID=ts.TOGspeciationProfileID
	and t.pollutantID=tsp.pollutantID
	and t.lumpedSpeciesName=tsp.lumpedSpeciesName
);

drop table if exists TOGTemp;

-- Delete any wildcard togSpeciationProfile entries after expansion
delete from togSpeciationProfile where togSpeciationProfileID='0';

-- Delete any togSpeciationProfile entries for integratedSpeciesSetID=0
-- that are represented by a non-zero integratedSpeciesSetID.
drop table if exists TOGTemp;
create table TOGTemp
select distinct mechanismID, TOGspeciationProfileID, pollutantID, lumpedSpeciesName
from togSpeciationProfile
where integratedSpeciesSetID<>0;

alter table TOGTemp add unique key (mechanismID, TOGspeciationProfileID, pollutantID, lumpedSpeciesName);

delete from togSpeciationProfile
where integratedSpeciesSetID=0
and exists (
	select *
	from TOGTemp t
	where t.mechanismID=togSpeciationProfile.mechanismID
	and t.TOGspeciationProfileID=togSpeciationProfile.TOGspeciationProfileID
	and t.pollutantID=togSpeciationProfile.pollutantID
	and t.lumpedSpeciesName=togSpeciationProfile.lumpedSpeciesName
);

drop table if exists TOGTemp;

-- Remove unused TOG profile names
delete from TOGSpeciationProfileName
where not exists (
	select * from TOGSpeciationProfile
	where TOGSpeciationProfile.togSpeciationProfileID = TOGSpeciationProfileName.togSpeciationProfileID);

-- Decode the togSpeciation.modelYearGroupID
alter table togSpeciation add minModelYearID smallint not null default 0;
alter table togSpeciation add maxModelYearID smallint not null default 0;

update togSpeciation set minModelYearID = floor(modelYearGroupID/10000),
	maxModelYearID = mod(modelYearGroupID,10000)
where minModelYearID=0 or maxModelYearID=0;

-- --------------------------------------------------------------------
-- Provide a place for THC E85/E10 fuel adjustments. These are special cases
-- to match E10. These ratios are made with E85 fuel properties except
-- using an E10 RVP.
-- --------------------------------------------------------------------
create table altCriteriaRatio like criteriaRatio;
