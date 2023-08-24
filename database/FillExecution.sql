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

-- --------------------------------------------------------------------
-- Provide a place for THC E85/E10 fuel adjustments. These are special cases
-- to match E10. These ratios are made with E85 fuel properties except
-- using an E10 RVP.
-- --------------------------------------------------------------------
create table altCriteriaRatio like criteriaRatio;
