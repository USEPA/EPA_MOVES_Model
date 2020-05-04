-- --------------------------------------------------------------------------------------
-- Convert a MOVES2010A CDM or PDM database to 2010B format.
-- No changes are made to the original input database.
-- Run this script in the context of a new database.
-- Populate ##defaultdb## with the name of the default database.
-- Populate ##inputdb## with the name of the original 2010A CDM or PDM database.
--
-- Author Wesley Faler
-- Version 2011-10-31
-- --------------------------------------------------------------------------------------

-- Provide tables not present in a 2010A database
create table if not exists avft like ##defaultdb##.avft;
create table if not exists sccRoadTypeDistribution like ##defaultdb##.sccRoadTypeDistribution;

-- Copy tables in a 2010A database
create table if not exists avgSpeedDistribution like ##inputdb##.avgSpeedDistribution;
create table if not exists county like ##inputdb##.county;
create table if not exists countyYear like ##inputdb##.countyYear;
create table if not exists dayVMTFraction like ##inputdb##.dayVMTFraction;
create table if not exists driveScheduleSecondLink like ##inputdb##.driveScheduleSecondLink;
create table if not exists fuelFormulation like ##inputdb##.fuelFormulation;
create table if not exists fuelSupply like ##inputdb##.fuelSupply;
create table if not exists hourVMTFraction like ##inputdb##.hourVMTFraction;
create table if not exists hpmsVTypeYear like ##inputdb##.hpmsVTypeYear;
create table if not exists IMCoverage like ##inputdb##.IMCoverage;
create table if not exists link like ##inputdb##.link;
create table if not exists linkSourceTypeHour like ##inputdb##.linkSourceTypeHour;
create table if not exists monthVMTFraction like ##inputdb##.monthVMTFraction;
create table if not exists offNetworkLink like ##inputdb##.offNetworkLink;
create table if not exists opModeDistribution like ##inputdb##.opModeDistribution;
create table if not exists roadType like ##inputdb##.roadType;
create table if not exists roadTypeDistribution like ##inputdb##.roadTypeDistribution;
create table if not exists sourceTypeAgeDistribution like ##inputdb##.sourceTypeAgeDistribution;
create table if not exists sourceTypeYear like ##inputdb##.sourceTypeYear;
create table if not exists state like ##inputdb##.state;
create table if not exists year like ##inputdb##.year;
create table if not exists zone like ##inputdb##.zone;
create table if not exists zoneMonthHour like ##inputdb##.zoneMonthHour;
create table if not exists zoneRoadType like ##inputdb##.zoneRoadType;

-- Provide tables that are needed in 2010B but may not have been in the 2010A database
create table if not exists avgSpeedDistribution like ##defaultdb##.avgSpeedDistribution;
create table if not exists county like ##defaultdb##.county;
create table if not exists countyYear like ##defaultdb##.countyYear;
create table if not exists dayVMTFraction like ##defaultdb##.dayVMTFraction;
create table if not exists driveScheduleSecondLink like ##defaultdb##.driveScheduleSecondLink;
create table if not exists fuelFormulation like ##defaultdb##.fuelFormulation;
create table if not exists fuelSupply like ##defaultdb##.fuelSupply;
create table if not exists hourVMTFraction like ##defaultdb##.hourVMTFraction;
create table if not exists hpmsVTypeYear like ##defaultdb##.hpmsVTypeYear;
create table if not exists IMCoverage like ##defaultdb##.IMCoverage;
create table if not exists link like ##defaultdb##.link;
create table if not exists linkSourceTypeHour like ##defaultdb##.linkSourceTypeHour;
create table if not exists monthVMTFraction like ##defaultdb##.monthVMTFraction;
create table if not exists offNetworkLink like ##defaultdb##.offNetworkLink;
create table if not exists opModeDistribution like ##defaultdb##.opModeDistribution;
create table if not exists roadType like ##defaultdb##.roadType;
create table if not exists roadTypeDistribution like ##defaultdb##.roadTypeDistribution;
create table if not exists sourceTypeAgeDistribution like ##defaultdb##.sourceTypeAgeDistribution;
create table if not exists sourceTypeYear like ##defaultdb##.sourceTypeYear;
create table if not exists state like ##defaultdb##.state;
create table if not exists year like ##defaultdb##.year;
create table if not exists zone like ##defaultdb##.zone;
create table if not exists zoneMonthHour like ##defaultdb##.zoneMonthHour;
create table if not exists zoneRoadType like ##defaultdb##.zoneRoadType;

-- Copy data from the input database.
-- Errors here are ignored by the scripting engine and occur when a table
-- is not present in the input database.
insert ignore into avgSpeedDistribution select * from ##inputdb##.avgSpeedDistribution;
insert ignore into county select * from ##inputdb##.county;
insert ignore into countyYear select * from ##inputdb##.countyYear;
insert ignore into dayVMTFraction select * from ##inputdb##.dayVMTFraction;
insert ignore into driveScheduleSecondLink select * from ##inputdb##.driveScheduleSecondLink;
insert ignore into fuelFormulation select * from ##inputdb##.fuelFormulation;
insert ignore into fuelSupply select * from ##inputdb##.fuelSupply;
insert ignore into hourVMTFraction select * from ##inputdb##.hourVMTFraction;
insert ignore into hpmsVTypeYear select * from ##inputdb##.hpmsVTypeYear;
insert ignore into IMCoverage select * from ##inputdb##.IMCoverage;
insert ignore into link select * from ##inputdb##.link;
insert ignore into linkSourceTypeHour select * from ##inputdb##.linkSourceTypeHour;
insert ignore into monthVMTFraction select * from ##inputdb##.monthVMTFraction;
insert ignore into offNetworkLink select * from ##inputdb##.offNetworkLink;
insert ignore into opModeDistribution select * from ##inputdb##.opModeDistribution;
insert ignore into roadType select * from ##inputdb##.roadType;
insert ignore into roadTypeDistribution select * from ##inputdb##.roadTypeDistribution;
insert ignore into sourceTypeAgeDistribution select * from ##inputdb##.sourceTypeAgeDistribution;
insert ignore into sourceTypeYear select * from ##inputdb##.sourceTypeYear;
insert ignore into state select * from ##inputdb##.state;
insert ignore into year select * from ##inputdb##.year;
insert ignore into zone select * from ##inputdb##.zone;
insert ignore into zoneMonthHour select * from ##inputdb##.zoneMonthHour;

insert ignore into roadType select * from ##defaultdb##.roadType;

insert ignore into zoneRoadType (zoneID, roadTypeID, SHOAllocFactor)
select zoneID, roadTypeID, 1 as SHOAllocFactor
from zone, ##defaultdb##.roadType
where roadTypeID <> 1;

drop table if exists zoneTemp;

create table zoneTemp
select roadTypeID, sum(SHOAllocFactor) as sumSHOAllocFactor
from ZoneRoadType zrt
inner join Zone z using (zoneID)
group by roadTypeID;

update ZoneRoadType, zoneTemp, Zone set
SHOAllocFactor=(case when (sumSHOAllocFactor is null or sumSHOAllocFactor <= 0) then 1 else (SHOAllocFactor/sumSHOAllocFactor) end)
where ZoneRoadType.zoneID=Zone.zoneID
and ZoneRoadType.roadTypeID=zoneTemp.roadTypeID;

drop table if exists zoneTemp;

-- Bring the fuelFormulation table up to 2010B
alter table fuelFormulation add column T50 float default null;
alter table fuelFormulation add column T90 float default null;

update fuelFormulation set T50 = 2.0408163 * (147.91 - e200)
where e200 is not null and e200 > 0
and (T50 is null or T50 <= 0);

update fuelFormulation set T90 = 4.5454545 * (155.47 - e300)
where e300 is not null and e300 > 0
and (T90 is null or T90 <= 0);

update fuelFormulation set e200 = 147.91-(T50/2.0408163)
where T50 is not null and T50 > 0
and (e200 is null or e200 <= 0);

update fuelFormulation set e300 = 155.47-(T90/4.5454545)
where T90 is not null and T90 > 0
and (e300 is null or e300 <= 0);

-- fix the default fuelformulationid = 10

drop table if exists temp;
create table temp like fuelformulation;
insert into temp select * from ##defaultdb##.fuelformulation where fuelformulationid = 98;
update temp set fuelformulationid = 10 where fuelformulationid = 98;
replace into fuelformulation select * from temp where fuelformulationid = 10;

drop table if exists temp;

-- Check for warning or error messages
drop procedure if exists spConvert2010ACDMPDM;

BeginBlock
create procedure spConvert2010ACDMPDM()
begin
	declare howMany int default 0;
	declare isCustomDomain int default 0;

	set howMany=0;
	select count(*) into howMany from County where stateID=99;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		set isCustomDomain=1;
	end if;

	if(isCustomDomain > 0) then
		set howMany=0;
		select count(*) into howMany
		from county
		inner join zone using (countyID)
		inner join sccRoadTypeDistribution using (zoneID)
		where stateID=99;
		set howMany=ifnull(howMany,0);
		if(howMany <= 0) then
			insert into convertTempMessages (message)
			select concat('WARNING: custom county ',countyID,' requires data in the sccRoadTypeDistribution table to obtain SCC results.')
			from county
			where stateID=99;
		end if;
	end if;
end
EndBlock

call spConvert2010ACDMPDM();
drop procedure if exists spConvert2010ACDMPDM;
