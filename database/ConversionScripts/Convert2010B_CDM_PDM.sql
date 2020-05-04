-- --------------------------------------------------------------------------------------
-- Convert a MOVES2010B CDM or PDM database to MOVES2014 format.
-- No changes are made to the original input database.
-- Run this script in the context of a new database.
-- Populate ##defaultdb## with the name of the default database.
-- Populate ##inputdb## with the name of the original 2010B CDM or PDM database.
--
-- Author Wesley Faler
-- Version 2014-07-23
-- --------------------------------------------------------------------------------------

-- Copy tables in a 2010B database
create table if not exists avft like ##inputdb##.avft;
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
-- create table if not exists sccRoadTypeDistribution like ##inputdb##.sccRoadTypeDistribution
create table if not exists sourceTypeAgeDistribution like ##inputdb##.sourceTypeAgeDistribution;
create table if not exists sourceTypeYear like ##inputdb##.sourceTypeYear;
create table if not exists state like ##inputdb##.state;
create table if not exists year like ##inputdb##.year;
create table if not exists zone like ##inputdb##.zone;
create table if not exists zoneMonthHour like ##inputdb##.zoneMonthHour;
create table if not exists zoneRoadType like ##inputdb##.zoneRoadType;

-- Provide tables that are needed in MOVES2014 but may not have been in the 2010B database
create table if not exists avft like ##defaultdb##.avft;
create table if not exists avgSpeedDistribution like ##defaultdb##.avgSpeedDistribution;
create table if not exists county like ##defaultdb##.county;
create table if not exists countyYear like ##defaultdb##.countyYear;
create table if not exists dayVMTFraction like ##defaultdb##.dayVMTFraction;
create table if not exists driveScheduleSecondLink like ##defaultdb##.driveScheduleSecondLink;
create table if not exists fuelFormulation like ##defaultdb##.fuelFormulation;
create table if not exists fuelSupply like ##defaultdb##.fuelSupply;
create table if not exists fuelusagefraction like ##defaultdb##.fuelusagefraction;
create table if not exists hotellingactivitydistribution like ##defaultdb##.hotellingactivitydistribution;
create table if not exists hotellinghours like ##defaultdb##.hotellinghours;
create table if not exists hourVMTFraction like ##defaultdb##.hourVMTFraction;
create table if not exists hpmsVTypeYear like ##defaultdb##.hpmsVTypeYear;
create table if not exists IMCoverage like ##defaultdb##.IMCoverage;
create table if not exists importstartsopmodedistribution like ##defaultdb##.importstartsopmodedistribution;
create table if not exists link like ##defaultdb##.link;
create table if not exists linkSourceTypeHour like ##defaultdb##.linkSourceTypeHour;
create table if not exists monthVMTFraction like ##defaultdb##.monthVMTFraction;
create table if not exists offNetworkLink like ##defaultdb##.offNetworkLink;
create table if not exists onroadretrofit like ##defaultdb##.onroadretrofit;
create table if not exists opModeDistribution like ##defaultdb##.opModeDistribution;
create table if not exists regioncounty like ##defaultdb##.regioncounty;
create table if not exists roadType like ##defaultdb##.roadType;
create table if not exists roadTypeDistribution like ##defaultdb##.roadTypeDistribution;
-- create table if not exists sccRoadTypeDistribution like ##defaultdb##.sccRoadTypeDistribution
create table if not exists sourceTypeAgeDistribution like ##defaultdb##.sourceTypeAgeDistribution;
create table if not exists sourceTypeYear like ##defaultdb##.sourceTypeYear;
create table if not exists starts like ##defaultdb##.starts;
create table if not exists startshourfraction like ##defaultdb##.startshourfraction;
create table if not exists startsmonthadjust like ##defaultdb##.startsmonthadjust;
create table if not exists startsperday like ##defaultdb##.startsperday;
create table if not exists startssourcetypefraction like ##defaultdb##.startssourcetypefraction;
create table if not exists state like ##defaultdb##.state;
create table if not exists year like ##defaultdb##.year;
create table if not exists zone like ##defaultdb##.zone;
create table if not exists zoneMonthHour like ##defaultdb##.zoneMonthHour;
create table if not exists zoneRoadType like ##defaultdb##.zoneRoadType;

-- Copy data from the input database.
-- Errors here are ignored by the scripting engine and occur when a table
-- is not present in the input database.
insert ignore into avft select * from ##inputdb##.avft;
insert ignore into avgSpeedDistribution select * from ##inputdb##.avgSpeedDistribution;
insert ignore into county select * from ##inputdb##.county;
insert ignore into countyYear select * from ##inputdb##.countyYear;
insert ignore into dayVMTFraction select * from ##inputdb##.dayVMTFraction;
insert ignore into driveScheduleSecondLink select * from ##inputdb##.driveScheduleSecondLink;
insert ignore into fuelFormulation select * from ##inputdb##.fuelFormulation;
insert ignore into fuelSupply select * from ##inputdb##.fuelSupply;
insert ignore into fuelusagefraction select * from ##inputdb##.fuelusagefraction;
insert ignore into hotellingactivitydistribution select * from ##inputdb##.hotellingactivitydistribution;
insert ignore into hotellinghours select * from ##inputdb##.hotellinghours;
insert ignore into hourVMTFraction select * from ##inputdb##.hourVMTFraction;
insert ignore into hpmsVTypeYear select * from ##inputdb##.hpmsVTypeYear;
insert ignore into IMCoverage select * from ##inputdb##.IMCoverage;
insert ignore into importstartsopmodedistribution select * from ##inputdb##.importstartsopmodedistribution;
insert ignore into link select * from ##inputdb##.link;
insert ignore into linkSourceTypeHour select * from ##inputdb##.linkSourceTypeHour;
insert ignore into monthVMTFraction select * from ##inputdb##.monthVMTFraction;
insert ignore into offNetworkLink select * from ##inputdb##.offNetworkLink;
insert ignore into onroadretrofit select * from ##inputdb##.onroadretrofit;
insert ignore into opModeDistribution select * from ##inputdb##.opModeDistribution;
insert ignore into regioncounty select * from ##inputdb##.regioncounty;
insert ignore into roadType select * from ##inputdb##.roadType;
insert ignore into roadTypeDistribution select * from ##inputdb##.roadTypeDistribution;
-- insert ignore into sccRoadTypeDistribution select * from ##inputdb##.sccRoadTypeDistribution
insert ignore into sourceTypeAgeDistribution select * from ##inputdb##.sourceTypeAgeDistribution;
insert ignore into sourceTypeYear select * from ##inputdb##.sourceTypeYear;
insert ignore into starts select * from ##inputdb##.starts;
insert ignore into startshourfraction select * from ##inputdb##.startshourfraction;
insert ignore into startsmonthadjust select * from ##inputdb##.startsmonthadjust;
insert ignore into startsperday select * from ##inputdb##.startsperday;
insert ignore into startssourcetypefraction select * from ##inputdb##.startssourcetypefraction;
insert ignore into state select * from ##inputdb##.state;
insert ignore into year select * from ##inputdb##.year;
insert ignore into zone select * from ##inputdb##.zone;
insert ignore into zoneMonthHour select * from ##inputdb##.zoneMonthHour;
insert ignore into zoneRoadType select * from ##inputdb##.zoneRoadType;

-- Upgrade offnetworkLink to allow for zoneID
alter table offNetworkLink add column zoneID integer not null default '0';
alter table offNetworkLink drop primary key;
alter table offNetworkLink add primary key (zoneID, sourceTypeID);
alter table offNetworkLink add key (sourceTypeID, zoneID);

-- Use the first zone.
update offNetworkLink, Zone set offnetworkLink.zoneID = zone.zoneID
where zone.zoneID = (select min(zoneID) from Zone);

-- Fuelregionid must correspond to the region in regioncounty.
alter table fuelsupply change countyid fuelRegionID integer default 0;

update fuelsupply f, ##defaultdb##.regioncounty r
set f.fuelregionid=r.regionid
where f.fuelregionid=r.countyid
and r.regionCodeID=1;

-- Remove baseYearOffNetVMT from hpmsvtypeyear
alter table hpmsvtypeyear drop baseYearOffNetVMT;

-- Add missing fields to roadtype
alter table roadtype add (
	isAffectedByOnroad tinyint(1) DEFAULT '1',
	isAffectedByNonroad tinyint(1) DEFAULT '0',
	shouldDisplay tinyint(1) DEFAULT '1'
);

-- Adjust HPMS VMT according to 2007+ HPMS categories
drop table if exists hpmsVTypeSum;
create table hpmsVTypeSum (
	yearID smallint not null default '0' primary key,
	sumOriginal double not null default '0',
	sumAdjusted double not null default '0',
	sumRatio double not null default '0'
);

insert into hpmsVTypeSum(yearID,sumOriginal,sumAdjusted,sumRatio)
select yearID,
	sum(HPMSBaseYearVMT) as sumOriginal,
	sum(HPMSBaseYearVMT *
		case
		when HPMSVTypeID = 10 then 1
		when HPMSVTypeID = 20 then 1
		when HPMSVTypeID = 30 then 1
		when HPMSVTypeID = 40 then 1
		when HPMSVTypeID = 50 then 1
		when HPMSVTypeID = 60 then 1
		else 1
		end
	) as sumAdjusted,
	1.0 as sumRatio
from hpmsVTypeYear
group by yearID;

update hpmsVTypeSum set sumRatio = case when sumAdjusted <= 0 then 1.0 else sumOriginal/sumAdjusted end;

drop table if exists tempHPMSVTypeYear;
create table tempHPMSVTypeYear like hpmsVTypeYear;
insert into tempHPMSVTypeYear select * from hpmsVTypeYear;
truncate table hpmsVTypeYear;

alter table tempHPMSVTypeYear add newHPMSVTypeID smallint not null default '0';
update tempHPMSVTypeYear set newHPMSVTypeID = case when HPMSVTypeID in (20,30) then 25 else HPMSVTypeID end;

insert into hpmsVTypeYear (HPMSVTypeID,yearID,VMTGrowthFactor,HPMSBaseYearVMT)
select newHPMSVTypeID, h.yearID, avg(VMTGrowthFactor) as VMTGrowthFactor,
	sum(HPMSBaseYearVMT*sumRatio*
		case
		when HPMSVTypeID = 10 then 1
		when HPMSVTypeID = 20 then 1
		when HPMSVTypeID = 30 then 1
		when HPMSVTypeID = 40 then 1
		when HPMSVTypeID = 50 then 1
		when HPMSVTypeID = 60 then 1
		else 1
		end
	) as HPMSBaseYearVMT
from tempHPMSVTypeYear h
inner join hpmsVTypeSum s using (yearID)
group by yearID, newHPMSVTypeID
order by null;

drop table if exists hpmsVTypeSum;
drop table if exists tempHPMSVTypeYear;

-- Check for warning or error messages
drop procedure if exists spConvert2010BCDMPDM;

BeginBlock
create procedure spConvert2010BCDMPDM()
begin
	declare howMany int default 0;
	declare isCustomDomain int default 0;
	declare howManyLeapYear int default 0;
	declare howManyNonLeapYear int default 0;

	-- Remove the leap year data from the month VMT fraction table.
	set howManyLeapYear=0;
	select count(*) into howManyLeapYear from monthVMTFraction where isLeapYear='Y';
	set howManyLeapYear=ifnull(howManyLeapYear,0);

	set howManyNonLeapYear=0;
	select count(*) into howManyNonLeapYear from monthVMTFraction where isLeapYear<>'Y';
	set howManyNonLeapYear=ifnull(howManyNonLeapYear,0);

	if(howManyLeapYear > 0 and howManyNonLeapYear > 0) then
		-- Has Both -> Keep Non-Leap Year
		delete from monthvmtfraction where isleapyear='Y';
		insert into convertTempMessages (message) values ('INFO: Keeping only Non-Leap Year monthVMTFraction entries and deleteing Leap Year monthVMTFraction entires. If your MOVES2010b monthVMTFraction table contains entries for a Leap Year, they should be re-imported in MOVES2014.');
	end if;
	if(howManyLeapYear > 0 and howManyNonLeapYear <= 0) then
		-- Has only Leap Year -> Keep Leap Year data
		delete from monthvmtfraction where isleapyear='N';
		insert into convertTempMessages (message) values ('INFO: Using Leap Year monthVMTFraction entries for all years.');
	end if;
	if(howManyLeapYear <= 0 and howManyNonLeapYear > 0) then
		-- Has only non-Leap Year data -> Keep Non-Leap Year data
		delete from monthvmtfraction where isleapyear='Y';
		insert into convertTempMessages (message) values ('INFO: Using Non-Leap Year monthVMTFraction entries for all years.');
	end if;
	alter table monthvmtfraction drop isleapyear;

	-- Handle custom domains/generic counties
	set howMany=0;
	select count(*) into howMany from County where stateID=99;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		set isCustomDomain=1;
	end if;

 	if(isCustomDomain > 0) then
		insert ignore into regionCounty (regionID,countyID,regionCodeID,fuelYearID)
		select 100000000 as regionID, countyID, 1 as regionCodeID, fuelYearID
		from county, year;
 	end if;
end
EndBlock

call spConvert2010BCDMPDM();
drop procedure if exists spConvert2010BCDMPDM;
