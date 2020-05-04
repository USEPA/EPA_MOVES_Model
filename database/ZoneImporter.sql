-- Author Wesley Faler
-- Author Don Smith
-- Version 2014-04-08

drop procedure if exists spCheckZoneImporter;

BeginBlock
create procedure spCheckZoneImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;

	-- Scale 0 is national
	-- Scale 1 is single county
	-- Scale 2 is project domain
	declare scale int default ##scale##;

	-- Rate 0 is Inventory
	-- Rate 1 is Rates
	declare rate int default ##rate##;

	declare desiredAllocFactor double default 1;
	declare howManyZones int default 0;

	-- Build links for imported zones but not for the Project domain (scale=2)
	if(scale <> 2) then
		delete from Link;

		insert ignore into Link (linkID, countyID, zoneID, roadTypeID)
		select (z.zoneID*10 + roadTypeID) as linkID, z.countyID, z.zoneID, roadTypeID
		from ##defaultDatabase##.roadType, Zone z;
	end if;

	-- Complain if alloc factors are not 1.0.
	set howManyZones=0;
	select count(*) into howManyZones from Zone z inner join County c on c.countyID=z.countyID;
	set howManyZones=ifnull(howManyZones,0);
	if(howManyZones > 0) then
		set desiredAllocFactor = 1.0;
	else
		insert into importTempMessages (message)
		select concat('ERROR: No Zones imported for County ',countyID) as errorMessage
		from County;
	end if;

	insert into importTempMessages (message)
	select concat('ERROR: Zone startAllocFactor is not ',round(desiredAllocFactor,4),' but instead ',round(sum(startAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(startAllocFactor),4) <> round(desiredAllocFactor,4);

	insert into importTempMessages (message)
	select concat('ERROR: Zone idleAllocFactor is not ',round(desiredAllocFactor,4),' but instead ',round(sum(idleAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(idleAllocFactor),4) <> round(desiredAllocFactor,4);

	insert into importTempMessages (message)
	select concat('ERROR: Zone SHPAllocFactor is not ',round(desiredAllocFactor,4),' but instead ',round(sum(SHPAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(SHPAllocFactor),4) <> round(desiredAllocFactor,4);

	insert into importTempMessages (message)
	select concat('ERROR: Road type ',roadTypeID,' SHOAllocFactor is not ',round(desiredAllocFactor,4),' but instead ',round(sum(SHOAllocFactor),4)) as errorMessage
	from ZoneRoadType
	group by roadTypeID
	having round(sum(SHOAllocFactor),4) <> round(desiredAllocFactor,4);

	-- Complain if sums exceed 1.0000
	insert into importTempMessages (message)
	select concat('ERROR: Zone startAllocFactor exceeds 1.0, being instead ',round(sum(startAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(startAllocFactor),4) > 1.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Zone idleAllocFactor exceeds 1.0, being instead ',round(sum(idleAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(idleAllocFactor),4) > 1.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Zone SHPAllocFactor exceeds 1.0, being instead ',round(sum(SHPAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(SHPAllocFactor),4) > 1.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Road type ',roadTypeID,' SHOAllocFactor exceeds 1.0, being instead ',round(sum(SHOAllocFactor),4)) as errorMessage
	from ZoneRoadType
	group by roadTypeID
	having round(sum(SHOAllocFactor),4) > 1.0000;

	-- Complain if sums are 0.0 or less
	insert into importTempMessages (message)
	select concat('ERROR: Zone startAllocFactor should not be zero, being instead ',round(sum(startAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(startAllocFactor),4) <= 0.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Zone idleAllocFactor should not be zero, being instead ',round(sum(idleAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(idleAllocFactor),4) <= 0.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Zone SHPAllocFactor should not be zero, being instead ',round(sum(SHPAllocFactor),4)) as errorMessage
	from Zone
	having round(sum(SHPAllocFactor),4) <= 0.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Road type ',roadTypeID,' SHOAllocFactor should not be zero, being instead ',round(sum(SHOAllocFactor),4)) as errorMessage
	from ZoneRoadType
	group by roadTypeID
	having round(sum(SHOAllocFactor),4) <= 0.0000;

	-- Complain about negative allocation factors
	insert into importTempMessages (message)
	select concat('ERROR: Zone ',zoneID,' startAllocFactor is negative, being ',round(startAllocFactor,4)) as errorMessage
	from Zone
	where round(startAllocFactor,4) < 0.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Zone ',zoneID,' idleAllocFactor is negative, being ',round(idleAllocFactor,4)) as errorMessage
	from Zone
	where round(idleAllocFactor,4) < 0.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Zone ',zoneID,' SHPAllocFactor is negative, being ',round(SHPAllocFactor,4)) as errorMessage
	from Zone
	where round(SHPAllocFactor,4) < 0.0000;

	insert into importTempMessages (message)
	select concat('ERROR: Zone ',zoneID,' Road type ',roadTypeID,' SHOAllocFactor is negative, being ',round(SHOAllocFactor,4)) as errorMessage
	from ZoneRoadType
	where round(SHOAllocFactor,4) < 0.0000;

	-- ZoneRoadType table should not be empty
	insert into importTempMessages (message)
	select concat('ERROR: ZoneRoadType references ',zrtZoneCount,' zones but should reference ',zoneCount,' instead') as errorMessage
	from (
	select (select count(*) from zone) as zoneCount,
		(select count(distinct zoneID)
		from zoneRoadType
		inner join zone using (zoneID)) as zrtZoneCount
	) T
	where zoneCount <> zrtZoneCount;

	if(isOk=1) then
		set howMany=0;
		select count(*) into howMany from importTempMessages where message like 'ERROR: %';
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
			set isOk=0;
		end if;
	end if;

	-- Insert 'NOT_READY' or 'OK' to indicate iconic success
	if(mode = 1) then
		insert into importTempMessages (message) values (case when isOk=1 then 'OK' else 'NOT_READY' end);
	end if;

end
EndBlock

call spCheckZoneImporter();
drop procedure if exists spCheckZoneImporter;
