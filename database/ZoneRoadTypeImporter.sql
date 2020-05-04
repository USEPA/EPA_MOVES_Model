-- Author Wesley Faler
-- Version 2009-12-05

drop procedure if exists spCheckZoneRoadTypeImporter;

BeginBlock
create procedure spCheckZoneRoadTypeImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;

	insert into importTempMessages (message)
	select concat('ERROR: Road type ',roadTypeID,' SHOAllocFactor is not 1.0 but instead ',round(sum(SHOAllocFactor),4)) as errorMessage
	from ZoneRoadType
	group by roadTypeID
	having round(sum(SHOAllocFactor),4) <> 1.0000;

	if(isOk=1) then
		set howMany=0;
		select count(*) into howMany from importTempMessages where message like 'ERROR: Road type%';
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

call spCheckZoneRoadTypeImporter();
drop procedure if exists spCheckZoneRoadTypeImporter;
