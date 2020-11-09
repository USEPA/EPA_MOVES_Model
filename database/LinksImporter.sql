-- Version 2012-02-18
-- Author Wesley Faler

drop procedure if exists spCheckLinkImporter;

BeginBlock
create procedure spCheckLinkImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;

	-- Check for missing road types
	insert into importTempMessages (message)
	select concat('ERROR: Link ',linkID,' is missing its roadTypeID') as errorMessage
	from link
	where (roadTypeID is null or roadTypeID <= 0)
	order by linkID;

	if(isOk=1) then
		set howMany=0;
		select count(*) into howMany from importTempMessages where message like 'ERROR: Link % is missing its roadTypeID';
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
			set isOk=0;
		end if;
	end if;

	-- Check for negative average speeds
	insert into importTempMessages (message)
	select concat('ERROR: Link ',linkID,' average speed (',linkAvgSpeed,') cannot be negative') as errorMessage
	from link
	where (linkAvgSpeed < 0)
	order by linkID;

	if(isOk=1) then
		set howMany=0;
		select count(*) into howMany from importTempMessages where message like 'ERROR: Link % average speed (%) cannot be negative';
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
			set isOk=0;
		end if;
	end if;

	-- Remind users that drive schedules will override any link average speed or grade
	insert into importTempMessages (message)
	select concat('INFO: Link ',linkID,' will obtain average speed and grade from its driving schedule') as errorMessage
	from link
	where linkID in (select distinct linkID from driveScheduleSecondLink)
	order by linkID;

	-- Note missing data
	insert into importTempMessages (message)
	select concat('ERROR: Link ',linkID,' is missing average speed, operating modes, and/or a drive schedule') as errorMessage
	from link
	where linkAvgSpeed is null
	and linkID not in (select distinct linkID from opModeDistribution)
	and linkID not in (select distinct linkID from driveScheduleSecondLink)
	order by linkID;

	insert into importTempMessages (message)
	select concat('ERROR: Link ',linkID,' is missing average speed but has operating mode data') as errorMessage
	from link
	where linkAvgSpeed is null
	and linkID in (select distinct linkID from opModeDistribution)
	and linkID not in (select distinct linkID from driveScheduleSecondLink)
	order by linkID;

	insert into importTempMessages (message)
	select concat('ERROR: Link ',linkID,' is missing average grade and cannot interpolate a drive schedule') as errorMessage
	from link
	where linkAvgSpeed is not null and linkAvgGrade is null
	and linkID not in (select distinct linkID from opModeDistribution)
	and linkID not in (select distinct linkID from driveScheduleSecondLink)
	order by linkID;

	insert into importTempMessages (message)
	select distinct concat('ERROR: Zone ',zoneID,' is missing Off-Network data') as errorMessage
	from link
	where roadTypeID = 1
	and zoneID not in (select distinct zoneID from offNetworkLink)
	order by zoneID;

	insert into importTempMessages (message)
	select concat('ERROR: Zone ',zoneID,' has more than 1 off-network link') as message
	from link
	where roadTypeID = 1
	group by zoneID
	having count(*) > 1;

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

call spCheckLinkImporter();
drop procedure if exists spCheckLinkImporter;
