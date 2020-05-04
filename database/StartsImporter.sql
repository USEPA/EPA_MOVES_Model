-- Author Wesley Faler
-- Version 2013-11-25

drop procedure if exists spCheckStartsImporter;

BeginBlock
create procedure spCheckStartsImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure, allowing data from the default database
	-- Mode 2 is run to check overall success/failure, requiring no data from the default database
	declare mode int default ##mode##;

	-- Scale 0 is national
	-- Scale 1 is single county
	-- Scale 2 is project domain
	declare scale int default ##scale##;

	declare howMany int default 0;
	
	-- startsHourFraction
	set howMany=0;
	select count(*) into howMany from startsHourFraction;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select concat('ERROR: Zone ',zoneID,', day ',dayID,' allocation fraction is not 1.0 but instead ',round(sum(allocationFraction),4))
		from startsHourFraction
		group by zoneID, dayID
		having round(sum(allocationFraction),4) <> 1.0000;
	end if;

	-- startsSourceTypeFraction
	set howMany=0;
	select count(*) into howMany from startsSourceTypeFraction;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select concat('ERROR: startsSourceTypeFraction total allocation fraction is not 1.0 but instead ',round(sum(allocationFraction),4))
		from startsSourceTypeFraction
		having round(sum(allocationFraction),4) <> 1.0000;
	end if;

	-- startsPerDay
	-- Nothing to check that is not already checked by the importer core logic

	-- startsMonthAdjust
	-- Nothing to check that is not already checked by the importer core logic

	-- importStartsOpModeDistribution
	set howMany=0;
	select count(*) into howMany from importStartsOpModeDistribution;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select concat('ERROR: Source type ',sourceTypeID,', hourDayID ',hourDayID,', link ',linkID,', polProcessID ',polProcessID,' opmodeFraction is not 1.0 but instead ',round(sum(opModeFraction),4))
		from importStartsOpModeDistribution
		group by sourceTypeID, hourDayID, linkID, polProcessID
		having round(sum(opModeFraction),4) <> 1.0000;
	end if;

	-- Complain about non-start pol/process entries in importStartsOpModeDistribution
	insert into importTempMessages (message)
	select distinct concat('ERROR: polProcessID ',polProcessID,' is not a Start Exhaust entry')
	from importStartsOpModeDistribution
	where mod(polProcessID,10)<>2;
end
EndBlock

call spCheckStartsImporter();
drop procedure if exists spCheckStartsImporter;
