-- Author Wesley Faler
-- Version 2013-11-12

drop procedure if exists spCheckOpModeDistributionImporter;

BeginBlock
create procedure spCheckOpModeDistributionImporter()
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
	
	-- opModeDistribution
	set howMany=0;
	select count(*) into howMany from opModeDistribution;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select concat('ERROR: Source type ',sourceTypeID,', hourDayID ',hourDayID,', link ',linkID,', polProcessID ',polProcessID,' opmodeFraction is not 1.0 but instead ',round(sum(opModeFraction),4))
		from opModeDistribution
		join ##defaultDatabase##.operatingmode using (opModeID)
		group by sourceTypeID, hourDayID, linkID, polProcessID
		having round(sum(opModeFraction),4) <> 1.0000;
	end if;
end
EndBlock

call spCheckOpModeDistributionImporter();
drop procedure if exists spCheckOpModeDistributionImporter;
