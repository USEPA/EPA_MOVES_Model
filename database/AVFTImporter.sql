-- Author Wesley Faler
-- Author Don Smith
-- Version 2011-08-17

drop procedure if exists spCheckAVFTImporter;

BeginBlock
create procedure spCheckAVFTImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;

	insert into importTempMessages (message)
	select concat('ERROR: source type ',sourceTypeID,', model year ',modelYearID,', fuel engine fraction is more than 1.0, being ',round(sum(fuelEngFraction),4))
	from avft 
	group by sourceTypeID, modelYearID
	having round(sum(fuelEngFraction),4) > 1.0000;

	insert into importTempMessages (message)
	select concat('ERROR: source type ',sourceTypeID,', model year ',modelYearID,', fuel ',fuelTypeID,', engine ',engTechID,', fuel engine fraction is less than 0.0, being ',round(fuelEngFraction,4))
	from avft
	where round(fuelEngFraction,4) < 0.0000;

	insert into importTempMessages (message)
	select concat('WARNING: source type ',sourceTypeID,', model year ',modelYearID,', fuel engine fraction is not 1.0 but instead ',round(sum(fuelEngFraction),4))
	from avft 
	group by sourceTypeID, modelYearID
	having round(sum(fuelEngFraction),4) < 1.0000 and round(sum(fuelEngFraction),4) > 0.0000;

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

call spCheckAVFTImporter();
drop procedure if exists spCheckAVFTImporter;
