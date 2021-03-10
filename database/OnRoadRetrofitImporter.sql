-- Author Wesley Faler
-- Version 2012-10-01

drop procedure if exists spCheckOnRoadRetrofitImporter;

BeginBlock
create procedure spCheckOnRoadRetrofitImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure for national domain
	-- Mode 2 is run to check overall success/failure for county and project domains
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;

	-- The beginModelYearID must be earlier or the same as the retrofitYearID.  If not, an ERROR message shall appear and the tab header shall show a red X.
	insert into importTempMessages (message)
	select distinct concat('ERROR: beginModelYearID ',beginModelYearID,' must be the same or before the calendar year ',retrofitYearID) as errorMessage
	from onRoadRetrofit
	where beginModelYearID > retrofitYearID;

	-- The endModelYearID must be earlier or the same as the retrofitYearID.  If not, an ERROR message shall appear and the tab header shall show a red X.
	insert into importTempMessages (message)
	select distinct concat('ERROR: endModelYearID ',endModelYearID,' must be the same or before the calendar year ',retrofitYearID) as errorMessage
	from onRoadRetrofit
	where endModelYearID > retrofitYearID;

	-- The endModelYearID must be the same or after the beginModelYearID.  If not, an ERROR message shall appear and the tab header shall show a red X.
	insert into importTempMessages (message)
	select distinct concat('ERROR: endModelYearID ',endModelYearID,' must be the same or after beginModelYearID ',beginModelYearID) as errorMessage
	from onRoadRetrofit
	where endModelYearID < beginModelYearID;

	-- The new input is cumFractionRetrofit and is the total retrofit coverage at a given calendar year. It cannot be greater than 1.0 or less than 0.0.  If it is, an ERROR message shall appear and the tab header shall show a red X.
	insert into importTempMessages (message)
	select distinct concat('ERROR: cumFractionRetrofit (',round(cumFractionRetrofit,4),') must be less than or equal to 1.0000') as errorMessage
	from onRoadRetrofit
	where round(cumFractionRetrofit,4) > 1.0000;

	insert into importTempMessages (message)
	select distinct concat('ERROR: cumFractionRetrofit (',round(cumFractionRetrofit,4),') must be greater than or equal to 0.0000') as errorMessage
	from onRoadRetrofit
	where round(cumFractionRetrofit,4) < 0.0000;

	-- The retrofitEffectiveFraction must not be greater than 1.  If it is, an ERROR message shall appear and the tab header shall show a red X.
	insert into importTempMessages (message)
	select distinct concat('ERROR: retrofitEffectiveFraction (',round(retrofitEffectiveFraction,4),') must be less than or equal to 1.0000') as errorMessage
	from onRoadRetrofit
	where round(retrofitEffectiveFraction,4) > 1.0000;

	-- The retrofitEffectiveFraction can be less than 0 (i.e. negative).  If it is, a NOTE message shall appear stating that the retrofit program will result in increased emissions.
	insert into importTempMessages (message)
	select distinct concat('NOTE: retrofitEffectiveFraction (',round(retrofitEffectiveFraction,4),') will increase emissions') as errorMessage
	from onRoadRetrofit
	where round(retrofitEffectiveFraction,4) < 0.0000;

	if(isOk=1) then
		set howMany=0;
		select count(*) into howMany from importTempMessages where message like 'ERROR:%';
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
			set isOk=0;
		end if;
	end if;

	-- Insert 'NOT_READY' or 'OK' to indicate iconic success
	if(mode >= 1) then
		insert into importTempMessages (message) values (case when isOk=1 then 'OK' else 'NOT_READY' end);
	end if;
end
EndBlock

call spCheckOnRoadRetrofitImporter();
drop procedure if exists spCheckOnRoadRetrofitImporter;
