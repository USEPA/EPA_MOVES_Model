-- Adjust the values within the TotalIdleFraction table using
-- user-supplied adjustments by model year range, month, sourcetype, and day.
-- Author Wesley Faler
-- Version 2017-09-29

drop procedure if exists spAdjustTotalIdleFraction;

BeginBlock
create procedure spAdjustTotalIdleFraction()
begin
	declare howManyIMYG int default 0;
	
	-- create table totalidlefractioninitial select * from totalidlefraction;

	set howManyIMYG=0;
	select count(*) into howManyIMYG from idleModelYearGrouping;
	set howManyIMYG=ifnull(howManyIMYG,0);

	if(howManyIMYG > 0) then
		-- this means the user supplied shaping tables and we can eliminate the default data
		truncate totalidlefraction;
		
		-- Populate totalIdleFraction from idleModelYearGrouping
		insert into totalIdleFraction(idleRegionID,countyTypeID,
			sourceTypeID,
			monthID,dayID,
			minModelYearID, maxModelYearID,
			totalIdleFraction)
		select distinct st.idleRegionID,c.countyTypeID,
			imyg.sourceTypeID,
			m.monthID, d.dayID,
			imyg.minModelYearID, imyg.maxModelYearID,
			imyg.totalIdleFraction
		from idleModelYearGrouping imyg,
		county c, state st,
		runspecmonth m, runspecday d;
	end if;
	
	-- apply idleMonthAdjust
	update totalIdleFraction
	inner join idleMonthAdjust using (sourceTypeID, monthID)
	set totalIdleFraction = totalIdleFraction * idleMonthAdjust;

	-- apply idleDayAdjust
	update totalIdleFraction
	inner join idleDayAdjust using (sourceTypeID, dayID)
	set totalIdleFraction = totalIdleFraction * idleDayAdjust;
end
EndBlock

call spAdjustTotalIdleFraction();
drop procedure if exists spAdjustTotalIdleFraction;
