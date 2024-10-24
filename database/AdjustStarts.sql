-- Adjust the distribution within the Starts table using
-- user-supplied adjustments by month, hour, day, sourcetype, and age.

-- Author Wesley Faler
-- Author John Covey
-- Version 2018-07-09

-- @algorithm starts = population * startsPerDayPerVehicle * combinedAgeEffectFraction * monthAdjustment * allocationFraction
-- @owner Starts

drop procedure if exists spAdjustStarts;

BeginBlock
create procedure spAdjustStarts(IN dayID int)
begin
	declare targetYearID int default ##yearID##;
	declare targetZoneID int default ##zoneID##;
	
	declare startsCnt int default 0;
	declare startsPerDayCnt int default 0;
	declare startsPerDayPerVehicleCnt int default 0;
	select count(*) into startsCnt from starts where isUserInput = 'Y' and RIGHT(starts.hourDayID, 1) = dayID;
	select count(*) into startsPerDayCnt from startsPerDay where startsPerDay.dayID = dayID;
	select count(*) into startsPerDayPerVehicleCnt from startsPerDayPerVehicle where startsPerDayPerVehicle.dayID = dayID;
	
	if (startsCnt > 0 and startsPerDayPerVehicleCnt > 0) then
		-- startsPerDayPerVehicleCnt is always >0 because it contains default data (which could be overwritten by user;
		-- 		in either case, it has more than 0 rows).
		-- starts will have more than 0 rows only if it was directly imported by the user. If this is the case, do not
		--      calculate the starts table (this is done in the else clause).
		-- Note: used to emit a warning message when this case was triggered. However, this case will always be triggered
		--      when importing the Starts table, and it is impossible to tell if a user imported both the starts table and
		--      the startsPerDayPerVehicle table, or just the starts table, so this message could be misleading.
		-- insert into tempMessages (message)
		--		select 'WARNING : Detected that user has imported Starts table. These values will be used instead of StartsPerDayPerVehicle or StartsPerDay.' from dual;					
		
		-- EM - in order to avoid errors, we need to do something in this if-then. For now, I think we want to keep it around in case we do want to 
		-- log this warning at some point, so I just put in a select 1; to make SQL feel happy about doing something.
		select 1;
		
	else 
		-- tempStartsPerDay will hold startsPerDay data, which could either be directly imported by the user, or calculated from
		-- startsPerDayPerVehicle (which has either user imported data or default data)
		drop table if exists tempStartsPerDay;
		CREATE TABLE tempStartsPerDay (
		  dayID smallint(6) NOT NULL DEFAULT 0,
		  sourceTypeID smallint(6) NOT NULL DEFAULT 0,
		  startsPerDay double DEFAULT NULL,
		  PRIMARY KEY (sourceTypeID,dayID),
		  KEY hourDayID (dayID),
		  KEY sourceTypeID (sourceTypeID)
		) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
		
		if (startsPerDayCnt > 0) then
			-- This data was directly imported by the user; just need to transfer from user input table to temp table
			-- Note: used to emit a warning message when this happened. However, this case is no longer possible to
			--       trigger accidentally as importer GUI forbids it.
			-- insert into tempMessages (message)
			--   select 'WARNING : Detected that user has imported StartsPerDay. These values will be used instead of StartsPerDayPerVehicle.' from dual;					
			insert into tempStartsPerDay (dayID, sourceTypeID, startsPerDay) 
				select dayID, sourceTypeID, startsPerDay from startsPerDay
                where startsPerDay.dayID = dayID;			
		else
			-- Need to calculate startsPerDay from startsPerDayPerVehicle and vehicle populations
			insert into tempStartsPerDay (dayID, sourceTypeID, startsPerDay)
				  select distinct spdpv.dayID as dayID,
					spdpv.sourceTypeID as sourceTypeID,
					sty.sourceTypePopulation * spdpv.startsPerDayPerVehicle as startsPerDay
				from StartsPerDayPerVehicle spdpv
				inner join sourceTypeYear sty on
					sty.sourceTypeID = spdpv.sourceTypeID
				where sty.yearID = targetYearID
                  and spdpv.dayID = dayID;
		
		end if;
		
		-- Calculate the combined age effects (age distribution and age adjustments)
		-- First, normalize the age adjustment (so it becomes a distributive effect instead of a multiplicative adjustment)
		drop table if exists tempStartsNormalizedAgeAdjust;
		CREATE TABLE tempStartsNormalizedAgeAdjust (
		  sourceTypeID smallint(6) NOT NULL DEFAULT 0,
		  ageID smallint(6) NOT NULL DEFAULT 0,
		  normalizedAgeAdjustment double DEFAULT NULL,
		  PRIMARY KEY (sourceTypeID,ageID),
		  KEY sourceTypeID (sourceTypeID),
		  KEY ageID (ageID)
		) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
		insert into tempStartsNormalizedAgeAdjust (sourceTypeID, ageID, normalizedAgeAdjustment)
			select sourceTypeID, ageID, ageAdjustment/TotalAgeAdjustment as normalizedAgeAdjustment
			from startsageadjustment saa
			join (select sourceTypeID, sum(ageAdjustment) as totalAgeAdjustment 
				  from startsageadjustment group by sourceTypeID) as taa using (sourceTypeID);
			
		-- Then, combine the age distribution with the normalized age adjustment and renormalize so that the new parameter
		-- distributes the calculated starts across ages. The window function calculates the SUMPRODUCT of the age distribution and the normalized
		-- age adjustment factor (element-wise multiplication and summing across all ages for each source type).
		drop table if exists tempStartsCombinedAgeEffect;
		CREATE TABLE tempStartsCombinedAgeEffect (
		  sourceTypeID smallint(6) NOT NULL DEFAULT 0,
		  ageID smallint(6) NOT NULL DEFAULT 0,
		  combinedAgeEffectFraction double DEFAULT NULL,
		  PRIMARY KEY (sourceTypeID,ageID),
		  KEY sourceTypeID (sourceTypeID),
		  KEY ageID (ageID)
		) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
		insert into tempStartsCombinedAgeEffect (sourceTypeID, ageID, combinedAgeEffectFraction)
			select stad.sourceTypeID, stad.ageID, ageFraction * normalizedAgeAdjustment / sumproduct
			from sourceTypeAgeDistribution stad
			join tempStartsNormalizedAgeAdjust as tsnaa on (stad.sourceTypeID = tsnaa.sourceTypeID and stad.ageID = tsnaa.ageID and stad.yearID = targetYearID)
			join (select stad.sourceTypeID, sum(ageFraction * normalizedAgeAdjustment) as sumproduct
				  from sourceTypeAgeDistribution stad
				  join tempStartsNormalizedAgeAdjust as tsnaa on (stad.sourceTypeID = tsnaa.sourceTypeID and stad.ageID = tsnaa.ageID and stad.yearID = targetYearID)
				  group by stad.sourceTypeID
				  ) as sumproduct on (stad.sourceTypeID = sumproduct.sourceTypeID);
			
		
		-- starts = startsPerDay * combinedAgeEffectFraction * monthAdjustment * hourAllocationFraction * startAllocFactor
		insert into starts (
			hourDayID, monthID, yearID, ageID,
			zoneID, sourceTypeID, starts, startsCV, isUserInput
			)
		select distinct
			(shf.hourID*10+shf.dayID) as hourDayID,
			sma.monthID as monthID,
			targetYearID as yearID,
			tscae.ageID as ageID,
			targetZoneID as zoneID,
			tspd.sourceTypeID as sourceTypeID,
		   (tspd.startsPerDay * tscae.combinedAgeEffectFraction * sma.monthAdjustment * shf.allocationFraction
			   * ifnull(z.startAllocFactor, 1)) as starts,
		    0 as startsCV,
		   'N' as isUserInput
		from tempStartsPerDay tspd
		inner join StartsMonthAdjust sma on
			sma.sourceTypeID = tspd.sourceTypeID  
		inner join StartsHourFraction shf on
			shf.sourceTypeID = tspd.sourceTypeID
			and shf.dayID = tspd.dayID and tspd.dayID = dayID
		inner join tempStartsCombinedAgeEffect tscae on
			tscae.sourceTypeID = tspd.sourceTypeID
		left join zone z on
			z.zoneID = targetZoneID;
	end if;
	
	-- starts algorithm assumes that the starts table contains starts as a portion of the week
	-- (i.e., starts for weekdays need to be multiplied by 5)
	update starts s
		inner join HourDay hd on s.hourDayID = hd.hourDayID
		inner join DayOfAnyWeek doa on hd.dayID = doa.dayID
		set s.starts = s.starts * doa.noOfRealDays
        WHERE doa.dayID = dayID;
	
	drop table if exists tempStartsPerDay;
	drop table if exists tempStartsNormalizedAgeAdjust;
	drop table if exists tempStartsCombinedAgeEffect;
end
EndBlock

call spAdjustStarts(0);
call spAdjustStarts(2);
call spAdjustStarts(5);
drop procedure if exists spAdjustStarts;
