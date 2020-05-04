-- Populate RatePerDistance, RatePerVehicle, RatePerProfile.
-- Run from an output database.
--
-- Expected replacement parameters:
--		##mainDatabase##	Ideally will be MOVESExecution, but could be a fully filled default database
--		##runID##
--		##scenarioID##
--		##isProjectDomain##	1 or 0
--		##isSCC## 1 or 0
--
-- Author Wesley Faler
-- Version 2014-02-13

drop procedure if exists spRatePostProcessor;

BeginBlock
create procedure spRatePostProcessor()
begin
	-- There will be no NULL columns in MOVESOutput or MOVESActivityOutput.  All NULL values
	-- have been replaced with 0's to facilitate joining.

	-- Create a temporary table, good for exactly one type of activity and tuned to the needs
	-- of joins for the rate tables.

	-- Populate RatePerDistance
	--	First, get just the distance activity into its own indexed table
	drop table if exists TempActivityOutput;
	create table TempActivityOutput (
		yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
		monthID              SMALLINT UNSIGNED NULL DEFAULT NULL,
		dayID                SMALLINT UNSIGNED NULL DEFAULT NULL,
		hourID               SMALLINT UNSIGNED NULL DEFAULT NULL,
		linkID               INTEGER UNSIGNED NULL DEFAULT NULL,
		sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
		regClassID           SMALLINT UNSIGNED NULL DEFAULT NULL,
		SCC					 CHAR(10) NULL DEFAULT '',
		fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
		modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
		roadTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
		activity             FLOAT NULL DEFAULT NULL,
		zoneID               INTEGER UNSIGNED NULL DEFAULT NULL,
		key (yearID, monthID, dayID, hourID,
			linkID,
			sourceTypeID, regClassID, fuelTypeID, modelYearID,
			roadTypeID)
	) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

	insert into TempActivityOutput (
		yearID, monthID, dayID, hourID,
		linkID,
		sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
		roadTypeID,
		activity,
		zoneID)
	select yearID, monthID, dayID, hourID,
		linkID,
		sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
		roadTypeID,
		activity,
		zoneID
	from MOVESActivityOutput
	where MOVESRunID = ##runID##
	and activityTypeID=1;
	
	analyze table TempActivityOutput;

	--	Now the inventory can be efficiently joined with the indexed activity
	insert into RatePerDistance (MOVESScenarioID, MOVESRunID,
		yearID, monthID, dayID, hourID,
		linkID,
		sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
		roadTypeID,
		pollutantID, processID,
		avgSpeedBinID,temperature, relHumidity,
		ratePerDistance)
	select '##scenarioID##' as MOVESScenarioID, ##runID## as MOVESRunID,
		o.yearID, o.monthID, o.dayID, o.hourID,
		o.linkID,
		o.sourceTypeID, o.regClassID, o.SCC, o.fuelTypeID, o.modelYearID,
		o.roadTypeID,
		o.pollutantID, o.processID,
		if(##isProjectDomain##>0,0,mod(o.linkID,100)) as avgSpeedBinID,
		z.temperature, z.relHumidity,
		case when a.activity > 0 then (o.emissionQuant / a.activity)
		else null
		end as ratePerDistance
	from MOVESOutput as o
	inner join TempActivityOutput as a using (
		yearID, monthID, dayID, hourID,
		linkID,
		sourceTypeID, regClassID, fuelTypeID, modelYearID,
		roadTypeID)
	inner join ##mainDatabase##.ZoneMonthHour as z on (
		z.zoneID=a.zoneID
		and z.monthID=a.monthID
		and z.hourID=a.hourID
	)
	where o.MOVESRunID=##runID##
	and (o.processID in (1,9,10,15,18,19)
	or (o.processID in (11,12,13) and o.roadTypeID<>1));

	-- Populate RatePerVehicle
	--	First, get just the population into its own indexed table
	drop table if exists TempActivityOutput;
	create table TempActivityOutput (
		zoneID               INTEGER UNSIGNED NULL DEFAULT NULL,
		yearID               SMALLINT UNSIGNED NULL DEFAULT NULL,
		sourceTypeID         SMALLINT UNSIGNED NULL DEFAULT NULL,
		regClassID	         SMALLINT UNSIGNED NULL DEFAULT NULL,
		SCC					 CHAR(10) NULL DEFAULT NULL,
		fuelTypeID           SMALLINT UNSIGNED NULL DEFAULT NULL,
		modelYearID          SMALLINT UNSIGNED NULL DEFAULT NULL,
		activity             FLOAT NULL DEFAULT NULL,
		key (zoneID,
			yearID, 
			sourceTypeID, regClassID, fuelTypeID, modelYearID)
	) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

	if(##isProjectDomain##>0) then
		-- Project population is by link and needs to be aggregated to a single zone-level population.
		insert into TempActivityOutput (
			zoneID,
			yearID,
			sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
			activity)
		select zoneID,
			yearID,
			sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
			sum(activity)
		from MOVESActivityOutput
		where MOVESRunID = ##runID##
		and activityTypeID=6
		and roadTypeID=1
		group by zoneID,
			yearID,
			sourceTypeID, regClassID, fuelTypeID, modelYearID;
	else
		-- Non-Project domains generate a single population entry per zone/month/day/hour and don't need to be aggregated.
		insert into TempActivityOutput (
			zoneID,
			yearID,
			sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
			activity)
		select distinct zoneID,
			yearID,
			sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
			activity
		from MOVESActivityOutput
		where MOVESRunID = ##runID##
		and activityTypeID=6;
	end if;

	--	Now the inventory can be efficiently joined with the indexed activity
	insert into RatePerVehicle (MOVESScenarioID, MOVESRunID,
		yearID, monthID, dayID, hourID,
		zoneID,
		sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
		pollutantID, processID,
		temperature,
		ratePerVehicle)
	select '##scenarioID##' as MOVESScenarioID, ##runID## as MOVESRunID,
		o.yearID, o.monthID, o.dayID, o.hourID,
		o.zoneID,
		o.sourceTypeID, o.regClassID, o.SCC, o.fuelTypeID, o.modelYearID,
		o.pollutantID, o.processID,
		z.temperature,
		case when a.activity > 0 then (o.emissionQuant / a.activity)
		else null
		end as ratePerVehicle
	from MOVESOutput as o
	inner join TempActivityOutput as a using (
		zoneID,
		yearID,
		sourceTypeID, regClassID, fuelTypeID, modelYearID)
	inner join ##mainDatabase##.ZoneMonthHour as z on (
		z.zoneID=a.zoneID
		and z.monthID=o.monthID
		and z.hourID=o.hourID
	)
	where o.MOVESRunID=##runID##
	and (o.processID in (2,16,17,90,91)
	or (o.processID in (11,13,18,19) and o.roadTypeID=1));

	--	Now the inventory can be efficiently joined with the indexed activity
	insert into StartsPerVehicle (MOVESScenarioID, MOVESRunID,
		yearID, monthID, dayID, hourID,
		zoneID,
		sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
		startsPerVehicle)
	select '##scenarioID##' as MOVESScenarioID, ##runID## as MOVESRunID,
		o.yearID, o.monthID, o.dayID, o.hourID,
		o.zoneID,
		o.sourceTypeID, o.regClassID, o.SCC, o.fuelTypeID, o.modelYearID,
		case when a.activity > 0 then (o.activity / a.activity)
		else null
		end as startsPerVehicle
	from MOVESActivityOutput as o
	inner join TempActivityOutput as a using (
		zoneID,
		yearID,
		sourceTypeID, regClassID, fuelTypeID, modelYearID)
	where o.MOVESRunID=##runID##
	and o.activityTypeID = 7;

-- Cannot make Project starts as "rate per started vehicle" because the sourceTypeID/SCC connection is many-to-many
-- and cannot be done for the SCC portion of rates.  In addition, this would make Project report rates differently
-- than other scales, which report starts as "rate per existing vehicle".
-- 	if(##isProjectDomain##>0) then
-- 		update RatePerVehicle, ##mainDatabase##.offNetworkLink
-- 		set ratePerVehicle = case when startFraction > 0 then (ratePerVehicle / startFraction) else 0 end
-- 		where MOVESRunID=##runID##
-- 		and processID = 2
-- 		and RatePerVehicle.sourceTypeID = ##mainDatabase##.offNetworkLink.sourceTypeID;
-- 	end if;

	-- Populate RatePerProfile
	-- Use the population activity from the prior step, already stored in TempActivityOutput
	--	Now the inventory can be efficiently joined with the indexed activity
	insert into RatePerProfile (MOVESScenarioID, MOVESRunID,
		temperatureProfileID,
		yearID, dayID, hourID,
		pollutantID, processID,
		sourceTypeID, regClassID, SCC, fuelTypeID, modelYearID,
		temperature,
		ratePerVehicle)
	select '##scenarioID##' as MOVESScenarioID, ##runID## as MOVESRunID,
		t.temperatureProfileID,
		o.yearID, o.dayID, o.hourID,
		o.pollutantID, o.processID,
		o.sourceTypeID, o.regClassID, o.SCC, o.fuelTypeID, o.modelYearID,
		z.temperature,
		case when a.activity > 0 then (o.emissionQuant / a.activity)
		else null
		end as ratePerVehicle
	from MOVESOutput as o
	inner join TempActivityOutput as a using (
		zoneID,
		yearID,
		sourceTypeID, regClassID, fuelTypeID, modelYearID)
	inner join ##mainDatabase##.ZoneMonthHour as z on (
		z.zoneID=a.zoneID
		and z.monthID=o.monthID
		and z.hourID=o.hourID
	)
	inner join ##mainDatabase##.TemperatureProfileID as t on (
		t.zoneID=a.zoneID
		and t.monthID=o.monthID
	)
	where o.MOVESRunID=##runID##
	and o.processID=12 and o.roadTypeID=1;

	-- Done
	drop table if exists TempActivityOutput;
end
EndBlock

call spRatePostProcessor();
drop procedure if exists spRatePostProcessor;
