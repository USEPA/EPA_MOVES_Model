-- FuelSupplyImporter.sql - script to check import errors for the
-- tables: AVFT, fuelFormulation, fuelSupply, and fuelUsageFraction.
-- Author Wesley Faler
-- Version 2016-10-04

drop procedure if exists spCheckFuelSupplyImporter;

BeginBlock
create procedure spCheckFuelSupplyImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure, allowing data from the default database
	-- Mode 2 is run to check overall success/failure, requiring no data from the default database
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;
	declare defaultFuelCount int default 0;
	declare avftFuelCount int default 0;

	-- Scale 0 is national
	-- Scale 1 is single county
	-- Scale 2 is project domain
	-- Scale 3 is Nonroad
	declare scale int default ##scale##;

	-- useFuelUsageFraction 0 when the fuelUsageFraction table is not used in the model
	declare useFuelUsageFraction int default ##USE_FUELUSAGEFRACTION##;

	if(mode = 0) then
		-- Create any new fuel years and associate them to the required years
		drop table if exists tempNewFuelYear;

		if(scale = 3) then
			create table tempNewFuelYear
			select distinct fuelYearID
			from nrFuelSupply fs
			left outer join ##defaultDatabase##.fuelSupplyYear fsy using (fuelYearID)
			where fsy.fuelYearID is null;
		else
			create table tempNewFuelYear
			select distinct fuelYearID
			from fuelSupply fs
			left outer join ##defaultDatabase##.fuelSupplyYear fsy using (fuelYearID)
			where fsy.fuelYearID is null;
		end if;
		
		drop table if exists fuelSupplyYear;
		
		create table if not exists fuelSupplyYear (
		  fuelYearID smallint(6) not null default '0',
		  primary key (fuelYearID)
		);
		
		insert ignore into fuelSupplyYear (fuelYearID)
		select fuelYearID from tempNewFuelYear;
		
		drop table if exists tempYear;
		
		create table if not exists tempYear (
		  yearID smallint(6) not null default '0',
		  isBaseYear char(1) default null,
		  fuelYearID smallint(6) not null default '0',
		  primary key  (yearID),
		  key isBaseYear (isBaseYear)
		);
		
		create table if not exists year (
		  yearID smallint(6) not null default '0',
		  isBaseYear char(1) default null,
		  fuelYearID smallint(6) not null default '0',
		  primary key  (yearID),
		  key isBaseYear (isBaseYear)
		);
		
		insert into tempYear (yearID, isBaseYear, fuelYearID)
		select yearID, isBaseYear, nfy.fuelYearID
		from tempNewFuelYear nfy
		inner join ##defaultDatabase##.year y on (y.yearID=nfy.fuelYearID);
		
-- 		insert ignore into year (yearID, isBaseYear, fuelYearID)
-- 		select yearID, isBaseYear, fuelYearID
-- 		from tempYear
		
		update year, tempYear set year.fuelYearID=tempYear.fuelYearID
		where year.yearID=tempYear.yearID;
		
		drop table if exists tempYear;
		drop table if exists tempNewFuelYear;
	end if;
	
	-- Complain about any years outside of MOVES's range
	if(scale = 3) then
		insert into importTempMessages (message)
		select distinct concat('ERROR: Fuel Year ',fuelYearID,' is outside the range of 1990-2060 and cannot be used') as errorMessage
		from nrFuelSupply
		where fuelYearID < 1990 or fuelYearID > 2060
		and marketShare > 0;
	else
		insert into importTempMessages (message)
		select distinct concat('ERROR: Fuel Year ',fuelYearID,' is outside the range of 1990-2060 and cannot be used') as errorMessage
		from fuelSupply
		where fuelYearID < 1990 or fuelYearID > 2060
		and marketShare > 0;
	end if;
	
	if(mode = 0) then
		if(scale = 3) then
			-- Remove records with zero market shares
			delete from nrFuelSupply where marketShare < 0.0001;
		else
			-- Remove records with zero market shares
			delete from fuelSupply where marketShare < 0.0001;
		end if;
	end if;
	
	-- Complain about unknown fuel formulations
	if(scale = 3) then
		insert into importTempMessages (message)
		select distinct concat('ERROR: Fuel formulation ',fuelFormulationID,' is unknown') as message
		from nrFuelSupply
		where fuelFormulationID not in (
			select fuelFormulationID
			from fuelFormulation
			union
			select fuelFormulationID
			from ##defaultDatabase##.fuelFormulation
		)
		and marketShare > 0;
	else 
		if(mode = 2 or scale in (1,2)) then
			insert into importTempMessages (message)
			select distinct concat('ERROR: Fuel formulation ',fuelFormulationID,' is unknown') as message
			from fuelSupply
			where fuelFormulationID not in (
				select fuelFormulationID
				from fuelFormulation
			)
			and marketShare > 0;
		else
			insert into importTempMessages (message)
			select distinct concat('ERROR: Fuel formulation ',fuelFormulationID,' is unknown') as message
			from fuelSupply
			where fuelFormulationID not in (
				select fuelFormulationID
				from fuelFormulation
				union
				select fuelFormulationID
				from ##defaultDatabase##.fuelFormulation
			)
			and marketShare > 0;
		end if;
	end if;

	if (scale = 3) then
		insert into importTempMessages (message)
		select concat('Warning: Fuel formulation ',fuelFormulationID,' is gasoline with ethanol volume greater than 10%') as message
		from fuelformulation 
		join nrfuelsupply using (fuelFormulationID)
		where ETOHVolume > 10 and fuelSubtypeID in (10,11,12,13,14,15,18) and marketShare > 0;
	else
		insert into importTempMessages (message)
		select concat('ERROR: Fuel formulation ',fuelFormulationID,' is gasoline with ethanol volume greater than 15%') as message
		from fuelformulation 
		join fuelsupply using (fuelFormulationID)
		where ETOHVolume > 15 and fuelSubtypeID in (10,11,12,13,14,15,18) and marketShare > 0;
	end if;

	insert into importTempMessages (message)
	select concat('ERROR: Fuel formulation ',fuelFormulationID,' has non-zero value for MTBE volume') as message
	from fuelformulation where MTBEVolume <> 0;

	insert into importTempMessages (message)
	select concat('ERROR: Fuel formulation ',fuelFormulationID,' has non-zero value for ETBE volume') as message
	from fuelformulation where ETBEVolume <> 0;

	insert into importTempMessages (message)
	select concat('ERROR: Fuel formulation ',fuelFormulationID,' has non-zero value for TAME volume') as message
	from fuelformulation where TAMEVolume <> 0;

	-- Correct fuelFormulation.fuelSubtypeID for gasoline and ethanol fuels
	-- Note: RFG (sub type 11) and conventional gasoline (sub type 10) cannot be distinguished by ETOHVolume, so anything with
	-- ----- a low ETOHVolume and not already assigned as RFG is assigned to conventional gasoline.

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' changed fuelSubtypeID from ',fuelSubtypeID, ' to 10 based on ETOHVolume') as message
	from fuelFormulation where fuelSubtypeID <> 10 and ETOHVolume < 0.10  and fuelSubtypeID <> 11 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' changed fuelSubtypeID from ',fuelSubtypeID, ' to 12 based on ETOHVolume') as message
	from fuelFormulation where fuelSubtypeID <> 12 and ETOHVolume >= 9    and ETOHVolume < 12.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' changed fuelSubtypeID from ',fuelSubtypeID, ' to 13 based on ETOHVolume') as message
	from fuelFormulation where fuelSubtypeID <> 13 and ETOHVolume >= 6    and ETOHVolume < 9 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' changed fuelSubtypeID from ',fuelSubtypeID, ' to 14 based on ETOHVolume') as message
	from fuelFormulation where fuelSubtypeID <> 14 and ETOHVolume >= 0.10 and ETOHVolume < 6 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' changed fuelSubtypeID from ',fuelSubtypeID, ' to 15 based on ETOHVolume') as message
	from fuelFormulation where fuelSubtypeID <> 15 and ETOHVolume >= 12.5 and ETOHVolume < 17.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' changed fuelSubtypeID from ',fuelSubtypeID, ' to 51 based on ETOHVolume') as message
	from fuelFormulation where fuelSubtypeID <> 51 and ETOHVolume >= 70.5 and ETOHVolume <= 100 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' changed fuelSubtypeID from ',fuelSubtypeID, ' to 52 based on ETOHVolume') as message
	from fuelFormulation where fuelSubtypeID <> 52 and ETOHVolume >= 50.5   and ETOHVolume < 70.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' changed fuelSubtypeID from ',fuelSubtypeID, ' to 18 based on ETOHVolume') as message
	from fuelFormulation where fuelSubtypeID <> 18 and ETOHVolume >= 17.5 and ETOHVolume < 50.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	update fuelFormulation set fuelSubtypeID = 10 where fuelSubtypeID <> 10 and ETOHVolume < 0.10  and fuelSubtypeID <> 11 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);
	update fuelFormulation set fuelSubtypeID = 12 where fuelSubtypeID <> 12 and ETOHVolume >= 9    and ETOHVolume < 12.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);
	update fuelFormulation set fuelSubtypeID = 13 where fuelSubtypeID <> 13 and ETOHVolume >= 6    and ETOHVolume < 9 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);
	update fuelFormulation set fuelSubtypeID = 14 where fuelSubtypeID <> 14 and ETOHVolume >= 0.10 and ETOHVolume < 6 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);
	update fuelFormulation set fuelSubtypeID = 15 where fuelSubtypeID <> 15 and ETOHVolume >= 12.5 and ETOHVolume < 17.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);
	update fuelFormulation set fuelSubtypeID = 51 where fuelSubtypeID <> 51 and ETOHVolume >= 70.5 and ETOHVolume <= 100 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);
	update fuelFormulation set fuelSubtypeID = 52 where fuelSubtypeID <> 52 and ETOHVolume >= 50.5 and ETOHVolume < 70.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);
	update fuelFormulation set fuelSubtypeID = 18 where fuelSubtypeID <> 18 and ETOHVolume >= 17.5 and ETOHVolume < 50.5 and fuelSubtypeID in (10,11,12,13,14,15,51,52,18);

	-- Complain about fuel types that were imported but won't be used
	if(scale = 3) then
		insert into importTempMessages (message)
		select distinct concat('Warning: Fuel type ',fuelTypeID,' is imported but will not be used') as message
		from nrFuelSupply fs
		inner join ##defaultDatabase##.fuelFormulation ff using (fuelFormulationID)
		inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID)
		where fuelTypeID not in (##fuelTypeIDs##)
		union
		select distinct concat('Warning: Fuel type ',fuelTypeID,' is imported but will not be used') as message
		from nrFuelSupply fs
		inner join fuelFormulation ff using (fuelFormulationID)
		inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID)
		where fuelTypeID not in (##fuelTypeIDs##);
	else
		if(mode = 2 or scale in (1,2)) then
			insert into importTempMessages (message)
			select distinct concat('Warning: Fuel type ',fuelTypeID,' is imported but will not be used') as message
			from fuelSupply fs
			inner join fuelFormulation ff using (fuelFormulationID)
			inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID)
			where fuelTypeID not in (##fuelTypeIDs##);
		else
			insert into importTempMessages (message)
			select distinct concat('Warning: Fuel type ',fuelTypeID,' is imported but will not be used') as message
			from fuelSupply fs
			inner join ##defaultDatabase##.fuelFormulation ff using (fuelFormulationID)
			inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID)
			where fuelTypeID not in (##fuelTypeIDs##)
			union
			select distinct concat('Warning: Fuel type ',fuelTypeID,' is imported but will not be used') as message
			from fuelSupply fs
			inner join fuelFormulation ff using (fuelFormulationID)
			inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID)
			where fuelTypeID not in (##fuelTypeIDs##);
		end if;
	end if;

	-- Complain about fixable gaps in T50/T90/E200/E300 data (only for gasoline & gasohol)
	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' is using calculated E200') as message
	from fuelFormulation where T50 is not null and T50 > 0 and (e200 is null or e200 <= 0)
	and fuelSubtypeID in (10, 11, 12, 13, 14, 15);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' is using calculated E300') as message
	from fuelFormulation where T90 is not null and T90 > 0 and (e300 is null or e300 <= 0)
	and fuelSubtypeID in (10, 11, 12, 13, 14, 15);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' is using calculated T50') as message
	from fuelFormulation where e200 is not null and e200 > 0 and (T50 is null or T50 <= 0)
	and fuelSubtypeID in (10, 11, 12, 13, 14, 15);

	insert into importTempMessages (message)
	select distinct concat('Warning: Fuel formulation ',fuelFormulationID,' is using calculated T90') as message
	from fuelFormulation where e300 is not null and e300 > 0 and (T90 is null or T90 <= 0)
	and fuelSubtypeID in (10, 11, 12, 13, 14, 15);

	-- Complain about unfixable gaps in T50/T90/E200/E300 data (only for gasoline & gasohol)
	insert into importTempMessages (message)
	select distinct concat('ERROR: Fuel formulation ',fuelFormulationID,' is missing both E200 and T50') as message
	from fuelFormulation where (T50 is null or T50 <= 0) and (e200 is null or e200 <= 0)
	and fuelSubtypeID in (10, 11, 12, 13, 14, 15);

	insert into importTempMessages (message)
	select distinct concat('ERROR: Fuel formulation ',fuelFormulationID,' is missing both E300 and T90') as message
	from fuelFormulation where (T90 is null or T90 <= 0) and (e300 is null or e300 <= 0)
	and fuelSubtypeID in (10, 11, 12, 13, 14, 15);

	-- Fill gaps in T50/T90/E200/E300 data (only for gasoline & gasohol)
	update fuelFormulation set T50 = 2.0408163 * (147.91 - e200) where e200 is not null and e200 > 0 and (T50 is null or T50 <= 0) and fuelSubTypeID in (10, 11, 12, 13, 14, 15);
	update fuelFormulation set T90 = 4.5454545 * (155.47 - e300) where e300 is not null and e300 > 0 and (T90 is null or T90 <= 0) and fuelSubTypeID in (10, 11, 12, 13, 14, 15);
	update fuelFormulation set e200 = 147.91-(T50/2.0408163) where T50 is not null and T50 > 0 and (e200 is null or e200 <= 0) and fuelSubTypeID in (10, 11, 12, 13, 14, 15);
	update fuelFormulation set e300 = 155.47-(T90/4.5454545) where T90 is not null and T90 > 0 and (e300 is null or e300 <= 0) and fuelSubTypeID in (10, 11, 12, 13, 14, 15);
	
	-- Ensure market shares sum to 1.0 for all fuel types, year, month, counties.
	drop table if exists tempFuelSupplyNotUnity;
	
	drop table if exists tempFuelSupplyUnion;

	if(scale = 3) then
		create table tempFuelSupplyUnion
		select fuelTypeID, fuelRegionID, fuelYearID, monthGroupID, marketShare, fuelFormulationID
		from nrFuelSupply fs
		inner join ##defaultDatabase##.fuelFormulation ff using (fuelFormulationID)
		inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID)
		union
		select fuelTypeID, fuelRegionID, fuelYearID, monthGroupID, marketShare, fuelFormulationID
		from nrFuelSupply fs
		inner join fuelFormulation ff using (fuelFormulationID)
		inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID);
	else
		if(mode = 2 or scale in (1,2)) then
			create table tempFuelSupplyUnion
			select fuelTypeID, fuelRegionID, fuelYearID, monthGroupID, marketShare, fuelFormulationID
			from fuelSupply fs
			inner join fuelFormulation ff using (fuelFormulationID)
			inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID);
		else
			create table tempFuelSupplyUnion
			select fuelTypeID, fuelRegionID, fuelYearID, monthGroupID, marketShare, fuelFormulationID
			from fuelSupply fs
			inner join ##defaultDatabase##.fuelFormulation ff using (fuelFormulationID)
			inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID)
			union
			select fuelTypeID, fuelRegionID, fuelYearID, monthGroupID, marketShare, fuelFormulationID
			from fuelSupply fs
			inner join fuelFormulation ff using (fuelFormulationID)
			inner join ##defaultDatabase##.fuelSubType fst using (fuelSubTypeID);
		end if;
	end if;

	create table tempFuelSupplyNotUnity
	select fuelTypeID, fuelRegionID, fuelYearID, monthGroupID, sum(marketShare) as sumMarketShare
	from tempFuelSupplyUnion fs
	group by fuelTypeID, fuelRegionID, fuelYearID, monthGroupID
	having round(sum(marketShare),4) <> 1.0000;

	drop table if exists tempFuelSupplyUnion;

	insert into importTempMessages (message)
	select concat('ERROR: Region ',fuelRegionID,', year ',fuelYearID,', month ',monthGroupID,', fuel type ',fuelTypeID,' market share is not 1.0 but instead ',round(sumMarketShare,4))
	from tempFuelSupplyNotUnity;
	
	drop table if exists tempFuelSupplyNotUnity;

	if(scale < 3 and useFuelUsageFraction > 0) then
		-- -----------------------------------------------------------------------------------------------------
		-- Check fuelUsageFraction table
		-- -----------------------------------------------------------------------------------------------------
	
		-- Complain about any years outside of MOVES's range
		insert into importTempMessages (message)
		select distinct concat('ERROR: Fuel Year ',fuelYearID,' is outside the range of 1990-2060 and cannot be used') as errorMessage
		from fuelUsageFraction
		where fuelYearID < 1990 or fuelYearID > 2060
		and usageFraction > 0;
		
		-- if(mode = 0) then
		-- 	-- Remove records with zero usage
		-- 	delete from fuelUsageFraction where usageFraction < 0.0001;
		-- end if;
		
		-- Ensure usage fractions sum to 1.0 for all counties, fuel years, model year groups, and sourcebin fuel types.
		drop table if exists tempFuelUsageFractionNotUnity;
		
		drop table if exists tempFuelUsageFractionNotUnity;
	
		create table tempFuelUsageFractionNotUnity
		select countyID, fuelYearID, modelYearGroupID, sourceBinFuelTypeID, sum(usageFraction) as sumUsageFraction
		from fuelUsageFraction
		group by countyID, fuelYearID, modelYearGroupID, sourceBinFuelTypeID
		having round(sum(usageFraction),4) <> 1.0000;
	
		insert into importTempMessages (message)
		select concat('ERROR: County ',countyID,', year ',fuelYearID,', model year group ',modelYearGroupID,', source fuel type ',sourceBinFuelTypeID,' usage fraction is not 1.0 but instead ',round(sumUsageFraction,4))
		from tempFuelUsageFractionNotUnity;
		
		drop table if exists tempFuelUsageFractionNotUnity;
	end if;

	-- Check AVFT
	if(scale < 3) then
		set howMany=0;
		select count(*) into howMany from avft;
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
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
			select concat('Warning: source type ',sourceTypeID,', model year ',modelYearID,', fuel engine fraction is not 1.0 but instead ',round(sum(fuelEngFraction),4))
			from avft 
			group by sourceTypeID, modelYearID
			having round(sum(fuelEngFraction),4) < 1.0000 and round(sum(fuelEngFraction),4) > 0.0000;
	
			insert into importTempMessages (message)
			select distinct concat('ERROR: Imported AVFT is missing source type ',sourceTypeID, ', model year ',modelYearID,', fuel ',fuelTypeID) as message
			from (
				select distinct sourceTypeID, modelYearID, fuelTypeID
				from ##defaultDatabase##.sampleVehiclePopulation
				where sourceTypeID in (##sourceTypeIDs##)
			) t1
			left outer join avft using (sourceTypeID, modelYearID, fuelTypeID)
			where avft.sourceTypeID is null 
				and avft.modelYearID is null 
				and avft.fuelTypeID is null
				and t1.sourceTypeID in (##sourceTypeIDs##)
			and t1.modelYearID in (
				select distinct modelYearID
				from ##defaultDatabase##.modelYear,
				##defaultDatabase##.year
				where yearID in (##yearIDs##)
				and modelYearID >= yearID - 40
				and modelYearID <= yearID
			)
			order by t1.sourceTypeID, t1.modelYearID, t1.fuelTypeID;
						
			insert into importTempMessages (message)
			SELECT concat('ERROR: No emission rates for source type ',sourceTypeID,', MY',modelYearID,', fuel type ',fuelTypeID,', engTech ',engTechID,'. Reassign this AVFT fuelEngFraction to a valid fuel/engTech combination.   ')
			FROM ( SELECT sourceTypeID, modelYearID, fuelTypeID, engTechID, sum(fuelEngFraction * COALESCE(stmyFuelEngFraction, 0)) AS effectiveFraction
				FROM avft
				LEFT JOIN ##defaultDatabase##.samplevehiclepopulation USING (sourceTypeID, modelYearID, fuelTypeID, engTechID)
				WHERE fuelEngFraction <> 0 AND sourceTypeID IN (##sourceTypeIDs##)
				  and avft.modelYearID in (select distinct modelYearID
										   from ##defaultDatabase##.modelYear,
											    ##defaultDatabase##.year
				                           where yearID in (##yearIDs##)
				                             and modelYearID >= yearID - 40
				                             and modelYearID <= yearID)
				GROUP BY sourceTypeID, modelYearID, fuelTypeID, engTechID
				HAVING effectiveFraction = 0
			    ORDER BY avft.sourceTypeID, avft.modelYearID, avft.fuelTypeID, avft.engTechID
			) as t1;
	
		end if;
	end if;

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

call spCheckFuelSupplyImporter();
drop procedure if exists spCheckFuelSupplyImporter;
