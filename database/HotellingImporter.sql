-- Author Wesley Faler
-- Version 2017-09-19

drop procedure if exists spCheckHotellingImporter;

BeginBlock
create procedure spCheckHotellingImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;

	-- Scale 0 is national
	-- Scale 1 is single county
	-- Scale 2 is project domain
	declare scale int default ##scale##;

	-- Rate 0 is Inventory
	-- Rate 1 is Rates
	declare rate int default ##rate##;

	-- Complain about an empty table in project domain
	if(scale = 2 and (90 in (##processIDs##) or 91 in (##processIDs##))) then
		set howMany=0;
		select count(*) into howMany from hotellingActivityDistribution;
		set howMany=ifnull(howMany,0);
		if(howMany <= 0) then
			insert into importTempMessages (message) values ('ERROR: hotellingActivityDistribution must be provided.');
		end if;
	end if;

	-- Check hotellingHourFraction if any entries are provided
	set howMany=0;
	select count(*) into howMany from hotellingHourFraction;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		-- Complain about zone/days with distributions that don't sum to exactly 1.0000
		insert into importTempMessages (message)
		select concat('ERROR: total HotellingHourFraction.hourFraction for zone ',zoneID,', day ',dayID,' should be 1 but instead is ',round(sum(hourFraction),4)) as errorMessage
		from hotellingHourFraction
		group by zoneID, dayID
		having round(sum(hourFraction),4) <> 1.0000;
	end if;

	-- Check hotellingAgeFraction if any entries are provided
	set howMany=0;
	select count(*) into howMany from hotellingAgeFraction;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		-- Complain about zones with distributions that don't sum to exactly 1.0000
		insert into importTempMessages (message)
		select concat('ERROR: total HotellingAgeFraction.ageFraction for zone ',zoneID,' should be 1 but instead is ',round(sum(ageFraction),4)) as errorMessage
		from hotellingAgeFraction
		group by zoneID
		having round(sum(ageFraction),4) <> 1.0000;
	end if;

	-- Complain about invalid operating modes
	insert into importTempMessages (message)
	select distinct concat('ERROR: Unknown opModeID (',opModeID,'). Hotelling operating modes are 200, 201, 203, and 204.') as errorMessage
	from hotellingActivityDistribution
	where opModeID not in (200, 201, 203, 204);
	
	-- Complain about invalid operating mode / fuel type combinations
	insert into importTempMessages (message)
	select distinct concat('ERROR: Cannot use a non-zero opModeFraction for electricity (fuelTypeID 9) and extended idle (opModeID 200)') as errorMessage
	from hotellingActivityDistribution
	where fuelTypeID = 9 and opModeID = 200 and opModeFraction <> 0;
	insert into importTempMessages (message)
	select distinct concat('ERROR: Cannot use a non-zero opModeFraction for electricity (fuelTypeID 9) and diesel APU usage (opModeID 201)') as errorMessage
	from hotellingActivityDistribution
	where fuelTypeID = 9 and opModeID = 201 and opModeFraction <> 0;
	insert into importTempMessages (message)
	select distinct concat('ERROR: Cannot use a non-zero opModeFraction for CNG (fuelTypeID 3) and diesel APU usage (opModeID 201)') as errorMessage
	from hotellingActivityDistribution
	where fuelTypeID = 3 and opModeID = 201 and opModeFraction <> 0;

	-- Complain if any model year ranges are inverted
	insert into importTempMessages (message)
	select distinct concat('ERROR: BeginModelYearID (',beginModelYearID,') must be <= EndModelYearID (',endModelYearID,')') as errorMessage
	from hotellingActivityDistribution
	where beginModelYearID > endModelYearID;

	-- Complain about entries with negative fractions
	insert into importTempMessages (message)
	select concat('ERROR: opModeFraction is less than zero (',opModeFraction,') for model years ',beginModelYearID,' to ',endModelYearID) as errorMessage
	from hotellingActivityDistribution
	where opModeFraction < 0;

	-- Complain about entries with fractions greater than 1
	insert into importTempMessages (message)
	select concat('ERROR: opModeFraction is greater than 1 (',round(opModeFraction,4),') for model years ',beginModelYearID,' to ',endModelYearID) as errorMessage
	from hotellingActivityDistribution
	where round(opModeFraction,4) > 1;

	-- Expand to full set of zones / model years / fuel types
	drop table if exists tempYear;
	create table if not exists tempYear (
		year int not null primary key
	) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
	insert into tempYear(year) values(1950),(1951),(1952),(1953),(1954),(1955),(1956),(1957)
		,(1958),(1959),(1960),(1961),(1962),(1963),(1964),(1965),(1966),(1967)
		,(1968),(1969),(1970),(1971),(1972),(1973),(1974),(1975),(1976),(1977)
		,(1978),(1979),(1980),(1981),(1982),(1983),(1984),(1985),(1986),(1987)
		,(1988),(1989),(1990),(1991),(1992),(1993),(1994),(1995),(1996),(1997)
		,(1998),(1999),(2000),(2001),(2002),(2003),(2004),(2005),(2006),(2007)
		,(2008),(2009),(2010),(2011),(2012),(2013),(2014),(2015),(2016),(2017)
		,(2018),(2019),(2020),(2021),(2022),(2023),(2024),(2025),(2026),(2027)
		,(2028),(2029),(2030),(2031),(2032),(2033),(2034),(2035),(2036),(2037)
		,(2038),(2039),(2040),(2041),(2042),(2043),(2044),(2045),(2046),(2047)
		,(2048),(2049),(2050),(2051),(2052),(2053),(2054),(2055),(2056),(2057)
		,(2058),(2059),(2060)
	;
	drop table if exists tempHotellingActivityDistribution;
	create table if not exists tempHotellingActivityDistribution (
		zoneID int,
		fuelTypeID  smallint(6),
		modelYearID smallint(6),
		opModeID smallint(6),
		opModeFraction float,
		key (zoneID, fuelTypeID, modelYearID, opModeID),
		key (zoneID, fuelTypeID, opModeID, modelYearID)
	) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

	-- first, expand model year ranges
	insert into tempHotellingActivityDistribution (zoneID, fuelTypeID, modelYearID, opModeID, opModeFraction)
	select zoneID, fuelTypeID, year, opModeID, opModeFraction
	from hotellingActivityDistribution, tempYear
	where beginModelYearID <= year
	and endModelYearID >= year;
	
	-- now, look for missing fuel type / model year combinations
	insert into tempHotellingActivityDistribution (zoneID, fuelTypeID, modelYearID, opModeID, opModeFraction)
	select z.zoneID, ft.fuelTypeID, year, opModeID, opModeFraction
	from tempYear
	join (select distinct fuelTypeID from hotellingactivitydistribution) AS ft
	join (select distinct zoneID from hotellingactivitydistribution) AS z
 	left join tempHotellingActivityDistribution thad ON (modelYearID <= year and modelYearID >= year 
	                                                     AND ft.fuelTypeID = thad.fuelTypeID
														 AND z.zoneID = thad.zoneID)
	where opModeFraction is null;

	-- Complain about model years that appear more than once
	insert into importTempMessages (message)
	select distinct concat('ERROR: Model year ',modelYearID,' appears more than once (',count(*),') for zone ',zoneID) as errorMessage
	from tempHotellingActivityDistribution
	group by zoneID, fuelTypeID, modelYearID, opModeID
	having count(*) > 1;

	-- Complain about model years with distributions that don't sum to exactly 1.0000 or are missing
    insert into importTempMessages (message)
	select concat('ERROR: total opModeFraction for zone ',zoneID,', model year ',modelYearID,', and fuel type ', fuelTypeID,
	              ' should be 1 but instead is ',coalesce(round(sum(opModeFraction),4), 'NULL')) as errorMessage
	from tempHotellingActivityDistribution
	group by zoneID, fuelTypeID, modelYearID
	having round(sum(opModeFraction),4) <> 1.0000 OR SUM(opModeFraction) IS null;

	-- Cleanup
	drop table if exists tempYear;
	drop table if exists tempHotellingActivityDistribution;

	-- Check final status
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

call spCheckHotellingImporter();
drop procedure if exists spCheckHotellingImporter;
