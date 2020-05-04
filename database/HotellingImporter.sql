-- Author Wesley Faler
-- Version 2014-01-11

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

	-- Complain about invalid operating modes
	insert into importTempMessages (message)
	select distinct concat('ERROR: Unknown opModeID (',opModeID,'). Hotelling operating modes are 200-299.') as errorMessage
	from hotellingActivityDistribution
	where opModeID < 200 || opModeID > 299;

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

	-- Expand to full set of model years
	drop table if exists tempYear;
	create table if not exists tempYear (
		year int not null primary key
	);
	
	insert into tempYear(year) values(1960),(1961),(1962),(1963),(1964),(1965),(1966),(1967)
		,(1968),(1969),(1970),(1971),(1972),(1973),(1974),(1975),(1976),(1977)
		,(1978),(1979),(1980),(1981),(1982),(1983),(1984),(1985),(1986),(1987)
		,(1988),(1989),(1990),(1991),(1992),(1993),(1994),(1995),(1996),(1997)
		,(1998),(1999),(2000),(2001),(2002),(2003),(2004),(2005),(2006),(2007)
		,(2008),(2009),(2010),(2011),(2012),(2013),(2014),(2015),(2016),(2017)
		,(2018),(2019),(2020),(2021),(2022),(2023),(2024),(2025),(2026),(2027)
		,(2028),(2029),(2030),(2031),(2032),(2033),(2034),(2035),(2036),(2037)
		,(2038),(2039),(2040),(2041),(2042),(2043),(2044),(2045),(2046),(2047)
		,(2048),(2049),(2050)
	;

	drop table if exists tempHotellingActivityDistribution;
	create table if not exists tempHotellingActivityDistribution (
		modelYearID smallint(6) not null,
		opModeID smallint(6) not null,
		opModeFraction float not null,
		key (modelYearID, opModeID),
		key (opModeID, modelYearID)
	);

	insert into tempHotellingActivityDistribution (modelYearID, opModeID, opModeFraction)
	select year, opModeID, opModeFraction
	from hotellingActivityDistribution, tempYear
	where beginModelYearID <= year
	and endModelYearID >= year;

	-- Complain about model years that appear more than once
	insert into importTempMessages (message)
	select distinct concat('ERROR: Model year ',modelYearID,' appears more than once (',count(*),')') as errorMessage
	from tempHotellingActivityDistribution
	group by modelYearID, opModeID
	having count(*) > 1;

	-- Complain about model years with distributions that don't sum to exactly 1.0000
	insert into importTempMessages (message)
	select concat('ERROR: total opModeFraction for model year ',modelYearID,' should be 1 but instead ',round(sum(opModeFraction),4)) as errorMessage
	from tempHotellingActivityDistribution
	group by modelYearID
	having round(sum(opModeFraction),4) <> 1.0000;

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
