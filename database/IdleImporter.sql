-- Author Wesley Faler
-- Version 2017-09-30

drop procedure if exists spCheckIdleImporter;

BeginBlock
create procedure spCheckIdleImporter()
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

	-- Complain if any model year ranges are inverted
	insert into importTempMessages (message)
	select distinct concat('ERROR: totalIdleFraction minModelYearID (',minModelYearID,') must be <= maxModelYearID (',maxModelYearID,')') as errorMessage
	from totalIdleFraction
	where minModelYearID > maxModelYearID;

	insert into importTempMessages (message)
	select distinct concat('ERROR: idleModelYearGrouping minModelYearID (',minModelYearID,') must be <= maxModelYearID (',maxModelYearID,')') as errorMessage
	from idleModelYearGrouping
	where minModelYearID > maxModelYearID;

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
		,(2048),(2049),(2050),(2051),(2052),(2053),(2054),(2055),(2056),(2057)
		,(2058),(2059),(2060)
	;

	drop table if exists tempTotalIdleFraction;
	create table if not exists tempTotalIdleFraction (
		idleRegionID int not null,
		countyTypeID int not null,
		sourceTypeID smallint not null,
		monthID smallint not null,
		dayID smallint not null,
		modelYearID smallint not null,
		totalIdleFraction double not null,
		key (idleRegionID, countyTypeID, sourceTypeID, monthID, dayID, modelYearID)
	);

	insert into tempTotalIdleFraction (idleRegionID,countyTypeID,sourceTypeID,monthID,dayID,modelYearID,totalIdleFraction)
	select idleRegionID,countyTypeID,sourceTypeID,monthID,dayID,year as modelYearID,totalIdleFraction
	from totalIdleFraction, tempYear
	where minModelYearID <= year
	and maxModelYearID >= year;

	drop table if exists tempIdleModelYearGrouping;
	create table if not exists tempIdleModelYearGrouping (
		sourceTypeID smallint not null,
		modelYearID smallint not null,
		totalIdleFraction double not null,
		key (sourceTypeID, modelYearID)
	);

	insert into tempIdleModelYearGrouping (sourceTypeID,modelYearID,totalIdleFraction)
	select sourceTypeID,year as modelYearID,totalIdleFraction
	from idleModelYearGrouping, tempYear
	where minModelYearID <= year
	and maxModelYearID >= year;

	-- Complain about model years that appear more than once
	insert into importTempMessages (message)
	select distinct concat('ERROR: totalIdleFraction model year ',modelYearID,
		' appears more than once (',count(*),') for idle region ',idleRegionID,
		', county type ',countyTypeID,', source type ',sourceTypeID,
		', month ',monthID,', day ',dayID) as errorMessage
	from tempTotalIdleFraction
	group by idleRegionID, countyTypeID, sourceTypeID, monthID, dayID, modelYearID
	having count(*) > 1;

	insert into importTempMessages (message)
	select distinct concat('ERROR: idleModelYearGrouping model year ',modelYearID,
		' appears more than once (',count(*),') for source type ',sourceTypeID) as errorMessage
	from tempIdleModelYearGrouping
	group by sourceTypeID, modelYearID
	having count(*) > 1;

	-- Complain about a TIF of 1
	insert into importTempMessages (message)
	select distinct concat('ERROR: totalIdleFraction is >= 1 for source type ', sourceTypeID) as errorMessage
	from totalIdleFraction tif
	where tif.totalIdleFraction >= 1
	group by sourceTypeID;
	
	insert into importTempMessages (message)
	select distinct concat('ERROR: totalIdleFraction is >= 1 for source type ', sourceTypeID) as errorMessage
	from idleModelYearGrouping
	where totalIdleFraction >= 1
	group by sourceTypeID;

	-- Cleanup
	drop table if exists tempYear;
	drop table if exists tempTotalIdleFraction;
	drop table if exists tempIdleModelYearGrouping;

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

call spCheckIdleImporter();
drop procedure if exists spCheckIdleImporter;
