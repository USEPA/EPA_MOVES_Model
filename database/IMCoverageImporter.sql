-- Author Wesley Faler
-- Version 2016-10-04

drop procedure if exists spCheckIMImporter;

BeginBlock
create procedure spCheckIMImporter()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure for national domain
	-- Mode 2 is run to check overall success/failure for county and project domains
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;
-- 	declare defaultRecordCount int default 0;

	update IMCoverage set useIMyn='Y' where useIMyn='y';

	drop table if exists tempSourceFuelType;
	create table if not exists tempSourceFuelType (
		sourceTypeID smallint not null,
		fuelTypeID smallint not null,
		primary key (sourceTypeID, fuelTypeID),
		key (fuelTypeID, sourceTypeID)
	) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
	insert into tempSourceFuelType (sourceTypeID, fuelTypeID)
	select sourceTypeID, fuelTypeID
	from ##defaultDatabase##.sourceUseType,
	##defaultDatabase##.fuelType
	where (sourceTypeID*100 + fuelTypeID) in (##sourceFuelTypeIDs##);

	drop table if exists tempModelYear;
	create table if not exists tempModelYear (
		modelYearID smallint not null primary key
	) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

	insert into tempModelYear(modelYearID) values(1950),(1951),(1952),(1953),(1954),(1955),(1956),(1957)
		,(1958),(1959),(1960),(1961),(1962),(1963),(1964),(1965),(1966),(1967)
		,(1968),(1969),(1970),(1971),(1972),(1973),(1974),(1975),(1976),(1977),(1978),(1979),(1980),(1981),(1982),(1983),(1984),(1985),(1986),(1987)
		,(1988),(1989),(1990),(1991),(1992),(1993),(1994),(1995),(1996),(1997),(1998),(1999),(2000),(2001),(2002),(2003),(2004),(2005),(2006),(2007)
		,(2008),(2009),(2010),(2011),(2012),(2013),(2014),(2015),(2016),(2017),(2018),(2019),(2020),(2021),(2022),(2023),(2024),(2025),(2026),(2027)
		,(2028),(2029),(2030),(2031),(2032),(2033),(2034),(2035),(2036),(2037),(2038),(2039),(2040),(2041),(2042),(2043),(2044),(2045),(2046),(2047)
		,(2048),(2049),(2050),(2051),(2052),(2053),(2054),(2055),(2056),(2057),(2058),(2059),(2060);

	drop table if exists expandedImportRecords;
	create table if not exists expandedImportRecords (
		polProcessID int not null,
		countyID int not null,
		yearID smallint not null,
		sourceTypeID smallint not null,
		fuelTypeID smallint not null,
		modelYearID smallint not null,
		key (polProcessID, countyID, yearID, sourceTypeID, fuelTypeID, modelYearID)
	) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- 	select count(*) into defaultRecordCount
-- 	from ##defaultDatabase##.IMCoverage
-- 	inner join tempSourceFuelType using (sourceTypeID, fuelTypeID)
-- 	where countyID in (##countyIDs##) and yearID in (##yearIDs##) and polProcessID in (##polProcessIDs##);
-- 
-- 	if(defaultRecordCount > 0) then
-- 		-- Expand the imported records regardless of active/inactive
-- 		truncate table expandedImportRecords;
-- 		insert into expandedImportRecords (polProcessID, countyID, yearID, sourceTypeID, fuelTypeID, modelYearID)
-- 		select distinct imc.polProcessID, imc.countyID, imc.yearID, imc.sourceTypeID, imc.fuelTypeID, tempModelYear.modelYearID
-- 		from IMCoverage imc
-- 		inner join tempModelYear on (begModelYearID <= modelYearID and modelYearID <= endModelYearID)
-- 		inner join tempSourceFuelType using (sourceTypeID, fuelTypeID)
-- 		where countyID in (##countyIDs##) and yearID in (##yearIDs##) and polProcessID in (##polProcessIDs##);
-- 
-- 		drop table if exists expandedDefaultRecords;
-- 		create table if not exists expandedDefaultRecords (
-- 			polProcessID int not null,
-- 			countyID int not null,
-- 			yearID smallint not null,
-- 			sourceTypeID smallint not null,
-- 			fuelTypeID smallint not null,
-- 			modelYearID smallint not null,
-- 			key (polProcessID, countyID, yearID, sourceTypeID, fuelTypeID, modelYearID)
-- 		);
-- 		insert into expandedDefaultRecords (polProcessID, countyID, yearID, sourceTypeID, fuelTypeID, modelYearID)
-- 		select distinct imc.polProcessID, imc.countyID, imc.yearID, imc.sourceTypeID, imc.fuelTypeID, tempModelYear.modelYearID
-- 		from ##defaultDatabase##.IMCoverage imc
-- 		inner join tempModelYear on (begModelYearID <= modelYearID and modelYearID <= endModelYearID)
-- 		inner join tempSourceFuelType using (sourceTypeID, fuelTypeID)
-- 		where countyID in (##countyIDs##) and yearID in (##yearIDs##) and polProcessID in (##polProcessIDs##);
-- 
-- 		-- If any of the default records has no corresponding imported record, isOk=0
-- 		set howMany = 0;
-- 		select d.polProcessID into howMany
-- 		from expandedDefaultRecords d
-- 		left outer join expandedImportRecords i using (polProcessID, countyID, yearID, sourceTypeID, fuelTypeID, modelYearID)
-- 		where i.polProcessID is null
-- 		limit 1;
-- 		if(howMany > 0) then
-- 			set isOk = 0;
-- 			insert into importTempMessages (message) values ('ERROR: Imported data does not yet cover all of the default cases');
-- 		end if;
-- 	end if;

	-- Complain about imported active records that overlap
	-- Expand the imported active records
	truncate table expandedImportRecords;
	insert into expandedImportRecords (polProcessID, countyID, yearID, sourceTypeID, fuelTypeID, modelYearID)
	select imc.polProcessID, imc.countyID, imc.yearID, imc.sourceTypeID, imc.fuelTypeID, tempModelYear.modelYearID
	from IMCoverage imc
	inner join tempModelYear on (begModelYearID <= modelYearID and modelYearID <= endModelYearID)
	inner join tempSourceFuelType using (sourceTypeID, fuelTypeID)
	where countyID in (##countyIDs##) and yearID in (##yearIDs##) and polProcessID in (##polProcessIDs##)
	and imc.useIMyn='Y';

	insert into importTempMessages (message)
	select concat('ERROR: Duplicate active program for pol/proc ',polProcessID,' in county ',countyID,', year ',yearID,' for source type ',sourceTypeID,', fuel ',fuelTypeID,', model year ',modelYearID) as errorMessage
	from expandedImportRecords
	group by polProcessID, countyID, yearID, sourceTypeID, fuelTypeID, modelYearID
	having count(*) > 1;

	if(isOk=1) then
		set howMany=0;
		select count(*) into howMany from importTempMessages where message like 'ERROR: Duplicate active program%';
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
			set isOk=0;
		end if;
	end if;

	-- Complain about any years outside of MOVES's range
	insert into importTempMessages (message)
	select distinct concat('ERROR: Year ',yearID,' is outside the range of 1990-2060 and cannot be used') as errorMessage
	from imCoverage
	where yearID < 1990 or yearID > 2060;
	if(isOk=1) then
		set howMany=0;
		select count(*) into howMany from importTempMessages where message like 'ERROR: Year%';
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
			set isOk=0;
		end if;
	end if;

	-- Complain about evap/exhaust process and test mismatches
	insert into importTempMessages (message)
	select distinct concat('ERROR: Pollutant/Process ',imc.polProcessID,' cannot use test standard ',imc.testStandardsID,' (',ts.testStandardsDesc,')') as errorMessage
	from IMCoverage imc
	inner join ##defaultDatabase##.IMTestStandards ts using (testStandardsID)
	inner join ##defaultDatabase##.PollutantProcessAssoc ppa using (polProcessID)
	inner join ##defaultDatabase##.Pollutant using (pollutantID)
	inner join ##defaultDatabase##.EmissionProcess using (processID)
	where (left(testStandardsDesc,4)='Evap' and isAffectedByEvapIM<>'Y')
	or (left(testStandardsDesc,4)<>'Evap' and isAffectedByExhaustIM<>'Y');
	if(isOk=1) then
		set howMany=0;
		select count(*) into howMany from importTempMessages where message like '% cannot use test standard %';
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
			set isOk=0;
		end if;
	end if;

	-- For county and project domains, the IMCoverage table cannot be empty.
	if(mode = 2 and isOk=1) then
		set howMany=0;
		select count(*) into howMany from IMCoverage;
		set howMany=ifnull(howMany,0);
		if(howMany <= 0) then
			set isOk=0;
		end if;
	end if;

	-- Insert 'NOT_READY' or 'OK' to indicate iconic success
	if(mode >= 1) then
		insert into importTempMessages (message) values (case when isOk=1 then 'OK' else 'NOT_READY' end);
	end if;

	drop table if exists tempSourceFuelType;
	drop table if exists tempModelYear;
	drop table if exists expandedImportRecords;
	drop table if exists expandedDefaultRecords;
end
EndBlock

call spCheckIMImporter();
drop procedure if exists spCheckIMImporter;
