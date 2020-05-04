-- Nonroad Post Processing Script (updated 7/26/2018):
-- Emission factors in grams per operating hour by SCC
--  
-- MOVES-Nonroad Output Guidance:
--       SCC must be selected and present in the results. This script
--       will run faster if model year, engine tech, and HP class
--       are not selected, and if there is only one sector, year,
--       month, and day in the output.
-- 
-- When prompted to save, specify one of the following file types: .xlsx, .xls, or .txt
-- The raw output of this script is also stored in the output database in a table called:
-- EmissionFactors_per_operatinghour_by_SCC
-- 
-- WARNING:
--       This script may take a long time to complete depending on
--       the size of the output database. A confirmation notice will
--       alert you when this action has completed.

flush tables;

-- Set up indexing
set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index1');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index1 on movesoutput ( MOVESRunID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index101');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index101 on movesoutput ( MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc,pollutantID,processID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesactivityoutput' and index_name = 'index10');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index10 on movesactivityoutput ( activitytypeid )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

-- Get time units
set @timeUnits := (select timeUnits from movesrun limit 1);

-- Get hours of activity 
drop table if exists hours;
create table hours
select 
	MOVESRunID,
	yearID,
	monthID,
	dayID,
	stateID,
	countyID,
	SCC,
	fuelTypeID,
	modelYearID,
	engTechID,
	hpID,
	activity as hours
from movesactivityoutput  
where activitytypeid = 2;

create index index1 on hours (MOVESRunID,yearID,monthID,dayID,stateID,countyID,SCC);


-- Set up unit conversions table
drop table if exists units;
create table units (fromUnit char(5), factor double, description text);
insert into units values 
('ton', 907185, 'From U.S. tons to grams'),
('lb', 453.592, 'From lbm to grams'),
('kg', 1000, 'From kg to grams'),
('g', 1, 'From grams to grams');


-- Get inventories by SCC
drop table if exists temp1;
create table temp1
select 
	MOVESRunID,
	yearID,
	monthID,
	dayID,
	stateID,
	countyID,
	scc,
    pollutantID,
    processID,
    units.factor * sum(emissionQuant) as emissionQuant
from movesoutput m
left join movesrun using (movesrunid)
left join units on (movesrun.massUnits = units.fromUnit)
group by MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc,pollutantID,processID;

create index index1 on temp1 (MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc);
create index index2 on temp1 (scc);


-- Get hours by SCC
drop table if exists temp2;
create table temp2
select 
	MOVESRunID,
	yearID,
	monthID,
	dayID,
	stateID,
	countyID,
	scc,
    sum(hours) as hours
from hours
group by MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc;

create index index1 on temp2 (MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc);


-- Join temp1 and temp2 and calculate the emission rate for the resulting output table
drop table if exists EmissionFactors_per_operatinghour_by_SCC;
create table EmissionFactors_per_operatinghour_by_SCC
select
	b1.MOVESRunID,
	b1.yearID,
	b1.monthID,
	b1.dayID,
	b1.stateID,
	b1.countyID,
	b1.scc,
	s.description as sccDescription,
	s.fuelTypeID,
    b1.pollutantID,
    b1.processID,
	b1.emissionQuant,
    b2.hours,
    IF(b2.hours != 0, b1.emissionQuant / b2.hours, NULL) as emissionRate,
    'g/hr' as emissionRateUnits
from temp1 b1
inner join temp2 b2 
	on ((b1.MOVESRunID=b2.MOVESRunID) AND
		(b1.yearID=b2.yearID) AND 
		(b1.monthID=b2.monthID) AND 
		(b1.dayID=b2.dayID) AND 
		(b1.stateID=b2.stateID OR b1.stateID IS NULL AND b2.stateID IS NULL) AND 
		(b1.countyID=b2.countyID OR b1.countyID IS NULL AND b2.countyID IS NULL) AND 
		(b1.SCC=b2.SCC))
left join ##defaultdb##.nrscc s on (b1.scc=s.scc);

drop table if exists hours;
drop table if exists temp1;
drop table if exists temp2;
drop table if exists units;