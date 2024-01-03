-- Nonroad Post Processing Script (updated 1/2/2024):
-- Emission factors in grams per operating hour by SCC and Model Year
--  
-- MOVES-Nonroad Output Guidance:
--       SCC and Model Year must be selected and present in the results.
--       This script will run faster if engine tech and HP class
--       are not selected, and if there is only one sector, year,
--       month, and day in the output.
-- 
-- When prompted to save, specify one of the following file types: .xlsx, .xls, or .txt
-- The raw output of this script is also stored in the output database in a table called:
-- EmissionFactors_per_operatinghour_by_SCC_and_ModelYear
-- 
-- WARNING:
--       This script may take a long time to complete depending on
--       the size of the output database. A confirmation notice will
--       alert you when this action has completed.

flush tables;

-- Set up indexing for setting NULL values to 0
set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index_state');
set @sqlstmt := if( @exist > 0, 'select ''INFO: index_state already exists.''', 'create index index_state on movesoutput ( stateID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesactivityoutput' and index_name = 'index_state');
set @sqlstmt := if( @exist > 0, 'select ''INFO: index_state already exists.''', 'create index index_state on movesactivityoutput ( stateID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index_county');
set @sqlstmt := if( @exist > 0, 'select ''INFO: index_county already exists.''', 'create index index_county on movesoutput ( countyID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesactivityoutput' and index_name = 'index_county');
set @sqlstmt := if( @exist > 0, 'select ''INFO: index_county already exists.''', 'create index index_county on movesactivityoutput ( countyID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

-- Convert NULLs to 0 to improve joins
UPDATE movesoutput SET stateID = 0 WHERE stateID IS NULL;
UPDATE movesoutput SET countyID = 0 WHERE countyID IS NULL;
UPDATE movesactivityoutput SET stateID = 0 WHERE stateID IS NULL;
UPDATE movesactivityoutput SET countyID = 0 WHERE countyID IS NULL;

-- Set up indexing for everything else
set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index1');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index1 on movesoutput ( MOVESRunID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index104');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index104 on movesoutput ( MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc,modelYearID,pollutantID,processID )');
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

create index index1 on hours (MOVESRunID,yearID,monthID,dayID,stateID,countyID,SCC,modelYearID);


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
	modelYearID,
    pollutantID,
    processID,
    units.factor * sum(emissionQuant) as emissionQuant
from movesoutput m
left join movesrun using (movesrunid)
left join units on (movesrun.massUnits = units.fromUnit)
group by MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc,modelYearID,pollutantID,processID;

create index index1 on temp1 (MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc,modelYearID);
create index index2 on temp1 (scc);


-- Get hours by SCC and ModelYearID
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
	modelYearID,
    sum(hours) as hours
from hours
group by MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc,modelYearID;

create index index1 on temp2 (MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc,modelYearID);


-- Join temp1 and temp2 and calculate the emission rate for the resulting output table
drop table if exists EmissionFactors_per_operatinghour_by_SCC_and_ModelYear;
create table EmissionFactors_per_operatinghour_by_SCC_and_ModelYear
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
	b1.modelYearID,
    b1.pollutantID,
    b1.processID,
	b1.emissionQuant,
    b2.hours,
    IF(b2.hours != 0, b1.emissionQuant / b2.hours, NULL) as emissionRate,
    'g/hr' as emissionRateUnits
from temp1 b1
inner join temp2 b2 USING (MOVESRunID,yearID,monthID,dayID,stateID,countyID,scc,modelYearID)
left join ##defaultdb##.nrscc s on (b1.scc=s.scc);

-- Drop intermediate tables and the primary indexes
drop table if exists hours;
drop table if exists temp1;
drop table if exists temp2;
drop table if exists units;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index1');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index1 does not exist.''', 'drop index index1 on movesoutput');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index104');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index104 does not exist.''', 'drop index index104 on movesoutput');
PREPARE stmt FROM @sqlstmt;																						
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesactivityoutput' and index_name = 'index10');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index10 does not exist.''', 'drop index index10 on movesactivityoutput');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

-- Revert 0s to NULLs
UPDATE movesoutput SET stateID = NULL WHERE stateID = 0;
UPDATE movesoutput SET countyID = NULL WHERE countyID = 0;
UPDATE movesactivityoutput SET stateID = NULL WHERE stateID = 0;
UPDATE movesactivityoutput SET countyID = NULL WHERE countyID = 0;

-- drop the rest of the indexes
set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index_state');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index_state does not exist.''', 'drop index index_state on movesoutput');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesactivityoutput' and index_name = 'index_state');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index_state does not exist.''', 'drop index index_state on movesactivityoutput');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index_county');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index_county does not exist.''', 'drop index index_county on movesoutput');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesactivityoutput' and index_name = 'index_county');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index_county does not exist.''', 'drop index index_county on movesactivityoutput');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;
