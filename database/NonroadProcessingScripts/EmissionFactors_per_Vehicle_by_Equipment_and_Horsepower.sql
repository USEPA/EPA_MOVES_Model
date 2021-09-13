-- Nonroad Post Processing Script (updated 6/23/2021):
-- Emission factors in grams per vehicle by equipment type and horsepower class
--  
-- MOVES-Nonroad Output Guidance:
--       SCC and HP class must be selected and present in the results.
--       This script will run faster if model year and engine tech
--       are not selected, and if there is only one sector, year,
--       month, and day in the output.
-- 
-- When prompted to save, specify one of the following file types: .xlsx, .xls, or .txt
-- The raw output of this script is also stored in the output database in a table called:
-- EmissionFactors_per_Vehicle_by_Equipment_and_Horsepower
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

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index2');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index2 on movesoutput ( scc )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index102');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index102 on movesoutput ( MOVESRunID,yearID,monthID,dayID,stateID,countyID,hpID,pollutantID,processID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesactivityoutput' and index_name = 'index10');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index10 on movesactivityoutput ( activitytypeid )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

-- Get time units
set @timeUnits := (select timeUnits from movesrun limit 1);

-- Get vehicle population
drop table if exists population;
create table population
select 
	MOVESRunID,
	yearID,
	monthID,
	dayID,
	stateID,
	countyID,
	SCC,
	modelYearID,
	engTechID,
	hpID,
	activity as population
from movesactivityoutput  
where activitytypeid = 6;

create index index1 on population (scc);

-- Set up unit conversions table
drop table if exists units;
create table units (fromUnit char(5), factor double, description text);
insert into units values 
('ton', 907185, 'From U.S. tons to grams'),
('lb', 453.592, 'From lbm to grams'),
('kg', 1000, 'From kg to grams'),
('g', 1, 'From grams to grams');

-- Get inventories by equipment type and hpID
drop table if exists temp1;
create table temp1
select 
	MOVESRunID,
	yearID,
	monthID,
	dayID,
	stateID,
	countyID,
	nrEquipTypeID,
	hpID,
	n.fuelTypeID,
    pollutantID,
    processID,
    units.factor * sum(emissionQuant) as emissionQuant
from movesoutput m
left join movesrun using (movesrunid)
left join units on (movesrun.massUnits = units.fromUnit)
left join ##defaultdb##.nrscc n using (scc)
group by MOVESRunID,yearID,monthID,dayID,stateID,countyID,nrEquipTypeID,hpID,n.fuelTypeID,pollutantID,processID;

create index index1 on temp1 (MOVESRunID,yearID,monthID,dayID,stateID,countyID,nrEquipTypeID,hpID,fuelTypeID);
create index index2 on temp1 (nrEquipTypeID);
create index index3 on temp1 (hpID);


-- Get population by equipment type and hpID
drop table if exists temp2;
create table temp2
select 
	MOVESRunID,
	yearID,
	monthID,
	dayID,
	stateID,
	countyID,
	nrEquipTypeID,
	hpID,
	n.fuelTypeID,
    sum(population) as population
from population
left join ##defaultdb##.nrscc n using (scc)
group by MOVESRunID,yearID,monthID,dayID,stateID,countyID,nrEquipTypeID,hpID,n.fuelTypeID;

create index index1 on temp2 (MOVESRunID,yearID,monthID,dayID,stateID,countyID,nrEquipTypeID,hpID,fuelTypeID);


-- Join temp1 and temp2 and calculate the emission rate for the resulting output table
drop table if exists EmissionFactors_per_Vehicle_by_Equipment_and_Horsepower;
create table EmissionFactors_per_Vehicle_by_Equipment_and_Horsepower
select
	b1.MOVESRunID,
	b1.yearID,
	b1.monthID,
	b1.dayID,
	b1.stateID,
	b1.countyID,
	e.description as equipDescription,
	b1.hpID,
	h.binName as hpBin,
	b1.fuelTypeID,
    b1.pollutantID,
    b1.processID,
	b1.emissionQuant,
    b2.population,
    IF(b2.population != 0, b1.emissionQuant / b2.population, NULL) as emissionRate,
    concat('g/vehicle per ',@timeUnits) as emissionRateUnits
from temp1 b1
inner join temp2 b2 USING (MOVESRunID,yearID,monthID,dayID,stateID,countyID,nrEquipTypeID,hpID,fuelTypeID)
left join ##defaultdb##.nrequipmenttype e on (b1.nrequiptypeid = e.nrequiptypeid)
left join ##defaultdb##.nrhprangebin h on (b1.hpID = h.NRHPRangeBinID);

-- Drop intermediate tables and the primary indexes
drop table if exists temp1;
drop table if exists temp2;
drop table if exists population;
drop table if exists units;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index1');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index1 does not exist.''', 'drop index index1 on movesoutput');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index2');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index2 does not exist.''', 'drop index index2 on movesoutput');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index101');
set @sqlstmt := if( @exist = 0, 'select ''INFO: index102 does not exist.''', 'drop index index102 on movesoutput');
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