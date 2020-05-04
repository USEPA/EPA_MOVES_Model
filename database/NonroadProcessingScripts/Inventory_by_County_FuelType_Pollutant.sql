-- Nonroad Post Processing Script (updated 7/26/2018):
-- Emissions inventory by county, fuel type, and pollutant
--  
-- When prompted to save, specify one of the following file types: .xlsx, .xls, or .txt
-- The raw output of this script is also stored in the output database in a table called:
-- Inventory_by_County_FuelType_Pollutant

flush tables;

-- Set up indexing
set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index1');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index1 on movesoutput ( MOVESRunID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index3');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index3 on movesoutput ( MOVESRunID,yearID,monthID,dayID,stateID,countyID,sectorID,fueltypeID,fuelSubTypeID,pollutantID,processID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

-- Select inventory
drop table if exists Inventory_by_County_FuelType_Pollutant;
create table Inventory_by_County_FuelType_Pollutant
select 
	MOVESRunID,
    yearID,
    monthID,
	dayID,
	stateID,
    countyID,
    sectorID,
    fueltypeID,
	fuelSubTypeID,
    pollutantID,
    processID,
    sum(emissionQuant) as emissionQuant,
    timeunits,
    massunits
from movesoutput
left join movesrun USING (movesrunid)
group by MOVESRunID,yearID,monthID,dayID,stateID,countyID,sectorID,fueltypeID,fuelSubTypeID,pollutantID,processID;