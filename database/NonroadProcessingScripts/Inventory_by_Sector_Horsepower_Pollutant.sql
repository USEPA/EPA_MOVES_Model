-- Nonroad Post Processing Script (updated 7/26/2018):
-- Emissions inventory by sector, horsepower, and pollutant
--  
-- MOVES-Nonroad Output Guidance:
--       SCC and HP class must be selected and present in the results.
-- 
-- When prompted to save, specify one of the following file types: .xlsx, .xls, or .txt
-- The raw output of this script is also stored in the output database in a table called:
-- Inventory_by_Sector_Horsepower_Pollutant

flush tables;

-- Set up indexing
set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index1');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index1 on movesoutput ( MOVESRunID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index2');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index2 on movesoutput ( scc )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesoutput' and index_name = 'index3');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index3 on movesoutput ( MOVESRunID,yearID,monthID,dayID,stateID,countyID,sectorID,fuelTypeID,fuelSubTypeID,pollutantID,processID )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;


-- Get inventories by equipment type and hpID
drop table if exists Inventory_by_Sector_Horsepower_Pollutant;
create table Inventory_by_Sector_Horsepower_Pollutant
select 
	MOVESRunID,
	yearID,
	monthID,
	dayID,
	stateID,
	countyID,
	sectorID,
	hpID,
	h.binName as hpBin,
	fuelTypeID,
	fuelSubTypeID,
    pollutantID,
    processID,
    sum(emissionQuant) as emissionQuant,
	timeUnits,
	massUnits
from movesoutput m
left join movesrun using (movesrunid)
left join ##defaultdb##.nrscc n using (scc)
left join ##defaultdb##.nrequipmenttype e using (nrequiptypeid)
left join ##defaultdb##.nrhprangebin h on (m.hpID = h.NRHPRangeBinID)
group by MOVESRunID,yearID,monthID,dayID,stateID,countyID,sectorID,hpID,fuelTypeID,fuelSubTypeID,pollutantID,processID;