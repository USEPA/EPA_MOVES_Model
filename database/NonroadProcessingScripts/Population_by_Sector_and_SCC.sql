-- Nonroad Post Processing Script (updated 7/26/2018):
-- Inventory population by sector and SCC
-- 
-- When prompted to save, specify one of the following file types: .xlsx, .xls, or .txt
-- The raw output of this script is also stored in the output database in a table called:
-- Population_by_Sector_and_SCC

flush tables;

-- Set up indexing
set @exist := (select count(*) from information_schema.statistics where table_schema = DATABASE() and table_name = 'movesactivityoutput' and index_name = 'index2');
set @sqlstmt := if( @exist > 0, 'select ''INFO: Index already exists.''', 'create index index2 on movesactivityoutput ( MOVESRunID,yearID,monthID,dayID,countyID,fuelTypeID,sectorID,scc )');
PREPARE stmt FROM @sqlstmt;
EXECUTE stmt;

-- Select population information
drop table if exists Population_by_Sector_and_SCC;
create table Population_by_Sector_and_SCC
select 
	MOVESRunID,
    yearID,
    monthID,
	dayID,
    countyID,
    fuelTypeID,
    sectorID,
    scc,
    sum(activity) as population,
    timeunits,
    massunits
from movesactivityoutput
left join movesrun USING (movesrunid)
where activitytypeid = 6
group by MOVESRunID,yearID,monthID,dayID,countyID,fuelTypeID,sectorID,scc;