-- Author Wesley Faler
-- Version 2016-10-04

-- Mark any years in SourceTypeYear as base years in the Year table

drop table if exists tempNewYear;

create table if not exists tempNewYear (
  yearID smallint(6) not null default '0',
  primary key  (yearID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into tempNewYear (yearID)
select distinct yearID
from SourceTypeYear;

-- Add 0 population entries for source types not imported
insert ignore into SourceTypeYear (yearID, sourceTypeID, salesGrowthFactor, sourceTypePopulation, migrationRate)
select yearID, sourceTypeID, 0, 0, 0
from tempNewYear,
##defaultDatabase##.sourceUseType;

drop table if exists tempYear;

create table if not exists tempYear (
  yearID smallint(6) not null default '0',
  isBaseYear char(1) default null,
  fuelYearID smallint(6) not null default '0',
  primary key  (yearID),
  key isBaseYear (isBaseYear)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

create table if not exists year (
  yearID smallint(6) not null default '0',
  isBaseYear char(1) default null,
  fuelYearID smallint(6) not null default '0',
  primary key  (yearID),
  key isBaseYear (isBaseYear)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into tempYear (yearID, isBaseYear, fuelYearID)
select y.yearID, 'Y' as isBaseYear, y.fuelYearID
from tempNewYear ny
inner join ##defaultDatabase##.year y on (y.yearID=ny.yearID);

-- insert ignore into year (yearID, isBaseYear, fuelYearID)
-- select yearID, isBaseYear, fuelYearID
-- from tempYear

update year, tempNewYear set year.isBaseYear='Y'
where year.yearID=tempNewYear.yearID;

drop table if exists tempYear;
drop table if exists tempNewYear;

-- Set salesGrowthFactor and migrationRate to 0 instead of NULL
update SourceTypeYear set salesGrowthFactor=0 where salesGrowthFactor is null;
update SourceTypeYear set migrationRate=0 where migrationRate is null;

-- Complain about any years outside of MOVES's range
insert into importTempMessages (message)
select distinct concat('ERROR: Year ',yearID,' is outside the range of 1990-2060 and cannot be used') as errorMessage
from SourceTypeYear
where yearID < 1990 or yearID > 2060;

-- Complain about any null VPOP values
insert into importTempMessages (message)
SELECT concat('ERROR: Missing sourceTypePopulation value for sourceTypeID: ', sourceTypeID)
from SourceTypeYear
where sourceTypePopulation IS NULL
LIMIT 1;
