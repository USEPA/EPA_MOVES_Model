-- Author Wesley Faler
-- Version 2016-10-04

-- Mark any years in SourceTypeAgeDistribution as base years in the Year table

drop table if exists tempNewYear;

create table if not exists tempNewYear (
  yearID smallint(6) not null default '0',
  primary key  (yearID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into tempNewYear (yearID)
select distinct yearID
from SourceTypeAgeDistribution;

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

-- Complain about any years outside of MOVES's range
insert into importTempMessages (message)
select distinct concat('ERROR: Year ',yearID,' is outside the range of 1990-2060 and cannot be used') as errorMessage
from SourceTypeAgeDistribution
where yearID < 1990 or yearID > 2060;

-- Complain about any ages outside of MOVES's range
insert into importTempMessages (message)
select 'ERROR: Negative AgeIDs are not allowed. Instead, include these vehicles in the ageID 0 fraction.' as errorMessage
from SourceTypeAgeDistribution
where ageID < 0
LIMIT 1;
insert into importTempMessages (message)
select distinct concat('ERROR: AgeID > 40 are not allowed. Instead, include these vehicles in the ageID 40 fraction.') as errorMessage
from SourceTypeAgeDistribution
where ageID > 40
LIMIT 1;

-- Ensure distributions sum to 1.0 for all sourceTypeID, yearID combinations.
drop table if exists tempNotUnity;

create table tempNotUnity
select sourceTypeID, yearID, sum(ageFraction) as sumAgeFraction
from SourceTypeAgeDistribution
group by sourceTypeID, yearID
having round(sum(ageFraction),4) <> 1.0000;

insert into importTempMessages (message)
select concat('ERROR: Source ',sourceTypeID,', year ',yearID,' ageFraction sum is not 1.0 but instead ',round(sumAgeFraction,4))
from tempNotUnity;

drop table if exists tempNotUnity;

-- Complain about any null values
insert into importTempMessages (message)
SELECT concat('ERROR: Found a NULL ageFraction value for sourceTypeID: ', sourceTypeID)
from SourceTypeAgeDistribution
where ageFraction IS NULL
LIMIT 1;