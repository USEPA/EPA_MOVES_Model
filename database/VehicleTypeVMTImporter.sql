-- Author Wesley Faler
-- Version 2016-10-04

-- Mark any years in hpmsVTypeYear as base years in the Year table

drop table if exists tempNewYear;

create table if not exists tempNewYear (
  yearID smallint(6) not null default '0',
  primary key  (yearID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into tempNewYear (yearID)
select distinct yearID
from hpmsVTypeYear;

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

-- Set VMTGrowthFactor to 0 instead of NULL
update HPMSVTypeYear set VMTGrowthFactor=0 where VMTGrowthFactor is null;

-- Complain about any years outside of MOVES's range
insert into importTempMessages (message)
select distinct concat('ERROR: Year ',yearID,' in HPMSVTypeYear is outside the range of 1990-2060 and cannot be used') as errorMessage
from hpmsVTypeYear
where yearID < 1990 or yearID > 2060;

insert into importTempMessages (message)
select distinct concat('ERROR: Year ',yearID,' in HPMSVtypeDay is outside the range of 1990-2060 and cannot be used') as errorMessage
from HPMSVtypeDay
where yearID < 1990 or yearID > 2060;

insert into importTempMessages (message)
select distinct concat('ERROR: Year ',yearID,' in SourceTypeYearVMT is outside the range of 1990-2060 and cannot be used') as errorMessage
from SourceTypeYearVMT
where yearID < 1990 or yearID > 2060;

insert into importTempMessages (message)
select distinct concat('ERROR: Year ',yearID,' in SourceTypeDayVMT is outside the range of 1990-2060 and cannot be used') as errorMessage
from SourceTypeDayVMT
where yearID < 1990 or yearID > 2060;

-- complain about any null VMT values
insert into importTempMessages (message)
SELECT concat('ERROR: HPMSVTypeYear has a NULL HPMSBaseYearVMT value for HPMSVtypeID: ', HPMSVtypeID)
from HPMSVTypeYear
where HPMSBaseYearVMT IS NULL
LIMIT 1;

insert into importTempMessages (message)
SELECT concat('ERROR: HPMSVTypeDay has a NULL VMT value for HPMSVtypeID: ', HPMSVtypeID)
from HPMSVTypeDay
where VMT IS NULL
LIMIT 1;

insert into importTempMessages (message)
SELECT concat('ERROR: SourceTypeYearVMT has a NULL VMT value for sourceTypeID: ', sourceTypeID)
from SourceTypeYearVMT
where VMT IS NULL
LIMIT 1;

insert into importTempMessages (message)
SELECT concat('ERROR: SourceTypeDayVMT has a NULL VMT value for sourceTypeID: ', sourceTypeID)
from SourceTypeDayVMT
where VMT IS NULL
LIMIT 1;

-- MonthVMTFraction
-- Fill with 0's for entries that were not imported
insert ignore into monthVMTFraction (sourceTypeID, monthID, monthVMTFraction)
select sourceTypeID, monthID, 0.0
from ##defaultDatabase##.sourceUseType, ##defaultDatabase##.monthOfAnyYear
where (select count(*) from monthVMTFraction where monthVMTFraction > 0) > 0;

-- Make sure distributions don't sum to greater than 1
insert into importTempMessages (message)
select distinct concat('ERROR: Source type ',sourceTypeID,' monthVMTFraction is greater than 1.0000') as errorMessage
from monthVMTFraction
group by sourceTypeID
having round(sum(monthVMTFraction),4)>1.0000;

-- Check that the supplied values sum to 1 if all months are in the runspec
insert into importTempMessages (message)
select distinct concat('ERROR: Source type ',sourceTypeID,' monthVMTFraction does not sum to 1.0000') as errorMessage
from monthVMTFraction
where '##monthIDs##' = '1,2,3,4,5,6,7,8,9,10,11,12'
group by sourceTypeID
having round(sum(monthVMTFraction),4)<1.0000 and sum(monthVMTFraction)>0.0000;

-- Complain about any null values
insert into importTempMessages (message)
SELECT concat('ERROR: Found a NULL monthVMTFraction value for sourceTypeID: ', sourceTypeID)
from monthVMTFraction
where monthVMTFraction IS NULL
LIMIT 1;


-- DayVMTFraction
-- Fill with 0's for entries that were not imported
insert ignore into dayVMTFraction (sourceTypeID, monthID, roadTypeID, dayID, dayVMTFraction)
select sourceTypeID, monthID, roadTypeID, dayID, 0.0
from ##defaultDatabase##.sourceUseType, ##defaultDatabase##.monthOfAnyYear,
	##defaultDatabase##.roadType, ##defaultDatabase##.dayOfAnyWeek
where (select count(*) from dayVMTFraction where dayVMTFraction > 0) > 0;

-- Make sure distributions don't sum to greater than 1
insert into importTempMessages (message)
select distinct concat('ERROR: Source type ',sourceTypeID,', month ',monthID,', road type ',roadTypeID,' dayVMTFraction is greater than 1.0') as errorMessage
from dayVMTFraction
group by sourceTypeID, monthID, roadTypeID
having round(sum(dayVMTFraction),4)>1.0000;

-- Check that the supplied values sum to 1 if all days are in the runspec
insert into importTempMessages (message)
select distinct concat('ERROR: Source type ',sourceTypeID,', month ',monthID,', road type ',roadTypeID,' dayVMTFraction does not sum to 1.0000') as errorMessage
from dayVMTFraction
where '##dayIDs##' = '2,5'
group by sourceTypeID, monthID, roadTypeID
having round(sum(dayVMTFraction),4)<1.0000 and sum(dayVMTFraction)>0.0000;

-- Complain about any null values
insert into importTempMessages (message)
SELECT concat('ERROR: Found a NULL dayVMTFraction value for sourceTypeID: ', sourceTypeID)
from dayVMTFraction
where dayVMTFraction IS NULL
LIMIT 1;


-- HourVMTFraction
-- Fill with 0's for entries that were not imported
insert ignore into hourVMTFraction (sourceTypeID, roadTypeID, dayID, hourID, hourVMTFraction)
select sourceTypeID, roadTypeID, dayID, hourID, 0.0
from ##defaultDatabase##.sourceUseType, ##defaultDatabase##.roadType, ##defaultDatabase##.hourDay
where (select count(*) from hourVMTFraction where hourVMTFraction > 0) > 0;

-- Make sure distributions don't sum to greater than 1
insert into importTempMessages (message)
select distinct concat('ERROR: Source type ',sourceTypeID,', day ',dayID,', road type ',roadTypeID,' hourVMTFraction is greater than 1.0') as errorMessage
from hourVMTFraction
group by sourceTypeID, dayID, roadTypeID
having round(sum(hourVMTFraction),4)>1.0000;

-- Check that the supplied values sum to 1 if all hours are in the runspec
insert into importTempMessages (message)
select distinct concat('ERROR: Source type ',sourceTypeID,', day ',dayID,', road type ',roadTypeID,' hourVMTFraction does not sum to 1.0000') as errorMessage
from hourVMTFraction
where '##hourIDs##' = '1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24'
group by sourceTypeID, dayID, roadTypeID
having round(sum(hourVMTFraction),4)<1.0000 and sum(hourVMTFraction)>0.0000;

-- Complain about any null values
insert into importTempMessages (message)
SELECT concat('ERROR: Found a NULL hourVMTFraction value for sourceTypeID: ', sourceTypeID)
from hourVMTFraction
where hourVMTFraction IS NULL
LIMIT 1;
