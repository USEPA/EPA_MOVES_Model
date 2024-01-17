-- Version 2008-10-28

-- Ensure distributions sum to 1.0 for all sourceTypeID, roadTypeID, hourDayID combinations.
drop table if exists tempNotUnity;

create table tempNotUnity
select sourceTypeID, roadTypeID, hourDayID, sum(avgSpeedFraction) as sumAvgSpeedFraction
from avgSpeedDistribution
group by sourceTypeID, roadTypeID, hourDayID
having round(sum(avgSpeedFraction),4) <> 1.0000;

insert into importTempMessages (message)
select concat('ERROR: Source ',sourceTypeID,', road ',roadTypeID,', hour/day ',hourDayID,' avgSpeedFraction sum is not 1.0 but instead ',round(sumAvgSpeedFraction,4))
from tempNotUnity;

drop table if exists tempNotUnity;

-- Complain about any null values
insert into importTempMessages (message)
SELECT concat('ERROR: Found a NULL avgSpeedFraction value for sourceTypeID: ', sourceTypeID)
from avgSpeedDistribution
where avgSpeedFraction IS NULL
LIMIT 1;