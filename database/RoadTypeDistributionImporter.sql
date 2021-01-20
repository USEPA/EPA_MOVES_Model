-- Version 2008-10-28

-- Ensure distributions sum to 1.0 for all sourceTypeID combinations. Added a check for nulls,
-- because otherwise it could try to evaluate NULL <> 1.0000, which would not result in any rows being identified.
drop table if exists tempNotUnity;

create table tempNotUnity
select sourceTypeID, sum(IFNULL(roadTypeVMTFraction, 0)) as sumRoadTypeVMTFraction
from roadTypeDistribution
group by sourceTypeID
having round(sum(IFNULL(roadTypeVMTFraction, 0)),4) <> 1.0000;

insert into importTempMessages (message)
select concat('ERROR: Source ',sourceTypeID,' roadTypeVMTFraction sum is not 1.0 but instead ',round(sumRoadTypeVMTFraction,4))
from tempNotUnity;

drop table if exists tempNotUnity;
