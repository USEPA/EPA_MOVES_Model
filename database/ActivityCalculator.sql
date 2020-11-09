-- Version 2017-09-29
-- Author Wesley Faler

-- @algorithm
-- @owner Activity Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

##create.HourDay##;
TRUNCATE TABLE HourDay;

##create.link##;
TRUNCATE TABLE link;

##create.sourceUseType##;
TRUNCATE TABLE sourceUseType;

##create.runSpecSourceType##;
TRUNCATE TABLE runSpecSourceType;

##create.runSpecSourceFuelType##;
TRUNCATE TABLE runSpecSourceFuelType;

-- The sections are listed here according to Activty Type, not alphabetically
-- Section SourceHours
##create.SourceHours##;
TRUNCATE TABLE SourceHours;
-- End Section SourceHours

-- Section ExtendedIdleHours
##create.extendedIdleHours##;
TRUNCATE TABLE extendedIdleHours;
-- End Section ExtendedIdleHours

-- Section hotellingHours
##create.hotellingHours##;
TRUNCATE TABLE hotellingHours;

##create.hotellingActivityDistribution##;
TRUNCATE TABLE hotellingActivityDistribution;
-- End Section hotellingHours

-- Section SHO
##create.SHO##;
TRUNCATE TABLE SHO;
-- End Section SHO

-- Section ONI
##create.SHO##;
TRUNCATE TABLE SHO;
-- End Section ONI

-- Section SHP
##create.SHP##;
TRUNCATE TABLE SHP;
-- End Section SHP

-- Section Population

-- Section NonProjectDomain
##create.sourceTypeAgePopulation##;
TRUNCATE TABLE sourceTypeAgePopulation;

##create.fractionWithinHPMSVType##;
TRUNCATE TABLE fractionWithinHPMSVType;

##create.analysisYearVMT##;
TRUNCATE TABLE analysisYearVMT;

##create.roadTypeDistribution##;
TRUNCATE TABLE roadTypeDistribution;

##create.zoneRoadType##;
TRUNCATE TABLE zoneRoadType;
-- End Section NonProjectDomain

-- Section ProjectDomain
##create.offNetworkLink##;
TRUNCATE TABLE offNetworkLink;

##create.linkSourceTypeHour##;
TRUNCATE TABLE linkSourceTypeHour;

##create.sourceTypeAgeDistribution##;
TRUNCATE TABLE sourceTypeAgeDistribution;
-- End Section ProjectDomain

-- End Section Population

-- Section Starts
##create.Starts##;
TRUNCATE TABLE Starts;
-- End Section Starts

create table if not exists sourceTypeFuelFraction (
	sourceTypeID smallint not null,
	modelYearID smallint not null,
	fuelTypeID smallint not null,
	fuelFraction double not null,
	primary key (sourceTypeID, modelYearID, fuelTypeID),
	key (modelYearID, sourceTypeID, fuelTypeID),
	key (modelYearID, fuelTypeID, sourceTypeID)
);
truncate table sourceTypeFuelFraction;

-- Section WithRegClassID
##create.RegClassSourceTypeFraction##;
TRUNCATE TABLE RegClassSourceTypeFraction;
-- End Section WithRegClassID

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

SELECT *
INTO OUTFILE '##HourDay##'
FROM HourDay;

-- Select all links in the current zone. This is required population calculations in Project domain.
-- Link is further filtered where needed.
SELECT *
INTO OUTFILE '##link##'
FROM link
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

SELECT *
INTO OUTFILE '##sourceUseType##'
FROM sourceUseType;

SELECT *
INTO OUTFILE '##runSpecSourceType##'
FROM runSpecSourceType;

SELECT *
INTO OUTFILE '##runSpecSourceFuelType##'
FROM runSpecSourceFuelType;

-- Section SourceHours
SELECT SourceHours.* 
INTO OUTFILE '##SourceHours##'
FROM SourceHours
INNER JOIN RunSpecMonth USING (monthID)
WHERE yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##;
-- End Section SourceHours

-- Section ExtendedIdleHours
SELECT extendedIdleHours.*
INTO OUTFILE '##ExtendedIdleHours##'
FROM extendedIdleHours
WHERE yearID = ##context.year##
AND zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section ExtendedIdleHours

-- Section hotellingHours
SELECT hotellingHours.*
INTO OUTFILE '##hotellingHours##'
FROM hotellingHours
WHERE yearID = ##context.year##
AND zoneID = ##context.iterLocation.zoneRecordID##;

cache select *
into outfile '##hotellingActivityDistribution##'
from hotellingActivityDistribution
where opModeID <> 200
and beginModelYearID <= ##context.year##
and endModelYearID >= ##context.year## - 30
and zoneID = ##hotellingActivityZoneID##;
-- End Section hotellingHours

-- Section SHO
SELECT SHO.* 
INTO OUTFILE '##SHO##'
FROM SHO
INNER JOIN RunSpecMonth USING (monthID)
WHERE yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##;
-- End Section SHO

-- Section ONI
SELECT SHO.* 
INTO OUTFILE '##SHO##'
FROM SHO
INNER JOIN RunSpecMonth USING (monthID)
WHERE yearID = ##context.year##
AND linkID = ##context.iterLocation.linkRecordID##;
-- End Section ONI

-- Section SHP
SELECT SHP.* 
INTO OUTFILE '##SHP##'
FROM SHP
WHERE yearID = ##context.year##
AND zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section SHP

-- Section Population

-- Section NonProjectDomain
cache SELECT *
INTO OUTFILE '##sourceTypeAgePopulation##'
FROM sourceTypeAgePopulation
WHERE yearID = ##context.year##;

cache SELECT *
INTO OUTFILE '##fractionWithinHPMSVType##'
FROM fractionWithinHPMSVType
WHERE yearID = ##context.year##;

cache SELECT *
INTO OUTFILE '##analysisYearVMT##'
FROM analysisYearVMT
WHERE yearID = ##context.year##;

cache SELECT *
INTO OUTFILE '##roadTypeDistribution##'
FROM roadTypeDistribution;

cache select zoneID,
	roadTypeID,
	sum(SHOAllocFactor) as SHOAllocFactor
into outfile '##zoneRoadType##'
from zoneRoadType
where zoneID=##context.iterLocation.zoneRecordID##
group by roadTypeID;
-- End Section NonProjectDomain

-- Section ProjectDomain
cache SELECT *
INTO OUTFILE '##offNetworkLink##'
FROM offNetworkLink;

cache SELECT linkSourceTypeHour.*
INTO OUTFILE '##linkSourceTypeHour##'
FROM linkSourceTypeHour
INNER JOIN link on (link.linkID = linkSourceTypeHour.linkID)
WHERE zoneID = ##context.iterLocation.zoneRecordID##;

cache SELECT sourceTypeAgeDistribution.*
INTO OUTFILE '##sourceTypeAgeDistribution##'
FROM sourceTypeAgeDistribution
INNER JOIN RunSpecSourceType using (sourceTypeID)
WHERE yearID = ##context.year##;
-- End Section ProjectDomain

-- End Section Population

-- Section Starts
SELECT Starts.* INTO OUTFILE '##Starts##'
FROM Starts
WHERE yearID = ##context.year##
AND zoneID = ##context.iterLocation.zoneRecordID##;
-- End Section Starts

-- Section createSourceTypeFuelFraction
drop table if exists sourceTypeFuelFraction;
drop table if exists sourceTypeFuelFractionTemp;
drop table if exists sourceTypeFuelFractionTotal;

create table if not exists sourceTypeFuelFraction (
	sourceTypeID smallint not null,
	modelYearID smallint not null,
	fuelTypeID smallint not null,
	fuelFraction double not null,
	primary key (sourceTypeID, modelYearID, fuelTypeID),
	key (modelYearID, sourceTypeID, fuelTypeID),
	key (modelYearID, fuelTypeID, sourceTypeID)
);

create table sourceTypeFuelFractionTemp (
	sourceTypeModelYearID int not null,
	fuelTypeID smallint not null,
	tempFuelFraction double,
	primary key (sourceTypeModelYearID, fuelTypeID),
	key (fuelTypeID, sourceTypeModelYearID)
);

create table sourceTypeFuelFractionTotal (
	sourceTypeModelYearID int not null,
	tempTotal double,
	sourceTypeID smallint null,
	modelYearID smallint null,
	primary key (sourceTypeModelYearID)
);

-- Section UseSampleVehiclePopulation
insert into sourceTypeFuelFractionTemp (sourceTypeModelYearID, fuelTypeID, tempFuelFraction)
select sourceTypeModelYearID, fuelTypeID, sum(stmyFraction) as tempFuelFraction
from sampleVehiclePopulation
group by sourceTypeModelYearID, fuelTypeID
order by null;
-- End Section UseSampleVehiclePopulation

-- Section UseFuelUsageFraction
insert into sourceTypeFuelFractionTemp (sourceTypeModelYearID, fuelTypeID, tempFuelFraction)
select sourceTypeModelYearID, fuelSupplyFuelTypeID as fuelTypeID, 
	sum(stmyFraction*usageFraction) as tempFuelFraction
from sampleVehiclePopulation svp
inner join fuelUsageFraction fuf on (
	fuf.sourceBinFuelTypeID = svp.fuelTypeID
)
where fuf.countyID = ##context.iterLocation.countyRecordID##
and fuf.fuelYearID = ##context.fuelYearID##
and fuf.modelYearGroupID = 0
group by sourceTypeModelYearID, fuelSupplyFuelTypeID
order by null;
-- End Section UseFuelUsageFraction

insert into sourceTypeFuelFractionTotal (sourceTypeModelYearID, tempTotal)
select sourceTypeModelYearID, sum(stmyFraction) as tempTotal
from sampleVehiclePopulation
group by sourceTypeModelYearID
order by null;

update sourceTypeFuelFractionTotal, sourceTypeModelYear set sourceTypeFuelFractionTotal.sourceTypeID=sourceTypeModelYear.sourceTypeID,
	sourceTypeFuelFractionTotal.modelYearID=sourceTypeModelYear.modelYearID
where sourceTypeModelYear.sourceTypeModelYearID=sourceTypeFuelFractionTotal.sourceTypeModelYearID;

insert into sourceTypeFuelFraction (sourceTypeID, modelYearID, fuelTypeID, fuelFraction)
select t.sourceTypeID, t.modelYearID, r.fuelTypeID,
	case when tempTotal > 0 then tempFuelFraction / tempTotal
	else 0 end as fuelFraction
from sourceTypeFuelFractionTemp r
inner join sourceTypeFuelFractionTotal t on (t.sourceTypeModelYearID=r.sourceTypeModelYearID)
inner join runSpecSourceFuelType rs on (rs.sourceTypeID=t.sourceTypeID and rs.fuelTypeID=r.fuelTypeID);

drop table if exists sourceTypeFuelFractionTemp;
drop table if exists sourceTypeFuelFractionTotal;
-- End Section createSourceTypeFuelFraction

-- Section UseSampleVehiclePopulation
cache SELECT *
INTO OUTFILE '##sourceTypeFuelFraction##'
FROM sourceTypeFuelFraction;
-- End Section UseSampleVehiclePopulation

-- Section UseFuelUsageFraction
cache(countyID=##context.iterLocation.countyRecordID##,fuelYearID=##context.fuelYearID##) SELECT *
INTO OUTFILE '##sourceTypeFuelFraction##'
FROM sourceTypeFuelFraction;
-- End Section UseFuelUsageFraction

-- Section WithRegClassID
cache select *
into outfile '##RegClassSourceTypeFraction##'
from RegClassSourceTypeFraction
where modelYearID <= ##context.year##
and modelYearID >= ##context.year## - 30;
-- End Section WithRegClassID

-- End Section Extract Data

-- Section Local Data Removal
--TRUNCATE XXXXXX;
-- End Section Local Data Removal

-- Section Processing

-- 2, "sourcehours", "Source Hours"
-- 3, "extidle", "Extended Idle Hours"
-- 4, "sho", "Source Hours Operating"
-- 5, "shp", "Source Hours Parked"
-- 6, "population", "Population"
-- 7, "starts", "Starts"
-- 13, "hotellingAux", "Hotelling Diesel Aux"
-- 14, "hotellingElectric", "Hotelling Battery or AC"
-- 15, "hotellingOff", "Hotelling All Engines Off"

-- Section SourceHours
-- 2, "sourcehours", "Source Hours"

-- Section WithRegClassID
-- @algorithm sourceHours = sourceHours[sourceTypeID,hourDayID,monthID,yearID,ageID,linkID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		s.linkID, s.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		l.roadTypeID as roadTypeID,
		NULL as SCC,
		2 as activityTypeID,
		(sourceHours*stff.fuelFraction*stf.regClassFraction) as activity
from SourceHours s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join Link l on (l.linkID=s.linkID)
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
);
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		s.linkID, s.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		l.roadTypeID as roadTypeID,
		NULL as SCC,
		2 as activityTypeID,
		(sourceHours*stff.fuelFraction) as activity
from SourceHours s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join Link l on (l.linkID=s.linkID);
-- End Section NoRegClassID

-- End Section SourceHours

-- Section ExtendedIdleHours
-- 3, "extidle", "Extended Idle Hours"

-- Section WithRegClassID
-- @algorithm extendedIdleHours = extendedIdleHours[sourceTypeID,hourDayID,monthID,yearID,ageID,zoneID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		zoneID,
		##context.iterLocation.linkRecordID## linkID, s.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		##context.iterLocation.roadTypeRecordID## as roadTypeID,
		NULL as SCC,
		3 as activityTypeID,
		(extendedIdleHours*stff.fuelFraction*stf.regClassFraction) as activity
from extendedIdleHours s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
);
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		zoneID,
		##context.iterLocation.linkRecordID## linkID, s.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		##context.iterLocation.roadTypeRecordID## as roadTypeID,
		NULL as SCC,
		3 as activityTypeID,
		(extendedIdleHours*stff.fuelFraction) as activity
from extendedIdleHours s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID));
-- End Section NoRegClassID

-- End Section ExtendedIdleHours

-- Section SHO
-- 4, "sho", "Source Hours Operating"

-- Section WithRegClassID
-- @algorithm sho = sho[sourceTypeID,hourDayID,monthID,yearID,ageID,linkID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		s.linkID, s.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		l.roadTypeID as roadTypeID,
		NULL as SCC,
		4 as activityTypeID,
		(SHO*stff.fuelFraction*stf.regClassFraction) as activity
from SHO s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join Link l on (l.linkID=s.linkID)
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
);
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		s.linkID, s.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		l.roadTypeID as roadTypeID,
		NULL as SCC,
		4 as activityTypeID,
		(SHO*stff.fuelFraction) as activity
from SHO s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join Link l on (l.linkID=s.linkID);
-- End Section NoRegClassID

-- End Section SHO

-- Section SHP
-- 5, "shp", "Source Hours Parked"

-- Section WithRegClassID
-- @algorithm shp = shp[sourceTypeID,hourDayID,monthID,yearID,ageID,zoneID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		zoneID,
		##context.iterLocation.linkRecordID## linkID, s.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		##context.iterLocation.roadTypeRecordID## as roadTypeID,
		NULL as SCC,
		5 as activityTypeID,
		(SHP*stff.fuelFraction*stf.regClassFraction) as activity
from SHP s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
);
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		zoneID,
		##context.iterLocation.linkRecordID## linkID, s.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		##context.iterLocation.roadTypeRecordID## as roadTypeID,
		NULL as SCC,
		5 as activityTypeID,
		(SHP*stff.fuelFraction) as activity
from SHP s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID));
-- End Section NoRegClassID

-- End Section SHP

-- Section Population
-- 6, "population", "Population" (Zone level)

-- Section NonProjectDomain
drop table if exists fractionBySourceTypeTemp;

-- @algorithm sutFraction[sourceTypeID] = sum(roadTypeVMTFraction * shoAllocFactor)/sum(roadTypeVMTFraction)
-- @condition Non-Project domain
create table fractionBySourceTypeTemp
select sut.sourceTypeID, sum(rtd.roadTypeVMTFraction*zrt.shoAllocFactor)/sum(rtd.roadTypeVMTFraction) as sutFraction
from sourceUseType sut
inner join roadTypeDistribution rtd on (rtd.sourceTypeID=sut.sourceTypeID)
inner join zoneRoadType zrt on (zrt.roadTypeID=rtd.roadTypeID and zrt.zoneID=##context.iterLocation.zoneRecordID##)
group by sut.sourceTypeID
order by null;

drop table if exists sourceTypeTempPopulation;

-- @algorithm tempPopulation = sourceTypeAgePopulation[yearID,sourceTypeID,ageID] * sutFraction[sourceTypeID]
-- @condition Non-Project domain
create table sourceTypeTempPopulation
select t.sourceTypeID, stap.ageID, (population*sutFraction) as population, l.linkID
from fractionBySourceTypeTemp t
inner join sourceTypeAgePopulation stap on (stap.sourceTypeID=t.sourceTypeID)
inner join runSpecSourceType rsst on (rsst.sourceTypeID=stap.sourceTypeID)
inner join link l on (l.roadTypeID=1);

-- Section WithRegClassID
-- @algorithm population = tempPopulation*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
-- @condition Non-Project domain
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select ##context.year##, 0 as monthID, 0 as dayID, 0 as hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		s.linkID,
		s.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(##context.year##-s.ageID) as modelYearID,
		1 as roadTypeID,
		NULL as SCC,
		6 as activityTypeID,
		(s.population*stff.fuelFraction*stf.regClassFraction) as activity
from sourceTypeTempPopulation s
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(##context.year##-s.ageID))
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
);
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select ##context.year##, 0 as monthID, 0 as dayID, 0 as hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		s.linkID,
		s.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(##context.year##-s.ageID) as modelYearID,
		1 as roadTypeID,
		NULL as SCC,
		6 as activityTypeID,
		(s.population*stff.fuelFraction) as activity
from sourceTypeTempPopulation s
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(##context.year##-s.ageID));
-- End Section NoRegClassID

-- End Section NonProjectDomain

-- Section ProjectDomain

-- Section WithRegClassID
-- @algorithm population on off-network link = vehiclePopulation*ageFraction[yearID,sourceTypeID,ageID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
-- @condition Project domain
-- @condition Offnetwork link
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select ##context.year##, 0 as monthID, 0 as dayID, 0 as hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		l.linkID,
		onl.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(##context.year##-stad.ageID) as modelYearID,
		1 as roadTypeID,
		NULL as SCC,
		6 as activityTypeID,
		(onl.vehiclePopulation*stad.ageFraction*stff.fuelFraction*stf.regClassFraction) as activity
from link l
inner join offNetworkLink onl using (zoneID)
inner join sourceTypeAgeDistribution stad on (stad.sourceTypeID=onl.sourceTypeID)
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=stad.sourceTypeID and stff.modelYearID=(##context.year##-stad.ageID))
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
)
and l.roadTypeID = 1
where l.zoneID = ##context.iterLocation.zoneRecordID##;

-- @algorithm population on roadways = linkVolume[linkID]*sourceTypeHourFraction[linkID,sourceTypeID]*ageFraction[yearID,sourceTypeID,ageID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
-- @condition Project domain
-- @condition On roadways
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select ##context.year##, 0 as monthID, 0 as dayID, 0 as hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		l.linkID,
		lsth.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(##context.year##-stad.ageID) as modelYearID,
		roadTypeID,
		NULL as SCC,
		6 as activityTypeID,
		(l.linkVolume*lsth.sourceTypeHourFraction*stad.ageFraction*stff.fuelFraction*stf.regClassFraction) as activity
from link l
inner join linkSourceTypeHour lsth on (lsth.linkID=l.linkID)
inner join sourceTypeAgeDistribution stad on (stad.sourceTypeID=lsth.sourceTypeID)
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=stad.sourceTypeID and stff.modelYearID=(##context.year##-stad.ageID))
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
)
where l.roadTypeID<>1;
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select ##context.year##, 0 as monthID, 0 as dayID, 0 as hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		l.linkID,
		onl.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(##context.year##-stad.ageID) as modelYearID,
		1 as roadTypeID,
		NULL as SCC,
		6 as activityTypeID,
		(onl.vehiclePopulation*stad.ageFraction*stff.fuelFraction) as activity
from link l
inner join offNetworkLink onl using (zoneID)
inner join sourceTypeAgeDistribution stad on (stad.sourceTypeID=onl.sourceTypeID)
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=stad.sourceTypeID and stff.modelYearID=(##context.year##-stad.ageID))
where l.zoneID = ##context.iterLocation.zoneRecordID##
and l.roadTypeID = 1;

insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select ##context.year##, 0 as monthID, 0 as dayID, 0 as hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		l.linkID,
		lsth.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(##context.year##-stad.ageID) as modelYearID,
		roadTypeID,
		NULL as SCC,
		6 as activityTypeID,
		(l.linkVolume*lsth.sourceTypeHourFraction*stad.ageFraction*stff.fuelFraction) as activity
from link l
inner join linkSourceTypeHour lsth on (lsth.linkID=l.linkID)
inner join sourceTypeAgeDistribution stad on (stad.sourceTypeID=lsth.sourceTypeID)
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=stad.sourceTypeID and stff.modelYearID=(##context.year##-stad.ageID))
where l.roadTypeID<>1;
-- End Section NoRegClassID

-- End Section ProjectDomain

-- End Section Population

-- Section Starts
-- 7, "starts", "Starts"

-- Section WithRegClassID
-- @algorithm starts = starts[sourceTypeID,hourDayID,monthID,yearID,ageID,zoneID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		##context.iterLocation.linkRecordID## as linkID,
		s.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		##context.iterLocation.roadTypeRecordID## as roadTypeID,
		NULL as SCC,
		7 as activityTypeID,
		(starts*stff.fuelFraction*stf.regClassFraction) as activity
from Starts s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
);
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		##context.iterLocation.linkRecordID## as linkID,
		s.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		##context.iterLocation.roadTypeRecordID## as roadTypeID,
		NULL as SCC,
		7 as activityTypeID,
		(starts*stff.fuelFraction) as activity
from Starts s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID));
-- End Section NoRegClassID

-- End Section Starts

-- Section hotellingHours
-- 13, "hotellingAux", "Hotelling Diesel Aux"
-- 14, "hotellingElectric", "Hotelling Battery or AC"
-- 15, "hotellingOff", "Hotelling All Engines Off"

-- Section WithRegClassID
-- @algorithm hotellingAux hours = hotellingHours[sourceTypeID,hourDayID,monthID,yearID,ageID,zoneID]*opModeFraction[opModeID=201,modelYearID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID].
-- hotellingElectric hours = hotellingHours[sourceTypeID,hourDayID,monthID,yearID,ageID,zoneID]*opModeFraction[opModeID=203,modelYearID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID].
-- hotellingOff hours = hotellingHours[sourceTypeID,hourDayID,monthID,yearID,ageID,zoneID]*opModeFraction[opModeID=204,modelYearID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID].
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		s.zoneID,
		##context.iterLocation.linkRecordID## linkID, s.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		##context.iterLocation.roadTypeRecordID## as roadTypeID,
		NULL as SCC,
		case when opModeID=201 then 13
			when opModeID=203 then 14
			when opModeID=204 then 15
			else 8 end as activityTypeID,
		(hotellingHours*opModeFraction*stff.fuelFraction*stf.regClassFraction) as activity
from hotellingHours s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID)
inner join hotellingActivityDistribution ha on (
	ha.beginModelYearID <= stf.modelYearID
	and ha.endModelYearID >= stf.modelYearID
	and ha.opModeID in (201,203,204));
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		s.zoneID,
		##context.iterLocation.linkRecordID## linkID, s.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		##context.iterLocation.roadTypeRecordID## as roadTypeID,
		NULL as SCC,
		case when opModeID=201 then 13
			when opModeID=203 then 14
			when opModeID=204 then 15
			else 8 end as activityTypeID,
		(hotellingHours*opModeFraction*stff.fuelFraction) as activity
from hotellingHours s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join hotellingActivityDistribution ha on (
	ha.beginModelYearID <= stff.modelYearID
	and ha.endModelYearID >= stff.modelYearID
	and ha.opModeID in (201,203,204));
-- End Section NoRegClassID

-- End Section hotellingHours

-- Section ONI
-- 16, "shi", "Source Hours Idling" -- changed to sho (4)

-- Section WithRegClassID
-- @algorithm shi = sho[roadTypeID=1,sourceTypeID,hourDayID,monthID,yearID,ageID,linkID]*fuelFraction[sourceTypeID,modelYearID,fuelTypeID]*regClassFraction[fuelTypeID,modelYearID,sourceTypeID,regClassID]
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, regClassID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		s.linkID, s.sourceTypeID, stf.regClassID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		l.roadTypeID as roadTypeID,
		NULL as SCC,
		4 as activityTypeID,
		(SHO*stff.fuelFraction*stf.regClassFraction) as activity
from SHO s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join Link l on (l.linkID=s.linkID)
inner join RegClassSourceTypeFraction stf on (
	stf.sourceTypeID = stff.sourceTypeID
	and stf.fuelTypeID = stff.fuelTypeID
	and stf.modelYearID = stff.modelYearID
);
-- End Section WithRegClassID

-- Section NoRegClassID
insert into ##ActivityTable## (yearID, monthID, dayID, hourID, stateID, countyID,
		zoneID, linkID, sourceTypeID, fuelTypeID, modelYearID, roadTypeID, SCC,
		activityTypeID, activity)
select s.yearID, s.monthID, h.dayID, h.hourID,
		##context.iterLocation.stateRecordID## as stateID,
		##context.iterLocation.countyRecordID## as countyID,
		##context.iterLocation.zoneRecordID## as zoneID,
		s.linkID, s.sourceTypeID,
		stff.fuelTypeID as fuelTypeID,
		(s.yearID-s.ageID) as modelYearID,
		l.roadTypeID as roadTypeID,
		NULL as SCC,
		4 as activityTypeID,
		(SHO*stff.fuelFraction) as activity
from SHO s
inner join HourDay h on h.hourDayID=s.hourDayID
inner join sourceTypeFuelFraction stff on (stff.sourceTypeID=s.sourceTypeID and stff.modelYearID=(s.yearID-s.ageID))
inner join Link l on (l.linkID=s.linkID);
-- End Section NoRegClassID

-- End Section ONI

-- End Section Processing

-- Section Cleanup
drop table if exists sourceTypeFuelFraction;
drop table if exists fractionBySourceTypeTemp;
drop table if exists sourceTypeTempPopulation;
-- End Section Cleanup
