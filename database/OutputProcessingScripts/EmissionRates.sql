-- This MySQL script produces an output table which reports your
-- onroad emission results in units of mass per distance.  This is done
-- by joining the activity table with the inventory output results.
-- Version 20091191 djb.
-- Updated 20150602 kjr.
-- The MySQL table produced is called: MovesRates
-- This script requires that users check the "Distance Traveled"
-- check box General Output panel of the MOVES graphical user
-- interface.  Users must also select
-- the Inventory calculation type in the Scale panel.
-- **************************************************************
-- Only onroad emission rates will be calculated. (No Nonroad)
-- The user *must* select Distance Traveled.
-- The user *must* select Inventory calculation type.
-- **************************************************************
--
--
--
--  create outputTemp   from movesOutput,
--  create activityTemp from movesActivityOutput,
--
--  update to zero 15 fields of outputTemp,
--  update to zero 15 fields of activityTemp,
--
--  create distanceTemp SUM from ActivityTemp,
--
--  insert SUM into ActivityTemp from distanceTemp (roadType = 0 )
--    "      "   "        "        "      "        (roadType = 1 )
--
--  add master key to outputTemp,
--  add master key to ActivityTemp,
--
--  join into movesRates from outputTemp,
--                            activityTemp,
--                            using master key,
--                            and activityType = 1
--                           (with rate calculation)
--
-- drop table outputTemp,
-- drop table activityTemp,
-- drop DistanceTemp.
--
--

FLUSH TABLES ;

-- Create the table to hold the calculation results.
DROP TABLE IF EXISTS MOVESRates;
CREATE TABLE `MOVESRates` (
  `MasterKey`   char(60)                default NULL,
  `MOVESRunID`  smallint(5) unsigned        NOT NULL,  -- mas key
  `iterationID` smallint(5) unsigned    default  '1',  -- mas key
  `yearID`      smallint(5) unsigned    default NULL,  -- mas key
  `monthID`     smallint(5) unsigned    default NULL,  -- mas key
  `dayID`       smallint(5) unsigned    default NULL,  -- mas key
  `hourID`      smallint(5) unsigned    default NULL,  -- mas key
  `stateID`     smallint(5) unsigned    default NULL,  -- mas key
  `countyID`    int(10)     unsigned    default NULL,  -- mas key
  `zoneID`      int(10)     unsigned    default NULL,  -- mas key
  `linkID`      int(10)     unsigned    default NULL,  -- mas key
  `pollutantID` smallint(5) unsigned    default NULL,
  `processID`      smallint(5) unsigned default NULL,
  `sourceTypeID`   smallint(5) unsigned default NULL,  -- mas key
  `regClassId`     smallint(5) unsigned default NULL,  -- mas key
  `fuelTypeID`     smallint(5) unsigned default NULL,  -- mas key
  `modelYearID`    smallint(5) unsigned default NULL,  -- mas key
  `roadTypeID`     smallint(5) unsigned default NULL,  -- mas key
  `SCC`            char(10)             default NULL,  -- mas key
  `emissionQuant`  double               default NULL,
  `activityTypeID` smallint(6)              NOT NULL,
  `activity`       double               default NULL,
  `emissionRate`   double               default NULL,
  `massUnits`      char(5)              default NULL,
  `distanceUnits`  char(5)              default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1
 ;

-- Check to see that there is activity output in the table.

-- Create copies of the results tables.
drop   table if exists outputtemp ;
create table outputtemp select * from movesoutput ;


drop table if exists activitytemp ;
create table activitytemp select * from movesactivityoutput;



-- Eliminate any NULL values. NULL values prevent joining of the tables.
update outputtemp   set movesrunid  =0 where isnull(movesrunid) ;
update outputtemp   set iterationID =0 where isnull(iterationID) ;
update outputtemp   set yearID      =0 where isnull(yearID) ;
update outputtemp   set monthID     =0 where isnull(monthID) ;
update outputtemp   set dayID       =0 where isnull(dayID) ;
update outputtemp   set hourID      =0 where isnull(hourID) ;
update outputtemp   set stateID     =0 where isnull(stateID) ;
update outputtemp   set countyID    =0 where isnull(countyID) ;
update outputtemp   set zoneID      =0 where isnull(zoneID) ;
update outputtemp   set linkID      =0 where isnull(linkID) ;
update outputtemp   set sourceTypeID=0 where isnull(sourceTypeID) ;
update outputtemp   set regClassId  =0 where isnull(regClassId  ) ;
update outputtemp   set fuelTypeID  =0 where isnull(fuelTypeID) ;
update outputtemp   set modelYearID =0 where isnull(modelYearID) ;
update outputtemp   set roadTypeID  =0 where isnull(roadTypeID) ;
update outputtemp   set SCC         =0 where isnull(SCC) ;

update activitytemp set movesrunid  =0 where isnull(movesrunid) ;
update activitytemp set iterationID =0 where isnull(iterationID) ;
update activitytemp set yearID      =0 where isnull(yearID) ;
update activitytemp set monthID     =0 where isnull(monthID) ;
update activitytemp set dayID       =0 where isnull(dayID) ;
update activitytemp set hourID      =0 where isnull(hourID) ;
update activitytemp set stateID     =0 where isnull(stateID) ;
update activitytemp set countyID    =0 where isnull(countyID) ;
update activitytemp set zoneID      =0 where isnull(zoneID) ;
update activitytemp set linkID      =0 where isnull(linkID) ;
update activitytemp set sourceTypeID=0 where isnull(sourceTypeID) ;
update activitytemp set regClassId  =0 where isnull(regClassId  ) ;
update activitytemp set fuelTypeID  =0 where isnull(fuelTypeID) ;
update activitytemp set modelYearID =0 where isnull(modelYearID) ;
update activitytemp set roadTypeID  =0 where isnull(roadTypeID) ;
update activitytemp set SCC         =0 where isnull(SCC) ;
update activitytemp set SCC         =0 where SCC="NOTHING" ;

-- Alter the SCC values to eliminate the text suffix.
update outputtemp   set scc=concat(mid(SCC,1,9),"0");
update activitytemp set scc=concat(mid(SCC,1,9),"0");

-- Create a table with the distance summed over road type.
drop table if exists distancetemp;
create table distancetemp
select
	a.MOVESRunID,
	a.iterationID,
	a.yearID,
	a.monthID,

	a.dayID,
	a.hourID,
	a.stateID,
	a.countyID,

	a.zoneID,
	a.linkID,
	a.sourceTypeID,
  a.regClassId,

	a.fuelTypeID,
	a.modelYearID,
	concat(mid(a.SCC,1,7),"000") as SCC,
	a.activityTypeID,

	sum(a.activity) as activitysum

from	activitytemp as a
group by
	a.MOVESRunID,
	a.iterationID,
	a.yearID,
	a.monthID,

	a.dayID,
	a.hourID,
	a.stateID,
	a.countyID,

	a.zoneID,
	a.linkID,
	a.sourceTypeID,
  a.regClassId,

	a.fuelTypeID,
	a.modelYearID,
	concat(mid(a.SCC,1,7),"000"),
	a.activityTypeID;


-- Set the distance for RoadTypeID=1 to be the distance sum.
-- SCC case: RoadTypeID=1 and SCC=SCC with 00 road type.
-- Records without SCC will also be added, but will not match
-- with the emission output records and will be ignored.


-- Updated for MOVES 2014:
--   (for user having selected SCC output):
--   include roadtypeId,
--   and used New SCC definition.

insert into activitytemp (
	MOVESRunID,
	iterationID,
	yearID,
	monthID,
	dayID,
	hourID,
	stateID,
	countyID,
	zoneID,
	linkID,
	sourceTypeID,
  regClassId,
	fuelTypeID,
	modelYearID,
	roadTypeID,
	SCC,
	activityTypeID,
	activity,
	activityMean,
	activitySigma )
select
	MOVESRunID,
	iterationID,
	yearID,
	monthID,
	dayID,
	hourID,
	stateID,
	countyID,
	zoneID,
	linkID,
	sourceTypeID,
  regClassId,
	fuelTypeID,
	modelYearID,
	1 as roadTypeID,                     -- for MOVES 2014 SCC now includes roadTypeId
	concat(mid(SCC,1,6),'0100') as SCC,  -- for MOVES 2014 (new) SCC
	activityTypeID,
	activitysum as activity,
	0.0 as activityMean,
	0.0 as activitySigma
from	distancetemp;


-- Set the distance for RoadTypeID=1 to be the distance sum.
-- Source type case: RoadTypeID=1 and SCC='00'.
-- Where SCC is not selected

insert into activitytemp (
	MOVESRunID,
	iterationID,
	yearID,
	monthID,
	dayID,
	hourID,
	stateID,
	countyID,
	zoneID,
	linkID,
	sourceTypeID,
  regClassId,
	fuelTypeID,
	modelYearID,
	roadTypeID,
	SCC,
	activityTypeID,
	activity,
	activityMean,
	activitySigma )
select
	MOVESRunID,
	iterationID,
	yearID,
	monthID,
	dayID,
	hourID,
	stateID,
	countyID,
	zoneID,
	linkID,
	sourceTypeID,
  regClassId,
	fuelTypeID,
	modelYearID,
	1 as roadTypeID,
	'00' as SCC,         -- MOVES 2014 uses '00' for no SCC usage
	activityTypeID,
	activitysum as activity,
	0.0 as activityMean,
	0.0 as activitySigma
from	distancetemp;


-- Add master keys to each table for joining.
alter table outputtemp add MasterKey char(60) default null ;
update outputtemp set MasterKey=concat_ws(",",
	MOVESRunID,
	iterationID,
	yearID,
	monthID,
	dayID,
	hourID,
	stateID,
	countyID,
	zoneID,
	linkID,
	sourceTypeID,
  regClassId,
	fuelTypeID,
	modelYearID,
	roadTypeID,
	mid(SCC,1,8) );



CREATE INDEX index1 ON outputtemp (MasterKey) ;

alter table activitytemp add MasterKey char(60) default null ;
update activitytemp set MasterKey=concat_ws(",",
	MOVESRunID,
	iterationID,
	yearID,
	monthID,
	dayID,
	hourID,
	stateID,
	countyID,
	zoneID,
	linkID,
	sourceTypeID,
  regClassId,
	fuelTypeID,
	modelYearID,
	roadTypeID,
	mid(SCC,1,8) );

CREATE INDEX index1 ON activitytemp (MasterKey) ;


-- Join the tables.
truncate movesrates;
insert into movesrates (
	MasterKey,
	MOVESRunID,
	iterationID,
	yearID,
	monthID,
	dayID,
	hourID,
	stateID,
	countyID,
	zoneID,
	linkID,
	pollutantID,
	processID,
	sourceTypeID,
  regClassId,
	fuelTypeID,
	modelYearID,
	roadTypeID,
	SCC,
	emissionQuant,
	activityTypeID,
	activity,
	emissionRate )
select
	a.MasterKey,
	a.MOVESRunID,
	a.iterationID,
	a.yearID,
	a.monthID,
	a.dayID,
	a.hourID,
	a.stateID,
	a.countyID,
	a.zoneID,
	a.linkID,
	a.pollutantID,
	a.processID,
	a.sourceTypeID,
  a.regClassId,
	a.fuelTypeID,
	a.modelYearID,
	a.roadTypeID,
	a.SCC,
	a.emissionQuant,
	b.activityTypeID,
	b.activity,
	(a.emissionquant/b.activity) as emissionRate
from
	outputtemp as a,
	activitytemp as b
where
	a.MasterKey = b.MasterKey
and b.activityTypeID = 1;




-- Eliminate the temporary tables.
drop table outputtemp ;
drop table activitytemp ;
drop table distancetemp ;

-- Add the units to the table.
update movesrates, movesrun set
	movesrates.distanceunits=movesrun.distanceunits,
	movesrates.massunits=movesrun.massunits
where
	movesrates.movesrunid=movesrun.movesrunid;

FLUSH TABLES;