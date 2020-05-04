-- Adjust the distribution within the Starts table using
-- user-supplied adjustments by month, hour, sourcetype, and day.
-- Author Wesley Faler
-- Version 2013-12-12

drop procedure if exists spAdjustStarts;

BeginBlock
create procedure spAdjustStarts()
begin
	declare targetZoneID int default ##zoneID##;
	declare targetYearID int default ##yearID##;
	declare howMany int default 0;

	-- startsHourFraction
	set howMany=0;
	select count(*) into howMany from startsHourFraction where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		alter table Starts add column dayID smallint not null default 0;
		alter table Starts add column hourID smallint not null default 0;

		drop table if exists startsTemp;

		create table startsTemp like Starts;
		alter table startsTemp drop primary key;

		update Starts set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10);

		insert into startsTemp (hourDayID,hourID,dayID,monthID,yearID,ageID,zoneID,sourceTypeID,starts)
		select 0 as hourDayID,0 as hourID,dayID,monthID,yearID,ageID,zoneID,sourceTypeID,
			sum(starts) as starts
		from Starts
		where yearID = targetYearID and zoneID = targetZoneID
		group by monthID,yearID,ageID,zoneID,sourceTypeID,dayID
		order by null;

		delete from starts where yearID = targetYearID and zoneID = targetZoneID;

		insert into Starts (hourDayID,monthID,yearID,ageID,zoneID,sourceTypeID,starts)
		select (o.hourID*10+o.dayID) as hourDayID,s.monthID,s.yearID,s.ageID,s.zoneID,s.sourceTypeID,
			s.starts*o.allocationFraction as starts
		from startsTemp s
		inner join startsHourFraction o using (zoneID,dayID);

		drop table startsTemp;

		alter table Starts drop column dayID;
		alter table Starts drop column hourID;
	end if;

	-- startsSourceTypeFraction
	set howMany=0;
	select count(*) into howMany from startsSourceTypeFraction;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		drop table if exists startsTemp;

		create table startsTemp like Starts;
		alter table startsTemp drop primary key;

		insert into startsTemp (hourDayID,monthID,yearID,ageID,zoneID,sourceTypeID,starts)
		select hourDayID,monthID,yearID,ageID,zoneID,0 as sourceTypeID,
			sum(starts) as starts
		from Starts
		where yearID = targetYearID and zoneID = targetZoneID
		group by hourDayID,monthID,yearID,ageID,zoneID
		order by null;

		delete from starts where yearID = targetYearID and zoneID = targetZoneID;

		insert into Starts (hourDayID,monthID,yearID,ageID,zoneID,sourceTypeID,starts)
		select s.hourDayID,s.monthID,s.yearID,s.ageID,s.zoneID,o.sourceTypeID,
			s.starts*o.allocationFraction as starts
		from startsTemp s, startsSourceTypeFraction o;

		drop table startsTemp;
	end if;

	-- startsPerDay
	set howMany=0;
	select count(*) into howMany from startsPerDay where zoneID=targetZoneID and yearID=targetYearID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		alter table Starts add column dayID smallint not null default 0;
		alter table Starts add column hourID smallint not null default 0;

		drop table if exists startsTemp;

		create table startsTemp like startsPerDay;
		alter table startsTemp drop primary key;

		update startsPerDay, dayOfAnyWeek
		set startsPerDay = startsPerDay * noOfRealDays
		where startsPerDay.dayID = dayOfAnyWeek.dayID
		and startsPerDay.yearID = targetYearID and startsPerDay.zoneID = targetZoneID;

		update Starts set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10)
		where zoneID = targetZoneID and yearID = targetYearID;

		insert into startsTemp (yearID,zoneID,dayID,startsPerDay)
		select yearID,zoneID,dayID, sum(starts) as startsPerDay
		from Starts
		where yearID = targetYearID and zoneID = targetZoneID
		group by yearID,zoneID,dayID;

		drop table if exists startsTempRatio;

		create table startsTempRatio like startsPerDay;

		insert into startsTempRatio (yearID,zoneID,dayID,startsPerDay)
		select a.yearID,a.zoneID,a.dayID,
			case when a.startsPerDay > 0 then (b.startsPerDay/a.startsPerDay)
			else 0 end as startsPerDay
		from startsTemp a
		inner join startsPerDay b using (yearID,zoneID,dayID);

		update Starts, startsTempRatio
		set Starts.starts = Starts.starts * startsTempRatio.startsPerDay
		where Starts.yearID = startsTempRatio.yearID
		and Starts.zoneID = startsTempRatio.zoneID
		and Starts.dayID = startsTempRatio.dayID;

		drop table startsTemp;
		drop table startsTempRatio;

		alter table Starts drop column dayID;
		alter table Starts drop column hourID;
	end if;

	-- startsMonthAdjust
	update Starts, startsMonthAdjust
	set starts = starts * monthAdjustment
	where Starts.monthID = startsMonthAdjust.monthID
	and Starts.yearID = targetYearID and Starts.zoneID = targetZoneID;
end
EndBlock

call spAdjustStarts();
drop procedure if exists spAdjustStarts;
