-- Adjust the distribution within the ExtendedIdleHours table using
-- user-supplied adjustments.
-- Author Wesley Faler
-- Version 2017-09-19

drop procedure if exists spAdjustExtendedIdle;

BeginBlock
create procedure spAdjustExtendedIdle()
begin
	declare targetZoneID int default ##zoneID##;
	declare targetYearID int default ##yearID##;
	declare activityZoneID int default ##activityZoneID##;
	declare howMany int default 0;

	-- hotellingHoursPerDay
	set howMany=0;
	select count(*) into howMany from hotellinghoursperday where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		alter table ExtendedIdleHours add column dayID smallint not null default 0;
		alter table ExtendedIdleHours add column hourID smallint not null default 0;

		drop table if exists newExtendedIdleHours_hhpd;
		drop table if exists defaultExtendedIdleHoursPerDay;

		create table newExtendedIdleHours_hhpd like ExtendedIdleHours;
		create table defaultExtendedIdleHoursPerDay (
			yearID int,
			zoneID int,
			dayID smallint,
			defaultExtendedIdleHours double -- this is units of hotelling hours in a typical day
		);

		update ExtendedIdleHours set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10)
		where zoneID = targetZoneID and yearID = targetYearID;
		
		-- calculate the defaults
		insert into defaultExtendedIdleHoursPerDay 
		select yearID,zoneID,dayID,sum(ExtendedIdleHours / noOfRealDays) 
		from ExtendedIdleHours
		join dayOfAnyWeek using (dayID)
		group by yearID,zoneID,dayID;
		
		
		-- we use the ratio between the defaults and user input to calcualte the new ExtendedIdleHours
		insert into newExtendedIdleHours_hhpd (sourceTypeID,yearID,monthID,dayID,hourID,hourDayID,zoneID,ageID,ExtendedIdleHours)
		-- units of ExtendedIdleHours : hours per portion of week
		-- units of (ExtendedIdleHoursperday / defaultExtendedIdleHours): hours per typical day / hours per typical day, month combination (implictly day*month)
		-- expression below becomes hours per potion of week * (1/ typical month) * months
		select sourceTypeID,yearID,monthID,dayID,hourID,hourID*10+dayID as hourDayID,eih.zoneID,ageID,
			(hotellingHoursPerDay / defaultExtendedIdleHours) * ExtendedIdleHours * 12 * opModeFraction as ExtendedIdleHours
			-- sum(case when opModeFraction>0 then (hotellingHoursPerDay / defaultExtendedIdleHours) * ExtendedIdleHours * 12 * opModeFraction else 0 end)
		from ExtendedIdleHours as eih
		join defaultExtendedIdleHoursPerDay using (yearID,zoneID,dayID)
		join hotellingHoursPerDay using (yearID,zoneID,dayID)
		join dayofanyweek using (dayID)
		join monthofanyyear using (monthID)
		join HotellingActivityDistribution had on (
			beginModelYearID <= yearID - ageID
			and endModelYearID >= yearID - ageID
			and opModeID = 200
			and had.zoneID = activityZoneID
		);
		
		-- replace into with the new data
		replace into ExtendedIdleHours select * from newExtendedIdleHours_hhpd;


		drop table newExtendedIdleHours_hhpd;
		drop table defaultExtendedIdleHoursPerDay;
		
		alter table ExtendedIdleHours drop column dayID;
		alter table ExtendedIdleHours drop column hourID;
	end if;


	-- hotellingHourFraction
	set howMany=0;
	select count(*) into howMany from hotellingHourFraction where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		alter table ExtendedIdleHours add column dayID smallint not null default 0;
		alter table ExtendedIdleHours add column hourID smallint not null default 0;

		drop table if exists newExtendedIdleHours_hhf;
		drop table if exists defaultHotellingHourFraction;

		create table newExtendedIdleHours_hhf like ExtendedIdleHours;
		-- alter table hotellingTemp drop primary key;

		update ExtendedIdleHours set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10);
		
		-- calculate the default hour fractions
		create table defaultHotellingHourFraction (
			zoneID int,
			dayID smallint,
			hourID smallint,
			defaultHourFraction double
		);
		insert into defaultHotellingHourFraction
		select zoneID,dayID,hourID,sum(ExtendedIdleHours) / total.total as defaultHourFraction
		from ExtendedIdleHours
		join (
			select zoneID,dayID,sum(ExtendedIdleHours) as total from ExtendedIdleHours
			group by zoneID,dayID
		) as total using (zoneID,dayID)
		group by zoneID,dayID,hourID;
		
		-- we use the ratio between the defaults and user input to calcualte the new ExtendedIdleHours
		insert into newExtendedIdleHours_hhf (sourceTypeID,yearID,monthID,dayID,hourID,hourDayID,zoneID,ageID,ExtendedIdleHours)
		-- becuase we are just scaling by new fractions, the ExtendedIdleHours units are preserved
		-- (hourFraction / defaultHourFraction) is unitless
		select sourceTypeID,yearID,monthID,dayID,hourID,hourID*10+dayID as hourDayID,zoneID,ageID, 
			ExtendedIdleHours * (hourFraction / defaultHourFraction) as ExtendedIdleHours
		from ExtendedIdleHours
		join defaultHotellingHourFraction using (zoneID,dayID,hourID)
		join hotellingHourFraction using (zoneID,dayID,hourID);

		-- replace into with the new values
		replace into ExtendedIdleHours select * from newExtendedIdleHours_hhf;
		
		drop table newExtendedIdleHours_hhf;
		drop table defaultHotellingHourFraction;

		alter table ExtendedIdleHours drop column dayID;
		alter table ExtendedIdleHours drop column hourID;
	end if;

	-- hotellingAgeFraction
	set howMany=0;
	select count(*) into howMany from hotellingAgeFraction where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		drop table if exists newExtendedIdleHours_had;
		drop table if exists defaultHotellingAgeFraction;
		
		alter table ExtendedIdleHours add column dayID smallint not null default 0;
		alter table ExtendedIdleHours add column hourID smallint not null default 0;
		update ExtendedIdleHours set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10);

		create table newExtendedIdleHours_had like ExtendedIdleHours;
		create table defaultHotellingAgeFraction (
			zoneID int,
			ageID smallint,
			defaultAgeFraction double
		);
		
		
		-- calculate the default age fraction
		insert into defaultHotellingAgeFraction
		select zoneID,ageID,sum(ExtendedIdleHours) / total.total as defaultAgeFraction
		from ExtendedIdleHours
		join (
			select zoneID,sum(ExtendedIdleHours) as total from ExtendedIdleHours
			group by zoneID
		) as total using (zoneID)
		group by zoneID,ageID;
		
		-- use the ratio of the new age fractions to the defaults to scale the hotelling horus
		insert into newExtendedIdleHours_had (sourceTypeID,yearID,monthID,dayID,hourID,hourDayID,zoneID,ageID,ExtendedIdleHours)
		-- becuase we are just scaling by new fractions, the ExtendedIdleHours units are preserved
		-- (ageFraction / defaultAgeFraction) is unitless
		select sourceTypeID,yearID,monthID,dayID,hourID,hourID*10+dayID as hourDayID,zoneID,ageID, 
			ExtendedIdleHours * (ageFraction / defaultAgeFraction) as ExtendedIdleHours
		from ExtendedIdleHours
		join defaultHotellingAgeFraction using (zoneID,ageID)
		join hotellingAgeFraction using (zoneID,ageID);
			
			
		-- replace into with the new data
		replace into ExtendedIdleHours select * from newExtendedIdleHours_had;
				
		drop table newExtendedIdleHours_had;
		drop table defaultHotellingAgeFraction;
		
		alter table ExtendedIdleHours drop column dayID;
		alter table ExtendedIdleHours drop column hourID;
	end if;

	
	-- hotellingMonthAdjust
	set howMany=0;
	select count(*) into howMany from hotellingMonthAdjust where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		alter table ExtendedIdleHours add column dayID smallint not null default 0;
		alter table ExtendedIdleHours add column hourID smallint not null default 0;
		update ExtendedIdleHours set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10);
		
		drop table if exists newExtendedIdleHours_hma;
		drop table if exists defaultHotellingMonthAdjust;

		create table newExtendedIdleHours_hma like ExtendedIdleHours;
		create table defaultHotellingMonthAdjust (
			zoneID int,
			monthID smallint,
			defaultMonthAdjustment double
		);
		
		-- calculate the default month adjustments
		insert into defaultHotellingMonthAdjust
		-- the month adjustment is the ratio between the month's average hours and the average across all months (aka the full year)
		select zoneID,monthID,avg(ExtendedIdleHours)/average.yearAverage as defaultMonthAdjustment
		from ExtendedIdleHours
		join (
			select zoneID,avg(ExtendedIdleHours) as yearAverage 
			from ExtendedIdleHours
			group by zoneID
		) as average
		using (zoneID)
		group by zoneID,monthID;
		
		-- use the ratio of the new month adjustments to the old ones to calcualte the new hotelling hours
		insert into newExtendedIdleHours_hma (sourceTypeID,yearID,monthID,dayID,hourID,hourDayID,zoneID,ageID,ExtendedIdleHours)
		-- becuase we are just scaling by new fractions, the ExtendedIdleHours units are preserved
		-- (monthAdjustment / defaultMonthAdjustment) is unitless
		select sourceTypeID,yearID,monthID,dayID,hourID,hourID*10+dayID as hourDayID,zoneID,ageID, 
			ExtendedIdleHours * (monthAdjustment / defaultMonthAdjustment) as ExtendedIdleHours
		from ExtendedIdleHours
		join defaultHotellingMonthAdjust using (zoneID,monthid)
		join hotellingmonthadjust using (zoneID,monthid);
		
		-- replace into with the new data
		replace into ExtendedIdleHours select * from newExtendedIdleHours_hma;
		
		drop table newExtendedIdleHours_hma;
		drop table defaultHotellingMonthAdjust;
		
		alter table ExtendedIdleHours drop column dayID;
		alter table ExtendedIdleHours drop column hourID;
	end if;
end
EndBlock

call spAdjustExtendedIdle();
drop procedure if exists spAdjustExtendedIdle;
