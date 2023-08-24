-- Adjust the distribution within the HotellingHours table using
-- user-supplied adjustments.
-- Author Wesley Faler
-- Version 2017-09-19

drop procedure if exists spAdjustHotelling;

BeginBlock
create procedure spAdjustHotelling()
begin
	declare targetZoneID int default ##zoneID##;
	declare targetYearID int default ##yearID##;
	declare activityZoneID int default ##activityZoneID##;
	declare howMany int default 0;
	
	-- HotellingHoursPerDay
	set howMany=0;
	select count(*) into howMany from hotellingHoursPerDay where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		alter table HotellingHours add column dayID smallint not null default 0;
		alter table HotellingHours add column hourID smallint not null default 0;

		drop table if exists newHotellingHours_hhpd;
		drop table if exists defaultHotellingHoursPerDay;

		create table newHotellingHours_hhpd like hotellingHours;
		create table defaultHotellingHoursPerDay (
			yearID int,
			zoneID int,
			dayID smallint,
			defaultHotellingHours double -- this is units of hotelling hours in a typical day
		);

		update HotellingHours set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10)
		where zoneID = targetZoneID and yearID = targetYearID;
		
		-- calculate the defaults
		insert into defaultHotellingHoursPerDay 
		select yearID,zoneID,dayID,sum(hotellinghours / noOfRealDays) 
		from hotellinghours
		join dayOfAnyWeek using (dayID)
		group by yearID,zoneID,dayID;
		
		
		-- we use the ratio between the defaults and user input to calculate the new HotellingHours
		insert into newHotellingHours_hhpd (sourceTypeID,fuelTypeID,yearID,monthID,dayID,hourID,hourDayID,zoneID,ageID,hotellingHours)
		-- units of hotellingHours: hours per portion of week
		-- units of (hotellinghoursperday / defaultHotellingHours): hours per typical day / hours per typical day, month combination 
        --     When at annual preagg, the expression below becomes hours per portion of week since month is already aggregated away
        --         In this case, we do not need to multiply by 12
        --     When no preagg or preagg less than year, defaultHotellingHoursPerDay contains the sum over all months, and then we are 
        --     dividing by that sum over all months in our ratio, so we need to multiply by 12 in the end.
		--         Essentially, the expression below becomes hours per potion of week * (1/ typical month) * months
		select sourceTypeID,fuelTypeID,yearID,monthID,dayID,hourID,hourID*10+dayID as hourDayID,zoneID,ageID,
			   case when monthID = 0 then (hotellingHoursPerDay / defaultHotellingHours) * hotellingHours
                    else                  (hotellingHoursPerDay / defaultHotellingHours) * hotellingHours * 12 
               end as hotellingHours
		from hotellingHours
		join defaultHotellingHoursPerDay using (yearID,zoneID,dayID)
		join hotellinghoursperday using (yearID,zoneID,dayID)
		join dayofanyweek using (dayID)
		join monthofanyyear using (monthID);
		
		-- replace into with the new data
		replace into hotellingHours select * from newHotellingHours_hhpd;

		drop table newHotellingHours_hhpd;
		drop table defaultHotellingHoursPerDay;
		
		alter table HotellingHours drop column dayID;
		alter table HotellingHours drop column hourID;
	end if;


	-- hotellingHourFraction
	set howMany=0;
	select count(*) into howMany from hotellingHourFraction where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		alter table HotellingHours add column dayID smallint not null default 0;
		alter table HotellingHours add column hourID smallint not null default 0;

		drop table if exists newHotellingHours_hhf;
		drop table if exists defaultHotellingHourFraction;

		create table newHotellingHours_hhf like HotellingHours;
		-- alter table hotellingTemp drop primary key;

		update HotellingHours set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10);
		
		-- calculate the default hour fractions
		create table defaultHotellingHourFraction (
			zoneID int,
			dayID smallint,
			hourID smallint,
			defaultHourFraction double
		);
		insert into defaultHotellingHourFraction
		select zoneID,dayID,hourID,sum(hotellingHours) / total.total as defaultHourFraction
		from hotellingHours
		join (
			select zoneID,dayID,sum(hotellingHours) as total from hotellingHours
			group by zoneID,dayID
		) as total using (zoneID,dayID)
		group by zoneID,dayID,hourID;
		
		-- we use the ratio between the defaults and user input to calculate the new HotellingHours
		insert into newHotellingHours_hhf (sourceTypeID,fuelTypeID,yearID,monthID,dayID,hourID,hourDayID,zoneID,ageID,hotellingHours)
		-- because we are just scaling by new fractions, the hotellinghours units are preserved
		-- (hourFraction / defaultHourFraction) is unitless
		select sourceTypeID,fuelTypeID,yearID,monthID,dayID,hourID,hourID*10+dayID as hourDayID,zoneID,ageID, 
			hotellingHours * (hourFraction / defaultHourFraction) as hotellingHours
		from hotellingHours
		join defaultHotellingHourFraction using (zoneID,dayID,hourID)
		join hotellingHourFraction using (zoneID,dayID,hourID);

		-- replace into with the new values
		replace into hotellingHours select * from newHotellingHours_hhf;

		drop table newHotellingHours_hhf;
		drop table defaultHotellingHourFraction;

		alter table HotellingHours drop column dayID;
		alter table HotellingHours drop column hourID;
	end if;

	-- hotellingAgeFraction
	set howMany=0;
	select count(*) into howMany from hotellingAgeFraction where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		drop table if exists newHotellingHours_had;
		drop table if exists defaultHotellingAgeFraction;
		
		alter table HotellingHours add column dayID smallint not null default 0;
		alter table HotellingHours add column hourID smallint not null default 0;
		update HotellingHours set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10);

		create table newHotellingHours_had like HotellingHours;
		create table defaultHotellingAgeFraction (
			zoneID int,
			ageID smallint,
			defaultAgeFraction double
		);
		
		
		-- calculate the default age fraction
		insert into defaultHotellingAgeFraction
		select zoneID,ageID,sum(hotellingHours) / total.total as defaultAgeFraction
		from hotellingHours
		join (
			select zoneID,sum(hotellingHours) as total from hotellingHours
			group by zoneID
		) as total using (zoneID)
		group by zoneID,ageID;
		
		-- use the ratio of the new age fractions to the defaults to scale the hotelling horus
		insert into newHotellingHours_had (sourceTypeID,fuelTypeID,yearID,monthID,dayID,hourID,hourDayID,zoneID,ageID,hotellingHours)
		-- because we are just scaling by new fractions, the hotellinghours units are preserved
		-- (ageFraction / defaultAgeFraction) is unitless
		select sourceTypeID,fuelTypeID,yearID,monthID,dayID,hourID,hourID*10+dayID as hourDayID,zoneID,ageID, 
			hotellingHours * (ageFraction / defaultAgeFraction) as hotellingHours
		from hotellingHours
		join defaultHotellingAgeFraction using (zoneID,ageID)
		join hotellingAgeFraction using (zoneID,ageID);
			
			
		-- replace into with the new data
		replace into hotellingHours select * from newHotellingHours_had;
		
		-- drop table newHotellingHours_had;
		drop table defaultHotellingAgeFraction;
		
		alter table HotellingHours drop column dayID;
		alter table HotellingHours drop column hourID;
	end if;

	
	-- hotellingMonthAdjust
	set howMany=0;
	select count(*) into howMany from hotellingMonthAdjust where zoneID=targetZoneID;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		alter table HotellingHours add column dayID smallint not null default 0;
		alter table HotellingHours add column hourID smallint not null default 0;
		update HotellingHours set dayID=mod(hourDayID,10), hourID=floor(hourDayID/10);
		
		drop table if exists newHotellingHours_hma;
		drop table if exists defaultHotellingMonthAdjust;

		create table newHotellingHours_hma like HotellingHours;
		create table defaultHotellingMonthAdjust (
			zoneID int,
			monthID smallint,
			defaultMonthAdjustment double
		);
		
		-- calculate the default month adjustments
		insert into defaultHotellingMonthAdjust
		-- the month adjustment is the ratio between the month's average hours and the average across all months (aka the full year)
		select zoneID,monthID,avg(hotellingHours)/average.yearAverage as defaultMonthAdjustment
		from hotellingHours
		join (
			select zoneID,avg(hotellinghours) as yearAverage 
			from hotellingHours
			group by zoneID
		) as average
		using (zoneID)
		group by zoneID,monthID;
		
		-- use the ratio of the new month adjustments to the old ones to calculate the new hotelling hours
		insert into newHotellingHours_hma (sourceTypeID,fuelTypeID,yearID,monthID,dayID,hourID,hourDayID,zoneID,ageID,hotellingHours)
		-- because we are just scaling by new fractions, the hotellinghours units are preserved
		-- (monthAdjustment / defaultMonthAdjustment) is unitless
		select sourceTypeID,fuelTypeID,yearID,monthID,dayID,hourID,hourID*10+dayID as hourDayID,zoneID,ageID, 
			hotellingHours * (monthAdjustment / defaultMonthAdjustment) as hotellingHours
		from hotellingHours
		join defaultHotellingMonthAdjust using (zoneID,monthid)
		join hotellingmonthadjust using (zoneID,monthid);
		
		-- replace into with the new data
		replace into hotellingHours select * from newHotellingHours_hma;
		
		drop table newHotellingHours_hma;
		drop table defaultHotellingMonthAdjust;
		
		alter table HotellingHours drop column dayID;
		alter table HotellingHours drop column hourID;
	end if;
end
EndBlock

call spAdjustHotelling();
drop procedure if exists spAdjustHotelling;
