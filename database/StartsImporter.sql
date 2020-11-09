-- Author Wesley Faler
-- Author John Covey
-- Version 2018-03-20

drop procedure if exists spCheckStartsImporter;

BeginBlock
create procedure spCheckStartsImporter()
begin
	-- Scale 0 is national
	-- Scale 1 is single county
	-- Scale 2 is project domain
	declare scale int default ##scale##;

	declare howMany int default 0;
	declare startsCnt int default 0;
	declare startsPerDayCnt int default 0;
	declare startsPerDayPerVehicleCnt int default 0;
	
	-- startsAgeAdjustment
	set howMany=0;
	select count(*) into howMany from startsAgeAdjustment;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)	
		select distinct
			case
			  when ac.ageID is null then  concat('ERROR: startsAgeAdjustment with ageID ',s.ageID,' is unknown')
			  when sut.sourceTypeID is null then  concat('ERROR: startsAgeAdjustment with sourceTypeID ',s.sourceTypeID,' is unknown')
		    else null end as message
		from startsAgeAdjustment s
		left join ##defaultDatabase##.agecategory ac
		  on s.ageID = ac.ageID
		left join ##defaultDatabase##.sourceusetype sut
		  on s.sourceTypeID = sut.sourceTypeID
		  where ac.ageID is null or sut.sourceTypeID is null;

		insert into importTempMessages (message)
		select concat('ERROR: startsAgeAdjustment with Age ',ageID,', Source Type ',sourceTypeID,' has ageAdjustment < 0') as message
		from startsAgeAdjustment
		where ageAdjustment < 0;
		
		insert into importTempMessages (message)
		select concat('ERROR: startsAgeAdjustment with Age ',ageID,', Source Type ',sourceTypeID,' has null ageAdjustment') as message
		from startsAgeAdjustment
		where ageAdjustment is null;
		
	end if;
	
	-- startsHourFraction
	set howMany=0;
	select count(*) into howMany from startsHourFraction;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)	
		select distinct
			case
			  when d.dayID is null then  concat('ERROR: StartsHourFraction with dayID ',s.dayID,' is unknown')
			  when hoad.hourID is null then  concat('ERROR: StartsHourFraction with hourID ',s.hourID,' is unknown')
			  when sut.sourceTypeID is null then  concat('ERROR: StartsHourFraction with sourceTypeID ',s.sourceTypeID,' is unknown')
		    else null end as message
		from startsHourFraction s
		left join ##defaultDatabase##.dayofanyweek d
		  on s.dayID = d.dayID
		left join ##defaultDatabase##.hourofanyday hoad
		  on s.hourID = hoad.hourID
		left join ##defaultDatabase##.sourceusetype sut
		  on s.sourceTypeID = sut.sourceTypeID
		  where d.dayID is null or hoad.hourID is null or sut.sourceTypeID is null;
		
		-- if not all hours are selected, you don't need to import data for all of them
		-- therefore, only check to make sure it doesn't sum to MORE than 1
		insert into importTempMessages (message)
		select concat('ERROR: StartsHourFraction with Day ',dayID,', Source Type ',sourceTypeID,' allocation fraction sums to ',round(sum(allocationFraction),4)) as message
		from startsHourFraction
		group by dayID, sourceTypeID
		having round(sum(allocationFraction),4) > 1.0000;

		insert into importTempMessages (message)
		select concat('ERROR: StartsHourFraction with Day ',dayID,', Source Type ',sourceTypeID,', Hour ',hourID,' has allocationFraction < 0') as message
		from startsHourFraction
		where allocationFraction < 0;
		
		insert into importTempMessages (message)
		select concat('ERROR: StartsHourFraction with Day ',dayID,', Source Type ',sourceTypeID,', Hour ',hourID,' has null allocationFraction') as message
		from startsHourFraction
		where allocationFraction is null;
		
		-- if not all hours are selected, you don't need to import data for all of them, so this is just a warning
		insert into importTempMessages (message)
		select concat('Warning: StartsHourFraction with Source Type ',sourceTypeID,' and Day ',dayID,' has missing hours') as message
		  from (
			select shf.sourceTypeID,shf.dayID,count(distinct shf.hourID) hourIDCnt from startshourfraction shf
			join ##defaultDatabase##.hourday hd
			  on hd.dayID = shf.dayID
			  and hd.hourID = shf.hourID
			group by shf.sourceTypeID, shf.dayID) a
		where hourIDCnt <> 24;
		
		-- if all hours are imported, then do the check for < 1 (check for >1 always happens)
		insert into importTempMessages (message)
		select concat('ERROR: StartsHourFraction with Day ',shf.dayID,', Source Type ',shf.sourceTypeID,' allocation fraction sums to ',round(sum(allocationFraction),4),
		              ' even though all hours are imported') as message
		from startsHourFraction shf
			join ##defaultDatabase##.hourday hd
			  on hd.dayID = shf.dayID
			  and hd.hourID = shf.hourID
		group by shf.dayID, shf.sourceTypeID
		having round(sum(allocationFraction),4) < 1.0000 AND count(distinct shf.hourID) = 24;

	end if;

	-- StartsPerDayPerVehicle
	set howMany=0;
	select count(*) into howMany from startsPerDayPerVehicle;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select distinct
			case
			  when d.dayID is null then  concat('ERROR: StartsPerDayPerVehicle with dayID ',s.dayID,' is unknown')
			  when sut.sourceTypeID is null then  concat('ERROR: StartsPerDayPerVehicle with sourceTypeID ',s.sourceTypeID,' is unknown')
		    else null end as message
		from startsPerDayPerVehicle s
		left join ##defaultDatabase##.dayofanyweek d
		  on s.dayID = d.dayID
		left join ##defaultDatabase##.sourceusetype sut
		  on s.sourceTypeID = sut.sourceTypeID
		  where d.dayID is null or sut.sourceTypeID is null;
	  
		insert into importTempMessages (message)
		select concat('ERROR: StartsPerDayPerVehicle with Day ',dayID,', Source Type ',sourceTypeID,' has StartsPerDayPerVehicle < 0') as message
		from StartsPerDayPerVehicle
		where StartsPerDayPerVehicle < 0;

		insert into importTempMessages (message)
		select concat('ERROR: StartsPerDayPerVehicle with Day ',dayID,', Source Type ',sourceTypeID,' has null StartsPerDayPerVehicle') as message
		from StartsPerDayPerVehicle
		where StartsPerDayPerVehicle is null;
		
	end if;

	-- StartsPerDay
	set howMany=0;
	select count(*) into howMany from startsPerDay;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select distinct
			case
			  when d.dayID is null then  concat('ERROR: StartsPerDay with dayID ',s.dayID,' is unknown')
			  when sut.sourceTypeID is null then  concat('ERROR: StartsPerDay with sourceTypeID ',s.sourceTypeID,' is unknown')
		    else null end as message
		from startsPerDay s
		left join ##defaultDatabase##.dayofanyweek d
		  on s.dayID = d.dayID
		left join ##defaultDatabase##.sourceusetype sut
		  on s.sourceTypeID = sut.sourceTypeID
		  where d.dayID is null or sut.sourceTypeID is null;
	  
		insert into importTempMessages (message)
		select concat('ERROR: StartsPerDay with Day ',dayID,', Source Type ',sourceTypeID,' has StartsPerDay < 0') as message
		from StartsPerDay
		where StartsPerDay < 0;
	  
		insert into importTempMessages (message)
		select concat('ERROR: StartsPerDay with Day ',dayID,', Source Type ',sourceTypeID,' has null StartsPerDay') as message
		from StartsPerDay
		where StartsPerDay is null;
	end if;

	-- startsMonthAdjust
	set howMany=0;
	select count(*) into howMany from startsMonthAdjust;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select distinct
			case
			  when m.monthID is null then  concat('ERROR: StartsMonthAdjust with monthID ',s.monthID,' is unknown')
			  when sut.sourceTypeID is null then  concat('ERROR: StartsMonthAdjust with sourceTypeID ',s.sourceTypeID,' is unknown')
		    else null end as message
		from startsMonthAdjust s
		left join ##defaultDatabase##.monthofanyyear m
		  on s.monthID = m.monthID
		left join ##defaultDatabase##.sourceusetype sut
		  on s.sourceTypeID = sut.sourceTypeID
		  where m.monthID is null or sut.sourceTypeID is null;
	  
		insert into importTempMessages (message)
		select concat('ERROR: StartsMonthAdjust with Month ',monthID,', Source Type ',sourceTypeID,' has monthAdjustment < 0') as message
		from startsMonthAdjust
		where monthAdjustment < 0;
	  
		insert into importTempMessages (message)
		select concat('ERROR: StartsMonthAdjust with Month ',monthID,', Source Type ',sourceTypeID,' has null monthAdjustment') as message
		from startsMonthAdjust
		where monthAdjustment is null;
	end if;

	-- startsOpModeDistribution
	set howMany=0;
	select count(*) into howMany from startsOpModeDistribution;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select distinct
			case
			  when d.dayID is null then  concat('ERROR: StartsOpModeDistribution with dayID ',s.dayID,' is unknown')
			  when hoad.hourID is null then  concat('ERROR: StartsOpModeDistribution with hourID ',s.hourID,' is unknown')
			  when sut.sourceTypeID is null then  concat('ERROR: StartsOpModeDistribution with sourceTypeID ',s.sourceTypeID,' is unknown')
			  when ac.ageID is null then  concat('ERROR: StartsOpModeDistribution with ageID ',s.ageID,' is unknown')
			  when om.opModeID is null then  concat('ERROR: StartsOpModeDistribution with opModeID ',s.opModeID,' is unknown')
		    else null end as message
		from startsOpModeDistribution s
		left join ##defaultDatabase##.dayofanyweek d
		  on s.dayID = d.dayID
		left join ##defaultDatabase##.hourofanyday hoad
		  on s.hourID = hoad.hourID
		left join ##defaultDatabase##.sourceusetype sut
		  on s.sourceTypeID = sut.sourceTypeID
		left join ##defaultDatabase##.agecategory ac
		  on s.ageID = ac.ageID
		left join ##defaultDatabase##.operatingmode om
		  on s.opModeID = om.opModeID
		  where d.dayID is null or hoad.hourID is null or sut.sourceTypeID is null or ac.ageID is null or om.opModeID is null;
	  
		insert into importTempMessages (message)
		select concat('ERROR: StartsOpModeDistribution with Day ',dayId,', Hour ,',hourId,'Source Type ',sourceTypeID,', Age ',ageID,' op mode fraction is not 1.0 but instead ',round(sum(opModeFraction),4)) as message
		from startsOpModeDistribution
		group by dayId,hourId,sourceTypeID, ageID
		having round(sum(opModeFraction),4) <> 1.0000;
		
		insert into importTempMessages (message)
		select concat('ERROR: StartsOpModeDistribution with Day ',dayID,',Hour ',hourID,',Source Type ',sourceTypeID,',Age ',ageID,',Op Mode ',opModeID,' has value < 0') as message
		from startsOpModeDistribution
		where opModeFraction < 0;		

		insert into importTempMessages (message)
		select concat('ERROR: StartsOpModeDistribution with Day ',dayID,',Hour ',hourID,',Source Type ',sourceTypeID,',Age ',ageID,',Op Mode ',opModeID,' has null value') as message
		from startsOpModeDistribution
		where opModeFraction is null;		

		select count(*) into howMany from ##defaultDatabase##.operatingmode
		where minSoakTime is not null or maxSoakTime is not null;

		insert into importTempMessages (message)
		select concat('ERROR: StartsOpModeDistribution with Day ',dayID,', Hour ',hourID, ', Source Type ',sourceTypeID,', and Age ',ageID,' has missing Op Mode') as message
		  from (
			select somd.dayID, somd.hourID,somd.sourceTypeID,somd.ageID,count(distinct somd.opModeID) opModeIDCnt 
			from startsOpModeDistribution somd
			join ##defaultDatabase##.operatingmode om
			  on om.opModeID = somd.opModeID
      	   where om.minSoakTime is not null or om.maxSoakTime is not null
           group by somd.dayID, somd.hourID,somd.sourceTypeID,somd.ageID) a
		where opModeIDCnt <> howMany;

	end if;
	
	-- Starts
	set howMany=0;
	select count(*) into howMany from starts;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		insert into importTempMessages (message)
		select distinct
			case
			  when hd.hourDayID is null then  concat('ERROR: Starts with hourDayID ',s.hourDayID,' is unknown')
			  when m.monthID is null then  concat('ERROR: Starts with monthID ',s.monthID,' is unknown')
			  when ac.ageID is null then  concat('ERROR: Starts with ageID ',s.ageID,' is unknown')
			  when sut.sourceTypeID is null then  concat('ERROR: Starts with sourceTypeID ',s.sourceTypeID,' is unknown')
		    else null end as message
		from Starts s
		left join ##defaultDatabase##.hourDay hd
		  on s.hourDayID = hd.hourDayID
		left join ##defaultDatabase##.monthofanyyear m
		  on s.monthID = m.monthID
		left join ##defaultDatabase##.agecategory ac
		  on s.ageID = ac.ageID
		left join ##defaultDatabase##.sourceusetype sut
		  on s.sourceTypeID = sut.sourceTypeID
		  where hd.hourDayID is null or m.monthID is null or ac.ageID is null or sut.sourceTypeID is null;
	  
		insert into importTempMessages (message)
		select concat('ERROR: Starts with HourDay ',hourDayID,', Month ',monthID,', Year ',yearID,', Age ',ageID,
		              ', Zone ',zoneID,', Source Type ',sourceTypeID,' has Starts < 0') as message
		from Starts
		where Starts < 0;
	  
		insert into importTempMessages (message)
		select concat('ERROR: Starts with HourDay ',hourDayID,', Month ',monthID,', Year ',yearID,', Age ',ageID,
		              ', Zone ',zoneID,', Source Type ',sourceTypeID,' has null Starts') as message
		from Starts
		where Starts is null;
	end if;
	
	select count(*) into startsCnt from starts where isUserInput = 'Y';
	select count(*) into startsPerDayCnt from startsPerDay;
	select count(*) into startsPerDayPerVehicleCnt from startsPerDayPerVehicle;
	
	-- see stackoverflow.com/questions/33378732 for "dual" table
	if (startsCnt > 0 and startsPerDayCnt > 0 and startsPerDayPerVehicleCnt > 0) then
		insert into importTempMessages (message)
			select 'ERROR: All three of the Starts, StartsPerDay, and StartsPerDayPerVehicle tables have been imported. MOVES can only run with one of them.' from dual;
	elseif (startsCnt > 0 and startsPerDayPerVehicleCnt > 0) then
		insert into importTempMessages (message)
			select 'ERROR: The Starts and StartsPerDayPerVehicle tables have both been imported. MOVES can only run with one of them.' from dual;
	elseif (startsCnt > 0 and startsPerDayCnt > 0) then
		insert into importTempMessages (message)
			select 'ERROR: The Starts and StartsPerDay tables have both been imported. MOVES can only run with one of them.' from dual;
	elseif (startsPerDayCnt > 0 and startsPerDayPerVehicleCnt > 0) then
		insert into importTempMessages (message)
			select 'ERROR: The StartsPerDay and StartsPerDayPerVehicle tables have both been imported. MOVES can only run with one of them.' from dual;
	end if;

end
EndBlock

call spCheckStartsImporter();
drop procedure if exists spCheckStartsImporter;
