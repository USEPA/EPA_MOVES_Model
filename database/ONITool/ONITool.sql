-- --------------------------------------------------------------------------------------
-- This script is run by the MOVES GUI to calculate default hours of Off-Network Idling
--     (ONI) activity for use with a Rates Mode run. It must be run with a fully populated
--     County Input Database.
-- The results are temporarily stored in a table called ONIToolOutput in the input database.
--     This table is managed by the MOVES GUI and deleted after the results are saved elsewhere.
-- The MOVES GUI replaces:
--     ##defaultdb## with the name of the default database
--     ##inputdb## with the name of the County Input Database
--     ##tempdb## with a temporary database name for all intermediate calculations
-- Messages to be displayed to the user are inserted into the oniTempMessages table, which
--     has one VARCHAR(1000) column. This table is managed by the MOVES GUI.
-- --------------------------------------------------------------------------------------

drop procedure if exists ONITool;

BeginBlock
create procedure ONITool()
ONITOOL_PROCEDURE: begin
	DECLARE howMany int default 0;

	-- ------------------------------
	-- START Ready to Calculate Block
	-- ------------------------------
	set howMany=0;
	select count(*) into howMany from ##inputdb##.RoadTypeDistribution;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		insert into ##tempdb##.oniTempMessages (message) values ('ERROR: RoadTypeDistribution must be provided.');
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.County;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		insert into ##tempdb##.oniTempMessages (message) values ('ERROR: County table must be provided.');
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.State;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		insert into ##tempdb##.oniTempMessages (message) values ('ERROR: State table must be provided.');
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.avgspeeddistribution;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		insert into ##tempdb##.oniTempMessages (message) values ('ERROR: avgspeeddistribution must be provided.');
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.sourcetypeyear;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		insert into ##tempdb##.oniTempMessages (message) values ('ERROR: sourcetypeyear must be provided.');
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.sourcetypeagedistribution;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		insert into ##tempdb##.oniTempMessages (message) values ('ERROR: sourcetypeagedistribution must be provided.');
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.hourvmtfraction;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		insert into ##tempdb##.oniTempMessages (message) values ('ERROR: hourvmtfraction must be provided.');
	end if;

	set howMany=0;
	SELECT SUM(c) into howMany FROM (select COUNT(*) AS c from ##inputdb##.sourcetypedayvmt
											  UNION select COUNT(*) AS c from ##inputdb##.sourcetypeyearvmt
										   UNION select COUNT(*) AS c from ##inputdb##.hpmsvtypeday
										   UNION select COUNT(*) AS c from ##inputdb##.hpmsvtypeyear) AS t1;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		insert into ##tempdb##.oniTempMessages (message) values ('ERROR: VMT input must be provided via sourcetypedayvmt, sourcetypeyearvmt, hpmsvtypeday, or hpmsvtypeyear.');
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.sourcetypeyearvmt;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		set howMany=0;
		select count(*) into howMany from ##inputdb##.monthvmtfraction;
		set howMany=ifnull(howMany,0);
		if(howMany = 0) then
			insert into ##tempdb##.oniTempMessages (message) values ('ERROR: monthvmtfraction must be provided when using sourcetypeyearvmt.');
		end if;
		set howMany=0;
		select count(*) into howMany from ##inputdb##.dayvmtfraction;
		set howMany=ifnull(howMany,0);
		if(howMany = 0) then
			insert into ##tempdb##.oniTempMessages (message) values ('ERROR: dayvmtfraction must be provided when using sourcetypeyearvmt.');
		end if;
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.hpmsvtypeyear;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		set howMany=0;
		select count(*) into howMany from ##inputdb##.monthvmtfraction;
		set howMany=ifnull(howMany,0);
		if(howMany = 0) then
			insert into ##tempdb##.oniTempMessages (message) values ('ERROR: monthvmtfraction must be provided when using hpmsvtypeyear.');
		end if;
		set howMany=0;
		select count(*) into howMany from ##inputdb##.dayvmtfraction;
		set howMany=ifnull(howMany,0);
		if(howMany = 0) then
			insert into ##tempdb##.oniTempMessages (message) values ('ERROR: dayvmtfraction must be provided when using hpmsvtypeyear.');
		end if;
	end if;

	set howMany=0;
	select count(*) into howMany from ##inputdb##.idlemodelyeargrouping;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		set howMany=0;
		select count(*) into howMany from ##inputdb##.totalidlefraction;
		set howMany=ifnull(howMany,0);
		if(howMany > 0) then
			insert into ##tempdb##.oniTempMessages (message) values ('ERROR: cannot use both totalidlefraction and idlemodelyeargrouping (choose one or the other).');
		end if;
	end if;
	
	-- Exit this stored procedure if there are any error messages at this point
	set howMany=0;
	select count(*) into howMany from ##tempdb##.oniTempMessages WHERE message like '%ERROR%';
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		LEAVE ONITOOL_PROCEDURE;
	end if;
	-- ----------------------------
	-- END Ready to Calculate Block
	-- ----------------------------

	-- ---------------------------------
	-- START Get TotalIdleFraction block (from /database/AdjustTotalIdleFraction.sql)
	-- ---------------------------------
	
	-- Start with user TotalIdleFractions
	CREATE TABLE IF NOT EXISTS ##tempdb##.totalidlefraction like ##inputdb##.totalidlefraction;
	INSERT INTO ##tempdb##.totalidlefraction SELECT * from ##inputdb##.totalidlefraction;
	
	-- Grab the default TotalIdleFractions if the user did not enter their own
	set howMany=0;
	select count(*) into howMany from ##tempdb##.totalidlefraction;
	set howMany=ifnull(howMany,0);
	if(howMany = 0) then
		INSERT INTO ##tempdb##.totalidlefraction (sourceTypeID, minModelYearID, maxModelYearID, monthID, dayID, idleRegionID, countyTypeID, totalidlefraction)
			SELECT sourceTypeID, minModelYearID, maxModelYearID, monthID, dayID, idleRegionID, countyTypeID, totalIdleFraction
			FROM ##inputdb##.county
			JOIN ##inputdb##.state
			JOIN ##defaultdb##.totalidlefraction USING (countyTypeID, idleRegionID);
	END if;
	
	-- Apply shaping tables if the user supplied them
	set howMany=0;
	select count(*) into howMany from ##inputdb##.idleModelYearGrouping;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then		
		-- eliminate the default data
		truncate table ##tempdb##.totalidlefraction;
		
		-- Populate totalIdleFraction from idleModelYearGrouping
		insert into ##tempdb##.totalIdleFraction (idleRegionID,countyTypeID,sourceTypeID,monthID,dayID,minModelYearID,maxModelYearID, totalIdleFraction)
		select distinct st.idleRegionID,c.countyTypeID,imyg.sourceTypeID, m.monthID, d.dayID, imyg.minModelYearID, imyg.maxModelYearID, imyg.totalIdleFraction
		from ##inputdb##.idleModelYearGrouping imyg
		join ##inputdb##.county c
		join ##inputdb##.state st
		join ##defaultdb##.monthofanyyear m
		join ##defaultdb##.dayofanyweek d;
		
		-- apply idleMonthAdjust
		update ##tempdb##.totalIdleFraction
		inner join ##inputdb##.idleMonthAdjust using (sourceTypeID, monthID)
		set totalIdleFraction = totalIdleFraction * idleMonthAdjust;

		-- apply idleDayAdjust
		update ##tempdb##.totalIdleFraction
		inner join ##inputdb##.idleDayAdjust using (sourceTypeID, dayID)
		set totalIdleFraction = totalIdleFraction * idleDayAdjust;
	end if;
	-- ---------------------------------
	-- END Get TotalIdleFraction block
	-- ---------------------------------

	-- -----------------------------------------
	-- START TotalActivityGenerator Calculations (from /gov/otaq/moves/master/implementation/ghg/TotalActivityGenerator.java)
	-- -----------------------------------------

	-- Create all intermediate tables
	CREATE TABLE IF NOT EXISTS ##tempdb##.SHOByAgeRoadwayHour (
		yearID         SMALLINT NOT NULL,
		roadTypeID     SMALLINT NOT NULL,
		sourceTypeID   SMALLINT NOT NULL,
		ageID          SMALLINT NOT NULL,
		monthID        SMALLINT NOT NULL,
		dayID          SMALLINT NOT NULL,
		hourID         SMALLINT NOT NULL,
		hourDayID      SMALLINT NOT NULL DEFAULT 0,
		SHO            DOUBLE NOT NULL,
		VMT            DOUBLE NOT NULL,
		UNIQUE INDEX XPKSHOByAgeRoadwayHour (yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, hourID));
	TRUNCATE ##tempdb##.SHOByAgeRoadwayHour;

	CREATE TABLE IF NOT EXISTS ##tempdb##.StartsByAgeHour (
		yearID         SMALLINT NOT NULL,
		sourceTypeID   SMALLINT NOT NULL,
		ageID          SMALLINT NOT NULL,
		monthID        SMALLINT NOT NULL,
		dayID          SMALLINT NOT NULL,
		hourID         SMALLINT NOT NULL,
		starts         DOUBLE NOT NULL,
		UNIQUE INDEX XPKStartsByAgeHour (yearID, sourceTypeID, ageID, monthID, dayID, hourID));
	TRUNCATE ##tempdb##.StartsByAgeHour;

	CREATE TABLE IF NOT EXISTS ##tempdb##.VMTByAgeRoadwayHour (
		yearID        SMALLINT NOT NULL,
		roadTypeID    SMALLINT NOT NULL,
		sourceTypeID  SMALLINT NOT NULL,
		ageID         SMALLINT NOT NULL,
		monthID       SMALLINT NOT NULL,
		dayID         SMALLINT NOT NULL,
		hourID        SMALLINT NOT NULL,
		VMT           DOUBLE NOT NULL,
		hourDayID     SMALLINT NOT NULL DEFAULT 0,
		UNIQUE INDEX XPKVMTByAgeRoadwayHour(yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, hourID));
	TRUNCATE ##tempdb##.VMTByAgeRoadwayHour;

	create table if not exists ##tempdb##.vmtByMYRoadHourFraction (
		yearID smallint not null,
		roadTypeID smallint not null,
		sourceTypeID smallint not null,
		modelYearID smallint not null,
		monthID smallint not null,
		dayID smallint not null,
		hourID smallint not null,
		hourDayID smallint not null,
		vmtFraction double,
		unique key (yearID, roadTypeID, sourceTypeID, modelYearID, monthID, hourID, dayID),
		unique key (yearID, roadTypeID, sourceTypeID, modelYearID, monthID, hourDayID));
	TRUNCATE ##tempdb##.vmtByMYRoadHourFraction;

	CREATE TABLE IF NOT EXISTS ##tempdb##.HPMSVTypePopulation (
		yearID       SMALLINT NOT NULL,
		HPMSVTypeID  SMALLINT NOT NULL,
		population   FLOAT NOT NULL,
		UNIQUE INDEX XPKHPMSVTypePopulation(yearID, HPMSVTypeID));
	TRUNCATE ##tempdb##.HPMSVTypePopulation;

	CREATE TABLE IF NOT EXISTS ##tempdb##.FractionWithinHPMSVType (
		yearID       SMALLINT NOT NULL,
		sourceTypeID SMALLINT NOT NULL,
		ageID        SMALLINT NOT NULL,
		fraction     FLOAT NOT NULL,
		UNIQUE INDEX XPKFractionWithinHPMSVType (yearID, sourceTypeID, ageID));
	TRUNCATE ##tempdb##.FractionWithinHPMSVType;

	CREATE TABLE IF NOT EXISTS ##tempdb##.HPMSTravelFraction (
		yearID      SMALLINT NOT NULL,
		HPMSVTypeID SMALLINT NOT NULL,
		fraction    FLOAT NOT NULL,
		UNIQUE INDEX XPKHPMSTravelFraction (yearID, HPMSVTypeID));
	TRUNCATE ##tempdb##.HPMSTravelFraction;

	CREATE TABLE IF NOT EXISTS ##tempdb##.TravelFraction (
		yearID        SMALLINT NOT NULL,
		sourceTypeID  SMALLINT NOT NULL,
		ageID         SMALLINT NOT NULL,
		fraction      FLOAT NOT NULL,
		UNIQUE INDEX XPKTravelFraction(yearID, sourceTypeID, ageID));
	TRUNCATE ##tempdb##.TravelFraction;

	CREATE TABLE IF NOT EXISTS ##tempdb##.AnnualVMTByAgeRoadway (
		yearID        SMALLINT NOT NULL,
		roadTypeID    SMALLINT NOT NULL,
		sourceTypeID  SMALLINT NOT NULL,
		ageID         SMALLINT NOT NULL,
		VMT           FLOAT NOT NULL,
		UNIQUE INDEX XPKAnnualVMTByAgeRoadway(yearID, roadTypeID, sourceTypeID, ageID));
	TRUNCATE ##tempdb##.AnnualVMTByAgeRoadway;

	CREATE TABLE IF NOT EXISTS ##tempdb##.AverageSpeed (
		roadTypeID    SMALLINT NOT NULL,
		sourceTypeID  SMALLINT NOT NULL,
		dayID         SMALLINT NOT NULL,
		hourID        SMALLINT NOT NULL,
		averageSpeed  FLOAT NOT NULL,
		UNIQUE INDEX XPKAverageSpeed (roadTypeID, sourceTypeID, dayID, hourID));
	TRUNCATE ##tempdb##.AverageSpeed;

	CREATE TABLE IF NOT EXISTS ##tempdb##.SHOByAgeDay (
		yearID         SMALLINT NOT NULL,
		sourceTypeID   SMALLINT NOT NULL,
		ageID          SMALLINT NOT NULL,
		monthID        SMALLINT NOT NULL,
		dayID          SMALLINT NOT NULL,
		SHO            DOUBLE NOT NULL,
		VMT            DOUBLE NOT NULL,
		UNIQUE INDEX XPKSHOByAgeDay(yearID, sourceTypeID, ageID, monthID, dayID));
	TRUNCATE ##tempdb##.SHOByAgeDay;
		
	CREATE TABLE IF NOT EXISTS ##tempdb##.AnalysisYearVMT (
		yearID      SMALLINT NOT NULL,
		HPMSVTypeID SMALLINT NOT NULL,
		VMT         FLOAT NOT NULL,
		UNIQUE INDEX XPKAnalysisYearVMT (yearID, HPMSVTypeID));
	TRUNCATE ##tempdb##.AnalysisYearVMT;

	CREATE TABLE IF NOT EXISTS ##tempdb##.SourceTypeAgePopulation (
		yearID         SMALLINT NOT NULL,
		sourceTypeID   SMALLINT NOT NULL,
		ageID          SMALLINT NOT NULL,
		population     FLOAT NOT NULL,
		UNIQUE INDEX XPKSourceTypeAgePopulation (yearID, sourceTypeID, ageID));
	TRUNCATE ##tempdb##.SourceTypeAgePopulation;

	create table if not exists ##tempdb##.vmtByMYRoadHourSummary (
		yearID smallint not null,
		roadTypeID smallint not null,
		sourceTypeID smallint not null,
		monthID smallint not null,
		dayID smallint not null,
		hourID smallint not null,
		hourDayID smallint not null,
		totalVMT double,
		unique key (yearID, roadTypeID, sourceTypeID, monthID, hourID, dayID),
		unique key (yearID, roadTypeID, sourceTypeID, monthID, hourDayID));
	TRUNCATE ##tempdb##.vmtByMYRoadHourSummary;

	create table IF NOT EXISTS ##tempdb##.ZoneRoadTypeLinkTemp (
		roadTypeID smallint not null,
		linkID int(11) not null,
		SHOAllocFactor double,
		unique index XPKZoneRoadTypeLinkTemp (roadTypeID, linkID));
	TRUNCATE ##tempdb##.ZoneRoadTypeLinkTemp;
	
	create table if not exists ##tempdb##.drivingIdleFraction (
		hourDayID smallint(6),
		yearID smallint(6),
		roadTypeID smallint(6),
		sourceTypeID smallint(6),
		drivingIdleFraction double,
		primary key (hourDayID,yearID,roadTypeID,sourceTypeID));
	TRUNCATE ##tempdb##.drivingIdleFraction;
	
	create table if not exists ##tempdb##.link like ##defaultdb##.link;
	TRUNCATE ##tempdb##.link;
	
	CREATE TABLE IF NOT EXISTS ##tempdb##.sho like ##defaultdb##.sho;
	TRUNCATE ##tempdb##.sho;

	-- perform intermediate calculations
	INSERT INTO ##tempdb##.SourceTypeAgePopulation (yearID,sourceTypeID,ageID,population) 
		SELECT sty.YearID, sty.SourceTypeID, stad.AgeID, sty.SourceTypePopulation * stad.AgeFraction 
		FROM ##inputdb##.SourceTypeYear sty,
			 ##inputdb##.SourceTypeAgeDistribution stad 
		WHERE sty.sourceTypeID = stad.sourceTypeID AND 
			  sty.yearID = stad.yearID;
						
	INSERT INTO ##tempdb##.HPMSVTypePopulation (yearID,HPMSVTypeID,population) 
		SELECT stap.yearID, sut.HPMSVTypeID, sum(stap.population) 
		FROM ##tempdb##.SourceTypeAgePopulation stap,
			 ##defaultdb##.sourceusetype sut 
		WHERE stap.sourceTypeID = sut.sourceTypeID
		GROUP BY stap.yearID, sut.HPMSVTypeID;
			
	INSERT INTO ##tempdb##.FractionWithinHPMSVType (yearID,sourceTypeID,ageID,fraction) 
		SELECT stap.yearID, stap.sourceTypeID, stap.ageID, stap.population / hvtp.population 
		FROM ##tempdb##.SourceTypeAgePopulation stap,
			 ##defaultdb##.SourceUseType sut,
			 ##tempdb##.HPMSVTypePopulation hvtp 
		WHERE stap.sourceTypeID = sut.sourceTypeID AND 
			  sut.HPMSVTypeID = hvtp.HPMSVTypeID AND 
			  stap.yearID = hvtp.yearID AND 
			  hvtp.population <> 0;
						
	INSERT INTO ##tempdb##.HPMSTravelFraction (yearID,HPMSVTypeID,fraction) 
		SELECT fwhvt.yearID, sut.HPMSVTypeID, sum(fwhvt.fraction * sta.relativeMAR) 
		FROM ##tempdb##.FractionWithinHPMSVType fwhvt,
			 ##defaultdb##.SourceUseType sut,
			 ##defaultdb##.SourceTypeAge sta 
		WHERE sta.sourceTypeID = fwhvt.sourceTypeID AND 
			  sta.ageID = fwhvt.ageID AND 
			  fwhvt.sourceTypeID = sut.sourceTypeID 
		GROUP BY fwhvt.yearID, sut.HPMSVTypeID;	
						
	INSERT INTO ##tempdb##.TravelFraction (yearID,sourceTypeID,ageID,fraction) 
		SELECT fwhvt.yearID, fwhvt.sourceTypeID, fwhvt.ageID, (fwhvt.fraction*sta.relativeMAR)/hpmstf.fraction 
		FROM ##tempdb##.FractionWithinHPMSVType fwhvt,
			 ##defaultdb##.SourceUseType sut,
			 ##defaultdb##.SourceTypeAge sta,
			 ##tempdb##.HPMSTravelFraction hpmstf 
		WHERE sta.sourceTypeID = fwhvt.sourceTypeID AND 
			  sta.ageID = fwhvt.ageID AND 
			  fwhvt.sourceTypeID = sut.sourceTypeID AND 
			  hpmstf.yearID = fwhvt.yearID AND 
			  hpmstf.HPMSVTypeID = sut.HPMSVTypeID AND 
			  hpmstf.fraction <> 0;

	-- If VMT by source type has been provided, instead of by HPMSVType, then
	-- normalize TravelFraction by year and sourcetype.
	set howMany=0;
	select sum(nrows) into howMany from (select count(*) as nrows from ##inputdb##.SourceTypeDayVMT UNION select count(*) as nrows from ##inputdb##.SourceTypeYearVMT) as t1;
	set howMany=ifnull(howMany,0);
	if(howMany > 0) then
		drop table if exists ##tempdb##.TravelFractionSourceTypeSum;

		create table ##tempdb##.TravelFractionSourceTypeSum
			select yearID, sourceTypeID, sum(fraction) as totalTravelFraction
			from ##tempdb##.TravelFraction
			group by yearID, sourceTypeID
			order by null;
		
		update ##tempdb##.TravelFraction, ##tempdb##.TravelFractionSourceTypeSum
			set fraction = case when totalTravelFraction > 0 then fraction / totalTravelFraction else 0 end
			where ##tempdb##.TravelFraction.yearID = ##tempdb##.TravelFractionSourceTypeSum.yearID
			and ##tempdb##.TravelFraction.sourceTypeID = ##tempdb##.TravelFractionSourceTypeSum.sourceTypeID;
	end if;
	
	-- if user VMT option is HPMS by Year (otherwise, this does nothing)
	INSERT IGNORE INTO ##tempdb##.AnalysisYearVMT (yearID,HPMSVTypeID,VMT) 
		SELECT hvty.yearID, hvty.HPMSVTypeID, hvty.HPMSBaseYearVMT 
		FROM ##defaultdb##.SourceUseType sut,
			 ##inputdb##.HPMSVTypeYear hvty 
		WHERE sut.HPMSVTypeID = hvty.HPMSVTypeID;
								
	INSERT INTO ##tempdb##.AnnualVMTByAgeRoadway (yearID,roadTypeID,sourceTypeID,ageID,VMT) 
		SELECT tf.yearID, rtd.roadTypeID, tf.sourceTypeID, tf.ageID, ayv.vmt*rtd.roadTypeVMTFraction*tf.fraction 
		FROM ##defaultdb##.RoadType rsrt,
			##tempdb##.TravelFraction tf,
			##tempdb##.AnalysisYearVMT ayv,
			##inputdb##.RoadTypeDistribution rtd,
			##defaultdb##.SourceUseType sut 
		WHERE rsrt.roadTypeID = rtd.roadTypeID AND 
			  ayv.yearID = tf.yearID AND 
			  tf.sourceTypeID = sut.sourceTypeID AND 
			  sut.HPMSVTypeID = ayv.HPMSVTypeID AND 
			  rtd.sourceTypeID = tf.sourceTypeID;
	
	-- if user VMT option is SourceType by Year otherwise, this does nothing)
	INSERT INTO ##tempdb##.AnnualVMTByAgeRoadway (yearID, roadTypeID, sourceTypeID, ageID, VMT)
		SELECT tf.yearID, rtd.roadTypeID, tf.sourceTypeID, tf.ageID, v.vmt*rtd.roadTypeVMTFraction*tf.fraction
		FROM ##defaultdb##.RoadType rsrt,
			 ##tempdb##.TravelFraction tf,
			 ##inputdb##.SourceTypeYearVMT v,
			 ##inputdb##.RoadTypeDistribution rtd
		WHERE rsrt.roadTypeID = rtd.roadTypeID AND
			  v.yearID = tf.yearID AND
			  tf.sourceTypeID = v.sourceTypeID AND
			  rtd.sourceTypeID = tf.sourceTypeID;

	DROP TABLE IF EXISTS ##tempdb##.AvarMonth;
	CREATE TABLE ##tempdb##.AvarMonth
		SELECT avar.*, monthID, monthVMTFraction
		FROM ##tempdb##.AnnualVMTByAgeRoadway avar
		INNER JOIN ##inputdb##.MonthVMTFraction mvf USING (sourceTypeID);

	DROP TABLE IF EXISTS ##tempdb##.AvarMonthDay;
	CREATE TABLE ##tempdb##.AvarMonthDay
		SELECT avarm.*, dayID, dayVMTFraction, monthVMTFraction*dayVMTFraction as monthDayFraction
		FROM ##tempdb##.AvarMonth avarm
		INNER JOIN ##inputdb##.DayVMTFraction dvf USING (sourceTypeID, monthID, roadTypeID);

	INSERT INTO ##tempdb##.VMTByAgeRoadwayHour (yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, hourID, VMT, hourDayID)
		SELECT avar.yearID, avar.roadTypeID, avar.sourceTypeID, avar.ageID, avar.monthID, avar.dayID, hvf.hourID,
			   avar.VMT*avar.monthDayFraction*hvf.hourVMTFraction / (noOfDays / 7), -- replaced weeksPerMonthClause in TAG with "(noOfDays / 7)", added moay join
			   hd.hourDayID
		FROM ##tempdb##.AvarMonthDay avar
		INNER JOIN ##inputdb##.HourVMTFraction hvf USING(sourceTypeID, roadTypeID, dayID)
		INNER JOIN ##defaultdb##.HourDay hd ON (hd.hourID=hvf.hourID and hd.dayID=avar.dayID)
		INNER JOIN ##defaultdb##.MonthOfAnyYear moay USING (monthID);

	-- if user VMT option is SourceType by Day (otherwise, this does nothing)
	insert ignore into ##tempdb##.VMTByAgeRoadwayHour (yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, hourID, VMT, hourDayID)
		select vmt.yearID, rtd.roadTypeID, vmt.sourceTypeID, tf.ageID, vmt.monthID, vmt.dayID, h.hourID,
			   vmt.VMT*h.hourVMTFraction*rtd.roadTypeVMTFraction*tf.fraction*dow.noOfRealDays as VMT,
			   hd.hourDayID
		from ##inputdb##.SourceTypeDayVMT vmt
		inner join ##inputdb##.RoadTypeDistribution rtd on (rtd.sourceTypeID=vmt.sourceTypeID)
		inner join ##defaultdb##.HourDay hd on (hd.dayID=vmt.dayID)
		inner join ##inputdb##.hourVMTFraction h on (h.hourID=hd.hourID and h.roadTypeID=rtd.roadTypeID and h.sourceTypeID=rtd.sourceTypeID)
		inner join ##tempdb##.TravelFraction tf on (tf.yearID=vmt.yearID and tf.sourceTypeID=rtd.sourceTypeID)
		inner join ##defaultdb##.DayOfAnyWeek dow on (dow.dayID=vmt.dayID);

	-- if user VMT option is HPMS by Day (otherwise, this does nothing)
	insert ignore into ##tempdb##.VMTByAgeRoadwayHour (yearID, roadTypeID, sourceTypeID, ageID, monthID, dayID, hourID, VMT, hourDayID)
		select vmt.yearID, rtd.roadTypeID, sut.sourceTypeID, tf.ageID, vmt.monthID, vmt.dayID, h.hourID,
			   vmt.VMT*h.hourVMTFraction*rtd.roadTypeVMTFraction*tf.fraction*dow.noOfRealDays as VMT,
			   hd.hourDayID
		from ##inputdb##.HPMSVTypeDay vmt
		inner join ##defaultdb##.SourceUseType sut on (sut.HPMSVTypeID=vmt.HPMSVTypeID)
		inner join ##inputdb##.RoadTypeDistribution rtd on (rtd.sourceTypeID=sut.sourceTypeID)
		inner join ##defaultdb##.HourDay hd on (hd.dayID=vmt.dayID)
		inner join ##inputdb##.hourVMTFraction h on (h.hourID=hd.hourID and h.roadTypeID=rtd.roadTypeID and h.sourceTypeID=rtd.sourceTypeID)
		inner join ##tempdb##.TravelFraction tf on (tf.yearID=vmt.yearID and tf.sourceTypeID=rtd.sourceTypeID)
		inner join ##defaultdb##.DayOfAnyWeek dow on (dow.dayID=vmt.dayID);
	
	-- regardless of how we got here, group over ageID for the summary table
	insert into ##tempdb##.vmtByMYRoadHourSummary (yearID, roadTypeID, sourceTypeID, monthID, hourID, dayID, hourDayID, totalVMT)
		select yearID, roadTypeID, sourceTypeID, monthID, hourID, dayID, hourDayID, sum(VMT) as totalVMT
		from ##tempdb##.VMTByAgeRoadwayHour
		group by yearID, roadTypeID, sourceTypeID, monthID, hourID, dayID
		having sum(VMT) > 0;
					
	insert into ##tempdb##.vmtByMYRoadHourFraction (yearID, roadTypeID, sourceTypeID, modelYearID, monthID, hourID, dayID, hourDayID, vmtFraction)
		select s.yearID, s.roadTypeID, s.sourceTypeID, (v.yearID-v.ageID) as modelYearID, s.monthID, s.hourID, s.dayID, s.hourDayID,  (VMT/totalVMT) as vmtFraction
		from ##tempdb##.vmtByMYRoadHourSummary s
		inner join ##tempdb##.VMTByAgeRoadwayHour v using (yearID, roadTypeID, sourceTypeID, monthID, dayID, hourID);

	INSERT INTO ##tempdb##.AverageSpeed (roadTypeID,sourceTypeID,dayID,hourID,averageSpeed) 
		SELECT asd.roadTypeID, asd.sourceTypeID, hd.dayID, hd.hourID, sum(asb.avgBinSpeed*asd.avgSpeedFraction) 
		FROM ##defaultdb##.RoadType rsrt,
			 ##defaultdb##.HourOfAnyDay hoad,
			 ##defaultdb##.AvgSpeedBin asb,
			 ##inputdb##.AvgSpeedDistribution asd,
			 ##defaultdb##.HourDay hd 
		WHERE rsrt.roadTypeID = asd.roadTypeID AND 
			  hd.hourID = hoad.hourID AND 
			  asb.avgSpeedBinID = asd.avgSpeedBinID AND 
			  asd.hourDayID = hd.hourDayID 
		GROUP BY asd.roadTypeID, asd.sourceTypeID, hd.dayID, hd.hourID;

	INSERT INTO ##tempdb##.SHOByAgeRoadwayHour (yearID,roadTypeID,sourceTypeID,ageID,monthID,dayID,hourID,hourDayID,SHO,VMT)
		SELECT varh.yearID, varh.roadTypeID, varh.sourceTypeID, varh.ageID, varh.monthID, varh.dayID, varh.hourID, varh.hourDayID,
			   IF(asp.averageSpeed<>0,
				  COALESCE(varh.VMT/asp.averageSpeed,0.0),
				  0.0),
			   varh.VMT 
		FROM ##tempdb##.VMTByAgeRoadwayHour varh 
		LEFT JOIN ##tempdb##.AverageSpeed asp ON (asp.roadTypeID = varh.roadTypeID AND 
												  asp.sourceTypeID = varh.sourceTypeID AND 
												  asp.dayID = varh.dayID AND 
												  asp.hourID = varh.hourID);
	
	INSERT INTO ##tempdb##.drivingIdleFraction
		select hourDayID,yearID,roadTypeID,sourceTypeID,sum(roadIdleFraction*avgSpeedFraction) as drivingIdleFraction
		from ##defaultdb##.roadidlefraction
		join ##defaultdb##.hourDay using (dayID)
		join ##inputdb##.avgSpeedDistribution using (hourDayID,avgSpeedBinID,sourceTypeID,roadTYpeID)
		join ##inputdb##.year
		group by hourDayID,yearID,roadTypeID,sourceTypeID
		order by yearID,sourceTypeID,roadTypeID,hourDayID;
		
	insert into ##tempdb##.link (linkID,countyID,zoneID,roadTypeID)
		select zoneID*10 + roadTypeID as linkID,countyID,zoneID,roadTypeID from ##inputdb##.zone
		join ##defaultdb##.roadtype
		where roadTypeID < 100;
		
	insert into ##tempdb##.sho	
		select hourDayID,monthID,yearID,ageID,linkID,sourceTypeID,SHO as SHO, NULL as SHOCV, VMT as distance, 'N' as isUserInput
		from ##tempdb##.shobyageroadwayhour
		join ##tempdb##.link using (roadTypeID)
        where SHO > 0 or VMT > 0
        order by sourceTypeID,linkID,ageID,monthID,hourDayID;
		
	
		
	-- ---------------------------------------
	-- END TotalActivityGenerator Calculations
	-- ---------------------------------------

	-- ---------------------------
	-- START ONI Calculation Block
	-- ---------------------------

	-- ONI Calculation is slightly different than TAG because we don't have drivingIdleFraction (that is calculated at run time)
    -- Instead, join with the pre-calculated roadIdleFraction and weight the calculation by the avgSpeedFraction 
    -- 		(roadIdleFraction is by avgSpeedBin, which drivingIdleFraction does not have)
    -- Also need to divide by number of days, because the ##tempdb##.SHOByAgeRoadwayHour table is by portion of week, whereas
    --      the MOVES output is by typical day
    DROP TABLE IF EXISTS ##tempdb##.onitooloutput_temp;
	
	CREATE TABLE IF NOT EXISTS ##tempdb##.onitooloutput_temp
	select s.yearID,s.monthID,hd.hourID,hd.dayID,s.sourceTypeID,s.ageID,tif.minModelYearID,tif.maxModelYearID,
	sum(s.sho) / doaw.noOfRealDays as onroadSHO,
    sum(s.sho * avgs.averageSpeed) / doaw.noOfRealDays as VMT,
	(case when totalIdleFraction <> 1 then 
				greatest(
					sum((s.sho))*
					(totalIdleFraction-sum(s.sho*drivingIdleFraction) /sum((s.sho)))
					/(1-totalIdleFraction),0) 
			else 0 
			end 
		) / doaw.noOfRealDays as ONI
		from ##tempdb##.sho s 
		inner join ##tempdb##.link l on ( 
			l.linkID = s.linkID 
			and l.roadTypeID <> 1) 
		inner join ##tempdb##.link lo on ( 
			l.zoneID = lo.zoneID 
			and lo.roadTypeID = 1) 
		inner join ##inputdb##.county c on (lo.countyID = c.countyID) 
		inner join ##inputdb##.state st using (stateID) 
		inner join ##defaultdb##.hourDay hd on (s.hourDayID = hd.hourDayID) 
		inner join ##tempdb##.totalIdleFraction tif on ( 
			tif.idleRegionID = st.idleRegionID 
			and tif.countyTypeID = c.countyTypeID 
			and tif.sourceTypeID = s.sourceTypeID 
			and tif.monthID = s.monthID 
			and tif.dayID = hd.dayID 
			and tif.minModelYearID <= s.yearID - s.ageID 
			and tif.maxModelYearID >= s.yearID - s.ageID) 
		inner join ##tempdb##.drivingIdleFraction dif on ( 
			dif.hourDayID = s.hourDayID 
			and dif.yearID = s.yearID 
			and dif.roadTypeID = l.roadTypeID 
			and dif.sourceTypeID = s.sourceTypeID) 
		inner join ##tempdb##.averagespeed avgs on (
			avgs.roadTypeID = l.roadTypeID 
            and avgs.dayID = hd.dayID
            and avgs.hourID = hd.hourID
            and avgs.sourceTypeID = s.sourceTypeID)
		inner join ##inputdb##.year y on (s.yearID = y.yearID)
		inner join ##inputdb##.zone z on (s.linkID DIV 10 = z.zoneID)
		inner join ##defaultdb##.dayofanyweek doaw on (doaw.dayID = hd.dayID)
		group by s.yearID,s.monthID,hd.hourID,hd.dayID,s.sourceTypeID,s.ageID,tif.minModelYearID,tif.maxModelYearID
        order by s.yearID,s.monthID,hd.hourID,hd.dayID,s.sourceTypeID,s.ageID,tif.minModelYearID,tif.maxModelYearID;
	
    -- Sum over ageID and calculate ONI per VMT and per SHO	
    insert into ##tempdb##.oniToolOutput (yearID, monthID, dayID, hourID, sourceTypeID, minModelYearID, maxModelYearID, 
										  `onroadSHO (hr)`, `VMT (mi)`, `ONI (hr)`,
										  `ONI per VMT (hr idle/mi)`, `ONI per SHO (hr idle/hr operating)`)
		SELECT yearID, monthID, dayID, hourID, sourceTypeID, minModelYearID, maxModelYearID,
			   sum(onroadSHO), sum(VMT), sum(ONI),
			   sum(ONI) / sum(VMT), sum(ONI) / sum(onroadSHO)
		FROM ##tempdb##.oniToolOutput_temp
        GROUP BY yearID, monthID, dayID, hourID, sourceTypeID, minModelYearID, maxModelYearID;
    
	insert into ##tempdb##.oniTempMessages (message) values ('INFO: Successfully calculated default ONI hours from tables in ##inputdb##');
	
	-- -------------------------
	-- END ONI Calculation Block
	-- -------------------------	


	-- ----------------------------------------
	-- Final Clean Up (currently commented out because ##tempdb## is dropped by MOVES GUI)
	-- ----------------------------------------
	-- drop table IF EXISTS ##tempdb##.totalidlefraction;
	-- drop table if exists ##tempdb##.SHOByAgeRoadwayHour;
	-- drop table if exists ##tempdb##.StartsByAgeHour;
	-- drop table if exists ##tempdb##.VMTByAgeRoadwayHour;
	-- drop table if exists ##tempdb##.vmtByMYRoadHourFraction;
	-- drop table if exists ##tempdb##.HPMSVTypePopulation;
	-- drop table if exists ##tempdb##.FractionWithinHPMSVType;
	-- drop table if exists ##tempdb##.HPMSTravelFraction;
	-- drop table if exists ##tempdb##.TravelFraction;
	-- drop table if exists ##tempdb##.AnnualVMTByAgeRoadway;
	-- drop table if exists ##tempdb##.AverageSpeed;
	-- drop table if exists ##tempdb##.SHOByAgeDay;
	-- drop table if exists ##tempdb##.AnalysisYearVMT;
	-- drop table if exists ##tempdb##.SourceTypeAgePopulation;
	-- drop table if exists ##tempdb##.vmtByMYRoadHourSummary;
	-- drop table if exists ##tempdb##.ZoneRoadTypeLinkTemp;
	-- drop table if exists ##tempdb##.TravelFractionSourceTypeSum;
	-- DROP TABLE IF EXISTS ##tempdb##.AvarMonth;
	-- DROP TABLE IF EXISTS ##tempdb##.AvarMonthDay;
	-- DROP TABLE IF EXISTS ##tempdb##.onitooloutput_temp;
	
end ONITOOL_PROCEDURE
EndBlock

call ONITool();
drop procedure if exists ONITool;