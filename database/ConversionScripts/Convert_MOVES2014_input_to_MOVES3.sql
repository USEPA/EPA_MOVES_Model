-- --------------------------------------------------------------------------------------
-- This script is run by the MOVES GUI to convert a MOVES2014a/b County, Project, or
-- Nonroad input database to the MOVES3 format.
-- No changes are made to the original input database.
-- This script is run in the context of the new database (i.e., after it has been created
--     and the equivalent of a "USE my_new_database;" statement has been executed).
-- The MOVES GUI replaces ##defaultdb## with the name of the default database and
--     ##inputdb## with the name of the original 2014a/b CDM or PDM database.
-- Messages to be displayed to the user can be inserted into the convertTempMessages
--     table, which has one VARCHAR(1000) column. This table is managed by the MOVES GUI.
-- --------------------------------------------------------------------------------------

-- 

drop procedure if exists DBConverter;

BeginBlock
create procedure DBConverter()
begin
	-- Initialize all "TableExists" variables to false, and set them to true if select(*) >= 0 doesn't raise an error.
    -- If the variable is still false, then that table doesn't need to be transfered.
	DECLARE auditlogTableExists boolean default FALSE;
	DECLARE avftTableExists boolean default FALSE;
	DECLARE avgspeeddistributionTableExists boolean default FALSE;
	DECLARE countyTableExists boolean default FALSE;
	DECLARE dayvmtfractionTableExists boolean default FALSE;
	DECLARE driveschedulesecondlinkTableExists boolean default FALSE;
	DECLARE fuelformulationTableExists boolean default FALSE;
	DECLARE fuelsupplyTableExists boolean default FALSE;
	DECLARE fuelusagefractionTableExists boolean default FALSE;
	DECLARE hotellingactivitydistributionTableExists boolean default FALSE;
	DECLARE hotellingagefractionTableExists boolean default FALSE;
	DECLARE hotellinghourfractionTableExists boolean default FALSE;
	DECLARE hotellinghoursperdayTableExists boolean default FALSE;
	DECLARE hotellingmonthadjustTableExists boolean default FALSE;
	DECLARE hourvmtfractionTableExists boolean default FALSE;
	DECLARE hpmsvtypedayTableExists boolean default FALSE;
	DECLARE hpmsvtypeyearTableExists boolean default FALSE;
	DECLARE idledayadjustTableExists boolean default FALSE;
	DECLARE idlemodelyeargroupingTableExists boolean default FALSE;
	DECLARE idlemonthadjustTableExists boolean default FALSE;
	DECLARE imcoverageTableExists boolean default FALSE;
	DECLARE linkTableExists boolean default FALSE;
	DECLARE linksourcetypehourTableExists boolean default FALSE;
	DECLARE offnetworklinkTableExists boolean default FALSE;
	DECLARE opmodedistributionTableExists boolean default FALSE;
	DECLARE monthvmtfractionTableExists boolean default FALSE;
	DECLARE onroadretrofitTableExists boolean default FALSE;
	DECLARE roadtypedistributionTableExists boolean default FALSE;
	DECLARE sourcetypeagedistributionTableExists boolean default FALSE;
	DECLARE sourcetypedayvmtTableExists boolean default FALSE;
	DECLARE sourcetypeyearTableExists boolean default FALSE;
	DECLARE sourcetypeyearvmtTableExists boolean default FALSE;
	DECLARE startsTableExists boolean default FALSE;
	DECLARE startsageadjustmentTableExists boolean default FALSE;
	DECLARE startshourfractionTableExists boolean default FALSE;
	DECLARE startsmonthadjustTableExists boolean default FALSE;
	DECLARE startsopmodedistributionTableExists boolean default FALSE;
	DECLARE startsperdayTableExists boolean default FALSE;
	DECLARE startsperdaypervehicleTableExists boolean default FALSE;
	DECLARE stateTableExists boolean default FALSE;
	DECLARE totalidlefractionTableExists boolean default FALSE;
	DECLARE yearTableExists boolean default FALSE;
	DECLARE zoneTableExists boolean default FALSE;
	DECLARE zonemonthhourTableExists boolean default FALSE;
	DECLARE zoneroadtypeTableExists boolean default FALSE;
	DECLARE nrbaseyearequippopulationTableExists boolean default FALSE;
	DECLARE nrdayallocationTableExists boolean default FALSE;
	DECLARE nrfuelsupplyTableExists boolean default FALSE;
	DECLARE nrgrowthindexTableExists boolean default FALSE;
	DECLARE nrgrowthpatternTableExists boolean default FALSE;
	DECLARE nrgrowthpatternfinderTableExists boolean default FALSE;
	DECLARE nrmonthallocationTableExists boolean default FALSE;
	DECLARE nrretrofitfactorsTableExists boolean default FALSE;
	DECLARE nrstatesurrogateTableExists boolean default FALSE;
	DECLARE nrusmonthallocationTableExists boolean default FALSE;
    DECLARE CONTINUE HANDLER FOR 1146 IF (FALSE) THEN SELECT 1; END IF; -- do nothing if error 1146 occurs (table doesn't exist)
    select count(*) >= 0 into auditlogTableExists from ##inputdb##.auditlog;
    select count(*) >= 0 into avftTableExists from ##inputdb##.avft;
    select count(*) >= 0 into avgspeeddistributionTableExists from ##inputdb##.avgspeeddistribution;
    select count(*) >= 0 into countyTableExists from ##inputdb##.county;
    select count(*) >= 0 into dayvmtfractionTableExists from ##inputdb##.dayvmtfraction;
    select count(*) >= 0 into driveschedulesecondlinkTableExists from ##inputdb##.driveschedulesecondlink;
    select count(*) >= 0 into fuelformulationTableExists from ##inputdb##.fuelformulation;
    select count(*) >= 0 into fuelsupplyTableExists from ##inputdb##.fuelsupply;
    select count(*) >= 0 into fuelusagefractionTableExists from ##inputdb##.fuelusagefraction;
    select count(*) >= 0 into hotellingactivitydistributionTableExists from ##inputdb##.hotellingactivitydistribution;
    select count(*) >= 0 into hotellingagefractionTableExists from ##inputdb##.hotellingagefraction;
    select count(*) >= 0 into hotellinghourfractionTableExists from ##inputdb##.hotellinghourfraction;
    select count(*) >= 0 into hotellinghoursperdayTableExists from ##inputdb##.hotellinghoursperday;
    select count(*) >= 0 into hotellingmonthadjustTableExists from ##inputdb##.hotellingmonthadjust;
    select count(*) >= 0 into hourvmtfractionTableExists from ##inputdb##.hourvmtfraction;
    select count(*) >= 0 into hpmsvtypedayTableExists from ##inputdb##.hpmsvtypeday;
    select count(*) >= 0 into hpmsvtypeyearTableExists from ##inputdb##.hpmsvtypeyear;
    select count(*) >= 0 into idledayadjustTableExists from ##inputdb##.idledayadjust;
    select count(*) >= 0 into idlemodelyeargroupingTableExists from ##inputdb##.idlemodelyeargrouping;
    select count(*) >= 0 into idlemonthadjustTableExists from ##inputdb##.idlemonthadjust;
    select count(*) >= 0 into imcoverageTableExists from ##inputdb##.imcoverage;
    select count(*) >= 0 into linkTableExists from ##inputdb##.link;
    select count(*) >= 0 into linksourcetypehourTableExists from ##inputdb##.linksourcetypehour;
    select count(*) >= 0 into offnetworklinkTableExists from ##inputdb##.offnetworklink;
    select count(*) >= 0 into opmodedistributionTableExists from ##inputdb##.opmodedistribution;
    select count(*) >= 0 into monthvmtfractionTableExists from ##inputdb##.monthvmtfraction;
    select count(*) >= 0 into onroadretrofitTableExists from ##inputdb##.onroadretrofit;
    select count(*) >= 0 into roadtypedistributionTableExists from ##inputdb##.roadtypedistribution;
    select count(*) >= 0 into sourcetypeagedistributionTableExists from ##inputdb##.sourcetypeagedistribution;
    select count(*) >= 0 into sourcetypedayvmtTableExists from ##inputdb##.sourcetypedayvmt;
    select count(*) >= 0 into sourcetypeyearTableExists from ##inputdb##.sourcetypeyear;
    select count(*) >= 0 into sourcetypeyearvmtTableExists from ##inputdb##.sourcetypeyearvmt;
    select count(*) >= 0 into startsTableExists from ##inputdb##.starts;
    select count(*) >= 0 into startsageadjustmentTableExists from ##inputdb##.startsageadjustment;
    select count(*) >= 0 into startshourfractionTableExists from ##inputdb##.startshourfraction;
    select count(*) >= 0 into startsmonthadjustTableExists from ##inputdb##.startsmonthadjust;
    select count(*) >= 0 into startsopmodedistributionTableExists from ##inputdb##.startsopmodedistribution;
    select count(*) >= 0 into startsperdayTableExists from ##inputdb##.startsperday;
    select count(*) >= 0 into startsperdaypervehicleTableExists from ##inputdb##.startsperdaypervehicle;
    select count(*) >= 0 into stateTableExists from ##inputdb##.state;
    select count(*) >= 0 into totalidlefractionTableExists from ##inputdb##.totalidlefraction;
    select count(*) >= 0 into yearTableExists from ##inputdb##.year;
    select count(*) >= 0 into zoneTableExists from ##inputdb##.zone;
    select count(*) >= 0 into zonemonthhourTableExists from ##inputdb##.zonemonthhour;
    select count(*) >= 0 into zoneroadtypeTableExists from ##inputdb##.zoneroadtype;
    select count(*) >= 0 into nrbaseyearequippopulationTableExists from ##inputdb##.nrbaseyearequippopulation;
    select count(*) >= 0 into nrdayallocationTableExists from ##inputdb##.nrdayallocation;
    select count(*) >= 0 into nrfuelsupplyTableExists from ##inputdb##.nrfuelsupply;
    select count(*) >= 0 into nrgrowthindexTableExists from ##inputdb##.nrgrowthindex;
    select count(*) >= 0 into nrgrowthpatternTableExists from ##inputdb##.nrgrowthpattern;
    select count(*) >= 0 into nrgrowthpatternfinderTableExists from ##inputdb##.nrgrowthpatternfinder;
    select count(*) >= 0 into nrmonthallocationTableExists from ##inputdb##.nrmonthallocation;
    select count(*) >= 0 into nrretrofitfactorsTableExists from ##inputdb##.nrretrofitfactors;
    select count(*) >= 0 into nrstatesurrogateTableExists from ##inputdb##.nrstatesurrogate;
    select count(*) >= 0 into nrusmonthallocationTableExists from ##inputdb##.nrusmonthallocation;
    
	-- -----------------------------------
	-- Now, transfer all tables that exist
	-- -----------------------------------
    
    -- auditlog: always create schema so that the converted database appears in the GUI, but only transfer data if the
	--           table exists in the original input db. Note, create statement is generated at runtime since this table
	--           doesn't exist in the default database.
	##create_auditlog_table##
    if (auditlogTableExists) then
        insert ignore into auditlog select * from ##inputdb##.auditlog;
	end if;

	-- avft: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (avftTableExists) then
		create table avft like ##defaultdb##.avft;
	end if;

	-- avgspeeddistribution: if original input db values are based on MOVES defaults, user should clear this data in
    --                       the new input db, export the new default values, and reimport them.
    if (avgspeeddistributionTableExists) then
		create table avgspeeddistribution like ##defaultdb##.avgspeeddistribution;
        insert ignore into avgspeeddistribution select * from ##inputdb##.avgspeeddistribution;
	end if;

	-- county: there is a schema change for this table
    if (countyTableExists) then
		create table county like ##defaultdb##.county;
        insert ignore into county
			select inDB.countyID, inDB.stateID, inDB.countyName, inDB.altitude, inDB.GPAFract, inDB.barometricPressure,inDB.barometricPressureCV,
				   defDB.countyTypeID, defDB.msa
			from ##inputdb##.county as inDB
            join ##defaultdb##.county as defDB using (countyID);
	end if;

	-- dayvmtfraction: no schema changes and input data can be transfered as-is
    if (dayvmtfractionTableExists) then
		create table dayvmtfraction like ##defaultdb##.dayvmtfraction;
        insert ignore into dayvmtfraction select * from ##inputdb##.dayvmtfraction;
	end if;

	-- driveschedulesecondlink: no schema changes and input data can be transfered as-is
    if (driveschedulesecondlinkTableExists) then
		create table driveschedulesecondlink like ##defaultdb##.driveschedulesecondlink;
        insert ignore into driveschedulesecondlink select * from ##inputdb##.driveschedulesecondlink;
	end if;

	-- fuelformulation: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (fuelformulationTableExists) then
		create table fuelformulation like ##defaultdb##.fuelformulation;
	end if;

	-- fuelsupply: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (fuelsupplyTableExists) then
		create table fuelsupply like ##defaultdb##.fuelsupply;
	end if;

	-- fuelusagefraction: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (fuelusagefractionTableExists) then
		create table fuelusagefraction like ##defaultdb##.fuelusagefraction;
	end if;

	-- hotelling tables: 
    -- 		hotellingactivitydistribution should exist in the original input db for any onroad run, so its existance shows that the new hotelling tables
    -- 		should be created.
    -- 		for hotellingactivitydistribution itself, do not transfer data from original input db, as user should export defaults and modify as necessary
    -- 		for the other hotelling tables, just create them, as they are new and wouldn't exist in the original input db
    if (hotellingactivitydistributionTableExists) then
		create table hotellingactivitydistribution like ##defaultdb##.hotellingactivitydistribution;
		create table hotellingagefraction like ##defaultdb##.hotellingagefraction;
		create table hotellinghourfraction like ##defaultdb##.hotellinghourfraction;
		create table hotellinghoursperday like ##defaultdb##.hotellinghoursperday;
		create table hotellingmonthadjust like ##defaultdb##.hotellingmonthadjust;
	end if;

	-- hourvmtfraction: no schema changes and input data can be transfered as-is
    if (hourvmtfractionTableExists) then
		create table hourvmtfraction like ##defaultdb##.hourvmtfraction;
        insert ignore into hourvmtfraction select * from ##inputdb##.hourvmtfraction;
	end if;

	-- hpmsvtypeday: no schema changes and input data can be transfered as-is
    if (hpmsvtypedayTableExists) then
		create table hpmsvtypeday like ##defaultdb##.hpmsvtypeday;
        insert ignore into hpmsvtypeday select * from ##inputdb##.hpmsvtypeday;
	end if;

	-- hpmsvtypeyear: no schema changes and input data can be transfered as-is
    if (hpmsvtypeyearTableExists) then
		create table hpmsvtypeyear like ##defaultdb##.hpmsvtypeyear;
        insert ignore into hpmsvtypeyear select * from ##inputdb##.hpmsvtypeyear;
	end if;

	-- idle tables: these tables are new but only used at County Scale, so their schemas just need to be created if this is a CDM
    -- 		use the existence of avgspeeddistribution to determine if this is a CDM (this table is only used at County Scale, too)
    if (avgspeeddistributionTableExists) then
		create table idledayadjust like ##defaultdb##.idledayadjust;
		create table idlemodelyeargrouping like ##defaultdb##.idlemodelyeargrouping;
		create table idlemonthadjust like ##defaultdb##.idlemonthadjust;
		create table totalidlefraction like ##defaultdb##.totalidlefraction;
	end if;

	-- imcoverage: if original input db values are based on MOVES defaults, user should clear this data in
    --             the new input db, export the new default values, and reimport them.
    if (imcoverageTableExists) then
		create table imcoverage like ##defaultdb##.imcoverage;
        insert ignore into imcoverage select * from ##inputdb##.imcoverage;
	end if;

	-- link: no schema changes and input data can be transfered as-is
    if (linkTableExists) then
		create table link like ##defaultdb##.link;
        insert ignore into link select * from ##inputdb##.link;
	end if;

	-- linksourcetypehour: no schema changes and input data can be transfered as-is
    if (linksourcetypehourTableExists) then
		create table linksourcetypehour like ##defaultdb##.linksourcetypehour;
        insert ignore into linksourcetypehour select * from ##inputdb##.linksourcetypehour;
	end if;

	-- offnetworklink: no schema changes and input data can be transfered as-is
    if (offnetworklinkTableExists) then
		create table offnetworklink like ##defaultdb##.offnetworklink;
        insert ignore into offnetworklink select * from ##inputdb##.offnetworklink;
	end if;

	-- opmodedistribution: no schema changes and input data can be transfered as-is
    if (opmodedistributionTableExists) then
		create table opmodedistribution like ##defaultdb##.opmodedistribution;
        insert ignore into opmodedistribution select * from ##inputdb##.opmodedistribution;
	end if;

	-- monthvmtfraction: no schema changes and input data can be transfered as-is
    if (monthvmtfractionTableExists) then
		create table monthvmtfraction like ##defaultdb##.monthvmtfraction;
        insert ignore into monthvmtfraction select * from ##inputdb##.monthvmtfraction;
	end if;

	-- onroadretrofit: no schema changes and input data can be transfered as-is
    if (onroadretrofitTableExists) then
		create table onroadretrofit like ##defaultdb##.onroadretrofit;
        insert ignore into onroadretrofit select * from ##inputdb##.onroadretrofit;
	end if;

	-- roadtypedistribution: no schema changes and input data can be transfered as-is
    if (roadtypedistributionTableExists) then
		create table roadtypedistribution like ##defaultdb##.roadtypedistribution;
        insert ignore into roadtypedistribution select * from ##inputdb##.roadtypedistribution;
	end if;

	-- sourcetypeagedistribution: if original input db values for some source types are based on MOVES defaults, user should export both the imported data 
    -- 							  and the default data for this table. Then the user should overwrite the values in the imported data spreadsheet with the
    -- 							  new defaults for just those source types. Finally, the user should clear the data for this table in the new input database,
    -- 							  and then re-import.
	if (sourcetypeagedistributionTableExists) then
		create table sourcetypeagedistribution like ##defaultdb##.sourcetypeagedistribution;
        insert ignore into sourcetypeagedistribution select * from ##inputdb##.sourcetypeagedistribution;
	end if;
    
	-- sourcetypedayvmt: no schema changes and input data can be transfered as-is
    if (sourcetypedayvmtTableExists) then
		create table sourcetypedayvmt like ##defaultdb##.sourcetypedayvmt;
        insert ignore into sourcetypedayvmt select * from ##inputdb##.sourcetypedayvmt;
	end if;
    
	-- sourcetypeyearTableExists: no schema changes and input data can be transfered as-is
    if (sourcetypeyearTableExists) then
		create table sourcetypeyear like ##defaultdb##.sourcetypeyear;
        insert ignore into sourcetypeyear select * from ##inputdb##.sourcetypeyear;
	end if;
    
	-- sourcetypeyearvmt: no schema changes and input data can be transfered as-is
    if (sourcetypeyearvmtTableExists) then
		create table sourcetypeyearvmt like ##defaultdb##.sourcetypeyearvmt;
        insert ignore into sourcetypeyearvmt select * from ##inputdb##.sourcetypeyearvmt;
	end if;
    
	-- starts tables: 
    -- 		starts should exist in the original input db for a county run run, so its existance shows that the new starts tables
    -- 		should be created.
    -- 		for starts itself, there are no schema changes and input data can be transfered as-is
    -- 		for startsageadjustment, this is a new table, so just create the schema
    -- 		for startshourfraction, do not transfer data from original input db, as user should export defaults and modify as necessary
    -- 		for startsmonthadjust, do not transfer data from original input db, as user should export defaults and modify as necessary
    -- 		for startsopmodedistribution, do not transfer data from original input db, as user should export defaults and modify as necessary
    -- 		for startsperday, do not transfer data from original input db, as user should export defaults and modify as necessary
    -- 		for startsperdaypervehicle, this is a new table, so just create the schema
    -- 		note: importstartsopmodedistribution is now called startsopmodedistribution, and startssourcetypefraction is no longer used
    if (startsTableExists) then
		create table starts like ##defaultdb##.starts;
        insert ignore into starts select * from ##inputdb##.starts;
		create table startsageadjustment like ##defaultdb##.startsageadjustment;
		create table startshourfraction like ##defaultdb##.startshourfraction;
		create table startsmonthadjust like ##defaultdb##.startsmonthadjust;
		create table startsopmodedistribution like ##defaultdb##.startsopmodedistribution;
		create table startsperday like ##defaultdb##.startsperday;
		create table startsperdaypervehicle like ##defaultdb##.startsperdaypervehicle;
	end if;

	-- state: there is a schema change for this table
    if (stateTableExists) then
		create table state like ##defaultdb##.state;
        insert ignore into state
			select inDB.stateID, inDB.stateName, inDB.stateABBR,
                   defDB.idleRegionID 
			from ##inputdb##.state as inDB
            join ##defaultdb##.state as defDB using (stateID);
	end if;
    
	-- year: no schema changes and input data can be transfered as-is
    if (yearTableExists) then
		create table year like ##defaultdb##.year;
        insert ignore into year select * from ##inputdb##.year;
	end if;
    
	-- zone: no schema changes and input data can be transfered as-is
    if (zoneTableExists) then
		create table zone like ##defaultdb##.zone;
        insert ignore into zone select * from ##inputdb##.zone;
	end if;
    
	-- zonemonthhour: no schema changes and input data can be transfered as-is
    if (zonemonthhourTableExists) then
		create table zonemonthhour like ##defaultdb##.zonemonthhour;
        insert ignore into zonemonthhour select * from ##inputdb##.zonemonthhour;
	end if;
    
	-- zoneroadtype: no schema changes and input data can be transfered as-is
    if (zoneroadtypeTableExists) then
		create table zoneroadtype like ##defaultdb##.zoneroadtype;
        insert ignore into zoneroadtype select * from ##inputdb##.zoneroadtype;
	end if;
    
	-- nrbaseyearequippopulation: no schema changes and input data can be transfered as-is
    if (nrbaseyearequippopulationTableExists) then
		create table nrbaseyearequippopulation like ##defaultdb##.nrbaseyearequippopulation;
        insert ignore into nrbaseyearequippopulation select * from ##inputdb##.nrbaseyearequippopulation;
	end if;
    
	-- nrdayallocation: no schema changes and input data can be transfered as-is
    if (nrdayallocationTableExists) then
		create table nrdayallocation like ##defaultdb##.nrdayallocation;
        insert ignore into nrdayallocation select * from ##inputdb##.nrdayallocation;
	end if;

	-- nrfuelsupply: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (nrfuelsupplyTableExists) then
		create table nrfuelsupply like ##defaultdb##.nrfuelsupply;
	end if;
    
	-- nrgrowthindex: no schema changes and input data can be transfered as-is
    if (nrgrowthindexTableExists) then
		create table nrgrowthindex like ##defaultdb##.nrgrowthindex;
        insert ignore into nrgrowthindex select * from ##inputdb##.nrgrowthindex;
	end if;
    
	-- nrgrowthpattern: no schema changes and input data can be transfered as-is
    if (nrgrowthpatternTableExists) then
		create table nrgrowthpattern like ##defaultdb##.nrgrowthpattern;
        insert ignore into nrgrowthpattern select * from ##inputdb##.nrgrowthpattern;
	end if;
    
	-- nrgrowthpatternfinder: no schema changes and input data can be transfered as-is
    if (nrgrowthpatternfinderTableExists) then
		create table nrgrowthpatternfinder like ##defaultdb##.nrgrowthpatternfinder;
        insert ignore into nrgrowthpatternfinder select * from ##inputdb##.nrgrowthpatternfinder;
	end if;
    
	-- nrmonthallocation: no schema changes and input data can be transfered as-is
    if (nrmonthallocationTableExists) then
		create table nrmonthallocation like ##defaultdb##.nrmonthallocation;
        insert ignore into nrmonthallocation select * from ##inputdb##.nrmonthallocation;
	end if;
    
	-- nrretrofitfactors: no schema changes and input data can be transfered as-is
    if (nrretrofitfactorsTableExists) then
		create table nrretrofitfactors like ##defaultdb##.nrretrofitfactors;
        insert ignore into nrretrofitfactors select * from ##inputdb##.nrretrofitfactors;
	end if;
    
	-- nrstatesurrogate: no schema changes and input data can be transfered as-is
    if (nrstatesurrogateTableExists) then
		create table nrstatesurrogate like ##defaultdb##.nrstatesurrogate;
        insert ignore into nrstatesurrogate select * from ##inputdb##.nrstatesurrogate;
	end if;
    
	-- These tables didn't exist in MOVES2014 and need to be added so MOVES3 recognizes
	-- this database as a valid input database. Using hpmsvtypeyearTableExists as a
	-- surrogate for being a CDM (these tables aren't needed for PDMs or NR input dbs).
	if (hpmsvtypeyearTableExists AND NOT hpmsvtypedayTableExists) then
		create table HPMSVTypeDay like ##defaultdb##.HPMSVTypeDay;
	end if;
	if (hpmsvtypeyearTableExists AND NOT sourcetypedayvmtTableExists) then
		create table SourceTypeDayVMT like ##defaultdb##.SourceTypeDayVMT;
	end if;
	if (hpmsvtypeyearTableExists AND NOT sourcetypeyearvmtTableExists) then
		create table SourceTypeYearVMT like ##defaultdb##.SourceTypeYearVMT;
	end if;
	
	-- nrfuelsupply is also a table added between MOVES2014 and MOVES3
	-- However, the old data shouldn't be carried over, and the table doesn't
	-- need to exist, so nothing to do here. If the user wants to change the
	-- fuel supply, the standard guidance for fuels applies.
	-- if (NOT nrfuelsupplyTableExists) then
	-- 	create table nrfuelsupply like ##defaultdb##.nrfuelsupply;
	-- end if;
end
EndBlock

call DBConverter();
drop procedure if exists DBConverter;

