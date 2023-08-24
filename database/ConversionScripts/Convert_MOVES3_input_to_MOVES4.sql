-- --------------------------------------------------------------------------------------
-- This script is run by the MOVES GUI to convert a MOVES3 County, Project, or Nonroad
-- input database to the MOVES4 format.
-- No changes are made to the original input database.
-- This script is run in the context of the new database (i.e., after it has been created
--     and the equivalent of a "USE my_new_database;" statement has been executed).
-- The MOVES GUI replaces ##defaultdb## with the name of the default database and
--     ##inputdb## with the name of the original MOVES3 CDM or PDM database.
-- Messages to be displayed to the user can be inserted into the convertTempMessages
--     table, which has one VARCHAR(1000) column. This table is managed by the MOVES GUI.
--
-- Last updated: 11/30/2022
-- --------------------------------------------------------------------------------------

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
        insert into convertTempMessages VALUES ('AVFT table not transfered. Export the defaults and modify as necessary.');
	end if;

	-- avgspeeddistribution: no schema changes and input data can be transfered as-is
    if (avgspeeddistributionTableExists) then
		create table avgspeeddistribution like ##defaultdb##.avgspeeddistribution;
        insert ignore into avgspeeddistribution select * from ##inputdb##.avgspeeddistribution;
	end if;

	-- county: no schema changes and input data can be transfered as-is
    if (countyTableExists) then
		create table county like ##defaultdb##.county;
        insert ignore into county select * from ##inputdb##.county;
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
	    insert into convertTempMessages VALUES ('FuelFormulation table not transfered. Export the defaults and modify with the Fuels Wizard as necessary.');
	end if;

	-- fuelsupply: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (fuelsupplyTableExists) then
		create table fuelsupply like ##defaultdb##.fuelsupply;
	    insert into convertTempMessages VALUES ('FuelSupply table not transfered. Export the defaults and modify as necessary.');
	end if;

	-- fuelusagefraction: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (fuelusagefractionTableExists) then
		create table fuelusagefraction like ##defaultdb##.fuelusagefraction;
	    insert into convertTempMessages VALUES ('FuelusageFraction table not transfered. Export the defaults and modify as necessary.');
	end if;
	
	-- hotellingactivitydistribution: MOVES3 versions of this table only have diesel values.
    --                                If the table exists, create it.
    --                                If it contains data: transfer the data and assign the diesel fuel type,
    --                                then insert default values for missing source types. This last part is tricky,
    --                                because the default values are for zoneID 990000, so we'll need to insert rows
    --                                for all zones present.
    if (hotellingactivitydistributionTableExists) then
		create table hotellingactivitydistribution like ##defaultdb##.hotellingactivitydistribution;
        if ((select count(*) from ##inputdb##.hotellingactivitydistribution) > 0) then
            insert ignore into hotellingactivitydistribution
                select zoneID, 2 as fuelTypeID, beginModelYearID, endModelYearID, opModeID, opModeFraction
                from ##inputdb##.hotellingactivitydistribution;
            drop table if exists hotellingactivitydistribution_tmp;
            create table hotellingactivitydistribution_tmp (zoneID int not NULL, PRIMARY KEY (zoneID) USING BTREE) ENGINE=MYISAM;
            insert ignore into hotellingactivitydistribution_tmp 
                select distinct zoneID from ##inputdb##.hotellingactivitydistribution;
            insert ignore into hotellingactivitydistribution
                select had_t.zoneID, fuelTypeID, beginModelYearID, endModelYearID, opModeID, opModeFraction 
                from ##defaultdb##.hotellingactivitydistribution had, hotellingactivitydistribution_tmp had_t
                where fuelTypeID <> 2; 
            drop table if exists hotellingactivitydistribution_tmp;
	        insert into convertTempMessages VALUES ('HotellingActivityDistribution data for diesel were transfered, and default values were imported for other fuel types.');
        end if;
	end if;
	
	-- hotellingagefraction: no schema changes and input data can be transfered as-is
    if (hotellingagefractionTableExists) then
		create table hotellingagefraction like ##defaultdb##.hotellingagefraction;
        insert ignore into hotellingagefraction select * from ##inputdb##.hotellingagefraction;
	end if;
	
	-- hotellinghourfraction: no schema changes and input data can be transfered as-is
    if (hotellinghourfractionTableExists) then
		create table hotellinghourfraction like ##defaultdb##.hotellinghourfraction;
        insert ignore into hotellinghourfraction select * from ##inputdb##.hotellinghourfraction;
	end if;
	
	-- hotellinghoursperday: no schema changes and input data can be transfered as-is
    if (hotellinghoursperdayTableExists) then
		create table hotellinghoursperday like ##defaultdb##.hotellinghoursperday;
        insert ignore into hotellinghoursperday select * from ##inputdb##.hotellinghoursperday;
	end if;
	
	-- hotellingmonthadjust: no schema changes and input data can be transfered as-is
    if (hotellingmonthadjustTableExists) then
		create table hotellingmonthadjust like ##defaultdb##.hotellingmonthadjust;
        insert ignore into hotellingmonthadjust select * from ##inputdb##.hotellingmonthadjust;
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
	
	-- idledayadjust: no schema changes and input data can be transfered as-is
    if (idledayadjustTableExists) then
		create table idledayadjust like ##defaultdb##.idledayadjust;
        insert ignore into idledayadjust select * from ##inputdb##.idledayadjust;
	end if;
	
	-- idlemodelyeargrouping: no schema changes and input data can be transfered as-is
    if (idlemodelyeargroupingTableExists) then
		create table idlemodelyeargrouping like ##defaultdb##.idlemodelyeargrouping;
        insert ignore into idlemodelyeargrouping select * from ##inputdb##.idlemodelyeargrouping;
	end if;
	
	-- idlemonthadjust: no schema changes and input data can be transfered as-is
    if (idlemonthadjustTableExists) then
		create table idlemonthadjust like ##defaultdb##.idlemonthadjust;
        insert ignore into idlemonthadjust select * from ##inputdb##.idlemonthadjust;
	end if;
	
	-- totalidlefraction: no schema changes and input data can be transfered as-is
    if (totalidlefractionTableExists) then
		create table totalidlefraction like ##defaultdb##.totalidlefraction;
        insert ignore into totalidlefraction select * from ##inputdb##.totalidlefraction;
	end if;

	-- imcoverage: if original input db values are based on MOVES defaults, user should clear this data in
    --             the new input db, export the new default values, and reimport them.
    if (imcoverageTableExists) then
		create table imcoverage like ##defaultdb##.imcoverage;
        insert ignore into imcoverage select * from ##inputdb##.imcoverage;
	    insert into convertTempMessages VALUES ('IMCoverage data transfered. If these were based on MOVES3 defaults, clear these data in the new input database,'),
                                               ('     export the MOVES4 defaults, and modify as necessary.');
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
	    insert into convertTempMessages VALUES ('SourceTypeAgeDistribution values transfered. If values for some source types in SourceTypeAgeDistribution are based'),
                                               ('     on MOVES3 defaults, export both the imported data and the MOVES4 defaults for this table. Then, overwrite the'),
                                               ('     MOVES3 default data with the MOVES4 defaults before reimporting.');
	end if;
    
	-- sourcetypedayvmt: no schema changes and input data can be transfered as-is
    if (sourcetypedayvmtTableExists) then
		create table sourcetypedayvmt like ##defaultdb##.sourcetypedayvmt;
        insert ignore into sourcetypedayvmt select * from ##inputdb##.sourcetypedayvmt;
	end if;
    
	-- sourcetypeyear: no schema changes and input data can be transfered as-is
    if (sourcetypeyearTableExists) then
		create table sourcetypeyear like ##defaultdb##.sourcetypeyear;
        insert ignore into sourcetypeyear select * from ##inputdb##.sourcetypeyear;
	end if;
    
	-- sourcetypeyearvmt: no schema changes and input data can be transfered as-is
    if (sourcetypeyearvmtTableExists) then
		create table sourcetypeyearvmt like ##defaultdb##.sourcetypeyearvmt;
        insert ignore into sourcetypeyearvmt select * from ##inputdb##.sourcetypeyearvmt;
	end if;

	-- starts: no schema changes and input data can be transfered as-is
    if (startsTableExists) then
		create table starts like ##defaultdb##.starts;
        insert ignore into starts select * from ##inputdb##.starts;
	end if;

	-- startsageadjustment: no schema changes and input data can be transfered as-is
    if (startsageadjustmentTableExists) then
		create table startsageadjustment like ##defaultdb##.startsageadjustment;
        insert ignore into startsageadjustment select * from ##inputdb##.startsageadjustment;
	end if;

	-- startshourfraction: no schema changes and input data can be transfered as-is
    if (startshourfractionTableExists) then
		create table startshourfraction like ##defaultdb##.startshourfraction;
        insert ignore into startshourfraction select * from ##inputdb##.startshourfraction;
	end if;

	-- startsmonthadjust: no schema changes and input data can be transfered as-is
    if (startsmonthadjustTableExists) then
		create table startsmonthadjust like ##defaultdb##.startsmonthadjust;
        insert ignore into startsmonthadjust select * from ##inputdb##.startsmonthadjust;
	end if;

	-- startsopmodedistribution: no schema changes and input data can be transfered as-is
    if (startsopmodedistributionTableExists) then
		create table startsopmodedistribution like ##defaultdb##.startsopmodedistribution;
        insert ignore into startsopmodedistribution select * from ##inputdb##.startsopmodedistribution;
	end if;

	-- startsperday: no schema changes and input data can be transfered as-is
    if (startsperdayTableExists) then
		create table startsperday like ##defaultdb##.startsperday;
        insert ignore into startsperday select * from ##inputdb##.startsperday;
	end if;

	-- startsperdaypervehicle: no schema changes and input data can be transfered as-is
    if (startsperdaypervehicleTableExists) then
		create table startsperdaypervehicle like ##defaultdb##.startsperdaypervehicle;
        insert ignore into startsperdaypervehicle select * from ##inputdb##.startsperdaypervehicle;
	end if;
	
	-- state: no schema changes and input data can be transfered as-is
    if (stateTableExists) then
		create table `state` like ##defaultdb##.`state`;
        insert ignore into `state` select * from ##inputdb##.`state`;
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
    
	-- zonemonthhour: this table has a schema change, but the changed columns are always null, so the input data can still be transfered
    if (zonemonthhourTableExists) then
		create table zonemonthhour like ##defaultdb##.zonemonthhour;
        insert ignore into zonemonthhour (monthID, zoneID, hourID, temperature, relHumidity)
		select monthID, zoneID, hourID, temperature, relHumidity from ##inputdb##.zonemonthhour;
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
    
end
EndBlock

call DBConverter();
drop procedure if exists DBConverter;