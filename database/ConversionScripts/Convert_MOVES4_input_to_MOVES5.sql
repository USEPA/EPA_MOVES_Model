-- --------------------------------------------------------------------------------------
-- This script is run by the MOVES GUI to convert a MOVES4 County, Project, or Nonroad
-- input database to the MOVES5 format.
-- No changes are made to the original input database.
-- This script is run in the context of the new database (i.e., after it has been created
--     and the equivalent of a "USE my_new_database;" statement has been executed).
-- The MOVES GUI replaces ##defaultdb## with the name of the default database and
--     ##inputdb## with the name of the original MOVES4 CDM or PDM database.
-- Messages to be displayed to the user can be inserted into the convertTempMessages
--     table, which has one VARCHAR(1000) column. This table is managed by the MOVES GUI.
--
-- Last updated: 9/19/2024
-- --------------------------------------------------------------------------------------


drop procedure if exists DBConverter_agedisthelper;
BeginBlock
create procedure DBConverter_agedisthelper()
begin

-- create table that contains the maximum age by source type.
-- Ages beyond these ages will be assumed to have an ageFraction of 0
CREATE TABLE maxage_tmp (sourceTypeID INT, maxAgeID INT);
INSERT INTO maxage_tmp VALUES (11,52),(21,62),(31,57),(32,55),(41,51),(42,51),(43,34),(51,35),(52,56),(53,56),(54,44),(61,43),(62,39);

-- create temporary source type age distribution table to do our calculations in
CREATE TABLE sourcetypeagedistribution_tmp LIKE sourcetypeagedistribution;
INSERT INTO sourcetypeagedistribution_tmp SELECT * FROM sourcetypeagedistribution;

-- create temporary table to help add ages for interim calculations
CREATE TABLE ages_tmp (ageID INT);
INSERT INTO ages_tmp VALUES (31),(32),(33),(34),(35),(36),(37),(38),(39),(40),(41),(42),(43),(44),(45),(46),(47),(48),(49),(50),(51),(52),(53),(54),(55),(56),(57),(58),(59),(60),(61),(62),(63),(64),(65),(66),(67),(68),(69),(70);

-- create rows for ages 31-70 (no source type has ages over age 70)
INSERT INTO sourcetypeagedistribution_tmp
SELECT sourceTypeID, yearID, ages_tmp.ageID, 0 AS ageFraction
FROM sourcetypeagedistribution stad, ages_tmp
WHERE stad.ageID = 0;
ALTER TABLE sourcetypeagedistribution_tmp ORDER BY sourceTypeID, yearID, ageID;

-- create temporary column to hold our interim calculations
ALTER TABLE sourcetypeagedistribution_tmp ADD COLUMN interimAgeFraction DOUBLE;

-- no change for ageFractions associated with ageIDs 0-29
UPDATE sourcetypeagedistribution_tmp SET interimAgeFraction = ageFraction
WHERE ageID BETWEEN 0 AND 29;

-- Need to estimate ages 30-40. Input data's ageFraction for age 30 represents all ages 30+. Therefore, we will 
-- spread out the ageFraction for age 30 across ages 30-39, and put the remainder in age 40, which will represent
-- all ages 40.
-- For simplicity, we will estimate a linear trend for ages 30+. From our national analysis, we have average maximum 
-- vehicle age by source type. Using this, we will assume that all age fractions beyond the maximum age are 0.
-- This allows us to set two constraints on our linear trend:
--     * The ageFraction for MaxAge+1 must be 0
--     * The integral of the linear trend from [30, MaxAge] must equal the "tail" value (the original age 30 fraction)
-- To solve for the linear trend, we can use the geometric area of a triangle: A = 0.5*b*h 
--     * A is the area and therefore the "tail" value
--     * b is the number of ages that we are extending through, or MaxAge + 2 - 30
--     * h is the new value at age 30.
-- We need to calculate h before we can calculate the rest of the ages:
--     new ageID 30 fraction = original ageID 30 fraction * 2 / (MaxAge + 2 - 30)
UPDATE sourcetypeagedistribution_tmp stad_tmp, maxage_tmp
SET stad_tmp.interimAgeFraction = stad_tmp.ageFraction * 2 / ((maxAgeID+2) - 30)
WHERE stad_tmp.sourceTypeID = maxage_tmp.sourceTypeID 
  AND stad_tmp.ageID = 30;

-- Now that we have the new age 30 fraction, we can linearly interpolate between the following two points:
-- (x1=30, y1=new age 30 fraction) and (x2=maxAge+1, y2=0)
-- Linear interpolation form: (y1-y2)/(x1-x2) * (x-x1) + y1
-- new ageID [31+] fraction = (new ageID 30 fraction - 0) / (30-(maxAge+1)) * (ageID-30) + new ageID 30 fraction
UPDATE sourcetypeagedistribution_tmp stad_tmp, maxage_tmp,
       sourcetypeagedistribution_tmp age30
SET stad_tmp.interimAgeFraction = age30.interimAgeFraction/(30-(maxAgeID+1)) * (stad_tmp.ageID-30) + age30.interimAgeFraction
WHERE stad_tmp.sourceTypeID = maxage_tmp.sourceTypeID 
  AND stad_tmp.sourceTypeID = age30.sourceTypeID
  AND stad_tmp.ageID BETWEEN 30 AND maxAgeID
  AND age30.ageID = 30 
  AND stad_tmp.yearID = age30.yearID;

-- The above query will result in NULLs for source types that have a max ageID less than 40. Convert these to 0s.
UPDATE sourcetypeagedistribution_tmp stad_tmp
SET interimAgeFraction = COALESCE(interimAgeFraction, 0);

-- MOVES can only estimate up to age 40, so group the age40+ values into age 40 and then drop ages > 40
CREATE TABLE sourcetypeagedistribution_tmp40
	SELECT sourceTypeID, yearID, SUM(interimAgeFraction) AS age40
	FROM sourcetypeagedistribution_tmp
	WHERE ageID >= 40
	GROUP BY sourceTypeID, yearID;
UPDATE sourcetypeagedistribution_tmp stad_tmp, sourcetypeagedistribution_tmp40 stad_tmp40
SET stad_tmp.interimAgeFraction = stad_tmp40.age40
WHERE stad_tmp.sourceTypeID = stad_tmp40.sourceTypeID
  AND stad_tmp.yearID = stad_tmp40.yearID
  AND stad_tmp.ageID = 40;
DELETE FROM sourcetypeagedistribution_tmp
WHERE ageID > 40;

-- replace input database's version of sourcetypeagedistribution with ours
TRUNCATE TABLE sourcetypeagedistribution;
INSERT INTO sourcetypeagedistribution (sourceTypeID, yearID, ageID, ageFraction)
SELECT sourceTypeID, yearID, ageID, interimAgeFraction
FROM sourcetypeagedistribution_tmp;

-- drop long-haul age distributions and replace with default ones
DELETE FROM sourcetypeagedistribution WHERE sourceTypeID IN (53, 62);
INSERT INTO sourcetypeagedistribution
    SELECT * FROM ##defaultdb##.sourcetypeagedistribution
    WHERE sourceTypeID IN (53, 62) AND yearID IN (SELECT DISTINCT yearID FROM sourcetypeagedistribution);

-- order resulting table
ALTER TABLE sourcetypeagedistribution ORDER BY sourceTypeID, yearID, ageID;

-- clean up
DROP TABLE ages_tmp;
DROP TABLE maxage_tmp;
DROP TABLE sourcetypeagedistribution_tmp;
DROP TABLE sourcetypeagedistribution_tmp40;

end
EndBlock


drop procedure if exists DBConverter;

BeginBlock
create procedure DBConverter()
begin
	-- Initialize all "TableRows" variables to -1, and set them to count(*)
    -- If the variable is still -1, then that table doesn't need to be created / transferred.
	DECLARE auditlogTableRows INT DEFAULT -1;
	DECLARE avftTableRows INT DEFAULT -1;
	DECLARE avgspeeddistributionTableRows INT DEFAULT -1;
	DECLARE countyTableRows INT DEFAULT -1;
	DECLARE dayvmtfractionTableRows INT DEFAULT -1;
	DECLARE driveschedulesecondlinkTableRows INT DEFAULT -1;
	DECLARE fuelformulationTableRows INT DEFAULT -1;
	DECLARE fuelsupplyTableRows INT DEFAULT -1;
	DECLARE fuelusagefractionTableRows INT DEFAULT -1;
	DECLARE hotellingactivitydistributionTableRows INT DEFAULT -1;
	DECLARE hotellingagefractionTableRows INT DEFAULT -1;
	DECLARE hotellinghourfractionTableRows INT DEFAULT -1;
	DECLARE hotellinghoursperdayTableRows INT DEFAULT -1;
	DECLARE hotellingmonthadjustTableRows INT DEFAULT -1;
	DECLARE hourvmtfractionTableRows INT DEFAULT -1;
	DECLARE hpmsvtypedayTableRows INT DEFAULT -1;
	DECLARE hpmsvtypeyearTableRows INT DEFAULT -1;
	DECLARE idledayadjustTableRows INT DEFAULT -1;
	DECLARE idlemodelyeargroupingTableRows INT DEFAULT -1;
	DECLARE idlemonthadjustTableRows INT DEFAULT -1;
	DECLARE imcoverageTableRows INT DEFAULT -1;
	DECLARE linkTableRows INT DEFAULT -1;
	DECLARE linksourcetypehourTableRows INT DEFAULT -1;
	DECLARE offnetworklinkTableRows INT DEFAULT -1;
	DECLARE opmodedistributionTableRows INT DEFAULT -1;
	DECLARE monthvmtfractionTableRows INT DEFAULT -1;
	DECLARE onroadretrofitTableRows INT DEFAULT -1;
	DECLARE roadtypedistributionTableRows INT DEFAULT -1;
	DECLARE sourcetypeagedistributionTableRows INT DEFAULT -1;
	DECLARE sourcetypedayvmtTableRows INT DEFAULT -1;
	DECLARE sourcetypeyearTableRows INT DEFAULT -1;
	DECLARE sourcetypeyearvmtTableRows INT DEFAULT -1;
	DECLARE startsTableRows INT DEFAULT -1;
	DECLARE startsageadjustmentTableRows INT DEFAULT -1;
	DECLARE startshourfractionTableRows INT DEFAULT -1;
	DECLARE startsmonthadjustTableRows INT DEFAULT -1;
	DECLARE startsopmodedistributionTableRows INT DEFAULT -1;
	DECLARE startsperdayTableRows INT DEFAULT -1;
	DECLARE startsperdaypervehicleTableRows INT DEFAULT -1;
	DECLARE stateTableRows INT DEFAULT -1;
	DECLARE totalidlefractionTableRows INT DEFAULT -1;
	DECLARE yearTableRows INT DEFAULT -1;
	DECLARE zoneTableRows INT DEFAULT -1;
	DECLARE zonemonthhourTableRows INT DEFAULT -1;
	DECLARE zoneroadtypeTableRows INT DEFAULT -1;
	DECLARE nrbaseyearequippopulationTableRows INT DEFAULT -1;
	DECLARE nrdayallocationTableRows INT DEFAULT -1;
	DECLARE nrfuelsupplyTableRows INT DEFAULT -1;
	DECLARE nrgrowthindexTableRows INT DEFAULT -1;
	DECLARE nrgrowthpatternTableRows INT DEFAULT -1;
	DECLARE nrgrowthpatternfinderTableRows INT DEFAULT -1;
	DECLARE nrmonthallocationTableRows INT DEFAULT -1;
	DECLARE nrretrofitfactorsTableRows INT DEFAULT -1;
	DECLARE nrstatesurrogateTableRows INT DEFAULT -1;
	DECLARE nrusmonthallocationTableRows INT DEFAULT -1;
    DECLARE CONTINUE HANDLER FOR 1146 IF (FALSE) THEN SELECT 1; END IF; -- do nothing if error 1146 occurs (table doesn't exist)
    select count(*) into auditlogTableRows from ##inputdb##.auditlog;
    select count(*) into avftTableRows from ##inputdb##.avft;
    select count(*) into avgspeeddistributionTableRows from ##inputdb##.avgspeeddistribution;
    select count(*) into countyTableRows from ##inputdb##.county;
    select count(*) into dayvmtfractionTableRows from ##inputdb##.dayvmtfraction;
    select count(*) into driveschedulesecondlinkTableRows from ##inputdb##.driveschedulesecondlink;
    select count(*) into fuelformulationTableRows from ##inputdb##.fuelformulation;
    select count(*) into fuelsupplyTableRows from ##inputdb##.fuelsupply;
    select count(*) into fuelusagefractionTableRows from ##inputdb##.fuelusagefraction;
    select count(*) into hotellingactivitydistributionTableRows from ##inputdb##.hotellingactivitydistribution;
    select count(*) into hotellingagefractionTableRows from ##inputdb##.hotellingagefraction;
    select count(*) into hotellinghourfractionTableRows from ##inputdb##.hotellinghourfraction;
    select count(*) into hotellinghoursperdayTableRows from ##inputdb##.hotellinghoursperday;
    select count(*) into hotellingmonthadjustTableRows from ##inputdb##.hotellingmonthadjust;
    select count(*) into hourvmtfractionTableRows from ##inputdb##.hourvmtfraction;
    select count(*) into hpmsvtypedayTableRows from ##inputdb##.hpmsvtypeday;
    select count(*) into hpmsvtypeyearTableRows from ##inputdb##.hpmsvtypeyear;
    select count(*) into idledayadjustTableRows from ##inputdb##.idledayadjust;
    select count(*) into idlemodelyeargroupingTableRows from ##inputdb##.idlemodelyeargrouping;
    select count(*) into idlemonthadjustTableRows from ##inputdb##.idlemonthadjust;
    select count(*) into imcoverageTableRows from ##inputdb##.imcoverage;
    select count(*) into linkTableRows from ##inputdb##.link;
    select count(*) into linksourcetypehourTableRows from ##inputdb##.linksourcetypehour;
    select count(*) into offnetworklinkTableRows from ##inputdb##.offnetworklink;
    select count(*) into opmodedistributionTableRows from ##inputdb##.opmodedistribution;
    select count(*) into monthvmtfractionTableRows from ##inputdb##.monthvmtfraction;
    select count(*) into onroadretrofitTableRows from ##inputdb##.onroadretrofit;
    select count(*) into roadtypedistributionTableRows from ##inputdb##.roadtypedistribution;
    select count(*) into sourcetypeagedistributionTableRows from ##inputdb##.sourcetypeagedistribution;
    select count(*) into sourcetypedayvmtTableRows from ##inputdb##.sourcetypedayvmt;
    select count(*) into sourcetypeyearTableRows from ##inputdb##.sourcetypeyear;
    select count(*) into sourcetypeyearvmtTableRows from ##inputdb##.sourcetypeyearvmt;
    select count(*) into startsTableRows from ##inputdb##.starts;
    select count(*) into startsageadjustmentTableRows from ##inputdb##.startsageadjustment;
    select count(*) into startshourfractionTableRows from ##inputdb##.startshourfraction;
    select count(*) into startsmonthadjustTableRows from ##inputdb##.startsmonthadjust;
    select count(*) into startsopmodedistributionTableRows from ##inputdb##.startsopmodedistribution;
    select count(*) into startsperdayTableRows from ##inputdb##.startsperday;
    select count(*) into startsperdaypervehicleTableRows from ##inputdb##.startsperdaypervehicle;
    select count(*) into stateTableRows from ##inputdb##.state;
    select count(*) into totalidlefractionTableRows from ##inputdb##.totalidlefraction;
    select count(*) into yearTableRows from ##inputdb##.year;
    select count(*) into zoneTableRows from ##inputdb##.zone;
    select count(*) into zonemonthhourTableRows from ##inputdb##.zonemonthhour;
    select count(*) into zoneroadtypeTableRows from ##inputdb##.zoneroadtype;
    select count(*) into nrbaseyearequippopulationTableRows from ##inputdb##.nrbaseyearequippopulation;
    select count(*) into nrdayallocationTableRows from ##inputdb##.nrdayallocation;
    select count(*) into nrfuelsupplyTableRows from ##inputdb##.nrfuelsupply;
    select count(*) into nrgrowthindexTableRows from ##inputdb##.nrgrowthindex;
    select count(*) into nrgrowthpatternTableRows from ##inputdb##.nrgrowthpattern;
    select count(*) into nrgrowthpatternfinderTableRows from ##inputdb##.nrgrowthpatternfinder;
    select count(*) into nrmonthallocationTableRows from ##inputdb##.nrmonthallocation;
    select count(*) into nrretrofitfactorsTableRows from ##inputdb##.nrretrofitfactors;
    select count(*) into nrstatesurrogateTableRows from ##inputdb##.nrstatesurrogate;
    select count(*) into nrusmonthallocationTableRows from ##inputdb##.nrusmonthallocation;
    
	-- -----------------------------------
	-- Now, transfer all tables that exist
	-- -----------------------------------
    
    -- auditlog: always create schema so that the converted database appears in the GUI, but only transfer data if the
	--           table exists in the original input db. Note, create statement is generated at runtime since this table
	--           doesn't exist in the default database.
	##create_auditlog_table##
    if (auditlogTableRows >= 0) then
        insert ignore into auditlog select * from ##inputdb##.auditlog;
	end if;
    if (auditlogTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred auditlog table', '');
	end if;

	-- avft: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (avftTableRows >= 0) then
		create table avft like ##defaultdb##.avft;
	end if;
    if (avftTableRows > 0) then
        insert into convertTempMessages VALUES ('AVFT table not transferred. Use the AVFT Tool or export the defaults and modify as necessary.');
		insert into auditlog VALUES (NOW(), 'Converter', 'did not transfer AVFT table', 'Use the AVFT Tool or export the defaults and modify as necessary');
	end if;

	-- avgspeeddistribution: no schema changes and input data can be transferred as-is
    if (avgspeeddistributionTableRows >= 0) then
		create table avgspeeddistribution like ##defaultdb##.avgspeeddistribution;
        insert ignore into avgspeeddistribution select * from ##inputdb##.avgspeeddistribution;
	end if;
    if (avgspeeddistributionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred avgspeeddistribution table', '');
	end if;

	-- county: no schema changes and input data can be transferred as-is
    if (countyTableRows >= 0) then
		create table county like ##defaultdb##.county;
        insert ignore into county select * from ##inputdb##.county;
	end if;
    if (countyTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred county table', '');
	end if;

	-- dayvmtfraction: no schema changes, but we removed the requirement for roadTypeID 1 rows in this table, so do not include those when transfering data
    if (dayvmtfractionTableRows >= 0) then
		create table dayvmtfraction like ##defaultdb##.dayvmtfraction;
        insert ignore into dayvmtfraction select * from ##inputdb##.dayvmtfraction WHERE roadTypeID <> 1;
	end if;
    if (dayvmtfractionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred dayvmtfraction table', 'Ignored roadTypeID 1 values (not necessary for MOVES5)');
	end if;

	-- driveschedulesecondlink: no schema changes and input data can be transferred as-is
    if (driveschedulesecondlinkTableRows >= 0) then
		create table driveschedulesecondlink like ##defaultdb##.driveschedulesecondlink;
        insert ignore into driveschedulesecondlink select * from ##inputdb##.driveschedulesecondlink;
	end if;
    if (driveschedulesecondlinkTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred driveschedulesecondlink table', '');
	end if;

	-- fuelformulation: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (fuelformulationTableRows >= 0) then
		create table fuelformulation like ##defaultdb##.fuelformulation;
	end if;
    if (fuelformulationTableRows > 0) then
	    insert into convertTempMessages VALUES ('FuelFormulation table not transferred. Export the defaults and modify with the Fuels Wizard as necessary.');
		insert into auditlog VALUES (NOW(), 'Converter', 'did not transfer FuelFormulation table', 'Export the defaults and modify with the Fuels Wizard as necessary');
	end if;

	-- fuelsupply: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (fuelsupplyTableRows >= 0) then
		create table fuelsupply like ##defaultdb##.fuelsupply;
	end if;
    if (fuelsupplyTableRows > 0) then
	    insert into convertTempMessages VALUES ('FuelSupply table not transferred. Export the defaults and modify as necessary.');
		insert into auditlog VALUES (NOW(), 'Converter', 'did not transfer FuelSupply table', 'Export the defaults and modify as necessary');
	end if;

	-- fuelusagefraction: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (fuelusagefractionTableRows >= 0) then
		create table fuelusagefraction like ##defaultdb##.fuelusagefraction;
	end if;
    if (fuelusagefractionTableRows > 0) then
	    insert into convertTempMessages VALUES ('FuelusageFraction table not transferred. Export the defaults and modify as necessary.');
		insert into auditlog VALUES (NOW(), 'Converter', 'did not transfer FuelusageFraction table', 'Export the defaults and modify as necessary');
	end if;
	
	-- hotellingactivitydistribution: no schema changes. However, if data contain MY1960, extend back to MY1950
    if (hotellingactivitydistributionTableRows >= 0) then
		create table hotellingactivitydistribution like ##defaultdb##.hotellingactivitydistribution;
        insert ignore into hotellingactivitydistribution select * from ##inputdb##.hotellingactivitydistribution;
	end if;
    if (hotellingactivitydistributionTableRows > 0) then
        if ((select count(*) from hotellingactivitydistribution WHERE beginModelYearID = 1960) > 0) then
            update hotellingactivitydistribution set beginModelYearID = 1950 WHERE beginModelYearID = 1960;
		    insert into auditlog VALUES (NOW(), 'Converter', 'transferred hotellingactivitydistribution table', 'Updated beginModelYearID from 1960 to 1950');
        else
		    insert into auditlog VALUES (NOW(), 'Converter', 'transferred hotellingactivitydistribution table', '');
        end if;
	end if;
	
	-- hotellingagefraction: no schema changes, but the MOVES4 data are not compatible with MOVES5
    if (hotellingagefractionTableRows >= 0) then
		create table hotellingagefraction like ##defaultdb##.hotellingagefraction;
	end if;
    if (hotellingagefractionTableRows > 0) then
	    insert into convertTempMessages VALUES ('HotellingAgeFraction table not transferred. MOVES5 requires data that varies by age to include ages 0-40.');
		insert into auditlog VALUES (NOW(), 'Converter', 'could not transfer hotellingagefraction table', 'Data must be reanalyzed for MOVES5');
	end if;
	
	-- hotellinghourfraction: no schema changes and input data can be transferred as-is
    if (hotellinghourfractionTableRows >= 0) then
		create table hotellinghourfraction like ##defaultdb##.hotellinghourfraction;
        insert ignore into hotellinghourfraction select * from ##inputdb##.hotellinghourfraction;
	end if;
    if (hotellinghourfractionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred hotellinghourfraction table', '');
	end if;
	
	-- hotellinghoursperday: no schema changes and input data can be transferred as-is
    if (hotellinghoursperdayTableRows >= 0) then
		create table hotellinghoursperday like ##defaultdb##.hotellinghoursperday;
        insert ignore into hotellinghoursperday select * from ##inputdb##.hotellinghoursperday;
	end if;
    if (hotellinghoursperdayTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred hotellinghoursperday table', '');
	end if;
	
	-- hotellingmonthadjust: no schema changes and input data can be transferred as-is
    if (hotellingmonthadjustTableRows >= 0) then
		create table hotellingmonthadjust like ##defaultdb##.hotellingmonthadjust;
        insert ignore into hotellingmonthadjust select * from ##inputdb##.hotellingmonthadjust;
	end if;
    if (hotellingmonthadjustTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred hotellingmonthadjust table', '');
	end if;
	
	-- hourvmtfraction: no schema changes, but we removed the requirement for roadTypeID 1 rows in this table, so do not include those when transfering data
    if (hourvmtfractionTableRows >= 0) then
		create table hourvmtfraction like ##defaultdb##.hourvmtfraction;
        insert ignore into hourvmtfraction select * from ##inputdb##.hourvmtfraction WHERE roadTypeID <> 1;
	end if;
    if (hourvmtfractionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred hourvmtfraction table', 'Ignored roadTypeID 1 values (not necessary for MOVES5)');
	end if;

	-- hpmsvtypeday: no schema changes and input data can be transferred as-is
    if (hpmsvtypedayTableRows >= 0) then
		create table hpmsvtypeday like ##defaultdb##.hpmsvtypeday;
        insert ignore into hpmsvtypeday select * from ##inputdb##.hpmsvtypeday;
	end if;
    if (hpmsvtypedayTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred hpmsvtypeday table', '');
	end if;

	-- hpmsvtypeyear: no schema changes and input data can be transferred as-is
    if (hpmsvtypeyearTableRows >= 0) then
		create table hpmsvtypeyear like ##defaultdb##.hpmsvtypeyear;
        insert ignore into hpmsvtypeyear select * from ##inputdb##.hpmsvtypeyear;
	end if;
    if (hpmsvtypeyearTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred hpmsvtypeyear table', '');
	end if;
	
	-- idledayadjust: no schema changes and input data can be transferred as-is
    if (idledayadjustTableRows >= 0) then
		create table idledayadjust like ##defaultdb##.idledayadjust;
        insert ignore into idledayadjust select * from ##inputdb##.idledayadjust;
	end if;
    if (idledayadjustTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred idledayadjust table', '');
	end if;
	
	-- idlemodelyeargrouping: no schema changes. However, if data contain MY1960, extend back to MY1950
    if (idlemodelyeargroupingTableRows >= 0) then
		create table idlemodelyeargrouping like ##defaultdb##.idlemodelyeargrouping;
        insert ignore into idlemodelyeargrouping select * from ##inputdb##.idlemodelyeargrouping;
	end if;
    if (idlemodelyeargroupingTableRows > 0) then
        if ((select count(*) from idlemodelyeargrouping WHERE minModelYearID = 1960) > 0) then
            update idlemodelyeargrouping set minModelYearID = 1950 WHERE minModelYearID = 1960;
		    insert into auditlog VALUES (NOW(), 'Converter', 'transferred idlemodelyeargrouping table', 'Updated minModelYearID from 1960 to 1950');
        else
		    insert into auditlog VALUES (NOW(), 'Converter', 'transferred idlemodelyeargrouping table', '');
        end if;
	end if;
	
	-- idlemonthadjust: no schema changes and input data can be transferred as-is
    if (idlemonthadjustTableRows >= 0) then
		create table idlemonthadjust like ##defaultdb##.idlemonthadjust;
        insert ignore into idlemonthadjust select * from ##inputdb##.idlemonthadjust;
	end if;
    if (idlemonthadjustTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred idlemonthadjust table', '');
	end if;
	
	-- totalidlefraction: no schema changes. However, if data contain MY1960, extend back to MY1950
    if (totalidlefractionTableRows >= 0) then
		create table totalidlefraction like ##defaultdb##.totalidlefraction;
        insert ignore into totalidlefraction select * from ##inputdb##.totalidlefraction;
	end if;
    if (totalidlefractionTableRows > 0) then
        if ((select count(*) from totalidlefraction WHERE minModelYearID = 1960) > 0) then
            update totalidlefraction set minModelYearID = 1950 WHERE minModelYearID = 1960;
		    insert into auditlog VALUES (NOW(), 'Converter', 'transferred totalidlefraction table', 'Updated minModelYearID from 1960 to 1950');
        else
		    insert into auditlog VALUES (NOW(), 'Converter', 'transferred totalidlefraction table', '');
        end if;
	end if;

	-- imcoverage: if original input db values are based on MOVES defaults, user should clear this data in
    --             the new input db, export the new default values, and reimport them.
    --             Note: the converter updates programs that apply to MY1960 to apply to MY1950 as well.
    if (imcoverageTableRows >= 0) then
		create table imcoverage like ##defaultdb##.imcoverage;
        insert ignore into imcoverage select * from ##inputdb##.imcoverage;
	end if;
    if (imcoverageTableRows > 0) then
        update imcoverage set begModelYearID = 1950 WHERE begModelYearID = 1960;
	    insert into convertTempMessages VALUES ('IMCoverage data transferred. If these were based on MOVES4 defaults, clear these data in the new input'),
                                               ('     database, export the MOVES5 defaults, and modify as necessary.');
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred imcoverage table', 'Additional work may be necessary. See converter help for more information');
	end if;

	-- link: no schema changes and input data can be transferred as-is
    if (linkTableRows >= 0) then
		create table link like ##defaultdb##.link;
        insert ignore into link select * from ##inputdb##.link;
	end if;
    if (linkTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred link table', '');
	end if;

	-- linksourcetypehour: no schema changes and input data can be transferred as-is
    if (linksourcetypehourTableRows >= 0) then
		create table linksourcetypehour like ##defaultdb##.linksourcetypehour;
        insert ignore into linksourcetypehour select * from ##inputdb##.linksourcetypehour;
	end if;
    if (linksourcetypehourTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred linksourcetypehour table', '');
	end if;

	-- offnetworklink: no schema changes and input data can be transferred as-is
    if (offnetworklinkTableRows >= 0) then
		create table offnetworklink like ##defaultdb##.offnetworklink;
        insert ignore into offnetworklink select * from ##inputdb##.offnetworklink;
	end if;
    if (offnetworklinkTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred offnetworklink table', '');
	end if;

	-- opmodedistribution: no schema changes and input data can be transferred as-is
    if (opmodedistributionTableRows >= 0) then
		create table opmodedistribution like ##defaultdb##.opmodedistribution;
        insert ignore into opmodedistribution select * from ##inputdb##.opmodedistribution;
	end if;
    if (opmodedistributionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred opmodedistribution table', '');
	end if;

	-- monthvmtfraction: no schema changes and input data can be transferred as-is
    if (monthvmtfractionTableRows >= 0) then
		create table monthvmtfraction like ##defaultdb##.monthvmtfraction;
        insert ignore into monthvmtfraction select * from ##inputdb##.monthvmtfraction;
	end if;
    if (monthvmtfractionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred monthvmtfraction table', '');
	end if;

	-- onroadretrofit: no schema changes and input data can be transferred as-is
    if (onroadretrofitTableRows >= 0) then
		create table onroadretrofit like ##defaultdb##.onroadretrofit;
        insert ignore into onroadretrofit select * from ##inputdb##.onroadretrofit;
	end if;
    if (onroadretrofitTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred onroadretrofit table', '');
	end if;

	-- roadtypedistribution: no schema changes and input data can be transferred as-is
    if (roadtypedistributionTableRows >= 0) then
		create table roadtypedistribution like ##defaultdb##.roadtypedistribution;
        insert ignore into roadtypedistribution select * from ##inputdb##.roadtypedistribution;
	end if;
    if (roadtypedistributionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred roadtypedistribution table', '');
	end if;

	-- sourcetypeagedistribution: need to estimate age fractions for ages 30-40 from the original age 30 fraction.
	if (sourcetypeagedistributionTableRows >= 0) then
		create table sourcetypeagedistribution like ##defaultdb##.sourcetypeagedistribution;
        insert ignore into sourcetypeagedistribution select * from ##inputdb##.sourcetypeagedistribution;
	end if;
    if (sourcetypeagedistributionTableRows > 0) then
        call DBConverter_agedisthelper();
	    insert into convertTempMessages VALUES ('SourceTypeAgeDistribution values transferred. Age fractions for 30-40 estimated based on the provided'),
                                               ('     fractions for age 30. Note that the MOVES5 default age distributions for long-haul source types (53 and 62)'),
                                               ('     were used instead transfering over the data from the source database.');
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred SourceTypeAgeDistribution table', 'See converter help for more information regarding this table');
	end if;
    
	-- sourcetypedayvmt: no schema changes and input data can be transferred as-is
    if (sourcetypedayvmtTableRows >= 0) then
		create table sourcetypedayvmt like ##defaultdb##.sourcetypedayvmt;
        insert ignore into sourcetypedayvmt select * from ##inputdb##.sourcetypedayvmt;
	end if;
    if (sourcetypedayvmtTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred sourcetypedayvmt table', '');
	end if;
    
	-- sourcetypeyear: no schema changes and input data can be transferred as-is
    if (sourcetypeyearTableRows >= 0) then
		create table sourcetypeyear like ##defaultdb##.sourcetypeyear;
        insert ignore into sourcetypeyear select * from ##inputdb##.sourcetypeyear;
	end if;
    if (sourcetypeyearTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred sourcetypeyear table', '');
	end if;
    
	-- sourcetypeyearvmt: no schema changes and input data can be transferred as-is
    if (sourcetypeyearvmtTableRows >= 0) then
		create table sourcetypeyearvmt like ##defaultdb##.sourcetypeyearvmt;
        insert ignore into sourcetypeyearvmt select * from ##inputdb##.sourcetypeyearvmt;
	end if;
    if (sourcetypeyearvmtTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred sourcetypeyearvmt table', '');
	end if;

	-- starts: no schema changes, but the MOVES4 data are not compatible with MOVES5
    if (startsTableRows >= 0) then
		create table starts like ##defaultdb##.starts;
	end if;
    if (startsTableRows > 0) then
    	insert into convertTempMessages VALUES ('Starts table not transferred. MOVES5 requires data that varies by age to include ages 0-40.');
		insert into auditlog VALUES (NOW(), 'Converter', 'could not transfer starts table', 'Data must be reanalyzed for MOVES5');
	end if;

	-- startsageadjustment: no schema changes, but the MOVES4 data are not compatible with MOVES5
    if (startsageadjustmentTableRows >= 0) then
		create table startsageadjustment like ##defaultdb##.startsageadjustment;
	end if;
    if (startsageadjustmentTableRows > 0) then
    	insert into convertTempMessages VALUES ('StartsAgeAdjustment table not transferred. MOVES5 requires data that varies by age to include ages 0-40.');
		insert into auditlog VALUES (NOW(), 'Converter', 'could not transfer startsageadjustment table', 'Data must be reanalyzed for MOVES5');
	end if;

	-- startshourfraction: no schema changes and input data can be transferred as-is
    if (startshourfractionTableRows >= 0) then
		create table startshourfraction like ##defaultdb##.startshourfraction;
        insert ignore into startshourfraction select * from ##inputdb##.startshourfraction;
	end if;
    if (startshourfractionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred startshourfraction table', '');
	end if;

	-- startsmonthadjust: no schema changes and input data can be transferred as-is
    if (startsmonthadjustTableRows >= 0) then
		create table startsmonthadjust like ##defaultdb##.startsmonthadjust;
        insert ignore into startsmonthadjust select * from ##inputdb##.startsmonthadjust;
	end if;
    if (startsmonthadjustTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred startsmonthadjust table', '');
	end if;

	-- startsopmodedistribution: no schema changes. Need to estimate soak distributions for ages 31-40
    --                           based on age 30
    if (startsopmodedistributionTableRows >= 0) then
		create table startsopmodedistribution like ##defaultdb##.startsopmodedistribution;
        insert ignore into startsopmodedistribution select * from ##inputdb##.startsopmodedistribution;
        insert ignore into startsopmodedistribution 
            select dayID, hourID, sourceTypeID, ac.ageID, opModeID, opModeFraction, isUserInput
            from startsopmodedistribution sod, ##defaultdb##.agecategory ac
            where sod.ageID = 30 and ac.ageID between 31 and 40;
	end if;
    if (startsopmodedistributionTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred startsopmodedistribution table', 'duplicated age 30 soak distributions for ages 31-40');
	end if;

	-- startsperday: no schema changes and input data can be transferred as-is
    if (startsperdayTableRows >= 0) then
		create table startsperday like ##defaultdb##.startsperday;
        insert ignore into startsperday select * from ##inputdb##.startsperday;
	end if;
    if (startsperdayTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred startsperday table', '');
	end if;

	-- startsperdaypervehicle: no schema changes and input data can be transferred as-is
    if (startsperdaypervehicleTableRows >= 0) then
		create table startsperdaypervehicle like ##defaultdb##.startsperdaypervehicle;
        insert ignore into startsperdaypervehicle select * from ##inputdb##.startsperdaypervehicle;
	end if;
    if (startsperdaypervehicleTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred startsperdaypervehicle table', '');
	end if;
	
	-- state: no schema changes and input data can be transferred as-is
    if (stateTableRows >= 0) then
		create table `state` like ##defaultdb##.`state`;
        insert ignore into `state` select * from ##inputdb##.`state`;
	end if;
    if (stateTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred state table', '');
	end if;
    
	-- year: no schema changes and input data can be transferred as-is
    if (yearTableRows >= 0) then
		create table year like ##defaultdb##.year;
        insert ignore into year select * from ##inputdb##.year;
	end if;
    if (yearTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred year table', '');
	end if;
    
	-- zone: no schema changes and input data can be transferred as-is
    if (zoneTableRows >= 0) then
		create table zone like ##defaultdb##.zone;
        insert ignore into zone select * from ##inputdb##.zone;
	end if;
    if (zoneTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred zone table', '');
	end if;
    
	-- zonemonthhour: no schema changes and input data can be transferred as-is
    if (zonemonthhourTableRows >= 0) then
		create table zonemonthhour like ##defaultdb##.zonemonthhour;
        insert ignore into zonemonthhour select * from ##inputdb##.zonemonthhour;
	end if;
    if (zonemonthhourTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred zonemonthhour table', '');
	end if;
    
	-- zoneroadtype: no schema changes and input data can be transferred as-is
    if (zoneroadtypeTableRows >= 0) then
		create table zoneroadtype like ##defaultdb##.zoneroadtype;
        insert ignore into zoneroadtype select * from ##inputdb##.zoneroadtype;
	end if;
    if (zoneroadtypeTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred zoneroadtype table', '');
	end if;
    
	-- nrbaseyearequippopulation: no schema changes and input data can be transferred as-is
    if (nrbaseyearequippopulationTableRows >= 0) then
		create table nrbaseyearequippopulation like ##defaultdb##.nrbaseyearequippopulation;
        insert ignore into nrbaseyearequippopulation select * from ##inputdb##.nrbaseyearequippopulation;
	end if;
    if (nrbaseyearequippopulationTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred nrbaseyearequippopulation table', '');
	end if;
    
	-- nrdayallocation: no schema changes and input data can be transferred as-is
    if (nrdayallocationTableRows >= 0) then
		create table nrdayallocation like ##defaultdb##.nrdayallocation;
        insert ignore into nrdayallocation select * from ##inputdb##.nrdayallocation;
	end if;
    if (nrdayallocationTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred nrdayallocation table', '');
	end if;

	-- nrfuelsupply: do not transfer data from original input db, as user should export defaults and modify as necessary
    if (nrfuelsupplyTableRows >= 0) then
		create table nrfuelsupply like ##defaultdb##.nrfuelsupply;
	end if;
    if (nrfuelsupplyTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'did not transfer nrfuelsupply table', 'Export the defaults and modify as necessary');
	end if;
    
	-- nrgrowthindex: no schema changes and input data can be transferred as-is
    if (nrgrowthindexTableRows >= 0) then
		create table nrgrowthindex like ##defaultdb##.nrgrowthindex;
        insert ignore into nrgrowthindex select * from ##inputdb##.nrgrowthindex;
	end if;
    if (nrgrowthindexTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred nrgrowthindex table', '');
	end if;
    
	-- nrgrowthpattern: no schema changes and input data can be transferred as-is
    if (nrgrowthpatternTableRows >= 0) then
		create table nrgrowthpattern like ##defaultdb##.nrgrowthpattern;
        insert ignore into nrgrowthpattern select * from ##inputdb##.nrgrowthpattern;
	end if;
    if (nrgrowthpatternTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred nrgrowthpattern table', '');
	end if;
    
	-- nrgrowthpatternfinder: no schema changes and input data can be transferred as-is
    if (nrgrowthpatternfinderTableRows >= 0) then
		create table nrgrowthpatternfinder like ##defaultdb##.nrgrowthpatternfinder;
        insert ignore into nrgrowthpatternfinder select * from ##inputdb##.nrgrowthpatternfinder;
	end if;
    if (nrgrowthpatternfinderTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred nrgrowthpatternfinder table', '');
	end if;
    
	-- nrmonthallocation: no schema changes and input data can be transferred as-is
    if (nrmonthallocationTableRows >= 0) then
		create table nrmonthallocation like ##defaultdb##.nrmonthallocation;
        insert ignore into nrmonthallocation select * from ##inputdb##.nrmonthallocation;
	end if;
    if (nrmonthallocationTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred nrmonthallocation table', '');
	end if;
    
	-- nrretrofitfactors: no schema changes and input data can be transferred as-is
    if (nrretrofitfactorsTableRows >= 0) then
		create table nrretrofitfactors like ##defaultdb##.nrretrofitfactors;
        insert ignore into nrretrofitfactors select * from ##inputdb##.nrretrofitfactors;
	end if;
    if (nrretrofitfactorsTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred nrretrofitfactors table', '');
	end if;
    
	-- nrstatesurrogate: no schema changes and input data can be transferred as-is
    if (nrstatesurrogateTableRows >= 0) then
		create table nrstatesurrogate like ##defaultdb##.nrstatesurrogate;
        insert ignore into nrstatesurrogate select * from ##inputdb##.nrstatesurrogate;
	end if;
    if (nrstatesurrogateTableRows > 0) then
		insert into auditlog VALUES (NOW(), 'Converter', 'transferred nrstatesurrogate table', '');
	end if;
    
end
EndBlock

call DBConverter();
drop procedure if exists DBConverter;
drop procedure if exists DBConverter_agedisthelper;