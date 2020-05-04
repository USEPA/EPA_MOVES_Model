-- --------------------------------------------------------------------------------------
-- Convert a MOVES2014 CDM or PDM database to MOVES2014A format.
-- No changes are made to the original input database.
-- Run this script in the context of a new database.
-- Populate ##defaultdb## with the name of the default database.
-- Populate ##inputdb## with the name of the original 2014 CDM or PDM database.
--
-- Author Wesley Faler
-- Version 2015-05-21
-- --------------------------------------------------------------------------------------

-- Copy tables in a 2014 database
create table if not exists avft like ##inputdb##.avft;
create table if not exists avgspeeddistribution like ##inputdb##.avgspeeddistribution;
create table if not exists county like ##inputdb##.county;
create table if not exists countyyear like ##inputdb##.countyyear;
create table if not exists dayvmtfraction like ##inputdb##.dayvmtfraction;
create table if not exists driveschedulesecondlink like ##inputdb##.driveschedulesecondlink;
create table if not exists fuelformulation like ##inputdb##.fuelformulation;
create table if not exists fuelsupply like ##inputdb##.fuelsupply;
create table if not exists fuelusagefraction like ##inputdb##.fuelusagefraction;
create table if not exists hotellingactivitydistribution like ##inputdb##.hotellingactivitydistribution;
create table if not exists hotellinghours like ##inputdb##.hotellinghours;
create table if not exists hourvmtfraction like ##inputdb##.hourvmtfraction;
create table if not exists hpmsvtypeyear like ##inputdb##.hpmsvtypeyear;
create table if not exists imcoverage like ##inputdb##.imcoverage;
create table if not exists importstartsopmodedistribution like ##inputdb##.importstartsopmodedistribution;
create table if not exists link like ##inputdb##.link;
create table if not exists linksourcetypehour like ##inputdb##.linksourcetypehour;
create table if not exists monthvmtfraction like ##inputdb##.monthvmtfraction;
create table if not exists offnetworklink like ##inputdb##.offnetworklink;
create table if not exists onroadretrofit like ##inputdb##.onroadretrofit;
create table if not exists opmodedistribution like ##inputdb##.opmodedistribution;
create table if not exists regioncounty like ##inputdb##.regioncounty;
create table if not exists roadtype like ##inputdb##.roadtype;
create table if not exists roadtypedistribution like ##inputdb##.roadtypedistribution;
create table if not exists sourcetypeagedistribution like ##inputdb##.sourcetypeagedistribution;
create table if not exists sourcetypeyear like ##inputdb##.sourcetypeyear;
create table if not exists starts like ##inputdb##.starts;
create table if not exists startshourfraction like ##inputdb##.startshourfraction;
create table if not exists startsmonthadjust like ##inputdb##.startsmonthadjust;
create table if not exists startsperday like ##inputdb##.startsperday;
create table if not exists startssourcetypefraction like ##inputdb##.startssourcetypefraction;
create table if not exists state like ##inputdb##.state;
create table if not exists year like ##inputdb##.year;
create table if not exists zone like ##inputdb##.zone;
create table if not exists zonemonthhour like ##inputdb##.zonemonthhour;
create table if not exists zoneroadtype like ##inputdb##.zoneroadtype;

-- Provide tables that are needed in MOVES2014A but may not have been in the 2014 database
create table if not exists HPMSVTypeDay like ##defaultdb##.HPMSVTypeDay;
create table if not exists SourceTypeDayVMT like ##defaultdb##.SourceTypeDayVMT;
create table if not exists SourceTypeYearVMT like ##defaultdb##.SourceTypeYearVMT;
create table if not exists nrfuelsupply like ##inputdb##.nrfuelsupply;

-- Copy data from the input database.
-- Errors here are ignored by the scripting engine and occur when a table
-- is not present in the input database.
insert ignore into avft select * from ##inputdb##.avft;
insert ignore into avgspeeddistribution select * from ##inputdb##.avgspeeddistribution;
insert ignore into county select * from ##inputdb##.county;
insert ignore into countyyear select * from ##inputdb##.countyyear;
insert ignore into dayvmtfraction select * from ##inputdb##.dayvmtfraction;
insert ignore into driveschedulesecondlink select * from ##inputdb##.driveschedulesecondlink;
insert ignore into fuelformulation select * from ##inputdb##.fuelformulation;
insert ignore into fuelsupply select * from ##inputdb##.fuelsupply;
insert ignore into fuelusagefraction select * from ##inputdb##.fuelusagefraction;
insert ignore into hotellingactivitydistribution select * from ##inputdb##.hotellingactivitydistribution;
insert ignore into hotellinghours select * from ##inputdb##.hotellinghours;
insert ignore into hourvmtfraction select * from ##inputdb##.hourvmtfraction;
insert ignore into hpmsvtypeyear select * from ##inputdb##.hpmsvtypeyear;
insert ignore into imcoverage select * from ##inputdb##.imcoverage;
insert ignore into importstartsopmodedistribution select * from ##inputdb##.importstartsopmodedistribution;
insert ignore into link select * from ##inputdb##.link;
insert ignore into linksourcetypehour select * from ##inputdb##.linksourcetypehour;
insert ignore into monthvmtfraction select * from ##inputdb##.monthvmtfraction;
insert ignore into offnetworklink select * from ##inputdb##.offnetworklink;
insert ignore into onroadretrofit select * from ##inputdb##.onroadretrofit;
insert ignore into opmodedistribution select * from ##inputdb##.opmodedistribution;
insert ignore into regioncounty select * from ##inputdb##.regioncounty;
insert ignore into roadtype select * from ##inputdb##.roadtype;
insert ignore into roadtypedistribution select * from ##inputdb##.roadtypedistribution;
insert ignore into sourcetypeagedistribution select * from ##inputdb##.sourcetypeagedistribution;
insert ignore into sourcetypeyear select * from ##inputdb##.sourcetypeyear;
insert ignore into starts select * from ##inputdb##.starts;
insert ignore into startshourfraction select * from ##inputdb##.startshourfraction;
insert ignore into startsmonthadjust select * from ##inputdb##.startsmonthadjust;
insert ignore into startsperday select * from ##inputdb##.startsperday;
insert ignore into startssourcetypefraction select * from ##inputdb##.startssourcetypefraction;
insert ignore into state select * from ##inputdb##.state;
insert ignore into year select * from ##inputdb##.year;
insert ignore into zone select * from ##inputdb##.zone;
insert ignore into zonemonthhour select * from ##inputdb##.zonemonthhour;
insert ignore into zoneroadtype select * from ##inputdb##.zoneroadtype;

insert ignore into HPMSVTypeDay select * from ##inputdb##.HPMSVTypeDay;
insert ignore into SourceTypeDayVMT select * from ##inputdb##.SourceTypeDayVMT;
insert ignore into SourceTypeYearVMT select * from ##inputdb##.SourceTypeYearVMT;
insert ignore into nrfuelsupply select * from ##inputdb##.nrfuelsupply;


-- Check for warning or error messages
drop procedure if exists spConvert2014CDMPDM;

BeginBlock
create procedure spConvert2014CDMPDM()
begin
	declare howMany int default 0;
	declare isCustomDomain int default 0;

	-- No actions are needed here
end
EndBlock

call spConvert2014CDMPDM();
drop procedure if exists spConvert2014CDMPDM;
