-- Update utility tables in the MOVESExecution database.
-- Author Wesley Faler
-- Version 2016-03-15
--
-- Author Daniel Bizer-Cox
-- Version 2019-12-05

-- --------------------------------------------------------------------
-- Create RegClassSourceTypeFraction
-- --------------------------------------------------------------------
drop table if exists runspecSourceTypeModelYearID;
create table if not exists runspecSourceTypeModelYearID (
	sourceTypeModelYearID int not null primary key
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into runspecSourceTypeModelYearID (sourceTypeModelYearID)
select sourceTypeID*10000 + modelYearID
from runspecSourceType, runspecModelYear;

drop table if exists RegClassSourceTypeFraction;
create table if not exists RegClassSourceTypeFraction (
	fuelTypeID smallint not null,
	modelYearID smallint not null,
	sourceTypeID smallint not null,
	regClassID smallint not null,
	regClassFraction double not null default 0,
	primary key (fuelTypeID, modelYearID, sourceTypeID, regClassID),
	key (fuelTypeID),
	key (fuelTypeID, sourceTypeID),
	key (modelYearID, fuelTypeID, sourceTypeID),
	key (modelYearID),
	key (sourceTypeID, modelYearID, fuelTypeID)
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- regClassFraction is fraction of a [source type,fuel used,modelyear] that a regclass covers, (accounting for fuel type usage)
-- Fix for MTEST-92: this table was originally not accounting for fuel usage fraction
insert into RegClassSourceTypeFraction (fuelTypeID, modelYearID, sourceTypeID, regClassID, regClassFraction)
select fuf.fuelSupplyFuelTypeID, svp.modelYearID, svp.sourceTypeID, svp.regClassID, 
       sum(usageFraction * stmyfraction) / mystftFraction as regClassFraction
from fuelusagefraction fuf
join samplevehiclepopulation svp on (fuf.sourceBinFuelTypeID = svp.fuelTypeID)
join (
    select sourceTypeModelYearID, fuelSupplyFuelTypeID, modelYearID, sum(usageFraction * stmyfraction) as mystftFraction
    from fuelusagefraction fuf
    join samplevehiclepopulation svp on (fuf.sourceBinFuelTypeID = svp.fuelTypeID)
    inner join runspecSourceTypeModelYearID using (sourceTypeModelYearID)
    group by fuelSupplyFuelTypeID, sourceTypeModelYearID
    having mystftFraction <> 0
) as t1 on fuf.fuelSupplyFuelTypeID = t1.fuelSupplyFuelTypeID AND svp.sourceTypeModelYearID = t1.sourceTypeModelYearID
inner join runspecSourceTypeModelYearID rsstmy on svp.sourceTypeModelYearID = rsstmy.sourceTypeModelYearID
group by sourceTypeID, fuelSupplyFuelTypeID, modelYearID, regClassID
having regClassFraction <> 0 and regClassFraction is not null;
-- --------------------------------------------------------------------
-- Done Creating RegClassSourceTypeFraction
-- --------------------------------------------------------------------

-- --------------------------------------------------------------------
-- Add indexes that improve SourceUseTypePhysics
-- --------------------------------------------------------------------
alter table emissionRateByAge add key sutphys (polProcessID, opModeID);
alter table emissionRate add key sutphys (polProcessID, opModeID);

