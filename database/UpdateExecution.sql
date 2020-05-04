-- Update utility tables in the MOVESExecution database.
-- Author Wesley Faler
-- Version 2013-09-30

-- --------------------------------------------------------------------
-- Create RegClassSourceTypeFraction
-- --------------------------------------------------------------------
drop table if exists runspecSourceTypeModelYearID;
create table if not exists runspecSourceTypeModelYearID (
	sourceTypeModelYearID int not null primary key
);

insert into runspecSourceTypeModelYearID (sourceTypeModelYearID)
select sourceTypeID*10000 + modelYearID
from runspecSourceType, runspecModelYear;

drop table if exists tempRegClassSourceTypeTotal;
create table if not exists tempRegClassSourceTypeTotal (
	fuelTypeID smallint not null,
	modelYearID smallint not null,
	sourceTypeID smallint not null,
	total double not null default 0,
	primary key (fuelTypeID, modelYearID, sourceTypeID)
);

insert into tempRegClassSourceTypeTotal (sourceTypeID, modelYearID, fuelTypeID, total)
select floor(svp.sourceTypeModelYearID/10000) as sourceTypeID, 
	mod(svp.sourceTypeModelYearID,10000) as modelYearID, 
	svp.fuelTypeID, 
	sum(svp.stmyFraction) as stmyfueltotal
from sampleVehiclePopulation svp
inner join runspecSourceTypeModelYearID using (sourceTypeModelYearID)
group by svp.sourceTypeModelYearID, svp.fuelTypeID
having sum(svp.stmyFraction) > 0;

drop table if exists tempRegClassSourceTypeFraction;
create table if not exists tempRegClassSourceTypeFraction (
	fuelTypeID smallint not null,
	modelYearID smallint not null,
	sourceTypeID smallint not null,
	regClassID smallint not null,
	regTotal double not null default 0,
	primary key (fuelTypeID, modelYearID, sourceTypeID, regClassID)
);

insert into tempRegClassSourceTypeFraction(sourceTypeID, modelYearID, fuelTypeID, regClassID, regTotal)
select floor(svp.sourceTypeModelYearID/10000) as sourceTypeID, 
	mod(svp.sourceTypeModelYearID,10000) as modelYearID, 
	svp.fuelTypeID, 
	svp.regClassID,
	sum(svp.stmyFraction) as regTotal
from sampleVehiclePopulation svp
inner join runspecSourceTypeModelYearID using (sourceTypeModelYearID)
group by svp.sourceTypeModelYearID, svp.fuelTypeID, svp.regClassID
having sum(svp.stmyFraction) > 0;

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
);

-- regClassFraction is fraction of a [source,fuel,modelyear] that a regclass covers.
insert into RegClassSourceTypeFraction (fuelTypeID, modelYearID, sourceTypeID, regClassID, regClassFraction)
select tst.fuelTypeID, tst.modelYearID, tst.sourceTypeID, tst.regClassID, regTotal/t.total
from tempRegClassSourceTypeFraction tst
inner join tempRegClassSourceTypeTotal t on (t.fuelTypeID=tst.fuelTypeID
	and t.modelYearID=tst.modelYearID
	and t.sourceTypeID=tst.sourceTypeID)
inner join runSpecSourceFuelType rs on (rs.sourceTypeID=tst.sourceTypeID and rs.fuelTypeID=tst.fuelTypeID);

drop table if exists tempRegClassSourceTypeFraction;
drop table if exists tempRegClassSourceTypeTotal;
-- --------------------------------------------------------------------
-- Done Creating RegClassSourceTypeFraction
-- --------------------------------------------------------------------
