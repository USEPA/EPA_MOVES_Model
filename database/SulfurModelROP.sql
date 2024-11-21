-- Sulfur Model
-- version 2016-10-04
-- authors Wesely Faler, Ed Campbell

-- subst ##sulfurInputTable## tempSulfurIR;
-- subst ##sulfurOutputTable## tempSulfurOR;

-- This is the form of the input table, named in ##sulfurInputTable##
-- drop table if exists ##sulfurInputTable##;
-- create table if not exists ##sulfurInputTable## (
--  	fuelTypeID int not null,
--  	fuelFormulationID int not null,
-- 		baseFuelFormulationID int not null,
-- 		polProcessID int not null,
--  	pollutantID int not null,
--  	processID int not null,
--  	modelYearGroupID int not null,
--  	minModelYearID int not null,
--  	maxModelYearID int not null,
--  	ageID int not null,
--  	ratioNoSulfur double
-- );

-- This is the form of the output table, named in ##sulfurOutputTable##
-- drop table if exists ##sulfurOutputTable##;
-- create table if not exists ##sulfurOutputTable## (
--  	fuelTypeID int not null,
--  	fuelFormulationID int not null,
--  	polProcessID int not null,
--  	pollutantID int not null,
--  	processID int not null,
--  	sourceTypeID int not null,
--  	modelYearID int not null,
--  	ageID int not null,
--  	ratio double null,
--  	ratioGPA double null,
--  	ratioNoSulfur double null
-- );

drop table if exists tempSulfurCalcs1;
create table if not exists tempSulfurCalcs1 (
	fuelTypeID int, 
	baseFuelFormulationID int,
	fuelFormulationID int, 
	polProcessID int, 
	pollutantID int, 
	processID int,
	modelYearID int, 
	ageID int, 
	m6emitterID int, 
	sourceTypeID int,
	sulfurCoeff double, 
	sulfurLevel double, 
	sulfurBasis double, 
	ratioNoSulfur double,
	sulfurGPAMax float,
	sulfShortTarget double, 
	sulfShort30 double,
	lowSulfurCoeff double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

drop table if exists tempSulfurCalcs2;
create table if not exists tempSulfurCalcs2 (
	fuelTypeID int, 
	baseFuelFormulationID int,
	fuelFormulationID int, 
	polProcessID int, 
	pollutantID int, 
	processID int,
	modelYearID int, 
	ageID int, 
	m6emitterID int, 
	sourceTypeID int,
	sulfurCoeff double, 
	sulfurLevel double, 
	sulfurBasis double, 
	ratioNoSulfur double,
	sulfurGPAMax float,
	sulfurIRFactor double,
	sulfShortTarget double, 
	sulfShort30 double,
	SulfShortAdj double, 
	sulfAdj2 double, 
	SulfIRR double,
	sulfurLongCoeff double,
	minSulfAdjust double,
	lowSulfurCoeff double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

drop table if exists tempSulfurCalcs3;
create table if not exists tempSulfurCalcs3 (
	fuelTypeID int, 
	baseFuelFormulationID int,
	fuelFormulationID int, 
	polProcessID int, 
	pollutantID int, 
	processID int,
	modelYearID int, 
	ageID int, 
	m6emitterID int, 
	sourceTypeID int,
	sulfurCoeff double, 
	sulfurLevel double, 
	sulfurBasis double, 
	ratioNoSulfur double,
	sulfurGPAMax float,
	sulfurIRFactor double,
	sulfShortTarget double, 
	sulfShort30 double,
	SulfShortAdj double, 
	sulfAdj2 double, 
	SulfIRR double,
	sulfurLongCoeff double,
	sulfMax double,
	sulfAdj3 double,
	sulfGPA1 double,
	ssulfGPA double,
	sulfGPA double,
	GPASulfadj double,
	minSulfAdjust double,
	lowSulfurCoeff double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

drop table if exists tempSulfurCalcs3High;
create table if not exists tempSulfurCalcs3High (
	fuelTypeID int, 
	baseFuelFormulationID int,
	fuelFormulationID int, 
	polProcessID int, 
	pollutantID int, 
	processID int,
	modelYearID int, 
	ageID int, 
	sourceTypeID int,
	sulfurCoeff double, 
	sulfurLevel double, 
	sulfurBasis double, 
	ratioNoSulfur double,
	sulfurGPAMax float,
	sulfurIRFactor double,
	sulfShortTarget double, 
	sulfShort30 double,
	SulfShortAdj double, 
	sulfAdj2 double, 
	SulfIRR double,
	sulfurLongCoeff double,
	sulfMax double,
	sulfAdj3 double,
	sulfGPA1 double,
	ssulfGPA double,
	sulfGPA double,
	GPASulfadj double,
	minSulfAdjust double,
	lowSulfurCoeff double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

drop table if exists tempSulfurCalcs3Normal;
create table if not exists tempSulfurCalcs3Normal (
	fuelTypeID int, 
	baseFuelFormulationID int,
	fuelFormulationID int, 
	polProcessID int, 
	pollutantID int, 
	processID int,
	modelYearID int, 
	ageID int, 
	sourceTypeID int,
	sulfurCoeff double, 
	sulfurLevel double, 
	sulfurBasis double, 
	ratioNoSulfur double,
	sulfurGPAMax float,
	sulfurIRFactor double,
	sulfShortTarget double, 
	sulfShort30 double,
	SulfShortAdj double, 
	sulfAdj2 double, 
	SulfIRR double,
	sulfurLongCoeff double,
	sulfMax double,
	sulfAdj3 double,
	sulfGPA1 double,
	ssulfGPA double,
	sulfGPA double,
	GPASulfadj double,
	minSulfAdjust double,
	lowSulfurCoeff double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

drop table if exists tempSulfurCalcs4;
create table if not exists tempSulfurCalcs4 (
	fuelTypeID int,
	fuelFormulationID int, 
	modelYearID int, 
	ratioNoSulfur double,
	pollutantID int,
	polProcessID int, 
	processID int,
	sourceTypeID int,
	ageID int,  
	baseFuelFormulationID int,
	sulfAdj3 double,
	GPAsulfAdj3	double,
	sulfAdj3Normal	double,
	sulfAdjHigh	double,
	GPASulfadjNormal double,
	GPASulfadjHigh	double,
	minSulfAdjust double,
	sulfurLevel double,
	lowSulfurCoeff double,
	sulfurBasis double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- LEFT JOINs should be used with M6SulfurCoeff, using a default value of 1.0 for sulfurLongCoeff
-- if a record is not found.

drop table if exists tempYear;
create table if not exists tempYear (
	year int not null primary key
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into tempYear(year) values(1950),(1951),(1952),(1953),(1954),(1955),(1956),(1957)
	,(1958),(1959),(1960),(1961),(1962),(1963),(1964),(1965),(1966),(1967)
	,(1968),(1969),(1970),(1971),(1972),(1973),(1974),(1975),(1976),(1977)
	,(1978),(1979),(1980),(1981),(1982),(1983),(1984),(1985),(1986),(1987)
	,(1988),(1989),(1990),(1991),(1992),(1993),(1994),(1995),(1996),(1997)
	,(1998),(1999),(2000),(2001),(2002),(2003),(2004),(2005),(2006),(2007)
	,(2008),(2009),(2010),(2011),(2012),(2013),(2014),(2015),(2016),(2017)
	,(2018),(2019),(2020),(2021),(2022),(2023),(2024),(2025),(2026),(2027)
	,(2028),(2029),(2030),(2031),(2032),(2033),(2034),(2035),(2036),(2037)
	,(2038),(2039),(2040),(2041),(2042),(2043),(2044),(2045),(2046),(2047)
	,(2048),(2049),(2050),(2051),(2052),(2053),(2054),(2055),(2056),(2057)
	,(2058),(2059),(2060)
;

drop table if exists tempSulfurBaseLookup;
create table if not exists tempSulfurBaseLookup (
	sulfurBasis int not null,
	modelYearID int not null,
	sulfurGPAMax float,
	sulfurBase float
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

drop table if exists tempSulfurCoeffLookup;
create table if not exists tempSulfurCoeffLookup (
	sulfurCoeff float,
	processID smallint,
	pollutantID smallint,
	M6EmitterID smallint,
	sourceTypeID smallint,
	modelYearID int not null,
	sulfurFunctionName char(10),
	sulfurFunctionID smallint,
	lowSulfurCoeff double
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

insert into tempSulfurBaseLookup (sulfurBasis, modelYearID, sulfurGPAMax, sulfurBase)
select distinct sulfurBasis, Year, sulfurGPAMax, sulfurBase
from tempYear
inner join sulfurBase on tempYear.year >= 
	CASE round(sulfurBase.modelYearGroupID / 10000,0) WHEN 0 THEN 1950 ELSE round(sulfurBase.modelYearGroupID / 10000,0) END
	and tempYear.year <= mod(sulfurBase.modelYearGroupID,10000)
inner join RunSpecModelYear on RunSpecModelYear.modelYearID = tempYear.year
;

alter table tempSulfurBaseLookup add key idxAll(sulfurBasis, modelYearID);
alter table tempSulfurBaseLookup add key idxAll2(modelYearID, sulfurBasis);

insert into tempSulfurCoeffLookup (sulfurCoeff, processID, pollutantID, M6EmitterID,
	sourceTypeID, modelYearID, sulfurFunctionName, sulfurFunctionID, lowSulfurCoeff
)
select sulfurCoeff, processID, pollutantID, sulfurModelCoeff.M6EmitterID,
	sulfurModelCoeff.sourceTypeID, year, sulfurFunctionName, sulfurModelCoeff.sulfurFunctionID, sulfurModelCoeff.lowSulfurCoeff
from tempYear
inner join sulfurModelCoeff on tempYear.year >= 
	CASE round(sulfurModelCoeff.fuelMyGroupID / 10000,0) WHEN 0 THEN 1950 ELSE round(sulfurModelCoeff.fuelMyGroupID / 10000,0) END 
	and tempYear.year <= mod(sulfurModelCoeff.fuelMyGroupID,10000)
inner join RunSpecModelYear on RunSpecModelYear.modelYearID = tempYear.year
inner join sulfurModelname on sulfurModelCoeff.M6EmitterID = sulfurModelname.M6EmitterID
	and sulfurModelCoeff.sulfurFunctionID = sulfurModelname.sulfurFunctionID
inner join RunSpecSourceType rsst on rsst.sourceTypeID = sulfurModelCoeff.sourceTypeID
;

alter table tempSulfurCoeffLookup add key(processID, pollutantID, modelYearID);

insert into tempSulfurCalcs1 (fuelTypeID, baseFuelFormulationID, fuelFormulationID, polProcessID, pollutantID, processID,
	modelYearID, ageID, m6emitterID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax,
	sulfShortTarget, sulfShort30, lowSulfurCoeff
)
select 
	IR.fuelTypeID, IR.baseFuelFormulationID, IR.fuelFormulationID, IR.polProcessID, IR.pollutantID, IR.processID,
	tempSulfurBaseLookup.modelYearID, IR.ageID, m6emitterID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax,
	CASE sulfurFunctionName WHEN 'log-log' THEN
			CASE WHEN sulfurLevel > 0 THEN exp(sulfurCoeff * LN(sulfurLevel)) ELSE 0 END
	ELSE
		exp(sulfurCoeff * sulfurLevel)
	END as sulfShortTarget,
	CASE sulfurFunctionName WHEN 'log-log' THEN
		exp(sulfurCoeff * LN(sulfurBasis))
	ELSE
		exp(sulfurCoeff * sulfurBasis)
	END as sulfShort30,
	lowSulfurCoeff
from ##sulfurInputTable## IR 
inner join tempSulfurCoeffLookup on 
	IR.processID = tempSulfurCoeffLookup.processID and IR.pollutantID = tempSulfurCoeffLookup.pollutantID
inner join fuelFormulation on IR.fuelFormulationID = fuelFormulation.fuelFormulationID
inner join tempSulfurBaseLookup on tempSulfurCoeffLookup.modelYearID = tempSulfurBaseLookup.modelYearID
	and IR.minModelYearID <= tempSulfurBaseLookup.modelYearID
	and IR.maxModelYearID >= tempSulfurBaseLookup.modelYearID
inner join RunSpecModelYearAge rsmya on rsmya.modelYearID = tempSulfurBaseLookup.modelYearID
	and rsmya.ageID = IR.ageID
;

alter table tempSulfurCalcs1 add key(pollutantID, modelYearID, fuelTypeID);

delete from M6SulfurCoeff where minModelYearID > ##cutoff.RateOfProgress##;
update M6SulfurCoeff set maxModelYearID=2060
where minModelYearID <= ##cutoff.RateOfProgress## and maxModelYearID >= ##cutoff.RateOfProgress## and maxModelYearID < 2060;

insert into tempSulfurCalcs2 (fuelTypeID, baseFuelFormulationID, fuelFormulationID, polProcessID, pollutantID, processID,
	modelYearID, ageID, m6emitterID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax, sulfurIRFactor,
	sulfShortTarget, sulfShort30, SulfShortAdj, sulfAdj2, SulfIRR, sulfurLongCoeff, lowSulfurCoeff
)
select 
	SC1.fuelTypeID, baseFuelFormulationID, SC1.fuelFormulationID, SC1.polProcessID, SC1.pollutantID, SC1.processID,
	SC1.modelYearID, ageID, SC1.m6emitterID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax, 
	CASE WHEN sulfurIRFactor is null THEN
		0
	ELSE
		CASE WHEN sulfurLevel <= maxIRFactorSulfur THEN
			sulfurIRFactor
		ELSE
			0
		END
	END as	
	sulfurIRFactor,
	sulfShortTarget, sulfShort30, 
	(sulfShortTarget - sulfShort30) / sulfShort30 as SulfShortAdj,
	((sulfShortTarget - sulfShort30) / sulfShort30) * ifnull(sulfurLongCoeff,1.0) as sulfAdj2,
	CASE WHEN (maxIRFactorSulfur is null or maxIRFactorSulfur <= 0) THEN -- or sulfurLevel < 30
		0
	WHEN sulfurLevel <= maxIRFactorSulfur THEN -- was <= maxIRFactorSulfur
		exp(sulfurCoeff * ln(maxIRFactorSulfur))
	ELSE
		CASE WHEN sulfurLevel > 0 THEN exp(sulfurCoeff * ln(sulfurLevel)) ELSE 0 END
	END as SulfIRR, 
	
	CASE WHEN sulfurLongCoeff is null THEN
		1.0
	ELSE
		sulfurLongCoeff
	END as
	sulfurLongCoeff,
	lowSulfurCoeff
from tempSulfurCalcs1 SC1
left join M6SulfurCoeff on 
	SC1.pollutantID = M6SulfurCoeff.pollutantID and
	SC1.modelYearID >= M6SulfurCoeff.minModelYearID and
	SC1.modelYearID <= M6SulfurCoeff.maxModelYearID
inner join sulfurCapAmount on SC1.fuelTypeID = sulfurCapAmount.fuelTypeID
;

-- 2010A:
-- 	(case when (tempSulfurBaseLookup.sulfurBase <= 30) then 0.85 else 0.50 end)

-- Revised internal:
--  (case when (tempSulfurBaseLookup.sulfurBase <= 30) then 0.40 else 0.40 end)

update tempSulfurCalcs2, tempSulfurBaseLookup set minSulfAdjust=
    (case when (tempSulfurBaseLookup.sulfurBase <= 30) then 0.85 else 0.50 end)
where tempSulfurBaseLookup.modelYearID=tempSulfurCalcs2.modelYearID;

insert into tempSulfurCalcs3 (fuelTypeID, baseFuelFormulationID, fuelFormulationID, polProcessID, pollutantID, processID,
	modelYearID, ageID, m6emitterID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax, sulfurIRFactor,
	sulfShortTarget, sulfShort30, SulfShortAdj, sulfAdj2, SulfIRR, sulfurLongCoeff,
	sulfMax, sulfAdj3, sulfGPA1, ssulfGPA, sulfGPA, GPASulfadj, minSulfAdjust, lowSulfurCoeff
)
select 
	SC2.fuelTypeID, baseFuelFormulationID, SC2.fuelFormulationID, SC2.polProcessID, SC2.pollutantID, SC2.processID,
	SC2.modelYearID, ageID, SC2.m6emitterID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax, sulfurIRFactor,
	sulfShortTarget, sulfShort30, SulfShortAdj, sulfAdj2, SulfIRR, sulfurLongCoeff,
	((sulfIRR - sulfShort30) / sulfShort30) as sulfMax,
	CASE WHEN 1.0 + (sulfurIRFactor * ((sulfIRR - sulfShort30) / sulfShort30) 
			+ (1.0 - sulfurIRFactor) * sulfAdj2) <= minSulfAdjust THEN
		minSulfAdjust
	ELSE
		1.0 + (sulfurIRFactor * ((sulfIRR - sulfShort30) / sulfShort30) 
		+ (1.0 - sulfurIRFactor) * sulfAdj2)
	END as sulfAdj3,
	CASE WHEN (ModelYearID in (-2004,-2005,-2006) and sulfurLevel >= 0) THEN
		((case when (SC2.pollutantID=3 and SC2.m6emitterID=2) then 0.60 else 1.0 end)*exp(sulfurCoeff * ln(sulfurGPAMax)))
	ELSE
		0
	END as sulfGPA1,
	CASE WHEN (ModelYearID in (-2004,-2005,-2006) and sulfurLevel >= 0) THEN
		-- NOx high emitters get sulfGPA1*0.6 and sulfShort30*0.6, which cancel and make no difference in the ratio here
		(exp(sulfurCoeff * ln(sulfurGPAMax)) - sulfShort30) / sulfShort30
	ELSE
		0
	END as ssulfGPA,
	CASE WHEN (ModelYearID in (-2004,-2005,-2006) and sulfurLevel >= 0) THEN
		-- NOx high emitters get sulfGPA1*0.6 and sulfShort30*0.6, which cancel and make no difference in the ratio here
		((exp(sulfurCoeff * ln(sulfurGPAMax)) - sulfShort30) / sulfShort30)
		* sulfurLongCoeff
	ELSE
		0
	END as sulfGPA,
	CASE WHEN (ModelYearID in (-2004,-2005,-2006) and sulfurLevel >= 0) THEN
		-- NOx high emitters get sulfGPA1*0.6 and sulfShort30*0.6, which cancel and make no difference in the ratio here
		1.0 + (sulfurIRFactor * (((exp(sulfurCoeff * ln(sulfurGPAMax)) - sulfShort30) / sulfShort30)
		* sulfurLongCoeff) + (1.0 - sulfurIRFactor) * sulfAdj2)
	ELSE
		CASE WHEN 1.0 + (sulfurIRFactor * ((sulfIRR - sulfShort30) / sulfShort30) 
				+ (1.0 - sulfurIRFactor) * sulfAdj2) <= minSulfAdjust THEN
			minSulfAdjust
		ELSE
			1.0 + (sulfurIRFactor * ((sulfIRR - sulfShort30) / sulfShort30) 
			+ (1.0 - sulfurIRFactor) * sulfAdj2)
		END
	END as GPASulfadj,
	minSulfAdjust,
	lowSulfurCoeff
from tempSulfurCalcs2 SC2
;

alter table tempSulfurCalcs3 add key(m6emitterID);

insert into tempSulfurCalcs3High (fuelTypeID, fuelFormulationID, baseFuelFormulationID, polProcessID, pollutantID, processID,
	modelYearID, ageID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax, sulfurIRFactor,
	sulfShortTarget, sulfShort30, SulfShortAdj, sulfAdj2, SulfIRR, sulfurLongCoeff,
	sulfMax, sulfAdj3, sulfGPA1, ssulfGPA, sulfGPA, GPASulfadj,minSulfAdjust,lowSulfurCoeff)
select 
	fuelTypeID, fuelFormulationID, baseFuelFormulationID, polProcessID, pollutantID, processID,
	modelYearID, ageID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax, sulfurIRFactor,
	sulfShortTarget, sulfShort30, SulfShortAdj, sulfAdj2, SulfIRR, sulfurLongCoeff,
	sulfMax, sulfAdj3, sulfGPA1, ssulfGPA, sulfGPA, GPASulfadj, minSulfAdjust, lowSulfurCoeff
from tempSulfurCalcs3 where m6emitterID = 2
;

insert into tempSulfurCalcs3Normal (fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,
	modelYearID, ageID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax, sulfurIRFactor,
	sulfShortTarget, sulfShort30, SulfShortAdj, sulfAdj2, SulfIRR, sulfurLongCoeff,
	sulfMax, sulfAdj3, sulfGPA1, ssulfGPA, sulfGPA, GPASulfadj, minSulfAdjust, lowSulfurCoeff)
select 
	fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,
	modelYearID, ageID, sourceTypeID,
	sulfurCoeff, sulfurLevel, sulfurBasis, ratioNoSulfur, sulfurGPAMax, sulfurIRFactor,
	sulfShortTarget, sulfShort30, SulfShortAdj, sulfAdj2, SulfIRR, sulfurLongCoeff,
	sulfMax, sulfAdj3, sulfGPA1, ssulfGPA, sulfGPA, GPASulfadj, minSulfAdjust, lowSulfurCoeff
from tempSulfurCalcs3 where m6emitterID = 1
;

alter table tempSulfurCalcs3High add key(fuelFormulationID, polProcessID, sourceTypeID, ageID, modelYearID);
alter table tempSulfurCalcs3Normal add key(fuelFormulationID, polProcessID, sourceTypeID, ageID, modelYearID);

insert into tempSulfurCalcs4 (fuelTypeID, fuelFormulationID, modelYearID, ratioNoSulfur, pollutantID,
	polProcessID, processID, sourceTypeID, ageID, baseFuelFormulationID, sulfAdj3,
	GPAsulfAdj3, sulfAdj3Normal, sulfAdjHigh, GPASulfadjNormal, GPASulfadjHigh, minSulfAdjust, lowSulfurCoeff, sulfurLevel, sulfurBasis)
select 
	tempSulfurCalcs3High.fuelTypeID, tempSulfurCalcs3High.fuelFormulationID, tempSulfurCalcs3High.modelYearID, tempSulfurCalcs3High.ratioNoSulfur, 
	tempSulfurCalcs3High.pollutantID, tempSulfurCalcs3High.polProcessID, tempSulfurCalcs3High.processID, tempSulfurCalcs3High.sourceTypeID, 
	tempSulfurCalcs3High.ageID, tempSulfurCalcs3High.baseFuelFormulationID,
	(1 - 0.5) * tempSulfurCalcs3Normal.sulfAdj3 + 0.5 * tempSulfurCalcs3High.sulfAdj3 as sulfAdj3,
	(1 - 0.5) * tempSulfurCalcs3Normal.GPASulfadj + 0.5 * tempSulfurCalcs3High.GPASulfadj as GPAsulfAdj3,
	tempSulfurCalcs3Normal.sulfAdj3 as sulfAdj3Normal,
	tempSulfurCalcs3High.sulfAdj3 as sulfAdjHigh,
	tempSulfurCalcs3Normal.GPASulfadj as GPASulfadjNormal,
	tempSulfurCalcs3High.GPASulfadj as GPASulfadjHigh,
	tempSulfurCalcs3Normal.minSulfAdjust as minSulfAdjust,
	tempSulfurCalcs3Normal.lowSulfurCoeff as lowSulfurCoeff,
	tempSulfurCalcs3Normal.sulfurLevel as sulfurLevel,
	tempSulfurCalcs3Normal.sulfurBasis as sulfurBasis
from tempSulfurCalcs3High
inner join tempSulfurCalcs3Normal on tempSulfurCalcs3High.fuelFormulationID = tempSulfurCalcs3Normal.fuelFormulationID and
	tempSulfurCalcs3High.polProcessID = tempSulfurCalcs3Normal.polProcessID and
	tempSulfurCalcs3High.modelYearID = tempSulfurCalcs3Normal.modelYearID and
	tempSulfurCalcs3High.ageID = tempSulfurCalcs3Normal.ageID and
	tempSulfurCalcs3High.sourceTypeID = tempSulfurCalcs3Normal.sourceTypeID;

alter table tempSulfurCalcs4 add key (fuelFormulationID, modelYearID, polProcessID, sourceTypeID, ageID);

insert ignore into ##sulfurOutputTable## (fuelTypeID, fuelFormulationID, polProcessID, pollutantID, processID,
sourceTypeID, modelYearID, ageID, ratio, ratioGPA, ratioNoSulfur)
select
	target.fuelTypeID, target.fuelFormulationID, target.polProcessID, target.pollutantID, 
	target.processID, target.sourceTypeID, target.modelYearID, target.ageID,
	CASE WHEN (target.lowSulfurCoeff is not null and target.sulfurLevel <= 30 and target.sulfurBasis=30) THEN
		greatest(1.0-target.lowSulfurCoeff*(30.0-target.sulfurLevel),0) * target.RatioNoSulfur
	ELSE
		CASE WHEN (target.modelYearID >= ##cutoff.sulfurModelTHCNOxStart## and target.modelYearID <= 12003 and target.pollutantID in (1, 3)) 
				or (target.modelYearID >= ##cutoff.sulfurModelCOStart## and target.pollutantID = 2) THEN
			greatest(target.sulfAdj3 / base.sulfAdj3, target.minSulfAdjust) * target.RatioNoSulfur
		ELSE
			CASE WHEN target.modelYearID >= 12004 and target.pollutantID in (1, 3) THEN
				greatest(target.sulfAdj3 / base.sulfAdj3, target.minSulfAdjust)
			ELSE
				1.0
			END
		END
	END as fuelAdjustment,
	CASE WHEN (target.lowSulfurCoeff is not null and target.sulfurLevel <= 30 and target.sulfurBasis=30) THEN
		greatest(1.0-target.lowSulfurCoeff*(30.0-target.sulfurLevel),0) * target.RatioNoSulfur
	ELSE
		CASE WHEN (target.modelYearID >= ##cutoff.sulfurModelTHCNOxStart## and target.modelYearID <= 12003 and target.pollutantID in (1, 3))
				or (target.modelYearID >= ##cutoff.sulfurModelCOStart## and target.pollutantID = 2) THEN
			greatest(target.sulfAdj3 / base.sulfAdj3, target.minSulfAdjust) * target.RatioNoSulfur
		ELSE
			CASE WHEN target.modelYearID >= 12004 and target.pollutantID in (1,3) THEN
				greatest(target.sulfAdj3/ base.sulfAdj3, target.minSulfAdjust)
			ELSE
				1.0
			END
		END
	END * 
	CASE WHEN (target.modelYearID IN (-2004,-2005,-2006) and (target.GPAsulfAdj3/ base.sulfAdj3) > 1.0) THEN
		greatest(target.GPAsulfAdj3/ base.sulfAdj3, target.minSulfAdjust)
	ELSE
		1.0
	END	as fuelAdjustmentGPA,
	target.ratioNoSulfur
from tempSulfurCalcs4 target
inner join tempSulfurCalcs4 base on base.fuelFormulationID = target.baseFuelFormulationID
	and base.modelYearID = target.modelYearID
	and base.polProcessID = target.polProcessID
	and base.sourceTypeID = target.sourceTypeID
	and base.ageID = target.ageID
;

drop table if exists debugSulfurOutputTable;

create table debugSulfurOutputTable
select
	target.fuelTypeID, target.fuelFormulationID, target.polProcessID, target.pollutantID, 
	target.processID, target.sourceTypeID, target.modelYearID, target.ageID,
	CASE WHEN (target.lowSulfurCoeff is not null and target.sulfurLevel <= 30 and target.sulfurBasis=30) THEN
		greatest(1.0-target.lowSulfurCoeff*(30.0-target.sulfurLevel),0) * target.RatioNoSulfur
	ELSE
		CASE WHEN (target.modelYearID >= ##cutoff.sulfurModelTHCNOxStart## and target.modelYearID <= 12003 and target.pollutantID in (1, 3)) 
				or (target.modelYearID >= ##cutoff.sulfurModelCOStart## and target.pollutantID = 2) THEN
			greatest(target.sulfAdj3 / base.sulfAdj3, target.minSulfAdjust) * target.RatioNoSulfur
		ELSE
			CASE WHEN target.modelYearID >= 12004 and target.pollutantID in (1, 3) THEN
				greatest(target.sulfAdj3 / base.sulfAdj3, target.minSulfAdjust)
			ELSE
				1.0
			END
		END
	END as fuelAdjustment,
	CASE WHEN (target.lowSulfurCoeff is not null and target.sulfurLevel <= 30 and target.sulfurBasis=30) THEN
		greatest(1.0-target.lowSulfurCoeff*(30.0-target.sulfurLevel),0) * target.RatioNoSulfur
	ELSE
		CASE WHEN (target.modelYearID >= ##cutoff.sulfurModelTHCNOxStart## and target.modelYearID <= 12003 and target.pollutantID in (1, 3))
				or (target.modelYearID >= ##cutoff.sulfurModelCOStart## and target.pollutantID = 2) THEN
			greatest(target.sulfAdj3 / base.sulfAdj3, target.minSulfAdjust) * target.RatioNoSulfur
		ELSE
			CASE WHEN target.modelYearID >= 12004 and target.pollutantID in (1,3) THEN
				greatest(target.sulfAdj3/ base.sulfAdj3, target.minSulfAdjust)
			ELSE
				1.0
			END
		END
	END * 
	CASE WHEN (target.modelYearID IN (-2004,-2005,-2006) and (target.GPAsulfAdj3/ base.sulfAdj3) > 1.0) THEN
		greatest(target.GPAsulfAdj3/ base.sulfAdj3, target.minSulfAdjust)
	ELSE
		1.0
	END	as fuelAdjustmentGPA,
	target.ratioNoSulfur,
	target.lowSulfurCoeff as target_lowSulfurCoeff,
	target.sulfurLevel as target_sulfurLevel,
	target.ratioNoSulfur as target_ratioNoSulfur,
	target.sulfAdj3 as target_sulfAdj3,
	base.sulfAdj3 as base_sulfAdj3,
	target.minSulfAdjust as target_minSulfAdjust,
	target.sulfurBasis as target_sulfurBasis
from tempSulfurCalcs4 target
inner join tempSulfurCalcs4 base on base.fuelFormulationID = target.baseFuelFormulationID
	and base.modelYearID = target.modelYearID
	and base.polProcessID = target.polProcessID
	and base.sourceTypeID = target.sourceTypeID
	and base.ageID = target.ageID
;
