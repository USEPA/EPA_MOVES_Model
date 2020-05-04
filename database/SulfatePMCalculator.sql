-- PM2.5 speciation calculator
-- Author Wesley Faler
-- Version 2014-07-15

-- @algorithm
-- @owner Sulfate PM Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

drop table if exists crankcaseSplit;
create table if not exists crankcaseSplit (
	processID smallint not null,
	pollutantID smallint not null,
	sourceTypeID smallint not null,
	fuelTypeID smallint not null,
	minModelYearID smallint not null,
	maxModelYearID smallint not null,
	crankcaseRatio double not null,
	primary key (pollutantID, sourceTypeID, fuelTypeID, minModelYearID, maxModelYearID, processID)
);
truncate table crankcaseSplit;

drop table if exists sPMOneCountyYearGeneralFuelRatio;
create table if not exists sPMOneCountyYearGeneralFuelRatio (
	fuelTypeID int not null,
	sourceTypeID int not null,
	monthID int not null,
	pollutantID int not null,
	processID int not null,
	modelYearID int not null,
	yearID int not null,
	fuelEffectRatio double not null default '0',
	primary key (fuelTypeID, sourceTypeID, monthID, pollutantID, modelYearID, yearID)
);
truncate table sPMOneCountyYearGeneralFuelRatio;

drop table if exists oneCountyYearSulfateFractions;
create table if not exists oneCountyYearSulfateFractions (
	processID smallint not null,
	fuelTypeID smallint not null,
	sourceTypeID smallint not null,
	monthID smallint not null,
	modelYearID smallint not null,
	SulfateNonECPMFraction double not null default '0',
	H2ONonECPMFraction double not null default '0',
	UnadjustedSulfatenonECPMFraction double not null default '0',
	UnadjustedH2ONonECPMFraction double not null default '0',
	primary key (processID, fuelTypeID, sourceTypeID, monthID, modelYearID)
);
truncate table oneCountyYearSulfateFractions;

drop table if exists oneZoneYearTemperatureFactor;
create table if not exists oneZoneYearTemperatureFactor (
	zoneID int not null,
	monthID smallint not null,
	hourID smallint not null,
	processID smallint not null,
	pollutantID smallint not null,
	fuelTypeID smallint not null,
	sourceTypeID smallint not null,
	minModelYearID smallint not null,
	maxModelYearID smallint not null,
	correctionFactor double not null,
	primary key (zoneID, monthID, hourID, processID, pollutantID, fuelTypeID, sourceTypeID, minModelYearID, maxModelYearID)
);
truncate table oneZoneYearTemperatureFactor;

##create.PMSpeciation##;
TRUNCATE TABLE PMSpeciation;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

-- @algorithm Get fuel effects for NonECNonSO4PM (120).
cache select gfr.fuelTypeID, gfr.sourceTypeID, may.monthID, gfr.pollutantID, gfr.processID, mya.modelYearID, mya.yearID,
	sum((ifnull(fuelEffectRatio,1)+GPAFract*(ifnull(fuelEffectRatioGPA,1)-ifnull(fuelEffectRatio,1)))*marketShare) as fuelEffectRatio
	INTO OUTFILE '##sPMOneCountyYearGeneralFuelRatio##'
from RunSpecMonthGroup rsmg
inner join RunSpecModelYearAge mya on (mya.yearID = ##context.year##)
inner join County c on (c.countyID = ##context.iterLocation.countyRecordID##)
inner join Year y on (y.yearID = mya.yearID)
inner join FuelSupply fs on (fs.fuelRegionID = ##context.fuelRegionID##
	and fs.fuelYearID = y.fuelYearID
	and fs.monthGroupID = rsmg.monthGroupID)
inner join MonthOfAnyYear may on (may.monthGroupID = fs.monthGroupID)
inner join RunSpecSourceFuelType rssf
inner join generalFuelRatio gfr on (gfr.fuelFormulationID = fs.fuelFormulationID
	and gfr.pollutantID in (120)
	and gfr.processID = ##context.iterProcess.databaseKey##
	and gfr.minModelYearID <= mya.modelYearID
	and gfr.maxModelYearID >= mya.modelYearID
	and gfr.minAgeID <= mya.ageID
	and gfr.maxAgeID >= mya.ageID
	and gfr.fuelTypeID = rssf.fuelTypeID
	and gfr.sourceTypeID = rssf.sourceTypeID)
group by gfr.fuelTypeID, gfr.sourceTypeID, may.monthID, gfr.pollutantID, gfr.processID, mya.modelYearID, mya.yearID
;

-- @algorithm Calculate adjusted SulfateNonECPMFraction and H2ONonECPMFraction 
-- using the sulfurLevel of available fuel formulations. Weight each adjusted fraction
-- by formulation market share.
cache select
	sf.processID,
	sf.fuelTypeID,
	sf.sourceTypeID,
	may.monthID,
	mya.modelYearID,
	sum(fs.marketShare * SulfatenonECPMFraction * (1 + BaseFuelSulfateFraction * ((coalesce(ff.sulfurLevel,0) / sf.BaseFuelSulfurLevel) - 1))) as SulfatenonECPMFraction,
	sum(fs.marketShare * H2ONonECPMFraction * (1 + BaseFuelSulfateFraction * ((coalesce(ff.sulfurLevel,0) / sf.BaseFuelSulfurLevel) - 1))) as H2ONonECPMFraction,
	SulfatenonECPMFraction as UnadjustedSulfatenonECPMFraction,
	H2ONonECPMFraction as UnadjustedH2ONonECPMFraction
	INTO OUTFILE '##oneCountyYearSulfateFractions##'
from RunSpecMonthGroup rsmg
inner join RunSpecModelYearAge mya on (mya.yearID = ##context.year##)
inner join County c on (c.countyID = ##context.iterLocation.countyRecordID##)
inner join Year y on (y.yearID = mya.yearID)
inner join FuelSupply fs on (fs.fuelRegionID = ##context.fuelRegionID##
	and fs.fuelYearID = y.fuelYearID
	and fs.monthGroupID = rsmg.monthGroupID)
inner join MonthOfAnyYear may on (may.monthGroupID = fs.monthGroupID)
inner join RunSpecSourceFuelType rssf
inner join FuelFormulation ff on (ff.fuelFormulationID = fs.fuelFormulationID)
inner join FuelSubtype fst on (
	fst.fuelSubtypeID = ff.fuelSubtypeID
	and fst.fuelTypeID = rssf.fuelTypeID)
inner join sulfateFractions sf on (
	sf.minModelYearID <= mya.modelYearID
	and sf.maxModelYearID >= mya.modelYearID
	and sf.fuelTypeID = rssf.fuelTypeID
	and sf.sourceTypeID = rssf.sourceTypeID)
group by 
	sf.processID,
	sf.fuelTypeID,
	sf.sourceTypeID,
	may.monthID,
	mya.modelYearID
order by null;

-- @algorithm Collect speciation data.
cache select *
	into outfile '##PMSpeciation##'
from PMSpeciation
where processID in (##primaryAndCrankcaseProcessIDs##)
and (outputPollutantID*100+processID) in (##polProcessIDs##);

-- @algorithm Create temperature effects for Sulfate (115), H2O (aersol) (119), and NonECNonSO4PM (120).
cache select zoneID, monthID, hourID, processID, pollutantID, fuelTypeID, sourceTypeID, minModelYearID, maxModelYearID,
	##context.temperatureFactorExpression##
	as correctionFactor
	INTO OUTFILE '##oneZoneYearTemperatureFactor##'
from zoneMonthHour zmh, temperatureFactorExpression tfe
where zmh.zoneID = ##context.iterLocation.zoneRecordID##
and tfe.minModelYearID <= ##context.year##
and tfe.maxModelYearID >= ##context.year## - 30
and tfe.processID = ##context.iterProcess.databaseKey##
and tfe.pollutantID in (115, 119, 120);

-- @algorithm Create crankcase split fractions for EC (112), Sulfate (115), H2O (aersol) (119), and NonECNonSO4PM (120).
-- The query must account for the lack of NonECNonSO4PM in the Pollutant table.
cache select processID, floor(r.polProcessID/100) as pollutantID, sourceTypeID, fuelTypeID,
	minModelYearID, maxModelYearID, crankcaseRatio
	INTO OUTFILE '##crankcaseSplit##'
from crankcaseEmissionRatio r, emissionProcess ep
where ep.processID in (##primaryAndCrankcaseProcessIDs##)
and r.polProcessID in (112*100 + ep.processID, 115*100 + ep.processID, 119*100 + ep.processID, 120*100 + ep.processID);

-- End Section Extract Data

-- Section Local Data Removal
-- End Section Local Data Removal

-- Section Processing

-- @algorithm
drop table if exists spmOutput;
create table spmOutput like MOVESWorkerOutput;

-- @algorithm Copy unadjusted EC (112) so it can be adjusted.
insert into spmOutput(MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate
from MOVESWorkerOutput mwo
where pollutantID=112;

-- @algorithm Remove unadjusted EC. The adjusted EC will be added later.
delete from MOVESWorkerOutput where pollutantID=112;

-- @algorithm
drop table if exists spmSplit1;
create table spmSplit1 (
	processID smallint not null,
	fuelTypeID smallint not null,
	sourceTypeID smallint not null,
	monthID smallint not null,
	modelYearID smallint not null,
	outputPollutantID smallint,
	conversionFraction double not null,
	primary key (processID, fuelTypeID, sourceTypeID, monthID, modelYearID, outputPollutantID)
);

-- @algorithm Specify the split to make sulfate (115) from NonECPM (118). Sulfate = NonECPM * SulfateNonECPMFraction.
insert into spmSplit1 (processID, fuelTypeID, sourceTypeID, monthID, modelYearID, outputPollutantID, conversionFraction)
select processID, fuelTypeID, sourceTypeID, monthID, modelYearID, 115 as outputPollutantID,
	SulfateNonECPMFraction as conversionFraction
from oneCountyYearSulfateFractions;

-- @algorithm Specify the split to make H2O (aerosol) (119) from NonECPM (118). H2O = NonECPM * H2ONonECPMFraction.
insert into spmSplit1 (processID, fuelTypeID, sourceTypeID, monthID, modelYearID, outputPollutantID, conversionFraction)
select processID, fuelTypeID, sourceTypeID, monthID, modelYearID, 119 as outputPollutantID,
	H2ONonECPMFraction as conversionFraction
from oneCountyYearSulfateFractions;

-- @algorithm Specify the split to make NonECNonSO4PM (120) from NonECPM (118). NonECNonSO4PM = NonECPM * (1 - UnadjustedH2ONonECPMFraction - UnadjustedSulfateNonECPMFraction).
insert into spmSplit1 (processID, fuelTypeID, sourceTypeID, monthID, modelYearID, outputPollutantID, conversionFraction)
select processID, fuelTypeID, sourceTypeID, monthID, modelYearID, 120 as outputPollutantID,
	greatest(1-UnadjustedH2ONonECPMFraction-UnadjustedSulfateNonECPMFraction,0) as conversionFraction
from oneCountyYearSulfateFractions;

-- @algorithm Apply the splits, making Sulfate (115), H2O (aerosol) (119), and NonECNonSO4PM (120) from NonECPM (118).
insert into spmOutput(MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select mwo.MOVESRunID,mwo.iterationID,
	mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,
	mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	s.outputPollutantID as pollutantID,mwo.processID,
	mwo.sourceTypeID,mwo.regClassID,mwo.fuelTypeID,mwo.modelYearID,
	mwo.roadTypeID,mwo.SCC,
	mwo.engTechID,mwo.sectorID,mwo.hpID,
	mwo.emissionQuant*s.conversionFraction as emissionQuant,
	mwo.emissionRate *s.conversionFraction as emissionRate
from MOVESWorkerOutput mwo
inner join spmSplit1 s on (
	s.processID = mwo.processID
	and s.fuelTypeID = mwo.fuelTypeID
	and s.sourceTypeID = mwo.sourceTypeID
	and s.monthID = mwo.monthID
	and s.modelYearID = mwo.modelYearID)
where pollutantID=118;

-- @algorithm Apply fuel effects. Only NonECNonSO4PM (120) is affected.
update spmOutput, sPMOneCountyYearGeneralFuelRatio set 
	emissionQuant=emissionQuant*fuelEffectRatio,
	emissionRate =emissionRate *fuelEffectRatio
where sPMOneCountyYearGeneralFuelRatio.fuelTypeID = spmOutput.fuelTypeID
and sPMOneCountyYearGeneralFuelRatio.sourceTypeID = spmOutput.sourceTypeID
and sPMOneCountyYearGeneralFuelRatio.monthID = spmOutput.monthID
and sPMOneCountyYearGeneralFuelRatio.pollutantID = spmOutput.pollutantID
and sPMOneCountyYearGeneralFuelRatio.processID = spmOutput.processID
and sPMOneCountyYearGeneralFuelRatio.modelYearID = spmOutput.modelYearID
and sPMOneCountyYearGeneralFuelRatio.yearID = spmOutput.yearID;

-- @algorithm Apply temperature effects to Sulfate, H2O (aersol), and NonECNonSO4PM.
update spmOutput, oneZoneYearTemperatureFactor set 
	emissionQuant=emissionQuant*correctionFactor,
	emissionRate =emissionRate *correctionFactor
where spmOutput.zoneID = oneZoneYearTemperatureFactor.zoneID
and spmOutput.monthID = oneZoneYearTemperatureFactor.monthID
and spmOutput.hourID = oneZoneYearTemperatureFactor.hourID
and spmOutput.processID = oneZoneYearTemperatureFactor.processID
and spmOutput.pollutantID = oneZoneYearTemperatureFactor.pollutantID
and spmOutput.fuelTypeID = oneZoneYearTemperatureFactor.fuelTypeID
and spmOutput.sourceTypeID = oneZoneYearTemperatureFactor.sourceTypeID
and spmOutput.modelYearID >= oneZoneYearTemperatureFactor.minModelYearID
and spmOutput.modelYearID <= oneZoneYearTemperatureFactor.maxModelYearID;

-- @algorithm
drop table if exists spmOutput2;
create table spmOutput2 like spmOutput;
alter table spmOutput2 add key polproc (pollutantID, processID);
alter table spmOutput2 add key procpol (processID, pollutantID);

-- @algorithm Split EC, Sulfate, H2O (aersol), and NonECNonSO4PM by crankcase effects.
insert into spmOutput2(MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select mwo.MOVESRunID,mwo.iterationID,
	mwo.yearID,mwo.monthID,mwo.dayID,mwo.hourID,
	mwo.stateID,mwo.countyID,mwo.zoneID,mwo.linkID,
	s.pollutantID as pollutantID,s.processID,
	mwo.sourceTypeID,mwo.regClassID,mwo.fuelTypeID,mwo.modelYearID,
	mwo.roadTypeID,mwo.SCC,
	mwo.engTechID,mwo.sectorID,mwo.hpID,
	mwo.emissionQuant*s.crankcaseRatio as emissionQuant,
	mwo.emissionRate *s.crankcaseRatio as emissionRate
from spmOutput mwo
inner join crankcaseSplit s on (
	s.pollutantID = mwo.pollutantID
	and s.fuelTypeID = mwo.fuelTypeID
	and s.sourceTypeID = mwo.sourceTypeID
	and s.minModelYearID <= mwo.modelYearID
	and s.maxModelYearID >= mwo.modelYearID);

-- Section MakePM2.5Total
-- @algorithm Sum EC, NonECNonSO4PM, Sulfate, and H2O (aerosol) to make Total PM 2.5 (110)
insert into MOVESWorkerOutput(MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	110 as pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	sum(emissionQuant),
	sum(emissionRate)
from spmOutput2
where pollutantID in (112,120,115,119)
and processID in (##primaryAndCrankcaseProcessIDsForPM25Total##)
group by 
	processID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID;
-- End Section MakePM2.5Total

-- @algorithm Copy EC, Sulfate, H2O to the output.
insert into MOVESWorkerOutput(MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate
from spmOutput2
where pollutantID in (112,115,119);

-- Note: To get NonECNonSO4PM in the output for debugging purposes, add 120 to the
-- list of pollutantIDs above.

-- @algorithm Remove unadjusted NonECPM (118).
delete from MOVESWorkerOutput where pollutantID=118 and processID in (##primaryAndCrankcaseProcessIDs##);

-- @algorithm Sum the adjusted NonECNonSO4PM, Sulfate, and H2O (aerosol) to make the adjusted NonECPM (118).
insert into MOVESWorkerOutput(MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	118 as pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	sum(emissionQuant),
	sum(emissionRate)
from spmOutput2
where pollutantID in (120,115,119)
and processID in (##primaryAndCrankcaseProcessIDs##)
group by 
	processID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID;

-- @algorithm Speciate the remaining pollutants. species output = pmSpeciationFraction * species input
insert into MOVESWorkerOutput(MOVESRunID,iterationID,
	yearID,monthID,dayID,hourID,
	stateID,countyID,zoneID,linkID,
	pollutantID,processID,
	sourceTypeID,regClassID,fuelTypeID,modelYearID,
	roadTypeID,SCC,
	engTechID,sectorID,hpID,
	emissionQuant,emissionRate)
select spm.MOVESRunID,spm.iterationID,
	spm.yearID,spm.monthID,spm.dayID,spm.hourID,
	spm.stateID,spm.countyID,spm.zoneID,spm.linkID,
	ps.outputPollutantID as pollutantID, spm.processID,
	spm.sourceTypeID,spm.regClassID,spm.fuelTypeID,spm.modelYearID,
	spm.roadTypeID,spm.SCC,
	spm.engTechID,spm.sectorID,spm.hpID,
	spm.emissionQuant * ps.pmSpeciationFraction as emissionQuant,
	spm.emissionRate  * ps.pmSpeciationFraction as emissionRate
from spmOutput2 spm
inner join PMSpeciation ps on (
	ps.processID = spm.processID
	and ps.inputPollutantID = spm.pollutantID
	and ps.sourceTypeID = spm.sourceTypeID
	and ps.fuelTypeID = spm.fuelTypeID
	and ps.minModelYearID <= spm.modelYearID
	and ps.maxModelYearID >= spm.modelYearID
);

-- End Section Processing

-- Section Cleanup

drop table if exists crankcaseSplit;
drop table if exists sPMOneCountyYearGeneralFuelRatio;
drop table if exists oneCountyYearSulfateFractions;
drop table if exists spmOutput;
drop table if exists spmOutput2;
drop table if exists spmSplit1;

-- End Section Cleanup
