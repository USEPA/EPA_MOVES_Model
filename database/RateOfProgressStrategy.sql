-- Remove effects of the Clean Air Act by propagating 1993 emission rates into the future.
-- Most rates are left untouched, instead population information is altered future vehicles
-- use the model year groups that include 1993, thus tieing them to 1993 emissions.
--
-- Author Wesley Faler
-- Version 2016-10-04

drop procedure if exists spDoRateOfProgress;

BeginBlock
create procedure spDoRateOfProgress()
begin
	-- Mode 0 is run after importing
	-- Mode 1 is run to check overall success/failure
	declare mode int default ##mode##;
	declare isOk int default 1;
	declare howMany int default 0;
	declare cutPoint int default 1993;
	declare cutPointFuelYear int default 1990;
	declare fuelYearToReplace int default 1990;

	select modelYearID into cutPoint
	from modelYearCutPoints
	where cutPointName='RateOfProgress';

	set cutPoint=ifnull(cutPoint,1993);

	select fuelYearID into cutPointFuelYear
	from year
	where yearID=(select max(yearID) from year where yearID <= cutPoint);

	set cutPointFuelYear=ifnull(cutPointFuelYear,1990);

	-- Update the calendar year's fuel year. This has a required ripple effect
	-- upon the FuelSupply and FuelUsageFraction tables.
	update year set fuelYearID=cutPointFuelYear where yearID > cutPoint;

	drop table if exists tempModelYear;
	create table if not exists tempModelYear (
		modelYearID smallint not null primary key
	) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

	insert into tempModelYear(modelYearID) values(1950),(1951),(1952),(1953),(1954),(1955),(1956),(1957)
		,(1958),(1959),(1960),(1961),(1962),(1963),(1964),(1965),(1966),(1967)
		,(1968),(1969),(1970),(1971),(1972),(1973),(1974),(1975),(1976),(1977),(1978),(1979),(1980),(1981),(1982),(1983),(1984),(1985),(1986),(1987)
		,(1988),(1989),(1990),(1991),(1992),(1993),(1994),(1995),(1996),(1997),(1998),(1999),(2000),(2001),(2002),(2003),(2004),(2005),(2006),(2007)
		,(2008),(2009),(2010),(2011),(2012),(2013),(2014),(2015),(2016),(2017),(2018),(2019),(2020),(2021),(2022),(2023),(2024),(2025),(2026),(2027)
		,(2028),(2029),(2030),(2031),(2032),(2033),(2034),(2035),(2036),(2037),(2038),(2039),(2040),(2041),(2042),(2043),(2044),(2045),(2046),(2047)
		,(2048),(2049),(2050),(2051),(2052),(2053),(2054),(2055),(2056),(2057),(2058),(2059),(2060);

	-- Decode model year groups
	-- single years represent single years, unless it is 1972 which represents 1950-1972
	-- 0 represents 1950-2060
	drop table if exists tempModelYearGroupDecode;
	create table tempModelYearGroupDecode (
		modelYearGroupID int(11) not null primary key,
		modelYearGroupName char(50) default null,
		minModelYearID smallint(6) default null,
		maxModelYearID smallint(6) default null,
		cutoffFlag smallint(6) default null
	) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select modelYearGroupID, modelYearGroupName from modelYearGroup;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'baseFuel' from baseFuel;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'cumTVVCoeffs' from cumTVVCoeffs;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'fuelModelWtFactor' from fuelModelWtFactor;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'meanFuelParameters' from meanFuelParameters;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'noNO2Ratio' from noNO2Ratio;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'pollutantProcessModelYear' from pollutantProcessModelYear;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'sourceBin' from sourceBin;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'sourceTypeModelYearGroup' from sourceTypeModelYearGroup;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'startTempAdjustment' from startTempAdjustment;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'sulfateEmissionRate' from sulfateEmissionRate;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'sulfurBase' from sulfurBase;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct fuelMYGroupID, 'sulfurModelCoeff' from sulfurModelCoeff;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct fuelMYGroupID, 'hcPermeationCoeff' from hcPermeationCoeff;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct fuelMYGroupID, 'hcSpeciation' from hcSpeciation;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'dioxinemissionrate' from dioxinemissionrate;
	
	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'metalemissionrate' from metalemissionrate;
	
	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'methanethcratio' from methanethcratio;
	
	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'minorhapratio' from minorhapratio;
	
	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'pahgasratio' from pahgasratio;
	
	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'pahparticleratio' from pahparticleratio;

	insert ignore into tempModelYearGroupDecode (modelYearGroupID, modelYearGroupName)
	select distinct modelYearGroupID, 'atrationongas' from atrationongas;


	update tempModelYearGroupDecode set minModelYearID=1950, maxModelYearID=2060
	where minModelYearID is null and maxModelYearID is null
	and modelYearGroupID=0;

	update tempModelYearGroupDecode set minModelYearID=1950, maxModelYearID=1972
	where minModelYearID is null and maxModelYearID is null
	and modelYearGroupID=1972;

	update tempModelYearGroupDecode set minModelYearID=modelYearGroupID, maxModelYearID=modelYearGroupID
	where minModelYearID is null and maxModelYearID is null
	and modelYearGroupID < 9999 and modelYearGroupID > 1950;

	update tempModelYearGroupDecode set minModelYearID=round(modelYearGroupID/10000,0), maxModelYearID=mod(modelYearGroupID,10000)
	where minModelYearID is null and maxModelYearID is null
	and modelYearGroupID > 9999;

	update tempModelYearGroupDecode set cutoffFlag=0 where cutoffFlag is null and minModelYearID <= cutPoint and maxModelYearID >= cutPoint;
	update tempModelYearGroupDecode set cutoffFlag= -1 where cutoffFlag is null and maxModelYearID < cutPoint;
	update tempModelYearGroupDecode set cutoffFlag= +1 where cutoffFlag is null and minModelYearID > cutPoint;

	-- baseFuel
	drop table if exists tempBaseFuel;
	create table tempBaseFuel
	select b.calculationEngine, b.fuelTypeID, b.fuelFormulationID, b.description, b.dataSourceID
	from baseFuel b
		inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);

	update baseFuel, tempBaseFuel, tempModelYearGroupDecode
	set baseFuel.fuelFormulationID = tempBaseFuel.fuelFormulationID, baseFuel.description = tempBaseFuel.description, baseFuel.dataSourceID = tempBaseFuel.dataSourceID
	where baseFuel.calculationEngine = tempBaseFuel.calculationEngine
		and baseFuel.fuelTypeID = tempBaseFuel.fuelTypeID
		and tempModelYearGroupDecode.modelYearGroupID = baseFuel.modelYearGroupID
		and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempBaseFuel;
	insert into tempMessages (message) values ('Updated baseFuel table');

	-- crankcaseEmissionRatio
	drop table if exists tempCrankcaseEmissionRatio;
	create table tempCrankcaseEmissionRatio
	select b.polProcessID, b.sourceTypeID, b.fuelTypeID, b.crankcaseRatio, b.crankcaseRatioCV
	from crankcaseEmissionRatio b
	where minModelYearID <= cutPoint
	and maxModelYearID >= cutPoint;
	
	update crankcaseEmissionRatio, tempCrankcaseEmissionRatio
	set crankcaseEmissionRatio.crankcaseRatio=tempCrankcaseEmissionRatio.crankcaseRatio, crankcaseEmissionRatio.crankcaseRatioCV=tempCrankcaseEmissionRatio.crankcaseRatioCV
	where crankcaseEmissionRatio.polProcessID=tempCrankcaseEmissionRatio.polProcessID and crankcaseEmissionRatio.sourceTypeID=tempCrankcaseEmissionRatio.sourceTypeID and crankcaseEmissionRatio.fuelTypeID=tempCrankcaseEmissionRatio.fuelTypeID
	and crankcaseEmissionRatio.minModelYearID > cutPoint;
	drop table if exists tempCrankcaseEmissionRatio;
	insert into tempMessages (message) values ('Updated crankcaseEmissionRatio table');

	-- cumTVVCoeffs
	drop table if exists tempcumtvvcoeffs;
	create table tempcumtvvcoeffs
	select b.regClassID, b.ageGroupID, b.polProcessID, b.tvvTermA, b.tvvTermB, b.tvvTermC, b.tvvTermACV, b.tvvTermBCV, b.tvvTermCCV, b.tvvTermAIM, b.tvvTermBIM, b.tvvTermCIM, b.tvvTermAIMCV, b.tvvTermBIMCV, b.tvvTermCIMCV, b.backPurgeFactor, b.averageCanisterCapacity, b.tvvEquation, b.leakEquation, b.leakFraction, b.tankSize, b.tankFillFraction
	from cumtvvcoeffs b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update cumtvvcoeffs, tempcumtvvcoeffs, tempModelYearGroupDecode
	set cumtvvcoeffs.tvvTermA=tempcumtvvcoeffs.tvvTermA, cumtvvcoeffs.tvvTermB=tempcumtvvcoeffs.tvvTermB, cumtvvcoeffs.tvvTermC=tempcumtvvcoeffs.tvvTermC, cumtvvcoeffs.tvvTermACV=tempcumtvvcoeffs.tvvTermACV, cumtvvcoeffs.tvvTermBCV=tempcumtvvcoeffs.tvvTermBCV, cumtvvcoeffs.tvvTermCCV=tempcumtvvcoeffs.tvvTermCCV, cumtvvcoeffs.tvvTermAIM=tempcumtvvcoeffs.tvvTermAIM, cumtvvcoeffs.tvvTermBIM=tempcumtvvcoeffs.tvvTermBIM, cumtvvcoeffs.tvvTermCIM=tempcumtvvcoeffs.tvvTermCIM, cumtvvcoeffs.tvvTermAIMCV=tempcumtvvcoeffs.tvvTermAIMCV, cumtvvcoeffs.tvvTermBIMCV=tempcumtvvcoeffs.tvvTermBIMCV, cumtvvcoeffs.tvvTermCIMCV=tempcumtvvcoeffs.tvvTermCIMCV, cumtvvcoeffs.backPurgeFactor=tempcumtvvcoeffs.backPurgeFactor, cumtvvcoeffs.averageCanisterCapacity=tempcumtvvcoeffs.averageCanisterCapacity, cumtvvcoeffs.tvvEquation=tempcumtvvcoeffs.tvvEquation, cumtvvcoeffs.leakEquation=tempcumtvvcoeffs.leakEquation, cumtvvcoeffs.leakFraction=tempcumtvvcoeffs.leakFraction, cumtvvcoeffs.tankSize=tempcumtvvcoeffs.tankSize, cumtvvcoeffs.tankFillFraction=tempcumtvvcoeffs.tankFillFraction
	where cumtvvcoeffs.regClassID=tempcumtvvcoeffs.regClassID
	and cumtvvcoeffs.ageGroupID=tempcumtvvcoeffs.ageGroupID
	and cumtvvcoeffs.polProcessID=tempcumtvvcoeffs.polProcessID
	and tempModelYearGroupDecode.modelYearGroupID = cumtvvcoeffs.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempcumtvvcoeffs;
	insert into tempMessages (message) values ('Updated cumTVVCoeffs table');

	-- fuelModelWtFactor
	drop table if exists tempFuelModelWtFactor;
	create table tempFuelModelWtFactor
	select b.fuelModelID, b.ageID, b.fuelModelWtFactor, b.dataSourceID
	from fuelModelWtFactor b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);

	update fuelModelWtFactor, tempFuelModelWtFactor, tempModelYearGroupDecode
	set fuelModelWtFactor.fuelModelWtFactor = tempFuelModelWtFactor.fuelModelWtFactor, fuelModelWtFactor.dataSourceID = tempFuelModelWtFactor.dataSourceID
	where fuelModelWtFactor.fuelModelID = tempFuelModelWtFactor.fuelModelID
		and fuelModelWtFactor.ageID = tempFuelModelWtFactor.ageID
		and tempModelYearGroupDecode.modelYearGroupID = fuelModelWtFactor.modelYearGroupID
		and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempFuelModelWtFactor;
	insert into tempMessages (message) values ('Updated fuelModelWtFactor table');

	-- fuelSupply
	select min(fuelYearID) into fuelYearToReplace from fuelSupply where fuelYearID >= cutPointFuelYear;
	set fuelYearToReplace=ifnull(fuelYearToReplace,1990);
	delete from fuelSupply where fuelYearID > fuelYearToReplace;
	update fuelSupply set fuelYearID=cutPointFuelYear where fuelYearID=fuelYearToReplace;

	-- fuelUsageFraction
	select min(fuelYearID) into fuelYearToReplace from fuelUsageFraction where fuelYearID >= cutPointFuelYear;
	set fuelYearToReplace=ifnull(fuelYearToReplace,1990);
	delete from fuelUsageFraction where fuelYearID > fuelYearToReplace;
	update fuelUsageFraction set fuelYearID=cutPointFuelYear where fuelYearID=fuelYearToReplace;

	-- generalFuelRatioExpression
	-- Delete anything that starts after cutPoint.
	delete from generalFuelRatioExpression where minModelYearID > cutPoint;

	-- Anything that applies prior to cutPoint should go unchanged.
	-- Anything that applies to cutPoint should apply to all years afterwards.  This is safe
	-- to do as any equation that used to begin after cutPoint was deleted above.
	update generalFuelRatioExpression set maxModelYearID=2060
	where minModelYearID <= cutPoint and maxModelYearID >= cutPoint and maxModelYearID < 2060;

	insert into tempMessages (message) values ('Updated generalFuelRatioExpression table');

	-- meanFuelParameters
	drop table if exists tempMeanFuelParameters;
	create table tempMeanFuelParameters
	select b.polProcessID, b.fuelTypeID, b.fuelParameterID, b.baseValue, b.centeringValue, b.stdDevValue, b.dataSourceID
	from meanFuelParameters b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update meanFuelParameters, tempMeanFuelParameters, tempModelYearGroupDecode
	set meanFuelParameters.baseValue=tempMeanFuelParameters.baseValue, meanFuelParameters.centeringValue=tempMeanFuelParameters.centeringValue, meanFuelParameters.stdDevValue=tempMeanFuelParameters.stdDevValue, meanFuelParameters.dataSourceID=tempMeanFuelParameters.dataSourceID
	where meanFuelParameters.polProcessID=tempMeanFuelParameters.polProcessID and meanFuelParameters.fuelTypeID=tempMeanFuelParameters.fuelTypeID and meanFuelParameters.fuelParameterID=tempMeanFuelParameters.fuelParameterID
	and tempModelYearGroupDecode.modelYearGroupID = meanFuelParameters.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempMeanFuelParameters;
	insert into tempMessages (message) values ('Updated meanFuelParameters table');

	-- noNO2Ratio
	drop table if exists tempNoNO2Ratio;
	create table tempNoNO2Ratio
	select b.polProcessID, b.sourceTypeID, b.fuelTypeID, b.NOxRatio, b.NOxRatioCV, b.dataSourceID
	from noNO2Ratio b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update noNO2Ratio, tempNoNO2Ratio, tempModelYearGroupDecode
	set noNO2Ratio.NOxRatio=tempNoNO2Ratio.NOxRatio, noNO2Ratio.NOxRatioCV=tempNoNO2Ratio.NOxRatioCV, noNO2Ratio.dataSourceID=tempNoNO2Ratio.dataSourceID
	where noNO2Ratio.polProcessID=tempNoNO2Ratio.polProcessID and noNO2Ratio.sourceTypeID=tempNoNO2Ratio.sourceTypeID and noNO2Ratio.fuelTypeID=tempNoNO2Ratio.fuelTypeID
	and tempModelYearGroupDecode.modelYearGroupID = noNO2Ratio.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempNoNO2Ratio;
	insert into tempMessages (message) values ('Updated noNO2Ratio table');

	-- PollutantProcessModelYear
	drop table if exists tempPollutantProcessModelYear;
	create table tempPollutantProcessModelYear
	select polProcessID, modelYearGroupID, fuelMYGroupID, IMModelYearGroupID
	from pollutantProcessModelYear
	where modelYearID = cutPoint;

	update pollutantProcessModelYear, tempPollutantProcessModelYear
	set pollutantProcessModelYear.modelYearGroupID = tempPollutantProcessModelYear.modelYearGroupID,
		pollutantProcessModelYear.fuelMYGroupID = tempPollutantProcessModelYear.fuelMYGroupID,
		pollutantProcessModelYear.IMModelYearGroupID = tempPollutantProcessModelYear.IMModelYearGroupID
	where pollutantProcessModelYear.polProcessID = tempPollutantProcessModelYear.polProcessID
		and pollutantProcessModelYear.modelYearID > cutPoint;
	drop table if exists tempPollutantProcessModelYear;
	insert into tempMessages (message) values ('Updated pollutantProcessModelYear table');

	-- sourceTypeModelYear
	drop table if exists tempSourceTypeModelYear;
	create table tempSourceTypeModelYear
	select b.sourceTypeID, b.ACPenetrationFraction, b.ACPenetrationFractionCV
	from sourceTypeModelYear b
	where modelYearID=cutPoint;
	
	update sourceTypeModelYear, tempSourceTypeModelYear
	set sourceTypeModelYear.ACPenetrationFraction=tempSourceTypeModelYear.ACPenetrationFraction, sourceTypeModelYear.ACPenetrationFractionCV=tempSourceTypeModelYear.ACPenetrationFractionCV
	where sourceTypeModelYear.sourceTypeID=tempSourceTypeModelYear.sourceTypeID
	and sourceTypeModelYear.modelYearID > cutPoint;
	drop table if exists tempSourceTypeModelYear;
	insert into tempMessages (message) values ('Updated sourceTypeModelYear table');

	-- sourceTypeModelYearGroup
	drop table if exists tempSourceTypeModelYearGroup;
	create table tempSourceTypeModelYearGroup
	select b.sourceTypeID, b.tankTemperatureGroupID
	from sourceTypeModelYearGroup b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update sourceTypeModelYearGroup, tempSourceTypeModelYearGroup, tempModelYearGroupDecode
	set sourceTypeModelYearGroup.tankTemperatureGroupID=tempSourceTypeModelYearGroup.tankTemperatureGroupID
	where sourceTypeModelYearGroup.sourceTypeID=tempSourceTypeModelYearGroup.sourceTypeID
	and tempModelYearGroupDecode.modelYearGroupID = sourceTypeModelYearGroup.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempSourceTypeModelYearGroup;
	insert into tempMessages (message) values ('Updated sourceTypeModelYearGroup table');

	-- SourceTypeTechAdjustment
	drop table if exists tempSourceTypeTechAdjustment;
	create table tempSourceTypeTechAdjustment
	select processID, sourceTypeID, refuelingTechAdjustment
	from sourceTypeTechAdjustment
	where modelYearID=cutPoint;

	update sourceTypeTechAdjustment, tempSourceTypeTechAdjustment
	set sourceTypeTechAdjustment.refuelingTechAdjustment = tempSourceTypeTechAdjustment.refuelingTechAdjustment
	where sourceTypeTechAdjustment.processID = tempSourceTypeTechAdjustment.processID
		and sourceTypeTechAdjustment.sourceTypeID = tempSourceTypeTechAdjustment.sourceTypeID
		and sourceTypeTechAdjustment.modelYearID > cutPoint;
	drop table if exists tempSourceTypeTechAdjustment;
	insert into tempMessages (message) values ('Updated sourceTypeTechAdjustment table');

	-- startTempAdjustment
	drop table if exists tempStartTempAdjustment;
	create table tempStartTempAdjustment
	select b.fuelTypeID, b.polProcessID, b.opModeID, b.tempAdjustTermA, b.tempAdjustTermACV, b.tempAdjustTermB, b.tempAdjustTermBCV, b.tempAdjustTermC, b.tempAdjustTermCCV
	from startTempAdjustment b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update startTempAdjustment, tempStartTempAdjustment, tempModelYearGroupDecode
	set startTempAdjustment.tempAdjustTermA=tempStartTempAdjustment.tempAdjustTermA, startTempAdjustment.tempAdjustTermACV=tempStartTempAdjustment.tempAdjustTermACV, startTempAdjustment.tempAdjustTermB=tempStartTempAdjustment.tempAdjustTermB, startTempAdjustment.tempAdjustTermBCV=tempStartTempAdjustment.tempAdjustTermBCV, startTempAdjustment.tempAdjustTermC=tempStartTempAdjustment.tempAdjustTermC, startTempAdjustment.tempAdjustTermCCV=tempStartTempAdjustment.tempAdjustTermCCV
	where startTempAdjustment.fuelTypeID=tempStartTempAdjustment.fuelTypeID and startTempAdjustment.polProcessID=tempStartTempAdjustment.polProcessID and startTempAdjustment.opModeID=tempStartTempAdjustment.opModeID
	and tempModelYearGroupDecode.modelYearGroupID = startTempAdjustment.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempStartTempAdjustment;
	insert into tempMessages (message) values ('Updated startTempAdjustment table');

	-- sulfateEmissionRate
	drop table if exists tempSulfateEmissionRate;
	create table tempSulfateEmissionRate
	select b.polProcessID, b.fuelTypeID, b.meanBaseRate, b.meanBaseRateCV, b.dataSourceID
	from sulfateEmissionRate b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update sulfateEmissionRate, tempSulfateEmissionRate, tempModelYearGroupDecode
	set sulfateEmissionRate.meanBaseRate=tempSulfateEmissionRate.meanBaseRate, sulfateEmissionRate.meanBaseRateCV=tempSulfateEmissionRate.meanBaseRateCV, sulfateEmissionRate.dataSourceID=tempSulfateEmissionRate.dataSourceID
	where sulfateEmissionRate.polProcessID=tempSulfateEmissionRate.polProcessID and sulfateEmissionRate.fuelTypeID=tempSulfateEmissionRate.fuelTypeID
	and tempModelYearGroupDecode.modelYearGroupID = sulfateEmissionRate.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempSulfateEmissionRate;
	insert into tempMessages (message) values ('Updated sulfateEmissionRate table');

	-- sulfurBase
	drop table if exists tempSulfurBase;
	create table tempSulfurBase
	select b.sulfurBase, b.sulfurBasis, b.sulfurGPAMax
	from sulfurBase b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update sulfurBase, tempSulfurBase, tempModelYearGroupDecode
	set sulfurBase.sulfurBase=tempSulfurBase.sulfurBase, sulfurBase.sulfurBasis=tempSulfurBase.sulfurBasis, sulfurBase.sulfurGPAMax=tempSulfurBase.sulfurGPAMax
	where 
	tempModelYearGroupDecode.modelYearGroupID = sulfurBase.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempSulfurBase;
	insert into tempMessages (message) values ('Updated sulfurBase table');

	-- sulfurModelCoeff
	drop table if exists tempSulfurModelCoeff;
	create table tempSulfurModelCoeff
	select b.processID, b.pollutantID, b.M6emitterID, b.sourceTypeID, b.sulfurFunctionID, b.sulfurCoeff, b.lowSulfurCoeff
	from sulfurModelCoeff b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.fuelMYGroupID and d.cutoffFlag=0);
	
	update sulfurModelCoeff, tempSulfurModelCoeff, tempModelYearGroupDecode
	set sulfurModelCoeff.sulfurCoeff=tempSulfurModelCoeff.sulfurCoeff, sulfurModelCoeff.lowSulfurCoeff=tempSulfurModelCoeff.lowSulfurCoeff, sulfurModelCoeff.sulfurFunctionID=tempSulfurModelCoeff.sulfurFunctionID
	where sulfurModelCoeff.processID=tempSulfurModelCoeff.processID 
	and sulfurModelCoeff.pollutantID=tempSulfurModelCoeff.pollutantID 
	and sulfurModelCoeff.M6emitterID=tempSulfurModelCoeff.M6emitterID 
	and sulfurModelCoeff.sourceTypeID=tempSulfurModelCoeff.sourceTypeID 
	and tempModelYearGroupDecode.modelYearGroupID = sulfurModelCoeff.fuelMYGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempSulfurModelCoeff;
	insert into tempMessages (message) values ('Updated sulfurModelCoeff table');

	-- hcPermeationCoeff
	drop table if exists temphcPermeationCoeff;
	create table temphcPermeationCoeff
	select polProcessID, etohThreshID, fuelAdjustment, fuelAdjustmentGPA, dataSourceID
	from hcPermeationCoeff b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.fuelMYGroupID and d.cutoffFlag=0);

	update hcPermeationCoeff, temphcPermeationCoeff, tempModelYearGroupDecode
	set hcPermeationCoeff.fuelAdjustment=temphcPermeationCoeff.fuelAdjustment, 
		hcPermeationCoeff.fuelAdjustmentGPA=temphcPermeationCoeff.fuelAdjustmentGPA, 
		hcPermeationCoeff.dataSourceID=temphcPermeationCoeff.dataSourceID
	where hcPermeationCoeff.polProcessID=temphcPermeationCoeff.polProcessID 
	and hcPermeationCoeff.etohThreshID=temphcPermeationCoeff.etohThreshID 
	and tempModelYearGroupDecode.modelYearGroupID = hcPermeationCoeff.fuelMYGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists temphcPermeationCoeff;
	insert into tempMessages (message) values ('Updated hcPermeationCoeff table');

	-- hcSpeciation
	drop table if exists temphcSpeciation;
	create table temphcSpeciation
	select polProcessID, fuelSubtypeID, etohThreshID, oxyThreshID, speciationConstant, oxySpeciation
	from hcSpeciation b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.fuelMYGroupID and d.cutoffFlag=0);

	update hcSpeciation, temphcSpeciation, tempModelYearGroupDecode
	set hcSpeciation.speciationConstant=temphcSpeciation.speciationConstant, 
		hcSpeciation.oxySpeciation=temphcSpeciation.oxySpeciation
	where hcSpeciation.polProcessID=temphcSpeciation.polProcessID 
	and hcSpeciation.fuelSubtypeID=temphcSpeciation.fuelSubtypeID
	and hcSpeciation.etohThreshID=temphcSpeciation.etohThreshID 
	and hcSpeciation.oxyThreshID=temphcSpeciation.oxyThreshID 
	and tempModelYearGroupDecode.modelYearGroupID = hcSpeciation.fuelMYGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists temphcSpeciation;
	insert into tempMessages (message) values ('Updated hcSpeciation table');

	-- dioxinemissionrate
	drop table if exists tempdioxinemissionrate;
	create table tempdioxinemissionrate
	select b.polProcessID, b.fuelTypeID, b.units, b.meanBaseRate, b.meanBaseRateCV, b.dataSourceId
	from dioxinemissionrate b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update dioxinemissionrate, tempdioxinemissionrate, tempModelYearGroupDecode
	set dioxinemissionrate.units=tempdioxinemissionrate.units, dioxinemissionrate.meanBaseRate=tempdioxinemissionrate.meanBaseRate, dioxinemissionrate.meanBaseRateCV=tempdioxinemissionrate.meanBaseRateCV, dioxinemissionrate.dataSourceId=tempdioxinemissionrate.dataSourceId
	where dioxinemissionrate.polProcessID=tempdioxinemissionrate.polProcessID
	and dioxinemissionrate.fuelTypeID=tempdioxinemissionrate.fuelTypeID
	and tempModelYearGroupDecode.modelYearGroupID = dioxinemissionrate.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempdioxinemissionrate;
	insert into tempMessages (message) values ('Updated dioxinemissionrate table');
	
	-- metalemissionrate
	drop table if exists tempmetalemissionrate;
	create table tempmetalemissionrate
	select b.polProcessID, b.fuelTypeID, b.sourceTypeID, b.units, b.meanBaseRate, b.meanBaseRateCV, b.dataSourceId
	from metalemissionrate b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update metalemissionrate, tempmetalemissionrate, tempModelYearGroupDecode
	set metalemissionrate.units=tempmetalemissionrate.units, metalemissionrate.meanBaseRate=tempmetalemissionrate.meanBaseRate, metalemissionrate.meanBaseRateCV=tempmetalemissionrate.meanBaseRateCV, metalemissionrate.dataSourceId=tempmetalemissionrate.dataSourceId
	where metalemissionrate.polProcessID=tempmetalemissionrate.polProcessID
	and metalemissionrate.fuelTypeID=tempmetalemissionrate.fuelTypeID
	and metalemissionrate.sourceTypeID=tempmetalemissionrate.sourceTypeID
	and tempModelYearGroupDecode.modelYearGroupID = metalemissionrate.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempmetalemissionrate;
	insert into tempMessages (message) values ('Updated metalemissionrate table');
	
	-- methanethcratio
	drop table if exists tempmethanethcratio;
	create table tempmethanethcratio
	select b.processID, b.fuelTypeID, b.sourceTypeID, b.ageGroupID, b.CH4THCRatio, b.CH4THCRatioCV
	from methanethcratio b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update methanethcratio, tempmethanethcratio, tempModelYearGroupDecode
	set methanethcratio.CH4THCRatio=tempmethanethcratio.CH4THCRatio, methanethcratio.CH4THCRatioCV=tempmethanethcratio.CH4THCRatioCV
	where methanethcratio.processID=tempmethanethcratio.processID
	and methanethcratio.fuelTypeID=tempmethanethcratio.fuelTypeID
	and methanethcratio.sourceTypeID=tempmethanethcratio.sourceTypeID
	and methanethcratio.ageGroupID=tempmethanethcratio.ageGroupID
	and tempModelYearGroupDecode.modelYearGroupID = methanethcratio.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempmethanethcratio;
	insert into tempMessages (message) values ('Updated methanethcratio table');
	
	-- minorhapratio
	drop table if exists tempminorhapratio;
	create table tempminorhapratio
	select b.polProcessID, b.fuelTypeID, b.fuelSubtypeID, b.atRatio, b.atRatioCV, b.dataSourceId
	from minorhapratio b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update minorhapratio, tempminorhapratio, tempModelYearGroupDecode
	set minorhapratio.atRatio=tempminorhapratio.atRatio, minorhapratio.atRatioCV=tempminorhapratio.atRatioCV, minorhapratio.dataSourceId=tempminorhapratio.dataSourceId
	where minorhapratio.polProcessID=tempminorhapratio.polProcessID
	and minorhapratio.fuelTypeID=tempminorhapratio.fuelTypeID
	and minorhapratio.fuelSubtypeID=tempminorhapratio.fuelSubtypeID
	and tempModelYearGroupDecode.modelYearGroupID = minorhapratio.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempminorhapratio;
	insert into tempMessages (message) values ('Updated minorhapratio table');
	
	-- pahgasratio
	drop table if exists temppahgasratio;
	create table temppahgasratio
	select b.polProcessID, b.fuelTypeID, b.atRatio, b.atRatioCV, b.dataSourceId
	from pahgasratio b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update pahgasratio, temppahgasratio, tempModelYearGroupDecode
	set pahgasratio.atRatio=temppahgasratio.atRatio, pahgasratio.atRatioCV=temppahgasratio.atRatioCV, pahgasratio.dataSourceId=temppahgasratio.dataSourceId
	where pahgasratio.polProcessID=temppahgasratio.polProcessID
	and pahgasratio.fuelTypeID=temppahgasratio.fuelTypeID
	and tempModelYearGroupDecode.modelYearGroupID = pahgasratio.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists temppahgasratio;
	insert into tempMessages (message) values ('Updated pahgasratio table');
	
	-- pahparticleratio
	drop table if exists temppahparticleratio;
	create table temppahparticleratio
	select b.polProcessID, b.fuelTypeID, b.atRatio, b.atRatioCV, b.dataSourceId
	from pahparticleratio b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update pahparticleratio, temppahparticleratio, tempModelYearGroupDecode
	set pahparticleratio.atRatio=temppahparticleratio.atRatio, pahparticleratio.atRatioCV=temppahparticleratio.atRatioCV, pahparticleratio.dataSourceId=temppahparticleratio.dataSourceId
	where pahparticleratio.polProcessID=temppahparticleratio.polProcessID
	and pahparticleratio.fuelTypeID=temppahparticleratio.fuelTypeID
	and tempModelYearGroupDecode.modelYearGroupID = pahparticleratio.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists temppahparticleratio;
	insert into tempMessages (message) values ('Updated pahparticleratio table');

	-- atrationongas
	drop table if exists tempatrationongas;
	create table tempatrationongas
	select b.polProcessID, b.sourceTypeID, b.fuelSubtypeID, b.ATRatio, b.ATRatioCV, b.dataSourceId
	from atrationongas b
	inner join tempModelYearGroupDecode d on (d.modelYearGroupID=b.modelYearGroupID and d.cutoffFlag=0);
	
	update atrationongas, tempatrationongas, tempModelYearGroupDecode
	set atrationongas.atRatio=tempatrationongas.atRatio, atrationongas.atRatioCV=tempatrationongas.atRatioCV, atrationongas.dataSourceId=tempatrationongas.dataSourceId
	where atrationongas.polProcessID=tempatrationongas.polProcessID
	and atrationongas.sourceTypeID=tempatrationongas.sourceTypeID
	and atrationongas.fuelSubtypeID=tempatrationongas.fuelSubtypeID
	and tempModelYearGroupDecode.modelYearGroupID = atrationongas.modelYearGroupID
	and tempModelYearGroupDecode.cutoffFlag = 1;
	drop table if exists tempatrationongas;
	insert into tempMessages (message) values ('Updated atrationongas table');




	drop table if exists tempModelYear;
	-- drop table if exists tempModelYearGroupDecode;
end
EndBlock

drop procedure if exists spCheckRateOfProgressIMPrograms;

BeginBlock
create procedure spCheckRateOfProgressIMPrograms()
begin
	-- Insert messages beginning with WARNING: or ERROR: to notify the user
	-- of IM programs that do not make sense if the Clean Air Act had not
	-- been enacted.
	-- insert into tempMessages (message) values ('WARNING: This is a test RoP warning');
	-- insert into tempMessages (message) values ('ERROR: This is a test RoP error');

	insert into tempMessages (message) values ('Checked IM programs for Rate of Progress suitability');
end
EndBlock

call spDoRateOfProgress();
drop procedure if exists spDoRateOfProgress;

call spCheckRateOfProgressIMPrograms();
drop procedure if exists spCheckRateOfProgressIMPrograms;
