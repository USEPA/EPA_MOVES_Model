-- Version 2014-05-31

-- @algorithm
-- @owner SO2 Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

drop table if exists so2RunSpecModelYear;
create table if not exists so2RunSpecModelYear (
	modelYearID smallint not null primary key
);
truncate so2RunSpecModelYear;

drop table if exists so2PMOneCountyYearGeneralFuelRatio;
create table if not exists so2PMOneCountyYearGeneralFuelRatio (
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
truncate so2PMOneCountyYearGeneralFuelRatio;

DROP TABLE IF EXISTS SO2CopyOfMonthOfAnyYear;
CREATE TABLE SO2CopyOfMonthOfAnyYear (
	monthID 		smallint(6),
	monthGroupID 	smallint(6)
);

DROP TABLE IF EXISTS SO2CopyOfPPA;
CREATE TABLE SO2CopyOfPPA (
	polProcessID	int,
	processID		SMALLINT(6),	
	pollutantID		SMALLINT(6)
);

DROP TABLE IF EXISTS SO2CopyOfYear;
CREATE TABLE SO2CopyOfYear (
       yearID        SMALLINT(6),
       isBaseYear    CHAR(1),
       fuelYearID    SMALLINT(6)
);

DROP TABLE IF EXISTS SO2CopyOfFuelFormulation;
CREATE TABLE SO2CopyOfFuelFormulation (
       fuelFormulationID        SMALLINT(6),
       fuelSubtypeID    		SMALLINT(6),
       sulfurLevel  			FLOAT,
       primary key (fuelFormulationID),
       key (fuelFormulationID, fuelSubtypeID),
       key (fuelSubtypeID, fuelFormulationID)
);

DROP TABLE IF EXISTS SO2CopyOfFuelSupply;
CREATE TABLE SO2CopyOfFuelSupply (
	fuelRegionID			int(11),
	fuelYearID				smallint(6),
	monthGroupID			smallint(6),
	fuelFormulationID		smallint(6),
	marketShare				float,
	marketShareCV 			float
);

DROP TABLE IF EXISTS SO2CopyOfFuelType;
CREATE TABLE SO2CopyOfFuelType (
       fuelTypeID        SMALLINT(6),
       primary key (fuelTypeID)
);

DROP TABLE IF EXISTS SO2CopyOfFuelSubType;
CREATE TABLE SO2CopyOfFuelSubType (
	   fuelSubTypeID	 SMALLINT(6),
       fuelTypeID        SMALLINT(6),
       energyContent	 FLOAT,
       primary key (fuelSubTypeID),
       key (fuelTypeID, fuelSubTypeID),
       key (fuelSubTypeID, fuelTypeID)
);

DROP TABLE IF EXISTS CopyOfSO2EmissionRate;
CREATE TABLE CopyOfSO2EmissionRate (
	polProcessID		int,
	fuelTypeID			smallint(6),
	modelYearGroupID	int(11),
	meanBaseRate 		float,
	meanBaseRateCV		float,
	dataSourceId		smallint(6)
);

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache select *
	into outfile '##so2RunSpecModelYear##'
from RunSpecModelYear;

cache select gfr.fuelTypeID, gfr.sourceTypeID, may.monthID, gfr.pollutantID, gfr.processID, mya.modelYearID, mya.yearID,
	sum((ifnull(fuelEffectRatio,1)+GPAFract*(ifnull(fuelEffectRatioGPA,1)-ifnull(fuelEffectRatio,1)))*marketShare) as fuelEffectRatio
	INTO OUTFILE '##so2PMOneCountyYearGeneralFuelRatio##'
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
	and gfr.pollutantID = 31
	and gfr.processID = ##context.iterProcess.databaseKey##
	and gfr.minModelYearID <= mya.modelYearID
	and gfr.maxModelYearID >= mya.modelYearID
	and gfr.minAgeID <= mya.ageID
	and gfr.maxAgeID >= mya.ageID
	and gfr.fuelTypeID = rssf.fuelTypeID
	and gfr.sourceTypeID = rssf.sourceTypeID)
group by gfr.fuelTypeID, gfr.sourceTypeID, may.monthID, gfr.pollutantID, gfr.processID, mya.modelYearID, mya.yearID;

cache SELECT 	MonthOfAnyYear.monthID, 
		MonthOfAnyYear.monthGroupID 
INTO OUTFILE '##SO2CopyOfMonthOfAnyYear##'  
FROM MonthOfAnyYear  INNER JOIN runspecmonth 
ON		MonthOfAnyYear.monthID = runspecmonth.monthID;

cache SELECT yearID, isBaseYear, fuelYearID INTO OUTFILE '##SO2CopyOfYear##'  FROM Year WHERE YearID = ##context.year##;

cache SELECT 	fuelFormulationID, 
		fuelSubTypeID, 
		sulfurLevel 
INTO OUTFILE 		'##SO2CopyOfFuelFormulation##'  
FROM FuelFormulation;

cache SELECT FuelSupply.*  INTO OUTFILE '##SO2CopyOfFuelSupply##'  
	FROM FuelSupply
	INNER JOIN Year ON FuelSupply.fuelYearID = Year.fuelYearID 
		AND Year.yearID = ##context.year##    
	WHERE fuelRegionID = ##context.fuelRegionID##;

cache SELECT fuelTypeID INTO OUTFILE '##SO2CopyOfFuelType##'
	FROM FuelType;

cache SELECT  fuelSubTypeID, fuelTypeID, energyContent INTO OUTFILE '##SO2CopyOfFuelSubType##'
	FROM FuelSubType;

cache SELECT 	*  INTO OUTFILE '##CopyOfSO2EmissionRate##'  FROM SulfateEmissionRate 
	WHERE polProcessID IN (3101, 3102, 3190, 3191);

cache SELECT polProcessID,processID,pollutantID
INTO OUTFILE '##SO2CopyOfPPA##' FROM pollutantprocessassoc 
WHERE processID=##context.iterProcess.databaseKey## 
AND pollutantID=31;

-- End Section Extract Data

-- Section Local Data Removal
-- End Section Local Data Removal

-- Section Processing

DROP TABLE IF EXISTS SO2FuelCalculation1;
CREATE TABLE SO2FuelCalculation1 (
	countyID				INT(11),
	yearID					SMALLINT(6),
	monthGroupID			SMALLINT(6),
	fuelTypeID				SMALLINT(6), 
	energyContent			FLOAT,
	WsulfurLevel			FLOAT
);

-- @algorithm energyContent = sum(marketShare * energyContent) across the fuel supply.
-- WsulfurLevel = sum(marketShare * sulfurLevel) across the fuel supply.
INSERT INTO SO2FuelCalculation1 (
	countyID,
	yearID,
	monthGroupID,
	fuelTypeID, 
	energyContent,
	WsulfurLevel   ) 
SELECT 
	##context.iterLocation.countyRecordID## as countyID, 
	y.yearID,
	fs.monthGroupID,
	ft.fuelTypeID, 
	sum(fs.marketShare * fst.energyContent) as energyContent,
	sum(fs.marketShare * ff.sulfurLevel) as WsulfurLevel  
FROM SO2CopyOfFuelSupply fs 
	INNER JOIN SO2CopyOfFuelFormulation ff 		ON fs.fuelFormulationID = ff.fuelFormulationID 
	INNER JOIN SO2CopyOfFuelSubType fst 		ON fst.fuelSubTypeID = ff.fuelSubTypeID 
	INNER JOIN SO2CopyOfFuelType ft 			ON fst.fuelTypeID = ft.fuelTypeID 
	INNER JOIN SO2CopyOfYear y 					ON y.fuelYearID = fs.fuelYearID 
GROUP BY fs.fuelRegionID, y.yearID, fs.monthGroupID, fst.fuelTypeID;

create index index1 on SO2FuelCalculation1 (countyID, yearID, monthGroupID, fuelTypeID);

DROP TABLE IF EXISTS SO2FuelCalculation2;
CREATE TABLE SO2FuelCalculation2 (
	polProcessID			int,
	processID				SMALLINT(6),
	pollutantID				SMALLINT(6),
	fuelTypeID				SMALLINT(6),
	modelYearID				SMALLINT(6),
	meanBaseRate			FLOAT
);

alter table CopyOfSO2EmissionRate add column minModelYearID smallint null;
alter table CopyOfSO2EmissionRate add column maxModelYearID smallint null;

update CopyOfSO2EmissionRate set
	minModelYearID = floor(modelYearGroupID / 10000),
	maxModelYearID = mod(modelYearGroupID, 10000);

INSERT INTO SO2FuelCalculation2 (
	polProcessID,
	processID,
	pollutantID,
	fuelTypeID,
	modelYearID,
	meanBaseRate     ) 
SELECT 
	ser.polProcessID,
	ppa.processID,
	ppa.pollutantID,
	ser.fuelTypeID,
	rsmy.modelYearID,
	ser.meanBaseRate 
FROM 	CopyOfSO2EmissionRate ser  
	INNER JOIN 	SO2CopyOfPPA  ppa 	 	ON ser.polProcessID = ppa.polProcessID
	INNER JOIN  so2RunSpecModelYear rsmy ON (
		rsmy.modelYearID >= ser.minModelYearID
		and rsmy.modelYearID <= ser.maxModelYearID
	);

create index index1 on SO2FuelCalculation2 (processID, pollutantID, modelYearID, fuelTypeID);

DROP TABLE IF EXISTS SO2MOVESOutputTemp1;

-- @algorithm SO2 (31) = (meanBaseRate * WsulfurLevel * Total Energy Consumption (91)) / energyContent.
CREATE TABLE SO2MOVESOutputTemp1
SELECT 
	mwo.MOVESRunID, mwo.iterationID, mwo.yearID, mwo.monthID, mwo.dayID, 
	mwo.hourID, mwo.stateID, mwo.countyID, mwo.zoneID, 
	mwo.linkID, fc2.pollutantID, fc2.processID, 
	mwo.sourceTypeID, mwo.regClassID, mwo.fuelTypeID, mwo.modelYearID, 
	mwo.roadTypeID, mwo.SCC,
	mwo.emissionQuant as energy,
	mwo.emissionRate as energyRate,
	fc1.WsulfurLevel,
	fc1.energyContent,
	fc2.meanBaseRate,
	( (fc2.meanBaseRate * fc1.WsulfurLevel * mwo.emissionQuant ) / fc1.energyContent ) as emissionQuant,
	( (fc2.meanBaseRate * fc1.WsulfurLevel * mwo.emissionRate )  / fc1.energyContent ) as emissionRate
FROM
	MOVESWorkerOutput mwo, SO2fuelcalculation1 fc1, SO2fuelcalculation2 fc2, SO2copyOfMonthOfAnyYear may  
WHERE 
	mwo.countyID			=	fc1.countyID 		AND
	mwo.yearID				=	fc1.yearID			AND 
	mwo.monthID				= 	may.monthID			AND
	fc1.monthGroupID		=	may.monthGroupID 	AND  
	mwo.fuelTypeID			=	fc1.fuelTypeID		AND 
	mwo.fuelTypeID			=	fc2.fuelTypeID		AND 
	mwo.modelyearID			=	fc2.modelyearID		AND
	mwo.pollutantID = 91 	AND
	mwo.processID = ##context.iterProcess.databaseKey##;

-- @algorithm Apply general fuel effects.
-- emissionQuant = emissionQuant * fuelEffectRatio.
update SO2MOVESOutputTemp1, so2PMOneCountyYearGeneralFuelRatio set 
	emissionQuant=emissionQuant*fuelEffectRatio,
	emissionRate =emissionRate *fuelEffectRatio
where so2PMOneCountyYearGeneralFuelRatio.fuelTypeID = SO2MOVESOutputTemp1.fuelTypeID
and so2PMOneCountyYearGeneralFuelRatio.sourceTypeID = SO2MOVESOutputTemp1.sourceTypeID
and so2PMOneCountyYearGeneralFuelRatio.monthID 		= SO2MOVESOutputTemp1.monthID
and so2PMOneCountyYearGeneralFuelRatio.pollutantID 	= SO2MOVESOutputTemp1.pollutantID
and so2PMOneCountyYearGeneralFuelRatio.processID 	= SO2MOVESOutputTemp1.processID
and so2PMOneCountyYearGeneralFuelRatio.modelYearID 	= SO2MOVESOutputTemp1.modelYearID
and so2PMOneCountyYearGeneralFuelRatio.yearID 		= SO2MOVESOutputTemp1.yearID;

INSERT INTO MOVESWorkerOutput ( 
	MOVESRunID,iterationID,yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant,emissionRate) 
SELECT 
	MOVESRunID,iterationID, yearID,monthID,dayID,hourID,stateID,countyID,zoneID, 
	linkID,pollutantID,processID,sourceTypeID,regClassID,fuelTypeID,modelYearID, 
	roadTypeID,SCC,emissionQuant,emissionRate
FROM SO2MOVESOutputTemp1;

-- End Section Processing

-- Section Cleanup

DROP TABLE IF EXISTS SO2MOVESOutputTemp1;
DROP TABLE IF EXISTS SO2FuelCalculation1;
DROP TABLE IF EXISTS SO2FuelCalculation2;
DROP TABLE IF EXISTS SO2CopyOfMonthOfAnyYear;
DROP TABLE IF EXISTS SO2CopyOfPPA;
DROP TABLE IF EXISTS SO2CopyOfYear;
DROP TABLE IF EXISTS SO2CopyOfFuelFormulation;
DROP TABLE IF EXISTS SO2CopyOfFuelSupply;
DROP TABLE IF EXISTS SO2CopyOfFuelType;
DROP TABLE IF EXISTS SO2CopyOfFuelSubType;
DROP TABLE IF EXISTS CopyOfSO2EmissionRate;
DROP TABLE IF EXISTS so2RunSpecModelYear;

-- End Section Cleanup
