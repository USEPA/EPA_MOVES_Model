-- Author Wesley Faler
-- Author Don Smith
-- Version 2013-04-15

CREATE TABLE NRAgeCategory (
  ageID smallint(6) NOT NULL,
  ageCategoryName char(50) DEFAULT NULL,
  PRIMARY KEY (ageID),
  UNIQUE KEY XPKNRAgeCategory (ageID)
);

CREATE TABLE NRBaseYearEquipPopulation (
  sourceTypeID smallint(6) NOT NULL,
  stateID smallint(6) NOT NULL,
  population float DEFAULT NULL,
  NRBaseYearID smallint(6) NOT NULL,
  PRIMARY KEY (sourceTypeID,stateID),
  UNIQUE KEY XPKNRBaseYearEquipPopulation (sourceTypeID,stateID)
);

CREATE TABLE NRCrankCaseEmissionRatio (
  sourceTypeID smallint(6) NOT NULL,
  polProcessID int NOT NULL,
  sourceBinID bigint(20) NOT NULL,
  NRProcessMeanBaseRate float DEFAULT NULL,
  NRProcessMeanBaseRateCV float DEFAULT NULL,
  dataSourceId smallint(6) NOT NULL,
  PRIMARY KEY (sourceTypeID,polProcessID,sourceBinID),
  UNIQUE KEY XPKNRProcessEmissionRate (sourceTypeID,polProcessID,sourceBinID),
  KEY INDEX1 (sourceTypeID,polProcessID,sourceBinID),
  KEY XPFnrCrankCaseEmissionRatio (sourceTypeID,polProcessID,sourceBinID)
);

CREATE TABLE NRDayAllocation (
  NREquipTypeID smallint(6) NOT NULL,
  dayID smallint(6) NOT NULL,
  dayFraction float NOT NULL,
  PRIMARY KEY (NREquipTypeID,dayID),
  UNIQUE KEY XPKNRDayAllocation (NREquipTypeID,dayID)
);

CREATE TABLE NRDeterioration (
  polProcessID int NOT NULL,
  engTechID smallint(6) NOT NULL,
  DFCoefficient float DEFAULT NULL,
  DFAgeExponent float DEFAULT NULL,
  emissionCap smallint(6) NOT NULL,
  PRIMARY KEY (polProcessID,engTechID),
  UNIQUE KEY XPKNRDeterioration (polProcessID,engTechID)
);

CREATE TABLE NREngTechFraction (
  sourceTypeID smallint(6) NOT NULL,
  modelYearID smallint(6) NOT NULL,
  processID smallint(6) NOT NULL,
  engTechID smallint(6) NOT NULL,
  NREngTechFraction float NOT NULL,
  PRIMARY KEY (sourceTypeID,modelYearID,processID,engTechID),
  UNIQUE KEY XPKNREngTechFraction (sourceTypeID,modelYearID,processID,engTechID)
);

CREATE TABLE NREquipmentType (
  NREquipTypeID smallint(6) NOT NULL,
  description char(40) DEFAULT NULL,
  sectorID smallint(6) NOT NULL,
  useDefaultScrappage char(1) DEFAULT NULL,
  surrogateID smallint(6) DEFAULT NULL,
  PRIMARY KEY (NREquipTypeID),
  UNIQUE KEY XPKNREquipmentType (NREquipTypeID)
);

CREATE TABLE NREvapEmissionRate (
  sourceTypeID smallint(6) NOT NULL,
  polProcessID int NOT NULL,
  sourceBinID bigint(20) NOT NULL,
  NRProcessMeanBaseRate float DEFAULT NULL,
  NRProcessMeanBaseRateCV float DEFAULT NULL,
  dataSourceId smallint(6) NOT NULL,
  PRIMARY KEY (sourceTypeID,polProcessID,sourceBinID),
  UNIQUE KEY XPKNRProcessEmissionRate (sourceTypeID,polProcessID,sourceBinID),
  KEY INDEX1 (sourceTypeID,polProcessID,sourceBinID),
  KEY XPFnrEvapEmissionRate (sourceTypeID,polProcessID,sourceBinID)
);

CREATE TABLE NRExhaustEmissionRate (
  sourceTypeID smallint(6) NOT NULL,
  polProcessID int NOT NULL,
  sourceBinID bigint(20) NOT NULL,
  NRProcessMeanBaseRate float DEFAULT NULL,
  NRProcessMeanBaseRateCV float DEFAULT NULL,
  dataSourceId smallint(6) NOT NULL,
  PRIMARY KEY (sourceTypeID,polProcessID,sourceBinID),
  UNIQUE KEY XPKNRExhaustEmissionRate (sourceTypeID,polProcessID,sourceBinID)
);

CREATE TABLE NRFuelOxyAdjustment (
  strokes tinyint(4) NOT NULL,
  polProcessID int NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  NRFuelOxyAdjust float DEFAULT NULL,
  PRIMARY KEY (strokes,polProcessID,fuelTypeID),
  UNIQUE KEY XPKNRFuelOxyAdjustment (strokes,polProcessID,fuelTypeID)
);

CREATE TABLE NRGrowthIndex (
  growthPatternID smallint(6) NOT NULL,
  yearID smallint(6) NOT NULL,
  growthIndex smallint(6) DEFAULT NULL,
  PRIMARY KEY (growthPatternID,yearID),
  UNIQUE KEY XPKNRGrowthIndex (growthPatternID,yearID)
);

CREATE TABLE NRGrowthPattern (
  growthPatternID smallint(6) NOT NULL,
  description char(80) DEFAULT NULL,
  PRIMARY KEY (growthPatternID),
  UNIQUE KEY XPKNRGrowthPattern (growthPatternID)
);

CREATE TABLE NRGrowthPatternFinder (
  SCC char(10) NOT NULL,
  stateID smallint(6) NOT NULL,
  growthPatternID smallint(6) NOT NULL,
  PRIMARY KEY (SCC,stateID),
  UNIQUE KEY XPKNRGrowthPatternFinder (SCC,stateID)
);

CREATE TABLE NRHourAllocation (
  NRHourAllocPatternID smallint(6) NOT NULL,
  hourID smallint(6) NOT NULL,
  hourFraction float NOT NULL,
  PRIMARY KEY (NRHourAllocPatternID,hourID),
  UNIQUE KEY XPKNRHourAllocation (NRHourAllocPatternID,hourID)
);

CREATE TABLE NRHourAllocPattern (
  NRHourAllocPatternID smallint(6) NOT NULL,
  description char(255) NOT NULL,
  PRIMARY KEY (NRHourAllocPatternID),
  UNIQUE KEY XPKNRHourAllocPattern (NRHourAllocPatternID)
);

CREATE TABLE NRHourPatternFinder (
  NREquipTypeID smallint(6) NOT NULL,
  NRHourAllocPatternID smallint(6) DEFAULT NULL,
  PRIMARY KEY (NREquipTypeID),
  UNIQUE KEY XPKNRHourPatternFinder (NREquipTypeID)
);

CREATE TABLE NRHPRangeBin (
  NRHPRangeBinID smallint(6) NOT NULL,
  binName char(20) DEFAULT NULL,
  hpMin smallint(6) DEFAULT NULL,
  hpMax smallint(6) DEFAULT NULL,
  engSizeID smallint(6) NOT NULL,
  PRIMARY KEY (NRHPRangeBinID),
  UNIQUE KEY XPKNRHPRangeBin (NRHPRangeBinID)
);

CREATE TABLE NRMonthAllocation (
  NREquipTypeID smallint(6) NOT NULL,
  stateID smallint(6) NOT NULL,
  monthID smallint(6) NOT NULL,
  monthFraction float NOT NULL,
  PRIMARY KEY (NREquipTypeID,stateID,monthID),
  UNIQUE KEY XPKNRMonthAllocation (NREquipTypeID,stateID,monthID)
);

CREATE TABLE NRPollutantProcessModelYear (
  polProcessID int NOT NULL,
  modelYearID smallint(6) NOT NULL,
  modelYearGroupID int(11) NOT NULL,
  PRIMARY KEY (polProcessID,modelYearID),
  UNIQUE KEY XPKNRPollutantProcessModelYear (polProcessID,modelYearID)
);

CREATE TABLE NRProcessEmissionRate (
  sourceTypeID smallint(6) NOT NULL,
  polProcessID int NOT NULL,
  sourceBinID bigint(20) NOT NULL,
  NRProcessMeanBaseRate float DEFAULT NULL,
  NRProcessMeanBaseRateCV float DEFAULT NULL,
  dataSourceId smallint(6) NOT NULL,
  PRIMARY KEY (sourceTypeID,polProcessID,sourceBinID),
  UNIQUE KEY XPKNRProcessEmissionRate (sourceTypeID,polProcessID,sourceBinID)
);

CREATE TABLE NRSCC (
  SCC char(10) NOT NULL,
  NREquipTypeID smallint(6) NOT NULL,
  strokes tinyint(4) DEFAULT NULL,
  description char(40) DEFAULT NULL,
  fuelTypeID smallint(6) NOT NULL,
  PRIMARY KEY (SCC),
  UNIQUE KEY XPKNRSCC (SCC)
);

CREATE TABLE NRScrappageCurve (
  NREquipTypeID smallint(6) NOT NULL,
  fractionLifeused float NOT NULL,
  percentageScrapped float DEFAULT NULL,
  PRIMARY KEY (NREquipTypeID,fractionLifeused),
  UNIQUE KEY XPKNRScrappageCurve (NREquipTypeID,fractionLifeused)
);

CREATE TABLE NRSourceBin (
  sourceBinID bigint(20) NOT NULL,
  fuelTypeID smallint(6) NOT NULL DEFAULT '0',
  engTechID smallint(6) NOT NULL DEFAULT '0',
  modelYearGroupID smallint(5) unsigned NOT NULL,
  engSizeID smallint(6) unsigned NOT NULL,
  PRIMARY KEY (sourceBinID),
  KEY Index_srcbin (fuelTypeID,engTechID,engSizeID)
);

CREATE TABLE NRSourceUseType (
  sourceTypeID smallint(6) NOT NULL,
  SCC char(10) NOT NULL,
  NRHPRangeBinID smallint(6) NOT NULL,
  medianLifeFullLoad float DEFAULT NULL,
  hoursUsedPerYear float DEFAULT NULL,
  loadFactor float DEFAULT NULL,
  hpAvg float DEFAULT NULL,
  isPumpFilled char(1) DEFAULT NULL,
  tankSize float DEFAULT NULL,
  tankFillFrac float DEFAULT NULL,
  tankMetalFrac float DEFAULT NULL,
  hoseLength float DEFAULT NULL,
  hoseDiameter float DEFAULT NULL,
  hoseMetalFrac float DEFAULT NULL,
  marineFillNeckHoseLength float DEFAULT NULL,
  marineFillNeckHoseDiameter float DEFAULT NULL,
  marineSupplyHoseLength float DEFAULT NULL,
  marineSupplyHoseDiameter float DEFAULT NULL,
  marineVentHoseLength float DEFAULT NULL,
  marineVentHoseDiameter float DEFAULT NULL,
  hotSoaksPerSHO float DEFAULT NULL,
  nonInstMarineTankFrac float DEFAULT NULL,
  marineInstPlasticTankTrailFrac float NOT NULL,
  marineInstPlasticTankWaterFrac float DEFAULT NULL,
  marineInstMetalTankTrailerFrac float DEFAULT NULL,
  marineInstMetalTankWaterFrac float DEFAULT NULL,
  e10TankPermeationAdjFac float DEFAULT NULL,
  e10HosePermeationAdjFac float DEFAULT NULL,
  e10MarineFillNeckPermAdjFac float DEFAULT NULL,
  e10MarineSupplyHosePermAdjFac float DEFAULT NULL,
  e10MarineVentHosePermAdjFac float DEFAULT NULL,
  PRIMARY KEY (sourceTypeID),
  UNIQUE KEY XPKNRSourceUseType (sourceTypeID)
);

CREATE TABLE NRStateSurrogateTotal (
  surrogateID smallint(6) NOT NULL,
  stateID smallint(6) NOT NULL,
  surrogateQuant float NOT NULL,
  surrogateYearID smallint(6) DEFAULT NULL,
  PRIMARY KEY (surrogateID,stateID),
  UNIQUE KEY XPKNRStateSurrogateTotal (surrogateID,stateID)
);

CREATE TABLE NRSulfurAdjustment (
  fuelTypeID smallint(6) NOT NULL,
  engTechID smallint(6) NOT NULL,
  PMBaseSulfur float NOT NULL,
  sulfatePMConversionFactor float NOT NULL,
  PRIMARY KEY (fuelTypeID,engTechID),
  UNIQUE KEY XPKNRSulfurAdjustment (fuelTypeID,engTechID)
);

CREATE TABLE NRSurrogate (
  surrogateID smallint(6) NOT NULL,
  description char(255) DEFAULT NULL,
  PRIMARY KEY (surrogateID),
  UNIQUE KEY XPKNRSurrogate (surrogateID)
);

CREATE TABLE NRTemperatureAdjustment (
  strokes tinyint(4) NOT NULL,
  polProcessID int NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  NRTemperatureAdjustGT75 float DEFAULT NULL,
  NRTemperatureAdjustLT75 float DEFAULT NULL,
  PRIMARY KEY (strokes,polProcessID,fuelTypeID),
  UNIQUE KEY XPKNRTemperatureAdjustment (strokes,polProcessID,fuelTypeID)
);

CREATE TABLE NRTransientAdjustFactor (
  NREquipTypeID smallint(6) NOT NULL,
  NRHPRangeBinID smallint(6) NOT NULL,
  polProcessID int NOT NULL,
  fuelTypeID smallint(6) NOT NULL,
  engTechID smallint(6) NOT NULL,
  NRTransientAdjustFactor float NOT NULL,
  PRIMARY KEY (NREquipTypeID,NRHPRangeBinID,polProcessID,fuelTypeID,engTechID),
  UNIQUE KEY XPKNRTransientAdjustFactor (NREquipTypeID,NRHPRangeBinID,polProcessID,fuelTypeID,engTechID)
);

CREATE TABLE NRYear (
  yearID SMALLINT NOT NULL,
  isBaseYear CHAR(1) NULL,
  fuelYearID smallint NOT NULL DEFAULT '0',
  primary key (yearID),
  key (isBaseYear)
);

CREATE TABLE NRZoneAllocation (
  surrogateID smallint(6) NOT NULL,
  stateID smallint(6) NOT NULL,
  zoneID int(11) NOT NULL,
  surrogateQuant float NOT NULL,
  PRIMARY KEY (surrogateID,stateID,zoneID),
  UNIQUE KEY XPKNRZoneAllocation (surrogateID,stateID,zoneID)
);

