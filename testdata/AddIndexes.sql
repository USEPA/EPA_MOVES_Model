ALTER TABLE AvgSpeedDistribution ADD (
       KEY (sourceTypeID),
       KEY (roadTypeID),
       KEY (hourDayID),
       KEY (avgSpeedBinID)
);
ALTER TABLE CountyYear ADD (
       KEY (countyID),
       KEY (yearID)
);
ALTER TABLE DayVMTFraction ADD (
       KEY (sourceTypeID),
       KEY (monthID),
       KEY (roadTypeID),
       KEY (dayID)
);
ALTER TABLE DriveScheduleAssoc ADD (
		KEY (sourceTypeID),
		KEY (roadTypeID),
		KEY (isRamp),
		KEY (driveScheduleID)
);
ALTER TABLE DriveScheduleSecond ADD (
		KEY (driveScheduleID),
		KEY (second)
);
ALTER TABLE EmissionRate ADD (
		KEY (sourceBinID),
		KEY (polProcessID),
		KEY (opModeID)
);
ALTER TABLE EmisTechFraction ADD (
		KEY (sourceTypeModelYearID),
		KEY (fuelTypeID),
		KEY (engTechID),
		KEY (emisTechID)
);
ALTER TABLE ExtendedIdleHours ADD (
		KEY (sourceTypeID),
		KEY (hourDayID),
		KEY (monthID),
		KEY (yearID),
		KEY (ageID),
		KEY (zoneID)
);
ALTER TABLE FuelAdjustment ADD (
		KEY (polProcessID),
		KEY (sourceTypeID),
		KEY (fuelSubtypeID)
);
ALTER TABLE FuelEngFraction ADD (
		KEY (sourceTypeModelYearID),
		KEY (fuelTypeID),
		KEY (engTechID)
);
ALTER TABLE FuelSupply ADD (
		KEY (countyID),
		KEY (yearID),
		KEY (monthGroupID),
		KEY (fuelSubtypeID)
);
ALTER TABLE FullACAdjustment ADD (
		KEY (sourceBinID),
		KEY (polProcessID),
		KEY (opModeID)
);
ALTER TABLE GREETManfAndDisposal ADD (
		KEY (GREETVehicleType),
		KEY (modelYearID),
		KEY (pollutantID),
		KEY (emissionStage)
);
ALTER TABLE GREETWellToPump ADD (
		KEY (yearID),
		KEY (pollutantID),
		KEY (fuelSubtypeID)
);
ALTER TABLE GridZoneAssoc ADD (
		KEY (zoneID),
		KEY (gridID)
);
ALTER TABLE HourDay ADD (
		KEY (dayID),
		KEY (hourID)
);
ALTER TABLE HourVMTFraction ADD (
		KEY (sourceTypeID),
		KEY (roadTypeID),
		KEY (dayID),
		KEY (hourID)
);
ALTER TABLE HPMSVtypeYear ADD (
		KEY (HPMSVtypeID),
		KEY (yearID)
);
ALTER TABLE IMOBDAdjustment ADD (
		KEY (sourceTypeID),
		KEY (countyID),
		KEY (yearID),
		KEY (ageID),
		KEY (fuelTypeID)
);
ALTER TABLE Link ADD (
		KEY (countyID),
		KEY (zoneID),
		KEY (roadTypeID)
);
ALTER TABLE LinkAverageSpeed ADD (
		KEY (linkID),
		KEY (hourDayID),
		KEY (sourceTypeID)
);
ALTER TABLE LinkHourVMTFraction ADD (
		KEY (linkID),
		KEY (monthID),
		KEY (sourceTypeID),
		KEY (dayID),
		KEY (hourID)
);
ALTER TABLE MonthGroupHour ADD (
		KEY (monthGroupID),
		KEY (hourID)
);
ALTER TABLE MonthofAnyYear ADD (
		KEY (monthGroupID)
);
ALTER TABLE MonthVMTFraction ADD (
		KEY (sourceTypeID),
		KEY (isLeapYear),
		KEY (monthID)
);
ALTER TABLE OpModeDistribution ADD (
		KEY (sourceTypeID),
		KEY (hourDayID),
		KEY (linkID),
		KEY (polProcessID),
		KEY (opModeID)
);
ALTER TABLE OpModePolProcAssoc ADD (
		KEY (polProcessID),
		KEY (opModeID)
);
ALTER TABLE PollutantProcessAssoc ADD (
		KEY (processID),
		KEY (pollutantID)
);
ALTER TABLE RoadTypeDistribution ADD (
		KEY (sourceTypeID),
		KEY (roadTypeID)
);
ALTER TABLE SCC ADD (
		KEY (roadTypeID),
		KEY (SCCVtypeID),
		KEY (SCCProcID)
);
ALTER TABLE SCCVTypeDistribution ADD (
		KEY (sourceTypeModelYearID),
		KEY (fuelTypeID),
		KEY (SCCVtypeID)
);
ALTER TABLE SHO ADD (
		KEY (hourDayID),
		KEY (monthID),
		KEY (yearID),
		KEY (ageID),
		KEY (linkID),
		KEY (sourceTypeID)
);
ALTER TABLE SizeWeightFraction ADD (
		KEY (sourceTypeModelyearID),
		KEY (fuelTypeID),
		KEY (engTechID),
		KEY (engSizeID),
		KEY (weightClassID)
);
ALTER TABLE SourceBin ADD (
		KEY (fuelTypeID),
		KEY (modelYearGroupID)
);
ALTER TABLE SourceBinDistribution ADD (
		KEY (sourceTypeModelYearID),
		KEY (polProcessID),
		KEY (sourceBinID)
);
ALTER TABLE SourceTypeAge ADD (
		KEY (ageID),
		KEY (sourceTypeID)
);
ALTER TABLE SourceTypeAgeDistribution ADD (
		KEY (sourceTypeID),
		KEY (yearID),
		KEY (ageID)
);
ALTER TABLE SourceTypeHour ADD (
		KEY (sourceTypeID),
		KEY (hourDayID)
);
ALTER TABLE SourceTypeModelYear ADD (
		KEY (modelYearID),
		KEY (sourceTypeID),
		KEY (modelYearGroupID)
);
ALTER TABLE SourceTypePolProcess ADD (
		KEY (sourceTypeID),
		KEY (polProcessID)
);
ALTER TABLE SourceTypeYear ADD (
		KEY (yearID),
		KEY (sourceTypeID)
);
ALTER TABLE SourceUseType ADD (
		KEY (HPMSVtypeID)
);
ALTER TABLE Starts ADD (
		KEY (hourDayID),
		KEY (monthID),
		KEY (yearID),
		KEY (ageID),
		KEY (zoneID),
		KEY (sourceTypeID)
);
ALTER TABLE TemperatureAdjustment ADD (
		KEY (sourceTypeID),
		KEY (polProcessID),
		KEY (fuelTypeID),
        KEY (regClassID)
);
ALTER TABLE Year ADD (
		KEY (isBaseYear)
);
ALTER TABLE Zone ADD (
		KEY (countyID)
);
ALTER TABLE ZoneMonth ADD (
		KEY (zoneID),
		KEY (monthID)
);
ALTER TABLE ZoneMonthHour ADD (
		KEY (monthID),
		KEY (zoneID),
		KEY (hourID)
);
ALTER TABLE ZoneYear ADD (
		KEY (zoneID),
		KEY (yearID)
);
ALTER TABLE ZoneYearRoadType ADD (
		KEY (zoneID),
		KEY (yearID),
		KEY (roadTypeID)
);
