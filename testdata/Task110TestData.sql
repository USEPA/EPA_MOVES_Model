USE MOVESDB062603;

--
-- Populate FuelSubType Table
-- 

UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 0.99 WHERE fueltypeid=1 AND fuelsubtypeid=10;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 0.98 WHERE fueltypeid=1 AND fuelsubtypeid=11;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 0.9 WHERE fueltypeid=1 AND fuelsubtypeid=12;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 1 WHERE fueltypeid=2 AND fuelsubtypeid=20;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 0.9 WHERE fueltypeid=2 AND fuelsubtypeid=21;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 1 WHERE fueltypeid=2 AND fuelsubtypeid=22;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 1 WHERE fueltypeid=3 AND fuelsubtypeid=30;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 1 WHERE fueltypeid=4 AND fuelsubtypeid=40;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 0 WHERE fueltypeid=5 AND fuelsubtypeid=50;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 1 WHERE fueltypeid=6 AND fuelsubtypeid=60;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 0.5 WHERE fueltypeid=7 AND fuelsubtypeid=70;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 0.5 WHERE fueltypeid=8 AND fuelsubtypeid=80;
UPDATE FuelSubType SET fuelSubtypePetroleumFraction = 0.5 WHERE fueltypeid=9 AND fuelsubtypeid=90;

UPDATE FuelSubType SET fuelSubtypeFossilFraction =1 WHERE fueltypeid=1 AND fuelsubtypeid=10;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =0.98 WHERE fueltypeid=1 AND fuelsubtypeid=11;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =0.9 WHERE fueltypeid=1 AND fuelsubtypeid=12;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =1 WHERE fueltypeid=2 AND fuelsubtypeid=20;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =0.9 WHERE fueltypeid=2 AND fuelsubtypeid=21;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =1 WHERE fueltypeid=2 AND fuelsubtypeid=22;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =1 WHERE fueltypeid=3 AND fuelsubtypeid=30;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =1 WHERE fueltypeid=4 AND fuelsubtypeid=40;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =0 WHERE fueltypeid=5 AND fuelsubtypeid=50;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =1 WHERE fueltypeid=6 AND fuelsubtypeid=60;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =0.98 WHERE fueltypeid=7 AND fuelsubtypeid=70;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =0.98 WHERE fueltypeid=8 AND fuelsubtypeid=80;
UPDATE FuelSubType SET fuelSubtypeFossilFraction =0.98 WHERE fueltypeid=9 AND fuelsubtypeid=90;

--
-- Populate GREETWellToPump Table
-- 

INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (1995,	91,	10,	0.37);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (1995,	91,	11,	0.24);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (1995,	91,	12,	0.48);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2000,	91,	10,	0.43);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2000,	91,	11,	0.28);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2000,	91,	12,	0.44);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2005,	91,	10,	0.49);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2005,	91,	11,	0.30);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2005,	91,	12,	0.42);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2010,	91,	10,	0.51);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2010,	91,	11,	0.33);
INSERT INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate ) VALUES (2010,	91,	12,	0.40);

--
-- Populate EmissionRate Table
-- 

INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	1,	0.50);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	2,	0.60);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	3,	0.70);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	4,	0.80);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	5,	0.90);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	6,	1.00);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	7,	1.10);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	8,	1.20);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	9,	1.30);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	10,	1.40);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	11,	1.50);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	12,	1.60);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	13,	1.70);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9101,	14,	1.80);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	1,	0.70);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	2,	0.75);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	3,	0.80);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	4,	0.85);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	5,	0.90);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	6,	0.95);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	7,	1.00);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	8,	1.05);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	9,	1.10);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	10,	1.15);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	11,	1.20);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	12,	1.25);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	13,	1.30);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9101,	14,	1.35);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9102,	100,0.20);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9102,	100,0.25);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400425303000,	9190,	200,0.35);
INSERT INTO EmissionRate (sourceTypeID, sourceBinID, polProcessID, opModeID, meanBaseRate) VALUES (21,	10101400430353000,	9190,	200,0.45);

--
-- Populate SCCVTypeDistribution Table
-- 

INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	1,	1,	1.0000);
INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	2,	1,	1.0000);
INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	3,	1,	1.0000);
INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	4,	1,	1.0000);
INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	5,	1,	1.0000);
INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	6,	1,	1.0000);
INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	7,	1,	1.0000);
INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	8,	1,	1.0000);
INSERT INTO SCCVTypeDistribution (sourceTypeModelYearID, fuelTypeID, SCCVTypeID, SCCVTypeFraction) VALUES (211991,	9,	1,	1.0000);

--
-- Populate IMOBDAdjustment Table
-- 

INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,0,1,1.0000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,1,1,0.9800);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,2,1,0.9700);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,3,1,0.9600);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,4,1,0.9500);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,5,1,0.9300);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,6,1,0.9100);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,7,1,0.9000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,8,1,0.8800);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,9,1,0.8700);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,10,1,0.8600);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,11,1,0.8500);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,12,1,0.8400);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,13,1,0.8300);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,14,1,0.8200);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,15,1,0.8100);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,16,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,17,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,18,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,19,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,20,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,21,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,22,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,23,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,24,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,25,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,26,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,27,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,28,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,29,1,0.8000);
INSERT INTO IMOBDAdjustment (sourceTypeID, countyID, yearID, ageID, fuelTypeID, IMOBDAdjustment) VALUES (21,26161,2001,30,1,0.8000);

--
-- Populate OpModeDistribution Table
-- 

INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,1,0.0283);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,2,0.0174);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,3,0.0206);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,4,0.0665);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,5,0.0622);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,6,0.0688);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,7,0.0852);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,8,0.0875);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,9,0.0992);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,10,0.1392);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,11,0.1469);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,12,0.1092);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,13,0.0535);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9101,14,0.0155);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9102,100,1.0000);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (21,2616123,71,9190,200,1.0000);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,1,0.0751);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,2,0.1235);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,3,0.6761);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,4,0.0551);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,5,0.0301);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,6,0.0234);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,7,0.0033);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,8,0.0083);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,9,0.0033);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,10,0.0017);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,11,0.0000);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,12,0.0000);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,13,0.0000);
INSERT INTO OpModeDistribution (SourceTypeID,linkID,hourDayID,polProcessID,opModeID,opModeFraction) VALUES (43,2616133,71,9101,14,0.0000);

--
-- Populate SourceBinDistribution Table
--

INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9101, 10101400425303000, 0.7500);
INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9101, 10101400430353000, 0.2500);
INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9101, 10101400435403000, 0.0000);
INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9102, 10101400425303000, 0.7000);
INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9102, 10101400430353000, 0.3000);
INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9102, 10101400435403000, 0.0000);
INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9190, 10101400425303000, 0.6500);
INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9190, 10101400430353000, 0.3500);
INSERT INTO SourceBinDistribution (sourceTypeModelYearID, polProcessID, sourceBinID, sourceBinActivityFraction) VALUES (211991, 9190, 10101400435403000, 0.0000);

-- 
-- Populate the ExtendedIdleHours Table
--

INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 0, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 1, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 2, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 3, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 4, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 5, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 6, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 7, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 8, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 9, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 10, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 11, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 12, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 13, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 14, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 15, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 16, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 17, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 18, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 19, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 20, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 21, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 22, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 23, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 24, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 25, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 26, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 27, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 28, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 29, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (21, 2001, 30, 261610, 7, 71, 0);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 0, 2616133, 1, 71, 0.000301608);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 1, 2616133, 1, 71, 0.000292738);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 2, 2616133, 1, 71, 0.000281144);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 3, 2616133, 1, 71, 0.000504053);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 4, 2616133, 1, 71, 0.000451461);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 5, 2616133, 1, 71, 0.000403985);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 6, 2616133, 1, 71, 0.00036115);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 7, 2616133, 1, 71, 0.000322554);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 8, 2616133, 1, 71, 0.000287784);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 9, 2616133, 1, 71, 0.000256471);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 10, 2616133, 1, 71, 0.000228316);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 11, 2616133, 1, 71, 0.000203012);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 12, 2616133, 1, 71, 0.000180281);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 13, 2616133, 1, 71, 0.000159889);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 14, 2616133, 1, 71, 0.0001416);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 15, 2616133, 1, 71, 0.000125223);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 16, 2616133, 1, 71, 0.000110571);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 17, 2616133, 1, 71, 9.74733E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 18, 2616133, 1, 71, 8.57796E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 19, 2616133, 1, 71, 7.53514E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 20, 2616133, 1, 71, 6.60553E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 21, 2616133, 1, 71, 5.77932E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 22, 2616133, 1, 71, 5.04454E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 23, 2616133, 1, 71, 4.39303E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 24, 2616133, 1, 71, 3.81618E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 25, 2616133, 1, 71, 3.30569E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 26, 2616133, 1, 71, 2.04766E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 27, 2616133, 1, 71, 1.88706E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 28, 2616133, 1, 71, 1.73301E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 29, 2616133, 1, 71, 1.58552E-05);
INSERT INTO ExtendedIdleHours (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, extendedIdleHours) VALUES (43, 2001, 30, 2616133, 1, 71, 4.11438E-05);

-- 
-- Populate the Starts Table
--

INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 0, 261610, 7, 71, 90.0806);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 1, 261610, 7, 71, 86.4419);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 2, 261610, 7, 71, 80.3868);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 3, 261610, 7, 71, 94.6809);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 4, 261610, 7, 71, 84.2849);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 5, 261610, 7, 71, 74.7913);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 6, 261610, 7, 71, 66.0798);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 7, 261610, 7, 71, 58.0380);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 8, 261610, 7, 71, 50.5675);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 9, 261610, 7, 71, 43.5917);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 10, 261610, 7, 71, 37.0603);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 11, 261610, 7, 71, 30.9539);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 12, 261610, 7, 71, 25.2853);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 13, 261610, 7, 71, 20.0956);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 14, 261610, 7, 71, 14.7265);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 15, 261610, 7, 71, 10.0258);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 16, 261610, 7, 71, 6.7884);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 17, 261610, 7, 71, 4.5685);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 18, 261610, 7, 71, 3.0537);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 19, 261610, 7, 71, 2.0255);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 20, 261610, 7, 71, 1.3318);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 21, 261610, 7, 71, 0.8670);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 22, 261610, 7, 71, 0.5578);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 23, 261610, 7, 71, 0.3541);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 24, 261610, 7, 71, 0.2211);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 25, 261610, 7, 71, 0.1425);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 26, 261610, 7, 71, 0.6338);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 27, 261610, 7, 71, 0.4875);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 28, 261610, 7, 71, 0.3604);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 29, 261610, 7, 71, 0.2522);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (21, 2001, 30, 261610, 7, 71, 0.3243);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 0, 2616133, 1, 71, 0.0444);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 1, 2616133, 1, 71, 0.0431);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 2, 2616133, 1, 71, 0.0414);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 3, 2616133, 1, 71, 0.0743);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 4, 2616133, 1, 71, 0.0665);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 5, 2616133, 1, 71, 0.0595);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 6, 2616133, 1, 71, 0.0532);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 7, 2616133, 1, 71, 0.0475);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 8, 2616133, 1, 71, 0.0424);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 9, 2616133, 1, 71, 0.0378);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 10, 2616133, 1, 71, 0.0336);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 11, 2616133, 1, 71, 0.0299);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 12, 2616133, 1, 71, 0.0266);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 13, 2616133, 1, 71, 0.0236);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 14, 2616133, 1, 71, 0.0209);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 15, 2616133, 1, 71, 0.0185);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 16, 2616133, 1, 71, 0.0163);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 17, 2616133, 1, 71, 0.0144);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 18, 2616133, 1, 71, 0.0126);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 19, 2616133, 1, 71, 0.0111);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 20, 2616133, 1, 71, 0.0097);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 21, 2616133, 1, 71, 0.0085);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 22, 2616133, 1, 71, 0.0074);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 23, 2616133, 1, 71, 0.0065);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 24, 2616133, 1, 71, 0.0056);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 25, 2616133, 1, 71, 0.0049);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 26, 2616133, 1, 71, 0.0030);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 27, 2616133, 1, 71, 0.0028);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 28, 2616133, 1, 71, 0.0026);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 29, 2616133, 1, 71, 0.0023);
INSERT INTO Starts (SourceTypeID, YearID, AgeID, zoneID, MonthID, hourDayID, Starts) VALUES (43, 2001, 30, 2616133, 1, 71, 0.0061);

-- 
-- Populate the SHO Table
--

INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 0, 2616123, 7, 71, 12.4636);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 1, 2616123, 7, 71, 11.9601);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 2, 2616123, 7, 71, 11.1223);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 3, 2616123, 7, 71, 13.1001);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 4, 2616123, 7, 71, 11.6617);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 5, 2616123, 7, 71, 10.3482);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 6, 2616123, 7, 71, 9.1428);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 7, 2616123, 7, 71, 8.0302);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 8, 2616123, 7, 71, 6.9965);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 9, 2616123, 7, 71, 6.0314);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 10, 2616123, 7, 71, 5.1277);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 11, 2616123, 7, 71, 4.2828);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 12, 2616123, 7, 71, 3.4985);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 13, 2616123, 7, 71, 2.7804);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 14, 2616123, 7, 71, 2.0376);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 15, 2616123, 7, 71, 1.3872);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 16, 2616123, 7, 71, 0.9393);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 17, 2616123, 7, 71, 0.6321);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 18, 2616123, 7, 71, 0.4225);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 19, 2616123, 7, 71, 0.2802);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 20, 2616123, 7, 71, 0.1843);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 21, 2616123, 7, 71, 0.1200);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 22, 2616123, 7, 71, 0.0772);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 23, 2616123, 7, 71, 0.0490);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 24, 2616123, 7, 71, 0.0306);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 25, 2616123, 7, 71, 0.0197);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 26, 2616123, 7, 71, 0.0877);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 27, 2616123, 7, 71, 0.0675);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 28, 2616123, 7, 71, 0.0499);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 29, 2616123, 7, 71, 0.0349);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (21, 2001, 30, 2616123, 7, 71, 0.0449);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 0, 2616133, 1, 71, 0.0307);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 1, 2616133, 1, 71, 0.0298);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 2, 2616133, 1, 71, 0.0287);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 3, 2616133, 1, 71, 0.0514);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 4, 2616133, 1, 71, 0.0460);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 5, 2616133, 1, 71, 0.0412);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 6, 2616133, 1, 71, 0.0368);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 7, 2616133, 1, 71, 0.0329);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 8, 2616133, 1, 71, 0.0293);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 9, 2616133, 1, 71, 0.0261);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 10, 2616133, 1, 71, 0.0233);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 11, 2616133, 1, 71, 0.0207);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 12, 2616133, 1, 71, 0.0184);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 13, 2616133, 1, 71, 0.0163);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 14, 2616133, 1, 71, 0.0144);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 15, 2616133, 1, 71, 0.0128);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 16, 2616133, 1, 71, 0.0113);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 17, 2616133, 1, 71, 0.0099);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 18, 2616133, 1, 71, 0.0087);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 19, 2616133, 1, 71, 0.0077);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 20, 2616133, 1, 71, 0.0067);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 21, 2616133, 1, 71, 0.0059);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 22, 2616133, 1, 71, 0.0051);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 23, 2616133, 1, 71, 0.0045);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 24, 2616133, 1, 71, 0.0039);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 25, 2616133, 1, 71, 0.0034);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 26, 2616133, 1, 71, 0.0021);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 27, 2616133, 1, 71, 0.0019);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 28, 2616133, 1, 71, 0.0018);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 29, 2616133, 1, 71, 0.0016);
INSERT INTO SHO (SourceTypeID, YearID, AgeID, linkID, MonthID, hourDayID, SHO) VALUES (43, 2001, 30, 2616133, 1, 71, 0.0042);

--
-- Populate the SourceTypeAge Table
--

UPDATE SourceTypeAge SET FunctioningACFraction = 1 WHERE SourceTypeID = 21 AND AgeID =0;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.99 WHERE SourceTypeID = 21 AND AgeID =1;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.98 WHERE SourceTypeID = 21 AND AgeID =2;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.97 WHERE SourceTypeID = 21 AND AgeID =3;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.96 WHERE SourceTypeID = 21 AND AgeID =4;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.95 WHERE SourceTypeID = 21 AND AgeID =5;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.94 WHERE SourceTypeID = 21 AND AgeID =6;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.93 WHERE SourceTypeID = 21 AND AgeID =7;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.92 WHERE SourceTypeID = 21 AND AgeID =8;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.91 WHERE SourceTypeID = 21 AND AgeID =9;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.9 WHERE SourceTypeID = 21 AND AgeID =10;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.88 WHERE SourceTypeID = 21 AND AgeID =11;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.86 WHERE SourceTypeID = 21 AND AgeID =12;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.84 WHERE SourceTypeID = 21 AND AgeID =13;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.82 WHERE SourceTypeID = 21 AND AgeID =14;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.8 WHERE SourceTypeID = 21 AND AgeID =15;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.78 WHERE SourceTypeID = 21 AND AgeID =16;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.76 WHERE SourceTypeID = 21 AND AgeID =17;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.74 WHERE SourceTypeID = 21 AND AgeID =18;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.72 WHERE SourceTypeID = 21 AND AgeID =19;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.7 WHERE SourceTypeID = 21 AND AgeID =20;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.68 WHERE SourceTypeID = 21 AND AgeID =21;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.66 WHERE SourceTypeID = 21 AND AgeID =22;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.64 WHERE SourceTypeID = 21 AND AgeID =23;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.62 WHERE SourceTypeID = 21 AND AgeID =24;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.6 WHERE SourceTypeID = 21 AND AgeID =25;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.57 WHERE SourceTypeID = 21 AND AgeID =26;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.54 WHERE SourceTypeID = 21 AND AgeID =27;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.51 WHERE SourceTypeID = 21 AND AgeID =28;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.48 WHERE SourceTypeID = 21 AND AgeID =29;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.4 WHERE SourceTypeID = 21 AND AgeID =30;
UPDATE SourceTypeAge SET FunctioningACFraction = 1 WHERE SourceTypeID = 43 AND AgeID =0;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.99 WHERE SourceTypeID = 43 AND AgeID =1;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.98 WHERE SourceTypeID = 43 AND AgeID =2;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.97 WHERE SourceTypeID = 43 AND AgeID =3;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.96 WHERE SourceTypeID = 43 AND AgeID =4;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.95 WHERE SourceTypeID = 43 AND AgeID =5;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.94 WHERE SourceTypeID = 43 AND AgeID =6;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.93 WHERE SourceTypeID = 43 AND AgeID =7;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.92 WHERE SourceTypeID = 43 AND AgeID =8;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.91 WHERE SourceTypeID = 43 AND AgeID =9;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.9 WHERE SourceTypeID = 43 AND AgeID =10;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.88 WHERE SourceTypeID = 43 AND AgeID =11;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.86 WHERE SourceTypeID = 43 AND AgeID =12;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.84 WHERE SourceTypeID = 43 AND AgeID =13;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.82 WHERE SourceTypeID = 43 AND AgeID =14;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.8 WHERE SourceTypeID = 43 AND AgeID =15;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.78 WHERE SourceTypeID = 43 AND AgeID =16;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.76 WHERE SourceTypeID = 43 AND AgeID =17;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.74 WHERE SourceTypeID = 43 AND AgeID =18;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.72 WHERE SourceTypeID = 43 AND AgeID =19;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.7 WHERE SourceTypeID = 43 AND AgeID =20;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.68 WHERE SourceTypeID = 43 AND AgeID =21;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.66 WHERE SourceTypeID = 43 AND AgeID =22;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.64 WHERE SourceTypeID = 43 AND AgeID =23;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.62 WHERE SourceTypeID = 43 AND AgeID =24;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.6 WHERE SourceTypeID = 43 AND AgeID =25;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.57 WHERE SourceTypeID = 43 AND AgeID =26;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.54 WHERE SourceTypeID = 43 AND AgeID =27;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.51 WHERE SourceTypeID = 43 AND AgeID =28;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.48 WHERE SourceTypeID = 43 AND AgeID =29;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.4 WHERE SourceTypeID = 43 AND AgeID =30;
UPDATE SourceTypeAge SET FunctioningACFraction = 1 WHERE SourceTypeID = 42 AND AgeID =0;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.99 WHERE SourceTypeID = 42 AND AgeID =1;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.98 WHERE SourceTypeID = 42 AND AgeID =2;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.97 WHERE SourceTypeID = 42 AND AgeID =3;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.96 WHERE SourceTypeID = 42 AND AgeID =4;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.95 WHERE SourceTypeID = 42 AND AgeID =5;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.94 WHERE SourceTypeID = 42 AND AgeID =6;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.93 WHERE SourceTypeID = 42 AND AgeID =7;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.92 WHERE SourceTypeID = 42 AND AgeID =8;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.91 WHERE SourceTypeID = 42 AND AgeID =9;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.9 WHERE SourceTypeID = 42 AND AgeID =10;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.88 WHERE SourceTypeID = 42 AND AgeID =11;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.86 WHERE SourceTypeID = 42 AND AgeID =12;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.84 WHERE SourceTypeID = 42 AND AgeID =13;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.82 WHERE SourceTypeID = 42 AND AgeID =14;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.8 WHERE SourceTypeID = 42 AND AgeID =15;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.78 WHERE SourceTypeID = 42 AND AgeID =16;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.76 WHERE SourceTypeID = 42 AND AgeID =17;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.74 WHERE SourceTypeID = 42 AND AgeID =18;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.72 WHERE SourceTypeID = 42 AND AgeID =19;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.7 WHERE SourceTypeID = 42 AND AgeID =20;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.68 WHERE SourceTypeID = 42 AND AgeID =21;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.66 WHERE SourceTypeID = 42 AND AgeID =22;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.64 WHERE SourceTypeID = 42 AND AgeID =23;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.62 WHERE SourceTypeID = 42 AND AgeID =24;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.6 WHERE SourceTypeID = 42 AND AgeID =25;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.57 WHERE SourceTypeID = 42 AND AgeID =26;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.54 WHERE SourceTypeID = 42 AND AgeID =27;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.51 WHERE SourceTypeID = 42 AND AgeID =28;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.48 WHERE SourceTypeID = 42 AND AgeID =29;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.4 WHERE SourceTypeID = 42 AND AgeID =30;
UPDATE SourceTypeAge SET FunctioningACFraction = 1 WHERE SourceTypeID = 41 AND AgeID =0;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.99 WHERE SourceTypeID = 41 AND AgeID =1;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.98 WHERE SourceTypeID = 41 AND AgeID =2;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.97 WHERE SourceTypeID = 41 AND AgeID =3;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.96 WHERE SourceTypeID = 41 AND AgeID =4;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.95 WHERE SourceTypeID = 41 AND AgeID =5;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.94 WHERE SourceTypeID = 41 AND AgeID =6;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.93 WHERE SourceTypeID = 41 AND AgeID =7;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.92 WHERE SourceTypeID = 41 AND AgeID =8;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.91 WHERE SourceTypeID = 41 AND AgeID =9;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.9 WHERE SourceTypeID = 41 AND AgeID =10;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.88 WHERE SourceTypeID = 41 AND AgeID =11;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.86 WHERE SourceTypeID = 41 AND AgeID =12;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.84 WHERE SourceTypeID = 41 AND AgeID =13;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.82 WHERE SourceTypeID = 41 AND AgeID =14;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.8 WHERE SourceTypeID = 41 AND AgeID =15;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.78 WHERE SourceTypeID = 41 AND AgeID =16;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.76 WHERE SourceTypeID = 41 AND AgeID =17;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.74 WHERE SourceTypeID = 41 AND AgeID =18;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.72 WHERE SourceTypeID = 41 AND AgeID =19;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.7 WHERE SourceTypeID = 41 AND AgeID =20;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.68 WHERE SourceTypeID = 41 AND AgeID =21;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.66 WHERE SourceTypeID = 41 AND AgeID =22;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.64 WHERE SourceTypeID = 41 AND AgeID =23;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.62 WHERE SourceTypeID = 41 AND AgeID =24;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.6 WHERE SourceTypeID = 41 AND AgeID =25;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.57 WHERE SourceTypeID = 41 AND AgeID =26;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.54 WHERE SourceTypeID = 41 AND AgeID =27;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.51 WHERE SourceTypeID = 41 AND AgeID =28;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.48 WHERE SourceTypeID = 41 AND AgeID =29;
UPDATE SourceTypeAge SET FunctioningACFraction = 0.4 WHERE SourceTypeID = 41 AND AgeID =30;

-- 
-- Populate the HourDay Table
--

-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (11, 1, 1);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (21, 1, 2);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (31, 1, 3);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (41, 1, 4);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (51, 1, 5);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (61, 1, 6);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (71, 1, 7);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (81, 1, 8);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (91, 1, 9);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (101, 1, 10);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (111, 1, 11);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (121, 1, 12);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (131, 1, 13);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (141, 1, 14);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (151, 1, 15);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (161, 1, 16);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (171, 1, 17);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (181, 1, 18);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (191, 1, 19);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (201, 1, 20);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (211, 1, 21);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (221, 1, 22);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (231, 1, 23);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (241, 1, 24);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (12, 2, 1);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (22, 2, 2);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (32, 2, 3);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (42, 2, 4);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (52, 2, 5);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (62, 2, 6);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (72, 2, 7);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (82, 2, 8);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (92, 2, 9);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (102, 2, 10);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (112, 2, 11);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (122, 2, 12);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (132, 2, 13);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (142, 2, 14);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (152, 2, 15);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (162, 2, 16);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (172, 2, 17);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (182, 2, 18);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (192, 2, 19);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (202, 2, 20);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (212, 2, 21);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (222, 2, 22);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (232, 2, 23);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (242, 2, 24);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (13, 3, 1);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (23, 3, 2);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (33, 3, 3);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (43, 3, 4);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (53, 3, 5);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (63, 3, 6);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (73, 3, 7);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (83, 3, 8);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (93, 3, 9);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (103, 3, 10);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (113, 3, 11);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (123, 3, 12);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (133, 3, 13);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (143, 3, 14);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (153, 3, 15);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (163, 3, 16);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (173, 3, 17);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (183, 3, 18);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (193, 3, 19);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (203, 3, 20);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (213, 3, 21);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (223, 3, 22);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (233, 3, 23);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (243, 3, 24);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (14, 4, 1);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (24, 4, 2);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (34, 4, 3);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (44, 4, 4);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (54, 4, 5);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (64, 4, 6);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (74, 4, 7);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (84, 4, 8);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (94, 4, 9);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (104, 4, 10);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (114, 4, 11);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (124, 4, 12);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (134, 4, 13);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (144, 4, 14);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (154, 4, 15);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (164, 4, 16);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (174, 4, 17);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (184, 4, 18);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (194, 4, 19);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (204, 4, 20);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (214, 4, 21);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (224, 4, 22);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (234, 4, 23);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (244, 4, 24);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (15, 5, 1);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (25, 5, 2);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (35, 5, 3);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (45, 5, 4);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (55, 5, 5);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (65, 5, 6);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (75, 5, 7);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (85, 5, 8);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (95, 5, 9);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (105, 5, 10);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (115, 5, 11);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (125, 5, 12);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (135, 5, 13);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (145, 5, 14);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (155, 5, 15);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (165, 5, 16);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (175, 5, 17);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (185, 5, 18);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (195, 5, 19);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (205, 5, 20);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (215, 5, 21);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (225, 5, 22);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (235, 5, 23);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (245, 5, 24);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (16, 6, 1);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (26, 6, 2);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (36, 6, 3);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (46, 6, 4);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (56, 6, 5);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (66, 6, 6);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (76, 6, 7);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (86, 6, 8);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (96, 6, 9);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (106, 6, 10);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (116, 6, 11);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (126, 6, 12);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (136, 6, 13);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (146, 6, 14);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (156, 6, 15);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (166, 6, 16);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (176, 6, 17);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (186, 6, 18);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (196, 6, 19);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (206, 6, 20);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (216, 6, 21);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (226, 6, 22);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (236, 6, 23);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (246, 6, 24);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (17, 7, 1);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (27, 7, 2);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (37, 7, 3);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (47, 7, 4);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (57, 7, 5);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (67, 7, 6);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (77, 7, 7);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (87, 7, 8);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (97, 7, 9);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (107, 7, 10);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (117, 7, 11);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (127, 7, 12);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (137, 7, 13);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (147, 7, 14);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (157, 7, 15);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (167, 7, 16);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (177, 7, 17);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (187, 7, 18);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (197, 7, 19);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (207, 7, 20);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (217, 7, 21);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (227, 7, 22);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (237, 7, 23);
-- INSERT INTO HourDay (hourDayID, dayID, hourID) VALUES (247, 7, 24);

--
-- Populate the TemperatureAdustment Table
--

INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 1, -0.02262, 0.00016828);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 2, -0.00011, 0.00021);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 3, -0.00012, 0.00022);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 4, -0.00013, 0.00023);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 5, -0.00014, 0.00024);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 6, -0.00015, 0.00025);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 7, -0.00016, 0.00026);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 8, -0.00017, 0.00027);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9101, 9, -0.00018, 0.00028);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 1, -0.02262, 0.00016828);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 2, -0.00011, 0.00021);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 3, -0.00012, 0.00022);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 4, -0.00013, 0.00023);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 5, -0.00014, 0.00024);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 6, -0.00015, 0.00025);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 7, -0.00016, 0.00026);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 8, -0.00017, 0.00027);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9102, 9, -0.00018, 0.00028);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 1, -0.02262, 0.00016828);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 2, -0.00011, 0.00021);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 3, -0.00012, 0.00022);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 4, -0.00013, 0.00023);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 5, -0.00014, 0.00024);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 6, -0.00015, 0.00025);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 7, -0.00016, 0.00026);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 8, -0.00017, 0.00027);
INSERT INTO TemperatureAdjustment (sourceTypeID, polProcessID, fuelTypeID, TempAdjustTermA, TempAdjustTermB) VALUES (21, 9190, 9, -0.00018, 0.00028);

--
-- Populate the TemperatureAdustment Table
--

INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9101, 10, 1.00);
INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9101, 11, 1.10);
INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9101, 12, 0.90);
INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9102, 10, 1.00);
INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9102, 11, 1.10);
INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9102, 12, 0.90);
INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9190, 10, 1.00);
INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9190, 11, 1.10);
INSERT INTO FuelAdjustment (sourceTypeID, polProcessID, fuelSubtypeID, fuelAdjustment) VALUES (21, 9190, 12, 0.90);

--
-- Populate the MonthGroupHour Table
--

INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 1, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 2, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 3, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 4, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 5, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 6, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 7, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 8, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 9, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 10, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 11, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 12, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 13, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 14, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 15, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 16, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 17, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 18, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 19, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 20, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 21, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 22, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 23, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (1, 24, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 1, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 2, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 3, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 4, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 5, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 6, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 7, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 8, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 9, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 10, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 11, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 12, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 13, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 14, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 15, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 16, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 17, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 18, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 19, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 20, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 21, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 22, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 23, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (2, 24, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 1, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 2, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 3, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 4, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 5, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 6, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 7, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 8, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 9, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 10, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 11, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 12, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 13, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 14, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 15, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 16, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 17, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 18, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 19, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 20, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 21, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 22, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 23, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (3, 24, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 1, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 2, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 3, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 4, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 5, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 6, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 7, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 8, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 9, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 10, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 11, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 12, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 13, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 14, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 15, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 16, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 17, -5.307355, 0.113973, -0.000521);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 18, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 19, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 20, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 21, -2.930273, 0.05911, -0.000213);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 22, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 23, -1.257412, 0.006753, 0.000143);
INSERT INTO MonthGroupHour (monthGroupID, hourID, ACActivityTermA, ACActivityTermB, ACActivityTermC) VALUES (4, 24, -1.257412, 0.006753, 0.000143);

--
-- Populate the SourceTypeModelYear Table
--

UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.75 WHERE sourceTypeModelYearID = 211981 AND modelYearID = 1981 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.77 WHERE sourceTypeModelYearID = 211982 AND modelYearID = 1982 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.81 WHERE sourceTypeModelYearID = 211983 AND modelYearID = 1983 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.82 WHERE sourceTypeModelYearID = 211984 AND modelYearID = 1984 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.83 WHERE sourceTypeModelYearID = 211985 AND modelYearID = 1985 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.85 WHERE sourceTypeModelYearID = 211986 AND modelYearID = 1986 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.85 WHERE sourceTypeModelYearID = 211987 AND modelYearID = 1987 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.88 WHERE sourceTypeModelYearID = 211988 AND modelYearID = 1988 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.90 WHERE sourceTypeModelYearID = 211989 AND modelYearID = 1989 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.92 WHERE sourceTypeModelYearID = 211990 AND modelYearID = 1990 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.95 WHERE sourceTypeModelYearID = 211991 AND modelYearID = 1991 AND sourceTypeID = 21;
UPDATE SourceTypeModelYear SET ACPenetrationFraction = 0.96 WHERE sourceTypeModelYearID = 211992 AND modelYearID = 1992 AND sourceTypeID = 21;

--
-- Populate the FullACAdjustment Table
--

INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 1, 1.01);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 2, 1.02);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 3, 1.03);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 4, 1.04);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 5, 1.05);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 6, 1.06);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 7, 1.07);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 8, 1.08);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 9, 1.07);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 10, 1.06);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 11, 1.04);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 12, 1.02);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 13, 1.01);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400425303000, 14, 1.00);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 1, 1.02);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 2, 1.03);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 3, 1.04);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 4, 1.05);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 5, 1.06);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 6, 1.07);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 7, 1.08);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 8, 1.09);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 9, 1.08);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 10, 1.07);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 11, 1.05);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 12, 1.03);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 13, 1.02);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9101, 10101400430353000, 14, 1.01);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9102, 10101400425303000, 100, 1.01);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9102, 10101400430353000, 100, 1.02);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9190, 10101400425303000, 200, 1.15);
INSERT INTO FullACAdjustment (polProcessID, sourceBinID, opModeID, FullACAdjustment) VALUES (9190, 10101400430353000, 200, 1.12);

--
-- Populate the ZoneMonthHour Table
--

INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 1,  72.0, 72.1, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 2,  72.5, 72.6, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 3,  75.5, 75.6, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 4,  80.3, 80.4, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 5,  85.2, 85.3, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 6,  89.4, 89.5, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 7,  93.1, 93.2, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 8,  95.1, 95.2, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 9,  95.8, 95.9, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 10, 96.0, 96.1, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 11, 95.5, 95.6, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 12, 94.1, 94.2, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 13, 91.7, 91.8, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 14, 88.6, 88.7, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 15, 85.5, 85.6, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 16, 82.8, 82.9, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 17, 80.9, 81.0, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 18, 79.0, 79.1, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 19, 77.2, 77.3, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 20, 75.8, 75.9, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 21, 74.7, 74.8, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 22, 73.9, 74.0, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 23, 73.3, 73.4, 0);
INSERT INTO ZoneMonthHour (zoneID, monthID, hourID, heatIndex, temperature, relHumidity) VALUES (261610, 7, 24, 72.6, 72.7, 0);

--
-- Populate the FuelSupply Table
--

INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 10, 0.6000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 11, 0.3000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 12, 0.1000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 20, 0.8500);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 21, 0.0500);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 22, 0.1000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 30, 1.0000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 40, 1.0000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 50, 1.0000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 60, 1.0000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 70, 1.0000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 80, 1.0000);
INSERT INTO FuelSupply (countyID, yearID, monthGroupID, fuelSubtypeID, marketShare) VALUES (26161, 2001, 1, 90, 1.0000);

