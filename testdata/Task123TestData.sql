-- MySQL script file to create operating mode 300 representing
--   all running conditions, 
-- And associate it with 
--   OpModePolProcess 501: Methane - Running
--   OpModePolProcess 601: N2O - Running
-- Also associate opmode 100 with
--   OpModePolProcess 502: Methane - Starting
--   OpModePolProcess 602: N2O - Starting
-- Modifies currently active database (Does not include USE command)

INSERT IGNORE INTO OperatingMode (opModeID, opModeName)
	VALUES (300, "All Running");
	
INSERT IGNORE INTO OpModePolProcAssoc (polProcessID, opModeID)
	VALUES (501, 300);
INSERT IGNORE INTO OpModePolProcAssoc (polProcessID, opModeID)
	VALUES (601, 300);
INSERT IGNORE INTO OpModePolProcAssoc (polProcessID, opModeID)
	VALUES (502, 100);
INSERT IGNORE INTO OpModePolProcAssoc (polProcessID, opModeID)
	VALUES (602, 100);

-- Add sample values to EmissionRate table for each Source Bin, Operating Mode, and Pollutant/Process
INSERT IGNORE INTO EmissionRate (sourceBinID,polProcessID,opModeID,meanBaseRate)
SELECT DISTINCT sourceBinID,501,300,501*2
FROM SourceBin;

INSERT IGNORE INTO EmissionRate (sourceBinID,polProcessID,opModeID,meanBaseRate)
SELECT DISTINCT sourceBinID,502,100,502*2
FROM SourceBin;

INSERT IGNORE INTO EmissionRate (sourceBinID,polProcessID,opModeID,meanBaseRate)
SELECT DISTINCT sourceBinID,601,300,601*2
FROM SourceBin;

INSERT IGNORE INTO EmissionRate (sourceBinID,polProcessID,opModeID,meanBaseRate)
SELECT DISTINCT sourceBinID,602,100,602*2
FROM SourceBin;


-- Add sample values to GREETWellToPump for each Year, Pollutant (5, 6), and Fuel Subtype
INSERT IGNORE INTO GREETWellToPump (yearID, pollutantID, fuelSubTypeID, emissionRate)
SELECT DISTINCT yearID, pollutantID, fuelSubTypeID, (yearID/2050*pollutantID/6) as emissionRate
FROM FuelSubType, year, pollutant
WHERE yearID >= 1990 and yearID <= 2050 and Mod(yearID,5) = 0
AND pollutantID IN (5,6);
