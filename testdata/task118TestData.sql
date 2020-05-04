
USE MOVESDEFAULT;

-- NOTE: All previous test data scripts must be run prior to this.

--EngineSize
INSERT IGNORE INTO EngineSize (engSizeID, engSizeName) VALUES (9, "280 cc <= displacement  (for MCs)");
INSERT IGNORE INTO EngineSize (engSizeID, engSizeName) VALUES (2, "170 cc <= displacement < 280 cc (for MCs)");
INSERT IGNORE INTO EngineSize (engSizeID, engSizeName) VALUES (1, "displacement < 170 cc (for MCs)");


--OperatingMode
REPLACE INTO OperatingMode (opModeID, opModeName, VSPLower, VSPUpper, speedLower, 
	speedUpper, brakeRate1Sec, brakeRate3Sec) VALUES (33, "Cruise/Acceleration; VSP< 6; 50<=Speed", 
	6, 50, NULL, NULL, NULL, NULL);
REPLACE INTO OperatingMode (opModeID, opModeName, VSPLower, VSPUpper, speedLower, 
	speedUpper, brakeRate1Sec, brakeRate3Sec) VALUES (36, "Cruise/Acceleration; 12 <= VSP; 50<=Speed", 
	12, NULL, 50, NULL, NULL, NULL);
--INSERT IGNORE INTO OperatingMode (opModeID, opModeName, VSPLower, VSPUpper, speedLower, 
--	speedUpper, brakeRate1Sec, brakeRate3Sec) VALUES (100, "Starting (Used for all starts)", 
--	NULL, NULL, NULL, NULL, NULL, NULL);
--INSERT IGNORE INTO OperatingMode (opModeID, opModeName, VSPLower, VSPUpper, speedLower, 
--	speedUpper, brakeRate1Sec, brakeRate3Sec) VALUES (200, "Extended Idling", 
--	NULL, NULL, NULL, NULL, NULL, NULL);

--WeightClass
INSERT IGNORE INTO WeightClass (weightClassID, weightClassName, midpointWeight) VALUES(5, 
	"weight < 500 pounds (for MCs)", 350);
INSERT IGNORE INTO WeightClass (weightClassID, weightClassName, midpointWeight) VALUES(7,
	"500 pounds <= weight < 700 pounds (for MCs)", 600);
INSERT IGNORE INTO WeightClass (weightClassID, weightClassName, midpointWeight) VALUES(9,
	"700 pounds <= weight (for MCs)", 700);

--MonthOfAnyYear
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (1, "January", 31, 1);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES	(2, "February", 28, 2);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (3, "March", 31, 3);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (4, "April", 30, 4);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (5, "May", 31, 5);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (6, "June", 30, 6);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (7, "July", 31, 7);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (8, "August", 31, 8);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (9, "September", 30, 9);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (10, "October", 31, 10);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (11, "November", 30, 11);
REPLACE INTO MonthOfAnyYear (monthID, monthName, noOfDays, monthGroupID) VALUES (12, "December", 31, 12);

--MonthGroupOfAnyYear
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (1, "January");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (2, "February");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (3, "March");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (4, "April");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (5, "May");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (6, "June");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (7, "July");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (8, "August");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (9, "September");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (10, "October");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (11, "November");
REPLACE INTO MonthGroupOfAnyYear (monthGroupID, monthGroupName) VALUES (12, "December");
