-- This SQL script produces an output table which reports your
-- onroad emission results as a rate per distance.  This is done
-- by joining the activity table with the inventory output results.
-- 
-- The results can be found in a new table in the output database 
-- called `movesrates`.
-- 
-- This script requires "Distance Traveled" to be selected on the
-- General Output panel, and "Inventory" to be selected on the Scale
-- panel.
-- 
-- This only produces "back of the envelope" emission rates, and
-- is not intended for regulatory use. Use "Emission Rates" mode
-- on the Scale panel for more precise emission rates.


FLUSH TABLES;

-- Create the table to hold the calculation results.
DROP TABLE IF EXISTS MOVESRates;
CREATE TABLE `MOVESRates` (
  `MOVESRunID`  smallint(5) unsigned    NOT NULL,
  `iterationID` smallint(5) unsigned    NOT NULL default '1',
  `yearID`      smallint(5) unsigned    NOT NULL,
  `monthID`     smallint(5) unsigned    NOT NULL default '0',
  `dayID`       smallint(5) unsigned    NOT NULL default '0',
  `hourID`      smallint(5) unsigned    NOT NULL default '0',
  `stateID`     smallint(5) unsigned    NOT NULL default '0',
  `countyID`    int(10)     unsigned    NOT NULL default '0',
  `zoneID`      int(10)     unsigned    NOT NULL default '0',
  `linkID`      int(10)     unsigned    NOT NULL default '0',
  `pollutantID` smallint(5) unsigned    NOT NULL,
  `processID`      smallint(5) unsigned NOT NULL default '0',
  `sourceTypeID`   smallint(5) unsigned NOT NULL default '0',
  `regClassID`     smallint(5) unsigned NOT NULL default '0',
  `fuelTypeID`     smallint(5) unsigned NOT NULL default '0',
  `modelYearID`    smallint(5) unsigned NOT NULL default '0',
  `roadTypeID`     smallint(5) unsigned NOT NULL default '0',
  `emissionQuant`  double               NOT NULL default '0',
  `activity`       double               NOT NULL default '0',
  `emissionRate`   double               NOT NULL default '0',
  `units`          varchar(10)             NOT NULL default  '',
  PRIMARY KEY (`MOVESRunID`, `iterationID`, `yearID`, `monthID`, `dayID`, `hourID`, `stateID`, `countyID`, `zoneID`, `linkID`, `pollutantID`,
                `processID`, `sourceTypeID`, `regClassID`, `fuelTypeID`, `modelYearID`, `roadTypeID`)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci' DELAY_KEY_WRITE=1
 ;

-- create temp table for inventory to remove NULLs which prevent joins from working (and set primary key to make eventual join fast)
DROP TABLE IF EXISTS `movesrates_tmp_inv`;
CREATE TABLE `movesrates_tmp_inv` (
  `MOVESRunID`  smallint(5) unsigned    NOT NULL,
  `iterationID` smallint(5) unsigned    NOT NULL default '1',
  `yearID`      smallint(5) unsigned    NOT NULL,
  `monthID`     smallint(5) unsigned    NOT NULL default '0',
  `dayID`       smallint(5) unsigned    NOT NULL default '0',
  `hourID`      smallint(5) unsigned    NOT NULL default '0',
  `stateID`     smallint(5) unsigned    NOT NULL default '0',
  `countyID`    int(10)     unsigned    NOT NULL default '0',
  `zoneID`      int(10)     unsigned    NOT NULL default '0',
  `linkID`      int(10)     unsigned    NOT NULL default '0',
  `pollutantID` smallint(5) unsigned    NOT NULL,
  `processID`      smallint(5) unsigned NOT NULL default '0',
  `sourceTypeID`   smallint(5) unsigned NOT NULL default '0',
  `regClassID`     smallint(5) unsigned NOT NULL default '0',
  `fuelTypeID`     smallint(5) unsigned NOT NULL default '0',
  `modelYearID`    smallint(5) unsigned NOT NULL default '0',
  `roadTypeID`     smallint(5) unsigned NOT NULL default '0',
  `emissionQuant`  double               NOT NULL default '0',
  PRIMARY KEY (`MOVESRunID`, `iterationID`, `yearID`, `monthID`, `dayID`, `hourID`, `stateID`, `countyID`, `zoneID`, `linkID`, 
               `pollutantID`, `processID`, `sourceTypeID`, `regClassID`, `fuelTypeID`, `modelYearID`, `roadTypeID`)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci' DELAY_KEY_WRITE=1;
INSERT INTO `movesrates_tmp_inv`
SELECT COALESCE(`MOVESRunID`, 0), COALESCE(`iterationID`, 0), COALESCE(`yearID`, 0), COALESCE(`monthID`, 0), COALESCE(`dayID`, 0),
       COALESCE(`hourID`, 0), COALESCE(`stateID`, 0), COALESCE(`countyID`, 0), COALESCE(`zoneID`, 0), COALESCE(`linkID`, 0), 
       COALESCE(`pollutantID`, 0), COALESCE(`processID`, 0), COALESCE(`sourceTypeID`, 0), COALESCE(`regClassID`, 0), COALESCE(`fuelTypeID`, 0), 
       COALESCE(`modelYearID`, 0), COALESCE(`roadTypeID`, 0), COALESCE(`emissionQuant`, 0)
FROM `movesoutput`;

-- create temp table for distance to remove NULLs which prevent joins from working (and set primary key to make eventual join fast)
DROP TABLE IF EXISTS `movesrates_tmp_dist`;
CREATE TABLE `movesrates_tmp_dist` (
  `MOVESRunID`  smallint(5) unsigned    NOT NULL,
  `iterationID` smallint(5) unsigned    NOT NULL default '1',
  `yearID`      smallint(5) unsigned    NOT NULL,
  `monthID`     smallint(5) unsigned    NOT NULL default '0',
  `dayID`       smallint(5) unsigned    NOT NULL default '0',
  `hourID`      smallint(5) unsigned    NOT NULL default '0',
  `stateID`     smallint(5) unsigned    NOT NULL default '0',
  `countyID`    int(10)     unsigned    NOT NULL default '0',
  `zoneID`      int(10)     unsigned    NOT NULL default '0',
  `linkID`      int(10)     unsigned    NOT NULL default '0',
  `sourceTypeID`   smallint(5) unsigned NOT NULL default '0',
  `regClassID`     smallint(5) unsigned NOT NULL default '0',
  `fuelTypeID`     smallint(5) unsigned NOT NULL default '0',
  `modelYearID`    smallint(5) unsigned NOT NULL default '0',
  `roadTypeID`     smallint(5) unsigned NOT NULL default '0',
  `activity`    double                  NOT NULL default '0',
  PRIMARY KEY (`MOVESRunID`, `iterationID`, `yearID`, `monthID`, `dayID`, `hourID`, `stateID`, `countyID`, `zoneID`, `linkID`,
               `sourceTypeID`, `regClassID`, `fuelTypeID`, `modelYearID`, `roadTypeID`)
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci' DELAY_KEY_WRITE=1;
INSERT INTO `movesrates_tmp_dist`
SELECT COALESCE(`MOVESRunID`, 0), COALESCE(`iterationID`, 0), COALESCE(`yearID`, 0), COALESCE(`monthID`, 0), COALESCE(`dayID`, 0),
       COALESCE(`hourID`, 0), COALESCE(`stateID`, 0), COALESCE(`countyID`, 0), COALESCE(`zoneID`, 0), COALESCE(`linkID`, 0), COALESCE(`sourceTypeID`, 0), 
       COALESCE(`regClassID`, 0), COALESCE(`fuelTypeID`, 0), COALESCE(`modelYearID`, 0), COALESCE(`roadTypeID`, 0), COALESCE(`activity`, 0)
FROM `movesactivityoutput`
WHERE `activityTypeID` = 1;

-- insert the total distance for roadTypeID 1, since that has no VMT of its own
INSERT INTO `movesrates_tmp_dist`
SELECT `MOVESRunID`, `iterationID`, `yearID`, `monthID`, `dayID`, `hourID`, `stateID`, `countyID`, `zoneID`, `linkID`,
       `sourceTypeID`, `regClassID`, `fuelTypeID`, `modelYearID`, 1 as `roadTypeID`, SUM(`activity`) as `activity`
FROM `movesrates_tmp_dist`
GROUP BY `MOVESRunID`, `iterationID`, `yearID`, `monthID`, `dayID`, `hourID`, `stateID`, `countyID`, `zoneID`, `linkID`,
         `sourceTypeID`, `regClassID`, `fuelTypeID`, `modelYearID`;

-- join the inventory table to the distance table, calculate rates & units, and insert results into the output table
INSERT INTO `MOVESRates`
SELECT `MOVESRunID`, `iterationID`, `yearID`, `monthID`, `dayID`, `hourID`, `stateID`, `countyID`, `zoneID`, `linkID`, `pollutantID`,
       `processID`, `sourceTypeID`, `regClassID`, `fuelTypeID`, `modelYearID`, `roadTypeID`, `emissionQuant`, `activity`, 
       `emissionQuant`/`activity` as `emissionRate`,
       CONCAT(CASE WHEN `pollutantID` = 91 THEN `energyUnits` ELSE `massUnits` END, '/', `distanceUnits`) as `units`
FROM `movesrates_tmp_inv`
JOIN `movesrates_tmp_dist` USING (`MOVESRunID`, `iterationID`, `yearID`, `monthID`, `dayID`, `hourID`, `stateID`, 
                                 `countyID`, `zoneID`, `linkID`, `sourceTypeID`, `regClassID`, `fuelTypeID`,
                                 `modelYearID`, `roadTypeID`)
JOIN `movesrun` USING (`MOVESRunID`);

-- clean up
drop table `movesrates_tmp_inv`;
drop table `movesrates_tmp_dist`;

FLUSH TABLES;
