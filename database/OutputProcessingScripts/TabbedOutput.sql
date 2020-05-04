-- This MySQL script produces tab-delimited output suitable for reading into an
-- EXCEL Spreadsheet from the MOVES MySQL database output tables.
-- 2008-07-07 changed "distance" to the more general "activity" in the activity output table.
-- 2009-07-14 removed MOVESOutputRowID from the script.
-- 2009-12-17 fixed file output names so that the script can be run multiple times
-- 2015-02-06 eliminated use of temporary table
-- 2015-09-10 added fuelsubtype
-- Three separate text files are produced.  They are:
--      MovesOutputyyyymmddhhmmss.txt
--      MovesActivityOutputyyyymmddhhmmss.txt
--      MovesRunyyyymmddhhmmss.txt

-- These correspond directly to the similarily named MOVES output tables.

-- The script does not write out the MovesError Table.

--
--
-- general:

FLUSH TABLES;

set @datetime = concat(  mid(curdate(),1,4),
                         mid(curdate(),6,2),
                         mid(curdate(),9,2),
                         mid(curtime(),1,2),
                         mid(curtime(),4,2),
                         mid(curtime(),7,2) );

-- create 'MovesOutputyyyymmddhhmmss.txt':

SET @sql_text =
   CONCAT (
"select 'MOVESRunID',    'IterationId',       'YearID',             'MonthID',           'DayID',
        'HourID',        'StateID',           'CountyID',           'ZoneID',            'LinkID',
        'PollutantID',   'ProcessID',         'SourceTypeID',       'regClassId',        'FuelTypeID',
        'fuelSubtypeid',
        'ModelYearID',   'RoadTypeID',        'SCC',                'engTechId',         'sectorId',
        'hpId',          'EmissionQuant',     'EmissionQuantMean',  'EmissionQuantSigma'"
" UNION ",
" SELECT movesoutput.*",
" INTO OUTFILE ", "'MovesOutput",
  @datetime,
  ".txt'",
  " FIELDS TERMINATED BY '\t'",
  " LINES TERMINATED BY '\r\n'"
  " from movesoutput;" );


PREPARE s1 FROM @sql_text;
EXECUTE s1;
DROP PREPARE s1;


-- create 'MovesActivityOutputyyyymmddhhmmss.txt':

SET @sql_text =
   CONCAT (
"select 'MOVESRunID',     'IterationId',  'YearID',        'MonthID',     'DayID',
        'HourID',         'StateID',      'CountyID',      'ZoneID',      'LinkID',
        'SourceTypeID',   'regClassId',   'FuelTypeID',    'ModelYearID', 'RoadTypeID',
        'SCC',            'engTechId',    'sectorId',      'hpId',        'ActivityTypeId',
        'Activity',       'ActivityMean', 'ActivitySigma'"
" UNION ",
" SELECT *",
" INTO OUTFILE ", "'MovesActivityOutput",
  @datetime,
  " .txt'",
  " FIELDS TERMINATED BY '\t'",
  " LINES TERMINATED BY '\r\n'"
  " from movesactivityoutput;" );


PREPARE s1 FROM @sql_text;
EXECUTE s1;
DROP PREPARE s1;


-- create 'MovesRunyyyymmddhhmmss.txt':

SET @sql_text =
   CONCAT (
"select 'MOVESRunID',          'outputTimePeriod',  'timeUnits',            'distanceUnits',
        'massUnits',           'energyUnits',       'runSpecFileName',      'runSpecDescription',
        'runSpecFileDateTime', 'runDateTime',       'scale',                'minutesDuration',
        'defaultDatabasrUsed', 'masterVersion',     'masterComputerId',     'masterIdNumber',
        'domain',              'domainCountryId',   'domainCountryName',    'domainDatabaseServer',
        'domainDataBaseName',  'expectedDONEfiles', 'retrievedDONEfiles',   'models'"
" UNION ",
" SELECT movesrun.*",
" INTO OUTFILE ", "'MovesRun",
  @datetime,
  " .txt'",
  " FIELDS TERMINATED BY '\t'",
  " LINES TERMINATED BY '\r\n'"
  " from movesrun;" );


PREPARE s1 FROM @sql_text;
EXECUTE s1;
DROP PREPARE s1;


FLUSH TABLES;