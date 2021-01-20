-- This MySQL script produces tab-delimited output suitable for reading into an
-- EXCEL Spreadsheet from the MOVES Maria database output tables.
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
CONCAT(
" SELECT * ",
" INTO OUTFILE ", "'MovesOutput",
	@datetime,
	".txt'",
	" FIELDS TERMINATED BY '\t'",
	" LINES TERMINATED BY '\r\n'",	
"FROM(",
	" select 'MOVESRunID',    'IterationId',       'YearID',             'MonthID',           'DayID',
        'HourID',        'StateID',           'CountyID',           'ZoneID',            'LinkID',
        'PollutantID',   'ProcessID',         'SourceTypeID',       'regClassId',        'FuelTypeID',
        'fuelSubtypeid',
        'ModelYearID',   'RoadTypeID',        'SCC',                'engTechId',         'sectorId',
        'hpId',          'EmissionQuant',     'EmissionQuantMean',  'EmissionQuantSigma'",
	" UNION ",
	" SELECT MOVESRunID,    IterationId,       YearID,             MonthID,           DayID,
        HourID,        StateID,           CountyID,           ZoneID,            LinkID,
        PollutantID,   ProcessID,         SourceTypeID,       regClassId,        FuelTypeID,
        fuelSubtypeid,
        ModelYearID,   RoadTypeID,        SCC,                engTechId,         sectorId,
        hpId,          EmissionQuant,     EmissionQuantMean,  EmissionQuantSigma ",
	" from movesoutput",
")as t1;");




PREPARE s1 FROM @sql_text;
EXECUTE s1;
DROP PREPARE s1;


-- create 'MovesActivityOutputyyyymmddhhmmss.txt':
SET @sql_text =
   CONCAT (
" SELECT * ",
" INTO OUTFILE ", "'MovesActivityOutput",
	@datetime,
	".txt'",
	" FIELDS TERMINATED BY '\t'",
	" LINES TERMINATED BY '\r\n'",	
"FROM(",
	" SELECT 'MOVESRunID',     'IterationId',  'YearID',        'MonthID',       'DayID',
			'HourID',         'StateID',      'CountyID',      'ZoneID',        'LinkID',
			'SourceTypeID',   'regClassId',   'FuelTypeID',    'fuelSubTypeId', 'ModelYearID',
			'RoadTypeID',     'SCC',          'engTechId',     'sectorId',      'hpId',
			'ActivityTypeId', 'Activity',     'ActivityMean',  'ActivitySigma'",
	" UNION ",
	" SELECT MOVESRunID,     IterationId,  YearID,        MonthID,       DayID,
        HourID,         StateID,      CountyID,      ZoneID,        LinkID,
        SourceTypeID,   regClassId,   FuelTypeID,    fuelSubTypeId, ModelYearID,
        RoadTypeID,     SCC,          engTechId,     sectorId,      hpId,
        ActivityTypeId, Activity,     ActivityMean,  ActivitySigma",
	" FROM MovesActivityOutput",
")as t1;");


PREPARE s2 FROM @sql_text;
EXECUTE s2;
DROP PREPARE s2;


-- create 'MovesRunyyyymmddhhmmss.txt':

SET @sql_text =
CONCAT (
" SELECT * ",
" INTO OUTFILE ", "'movesrun",
	@datetime,
	".txt'",
	" FIELDS TERMINATED BY '\t'",
	" LINES TERMINATED BY '\r\n'",	
"FROM(",
	" SELECT 'MOVESRunID',          'outputTimePeriod',  'timeUnits',            'distanceUnits',
        'massUnits',           'energyUnits',       'runSpecFileName',      'runSpecDescription',
        'runSpecFileDateTime', 'runDateTime',       'scale',                'minutesDuration',
        'defaultDatabaseUsed', 'masterVersion',     'masterComputerId',     'masterIdNumber',
        'domain',              'domainCountyId',   'domainCountyName',    'domainDatabaseServer',
        'domainDataBaseName',  'expectedDONEfiles', 'retrievedDONEfiles',   'models'",
	" UNION ",
	" SELECT MOVESRunID,          outputTimePeriod,  timeUnits,            distanceUnits,
        massUnits,           energyUnits,       runSpecFileName,      runSpecDescription,
        runSpecFileDateTime, runDateTime,       scale,                minutesDuration,
        defaultDatabaseUsed, masterVersion,     masterComputerId,     masterIdNumber,
        domain,              domainCountyId,   domainCountyName,    domainDatabaseServer,
        domainDataBaseName,  expectedDONEfiles, retrievedDONEfiles,   models",
	" FROM movesrun",
")as t1;");


PREPARE s3 FROM @sql_text;
EXECUTE s3;
DROP PREPARE s3;


FLUSH TABLES;
