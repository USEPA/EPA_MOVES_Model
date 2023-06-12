-- MOVES3 NEI QA Script for Nonroad CDBs
-- Last Updated: MOVES3.0.1
-- ##############################################################################
-- Usage Notes:  This script is intended to be run via the ANT command 
--               "nonroadNEIQA". See database\NEIQA\NEIQAHelp.pdf for more info.
--               If this script needs to be run directly against MariaDB (i.e.,
--               using mysql.exe or through Workbench/Heidi:
--               1. Find & Replace "BeginBlock" with "Delimiter $$"
--               2. Find & Replace "EndBlock" with "$$\nDelimiter ;" (converting
--                     the \n to a new line)
--               3. Find & Replace "##defaultdb##" with the name of the default 
--                     database, as found in MOVESConfiguration.txt
-- ##############################################################################

set @version = MID("##defaultdb##", 8, 8);

DROP   PROCEDURE IF EXISTS NEI_QA_NR_M3;
BeginBlock
CREATE PROCEDURE NEI_QA_NR_M3()
BEGIN

declare nErrors   int;
declare nWarnings int;
declare tblNo     int;
declare tblName   char(30) COLLATE utf8_general_ci;
declare numRows   int;
declare errMsg    char(30) COLLATE utf8_general_ci;
declare Q         int;
declare dataBaseName char(30) COLLATE utf8_general_ci;

   select '  .. M3nonroadCDBchecks.sql',curTime(), database();

-- Create a table to contain the results of the table checks.
drop table if exists CDB_Checks;
CREATE TABLE CDB_Checks (
   countyID          char(10),
   `status`          char(20),
   tableId           int,
   tableName         char(100),
   checkNumber       int,
   testDescription   char(250),
   testValue         text,
   `count`           int(11),
   dataBaseName      char(100),
   dayID             smallint(6),
   fuelFormulationID INT(11),
   fuelTypeId        smallint(6),
   fuelSubtypeID     smallint(6),
   fuelYearID        smallint(6),
   hourDayID         smallint(6),
   hourID            smallint(6),
   HPMSVtypeID       smallint(6),
   monthGroupID      smallint(6),
   monthID           smallint(6),
   roadTypeID        smallint(6),
   sourceTypeID      smallint(6),
   stateID           smallint(6),
   yearID            smallint(6),
   zoneID            int(11),
   msgType           char(50),
   msgDate           date,
   msgTime           time,
   version       char(8),
   sumKeyID          int(11),
   sumKeyDescription char(50)
 ) ENGINE=MyISAM;
 
-- This table contains entries for every QA check, so if this script fails, looking at the last row
-- should help determine where the error occurred.
drop table if exists QA_Checks_Log;
create table
  QA_Checks_Log
  (
    tableId     int,
    tableName   char(30),
    checkNo     int,
    description char(30),
    status      char(20)
  ) ENGINE=MyISAM;

  set nErrors   = 0;
  set nWarnings = 0;

  --       Table  Check: fuelFormulation *start*
-- check no. 2000 -- check that fuelFormulation exists (info if not) or is filled (warning) -- empty is ok!
  set tblName = 'fuelFormulation';
  set tblNo   =  20;
  set errMsg  = '';

  set numRows =  ( select table_rows
                  from   information_schema.tables
                  where  table_schema = database()
                    and  table_name = tblName );

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    2000,     'init table check', 'unknown' );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,    status )
                                                 values ( tblNo,   tblName,   2000,  'Missing Table','Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 2000,  'Missing Table',  'INFO');
                                                 set nWarnings = nWarnings + 1;
                                                 set errMsg  = 'Missing Table';
  -- no message or further checks if table is empty
  elseif numRows > 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,    status )
                                                 values ( tblNo,   tblName,   2000,  'Table likely to be overwritten',  'Warning' );

                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 2000, 'Table likely to be overwritten',  'WARNING'  );

	--       check no. 2001 -- check for unknown fuelSubTypeIDs
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,     status    )
					   values ( tblNo,  'fuelFormulation', 2001,   'fuelSubTypeId', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   fuelSubTypeId,
			 fuelFormulationId,
			 'no '         as aMatch,
			 count(*)      as n
	From     fuelFormulation
	Group by fuelFormulationId;

	Update tempA as a set aMatch='yes' where (Select m.fuelsubtypeId
											  From   ##defaultdb##.nrfuelsubtype as m
											  Where  a.fuelSubTypeId = m.fuelSubTypeId)>0;

	Insert into CDB_Checks
		   ( TableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelSubTypeId,
			 fuelFormulationId )
	Select   tblNo            as tableId,
			"fuelFormulation" as tableName,
			 2001             as CheckNumber,
			"fuelSubTypeId"   as testDescription,
			 fuelSubTypeId    as testValue,
			 n                as count,
			 'Error'          as status,
			 fuelSubTypeId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';


	--       check no. 2002 -- checks that RVP is between 5 and 20 for gasoline
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,  status    )
					   values ( tblNo,  'fuelFormulation', 2002,    'RVP',       'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   RVP,
			 fuelSubTypeId,
			 fuelFormulationId,
			 'no '         as aMatch,
			 count(*)      as n
	From     fuelFormulation
	Group by fuelFormulationId;

	Update tempA as a set aMatch='yes' where RVP>=5.0 and RVP<=20.0 and fuelSubTypeId     in (10,11,12,13,14,15);
	Update tempA as a set aMatch='yes' where                            fuelSubTypeId not in (10,11,12,13,14,15);

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 fuelSubTypeId,
			 fuelFormulationId,
			 count,
			 status   )
	Select   tblNo as tableId,
			'fuelFormulation' as tableName,
			 2002             as checkNumber,
			'RVP'             as testDescription,
			 RVP              as testValue,
			 fuelSubtypeId,
			 fuelFormulationId,
			 n                as count,
			'Error'           as status
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2003 -- checks that sulfurLevel is between 0 and 5000 for all fuels
	insert into QA_Checks_Log ( tableId, tableName,        checkNo,  description,   status    )
					   values ( tblNo,  'fuelFormulation', 2003,    'sulfurLevel', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   sulfurLevel,
			 fuelFormulationId,
			 'no '             as aMatch,
			 count(*)          as n
	From     fuelFormulation
	Group by fuelFormulationId;

	update tempA as a set aMatch='yes' where sulfurLevel>=0.0 and sulfurLevel<=5000.0;   -- real test

	Insert into CDB_Checks
		   ( TableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelFormulationId  )
	Select   tblNo              as tableId,
			'fuelFormulation'   as tableName,
			 2003               as checkNumber,
			'sulfurLevel'       as testDescription,
			 sulfurLevel        as testValue,
			 n                  as count,
			'Error'             as status,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2004 -- checks that ETOH is between 0 and 100 for gasoline fuel subtypes
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,   status    )
					   values ( tblNo,  'fuelFormulation', 2004,    'ETOHVolume', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   ETOHVolume,
			 'no '             as aMatch,
			 count(*)          as n,
			 fuelsubtypeId,
			 fuelFormulationId
	From     fuelFormulation
	Where    fuelsubtypeId in (10,11,12,13,14,15)   
	  and    fuelformulationId >=100                
	Group by fuelFormulationId;

	Update tempA as a set aMatch='yes' where ETOHVolume>=0.0 and ETOHVolume<=100.0;

	Insert into CDB_Checks
		   ( TableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelSubTypeId,
			 fuelFormulationId  )
	Select   tblNo,
			'fuelFormulation' as tableName,
			 2004,
			'ETOHVolume'      as testDescription,
			 ETOHVolume       as testValue,
			 n                as count,
			'Error'           as status,
			 fuelSubTypeId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2005 -- checks that MTBE is 0 or NULL
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,  status    )
					   values ( tblNo,  'fuelFormulation', 2005,   'MTBEVolume', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   MTBEVolume,
			 'no '         as aMatch,
			 count(*)      as n,
			 fuelsubtypeId,
			 fuelFormulationId
	From     fuelFormulation
	Where    fuelsubtypeid in (10,11,12,13,14,15)
	  and    fuelformulationid >=100
	Group by fuelFormulationId;

	Update tempA as a set aMatch='yes' where MTBEVolume=0 or MTBEVolume is NULL;

	Insert into CDB_Checks
		   ( TableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelsubtypeId,
			 fuelFormulationId )
	Select   tblNo,
			'fuelFormulation' as tableName,
			 2005,
			'MTBEVolume'      as testDescription,
			 MTBEVolume       as testValue,
			 n                as count,
			'Error'           as status,
			 fuelsubtypeId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2006 -- checks that ETBE is 0 or NULL
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,         status    )
					   values ( tblNo,  'fuelFormulation', 2006,   'ETBEVolume', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   fuelFormulationId,
			'no '         as aMatch,
			 count(*)     as n,
			 ETBEVolume,
			 fuelsubtypeId
	From     fuelFormulation
	Where    fuelsubtypeId in (10,11,12,13,14,15)
	  and    fuelformulationid >=100
	Group by fuelFormulationId;

	Update tempA as a set aMatch='yes' where ETBEVolume=0 OR ETBEVolume is NULL;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelFormulationId,
			 fuelSubTypeId )
	Select   tblNo            as tableId,
			'fuelFormulation' as tableName,
			 2006             as checkNmber,
			'ETBEVolume'      as testDescription,
			 ETBEVolume       as testValue,
			 n                as count,
			'Error'           as status,
			 fuelFormulationId,
			 fuelSubtypeId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2007 -- check that TAME is 0 or NULL
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,  status    )
					   values ( tblNo,  'fuelFormulation', 2007,   'TAMEVolume', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   TAMEVolume,
			 'no '              as aMatch,
			 count(*)           as n,
			 fuelFormulationId,
			 fuelSubtypeid
	From     fuelFormulation
	Where    fuelsubtypeid in (10,11,12,13,14,15)
	  and    fuelformulationid >=100             
	Group by fuelFormulationId;

	Update tempA as a set aMatch='yes' where TAMEVolume=0 or TAMEVolume is NULL;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelFormulationId,
			 fuelSubtypeId )
	Select   tblNo            as tableId,
			'fuelFormulation' as tableName,
			 2007             as checkNumber,
			 'TAMEVolume'     as testDescription,
			 TAMEVolume       as testValue,
			 n                as count,
			'Error'           as status,
			 fuelFormulationId,
			 fuelSubtypeId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2008 -- check that aromaticContent is between 0 and 55 for gasoline
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,       status    )
					   values ( tblNo,  'fuelFormulation', 2008,   'aromaticContent', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   aromaticContent,
			 'no '           as aMatch,
			 count(*)        as n,
			 fuelsubtypeId,
			 fuelFormulationId
	From     fuelFormulation
	Where    fuelsubtypeid in (10,11,12,13,14,15)  
	  and    fuelformulationid >=100               
	Group by fuelSubTypeId,
			 fuelFormulationId;

	Update tempA as a set aMatch='yes' where aromaticContent>=0.0 and aromaticContent<=55.0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelsubtypeId,
			 fuelFormulationId  )
	Select   tblNo,
			'fuelFormulation'  as tableName,
			 2008              as CheckNumber,
			'aromaticContent'  as testDescription,
			 aromaticContent   as testValue,
			 n                 as count,
			'Error'            as status,
			 fuelsubtypeId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2009 -- check that olefinContent is between 0 and 25 gasoline
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,     status    )
					   values ( tblNo,  'fuelFormulation', 2009,   'olefinContent', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   olefinContent as olefinContent2,
			 'no '           as aMatch,
			 count(*)        as n,
			 fuelsubtypeId,
			 fuelFormulationId
	From     fuelFormulation
	Where    fuelsubtypeid in (10,11,12,13,14,15)
	  and    fuelformulationid >=100
	Group by olefinContent2,
			 fuelSubtypeId,
			 fuelFormulationId;

	Update tempA as a set aMatch='yes' where olefinContent2>=0.0 and olefinContent2<=25.0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count ,
			 status,
			 fuelFormulationId  )
	Select   tblNo            as tableId,
			'fuelFormulation' as tableName,
			 2009             as CheckNumber,
			'olefinContent'   as testDescription,
			 olefinContent2   as testValue,
			 n                as count,
			'Error'           as status,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2010 -- check that benzene is between 0 and 5 for gasoline
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description,      status    )
					   values ( tblNo,  'fuelFormulation', 2010,   'benzeneContent', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   benzeneContent  as benzeneContent2,
			 'no '           as aMatch,
			 count(*)        as n,
			 fuelSubtypeId,
			 fuelFormulationId
	From     fuelFormulation
	Where    fuelsubtypeid in (10,11,12,13,14,15)
	   and    fuelformulationid >=100
	Group by benzeneContent2,
			 fuelSubtypeId,
			 fuelFormulationId;

	Update tempA as a set aMatch='yes' where benzeneContent2>=0.0 and benzeneContent2<=5.0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelSubtypeId,
			 fuelFormulationId  )
	Select   tblNo             as tableId,
			'fuelFormulation'  as tableName,
			 2010              as checkNumber,
			 'benzeneContent'  as testDescription,
			 benzeneContent2   as testValue,
			 n                 as count,
			'Error'            as status,
			 fuelSubtypeId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';


	--       check no. 2011 -- check that e200 is between 0 and 70 for gasoline
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description, status    )
					   values ( tblNo,  'fuelFormulation', 2011,   'e200',      'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   e200,
			 'no '           as aMatch,
			 count(*)        as n,
			 fuelSubtypeId,
			 fuelFormulationId
	From     fuelFormulation
	Where    fuelsubtypeid in (10,11,12,13,14,15)
	   and    fuelformulationid >=100            
	Group by e200,
			 fuelsubtypeId,
			 fuelFormulationId;

	Update tempA as a set aMatch='yes' where e200>=0.0 and e200<=70.0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelSubtypeId,
			 fuelFormulationId  )
	Select   tblNo            as tableId,
			'fuelFormulation' as tableName,
			 2011             as checkNumber,
			'e200'            as testDescription,
			 e200             as testValue,
			 n                as count,
			'Error'           as status,
			 fuelSubtypeId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2012 -- check that e300 is between 0 and 100 for gasoline
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description, status    )
					   values ( tblNo,  'fuelFormulation', 2012,   'e300',      'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   e300,
			 'no '           as aMatch,
			 count(*)        as n,
			 fUelSubtypeId,
			 fuelFormulationId
	From     fuelFormulation
	Where    fuelsubtypeid in (10,11,12,13,14,15)
	  and    fuelformulationid >=100            
	Group by e300,
			 fuelSuBtypeId,
			 fuelFormulationId;

	Update tempA as a set aMatch='yes' where e300>=0.0 and e300<=100.0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuElSubtypeId,
			 fuelFormulationId  )
	Select   tblNo            as tableId,
			'fuelFormulation' as tableName,
			 2012             as checkNumber,
			'e300'            as testDescription,
			 e300             as testValue,
			 n                as count,
			'Error'           as status,
			 fueLSubtypeId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 2013 -- check that t50/t90 columns exist
	insert into QA_Checks_Log ( tableId, tableName,        checkNo, description, status    )
					   values ( tblNo,  'fuelFormulation', 2013,   't50/t90',      'checking' );
    Insert into CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             count,
             status)
    values
            (tblNo, 
             'fuelFormulation',
             2013,
             "T50 and/or T90 Missing",
             "Column count (should be 2):",
             (Select count(*)
              from   information_schema.columns
              where  table_name   = 'fuelformulation'
                and  column_name in ('t50', 't90')
                and  table_schema = database()),
             'ERROR');
    Delete from CDB_Checks where checkNumber=2013 and `count`=2;

    --       check no. 2014: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    2014, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            2014 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

 end if; -- end checks for fuelFormulation

 INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                              values ( tblNo,   tblName,   2000,          'Table Check',     'Completed' );
  --       Table  Check: fuelformulation *end*


  --       Table  Check: zoneMonthHour *start*
-- check no. 5000 -- check that zoneMonthHour exists (error if not) or is filled (warning) -- empty is ok!
  set tblName = 'zoneMonthHour';
  set tblNo   = 50;
  set errMsg  = '';

  set numRows = ( select table_rows
                  from   information_schema.tables
                  where  table_schema = database()
                    and  table_name = tblName );

    insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                       values ( tblNo,   tblName,    5000,     'init table check', 'unknown' );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,    status )
                                                 values ( tblNo,   tblName,   5000,  'Missing Table','Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 5000,  'Missing Table',  'INFO');
  -- no warning or further checks if table is empty                                                         
  elseif numRows > 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,    status )
                                                 values ( tblNo,   tblName,   5000,  'Table likely to be overwritten',  'Warning' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 5000,  'Table likely to be overwritten',  'WARNING'  );

	--       check no. 5001 -- check for unknown hourIDs
	insert into QA_Checks_Log ( tableId, tableName,  checkNo, description,  status    )
					   values ( tblNo,   tblName,    5001,    'hourId    ', 'checking' );

	Drop     table if exists temp_MoZoHr;  -- month zone hour
	Create   table           temp_MoZoHr
	Select   monthId,
			 zoneId,
			 hourId,
			 'no '     as aMatch,
			 count(*)  as n
	From     zoneMonthHour
	Group by monthId,
			 zoneId,
			 hourId;

	Update temp_MoZoHr as a set aMatch='yes' where (Select count(*)
													  From   ##defaultdb##.hourOfAnyDay as m
													  Where  a.hourId = m.hourId)>0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 monthId,
			 zoneId,
			 hourId )
	Select   tblNo,
			 tblName,
			 5001,
			 'hourId',
			 hourId,
			 n           as count,
			'Error'      as status,
			 monthId,
			 zoneId,
			 hourId
	From     temp_MoZoHr
	Where    aMatch <> 'yes';

	--       check no. 5002 -- check for unknown monthIDs
	insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,   status    )
					   values ( tblNo,   tblName,    5002,     'monthId    ', 'checking' );

	Update temp_MoZoHr      set aMatch='no';
	Update temp_MoZoHr as a set aMatch='yes' where (Select count(*)
											  From   ##defaultdb##.monthOfAnyYear as m
											  Where  a.monthId = m.monthId)>0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 monthId,
			 zoneId,
			 hourId )
	Select   tblNo,
			 tblName,
			 5002,
			 'monthId',
			 monthId,
			 n           as count,
			'Error'      as status,
			 monthId,
			 zoneId,
			 hourId
	From     temp_MoZoHr
	Where    aMatch <> 'yes';

	--       check no. 5003 -- checks for unknown zoneIDs
	insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,  status    )
					   values ( tblNo,    tblName,   5003,     'zoneId    ', 'checking' );

	Update temp_MoZoHr      set aMatch='no';
	Update temp_MoZoHr as a set aMatch='yes' where (Select m.zoneId
													 From   ##defaultdb##.zone as m
													 Where  a.zoneId = m.zoneId);

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 monthId,
			 zoneId,
			 hourId )
	Select   tblNo,
			 tblName,
			 5003,
			'zoneId',
			 zoneId,
			 n           as count,
			'Error'      as status,
			 monthId,
			 zoneId,
			 hourId
	From     temp_MoZoHr
	Where    aMatch <> 'yes';

	--       check no. 5004 -- checks for temperature between -80 and 150
	insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,   status    )
					   values ( tblNo,    tblName,   5004,     'temperature', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   monthId,
			 zoneId,
			 hourId,
			 temperature,
			 'no '        as aMatch,
			 count(*)     as n
	From     zoneMonthHour
	Group by monthId,
			 zoneId,
			 hourId;

	Update tempA as a set aMatch='yes' where temperature>=-80.0 and temperature<=150.0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 monthId,
			 zoneId,
			 hourId  )
	Select   tblNo,
			 tblName,
			 5004,
			'temperature',
			 temperature,
			 n                  as count,
			'Error',
			 monthId,
			 zoneId,
			 hourId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 5005 -- checks that relative humidity is between 0 and 100
	insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,   status    )
					   values ( tblNo,    tblName,   5005,     'relHumidity', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   monthId,
			 zoneId,
			 hourId,
			 relHumidity,
			 'no '        as aMatch,
			 count(*)     as n
	From     zoneMonthHour
	Group by monthId,
			 zoneId,
			 hourId;

	Update tempA as a set aMatch='yes' where relHumidity>=0.0 and relHumidity<=100.0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 monthId,
			 zoneId,
			 hourId  )
	Select   tblNo,
			 tblName,
			 5005,
			'relHumidity',
			 relHumidity,
			 n                  as count,
			'Error',
			 monthId,
			 zoneId,
			 hourId               --
	From     tempA
	Where    aMatch <> 'yes';

--       check no. 5006: check for missing monthID and hourID combinations
	insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,   status    )
					   values ( tblNo,    tblName,   5006,     'missing key combinations', 'checking' );
                       
    Insert into CDB_Checks
        (   tableId,
            TableName,
            CheckNumber,
            TestDescription,
            status,
            monthID, zoneID, hourID)
    Select  tblNo as tableId,
			tblName as tableName,
            5006 as CheckNumber,
            'Missing combination of valid monthID and hourID' as testDescription,
            'ERROR' as status,
            monthID, zoneID, hourID
    from (
        SELECT monthID, hourID
        FROM  ##defaultdb##.monthOfAnyYear
        CROSS JOIN ##defaultdb##.hourOfAnyDay
    ) as t1
    left join zonemonthhour using (monthID, hourID)
    join (select count(*) as n from zonemonthhour) as t2
    where (temperature is NULL or relHumidity is NULL) and n > 0
    ORDER BY monthID, zoneID, hourID LIMIT 1;

    --       check no. 5007: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    5007, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            5007 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;
	
	drop table temp_MoZoHr;
  end if; -- end checks for zonemonthhour
  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                               values ( tblNo,   tblName,   5000,         'Table Check',     'Completed' );
  --       Table  Check: zoneMonthHour *end*

-- Table  Check: nrbaseyearequipment *start*
-- check no. 10000 -- check that nrbaseyearequippopulation exists (info if not) or is empty (warning)
  set tblNo   = 100;
  set tblName = 'nrbaseyearequippopulation';
  set errMsg  = '';

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    10000,     'init table check', 'unknown' );

  set    numRows =  ( select table_rows
                      from   information_schema.tables
                      where  table_schema = database()
                        and  table_name   = tblName      );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status )
                                                 values ( tblNo,   tblName,   10000,  'Missing Table',  'Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10000,  'Missing Table',  'INFO');

  elseif numRows = 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status   )
                                                 values ( tblNo,   tblName,   10000,  'Empty Table',    'Warning' );

                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10000,  'Empty Table',  'WARNING'  );
                                                 set nWarnings = nWarnings + 1;
                                                 set errMsg  = 'Empty Table';

  else                        INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status     )
                                                 values ( tblNo,   tblName,   10000,  'Present Table',  'Completed' );
                                                 
	-- check no. 10001 -- check for unknown source types
	insert into QA_Checks_Log ( tableId, tableName, checkNo, description,     status    )
					   values ( tblNo,   tblName,   10001,  'sourceTypeId', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   sourceTypeId,
			'no '        as aMatch,
			 count(*)    as n
	From     nrbaseyearequippopulation
	Group by sourceTypeId;

	Update tempA as a set aMatch='yes' where ( Select count(*)
											   From ##defaultdb##.nrSourceUseType as m
											   Where  a.sourceTypeId = m.sourceTypeId     ) > 0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 sourceTypeId )
	Select   tblNo                      as tableId,
			'nrbaseyearequippopulation' as tableName,
			 10001                      as checkNumber,
			'sourceTypeId is invalid'   as testDescription,
			 sourceTypeId               as testValue,
			 n                          as count,
			'ERROR'                     as status,
			 sourceTypeId
	From     tempA
	Where    aMatch <> 'yes';

	-- check no. 10002 -- check for unknown stateIDs
	insert into QA_Checks_Log ( tableId, tableName, checkNo, description, status    )
					   values ( tblNo,     tblName,   10002,    'stateId',   'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   stateId,
			 'no '        as aMatch,
			 count(*)     as n
	From     nrbaseyearequippopulation
	Group by stateId;

	Update tempA as a set aMatch='yes' where (Select count(*)
											  From   ##defaultdb##.state as m
											  Where  a.stateId = m.stateId)>0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 stateId,
			 count,
			 status  )
	Select   tblNo                      as tableId,
			'nrbaseyearequippopulation' as tableName,
			 10002                      as checkNumber,
			'stateId not valid'         as testDescription,
			 stateId                    as testValue,
			 stateId                    as stateId,
			 n                          as count,
			'ERROR'                     as status
	From     tempA
	Where    aMatch <> 'yes';

    --       check no. 10003: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10003, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10003 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

  end if; -- end if nrbaseyearequippopulation exists and has rows
  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
				   values ( tblNo,   tblName,   10000,        'Table Check',     'Completed' );
--       Table  Check: nrbaseyearequippopulation *end*


--       Table  Check: nrdayallocation *start*
-- check no. 10100 -- check that nrdayallocation exists (info if not) or is empty (warning)
  set tblNo   = 101;
  set tblName = 'nrdayallocation';
  set errMsg  = '';

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    10100,     'init table check', 'unknown' );

  set    numRows =  ( select table_rows
                      from   information_schema.tables
                      where  table_schema = database()
                        and  table_name   = tblName      );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status )
                                                 values ( tblNo,   tblName,   10100,  'Missing Table',  'Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10100,  'Missing Table',  'INFO');

  elseif numRows = 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status   )
                                                 values ( tblNo,   tblName,   10100,  'Empty Table',    'Warning' );

                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10100,  'Empty Table',  'WARNING'  );
                                                 set nWarnings = nWarnings + 1;
                                                 set errMsg  = 'Empty Table';

  else                        INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status     )
                                                 values ( tblNo,   tblName,   10100,  'Present Table',  'Completed' );

    --       check no. 10101 -- check for unknown dayIDs
	insert into QA_Checks_Log ( tableId, tableName, checkNo, description,  status    )
					   values ( tblNo,   tblName,   10101,     'dayId',      'checking' );

	Drop   table if exists tempA;
	create table tempA
	select  'no ' as OK,
			 dayId,
			 count(*) as n
	from     nrdayallocation;

	Update tempA as a set a.OK='yes' where (select count(*)
											from   ##defaultdb##.dayOfAnyWeek as b
											where a.dayId = b.dayId) > 0;

	set nErrors = nErrors + (select count(*) from tempA where OK = 'no');

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 dayId,
			 count,
			 status  )
	Select   tblNo            as tableId,
			'nrdayallocation' as tableName,
			 10101            as checkNumber,
			'invalid dayId'   as testDescription,
			 dayId            as testValue,
			 dayId,
			 n                  as count,
			'ERROR'             as status
	From     tempA
	Where    OK <> 'yes';

		--       check no. 10102 -- checks that the dayFraction sums to 1
	insert into QA_Checks_Log ( tableId, tableName, checkNo, description,   status    )
					   values ( tblNo,   tblName,   10102,     'dayFraction', 'checking' );

	Drop   table if exists tempA;
	create table           tempA
	select   scc,
			 dayId,
			 sum(dayId*dayFraction) as s1
	from     nrdayallocation
	group by scc,
			 dayId;

	drop   table if exists tempB;
	create table           tempB
	select   'no '    as OK,
			 scc,
			 sum(s1) as s2
	from     tempA
	group by scc;

	Update tempB set OK='yes' where s2>0.99999 and s2<1.00001;

	set nErrors = nErrors + (select count(*) from tempB where OK = 'no');

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status  )
	Select   tblNo                  as tableId,
			'nrdayallocation'       as tableName,
			 10102                  as checkNumber,
			'dayFraction sum <> 1.0' as testDescription,
			 s2                     as testValue,
			 1                      as count,
			'ERROR'                 as status
	From     tempB
	Where    OK <> 'yes';

    --       check no. 10103: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10103, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10103 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

  end if; -- end dayallocation checks
  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                               values ( tblNo,   tblName,   10100,          'Table Check',     'Completed' );
--       Table  Check: nrdayallocation *end*


  --       Table  Check: nrfuelSupply *start*
-- check no. 10200 -- check that nrfuelSupply exists (info if not) or is filled (warning) -- empty is ok!
  set tblName = 'nrFuelSupply';
  set tblNo   = 102;
  set errMsg  = '';

  set numRows = ( select table_rows
                  from   information_schema.tables
                  where  table_schema = database()
                    and  table_name = tblName );

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    10200,     'init table check', 'unknown' );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,    status )
                                                 values ( tblNo,   tblName,   10200,  'Missing Table','Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10200,  'Missing Table',  'INFO');
  -- no warning or further checks if table is empty                                                         
  elseif numRows > 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,    status )
                                                 values ( tblNo,   tblName,   10200,  'Table likely to be overwritten',  'Warning' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10200,  'Table likely to be overwritten',  'WARNING'  );


	--       check no. 10201 -- check for unknown fuelregionids
	insert into QA_Checks_Log ( tableId, tableName,   checkNo,  description,    status    )
					   values ( tblNo,   tblName,     10201,      'fuelRegionId', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   fuelRegionId as fuelRegionId,
			 'no '        as aMatch,
			 count(*)     as n,
			 fuelYearId,
			 monthGroupId,
			 fuelFormulationId
	From     nrfuelSupply
	Group by fuelRegionId;

	Update tempA as a set aMatch='yes' where (Select count(*)
											  From   ##defaultdb##.regionCounty as r
											  Where  a.fuelRegionId = r.regionId) > 0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelYearId,
			 monthGroupId,
			 fuelFormulationId  )
	Select   tblNo,
			 tblName,
			 10201,
			'fuelRegionId',
			 fuelRegionId,
			 n,
			'Error',
			 fuelYearId,
			 monthGroupId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 10202 -- check for unknown fuelformulationids
	insert into QA_Checks_Log ( tableId, tableName,  checkNo, description,         status    )
					   values ( tblNo,    tblName,   10202,     'fuelFormulationId', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   fuelFormulationId,
			 'no '             as aMatch,
			 count(*)          as n,
			 fuelYearId,
			 monthGroupId
	From     nrfuelSupply
	Group by fuelFormulationId,
			 fuelYearId,
			 monthGroupId;

	Update tempA as a set aMatch='yes' where (Select m.fuelFormulationID
											  From   fuelformulation as m
											  Where  a.fuelFormulationId = m.fuelFormulationID);
                                              
	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelYearId,
			 monthGroupId )
	Select   tblNo              as tableId,
			 tblName            as tableName,
			 10202                 as checkNumber,
			'fuelFormulationId' as testDescription,
			 fuelFormulationId  as testValue,
			 n                  as count,
			'Error'             as status,
			 fuelYearId,
			 monthGroupId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 10203 -- check for unknown fuelyearid
	insert into QA_Checks_Log ( tableId, tableName,  checkNo, description,  status    )
					   values ( tblNo,    tblName,   10203,     'fuelYearId', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select   fuelYearId,
			 'no '      as aMatch,
			 count(*)   as n,
			 monthGroupId,
			 fuelFormulationId
	From     nrfuelSupply
	Group by fuelYearId,
			 monthGroupId,
			 fuelFormulationId;

	Update tempA as a set aMatch='yes' where (Select y.fuelYearId
											  From   ##defaultdb##.year as y
											  Where  a.fuelYearId = y.fuelYearId);

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 monthGroupId,
			 fuelFormulationId  )
	Select   tblNo,
			 tblName      as tableName,
			 10203,
			"fuelYearId"  as testDescription,
			 fuelYearId   as testValue,
			 n            as count,
			'Error'       as status,
			 monthGroupId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

	--       check no. 10204 -- check that the marketshare sums to 1
	insert into QA_Checks_Log ( tableId, tableName, checkNo, description,   status    )
					   values ( tblNo,    tblName,  10204,     'marketShare', 'checking' );

	-- all fuel types:
	drop   table tempA;
	create table tempA
	Select   tblNo                       as tableId,
			 tblName                     as tableName,
			 10204                          as checkNumber,
			'sum of marketShare <> 1.0'  as testDescription,
			 sum(marketShare)            as testValue,
			'Error'                      as status,
			 count(*)                    as n,
			 fuelRegionId,
			 ' no'                       as aMatch,
			 fuelYearId,
			 monthGroupId,
		   a.fuelFormulationId,
			 fuelTypeId
	From     nrfuelSupply                  as a,
			 fuelFormulation             as b,
			 ##defaultdb##.nrfuelsubtype as c
	Where    a.fuelFormulationId = b.fuelFormulationId
	  and    b.fuelSubTypeId     = c.fuelSubtypeId
	Group by fuelRegionId,
			 fuelYearId,
			 monthGroupId,
			 fuelTypeId;

	update tempA set aMatch='Yes' where testValue>0.99999 and testValue<1.00001;

	Insert into CDB_Checks
		   ( tableId,
			 tableName,
			 checkNumber,
			 testDescription,
			 testValue,
			 status,
			 count,
			 fuelYearId,
			 monthGroupId,
			 fuelTypeId,
			 fuelFormulationId )
	Select   tblNo                       as tableId,
			 tblName                     as tableName,
			 10204                          as checkNumber,
			'sum of marketShare <> 1.0'  as testDescription,
			 testValue,
			'Error'                      as status,
			 n                           as count,
			 fuelYearId,
			 monthGroupId,
			 fuelTypeId,
			 fuelFormulationId
	From     tempA
	where    aMatch <> 'yes';

	--       check no. 10205 -- check for unknown monthGroupIDs
	insert into QA_Checks_Log ( tableId, tableName,  checkNo, description,    status    )
					   values ( tblNo,    tblName,   10205,     'monthGroupId', 'checking' );

	Drop     table if exists tempA;
	Create   table           tempA
	Select  'no '        as aMatch,
			 count(*)     as n,
			 fuelYearId,
			 monthGroupId,
			 fuelFormulationId
	From     nrfuelSupply
	Group by fuelYearId,
			 monthGroupId,
			 fuelFormulationId;

	Update tempA as a set aMatch='yes' where (Select count(*)
											  From   ##defaultdb##.monthGroupOfAnyYear as m
											  Where  a.monthGroupId = m.monthGroupId)>0;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status,
			 fuelYearId,
			 monthGroupId,
			 fuelFormulationId  )
	Select   tblNo         as tableId,
			 tblName       as tableName,
			 10205            as checkNumber,
			'monthGroupId' as testDescription,
			 monthGroupId  as testValue,
			 n             as count,
			'Error'        as status,
			 fuelYearId,
			 monthGroupId,
			 fuelFormulationId
	From     tempA
	Where    aMatch <> 'yes';

    --       check no. 10206: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10206, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10206 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

 end if; -- end fuelsupply checks

 INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                              values ( tblNo,   tblName,   10200,         'Table Check',     'Completed' );
  --       Table  Check: nrFuelSupply *end*


  --       Table  Check: nrGrowthIndex *start*
-- check no. 10300 -- check that nrGrowthIndex exists (info if not) or is empty (warning)
  set tblName = 'nrgrowthIndex';
  set tblNo   = 103;
  set errMsg  = '';

  set numRows = ( select table_rows
                  from   information_schema.tables
                  where  table_schema = database()
                    and  table_name = tblName );

    insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                       values ( tblNo,   tblName,    10300,     'init table check', 'unknown' );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,    status )
                                                 values ( tblNo,   tblName,   10300,  'Missing Table','Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10300,  'Missing Table',  'INFO');
                                                           
  elseif numRows = 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,    status )
                                                 values ( tblNo,   tblName,   10300,  'Empty Table',  'Warning' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10300,  'Empty Table',  'WARNING'  );

  else                        INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status     )
                                                 values ( tblNo,   tblName,   10300,  'Present Table',  'Completed' );

    --       check no. 10301: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10301, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10301 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

  end if; -- end nrgrowthindex
 
  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                               values ( tblNo,   tblName,   10300,         'Table Check',     'Completed' );

  --       Table  Check: nrGrowthIndex *end*


--       Table  Check: nrhourallocation *start*
-- check no. 10400 -- check that nrhourallocation exists (info if not) or is empty (warning)
  set tblNo   = 104;
  set tblName = 'nrHourAllocation';
  set errMsg  = '';

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    10400,     'init table check', 'unknown' );

  set    numRows =  ( select table_rows
                      from   information_schema.tables
                      where  table_schema = database()
                        and  table_name   = tblName      );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status )
                                                 values ( tblNo,   tblName,   10400,  'Missing Table',  'Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10400,  'Missing Table',  'INFO');

  elseif numRows = 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status   )
                                                 values ( tblNo,   tblName,   10400,  'Empty Table',    'Warning' );

                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10400,  'Empty Table',  'WARNING'  );
                                                 set nWarnings = nWarnings + 1;
                                                 set errMsg  = 'Empty Table';

  else                        INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status     )
                                                 values ( tblNo,   tblName,   10400,  'Present Table',  'Completed' );

    --       check no. 10401 -- checks for unknown hourIDs (comparing to 1-24, not the hourDay table)
	 insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,    status   )
						values ( tblNo,   tblName,    10401,      'hourID',       'unknown' );

	drop   table if exists tempA;
	create table           tempA
	select  'no ' as OK,
			 hourId,
			 count(*) as n
	from     nrHourAllocation
	group by hourId;

	update tempA set OK='yes' where hourId>=1 and hourId<=24;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 hourId,
			 count,
			 status  )
	Select   tblNo             as tableId,
			'nrHourAllocation' as tableName,
			 10401             as checkNumber,
			'invalid hourId'   as testDescription,
			 hourId            as testValue,
			 hourId,
			 n                 as count,
			'ERROR'            as status
	From     tempA
	Where    OK <> 'yes';

		--       check no. 10402 -- check that the hourFraction sums to 1
	 insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,    status   )
						values ( tblNo,   tblName,    10402,      'hourFraction', 'unknown' );

	drop   table if exists tempA;
	create table           tempA
	select  'no ' as OK,
			 NRHourAllocPatternId,
			 count(*) as n,
			 sum(hourFraction) as s
	from     nrHourAllocation
	group by NRHourAllocPatternId;

	update tempA set OK='yes' where s>= 0.99999 and s<= 1.00001;

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 count,
			 status  )
	Select   tblNo                         as tableId,
			'nrHourAllocation'             as tableName,
			 10402                         as checkNumber,
			'distribution is out of range' as testDescription,
			 s                             as testValue,
			 n                             as count,
			'ERROR'                        as status
	From     tempA
	Where    OK <> 'yes';


    --       check no. 10403: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10403, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10403 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

  end if;

  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                               values ( tblNo,   tblName,   10400,          'Table Check',     'Completed' );
--       Table  Check: nrHourAllocation *end*


--       Table  Check: nrmonthallocation *start*
-- check no. 10500 -- check that nrmonthallocation exists (info if not) or is empty (warning)
  set tblNo   = 105;
  set tblName = 'nrmonthallocation';
  set errMsg  =  '';

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    10500,     'init table check', 'unknown' );

  set    numRows =  ( select table_rows
                      from   information_schema.tables
                      where  table_schema = database()
                        and  table_name   = tblName      );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status )
                                                 values ( tblNo,   tblName,   10500,  'Missing Table',  'Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10500,  'Missing Table',  'INFO');

  elseif numRows = 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status   )
                                                 values ( tblNo,   tblName,   10500,  'Empty Table',    'Warning' );

                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10500,  'Empty Table',  'WARNING'  );
                                                 set nWarnings = nWarnings + 1;
                                                 set errMsg  = 'Empty Table';

  else                        INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status     )
                                                 values ( tblNo,   tblName,   10500,  'Present Table',  'Completed' );

    --       check no. 10501 -- check for unknown stateIDs
	insert into QA_Checks_Log ( tableId, tableName, checkNo, description,  status    )
					   values ( tblNo,   tblName,   10501,     'stateId',    'checking' );

	Drop   table if exists tempA;
	create table tempA
	select  'no ' as OK,
			 stateId,
			 count(*) as n
	from     nrMonthAllocation
	group by stateId;

	Update tempA as a set a.OK='yes' where (select count(*)
											from  ##defaultdb##.state as b
											where a.stateId = b.stateId) > 0;

	set nErrors = nErrors + (select count(*) from tempA where OK = 'no');

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 stateId,
			 count,
			 status  )
	Select   tblNo              as tableId,
			'nrMonthAllocation' as tableName,
			 10501              as checkNumber,
			'invalid stateId'   as testDescription,
			 stateId            as testValue,
			 stateId,
			 n                  as count,
			'ERROR'             as status
	From     tempA
	Where    OK <> 'yes';

		--       check no. 10502 -- check for unknown monthIDs
	insert into QA_Checks_Log ( tableId, tableName, checkNo, description,  status    )
					   values ( tblNo,   tblName,   10502,     'monthId',    'checking' );

	Drop   table if exists tempA;
	create table tempA
	select  'no ' as OK,
			 monthId,
			 count(*) as n
	from     nrMonthAllocation;

	Update tempA as a set a.OK='yes' where (select count(*)
											from   ##defaultdb##.monthOfAnyYear as b
											where a.monthId = b.monthId) > 0;

	set nErrors = nErrors + (select count(*) from tempA where OK = 'no');

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 monthId,
			 count,
			 status  )
	Select   tblNo              as tableId,
			'nrMonthAllocation' as tableName,
			 10502              as checkNumber,
			'invalid monthId'   as testDescription,
			 monthId            as testValue,
			 monthId,
			 n                  as count,
			'ERROR'             as status
	From     tempA
	Where    OK <> 'yes';

	  --       check no. 10503 -- check that monthFraction sums to 1
	insert into QA_Checks_Log ( tableId, tableName, checkNo, description,     status    )
					   values ( tblNo,   tblName,   10503,     'monthFraction', 'checking' );

	Drop   table if exists tempA;
	create table tempA
	select  'no ' as OK,
			 SCC,
			 stateId,
			 count(*) as n,
			 sum(monthFraction) as s
	from     nrMonthAllocation
	group by SCC,
			 stateId;

	Update tempA set OK='yes' where s>=0.99999 and s<=1.00001;

	set nErrors = nErrors + (select count(*) from tempA where OK = 'no');

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 stateId,
			 count,
			 status  )
	Select   tblNo                         as tableId,
			'nrMonthAllocation'            as tableName,
			 10503                         as checkNumber,
			'distribution is out of range' as testDescription,
			 s                             as testValue,
			 stateId                       as stateId,
			 n                             as count,
			'ERROR'                        as status
	From     tempA
	Where    OK <> 'yes';

    --       check no. 10504: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10504, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10504 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

  end if; -- end checking nrMonthAllocation

  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                               values ( tblNo,   tblName,   10500,          'Table Check',     'Completed' );
--       Table  Check: nrMonthAllocation *end*

--       Table  Check: nrretrofitfactors *start*
-- check no. 10600 -- check that nrhourallocation exists (info if not) or is empty (warning)
  set tblNo   = 106;
  set tblName = 'nrretrofitfactors';
  set errMsg  = '';

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    10600,     'init table check', 'unknown' );

  set    numRows =  ( select table_rows
                      from   information_schema.tables
                      where  table_schema = database()
                        and  table_name   = tblName      );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status )
                                                 values ( tblNo,   tblName,   10600,  'Missing Table',  'Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10600,  'Missing Table',  'INFO');

  elseif numRows = 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status   )
                                                 values ( tblNo,   tblName,   10600,  'Empty Table',    'Warning' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10600,  'Empty Table',  'WARNING'  );
                                                 set nWarnings = nWarnings + 1;
                                                 set errMsg  = 'Empty Table';

  else                        INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status     )
                                                 values ( tblNo,   tblName,   10600,  'Present Table',  'Completed' );

		--       check no. 10601 -- check for unknown pollutantIDs
		INSERT INTO QA_Checks_Log (tableId, tableName, checkNo, description, status) 
                           values (  tblNo,   tblName,   10601, 'pollutantID', 'unknown' );
		Drop table if exists tempA;
		Create table tempA
		select  pollutantId,
			   'no '    as aMatch,
			   count(*) as n
		from   nrretrofitfactors
		group by pollutantId;

		Update tempA as a set aMatch='yes' where (Select count(*)
												  From   ##defaultdb##.pollutant as p
												  Where  p.pollutantId = a.pollutantId > 0);

		Insert into CDB_Checks
			   ( `status`, 
			     tableID, 
                 TableName,
				 CheckNumber,
				 TestDescription,
				 testValue,
				 count  )
		Select   "ERROR" as `status`,
                 tblNo as tableID,
                 "nrretrofitfactors" as tableName,
				 10601              as checkNumber,
				"pollutantId"     as testDescription,
				 pollutantId      as testValue,
				 count(*)         as cou
		from     tempA
		where    aMatch <> 'yes ';
		Delete from CDB_Checks where checkNumber=3701 and count=0;

		--       check no. 10602 -- check that the retrofitStartYear <= retrofitEndYear
		INSERT INTO QA_Checks_Log (tableId, tableName, checkNo, description, status) 
                           values (  tblNo,   tblName,   10602, 'retrofitEndYear', 'unknown' );
		Insert into CDB_Checks
			   ( `status`, 
			     tableID, 
                 TableName,
				 CheckNumber,
				 TestDescription,
				 testValue,
				 count  )
		Select   "ERROR" as `status`,
                 tblNo as tableID,
                 "nrretrofitfactors" as tableName,
				 10602                as checkNumber,
				"retrofitStartYear > retrofitEndYear"  as testDescription,
				 CONCAT(retrofitStartYear, ' > ', retrofitEndYear) as testValue,
				 count(*)         as cou
		from     nrretrofitfactors
		where    retrofitStartYear > retrofitEndYear
		group by retrofitEndYear;


		--       check no. 10603 -- check that EndModelYear <= retrofitEndYear
		INSERT INTO QA_Checks_Log (tableId, tableName, checkNo, description, status) 
                           values (  tblNo,   tblName,   10603, 'EndModelYear', 'unknown' );
		Insert into CDB_Checks
			   ( `status`, 
			     tableID, 
                 TableName,
				 CheckNumber,
				 TestDescription,
				 testValue,
				 count  )
		Select   "ERROR" as `status`,
                 tblNo as tableID,
                 "nrretrofitfactors" as tableName,
				 10603                as checkNumber,
				"EndModelYear > retrofitEndYear"  as testDescription,
				 CONCAT(EndModelYear, ' > ', retrofitEndYear) as testValue,
				 count(*)         as cou
		from     nrretrofitfactors
		where    EndModelYear > retrofitEndYear
		group by EndModelYear, retrofitEndYear;

		--       check no. 10604 -- check that StartModelYear <= EndModelYear
		INSERT INTO QA_Checks_Log (tableId, tableName, checkNo, description, status) 
                           values (  tblNo,   tblName,   10604, 'StartModelYear', 'unknown' );
		Insert into CDB_Checks
			   ( `status`, 
			     tableID, 
                 TableName,
				 CheckNumber,
				 TestDescription,
				 testValue,
				 count  )
		Select   "ERROR" as `status`,
                 tblNo as tableID,
                 "nrretrofitfactors"  as tableName,
				 10604               as checkNumber,
				"StartModelYear > EndModelYear" as testDescription,
				 CONCAT(StartModelYear, ' > ', EndModelYear) as testValue,
				 count(*)          as cou
		from     nrretrofitfactors
		where    StartModelYear > EndModelYear
		group by StartModelYear, EndModelYear;

		--       check no. 10605 -- check that annualFractionRetrofit between 0 and 1
		INSERT INTO QA_Checks_Log (tableId, tableName, checkNo, description, status) 
                           values (  tblNo,   tblName,   10605, 'annualFractionRetrofit', 'unknown' );
		Insert into CDB_Checks
			   ( `status`, 
			     tableID, 
                 TableName,
				 CheckNumber,
				 TestDescription,
				 testValue,
				 count  )
		Select   "ERROR" as `status`,
                 tblNo as tableID,
                 "nrretrofitfactors"  as tableName,
				 10605                 as checkNumber,
				"annualFractionRetrofit range" as testDescription,
				 annualFractionRetrofit as testValue,
				 count(*)          as cou
		from     nrretrofitfactors
		where    annualFractionRetrofit NOT BETWEEN 0 and 1
		group by annualFractionRetrofit;

		--       check no. 10606 -- check that retrofitEffectiveFraction <= 1
		INSERT INTO QA_Checks_Log (tableId, tableName, checkNo, description, status) 
                           values (  tblNo,   tblName,   10606, 'retrofitEffectiveFraction', 'unknown' );
		Insert into CDB_Checks
			   ( `status`, 
			     tableID, 
                 TableName,
				 CheckNumber,
				 TestDescription,
				 testValue,
				 count  )
		Select   "ERROR" as `status`,
                 tblNo as tableID,
                 "nrretrofitfactors"  as tableName,
				 10606                 as checkNumber,
				"retrofitEffectiveFraction range" as testDescription,
				 retrofitEffectiveFraction as testValue,
				 count(*)          as cou
		from     nrretrofitfactors
		where    retrofitEffectiveFraction > 1
		group by retrofitEffectiveFraction;

    --       check no. 10607: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10607, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10607 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

  end if;

  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                               values ( tblNo,   tblName,   10600,          'Table Check',     'Completed' );

--       Table  Check: nrretrofitfactors *end*

--       Table  Check: nrsourceusetype *start*
-- check no. 10700 -- check that nrsourceusetype exists (info if not) or is empty (warning)
  set tblNo   = 107;
  set tblName = 'nrsourceusetype';
  set errMsg  = '';

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    10700,     'init table check', 'unknown' );

  set    numRows =  ( select table_rows
                      from   information_schema.tables
                      where  table_schema = database()
                        and  table_name   = tblName      );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status )
                                                 values ( tblNo,   tblName,   10700,  'Missing Table',  'Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10700,  'Missing Table',  'INFO');

  elseif numRows = 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status   )
                                                 values ( tblNo,   tblName,   10700,  'Empty Table',    'Warning' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10700,  'Empty Table',  'WARNING'  );
                                                 set nWarnings = nWarnings + 1;
                                                 set errMsg  = 'Empty Table';

  else                        INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status     )
                                                 values ( tblNo,   tblName,   10700,  'Present Table',  'Completed' );

    --       check no. 10701: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10701, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10701 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;
  
  end if;
  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                               values ( tblNo,   tblName,   10700,          'Table Check',     'Completed' );

-- --**--
--       Table  Check: nrStateSurrogate *start*
-- check no. 10800 -- check that nrStateSurrogate exists (info if not) or is empty (warning)
  set tblNo   = 108;
  set tblName = 'nrStateSurrogate';
  set errMsg  = '';

  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,        status   )
                     values ( tblNo,   tblName,    10800,     'init table check', 'unknown' );

  set    numRows =  ( select table_rows
                      from   information_schema.tables
                      where  table_schema = database()
                        and  table_name   = tblName      );

  if     numRows is null then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status )
                                                 values ( tblNo,   tblName,   10800,  'Missing Table',  'Info' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status)
                                                           values( tblNo,   tblNAME, 10800,  'Missing Table',  'INFO');

  elseif numRows = 0     then INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status   )
                                                 values ( tblNo,   tblName,   10800,  'Empty Table',    'Warning' );
                               Insert into CDB_Checks( tableId, TableName, checkNumber, testDescription,  status  )
                                                           values( tblNo,   tblNAME, 10800,  'Empty Table',  'WARNING'  );
                                                 set nWarnings = nWarnings + 1;
                                                 set errMsg  = 'Empty Table';

  else                        INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo, description,      status     )
                                                 values ( tblNo,   tblName,   10800,  'Present Table',  'Completed' );

	-- check no. 10801 -- check for unknown stateIDs
	  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,   status    )
						 values ( tblNo,   tblName,    10801,      'stateId',     'checking' );

	Drop   table if exists tempA;
	create table tempA
	select  'no ' as OK,
			 stateId,
			 count(*) as n
	from     nrStateSurrogate
	group by stateId;

	Update tempA as a set a.OK='yes' where (select count(*)
											from   ##defaultdb##.state as b
											where a.stateId = b.stateId) > 0;

	set nErrors = nErrors + (select count(*) from tempA where OK = 'no');

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 stateId,
			 count,
			 status  )
	Select   tblNo             as tableId,
			'nrStateSurrogate' as tableName,
			 10801             as checkNumber,
			'invalid stateId'  as testDescription,
			 stateId           as testValue,
			 stateId,
			 n                  as count,
			'ERROR'             as status
	From     tempA
	Where    OK <> 'yes';


	-- check no. 10802 -- check for unknown countyIDs
	  insert into QA_Checks_Log ( tableId, tableName,  checkNo,  description,   status    )
						 values ( tblNo,   tblName,    10802,      'countyId',    'checking' );

	Drop   table if exists tempA;
	create table tempA
	select  'no ' as OK,
			 countyId,
			 count(*) as n
	from     nrStateSurrogate
	group by countyId;

	Update tempA as a set a.OK='yes' where (select count(*)
											from   ##defaultdb##.county as b
											where a.countyId = b.countyId) > 0;

	Update tempA as a set a.OK='yes' where (select count(*)
											from   ##defaultdb##.state as b
											where a.countyId = b.stateId*1000) > 0;

	set nErrors = nErrors + (select count(*) from tempA where OK <> 'yes');

	Insert into CDB_Checks
		   ( tableId,
			 TableName,
			 CheckNumber,
			 TestDescription,
			 testValue,
			 stateId,
			 count,
			 status  )
	Select   tblNo             as tableId,
			'nrStateSurrogate' as tableName,
			 10802             as checkNumber,
			'invalid countyId' as testDescription,
			 countyId          as testValue,
			 countyId,
			 n                 as count,
			'ERROR'            as status
	From     tempA
	Where    OK <> 'yes';

    --       check no. 10803: check column type definitions for input db mismatches with default db
    INSERT INTO QA_Checks_Log ( tableId, tableName, checkNo,    description,     status)
                       values (   tblNo, tblName,    10803, 'schema check', 'checking');
    INSERT INTO CDB_Checks
            (tableId,
             TableName,
             CheckNumber,
             TestDescription,
             testValue,
             status)
    SELECT  tblNo as tableId,
            tblName as TableName, 
            10803 as CheckNumber, 
            'Incorrect Column Definition' as TestDescription, 
            concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
                    case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
                    ') but is of type ', t1.column_type, '(',
                    case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
                    case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
                    ')'
            ) as testValue,
            'ERROR' as `status`
    from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##inputdb##' and table_name = tblName) t1 
    join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
            where table_schema = '##defaultdb##' and table_name = tblName) t2 using (column_name)
    where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

  end if; -- end checking nrStateSurrogate

  INSERT INTO CDB_Checks  ( tableId, tableName, checkNumber, testDescription,   status     )
                               values ( tblNo,   tblName,   10800,          'Table Check',     'Completed' );
  --       Table  Check: nrStateSurrogate *end*


-- Report Wrap-up:

        set dataBaseName = (select database());
Update  CDB_Checks set countyId     = mid( dataBaseName, 2, 5 );
Update  CDB_Checks set msgDate      = curDate();
Update  CDB_Checks set msgTime      = curTime();
Update  CDB_Checks set version  = @version;

-- select '  ..   End of Nonroad Checks', nErrors, nWarnings;

END
EndBlock

call NEI_QA_NR_M3();

-- Final summary:

set @nErrors   = 0;
set @nWarnings = 0;
set @nErrors = ( select count(*)
                      from   CDB_Checks
                      where  status="ERROR" );
set @nWarnings = ( select count(*)
                      from   CDB_Checks
                      where  status="Warning" );

select '  .. M3nonroadCDBchecks.sql Done', @nErrors, @nWarnings;

-- -------------------------------------------------------------------------------------------
-- Store results from multiple runs.
-- -------------------------------------------------------------------------------------------

create Database if not exists all_cdb_checks;
create Table if not exists all_cdb_checks.All_NRCDB_Checks like CDB_Checks;
Insert into all_cdb_checks.All_NRCDB_Checks Select * from CDB_Checks;

flush tables;

drop procedure if exists NEI_QA_NR_M3;
Drop     table if exists tempA;
Drop     table if exists tempB;

-- drop final tables
drop table CDB_Checks;
drop table QA_Checks_Log;

-- the end --
