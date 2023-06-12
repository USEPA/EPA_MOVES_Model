-- MOVES3 NEI QA Script for Onroad CDBs
-- Last Updated: MOVES3.0.3
-- ##############################################################################
-- Usage Notes:  This script is intended to be run via the ANT command 
--               "onroadNEIQA". See database\NEIQA\NEIQAHelp.pdf for more info.
--               If this script needs to be run directly against MariaDB (i.e.,
--               using mysql.exe or through Workbench/Heidi:
--               1. Find & Replace "BeginBlock" with "Delimiter $$"
--               2. Find & Replace "EndBlock" with "$$\nDelimiter ;" (converting
--                     the \n to a new line)
--               3. Find & Replace "##defaultdb##" with the name of the default 
--                     database, as found in MOVESConfiguration.txt
-- ##############################################################################

set @version = CONCAT('db', MID("##defaultdb##", 8, 8), '-rev20220104');

-- Coordinate this message with the Done statement at the end of this file.
select '  .. M3onroadCDBchecks.sql',curTime(), database();

-- ##############################################################################
-- define stored procedures
-- ##############################################################################


-- ##############################################################################
-- Check the IMCoverage table for overlaps and gaps
-- ##############################################################################
DROP   PROCEDURE IF EXISTS checkImCoverage;
BeginBlock
CREATE PROCEDURE checkImCoverage()
BEGIN
  DECLARE done INT DEFAULT FALSE;
  declare Cpol, Ccou, Cyea, Csou, Cfue, CBEGMY, CENDMY int;  -- C is for current (row of data)
  declare Lpol, Lcou, Lyea, Lsou, Lfue, LBEGMY, LENDMY int;  -- L is for last     "   "  "
  declare rows_processed,
          rows_skipped   int default 0;
  declare CuseIMyn char(1);
  declare reason char(40);
  declare sameSet int;
  DECLARE curIMCov CURSOR FOR SELECT polProcessId,
                                     countyId,
                                     yearId,
                                     sourceTypeId,
                                     fuelTypeId,
                                     begModelYearId,
                                     endModelYearId,
                                     useIMyn
                                FROM imCoverage
                            ORDER BY useIMyn,
                                     polProcessId,
                                     countyId,
                                     yearId,
                                     sourceTypeId,
                                     fuelTypeId,
                                     begModelYearId,
                                     endModelYearId;

  DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
  OPEN curIMCov;
  SET Lpol   = -1;
  SET Lcou   = -1;
  SET Lyea   = -1;
  SET Lsou   = -1;
  SET Lfue   = -1;
  SET LENDMY = -1;

  read_loop: LOOP
    FETCH curIMCov INTO Cpol, Ccou, Cyea, Csou, Cfue, CBEGMY, CENDMY, CuseIMyn;
    IF done THEN LEAVE read_loop; END IF;
    if CuseIMyn = 'Y' then
        set rows_processed = rows_processed + 1;
        
        -- Order Error:  (regardless of sameSet or not)
        if CBEGMY > CENDMY
        then
          Set reason = 'CbegModelYearId>CendModelYearId,';
          INSERT INTO qa_checks_im values ( Cpol, Ccou, Cyea, Csou, Cfue, LENDMY, CBEGMY, CENDMY, CuseIMyn, reason );
		else
          if Cpol=Lpol and Ccou=Lcou and Cyea=Lyea and Csou=Lsou and Cfue=Lfue
		  then  -- in set:

            -- overlap error from consecutive rows
            IF CBEGMY<=LENDMY      then  -- overlap error:
            Set reason = 'CBEGMY<=LENDMY (Overlap)';
            INSERT INTO qa_checks_im values ( Cpol, Ccou, Cyea, Csou, Cfue, LENDMY, CBEGMY, CENDMY, CuseIMyn, reason );

            -- Gap from BEGMY to LENDMY > 1
            elseif CBEGMY>LENDMY+1
            then  -- gap error:
               Set Reason = 'CBEGMY>LENDMY+1 (GAP)';
               INSERT INTO qa_checks_im values ( Cpol, Ccou, Cyea, Csou, Cfue, LENDMY, CBEGMY, CENDMY, CuseIMyn, reason );
			end if;  -- end of overlap & gap checking

		  else  -- not in set:
			SET Lpol   =   Cpol;
            SET Lcou   =   Ccou;
            SET Lyea   =   Cyea;
            SET Lsou   =   Csou;
            SET Lfue   =   Cfue;
		  end if;  -- end of set

		  set LENDMY = CENDMY;
          
		end if;  -- end of order check
        
	 else     -- CuseIMyn check
		set rows_skipped = rows_skipped + 1;
     end if;  -- CuseIMyn check

  END LOOP;
  CLOSE curIMCov;

END
EndBlock
-- ##############################################################################


-- ##############################################################################
-- Check the HotellingActivityDistribution table for overlaps and gaps in MYs
-- ##############################################################################
DROP   PROCEDURE IF EXISTS checkHotellingActivityDistribution;
BeginBlock
CREATE PROCEDURE checkHotellingActivityDistribution()
BEGIN
  DECLARE done INT DEFAULT FALSE;
  declare Czone, CBEGMY, CENDMY int;  -- C is for current (row of data)
  declare Lzone, LBEGMY, LENDMY int;  -- L is for last     "   "  "
  declare rows_processed,
          rows_skipped   int default 0;
  declare reason char(40);
  declare sameSet int;
  DECLARE curHAD CURSOR FOR SELECT distinct zoneID,
                                     beginModelYearId,
                                     endModelYearId
                                FROM hotellingactivitydistribution
                            ORDER BY zoneID,
                                     beginModelYearId,
                                     endModelYearId;

  DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
  OPEN curHAD;
  SET Lzone   = -1;
  SET LENDMY = -1;

  read_loop: LOOP
    FETCH curHAD INTO Czone, CBEGMY, CENDMY;
    IF done THEN LEAVE read_loop; END IF;

	set rows_processed = rows_processed + 1;
	
	-- Order Error:  (regardless of sameSet or not)
	if CBEGMY > CENDMY
	then
	  Set reason = 'CbegModelYearId>CendModelYearId,';
	  INSERT INTO qa_checks_had values ( Czone, LENDMY, CBEGMY, CENDMY, reason );
	else
	  if Czone=Lzone
	  then  -- in set:

		-- overlap error from consecutive rows
		IF CBEGMY<=LENDMY      then  -- overlap error:
		Set reason = 'CBEGMY<=LENDMY (Overlap)';
		INSERT INTO qa_checks_had values ( Czone, LENDMY, CBEGMY, CENDMY, reason );

		-- Gap from BEGMY to LENDMY > 1
		elseif CBEGMY>LENDMY+1
		then  -- gap error:
		   Set Reason = 'CBEGMY>LENDMY+1 (GAP)';
		   INSERT INTO qa_checks_had values ( Czone, LENDMY, CBEGMY, CENDMY, reason );
		end if;  -- end of overlap & gap checking

	  else  -- not in set:
		SET Lzone   =   Czone;
	  end if;  -- end of set

	  set LENDMY = CENDMY;
	  
	end if;  -- end of order check
	
  END LOOP;
  CLOSE curHAD;

END
EndBlock
-- ##############################################################################


-- ##############################################################################
-- Check the idleModelYearGrouping table for overlaps and gaps in MYs
-- ##############################################################################
DROP PROCEDURE IF EXISTS checkIdleModelYearGrouping;
BeginBlock
CREATE PROCEDURE checkIdleModelYearGrouping()
BEGIN
  DECLARE done INT DEFAULT FALSE;
  declare Cst, CBEGMY, CENDMY int;  -- C is for current (row of data)
  declare Lst, LBEGMY, LENDMY int;  -- L is for last     "   "  "
  declare rows_processed,
          rows_skipped   int default 0;
  declare reason char(40);
  declare sameSet int;
  DECLARE curIMYG CURSOR FOR SELECT distinct sourceTypeID,
											 minModelYearID,
											 maxModelYearID
										FROM idlemodelyeargrouping
									ORDER BY sourceTypeID,
											 minModelYearId,
											 maxModelYearId;

  DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
  OPEN curIMYG;
  SET Lst    = -1;
  SET LENDMY = -1;

  read_loop: LOOP
    FETCH curIMYG INTO Cst, CBEGMY, CENDMY;
    IF done THEN LEAVE read_loop; END IF;

	set rows_processed = rows_processed + 1;
	
	-- Order Error:  (regardless of sameSet or not)
	if CBEGMY > CENDMY
	then
	  Set reason = 'CminModelYearId>CmaxModelYearId,';
	  INSERT INTO qa_checks_imyg values ( Cst, LENDMY, CBEGMY, CENDMY, reason );
	else
	  if Cst=Lst
	  then  -- in set:

		-- overlap error from consecutive rows
		IF CBEGMY<=LENDMY      then  -- overlap error:
		Set reason = 'CMinMY<=LMaxMY (Overlap)';
		INSERT INTO qa_checks_imyg values ( Cst, LENDMY, CBEGMY, CENDMY, reason );

		-- Gap from BEGMY to LENDMY > 1
		elseif CBEGMY>LENDMY+1
		then  -- gap error:
		   Set Reason = 'CMinMY>LMaxMY+1 (GAP)';
		   INSERT INTO qa_checks_imyg values ( Cst, LENDMY, CBEGMY, CENDMY, reason );
		end if;  -- end of overlap & gap checking

	  else  -- not in set:
		SET Lst   =   Cst;
	  end if;  -- end of set

	  set LENDMY = CENDMY;
	  
	end if;  -- end of order check
	
  END LOOP;
  CLOSE curIMYG;

END
EndBlock
-- ##############################################################################


-- ##############################################################################
-- Check the totalidlefraction table for overlaps and gaps
-- ##############################################################################
DROP   PROCEDURE IF EXISTS checkTotalIdleFraction;
BeginBlock
CREATE PROCEDURE checkTotalIdleFraction()
BEGIN
  DECLARE done INT DEFAULT FALSE;
  declare Cst, Cmonth, Cday, CBEGMY, CENDMY int;  -- C is for current (row of data)
  declare Lst, Lmonth, Lday, LBEGMY, LENDMY int;  -- L is for last     "   "  "
  declare rows_processed,
          rows_skipped   int default 0;
  declare reason char(40);
  declare sameSet int;
  DECLARE curTIF CURSOR FOR SELECT sourceTypeID,
                                     monthID,
                                     dayID,
                                     minModelYearId,
                                     maxModelYearId
                                FROM totalidlefraction
                            ORDER BY sourceTypeID,
                                     monthID,
                                     dayID,
                                     minModelYearId,
                                     maxModelYearId;

  DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
  OPEN curTIF;
  SET Lst    = -1;
  SET Lmonth = -1;
  SET Lday   = -1;
  SET LENDMY = -1;

  read_loop: LOOP
    FETCH curTIF INTO Cst, Cmonth, Cday, CBEGMY, CENDMY;
    IF done THEN LEAVE read_loop; END IF;

	set rows_processed = rows_processed + 1;
	
	-- Order Error:  (regardless of sameSet or not)
	if CBEGMY > CENDMY
	then
	  Set reason = 'CminModelYearId>CmaxModelYearId,';
	  INSERT INTO qa_checks_tif values ( Cst, Cmonth, Cday, LENDMY, CBEGMY, CENDMY, reason );
	else
	  if Cst=Lst and Cmonth=Lmonth and Cday=Lday
	  then  -- in set:

		-- overlap error from consecutive rows
		IF CBEGMY<=LENDMY      then  -- overlap error:
		Set reason = 'CMaxMY<=LMinMY (Overlap)';
		INSERT INTO qa_checks_tif values ( Cst, Cmonth, Cday, LENDMY, CBEGMY, CENDMY, reason );

		-- Gap from BEGMY to LENDMY > 1
		elseif CBEGMY>LENDMY+1
		then  -- gap error:
		   Set Reason = 'CMinMY>LMaxMY+1 (GAP)';
		   INSERT INTO qa_checks_tif values ( Cst, Cmonth, Cday, LENDMY, CBEGMY, CENDMY, reason );
		end if;  -- end of overlap & gap checking

	  else  -- not in set:
		SET Lst    =   Cst;
		SET Lmonth =   Cmonth;
		SET Lday   =   Cday;
	  end if;  -- end of set

	  set LENDMY = CENDMY;
	  
	end if;  -- end of order check

  END LOOP;
  CLOSE curTIF;

END
EndBlock
-- ##############################################################################


-- ##############################################################################
-- The overlaps and gaps checking store procedures create temporary tables to
-- store the detailed results of their checks; drop these tables if they are
-- empty, but keep them if they have useful data showing where the errors are
--
-- Perform other cleanup
-- ##############################################################################
DROP   PROCEDURE IF EXISTS emptyTableCleanUp;
BeginBlock
CREATE PROCEDURE emptyTableCleanUp()
BEGIN
  -- holds rows of the qa_checks_x tables that exist and are empty
  drop table if exists tempC;
  create table tempC
	select distinct
		   table_Name as TableName,
		   table_rows
	from   information_schema.TABLES
	where  TABLE_SCHEMA = (select database())
	  and  TABLE_ROWS = 0
	  and  Table_Name in ( 'qa_checks_im', 'qa_checks_had', 'qa_checks_imyg', 'qa_checks_tif');

  -- drop qa_checks_x tables if they appear in tempC; otherwise, save them
  if (select count(*) from tempC where TableName = 'qa_checks_im') = 1 then
	drop table qa_checks_im;
  end if;
  if (select count(*) from tempC where TableName = 'qa_checks_had') = 1 then
	drop table qa_checks_had;
  end if;
  if (select count(*) from tempC where TableName = 'qa_checks_imyg') = 1 then
	drop table qa_checks_imyg;
  end if;
  if (select count(*) from tempC where TableName = 'qa_checks_tif') = 1 then
	drop table qa_checks_tif;
  end if;
  
  -- drop tables that get added to the input database if they are missing (so the script doesn't exit early)
  -- but we don't actually want them after the script runs if they are not necessary
  if (select count(*) from CDB_Checks where status = 'todo' and testDescription='table added by QA script and should be removed' and tableName = 'countyyear') = 1 then
	delete from CDB_Checks where status = 'todo' and testDescription='table added by QA script and should be removed' and tableName = 'countyyear';
    drop table if exists countyyear;
  end if;
  if (select count(*) from CDB_Checks where status = 'todo' and testDescription='table added by QA script and should be removed' and tableName = 'emissionratebyage') = 1 then
	delete from CDB_Checks where status = 'todo' and testDescription='table added by QA script and should be removed' and tableName = 'emissionratebyage';
    drop table if exists emissionratebyage;
  end if;
  if (select count(*) from CDB_Checks where status = 'todo' and testDescription='table added by QA script and should be removed' and tableName = 'hotellinghours') = 1 then
	delete from CDB_Checks where status = 'todo' and testDescription='table added by QA script and should be removed' and tableName = 'hotellinghours';
    drop table if exists hotellinghours;
  end if;
  
  -- drop temporary table
  drop table if exists tempC;

END
EndBlock
-- ##############################################################################


-- ##############################################################################
-- Check for missing tables or warn if certain tables are present but unexpected
-- ##############################################################################
-- Create a table to contain the results of the table checks.
drop table if exists CDB_Checks;
CREATE TABLE CDB_Checks (
   countyID          int(11),
   `status`          char(20),
   tableName         char(100),
   checkNumber       smallint(6),
   testDescription   char(250),
   testValue         text,
   `count`           int(11),
   databaseName      char(100),
   dayID             smallint(6),
   fuelFormulationID int(11),
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
   version           char(22),
   sumKeyID          int(11),
   sumKeyDescription char(50)
 ) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- This table contains entries for every QA check, so if this script fails, looking at the last row
-- should help determine where the error occurred.
drop table if exists QA_Checks_Log;
create table QA_Checks_Log (
    checkNo    int(11),
    status     char(20),
    version    char(8),
    msgDate    date,
    msgTime    time
  ) ENGINE=MyISAM DEFAULT CHARSET=latin1;

-- The first set of rows in the the CDB_Checks simply list the tables we are checking along with 
-- how many rows are in each table. This chunk creates the entries for each table; the number of
-- rows are added to these rows below. The reason why this is done in a two-step process is because
-- we don't want the script to crash if a table is missing (so get the information about them from
-- table schema instead of directly)
Insert into CDB_Checks set tableName = 'auditlog';
Insert into CDB_Checks set tableName = 'county';
Insert into CDB_Checks set tableName = 'countyyear';
Insert into CDB_Checks set tableName = 'state';
Insert into CDB_Checks set tableName = 'zone';
Insert into CDB_Checks set tableName = 'zonemonthhour';
Insert into CDB_Checks set tableName = 'zoneroadtype';
Insert into CDB_Checks set tableName = 'year';
Insert into CDB_Checks set tableName = 'avft';
Insert into CDB_Checks set tableName = 'fuelformulation';
Insert into CDB_Checks set tableName = 'fuelsupply';
Insert into CDB_Checks set tableName = 'fuelusagefraction';
Insert into CDB_Checks set tableName = 'hourvmtfraction';
Insert into CDB_Checks set tableName = 'dayvmtfraction';
Insert into CDB_Checks set tableName = 'monthvmtfraction';
Insert into CDB_Checks set tableName = 'hpmsvtypeday';
Insert into CDB_Checks set tableName = 'hpmsvtypeyear';
Insert into CDB_Checks set tableName = 'sourcetypedayvmt';
Insert into CDB_Checks set tableName = 'sourcetypeyearvmt';
Insert into CDB_Checks set tableName = 'hotellingactivitydistribution';
Insert into CDB_Checks set tableName = 'hotellingagefraction';
Insert into CDB_Checks set tableName = 'hotellinghoursperday';
Insert into CDB_Checks set tableName = 'hotellinghourfraction';
Insert into CDB_Checks set tableName = 'hotellingmonthadjust';
Insert into CDB_Checks set tableName = 'starts';
Insert into CDB_Checks set tableName = 'startsageadjustment';
Insert into CDB_Checks set tableName = 'startshourfraction';
Insert into CDB_Checks set tableName = 'startsmonthadjust';
Insert into CDB_Checks set tableName = 'startsperday';
Insert into CDB_Checks set tableName = 'startsperdaypervehicle';
Insert into CDB_Checks set tableName = 'startsopmodedistribution';
Insert into CDB_Checks set tableName = 'idledayadjust';
Insert into CDB_Checks set tableName = 'idlemodelyeargrouping';
Insert into CDB_Checks set tableName = 'idlemonthadjust';
Insert into CDB_Checks set tableName = 'totalidlefraction';
Insert into CDB_Checks set tableName = 'avgspeeddistribution';
Insert into CDB_Checks set tableName = 'imcoverage';
Insert into CDB_Checks set tableName = 'onroadretrofit';
Insert into CDB_Checks set tableName = 'roadtypedistribution';
Insert into CDB_Checks set tableName = 'sourcetypeagedistribution';
Insert into CDB_Checks set tableName = 'sourcetypeyear';
Insert into CDB_Checks set tableName = 'emissionratebyage';
Insert into CDB_Checks set tableName = 'hotellinghours';

Update      CDB_Checks set msgType   = 'Table Check';
Update      CDB_Checks set msgDate   = curDate();
Update      CDB_Checks set msgTime   = curTime();

-- tempB holds the number of rows for each table that we are checking
Drop   table if exists tempB;
Create table           tempB
select distinct
       table_Name as TableName,
       table_rows
from   information_schema.TABLES
where  TABLE_SCHEMA = (select database())
  and  TABLE_ROWS >= 0
  and  Table_Name in ( 'auditlog',
					   'county',
					   'countyyear',
					   'state',
					   'zone',
					   'zonemonthhour',
					   'zoneroadtype',
					   'year',
					   'avft',
					   'fuelformulation',
					   'fuelsupply',
					   'fuelusagefraction',
					   'hourvmtfraction',
					   'dayvmtfraction',
					   'monthvmtfraction',
					   'hpmsvtypeday',
					   'hpmsvtypeyear',
					   'sourcetypedayvmt',
					   'sourcetypeyearvmt',
					   'hotellingactivitydistribution',
					   'hotellingagefraction',
					   'hotellinghoursperday',
					   'hotellinghourfraction',
					   'hotellingmonthadjust',
					   'starts',
					   'startsageadjustment',
					   'startshourfraction',
					   'startsmonthadjust',
					   'startsperday',
					   'startsperdaypervehicle',
					   'startsopmodedistribution',
					   'idledayadjust',
					   'idlemodelyeargrouping',
					   'idlemonthadjust',
					   'totalidlefraction',
					   'avgspeeddistribution',
					   'imcoverage',
					   'onroadretrofit',
					   'roadtypedistribution',
					   'sourcetypeagedistribution',
					   'sourcetypeyear',
					   'emissionratebyage',
					   'hotellinghours'
                     );

-- This chunk updates the first set of entries in CDB_Checks to contain the number of rows in each table
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'auditlog' and  b.TableName = 'auditlog'; 
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'county' and  b.TableName = 'county';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'countyyear' and  b.TableName = 'countyyear';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'state' and  b.TableName = 'state';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'zone' and  b.TableName = 'zone';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'zonemonthhour' and  b.TableName = 'zonemonthhour';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'zoneroadtype' and  b.TableName = 'zoneroadtype';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'year' and  b.TableName = 'year';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'avft' and  b.TableName = 'avft';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'fuelformulation' and  b.TableName = 'fuelformulation';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'fuelsupply' and  b.TableName = 'fuelsupply';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'fuelusagefraction' and  b.TableName = 'fuelusagefraction';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hourvmtfraction' and  b.TableName = 'hourvmtfraction';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'dayvmtfraction' and  b.TableName = 'dayvmtfraction';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'monthvmtfraction' and  b.TableName = 'monthvmtfraction';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hpmsvtypeday' and  b.TableName = 'hpmsvtypeday';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hpmsvtypeyear' and  b.TableName = 'hpmsvtypeyear';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'sourcetypedayvmt' and  b.TableName = 'sourcetypedayvmt';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'sourcetypeyearvmt' and  b.TableName = 'sourcetypeyearvmt';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hotellingactivitydistribution' and  b.TableName = 'hotellingactivitydistribution';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hotellingagefraction' and  b.TableName = 'hotellingagefraction';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hotellinghoursperday' and  b.TableName = 'hotellinghoursperday';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hotellinghourfraction' and  b.TableName = 'hotellinghourfraction';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hotellingmonthadjust' and  b.TableName = 'hotellingmonthadjust';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'starts' and  b.TableName = 'starts';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'startsageadjustment' and  b.TableName = 'startsageadjustment';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'startshourfraction' and  b.TableName = 'startshourfraction';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'startsmonthadjust' and  b.TableName = 'startsmonthadjust';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'startsperday' and  b.TableName = 'startsperday';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'startsperdaypervehicle' and  b.TableName = 'startsperdaypervehicle';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'startsopmodedistribution' and  b.TableName = 'startsopmodedistribution';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'idledayadjust' and  b.TableName = 'idledayadjust';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'idlemodelyeargrouping' and  b.TableName = 'idlemodelyeargrouping';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'idlemonthadjust' and  b.TableName = 'idlemonthadjust';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'totalidlefraction' and  b.TableName = 'totalidlefraction';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'avgspeeddistribution' and  b.TableName = 'avgspeeddistribution';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'imcoverage' and  b.TableName = 'imcoverage';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'onroadretrofit' and  b.TableName = 'onroadretrofit';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'roadtypedistribution' and  b.TableName = 'roadtypedistribution';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'sourcetypeagedistribution' and  b.TableName = 'sourcetypeagedistribution';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'sourcetypeyear' and  b.TableName = 'sourcetypeyear';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'emissionratebyage' and  b.TableName = 'emissionratebyage';
Update CDB_Checks as a, tempb as b set a.count = b.table_rows Where a.tableName = 'hotellinghours' and  b.TableName = 'hotellinghours';

-- Present tables
Update CDB_Checks set testDescription = 'Present'       where `count` is not null;
																	
Update CDB_Checks set testDescription = 'Table likely to be overwritten',
					  `status` = 'Warning'
														where `count` > 0
														and tableName in ('emissionratebyage', 'fuelsupply', 'fuelformulation', 'zonemonthhour');
                                                        
Update CDB_Checks set testDescription = 'Table no longer used as user input',
					  `status` = 'Error'
														where `count` > 0
														and tableName in ('hotellinghours');
                                                        
-- Missing tables
Update CDB_Checks set testDescription = 'Table Missing' where count is null;
Update CDB_Checks set status          = 'Error'         where count is null and tableName not in ('countyyear');
Update CDB_Checks set status          = 'Warning'       where count is null and tableName in ('countyyear');

-- These tables are okay if they are missing
Update CDB_Checks set status = 'todo', testDescription='table added by QA script and should be removed' WHERE count is null and tableName in ('countyyear', 'emissionratebyage', 'hotellinghours');

Delete from CDB_Checks where testDescription = 'Present';


-- ----------------------------------------------------------------------------------------
-- Create missing tables to keep error-checking code from crashing-------------------------
-- Get table schemas from the default database to make sure the schemas are up to date ----
-- ----------------------------------------------------------------------------------------

-- AuditLog is the only input database table that is not in the default database
-- Ideally, we'd like to get this via SOURCE "database/CreateAuditLogTables.sql" but that isn't working at the moment
CREATE TABLE IF NOT EXISTS `auditlog` (
  `whenHappened`     datetime          NOT NULL,
  `importerName`     varchar(100)      NOT NULL,
  `briefDescription` varchar(100)  DEFAULT NULL,
  `fullDescription`  varchar(4096) DEFAULT NULL,
  KEY `logByDate`     (`whenHappened`),
  KEY `logByImporter` (`importerName`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 DELAY_KEY_WRITE=1;

-- geography
CREATE TABLE IF NOT EXISTS `county` LIKE ##defaultdb##.county;
CREATE TABLE IF NOT EXISTS `countyyear` LIKE ##defaultdb##.countyyear;
CREATE TABLE IF NOT EXISTS `state` LIKE ##defaultdb##.state;
CREATE TABLE IF NOT EXISTS `zone` LIKE ##defaultdb##.zone;
CREATE TABLE IF NOT EXISTS `zonemonthhour` LIKE ##defaultdb##.zonemonthhour;
CREATE TABLE IF NOT EXISTS `zoneroadtype` LIKE ##defaultdb##.zoneroadtype;
CREATE TABLE IF NOT EXISTS `year` LIKE ##defaultdb##.year;

-- fuels
CREATE TABLE IF NOT EXISTS `avft` LIKE ##defaultdb##.avft;
CREATE TABLE IF NOT EXISTS `fuelformulation` LIKE ##defaultdb##.fuelformulation;
CREATE TABLE IF NOT EXISTS `fuelsupply` LIKE ##defaultdb##.fuelsupply;
CREATE TABLE IF NOT EXISTS `fuelusagefraction` LIKE ##defaultdb##.fuelusagefraction;

-- vmt
CREATE TABLE IF NOT EXISTS `hourvmtfraction` LIKE ##defaultdb##.hourvmtfraction;
CREATE TABLE IF NOT EXISTS `dayvmtfraction` LIKE ##defaultdb##.dayvmtfraction;
CREATE TABLE IF NOT EXISTS `monthvmtfraction` LIKE ##defaultdb##.monthvmtfraction;
CREATE TABLE IF NOT EXISTS `hpmsvtypeday` LIKE ##defaultdb##.hpmsvtypeday;
CREATE TABLE IF NOT EXISTS `hpmsvtypeyear` LIKE ##defaultdb##.hpmsvtypeyear;
CREATE TABLE IF NOT EXISTS `sourcetypedayvmt` LIKE ##defaultdb##.sourcetypedayvmt;
CREATE TABLE IF NOT EXISTS `sourcetypeyearvmt` LIKE ##defaultdb##.sourcetypeyearvmt;

-- hotelling
CREATE TABLE IF NOT EXISTS `hotellingactivitydistribution` LIKE ##defaultdb##.hotellingactivitydistribution;
CREATE TABLE IF NOT EXISTS `hotellingagefraction` LIKE ##defaultdb##.hotellingagefraction;
CREATE TABLE IF NOT EXISTS `hotellinghoursperday` LIKE ##defaultdb##.hotellinghoursperday;
CREATE TABLE IF NOT EXISTS `hotellinghourfraction` LIKE ##defaultdb##.hotellinghourfraction;
CREATE TABLE IF NOT EXISTS `hotellingmonthadjust` LIKE ##defaultdb##.hotellingmonthadjust;

-- starts
CREATE TABLE IF NOT EXISTS `starts` LIKE ##defaultdb##.`starts`;
CREATE TABLE IF NOT EXISTS `startsageadjustment` LIKE ##defaultdb##.startsageadjustment;
CREATE TABLE IF NOT EXISTS `startshourfraction` LIKE ##defaultdb##.startshourfraction;
CREATE TABLE IF NOT EXISTS `startsmonthadjust` LIKE ##defaultdb##.startsmonthadjust;
CREATE TABLE IF NOT EXISTS `startsperday` LIKE ##defaultdb##.startsperday;
CREATE TABLE IF NOT EXISTS `startsperdaypervehicle` LIKE ##defaultdb##.startsperdaypervehicle;
CREATE TABLE IF NOT EXISTS `startsopmodedistribution` LIKE ##defaultdb##.startsopmodedistribution;

-- idle
CREATE TABLE IF NOT EXISTS `idledayadjust` LIKE ##defaultdb##.idledayadjust;
CREATE TABLE IF NOT EXISTS `idlemodelyeargrouping` LIKE ##defaultdb##.idlemodelyeargrouping;
CREATE TABLE IF NOT EXISTS `idlemonthadjust` LIKE ##defaultdb##.idlemonthadjust;
CREATE TABLE IF NOT EXISTS `totalidlefraction` LIKE ##defaultdb##.totalidlefraction;

-- other input tables
CREATE TABLE IF NOT EXISTS `avgspeeddistribution` LIKE ##defaultdb##.avgspeeddistribution;
CREATE TABLE IF NOT EXISTS `imcoverage` LIKE ##defaultdb##.imcoverage;
CREATE TABLE IF NOT EXISTS `onroadretrofit` LIKE ##defaultdb##.onroadretrofit;
CREATE TABLE IF NOT EXISTS `roadtypedistribution` LIKE ##defaultdb##.roadtypedistribution;
CREATE TABLE IF NOT EXISTS `sourcetypeagedistribution` LIKE ##defaultdb##.sourcetypeagedistribution;
CREATE TABLE IF NOT EXISTS `sourcetypeyear` LIKE ##defaultdb##.sourcetypeyear;
CREATE TABLE IF NOT EXISTS `emissionratebyage` LIKE ##defaultdb##.emissionratebyage;


-- ##############################################################################
-- Start data QA checks
-- ##############################################################################

--       check no. 1001 -- check that one and only one of HPMSVtypeYear, HPMSVtypeDay, SourceTypeYearVMT, SourceTypeDayVMT has at least 1 row
INSERT INTO QA_Checks_Log values ( 1001, 'OK', @hVersion, curDate(), curTime() );   
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   `count`   )
 SELECT
  "VMT Tables" as TableName,
   1001											  as checkNumber,
  "# VMT tables with data <> 1"                       as TestDescription,
  GROUP_CONCAT(DISTINCT tableName SEPARATOR ', ') as testValue,
  sum(tableIsUsed)                                as `count`
from (
	select distinct 'hpmsvtypeday' as tableName, 1 as tableIsUsed from `hpmsvtypeday`
	UNION
	select distinct 'hpmsvtypeyear' as tableName, 1 as tableIsUsed from `hpmsvtypeyear`
	UNION
	select distinct 'sourcetypedayvmt' as tableName, 1 as tableIsUsed from `sourcetypedayvmt`
	UNION
	select distinct 'sourcetypeyearvmt' as tableName, 1 as tableIsUsed from `sourcetypeyearvmt`
) as t1
having `count` <> 1;

--       check no. 1002 -- Record number of rows in HPMSVTypeDay
INSERT INTO QA_Checks_Log values ( 1002, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   msgtype   )
 values
 ("hpmsVTypeDay",
   1002,
  "Number of Rows",
  (Select count(*) from hpmsVTypeDay),
  "Info" );

--       check no. 1003 -- Record number of rows in HPMSVtypeYear
INSERT INTO QA_Checks_Log values ( 1003, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   msgtype   )
 values
 ("hpmsVTypeYear",
   1003,
  "Number of Rows",
  (Select count(*) from hpmsVTypeYear),
  "Info" );

--       check no. 1004 -- Record number of rows in SourceTypeDayVMT
INSERT INTO QA_Checks_Log values ( 1004, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   msgtype   )
 values
 ("sourceTypeDayVmt",
   1004,
  "Number of Rows",
  (Select count(*) from sourceTypeDayVmt), 
  "Info" );

--       check no. 1005 -- Record number of rows in SourceTypeYearVMT
INSERT INTO QA_Checks_Log values ( 1005, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   msgtype   )
 values
 ("sourceTypeYearVmt",
   1005,
  "Number of Rows",
  (Select count(*) from sourceTypeYearVmt),
  "Info" );

--       check no. 1006 -- Check that 0 or 1 total of startsPerDay or startsPerDayPerVehicle or starts are included
INSERT INTO QA_Checks_Log values ( 1006, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   `count`   )
 SELECT
  "starts OR startsPerDay OR startsPerDayPerVehicle" as TableName,
   1006												 as checkNumber,
  "Number of starts tables used"                     as TestDescription,
  GROUP_CONCAT(DISTINCT tableName SEPARATOR ', ')    as testValue,
  sum(tableIsUsed)                                   as `count`
from (
	select distinct 'starts' as tableName, 1 as tableIsUsed from `starts`
	UNION
	select distinct 'StartsPerDay' as tableName, 1 as tableIsUsed from `StartsPerDay`
	UNION
	select distinct 'StartsPerDayPerVehicle' as tableName, 1 as tableIsUsed from `StartsPerDayPerVehicle`
) as t1
having `count` > 1;

--       check no. 1007 -- Record number of rows in starts
INSERT INTO QA_Checks_Log values ( 1007, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   msgtype   )
 values
 ("starts",
   1007,
  "Number of Rows",
  (Select count(*) from `starts`),
  "Info" );

--       check no. 1008 -- Record number of rows in StartsPerDay
INSERT INTO QA_Checks_Log values ( 1008, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   msgtype   )
 values
 ("StartsPerDay",
   1008,
  "Number of Rows",
  (Select count(*) from `StartsPerDay`),
  "Info" );

--       check no. 1009 -- Record number of rows in StartsPerDayPerVehicle
INSERT INTO QA_Checks_Log values ( 1009, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue,
   msgtype   )
 values
 ("StartsPerDayPerVehicle",
   1009,
  "Number of Rows",
  (Select count(*) from `StartsPerDayPerVehicle`),
  "Info" );


-- year
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1100, "year", "Table Check:");

--       check no. 1101 -- check that isBaseYear is either Y, N, y, n
INSERT INTO QA_Checks_Log values ( 1101, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   isBaseYear   as isBaseYear2,
         'no '        as aMatch,
         count(*)     as n
From     year
Group by isBaseYear2;

Update tempA as a set aMatch='yes' where isBaseYear2 in ('Y', 'N', 'y', 'n');

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "year"                  as tableName,
         1101,
         "isBaseYear not Y or N" as testDescription,
         null                    as testValue,
         n                       as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1102 -- check that fuelYearID is the same as yearID in this table
INSERT INTO QA_Checks_Log values ( 1102, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelYearId   as fuelYearId2,
         yearId       as yearId2,
         'no '        as aMatch,
         count(*)     as n
From     year
Group by fuelYearId2,
         yearId2;

Update tempA set aMatch='yes' where fuelYearId2=yearId2;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count     )
Select   "year"                          as tableName,
         1102                             as checkNumber,
        "fuelYearId not equal to yearId" as testDescription,
         fuelYearId2                     as testValue,
         n                               as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1103 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 1103, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId       as yearId2,
         'no '        as aMatch,
         count(*)     as n
From     year
Group by yearId2;
							  
Update tempA as a 
inner join ##defaultdb##.year as m on a.yearId2 = m.yearId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "year"   as tableName,
         1103,
        "yearId"  as testDescription,
         yearId2  as testValue,
         n        as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1104 -- check that yearID matches the name of the database (cXXXXXyYYYY)
INSERT INTO QA_Checks_Log values ( 1104, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId       as yearId2,
         'no '        as aMatch,
         count(*)     as n
From     year
Group by yearId2;

Update tempA as a set aMatch='yes' where mid((select DATABASE() from dual), 7, 5) = CONCAT('y', yearID2);

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "year"   as tableName,
         1104,
        "yearID doesn't match database name"  as testDescription,
         yearId2  as testValue,
         n        as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1105: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1105, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'year' as TableName, 
		1105 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'year') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'year') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

-- state
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1200, "state", "Table Check:");

--       check no. 1201 -- check for unknown stateID
INSERT INTO QA_Checks_Log values ( 1201, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   stateId      as stateId2,
         'no '        as aMatch,
         count(*)     as n
From     state
Group by stateId2;
									  
Update tempA as a 
inner join ##defaultdb##.state as m on a.stateId2 = m.stateId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "state"             as tableName,
         1201,
         "stateId not valid" as testDescription,
         stateId2            as testValue,
         n                   as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1202 -- check for unknown idleRegionID
INSERT INTO QA_Checks_Log values ( 1202, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   idleRegionID as idleRegionID2,
         'no '        as aMatch,
         count(*)     as n
From     state
Group by idleRegionID2;

Update tempA as a 
inner join ##defaultdb##.idleregion as m on a.idleRegionID2 = m.idleRegionID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "state"             as tableName,
         1202,
         "idleRegionID not valid" as testDescription,
         idleRegionID2       as testValue,
         n                   as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1203 -- check that state has at least 1 row
INSERT INTO QA_Checks_Log values ( 1203, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue )
 values
 ("state",
   1203,
  "Number of Rows",
  (Select count(*) from state) );
Delete from CDB_Checks where checkNumber=1203 and testValue>0;

--       check no. 1204: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1204, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'state' as TableName, 
		1204 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'state') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'state') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- Check for the county table
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1300, "county", "Table Check:");

--       check no. 1301: check for unknown countyIDs
INSERT INTO QA_Checks_Log values ( 1301, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   countyId    as countyId2,
         'no '       as aMatch,
         count(*)    as n
From     county
Group by countyId2;

Update tempA as a 
inner join ##defaultdb##.county as m on a.countyId2 = m.countyId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "county"   as tableName,
         1301,
         "countyId" as testDescription,
         countyId2  as testValue,
         n          as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1302 -- check that countyID matches the name of the database (cXXXXXyYYYY)
INSERT INTO QA_Checks_Log values ( 1302, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   countyID     as countyID2,
         'no '        as aMatch,
         count(*)     as n
From     county
Group by countyID2;

Update tempA as a set aMatch='yes' where convert(mid((select DATABASE() from dual), 2, 5), UNSIGNED) = countyID2;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "county"   as tableName,
         1302,
        "countyID doesn't match database name"  as testDescription,
         countyID2  as testValue,
         n        as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1303: check to make sure the altitude field is L or H
INSERT INTO QA_Checks_Log values ( 1303, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   altitude     as altitude2,
         'no '        as aMatch,
         count(*)     as n
From     county
Group by altitude2;

Update tempA as a set aMatch='yes' where altitude2 in ('L','H');

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         -- testValue,
         count  )
Select   "county"              as tableName,
         1303,
         "altitude not L or H" as testDescription,
         -- altitude2          as testValue,
         n                     as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1304: make sure GPAFract is between 0 and 1
INSERT INTO QA_Checks_Log values ( 1304, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   GPAFract     as GPAFract2,
         'no '        as aMatch,
         count(*)     as n
From     county
Group by GPAFract2;

Update tempA set aMatch='yes' where GPAFract2>=0.0 and GPAFract2<=1.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "county"       as tableName,
         1304,
         "GPACFract"    as testDescription,
         GPAFract2      as testValue,
         n              as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1305: make sure the barometricPressure field is between 20 and 33
INSERT INTO QA_Checks_Log values ( 1305, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   barometricPressure as barPre,
         'no '              as aMatch,
         count(*)           as n
From     county
Group by barPre;

Update tempA as a set aMatch='yes' where BarPre>=20.0 and BarPre<=33.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "county"             as tableName,
         1305,
         "barometricPressure" as testDescription,
         BarPre               as testValue,
         n                    as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1306: check for unknown stateIDs
INSERT INTO QA_Checks_Log values ( 1306, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   stateId      as stateId2,
         'no '        as aMatch,
         count(*)     as n
From     county
Group by stateId2;

Update tempA as a 
inner join state as c on a.stateId2 = c.stateId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "county"            as tableName,
         1306,
         "stateId not valid" as testDescription,
         stateId2            as testValue,
         n                   as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1307 -- check that county has at least 1 row
INSERT INTO QA_Checks_Log values ( 1307, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue )
 values
 ("county",
   1307,
  "Number of Rows",
  (Select count(*) from county) );
Delete from CDB_Checks where checkNumber=1307 and testValue>0;

--       check no. 1308 -- check for unknown countyTypeID
INSERT INTO QA_Checks_Log values ( 1308, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   countyTypeID as countyTypeID2,
         'no '        as aMatch,
         count(*)     as n
From     county
Group by countyTypeID2;

Update tempA as a 
inner join ##defaultdb##.countytype as m on a.countyTypeID2 = m.countyTypeID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "county"             as tableName,
         1308,
         "countyTypeID not valid" as testDescription,
         countyTypeID2       as testValue,
         n                   as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1309: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1309, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'county' as TableName, 
		1309 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'county') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'county') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- Zone
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1400, "zone", "Table Check:");

--       check no. 1401 -- check for unknown countyIDs
INSERT INTO QA_Checks_Log values ( 1401, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   countyId        as countyId2,
         'no '           as aMatch,
         count(*)        as n
From     zone
Group by countyId2;

Update tempA as a 
inner join county as c on a.countyId2 = c.countyId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zone"     as tableName,
         1401,
        "countyId"  as testDescription,
         countyId2  as testValue,
         n          as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1402 -- check that the startAllocFactor sums to 1
INSERT INTO QA_Checks_Log values ( 1402, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue    )
Select  "zone"                           as tableName,
         1402                            as checkNumber,
        "sum of startAllocFactor <> 1.0" as testDescription,
         sum(startAllocFactor)           as testValue
From     zone
Having   testValue <0.99999 or testValue >1.00001;

--       check no. 1403 -- checks that the idleAllocFactor sums to 1
INSERT INTO QA_Checks_Log values ( 1403, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue    )
Select  "zone"                          as tableName,
         1403                            as checkNumber,
        "sum of idleAllocFactor <> 1.0" as testDescription,
         sum(idleAllocFactor)           as testValue
From     zone
Having   testValue <0.99999 or testValue >1.00001;

--       check no. 1404 -- checks that the SHPAllocFactor sums to 1
INSERT INTO QA_Checks_Log values ( 1404, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue    )
Select  "zone"                         as tableName,
         1404                           as checkNumber,
        "sum of SHPAllocFactor <> 1.0" as testDescription,
         sum(SHPAllocFactor)           as testValue
From     zone
Having   testValue <0.99999 or testValue >1.00001;

--       check no. 1405 -- check that zoneID is consistent with countyID
INSERT INTO QA_Checks_Log values ( 1405, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zone"   as tableName,
         1405	  as CheckNumber,
        "zoneId does not match countyID*10"  as testDescription,
         zoneId   as testValue,
         count(*) as count
From     zone
Group By zoneID, countyID
Having   zoneID <> countyID * 10;

--       check no. 1406 -- check that zone has at least 1 row
INSERT INTO QA_Checks_Log values ( 1406, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue )
 values
 ("zone",
   1406,
  "Number of Rows",
  (Select count(*) from zone) );
Delete from CDB_Checks where checkNumber=1406 and testValue>0;

--       check no. 1407: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1407, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'zone' as TableName, 
		1407 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'zone') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'zone') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


--       Table  Check: avft
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1500, "avft", "Table Check:");

--       check no. 1501: check for unknown sourceTypeIDs (e.g., 22)
INSERT INTO QA_Checks_Log values ( 1501, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     avft
Group by sourceTypeId2;
                        
Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';										  
										  
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avft"        as tableName,
         1501,
        "sourceTypeId" as testDescription,
         sourceTypeId2 as testValue,
         n             as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1502: Check for unknown modelYearIDs (e.g., 2061)
INSERT INTO QA_Checks_Log values ( 1502, 'OK', @hversion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   modelYearId  as modelYearId2,
         'no '        as aMatch,
         count(*)     as n
From     avft
Group by modelYearId2;
                    
Update tempA as a 
inner join ##defaultdb##.modelYear as m on a.modelYearId2 = m.modelYearId
set aMatch='yes';										  

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avft"       as tableName,
         1502,
        "modelYearId" as testDescription,
         modelYearId2 as testValue,
         n            as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1503: check for unknown fuelTypeIDs (e.g., 10)
INSERT INTO QA_Checks_Log values ( 1503, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelTypeId  as fuelTypeId2,
         'no '       as aMatch,
         count(*)    as n
From     avft
Group by fuelTypeId2;
                
Update tempA as a 
inner join ##defaultdb##.fuelType as m on a.fuelTypeId2 = m.fuelTypeId
set aMatch='yes';										  

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avft"      as tableName,
         1503,
        "fuelTypeId" as testDescription,
         fuelTypeId2 as testValue,
         n           as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1504: check for unknown engTechIDs
INSERT INTO QA_Checks_Log values ( 1504, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   engTechId  as engTechId2,
         'no '      as aMatch,
         count(*)   as n
From     avft
Group by engTechId2;

Update tempA as a 
inner join ##defaultdb##.engineTech as m on a.engTechId2 = m.engTechId
set aMatch='yes';		

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avft"     as tableName,
         1504,
        "engTechId" as testDescription,
         engTechId2 as testValue,
         n          as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1505: check for bad fuelEngFractions in avft
--                       allow fractions up to 1.00001 because if you export defaults, MOVES is doing joins
--                       in the background, and it is plausible that it would end up with floating point noise
--                       above 1 for an individual row.
INSERT INTO QA_Checks_Log values ( 1505, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelEngFraction as fuelEngFraction2,
         'no '           as aMatch,
         count(*)        as n
From     avft
Group by fuelEngFraction2;

Update tempA as a set aMatch='yes' where fuelEngFraction2 between 0.0 and 1.00001;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avft"           as tableName,
         1505,
        "fuelEngFraction" as testDescription,
         fuelEngFraction2 as testValue,
         n                as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1506 -- check that the fuelEngFracion distributions of AVFT table sums to 1
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
         modelYearId,
         sum(fuelEngFraction) as distribution
from     avft
group by sourceTypeId,
         modelYearId;

update tempA set distribution = 1.0 where distribution > 0.99999 and distribution < 1.00001;

INSERT INTO QA_Checks_Log values ( 1506, 'OK', @hVersion, curDate(), curTime() );
  Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue
       )
  Select 'avft'                 as tableName,
          1506                   as checkNumber,
         'Sum <> 1.0'           as testDescription,
         ( select count(*)
           from   tempA
           where  distribution <> 1.0 ) as testValue;
delete from CDB_Checks where checkNumber = 1506 and testValue = 0;
Drop table if exists tempA;

--       check no. 1507: check for missing source type, fuel type, and model year combinations
-- 		 Note: this completeness check is different from others in that not all combinations of 
-- 		 source type, fuel type, and model year are valid (i.e., no diesel motorcycles).
-- 		 So this table checks for completeness vs. samplevehiclepopulation, which contains this definition
--       Also, only check for the existence of modelyearids that will appear in the run (according to the year table)
INSERT INTO QA_Checks_Log values ( 1507, 'OK', @hVersion, curDate(), curTime() );
  Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue
       )
  Select 'avft'                 as tableName,
          1507                  as checkNumber,
         'Missing combination of valid sourceTypeID, fuelTypeID, and modelYearID'           as testDescription,
		 concat('ST: ', sourceTypeID, ', FT: ', fuelTypeID, ', MY: ', modelyearID) as testValue
  from (select distinct sourceTypeID, fuelTypeID, modelYearID
		from ##defaultdb##.samplevehiclepopulation
        join `year`
        where modelYearID between yearID-30 and yearID) as t1
  left join avft using (sourceTypeID, fuelTypeID, modelYearID)
	where fuelEngFraction is NULL 
	ORDER BY sourceTypeID, fuelTypeID, modelYearID LIMIT 1;
	
--       check no. 1508: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1508, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'avft' as TableName, 
		1508 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'avft') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'avft') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


--       Table  Check: avgspeeddistribution
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1600, "avgspeeddistribution", "Table Check:");

--       check no. 1601: checks for unknown avgSpeedBinIDs (e.g., 17)
INSERT INTO QA_Checks_Log values ( 1601, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   avgSpeedBinId   as avgSpeedBinId2,
         'no '           as aMatch,
         count(*)        as n
From     avgSpeedDistribution
Group by avgSpeedBinId2;

Update   tempA as a set aMatch='yes' where (Select m.avgSpeedBinId
                                            From   ##defaultdb##.avgSpeedBin as m
                                            Where  a.avgSpeedBinId2 = m.avgSpeedBinId);

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avgSpeedDistribution"    as tableName,
         1601,
         "avgSpeedBinId Not Valid" as testDescription,
         avgSpeedBinId2            as testValue,
         n                         as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1602: make sure the avgSpeedFraction sums to 1 for each sourceTypeID, roadTypeID, and hourDayID
INSERT INTO QA_Checks_Log values ( 1602, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue,
           sourceTypeId,
           roadTypeId,
           hourDayId )
Select  "avgspeeddistribution"           as tableName,
         1602                              as checkNumber,
        "sum of avgSpeedFraction <> 1.0" as testDescription,
         sum(avgSpeedFraction)           as testValue,
           sourceTypeId,
           roadTypeId,
           hourDayId
From     avgspeeddistribution
Group by sourceTypeId,
         roadTypeId,
         hourDayId
Having   testValue <0.99999 or testValue >1.00001;

--       check no. 1603: checks for unknown hourDayIDs
INSERT INTO QA_Checks_Log values ( 1603, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   hourDayId       as hourDayId2,
         'no '           as aMatch,
         count(*)        as n
From     avgSpeedDistribution
Group by hourDayId2;

Update tempA as a 
inner join ##defaultdb##.hourDay as m on a.hourDayId2 = m.hourDayId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avgSpeedDistribution" as tableName,
         1603,
         "hourDayId"            as testDescription,
         hourDayId2             as testValue,
         n                      as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1604: check for unknown roadTypeIDs
INSERT INTO QA_Checks_Log values ( 1604, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   roadTypeId      as roadTypeId2,
         'no '           as aMatch,
         count(*)        as n
From     avgSpeedDistribution
Group by roadTypeId2;
									  
Update tempA as a 
inner join ##defaultdb##.roadType as m on a.roadTypeId2 = m.roadTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avgSpeedDistribution" as tableName,
         1604,
         "roadTypeId"           as testDescription,
         roadTypeId2            as testValue,
         n                      as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1605: check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 1605, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId    as sourceTypeId2,
         'no '           as aMatch,
         count(*)        as n
From     avgSpeedDistribution
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "avgSpeedDistribution" as tableName,
         1605,
         "sourceTypeId"         as testDescription,
         sourceTypeId2          as testValue,
         n                      as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1606: check for missing sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID combinations
INSERT INTO QA_Checks_Log values ( 1606, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "avgSpeedDistribution" as tableName,
         1606,
         'Missing combination of valid sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID' as testDescription,
		 concat('ST: ', sourceTypeID, ', RT: ', roadTypeID, ', HD: ', hourDayID, ', SB: ', avgSpeedBinID) as testValue
from (
	SELECT sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.roadtype on roadTypeID in (2, 3, 4, 5)
	CROSS JOIN ##defaultdb##.hourday
	CROSS JOIN ##defaultdb##.avgspeedbin
) as t1 left join avgspeeddistribution using (sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID)
where avgSpeedFraction is NULL 
ORDER BY sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID LIMIT 1;

--       check no. 1607: make sure no fractions are 1
INSERT INTO QA_Checks_Log values ( 1607, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "avgSpeedDistribution" as tableName,
         1607,
         'avgSpeedFraction >= 1' as testDescription,
		 concat('ST: ', sourceTypeID, ', RT: ', roadTypeID, ', HD: ', hourDayID,
                ', SB: ', avgSpeedBinID, ', avgSpeedFraction = ', avgSpeedFraction) as testValue
FROM  avgspeeddistribution
where avgSpeedFraction >= 1.0
ORDER BY sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID LIMIT 1;

--       check no. 1608: make sure no profiles are flat
INSERT INTO QA_Checks_Log values ( 1608, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID,
         roadTypeID,
         hourDayID)
Select   "avgSpeedDistribution" as tableName,
         1608 as checkNumber,
         'avgSpeedFraction is a flat profile' as testDescription,
         concat('all are ', avgSpeedFraction) as testValue,
		 sourceTypeID,
         roadTypeID,
         hourDayID
from avgspeeddistribution
where roadTypeID not in (1, 100)
group by sourceTypeID, roadTypeID, hourDayID, avgSpeedFraction
having count(*) = (select count(*) from ##defaultdb##.avgspeedbin)
order by sourceTypeID, roadTypeID, hourDayID, avgSpeedBinID LIMIT 1;

--       check no. 1609: make sure weekend and weekday profiles are different
INSERT INTO QA_Checks_Log values ( 1609, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID,
         roadTypeID,
         hourID)
Select   "avgSpeedDistribution" as tableName,
         1609 as checkNumber,
         'avgSpeedFraction is the same between weekend and weekday' as testDescription,
		 sourceTypeID,
         roadTypeID,
         hourID
from (select sourceTypeID, roadTypeID, hourID, avgSpeedBinID, avgSpeedFraction as weekendFraction
	  from avgspeeddistribution
	  join ##defaultdb##.hourday using (hourDayID)
	  where dayID = 2 and roadTypeID not in (1, 100)) as we
join (select sourceTypeID, roadTypeID, hourID, avgSpeedBinID, avgSpeedFraction as weekdayFraction
	  from avgspeeddistribution
	  join ##defaultdb##.hourday using (hourDayID)
	  where dayID = 5 and roadTypeID not in (1, 100)) as wd using (sourceTypeID, roadTypeID, hourID, avgSpeedBinID)
group by sourceTypeID, roadTypeID, hourID
having sum(abs(weekendFraction - weekdayFraction)) < 0.00001
order by sourceTypeID, roadTypeID, hourID LIMIT 1;

--       check no. 1610: make sure weekend and weekday profiles are different
--  	 compare distributions by sourceTypeID and hourDayID for each road type pair 
--  	 by summing the absolute differences by avgSpeedBinID, and anywhere the sum
--       is 0, the distributions are identical
INSERT INTO QA_Checks_Log values ( 1610, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID,
         hourDayID)
Select   "avgSpeedDistribution" as tableName,
         1610 as checkNumber,
         'avgSpeedFraction is the same between at least two road types' as testDescription,
		 sourceTypeID,
         hourDayID
from (select sourceTypeID, hourDayID, avgSpeedBinID, avgSpeedFraction as rt2Fraction
	  from avgspeeddistribution
	  where roadTypeID = 2) as rt2
join (select sourceTypeID, hourDayID, avgSpeedBinID, avgSpeedFraction as rt3Fraction
	  from avgspeeddistribution
	  where roadTypeID = 3) as rt3 using (sourceTypeID, hourDayID, avgSpeedBinID)
join (select sourceTypeID, hourDayID, avgSpeedBinID, avgSpeedFraction as rt4Fraction
	  from avgspeeddistribution
	  where roadTypeID = 4) as rt4 using (sourceTypeID, hourDayID, avgSpeedBinID)
join (select sourceTypeID, hourDayID, avgSpeedBinID, avgSpeedFraction as rt5Fraction
	  from avgspeeddistribution
	  where roadTypeID = 5) as rt5 using (sourceTypeID, hourDayID, avgSpeedBinID)
group by sourceTypeID, hourDayID
HAVING sum(abs(rt2Fraction - rt3Fraction)) < 0.00001 or
	   sum(abs(rt2Fraction - rt4Fraction)) < 0.00001 or 
	   sum(abs(rt2Fraction - rt5Fraction)) < 0.00001 or 
	   sum(abs(rt3Fraction - rt4Fraction)) < 0.00001 or 
	   sum(abs(rt3Fraction - rt5Fraction)) < 0.00001 or 
	   sum(abs(rt4Fraction - rt5Fraction)) < 0.00001
LIMIT 1;

--       check no. 1611: check for 0% speed distributions in speed bin 1
INSERT INTO QA_Checks_Log values ( 1611, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID,
         roadTypeID,
         hourDayID)
Select   "avgSpeedDistribution" as tableName,
         1611 as checkNumber,
         'avgSpeedFraction is 0 in avgSpeedBinID 1' as testDescription,
		 sourceTypeID,
         roadTypeID,
         hourDayID
from avgspeeddistribution
where avgSpeedFraction = 0 and avgSpeedBinID = 1 and roadTypeID not in (1, 100)
LIMIT 1;

--       check no. 1612: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1612, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'avgSpeedDistribution' as TableName, 
		1612 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'avgspeeddistribution') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'avgspeeddistribution') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- countyYear
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1700, "countyyear", "Table Check:");

--       check no. 1701 -- check for unknown countyIDs
INSERT INTO QA_Checks_Log values ( 1701, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   countyId        as countyId2,
         'no '           as aMatch,
         count(*)        as n
From     countyYear
Group by countyId2;

Update tempA as a 
inner join county as c on a.countyId2 = c.countyId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "countyYear" as tableName,
         1701,
        "countyId" as testDescription,
         countyId2 as testValue,
         n         as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1702 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 1702, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId          as yearId2,
         'no '           as aMatch,
         count(*)        as n
From     countyYear
Group by yearId2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "countyYear" as tableName,
         1702,
        "yearId" as testDescription,
         yearId2 as testValue,
         n       as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1703 -- check that the refuelingVaporProgramAdjust value is between 0 and 1
INSERT INTO QA_Checks_Log values ( 1703, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   refuelingVaporProgramAdjust as refuelingVaporProgramAdjust2,
         'no '                       as aMatch,
         count(*)                    as n
From     countyYear
Group by refuelingVaporProgramAdjust2;

Update tempA as a set aMatch='yes' where refuelingVaporProgramAdjust2 >= 0.0
                                     and refuelingVaporProgramAdjust2 <= 1.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "countyYear"                 as tableName,
         1703,
        "refuelingVaporProgramAdjust" as testDescription,
         refuelingVaporProgramAdjust2 as testValue,
         n       as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1704 -- check that the refuelingSpillProgramAdjust value is between 0 and 1
INSERT INTO QA_Checks_Log values ( 1704, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   refuelingSpillProgramAdjust as refuelingSpillProgramAdjust2,
         'no '                       as aMatch,
         count(*)                    as n
From     countyYear
Group by refuelingSpillProgramAdjust2;

Update tempA as a set aMatch='yes' where refuelingSpillProgramAdjust2 >= 0.0
                                     and refuelingSpillProgramAdjust2 <= 1.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "countyYear"                 as tableName,
         1704,
        "refuelingSpillProgramAdjust" as testDescription,
         refuelingSpillProgramAdjust2 as testValue,
         n       as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1705: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1705, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'countyYear' as TableName, 
		1705 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'countyyear') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'countyyear') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- dayVMTFraction checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1800, "dayVmtFraction", "Table Check:");

--       check no. 1801: check for unknown dayIDs
INSERT INTO QA_Checks_Log values ( 1801, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   dayId        as dayId2,
         'no '        as aMatch,
         count(*)     as n
From     dayVmtFraction
Group by dayId2;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId2 = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "dayVmtFraction" as tableName,
         1801,
         "dayId"          as testDescription,
         dayId2           as testValue,
         n                as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1802: check that dayVMTFraction sums to 1 for each source type, road type, and month for the onroad road types
INSERT INTO QA_Checks_Log values ( 1802, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue,
           sourceTypeId,
           monthId,
           roadTypeId )
Select  "dayVmtFraction"               as tableName,
         1802                          as checkNumber,
        "sum of dayVMTFraction <> 1.0" as testDescription,
         sum(dayVMTFraction)           as testValue,
           sourceTypeId,
           monthId,
           roadTypeId
From     dayVmtFraction
Where    roadTypeId in (2,3,4,5)
Group by sourceTypeId,
         monthId,
         roadTypeId
Having   testValue <0.99999 or testValue >1.00001;

--       check no. 1803: check for unknown monthIDs
INSERT INTO QA_Checks_Log values ( 1803, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthId      as monthId2,
         'no '        as aMatch,
         count(*)     as n
From     dayVmtFraction
Group by monthId2;
										  
Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId2 = m.monthId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "dayVmtFraction" as tableName,
         1803,
         "monthId"        as testDescription,
         monthId2         as testValue,
         n                as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1804: check for unknown roadTypeIDs
INSERT INTO QA_Checks_Log values ( 1804, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   roadTypeId   as roadTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     dayVmtFraction
Group by roadTypeId2;

Update tempA as a 
inner join ##defaultdb##.roadType as m on a.roadTypeId2 = m.roadTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "dayVmtFraction" as tableName,
         1804,
         "roadTypeId"     as testDescription,
         roadTypeId2      as testValue,
         n                as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1805: check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 1805, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     dayVmtFraction
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "dayVmtFraction" as tableName,
         1805,
         "sourceTypeId"   as testDescription,
         sourceTypeId2    as testValue,
         n                as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 1806: make sure no fractions are 1
INSERT INTO QA_Checks_Log values ( 1806, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "dayVMTFraction" as tableName,
         1806,
         'dayVMTFraction >= 1' as testDescription,
		 concat('ST: ', sourceTypeID, ', Month: ', monthID, ', RT: ', roadTypeID,
                ', Day: ', dayID, ', dayVMTFraction = ', dayVMTFraction) as testValue
FROM  dayvmtfraction
where dayVMTFraction >= 1.0
ORDER BY sourceTypeID, monthID, roadTypeID, dayID LIMIT 1;

--       check no. 1807: make sure no profiles are flat
INSERT INTO QA_Checks_Log values ( 1807, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
          sourceTypeID, monthID, roadTypeID)
Select   "dayVMTFraction" as tableName,
         1807 as checkNumber,
         'dayVMTFraction is a flat profile' as testDescription,
         'dayID 2 is 2/7 and dayID 5 is 5/7' as testValue,
         sourceTypeID, monthID, roadTypeID
from dayvmtfraction
where abs(dayVMTFraction - dayID/7) < 0.00001 
  and roadTypeID not in (1, 100)
order by sourceTypeID, monthID, roadTypeID LIMIT 1;

--       check no. 1808: check for missing sourceTypeID, monthID, roadTypeID, dayID combinations
--                       only if dayVMTFraction is used
INSERT INTO QA_Checks_Log values ( 1808, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID, monthID, roadTypeID, dayID)
Select   "dayvmtfraction" as tableName,
         1808,
         'Missing combination of valid sourceTypeID, monthID, roadTypeID, dayID' as testDescription,
		 sourceTypeID, monthID, roadTypeID, dayID
from (
	SELECT sourceTypeID, monthID, roadTypeID, dayID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.monthOfAnyYear
	CROSS JOIN ##defaultdb##.roadtype
	CROSS JOIN ##defaultdb##.dayOfAnyWeek
    where roadTypeID in (2, 3, 4, 5)
) as t1 
left join dayvmtfraction using (sourceTypeID, monthID, roadTypeID, dayID)
join (select count(*) as n from dayvmtfraction) as t2
where dayVMTFraction is NULL and n > 0
ORDER BY sourceTypeID, monthID, roadTypeID, dayID LIMIT 1;

--       check no. 1809: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1809, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'dayVMTFraction' as TableName, 
		1809 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'dayvmtfraction') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'dayvmtfraction') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- emissionRateByAge
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (1900, "emissionRateByAge", "Table Check:");

--       check no. 1901 -- check for unknown polProcessIDs
INSERT INTO QA_Checks_Log values ( 1901, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   polProcessId    as polProcessId2,
         'no '           as aMatch,
         count(*)        as n
From     emissionRateByAge
Group by polProcessId2;
										  
Update tempA as a 
inner join ##defaultdb##.emissionRateByAge as m on a.polProcessId2 = m.polProcessId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "emissionRateByAge" as tableName,
         1901,
        "polProcessId"       as testDescription,
         polProcessId2       as testValue,
         n                   as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1902 -- check for unknown opModeIDs
INSERT INTO QA_Checks_Log values ( 1902, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   opModeId        as opModeId2,
         'no '           as aMatch,
         count(*)        as n
From     emissionRateByAge
Group by opModeId2;

Update tempA as a 
inner join ##defaultdb##.operatingmode as m on a.opModeId2 = m.opModeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "emissionRateByAge" as tableName,
         1902,
        "opModeId"       as testDescription,
         opModeId2       as testValue,
         n               as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1903 -- check for unknown ageGroupIDs
INSERT INTO QA_Checks_Log values ( 1903, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   ageGroupId      as ageGroupId2,
         'no '           as aMatch,
         count(*)        as n
From     emissionRateByAge
Group by ageGroupId2;

Update tempA as a 
inner join ##defaultdb##.ageGroup as m on a.ageGroupId2 = m.ageGroupId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "emissionRateByAge" as tableName,
         1903,
        "ageGroupId"         as testDescription,
         ageGroupId2         as testValue,
         n                   as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 1904: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (1904, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'emissionRateByAge' as TableName, 
		1904 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'emissionratebyage') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'emissionratebyage') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

-- Checks for fuelformulation
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2000, "fuelFormulation", "Table Check:");

--       check no. 2001: check for unknown fuelSubTypeIDs
INSERT INTO QA_Checks_Log values ( 2001, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelSubTypeId as fuelSubTypeId2,
         'no '         as aMatch,
         count(*)      as n
From     fuelFormulation
Group by fuelSubTypeId2;

Update tempA as a 
inner join ##defaultdb##.fuelSubType as m on a.fuelSubTypeId2 = m.fuelSubTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2001,
         "fuelSubTypeId"   as testDescription,
         fuelSubTypeId2    as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2002: checks for RVP between 5 and 20 for gasoline subtypes (not including E85)
INSERT INTO QA_Checks_Log values ( 2002, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   RVP           as RVP2,
         fuelSubTypeId as fuelSubType2,
         'no '         as aMatch,
         count(*)      as n
From     fuelFormulation
Group by RVP2;

Update tempA as a set aMatch='yes' where RVP2>=5.0 and RVP2<=20.0 and fuelSubType2     in (10,11,12,13,14,15);
Update tempA as a set aMatch='yes' where                              fuelSubType2 not in (10,11,12,13,14,15);

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2002,
         "RVP"             as testDescription,
         RVP2              as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2003: check for valid sulfur levels between 0 and 5000
INSERT INTO QA_Checks_Log values ( 2003, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sulfurLevel   as sulfurLevel2,
         'no '         as aMatch,
         count(*)      as n
From     fuelFormulation
Group by sulfurLevel2;

Update tempA as a set aMatch='yes' where sulfurLevel2>=0.0 and sulfurLevel2<=5000.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2003,
         "sulfurLevel"     as testDescription,
         sulfurLevel2      as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2004: check for an ETOHVolume between 0 and 100 for all fuels
INSERT INTO QA_Checks_Log values ( 2004, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   ETOHVolume    as ETOHVolume2,
         'no '         as aMatch,
         count(*)      as n,
         fuelsubtypeid as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by ETOHVolume2;

Update tempA as a set aMatch='yes' where ETOHVolume2>=0.0 and ETOHVolume2<=100.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2004,
         "ETOHVolume"      as testDescription,
         ETOHVolume2       as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2005: checks that MTBEVolume is 0 or NULL
INSERT INTO QA_Checks_Log values ( 2005, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   MTBEVolume    as MTBEVolume2,
         'no '         as aMatch,
         count(*)      as n,
         fuelsubtypeid as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by MTBEVolume2;

Update tempA as a set aMatch='yes' where MTBEVolume2 is NULL OR MTBEVolume2=0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2005,
         "MTBEVolume"      as testDescription,
         MTBEVolume2       as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2006: Check that ETBEVolume is 0 or NULL
INSERT INTO QA_Checks_Log values ( 2006, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelFormulationID,
         ETBEVolume    as ETBEVolume2,
         'no '         as aMatch,
         count(*)      as n,
         fuelsubtypeid as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by ETBEVolume2;

Update tempA as a set aMatch='yes' where ETBEVolume2 is NULL or ETBEVolume2=0;

Insert into CDB_Checks
       ( TableName,
         fuelFormulationID,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         fuelFormulationID,
         2006,
         "ETBEVolume"      as testDescription,
         ETBEVolume2       as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2007: check that TAMEVolume is 0 or NULL
INSERT INTO QA_Checks_Log values ( 2007, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   TAMEVolume    as TAMEVolume2,
         'no '         as aMatch,
         count(*)      as n,
         fuelsubtypeid as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by TAMEVolume2;

Update tempA as a set aMatch='yes' where TAMEVolume2 is NULL or TAMEVolume2=0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2007,
         "TAMEVolume"      as testDescription,
         TAMEVolume2       as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2008: check for aromaticContent between 0 and 55 for gasoline subtypes
INSERT INTO QA_Checks_Log values ( 2008, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   aromaticContent as aromaticContent2,
         'no '           as aMatch,
         count(*)        as n,
         fuelsubtypeid   as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by aromaticContent2;

Update tempA as a set aMatch='yes' where aromaticContent2>=0.0 and aromaticContent2<=55.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2008,
         "aromaticContent" as testDescription,
         aromaticContent2  as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2009: check for olefinContent between 0 and 25 for gasoline subtypes
INSERT INTO QA_Checks_Log values ( 2009, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   olefinContent as olefinContent2,
         'no '           as aMatch,
         count(*)        as n,
         fuelsubtypeid   as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by olefinContent2;

Update tempA as a set aMatch='yes' where olefinContent2>=0.0 and olefinContent2<=25.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2009,
         "olefinContent"   as testDescription,
         olefinContent2    as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2010: check for benzeneContent between 0 and 5 for gasoline subtypes
INSERT INTO QA_Checks_Log values ( 2010, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   benzeneContent  as benzeneContent2,
         'no '           as aMatch,
         count(*)        as n,
         fuelsubtypeid   as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by benzeneContent2;

Update tempA as a set aMatch='yes' where benzeneContent2>=0.0 and benzeneContent2<=5.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2010,
         "benzeneContent"  as testDescription,
         benzeneContent2   as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2011: check for e200 between 0 and 70 for gasoline subtypes
INSERT INTO QA_Checks_Log values ( 2011, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   e200  as e2002,
         'no '           as aMatch,
         count(*)        as n,
         fuelsubtypeid   as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by e2002;

Update tempA as a set aMatch='yes' where e2002>=0.0 and e2002<=70.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2011,
         "e200"            as testDescription,
         e2002             as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2012: check for e300 between 0 and 100 for gasoline subtypes
INSERT INTO QA_Checks_Log values ( 2012, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   e300            as e3002,
         'no '           as aMatch,
         count(*)        as n,
         fuelsubtypeid   as fuelsubtype2
From     fuelFormulation
Where    fuelsubtypeid in (10,11,12,13,14,15)
  and    fuelformulationid >=100
Group by e3002;

Update tempA as a set aMatch='yes' where e3002>=0.0 and e3002<=100.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelFormulation" as tableName,
         2012,
         "e300"            as testDescription,
         e3002             as testValue,
         n                 as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2013 -- check that the T50/T90 columns exist
INSERT INTO QA_Checks_Log values ( 2013, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
 ( TableName,
   checkNumber,
   TestDescription,
   testValue )
 values
 ("fuelFormulation",
   2013,
  "T50 and/or T90 Missing",
  (Select count(*)
   from   information_schema.columns
   where  table_name   = 'fuelformulation'
     and  column_name in ('t50', 't90')
     and  table_schema = database()) );
Delete from CDB_Checks where checkNumber=2013 and testValue=2;

--       check no. 2014: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2014, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'fuelFormulation' as TableName, 
		2014 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'fuelformulation') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'fuelformulation') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;

-- fuelsupply checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2100, "fuelSupply", "Table Check:");

--       check no. 2101: check for unknown fuelFormulationIDs
INSERT INTO QA_Checks_Log values ( 2101, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelFormulationID as fuelFormulationID2,
         'no '             as aMatch,
         count(*)          as n
From     fuelSupply
Group by fuelFormulationID2;

Update tempA as a 
inner join fuelformulation as c on a.fuelFormulationID2 = c.fuelFormulationID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelSupply" as tableName,
         2101,
        "fuelFormulationID" as testDescription,
         fuelFormulationID2  as testValue,
         n                   as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2102: check for unknown fuelYearIDs
INSERT INTO QA_Checks_Log values ( 2102, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelYearId as fuelYearId2,
         'no '      as aMatch,
         count(*)   as n
From     fuelSupply
Group by fuelYearId2;

Update tempA as a 
inner join year as c on a.fuelYearId2 = c.fuelYearId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelSupply" as tableName,
         2102,
        "fuelYearId"  as testDescription,
         fuelYearId2  as testValue,
         n            as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2103 -- check for unknown monthGroupIDs
INSERT INTO QA_Checks_Log values ( 2103, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthGroupId as monthGroupId2,
         'no '        as aMatch,
         count(*)     as n
From     fuelSupply
Group by monthGroupId2;

Update tempA as a 
inner join ##defaultdb##.monthGroupOfAnyYear as m on a.monthGroupId2 = m.monthGroupId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelSupply"   as tableName,
         2103,
        "monthGroupId"  as testDescription,
         monthGroupId2  as testValue,
         n              as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2104 -- check for multiple fuelRegionIDs
INSERT INTO QA_Checks_Log values ( 2104, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         `count`  )
Select   "fuelSupply"   as tableName,
         2104,
        "Multiple fuelRegionIDs"  as testDescription,
         count(distinct fuelRegionID)              as `count`
from fuelsupply
having `count` > 1;

--       check no. 2105: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2105, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'fuelSupply' as TableName, 
		2105 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'fuelsupply') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'fuelsupply') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- fuelusagefraction
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2200, "fuelUsageFraction", "Table Check:");

--       check no. 2201 -- check for unknown fuelYearIDs
INSERT INTO QA_Checks_Log values ( 2201, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA ;
Create table tempA
select  fuelYearId,
       'no '    as aMatch,
       count(*) as n
from   fuelUsageFraction
group by fuelYearId;

Update tempA as a 
inner join year as c on a.fuelYearId = c.fuelYearId
set aMatch='yes';	


Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "fuelUsageFraction" as tableName,
         2201                 as checkNumber,
        "fuelYearId"         as testDescription,
         fuelYearId          as testValue,
         count(*)            as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=2201 and count=0;

--       check no. 2202 -- checks for unknown countyIDs
INSERT INTO QA_Checks_Log values ( 2202, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   countyId,
         'no '           as aMatch,
         count(*)        as n
From     fuelUsageFraction
group by countyId;

Update tempA as a 
inner join county as c on a.countyId = c.countyId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "fuelUsageFraction"  as tableName,
         2202                  as checkNumber,
        'countyId'            as testDescription,
         countyId             as testValue,
         count(*)             as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2202 and count=0;

--       check no. 2203 -- check for unknown modelYearGroupIDs
INSERT INTO QA_Checks_Log values ( 2203, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   modelYearGroupId,
         'no '           as aMatch,
         count(*)        as n
From     fuelUsageFraction
group by modelYearGroupId;

Update tempA as a 
inner join ##defaultdb##.modelYearGroup as m on a.modelYearGroupId = m.modelYearGroupId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "fuelUsageFraction"  as tableName,
         2203                  as checkNumber,
        'modelYearGroupId'    as testDescription,
         modelYearGroupId     as testValue,
         count(*)             as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2203 and count=0;

--       check no. 2204 -- check for unknown SourceBinFuelTypeIDs
INSERT INTO QA_Checks_Log values ( 2204, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceBinFuelTypeId,
         'no '           as aMatch,
         count(*)        as n
From     fuelUsageFraction
group by sourceBinFuelTypeId;

Update tempA as a 
inner join ##defaultdb##.fuelType as m on a.sourceBinFuelTypeId = m.fuelTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "fuelUsageFraction"  as tableName,
         2204                  as checkNumber,
        'sourceBinFuelTypeId' as testDescription,
         sourceBinFuelTypeId  as testValue,
         count(*)             as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2204 and count=0;

--       check no. 2205 -- checks for unknown fuelSupplyFuelTypeIDs
INSERT INTO QA_Checks_Log values ( 2205, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelSupplyFuelTypeId,
         'no '           as aMatch,
         count(*)        as n
From     fuelUsageFraction
group by fuelSupplyFuelTypeId;

Update tempA as a 
inner join ##defaultdb##.fuelType as m on a.fuelSupplyFuelTypeId = m.fuelTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "fuelUsageFraction"   as tableName,
         2205                   as checkNumber,
        'fuelSupplyFuelTypeId' as testDescription,
         fuelSupplyFuelTypeId  as testValue,
         count(*)              as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2205 and count=0;

--       check no. 2206 -- check that fuelSupplyFuelTypeId must match sourceBinFuelTypeId for non-FFV
INSERT INTO QA_Checks_Log values ( 2206, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   countyId,
         fuelYearId,
         sourceBinFuelTypeId,
         fuelSupplyFuelTypeId,
         count(*) as cou
From     fuelUsageFraction
where    sourceBinFuelTypeId<>5
  and    sourceBinFuelTypeId<>fuelSupplyFuelTypeId
group by countyId,
         fuelYearId,
         sourceBinFuelTypeId;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         countyId,
         TestDescription,
         testValue,
         fuelYearId,
         count  )
Select  'fuelUsageFraction'  as tableName,
         2206                 as checkNumber,
         countyId            as countyId,
        'fuelTypes Mismatch for non-FFV' as testDescription,
         sourceBinFuelTypeId as testValue,
         fuelYearId          as fuelYearId,
         cou                 as count
from     tempA;
Delete from CDB_Checks where checkNumber=2206 and count=0;

--       check no. 2207 -- check that sourceBinFuelTypeId must = 1 or 5 when fuelSupplyFuelTypeId =5 
INSERT INTO QA_Checks_Log values (2207, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select *,
       count(*) as cou
from (select countyId,
             fuelYearId,
             sourceBinFuelTypeId,
             fuelSupplyFuelTypeId
      from   fuelUsageFraction
      where  sourceBinFuelTypeId=5
        and  fuelSupplyFuelTypeId<>1) as a
where        a.fuelSupplyFuelTypeId<>5
group by     countyId,
             fuelYearId,
             fuelSupplyFuelTypeId;

Insert into CDB_Checks
           ( TableName,
             CheckNumber,
             countyId,
             fuelYearId,
             TestDescription,
             testValue,
             count  )
Select      'fuelUsageFraction'               as tableName,
             2207                              as checkNumber,
             countyId                         as countyId,
             fuelYearId                       as fuelYearId,
            'FFV assigned a fuel other than FT 1 or 5' as testDescription,
             sourceBinFuelTypeId              as testValue,
             cou                              as count
from         tempA;
Delete from CDB_Checks where checkNumber=2207 and count=0;

--       check no. 2208 -- make sure there is a row for sourceBinFuelTypeID = 5 and each of fuelSupplyFuelTypeID = 1 and 5
Insert into CDB_Checks
           ( TableName,
             CheckNumber,
             countyId,
             fuelYearId,
             TestDescription,
             testValue,
             count  )
Select      'fuelUsageFraction'               as tableName,
             2208                              as checkNumber,
             countyId                         as countyId,
             fuelYearId                       as fuelYearId,
            'FFV are missing values for FT 1 or 5' as testDescription,
             CONCAT('modelYearGroupID: ', modelYearGroupID) as testValue,
             count(distinct fuelSupplyFuelTypeID) as count
from fuelusagefraction
where sourceBinFuelTypeID = 5 and fuelSupplyFuelTypeID in (1, 5)
group by countyID, fuelYearID, modelYearGroupID, sourceBinFuelTypeID
having `count` <> 2;

--       check no. 2209 -- checks that usageFraction sums to 1 for all sourceBinFuelTypeIds
INSERT INTO QA_Checks_Log values ( 2209, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   countyId,
	   fuelYearId,
	   modelYearGroupID,
	   sourceBinFuelTypeId,
	   sum(usageFraction) as s
from     fuelUsageFraction
group by countyId,
	   fuelYearId,
	   modelYearGroupID,
	   sourceBinFuelTypeId;
       
Insert into CDB_Checks
           ( TableName,
             CheckNumber,
             TestDescription,
             testValue,
             countyId,
             fuelYearId)
Select      'fuelUsageFraction'   as tableName,
             2209                 as checkNumber,
            'distribution <> 1.0' as testDescription,
             CONCAT('MYG: ', modelYearGroupID, ', sourceBinFuelTypeID: ', sourceBinFuelTypeID, ' sums to ', s) as testValue,
             countyId             as countyId,
             fuelYearId           as fuelYearId
from         tempA
where    s < 0.99999
   or    s > 1.00001;
   
--       check no. 2210: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2210, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'fuelUsageFraction' as TableName, 
		2210 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'fuelusagefraction') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'fuelusagefraction') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;
 

-- hotellingActivityDistribution
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2300, "hotellingActivityDistribution", "Table Check:");

--       check no. 2301 -- check for unknown opModeID
INSERT INTO QA_Checks_Log values ( 2301, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   opModeId,
         'no '           as aMatch,
         count(*)        as n
From     hotellingActivityDistribution
group by opModeId;

Update tempA as a 
inner join ##defaultdb##.operatingMode as m on a.opModeId = m.opModeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "hotellingActivityDistribution" as tableName,
         2301                             as checkNumber,
        'opModeId'                       as testDescription,
         opModeId                        as testValue,
         count(*)                        as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2301 and count=0;

--       check no. 2302 -- check that the opModeFraction sums to 1 by model year range
INSERT INTO QA_Checks_Log values ( 2302, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   beginmodelyearId,endmodelyearid,
         sum(opModeFraction) as s,
         count(*)            as cou
From     hotellingActivityDistribution
group by beginmodelyearId,endmodelyearid;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count    )
Select   "hotellingActivityDistribution" as tableName,
         2302                             as checkNumber,
        'distribution<>1.0'              as testDescription,
         CONCAT(beginmodelyearId, '-', endmodelyearid, ' sums to ', s)               as testValue,
         cou                             as count
from     tempA
where    s<0.99999
   or    s>1.00001;

--       check no. 2303 -- check for unknown zoneID
INSERT INTO QA_Checks_Log values ( 2303, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   zoneID,
         'no '           as aMatch,
         count(*)        as n
From     hotellingActivityDistribution
group by zoneID;

Update tempA as a 
inner join zone as c on a.zoneID = c.zoneID
set aMatch='yes';								

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "hotellingActivityDistribution" as tableName,
         2303                             as checkNumber,
        'zoneID'                       as testDescription,
         zoneID                        as testValue,
         count(*)                        as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2303 and count=0;

--       check no. 2304 -- Check for gaps and overlaps in the model years columns in the hotellingActivityDistribution  table.
INSERT INTO QA_Checks_Log values ( 2304, 'OK', @hVersion, curDate(), curTime() );

-- Add a table to contain the results of the gaps/overlaps check.
Drop   table if exists     qa_checks_had;
Create Table if Not Exists qa_checks_had (
  Czone   int,          -- zoneID
  LENDMY int,          -- last    row's end model year
  CBEGMY int,          -- Current row's beg model year
  CENDMY int,          -- Current row's end model year
  Reason varchar(40) );

-- Call the procedure to check for gaps and overlaps.
call checkHotellingActivityDistribution();

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         count  )
Select  "hotellingActivityDistribution" as tableName,
         2304,
        "gaps and overlaps" as testDescription,
         (Select count(*) from qa_checks_had) as count
From     qa_checks_had
Where    (Select count(*) from qa_checks_had) > 0;

--       check no. 2306: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2306, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'hotellingActivityDistribution' as TableName, 
		2306 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'hotellingactivitydistribution') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'hotellingactivitydistribution') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- hotellingagefraction
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2400, "hotellingagefraction", "Table Check:");

--       check no. 2401 -- check for unknown zoneID
INSERT INTO QA_Checks_Log values ( 2401, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   zoneID,
         'no '           as aMatch,
         count(*)        as n
From     hotellingagefraction
group by zoneID;

Update tempA as a 
inner join zone as c on a.zoneID = c.zoneID
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "hotellingagefraction" as tableName,
         2401                   as checkNumber,
        'zoneID'                as testDescription,
         zoneID                 as testValue,
         count(*)               as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2401 and count=0;

--       check no. 2402 -- check for unknown ageID
INSERT INTO QA_Checks_Log values ( 2402, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   ageId         as ageId2,
         'no '         as aMatch,
         count(*)      as n
From     hotellingagefraction
Group by ageId2;

Update tempA as a 
inner join ##defaultdb##.ageCategory as m on a.ageId2 = m.ageId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "hotellingagefraction" as tableName,
         2402,
        "ageId"                      as testDescription,
         ageId2                      as testValue,
         n                           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2403 -- check for missing zoneID and ageID combinations (as long as this table has contents)
INSERT INTO QA_Checks_Log values ( 2403, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "hotellingagefraction" as tableName,
         2403,
         'Missing combination of valid zoneID and ageID' as testDescription,
		 concat('Z: ', zoneID, ', age: ', ageID) as testValue
from (
	SELECT zoneID, ageID
	FROM  zone
	CROSS JOIN ##defaultdb##.ageCategory
) as t1 
left join hotellingagefraction using (zoneID, ageID)
join (select count(*) as c from hotellingagefraction) as t2
where ageFraction is NULL and c > 0
ORDER BY zoneID, ageID LIMIT 1;

--       check no. 2404 -- check for ageFractions >= 1
INSERT INTO QA_Checks_Log values ( 2404, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "hotellingagefraction" as tableName,
         2404,
         'ageFraction >= 1' as testDescription,
		 ageFraction as testValue
from hotellingagefraction
where ageFraction >= 1
ORDER BY zoneID, ageID LIMIT 1;

--       check no. 2405 -- check that the distribution sums to 1
INSERT INTO QA_Checks_Log values ( 2405, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         zoneID)
Select   "hotellingagefraction" as tableName,
         2405,
         'distribution <> 1' as testDescription,
		 sum(ageFraction) as testValue,
         zoneID
from hotellingagefraction
GROUP BY zoneID
having testValue < .99999 or testvalue > 1.00001;

--       check no. 2406: make sure age distributions aren't flat
INSERT INTO QA_Checks_Log values ( 2406, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         zoneID)
Select   "hotellingagefraction" as tableName,
         2406 as checkNumber,
         'ageFraction is a flat profile' as testDescription,
         concat('all are ', ageFraction) as testValue,
		 zoneID
from hotellingagefraction
group by zoneID, ageFraction
having count(*) = (select count(*) from ##defaultdb##.agecategory)
order by zoneID LIMIT 1;

--       check no. 2407: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2407, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'hotellingAgeFraction' as TableName, 
		2407 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'hotellingagefraction') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'hotellingagefraction') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- hotellinghourfraction
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2500, "hotellinghourfraction", "Table Check:");

--       check no. 2501 -- check for unknown zoneID
INSERT INTO QA_Checks_Log values ( 2501, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   zoneID,
         'no '           as aMatch,
         count(*)        as n
From     hotellinghourfraction
group by zoneID;

Update tempA as a 
inner join zone as c on a.zoneID = c.zoneID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "hotellinghourfraction" as tableName,
         2501                   as checkNumber,
        'zoneID'                as testDescription,
         zoneID                 as testValue,
         count(*)               as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2501 and count=0;

--       check no. 2502 -- check for unknown dayID
INSERT INTO QA_Checks_Log values ( 2502, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   dayID         as dayId2,
         'no '         as aMatch,
         count(*)      as n
From     hotellinghourfraction
Group by dayId2;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId2 = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "hotellinghourfraction" as tableName,
         2502,
        "dayId"                      as testDescription,
         dayId2                      as testValue,
         n                           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2503 -- check for unknown hourID
INSERT INTO QA_Checks_Log values ( 2503, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   hourID         as hourId2,
         'no '         as aMatch,
         count(*)      as n
From     hotellinghourfraction
Group by hourID2;

Update tempA as a 
inner join ##defaultdb##.hourOfAnyDay as m on a.hourId2 = m.hourId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "hotellinghourfraction" as tableName,
         2503,
        "hourId"                      as testDescription,
         hourId2                      as testValue,
         n                           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2504 -- check for missing zoneID, dayID, and hourID combinations (as long as this table has contents)
INSERT INTO QA_Checks_Log values ( 2504, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "hotellinghourfraction" as tableName,
         2504,
         'Missing combination of valid zoneID, dayID, hourID' as testDescription,
		 concat('Z: ', zoneID, ', dayID: ', dayID, ', hourID: ', hourID) as testValue
from (
	SELECT zoneID, dayID, hourID
	FROM  zone
	CROSS JOIN ##defaultdb##.dayOfAnyWeek
	CROSS JOIN ##defaultdb##.hourOfAnyDay
) as t1 
left join hotellinghourfraction using (zoneID, dayID, hourID)
join (select count(*) as c from hotellinghourfraction) as t2
where hourFraction is NULL and c > 0
ORDER BY zoneID, dayID, hourID LIMIT 1;

--       check no. 2505 -- check for hourFractions >= 1
INSERT INTO QA_Checks_Log values ( 2505, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         zoneID, dayID, hourID)
Select   "hotellinghourfraction" as tableName,
         2505,
         'hourFraction >= 1' as testDescription,
		 hourFraction as testValue,
         zoneID, dayID, hourID
from hotellinghourfraction
where hourFraction >= 1
ORDER BY zoneID, dayID, hourID LIMIT 1;

--       check no. 2506 -- check that the distribution sums to 1
INSERT INTO QA_Checks_Log values ( 2506, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         zoneID, dayID)
Select   "hotellinghourfraction" as tableName,
         2506,
         'distribution <> 1' as testDescription,
		 sum(hourFraction) as testValue,
         zoneID, dayID
from hotellinghourfraction
GROUP BY zoneID, dayID
having testValue < .99999 or testvalue > 1.00001;

--       check no. 2507: make sure hour distributions aren't flat
INSERT INTO QA_Checks_Log values ( 2507, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         zoneID,
         dayID)
Select   "hotellinghourfraction" as tableName,
         2507 as checkNumber,
         'hourFraction is a flat profile' as testDescription,
         concat('all are ', hourFraction) as testValue,
		 zoneID, dayID
from hotellinghourfraction
group by zoneID, dayID, hourFraction
having count(*) = (select count(*) from ##defaultdb##.hourOfAnyDay)
order by zoneID, dayID LIMIT 1;

--       check no. 2508: make sure weekend and weekday profiles are different
INSERT INTO QA_Checks_Log values ( 2508, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         zoneID)
Select   "hotellinghourfraction" as tableName,
         2508 as checkNumber,
         'hourFraction is the same between weekend and weekday' as testDescription,
		 zoneID
from (select zoneID, hourID, hourFraction as weekendFraction
	  from hotellinghourfraction
	  where dayID = 2) as we
join (select zoneID, hourID, hourFraction as weekdayFraction
	  from hotellinghourfraction
	  where dayID = 5) as wd using (zoneID, hourID)
group by zoneID
having sum(abs(weekendFraction - weekdayFraction)) < 0.00001
order by zoneID LIMIT 1;

--       check no. 2509: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2509, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'hotellingHourFraction' as TableName, 
		2509 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'hotellinghourfraction') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'hotellinghourfraction') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- hotellinghoursperday
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2600, "hotellinghoursperday", "Table Check:");

--       check no. 2601 -- check for unknown yearID
INSERT INTO QA_Checks_Log values ( 2601, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearID         as yearId2,
         'no '         as aMatch,
         count(*)      as n
From     hotellinghoursperday
Group by yearID2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "hotellinghoursperday" as tableName,
         2601,
        "yearID"                      as testDescription,
         yearID2                      as testValue,
         n                           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2602 -- check for unknown zoneID
INSERT INTO QA_Checks_Log values ( 2602, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   zoneID,
         'no '           as aMatch,
         count(*)        as n
From     hotellinghoursperday
group by zoneID;

Update tempA as a 
inner join zone as c on a.zoneID = c.zoneID
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "hotellinghoursperday" as tableName,
         2602                   as checkNumber,
        'zoneID'                as testDescription,
         zoneID                 as testValue,
         count(*)               as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2602 and count=0;

--       check no. 2603 -- check for unknown dayID
INSERT INTO QA_Checks_Log values ( 2603, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   dayID         as dayId2,
         'no '         as aMatch,
         count(*)      as n
From     hotellinghoursperday
Group by dayId2;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId2 = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "hotellinghoursperday" as tableName,
         2603,
        "dayId"                      as testDescription,
         dayId2                      as testValue,
         n                           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2604 -- check for missing yearID, zoneID, and dayID combinations (as long as this table has contents)
INSERT INTO QA_Checks_Log values ( 2604, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "hotellinghoursperday" as tableName,
         2604,
         'Missing combination of valid yearID, zoneID, dayID' as testDescription,
		 concat('Year: ', yearID, ', Z: ', zoneID, ', dayID: ', dayID) as testValue
from (
	SELECT yearID, zoneID, dayID
	FROM  year
	CROSS JOIN zone
	CROSS JOIN ##defaultdb##.dayOfAnyWeek
) as t1 
left join hotellinghoursperday using (yearID, zoneID, dayID)
join (select count(*) as c from hotellinghoursperday) as t2
where hotellingHoursPerDay is NULL and c > 0
ORDER BY yearID, zoneID, dayID LIMIT 1;

--       check no. 2605: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2605, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'hotellingHoursPerDay' as TableName, 
		2605 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'hotellinghoursperday') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'hotellinghoursperday') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- hotellingmonthadjust
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2700, "hotellingmonthadjust", "Table Check:");

--       check no. 2701 -- check for unknown zoneID
INSERT INTO QA_Checks_Log values ( 2701, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   zoneID,
         'no '           as aMatch,
         count(*)        as n
From     hotellingmonthadjust
group by zoneID;

Update tempA as a 
inner join zone as c on a.zoneID = c.zoneID
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "hotellingmonthadjust" as tableName,
         2701                   as checkNumber,
        'zoneID'                as testDescription,
         zoneID                 as testValue,
         count(*)               as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2701 and count=0;

--       check no. 2702 -- check for unknown monthID
INSERT INTO QA_Checks_Log values ( 2702, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthID,
         'no '           as aMatch,
         count(*)        as n
From     hotellingmonthadjust
group by monthID;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId = m.monthId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count )
Select   "hotellingmonthadjust" as tableName,
         2702                   as checkNumber,
        'monthID'                as testDescription,
         monthID                 as testValue,
         count(*)               as count
from     tempA
where    aMatch <> 'yes';
Delete from CDB_Checks where checkNumber=2702 and count=0;

--       check no. 2703 -- check for missing zoneID and monthID combinations (as long as this table has contents)
INSERT INTO QA_Checks_Log values ( 2703, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "hotellingmonthadjust" as tableName,
         2703,
         'Missing combination of valid zoneID and monthID' as testDescription,
		 concat('Z: ', zoneID, ', monthID: ', monthID) as testValue
from (
	SELECT zoneID, monthID
	FROM  zone
	CROSS JOIN ##defaultdb##.monthOfAnyYear
) as t1 
left join hotellingmonthadjust using (zoneID, monthID)
join (select count(*) as c from hotellingmonthadjust) as t2
where monthAdjustment is NULL and c > 0
ORDER BY zoneID, monthID LIMIT 1;

--       check no. 2704: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2704, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'hotellingMonthAdjust' as TableName, 
		2704 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'hotellingmonthadjust') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'hotellingmonthadjust') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- hourVMTFraction checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2800, "hourVmtFraction", "Table Check:");

--       check no. 2801 -- checks unknown dayIDs
INSERT INTO QA_Checks_Log values ( 2801, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   dayId    as dayId2,
         'no '    as aMatch,
         count(*) as n
From     hourVmtFraction
Group by dayId2;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId2 = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "hourVmtFraction" as tableName,
         2801,
         "dayId"           as testDescription,
         dayId2            as testValue,
         n                 as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 2802 -- check for unknown hourIDs
INSERT INTO QA_Checks_Log values ( 2802, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   hourId    as hourId2,
         'no '     as aMatch,
         count(*)  as n
From     hourVmtFraction
Group by hourId2;

Update tempA as a 
inner join ##defaultdb##.hourOfAnyDay as m on a.hourId2 = m.hourId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "hourVmtFraction" as tableName,
         2802,
        "hourId"          as testDescription,
         hourId2          as testValue,
         n                as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 2803 -- check for unknown roadTypeIDs
INSERT INTO QA_Checks_Log values (2803, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   roadTypeId as roadTypeId2,
         'no '      as aMatch,
         count(*)   as n
From     hourVmtFraction
Group by roadTypeId2;

Update tempA as a 
inner join ##defaultdb##.roadType as m on a.roadTypeId2 = m.roadTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "hourVmtFraction" as tableName,
         2803,
        "roadTypeId"      as testDescription,
         roadTypeId2      as testValue,
         n                as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 2804 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 2804, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     hourVmtFraction
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "hourVmtFraction" as tableName,
         2804,
        "sourceTypeId"    as testDescription,
         sourceTypeId2    as testValue,
         n                as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 2805 -- check that hourVMTFraction sums to 1 over sourceTypeID, roadTypeID, and dayID
INSERT INTO QA_Checks_Log values ( 2805, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;

Insert into CDB_Checks
       (  tableName,
          checkNumber,
          testDescription,
          testValue,
            sourceTypeId,
            roadTypeId,
            dayId  )
Select   "hourVmtFraction"               as tableName,
          2805                             as checkNumber,
         "sum of hourVmtFraction <> 1.0" as tesDescription,
          sum(hourVmtFraction)           as testValue,
            sourceTypeId,
            roadTypeId,
            dayId
From      hourVmtFraction
Where     roadTypeId in (2,3,4,5)
Group by  sourceTypeId,
          roadTypeId,
          dayId
Having    testValue <0.99999 or testValue >1.00001;

--       check no. 2806: check for missing sourceTypeID, roadTypeID, dayID, hourID combinations
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID, roadTypeID, dayID, hourID)
Select   "hourVMTFraction" as tableName,
         2806,
         'Missing combination of valid sourceTypeID, roadTypeID, dayID, hourID' as testDescription,
         sourceTypeID, roadTypeID, dayID, hourID
from (
	SELECT sourceTypeID, roadTypeID, dayID, hourID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.roadtype
	CROSS JOIN ##defaultdb##.dayofanyweek
	CROSS JOIN ##defaultdb##.hourofanyday
	WHERE roadTypeID BETWEEN 2 and 5
) as t1 left join hourvmtfraction using (sourceTypeID, roadTypeID, dayID, hourID)
where hourVMTFraction is NULL 
ORDER BY sourceTypeID, roadTypeID, dayID, hourID LIMIT 1;

--       check no. 2807 -- check for hourVMTFractions >= 1
INSERT INTO QA_Checks_Log values ( 2807, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID, roadTypeID, dayID, hourID)
Select   "hourVMTFraction" as tableName,
         2807,
         'hourVMTFraction >= 1' as testDescription,
		 hourVMTFraction as testValue,
         sourceTypeID, roadTypeID, dayID, hourID
from hourvmtfraction
where hourVMTFraction >= 1
ORDER BY sourceTypeID, roadTypeID, dayID, hourID LIMIT 1;

--       check no. 2808: make sure hour VMT distributions aren't flat
INSERT INTO QA_Checks_Log values ( 2808, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID, roadTypeID, dayID)
Select   "hourVMTFraction" as tableName,
         2808 as checkNumber,
         'hourVMTFraction is a flat profile' as testDescription,
         concat('all are ', hourVMTFraction) as testValue,
		 sourceTypeID, roadTypeID, dayID
from hourvmtfraction
where roadTypeID not in (1, 100)
group by sourceTypeID, roadTypeID, dayID, hourVMTFraction
having count(*) = (select count(*) from ##defaultdb##.hourOfAnyDay)
order by sourceTypeID, roadTypeID, dayID LIMIT 1;

--       check no. 2809: make sure weekend and weekday profiles are different
INSERT INTO QA_Checks_Log values ( 2809, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID,
         roadTypeID)
Select   "hourVMTFraction" as tableName,
         2809 as checkNumber,
         'hourVMTFraction is the same between weekend and weekday' as testDescription,
		 sourceTypeID,
         roadTypeID
from (select sourceTypeID, roadTypeID, hourID, hourVMTFraction as weekendFraction
	  from hourVMTFraction
	  where dayID = 2 and roadTypeID not in (1, 100)) as we
join (select sourceTypeID, roadTypeID, hourID, hourVMTFraction as weekdayFraction
	  from hourVMTFraction
	  where dayID = 5 and roadTypeID not in (1, 100)) as wd using (sourceTypeID, roadTypeID, hourID)
group by sourceTypeID, roadTypeID
having sum(abs(weekendFraction - weekdayFraction)) < 0.00001
order by sourceTypeID, roadTypeID LIMIT 1;

--       check no. 2810: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2810, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'hourVMTFraction' as TableName, 
		2810 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'hourvmtfraction') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'hourvmtfraction') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- HPMSVTypeDay
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (2900, "HPMSVTypeDay", "Table Check:");

--       check no. 2901 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 2901, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId       as yearId2,
         'no '        as aMatch,
         count(*)     as n
From     hpmsVTypeDay
Group by yearId2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         yearId  )
Select   "hpmsVTypeDay"   as tableName,
         2901,
        "yearId"          as testDescription,
         yearId2          as testValue,
         n                as count,
         yearId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 2902 -- check for unknown monthIDs
INSERT INTO QA_Checks_Log values ( 2902, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthId      as monthId2,
         'no '        as aMatch,
         count(*)     as n
From     hpmsVTypeDay
Group by monthId2;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId2 = m.monthId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         monthId  )
Select   "hpmsVTypeDay" as tableName,
         2902,
         "monthId"      as testDescription,
         monthId2       as testValue,
         n              as count,
         monthId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 2903 -- check for unknown dayIDs
INSERT INTO QA_Checks_Log values ( 2903, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   dayId        as dayId2,
         'no '        as aMatch,
         count(*)     as n
From     hpmsVTypeDay
Group by dayId2;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId2 = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         dayId   )
Select   "hpmsVTypeDay" as tableName,
         2903,
         "dayId"        as testDescription,
         dayId2         as testValue,
         n              as count,
         dayId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 2904 -- check for unknown hpmsVTypeIDs
INSERT INTO QA_Checks_Log values ( 2904, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   HPMSVtypeID  as HPMSVtypeID2,
         'no '        as aMatch,
         count(*)     as n
From     hpmsVTypeDay
Group by hpmsVTypeID2;

Update tempA as a 
inner join ##defaultdb##.hpmsvtype as m on a.HPMSVtypeID2 = m.HPMSVtypeID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count)
Select   "hpmsVTypeDay" as tableName,
         2904,
         "HPMSVtypeID"        as testDescription,
         HPMSVtypeID2         as testValue,
         n              as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 2905: check for missing yearID, monthID, dayID, HPMSVtypeID combinations
--                       only when hpmsvtypeday is used
INSERT INTO QA_Checks_Log values ( 2905, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         yearID, monthID, dayID, HPMSVtypeID)
Select   "hpmsvtypeday" as tableName,
         2905,
         'Missing combination of valid yearID, monthID, dayID, HPMSVtypeID' as testDescription,
		 yearID, monthID, dayID, HPMSVtypeID
from (
	SELECT yearID, monthID, dayID, HPMSVtypeID
	FROM  `year`
	CROSS JOIN ##defaultdb##.monthOfAnyYear
	CROSS JOIN ##defaultdb##.dayOfAnyWeek
	CROSS JOIN ##defaultdb##.hpmsvtype
) as t1
left join hpmsvtypeday using (yearID, monthID, dayID, HPMSVtypeID)
join (select count(*) as n from hpmsvtypeday) as t2
where VMT is NULL and n > 0
ORDER BY yearID, monthID, dayID, HPMSVtypeID LIMIT 1;

--       check no. 2906: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (2906, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'hpmsVtypeDay' as TableName, 
		2906 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'hpmsvtypeday') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'hpmsvtypeday') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- checks for HPMSVTypeYear
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3000, "hpmsVTypeYear", "Table Check:");

--       check no. 3001 -- check for unknown HPMSVTypeIDs
INSERT INTO QA_Checks_Log values ( 3001, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   HPMSVtypeID  as HPMSVtypeID2,
         'no '        as aMatch,
         count(*)     as n
From     hpmsVTypeYear
Group by HPMSVtypeID2;

Update tempA as a 
inner join ##defaultdb##.hpmsvtype as m on a.HPMSVtypeID2 = m.HPMSVtypeID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "hpmsVTypeYear"   as tableName,
         3001,
        "hpmSvTypeId"      as testDescription,
         HPMSVtypeId2      as testValue,
         n                 as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3002 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 3002, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId          as yearId2,
         'no '           as aMatch,
         count(*)        as n
From     hpmsVTypeYear
Group by yearId2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         yearId  )
Select  "hpmsVTypeYear"   as tableName,
         3002,
        "yearId"      as testDescription,
         yearId2      as testValue,
         n            as count,
         yearId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 3003: check for missing yearID, HPMSVtypeID combinations
INSERT INTO QA_Checks_Log values ( 3003, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         yearID, HPMSVtypeID)
Select   "hpmsvtypeyear" as tableName,
         3003,
         'Missing combination of valid yearID, HPMSVtypeID' as testDescription,
		 yearID, HPMSVtypeID
from (
	SELECT yearID, HPMSVtypeID
	FROM  `year`
	CROSS JOIN ##defaultdb##.hpmsvtype
) as t1
left join hpmsvtypeyear using (yearID, HPMSVtypeID)
join (select count(*) as n from hpmsvtypeyear) as t2
where HPMSBaseYearVMT is NULL and n > 0
ORDER BY yearID, HPMSVtypeID LIMIT 1;

--       check no. 3004: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3004, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'hpmsVtypeYear' as TableName, 
		3004 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'hpmsvtypeyear') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'hpmsvtypeyear') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- idleDayAdjust
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3100, "idleDayAdjust", "Table Check:");

--       check no. 3101 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3101, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     idledayadjust
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "idledayadjust" as tableName,
         3101,
        "sourceTypeId"    as testDescription,
         sourceTypeId2    as testValue,
         n                as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3102 -- checks unknown dayIDs
INSERT INTO QA_Checks_Log values ( 3102, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   dayId    as dayId2,
         'no '    as aMatch,
         count(*) as n
From     idledayadjust
Group by dayId2;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId2 = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "idledayadjust" as tableName,
         3102,
         "dayId"           as testDescription,
         dayId2            as testValue,
         n                 as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3103: check for missing sourceTypeID and dayID combinations
INSERT INTO QA_Checks_Log values ( 3103, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID, dayID)
Select   "idledayadjust" as tableName,
         3103,
         'Missing combination of valid sourceTypeID, dayID' as testDescription,
		 sourceTypeID, dayID
from (
	SELECT sourceTypeID, dayID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.dayofanyweek
) as t1 left join idledayadjust using (sourceTypeID, dayID)
join (select count(*) as n from idledayadjust) as t2
where idleDayAdjust is NULL and n > 0
ORDER BY sourceTypeID, dayID LIMIT 1;

--       check no. 3104: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3104, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'idleDayAdjust' as TableName, 
		3104 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'idledayadjust') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'idledayadjust') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- idlemodelyeargrouping
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3200, "idlemodelyeargrouping", "Table Check:");

--       check no. 3201 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3201, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     idlemodelyeargrouping
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "idlemodelyeargrouping" as tableName,
         3201,
        "sourceTypeId"    as testDescription,
         sourceTypeId2    as testValue,
         n                as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3202: check for missing source types if this table is populated
INSERT INTO QA_Checks_Log values ( 3202, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID)
Select   "idlemodelyeargrouping" as tableName,
         3202,
         'Missing sourceTypeID' as testDescription,
		 sourceTypeID
from ##defaultdb##.sourceusetype
left join idlemodelyeargrouping using (sourceTypeID)
join (select count(*) as n from idlemodelyeargrouping) as t1
where totalIdleFraction is NULL and n > 0
ORDER BY sourceTypeID LIMIT 1;

--       check no. 3203 -- Check for gaps and overlaps in the model years columns in the idlemodelyeargrouping table.
INSERT INTO QA_Checks_Log values ( 3203, 'OK', @hVersion, curDate(), curTime() );

-- Add a table to contain the results of the gaps/overlaps check.
Drop   table if exists     qa_checks_imyg;
Create Table if Not Exists qa_checks_imyg (
  CsourceType   int,   -- sourceTypeID
  LMaxMY int,          -- last    row's max model year
  CMinMY int,          -- Current row's min model year
  CMaxMY int,          -- Current row's max model year
  Reason varchar(40) );

-- Call the procedure to check for gaps and overlaps.
call checkIdleModelYearGrouping();

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         count  )
Select  "idlemodelyeargrouping" as tableName,
         3203,
        "gaps and overlaps" as testDescription,
         (Select count(*) from qa_checks_imyg) as count
From     qa_checks_imyg
Where    (Select count(*) from qa_checks_imyg) > 0;

--       check no. 3204: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3204, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'idleModelYearGrouping' as TableName, 
		3204 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'idlemodelyeargrouping') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'idlemodelyeargrouping') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- idlemonthadjust
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3300, "idlemonthadjust", "Table Check:");

--       check no. 3301 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3301, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     idlemonthadjust
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "idlemonthadjust" as tableName,
         3301,
        "sourceTypeId"    as testDescription,
         sourceTypeId2    as testValue,
         n                as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3302 -- checks unknown monthIDs
INSERT INTO QA_Checks_Log values ( 3302, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthID    as monthID2,
         'no '    as aMatch,
         count(*) as n
From     idlemonthadjust
Group by monthID2;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId2 = m.monthId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "idlemonthadjust" as tableName,
         3302,
         "monthID"           as testDescription,
         monthID2            as testValue,
         n                 as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3303: check for missing sourceTypeID and monthID combinations
INSERT INTO QA_Checks_Log values ( 3303, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID, monthID)
Select   "idlemonthadjust" as tableName,
         3303,
         'Missing combination of valid sourceTypeID, monthID' as testDescription,
		 sourceTypeID, monthID
from (
	SELECT sourceTypeID, monthID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.monthOfAnyYear
) as t1 left join idlemonthadjust using (sourceTypeID, monthID)
join (select count(*) as n from idlemonthadjust) as t2
where idlemonthadjust is NULL and n > 0
ORDER BY sourceTypeID, monthID LIMIT 1;

--       check no. 3304: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3304, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'idleMonthAdjust' as TableName, 
		3304 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'idlemonthadjust') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'idlemonthadjust') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;



-- totalidlefraction
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3400, "totalidlefraction", "Table Check:");

--       check no. 3401 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3401, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     totalidlefraction
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "totalidlefraction" as tableName,
         3401,
        "sourceTypeId"    as testDescription,
         sourceTypeId2    as testValue,
         n                as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3402 -- checks unknown monthIDs
INSERT INTO QA_Checks_Log values ( 3402, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthID    as monthID2,
         'no '    as aMatch,
         count(*) as n
From     totalidlefraction
Group by monthID2;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId2 = m.monthId
set aMatch='yes';
	
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "totalidlefraction" as tableName,
         3402,
         "monthID"           as testDescription,
         monthID2            as testValue,
         n                 as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3403 -- checks unknown dayIDs
INSERT INTO QA_Checks_Log values ( 3403, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   dayId    as dayId2,
         'no '    as aMatch,
         count(*) as n
From     totalidlefraction
Group by dayId2;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId2 = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "totalidlefraction" as tableName,
         3403,
         "dayId"           as testDescription,
         dayId2            as testValue,
         n                 as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3404 -- checks unknown idleRegionID
INSERT INTO QA_Checks_Log values ( 3404, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   idleRegionID    as idleRegionID2,
         'no '    as aMatch,
         count(*) as n
From     totalidlefraction
Group by idleRegionID2;

Update tempA as a 
inner join ##defaultdb##.idleregion as m on a.idleRegionID2 = m.idleRegionID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "totalidlefraction" as tableName,
         3404,
         "idleRegionID"      as testDescription,
         idleRegionID2       as testValue,
         n                   as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3405 -- check for multiple idleRegionIDs
INSERT INTO QA_Checks_Log values ( 3405, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         `count`  )
Select   "totalidlefraction"           as tableName,
         3405,
        "Multiple idleRegionIDs"       as testDescription,
         count(distinct idleRegionID) as `count`
from totalidlefraction
having `count` > 1;

--       check no. 3406 -- checks unknown countyTypeID
INSERT INTO QA_Checks_Log values ( 3406, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   countyTypeID    as countyTypeID2,
         'no '    as aMatch,
         count(*) as n
From     totalidlefraction
Group by countyTypeID2;

Update tempA as a 
inner join ##defaultdb##.countytype as m on a.countyTypeID2 = m.countyTypeID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "totalidlefraction" as tableName,
         3406,
         "countyTypeID"      as testDescription,
         countyTypeID2       as testValue,
         n                   as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3407 -- check for multiple countyTypeIDs
INSERT INTO QA_Checks_Log values ( 3407, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         `count`  )
Select   "totalidlefraction"          as tableName,
         3407,
        "Multiple countyTypeIDs"      as testDescription,
         count(distinct countyTypeID) as `count`
from totalidlefraction
having `count` > 1;

--       check no. 3408 -- Check for gaps and overlaps in the model years columns in the TIF table.
INSERT INTO QA_Checks_Log values ( 3408, 'OK', @hVersion, curDate(), curTime() );

-- Add a table to contain the results of the gaps/overlaps check.
Drop   table if exists     qa_checks_tif;
Create Table if Not Exists qa_checks_tif (
  CsourceType   int,   -- sourceTypeID
  Cmonth		int,   -- monthID
  Cday          int,   -- dayID
  LMaxMY int,          -- last    row's max model year
  CMinMY int,          -- Current row's min model year
  CMaxMY int,          -- Current row's max model year
  Reason varchar(40) );

-- Call the procedure to check for gaps and overlaps.
call checkTotalIdleFraction();

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         count  )
Select  "totalidlefraction" as tableName,
         3408,
        "gaps and overlaps" as testDescription,
         (Select count(*) from qa_checks_tif) as count
From     qa_checks_tif
Where    (Select count(*) from qa_checks_tif) > 0;

--       check no. 3409: check for missing sourceTypeID, monthID, dayID combinations
INSERT INTO QA_Checks_Log values ( 3409, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID, monthID, dayID)
Select   "totalidlefraction" as tableName,
         3409,
         'Missing combination of valid sourceTypeID, monthID, dayID' as testDescription,
		 sourceTypeID, monthID, dayID
from (
	SELECT sourceTypeID, monthID, dayID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.monthOfAnyYear
	CROSS JOIN ##defaultdb##.dayOfAnyWeek
) as t1 left join totalidlefraction using (sourceTypeID, monthID, dayID)
join (select count(*) as n from totalidlefraction) as t2
where totalIdleFraction is NULL and n > 0
ORDER BY sourceTypeID, monthID, dayID LIMIT 1;

--       check no. 3410: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3410, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'totalidlefraction' as TableName, 
		3410 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'totalidlefraction') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'totalidlefraction') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- IMCoverage checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3500, "imCoverage", "Table Check:");

--       check no. 3501 -- check for unknown countyIDs
INSERT INTO QA_Checks_Log values ( 3501, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   countyId        as countyId2,
         'no '           as aMatch,
         count(*)        as n
From     imCoverage
Group by countyId2;

Update tempA as a 
inner join county as c on a.countyId2 = c.countyId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "imCoverage" as tableName,
         3501,
        "countyId"      as testDescription,
         countyId2      as testValue,
         n              as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3502 -- check for unknown fuelTypeIDs
INSERT INTO QA_Checks_Log values ( 3502, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   fuelTypeId      as fuelTypeId2,
         'no '           as aMatch,
         count(*)        as n
From     imCoverage
Group by fuelTypeId2;

Update tempA as a 
inner join ##defaultdb##.fuelType as m on a.FuelTypeId2 = m.fuelTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "imCoverage" as tableName,
         3502,
        "fuelTypeId" as testDescription,
         fuelTypeId2 as testValue,
         n           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3503 -- check for unknown polProcessIDs
INSERT INTO QA_Checks_Log values ( 3503, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   polProcessId    as polProcessId2,
         'no '           as aMatch,
         count(*)        as n
From     imCoverage
Group by polProcessId2;
							  
Update tempA as a 
inner join ##defaultdb##.pollutantProcessAssoc as m on a.polProcessId2 = m.polProcessId
set aMatch='yes';										 

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "imCoverage" as tableName,
         3503,
        "polProcessId"      as testDescription,
         polProcessId2      as testValue,
         n                  as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3504 -- check that inspectFreq is either 1 or 2
INSERT INTO QA_Checks_Log values ( 3504, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   inspectFreq     as inspectFreq2,
         'no '           as aMatch,
         count(*)        as n
From     imCoverage
Group by inspectFreq2;

Update tempA as a set aMatch='yes' where inspectFreq2 in (1,2);

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select  "imCoverage"  as tableName,
         3504,
        "inspectFreq" as testDescription,
         inspectFreq2 as testValue,
         n            as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3505 -- check that useIMyn is either "y" or "n"
INSERT INTO QA_Checks_Log values ( 3505, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   useIMyn         as useIMyn2,
         'no '           as aMatch,
         count(*)        as n
From     imCoverage
Group by useIMyn2;

Update tempA as a set aMatch='yes' where useIMyn2 in ('y', 'n', 'Y', 'N');

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "imCoverage" as tableName,
         3505,
        "useIMyn not Y or N" as testDescription,
         useIMyn2      as testValue,
         n         as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3506 -- check that the complianceFactor is between 0 and 100
INSERT INTO QA_Checks_Log values ( 3506, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   complianceFactor as complianceFactor2,
         'no '            as aMatch,
         count(*)         as n
From     imCoverage
Group by complianceFactor;

Update tempA as a set aMatch='yes' where complianceFactor2>=0.0 and complianceFactor2<=100.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "imCoverage" as tableName,
         3506,
        "complianceFactor range" as testDescription,
         complianceFactor2       as testValue,
         n                       as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3507 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3507, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     imCoverage
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "imCoverage"  as tableName,
         3507,
        "sourceTypeId" as testDescription,
         sourceTypeId2 as testValue,
         n             as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3508 -- check for unknown stateIDs
INSERT INTO QA_Checks_Log values ( 3508, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   stateId      as stateId2,
         'no '        as aMatch,
         count(*)     as n
From     imcoverage
Group by stateId2;

Update tempA as a 
inner join state as c on a.stateId2 = c.stateId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "imCoverage"        as tableName,
         3508,
         "stateId not valid" as testDescription,
         stateId2            as testValue,
         n                   as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3509 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 3509, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId          as yearId2,
         'no '           as aMatch,
         count(*)        as n
From     imcoverage
Group by yearId2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "imCoverage" as tableName,
         3509,
        "yearId"      as testDescription,
         yearId2      as testValue,
         n            as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3510 -- check to make sure there are the same number of entries (by state, county, year, and source type) for gasoline and FFV.
INSERT INTO QA_Checks_Log values ( 3510, 'OK', @hVersion, curDate(), curTime() );

Drop table if exists tempA;
Create table tempA
select stateId,
       countyId,
       yearId,
       sourceTypeId,
       fuelTypeId
from   imCoverage;

alter table tempA add numFtype1 int default 0;
alter table tempA add numFtype5 int default 0;

drop   table if exists tempB;
create table           tempB
select * from tempA;

create INDEX idxA on tempA (stateId, countyId, yearId, sourceTypeId);
create INDEX idxB on tempB (stateId, countyId, yearId, sourceTypeId);

update tempA as a set a.numFtype1 = (select count(*)
                                     from tempB as b
                                     where b.fuelTypeId=1
                                       and a.stateId      = b.stateId
                                       and a.countyId     = b.countyId
                                       and a.yearId       = b.yearId
                                       and a.sourceTypeId = b.sourceTypeId
                                     group by               b.stateId,
                                                            b.countyId,
                                                            b.yearId,
                                                            b.sourceTypeId);

update tempA as a set a.numFtype5 = (select count(*)
                                     from tempB as b
                                     where b.fuelTypeId=5
                                       and a.stateId      = b.stateId
                                       and a.countyId     = b.countyId
                                       and a.yearId       = b.yearId
                                       and a.sourceTypeId = b.sourceTypeId
                                     group by               b.stateId,
                                                            b.countyId,
                                                            b.yearId,
                                                            b.sourceTypeId);

update tempa set numFtype1=0 where isnull(numFtype1);
update tempa set numFtype5=0 where isnull(numFtype5);

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         count,
         stateId,
         countyId,
         yearId,
         sourceTypeId )
Select  'imCoverage'               as tableName,
         3510                       as checkNumber,
        'counts fuelTypesIds 1<>5' as testDescription,
         count(*)                  as count,
         stateId,
         countyId,
         yearId,
         sourceTypeId
from     tempA
where numFtype1<>numFtype5
group by stateId,
         countyId,
         yearId,
         sourceTypeId;

--       check no. 3511 -- Check for gaps and overlaps in the IMCoverage table.
-- Add a table to contain the results of the IMCoverage gaps/overlaps check.
Drop   table if exists     qa_checks_im;
Create Table if Not Exists qa_checks_im (
  Cpol   int,          -- polProcessId
  Ccou   int,          -- CountyId
  Cyea   int,          -- yearId
  Csou   int,          -- sourceTypeId
  Cfue   int,          -- fuelTypeId
  LENDMY int,          -- last    row's end model year
  CBEGMY int,          -- Current row's beg model year
  CENDMY int,          -- Current row's end model year
  useIMyn char(1),
  Reason varchar(40) );

-- Call the procedure to check for gaps and overlaps.
call checkImCoverage();

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         count  )
Select  "imCoverage" as tableName,
         3511,
        "gaps and overlaps" as testDescription,
         (Select count(*) from qa_checks_im) as count
From     qa_checks_im
Where    (Select count(*) from qa_checks_im) > 0;

--       check no. 3512: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3512, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'imCoverage' as TableName, 
		3512 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'imcoverage') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'imcoverage') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- monthVMTFraction checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3600, "monthVmtFraction", "Table Check:");

--       check no. 3601 -- check for unknown monthIDs
INSERT INTO QA_Checks_Log values ( 3601, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthId      as monthId2,
         'no '        as aMatch,
         count(*)     as n
From     monthVmtFraction
Group by monthId2;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId2 = m.monthId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "monthVmtFraction" as tableName,
         3601,
        "monthId"           as testDescription,
         monthId2           as testValue,
         n                  as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 3602 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3602, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     monthVmtFraction
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "monthVmtFraction" as tableName,
         3602,
         "sourceTypeId"     as testDescription,
         sourceTypeId2      as testValue,
         n                  as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3603 -- check that monthVMTFraction sums to 1 for each sourceTypeID
INSERT INTO QA_Checks_Log values ( 3603, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
         sum(monthVmtfraction) as s,
         'n' as Ok
from     monthVMTfraction
group by sourceTypeId;

update tempA set Ok = 'y' where s > 0.99999 and s < 1.00001 and s is not null;

Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue,
         sourceTypeId
       )
Select  'monthVmtFraction'    as tableName,
         3603                  as checkNumber,
         'sum of monthVMTFraction <> 1' as testDescription,
         s                    as testValue,
         sourceTypeId         as sourceType
From     tempA
Where    Ok = 'n';

--       check no. 3604 -- check for monthVMTFractions >= 1
INSERT INTO QA_Checks_Log values ( 3604, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID)
Select   "monthvmtfraction" as tableName,
         3604,
         'monthVMTFraction >= 1' as testDescription,
		 monthVMTFraction as testValue,
         sourceTypeID
from monthvmtfraction
where monthVMTFraction >= 1
ORDER BY sourceTypeID LIMIT 1;

--       check no. 3605: make sure month distributions aren't flat
INSERT INTO QA_Checks_Log values ( 3605, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID)
Select   "monthvmtfraction" as tableName,
         3605 as checkNumber,
         'monthVMTFraction is a flat profile' as testDescription,
         concat('all are ', monthVMTFraction) as testValue,
		 sourceTypeID
from monthvmtfraction
group by sourceTypeID, monthVMTFraction
having count(*) = (select count(*) from ##defaultdb##.monthOfAnyYear)
order by sourceTypeID LIMIT 1;

--       check no. 3606: check for missing sourceTypeID, monthID combinations
--                       only if monthVMTFraction is used
INSERT INTO QA_Checks_Log values ( 3606, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID, monthID)
Select   "monthVMTFraction" as tableName,
         3606,
         'Missing combination of valid sourceTypeID, monthID' as testDescription,
		 sourceTypeID, monthID
from (
	SELECT sourceTypeID, monthID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.monthOfAnyYear
) as t1 
left join monthVMTFraction using (sourceTypeID, monthID)
join (select count(*) as n from monthVMTFraction) as t2
where monthVMTFraction.monthVMTFraction is NULL and n > 0
ORDER BY sourceTypeID, monthID LIMIT 1;

--       check no. 3607: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3607, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'monthVMTFraction' as TableName, 
		3607 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'monthvmtfraction') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'monthvmtfraction') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- onroadretrofit
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3700, "onRoadRetroFit", "Table Check:");

--       check no. 3701 -- check for unknown pollutantIDs
INSERT INTO QA_Checks_Log values ( 3701, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select  pollutantId,
       'no '    as aMatch,
       count(*) as n
from   onRoadRetroFit
group by pollutantId;

Update tempA as a 
inner join ##defaultdb##.pollutant as m on a.pollutantId = m.pollutantId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit" as tableName,
         3701              as checkNumber,
        "pollutantId"     as testDescription,
         pollutantId      as testValue,
         count(*)         as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=3701 and count=0;

--       check no. 3702 -- check for unknown processIDs
INSERT INTO QA_Checks_Log values ( 3702, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select  processId,
       'no '    as aMatch,
       count(*) as n
from   onRoadRetroFit
group by processId;
									  
Update tempA as a 
inner join ##defaultdb##.pollutantProcessAssoc as m on a.processId = m.processId
set aMatch='yes';	
									  
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit" as tableName,
         3702              as checkNumber,
        "processId"       as testDescription,
         processId        as testValue,
         count(*)         as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=3702 and count=0;

--       check no. 3703 -- check for unknown fuelTypeIDs
INSERT INTO QA_Checks_Log values ( 3703, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select  fuelTypeId ,
       'no '    as aMatch,
       count(*) as n
from   onRoadRetroFit
group by fuelTypeId;

Update tempA as a 
inner join ##defaultdb##.fuelType as m on a.FuelTypeId = m.fuelTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit" as tableName,
         3703              as checkNumber,
        "fuelTypeId "     as testDescription,
         fuelTypeId       as testValue,
         count(*)         as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=3703 and count=0;

--       check no. 3704 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3704, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select  sourceTypeId,
       'no '    as aMatch,
       count(*) as n
from   onRoadRetroFit
group by sourceTypeId;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit" as tableName,
         3704              as checkNumber,
        "sourceTypeId"    as testDescription,
         sourceTypeId     as testValue,
         count(*)         as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=3704 and count=0;

--       check no. 3705 -- check that the retrofitYearID <= the analysis year
INSERT INTO QA_Checks_Log values ( 3705, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit" as tableName,
         3705              as checkNumber,
        "retrofitYearID > yearID"  as testDescription,
         retrofitYearId   as testValue,
         count(*)         as cou
from     onroadretrofit
join     `year` on yearID
where    retrofitYearID > yearID
group by retrofitYearID;

--       check no. 3706 -- check that endModelYearID <= retrofitYearID
INSERT INTO QA_Checks_Log values ( 3706, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit" as tableName,
         3706              as checkNumber,
        "endModelYearId > retrofitYearID"  as testDescription,
         CONCAT(endModelYearId, ' > ', retrofitYearID) as testValue,
         count(*)         as cou
from     onroadretrofit
where    endModelYearID > retrofitYearID
group by endModelYearID, retrofitYearID;

--       check no. 3707 -- check that beginModelYearID <= endModelYearID
INSERT INTO QA_Checks_Log values ( 3707, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit"  as tableName,
         3707               as checkNumber,
        "beginModelYearId > endModelYearID" as testDescription,
         CONCAT(beginModelYearId, ' > ', endModelYearID)  as testValue,
         count(*)          as cou
from     onroadretrofit
where    beginModelYearId > endModelYearID
group by beginModelYearId, endModelYearID;

--       check no. 3708 -- check that cumFractionRetrofit between 0 and 1
INSERT INTO QA_Checks_Log values ( 3708, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit"  as tableName,
         3708               as checkNumber,
        "cumFractionRetrofit range" as testDescription,
         cumFractionRetrofit as testValue,
         count(*)          as cou
from     onroadretrofit
where    cumFractionRetrofit NOT BETWEEN 0 and 1
group by cumFractionRetrofit;

--       check no. 3709 -- check that retrofitEffectiveFraction <= 1
INSERT INTO QA_Checks_Log values ( 3709, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "onRoadRetrofit"  as tableName,
         3709               as checkNumber,
        "retrofitEffectiveFraction range" as testDescription,
         retrofitEffectiveFraction as testValue,
         count(*)          as cou
from     onroadretrofit
where    retrofitEffectiveFraction > 1
group by retrofitEffectiveFraction;

--       check no. 3710: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3710, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'onRoadRetrofit' as TableName, 
		3710 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'onroadretrofit') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'onroadretrofit') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- roadTypeDistribution checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3800, "roadTypeDistribution", "Table Check:");

--       check no. 3801 -- check for unknown roadTypeID
INSERT INTO QA_Checks_Log values ( 3801, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   roadTypeId as roadTypeId2,
         'no '      as aMatch,
         count(*)   as n
From     roadTypeDistribution
Group by roadTypeId2;

Update tempA as a 
inner join ##defaultdb##.roadType as m on a.roadTypeId2 = m.roadTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "roadTypeDistribution" as tableName,
         3801,
        "roadTypeId"            as testDescription,
         roadTypeId2            as testValue,
         n                      as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3802 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3802, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     roadTypeDistribution
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "roadTypeDistribution" as tableName,
         3802,
        "sourceTypeId"          as testDescription,
         sourceTypeId2          as testValue,
         n                      as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3803 -- check that the roadTypeVMTFraction sums to 1
INSERT INTO QA_Checks_Log values ( 3803, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue,
           sourceTypeId  )
Select  "roadTypeDistribution"              as tableName,
         3803                                 as checkNumber,
        "sum of roadTypeVmtFraction <> 1.0" as testDescription,
         sum(roadTypeVmtFraction)           as testValue,
           sourceTypeId
From     roadTypeDistribution
Group by sourceTypeId
Having   testValue <0.99999 or testValue >1.00001;

--       check no. 3804 -- check for roadTypeVMTFraction >= 1
INSERT INTO QA_Checks_Log values ( 3804, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID)
Select   "roadtypedistribution" as tableName,
         3804,
         'roadTypeVMTFraction >= 1' as testDescription,
		 roadTypeVMTFraction as testValue,
         sourceTypeID
from roadtypedistribution
where roadTypeVMTFraction >= 1
ORDER BY sourceTypeID LIMIT 1;

--       check no. 3805: make sure road type distributions aren't flat
INSERT INTO QA_Checks_Log values ( 3805, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID)
Select   "roadtypedistribution" as tableName,
         3805 as checkNumber,
         'roadTypeVMTFraction is a flat profile' as testDescription,
         concat('all are ', roadTypeVMTFraction) as testValue,
		 sourceTypeID
from roadtypedistribution
where roadTypeID not in (1, 100)
group by sourceTypeID, roadTypeVMTFraction
having count(*) = (select count(*) from ##defaultdb##.roadtype where roadTypeID not in (1, 100))
order by sourceTypeID LIMIT 1;

--       check no. 3806: check for missing sourceTypeID, roadTypeID combinations
INSERT INTO QA_Checks_Log values ( 3806, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         sourceTypeID, roadTypeID)
Select   "roadtypedistribution" as tableName,
         3806,
         'Missing combination of valid sourceTypeID, roadTypeID' as testDescription,
		 sourceTypeID, roadTypeID
from (
	SELECT sourceTypeID, roadTypeID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.roadtype where roadTypeID in (2, 3, 4, 5)
) as t1 left join roadtypedistribution using (sourceTypeID, roadTypeID)
where roadTypeVMTFraction is NULL 
ORDER BY sourceTypeID, roadTypeID LIMIT 1;

--       check no. 3807: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3807, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'roadTypeDistribution' as TableName, 
		3807 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'roadtypedistribution') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'roadtypedistribution') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- SourceTypeAgeDistribution checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (3900, "sourceTypeAgeDistribution", "Table Check:");

--       check no. 3901  -- check for unknown ageIDs
INSERT INTO QA_Checks_Log values ( 3901, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   ageId         as ageId2,
         'no '         as aMatch,
         count(*)      as n
From     sourceTypeAgeDistribution
Group by ageId2;

Update tempA as a 
inner join ##defaultdb##.ageCategory as m on a.ageId2 = m.ageId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "sourceTypeAgeDistribution" as tableName,
         3901,
        "ageId"                      as testDescription,
         ageId2                      as testValue,
         n                           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3902 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 3902, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId as sourceTypeId2,
         'no '        as aMatch,
         count(*)     as n
From     sourceTypeAgeDistribution
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "sourceTypeAgeDistribution" as tableName,
         3902,
        "sourceTypeId"               as testDescription,
         sourceTypeId2               as testValue,
         n                           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3903 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 3903, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId       as yearId2,
         'no '        as aMatch,
         count(*)     as n
From     sourceTypeAgeDistribution
Group by yearId2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "sourceTypeAgeDistribution" as tableName,
         3903,
        "yearId"                     as testDescription,
         yearId2                     as testValue,
         n                           as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 3904 -- check that ageFraction sums to 1
INSERT INTO QA_Checks_Log values ( 3904, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue,
           sourceTypeId,
           yearId)
Select  "sourceTypeAgeDistribution"   as tableName,
         3904                           as checkNumber,
        "sum of AgeFraction <> 1.0"   as testDescription,
         sum(ageFraction)             as testValue,
           sourceTypeId,
           yearId
From     sourceTypeAgeDistribution
Group by sourceTypeId,
         yearId
Having   testValue <0.99999 or testValue >1.00001;

--       check no. 3905: check for missing sourceTypeID, yearID, ageID combinations
INSERT INTO QA_Checks_Log values ( 3905, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue)
Select   "sourcetypeagedistribution" as tableName,
         3905,
         'Missing combination of valid sourceTypeID, yearID, ageID' as testDescription,
		 concat('ST: ', sourceTypeID, ', year: ', yearID, ', age: ', ageID) as testValue
from (
	SELECT sourceTypeID, yearID, ageID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN year
	CROSS JOIN ##defaultdb##.agecategory
) as t1 left join sourcetypeagedistribution using (sourceTypeID, yearID, ageID)
where ageFraction is NULL 
ORDER BY sourceTypeID, yearID, ageID LIMIT 1;

--       check no. 3906: make sure age distributions aren't flat
INSERT INTO QA_Checks_Log values ( 3906, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID, yearID)
Select   "sourcetypeagedistribution" as tableName,
         3906 as checkNumber,
         'ageFraction is a flat profile' as testDescription,
         concat('all are ', ageFraction) as testValue,
		 sourceTypeID, yearID
from sourcetypeagedistribution
group by sourceTypeID, yearID, ageFraction
having count(*) = (select count(*) from ##defaultdb##.ageCategory)
order by sourceTypeID, yearID LIMIT 1;

--       check no. 3907: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (3907, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'sourceTypeAgeDistribution' as TableName, 
		3907 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'sourcetypeagedistribution') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'sourcetypeagedistribution') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- sourceTypeDayVMT checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4000, "sourceTypeDayVMT", "Table Check:");

--       check no. 4001 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 4001, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId       as yearId2,
         'no '        as aMatch,
         count(*)     as n
From     sourceTypeDayVmt
Group by yearId2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         yearId  )
Select   "sourceTypeDayVmt" as tableName,
         4001,
        "yearId"            as testDescription,
         yearId2            as testValue,
         n                  as count,
         yearId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 4002 -- check for unknown monthIDs
INSERT INTO QA_Checks_Log values ( 4002, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthId      as monthId2,
         'no '        as aMatch,
         count(*)     as n
From     sourceTypeDayVmt
Group by monthId2;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId2 = m.monthId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         monthId  )
Select   "sourceTypeDayVmt" as tableName,
         4002,
         "monthId"          as testDescription,
         monthId2           as testValue,
         n                  as count,
         monthId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 4003 -- check for unknown dayIDs
INSERT INTO QA_Checks_Log values ( 4003, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   dayId        as dayId2,
         'no '        as aMatch,
         count(*)     as n
From     sourceTypeDayVmt
Group by dayId2;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId2 = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         dayId  )
Select   "sourceTypeDayVmt" as tableName,
         4003,
         "dayId"            as testDescription,
         dayId2             as testValue,
         n                  as count,
         dayId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 4004 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4004, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId   as sourceTypeId2,
         'no '          as aMatch,
         count(*)       as n
From     sourceTypeDayVmt
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         sourceTypeId  )
Select   "sourceTypeDayVmt" as tableName,
         4004,
        "sourceTypeId"      as testDescription,
         sourceTypeId2      as testValue,
         n                  as count,
         sourceTypeId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 4005: check for missing yearID, monthID, dayID, sourceTypeID combinations
--                       only when sourcetypedayvmt is used
INSERT INTO QA_Checks_Log values ( 4005, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         yearID, monthID, dayID, sourceTypeID)
Select   "sourcetypedayvmt" as tableName,
         4005,
         'Missing combination of valid yearID, monthID, dayID, sourceTypeID' as testDescription,
		 yearID, monthID, dayID, sourceTypeID
from (
	SELECT yearID, monthID, dayID, sourceTypeID
	FROM  `year`
	CROSS JOIN ##defaultdb##.monthOfAnyYear
	CROSS JOIN ##defaultdb##.dayOfAnyWeek
	CROSS JOIN ##defaultdb##.sourceusetype
) as t1
left join sourcetypedayvmt using (yearID, monthID, dayID, sourceTypeID)
join (select count(*) as n from sourcetypedayvmt) as t2
where VMT is NULL and n > 0
ORDER BY yearID, monthID, dayID, sourceTypeID LIMIT 1;

--       check no. 4006: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4006, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'sourceTypeDayVMT' as TableName, 
		4006 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'sourcetypedayvmt') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'sourcetypedayvmt') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- sourceTypeYearVMT
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4100, "sourceTypeYearVMT", "Table Check:");

--       check no. 4101 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 4101, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId       as yearId2,
         'no '        as aMatch,
         count(*)     as n
From     sourceTypeYearVmt
Group by yearId2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         yearId  )
Select   "sourceTypeYearVmt" as tableName,
         4101,
        "yearId"             as testDescription,
         yearId2             as testValue,
         n                   as count,
         yearId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 4102 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4102, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId   as sourceTypeId2,
         'no '          as aMatch,
         count(*)       as n
From     sourceTypeYearVMT
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         sourceTypeId  )
Select   "sourceTypeYearVmt" as tableName,
         4102,
        "sourceTypeId"       as testDescription,
         sourceTypeId2       as testValue,
         n                   as count,
         sourceTypeId2
From     tempA
Where    aMatch <> 'yes';

--       check no. 4103: check for missing yearID, sourceTypeID combinations
--                       only when sourcetypeyearvmt is used
INSERT INTO QA_Checks_Log values ( 4103, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         yearID, sourceTypeID)
Select   "sourcetypeyearvmt" as tableName,
         4103,
         'Missing combination of valid yearID, sourceTypeID' as testDescription,
		 yearID, sourceTypeID
from (
	SELECT yearID, sourceTypeID
	FROM  `year`
	CROSS JOIN ##defaultdb##.sourceusetype
) as t1
left join sourcetypeyearvmt using (yearID, sourceTypeID)
join (select count(*) as n from sourcetypeyearvmt) as t2
where VMT is NULL and n > 0
ORDER BY yearID, sourceTypeID LIMIT 1;

--       check no. 4104: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4104, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'sourceTypeYearVMT' as TableName, 
		4104 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'sourcetypeyearvmt') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'sourcetypeyearvmt') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- SourceTypeYear
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4200, "sourceTypeYear", "Table Check:");

--       check no. 4201 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 4201, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   yearId       as yearId2,
         'no '        as aMatch,
         count(*)     as n
From     sourceTypeYear
Group by yearId2;

Update tempA as a 
inner join year as c on a.yearId2 = c.yearId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "sourceTypeYear" as tableName,
         4201,
        "yearId"          as testDescription,
         yearId2          as testValue,
         n                as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 4202 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4202, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sourceTypeId   as sourceTypeId2,
         'no '          as aMatch,
         count(*)       as n
From     sourceTypeYear
Group by sourceTypeId2;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId2 = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "sourceTypeYear" as tableName,
         4202,
        "sourceTypeId"    as testDescription,
         sourceTypeId2    as testValue,
         n                as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 4203: check for missing yearID and sourceTypeID combinations
INSERT INTO QA_Checks_Log values ( 4203, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         yearID, sourceTypeID)
Select   "sourcetypeyear" as tableName,
         4203,
         'Missing combination of valid yearID and sourceTypeID' as testDescription,
		 yearID, sourceTypeID
from (
	SELECT yearID, sourceTypeID
	FROM  `year`
	CROSS JOIN ##defaultdb##.sourceusetype
) as t1 left join sourcetypeyear using (yearID, sourceTypeID)
where sourceTypePopulation is NULL 
ORDER BY yearID, sourceTypeID LIMIT 1;

--       check no. 4204 -- if using the HPMSVtypeDay input, check that population summed by HPMS type is <>0 if the VMT is also >0
INSERT INTO QA_Checks_Log values ( 4204, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sum(sourceTypePopulation) as sourceTypePopulation2,
         a.yearId,
         b.HPMSVtypeId,
         b.VMT,
         'no '                as aMatch,
         count(*)             as n
From     sourceTypeYear as a
Join     ##defaultdb##.sourceUseType  as c on c.sourceTypeId = a.sourceTypeId
Join     HPMSVtypeDay   as b on b.HPMSVtypeId  = c.HPMSVtypeId
where    a.yearId = b.yearId
Group by a.yearId,
         b.HPMSVtypeId;

Update tempA as a set aMatch='yes' where a.VMT > 0 and sourceTypePopulation2 > 0;
Update tempA as a set aMatch='yes' where a.VMT = 0 and sourceTypePopulation2 = 0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         HPMSVtypeId,
         yearId)
Select   "sourceTypeYear or HPMSVtypeDay"     as tableName,
         4204,
        "sourceTypePopulation or VMT is zero" as testDescription,
         VMT                                  as testValue,
         n                                    as count,
         HPMSVTypeId,
         yearId
From     tempA
Where    aMatch <> 'yes';

--       check no. 4205 -- if using the HPMSVtypeYear input, check that population summed by HPMS type is <>0 if the VMT is also >0
INSERT INTO QA_Checks_Log values ( 4205, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   sum(sourceTypePopulation) as sourceTypePopulation2,
         a.yearId,
         b.HPMSVtypeId,
         b.HPMSBaseYearVMT,
         'no '                as aMatch,
         count(*)             as n

From                     sourceTypeYear as a
Join     ##defaultdb##.sourceUseType  as c on c.sourceTypeId = a.sourceTypeId
Join                     hpmsVTypeYear  as b on b.HPMSVtypeId  = c.HPMSVtypeId
where    a.yearId = b.yearId
Group by a.yearId,
         b.HPMSVtypeId;

Update tempA as a set aMatch='yes' where a.HPMSBaseYearVMT > 0 and sourceTypePopulation2 > 0;
Update tempA as a set aMatch='yes' where a.HPMSBaseYearVMT = 0 and sourceTypePopulation2 = 0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         HPMSVtypeId,
         yearId)
Select   "sourceTypeYear or hpmsVTypeYear"                as tableName,
         4205,
        "sourceTypePopulation or HPMSBaseYearVMT is zero" as testDescription,
         HPMSBaseYearVMT                                  as testValue,
         n                                                as count,
         HPMSVTypeId,
         yearId
From     tempA
Where    aMatch <> 'yes';

--       check no. 4206 -- if using the SourceTypeDayVMT input, check that population summed by HPMS type is <>0 if the VMT is also >0
INSERT INTO QA_Checks_Log values ( 4206, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         sourceTypeID,
         yearId)
Select   "sourceTypeYear or sourceTypeDayVMT" as tableName,
         4206,
        "sourceTypePopulation or VMT is zero" as testDescription,
         VMT                                  as testValue,
         count(*)                             as count,
         sourceTypeID,
         yearId
From     sourcetypedayvmt
JOIN	 sourcetypeyear using (sourceTypeID, yearID)
Where    (VMT > 0 and sourceTypePopulation = 0) OR
         (VMT = 0 and sourceTypePopulation > 0)
group by sourceTypeID, yearID;
         
--       check no. 4207 -- if using the SourceTypeYearVMT input, check that population summed by HPMS type is <>0 if the VMT is also >0
INSERT INTO QA_Checks_Log values ( 4207, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         sourceTypeID,
         yearId)
Select   "sourceTypeYear or sourceTypeYearVMT" as tableName,
         4207,
        "sourceTypePopulation or VMT is zero"  as testDescription,
         VMT                                   as testValue,
         count(*)                              as count,
         sourceTypeID,
         yearId
From     SourceTypeYearVMT
JOIN	 sourcetypeyear using (sourceTypeID, yearID)
Where    (VMT > 0 and sourceTypePopulation = 0) OR
         (VMT = 0 and sourceTypePopulation > 0)
group by sourceTypeID, yearID;

--       check no. 4208: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4208, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'sourceTypeYear' as TableName, 
		4208 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'sourcetypeyear') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'sourcetypeyear') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;



-- starts checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4300, "starts", "Table Check:");

--       check no. 4301 -- check for unknown hourDayIDs
INSERT INTO QA_Checks_Log values ( 4301, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   hourDayId,
        'no '    as aMatch,
         count(*) as n
from     starts
group by hourDayId;

Update tempA as a 
inner join ##defaultdb##.hourDay as m on a.hourDayId = m.hourDayId
set aMatch='yes';									

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "starts"    as tableName,
         4301         as checkNumber,
        "hourDayId"  as testDescription,
         hourDayId   as testValue,
         count(*)    as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4301 and count=0;

--       check no. 4302 -- check for unknown monthIDs
INSERT INTO QA_Checks_Log values ( 4302, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   monthId,
        'no '    as aMatch,
         count(*) as n
from     starts
group by monthId;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId = m.monthId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "starts"       as tableName,
         4302            as checkNumber,
        "monthId"       as testDescription,
         monthId        as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4302 and count=0;

--       check no. 4303 -- check for unknown yearIDs
INSERT INTO QA_Checks_Log values ( 4303, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   yearId,
        'no '     as aMatch,
         count(*) as n
from     starts
group by yearId;

Update tempA as a 
inner join year as c on a.yearId = c.yearId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "starts" as tableName,
         4303      as checkNumber,
        "yearId"  as testDescription,
         yearId   as testValue,
         count(*) as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4303 and count=0;

--       check no. 4304 -- check for unknown ageIDs
INSERT INTO QA_Checks_Log values ( 4304, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   ageId,
        'no '    as aMatch,
         count(*) as n
from     starts
group by ageId;

Update tempA as a 
inner join ##defaultdb##.ageCategory as m on a.ageId = m.ageId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "starts"       as tableName,
         4304            as checkNumber,
        "ageId"         as testDescription,
         ageId          as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4304 and count=0;

--       check no. 4305 -- check for unknown zoneIDs
INSERT INTO QA_Checks_Log values ( 4305, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   zoneId,
        'no '    as aMatch,
         count(*) as n
from     starts
group by zoneId;

Update tempA as a 
inner join zone as c on a.zoneID = c.zoneID
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "starts" as tableName,
         4305      as checkNumber,
        "zoneId"  as testDescription,
         zoneId   as testValue,
         count(*) as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4305 and count=0;

--       check no. 4306 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4306, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
        'no '    as aMatch,
         count(*) as n
from     starts
group by sourceTypeId;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "starts"       as tableName,
         4306            as checkNumber,
        "sourceTypeId"  as testDescription,
         sourceTypeId   as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4306 and count=0;

--       check no. 4307 -- check that isUserInput is either "y" or "n"
INSERT INTO QA_Checks_Log values ( 4307, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   isUserInput     as isUserInput2,
         'no '           as aMatch,
         count(*)        as n
From     starts
Group by isUserInput2;

Update tempA as a set aMatch='yes' where isUserInput2 in ('y', 'n', 'Y', 'N');

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "starts" as tableName,
         4307,
        "isUserInput not Y or N" as testDescription,
         isUserInput2      as testValue,
         n         as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 4308: check for missing hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID combinations
--                       only when starts table is provided
INSERT INTO QA_Checks_Log values ( 4308, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         hourDayID, monthID, yearID, zoneID, sourceTypeID)
Select   "starts"   as tableName,
         4308                     as checkNumber,
         'Missing combination of valid hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID' as testDescription,
		 concat('ageID: ', ageID) as testValue,
         hourDayID, monthID, yearID, zoneID, sourceTypeID
from (
	SELECT hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID
	FROM  ##defaultdb##.hourday
	CROSS JOIN ##defaultdb##.monthOfAnyYear
	CROSS JOIN `year`
	CROSS JOIN ##defaultdb##.ageCategory
	CROSS JOIN zone
	CROSS JOIN ##defaultdb##.sourceusetype
) as t1 
left join `starts` using (hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID)
join (select count(*) as n from `starts`) as t2
where `starts`.`starts` is NULL and n > 0
ORDER BY hourDayID, monthID, yearID, ageID, zoneID, sourceTypeID LIMIT 1;

--       check no. 4309: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4309, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'starts' as TableName, 
		4309 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'starts') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'starts') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- startsAgeAdjustment checks
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4400, "startsAgeAdjustment", "Table Check:");

--       check no. 4401 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4401, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
        'no '    as aMatch,
         count(*) as n
from     startsageadjustment
group by sourceTypeId;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsageadjustment"       as tableName,
         4401            as checkNumber,
        "sourceTypeId"  as testDescription,
         sourceTypeId   as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4401 and count=0;

--       check no. 4402 -- check for unknown ageIDs
INSERT INTO QA_Checks_Log values ( 4402, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   ageId,
        'no '    as aMatch,
         count(*) as n
from     startsageadjustment
group by ageId;

Update tempA as a 
inner join ##defaultdb##.ageCategory as m on a.ageId = m.ageId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsageadjustment"       as tableName,
         4402            as checkNumber,
        "ageId"         as testDescription,
         ageId          as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4402 and count=0;

--       check no. 4403: check for missing sourceTypeID, ageID combinations
--                       only when startsageadjustment table is provided
INSERT INTO QA_Checks_Log values ( 4403, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         sourceTypeID)
Select   "startsageadjustment"   as tableName,
         4403                     as checkNumber,
         'Missing combination of valid sourceTypeID, ageID' as testDescription,
		 concat('ageID: ', ageID) as testValue,
         sourceTypeID
from (
	SELECT sourceTypeID, ageID
	FROM  ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.ageCategory
) as t1 
left join startsageadjustment using (sourceTypeID, ageID)
join (select count(*) as n from startsageadjustment) as t2
where ageAdjustment is NULL and n > 0
ORDER BY sourceTypeID, ageID LIMIT 1;

--       check no. 4404: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4404, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'startsAgeAdjustment' as TableName, 
		4404 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'startsageadjustment') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'startsageadjustment') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- startsHourFraction
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4500, "startsHourFraction", "Table Check:");

--       check no. 4501 -- check for unknown dayIDs
INSERT INTO QA_Checks_Log values ( 4501, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   dayId,
        'no '     as aMatch,
         count(*) as n
from     startsHourFraction
group by dayId;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsHourFraction" as tableName,
         4501                  as checkNumber,
        "dayId"               as testDescription,
         dayId                as testValue,
         count(*)             as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4501 and count=0;

--       check no. 4502 -- check for unknown hourIDs
INSERT INTO QA_Checks_Log values ( 4502, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   hourId,
        'no '     as aMatch,
         count(*) as n
from     startsHourFraction
group by hourId;

Update tempA as a 
inner join ##defaultdb##.hourOfAnyDay as m on a.hourId = m.hourId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsHourFraction" as tableName,
         4502                  as checkNumber,
        "hourId"              as testDescription,
         hourId               as testValue,
         count(*)             as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4502 and count=0;

--       check no. 4503 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4503, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
        'no '    as aMatch,
         count(*) as n
from     startshourfraction
group by sourceTypeId;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startshourfraction"       as tableName,
         4503            as checkNumber,
        "sourceTypeId"  as testDescription,
         sourceTypeId   as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4503 and count=0;

--       check no. 4504 -- check that allocationFraction sums to 1 for each sourceTypeID and dayID
INSERT INTO QA_Checks_Log values ( 4504, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue,
           sourceTypeId,
           dayID )
Select  "startsHourFraction"               as tableName,
         4504                              as checkNumber,
        "sum of allocationFraction <> 1.0" as testDescription,
         sum(allocationFraction)           as testValue,
           sourceTypeId,
           dayID
From     startshourfraction
Group by sourceTypeId,
         dayID
Having   testValue <0.99999 or testValue >1.00001;

--       check no. 4505 -- check for allocationFraction >= 1
INSERT INTO QA_Checks_Log values ( 4505, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         dayID, hourID, sourceTypeID)
Select   "startshourfraction" as tableName,
         4505,
         'allocationFraction >= 1' as testDescription,
		 allocationFraction as testValue,
         dayID, hourID, sourceTypeID
from startshourfraction
where allocationFraction >= 1
ORDER BY dayID, hourID, sourceTypeID LIMIT 1;

--       check no. 4506: make sure allocationFraction distributions aren't flat
INSERT INTO QA_Checks_Log values ( 4506, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         dayID, sourceTypeID)
Select   "startshourfraction" as tableName,
         4506 as checkNumber,
         'allocationFraction is a flat profile' as testDescription,
         concat('all are ', allocationFraction) as testValue,
		 dayID, sourceTypeID
from startshourfraction
group by dayID, sourceTypeID, allocationFraction
having count(*) = (select count(*) from ##defaultdb##.hourOfAnyDay)
order by dayID, sourceTypeID LIMIT 1;

--       check no. 4507: check for missing dayID, hourID, sourceTypeID combinations
--                       only when startshourfraction table is provided
INSERT INTO QA_Checks_Log values ( 4507, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         dayID, hourID, sourceTypeID)
Select   "startshourfraction"   as tableName,
         4507                     as checkNumber,
         'Missing combination of valid dayID, hourID, sourceTypeID' as testDescription,
         dayID, hourID, sourceTypeID
from (
	SELECT dayID, hourID, sourceTypeID
	FROM  ##defaultdb##.dayOfAnyWeek
	CROSS JOIN ##defaultdb##.hourOfAnyDay
	CROSS JOIN ##defaultdb##.sourceusetype
) as t1 
left join startshourfraction using (dayID, hourID, sourceTypeID)
join (select count(*) as n from startshourfraction) as t2
where allocationFraction is NULL and n > 0
ORDER BY dayID, hourID, sourceTypeID LIMIT 1;

--       check no. 4508: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4508, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'startsHourFraction' as TableName, 
		4508 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'startshourfraction') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'startshourfraction') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- startsmonthadjust
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4600, "startsMonthAdjust", "Table Check:");

--       check no. 4601 -- check for unknown monthIDs
INSERT INTO QA_Checks_Log values ( 4601, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   monthId,
        'no '     as aMatch,
         count(*) as n
from     startsMonthAdjust
group by monthId;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId = m.monthId
set aMatch='yes';
	
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsMonthAdjust" as tableName,
         4601                 as checkNumber,
        "monthId"            as testDescription,
         monthId             as testValue,
         count(*)            as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4601 and count=0;

--       check no. 4602 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4602, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
        'no '    as aMatch,
         count(*) as n
from     startsmonthadjust
group by sourceTypeId;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsmonthadjust"       as tableName,
         4602            as checkNumber,
        "sourceTypeId"  as testDescription,
         sourceTypeId   as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4602 and count=0;

--       check no. 4603: check for missing monthID, sourceTypeID combinations
--                       only when startsmonthadjust table is provided
INSERT INTO QA_Checks_Log values ( 4603, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         monthID, sourceTypeID)
Select   "startsmonthadjust"   as tableName,
         4603                  as checkNumber,
         'Missing combination of valid monthID, sourceTypeID' as testDescription,
         monthID, sourceTypeID
from (
	SELECT monthID, sourceTypeID
	FROM  ##defaultdb##.monthOfAnyYear
	CROSS JOIN ##defaultdb##.sourceusetype
) as t1 
left join startsmonthadjust using (monthID, sourceTypeID)
join (select count(*) as n from startsmonthadjust) as t2
where monthAdjustment is NULL and n > 0
ORDER BY monthID, sourceTypeID LIMIT 1;

--       check no. 4604: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4604, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'startsMonthAdjust' as TableName, 
		4604 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'startsmonthadjust') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'startsmonthadjust') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- startsopmodedistribution
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4700, "startsopmodedistribution", "Table Check:");

--       check no. 4701 -- check for unknown dayIDs
INSERT INTO QA_Checks_Log values ( 4701, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   dayID as dayID,
        'no '    as aMatch,
         count(*) as n
from     startsopmodedistribution
group by dayID;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsopmodedistribution"    as tableName,
         4701         as checkNumber,
        "dayID"  as testDescription,
         dayID   as testValue,
         count(*)    as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4701 and count=0;

--       check no. 4702 -- check for unknown hourIDs
INSERT INTO QA_Checks_Log values ( 4302, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   hourID,
        'no '    as aMatch,
         count(*) as n
from     startsopmodedistribution
group by hourID;

Update tempA as a 
inner join ##defaultdb##.hourOfAnyDay as m on a.hourId = m.hourId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsopmodedistribution"       as tableName,
         4702            as checkNumber,
        "hourID"       as testDescription,
         hourID        as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4702 and count=0;

--       check no. 4703 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4703, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
        'no '    as aMatch,
         count(*) as n
from     startsopmodedistribution
group by sourceTypeId;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId = m.sourceTypeId
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsopmodedistribution"       as tableName,
         4703            as checkNumber,
        "sourceTypeId"  as testDescription,
         sourceTypeId   as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4703 and count=0;

--       check no. 4704 -- check for unknown ageIDs
INSERT INTO QA_Checks_Log values ( 4704, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   ageId,
        'no '    as aMatch,
         count(*) as n
from     startsopmodedistribution
group by ageId;

Update tempA as a 
inner join ##defaultdb##.ageCategory as m on a.ageId = m.ageId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsopmodedistribution"       as tableName,
         4704            as checkNumber,
        "ageId"         as testDescription,
         ageId          as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4704 and count=0;

--       check no. 4705 -- check for unknown opModeIDs
INSERT INTO QA_Checks_Log values ( 4705, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   opModeID,
        'no '    as aMatch,
         count(*) as n
from     startsopmodedistribution
group by opModeID;

Update tempA as a 
inner join ##defaultdb##.operatingmode as m on a.opModeID = m.opModeID
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsopmodedistribution"       as tableName,
         4705            as checkNumber,
        "opModeID"  as testDescription,
         opModeID   as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4705 and count=0;

--       check no. 4706 -- check that isUserInput is either "y" or "n"
INSERT INTO QA_Checks_Log values ( 4706, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   isUserInput     as isUserInput2,
         'no '           as aMatch,
         count(*)        as n
From     startsopmodedistribution
Group by isUserInput2;

Update tempA as a set aMatch='yes' where isUserInput2 in ('y', 'n', 'Y', 'N');

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsopmodedistribution" as tableName,
         4706,
        "isUserInput not Y or N" as testDescription,
         isUserInput2      as testValue,
         n         as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 4707 -- check that opModeFraction sums to 1 for each dayID, hourID, sourceTypeID, ageID
INSERT INTO QA_Checks_Log values ( 4707, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( tableName,
         checkNumber,
         testDescription,
         testValue,
		 dayID, hourID, sourceTypeID )
Select  "startsopmodedistribution"               as tableName,
         4707                              as checkNumber,
        "sum of opModeFraction <> 1.0" as testDescription,
         CONCAT('age ', ageID, ' sums to ', sum(opModeFraction))           as testValue,
		 dayID, hourID, sourceTypeID
From     startsopmodedistribution
Group by dayID, hourID, sourceTypeID, ageID
Having   sum(opModeFraction) <0.99999 or sum(opModeFraction) >1.00001;

--       check no. 4708: check for missing dayID, hourID, sourceTypeID, ageID, opModeID combinations
--                       only when startsopmodedistribution table is provided
INSERT INTO QA_Checks_Log values ( 4708, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         dayID, hourID, sourceTypeID)
Select   "startsopmodedistribution"   as tableName,
         4708                     as checkNumber,
         'Missing combination of valid dayID, hourID, sourceTypeID, ageID, opModeID' as testDescription,
		 concat('ageID: ', ageID, ', opModeID: ', opModeID) as testValue,
         dayID, hourID, sourceTypeID
from (
	SELECT dayID, hourID, sourceTypeID, ageID, opModeID
	FROM  ##defaultdb##.dayOfAnyWeek
	CROSS JOIN ##defaultdb##.hourOfAnyDay
	CROSS JOIN ##defaultdb##.sourceusetype
	CROSS JOIN ##defaultdb##.agecategory
	CROSS JOIN ##defaultdb##.operatingmode
    where opModeID between 101 and 108
) as t1 
left join startsopmodedistribution using (dayID, hourID, sourceTypeID, ageID, opModeID)
join (select count(*) as n from startsopmodedistribution) as t2
where opModeFraction is NULL and n > 0
ORDER BY dayID, hourID, sourceTypeID, ageID, opModeID LIMIT 1;

--       check no. 4709: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4709, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'startsOpModeDistribution' as TableName, 
		4709 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'startsopmodedistribution') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'startsopmodedistribution') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- startsPerDay
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4800, "startsPerDay", "Table Check:");

--       check no. 4801 -- check for unknown dayIDs
INSERT INTO QA_Checks_Log values ( 4801, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   dayId,
        'no '     as aMatch,
         count(*) as n
from     startsPerDay
group by dayId;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId = m.dayId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsPerDay" as tableName,
         4801            as checkNumber,
        "dayId"         as testDescription,
         dayId          as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4801 and count=0;

--       check no. 4802 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4802, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
        'no '    as aMatch,
         count(*) as n
from     startsperday
group by sourceTypeId;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsperday"       as tableName,
         4802            as checkNumber,
        "sourceTypeId"  as testDescription,
         sourceTypeId   as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4802 and count=0;

--       check no. 4803: check for missing dayID, sourceTypeID combinations
--                       only when startsperday table is provided
INSERT INTO QA_Checks_Log values ( 4803, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         dayID, sourceTypeID)
Select   "startsperday"   as tableName,
         4803                  as checkNumber,
         'Missing combination of valid dayID, sourceTypeID' as testDescription,
         dayID, sourceTypeID
from (
	SELECT dayID, sourceTypeID
	FROM  ##defaultdb##.dayOfAnyWeek
	CROSS JOIN ##defaultdb##.sourceusetype
) as t1 
left join startsperday using (dayID, sourceTypeID)
join (select count(*) as n from startsperday) as t2
where startsPerDay.startsPerDay is NULL and n > 0
ORDER BY dayID, sourceTypeID LIMIT 1;

--       check no. 4804: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4804, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'startsPerDay' as TableName, 
		4804 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'startsperday') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'startsperday') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- startsPerDayPerVehicle
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (4900, "startsPerDayPerVehicle", "Table Check:");

--       check no. 4901 -- check for unknown dayIDs
INSERT INTO QA_Checks_Log values ( 4901, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   dayId,
        'no '     as aMatch,
         count(*) as n
from     startsPerDayPerVehicle
group by dayId;

Update tempA as a 
inner join ##defaultdb##.dayOfAnyWeek as m on a.dayId = m.dayId
set aMatch='yes';


Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsPerDayPerVehicle" as tableName,
         4901            as checkNumber,
        "dayId"         as testDescription,
         dayId          as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4901 and count=0;

--       check no. 4902 -- check for unknown sourceTypeIDs
INSERT INTO QA_Checks_Log values ( 4902, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
select   sourceTypeId,
        'no '    as aMatch,
         count(*) as n
from     startsPerDayPerVehicle
group by sourceTypeId;

Update tempA as a 
inner join ##defaultdb##.sourceUseType as m on a.sourceTypeId = m.sourceTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "startsPerDayPerVehicle"       as tableName,
         4902            as checkNumber,
        "sourceTypeId"  as testDescription,
         sourceTypeId   as testValue,
         count(*)       as cou
from     tempA
where    aMatch <> 'yes ';
Delete from CDB_Checks where checkNumber=4902 and count=0;

--       check no. 4903: check for missing dayID, sourceTypeID combinations
--                       only when startsPerDayPerVehicle table is provided
INSERT INTO QA_Checks_Log values ( 4903, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         dayID, sourceTypeID)
Select   "startsPerDayPerVehicle"   as tableName,
         4903                  as checkNumber,
         'Missing combination of valid dayID, sourceTypeID' as testDescription,
         dayID, sourceTypeID
from (
	SELECT dayID, sourceTypeID
	FROM  ##defaultdb##.dayOfAnyWeek
	CROSS JOIN ##defaultdb##.sourceusetype
) as t1 
left join startsPerDayPerVehicle using (dayID, sourceTypeID)
join (select count(*) as n from startsPerDayPerVehicle) as t2
where startsPerDayPerVehicle.startsPerDayPerVehicle is NULL and n > 0
ORDER BY dayID, sourceTypeID LIMIT 1;

--       check no. 4904: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (4904, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'startsPerDayPerVehicle' as TableName, 
		4904 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'startsperdaypervehicle') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'startsperdaypervehicle') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- zoneMonthHour
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (5000, "zoneMonthHour", "Table Check:");

--       check no. 5001 -- check for unknown hourIDs
INSERT INTO QA_Checks_Log values ( 5001, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   hourId    as hourId2,
         'no '     as aMatch,
         count(*)  as n
From     zoneMonthHour
Group by hourId2;

Update tempA as a 
inner join ##defaultdb##.hourOfAnyDay as m on a.hourId2 = m.hourId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zoneMonthHour" as tableName,
         5001,
         "hourId"        as testDescription,
         hourId2         as testValue,
         n               as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 5002 -- check for unknown monthIDs
INSERT INTO QA_Checks_Log values ( 5002, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   monthId      as monthId2,
         'no '        as aMatch,
         count(*)     as n
From     zoneMonthHour
Group by monthId2;

Update tempA as a 
inner join ##defaultdb##.monthOfAnyYear as m on a.monthId2 = m.monthId
set aMatch='yes';
	
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zoneMonthHour"    as tableName,
         5002,
        "monthId"           as testDescription,
         monthId2           as testValue,
         n                  as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 5003 -- check for unknown zoneIDs
INSERT INTO QA_Checks_Log values ( 5003, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   zoneId          as zoneId2,
         'no '           as aMatch,
         count(*)        as n
From     zonemonthhour
Group by zoneId2;

Update tempA as a 
inner join zone as c on a.zoneID2 = c.zoneID
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zoneMonthHour" as tableName,
         5003,
        "zoneId"         as testDescription,
         zoneId2         as testValue,
         n               as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 5004 -- check that the temperature is between -80 and 150
INSERT INTO QA_Checks_Log values ( 5004, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   temperature  as temperature2,
         'no '        as aMatch,
         count(*)     as n
From     zoneMonthHour
Group by temperature2;

Update tempA as a set aMatch='yes' where temperature2>=-80.0 and temperature2<=150.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zoneMonthHour"    as tableName,
         5004,
        "temperature"       as testDescription,
         temperature2       as testValue,
         n                  as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 5005 -- check that the humidity is between 0 and 100
INSERT INTO QA_Checks_Log values ( 5005, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   relHumidity  as relHumidity2,
         'no '        as aMatch,
         count(*)     as n
From     zoneMonthHour
Group by relHumidity2;

Update tempA as a set aMatch='yes' where relHumidity2>=0.0 and relHumidity2<=100.0;

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zoneMonthHour"    as tableName,
         5005,
        "relHumidity"       as testDescription,
         relHumidity2       as testValue,
         n                  as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 5006: check for missing monthID, zoneID, hourID combinations
INSERT INTO QA_Checks_Log values ( 5006, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         monthID, zoneID, hourID)
Select   "zonemonthhour" as tableName,
         5006,
         'Missing combination of valid monthID, zoneID, hourID' as testDescription,
		 monthID, zoneID, hourID
from (
	SELECT monthID, zoneID, hourID
	FROM  ##defaultdb##.monthOfAnyYear
	CROSS JOIN zone
	CROSS JOIN ##defaultdb##.hourOfAnyDay
) as t1
left join zonemonthhour using (monthID, zoneID, hourID)
join (select count(*) as n from zonemonthhour) as t2
where (temperature is NULL or relHumidity is NULL) and n > 0
ORDER BY monthID, zoneID, hourID LIMIT 1;

--       check no. 5007: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (5007, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'zoneMonthHour' as TableName, 
		5007 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'zonemonthhour') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'zonemonthhour') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;


-- zoneRoadType
Insert into CDB_Checks (CheckNumber, TableName, TestDescription) values (5100, "zoneRoadType", "Table Check:");

--       check no. 5101 -- check for unknown roadTypeIDs
INSERT INTO QA_Checks_Log values ( 5101, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   roadTypeId as roadTypeId2,
         'no '      as aMatch,
         count(*)   as n
From     zoneRoadtype
Group by roadTypeId2;

Update tempA as a 
inner join ##defaultdb##.roadType as m on a.roadTypeId2 = m.roadTypeId
set aMatch='yes';

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zoneRoadType" as tableName,
         5101,
        "roadTypeId"            as testDescription,
         roadTypeId2            as testValue,
         n                      as count
From     tempA
Where    aMatch <> 'yes';

--       check no. 5102 -- check for unknown zoneIDs
INSERT INTO QA_Checks_Log values ( 5102, 'OK', @hVersion, curDate(), curTime() );
Drop table if exists tempA;
Create table tempA
Select   zoneId          as zoneId2,
         'no '           as aMatch,
         count(*)        as n
From     zoneroadtype
Group by zoneId2;

Update tempA as a 
inner join zone as c on a.zoneID2 = c.zoneID
set aMatch='yes';	

Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count  )
Select   "zoneRoadType" as tableName,
         5102,
        "zoneId"        as testDescription,
         zoneId2        as testValue,
         n              as count               --
From     tempA
Where    aMatch <> 'yes';

--       check no. 5103 -- check that the SHOAllocFactor sums to 1 for each road type
--                         No tolerance on this one because there should only be one 
--                         row per road type, so no floating point issues here
INSERT INTO QA_Checks_Log values ( 5103, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         testValue,
         count,
         roadTypeID)
Select  "zoneRoadType"       as tableName,
         5103,
        "SHOAllocFactor should be 1"         as testDescription,
         sum(SHOAllocFactor) as SHOAllocFactor2,
         count(*) as `count`,
         roadTypeID
From     zoneroadtype
where    roadTypeID not in (1, 100)
Group by roadTypeId
Having   SHOAllocFactor2 <> 1 or `count` > 1;
 
--       check no. 5104: check for missing zoneID, roadTypeID combinations
INSERT INTO QA_Checks_Log values ( 5104, 'OK', @hVersion, curDate(), curTime() );
Insert into CDB_Checks
       ( TableName,
         CheckNumber,
         TestDescription,
         zoneID, roadTypeID)
Select   "zoneroadtype" as tableName,
         5104,
         'Missing combination of valid zoneID, roadTypeID' as testDescription,
		 zoneID, roadTypeID
from (
	SELECT zoneID, roadTypeID
	FROM  zone
	CROSS JOIN ##defaultdb##.roadtype where roadTypeID in (2, 3, 4, 5)
) as t1 left join zoneroadtype using (zoneID, roadTypeID)
where SHOAllocFactor is NULL 
ORDER BY zoneID, roadTypeID LIMIT 1;

--       check no. 5105: check column type definitions for input db mismatches with default db
INSERT INTO QA_Checks_Log values (5105, 'OK', @hVersion, curDate(), curTime());
INSERT INTO CDB_Checks
		( TableName,
		  CheckNumber,
		  TestDescription,
		  TestValue)
SELECT 'zoneRoadType' as TableName, 
		5105 as CheckNumber, 
		'Incorrect Column Definition' as TestDescription, 
		concat('Expected ', column_name, ' to be of type ', t2.column_type, '(', 
				case when t2.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t2.column_key <> '' then concat(',key:', t2.column_key) else '' end, 
				') but is of type ', t1.column_type, '(',
				case when t1.is_nullable = 'YES' then 'NULL' else 'NOT NULL' end, 
				case when t1.column_key <> '' then concat(',key:', t1.column_key) else '' end, 
				')'
		) as TestValue
from (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##inputdb##' and table_name = 'zoneroadtype') t1 
join (select column_name, column_type, is_nullable, column_key from information_schema.columns 
		where table_schema = '##defaultdb##' and table_name = 'zoneroadtype') t2 using (column_name)
where t1.column_type <> t2.column_type or t1.is_nullable <> t2.is_nullable or t1.column_key <> t2.column_key;
-- ##############################################################################
-- End data QA checks
-- ##############################################################################


-- ##############################################################################
-- Final steps
-- ##############################################################################
--       Clean up the results table:
Delete from   CDB_Checks where TableName = "ALL";
Insert into   CDB_Checks ( TableName, TestDescription ) values   ("All Tables",     "Tables Checked.");
Update        CDB_Checks set msgType = 'Info'   where testDescription = 'Table Check:';
Update        CDB_Checks set msgType = 'Data Problem' where checkNumber is not null and msgType is null;
Update        CDB_Checks set msgType = 'DB Checked' where tableName = 'ALL';
Update        CDB_Checks set msgDate = curDate();
Update        CDB_Checks set msgTime = curTime();
Update        CDB_Checks set dataBaseName = database();
Update        CDB_Checks set countyId = (select min(countyId) from county);

-- set Status to 'Completed' by default (this gets overwritten in the cases outlined below)
Update        CDB_Checks set status = 'Completed' where `status` is NULL or `status` = '';

-- set Status to 'Error' for most checks
Update        CDB_Checks set status = 'Error'   where checkNumber is not null and testDescription <> 'Table Check:';

-- set Status to 'Warning' for select checks (these are typically distribution checks, as well as tables that give warnings
--                                            if you supply them: fuelformulation, fuelsupply, and zonemonthhour)
Update        CDB_Checks set status = 'Warning' where checkNumber in 
			  (1608,1609,1610,1611,1807,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2101,
			   2102,2103,2104,2406,2507,2508,2808,2809,3605,3804,3805,3906,4506,5001,5002,5003,5004,5005,
			   5006);

-- special cases for VMT/starts checks --
-- mark all VMT row count checks as 'complete' regardless of results
Update        CDB_Checks set status = 'Completed' where checkNumber in (1002, 1003, 1004, 1005);
-- mark all starts row count checks as 'complete' regardless of results
Update        CDB_Checks set status = 'Completed' where checkNumber in (1007, 1008, 1009);

-- order the results
alter table CDB_Checks order by checkNumber, tableName;

-- Eliminate temporary tables from the CDB.
Drop table if exists tempA;
Drop table if exists tempB;
call emptyTableCleanUp();

-- Version is set at beginning of the file.
Update CDB_Checks set version = @version;

-- Create a database to hold all results from a batch file run.
create Database if not exists All_CDB_Checks;

-- Create a table in the database to hold all results from a batch file run.
create Table if not exists All_CDB_Checks.All_CDB_Checks like CDB_Checks;

-- Copy the results from this run into the table holding all results from a batch run.
insert into All_CDB_Checks.All_CDB_Checks select * from CDB_Checks;

-- Clear all connections to the database tables.
flush tables;

-- Eliminate the stored procedures generated by this script.
drop procedure if exists checkImCoverage;
drop procedure if exists checkHotellingActivityDistribution;
drop procedure if exists checkIdleModelYearGrouping;
drop procedure if exists checkTotalIdleFraction;
drop procedure if exists emptyTableCleanUp;

-- drop final tables
drop table CDB_Checks;
drop table QA_Checks_Log;

select '  .. M3onroadCDBchecks.sql Done',curTime(),database();

-- done.
