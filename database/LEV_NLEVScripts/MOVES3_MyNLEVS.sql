###################################################################
-- MOVES3MyLEVs.sql
-- 08/14/14
-- 
-- This script uses the database "MOVES3_LEV_Standards" to  create a new database, "##outputdb##" 
-- with adjusted LEV emission rates for only a subset of model years
--
-- TODO: Explain ##outputdb## for manual editing
--
--
-- Users must edit the last command in the script to specify the desired model years.
-- See "Instructions for using LEV and NLEV Inputs for MOVES3"

-- ###################################################################
-- Create database

drop database if exists ##outputdb##;
create database ##outputdb##;

-- ###################################################################

-- Create a table to hold the adjusted LEV emission rates.

drop table if exists ##outputdb##.emissionratebyage;
CREATE TABLE ##outputdb##.emissionratebyage (
  `sourceBinID` bigint(20) NOT NULL default '0',
  `polProcessID` smallint(6) NOT NULL default '0',
  `opModeID` smallint(6) NOT NULL default '0',
  `ageGroupID` smallint(6) NOT NULL default '0',
  `meanBaseRate` float default NULL,
  `meanBaseRateCV` float default NULL,
  `meanBaseRateIM` float default NULL,
  `meanBaseRateIMCV` float default NULL,
  `dataSourceId` smallint(6) default NULL,
  PRIMARY KEY  (`ageGroupID`,`opModeID`,`polProcessID`,`sourceBinID`),
  UNIQUE KEY `XPKEmissionRateByAge` 
    (`sourceBinID`,`polProcessID`,`opModeID`,`ageGroupID`),
  KEY `polProcessID` (`polProcessID`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 ;

-- ###################################################################

-- Table listing of short codes and model year included in "MOVES3_LEV_Standards" database

-- shortModYrGroupID | modelYearGroupName 
--                94 | 1994               
--                95 | 1995               
--                96 | 1996               
--                97 | 1997               
--                98 | 1998               
--                99 | 1999               
--                20 | 2000               
--                21 | 2001               
--                22 | 2002               
--                23 | 2003               
--                24 | 2004               
--                25 | 2005               
--                26 | 2006               
--                27 | 2007               
--                28 | 2008               
--                29 | 2009               
--                30 | 2010               
--                31 | 2011               
--                32 | 2012               
--                33 | 2013               
--                34 | 2014               
--                35 | 2015               
--                36 | 2016 
--                37 | 2017 
--                38 | 2018 
--                39 | 2019 
--                40 | 2020
--                41 | 2021
--                42 | 2022
--                43 | 2023
--                44 | 2024
--                45 | 2025
--                46 | 2026
--                47 | 2027
--                48 | 2028
--                49 | 2029
--                50 | 2030
--                 9 | 2031-2050
--                60 | 2051-2060

-- Model years 1994-2003 are LEV I program model years.
-- Model years 2004-2014 are LEV II program model years. 
-- Model years 2015-2060 are LEV III program model years. 

-- ###################################################################

-- Insert values for LEV emission rates for years covered by program.

-- Edit this command as described in "Instructions for using LEV and NLEV Inputs for MOVES3" 

--       In the sequence of short codes at the bottom of this section,
--       keep the codes that correspond to model years in the LEV Program; and
--       delete the codes that correspond to model years NOT in the LEV Program.
--       Make sure that a comma is not between a number and a parenthesis.

insert into ##outputdb##.emissionratebyage (
	sourcebinid,
	polprocessid,
	opmodeid,
	agegroupid,
	meanbaserate,
	meanbaseratecv,
	meanbaserateim,
	meanbaserateimcv,
	datasourceid )
select  
	sourcebinid,
	polprocessid,
	opmodeid,
	agegroupid,
	meanbaserate,
	meanbaseratecv,
	meanbaserateim,
	meanbaserateimcv,
	datasourceid 
from ##defaultdb##.emissionratebyagenlev;

-- ###################################################################














