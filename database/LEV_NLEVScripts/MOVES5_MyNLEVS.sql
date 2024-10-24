-- ###################################################################
-- MOVES5_MyNLEVs.sql
-- 11/16/2022
-- 
-- This script is intended to be used in conjunction with the NLEV
-- Tool in MOVES. To access the tool, open the Tools menu in MOVES, 
-- and select "Build NLEV Input Database". The full instructions for
-- using this tool can be found by clicking the "Open Instructions"
-- button at the bottom of the tool's screen.
--
-- This tool creates a new MOVES input database that contains NLEV
-- emission rates for model years 1999 and 2000.
--
-- NOTE: This script should not be modified as part of running the
-- tool (unlike the *LEV* Tool, which contains a template to be
-- modified by the user).
-- 
-- This script cannot be run directly outside of the MOVES GUI. If
-- you would like to execute it manually (for example, in HeidiSQL),
-- you will need to replace:
--   "##outputdb##" with your target new database name
--   "##defaultdb##" with the name of the default MOVES database

-- ###################################################################

-- Create database
drop database if exists ##outputdb##;
create database ##outputdb##;

-- Create a table to hold the adjusted NLEV emission rates
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
) ENGINE=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';

-- Insert NLEV emission rates into the new table
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
