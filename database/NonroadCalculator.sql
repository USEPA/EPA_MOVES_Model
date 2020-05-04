-- Nonroad calculator script.
-- Author Wesley Faler
-- Version 2015-04-07

-- @algorithm
-- @owner Nonroad Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

##create.nrequipmenttype##;
TRUNCATE TABLE nrequipmenttype;

##create.nrscc##;
TRUNCATE TABLE nrscc;

##create.engineTech##;
TRUNCATE TABLE engineTech;

##create.nrsourceusetype##;
TRUNCATE TABLE nrsourceusetype;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

-- End Section Extract Data

-- Section Local Data Removal
-- End Section Local Data Removal

-- Section Processing

nonroad monthToRealDayFactor=##NRmonthToRealDayFactor## ##NRPolProcessIDs##;

-- End Section Processing

-- Section Cleanup
DROP TABLE IF EXISTS nrequipmenttype;
DROP TABLE IF EXISTS nrscc;
DROP TABLE IF EXISTS enginetech;
DROP TABLE IF EXISTS nrsourceusetype;
-- End Section Cleanup
