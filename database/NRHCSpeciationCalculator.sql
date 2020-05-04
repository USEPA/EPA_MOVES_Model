-- Author Wesley Faler
-- Version 2015-02-07

-- @algorithm
-- @owner Nonroad HC Speciation Calculator
-- @calculator

-- Section Create Remote Tables for Extracted Data

##create.nrHCSpeciation##;
TRUNCATE TABLE nrHCSpeciation;

##create.nrMethaneTHCRatio##;
TRUNCATE TABLE nrMethaneTHCRatio;

-- End Section Create Remote Tables for Extracted Data

-- Section Extract Data

cache select pollutantID,processID,engTechID,fuelSubtypeID,nrHPCategory,speciationConstant
into outfile '##nrhcspeciation##'
from nrHCSpeciation
where (pollutantID*100+processID) in (##hcPolProcessIDs##);

cache select processID,engTechID,fuelSubtypeID,nrHPCategory,CH4THCRatio
into outfile '##nrMethaneTHCRatio##'
from nrMethaneTHCRatio
where processID in (##hcProcessIDs##)
and fuelSubTypeID in (##macro.csv.all.nrFuelSubTypeID##);

-- End Section Extract Data

-- Section Processing

-- All processing logic is done in the external calculator.

-- End Section Processing

-- Section Cleanup
drop table if exists HCETOHBin;
drop table if exists nrHCSpeciation;
-- End Section Cleanup

-- Section Final Cleanup
-- End Section Final Cleanup
